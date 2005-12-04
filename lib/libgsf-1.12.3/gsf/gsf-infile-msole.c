/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-infile-msole.c :
 *
 * Copyright (C) 2002-2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2.1 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

/* Lots of useful information in
 *   http://www.aafassociation.org/html/specs/aafcontainerspec-v1.0.1.pdf
 */

#include <gsf-config.h>
#include <gsf/gsf-infile-impl.h>
#include <gsf/gsf-infile-msole.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-msole-impl.h>
#include <gsf/gsf-input-proxy.h>

#include <string.h>
#include <stdio.h>

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "libgsf:msole"

static GObjectClass *parent_class;

typedef struct {
	guint32 *block;
	guint32  num_blocks;
} MSOleBAT;

typedef struct {
	char	 *name;
	char	 *collation_name;
	int	  index;
	size_t    size;
	gboolean  use_sb;
	guint32   first_block;
	gboolean  is_directory;
	GList	 *children;
	unsigned char clsid[16];	/* 16 byte GUID used by some apps */
} MSOleDirent;

typedef struct {
	struct {
		MSOleBAT bat;
		unsigned shift;
		unsigned filter;
		size_t   size;
	} bb, sb;
	gsf_off_t max_block;
	guint32 threshold; /* transition between small and big blocks */
        guint32 sbat_start, num_sbat;

	MSOleDirent *root_dir;
	GsfInput *sb_file;

	int ref_count;
} MSOleInfo;

struct _GsfInfileMSOle {
	GsfInfile parent;

	GsfInput    *input;
	MSOleInfo   *info;
	MSOleDirent *dirent;
	MSOleBAT     bat;
	gsf_off_t    cur_block;

	struct {
		guint8  *buf;
		size_t  buf_size;
	} stream;
};

typedef struct {
	GsfInfileClass  parent_class;
} GsfInfileMSOleClass;

#define GSF_INFILE_MSOLE_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST ((k), GSF_INFILE_MSOLE_TYPE, GsfInfileMSOleClass))
#define GSF_IS_INFILE_MSOLE_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), GSF_INFILE_MSOLE_TYPE))

/* utility macros */
#define OLE_BIG_BLOCK(index, ole)	((index) >> ole->info->bb.shift)

static GsfInput *gsf_infile_msole_new_child (GsfInfileMSOle *parent,
					     MSOleDirent *dirent, GError **err);
static void ole_info_unref (MSOleInfo *info);

/**
 * ole_get_block :
 * @ole    : the infile
 * @block  :
 * @buffer : optionally NULL
 *
 * Read a block of data from the underlying input.
 * Be really anal.
 **/
static guint8 const *
ole_get_block (GsfInfileMSOle const *ole, guint32 block, guint8 *buffer)
{
	g_return_val_if_fail (block < ole->info->max_block, NULL);

	/* OLE_HEADER_SIZE is fixed at 512, but the sector containing the
	 * header is padded out to bb.size (sector size) when bb.size > 512. */
	if (gsf_input_seek (ole->input,
		(gsf_off_t)(MAX (OLE_HEADER_SIZE, ole->info->bb.size) + (block << ole->info->bb.shift)),
		G_SEEK_SET) < 0)
		return NULL;

	return gsf_input_read (ole->input, ole->info->bb.size, buffer);
}

/**
 * ole_make_bat :
 * @metabat	: a meta bat to connect to the raw blocks (small or large)
 * @size_guess	: An optional guess as to how many blocks are in the file
 * @block	: The first block in the list.
 * @res		: where to store the result.
 *
 * Walk the linked list of the supplied block allocation table and build up a
 * table for the list starting in @block.
 *
 * Retrurns TRUE on error.
 */
static gboolean
ole_make_bat (MSOleBAT const *metabat, size_t size_guess, guint32 block,
	      MSOleBAT *res)
{
	/* NOTE : Only use size as a suggestion, sometimes it is wrong */
	GArray *bat = g_array_sized_new (FALSE, FALSE,
		sizeof (guint32), size_guess);

	guint8 *used = (guint8*)g_alloca (1 + metabat->num_blocks / 8);
	memset (used, 0, 1 + metabat->num_blocks / 8);

	if (block < metabat->num_blocks)
		do {
			/* Catch cycles in the bat list */
			g_return_val_if_fail (0 == (used[block/8] & (1 << (block & 0x7))), TRUE);
			used[block/8] |= 1 << (block & 0x7);

			g_array_append_val (bat, block);
			block = metabat->block [block];
		} while (block < metabat->num_blocks);

	res->block = NULL;

	res->num_blocks = bat->len;
	res->block = (guint32 *) (gpointer) g_array_free (bat, FALSE);

	if (block != BAT_MAGIC_END_OF_CHAIN) {
		g_warning ("This OLE2 file is invalid.\n"
			   "The Block Allocation  Table for one of the streams had %x instead of a terminator (%x).\n"
			   "We might still be able to extract some data, but you'll want to check the file.",
			   block, BAT_MAGIC_END_OF_CHAIN);
	}

	return FALSE;
}

static void
ols_bat_release (MSOleBAT *bat)
{
	if (bat->block != NULL) {
		g_free (bat->block);
		bat->block = NULL;
		bat->num_blocks = 0;
	}
}

/**
 * ole_info_read_metabat :
 * @ole  :
 * @bats :
 *
 * A small utility routine to read a set of references to bat blocks
 * either from the OLE header, or a meta-bat block.
 *
 * Returns a pointer to the element after the last position filled.
 **/
static guint32 *
ole_info_read_metabat (GsfInfileMSOle *ole, guint32 *bats, guint32 max,
		       guint32 const *metabat, guint32 const *metabat_end)
{
	guint8 const *bat, *end;

	for (; metabat < metabat_end; metabat++) {
		bat = ole_get_block (ole, *metabat, NULL);
		if (bat == NULL)
			return NULL;
		end = bat + ole->info->bb.size;
		for ( ; bat < end ; bat += BAT_INDEX_SIZE, bats++) {
			*bats = GSF_LE_GET_GUINT32 (bat);
			g_return_val_if_fail (*bats < max ||
					      *bats >= BAT_MAGIC_METABAT, NULL);
		}
	}
	return bats;
}

/**
 * gsf_ole_get_guint32s :
 * @dst :
 * @src :
 * @num_bytes :
 *
 * Copy some some raw data into an array of guint32.
 **/
static void
gsf_ole_get_guint32s (guint32 *dst, guint8 const *src, int num_bytes)
{
	for (; (num_bytes -= BAT_INDEX_SIZE) >= 0 ; src += BAT_INDEX_SIZE)
		*dst++ = GSF_LE_GET_GUINT32 (src);
}

static GsfInput *
ole_info_get_sb_file (GsfInfileMSOle *parent)
{
	MSOleBAT meta_sbat;

	if (parent->info->sb_file != NULL)
		return parent->info->sb_file;

	parent->info->sb_file = gsf_infile_msole_new_child (parent,
		parent->info->root_dir, NULL);
	if (!parent->info->sb_file)
		return NULL;

	/* avoid creating a circular reference */
	ole_info_unref (((GsfInfileMSOle *)parent->info->sb_file)->info);

	g_return_val_if_fail (parent->info->sb.bat.block == NULL, NULL);

	if (ole_make_bat (&parent->info->bb.bat,
			  parent->info->num_sbat, parent->info->sbat_start, &meta_sbat))
		return NULL;

	parent->info->sb.bat.num_blocks = meta_sbat.num_blocks * (parent->info->bb.size / BAT_INDEX_SIZE);
	parent->info->sb.bat.block	= g_new0 (guint32, parent->info->sb.bat.num_blocks);
	ole_info_read_metabat (parent, parent->info->sb.bat.block,
		parent->info->sb.bat.num_blocks,
		meta_sbat.block, meta_sbat.block + meta_sbat.num_blocks);
	ols_bat_release (&meta_sbat);

	return parent->info->sb_file;
}

static gint
ole_dirent_cmp (MSOleDirent const *a, MSOleDirent const *b)
{
	g_return_val_if_fail (a, 0);
	g_return_val_if_fail (b, 0);

	g_return_val_if_fail (a->collation_name, 0);
	g_return_val_if_fail (b->collation_name, 0);

	return strcmp (b->collation_name, a->collation_name);
}

/**
 * ole_dirent_new :
 * @ole    :
 * @entry  :
 * @parent : optional
 *
 * Parse dirent number @entry and recursively handle its siblings and children.
 **/
static MSOleDirent *
ole_dirent_new (GsfInfileMSOle *ole, guint32 entry, MSOleDirent *parent)
{
	MSOleDirent *dirent;
	guint32 block, next, prev, child, size;
	guint8 const *data;
	guint8 type;
	guint16 name_len;

	if (entry >= DIRENT_MAGIC_END)
		return NULL;

	block = OLE_BIG_BLOCK (entry * DIRENT_SIZE, ole);

	g_return_val_if_fail (block < ole->bat.num_blocks, NULL);
	data = ole_get_block (ole, ole->bat.block [block], NULL);
	if (data == NULL)
		return NULL;
	data += (DIRENT_SIZE * entry) % ole->info->bb.size;

	type = GSF_LE_GET_GUINT8 (data + DIRENT_TYPE);
	if (type != DIRENT_TYPE_DIR &&
	    type != DIRENT_TYPE_FILE &&
	    type != DIRENT_TYPE_ROOTDIR) {
		g_warning ("Unknown stream type 0x%x", type);
		return NULL;
	}

	/* It looks like directory (and root directory) sizes are sometimes bogus */
	size = GSF_LE_GET_GUINT32 (data + DIRENT_FILE_SIZE);
	g_return_val_if_fail (type == DIRENT_TYPE_DIR || type == DIRENT_TYPE_ROOTDIR ||
			      size <= (guint32)ole->input->size, NULL);

	dirent = g_new0 (MSOleDirent, 1);
	dirent->index	     = entry;
	dirent->size	     = size;
	/* Store the class id which is 16 byte identifier used by some apps */
	memcpy(dirent->clsid, data + DIRENT_CLSID, sizeof(dirent->clsid));

	/* root dir is always big block */
	dirent->use_sb	     = parent && (size < ole->info->threshold);
	dirent->first_block  = (GSF_LE_GET_GUINT32 (data + DIRENT_FIRSTBLOCK));
	dirent->is_directory = (type != DIRENT_TYPE_FILE);
	dirent->children     = NULL;
	prev  = GSF_LE_GET_GUINT32 (data + DIRENT_PREV);
	next  = GSF_LE_GET_GUINT32 (data + DIRENT_NEXT);
	child = GSF_LE_GET_GUINT32 (data + DIRENT_CHILD);
	name_len = GSF_LE_GET_GUINT16 (data + DIRENT_NAME_LEN);
	dirent->name = NULL;
	if (0 < name_len && name_len <= DIRENT_MAX_NAME_SIZE) {
		gunichar2 uni_name [DIRENT_MAX_NAME_SIZE+1];
		gchar const *end;
		int i;

		/* !#%!@$#^
		 * Sometimes, rarely, people store the stream name as ascii
		 * rather than utf16.  Do a validation first just in case.
		 */
		if (!g_utf8_validate (data, -1, &end) ||
		    ((guint8 const *)end - data + 1) != name_len) {
			/* be wary about endianness */
			for (i = 0 ; i < name_len ; i += 2)
				uni_name [i/2] = GSF_LE_GET_GUINT16 (data + i);
			uni_name [i/2] = 0;

			dirent->name = g_utf16_to_utf8 (uni_name, -1, NULL, NULL, NULL);
		} else
			dirent->name = g_strndup ((gchar *)data, (gsize)((guint8 const *)end - data + 1));
	}
	/* be really anal in the face of screwups */
	if (dirent->name == NULL)
		dirent->name = g_strdup ("");
	dirent->collation_name = g_utf8_collate_key (dirent->name, -1);

#if 0
	printf ("%c '%s' :\tsize = %d\tfirst_block = 0x%x\n",
		dirent->is_directory ? 'd' : ' ',
		dirent->name, dirent->size, dirent->first_block);
#endif

	if (parent != NULL)
		parent->children = g_list_insert_sorted (parent->children,
			dirent, (GCompareFunc)ole_dirent_cmp);

	/* NOTE : These links are a tree, not a linked list */
	if (prev == entry) {
		g_warning ("Invalid OLE file with a cycle in its directory tree");
	} else
		ole_dirent_new (ole, prev, parent); 
	if (next == entry) {
		g_warning ("Invalid OLE file with a cycle in its directory tree");
	} else
		ole_dirent_new (ole, next, parent); 

	if (dirent->is_directory)
		ole_dirent_new (ole, child, dirent);
	else if (child != DIRENT_MAGIC_END)
		g_warning ("A non directory stream with children ?");

	return dirent;
}

static void
ole_dirent_free (MSOleDirent *dirent)
{
	GList *tmp;
	g_return_if_fail (dirent != NULL);

	g_free (dirent->name);
	g_free (dirent->collation_name);

	for (tmp = dirent->children; tmp; tmp = tmp->next)
		ole_dirent_free ((MSOleDirent *)tmp->data);
	g_list_free (dirent->children);
	g_free (dirent);
}

/*****************************************************************************/

static void
ole_info_unref (MSOleInfo *info)
{
	if (info->ref_count-- != 1)
		return;

	ols_bat_release (&info->bb.bat);
	ols_bat_release (&info->sb.bat);
	if (info->root_dir != NULL) {
		ole_dirent_free (info->root_dir);
		info->root_dir = NULL;
	}
	if (info->sb_file != NULL)  {
		g_object_unref (G_OBJECT (info->sb_file));
		info->sb_file = NULL;
	}
	g_free (info);
}

static MSOleInfo *
ole_info_ref (MSOleInfo *info)
{
	info->ref_count++;
	return info;
}

/**
 * ole_dup :
 * @src :
 *
 * Utility routine to _partially_ replicate a file.  It does NOT copy the bat
 * blocks, or init the dirent.
 *
 * Return value: the partial duplicate.
 **/
static GsfInfileMSOle *
ole_dup (GsfInfileMSOle const *src, GError **err)
{
	GsfInfileMSOle	*dst;
	GsfInput *input;

	g_return_val_if_fail (src != NULL, NULL);

	input = gsf_input_dup (src->input, err);
	if (input == NULL) {
		if (err != NULL)
			*err = g_error_new (gsf_input_error_id (), 0,
					    "Failed to duplicate input stream");
		return NULL;
	}

	dst = (GsfInfileMSOle *)g_object_new (GSF_INFILE_MSOLE_TYPE, NULL);
	dst->input = input;
	dst->info  = ole_info_ref (src->info);

	/* buf and buf_size are initialized to NULL */

	return dst;
}

/**
 * ole_init_info :
 * @ole :
 * @err : optionally NULL
 *
 * Read an OLE header and do some sanity checking
 * along the way.
 *
 * Return value: TRUE on error setting @err if it is supplied.
 **/
static gboolean
ole_init_info (GsfInfileMSOle *ole, GError **err)
{
	static guint8 const signature[] =
		{ 0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1 };
	guint8 const *header, *tmp;
	guint32 *metabat = NULL;
	MSOleInfo *info;
	guint32 bb_shift, sb_shift, num_bat, num_metabat, last, dirent_start;
	guint32 metabat_block, *ptr;

	/* check the header */
	if (gsf_input_seek (ole->input, 0, G_SEEK_SET) ||
	    NULL == (header = gsf_input_read (ole->input, OLE_HEADER_SIZE, NULL)) ||
	    0 != memcmp (header, signature, sizeof (signature))) {
		if (err != NULL)
			*err = g_error_new (gsf_input_error_id (), 0,
				"No OLE2 signature");
		return TRUE;
	}

	bb_shift      = GSF_LE_GET_GUINT16 (header + OLE_HEADER_BB_SHIFT);
	sb_shift      = GSF_LE_GET_GUINT16 (header + OLE_HEADER_SB_SHIFT);
	num_bat	      = GSF_LE_GET_GUINT32 (header + OLE_HEADER_NUM_BAT);
	dirent_start  = GSF_LE_GET_GUINT32 (header + OLE_HEADER_DIRENT_START);
        metabat_block = GSF_LE_GET_GUINT32 (header + OLE_HEADER_METABAT_BLOCK);
	num_metabat   = GSF_LE_GET_GUINT32 (header + OLE_HEADER_NUM_METABAT);

	/* Some sanity checks
	 * 1) There should always be at least 1 BAT block
	 * 2) It makes no sense to have a block larger than 2^31 for now.
	 *    Maybe relax this later, but not much.
	 */
	if (6 > bb_shift || bb_shift >= 31 || sb_shift > bb_shift) {
		if (err != NULL)
			*err = g_error_new (gsf_input_error_id (), 0,
				"Unreasonable block sizes");
		return TRUE;
	}

	info = g_new0 (MSOleInfo, 1);
	ole->info = info;

	info->ref_count	     = 1;
	info->bb.shift	     = bb_shift;
	info->bb.size	     = 1 << info->bb.shift;
	info->bb.filter	     = info->bb.size - 1;
	info->sb.shift	     = sb_shift;
	info->sb.size	     = 1 << info->sb.shift;
	info->sb.filter	     = info->sb.size - 1;
	info->threshold	     = GSF_LE_GET_GUINT32 (header + OLE_HEADER_THRESHOLD);
        info->sbat_start     = GSF_LE_GET_GUINT32 (header + OLE_HEADER_SBAT_START);
        info->num_sbat       = GSF_LE_GET_GUINT32 (header + OLE_HEADER_NUM_SBAT);
	info->max_block	     = (gsf_input_size (ole->input) - OLE_HEADER_SIZE) / info->bb.size;
	info->sb_file	     = NULL;

	if (info->num_sbat == 0 && info->sbat_start != BAT_MAGIC_END_OF_CHAIN) {
		g_warning ("There is are not supposed to be any blocks in the small block allocation table, yet there is a link to some.  Ignoring it.");
	}

	/* very rough heuristic, just in case */
	if (num_bat < info->max_block) {
		info->bb.bat.num_blocks = num_bat * (info->bb.size / BAT_INDEX_SIZE);
		info->bb.bat.block	= g_new0 (guint32, info->bb.bat.num_blocks);

		metabat = (guint32 *)g_alloca (MAX (info->bb.size, OLE_HEADER_SIZE));

		/* Reading the elements invalidates this memory, make copy */
		gsf_ole_get_guint32s (metabat, header + OLE_HEADER_START_BAT,
			OLE_HEADER_SIZE - OLE_HEADER_START_BAT);
		last = num_bat;
		if (last > OLE_HEADER_METABAT_SIZE)
			last = OLE_HEADER_METABAT_SIZE;

		ptr = ole_info_read_metabat (ole, info->bb.bat.block,
			info->bb.bat.num_blocks, metabat, metabat + last);
		num_bat -= last;
	} else
		ptr = NULL;

	last = (info->bb.size - BAT_INDEX_SIZE) / BAT_INDEX_SIZE;
	while (ptr != NULL && num_metabat-- > 0) {
		tmp = ole_get_block (ole, metabat_block, NULL);
		if (tmp == NULL) {
			ptr = NULL;
			break;
		}

		/* Reading the elements invalidates this memory, make copy */
		gsf_ole_get_guint32s (metabat, tmp, (int)info->bb.size);

		if (num_metabat == 0) {
			if (last < num_bat) {
				/* there should be less that a full metabat block
				 * remaining */
				ptr = NULL;
				break;
			}
			last = num_bat;
		} else if (num_metabat > 0) {
			metabat_block = metabat[last];
			num_bat -= last;
		}

		ptr = ole_info_read_metabat (ole, ptr,
			info->bb.bat.num_blocks, metabat, metabat + last);
	}

	if (ptr == NULL) {
		if (err != NULL)
			*err = g_error_new (gsf_input_error_id (), 0,
				"Inconsistent block allocation table");
		return TRUE;
	}

	/* Read the directory's bat, we do not know the size */
	if (ole_make_bat (&info->bb.bat, 0, dirent_start, &ole->bat)) {
		if (err != NULL)
			*err = g_error_new (gsf_input_error_id (), 0,
				"Problems making block allocation table");
		return TRUE;
	}

	/* Read the directory */
	ole->dirent = info->root_dir = ole_dirent_new (ole, 0, NULL);
	if (ole->dirent == NULL) {
		if (err != NULL)
			*err = g_error_new (gsf_input_error_id (), 0,
				"Problems reading directory");
		return TRUE;
	}

	return FALSE;
}

static void
gsf_infile_msole_finalize (GObject *obj)
{
	GsfInfileMSOle *ole = GSF_INFILE_MSOLE (obj);

	if (ole->input != NULL) {
		g_object_unref (G_OBJECT (ole->input));
		ole->input = NULL;
	}
	if (ole->info != NULL &&
	    ole->info->sb_file != (GsfInput *)ole) {
		ole_info_unref (ole->info);
		ole->info = NULL;
	}
	ols_bat_release (&ole->bat);

	g_free (ole->stream.buf);

	parent_class->finalize (obj);
}

static GsfInput *
gsf_infile_msole_dup (GsfInput *src_input, GError **err)
{
	GsfInfileMSOle const *src = GSF_INFILE_MSOLE (src_input);
	GsfInfileMSOle *dst = ole_dup (src, err);

	if (dst == NULL)
		return NULL;

	if (src->bat.block != NULL) {
		dst->bat.block = g_new (guint32, src->bat.num_blocks),
		memcpy (dst->bat.block, src->bat.block,
			sizeof (guint32) * src->bat.num_blocks);
	}
	dst->bat.num_blocks = src->bat.num_blocks;
	dst->dirent = src->dirent;

	return GSF_INPUT (dst);
}

static guint8 const *
gsf_infile_msole_read (GsfInput *input, size_t num_bytes, guint8 *buffer)
{
	GsfInfileMSOle *ole = GSF_INFILE_MSOLE (input);
	gsf_off_t first_block, last_block, raw_block, offset, i;
	guint8 const *data;
	guint8 *ptr;
	size_t count;

	/* small block files are preload */
	if (ole->dirent != NULL && ole->dirent->use_sb) {
		if (buffer != NULL) {
			memcpy (buffer, ole->stream.buf + input->cur_offset, num_bytes);
			return buffer;
		}
		return ole->stream.buf + input->cur_offset;
	}

	/* GsfInput guarantees that num_bytes > 0 */
	first_block = OLE_BIG_BLOCK (input->cur_offset, ole);
	last_block = OLE_BIG_BLOCK (input->cur_offset + num_bytes - 1, ole);
	offset = input->cur_offset & ole->info->bb.filter;

	/* optimization : are all the raw blocks contiguous */
	i = first_block;
	raw_block = ole->bat.block [i];
	while (++i <= last_block && ++raw_block == ole->bat.block [i])
		;
	if (i > last_block) {
		/* optimization don't seek if we don't need to */
		if (ole->cur_block != first_block) {
			if (gsf_input_seek (ole->input,
				(gsf_off_t)(MAX (OLE_HEADER_SIZE, ole->info->bb.size) + (ole->bat.block [first_block] << ole->info->bb.shift) + offset),
				G_SEEK_SET) < 0)
				return NULL;
		}
		ole->cur_block = last_block;
		return gsf_input_read (ole->input, num_bytes, buffer);
	}

	/* damn, we need to copy it block by block */
	if (buffer == NULL) {
		if (ole->stream.buf_size < num_bytes) {
			if (ole->stream.buf != NULL)
				g_free (ole->stream.buf);
			ole->stream.buf_size = num_bytes;
			ole->stream.buf = g_new (guint8, num_bytes);
		}
		buffer = ole->stream.buf;
	}

	ptr = buffer;
	for (i = first_block ; i <= last_block ; i++ , ptr += count, num_bytes -= count) {
		count = ole->info->bb.size - offset;
		if (count > num_bytes)
			count = num_bytes;
		data = ole_get_block (ole, ole->bat.block [i], NULL);
		if (data == NULL)
			return NULL;

		/* TODO : this could be optimized to avoid the copy */
		memcpy (ptr, data + offset, count);
		offset = 0;
	}
	ole->cur_block = BAT_MAGIC_UNUSED;

	return buffer;
}

static gboolean
gsf_infile_msole_seek (GsfInput *input, gsf_off_t offset, GSeekType whence)
{
	GsfInfileMSOle *ole = GSF_INFILE_MSOLE (input);

	(void) offset; 
	(void) whence;

	ole->cur_block = BAT_MAGIC_UNUSED;
	return FALSE;
}

static GsfInput *
gsf_infile_msole_new_child (GsfInfileMSOle *parent,
			    MSOleDirent *dirent, GError **err)
{
	GsfInfileMSOle *child;
	MSOleInfo *info;
	MSOleBAT const *metabat;
	GsfInput *sb_file = NULL;
	size_t size_guess;

	child = ole_dup (parent, err);
	if (!child)
		return NULL;

	child->dirent = dirent;
	gsf_input_set_size (GSF_INPUT (child), (gsf_off_t) dirent->size);

	/* The root dirent defines the small block file */
	if (dirent->index != 0) {
		gsf_input_set_name (GSF_INPUT (child), dirent->name);
		gsf_input_set_container (GSF_INPUT (child), GSF_INFILE (parent));

		if (dirent->is_directory) {
			/* be wary.  It seems as if some implementations pretend that the
			 * directories contain data */
			gsf_input_set_size (GSF_INPUT (child), 0);
			return GSF_INPUT (child);
		}
	}

	info = parent->info;

	/* build the bat */
	if (dirent->use_sb) {
		metabat = &info->sb.bat;
		size_guess = dirent->size >> info->sb.shift;
		sb_file = ole_info_get_sb_file (parent);
		if (!sb_file) {
			if (err != NULL)
				*err = g_error_new (gsf_input_error_id (), 0,
						    "Failed to access child");
			g_object_unref (G_OBJECT (child));
			return NULL;
		}
	} else {
		metabat = &info->bb.bat;
		size_guess = dirent->size >> info->bb.shift;
	}
	if (ole_make_bat (metabat, size_guess + 1, dirent->first_block, &child->bat)) {
		g_object_unref (G_OBJECT (child));
		return NULL;
	}

	if (dirent->use_sb) {
		unsigned i;
		guint8 const *data;

		g_return_val_if_fail (sb_file != NULL, NULL);

		child->stream.buf_size = info->threshold;
		child->stream.buf = g_new (guint8, info->threshold);

		for (i = 0 ; i < child->bat.num_blocks; i++)
			if (gsf_input_seek (GSF_INPUT (sb_file),
				(gsf_off_t)(child->bat.block [i] << info->sb.shift), G_SEEK_SET) < 0 ||
			    (data = gsf_input_read (GSF_INPUT (sb_file),
				info->sb.size, 
				child->stream.buf + (i << info->sb.shift))) == NULL) {

				g_warning ("failure reading block %d", i);

				g_object_unref (G_OBJECT (child));
				return NULL;
			}
	}

	return GSF_INPUT (child);
}

static GsfInput *
gsf_infile_msole_child_by_index (GsfInfile *infile, int target, GError **err)
{
	GsfInfileMSOle *ole = GSF_INFILE_MSOLE (infile);
	GList *p;

	for (p = ole->dirent->children; p != NULL ; p = p->next)
		if (target-- <= 0)
			return gsf_infile_msole_new_child (ole,
				(MSOleDirent *)p->data, err);
	return NULL;
}

static char const *
gsf_infile_msole_name_by_index (GsfInfile *infile, int target)
{
	GsfInfileMSOle *ole = GSF_INFILE_MSOLE (infile);
	GList *p;

	for (p = ole->dirent->children; p != NULL ; p = p->next)
		if (target-- <= 0)
			return ((MSOleDirent *)p->data)->name;
	return NULL;
}

static GsfInput *
gsf_infile_msole_child_by_name (GsfInfile *infile, char const *name, GError **err)
{
	GsfInfileMSOle *ole = GSF_INFILE_MSOLE (infile);
	GList *p;

	for (p = ole->dirent->children; p != NULL ; p = p->next) {
		MSOleDirent *dirent = p->data;
		if (dirent->name != NULL && !strcmp (name, dirent->name))
			return gsf_infile_msole_new_child (ole, dirent, err);
	}
	return NULL;
}

static int
gsf_infile_msole_num_children (GsfInfile *infile)
{
	GsfInfileMSOle *ole = GSF_INFILE_MSOLE (infile);

	g_return_val_if_fail (ole->dirent != NULL, -1);

	if (!ole->dirent->is_directory)
		return -1;
	return g_list_length (ole->dirent->children);
}

static void
gsf_infile_msole_init (GObject *obj)
{
	GsfInfileMSOle *ole = GSF_INFILE_MSOLE (obj);

	ole->input		= NULL;
	ole->info		= NULL;
	ole->bat.block		= NULL;
	ole->bat.num_blocks	= 0;
	ole->cur_block		= BAT_MAGIC_UNUSED;
	ole->stream.buf		= NULL;
	ole->stream.buf_size	= 0; 
}

static void
gsf_infile_msole_class_init (GObjectClass *gobject_class)
{
	GsfInputClass  *input_class  = GSF_INPUT_CLASS (gobject_class);
	GsfInfileClass *infile_class = GSF_INFILE_CLASS (gobject_class);

	gobject_class->finalize		= gsf_infile_msole_finalize;
	input_class->Dup		= gsf_infile_msole_dup;
	input_class->Read		= gsf_infile_msole_read;
	input_class->Seek		= gsf_infile_msole_seek;
	infile_class->num_children	= gsf_infile_msole_num_children;
	infile_class->name_by_index	= gsf_infile_msole_name_by_index;
	infile_class->child_by_index	= gsf_infile_msole_child_by_index;
	infile_class->child_by_name	= gsf_infile_msole_child_by_name;

	parent_class = g_type_class_peek_parent (gobject_class);
}

GSF_CLASS (GsfInfileMSOle, gsf_infile_msole,
	   gsf_infile_msole_class_init, gsf_infile_msole_init,
	   GSF_INFILE_TYPE)

/**
 * gsf_infile_msole_new :
 * @source :
 * @err   :
 *
 * Opens the root directory of an MS OLE file.
 * NOTE : adds a reference to @source
 *
 * Returns : the new ole file handler
 **/
GsfInfile *
gsf_infile_msole_new (GsfInput *source, GError **err)
{
	GsfInfileMSOle *ole;
	gsf_off_t calling_pos;

	g_return_val_if_fail (GSF_IS_INPUT (source), NULL);

	ole = (GsfInfileMSOle *)g_object_new (GSF_INFILE_MSOLE_TYPE, NULL);
	ole->input = gsf_input_proxy_new (source);
	gsf_input_set_size (GSF_INPUT (ole), 0);

	calling_pos = gsf_input_tell (source);
	if (ole_init_info (ole, err)) {
		/* It's not clear to me why we do this.  And if this
		   fails, there's really nothing we can do.  */
		(void)gsf_input_seek (source, calling_pos, G_SEEK_SET);

		g_object_unref (G_OBJECT (ole));
		return NULL;
	}

	return GSF_INFILE (ole);
}

/**
 * gsf_infile_msole_get_class_id :
 * @ole: a #GsfInfileMSOle
 * @res: 16 byte identifier (often a GUID in MS Windows apps)
 *
 * Retrieves the 16 byte indentifier (often a GUID in MS Windows apps)
 * stored within the directory associated with @ole and stores it in @res.
 *
 * Returns TRUE on success
 **/
gboolean
gsf_infile_msole_get_class_id (GsfInfileMSOle const *ole, guint8 *res)
{
	g_return_val_if_fail (ole != NULL && ole->dirent != NULL, FALSE);

	memcpy (res, ole->dirent->clsid,
		sizeof(ole->dirent->clsid));
	return TRUE;
}
