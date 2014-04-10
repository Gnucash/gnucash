/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-infile-zip.c :
 *
 * Copyright (C) 2002-2004 Jody Goldberg (jody@gnome.org)
 *                    Tambet Ingo   (tambet@ximian.com)
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

#include <gsf-config.h>
#include <gsf/gsf-infile-impl.h>
#include <gsf/gsf-infile-zip.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-zip-impl.h>
#include <gsf/gsf-input-proxy.h>

#include <string.h>
#include <zlib.h>

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "libgsf:zip"

enum {
	PROP_0,
	PROP_SOURCE,
	PROP_COMPRESSION_LEVEL,
	PROP_INTERNAL_PARENT,
};

static GObjectClass *parent_class;

typedef struct {
	guint16     entries;
	guint32     dir_pos;
	GList	   *dirent_list;
	GsfZipVDir *vdir;

	int ref_count;
} ZipInfo;

struct _GsfInfileZip {
	GsfInfile parent;

	GsfInput  *source;
	ZipInfo	  *info;

	GsfZipVDir   *vdir;

	z_stream  *stream;
	guint32   restlen;
	guint32   crestlen;
	
	guint8   *buf;
	size_t    buf_size;
	gsf_off_t seek_skipped;

	GError *err;
	GsfInfileZip *dup_parent;
};

typedef struct {
	GsfInfileClass  parent_class;
} GsfInfileZipClass;

#define GSF_INFILE_ZIP_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST ((k), GSF_INFILE_ZIP_TYPE, GsfInfileZipClass))
#define GSF_IS_INFILE_ZIP_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), GSF_INFILE_ZIP_TYPE))

static GsfZipVDir *
vdir_child_by_name (GsfZipVDir *vdir, char const *name)
{
	GSList *l;

	for (l = vdir->children; l; l = l->next) {
		GsfZipVDir *child = (GsfZipVDir *) l->data;
		if (strcmp (child->name, name) == 0)
			return child;
	}
	return NULL;
}

static GsfZipVDir *
vdir_child_by_index (GsfZipVDir *vdir, int target)
{
	return g_slist_nth_data (vdir->children, target);
}

static void
vdir_insert (GsfZipVDir *vdir, char const * name, GsfZipDirent *dirent)
{
	char const *p;
	char *dirname;
	GsfZipVDir *child;
	
	p = strchr (name, ZIP_NAME_SEPARATOR);
	if (p) {	/* A directory */
		dirname = g_strndup (name, (gsize) (p - name));
		child = vdir_child_by_name (vdir, dirname);
		if (!child) {
			child = gsf_vdir_new (dirname, TRUE, NULL);
			gsf_vdir_add_child (vdir, child);
		}
		g_free (dirname);
		if (*(p+1) != '\0') {
			name = p+1;
			vdir_insert (child, name, dirent);
		}
	} else { /* A simple file name */
		child = gsf_vdir_new (name, FALSE, dirent);
		gsf_vdir_add_child (vdir, child);
	}
}

static gsf_off_t
zip_find_trailer (GsfInfileZip *zip)
{
	static guint8 const trailer_signature[] =
		{ 'P', 'K', 0x05, 0x06 };
	gsf_off_t offset, trailer_offset, filesize;
	gsf_off_t maplen;
	guint8 const *data;

	filesize = gsf_input_size (zip->source);
	if (filesize < ZIP_TRAILER_SIZE)
		return -1;

	trailer_offset = filesize;
	maplen = filesize & (ZIP_BUF_SIZE - 1);
	if (maplen == 0)
		maplen = ZIP_BUF_SIZE;
	offset = filesize - maplen; /* offset is now BUFSIZ aligned */

	while (TRUE) {
		guchar *p, *s;

		if (gsf_input_seek (zip->source, offset, G_SEEK_SET))
			return -1;

		if ((data = gsf_input_read (zip->source, maplen, NULL)) == NULL)
			return -1;

		p = (guchar *) data;
        
		for (s = p + maplen - 1; (s >= p); s--, trailer_offset--) {
			if ((*s == 'P') &&
			    (p + maplen - 1 - s > ZIP_TRAILER_SIZE - 2) &&
			    !memcmp (s, trailer_signature, sizeof (trailer_signature))) {
				return --trailer_offset;
			}
		}
        
		/* not found in currently mapped block, so update it if
		 * there is some room in before. The requirements are..
		 * (a) mappings should overlap so that trailer can cross BUFSIZ-boundary
		 * (b) trailer cannot be farther away than 64K from fileend
		 */

		/* outer loop cond */
		if (offset <= 0)
			return -1;

		/* outer loop step */
		offset -= ZIP_BUF_SIZE / 2;
		maplen = MIN (filesize - offset, ZIP_BUF_SIZE);
		trailer_offset = offset + maplen;

		if (filesize - offset > 64 * 1024)
			return -1;
	} /*outer loop*/

	return -1;
}

static GsfZipDirent *
zip_dirent_new_in (GsfInfileZip *zip, gsf_off_t *offset)
{
	static guint8 const dirent_signature[] =
		{ 'P', 'K', 0x01, 0x02 };
	GsfZipDirent *dirent;
	guint8 const *data;
	guint16 name_len, extras_len, comment_len, compr_method;
	guint32 crc32, csize, usize, off;
	gchar *name;

	/* Read data and check the header */
	if (gsf_input_seek (zip->source, *offset, G_SEEK_SET) ||
	    NULL == (data = gsf_input_read (zip->source, ZIP_DIRENT_SIZE, NULL)) ||
	    0 != memcmp (data, dirent_signature, sizeof (dirent_signature))) {
		return NULL;
	}

	name_len =      GSF_LE_GET_GUINT16 (data + ZIP_DIRENT_NAME_SIZE);
	extras_len =    GSF_LE_GET_GUINT16 (data + ZIP_DIRENT_EXTRAS_SIZE);
	comment_len =   GSF_LE_GET_GUINT16 (data + ZIP_DIRENT_COMMENT_SIZE);

	compr_method =  GSF_LE_GET_GUINT16 (data + ZIP_DIRENT_COMPR_METHOD);
	crc32 =         GSF_LE_GET_GUINT32 (data + ZIP_DIRENT_CRC32);
	csize =         GSF_LE_GET_GUINT32 (data + ZIP_DIRENT_CSIZE);
	usize =         GSF_LE_GET_GUINT32 (data + ZIP_DIRENT_USIZE);
	off =           GSF_LE_GET_GUINT32 (data + ZIP_DIRENT_OFFSET);

	if ((data = gsf_input_read (zip->source, name_len, NULL)) == NULL)
		return NULL;

	name = g_new (gchar, (gulong) (name_len + 1));
	memcpy (name, data, name_len);
	name[name_len] = '\0';

	dirent = gsf_zip_dirent_new ();
	dirent->name = name;

	dirent->compr_method =  compr_method;
	dirent->crc32 =         crc32;
	dirent->csize =         csize;
	dirent->usize =         usize;
	dirent->offset =        off;

	*offset += ZIP_DIRENT_SIZE + name_len + extras_len + comment_len;

	return dirent;
}

/*****************************************************************************/
static ZipInfo *
zip_info_ref (ZipInfo *info)
{
	info->ref_count++;
	return info;
}

static void
zip_info_unref (ZipInfo *info)
{
	GList *p;

	if (info->ref_count-- != 1)
		return;

	gsf_vdir_free (info->vdir, FALSE);
	for (p = info->dirent_list; p != NULL; p = p->next)
		gsf_zip_dirent_free ((GsfZipDirent *) p->data);

	g_list_free (info->dirent_list);

	g_free (info);
}

/**
 * zip_dup :
 * @src :
 *
 * Return value: the partial duplicate.
 **/
static GsfInfileZip *
zip_dup (GsfInfileZip const *src, GError **err)
{
	GsfInfileZip *dst;

	g_return_val_if_fail (src != NULL, NULL);

	dst = g_object_new (GSF_INFILE_ZIP_TYPE,
			    "internal-parent", src,
			    NULL);

	if (dst->err) {
		if (err)
			*err = g_error_copy (dst->err);
		g_object_unref (dst);
		return NULL;
	}

	return dst;
}

/**
 * zip_read_dirents:
 * @zip :
 *
 * Read zip headers and do some sanity checking
 * along the way.
 *
 * Return value: TRUE on error setting zip->err.
 **/
static gboolean
zip_read_dirents (GsfInfileZip *zip)
{
	guint8 const *trailer;
	guint16 entries, i;
	guint32 dir_pos;
	ZipInfo *info;
	gsf_off_t offset;

	/* Find and check the trailing header */
	offset = zip_find_trailer (zip);
	if (offset < 0) {
		zip->err = g_error_new (gsf_input_error_id (), 0,
					"No Zip trailer");
		return TRUE;
	}

	if (gsf_input_seek (zip->source, offset, G_SEEK_SET) ||
	    NULL == (trailer = gsf_input_read (zip->source, ZIP_TRAILER_SIZE, NULL))) {
		zip->err = g_error_new (gsf_input_error_id (), 0,
					"Error reading Zip signature");
		return TRUE;
	}

	entries      = GSF_LE_GET_GUINT32 (trailer + ZIP_TRAILER_ENTRIES);
	dir_pos      = GSF_LE_GET_GUINT32 (trailer + ZIP_TRAILER_DIR_POS);

	info = g_new0 (ZipInfo, 1);
	zip->info = info;

	info->ref_count    = 1;
	info->entries      = entries;
	info->dir_pos      = dir_pos;

	/* Read the directory */
	for (i = 0, offset = dir_pos; i < entries; i++) {
		GsfZipDirent *d;

		d = zip_dirent_new_in (zip, &offset);
		if (d == NULL) {
			zip->err = g_error_new (gsf_input_error_id (), 0,
						"Error reading zip dirent");
			return TRUE;
		}

		info->dirent_list = g_list_append (info->dirent_list, d);
	}

	return FALSE;
}

static void
zip_build_vdirs (GsfInfileZip *zip)
{
	GList *l;
	GsfZipDirent *dirent;
	ZipInfo *info = zip->info;

	info->vdir = gsf_vdir_new ("", TRUE, NULL);
	for (l = info->dirent_list; l; l = l->next) {
		dirent = (GsfZipDirent *) l->data;
		vdir_insert (info->vdir, dirent->name, dirent);
	}
}

/**
 * zip_init_info :
 * @zip :
 *
 * Read zip headers and do some sanity checking
 * along the way.
 *
 * Return value: TRUE on error setting zip->err.
 **/
static gboolean
zip_init_info (GsfInfileZip *zip)
{
	gboolean ret;
	
	ret = zip_read_dirents (zip);
	if (ret != FALSE)
		return ret;
	zip_build_vdirs (zip);

	return FALSE;
}

/* returns TRUE on error */
static gboolean
zip_child_init (GsfInfileZip *child, GError **errmsg)
{
	static guint8 const header_signature[] =
		{ 'P', 'K', 0x03, 0x04 };
	guint8 const *data;
	guint16 name_len, extras_len;
	GsfZipDirent *dirent = child->vdir->dirent;

	/* skip local header
	 * should test tons of other info, but trust that those are correct
	 **/

	if (gsf_input_seek (child->source, (gsf_off_t) dirent->offset, G_SEEK_SET) ||
	    NULL == (data = gsf_input_read (child->source, ZIP_FILE_HEADER_SIZE, NULL)) ||
	    0 != memcmp (data, header_signature, sizeof (header_signature))) {
		if (errmsg != NULL)
			*errmsg = g_error_new (gsf_input_error_id (), 0,
					       "Unable to read zip header.");
		return TRUE;
	}

	name_len =   GSF_LE_GET_GUINT16 (data + ZIP_FILE_HEADER_NAME_SIZE);
	extras_len = GSF_LE_GET_GUINT16 (data + ZIP_FILE_HEADER_EXTRAS_SIZE);

	dirent->data_offset = dirent->offset + ZIP_FILE_HEADER_SIZE + name_len + extras_len;
	child->restlen  = dirent->usize;
	child->crestlen = dirent->csize;

	if (dirent->compr_method != GSF_ZIP_STORED) {
		int err;

		if (!child->stream)
			child->stream = g_new0 (z_stream, 1);

		err = inflateInit2 (child->stream, -MAX_WBITS);
		if (err != Z_OK) {
			if (errmsg != NULL)
				*errmsg = g_error_new (gsf_input_error_id (), 0,
						       "problem uncompressing stream");
			return TRUE;
		}
	}

	return FALSE;
}

/* GsfInput class functions */

static GsfInput *
gsf_infile_zip_dup (GsfInput *src_input, GError **err)
{
	GsfInfileZip const *src = GSF_INFILE_ZIP (src_input);
	GsfInfileZip *dst = zip_dup (src, err);

	if (dst == NULL)
		return NULL;

	dst->vdir = src->vdir;

	if (dst->vdir->dirent && zip_child_init (dst, err)) {
		g_object_unref (dst);
		return NULL;
	}

	return GSF_INPUT (dst);
}

static gboolean
zip_update_stream_in (GsfInfileZip *zip)
{
	guint32 read_now;
	guint8 const *data;
	gsf_off_t pos;
	
	if (zip->crestlen == 0)
		return FALSE;

	read_now = MIN (zip->crestlen, ZIP_BLOCK_SIZE);

	pos = zip->vdir->dirent->data_offset + zip->stream->total_in;
	if (gsf_input_seek (zip->source, pos, G_SEEK_SET))
		return FALSE;
	if ((data = gsf_input_read (zip->source, read_now, NULL)) == NULL)
		return FALSE;

	zip->crestlen -= read_now;
	zip->stream->next_in  = (unsigned char *) data;      /* next input byte */
	zip->stream->avail_in = read_now;  /* number of bytes available at next_in */

	return TRUE;
}

static guint8 const *
gsf_infile_zip_read (GsfInput *input, size_t num_bytes, guint8 *buffer)
{
	GsfInfileZip *zip = GSF_INFILE_ZIP (input);
	GsfZipVDir      *vdir = zip->vdir;
	gsf_off_t pos;

	if (zip->restlen < num_bytes)
		return NULL;

	switch (vdir->dirent->compr_method) {
	case GSF_ZIP_STORED:
		zip->restlen -= num_bytes;
		pos = zip->vdir->dirent->data_offset + input->cur_offset;
		if (gsf_input_seek (zip->source, pos, G_SEEK_SET))
			return NULL;
		return gsf_input_read (zip->source, num_bytes, buffer);

	case GSF_ZIP_DEFLATED:
		if (buffer == NULL) {
			if (zip->buf_size < num_bytes) {
				zip->buf_size = MAX (num_bytes, 256);
				g_free (zip->buf);
				zip->buf = g_new (guint8, zip->buf_size);
			}
			buffer = zip->buf;
		}

		zip->stream->avail_out = num_bytes;
		zip->stream->next_out = (unsigned char *)buffer;

		do {
			int err;
			int startlen;

			if (zip->crestlen > 0 && zip->stream->avail_in == 0)
				if (!zip_update_stream_in (zip))
					break;

			startlen = zip->stream->total_out;
			err = inflate(zip->stream, Z_NO_FLUSH);

			if (err == Z_STREAM_END) 
				zip->restlen = 0;
			else if (err == Z_OK)
				zip->restlen -= (zip->stream->total_out - startlen);
			else
				break;

		} while (zip->restlen && zip->stream->avail_out);

		return buffer;

	default:
		break;
	}

	return NULL;
}

static gboolean
gsf_infile_zip_seek (GsfInput *input, gsf_off_t offset, GSeekType whence)
{
	GsfInfileZip *zip = GSF_INFILE_ZIP (input);
	/* Global flag -- we don't want one per stream.  */
	static gboolean warned = FALSE;
	gsf_off_t pos = offset;

	/* Note, that pos has already been sanity checked.  */
	switch (whence) {
	case G_SEEK_SET : break;
	case G_SEEK_CUR : pos += input->cur_offset;	break;
	case G_SEEK_END : pos += input->size;		break;
	default : return TRUE;
	}

	if (zip->stream) {
		zip->stream->next_in  = NULL;
		zip->stream->avail_in = 0;
		zip->stream->total_in = 0;
	}

	if (zip_child_init (zip, NULL)) {
		g_warning ("failure initializing zip child");
		return TRUE;
	}

	input->cur_offset = 0;
	if (gsf_input_seek_emulate (input, pos))
		return TRUE;

	zip->seek_skipped += pos;
	if (!warned &&
	    zip->seek_skipped != pos && /* Don't warn for single seek.  */
	    zip->seek_skipped >= 1000000) {
		warned = TRUE;
		g_warning ("Seeking in zip child streams is awfully slow.");
	}

	return FALSE;
}

/* GsfInfile class functions */

/*****************************************************************************/


static GsfInput *
gsf_infile_zip_new_child (GsfInfileZip *parent, GsfZipVDir *vdir, GError **err)
{
	GsfInfileZip *child;
	GsfZipDirent *dirent = vdir->dirent;
	child = zip_dup (parent, err);

	if (child == NULL)
		return NULL;

	gsf_input_set_name (GSF_INPUT (child), vdir->name);
	gsf_input_set_container (GSF_INPUT (child), GSF_INFILE (parent));

	child->vdir = vdir;

	if (dirent) {
		gsf_input_set_size (GSF_INPUT (child),
				    (gsf_off_t) dirent->usize);
		if (zip_child_init (child, err) != FALSE) {
			g_object_unref (child);
			return NULL;
		}
	} else
		gsf_input_set_size (GSF_INPUT (child), 0);

	return GSF_INPUT (child);
}

static GsfInput *
gsf_infile_zip_child_by_index (GsfInfile *infile, int target, GError **err)
{
	GsfInfileZip *zip = GSF_INFILE_ZIP (infile);
	GsfZipVDir *child_vdir = vdir_child_by_index (zip->vdir, target);

	if (child_vdir)
		return gsf_infile_zip_new_child (zip, child_vdir, err);

	return NULL;
}

static char const *
gsf_infile_zip_name_by_index (GsfInfile *infile, int target)
{
	GsfInfileZip *zip = GSF_INFILE_ZIP (infile);
	GsfZipVDir *child_vdir = vdir_child_by_index (zip->vdir, target);

	if (child_vdir)
		return child_vdir->name;

	return NULL;
}

static GsfInput *
gsf_infile_zip_child_by_name (GsfInfile *infile, char const *name, GError **err)
{
	GsfInfileZip *zip = GSF_INFILE_ZIP (infile);
	GsfZipVDir *child_vdir = vdir_child_by_name (zip->vdir, name);

	if (child_vdir)
		return gsf_infile_zip_new_child (zip, child_vdir, err);

	return NULL;
}

static int
gsf_infile_zip_num_children (GsfInfile *infile)
{
	GsfInfileZip *zip = GSF_INFILE_ZIP (infile);

	g_return_val_if_fail (zip->vdir != NULL, -1);

	if (!zip->vdir->is_directory)
		return -1;
	return g_slist_length (zip->vdir->children);
}

static void
gsf_infile_zip_finalize (GObject *obj)
{
	GsfInfileZip *zip = GSF_INFILE_ZIP (obj);

	if (zip->source != NULL) {
		g_object_unref (G_OBJECT (zip->source));
		zip->source = NULL;
	}
	if (zip->info != NULL) {
		zip_info_unref (zip->info);
		zip->info = NULL;
	}

	if (zip->stream)
		(void) inflateEnd (zip->stream);
	g_free (zip->stream);
	g_free (zip->buf);

	g_clear_error (&zip->err);

	parent_class->finalize (obj);
}

static GObject*
gsf_infile_zip_constructor (GType                  type,
			    guint                  n_construct_properties,
			    GObjectConstructParam *construct_params)
{
	GsfInfileZip *zip;

	zip = (GsfInfileZip *)(parent_class->constructor (type,
							  n_construct_properties,
							  construct_params));
	if (zip->dup_parent) {
		/* Special call from zip_dup.  */
		zip->source = gsf_input_dup (zip->dup_parent->source, &zip->err);
		zip->info = zip_info_ref (zip->dup_parent->info);
		zip->dup_parent = NULL;
	} else {
		if (!zip_init_info (zip))
			zip->vdir = zip->info->vdir;
	}

	return (GObject *)zip;
}


static void
gsf_infile_zip_init (GObject *obj)
{
	GsfInfileZip *zip = (GsfInfileZip *)obj;
	zip->source = NULL;
	zip->info = NULL;
	zip->vdir = NULL;
	zip->stream = NULL;
	zip->restlen = 0;
	zip->crestlen = 0;
	zip->buf = NULL;
	zip->buf_size = 0;
	zip->seek_skipped = 0;
	zip->err = NULL;
}

static void
gsf_infile_zip_get_property (GObject     *object,
			     guint        property_id,
			     GValue      *value,
			     GParamSpec  *pspec)
{
	GsfInfileZip *zip = (GsfInfileZip *)object;

	switch (property_id) {
	case PROP_SOURCE:
		g_value_set_object (value, zip->source);
		break;
	case PROP_COMPRESSION_LEVEL:
		g_value_set_int (value,
				 zip->vdir->dirent
				 ? zip->vdir->dirent->compr_method
				 : 0);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}

static void
gsf_infile_zip_set_source (GsfInfileZip *zip, GsfInput *src)
{
	if (src)
		src = gsf_input_proxy_new (src);
	if (zip->source)
		g_object_unref (zip->source);
	zip->source = src;
}

static void
gsf_infile_zip_set_property (GObject      *object,
			     guint         property_id,
			     GValue const *value,
			     GParamSpec   *pspec)
{
	GsfInfileZip *zip = (GsfInfileZip *)object;

	switch (property_id) {
	case PROP_SOURCE:
		gsf_infile_zip_set_source (zip, g_value_get_object (value));
		break;
	case PROP_INTERNAL_PARENT:
		zip->dup_parent = g_value_get_object (value);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}


static void
gsf_infile_zip_class_init (GObjectClass *gobject_class)
{
	GsfInputClass  *input_class  = GSF_INPUT_CLASS (gobject_class);
	GsfInfileClass *infile_class = GSF_INFILE_CLASS (gobject_class);

	gobject_class->constructor      = gsf_infile_zip_constructor;
	gobject_class->finalize		= gsf_infile_zip_finalize;
	gobject_class->get_property     = gsf_infile_zip_get_property;
	gobject_class->set_property     = gsf_infile_zip_set_property;

	input_class->Dup		= gsf_infile_zip_dup;
	input_class->Read		= gsf_infile_zip_read;
	input_class->Seek		= gsf_infile_zip_seek;
	infile_class->num_children	= gsf_infile_zip_num_children;
	infile_class->name_by_index	= gsf_infile_zip_name_by_index;
	infile_class->child_by_index	= gsf_infile_zip_child_by_index;
	infile_class->child_by_name	= gsf_infile_zip_child_by_name;

	parent_class = g_type_class_peek_parent (gobject_class);

	g_object_class_install_property
		(gobject_class,
		 PROP_SOURCE,
		 g_param_spec_object ("source",
				      "Source",
				      "The archive being interpreted.",
				      GSF_INPUT_TYPE,
				      GSF_PARAM_STATIC |
				      G_PARAM_READWRITE |
				      G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property
		(gobject_class,
		 PROP_COMPRESSION_LEVEL,
		 g_param_spec_int ("compression-level",
				   "Compression Level",
				   "The level of compression used, zero meaning none.",
				   0, 10,
				   0,
				   GSF_PARAM_STATIC |
				   G_PARAM_READABLE));
	g_object_class_install_property
		(gobject_class,
		 PROP_INTERNAL_PARENT,
		 g_param_spec_object ("internal-parent",
				      "",
				      "Internal use only",
				      GSF_INFILE_ZIP_TYPE,
				      GSF_PARAM_STATIC |
				      G_PARAM_WRITABLE |
				      G_PARAM_CONSTRUCT_ONLY));
}

GSF_CLASS (GsfInfileZip, gsf_infile_zip,
	   gsf_infile_zip_class_init, gsf_infile_zip_init,
	   GSF_INFILE_TYPE)

/**
 * gsf_infile_zip_new :
 * @source : A base #GsfInput
 * @err   : A #GError, optionally %null
 *
 * Opens the root directory of a Zip file.
 * NOTE : adds a reference to @source
 *
 * Returns : the new zip file handler
 **/
GsfInfile *
gsf_infile_zip_new (GsfInput *source, GError **err)
{
	GsfInfileZip *zip;

	g_return_val_if_fail (GSF_IS_INPUT (source), NULL);

	zip = g_object_new (GSF_INFILE_ZIP_TYPE,
			    "source", source,
			    NULL);
	if (zip->err) {
		if (err)
			*err = g_error_copy (zip->err);
		g_object_unref (zip);
		return NULL;
	}

	return GSF_INFILE (zip);
}
