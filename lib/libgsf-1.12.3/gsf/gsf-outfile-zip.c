/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-outfile-zip.c: zip archive output.
 *
 * Copyright (C) 2002-2004 Jon K Hellan (hellan@acm.org)
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
 * Foundation, Outc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <gsf-config.h>
#include <gsf/gsf-outfile-impl.h>
#include <gsf/gsf-outfile-zip.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-zip-impl.h>

#include <string.h>
#include <time.h>
#include <zlib.h>

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "libgsf:zip"

enum {
	PROP_0,
	PROP_SINK,
	PROP_ENTRY_NAME,
	PROP_COMPRESSION_LEVEL
};

static GObjectClass *parent_class;

struct _GsfOutfileZip {
	GsfOutfile parent;

	GsfOutput     *sink;
	GsfOutfileZip *root;

	char *entry_name;

	GsfZipVDir    *vdir;
	GPtrArray     *root_order;	/* only valid for the root */

	z_stream  *stream;
	GsfZipCompressionMethod compression_method;

	gboolean   writing;

	guint8   *buf;
	size_t    buf_size;
};

typedef struct {
	GsfOutfileClass  parent_class;
} GsfOutfileZipClass;

#define GSF_OUTFILE_ZIP_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST ((k), GSF_OUTFILE_ZIP_TYPE, GsfOutfileZipClass))
#define GSF_IS_OUTFILE_ZIP_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), GSF_OUTFILE_ZIP_TYPE))

static void
disconnect_children (GsfOutfileZip *zip)
{
	unsigned i;

	if (!zip->root_order)
		return;

	for (i = 0 ; i < zip->root_order->len ; i++) {
		GsfOutfileZip *child =
			g_ptr_array_index (zip->root_order, i);
		if (child)
			g_object_unref (child);
	}
	g_ptr_array_free (zip->root_order, TRUE);
	zip->root_order = NULL;
}

static void
gsf_outfile_zip_finalize (GObject *obj)
{
	GsfOutfileZip *zip = GSF_OUTFILE_ZIP (obj);

	/* If the closing failed, we might have stuff here.  */
	disconnect_children (zip);

	if (zip->sink != NULL) {
		g_object_unref (zip->sink);
		zip->sink = NULL;
	}

	g_free (zip->entry_name);

	if (zip->stream)
		(void) deflateEnd (zip->stream);
	g_free (zip->stream);
	g_free (zip->buf);

	if (zip == zip->root)
		gsf_vdir_free (zip->vdir, TRUE); /* Frees vdirs recursively */

	parent_class->finalize (obj);
}

static GObject *
gsf_outfile_zip_constructor (GType                  type,
			     guint                  n_construct_properties,
			     GObjectConstructParam *construct_params)
{
	GsfOutfileZip *zip =(GsfOutfileZip *)
		(parent_class->constructor (type,
					    n_construct_properties,
					    construct_params));

	if (!zip->entry_name) {
		zip->vdir = gsf_vdir_new ("", TRUE, NULL);
		zip->root_order = g_ptr_array_new ();
		zip->root = zip;

		/* The names are the same */
		gsf_output_set_name (GSF_OUTPUT (zip), gsf_output_name (zip->sink));
		gsf_output_set_container (GSF_OUTPUT (zip), NULL);
	}

	return (GObject *)zip;
}

static gboolean
gsf_outfile_zip_seek (G_GNUC_UNUSED GsfOutput *output,
		      G_GNUC_UNUSED gsf_off_t offset,
		      G_GNUC_UNUSED GSeekType whence)
{
	return FALSE;
}

static gboolean
zip_dirent_write (GsfOutput *sink, GsfZipDirent *dirent)
{
	static guint8 const dirent_signature[] =
		{ 'P', 'K', 0x01, 0x02 };
	guint8 buf[ZIP_DIRENT_SIZE];
	int nlen = strlen (dirent->name);
	gboolean ret;

	memset (buf, 0, sizeof buf);
	memcpy (buf, dirent_signature, sizeof dirent_signature);
	GSF_LE_SET_GUINT16 (buf + ZIP_DIRENT_ENCODER, 0x317); /* Unix */
	GSF_LE_SET_GUINT16 (buf + ZIP_DIRENT_EXTRACT, 0x14);
	GSF_LE_SET_GUINT16 (buf + ZIP_DIRENT_FLAGS, 0x08);
	GSF_LE_SET_GUINT16 (buf + ZIP_DIRENT_COMPR_METHOD,
			    dirent->compr_method);
	GSF_LE_SET_GUINT32 (buf + ZIP_DIRENT_DOSTIME, dirent->dostime);
	GSF_LE_SET_GUINT32 (buf + ZIP_DIRENT_CRC32, dirent->crc32);
	GSF_LE_SET_GUINT32 (buf + ZIP_DIRENT_CSIZE, dirent->csize);
	GSF_LE_SET_GUINT32 (buf + ZIP_DIRENT_USIZE, dirent->usize);
	GSF_LE_SET_GUINT16 (buf + ZIP_DIRENT_NAME_SIZE, nlen);
	GSF_LE_SET_GUINT16 (buf + ZIP_DIRENT_EXTRAS_SIZE, 0);
	GSF_LE_SET_GUINT16 (buf + ZIP_DIRENT_COMMENT_SIZE, 0);
	GSF_LE_SET_GUINT16 (buf + ZIP_DIRENT_DISKSTART, 0);
	GSF_LE_SET_GUINT16 (buf + ZIP_DIRENT_FILE_TYPE, 0);
	/* Hardcode file mode 644 */
	GSF_LE_SET_GUINT32 (buf + ZIP_DIRENT_FILE_MODE, 0644 << 16);
	GSF_LE_SET_GUINT32 (buf + ZIP_DIRENT_OFFSET, dirent->offset);

	ret = gsf_output_write (sink, sizeof buf, buf);
	if (ret)
		ret = gsf_output_write (sink, nlen, dirent->name);

	return ret;
}

static gboolean
zip_trailer_write (GsfOutfileZip *zip, unsigned entries, gsf_off_t dirpos)
{
	static guint8 const trailer_signature[] =
		{ 'P', 'K', 0x05, 0x06 };
	guint8 buf[ZIP_TRAILER_SIZE];
	gsf_off_t pos = gsf_output_tell (zip->sink);

	memset (buf, 0, sizeof buf);
	memcpy (buf, trailer_signature, sizeof trailer_signature);
	GSF_LE_SET_GUINT16 (buf + ZIP_TRAILER_ENTRIES, entries);
	GSF_LE_SET_GUINT16 (buf + ZIP_TRAILER_TOTAL_ENTRIES, entries);
	GSF_LE_SET_GUINT32 (buf + ZIP_TRAILER_DIR_SIZE, pos - dirpos);
	GSF_LE_SET_GUINT32 (buf + ZIP_TRAILER_DIR_POS, dirpos);

	return gsf_output_write (zip->sink, sizeof buf, buf);
}

static gboolean
zip_close_root (GsfOutput *output)
{
	GsfOutfileZip *zip = GSF_OUTFILE_ZIP (output);
	GsfOutfileZip *child;
	gsf_off_t dirpos = gsf_output_tell (zip->sink);
	GPtrArray *elem = zip->root_order;
	unsigned entries = elem->len;
	unsigned i;

	/* Check that children are closed */
	for (i = 0 ; i < elem->len ; i++) {
		child = g_ptr_array_index (elem, i);
		if (!gsf_output_is_closed (GSF_OUTPUT (child))) {
			g_warning ("Child still open");
			return FALSE;
		}
	}

	/* Write directory */
	for (i = 0 ; i < entries ; i++) {
		child = g_ptr_array_index (elem, i);
		if (!zip_dirent_write (zip->sink, child->vdir->dirent))
			return FALSE;
	}

	disconnect_children (zip);

	return zip_trailer_write (zip, entries, dirpos);
}

static void
stream_name_write_to_buf (GsfOutfileZip *zip, GString *res)
{
	GsfOutput  *output = GSF_OUTPUT (zip);
	GsfOutfile *container;

	if (zip == zip->root)
		return;

	container = gsf_output_container (output);
	if (container) {
		stream_name_write_to_buf (GSF_OUTFILE_ZIP (container), res);
		if (res->len) {
			/* Forward slash is specified by the format.  */
			g_string_append_c (res, '/');
		}
	}

	if (zip->entry_name)
		g_string_append (res, zip->entry_name);
}

static char *
stream_name_build (GsfOutfileZip *zip)
{
	GString *str = g_string_sized_new (80);
	stream_name_write_to_buf (zip, str);
	return g_string_free (str, FALSE);
}

static guint32
zip_time_make (time_t t)
{
	struct tm *localnow = localtime (&t);
	guint32 ztime;

	ztime = (localnow->tm_year - 80) & 0x7f;
	ztime = (ztime << 4) | ((localnow->tm_mon + 1)  & 0x0f);
	ztime = (ztime << 5) | (localnow->tm_mday & 0x1f);
	ztime = (ztime << 5) | (localnow->tm_hour & 0x1f);
	ztime = (ztime << 6) | (localnow->tm_min  & 0x3f);
	ztime = (ztime << 5) | ((localnow->tm_sec / 2) & 0x1f);

	return ztime;
}

static GsfZipDirent*
zip_dirent_new_out (GsfOutfileZip *zip)
{
	GsfZipDirent *dirent = gsf_zip_dirent_new ();
	dirent->name = stream_name_build (zip);
	dirent->compr_method = zip->compression_method;
	dirent->dostime = zip_time_make (time (NULL));
	return dirent;
}

static gboolean
zip_header_write (GsfOutfileZip *zip)
{
	static guint8 const header_signature[] =
		{ 'P', 'K', 0x03, 0x04 };
	guint8 hbuf[ZIP_HEADER_SIZE];
	GsfZipDirent *dirent = zip->vdir->dirent;
	guint16 flags = 0;
	char *name = dirent->name;
	int   nlen = strlen (name);
	gboolean ret;

	memset (hbuf, 0, sizeof hbuf);
	memcpy (hbuf, header_signature, sizeof header_signature);
	GSF_LE_SET_GUINT16 (hbuf + ZIP_HEADER_VERSION, 0x14);
	if (dirent->compr_method == GSF_ZIP_DEFLATED)
		flags = 0x08;
	GSF_LE_SET_GUINT16 (hbuf + ZIP_HEADER_FLAGS, flags);
	GSF_LE_SET_GUINT16 (hbuf + ZIP_HEADER_COMP_METHOD,
			    dirent->compr_method);
	GSF_LE_SET_GUINT32 (hbuf + ZIP_HEADER_TIME, dirent->dostime);
	GSF_LE_SET_GUINT16 (hbuf + ZIP_HEADER_NAME_LEN, nlen);
	ret = gsf_output_write (zip->sink, sizeof hbuf, hbuf);
	if (ret)
		ret = gsf_output_write (zip->sink, nlen, name);

	return ret;
}

static gboolean
zip_init_write (GsfOutput *output)
{
	GsfOutfileZip *zip = GSF_OUTFILE_ZIP (output);
	GsfZipDirent *dirent;
	int      ret;

	if (zip->root->writing) {
		g_warning ("Already writing to another stream in archive");
		return FALSE;
	}

	if (!gsf_output_wrap (G_OBJECT (output), zip->sink))
		return FALSE;

	dirent = zip_dirent_new_out (zip);
	dirent->offset = gsf_output_tell (zip->sink);
	if (zip->vdir->dirent)
		g_warning ("Leak.");

	zip->vdir->dirent = dirent;
	zip_header_write (zip);
	zip->writing = TRUE;
	zip->root->writing = TRUE;
	dirent->crc32 = crc32 (0L, Z_NULL, 0);
	if (zip->compression_method == GSF_ZIP_DEFLATED) {
		if (!zip->stream) {
			zip->stream = g_new0 (z_stream, 1);
		}
		ret = deflateInit2 (zip->stream, Z_DEFAULT_COMPRESSION,
				    Z_DEFLATED, -MAX_WBITS, MAX_MEM_LEVEL,
				    Z_DEFAULT_STRATEGY);
		if (ret != Z_OK)
			return FALSE;
		if (!zip->buf) {
			zip->buf_size = ZIP_BUF_SIZE;
			zip->buf = g_new (guint8, zip->buf_size);
		}
		zip->stream->next_out  = zip->buf;
		zip->stream->avail_out = zip->buf_size;
	}

	return TRUE;
}

static gboolean
zip_output_block (GsfOutfileZip *zip)
{
	size_t num_bytes = zip->buf_size - zip->stream->avail_out;
	GsfZipDirent *dirent = zip->vdir->dirent;

	if (!gsf_output_write (zip->sink, num_bytes, zip->buf)) {
		return FALSE;
	}
	dirent->csize += num_bytes;
	zip->stream->next_out  = zip->buf;
	zip->stream->avail_out = zip->buf_size;

	return TRUE;
}

static gboolean
zip_flush (GsfOutfileZip *zip)
{
	int zret;

	do {
		zret = deflate (zip->stream, Z_FINISH);
		if (zret == Z_OK || (zret == Z_BUF_ERROR && zip->stream->avail_out == 0)) {
			/*  In this case Z_OK or Z_BUF_ERROR means more buffer 
			    space is needed  */
			if (!zip_output_block (zip))
				return FALSE;
		}
	} while (zret == Z_OK || zret == Z_BUF_ERROR);
	if (zret != Z_STREAM_END)
		return FALSE;
	if (!zip_output_block (zip))
		return FALSE;

	return TRUE;
}

/* Write the per stream data descriptor */
static gboolean
zip_ddesc_write (GsfOutfileZip *zip)
{
	static guint8 const ddesc_signature[] =
		{ 'P', 'K', 0x07, 0x08 };
	guint8 buf[16];
	GsfZipDirent *dirent = zip->vdir->dirent;

	memcpy (buf, ddesc_signature, sizeof ddesc_signature);
	GSF_LE_SET_GUINT32 (buf + 4, dirent->crc32);
	GSF_LE_SET_GUINT32 (buf + 8, dirent->csize);
	GSF_LE_SET_GUINT32 (buf + 12, dirent->usize);
	if (!gsf_output_write (zip->sink, sizeof buf, buf)) {
		return FALSE;
	}

	return TRUE;
}

static gboolean
zip_header_write_sizes (GsfOutfileZip *zip)
{
	guint8 hbuf[ZIP_HEADER_SIZE];
	GsfZipDirent *dirent = zip->vdir->dirent;
	gsf_off_t pos = gsf_output_tell (zip->sink);

	if (!gsf_output_seek (zip->sink, dirent->offset + ZIP_HEADER_CRC,
			      G_SEEK_SET))
		return FALSE;

	GSF_LE_SET_GUINT32 (hbuf + ZIP_HEADER_CRC, dirent->crc32);
	GSF_LE_SET_GUINT32 (hbuf + ZIP_HEADER_COMP_SIZE, dirent->csize);
	GSF_LE_SET_GUINT32 (hbuf + ZIP_HEADER_UNCOMP_SIZE, dirent->usize);
	if (!gsf_output_write (zip->sink, 12, hbuf + ZIP_HEADER_CRC))
		return FALSE;
	if (!gsf_output_seek (zip->sink, pos, G_SEEK_SET))
		return FALSE;

	return TRUE;
}

static gboolean
zip_close_stream (GsfOutput *output)
{
	GsfOutfileZip *zip = GSF_OUTFILE_ZIP (output);
	gboolean result;

	if (!zip->writing)
		if (!zip_init_write (output))
			return FALSE;

	if (zip->compression_method == GSF_ZIP_DEFLATED) {
		if (!zip_flush (zip))
			return FALSE;

		if (!zip_ddesc_write (zip)) /* Write data descriptor */
			return FALSE;
	} else {
		if (!zip_header_write_sizes (zip)) /* Write crc, sizes */
			return FALSE;
	}
	zip->root->writing = FALSE;

	result = gsf_output_unwrap (G_OBJECT (output), zip->sink);

	/* Free unneeded memory */
	if (zip->stream) {
		(void) deflateEnd (zip->stream);
		g_free (zip->stream);
		zip->stream = NULL;
		g_free (zip->buf);
		zip->buf = NULL;
	}

	return result;
}

static gboolean
gsf_outfile_zip_close (GsfOutput *output)
{
	GsfOutfileZip *zip = GSF_OUTFILE_ZIP (output);
	gboolean ret;

	/* The root dir */
	if (zip == zip->root)
		ret = zip_close_root (output);
	else if (zip->vdir->is_directory)
		/* Directories: Do nothing. Should change this to actually
		 * write dirs which don't have children. */
		ret = TRUE;
	else
		ret = zip_close_stream (output);

	return ret;
}

static gboolean
gsf_outfile_zip_write (GsfOutput *output,
		       size_t num_bytes, guint8 const *data)
{
	GsfOutfileZip *zip = GSF_OUTFILE_ZIP (output);
	GsfZipDirent *dirent;
	int ret;

	g_return_val_if_fail (zip && zip->vdir, FALSE);
	g_return_val_if_fail (!zip->vdir->is_directory, FALSE);
	g_return_val_if_fail (data, FALSE);

	if (!zip->writing)
		if (!zip_init_write (output))
			return FALSE;

	dirent = zip->vdir->dirent;
	if (zip->compression_method == GSF_ZIP_DEFLATED) {
		zip->stream->next_in  = (unsigned char *) data;
		zip->stream->avail_in = num_bytes;

		while (zip->stream->avail_in > 0) {
			if (zip->stream->avail_out == 0) {
				if (!zip_output_block (zip))
					return FALSE;
			}
			ret = deflate (zip->stream, Z_NO_FLUSH);
			if (ret != Z_OK)
				return FALSE;
		}
	} else {
		if (!gsf_output_write (zip->sink, num_bytes, data))
			return FALSE;
		dirent->csize += num_bytes;
	}
	dirent->crc32 = crc32 (dirent->crc32, data, num_bytes);
	dirent->usize += num_bytes;

	return TRUE;
}

static void
root_register_child (GsfOutfileZip *root, GsfOutfileZip *child)
{
	child->root = root;
	if (!child->vdir->is_directory) {
		g_object_ref (child);
		g_ptr_array_add (root->root_order, child);
	}
}

static void
gsf_outfile_zip_set_sink (GsfOutfileZip *zip, GsfOutput *sink)
{
	if (sink)
		g_object_ref (sink);
	if (zip->sink)
		g_object_unref (zip->sink);
	zip->sink = sink;
}

static GsfOutput *
gsf_outfile_zip_new_child (GsfOutfile *parent,
			   char const *name, gboolean is_dir,
			   char const *first_property_name, va_list args)
{
	GsfOutfileZip *zip_parent = (GsfOutfileZip *)parent;
	GsfOutfileZip *child;
	size_t n_params = 0;
	GParameter *params = NULL;
	char *display_name;

	g_return_val_if_fail (zip_parent != NULL, NULL);
	g_return_val_if_fail (zip_parent->vdir, NULL);
	g_return_val_if_fail (zip_parent->vdir->is_directory, NULL);
	g_return_val_if_fail (name && *name, NULL);

	gsf_property_settings_collect (GSF_OUTFILE_ZIP_TYPE,
				       &params, &n_params,
				       "sink", zip_parent->sink,
				       "entry-name", name,
				       NULL);
	gsf_property_settings_collect_valist (GSF_OUTFILE_ZIP_TYPE,
					      &params, &n_params,
					      first_property_name,
					      args);
	child = (GsfOutfileZip *)g_object_newv (GSF_OUTFILE_ZIP_TYPE,
						n_params,
						params);
	gsf_property_settings_free (params, n_params);

	child->vdir = gsf_vdir_new (name, is_dir, NULL);

	/* FIXME: It isn't clear what encoding name is in.  */
	display_name = g_filename_display_name (name);
	gsf_output_set_name (GSF_OUTPUT (child), display_name);
	g_free (display_name);

	gsf_output_set_container (GSF_OUTPUT (child), parent);
	gsf_vdir_add_child (zip_parent->vdir, child->vdir);
	root_register_child (zip_parent->root, child);

	return GSF_OUTPUT (child);
}

static void
gsf_outfile_zip_init (GObject *obj)
{
	GsfOutfileZip *zip = GSF_OUTFILE_ZIP (obj);

	zip->sink = NULL;
	zip->root = NULL;
	zip->entry_name = NULL;
	zip->vdir = NULL;
	zip->root_order = NULL;
	zip->stream = NULL;
	zip->compression_method = GSF_ZIP_DEFLATED;
	zip->writing = FALSE;
	zip->buf = NULL;
	zip->buf_size = 0;
}

static void
gsf_outfile_zip_get_property (GObject     *object,
			      guint        property_id,
			      GValue      *value,
			      GParamSpec  *pspec)
{
	GsfOutfileZip *zip = (GsfOutfileZip *)object;

	switch (property_id) {
	case PROP_SINK:
		g_value_set_object (value, zip->sink);
		break;
	case PROP_ENTRY_NAME:
		g_value_set_string (value, zip->entry_name);
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
gsf_outfile_zip_set_property (GObject      *object,
			      guint         property_id,
			      GValue const *value,
			      GParamSpec   *pspec)
{
	GsfOutfileZip *zip = (GsfOutfileZip *)object;

	switch (property_id) {
	case PROP_SINK:
		gsf_outfile_zip_set_sink (zip, g_value_get_object (value));
		break;
	case PROP_ENTRY_NAME:
		zip->entry_name = g_strdup (g_value_get_string (value));
		break;
	case PROP_COMPRESSION_LEVEL: {
		int level = g_value_get_int (value);
		switch (level) {
		case GSF_ZIP_STORED:
		case GSF_ZIP_DEFLATED:
			zip->compression_method = level;
			break;
		default:
			g_warning ("Unsupported compression level %d", level);
		}
		break;
	}
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}

static void
gsf_outfile_zip_class_init (GObjectClass *gobject_class)
{
	GsfOutputClass  *input_class  = GSF_OUTPUT_CLASS (gobject_class);
	GsfOutfileClass *outfile_class = GSF_OUTFILE_CLASS (gobject_class);

	gobject_class->constructor      = gsf_outfile_zip_constructor;
	gobject_class->finalize		= gsf_outfile_zip_finalize;
	gobject_class->get_property     = gsf_outfile_zip_get_property;
	gobject_class->set_property     = gsf_outfile_zip_set_property;

	input_class->Write		= gsf_outfile_zip_write;
	input_class->Seek		= gsf_outfile_zip_seek;
	input_class->Close		= gsf_outfile_zip_close;
	outfile_class->new_child	= gsf_outfile_zip_new_child;

	parent_class = g_type_class_peek_parent (gobject_class);

	g_object_class_install_property
		(gobject_class,
		 PROP_SINK,
		 g_param_spec_object ("sink", "Sink",
				      "Where the archive is written.",
				      GSF_OUTPUT_TYPE,
				      GSF_PARAM_STATIC |
				      G_PARAM_READWRITE |
				      G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property
		(gobject_class,
		 PROP_ENTRY_NAME,
		 g_param_spec_string ("entry-name", "Entry Name",
				      "The filename of this member in the archive without path.",
				      NULL,
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
				   GSF_ZIP_DEFLATED,
				   GSF_PARAM_STATIC |
				   G_PARAM_READWRITE |
				   G_PARAM_CONSTRUCT_ONLY));
}

GSF_CLASS (GsfOutfileZip, gsf_outfile_zip,
	   gsf_outfile_zip_class_init, gsf_outfile_zip_init,
	   GSF_OUTFILE_TYPE)

/**
 * gsf_outfile_zip_new :
 * @sink :
 * @err   :
 *
 * Creates the root directory of a Zip file and manages the addition of
 * children.
 *
 * NOTE : adds a reference to @sink
 *
 * Returns : the new zip file handler
 **/
GsfOutfile *
gsf_outfile_zip_new (GsfOutput *sink, G_GNUC_UNUSED GError **err)
{
	g_return_val_if_fail (GSF_IS_OUTPUT (sink), NULL);

	return (GsfOutfile *)g_object_new (GSF_OUTFILE_ZIP_TYPE,
					   "sink", sink,
					   NULL);
}

/* deprecated has no effect */
gboolean
gsf_outfile_zip_set_compression_method (G_GNUC_UNUSED GsfOutfileZip *zip,
					G_GNUC_UNUSED GsfZipCompressionMethod method)
{
	return TRUE;
}
