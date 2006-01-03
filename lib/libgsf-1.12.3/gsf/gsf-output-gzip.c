/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-output-gzip.c: wrapper to compress to gzipped output. See rfc1952.
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <gsf-config.h>
#include <gsf/gsf-output-gzip.h>
#include <gsf/gsf-output-impl.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-utils.h>

#include <zlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>

static GObjectClass *parent_class;

struct _GsfOutputGZip {
	GsfOutput output;

	GsfOutput *sink; /* compressed data */
	gboolean raw; /* No header and no trailer.  */

	z_stream  stream;
	uLong     crc;     /* crc32 of uncompressed data */
	size_t    isize;

	guint8   *buf;
	size_t    buf_size;
};

typedef struct {
	GsfOutputClass output_class;
} GsfOutputGZipClass;

enum {
	PROP_0,
	PROP_RAW,
	PROP_SINK
};


/* gzip flag byte */
#define GZIP_ORIGINAL_NAME	0x08 /* the original is stored */

static gboolean
init_gzip (GsfOutputGZip *gzip)
{
	int ret;

	ret = deflateInit2 (&gzip->stream, Z_DEFAULT_COMPRESSION,
			    Z_DEFLATED, -MAX_WBITS, MAX_MEM_LEVEL,
			    Z_DEFAULT_STRATEGY);
	if (ret != Z_OK)
		return FALSE;

	if (!gzip->buf) {
		gzip->buf_size = 0x100;
		gzip->buf = g_new (guint8, gzip->buf_size);
	}
	gzip->stream.next_out  = gzip->buf;
	gzip->stream.avail_out = gzip->buf_size;

	return TRUE;
}

static gboolean
gzip_output_header (GsfOutputGZip *gzip)
{
	guint8 buf[3 + 1 + 4 + 2];
	static guint8 const gzip_signature[] = { 0x1f, 0x8b, 0x08 } ;
	time_t mtime = time (NULL);
	char const *name = gsf_output_name (gzip->sink);
	/* FIXME: What to do about gz extension ... ? */
	int nlen = 0;  /* name ? strlen (name) : 0; */
	gboolean ret;

	memset (buf, 0, sizeof buf);
	memcpy (buf, gzip_signature, 3);
	if (nlen > 0)
		buf[3] = GZIP_ORIGINAL_NAME;
	GSF_LE_SET_GUINT32 (buf + 4, (guint32) mtime);
	buf[9] = 3;	/* UNIX */
	ret = gsf_output_write (gzip->sink, sizeof buf, buf);
	if (ret && name && nlen > 0)
		ret = gsf_output_write (gzip->sink, nlen, name);

	return ret;
}

/**
 * gsf_output_gzip_new :
 * @sink : The underlying data source.
 * @err	   : optionally NULL.
 *
 * Adds a reference to @sink.
 *
 * Returns a new file or NULL.
 **/
GsfOutput *
gsf_output_gzip_new (GsfOutput *sink, GError **err)
{
	GsfOutput *output;
	const GError *con_err;

	g_return_val_if_fail (GSF_IS_OUTPUT (sink), NULL);

	output = g_object_new (GSF_OUTPUT_GZIP_TYPE, "sink", sink, NULL);
	con_err = gsf_output_error (output);

	if (con_err) {
		if (err)
			*err = g_error_copy (con_err);
		g_object_unref (output);
		return NULL;
	}

	return output;
}

static void
gsf_output_gzip_finalize (GObject *obj)
{
	GsfOutputGZip *gzip = (GsfOutputGZip *)obj;

	if (gzip->sink != NULL) {
		g_object_unref (G_OBJECT (gzip->sink));
		gzip->sink = NULL;
	}

	g_free (gzip->buf);

	/* FIXME: check for error?  */
	deflateEnd (&gzip->stream);

	parent_class->finalize (obj);
}

static gboolean
gzip_output_block (GsfOutputGZip *gzip)
{
	size_t num_bytes = gzip->buf_size - gzip->stream.avail_out;

	if (!gsf_output_write (gzip->sink, num_bytes, gzip->buf)) {
		gsf_output_set_error (GSF_OUTPUT (gzip), 0,
				      "Failed to write");
		return FALSE;
	}
	gzip->stream.next_out  = gzip->buf;
	gzip->stream.avail_out = gzip->buf_size;

	return TRUE;
}

static gboolean
gzip_flush (GsfOutputGZip *gzip)
{
	int zret;

	do {
		zret = deflate (&gzip->stream, Z_FINISH);
		if (zret == Z_OK) {
			/*  In this case Z_OK means more buffer space
			    needed  */
			if (!gzip_output_block (gzip))
				return FALSE;
		}
	} while (zret == Z_OK);
	if (zret != Z_STREAM_END) {
		gsf_output_set_error (GSF_OUTPUT (gzip), 0,
				      "Unexpected compression failure");
		g_warning ("Unexpected error code %d from zlib during compression.",
			   zret);
		return FALSE;
	}
	if (!gzip_output_block (gzip))
		return FALSE;

	return TRUE;
}

static gboolean
gsf_output_gzip_write (GsfOutput *output,
		       size_t num_bytes, guint8 const *data)
{
	GsfOutputGZip *gzip = GSF_OUTPUT_GZIP (output);

	g_return_val_if_fail (data, FALSE);

	gzip->stream.next_in  = (unsigned char *) data;
	gzip->stream.avail_in = num_bytes;

	while (gzip->stream.avail_in > 0) {
		int zret;
		if (gzip->stream.avail_out == 0) {
			if (!gzip_output_block (gzip))
				return FALSE;
		}

		zret = deflate (&gzip->stream, Z_NO_FLUSH);
		if (zret != Z_OK) {
			gsf_output_set_error (output, 0,
					      "Unexpected compression failure");
			g_warning ("Unexpected error code %d from zlib during compression.",
				   zret);
			return FALSE;
		}
	}

	gzip->crc = crc32 (gzip->crc, data, num_bytes);
	gzip->isize += num_bytes;

	if (gzip->stream.avail_out == 0) {
		if (!gzip_output_block (gzip))
			return FALSE;
	}

	return TRUE;
}

static gboolean
gsf_output_gzip_seek (G_GNUC_UNUSED GsfOutput *output,
		      G_GNUC_UNUSED gsf_off_t offset,
		      G_GNUC_UNUSED GSeekType whence)
{
	return FALSE;
}

static gboolean
gsf_output_gzip_close (GsfOutput *output)
{
	if (!gsf_output_error (output)) {
		GsfOutputGZip *gzip = GSF_OUTPUT_GZIP (output);

		if (!gzip_flush (gzip))
			return FALSE;

		if (!gzip->raw) {
			guint8 buf[8];

			GSF_LE_SET_GUINT32 (buf,     gzip->crc);
			GSF_LE_SET_GUINT32 (buf + 4, gzip->isize);
			if (!gsf_output_write (gzip->sink, 8, buf))
				return FALSE;
		}
	}

	return TRUE;
}

static void
gsf_output_gzip_init (GObject *obj)
{
	GsfOutputGZip *gzip = GSF_OUTPUT_GZIP (obj);

	gzip->sink = NULL;
	gzip->stream.zalloc	= (alloc_func)0;
	gzip->stream.zfree	= (free_func)0;
	gzip->stream.opaque	= (voidpf)0;
	gzip->stream.next_in	= Z_NULL;
	gzip->stream.next_out	= Z_NULL;
	gzip->stream.avail_in	= gzip->stream.avail_out = 0;
	gzip->crc		= crc32 (0L, Z_NULL, 0);
	gzip->isize             = 0;
	gzip->buf		= NULL;
	gzip->buf_size		= 0;
}

static void
gsf_output_gzip_get_property (GObject     *object,
			      guint        property_id,
			      GValue      *value,
			      GParamSpec  *pspec)
{
	GsfOutputGZip *gzip = (GsfOutputGZip *)object;

	switch (property_id) {
	case PROP_RAW:
		g_value_set_boolean (value, gzip->raw);
		break;
	case PROP_SINK:
		g_value_set_object (value, gzip->sink);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}

static void
gsf_output_gzip_set_sink (GsfOutputGZip *gzip, GsfOutput *sink)
{
	if (sink)
		g_object_ref (GSF_OUTPUT (sink));
	if (gzip->sink)
		g_object_unref (gzip->sink);
	gzip->sink = sink;
}

static void
gsf_output_gzip_set_property (GObject      *object,
			      guint         property_id,
			      GValue const *value,
			      GParamSpec   *pspec)
{
	GsfOutputGZip *gzip = (GsfOutputGZip *)object;

	switch (property_id) {
	case PROP_RAW:
		gzip->raw = g_value_get_boolean (value);
		break;
	case PROP_SINK:
		gsf_output_gzip_set_sink (gzip, g_value_get_object (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}

static GObject*
gsf_output_gzip_constructor (GType                  type,
			     guint                  n_construct_properties,
			     GObjectConstructParam *construct_params)
{
	GsfOutputGZip *gzip;

	gzip = (GsfOutputGZip *)(parent_class->constructor (type,
							    n_construct_properties,
							    construct_params));

	if (!gzip->sink)
		gsf_output_set_error (GSF_OUTPUT (gzip),
				      0,
				      "NULL sink");
	else if (!init_gzip (gzip))
		gsf_output_set_error (GSF_OUTPUT (gzip),
				      0,
				      "Failed to initialize zlib structure");
	else if (!gzip->raw && !gzip_output_header (gzip))
		gsf_output_set_error (GSF_OUTPUT (gzip),
				      0,
				      "Failed to write gzip header");

	return (GObject *)gzip;
}

static void
gsf_output_gzip_class_init (GObjectClass *gobject_class)
{
	GsfOutputClass *output_class = GSF_OUTPUT_CLASS (gobject_class);

	gobject_class->constructor  = gsf_output_gzip_constructor;
	gobject_class->finalize     = gsf_output_gzip_finalize;
	gobject_class->set_property = gsf_output_gzip_set_property;
	gobject_class->get_property = gsf_output_gzip_get_property;
	output_class->Write	    = gsf_output_gzip_write;
	output_class->Seek	    = gsf_output_gzip_seek;
	output_class->Close	    = gsf_output_gzip_close;

	g_object_class_install_property
		(gobject_class,
		 PROP_RAW,
		 g_param_spec_boolean ("raw", "Raw",
				       "Whether to write compressed data with no header/tailer.",
				       FALSE,
				       GSF_PARAM_STATIC |
				       G_PARAM_READWRITE |
				       G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property
		(gobject_class,
		 PROP_SINK,
		 g_param_spec_object ("sink", "Sink",
				      "Where the compressed data is written.",
				      GSF_OUTPUT_TYPE,
				      GSF_PARAM_STATIC |
				      G_PARAM_READWRITE |
				      G_PARAM_CONSTRUCT_ONLY));

	parent_class = g_type_class_peek_parent (gobject_class);
}

GSF_CLASS (GsfOutputGZip, gsf_output_gzip,
	   gsf_output_gzip_class_init, gsf_output_gzip_init, GSF_OUTPUT_TYPE)
