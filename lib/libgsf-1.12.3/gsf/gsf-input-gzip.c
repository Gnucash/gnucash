/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-input-gzip.c: wrapper to uncompress gzipped input
 *
 * Copyright (C) 2002-2004 Jody Goldberg (jody@gnome.org)
 * Copyright (C) 2005 Morten Welinder (terra@gnome.org)
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
#include <gsf/gsf-input-gzip.h>
#include <gsf/gsf-input-impl.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-utils.h>

#include <zlib.h>
#include <stdio.h>
#include <string.h>

#define Z_BUFSIZE 0x100

static GObjectClass *parent_class;

struct _GsfInputGZip {
	GsfInput input;

	GsfInput *source; /* compressed data */
	gboolean raw; /* No header and no trailer.  */
	GError *err;
	gsf_off_t uncompressed_size;
	gboolean stop_byte_added;

	z_stream  stream;
	guint8 const *gzipped_data;
	uLong     crc;     /* crc32 of uncompressed data */

	guint8   *buf;
	size_t    buf_size;

	size_t    header_size, trailer_size;
	gsf_off_t seek_skipped;
};

typedef struct {
	GsfInputClass input_class;
} GsfInputGZipClass;

enum {
	PROP_0,
	PROP_RAW,
	PROP_SOURCE,
	PROP_UNCOMPRESSED_SIZE
};

/* gzip flag byte */
#define GZIP_IS_ASCII		0x01 /* file contains text ? */
#define GZIP_HEADER_CRC		0x02 /* there is a CRC in the header */
#define GZIP_EXTRA_FIELD	0x04 /* there is an 'extra' field */
#define GZIP_ORIGINAL_NAME	0x08 /* the original is stored */
#define GZIP_HAS_COMMENT	0x10 /* There is a comment in the header */
#define GZIP_HEADER_FLAGS (unsigned)(GZIP_IS_ASCII |GZIP_HEADER_CRC |GZIP_EXTRA_FIELD |GZIP_ORIGINAL_NAME |GZIP_HAS_COMMENT)

static gboolean
check_header (GsfInputGZip *input)
{
	if (input->raw) {
		input->header_size = 0;
		input->trailer_size = 0;
	} else {
		static guint8 const signature[2] = {0x1f, 0x8b};
		guint8 const *data;
		unsigned flags, len;

		/* Check signature */
		if (NULL == (data = gsf_input_read (input->source, 2 + 1 + 1 + 6, NULL)) ||
		    0 != memcmp (data, signature, sizeof (signature)))
			return TRUE;

		/* verify flags and compression type */
		flags  = data[3];
		if (data[2] != Z_DEFLATED || (flags & ~GZIP_HEADER_FLAGS) != 0)
			return TRUE;

		/* If we have the size, don't bother seeking to the end.  */
		if (input->uncompressed_size < 0) {
			/* Get the uncompressed size */
			if (gsf_input_seek (input->source, (gsf_off_t) -4, G_SEEK_END) ||
			    NULL == (data = gsf_input_read (input->source, 4, NULL)))
				return TRUE;
			/* FIXME, but how?  The size read here is modulo 2^32.  */
			input->uncompressed_size = GSF_LE_GET_GUINT32 (data);

			if (input->uncompressed_size / 1000 > gsf_input_size (input->source)) {
				g_warning ("Suspiciously well compressed file with better than 1000:1 ratio.\n"
					   "It is probably truncated or corrupt");
			}
		}

		if (gsf_input_seek (input->source, 2 + 1 + 1 + 6, G_SEEK_SET))
			return TRUE;

		if (flags & GZIP_EXTRA_FIELD) {
			if (NULL == (data = gsf_input_read (input->source, 2, NULL)))
				return TRUE;
			len = GSF_LE_GET_GUINT16 (data);
			if (NULL == gsf_input_read (input->source, len, NULL))
				return TRUE;
		}
		if (flags & GZIP_ORIGINAL_NAME) {
			/* Skip over the filename (which is in ISO 8859-1 encoding).  */
			do {
				if (NULL == (data = gsf_input_read (input->source, 1, NULL)))
					return TRUE;
			} while (*data != 0);
		}

		if (flags & GZIP_HAS_COMMENT) {
			/* Skip over the comment (which is in ISO 8859-1 encoding).  */
			do {
				if (NULL == (data = gsf_input_read (input->source, 1, NULL)))
					return TRUE;
			} while (*data != 0);
		}

		if (flags & GZIP_HEADER_CRC &&
		    NULL == (data = gsf_input_read (input->source, 2, NULL)))
			return TRUE;

		input->header_size = input->source->cur_offset;
		/* the last 8 bytes are the crc and size.  */
		input->trailer_size = 8;
	}

	gsf_input_set_size (GSF_INPUT (input), input->uncompressed_size);

	if (gsf_input_remaining (input->source) < input->trailer_size)
		return TRUE;	/* No room for payload */

	return FALSE;
}

static gboolean
init_zip (GsfInputGZip *gzip, GError **err)
{
	gsf_off_t cur_pos;

	if (Z_OK != inflateInit2 (&(gzip->stream), -MAX_WBITS)) {
		if (err != NULL)
			*err = g_error_new (gsf_input_error_id (), 0,
				"Unable to initialize zlib");
		return TRUE;
	}

	cur_pos = gsf_input_tell (gzip->source);
	if (gsf_input_seek (gzip->source, 0, G_SEEK_SET)) {
		if (err)
			*err = g_error_new (gsf_input_error_id (), 0,
					    "Failed to rewind source");
		return TRUE;
	}

	if (check_header (gzip) != FALSE) {
		if (err != NULL)
			*err = g_error_new (gsf_input_error_id (), 0,
				"Invalid gzip header");
		if (gsf_input_seek (gzip->source, cur_pos, G_SEEK_SET)) {
			g_warning ("attempt to restore position failed ??");
		}
		return TRUE;
	}

	return FALSE;
}

/**
 * gsf_input_gzip_new :
 * @source : The underlying data source.
 * @err	   : optionally NULL.
 *
 * Adds a reference to @source.
 *
 * Returns a new file or NULL.
 **/
GsfInput *
gsf_input_gzip_new (GsfInput *source, GError **err)
{
	GsfInputGZip *gzip;

	g_return_val_if_fail (GSF_IS_INPUT (source), NULL);

	gzip = g_object_new (GSF_INPUT_GZIP_TYPE,
			     "source", source,
			     NULL);
	if (gzip->err) {
		if (err)
			*err = g_error_copy (gzip->err);
		g_object_unref (gzip);
		return NULL;
	}

	return GSF_INPUT (gzip);
}

static void
gsf_input_gzip_finalize (GObject *obj)
{
	GsfInputGZip *gzip = (GsfInputGZip *)obj;

	if (gzip->source != NULL) {
		g_object_unref (G_OBJECT (gzip->source));
		gzip->source = NULL;
	}

	g_free (gzip->buf);

	if (gzip->stream.state != NULL)
		inflateEnd (&(gzip->stream));

	g_clear_error (&gzip->err);

	parent_class->finalize (obj);
}

static GsfInput *
gsf_input_gzip_dup (GsfInput *src_input, GError **err)
{
	GsfInputGZip const *src = (GsfInputGZip *)src_input;
	GsfInputGZip *dst;
	GsfInput *src_source_copy;

	if (src->source) {
		src_source_copy = gsf_input_dup (src->source, err);
		if (err)
			return NULL;
	} else
		src_source_copy = NULL;

	dst = g_object_new (GSF_INPUT_GZIP_TYPE,
			    "source", src_source_copy,
			    "raw", src->raw,
			    NULL);
	if (src_source_copy)
		g_object_unref (src_source_copy);

	if (src->err) {
		g_clear_error (&dst->err);
		dst->err = g_error_copy (src->err);
	} else if (dst->err) {
		if (err)
			*err = g_error_copy (dst->err);
		g_object_unref (dst);
		return NULL;
	}

	return GSF_INPUT (dst);
}

static guint8 const *
gsf_input_gzip_read (GsfInput *input, size_t num_bytes, guint8 *buffer)
{
	GsfInputGZip *gzip = GSF_INPUT_GZIP (input);

	if (buffer == NULL) {
		if (gzip->buf_size < num_bytes) {
			gzip->buf_size = MAX (num_bytes, 256);
			g_free (gzip->buf);
			gzip->buf = g_new (guint8, gzip->buf_size);
		}
		buffer = gzip->buf;
	}

	gzip->stream.next_out = buffer;
	gzip->stream.avail_out = num_bytes;
	while (gzip->stream.avail_out != 0) {
		int zerr;
		if (gzip->stream.avail_in == 0) {
			gsf_off_t remain = gsf_input_remaining (gzip->source);
			if (remain <= gzip->trailer_size) {
				if (remain < gzip->trailer_size || gzip->stop_byte_added) {
					g_clear_error (&gzip->err);
					gzip->err = g_error_new
						(gsf_input_error_id (), 0,
						 "truncated source");
					return NULL;
				}
				/* zlib requires an extra byte.  */
				gzip->stream.avail_in = 1;
				gzip->gzipped_data = "";
				gzip->stop_byte_added = TRUE;
			} else {
				size_t n = MIN (remain - gzip->trailer_size,
						Z_BUFSIZE);

				gzip->gzipped_data =
					gsf_input_read (gzip->source, n, NULL);
				if (!gzip->gzipped_data) {
					g_clear_error (&gzip->err);
					gzip->err = g_error_new
						(gsf_input_error_id (), 0,
						 "Failed to read from source");
					return NULL;
				}
				gzip->stream.avail_in = n;
			}
			gzip->stream.next_in = (Byte *)gzip->gzipped_data;
		}
		zerr = inflate (&(gzip->stream), Z_NO_FLUSH);
		if (zerr != Z_OK) {
			if (zerr != Z_STREAM_END)
				return NULL;
			/* Premature end of stream.  */
			if (gzip->stream.avail_out != 0)
				return NULL;
		}
	}

	gzip->crc = crc32 (gzip->crc, buffer, (uInt)(gzip->stream.next_out - buffer));
	return buffer;
}

static gboolean
gsf_input_gzip_seek (GsfInput *input, gsf_off_t offset, GSeekType whence)
{
	GsfInputGZip *gzip = GSF_INPUT_GZIP (input);
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

	if (pos < input->cur_offset) {
		if (gsf_input_seek (gzip->source,
				    (gsf_off_t)gzip->header_size,
				    G_SEEK_SET))
			return TRUE;
		gzip->crc = crc32 (0L, Z_NULL, 0);
		gzip->stream.avail_in = 0;
		if (inflateReset (&(gzip->stream)) != Z_OK)
			return TRUE;
		input->cur_offset = 0;
	}

	if (gsf_input_seek_emulate (input, pos))
		return TRUE;

	gzip->seek_skipped += pos;
	if (!warned &&
	    gzip->seek_skipped != pos && /* Don't warn for single seek.  */
	    gzip->seek_skipped >= 1000000) {
		warned = TRUE;
		g_warning ("Seeking in gzipped streams is awfully slow.");
	}

	return FALSE;
}

static void
gsf_input_gzip_init (GObject *obj)
{
	GsfInputGZip *gzip = GSF_INPUT_GZIP (obj);

	gzip->source = NULL;
	gzip->raw = FALSE;
	gzip->uncompressed_size = -1;
	gzip->err = NULL;
	gzip->stream.zalloc	= (alloc_func)0;
	gzip->stream.zfree	= (free_func)0;
	gzip->stream.opaque	= (voidpf)0;
	gzip->stream.next_in	= Z_NULL;
	gzip->stream.next_out	= Z_NULL;
	gzip->stream.avail_in	= gzip->stream.avail_out = 0;
	gzip->crc		= crc32 (0L, Z_NULL, 0);
	gzip->buf		= NULL;
	gzip->buf_size		= 0;
	gzip->seek_skipped = 0;
}

static void
gsf_input_gzip_get_property (GObject     *object,
			     guint        property_id,
			     GValue      *value,
			     GParamSpec  *pspec)
{
	GsfInputGZip *gzip = (GsfInputGZip *)object;

	switch (property_id) {
	case PROP_RAW:
		g_value_set_boolean (value, gzip->raw);
		break;
	case PROP_SOURCE:
		g_value_set_object (value, gzip->source);
		break;
	case PROP_UNCOMPRESSED_SIZE:
		g_value_set_int64 (value, gzip->uncompressed_size);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}

static void
gsf_input_gzip_set_source (GsfInputGZip *gzip, GsfInput *source)
{
	if (source)
		g_object_ref (GSF_INPUT (source));
	if (gzip->source)
		g_object_unref (gzip->source);
	gzip->source = source;
}

static void
gsf_input_gzip_set_property (GObject      *object,
			     guint         property_id,
			     GValue const *value,
			     GParamSpec   *pspec)
{
	GsfInputGZip *gzip = (GsfInputGZip *)object;

	switch (property_id) {
	case PROP_RAW:
		gzip->raw = g_value_get_boolean (value);
		break;
	case PROP_SOURCE:
		gsf_input_gzip_set_source (gzip, g_value_get_object (value));
		break;
	case PROP_UNCOMPRESSED_SIZE:
		gzip->uncompressed_size = g_value_get_int64 (value);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}

static GObject*
gsf_input_gzip_constructor (GType                  type,
			    guint                  n_construct_properties,
			    GObjectConstructParam *construct_params)
{
  GsfInputGZip *gzip;

  gzip = (GsfInputGZip *)(parent_class->constructor (type,
						     n_construct_properties,
						     construct_params));

  if (!gzip->source) {
	  g_clear_error (&gzip->err);
	  gzip->err = g_error_new (gsf_input_error_id (), 0,
				   "NULL source");
  } else if (gzip->raw && gzip->uncompressed_size < 0) {
	  g_clear_error (&gzip->err);
	  gzip->err = g_error_new (gsf_input_error_id (), 0,
				   "Uncompressed size not set");
  } else if (init_zip (gzip, &gzip->err) != FALSE) {
	  /* Nothing more.  */
  }

  return (GObject *)gzip;
}

static void
gsf_input_gzip_class_init (GObjectClass *gobject_class)
{
	GsfInputClass *input_class = GSF_INPUT_CLASS (gobject_class);

	gobject_class->constructor  = gsf_input_gzip_constructor;
	gobject_class->finalize     = gsf_input_gzip_finalize;
	gobject_class->set_property = gsf_input_gzip_set_property;
	gobject_class->get_property = gsf_input_gzip_get_property;
	input_class->Dup	    = gsf_input_gzip_dup;
	input_class->Read	    = gsf_input_gzip_read;
	input_class->Seek	    = gsf_input_gzip_seek;

	g_object_class_install_property
		(gobject_class,
		 PROP_RAW,
		 g_param_spec_boolean ("raw", "Raw",
				       "Whether to read compressed data with no header and no trailer.",
				       FALSE,
				       GSF_PARAM_STATIC |
				       G_PARAM_READWRITE |
				       G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property
		(gobject_class,
		 PROP_SOURCE,
		 g_param_spec_object ("source", "Source",
				      "Where the compressed data comes from.",
				      GSF_INPUT_TYPE,
				      GSF_PARAM_STATIC |
				      G_PARAM_READWRITE |
				      G_PARAM_CONSTRUCT_ONLY));
	/**
	 * GsfInputGzip:uncompressed_size:
	 *
	 * The size that the data will have after uncompression.
	 * The is mandatory for raw streams and if the uncompressed size is
	 * larger than 4GB.
	 */  
	g_object_class_install_property
		(gobject_class,
		 PROP_UNCOMPRESSED_SIZE,
		 g_param_spec_int64 ("uncompressed-size", "Size after decompression",
				     "The source's uncompressed size",
				     -1, G_MAXINT64, -1,
				     GSF_PARAM_STATIC |
				     G_PARAM_READWRITE |
				     G_PARAM_CONSTRUCT_ONLY));

	parent_class = g_type_class_peek_parent (gobject_class);
}

GSF_CLASS (GsfInputGZip, gsf_input_gzip,
	   gsf_input_gzip_class_init, gsf_input_gzip_init, GSF_INPUT_TYPE)
