/*
 * gsf-output-iconv.c: wrapper to convert character sets.
 *
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
#include <gsf/gsf-output-iconv.h>
#include <gsf/gsf-output-impl.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-utils.h>
#include <glib/gconvert.h>
#include <string.h>

#define BUF_SIZE 0x400

static GObjectClass *parent_class;

struct _GsfOutputIconv {
	GsfOutput output;

	GsfOutput *sink;
	char *input_charset;
	char *output_charset;
	char *fallback;

	guint8 *buf;
	size_t buf_len;
};

enum {
	PROP_0,
	PROP_SINK,
	PROP_INPUT_CHARSET,
	PROP_OUTPUT_CHARSET,
	PROP_FALLBACK
};

/**
 * gsf_output_iconv_new :
 * @sink : The underlying data source.
 * @dst : The target character set.
 * @src : The source character set.
 *
 * Adds a reference to @sink.
 *
 * Returns a new GsfOutput object or NULL.
 **/
GsfOutput *
gsf_output_iconv_new (GsfOutput *sink, char const *dst, char const *src)
{
	GError *error = NULL;
	g_return_val_if_fail (GSF_IS_OUTPUT (sink), NULL);

	if (!dst) dst = "UTF-8";
	if (!src) src = "UTF-8";
	g_free (g_convert ("", 0, dst, src, NULL, NULL, &error));

	if (error) {
		g_error_free (error);
		return NULL;
	}

	return g_object_new (GSF_OUTPUT_ICONV_TYPE,
			     "sink", sink,
			     "input-charset", src,
			     "output-charset", dst,
			     NULL);
}

static void
gsf_output_iconv_finalize (GObject *obj)
{
	GsfOutputIconv *ic = (GsfOutputIconv *)obj;

	if (ic->sink != NULL)
		g_object_unref (G_OBJECT (ic->sink));
	g_free (ic->input_charset);
	g_free (ic->output_charset);
	g_free (ic->buf);

	parent_class->finalize (obj);
}

static gboolean
iconv_flush (GsfOutputIconv *ic, gboolean must_empty)
{
	if (gsf_output_error (GSF_OUTPUT (ic)))
		return FALSE;

	if (ic->buf_len > 0) {
		gsize bytes_read, bytes_written;
		gboolean ok;
		char *data = g_convert_with_fallback (ic->buf, ic->buf_len,
						      ic->output_charset,
						      ic->input_charset,
						      ic->fallback,
						      &bytes_read,
						      &bytes_written,
						      NULL);
		if (data == NULL || bytes_read <= 0) {
			gsf_output_set_error (GSF_OUTPUT (ic),
					      0,
					      "Failed to convert string");
			ok = FALSE;
		} else {
			ic->buf_len -= bytes_read;
			if (ic->buf_len)
				g_memmove (ic->buf, ic->buf + ic->buf_len, ic->buf_len);

			ok = gsf_output_write (ic->sink, bytes_written, data);
			if (!ok) {
				gsf_output_set_error (GSF_OUTPUT (ic),
						      0,
						      "Failed to write");
			}
		}

		g_free (data);
		return ok && (!must_empty || ic->buf_len == 0);
	} else
		return TRUE;
}

static gboolean
gsf_output_iconv_write (GsfOutput *output,
			size_t num_bytes, guint8 const *data)
{
	GsfOutputIconv *ic = GSF_OUTPUT_ICONV (output);

	g_return_val_if_fail (data, FALSE);

	while (num_bytes > 0) {
		if (gsf_output_error (output))
			return FALSE;
		if (ic->buf_len == BUF_SIZE)
			iconv_flush (ic, FALSE);
		else {
			size_t count = MIN (BUF_SIZE - ic->buf_len, num_bytes);
			memcpy (ic->buf + ic->buf_len, data, count);
			ic->buf_len += count;
			num_bytes -= count;
			data += count;
		}
	}

	return TRUE;
}

static gboolean
gsf_output_iconv_seek (G_GNUC_UNUSED GsfOutput *output,
		      G_GNUC_UNUSED gsf_off_t offset,
		      G_GNUC_UNUSED GSeekType whence)
{
	return FALSE;
}

static gboolean
gsf_output_iconv_close (GsfOutput *output)
{
	if (!gsf_output_error (output)) {
		GsfOutputIconv *ic = GSF_OUTPUT_ICONV (output);

		if (!iconv_flush (ic, TRUE))
			return FALSE;
	}

	return TRUE;
}

static void
gsf_output_iconv_init (GObject *obj)
{
	GsfOutputIconv *ic = GSF_OUTPUT_ICONV (obj);

	ic->buf = g_malloc (BUF_SIZE);
	ic->buf_len = 0;
}

static void
gsf_output_iconv_get_property (GObject     *object,
			      guint        property_id,
			      GValue      *value,
			      GParamSpec  *pspec)
{
	GsfOutputIconv *ic = (GsfOutputIconv *)object;

	switch (property_id) {
	case PROP_SINK:
		g_value_set_object (value, ic->sink);
		break;
	case PROP_INPUT_CHARSET:
		g_value_set_string (value, ic->input_charset);
		break;
	case PROP_OUTPUT_CHARSET:
		g_value_set_string (value, ic->output_charset);
		break;
	case PROP_FALLBACK:
		g_value_set_string (value, ic->fallback);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}

static void
gsf_output_iconv_set_sink (GsfOutputIconv *ic, GsfOutput *sink)
{
	g_return_if_fail (GSF_IS_OUTPUT (sink));
	g_object_ref (sink);
	if (ic->sink)
		g_object_unref (ic->sink);
	ic->sink = sink;
}

static void
gsf_output_iconv_set_property (GObject      *object,
			       guint         property_id,
			       GValue const *value,
			       GParamSpec   *pspec)
{
	GsfOutputIconv *ic = (GsfOutputIconv *)object;
	char *scopy;

	switch (property_id) {
	case PROP_SINK:
		gsf_output_iconv_set_sink (ic, g_value_get_object (value));
		break;
	case PROP_INPUT_CHARSET:
		ic->input_charset = g_strdup (g_value_get_string (value));
		break;
	case PROP_OUTPUT_CHARSET:
		ic->output_charset = g_strdup (g_value_get_string (value));
		break;
	case PROP_FALLBACK:
		scopy = g_strdup (g_value_get_string (value));
		g_free (ic->fallback);
		ic->fallback = scopy;
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}

static void
gsf_output_iconv_class_init (GObjectClass *gobject_class)
{
	GsfOutputClass *output_class = GSF_OUTPUT_CLASS (gobject_class);

	gobject_class->finalize     = gsf_output_iconv_finalize;
	gobject_class->set_property = gsf_output_iconv_set_property;
	gobject_class->get_property = gsf_output_iconv_get_property;
	output_class->Write	    = gsf_output_iconv_write;
	output_class->Seek	    = gsf_output_iconv_seek;
	output_class->Close	    = gsf_output_iconv_close;

	g_object_class_install_property
		(gobject_class,
		 PROP_SINK,
		 g_param_spec_object ("sink", "Sink",
				      "Where the converted data is written.",
				      GSF_OUTPUT_TYPE,
				      GSF_PARAM_STATIC |
				      G_PARAM_READWRITE |
				      G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property
		(gobject_class,
		 PROP_INPUT_CHARSET,
		 g_param_spec_string ("input-charset", "Input Charset",
				      "The character set to convert from.",
				      "UTF-8",
				      GSF_PARAM_STATIC |
				      G_PARAM_READWRITE |
				      G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property
		(gobject_class,
		 PROP_OUTPUT_CHARSET,
		 g_param_spec_string ("output-charset", "Output Charset",
				      "The character set to convert to.",
				      "UTF-8",
				      GSF_PARAM_STATIC |
				      G_PARAM_READWRITE |
				      G_PARAM_CONSTRUCT_ONLY));
	/**
	 * GsfOutputIconv:fallback:
	 *
	 * Either NULL or a UTF-8 string (representable in the target encoding)
	 * to convert and output in place of characters that cannot be represented
	 * in the target encoding.  NULL means use \u1234 or \U12345678 format.
	 */  
	g_object_class_install_property
		(gobject_class,
		 PROP_FALLBACK,
		 g_param_spec_string ("fallback", "Fallback",
				      "The string to use for invalid characters.",
				      NULL,
				      GSF_PARAM_STATIC |
				      G_PARAM_READWRITE));

	parent_class = g_type_class_peek_parent (gobject_class);
}

GSF_CLASS (GsfOutputIconv, gsf_output_iconv,
	   gsf_output_iconv_class_init, gsf_output_iconv_init, GSF_OUTPUT_TYPE)
