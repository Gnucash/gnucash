/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-input-textline.c: textline based input
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

#include <gsf-config.h>
#include <gsf/gsf-input-textline.h>
#include <gsf/gsf-input-impl.h>
#include <gsf/gsf-impl-utils.h>

#include <string.h>

static GObjectClass *parent_class;

struct _GsfInputTextline {
	GsfInput input;

	GsfInput	*source;
	guint8 const	*remainder;
	unsigned	 remainder_size;
	unsigned	 max_line_size;

	unsigned char	*buf;
	unsigned	 buf_size;

	/* int		 current_line; */
};

typedef struct {
	GsfInputClass input_class;
} GsfInputTextlineClass;

/**
 * gsf_input_textline_new :
 * @source : in some combination of ascii and utf8
 *
 * NOTE : adds a reference to @source
 *
 * Returns a new file or NULL.
 **/
GsfInput *
gsf_input_textline_new (GsfInput *source)
{
	GsfInputTextline *input;

	g_return_val_if_fail (source != NULL, NULL);

	input = g_object_new (GSF_INPUT_TEXTLINE_TYPE, NULL);
	g_object_ref (G_OBJECT (source));
	input->source = source;
	input->buf  = NULL;
	input->buf_size = 0;
	gsf_input_set_size (GSF_INPUT (source), gsf_input_size (source));

	return GSF_INPUT (input);
}

static void
gsf_input_textline_finalize (GObject *obj)
{
	GsfInputTextline *input = (GsfInputTextline *)obj;

	if (input->source != NULL) {
		g_object_unref (G_OBJECT (input->source));
		input->source = NULL;
	}
	if (input->buf != NULL) {
		g_free (input->buf);
		input->buf  = NULL;
	}
	input->buf_size = 0;

	parent_class->finalize (obj);
}

static GsfInput *
gsf_input_textline_dup (GsfInput *src_input, G_GNUC_UNUSED GError **err)
{
	GsfInputTextline const *src = (GsfInputTextline *)src_input;
	GsfInputTextline *dst = g_object_new (GSF_INPUT_TEXTLINE_TYPE, NULL);

	dst->source = src->source;
	g_object_ref (G_OBJECT (dst->source));
	gsf_input_set_size (GSF_INPUT (dst), gsf_input_size (src_input));

	return GSF_INPUT (dst);
}

static guint8 const *
gsf_input_textline_read (GsfInput *input, size_t num_bytes, guint8 *buffer)
{
	GsfInputTextline *textline = GSF_INPUT_TEXTLINE (input);
	textline->remainder = NULL;
	return gsf_input_read (textline->source, num_bytes, buffer);
}

static gboolean
gsf_input_textline_seek (GsfInput *input, gsf_off_t offset, GSeekType whence)
{
	GsfInputTextline *textline = GSF_INPUT_TEXTLINE (input);
	textline->remainder = NULL;
	return gsf_input_seek (textline->source, offset, whence);
}

static void
gsf_input_textline_init (GObject *obj)
{
	GsfInputTextline *textline = GSF_INPUT_TEXTLINE (obj);

	textline->source	 = NULL;
	textline->remainder	 = NULL;
	textline->remainder_size = 0;
	textline->max_line_size  = 512;	/* an initial guess */
	textline->buf		 = NULL;
	textline->buf_size	 = 0;
}

static void
gsf_input_textline_class_init (GObjectClass *gobject_class)
{
	GsfInputClass *input_class = GSF_INPUT_CLASS (gobject_class);

	gobject_class->finalize = gsf_input_textline_finalize;
	input_class->Dup	= gsf_input_textline_dup;
	input_class->Read	= gsf_input_textline_read;
	input_class->Seek	= gsf_input_textline_seek;

	parent_class = g_type_class_peek_parent (gobject_class);
}

GSF_CLASS (GsfInputTextline, gsf_input_textline,
	   gsf_input_textline_class_init, gsf_input_textline_init, GSF_INPUT_TYPE)

/**
 * gsf_input_textline_ascii_gets :
 * @input :
 *
 * A utility routine to read things line by line from the underlying source.
 * Trailing newlines and carriage returns are stripped, and the resultant buffer
 * can be edited.
 *
 * returns the string read, or NULL on eof.
 **/
unsigned char *
gsf_input_textline_ascii_gets (GsfInputTextline *textline)
{
	guint8 const *ptr ,*end;
	gsf_off_t remain;
	unsigned len, count = 0;

	g_return_val_if_fail (textline != NULL, NULL);

	while (1) {
		if (textline->remainder == NULL ||
		    textline->remainder_size == 0) {
			remain = gsf_input_remaining (textline->source);
			len = MIN (remain, textline->max_line_size);

			textline->remainder = gsf_input_read (textline->source, len, NULL);
			if (textline->remainder == NULL)
				return NULL;
			textline->remainder_size = len;
		}

		ptr = textline->remainder;
		end = ptr + textline->remainder_size;
		for (; ptr < end ; ptr++)
			if (*ptr == '\n' || *ptr == '\r')
				break;

		/* copy the remains into the buffer, grow it if necessary */
		len = ptr - textline->remainder;
		if (count + len >= textline->buf_size) {
			textline->buf_size += len;
			textline->buf = g_renew (guint8, textline->buf,
						 textline->buf_size + 1);
		}

		g_return_val_if_fail (textline->buf != NULL, NULL);

		memcpy (textline->buf + count, textline->remainder, len);
		count += len;

		if (ptr < end) {
			unsigned char last = ptr [0];

			/* eat the trailing new line */
			ptr++;
			if (ptr >= end) {
				/* be extra careful, the newline is at the bound */
				if (gsf_input_remaining (textline->source) > 0) {
					ptr = gsf_input_read (textline->source, 1, NULL);
					if (ptr == NULL)
						return NULL;
					textline->remainder = ptr;
					textline->remainder_size = 1;
					end = ptr + 1;
				} else
					ptr = end = NULL;
			}
			if (ptr != NULL &&
			    ((last == '\n' && *ptr == '\r') ||
			     (last == '\r' && *ptr == '\n')))
				ptr++;
			break;
		} else if (gsf_input_remaining (textline->source) <= 0) {
			ptr = end = NULL;
			break;
		} else
			textline->remainder = NULL;

	}

	textline->remainder = ptr;
	textline->remainder_size = end - ptr;

	textline->buf [count] = '\0';
	return textline->buf;
}

/**
 * gsf_input_textline_utf8_gets :
 * @input :
 *
 * A utility routine to read things line by line from the underlying source.
 * Trailing newlines and carriage returns are stripped, and the resultant buffer
 * can be edited.
 *
 * returns the string read, or NULL on eof.
 **/
guint8 *
gsf_input_textline_utf8_gets (GsfInputTextline *textline)
{
	guint8 const *ptr ,*end;
	gsf_off_t remain;
	unsigned len, count = 0;

	g_return_val_if_fail (textline != NULL, NULL);

	while (1) {
		if (textline->remainder == NULL ||
		    textline->remainder_size == 0) {
			remain = gsf_input_remaining (textline->source);
			len = MIN (remain, textline->max_line_size);

			textline->remainder = gsf_input_read (textline->source, len, NULL);
			if (textline->remainder == NULL)
				return NULL;
			textline->remainder_size = len;
		}

		ptr = textline->remainder;
		end = ptr + textline->remainder_size;
		for (; ptr < end ; ptr = (guint8 *) g_utf8_next_char (ptr))
			if (*ptr == '\n' || *ptr == '\r')
				break;

		/* copy the remains into the buffer, grow it if necessary */
		len = ptr - textline->remainder;
		if (count + len >= textline->buf_size) {
			textline->buf_size += len;
			textline->buf = g_renew (guint8, textline->buf,
						 textline->buf_size + 1);
		}

		g_return_val_if_fail (textline->buf != NULL, NULL);

		memcpy (textline->buf + count, textline->remainder, len);
		count += len;

		if (ptr < end) {
			unsigned char last = ptr [0];

			/* eat the trailing new line */
			ptr++;
			if (ptr >= end) {
				/* be extra careful, the newline is at the bound */
				if (gsf_input_remaining (textline->source) > 0) {
					ptr = gsf_input_read (textline->source, 1, NULL);
					if (ptr == NULL)
						return NULL;
					textline->remainder = ptr;
					textline->remainder_size = 1;
					end = ptr + 1;
				} else
					ptr = end = NULL;
			}
			if (ptr != NULL &&
			    ((last == '\n' && *ptr == '\r') ||
			     (last == '\r' && *ptr == '\n')))
				ptr++;
			break;
		} else if (gsf_input_remaining (textline->source) <= 0) {
			ptr = end = NULL;
			break;
		} else
			textline->remainder = NULL;

	}

	textline->remainder = ptr;
	textline->remainder_size = end - ptr;

	textline->buf [count] = '\0';
	return textline->buf;
}
