/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-input-stdio.c: stdio based input
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
#include <gsf/gsf-input-stdio.h>
#include <gsf/gsf-input-impl.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-utils.h>
#include <glib/gstdio.h>

#include <stdio.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>

static GObjectClass *parent_class;

struct _GsfInputStdio {
	GsfInput input;

	FILE     *file;
	char     *filename;
	guint8   *buf;
	size_t   buf_size;
	gboolean keep_open;
};

typedef struct {
	GsfInputClass input_class;
} GsfInputStdioClass;

/**
 * gsf_input_stdio_new :
 * @filename : in utf8.
 * @err	     : optionally NULL.
 *
 * Returns a new file or NULL.
 **/
GsfInput *
gsf_input_stdio_new (char const *filename, GError **err)
{
	GsfInputStdio *input;
	struct stat st;
	FILE *file;
	gsf_off_t size;

	g_return_val_if_fail (filename != NULL, NULL);

	file = g_fopen (filename, "rb");
	if (file == NULL || fstat (fileno (file), &st) < 0) {
		if (err) {
			int save_errno = errno;
			char *utf8name = g_filename_display_name (filename);
			g_set_error (err,
				     G_FILE_ERROR,
				     g_file_error_from_errno (save_errno),
				     "%s: %s",
				     utf8name, g_strerror (save_errno));
			g_free (utf8name);
		}
		if (file) fclose (file); /* Just in case.  */
		return NULL;
	}

	if (!S_ISREG (st.st_mode)) {
		if (err) {
			char *utf8name = g_filename_display_name (filename);
			g_set_error (err, gsf_input_error_id (), 0,
				     "%s: not a regular file",
				     utf8name);
			g_free (utf8name);
		}
		fclose (file);
		return NULL;
	}

	size = st.st_size;
	input = (GsfInputStdio *)g_object_new (GSF_INPUT_STDIO_TYPE, NULL);
	input->file = file;
	input->filename = g_strdup (filename);
	input->buf  = NULL;
	input->buf_size = 0;
	input->keep_open = FALSE;
	gsf_input_set_size (GSF_INPUT (input), size);
	gsf_input_set_name_from_filename (GSF_INPUT (input), filename);

	return GSF_INPUT (input);
}

/**
 * gsf_input_stdio_new_FILE :
 * @filename  : The filename corresponding to @file.
 * @file      : an existing stdio FILE *
 * @keep_open : Should @file be closed when the wrapper is closed
 *
 * Assumes ownership of @file.  If @keep_open is true, ownership reverts
 * to caller when the GsfObject is closed.
 *
 * Returns a new GsfInput wrapper for @file.  Note: the file must be
 * seekable, so this will not work for stdin when that is a tty or pipe.
 **/
GsfInput *
gsf_input_stdio_new_FILE (char const *filename, FILE *file, gboolean keep_open)
{
	GsfInputStdio *stdio;
	struct stat st;
	gsf_off_t size;

	g_return_val_if_fail (filename != NULL, NULL);
	g_return_val_if_fail (file != NULL, NULL);

	if (fstat (fileno (file), &st) < 0)
		return NULL;
	if (!S_ISREG (st.st_mode))
		/* It's not that we really care, but we need st.st_size to be sane.  */
		return NULL;
	size = st.st_size;

	stdio = g_object_new (GSF_INPUT_STDIO_TYPE, NULL);
	stdio->file = file;
	stdio->keep_open = keep_open;
	stdio->filename = g_strdup (filename);
	gsf_input_set_size (GSF_INPUT (stdio), size);
	gsf_input_set_name_from_filename (GSF_INPUT (stdio), filename);
	return GSF_INPUT (stdio);
}

static void
gsf_input_stdio_finalize (GObject *obj)
{
	GsfInputStdio *input = (GsfInputStdio *)obj;

	if (input->file != NULL) {
		if (!input->keep_open)
			fclose (input->file);
		input->file = NULL;
	}

	g_free (input->buf);
	input->buf = NULL;
	input->buf_size = 0;

	g_free (input->filename);

	parent_class->finalize (obj);
}

static GsfInput *
gsf_input_stdio_dup (GsfInput *src_input, GError **err)
{
	GsfInputStdio const *src = (GsfInputStdio *)src_input;
	return gsf_input_stdio_new (src->filename, err);
}

static guint8 const *
gsf_input_stdio_read (GsfInput *input, size_t num_bytes,
		      guint8 *buffer)
{
	GsfInputStdio *stdio = GSF_INPUT_STDIO (input);
	size_t nread = 0, total_read = 0;

	g_return_val_if_fail (stdio != NULL, NULL);
	g_return_val_if_fail (stdio->file != NULL, NULL);

	if (buffer == NULL) {
		if (stdio->buf_size < num_bytes) {
			stdio->buf_size = num_bytes;
			g_free (stdio->buf);
			stdio->buf = g_new (guint8, stdio->buf_size);
		}
		buffer = stdio->buf;
	}

	while (total_read < num_bytes) {
		nread = fread (buffer + total_read, 1, 
			       num_bytes - total_read, stdio->file);
		total_read += nread;
		if (total_read < num_bytes &&
		    (ferror (stdio->file) || feof (stdio->file)))
			return NULL;
	}

	return buffer;
}

static gboolean
gsf_input_stdio_seek (GsfInput *input, gsf_off_t offset, GSeekType whence)
{
	GsfInputStdio const *stdio = GSF_INPUT_STDIO (input);
	int stdio_whence = SEEK_SET;

#ifndef HAVE_FSEEKO
	long loffset;
#else
	off_t loffset;
#endif

	if (stdio->file == NULL)
		return TRUE;

	loffset = offset;
	if ((gsf_off_t) loffset != offset) { /* Check for overflow */
#ifdef HAVE_FSEEKO
		g_warning ("offset too large for fseeko");
#else
		g_warning ("offset too large for fseek");
#endif
		return TRUE;
	}
	switch (whence) {
	case G_SEEK_CUR : stdio_whence = SEEK_CUR; break;
	case G_SEEK_END : stdio_whence = SEEK_END; break;
	case G_SEEK_SET:
	default:
		break;
	}

	errno = 0;
#ifdef HAVE_FSEEKO
	if (0 == fseeko (stdio->file, loffset, stdio_whence))
		return FALSE;
#else
	if (0 == fseek (stdio->file, loffset, stdio_whence))
		return FALSE;
#endif

	return TRUE;
}

static void
gsf_input_stdio_init (GObject *obj)
{
	GsfInputStdio *stdio = GSF_INPUT_STDIO (obj);

	stdio->file = NULL;
	stdio->filename = NULL;
	stdio->buf = NULL;
	stdio->buf_size = 0;
	stdio->keep_open = FALSE;
}

static void
gsf_input_stdio_class_init (GObjectClass *gobject_class)
{
	GsfInputClass *input_class = GSF_INPUT_CLASS (gobject_class);

	gobject_class->finalize = gsf_input_stdio_finalize;
	input_class->Dup	= gsf_input_stdio_dup;
	input_class->Read	= gsf_input_stdio_read;
	input_class->Seek	= gsf_input_stdio_seek;

	parent_class = g_type_class_peek_parent (gobject_class);
}

GSF_CLASS (GsfInputStdio, gsf_input_stdio,
	   gsf_input_stdio_class_init, gsf_input_stdio_init, GSF_INPUT_TYPE)
