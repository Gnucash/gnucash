 /* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-input-bonobo.c: bonobo based input
 *
 * Copyright (C) 2002-2003 Jon K Hellan (hellan@acm.org)
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
#include <gsf-gnome/gsf-input-bonobo.h>
#include <gsf/gsf-input-impl.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf-gnome/gsf-shared-bonobo-stream.h>
#include <bonobo/bonobo-persist-stream.h>
#include <bonobo/bonobo-exception.h>
#include <string.h>

/* FIXME: Should make CORBA environment available to caller somehow. */
struct _GsfInputBonobo {
	GsfInput input;
	GsfSharedBonoboStream *shared;
	guint8   *buf;
	size_t   buf_size;
	gsf_off_t pos;
};

typedef struct {
	GsfInputClass input_class;
} GsfInputBonoboClass;

static int
gib_synch_shared_ptr (GsfInputBonobo *binput)
{
	CORBA_Environment ev;
	CORBA_long new_pos;

	if (binput->shared == NULL)
		return 0;
	if (binput->pos == (gsf_off_t) binput->shared->pos)
		return 0;

	CORBA_exception_init (&ev);
	new_pos = (CORBA_long) binput->pos;
	Bonobo_Stream_seek (binput->shared->stream, new_pos,
			    Bonobo_Stream_SeekSet, &ev);
	if (BONOBO_EX (&ev)) {
		g_warning (bonobo_exception_get_text (&ev));
		CORBA_exception_free (&ev);
		return -1;
	} else {
		binput->shared->pos = new_pos;
		return 0;
	}
}

/**
 * gsf_input_bonobo_new :
 * @stream   : Bonobo stream
 * @err	     : optionally NULL.
 *
 * Returns a new input object or NULL.
 **/
GsfInput *
gsf_input_bonobo_new (Bonobo_Stream const stream, GError **err)
{
	GsfInputBonobo *input;
	Bonobo_StorageInfo   *info;
	CORBA_Environment ev;
	CORBA_long size;

	if (stream == NULL) {
		if (err != NULL)
			*err = g_error_new (gsf_input_error_id (), 0,
				"stream is NULL");
		return NULL;
	}

	CORBA_exception_init (&ev);
	/* <ICK!> info->size doesn't work */
	size = Bonobo_Stream_seek (stream, 0, Bonobo_Stream_SeekEnd, &ev);
	if (BONOBO_EX (&ev)) {
		if (err != NULL)
			*err = g_error_new (gsf_input_error_id (), 0,
					    "%s: %s",
					    "Error seeking to get stream size",
					    bonobo_exception_get_text (&ev));
		CORBA_exception_free (&ev);
		return NULL;
	}
	Bonobo_Stream_seek (stream, 0, Bonobo_Stream_SeekSet, &ev);
	if (BONOBO_EX (&ev)) {
		if (err != NULL)
			*err = g_error_new (gsf_input_error_id (), 0,
					    "%s: %s",
					    "Error seeking to get stream size",
					    bonobo_exception_get_text (&ev));
		CORBA_exception_free (&ev);
		return NULL;
	}
	/* </ICK!> */

	info = Bonobo_Stream_getInfo (stream, 0, &ev);
	if (BONOBO_EX (&ev)) {
		if (err != NULL)
			*err = g_error_new (gsf_input_error_id (), 0,
					    "%s: %s",
					    "Error getting stream info",
					    bonobo_exception_get_text (&ev));
		CORBA_exception_free (&ev);
		return NULL;
	}

	input = g_object_new (GSF_INPUT_BONOBO_TYPE, NULL);
	input->shared = gsf_shared_bonobo_stream_new (stream);
	input->buf  = NULL;
	input->buf_size = 0;
	gsf_input_set_size (GSF_INPUT (input), (gsf_off_t) size);
	gsf_input_set_name (GSF_INPUT (input), info->name);

	CORBA_free (info);

	return GSF_INPUT (input);
}

static void
gsf_input_bonobo_finalize (GObject *obj)
{
	GObjectClass *parent_class;
	GsfInputBonobo *input = (GsfInputBonobo *)obj;

	if (input->shared)
		g_object_unref (G_OBJECT (input->shared));
	input->shared = NULL;

	g_free (input->buf);
	input->buf = NULL;
	input->buf_size = 0;

	parent_class = g_type_class_peek (GSF_INPUT_TYPE);
	if (parent_class && parent_class->finalize)
		parent_class->finalize (obj);
}

static GsfInput *
gsf_input_bonobo_dup (GsfInput *src_input, GError **err)
{
	GsfInputBonobo const *src = (GsfInputBonobo *)src_input;
	GsfInputBonobo *dst = g_object_new (GSF_INPUT_BONOBO_TYPE, NULL);

	(void) err;

	dst->shared = src->shared;
	g_object_ref (G_OBJECT (dst->shared));

	return GSF_INPUT (dst);
}

static guint8 const *
gsf_input_bonobo_read (GsfInput *input, size_t num_bytes,
		       guint8 *buffer)
{
	GsfInputBonobo *binput = GSF_INPUT_BONOBO (input);
	CORBA_unsigned_long num_read;
	Bonobo_Stream_iobuf *bsibuf;
	CORBA_Environment ev;

	g_return_val_if_fail (binput != NULL, NULL);
	g_return_val_if_fail (binput->shared != NULL, NULL);
	g_return_val_if_fail (binput->shared->stream != NULL, NULL);

	if (buffer == NULL) {
		if (binput->buf_size < num_bytes) {
			binput->buf_size = num_bytes;
			g_free (binput->buf);
			binput->buf = g_new (guint8, binput->buf_size);
		}
		buffer = binput->buf;
	}

	if (gib_synch_shared_ptr (binput) != 0)
		return NULL;

	CORBA_exception_init (&ev);
	Bonobo_Stream_read (binput->shared->stream, (CORBA_long) num_bytes,
			    &bsibuf, &ev);
	if (BONOBO_EX (&ev)) {
		g_warning (bonobo_exception_get_text (&ev));
		return NULL;
	} else {
		memcpy (buffer, bsibuf->_buffer, bsibuf->_length);
		num_read = bsibuf->_length;
		CORBA_free (bsibuf);
	}
	if ((size_t) num_read == num_bytes) {
		return buffer;
	} else {
		g_warning ("Only read %d bytes, asked for %zd",
			   num_read, num_bytes);
		return NULL;
	}
}

static gboolean
gsf_input_bonobo_seek (GsfInput *input, gsf_off_t offset, GSeekType whence)
{
	GsfInputBonobo *binput = GSF_INPUT_BONOBO (input);
	Bonobo_Stream_SeekType bwhence;
	CORBA_long pos, coffset;
	CORBA_Environment ev;

	g_return_val_if_fail (binput != NULL, TRUE);
	g_return_val_if_fail (binput->shared != NULL, TRUE);
	g_return_val_if_fail (binput->shared->stream != NULL, TRUE);

	if (whence == G_SEEK_CUR) {
		if (gib_synch_shared_ptr (binput) != 0)
			return TRUE;
	}
	
	switch (whence) {
	case G_SEEK_SET :
		bwhence =  Bonobo_Stream_SeekSet;
		break;
	case G_SEEK_CUR :
		bwhence = Bonobo_Stream_SeekCur;
		break;
	case G_SEEK_END :
		bwhence = Bonobo_Stream_SeekEnd;
		break;
	default:
		return TRUE;
	}
	

	coffset = offset;
	if ((gsf_off_t) coffset != offset) { /* Check for overflow */
		g_warning ("offset too large for Bonobo_Stream_seek");
		return TRUE;
	}
	CORBA_exception_init (&ev);
	pos = Bonobo_Stream_seek
		(binput->shared->stream, coffset, bwhence, &ev);
	if (BONOBO_EX (&ev)) {
		g_warning (bonobo_exception_get_text (&ev));
		return TRUE;
	} else {
		binput->shared->pos = pos;
		binput->pos = (gsf_off_t) pos;
		return FALSE;
	}
}

static void
gsf_input_bonobo_init (GObject *obj)
{
	GsfInputBonobo *binput = GSF_INPUT_BONOBO (obj);

	binput->shared = NULL;
	binput->buf  = NULL;
	binput->buf_size = 0;
}

static void
gsf_input_bonobo_class_init (GObjectClass *gobject_class)
{
	GsfInputClass *input_class = GSF_INPUT_CLASS (gobject_class);

	gobject_class->finalize = gsf_input_bonobo_finalize;
	input_class->Dup	= gsf_input_bonobo_dup;
	input_class->Read	= gsf_input_bonobo_read;
	input_class->Seek	= gsf_input_bonobo_seek;
}

GSF_CLASS (GsfInputBonobo, gsf_input_bonobo,
	   gsf_input_bonobo_class_init, gsf_input_bonobo_init, GSF_INPUT_TYPE)
