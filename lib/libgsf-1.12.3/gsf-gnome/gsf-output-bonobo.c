/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-output-bonobo.c: bonobo based output
 *
 * Copyright (C) 2002-2003 Dom Lachowicz (cinamod@hotmail.com)
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
#include <gsf-gnome/gsf-output-bonobo.h>
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-persist-stream.h>
#include <gsf/gsf-output-impl.h>
#include <gsf/gsf-impl-utils.h>

struct _GsfOutputBonobo {
	GsfOutput output;
	Bonobo_Stream stream ;
};

typedef struct {
	GsfOutputClass output_class;
} GsfOutputBonoboClass;

/**
 * gsf_output_bonobo_new :
 * @stream : non-NULL bonobo stream
 * @err	     : optionally NULL.
 *
 * Returns a new file or NULL.
 **/
GsfOutput *
gsf_output_bonobo_new (Bonobo_Stream const stream, G_GNUC_UNUSED GError **err)
{
	GsfOutputBonobo *res;

	res = g_object_new (GSF_OUTPUT_BONOBO_TYPE, NULL);
	res->stream = stream;

	return GSF_OUTPUT (res);
}

static gboolean
gsf_output_bonobo_close (GsfOutput *output)
{
	GsfOutputBonobo *bonobo = GSF_OUTPUT_BONOBO (output);
	gboolean res = FALSE;

	if (bonobo->stream != NULL) {
		bonobo->stream = NULL;
		res = TRUE;
	}

	return res;
}

static gboolean
gsf_output_bonobo_seek (GsfOutput *output, gsf_off_t offset,
			GSeekType whence)
{
	GsfOutputBonobo const *bonobo = GSF_OUTPUT_BONOBO (output);
	Bonobo_Stream_SeekType bwhence = 0; /* make compiler shut up */
	CORBA_long	  pos;
	CORBA_Environment ev;

	g_return_val_if_fail (bonobo->stream != CORBA_OBJECT_NIL, 
		gsf_output_set_error (output, 0, "missing stream"));

	switch (whence) {
	case G_SEEK_SET : bwhence = Bonobo_Stream_SeekSet; break;
	case G_SEEK_CUR : bwhence = Bonobo_Stream_SeekCur; break;
	case G_SEEK_END : bwhence = Bonobo_Stream_SeekEnd; break;
	default:
		break; /*checked in GsfOutput wrapper */
	}

	CORBA_exception_init (&ev);
	pos = Bonobo_Stream_seek
		(bonobo->stream, offset, bwhence, &ev);
	if (BONOBO_EX (&ev)) {
		gsf_output_set_error (output, 0,
			bonobo_exception_get_text (&ev));
		CORBA_exception_free (&ev);
		return FALSE;
	}
	return TRUE;
}

static gboolean
gsf_output_bonobo_write (GsfOutput *output,
			 size_t num_bytes,
			 guint8 const *buffer)
{
	GsfOutputBonobo *bonobo = GSF_OUTPUT_BONOBO (output);
	Bonobo_Stream_iobuf *bsobuf;
	CORBA_Environment ev;

	g_return_val_if_fail (bonobo != NULL, FALSE);
	g_return_val_if_fail (bonobo->stream != NULL, FALSE);

	bsobuf = Bonobo_Stream_iobuf__alloc ();
	bsobuf->_buffer = (CORBA_octet*)buffer;
	bsobuf->_length = num_bytes;

	CORBA_exception_init (&ev);
	Bonobo_Stream_write (bonobo->stream, bsobuf, &ev);
	if (BONOBO_EX (&ev)) {
		g_warning (bonobo_exception_get_text (&ev));
		CORBA_exception_free (&ev);
		return FALSE;
	}

	return TRUE;
}

static void
gsf_output_bonobo_finalize (GObject *obj)
{
	GObjectClass *parent_class;
	GsfOutput *output = (GsfOutput *)obj;

	gsf_output_bonobo_close (output);

	parent_class = g_type_class_peek (GSF_OUTPUT_TYPE);
	if (parent_class && parent_class->finalize)
		parent_class->finalize (obj);
}

static void
gsf_output_bonobo_init (GObject *obj)
{
	GsfOutputBonobo *stream = GSF_OUTPUT_BONOBO (obj);

	stream->stream = NULL;
}

static void
gsf_output_bonobo_class_init (GObjectClass *gobject_class)
{
	GsfOutputClass *output_class = GSF_OUTPUT_CLASS (gobject_class);

	gobject_class->finalize = gsf_output_bonobo_finalize;
	output_class->Close	= gsf_output_bonobo_close;
	output_class->Seek	= gsf_output_bonobo_seek;
	output_class->Write	= gsf_output_bonobo_write;
}

GSF_CLASS (GsfOutputBonobo, gsf_output_bonobo,
	   gsf_output_bonobo_class_init, gsf_output_bonobo_init, GSF_OUTPUT_TYPE)
