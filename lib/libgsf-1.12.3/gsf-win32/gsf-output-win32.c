/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-output-win32.c:
 *
 * Copyright (C) 2003-2004 Dom Lachowicz <cinamod@hotmail.com>
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
#include <gsf-win32/gsf-output-win32.h>
#include <gsf/gsf-output-impl.h>
#include <gsf/gsf-impl-utils.h>
#include <string.h>
#include <objidl.h>

struct _GsfOutputIStream {
	GsfOutput output;
	IStream * stream;
};

typedef struct {
	GsfOutputClass output_class;
} GsfOutputIStreamClass;

#define NEED_ISTREAM_MACROS

#ifdef NEED_ISTREAM_MACROS
#define IStream_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IStream_Write(This,pv,cb,pcbRead) (This)->lpVtbl->Read(This,pv,cb,pcbRead)
#define IStream_Release(This) (This)->lpVtbl->Release(This)
#define IStream_Seek(This,dlibMove,dwOrigin,plibNewPosition) (This)->lpVtbl->Seek(This,dlibMove,dwOrigin,plibNewPosition)
#endif

/* declared in gsf-input-win32.c */
extern gchar * gsf_win32_hresult_to_utf8 (HRESULT hr);

static gboolean
gsf_output_istream_set_error (GsfOutput * output, HRESULT hr)
{
	if (!SUCCEEDED (hr)) {
		gchar * msg;

		msg = gsf_win32_hresult_to_utf8 (hr);
		if (msg) {
			gsf_output_set_error (output, 0, msg);
			g_free (msg);
		} /* "else" case should never happen */

		return FALSE;
	}

	return TRUE;
}

/**
 * gsf_output_istream_new :
 * @stream   : IStream stream
 *
 * Returns a new output object or NULL.
 **/
GsfOutput *
gsf_output_istream_new (IStream * stream)
{
	GsfOutputIStream *output;

	g_return_val_if_fail (stream != NULL, NULL);

	output = g_object_new (GSF_OUTPUT_ISTREAM_TYPE, NULL);
	output->stream = stream;
	IStream_AddRef (output->stream);

	return GSF_OUTPUT(output);
}

static gboolean
gsf_output_istream_close (GsfOutput *output)
{
	GsfOutputIStream *istream = GSF_OUTPUT_ISTREAM (output);
	gboolean res = FALSE;

	if (istream->stream != NULL) {
		IStream_Release (istream->stream);
		istream->stream = NULL;
		res = TRUE;
	}

	return res;
}

static void
gsf_output_istream_finalize (GObject *obj)
{
	GObjectClass *parent_class;
	GsfOutputIStream *output = (GsfOutputIStream *)obj;

	gsf_output_istream_close (GSF_OUTPUT(output));

	parent_class = g_type_class_peek (GSF_OUTPUT_TYPE);
	if (parent_class && parent_class->finalize)
		parent_class->finalize (obj);
}

static gboolean
gsf_output_istream_write (GsfOutput *output,
			  size_t num_bytes,
			  guint8 const *buffer)
{
	GsfOutputIStream *istm = GSF_OUTPUT_ISTREAM (output);
	HRESULT hr;
	ULONG nwritten, total_written = 0;

	g_return_val_if_fail (istm != NULL, FALSE);
	g_return_val_if_fail (istm->stream != NULL, FALSE);

	while (1) {
		hr = IStream_Write (istm->stream, (guint8 *)(buffer + total_written), (ULONG)(num_bytes - total_written), &nwritten);

		if (SUCCEEDED (hr)) {
			total_written += nwritten;
			if ((size_t)total_written == num_bytes)
				return TRUE;
		} else {
			return gsf_output_istream_set_error (output, hr);
		}
	}

	return TRUE;
}

static gboolean
gsf_output_istream_seek (GsfOutput *output, gsf_off_t offset, GSeekType whence)
{
	GsfOutputIStream *istm = GSF_OUTPUT_ISTREAM (output);
	DWORD dwhence = STREAM_SEEK_SET;
	HRESULT hr;

	g_return_val_if_fail (istm != NULL, gsf_output_set_error (output, 0, "missing handle"));
	g_return_val_if_fail (istm->stream != NULL, gsf_output_set_error (output, 0, "missing handle"));

	switch (whence) {
	case G_SEEK_SET :
		dwhence = STREAM_SEEK_SET;
		break;
	case G_SEEK_CUR :
		dwhence = STREAM_SEEK_CUR;
		break;
	case G_SEEK_END :
		dwhence = STREAM_SEEK_END;
		break;
	default:
		break; /* checked in parent wrapper */
	}

	hr = IStream_Seek (istm->stream, *(LARGE_INTEGER *) &offset, dwhence, NULL);

	if (SUCCEEDED (hr))
		return TRUE;

	return gsf_output_istream_set_error (output, hr);
}

static void
gsf_output_istream_init (GObject *obj)
{
	GsfOutputIStream *istm = GSF_OUTPUT_ISTREAM (obj);

	istm->stream = NULL;
}

static void
gsf_output_istream_class_init (GObjectClass *gobject_class)
{
	GsfOutputClass *output_class = GSF_OUTPUT_CLASS (gobject_class);

	gobject_class->finalize = gsf_output_istream_finalize;
	output_class->Close	= gsf_output_istream_close;
	output_class->Write	= gsf_output_istream_write;
	output_class->Seek	= gsf_output_istream_seek;
}

GSF_CLASS (GsfOutputIStream, gsf_output_istream,
	   gsf_output_istream_class_init, gsf_output_istream_init, GSF_OUTPUT_TYPE)

/***************************************************************************/
/***************************************************************************/
