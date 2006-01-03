/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-input-win32.c:
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
#include <gsf-win32/gsf-input-win32.h>
#include <gsf/gsf-input-impl.h>
#include <gsf/gsf-impl-utils.h>
#include <string.h>

struct _GsfInputIStream {
	GsfInput input;
	IStream * stream;
	guint8   *buf;
	size_t   buf_size;
};

typedef struct {
	GsfInputClass input_class;
} GsfInputIStreamClass;

#define NEED_ISTREAM_MACROS

#ifdef NEED_ISTREAM_MACROS
#define IStream_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IStream_Clone(This,ppstm) (This)->lpVtbl->Clone(This,ppstm)
#define IStream_Read(This,pv,cb,pcbRead) (This)->lpVtbl->Read(This,pv,cb,pcbRead)
#define IStream_Release(This) (This)->lpVtbl->Release(This)
#define IStream_Seek(This,dlibMove,dwOrigin,plibNewPosition) (This)->lpVtbl->Seek(This,dlibMove,dwOrigin,plibNewPosition)
#define IStream_Stat(This,pstatstg,grfStatFlag)	(This)->lpVtbl->Stat(This,pstatstg,grfStatFlag)
#endif

gchar * gsf_win32_hresult_to_utf8 (HRESULT hr);

gchar *
gsf_win32_hresult_to_utf8 (HRESULT hr)
{
	void * pMsgBuf;
	gchar * utf8_msg;

	if (SUCCEEDED (hr))
		return NULL;

	FormatMessage (FORMAT_MESSAGE_ALLOCATE_BUFFER|FORMAT_MESSAGE_FROM_SYSTEM, NULL, hr,
					MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR)&pMsgBuf, 0, NULL);

	utf8_msg = g_locale_to_utf8 ((char const *)pMsgBuf, -1, NULL, NULL, NULL);
	if (utf8_msg == NULL)
		utf8_msg = g_strdup ("!SUCCEEDED (hr)");

	LocalFree(pMsgBuf);

	return utf8_msg;
}

static void
hresult_to_gerror (HRESULT hr, GError ** err)
{
	if (err) {
		gchar * msg;

		msg = gsf_win32_hresult_to_utf8 (hr);

		if (msg) {
			*err = g_error_new (gsf_input_error_id (), 0, msg);
			g_free (msg);
		}
	}
}

static char *
lpwstr_to_utf8 (LPWSTR str)
{
	if (str)
		return g_utf16_to_utf8 (str, -1, NULL, NULL, NULL);
	return NULL;
}

/**
 * gsf_input_istream_new :
 * @stream   : IStream stream
 * @err	     : optionally NULL.
 *
 * Returns a new input object or NULL.
 **/
GsfInput *
gsf_input_istream_new (IStream * stream, GError **err)
{
	GsfInputIStream *input;
	STATSTG statbuf;
	HRESULT hr;
	char * name;

	if (stream == NULL) {
		if (err != NULL)
			*err = g_error_new (gsf_input_error_id (), 0,
					    "stream is NULL");
		return NULL;
	}

	if (FAILED (hr = IStream_Stat (stream, &statbuf, STATFLAG_DEFAULT))) {
		hresult_to_gerror (hr, err);
		return NULL;
	}

	input = g_object_new (GSF_INPUT_ISTREAM_TYPE, NULL);
	input->stream = stream;
	input->buf  = NULL;
	input->buf_size = 0;

	IStream_AddRef (input->stream);

	/* LowPart and HiPart are the low and high 32 bit UINT parts. The MSDN documentation
	   says to use QuadPart if your compiler supports 64 bit ints. gsf_off_t is a gint64 value.
	   http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winprog/winprog/large_integer_str.asp
	 */
	gsf_input_set_size (GSF_INPUT (input), (gsf_off_t) statbuf.cbSize.QuadPart);

	name = lpwstr_to_utf8 (statbuf.pwcsName);
	if (name) {
		gsf_input_set_name (GSF_INPUT (input), name);
		g_free (name);
	}

	return GSF_INPUT(input);
}

static void
gsf_input_istream_finalize (GObject *obj)
{
	GObjectClass *parent_class;
	GsfInputIStream *input = (GsfInputIStream *)obj;

	IStream_Release (input->stream);
	input->stream = NULL;

	if (input->buf != NULL) {
		g_free (input->buf);
		input->buf  = NULL;
		input->buf_size = 0;
	}

	parent_class = g_type_class_peek (GSF_INPUT_TYPE);
	if (parent_class && parent_class->finalize)
		parent_class->finalize (obj);
}

static GsfInput *
gsf_input_istream_dup (GsfInput *src_input, GError **err)
{
	GsfInputIStream const *src = (GsfInputIStream *)src_input;
	GsfInput *dst;
	HRESULT hr;
	IStream * clone;

	g_return_val_if_fail (src_input != NULL, NULL);
	g_return_val_if_fail (src->stream != NULL, NULL);

	if (SUCCEEDED (hr = IStream_Clone (src->stream, &clone))) {
		dst = gsf_input_istream_new (clone, NULL);
		IStream_Release (clone); /* gsf_input_istream_new() adds a ref */
		return dst;
	}

	hresult_to_gerror (hr, err);
	return NULL;
}

static guint8 const *
gsf_input_istream_read (GsfInput *input, size_t num_bytes,
			guint8 *buffer)
{
	GsfInputIStream *istm = GSF_INPUT_ISTREAM (input);
	HRESULT hr;
	ULONG nread, total_read = 0;

	g_return_val_if_fail (istm != NULL, NULL);
	g_return_val_if_fail (istm->stream != NULL, NULL);

	if (buffer == NULL) {
		if (istm->buf_size < num_bytes) {
			istm->buf_size = num_bytes;
			g_free (istm->buf);
			istm->buf = g_new (guint8, istm->buf_size);
		}
		buffer = istm->buf;
	}

	while (1)
	    {
		    hr = IStream_Read (istm->stream, (buffer + total_read), (ULONG)(num_bytes - total_read), &nread);

		    if (SUCCEEDED (hr)) {
			    total_read += nread;
			    if ((size_t) total_read == num_bytes) {
				    return buffer;
			    }
		    } else
			    break;
	    }

	g_warning ("IStream read failed\n");
	return NULL;
}

static gboolean
gsf_input_istream_seek (GsfInput *input, gsf_off_t offset, GSeekType whence)
{
	GsfInputIStream *istm = GSF_INPUT_ISTREAM (input);
	DWORD dwhence;

	g_return_val_if_fail (istm != NULL, TRUE);
	g_return_val_if_fail (istm->stream != NULL, TRUE);

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
		return TRUE;
	}

	if(SUCCEEDED (IStream_Seek (istm->stream, *(LARGE_INTEGER *) &offset, dwhence, NULL)))
		return FALSE;
	return TRUE;
}

static void
gsf_input_istream_init (GObject *obj)
{
	GsfInputIStream *istm = GSF_INPUT_ISTREAM (obj);

	istm->stream = NULL;
	istm->buf  = NULL;
	istm->buf_size = 0;
}

static void
gsf_input_istream_class_init (GObjectClass *gobject_class)
{
	GsfInputClass *input_class = GSF_INPUT_CLASS (gobject_class);

	gobject_class->finalize = gsf_input_istream_finalize;
	input_class->Dup	= gsf_input_istream_dup;
	input_class->Read	= gsf_input_istream_read;
	input_class->Seek	= gsf_input_istream_seek;
}

GSF_CLASS (GsfInputIStream, gsf_input_istream,
	   gsf_input_istream_class_init, gsf_input_istream_init, GSF_INPUT_TYPE)

/***************************************************************************/
/***************************************************************************/
