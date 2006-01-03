/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-input-iochannel.c: BZ2 based input
 *
 * Copyright (C) 2003-2004 Dom Lachowicz (cinamod@hotmail.com)
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
#include <gsf-input-bzip.h>
#include <gsf-output-memory.h>
#include <string.h>

#ifdef HAVE_BZ2
/* For getting FILE.  Don't ask.  */
#include <stdio.h>
#include <bzlib.h>
#define BZ_BUFSIZ 1024
#endif

/**
 * gsf_input_memory_new_from_bzip :
 * @source : a #GsfInput
 * @err    : a #GError
 *
 * Returns a new #GsfInputMemory or NULL.
 */
GsfInput * 
gsf_input_memory_new_from_bzip (GsfInput *source, GError **err)
{
#ifndef HAVE_BZ2
	if (err)
		*err = g_error_new (gsf_input_error_id (), 0,
				    "BZ2 support not enabled");
	return NULL;
#else
	bz_stream  bzstm;
	GsfInput  *mem       = NULL;
	GsfOutput *sink      = NULL;
	guint8     out_buf [BZ_BUFSIZ];
	int        bzerr     = BZ_OK;

	g_return_val_if_fail (source != NULL, NULL);

	memset (&bzstm, 0, sizeof (bzstm));
	if (BZ_OK != BZ2_bzDecompressInit (&bzstm, 0, 0)) {
		if (err)
			*err = g_error_new (gsf_input_error_id (), 0,
					    "BZ2 decompress init failed");
		return NULL;
	}

	sink = gsf_output_memory_new ();

	for (;;) {
		bzstm.next_out  = (char *)out_buf;
		bzstm.avail_out = (unsigned int)sizeof (out_buf);

		if (bzstm.avail_in == 0) {
			bzstm.avail_in = (unsigned int)MIN (gsf_input_remaining (source), BZ_BUFSIZ);
			bzstm.next_in  = (char *)gsf_input_read (source, bzstm.avail_in, NULL);
		}
		
		bzerr = BZ2_bzDecompress (&bzstm);
		if (bzerr != BZ_OK && bzerr != BZ_STREAM_END) {
			if (err)
				*err = g_error_new (gsf_input_error_id (), 0,
						    "BZ2 decompress failed");
			BZ2_bzDecompressEnd (&bzstm);
			gsf_output_close (sink);
			g_object_unref (G_OBJECT (sink));
			return NULL;
		}
		
		gsf_output_write (sink, BZ_BUFSIZ - bzstm.avail_out, out_buf);
		if (bzerr == BZ_STREAM_END)
			break;
	}

	gsf_output_close (sink);
	
	if (BZ_OK != BZ2_bzDecompressEnd (&bzstm)) {
		if (err)
			*err = g_error_new (gsf_input_error_id (), 0,
					    "BZ2 decompress end failed");
		g_object_unref (G_OBJECT (sink));
		return NULL;
	}

	mem = gsf_input_memory_new_clone (
		gsf_output_memory_get_bytes (GSF_OUTPUT_MEMORY (sink)), 
		gsf_output_size (sink));
	g_object_unref (G_OBJECT (sink));
	return mem;
#endif
}
