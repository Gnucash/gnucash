/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * test-cp-zip.c: Test gsf_input_copy
 *
 * Copyright (C) 2002-2003	Dom Lachowicz <cinamod@hotmail.com>
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

#include <stdio.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-input-stdio.h>
#include <gsf/gsf-infile.h>
#include <gsf/gsf-output-stdio.h>
#include <gsf/gsf-outfile.h>

static int
test (char *argv[])
{
	GsfInput   *input;
	GsfOutput  *output;
	GError     *err;
	int         rval = 0;

	input = gsf_input_stdio_new (argv[1], &err);
	if (input == NULL) {

		g_return_val_if_fail (err != NULL, 1);

		g_warning ("'%s' error: %s\n", argv[1], err->message);
		g_error_free (err);
		return 1;
	}

	output = gsf_output_stdio_new (argv[2], &err);
	if (output == NULL) {

		g_return_val_if_fail (err != NULL, 1);

		g_warning ("'%s' error: %s\n", argv[2], err->message);
		g_error_free (err);

		g_object_unref (G_OBJECT (input));
		return 1;
	}

	if (gsf_input_copy (input, output) == FALSE) {
		rval = 1;
		err = (GError*) gsf_output_error (output);
		if (err != NULL) {
			g_warning ("'%s' error: %s\n", argv[2], err->message);	
		}
	}

	g_object_unref (G_OBJECT (input));

	gsf_output_close (output);
	g_object_unref (G_OBJECT (output));

	return rval;
}

int
main (int argc, char *argv[])
{
	int res;

	if (argc != 3) {
		fprintf (stderr, "%s : infile outfile\n", argv [0]);
		return 1;
	}

	gsf_init ();
	res = test (argv);
	gsf_shutdown ();

	return res;
}
