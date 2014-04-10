/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * test-msole-printf.c:
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

#include <gsf/gsf-utils.h>
#include <gsf/gsf-output-stdio.h>
#include <gsf/gsf-outfile.h>
#include <gsf/gsf-outfile-msole.h>

#include <stdio.h>

static gboolean
test_write_once (GsfOutput *output)
{
	if (!gsf_output_printf (output,
				"The %s sat on the %s.\n", "cat", "mat"))
		return FALSE;
	if (!gsf_output_printf (output, "%d %ss sat on the %s.\n",
				2, "cat", "mat"))
		return FALSE;
	if (!gsf_output_puts (output,
			      "The quick brown fox is afraid of the cats.\n"))
		return FALSE;

	return TRUE;
}

static int
test (int argc, char *argv[])
{
	GsfOutput  *output;
	GsfOutfile *outfile;
	GsfOutput  *small;
	GsfOutput  *large;
	GError   *err;
	int i;

	if (argc != 2) {
		fprintf (stderr, "Usage : %s outfile\n", argv[0]);
		return 1;
	}

	output = gsf_output_stdio_new (argv[1], &err);
	if (output == NULL) {
		g_return_val_if_fail (err != NULL, 1);

		g_warning ("'%s' error: %s", argv[1], err->message);
		g_error_free (err);
		return 1;
	}
	outfile = gsf_outfile_msole_new (output);
	g_object_unref (G_OBJECT (output));
	small = gsf_outfile_new_child  (outfile, "small", FALSE);

	if (!test_write_once (small))
		return 1;
	if (!gsf_output_close (small))
		return 1;

	large = gsf_outfile_new_child  (outfile, "large", FALSE);

	for (i = 1; i <= 100; i++) {
		if (!gsf_output_printf (large, "=== Round %d ===\n", i))
		    return 1;
		if (!test_write_once (large))
		    return 1;
	}
		
	if (!gsf_output_close (large))
		return 1;
	if (!gsf_output_close ((GsfOutput *) outfile))
		return 1;
	g_object_unref (G_OBJECT (outfile));
	g_object_unref (small);

	return 0;
}

int
main (int argc, char *argv[])
{
	int res;

	gsf_init ();
	res = test (argc, argv);
	gsf_shutdown ();

	return res;
}
