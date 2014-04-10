/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * test-outmem-printf.c:
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
#include <gsf/gsf-output-memory.h>

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
	guint8 const *buf;
	gsf_off_t size;
	FILE *fout;
	int i;
	int res;

	if (argc != 2) {
		fprintf (stderr, "Usage : %s outfile\n", argv[0]);
		return 1;
	}

	fout = fopen (argv[1], "w");
	if (!fout) {
		perror (argv[1]);
		return 1;
	}

	output = gsf_output_memory_new ();
	for (i = 1; i <= 100; i++) {
		if (!gsf_output_printf (output, "=== Round %d ===\n", i))
		    return 1;
		if (!test_write_once (output))
		    return 1;
	}

	buf = gsf_output_memory_get_bytes (GSF_OUTPUT_MEMORY (output));
	size = gsf_output_size (output);
	res = fwrite (buf, size, 1, fout);
	fclose (fout);
	gsf_output_close (output);
	g_object_unref (output);
	return (res == 1);
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
