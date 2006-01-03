/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * test-ls-zip.c: test program to list content of zip files.
 *
 * Copyright (C) 2002-2003	Tambet Ingo (tambet@ximian.com)
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

#include <gsf/gsf-input-stdio.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-infile.h>
#include <gsf/gsf-infile-zip.h>

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

static int
test (int argc, char *argv[])
{
	GsfInput  *input;
	GsfInfile *infile;
	GError    *err;
	gint       i, j;

	for (i = 1; i < argc; i++) {
		fprintf (stderr, "%s\n", argv [i]);
		input = gsf_input_stdio_new (argv[i], &err);
		if (input == NULL) {

			g_return_val_if_fail (err != NULL, 1);

			g_warning ("'%s' error: %s", argv[i], err->message);
			g_error_free (err);
			return 1;
		}

		input = gsf_input_uncompress (input);
		infile = gsf_infile_zip_new (input, &err);
		g_object_unref (G_OBJECT (input));

		if (infile == NULL) {
			g_return_val_if_fail (err != NULL, 1);

			g_warning ("'%s' Not a zip file: %s", argv[i], err->message);
			g_error_free (err);
			return 1;
		}

		for (j = 0; j < gsf_infile_num_children (infile); j++) {
			GsfInput *child = gsf_infile_child_by_index (infile, j);

			g_print ("\t%s\t\t%" GSF_OFF_T_FORMAT "\n", gsf_input_name (child), gsf_input_size (child));
			g_object_unref (G_OBJECT (child));
		}

		g_object_unref (G_OBJECT (infile));
	}

	return 0;
}

int
main (int argc, char *argv[])
{
	int res;

	if (argc < 2) {
		fprintf (stderr, "%s : file.zip  ...\n", argv [0]);
		return 1;
	}

	gsf_init ();
	res = test (argc, argv);
	gsf_shutdown ();

	return res;
}
