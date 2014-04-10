/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * test-cp-zip.c: Test gsf-outfile-zip by cloning a file the hard way
 *
 * Copyright (C) 2002-2003	Jody Goldberg (jody@gnome.org)
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

#include <gsf/gsf-input-stdio.h>
#include <gsf/gsf-infile.h>
#include <gsf/gsf-infile-zip.h>

#include <gsf/gsf-output-stdio.h>
#include <gsf/gsf-outfile.h>
#include <gsf/gsf-outfile-zip.h>

#include <stdio.h>

static void
clone (GsfInfile *in, GsfOutfile *out)
{
	GsfInput *input = GSF_INPUT (in);
	GsfOutput *output = GSF_OUTPUT (out);

	if (gsf_input_size (input) > 0) {
		size_t len;
		while ((len = gsf_input_remaining (input)) > 0) {
			guint8 const *data;
			/* copy in odd sized chunks to exercise system */
			if (len > 314)
				len = 314;
			if (NULL == (data = gsf_input_read (input, len, NULL))) {
				g_warning ("error reading ?");
				return;
			}
			if (!gsf_output_write (output, len, data)) {
				g_warning ("error writing ?");
				return;
			}
		}
	} else {
		int i, n = gsf_infile_num_children (in);
		for (i = 0 ; i < n; i++) {
			const char *name;
			char *display_name;
			int level;
			gboolean is_dir;

			input = gsf_infile_child_by_index (in, i);
			name = gsf_infile_name_by_index (in, i);
			is_dir = gsf_infile_num_children (GSF_INFILE (input)) >= 0;

			g_object_get (G_OBJECT (input), "compression-level", &level, NULL);

			display_name = name
				? g_filename_display_name (name)
				: NULL;
			g_print ("%s: size=%ld, level=%d, %s\n",
				 display_name ? display_name : "?",
				 (long)gsf_input_size (input),
				 level,
				 is_dir ? "directory" : "file");
			g_free (display_name);

			output = gsf_outfile_new_child_full  (out, name, is_dir,
							      "compression-level", level,
							      NULL);
			clone (GSF_INFILE (input), GSF_OUTFILE (output));
		}
	}
	gsf_output_close (GSF_OUTPUT (out));
	g_object_unref (G_OBJECT (out));
	g_object_unref (G_OBJECT (in));
}

static int
test (char *argv[])
{
	GsfInput   *input;
	GsfInfile  *infile;
	GsfOutput  *output;
	GsfOutfile *outfile;
	GError    *err;

	fprintf (stderr, "%s\n", argv [1]);
	input = gsf_input_stdio_new (argv[1], &err);
	if (input == NULL) {

		g_return_val_if_fail (err != NULL, 1);

		g_warning ("'%s' error: %s", argv[1], err->message);
		g_error_free (err);
		return 1;
	}

	infile = gsf_infile_zip_new (input, &err);
	g_object_unref (G_OBJECT (input));

	if (infile == NULL) {
		g_return_val_if_fail (err != NULL, 1);

		g_warning ("'%s' Not a zip file: %s", argv[1], err->message);
		g_error_free (err);
		return 1;
	}

	output = gsf_output_stdio_new (argv[2], &err);
	if (output == NULL) {

		g_return_val_if_fail (err != NULL, 1);

		g_warning ("'%s' error: %s", argv[2], err->message);
		g_error_free (err);
		g_object_unref (G_OBJECT (infile));
		return 1;
	}

	outfile = gsf_outfile_zip_new (output, &err);
	g_object_unref (G_OBJECT (output));
	clone (infile, outfile);

	return 0;
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
