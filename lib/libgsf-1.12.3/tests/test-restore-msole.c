/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * test-cp-msole.c: Test gsf-outfile-msole by cloning a file the hard way
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
#include <gsf/gsf-infile-stdio.h>
#include <gsf/gsf-infile-msole.h>

#include <gsf/gsf-output-stdio.h>
#include <gsf/gsf-outfile.h>
#include <gsf/gsf-outfile-msole.h>

#include <stdio.h>

static void
clone (GsfInput *input, GsfOutput *output)
{
	guint8 const *data;
	size_t len;
	int i;

	if (gsf_input_size (input) > 0) {
		while ((len = gsf_input_remaining (input)) > 0) {
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
	} 
	
	if (GSF_IS_INFILE (input) &&
	    gsf_infile_num_children (GSF_INFILE (input)) > 0) {
		GsfInfile *in = GSF_INFILE (input);
		GsfOutfile *out = GSF_OUTFILE (output);
		GsfInput *src;
		GsfOutput *dst;

		for (i = 0 ; i < gsf_infile_num_children (in) ; i++) {
			src = gsf_infile_child_by_index (in, i);
			dst = gsf_outfile_new_child  (out,
				gsf_infile_name_by_index  (in, i),
				GSF_IS_INFILE (src) && gsf_infile_num_children (GSF_INFILE (src)) >= 0);
			clone (src, dst);
		}
	}

	gsf_output_close (output);
	g_object_unref (G_OBJECT (output));
	g_object_unref (G_OBJECT (input));
}

static int
test (char *argv[])
{
	GsfInfile  *infile;
	GsfOutfile *outfile;
	GsfOutput  *output;
	GError    *err;

	fprintf (stderr, "%s\n", argv [1]);
	infile = gsf_infile_stdio_new (argv[1], &err);
	if (infile == NULL) {

		g_return_val_if_fail (err != NULL, 1);

		g_warning ("'%s' error: %s", argv[1], err->message);
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

	outfile = gsf_outfile_msole_new (output);
	g_object_unref (G_OBJECT (output));
	clone (GSF_INPUT (infile), GSF_OUTPUT (outfile));

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
