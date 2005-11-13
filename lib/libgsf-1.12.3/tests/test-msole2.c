/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * test-msole1.c: test program to list content and dump raw stream data
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

#include <gsf/gsf-input-stdio.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-infile.h>
#include <gsf/gsf-infile-msole.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

static gboolean dump_as_hex = FALSE;

static void
ls_R (GsfInput *input)
{
	int i;
	char const *name = gsf_input_name (GSF_INPUT (input));
	gboolean is_dir = GSF_IS_INFILE (input) &&
		(gsf_infile_num_children (GSF_INFILE (input)) >= 0);
	
	printf ("%c '%s'\t\t%" GSF_OFF_T_FORMAT "\n",
		(is_dir ? 'd' : ' '),
		(name != NULL) ? name : "",
		gsf_input_size (GSF_INPUT (input)));

	if (is_dir) {
		puts ("{");
		for (i = 0 ; i < gsf_infile_num_children (GSF_INFILE (input)) ; i++)
			ls_R (gsf_infile_child_by_index (GSF_INFILE (input), i));
		puts ("}");
	}

	g_object_unref (G_OBJECT (input));
}

static int
test (int argc, char *argv[])
{
	GsfInput  *input;
	GsfInfile *infile;
	GError    *err;

	fprintf (stderr, "%s\n", argv [1]);
	input = gsf_input_stdio_new (argv[1], &err);
	if (input == NULL) {

		g_return_val_if_fail (err != NULL, 1);

		g_warning ("'%s' error: %s", argv[1], err->message);
		g_error_free (err);
		return 1;
	}

	input = gsf_input_uncompress (input);
	infile = gsf_infile_msole_new (input, &err);
	g_object_unref (G_OBJECT (input));

	if (infile == NULL) {
		g_return_val_if_fail (err != NULL, 1);

		g_warning ("'%s' Not an OLE file: %s", argv[1], err->message);
		g_error_free (err);
		return 1;
	}

	if (argc > 2) {
		int i;
		GsfInput *child, *ptr = GSF_INPUT (infile);
		for (i = 2 ; i < argc && ptr != NULL; i++, ptr = child) {
			fprintf (stderr, "--> '%s'\n", argv [i]);
			if (GSF_IS_INFILE (ptr) &&
			    gsf_infile_num_children (GSF_INFILE (ptr)) >= 0) {
				child = gsf_infile_child_by_name (GSF_INFILE (ptr), argv [i]);

				if (child == NULL) {
					g_warning ("No child named '%s'", argv [i]);
					child = NULL;
					break;
				}
			} else {
				g_warning ("stream is not a directory '%s'", argv [i]);
				child = NULL;
				break;
			}
			g_object_unref (G_OBJECT (ptr));
		}
		if (ptr != NULL) {
			if (GSF_IS_INFILE (ptr) &&
			    gsf_infile_num_children (GSF_INFILE (ptr)) >= 0)
				ls_R (ptr); /* unrefs infile */
			else {
				gsf_input_dump (GSF_INPUT (ptr), dump_as_hex);
				g_object_unref (G_OBJECT (ptr));
			}
		}
	} else
		ls_R (GSF_INPUT (infile)); /* unrefs infile */

	return 0;
}

int
main (int argc, char *argv[])
{
	int res;

	if (argc < 2) {
		fprintf (stderr, "%s : file [--hex] stream stream ...\n", argv [0]);
		return 1;
	}

	if (argv[1] != NULL && 0 == strcmp (argv[1], "--hex")) {
		dump_as_hex = TRUE;
		argv++;
		argc--;
	}

	gsf_init ();
	res = test (argc, argv);
	gsf_shutdown ();

	return res;
}
