/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * test-zip2.c:
 *
 * Copyright (C) 2002-2003 Jody Goldberg (jody@gnome.org)
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
#include <gsf/gsf-input-textline.h>

#include <stdio.h>
#include <string.h>

static void
dump_child (GsfInfile *infile, char const *childname)
{
	GsfInput *child = gsf_infile_child_by_name (infile, childname);
	GsfInputTextline *textinput;
	unsigned char *res;
	int len = 0;
	
	if (child == NULL) {
		printf ("not an OpenOffice document\n");
		return;
	}

	textinput = (GsfInputTextline *)gsf_input_textline_new (child);
	if (textinput == NULL) {
		printf ("Could not read lines from %s",
			gsf_input_name (child));
		return;
	}

	do {
		res = gsf_input_textline_ascii_gets (textinput);
		if (res) {
			printf ("'%s'\n", res);
			len += strlen (res) + 1;
		}
	} while (res);

	printf ("Finished reading - %d bytes\n", len);
	g_object_unref (G_OBJECT (textinput));
	g_object_unref (G_OBJECT (child));
}

static int
test (int argc, char *argv[])
{
	GsfInput  *input;
	GsfInfile *infile;
	GError    *err = NULL;
	int i;

	puts (argv[1]);
	input = gsf_input_stdio_new (argv[1], &err);
	if (input == NULL) {

		g_return_val_if_fail (err != NULL, 1);

		g_warning ("'%s' error: %s", argv[1], err->message);
		g_error_free (err);
		return -1;
	}

	infile = gsf_infile_zip_new (input, &err);
	if (infile == NULL) {

		g_return_val_if_fail (err != NULL, 1);

		g_warning ("'%s' Not a Zip file: %s", argv[1], err->message);
		g_error_free (err);
		g_object_unref (G_OBJECT (input));
		return -1;
	}

	for (i = 2 ; i < argc ; i++) {
		dump_child (infile, argv[i]);
	}
	g_object_unref (G_OBJECT (infile));
	g_object_unref (G_OBJECT (input));
	
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
