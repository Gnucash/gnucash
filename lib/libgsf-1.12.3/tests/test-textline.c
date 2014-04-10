/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * test-textline.c:
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
#include <gsf/gsf-input-textline.h>

#include <stdio.h>

static int
test (int argc, char *argv[])
{
	GsfInput     	 *input;
	GsfInputTextline *textline;
	GError       	 *err;
	unsigned char *line;

	if (argc < 2) {
		fprintf (stderr, "Usage : %s <text_filename>\n", argv[0]);
		return 1;
	}

	fprintf (stderr, "%s\n", argv[1]);
	input = gsf_input_stdio_new (argv[1], &err);
	if (input == NULL) {

		g_return_val_if_fail (err != NULL, 1);

		g_warning ("'%s' error: %s", argv[1], err->message);
		g_error_free (err);
		return 1;
	}
	textline = (GsfInputTextline *)gsf_input_textline_new (input);
	if (textline == NULL) {
		g_warning ("unable to create a textline");
		return 2;
	}

	while (NULL != (line = gsf_input_textline_ascii_gets (textline)))
		puts (line);

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
