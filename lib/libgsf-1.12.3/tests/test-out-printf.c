/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * test-input1.c:
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

#include <gsf/gsf-utils.h>
#include <gsf/gsf-output-stdio.h>

#include <stdio.h>

static int
test (int argc, char *argv[])
{
	GsfOutput *output;
	GError   *err;

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

	if (!gsf_output_printf (output,
				"The %s sat on the %s.\n", "cat", "mat"))
		return 1;
	if (!gsf_output_printf (output, "%d %ss sat on the %s.\n",
				2, "cat", "mat"))
		return 1;
	if (!gsf_output_puts (output,
			      "The quick brown fox is afraid of the cats.\n"))
		return 1;
	if (!gsf_output_close (output))
		return 1;
	g_object_unref (G_OBJECT (output));

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
