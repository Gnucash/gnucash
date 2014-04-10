/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * test-bzip.c:
 *
 * Copyright (C) 2003 Dom Lachowicz (cinamod@hotmail.com)
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
#include <gsf/gsf-input-stdio.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-input-bzip.h>

static int
test (int argc, char *argv[])
{
	GsfInput *input;
	GsfInput *bzip;
	GError   *err;
	int i;

	for (i = 1 ; i < argc ; i++) {
		puts (argv[i]);
		input = gsf_input_stdio_new (argv[i], &err);
		if (input == NULL) {

			g_return_val_if_fail (err != NULL, 1);

			g_warning ("'%s' error: %s", argv[i], err->message);
			g_error_free (err);
			continue;
		}

		bzip = gsf_input_memory_new_from_bzip (input, &err);
		if (bzip == NULL) {

			g_return_val_if_fail (err != NULL, 1);

			g_warning ("'%s' Not a BZip file: %s", argv[i], err->message);
			g_error_free (err);
			g_object_unref (G_OBJECT (input));
			continue;
		}
		gsf_input_dump (bzip, FALSE);

		g_object_unref (G_OBJECT (bzip));
		g_object_unref (G_OBJECT (input));
	}

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
