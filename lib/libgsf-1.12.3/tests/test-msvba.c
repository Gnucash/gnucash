/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * test-msvba.c:
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
#include <gsf/gsf-input-memory.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-infile.h>
#include <gsf/gsf-infile-msole.h>
#include <gsf/gsf-infile-msvba.h>

#include <stdio.h>

static int
test (int argc, char *argv[])
{
	GsfInfile *vba_wrapper;
	GsfInput  *input;
	GError    *err = NULL;
	int i;

	for (i = 1 ; i < argc ; i++) {
		input = gsf_input_mmap_new (argv[i], NULL);
		if (input == NULL)	/* Only report error if stdio fails too */
			input = gsf_input_stdio_new (argv[i], &err);
		if (input != NULL) {
			GsfInfile *infile = gsf_infile_msole_new (input, &err);
			if (infile != NULL) {
				GsfInput *vba = gsf_infile_child_by_vname (infile, "_VBA_PROJECT_CUR", "VBA", NULL);
				if (vba != NULL) {
					fprintf( stderr, "%s\n",argv[i]);

					vba_wrapper = gsf_infile_msvba_new (GSF_INFILE (vba), &err);
					if (vba_wrapper != NULL) {
						g_object_unref (G_OBJECT (vba_wrapper));
					}
					g_object_unref (G_OBJECT (vba));
				} else {
#if 0
					g_warning ("'%s' : No VBA directory", argv[i]);
#endif
					continue;
				}

				g_object_unref (G_OBJECT (infile));
			}
			g_object_unref (G_OBJECT (input));
		}

		if (err != NULL) {
			g_warning ("'%s' error: %s", argv[i], err->message);
			g_error_free (err);
		}
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
