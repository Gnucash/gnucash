/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * test-msole1.c: test program to dump biff streams
 *
 * Copyright (C) 2002-2003	Jody Goldberg (jody@gnome.org)
 * 			Michael Meeks (michael@ximian.com)
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
 *
 * Parts of this code are taken from libole2/test/test-ole.c
 */

#include <gsf/gsf-input-stdio.h>
#include <gsf/gsf-input-memory.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-infile.h>
#include <gsf/gsf-infile-msole.h>
#include <gsf/gsf-msole-utils.h>
#include <gsf/gsf-docprop-vector.h>

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#define BIFF_TYPES_FILE    "biff-types.h"
#define DUMP_CONTENT

typedef struct {
	guint16 opcode;
	char *name;
} GENERIC_TYPE;

#ifdef DUMP_CONTENT
static GPtrArray *biff_types   = NULL;
static void
read_types (char const *fname, GPtrArray **types)
{
	FILE *file = fopen(fname, "r");
	unsigned char buffer[1024];
	*types = g_ptr_array_new ();
	if (!file) {
		char *newname = g_strconcat ("../", fname, NULL);
		file = fopen (newname, "r");
	}
	if (!file) {
		g_printerr ("Can't find vital file '%s'\n", fname);
		return;
	}
	while (!feof(file)) {
		unsigned char *p;
		fgets ((char *)buffer, sizeof (buffer)-1, file);
		for (p=buffer;*p;p++)
			if (*p=='0' && *(p+1)=='x') {
				GENERIC_TYPE *bt = g_new (GENERIC_TYPE,1);
				unsigned char *name, *pt;
				bt->opcode=strtol((char *)p+2,0,16);
				pt = buffer;
				while (*pt && *pt != '#') pt++;      /* # */
				while (*pt && !isspace(*pt))
					pt++;  /* define */
				while (*pt && isspace((unsigned char)*pt))
					pt++;  /* '   ' */
				while (*pt && *pt != '_') pt++;     /* BIFF_ */
				name = *pt?pt+1:pt;
				while (*pt && !isspace((unsigned char)*pt))
					pt++;
				bt->name = g_strndup ((gchar *)name, (unsigned)(pt - name));
				g_ptr_array_add (*types, bt);
				break;
			}
	}
	fclose (file);
}

static char const *
get_biff_opcode_name (unsigned opcode)
{
	int lp;
	if (!biff_types)
		read_types (BIFF_TYPES_FILE, &biff_types);
	/* Count backwars to give preference to non-filtered record types */
	for (lp=biff_types->len; --lp >= 0 ;) {
		GENERIC_TYPE *bt = g_ptr_array_index (biff_types, lp);
		if (bt->opcode == opcode)
			return bt->name;
	}
	return "Unknown";
}

static void
dump_biff_stream (GsfInput *stream)
{
	guint8 const *data;
	guint16 len, opcode;
	unsigned pos = gsf_input_tell (stream);

	while (NULL != (data = gsf_input_read (stream, 4, NULL))) {
		gboolean enable_dump = TRUE;

		opcode	= GSF_LE_GET_GUINT16 (data);
		len	= GSF_LE_GET_GUINT16 (data+2);

		if (len > 15000) {
			enable_dump = TRUE;
			g_warning ("Suspicious import of biff record > 15,000 (0x%x) for opcode 0x%hx",
				   len, opcode);
		} else if ((opcode & 0xff00) > 0x1000) {
			enable_dump = TRUE;
			g_warning ("Suspicious import of biff record with opcode 0x%hx",
				   opcode);
		}

		if (enable_dump)
			printf ("Opcode 0x%3hx : %15s, length 0x%hx (=%hd)\n",
				opcode, get_biff_opcode_name (opcode),
				len, len);

		if (len > 0) {
			data = gsf_input_read (stream, len, NULL);
			if (data == NULL)
				break;
			if (enable_dump)
				gsf_mem_dump (data, len);
		}
		pos = gsf_input_tell (stream);
	}
}
#endif /* DUMP_CONTENT */

static void
cb_print_property (char const *name, GsfDocProp const *prop)
{
	GValue const *val = gsf_doc_prop_get_val  (prop);
	char *tmp;

	if (gsf_doc_prop_get_link (prop) != NULL)
		g_print ("prop '%s' LINKED TO  -> '%s'\n",
			 name, gsf_doc_prop_get_link (prop));
	else
		g_print ("prop '%s'\n", name);

	if (VAL_IS_GSF_DOCPROP_VECTOR ((GValue *)val)) {
		GValueArray *va = gsf_value_get_docprop_varray (val);
		unsigned i;

		for (i = 0 ; i < va->n_values; i++) {
			tmp = g_strdup_value_contents (
				g_value_array_get_nth (va, i));
			g_print ("\t[%u] = %s\n", i, tmp);
			g_free (tmp);
		}
	} else {
		tmp = g_strdup_value_contents (val);
		printf ("\t= %s\n", tmp);
		g_free (tmp);
	}
}

static int
test (unsigned argc, char *argv[])
{
	static char const * const stream_names[] = {
		"Workbook",	"WORKBOOK",	"workbook",
		"Book",		"BOOK",		"book"
	};

	GsfInput  *input, *stream;
	GsfInfile *infile;
	GError    *err = NULL;
	unsigned i, j;

	for (i = 1 ; i < argc ; i++) {
		fprintf( stderr, "%s\n",argv[i]);

		input = gsf_input_mmap_new (argv[i], NULL);
		if (input == NULL)	/* Only report error if stdio fails too */
			input = gsf_input_stdio_new (argv[i], &err);
		if (input == NULL) {
			g_return_val_if_fail (err != NULL, 1);
			g_warning ("'%s' error: %s", argv[i], err->message);
			g_error_free (err);
			continue;
		}

		input = gsf_input_uncompress (input);

		infile = gsf_infile_msole_new (input, &err);

		if (infile == NULL) {

			g_return_val_if_fail (err != NULL, 1);

			g_warning ("'%s' Not an OLE file: %s", argv[i], err->message);
			g_error_free (err);

#ifdef DUMP_CONTENT
			dump_biff_stream (input);
#endif

			g_object_unref (G_OBJECT (input));
			continue;
		}
#if 0
		stream = gsf_infile_child_by_name (infile, "\01CompObj");
		if (stream != NULL) {
			gsf_off_t len = gsf_input_size (stream);
			guint8 const *data = gsf_input_read (stream, len, NULL);
			if (data != NULL)
				gsf_mem_dump (data, len);
			g_object_unref (G_OBJECT (stream));
		}
		return 0;
#endif

		stream = gsf_infile_child_by_name (infile, "\05SummaryInformation");
		if (stream != NULL) {
			GsfDocMetaData *meta_data = gsf_doc_meta_data_new ();

			puts ( "SummaryInfo");
			err = gsf_msole_metadata_read (stream, meta_data);
			if (err != NULL) {
				g_warning ("'%s' error: %s", argv[i], err->message);
				g_error_free (err);
				err = NULL;
			} else
				gsf_doc_meta_data_foreach (meta_data,
					(GHFunc) cb_print_property, NULL);

			g_object_unref (meta_data);
			g_object_unref (G_OBJECT (stream));
		}

		stream = gsf_infile_child_by_name (infile, "\05DocumentSummaryInformation");
		if (stream != NULL) {
			GsfDocMetaData *meta_data = gsf_doc_meta_data_new ();

			puts ( "DocSummaryInfo");
			err = gsf_msole_metadata_read (stream, meta_data);
			if (err != NULL) {
				g_warning ("'%s' error: %s", argv[i], err->message);
				g_error_free (err);
				err = NULL;
			} else
				gsf_doc_meta_data_foreach (meta_data,
					(GHFunc) cb_print_property, NULL);

			g_object_unref (meta_data);
			g_object_unref (G_OBJECT (stream));
		}

		for (j = 0 ; j < G_N_ELEMENTS (stream_names) ; j++) {
			stream = gsf_infile_child_by_name (infile, stream_names[j]);
			if (stream != NULL) {
				puts (j < 3 ? "Excel97" : "Excel95");
#ifdef DUMP_CONTENT
				dump_biff_stream (stream);
#endif
				g_object_unref (G_OBJECT (stream));
				break;
			}
		}

		g_object_unref (G_OBJECT (infile));
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
