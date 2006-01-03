/* GNOME thumbnailer for Office files
 * Copyright (C) 2005 Novell, Inc.
 *
 * Authors:
 *   Federico Mena-Quintero <federico@novell.com>
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
#include <stdlib.h>
#include <unistd.h>
#include <glib.h>
#include <gsf/gsf-input-memory.h>
#include <gsf/gsf-input-stdio.h>
#include <gsf/gsf-infile.h>
#include <gsf/gsf-infile-msole.h>
#include <gsf/gsf-meta-names.h>
#include <gsf/gsf-msole-utils.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-clip-data.h>

G_GNUC_NORETURN static void
show_error_string_and_exit (const char *str)
{
	fprintf (stderr, "error: %s\n", str);
	exit (EXIT_FAILURE);
}

G_GNUC_NORETURN static void
show_error_and_exit (GError *error)
{
	if (error)
		show_error_string_and_exit (error->message);
	else
		show_error_string_and_exit ("an error happened, and we didn't get a GError.  Exiting.");
}

static void
call_convert (const char *in_filename, const char *out_filename, int thumb_size)
{
	char *in_quote;
	char *out_quote;
	char *cmd_line;
	GError *error;
	gint exit_status;

	in_quote = g_shell_quote (in_filename);
	out_quote = g_shell_quote (out_filename);
	cmd_line = g_strdup_printf ("convert -resize %dx%d %s png:%s",
				    thumb_size, thumb_size,
				    in_quote,
				    out_quote);
	fprintf (stderr, "calling %s\n", cmd_line);
	g_free (in_quote);
	g_free (out_quote);

	error = NULL;
	if (!g_spawn_command_line_sync (cmd_line, NULL, NULL, &exit_status, &error))
		show_error_and_exit (error);

	g_free (cmd_line);
}

static void
write_thumbnail (const char *filename, gconstpointer data, gsize size, int thumb_size)
{
	char *tmp_name;
	int fd;
	FILE *file;

	tmp_name = g_strdup_printf ("%s.XXXXXX", filename);
	fd = g_mkstemp (tmp_name);
	if (fd == -1) {
		perror ("Could not create temporary file");
		exit (EXIT_FAILURE);
	}

	file = fdopen (fd, "w");
	if (!file) {
		show_error_string_and_exit ("Could not open temporary file for writing");
		exit (EXIT_FAILURE);
	}

	if (fwrite (data, 1, size, file) != size) {
		perror ("Could not write data to output file");
		exit (EXIT_FAILURE);
	}

	if (fclose (file) != 0) {
		perror ("Could not close oputput file");
		exit (EXIT_FAILURE);
	}

	call_convert (tmp_name, filename, thumb_size);
	unlink (tmp_name);
}

static void
read_thumbnail_and_write (const char *in_filename, const char *out_filename, int thumb_size)
{
	GsfInput *input;
	GsfInfile *infile;
	GError *error;
	GsfInput *summary_stream;
	GsfDocMetaData *meta_data;
	GsfDocProp *thumb_doc_prop;
	GValue const *thumb_value;
	GsfClipData *clip_data;
	GsfClipFormat clip_format;
	gconstpointer data;
	gsize size;

	input = gsf_input_mmap_new (in_filename, NULL);
	if (!input) {
		error = NULL;
		input = gsf_input_stdio_new (in_filename, &error);
		if (!input)
			show_error_and_exit (error);
	}

	input = gsf_input_uncompress (input);

	error = NULL;
	infile = gsf_infile_msole_new (input, &error);
	if (!infile)
		show_error_and_exit (error);

	summary_stream = gsf_infile_child_by_name (infile, "\05SummaryInformation");
	if (!summary_stream)
		show_error_string_and_exit ("Could not find the SummaryInformation stream");

	meta_data = gsf_doc_meta_data_new ();
	error = gsf_msole_metadata_read (summary_stream, meta_data);
	if (error)
		show_error_and_exit (error);

	thumb_doc_prop = gsf_doc_meta_data_lookup (meta_data, GSF_META_NAME_THUMBNAIL);
	if (!thumb_doc_prop)
		show_error_string_and_exit ("The metadata does not have a thumbnail property");

	thumb_value = gsf_doc_prop_get_val (thumb_doc_prop);
	if (!thumb_value)
		show_error_string_and_exit ("We got the thumbnail property, but it didn't have a value!?");

	clip_data = GSF_CLIP_DATA (g_value_get_object (thumb_value));

	clip_format = gsf_clip_data_get_format (clip_data);

	if (clip_format == GSF_CLIP_FORMAT_WINDOWS_CLIPBOARD) {
		GsfClipFormatWindows win_format;

		error = NULL;
		win_format = gsf_clip_data_get_windows_clipboard_format (clip_data, &error);
		if (win_format == GSF_CLIP_FORMAT_WINDOWS_ERROR)
			show_error_and_exit (error);
	}

	error = NULL;
	data = gsf_clip_data_peek_real_data (clip_data, &size, &error);

	if (!data)
		show_error_and_exit (error);

	write_thumbnail (out_filename, data, size, thumb_size);

	g_object_unref (clip_data);

	g_object_unref (meta_data);
	g_object_unref (summary_stream);
	g_object_unref (infile);
	g_object_unref (input);
}

/* Command-line options */
static int option_size = -1;
static char *option_input_filename = NULL;
static char *option_output_filename = NULL;

static GOptionEntry option_entries[] = {
	{ "input", 'i', 0, G_OPTION_ARG_FILENAME, &option_input_filename,
	  "Name of file for which to create a thumbnail",
	  "filename" },
	{ "output", 'o', 0, G_OPTION_ARG_FILENAME, &option_output_filename,
	  "Name of the file to put the thumbnail",
	  "filename" },
	{ "size", 's', 0, G_OPTION_ARG_INT, &option_size,
	  "Size of thumbnail in pixels; the thumbnail will be at most N*N pixels large",
	  "N" },
	{ NULL, 0, 0, 0, NULL, NULL, NULL }
};

int
main (int argc, char **argv)
{
	GOptionContext *option_ctx;

	option_ctx = g_option_context_new ("Options");
	g_option_context_add_main_entries (option_ctx, option_entries, NULL); /* FIXME: no translation domain */
	if (!g_option_context_parse (option_ctx, &argc, &argv, NULL)
	    || option_size == -1
	    || option_input_filename == NULL
	    || option_output_filename == NULL) {
		fprintf (stderr, "Invalid usage; type \"%s --help\" for instructions.  All the options must be used.\n", argv[0]);
		exit (EXIT_FAILURE);
	}

	gsf_init ();
	read_thumbnail_and_write (option_input_filename, option_output_filename, option_size);

	return 0;
}
