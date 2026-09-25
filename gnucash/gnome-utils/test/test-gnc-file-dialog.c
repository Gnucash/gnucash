/********************************************************************\
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:
 * Free Software Foundation           Voice:  +1-617-542-5942      *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-5942      *
 * Boston, MA  02110-1301,  USA                                      *
\********************************************************************/

#include <config.h>

#include <glib.h>

#include "gnc-file.h"

typedef struct
{
    GMainLoop *loop;
    GError *error;
    GFile *file;
    GListModel *files;
    gboolean multiple;
} FileDialogResult;

static void
file_dialog_finished (GObject *source, GAsyncResult *result, gpointer user_data)
{
    FileDialogResult *dialog_result = user_data;
    GncFileDialogRequest *request = GNC_FILE_DIALOG_REQUEST (source);

    if (dialog_result->multiple)
        dialog_result->files = gnc_file_dialog_request_finish_multiple (
            request, result, &dialog_result->error);
    else
        dialog_result->file = gnc_file_dialog_request_finish (
            request, result, &dialog_result->error);
    g_main_loop_quit (dialog_result->loop);
}

static void
file_dialog_result_clear (FileDialogResult *dialog_result)
{
    g_clear_error (&dialog_result->error);
    g_clear_object (&dialog_result->file);
    g_clear_object (&dialog_result->files);
    g_main_loop_unref (dialog_result->loop);
}

static void
test_open_request_rejects_save (void)
{
    FileDialogResult dialog_result = { 0 };
    GtkFileFilter *filter = gtk_file_filter_new ();
    GList *filters = g_list_append (NULL, filter);
    GncFileDialogRequest *request;

    gtk_file_filter_set_name (filter, "GnuCash data");
    gtk_file_filter_add_pattern (filter, "*.gnucash");
    request = gnc_file_dialog_request_new (NULL, NULL, filters, ".",
                                           GNC_FILE_DIALOG_OPEN);
    g_assert_nonnull (request);

    dialog_result.loop = g_main_loop_new (NULL, FALSE);
    gnc_file_dialog_request_save_async (request, NULL, file_dialog_finished,
                                        &dialog_result);
    g_main_loop_run (dialog_result.loop);

    g_assert_null (dialog_result.file);
    g_assert_error (dialog_result.error, G_IO_ERROR, G_IO_ERROR_INVALID_ARGUMENT);
    file_dialog_result_clear (&dialog_result);
    g_object_unref (request);
}

static void
test_save_request_rejects_open_multiple (void)
{
    FileDialogResult dialog_result = { 0 };
    GncFileDialogRequest *request = gnc_file_dialog_request_new (
        NULL, NULL, NULL, NULL, GNC_FILE_DIALOG_SAVE);

    g_assert_nonnull (request);
    dialog_result.loop = g_main_loop_new (NULL, FALSE);
    dialog_result.multiple = TRUE;
    gnc_file_dialog_request_open_multiple_async (request, NULL,
                                                  file_dialog_finished,
                                                  &dialog_result);
    g_main_loop_run (dialog_result.loop);

    g_assert_null (dialog_result.files);
    g_assert_error (dialog_result.error, G_IO_ERROR, G_IO_ERROR_INVALID_ARGUMENT);
    file_dialog_result_clear (&dialog_result);
    g_object_unref (request);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    g_test_add_func ("/gnome-utils/file-dialog/open-rejects-save",
                     test_open_request_rejects_save);
    g_test_add_func ("/gnome-utils/file-dialog/save-rejects-open-multiple",
                     test_save_request_rejects_open_multiple);

    return g_test_run ();
}
