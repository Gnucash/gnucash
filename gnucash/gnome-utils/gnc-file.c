/********************************************************************\
 * FileDialog.c -- file-handling utility dialogs for gnucash.       *
 *                                                                  *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998, 1999, 2000 Linas Vepstas                     *
 *                                                                  *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include <config.h>

#include <stdbool.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <errno.h>
#include <string.h>

#include "dialog-utils.h"
#include "assistant-xml-encoding.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "Account.h"
#include "gnc-file.h"
#include "gnc-features.h"
#include "gnc-filepath-utils.h"
#include "gnc-string-utils.h"
#include "gnc-gui-query.h"
#include "gnc-gnome-utils.h"
#include "gnc-hooks.h"
#include "gnc-keyring.h"
#include "gnc-splash.h"
#include "gnc-ui.h"
#include "gnc-ui-balances.h"
#include "gnc-ui-util.h"
#include "gnc-uri-utils.h"
#include "gnc-window.h"
#include "gnc-plugin-file-history.h"
#include "qof.h"
#include "Scrub.h"
#include "ScrubBudget.h"
#include "TransLog.h"
#include "gnc-session.h"
#include "gnc-state.h"
#include "gnc-autosave.h"
#include <gnc-sx-instance-model.h>
#include <SX-book.h>

/** GLOBALS *********************************************************/
/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static GNCShutdownCB shutdown_cb = NULL;
static gint save_in_progress = 0;

typedef bool (*CharToBool)(const char*);

static bool datafile_filter (const GtkFileFilterInfo* info, CharToBool checker)
{
    return info && info->filename && checker (info->filename);
}

GList*
gnc_file_chooser_get_datafile_filters ()
{
    /* Translators: *.gnucash.*.gnucash, *.xac.*.xac are file patterns
       and must not be translated*/
    const char* datafiles = N_("Datafiles only (*.gnucash, *.xac)");
    const char* backups = N_("Backups only (*.gnucash.*.gnucash, *.xac.*.xac)");
    GList* rv = NULL;

    GtkFileFilter *filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (filter, _(datafiles));
    gtk_file_filter_add_custom (filter, GTK_FILE_FILTER_FILENAME,
                                (GtkFileFilterFunc)datafile_filter,
                                gnc_filename_is_datafile, NULL);
    rv = g_list_prepend (rv, filter);

    filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (filter, _(backups));
    gtk_file_filter_add_custom (filter, GTK_FILE_FILTER_FILENAME,
                                (GtkFileFilterFunc)datafile_filter,
                                gnc_filename_is_backup, NULL);
    rv = g_list_prepend (rv, filter);

    return g_list_reverse (rv);
}

void
gnc_file_chooser_add_filters (GtkFileChooser* file_box, GList *filters)
{
    g_return_if_fail (GTK_IS_WIDGET (file_box));
    if (filters == NULL) return;

    /* The caller transfers the list and its floating GtkFileFilter objects.
     * GtkFileChooser sinks each filter; only the list container is freed here. */
    for (GList* node = filters; node; node = node->next)
        gtk_file_chooser_add_filter (file_box, GTK_FILE_FILTER (node->data));

    GtkFileFilter* all_filter = gtk_file_filter_new();
    gtk_file_filter_set_name (all_filter, _("All files"));
    gtk_file_filter_add_pattern (all_filter, "*");
    gtk_file_chooser_add_filter (file_box, all_filter);

    /* preselect the first filter */
    gtk_file_chooser_set_filter (file_box, filters->data);
    g_list_free (filters);
}

static GtkWidget *
gnc_file_dialog_create (GtkWindow *parent,
                     const char * title,
                     GList * filters,
                     const char * starting_dir,
                     GNCFileDialogType type,
                     gboolean multi
                     )
{
    GtkWidget *file_box;
    gchar * okbutton = NULL;
    const gchar *ok_icon = NULL;
    GtkFileChooserAction action = GTK_FILE_CHOOSER_ACTION_OPEN;

    ENTER(" ");

    switch (type)
    {
    case GNC_FILE_DIALOG_OPEN:
        action = GTK_FILE_CHOOSER_ACTION_OPEN;
        okbutton = _("_Open");
        if (title == NULL)
            title = _("Open");
        break;
    case GNC_FILE_DIALOG_IMPORT:
        action = GTK_FILE_CHOOSER_ACTION_OPEN;
        okbutton = _("_Import");
        if (title == NULL)
            title = _("Import");
        break;
    case GNC_FILE_DIALOG_SAVE:
        action = GTK_FILE_CHOOSER_ACTION_SAVE;
        okbutton = _("_Save");
        if (title == NULL)
            title = _("Save");
        break;
    case GNC_FILE_DIALOG_EXPORT:
        action = GTK_FILE_CHOOSER_ACTION_SAVE;
        okbutton = _("_Export");
        ok_icon = "go-next";
        if (title == NULL)
            title = _("Export");
        break;

    }

    file_box = gtk_file_chooser_dialog_new(
                   title,
                   parent,
                   action,
                   _("_Cancel"), GTK_RESPONSE_CANCEL,
                   NULL);
    if (multi)
        gtk_file_chooser_set_select_multiple (GTK_FILE_CHOOSER (file_box), TRUE);

    if (ok_icon)
        gnc_gtk_dialog_add_button(file_box, okbutton, ok_icon, GTK_RESPONSE_ACCEPT);
    else
        gtk_dialog_add_button(GTK_DIALOG(file_box),
                              okbutton, GTK_RESPONSE_ACCEPT);

    if (starting_dir)
        gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER (file_box),
                                            starting_dir);

    gtk_window_set_modal(GTK_WINDOW(file_box), TRUE);

    if (filters != NULL)
        gnc_file_chooser_add_filters (GTK_FILE_CHOOSER (file_box), filters);

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(file_box), "gnc-id-file");

    LEAVE ("file chooser");
    return file_box;
}

static GSList *
gnc_file_dialog_get_selection (GtkWidget *file_box, gboolean multi,
                               gint response)
{
    char *file_name = NULL;
    GSList* file_name_list = NULL;
    if (response == GTK_RESPONSE_ACCEPT)
    {
        if (multi)
        {
            file_name_list = gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (file_box));
        }
        else
        {
            /* look for constructs like postgres://foo */
            file_name = gtk_file_chooser_get_uri(GTK_FILE_CHOOSER (file_box));
            if (file_name != NULL)
            {
                if (strstr (file_name, "file://") == file_name)
                {
                    g_free (file_name);
                    /* nope, a local file name */
                    file_name = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER (file_box));
                }
                file_name_list = g_slist_append (file_name_list, file_name);
            }
        }
    }
    return file_name_list;
}

typedef struct
{
    GNCFileDialogAsyncCallback callback;
    gpointer user_data;
    GDestroyNotify destroy_notify;
    GtkWidget *dialog; /* retained until gnc_gui_query_bind_dialog_response completes */
    gboolean multi;
    gulong response_id;
    gboolean has_owner;
    GSList *filenames;
} GNCFileDialogAsync;

static void
gnc_file_dialog_async_capture (GtkDialog *dialog, gint response,
                               gpointer user_data)
{
    GNCFileDialogAsync *state = user_data;
    if (state->response_id &&
        g_signal_handler_is_connected (dialog, state->response_id))
        g_signal_handler_disconnect (dialog, state->response_id);
    state->response_id = 0;
    if (response == GTK_RESPONSE_ACCEPT)
        state->filenames = gnc_file_dialog_get_selection (GTK_WIDGET (dialog),
                                                          state->multi, response);
}

static void
gnc_file_dialog_async_complete (GtkWindow *parent, gint response,
                                gpointer user_data)
{
    GNCFileDialogAsync *state = user_data;
    if (state->response_id &&
        g_signal_handler_is_connected (state->dialog, state->response_id))
        g_signal_handler_disconnect (state->dialog, state->response_id);
    if (response != GTK_RESPONSE_ACCEPT || (state->has_owner && !parent))
    {
        g_slist_free_full (state->filenames, g_free);
        state->filenames = NULL;
    }
    if (state->callback)
        state->callback (state->filenames, state->user_data);
    else
        g_slist_free_full (state->filenames, g_free);
    if (state->destroy_notify)
        state->destroy_notify (state->user_data);
    g_object_unref (state->dialog);
    g_free (state);
}

void
gnc_file_dialog_async (GtkWindow *parent, const char *title, GList *filters,
                       const char *starting_dir, GNCFileDialogType type,
                       gboolean multi, GNCFileDialogAsyncCallback callback,
                       gpointer user_data, GDestroyNotify destroy_notify)
{
    GtkWidget *dialog = gnc_file_dialog_create (parent, title, filters,
                                                starting_dir, type, multi);
    gtk_window_set_destroy_with_parent (GTK_WINDOW (dialog), TRUE);
    GNCFileDialogAsync *state = g_new0 (GNCFileDialogAsync, 1);
    state->callback = callback;
    state->user_data = user_data;
    state->destroy_notify = destroy_notify;
    state->dialog = dialog;
    state->multi = multi;
    state->has_owner = parent != NULL;
    g_object_ref (dialog);
    state->response_id = g_signal_connect (dialog, "response",
                      G_CALLBACK (gnc_file_dialog_async_capture), state);
    gnc_dialog_run_async (GTK_DIALOG (dialog), NULL,
                          gnc_file_dialog_async_complete, state);
}

typedef struct
{
    gchar *filename;
    GncGuiQueryResponseCallback completed;
    gpointer user_data;
    gboolean has_parent;
    QofSession *session;
    QofBook *book;
} SessionHistoryRequest;

static void
session_error_ignored ([[maybe_unused]] GtkWindow *parent,
                        [[maybe_unused]] gint response, [[maybe_unused]] gpointer data)
{
}

static void
session_history_closed (GtkWindow *parent, gint response, gpointer user_data)
{
    SessionHistoryRequest *request = user_data;
    gboolean current = !request->session || (request->book && gnc_current_session_exist () &&
        gnc_get_current_session () == request->session &&
        qof_session_get_book (request->session) == request->book);
    if ((!request->has_parent || parent) && current && response == GTK_RESPONSE_YES &&
        gnc_history_test_for_file (request->filename))
        gnc_history_remove_file (request->filename);
    if (request->book)
        g_object_remove_weak_pointer (G_OBJECT (request->book), (gpointer *)&request->book);
    GncGuiQueryResponseCallback completed = request->completed;
    gpointer data = request->user_data;
    g_free (request->filename);
    g_free (request);
    if (completed)
        completed (parent, response, data);
}

static void
show_session_error_full (GtkWindow *parent,
                    QofBackendError io_error,
                    const char *newfile,
                    GNCFileDialogType type,
                         GncGuiQueryResponseCallback completed,
                         gpointer user_data)
{
    GtkWidget *dialog;
    const char *fmt, *label;
    gchar *displayname;

    if (NULL == newfile)
    {
        displayname = g_strdup(_("(null)"));
    }
    else if (!gnc_uri_targets_local_fs (newfile)) /* Hide the db password in error messages */
        displayname = gnc_uri_normalize_uri ( newfile, FALSE);
    else
    {
        /* Strip the protocol from the file name and ensure absolute filename. */
        char *uri = gnc_uri_normalize_uri(newfile, FALSE);
        displayname = gnc_uri_get_path(uri);
        g_free(uri);
    }

    switch (io_error)
    {
    case ERR_BACKEND_NO_ERR:
        break;

    case ERR_BACKEND_NO_HANDLER:
        fmt = _("No suitable backend was found for %s.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_BACKEND_NO_BACKEND:
        fmt = _("The URL %s is not supported by this version of GnuCash.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_BACKEND_BAD_URL:
        fmt = _("Can't parse the URL %s.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_BACKEND_CANT_CONNECT:
        fmt = _("Can't connect to %s. "
                "The host, username or password were incorrect.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_BACKEND_CONN_LOST:
        fmt = _("Can't connect to %s. "
                "Connection was lost, unable to send data.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_BACKEND_TOO_NEW:
        fmt = _("This file/URL appears to be from a newer version "
                "of GnuCash. You must upgrade your version of GnuCash "
                "to work with this data.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, "%s", fmt);
        break;

    case ERR_BACKEND_NO_SUCH_DB:
        fmt = _("The database %s doesn't seem to exist. "
                "Do you want to create it?");
        gnc_verify_dialog_async (parent, TRUE, completed ? completed : session_error_ignored,
                                 user_data, fmt, displayname);
        break;

    case ERR_BACKEND_LOCKED:
        switch (type)
        {
        case GNC_FILE_DIALOG_OPEN:
        default:
            label = _("Open");
            fmt = _("GnuCash could not obtain the lock for %s. "
                    "That database may be in use by another user, "
                    "in which case you should not open the database. "
                    "Do you want to proceed with opening the database?");
            break;

        case GNC_FILE_DIALOG_IMPORT:
            label = _("Import");
            fmt = _("GnuCash could not obtain the lock for %s. "
                    "That database may be in use by another user, "
                    "in which case you should not import the database. "
                    "Do you want to proceed with importing the database?");
            break;

        case GNC_FILE_DIALOG_SAVE:
            label = _("Save");
            fmt = _("GnuCash could not obtain the lock for %s. "
                    "That database may be in use by another user, "
                    "in which case you should not save the database. "
                    "Do you want to proceed with saving the database?");
            break;

        case GNC_FILE_DIALOG_EXPORT:
            label = _("Export");
            fmt = _("GnuCash could not obtain the lock for %s. "
                    "That database may be in use by another user, "
                    "in which case you should not export the database. "
                    "Do you want to proceed with exporting the database?");
            break;
        }

        dialog = gtk_message_dialog_new(parent,
                                        GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_QUESTION,
                                        GTK_BUTTONS_NONE,
                                        fmt,
                                        displayname);
        gtk_dialog_add_buttons(GTK_DIALOG(dialog),
                               _("_Cancel"), GTK_RESPONSE_CANCEL,
                               label, GTK_RESPONSE_YES,
                               NULL);
        if (!parent)
            gtk_window_set_skip_taskbar_hint(GTK_WINDOW(dialog), FALSE);
        gnc_dialog_run_async (GTK_DIALOG (dialog), NULL,
            completed ? completed : session_error_ignored, user_data);
        break;

    case ERR_BACKEND_READONLY:
        fmt = _("GnuCash could not write to %s. "
                "That database may be on a read-only file system, "
                "you may not have write permission for the directory "
                "or your anti-virus software is preventing this action.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_BACKEND_DATA_CORRUPT:
        fmt = _("The file/URL %s "
                "does not contain GnuCash data or the data is corrupt.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_BACKEND_SERVER_ERR:
        fmt = _("The server at URL %s "
                "experienced an error or encountered bad or corrupt data.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_BACKEND_PERM:
        fmt = _("You do not have permission to access %s.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_BACKEND_MISC:
        fmt = _("An error occurred while processing %s.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_FILEIO_FILE_BAD_READ:
        fmt = _("There was an error reading the file. "
                "Do you want to continue?");
        gnc_verify_dialog_async (parent, TRUE, completed ? completed : session_error_ignored,
                                 user_data, "%s", fmt);
        break;

    case ERR_FILEIO_PARSE_ERROR:
        fmt = _("There was an error parsing the file %s.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_FILEIO_FILE_EMPTY:
        fmt = _("The file %s is empty.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_FILEIO_FILE_NOT_FOUND:
        if (type == GNC_FILE_DIALOG_SAVE)
        {
        }
        else
        {
            if (gnc_history_test_for_file (displayname))
            {
                fmt = _("The file/URI %s could not be found.\n\nThe file is in the history list, do you want to remove it?");
                SessionHistoryRequest *request = g_new0 (SessionHistoryRequest, 1);
                request->filename = g_strdup (displayname);
                request->completed = completed;
                request->user_data = user_data;
                request->has_parent = parent != NULL;
                if (gnc_current_session_exist ())
                {
                    request->session = gnc_get_current_session ();
                    request->book = qof_session_get_book (request->session);
                    g_object_add_weak_pointer (G_OBJECT (request->book), (gpointer *)&request->book);
                }
                gnc_verify_dialog_async (parent, FALSE, session_history_closed,
                                          request, fmt, displayname);
            }
            else
            {
                fmt = _("The file/URI %s could not be found.");
                gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
            }
        }
        break;

    case ERR_FILEIO_FILE_TOO_OLD:
        fmt = _("This file is from an older version of GnuCash. "
                "Do you want to continue?");
        gnc_verify_dialog_async (parent, TRUE, completed ? completed : session_error_ignored,
                                 user_data, "%s", fmt);
        break;

    case ERR_FILEIO_UNKNOWN_FILE_TYPE:
        fmt = _("The file type of file %s is unknown.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_FILEIO_BACKUP_ERROR:
        fmt = _("Could not make a backup of the file %s");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_FILEIO_WRITE_ERROR:
        fmt = _("Could not write to file %s. Check that you have "
                "permission to write to this file and that "
                "there is sufficient space to create it.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_FILEIO_FILE_EACCES:
        fmt = _("No read permission to read from file %s.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, displayname);
        break;

    case ERR_FILEIO_RESERVED_WRITE:
        /* Translators: the first %s is a path in the filesystem,
           the second %s is PACKAGE_NAME, which by default is "GnuCash" */
        fmt = _("You attempted to save in\n%s\nor a subdirectory thereof. "
                "This is not allowed as %s reserves that directory for internal use.\n\n"
                "Please try again in a different directory.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, gnc_userdata_dir(), PACKAGE_NAME);
        break;

    case ERR_SQL_DB_TOO_OLD:
        fmt = _("This database is from an older version of GnuCash. "
                "Select OK to upgrade it to the current version, Cancel "
                "to mark it read-only.");

        gnc_ok_cancel_dialog_async (parent, GTK_RESPONSE_CANCEL,
            completed ? completed : session_error_ignored, user_data, "%s", fmt);
        break;

    case ERR_SQL_DB_TOO_NEW:
        fmt = _("This database is from a newer version of GnuCash. "
                "This version can read it, but cannot safely save to it. "
                "It will be marked read-only until you do File->Save As, "
                "but data may be lost in writing to the old version.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_WARNING,
                                            completed, user_data, "%s", fmt);
        break;

    case ERR_SQL_DB_BUSY:
        fmt = _("The SQL database is in use by other users, "
                "and the upgrade cannot be performed until they logoff. "
                "If there are currently no other users, consult the "
                "documentation to learn how to clear out dangling login "
                "sessions.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, "%s", fmt);
        break;

    case ERR_SQL_BAD_DBI:

        fmt = _("The library \"libdbi\" installed on your system doesn't correctly "
                "store large numbers. This means GnuCash cannot use SQL databases "
                "correctly. Gnucash will not open or save to SQL databases until this is "
                "fixed by installing a different version of \"libdbi\". Please see "
                "https://bugs.gnucash.org/show_bug.cgi?id=611936 for more "
                "information.");

        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, "%s", fmt);
        break;

    case ERR_SQL_DBI_UNTESTABLE:

        fmt = _("GnuCash could not complete a critical test for the presence of "
                "a bug in the \"libdbi\" library. This may be caused by a "
                "permissions misconfiguration of your SQL database. Please see "
                "https://bugs.gnucash.org/show_bug.cgi?id=645216 for more "
                "information.");

        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, "%s", fmt);
        break;

    case ERR_FILEIO_FILE_UPGRADE:
        fmt = _("This file is from an older version of GnuCash and will be "
                "upgraded when saved by this version. You will not be able "
                "to read the saved file from the older version of Gnucash "
                "(it will report an \"error parsing the file\"). If you wish "
                "to preserve the old version, exit without saving.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_WARNING,
                                            completed, user_data, "%s", fmt);
        break;

    default:
        PERR("FIXME: Unhandled error %d", io_error);
        fmt = _("An unknown I/O error (%d) occurred.");
        gnc_message_dialog_async_response (parent, GTK_MESSAGE_ERROR,
                                            completed, user_data, fmt, io_error);
        break;
    }

    g_free (displayname);
    if (completed && (io_error == ERR_BACKEND_NO_ERR ||
        (io_error == ERR_FILEIO_FILE_NOT_FOUND && type == GNC_FILE_DIALOG_SAVE)))
        completed (parent, GTK_RESPONSE_CLOSE, user_data);
}

void
gnc_file_show_session_error_async (GtkWindow *parent, QofBackendError io_error,
                                   const char *newfile, GNCFileDialogType type,
                                   GncGuiQueryResponseCallback completed, gpointer user_data)
{
    show_session_error_full (parent, io_error, newfile, type, completed, user_data);
}

static void
gnc_add_history (QofSession * session)
{
    const gchar *url;
    char *file;

    if (!session) return;

    url = qof_session_get_url ( session );
    if ( !strlen (url) )
        return;

    if (gnc_uri_targets_local_fs (url))
        file = gnc_uri_get_path ( url );
    else
        file = gnc_uri_normalize_uri ( url, FALSE ); /* Note that the password is not saved in history ! */

    gnc_history_add_file (file);
    g_free (file);
}

static void
gnc_book_opened (void)
{
    gnc_hook_run(HOOK_BOOK_OPENED, gnc_get_current_session());
}

static void
file_new_after_save (gboolean proceed, gpointer user_data)
{
    GtkWindow *parent = user_data;
    QofSession *session;

    if (!proceed || gnc_file_save_in_progress ())
    {
        g_clear_object (&parent);
        return;
    }

    if (gnc_current_session_exist())
    {
        session = gnc_get_current_session ();

        /* close any ongoing file sessions, and free the accounts.
         * disable events so we don't get spammed by redraws. */
        qof_event_suspend ();

        gnc_hook_run(HOOK_BOOK_CLOSED, session);

        gnc_close_gui_component_by_session (session);
        gnc_state_save (session);
        gnc_clear_current_session();
        qof_event_resume ();
    }

    /* start a new book */
    gnc_get_current_session ();

    gnc_hook_run(HOOK_NEW_BOOK, NULL);

    gnc_gui_refresh_all ();

    /* Call this after re-enabling events. */
    gnc_book_opened ();
    g_clear_object (&parent);
}

void
gnc_file_new (GtkWindow *parent)
{
    if (gnc_file_save_in_progress ())
        return;
    gnc_file_query_save_async (parent, TRUE, file_new_after_save,
                               parent ? g_object_ref (parent) : NULL);
}

static char*
get_account_sep_warning (QofBook *book)
{
    const char *sep = gnc_get_account_separator_string ();
    GList *violation_accts = gnc_account_list_name_violations (book, sep);
    if (!violation_accts)
        return NULL;

    gchar *rv = gnc_account_name_violations_errmsg (sep, violation_accts);
    g_list_free_full (violation_accts, g_free);
    return rv;
}

/* private utilities for file open; done in two stages */

#define RESPONSE_NEW 1
#define RESPONSE_OPEN 2
#define RESPONSE_QUIT 3
#define RESPONSE_READONLY 4
#define RESPONSE_FILE 5

/* This function is called after loading datafile. It's meant to
   collect all scrubbing routines. */
static void
run_post_load_scrubs (GtkWindow *parent, QofBook *book)
{
    const char *budget_warning =
        _("This book has budgets. The internal representation of "
          "budget amounts no longer depends on the Reverse Balanced "
          "Accounts preference. Please review the budgets and amend "
          "signs if necessary.");

    GList *infos = NULL;

    qof_event_suspend();

    /* If feature GNC_FEATURE_BUDGET_UNREVERSED is not set, and there
       are budgets, fix signs */
    if (gnc_maybe_scrub_all_budget_signs (book))
        infos = g_list_prepend (infos, g_strdup (budget_warning));

    // Fix account color slots being set to 'Not Set', should run once on a book
    xaccAccountScrubColorNotSet (book);

    /* Check for account names that may contain the current separator character
     * and inform the user if there are any */
    char *sep_warning = get_account_sep_warning (book);
    if (sep_warning)
        infos = g_list_prepend (infos, sep_warning);

    qof_event_resume();

    if (!infos)
        return;

    const char *header = N_("The following are noted in this file:");
    infos = g_list_reverse (infos);
    infos = g_list_prepend (infos, g_strdup (_(header)));
    char *final = gnc_g_list_stringjoin (infos, "\n\n• ");
    gnc_info_dialog_async (parent, "%s", final);

    g_free (final);
    g_list_free_full (infos, g_free);
}

/* A suspended file command owns its strings and destination session, and
 * validates the source book before every continuation. Engine event suspension
 * covers backend/commit work only, never the time spent waiting for a dialog. */
typedef struct
{
    GtkWindow *parent;
    gboolean parent_destroyed;
    QofSession *original;
    QofBook *book;
    QofSession *destination;
    gchar *uri;
    gboolean readonly;
    QofBackendError error;
    GNCFileSaveCallback completed;
    gpointer user_data;
} FileOpenRequest;

static void file_open_begin (FileOpenRequest *request, SessionOpenMode mode);
static void file_open_load (FileOpenRequest *request);
static void file_open_choose (FileOpenRequest *request, const gchar *directory);
static void file_export_write (FileOpenRequest *request);

static void
file_open_parent_destroyed ([[maybe_unused]] GtkWidget *parent,
                            FileOpenRequest *request)
{
    request->parent_destroyed = TRUE;
}

static gboolean
file_open_current (FileOpenRequest *request)
{
    return !request->parent_destroyed && request->book &&
        qof_book_is_open (request->book) && !qof_book_shutting_down (request->book) &&
        gnc_current_session_exist () && gnc_get_current_session () == request->original &&
        qof_session_get_book (request->original) == request->book;
}

static void
file_open_finish (FileOpenRequest *request, gboolean succeeded)
{
    if (request->destination)
    {
        xaccLogDisable ();
        qof_session_destroy (request->destination);
        xaccLogEnable ();
    }
    if (request->book)
    {
        if (g_object_get_data (G_OBJECT (request->book), "gnc-file-open-pending") == request)
            g_object_set_data (G_OBJECT (request->book), "gnc-file-open-pending", NULL);
        g_object_remove_weak_pointer (G_OBJECT (request->book), (gpointer *)&request->book);
    }
    if (request->parent)
        g_signal_handlers_disconnect_by_data (request->parent, request);
    g_clear_object (&request->parent);
    GNCFileSaveCallback completed = request->completed;
    gpointer user_data = request->user_data;
    g_free (request->uri);
    g_free (request);
    if (completed)
        completed (succeeded, user_data);
}

static FileOpenRequest *
file_open_request_new (GtkWindow *parent, GNCFileSaveCallback completed,
                        gpointer user_data)
{
    QofSession *session = gnc_get_current_session ();
    QofBook *book = qof_session_get_book (session);
    if (gnc_file_save_in_progress () || gnc_gui_session_operation_pending () ||
        g_object_get_data (G_OBJECT (book), "gnc-file-open-pending") ||
        (parent && gtk_widget_in_destruction (GTK_WIDGET (parent))))
    {
        if (completed)
            completed (FALSE, user_data);
        return NULL;
    }
    FileOpenRequest *request = g_new0 (FileOpenRequest, 1);
    request->original = session;
    request->book = book;
    request->completed = completed;
    request->user_data = user_data;
    g_object_add_weak_pointer (G_OBJECT (book), (gpointer *)&request->book);
    g_object_set_data (G_OBJECT (book), "gnc-file-open-pending", request);
    if (parent)
    {
        request->parent = g_object_ref (parent);
        g_signal_connect (parent, "destroy", G_CALLBACK (file_open_parent_destroyed), request);
    }
    return request;
}

static void
file_open_error_closed ([[maybe_unused]] GtkWindow *parent,
                         [[maybe_unused]] gint response, gpointer user_data)
{
    file_open_finish (user_data, FALSE);
}

static void
file_open_commit (FileOpenRequest *request)
{
    if (!file_open_current (request))
    {
        file_open_finish (request, FALSE);
        return;
    }
    QofBook *book = qof_session_get_book (request->destination);
    gchar *unknown = gnc_features_test_unknown (book);
    if (unknown)
    {
        gnc_message_dialog_async_response (request->parent, GTK_MESSAGE_ERROR,
            file_open_error_closed, request, "%s", unknown);
        g_free (unknown);
        return;
    }
    Account *root = gnc_book_get_root_account (book);
    if (!root)
    {
        show_session_error_full (request->parent, ERR_BACKEND_MISC, request->uri,
                                GNC_FILE_DIALOG_OPEN, file_open_error_closed, request);
        return;
    }
    Account *template_root = gnc_book_get_template_root (book);
    if (template_root)
    {
        GList *children = gnc_account_get_descendants (template_root);
        for (GList *node = children; node; node = node->next)
        {
            GList *splits = xaccAccountGetSplitList (GNC_ACCOUNT (node->data));
            g_list_foreach (splits, (GFunc)gnc_sx_scrub_split_numerics, NULL);
            g_list_free (splits);
        }
        g_list_free (children);
    }
    if (!file_open_current (request))
    {
        file_open_finish (request, FALSE);
        return;
    }
    qof_event_suspend ();
    gnc_hook_run (HOOK_BOOK_CLOSED, request->original);
    /* Hooks may dispatch synchronous destruction. Validate the identity again
     * before closing components or exchanging sessions. */
    if (!file_open_current (request))
    {
        qof_event_resume ();
        file_open_finish (request, FALSE);
        return;
    }
    gnc_close_gui_component_by_session (request->original);
    if (!file_open_current (request))
    {
        qof_event_resume ();
        file_open_finish (request, FALSE);
        return;
    }
    gnc_state_save (request->original);
    QofSession *opened = request->destination;
    gnc_exchange_current_session (opened);
    request->destination = NULL;
    gnc_add_history (opened);
    /* Remove the guard while the original book still exists. */
    g_object_set_data (G_OBJECT (request->book), "gnc-file-open-pending", NULL);
    g_object_remove_weak_pointer (G_OBJECT (request->book), (gpointer *)&request->book);
    request->book = NULL;
    xaccLogDisable ();
    qof_session_destroy (request->original);
    xaccLogEnable ();
    qof_event_resume ();
    gnc_gui_refresh_all ();
    gnc_book_opened ();
    if (gnc_current_session_exist () && gnc_get_current_session () == opened)
        run_post_load_scrubs (request->parent_destroyed ? NULL : request->parent, book);
    file_open_finish (request, TRUE);
}

static void
file_open_upgrade_closed ([[maybe_unused]] GtkWindow *parent, gint response,
                           gpointer user_data)
{
    FileOpenRequest *request = user_data;
    if (!file_open_current (request) || response == GTK_RESPONSE_CANCEL)
        file_open_finish (request, FALSE);
    else if (request->error != ERR_BACKEND_NO_ERR)
    {
        qof_book_mark_readonly (qof_session_get_book (request->destination));
        file_open_commit (request);
    }
    else
        file_open_commit (request);
}

static void
file_open_loaded_error (GtkWindow *parent, gint response, gpointer user_data)
{
    FileOpenRequest *request = user_data;
    if (!file_open_current (request) || (request->parent && !parent))
    {
        file_open_finish (request, FALSE);
        return;
    }
    QofBackendError error = request->error;
    if (error == ERR_SQL_DB_TOO_OLD && response == GTK_RESPONSE_OK)
    {
        gnc_set_busy_cursor (NULL, TRUE);
        qof_session_safe_save (request->destination, gnc_window_show_progress);
        gnc_unset_busy_cursor (NULL);
        request->error = qof_session_get_error (request->destination);
        show_session_error_full (request->parent, request->error, request->uri,
            GNC_FILE_DIALOG_SAVE, file_open_upgrade_closed, request);
    }
    else if (error == ERR_SQL_DB_TOO_OLD || error == ERR_SQL_DB_TOO_NEW)
    {
        qof_book_mark_readonly (qof_session_get_book (request->destination));
        file_open_commit (request);
    }
    else if (error == ERR_BACKEND_NO_ERR || error == ERR_FILEIO_FILE_UPGRADE ||
             ((error == ERR_FILEIO_FILE_BAD_READ || error == ERR_FILEIO_FILE_TOO_OLD) &&
               response == GTK_RESPONSE_YES))
        file_open_commit (request);
    else
        file_open_finish (request, FALSE);
}

static void
file_open_converted (gboolean converted, gpointer user_data)
{
    FileOpenRequest *request = user_data;
    if (!converted || !file_open_current (request))
        file_open_finish (request, FALSE);
    else
        file_open_load (request);
}

static void
file_open_load (FileOpenRequest *request)
{
    if (!file_open_current (request))
    {
        file_open_finish (request, FALSE);
        return;
    }
    qof_event_suspend ();
    gnc_set_busy_cursor (NULL, TRUE);
    xaccLogDisable ();
    gnc_window_show_progress (_("Loading user data…"), 0.0);
    qof_session_load (request->destination, gnc_window_show_progress);
    gnc_window_show_progress (NULL, -1.0);
    xaccLogEnable ();
    gnc_unset_busy_cursor (NULL);
    qof_event_resume ();
    if (!file_open_current (request))
    {
        file_open_finish (request, FALSE);
        return;
    }
    request->error = qof_session_pop_error (request->destination);
    if (request->error == ERR_FILEIO_NO_ENCODING)
    {
        gnc_xml_convert_single_file_async (request->parent, request->uri,
                                            file_open_converted, request);
        return;
    }
    if (request->readonly)
        qof_book_mark_readonly (qof_session_get_book (request->destination));
    show_session_error_full (request->parent, request->error, request->uri,
        GNC_FILE_DIALOG_OPEN, file_open_loaded_error, request);
}

static void
file_open_choice (GtkWindow *parent, gint response, gpointer user_data)
{
    FileOpenRequest *request = user_data;
    if (!file_open_current (request) || (request->parent && !parent))
    {
        file_open_finish (request, FALSE);
        return;
    }
    if (response == RESPONSE_READONLY)
    {
        request->readonly = TRUE;
        file_open_begin (request, SESSION_READ_ONLY);
    }
    else if (response == RESPONSE_OPEN)
        file_open_begin (request, SESSION_BREAK_LOCK);
    else if (response == GTK_RESPONSE_YES)
        file_open_begin (request, SESSION_NEW_STORE);
    else if (response == RESPONSE_FILE)
        file_open_choose (request, NULL);
    else if (response == RESPONSE_NEW)
    {
        GtkWindow *held_parent = parent ? g_object_ref (parent) : NULL;
        file_open_finish (request, FALSE);
        gnc_file_new (held_parent);
        g_clear_object (&held_parent);
    }
    else if (response == RESPONSE_QUIT)
    {
        file_open_finish (request, FALSE);
        if (shutdown_cb) shutdown_cb (0);
    }
    else
        file_open_finish (request, FALSE);
}

static void
file_open_bad_url_closed (GtkWindow *parent, [[maybe_unused]] gint response,
                           gpointer user_data)
{
    FileOpenRequest *request = user_data;
    if (!file_open_current (request) || (request->parent && !parent))
        file_open_finish (request, FALSE);
    else
        file_open_choose (request, NULL);
}

static void
file_open_begin (FileOpenRequest *request, SessionOpenMode mode)
{
    if (!file_open_current (request))
    {
        file_open_finish (request, FALSE);
        return;
    }
    if (!request->destination)
        request->destination = qof_session_new (qof_book_new ());
    qof_session_begin (request->destination, request->uri, mode);
    QofBackendError error = qof_session_get_error (request->destination);
    if (error == ERR_BACKEND_NO_ERR)
    {
        file_open_load (request);
        return;
    }
    if (mode == SESSION_NORMAL_OPEN &&
        (error == ERR_BACKEND_LOCKED || error == ERR_BACKEND_READONLY))
    {
        gchar *name = gnc_uri_targets_local_fs (request->uri) ?
            gnc_uri_get_path (request->uri) : gnc_uri_normalize_uri (request->uri, FALSE);
        GtkWidget *dialog = gtk_message_dialog_new (request->parent,
            GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
            GTK_MESSAGE_WARNING, GTK_BUTTONS_NONE,
            _("GnuCash could not obtain the lock for %s."), name);
        g_free (name);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog), "%s",
            error == ERR_BACKEND_LOCKED ?
            _("That database may be in use by another user, in which case you should not open the database. What would you like to do?") :
            _("That database may be on a read-only file system, you may not have write permission for the directory, or your anti-virus software is preventing this action. If you proceed you may not be able to save any changes. What would you like to do?"));
        gnc_gtk_dialog_add_button (dialog, _("Open _Read-Only"), "emblem-readonly", RESPONSE_READONLY);
        gnc_gtk_dialog_add_button (dialog, _("Create _New File"), "document-new-symbolic", RESPONSE_NEW);
        gnc_gtk_dialog_add_button (dialog, _("Open _Anyway"), "document-open-symbolic", RESPONSE_OPEN);
        gnc_gtk_dialog_add_button (dialog, _("Open _Folder"), "folder-open-symbolic", RESPONSE_FILE);
        if (shutdown_cb) gtk_dialog_add_button (GTK_DIALOG (dialog), _("_Quit"), RESPONSE_QUIT);
        gtk_dialog_set_default_response (GTK_DIALOG (dialog), shutdown_cb ? RESPONSE_QUIT : RESPONSE_FILE);
        gnc_dialog_run_async (GTK_DIALOG (dialog), NULL, file_open_choice, request);
    }
    else if (mode == SESSION_NORMAL_OPEN && error == ERR_BACKEND_NO_SUCH_DB)
        show_session_error_full (request->parent, error, request->uri,
            GNC_FILE_DIALOG_OPEN, file_open_choice, request);
    else
        show_session_error_full (request->parent, error, request->uri,
            GNC_FILE_DIALOG_OPEN, error == ERR_BACKEND_BAD_URL ?
            file_open_bad_url_closed : file_open_error_closed, request);
}

static void
file_open_password_ready (gboolean accepted, gchar *username, gchar *password,
                           gpointer user_data)
{
    FileOpenRequest *request = user_data;
    if (!accepted || !file_open_current (request))
    {
        g_free (username);
        g_free (password);
        file_open_finish (request, FALSE);
        return;
    }
    gchar *scheme = NULL, *hostname = NULL, *olduser = NULL, *oldpass = NULL, *path = NULL;
    gint32 port = 0;
    gnc_uri_get_components (request->uri, &scheme, &hostname, &port, &olduser, &oldpass, &path);
    g_free (request->uri);
    request->uri = gnc_uri_create_uri (scheme, hostname, port, username, password, path);
    gnc_keyring_set_password (scheme, hostname, port, path, username, password);
    g_free (scheme); g_free (hostname); g_free (olduser); g_free (oldpass); g_free (path);
    g_free (username); g_free (password);
    file_open_begin (request, request->readonly ? SESSION_READ_ONLY : SESSION_NORMAL_OPEN);
}

static void
file_open_normalize (FileOpenRequest *request, const gchar *filename)
{
    g_clear_pointer (&request->uri, g_free);
    request->uri = gnc_uri_normalize_uri (filename, TRUE);
    if (!request->uri)
    {
        show_session_error_full (request->parent, ERR_FILEIO_FILE_NOT_FOUND, filename,
            GNC_FILE_DIALOG_OPEN, file_open_error_closed, request);
        return;
    }
    gchar *scheme = NULL, *hostname = NULL, *username = NULL, *password = NULL, *path = NULL;
    gint32 port = 0;
    gnc_uri_get_components (request->uri, &scheme, &hostname, &port, &username, &password, &path);
    gboolean ask_password = !gnc_uri_is_file_scheme (scheme) && !password;
    if (gnc_uri_is_file_scheme (scheme))
    {
        gchar *directory = g_path_get_dirname (path);
        gnc_set_default_directory (GNC_PREFS_GROUP_OPEN_SAVE, directory);
        g_free (directory);
    }
    if (ask_password)
        gnc_keyring_get_password_async (request->parent ? GTK_WIDGET (request->parent) : NULL,
            scheme, hostname, port, path, username, password, file_open_password_ready, request);
    g_free (scheme); g_free (hostname); g_free (username); g_free (password); g_free (path);
    if (!ask_password)
        file_open_begin (request, request->readonly ? SESSION_READ_ONLY : SESSION_NORMAL_OPEN);
}

static void
file_open_selected (GSList *filenames, gpointer user_data)
{
    FileOpenRequest *request = user_data;
    if (!filenames || !file_open_current (request))
        file_open_finish (request, FALSE);
    else
        file_open_normalize (request, filenames->data);
    g_slist_free_full (filenames, g_free);
}

static void
file_open_choose (FileOpenRequest *request, const gchar *directory)
{
    if (request->destination)
    {
        qof_book_mark_session_saved (qof_session_get_book (request->destination));
        xaccLogDisable ();
        qof_session_destroy (request->destination);
        xaccLogEnable ();
        request->destination = NULL;
    }
    gchar *default_directory = directory ? g_strdup (directory) :
        gnc_get_default_directory (GNC_PREFS_GROUP_OPEN_SAVE);
    gnc_file_dialog_async (request->parent, _("Open"),
        gnc_file_chooser_get_datafile_filters (), default_directory,
        GNC_FILE_DIALOG_OPEN, FALSE, file_open_selected, request, NULL);
    g_free (default_directory);
}

static void
file_open_after_save (gboolean permitted, gpointer user_data)
{
    FileOpenRequest *request = user_data;
    /* Save As may replace the session while retaining exactly the same book. */
    if (permitted && request->book && gnc_current_session_exist () &&
        qof_session_get_book (gnc_get_current_session ()) == request->book)
        request->original = gnc_get_current_session ();
    if (!permitted || !file_open_current (request))
        file_open_finish (request, FALSE);
    else if (!request->uri)
        file_open_choose (request, NULL);
    else
    {
        gchar *filename = g_strdup (request->uri);
        file_open_normalize (request, filename);
        g_free (filename);
    }
}

void
gnc_file_open_file_async (GtkWindow *parent, const gchar *filename,
                          gboolean readonly, GNCFileSaveCallback completed,
                          gpointer user_data)
{
    FileOpenRequest *request = file_open_request_new (parent, completed, user_data);
    if (!request) return;
    request->readonly = readonly;
    request->uri = g_strdup (filename);
    gnc_account_reset_convert_bayes_to_flat ();
    gnc_file_query_save_async (parent, TRUE, file_open_after_save, request);
}

void
gnc_file_open (GtkWindow *parent)
{
    gnc_file_open_file_async (parent, NULL, FALSE, NULL, NULL);
}

void
gnc_file_open_file (GtkWindow *parent, const gchar *filename, gboolean readonly)
{
    if (filename && *filename)
        gnc_file_open_file_async (parent, filename, readonly, NULL, NULL);
}


/* Prevent the user from storing or exporting data files into the settings
 * directory.
 */
static gboolean
check_file_path (const char *path)
{
    /* Remember the directory as the default. */
     gchar *dir = g_path_get_dirname(path);
     const gchar *dotgnucash = gnc_userdata_dir();
     char *dirpath = dir;

     /* Prevent user from storing file in GnuCash' private configuration
      * directory (~/.gnucash by default in linux, but can be overridden)
      */
     while (strcmp(dir = g_path_get_dirname(dirpath), dirpath) != 0)
     {
         if (strcmp(dirpath, dotgnucash) == 0)
         {
             g_free (dir);
             g_free (dirpath);
             return TRUE;
         }
         g_free (dirpath);
         dirpath = dir;
     }
     g_free (dirpath);
     g_free(dir);
     return FALSE;
}


static void
file_export_write (FileOpenRequest *request)
{
    if (!file_open_current (request))
    {
        file_open_finish (request, FALSE);
        return;
    }
    qof_event_suspend ();
    gnc_set_busy_cursor (NULL, TRUE);
    gnc_window_show_progress (_("Exporting file…"), 0.0);
    gboolean saved = qof_session_export (request->destination, request->original,
                                         gnc_window_show_progress);
    gnc_window_show_progress (NULL, -1.0);
    gnc_unset_busy_cursor (NULL);
    qof_event_resume ();
    if (!saved)
    {
        gnc_message_dialog_async_response (request->parent_destroyed ? NULL : request->parent,
            GTK_MESSAGE_ERROR, file_open_error_closed, request,
            _("There was an error saving the file.\n\n%s"), strerror (errno));
        return;
    }
    file_open_finish (request, TRUE);
}

static void
file_export_confirmed (GtkWindow *parent, gint response, gpointer user_data)
{
    FileOpenRequest *request = user_data;
    if (!file_open_current (request) || (request->parent && !parent) ||
        response != GTK_RESPONSE_YES)
    {
        file_open_finish (request, FALSE);
        return;
    }
    qof_session_begin (request->destination, request->uri,
        request->error == ERR_BACKEND_STORE_EXISTS ? SESSION_NEW_OVERWRITE : SESSION_BREAK_LOCK);
    request->error = qof_session_get_error (request->destination);
    if (request->error == ERR_BACKEND_NO_ERR)
        file_export_write (request);
    else
        show_session_error_full (request->parent, request->error, request->uri,
            GNC_FILE_DIALOG_EXPORT, file_open_error_closed, request);
}

void
gnc_file_do_export (GtkWindow *parent, const char *filename)
{
    FileOpenRequest *request = file_open_request_new (parent, NULL, NULL);
    if (!request) return;
    gchar *normalized = gnc_uri_normalize_uri (filename, TRUE);
    if (!normalized)
    {
        show_session_error_full (parent, ERR_FILEIO_FILE_NOT_FOUND, filename,
            GNC_FILE_DIALOG_EXPORT, file_open_error_closed, request);
        return;
    }
    request->uri = gnc_uri_add_extension (normalized, GNC_DATAFILE_EXT);
    g_free (normalized);
    gchar *scheme = NULL, *hostname = NULL, *username = NULL, *password = NULL, *path = NULL;
    gint32 port = 0;
    gnc_uri_get_components (request->uri, &scheme, &hostname, &port, &username, &password, &path);
    if (g_strcmp0 (scheme, "file") == 0)
    {
        g_free (scheme);
        scheme = g_strdup ("xml");
        g_free (request->uri);
        request->uri = gnc_uri_create_uri (scheme, hostname, port, username, password, path);
    }
    QofBackendError error = ERR_BACKEND_NO_ERR;
    if (gnc_uri_is_file_scheme (scheme))
    {
        if (check_file_path (path))
            error = ERR_FILEIO_RESERVED_WRITE;
        else
        {
            gchar *directory = g_path_get_dirname (path);
            gnc_set_default_directory (GNC_PREFS_GROUP_EXPORT, directory);
            g_free (directory);
        }
    }
    g_free (scheme); g_free (hostname); g_free (username); g_free (password); g_free (path);
    if (g_strcmp0 (request->uri, qof_session_get_url (request->original)) == 0)
        error = ERR_FILEIO_WRITE_ERROR;
    if (error != ERR_BACKEND_NO_ERR)
    {
        show_session_error_full (parent, error, request->uri,
            GNC_FILE_DIALOG_EXPORT, file_open_error_closed, request);
        return;
    }
    request->destination = qof_session_new (NULL);
    qof_session_begin (request->destination, request->uri, SESSION_NEW_STORE);
    request->error = qof_session_get_error (request->destination);
    if (request->error == ERR_BACKEND_NO_ERR)
        file_export_write (request);
    else if (request->error == ERR_BACKEND_STORE_EXISTS)
    {
        gchar *name = gnc_uri_targets_local_fs (request->uri) ?
            gnc_uri_get_path (request->uri) : gnc_uri_normalize_uri (request->uri, FALSE);
        gnc_verify_dialog_async (parent, FALSE, file_export_confirmed, request,
            _("The file %s already exists. Are you sure you want to overwrite it?"), name);
        g_free (name);
    }
    else
        show_session_error_full (parent, request->error, request->uri,
            GNC_FILE_DIALOG_EXPORT,
            request->error == ERR_BACKEND_LOCKED ? file_export_confirmed : file_open_error_closed,
            request);
}

typedef struct { GtkWindow *parent; gboolean destroyed; } FileExportChooser;

static void
file_export_parent_destroyed ([[maybe_unused]] GtkWidget *parent, FileExportChooser *request)
{
    request->destroyed = TRUE;
}

static void
file_export_selected (GSList *filenames, gpointer user_data)
{
    FileExportChooser *request = user_data;
    if (filenames && !request->destroyed)
        gnc_file_do_export (request->parent, filenames->data);
    g_slist_free_full (filenames, g_free);
    if (request->parent)
        g_signal_handlers_disconnect_by_data (request->parent, request);
    g_clear_object (&request->parent);
    g_free (request);
}

void
gnc_file_export (GtkWindow *parent)
{
    FileExportChooser *request = g_new0 (FileExportChooser, 1);
    if (parent)
    {
        request->parent = g_object_ref (parent);
        g_signal_connect (parent, "destroy", G_CALLBACK (file_export_parent_destroyed), request);
    }
    gchar *directory = gnc_get_default_directory (GNC_PREFS_GROUP_EXPORT);
    gnc_file_dialog_async (parent, _("Save"), gnc_file_chooser_get_datafile_filters (),
        directory, GNC_FILE_DIALOG_SAVE, FALSE, file_export_selected, request, NULL);
    g_free (directory);
}


/* Saving is synchronous backend work, but choosing its destination and deciding
 * whether to overwrite are not. Keep only the original identity across those
 * responses; never keep a suspended engine event stream across a dialog. */
typedef struct
{
    GtkWindow *parent;
    gboolean parent_destroyed;
    QofBook *book;
    QofSession *session;
    QofSession *destination;
    char *uri;
    GNCFileSaveCallback completed;
    gpointer user_data;
} FileSaveRequest;

static void file_save_choose (FileSaveRequest *request);
static void file_save_write_current (FileSaveRequest *request);
static void file_save_write_destination (FileSaveRequest *request);

static void
file_save_parent_destroyed ([[maybe_unused]] GtkWidget *parent,
                            FileSaveRequest *request)
{
    request->parent_destroyed = TRUE;
}

static gboolean
file_save_is_current (FileSaveRequest *request)
{
    return !request->parent_destroyed && request->book &&
        qof_book_is_open (request->book) &&
        !qof_book_shutting_down (request->book) &&
        gnc_current_session_exist () &&
        gnc_get_current_session () == request->session &&
        qof_session_get_book (request->session) == request->book;
}

static void
file_save_finish (FileSaveRequest *request, gboolean saved)
{
    GNCFileSaveCallback completed = request->completed;
    gpointer user_data = request->user_data;
    if (request->destination)
    {
        xaccLogDisable ();
        qof_session_destroy (request->destination);
        xaccLogEnable ();
    }
    if (request->book)
        g_object_remove_weak_pointer (G_OBJECT (request->book),
                                      (gpointer *)&request->book);
    if (request->parent)
        g_signal_handlers_disconnect_by_data (request->parent, request);
    g_clear_object (&request->parent);
    g_free (request->uri);
    g_free (request);
    --save_in_progress;
    if (completed)
        completed (saved, user_data);
}

static FileSaveRequest *
file_save_request_new (GtkWindow *parent, GNCFileSaveCallback completed,
                       gpointer user_data)
{
    FileSaveRequest *request;
    /* Reject concurrent commands instead of introducing a file transition queue.
     * The caller gets an explicit cancellation and may issue a new command. */
    if (!gnc_current_session_exist () || gnc_file_save_in_progress () ||
        gnc_gui_session_operation_pending () ||
        (g_object_get_data (G_OBJECT (gnc_get_current_book ()), "gnc-file-open-pending") &&
         !g_object_get_data (G_OBJECT (gnc_get_current_book ()), "gnc-query-save-pending")) ||
        (parent && gtk_widget_in_destruction (GTK_WIDGET (parent))))
    {
        if (completed)
            completed (FALSE, user_data);
        return NULL;
    }
    request = g_new0 (FileSaveRequest, 1);
    request->session = gnc_get_current_session ();
    request->book = qof_session_get_book (request->session);
    request->completed = completed;
    request->user_data = user_data;
    g_object_add_weak_pointer (G_OBJECT (request->book),
                               (gpointer *)&request->book);
    if (parent)
    {
        request->parent = g_object_ref (parent);
        g_signal_connect (parent, "destroy",
                          G_CALLBACK (file_save_parent_destroyed), request);
    }
    ++save_in_progress;
    return request;
}

static char *
file_save_display_name (const char *uri)
{
    return gnc_uri_targets_local_fs (uri) ? gnc_uri_get_path (uri) :
        gnc_uri_normalize_uri (uri, FALSE);
}

static void
file_save_report_error_full (FileSaveRequest *request, QofBackendError error,
                             GncGuiQueryResponseCallback completed)
{
    /* A failed write is terminal here. The recovery questions from the old
     * synchronous helper belong to opening/beginning a store, not retrying a
     * failed write after swapping books. Never run one in a nested loop. */
    switch (error)
    {
    case ERR_BACKEND_LOCKED:
    case ERR_BACKEND_NO_SUCH_DB:
    case ERR_SQL_DB_TOO_OLD:
    case ERR_FILEIO_FILE_BAD_READ:
    case ERR_FILEIO_FILE_TOO_OLD:
    {
        char *name = file_save_display_name (request->uri ? request->uri :
                                             qof_session_get_url (request->session));
        gnc_message_dialog_async_response (request->parent,
            GTK_MESSAGE_ERROR, completed, request,
            _("Unable to save %s (I/O error %d). The original book remains open."),
            name, error);
        g_free (name);
        break;
    }
    default:
        show_session_error_full (request->parent, error,
                            request->uri ? request->uri :
                            qof_session_get_url (request->session),
                            GNC_FILE_DIALOG_SAVE, completed, request);
        break;
    }
}

static void
file_save_report_error (FileSaveRequest *request, QofBackendError error)
{
    file_save_report_error_full (request, error, NULL);
}

static void
file_save_recovery_dismissed (GtkWindow *parent, [[maybe_unused]] gint response,
                               gpointer user_data)
{
    FileSaveRequest *request = user_data;
    if ((request->parent && !parent) || !file_save_is_current (request))
        file_save_finish (request, FALSE);
    else
        file_save_choose (request);
}

static void
file_save_begin_response (GtkWindow *parent, gint response, gpointer user_data)
{
    FileSaveRequest *request = user_data;
    QofBackendError error;
    SessionOpenMode mode;
    if ((request->parent && !parent) || !file_save_is_current (request) ||
        (response != GTK_RESPONSE_YES && response != GTK_RESPONSE_OK))
    {
        file_save_finish (request, FALSE);
        return;
    }
    error = qof_session_get_error (request->destination);
    mode = error == ERR_BACKEND_STORE_EXISTS ? SESSION_NEW_OVERWRITE :
        error == ERR_BACKEND_LOCKED ? SESSION_BREAK_LOCK : SESSION_NEW_STORE;
    qof_session_begin (request->destination, request->uri, mode);
    error = qof_session_get_error (request->destination);
    if (error != ERR_BACKEND_NO_ERR)
    {
        file_save_report_error (request, error);
        file_save_finish (request, FALSE);
        return;
    }
    file_save_write_destination (request);
}

static void
file_save_begin_destination (FileSaveRequest *request, const char *filename)
{
    gchar *normalized = gnc_uri_normalize_uri (filename, TRUE);
    gchar *scheme = NULL, *hostname = NULL, *username = NULL;
    gchar *password = NULL, *path = NULL;
    gint32 port = 0;
    QofBackendError error;
    if (!normalized || !file_save_is_current (request))
    {
        g_free (normalized);
        file_save_finish (request, FALSE);
        return;
    }
    request->uri = gnc_uri_add_extension (normalized, GNC_DATAFILE_EXT);
    g_free (normalized);
    gnc_uri_get_components (request->uri, &scheme, &hostname, &port,
                            &username, &password, &path);
    if (g_strcmp0 (scheme, "file") == 0)
    {
        g_free (scheme);
        scheme = g_strdup ("xml");
        g_free (request->uri);
        request->uri = gnc_uri_create_uri (scheme, hostname, port,
                                           username, password, path);
    }
    error = ERR_BACKEND_NO_ERR;
    if (gnc_uri_is_file_scheme (scheme))
    {
        if (check_file_path (path))
            error = ERR_FILEIO_RESERVED_WRITE;
        else
        {
            gchar *directory = g_path_get_dirname (path);
            gnc_set_default_directory (GNC_PREFS_GROUP_OPEN_SAVE, directory);
            g_free (directory);
        }
    }
    g_free (scheme);
    g_free (hostname);
    g_free (username);
    g_free (password);
    g_free (path);
    if (error != ERR_BACKEND_NO_ERR)
    {
        file_save_report_error (request, error);
        file_save_finish (request, FALSE);
        return;
    }
    if (g_strcmp0 (qof_session_get_url (request->session), request->uri) == 0)
    {
        /* A read-only book cannot be saved to the same destination. */
        if (qof_book_is_readonly (request->book))
            file_save_finish (request, FALSE);
        else
            file_save_write_current (request);
        return;
    }
    request->destination = qof_session_new (NULL);
    qof_session_begin (request->destination, request->uri, SESSION_NEW_STORE);
    error = qof_session_get_error (request->destination);
    if (error == ERR_BACKEND_NO_ERR)
        file_save_write_destination (request);
    else if (error == ERR_BACKEND_STORE_EXISTS || error == ERR_BACKEND_LOCKED ||
             error == ERR_BACKEND_NO_SUCH_DB || error == ERR_SQL_DB_TOO_OLD)
    {
        gchar *name = file_save_display_name (request->uri);
        if (error == ERR_SQL_DB_TOO_OLD)
            gnc_ok_cancel_dialog_async (request->parent, GTK_RESPONSE_CANCEL,
                file_save_begin_response, request, "%s",
                _("This database is from an older version of GnuCash. "
                  "Select OK to upgrade it to the current version, Cancel "
                  "to mark it read-only."));
        else
            gnc_verify_dialog_async (request->parent,
                error == ERR_BACKEND_NO_SUCH_DB, file_save_begin_response,
                request, error == ERR_BACKEND_STORE_EXISTS ?
                _("The file %s already exists. Are you sure you want to overwrite it?") :
                error == ERR_BACKEND_LOCKED ?
                _("GnuCash could not obtain the lock for %s. "
                  "That database may be in use by another user, "
                  "in which case you should not save the database. "
                  "Do you want to proceed with saving the database?") :
                _("The database %s doesn't seem to exist. Do you want to create it?"),
                name);
        g_free (name);
    }
    else if (error == ERR_FILEIO_FILE_NOT_FOUND)
    {
        qof_session_begin (request->destination, request->uri, SESSION_NEW_STORE);
        error = qof_session_get_error (request->destination);
        if (error == ERR_BACKEND_NO_ERR)
            file_save_write_destination (request);
        else
        {
            file_save_report_error (request, error);
            file_save_finish (request, FALSE);
        }
    }
    else
    {
        file_save_report_error (request, error);
        file_save_finish (request, FALSE);
    }
}

static void
file_save_selected (GSList *filenames, gpointer user_data)
{
    FileSaveRequest *request = user_data;
    if (!filenames || !file_save_is_current (request))
        file_save_finish (request, FALSE);
    else
        file_save_begin_destination (request, filenames->data);
    g_slist_free_full (filenames, g_free);
}

static void
file_save_choose (FileSaveRequest *request)
{
    gchar *last = gnc_history_get_last ();
    gchar *directory;
    if (last && gnc_uri_targets_local_fs (last))
    {
        gchar *path = gnc_uri_get_path (last);
        directory = g_path_get_dirname (path);
        g_free (path);
    }
    else
        directory = gnc_get_default_directory (GNC_PREFS_GROUP_OPEN_SAVE);
    gnc_file_dialog_async (request->parent, _("Save"),
        gnc_file_chooser_get_datafile_filters (), directory,
        GNC_FILE_DIALOG_SAVE, FALSE, file_save_selected, request, NULL);
    g_free (last);
    g_free (directory);
}

static void
file_save_write_current (FileSaveRequest *request)
{
    QofBackendError error;
    if (!file_save_is_current (request) || qof_book_is_readonly (request->book))
    {
        file_save_finish (request, FALSE);
        return;
    }
    gnc_set_busy_cursor (NULL, TRUE);
    gnc_window_show_progress (_("Writing file…"), 0.0);
    qof_session_save (request->session, gnc_window_show_progress);
    gnc_window_show_progress (NULL, -1.0);
    gnc_unset_busy_cursor (NULL);
    if (!file_save_is_current (request))
    {
        file_save_finish (request, FALSE);
        return;
    }
    error = qof_session_get_error (request->session);
    if (error != ERR_BACKEND_NO_ERR)
    {
        /* Preserve Save's recovery through Save As, but wait for the error
         * acknowledgement before opening the chooser. The same request owns
         * every stage, so no destructive continuation can run in between. */
        file_save_report_error_full (request, error, file_save_recovery_dismissed);
        return;
    }
    else
    {
        xaccReopenLog ();
        gnc_add_history (request->session);
        gnc_hook_run (HOOK_BOOK_SAVED, request->session);
    }
    file_save_finish (request, error == ERR_BACKEND_NO_ERR &&
                      file_save_is_current (request) &&
                      !qof_book_session_not_saved (request->book));
}

static void
file_save_write_destination (FileSaveRequest *request)
{
    QofSession *original;
    QofBackendError error;
    if (!file_save_is_current (request))
    {
        file_save_finish (request, FALSE);
        return;
    }
    qof_event_suspend ();
    gnc_suspend_gui_refresh ();
    qof_session_ensure_all_data_loaded (request->session);
    gnc_resume_gui_refresh ();
    qof_event_resume ();
    if (!file_save_is_current (request))
    {
        file_save_finish (request, FALSE);
        return;
    }
    error = qof_session_get_error (request->session);
    if (error != ERR_BACKEND_NO_ERR)
    {
        file_save_report_error (request, error);
        file_save_finish (request, FALSE);
        return;
    }
    /* Install the session containing the original book before backend progress
     * can dispatch GUI work. On failure, exchange it back before notifications.
     * No session is destroyed while it still owns the original book. */
    original = request->session;
    qof_event_suspend ();
    qof_session_swap_data (original, request->destination);
    qof_book_mark_session_dirty (request->book);
    gnc_exchange_current_session (request->destination);
    request->session = request->destination;
    request->destination = NULL;
    qof_event_resume ();
    gnc_set_busy_cursor (NULL, TRUE);
    gnc_window_show_progress (_("Writing file…"), 0.0);
    qof_session_save (request->session, gnc_window_show_progress);
    gnc_window_show_progress (NULL, -1.0);
    gnc_unset_busy_cursor (NULL);
    error = qof_session_get_error (request->session);
    qof_event_suspend ();
    if (error != ERR_BACKEND_NO_ERR)
    {
        request->destination = request->session;
        qof_session_swap_data (request->destination, original);
        gnc_exchange_current_session (original);
        request->session = original;
    }
    else
    {
        gnc_gui_component_reset_session (original, request->session);
        xaccLogDisable ();
        qof_session_destroy (original);
        xaccLogEnable ();
    }
    qof_event_resume ();
    if (error != ERR_BACKEND_NO_ERR)
        file_save_report_error (request, error);
    else
    {
        gchar *scheme = NULL, *hostname = NULL, *username = NULL;
        gchar *password = NULL, *path = NULL;
        gint32 port = 0;
        gnc_uri_get_components (request->uri, &scheme, &hostname, &port,
                                &username, &password, &path);
        if (!gnc_uri_is_file_scheme (scheme))
            gnc_keyring_set_password (scheme, hostname, port, path, username, password);
        g_free (scheme);
        g_free (hostname);
        g_free (username);
        g_free (password);
        g_free (path);
        xaccReopenLog ();
        gnc_add_history (request->session);
        gnc_hook_run (HOOK_BOOK_SAVED, request->session);
    }
    file_save_finish (request, error == ERR_BACKEND_NO_ERR &&
                      file_save_is_current (request) &&
                      !qof_book_session_not_saved (request->book));
}

static void
file_save_readonly_response (GtkWindow *parent, gint response, gpointer user_data)
{
    FileSaveRequest *request = user_data;
    if ((request->parent && !parent) || response != GTK_RESPONSE_OK ||
        !file_save_is_current (request))
        file_save_finish (request, FALSE);
    else
        file_save_choose (request);
}

void
gnc_file_save_async (GtkWindow *parent, GNCFileSaveCallback completed,
                      gpointer user_data)
{
    FileSaveRequest *request = file_save_request_new (parent, completed, user_data);
    if (!request)
        return;
    if (!strlen (qof_session_get_url (request->session)))
        file_save_choose (request);
    else if (qof_book_is_readonly (request->book))
        gnc_ok_cancel_dialog_async (parent, GTK_RESPONSE_CANCEL,
            file_save_readonly_response, request, "%s",
            _("The database was opened read-only. Do you want to save it to a different place?"));
    else
        file_save_write_current (request);
}

void
gnc_file_save_as_async (GtkWindow *parent, GNCFileSaveCallback completed,
                         gpointer user_data)
{
    FileSaveRequest *request = file_save_request_new (parent, completed, user_data);
    if (request)
        file_save_choose (request);
}

void
gnc_file_do_save_as_async (GtkWindow *parent, const char *filename,
                            GNCFileSaveCallback completed, gpointer user_data)
{
    FileSaveRequest *request = file_save_request_new (parent, completed, user_data);
    if (!request)
        return;
    if (!filename)
        file_save_finish (request, FALSE);
    else
        file_save_begin_destination (request, filename);
}

typedef struct
{
    GtkWindow *parent;
    gboolean parent_destroyed;
    QofBook *book;
    gboolean can_cancel;
    GNCFileSaveCallback completed;
    gpointer user_data;
} FileQuerySaveRequest;

static void file_query_save_present (FileQuerySaveRequest *request);

static void
file_query_save_parent_destroyed ([[maybe_unused]] GtkWidget *parent,
                                  FileQuerySaveRequest *request)
{
    request->parent_destroyed = TRUE;
}

static gboolean
file_query_save_is_current (FileQuerySaveRequest *request)
{
    return !request->parent_destroyed && request->book &&
        qof_book_is_open (request->book) && !qof_book_shutting_down (request->book) &&
        gnc_current_session_exist () &&
        qof_session_get_book (gnc_get_current_session ()) == request->book;
}

static void
file_query_save_finish (FileQuerySaveRequest *request, gboolean proceed)
{
    GNCFileSaveCallback completed = request->completed;
    gpointer user_data = request->user_data;
    if (request->book)
    {
        if (g_object_get_data (G_OBJECT (request->book), "gnc-query-save-pending") == request)
            g_object_set_data (G_OBJECT (request->book), "gnc-query-save-pending", NULL);
        g_object_remove_weak_pointer (G_OBJECT (request->book), (gpointer *)&request->book);
    }
    if (request->parent)
        g_signal_handlers_disconnect_by_data (request->parent, request);
    g_clear_object (&request->parent);
    g_free (request);
    if (completed)
        completed (proceed, user_data);
}

static void
file_query_save_saved ([[maybe_unused]] gboolean saved, gpointer user_data)
{
    FileQuerySaveRequest *request = user_data;
    if (!file_query_save_is_current (request))
        file_query_save_finish (request, FALSE);
    else
        file_query_save_present (request);
}

static void
file_query_save_response (GtkWindow *parent, gint response, gpointer user_data)
{
    FileQuerySaveRequest *request = user_data;
    if ((request->parent && !parent) || !file_query_save_is_current (request))
        file_query_save_finish (request, FALSE);
    else if (response == GTK_RESPONSE_YES)
        gnc_file_save_async (request->parent, file_query_save_saved, request);
    else
        file_query_save_finish (request, response == GTK_RESPONSE_OK ||
                                (!request->can_cancel && response == GTK_RESPONSE_DELETE_EVENT));
}

static void
file_query_save_present (FileQuerySaveRequest *request)
{
    GtkWidget *dialog;
    gint minutes;
    if (!file_query_save_is_current (request))
    {
        file_query_save_finish (request, FALSE);
        return;
    }
    if (!qof_book_session_not_saved (request->book))
    {
        file_query_save_finish (request, TRUE);
        return;
    }
    dialog = gtk_message_dialog_new (request->parent,
        GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
        GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE, "%s", _("Save changes to the file?"));
    minutes = (gnc_time (NULL) - qof_book_get_session_dirty_time (request->book)) / 60 + 1;
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
        ngettext ("If you don't save, changes from the past %d minute will be discarded.",
                  "If you don't save, changes from the past %d minutes will be discarded.",
                  minutes), minutes);
    gtk_dialog_add_button (GTK_DIALOG (dialog), _("Continue _Without Saving"), GTK_RESPONSE_OK);
    if (request->can_cancel)
        gtk_dialog_add_button (GTK_DIALOG (dialog), _("_Cancel"), GTK_RESPONSE_CANCEL);
    gtk_dialog_add_button (GTK_DIALOG (dialog), _("_Save"), GTK_RESPONSE_YES);
    gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);
    gnc_dialog_run_async (GTK_DIALOG (dialog), NULL, file_query_save_response, request);
}

void
gnc_file_query_save_async (GtkWindow *parent, gboolean can_cancel,
                            GNCFileSaveCallback completed, gpointer user_data)
{
    FileQuerySaveRequest *request;
    QofBook *book;
    if (gnc_file_save_in_progress () || gnc_gui_session_operation_pending () ||
        (parent && gtk_widget_in_destruction (GTK_WIDGET (parent))))
    {
        if (completed)
            completed (FALSE, user_data);
        return;
    }
    if (!gnc_current_session_exist ())
    {
        if (completed)
            completed (TRUE, user_data);
        return;
    }
    book = qof_session_get_book (gnc_get_current_session ());
    gpointer opening = g_object_get_data (G_OBJECT (book), "gnc-file-open-pending");
    if (opening && opening != user_data)
    {
        if (completed)
            completed (FALSE, user_data);
        return;
    }
    if (g_object_get_data (G_OBJECT (book), "gnc-query-save-pending") ||
        g_object_get_data (G_OBJECT (book), "gnc-save-close-pending"))
    {
        if (completed)
            completed (FALSE, user_data);
        return;
    }
    request = g_new0 (FileQuerySaveRequest, 1);
    request->parent = parent ? g_object_ref (parent) : NULL;
    request->book = book;
    request->can_cancel = can_cancel;
    request->completed = completed;
    request->user_data = user_data;
    g_object_add_weak_pointer (G_OBJECT (book), (gpointer *)&request->book);
    g_object_set_data (G_OBJECT (book), "gnc-query-save-pending", request);
    if (parent)
        g_signal_connect (parent, "destroy", G_CALLBACK (file_query_save_parent_destroyed), request);
    gnc_autosave_remove_timer (book);
    file_query_save_present (request);
}

static void
file_revert_confirmed (GtkWindow *parent, gint response, gpointer user_data)
{
    FileOpenRequest *request = user_data;
    if (response != GTK_RESPONSE_YES || (request->parent && !parent) || !file_open_current (request))
        file_open_finish (request, FALSE);
    else
    {
        qof_book_mark_session_saved (request->book);
        gchar *filename = g_strdup (request->uri);
        file_open_normalize (request, filename);
        g_free (filename);
    }
}

static void
file_revert_pending_finished (gboolean accepted, gpointer user_data)
{
    FileOpenRequest *request = user_data;
    GtkWindow *parent = request->parent;
    if (!accepted || !file_open_current (request))
    {
        file_open_finish (request, FALSE);
        return;
    }
    request->uri = g_strdup (qof_session_get_url (request->original));
    request->readonly = qof_book_is_readonly (request->book);
    gchar *name = gnc_uri_targets_local_fs (request->uri) ?
        gnc_uri_get_path (request->uri) : gnc_uri_normalize_uri (request->uri, FALSE);
    gnc_verify_dialog_async (parent, FALSE, file_revert_confirmed, request,
        _("Reverting will discard all unsaved changes to %s. Are you sure you want to proceed?"),
        name && *name ? name : _("<unknown>"));
    g_free (name);
}

void
gnc_file_revert (GtkWindow *parent)
{
    if (gnc_file_save_in_progress ()) return;
    FileOpenRequest *request = file_open_request_new (parent, NULL, NULL);
    if (request)
        gnc_main_window_all_finish_pending_async (NULL, file_revert_pending_finished, request);
}

void
gnc_file_quit (void)
{
    QofSession *session;

    if (gnc_file_save_in_progress () || gnc_gui_session_operation_pending ())
        return;
    if (!gnc_current_session_exist ())
        return;
    gnc_set_busy_cursor (NULL, TRUE);
    session = gnc_get_current_session ();

    /* disable events; otherwise the mass deletion of accounts and
     * transactions during shutdown would cause massive redraws */
    qof_event_suspend ();

    gnc_hook_run(HOOK_BOOK_CLOSED, session);
    gnc_close_gui_component_by_session (session);
    gnc_state_save (session);
    gnc_clear_current_session();

    qof_event_resume ();
    gnc_unset_busy_cursor (NULL);
}

void
gnc_file_set_shutdown_callback (GNCShutdownCB cb)
{
    shutdown_cb = cb;
}

gboolean
gnc_file_save_in_progress (void)
{
    if (save_in_progress > 0)
        return TRUE;
    if (gnc_current_session_exist())
    {
        QofSession *session = gnc_get_current_session();
        return qof_session_save_in_progress(session);
    }
    return FALSE;
}

void gnc_file_save (GtkWindow *parent)
{
    gnc_file_save_async (parent, NULL, NULL);
}

void gnc_file_save_as (GtkWindow *parent)
{
    gnc_file_save_as_async (parent, NULL, NULL);
}

void gnc_file_do_save_as (GtkWindow *parent, const char *filename)
{
    gnc_file_do_save_as_async (parent, filename, NULL, NULL);
}
