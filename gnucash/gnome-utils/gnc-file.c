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
#include "gnc-gui-query.h"
#include "gnc-hooks.h"
#include "gnc-keyring.h"
#include "gnc-splash.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-uri-utils.h"
#include "gnc-window.h"
#include "gnc-plugin-file-history.h"
#include "qof.h"
#include "Scrub.h"
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


/********************************************************************\
 * gnc_file_dialog                                                  *
 *   Pops up a file selection dialog (either a "Save As" or an      *
 *   "Open"), and returns the name of the file the user selected.   *
 *   (This function does not return until the user selects a file   *
 *   or presses "Cancel" or the window manager destroy button)      *
 *                                                                  *
 * Args:   title        - the title of the window                   *
 *         filters      - list of GtkFileFilters to use, will be    *
                          freed automatically                       *
 *         default_dir  - start the chooser in this directory       *
 *         type         - what type of dialog (open, save, etc.)    *
 * Return: containing the name of the file the user selected        *
\********************************************************************/

char *
gnc_file_dialog (GtkWindow *parent,
                 const char * title,
                 GList * filters,
                 const char * starting_dir,
                 GNCFileDialogType type
                )
{
    GtkWidget *file_box;
    const char *internal_name;
    char *file_name = NULL;
    gchar * okbutton = NULL;
    const gchar *ok_icon = NULL;
    GtkFileChooserAction action = GTK_FILE_CHOOSER_ACTION_OPEN;
    gint response;

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
    {
        GList* filter;
        GtkFileFilter* all_filter = gtk_file_filter_new();

        for (filter = filters; filter; filter = filter->next)
        {
            g_return_val_if_fail(GTK_IS_FILE_FILTER(filter->data), NULL);
            gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (file_box),
                                         GTK_FILE_FILTER (filter->data));
        }

        gtk_file_filter_set_name (all_filter, _("All files"));
        gtk_file_filter_add_pattern (all_filter, "*");
        gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (file_box), all_filter);

        /* Note: You cannot set a file filter and preselect a file name.
         * The latter wins, and the filter ends up disabled.  Since we are
         * only setting the starting directory for the chooser dialog,
         * everything works as expected. */
        gtk_file_chooser_set_filter (GTK_FILE_CHOOSER (file_box),
                                     GTK_FILE_FILTER (filters->data));
        g_list_free (filters);
    }

    response = gtk_dialog_run(GTK_DIALOG(file_box));

    // Set the style context for this widget so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(file_box), "GncFileDialog");

    if (response == GTK_RESPONSE_ACCEPT)
    {
        /* look for constructs like postgres://foo */
        internal_name = gtk_file_chooser_get_uri(GTK_FILE_CHOOSER (file_box));
        if (internal_name != NULL)
        {
            if (strstr (internal_name, "file://") == internal_name)
            {
                /* nope, a local file name */
                internal_name = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER (file_box));
            }
            file_name = g_strdup(internal_name);
        }
    }
    gtk_widget_destroy(GTK_WIDGET(file_box));
    LEAVE("%s", file_name ? file_name : "(null)");
    return file_name;
}


gboolean
show_session_error (GtkWindow *parent,
                    QofBackendError io_error,
                    const char *newfile,
                    GNCFileDialogType type)
{
    GtkWidget *dialog;
    gboolean uh_oh = TRUE;
    const char *fmt, *label;
    gchar *displayname;
    gint response;

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
        uh_oh = FALSE;
        break;

    case ERR_BACKEND_NO_HANDLER:
        fmt = _("No suitable backend was found for %s.");
        gnc_error_dialog(parent, fmt, displayname);
        break;

    case ERR_BACKEND_NO_BACKEND:
        fmt = _("The URL %s is not supported by this version of GnuCash.");
        gnc_error_dialog (parent, fmt, displayname);
        break;

    case ERR_BACKEND_BAD_URL:
        fmt = _("Can't parse the URL %s.");
        gnc_error_dialog (parent, fmt, displayname);
        break;

    case ERR_BACKEND_CANT_CONNECT:
        fmt = _("Can't connect to %s. "
                "The host, username or password were incorrect.");
        gnc_error_dialog (parent, fmt, displayname);
        break;

    case ERR_BACKEND_CONN_LOST:
        fmt = _("Can't connect to %s. "
                "Connection was lost, unable to send data.");
        gnc_error_dialog (parent, fmt, displayname);
        break;

    case ERR_BACKEND_TOO_NEW:
        fmt = _("This file/URL appears to be from a newer version "
                "of GnuCash. You must upgrade your version of GnuCash "
                "to work with this data.");
        gnc_error_dialog (parent, "%s", fmt);
        break;

    case ERR_BACKEND_NO_SUCH_DB:
        fmt = _("The database %s doesn't seem to exist. "
                "Do you want to create it?");
        if (gnc_verify_dialog (parent, TRUE, fmt, displayname))
        {
            uh_oh = FALSE;
        }
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
        response = gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);
        uh_oh = (response != GTK_RESPONSE_YES);
        break;

    case ERR_BACKEND_READONLY:
        fmt = _("GnuCash could not write to %s. "
                "That database may be on a read-only file system, "
                "you may not have write permission for the directory "
                "or your anti-virus software is preventing this action.");
        gnc_error_dialog (parent, fmt, displayname);
        break;

    case ERR_BACKEND_DATA_CORRUPT:
        fmt = _("The file/URL %s "
                "does not contain GnuCash data or the data is corrupt.");
        gnc_error_dialog (parent, fmt, displayname);
        break;

    case ERR_BACKEND_SERVER_ERR:
        fmt = _("The server at URL %s "
                "experienced an error or encountered bad or corrupt data.");
        gnc_error_dialog (parent, fmt, displayname);
        break;

    case ERR_BACKEND_PERM:
        fmt = _("You do not have permission to access %s.");
        gnc_error_dialog (parent, fmt, displayname);
        break;

    case ERR_BACKEND_MISC:
        fmt = _("An error occurred while processing %s.");
        gnc_error_dialog (parent, fmt, displayname);
        break;

    case ERR_FILEIO_FILE_BAD_READ:
        fmt = _("There was an error reading the file. "
                "Do you want to continue?");
        if (gnc_verify_dialog (parent, TRUE, "%s", fmt))
        {
            uh_oh = FALSE;
        }
        break;

    case ERR_FILEIO_PARSE_ERROR:
        fmt = _("There was an error parsing the file %s.");
        gnc_error_dialog (parent, fmt, displayname);
        break;

    case ERR_FILEIO_FILE_EMPTY:
        fmt = _("The file %s is empty.");
        gnc_error_dialog (parent, fmt, displayname);
        break;

    case ERR_FILEIO_FILE_NOT_FOUND:
        if (type == GNC_FILE_DIALOG_SAVE)
        {
            uh_oh = FALSE;
        }
        else
        {
            if (gnc_history_test_for_file (displayname))
            {
                fmt = _("The file/URI %s could not be found.\n\nThe file is in the history list, do you want to remove it?");
                if (gnc_verify_dialog (parent, FALSE, fmt, displayname))
                    gnc_history_remove_file (displayname);
            }
            else
            {
                fmt = _("The file/URI %s could not be found.");
                gnc_error_dialog (parent, fmt, displayname);
            }
        }
        break;

    case ERR_FILEIO_FILE_TOO_OLD:
        fmt = _("This file is from an older version of GnuCash. "
                "Do you want to continue?");
        if (gnc_verify_dialog (parent, TRUE, "%s", fmt))
        {
            uh_oh = FALSE;
        }
        break;

    case ERR_FILEIO_UNKNOWN_FILE_TYPE:
        fmt = _("The file type of file %s is unknown.");
        gnc_error_dialog(parent, fmt, displayname);
        break;

    case ERR_FILEIO_BACKUP_ERROR:
        fmt = _("Could not make a backup of the file %s");
        gnc_error_dialog(parent, fmt, displayname);
        break;

    case ERR_FILEIO_WRITE_ERROR:
        fmt = _("Could not write to file %s. Check that you have "
                "permission to write to this file and that "
                "there is sufficient space to create it.");
        gnc_error_dialog(parent, fmt, displayname);
        break;

    case ERR_FILEIO_FILE_EACCES:
        fmt = _("No read permission to read from file %s.");
        gnc_error_dialog (parent, fmt, displayname);
        break;

    case ERR_FILEIO_RESERVED_WRITE:
        /* Translators: the first %s is a path in the filesystem,
         * the second %s is PACKAGE_NAME, which by default is "GnuCash"
         */
        fmt = _("You attempted to save in\n%s\nor a subdirectory thereof. "
                "This is not allowed as %s reserves that directory for internal use.\n\n"
                "Please try again in a different directory.");
        gnc_error_dialog (parent, fmt, gnc_userdata_dir(), PACKAGE_NAME);
        break;

    case ERR_SQL_DB_TOO_OLD:
        fmt = _("This database is from an older version of GnuCash. "
                "Select OK to upgrade it to the current version, Cancel "
                "to mark it read-only.");

        response = gnc_ok_cancel_dialog(parent, GTK_RESPONSE_CANCEL, "%s", fmt);
        uh_oh = (response == GTK_RESPONSE_CANCEL);
        break;

    case ERR_SQL_DB_TOO_NEW:
        fmt = _("This database is from a newer version of GnuCash. "
                "This version can read it, but cannot safely save to it. "
                "It will be marked read-only until you do File->Save As, "
                "but data may be lost in writing to the old version.");
        gnc_warning_dialog (parent, "%s", fmt);
        uh_oh = TRUE;
        break;

    case ERR_SQL_DB_BUSY:
        fmt = _("The SQL database is in use by other users, "
                "and the upgrade cannot be performed until they logoff. "
                "If there are currently no other users, consult the  "
                "documentation to learn how to clear out dangling login "
                "sessions.");
        gnc_error_dialog (parent, "%s", fmt);
        break;

    case ERR_SQL_BAD_DBI:

        fmt = _("The library \"libdbi\" installed on your system doesn't correctly "
                "store large numbers. This means GnuCash cannot use SQL databases "
                "correctly. Gnucash will not open or save to SQL databases until this is "
                "fixed by installing a different version of \"libdbi\". Please see "
                "https://bugs.gnucash.org/show_bug.cgi?id=611936 for more "
                "information.");

        gnc_error_dialog (parent, "%s", fmt);
        break;

    case ERR_SQL_DBI_UNTESTABLE:

        fmt = _("GnuCash could not complete a critical test for the presence of "
                "a bug in the \"libdbi\" library. This may be caused by a "
                "permissions misconfiguration of your SQL database. Please see "
                "https://bugs.gnucash.org/show_bug.cgi?id=645216 for more "
                "information.");

        gnc_error_dialog (parent, "%s", fmt);
        break;

    case ERR_FILEIO_FILE_UPGRADE:
        fmt = _("This file is from an older version of GnuCash and will be "
                "upgraded when saved by this version. You will not be able "
                "to read the saved file from the older version of Gnucash "
                "(it will report an \"error parsing the file\"). If you wish "
                "to preserve the old version, exit without saving.");
        gnc_warning_dialog (parent, "%s", fmt);
        uh_oh = FALSE;
        break;

    default:
        PERR("FIXME: Unhandled error %d", io_error);
        fmt = _("An unknown I/O error (%d) occurred.");
        gnc_error_dialog (parent, fmt, io_error);
        break;
    }

    g_free (displayname);
    return uh_oh;
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
}

static void
gnc_book_opened (void)
{
    gnc_hook_run(HOOK_BOOK_OPENED, gnc_get_current_session());
}

void
gnc_file_new (GtkWindow *parent)
{
    QofSession *session;

    /* If user attempts to start a new session before saving results of
     * the last one, prompt them to clean up their act. */
    if (!gnc_file_query_save (parent, TRUE))
        return;

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
}

gboolean
gnc_file_query_save (GtkWindow *parent, gboolean can_cancel)
{
    QofBook *current_book;

    if (!gnc_current_session_exist())
        return TRUE;

    current_book = qof_session_get_book (gnc_get_current_session ());
    /* Remove any pending auto-save timeouts */
    gnc_autosave_remove_timer(current_book);

    /* If user wants to mess around before finishing business with
     * the old file, give him a chance to figure out what's up.
     * Pose the question as a "while" loop, so that if user screws
     * up the file-selection dialog, we don't blow him out of the water;
     * instead, give them another chance to say "no" to the verify box.
     */
    while (qof_book_session_not_saved(current_book))
    {
        GtkWidget *dialog;
        gint response;
        const char *title = _("Save changes to the file?");
        /* This should be the same message as in gnc-main-window.c */
        time64 oldest_change;
        gint minutes;

        dialog = gtk_message_dialog_new(parent,
                                        GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_QUESTION,
                                        GTK_BUTTONS_NONE,
                                        "%s", title);
        oldest_change = qof_book_get_session_dirty_time(current_book);
        minutes = (gnc_time (NULL) - oldest_change) / 60 + 1;
        gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                ngettext("If you don't save, changes from the past %d minute will be discarded.",
                         "If you don't save, changes from the past %d minutes will be discarded.",
                         minutes), minutes);
        gtk_dialog_add_button(GTK_DIALOG(dialog),
                              _("Continue _Without Saving"), GTK_RESPONSE_OK);

        if (can_cancel)
            gtk_dialog_add_button(GTK_DIALOG(dialog),
                                  _("_Cancel"), GTK_RESPONSE_CANCEL);
        gtk_dialog_add_button(GTK_DIALOG(dialog),
                              _("_Save"), GTK_RESPONSE_YES);

        gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_YES);

        response = gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);

        switch (response)
        {
        case GTK_RESPONSE_YES:
            gnc_file_save (parent);
            /* Go check the loop condition. */
            break;

        case GTK_RESPONSE_CANCEL:
        default:
            if (can_cancel)
                return FALSE;
            /* No cancel function available. */
            /* Fall through */

        case GTK_RESPONSE_OK:
            return TRUE;
        }
    }

    return TRUE;
}


/* private utilities for file open; done in two stages */

#define RESPONSE_NEW  1
#define RESPONSE_OPEN 2
#define RESPONSE_QUIT 3
#define RESPONSE_READONLY 4

static gboolean
gnc_post_file_open (GtkWindow *parent, const char * filename, gboolean is_readonly)
{
    QofSession *current_session, *new_session;
    QofBook *new_book;
    GList *invalid_account_names;
    gboolean uh_oh = FALSE;
    char * newfile;
    QofBackendError io_err = ERR_BACKEND_NO_ERR;

    gchar *scheme   = NULL;
    gchar *hostname = NULL;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path = NULL;
    gint32 port = 0;


    ENTER("filename %s", filename);
RESTART:
    if (!filename || (*filename == '\0')) return FALSE;

    /* Convert user input into a normalized uri
     * Note that the normalized uri for internal use can have a password */
    newfile = gnc_uri_normalize_uri ( filename, TRUE );
    if (!newfile)
    {
        show_session_error (parent,
                            ERR_FILEIO_FILE_NOT_FOUND, filename,
                            GNC_FILE_DIALOG_OPEN);
        return FALSE;
    }

    gnc_uri_get_components (newfile, &scheme, &hostname,
                            &port, &username, &password, &path);

    /* If the file to open is a database, and no password was given,
     * attempt to look it up in a keyring. If that fails the keyring
     * function will ask the user to enter a password. The user can
     * cancel this dialog, in which case the open file action will be
     * abandoned.
     * Note newfile is normalized uri so we can safely call
     * gnc_uri_is_file_scheme on it.
     */
    if (!gnc_uri_is_file_scheme (scheme) && !password)
    {
        gboolean have_valid_pw = FALSE;
        have_valid_pw = gnc_keyring_get_password ( NULL, scheme, hostname, port,
                        path, &username, &password );
        if (!have_valid_pw)
            return FALSE;

        /* Got password. Recreate the uri to use internally. */
        g_free ( newfile );
        newfile = gnc_uri_create_uri ( scheme, hostname, port,
                                       username, password, path);
    }

    /* For file based uri's, remember the directory as the default. */
    if (gnc_uri_is_file_scheme(scheme))
    {
        gchar *default_dir = g_path_get_dirname(path);
        gnc_set_default_directory (GNC_PREFS_GROUP_OPEN_SAVE, default_dir);
        g_free(default_dir);
    }

    /* disable events while moving over to the new set of accounts;
     * the mass deletion of accounts and transactions during
     * switchover would otherwise cause excessive redraws. */
    qof_event_suspend ();

    /* Change the mouse to a busy cursor */
    gnc_set_busy_cursor (NULL, TRUE);

    /* -------------- BEGIN CORE SESSION CODE ------------- */
    /* -- this code is almost identical in FileOpen and FileSaveAs -- */
    if (gnc_current_session_exist())
    {
        current_session = gnc_get_current_session();
        gnc_hook_run(HOOK_BOOK_CLOSED, current_session);
        gnc_close_gui_component_by_session (current_session);
        gnc_state_save (current_session);
        gnc_clear_current_session();
    }

    /* load the accounts from the users datafile */
    /* but first, check to make sure we've got a session going. */
    new_session = qof_session_new ();

    // Begin the new session. If we are in read-only mode, ignore the locks.
    qof_session_begin (new_session, newfile, is_readonly, FALSE, FALSE);
    io_err = qof_session_get_error (new_session);

    if (ERR_BACKEND_BAD_URL == io_err)
    {
        gchar *directory;
        show_session_error (parent, io_err, newfile, GNC_FILE_DIALOG_OPEN);
        if (g_file_test (filename, G_FILE_TEST_IS_DIR))
            directory = g_strdup (filename);
        else
            directory = gnc_get_default_directory (GNC_PREFS_GROUP_OPEN_SAVE);

        filename = gnc_file_dialog (parent, NULL, NULL, directory,
                                    GNC_FILE_DIALOG_OPEN);
        qof_session_destroy (new_session);
        new_session = NULL;
        g_free (directory);
        goto RESTART;
    }
    /* if file appears to be locked, ask the user ... */
    else if (ERR_BACKEND_LOCKED == io_err || ERR_BACKEND_READONLY == io_err)
    {
        GtkWidget *dialog;
        gchar *displayname = NULL;

        char *fmt1 = _("GnuCash could not obtain the lock for %s.");
        char *fmt2 = ((ERR_BACKEND_LOCKED == io_err) ?
                      _("That database may be in use by another user, "
                        "in which case you should not open the database. "
                        "What would you like to do?") :
                      _("That database may be on a read-only file system, "
                        "you may not have write permission for the directory, "
                        "or your anti-virus software is preventing this action. "
                        "If you proceed you may not be able to save any changes. "
                        "What would you like to do?")
                     );
        int rc;

        /* Hide the db password and local filesystem schemes in error messages */
        if (!gnc_uri_is_file_uri (newfile))
            displayname = gnc_uri_normalize_uri ( newfile, FALSE);
        else
            displayname = gnc_uri_get_path (newfile);

        dialog = gtk_message_dialog_new(parent,
                                        0,
                                        GTK_MESSAGE_WARNING,
                                        GTK_BUTTONS_NONE,
                                        fmt1, displayname);
        gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                "%s", fmt2);
        gtk_window_set_skip_taskbar_hint(GTK_WINDOW(dialog), FALSE);

        gnc_gtk_dialog_add_button(dialog, _("_Open Read-Only"),
                                  "document-revert", RESPONSE_READONLY);
        gnc_gtk_dialog_add_button(dialog, _("_Create New File"),
                                  "document-new", RESPONSE_NEW);
        gnc_gtk_dialog_add_button(dialog, _("Open _Anyway"),
                                  "document-open", RESPONSE_OPEN);
        if (shutdown_cb)
            gtk_dialog_add_button(GTK_DIALOG(dialog),
                                  _("_Quit"), RESPONSE_QUIT);
        rc = gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);
        g_free (displayname);

        if (rc == GTK_RESPONSE_DELETE_EVENT)
        {
            rc = shutdown_cb ? RESPONSE_QUIT : RESPONSE_NEW;
        }
        switch (rc)
        {
        case RESPONSE_QUIT:
            if (shutdown_cb)
                shutdown_cb(0);
            g_assert(1);
            break;
        case RESPONSE_READONLY:
            is_readonly = TRUE;
            /* user told us to open readonly. We do ignore locks (just as before), but now also force the opening. */
            qof_session_begin (new_session, newfile, is_readonly, FALSE, TRUE);
            break;
        case RESPONSE_OPEN:
            /* user told us to ignore locks. So ignore them. */
            qof_session_begin (new_session, newfile, TRUE, FALSE, FALSE);
            break;
        default:
            /* Can't use the given file, so just create a new
             * database so that the user will get a window that
             * they can click "Exit" on.
             */
            gnc_file_new (parent);
            break;
        }
    }
    /* if the database doesn't exist, ask the user ... */
    else if ((ERR_BACKEND_NO_SUCH_DB == io_err))
    {
        if (!show_session_error (parent, io_err, newfile, GNC_FILE_DIALOG_OPEN))
        {
            /* user told us to create a new database. Do it. We
                     * shouldn't have to worry about locking or clobbering,
                     * it's supposed to be new. */
            qof_session_begin (new_session, newfile, FALSE, TRUE, FALSE);
        }
    }

    /* Check for errors again, since above may have cleared the lock.
     * If its still locked, still, doesn't exist, still too old, then
     * don't bother with the message, just die. */
    io_err = qof_session_get_error (new_session);
    if ((ERR_BACKEND_LOCKED == io_err) ||
            (ERR_BACKEND_READONLY == io_err) ||
            (ERR_BACKEND_NO_SUCH_DB == io_err))
    {
        uh_oh = TRUE;
    }

    else
    {
        uh_oh = show_session_error (parent, io_err, newfile, GNC_FILE_DIALOG_OPEN);
    }

    if (!uh_oh)
    {
        Account *new_root;

        /* If the new "file" is a database, attempt to store the password
         * in a keyring. GnuCash itself will not save it.
         */
        if ( !gnc_uri_is_file_scheme (scheme))
            gnc_keyring_set_password ( scheme, hostname, port,
                                       path, username, password );

        xaccLogDisable();
        gnc_window_show_progress(_("Loading user data..."), 0.0);
        qof_session_load (new_session, gnc_window_show_progress);
        gnc_window_show_progress(NULL, -1.0);
        xaccLogEnable();

        if (is_readonly)
        {
            // If the user chose "open read-only" above, make sure to have this
            // read-only here.
            qof_book_mark_readonly(qof_session_get_book(new_session));
        }

        /* check for i/o error, put up appropriate error dialog */
        io_err = qof_session_pop_error (new_session);

        if (io_err == ERR_FILEIO_NO_ENCODING)
        {
            if (gnc_xml_convert_single_file (newfile))
            {
                /* try to load once again */
                gnc_window_show_progress(_("Loading user data..."), 0.0);
                qof_session_load (new_session, gnc_window_show_progress);
                gnc_window_show_progress(NULL, -1.0);
                xaccLogEnable();
                io_err = qof_session_get_error (new_session);
            }
            else
            {
                io_err = ERR_FILEIO_PARSE_ERROR;
            }
        }

        uh_oh = show_session_error (parent, io_err, newfile, GNC_FILE_DIALOG_OPEN);
        /* Attempt to update the database if it's too old */
        if ( !uh_oh && io_err == ERR_SQL_DB_TOO_OLD )
        {
            gnc_window_show_progress(_("Re-saving user data..."), 0.0);
            qof_session_safe_save(new_session, gnc_window_show_progress);
            io_err = qof_session_get_error(new_session);
            uh_oh = show_session_error(parent, io_err, newfile, GNC_FILE_DIALOG_SAVE);
        }
        /* Database is either too old and couldn't (or user didn't
         * want it to) be updated or it's too new. Mark it as
         * read-only
         */
        if (uh_oh && (io_err == ERR_SQL_DB_TOO_OLD ||
                      io_err == ERR_SQL_DB_TOO_NEW))
        {
            qof_book_mark_readonly(qof_session_get_book(new_session));
            uh_oh = FALSE;
        }
        new_root = gnc_book_get_root_account (qof_session_get_book (new_session));
        if (uh_oh) new_root = NULL;

        /* Umm, came up empty-handed, but no error:
         * The backend forgot to set an error. So make one up. */
        if (!uh_oh && !new_root)
        {
            uh_oh = show_session_error (parent, ERR_BACKEND_MISC, newfile,
                                        GNC_FILE_DIALOG_OPEN);
        }

        /* test for unknown features. */
        if (!uh_oh)
        {
            QofBook *book = qof_session_get_book (new_session);
            gchar *msg = gnc_features_test_unknown (book);
            Account *template_root = gnc_book_get_template_root (book);

            if (msg)
            {
                uh_oh = TRUE;

                // XXX: should pull out the file name here */
                gnc_error_dialog (parent, msg, "");
                g_free (msg);
            }
            if (template_root != NULL)
            {
                GList *child = NULL;
                GList *children = gnc_account_get_descendants (template_root);

                for (child = children; child; child = g_list_next (child))
                {
                    Account *acc = GNC_ACCOUNT (child->data);
                    GList *splits = xaccAccountGetSplitList (acc);
                    g_list_foreach (splits,
                                    (GFunc)gnc_sx_scrub_split_numerics, NULL);
                }
                g_list_free (children);
            }
        }
    }

    g_free (scheme);
    g_free (hostname);
    g_free (username);
    g_free (password);
    g_free (path);

    gnc_unset_busy_cursor (NULL);

    /* going down -- abandon ship */
    if (uh_oh)
    {
        xaccLogDisable();
        qof_session_destroy (new_session);
        xaccLogEnable();

        /* well, no matter what, I think it's a good idea to have a root
         * account around.  For example, early in the gnucash startup
         * sequence, the user opens a file; if this open fails for any
         * reason, we don't want to leave them high & dry without a root
         * account, because if the user continues, then bad things will
         * happen. */
        gnc_get_current_session ();

        g_free (newfile);

        qof_event_resume ();
        gnc_gui_refresh_all ();

        return FALSE;
    }

    /* if we got to here, then we've successfully gotten a new session */
    /* close up the old file session (if any) */
    gnc_set_current_session(new_session);

    /* --------------- END CORE SESSION CODE -------------- */

    /* clean up old stuff, and then we're outta here. */
    gnc_add_history (new_session);

    g_free (newfile);

    qof_event_resume ();
    gnc_gui_refresh_all ();

    /* Call this after re-enabling events. */
    gnc_book_opened ();

    /* Check for account names that may contain the current separator character
     * and inform the user if there are any */
    new_book = gnc_get_current_book();
    invalid_account_names = gnc_account_list_name_violations ( new_book,
                            gnc_get_account_separator_string() );
    if ( invalid_account_names )
    {
        gchar *message = gnc_account_name_violations_errmsg ( gnc_get_account_separator_string(),
                         invalid_account_names );
        gnc_warning_dialog(parent, "%s", message);
        g_free ( message );
    }

    // Fix account color slots being set to 'Not Set', should run once on a book
    qof_event_suspend();
    xaccAccountScrubColorNotSet (gnc_get_current_book());
    qof_event_resume();

    return TRUE;
}

/* Routine that pops up a file chooser dialog
 *
 * Note: this dialog is used when dbi is not enabled
 *       so the paths used in here are always file
 *       paths, never db uris.
 */
gboolean
gnc_file_open (GtkWindow *parent)
{
    const gchar * newfile;
    gchar *last = NULL;
    gchar *default_dir = NULL;
    gboolean result;

    if (!gnc_file_query_save (parent, TRUE))
        return FALSE;

    if ( last && gnc_uri_targets_local_fs (last))
    {
        gchar *filepath = gnc_uri_get_path ( last );
        default_dir = g_path_get_dirname( filepath );
        g_free ( filepath );
    }
    else
        default_dir = gnc_get_default_directory(GNC_PREFS_GROUP_OPEN_SAVE);

    newfile = gnc_file_dialog (parent, _("Open"), NULL, default_dir, GNC_FILE_DIALOG_OPEN);
    g_free ( last );
    g_free ( default_dir );

    result = gnc_post_file_open (parent, newfile, /*is_readonly*/ FALSE );

    /* This dialogue can show up early in the startup process. If the
     * user fails to pick a file (by e.g. hitting the cancel button), we
     * might be left with a null topgroup, which leads to nastiness when
     * user goes to create their very first account. So create one. */
    gnc_get_current_session ();

    return result;
}

gboolean
gnc_file_open_file (GtkWindow *parent, const char * newfile, gboolean open_readonly)
{
    if (!newfile) return FALSE;

    if (!gnc_file_query_save (parent, TRUE))
        return FALSE;

    return gnc_post_file_open (parent, newfile, open_readonly);
}

/* Note: this dialog will only be used when dbi is not enabled
 *       paths used in it always refer to files and are
 *       never db uris
 */
void
gnc_file_export (GtkWindow *parent)
{
    const char *filename;
    char *default_dir = NULL;        /* Default to last open */
    char *last;

    ENTER(" ");

    last = gnc_history_get_last();
    if ( last && gnc_uri_targets_local_fs (last))
    {
        gchar *filepath = gnc_uri_get_path ( last );
        default_dir = g_path_get_dirname( filepath );
        g_free ( filepath );
    }
    else
        default_dir = gnc_get_default_directory(GNC_PREFS_GROUP_EXPORT);

    filename = gnc_file_dialog (parent,
                                _("Save"), NULL, default_dir,
                                GNC_FILE_DIALOG_SAVE);
    g_free ( last );
    g_free ( default_dir );
    if (!filename) return;

    gnc_file_do_export (parent, filename);

    LEAVE (" ");
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


void
gnc_file_do_export(GtkWindow *parent, const char * filename)
{
    QofSession *current_session, *new_session;
    gboolean ok;
    QofBackendError io_err = ERR_BACKEND_NO_ERR;
    gchar *norm_file;
    gchar *newfile;
    const gchar *oldfile;

    gchar *scheme   = NULL;
    gchar *hostname = NULL;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path = NULL;
    gint32 port = 0;

    ENTER(" ");

    /* Convert user input into a normalized uri
     * Note that the normalized uri for internal use can have a password */
    norm_file = gnc_uri_normalize_uri ( filename, TRUE );
    if (!norm_file)
    {
        show_session_error (parent, ERR_FILEIO_FILE_NOT_FOUND, filename,
                            GNC_FILE_DIALOG_EXPORT);
        return;
    }

    newfile = gnc_uri_add_extension (norm_file, GNC_DATAFILE_EXT);
    g_free (norm_file);
    gnc_uri_get_components (newfile, &scheme, &hostname,
                            &port, &username, &password, &path);

    /* Save As can't use the generic 'file' protocol. If the user didn't set
     * a specific protocol, assume the default 'xml'.
     */
    if (g_strcmp0 (scheme, "file") == 0)
    {
        g_free (scheme);
        scheme = g_strdup ("xml");
        norm_file = gnc_uri_create_uri (scheme, hostname, port,
                                        username, password, path);
        g_free (newfile);
        newfile = norm_file;
    }

    /* Some extra steps for file based uri's only
     * Note newfile is normalized uri so we can safely call
     * gnc_uri_is_file_scheme on it. */
    if (gnc_uri_is_file_scheme (scheme))
    {
        if (check_file_path (path))
        {
            show_session_error (parent, ERR_FILEIO_RESERVED_WRITE, newfile,
                    GNC_FILE_DIALOG_SAVE);
            return;
        }
        gnc_set_default_directory (GNC_PREFS_GROUP_OPEN_SAVE,
                       g_path_get_dirname(path));
    }
    /* Check to see if the user specified the same file as the current
     * file. If so, prevent the export from happening to avoid killing this file */
    current_session = gnc_get_current_session ();
    oldfile = qof_session_get_url(current_session);
    if (strlen (oldfile) && (strcmp(oldfile, newfile) == 0))
    {
        g_free (newfile);
        show_session_error (parent, ERR_FILEIO_WRITE_ERROR, filename,
                            GNC_FILE_DIALOG_EXPORT);
        return;
    }

    qof_event_suspend();

    /* -- this session code is NOT identical in FileOpen and FileSaveAs -- */

    new_session = qof_session_new ();
    qof_session_begin (new_session, newfile, FALSE, TRUE, FALSE);

    io_err = qof_session_get_error (new_session);
    /* If the file exists and would be clobbered, ask the user */
    if (ERR_BACKEND_STORE_EXISTS == io_err)
    {
        const char *format = _("The file %s already exists. "
                               "Are you sure you want to overwrite it?");

        const char *name;
        if ( gnc_uri_is_file_uri ( newfile ) )
            name = gnc_uri_get_path ( newfile );
        else
            name = gnc_uri_normalize_uri ( newfile, FALSE );
        /* if user says cancel, we should break out */
        if (!gnc_verify_dialog (parent, FALSE, format, name))
        {
            return;
        }
        qof_session_begin (new_session, newfile, FALSE, TRUE, TRUE);
    }
    /* if file appears to be locked, ask the user ... */
    if (ERR_BACKEND_LOCKED == io_err || ERR_BACKEND_READONLY == io_err)
    {
        if (!show_session_error (parent, io_err, newfile, GNC_FILE_DIALOG_EXPORT))
        {
            /* user told us to ignore locks. So ignore them. */
            qof_session_begin (new_session, newfile, TRUE, FALSE, FALSE);
        }
    }

    /* --------------- END CORE SESSION CODE -------------- */

    /* use the current session to save to file */
    gnc_set_busy_cursor (NULL, TRUE);
    gnc_window_show_progress(_("Exporting file..."), 0.0);
    ok = qof_session_export (new_session, current_session,
                             gnc_window_show_progress);
    gnc_window_show_progress(NULL, -1.0);
    gnc_unset_busy_cursor (NULL);
    xaccLogDisable();
    qof_session_destroy (new_session);
    xaccLogEnable();
    qof_event_resume();

    if (!ok)
    {
        /* %s is the strerror(3) error string of the error that occurred. */
        const char *format = _("There was an error saving the file.\n\n%s");

        gnc_error_dialog (parent, format, strerror(errno));
        return;
    }
}

static gboolean been_here_before = FALSE;

void
gnc_file_save (GtkWindow *parent)
{
    QofBackendError io_err;
    const char * newfile;
    QofSession *session;
    ENTER (" ");

    /* hack alert -- Somehow make sure all in-progress edits get committed! */

    /* If we don't have a filename/path to save to get one. */
    session = gnc_get_current_session ();

    if (!strlen (qof_session_get_url (session)))
    {
        gnc_file_save_as (parent);
        return;
    }

    if (qof_book_is_readonly(qof_session_get_book(session)))
    {
        gint response = gnc_ok_cancel_dialog(parent,
                                             GTK_RESPONSE_CANCEL,
                                             _("The database was opened read-only. "
                                               "Do you want to save it to a different place?"));
        if (response == GTK_RESPONSE_OK)
        {
            gnc_file_save_as (parent);
        }
        return;
    }

    /* use the current session to save to file */
    save_in_progress++;
    gnc_set_busy_cursor (NULL, TRUE);
    gnc_window_show_progress(_("Writing file..."), 0.0);
    qof_session_save (session, gnc_window_show_progress);
    gnc_window_show_progress(NULL, -1.0);
    gnc_unset_busy_cursor (NULL);
    save_in_progress--;

    /* Make sure everything's OK - disk could be full, file could have
       become read-only etc. */
    io_err = qof_session_get_error (session);
    if (ERR_BACKEND_NO_ERR != io_err)
    {
        newfile = qof_session_get_url(session);
        show_session_error (parent, io_err, newfile, GNC_FILE_DIALOG_SAVE);

        if (been_here_before) return;
        been_here_before = TRUE;
        gnc_file_save_as (parent);   /* been_here prevents infinite recursion */
        been_here_before = FALSE;
        return;
    }

    xaccReopenLog();
    gnc_add_history (session);
    gnc_hook_run(HOOK_BOOK_SAVED, session);
    LEAVE (" ");
}

/* Note: this dialog will only be used when dbi is not enabled
 *       paths used in it always refer to files and are
 *       never db uris. See gnc_file_do_save_as for that.
 */
void
gnc_file_save_as (GtkWindow *parent)
{
    const gchar *filename;
    gchar *default_dir = NULL;        /* Default to last open */
    gchar *last;

    ENTER(" ");

    last = gnc_history_get_last();
    if ( last && gnc_uri_targets_local_fs (last))
    {
        gchar *filepath = gnc_uri_get_path ( last );
        default_dir = g_path_get_dirname( filepath );
        g_free ( filepath );
    }
    else
        default_dir = gnc_get_default_directory(GNC_PREFS_GROUP_OPEN_SAVE);

    filename = gnc_file_dialog (parent,
                                _("Save"), NULL, default_dir,
                                GNC_FILE_DIALOG_SAVE);
    g_free ( last );
    g_free ( default_dir );
    if (!filename) return;

    gnc_file_do_save_as (parent, filename);

    LEAVE (" ");
}

void
gnc_file_do_save_as (GtkWindow *parent, const char* filename)
{
    QofSession *new_session;
    QofSession *session;
    gchar *norm_file;
    gchar *newfile;
    const gchar *oldfile;

    gchar *scheme   = NULL;
    gchar *hostname = NULL;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path = NULL;
    gint32 port = 0;


    QofBackendError io_err = ERR_BACKEND_NO_ERR;

    ENTER(" ");

    /* Convert user input into a normalized uri
     * Note that the normalized uri for internal use can have a password */
    norm_file = gnc_uri_normalize_uri ( filename, TRUE );
    if (!norm_file)
    {
        show_session_error (parent, ERR_FILEIO_FILE_NOT_FOUND, filename,
                            GNC_FILE_DIALOG_SAVE);
        return;
    }

    newfile = gnc_uri_add_extension (norm_file, GNC_DATAFILE_EXT);
    g_free (norm_file);
    gnc_uri_get_components (newfile, &scheme, &hostname,
                            &port, &username, &password, &path);

    /* Save As can't use the generic 'file' protocol. If the user didn't set
     * a specific protocol, assume the default 'xml'.
     */
    if (g_strcmp0 (scheme, "file") == 0)
    {
        g_free (scheme);
        scheme = g_strdup ("xml");
        norm_file = gnc_uri_create_uri (scheme, hostname, port,
                                        username, password, path);
        g_free (newfile);
        newfile = norm_file;
    }

    /* Some extra steps for file based uri's only
     * Note newfile is normalized uri so we can safely call
     * gnc_uri_is_file_scheme on it. */
    if (gnc_uri_is_file_scheme (scheme))
    {
        if (check_file_path (path))
        {
            show_session_error (parent, ERR_FILEIO_RESERVED_WRITE, newfile,
                    GNC_FILE_DIALOG_SAVE);
            return;
        }
        gnc_set_default_directory (GNC_PREFS_GROUP_OPEN_SAVE,
                       g_path_get_dirname (path));
    }

    /* Check to see if the user specified the same file as the current
     * file. If so, then just do a simple save, instead of a full save as */
    session = gnc_get_current_session ();
    oldfile = qof_session_get_url(session);
    if (strlen (oldfile) && (strcmp(oldfile, newfile) == 0))
    {
        g_free (newfile);
        gnc_file_save (parent);
        return;
    }

    /* Make sure all of the data from the old file is loaded */
    qof_session_ensure_all_data_loaded(session);

    /* -- this session code is NOT identical in FileOpen and FileSaveAs -- */

    save_in_progress++;

    new_session = qof_session_new ();
    qof_session_begin (new_session, newfile, FALSE, TRUE, FALSE);

    io_err = qof_session_get_error (new_session);

    /* If the file exists and would be clobbered, ask the user */
    if (ERR_BACKEND_STORE_EXISTS == io_err)
    {
        const char *format = _("The file %s already exists. "
                               "Are you sure you want to overwrite it?");

        const char *name;
        if ( gnc_uri_is_file_uri ( newfile ) )
            name = gnc_uri_get_path ( newfile );
        else
            name = gnc_uri_normalize_uri ( newfile, FALSE );

        /* if user says cancel, we should break out */
        if (!gnc_verify_dialog (parent, FALSE, format, name ))
        {
            xaccLogDisable();
            qof_session_destroy (new_session);
            xaccLogEnable();
            g_free (newfile);
            save_in_progress--;
            return;
        }
        qof_session_begin (new_session, newfile, FALSE, TRUE, TRUE);
    }
    /* if file appears to be locked, ask the user ... */
    else if (ERR_BACKEND_LOCKED == io_err || ERR_BACKEND_READONLY == io_err)
    {
        if (!show_session_error (parent, io_err, newfile, GNC_FILE_DIALOG_SAVE))
        {
            /* user told us to ignore locks. So ignore them. */
            qof_session_begin (new_session, newfile, TRUE, FALSE, FALSE);
        }
    }

    /* if the database doesn't exist, ask the user ... */
    else if ((ERR_FILEIO_FILE_NOT_FOUND == io_err) ||
             (ERR_BACKEND_NO_SUCH_DB == io_err) ||
             (ERR_SQL_DB_TOO_OLD == io_err))
    {
        if (!show_session_error (parent, io_err, newfile, GNC_FILE_DIALOG_SAVE))
        {
            /* user told us to create a new database. Do it. */
            qof_session_begin (new_session, newfile, FALSE, TRUE, FALSE);
        }
    }

    /* check again for session errors (since above dialog may have
     * cleared a file lock & moved things forward some more)
     * This time, errors will be fatal.
     */
    io_err = qof_session_get_error (new_session);
    if (ERR_BACKEND_NO_ERR != io_err)
    {
        show_session_error (parent, io_err, newfile, GNC_FILE_DIALOG_SAVE);
        xaccLogDisable();
        qof_session_destroy (new_session);
        xaccLogEnable();
        g_free (newfile);
        save_in_progress--;
        return;
    }

    /* If the new "file" is a database, attempt to store the password
     * in a keyring. GnuCash itself will not save it.
     */
    if ( !gnc_uri_is_file_scheme (scheme))
        gnc_keyring_set_password ( scheme, hostname, port,
                                   path, username, password );

    /* Prevent race condition between swapping the contents of the two
     * sessions, and actually installing the new session as the current
     * one. Any event callbacks that occur in this interval will have
     * problems if they check for the current book. */
    qof_event_suspend();

    /* if we got to here, then we've successfully gotten a new session */
    /* close up the old file session (if any) */
    qof_session_swap_data (session, new_session);
    qof_book_mark_session_dirty (qof_session_get_book (new_session));

    qof_event_resume();


    gnc_set_busy_cursor (NULL, TRUE);
    gnc_window_show_progress(_("Writing file..."), 0.0);
    qof_session_save (new_session, gnc_window_show_progress);
    gnc_window_show_progress(NULL, -1.0);
    gnc_unset_busy_cursor (NULL);

    io_err = qof_session_get_error( new_session );
    if ( ERR_BACKEND_NO_ERR != io_err )
    {
        /* Well, poop. The save failed, so the new session is invalid and we
         * need to restore the old one.
         */
        show_session_error (parent, io_err, newfile, GNC_FILE_DIALOG_SAVE);
        qof_event_suspend();
        qof_session_swap_data( new_session, session );
        qof_session_destroy( new_session );
        new_session = NULL;
        qof_event_resume();
    }
    else
    {
        /* Yay! Save was successful, we can dump the old session */
        qof_event_suspend();
        gnc_clear_current_session();
        gnc_set_current_session( new_session );
        qof_event_resume();
        session = NULL;

        xaccReopenLog();
        gnc_add_history (new_session);
        gnc_hook_run(HOOK_BOOK_SAVED, new_session);
    }
    /* --------------- END CORE SESSION CODE -------------- */

    save_in_progress--;

    g_free (newfile);
    LEAVE (" ");
}

void
gnc_file_revert (GtkWindow *parent)
{
    QofSession *session;
    const gchar *fileurl, *filename, *tmp;
    const gchar *title = _("Reverting will discard all unsaved changes to %s. Are you sure you want to proceed ?");

    if (!gnc_main_window_all_finish_pending())
        return;

    session = gnc_get_current_session();
    fileurl = qof_session_get_url(session);
    if (!strlen (fileurl))
        fileurl = _("<unknown>");
    if ((tmp = strrchr(fileurl, '/')) != NULL)
        filename = tmp + 1;
    else
        filename = fileurl;

    if (!gnc_verify_dialog (parent, FALSE, title, filename))
        return;

    qof_book_mark_session_saved (qof_session_get_book (session));
    gnc_file_open_file (parent, fileurl, qof_book_is_readonly(gnc_get_current_book()));}

void
gnc_file_quit (void)
{
    QofSession *session;

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
    if (gnc_current_session_exist())
    {
        QofSession *session = gnc_get_current_session();
        return (qof_session_save_in_progress(session) || save_in_progress > 0);
    }
    return FALSE;
}
