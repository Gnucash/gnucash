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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <errno.h>
#include <libguile.h>
#include <string.h>

#include "dialog-utils.h"
#include "druid-gnc-xml-import.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "Account.h"
#include "gnc-file.h"
#include "gnc-gui-query.h"
#include "gnc-hooks.h"
#include "gnc-splash.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-uri-utils.h"
#include "gnc-window.h"
#include "gnc-plugin-file-history.h"
#include "qof.h"
#include "TransLog.h"
#include "gnc-session.h"
#include "gnc-autosave.h"

#define GCONF_SECTION "dialogs/export_accounts"

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
gnc_file_dialog (const char * title,
                 GList * filters,
                 const char * starting_dir,
                 GNCFileDialogType type
                )
{
    GtkWidget *file_box;
    const char *internal_name;
    char *file_name = NULL;
    gchar * okbutton = GTK_STOCK_OPEN;
    const gchar *ok_icon = NULL;
    GtkFileChooserAction action = GTK_FILE_CHOOSER_ACTION_OPEN;
    gint response;

    ENTER(" ");

    switch (type)
    {
    case GNC_FILE_DIALOG_OPEN:
        action = GTK_FILE_CHOOSER_ACTION_OPEN;
        okbutton = GTK_STOCK_OPEN;
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
        okbutton = GTK_STOCK_SAVE;
        if (title == NULL)
            title = _("Save");
        break;
    case GNC_FILE_DIALOG_EXPORT:
        action = GTK_FILE_CHOOSER_ACTION_SAVE;
        okbutton = _("_Export");
        ok_icon = GTK_STOCK_CONVERT;
        if (title == NULL)
            title = _("Export");
        break;

    }

    file_box = gtk_file_chooser_dialog_new(
                   title,
                   NULL,
                   action,
                   GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
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
    /*
    gtk_window_set_transient_for(GTK_WINDOW(file_box),
    		       GTK_WINDOW(gnc_ui_get_toplevel()));
    */

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

    if (response == GTK_RESPONSE_ACCEPT)
    {
        /* look for constructs like postgres://foo */
        internal_name = gtk_file_chooser_get_uri(GTK_FILE_CHOOSER (file_box));
        if (strstr (internal_name, "file://") == internal_name)
        {
            /* nope, a local file name */
            internal_name = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER (file_box));
        }
        file_name = g_strdup(internal_name);
    }
    gtk_widget_destroy(GTK_WIDGET(file_box));
    LEAVE("%s", file_name ? file_name : "(null)");
    return file_name;
}


gboolean
show_session_error (QofBackendError io_error,
                    const char *newfile,
                    GNCFileDialogType type)
{
    GtkWidget *parent = gnc_ui_get_toplevel();
    GtkWidget *dialog;
    gboolean uh_oh = TRUE;
    const char *fmt, *label;
    gint response;

    if (NULL == newfile)
    {
        newfile = _("(null)");
    }

    switch (io_error)
    {
    case ERR_BACKEND_NO_ERR:
        uh_oh = FALSE;
        break;

    case ERR_BACKEND_NO_HANDLER:
        fmt = _("No suitable backend was found for %s.");
        gnc_error_dialog(parent, fmt, newfile);
        break;

    case ERR_BACKEND_NO_BACKEND:
        fmt = _("The URL %s is not supported by this version of GnuCash.");
        gnc_error_dialog (parent, fmt, newfile);
        break;

    case ERR_BACKEND_BAD_URL:
        fmt = _("Can't parse the URL %s.");
        gnc_error_dialog (parent, fmt, newfile);
        break;

    case ERR_BACKEND_CANT_CONNECT:
        fmt = _("Can't connect to %s. "
                "The host, username or password were incorrect.");
        gnc_error_dialog (parent, fmt, newfile);
        break;

    case ERR_BACKEND_CONN_LOST:
        fmt = _("Can't connect to %s. "
                "Connection was lost, unable to send data.");
        gnc_error_dialog (parent, fmt, newfile);
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
        if (gnc_verify_dialog (parent, TRUE, fmt, newfile))
        {
            uh_oh = FALSE;
        }
        break;

    case ERR_BACKEND_LOCKED:
        switch (type)
        {
        case GNC_FILE_DIALOG_OPEN:
        default:
            label = GTK_STOCK_OPEN;
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
            label = GTK_STOCK_SAVE;
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

        dialog = gtk_message_dialog_new(GTK_WINDOW(parent),
                                        GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_QUESTION,
                                        GTK_BUTTONS_NONE,
                                        fmt,
                                        newfile);
        gtk_dialog_add_buttons(GTK_DIALOG(dialog),
                               GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                               label, GTK_RESPONSE_YES,
                               NULL);
        response = gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);
        uh_oh = (response != GTK_RESPONSE_YES);
        break;

    case ERR_BACKEND_READONLY:
        fmt = _("GnuCash could not write to %s. "
                "That database may be on a read-only file system, "
                "or you may not have write permission for the directory.");
        gnc_error_dialog (parent, fmt, newfile);
        break;

    case ERR_BACKEND_DATA_CORRUPT:
        fmt = _("The file/URL %s "
                "does not contain GnuCash data or the data is corrupt.");
        gnc_error_dialog (parent, fmt, newfile);
        break;

    case ERR_BACKEND_SERVER_ERR:
        fmt = _("The server at URL %s "
                "experienced an error or encountered bad or corrupt data.");
        gnc_error_dialog (parent, fmt, newfile);
        break;

    case ERR_BACKEND_PERM:
        fmt = _("You do not have permission to access %s.");
        gnc_error_dialog (parent, fmt, newfile);
        break;

    case ERR_BACKEND_MISC:
        fmt = _("An error occurred while processing %s.");
        gnc_error_dialog (parent, fmt, newfile);
        break;

        /* QSF additions */
    case ERR_QSF_INVALID_OBJ:
        fmt = _("Invalid QSF Object file! The QSF object file %s failed to"
                " validate against the QSF object schema. The XML structure of"
                " the file is either not well-formed or contains illegal data.");
        gnc_error_dialog(parent, fmt, newfile);
        break;

    case ERR_QSF_INVALID_MAP:
        fmt = _("Invalid QSF Map file! The QSF map file %s failed to validate"
                " against the QSF map schema. The XML structure of the file"
                " is either not well-formed or contains illegal data.");
        gnc_error_dialog(parent, fmt, newfile);
        break;

    case ERR_QSF_BAD_QOF_VERSION:
        fmt = _("The QSF Map file %s was written for a different version of"
                " QOF.  It may need to be modified to work with your current"
                " QOF installation.");
        gnc_error_dialog(parent, fmt, newfile);
        break;

    case ERR_QSF_BAD_MAP:
        fmt = _("The selected QSF map %s contains unusable data. "
                "This is usually because not all the required parameters for "
                "the defined objects have calculations described in the map.");
        gnc_error_dialog(parent, fmt, newfile);
        break;

    case ERR_QSF_BAD_OBJ_GUID:
        fmt = _("The selected QSF object file %s contains one or more invalid "
                "GUIDs. The file cannot be processed - please check the source "
                "of the file and try again.");
        gnc_error_dialog(parent, fmt, newfile);
        break;

    case ERR_QSF_NO_MAP:
        fmt = _("The selected QSF Object file %s requires a map but it was "
                "not provided.");
        gnc_error_dialog(parent, fmt, newfile);
        break;

    case ERR_QSF_WRONG_MAP:
        fmt = _("Wrong QSF map selected. The selected map %s validates but was "
                "written for different QOF objects.  The list of objects defined "
                "in this map does not include all the objects described in "
                "the current QSF object file.");
        gnc_error_dialog(parent, fmt, newfile);
        break;

    case ERR_QSF_MAP_NOT_OBJ:
        fmt = _("The selected file %s is a QSF map and cannot be "
                "opened as a QSF object.");
        gnc_error_dialog(parent, fmt, newfile);
        break;

    case ERR_QSF_OVERFLOW:
        fmt = _("When converting XML strings into numbers, an overflow "
                "has been detected. The QSF object file %s contains invalid "
                "data in a field that is meant to hold a number.");
        gnc_error_dialog(parent, fmt, newfile);
        break;

    case ERR_QSF_OPEN_NOT_MERGE:
        fmt = _("The QSF object file %s is valid and contains GnuCash "
                "objects. However, GnuCash cannot open the file directly because "
                "the data needs to be merged into an existing GnuCash data book. "
                "Please open a GnuCash file or create a new one, then import "
                "this QSF object file so that the data can be merged into the "
                "main data book.");
        gnc_error_dialog(parent, fmt, newfile);
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
        gnc_error_dialog (parent, fmt, newfile);
        break;

    case ERR_FILEIO_FILE_EMPTY:
        fmt = _("The file %s is empty.");
        gnc_error_dialog (parent, fmt, newfile);
        break;

    case ERR_FILEIO_FILE_NOT_FOUND:
        if (type == GNC_FILE_DIALOG_SAVE)
        {
            uh_oh = FALSE;
        }
        else
        {
            fmt = _("The file %s could not be found.");
            gnc_error_dialog (parent, fmt, newfile);
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
        gnc_error_dialog(parent, fmt, newfile);
        break;

    case ERR_FILEIO_BACKUP_ERROR:
        fmt = _("Could not make a backup of the file %s");
        gnc_error_dialog(parent, fmt, newfile);
        break;

    case ERR_FILEIO_WRITE_ERROR:
        fmt = _("Could not write to file %s.  Check that you have "
                "permission to write to this file and that "
                "there is sufficient space to create it.");
        gnc_error_dialog(parent, fmt, newfile);
        break;

    case ERR_FILEIO_FILE_EACCES:
        fmt = _("No read permission to read from file %s.");
        gnc_error_dialog (parent, fmt, newfile);
        break;

    case ERR_SQL_DB_TOO_OLD:
        fmt = _("This database is from an older version of GnuCash. "
                "Do you want to want to upgrade the database "
                "to the current version?");
        if (gnc_verify_dialog (parent, TRUE, "%s", fmt))
        {
            uh_oh = FALSE;
        }
        break;

    case ERR_SQL_DB_BUSY:
        fmt = _("The SQL database is in use by other users, "
                "and the upgrade cannot be performed until they logoff. "
                "If there are currently no other users, consult the  "
                "documentation to learn how to clear out dangling login "
                "sessions.");
        gnc_error_dialog (parent, "%s", fmt);
        break;

    default:
        PERR("FIXME: Unhandled error %d", io_error);
        fmt = _("An unknown I/O error (%d) occurred.");
        gnc_error_dialog (parent, fmt, io_error);
        break;
    }

    return uh_oh;
}

static void
gnc_add_history (QofSession * session)
{
    const gchar *url;
    char *file;

    if (!session) return;

    url = qof_session_get_url ( session );
    if ( !url )
        return;

    if ( gnc_uri_is_file_uri ( url ) )
        file = gnc_uri_get_path ( url );
    else
        file = gnc_uri_normalize_uri ( url, TRUE ); /* FIXME this saves the password visibly in history ! */

    gnc_history_add_file (file);
}

static void
gnc_book_opened (void)
{
    gnc_hook_run(HOOK_BOOK_OPENED, gnc_get_current_session());
}

void
gnc_file_new (void)
{
    QofSession *session;

    /* If user attempts to start a new session before saving results of
     * the last one, prompt them to clean up their act. */
    if (!gnc_file_query_save (TRUE))
        return;

    if (gnc_current_session_exist())
    {
        session = gnc_get_current_session ();

        /* close any ongoing file sessions, and free the accounts.
         * disable events so we don't get spammed by redraws. */
        qof_event_suspend ();

        qof_session_call_close_hooks(session);
        gnc_hook_run(HOOK_BOOK_CLOSED, session);

        gnc_close_gui_component_by_session (session);
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
gnc_file_query_save (gboolean can_cancel)
{
    GtkWidget *parent = gnc_ui_get_toplevel();
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
    while (qof_book_not_saved(current_book))
    {
        GtkWidget *dialog;
        gint response;
        const char *title = _("Save changes to the file?");
        /* This should be the same message as in gnc-main-window.c */
        const gchar *message =
            _("If you don't save, changes from the past %d minutes will be discarded.");
        time_t oldest_change;
        gint minutes;

        dialog = gtk_message_dialog_new(GTK_WINDOW(parent),
                                        GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_QUESTION,
                                        GTK_BUTTONS_NONE,
                                        "%s", title);
        oldest_change = qof_book_get_dirty_time(current_book);
        minutes = (time(NULL) - oldest_change) / 60 + 1;
        gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                message, minutes);
        gtk_dialog_add_button(GTK_DIALOG(dialog),
                              _("Continue _Without Saving"), GTK_RESPONSE_OK);

        if (can_cancel)
            gtk_dialog_add_button(GTK_DIALOG(dialog),
                                  GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
        gtk_dialog_add_button(GTK_DIALOG(dialog),
                              GTK_STOCK_SAVE, GTK_RESPONSE_YES);
        response = gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);

        switch (response)
        {
        case GTK_RESPONSE_YES:
            gnc_file_save ();
            /* Go check the loop condition. */
            break;

        case GTK_RESPONSE_CANCEL:
        default:
            if (can_cancel)
                return FALSE;
            /* No cancel function available.  Fall through. */

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

static gboolean
gnc_post_file_open (const char * filename)
{
    QofSession *current_session, *new_session;
    QofBook *new_book;
    GList *invalid_account_names;
    gboolean uh_oh = FALSE;
    char * newfile;
    QofBackendError io_err = ERR_BACKEND_NO_ERR;

    if (!filename) return FALSE;

    /* FIXME Verify if it is ok that a password is stored
     * in the uri here.
     */
    newfile = gnc_uri_normalize_uri ( filename, TRUE );
    if (!newfile)
    {
        show_session_error (ERR_FILEIO_FILE_NOT_FOUND, filename,
                            GNC_FILE_DIALOG_OPEN);
        return FALSE;
    }

    /* disable events while moving over to the new set of accounts;
     * the mass deletion of accounts and transactions during
     * switchover would otherwise cause excessive redraws. */
    qof_event_suspend ();

    /* Change the mouse to a busy cursor */
    gnc_set_busy_cursor (NULL, TRUE);

    /* -------------- BEGIN CORE SESSION CODE ------------- */
    /* -- this code is almost identical in FileOpen and FileSaveAs -- */
    current_session = gnc_get_current_session();
    qof_session_call_close_hooks(current_session);
    gnc_hook_run(HOOK_BOOK_CLOSED, current_session);
    gnc_close_gui_component_by_session (current_session);
    gnc_clear_current_session();

    /* load the accounts from the users datafile */
    /* but first, check to make sure we've got a session going. */
    new_session = qof_session_new ();

    qof_session_begin (new_session, newfile, FALSE, FALSE);
    io_err = qof_session_get_error (new_session);
    /* if file appears to be locked, ask the user ... */
    if (ERR_BACKEND_LOCKED == io_err || ERR_BACKEND_READONLY == io_err)
    {
        GtkWidget *dialog;
        char *fmt1 = _("GnuCash could not obtain the lock for %s.");
        char *fmt2 = ((ERR_BACKEND_LOCKED == io_err) ?
                      _("That database may be in use by another user, "
                        "in which case you should not open the database. "
                        "What would you like to do?") :
                      _("That database may be on a read-only file system, "
                        "or you may not have write permission for the directory. "
                        "If you proceed you may not be able to save any changes. "
                        "What would you like to do?")
                     );
        int rc;

        // Bug#467521: on Mac (and maybe Win?), the dialog will appear below the
        // splash, but is modal, so we can't get rid of the splash...  So, get
        // rid of it now.
        gnc_destroy_splash_screen();

        dialog = gtk_message_dialog_new(NULL,
                                        0,
                                        GTK_MESSAGE_WARNING,
                                        GTK_BUTTONS_NONE,
                                        fmt1, newfile);
        gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                "%s", fmt2);

        gnc_gtk_dialog_add_button(dialog, _("_Open Anyway"),
                                  GTK_STOCK_OPEN, RESPONSE_OPEN);
        gnc_gtk_dialog_add_button(dialog, _("_Create New File"),
                                  GTK_STOCK_NEW, RESPONSE_NEW);
        if (shutdown_cb)
            gtk_dialog_add_button(GTK_DIALOG(dialog),
                                  GTK_STOCK_QUIT, RESPONSE_QUIT);
        rc = gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);

        if (rc == GTK_RESPONSE_DELETE_EVENT)
        {
            rc = shutdown_cb ? RESPONSE_QUIT : RESPONSE_NEW;
        }
        if (rc == RESPONSE_QUIT)
        {
            if (shutdown_cb)
                shutdown_cb(0);
            g_assert(1);
        }
        else if (rc == RESPONSE_OPEN)
        {
            /* user told us to ignore locks. So ignore them. */
            qof_session_begin (new_session, newfile, TRUE, FALSE);
        }
        else
        {
            /* Can't use the given file, so just create a new
             * database so that the user will get a window that
             * they can click "Exit" on.
             */
            gnc_file_new ();
        }
    }
    if (ERR_QSF_OPEN_NOT_MERGE == io_err)
    {
        uh_oh = TRUE;
    }
    /* if the database doesn't exist, ask the user ... */
    else if ((ERR_BACKEND_NO_SUCH_DB == io_err) ||
             (ERR_SQL_DB_TOO_OLD == io_err))
    {
        if (FALSE == show_session_error (io_err, newfile, GNC_FILE_DIALOG_OPEN))
        {
            /* user told us to create a new database. Do it. */
            qof_session_begin (new_session, newfile, FALSE, TRUE);
        }
    }

    /* Check for errors again, since above may have cleared the lock.
     * If its still locked, still, doesn't exist, still too old, then
     * don't bother with the message, just die. */
    io_err = qof_session_get_error (new_session);
    if ((ERR_BACKEND_LOCKED == io_err) ||
            (ERR_BACKEND_READONLY == io_err) ||
            (ERR_BACKEND_NO_SUCH_DB == io_err) ||
            (ERR_SQL_DB_TOO_OLD == io_err))
    {
        uh_oh = TRUE;
    }
    else
    {
        uh_oh = show_session_error (io_err, newfile, GNC_FILE_DIALOG_OPEN);
    }

    if (!uh_oh)
    {
        Account *new_root;
        gchar *logpath = NULL;

        /* XXX Would logging make sense for databases as well (mysql/postgres) ?
         * Currently the logpath is relative to the data file path.
         * Databases don't have a file path, so no logging will be
         * done for them in the current setup.
         */
        if ( gnc_uri_is_file_uri ( newfile ) )
            logpath = gnc_uri_get_path(newfile);
        PINFO ("logpath=%s", logpath ? logpath : "(null)");
        xaccLogSetBaseName (logpath);
        g_free ( logpath );

        xaccLogDisable();
        gnc_window_show_progress(_("Loading user data..."), 0.0);
        qof_session_load (new_session, gnc_window_show_progress);
        gnc_window_show_progress(NULL, -1.0);
        xaccLogEnable();

        /* check for i/o error, put up appropriate error dialog */
        io_err = qof_session_get_error (new_session);

        if (io_err == ERR_FILEIO_NO_ENCODING)
        {
            qof_session_pop_error (new_session);
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

        uh_oh = show_session_error (io_err, newfile, GNC_FILE_DIALOG_OPEN);

        new_root = gnc_book_get_root_account (qof_session_get_book (new_session));
        if (uh_oh) new_root = NULL;

        /* Umm, came up empty-handed, but no error:
         * The backend forgot to set an error. So make one up. */
        if (!uh_oh && !new_root)
        {
            uh_oh = show_session_error (ERR_BACKEND_MISC, newfile,
                                        GNC_FILE_DIALOG_OPEN);
        }
    }

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
        gnc_warning_dialog(NULL, "%s", message);
        g_free ( message );
    }

    return TRUE;
}

/* Routine that pops up a file chooser dialog
 *
 * Note: this dialog is used when dbi is not enabled
 *       so the paths used in here are always file
 *       paths, never db uris.
 */
gboolean
gnc_file_open (void)
{
    const char * newfile;
    gchar *lastpath = NULL;
    gchar *lastfile = NULL;
    gchar *last_file_dir = NULL;
    gboolean result;

    if (!gnc_file_query_save (TRUE))
        return FALSE;

    lastpath = gnc_history_get_last();
    lastfile = gnc_uri_get_path ( lastpath );
    if ( lastfile )
        last_file_dir = g_path_get_dirname(lastfile);
    newfile = gnc_file_dialog (_("Open"), NULL, last_file_dir, GNC_FILE_DIALOG_OPEN);
    g_free ( lastpath );
    g_free ( lastfile );
    g_free ( last_file_dir );

    result = gnc_post_file_open ( newfile );

    /* This dialogue can show up early in the startup process. If the
     * user fails to pick a file (by e.g. hitting the cancel button), we
     * might be left with a null topgroup, which leads to nastiness when
     * user goes to create their very first account. So create one. */
    gnc_get_current_session ();

    return result;
}

gboolean
gnc_file_open_file (const char * newfile)
{
    if (!newfile) return FALSE;

    if (!gnc_file_query_save (TRUE))
        return FALSE;

    return gnc_post_file_open (newfile);
}

void
gnc_file_export_file(const char * newfile)
{
    QofSession *current_session, *new_session;
    gboolean ok;
    QofBackendError io_err = ERR_BACKEND_NO_ERR;
    gchar *default_dir;

    if (!newfile)
    {
        default_dir = gnc_get_default_directory (GCONF_SECTION);
        newfile =  gnc_file_dialog (_("Export"), NULL, default_dir, GNC_FILE_DIALOG_EXPORT);
        g_free(default_dir);
        if (!newfile)
            return;
    }

    /* Remember the directory as the default. */
    default_dir = g_path_get_dirname(newfile);
    gnc_set_default_directory (GCONF_SECTION, default_dir);
    g_free(default_dir);

    qof_event_suspend();

    /* -- this session code is NOT identical in FileOpen and FileSaveAs -- */

    new_session = qof_session_new ();
    qof_session_begin (new_session, newfile, FALSE, TRUE);

    io_err = qof_session_get_error (new_session);

    /* if file appears to be locked, ask the user ... */
    if (ERR_BACKEND_LOCKED == io_err || ERR_BACKEND_READONLY == io_err)
    {
        if (FALSE == show_session_error (io_err, newfile, GNC_FILE_DIALOG_EXPORT))
        {
            /* user told us to ignore locks. So ignore them. */
            qof_session_begin (new_session, newfile, TRUE, FALSE);
        }
    }

    /* --------------- END CORE SESSION CODE -------------- */

    /* oops ... file already exists ... ask user what to do... */
    if (qof_session_save_may_clobber_data (new_session))
    {
        const char *format = _("The file %s already exists. "
                               "Are you sure you want to overwrite it?");

        /* if user says cancel, we should break out */
        if (!gnc_verify_dialog (NULL, FALSE, format, newfile))
        {
            return;
        }

        /* Whoa-ok. Blow away the previous file. */
    }

    /* use the current session to save to file */
    gnc_set_busy_cursor (NULL, TRUE);
    current_session = gnc_get_current_session();
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

        gnc_error_dialog (NULL, format, strerror(errno));
        return;
    }
}

static gboolean been_here_before = FALSE;

void
gnc_file_save (void)
{
    QofBackendError io_err;
    const char * newfile;
    QofSession *session;
    ENTER (" ");

    /* hack alert -- Somehow make sure all in-progress edits get committed! */

    /* If we don't have a filename/path to save to get one. */
    session = gnc_get_current_session ();

    if (!qof_session_get_url(session))
    {
        gnc_file_save_as ();
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
        show_session_error (io_err, newfile, GNC_FILE_DIALOG_SAVE);

        if (been_here_before) return;
        been_here_before = TRUE;
        gnc_file_save_as ();   /* been_here prevents infinite recursion */
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
 *       never db uris
 */
void
gnc_file_save_as (void)
{
    QofSession *new_session;
    QofSession *session;
    const char *filename;
    char *default_dir = NULL;        /* Default to last open */
    char *last;
    char *newfile;
    const char *oldfile;
    QofBackendError io_err = ERR_BACKEND_NO_ERR;

    ENTER(" ");

    last = gnc_history_get_last();
    if ( last && gnc_uri_is_file_uri ( last ) )
    {
        gchar *filepath = gnc_uri_get_path ( last );
        default_dir = g_path_get_dirname( filepath );
        g_free ( filepath );
    }
    else
    {
        default_dir = gnc_get_default_directory(GCONF_SECTION);
    }
    filename = gnc_file_dialog (_("Save"), NULL, default_dir,
                                GNC_FILE_DIALOG_SAVE);
    g_free ( last );
    g_free ( default_dir );
    if (!filename) return;

    gnc_file_do_save_as( filename );

    LEAVE (" ");
}

void
gnc_file_do_save_as (const char* filename)
{
    QofSession *new_session;
    QofSession *session;
    char *default_dir = NULL;        /* Default to last open */
    char *last;
    char *newfile;
    const char *oldfile;
    gchar *logpath = NULL;

    QofBackendError io_err = ERR_BACKEND_NO_ERR;

    ENTER(" ");

    /* Check to see if the user specified the same file as the current
     * file. If so, then just do a simple save, instead of a full save as */
    /* FIXME Check if it is ok to have a password in the uri here */
    newfile = gnc_uri_normalize_uri ( filename, TRUE );
    if (!newfile)
    {
        show_session_error (ERR_FILEIO_FILE_NOT_FOUND, filename,
                            GNC_FILE_DIALOG_SAVE);
        return;
    }

    session = gnc_get_current_session ();
    oldfile = qof_session_get_url(session);
    if (oldfile && (strcmp(oldfile, newfile) == 0))
    {
        g_free (newfile);
        gnc_file_save ();
        return;
    }

    /* Make sure all of the data from the old file is loaded */
    qof_session_ensure_all_data_loaded(session);

    /* -- this session code is NOT identical in FileOpen and FileSaveAs -- */

    save_in_progress++;
    new_session = qof_session_new ();
    qof_session_begin (new_session, newfile, FALSE, FALSE);

    io_err = qof_session_get_error (new_session);

    /* if file appears to be locked, ask the user ... */
    if (ERR_BACKEND_LOCKED == io_err || ERR_BACKEND_READONLY == io_err)
    {
        if (FALSE == show_session_error (io_err, newfile, GNC_FILE_DIALOG_SAVE))
        {
            /* user told us to ignore locks. So ignore them. */
            qof_session_begin (new_session, newfile, TRUE, FALSE);
        }
    }

    /* if the database doesn't exist, ask the user ... */
    else if ((ERR_FILEIO_FILE_NOT_FOUND == io_err) ||
             (ERR_BACKEND_NO_SUCH_DB == io_err) ||
             (ERR_SQL_DB_TOO_OLD == io_err))
    {
        if (FALSE == show_session_error (io_err, newfile, GNC_FILE_DIALOG_SAVE))
        {
            /* user told us to create a new database. Do it. */
            qof_session_begin (new_session, newfile, FALSE, TRUE);
        }
    }

    /* check again for session errors (since above dialog may have
     * cleared a file lock & moved things forward some more)
     * This time, errors will be fatal.
     */
    io_err = qof_session_get_error (new_session);
    if (ERR_BACKEND_NO_ERR != io_err)
    {
        show_session_error (io_err, newfile, GNC_FILE_DIALOG_SAVE);
        xaccLogDisable();
        qof_session_destroy (new_session);
        xaccLogEnable();
        g_free (newfile);
        save_in_progress--;
        return;
    }

    /* oops ... file already exists ... ask user what to do... */
    if (qof_session_save_may_clobber_data (new_session))
    {
        const char *format = _("The file %s already exists. "
                               "Are you sure you want to overwrite it?");

        /* if user says cancel, we should break out */
        if (!gnc_verify_dialog (NULL, FALSE, format, newfile))
        {
            xaccLogDisable();
            qof_session_destroy (new_session);
            xaccLogEnable();
            g_free (newfile);
            save_in_progress--;
            return;
        }

        /* Whoa-ok. Blow away the previous file. */
    }

    /* XXX Would logging make sense for databases as well (mysql/postgres) ?
     * Currently the logpath is relative to the data file path.
     * Databases don't have a file path, so no logging will be
     * done for them in the current setup.
     */
    if ( gnc_uri_is_file_uri ( newfile ) )
        logpath = gnc_uri_get_path(newfile);
    PINFO ("logpath=%s", logpath ? logpath : "(null)");
    xaccLogSetBaseName (logpath);
    g_free ( logpath );


    /* Prevent race condition between swapping the contents of the two
     * sessions, and actually installing the new session as the current
     * one. Any event callbacks that occur in this interval will have
     * problems if they check for the current book. */
    qof_event_suspend();

    /* if we got to here, then we've successfully gotten a new session */
    /* close up the old file session (if any) */
    qof_session_swap_data (session, new_session);
    gnc_clear_current_session();
    session = NULL;

    /* XXX At this point, we should really mark the data in the new session
     * as being 'dirty', since we haven't saved it at all under the new
     * session. But I'm lazy...
     */
    gnc_set_current_session(new_session);

    qof_event_resume();

    /* --------------- END CORE SESSION CODE -------------- */

    gnc_file_save ();
    save_in_progress--;

    g_free (newfile);
    LEAVE (" ");
}

void
gnc_file_quit (void)
{
    QofSession *session;

    gnc_set_busy_cursor (NULL, TRUE);
    session = gnc_get_current_session ();

    /* disable events; otherwise the mass deletion of accounts and
     * transactions during shutdown would cause massive redraws */
    qof_event_suspend ();

    qof_session_call_close_hooks(session);
    gnc_hook_run(HOOK_BOOK_CLOSED, session);
    gnc_close_gui_component_by_session (session);
    gnc_clear_current_session();

    gnc_get_current_session ();

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
    QofSession *session = gnc_get_current_session();
    return (qof_session_save_in_progress(session) || save_in_progress > 0);
}
