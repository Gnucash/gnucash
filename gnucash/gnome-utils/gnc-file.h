/********************************************************************\
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998, 1999, 2000 Linas Vepstas (linas@linas.org)   *
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

/*
 * FILE: gnc-file.h
 *
 * FUNCTION:
 * A set of file-handling utilities for GnuCash applications.
 * These utilities will "do the right thing" when used in the "File…"
 * pulldown menu, for the "New", "Open", "Save", "SaveAs", etc. menu entries.
 * In particular, they will verify that old files don't get clobbered,
 * they'll put up dialogue boxes to ask the user to confirm their actions,
 * etc.
 *
 * These utilities are written in a GUI-independent fashion, and should
 * work just fine with the Motif, gnome/gtk and Qt interfaces.
 * These utilities are appropriate for direct invocation from guile.
 *
 * These GUI utilities implement and maintain a single global "session"
 * that defines the currently edited account group.  In a sense, these
 * functions provide the GUI for the qof_session object.  The session
 * is essentially a file that is open for editing, with locks on it
 * to prevent other readers and writers from accessing it as long as its
 * open.
 *
 *
 * The gnc_file_save() routine will check for an existing edit session,
 *    and if one exists, it will save the account info to a file.
 *    If an error occurs, a popup dialogue will inform the user of
 *    the error.  If there is no existing filename open, then the
 *    user will be prompted for a file to save to (using the
 *    gnc_file_save_as() routine).  The existing session will remain
 *    open for further editing.
 *
 * The gnc_file_save_as() routine will prompt the user for a filename
 *    to save the account data to (using the standard GUI file dialogue
 *    box).  If the user specifies a filename, the account data will be
 *    saved. If an error occurs, a popup dialogue will inform the user
 *    of the error.  One possible error is that another user has
 *    the indicated file already locked up in a different session
 *    (in which case it is up to the user to try again, or to pick
 *    a different filename).  If it is possible to save without
 *    an error, then a new session is started for the indicated
 *    filename, locking out other users.  This new session remains
 *    open for further editing.
 *
 * The gnc_file_query_save_async() routine displays a native asynchronous
 *    dialog asking whether current work should be saved. Its continuation is
 *    called only after saving, discarding, or cancelling has completed. If
 *    saving needs a new filename, the continuation waits for that native
 *    chooser and its resulting save operation as well.
 *
 * The gnc_file_new() routine will check for an existing edit session.
 *    If one exists, it will ask the user if they want to save it,
 *    (using the gnc_file_query_save_async() dialogue). Then the current
 *    session will be destroyed, file locks will be removed, and
 *    account group structures will be set up for a new session.
 *
 * The gnc_file_open() routine check for an existing edit session.
 *    If one exists, it will ask the user if they want to save it.
 *    (using the gnc_file_query_save_async() dialogue). Next, the user will
 *    be prompted with a GUI standard file-selection dialogue to
 *    to pick a new file.  If no file is picked, this routine returns.
 *    If a new file was picked, then the current session will be
 *    destroyed and file locks on it will be removed.  The new
 *    file will then be opened for editing, establishing locks, etc.
 *    If an error occurs, the user will be informed with a pop-up
 *    dialogue.  If the file cannot be found, or if a read
 *    error occurs, a popup describing the error will pop up.
 *    One possible error is that another user has the indicated
 *    file already locked up in a different session (in which
 *    case it is up to the user to try again, or to pick
 *    a different filename).
 *
 * The gnc_file_revert() routine will discard any changes since the last
 * time the session was saved (but only after user confirmation).
 *
 * The gnc_file_open_file() routine behaves much like the gnc_file_open()
 *    routine, except that the new file to open is passed as a char *
 *    argument.
 *
 * The gnc_file_export() routine will check for an existing edit
 *    session, and if one exists, it will save just the commodities
 *    and accounts to a file.  If an error occurs, a popup dialogue
 *    will inform the user of the error.
 *
 * The gnc_file_quit() routine will close out and destroy the current session.
 *    The user WILL NOT BE PROMPTED to confirm this action, or do
 *    any kind of saving beforehand.
 *
 * HISTORY:
 * Derived from Rob Clark's original MainWindow.c code, Dec 1998
 */

#ifndef GNC_FILE_H
#define GNC_FILE_H

#include <glib.h>
#include "qof.h"
#include <gtk/gtk.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum
{
    GNC_FILE_DIALOG_OPEN,
    GNC_FILE_DIALOG_IMPORT,
    GNC_FILE_DIALOG_SAVE,
    GNC_FILE_DIALOG_EXPORT
} GNCFileDialogType;

#define GNC_TYPE_FILE_DIALOG_REQUEST (gnc_file_dialog_request_get_type ())
G_DECLARE_FINAL_TYPE (GncFileDialogRequest, gnc_file_dialog_request, GNC,
                      FILE_DIALOG_REQUEST, GObject)

/**
 * gnc_file_dialog_request_new:
 * @parent: (nullable): transient parent for the native chooser
 * @title: (nullable): chooser title; the dialog type supplies the default
 * @filters: (transfer full) (nullable): #GtkFileFilter list to offer
 * @starting_dir: (nullable): local directory to show initially
 * @type: the requested GnuCash file operation
 *
 * Creates an immutable description of a GTK4 file chooser.  The request
 * consumes both the list and its filter references.  A caller may unref the
 * request after starting an operation; it stays alive through its completion
 * callback.
 *
 * The operation must match @type: use open or open_multiple for OPEN and
 * IMPORT, and save for SAVE and EXPORT.
 */
GncFileDialogRequest *gnc_file_dialog_request_new (GtkWindow *parent,
                                                    const gchar *title,
                                                    GList *filters,
                                                    const gchar *starting_dir,
                                                    GNCFileDialogType type);

/**
 * gnc_file_dialog_request_new_for_folder:
 * @parent: (nullable): transient parent for the native chooser
 * @title: (nullable): chooser title; the dialog type supplies the default
 * @filters: (transfer full) (nullable): #GtkFileFilter list to offer
 * @initial_folder: (transfer none) (nullable): local or URI-based folder
 * @type: the requested GnuCash file operation
 *
 * Creates the same immutable request as gnc_file_dialog_request_new(), but
 * preserves a #GFile start folder for callers that need URI semantics.
 */
GncFileDialogRequest *gnc_file_dialog_request_new_for_folder (
    GtkWindow *parent, const gchar *title, GList *filters,
    GFile *initial_folder, GNCFileDialogType type);

/**
 * gnc_file_dialog_request_new_for_file:
 * @parent: (nullable): transient parent for the native chooser
 * @title: (nullable): chooser title; the dialog type supplies the default
 * @filters: (transfer full) (nullable): #GtkFileFilter list to offer
 * @initial_file: (transfer none) (nullable): initial file name for a save request
 * @type: the requested GnuCash file operation
 *
 * Creates the same immutable request as gnc_file_dialog_request_new(), but
 * preserves a #GFile initial name for native GTK4 save dialogs.
 */
GncFileDialogRequest *gnc_file_dialog_request_new_for_file (
    GtkWindow *parent, const gchar *title, GList *filters,
    GFile *initial_file, GNCFileDialogType type);

void gnc_file_dialog_request_open_async (GncFileDialogRequest *request,
                                         GCancellable *cancellable,
                                         GAsyncReadyCallback callback,
                                         gpointer user_data);

void gnc_file_dialog_request_save_async (GncFileDialogRequest *request,
                                         GCancellable *cancellable,
                                         GAsyncReadyCallback callback,
                                         gpointer user_data);

void gnc_file_dialog_request_open_multiple_async (GncFileDialogRequest *request,
                                                  GCancellable *cancellable,
                                                  GAsyncReadyCallback callback,
                                                  gpointer user_data);

GFile *gnc_file_dialog_request_finish (GncFileDialogRequest *request,
                                       GAsyncResult *result,
                                       GError **error);

GListModel *gnc_file_dialog_request_finish_multiple (GncFileDialogRequest *request,
                                                      GAsyncResult *result,
                                                      GError **error);

void gnc_file_new (GtkWindow *parent);
gboolean gnc_file_open (GtkWindow *parent);
void gnc_file_export(GtkWindow *parent);
void gnc_file_save (GtkWindow *parent);
void gnc_file_save_as (GtkWindow *parent);
void gnc_file_do_export(GtkWindow *parent, const char* filename);
void gnc_file_do_save_as(GtkWindow *parent, const char* filename);
void gnc_file_revert (GtkWindow *parent);

GList *gnc_file_dialog_get_datafile_filters (void);



typedef enum
{
    GNC_FILE_OPEN_REJECTED,
    GNC_FILE_OPEN_STARTED,
    GNC_FILE_OPEN_QUEUED
} GncFileOpenResult;

GncFileOpenResult gnc_file_open_file (GtkWindow *parent,
                                      const char *filename,
                                      gboolean open_readonly);

/**
 * GncFileQuerySaveCallback:
 * @parent: (nullable): window that owned the decision
 * @can_continue: whether saving completed or discarding was explicitly chosen
 * @user_data: caller supplied continuation data
 *
 * Receives the asynchronous result of the save-before-close decision. The
 * callback owns no reference to @parent; it can be %NULL if its window was
 * destroyed while a native dialog was active.
 */
typedef void (*GncFileQuerySaveCallback) (GtkWindow *parent,
                                          gboolean can_continue,
                                          gpointer user_data);
/**
 * Save the current session and invoke @completed once the save or a possible
 * native Save As flow has either completed or been cancelled.
 */
void gnc_file_save_async (GtkWindow *parent, GncFileQuerySaveCallback completed,
                          gpointer user_data);

void gnc_file_query_save_async (GtkWindow *parent, gboolean can_cancel,
                                GncFileQuerySaveCallback completed,
                                gpointer user_data);

void gnc_file_quit (void);

typedef void (*GNCShutdownCB) (int);
void gnc_file_set_shutdown_callback (GNCShutdownCB cb);
gboolean gnc_file_save_in_progress (void);

#ifdef __cplusplus
}
#endif

#endif /* GNC_FILE_H */
