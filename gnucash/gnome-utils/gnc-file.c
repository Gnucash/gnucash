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
#include <stdarg.h>

#include "dialog-utils.h"
#include "assistant-xml-encoding.h"
#include "gnc-backend-xml.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "Account.h"
#include "gnc-file.h"
#include "gnc-features.h"
#include "gnc-filepath-utils.h"
#include "gnc-string-utils.h"
#include "gnc-gui-query.h"
#include "gnc-hooks.h"
#include "gnc-keyring.h"
#include "gnc-session-load-executor.h"
#include "gnc-splash.h"
#include "gnc-session-transition.h"
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

typedef struct
{
    QofSessionOperationLease *lease_a;
    QofSessionOperationLease *lease_b;
} GncFileSessionLeasePair;

static void
file_session_lease_pair_release (GncFileSessionLeasePair *pair)
{
    if (!pair)
        return;
    qof_session_operation_lease_release (pair->lease_a);
    qof_session_operation_lease_release (pair->lease_b);
    pair->lease_a = NULL;
    pair->lease_b = NULL;
}

static gboolean
file_session_lease_pair_acquire (QofSession *session_a,
                                 QofSessionOperationKind kind_a,
                                 QofSession *session_b,
                                 QofSessionOperationKind kind_b,
                                 GncFileSessionLeasePair *pair)
{
    g_return_val_if_fail (pair != NULL, FALSE);

    pair->lease_a = NULL;
    pair->lease_b = NULL;
    if (!session_a || !session_b || session_a == session_b)
        return FALSE;

    if ((guintptr)session_a < (guintptr)session_b)
    {
        pair->lease_a = qof_session_operation_lease_acquire_for (session_a, kind_a);
        if (pair->lease_a)
            pair->lease_b = qof_session_operation_lease_acquire_for (session_b, kind_b);
    }
    else
    {
        pair->lease_b = qof_session_operation_lease_acquire_for (session_b, kind_b);
        if (pair->lease_b)
            pair->lease_a = qof_session_operation_lease_acquire_for (session_a, kind_a);
    }

    if (pair->lease_a && pair->lease_b)
        return TRUE;

    file_session_lease_pair_release (pair);
    return FALSE;
}

static gboolean
file_session_destroy (QofSession *session)
{
    QofSessionOperationLease *lease;
    gboolean destroyed;

    if (!session)
        return TRUE;
    lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_CLOSE);
    if (!lease)
        return FALSE;
    xaccLogDisable ();
    destroyed = qof_session_destroy_with_lease (session, lease);
    xaccLogEnable ();
    qof_session_operation_lease_release (lease);
    return destroyed;
}

struct _GncFileDialogRequest
{
    GObject parent_instance;
    GWeakRef parent;
    gchar *title;
    GFile *initial_folder;
    GFile *initial_file;
    GListStore *filters;
    GtkFileFilter *default_filter;
    GNCFileDialogType type;
};

G_DEFINE_FINAL_TYPE (GncFileDialogRequest, gnc_file_dialog_request, G_TYPE_OBJECT)

static gboolean
file_dialog_type_is_valid (GNCFileDialogType type)
{
    return type >= GNC_FILE_DIALOG_OPEN && type <= GNC_FILE_DIALOG_EXPORT;
}

static gboolean
file_dialog_type_is_open (GNCFileDialogType type)
{
    return type == GNC_FILE_DIALOG_OPEN || type == GNC_FILE_DIALOG_IMPORT;
}

static const gchar *
file_dialog_default_title (GNCFileDialogType type)
{
    switch (type)
    {
    case GNC_FILE_DIALOG_OPEN:
        return _("Open");
    case GNC_FILE_DIALOG_IMPORT:
        return _("Import");
    case GNC_FILE_DIALOG_SAVE:
        return _("Save");
    case GNC_FILE_DIALOG_EXPORT:
        return _("Export");
    }

    g_assert_not_reached ();
    return NULL;
}

static const gchar *
file_dialog_accept_label (GNCFileDialogType type)
{
    switch (type)
    {
    case GNC_FILE_DIALOG_OPEN:
        return _("Open");
    case GNC_FILE_DIALOG_IMPORT:
        return _("Import");
    case GNC_FILE_DIALOG_SAVE:
        return _("Save");
    case GNC_FILE_DIALOG_EXPORT:
        return _("Export");
    }

    g_assert_not_reached ();
    return NULL;
}

static void
file_dialog_request_finalize (GObject *object)
{
    GncFileDialogRequest *request = GNC_FILE_DIALOG_REQUEST (object);

    g_weak_ref_clear (&request->parent);
    g_clear_pointer (&request->title, g_free);
    g_clear_object (&request->initial_folder);
    g_clear_object (&request->initial_file);
    g_clear_object (&request->default_filter);
    g_clear_object (&request->filters);

    G_OBJECT_CLASS (gnc_file_dialog_request_parent_class)->finalize (object);
}

static void
gnc_file_dialog_request_class_init (GncFileDialogRequestClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    object_class->finalize = file_dialog_request_finalize;
}

static void
gnc_file_dialog_request_init (GncFileDialogRequest *request)
{
    g_weak_ref_init (&request->parent, NULL);
}

static void
file_dialog_request_take_filters (GncFileDialogRequest *request, GList *filters)
{
    GtkFileFilter *all_filter;

    if (!filters)
        return;

    request->filters = g_list_store_new (GTK_TYPE_FILE_FILTER);
    for (GList *node = filters; node; node = node->next)
    {
        GtkFileFilter *filter = GTK_FILE_FILTER (node->data);

        if (!GTK_IS_FILE_FILTER (filter))
        {
            g_warning ("Ignoring invalid file filter in dialog request");
            continue;
        }

        g_list_store_append (request->filters, filter);
        if (!request->default_filter)
            request->default_filter = g_object_ref (filter);
        g_object_unref (filter);
    }
    g_list_free (filters);

    all_filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (all_filter, _("All files"));
    gtk_file_filter_add_pattern (all_filter, "*");
    g_list_store_append (request->filters, all_filter);
    g_object_unref (all_filter);
}

static GncFileDialogRequest *
file_dialog_request_new_internal (GtkWindow *parent, const gchar *title,
                                  GList *filters, GFile *initial_folder,
                                  GFile *initial_file, GNCFileDialogType type)
{
    GncFileDialogRequest *request;

    g_return_val_if_fail (!parent || GTK_IS_WINDOW (parent), NULL);
    g_return_val_if_fail (!initial_folder || G_IS_FILE (initial_folder), NULL);
    g_return_val_if_fail (!initial_file || G_IS_FILE (initial_file), NULL);
    g_return_val_if_fail (!initial_folder || !initial_file, NULL);
    g_return_val_if_fail (file_dialog_type_is_valid (type), NULL);

    request = g_object_new (GNC_TYPE_FILE_DIALOG_REQUEST, NULL);
    g_weak_ref_set (&request->parent, parent);
    request->title = g_strdup (title ? title : file_dialog_default_title (type));
    if (initial_folder)
        request->initial_folder = g_object_ref (initial_folder);
    if (initial_file)
        request->initial_file = g_object_ref (initial_file);
    request->type = type;
    file_dialog_request_take_filters (request, filters);

    return request;
}

GncFileDialogRequest *
gnc_file_dialog_request_new (GtkWindow *parent, const gchar *title,
                             GList *filters, const gchar *starting_dir,
                             GNCFileDialogType type)
{
    GFile *initial_folder = NULL;
    GncFileDialogRequest *request;

    if (starting_dir && *starting_dir)
        initial_folder = g_file_new_for_path (starting_dir);
    request = file_dialog_request_new_internal (parent, title, filters,
                                                initial_folder, NULL, type);
    g_clear_object (&initial_folder);
    return request;
}

GncFileDialogRequest *
gnc_file_dialog_request_new_for_folder (GtkWindow *parent, const gchar *title,
                                        GList *filters, GFile *initial_folder,
                                        GNCFileDialogType type)
{
    return file_dialog_request_new_internal (parent, title, filters,
                                             initial_folder, NULL, type);
}

GncFileDialogRequest *
gnc_file_dialog_request_new_for_file (GtkWindow *parent, const gchar *title,
                                      GList *filters, GFile *initial_file,
                                      GNCFileDialogType type)
{
    return file_dialog_request_new_internal (parent, title, filters, NULL,
                                             initial_file, type);
}

static GtkFileDialog *
file_dialog_request_create_dialog (GncFileDialogRequest *request)
{
    GtkFileDialog *dialog = gtk_file_dialog_new ();

    gtk_file_dialog_set_title (dialog, request->title);
    gtk_file_dialog_set_accept_label (dialog,
                                      file_dialog_accept_label (request->type));
    if (request->initial_file)
        gtk_file_dialog_set_initial_file (dialog, request->initial_file);
    else if (request->initial_folder)
        gtk_file_dialog_set_initial_folder (dialog, request->initial_folder);
    if (request->filters)
    {
        gtk_file_dialog_set_filters (dialog, G_LIST_MODEL (request->filters));
        gtk_file_dialog_set_default_filter (dialog, request->default_filter);
    }

    return dialog;
}

static void
file_dialog_request_return_invalid_operation (GncFileDialogRequest *request,
                                              GCancellable *cancellable,
                                              GAsyncReadyCallback callback,
                                              gpointer user_data,
                                              const gchar *operation)
{
    GTask *task = g_task_new (request, cancellable, callback, user_data);

    g_task_return_new_error (task, G_IO_ERROR, G_IO_ERROR_INVALID_ARGUMENT,
                             "%s is incompatible with this file dialog type",
                             operation);
    g_object_unref (task);
}

static void
file_dialog_request_open_finished (GObject *source, GAsyncResult *result,
                                   gpointer user_data)
{
    GTask *task = G_TASK (user_data);
    GError *error = NULL;
    GFile *file = gtk_file_dialog_open_finish (GTK_FILE_DIALOG (source), result,
                                               &error);

    if (file)
        g_task_return_pointer (task, file, g_object_unref);
    else if (error)
        g_task_return_error (task, error);
    else
        g_task_return_new_error (task, G_IO_ERROR, G_IO_ERROR_FAILED,
                                 "The file dialog returned no selected file");
    g_object_unref (task);
}

static void
file_dialog_request_save_finished (GObject *source, GAsyncResult *result,
                                   gpointer user_data)
{
    GTask *task = G_TASK (user_data);
    GError *error = NULL;
    GFile *file = gtk_file_dialog_save_finish (GTK_FILE_DIALOG (source), result,
                                               &error);

    if (file)
        g_task_return_pointer (task, file, g_object_unref);
    else if (error)
        g_task_return_error (task, error);
    else
        g_task_return_new_error (task, G_IO_ERROR, G_IO_ERROR_FAILED,
                                 "The file dialog returned no selected file");
    g_object_unref (task);
}

static void
file_dialog_request_open_multiple_finished (GObject *source,
                                            GAsyncResult *result,
                                            gpointer user_data)
{
    GTask *task = G_TASK (user_data);
    GError *error = NULL;
    GListModel *files = gtk_file_dialog_open_multiple_finish (
        GTK_FILE_DIALOG (source), result, &error);

    if (files)
        g_task_return_pointer (task, files, g_object_unref);
    else if (error)
        g_task_return_error (task, error);
    else
        g_task_return_new_error (task, G_IO_ERROR, G_IO_ERROR_FAILED,
                                 "The file dialog returned no selected files");
    g_object_unref (task);
}

void
gnc_file_dialog_request_open_async (GncFileDialogRequest *request,
                                    GCancellable *cancellable,
                                    GAsyncReadyCallback callback,
                                    gpointer user_data)
{
    GtkFileDialog *dialog;
    GtkWindow *parent;
    GTask *task;

    g_return_if_fail (GNC_IS_FILE_DIALOG_REQUEST (request));
    if (!file_dialog_type_is_open (request->type))
    {
        file_dialog_request_return_invalid_operation (request, cancellable,
                                                      callback, user_data,
                                                      "Opening files");
        return;
    }

    dialog = file_dialog_request_create_dialog (request);
    parent = g_weak_ref_get (&request->parent);
    task = g_task_new (request, cancellable, callback, user_data);
    gtk_file_dialog_open (dialog, parent, cancellable,
                          file_dialog_request_open_finished, task);
    g_clear_object (&parent);
    g_object_unref (dialog);
}

void
gnc_file_dialog_request_save_async (GncFileDialogRequest *request,
                                    GCancellable *cancellable,
                                    GAsyncReadyCallback callback,
                                    gpointer user_data)
{
    GtkFileDialog *dialog;
    GtkWindow *parent;
    GTask *task;

    g_return_if_fail (GNC_IS_FILE_DIALOG_REQUEST (request));
    if (file_dialog_type_is_open (request->type))
    {
        file_dialog_request_return_invalid_operation (request, cancellable,
                                                      callback, user_data,
                                                      "Saving files");
        return;
    }

    dialog = file_dialog_request_create_dialog (request);
    parent = g_weak_ref_get (&request->parent);
    task = g_task_new (request, cancellable, callback, user_data);
    gtk_file_dialog_save (dialog, parent, cancellable,
                          file_dialog_request_save_finished, task);
    g_clear_object (&parent);
    g_object_unref (dialog);
}

void
gnc_file_dialog_request_open_multiple_async (GncFileDialogRequest *request,
                                             GCancellable *cancellable,
                                             GAsyncReadyCallback callback,
                                             gpointer user_data)
{
    GtkFileDialog *dialog;
    GtkWindow *parent;
    GTask *task;

    g_return_if_fail (GNC_IS_FILE_DIALOG_REQUEST (request));
    if (!file_dialog_type_is_open (request->type))
    {
        file_dialog_request_return_invalid_operation (request, cancellable,
                                                      callback, user_data,
                                                      "Opening multiple files");
        return;
    }

    dialog = file_dialog_request_create_dialog (request);
    parent = g_weak_ref_get (&request->parent);
    task = g_task_new (request, cancellable, callback, user_data);
    gtk_file_dialog_open_multiple (dialog, parent, cancellable,
                                   file_dialog_request_open_multiple_finished,
                                   task);
    g_clear_object (&parent);
    g_object_unref (dialog);
}

GFile *
gnc_file_dialog_request_finish (GncFileDialogRequest *request,
                                GAsyncResult *result, GError **error)
{
    g_return_val_if_fail (GNC_IS_FILE_DIALOG_REQUEST (request), NULL);
    g_return_val_if_fail (g_task_is_valid (result, request), NULL);

    return g_task_propagate_pointer (G_TASK (result), error);
}

GListModel *
gnc_file_dialog_request_finish_multiple (GncFileDialogRequest *request,
                                         GAsyncResult *result, GError **error)
{
    g_return_val_if_fail (GNC_IS_FILE_DIALOG_REQUEST (request), NULL);
    g_return_val_if_fail (g_task_is_valid (result, request), NULL);

    return g_task_propagate_pointer (G_TASK (result), error);
}

GList*
gnc_file_dialog_get_datafile_filters (void)
{
    /* GtkFileDialog accepts GtkFileFilter patterns only. The explicit backup
     * filter retains direct access to timestamped copies; the datafile filter
     * is intentionally an extension filter because the native portal cannot
     * express the previous negative backup predicate. "All files" has always
     * remained available as an explicit final filter. */
    const char *datafiles = N_("GnuCash files (*.gnucash, *.xac)");
    const char *backups = N_("Backups only (*.gnucash.*.gnucash, *.xac.*.xac)");
    GtkFileFilter *filter;
    GList *filters = NULL;

    filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (filter, _(datafiles));
    gtk_file_filter_add_pattern (filter, "*.gnucash");
    gtk_file_filter_add_pattern (filter, "*.xac");
    filters = g_list_append (filters, filter);

    filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (filter, _(backups));
    gtk_file_filter_add_pattern (filter, "*.gnucash.??????????????.gnucash");
    gtk_file_filter_add_pattern (filter, "*.xac.??????????????.xac");
    filters = g_list_append (filters, filter);

    return filters;
}

typedef void (*GncFileSelectionFunc) (GtkWindow *parent,
                                      const gchar *filename,
                                      gpointer user_data);
typedef void (*GncFileSelectionCancelledFunc) (GtkWindow *parent,
                                               const GError *error,
                                               gpointer user_data);

typedef struct
{
    GWeakRef parent;
    gboolean has_parent;
    GncFileSelectionFunc selected;
    GncFileSelectionCancelledFunc cancelled;
    gpointer user_data;
    GDestroyNotify user_data_destroy;
} GncFileSelectionData;

static void
gnc_file_selection_data_free (GncFileSelectionData *data)
{
    if (data->user_data_destroy)
        data->user_data_destroy (data->user_data);
    g_weak_ref_clear (&data->parent);
    g_free (data);
}

static void
gnc_file_selection_finished (GObject *source, GAsyncResult *result,
                             gpointer user_data)
{
    GncFileSelectionData *data = user_data;
    GncFileDialogRequest *request = GNC_FILE_DIALOG_REQUEST (source);
    GError *error = NULL;
    GFile *file;
    GtkWindow *parent;

    file = gnc_file_dialog_request_finish (request, result, &error);
    parent = GTK_WINDOW (g_weak_ref_get (&data->parent));
    if (data->has_parent && !parent)
    {
        if (data->cancelled)
            data->cancelled (NULL, NULL, data->user_data);
    }
    else if (file)
    {
        gchar *filename = g_file_get_path (file);

        if (filename)
        {
            data->selected (parent, filename, data->user_data);
            g_free (filename);
        }
        else if (data->cancelled)
            data->cancelled (parent, NULL, data->user_data);
        else if (parent)
            gnc_error_dialog (parent, "%s", _("Please select a local file."));
    }
    else if (data->cancelled)
        data->cancelled (parent, error, data->user_data);
    else if (error && parent &&
             !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
        gnc_error_dialog (parent, "%s", error->message);

    g_clear_object (&file);
    g_clear_error (&error);
    g_clear_object (&parent);
    gnc_file_selection_data_free (data);
}

static void
gnc_file_select_async_full (GtkWindow *parent, const gchar *title, GList *filters,
                            const gchar *starting_dir, GNCFileDialogType type,
                            GncFileSelectionFunc selected,
                            GncFileSelectionCancelledFunc cancelled,
                            gpointer user_data,
                            GDestroyNotify user_data_destroy)
{
    GncFileSelectionData *data;
    GncFileDialogRequest *request;

    g_return_if_fail (selected != NULL);

    data = g_new0 (GncFileSelectionData, 1);
    g_weak_ref_init (&data->parent, parent);
    data->has_parent = parent != NULL;
    data->selected = selected;
    data->cancelled = cancelled;
    data->user_data = user_data;
    data->user_data_destroy = user_data_destroy;
    request = gnc_file_dialog_request_new (parent, title, filters, starting_dir,
                                           type);
    if (type == GNC_FILE_DIALOG_OPEN || type == GNC_FILE_DIALOG_IMPORT)
        gnc_file_dialog_request_open_async (request, NULL,
                                            gnc_file_selection_finished, data);
    else
        gnc_file_dialog_request_save_async (request, NULL,
                                            gnc_file_selection_finished, data);
    g_object_unref (request);
}

static void
gnc_file_select_async (GtkWindow *parent, const gchar *title, GList *filters,
                       const gchar *starting_dir, GNCFileDialogType type,
                       GncFileSelectionFunc selected)
{
    gnc_file_select_async_full (parent, title, filters, starting_dir, type,
                                selected, NULL, NULL, NULL);
}
static void gnc_file_open_request_dialog_with_transition (
    GtkWindow *parent, const gchar *starting_dir,
    GncSessionTransition *transition);
typedef void (*GncFileSessionErrorCallback) (GtkWindow *parent,
                                             gboolean uh_oh,
                                             gpointer user_data);

typedef struct
{
    GWeakRef parent;
    gboolean has_parent;
    gchar *history_file;
    QofSession *session;
    QofBook *book;
    gchar *session_url;
    guint64 session_generation;
    gboolean uh_oh;
    GncFileSessionErrorCallback completed;
    gpointer user_data;
} GncFileHistoryRemovalRequest;

static gboolean
file_session_error_context_is_current (GncFileHistoryRemovalRequest *request)
{
    QofSession *session;

    if (gnc_current_session_get_generation () != request->session_generation)
        return FALSE;
    if (!request->session)
        return !gnc_current_session_exist ();
    if (!gnc_current_session_exist ())
        return FALSE;
    session = gnc_get_current_session ();
    return session == request->session &&
           qof_session_get_book (session) == request->book &&
           g_strcmp0 (qof_session_get_url (session), request->session_url) == 0;
}

static void
file_session_error_complete (GncFileSessionErrorCallback completed,
                             GtkWindow *parent, gboolean uh_oh,
                             gpointer user_data)
{
    if (completed)
        completed (parent, uh_oh, user_data);
}

static GncFileHistoryRemovalRequest *
file_session_error_request_new (GtkWindow *parent,
                                GncFileSessionErrorCallback completed,
                                gpointer user_data);

static void
file_session_error_verify_finished (GtkWindow *parent, gint response,
                                    gpointer user_data)
{
    GncFileHistoryRemovalRequest *request = user_data;

    file_session_error_complete (request->completed, parent,
                                 response != GTK_RESPONSE_YES,
                                 request->user_data);
    g_free (request->session_url);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static void
file_session_error_action_finished (GtkWindow *parent, gint response,
                                    gpointer user_data)
{
    GncFileHistoryRemovalRequest *request = user_data;

    file_session_error_complete (request->completed, parent,
                                 response != GTK_RESPONSE_ACCEPT,
                                 request->user_data);
    g_free (request->session_url);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static void
file_session_error_upgrade_finished (GtkWindow *parent, gint response,
                                     gpointer user_data)
{
    GncFileHistoryRemovalRequest *request = user_data;

    file_session_error_complete (request->completed, parent,
                                 response != GTK_RESPONSE_OK,
                                 request->user_data);
    g_free (request->session_url);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static void
file_session_error_info_finished (GObject *source, GAsyncResult *result,
                                  gpointer user_data)
{
    GncFileHistoryRemovalRequest *request = user_data;
    GError *error = NULL;

    (void)gtk_alert_dialog_choose_finish (GTK_ALERT_DIALOG (source), result, &error);
    if (error && !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
        g_warning ("File error notification failed: %s", error->message);
    g_clear_error (&error);
    {
        GtkWindow *parent = GTK_WINDOW (g_weak_ref_get (&request->parent));
        if (request->has_parent && !parent)
            request->uh_oh = TRUE;
        file_session_error_complete (request->completed, parent, request->uh_oh,
                                     request->user_data);
        g_clear_object (&parent);
    }
    g_free (request->session_url);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static gboolean
file_session_error_report (GtkWindow *parent,
                           GncFileSessionErrorCallback completed,
                           gpointer user_data, gboolean uh_oh,
                           const gchar *format, ...)
{
    va_list args;
    gchar *message;

    va_start (args, format);
    message = g_strdup_vprintf (format, args);
    va_end (args);
    if (completed)
    {
        GncFileHistoryRemovalRequest *request =
            file_session_error_request_new (parent, completed, user_data);
        GtkAlertDialog *dialog = gtk_alert_dialog_new ("%s", message);

        request->uh_oh = uh_oh;
        gtk_alert_dialog_choose (dialog, parent, NULL,
                                 file_session_error_info_finished, request);
        g_object_unref (dialog);
        g_free (message);
        return TRUE;
    }
    gnc_error_dialog (parent, "%s", message);
    g_free (message);
    return FALSE;
}

static void
file_session_error_history_finished (GtkWindow *parent, gint response,
                                     gpointer user_data)
{
    GncFileHistoryRemovalRequest *request = user_data;

    if ((!request->has_parent || parent) &&
        file_session_error_context_is_current (request) &&
        response == GTK_RESPONSE_YES && gnc_history_test_for_file (request->history_file))
        gnc_history_remove_file (request->history_file);
    file_session_error_complete (request->completed, parent, TRUE,
                                 request->user_data);
    g_free (request->history_file);
    g_free (request->session_url);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static GncFileHistoryRemovalRequest *
file_session_error_request_new (GtkWindow *parent,
                                GncFileSessionErrorCallback completed,
                                gpointer user_data)
{
    GncFileHistoryRemovalRequest *request = g_new0 (GncFileHistoryRemovalRequest, 1);

    g_weak_ref_init (&request->parent, parent);
    request->has_parent = parent != NULL;
    request->session_generation = gnc_current_session_get_generation ();
    if (gnc_current_session_exist ())
    {
        request->session = gnc_get_current_session ();
        request->book = qof_session_get_book (request->session);
        request->session_url = g_strdup (qof_session_get_url (request->session));
    }
    request->completed = completed;
    request->user_data = user_data;
    return request;
}

static void
show_session_error_async (GtkWindow *parent,
                          QofBackendError io_error,
                          const char *newfile,
                          GNCFileDialogType type,
                          GncFileSessionErrorCallback completed,
                          gpointer user_data)
{
    gboolean uh_oh = TRUE;
    gboolean deferred = FALSE;
    const char *fmt, *label;
    gchar *displayname;
    GncFileHistoryRemovalRequest *request;

    if (NULL == newfile)
    {
        displayname = g_strdup (_("(null)"));
    }
    else if (!gnc_uri_targets_local_fs (newfile)) /* Hide the db password in error messages */
        displayname = gnc_uri_normalize_uri (newfile, FALSE);
    else
    {
        /* Strip the protocol from the file name and ensure absolute filename. */
        char *uri = gnc_uri_normalize_uri (newfile, FALSE);
        displayname = gnc_uri_get_path (uri);
        g_free (uri);
    }

    switch (io_error)
    {
    case ERR_BACKEND_NO_ERR:
        uh_oh = FALSE;
        break;

    case ERR_BACKEND_NO_HANDLER:
        fmt = _("No suitable backend was found for %s.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_BACKEND_NO_BACKEND:
        fmt = _("The URL %s is not supported by this version of GnuCash.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_BACKEND_BAD_URL:
        fmt = _("Can't parse the URL %s.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_BACKEND_CANT_CONNECT:
        fmt = _("Can't connect to %s. "
                "The host, username or password were incorrect.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_BACKEND_CONN_LOST:
        fmt = _("Can't connect to %s. "
                "Connection was lost, unable to send data.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_BACKEND_TOO_NEW:
        fmt = _("This file/URL appears to be from a newer version "
                "of GnuCash. You must upgrade your version of GnuCash "
                "to work with this data.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, "%s", fmt);
        break;

    case ERR_BACKEND_NO_SUCH_DB:
        fmt = _("The database %s doesn't seem to exist. "
                "Do you want to create it?");
        request = file_session_error_request_new (parent, completed, user_data);
        gnc_verify_dialog_async (parent, TRUE, file_session_error_verify_finished,
                                 request, fmt, displayname);
        deferred = TRUE;
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

        request = file_session_error_request_new (parent, completed, user_data);
        gnc_action_dialog_async (parent, label, FALSE,
                                 file_session_error_action_finished, request,
                                 fmt, displayname);
        deferred = TRUE;
        break;

    case ERR_BACKEND_READONLY:
        fmt = _("GnuCash could not write to %s. "
                "That database may be on a read-only file system, "
                "you may not have write permission for the directory "
                "or your anti-virus software is preventing this action.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_BACKEND_DATA_CORRUPT:
        fmt = _("The file/URL %s "
                "does not contain GnuCash data or the data is corrupt.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_BACKEND_SERVER_ERR:
        fmt = _("The server at URL %s "
                "experienced an error or encountered bad or corrupt data.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_BACKEND_PERM:
        fmt = _("You do not have permission to access %s.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_BACKEND_MISC:
        fmt = _("An error occurred while processing %s.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_FILEIO_FILE_BAD_READ:
        fmt = _("There was an error reading the file. "
                "Do you want to continue?");
        request = file_session_error_request_new (parent, completed, user_data);
        gnc_verify_dialog_async (parent, TRUE, file_session_error_verify_finished,
                                 request, "%s", fmt);
        deferred = TRUE;
        break;

    case ERR_FILEIO_PARSE_ERROR:
        fmt = _("There was an error parsing the file %s.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_FILEIO_FILE_EMPTY:
        fmt = _("The file %s is empty.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_FILEIO_FILE_NOT_FOUND:
        if (type == GNC_FILE_DIALOG_SAVE)
        {
            uh_oh = FALSE;
        }
        else if (gnc_history_test_for_file (displayname))
        {
            fmt = _("The file/URI %s could not be found.\n\nThe file is in the history list, do you want to remove it?");
            request = file_session_error_request_new (parent, completed, user_data);
            request->history_file = g_strdup (displayname);
            gnc_verify_dialog_async (parent, FALSE, file_session_error_history_finished,
                                     request, fmt, displayname);
            deferred = TRUE;
        }
        else
        {
            fmt = _("The file/URI %s could not be found.");
            deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        }
        break;

    case ERR_FILEIO_FILE_TOO_OLD:
        fmt = _("This file is from an older version of GnuCash. "
                "Do you want to continue?");
        request = file_session_error_request_new (parent, completed, user_data);
        gnc_verify_dialog_async (parent, TRUE, file_session_error_verify_finished,
                                 request, "%s", fmt);
        deferred = TRUE;
        break;

    case ERR_FILEIO_UNKNOWN_FILE_TYPE:
        fmt = _("The file type of file %s is unknown.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_FILEIO_BACKUP_ERROR:
        fmt = _("Could not make a backup of the file %s");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_FILEIO_WRITE_ERROR:
        fmt = _("Could not write to file %s. Check that you have "
                "permission to write to this file and that "
                "there is sufficient space to create it.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_FILEIO_FILE_EACCES:
        fmt = _("No read permission to read from file %s.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, displayname);
        break;

    case ERR_FILEIO_RESERVED_WRITE:
        /* Translators: the first %s is a path in the filesystem,
           the second %s is PACKAGE_NAME, which by default is "GnuCash" */
        fmt = _("You attempted to save in\n%s\nor a subdirectory thereof. "
                "This is not allowed as %s reserves that directory for internal use.\n\n"
                "Please try again in a different directory.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, gnc_userdata_dir (), PACKAGE_NAME);
        break;

    case ERR_SQL_DB_TOO_OLD:
        fmt = _("This database is from an older version of GnuCash. "
                "Select OK to upgrade it to the current version, Cancel "
                "to mark it read-only.");
        request = file_session_error_request_new (parent, completed, user_data);
        gnc_ok_cancel_dialog_async (parent, GTK_RESPONSE_CANCEL,
                                    file_session_error_upgrade_finished,
                                    request, "%s", fmt);
        deferred = TRUE;
        break;

    case ERR_SQL_DB_TOO_NEW:
        fmt = _("This database is from a newer version of GnuCash. "
                "This version can read it, but cannot safely save to it. "
                "It will be marked read-only until you do File->Save As, "
                "but data may be lost in writing to the old version.");
        deferred = file_session_error_report (parent, completed, user_data, uh_oh, "%s", fmt);
        break;

    case ERR_SQL_DB_BUSY:
        fmt = _("The SQL database is in use by other users, "
                "and the upgrade cannot be performed until they logoff. "
                "If there are currently no other users, consult the "
                "documentation to learn how to clear out dangling login "
                "sessions.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, "%s", fmt);
        break;

    case ERR_SQL_BAD_DBI:
        fmt = _("The library \"libdbi\" installed on your system doesn't correctly "
                "store large numbers. This means GnuCash cannot use SQL databases "
                "correctly. Gnucash will not open or save to SQL databases until this is "
                "fixed by installing a different version of \"libdbi\". Please see "
                "https://bugs.gnucash.org/show_bug.cgi?id=611936 for more "
                "information.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, "%s", fmt);
        break;

    case ERR_SQL_DBI_UNTESTABLE:
        fmt = _("GnuCash could not complete a critical test for the presence of "
                "a bug in the \"libdbi\" library. This may be caused by a "
                "permissions misconfiguration of your SQL database. Please see "
                "https://bugs.gnucash.org/show_bug.cgi?id=645216 for more "
                "information.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, "%s", fmt);
        break;

    case ERR_FILEIO_FILE_UPGRADE:
        fmt = _("This file is from an older version of GnuCash and will be "
                "upgraded when saved by this version. You will not be able "
                "to read the saved file from the older version of Gnucash "
                "(it will report an \"error parsing the file\"). If you wish "
                "to preserve the old version, exit without saving.");
        deferred = file_session_error_report (parent, completed, user_data, uh_oh, "%s", fmt);
        uh_oh = FALSE;
        break;

    default:
        PERR ("FIXME: Unhandled error %d", io_error);
        fmt = _("An unknown I/O error (%d) occurred.");
        deferred = file_session_error_report (parent, completed, user_data, TRUE, fmt, io_error);
        break;
    }

    g_free (displayname);
    if (!deferred)
        file_session_error_complete (completed, parent, uh_oh, user_data);
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

typedef struct
{
    GWeakRef parent;
    gboolean has_parent;
    gboolean can_cancel;
    GncFileQuerySaveCallback completed;
    gpointer user_data;
} GncFileQuerySaveRequest;

void gnc_file_save_async (GtkWindow *parent,
                          GncFileQuerySaveCallback completed,
                          gpointer user_data);

static void
gnc_file_query_save_request_free (GncFileQuerySaveRequest *request)
{
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static void
gnc_file_query_save_complete (GncFileQuerySaveRequest *request,
                              gboolean can_continue)
{
    GtkWindow *parent = GTK_WINDOW (g_weak_ref_get (&request->parent));

    if (request->has_parent && !parent)
        can_continue = FALSE;
    request->completed (parent, can_continue, request->user_data);
    g_clear_object (&parent);
    gnc_file_query_save_request_free (request);
}

static void gnc_file_query_save_continue (GncFileQuerySaveRequest *request);
static void gnc_file_query_save_after_save (GtkWindow *parent, gboolean saved,
                                             gpointer user_data);

static void
gnc_file_query_save_finished (GObject *source, GAsyncResult *result,
                              gpointer user_data)
{
    GncFileQuerySaveRequest *request = user_data;
    GError *error = NULL;
    gint response;
    GtkWindow *parent;

    response = gtk_alert_dialog_choose_finish (GTK_ALERT_DIALOG (source), result,
                                               &error);
    parent = GTK_WINDOW (g_weak_ref_get (&request->parent));
    if (request->has_parent && !parent)
    {
        g_clear_error (&error);
        gnc_file_query_save_complete (request, FALSE);
        return;
    }

    if (error && !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
        g_warning ("Save-before-close confirmation failed: %s", error->message);
    g_clear_error (&error);

    if (response == 0)
        gnc_file_query_save_complete (request, TRUE);
    else if (response == 1 && request->can_cancel)
        gnc_file_query_save_complete (request, FALSE);
    else if (response == (request->can_cancel ? 2 : 1))
        gnc_file_save_async (parent, gnc_file_query_save_after_save,
                                       request);
    else
        gnc_file_query_save_complete (request, request->can_cancel ? FALSE : TRUE);

    g_clear_object (&parent);
}

static void
gnc_file_query_save_after_save (GtkWindow *parent, gboolean saved,
                                gpointer user_data)
{
    GncFileQuerySaveRequest *request = user_data;

    (void)parent;
    if (saved)
        gnc_file_query_save_complete (request, TRUE);
    else
        gnc_file_query_save_continue (request);
}

static void
gnc_file_query_save_continue (GncFileQuerySaveRequest *request)
{
    QofBook *book;
    GtkWindow *parent;
    GtkAlertDialog *dialog;
    const char *buttons_with_cancel[] =
    {
        _("Continue Without Saving"),
        _("Cancel"),
        _("Save"),
        NULL
    };
    const char *buttons_without_cancel[] =
    {
        _("Continue Without Saving"),
        _("Save"),
        NULL
    };
    time64 oldest_change;
    gint minutes;
    gchar *detail;

    if (!gnc_current_session_exist ())
    {
        gnc_file_query_save_complete (request, TRUE);
        return;
    }

    book = qof_session_get_book (gnc_get_current_session ());
    gnc_autosave_remove_timer (book);
    if (!qof_book_session_not_saved (book))
    {
        gnc_file_query_save_complete (request, TRUE);
        return;
    }

    parent = GTK_WINDOW (g_weak_ref_get (&request->parent));
    if (request->has_parent && !parent)
    {
        g_clear_object (&parent);
        gnc_file_query_save_complete (request, FALSE);
        return;
    }

    oldest_change = qof_book_get_session_dirty_time (book);
    minutes = (gnc_time (NULL) - oldest_change) / 60 + 1;
    detail = g_strdup_printf (ngettext (
        "If you don't save, changes from the past %d minute will be discarded.",
        "If you don't save, changes from the past %d minutes will be discarded.",
        minutes), minutes);
    dialog = gtk_alert_dialog_new ("%s", _("Save changes to the file?"));
    gtk_alert_dialog_set_detail (dialog, detail);
    gtk_alert_dialog_set_buttons (dialog, request->can_cancel ?
                                  buttons_with_cancel : buttons_without_cancel);
    gtk_alert_dialog_set_default_button (dialog, request->can_cancel ? 2 : 1);
    gtk_alert_dialog_set_cancel_button (dialog, request->can_cancel ? 1 : 0);
    gtk_alert_dialog_choose (dialog, parent, NULL, gnc_file_query_save_finished,
                             request);
    g_object_unref (dialog);
    g_free (detail);
    g_clear_object (&parent);
}

void
gnc_file_query_save_async (GtkWindow *parent, gboolean can_cancel,
                           GncFileQuerySaveCallback completed,
                           gpointer user_data)
{
    GncFileQuerySaveRequest *request;

    g_return_if_fail (completed != NULL);
    request = g_new0 (GncFileQuerySaveRequest, 1);
    g_weak_ref_init (&request->parent, parent);
    request->has_parent = parent != NULL;
    request->can_cancel = can_cancel;
    request->completed = completed;
    request->user_data = user_data;
    gnc_file_query_save_continue (request);
}

typedef struct
{
    GWeakRef parent;
    gboolean has_parent;
    GncSessionTransition *transition;
} GncFileNewTransitionRequest;

static void
gnc_file_new_transition_request_free (GncFileNewTransitionRequest *request)
{
    GncSessionTransition *transition = request->transition;

    request->transition = NULL;
    g_weak_ref_clear (&request->parent);
    g_free (request);
    if (transition)
        gnc_session_transition_complete (transition);
}

static void
gnc_file_new_after_query (GtkWindow *parent, gboolean can_continue,
                          gpointer user_data)
{
    GncFileNewTransitionRequest *request = user_data;
    QofSession *session;
    QofSessionOperationLease *close_lease;

    if (!can_continue)
        goto out;

    if (gnc_current_session_exist())
    {
        session = gnc_get_current_session ();
        close_lease = qof_session_operation_lease_acquire_for (
            session, QOF_SESSION_OPERATION_CLOSE);
        if (!close_lease)
            goto out;

        qof_event_suspend ();
        gnc_hook_run(HOOK_BOOK_CLOSED, session);
        gnc_close_gui_component_by_session (session);
        gnc_state_save (session);
        if (!gnc_current_session_exist () ||
            gnc_get_current_session () != session ||
            !gnc_clear_current_session_with_lease (close_lease))
        {
            qof_session_operation_lease_release (close_lease);
            qof_event_resume ();
            goto out;
        }
        qof_session_operation_lease_release (close_lease);
        qof_event_resume ();
    }

    gnc_get_current_session ();
    gnc_hook_run(HOOK_NEW_BOOK, NULL);
    gnc_gui_refresh_all ();
    gnc_book_opened ();
    (void)parent;

out:
    gnc_file_new_transition_request_free (request);
}

static void
gnc_file_new_transition_start (GncSessionTransition *transition,
                               gpointer user_data)
{
    GncFileNewTransitionRequest *request = user_data;
    GtkWindow *parent = GTK_WINDOW (g_weak_ref_get (&request->parent));

    request->transition = transition;
    if (request->has_parent && !parent)
    {
        gnc_file_new_transition_request_free (request);
        return;
    }
    gnc_file_query_save_async (parent, TRUE, gnc_file_new_after_query, request);
    g_clear_object (&parent);
}

static void
gnc_file_new_from_transition (GtkWindow *parent,
                              GncSessionTransition *transition)
{
    GncFileNewTransitionRequest *request =
        g_new0 (GncFileNewTransitionRequest, 1);

    g_weak_ref_init (&request->parent, parent);
    request->has_parent = parent != NULL;
    request->transition = transition;
    gnc_file_new_after_query (parent, TRUE, request);
}

void
gnc_file_new (GtkWindow *parent)
{
    GncFileNewTransitionRequest *request =
        g_new0 (GncFileNewTransitionRequest, 1);
    GncSessionTransitionDisposition disposition;

    g_weak_ref_init (&request->parent, parent);
    request->has_parent = parent != NULL;
    disposition = gnc_session_transition_enqueue (
        GNC_SESSION_TRANSITION_NEW, gnc_file_new_transition_start,
        (GncSessionTransitionCancelFunc)gnc_file_new_transition_request_free,
        request);
    if (disposition == GNC_SESSION_TRANSITION_REJECTED)
        gnc_file_new_transition_request_free (request);
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
    gnc_info_dialog (parent, "%s", final);

    g_free (final);
    g_list_free_full (infos, g_free);
}

typedef struct
{
    GWeakRef parent;
    gchar *filename;
    gboolean is_readonly;
    gboolean reset_bayes_conversion;
    gboolean break_lock;
    GncSessionTransition *transition;
} GncFileOpenRequest;


static gboolean gnc_file_open_request_with_mode (GtkWindow *parent,
                                                  const char *filename,
                                                  gboolean is_readonly,
                                                  gboolean reset_bayes_conversion,
                                                  gboolean break_lock,
                                                  GncSessionTransition *transition);
static gboolean gnc_post_file_open (GtkWindow *parent, const char *filename,
                                    gboolean is_readonly,
                                    gboolean reset_bayes_conversion,
                                    gboolean break_lock,
                                    GncSessionTransition *transition);
typedef struct
{
    GWeakRef parent;
    gboolean has_parent;
    gchar *scheme;
    gchar *hostname;
    gint32 port;
    gchar *path;
    gboolean is_readonly;
    gboolean reset_bayes_conversion;
    gboolean break_lock;
    GncSessionTransition *transition;
} GncFilePasswordRequest;

static void
gnc_file_password_request_free (GncFilePasswordRequest *request)
{
    GncSessionTransition *transition = request->transition;

    request->transition = NULL;
    g_weak_ref_clear (&request->parent);
    g_free (request->scheme);
    g_free (request->hostname);
    g_free (request->path);
    g_free (request);
    if (transition)
        gnc_session_transition_complete (transition);
}

static void
gnc_file_open_after_keyring_password (GObject *source, GAsyncResult *result,
                                      gpointer user_data)
{
    GncFilePasswordRequest *request = user_data;
    GtkWindow *parent = GTK_WINDOW (g_weak_ref_get (&request->parent));
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *uri = NULL;
    GError *error = NULL;

    (void)source;
    if (request->has_parent && !parent)
        goto out;

    if (gnc_keyring_get_password_finish (result, &username, &password, &error))
    {
        uri = gnc_uri_create_uri (request->scheme, request->hostname,
                                  request->port, username, password,
                                  request->path);
        if (uri)
        {
            if (gnc_post_file_open (parent, uri, request->is_readonly,
                                    request->reset_bayes_conversion,
                                    request->break_lock,
                                    request->transition))
                request->transition = NULL;
        }
        else
            gnc_error_dialog (parent, "%s",
                              _("The database connection URI is invalid."));
    }
    else if (error &&
             !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
        gnc_error_dialog (parent, "%s", error->message);

out:
    g_clear_error (&error);
    g_free (uri);
    g_free (username);
    g_free (password);
    g_clear_object (&parent);
    gnc_file_password_request_free (request);
}



static void
gnc_file_open_request_free (GncFileOpenRequest *request)
{
    GncSessionTransition *transition = request->transition;

    request->transition = NULL;
    g_weak_ref_clear (&request->parent);
    g_free (request->filename);
    g_free (request);
    if (transition)
        gnc_session_transition_complete (transition);
}

static gboolean
gnc_file_needs_xml_encoding_conversion (const gchar *filename)
{
    gchar *normalized_uri;
    gchar *scheme = NULL;
    gchar *hostname = NULL;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path = NULL;
    gint32 port = 0;
    gboolean needs_conversion = FALSE;

    normalized_uri = gnc_uri_normalize_uri (filename, FALSE);
    if (!normalized_uri)
        return FALSE;

    gnc_uri_get_components (normalized_uri, &scheme, &hostname, &port,
                            &username, &password, &path);
    if (gnc_uri_is_file_scheme (scheme) && path)
        needs_conversion = gnc_xml_file_needs_encoding_conversion (path);

    g_free (scheme);
    g_free (hostname);
    g_free (username);
    g_free (password);
    g_free (path);
    g_free (normalized_uri);

    return needs_conversion;
}

typedef enum
{
    GNC_FILE_OPEN_NORMAL,
    GNC_FILE_OPEN_BREAK_LOCK,
    GNC_FILE_OPEN_NEW_STORE,
} GncFileOpenMode;

typedef struct
{
    GWeakRef parent;
    gboolean has_parent;
    gchar *filename;
    gboolean is_readonly;
    gboolean reset_bayes_conversion;
    GncFileOpenMode mode;
    QofSession *expected_session;
    QofBook *expected_book;
    gchar *expected_url;
    guint64 expected_generation;
    QofSession *new_session;
    QofBackendError last_error;
    gulong parent_destroy_handler;
    gboolean load_registered;
    GncSessionLoadExecutor *load_executor;
    GncSessionTransition *transition;
} GncFileOpenOperation;

static GList *file_open_load_operations;
static gboolean file_open_load_shutdown_hook_registered;

static void file_open_start (GncFileOpenOperation *operation);

static void
file_open_load_registry_remove (GncFileOpenOperation *operation)
{
    GtkWindow *parent;

    if (!operation || !operation->load_registered)
        return;
    operation->load_registered = FALSE;
    file_open_load_operations = g_list_remove (file_open_load_operations,
                                               operation);
    parent = GTK_WINDOW (g_weak_ref_get (&operation->parent));
    if (parent && operation->parent_destroy_handler)
        g_signal_handler_disconnect (parent, operation->parent_destroy_handler);
    operation->parent_destroy_handler = 0;
    g_clear_object (&parent);
}

static void
file_open_parent_destroyed (GtkWidget *parent, gpointer user_data)
{
    GncFileOpenOperation *operation = user_data;

    (void)parent;
    operation->parent_destroy_handler = 0;
    if (operation->load_registered && operation->new_session)
        (void)qof_session_cancel_active_load (operation->new_session);
}

static void
file_open_ui_shutdown (gpointer hook_data, gpointer user_data)
{
    GList *operations = g_list_copy (file_open_load_operations);

    (void)hook_data;
    (void)user_data;
    for (GList *node = operations; node; node = node->next)
    {
        GncFileOpenOperation *operation = node->data;
        if (operation->load_registered && operation->new_session)
            (void)qof_session_cancel_active_load (operation->new_session);
    }
    g_list_free (operations);
}

static void
file_open_load_registry_add (GncFileOpenOperation *operation,
                             GtkWindow *parent)
{
    g_return_if_fail (operation != NULL);
    g_return_if_fail (!operation->load_registered);

    if (!file_open_load_shutdown_hook_registered)
    {
        gnc_hook_add_dangler (HOOK_UI_SHUTDOWN,
                              (GFunc)file_open_ui_shutdown, NULL, NULL);
        file_open_load_shutdown_hook_registered = TRUE;
    }
    operation->load_registered = TRUE;
    file_open_load_operations = g_list_prepend (file_open_load_operations,
                                                operation);
    if (parent)
        operation->parent_destroy_handler = g_signal_connect (
            parent, "destroy", G_CALLBACK (file_open_parent_destroyed), operation);
}

static void
file_open_operation_free (GncFileOpenOperation *operation)
{
    GncSessionTransition *transition = operation->transition;

    operation->transition = NULL;
    file_open_load_registry_remove (operation);
    gnc_session_load_executor_free (operation->load_executor);
    operation->load_executor = NULL;
    g_weak_ref_clear (&operation->parent);
    g_free (operation->filename);
    g_free (operation->expected_url);
    g_free (operation);
    if (transition)
        gnc_session_transition_complete (transition);
}

static GncSessionTransition *
file_open_operation_take_transition (GncFileOpenOperation *operation)
{
    GncSessionTransition *transition = operation->transition;

    operation->transition = NULL;
    return transition;
}

static void
file_open_discard_new_session (GncFileOpenOperation *operation)
{
    if (!operation->new_session)
        return;
    if (file_session_destroy (operation->new_session))
        operation->new_session = NULL;
}

static gboolean
file_open_operation_is_current (GncFileOpenOperation *operation)
{
    QofSession *session;

    if (gnc_current_session_get_generation () != operation->expected_generation)
        return FALSE;
    if (!operation->expected_session)
        return !gnc_current_session_exist ();
    if (!gnc_current_session_exist ())
        return FALSE;
    session = gnc_get_current_session ();
    return session == operation->expected_session &&
           qof_session_get_book (session) == operation->expected_book &&
           g_strcmp0 (qof_session_get_url (session), operation->expected_url) == 0;
}

static void
file_open_operation_fail (GncFileOpenOperation *operation)
{
    file_open_discard_new_session (operation);
    file_open_operation_free (operation);
}

static void
file_open_commit (GncFileOpenOperation *operation, GtkWindow *parent)
{
    QofSession *new_session = operation->new_session;
    QofSessionOperationLease *close_lease = NULL;

    if (!new_session || !file_open_operation_is_current (operation))
    {
        file_open_operation_fail (operation);
        return;
    }

    if (operation->expected_session)
    {
        close_lease = qof_session_operation_lease_acquire_for (
            operation->expected_session, QOF_SESSION_OPERATION_CLOSE);
        if (!close_lease)
        {
            file_open_operation_fail (operation);
            return;
        }
    }

    qof_event_suspend ();
    if (operation->expected_session)
    {
        gnc_hook_run (HOOK_BOOK_CLOSED, operation->expected_session);
        gnc_close_gui_component_by_session (operation->expected_session);
        gnc_state_save (operation->expected_session);
        if (!file_open_operation_is_current (operation) ||
            !gnc_clear_current_session_with_lease (close_lease))
        {
            qof_session_operation_lease_release (close_lease);
            qof_event_resume ();
            file_open_operation_fail (operation);
            return;
        }
        qof_session_operation_lease_release (close_lease);
    }

    gnc_set_current_session (new_session);
    operation->new_session = NULL;
    qof_event_resume ();

    gnc_add_history (new_session);
    gnc_gui_refresh_all ();
    gnc_book_opened ();
    run_post_load_scrubs (parent, qof_session_get_book (new_session));
    gnc_main_window_show_all_windows ();
    file_open_operation_free (operation);
}

static void
file_open_error_finished (GtkWindow *parent, gboolean uh_oh, gpointer user_data)
{
    GncFileOpenOperation *operation = user_data;

    (void)parent;
    (void)uh_oh;
    file_open_operation_fail (operation);
}

static void
file_open_bad_url_finished (GtkWindow *parent, gboolean uh_oh, gpointer user_data)
{
    GncFileOpenOperation *operation = user_data;
    GncSessionTransition *transition;
    gchar *directory;

    (void)uh_oh;
    if ((operation->has_parent && !parent) ||
        !file_open_operation_is_current (operation))
    {
        file_open_operation_fail (operation);
        return;
    }

    if (g_file_test (operation->filename, G_FILE_TEST_IS_DIR))
        directory = g_strdup (operation->filename);
    else
        directory = gnc_get_default_directory (GNC_PREFS_GROUP_OPEN_SAVE);
    transition = file_open_operation_take_transition (operation);
    file_open_operation_free (operation);
    gnc_file_open_request_dialog_with_transition (parent, directory,
                                                  transition);
    g_free (directory);
}

static void
file_open_create_finished (GtkWindow *parent, gboolean uh_oh, gpointer user_data)
{
    GncFileOpenOperation *operation = user_data;

    if ((!operation->has_parent || parent) && !uh_oh &&
        file_open_operation_is_current (operation))
    {
        operation->mode = GNC_FILE_OPEN_NEW_STORE;
        file_open_start (operation);
        return;
    }
    file_open_operation_fail (operation);
}

static void
file_open_locked_choice_finished (GtkWindow *parent, gint choice, gpointer user_data)
{
    GncFileOpenOperation *operation = user_data;
    GncSessionTransition *transition;

    if ((operation->has_parent && !parent) ||
        !file_open_operation_is_current (operation))
    {
        file_open_operation_fail (operation);
        return;
    }

    switch (choice)
    {
    case 0: /* Open Read-Only */
        operation->is_readonly = TRUE;
        operation->mode = GNC_FILE_OPEN_NORMAL;
        file_open_start (operation);
        return;
    case 1: /* Create New File */
        transition = file_open_operation_take_transition (operation);
        file_open_operation_free (operation);
        gnc_file_new_from_transition (parent, transition);
        return;
    case 2: /* Open Anyway */
        operation->mode = GNC_FILE_OPEN_BREAK_LOCK;
        file_open_start (operation);
        return;
    case 4: /* Quit */
        transition = file_open_operation_take_transition (operation);
        file_open_operation_free (operation);
        gnc_session_transition_begin_shutdown (transition);
        if (shutdown_cb)
            shutdown_cb (0);
        return;
    default: /* Open Folder and cancellation */
        transition = file_open_operation_take_transition (operation);
        file_open_operation_free (operation);
        gnc_file_open_request_dialog_with_transition (parent, NULL,
                                                      transition);
        return;
    }
}

static void
file_open_prompt_locked (GncFileOpenOperation *operation, GtkWindow *parent,
                         QofBackendError error, const gchar *newfile)
{
    const gchar *detail = error == ERR_BACKEND_LOCKED ?
        _("That database may be in use by another user, in which case you "
          "should not open the database. What would you like to do?") :
        _("That database may be on a read-only file system, you may not have "
          "write permission for the directory, or your anti-virus software is "
          "preventing this action. If you proceed you may not be able to save "
          "any changes. What would you like to do?");
    GList *choices = NULL;
    gchar *displayname;
    gchar *title;

    if (!gnc_uri_is_file_uri (newfile))
        displayname = gnc_uri_normalize_uri (newfile, FALSE);
    else
        displayname = gnc_uri_get_path (newfile);
    title = g_strdup_printf (_("GnuCash could not obtain the lock for %s."),
                             displayname);
    choices = g_list_append (choices, _("Open Read-Only"));
    choices = g_list_append (choices, _("Create New File"));
    choices = g_list_append (choices, _("Open Anyway"));
    choices = g_list_append (choices, _("Open Folder"));
    if (shutdown_cb)
        choices = g_list_append (choices, _("Quit"));
    gnc_choose_option_dialog_async (parent, title, detail, choices,
                                    shutdown_cb ? 4 : 3,
                                    file_open_locked_choice_finished, operation);
    g_list_free (choices);
    g_free (title);
    g_free (displayname);
}

static void file_open_after_loaded_error (GtkWindow *parent, gboolean uh_oh,
                                          gpointer user_data);

static void
file_open_upgrade_finished (GtkWindow *parent, gboolean uh_oh, gpointer user_data)
{
    GncFileOpenOperation *operation = user_data;

    if ((operation->has_parent && !parent) ||
        !file_open_operation_is_current (operation))
    {
        file_open_operation_fail (operation);
        return;
    }
    if (!uh_oh)
    {
        file_open_commit (operation, parent);
        return;
    }
    if (operation->last_error == ERR_SQL_DB_TOO_OLD ||
        operation->last_error == ERR_SQL_DB_TOO_NEW)
    {
        qof_book_mark_readonly (qof_session_get_book (operation->new_session));
        file_open_commit (operation, parent);
        return;
    }
    file_open_operation_fail (operation);
}

static void
file_open_after_loaded_error (GtkWindow *parent, gboolean uh_oh, gpointer user_data)
{
    GncFileOpenOperation *operation = user_data;
    Account *new_root;

    if ((operation->has_parent && !parent) ||
        !file_open_operation_is_current (operation))
    {
        file_open_operation_fail (operation);
        return;
    }
    if (uh_oh)
    {
        if (operation->last_error == ERR_SQL_DB_TOO_OLD ||
            operation->last_error == ERR_SQL_DB_TOO_NEW)
        {
            qof_book_mark_readonly (qof_session_get_book (operation->new_session));
            file_open_commit (operation, parent);
        }
        else
            file_open_operation_fail (operation);
        return;
    }

    if (operation->last_error == ERR_SQL_DB_TOO_OLD)
    {
        QofSessionOperationLease *lease =
            qof_session_operation_lease_acquire_for (
                operation->new_session, QOF_SESSION_OPERATION_OPEN);
        gboolean authorized = lease != NULL;

        gnc_window_show_progress (_("Re-saving user data…"), 0.0);
        if (authorized)
            authorized = qof_session_safe_save_with_lease (
                operation->new_session, lease, gnc_window_show_progress);
        gnc_window_show_progress (NULL, -1.0);
        qof_session_operation_lease_release (lease);
        operation->last_error = authorized ?
            qof_session_get_error (operation->new_session) : ERR_BACKEND_MISC;
        show_session_error_async (parent, operation->last_error, operation->filename,
                                  GNC_FILE_DIALOG_SAVE, file_open_upgrade_finished,
                                  operation);
        return;
    }

    new_root = gnc_book_get_root_account (qof_session_get_book (operation->new_session));
    if (!new_root)
    {
        show_session_error_async (parent, ERR_BACKEND_MISC, operation->filename,
                                  GNC_FILE_DIALOG_OPEN, file_open_error_finished,
                                  operation);
        return;
    }

    {
        QofBook *book = qof_session_get_book (operation->new_session);
        gchar *msg = gnc_features_test_unknown (book);
        Account *template_root = gnc_book_get_template_root (book);

        if (msg)
        {
            gnc_error_dialog (parent, msg, "");
            g_free (msg);
            file_open_operation_fail (operation);
            return;
        }
        if (template_root)
        {
            GList *children = gnc_account_get_descendants (template_root);
            for (GList *child = children; child; child = child->next)
            {
                Account *account = GNC_ACCOUNT (child->data);
                GList *splits = xaccAccountGetSplitList (account);
                g_list_foreach (splits, (GFunc)gnc_sx_scrub_split_numerics, NULL);
                g_list_free (splits);
            }
            g_list_free (children);
        }
    }
    file_open_commit (operation, parent);
}

static void
file_open_load_finished (QofSession *session, QofSessionLoadAsyncStatus status,
                         gpointer user_data)
{
    GncFileOpenOperation *operation = user_data;
    GtkWindow *parent = GTK_WINDOW (g_weak_ref_get (&operation->parent));

    /* The terminal callback is now the sole owner of the operation. */
    file_open_load_registry_remove (operation);
    gnc_window_show_progress (NULL, -1.0);
    if (session != operation->new_session ||
        (operation->has_parent && !parent) ||
        !file_open_operation_is_current (operation))
    {
        g_clear_object (&parent);
        file_open_operation_fail (operation);
        return;
    }

    if (status == QOF_SESSION_LOAD_ERROR)
    {
        operation->last_error = qof_session_pop_error (operation->new_session);
        if (operation->last_error == ERR_BACKEND_NO_ERR)
            operation->last_error = ERR_BACKEND_MISC;
        show_session_error_async (parent, operation->last_error,
                                  operation->filename, GNC_FILE_DIALOG_OPEN,
                                  file_open_error_finished, operation);
        g_clear_object (&parent);
        return;
    }
    if (status != QOF_SESSION_LOAD_COMPLETED)
    {
        g_clear_object (&parent);
        file_open_operation_fail (operation);
        return;
    }

    if (operation->is_readonly)
        qof_book_mark_readonly (qof_session_get_book (operation->new_session));
    operation->last_error = qof_session_pop_error (operation->new_session);
    show_session_error_async (parent, operation->last_error, operation->filename,
                              GNC_FILE_DIALOG_OPEN, file_open_after_loaded_error,
                              operation);
    g_clear_object (&parent);
}

static void
file_open_start (GncFileOpenOperation *operation)
{
    GtkWindow *parent = GTK_WINDOW (g_weak_ref_get (&operation->parent));
    gchar *newfile;
    gchar *scheme = NULL;
    gchar *hostname = NULL;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path = NULL;
    gint32 port = 0;
    QofSessionOperationLease *lease;
    gboolean authorized;

    if ((operation->has_parent && !parent) ||
        !file_open_operation_is_current (operation))
    {
        g_clear_object (&parent);
        file_open_operation_fail (operation);
        return;
    }

    newfile = gnc_uri_normalize_uri (operation->filename, TRUE);
    if (!newfile)
    {
        show_session_error_async (parent, ERR_FILEIO_FILE_NOT_FOUND,
                                  operation->filename, GNC_FILE_DIALOG_OPEN,
                                  file_open_error_finished, operation);
        g_clear_object (&parent);
        return;
    }
    gnc_uri_get_components (newfile, &scheme, &hostname, &port, &username,
                            &password, &path);
    if (gnc_uri_is_file_scheme (scheme))
    {
        gchar *default_dir = g_path_get_dirname (path);
        gnc_set_default_directory (GNC_PREFS_GROUP_OPEN_SAVE, default_dir);
        g_free (default_dir);
    }

    operation->new_session = qof_session_new (qof_book_new ());
    lease = qof_session_operation_lease_acquire_for (
        operation->new_session, QOF_SESSION_OPERATION_OPEN);
    authorized = lease && qof_session_begin_with_lease (
        operation->new_session, lease, newfile,
        operation->mode == GNC_FILE_OPEN_BREAK_LOCK ?
        SESSION_BREAK_LOCK :
        operation->mode == GNC_FILE_OPEN_NEW_STORE ?
        SESSION_NEW_STORE :
        (operation->is_readonly ? SESSION_READ_ONLY : SESSION_NORMAL_OPEN));
    qof_session_operation_lease_release (lease);
    operation->last_error = authorized ?
        qof_session_get_error (operation->new_session) : ERR_BACKEND_MISC;
    if (operation->last_error != ERR_BACKEND_NO_ERR)
    {
        QofBackendError error = operation->last_error;

        file_open_discard_new_session (operation);
        if (error == ERR_BACKEND_BAD_URL)
            show_session_error_async (parent, error, newfile, GNC_FILE_DIALOG_OPEN,
                                      file_open_bad_url_finished, operation);
        else if (error == ERR_BACKEND_LOCKED || error == ERR_BACKEND_READONLY)
            file_open_prompt_locked (operation, parent, error, newfile);
        else if (error == ERR_BACKEND_NO_SUCH_DB &&
                 operation->mode == GNC_FILE_OPEN_NORMAL)
            show_session_error_async (parent, error, newfile, GNC_FILE_DIALOG_OPEN,
                                      file_open_create_finished, operation);
        else
            show_session_error_async (parent, error, newfile, GNC_FILE_DIALOG_OPEN,
                                      file_open_error_finished, operation);
        g_free (scheme);
        g_free (hostname);
        g_free (username);
        g_free (password);
        g_free (path);
        g_free (newfile);
        g_clear_object (&parent);
        return;
    }

    gnc_window_show_progress (_("Loading user data…"), 0.0);
    lease = qof_session_operation_lease_acquire_for (
        operation->new_session, QOF_SESSION_OPERATION_LOAD);
    operation->load_executor = gnc_session_load_executor_new ();
    authorized = lease && qof_session_load_async_with_lease (
        operation->new_session, lease, gnc_window_show_progress,
        gnc_session_load_executor_get (operation->load_executor),
        file_open_load_finished, operation);
    if (authorized)
    {
        /* The terminal callback owns the operation after the LOAD handoff. */
        file_open_load_registry_add (operation, parent);
        g_free (scheme);
        g_free (hostname);
        g_free (username);
        g_free (password);
        g_free (path);
        g_free (newfile);
        g_clear_object (&parent);
        return;
    }
    qof_session_operation_lease_release (lease);
    gnc_session_load_executor_free (operation->load_executor);
    operation->load_executor = NULL;
    gnc_window_show_progress (NULL, -1.0);
    if (!authorized)
    {
        operation->last_error = ERR_BACKEND_MISC;
        show_session_error_async (parent, operation->last_error, newfile,
                                  GNC_FILE_DIALOG_OPEN, file_open_error_finished,
                                  operation);
        goto file_open_start_out;
    }

file_open_start_out:
    g_free (scheme);
    g_free (hostname);
    g_free (username);
    g_free (password);
    g_free (path);
    g_free (newfile);
    g_clear_object (&parent);
}
static void
gnc_file_open_after_xml_conversion (GObject *source, GAsyncResult *result,
                                    gpointer user_data)
{
    GncFileOpenRequest *request = user_data;
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    GError *error = NULL;

    if (gnc_xml_convert_single_file_finish (result, &error))
    {
        if (request->reset_bayes_conversion)
            gnc_account_reset_convert_bayes_to_flat ();
        if (gnc_post_file_open (parent, request->filename,
                                request->is_readonly,
                                request->reset_bayes_conversion,
                                request->break_lock,
                                request->transition))
            request->transition = NULL;
    }
    else if (error && !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
        gnc_error_dialog (parent, "%s", error->message);

    g_clear_error (&error);
    g_clear_object (&parent);
    gnc_file_open_request_free (request);
    (void)source;
}

static gboolean
gnc_file_open_request (GtkWindow *parent, const char *filename,
                       gboolean is_readonly,
                       gboolean reset_bayes_conversion,
                       GncSessionTransition *transition)
{
    return gnc_file_open_request_with_mode (parent, filename, is_readonly,
                                            reset_bayes_conversion, FALSE,
                                            transition);
}

static gboolean
gnc_file_open_request_with_mode (GtkWindow *parent, const char *filename,
                                  gboolean is_readonly,
                                  gboolean reset_bayes_conversion,
                                  gboolean break_lock,
                                  GncSessionTransition *transition)
{
    GncFileOpenRequest *request;

    if (!filename || *filename == '\0')
        return FALSE;

    if (!gnc_file_needs_xml_encoding_conversion (filename))
    {
        if (reset_bayes_conversion)
            gnc_account_reset_convert_bayes_to_flat ();
        return gnc_post_file_open (parent, filename, is_readonly,
                                   reset_bayes_conversion, break_lock,
                                   transition);
    }

    request = g_new0 (GncFileOpenRequest, 1);
    g_weak_ref_init (&request->parent, parent);
    request->filename = g_strdup (filename);
    request->is_readonly = is_readonly;
    request->reset_bayes_conversion = reset_bayes_conversion;
    request->break_lock = break_lock;
    request->transition = transition;
    gnc_xml_convert_single_file_async (filename, parent, NULL,
                                       gnc_file_open_after_xml_conversion,
                                       request);
    return TRUE;
}
static gboolean
gnc_post_file_open (GtkWindow *parent, const char *filename, gboolean is_readonly,
                    gboolean reset_bayes_conversion, gboolean break_lock,
                    GncSessionTransition *transition)
{
    GncFileOpenOperation *operation;
    GncFilePasswordRequest *password_request;
    QofSession *session = NULL;
    gchar *newfile;
    gchar *scheme = NULL;
    gchar *hostname = NULL;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path = NULL;
    gint32 port = 0;

    ENTER ("opening requested file");
    if (!filename || !*filename)
        return FALSE;

    newfile = gnc_uri_normalize_uri (filename, TRUE);
    if (!newfile)
    {
        show_session_error_async (parent, ERR_FILEIO_FILE_NOT_FOUND, filename,
                                  GNC_FILE_DIALOG_OPEN, NULL, NULL);
        return FALSE;
    }
    gnc_uri_get_components (newfile, &scheme, &hostname, &port, &username,
                            &password, &path);

    if (!gnc_uri_is_file_scheme (scheme) && !password)
    {
        password_request = g_new0 (GncFilePasswordRequest, 1);
        g_weak_ref_init (&password_request->parent, parent);
        password_request->has_parent = parent != NULL;
        password_request->scheme = g_strdup (scheme);
        password_request->hostname = g_strdup (hostname);
        password_request->port = port;
        password_request->path = g_strdup (path);
        password_request->is_readonly = is_readonly;
        password_request->reset_bayes_conversion = reset_bayes_conversion;
        password_request->break_lock = break_lock;
        password_request->transition = transition;
        gnc_keyring_get_password_async (parent, password_request->scheme,
                                        password_request->hostname,
                                        password_request->port,
                                        password_request->path, username, NULL,
                                        gnc_file_open_after_keyring_password,
                                        password_request);
        g_free (scheme);
        g_free (hostname);
        g_free (username);
        g_free (password);
        g_free (path);
        g_free (newfile);
        return TRUE;
    }

    operation = g_new0 (GncFileOpenOperation, 1);
    g_weak_ref_init (&operation->parent, parent);
    operation->has_parent = parent != NULL;
    operation->filename = g_strdup (filename);
    operation->is_readonly = is_readonly;
    operation->reset_bayes_conversion = reset_bayes_conversion;
    operation->mode = break_lock ? GNC_FILE_OPEN_BREAK_LOCK : GNC_FILE_OPEN_NORMAL;
    operation->transition = transition;
    operation->expected_generation = gnc_current_session_get_generation ();
    if (gnc_current_session_exist ())
    {
        session = gnc_get_current_session ();
        operation->expected_session = session;
        operation->expected_book = qof_session_get_book (session);
        operation->expected_url = g_strdup (qof_session_get_url (session));
    }

    g_free (scheme);
    g_free (hostname);
    g_free (username);
    g_free (password);
    g_free (path);
    g_free (newfile);
    file_open_start (operation);
    return TRUE;
}
/* Routine that pops up a file chooser dialog
 *
 * Note: this dialog is used when dbi is not enabled
 *       so the paths used in here are always file
 *       paths, never db uris.
 */
typedef struct
{
    GWeakRef parent;
    gboolean has_parent;
    gchar *filename;
    gboolean open_readonly;
    gboolean choose_file;
    GncSessionTransition *transition;
} GncFileOpenTransitionRequest;

static void
gnc_file_open_transition_request_free (GncFileOpenTransitionRequest *request)
{
    GncSessionTransition *transition = request->transition;

    request->transition = NULL;
    g_weak_ref_clear (&request->parent);
    g_free (request->filename);
    g_free (request);
    if (transition)
        gnc_session_transition_complete (transition);
}

static void
gnc_file_open_transition_selected (GtkWindow *parent, const gchar *filename,
                                   gpointer user_data)
{
    GncFileOpenTransitionRequest *request = user_data;
    GncSessionTransition *transition = request->transition;

    request->transition = NULL;
    if (!gnc_file_open_request (parent, filename, FALSE, FALSE, transition))
        request->transition = transition;
}

static void
gnc_file_open_transition_cancelled (GtkWindow *parent, const GError *error,
                                    gpointer user_data)
{
    (void)parent;
    (void)error;
    (void)user_data;
}

static void
gnc_file_open_transition_choose (GncFileOpenTransitionRequest *request,
                                 GtkWindow *parent,
                                 const gchar *starting_dir)
{
    gnc_file_select_async_full (
        parent, _("Open"), gnc_file_dialog_get_datafile_filters (), starting_dir,
        GNC_FILE_DIALOG_OPEN, gnc_file_open_transition_selected,
        gnc_file_open_transition_cancelled, request,
        (GDestroyNotify)gnc_file_open_transition_request_free);
}

static void
gnc_file_open_request_dialog_with_transition (
    GtkWindow *parent, const gchar *starting_dir,
    GncSessionTransition *transition)
{
    GncFileOpenTransitionRequest *request =
        g_new0 (GncFileOpenTransitionRequest, 1);

    g_weak_ref_init (&request->parent, parent);
    request->has_parent = parent != NULL;
    request->choose_file = TRUE;
    request->transition = transition;
    gnc_file_open_transition_choose (request, parent, starting_dir);
}

/* Starts the native file request only after the current session has either
 * been saved or explicitly discarded. */
static void
gnc_file_open_after_query (GtkWindow *parent, gboolean can_continue,
                           gpointer user_data)
{
    GncFileOpenTransitionRequest *request = user_data;
    GncSessionTransition *transition;
    gchar *default_dir;
    gchar *last;

    if (!can_continue)
    {
        gnc_file_open_transition_request_free (request);
        return;
    }

    if (!request->choose_file)
    {
        transition = request->transition;
        request->transition = NULL;
        if (!gnc_file_open_request (parent, request->filename,
                                    request->open_readonly,
                                    /*reset_bayes_conversion*/ TRUE,
                                    transition))
            request->transition = transition;
        gnc_file_open_transition_request_free (request);
        return;
    }

    last = gnc_history_get_last ();
    if (last && gnc_uri_targets_local_fs (last))
    {
        gchar *filepath = gnc_uri_get_path (last);

        default_dir = g_path_get_dirname (filepath);
        g_free (filepath);
    }
    else
        default_dir = gnc_get_default_directory (GNC_PREFS_GROUP_OPEN_SAVE);

    gnc_file_open_transition_choose (request, parent, default_dir);
    g_free (last);
    g_free (default_dir);

    /* Keep a valid empty session if the native chooser is cancelled. */
    gnc_get_current_session ();
}

static void
gnc_file_open_transition_start (GncSessionTransition *transition,
                                gpointer user_data)
{
    GncFileOpenTransitionRequest *request = user_data;
    GtkWindow *parent = GTK_WINDOW (g_weak_ref_get (&request->parent));

    request->transition = transition;
    if (request->has_parent && !parent)
    {
        gnc_file_open_transition_request_free (request);
        return;
    }
    gnc_file_query_save_async (parent, TRUE, gnc_file_open_after_query, request);
    g_clear_object (&parent);
}

gboolean
gnc_file_open (GtkWindow *parent)
{
    GncFileOpenTransitionRequest *request =
        g_new0 (GncFileOpenTransitionRequest, 1);
    GncSessionTransitionDisposition disposition;

    g_weak_ref_init (&request->parent, parent);
    request->has_parent = parent != NULL;
    request->choose_file = TRUE;
    disposition = gnc_session_transition_enqueue (
        GNC_SESSION_TRANSITION_OPEN, gnc_file_open_transition_start,
        (GncSessionTransitionCancelFunc)gnc_file_open_transition_request_free,
        request);
    if (disposition == GNC_SESSION_TRANSITION_REJECTED)
        gnc_file_open_transition_request_free (request);
    return disposition != GNC_SESSION_TRANSITION_REJECTED;
}

GncFileOpenResult
gnc_file_open_file (GtkWindow *parent, const char *newfile, gboolean open_readonly)
{
    GncFileOpenTransitionRequest *request;
    GncSessionTransitionDisposition disposition;

    if (!newfile)
        return GNC_FILE_OPEN_REJECTED;

    request = g_new0 (GncFileOpenTransitionRequest, 1);
    g_weak_ref_init (&request->parent, parent);
    request->has_parent = parent != NULL;
    request->filename = g_strdup (newfile);
    request->open_readonly = open_readonly;
    disposition = gnc_session_transition_enqueue (
        GNC_SESSION_TRANSITION_OPEN, gnc_file_open_transition_start,
        (GncSessionTransitionCancelFunc)gnc_file_open_transition_request_free,
        request);
    if (disposition == GNC_SESSION_TRANSITION_REJECTED)
    {
        gnc_file_open_transition_request_free (request);
        return GNC_FILE_OPEN_REJECTED;
    }
    return disposition == GNC_SESSION_TRANSITION_QUEUED ?
        GNC_FILE_OPEN_QUEUED : GNC_FILE_OPEN_STARTED;
}
/* Note: this dialog will only be used when dbi is not enabled
 *       paths used in it always refer to files and are
 *       never db uris
 */
static void
gnc_file_export_selected (GtkWindow *parent, const gchar *filename,
                          gpointer user_data)
{
    (void)user_data;
    gnc_file_do_export (parent, filename);
}

void
gnc_file_export (GtkWindow *parent)
{
    gchar *default_dir;
    gchar *last;

    ENTER (" ");

    last = gnc_history_get_last ();
    if (last && gnc_uri_targets_local_fs (last))
    {
        gchar *filepath = gnc_uri_get_path (last);

        default_dir = g_path_get_dirname (filepath);
        g_free (filepath);
    }
    else
        default_dir = gnc_get_default_directory (GNC_PREFS_GROUP_EXPORT);

    gnc_file_select_async (parent, _("Save"),
                           gnc_file_dialog_get_datafile_filters (), default_dir,
                           GNC_FILE_DIALOG_EXPORT, gnc_file_export_selected);
    g_free (last);
    g_free (default_dir);

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


typedef enum
{
    GNC_FILE_EXPORT_NEW_STORE,
    GNC_FILE_EXPORT_OVERWRITE,
    GNC_FILE_EXPORT_BREAK_LOCK,
} GncFileExportMode;

typedef struct
{
    GWeakRef parent;
    gboolean has_parent;
    gchar *filename;
    QofSession *session;
    QofBook *book;
    gchar *session_url;
    guint64 session_generation;
    GncFileExportMode mode;
} GncFileExportRequest;

static void file_export_start (GncFileExportRequest *request);

static void
file_export_request_free (GncFileExportRequest *request)
{
    g_weak_ref_clear (&request->parent);
    g_free (request->filename);
    g_free (request->session_url);
    g_free (request);
}

static gboolean
file_export_request_is_current (GncFileExportRequest *request)
{
    QofSession *session;

    if (gnc_current_session_get_generation () != request->session_generation)
        return FALSE;
    if (!gnc_current_session_exist ())
        return FALSE;
    session = gnc_get_current_session ();
    return session == request->session &&
           qof_session_get_book (session) == request->book &&
           g_strcmp0 (qof_session_get_url (session), request->session_url) == 0;
}

static void
file_export_complete (GncFileExportRequest *request)
{
    file_export_request_free (request);
}

static void
file_export_overwrite_finished (GtkWindow *parent, gint response,
                                gpointer user_data)
{
    GncFileExportRequest *request = user_data;

    if ((!request->has_parent || parent) &&
        response == GTK_RESPONSE_YES && file_export_request_is_current (request))
    {
        request->mode = GNC_FILE_EXPORT_OVERWRITE;
        file_export_start (request);
        return;
    }
    file_export_complete (request);
}

static void
file_export_lock_finished (GtkWindow *parent, gboolean uh_oh, gpointer user_data)
{
    GncFileExportRequest *request = user_data;

    if ((!request->has_parent || parent) && !uh_oh &&
        file_export_request_is_current (request))
    {
        request->mode = GNC_FILE_EXPORT_BREAK_LOCK;
        file_export_start (request);
        return;
    }
    file_export_complete (request);
}

static void
file_export_error_finished (GtkWindow *parent, gboolean uh_oh, gpointer user_data)
{
    GncFileExportRequest *request = user_data;

    (void)parent;
    (void)uh_oh;
    file_export_complete (request);
}

static void
file_export_begin_error (GncFileExportRequest *request, GtkWindow *parent,
                         QofBackendError error, const gchar *newfile)
{
    if (error == ERR_BACKEND_STORE_EXISTS &&
        request->mode == GNC_FILE_EXPORT_NEW_STORE)
    {
        gchar *name = gnc_uri_is_file_uri (newfile) ?
            gnc_uri_get_path (newfile) : gnc_uri_normalize_uri (newfile, FALSE);
        const gchar *format = _("The file %s already exists. Are you sure you want to overwrite it?");

        gnc_verify_dialog_async (parent, FALSE, file_export_overwrite_finished,
                                 request, format, name);
        g_free (name);
        return;
    }
    if (error == ERR_BACKEND_LOCKED &&
        request->mode != GNC_FILE_EXPORT_BREAK_LOCK)
    {
        show_session_error_async (parent, error, newfile, GNC_FILE_DIALOG_EXPORT,
                                  file_export_lock_finished, request);
        return;
    }

    show_session_error_async (parent, error, newfile, GNC_FILE_DIALOG_EXPORT,
                              file_export_error_finished, request);
}

static void
file_export_start (GncFileExportRequest *request)
{
    QofSession *current_session;
    QofSession *new_session;
    GtkWindow *parent = GTK_WINDOW (g_weak_ref_get (&request->parent));
    gboolean ok;
    QofBackendError io_err;
    gchar *norm_file;
    gchar *newfile;
    gchar *scheme = NULL;
    gchar *hostname = NULL;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path = NULL;
    gint32 port = 0;
    QofSessionOperationLease *lease;
    GncFileSessionLeasePair pair = { NULL, NULL };
    gboolean authorized;

    if ((request->has_parent && !parent) || !file_export_request_is_current (request))
    {
        g_clear_object (&parent);
        file_export_complete (request);
        return;
    }

    norm_file = gnc_uri_normalize_uri (request->filename, TRUE);
    if (!norm_file)
    {
        show_session_error_async (parent, ERR_FILEIO_FILE_NOT_FOUND, request->filename,
                                  GNC_FILE_DIALOG_EXPORT, file_export_error_finished,
                                  request);
        g_clear_object (&parent);
        return;
    }
    newfile = gnc_uri_add_extension (norm_file, GNC_DATAFILE_EXT);
    g_free (norm_file);
    gnc_uri_get_components (newfile, &scheme, &hostname, &port, &username,
                            &password, &path);

    if (g_strcmp0 (scheme, "file") == 0)
    {
        gchar *uri;

        g_free (scheme);
        scheme = g_strdup ("xml");
        uri = gnc_uri_create_uri (scheme, hostname, port, username, password, path);
        g_free (newfile);
        newfile = uri;
    }

    if (gnc_uri_is_file_scheme (scheme))
    {
        gchar *default_dir;

        if (check_file_path (path))
        {
            show_session_error_async (parent, ERR_FILEIO_RESERVED_WRITE, newfile,
                                      GNC_FILE_DIALOG_EXPORT,
                                      file_export_error_finished, request);
            g_free (scheme);
            g_free (hostname);
            g_free (username);
            g_free (password);
            g_free (path);
            g_free (newfile);
            g_clear_object (&parent);
            return;
        }
        default_dir = g_path_get_dirname (path);
        gnc_set_default_directory (GNC_PREFS_GROUP_OPEN_SAVE, default_dir);
        g_free (default_dir);
    }

    current_session = gnc_get_current_session ();
    if (g_strcmp0 (qof_session_get_url (current_session), newfile) == 0)
    {
        show_session_error_async (parent, ERR_FILEIO_WRITE_ERROR, request->filename,
                                  GNC_FILE_DIALOG_EXPORT,
                                  file_export_error_finished, request);
        g_free (scheme);
        g_free (hostname);
        g_free (username);
        g_free (password);
        g_free (path);
        g_free (newfile);
        g_clear_object (&parent);
        return;
    }

    new_session = qof_session_new (NULL);
    lease = qof_session_operation_lease_acquire_for (
        new_session, QOF_SESSION_OPERATION_EXPORT);
    authorized = lease && qof_session_begin_with_lease (
        new_session, lease, newfile,
        request->mode == GNC_FILE_EXPORT_OVERWRITE ?
        SESSION_NEW_OVERWRITE :
        request->mode == GNC_FILE_EXPORT_BREAK_LOCK ?
        SESSION_BREAK_LOCK : SESSION_NEW_STORE);
    qof_session_operation_lease_release (lease);
    io_err = authorized ? qof_session_get_error (new_session) : ERR_BACKEND_MISC;
    if (io_err != ERR_BACKEND_NO_ERR)
    {
        (void)file_session_destroy (new_session);
        file_export_begin_error (request, parent, io_err, newfile);
        g_free (scheme);
        g_free (hostname);
        g_free (username);
        g_free (password);
        g_free (path);
        g_free (newfile);
        g_clear_object (&parent);
        return;
    }

    qof_event_suspend ();
    gnc_set_busy_cursor (NULL, TRUE);
    gnc_window_show_progress (_("Exporting file…"), 0.0);
    ok = FALSE;
    authorized = file_session_lease_pair_acquire (
        new_session, QOF_SESSION_OPERATION_EXPORT,
        current_session, QOF_SESSION_OPERATION_EXPORT, &pair);
    if (authorized)
        authorized = qof_session_export_with_leases (
            new_session, pair.lease_a, current_session, pair.lease_b,
            gnc_window_show_progress, &ok);
    file_session_lease_pair_release (&pair);
    if (!authorized)
        ok = FALSE;
    gnc_window_show_progress (NULL, -1.0);
    gnc_unset_busy_cursor (NULL);
    if (!file_session_destroy (new_session))
        ok = FALSE;
    qof_event_resume ();

    if (!ok)
        gnc_error_dialog (parent, _("There was an error saving the file.\n\n%s"),
                          strerror (errno));

    g_free (scheme);
    g_free (hostname);
    g_free (username);
    g_free (password);
    g_free (path);
    g_free (newfile);
    g_clear_object (&parent);
    file_export_complete (request);
}

void
gnc_file_do_export (GtkWindow *parent, const char *filename)
{
    GncFileExportRequest *request;
    QofSession *session;

    if (!filename || !gnc_current_session_exist ())
        return;

    session = gnc_get_current_session ();
    request = g_new0 (GncFileExportRequest, 1);
    g_weak_ref_init (&request->parent, parent);
    request->has_parent = parent != NULL;
    request->filename = g_strdup (filename);
    request->session = session;
    request->book = qof_session_get_book (session);
    request->session_url = g_strdup (qof_session_get_url (session));
    request->session_generation = gnc_current_session_get_generation ();
    request->mode = GNC_FILE_EXPORT_NEW_STORE;
    file_export_start (request);
}
static void gnc_file_save_as_with_completion (GtkWindow *parent,
                                               GncFileQuerySaveCallback completed,
                                               gpointer user_data);
static void gnc_file_do_save_as_async (GtkWindow *parent, const char *filename,
                                       GncFileQuerySaveCallback completed,
                                       gpointer user_data);
static void gnc_file_save_complete (GtkWindow *parent,
                                    GncFileQuerySaveCallback completed,
                                    gpointer user_data, gboolean saved);

typedef struct
{
    GWeakRef parent;
    gboolean has_parent;
    QofSession *session;
    QofBook *book;
    gchar *session_url;
    guint64 session_generation;
    GncFileQuerySaveCallback completed;
    gpointer user_data;
} GncFileSaveErrorRequest;

static void
file_save_error_request_free (GncFileSaveErrorRequest *request)
{
    g_weak_ref_clear (&request->parent);
    g_free (request->session_url);
    g_free (request);
}

static gboolean
file_save_error_request_is_current (GncFileSaveErrorRequest *request)
{
    QofSession *session;

    if (gnc_current_session_get_generation () != request->session_generation)
        return FALSE;
    if (!gnc_current_session_exist ())
        return FALSE;
    session = gnc_get_current_session ();
    return session == request->session &&
           qof_session_get_book (session) == request->book &&
           g_strcmp0 (qof_session_get_url (session), request->session_url) == 0;
}

static void
file_save_error_finished (GtkWindow *parent, gboolean uh_oh, gpointer user_data)
{
    GncFileSaveErrorRequest *request = user_data;
    GncFileQuerySaveCallback completed = request->completed;
    gpointer completed_data = request->user_data;

    (void)uh_oh;
    if ((request->has_parent && !parent) ||
        !file_save_error_request_is_current (request))
    {
        file_save_error_request_free (request);
        gnc_file_save_complete (parent, completed, completed_data, FALSE);
        return;
    }

    file_save_error_request_free (request);
    /* A failed regular save has always continued with Save As after the user
       has acknowledged its error. The new request owns the next decision. */
    gnc_file_save_as_with_completion (parent, completed, completed_data);
}

static void
gnc_file_save_complete (GtkWindow *parent, GncFileQuerySaveCallback completed,
                        gpointer user_data, gboolean saved)
{
    if (completed)
        completed (parent, saved, user_data);
}

typedef struct
{
    GncFileQuerySaveCallback completed;
    gpointer user_data;
} GncFileReadOnlySaveRequest;

static void
gnc_file_read_only_save_finished (GtkWindow *parent, gint response,
                                  gpointer user_data)
{
    GncFileReadOnlySaveRequest *request = user_data;

    if (response == GTK_RESPONSE_OK)
        gnc_file_save_as_with_completion (parent, request->completed, request->user_data);
    else
        gnc_file_save_complete (parent, request->completed, request->user_data, FALSE);
    g_free (request);
}

static void
gnc_file_read_only_save_as_async (GtkWindow *parent,
                                  GncFileQuerySaveCallback completed,
                                  gpointer user_data)
{
    GncFileReadOnlySaveRequest *request = g_new0 (GncFileReadOnlySaveRequest, 1);

    request->completed = completed;
    request->user_data = user_data;
    gnc_ok_cancel_dialog_async (parent, GTK_RESPONSE_CANCEL,
                                gnc_file_read_only_save_finished, request,
                                "%s", _("The database was opened read-only. "
                                        "Do you want to save it to a different location?"));
}


void
gnc_file_save_async (GtkWindow *parent,
                     GncFileQuerySaveCallback completed,
                     gpointer user_data)
{
    QofBackendError io_err;
    const char *newfile;
    QofSession *session;
    QofSessionOperationLease *lease;
    gboolean authorized;

    ENTER (" ");

    if (!gnc_current_session_exist ())
    {
        gnc_file_save_complete (parent, completed, user_data, TRUE);
        return;
    }

    session = gnc_get_current_session ();
    if (!strlen (qof_session_get_url (session)))
    {
        gnc_file_save_as_with_completion (parent, completed, user_data);
        return;
    }

    if (qof_book_is_readonly (qof_session_get_book (session)))
    {
        gnc_file_read_only_save_as_async (parent, completed, user_data);
        return;
    }

    lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_SAVE);
    if (!lease)
    {
        gnc_file_save_complete (parent, completed, user_data, FALSE);
        LEAVE ("Session operation already in progress.");
        return;
    }

    gnc_set_busy_cursor (NULL, TRUE);
    gnc_window_show_progress (_("Writing file…"), 0.0);
    authorized = qof_session_save_with_lease (
        session, lease, gnc_window_show_progress);
    gnc_window_show_progress (NULL, -1.0);
    gnc_unset_busy_cursor (NULL);
    qof_session_operation_lease_release (lease);
    if (!authorized)
    {
        gnc_file_save_complete (parent, completed, user_data, FALSE);
        LEAVE ("Session operation was invalidated.");
        return;
    }

    io_err = qof_session_get_error (session);
    if (ERR_BACKEND_NO_ERR != io_err)
    {
        GncFileSaveErrorRequest *request = g_new0 (GncFileSaveErrorRequest, 1);

        g_weak_ref_init (&request->parent, parent);
        request->has_parent = parent != NULL;
        request->session = session;
        request->book = qof_session_get_book (session);
        request->session_url = g_strdup (qof_session_get_url (session));
        request->session_generation = gnc_current_session_get_generation ();
        request->completed = completed;
        request->user_data = user_data;
        newfile = qof_session_get_url (session);
        show_session_error_async (parent, io_err, newfile, GNC_FILE_DIALOG_SAVE,
                                  file_save_error_finished, request);
        return;
    }

    xaccReopenLog ();
    gnc_add_history (session);
    gnc_hook_run (HOOK_BOOK_SAVED, session);
    gnc_file_save_complete (parent, completed, user_data, TRUE);
    LEAVE (" ");
}

typedef enum
{
    GNC_FILE_MANUAL_SAVE,
    GNC_FILE_MANUAL_SAVE_AS,
    GNC_FILE_MANUAL_SAVE_AS_FILE
} GncFileManualSaveKind;

typedef struct
{
    GWeakRef parent;
    gboolean has_parent;
    GncFileManualSaveKind kind;
    gchar *filename;
    QofSession *session;
    QofBook *book;
    gchar *session_url;
    guint64 session_generation;
    GncSessionTransition *transition;
} GncFileManualSaveRequest;

static void
gnc_file_manual_save_request_free (GncFileManualSaveRequest *request)
{
    g_weak_ref_clear (&request->parent);
    g_free (request->filename);
    g_free (request->session_url);
    g_free (request);
}

static gboolean
gnc_file_manual_save_request_is_current (GncFileManualSaveRequest *request)
{
    QofSession *session;

    if (!request->session)
        return !gnc_current_session_exist () &&
               gnc_current_session_get_generation () ==
                   request->session_generation;
    if (!gnc_current_session_exist () ||
        gnc_current_session_get_generation () != request->session_generation)
        return FALSE;
    session = gnc_get_current_session ();
    return session == request->session &&
           qof_session_get_book (session) == request->book &&
           g_strcmp0 (qof_session_get_url (session), request->session_url) == 0;
}

static void
gnc_file_manual_save_complete (GtkWindow *parent, gboolean saved,
                               gpointer user_data)
{
    GncFileManualSaveRequest *request = user_data;
    GncSessionTransition *transition = request->transition;

    (void)parent;
    (void)saved;
    request->transition = NULL;
    gnc_file_manual_save_request_free (request);
    gnc_session_transition_complete (transition);
}

static void
gnc_file_manual_save_start (GncSessionTransition *transition,
                            gpointer user_data)
{
    GncFileManualSaveRequest *request = user_data;
    GtkWindow *parent = GTK_WINDOW (g_weak_ref_get (&request->parent));

    request->transition = transition;
    if ((request->has_parent && !parent) ||
        !gnc_file_manual_save_request_is_current (request))
    {
        gnc_file_manual_save_request_free (request);
        gnc_session_transition_complete (transition);
        return;
    }

    switch (request->kind)
    {
    case GNC_FILE_MANUAL_SAVE:
        gnc_file_save_async (parent, gnc_file_manual_save_complete, request);
        break;
    case GNC_FILE_MANUAL_SAVE_AS:
        gnc_file_save_as_with_completion (
            parent, gnc_file_manual_save_complete, request);
        break;
    case GNC_FILE_MANUAL_SAVE_AS_FILE:
        gnc_file_do_save_as_async (
            parent, request->filename, gnc_file_manual_save_complete, request);
        break;
    }
    g_clear_object (&parent);
}

static void
gnc_file_manual_save_enqueue (GtkWindow *parent,
                              GncFileManualSaveKind kind,
                              const gchar *filename)
{
    GncFileManualSaveRequest *request =
        g_new0 (GncFileManualSaveRequest, 1);
    GncSessionTransitionDisposition disposition;

    g_weak_ref_init (&request->parent, parent);
    request->has_parent = parent != NULL;
    request->kind = kind;
    request->filename = g_strdup (filename);
    request->session_generation = gnc_current_session_get_generation ();
    if (gnc_current_session_exist ())
    {
        request->session = gnc_get_current_session ();
        request->book = qof_session_get_book (request->session);
        request->session_url = g_strdup (qof_session_get_url (request->session));
    }
    disposition = gnc_session_transition_enqueue (
        GNC_SESSION_TRANSITION_SAVE, gnc_file_manual_save_start,
        (GncSessionTransitionCancelFunc)gnc_file_manual_save_request_free,
        request);
    if (disposition == GNC_SESSION_TRANSITION_REJECTED)
        gnc_file_manual_save_request_free (request);
}

void
gnc_file_save (GtkWindow *parent)
{
    gnc_file_manual_save_enqueue (parent, GNC_FILE_MANUAL_SAVE, NULL);
}
/* Note: this dialog will only be used when dbi is not enabled
 *       paths used in it always refer to files and are
 *       never db uris. See gnc_file_do_save_as for that.
 */
typedef struct
{
    GWeakRef parent;
    gboolean has_parent;
    GncFileQuerySaveCallback completed;
    gpointer user_data;
} GncFileSaveAsRequest;

static void
gnc_file_save_as_request_free (GncFileSaveAsRequest *request)
{
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static void
gnc_file_save_as_complete (GncFileSaveAsRequest *request, GtkWindow *parent,
                           gboolean saved)
{
    if (request->has_parent && !parent)
        saved = FALSE;
    gnc_file_save_complete (parent, request->completed, request->user_data, saved);
}

static void
gnc_file_save_as_selected (GtkWindow *parent, const gchar *filename,
                           gpointer user_data)
{
    GncFileSaveAsRequest *request = user_data;
    gboolean saved = FALSE;

    (void)saved;
    if (filename)
        gnc_file_do_save_as_async (parent, filename, request->completed,
                                   request->user_data);
    else
        gnc_file_save_as_complete (request, parent, FALSE);
}

static void
gnc_file_save_as_cancelled (GtkWindow *parent, const GError *error,
                             gpointer user_data)
{
    GncFileSaveAsRequest *request = user_data;

    (void)error;
    gnc_file_save_as_complete (request, parent, FALSE);
}

static void
gnc_file_save_as_with_completion (GtkWindow *parent,
                                  GncFileQuerySaveCallback completed,
                                  gpointer user_data)
{
    gchar *default_dir;
    gchar *last;
    GncFileSaveAsRequest *request;

    ENTER (" ");

    if (!gnc_current_session_exist ())
    {
        gnc_file_save_complete (parent, completed, user_data, FALSE);
        LEAVE ("No Session.");
        return;
    }

    last = gnc_history_get_last ();
    if (last && gnc_uri_targets_local_fs (last))
    {
        gchar *filepath = gnc_uri_get_path (last);

        default_dir = g_path_get_dirname (filepath);
        g_free (filepath);
    }
    else
        default_dir = gnc_get_default_directory (GNC_PREFS_GROUP_OPEN_SAVE);

    request = g_new0 (GncFileSaveAsRequest, 1);
    g_weak_ref_init (&request->parent, parent);
    request->has_parent = parent != NULL;
    request->completed = completed;
    request->user_data = user_data;
    gnc_file_select_async_full (parent, _("Save"),
                                gnc_file_dialog_get_datafile_filters (), default_dir,
                                GNC_FILE_DIALOG_SAVE, gnc_file_save_as_selected,
                                gnc_file_save_as_cancelled, request,
                                (GDestroyNotify)gnc_file_save_as_request_free);
    g_free (last);
    g_free (default_dir);

    LEAVE (" ");
}

void
gnc_file_save_as (GtkWindow *parent)
{
    gnc_file_manual_save_enqueue (parent, GNC_FILE_MANUAL_SAVE_AS, NULL);
}

typedef enum
{
    GNC_FILE_SAVE_AS_NEW_STORE,
    GNC_FILE_SAVE_AS_CREATE_RETRY,
    GNC_FILE_SAVE_AS_OVERWRITE,
    GNC_FILE_SAVE_AS_BREAK_LOCK,
} GncFileSaveAsMode;

typedef struct
{
    GWeakRef parent;
    gboolean has_parent;
    gchar *filename;
    QofSession *session;
    QofBook *book;
    gchar *session_url;
    guint64 session_generation;
    GncFileSaveAsMode mode;
    GncFileQuerySaveCallback completed;
    gpointer user_data;
} GncFileSaveAsOperation;

static void file_save_as_start (GncFileSaveAsOperation *operation);

static void
file_save_as_operation_free (GncFileSaveAsOperation *operation)
{
    g_weak_ref_clear (&operation->parent);
    g_free (operation->filename);
    g_free (operation->session_url);
    g_free (operation);
}

static gboolean
file_save_as_operation_is_current (GncFileSaveAsOperation *operation)
{
    QofSession *session;

    if (gnc_current_session_get_generation () != operation->session_generation)
        return FALSE;
    if (!gnc_current_session_exist ())
        return FALSE;
    session = gnc_get_current_session ();
    return session == operation->session &&
           qof_session_get_book (session) == operation->book &&
           g_strcmp0 (qof_session_get_url (session), operation->session_url) == 0;
}

static void
file_save_as_operation_complete (GncFileSaveAsOperation *operation, gboolean saved)
{
    GtkWindow *parent = GTK_WINDOW (g_weak_ref_get (&operation->parent));

    if (operation->has_parent && !parent)
        saved = FALSE;
    gnc_file_save_complete (parent, operation->completed, operation->user_data, saved);
    g_clear_object (&parent);
    file_save_as_operation_free (operation);
}

static void
file_save_as_error_finished (GtkWindow *parent, gboolean uh_oh, gpointer user_data)
{
    GncFileSaveAsOperation *operation = user_data;

    (void)parent;
    (void)uh_oh;
    file_save_as_operation_complete (operation, FALSE);
}

static void
file_save_as_overwrite_finished (GtkWindow *parent, gint response,
                                 gpointer user_data)
{
    GncFileSaveAsOperation *operation = user_data;

    if ((!operation->has_parent || parent) && response == GTK_RESPONSE_YES &&
        file_save_as_operation_is_current (operation))
    {
        operation->mode = GNC_FILE_SAVE_AS_OVERWRITE;
        file_save_as_start (operation);
        return;
    }
    file_save_as_operation_complete (operation, FALSE);
}

static void
file_save_as_create_finished (GtkWindow *parent, gboolean uh_oh,
                              gpointer user_data)
{
    GncFileSaveAsOperation *operation = user_data;

    if ((!operation->has_parent || parent) && !uh_oh &&
        file_save_as_operation_is_current (operation))
    {
        operation->mode = GNC_FILE_SAVE_AS_CREATE_RETRY;
        file_save_as_start (operation);
        return;
    }
    file_save_as_operation_complete (operation, FALSE);
}

static void
file_save_as_lock_finished (GtkWindow *parent, gboolean uh_oh,
                            gpointer user_data)
{
    GncFileSaveAsOperation *operation = user_data;

    if ((!operation->has_parent || parent) && !uh_oh &&
        file_save_as_operation_is_current (operation))
    {
        operation->mode = GNC_FILE_SAVE_AS_BREAK_LOCK;
        file_save_as_start (operation);
        return;
    }
    file_save_as_operation_complete (operation, FALSE);
}

static void
file_save_as_begin_error (GncFileSaveAsOperation *operation, GtkWindow *parent,
                          QofBackendError error, const gchar *newfile)
{
    if (error == ERR_BACKEND_STORE_EXISTS &&
        operation->mode == GNC_FILE_SAVE_AS_NEW_STORE)
    {
        gchar *name = gnc_uri_is_file_uri (newfile) ?
            gnc_uri_get_path (newfile) : gnc_uri_normalize_uri (newfile, FALSE);
        const gchar *format = _("The file %s already exists. Are you sure you want to overwrite it?");

        gnc_verify_dialog_async (parent, FALSE, file_save_as_overwrite_finished,
                                 operation, format, name);
        g_free (name);
        return;
    }
    if (error == ERR_BACKEND_LOCKED &&
        operation->mode != GNC_FILE_SAVE_AS_BREAK_LOCK)
    {
        show_session_error_async (parent, error, newfile, GNC_FILE_DIALOG_SAVE,
                                  file_save_as_lock_finished, operation);
        return;
    }
    if ((error == ERR_FILEIO_FILE_NOT_FOUND || error == ERR_BACKEND_NO_SUCH_DB ||
         error == ERR_SQL_DB_TOO_OLD) &&
        operation->mode == GNC_FILE_SAVE_AS_NEW_STORE)
    {
        show_session_error_async (parent, error, newfile, GNC_FILE_DIALOG_SAVE,
                                  file_save_as_create_finished, operation);
        return;
    }

    show_session_error_async (parent, error, newfile, GNC_FILE_DIALOG_SAVE,
                              file_save_as_error_finished, operation);
}

static void
file_save_as_start (GncFileSaveAsOperation *operation)
{
    QofSession *session;
    QofSession *new_session;
    GtkWindow *parent = GTK_WINDOW (g_weak_ref_get (&operation->parent));
    gchar *norm_file;
    gchar *newfile;
    gchar *scheme = NULL;
    gchar *hostname = NULL;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *path = NULL;
    gint32 port = 0;
    QofBackendError io_err;
    QofSessionOperationLease *lease;
    GncFileSessionLeasePair pair = { NULL, NULL };
    gboolean authorized;

    if ((operation->has_parent && !parent) ||
        !file_save_as_operation_is_current (operation))
    {
        g_clear_object (&parent);
        file_save_as_operation_complete (operation, FALSE);
        return;
    }

    norm_file = gnc_uri_normalize_uri (operation->filename, TRUE);
    if (!norm_file)
    {
        show_session_error_async (parent, ERR_FILEIO_FILE_NOT_FOUND,
                                  operation->filename, GNC_FILE_DIALOG_SAVE,
                                  file_save_as_error_finished, operation);
        g_clear_object (&parent);
        return;
    }
    newfile = gnc_uri_add_extension (norm_file, GNC_DATAFILE_EXT);
    g_free (norm_file);
    gnc_uri_get_components (newfile, &scheme, &hostname, &port, &username,
                            &password, &path);

    if (g_strcmp0 (scheme, "file") == 0)
    {
        gchar *uri;

        g_free (scheme);
        scheme = g_strdup ("xml");
        uri = gnc_uri_create_uri (scheme, hostname, port, username, password, path);
        g_free (newfile);
        newfile = uri;
    }

    if (gnc_uri_is_file_scheme (scheme))
    {
        gchar *default_dir;

        if (check_file_path (path))
        {
            show_session_error_async (parent, ERR_FILEIO_RESERVED_WRITE, newfile,
                                      GNC_FILE_DIALOG_SAVE,
                                      file_save_as_error_finished, operation);
            g_free (scheme);
            g_free (hostname);
            g_free (username);
            g_free (password);
            g_free (path);
            g_free (newfile);
            g_clear_object (&parent);
            return;
        }
        default_dir = g_path_get_dirname (path);
        gnc_set_default_directory (GNC_PREFS_GROUP_OPEN_SAVE, default_dir);
        g_free (default_dir);
    }

    session = gnc_get_current_session ();
    if (g_strcmp0 (qof_session_get_url (session), newfile) == 0)
    {
        g_free (scheme);
        g_free (hostname);
        g_free (username);
        g_free (password);
        g_free (path);
        g_free (newfile);
        gnc_file_save_async (parent, operation->completed, operation->user_data);
        g_clear_object (&parent);
        file_save_as_operation_free (operation);
        return;
    }

    qof_event_suspend ();
    gnc_suspend_gui_refresh ();
    lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_SAVE_AS);
    authorized = lease && qof_session_ensure_all_data_loaded_with_lease (
        session, lease);
    qof_session_operation_lease_release (lease);
    gnc_resume_gui_refresh ();
    qof_event_resume ();
    if (!authorized)
    {
        show_session_error_async (parent, ERR_BACKEND_MISC, newfile,
                                  GNC_FILE_DIALOG_SAVE,
                                  file_save_as_error_finished, operation);
        goto file_save_as_start_out;
    }

    new_session = qof_session_new (NULL);
    authorized = file_session_lease_pair_acquire (
        session, QOF_SESSION_OPERATION_SAVE_AS,
        new_session, QOF_SESSION_OPERATION_SAVE_AS, &pair);
    if (authorized)
        authorized = qof_session_begin_with_lease (
            new_session, pair.lease_b, newfile,
            operation->mode == GNC_FILE_SAVE_AS_OVERWRITE ?
            SESSION_NEW_OVERWRITE :
            operation->mode == GNC_FILE_SAVE_AS_BREAK_LOCK ?
            SESSION_BREAK_LOCK : SESSION_NEW_STORE);
    io_err = authorized ? qof_session_get_error (new_session) : ERR_BACKEND_MISC;
    if (io_err != ERR_BACKEND_NO_ERR)
    {
        if (pair.lease_b)
        {
            xaccLogDisable ();
            (void)qof_session_destroy_with_lease (new_session, pair.lease_b);
            xaccLogEnable ();
        }
        else
            (void)file_session_destroy (new_session);
        file_session_lease_pair_release (&pair);
        file_save_as_begin_error (operation, parent, io_err, newfile);
        goto file_save_as_start_out;
    }

    if (!gnc_uri_is_file_scheme (scheme))
        gnc_keyring_set_password (scheme, hostname, port, path, username, password);

    qof_event_suspend ();
    authorized = qof_session_swap_data_with_leases (
        session, pair.lease_a, new_session, pair.lease_b);
    file_session_lease_pair_release (&pair);
    if (!authorized)
    {
        qof_event_resume ();
        (void)file_session_destroy (new_session);
        show_session_error_async (parent, ERR_BACKEND_MISC, newfile,
                                  GNC_FILE_DIALOG_SAVE,
                                  file_save_as_error_finished, operation);
        goto file_save_as_start_out;
    }

    authorized = file_session_lease_pair_acquire (
        session, QOF_SESSION_OPERATION_SAVE_AS,
        new_session, QOF_SESSION_OPERATION_SAVE_AS, &pair);
    if (!authorized)
        g_error ("Unable to reacquire Save As leases after an atomic swap");
    qof_book_mark_session_dirty (qof_session_get_book (new_session));
    qof_event_resume ();

    gnc_set_busy_cursor (NULL, TRUE);
    gnc_window_show_progress (_("Writing file…"), 0.0);
    authorized = qof_session_save_with_lease (
        new_session, pair.lease_b, gnc_window_show_progress);
    gnc_window_show_progress (NULL, -1.0);
    gnc_unset_busy_cursor (NULL);
    io_err = authorized ? qof_session_get_error (new_session) : ERR_BACKEND_MISC;
    if (io_err != ERR_BACKEND_NO_ERR)
    {
        gboolean swapped_back;

        qof_event_suspend ();
        swapped_back = qof_session_swap_data_with_leases (
            new_session, pair.lease_b, session, pair.lease_a);
        qof_event_resume ();
        file_session_lease_pair_release (&pair);
        if (swapped_back)
            (void)file_session_destroy (new_session);
        else
            PWARN ("Save As rollback lost its session authority");
        show_session_error_async (parent, io_err, newfile, GNC_FILE_DIALOG_SAVE,
                                  file_save_as_error_finished, operation);
    }
    else
    {
        gboolean cleared;

        qof_event_suspend ();
        gnc_gui_component_reset_session (session, new_session);
        qof_session_operation_lease_release (pair.lease_b);
        pair.lease_b = NULL;
        cleared = gnc_clear_current_session_with_lease (pair.lease_a);
        qof_session_operation_lease_release (pair.lease_a);
        pair.lease_a = NULL;
        if (!cleared)
        {
            qof_event_resume ();
            show_session_error_async (parent, ERR_BACKEND_MISC, newfile,
                                      GNC_FILE_DIALOG_SAVE,
                                      file_save_as_error_finished, operation);
            goto file_save_as_start_out;
        }
        gnc_set_current_session (new_session);
        qof_event_resume ();
        xaccReopenLog ();
        gnc_add_history (new_session);
        gnc_hook_run (HOOK_BOOK_SAVED, new_session);
        file_save_as_operation_complete (operation, TRUE);
    }

file_save_as_start_out:
    g_free (scheme);
    g_free (hostname);
    g_free (username);
    g_free (password);
    g_free (path);
    g_free (newfile);
    g_clear_object (&parent);
}

static void
gnc_file_do_save_as_async (GtkWindow *parent, const char *filename,
                           GncFileQuerySaveCallback completed,
                           gpointer user_data)
{
    GncFileSaveAsOperation *operation;
    QofSession *session;

    if (!filename || !gnc_current_session_exist ())
    {
        gnc_file_save_complete (parent, completed, user_data, FALSE);
        return;
    }

    session = gnc_get_current_session ();
    operation = g_new0 (GncFileSaveAsOperation, 1);
    g_weak_ref_init (&operation->parent, parent);
    operation->has_parent = parent != NULL;
    operation->filename = g_strdup (filename);
    operation->session = session;
    operation->book = qof_session_get_book (session);
    operation->session_url = g_strdup (qof_session_get_url (session));
    operation->session_generation = gnc_current_session_get_generation ();
    operation->mode = GNC_FILE_SAVE_AS_NEW_STORE;
    operation->completed = completed;
    operation->user_data = user_data;
    file_save_as_start (operation);
}

void
gnc_file_do_save_as (GtkWindow *parent, const char *filename)
{
    gnc_file_manual_save_enqueue (parent, GNC_FILE_MANUAL_SAVE_AS_FILE,
                                  filename);
}
typedef struct
{
    GWeakRef parent;
    gboolean has_parent;
    QofSession *session;
    QofBook *book;
    gchar *fileurl;
    gchar *session_url;
    guint64 session_generation;
    gboolean open_readonly;
    GncSessionTransition *transition;
} GncFileRevertRequest;

static void
gnc_file_revert_request_free (GncFileRevertRequest *request)
{
    GncSessionTransition *transition = request->transition;

    request->transition = NULL;
    g_weak_ref_clear (&request->parent);
    g_free (request->fileurl);
    g_free (request->session_url);
    g_free (request);
    if (transition)
        gnc_session_transition_complete (transition);
}

static void
gnc_file_revert_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    GncFileRevertRequest *request = user_data;
    QofSessionOperationLease *lease = NULL;
    GncSessionTransition *transition;

    if (response == GTK_RESPONSE_YES && gnc_current_session_exist () &&
        gnc_current_session_get_generation () == request->session_generation &&
        gnc_get_current_session () == request->session &&
        qof_session_get_book (request->session) == request->book &&
        g_strcmp0 (qof_session_get_url (request->session),
                   request->session_url) == 0)
    {
        lease = qof_session_operation_lease_acquire_for (
            request->session, QOF_SESSION_OPERATION_OPEN);
        if (lease)
        {
            qof_book_mark_session_saved (request->book);
            qof_session_operation_lease_release (lease);
            transition = request->transition;
            request->transition = NULL;
            if (!gnc_file_open_request (parent, request->fileurl,
                                        request->open_readonly,
                                        /*reset_bayes_conversion*/ TRUE,
                                        transition))
                request->transition = transition;
        }
    }
    gnc_file_revert_request_free (request);
}

static void
gnc_file_revert_start (GncSessionTransition *transition, gpointer user_data)
{
    GncFileRevertRequest *request = user_data;
    GtkWindow *parent = GTK_WINDOW (g_weak_ref_get (&request->parent));
    QofSession *session;
    const gchar *fileurl;
    const gchar *filename;
    const gchar *tmp;
    const gchar *title = _("Reverting will discard all unsaved changes to %s. "
                           "Are you sure you want to proceed?");

    request->transition = transition;
    if ((request->has_parent && !parent) || !gnc_current_session_exist ())
    {
        gnc_file_revert_request_free (request);
        g_clear_object (&parent);
        return;
    }

    session = gnc_get_current_session ();
    fileurl = qof_session_get_url (session);
    if (!strlen (fileurl))
        fileurl = _("<unknown>");
    tmp = strrchr (fileurl, '/');
    filename = tmp ? tmp + 1 : fileurl;

    request->session = session;
    request->book = qof_session_get_book (session);
    request->fileurl = g_strdup (fileurl);
    request->session_url = g_strdup (qof_session_get_url (session));
    request->session_generation = gnc_current_session_get_generation ();
    request->open_readonly = qof_book_is_readonly (request->book);
    gnc_verify_dialog_async (parent, FALSE, gnc_file_revert_finished, request,
                             title, filename);
    g_clear_object (&parent);
}

void
gnc_file_revert (GtkWindow *parent)
{
    GncFileRevertRequest *request = g_new0 (GncFileRevertRequest, 1);
    GncSessionTransitionDisposition disposition;

    g_weak_ref_init (&request->parent, parent);
    request->has_parent = parent != NULL;
    disposition = gnc_session_transition_enqueue (
        GNC_SESSION_TRANSITION_REVERT, gnc_file_revert_start,
        (GncSessionTransitionCancelFunc)gnc_file_revert_request_free, request);
    if (disposition == GNC_SESSION_TRANSITION_REJECTED)
        gnc_file_revert_request_free (request);
}

void
gnc_file_quit (void)
{
    QofSession *session;
    QofSessionOperationLease *close_lease;

    gnc_session_transition_begin_shutdown (NULL);
    if (!gnc_current_session_exist ())
        return;
    gnc_set_busy_cursor (NULL, TRUE);
    session = gnc_get_current_session ();
    close_lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_CLOSE);
    if (!close_lease)
    {
        gnc_unset_busy_cursor (NULL);
        return;
    }

    /* disable events; otherwise the mass deletion of accounts and
     * transactions during shutdown would cause massive redraws */
    qof_event_suspend ();

    gnc_hook_run(HOOK_BOOK_CLOSED, session);
    gnc_close_gui_component_by_session (session);
    gnc_state_save (session);
    if (!gnc_current_session_exist () ||
        gnc_get_current_session () != session ||
        !gnc_clear_current_session_with_lease (close_lease))
    {
        qof_session_operation_lease_release (close_lease);
        qof_event_resume ();
        gnc_unset_busy_cursor (NULL);
        return;
    }
    qof_session_operation_lease_release (close_lease);

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
        return qof_session_save_in_progress (session) ||
               qof_session_has_active_operation_kind (
                   session, QOF_SESSION_OPERATION_SAVE) ||
               qof_session_has_active_operation_kind (
                   session, QOF_SESSION_OPERATION_SAVE_AS);
    }
    return FALSE;
}
