/********************************************************************\
 * dialog-userpass.c -- dialog for username/password entry          *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include <config.h>
#include <gio/gio.h>
#include <gtk/gtk.h>

#include "dialog-utils.h"
#include "gnc-ui.h"

typedef struct
{
    gchar *username;
    gchar *password;
} GncUserPasswordResult;

typedef struct
{
    GTask *task;
    GWeakRef parent;
    GtkWindow *window;
    GtkEntry *username_entry;
    GtkEntry *password_entry;
    gulong parent_destroy_handler;
    gulong cancellable_handler;
    gboolean completed;
} GncUserPasswordRequest;

static void
gnc_user_password_result_free (GncUserPasswordResult *result)
{
    if (!result)
        return;

    g_free (result->username);
    g_free (result->password);
    g_free (result);
}

static void
gnc_user_password_request_free (GncUserPasswordRequest *request)
{
    g_weak_ref_clear (&request->parent);
    g_clear_object (&request->window);
    g_clear_object (&request->task);
    g_free (request);
}

static void
gnc_user_password_request_complete (GncUserPasswordRequest *request,
                                    GncUserPasswordResult *result,
                                    gboolean destroy_window)
{
    GtkWindow *parent;
    GtkWindow *window;

    if (request->completed)
    {
        gnc_user_password_result_free (result);
        return;
    }
    request->completed = TRUE;

    if (request->cancellable_handler)
    {
        g_cancellable_disconnect (g_task_get_cancellable (request->task),
                                  request->cancellable_handler);
        request->cancellable_handler = 0;
    }

    parent = GTK_WINDOW (g_weak_ref_get (&request->parent));
    if (parent && request->parent_destroy_handler)
        g_signal_handler_disconnect (parent, request->parent_destroy_handler);
    request->parent_destroy_handler = 0;
    g_clear_object (&parent);

    window = g_steal_pointer (&request->window);
    if (window && destroy_window)
        gtk_window_destroy (window);
    g_clear_object (&window);

    if (result)
        g_task_return_pointer (request->task, result,
                               (GDestroyNotify)gnc_user_password_result_free);
    else
        g_task_return_new_error (request->task, G_IO_ERROR,
                                 G_IO_ERROR_CANCELLED,
                                 "%s", _("Password entry was cancelled."));
    gnc_user_password_request_free (request);
}

static void
gnc_user_password_request_fail (GncUserPasswordRequest *request,
                                const gchar *message)
{
    if (request->completed)
        return;
    request->completed = TRUE;
    if (request->cancellable_handler)
    {
        g_cancellable_disconnect (g_task_get_cancellable (request->task),
                                  request->cancellable_handler);
        request->cancellable_handler = 0;
    }
    g_task_return_new_error (request->task, G_IO_ERROR, G_IO_ERROR_FAILED,
                             "%s", message);
    gnc_user_password_request_free (request);
}

static void
gnc_user_password_cancel_clicked (GtkButton *button, gpointer user_data)
{
    (void)button;
    gnc_user_password_request_complete (user_data, NULL, TRUE);
}

static void
gnc_user_password_ok_clicked (GtkButton *button, gpointer user_data)
{
    GncUserPasswordRequest *request = user_data;
    GncUserPasswordResult *result;

    (void)button;
    result = g_new0 (GncUserPasswordResult, 1);
    result->username = gtk_editable_get_chars (
        GTK_EDITABLE (request->username_entry), 0, -1);
    result->password = gtk_editable_get_chars (
        GTK_EDITABLE (request->password_entry), 0, -1);
    gnc_user_password_request_complete (request, result, TRUE);
}

static gboolean
gnc_user_password_close_request (GtkWindow *window, gpointer user_data)
{
    (void)window;
    gnc_user_password_request_complete (user_data, NULL, TRUE);
    return TRUE;
}

static void
gnc_user_password_destroyed (GtkWidget *window, gpointer user_data)
{
    GncUserPasswordRequest *request = user_data;

    (void)window;
    g_clear_object (&request->window);
    gnc_user_password_request_complete (request, NULL, FALSE);
}

static void
gnc_user_password_parent_destroyed (GtkWidget *parent, gpointer user_data)
{
    (void)parent;
    gnc_user_password_request_complete (user_data, NULL, TRUE);
}

static void
gnc_user_password_cancelled (GCancellable *cancellable, gpointer user_data)
{
    (void)cancellable;
    gnc_user_password_request_complete (user_data, NULL, TRUE);
}

void
gnc_get_username_password_async (GtkWindow *parent,
                                 const gchar *heading,
                                 const gchar *initial_username,
                                 const gchar *initial_password,
                                 GCancellable *cancellable,
                                 GAsyncReadyCallback callback,
                                 gpointer user_data)
{
    GncUserPasswordRequest *request;
    GtkBuilder *builder;
    GObject *window_object;
    GtkLabel *heading_label;
    GtkButton *cancel_button;
    GtkButton *ok_button;

    g_return_if_fail (!parent || GTK_IS_WINDOW (parent));

    request = g_new0 (GncUserPasswordRequest, 1);
    request->task = g_task_new (NULL, cancellable, callback, user_data);
    g_weak_ref_init (&request->parent, parent);

    if (cancellable)
    {
        request->cancellable_handler = g_cancellable_connect (
            cancellable, G_CALLBACK (gnc_user_password_cancelled), request,
            NULL);
        if (!request->cancellable_handler)
            return;
    }

    builder = gtk_builder_new ();
    if (!gnc_builder_add_from_file (builder, "dialog-userpass.glade",
                                    "username_password_dialog"))
    {
        g_object_unref (builder);
        gnc_user_password_request_fail (
            request, _("The username and password dialog is incomplete."));
        return;
    }

    window_object = gtk_builder_get_object (builder, "username_password_dialog");
    if (window_object)
        request->window = GTK_WINDOW (g_object_ref (window_object));
    heading_label = GTK_LABEL (gtk_builder_get_object (builder, "heading_label"));
    request->username_entry = GTK_ENTRY (gtk_builder_get_object (
        builder, "username_entry"));
    request->password_entry = GTK_ENTRY (gtk_builder_get_object (
        builder, "password_entry"));
    cancel_button = GTK_BUTTON (gtk_builder_get_object (builder,
                                                         "cancel_button"));
    ok_button = GTK_BUTTON (gtk_builder_get_object (builder, "ok_button"));
    g_object_unref (builder);

    if (!request->window || !heading_label || !request->username_entry ||
        !request->password_entry || !cancel_button || !ok_button)
    {
        gnc_user_password_request_fail (
            request, _("The username and password dialog is incomplete."));
        return;
    }

    gtk_widget_set_name (GTK_WIDGET (request->window),
                         "gnc-id-user-password");
    gtk_window_set_modal (request->window, TRUE);
    if (parent)
    {
        gtk_window_set_transient_for (request->window, parent);
        request->parent_destroy_handler = g_signal_connect (
            parent, "destroy", G_CALLBACK (gnc_user_password_parent_destroyed),
            request);
    }

    if (heading)
        gtk_label_set_text (heading_label, heading);
    if (initial_username)
        gtk_editable_set_text (GTK_EDITABLE (request->username_entry),
                               initial_username);
    gtk_editable_select_region (GTK_EDITABLE (request->username_entry), 0, -1);
    if (initial_password)
        gtk_editable_set_text (GTK_EDITABLE (request->password_entry),
                               initial_password);

    g_signal_connect (cancel_button, "clicked",
                      G_CALLBACK (gnc_user_password_cancel_clicked), request);
    g_signal_connect (ok_button, "clicked",
                      G_CALLBACK (gnc_user_password_ok_clicked), request);
    g_signal_connect (request->window, "close-request",
                      G_CALLBACK (gnc_user_password_close_request), request);
    g_signal_connect (request->window, "destroy",
                      G_CALLBACK (gnc_user_password_destroyed), request);
    gtk_window_set_default_widget (request->window, GTK_WIDGET (ok_button));
    gtk_widget_grab_focus (GTK_WIDGET (request->username_entry));
    gtk_window_present (request->window);
}

gboolean
gnc_get_username_password_finish (GAsyncResult *result, gchar **username,
                                  gchar **password, GError **error)
{
    GncUserPasswordResult *credentials;

    g_return_val_if_fail (username != NULL, FALSE);
    g_return_val_if_fail (password != NULL, FALSE);
    g_return_val_if_fail (g_task_is_valid (result, NULL), FALSE);

    *username = NULL;
    *password = NULL;
    credentials = g_task_propagate_pointer (G_TASK (result), error);
    if (!credentials)
        return FALSE;

    *username = g_steal_pointer (&credentials->username);
    *password = g_steal_pointer (&credentials->password);
    gnc_user_password_result_free (credentials);
    return TRUE;
}
