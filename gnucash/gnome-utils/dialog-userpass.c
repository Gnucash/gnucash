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
#include <gtk/gtk.h>

#include "dialog-utils.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"


typedef struct
{
    GtkWidget *dialog;
    GtkEntry *username_entry;
    GtkEntry *password_entry;
    gchar *username;
    gchar *password;
    GncUsernamePasswordCallback callback;
    gpointer user_data;
} UsernamePasswordRequest;

static void
username_password_capture (GtkDialog *dialog, gint response, gpointer user_data)
{
    UsernamePasswordRequest *request = user_data;
    if (response != GTK_RESPONSE_OK) return;
    request->username = gtk_editable_get_chars (GTK_EDITABLE (request->username_entry), 0, -1);
    request->password = gtk_editable_get_chars (GTK_EDITABLE (request->password_entry), 0, -1);
}

static void
username_password_finish (GtkWindow *parent, gint response, gpointer user_data)
{
    UsernamePasswordRequest *request = user_data;
    gboolean accepted = response == GTK_RESPONSE_OK;
    GncUsernamePasswordCallback callback = request->callback;
    gpointer data = request->user_data;
    gchar *username = request->username;
    gchar *password = request->password;
    g_signal_handlers_disconnect_by_data (request->dialog, request);
    g_object_unref (request->dialog);
    g_free (request);
    if (!accepted)
    {
        g_clear_pointer (&username, g_free);
        g_clear_pointer (&password, g_free);
    }
    callback (accepted, username, password, data);
}

void
gnc_get_username_password_async (GtkWindow *parent, const gchar *heading,
                                 const gchar *initial_username,
                                 const gchar *initial_password,
                                 GncUsernamePasswordCallback callback,
                                 gpointer user_data)
{
    GtkBuilder *builder;
    UsernamePasswordRequest *request;
    GtkWidget *dialog;
    GtkWidget *heading_label;
    g_return_if_fail (callback != NULL);

    builder = gtk_builder_new ();
    if (!gnc_builder_add_from_file (builder, "dialog-userpass.glade", "username_password_dialog"))
    {
        g_object_unref (builder);
        callback (FALSE, NULL, NULL, user_data);
        return;
    }
    dialog = GTK_WIDGET (gtk_builder_get_object (builder, "username_password_dialog"));
    request = g_new0 (UsernamePasswordRequest, 1);
    request->dialog = g_object_ref (dialog);
    request->username_entry = GTK_ENTRY (gtk_builder_get_object (builder, "username_entry"));
    request->password_entry = GTK_ENTRY (gtk_builder_get_object (builder, "password_entry"));
    request->callback = callback;
    request->user_data = user_data;
    gtk_widget_set_name (dialog, "gnc-id-user-password");
    if (parent)
    {
        gtk_window_set_transient_for (GTK_WINDOW (dialog), parent);
        gtk_window_set_destroy_with_parent (GTK_WINDOW (dialog), TRUE);
    }
    heading_label = GTK_WIDGET (gtk_builder_get_object (builder, "heading_label"));
    if (heading)
        gtk_label_set_text (GTK_LABEL (heading_label), heading);
    if (initial_username)
        gtk_entry_set_text (request->username_entry, initial_username);
    gtk_editable_select_region (GTK_EDITABLE (request->username_entry), 0, -1);
    if (initial_password)
        gtk_entry_set_text (request->password_entry, initial_password);
    /* Capture live entry contents before the common helper destroys the dialog.
     * That helper also completes cancellation on owner/dialog destruction. */
    g_signal_connect (dialog, "response", G_CALLBACK (username_password_capture), request);
    gnc_gui_query_bind_dialog_response (GTK_DIALOG (dialog), username_password_finish, request);
    gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
    g_object_unref (builder);
    gtk_widget_show (dialog);
}
