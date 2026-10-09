/********************************************************************\
 * gnc-gui-query.c -- functions for creating dialogs for GnuCash    *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include <config.h>

#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "qof.h"
#include "gnc-gui-query.h"
#include "gnc-ui.h"

#define INDEX_LABEL "index"

typedef struct
{
    GWeakRef parent;
    gboolean has_parent;
    gboolean parent_destroyed;
    GncGuiQueryResponseCallback callback;
    gpointer user_data;
    gint accept_response;
    gint cancel_response;
    gboolean preserve_responses;
} GncGuiQueryRequest;

static void
gnc_gui_query_parent_destroyed ([[maybe_unused]] GtkWidget *parent,
                                GncGuiQueryRequest *request)
{
    request->parent_destroyed = TRUE;
}

static void
gnc_gui_query_complete (GtkWidget *dialog, GncGuiQueryRequest *request,
                        gint response, gboolean destroying)
{
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    GncGuiQueryResponseCallback callback = request->callback;
    gpointer user_data = request->user_data;

    /* Disconnect before destroying: response and destroy are two ways of
     * completing the same request, not two independent notifications. */
    g_signal_handlers_disconnect_by_data (dialog, request);
    if (!destroying)
        gtk_widget_destroy (dialog);
    /* Destroy notifications can themselves close the parent. A retained
     * GObject reference does not keep its GTK window usable. */
    if (parent)
    {
        g_signal_handlers_disconnect_by_data (parent, request);
        if (request->parent_destroyed ||
            gtk_widget_in_destruction (GTK_WIDGET (parent)))
            g_clear_object (&parent);
    }
    if (request->preserve_responses)
    {
        if (destroying || response == GTK_RESPONSE_NONE ||
            response == GTK_RESPONSE_DELETE_EVENT ||
            (request->has_parent && !parent))
            response = GTK_RESPONSE_CANCEL;
    }
    else if (response != request->accept_response || destroying ||
             (request->has_parent && !parent))
        response = request->cancel_response;
    g_weak_ref_clear (&request->parent);
    g_free (request);
    callback (parent, response, user_data);
    g_clear_object (&parent);
}

static void
gnc_gui_query_response (GtkDialog *dialog, gint response, gpointer user_data)
{
    gnc_gui_query_complete (GTK_WIDGET (dialog), user_data, response, FALSE);
}

static void
gnc_gui_query_destroyed (GtkWidget *dialog, gpointer user_data)
{
    gnc_gui_query_complete (dialog, user_data, GTK_RESPONSE_NONE, TRUE);
}

static void
gnc_gui_query_bind_response (GtkWidget *dialog, GtkWindow *parent,
                             gint accept_response, gint cancel_response,
                             GncGuiQueryResponseCallback completed,
                             gpointer user_data,
                             gboolean preserve_responses)
{
    GncGuiQueryRequest *request = g_new0 (GncGuiQueryRequest, 1);
    g_weak_ref_init (&request->parent, parent ? G_OBJECT (parent) : NULL);
    request->has_parent = parent != NULL;
    request->callback = completed;
    request->user_data = user_data;
    request->accept_response = accept_response;
    request->cancel_response = cancel_response;
    request->preserve_responses = preserve_responses;
    if (parent)
        g_signal_connect (parent, "destroy",
                          G_CALLBACK (gnc_gui_query_parent_destroyed), request);
    g_signal_connect (dialog, "response", G_CALLBACK (gnc_gui_query_response), request);
    g_signal_connect (dialog, "destroy", G_CALLBACK (gnc_gui_query_destroyed), request);
}

void
gnc_gui_query_bind_dialog_response (GtkDialog *dialog,
                                   GncGuiQueryResponseCallback completed,
                                   gpointer user_data)
{
    g_return_if_fail (GTK_IS_DIALOG (dialog));
    g_return_if_fail (completed != NULL);
    auto parent = gtk_window_get_transient_for (GTK_WINDOW (dialog));
    gnc_gui_query_bind_response (GTK_WIDGET (dialog), parent, 0,
                                 GTK_RESPONSE_CANCEL, completed, user_data,
                                 TRUE);
}

static void
gnc_gui_query_async_va (GtkWindow *parent, const gchar *accept_label,
                        const gchar *cancel_label, gint accept_response,
                        gint cancel_response, gboolean accept_default,
                        GncGuiQueryResponseCallback completed,
                        gpointer user_data, const gchar *format, va_list args)
{
    g_return_if_fail (completed != NULL);
    if (!parent)
        parent = gnc_ui_get_main_window (NULL);

    gchar *message = g_strdup_vprintf (format, args);
    GtkWidget *dialog = gtk_message_dialog_new (
        parent, GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
        GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE, "%s", message);
    g_free (message);
    gtk_dialog_add_button (GTK_DIALOG (dialog), cancel_label, cancel_response);
    gtk_dialog_add_button (GTK_DIALOG (dialog), accept_label, accept_response);
    gtk_dialog_set_default_response (GTK_DIALOG (dialog),
                                    accept_default ? accept_response : cancel_response);
    if (!parent)
        gtk_window_set_skip_taskbar_hint (GTK_WINDOW (dialog), FALSE);

    gnc_gui_query_bind_response (dialog, parent, accept_response,
                                 cancel_response, completed, user_data, FALSE);
    gtk_widget_show_all (dialog);
}

void
gnc_ok_cancel_dialog_async (GtkWindow *parent, gint default_result,
                            GncGuiQueryResponseCallback completed,
                            gpointer user_data, const gchar *format, ...)
{
    va_list args;
    va_start (args, format);
    gnc_gui_query_async_va (parent, _("_OK"), _("_Cancel"), GTK_RESPONSE_OK,
                            GTK_RESPONSE_CANCEL, default_result == GTK_RESPONSE_OK,
                            completed, user_data, format, args);
    va_end (args);
}

void
gnc_verify_dialog_async (GtkWindow *parent, gboolean yes_is_default,
                         GncGuiQueryResponseCallback completed,
                         gpointer user_data, const gchar *format, ...)
{
    va_list args;
    va_start (args, format);
    gnc_gui_query_async_va (parent, _("_Yes"), _("_No"), GTK_RESPONSE_YES,
                            GTK_RESPONSE_NO, yes_is_default,
                            completed, user_data, format, args);
    va_end (args);
}

void
gnc_action_dialog_async (GtkWindow *parent, const gchar *action,
                         gboolean action_default,
                         GncGuiQueryResponseCallback completed,
                         gpointer user_data, const gchar *format, ...)
{
    va_list args;
    g_return_if_fail (action != NULL);
    va_start (args, format);
    gnc_gui_query_async_va (parent, action, _("_Cancel"), GTK_RESPONSE_ACCEPT,
                            GTK_RESPONSE_CANCEL, action_default,
                            completed, user_data, format, args);
    va_end (args);
}

/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

static GtkWidget *
gnc_message_dialog_create (GtkWindow *parent, const gchar *format, GtkMessageType msg_type, va_list args)
{
    GtkWidget *dialog = NULL;
    gchar *buffer;

    if (!parent)
        parent = gnc_ui_get_main_window (NULL);

    buffer = g_strdup_vprintf(format, args);
    dialog = gtk_message_dialog_new (parent,
                                     GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                     msg_type,
                                     GTK_BUTTONS_CLOSE,
                                     "%s",
                                     buffer);
    g_free(buffer);

    if (!parent)
        gtk_window_set_skip_taskbar_hint(GTK_WINDOW(dialog), FALSE);

    return dialog;
}

static void
gnc_message_dialog_common (GtkWindow *parent, const gchar *format,
                          GtkMessageType msg_type, va_list args)
{
    GtkWidget *dialog = gnc_message_dialog_create (parent, format, msg_type, args);
    g_signal_connect_swapped (dialog, "response",
                              G_CALLBACK (gtk_widget_destroy), dialog);
    gtk_widget_show (dialog);
}

/********************************************************************\
 * gnc_info_dialog                                                  *
 *   displays an information dialog box                             *
 *                                                                  *
 * Args:   parent  - the parent window                              *
 *         format - the format string for the message to display    *
 *                   This is a standard 'printf' style string.      *
 *         args - a pointer to the first argument for the format    *
 *                string.                                           *
 * Return: none                                                     *
\********************************************************************/
void
gnc_info_dialog (GtkWindow *parent, const gchar *format, ...)
{
    va_list args;

    va_start(args, format);
    gnc_message_dialog_common (parent, format, GTK_MESSAGE_INFO, args);
    va_end(args);
}



/********************************************************************\
 * gnc_warning_dialog                                               *
 *   displays a warning dialog box                                  *
 *                                                                  *
 * Args:   parent  - the parent window                              *
 *         format - the format string for the message to display    *
 *                   This is a standard 'printf' style string.      *
 *         args - a pointer to the first argument for the format    *
 *                string.                                           *
 * Return: none                                                     *
\********************************************************************/

void
gnc_warning_dialog (GtkWindow *parent, const gchar *format, ...)
{
    va_list args;

    va_start(args, format);
    gnc_message_dialog_common (parent, format, GTK_MESSAGE_WARNING, args);
    va_end(args);
}


/********************************************************************\
 * gnc_error_dialog                                                 *
 *   displays an error dialog box                                   *
 *                                                                  *
 * Args:   parent  - the parent window                              *
 *         format - the format string for the message to display    *
 *                   This is a standard 'printf' style string.      *
 *         args - a pointer to the first argument for the format    *
 *                string.                                           *
 * Return: none                                                     *
\********************************************************************/
void gnc_error_dialog (GtkWindow* parent, const char* format, ...)
{
    va_list args;

    va_start(args, format);
    gnc_message_dialog_common (parent, format, GTK_MESSAGE_ERROR, args);
    va_end(args);
}

static void
gnc_message_dialog_async_va (GtkWindow *parent, GtkMessageType type,
                             const gchar *format, va_list args)
{
    GtkWidget *dialog = gnc_message_dialog_create (parent, format, type, args);
    g_signal_connect_swapped (dialog, "response",
                              G_CALLBACK (gtk_widget_destroy), dialog);
    gtk_widget_show (dialog);
}

void
gnc_error_dialog_async (GtkWindow *parent, const gchar *format, ...)
{
    va_list args;
    va_start (args, format);
    gnc_message_dialog_async_va (parent, GTK_MESSAGE_ERROR, format, args);
    va_end (args);
}

void
gnc_warning_dialog_async (GtkWindow *parent, const gchar *format, ...)
{
    va_list args;
    va_start (args, format);
    gnc_message_dialog_async_va (parent, GTK_MESSAGE_WARNING, format, args);
    va_end (args);
}

void
gnc_error_dialog_async_list (GtkWindow *parent, const GList *errors)
{
    GString *message;
    if (!errors)
        return;

    message = g_string_new (NULL);
    for (const GList *node = errors; node; node = node->next)
    {
        if (node != errors)
            g_string_append (message, "\n\n");
        g_string_append (message, node->data);
    }
    gnc_error_dialog_async (parent, "%s", message->str);
    g_string_free (message, TRUE);
}

void
gnc_info_dialog_async (GtkWindow *parent, const gchar *format, ...)
{
    va_list args;
    va_start (args, format);
    gnc_message_dialog_async_va (parent, GTK_MESSAGE_INFO, format, args);
    va_end (args);
}

static void
gnc_message_dialog_async_response_va (GtkWindow *parent, GtkMessageType type,
                                     GncGuiQueryResponseCallback completed,
                                     gpointer user_data, const gchar *format,
                                     va_list args)
{
    GtkWidget *dialog;
    GtkWindow *dialog_parent;
    if (!completed)
    {
        gnc_message_dialog_async_va (parent, type, format, args);
        return;
    }
    dialog = gnc_message_dialog_create (parent, format, type, args);
    dialog_parent = gtk_window_get_transient_for (GTK_WINDOW (dialog));
    gnc_gui_query_bind_response (dialog, dialog_parent, GTK_RESPONSE_CLOSE,
                                 GTK_RESPONSE_CANCEL, completed, user_data, FALSE);
    gtk_widget_show (dialog);
}

void
gnc_message_dialog_async_response (GtkWindow *parent, GtkMessageType type,
                                  GncGuiQueryResponseCallback completed,
                                  gpointer user_data, const gchar *format, ...)
{
    va_list args;
    va_start (args, format);
    gnc_message_dialog_async_response_va (parent, type, completed, user_data,
                                         format, args);
    va_end (args);
}

void
gnc_info_dialog_async_response (GtkWindow *parent,
                               GncGuiQueryResponseCallback completed,
                               gpointer user_data, const gchar *format, ...)
{
    va_list args;
    g_return_if_fail (completed != NULL);
    va_start (args, format);
    gnc_message_dialog_async_response_va (parent, GTK_MESSAGE_INFO, completed,
                                         user_data, format, args);
    va_end (args);
}

static void
gnc_choose_radio_button_cb(GtkWidget *w, gpointer data)
{
    int *result = data;

    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w)))
        *result = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(w), INDEX_LABEL));
}

/********************************************************************
 gnc_choose_radio_option_dialog

 display a group of radio_buttons and return the index of
 the selected one
*/

typedef struct
{
    gint selected;
    GPtrArray *buttons;
    GncGuiQueryResponseCallback completed;
    gpointer user_data;
} RadioQuery;

static void
radio_query_completed (GtkWindow *parent, gint response, gpointer user_data)
{
    RadioQuery *request = user_data;
    gint selected = response == GTK_RESPONSE_OK ? request->selected : -1;
    GncGuiQueryResponseCallback completed = request->completed;
    gpointer data = request->user_data;
    for (guint i = 0; i < request->buttons->len; ++i)
        g_signal_handlers_disconnect_by_data (g_ptr_array_index (request->buttons, i),
                                               &request->selected);
    g_ptr_array_unref (request->buttons);
    g_free (request);
    completed (parent, selected, data);
}

void
gnc_choose_radio_option_dialog_async(GtkWidget *parent,
                               const char *title,
                               const char *msg,
                               const char *button_name,
                               int default_value,
                               GList *radio_list,
                               GncGuiQueryResponseCallback completed,
                               gpointer user_data)
{
    g_return_if_fail (completed != NULL);
    RadioQuery *request = g_new0 (RadioQuery, 1);
    request->buttons = g_ptr_array_new_with_free_func (g_object_unref);
    request->completed = completed;
    request->user_data = user_data;
    GtkWidget *vbox;
    GtkWidget *main_vbox;
    GtkWidget *label;
    GtkWidget *radio_button;
    GtkWidget *dialog;
    GtkWidget *dvbox;
    GSList *group = NULL;
    GList *node;
    int i;

    main_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
    gtk_box_set_homogeneous (GTK_BOX (main_vbox), FALSE);
    gtk_container_set_border_width(GTK_CONTAINER(main_vbox), 6);
    gtk_widget_show(main_vbox);

    label = gtk_label_new(msg);
    gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
    gtk_box_pack_start(GTK_BOX(main_vbox), label, FALSE, FALSE, 0);
    gtk_widget_show(label);

    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
    gtk_box_set_homogeneous (GTK_BOX (vbox), TRUE);
    gtk_container_set_border_width(GTK_CONTAINER(vbox), 6);
    gtk_container_add(GTK_CONTAINER(main_vbox), vbox);
    gtk_widget_show(vbox);

    for (node = radio_list, i = 0; node; node = node->next, i++)
    {
        radio_button = gtk_radio_button_new_with_mnemonic(group, node->data);
        group = gtk_radio_button_get_group(GTK_RADIO_BUTTON(radio_button));
        gtk_widget_set_halign (GTK_WIDGET(radio_button), GTK_ALIGN_START);

        if (i == default_value) /* default is first radio button */
        {
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio_button), TRUE);
            request->selected = default_value;
        }

        gtk_widget_show(radio_button);
        gtk_box_pack_start(GTK_BOX(vbox), radio_button, FALSE, FALSE, 0);
        g_ptr_array_add (request->buttons, g_object_ref (radio_button));
        g_object_set_data(G_OBJECT(radio_button), INDEX_LABEL, GINT_TO_POINTER(i));
        g_signal_connect(radio_button, "clicked",
                         G_CALLBACK(gnc_choose_radio_button_cb),
                         &request->selected);
    }

    if (!button_name)
        button_name = _("_OK");
    dialog = gtk_dialog_new_with_buttons (title, GTK_WINDOW(parent),
                                          GTK_DIALOG_DESTROY_WITH_PARENT,
                                          _("_Cancel"), GTK_RESPONSE_CANCEL,
                                          button_name, GTK_RESPONSE_OK,
                                          NULL);

    /* default to ok */
    gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK);

    dvbox = gtk_dialog_get_content_area (GTK_DIALOG(dialog));

    gtk_box_pack_start(GTK_BOX(dvbox), main_vbox, TRUE, TRUE, 0);

    gnc_dialog_run_async (GTK_DIALOG (dialog), NULL,
                          radio_query_completed, request);
}

typedef struct
{
    GtkWidget *dialog;
    GtkWidget *view;
    gboolean use_entry;
    gchar *text;
    GncInputDialogCallback completed;
    gpointer user_data;
} InputQuery;

static void
input_query_capture ([[maybe_unused]] GtkDialog *dialog, gint response,
                     InputQuery *request)
{
    if (response != GTK_RESPONSE_ACCEPT)
        return;
    if (request->use_entry)
        request->text = g_strdup (gtk_entry_get_text (GTK_ENTRY (request->view)));
    else
    {
        GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (request->view));
        GtkTextIter start, end;
        gtk_text_buffer_get_bounds (buffer, &start, &end);
        request->text = gtk_text_buffer_get_text (buffer, &start, &end, FALSE);
    }
}

static void
input_query_completed (GtkWindow *parent, gint response, gpointer user_data)
{
    InputQuery *request = user_data;
    if (response != GTK_RESPONSE_ACCEPT)
        g_clear_pointer (&request->text, g_free);
    GncInputDialogCallback completed = request->completed;
    gpointer data = request->user_data;
    gchar *text = request->text;
    g_signal_handlers_disconnect_by_data (request->dialog, request);
    g_object_unref (request->dialog);
    g_free (request);
    completed (parent, text, data);
}

static void
gnc_input_dialog_internal (GtkWidget *parent, const gchar *title,
                           const gchar *msg, const gchar *default_input,
                           gboolean use_entry, GncInputDialogCallback completed,
                           gpointer user_data)
{
    g_return_if_fail (completed != NULL);
    InputQuery *request = g_new0 (InputQuery, 1);
    request->use_entry = use_entry;
    request->completed = completed;
    request->user_data = user_data;
    GtkWidget *dialog = gtk_dialog_new_with_buttons (
        title, parent ? GTK_WINDOW (parent) : NULL,
        GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
        _("_OK"), GTK_RESPONSE_ACCEPT, _("_Cancel"), GTK_RESPONSE_REJECT, NULL);
    GtkWidget *content = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
    request->dialog = g_object_ref (dialog);
    gtk_box_pack_start (GTK_BOX (content), gtk_label_new (msg), FALSE, FALSE, 0);
    if (use_entry)
    {
        request->view = gtk_entry_new ();
        gtk_entry_set_text (GTK_ENTRY (request->view), default_input ? default_input : "");
    }
    else
    {
        request->view = gtk_text_view_new ();
        gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (request->view), GTK_WRAP_WORD_CHAR);
        gtk_text_buffer_set_text (gtk_text_view_get_buffer (GTK_TEXT_VIEW (request->view)),
                                  default_input ? default_input : "", -1);
    }
    gtk_box_pack_start (GTK_BOX (content), request->view, TRUE, TRUE, 0);
    g_signal_connect (dialog, "response", G_CALLBACK (input_query_capture), request);
    gnc_dialog_run_async (GTK_DIALOG (dialog), NULL, input_query_completed, request);
}

void
gnc_input_dialog_async (GtkWidget *parent, const gchar *title, const gchar *msg,
                        const gchar *default_input, GncInputDialogCallback completed,
                        gpointer user_data)
{
    gnc_input_dialog_internal (parent, title, msg, default_input, FALSE, completed, user_data);
}

void
gnc_input_dialog_with_entry_async (GtkWidget *parent, const gchar *title,
                                   const gchar *msg, const gchar *default_input,
                                   GncInputDialogCallback completed, gpointer user_data)
{
    gnc_input_dialog_internal (parent, title, msg, default_input, TRUE, completed, user_data);
}

void
gnc_info2_dialog (GtkWidget *parent, const gchar *title, const gchar *msg)
{
    GtkWidget *view;
    GtkTextBuffer *buffer;
    gint width, height;
    
    /* Create the widgets */
    GtkWidget* dialog = gtk_dialog_new_with_buttons (title, GTK_WINDOW (parent),
                                          GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                          _("_OK"), GTK_RESPONSE_ACCEPT,
                                          NULL);
    GtkWidget* content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
    
    // add a scroll area
    GtkWidget* scrolledwindow = gtk_scrolled_window_new (NULL, NULL);
    gtk_box_pack_start(GTK_BOX(content_area), scrolledwindow, TRUE, TRUE, 0);
    
    // add a textview
    view = gtk_text_view_new ();
    gtk_text_view_set_editable (GTK_TEXT_VIEW (view), FALSE);
    buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));
    gtk_text_buffer_set_text (buffer, msg, -1);
    gtk_container_add (GTK_CONTAINER (scrolledwindow), view);
    
    // run the dialog
    if (parent)
    {
        gtk_window_get_size (GTK_WINDOW(parent), &width, &height);
        gtk_window_set_default_size (GTK_WINDOW(dialog), width, height);
    }
    gtk_widget_show_all (dialog);
    g_signal_connect_swapped (dialog, "response",
                              G_CALLBACK (gtk_widget_destroy), dialog);
    gtk_widget_show (dialog);
}

void
gnc_info2_dialog_async (GtkWidget *parent, const gchar *title,
                       const gchar *msg,
                       GncGuiQueryResponseCallback completed,
                       gpointer user_data)
{
    GtkWidget *view;
    GtkTextBuffer *buffer;
    gint width, height;
    GtkWindow *parent_window = parent && GTK_IS_WINDOW (parent) ?
        GTK_WINDOW (parent) : NULL;
    g_return_if_fail (completed != NULL);

    GtkWidget *dialog = gtk_dialog_new_with_buttons (
        title, parent_window,
        GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
        _("_OK"), GTK_RESPONSE_ACCEPT, NULL);
    GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
    GtkWidget *scrolledwindow = gtk_scrolled_window_new (NULL, NULL);

    gtk_box_pack_start (GTK_BOX (content_area), scrolledwindow, TRUE, TRUE, 0);
    view = gtk_text_view_new ();
    gtk_text_view_set_editable (GTK_TEXT_VIEW (view), FALSE);
    buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));
    gtk_text_buffer_set_text (buffer, msg ? msg : "", -1);
    gtk_container_add (GTK_CONTAINER (scrolledwindow), view);

    if (parent_window)
    {
        gtk_window_get_size (parent_window, &width, &height);
        gtk_window_set_default_size (GTK_WINDOW (dialog), width, height);
    }

    gnc_gui_query_bind_response (dialog, parent_window, GTK_RESPONSE_ACCEPT,
                                 GTK_RESPONSE_CANCEL, completed, user_data, FALSE);
    gtk_widget_show_all (dialog);
}
