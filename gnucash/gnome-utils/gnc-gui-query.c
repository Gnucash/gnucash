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

/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

/********************************************************************\
 * gnc_ok_cancel_dialog                                             *
 *   display a message, and asks the user to press "Ok" or "Cancel" *
 *                                                                  *
 * NOTE: This function does not return until the dialog is closed   *
 *                                                                  *
 * Args:   parent  - the parent window                              *
 *         default - the button that will be the default            *
 *         message - the message to display                         *
 *         format - the format string for the message to display    *
 *                   This is a standard 'printf' style string.      *
 *         args - a pointer to the first argument for the format    *
 *                string.                                           *
 * Return: the result the user selected                             *
\********************************************************************/
gint
gnc_ok_cancel_dialog(GtkWindow *parent,
                     gint default_result,
                     const gchar *format, ...)
{
    GtkWidget *dialog = NULL;
    gint result;
    gchar *buffer;
    va_list args;

    if (!parent)
        parent = gnc_ui_get_main_window (NULL);

    va_start(args, format);
    buffer = g_strdup_vprintf(format, args);
    dialog = gtk_message_dialog_new (parent,
                                     GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_QUESTION,
                                     GTK_BUTTONS_OK_CANCEL,
                                     "%s",
                                     buffer);
    g_free(buffer);
    va_end(args);

    if (!parent)
        gtk_window_set_skip_taskbar_hint(GTK_WINDOW(dialog), FALSE);

    gtk_dialog_set_default_response (GTK_DIALOG(dialog), default_result);
    result = gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy (dialog);
    return(result);
}



/********************************************************************\
 * gnc_verify_dialog                                                *
 *   display a message, and asks the user to press "Yes" or "No"    *
 *                                                                  *
 * NOTE: This function does not return until the dialog is closed   *
 *                                                                  *
 * Args:   parent  - the parent window                              *
 *         yes_is_default - If true, "Yes" is default,              *
 *                          "No" is the default button.             *
 *         format - the format string for the message to display    *
 *                   This is a standard 'printf' style string.      *
 *         args - a pointer to the first argument for the format    *
 *                string.                                           *
\********************************************************************/
gboolean
gnc_verify_dialog(GtkWindow *parent, gboolean yes_is_default,
                  const gchar *format, ...)
{
    GtkWidget *dialog;
    gchar *buffer;
    gint result;
    va_list args;

    if (!parent)
        parent = gnc_ui_get_main_window (NULL);

    va_start(args, format);
    buffer = g_strdup_vprintf(format, args);
    dialog = gtk_message_dialog_new (parent,
                                     GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_QUESTION,
                                     GTK_BUTTONS_YES_NO,
                                     "%s",
                                     buffer);
    g_free(buffer);
    va_end(args);

    if (!parent)
        gtk_window_set_skip_taskbar_hint(GTK_WINDOW(dialog), FALSE);

    gtk_dialog_set_default_response(GTK_DIALOG(dialog),
                                    (yes_is_default ? GTK_RESPONSE_YES : GTK_RESPONSE_NO));
    result = gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy (dialog);
    return (result == GTK_RESPONSE_YES);
}

static void
gnc_message_dialog_common (GtkWindow *parent, const gchar *format, GtkMessageType msg_type, va_list args)
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

    gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);
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

int
gnc_choose_radio_option_dialog(GtkWidget *parent,
                               const char *title,
                               const char *msg,
                               const char *button_name,
                               int default_value,
                               GList *radio_list)
{
    int radio_result = 0; /* initial selected value is first one */
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
            radio_result = default_value;
        }

        gtk_widget_show(radio_button);
        gtk_box_pack_start(GTK_BOX(vbox), radio_button, FALSE, FALSE, 0);
        g_object_set_data(G_OBJECT(radio_button), INDEX_LABEL, GINT_TO_POINTER(i));
        g_signal_connect(radio_button, "clicked",
                         G_CALLBACK(gnc_choose_radio_button_cb),
                         &radio_result);
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

    if (gtk_dialog_run(GTK_DIALOG(dialog)) != GTK_RESPONSE_OK)
        radio_result = -1;

    gtk_widget_destroy (dialog);

    return radio_result;
}
