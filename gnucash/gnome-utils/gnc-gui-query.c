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
gnc_ok_cancel_dialog (GtkWindow *parent,
                      gint default_result,
                      const gchar *format, ...)
{
    GtkWidget *dialog = NULL;
    gint result;
    gchar *buffer;
    va_list args;

    if (!parent)
        parent = gnc_ui_get_main_window (NULL);

    va_start (args, format);
    buffer = g_strdup_vprintf (format, args);
    dialog = gtk_message_dialog_new (parent,
                                     GTK_DIALOG_MODAL |
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_QUESTION,
                                     GTK_BUTTONS_OK_CANCEL,
                                     "%s",
                                     buffer);
    g_free (buffer);
    va_end (args);

//FIXME gtk4    if (!parent)
//        gtk_window_set_skip_taskbar_hint(GTK_WINDOW(dialog), FALSE);

    gtk_dialog_set_default_response (GTK_DIALOG(dialog), default_result);
    result = gnc_dialog_run (GTK_DIALOG(dialog));

    return (result);
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

    va_start (args, format);
    buffer = g_strdup_vprintf (format, args);
    dialog = gtk_message_dialog_new (parent,
                                     GTK_DIALOG_MODAL |
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_QUESTION,
                                     GTK_BUTTONS_YES_NO,
                                     "%s",
                                     buffer);
    g_free (buffer);
    va_end (args);

//FIXME gtk4    if (!parent)
//        gtk_window_set_skip_taskbar_hint(GTK_WINDOW(dialog), FALSE);

    gtk_dialog_set_default_response (GTK_DIALOG(dialog),
                                    (yes_is_default ? GTK_RESPONSE_YES : GTK_RESPONSE_NO));
    result = gnc_dialog_run (GTK_DIALOG(dialog));

    return (result == GTK_RESPONSE_YES);
}

static void
gnc_message_dialog_common (GtkWindow *parent, const gchar *format,
                           GtkMessageType msg_type, va_list args)
{
    GtkWidget *dialog = NULL;
    gchar *buffer;

    if (!parent)
        parent = gnc_ui_get_main_window (NULL);

    buffer = g_strdup_vprintf (format, args);
    dialog = gtk_message_dialog_new (parent,
                                     GTK_DIALOG_MODAL |
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     msg_type,
                                     GTK_BUTTONS_CLOSE,
                                     "%s",
                                     buffer);
    g_free (buffer);

//FIXME gtk4    if (!parent)
//        gtk_window_set_skip_taskbar_hint(GTK_WINDOW(dialog), FALSE);

    gnc_dialog_run (GTK_DIALOG(dialog));
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

    va_start (args, format);
    gnc_message_dialog_common (parent, format, GTK_MESSAGE_INFO, args);
    va_end (args);
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

    va_start (args, format);
    gnc_message_dialog_common (parent, format, GTK_MESSAGE_WARNING, args);
    va_end (args);
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

    va_start (args, format);
    gnc_message_dialog_common (parent, format, GTK_MESSAGE_ERROR, args);
    va_end (args);
}

static void
gnc_choose_check_button_cb (GtkWidget *w, gpointer data)
{
    int *result = data;

    if (gtk_check_button_get_active (GTK_CHECK_BUTTON(w)))
        *result = GPOINTER_TO_INT(g_object_get_data (G_OBJECT(w), INDEX_LABEL));
}

/********************************************************************
 gnc_choose_radio_option_dialog

 display a group of radio_buttons and return the index of
 the selected one
*/

int
gnc_choose_radio_option_dialog (GtkWidget *parent,
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
    GtkWidget *check_button;
    GtkWidget *dialog;
    GtkWidget *dvbox;
    GtkWidget *first_check_button;
    GList *node;
    int i;

    main_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
    gtk_box_set_homogeneous (GTK_BOX (main_vbox), FALSE);
    gnc_box_set_all_margins (GTK_BOX(main_vbox), 6);
    gtk_widget_set_visible (GTK_WIDGET(main_vbox), TRUE);

    label = gtk_label_new(msg);
    gtk_label_set_justify (GTK_LABEL(label), GTK_JUSTIFY_LEFT);
    gtk_box_append (GTK_BOX(main_vbox), GTK_WIDGET(label));
    gtk_widget_set_visible (GTK_WIDGET(label), TRUE);

    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
    gtk_box_set_homogeneous (GTK_BOX(vbox), TRUE);
    gnc_box_set_all_margins (GTK_BOX(vbox), 6);
    gtk_box_prepend (GTK_BOX(main_vbox), GTK_WIDGET(vbox));
    gtk_widget_set_visible (GTK_WIDGET(vbox), TRUE);

    for (node = radio_list, i = 0; node; node = node->next, i++)
    {
        check_button = gtk_check_button_new_with_mnemonic (node->data);

        if (i == 0)
            first_check_button = check_button;
        else
            gtk_check_button_set_group (GTK_CHECK_BUTTON(check_button),
                                        GTK_CHECK_BUTTON(first_check_button));

        gtk_widget_set_halign (GTK_WIDGET(check_button), GTK_ALIGN_START);

        if (i == default_value) /* default is first radio button */
        {
            gtk_check_button_set_active (GTK_CHECK_BUTTON(check_button), TRUE);
            radio_result = default_value;
        }

        gtk_widget_set_visible (GTK_WIDGET(check_button), TRUE);
        gtk_box_append (GTK_BOX(vbox), GTK_WIDGET(check_button));
        g_object_set_data (G_OBJECT(check_button), INDEX_LABEL, GINT_TO_POINTER(i));
        g_signal_connect (G_OBJECT(check_button), "clicked",
                          G_CALLBACK(gnc_choose_check_button_cb),
                          &radio_result);
    }

    if (!button_name)
        button_name = _("_OK");
    dialog = gtk_dialog_new_with_buttons (title,
                                          GTK_WINDOW(parent),
                                          GTK_DIALOG_DESTROY_WITH_PARENT,
                                          _("_Cancel"),
                                          GTK_RESPONSE_CANCEL,
                                          button_name, 
                                          GTK_RESPONSE_OK,
                                          NULL);

    /* default to ok */
    gtk_dialog_set_default_response (GTK_DIALOG(dialog), GTK_RESPONSE_OK);

    dvbox = gtk_dialog_get_content_area (GTK_DIALOG(dialog));

    gtk_box_append (GTK_BOX(dvbox), GTK_WIDGET(main_vbox));

    if (gnc_dialog_run (GTK_DIALOG(dialog)) != GTK_RESPONSE_OK)
        radio_result = -1;

    return radio_result;
}

static gchar *
gnc_input_dialog_internal (GtkWidget *parent, const gchar *title,
                           const gchar *msg, const gchar *default_input,
                           gboolean use_entry)
{
    gint result;
    GtkWidget *view;
    GtkTextBuffer *buffer;
    gchar *user_input = NULL;
    GtkTextIter start, end;
    
    /* Create the widgets */
    GtkWidget* dialog = gtk_dialog_new_with_buttons (title,
                                                     GTK_WINDOW(parent),
                                                     GTK_DIALOG_MODAL |
                                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                                     _("_OK"), GTK_RESPONSE_ACCEPT,
                                                     _("_Cancel"), GTK_RESPONSE_REJECT,
                                                     NULL);
    GtkWidget* content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
    
    // add a label
    GtkWidget* label = gtk_label_new (msg);
    gtk_box_append (GTK_BOX(content_area), GTK_WIDGET(label));

    // add a textview or an entry.
    if (use_entry)
    {
        view = gtk_entry_new ();
        gnc_entry_set_text (GTK_ENTRY (view), default_input);
    }
    else
    {
        view = gtk_text_view_new ();
        gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (view), GTK_WRAP_WORD_CHAR);
        buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));
        gtk_text_buffer_set_text (buffer, default_input, -1);
    }
    gtk_box_append (GTK_BOX(content_area), GTK_WIDGET(view));

    // run the dialog
    result = gnc_dialog_run (GTK_DIALOG(dialog));
    
    if (result != GTK_RESPONSE_REJECT)
    {
        if (use_entry)
            user_input = g_strdup (gnc_entry_get_text ((GTK_ENTRY(view))));
        else
        {
            gtk_text_buffer_get_start_iter (buffer, &start);
            gtk_text_buffer_get_end_iter (buffer, &end);
            user_input = gtk_text_buffer_get_text (buffer, &start, &end, FALSE);
        }
    }
    return user_input;
}

/********************************************************************\
 * gnc_input_dialog                                                 *
 *   simple convenience dialog to get a single value from the user  *
 *   user may choose between "Ok" and "Cancel"                      *
 *                                                                  *
 * NOTE: This function does not return until the dialog is closed   *
 *                                                                  *
 * Args:   parent  - the parent window or NULL                      *
 *         title   - the title of the dialog                        *
 *         msg     - the message to display                         *
 *         default_input - will be displayed as default input       *
 * Return: the input (text) the user entered, if pressed "Ok"       *
 *         NULL, if pressed "Cancel"                                *
 \********************************************************************/
gchar *
gnc_input_dialog (GtkWidget *parent, const gchar *title, const gchar *msg,
                  const gchar *default_input)
{
    return gnc_input_dialog_internal (parent, title, msg, default_input, FALSE);
}

/********************************************************************\
 * gnc_input_dialog_with_entry                                      *
 *   Similar to gnc_input_dialog but use a single line entry widget *
 *   user may choose between "Ok" and "Cancel"                      *
 \********************************************************************/
gchar *
gnc_input_dialog_with_entry (GtkWidget *parent, const gchar *title,
                             const gchar *msg, const gchar *default_input)
{
    return gnc_input_dialog_internal (parent, title, msg, default_input, TRUE);
}

void
gnc_info2_dialog (GtkWidget *parent, const gchar *title, const gchar *msg)
{
    GtkWidget *view;
    GtkTextBuffer *buffer;
    gint width, height;
    
    /* Create the widgets */
    GtkWidget* dialog = gtk_dialog_new_with_buttons (title,
                                                     GTK_WINDOW(parent),
                                                     GTK_DIALOG_MODAL |
                                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                                     _("_OK"), GTK_RESPONSE_ACCEPT,
                                                     NULL);
    GtkWidget* content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
    
    // add a scroll area
    GtkWidget* scrolled_window = gtk_scrolled_window_new ();
    gtk_box_append (GTK_BOX(content_area), GTK_WIDGET(scrolled_window));

    // add a textview
    view = gtk_text_view_new ();
    gtk_text_view_set_editable (GTK_TEXT_VIEW(view), FALSE);
    buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(view));
    gtk_text_buffer_set_text (buffer, msg, -1);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_WIDGET(view));

    // run the dialog
    if (parent)
    {
        gtk_window_get_default_size (GTK_WINDOW(parent), &width, &height);
        gtk_window_set_default_size (GTK_WINDOW(dialog), width, height);
    }
    gnc_dialog_run (GTK_DIALOG(dialog));
}
