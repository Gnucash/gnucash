/********************************************************************\
 * dialog-totd.c : dialog to display a "tip of the day"             *
 *                                                                  *
 * Initial copyright not recorded.                                  *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
 * Copyright (c) 2011 Robert Fewell                                 *
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

#include "config.h"
#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-totd.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-filepath-utils.h"
#include "gnc-prefs.h"
#include "gnc-gnome-utils.h"
#include "gnc-engine.h"

#define GNC_PREFS_GROUP      "dialogs.totd"
#define GNC_PREF_CURRENT_TIP "current-tip"
#define GNC_PREF_SHOW_TIPS   "show-at-startup"
#define DIALOG_TOTD_CM_CLASS "dialog-totd"

#define GNC_RESPONSE_FORWARD 1
#define GNC_RESPONSE_BACK    2

/* Callbacks */
void gnc_totd_dialog_response_cb (GtkDialog *dialog, gint reponse, gpointer user_data);
void gnc_totd_dialog_startup_toggled_cb (GtkToggleButton *button, gpointer user_data);

/* The Tips */
static gchar **tip_list;
static gint tip_count = -1;
static gint current_tip_number = -1;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

typedef struct
{
    GtkWidget   *dialog;
    GtkTextView *textview;
    GtkWidget   *showcheck_button;
} TotdDialog;


/***********************************************************************
 *  This function should be called to change the tip number.  It
 *  handles clamping the number to the range of tips available, saving
 *  the number in the preferences database, and updating the dialog window
 *  with the text of the newly selected tip.
 *
 *  @param Tip of the day structure. This points to the dialog and
 *  the GtkTextView widget that holds the text of the tip.
 *
 *  @param offset Which tip to show.  If the value is zero then the
 *  current tip will be shown.  If the value is negative the previous
 *  tip will be shown.  If the value is positive the next tip will be
 *  shown.
 ************************************************************************/
static void
gnc_new_tip_number (TotdDialog *totd_dialog, gint offset)
{

    gchar **tip_components = NULL;
    gchar *tip;

    ENTER("TotdDialog %p, offset %d", totd_dialog, offset);
    current_tip_number += offset;
    DEBUG("clamp %d to '0 <= x < %d'", current_tip_number, tip_count);
    if (current_tip_number < 0)
        current_tip_number = tip_count - 1;
    if (current_tip_number >= tip_count)
        current_tip_number = 0;
    gnc_prefs_set_int(GNC_PREFS_GROUP, GNC_PREF_CURRENT_TIP, current_tip_number);

    /* A tip consists of a translatable string, which might contain a %s
     * placeholder, optionally followed by a | and a (non-translated)
     * string to put in the placeholder. For example:
     *
     *  Welcome to GnuCash version %s|2.4
     */
    if (tip_list[current_tip_number])
	tip_components = g_strsplit(tip_list[current_tip_number], "|", 0);
    /* If the tip is empty, g_strisplit will return an empty list. This
     * shouldn't normally happen, but make sure we don't crash just in
     * case */
    if (tip_components[0] == NULL)
    {
        tip = g_strdup("");
    }
    else
    {
        /* Use printf to do the substitution. Note that if there is no |
         * in the tip, tip_components[1] will be the terminating NULL,
         * so this will never cause an out-of-bounds array access.
         */
        tip = g_strdup_printf( _(tip_components[0]), tip_components[1]);
    }

    g_strfreev(tip_components);
    gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(totd_dialog->textview)),
                             tip, -1);
    g_free(tip);
    LEAVE("");
}


/***************************/
/*    Dialog Callbacks     */
/***************************/
void gnc_totd_dialog_response_cb (GtkDialog *dialog,
                                  gint       response,
                                  gpointer   user_data)
{
    TotdDialog *totd_dialog = user_data;

    ENTER("dialog %p, response %d, user_data %p", dialog, response, user_data);
    switch (response)
    {
    case GNC_RESPONSE_FORWARD:
        gnc_new_tip_number(totd_dialog, 1);
        break;

    case GNC_RESPONSE_BACK:
        gnc_new_tip_number(totd_dialog, -1);
        break;

    case GTK_RESPONSE_CLOSE:
        gnc_save_window_size(GNC_PREFS_GROUP, GTK_WINDOW(totd_dialog->dialog));
        /* fall through */

    default:
        gnc_unregister_gui_component_by_data(DIALOG_TOTD_CM_CLASS, totd_dialog);
        gtk_widget_destroy(GTK_WIDGET(totd_dialog->dialog));
        break;
    }
    LEAVE("");
}


void
gnc_totd_dialog_startup_toggled_cb (GtkToggleButton *button,
                                    gpointer user_data)
{
    gboolean active;

    active = gtk_toggle_button_get_active(button);
    gnc_prefs_set_bool(GNC_PREFS_GROUP, GNC_PREF_SHOW_TIPS, active);
}


/***********************************/
/*     Tip of the Day Parser       */
/***********************************/
static gboolean
gnc_totd_initialize (void)
{
    gchar *filename, *contents, *new_str;
    gsize length;
    GError *error;

    /* Find the file */
    filename = gnc_filepath_locate_data_file("tip_of_the_day.list");
    if (!filename)
        return FALSE;

    /* Read it */
    if (!g_file_get_contents(filename, &contents, &length, &error))
    {
        printf("Unable to read file: %s\n", error->message);
        g_error_free(error);
        g_free(filename);
        return FALSE;
    }
    g_free(filename);

    /* Split into multiple strings. Due to the nature of the
     * tip list file, this can contain empty strings */
    if (contents)
	tip_list = g_strsplit(contents, "\n", 0);
    g_free(contents);
    contents = NULL;

    /* Remove the empty strings */
    for (tip_count = 0; tip_list[tip_count] != NULL; tip_count++)
    {
        if (*tip_list[tip_count]!='\0')
        {
            g_strstrip(tip_list[tip_count]);
            if (!contents)
                contents = g_strdup (tip_list[tip_count]);
            else
            {
                new_str = g_strjoin ("\n", contents, tip_list[tip_count], NULL);
                g_free (contents);
                contents = new_str;
            }
        }
    }

    /* Split cleaned up contents into multiple strings again */
    g_strfreev (tip_list);
    if (contents)
        tip_list = g_strsplit(contents, "\n", 0);

    /* Convert any escaped characters while counting the strings */
    for (tip_count = 0; tip_list[tip_count] != NULL; tip_count++)
    {
        new_str = g_strcompress(tip_list[tip_count]);
        g_free(tip_list[tip_count]);
        tip_list[tip_count] = new_str;
    }


    /* Don't continue when no tips were found, to prevent
     * gnc_new_tip_number doesn't handle that case (it would try to
     * display the terminating NULL). There's nothing to show
     * anyway...*/
    if (tip_count == 0)
    {
        PWARN("No tips found - Tips of the day window won't be displayed.");
        return FALSE;
    }

    return TRUE;
}


/***********************************************************************
 *  Raise the totd dialog to the top of the window stack.  This
 *  function is called if the user attempts to create a second totd
 *  dialog.
 *
 *  @internal
 *
 *  @param class_name Unused.
 *
 *  @param component_id Unused.
 *
 *  @param user_data A pointer to the totd structure.
 *
 *  @param iter_data Unused.
 ***********************************************************************/
static gboolean
show_handler (const char *class_name, gint component_id,
              gpointer user_data, gpointer iter_data)
{
    TotdDialog *totd_dialog = user_data;

    ENTER(" ");
    if (!totd_dialog)
    {
        LEAVE("no data strucure");
        return(FALSE);
    }

    gtk_window_present(GTK_WINDOW(totd_dialog->dialog));
    LEAVE(" ");
    return(TRUE);
}


/****************************************************
 *  Close the totd dialog.
 *
 *  @internal
 *
 *  @param user_data A pointer to the totd structure.
 ****************************************************/
static void
close_handler (gpointer user_data)
{
    TotdDialog *totd_dialog = user_data;

    ENTER(" ");

    gnc_unregister_gui_component_by_data(DIALOG_TOTD_CM_CLASS, totd_dialog);

    LEAVE(" ");
}


/*************************************/
/*     Create the TotD Dialog        */
/*************************************/
void
gnc_totd_dialog (GtkWindow *parent, gboolean startup)
{
    TotdDialog *totd_dialog;

    GtkBuilder *builder;
    GtkWidget *dialog, *button;
    GtkTextView *textview;
    gboolean show_tips;

    totd_dialog = g_new0 (TotdDialog, 1);

    show_tips = gnc_prefs_get_bool(GNC_PREFS_GROUP, GNC_PREF_SHOW_TIPS);
    if (startup && !show_tips)
        return;

    if (tip_count == -1)
    {
        if (!gnc_totd_initialize())
            return;
        current_tip_number =  gnc_prefs_get_int(GNC_PREFS_GROUP, GNC_PREF_CURRENT_TIP);
    }

    if (gnc_forall_gui_components(DIALOG_TOTD_CM_CLASS, show_handler, NULL))
    {
        return;
    }

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-totd.glade", "totd_dialog");
    dialog  = GTK_WIDGET(gtk_builder_get_object (builder, "totd_dialog"));
    gtk_window_set_transient_for(GTK_WINDOW (dialog), parent);

    totd_dialog->dialog = dialog;

    ENTER("totd_dialog %p, dialog %p", totd_dialog, dialog);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, totd_dialog);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "show_checkbutton"));
    totd_dialog->showcheck_button = button;

    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON (button), show_tips);

    textview = GTK_TEXT_VIEW(gtk_builder_get_object (builder, "tip_textview"));
    totd_dialog->textview = textview;

    gnc_new_tip_number(totd_dialog, 1);

    gnc_restore_window_size(GNC_PREFS_GROUP, GTK_WINDOW(totd_dialog->dialog));
    gtk_widget_show(GTK_WIDGET (totd_dialog->dialog));

    gnc_register_gui_component(DIALOG_TOTD_CM_CLASS,
                               NULL, close_handler, totd_dialog);

    g_object_unref(G_OBJECT(builder));

    LEAVE("");
}
