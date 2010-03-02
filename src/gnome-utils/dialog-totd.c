/*
 * dialog-totd.c : dialog to display a "tip of the day"
 *
 * Initial copyright not recorded.
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glade/glade-xml.h>

#include "dialog-totd.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-gconf-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-engine.h"


#define GCONF_SECTION   "dialogs/tip_of_the_day"
#define KEY_CURRENT_TIP "current_tip"
#define KEY_SHOW_TIPS   "show_at_startup"
#define DIALOG_TOTD_CM_CLASS	"dialog-totd"

#define GNC_RESPONSE_FORWARD 1
#define GNC_RESPONSE_BACK    2

/* Callbacks */
void gnc_totd_dialog_response (GtkDialog *dialog, gint reponse, gpointer user_data);
void gnc_totd_dialog_startup_toggled (GtkToggleButton *button, gpointer user_data);

/* The Tips */
static gchar **tip_list;
static gint tip_count = -1;
static gint current_tip_number = -1;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;


/*********************/
/* Utility Functions */
/*********************/

/** This function should be called to change the tip number.  It
 *  handles clamping the number to the range of tips available, saving
 *  the number in the GConf database, and updating the dialog window
 *  with the text of the newly selected tip.
 *
 *  @param widget A pointer to any widget in the dialog.  This widget
 *  is used as a starting point to find the GtkTextView widget that
 *  holds the text of the tip.
 *
 *  @param offset Which tip to show.  If the value is zero then the
 *  current tip will be shown.  If the value is negative the previous
 *  tip will be shown.  If the value is positive the next tip will be
 *  shown.
 */
static void
gnc_new_tip_number (GtkWidget *widget,
                    gint offset)
{
    GtkWidget *textview;

    ENTER("widget %p, offset %d", widget, offset);
    current_tip_number += offset;
    DEBUG("clamp %d to '0 <= x < %d'", current_tip_number, tip_count);
    if (current_tip_number < 0)
        current_tip_number = tip_count - 1;
    if (current_tip_number >= tip_count)
        current_tip_number = 0;
    gnc_gconf_set_int(GCONF_SECTION, KEY_CURRENT_TIP, current_tip_number, NULL);

    textview = gnc_glade_lookup_widget(widget, "tip_textview");
    gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview)),
                             _(tip_list[current_tip_number]), -1);
    LEAVE("");
}


/********************/
/*    Callbacks     */
/********************/

void gnc_totd_dialog_response (GtkDialog *dialog,
                               gint       response,
                               gpointer   user_data)
{
    ENTER("dialog %p, response %d, user_data %p", dialog, response, user_data);
    switch (response)
    {
    case GNC_RESPONSE_FORWARD:
        gnc_new_tip_number(GTK_WIDGET(dialog), 1);
        break;

    case GNC_RESPONSE_BACK:
        gnc_new_tip_number(GTK_WIDGET(dialog), -1);
        break;

    case GTK_RESPONSE_CLOSE:
        gnc_save_window_size(GCONF_SECTION, GTK_WINDOW(dialog));
        /* fall through */

    default:
        gnc_unregister_gui_component_by_data(DIALOG_TOTD_CM_CLASS, dialog);
        gtk_widget_destroy(GTK_WIDGET(dialog));
        break;
    }
    LEAVE("");
}

void
gnc_totd_dialog_startup_toggled (GtkToggleButton *button,
                                 gpointer user_data)
{
    gboolean active;

    active = gtk_toggle_button_get_active(button);
    gnc_gconf_set_bool(GCONF_SECTION, KEY_SHOW_TIPS, active, NULL);
}

/********************/
/*     Parser       */
/********************/

static gboolean
gnc_totd_initialize (void)
{
    gchar *filename, *contents, *new, *found;
    gsize length;
    GError *error;

    /* Find the file */
    filename = gnc_gnome_locate_data_file("tip_of_the_day.list");
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

    /* Replace maximal substrings of more than two newlines by \n\n,
     * remove leading and trailing \n\n */
    while ((found = strstr(contents, "\n\n\n")) != NULL)
    {
        *found++ = '\0';
        while (*found == '\n') found++;
        if (*contents && *found)
        {
            /* put \n\n between the two nonempty parts */
            new = g_strdup_printf("%s\n\n%s", contents, found);
            g_free(contents);
            contents = new;
        }
        else if (*found)
        {
            /* remove leading newlines */
            new = g_strdup(found);
            g_free(contents);
            contents = new;
        }
    }

    /* Split into multiple strings */
    tip_list = g_strsplit(contents, "\n\n", 0);

    /* Convert any escaped characters while counting the strings */
    for (tip_count = 0; tip_list[tip_count] != NULL; tip_count++)
    {

        g_strstrip(tip_list[tip_count]);
        new = g_strcompress(g_strdelimit(tip_list[tip_count], "\n", ' '));
        g_free(tip_list[tip_count]);
        tip_list[tip_count] = new;
    }

    g_free(contents);
    g_free(filename);
    return TRUE;
}

/** Raise the totd dialog to the top of the window stack.  This
 *  function is called if the user attempts to create a second totd
 *  dialog.
 *
 *  @internal
 *
 *  @param class Unused.
 *
 *  @param component_id Unused.
 *
 *  @param user_data A pointer to the totd dialog.
 *
 *  @param iter_data Unused.
 */
static gboolean
show_handler (const char *class, gint component_id,
              gpointer user_data, gpointer iter_data)
{
    GtkWidget *dialog;

    ENTER(" ");
    dialog = GTK_WIDGET(user_data);
    gtk_window_present(GTK_WINDOW(dialog));
    LEAVE(" ");
    return(TRUE);
}

/** Close the totd dialog.
 *
 *  @internal
 *
 *  @param user_data A pointer to the totd dialog.
 */
static void
close_handler (gpointer user_data)
{
    GtkWidget *dialog;

    ENTER(" ");
    dialog = GTK_WIDGET(user_data);
    gnc_unregister_gui_component_by_data(DIALOG_TOTD_CM_CLASS, dialog);
    gtk_widget_destroy(dialog);
    LEAVE(" ");
}


/********************/
/*      Main        */
/********************/

void
gnc_totd_dialog (GtkWindow *parent, gboolean startup)
{
    GladeXML *xml;
    GtkWidget *dialog, *button;
    gboolean show_tips;

    show_tips = gnc_gconf_get_bool(GCONF_SECTION, KEY_SHOW_TIPS, NULL);
    if (startup && !show_tips)
        return;

    if (tip_count == -1)
    {
        if (!gnc_totd_initialize())
            return;
        current_tip_number =  gnc_gconf_get_int(GCONF_SECTION, KEY_CURRENT_TIP, NULL);
    }

    if (gnc_forall_gui_components(DIALOG_TOTD_CM_CLASS, show_handler, NULL))
    {
        return;
    }

    xml = gnc_glade_xml_new ("totd.glade", "totd_dialog");
    dialog  = glade_xml_get_widget (xml, "totd_dialog");
    gtk_window_set_transient_for(GTK_WINDOW (dialog), parent);
    glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func,
                                      dialog);

    gnc_new_tip_number(dialog, 1);

    button = glade_xml_get_widget(xml, "show_checkbutton");
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON (button), show_tips);

    gnc_restore_window_size(GCONF_SECTION, GTK_WINDOW(dialog));
    gtk_widget_show(GTK_WIDGET (dialog));

    gnc_register_gui_component(DIALOG_TOTD_CM_CLASS,
                               NULL, close_handler, dialog);
}
