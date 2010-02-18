/********************************************************************\
 * dialog-daterange.c -- dialog for date range entry                *
 * Copyright (C) 2002 Christian Stimming                            *
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

#include "dialog-utils.h"
#include "gnc-ui.h"
#include "gnc-date-edit.h"
#include "dialog-daterange.h"

typedef struct
{
    GtkWidget *enter_from_button;
    GtkWidget *enter_to_button;
    GtkWidget *from_dateedit;
    GtkWidget *to_dateedit;
} DaterangeInfo;

void on_button_toggled (GtkToggleButton *button, gpointer user_data);


gboolean
gnc_hbci_enter_daterange (GtkWidget *parent,
                          const char *heading,
                          Timespec *from_date,
                          gboolean *last_retv_date,
                          gboolean *first_possible_date,
                          Timespec *to_date,
                          gboolean *to_now)
{
    GtkWidget *dialog;
    GladeXML *xml;
    gint result;
    DaterangeInfo info;

    GtkWidget *heading_label;
    GtkWidget *last_retrieval_button;
    GtkWidget *first_button;
    GtkWidget *now_button;

    xml = gnc_glade_xml_new ("hbci.glade", "HBCI_daterange_dialog");

    dialog = glade_xml_get_widget (xml, "HBCI_daterange_dialog");

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));

    heading_label  = glade_xml_get_widget (xml, "heading_label");
    last_retrieval_button  = glade_xml_get_widget (xml, "last_retrieval_button");
    first_button  = glade_xml_get_widget (xml, "first_button");
    info.enter_from_button  = glade_xml_get_widget (xml, "enter_from_button");
    info.enter_to_button  = glade_xml_get_widget (xml, "enter_to_button");
    now_button  = glade_xml_get_widget (xml, "now_button");

    info.from_dateedit = gnc_date_edit_new_ts (*from_date, FALSE, FALSE);
    gtk_container_add (GTK_CONTAINER (glade_xml_get_widget
                                      (xml, "enter_from_box")),
                       info.from_dateedit);

    info.to_dateedit = gnc_date_edit_new_ts (*to_date, FALSE, FALSE);
    gtk_container_add (GTK_CONTAINER (glade_xml_get_widget
                                      (xml, "enter_to_box")),
                       info.to_dateedit);

    if (*last_retv_date == FALSE)
    {
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (first_button), TRUE);
        gtk_widget_set_sensitive (last_retrieval_button, FALSE);
    }

    gtk_widget_set_sensitive (info.from_dateedit, FALSE);
    gtk_widget_set_sensitive (info.to_dateedit, FALSE);
    g_signal_connect (info.enter_from_button, "toggled",
                      G_CALLBACK (on_button_toggled), &info);
    g_signal_connect (info.enter_to_button, "toggled",
                      G_CALLBACK (on_button_toggled), &info);

    gtk_dialog_set_default_response (GTK_DIALOG (dialog), 1);

    if (heading)
        gtk_label_set_text (GTK_LABEL (heading_label), heading);

    gtk_widget_grab_focus (glade_xml_get_widget (xml, "ok_button"));

    gtk_widget_show_all (GTK_WIDGET (dialog));

    result = gtk_dialog_run (GTK_DIALOG (dialog));
    if (result >= 0)
        gtk_widget_hide (GTK_WIDGET (dialog));

    if (result == 1)
    {
        *from_date = gnc_date_edit_get_date_ts (GNC_DATE_EDIT (info.from_dateedit));
        *last_retv_date = gtk_toggle_button_get_active
                          (GTK_TOGGLE_BUTTON (last_retrieval_button));
        *first_possible_date = gtk_toggle_button_get_active
                               (GTK_TOGGLE_BUTTON (first_button));
        *to_date = gnc_date_edit_get_date_ts (GNC_DATE_EDIT (info.to_dateedit));
        *to_now = gtk_toggle_button_get_active
                  (GTK_TOGGLE_BUTTON (now_button));

        gtk_widget_destroy (GTK_WIDGET (dialog));
        return TRUE;
    }

    if (result >= 0)
        gtk_widget_destroy (GTK_WIDGET (dialog));
    return FALSE;
}

void on_button_toggled (GtkToggleButton *button, gpointer user_data)
{
    DaterangeInfo *info = user_data;
    g_assert (info);

    gtk_widget_set_sensitive (info->from_dateedit,
                              gtk_toggle_button_get_active
                              (GTK_TOGGLE_BUTTON (info->enter_from_button)));
    gtk_widget_set_sensitive (info->to_dateedit,
                              gtk_toggle_button_get_active
                              (GTK_TOGGLE_BUTTON (info->enter_to_button)));
}
