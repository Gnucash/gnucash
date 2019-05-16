/*
 * dialog-ab-daterange.c --
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

/**
 * @internal
 * @file dialog-daterange.c
 * @brief Dialog for date range entry
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include <config.h>

#include "dialog-ab-daterange.h"
#include "dialog-utils.h"
#include "gnc-date-edit.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

typedef struct _DaterangeInfo DaterangeInfo;

void ddr_toggled_cb(GtkToggleButton *button, gpointer user_data);

struct _DaterangeInfo
{
    GtkWidget *enter_from_button;
    GtkWidget *enter_to_button;
    GtkWidget *from_dateedit;
    GtkWidget *to_dateedit;
};

gboolean
gnc_ab_enter_daterange(GtkWidget *parent,
                       const char *heading,
                       time64 *from_date,
                       gboolean *last_retv_date,
                       gboolean *first_possible_date,
                       time64 *to_date,
                       gboolean *to_now)
{
    GtkBuilder *builder;
    GtkWidget *dialog;
    GtkWidget *heading_label;
    GtkWidget *first_button;
    GtkWidget *last_retrieval_button;
    GtkWidget *now_button;
    DaterangeInfo info;
    gint result;

    ENTER("");

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-ab.glade", "aqbanking_date_range_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "aqbanking_date_range_dialog"));

    /* Connect the signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, &info );

    if (parent)
        gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(parent));

    heading_label  = GTK_WIDGET(gtk_builder_get_object (builder, "date_heading_label"));
    first_button  = GTK_WIDGET(gtk_builder_get_object (builder, "first_button"));
    last_retrieval_button  = GTK_WIDGET(gtk_builder_get_object (builder, "last_retrieval_button"));
    info.enter_from_button  = GTK_WIDGET(gtk_builder_get_object (builder, "enter_from_button"));
    now_button  = GTK_WIDGET(gtk_builder_get_object (builder, "now_button"));
    info.enter_to_button  = GTK_WIDGET(gtk_builder_get_object (builder, "enter_to_button"));

    info.from_dateedit = gnc_date_edit_new (*from_date, FALSE, FALSE);
    gtk_container_add(GTK_CONTAINER(gtk_builder_get_object (builder, "enter_from_box")),
                      info.from_dateedit);
    gtk_widget_show(info.from_dateedit);

    info.to_dateedit = gnc_date_edit_new (*to_date, FALSE, FALSE);
    gtk_container_add(GTK_CONTAINER(gtk_builder_get_object (builder, "enter_to_box")),
                      info.to_dateedit);
    gtk_widget_show(info.to_dateedit);

    if (*last_retv_date)
    {
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(last_retrieval_button),
                                     TRUE);
    }
    else
    {
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(first_button), TRUE);
        gtk_widget_set_sensitive(last_retrieval_button, FALSE);
    }

    gtk_widget_set_sensitive(info.from_dateedit, FALSE);
    gtk_widget_set_sensitive(info.to_dateedit, FALSE);

    gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK);

    if (heading)
        gtk_label_set_text(GTK_LABEL(heading_label), heading);

    gtk_widget_show(dialog);

    result = gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_hide(dialog);

    if (result == GTK_RESPONSE_OK)
    {
        *from_date = gnc_date_edit_get_date(GNC_DATE_EDIT(info.from_dateedit));
        *last_retv_date = gtk_toggle_button_get_active(
                              GTK_TOGGLE_BUTTON(last_retrieval_button));
        *first_possible_date = gtk_toggle_button_get_active(
                                   GTK_TOGGLE_BUTTON(first_button));
        *to_date = gnc_date_edit_get_date (GNC_DATE_EDIT(info.to_dateedit));
        *to_now = gtk_toggle_button_get_active(
                      GTK_TOGGLE_BUTTON(now_button));
    }

    g_object_unref(G_OBJECT(builder));

    gtk_widget_destroy(dialog);

    LEAVE("");
    return result == GTK_RESPONSE_OK;
}

void
ddr_toggled_cb(GtkToggleButton *button, gpointer user_data)
{
    DaterangeInfo *info = user_data;

    g_return_if_fail(info);

    gtk_widget_set_sensitive(info->from_dateedit,
                             gtk_toggle_button_get_active(
                                 GTK_TOGGLE_BUTTON(info->enter_from_button)));
    gtk_widget_set_sensitive(info->to_dateedit,
                             gtk_toggle_button_get_active(
                                 GTK_TOGGLE_BUTTON(info->enter_to_button)));
}
