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
    GtkWidget *last_retrieval_button;
    GtkWidget *from_dateedit;
    GtkWidget *to_dateedit;
};

typedef struct
{
    GtkBuilder *builder;
    DaterangeInfo widgets;
    GncABDateRangeCallback completed;
    gpointer user_data;
    time64 from_date;
    gboolean last_retrieval_date;
    gboolean earliest_date;
    time64 to_date;
    gboolean until_now;
    GWeakRef owner_parent;
    gulong parent_destroy_handler;
    gboolean has_parent;
    gboolean parent_destroyed;
    GtkWidget *dialog;
    gulong capture_handler;
} DaterangeRequest;

static void
daterange_disconnect_builder_handlers (GtkWidget *widget, gpointer data)
{
    g_signal_handlers_disconnect_by_data (widget, data);
    if (!GTK_IS_CONTAINER (widget))
        return;
    GList *children = gtk_container_get_children (GTK_CONTAINER (widget));
    for (GList *node = children; node; node = node->next)
        daterange_disconnect_builder_handlers (GTK_WIDGET (node->data), data);
    g_list_free (children);
}

static void
daterange_parent_destroyed (G_GNUC_UNUSED GtkWidget *parent,
                            DaterangeRequest *request)
{
    request->parent_destroyed = TRUE;
}

static void
daterange_capture_response (GtkDialog *dialog, gint response, gpointer user_data)
{
    DaterangeRequest *request = user_data;
    DaterangeInfo *info = &request->widgets;

    if (response != GTK_RESPONSE_OK)
        return;
    request->from_date = gnc_date_edit_get_date (GNC_DATE_EDIT (info->from_dateedit));
    request->last_retrieval_date = gtk_toggle_button_get_active (
        GTK_TOGGLE_BUTTON (info->last_retrieval_button));
    request->earliest_date = gtk_toggle_button_get_active (
        GTK_TOGGLE_BUTTON (gtk_builder_get_object (request->builder, "first_button")));
    request->to_date = gnc_date_edit_get_date (GNC_DATE_EDIT (info->to_dateedit));
    request->until_now = gtk_toggle_button_get_active (
        GTK_TOGGLE_BUTTON (gtk_builder_get_object (request->builder, "now_button")));
}

static void
daterange_completed (GtkWindow *parent, gint response, gpointer user_data)
{
    DaterangeRequest *request = user_data;
    GncABDateRangeCallback completed = request->completed;
    gpointer callback_data = request->user_data;
    GtkWidget *owner = request->has_parent ?
        GTK_WIDGET (g_weak_ref_get (&request->owner_parent)) : NULL;
    gboolean accepted = parent && response == GTK_RESPONSE_OK &&
        (!request->has_parent || (owner && !request->parent_destroyed &&
         !gtk_widget_in_destruction (owner)));

    if (owner && request->parent_destroy_handler &&
        g_signal_handler_is_connected (owner, request->parent_destroy_handler))
        g_signal_handler_disconnect (owner, request->parent_destroy_handler);
    if (request->capture_handler && request->dialog &&
        g_signal_handler_is_connected (request->dialog, request->capture_handler))
        g_signal_handler_disconnect (request->dialog, request->capture_handler);
    if (request->dialog)
        daterange_disconnect_builder_handlers (request->dialog,
                                                &request->widgets);
    if (request->has_parent)
        g_weak_ref_clear (&request->owner_parent);
    g_clear_object (&owner);

    if (request->builder)
        g_object_unref (request->builder);
    completed (accepted, request->from_date, request->last_retrieval_date,
               request->earliest_date, request->to_date, request->until_now,
               callback_data);
    g_free (request);
}

void
gnc_ab_enter_daterange_async (GtkWindow *parent, const gchar *heading,
                              time64 from_date,
                              gboolean last_retrieval_date,
                              gboolean earliest_date, time64 to_date,
                              gboolean until_now,
                              GncABDateRangeCallback completed,
                              gpointer user_data)
{
    GtkBuilder *builder;
    GtkWidget *dialog;
    GtkWidget *heading_label;
    GtkWidget *first_button;
    GtkWidget *last_retrieval_button;
    GtkWidget *now_button;
    DaterangeRequest *request;
    DaterangeInfo *info;

    g_return_if_fail (completed != NULL);
    ENTER("");

    request = g_new0 (DaterangeRequest, 1);
    request->completed = completed;
    request->user_data = user_data;
    request->from_date = from_date;
    request->last_retrieval_date = last_retrieval_date;
    request->earliest_date = earliest_date;
    request->to_date = to_date;
    request->until_now = until_now;
    if (parent)
    {
        g_weak_ref_init (&request->owner_parent, G_OBJECT (parent));
        request->has_parent = TRUE;
        request->parent_destroy_handler = g_signal_connect (
            parent, "destroy", G_CALLBACK (daterange_parent_destroyed), request);
    }
    info = &request->widgets;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-ab.glade", "aqbanking_date_range_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "aqbanking_date_range_dialog"));

    /* Connect the signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, info );

    if (parent)
    {
        gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(parent));
        gtk_window_set_destroy_with_parent (GTK_WINDOW (dialog), TRUE);
    }

    heading_label  = GTK_WIDGET(gtk_builder_get_object (builder, "date_heading_label"));
    first_button  = GTK_WIDGET(gtk_builder_get_object (builder, "first_button"));
    last_retrieval_button  = info->last_retrieval_button = GTK_WIDGET(
        gtk_builder_get_object (builder, "last_retrieval_button"));
    info->enter_from_button  = GTK_WIDGET(gtk_builder_get_object (builder, "enter_from_button"));
    now_button  = GTK_WIDGET(gtk_builder_get_object (builder, "now_button"));
    info->enter_to_button  = GTK_WIDGET(gtk_builder_get_object (builder, "enter_to_button"));

    info->from_dateedit = gnc_date_edit_new (from_date, FALSE, FALSE);
    gtk_container_add(GTK_CONTAINER(gtk_builder_get_object (builder, "enter_from_box")),
                      info->from_dateedit);
    gtk_widget_show(info->from_dateedit);

    info->to_dateedit = gnc_date_edit_new (to_date, FALSE, FALSE);
    gtk_container_add(GTK_CONTAINER(gtk_builder_get_object (builder, "enter_to_box")),
                      info->to_dateedit);
    gtk_widget_show(info->to_dateedit);

    if (last_retrieval_date)
    {
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(last_retrieval_button),
                                     TRUE);
    }
    else
    {
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(first_button), earliest_date);
        gtk_widget_set_sensitive(last_retrieval_button, FALSE);
    }

    gtk_widget_set_sensitive(info->from_dateedit, FALSE);
    gtk_widget_set_sensitive(info->to_dateedit, FALSE);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (now_button), until_now);

    gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK);

    if (heading)
        gtk_label_set_text(GTK_LABEL(heading_label), heading);

    request->builder = builder;
    request->dialog = dialog;
    request->capture_handler = g_signal_connect (dialog, "response",
        G_CALLBACK (daterange_capture_response), request);
    gtk_widget_show (dialog);
    gnc_dialog_run_async (GTK_DIALOG (dialog), NULL, daterange_completed, request);
    LEAVE("");
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
