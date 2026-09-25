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
G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

typedef struct
{
    GtkWidget *dialog;
    GtkBuilder *builder;
    GTask *task;
    GtkWidget *enter_from_button;
    GtkWidget *enter_to_button;
    GtkWidget *from_dateedit;
    GtkWidget *to_dateedit;
    GtkWidget *first_button;
    GtkWidget *last_retrieval_button;
    GtkWidget *now_button;
} DaterangeInfo;

void ddr_toggled_cb (GtkCheckButton *button, gpointer user_data);

static void daterange_complete (DaterangeInfo *info, gboolean accepted);

static void
daterange_accept_clicked (GtkButton *button, gpointer user_data)
{
    (void)button;
    daterange_complete (user_data, TRUE);
}

static void
daterange_cancel_clicked (GtkButton *button, gpointer user_data)
{
    (void)button;
    daterange_complete (user_data, FALSE);
}

static gboolean
daterange_close_requested (GtkWindow *window, gpointer user_data)
{
    (void)window;
    daterange_complete (user_data, FALSE);
    return TRUE;
}

static gboolean
daterange_escape_pressed (GtkWidget *widget, GVariant *args, gpointer user_data)
{
    (void)widget;
    (void)args;
    daterange_complete (user_data, FALSE);
    return TRUE;
}

static void
daterange_add_shortcuts (DaterangeInfo *info)
{
    GtkShortcutController *controller = GTK_SHORTCUT_CONTROLLER (
        gtk_shortcut_controller_new ());

    gtk_shortcut_controller_set_scope (controller, GTK_SHORTCUT_SCOPE_MANAGED);
    gtk_shortcut_controller_add_shortcut (
        controller,
        gtk_shortcut_new (
            gtk_keyval_trigger_new (GDK_KEY_Escape, 0),
            gtk_callback_action_new (daterange_escape_pressed, info, NULL)));
    gtk_widget_add_controller (info->dialog, GTK_EVENT_CONTROLLER (controller));
}

static void
daterange_dialog_destroyed (GtkWidget *widget, gpointer user_data)
{
    DaterangeInfo *info = user_data;

    if (info->dialog == widget)
        info->dialog = NULL;
    daterange_complete (info, FALSE);
}

static void
daterange_complete (DaterangeInfo *info, gboolean accepted)
{
    GTask *task;
    GncABDateRange *range = NULL;

    if (!info || !info->task)
        return;

    task = g_steal_pointer (&info->task);
    if (accepted)
    {
        range = g_new0 (GncABDateRange, 1);
        range->from_date = gnc_date_edit_get_date (
            GNC_DATE_EDIT (info->from_dateedit));
        range->last_retrieval_date = gtk_check_button_get_active (
            GTK_CHECK_BUTTON (info->last_retrieval_button));
        range->first_possible_date = gtk_check_button_get_active (
            GTK_CHECK_BUTTON (info->first_button));
        range->to_date = gnc_date_edit_get_date (
            GNC_DATE_EDIT (info->to_dateedit));
        range->to_now = gtk_check_button_get_active (
            GTK_CHECK_BUTTON (info->now_button));
    }

    if (info->dialog)
        gtk_window_destroy (GTK_WINDOW (info->dialog));
    g_clear_object (&info->builder);
    g_task_return_pointer (task, range, g_free);
    g_object_unref (task);
    g_free (info);
}

void
gnc_ab_enter_daterange_async (GtkWidget *parent, const char *heading,
                              const GncABDateRange *initial,
                              GCancellable *cancellable,
                              GAsyncReadyCallback callback, gpointer user_data)
{
    DaterangeInfo *info;
    GtkWidget *heading_label;

    g_return_if_fail (!parent || GTK_IS_WIDGET (parent));
    g_return_if_fail (initial);

    info = g_new0 (DaterangeInfo, 1);
    info->task = g_task_new (NULL, cancellable, callback, user_data);
    g_task_set_source_tag (info->task, gnc_ab_enter_daterange_async);
    info->builder = gtk_builder_new ();
    gnc_builder_add_from_file (info->builder, "dialog-ab.glade",
                               "aqbanking_date_range_dialog");
    info->dialog = GTK_WIDGET (gtk_builder_get_object (
        info->builder, "aqbanking_date_range_dialog"));
    if (!info->dialog)
    {
        g_task_return_new_error (info->task, G_IO_ERROR, G_IO_ERROR_FAILED,
                                 "Could not create AqBanking date range dialog");
        g_object_unref (info->task);
        g_clear_object (&info->builder);
        g_free (info);
        return;
    }

    gnc_builder_connect_signals_full (info->builder, gnc_builder_connect_full_func,
                                      info);
    g_signal_connect (gtk_builder_get_object (info->builder, "ok_button"),
                      "clicked", G_CALLBACK (daterange_accept_clicked), info);
    g_signal_connect (gtk_builder_get_object (info->builder, "cancel_button1"),
                      "clicked", G_CALLBACK (daterange_cancel_clicked), info);
    g_signal_connect (info->dialog, "close-request",
                      G_CALLBACK (daterange_close_requested), info);
    g_signal_connect (info->dialog, "destroy",
                      G_CALLBACK (daterange_dialog_destroyed), info);
    daterange_add_shortcuts (info);

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW (info->dialog),
                                      GTK_WINDOW (parent));

    heading_label = GTK_WIDGET (gtk_builder_get_object (info->builder,
                                                         "date_heading_label"));
    info->first_button = GTK_WIDGET (gtk_builder_get_object (info->builder,
                                                             "first_button"));
    info->last_retrieval_button = GTK_WIDGET (gtk_builder_get_object (
        info->builder, "last_retrieval_button"));
    info->enter_from_button = GTK_WIDGET (gtk_builder_get_object (info->builder,
                                                                   "enter_from_button"));
    info->now_button = GTK_WIDGET (gtk_builder_get_object (info->builder,
                                                           "now_button"));
    info->enter_to_button = GTK_WIDGET (gtk_builder_get_object (info->builder,
                                                                 "enter_to_button"));

    info->from_dateedit = gnc_date_edit_new (initial->from_date, FALSE, FALSE);
    gtk_box_prepend (GTK_BOX (gtk_builder_get_object (info->builder,
                                                      "enter_from_box")),
                     info->from_dateedit);
    gtk_widget_set_visible (info->from_dateedit, TRUE);

    info->to_dateedit = gnc_date_edit_new (initial->to_date, FALSE, FALSE);
    gtk_box_prepend (GTK_BOX (gtk_builder_get_object (info->builder,
                                                      "enter_to_box")),
                     info->to_dateedit);
    gtk_widget_set_visible (info->to_dateedit, TRUE);

    if (initial->last_retrieval_date)
        gtk_check_button_set_active (GTK_CHECK_BUTTON (
                                          info->last_retrieval_button), TRUE);
    else if (initial->first_possible_date)
    {
        gtk_check_button_set_active (GTK_CHECK_BUTTON (info->first_button), TRUE);
        gtk_widget_set_sensitive (info->last_retrieval_button, FALSE);
    }
    else
        gtk_check_button_set_active (GTK_CHECK_BUTTON (
                                          info->enter_from_button), TRUE);

    if (!initial->to_now)
        gtk_check_button_set_active (GTK_CHECK_BUTTON (info->enter_to_button),
                                      TRUE);

    gtk_widget_set_sensitive (info->from_dateedit,
                              !initial->last_retrieval_date
                              && !initial->first_possible_date);
    gtk_widget_set_sensitive (info->to_dateedit, !initial->to_now);
    gtk_window_set_default_widget (
        GTK_WINDOW (info->dialog),
        GTK_WIDGET (gtk_builder_get_object (info->builder, "ok_button")));

    if (heading)
        gtk_label_set_text (GTK_LABEL (heading_label), heading);
    gtk_window_present (GTK_WINDOW (info->dialog));
}

gboolean
gnc_ab_enter_daterange_finish (GAsyncResult *result, GncABDateRange *range,
                               GError **error)
{
    GncABDateRange *selection;

    g_return_val_if_fail (g_task_is_valid (result, NULL), FALSE);
    g_return_val_if_fail (range, FALSE);

    selection = g_task_propagate_pointer (G_TASK (result), error);
    if (!selection)
        return FALSE;

    *range = *selection;
    g_free (selection);
    return TRUE;
}

void
ddr_toggled_cb (GtkCheckButton *button, gpointer user_data)
{
    DaterangeInfo *info = user_data;

    (void)button;
    g_return_if_fail (info);

    gtk_widget_set_sensitive (info->from_dateedit,
                              gtk_check_button_get_active (
                                  GTK_CHECK_BUTTON (info->enter_from_button)));
    gtk_widget_set_sensitive (info->to_dateedit,
                              gtk_check_button_get_active (
                                  GTK_CHECK_BUTTON (info->enter_to_button)));
}
