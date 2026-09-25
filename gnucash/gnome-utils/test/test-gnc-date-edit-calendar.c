/*
 * test-gnc-date-edit-calendar.c -- GtkCalendar signal regression tests
 *
 * Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-date-edit.h"

static GtkEventController *
find_controller (GtkWidget *widget, const char *name)
{
    GListModel *controllers = gtk_widget_observe_controllers (widget);
    GtkEventController *found = NULL;

    for (guint index = 0;
         index < g_list_model_get_n_items (controllers) && !found;
         index++)
    {
        GtkEventController *controller =
            g_list_model_get_item (controllers, index);

        if (g_strcmp0 (gtk_event_controller_get_name (controller), name) == 0)
            found = controller;
        else
            g_object_unref (controller);
    }

    g_object_unref (controllers);
    return found;
}

static void
mark_finalized (gpointer data, GObject *where_the_object_was)
{
    (void)where_the_object_was;
    *(gboolean *)data = TRUE;
}

static void
test_calendar_selection_and_keys (void)
{
    GNCDateEdit *date_edit = GNC_DATE_EDIT
        (gnc_date_edit_new (0, FALSE, FALSE));
    GtkWidget *popup;
    GtkEventController *key_controller;
    GDate selected_date;
    gboolean handled = FALSE;
    gboolean date_edit_finalized = FALSE;
    gboolean popup_finalized = FALSE;
    guint closed_signal;
    guint key_pressed_signal;

    g_object_ref_sink (date_edit);
    popup = g_object_ref (date_edit->cal_popup);
    key_controller = find_controller (popup, "gnc-date-edit-popup-key");
    g_assert_nonnull (key_controller);
    g_object_weak_ref (G_OBJECT (date_edit), mark_finalized,
                       &date_edit_finalized);
    g_object_weak_ref (G_OBJECT (popup), mark_finalized, &popup_finalized);

    /* GtkCalendar has no GTK4 "activate" signal. Construction must not
     * attempt to connect it, and day-selected remains the update path. */
    g_assert_cmpuint (g_signal_lookup ("activate", GTK_TYPE_CALENDAR), ==, 0);

    gtk_calendar_set_day (GTK_CALENDAR (date_edit->calendar), 7);
    gtk_calendar_set_year (GTK_CALENDAR (date_edit->calendar), 2026);
    gtk_calendar_set_month (GTK_CALENDAR (date_edit->calendar), 8);
    g_signal_emit_by_name (date_edit->calendar, "day-selected");

    gnc_date_edit_get_gdate (date_edit, &selected_date);
    g_assert_cmpuint (g_date_get_day (&selected_date), ==, 7);
    g_assert_cmpuint (g_date_get_month (&selected_date), ==, 9);
    g_assert_cmpuint (g_date_get_year (&selected_date), ==, 2026);

    g_signal_emit_by_name (key_controller, "key-pressed", GDK_KEY_Return,
                           0, (GdkModifierType)0, &handled);
    g_assert_true (handled);

    handled = FALSE;
    g_signal_emit_by_name (key_controller, "key-pressed", GDK_KEY_Escape,
                           0, (GdkModifierType)0, &handled);
    g_assert_true (handled);

    closed_signal = g_signal_lookup ("closed", GTK_TYPE_POPOVER);
    key_pressed_signal = g_signal_lookup ("key-pressed",
                                          GTK_TYPE_EVENT_CONTROLLER_KEY);
    g_object_run_dispose (G_OBJECT (date_edit));
    g_object_run_dispose (G_OBJECT (date_edit));

    g_assert_null (date_edit->cal_popup);
    g_assert_null (date_edit->calendar);
    g_assert_null (date_edit->cal_label);
    g_assert_cmpuint (g_signal_handler_find
                      (popup, G_SIGNAL_MATCH_ID | G_SIGNAL_MATCH_DATA,
                       closed_signal, 0, NULL, NULL, date_edit), ==, 0);
    g_assert_cmpuint (g_signal_handler_find
                      (key_controller, G_SIGNAL_MATCH_ID | G_SIGNAL_MATCH_DATA,
                       key_pressed_signal, 0, NULL, NULL, date_edit), ==, 0);

    g_object_unref (date_edit);
    g_assert_true (date_edit_finalized);
    g_assert_false (popup_finalized);

    handled = TRUE;
    g_signal_emit_by_name (key_controller, "key-pressed", GDK_KEY_Escape,
                           0, (GdkModifierType)0, &handled);
    g_assert_false (handled);

    g_object_unref (popup);
    g_assert_true (popup_finalized);
    g_object_unref (key_controller);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    gtk_init ();

    g_test_add_func ("/gnome-utils/date-edit/calendar-selection-and-keys",
                     test_calendar_selection_and_keys);

    return g_test_run ();
}
