/********************************************************************
 * test-gnucash-date-picker.c -- GtkCalendar signal regression tests *
 *                                                                  *
 * Copyright (C) 2026 GnuCash Developers                            *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#include <config.h>

#include <gtk/gtk.h>

#include "gnucash-date-picker.h"

typedef struct
{
    guint selected;
    guint picked;
    guint cancelled;
} PickerSignals;

static GtkEventController *
find_controller (GtkWidget *widget, const char *name)
{
    GListModel *controllers = gtk_widget_observe_controllers (widget);
    GtkEventController *found = NULL;

    for (guint index = 0; index < g_list_model_get_n_items (controllers); index++)
    {
        GtkEventController *controller =
            g_list_model_get_item (controllers, index);

        if (g_strcmp0 (gtk_event_controller_get_name (controller), name) == 0)
        {
            found = controller;
            break;
        }
        else
            g_object_unref (controller);
    }

    g_object_unref (controllers);
    return found;
}

static GtkWidget *
find_widget_with_css_class (GtkWidget *widget, const char *css_class)
{
    if (gtk_widget_has_css_class (widget, css_class))
        return widget;

    for (GtkWidget *child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
    {
        GtkWidget *found = find_widget_with_css_class (child, css_class);

        if (found)
            return found;
    }

    return NULL;
}

static GtkWidget *
find_widget_of_type (GtkWidget *widget, GType type)
{
    if (G_TYPE_CHECK_INSTANCE_TYPE (widget, type))
        return widget;

    for (GtkWidget *child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
    {
        GtkWidget *found = find_widget_of_type (child, type);

        if (found)
            return found;
    }

    return NULL;
}

static GtkWidget *
find_selected_day_label (GtkWidget *widget)
{
    if (gtk_widget_has_css_class (widget, "day-number") &&
        (gtk_widget_get_state_flags (widget) & GTK_STATE_FLAG_SELECTED))
        return widget;

    for (GtkWidget *child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
    {
        GtkWidget *found = find_selected_day_label (child);

        if (found)
            return found;
    }

    return NULL;
}

static void
widget_center (GtkWidget *widget, GtkWidget *ancestor, double *x, double *y)
{
    graphene_rect_t bounds;

    g_assert_true (gtk_widget_compute_bounds (widget, ancestor, &bounds));
    g_assert_cmpfloat (bounds.size.width, >, 0.0);
    g_assert_cmpfloat (bounds.size.height, >, 0.0);
    *x = bounds.origin.x + bounds.size.width / 2.0;
    *y = bounds.origin.y + bounds.size.height / 2.0;
}

static void
date_selected (GNCDatePicker *picker, PickerSignals *signals)
{
    (void)picker;
    signals->selected++;
}

static void
date_picked (GNCDatePicker *picker, PickerSignals *signals)
{
    (void)picker;
    signals->picked++;
}

static void
cancelled (GNCDatePicker *picker, PickerSignals *signals)
{
    (void)picker;
    signals->cancelled++;
}

static void
test_calendar_selection_click_and_keys (void)
{
    GtkWindow *window = GTK_WINDOW (gtk_window_new ());
    GNCDatePicker *picker = GNC_DATE_PICKER (gnc_date_picker_new ());
    GtkEventController *click_controller;
    GtkEventController *key_controller;
    GtkWidget *day_label;
    GtkWidget *day_name;
    GtkWidget *week_number;
    GtkWidget *header_button;
    PickerSignals signals = { 0 };
    gboolean handled = FALSE;
    double x, y;

    g_object_ref_sink (window);
    g_object_ref_sink (picker);

    g_assert_cmpuint (g_signal_lookup ("activate", GTK_TYPE_CALENDAR), ==, 0);
    g_signal_connect (picker, "date_selected", G_CALLBACK (date_selected),
                      &signals);
    g_signal_connect (picker, "date_picked", G_CALLBACK (date_picked),
                      &signals);
    g_signal_connect (picker, "cancelled", G_CALLBACK (cancelled), &signals);

    gtk_calendar_set_show_week_numbers (picker->calendar, TRUE);
    gtk_window_set_default_size (window, 360, 280);
    gtk_window_set_child (window, GTK_WIDGET (picker));
    gtk_window_present (window);
    gtk_test_widget_wait_for_draw (GTK_WIDGET (picker));

    click_controller = find_controller (GTK_WIDGET (picker->calendar),
                                        "gnc-date-picker-calendar-double-click");
    key_controller = find_controller (GTK_WIDGET (picker->calendar),
                                      "gnc-date-picker-key");
    day_label = find_widget_with_css_class (GTK_WIDGET (picker->calendar),
                                            "day-number");
    day_name = find_widget_with_css_class (GTK_WIDGET (picker->calendar),
                                           "day-name");
    week_number = find_widget_with_css_class (GTK_WIDGET (picker->calendar),
                                              "week-number");
    header_button = find_widget_of_type (GTK_WIDGET (picker->calendar),
                                         GTK_TYPE_BUTTON);
    g_assert_nonnull (click_controller);
    g_assert_nonnull (key_controller);
    g_assert_nonnull (day_label);
    g_assert_nonnull (day_name);
    g_assert_nonnull (week_number);
    g_assert_nonnull (header_button);

    /* Non-day targets must never complete the picker. */
    widget_center (header_button, GTK_WIDGET (picker->calendar), &x, &y);
    g_signal_emit_by_name (click_controller, "released", 2, x, y);
    widget_center (day_name, GTK_WIDGET (picker->calendar), &x, &y);
    g_signal_emit_by_name (click_controller, "released", 2, x, y);
    widget_center (week_number, GTK_WIDGET (picker->calendar), &x, &y);
    g_signal_emit_by_name (click_controller, "released", 2, x, y);
    g_assert_cmpuint (signals.picked, ==, 0);

    /* Programmatic date changes may select a day but never complete it. */
    gtk_calendar_set_day (picker->calendar,
                          gtk_calendar_get_day (picker->calendar) == 1 ? 2 : 1);
    g_assert_cmpuint (signals.selected, ==, 1);
    g_assert_cmpuint (signals.picked, ==, 0);

    /* A double click on the already selected day has no day-selected signal
     * in GtkCalendar, but must still complete the picker. */
    day_label = find_selected_day_label (GTK_WIDGET (picker->calendar));
    g_assert_nonnull (day_label);
    widget_center (day_label, GTK_WIDGET (picker->calendar), &x, &y);
    g_signal_emit_by_name (click_controller, "released", 2, x, y);
    g_assert_cmpuint (signals.picked, ==, 1);

    g_signal_emit_by_name (key_controller, "key-pressed", GDK_KEY_Return,
                           0, (GdkModifierType)0, &handled);
    g_assert_true (handled);
    g_assert_cmpuint (signals.picked, ==, 2);

    handled = FALSE;
    g_signal_emit_by_name (key_controller, "key-pressed", GDK_KEY_Escape,
                           0, (GdkModifierType)0, &handled);
    g_assert_true (handled);
    g_assert_cmpuint (signals.cancelled, ==, 1);

    g_object_unref (key_controller);
    g_object_unref (click_controller);
    gtk_window_destroy (window);
    g_object_unref (picker);
    g_object_unref (window);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    gtk_init ();

    g_test_add_func ("/register/date-picker/calendar-selection-click-and-keys",
                     test_calendar_selection_click_and_keys);

    return g_test_run ();
}
