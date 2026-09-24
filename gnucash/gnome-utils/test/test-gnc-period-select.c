/*
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 */

/*
 * test-gnc-period-select.c -- Period selection semantics regression tests
 */

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-period-select.h"
#include "gnc-prefs-utils.h"

static GtkDropDown *
period_dropdown (GtkWidget *widget)
{
    if (GTK_IS_DROP_DOWN (widget))
        return GTK_DROP_DOWN (widget);

    for (GtkWidget *child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
    {
        GtkDropDown *dropdown = period_dropdown (child);

        if (dropdown)
            return dropdown;
    }
    return NULL;
}

static void
count_changed (GncPeriodSelect *period, guint *count)
{
    (*count)++;
    (void)period;
}

static void
count_active_notify (GncPeriodSelect *period, GParamSpec *pspec, guint *count)
{
    (*count)++;
    (void)period;
    (void)pspec;
}

static void
test_neutral_selection_preserves_period_indices (void)
{
    GtkWidget *widget = gnc_period_select_new (TRUE);
    GncPeriodSelect *period = GNC_PERIOD_SELECT (widget);
    GtkDropDown *dropdown;
    GDate fy_end;
    GDate *date;
    guint changed_count = 0;
    guint active_notify_count = 0;

    g_object_ref_sink (widget);
    dropdown = period_dropdown (widget);
    g_assert_nonnull (dropdown);
    g_assert_cmpuint (gtk_drop_down_get_selected (dropdown), ==, 0);
    g_assert_cmpint (gnc_period_select_get_active (period), ==, -1);
    g_assert_null (gnc_period_select_get_date (period));

    g_signal_connect (period, "changed", G_CALLBACK (count_changed),
                      &changed_count);
    g_signal_connect (period, "notify::active", G_CALLBACK (count_active_notify),
                      &active_notify_count);
    gtk_drop_down_set_selected (dropdown, GNC_ACCOUNTING_PERIOD_TODAY + 1);
    g_assert_cmpint (gnc_period_select_get_active (period), ==,
                     GNC_ACCOUNTING_PERIOD_TODAY);
    g_assert_cmpuint (changed_count, ==, 1);
    g_assert_cmpuint (active_notify_count, ==, 1);

    gtk_drop_down_set_selected (dropdown, 0);
    g_assert_cmpint (gnc_period_select_get_active (period), ==, -1);
    g_assert_null (gnc_period_select_get_date (period));
    g_assert_cmpuint (changed_count, ==, 2);
    g_assert_cmpuint (active_notify_count, ==, 2);

    gtk_drop_down_set_selected (dropdown, GNC_ACCOUNTING_PERIOD_TODAY + 1);
    g_assert_cmpint (gnc_period_select_get_active (period), ==,
                     GNC_ACCOUNTING_PERIOD_TODAY);
    g_assert_cmpuint (changed_count, ==, 3);
    g_assert_cmpuint (active_notify_count, ==, 3);

    gnc_period_select_set_active (period, GNC_ACCOUNTING_PERIOD_FYEAR);
    g_assert_cmpuint (gtk_drop_down_get_selected (dropdown), ==, 0);
    g_assert_cmpint (gnc_period_select_get_active (period), ==, -1);
    g_assert_cmpuint (changed_count, ==, 4);
    g_assert_cmpuint (active_notify_count, ==, 4);

    g_date_set_dmy (&fy_end, 31, G_DATE_DECEMBER, 2024);
    gnc_period_select_set_fy_end (period, &fy_end);
    gnc_period_select_set_active (period, GNC_ACCOUNTING_PERIOD_FYEAR);
    g_assert_cmpuint (gtk_drop_down_get_selected (dropdown), ==,
                      GNC_ACCOUNTING_PERIOD_FYEAR + 1);
    g_assert_cmpint (gnc_period_select_get_active (period), ==,
                     GNC_ACCOUNTING_PERIOD_FYEAR);
    date = gnc_period_select_get_date (period);
    g_assert_nonnull (date);
    g_date_free (date);
    g_assert_cmpuint (changed_count, ==, 5);
    g_assert_cmpuint (active_notify_count, ==, 5);

    gnc_period_select_set_fy_end (period, NULL);
    g_assert_cmpuint (gtk_drop_down_get_selected (dropdown), ==, 0);
    g_assert_cmpint (gnc_period_select_get_active (period), ==, -1);
    g_assert_null (gnc_period_select_get_date (period));
    g_assert_cmpuint (changed_count, ==, 5);
    g_assert_cmpuint (active_notify_count, ==, 5);

    g_object_unref (widget);
}

int
main (int argc, char **argv)
{
    int status;

    g_setenv ("GSETTINGS_BACKEND", "memory", TRUE);
    g_test_init (&argc, &argv, NULL);
    gtk_init ();
    gnc_prefs_init ();

    g_test_add_func ("/gnome-utils/period-select/neutral-selection",
                     test_neutral_selection_preserves_period_indices);
    status = g_test_run ();

    gnc_prefs_remove_registered ();
    return status;
}
