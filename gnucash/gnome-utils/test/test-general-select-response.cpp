/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include <gtk/gtk.h>

#include "gnc-general-select.h"

namespace
{
gboolean display_available;
int old_selection;
int new_selection;
GtkWidget *destroy_from_get_string;

struct Selector
{
    guint calls = 0;
    gboolean complete_inline = FALSE;
    gpointer inline_selection = nullptr;
    GNCGeneralSelectAsyncResultCB completed = nullptr;
    gpointer completion_data = nullptr;
};

const char *
get_string (gpointer selection)
{
    if (destroy_from_get_string)
    {
        auto widget = destroy_from_get_string;
        destroy_from_get_string = nullptr;
        gtk_widget_destroy (widget);
    }
    if (selection == &old_selection)
        return "Old selection";
    if (selection == &new_selection)
        return "New selection";
    return "Unknown selection";
}

void
select_async ([[maybe_unused]] gpointer cb_arg, gpointer,
              [[maybe_unused]] GtkWidget *parent,
              GNCGeneralSelectAsyncResultCB completed, gpointer user_data)
{
    auto selector = static_cast<Selector *> (cb_arg);
    ++selector->calls;
    if (selector->complete_inline)
    {
        completed (selector->inline_selection, user_data);
        return;
    }
    selector->completed = completed;
    selector->completion_data = user_data;
}

void
complete_selection (Selector &selector, gpointer selection)
{
    auto completed = selector.completed;
    auto data = selector.completion_data;
    selector.completed = nullptr;
    selector.completion_data = nullptr;
    g_assert_nonnull (completed);
    completed (selection, data);
}

void
count_changed (GNCGeneralSelect *, gpointer data)
{
    ++*static_cast<guint *> (data);
}

GtkWidget *
create_select (GtkWidget *parent, Selector &selector)
{
    auto widget = gnc_general_select_new_async (
        GNC_GENERAL_SELECT_TYPE_SELECT, get_string, select_async, &selector);
    gtk_container_add (GTK_CONTAINER (parent), widget);
    return widget;
}

void
test_pending_cancel_update_and_inline_completion ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    Selector selector;
    auto widget = create_select (parent, selector);
    auto select = GNC_GENERAL_SELECT (widget);
    guint changed = 0;
    gnc_general_select_set_selected (select, &old_selection);
    g_assert_true (gnc_general_select_get_selected (select) == &old_selection);
    g_signal_connect (select, "changed", G_CALLBACK (count_changed), &changed);

    gtk_button_clicked (GTK_BUTTON (select->button));
    gtk_button_clicked (GTK_BUTTON (select->button));
    g_assert_cmpuint (selector.calls, ==, 1);
    complete_selection (selector, nullptr);
    g_assert_true (gnc_general_select_get_selected (select) == &old_selection);
    g_assert_cmpuint (changed, ==, 0);

    gtk_button_clicked (GTK_BUTTON (select->button));
    g_assert_cmpuint (selector.calls, ==, 2);
    complete_selection (selector, &new_selection);
    g_assert_true (gnc_general_select_get_selected (select) == &new_selection);
    g_assert_cmpstr (gtk_entry_get_text (GTK_ENTRY (select->entry)), ==,
                     "New selection");
    g_assert_cmpuint (changed, ==, 1);

    selector.complete_inline = TRUE;
    selector.inline_selection = &old_selection;
    gtk_button_clicked (GTK_BUTTON (select->button));
    g_assert_cmpuint (selector.calls, ==, 3);
    g_assert_true (gnc_general_select_get_selected (select) == &old_selection);
    g_assert_cmpuint (changed, ==, 2);
    gtk_widget_destroy (parent);
}

void
destroy_on_entry_changed (GtkEditable *, gpointer data)
{
    gtk_widget_destroy (GTK_WIDGET (data));
}

void
test_late_completion_after_destroy_and_entry_notification_destroy ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    Selector selector;
    auto widget = create_select (parent, selector);
    auto select = GNC_GENERAL_SELECT (widget);
    guint changed = 0;
    g_signal_connect (select, "changed", G_CALLBACK (count_changed), &changed);

    gtk_button_clicked (GTK_BUTTON (select->button));
    g_object_ref (widget);
    gtk_widget_destroy (parent);
    complete_selection (selector, &new_selection);
    g_assert_null (gnc_general_select_get_selected (select));
    g_assert_cmpuint (changed, ==, 0);
    g_object_unref (widget);

    parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    Selector second_selector;
    widget = create_select (parent, second_selector);
    select = GNC_GENERAL_SELECT (widget);
    g_signal_connect (select, "changed", G_CALLBACK (count_changed), &changed);
    g_signal_connect (select->entry, "changed",
                      G_CALLBACK (destroy_on_entry_changed), select);
    gtk_widget_show_all (parent);
    g_object_ref (widget);
    gnc_general_select_set_selected (select, &new_selection);
    g_assert_null (gnc_general_select_get_selected (select));
    g_assert_cmpuint (changed, ==, 0);
    g_object_unref (widget);
    gtk_widget_destroy (parent);
}

void
test_get_string_can_destroy_widget ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    Selector selector;
    auto widget = create_select (parent, selector);
    auto select = GNC_GENERAL_SELECT (widget);
    guint changed = 0;
    g_signal_connect (select, "changed", G_CALLBACK (count_changed), &changed);
    g_object_ref (widget);
    destroy_from_get_string = widget;
    gnc_general_select_set_selected (select, &new_selection);
    g_assert_null (gnc_general_select_get_selected (select));
    g_assert_cmpuint (changed, ==, 0);
    g_object_unref (widget);
    gtk_widget_destroy (parent);
}
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    g_test_add_func ("/gnome-utils/general-select/async-pending-cancel-update-inline",
                     test_pending_cancel_update_and_inline_completion);
    g_test_add_func ("/gnome-utils/general-select/async-late-completion-destroy",
                     test_late_completion_after_destroy_and_entry_notification_destroy);
    g_test_add_func ("/gnome-utils/general-select/get-string-destroys-widget",
                     test_get_string_can_destroy_widget);
    return g_test_run ();
}
