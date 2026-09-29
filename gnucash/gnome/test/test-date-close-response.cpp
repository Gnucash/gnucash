/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>

extern "C"
{
#include "dialog-date-close.h"
#include "gnc-date-edit.h"
}

namespace
{
struct Result
{
    guint calls{0};
    gboolean accepted{FALSE};
    time64 date{0};
};
gboolean display_available;

GtkWidget *
find_buildable (GtkWidget *root, const char *name)
{
    if (GTK_IS_BUILDABLE (root) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (root)), name) == 0)
        return root;
    if (!GTK_IS_CONTAINER (root))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *result = nullptr;
    for (auto node = children; node && !result; node = node->next)
        result = find_buildable (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return result;
}

GtkWidget *
find_date_dialog ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node && !result; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (widget), "gnc-id-date-close") == 0)
            result = widget;
    }
    g_list_free (windows);
    return result;
}

GNCDateEdit *
find_date_edit (GtkWidget *root)
{
    if (GNC_IS_DATE_EDIT (root))
        return GNC_DATE_EDIT (root);
    if (!GTK_IS_CONTAINER (root))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GNCDateEdit *result = nullptr;
    for (auto node = children; node && !result; node = node->next)
        result = find_date_edit (GTK_WIDGET (node->data));
    g_list_free (children);
    return result;
}

void
completed (gboolean accepted, time64 date, gpointer data)
{
    auto result = static_cast<Result *> (data);
    ++result->calls;
    result->accepted = accepted;
    result->date = date;
}

void
test_cancel_response ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    Result result;
    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gnc_dialog_date_close_async_parented (
        parent, "Close?", "Date", TRUE, 1234, completed, &result);
    auto dialog = find_date_dialog ();
    g_assert_nonnull (dialog);
    gtk_button_clicked (GTK_BUTTON (find_buildable (dialog, "cancelbutton")));
    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_false (result.accepted);
    gtk_widget_destroy (parent);
}

void
test_ok_response ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    Result result;
    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gnc_dialog_date_close_async_parented (
        parent, "Close?", "Date", TRUE, 1234, completed, &result);
    auto dialog = find_date_dialog ();
    g_assert_nonnull (dialog);
    auto date_edit = find_date_edit (dialog);
    g_assert_nonnull (date_edit);
    auto expected_date = gnc_date_edit_get_date (date_edit);
    gtk_button_clicked (GTK_BUTTON (find_buildable (dialog, "okbutton")));
    g_assert_true (result.accepted);
    g_assert_cmpint (result.date, ==, expected_date);
    g_assert_cmpuint (result.calls, ==, 1);
    gtk_widget_destroy (parent);
}

void
test_parent_destroy_cancels ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    Result result;
    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gnc_dialog_date_close_async_parented (
        parent, "Close?", "Date", TRUE, 1234, completed, &result);
    auto dialog = find_date_dialog ();
    g_assert_nonnull (dialog);
    g_object_ref (dialog);
    gtk_widget_destroy (parent);
    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_false (result.accepted);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_cmpuint (result.calls, ==, 1);
    g_object_unref (dialog);
}

void
destroy_parent_during_completion (GtkWidget *, gpointer parent)
{
    gtk_widget_destroy (GTK_WIDGET(parent));
}

void
test_parent_destroy_during_acceptance ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    Result result;
    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    g_object_ref_sink (parent);
    gnc_dialog_date_close_async_parented (
        parent, "Close?", "Date", TRUE, 1234, completed, &result);
    auto dialog = find_date_dialog ();
    g_assert_nonnull (dialog);
    g_signal_connect (dialog, "destroy",
                      G_CALLBACK(destroy_parent_during_completion), parent);
    gtk_button_clicked (GTK_BUTTON(find_buildable (dialog, "okbutton")));
    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_false (result.accepted);
    g_object_unref (parent);
}
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    g_test_add_func ("/gnome/date-close/cancel", test_cancel_response);
    g_test_add_func ("/gnome/date-close/ok", test_ok_response);
    g_test_add_func ("/gnome/date-close/parent-destroy", test_parent_destroy_cancels);
    g_test_add_func ("/gnome/date-close/parent-destroy-during-acceptance",
                     test_parent_destroy_during_acceptance);
    return g_test_run ();
}
