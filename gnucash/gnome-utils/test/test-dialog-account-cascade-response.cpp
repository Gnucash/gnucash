/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "Account.h"
#include "cashobjects.h"
#include "dialog-account.h"
#include "qof.h"
#include "qofbook.h"
#include "qofevent.h"

static gboolean display_available;

struct AccountTree
{
    QofBook *book;
    Account *book_root;
    Account *account;
    Account *child;
    Account *grandchild;
};

static AccountTree
make_account_tree ()
{
    AccountTree tree{};
    tree.book = qof_book_new ();
    tree.book_root = gnc_account_create_root (tree.book);
    tree.account = xaccMallocAccount (tree.book);
    tree.child = xaccMallocAccount (tree.book);
    tree.grandchild = xaccMallocAccount (tree.book);
    xaccAccountSetName (tree.account, "Cascade target");
    xaccAccountSetName (tree.child, "Cascade child");
    xaccAccountSetName (tree.grandchild, "Cascade grandchild");
    gnc_account_append_child (tree.book_root, tree.account);
    gnc_account_append_child (tree.account, tree.child);
    gnc_account_append_child (tree.child, tree.grandchild);
    return tree;
}

static GtkWidget *
find_buildable (GtkWidget *widget, const gchar *name)
{
    if (GTK_IS_BUILDABLE (widget) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (widget)), name) == 0)
        return widget;

    if (!GTK_IS_CONTAINER (widget))
        return nullptr;

    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_buildable (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return found;
}

static GtkWidget *
find_cascade_dialog ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *dialog = nullptr;
    for (auto node = windows; node && !dialog; node = node->next)
        dialog = find_buildable (GTK_WIDGET (node->data),
                                 "account_cascade_dialog");
    g_list_free (windows);
    return dialog;
}

static GtkWidget *
start_cascade_dialog (AccountTree &tree, GtkWidget **parent)
{
    *parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gnc_account_cascade_properties_dialog (*parent, tree.account);
    auto dialog = find_cascade_dialog ();
    g_assert_true (GTK_IS_DIALOG (dialog));
    return dialog;
}

static GtkWidget *
control (GtkWidget *dialog, const gchar *name)
{
    auto widget = find_buildable (dialog, name);
    g_assert_nonnull (widget);
    return widget;
}

static void
select_all_updates (GtkWidget *dialog, gboolean replace)
{
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "enable_cascade_color")), TRUE);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "replace_check")), replace);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "enable_cascade_placeholder")), TRUE);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "placeholder_check_button")), TRUE);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "enable_cascade_hidden")), TRUE);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "hidden_check_button")), TRUE);

    GdkRGBA color{0.2, 0.4, 0.6, 1.0};
    gtk_color_chooser_set_rgba (
        GTK_COLOR_CHOOSER (control (dialog, "color_button")), &color);
}

static void
test_apply_and_late_response ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto tree = make_account_tree ();
    xaccAccountSetColor (tree.account, "red");
    xaccAccountSetColor (tree.child, "blue");
    GtkWidget *parent;
    auto dialog = start_cascade_dialog (tree, &parent);
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    select_all_updates (dialog, TRUE);
    g_object_ref (dialog);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    GdkRGBA color{0.2, 0.4, 0.6, 1.0};
    auto expected_color = gdk_rgba_to_string (&color);
    g_assert_cmpstr (xaccAccountGetColor (tree.account), ==, expected_color);
    g_assert_cmpstr (xaccAccountGetColor (tree.child), ==, expected_color);
    g_assert_cmpstr (xaccAccountGetColor (tree.grandchild), ==, expected_color);
    g_assert_true (xaccAccountGetPlaceholder (tree.account));
    g_assert_true (xaccAccountGetPlaceholder (tree.child));
    g_assert_true (xaccAccountGetHidden (tree.account));
    g_assert_true (xaccAccountGetHidden (tree.child));
    g_assert_true (xaccAccountGetHidden (tree.grandchild));

    xaccAccountSetHidden (tree.child, FALSE);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_false (xaccAccountGetHidden (tree.child));

    g_free (expected_color);
    g_object_unref (dialog);
    gtk_widget_destroy (parent);
    qof_book_destroy (tree.book);
}

static void
test_replace_disabled ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto tree = make_account_tree ();
    xaccAccountSetColor (tree.account, "red");
    xaccAccountSetColor (tree.child, "blue");
    GtkWidget *parent;
    auto dialog = start_cascade_dialog (tree, &parent);
    select_all_updates (dialog, FALSE);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "enable_cascade_placeholder")), FALSE);
    gtk_toggle_button_set_active (
        GTK_TOGGLE_BUTTON (control (dialog, "enable_cascade_hidden")), FALSE);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    g_assert_cmpstr (xaccAccountGetColor (tree.account), ==, "red");
    g_assert_cmpstr (xaccAccountGetColor (tree.child), ==, "blue");
    GdkRGBA color{0.2, 0.4, 0.6, 1.0};
    auto expected_color = gdk_rgba_to_string (&color);
    g_assert_cmpstr (xaccAccountGetColor (tree.grandchild), ==, expected_color);
    g_free (expected_color);
    gtk_widget_destroy (parent);
    qof_book_destroy (tree.book);
}

static void
test_cancel_and_parent_destroy ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto tree = make_account_tree ();
    GtkWidget *parent;
    auto dialog = start_cascade_dialog (tree, &parent);
    select_all_updates (dialog, TRUE);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);
    g_assert_null (xaccAccountGetColor (tree.account));
    g_assert_false (xaccAccountGetPlaceholder (tree.child));
    g_assert_false (xaccAccountGetHidden (tree.child));
    gtk_widget_destroy (parent);

    dialog = start_cascade_dialog (tree, &parent);
    select_all_updates (dialog, TRUE);
    gtk_widget_destroy (parent);
    g_assert_null (find_cascade_dialog ());
    g_assert_null (xaccAccountGetColor (tree.account));
    g_assert_false (xaccAccountGetPlaceholder (tree.child));
    g_assert_false (xaccAccountGetHidden (tree.child));
    qof_book_destroy (tree.book);
}

static void
test_dialog_destroy_and_late_response ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto tree = make_account_tree ();
    GtkWidget *parent;
    auto dialog = start_cascade_dialog (tree, &parent);
    select_all_updates (dialog, TRUE);
    g_object_ref (dialog);
    GtkWidget *weak_dialog = dialog;
    g_object_add_weak_pointer (G_OBJECT (dialog),
                               reinterpret_cast<gpointer *> (&weak_dialog));

    gtk_widget_destroy (dialog);
    g_assert_null (xaccAccountGetColor (tree.account));
    g_assert_false (xaccAccountGetPlaceholder (tree.child));
    g_assert_false (xaccAccountGetHidden (tree.child));
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_null (xaccAccountGetColor (tree.account));
    g_assert_false (xaccAccountGetPlaceholder (tree.child));
    g_assert_false (xaccAccountGetHidden (tree.child));

    g_object_unref (dialog);
    g_assert_null (weak_dialog);
    gtk_widget_destroy (parent);
    qof_book_destroy (tree.book);
}

static void
test_readonly_and_closed_book ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto tree = make_account_tree ();
    GtkWidget *parent;
    auto dialog = start_cascade_dialog (tree, &parent);
    select_all_updates (dialog, TRUE);
    qof_book_mark_readonly (tree.book);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_null (xaccAccountGetColor (tree.account));
    g_assert_false (xaccAccountGetHidden (tree.child));
    gtk_widget_destroy (parent);
    qof_book_destroy (tree.book);

    tree = make_account_tree ();
    dialog = start_cascade_dialog (tree, &parent);
    select_all_updates (dialog, TRUE);
    qof_book_mark_closed (tree.book);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_null (xaccAccountGetColor (tree.account));
    g_assert_false (xaccAccountGetPlaceholder (tree.child));
    gtk_widget_destroy (parent);
    qof_book_destroy (tree.book);
}

static void
test_removed_account_and_destroyed_book ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto tree = make_account_tree ();
    GtkWidget *parent;
    auto dialog = start_cascade_dialog (tree, &parent);
    select_all_updates (dialog, TRUE);
    xaccAccountBeginEdit (tree.account);
    xaccAccountDestroy (tree.account);
    tree.account = nullptr;
    tree.child = nullptr;
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    gtk_widget_destroy (parent);
    qof_book_destroy (tree.book);

    tree = make_account_tree ();
    dialog = start_cascade_dialog (tree, &parent);
    select_all_updates (dialog, TRUE);
    qof_book_destroy (tree.book);
    tree.book = nullptr;
    tree.account = nullptr;
    tree.child = nullptr;
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    gtk_widget_destroy (parent);
}

struct RemoveChildOnModify
{
    Account *target;
    Account *child;
    gboolean removed;
};

static void
remove_child_on_modify (QofInstance *entity, QofEventId event,
                        gpointer user_data, gpointer event_data)
{
    auto removal = static_cast<RemoveChildOnModify *> (user_data);
    if (!removal->removed && event == QOF_EVENT_MODIFY &&
        entity == QOF_INSTANCE (removal->target))
    {
        removal->removed = TRUE;
        xaccAccountBeginEdit (removal->child);
        xaccAccountDestroy (removal->child);
        removal->child = nullptr;
    }
}

static void
test_reentrant_account_event ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto tree = make_account_tree ();
    RemoveChildOnModify removal{tree.account, tree.child, FALSE};
    auto handler = qof_event_register_handler (remove_child_on_modify, &removal);
    GtkWidget *parent;
    auto dialog = start_cascade_dialog (tree, &parent);
    select_all_updates (dialog, TRUE);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    qof_event_unregister_handler (handler);

    g_assert_true (removal.removed);
    g_assert_true (xaccAccountGetHidden (tree.account));
    gtk_widget_destroy (parent);
    qof_book_destroy (tree.book);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    g_test_add_func ("/gnome-utils/account-cascade/apply-and-late-response",
                     test_apply_and_late_response);
    g_test_add_func ("/gnome-utils/account-cascade/replace-disabled",
                     test_replace_disabled);
    g_test_add_func ("/gnome-utils/account-cascade/cancel-and-parent-destroy",
                     test_cancel_and_parent_destroy);
    g_test_add_func ("/gnome-utils/account-cascade/dialog-destroy-and-late-response",
                     test_dialog_destroy_and_late_response);
    g_test_add_func ("/gnome-utils/account-cascade/readonly-and-closed-book",
                     test_readonly_and_closed_book);
    g_test_add_func ("/gnome-utils/account-cascade/removed-account-and-destroyed-book",
                     test_removed_account_and_destroyed_book);
    g_test_add_func ("/gnome-utils/account-cascade/reentrant-account-event",
                     test_reentrant_account_event);
    auto result = g_test_run ();
    qof_close ();
    return result;
}
