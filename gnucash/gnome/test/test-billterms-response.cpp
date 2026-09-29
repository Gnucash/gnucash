/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include <gtk/gtk.h>

#include "cashobjects.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gncBillTerm.h"
#include "qofevent.h"
#include "qofinstance.h"
#include "qof.h"
extern "C"
{
#include "dialog-billterms.h"
}

namespace
{
gboolean display_available;
QofSession *test_session;
GtkWidget *parent_to_destroy;
GtkWidget *dialog_to_reenter;
GncGUID watched_term_guid;
gboolean parent_destroyed_by_modify;
gboolean response_reentered;

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
find_named_toplevel (const char *name, GtkWindow *parent = nullptr)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (widget), name) == 0 &&
            (!parent || gtk_window_get_transient_for (GTK_WINDOW(widget)) == parent))
        {
            g_assert_null (result);
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

void
destroy_parent_on_modify (QofInstance *entity, QofEventId event_type,
                          gpointer, gpointer)
{
    if (!(event_type & QOF_EVENT_MODIFY) ||
        !guid_equal (qof_instance_get_guid (entity), &watched_term_guid) ||
        !parent_to_destroy)
        return;
    auto parent = parent_to_destroy;
    parent_to_destroy = nullptr;
    parent_destroyed_by_modify = TRUE;
    if (dialog_to_reenter)
    {
        auto dialog = dialog_to_reenter;
        dialog_to_reenter = nullptr;
        response_reentered = TRUE;
        gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    }
    gtk_widget_destroy (parent);
}

void
test_delete_response ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    for (int scenario = 0; scenario != 4; ++scenario)
    {
        auto book = qof_book_new ();
        test_session = qof_session_new (book);
        gnc_set_current_session (test_session);
        auto term = gncBillTermCreate (book);
        gncBillTermSetType (term, GNC_TERM_TYPE_DAYS);
        gncBillTermSetName (term, "Confirmed term");
        auto guid = *gncBillTermGetGUID (term);
        auto owner = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        gtk_widget_realize (owner);
        g_assert_nonnull (gnc_ui_billterms_window_new (GTK_WINDOW (owner), book));
        auto parent = find_named_toplevel ("gnc-id-bill-terms");
        auto button = find_buildable (parent, "delete_term_button");
        g_assert_true (GTK_IS_BUTTON (button));
        gtk_button_clicked (GTK_BUTTON (button));
        GtkWidget *question = nullptr;
        auto windows = gtk_window_list_toplevels ();
        for (auto node = windows; node; node = node->next)
            if (GTK_IS_MESSAGE_DIALOG (node->data) &&
                gtk_window_get_transient_for (GTK_WINDOW (node->data)) ==
                    GTK_WINDOW (parent))
                question = GTK_WIDGET (node->data);
        g_list_free (windows);
        g_assert_nonnull (question);
        g_assert_nonnull (gncBillTermLookup (book, &guid));
        if (scenario == 2)
            gncBillTermIncRef (term);
        if (scenario == 3)
        {
            g_object_ref (question);
            gtk_widget_destroy (parent);
        }
        gtk_dialog_response (GTK_DIALOG (question), scenario == 0 ?
                              GTK_RESPONSE_NO : GTK_RESPONSE_YES);
        if (scenario == 3)
            g_object_unref (question);
        term = gncBillTermLookup (book, &guid);
        if (scenario == 1)
            g_assert_null (term);
        else
            g_assert_nonnull (term);
        if (scenario == 2)
            gncBillTermDecRef (term);
        if (scenario != 3)
            gtk_widget_destroy (parent);
        gtk_widget_destroy (owner);
        gnc_clear_current_session ();
        test_session = nullptr;
    }
}

void
test_new_edit_response_and_parent_destroy ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto book = qof_book_new ();
    test_session = qof_session_new (book);
    gnc_set_current_session (test_session);
    auto owner = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_realize (owner);
    g_assert_nonnull (gnc_ui_billterms_window_new (GTK_WINDOW (owner), book));
    auto parent = find_named_toplevel ("gnc-id-bill-terms");
    g_assert_nonnull (parent);

    auto new_button = find_buildable (parent, "new_term_button");
    g_assert_true (GTK_IS_BUTTON (new_button));
    gtk_button_clicked (GTK_BUTTON (new_button));
    auto dialog = find_named_toplevel ("gnc-id-new-bill-terms",
                                      GTK_WINDOW (parent));
    g_assert_true (GTK_IS_DIALOG (dialog));
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    auto name = find_buildable (dialog, "name_entry");
    g_assert_true (GTK_IS_ENTRY (name));
    gtk_entry_set_text (GTK_ENTRY (name), "Response-created term");
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    auto term = gncBillTermLookupByName (book, "Response-created term");
    g_assert_nonnull (term);
    auto term_guid = *gncBillTermGetGUID (term);
    g_assert_null (find_named_toplevel ("gnc-id-new-bill-terms",
                                       GTK_WINDOW (parent)));

    auto view = find_buildable (parent, "terms_view");
    g_assert_true (GTK_IS_TREE_VIEW (view));
    GtkTreeIter iter;
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    g_assert_true (gtk_tree_model_get_iter_first (model, &iter));
    auto path = gtk_tree_model_get_path (model, &iter);
    gtk_tree_selection_select_path (
        gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), path);
    gtk_tree_path_free (path);
    auto edit_button = find_buildable (parent, "edit_term_button");
    g_assert_true (GTK_IS_BUTTON (edit_button));
    gtk_button_clicked (GTK_BUTTON (edit_button));
    dialog = find_named_toplevel ("gnc-id-new-bill-terms",
                                 GTK_WINDOW (parent));
    g_assert_true (GTK_IS_DIALOG (dialog));
    auto description = find_buildable (dialog, "entry_desc");
    g_assert_true (GTK_IS_ENTRY (description));
    gtk_entry_set_text (GTK_ENTRY (description), "Must not survive parent close");

    /* Retain the destroyed widget so a late response also proves its handler
       was disconnected before the response context was released. */
    g_object_ref (dialog);
    gtk_widget_destroy (parent);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_object_unref (dialog);
    term = gncBillTermLookup (book, &term_guid);
    g_assert_nonnull (term);
    g_assert_cmpstr (gncBillTermGetDescription (term), ==, "");
    g_assert_null (find_named_toplevel ("gnc-id-new-bill-terms"));

    /* The outer QOF edit defers MODIFY until commit, where an event handler
       can reenter the response and destroy the parent before it returns. */
    g_assert_nonnull (gnc_ui_billterms_window_new (GTK_WINDOW (owner), book));
    parent = find_named_toplevel ("gnc-id-bill-terms");
    g_assert_nonnull (parent);
    view = find_buildable (parent, "terms_view");
    model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    g_assert_true (gtk_tree_model_get_iter_first (model, &iter));
    path = gtk_tree_model_get_path (model, &iter);
    gtk_tree_selection_select_path (
        gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), path);
    gtk_tree_path_free (path);
    edit_button = find_buildable (parent, "edit_term_button");
    gtk_button_clicked (GTK_BUTTON (edit_button));
    dialog = find_named_toplevel ("gnc-id-new-bill-terms",
                                 GTK_WINDOW (parent));
    g_assert_true (GTK_IS_DIALOG (dialog));
    description = find_buildable (dialog, "entry_desc");
    gtk_entry_set_text (GTK_ENTRY (description), "Committed before parent close");
    auto due_days = find_buildable (dialog, "days:due_days");
    auto discount_days = find_buildable (dialog, "days:discount_days");
    g_assert_true (GTK_IS_SPIN_BUTTON (due_days));
    g_assert_true (GTK_IS_SPIN_BUTTON (discount_days));
    gtk_spin_button_set_value (GTK_SPIN_BUTTON (due_days), 20);
    gtk_spin_button_set_value (GTK_SPIN_BUTTON (discount_days), 3);
    watched_term_guid = term_guid;
    parent_to_destroy = parent;
    dialog_to_reenter = dialog;
    parent_destroyed_by_modify = FALSE;
    response_reentered = FALSE;
    auto event_handler = qof_event_register_handler (destroy_parent_on_modify,
                                                      nullptr);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    qof_event_unregister_handler (event_handler);
    g_assert_true (parent_destroyed_by_modify);
    g_assert_true (response_reentered);
    g_assert_null (dialog_to_reenter);
    g_assert_null (find_named_toplevel ("gnc-id-bill-terms"));
    term = gncBillTermLookup (book, &term_guid);
    g_assert_nonnull (term);
    g_assert_cmpstr (gncBillTermGetDescription (term), ==,
                     "Committed before parent close");
    g_assert_cmpint (gncBillTermGetDueDays (term), ==, 20);
    g_assert_cmpint (gncBillTermGetDiscountDays (term), ==, 3);
    g_assert_cmpint (qof_instance_get_editlevel (term), ==, 0);

    gtk_widget_destroy (owner);
    gnc_clear_current_session ();
    test_session = nullptr;
}
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
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    g_test_add_func ("/gnome/billterms/new-edit-response-parent-destroy",
                     test_new_edit_response_and_parent_destroy);
    g_test_add_func ("/gnome/billterms/delete-response", test_delete_response);
    auto result = g_test_run ();
    gnc_gsettings_shutdown ();
    gnc_component_manager_shutdown ();
    gnc_clear_current_session ();
    qof_close ();
    return result;
}
