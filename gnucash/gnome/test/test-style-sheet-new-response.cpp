/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include <cstdlib>

#include "dialog-report-style-sheet.h"
#include "gnc-engine.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "qof.h"

namespace
{
gboolean display_available;

GtkWidget *
find_named (GtkWidget *widget, const char *name)
{
    if (g_strcmp0 (gtk_widget_get_name (widget), name) == 0)
        return widget;
    if (GTK_IS_BUILDABLE (widget) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (widget)), name) == 0)
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto child = children; child && !found; child = child->next)
        found = find_named (GTK_WIDGET (child->data), name);
    g_list_free (children);
    return found;
}

GtkWidget *
find_toplevel (const char *name)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node && !found; node = node->next)
        found = find_named (GTK_WIDGET (node->data), name);
    g_list_free (windows);
    return found;
}

GtkWidget *
find_new_sheet_dialog ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node && !found; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (GTK_IS_DIALOG (widget) &&
            g_strcmp0 (gtk_widget_get_name (widget),
                       "gnc-id-style-sheet-new") == 0)
            found = widget;
    }
    g_list_free (windows);
    return found;
}

void
destroy_sheet_owner (GtkWidget *, gpointer owner)
{
    gtk_widget_destroy (GTK_WIDGET (owner));
}

void
destroy_owner_on_insert (GtkTreeModel *, GtkTreePath *, GtkTreeIter *,
                         gpointer owner)
{
    gtk_widget_destroy (GTK_WIDGET (owner));
}

void
test_create_and_parent_destroy ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    gtk_widget_realize (GTK_WIDGET (parent));
    gnc_style_sheet_dialog_open (parent);
    auto owner = find_toplevel ("gnc-id-style-sheet-select");
    g_assert_nonnull (owner);
    auto add = find_named (owner, "add_button");
    g_assert_true (GTK_IS_BUTTON (add));
    auto initial = scm_to_int (scm_c_eval_string (
        "(length (gnc:get-html-style-sheets))"));

    gtk_button_clicked (GTK_BUTTON (add));
    auto dialog = find_new_sheet_dialog ();
    g_assert_true (GTK_IS_DIALOG (dialog));
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    auto combo = GTK_COMBO_BOX (find_named (dialog, "template_combobox"));
    auto entry = GTK_ENTRY (find_named (dialog, "name_entry"));
    g_assert_true (GTK_IS_COMBO_BOX (combo));
    g_assert_true (GTK_IS_ENTRY (entry));
    gtk_entry_set_text (entry, "Async style sheet test");
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    g_assert_cmpint (scm_to_int (scm_c_eval_string (
        "(length (gnc:get-html-style-sheets))")), ==, initial + 1);
    g_assert_null (find_new_sheet_dialog ());

    /* A pending completion must become a no-op when the owning window goes
       away while the response dialog is being closed. */
    gtk_button_clicked (GTK_BUTTON (add));
    dialog = find_new_sheet_dialog ();
    g_assert_true (GTK_IS_DIALOG (dialog));
    entry = GTK_ENTRY (find_named (dialog, "name_entry"));
    gtk_entry_set_text (entry, "Must not outlive owner");
    g_signal_connect (dialog, "destroy", G_CALLBACK (destroy_sheet_owner),
                      owner);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_cmpint (scm_to_int (scm_c_eval_string (
        "(length (gnc:get-html-style-sheets))")), ==, initial + 1);

    /* Reopen the manager, then destroy it from the model insertion emitted
       after Scheme has already created the globally registered stylesheet. */
    gnc_style_sheet_dialog_open (parent);
    owner = find_toplevel ("gnc-id-style-sheet-select");
    g_assert_nonnull (owner);
    add = find_named (owner, "add_button");
    auto list = GTK_TREE_VIEW (find_named (owner, "style_sheet_list_view"));
    g_assert_true (GTK_IS_TREE_VIEW (list));
    g_signal_connect (gtk_tree_view_get_model (list), "row-inserted",
                      G_CALLBACK (destroy_owner_on_insert), owner);
    gtk_button_clicked (GTK_BUTTON (add));
    dialog = find_new_sheet_dialog ();
    g_assert_true (GTK_IS_DIALOG (dialog));
    entry = GTK_ENTRY (find_named (dialog, "name_entry"));
    gtk_entry_set_text (entry, "Survives owner close after Scheme creation");
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_cmpint (scm_to_int (scm_c_eval_string (
        "(length (gnc:get-html-style-sheets))")), ==, initial + 2);
    g_assert_null (find_toplevel ("gnc-id-style-sheet-select"));
    gtk_widget_destroy (GTK_WIDGET (parent));
}

void
run_tests (void *, int, char **)
{
    qof_init ();
    gnc_engine_init (0, nullptr);
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    scm_c_use_module ("gnucash report");
    scm_c_use_module ("gnucash reports");
    scm_c_use_module ("gnucash report report-core");
    scm_c_eval_string (
        "(report-module-loader (list '(gnucash report stylesheets)))");
    g_test_add_func ("/gnome/style-sheet/new-response-parent-destroy",
                     test_create_and_parent_destroy);
    auto result = g_test_run ();
    gnc_clear_current_session ();
    gnc_gsettings_shutdown ();
    gnc_component_manager_shutdown ();
    qof_close ();
    exit (result);
}
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    scm_boot_guile (argc, argv, run_tests, nullptr);
    return 0;
}
