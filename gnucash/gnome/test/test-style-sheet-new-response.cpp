/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include <gtk/gtk.h>
#include "test/gnome-response-test-fixture.h"
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

GtkWidget *
find_style_sheet_options_window (GtkWindow *owner)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node && !found; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (widget), "gnc-id-options") == 0 &&
            gtk_window_get_transient_for (GTK_WINDOW (widget)) == owner)
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

int style_sheet_count ()
{
    return scm_to_int (scm_c_eval_string (
        "(length (gnc:get-html-style-sheets))"));
}

class StyleSheetCreationTest : public GnomeResponseTest
{
protected:
    void SetUp () override
    {
        GnomeResponseTest::SetUp ();
        session = qof_session_new (qof_book_new ());
        gnc_set_current_session (session);
        parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
        ASSERT_NE (parent, nullptr);
        gtk_widget_realize (GTK_WIDGET (parent));
        gnc_style_sheet_dialog_open (parent);
        owner = find_toplevel ("gnc-id-style-sheet-select");
        ASSERT_NE (owner, nullptr);
        add_button = find_named (owner, "add_button");
        ASSERT_TRUE (GTK_IS_BUTTON (add_button));
    }

    void TearDown () override
    {
        if (auto dialog = find_new_sheet_dialog ())
            gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);

        auto manager = find_toplevel ("gnc-id-style-sheet-select");
        while (manager)
        {
            g_object_ref (manager);
            auto editor = find_style_sheet_options_window (GTK_WINDOW (manager));
            while (editor)
            {
                g_object_ref (editor);
                auto cancel = find_named (editor, "cancel_button");
                EXPECT_TRUE (GTK_IS_BUTTON (cancel));
                if (!GTK_IS_BUTTON (cancel))
                {
                    g_object_unref (editor);
                    break;
                }
                gtk_button_clicked (GTK_BUTTON (cancel));
                while (g_main_context_iteration (nullptr, FALSE))
                    ;
                g_object_unref (editor);
                editor = find_style_sheet_options_window (GTK_WINDOW (manager));
            }

            if (!gtk_widget_in_destruction (manager))
            {
                auto close = find_named (manager, "close_button");
                EXPECT_TRUE (GTK_IS_BUTTON (close));
                if (GTK_IS_BUTTON (close))
                    gtk_button_clicked (GTK_BUTTON (close));
            }
            while (g_main_context_iteration (nullptr, FALSE))
                ;
            g_object_unref (manager);
            manager = find_toplevel ("gnc-id-style-sheet-select");
        }
        if (parent)
            gtk_widget_destroy (GTK_WIDGET (parent));
        GnomeResponseTest::TearDown ();
        gnc_clear_current_session ();
        session = nullptr;
    }

    QofSession *session{};
    GtkWindow *parent{};
    GtkWidget *owner{};
    GtkWidget *add_button{};
};

TEST_F (StyleSheetCreationTest, AcceptCreatesOneStyleSheet)
{
    const auto initial = style_sheet_count ();
    gtk_button_clicked (GTK_BUTTON (add_button));
    auto dialog = find_new_sheet_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    auto combo = GTK_COMBO_BOX (find_named (dialog, "template_combobox"));
    auto entry = GTK_ENTRY (find_named (dialog, "name_entry"));
    ASSERT_TRUE (GTK_IS_COMBO_BOX (combo));
    ASSERT_TRUE (GTK_IS_ENTRY (entry));
    gtk_entry_set_text (entry, "Async style sheet test");
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    EXPECT_EQ (style_sheet_count (), initial + 1);
    EXPECT_EQ (find_new_sheet_dialog (), nullptr);
}

TEST_F (StyleSheetCreationTest, ClosingOwnerWithResponseDoesNotCreateSheet)
{
    const auto initial = style_sheet_count ();
    gtk_button_clicked (GTK_BUTTON (add_button));
    auto dialog = find_new_sheet_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    auto entry = GTK_ENTRY (find_named (dialog, "name_entry"));
    ASSERT_TRUE (GTK_IS_ENTRY (entry));
    gtk_entry_set_text (entry, "Must not outlive owner");
    g_signal_connect (dialog, "destroy", G_CALLBACK (destroy_sheet_owner),
                      owner);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    EXPECT_EQ (style_sheet_count (), initial);
    EXPECT_EQ (find_toplevel ("gnc-id-style-sheet-select"), nullptr);
}

TEST_F (StyleSheetCreationTest, OwnerMayCloseAfterSchemeCreatesSheet)
{
    const auto initial = style_sheet_count ();
    auto list = GTK_TREE_VIEW (find_named (owner, "style_sheet_list_view"));
    ASSERT_TRUE (GTK_IS_TREE_VIEW (list));
    g_signal_connect (gtk_tree_view_get_model (list), "row-inserted",
                      G_CALLBACK (destroy_owner_on_insert), owner);
    gtk_button_clicked (GTK_BUTTON (add_button));
    auto dialog = find_new_sheet_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    auto entry = GTK_ENTRY (find_named (dialog, "name_entry"));
    ASSERT_TRUE (GTK_IS_ENTRY (entry));
    gtk_entry_set_text (entry, "Survives owner close after Scheme creation");
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    EXPECT_EQ (style_sheet_count (), initial + 1);
    EXPECT_EQ (find_toplevel ("gnc-id-style-sheet-select"), nullptr);
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
    g_log_set_always_fatal (static_cast<GLogLevelFlags> (
        G_LOG_FATAL_MASK | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL));
    auto result = RUN_ALL_TESTS ();
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
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
    {
        g_printerr ("GTK display initialization failed; GUI tests require a display.\n");
        return 1;
    }
    scm_boot_guile (argc, argv, run_tests, nullptr);
    return 0;
}
