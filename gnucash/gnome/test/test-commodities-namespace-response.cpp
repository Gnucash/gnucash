/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>
#include "test-logging.hpp"

#include <gtk/gtk.h>
#include "test/gnome-response-test-fixture.h"

#include "cashobjects.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gnc-tree-model-commodity.h"
#include "gnc-tree-view-commodity.h"
#include "qof.h"
#include "gnc-ui.h"


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
find_window_named (const gchar *name)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node && !found; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (widget), name) == 0)
            found = widget;
    }
    g_list_free (windows);
    return found;
}

static GtkWidget *
find_commodity_view (GtkWidget *widget)
{
    if (GNC_IS_TREE_VIEW_COMMODITY (widget))
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_commodity_view (GTK_WIDGET (node->data));
    g_list_free (children);
    return found;
}

static gboolean
select_namespace (GncTreeViewCommodity *view,
                  gnc_commodity_namespace *target)
{
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    auto selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (view));
    GtkTreeIter iter;
    if (!gtk_tree_model_get_iter_first (model, &iter))
        return false;
    do
    {
        auto path = gtk_tree_model_get_path (model, &iter);
        gtk_tree_selection_select_path (selection, path);
        gtk_tree_path_free (path);
        if (gnc_tree_view_commodity_get_selected_namespace (view) == target)
            return true;
    }
    while (gtk_tree_model_iter_next (model, &iter));
    return false;
}

class CommodityNamespaceResponseTest : public GnomeResponseTest
{
protected:
    void SetUp () override
    {
        GnomeResponseTest::SetUp ();
        book = qof_book_new ();
        session = qof_session_new (book);
        gnc_set_current_session (session);
        table = gnc_commodity_table_get_table (book);
        gnc_commodity_table_add_namespace (table, "OLDNS", book);
        gnc_commodity_table_add_namespace (table, "EXISTS", book);
        gnc_commodity_table_insert (
            table, gnc_commodity_new (book, "Old namespace test", "OLDNS", "OLD",
                                      nullptr, 100));
        gnc_commodity_table_insert (
            table, gnc_commodity_new (book, "Existing namespace test", "EXISTS",
                                      "EXS", nullptr, 100));
        owner = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        g_object_ref_sink (owner);
        gtk_widget_realize (owner);
        gnc_commodities_dialog (owner);
        commodities_window = find_window_named ("gnc-id-commodity");
        ASSERT_TRUE (GTK_IS_WINDOW (commodities_window));
        view = GNC_TREE_VIEW_COMMODITY (find_commodity_view (commodities_window));
        ASSERT_TRUE (GNC_IS_TREE_VIEW_COMMODITY (view));
        original_namespace = gnc_commodity_table_find_namespace (table, "OLDNS");
        ASSERT_TRUE (select_namespace (view, original_namespace));
        rename_button = find_buildable (commodities_window, "rename_namespace_button");
        ASSERT_TRUE (GTK_IS_BUTTON (rename_button));
    }

    void TearDown () override
    {
        if (commodities_window)
        {
            gtk_widget_destroy (commodities_window);
            commodities_window = nullptr;
        }
        if (owner)
        {
            gtk_widget_destroy (owner);
            g_object_unref (owner);
            owner = nullptr;
        }
        GnomeResponseTest::TearDown ();
        gnc_clear_current_session ();
        session = nullptr;
    }

    QofBook *book{};
    QofSession *session{};
    gnc_commodity_table *table{};
    gnc_commodity_namespace *original_namespace{};
    GtkWidget *owner{};
    GtkWidget *commodities_window{};
    GncTreeViewCommodity *view{};
    GtkWidget *rename_button{};

    GtkWidget *open_rename_dialog ()
    {
        gtk_button_clicked (GTK_BUTTON (rename_button));
        return find_window_named ("gnc-id-rename-namespace");
    }
};

TEST_F (CommodityNamespaceResponseTest, EmptyNameKeepsDialogOpen)
{
    auto dialog = open_rename_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    auto entry = GTK_ENTRY (find_buildable (dialog, "rename_entry"));
    auto label = GTK_LABEL (find_buildable (dialog, "rename_label"));
    ASSERT_TRUE (GTK_IS_ENTRY (entry));
    ASSERT_TRUE (GTK_IS_LABEL (label));
    gtk_entry_set_text (entry, "");
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_TRUE (GTK_IS_DIALOG (find_window_named ("gnc-id-rename-namespace")));
    EXPECT_STREQ (gtk_label_get_text (label), "No new name");
}

TEST_F (CommodityNamespaceResponseTest, ExistingNameKeepsDialogOpen)
{
    auto dialog = open_rename_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    gtk_entry_set_text (GTK_ENTRY (find_buildable (dialog, "rename_entry")),
                        "EXISTS");
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_TRUE (GTK_IS_DIALOG (find_window_named ("gnc-id-rename-namespace")));
    EXPECT_EQ (gnc_commodity_table_find_namespace (table, "OLDNS"),
               original_namespace);
}

TEST_F (CommodityNamespaceResponseTest, ValidNameRenamesNamespace)
{
    auto dialog = open_rename_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    gtk_entry_set_text (GTK_ENTRY (find_buildable (dialog, "rename_entry")),
                        "NEWNS");
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_EQ (gnc_commodity_table_find_namespace (table, "NEWNS"),
               original_namespace);
    EXPECT_EQ (gnc_commodity_table_find_namespace (table, "OLDNS"), nullptr);
    EXPECT_EQ (find_window_named ("gnc-id-rename-namespace"), nullptr);
}

TEST_F (CommodityNamespaceResponseTest, ParentDestructionClosesPendingDialog)
{
    auto dialog = open_rename_dialog ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    g_object_ref (dialog);
    gtk_widget_destroy (commodities_window);
    commodities_window = nullptr;
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_EQ (find_window_named ("gnc-id-rename-namespace"), nullptr);
    EXPECT_EQ (gnc_commodity_table_find_namespace (table, "OLDNS"),
               original_namespace);
    g_object_unref (dialog);
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
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    gnc::test::initialize_logging ();
    auto result = RUN_ALL_TESTS ();
    gnc_gsettings_shutdown ();
    gnc_component_manager_shutdown ();
    gnc_clear_current_session ();
    qof_close ();
    return result;
}
