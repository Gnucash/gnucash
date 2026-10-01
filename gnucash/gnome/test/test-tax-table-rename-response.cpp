/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include <cstdint>
#include <gtk/gtk.h>
#include "test/gnome-response-test-fixture.h"

#include "cashobjects.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gncTaxTable.h"
#include "qof.h"
#include "qofevent.h"
extern "C"
{
#include "dialog-tax-table.h"
}

namespace
{
struct ModificationMonitor
{
    GncGUID table_guid{};
    std::uint32_t modifications{};
};

class TaxTableRenameResponseTest : public GnomeResponseTest
{
protected:
    void SetUp () override;
    void TearDown () override;

    QofBook *book{};
    QofSession *session{};
    GncGUID table_guid{};
    GtkWidget *owner{};
    GtkWidget *table_window{};
    std::int32_t event_handler{};
    ModificationMonitor monitor{};
};

static GtkWidget *
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

static GtkWidget *
find_tax_table_window ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (widget),
                       "gnc-id-new-tax-table") == 0)
        {
            if (result)
            {
                g_list_free (windows);
                return nullptr;
            }
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

static GtkWidget *
find_entry (GtkWidget *root);

static GtkWidget *
find_rename_dialog (GtkWidget *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (GTK_IS_DIALOG (widget) &&
            gtk_window_get_transient_for (GTK_WINDOW (widget)) ==
            GTK_WINDOW (parent) && find_entry (widget))
        {
            if (result)
            {
                g_list_free (windows);
                return nullptr;
            }
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

static GtkWidget *
find_entry (GtkWidget *root)
{
    if (GTK_IS_ENTRY (root))
        return root;
    if (!GTK_IS_CONTAINER (root))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *result = nullptr;
    for (auto node = children; node && !result; node = node->next)
        result = find_entry (GTK_WIDGET (node->data));
    g_list_free (children);
    return result;
}

static void
destroy_tax_table_parent (GtkWidget *, gpointer user_data)
{
    gtk_widget_destroy (GTK_WIDGET (user_data));
}

static void
destroy_current_session (GtkWidget *, gpointer user_data)
{
    auto session = static_cast<QofSession **> (user_data);
    if (*session)
        gnc_close_gui_component_by_session (*session);
    auto current = gnc_exchange_current_session (nullptr);
    if (current)
        qof_session_destroy (current);
    *session = nullptr;
}

static void
count_table_modification (QofInstance *entity, QofEventId event_type,
                          gpointer user_data,
                          [[maybe_unused]] gpointer event_data)
{
    auto monitor = static_cast<ModificationMonitor *> (user_data);
    if ((event_type & QOF_EVENT_MODIFY) &&
        guid_equal (qof_instance_get_guid (entity), &monitor->table_guid))
        ++monitor->modifications;
}

static bool
select_tax_table (GtkWidget *window)
{
    auto view = find_buildable (window, "tax_tables_view");
    if (!GTK_IS_TREE_VIEW (view))
        return false;
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    GtkTreeIter iter;
    if (!gtk_tree_model_get_iter_first (model, &iter))
        return false;
    auto path = gtk_tree_model_get_path (model, &iter);
    gtk_tree_selection_select_path (
        gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), path);
    gtk_tree_path_free (path);
    return true;
}

void
TaxTableRenameResponseTest::SetUp ()
{
    GnomeResponseTest::SetUp ();
    book = qof_book_new ();
    ASSERT_NE (book, nullptr);
    session = qof_session_new (book);
    ASSERT_NE (session, nullptr);
    gnc_set_current_session (session);
    auto table = gncTaxTableCreate (book);
    ASSERT_NE (table, nullptr);
    gncTaxTableSetName (table, "Tax table before response");
    table_guid = *gncTaxTableGetGUID (table);

    owner = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    ASSERT_NE (owner, nullptr);
    gtk_widget_realize (owner);
    ASSERT_NE (gnc_ui_tax_table_window_new (GTK_WINDOW (owner), book), nullptr);
    table_window = find_tax_table_window ();
    ASSERT_NE (table_window, nullptr);
}

void
TaxTableRenameResponseTest::TearDown ()
{
    if (event_handler)
    {
        qof_event_unregister_handler (event_handler);
        event_handler = 0;
    }
    if (session)
        gnc_close_gui_component_by_session (session);
    GnomeResponseTest::TearDown ();
    auto current = gnc_exchange_current_session (nullptr);
    if (current)
        qof_session_destroy (current);
    if (session && session != current)
        qof_session_destroy (session);
    session = nullptr;
}

TEST_F (TaxTableRenameResponseTest, AcceptRenamesPublicTaxTable)
{
    ASSERT_TRUE (select_tax_table (table_window));
    auto rename_button = find_buildable (table_window, "rename_table_button");
    ASSERT_TRUE (GTK_IS_BUTTON (rename_button));
    gtk_button_clicked (GTK_BUTTON (rename_button));
    auto dialog = find_rename_dialog (table_window);
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    auto entry = find_entry (dialog);
    ASSERT_TRUE (GTK_IS_ENTRY (entry));
    gtk_entry_set_text (GTK_ENTRY (entry), "Renamed asynchronously");
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    auto table = gncTaxTableLookup (book, &table_guid);
    ASSERT_NE (table, nullptr);
    EXPECT_STREQ (gncTaxTableGetName (table), "Renamed asynchronously");
    EXPECT_EQ (find_rename_dialog (table_window), nullptr);
}

TEST_F (TaxTableRenameResponseTest, DestroyedManagerIgnoresLateRename)
{
    ASSERT_TRUE (select_tax_table (table_window));
    auto rename_button = find_buildable (table_window, "rename_table_button");
    ASSERT_TRUE (GTK_IS_BUTTON (rename_button));
    gtk_button_clicked (GTK_BUTTON (rename_button));
    auto dialog = find_rename_dialog (table_window);
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    auto entry = find_entry (dialog);
    ASSERT_TRUE (GTK_IS_ENTRY (entry));
    gtk_entry_set_text (GTK_ENTRY (entry), "Must not be applied");
    g_signal_connect (dialog, "destroy",
                      G_CALLBACK (destroy_tax_table_parent), table_window);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    auto table = gncTaxTableLookup (book, &table_guid);
    ASSERT_NE (table, nullptr);
    EXPECT_STREQ (gncTaxTableGetName (table), "Tax table before response");
    EXPECT_EQ (find_tax_table_window (), nullptr);
}

TEST_F (TaxTableRenameResponseTest, SessionCloseDuringResponseDoesNotRename)
{
    ASSERT_TRUE (select_tax_table (table_window));
    auto rename_button = find_buildable (table_window, "rename_table_button");
    ASSERT_TRUE (GTK_IS_BUTTON (rename_button));
    gtk_button_clicked (GTK_BUTTON (rename_button));
    auto dialog = find_rename_dialog (table_window);
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    auto entry = find_entry (dialog);
    ASSERT_TRUE (GTK_IS_ENTRY (entry));
    gtk_entry_set_text (GTK_ENTRY (entry), "Must not survive session close");
    monitor.table_guid = table_guid;
    event_handler = qof_event_register_handler (
        count_table_modification, &monitor);
    g_signal_connect (dialog, "destroy", G_CALLBACK (destroy_current_session),
                      &session);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    qof_event_unregister_handler (event_handler);
    event_handler = 0;

    EXPECT_EQ (monitor.modifications, 0u);
    EXPECT_FALSE (gnc_current_session_exist ());
    EXPECT_EQ (find_tax_table_window (), nullptr);
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
    qof_init ();
    if (!cashobjects_register ())
        g_error ("Failed to register cash objects for tax table tests");
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    g_log_set_always_fatal (static_cast<GLogLevelFlags> (
        G_LOG_FATAL_MASK | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL));
    auto result = RUN_ALL_TESTS ();
    gnc_gsettings_shutdown ();
    gnc_component_manager_shutdown ();
    gnc_clear_current_session ();
    qof_close ();
    return result;
}
