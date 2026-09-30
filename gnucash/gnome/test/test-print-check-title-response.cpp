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

#include "cashobjects.h"
#include "Account.h"
#include "Split.h"
#include "Transaction.h"
#include "gnc-commodity.h"
#include "gnc-session.h"
#include "gnc-gsettings.h"
#include "qof.h"
#include "dialog-print-check.h"

namespace
{

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
find_print_check_dialog ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (widget), "gnc-id-print-check") == 0)
        {
            EXPECT_EQ (result, nullptr);
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

GtkWidget *
find_title_dialog (GtkWidget *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (GTK_IS_DIALOG (widget) &&
            gtk_window_get_transient_for (GTK_WINDOW (widget)) == GTK_WINDOW (parent) &&
            find_buildable (widget, "format_title"))
        {
            EXPECT_EQ (result, nullptr);
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

struct CheckFixture
{
    QofBook *book;
    QofSession *session;
    gnc_commodity *currency;
    Account *bank;
    Transaction *txn;
    Split *check_split;
};

CheckFixture
make_check_fixture ()
{
    CheckFixture fixture{};
    fixture.book = qof_book_new ();
    fixture.session = qof_session_new (fixture.book);
    gnc_set_current_session (fixture.session);
    auto table = gnc_commodity_table_get_table (fixture.book);
    gnc_commodity_table_add_namespace (table, "CURRENCY", fixture.book);
    fixture.currency = gnc_commodity_new (fixture.book, "Test Currency",
                                           "CURRENCY", "TST", nullptr, 100);
    gnc_commodity_table_insert (table, fixture.currency);

    auto root = gnc_account_create_root (fixture.book);
    fixture.bank = xaccMallocAccount (fixture.book);
    xaccAccountSetName (fixture.bank, "synthetic-check-bank");
    xaccAccountSetType (fixture.bank, ACCT_TYPE_BANK);
    xaccAccountSetCommodity (fixture.bank, fixture.currency);
    gnc_account_append_child (root, fixture.bank);
    auto expense = xaccMallocAccount (fixture.book);
    xaccAccountSetName (expense, "synthetic-check-expense");
    xaccAccountSetType (expense, ACCT_TYPE_EXPENSE);
    xaccAccountSetCommodity (expense, fixture.currency);
    gnc_account_append_child (root, expense);

    fixture.txn = xaccMallocTransaction (fixture.book);
    xaccTransBeginEdit (fixture.txn);
    xaccTransSetCurrency (fixture.txn, fixture.currency);
    fixture.check_split = xaccMallocSplit (fixture.book);
    auto other_split = xaccMallocSplit (fixture.book);
    xaccTransAppendSplit (fixture.txn, fixture.check_split);
    xaccTransAppendSplit (fixture.txn, other_split);
    xaccSplitSetAccount (fixture.check_split, fixture.bank);
    xaccSplitSetAccount (other_split, expense);
    xaccSplitSetAmount (fixture.check_split, gnc_numeric_create (100, 1));
    xaccSplitSetValue (fixture.check_split, gnc_numeric_create (100, 1));
    xaccSplitSetAmount (other_split, gnc_numeric_create (-100, 1));
    xaccSplitSetValue (other_split, gnc_numeric_create (-100, 1));
    xaccTransCommitEdit (fixture.txn);
    return fixture;
}

struct CloseCheckParentState
{
    GtkWidget *check{};
};

void
close_check_parent_on_title_destroy (GtkWidget *, gpointer user_data)
{
    auto state = static_cast<CloseCheckParentState *> (user_data);
    gtk_dialog_response (GTK_DIALOG (state->check), GTK_RESPONSE_DELETE_EVENT);
}

class PrintCheckTitleResponseTest : public GnomeResponseTest
{
protected:
    void SetUp () override
    {
        GnomeResponseTest::SetUp ();
        fixture = make_check_fixture ();
        host = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        g_object_ref_sink (host);
        gtk_widget_realize (host);
        GList *splits = g_list_append (nullptr, fixture.check_split);
        gnc_ui_print_check_dialog_create (host, splits, fixture.bank);
        g_list_free (splits);
        check = find_print_check_dialog ();
        ASSERT_TRUE (GTK_IS_DIALOG (check));
        g_object_ref (check);
    }

    void TearDown () override
    {
        for (auto widget : retained_widgets)
            g_object_unref (widget);
        if (check)
        {
            gtk_widget_destroy (check);
            g_object_unref (check);
        }
        if (host)
        {
            gtk_widget_destroy (host);
            g_object_unref (host);
        }
        GnomeResponseTest::TearDown ();
        gnc_clear_current_session ();
    }

    CheckFixture fixture{};
    GtkWidget *host{};
    GtkWidget *check{};
    CloseCheckParentState parent_state{};
    std::vector<GtkWidget *> retained_widgets;
};

TEST_F (PrintCheckTitleResponseTest, CancelDuplicateAndLateResponseAfterParentDestroy)
{
    auto save = find_buildable (check, "save_button");
    ASSERT_TRUE (GTK_IS_BUTTON (save));
    gtk_button_clicked (GTK_BUTTON (save));
    auto title = find_title_dialog (check);
    ASSERT_TRUE (GTK_IS_DIALOG (title));
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (title)));
    EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (title)));
    gtk_button_clicked (GTK_BUTTON (save));
    EXPECT_EQ (find_title_dialog (check), title);
    gtk_dialog_response (GTK_DIALOG (title), GTK_RESPONSE_CANCEL);
    EXPECT_EQ (g_object_get_data (G_OBJECT (check), "check-title-dialog"), nullptr);

    gtk_button_clicked (GTK_BUTTON (save));
    title = find_title_dialog (check);
    ASSERT_TRUE (GTK_IS_DIALOG (title));
    g_object_ref (title);
    retained_widgets.push_back (title);
    gtk_dialog_response (GTK_DIALOG (check), GTK_RESPONSE_DELETE_EVENT);
    EXPECT_EQ (g_object_get_data (G_OBJECT (check), "print-check-owner"), nullptr);
    EXPECT_EQ (find_title_dialog (check), nullptr);
    gtk_dialog_response (GTK_DIALOG (title), GTK_RESPONSE_OK);
    EXPECT_EQ (g_object_get_data (G_OBJECT (check), "check-title-dialog"), nullptr);
}

TEST_F (PrintCheckTitleResponseTest, TitleResponseCanReentrantlyDestroyParent)
{
    auto save = find_buildable (check, "save_button");
    ASSERT_TRUE (GTK_IS_BUTTON (save));
    gtk_button_clicked (GTK_BUTTON (save));
    auto title = find_title_dialog (check);
    ASSERT_TRUE (GTK_IS_DIALOG (title));
    auto entry = GTK_ENTRY (find_buildable (title, "format_title"));
    ASSERT_TRUE (GTK_IS_ENTRY (entry));
    gtk_entry_set_text (entry, "synthetic-no-write-response");

    parent_state.check = check;
    g_signal_connect (title, "destroy",
                      G_CALLBACK (close_check_parent_on_title_destroy),
                      &parent_state);
    gtk_dialog_response (GTK_DIALOG (title), GTK_RESPONSE_OK);
    EXPECT_EQ (g_object_get_data (G_OBJECT (check), "print-check-owner"), nullptr);
    EXPECT_EQ (g_object_get_data (G_OBJECT (check), "check-title-dialog"), nullptr);
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
    g_assert_true (cashobjects_register ());
    gnc_gsettings_load_backend ();
    g_log_set_always_fatal (static_cast<GLogLevelFlags> (
        G_LOG_FATAL_MASK | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL));
    auto result = RUN_ALL_TESTS ();
    gnc_gsettings_shutdown ();
    qof_close ();
    return result;
}
