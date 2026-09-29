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
find_print_check_dialog ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (widget), "gnc-id-print-check") == 0)
        {
            g_assert_null (result);
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
            g_assert_null (result);
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

GtkWidget *
open_title_dialog (GtkWidget *check_dialog)
{
    auto save = find_buildable (check_dialog, "save_button");
    g_assert_true (GTK_IS_BUTTON (save));
    gtk_button_clicked (GTK_BUTTON (save));
    return find_title_dialog (check_dialog);
}

void
close_check_parent_on_title_destroy (GtkWidget *, gpointer check_dialog)
{
    gtk_dialog_response (GTK_DIALOG (check_dialog), GTK_RESPONSE_DELETE_EVENT);
}

void
test_cancel_duplicate_and_retained_parent_late_response ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto fixture = make_check_fixture ();
    auto host = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_realize (host);
    GList *splits = g_list_append (nullptr, fixture.check_split);
    gnc_ui_print_check_dialog_create (host, splits, fixture.bank);
    g_list_free (splits);
    auto check = find_print_check_dialog ();
    g_assert_true (GTK_IS_DIALOG (check));
    auto save = find_buildable (check, "save_button");

    auto title = open_title_dialog (check);
    g_assert_true (GTK_IS_DIALOG (title));
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (title)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (title)));
    gtk_button_clicked (GTK_BUTTON (save));
    g_assert_true (find_title_dialog (check) == title);
    gtk_dialog_response (GTK_DIALOG (title), GTK_RESPONSE_CANCEL);
    g_assert_null (g_object_get_data (G_OBJECT (check), "check-title-dialog"));

    title = open_title_dialog (check);
    g_assert_true (GTK_IS_DIALOG (title));
    g_object_ref (title);
    g_object_ref (check); // Keep the destroyed parent alive to exercise stale-owner handling.
    gtk_dialog_response (GTK_DIALOG (check), GTK_RESPONSE_DELETE_EVENT);
    g_assert_null (g_object_get_data (G_OBJECT (check), "print-check-owner"));
    g_assert_null (find_title_dialog (check));
    gtk_dialog_response (GTK_DIALOG (title), GTK_RESPONSE_OK);
    g_assert_null (g_object_get_data (G_OBJECT (check), "check-title-dialog"));
    g_object_unref (title);
    g_object_unref (check);
    gtk_widget_destroy (host);
    gnc_clear_current_session ();
}

void
test_title_response_after_reentrant_parent_destroy ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto fixture = make_check_fixture ();
    auto host = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_realize (host);
    GList *splits = g_list_append (nullptr, fixture.check_split);
    gnc_ui_print_check_dialog_create (host, splits, fixture.bank);
    g_list_free (splits);
    auto check = find_print_check_dialog ();
    g_assert_true (GTK_IS_DIALOG (check));
    auto title = open_title_dialog (check);
    g_assert_true (GTK_IS_DIALOG (title));
    auto entry = GTK_ENTRY (find_buildable (title, "format_title"));
    g_assert_true (GTK_IS_ENTRY (entry));
    gtk_entry_set_text (entry, "synthetic-no-write-response");

    g_object_ref (check);
    g_signal_connect (title, "destroy",
                      G_CALLBACK (close_check_parent_on_title_destroy), check);
    gtk_dialog_response (GTK_DIALOG (title), GTK_RESPONSE_OK);
    g_assert_null (g_object_get_data (G_OBJECT (check), "print-check-owner"));
    g_assert_null (g_object_get_data (G_OBJECT (check), "check-title-dialog"));
    g_object_unref (check);
    gtk_widget_destroy (host);
    gnc_clear_current_session ();
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
    gnc_gsettings_load_backend ();
    g_test_add_func ("/gnome/print-check/title-cancel-duplicate-parent-destroy",
                     test_cancel_duplicate_and_retained_parent_late_response);
    g_test_add_func ("/gnome/print-check/title-response-reentrant-parent-destroy",
                     test_title_response_after_reentrant_parent_destroy);
    auto result = g_test_run ();
    gnc_gsettings_shutdown ();
    qof_close ();
    return result;
}
