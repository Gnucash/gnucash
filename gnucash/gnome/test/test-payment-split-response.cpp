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

#include "cashobjects.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gncCustomer.h"
#include "gncOwner.h"
#include "gnc-commodity.h"
#include "Account.h"
#include "Split.h"
#include "Transaction.h"
#include "qof.h"
#include "dialog-payment.h"

namespace
{
gboolean display_available;
gboolean parent_was_destroyed;

GtkWidget *
find_payment_split_dialog (GtkWidget *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (GTK_IS_DIALOG (widget) &&
            gtk_window_get_transient_for (GTK_WINDOW (widget)) ==
            GTK_WINDOW (parent))
        {
            g_assert_null (result);
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

GtkWidget *
find_payment_window ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (widget), "gnc-id-payment") == 0)
        {
            g_assert_null (result);
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

struct PaymentFixture
{
    QofBook *book;
    QofSession *session;
    Transaction *txn;
    Split *first_split;
    Split *other_split;
    Split *counter_split;
    GncOwner owner;
};

PaymentFixture
make_payment_fixture ()
{
    PaymentFixture fixture{};
    fixture.book = qof_book_new ();
    fixture.session = qof_session_new (fixture.book);
    gnc_set_current_session (fixture.session);

    auto commodity_table = gnc_commodity_table_get_table (fixture.book);
    gnc_commodity_table_add_namespace (commodity_table, "CURRENCY",
                                       fixture.book);
    auto currency = gnc_commodity_new (fixture.book, "Test Currency",
                                       "CURRENCY", "TST", nullptr, 100);
    gnc_commodity_table_insert (commodity_table, currency);
    auto root = gnc_account_create_root (fixture.book);
    auto customer = gncCustomerCreate (fixture.book);
    gncCustomerSetID (customer, "TEST-CUSTOMER");
    gncCustomerSetName (customer, "Test customer");
    gncCustomerSetCurrency (customer, currency);
    gncOwnerInitCustomer (&fixture.owner, customer);
    Account *accounts[3];
    for (int i = 0; i < 3; ++i)
    {
        accounts[i] = xaccMallocAccount (fixture.book);
        xaccAccountSetName (accounts[i], i == 0 ? "test-bank-a" :
                            i == 1 ? "test-bank-b" : "test-expense");
        xaccAccountSetType (accounts[i], i == 2 ? ACCT_TYPE_EXPENSE :
                            ACCT_TYPE_BANK);
        xaccAccountSetCommodity (accounts[i], currency);
        gnc_account_append_child (root, accounts[i]);
    }
    /* Customer payments require a receivable posting account. Without one,
     * the real payment dialog opens its blocking "no valid Post To account"
     * warning, which is unrelated to split-choice response handling. */
    auto receivable = xaccMallocAccount (fixture.book);
    xaccAccountSetName (receivable, "test-receivable");
    xaccAccountSetType (receivable, ACCT_TYPE_RECEIVABLE);
    xaccAccountSetCommodity (receivable, currency);
    gnc_account_append_child (root, receivable);

    fixture.txn = xaccMallocTransaction (fixture.book);
    xaccTransBeginEdit (fixture.txn);
    xaccTransSetCurrency (fixture.txn, currency);
    for (int i = 0; i < 3; ++i)
    {
        auto split = xaccMallocSplit (fixture.book);
        xaccTransAppendSplit (fixture.txn, split);
        xaccSplitSetAccount (split, accounts[i]);
        xaccSplitSetAmount (split, gnc_numeric_create (i == 2 ? -20 : 10, 1));
        xaccSplitSetValue (split, gnc_numeric_create (i == 2 ? -20 : 10, 1));
        if (i == 0)
            fixture.first_split = split;
        else if (i == 1)
            fixture.other_split = split;
        else
            fixture.counter_split = split;
    }
    xaccTransCommitEdit (fixture.txn);
    return fixture;
}

GtkWidget *
find_split_radio (GtkWidget *root, const GncGUID *split_guid)
{
    if (GTK_IS_RADIO_BUTTON (root))
    {
        auto guid = static_cast<GncGUID *>(
            g_object_get_data (G_OBJECT (root), "split-guid"));
        if (guid && guid_equal (guid, split_guid))
            return root;
    }
    if (!GTK_IS_CONTAINER (root))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *result = nullptr;
    for (auto node = children; node && !result; node = node->next)
        result = find_split_radio (GTK_WIDGET (node->data), split_guid);
    g_list_free (children);
    return result;
}

void
finish_fixture (PaymentFixture &fixture)
{
    gnc_clear_current_session ();
    fixture.session = nullptr;
}

void
destroy_parent_on_dialog_destroy (GtkWidget *, gpointer parent)
{
    parent_was_destroyed = TRUE;
    gtk_widget_destroy (GTK_WIDGET (parent));
}

void
test_parent_destroy_during_accepted_response ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto fixture = make_payment_fixture ();
    gnc_ui_payment_new_with_txn_async (nullptr, &fixture.owner, fixture.txn);
    g_assert_null (find_payment_window ());
    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    parent_was_destroyed = FALSE;
    g_object_ref_sink (parent); // Retain the destroyed widget to exercise the weak-pointer boundary.
    gtk_widget_realize (parent);
    gnc_ui_payment_new_with_txn_async (GTK_WINDOW (parent), &fixture.owner,
                                       fixture.txn);
    auto chooser = find_payment_split_dialog (parent);
    g_assert_true (GTK_IS_DIALOG (chooser));
    g_signal_connect (chooser, "destroy",
                      G_CALLBACK (destroy_parent_on_dialog_destroy), parent);
    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_OK);

    g_assert_true (parent_was_destroyed);
    g_assert_null (find_payment_window ());
    g_object_unref (parent);
    finish_fixture (fixture);
}

void
test_cancel_and_deleted_split ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto fixture = make_payment_fixture ();
    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_realize (parent);
    gnc_ui_payment_new_with_txn_async (GTK_WINDOW (parent), &fixture.owner,
                                       fixture.txn);
    auto chooser = find_payment_split_dialog (parent);
    g_assert_true (GTK_IS_DIALOG (chooser));
    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_CANCEL);
    g_assert_null (find_payment_window ());
    gtk_widget_destroy (parent);

    parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_realize (parent);
    gnc_ui_payment_new_with_txn_async (GTK_WINDOW (parent), &fixture.owner,
                                       fixture.txn);
    chooser = find_payment_split_dialog (parent);
    g_assert_true (GTK_IS_DIALOG (chooser));
    auto selected_guid = *xaccSplitGetGUID (fixture.first_split);
    auto radio = find_split_radio (chooser, &selected_guid);
    g_assert_true (GTK_IS_RADIO_BUTTON (radio));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radio), TRUE);
    xaccTransBeginEdit (fixture.txn);
    xaccSplitSetAmount (fixture.counter_split,
                        gnc_numeric_create (-10, 1));
    xaccSplitSetValue (fixture.counter_split,
                       gnc_numeric_create (-10, 1));
    xaccSplitDestroy (fixture.first_split);
    xaccTransCommitEdit (fixture.txn);
    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_OK);
    g_assert_null (find_payment_window ());
    gtk_widget_destroy (parent);
    finish_fixture (fixture);
}

void
test_accepted_response_opens_payment_window ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto fixture = make_payment_fixture ();
    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_realize (parent);
    gnc_ui_payment_new_with_txn_async (GTK_WINDOW (parent), &fixture.owner,
                                       fixture.txn);
    auto chooser = find_payment_split_dialog (parent);
    g_assert_true (GTK_IS_DIALOG (chooser));
    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_OK);
    g_assert_nonnull (find_payment_window ());
    gtk_widget_destroy (find_payment_window ());
    gtk_widget_destroy (parent);
    finish_fixture (fixture);
}
}

static int
run_tests (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    g_test_add_func ("/gnome/payment-split/parent-destroy-response",
                     test_parent_destroy_during_accepted_response);
    g_test_add_func ("/gnome/payment-split/cancel-and-stale-split",
                     test_cancel_and_deleted_split);
    g_test_add_func ("/gnome/payment-split/accepted-response-opens-window",
                     test_accepted_response_opens_payment_window);
    auto result = g_test_run ();
    gnc_gsettings_shutdown ();
    gnc_component_manager_shutdown ();
    gnc_clear_current_session ();
    qof_close ();
    return result;
}

static void
guile_main ([[maybe_unused]] void *data, int argc, char **argv)
{
    std::exit (run_tests (argc, argv));
}

int
main (int argc, char **argv)
{
    scm_boot_guile (argc, argv, guile_main, nullptr);
    return 0;
}
