/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include "test-logging.hpp"
#include <gtk/gtk.h>
#include "test/gnome-response-test-fixture.h"
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
struct ParentDestroyState
{
    GtkWidget *parent{};
    bool destroyed{};
};

static void
destroy_parent_on_dialog_destroy (GtkWidget *, gpointer user_data)
{
    auto state = static_cast<ParentDestroyState *> (user_data);
    state->destroyed = true;
    gtk_widget_destroy (state->parent);
}

static GtkWidget *
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

static GtkWidget *
find_payment_window ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (widget), "gnc-id-payment") == 0)
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

static PaymentFixture
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

static GtkWidget *
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

class PaymentSplitResponseTest : public GnomeResponseTest
{
protected:
    void SetUp () override
    {
        GnomeResponseTest::SetUp ();
        fixture = make_payment_fixture ();
        parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        ASSERT_NE (parent, nullptr);
        g_object_ref_sink (parent);
        gtk_widget_realize (parent);
    }
    void TearDown () override
    {
        if (parent)
        {
            gtk_widget_destroy (parent);
            g_object_unref (parent);
            parent = nullptr;
        }
        GnomeResponseTest::TearDown ();
        gnc_clear_current_session ();
        fixture.session = nullptr;
    }
    PaymentFixture fixture{};
    GtkWidget *parent{};
    ParentDestroyState parent_destroy_state{};
};

TEST_F (PaymentSplitResponseTest, ParentDestroyDuringAcceptedResponse)
{
    gnc_ui_payment_new_with_txn_async (nullptr, &fixture.owner, fixture.txn);
    EXPECT_EQ (find_payment_window (), nullptr);
    gnc_ui_payment_new_with_txn_async (GTK_WINDOW (parent), &fixture.owner,
                                       fixture.txn);
    auto chooser = find_payment_split_dialog (parent);
    ASSERT_TRUE (GTK_IS_DIALOG (chooser));
    parent_destroy_state = {parent, false};
    g_signal_connect (chooser, "destroy",
                      G_CALLBACK (destroy_parent_on_dialog_destroy),
                      &parent_destroy_state);
    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_OK);
    EXPECT_TRUE (parent_destroy_state.destroyed);
    EXPECT_EQ (find_payment_window (), nullptr);
}

TEST_F (PaymentSplitResponseTest, CancelDoesNotOpenPaymentWindow)
{
    gnc_ui_payment_new_with_txn_async (GTK_WINDOW (parent), &fixture.owner,
                                       fixture.txn);
    auto chooser = find_payment_split_dialog (parent);
    ASSERT_TRUE (GTK_IS_DIALOG (chooser));
    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_CANCEL);
    EXPECT_EQ (find_payment_window (), nullptr);
}

TEST_F (PaymentSplitResponseTest, DeletedSelectedSplitDoesNotOpenPaymentWindow)
{
    gnc_ui_payment_new_with_txn_async (GTK_WINDOW (parent), &fixture.owner,
                                       fixture.txn);
    auto chooser = find_payment_split_dialog (parent);
    ASSERT_TRUE (GTK_IS_DIALOG (chooser));
    auto selected_guid = *xaccSplitGetGUID (fixture.first_split);
    auto radio = find_split_radio (chooser, &selected_guid);
    ASSERT_TRUE (GTK_IS_RADIO_BUTTON (radio));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radio), true);
    xaccTransBeginEdit (fixture.txn);
    xaccSplitSetAmount (fixture.counter_split, gnc_numeric_create (-10, 1));
    xaccSplitSetValue (fixture.counter_split, gnc_numeric_create (-10, 1));
    xaccSplitDestroy (fixture.first_split);
    xaccTransCommitEdit (fixture.txn);
    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_OK);
    EXPECT_EQ (find_payment_window (), nullptr);
}

TEST_F (PaymentSplitResponseTest, AcceptedResponseOpensPaymentWindow)
{
    gnc_ui_payment_new_with_txn_async (GTK_WINDOW (parent), &fixture.owner,
                                       fixture.txn);
    auto chooser = find_payment_split_dialog (parent);
    ASSERT_TRUE (GTK_IS_DIALOG (chooser));
    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_OK);
    auto payment = find_payment_window ();
    ASSERT_NE (payment, nullptr);
    gtk_widget_destroy (payment);
}
}

static int
run_tests (int argc, char **argv)
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
