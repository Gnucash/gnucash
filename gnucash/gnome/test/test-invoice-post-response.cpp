/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <cstdint>
#include <gtk/gtk.h>
#include <libguile.h>
#include <gtest/gtest.h>
#include "test-logging.hpp"
#include "test/gnome-response-test-fixture.h"
#include <cstdlib>
#include <utility>

#include "Account.h"
#include "cashobjects.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-main-window.h"
#include "gnc-session.h"
#include "gncCustomer.h"
#include "gncInvoice.h"
#include "gncOwner.h"
#include "gncEntry.h"
#include "gnc-commodity.h"
#include "dialog-invoice.h"
#include "gnc-amount-edit.h"
#include "gnc-account-sel.h"
#include "gnucash-register.h"

namespace
{

QofSession *window_sentinel_session{};
GncMainWindow *window_sentinel{};

static void
create_window_sentinel ()
{
    window_sentinel_session = qof_session_new (qof_book_new ());
    gnc_set_current_session (window_sentinel_session);
    window_sentinel = gnc_main_window_new ();
    g_object_ref_sink (window_sentinel);
    gnc_exchange_current_session (nullptr);
}

static void
destroy_window_sentinel ()
{
    if (window_sentinel)
    {
        gtk_widget_destroy (GTK_WIDGET (window_sentinel));
        g_object_unref (window_sentinel);
        window_sentinel = nullptr;
    }
    if (window_sentinel_session)
    {
        qof_session_destroy (window_sentinel_session);
        window_sentinel_session = nullptr;
    }
}

struct Fixture
{
    QofSession *session{};
    QofBook *book{};
    GncMainWindow *window{};
    InvoiceWindow *invoice_window{};
    GncInvoice *invoice{};
    Account *posting_account{};
};

static Fixture
make_fixture ()
{
    Fixture f{};
    f.book = qof_book_new ();
    f.session = qof_session_new (f.book);
    gnc_set_current_session (f.session);
    auto table = gnc_commodity_table_get_table (f.book);
    gnc_commodity_table_add_namespace (table, "CURRENCY", f.book);
    auto make_currency = [f, table] (const char *mnemonic) mutable {
        if (auto existing = gnc_commodity_table_lookup(table, "CURRENCY", mnemonic))
            return existing;
        auto commodity = gnc_commodity_new (f.book, mnemonic, "CURRENCY",
                                             mnemonic, nullptr, 100);
        gnc_commodity_table_insert (table, commodity);
        return commodity;
    };
    auto usd = make_currency ("USD");
    auto eur = make_currency ("EUR");
    auto gbp = make_currency ("GBP");

    auto root = gnc_account_create_root (f.book);
    auto receivable = xaccMallocAccount (f.book);
    f.posting_account = receivable;
    xaccAccountSetName (receivable, "Test receivable");
    xaccAccountSetType (receivable, ACCT_TYPE_RECEIVABLE);
    xaccAccountSetCommodity (receivable, usd);
    gnc_account_append_child (root, receivable);
    auto income_eur = xaccMallocAccount (f.book);
    xaccAccountSetName (income_eur, "EUR income");
    xaccAccountSetType (income_eur, ACCT_TYPE_INCOME);
    xaccAccountSetCommodity (income_eur, eur);
    gnc_account_append_child (root, income_eur);
    auto income_gbp = xaccMallocAccount (f.book);
    xaccAccountSetName (income_gbp, "GBP income");
    xaccAccountSetType (income_gbp, ACCT_TYPE_INCOME);
    xaccAccountSetCommodity (income_gbp, gbp);
    gnc_account_append_child (root, income_gbp);

    auto customer = gncCustomerCreate (f.book);
    gncCustomerSetID (customer, "TEST-CUSTOMER");
    gncCustomerSetName (customer, "Test customer");
    gncCustomerSetCurrency (customer, usd);
    gncCustomerBeginEdit(customer);
    qof_instance_set(QOF_INSTANCE(customer), "invoice-last-posted-account",
                      xaccAccountGetGUID(receivable), nullptr);
    gncCustomerCommitEdit(customer);
    GncOwner owner{};
    gncOwnerInitCustomer (&owner, customer);
    f.invoice = gncInvoiceCreate (f.book);
    gncInvoiceSetID (f.invoice, "TEST-INVOICE");
    gncInvoiceSetOwner (f.invoice, &owner);
    gncInvoiceSetCurrency (f.invoice, usd);
    for (auto [account, description] : {std::pair{income_eur, "EUR entry"},
                                        std::pair{income_gbp, "GBP entry"}})
    {
        auto entry = gncEntryCreate (f.book);
        gncEntrySetDate (entry, gnc_time (nullptr));
        gncEntrySetDateEntered (entry, gnc_time (nullptr));
        gncEntrySetDescription (entry, description);
        gncEntrySetQuantity (entry, gnc_numeric_create (1, 1));
        gncEntrySetInvPrice (entry, gnc_numeric_create (10, 1));
        gncEntrySetInvAccount (entry, account);
        gncInvoiceAddEntry (f.invoice, entry);
    }

    f.window = gnc_main_window_new ();
    g_object_ref_sink (f.window);
    f.invoice_window = gnc_ui_invoice_edit (GTK_WINDOW (f.window), f.invoice);
    return f;
}

static GtkWidget *
find_widget (GtkWidget *root, const char *name)
{
    if (g_strcmp0 (gtk_widget_get_name (root), name) == 0 ||
        (GTK_IS_BUILDABLE(root) && !g_strcmp0(gtk_buildable_get_name(GTK_BUILDABLE(root)), name)))
        return root;
    if (!GTK_IS_CONTAINER (root))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_widget (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return found;
}

static GtkWidget *
find_post_dialog ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node && !found; node = node->next)
        if (g_strcmp0 (gtk_widget_get_name (GTK_WIDGET (node->data)),
                       "gnc-id-date-close") == 0)
            found = GTK_WIDGET (node->data);
    g_list_free (windows);
    return found;
}

static GtkWidget *
find_transfer_dialog ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node && !found; node = node->next)
        if (!g_strcmp0 (gtk_widget_get_name (GTK_WIDGET (node->data)),
                        "gnc-id-transfer"))
            found = GTK_WIDGET (node->data);
    g_list_free (windows);
    return found;
}

static GtkWidget *
wait_for_dialog (GtkWidget *(*find_dialog) ())
{
    for (std::uint32_t i = 0; i < 2000 && !find_dialog (); ++i)
    {
        while (g_main_context_iteration (nullptr, false))
            ;
        g_usleep (1000);
    }
    return find_dialog ();
}

class InvoicePostResponseTest : public GnomeResponseTest
{
protected:
    void SetUp () override
    {
        GnomeResponseTest::SetUp ();
        fixture = make_fixture ();
        ASSERT_NE (fixture.invoice_window, nullptr);
        gtk_widget_realize (GTK_WIDGET (fixture.window));
        auto foreign = gncInvoiceGetForeignCurrencies (fixture.invoice);
        const auto foreign_count = g_hash_table_size (foreign);
        g_hash_table_unref (foreign);
        ASSERT_EQ (foreign_count, 2u);
    }

    void TearDown () override
    {
        if (auto notice = find_currency_notice ())
        {
            retain_widget (notice);
            gtk_dialog_response (GTK_DIALOG (notice), GTK_RESPONSE_OK);
        }
        if (fixture.window)
        {
            gtk_widget_destroy (GTK_WIDGET (fixture.window));
            while (g_main_context_iteration (nullptr, false))
                ;
            g_object_unref (fixture.window);
            fixture.window = nullptr;
        }
        for (auto widget : retained_widgets)
            g_object_unref (widget);
        GnomeResponseTest::TearDown ();
        gnc_clear_current_session ();
        fixture.session = nullptr;
    }

    Fixture fixture{};
    std::vector<GtkWidget *> retained_widgets;

    void retain_widget (GtkWidget *widget)
    {
        g_object_ref (widget);
        retained_widgets.push_back (widget);
    }

    GNCAccountSel *posting_selector (GtkWidget *form)
    {
        auto box = find_widget (form, "acct_hbox");
        if (!GTK_IS_CONTAINER (box))
            return nullptr;
        auto children = gtk_container_get_children (GTK_CONTAINER (box));
        GNCAccountSel *selector = nullptr;
        for (auto node = children; node; node = node->next)
            if (GNC_IS_ACCOUNT_SEL (node->data))
                selector = GNC_ACCOUNT_SEL (node->data);
        g_list_free (children);
        return selector;
    }

    std::uint32_t dismiss_posting_errors (GtkWidget *form)
    {
        std::vector<GtkWidget *> errors;
        auto windows = gtk_window_list_toplevels ();
        for (auto node = windows; node; node = node->next)
        {
            auto widget = GTK_WIDGET (node->data);
            if (!GTK_IS_MESSAGE_DIALOG (widget) ||
                gtk_window_get_transient_for (GTK_WINDOW (widget)) != GTK_WINDOW (form))
                continue;
            GtkMessageType type;
            g_object_get (widget, "message-type", &type, nullptr);
            if (type == GTK_MESSAGE_ERROR)
            {
                retain_widget (widget);
                errors.push_back (widget);
            }
        }
        g_list_free (windows);
        for (auto error : errors)
            gtk_dialog_response (GTK_DIALOG (error), GTK_RESPONSE_CLOSE);
        return errors.size ();
    }

    GtkWidget *find_currency_notice ()
    {
        GtkWidget *notice = nullptr;
        auto windows = gtk_window_list_toplevels ();
        for (auto node = windows; node; node = node->next)
        {
            auto widget = GTK_WIDGET (node->data);
            if (!GTK_IS_MESSAGE_DIALOG (widget))
                continue;
            GtkMessageType type;
            g_object_get (widget, "message-type", &type, nullptr);
            if (type == GTK_MESSAGE_INFO &&
                gtk_window_get_transient_for (GTK_WINDOW (widget)) ==
                    GTK_WINDOW (fixture.window))
            {
                if (notice)
                {
                    g_list_free (windows);
                    return nullptr;
                }
                notice = widget;
            }
        }
        g_list_free (windows);
        return notice;
    }

    bool dismiss_currency_notice ()
    {
        auto notice = find_currency_notice ();
        if (!notice)
        {
            ADD_FAILURE () << "Invoice currency notice was not shown";
            return false;
        }

        retain_widget (notice);
        gtk_dialog_response (GTK_DIALOG (notice), GTK_RESPONSE_OK);
        return true;
    }

};

TEST_F (InvoicePostResponseTest, CancelDoesNotPostInvoice)
{
    gnc_invoice_window_postCB (nullptr, fixture.invoice_window);
    auto post_dialog = wait_for_dialog (find_post_dialog);
    ASSERT_NE (post_dialog, nullptr);
    EXPECT_EQ (qof_instance_get_editlevel (fixture.invoice), 0);
    EXPECT_FALSE (gnc_gui_refresh_suspended ());
    EXPECT_FALSE (gncInvoiceIsPosted (fixture.invoice));
    gtk_dialog_response (GTK_DIALOG (post_dialog), GTK_RESPONSE_CANCEL);
    EXPECT_FALSE (gncInvoiceIsPosted (fixture.invoice));
    EXPECT_EQ (qof_instance_get_editlevel (fixture.invoice), 0);
    EXPECT_FALSE (gnc_gui_refresh_suspended ());
}

TEST_F (InvoicePostResponseTest, MissingAccountShowsOneErrorPerClick)
{
    gnc_invoice_window_postCB (nullptr, fixture.invoice_window);
    auto form = wait_for_dialog (find_post_dialog);
    ASSERT_NE (form, nullptr);
    retain_widget (form);
    auto selector = posting_selector (form);
    ASSERT_NE (selector, nullptr);
    gnc_account_sel_set_account (selector, nullptr, false);
    auto ok = find_widget (form, "okbutton1");
    ASSERT_TRUE (GTK_IS_BUTTON (ok));

    for (int attempt = 0; attempt < 2; ++attempt)
    {
        gtk_button_clicked (GTK_BUTTON (ok));
        EXPECT_EQ (dismiss_posting_errors (form), 1u);
        EXPECT_EQ (find_post_dialog (), form);
        EXPECT_FALSE (gncInvoiceIsPosted (fixture.invoice));
    }
    gtk_dialog_response (GTK_DIALOG (form), GTK_RESPONSE_CANCEL);
}

TEST_F (InvoicePostResponseTest, PlaceholderAccountShowsOneErrorPerResponse)
{
    gnc_invoice_window_postCB (nullptr, fixture.invoice_window);
    auto form = wait_for_dialog (find_post_dialog);
    ASSERT_NE (form, nullptr);
    retain_widget (form);
    auto selector = posting_selector (form);
    ASSERT_NE (selector, nullptr);
    xaccAccountSetPlaceholder (fixture.posting_account, true);
    gnc_account_sel_set_account (selector, fixture.posting_account, false);

    gtk_dialog_response (GTK_DIALOG (form), GTK_RESPONSE_OK);
    EXPECT_EQ (dismiss_posting_errors (form), 1u);
    EXPECT_EQ (find_post_dialog (), form);
    EXPECT_FALSE (gncInvoiceIsPosted (fixture.invoice));

    auto ok = find_widget (form, "okbutton1");
    ASSERT_TRUE (GTK_IS_BUTTON (ok));
    gtk_button_clicked (GTK_BUTTON (ok));
    EXPECT_EQ (dismiss_posting_errors (form), 1u);
    EXPECT_EQ (find_post_dialog (), form);
    EXPECT_FALSE (gncInvoiceIsPosted (fixture.invoice));
    gtk_dialog_response (GTK_DIALOG (form), GTK_RESPONSE_CANCEL);
}

TEST_F (InvoicePostResponseTest, ParentDestroyPreventsLatePostResponse)
{
    gnc_invoice_window_postCB (nullptr, fixture.invoice_window);
    auto dialog = wait_for_dialog (find_post_dialog);
    ASSERT_NE (dialog, nullptr);
    retain_widget (dialog);
    auto ok_button = find_widget (dialog, "okbutton1");
    ASSERT_NE (ok_button, nullptr);
    retain_widget (ok_button);
    gtk_widget_destroy (GTK_WIDGET (fixture.window));
    gtk_button_clicked (GTK_BUTTON (ok_button));
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_FALSE (gncInvoiceIsPosted (fixture.invoice));
    EXPECT_EQ (qof_instance_get_editlevel (fixture.invoice), 0);
    EXPECT_FALSE (gnc_gui_refresh_suspended ());
}

TEST_F (InvoicePostResponseTest, CancelsForeignCurrencySelection)
{
    gnc_invoice_window_postCB (nullptr, fixture.invoice_window);
    auto post_dialog = wait_for_dialog (find_post_dialog);
    ASSERT_NE (post_dialog, nullptr);
    auto post_ok = find_widget (post_dialog, "okbutton1");
    ASSERT_NE (post_ok, nullptr);
    gtk_button_clicked (GTK_BUTTON (post_ok));
    auto transfer = wait_for_dialog (find_transfer_dialog);
    ASSERT_NE (transfer, nullptr);
    ASSERT_TRUE (dismiss_currency_notice ());
    retain_widget (transfer);
    EXPECT_EQ (qof_instance_get_editlevel (fixture.invoice), 0);
    EXPECT_FALSE (gnc_gui_refresh_suspended ());
    EXPECT_FALSE (gncInvoiceIsPosted (fixture.invoice));
    gtk_dialog_response (GTK_DIALOG (transfer), GTK_RESPONSE_CANCEL);
    while (g_main_context_iteration (nullptr, false))
        ;
    EXPECT_FALSE (gncInvoiceIsPosted (fixture.invoice));
    EXPECT_EQ (qof_instance_get_editlevel (fixture.invoice), 0);
    EXPECT_FALSE (gnc_gui_refresh_suspended ());
    gtk_dialog_response (GTK_DIALOG (transfer), GTK_RESPONSE_OK);
    EXPECT_FALSE (gncInvoiceIsPosted (fixture.invoice));
}

TEST_F (InvoicePostResponseTest, ParentDestructionCancelsCurrencySelection)
{
    gnc_invoice_window_postCB (nullptr, fixture.invoice_window);
    auto post_dialog = wait_for_dialog (find_post_dialog);
    ASSERT_NE (post_dialog, nullptr);
    auto post_ok = find_widget (post_dialog, "okbutton1");
    ASSERT_NE (post_ok, nullptr);
    gtk_button_clicked (GTK_BUTTON (post_ok));
    auto transfer = wait_for_dialog (find_transfer_dialog);
    ASSERT_NE (transfer, nullptr);
    ASSERT_TRUE (dismiss_currency_notice ());
    retain_widget (transfer);
    EXPECT_EQ (qof_instance_get_editlevel (fixture.invoice), 0);
    EXPECT_FALSE (gnc_gui_refresh_suspended ());
    EXPECT_FALSE (gncInvoiceIsPosted (fixture.invoice));
    gtk_widget_destroy (GTK_WIDGET (fixture.window));
    while (g_main_context_iteration (nullptr, false))
        ;
    EXPECT_FALSE (gncInvoiceIsPosted (fixture.invoice));
    EXPECT_EQ (qof_instance_get_editlevel (fixture.invoice), 0);
    EXPECT_FALSE (gnc_gui_refresh_suspended ());
    gtk_dialog_response (GTK_DIALOG (transfer), GTK_RESPONSE_OK);
    EXPECT_FALSE (gncInvoiceIsPosted (fixture.invoice));
}

TEST_F (InvoicePostResponseTest, AcceptsTwoForeignCurrenciesSequentially)
{
    gnc_invoice_window_postCB (nullptr, fixture.invoice_window);
    auto post_dialog = wait_for_dialog (find_post_dialog);
    ASSERT_NE (post_dialog, nullptr);
    auto post_ok = find_widget (post_dialog, "okbutton1");
    ASSERT_NE (post_ok, nullptr);
    gtk_button_clicked (GTK_BUTTON (post_ok));
    auto first = wait_for_dialog (find_transfer_dialog);
    ASSERT_NE (first, nullptr);
    ASSERT_TRUE (dismiss_currency_notice ());
    retain_widget (first);
    EXPECT_EQ (qof_instance_get_editlevel (fixture.invoice), 0);
    EXPECT_FALSE (gnc_gui_refresh_suspended ());
    EXPECT_FALSE (gncInvoiceIsPosted (fixture.invoice));

    auto price_box = find_widget (first, "price_hbox");
    ASSERT_NE (price_box, nullptr);
    auto children = gtk_container_get_children (GTK_CONTAINER (price_box));
    ASSERT_NE (children, nullptr);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (children->data),
                                gnc_numeric_create (2, 1));
    g_list_free (children);
    gtk_dialog_response (GTK_DIALOG (first), GTK_RESPONSE_OK);

    auto second = wait_for_dialog (find_transfer_dialog);
    ASSERT_NE (second, nullptr);
    EXPECT_NE (second, first);
    EXPECT_FALSE (gncInvoiceIsPosted (fixture.invoice));
    EXPECT_EQ (qof_instance_get_editlevel (fixture.invoice), 0);
    EXPECT_FALSE (gnc_gui_refresh_suspended ());
    price_box = find_widget (second, "price_hbox");
    ASSERT_NE (price_box, nullptr);
    children = gtk_container_get_children (GTK_CONTAINER (price_box));
    ASSERT_NE (children, nullptr);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (children->data),
                                gnc_numeric_create (3, 1));
    g_list_free (children);
    gtk_dialog_response (GTK_DIALOG (second), GTK_RESPONSE_OK);

    while (g_main_context_iteration (nullptr, false))
        ;
    EXPECT_TRUE (gncInvoiceIsPosted (fixture.invoice));
    EXPECT_EQ (qof_instance_get_editlevel (fixture.invoice), 0);
    EXPECT_FALSE (gnc_gui_refresh_suspended ());
    gtk_dialog_response (GTK_DIALOG (first), GTK_RESPONSE_OK);
    EXPECT_TRUE (gncInvoiceIsPosted (fixture.invoice));
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
    gnucash_register_add_cell_types ();
    create_window_sentinel ();
    gnc::test::initialize_logging ();
    auto result = RUN_ALL_TESTS ();
    destroy_window_sentinel ();
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
