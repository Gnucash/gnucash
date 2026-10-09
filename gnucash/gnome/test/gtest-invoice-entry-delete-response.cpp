/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include "googletest-glib-log-handler.hpp"
#include <cstdint>
#include <gtk/gtk.h>
#include "test/gnome-response-test-fixture.h"
#include <libguile.h>
#include <cstdlib>
#include <vector>

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
#include "gnucash-register.h"

namespace
{

struct InvoiceFixture
{
    QofSession *session{};
    QofBook *book{};
    GncMainWindow *main_window{};
    InvoiceWindow *invoice_window{};
    GncInvoice *invoice{};
    GncEntry *entries[2]{};
    GncGUID entry_guids[2]{};
    std::uint32_t entry_count{};
};

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

static InvoiceFixture
make_fixture (std::uint32_t entry_count)
{
    InvoiceFixture fixture{};
    fixture.entry_count = entry_count;
    fixture.book = qof_book_new ();
    fixture.session = qof_session_new (fixture.book);
    gnc_set_current_session (fixture.session);

    auto table = gnc_commodity_table_get_table (fixture.book);
    gnc_commodity_table_add_namespace (table, "CURRENCY", fixture.book);
    auto currency = gnc_commodity_new (fixture.book, "Test currency",
                                       "CURRENCY", "TST", nullptr, 100);
    gnc_commodity_table_insert (table, currency);

    auto root = gnc_account_create_root (fixture.book);
    auto receivable = xaccMallocAccount (fixture.book);
    xaccAccountSetName (receivable, "Test receivable");
    xaccAccountSetType (receivable, ACCT_TYPE_RECEIVABLE);
    xaccAccountSetCommodity (receivable, currency);
    gnc_account_append_child (root, receivable);
    auto income = xaccMallocAccount (fixture.book);
    xaccAccountSetName (income, "Test income");
    xaccAccountSetType (income, ACCT_TYPE_INCOME);
    xaccAccountSetCommodity (income, currency);
    gnc_account_append_child (root, income);

    auto customer = gncCustomerCreate (fixture.book);
    gncCustomerSetID (customer, "TEST-CUSTOMER");
    gncCustomerSetName (customer, "Test customer");
    gncCustomerSetCurrency (customer, currency);
    GncOwner owner{};
    gncOwnerInitCustomer (&owner, customer);

    fixture.invoice = gncInvoiceCreate (fixture.book);
    gncInvoiceSetID (fixture.invoice, "TEST-INVOICE");
    gncInvoiceSetOwner (fixture.invoice, &owner);
    gncInvoiceSetCurrency (fixture.invoice, currency);

    for (std::uint32_t i = 0; i < entry_count; ++i)
    {
        auto entry = gncEntryCreate (fixture.book);
        gncEntrySetDate (entry, gnc_time (nullptr));
        gncEntrySetDateEntered (entry, gnc_time (nullptr));
        gncEntrySetDescription (entry, i == 0 ? "Entry one" : "Entry two");
        gncEntrySetQuantity (entry, gnc_numeric_create (1, 1));
        gncEntrySetInvPrice (entry, gnc_numeric_create (10 + i, 1));
        gncEntrySetInvAccount (entry, income);
        gncInvoiceAddEntry (fixture.invoice, entry);
        fixture.entries[i] = entry;
        fixture.entry_guids[i] = *gncEntryGetGUID (entry);
    }

    fixture.main_window = gnc_main_window_new ();
    g_object_ref_sink (fixture.main_window);
    fixture.invoice_window = gnc_ui_invoice_edit (
        GTK_WINDOW (fixture.main_window), fixture.invoice);
    EXPECT_NE (fixture.invoice_window, nullptr);
    if (!fixture.invoice_window)
        return fixture;

    /* The entry-ledger constructor starts at virtual row 1, column 0.
     * Re-select that row through the actual register widget so each test
     * begins on the first persisted invoice entry, not the blank row. */
    auto reg = gnc_invoice_get_register (fixture.invoice_window);
    EXPECT_TRUE (GNUCASH_IS_REGISTER (reg));
    if (!GNUCASH_IS_REGISTER (reg))
        return fixture;
    VirtualCellLocation first_entry{1, 0};
    gnucash_register_goto_virt_cell (GNUCASH_REGISTER (reg), first_entry);
    return fixture;
}

static GtkWidget *
find_confirmation (GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (GTK_IS_MESSAGE_DIALOG (widget) &&
            gtk_window_get_transient_for (GTK_WINDOW (widget)) == parent)
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

static void
request_delete (InvoiceFixture &fixture)
{
    gnc_invoice_window_deleteCB (nullptr, fixture.invoice_window);
}

static void
finish_fixture (InvoiceFixture &fixture)
{
    if (!fixture.main_window)
        return;
    gtk_widget_destroy (GTK_WIDGET (fixture.main_window));
    while (g_main_context_iteration (nullptr, FALSE))
        ;
    g_object_unref (fixture.main_window);
}

static bool
entry_exists (InvoiceFixture &fixture, std::uint32_t index)
{
    return gncEntryLookup (fixture.book, &fixture.entry_guids[index]) != nullptr;
}

class InvoiceEntryDeleteResponseTest : public GnomeResponseTest
{
protected:
    void SetUp () override
    {
        GnomeResponseTest::SetUp ();
        fixture = make_fixture (1);
    }
    void TearDown () override
    {
        finish_fixture (fixture);
        GnomeResponseTest::TearDown ();
        for (auto widget : retained_widgets)
            g_object_unref (widget);
        gnc_clear_current_session ();
    }
    InvoiceFixture fixture{};
    std::vector<GtkWidget *> retained_widgets;
};

class InvoiceEntryDeleteTwoEntryTest : public InvoiceEntryDeleteResponseTest
{
protected:
    void SetUp () override
    {
        GnomeResponseTest::SetUp ();
        fixture = make_fixture (2);
    }
};

TEST_F (InvoiceEntryDeleteResponseTest, NoKeepsEntry)
{
    request_delete (fixture);
    auto dialog = find_confirmation (GTK_WINDOW (fixture.main_window));
    ASSERT_NE (dialog, nullptr);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_NO);
    EXPECT_TRUE (entry_exists (fixture, 0));
    EXPECT_EQ (g_list_length (gncInvoiceGetEntries (fixture.invoice)), 1u);
}

TEST_F (InvoiceEntryDeleteResponseTest, YesDeletesOriginalEntry)
{
    request_delete (fixture);
    auto dialog = find_confirmation (GTK_WINDOW (fixture.main_window));
    ASSERT_NE (dialog, nullptr);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);
    EXPECT_FALSE (entry_exists (fixture, 0));
    EXPECT_EQ (g_list_length (gncInvoiceGetEntries (fixture.invoice)), 0u);
}

TEST_F (InvoiceEntryDeleteResponseTest, PageDestroyIgnoresLateYes)
{
    request_delete (fixture);
    auto dialog = find_confirmation (GTK_WINDOW (fixture.main_window));
    ASSERT_NE (dialog, nullptr);
    g_object_ref (dialog);
    retained_widgets.push_back (dialog);
    auto page = gnc_main_window_get_current_page (fixture.main_window);
    ASSERT_NE (page, nullptr);
    gnc_main_window_close_page (page);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);
    EXPECT_TRUE (entry_exists (fixture, 0));
    EXPECT_EQ (g_list_length (gncInvoiceGetEntries (fixture.invoice)), 1u);
}

TEST_F (InvoiceEntryDeleteTwoEntryTest, SelectionDriftRejectsOldConfirmation)
{
    request_delete (fixture);
    auto dialog = find_confirmation (GTK_WINDOW (fixture.main_window));
    ASSERT_NE (dialog, nullptr);
    auto reg = GNUCASH_REGISTER (gnc_invoice_get_register (
        fixture.invoice_window));
    gnucash_register_goto_next_virt_row (reg);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);
    EXPECT_TRUE (entry_exists (fixture, 0));
    EXPECT_TRUE (entry_exists (fixture, 1));
    EXPECT_EQ (g_list_length (gncInvoiceGetEntries (fixture.invoice)), 2u);
}
}

static int
run_tests (int argc, char **argv)
{
    /* CTest supplies build paths and an isolated memory preference backend. */
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
