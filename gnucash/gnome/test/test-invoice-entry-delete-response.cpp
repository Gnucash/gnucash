/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include <cstdlib>

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
gboolean display_available;

struct InvoiceFixture
{
    QofSession *session{};
    QofBook *book{};
    GncMainWindow *main_window{};
    InvoiceWindow *invoice_window{};
    GncInvoice *invoice{};
    GncEntry *entries[2]{};
    GncGUID entry_guids[2]{};
    guint entry_count{};
};

InvoiceFixture
make_fixture (guint entry_count)
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

    for (guint i = 0; i < entry_count; ++i)
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
    g_assert_nonnull (fixture.invoice_window);

    /* The entry-ledger constructor starts at virtual row 1, column 0.
     * Re-select that row through the actual register widget so each test
     * begins on the first persisted invoice entry, not the blank row. */
    auto reg = gnc_invoice_get_register (fixture.invoice_window);
    g_assert_true (GNUCASH_IS_REGISTER (reg));
    VirtualCellLocation first_entry{1, 0};
    gnucash_register_goto_virt_cell (GNUCASH_REGISTER (reg), first_entry);
    return fixture;
}

GtkWidget *
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
            g_assert_null (result);
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

void
request_delete (InvoiceFixture &fixture)
{
    gnc_invoice_window_deleteCB (nullptr, fixture.invoice_window);
    g_assert_nonnull (find_confirmation (GTK_WINDOW (fixture.main_window)));
}

void
finish_fixture (InvoiceFixture &fixture)
{
    gtk_widget_destroy (GTK_WIDGET (fixture.main_window));
    while (g_main_context_iteration (nullptr, FALSE))
        ;
    g_object_unref (fixture.main_window);
    gnc_clear_current_session ();
}

gboolean
entry_exists (InvoiceFixture &fixture, guint index)
{
    return gncEntryLookup (fixture.book, &fixture.entry_guids[index]) != nullptr;
}

void
test_no_keeps_entry ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto fixture = make_fixture (1);
    request_delete (fixture);
    gtk_dialog_response (GTK_DIALOG (find_confirmation (
        GTK_WINDOW (fixture.main_window))), GTK_RESPONSE_NO);
    g_assert_true (entry_exists (fixture, 0));
    g_assert_cmpuint (g_list_length (gncInvoiceGetEntries (fixture.invoice)), ==, 1);
    finish_fixture (fixture);
}

void
test_yes_deletes_original_entry ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto fixture = make_fixture (1);
    request_delete (fixture);
    gtk_dialog_response (GTK_DIALOG (find_confirmation (
        GTK_WINDOW (fixture.main_window))), GTK_RESPONSE_YES);
    g_assert_false (entry_exists (fixture, 0));
    g_assert_cmpuint (g_list_length (gncInvoiceGetEntries (fixture.invoice)), ==, 0);
    finish_fixture (fixture);
}

void
test_page_destroy_then_late_yes_does_not_delete ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto fixture = make_fixture (1);
    request_delete (fixture);
    auto dialog = find_confirmation (GTK_WINDOW (fixture.main_window));
    g_object_ref (dialog);
    auto page = gnc_main_window_get_current_page (fixture.main_window);
    g_assert_nonnull (page);
    gnc_main_window_close_page (page);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);
    g_assert_true (entry_exists (fixture, 0));
    g_assert_cmpuint (g_list_length (gncInvoiceGetEntries (fixture.invoice)), ==, 1);
    g_object_unref (dialog);
    finish_fixture (fixture);
}

void
test_selection_drift_rejects_old_confirmation ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto fixture = make_fixture (2);
    request_delete (fixture);
    auto reg = GNUCASH_REGISTER (gnc_invoice_get_register (
        fixture.invoice_window));
    gnucash_register_goto_next_virt_row (reg);
    gtk_dialog_response (GTK_DIALOG (find_confirmation (
        GTK_WINDOW (fixture.main_window))), GTK_RESPONSE_YES);
    g_assert_true (entry_exists (fixture, 0));
    g_assert_true (entry_exists (fixture, 1));
    g_assert_cmpuint (g_list_length (gncInvoiceGetEntries (fixture.invoice)), ==, 2);
    finish_fixture (fixture);
}
}

static int
run_tests (int argc, char **argv)
{
    /* CTest supplies build paths and an isolated memory preference backend. */
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);

    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    gnucash_register_add_cell_types ();

    g_test_add_func ("/gnome/invoice-entry-delete/no-keeps-entry",
                     test_no_keeps_entry);
    g_test_add_func ("/gnome/invoice-entry-delete/yes-deletes-original",
                     test_yes_deletes_original_entry);
    g_test_add_func ("/gnome/invoice-entry-delete/page-destroy-late-response",
                     test_page_destroy_then_late_yes_does_not_delete);
    g_test_add_func ("/gnome/invoice-entry-delete/selection-drift",
                     test_selection_drift_rejects_old_confirmation);

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
