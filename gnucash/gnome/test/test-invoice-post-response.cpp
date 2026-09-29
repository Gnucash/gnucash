/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>
#include <libguile.h>
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
#include "gnucash-register.h"

namespace
{
gboolean display_available;

struct Fixture
{
    QofSession *session{};
    QofBook *book{};
    GncMainWindow *window{};
    InvoiceWindow *invoice_window{};
    GncInvoice *invoice{};
};

Fixture
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
    g_assert_nonnull (f.invoice_window);
    return f;
}

GtkWidget *
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

GtkWidget *
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

void
wait_for_post_dialog ()
{
    for (guint i = 0; i < 2000 && !find_post_dialog (); ++i)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        g_usleep (1000);
    }
    g_assert_nonnull (find_post_dialog ());
}

void
finish_fixture (Fixture &f)
{
    gtk_widget_destroy (GTK_WIDGET (f.window));
    while (g_main_context_iteration (nullptr, FALSE))
        ;
    g_object_unref (f.window);
    gnc_clear_current_session ();
}

void
start_post_with_two_foreign_currencies (Fixture &f)
{
    auto foreign = gncInvoiceGetForeignCurrencies (f.invoice);
    g_assert_cmpuint (g_hash_table_size (foreign), ==, 2);
    g_hash_table_unref (foreign);
    gnc_invoice_window_postCB (nullptr, f.invoice_window);
    wait_for_post_dialog ();
    g_assert_cmpint (qof_instance_get_editlevel (f.invoice), ==, 0);
    g_assert_false (gnc_gui_refresh_suspended ());
    g_assert_false (gncInvoiceIsPosted (f.invoice));
}

void
test_cancel_post_does_not_post_invoice ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto f = make_fixture ();
    start_post_with_two_foreign_currencies (f);
    gtk_dialog_response (GTK_DIALOG (find_post_dialog ()), GTK_RESPONSE_CANCEL);
    g_assert_false (gncInvoiceIsPosted (f.invoice));
    g_assert_cmpint (qof_instance_get_editlevel (f.invoice), ==, 0);
    g_assert_false (gnc_gui_refresh_suspended ());
    finish_fixture (f);
}

void
test_parent_destroy_prevents_late_post_response ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto f = make_fixture ();
    start_post_with_two_foreign_currencies (f);
    auto dialog = find_post_dialog ();
    g_object_ref (dialog);
    auto ok_button = find_widget(dialog, "okbutton1");
    g_assert_nonnull(ok_button);
    g_object_ref(ok_button);
    gtk_widget_destroy (GTK_WIDGET (f.window));
    gtk_button_clicked(GTK_BUTTON(ok_button));
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_false (gncInvoiceIsPosted (f.invoice));
    g_assert_cmpint (qof_instance_get_editlevel (f.invoice), ==, 0);
    g_assert_false (gnc_gui_refresh_suspended ());
    g_object_unref(ok_button);
    g_object_unref (dialog);
    g_object_unref (f.window);
    gnc_clear_current_session ();
}

GtkWidget *find_transfer_dialog()
{
    auto windows = gtk_window_list_toplevels();
    GtkWidget *found = nullptr;
    for (auto item = windows; item; item = item->next)
        if (!g_strcmp0(gtk_widget_get_name(GTK_WIDGET(item->data)), "gnc-id-transfer"))
            found = GTK_WIDGET(item->data);
    g_list_free(windows);
    return found;
}

void wait_for_transfer_dialog()
{
    const auto deadline = g_get_monotonic_time() + 2 * G_TIME_SPAN_SECOND;
    while (!find_transfer_dialog() && g_get_monotonic_time() < deadline)
    {
        while (g_main_context_iteration(nullptr, FALSE));
        g_usleep(1000);
    }
    g_assert_nonnull(find_transfer_dialog());
}

void test_currency_responses(gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip("No graphical display is available");
        return;
    }
    const auto action = GPOINTER_TO_INT(data);
    auto f = make_fixture();
    start_post_with_two_foreign_currencies(f);
    auto post_ok = find_widget(find_post_dialog(), "okbutton1");
    g_assert_nonnull(post_ok);
    gtk_button_clicked(GTK_BUTTON(post_ok));
    wait_for_transfer_dialog();
    auto first = find_transfer_dialog();
    g_object_ref(first);
    g_assert_cmpint(qof_instance_get_editlevel(f.invoice), ==, 0);
    g_assert_false(gnc_gui_refresh_suspended());
    g_assert_false(gncInvoiceIsPosted(f.invoice));
    if (action == 0)
        gtk_dialog_response(GTK_DIALOG(first), GTK_RESPONSE_CANCEL);
    else if (action == 1)
        gtk_widget_destroy(GTK_WIDGET(f.window));
    else
    {
        auto price_box = find_widget(first, "price_hbox");
        g_assert_nonnull(price_box);
        auto children = gtk_container_get_children(GTK_CONTAINER(price_box));
        g_assert_nonnull(children);
        gnc_amount_edit_set_amount(GNC_AMOUNT_EDIT(children->data), gnc_numeric_create(2, 1));
        g_list_free(children);
        gtk_dialog_response(GTK_DIALOG(first), GTK_RESPONSE_OK);
        wait_for_transfer_dialog();
        auto second = find_transfer_dialog();
        g_assert_true(second != first);
        g_assert_false(gncInvoiceIsPosted(f.invoice));
        g_assert_cmpint(qof_instance_get_editlevel(f.invoice), ==, 0);
        g_assert_false(gnc_gui_refresh_suspended());
        price_box = find_widget(second, "price_hbox");
        children = gtk_container_get_children(GTK_CONTAINER(price_box));
        gnc_amount_edit_set_amount(GNC_AMOUNT_EDIT(children->data), gnc_numeric_create(3, 1));
        g_list_free(children);
        gtk_dialog_response(GTK_DIALOG(second), GTK_RESPONSE_OK);
    }
    while (g_main_context_iteration(nullptr, FALSE));
    g_assert_cmpint(gncInvoiceIsPosted(f.invoice), ==, action == 2);
    g_assert_cmpint(qof_instance_get_editlevel(f.invoice), ==, 0);
    g_assert_false(gnc_gui_refresh_suspended());
    gtk_dialog_response(GTK_DIALOG(first), GTK_RESPONSE_OK);
    g_assert_cmpint(gncInvoiceIsPosted(f.invoice), ==, action == 2);
    g_object_unref(first);
    finish_fixture(f);
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
    gnucash_register_add_cell_types ();
    g_test_add_func ("/gnome/invoice-post/cancel-with-two-foreign-currencies",
                     test_cancel_post_does_not_post_invoice);
    g_test_add_func ("/gnome/invoice-post/parent-destroy-late-response",
                     test_parent_destroy_prevents_late_post_response);
    g_test_add_data_func("/gnome/invoice-post/currency-cancel", GINT_TO_POINTER(0), test_currency_responses);
    g_test_add_data_func("/gnome/invoice-post/currency-parent-destroy", GINT_TO_POINTER(1), test_currency_responses);
    g_test_add_data_func("/gnome/invoice-post/currency-accept-sequential", GINT_TO_POINTER(2), test_currency_responses);
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
