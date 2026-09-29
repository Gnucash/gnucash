/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include <cstdlib>

#include "Account.h"
#include "cashobjects.h"
#include "gncEntryLedger.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gncInvoice.h"
#include "gncTaxTable.h"
#include "gnc-session.h"
#include "combocell.h"
#include "gnucash-register.h"
#include "qof.h"

namespace
{
gboolean display_available;

struct Fixture
{
    QofSession *session{};
    QofBook *book{};
    GncInvoice *invoice{};
    GncEntryLedger *ledger{};
    GtkWidget *parent{};
};

Fixture
make_fixture ()
{
    Fixture fixture{};
    fixture.book = qof_book_new ();
    if (!gnc_book_get_root_account (fixture.book))
        gnc_account_create_root (fixture.book);
    fixture.session = qof_session_new (fixture.book);
    gnc_set_current_session (fixture.session);
    fixture.invoice = gncInvoiceCreate (fixture.book);
    fixture.ledger = gnc_entry_ledger_new (fixture.book, GNCENTRY_INVOICE_ENTRY);
    fixture.parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_realize (fixture.parent);
    gnc_entry_ledger_set_parent (fixture.ledger, fixture.parent);
    gnc_entry_ledger_set_default_invoice (fixture.ledger, fixture.invoice);
    return fixture;
}

GtkWidget *
find_confirmation ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_MESSAGE_DIALOG (node->data))
        {
            g_assert_null (found);
            found = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return found;
}

GtkWidget *
find_new_account_dialog ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        const gchar *title = gtk_window_get_title (GTK_WINDOW (widget));
        if (GTK_IS_DIALOG (widget) && title &&
            g_str_has_prefix (title, "New Account"))
        {
            g_assert_null (found);
            found = widget;
        }
    }
    g_list_free (windows);
    return found;
}

void
edit_description (Fixture &fixture, const gchar *value)
{
    auto table = gnc_entry_ledger_get_table (fixture.ledger);
    auto cell = static_cast<BasicCell *> (
        gnc_table_layout_get_cell (table->layout, ENTRY_DESC_CELL));
    g_assert_nonnull (cell);
    gnc_basic_cell_set_value (cell, value);
    gnc_basic_cell_set_changed (cell, TRUE);
}

void
completed (gboolean accepted, gpointer data)
{
    auto result = static_cast<gboolean *> (data);
    *result = accepted ? 1 : -1;
}

void
finish_fixture (Fixture &fixture)
{
    if (fixture.ledger)
        gnc_entry_ledger_destroy (fixture.ledger);
    if (fixture.parent)
        gtk_widget_destroy (fixture.parent);
    gnc_clear_current_session ();
}

void
test_save_confirmation_commits_before_completion ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto fixture = make_fixture ();
    edit_description (fixture, "Saved through async close");
    gboolean result = 0;
    gnc_entry_ledger_check_close_async (fixture.parent, fixture.ledger,
                                        completed, &result);
    auto dialog = find_confirmation ();
    g_assert_nonnull (dialog);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);
    g_assert_cmpint (result, ==, 1);
    auto entries = gncInvoiceGetEntries (fixture.invoice);
    g_assert_nonnull (entries);
    g_assert_cmpstr (gncEntryGetDescription (static_cast<GncEntry *>(entries->data)), ==,
                     "Saved through async close");
    finish_fixture (fixture);
}

void
test_destroyed_parent_aborts_once ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto fixture = make_fixture ();
    edit_description (fixture, "Must not be saved");
    gint result = 0;
    gnc_entry_ledger_check_close_async (fixture.parent, fixture.ledger,
                                        completed, &result);
    auto dialog = find_confirmation ();
    g_assert_nonnull (dialog);
    g_object_ref (dialog);
    gtk_widget_destroy (fixture.parent);
    g_assert_cmpint (result, ==, -1);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);
    g_assert_cmpint (result, ==, -1);
    g_object_unref (dialog);
    gnc_entry_ledger_destroy (fixture.ledger);
    fixture.ledger = nullptr;
    fixture.parent = nullptr;
    gnc_clear_current_session ();
}

void
test_session_switch_aborts_save ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto fixture = make_fixture ();
    edit_description (fixture, "Must not cross sessions");
    gint result = 0;
    gnc_entry_ledger_check_close_async (fixture.parent, fixture.ledger,
                                        completed, &result);
    auto dialog = find_confirmation ();
    g_assert_nonnull (dialog);
    auto other_session = qof_session_new (qof_book_new ());
    gnc_set_current_session (other_session);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);
    g_assert_cmpint (result, ==, -1);
    g_assert_null (gncInvoiceGetEntries (fixture.invoice));
    gnc_set_current_session (fixture.session);
    finish_fixture (fixture);
}

void
test_no_changes_completes_immediately ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto fixture = make_fixture ();
    gint result = 0;
    gnc_entry_ledger_check_close_async (fixture.parent, fixture.ledger,
                                        completed, &result);
    g_assert_cmpint (result, ==, 1);
    g_assert_null (find_confirmation ());
    finish_fixture (fixture);
}

void
test_account_creation_cancel_aborts_close ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto fixture = make_fixture ();
    auto table = gnc_entry_ledger_get_table (fixture.ledger);
    auto cell = reinterpret_cast<ComboCell *> (
        gnc_table_layout_get_cell (table->layout, ENTRY_IACCT_CELL));
    g_assert_nonnull (cell);
    gnc_combo_cell_set_value (cell, "New account from ledger test");
    gnc_basic_cell_set_changed (&cell->cell, TRUE);
    gint result = 0;
    gnc_entry_ledger_check_close_async (fixture.parent, fixture.ledger,
                                        completed, &result);
    auto confirmation = find_confirmation ();
    g_assert_nonnull (confirmation);
    gtk_dialog_response (GTK_DIALOG (confirmation), GTK_RESPONSE_YES);
    auto dialog = find_new_account_dialog ();
    g_assert_nonnull (dialog);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);
    g_assert_cmpint (result, ==, -1);
    g_assert_null (gncInvoiceGetEntries (fixture.invoice));
    finish_fixture (fixture);
}

Account *
make_invoice_account (QofBook *book, const gchar *name)
{
    auto account = xaccMallocAccount (book);
    xaccAccountSetName (account, name);
    xaccAccountSetType (account, ACCT_TYPE_INCOME);
    gnc_account_append_child (gnc_book_get_root_account (book), account);
    return account;
}

void
test_account_removed_during_save_confirmation_reenters_async_creation ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto fixture = make_fixture ();
    auto account = make_invoice_account (fixture.book, "Account removed while saving");
    auto table = gnc_entry_ledger_get_table (fixture.ledger);
    auto cell = reinterpret_cast<ComboCell *> (
        gnc_table_layout_get_cell (table->layout, ENTRY_IACCT_CELL));
    gnc_combo_cell_set_value (cell, "Account removed while saving");
    gnc_basic_cell_set_changed (&cell->cell, TRUE);
    gint result = 0;
    gnc_entry_ledger_check_close_async (fixture.parent, fixture.ledger,
                                        completed, &result);
    auto save_dialog = find_confirmation ();
    g_assert_nonnull (save_dialog);
    xaccAccountBeginEdit (account);
    xaccAccountDestroy (account);
    gtk_dialog_response (GTK_DIALOG (save_dialog), GTK_RESPONSE_YES);
    auto create_prompt = find_confirmation ();
    g_assert_nonnull (create_prompt);
    gtk_dialog_response (GTK_DIALOG (create_prompt), GTK_RESPONSE_YES);
    auto create_editor = find_new_account_dialog ();
    g_assert_nonnull (create_editor);
    gtk_dialog_response (GTK_DIALOG (create_editor), GTK_RESPONSE_CANCEL);
    g_assert_cmpint (result, ==, -1);
    g_assert_null (gncInvoiceGetEntries (fixture.invoice));
    finish_fixture (fixture);
}

void
test_tax_table_removed_during_save_confirmation_reprompts_then_saves ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto fixture = make_fixture ();
    auto tax_table = gncTaxTableCreate (fixture.book);
    gncTaxTableSetName (tax_table, "Tax table removed while saving");
    auto table = gnc_entry_ledger_get_table (fixture.ledger);
    auto cell = reinterpret_cast<ComboCell *> (
        gnc_table_layout_get_cell (table->layout, ENTRY_TAXTABLE_CELL));
    gnc_combo_cell_set_value (cell, "Tax table removed while saving");
    gnc_basic_cell_set_changed (&cell->cell, TRUE);
    gint result = 0;
    gnc_entry_ledger_check_close_async (fixture.parent, fixture.ledger,
                                        completed, &result);
    auto save_dialog = find_confirmation ();
    g_assert_nonnull (save_dialog);
    gncTaxTableBeginEdit (tax_table);
    gncTaxTableDestroy (tax_table);
    gtk_dialog_response (GTK_DIALOG (save_dialog), GTK_RESPONSE_YES);
    auto create_prompt = find_confirmation ();
    g_assert_nonnull (create_prompt);
    gtk_dialog_response (GTK_DIALOG (create_prompt), GTK_RESPONSE_NO);
    g_assert_cmpint (result, ==, 1);
    g_assert_nonnull (gncInvoiceGetEntries (fixture.invoice));
    finish_fixture (fixture);
}
}

static int
run_tests (int argc, char **argv)
{
    g_setenv ("GNC_UNINSTALLED", "YES", TRUE);
    g_setenv ("GSETTINGS_BACKEND", "memory", TRUE);
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_gsettings_load_backend ();
    gnc_component_manager_init ();
    gnucash_register_add_cell_types ();
    g_test_add_func ("/ledger/close-async/commit-before-completion",
                     test_save_confirmation_commits_before_completion);
    g_test_add_func ("/ledger/close-async/account-drift-restarts-async-creation",
                     test_account_removed_during_save_confirmation_reenters_async_creation);
    g_test_add_func ("/ledger/close-async/tax-table-drift-reprompts",
                     test_tax_table_removed_during_save_confirmation_reprompts_then_saves);
    g_test_add_func ("/ledger/close-async/destroyed-parent-aborts-once",
                     test_destroyed_parent_aborts_once);
    g_test_add_func ("/ledger/close-async/session-switch-aborts-save",
                     test_session_switch_aborts_save);
    g_test_add_func ("/ledger/close-async/unchanged-completes-immediately",
                     test_no_changes_completes_immediately);
    g_test_add_func ("/ledger/close-async/account-creation-cancel-aborts",
                     test_account_creation_cancel_aborts_close);
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
