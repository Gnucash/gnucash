/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <cstdint>
#include <gtk/gtk.h>
#include <libguile.h>
#include <cstdlib>
#include <gtest/gtest.h>
#include "googletest-glib-log-handler.hpp"

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
class EntryLedgerCloseTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        book = qof_book_new ();
        ASSERT_NE (book, nullptr);
        if (!gnc_book_get_root_account (book))
            gnc_account_create_root (book);
        session = qof_session_new (book);
        ASSERT_NE (session, nullptr);
        gnc_set_current_session (session);
        invoice = gncInvoiceCreate (book);
        ASSERT_NE (invoice, nullptr);
        ledger = gnc_entry_ledger_new (book, GNCENTRY_INVOICE_ENTRY);
        ASSERT_NE (ledger, nullptr);
        parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        g_object_ref_sink (parent);
        gtk_widget_realize (parent);
        gnc_entry_ledger_set_parent (ledger, parent);
        gnc_entry_ledger_set_default_invoice (ledger, invoice);
    }

    void TearDown () override
    {
        if (result == 0 && parent)
        {
            gtk_widget_destroy (parent);
            const std::int64_t deadline =
                g_get_monotonic_time () + 5 * G_TIME_SPAN_SECOND;
            while (result == 0 && g_get_monotonic_time () < deadline)
            {
                while (g_main_context_iteration (nullptr, FALSE))
                    ;
                g_usleep (1000);
            }
        }
        if (ledger)
            gnc_entry_ledger_destroy (ledger);
        if (parent)
        {
            gtk_widget_destroy (parent);
            g_object_unref (parent);
        }
        auto current = gnc_exchange_current_session (nullptr);
        if (current)
            qof_session_destroy (current);
        if (session && session != current)
            qof_session_destroy (session);
        if (other_session && other_session != current &&
            other_session != session)
            qof_session_destroy (other_session);
    }

    QofSession *session{};
    QofBook *book{};
    GncInvoice *invoice{};
    GncEntryLedger *ledger{};
    GtkWidget *parent{};
    QofSession *other_session{};
    std::int32_t result{};
};

static GtkWidget *
find_confirmation ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_MESSAGE_DIALOG (node->data))
        {
            EXPECT_EQ (found, nullptr);
            found = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return found;
}

static GtkWidget *
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
            EXPECT_EQ (found, nullptr);
            found = widget;
        }
    }
    g_list_free (windows);
    return found;
}

static void
completed (gboolean accepted, gpointer data)
{
    auto result = static_cast<std::int32_t *> (data);
    *result = accepted ? 1 : -1;
}

TEST_F (EntryLedgerCloseTest, AcceptedCloseCommitsBeforeCompletion)
{
    auto table = gnc_entry_ledger_get_table (ledger);
    auto cell = static_cast<BasicCell *>(
        gnc_table_layout_get_cell (table->layout, ENTRY_DESC_CELL));
    ASSERT_NE (cell, nullptr);
    gnc_basic_cell_set_value (cell, "Saved through async close");
    gnc_basic_cell_set_changed (cell, TRUE);
    gnc_entry_ledger_check_close_async (parent, ledger,
                                        completed, &result);
    auto dialog = find_confirmation ();
    ASSERT_NE (dialog, nullptr);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);
    EXPECT_EQ (result, 1);
    auto entries = gncInvoiceGetEntries (invoice);
    ASSERT_NE (entries, nullptr);
    EXPECT_STREQ (gncEntryGetDescription (static_cast<GncEntry *>(entries->data)),
                  "Saved through async close");
}

TEST_F (EntryLedgerCloseTest, DestroyedParentAbortsOnce)
{
    auto table = gnc_entry_ledger_get_table (ledger);
    auto cell = static_cast<BasicCell *>(
        gnc_table_layout_get_cell (table->layout, ENTRY_DESC_CELL));
    ASSERT_NE (cell, nullptr);
    gnc_basic_cell_set_value (cell, "Must not be saved");
    gnc_basic_cell_set_changed (cell, TRUE);
    gnc_entry_ledger_check_close_async (parent, ledger,
                                        completed, &result);
    auto dialog = find_confirmation ();
    ASSERT_NE (dialog, nullptr);
    g_object_ref (dialog);
    gtk_widget_destroy (parent);
    EXPECT_EQ (result, -1);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);
    EXPECT_EQ (result, -1);
    g_object_unref (dialog);
    gnc_entry_ledger_destroy (ledger);
    ledger = nullptr;
    g_object_unref (parent);
    parent = nullptr;
}

TEST_F (EntryLedgerCloseTest, SessionSwitchAbortsSave)
{
    auto table = gnc_entry_ledger_get_table (ledger);
    auto cell = static_cast<BasicCell *> (
        gnc_table_layout_get_cell (table->layout, ENTRY_DESC_CELL));
    ASSERT_NE (cell, nullptr);
    gnc_basic_cell_set_value (cell, "Must not cross sessions");
    gnc_basic_cell_set_changed (cell, TRUE);
    gnc_entry_ledger_check_close_async (parent, ledger,
                                        completed, &result);
    auto dialog = find_confirmation ();
    ASSERT_NE (dialog, nullptr);
    other_session = qof_session_new (qof_book_new ());
    EXPECT_EQ (gnc_exchange_current_session (other_session), session);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);
    EXPECT_EQ (result, -1);
    EXPECT_EQ (gncInvoiceGetEntries (invoice), nullptr);
    EXPECT_EQ (gnc_exchange_current_session (session), other_session);
}

TEST_F (EntryLedgerCloseTest, UnchangedLedgerCompletesImmediately)
{
    gnc_entry_ledger_check_close_async (parent, ledger,
                                        completed, &result);
    EXPECT_EQ (result, 1);
    EXPECT_EQ (find_confirmation (), nullptr);
}

TEST_F (EntryLedgerCloseTest, AccountCreationCancelAbortsClose)
{
    auto table = gnc_entry_ledger_get_table (ledger);
    auto cell = reinterpret_cast<ComboCell *> (
        gnc_table_layout_get_cell (table->layout, ENTRY_IACCT_CELL));
    ASSERT_NE (cell, nullptr);
    gnc_combo_cell_set_value (cell, "New account from ledger test");
    gnc_basic_cell_set_changed (&cell->cell, TRUE);
    gnc_entry_ledger_check_close_async (parent, ledger,
                                        completed, &result);
    auto confirmation = find_confirmation ();
    ASSERT_NE (confirmation, nullptr);
    gtk_dialog_response (GTK_DIALOG (confirmation), GTK_RESPONSE_YES);
    auto dialog = find_new_account_dialog ();
    ASSERT_NE (dialog, nullptr);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);
    EXPECT_EQ (result, -1);
    EXPECT_EQ (gncInvoiceGetEntries (invoice), nullptr);
}

static Account *
make_invoice_account (QofBook *book, const gchar *name)
{
    auto account = xaccMallocAccount (book);
    xaccAccountSetName (account, name);
    xaccAccountSetType (account, ACCT_TYPE_INCOME);
    gnc_account_append_child (gnc_book_get_root_account (book), account);
    return account;
}

TEST_F (EntryLedgerCloseTest, RemovedAccountRestartsAsyncCreation)
{
    auto account = make_invoice_account (book, "Account removed while saving");
    auto table = gnc_entry_ledger_get_table (ledger);
    auto cell = reinterpret_cast<ComboCell *> (
        gnc_table_layout_get_cell (table->layout, ENTRY_IACCT_CELL));
    gnc_combo_cell_set_value (cell, "Account removed while saving");
    gnc_basic_cell_set_changed (&cell->cell, TRUE);
    gnc_entry_ledger_check_close_async (parent, ledger,
                                        completed, &result);
    auto save_dialog = find_confirmation ();
    ASSERT_NE (save_dialog, nullptr);
    xaccAccountBeginEdit (account);
    xaccAccountDestroy (account);
    gtk_dialog_response (GTK_DIALOG (save_dialog), GTK_RESPONSE_YES);
    auto create_prompt = find_confirmation ();
    ASSERT_NE (create_prompt, nullptr);
    gtk_dialog_response (GTK_DIALOG (create_prompt), GTK_RESPONSE_YES);
    auto create_editor = find_new_account_dialog ();
    ASSERT_NE (create_editor, nullptr);
    gtk_dialog_response (GTK_DIALOG (create_editor), GTK_RESPONSE_CANCEL);
    EXPECT_EQ (result, -1);
    EXPECT_EQ (gncInvoiceGetEntries (invoice), nullptr);
}

TEST_F (EntryLedgerCloseTest, RemovedTaxTablePromptsAgainThenSaves)
{
    auto tax_table = gncTaxTableCreate (book);
    gncTaxTableSetName (tax_table, "Tax table removed while saving");
    auto table = gnc_entry_ledger_get_table (ledger);
    auto cell = reinterpret_cast<ComboCell *> (
        gnc_table_layout_get_cell (table->layout, ENTRY_TAXTABLE_CELL));
    gnc_combo_cell_set_value (cell, "Tax table removed while saving");
    gnc_basic_cell_set_changed (&cell->cell, TRUE);
    gnc_entry_ledger_check_close_async (parent, ledger,
                                        completed, &result);
    auto save_dialog = find_confirmation ();
    ASSERT_NE (save_dialog, nullptr);
    gncTaxTableBeginEdit (tax_table);
    gncTaxTableDestroy (tax_table);
    gtk_dialog_response (GTK_DIALOG (save_dialog), GTK_RESPONSE_YES);
    auto create_prompt = find_confirmation ();
    ASSERT_NE (create_prompt, nullptr);
    gtk_dialog_response (GTK_DIALOG (create_prompt), GTK_RESPONSE_NO);
    EXPECT_EQ (result, 1);
    EXPECT_NE (gncInvoiceGetEntries (invoice), nullptr);
}
}

static int
run_tests (int argc, char **argv)
{
    g_setenv ("GNC_UNINSTALLED", "YES", TRUE);
    g_setenv ("GSETTINGS_BACKEND", "memory", TRUE);
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("GTK display is required for the entry-ledger response tests");
    qof_init ();
    if (!cashobjects_register ())
        g_error ("Failed to register cash objects for entry-ledger tests");
    gnc_gsettings_load_backend ();
    gnc_component_manager_init ();
    gnucash_register_add_cell_types ();
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
