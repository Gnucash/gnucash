/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <gtk/gtk.h>

#include <aqbanking/types/imexporter_accountinfo.h>
#include <aqbanking/types/imexporter_context.h>
#include <aqbanking/types/transaction.h>
#include <aqbanking/types/value.h>
#include <gwenhywfar/gwendate.h>

#include "Account.h"
#include "Split.h"
#include "Transaction.h"
#include "cashobjects.h"
#include "gnc-ab-kvp.h"
#include "gnc-ab-utils.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-gnome-utils.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"

static gboolean display_available;

enum class Action { Accept, Cancel, DestroyParent };

struct ImportRun
{
    GThread *gtk_thread{};
    QofSession *session{};
    QofBook *book{};
    GtkWidget *parent{};
    AB_IMEXPORTER_CONTEXT *context{};
    GncABImExContextImport *ieci{};
    Action action{};
    guint session_lease{};
    guint operation_token{};
    guint dialog_source{};
    guint completion_count{};
    gboolean accepted{};
    gboolean parent_destroyed{};
    gboolean finished{};
};

static GtkWidget *find_named (GtkWidget *widget, const gchar *name)
{
    if (GTK_IS_BUILDABLE (widget) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (widget)), name) == 0)
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_named (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return found;
}

static gboolean label_contains (GtkWidget *widget, const gchar *needle)
{
    if (GTK_IS_LABEL (widget) &&
        g_strstr_len (gtk_label_get_text (GTK_LABEL (widget)), -1, needle))
        return TRUE;
    if (!GTK_IS_CONTAINER (widget))
        return FALSE;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    gboolean found = FALSE;
    for (auto node = children; node && !found; node = node->next)
        found = label_contains (GTK_WIDGET (node->data), needle);
    g_list_free (children);
    return found;
}

static GtkWidget *find_dialog_with_text (const gchar *needle)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node && !found; node = node->next)
        if (GTK_IS_DIALOG (node->data) &&
            label_contains (GTK_WIDGET (node->data), needle))
            found = GTK_WIDGET (node->data);
    g_list_free (windows);
    return found;
}

static void finish_import (GncABImExContextImport *ieci, gpointer user_data)
{
    auto run = static_cast<ImportRun *> (user_data);
    g_assert_true (g_thread_self () == run->gtk_thread);
    if (!ieci)
    {
        run->finished = TRUE;
        ++run->completion_count;
        gnc_ab_operation_release (run->operation_token);
        gnc_gui_end_session_operation (run->session_lease);
        return;
    }
    /* The real traversal must have materialized the bank transaction before
     * showing the generic matcher. Its acceptance is what commits it to QOF. */
    run->ieci = ieci;
    gnc_ab_ieci_run_matcher_async (ieci,
        [](gboolean accepted, gpointer data)
        {
            auto state = static_cast<ImportRun *> (data);
            g_assert_true (g_thread_self () == state->gtk_thread);
            state->accepted = accepted;
            ++state->completion_count;
            gnc_ab_ieci_free (state->ieci);
            state->ieci = nullptr;
            state->finished = TRUE;
            gnc_ab_operation_release (state->operation_token);
            gnc_gui_end_session_operation (state->session_lease);
        }, run);
}

static void start_import (guint token, gpointer user_data)
{
    auto run = static_cast<ImportRun *> (user_data);
    run->operation_token = token;
    gnc_ab_import_context_async (run->context, AWAIT_TRANSACTIONS, FALSE,
        nullptr, run->parent, finish_import, run);
}

static gboolean drive_dialogs (gpointer user_data)
{
    auto run = static_cast<ImportRun *> (user_data);
    if (run->finished)
        return G_SOURCE_REMOVE;
    if (run->action == Action::DestroyParent)
    {
        if (!run->parent_destroyed)
        {
            run->parent_destroyed = TRUE;
            gtk_widget_destroy (run->parent);
        }
        return G_SOURCE_CONTINUE;
    }

    if (auto prompt = find_dialog_with_text (
            "The bank sent transaction information"))
    {
        gtk_dialog_response (GTK_DIALOG (prompt), GTK_RESPONSE_YES);
        return G_SOURCE_CONTINUE;
    }
    if (!run->ieci)
        return G_SOURCE_CONTINUE;
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *matcher = nullptr;
    for (auto node = windows; node && !matcher; node = node->next)
        if (gtk_widget_get_visible (GTK_WIDGET (node->data)) &&
            gtk_widget_get_mapped (GTK_WIDGET (node->data)) &&
            find_named (GTK_WIDGET (node->data), "matcher_cancel"))
            matcher = GTK_WIDGET (node->data);
    g_list_free (windows);
    if (matcher)
    {
        auto view = find_named (matcher, "downloaded_view");
        g_assert_true (GTK_IS_TREE_VIEW (view));
        auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
        /* Do not let an empty matcher count as successful acceptance: it
         * closes with accepted=TRUE but proves nothing about stage 3. */
        if (gtk_tree_model_iter_n_children (model, nullptr) == 0)
            return G_SOURCE_CONTINUE;
        const auto button_name = run->action == Action::Accept ?
            "matcher_ok" : "matcher_cancel";
        auto button = find_named (matcher, button_name);
        g_assert_nonnull (button);
        gtk_button_clicked (GTK_BUTTON (button));
    }
    return G_SOURCE_CONTINUE;
}

static gboolean contains_imported_transaction (QofBook *book,
                                              const gchar *fitid)
{
    auto root = gnc_book_get_root_account (book);
    auto accounts = gnc_account_get_descendants (root);
    gboolean found = FALSE;
    for (auto node = accounts; node && !found; node = node->next)
    {
        auto account = static_cast<Account *>(node->data);
        for (auto split = xaccAccountGetSplitList (account); split;
             split = split->next)
            if (g_strcmp0 (xaccSplitGetOnlineID (
                    static_cast<Split *>(split->data)), fitid) == 0)
                found = TRUE;
    }
    g_list_free (accounts);
    return found;
}

static void test_context_import (gconstpointer test_data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    ImportRun run;
    run.gtk_thread = g_thread_self ();
    run.action = static_cast<Action>(GPOINTER_TO_INT (test_data));
    run.session = qof_session_new (qof_book_new ());
    run.book = qof_session_get_book (run.session);
    gnc_account_create_root (run.book);
    qof_book_mark_session_saved (run.book);
    gnc_set_current_session (run.session);
    auto currency = gnc_commodity_table_lookup (
        gnc_commodity_table_get_table (run.book), GNC_COMMODITY_NS_CURRENCY,
        "EUR");
    g_assert_nonnull (currency);
    auto account = xaccMallocAccount (run.book);
    xaccAccountBeginEdit (account);
    xaccAccountSetName (account, "Imported checking");
    xaccAccountSetType (account, ACCT_TYPE_BANK);
    xaccAccountSetCommodity (account, currency);
    gnc_account_append_child (gnc_book_get_root_account (run.book), account);
    xaccAccountCommitEdit (account);
    gnc_ab_set_account_bankcode (account, "50010517");
    gnc_ab_set_account_accountid (account, "5447461406");
    auto online_id = gnc_ab_create_online_id ("50010517", "5447461406");
    xaccAccountSetOnlineID (account, online_id);
    g_free (online_id);

    run.context = AB_ImExporterContext_new ();
    auto account_info = AB_ImExporterAccountInfo_new ();
    AB_ImExporterAccountInfo_SetBankCode (account_info, "50010517");
    AB_ImExporterAccountInfo_SetAccountNumber (account_info, "5447461406");
    AB_ImExporterAccountInfo_SetAccountName (account_info, "Checking");
    AB_ImExporterContext_AddAccountInfo (run.context, account_info);
    auto transaction = AB_Transaction_new ();
    AB_Transaction_SetType (transaction, AB_Transaction_TypeStatement);
    AB_Transaction_SetFiId (transaction, "stage3-regression-fitid");
    AB_Transaction_SetLocalBankCode (transaction, "50010517");
    AB_Transaction_SetLocalAccountNumber (transaction, "5447461406");
    AB_Transaction_SetRemoteName (transaction, "Regression payee");
    auto value = AB_Value_fromString ("12.34");
    AB_Transaction_SetValue (transaction, value);
    auto date = GWEN_Date_fromStringWithTemplate ("20260929", "YYYYMMDD");
    AB_Transaction_SetValutaDate (transaction, date);
    AB_ImExporterAccountInfo_AddTransaction (account_info, transaction);
    AB_Value_free (value);
    GWEN_Date_free (date);

    run.parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    g_object_ref_sink (run.parent);
    gtk_widget_show (run.parent);
    run.session_lease = gnc_gui_begin_session_operation (run.book);
    g_assert_cmpuint (run.session_lease, !=, 0);
    gnc_ab_operation_acquire_async (start_import, &run);
    run.dialog_source = g_timeout_add (10, drive_dialogs, &run);

    const gint64 deadline = g_get_monotonic_time () + 8 * G_TIME_SPAN_SECOND;
    while (!run.finished && g_get_monotonic_time () < deadline)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        g_usleep (1000);
    }
    g_assert_true (run.finished);
    g_assert_cmpuint (run.completion_count, ==, 1);
    g_assert_cmpint (contains_imported_transaction (run.book,
                    "stage3-regression-fitid"), ==,
        run.action == Action::Accept);

    if (run.dialog_source &&
        g_main_context_find_source_by_id (nullptr, run.dialog_source))
        g_source_remove (run.dialog_source);
    run.dialog_source = 0;
    gtk_widget_destroy (run.parent);
    g_object_unref (run.parent);
    AB_ImExporterContext_free (run.context);
    gnc_clear_current_session ();
}

int main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    const char *names[] = {"accept", "cancel", "parent-destroy"};
    for (guint i = 0; i < G_N_ELEMENTS (names); ++i)
        g_test_add_data_func (g_strdup_printf (
            "/import-export/aqb/context-import/%s", names[i]),
            GINT_TO_POINTER (i), test_context_import);
    return g_test_run ();
}
