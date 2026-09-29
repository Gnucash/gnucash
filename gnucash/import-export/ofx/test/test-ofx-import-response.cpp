/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

/* Include the implementation to exercise the real importer entry continuation
 * without opening the file chooser. The test remains linked to the same
 * production dependencies as gncmod-ofx. */
#include "../gnc-ofx-import.cpp"

#include <gtk/gtk.h>
#include <glib/gstdio.h>

#include "cashobjects.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gnc-tree-view-account.h"

static gboolean display_available;
static guint parsed_fixture_transactions;

static int
count_fixture_transaction(OfxTransactionData data, void *)
{
    ++parsed_fixture_transactions;
    return 0;
}

static const char *fixture_text =
    "OFXHEADER:100\nDATA:OFXSGML\nVERSION:102\nSECURITY:NONE\n"
    "ENCODING:USASCII\nCHARSET:1252\nCOMPRESSION:NONE\n"
    "OLDFILEUID:NONE\nNEWFILEUID:NONE\n\n"
    "<OFX><SIGNONMSGSRSV1><SONRS><STATUS><CODE>0<SEVERITY>INFO"
    "<MESSAGE>OK</STATUS><DTSERVER>20260103120000<LANGUAGE>ENG"
    "</SONRS></SIGNONMSGSRSV1><BANKMSGSRSV1><STMTTRNRS><TRNUID>1"
    "<STATUS><CODE>0<SEVERITY>INFO<MESSAGE>OK</STATUS><STMTRS>"
    "<CURDEF>USD<BANKACCTFROM><BANKID>000000000"
    "<ACCTID>OFX-ASYNC-TEST-ACCOUNT<ACCTTYPE>CHECKING</BANKACCTFROM>"
    "<BANKTRANLIST><DTSTART>20260101<DTEND>20260103<STMTTRN>"
    "<TRNTYPE>DEBIT<DTPOSTED>20260102120000<TRNAMT>-1.23"
    "<FITID>OFX-ASYNC-TEST-TRANSACTION<NAME>OFX fixture transaction"
    "</STMTTRN></BANKTRANLIST><LEDGERBAL><BALAMT>-1.23"
    "<DTASOF>20260103</LEDGERBAL></STMTRS></STMTTRNRS>"
    "</BANKMSGSRSV1></OFX>\n";

struct TestSession
{
    QofSession *session{};
    Account *bank{};
    gnc_commodity *usd{};
};

static TestSession
make_session (gboolean matching_account)
{
    TestSession result;
    auto book = qof_book_new ();
    result.session = qof_session_new (book);
    gnc_set_current_session (result.session);

    auto table = gnc_commodity_table_get_table (book);
    gnc_commodity_table_add_namespace (table, GNC_COMMODITY_NS_CURRENCY, book);
    result.usd = gnc_commodity_table_lookup (table,
                                              GNC_COMMODITY_NS_CURRENCY,
                                              "USD");
    if (!result.usd)
    {
        auto usd = gnc_commodity_new (book, "US Dollar",
                                     GNC_COMMODITY_NS_CURRENCY,
                                     "USD", "USD", 100);
        result.usd = gnc_commodity_table_insert (table, usd);
    }

    auto root = gnc_account_create_root (book);
    result.bank = xaccMallocAccount (book);
    xaccAccountSetName (result.bank, "OFX fixture account");
    xaccAccountSetType (result.bank, ACCT_TYPE_BANK);
    xaccAccountSetCommodity (result.bank, result.usd);
    if (matching_account)
        xaccAccountSetOnlineID (result.bank, "OFX-ASYNC-TEST-ACCOUNT");
    gnc_account_append_child (root, result.bank);
    return result;
}

static gchar *
write_fixture_file ()
{
    GError *error = NULL;
    gchar *path = NULL;
    gint fd = g_file_open_tmp ("gnucash-ofx-XXXXXX", &path, &error);
    g_assert_no_error (error);
    g_assert_cmpint (fd, >=, 0);
    g_assert_true (g_close (fd, &error));
    g_assert_no_error (error);
    g_assert_true (g_file_set_contents (path, fixture_text, -1, &error));
    g_assert_no_error (error);
    return path;
}

static void
assert_fixture_has_transactions(const gchar *path)
{
    parsed_fixture_transactions = 0;
    auto context = libofx_get_new_context();
    ofx_set_transaction_cb(context, count_fixture_transaction, nullptr);
    libofx_proc_file(context, path, AUTODETECT);
    libofx_free_context(context);
    g_assert_cmpuint(parsed_fixture_transactions, >, 0);
}

static GtkWidget *
find_named_widget (GtkWidget *root, const gchar *name)
{
    if (GTK_IS_BUILDABLE (root) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (root)), name) == 0)
        return root;
    if (!GTK_IS_CONTAINER (root))
        return NULL;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *found = NULL;
    for (auto node = children; node && !found; node = node->next)
        found = find_named_widget (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return found;
}

static GncTreeViewAccount *
find_account_tree(GtkWidget *root)
{
    if (GNC_IS_TREE_VIEW_ACCOUNT(root))
        return GNC_TREE_VIEW_ACCOUNT(root);
    if (!GTK_IS_CONTAINER(root))
        return nullptr;
    auto children = gtk_container_get_children(GTK_CONTAINER(root));
    GncTreeViewAccount *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_account_tree(GTK_WIDGET(node->data));
    g_list_free(children);
    return found;
}

static GtkWidget *
find_transient_dialog (GtkWindow *parent, const gchar *child_name)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = NULL;
    for (auto node = windows; node && !found; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (GTK_IS_DIALOG (widget) && gtk_widget_get_visible (widget) &&
            gtk_window_get_transient_for (GTK_WINDOW (widget)) == parent &&
            (!child_name || find_named_widget (widget, child_name)))
            found = widget;
    }
    g_list_free (windows);
    return found;
}

static GtkWidget *
wait_for_transient_dialog (GtkWindow *parent, const gchar *child_name)
{
    for (guint i = 0; i < 2000; ++i)
    {
        while (g_main_context_iteration (NULL, FALSE))
            ;
        auto dialog = find_transient_dialog (parent, child_name);
        if (dialog)
            return dialog;
        g_usleep (1000);
    }
    return NULL;
}

static GtkWidget *
wait_for_matcher_resolving_account_picker(GtkWindow *parent, Account *account)
{
    for (guint i = 0; i < 3000; ++i)
    {
        while (g_main_context_iteration (NULL, FALSE))
            ;
        auto windows = gtk_window_list_toplevels ();
        GtkWidget *matcher = nullptr;
        GtkWidget *picker = nullptr;
        for (auto node = windows; node; node = node->next)
        {
            auto widget = GTK_WIDGET (node->data);
            if (!GTK_IS_DIALOG (widget) || !gtk_widget_get_visible (widget) ||
                gtk_window_get_transient_for (GTK_WINDOW (widget)) != parent)
                continue;
            if (find_named_widget (widget, "matcher_cancel") ||
                find_named_widget (widget, "matcher_ok"))
                matcher = widget;
            else if (g_strcmp0 (gtk_widget_get_name (widget),
                                "gnc-id-import-account-picker") == 0)
                picker = widget;
        }
        g_list_free (windows);
        if (matcher)
            return matcher;
        if (picker)
        {
            auto account_tree = find_account_tree(picker);
            if (!account_tree || !account)
                return nullptr;
            gnc_tree_view_account_set_selected_account (
                account_tree, account);
            gtk_dialog_response (GTK_DIALOG (picker), GTK_RESPONSE_OK);
        }
        g_usleep (1000);
    }
    return nullptr;
}

static void
assert_matcher_has_downloaded_transaction(GtkWidget *matcher)
{
    auto view = find_named_widget(matcher, "downloaded_view");
    g_assert_true(GTK_IS_TREE_VIEW(view));
    auto model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
    g_assert_nonnull(model);
    g_assert_cmpint(gtk_tree_model_iter_n_children(model, nullptr), >, 0);
}

static void
start_real_import (GtkWindow *parent, const gchar *path)
{
    auto selected = g_slist_append (NULL, g_strdup (path));
    gnc_file_ofx_import_files_selected (selected, parent);
}

static void
finish_session (GtkWindow *parent)
{
    gtk_widget_destroy (GTK_WIDGET (parent));
    g_object_unref (parent);
    gnc_clear_current_session ();
}

static void
test_ofx_two_pass_then_cancel_matcher ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto test_session = make_session (TRUE);
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    g_object_ref_sink (parent);
    gtk_widget_show (GTK_WIDGET (parent));
    auto path = write_fixture_file ();
    assert_fixture_has_transactions(path);
    start_real_import (parent, path);
    auto matcher_dialog = wait_for_matcher_resolving_account_picker (
        parent, test_session.bank);
    g_assert_nonnull (matcher_dialog);
    g_assert_true (gnc_gui_session_operation_pending ());
    assert_matcher_has_downloaded_transaction(matcher_dialog);
    g_assert_cmpuint (g_list_length (xaccAccountGetSplitList (test_session.bank)), ==, 0);

    g_object_ref (matcher_dialog);
    auto cancel = find_named_widget (matcher_dialog, "matcher_cancel");
    g_assert_nonnull (cancel);
    g_object_ref (cancel);
    gtk_button_clicked (GTK_BUTTON (cancel));
    g_assert_false (gnc_gui_session_operation_pending ());
    g_assert_cmpuint (g_list_length (xaccAccountGetSplitList (test_session.bank)), ==, 0);

    /* A retained, already-closed matcher must not resume the freed OFX request. */
    gtk_dialog_response (GTK_DIALOG (matcher_dialog), GTK_RESPONSE_ACCEPT);
    gtk_button_clicked (GTK_BUTTON (cancel));
    g_assert_false (gnc_gui_session_operation_pending ());
    g_object_unref (cancel);
    g_object_unref (matcher_dialog);
    g_remove (path);
    g_free (path);
    finish_session (parent);
}

static void
test_ofx_two_pass_import_accepted ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto test_session = make_session (TRUE);
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    g_object_ref_sink (parent);
    gtk_widget_show (GTK_WIDGET (parent));
    auto path = write_fixture_file ();

    start_real_import (parent, path);
    auto matcher_dialog = wait_for_matcher_resolving_account_picker (
        parent, test_session.bank);
    g_assert_nonnull (matcher_dialog);
    g_assert_true (gnc_gui_session_operation_pending ());
    assert_matcher_has_downloaded_transaction(matcher_dialog);
    g_assert_cmpuint (g_list_length (xaccAccountGetSplitList (test_session.bank)), ==, 0);

    g_object_ref (matcher_dialog);
    auto accept = find_named_widget (matcher_dialog, "matcher_ok");
    g_assert_nonnull (accept);
    g_object_ref (accept);
    gtk_button_clicked (GTK_BUTTON (accept));
    g_assert_false (gnc_gui_session_operation_pending ());
    g_assert_cmpuint (g_list_length (xaccAccountGetSplitList (test_session.bank)), >, 0);

    /* The committed matcher widget cannot complete the OFX request twice. */
    gtk_button_clicked (GTK_BUTTON (accept));
    g_assert_false (gnc_gui_session_operation_pending ());
    g_assert_cmpuint (g_list_length (xaccAccountGetSplitList (test_session.bank)), >, 0);
    g_object_unref (accept);
    g_object_unref (matcher_dialog);
    g_remove (path);
    g_free (path);
    finish_session (parent);
}

static void
test_ofx_parent_destroy_during_account_selection ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto test_session = make_session (FALSE);
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    g_object_ref_sink (parent);
    gtk_widget_show (GTK_WIDGET (parent));
    auto path = write_fixture_file ();

    start_real_import (parent, path);
    auto picker = wait_for_transient_dialog (parent, NULL);
    g_assert_nonnull (picker);
    g_object_ref (picker);
    g_assert_true (gnc_gui_session_operation_pending ());

    gtk_widget_destroy (GTK_WIDGET (parent));
    /* The account picker is destroyed with its owner, which cancels the
     * pending import before any account resolution or second parser pass. */
    g_assert_false (gnc_gui_session_operation_pending ());
    g_assert_cmpuint (g_list_length (xaccAccountGetSplitList (test_session.bank)), ==, 0);

    /* A late accept after teardown cannot run the cancelled continuation. */
    gtk_dialog_response (GTK_DIALOG (picker), GTK_RESPONSE_OK);
    g_assert_false (gnc_gui_session_operation_pending ());
    g_assert_cmpuint (g_list_length (xaccAccountGetSplitList (test_session.bank)), ==, 0);
    g_object_unref (picker);
    g_object_unref (parent);
    g_remove (path);
    g_free (path);
    gnc_clear_current_session ();
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();

    g_test_add_func ("/ofx/import/two-pass-cancel-matcher",
                     test_ofx_two_pass_then_cancel_matcher);
    g_test_add_func ("/ofx/import/two-pass-accepted",
                     test_ofx_two_pass_import_accepted);
    g_test_add_func ("/ofx/import/parent-destroy-account-selection",
                     test_ofx_parent_destroy_during_account_selection);
    auto status = g_test_run ();

    gnc_component_manager_shutdown ();
    gnc_gsettings_shutdown ();
    qof_close ();
    return status;
}
