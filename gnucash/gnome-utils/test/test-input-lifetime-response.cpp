/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <gtk/gtk.h>
#include "dialog-dup-trans.h"
#include "dialog-transfer.h"
#include "cashobjects.h"
#include "Account.h"
#include "gnc-commodity.h"
#include "gnc-gsettings.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"
#include "gnc-ui.h"

static gboolean display_available;

struct Result
{
    guint calls{};
    gboolean accepted{};
    gchar *username{};
    gchar *password{};
    GncDupTransResult *duplicate{};
};

static void credentials_finished (gboolean accepted, gchar *user, gchar *password, gpointer data)
{
    auto result = static_cast<Result *> (data);
    ++result->calls;
    result->accepted = accepted;
    result->username = user;
    result->password = password;
}

static void duplicate_finished (GncDupTransResult *duplicate, gpointer data)
{
    auto result = static_cast<Result *> (data);
    ++result->calls;
    result->accepted = duplicate != nullptr;
    result->duplicate = duplicate;
}

static GtkWidget *find_named (GtkWidget *widget, const gchar *name)
{
    if (GTK_IS_BUILDABLE (widget) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (widget)), name) == 0)
        return widget;
    if (!GTK_IS_CONTAINER (widget)) return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_named (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return found;
}

static void close_parent (GtkWidget *, gpointer parent)
{
    gtk_widget_destroy (GTK_WIDGET (parent));
}

static void test_input (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto scenario = GPOINTER_TO_INT (data);
    const auto duplicate = scenario >= 5;
    const auto action = scenario % 5;
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    g_object_ref (parent);
    Result result;
    if (duplicate)
        gnc_dup_trans_dialog_async (parent, "Duplicate", "Test", TRUE,
            1700000000, "10", "20", "test-link", duplicate_finished, &result);
    else
        gnc_get_username_password_async (parent, "Test", "Zähler", "synthetic-password",
                                         credentials_finished, &result);
    g_assert_cmpuint (result.calls, ==, 0);
    auto windows = gtk_window_list_toplevels ();
    GtkDialog *dialog = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == parent)
        {
            g_assert_null (dialog);
            dialog = GTK_DIALOG (node->data);
        }
    g_list_free (windows);
    g_assert_nonnull (dialog);
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_object_ref (dialog);
    if (duplicate)
    {
        gtk_entry_set_text (GTK_ENTRY (find_named (GTK_WIDGET (dialog), "num_entry")), "11");
        gtk_entry_set_text (GTK_ENTRY (find_named (GTK_WIDGET (dialog), "tnum_entry")), "21");
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (find_named (
            GTK_WIDGET (dialog), "link_check_button")), TRUE);
    }
    if (action == 2)
        gtk_widget_destroy (GTK_WIDGET (parent));
    else if (action == 3)
        gtk_widget_destroy (GTK_WIDGET (dialog));
    else
    {
        if (action == 4)
            g_signal_connect (dialog, "destroy", G_CALLBACK (close_parent), parent);
        gtk_dialog_response (dialog, action == 1 ? GTK_RESPONSE_CANCEL : GTK_RESPONSE_OK);
    }
    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_cmpint (result.accepted, ==, action == 0);
    if (action == 0 && duplicate)
    {
        g_assert_cmpstr (result.duplicate->num, ==, "11");
        g_assert_cmpstr (result.duplicate->tnum, ==, "21");
        g_assert_cmpstr (result.duplicate->doclink, ==, "test-link");
        g_assert_true (g_date_valid (&result.duplicate->gdate));
    }
    else if (action == 0)
    {
        g_assert_cmpstr (result.username, ==, "Zähler");
        g_assert_cmpstr (result.password, ==, "synthetic-password");
    }
    else
    {
        g_assert_null (result.username);
        g_assert_null (result.password);
        g_assert_null (result.duplicate);
    }
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    g_assert_cmpuint (result.calls, ==, 1);
    g_object_unref (dialog);
    gtk_widget_destroy (GTK_WIDGET (parent));
    g_object_unref (parent);
    g_free (result.username);
    g_free (result.password);
    gnc_dup_trans_result_free (result.duplicate);
}

static void transfer_finished (gboolean accepted, gpointer data)
{
    auto result = static_cast<Result *> (data);
    ++result->calls;
    result->accepted = accepted;
}

static void test_transfer (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto action = GPOINTER_TO_INT (data);
    auto session = qof_session_new (qof_book_new ());
    auto book = qof_session_get_book (session);
    gnc_set_current_session (session);
    auto root = gnc_account_create_root (book);
    auto account = xaccMallocAccount (book);
    xaccAccountBeginEdit (account);
    xaccAccountSetName (account, "Cash");
    xaccAccountSetType (account, ACCT_TYPE_ASSET);
    auto currency = gnc_commodity_table_lookup (gnc_commodity_table_get_table (book), "CURRENCY", "EUR");
    g_assert_nonnull (currency);
    xaccAccountSetCommodity (account, currency);
    gnc_account_append_child (root, account);
    xaccAccountCommitEdit (account);
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    g_object_ref (parent);
    Result result;
    gtk_widget_show (GTK_WIDGET (parent));
    auto transfer = gnc_xfer_dialog (GTK_WIDGET (parent), account);
    gnc_xfer_dialog_run_async (transfer, transfer_finished, &result);
    g_assert_cmpuint (result.calls, ==, 0);
    auto windows = gtk_window_list_toplevels ();
    GtkDialog *dialog = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == parent)
            dialog = GTK_DIALOG (node->data);
    g_list_free (windows);
    g_assert_nonnull (dialog);
    g_object_ref (dialog);
    if (action == 1) gtk_widget_destroy (GTK_WIDGET (parent));
    else if (action == 2) gtk_widget_destroy (GTK_WIDGET (dialog));
    else if (action == 3) gnc_xfer_dialog_close (transfer);
    else gtk_dialog_response (dialog, GTK_RESPONSE_CANCEL);
    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_false (result.accepted);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    g_assert_cmpuint (result.calls, ==, 1);
    g_object_unref (dialog);
    gtk_widget_destroy (GTK_WIDGET (parent));
    g_object_unref (parent);
    gnc_clear_current_session ();
}

int main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY")) g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    const char *names[] = {"accept", "cancel", "owner-destroy", "dialog-destroy", "destroy-owner-on-accept"};
    for (guint i = 0; i < 10; ++i)
    {
        auto path = g_strdup_printf ("/gnome-utils/input/%s/%s",
            i >= 5 ? "duplicate" : "credentials", names[i % 5]);
        g_test_add_data_func (path, GINT_TO_POINTER (i), test_input);
        g_free (path);
    }
    for (guint i = 0; i < 4; ++i)
    {
        const char *transfer_names[] = {"cancel", "owner-destroy", "dialog-destroy", "close"};
        auto path = g_strdup_printf ("/gnome-utils/input/transfer/%s", transfer_names[i]);
        g_test_add_data_func (path, GINT_TO_POINTER (i), test_transfer);
        g_free (path);
    }
    auto status = g_test_run ();
    gnc_component_manager_shutdown ();
    gnc_gsettings_shutdown ();
    qof_close ();
    return status;
}
