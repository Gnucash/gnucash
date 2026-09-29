/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */
#include <config.h>
#include <gtk/gtk.h>
#include "Account.h"
#include "cashobjects.h"
#include "dialog-account.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gnc-tree-model-account-types.h"
#include "qof.h"

namespace
{
gboolean display_available;

GtkWidget *
find_control (GtkWidget *root, const char *name)
{
    if (GTK_IS_BUILDABLE (root) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (root)), name) == 0)
        return root;
    if (!GTK_IS_CONTAINER (root)) return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *result = nullptr;
    for (auto node = children; node && !result; node = node->next)
        result = find_control (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return result;
}

GtkWidget *
find_window (GtkWindow *parent = nullptr)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if ((parent && GTK_IS_DIALOG (widget) &&
             gtk_window_get_transient_for (GTK_WINDOW (widget)) == parent) ||
            (!parent && g_strcmp0 (gtk_widget_get_name (widget), "gnc-id-account") == 0))
        {
            g_assert_null (result);
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

void
test_children_confirmation ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    for (int scenario = 0; scenario != 4; ++scenario)
    {
        auto book = qof_book_new ();
        gnc_set_current_session (qof_session_new (book));
        auto root = gnc_account_create_root (book);
        auto currency = gnc_commodity_new (book, "Test currency", "CURRENCY",
                                            "TST", "", 100);
        currency = gnc_commodity_table_insert (gnc_commodity_table_get_table (book),
                                                currency);
        auto account = xaccMallocAccount (book);
        auto child = xaccMallocAccount (book);
        xaccAccountSetName (account, "Confirmed parent");
        xaccAccountSetName (child, "Confirmed child");
        xaccAccountSetType (account, ACCT_TYPE_BANK);
        xaccAccountSetType (child, ACCT_TYPE_BANK);
        xaccAccountSetCommodity (account, currency);
        xaccAccountSetCommodity (child, currency);
        gnc_account_append_child (root, account);
        gnc_account_append_child (account, child);
        auto owner = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        gtk_widget_realize (owner);
        gnc_ui_edit_account_window (GTK_WINDOW (owner), account);
        auto parent = find_window ();
        g_assert_true (GTK_IS_DIALOG (parent));
        auto type = find_control (parent, "account_type_combo");
        g_assert_true (GTK_IS_COMBO_BOX (type));
        gnc_tree_model_account_types_set_active_combo (GTK_COMBO_BOX (type),
                                                        1 << ACCT_TYPE_INCOME);
        gtk_dialog_response (GTK_DIALOG (parent), GTK_RESPONSE_OK);
        auto question = find_window (GTK_WINDOW (parent));
        g_assert_true (GTK_IS_DIALOG (question));
        g_assert_cmpint (xaccAccountGetType (account), ==, ACCT_TYPE_BANK);
        g_assert_cmpint (xaccAccountGetType (child), ==, ACCT_TYPE_BANK);
        if (scenario == 2)
        {
            g_object_ref (question);
            gtk_widget_destroy (parent);
        }
        if (scenario == 3)
            qof_book_mark_readonly (book);
        gtk_dialog_response (GTK_DIALOG (question), scenario == 0 ?
                              GTK_RESPONSE_CANCEL : GTK_RESPONSE_OK);
        if (scenario == 2)
            g_object_unref (question);
        auto expected = scenario == 1 ? ACCT_TYPE_INCOME : ACCT_TYPE_BANK;
        g_assert_cmpint (xaccAccountGetType (account), ==, expected);
        g_assert_cmpint (xaccAccountGetType (child), ==, expected);
        if (scenario == 0 || scenario == 3)
            gtk_widget_destroy (parent);
        gtk_widget_destroy (owner);
        gnc_clear_current_session ();
    }
}

struct CreationResult
{
    guint calls{};
    Account *account{};
};

void
creation_completed (Account *account, gpointer data)
{
    auto result = static_cast<CreationResult *>(data);
    ++result->calls;
    result->account = account;
}

void
test_account_creation_response (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip("No graphical display is available");
        return;
    }
    auto scenario = GPOINTER_TO_INT(data);
    auto book = qof_book_new();
    auto session = qof_session_new(book);
    gnc_set_current_session(session);
    auto root = gnc_account_create_root(book);
    auto currency = gnc_commodity_new(book, "Test currency", "CURRENCY", "TST", "", 100);
    currency = gnc_commodity_table_insert(gnc_commodity_table_get_table(book), currency);
    auto base = xaccMallocAccount(book);
    xaccAccountSetName(base, "Existing parent");
    xaccAccountSetType(base, ACCT_TYPE_BANK);
    xaccAccountSetCommodity(base, currency);
    gnc_account_append_child(root, base);
    auto owner = GTK_WINDOW(gtk_window_new(GTK_WINDOW_TOPLEVEL));
    g_object_ref_sink(owner);
    gtk_widget_realize(GTK_WIDGET(owner));
    CreationResult result;
    auto types = g_list_prepend(nullptr, GINT_TO_POINTER(ACCT_TYPE_BANK));
    auto name = g_strdup("Created child");
    gnc_ui_new_accounts_from_name_with_defaults_async(
        owner, name, types, currency, base, creation_completed, &result);
    g_free(name);
    g_list_free(types);
    auto dialog = find_window(owner);
    g_assert_true(GTK_IS_DIALOG(dialog));
    g_assert_true(gtk_window_get_modal(GTK_WINDOW(dialog)));
    g_assert_true(gtk_window_get_destroy_with_parent(GTK_WINDOW(dialog)));
    g_assert_cmpuint(result.calls, ==, 0);
    g_object_ref(dialog);

    if (scenario == 2)
        gtk_widget_destroy(GTK_WIDGET(owner));
    else
    {
        if (scenario == 3)
        {
            auto other = qof_session_new(qof_book_new());
            gnc_set_current_session(other);
        }
        gtk_dialog_response(GTK_DIALOG(dialog), scenario == 1 ?
                             GTK_RESPONSE_CANCEL : GTK_RESPONSE_OK);
    }
    g_assert_cmpuint(result.calls, ==, 1);
    if (scenario == 0)
    {
        g_assert_nonnull(result.account);
        g_assert_cmpstr(xaccAccountGetName(result.account), ==, "Created child");
        g_assert_true(gnc_account_get_parent(result.account) == base);
        g_assert_true(gnc_account_get_book(result.account) == book);
    }
    else
        g_assert_null(result.account);
    /* Even a late response to a retained destroyed dialog completes once. */
    gtk_dialog_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK);
    g_assert_cmpuint(result.calls, ==, 1);
    g_object_unref(dialog);
    if (scenario != 2)
        gtk_widget_destroy(GTK_WIDGET(owner));
    g_object_unref(owner);
    if (scenario == 3)
    {
        gnc_clear_current_session();
        gnc_set_current_session(session);
    }
    gnc_clear_current_session();
}
}

int
main (int argc, char **argv)
{
    g_setenv("GNC_UNINSTALLED", "YES", TRUE);
    g_setenv("GSETTINGS_BACKEND", "memory", TRUE);
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY")) g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    g_test_add_func ("/gnome-utils/account/children-response", test_children_confirmation);
    const char *creation_cases[] = {"accept", "cancel", "parent-destroy", "session-switch"};
    for (guint i = 0; i < G_N_ELEMENTS(creation_cases); ++i)
    {
        auto path = g_strdup_printf("/gnome-utils/account/create/%s", creation_cases[i]);
        g_test_add_data_func(path, GINT_TO_POINTER(i), test_account_creation_response);
        g_free(path);
    }
    auto result = g_test_run ();
    gnc_gsettings_shutdown ();
    gnc_component_manager_shutdown ();
    gnc_clear_current_session ();
    qof_close ();
    return result;
}
