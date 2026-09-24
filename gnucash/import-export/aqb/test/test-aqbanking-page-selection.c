/*
 * test-aqbanking-page-selection.c -- AqBanking page selection regression test
 * Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>
#include <qof.h>

#include "Account.h"
#include "gnc-ab-kvp.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-gtk-utils.h"
#include "gnc-main-window.h"
#include "gnc-plugin-aqbanking.h"
#include "gnc-plugin-page-account-tree.h"
#include "gnc-prefs.h"
#include "gnc-prefs-utils.h"
#include "gnc-session.h"
#include "gnc-tree-view-account.h"

#define AQ_ACTION_GROUP "gnc-plugin-aqbanking-actions"
#define BALANCE_ACTION "ABGetBalanceAction"
#define BALANCE_MENU_ACTION AQ_ACTION_GROUP "." BALANCE_ACTION

typedef struct
{
    GMainLoop *loop;
    Account *expected;
    gboolean emitted;
    gboolean barrier_completed;
    gboolean timed_out;
    guint barrier_source;
    guint timeout_source;
} SelectionWait;

static GtkWidget *
find_widget_of_type (GtkWidget *root, GType type)
{
    if (G_TYPE_CHECK_INSTANCE_TYPE (root, type))
        return root;

    for (GtkWidget *child = gtk_widget_get_first_child (root); child;
         child = gtk_widget_get_next_sibling (child))
    {
        GtkWidget *match = find_widget_of_type (child, type);
        if (match)
            return match;
    }
    return NULL;
}

static gboolean
selection_wait_timeout (gpointer user_data)
{
    SelectionWait *wait = user_data;
    wait->timeout_source = 0;
    wait->timed_out = TRUE;
    g_main_loop_quit (wait->loop);
    return G_SOURCE_REMOVE;
}

static void
account_selected (GncPluginPage *page, Account *account, gpointer user_data)
{
    SelectionWait *wait = user_data;

    if (account == wait->expected)
        wait->emitted = TRUE;
    (void)page;
}

static gboolean
selection_wait_barrier (gpointer user_data)
{
    SelectionWait *wait = user_data;

    wait->barrier_source = 0;
    wait->barrier_completed = TRUE;
    g_main_loop_quit (wait->loop);
    return G_SOURCE_REMOVE;
}

static void
set_account_and_wait (const gchar *phase, GncPluginPage *page,
                      GncTreeViewAccount *view, Account *account)
{
    Account *before = gnc_tree_view_account_get_selected_account (view);
    SelectionWait wait = { 0 };
    gulong handler;

    g_test_message ("%s: selecting %s; previous selection %s", phase,
                    account ? xaccAccountGetName (account) : "(none)",
                    before ? xaccAccountGetName (before) : "(none)");
    g_assert_true (before != account);
    wait.loop = g_main_loop_new (NULL, FALSE);
    wait.expected = account;
    handler = g_signal_connect (page, "account_selected",
                                G_CALLBACK (account_selected), &wait);

    gnc_tree_view_account_set_selected_account (view, account);
    /* Product selection restore runs at DEFAULT_IDLE. This one-shot lower
     * priority barrier observes its completed account_selected emission. */
    wait.barrier_source = g_idle_add_full (G_PRIORITY_LOW,
                                           selection_wait_barrier, &wait,
                                           NULL);
    wait.timeout_source = g_timeout_add_seconds (60, selection_wait_timeout,
                                                  &wait);
    g_main_loop_run (wait.loop);

    if (wait.barrier_source)
        g_source_remove (wait.barrier_source);
    if (wait.timeout_source)
        g_source_remove (wait.timeout_source);
    g_signal_handler_disconnect (page, handler);
    g_main_loop_unref (wait.loop);
    g_test_message ("%s: barrier=%d emission=%d selection=%s", phase,
                    wait.barrier_completed, wait.emitted,
                    gnc_tree_view_account_get_selected_account (view)
                        ? xaccAccountGetName (
                              gnc_tree_view_account_get_selected_account (view))
                        : "(none)");
    g_assert_false (wait.timed_out);
    g_assert_true (wait.barrier_completed);
    g_assert_true (wait.emitted);
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == account);
}

static gboolean
action_enabled (GncMainWindow *window)
{
    GAction *action = gnc_main_window_find_action_in_group (
        window, AQ_ACTION_GROUP, BALANCE_ACTION);
    g_assert_nonnull (action);
    return g_action_get_enabled (action);
}

static gboolean
balance_menu_visible (GncMainWindow *window)
{
    GncMenuModelSearch search = { 0 };
    search.search_action_name = BALANCE_MENU_ACTION;
    return gnc_menubar_model_find_item (gnc_main_window_get_menu_model (window),
                                        &search);
}

static GncTreeViewAccount *
account_view (GncPluginPage *page)
{
    GtkWidget *widget = find_widget_of_type (page->notebook_page,
                                             GNC_TYPE_TREE_VIEW_ACCOUNT);
    g_assert_nonnull (widget);
    return GNC_TREE_VIEW_ACCOUNT (widget);
}

static Account *
new_account (QofBook *book, Account *root, const gchar *name,
             gboolean online_capable)
{
    Account *account = xaccMallocAccount (book);
    xaccAccountSetName (account, name);
    xaccAccountSetType (account, ACCT_TYPE_BANK);
    gnc_account_append_child (root, account);
    if (online_capable)
    {
        gnc_ab_set_account_bankcode (account, "12345678");
        gnc_ab_set_account_accountid (account, "9876543210");
    }
    return account;
}

static void
test_inactive_account_restore_does_not_override_online_actions (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    Account *root;
    Account *online;
    Account *offline;
    GncMainWindow *window;
    GncPluginPage *active_page;
    GncPluginPage *inactive_page;
    GncTreeViewAccount *active_view;
    GncTreeViewAccount *inactive_view;

    gnc_set_current_session (session);
    root = gnc_account_create_root (book);
    online = new_account (book, root, "Online bank", TRUE);
    offline = new_account (book, root, "Offline bank", FALSE);

    gnc_plugin_aqbanking_create_plugin ();
    window = g_object_ref_sink (gnc_main_window_new ());
    active_page = gnc_plugin_page_account_tree_new ();
    inactive_page = gnc_plugin_page_account_tree_new ();
    g_assert_true (active_page != inactive_page);
    gnc_main_window_open_page (window, active_page);
    gnc_main_window_open_page (window, inactive_page);
    active_view = account_view (active_page);
    inactive_view = account_view (inactive_page);
    g_assert_true (active_view != inactive_view);

    /* Establish the inactive page's capability state before selecting the
     * authoritative active page. A non-NULL account stays visible but is
     * disabled when its AqBanking identifiers are absent. */
    set_account_and_wait ("initialize inactive page", inactive_page,
                          inactive_view, offline);
    g_assert_false (action_enabled (window));
    g_assert_true (balance_menu_visible (window));

    gnc_main_window_display_page (active_page);
    set_account_and_wait ("select active online account", active_page,
                          active_view, online);
    g_assert_true (action_enabled (window));
    g_assert_true (balance_menu_visible (window));

    /* The deferred selection restore is the startup ordering edge: an
     * inactive page must not rewrite window-wide AqBanking state. */
    set_account_and_wait ("late inactive clear", inactive_page,
                          inactive_view, NULL);
    g_assert_true (gnc_main_window_get_current_page (window) == active_page);
    g_assert_true (action_enabled (window));
    g_assert_true (balance_menu_visible (window));

    set_account_and_wait ("late inactive offline selection", inactive_page,
                          inactive_view, offline);
    g_assert_true (gnc_main_window_get_current_page (window) == active_page);
    g_assert_true (action_enabled (window));
    g_assert_true (balance_menu_visible (window));

    gnc_main_window_display_page (inactive_page);
    g_assert_false (action_enabled (window));
    g_assert_true (balance_menu_visible (window));

    set_account_and_wait ("clear active offline page", inactive_page,
                          inactive_view, NULL);
    g_assert_false (action_enabled (window));
    g_assert_false (balance_menu_visible (window));

    set_account_and_wait ("restore active offline account", inactive_page,
                          inactive_view, offline);
    g_assert_false (action_enabled (window));
    g_assert_true (balance_menu_visible (window));

    gnc_main_window_display_page (active_page);
    g_assert_true (action_enabled (window));
    g_assert_true (balance_menu_visible (window));

    gtk_window_destroy (GTK_WINDOW (window));
    g_object_unref (window);
    gnc_clear_current_session ();
}

int
main (int argc, char **argv)
{
    int status;

    g_setenv ("GSETTINGS_BACKEND", "memory", TRUE);
    g_test_init (&argc, &argv, NULL);
    gtk_init ();
    qof_log_init_filename_special ("stderr");
    /* Keep fatal GUI diagnostics visible; the QOF bridge can filter log flags. */
    g_log_set_default_handler (g_log_default_handler, NULL);
    gnc_engine_init_static (argc, argv);
    gnc_prefs_init ();
    gnc_component_manager_init ();

    g_test_add_func ("/import-export/aqb/page-selection/inactive-restore",
                     test_inactive_account_restore_does_not_override_online_actions);
    status = g_test_run ();

    gnc_component_manager_shutdown ();
    gnc_prefs_remove_registered ();
    gnc_engine_shutdown ();
    return status;
}
