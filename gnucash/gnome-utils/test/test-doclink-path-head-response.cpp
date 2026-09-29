/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>
#include <gtk/gtk.h>

#include "cashobjects.h"
#include "dialog-doclink-utils.h"
#include "gnc-prefs.h"
#include "gnc-prefs-p.h"
#include "gnc-session.h"
#include "gnc-uri-utils.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "qofbook.h"
#include "qofsession.h"
#include "qofevent.h"
#include "Transaction.h"
#include "Account.h"
#include "Split.h"
#include "gnc-commodity.h"

static gboolean display_available;
static gchar *path_head;

static gchar *
memory_get_string (const gchar *group, const gchar *name)
{
    if (g_strcmp0 (group, GNC_PREFS_GROUP_GENERAL) == 0 &&
        g_strcmp0 (name, GNC_DOC_LINK_PATH_HEAD) == 0)
        return g_strdup (path_head);
    return nullptr;
}

static gboolean
memory_set_string (const gchar *group, const gchar *name, const gchar *value)
{
    if (g_strcmp0 (group, GNC_PREFS_GROUP_GENERAL) != 0 ||
        g_strcmp0 (name, GNC_DOC_LINK_PATH_HEAD) != 0)
        return FALSE;
    g_free (path_head);
    path_head = g_strdup (value);
    return TRUE;
}

static Transaction *
new_transaction (QofBook *book)
{
    auto currency = gnc_commodity_new (book, "Test", GNC_COMMODITY_NS_CURRENCY,
                                       "TST", "", 100);
    gnc_commodity_table_insert (gnc_commodity_table_get_table (book), currency);
    auto debit = xaccMallocAccount (book);
    auto credit = xaccMallocAccount (book);
    xaccAccountSetType (debit, ACCT_TYPE_BANK);
    xaccAccountSetType (credit, ACCT_TYPE_EXPENSE);
    xaccAccountSetCommodity (debit, currency);
    xaccAccountSetCommodity (credit, currency);
    auto trans = xaccMallocTransaction (book);
    xaccTransBeginEdit (trans);
    xaccTransSetCurrency (trans, currency);
    for (guint i = 0; i < 2; ++i)
    {
        auto split = xaccMallocSplit (book);
        xaccSplitSetParent (split, trans);
        xaccSplitSetAccount (split, i == 0 ? debit : credit);
        auto value = gnc_numeric_create (i == 0 ? 100 : -100, 100);
        xaccSplitSetValue (split, value);
        xaccSplitSetAmount (split, value);
    }
    xaccTransCommitEdit (trans);
    return trans;
}

static GtkWidget *
find_path_head_dialog ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *dialog = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (widget),
                       "gnc-id-doclink-change") == 0)
        {
            g_assert_null (dialog);
            dialog = widget;
        }
    }
    g_list_free (windows);
    return dialog;
}

static void
set_doclink (Transaction *trans, const gchar *uri)
{
    xaccTransBeginEdit (trans);
    xaccTransSetDocLink (trans, uri);
    xaccTransCommitEdit (trans);
}

static void
test_path_head_response ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto book = gnc_get_current_book ();
    auto trans = new_transaction (book);
    const gchar *new_head = "file:///new-head/";
    const gchar *old_head = "file:///old-head/";
    const gchar *absolute_link = "file:///new-head/document.pdf";
    g_assert_true (gnc_prefs_set_string (GNC_PREFS_GROUP_GENERAL,
                                         GNC_DOC_LINK_PATH_HEAD, new_head));
    set_doclink (trans, absolute_link);

    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    auto borrowed_old_head = g_strdup (old_head);
    gnc_doclink_pref_path_head_changed (parent, borrowed_old_head);
    g_free (borrowed_old_head);

    auto dialog = find_path_head_dialog ();
    g_assert_nonnull (dialog);
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    g_assert_cmpstr (xaccTransGetDocLink (trans), ==, absolute_link);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);
    g_assert_null (find_path_head_dialog ());
    g_assert_cmpstr (xaccTransGetDocLink (trans), ==, absolute_link);

    gnc_doclink_pref_path_head_changed (parent, old_head);
    dialog = find_path_head_dialog ();
    g_assert_nonnull (dialog);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_null (find_path_head_dialog ());
    g_assert_cmpstr (xaccTransGetDocLink (trans), ==, "document.pdf");

    set_doclink (trans, absolute_link);
    gnc_doclink_pref_path_head_changed (parent, old_head);
    g_assert_nonnull (find_path_head_dialog ());
    gtk_widget_destroy (GTK_WIDGET (parent));
    g_assert_null (find_path_head_dialog ());
    g_assert_cmpstr (xaccTransGetDocLink (trans), ==, absolute_link);
    gnc_clear_current_session ();
}

static void
destroy_parent ([[maybe_unused]] GtkWidget *dialog, gpointer parent)
{
    gtk_widget_destroy (GTK_WIDGET (parent));
}

struct LinkReplacement
{
    Transaction *trans;
    bool replaced;
};

static void
replace_link_during_commit (QofInstance *instance, QofEventId event,
                            gpointer user_data, [[maybe_unused]] gpointer event_data)
{
    auto state = static_cast<LinkReplacement *> (user_data);
    if (instance != QOF_INSTANCE (state->trans) ||
        !(event & QOF_EVENT_MODIFY) || state->replaced)
        return;
    state->replaced = true;
    xaccTransSetDocLink (state->trans, "file:///newer/document.pdf");
}

static void
test_reentrant_link_replacement ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    gnc_set_current_session (qof_session_new (qof_book_new ()));
    g_assert_true (memory_set_string (GNC_PREFS_GROUP_GENERAL,
                                      GNC_DOC_LINK_PATH_HEAD, "file:///new-head/"));
    auto trans = new_transaction (gnc_get_current_book ());
    set_doclink (trans, "file:document.pdf");
    LinkReplacement state{trans, false};
    auto handler = qof_event_register_handler (replace_link_during_commit, &state);
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    gnc_doclink_pref_path_head_changed (parent, "file:///old-head/");
    auto dialog = find_path_head_dialog ();
    g_assert_nonnull (dialog);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    qof_event_unregister_handler (handler);
    g_assert_true (state.replaced);
    g_assert_cmpstr (xaccTransGetDocLink (trans), ==, "file:///newer/document.pdf");
    gtk_widget_destroy (GTK_WIDGET (parent));
    gnc_clear_current_session ();
}

static void
test_stale_response (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto scenario = GPOINTER_TO_INT (data);
    g_assert_true (memory_set_string (GNC_PREFS_GROUP_GENERAL,
                                      GNC_DOC_LINK_PATH_HEAD, "file:///new-head/"));
    auto session = qof_session_new (qof_book_new ());
    gnc_set_current_session (session);
    auto book = qof_session_get_book (session);
    QofBook *retained_book = nullptr;
    auto trans = new_transaction (book);
    const char *absolute = "file:///new-head/document.pdf";
    set_doclink (trans, absolute);
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    gnc_doclink_pref_path_head_changed (parent, "file:///old-head/");
    auto dialog = find_path_head_dialog ();
    g_assert_nonnull (dialog);
    g_object_ref (dialog);
    if (scenario == 0)
        qof_book_mark_readonly (book);
    else if (scenario == 1)
        gnc_set_current_session (qof_session_new (qof_book_new ()));
    else if (scenario == 3)
        g_assert_true (memory_set_string (GNC_PREFS_GROUP_GENERAL,
                                          GNC_DOC_LINK_PATH_HEAD,
                                          "file:///different-head/"));
    else if (scenario == 4)
    {
        retained_book = QOF_BOOK (g_object_ref (book));
        /* Release the instance's collection membership before session teardown
         * frees collections. The retained GObject remains alive but its book
         * contents are destroyed by gnc_clear_current_session below. */
        g_object_run_dispose (G_OBJECT (retained_book));
        gnc_clear_current_session ();
        gnc_set_current_session (qof_session_new (qof_book_new ()));
        trans = new_transaction (gnc_get_current_book ());
        set_doclink (trans, absolute);
    }
    else
        g_signal_connect (dialog, "destroy", G_CALLBACK (destroy_parent), parent);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_null (find_path_head_dialog ());
    g_assert_cmpstr (xaccTransGetDocLink (trans), ==, absolute);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_cmpstr (xaccTransGetDocLink (trans), ==, absolute);
    g_object_unref (dialog);
    if (scenario != 2)
        gtk_widget_destroy (GTK_WIDGET (parent));
    if (scenario == 1)
    {
        gnc_clear_current_session ();
        qof_session_destroy (session);
    }
    else
        gnc_clear_current_session ();
    g_clear_object (&retained_book);
}

int
main (int argc, char **argv)
{
    PrefsBackend backend{};
    auto saved_backend = prefsbackend;
    backend.get_string = memory_get_string;
    backend.set_string = memory_set_string;
    prefsbackend = &backend;
    path_head = g_strdup ("file:///new-head/");
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    g_test_add_func ("/gnome-utils/doclink-path-head/response",
                     test_path_head_response);
    g_test_add_func ("/gnome-utils/doclink-path-head/reentrant-link-replacement",
                     test_reentrant_link_replacement);
    g_test_add_data_func ("/gnome-utils/doclink-path-head/read-only",
                          GINT_TO_POINTER (0), test_stale_response);
    g_test_add_data_func ("/gnome-utils/doclink-path-head/session-switch",
                          GINT_TO_POINTER (1), test_stale_response);
    g_test_add_data_func ("/gnome-utils/doclink-path-head/parent-destroy-during-response",
                          GINT_TO_POINTER (2), test_stale_response);
    g_test_add_data_func ("/gnome-utils/doclink-path-head/preference-drift",
                          GINT_TO_POINTER (3), test_stale_response);
    g_test_add_data_func ("/gnome-utils/doclink-path-head/book-destroy-with-retained-object",
                          GINT_TO_POINTER (4), test_stale_response);
    auto result = g_test_run ();
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    qof_close ();
    prefsbackend = saved_backend;
    g_free (path_head);
    return result;
}
