/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>
#include <cstdint>
#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include "googletest-glib-log-handler.hpp"
#include <string>

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

static gchar *path_head;
enum class StaleResponseCase { read_only, session_switch, parent_destroy,
                               preference_drift, disposed_book };

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
        return false;
    g_free (path_head);
    path_head = g_strdup (value);
    return true;
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
    for (std::uint32_t i = 0; i < 2; ++i)
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

struct LinkReplacement
{
    Transaction *trans{};
    bool replaced{};
};

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
            EXPECT_EQ (dialog, nullptr);
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

template <typename TestBase>
class DoclinkPathHeadFixture : public TestBase
{
protected:
    void SetUp () override
    {
        m_session = qof_session_new (qof_book_new ());
        gnc_set_current_session (m_session);
        m_book = qof_session_get_book (m_session);
        m_trans = new_transaction (m_book);
        m_parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
        g_object_ref_sink (m_parent);
    }
    void TearDown () override
    {
        if (m_handler)
        {
            qof_event_unregister_handler (m_handler);
            m_handler = 0;
        }
        if (m_parent)
        {
            gtk_widget_destroy (GTK_WIDGET (m_parent));
            g_object_unref (m_parent);
        }
        auto current = gnc_exchange_current_session (nullptr);
        if (current)
            qof_session_destroy (current);
        if (m_session && m_session != current)
            qof_session_destroy (m_session);
        g_clear_object (&m_retained_book);
        if (m_dialog)
            g_object_unref (m_dialog);
    }
    QofSession *m_session{};
    QofBook *m_book{};
    Transaction *m_trans{};
    GtkWindow *m_parent{};
    GtkWidget *m_dialog{};
    QofBook *m_retained_book{};
    LinkReplacement m_replacement{};
    std::int32_t m_handler{};
    void begin_stale_prompt ()
    {
        ASSERT_TRUE (memory_set_string (GNC_PREFS_GROUP_GENERAL,
                                        GNC_DOC_LINK_PATH_HEAD,
                                        "file:///new-head/"));
        set_doclink (m_trans, "file:///new-head/document.pdf");
        gnc_doclink_pref_path_head_changed (m_parent, "file:///old-head/");
        m_dialog = find_path_head_dialog ();
        ASSERT_NE (m_dialog, nullptr);
        g_object_ref (m_dialog);
    }
};

using DoclinkPathHeadTest = DoclinkPathHeadFixture<::testing::Test>;
using DoclinkStaleResponseTest =
    DoclinkPathHeadFixture<::testing::TestWithParam<StaleResponseCase>>;

static std::string
stale_response_name (const ::testing::TestParamInfo<StaleResponseCase> &info)
{
    const char *names[] = {"ReadOnly", "SessionSwitch", "ParentDestroy",
                           "PreferenceDrift", "DisposedBookRetainedObject"};
    return names[static_cast<int> (info.param)];
}

TEST_F (DoclinkPathHeadTest, CancelThenAcceptRewritesTheMatchingLink)
{
    auto trans = m_trans;
    const gchar *new_head = "file:///new-head/";
    const gchar *old_head = "file:///old-head/";
    const gchar *absolute_link = "file:///new-head/document.pdf";
    ASSERT_TRUE (gnc_prefs_set_string (GNC_PREFS_GROUP_GENERAL,
                                       GNC_DOC_LINK_PATH_HEAD, new_head));
    set_doclink (trans, absolute_link);

    auto borrowed_old_head = g_strdup (old_head);
    gnc_doclink_pref_path_head_changed (m_parent, borrowed_old_head);
    g_free (borrowed_old_head);

    auto dialog = find_path_head_dialog ();
    ASSERT_NE (dialog, nullptr);
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    EXPECT_STREQ (xaccTransGetDocLink (trans), absolute_link);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);
    EXPECT_EQ (find_path_head_dialog (), nullptr);
    EXPECT_STREQ (xaccTransGetDocLink (trans), absolute_link);

    gnc_doclink_pref_path_head_changed (m_parent, old_head);
    dialog = find_path_head_dialog ();
    ASSERT_NE (dialog, nullptr);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_EQ (find_path_head_dialog (), nullptr);
    EXPECT_STREQ (xaccTransGetDocLink (trans), "document.pdf");

    set_doclink (trans, absolute_link);
    gnc_doclink_pref_path_head_changed (m_parent, old_head);
    ASSERT_NE (find_path_head_dialog (), nullptr);
    gtk_widget_destroy (GTK_WIDGET (m_parent));
    EXPECT_EQ (find_path_head_dialog (), nullptr);
    EXPECT_STREQ (xaccTransGetDocLink (trans), absolute_link);
}

static void
destroy_parent ([[maybe_unused]] GtkWidget *dialog, gpointer parent)
{
    gtk_widget_destroy (GTK_WIDGET (parent));
}

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

TEST_F (DoclinkPathHeadTest, ReentrantLinkReplacementWins)
{
    EXPECT_TRUE (memory_set_string (GNC_PREFS_GROUP_GENERAL,
                                      GNC_DOC_LINK_PATH_HEAD, "file:///new-head/"));
    auto trans = m_trans;
    set_doclink (trans, "file:document.pdf");
    m_replacement = {trans, false};
    m_handler = qof_event_register_handler (replace_link_during_commit,
                                            &m_replacement);
    gnc_doclink_pref_path_head_changed (m_parent, "file:///old-head/");
    auto dialog = find_path_head_dialog ();
    ASSERT_NE (dialog, nullptr);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_TRUE (m_replacement.replaced);
    EXPECT_STREQ (xaccTransGetDocLink (trans), "file:///newer/document.pdf");
}

TEST_P (DoclinkStaleResponseTest, RejectsStaleResponse)
{
    const auto scenario = GetParam ();
    auto book = m_book;
    auto trans = m_trans;
    const char *absolute = "file:///new-head/document.pdf";
    set_doclink (trans, absolute);
    gnc_doclink_pref_path_head_changed (m_parent, "file:///old-head/");
    m_dialog = find_path_head_dialog ();
    ASSERT_NE (m_dialog, nullptr);
    g_object_ref (m_dialog);
    if (scenario == StaleResponseCase::read_only)
        qof_book_mark_readonly (book);
    else if (scenario == StaleResponseCase::session_switch)
        gnc_set_current_session (qof_session_new (qof_book_new ()));
    else if (scenario == StaleResponseCase::preference_drift)
        ASSERT_TRUE (memory_set_string (GNC_PREFS_GROUP_GENERAL,
                                        GNC_DOC_LINK_PATH_HEAD,
                                        "file:///different-head/"));
    else if (scenario == StaleResponseCase::disposed_book)
    {
        m_retained_book = QOF_BOOK (g_object_ref (book));
        /* Release the instance's collection membership before session teardown
         * frees collections. The retained GObject remains alive but its book
         * contents are destroyed by gnc_clear_current_session below. */
        g_object_run_dispose (G_OBJECT (m_retained_book));
        gnc_clear_current_session ();
        m_session = nullptr;
        m_book = nullptr;
        m_trans = nullptr;
        gnc_set_current_session (qof_session_new (qof_book_new ()));
        m_session = gnc_get_current_session ();
        m_book = qof_session_get_book (m_session);
        m_trans = new_transaction (m_book);
        trans = m_trans;
        set_doclink (trans, absolute);
    }
    else
        g_signal_connect (m_dialog, "destroy", G_CALLBACK (destroy_parent), m_parent);
    gtk_dialog_response (GTK_DIALOG (m_dialog), GTK_RESPONSE_OK);
    EXPECT_EQ (find_path_head_dialog (), nullptr);
    EXPECT_STREQ (xaccTransGetDocLink (trans), absolute);
    gtk_dialog_response (GTK_DIALOG (m_dialog), GTK_RESPONSE_OK);
    EXPECT_STREQ (xaccTransGetDocLink (trans), absolute);
}

INSTANTIATE_TEST_SUITE_P (
    StaleSources, DoclinkStaleResponseTest,
    ::testing::Values (StaleResponseCase::read_only,
                       StaleResponseCase::session_switch,
                       StaleResponseCase::parent_destroy,
                       StaleResponseCase::preference_drift,
                       StaleResponseCase::disposed_book),
    stale_response_name);

int
main (int argc, char **argv)
{
    PrefsBackend backend{};
    auto saved_backend = prefsbackend;
    backend.get_string = memory_get_string;
    backend.set_string = memory_set_string;
    prefsbackend = &backend;
    path_head = g_strdup ("file:///new-head/");
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("A graphical display is required for doclink path-head tests");
    qof_init ();
    if (!cashobjects_register ())
        g_error ("Failed to register cash objects");
    gnc::test::initialize_logging ();
    auto result = RUN_ALL_TESTS ();
    qof_close ();
    prefsbackend = saved_backend;
    g_free (path_head);
    return result;
}
