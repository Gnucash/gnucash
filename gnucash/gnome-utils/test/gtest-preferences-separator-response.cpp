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

#include "Account.h"
#include "cashobjects.h"
#include "gnc-component-manager.h"
#include "gnc-prefs-p.h"
#include "gnc-session.h"

extern "C" void gnc_preferences_response_cb (GtkDialog *, gint, GtkDialog *);

static void
observe_reset (GtkEditable *entry, std::uint32_t *resets)
{
    if (g_strcmp0 (gtk_entry_get_text (GTK_ENTRY (entry)), ":") == 0)
        ++*resets;
}

static void
destroy_on_reset ([[maybe_unused]] GtkEditable *entry, GtkWidget *parent)
{
    gtk_widget_destroy (parent);
}

class PreferencesSeparatorResponseTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        auto book = qof_book_new ();
        auto root = gnc_account_create_root (book);
        auto account = xaccMallocAccount (book);
        xaccAccountSetName (account, "Account-Conflict");
        gnc_account_append_child (root, account);
        gnc_set_current_session (qof_session_new (book));
        parent = GTK_DIALOG (gtk_dialog_new ());
        auto content = gtk_dialog_get_content_area (parent);
        entry = gtk_entry_new ();
        gtk_entry_set_text (GTK_ENTRY (entry), "-");
        g_object_set_data (G_OBJECT (entry), "original_text", const_cast<char *> (":"));
        g_object_set_data (G_OBJECT (parent), "account-separator", entry);
        gtk_container_add (GTK_CONTAINER (content), entry);
        notebook = gtk_notebook_new ();
        auto first = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
        auto accounts = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
        gtk_widget_set_name (accounts, "accounts_page");
        gtk_notebook_append_page (GTK_NOTEBOOK (notebook), first, nullptr);
        gtk_notebook_append_page (GTK_NOTEBOOK (notebook), accounts, nullptr);
        gtk_container_add (GTK_CONTAINER (content), notebook);
        g_object_set_data (G_OBJECT (parent), "notebook", notebook);
        gtk_widget_show_all (GTK_WIDGET (parent));
        g_object_ref (parent);
        g_object_ref (entry);
        gnc_preferences_response_cb (parent, GTK_RESPONSE_CLOSE, nullptr);
        question = GTK_DIALOG (g_object_get_data (G_OBJECT (parent), "separator-question"));
        ASSERT_NE (question, nullptr);
        g_object_ref (question);
        ASSERT_TRUE (gtk_window_get_modal (GTK_WINDOW (question)));
        gnc_preferences_response_cb (parent, GTK_RESPONSE_CLOSE, nullptr);
        ASSERT_EQ (g_object_get_data (G_OBJECT (parent), "separator-question"), question);
        g_signal_connect (entry, "changed", G_CALLBACK (observe_reset), &resets);
    }
    void TearDown () override
    {
        if (parent && !gtk_widget_in_destruction (GTK_WIDGET (parent)))
            gtk_widget_destroy (GTK_WIDGET (parent));
        if (question) g_object_unref (question);
        if (entry) g_object_unref (entry);
        if (parent) g_object_unref (parent);
        gnc_clear_current_session ();
    }
    GtkDialog *parent{};
    GtkWidget *entry{};
    GtkWidget *notebook{};
    GtkDialog *question{};
    std::uint32_t resets{};
};

TEST_F (PreferencesSeparatorResponseTest, CancelReturnsToAccounts)
{
    gtk_dialog_response (question, GTK_RESPONSE_CANCEL);
    EXPECT_STREQ (gtk_entry_get_text (GTK_ENTRY (entry)), "-");
    EXPECT_EQ (gtk_notebook_get_current_page (GTK_NOTEBOOK (notebook)), 1);
    EXPECT_TRUE (gtk_widget_get_visible (GTK_WIDGET (parent)));
    EXPECT_EQ (g_object_get_data (G_OBJECT (parent), "separator-question"), nullptr);
}

TEST_F (PreferencesSeparatorResponseTest, AcceptResetsSeparatorAndCloses)
{
    gtk_dialog_response (question, GTK_RESPONSE_ACCEPT);
    EXPECT_EQ (resets, 1u);
    EXPECT_FALSE (gtk_widget_get_visible (GTK_WIDGET (parent)));
    EXPECT_EQ (g_object_get_data (G_OBJECT (parent), "separator-question"), nullptr);
    gtk_dialog_response (question, GTK_RESPONSE_ACCEPT);
    EXPECT_EQ (resets, 1u);
}

TEST_F (PreferencesSeparatorResponseTest, ParentDestroyCancelsQuestion)
{
    gtk_widget_destroy (GTK_WIDGET (parent));
    EXPECT_EQ (g_object_get_data (G_OBJECT (parent), "separator-question"), nullptr);
    EXPECT_EQ (resets, 0u);
    gtk_dialog_response (question, GTK_RESPONSE_ACCEPT);
    EXPECT_EQ (resets, 0u);
}

TEST_F (PreferencesSeparatorResponseTest, ReentrantResetCanDestroyParent)
{
    g_signal_connect (entry, "changed", G_CALLBACK (destroy_on_reset), parent);
    gtk_dialog_response (question, GTK_RESPONSE_ACCEPT);
    EXPECT_EQ (resets, 1u);
    EXPECT_FALSE (gtk_widget_get_visible (GTK_WIDGET (parent)));
    EXPECT_EQ (g_object_get_data (G_OBJECT (parent), "separator-question"), nullptr);
    gtk_dialog_response (question, GTK_RESPONSE_ACCEPT);
    EXPECT_EQ (resets, 1u);
}

TEST_F (PreferencesSeparatorResponseTest, DestroyedQuestionCannotContinue)
{
    gtk_widget_destroy (GTK_WIDGET (question));
    EXPECT_STREQ (gtk_entry_get_text (GTK_ENTRY (entry)), "-");
    EXPECT_EQ (gtk_notebook_get_current_page (GTK_NOTEBOOK (notebook)), 1);
    EXPECT_TRUE (gtk_widget_get_visible (GTK_WIDGET (parent)));
    EXPECT_EQ (g_object_get_data (G_OBJECT (parent), "separator-question"), nullptr);
    gtk_dialog_response (question, GTK_RESPONSE_ACCEPT);
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("A graphical display is required for preferences separator tests");
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    PrefsBackend memory_backend{};
    auto saved_backend = prefsbackend;
    prefsbackend = &memory_backend;
    gnc::test::initialize_logging ();
    auto result = RUN_ALL_TESTS ();
    prefsbackend = saved_backend;
    gnc_component_manager_shutdown ();
    qof_close ();
    return result;
}
