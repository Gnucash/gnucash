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
#include "gnc-autosave.h"
#include "gnc-prefs.h"
#include "gnc-prefs-p.h"
#include "gnc-session.h"
#include "qof.h"
#include "qofbook.h"

#include <string>
#include <unordered_map>

enum class PrefHook { none, switch_session, destroy_session, destroy_parent };
struct AutosaveState
{
    QofBook *book{};
    GtkWidget *parent{};
    QofSession *test_session{};
    QofSession *original_session{};
    QofSession *temporary_session{};
    GtkWidget *retained_dialog{};
    std::unordered_map<std::string, bool> bool_prefs;
    std::unordered_map<std::string, gdouble> float_prefs;
    PrefHook pref_hook{PrefHook::none};
};
static AutosaveState *active_state{};

static std::string
pref_key (const gchar *group, const gchar *name)
{
    return std::string (group) + "/" + name;
}

static gboolean
memory_get_bool (const gchar *group, const gchar *name)
{
    auto it = active_state->bool_prefs.find (pref_key (group, name));
    return it == active_state->bool_prefs.end () ? false : it->second;
}

static gdouble
memory_get_float (const gchar *group, const gchar *name)
{
    auto it = active_state->float_prefs.find (pref_key (group, name));
    return it == active_state->float_prefs.end () ? 0.0 : it->second;
}

static gboolean
memory_set_bool (const gchar *group, const gchar *name, gboolean value)
{
    active_state->bool_prefs[pref_key (group, name)] = value;
    if (pref_key (group, name) != pref_key (GNC_PREFS_GROUP_GENERAL,
                                            "autosave-show-explanation"))
        return true;

    if (active_state->pref_hook == PrefHook::destroy_parent)
    {
        active_state->pref_hook = PrefHook::none;
        gtk_widget_destroy (active_state->parent);
        active_state->parent = nullptr;
    }
    else if (active_state->pref_hook == PrefHook::switch_session)
    {
        active_state->pref_hook = PrefHook::none;
        active_state->test_session = qof_session_new (qof_book_new ());
        gnc_set_current_session (active_state->test_session);
    }
    else if (active_state->pref_hook == PrefHook::destroy_session)
    {
        active_state->pref_hook = PrefHook::none;
        if (gnc_current_session_exist () &&
            gnc_get_current_session () == active_state->original_session)
            active_state->original_session = nullptr;
        gnc_clear_current_session ();
        active_state->test_session = nullptr;
        active_state->book = nullptr;
    }
    return true;
}

static gboolean
memory_set_float (const gchar *group, const gchar *name, gdouble value)
{
    active_state->float_prefs[pref_key (group, name)] = value;
    return true;
}

static PrefsBackend memory_backend = [] {
    PrefsBackend backend{};
    backend.get_bool = memory_get_bool;
    backend.get_float = memory_get_float;
    backend.set_bool = memory_set_bool;
    backend.set_float = memory_set_float;
    return backend;
} ();

static GtkWidget *
find_confirmation ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *dialog = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_DIALOG (node->data) &&
            g_strcmp0 (gtk_widget_get_name (GTK_WIDGET (node->data)),
                       "gnc-id-auto-save") == 0)
        {
            EXPECT_EQ (dialog, nullptr);
            dialog = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return dialog;
}

static std::uint32_t
timer_id ()
{
    return GPOINTER_TO_UINT (qof_book_get_data (active_state->book, "autosave_source_id"));
}

static void
fire_timer ()
{
    auto id = timer_id ();
    EXPECT_NE (id, 0u);
    if (id == 0u)
        return;
    auto source = g_main_context_find_source_by_id (nullptr, id);
    EXPECT_NE (source, nullptr);
    if (!source)
        return;
    g_source_set_ready_time (source, 0);
    for (std::uint32_t attempt = 0; !find_confirmation () && attempt < 1000; ++attempt)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        if (!find_confirmation ())
            g_usleep (1000);
    }
    EXPECT_NE (find_confirmation (), nullptr);
}

static void
drain_events ()
{
    for (std::uint32_t attempt = 0; attempt < 1000; ++attempt)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        if (!find_confirmation ())
            return;
        g_usleep (1000);
    }
    EXPECT_EQ (find_confirmation (), nullptr);
}

class AutosaveResponseTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        m_state = {};
        active_state = &m_state;
        active_state->test_session = qof_session_new (qof_book_new ());
        active_state->original_session = active_state->test_session;
        active_state->book = qof_session_get_book (active_state->test_session);
        gnc_set_current_session (active_state->test_session);
        active_state->parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        gtk_widget_show (active_state->parent);
    }
    void TearDown () override
    {
        if (active_state->book)
            gnc_autosave_remove_timer (active_state->book);
        if (active_state->parent)
        {
            gtk_widget_destroy (active_state->parent);
            active_state->parent = nullptr;
        }
        if (active_state->retained_dialog)
        {
            g_object_unref (active_state->retained_dialog);
            active_state->retained_dialog = nullptr;
        }
        if (gnc_current_session_exist ())
        {
            auto current = gnc_get_current_session ();
            if (current == active_state->original_session)
                active_state->original_session = nullptr;
            if (current == active_state->temporary_session)
                active_state->temporary_session = nullptr;
            gnc_clear_current_session ();
        }
        if (active_state->original_session)
        {
            qof_session_destroy (active_state->original_session);
            active_state->original_session = nullptr;
        }
        if (active_state->temporary_session)
        {
            qof_session_destroy (active_state->temporary_session);
            active_state->temporary_session = nullptr;
        }
        active_state->test_session = nullptr;
        active_state->book = nullptr;
        active_state->pref_hook = PrefHook::none;
        active_state = nullptr;
    }
    AutosaveState m_state{};
    void retain_dialog (GtkWidget *dialog)
    {
        if (m_state.retained_dialog)
            g_object_unref (m_state.retained_dialog);
        m_state.retained_dialog = GTK_WIDGET (g_object_ref (dialog));
    }
    void prepare_autosave ()
    {
        gnc_prefs_set_float (GNC_PREFS_GROUP_GENERAL,
                             "autosave-interval-minutes", 1);
        gnc_prefs_set_bool (GNC_PREFS_GROUP_GENERAL,
                            "autosave-show-explanation", TRUE);
        gnc_account_create_root (m_state.book);
        qof_book_mark_session_dirty (m_state.book);
        gnc_autosave_dirty_handler (m_state.book, TRUE);
        fire_timer ();
    }
};

TEST_F (AutosaveResponseTest, ConsumedTimerAfterSessionSwitch)
{
    active_state->temporary_session = qof_session_new (qof_book_new ());
    auto temporary_book = qof_session_get_book (active_state->temporary_session);
    gnc_set_current_session (active_state->temporary_session);
    gnc_prefs_set_float (GNC_PREFS_GROUP_GENERAL, "autosave-interval-minutes", 1);
    gnc_autosave_dirty_handler (temporary_book, TRUE);
    auto id = GPOINTER_TO_UINT (qof_book_get_data (temporary_book, "autosave_source_id"));
    ASSERT_GT (id, 0u);
    auto source = g_main_context_find_source_by_id (nullptr, id);
    ASSERT_NE (source, nullptr);
    gnc_set_current_session (active_state->test_session);
    g_source_set_ready_time (source, 0);
    while (g_main_context_iteration (nullptr, FALSE))
        ;
    EXPECT_EQ (g_main_context_find_source_by_id (nullptr, id), nullptr);
    EXPECT_EQ (qof_book_get_data (temporary_book, "autosave_source_id"), nullptr);
    gnc_autosave_remove_timer (temporary_book);
    qof_session_destroy (active_state->temporary_session);
    active_state->temporary_session = nullptr;
}

TEST_F (AutosaveResponseTest, DecliningExplanationReschedules)
{
    prepare_autosave ();
    auto dialog = find_confirmation ();
    ASSERT_NE (dialog, nullptr);
    retain_dialog (dialog);
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    gtk_dialog_response (GTK_DIALOG (dialog), 4);
    drain_events ();
    gtk_dialog_response (GTK_DIALOG (dialog), 4);
    EXPECT_TRUE (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, "autosave-show-explanation"));
    EXPECT_GT (timer_id (), 0u);
}

TEST_F (AutosaveResponseTest, PreferenceChangeDestroyingParentPreventsSave)
{
    prepare_autosave ();
    auto dialog = find_confirmation ();
    ASSERT_NE (dialog, nullptr);
    retain_dialog (dialog);
    gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (active_state->parent));
    active_state->pref_hook = PrefHook::destroy_parent;
    gtk_dialog_response (GTK_DIALOG (dialog), 1);
    EXPECT_EQ (active_state->parent, nullptr);
    EXPECT_EQ (find_confirmation (), nullptr);
    EXPECT_TRUE (qof_book_session_not_saved (active_state->book));
    EXPECT_GT (timer_id (), 0u);
}

TEST_F (AutosaveResponseTest, WindowCloseReschedules)
{
    prepare_autosave ();
    auto dialog = find_confirmation ();
    ASSERT_NE (dialog, nullptr);
    retain_dialog (dialog);
    gtk_window_close (GTK_WINDOW (dialog));
    drain_events ();
    EXPECT_TRUE (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, "autosave-show-explanation"));
    EXPECT_GT (timer_id (), 0u);
}

TEST_F (AutosaveResponseTest, ParentDestroyCancelsAndReschedules)
{
    prepare_autosave ();
    auto dialog = find_confirmation ();
    ASSERT_NE (dialog, nullptr);
    retain_dialog (dialog);
    gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (active_state->parent));
    gtk_widget_destroy (active_state->parent);
    active_state->parent = nullptr;
    drain_events ();
    EXPECT_EQ (find_confirmation (), nullptr);
    EXPECT_TRUE (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, "autosave-show-explanation"));
    EXPECT_GT (timer_id (), 0u);
}

TEST_F (AutosaveResponseTest, SessionSwitchMakesLateResponseInert)
{
    prepare_autosave ();
    auto dialog = find_confirmation ();
    ASSERT_NE (dialog, nullptr);
    retain_dialog (dialog);
    active_state->pref_hook = PrefHook::switch_session;
    gtk_dialog_response (GTK_DIALOG (dialog), 4);
    EXPECT_EQ (find_confirmation (), nullptr);
    ASSERT_TRUE (gnc_current_session_exist ());
    EXPECT_NE (qof_session_get_book (gnc_get_current_session ()), active_state->book);
    gnc_autosave_remove_timer (active_state->book);
    qof_session_destroy (active_state->original_session);
    active_state->original_session = nullptr;
    active_state->book = qof_session_get_book (active_state->test_session);
}

TEST_F (AutosaveResponseTest, PreferenceChangeDestroyingSessionCancels)
{
    auto old_session = active_state->test_session;
    active_state->test_session = qof_session_new (qof_book_new ());
    gnc_set_current_session (active_state->test_session);
    qof_session_destroy (old_session);
    active_state->original_session = nullptr;
    active_state->book = qof_session_get_book (active_state->test_session);
    prepare_autosave ();
    auto dialog = find_confirmation ();
    ASSERT_NE (dialog, nullptr);
    retain_dialog (dialog);
    active_state->pref_hook = PrefHook::destroy_session;
    gtk_dialog_response (GTK_DIALOG (dialog), 4);
    EXPECT_EQ (find_confirmation (), nullptr);
    EXPECT_FALSE (gnc_current_session_exist ());
}
int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("A graphical display is required for autosave response tests");
    qof_init ();
    auto saved_backend = prefsbackend;
    prefsbackend = &memory_backend;
    if (!cashobjects_register ())
        g_error ("Failed to register cash objects");
    gnc::test::initialize_logging ();
    auto result = RUN_ALL_TESTS ();
    prefsbackend = saved_backend;
    qof_close ();
    return result;
}
