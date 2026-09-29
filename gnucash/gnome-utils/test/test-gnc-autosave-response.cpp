/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>
#include <gtk/gtk.h>
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

static gboolean display_available;
static QofBook *book;
static GtkWidget *parent;
static QofSession *test_session;
static QofSession *original_session;
static std::unordered_map<std::string, gboolean> bool_prefs;
static std::unordered_map<std::string, gdouble> float_prefs;
static enum class PrefHook { none, switch_session, destroy_session, destroy_parent } pref_hook;

static std::string
pref_key (const gchar *group, const gchar *name)
{
    return std::string (group) + "/" + name;
}

static gboolean
memory_get_bool (const gchar *group, const gchar *name)
{
    auto it = bool_prefs.find (pref_key (group, name));
    return it == bool_prefs.end () ? FALSE : it->second;
}

static gdouble
memory_get_float (const gchar *group, const gchar *name)
{
    auto it = float_prefs.find (pref_key (group, name));
    return it == float_prefs.end () ? 0.0 : it->second;
}

static gboolean
memory_set_bool (const gchar *group, const gchar *name, gboolean value)
{
    bool_prefs[pref_key (group, name)] = value;
    if (pref_key (group, name) != pref_key (GNC_PREFS_GROUP_GENERAL,
                                            "autosave-show-explanation"))
        return TRUE;

    if (pref_hook == PrefHook::destroy_parent)
    {
        pref_hook = PrefHook::none;
        gtk_widget_destroy (parent);
        parent = nullptr;
    }
    else if (pref_hook == PrefHook::switch_session)
    {
        pref_hook = PrefHook::none;
        test_session = qof_session_new (qof_book_new ());
        gnc_set_current_session (test_session);
    }
    else if (pref_hook == PrefHook::destroy_session)
    {
        pref_hook = PrefHook::none;
        gnc_clear_current_session ();
        test_session = nullptr;
        book = nullptr;
    }
    return TRUE;
}

static gboolean
memory_set_float (const gchar *group, const gchar *name, gdouble value)
{
    float_prefs[pref_key (group, name)] = value;
    return TRUE;
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
            g_assert_null (dialog);
            dialog = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return dialog;
}

static guint
timer_id ()
{
    return GPOINTER_TO_UINT (qof_book_get_data (book, "autosave_source_id"));
}

static void
fire_timer ()
{
    auto id = timer_id ();
    g_assert_cmpuint (id, >, 0);
    auto source = g_main_context_find_source_by_id (nullptr, id);
    g_assert_nonnull (source);
    g_source_set_ready_time (source, 0);
    for (guint attempt = 0; !find_confirmation () && attempt < 1000; ++attempt)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        if (!find_confirmation ())
            g_usleep (1000);
    }
    g_assert_nonnull (find_confirmation ());
}

static void
drain_events ()
{
    for (guint attempt = 0; attempt < 1000; ++attempt)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        if (!find_confirmation ())
            return;
        g_usleep (1000);
    }
    g_assert_null (find_confirmation ());
}

static void
test_consumed_timer_after_session_switch ()
{
    auto temporary_session = qof_session_new (qof_book_new ());
    auto temporary_book = qof_session_get_book (temporary_session);
    gnc_set_current_session (temporary_session);
    gnc_prefs_set_float (GNC_PREFS_GROUP_GENERAL, "autosave-interval-minutes", 1);
    gnc_autosave_dirty_handler (temporary_book, TRUE);
    auto id = GPOINTER_TO_UINT (qof_book_get_data (temporary_book,
                                                 "autosave_source_id"));
    auto source = g_main_context_find_source_by_id (nullptr, id);
    g_assert_nonnull (source);
    gnc_set_current_session (test_session);
    g_source_set_ready_time (source, 0);
    while (g_main_context_iteration (nullptr, FALSE))
        ;
    g_assert_null (g_main_context_find_source_by_id (nullptr, id));
    g_assert_null (qof_book_get_data (temporary_book, "autosave_source_id"));
    /* Cleanup must not try to remove the already-consumed source. */
    gnc_autosave_remove_timer (temporary_book);
    qof_session_destroy (temporary_session);
}

static void
test_response_and_close ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    gnc_prefs_set_float (GNC_PREFS_GROUP_GENERAL, "autosave-interval-minutes", 1);
    gnc_prefs_set_bool (GNC_PREFS_GROUP_GENERAL, "autosave-show-explanation", TRUE);
    gnc_account_create_root (book);
    qof_book_mark_session_dirty (book);
    g_assert_true (qof_book_session_not_saved (book));
    gnc_autosave_dirty_handler (book, TRUE);
    fire_timer ();
    auto dialog = find_confirmation ();
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    g_object_ref (dialog);
    gtk_dialog_response (GTK_DIALOG (dialog), 4); /* No, not this time */
    drain_events ();
    gtk_dialog_response (GTK_DIALOG (dialog), 4); /* Must be inert after finish. */
    g_object_unref (dialog);
    g_assert_true (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                                       "autosave-show-explanation"));
    g_assert_cmpuint (timer_id (), >, 0);

    /* Preference notifications may destroy the window after an affirmative
     * answer. That must not start Save/Save As with a destroyed parent. */
    fire_timer ();
    dialog = find_confirmation ();
    gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));
    pref_hook = PrefHook::destroy_parent;
    gtk_dialog_response (GTK_DIALOG (dialog), 1);
    g_assert_null (parent);
    g_assert_null (find_confirmation ());
    g_assert_true (qof_book_session_not_saved (book));
    g_assert_cmpuint (timer_id (), >, 0);
    parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_show (parent);

    fire_timer ();
    dialog = find_confirmation ();
    gtk_window_close (GTK_WINDOW (dialog));
    drain_events ();
    g_assert_true (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                                       "autosave-show-explanation"));
    g_assert_cmpuint (timer_id (), >, 0);

    fire_timer ();
    dialog = find_confirmation ();
    gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));
    gtk_widget_destroy (parent); /* destroy-with-parent cancellation */
    parent = nullptr;
    drain_events ();
    g_assert_null (find_confirmation ());
    g_assert_true (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                                       "autosave-show-explanation"));

    /* A response after a session switch must not act on the replacement. */
    fire_timer ();
    auto switched_dialog = find_confirmation ();
    pref_hook = PrefHook::switch_session;
    gtk_dialog_response (GTK_DIALOG (switched_dialog), 4);
    g_assert_null (find_confirmation ());
    g_assert_true (gnc_current_session_exist ());
    g_assert_true (qof_session_get_book (gnc_get_current_session ()) != book);
    gnc_autosave_remove_timer (book);
    qof_session_destroy (original_session);
    original_session = nullptr;
    book = qof_session_get_book (test_session);

    /* A preference callback may destroy the original book reentrantly. */
    gnc_account_create_root (book);
    qof_book_mark_session_dirty (book);
    gnc_autosave_dirty_handler (book, TRUE);
    fire_timer ();
    auto destroy_dialog = find_confirmation ();
    pref_hook = PrefHook::destroy_session;
    gtk_dialog_response (GTK_DIALOG (destroy_dialog), 4);
    g_assert_null (find_confirmation ());
    g_assert_false (gnc_current_session_exist ());
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    auto saved_backend = prefsbackend;
    prefsbackend = &memory_backend;
    g_assert_true (cashobjects_register ());
    test_session = qof_session_new (qof_book_new ());
    original_session = test_session;
    book = qof_session_get_book (test_session);
    gnc_set_current_session (test_session);
    if (display_available)
    {
        parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        gtk_widget_show (parent);
    }
    g_test_add_func ("/gnome-utils/autosave/consumed-timer-session-switch",
                     test_consumed_timer_after_session_switch);
    g_test_add_func ("/gnome-utils/autosave/response-close",
                     test_response_and_close);
    auto result = g_test_run ();
    if (book)
        gnc_autosave_remove_timer (book);
    if (parent)
        gtk_widget_destroy (parent);
    if (gnc_current_session_exist ())
    {
        if (gnc_get_current_session () == original_session)
            original_session = nullptr; /* clear_current_session owns it */
        gnc_clear_current_session ();
    }
    if (original_session)
        qof_session_destroy (original_session);
    qof_close ();
    prefsbackend = saved_backend;
    return result;
}
