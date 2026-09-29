/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>
#include <gtk/gtk.h>

#include "dialog-new-user.h"
#include "gnc-prefs-p.h"

static gboolean display_available;
static guint preference_writes;
static gboolean first_startup;
static GtkWidget *destroy_on_write;

static gboolean
set_bool (const gchar *group, const gchar *name, gboolean value)
{
    g_assert_cmpstr (group, ==, GNC_PREFS_GROUP_NEW_USER);
    g_assert_cmpstr (name, ==, GNC_PREF_FIRST_STARTUP);
    ++preference_writes;
    first_startup = value;
    if (destroy_on_write)
    {
        auto window = destroy_on_write;
        destroy_on_write = nullptr;
        gtk_widget_destroy (window);
    }
    return TRUE;
}

static GtkWidget *
find_named (GtkWidget *widget, const char *name)
{
    if (GTK_IS_BUILDABLE (widget) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (widget)), name) == 0)
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto child = children; child && !found; child = child->next)
        found = find_named (GTK_WIDGET (child->data), name);
    g_list_free (children);
    return found;
}

static GtkWidget *
find_window (const char *name)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto window = windows; window; window = window->next)
        if (g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (window->data)),
                       name) == 0)
        {
            g_assert_null (found);
            found = GTK_WIDGET (window->data);
        }
    g_list_free (windows);
    return found;
}

static void
test_response (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    preference_writes = 0;
    first_startup = TRUE;
    destroy_on_write = nullptr;
    gnc_ui_new_user_dialog ();
    auto window = find_window ("new_user_window");
    g_assert_nonnull (window);
    auto mode = GPOINTER_TO_INT (data);
    if (mode == 2)
        gtk_widget_destroy (window);
    else
    {
        auto cancel = find_named (window, "cancel_but");
        g_assert_nonnull (cancel);
        gtk_button_clicked (GTK_BUTTON (cancel));
        auto question = find_window ("new_user_cancel_dialog");
        g_assert_nonnull (question);
        g_assert_cmpuint (preference_writes, ==, 0);
        g_assert_true (gtk_window_get_modal (GTK_WINDOW (question)));
        g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (question)));
        gtk_button_clicked (GTK_BUTTON (cancel));
        g_assert_true (find_window ("new_user_cancel_dialog") == question);
        if (mode == 3)
            gtk_widget_destroy (window);
        else if (mode == 5)
        {
            g_object_ref (question);
            gtk_widget_destroy (question);
            gtk_dialog_response (GTK_DIALOG (question), GTK_RESPONSE_YES);
            g_object_unref (question);
        }
        else
        {
            if (mode == 4)
                destroy_on_write = window;
            gtk_dialog_response (GTK_DIALOG (question),
                                 mode == 1 || mode == 4 ? GTK_RESPONSE_YES :
                                                        GTK_RESPONSE_NO);
        }
    }
    g_assert_cmpuint (preference_writes, ==, 1);
    g_assert_cmpint (first_startup, ==, mode == 1 || mode == 4);
    g_assert_null (find_window ("new_user_window"));
    g_assert_null (find_window ("new_user_cancel_dialog"));
    /* The original idle presentation must not survive either close path. */
    while (g_main_context_iteration (nullptr, FALSE))
        ;
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    PrefsBackend memory_backend{};
    memory_backend.set_bool = set_bool;
    auto saved_backend = prefsbackend;
    prefsbackend = &memory_backend;
    g_test_add_data_func ("/gnome/new-user/no", GINT_TO_POINTER (0), test_response);
    g_test_add_data_func ("/gnome/new-user/yes", GINT_TO_POINTER (1), test_response);
    g_test_add_data_func ("/gnome/new-user/destroy-before-idle", GINT_TO_POINTER (2), test_response);
    g_test_add_data_func ("/gnome/new-user/parent-destroyed", GINT_TO_POINTER (3), test_response);
    g_test_add_data_func ("/gnome/new-user/reentrant-preference", GINT_TO_POINTER (4), test_response);
    g_test_add_data_func ("/gnome/new-user/question-destroyed-late-response", GINT_TO_POINTER (5), test_response);
    auto result = g_test_run ();
    prefsbackend = saved_backend;
    return result;
}
