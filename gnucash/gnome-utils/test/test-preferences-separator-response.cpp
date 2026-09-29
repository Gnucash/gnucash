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
#include "gnc-component-manager.h"
#include "gnc-prefs-p.h"
#include "gnc-session.h"

extern "C" void gnc_preferences_response_cb (GtkDialog *, gint, GtkDialog *);

static gboolean display_available;

static void
observe_reset (GtkEditable *entry, guint *resets)
{
    if (g_strcmp0 (gtk_entry_get_text (GTK_ENTRY (entry)), ":") == 0)
        ++*resets;
}

static void
destroy_on_reset ([[maybe_unused]] GtkEditable *entry, GtkWidget *parent)
{
    gtk_widget_destroy (parent);
}

static void
test_response (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto book = qof_book_new ();
    auto root = gnc_account_create_root (book);
    auto account = xaccMallocAccount (book);
    xaccAccountSetName (account, "Account-Conflict");
    gnc_account_append_child (root, account);
    gnc_set_current_session (qof_session_new (book));
    auto parent = GTK_DIALOG (gtk_dialog_new ());
    auto content = gtk_dialog_get_content_area (parent);
    auto entry = gtk_entry_new ();
    gtk_entry_set_text (GTK_ENTRY (entry), "-");
    g_object_set_data (G_OBJECT (entry), "original_text", const_cast<char *> (":"));
    g_object_set_data (G_OBJECT (parent), "account-separator", entry);
    gtk_container_add (GTK_CONTAINER (content), entry);
    auto notebook = gtk_notebook_new ();
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
    auto question = GTK_DIALOG (g_object_get_data (G_OBJECT (parent), "separator-question"));
    g_assert_nonnull (question);
    g_object_ref (question);
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (question)));
    gnc_preferences_response_cb (parent, GTK_RESPONSE_CLOSE, nullptr);
    g_assert_true (g_object_get_data (G_OBJECT (parent), "separator-question") == question);
    auto mode = GPOINTER_TO_INT (data);
    guint resets = 0;
    g_signal_connect (entry, "changed", G_CALLBACK (observe_reset), &resets);
    if (mode == 2)
        gtk_widget_destroy (GTK_WIDGET (parent));
    else if (mode == 4)
        gtk_widget_destroy (GTK_WIDGET (question));
    else
    {
        if (mode == 3)
            g_signal_connect (entry, "changed", G_CALLBACK (destroy_on_reset), parent);
        gtk_dialog_response (question, mode == 0 ? GTK_RESPONSE_CANCEL : GTK_RESPONSE_ACCEPT);
    }
    if (mode == 0 || mode == 4)
    {
        g_assert_cmpstr (gtk_entry_get_text (GTK_ENTRY (entry)), ==, "-");
        g_assert_cmpint (gtk_notebook_get_current_page (GTK_NOTEBOOK (notebook)), ==, 1);
        g_assert_true (gtk_widget_get_visible (GTK_WIDGET (parent)));
        gtk_widget_destroy (GTK_WIDGET (parent));
    }
    else if (mode == 1 || mode == 3)
    {
        /* GTK clears entry contents during destruction: observe the actual
         * reset before teardown instead of inspecting a destroyed widget. */
        g_assert_cmpuint (resets, ==, 1);
        g_assert_false (gtk_widget_get_visible (GTK_WIDGET (parent)));
    }
    g_assert_null (g_object_get_data (G_OBJECT (parent), "separator-question"));
    /* Holding the destroyed question must not permit a second continuation. */
    gtk_dialog_response (question, GTK_RESPONSE_ACCEPT);
    g_object_unref (question);
    g_object_unref (entry);
    g_object_unref (parent);
    gnc_clear_current_session ();
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    PrefsBackend memory_backend{};
    auto saved_backend = prefsbackend;
    prefsbackend = &memory_backend;
    g_test_add_data_func ("/gnome-utils/separator/back-to-accounts", GINT_TO_POINTER (0), test_response);
    g_test_add_data_func ("/gnome-utils/separator/reset-and-close", GINT_TO_POINTER (1), test_response);
    g_test_add_data_func ("/gnome-utils/separator/parent-destroyed", GINT_TO_POINTER (2), test_response);
    g_test_add_data_func ("/gnome-utils/separator/reentrant-reset", GINT_TO_POINTER (3), test_response);
    g_test_add_data_func ("/gnome-utils/separator/question-destroyed", GINT_TO_POINTER (4), test_response);
    auto result = g_test_run ();
    prefsbackend = saved_backend;
    gnc_component_manager_shutdown ();
    qof_close ();
    return result;
}
