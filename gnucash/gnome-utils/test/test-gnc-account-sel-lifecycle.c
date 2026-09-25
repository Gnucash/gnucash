/*
 * test-gnc-account-sel-lifecycle.c -- GncAccountSel popup lifetime tests
 *
 * Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "Account.h"
#include "gnc-account-sel.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-prefs-utils.h"
#include "gnc-session.h"
#include "qof.h"

static GtkEntry *
find_entry (GtkWidget *widget)
{
    GtkWidget *child;

    if (GTK_IS_ENTRY (widget))
        return GTK_ENTRY (widget);
    for (child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
    {
        GtkEntry *entry = find_entry (child);

        if (entry)
            return entry;
    }
    return NULL;
}

static void
collect_popovers (GtkWidget *widget, GPtrArray *popovers)
{
    GtkWidget *child;

    if (GTK_IS_POPOVER (widget))
        g_ptr_array_add (popovers, g_object_ref (widget));
    for (child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
        collect_popovers (child, popovers);
}

static void
assert_no_callbacks (GtkWidget *widget, gpointer data)
{
    GtkWidget *child;

    g_assert_cmpuint (g_signal_handler_find
                      (widget, G_SIGNAL_MATCH_DATA, 0, 0, NULL, NULL, data), ==, 0);
    for (child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
        assert_no_callbacks (child, data);
}

static void
test_popovers_detach_before_account_sel_dispose (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    GtkWidget *selector;
    GtkEntry *entry;
    GPtrArray *popovers;

    gnc_set_current_session (session);
    gnc_account_create_root (book);
    selector = gnc_account_sel_new ();
    g_object_ref_sink (selector);
    entry = find_entry (selector);
    g_assert_nonnull (entry);
    g_object_ref (entry);
    popovers = g_ptr_array_new_with_free_func (g_object_unref);
    collect_popovers (selector, popovers);
    g_assert_cmpuint (popovers->len, ==, 2);
    for (guint index = 0; index < popovers->len; index++)
        g_assert_true (gtk_widget_get_parent (g_ptr_array_index (popovers, index))
                       == GTK_WIDGET (entry));

    g_object_run_dispose (G_OBJECT (selector));
    g_object_run_dispose (G_OBJECT (selector));

    assert_no_callbacks (GTK_WIDGET (entry), selector);
    for (guint index = 0; index < popovers->len; index++)
    {
        GtkWidget *popover = g_ptr_array_index (popovers, index);

        g_assert_null (gtk_widget_get_parent (popover));
        assert_no_callbacks (popover, selector);
    }
    g_signal_emit_by_name (entry, "changed");

    g_object_unref (selector);
    g_object_unref (entry);
    g_ptr_array_unref (popovers);
    gnc_clear_current_session ();
}

int
main (int argc, char **argv)
{
    int status;

    g_setenv ("GSETTINGS_BACKEND", "memory", TRUE);
    g_test_init (&argc, &argv, NULL);
    gtk_init ();
    gnc_engine_init_static (argc, argv);
    gnc_prefs_init ();
    gnc_component_manager_init ();

    g_test_add_func ("/gnome-utils/account-sel/popover-lifecycle",
                     test_popovers_detach_before_account_sel_dispose);
    status = g_test_run ();

    gnc_component_manager_shutdown ();
    gnc_prefs_remove_registered ();
    gnc_engine_shutdown ();
    return status;
}
