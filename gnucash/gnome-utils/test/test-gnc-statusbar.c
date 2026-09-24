/*
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-gtk-utils.h"
#include "gnc-plugin.h"

static const gchar *
statusbar_text (GtkWidget *statusbar)
{
    GtkWidget *label = gtk_widget_get_first_child (statusbar);

    g_assert_true (GTK_IS_LABEL (label));
    return gtk_label_get_text (GTK_LABEL (label));
}

static void
test_message_stack (void)
{
    GtkWidget *statusbar = gnc_statusbar_new ();
    guint base;
    guint section;
    guint tooltip;

    g_object_ref_sink (statusbar);
    g_assert_true (gnc_statusbar_is (statusbar));
    g_assert_cmpstr (statusbar_text (statusbar), ==, " ");

    base = gnc_statusbar_push (statusbar, 0, "base");
    section = gnc_statusbar_push (statusbar, 1, "section");
    tooltip = gnc_statusbar_push (statusbar, 0, "tooltip");
    g_assert_cmpuint (base, !=, 0);
    g_assert_cmpuint (section, !=, 0);
    g_assert_cmpuint (tooltip, !=, 0);
    g_assert_cmpstr (statusbar_text (statusbar), ==, "tooltip");

    gnc_statusbar_pop (statusbar, 0);
    g_assert_cmpstr (statusbar_text (statusbar), ==, "section");
    gnc_statusbar_remove (statusbar, 1, section);
    g_assert_cmpstr (statusbar_text (statusbar), ==, "base");
    gnc_statusbar_remove (statusbar, 0, base);
    g_assert_cmpstr (statusbar_text (statusbar), ==, " ");

    g_object_unref (statusbar);
}

static void
test_plugin_tooltip_callbacks_accept_gnc_statusbar (void)
{
    GtkWidget *statusbar = gnc_statusbar_new ();
    GtkWidget *toolbar = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    GtkWidget *button = gtk_button_new_with_label ("Action");

    g_object_ref_sink (statusbar);
    g_object_ref_sink (toolbar);
    gtk_actionable_set_action_name (GTK_ACTIONABLE (button), "win.test");
    gtk_widget_set_tooltip_text (button, "Tooltip");
    gtk_box_append (GTK_BOX (toolbar), button);

    gnc_plugin_add_toolbar_tooltip_callbacks (toolbar, statusbar);

    g_object_unref (toolbar);
    g_object_unref (statusbar);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    gtk_init ();
    g_test_add_func ("/gnome-utils/statusbar/message-stack", test_message_stack);
    g_test_add_func ("/gnome-utils/statusbar/plugin-tooltip-callbacks",
                     test_plugin_tooltip_callbacks_accept_gnc_statusbar);

    return g_test_run ();
}
