/********************************************************************\
 * gnc-splash.c -- splash screen for GnuCash                        *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <math.h>

#include "gnc-gnome-utils.h"
#include "gnc-splash.h"
#include "gnc-version.h"
#include "gnc-prefs.h"
#include "dialog-utils.h"

#define MARKUP_STRING "<span size='small'>%s</span>"
#define GNC_PREF_SHOW_SPLASH "show-splash-screen"

static GtkWidget * splash = NULL;
static GtkWidget * progress = NULL;
static GtkWidget * progress_bar = NULL;

static void
splash_destroy_cb (GtkWidget *object, gpointer user_data)
{
    splash = NULL;
}

static gboolean
button_press_cb (GtkGestureClick *gesture,
                 int n_press,
                 double x,
                 double y,
                 gpointer user_data)
{
    if (splash)
        gtk_window_minimize (GTK_WINDOW(splash)); //FIXME gtk4 this does not seem to work
//FIXME gtk4        gtk_window_iconify (GTK_WINDOW(splash));
    return TRUE;
}

void
gnc_show_splash_screen (void)
{
    GtkWidget *picture;
    GtkWidget *frame;
    GtkWidget *vbox;
    GtkWidget *hbox;
    GtkWidget *version;
    GtkWidget *separator;
    gchar *ver_string, *markup;

    if (splash)
        return;
    if (!gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_SHOW_SPLASH))
        return;

    splash = gtk_window_new ();
    gtk_window_set_decorated (GTK_WINDOW(splash), FALSE);
//FIXME gtk4    gtk_window_set_skip_taskbar_hint (GTK_WINDOW (splash), TRUE);

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(splash), "gnc-id-splash");

    g_signal_connect (G_OBJECT(splash), "destroy",
                      G_CALLBACK(splash_destroy_cb), NULL);

    gtk_window_set_title (GTK_WINDOW(splash), "GnuCash");

//FIXME gtk4    gtk_window_set_position (GTK_WINDOW (splash), GTK_WIN_POS_CENTER);
//FIXME gtk4    gtk_window_set_type_hint (GTK_WINDOW (splash), GDK_WINDOW_TYPE_HINT_DIALOG);

    picture = gnc_gnome_get_picture ("gnucash_splash.png");

    if (!picture)
    {
        g_warning ("can't find splash pixmap");
//FIXME gtk4        gtk_window_destroy (GTK_WINDOW(splash));
        return;
    }

    frame = gtk_frame_new (NULL);
    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
    gtk_box_set_homogeneous (GTK_BOX(vbox), FALSE);
    hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3);
    gtk_box_set_homogeneous (GTK_BOX(hbox), FALSE);

    ver_string = g_strdup_printf ("%s: %s, %s: %s", _("Version"),
                                  gnc_version(), _("Build ID"), gnc_build_id());

    version = gtk_label_new (NULL);
    markup = g_markup_printf_escaped (MARKUP_STRING, ver_string);
    gtk_label_set_markup (GTK_LABEL(version), markup);
    g_free (markup);
    g_free (ver_string);

    separator = gtk_separator_new (GTK_ORIENTATION_HORIZONTAL);

    progress = gtk_label_new (NULL);
    /* the set_max_width avoids "bumping" of the splash screen
       if a long string is given in gnc_update_splash_screen();
       presumably it would be better to inhibit size change of the
       top level container, but I don't know how to do this */
    gtk_label_set_max_width_chars (GTK_LABEL(progress), 34);
    markup = g_markup_printf_escaped (MARKUP_STRING, _("Loading…"));
    gtk_label_set_markup (GTK_LABEL(progress), markup);
    g_free (markup);

    progress_bar = gtk_progress_bar_new ();

    gtk_frame_set_child (GTK_FRAME(frame), GTK_WIDGET(picture));
    gtk_widget_set_vexpand (GTK_WIDGET(frame), TRUE);
    gtk_widget_set_vexpand (GTK_WIDGET(picture), TRUE);

    gtk_box_append (GTK_BOX(vbox), GTK_WIDGET(frame));
    gtk_box_append (GTK_BOX(vbox), GTK_WIDGET(version));
    gtk_box_append (GTK_BOX(vbox), GTK_WIDGET(separator));
    gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(progress));
    gtk_widget_set_hexpand (GTK_WIDGET(progress), TRUE);
    gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(progress_bar));
    gtk_widget_set_halign (GTK_WIDGET(progress_bar), GTK_ALIGN_END);

    gtk_box_append (GTK_BOX(vbox), GTK_WIDGET(hbox));
    gtk_window_set_child (GTK_WINDOW(splash), GTK_WIDGET(vbox));

    GtkGesture *event_gesture = gtk_gesture_click_new ();
    gtk_widget_add_controller (GTK_WIDGET(splash), GTK_EVENT_CONTROLLER(event_gesture));
    g_signal_connect (G_OBJECT(event_gesture), "pressed",
                      G_CALLBACK(button_press_cb), NULL);

    gtk_window_set_auto_startup_notification (FALSE);
    gtk_widget_set_visible (GTK_WIDGET(splash), TRUE);
    gtk_window_set_auto_startup_notification (TRUE);

    gtk_window_present (GTK_WINDOW(splash));

    /* make sure splash is up */
    while (g_main_context_pending (NULL))
        g_main_context_iteration (NULL, TRUE);
}

void
gnc_destroy_splash_screen (void)
{
    if (splash)
    {
        gtk_window_destroy (GTK_WINDOW(splash));
        progress = NULL;
        progress_bar = NULL;
        splash = NULL;
    }
}

void
gnc_update_splash_screen (const gchar *string, double percentage)
{
    gchar *markup;

    if (progress)
    {
        if (string && strcmp (string, ""))
        {
            markup = g_markup_printf_escaped (MARKUP_STRING, string);
            gtk_label_set_markup (GTK_LABEL(progress), markup);
            g_free (markup);

            /* make sure new text is up */
            while (g_main_context_pending (NULL))
                g_main_context_iteration (NULL, TRUE);
        }
    }

    if (progress_bar )
    {
         double curr_fraction =
              round (gtk_progress_bar_get_fraction (GTK_PROGRESS_BAR(progress_bar)) * 100.0);
         if (percentage >= 0 && percentage <= 100.0 &&
             round (percentage) == curr_fraction)
              return; // No change so don't waste time running the main loop

        if (percentage <= 0)
        {
            gtk_progress_bar_set_fraction (GTK_PROGRESS_BAR(progress_bar), 0.0);
        }
        else
        {
            if (percentage <= 100)
            {
                gtk_progress_bar_set_fraction (GTK_PROGRESS_BAR(progress_bar),
                                               percentage / 100);
            }
            else
            {
                gtk_progress_bar_pulse (GTK_PROGRESS_BAR(progress_bar));
            }
        }

        /* make sure new status bar is up */
        while (g_main_context_pending (NULL))
            g_main_context_iteration (NULL, TRUE);
    }
}

GtkWindow *gnc_get_splash_screen (void)
{
    return GTK_WINDOW(splash);
}
