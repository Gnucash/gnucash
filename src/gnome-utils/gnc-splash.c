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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-gnome-utils.h"
#include "gnc-splash.h"
#include "gnc-version.h"

#define MARKUP_STRING "<span size='small'>%s</span>"

static GtkWidget * splash = NULL;
static GtkWidget * progress = NULL;
static GtkWidget * progress_bar = NULL;
static int splash_is_initialized = FALSE;

static void
splash_destroy_cb (GtkObject *object, gpointer user_data)
{
    splash = NULL;
}

void
gnc_gui_init_splash (void)
{
    if (!splash_is_initialized)
    {
        splash_is_initialized = TRUE;
        gnc_show_splash_screen ();
    }
}

static gboolean
button_press_cb(GtkWidget *widget, GdkEventButton *event, gpointer unused)
{
    gnc_destroy_splash_screen();
    return TRUE;
}

void
gnc_show_splash_screen (void)
{
    GtkWidget *pixmap;
    GtkWidget *frame;
    GtkWidget *vbox;
    GtkWidget *hbox;
    GtkWidget *version;
    GtkWidget *separator;
    gchar *ver_string, *markup;

    if (splash) return;

    splash = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_window_set_type_hint (GTK_WINDOW (splash), GDK_WINDOW_TYPE_HINT_SPLASHSCREEN);
    gtk_window_set_skip_taskbar_hint (GTK_WINDOW (splash), TRUE);

    g_signal_connect (splash, "destroy",
                      G_CALLBACK (splash_destroy_cb), NULL);

    gtk_window_set_title (GTK_WINDOW (splash), "GnuCash");
    gtk_window_set_position (GTK_WINDOW (splash), GTK_WIN_POS_CENTER);

    pixmap = gnc_gnome_get_pixmap ("gnucash_splash.png");

    if (!pixmap)
    {
        g_warning ("can't find splash pixmap");
        gtk_widget_destroy (splash);
        return;
    }

    frame = gtk_frame_new (NULL);
    vbox = gtk_vbox_new (FALSE, 3);
    hbox = gtk_hbox_new (FALSE, 3);
#ifdef GNUCASH_SVN
    /* Development version */
    ver_string = g_strdup_printf(_("Version: GnuCash-%s svn (r%s built %s)"),
                                 VERSION, GNUCASH_SVN_REV, GNUCASH_BUILD_DATE);
#else
    /* Dist Tarball */
    ver_string = g_strdup_printf(_("Version: GnuCash-%s (r%s built %s)"),
                                 VERSION, GNUCASH_SVN_REV, GNUCASH_BUILD_DATE);
#endif

    version = gtk_label_new(NULL);
    markup = g_markup_printf_escaped(MARKUP_STRING, ver_string);
    gtk_label_set_markup(GTK_LABEL(version), markup);
    g_free(markup);
    g_free(ver_string);
    separator = gtk_hseparator_new();

    progress = gtk_label_new(NULL);
    /* the set_max_width avoids "bumping" of the splash screen
       if a long string is given in gnc_update_splash_screen();
       presumably it would be better to inhibit size change of the
       top level container, but I don't know how to do this */
    gtk_label_set_max_width_chars(GTK_LABEL(progress), 34);
    markup = g_markup_printf_escaped(MARKUP_STRING, _("Loading..."));
    gtk_label_set_markup(GTK_LABEL(progress), markup);
    g_free(markup);

    progress_bar = gtk_progress_bar_new ();

    gtk_container_add (GTK_CONTAINER (frame), pixmap);
    gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, FALSE, 0);
    gtk_box_pack_start (GTK_BOX (vbox), version, FALSE, FALSE, 0);
    gtk_box_pack_start (GTK_BOX (vbox), separator, FALSE, FALSE, 0);
    gtk_box_pack_start (GTK_BOX (hbox), progress, TRUE, TRUE, 0);
    gtk_box_pack_start (GTK_BOX (hbox), progress_bar, FALSE, FALSE, 0);
    gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);
    gtk_container_add (GTK_CONTAINER (splash), vbox);

    gtk_widget_add_events(splash, GDK_BUTTON_PRESS_MASK);
    g_signal_connect(splash, "button_press_event",
                     G_CALLBACK(button_press_cb), NULL);

    gtk_window_set_auto_startup_notification (FALSE);
    gtk_widget_show_all (splash);
    gtk_window_set_auto_startup_notification (TRUE);

    /* make sure splash is up */
    while (gtk_events_pending ())
        gtk_main_iteration ();
}

void
gnc_destroy_splash_screen (void)
{
    if (splash)
    {
        gtk_widget_destroy (splash);
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
        if (string && strcmp(string, ""))
        {
            markup = g_markup_printf_escaped(MARKUP_STRING, string);
            gtk_label_set_markup (GTK_LABEL(progress), markup);
            g_free (markup);

            /* make sure new text is up */
            while (gtk_events_pending ())
                gtk_main_iteration ();
        }
    }

    if (progress_bar)
    {
        if (percentage < 0)
        {
            gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progress_bar), 0.0);
        }
        else
        {
            if (percentage <= 100)
            {
                gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progress_bar),
                                              percentage / 100);
            }
            else
            {
                gtk_progress_bar_pulse(GTK_PROGRESS_BAR(progress_bar));
            }
        }

        /* make sure new status bar is up */
        while (gtk_events_pending ())
            gtk_main_iteration ();
    }
}
