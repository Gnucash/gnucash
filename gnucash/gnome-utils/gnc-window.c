/*
 * gnc-window.c -- structure which represents a GnuCash window.
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include <config.h>

#include <gtk/gtk.h>
#include <math.h>

#include "gnc-engine.h"
#include "gnc-plugin-page.h"
#include "gnc-window.h"
#include "gnc-splash.h"

static QofLogModule log_module = GNC_MOD_GUI;

G_DEFINE_INTERFACE (GncWindow, gnc_window, G_TYPE_OBJECT)

static void
gnc_window_default_init (GncWindowInterface *klass)
{
}

/************************************************************
 *                Interface access functions                *
 ************************************************************/

GtkWindow *
gnc_window_get_gtk_window (GncWindow *window)
{
    g_return_val_if_fail(GNC_WINDOW (window), NULL);

    /* mandatory */
    g_return_val_if_fail(GNC_WINDOW_GET_IFACE (window)->get_gtk_window, NULL);

    return GNC_WINDOW_GET_IFACE (window)->get_gtk_window (window);
}

GtkWidget *
gnc_window_get_statusbar (GncWindow *window)
{
    g_return_val_if_fail(GNC_WINDOW (window), NULL);

    /* mandatory */
    g_return_val_if_fail(GNC_WINDOW_GET_IFACE (window)->get_statusbar, NULL);

    return GNC_WINDOW_GET_IFACE (window)->get_statusbar (window);
}

GtkWidget *
gnc_window_get_progressbar (GncWindow *window)
{
    g_return_val_if_fail(GNC_WINDOW (window), NULL);

    /* optional */
    if (GNC_WINDOW_GET_IFACE (window)->get_progressbar == NULL)
        return NULL;

    return GNC_WINDOW_GET_IFACE (window)->get_progressbar (window);
}

GtkWidget *
gnc_window_get_menubar (GncWindow *window)
{
    g_return_val_if_fail (GNC_WINDOW(window), NULL);

    /* optional */
    if (GNC_WINDOW_GET_IFACE(window)->get_menubar == NULL)
        return NULL;

    return GNC_WINDOW_GET_IFACE(window)->get_menubar (window);
}

GtkWidget *
gnc_window_get_toolbar (GncWindow *window)
{
    g_return_val_if_fail (GNC_WINDOW(window), NULL);

    /* optional */
    if (GNC_WINDOW_GET_IFACE(window)->get_toolbar == NULL)
        return NULL;

    return GNC_WINDOW_GET_IFACE(window)->get_toolbar (window);
}

GMenuModel *
gnc_window_get_menubar_model (GncWindow *window)
{
    g_return_val_if_fail (GNC_WINDOW(window), NULL);

    /* optional */
    if (GNC_WINDOW_GET_IFACE(window)->get_menubar_model == NULL)
        return NULL;

    return GNC_WINDOW_GET_IFACE(window)->get_menubar_model (window);
}

GtkAccelGroup *
gnc_window_get_accel_group (GncWindow *window)
{
    g_return_val_if_fail (GNC_WINDOW(window), NULL);

    /* optional */
    if (GNC_WINDOW_GET_IFACE(window)->get_accel_group == NULL)
        return NULL;

    return GNC_WINDOW_GET_IFACE(window)->get_accel_group (window);
}
/************************************************************
 *              Auxiliary status bar functions              *
 ************************************************************/

void
gnc_window_update_status (GncWindow *window, GncPluginPage *page)
{
    GtkWidget *statusbar;
    const gchar *message;

    g_return_if_fail(GNC_WINDOW (window));

    statusbar = gnc_window_get_statusbar (window);
    message = gnc_plugin_page_get_statusbar_text(page);
    gtk_statusbar_pop(GTK_STATUSBAR(statusbar), 0);
    gtk_statusbar_push(GTK_STATUSBAR(statusbar), 0, message ? message : " ");
}

void
gnc_window_set_status (GncWindow *window, GncPluginPage *page,
                       const gchar *message)
{
    g_return_if_fail(GNC_WINDOW (window));
    g_return_if_fail(GNC_PLUGIN_PAGE (page));

    gnc_plugin_page_set_statusbar_text(page, message);
    gnc_window_update_status (window, page);
}

/************************************************************
 *             Auxiliary progress bar functions             *
 ************************************************************/

/*
 * Single threaded hack.  Otherwise the window value has to be passed
 * all the way down to the backend and then back out again.  Not too
 * bad from C, but also has to be done in Scheme.
 */
static GncWindow *progress_bar_hack_window = NULL;

/*
 * Must be set to a valid window or to NULL (no window).
 */
void
gnc_window_set_progressbar_window (GncWindow *window)
{
    if (window != NULL)
    {
        g_return_if_fail(GNC_WINDOW (window));
    }

    progress_bar_hack_window = window;
}


GncWindow *
gnc_window_get_progressbar_window (void)
{
    return progress_bar_hack_window;
}


void
gnc_window_show_progress (const char *message, double percentage)
{
    GncWindow *window;
    GtkWidget *progressbar;
    double curr_fraction;

    window = progress_bar_hack_window;
    if (window == NULL)
        return;

    progressbar = gnc_window_get_progressbar (window);
    if (progressbar == NULL)
    {
        DEBUG( "no progressbar in hack-window" );
        return;
    }

    curr_fraction =
         round(gtk_progress_bar_get_fraction(GTK_PROGRESS_BAR(progressbar)) * 100.0);

    if (percentage >= 0 && percentage <= 100 &&
        round(percentage) == curr_fraction)
         return; // No change, so don't waste time running the main loop.

    gnc_update_splash_screen(message, percentage);

    if (percentage < 0)
    {
        gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar), " ");
        gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar), 0.0);
        if (GNC_WINDOW_GET_IFACE(window)->ui_set_sensitive != NULL)
            GNC_WINDOW_GET_IFACE(window)->ui_set_sensitive(window, TRUE);
    }
    else
    {
        if (message && *message)
            gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar), message);
        if ((percentage == 0.0) &&
                (GNC_WINDOW_GET_IFACE(window)->ui_set_sensitive != NULL))
            GNC_WINDOW_GET_IFACE(window)->ui_set_sensitive(window, FALSE);
        if (percentage <= 100.0)
        {
            gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar),
                                          percentage / 100.0);
        }
        else
        {
            gtk_progress_bar_pulse(GTK_PROGRESS_BAR(progressbar));
        }
    }

    /* make sure new text is up */
    while (gtk_events_pending ())
        gtk_main_iteration ();
}
