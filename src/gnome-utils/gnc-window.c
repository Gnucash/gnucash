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

#include "config.h"

#include <gtk/gtk.h>

#include "gnc-engine.h"
#include "gnc-plugin-page.h"
#include "gnc-window.h"
#include "gnc-splash.h"

static QofLogModule log_module = GNC_MOD_GUI;

GType
gnc_window_get_type (void)
{
  static GType gnc_window_type = 0;

  if (gnc_window_type == 0) {
    static const GTypeInfo our_info = {
      sizeof (GncWindowIface),
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      0,
      0,
      NULL
    };

    gnc_window_type = g_type_register_static (G_TYPE_INTERFACE,
					      "GncWindow",
					      &our_info, 0);
    g_type_interface_add_prerequisite (gnc_window_type, G_TYPE_OBJECT);
  }

  return gnc_window_type;
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

static GtkWidget *
gnc_window_get_statusbar (GncWindow *window)
{
  g_return_val_if_fail(GNC_WINDOW (window), NULL);

  /* mandatory */
  g_return_val_if_fail(GNC_WINDOW_GET_IFACE (window)->get_statusbar, NULL);

  return GNC_WINDOW_GET_IFACE (window)->get_statusbar (window);
}

static GtkWidget *
gnc_window_get_progressbar (GncWindow *window)
{
  g_return_val_if_fail(GNC_WINDOW (window), NULL);

  /* optional */
  if (GNC_WINDOW_GET_IFACE (window)->get_progressbar == NULL)
    return NULL;

  return GNC_WINDOW_GET_IFACE (window)->get_progressbar (window);
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
  gtk_statusbar_push(GTK_STATUSBAR(statusbar), 0, message ? message : "");
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
  if (window != NULL) {
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

  window = progress_bar_hack_window;
  if (window == NULL)
    return;

  progressbar = gnc_window_get_progressbar (window);
  if (progressbar == NULL) {
    DEBUG( "no progressbar in hack-window" );
    return;
  }

  gnc_update_splash_screen(message, percentage);

  if (percentage < 0) {
    gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar), " ");
    gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar), 0.0);
    if (GNC_WINDOW_GET_IFACE(window)->ui_set_sensitive != NULL)
      GNC_WINDOW_GET_IFACE(window)->ui_set_sensitive(window, TRUE);
  } else {
    if (message)
      gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar), message);
    if ((percentage == 0) &&
	(GNC_WINDOW_GET_IFACE(window)->ui_set_sensitive != NULL))
      GNC_WINDOW_GET_IFACE(window)->ui_set_sensitive(window, FALSE);
    if (percentage <= 100) {
      gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar), 
                                    percentage/100);
    } else {
      gtk_progress_bar_pulse(GTK_PROGRESS_BAR(progressbar));
    }
  }

  /* make sure new text is up */
  while (gtk_events_pending ())
    gtk_main_iteration ();
}
