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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <gnome.h>

#include "dialog-utils.h"
#include "gnc-splash.h"


static GtkWidget * splash = NULL;


static gint
splash_timeout (gpointer not_used)
{
  if (splash)
  {
    gtk_widget_destroy (splash);
    splash = NULL;
  }

  return FALSE;
}

void
gnc_show_splash_screen (void)
{
  GtkWidget *pixmap;
  GtkWidget *frame;

  if (splash) return;

  splash = gtk_window_new (GTK_WINDOW_POPUP);

  gtk_window_set_position (GTK_WINDOW (splash), GTK_WIN_POS_CENTER);

  pixmap = gnc_get_pixmap ("gnucash_splash.png");

  if (!pixmap)
  {
    g_warning ("can't find splash pixmap");
    gtk_widget_destroy (splash);
    return;
  }

  frame = gtk_frame_new (NULL);

  gtk_container_add (GTK_CONTAINER (frame), pixmap);
  gtk_container_add (GTK_CONTAINER (splash), frame);

  gtk_widget_show_all (splash);

  gtk_timeout_add (4000, splash_timeout, NULL); /* 4 seconds */
}
