/********************************************************************\
 * cursor.c -- functions for changing cursors                       *
 *                                                                  *
 * Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>            *
 * Copyright (C) 1998-2000 Linas Vepstas <linas@linas.org>          *
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
#include "gnc-ui.h"


static void
gnc_ui_set_cursor (GtkWidget *widget, const char *cursor_name)
{
    if (widget != NULL && GTK_IS_WIDGET (widget))
        gtk_widget_set_cursor_from_name (widget, cursor_name);
}
/********************************************************************\
 * gnc_set_busy_cursor                                              *
 *   sets the cursor to the busy watch for the given window.        *
 *   if the window is null, sets the cursor for all toplevel windows*
 *                                                                  *
 * Args: w          - the widget over which to make cursor busy     *
 *       update_now - if true the cursor will be changed when the   *
 *                    call returns.                                 *
 * Return: none                                                     *
\********************************************************************/
void
gnc_set_busy_cursor (GtkWidget *w, gboolean update_now)
{
    (void)update_now;

    if (w != NULL)
        gnc_ui_set_cursor (w, "wait");
    else
    {
        GList *toplevels = gtk_window_list_toplevels ();

        for (GList *node = toplevels; node != NULL; node = node->next)
            gnc_ui_set_cursor (GTK_WIDGET (node->data), "wait");

        g_list_free (toplevels);
    }
}


/********************************************************************\
 * gnc_unset_busy_cursor                                            *
 *   sets the cursor to the default cursor for the given window.    *
 *   if the window is null, sets the cursor for all toplevel windows*
 *                                                                  *
 * Args:   w - the widget over which to make cursor normal          *
 * Return: none                                                     *
\********************************************************************/
void
gnc_unset_busy_cursor (GtkWidget *w)
{
    if (w != NULL)
        gnc_ui_set_cursor (w, NULL);
    else
    {
        GList *toplevels = gtk_window_list_toplevels ();

        for (GList *node = toplevels; node != NULL; node = node->next)
            gnc_ui_set_cursor (GTK_WIDGET (node->data), NULL);

        g_list_free (toplevels);
    }
}

