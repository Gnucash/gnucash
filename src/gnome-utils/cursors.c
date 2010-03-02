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

/********************************************************************\
 * 2003-03-16 TomF changes for gnucash-gnome2-dev, 8th batch	    *
 * * src/gnome-utils/cursors.c					    *
 *   Replace calls of deprecated gtk_container_get_toplevels by	    *
 *   gtk_window_list_toplevels					    *
\********************************************************************/

#include "config.h"

#include <gtk/gtk.h>

#include "gnc-ui.h"


typedef enum
{
    GNC_CURSOR_NORMAL = -1,
    GNC_CURSOR_BUSY   = GDK_WATCH
} GNCCursorType;


/********************************************************************\
 * gnc_ui_set_cursor                                                *
 *   sets the cursor to the specified type                          *
 *                                                                  *
 * Args: w    - the widget over which to change the cursor          *
 *       type - the type of cursor to make                          *
 * Return: none                                                     *
\********************************************************************/
static void
gnc_ui_set_cursor (GdkWindow *win, GNCCursorType type, gboolean update_now)
{
    GdkCursor *cursor = NULL;

    if (win == NULL)
        return;

    if (type != GNC_CURSOR_NORMAL)
        cursor = gdk_cursor_new (type);

    gdk_window_set_cursor (win, cursor);

    if (update_now && type != GNC_CURSOR_NORMAL)
    {
        while (gtk_events_pending ())
            gtk_main_iteration ();
    }

    if (type != GNC_CURSOR_NORMAL)
        gdk_cursor_unref (cursor);
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
    if (w != NULL)
        gnc_ui_set_cursor (w->window, GNC_CURSOR_BUSY, update_now);
    else
    {
        GList *containerstop, *node;

        for (containerstop = node = gtk_window_list_toplevels (); node; node = node->next)
        {
            w = node->data;

            if (!w || !GTK_IS_WIDGET (w) || !w->window)
                continue;

            gnc_ui_set_cursor (w->window, GNC_CURSOR_BUSY, update_now);
        }
        g_list_free (containerstop);
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
        gnc_ui_set_cursor (w->window, GNC_CURSOR_NORMAL, FALSE);
    else
    {
        GList *containerstop, *node;

        for (containerstop = node = gtk_window_list_toplevels (); node; node = node->next)
        {
            w = GTK_WIDGET (node->data);

            if (!w || !GTK_IS_WIDGET (w) || GTK_WIDGET_NO_WINDOW(w))
                continue;

            gnc_ui_set_cursor (w->window, GNC_CURSOR_NORMAL, FALSE);
        }
        g_list_free (containerstop);
    }
}

