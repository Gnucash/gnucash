/********************************************************************\
 * cursor.c -- functions for changing cursors                       *
 *                                                                  *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#include "config.h"

#include <gnome.h>

#include "ui-callbacks.h"
#include "cursors.h"


/********************************************************************\
 * gnc_ui_set_cursor                                                * 
 *   sets the cursor to the specified type                          * 
 *                                                                  * 
 * Args: w    - the widget over which to change the cursor          *
 *       type - the type of cursor to make                          *
 * Return: none                                                     * 
\********************************************************************/
void
gnc_ui_set_cursor (GdkWindow *win, int type)
{
  GdkCursor *cursor = NULL;

  if (!win)
    return;

  if (type != GNC_CURSOR_NORMAL)
    cursor = gdk_cursor_new(type);

  gdk_window_set_cursor (win, cursor);

  while (gtk_events_pending())
    gtk_main_iteration();

  if (type != GNC_CURSOR_NORMAL)
    gdk_cursor_destroy(cursor);
}


/********************************************************************\
 * gnc_set_busy_cursor                                              * 
 *   sets the cursor to the busy watch                              * 
 *                                                                  * 
 * Args:   w - the widget over which to make cursor busy            * 
 * Return: none                                                     * 
\********************************************************************/
void 
gnc_set_busy_cursor(GtkWidget *w)
{
  if (w != NULL)
    gnc_ui_set_cursor(w->window, GNC_CURSOR_BUSY);
}


/********************************************************************\
 * gnc_unset_busy_cursor                                            * 
 *   sets the cursor to the default cursor                          * 
 *                                                                  * 
 * Args:   w - the widget over which to make cursor normal          * 
 * Return: none                                                     * 
\********************************************************************/
void 
gnc_unset_busy_cursor(GtkWidget *w)
{
  if (w != NULL)
    gnc_ui_set_cursor(w->window, GNC_CURSOR_NORMAL);
}

/************************* END OF FILE ******************************\
\********************************************************************/
