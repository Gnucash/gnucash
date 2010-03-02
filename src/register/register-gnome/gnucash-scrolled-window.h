/********************************************************************\
 * gnucash-scrolled-window.h -- gnucash specific scrolled window    *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
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

#ifndef GNC_SCROLLED_WINDOW_H
#define GNC_SCROLLED_WINDOW_H

#include <gtk/gtkscrolledwindow.h>

G_BEGIN_DECLS

#define GNC_TYPE_SCROLLED_WINDOW        (gnc_scrolled_window_get_type ())
#define GNC_SCROLLED_WINDOW(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_SCROLLED_WINDOW, GNCScrolledWindow))
#define GNC_SCROLLED_WINDOW_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_SCROLLED_WINDOW, GNCScrolledWindowClass))
#define GNC_IS_SCROLLED_WINDOW(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_SCROLLED_WINDOW))
#define GNC_IS_SCROLLED_WINDOW_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_SCROLLED_WINDOW))


typedef struct
{
    GtkScrolledWindow scrollwin;
} GncScrolledWindow;

typedef struct
{
    GtkScrolledWindowClass parent_class;
} GncScrolledWindowClass;


GType      gnc_scrolled_window_get_type (void);
GtkWidget *gnc_scrolled_window_new (void);

G_END_DECLS

#endif /* GNC_SCROLLED_WINDOW_H */
