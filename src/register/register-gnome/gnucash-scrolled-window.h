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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef GNC_SCROLLED_WINDOW_H
#define GNC_SCROLLED_WINDOW_H

#include <gtk/gtkscrolledwindow.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define GNC_TYPE_SCROLLED_WINDOW            (gnc_scrolled_window_get_type ())
#define GNC_SCROLLED_WINDOW(obj)            (GTK_CHECK_CAST ((obj), GNC_TYPE_SCROLLED_WINDOW, GNCScrolledWindow))
#define GNC_SCROLLED_WINDOW_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_SCROLLED_WINDOW, GNCScrolledWindowClass))
#define GNC_IS_SCROLLED_WINDOW(obj)         (GTK_CHECK_TYPE ((obj), GNC_TYPE_SCROLLED_WINDOW))
#define GNC_IS_SCROLLED_WINDOW_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GNC_TYPE_SCROLLED_WINDOW))


typedef struct _GNCScrolledWindow       GNCScrolledWindow;
typedef struct _GNCScrolledWindowClass  GNCScrolledWindowClass;

struct _GNCScrolledWindow
{
  GtkScrolledWindow scrollwin;
};

struct _GNCScrolledWindowClass
{
  GtkScrolledWindowClass parent_class;
};


GtkType     gnc_scrolled_window_get_type (void);
GtkWidget * gnc_scrolled_window_new (void);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* GNC_SCROLLED_WINDOW_H */
