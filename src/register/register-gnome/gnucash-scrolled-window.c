/********************************************************************\
 * gnucash-scrolled-window.c -- gnucash specific scrolled window    *
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

#include "config.h"

#include "gnucash-scrolled-window.h"


/** Static Globals ****************************************************/
static GtkScrolledWindowClass *parent_class = NULL;


/** Declarations ******************************************************/
static void gnc_scrolled_window_class_init (GncScrolledWindowClass *class);
static void gnc_scrolled_window_init (GncScrolledWindow *scrollwin);


/** Implementations ***************************************************/

GType
gnc_scrolled_window_get_type (void)
{
    static GType gnc_scrolled_window_type = 0;

    if (!gnc_scrolled_window_type)
    {
        static const GTypeInfo gnc_scrolled_window_info =
        {
            sizeof (GncScrolledWindowClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_scrolled_window_class_init,
            NULL,
            NULL,
            sizeof (GncScrolledWindow),
            0,
            (GInstanceInitFunc) gnc_scrolled_window_init
        };

        gnc_scrolled_window_type = g_type_register_static (GTK_TYPE_SCROLLED_WINDOW,
                                   "GncScrolledWindow",
                                   &gnc_scrolled_window_info, 0);
    }

    return gnc_scrolled_window_type;
}


GtkWidget *
gnc_scrolled_window_new (void)
{
    return gtk_widget_new (GNC_TYPE_SCROLLED_WINDOW,
                           "hadjustment", NULL,
                           "vadjustment", NULL,
                           NULL);
}


static void
gnc_scrolled_window_class_init (GncScrolledWindowClass *class)
{
    GtkScrolledWindowClass *scroll_class = GTK_SCROLLED_WINDOW_CLASS (class);

    parent_class = g_type_class_peek_parent (class);

    scroll_class->scrollbar_spacing = 0;
}

static void
gnc_scrolled_window_init (GncScrolledWindow *scrollwin)
{
}
