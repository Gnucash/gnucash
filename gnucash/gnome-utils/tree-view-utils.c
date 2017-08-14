/*
 * tree-view-utils.c -- some convenience functions for use with
 *                      plain GtkTreeViews in situations where a
 *                      fully fledged GncTreeView is overkill.
 *                      Handy with GtkTreeViews defined in glade files.
 *
 * Copyright (C) 2013 Geert Janssens <geert@kobaltwit.be>
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

/** @addtogroup GUI
    @{ */
/** @addtogroup GncTreeView
 * @{ */
/** @file tree-view-utils.c
    @brief Simple convenience functions for common tasks on GtkTreeViews.
    @author Geert Janssens <geert@kobaltwit.be>
*/

#include "config.h"

#include <gtk/gtk.h>
#include <string.h>

#include "tree-view-utils.h"

void tree_view_column_set_default_width (GtkTreeView *view,
                                         GtkTreeViewColumn *column,
                                         const gchar *sizing_text)
{
    PangoLayout* layout;
    int default_width, title_width;
    const gchar *column_title;

    /* Default size is the larger of the column title and the sizing text */
    column_title = gtk_tree_view_column_get_title (column);
    layout = gtk_widget_create_pango_layout (GTK_WIDGET(view), column_title);
    pango_layout_get_pixel_size(layout, &title_width, NULL);
    g_object_unref(layout);
    layout = gtk_widget_create_pango_layout (GTK_WIDGET(view), sizing_text);
    pango_layout_get_pixel_size(layout, &default_width, NULL);
    g_object_unref(layout);
    default_width = MAX(default_width, title_width);
    if (default_width)
    {
        default_width += 10; /* add some padding */
        g_object_set(G_OBJECT(column),
                     "sizing",      GTK_TREE_VIEW_COLUMN_FIXED,
                     "fixed-width", default_width,
                     NULL);
    }
}

/** @} */
/** @} */
