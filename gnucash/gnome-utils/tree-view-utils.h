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

#ifndef TREE_VIEW_UTILS_H_
#define TREE_VIEW_UTILS_H_

#include "config.h"

#include <gtk/gtk.h>
#include <string.h>

/** Set default width for a treeview column. This base width
 *  is the largest of the column title and some arbitrary
 *  text passed in via sizing_text. This base width is then
 *  increased with some padding.
 */
void tree_view_column_set_default_width (GtkTreeView *view,
                                         GtkTreeViewColumn *column,
                                         const gchar *sizing_text);

/** @} */
/** @} */

#endif /* TREE_VIEW_UTILS_H_ */
