/********************************************************************\
 * gnc-tree-view-common.h -- common utilities for manipulating a    *
 *                     GtkTreeView within gnucash                   *
 * Copyright (C) 2003 David Hampton <hampton@employees.org>         *
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
 *                                                                  *
\********************************************************************/

/** @addtogroup Engine
    @{ */
/** @file gnc-tree-view-common.h
    @brief common utilities for manipulating a GtkTreeView within gnucash  
    @author David Hampton <hampton@employees.org>
*/

#ifndef __GNC_TREE_VIEW_COMMON_H
#define __GNC_TREE_VIEW_COMMON_H

#include <gtk/gtktreemodel.h>
#include <gtk/gtktreeview.h>

#define GNC_TREE_VIEW_COLUMN_COLOR_NONE -1
#define GNC_TREE_VIEW_COLUMN_ALIGN_NONE -1
#define GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS -1

typedef void (* renderer_toggled) (GtkCellRendererToggle *cell_renderer_toggle,
				   const gchar           *path,
				   gpointer               user_data);

typedef struct {
  gint column;
  gint visibility_column;
  gint color_column;
  gfloat label_align;
  gint align_column;
  GtkTreeIterCompareFunc sort_fn;
  GtkTreeViewColumnSizing sizing;
  gboolean is_toggle;
  renderer_toggled toggle_edited;
  const char *pref_name;
  const char *field_name;
} gnc_view_column;

/** @name Tree View Creation State */
/** @{ */

/** This function creates all the columns in a GtkTreeView.  It is
 *  passed a data structure that specifies all the details for each
 *  column
 *
 *  @param view A pointer to a generic GtkTreeView.
 *
 *  @param name The name of this view.  This string is used for
 *  debugging messages only.
 *
 *  @param stock_icon The name of the stock icon to use in the first
 *  column of the tree.  If null, no icon will be included.
 *
 *  @param defaults A pointer to the data structure destribing the
 *  tree view. */
void gnc_tree_view_common_create_columns (GtkTreeView *view,
					  const gchar *tree_name,
					  const gchar *stock_icon,
					  gnc_view_column *defaults);
/** @} */


/** @name Tree View Save/Restore State */
/** @{ */

/** This function saves all settings common to a GtkTreeView as used
 *  in gnucash.  This function currently saves the order in which the
 *  column appear in the view, and the visibility and width of each
 *  column.  If the view is sortable, it also remembers the selected
 *  sort column and sort order.  A future enhancement will be to make
 *  it remember which rows in the tree are expanded.
 *
 *  @param view A pointer to a generic GtkTreeView.
 *
 *  @param section The name of a gconf section where the tree
 *  information should be saved.
 *
 *  @param defaults A pointer to the data structure destribing the
 *  tree view. */
void gnc_tree_view_common_save_settings (GtkTreeView *view,
					 const gchar *section,
					 gnc_view_column *defaults);

/** This function restores all the settings common to a GtkTreeView as
 *  used in gnucash.  This function currently restores the order in
 *  which the column appear in the view, and the visibility (but not
 *  the width) of each column.  If the view is sortable, it also
 *  restores the selected sort column and sort order.  A future
 *  enhancement will be to make it restore which rows in the tree are
 *  expanded.
 *
 *  @param view A pointer to a generic GtkTreeView.
 *
 *  @param section The name of a gconf section where the tree
 *  information should be restored from.
 *
 *  @param defaults A pointer to the data structure destribing the
 *  tree view. */
void gnc_tree_view_common_restore_settings (GtkTreeView *view,
					    const gchar *section,
					    gnc_view_column *defaults);
/** @} */


/** @} */

G_END_DECLS

#endif /* __GNC_TREE_VIEW_COMMON_H */
