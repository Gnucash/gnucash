/********************************************************************\
 * gnc-tree-view.h -- new GtkTreeView with extra features used by   *
 *                    all the tree views in gnucash                 *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/** @addtogroup GUI
    @{ */
/** @file gnc-tree-view.h
    @brief common utilities for manipulating a GtkTreeView within gnucash
    @author David Hampton <hampton@employees.org>
*/

#ifndef __GNC_TREE_VIEW_H
#define __GNC_TREE_VIEW_H

#include <gtk/gtk.h>
#include "gnc-cell-renderer-date.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_VIEW            (gnc_tree_view_get_type ())
#define GNC_TREE_VIEW(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_VIEW, GncTreeView))
#define GNC_TREE_VIEW_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_VIEW, GncTreeViewClass))
#define GNC_IS_TREE_VIEW(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_VIEW))
#define GNC_IS_TREE_VIEW_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_VIEW))
#define GNC_TREE_VIEW_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_VIEW, GncTreeViewClass))
#define GNC_TREE_VIEW_NAME            "GncTreeView"


/* typedefs & structures */
typedef struct
{
    GtkTreeView gtk_tree_view;
} GncTreeView;

typedef struct
{
    GtkTreeViewClass gtk_tree_view;
} GncTreeViewClass;

/* Standard g_object type */
GType gnc_tree_view_get_type (void);


/* The columns managed by gnc-tree-view can use the following column
   attributes.  Set them with:

   g_object_set_data(col, ATTRIBUTE_NAME, value);
*/

/* The column id refers to a specific column in the tree model.  It is
 * also attached to the side of the tree column to allow lookup of a
 * GtkTreeViewColumn when passed a column id from the underlying
 * model. By convention, negative column numbers are used when the
 * visible column has no association with the underlying model.*/
#define MODEL_COLUMN "model_column"

/* For checkbox columns, this contains the real title for the column. */
#define REAL_TITLE  "real_title"

/* The name of this column to use when saving the view's state.  It is
 * used internally to map this colum's properties to the saved state. */
#define PREF_NAME  "pref-name"

/* A column with this attribute set cannot be hidden from view. Valid
   values: GINT_TO_POINTER(0) and GINT_TO_POINTER(1) */
#define ALWAYS_VISIBLE  "always-visible"

/* This attribute controls visibility of a column if not state was saved
   for this column (yet). Valid values:
   GINT_TO_POINTER(0) and GINT_TO_POINTER(1)  */
#define DEFAULT_VISIBLE  "default-visible"

#define GNC_TREE_VIEW_COLUMN_DATA_NONE -1
#define GNC_TREE_VIEW_COLUMN_COLOR_NONE -1
#define GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS -1


typedef void (* renderer_toggled) (GtkCellRendererToggle *cell_renderer_toggle,
                                   const gchar           *path,
                                   gpointer               user_data);


/** @name Tree View Creation */
/** @{ */

/** This function adds a new toggle column to a GncTreeView base view.
 *  It takes all the parameters necessary to hook a GtkTreeModel
 *  column to a GtkTreeViewColumn.  It handles creating a tooltip to
 *  show the full title name, and setting the sort and edit callback
 *  functions.
 *
 *  @param view A pointer to a generic GncTreeView.
 *
 *  @param column_title The full title for this column.  This will be
 *  added as a tooltip what will be displayed when the mouse is
 *  hovered over the column title button.
 *
 *  @param column_short_title This is what will actually be displayed
 *  as the title of the column.  This should be a single character, as
 *  anything more will unnecessarily expand the width of the column.
 *
 *  @param pref_name The internal name of this column.  This name is
 *  used in several functions to look up the column, and it is also
 *  used when saving/restoring the view's state.
 *
 *  @param model_data_column The index of the GtkTreeModel data column
 *  used to determine whether the displayed checkbox for each row will
 *  be show as checked or empty.  Use GNC_TREE_VIEW_COLUMN_DATA_NONE
 *  if you plan on using an non-model data source for this column.
 *
 *  @param model_visibility_column The index of the GtkTreeModel data
 *  column used to determine whether or not a checkbox for each row
 *  will be displayed at all.  Use GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS
 *  if the checkbox should be displayed in all rows.
 *
 *  @param column_sort_fn The function that GtkTreeModelSort
 *  will call to compare two rows to determine their displayed order.
 *
 *  @param toggle_edited_cb The function to call when the user toggles
 *  the state of a checkbox.
 *
 *  @return The newly created GtkTreeViewColumn.
 */
GtkTreeViewColumn *
gnc_tree_view_add_toggle_column (GncTreeView *view,
                                 const gchar *column_title,
                                 const gchar *column_short_title,
                                 const gchar *pref_name,
                                 gint model_data_column,
                                 gint model_visibility_column,
                                 GtkTreeIterCompareFunc column_sort_fn,
                                 renderer_toggled toggle_edited_cb);

/** This function adds a new text column to a GncTreeView base view.
 *  It takes all the parameters necessary to hook a GtkTreeModel
 *  column to a GtkTreeViewColumn.
 *
 *  @param view A pointer to a generic GncTreeView.
 *
 *  @param column_title The title for this column.
 *
 *  @param pref_name The internal name of this column.  This name is
 *  used in several functions to look up the column, and it is also
 *  used when saving/restoring the view's state.
 *
 *  @param stock_icon_name The name of the stock icon to display to
 *  the left of the text in this column.  Used for adding icons like
 *  the "account" icon to a view.  This must be a registered stock
 *  icon, not a filename.
 *
 *  @param sizing_text A string used to compute the default width of
 *  the column.  This text is never displayed.
 *
 *  @param model_data_column The index of the GtkTreeModel data column
 *  used to determine the data that will be displayed in this column
 *  for each row in the view.  Use GNC_TREE_VIEW_COLUMN_DATA_NONE if
 *  you plan on using a non-model data source for this column.  This
 *  index is connected to the "text" attribute of the cell renderer.
 *
 *  @param model_visibility_column The index of the GtkTreeModel data
 *  column used to determine whether or not a checkbox for each row
 *  will be displayed at all.  Use GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS
 *  if the checkbox should be displayed in all rows.
 *
 *  @param column_sort_fn The function that GtkTreeModelSort
 *  will call to compare two rows to determine their displayed
 *  order.
 *
 *  @return The newly created GtkTreeViewColumn.
 */
GtkTreeViewColumn *
gnc_tree_view_add_text_column (GncTreeView *view,
                               const gchar *column_title,
                               const gchar *pref_name,
                               const gchar *stock_icon_name,
                               const gchar *sizing_text,
                               gint model_data_column,
                               gint model_visibility_column,
                               GtkTreeIterCompareFunc column_sort_fn);

/** This function adds a new combobox column to a GncTreeView base
 *  view.  The parameters it takes in common with
 *  gnc_tree_view_add_text_column() behave the same as there.  In
 *  addition, it will use combo_tree_model as the GtkTreeModel for the
 *  combobox, and combo_model_text_column will be the column in the
 *  model used for displaying the text in the combobox.
 */
GtkTreeViewColumn *
gnc_tree_view_add_combo_column (GncTreeView *view,
                                const gchar *column_title,
                                const gchar *pref_name,
                                const gchar *sizing_text,
                                gint model_data_column,
                                gint model_visibility_column,
                                GtkTreeModel *combo_tree_model,
                                gint combo_model_text_column,
                                GtkTreeIterCompareFunc column_sort_fn);


/** This function adds a new date column to a GncTreeView base
 *  view.  The parameters it takes in common with
 *  gnc_tree_view_add_text_column() behave the same as there.
 */
GtkTreeViewColumn *
gnc_tree_view_add_date_column (GncTreeView *view,
                               const gchar *column_title,
                               const gchar *pref_name,
                               const gchar *stock_icon_name,
                               const gchar *sizing_text,
                               gint model_data_column,
                               gint model_visibility_column,
                               GtkTreeIterCompareFunc column_sort_fn);


/** This function adds a new numeric column to a GncTreeView base
 *  view.  It takes all the parameters necessary to hook a
 *  GtkTreeModel column to a GtkTreeViewColumn.  A numeric
 *  column is nothing more then a text column with a few extra
 *  attributes.
 *
 *  @param view A pointer to a generic GncTreeView.
 *
 *  @param column_title The title for this column.
 *
 *  @param pref_name The internal name of this column.  This name is
 *  used in several functions to look up the column, and it is also
 *  used when saving/restoring the view's state.
 *
 *  @param sizing_text A string used to compute the default width of
 *  the column.  This text is never displayed.
 *
 *  @param model_data_column The index of the GtkTreeModel data column
 *  used to determine the data that will be displayed in this column
 *  for each row in the view.  Use GNC_TREE_VIEW_COLUMN_DATA_NONE if
 *  you plan on using an non-model data source for this column.
 *
 *  @param model_color_column The index of the GtkTreeModel data
 *  column used to determine the foreground color of any text in this
 *  column.  It should be used to display negative numbers in red.
 *  Use GNC_TREE_VIEW_COLUMN_COLOR_NONE if the text in this column
 *  should always be displayed in the default theme color for text.
 *
 *  @param model_visibility_column The index of the GtkTreeModel data
 *  column used to determine whether or not a checkbox for each row
 *  will be displayed at all.  Use GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS
 *  if the checkbox should be displayed in all rows.
 *
 *  @param column_sort_fn The function that GtkTreeModelSort
 *  will call to compare two rows to determine their displayed
 *  order.
 *
 *  @return The newly created GtkTreeViewColumn.
 */
GtkTreeViewColumn *
gnc_tree_view_add_numeric_column (GncTreeView *view,
                                  const gchar *column_title,
                                  const gchar *pref_name,
                                  const gchar *sizing_text,
                                  gint model_data_column,
                                  gint model_color_column,
                                  gint model_visibility_column,
                                  GtkTreeIterCompareFunc column_sort_fn);

/** Add a column to a view based upon a GncTreeView.  This function
 *  knows about the two special columns on the right side of this type
 *  of view, and adds the new column before these two columns.  You
 *  could say that it appends to the data columns and ignores the
 *  infrastructure columns.
 *
 *  @param view A pointer to a generic GncTreeView.
 *
 *  @param column The column to append.
 *
 *  @return The index of the newly added column.
 */
gint gnc_tree_view_append_column (GncTreeView *view,
                                  GtkTreeViewColumn *column);

/** @} */


/** @name Tree View Properties */
/** @{ */

/** Make all the correct columns visible, respecting their default
 *  visibility setting, their "always" visibility setting, and the
 *  last saved state if available.
 *
 *  @param view A pointer to an gnc tree view.
 */
void gnc_tree_view_configure_columns (GncTreeView *view);

/** Find a tree column given the "pref name".  This
 *  function simply runs the list of all (visible and invisible)
 *  columns looking for a match.  Column names were attached to each
 *  column at the time the column was created.
 *
 *  @param view The visible tree widget.
 *
 *  @param wanted The "pref name" to find.
 *
 */
GtkTreeViewColumn *
gnc_tree_view_find_column_by_name (GncTreeView *view,
                                   const gchar *wanted);

/** This function is called to set up or remove an association between
 *  a saved state section and the display of a view.  It will first remove
 *  any existing association, and then install the new one.
 *
 *  @note This function currently marks the first column as autosized
 *  and all other columns as fixed size.
 *  This may change in the future.  It must change if we want
 *  to take advantage of the "fixed row height" performance
 *  enhancements added to GtkTreeView in gtk-2.4
 *
 *  @param view The tree view.
 *
 *  @param section Link the view to this saved state section.
 *  Use NULL to disconnect saved state association.
 */
void gnc_tree_view_set_state_section (GncTreeView *view,
                                      const gchar *section);

/** This function is called to get the current association between a
 *  saved state section and the display of a view.  It returns the same
 *  value passed to gnc_tree_view_set_state_section().
 *
 *  @param view The tree view.
 *
 *  @return The current state section.
 */
const gchar *gnc_tree_view_get_state_section (GncTreeView *view);


/** This function is called to completely wipe the treeview's state
 *  information (column visibility, width, sorting order,..).
 *  This function may be called at any time; either when the user
 *  wants to disconnect or when the view object is being destroyed.
 *
 *  @param view The tree view.
 */
void gnc_tree_view_remove_state_information(GncTreeView *view);


/** This function is called to write the treeview's state
 *  information (column visibility, width, sorting order,..)
 *  to the state file.
 *
 *  @param view The tree view.
 */
void gnc_tree_view_save_state (GncTreeView *view);


/** This function set the columns that will be allocated the free space
 *  in the view.
 *
 *  @param view The tree view.
 *
 *  @param list of column names.
 */
void gnc_tree_view_expand_columns (GncTreeView *view,
                                   gchar *first_column_name,
                                   ...);

/** This function links the cell backgrounds of the two control columns
 *  to a column in the model that has color strings or a cell data function
 *  that sets the "cell-background" property.
 *
 *  @param view The tree view.
 *
 *  @param column The column in the model containing color strings.
 *
 *  @param func This is a cell data function that sets the "cell-background".
 */
void
gnc_tree_view_set_control_column_background (GncTreeView *view, gint column,
        GtkTreeCellDataFunc func);

/** This allows the columns to be setup without the model connected
 *
 *  @param view The tree view.
 *
 *  @param sort model.
 */
void
gnc_tree_view_set_sort_user_data (GncTreeView *view, GtkTreeModel *s_model);

/** This function is called to set the "show-column-menu" property on
 *  this view.  This function has no visible effect if the
 *  "state-section" property has not been set.
 *
 *  @param view The tree view.
 *
 *  @param visible Create the column selection menu if TRUE.
 */
void
gnc_tree_view_set_show_column_menu (GncTreeView *view,
                                    gboolean visible);

/** This function is called to get the current value of the
 *  "show-column-menu" property.  It returns the same value passed to
 *  gnc_tree_view_set_show_menu_column().
 *
 *  @param view The tree view.
 *
 *  @return Whether or not the column selection menu should be shown.
 */
gboolean
gnc_tree_view_get_show_column_menu (GncTreeView *view);

/** Return the "main" cell renderer from a GtkTreeViewColumn added to
 *  a GncTreeView my one of the convenience routines.
 *
 *  @param column The tree view column that was added to the GncTreeView
 *
 *  @returns The cell renderer in use in the column.
 */
GtkCellRenderer *
gnc_tree_view_column_get_renderer(GtkTreeViewColumn *column);


/* Takes a GdkEventKey and the current path and column for the
 * treeview.  Interprets the event as something that might move the
 * cursor.  Returns the new column and the possibly changed (if
 * navigation wrapped a row) path. */
void
gnc_tree_view_keynav(GncTreeView *view, GtkTreeViewColumn **col,
                     GtkTreePath *path, GdkEventKey *event);

/* Returns TRUE if path is a vaid path for the treeview */
gboolean
gnc_tree_view_path_is_valid(GncTreeView *view, GtkTreePath *path);

/** @} */

/** @} */

G_END_DECLS

#endif /* __GNC_TREE_VIEW_H */
