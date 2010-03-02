/********************************************************************\
 * gnc-tree-view-commodity.h -- GtkTreeView implementation to       *
 *                     display commodities in a GtkTreeView.        *
 * Copyright (C) 2003,2005 David Hampton <hampton@employees.org>    *
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
/** @addtogroup GuiTreeModel
 *  @{ */
/** @file gnc-tree-view-commodity.h
    @brief GtkTreeView implementation for gnucash commodity tree.
    @author David Hampton <hampton@employees.org>
*/

#ifndef __GNC_TREE_VIEW_COMMODITY_H
#define __GNC_TREE_VIEW_COMMODITY_H

#include <gtk/gtktreemodel.h>
#include <gtk/gtktreeview.h>
#include "gnc-tree-view.h"

#include "gnc-commodity.h"
#include "gnc-ui-util.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_VIEW_COMMODITY            (gnc_tree_view_commodity_get_type ())
#define GNC_TREE_VIEW_COMMODITY(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_VIEW_COMMODITY, GncTreeViewCommodity))
#define GNC_TREE_VIEW_COMMODITY_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_VIEW_COMMODITY, GncTreeViewCommodityClass))
#define GNC_IS_TREE_VIEW_COMMODITY(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_VIEW_COMMODITY))
#define GNC_IS_TREE_VIEW_COMMODITY_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_VIEW_COMMODITY))
#define GNC_TREE_VIEW_COMMODITY_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_VIEW_COMMODITY, GncTreeViewCommodityClass))

/* typedefs & structures */
typedef struct
{
    GncTreeView gnc_tree_view;
    int stamp;
} GncTreeViewCommodity;

typedef struct
{
    GncTreeViewClass gnc_tree_view;
} GncTreeViewCommodityClass;



/* Get the GType for an GncTreeViewCommodity object. */
GType gnc_tree_view_commodity_get_type (void);


/** @name Commodity Tree View Constructors
 @{ */

/** Create a new commodity tree view.  This view may or may not show a
 *  pseudo top-level commodity.  The gnucash engine does not have a
 *  single top level commodity (it has a list of top level commodities),
 *  but this code provides one so that it can be used with all parts
 *  of the gnucash gui.
 *
 *  @param book The book containing the commodities to show.
 *
 *  @param first_property_name Pairs of property name/value settings
 *  for the newly created object.  Terminate the properties with a
 *  single NULL argument.
 *
 *  @return A pointer to a new commodity tree view.
 */
GtkTreeView *gnc_tree_view_commodity_new (QofBook *book,
        const gchar *first_property_name,
        ...);
/** @} */


/** @name Commodity Tree View Configuration
 @{ */

/** Configure (by name) the set of visible columns in an commodity tree
 *  view.  By default, only the commodity name column is show.  The
 *  avalible list of columns can be found in the file
 *  gnc-tree-view-commodity.c
 *
 *  @param view A pointer to an commodity tree view.
 *
 *  @param column_names A list of column names to make visible.
 */
void gnc_tree_view_commodity_configure_columns (GncTreeViewCommodity *view,
        GSList *column_names);

#ifdef OLD
/** Add a new column to the set of columns in an commodity tree view.
 *  This column will display the contents of a specified KVP slot.
 *
 *  @param view A pointer to an commodity tree view.
 *
 *  @param column_title The title for this new column.
 *
 *  @param kvp_key The lookup key to use for looking up data in the
 *  commodity KVP structures. The value associated with this key is what
 *  will be displayed in the column.
 */
void gnc_tree_view_commodity_add_kvp_column (GncTreeViewCommodity *view,
        const gchar *column_title,
        const gchar *kvp_key);
#endif
/** @} */


/** @name Commodity Tree View Filtering
 @{ */

typedef gboolean (*gnc_tree_view_commodity_ns_filter_func)(gnc_commodity_namespace*, gpointer data);
typedef gboolean (*gnc_tree_view_commodity_cm_filter_func)(gnc_commodity*, gpointer data);

/** This function attaches a filter function to the given commodity
 *  tree.  This function will be called for each commodity that the view
 *  thinks should possibly show.  The filter may perform any actions
 *  necessary on the commodity to decide whether it should be shown or
 *  not.  (I.E. Check type, placeholder status, etc.)  If the filter
 *  returns TRUE then the commodity wil be displayed.
 *
 *  @param view A pointer to an commodity tree view.
 *
 *  @param ns_func A filtration function that is called on individual
 *  tree elements that represent a namespace.  If this function
 *  returns TRUE, the namespace (and commodities under it) will be
 *  displayed.
 *
 *  @param cm_func A filtration function that is called on individual
 *  tree elements that represent a commodity.  If this function
 *  returns TRUE, the commodity will be displayed.
 *
 *  @param data A data block passed into each instance of the function.
 *
 *  @param destroy A function to destroy the data block.  This
 *  function will be called when the filter is destroyed.  may be
 *  NULL.
 */
void gnc_tree_view_commodity_set_filter (GncTreeViewCommodity *view,
        gnc_tree_view_commodity_ns_filter_func ns_func,
        gnc_tree_view_commodity_cm_filter_func cm_func,
        gpointer data,
        GtkDestroyNotify destroy);


/** This function forces the commodity tree filter to be evaluated.  It
 *  may be necessary to call this function if the initial state of the
 *  view is incorrect.  This appears to only be necessary if the
 *  filter affects one of the top level commodities in gnucash.
 *
 *  @note This calls a function in gtk that is annotated in the
 *  sources as being slow.  You have been warned.
 *
 *  @param view A pointer to an commodity tree view.
 */
void gnc_tree_view_commodity_refilter (GncTreeViewCommodity *view);
/** @} */


/** @name Commodity Tree View Get/Set Functions
 @{ */

/** This function determines if an commodity in the commodity tree view
 *  has any visible children.
 *
 *  @param view A pointer to an commodity tree view.
 *
 *  @param commodity A pointer to the commodity to check.
 *
 *  @return The number of children of the specified commodity. Returns 0
 *  on error.
 */
gint gnc_tree_view_commodity_count_children (GncTreeViewCommodity *view,
        gnc_commodity *commodity);



/** This function retrieves a pointer to a commodity based upon the
 *  model and iter passed into it.  It should only be used by callback
 *  functions that have had the model/iter passed to them.
 *
 *  @param column A pointer to a tree view column from a commodity view.
 *
 *  @param f_model A pointer to the filter model for the view.
 *
 *  @param f_iter A pointer to the iter for a particular row in the view.
 *
 *  @return The commodity associated with the spcified row in the view.
 */
gnc_commodity * gnc_tree_view_commodity_get_commodity_from_column (GtkTreeViewColumn *column,
        GtkTreeModel *f_model,
        GtkTreeIter  *f_iter);



/** This function returns the commodity associated with the specified
 *  path.  This function is useful in selection callbacks on an
 *  commodity tree widget.
 *
 *  @param view A pointer to an commodity tree view.
 *
 *  @param path A path specifying a node in the commodity tree.
 *
 *  @return The commodity associated with this path.
 */
gnc_commodity * gnc_tree_view_commodity_get_commodity_from_path (GncTreeViewCommodity *view,
        GtkTreePath *path);


/** This function returns the commodity in the commodity tree view at the
 *  current location of the cursor. (The outline frame. Usually is
 *  selected and therefore filled in, but not always.)
 *
 *  @param view A pointer to an commodity tree view.
 *
 *  @return The commodity at the cursor.
 */
gnc_commodity * gnc_tree_view_commodity_get_cursor_commodity (GncTreeViewCommodity *view);


/** This function returns the commodity associated with the selected
 *  item in the commodity tree view.
 *
 *  @note It only makes sense to call this function when the commodity
 *  tree is set to select a single item.  There is a different
 *  function to use when the tree supports multiple selections.
 *
 *  @param view A pointer to an commodity tree view.
 *
 *  @return The selected commodity, or NULL if no commodity was selected.
 */
gnc_commodity * gnc_tree_view_commodity_get_selected_commodity  (GncTreeViewCommodity *view);


/** This function selects an commodity in the commodity tree view.  All
 *  other commodities will be unselected.  In addition, this function
 *  collapses the entitre tree and then expands only the path to the
 *  selected commodity, making the item easy to find.  In general, this
 *  routine only need be called when initially putting up a window
 *  containing an commodity tree view widget.
 *
 *  @note It only makes sense to call this function when the commodity
 *  tree is set to select a single item.  There is a different
 *  function to use when the tree supports multiple selections.
 *
 *  @param view A pointer to an commodity tree view.
 *
 *  @param commodity A pointer to the commodity to select.
 */
void gnc_tree_view_commodity_set_selected_commodity (GncTreeViewCommodity *view,
        gnc_commodity *commodity);


/** This function returns a list of the commodities associated with the
 *  selected items in the commodity tree view.
 *
 *  @note It only makes sense to call this function when the commodity
 *  tree is set to select multiple items.  There is a different
 *  function to use when the tree supports single selection.
 *
 *  @param view A pointer to an commodity tree view.
 *
 *  @return A list of commodities, or NULL if no commodity was selected.
 */
GList * gnc_tree_view_commodity_get_selected_commodities (GncTreeViewCommodity *view);


/** This function selects a set of commodities in the commodity tree view.
 *  All other commodities will be unselected.  In addition, this function
 *  collapses the entitre tree and then expands only the path to the
 *  selected commodities, making them easy to find.  In general, this
 *  routine only need be called when initially putting up a window
 *  containing an commodity tree view widget.
 *
 *  @note It only makes sense to call this function when the commodity
 *  tree is set to select a single item.  There is a different
 *  function to use when the tree supports multiple selections.
 *
 *  @note It is the responsibility of the caller to free the returned
 *  list.
 *
 *  @param view A pointer to an commodity tree view.
 *
 *  @param commodity_list A list of commodities to select.
 *
 *  @param show_last Force the window to scroll to the last commodity
 *  selected.
 */
void gnc_tree_view_commodity_set_selected_commodities (GncTreeViewCommodity *view,
        GList *commodity_list,
        gboolean show_last);


/** This function selects all sub-commodities of an commodity in the
 *  commodity tree view.  All other commodities will be unselected.
 *
 *  @note It only makes sense to call this function when the commodity
 *  tree is set to select multiple items.  There is a different
 *  function to use when the tree supports multiple selections.
 *
 *  @param view A pointer to an commodity tree view.
 *
 *  @param commodity A pointer to the commodity whose children should be
 *  selected.
 */
void gnc_tree_view_commodity_select_subcommodities (GncTreeViewCommodity *view,
        gnc_commodity *commodity);

/** @} */

G_END_DECLS

#endif /* __GNC_TREE_VIEW_COMMODITY_H */

/** @} */
/** @} */
