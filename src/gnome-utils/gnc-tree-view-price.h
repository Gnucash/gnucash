/********************************************************************\
 * gnc-tree-view-price.h -- GtkTreeView implementation to display   *
 *                            prices in a GtkTreeView.              *
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
/** @file gnc-tree-view-price.h
    @brief GtkTreeView implementation for gnucash price tree.
    @author David Hampton <hampton@employees.org>
*/

#ifndef __GNC_TREE_VIEW_PRICE_H
#define __GNC_TREE_VIEW_PRICE_H

#include <gtk/gtktreemodel.h>
#include <gtk/gtktreeview.h>
#include "gnc-tree-view.h"

#include "gnc-pricedb.h"
#include "gnc-ui-util.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_VIEW_PRICE            (gnc_tree_view_price_get_type ())
#define GNC_TREE_VIEW_PRICE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_VIEW_PRICE, GncTreeViewPrice))
#define GNC_TREE_VIEW_PRICE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_VIEW_PRICE, GncTreeViewPriceClass))
#define GNC_IS_TREE_VIEW_PRICE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_VIEW_PRICE))
#define GNC_IS_TREE_VIEW_PRICE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_VIEW_PRICE))
#define GNC_TREE_VIEW_PRICE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_VIEW_PRICE, GncTreeViewPriceClass))

/* typedefs & structures */
typedef struct
{
    GncTreeView gnc_tree_view;
    int stamp;
} GncTreeViewPrice;

typedef struct
{
    GncTreeViewClass gnc_tree_view;
} GncTreeViewPriceClass;



/* Standard g_object type */
GType         gnc_tree_view_price_get_type              (void);


/** @name Price Tree View Constructors
 @{ */

/** Create a new price tree view.  This view may or may not show a
 *  pseudo top-level price.  The gnucash engine does not have a
 *  single top level price (it has a list of top level prices),
 *  but this code provides one so that it can be used with all parts
 *  of the gnucash gui.
 *
 *  @param book The book containing the prices to show.
 *
 *  @param first_property_name Pairs of property name/value settings
 *  for the newly created object.  Terminate the properties with a
 *  single NULL argument.
 *
 *  @return A pointer to a new price tree view.
 */
GtkTreeView *gnc_tree_view_price_new (QofBook *book,
                                      const gchar *first_property_name,
                                      ...);
/** @} */


/** @name Price Tree View Configuration
 @{ */

/** Configure (by name) the set of visible columns in an price tree
 *  view.  By default, only the price name column is show.  The
 *  avalible list of columns can be found in the file
 *  gnc-tree-view-price.c
 *
 *  @param price_view A pointer to an price tree view.
 *
 *  @param column_names A list of column names to make visible.
 */
void gnc_tree_view_price_configure_columns (GncTreeViewPrice *price_view,
        GSList *column_names);

#ifdef OLD
/** Add a new column to the set of columns in an price tree view.
 *  This column will display the contents of a specified KVP slot.
 *
 *  @param view A pointer to an price tree view.
 *
 *  @param column_title The title for this new column.
 *
 *  @param kvp_key The lookup key to use for looking up data in the
 *  price KVP structures. The value associated with this key is what
 *  will be displayed in the column.
 */
void gnc_tree_view_price_add_kvp_column (GncTreeViewPrice *view,
        const gchar *column_title,
        const gchar *kvp_key);
#endif
/** @} */


/** @name Price Tree View Filtering
 @{ */

/** This function attaches a filter function to the given price
 *  tree.  This function will be called for each price that the view
 *  thinks should possibly show.  The filter may perform any actions
 *  necessary on the price to decide whether it should be shown or
 *  not.  (I.E. Check type, placeholder status, etc.)  If the filter
 *  returns TRUE then the price wil be displayed.
 *
 *  @param price_view A pointer to an price tree view.
 *
 *  @param func A filtration function that is called on individual
 *  elements in the tree.  If this function returns TRUE, the price
 *  will be displayed.
 *
 *  @param data A data block passed into each instance of the function.
 *
 *  @param destroy A function to destroy the data block.  This
 *  function will be called when the filter is destroyed.  may be
 *  NULL.
 */
typedef gboolean (*gnc_tree_view_price_ns_filter_func)(gnc_commodity_namespace*, gpointer data);
typedef gboolean (*gnc_tree_view_price_cm_filter_func)(gnc_commodity *, gpointer data);
typedef gboolean (*gnc_tree_view_price_pc_filter_func)(GNCPrice *, gpointer data);
void gnc_tree_view_price_set_filter (GncTreeViewPrice *view,
                                     gnc_tree_view_price_ns_filter_func ns_func,
                                     gnc_tree_view_price_cm_filter_func cm_func,
                                     gnc_tree_view_price_pc_filter_func pc_func,
                                     gpointer data,
                                     GtkDestroyNotify destroy);

/** This function forces the price tree filter to be evaluated.  It
 *  may be necessary to call this function if the initial state of the
 *  view is incorrect.  This appears to only be necessary if the
 *  filter affects one of the top level prices in gnucash.
 *
 *  @note This calls a function in gtk that is annotated in the
 *  sources as being slow.  You have been warned.
 *
 *  @param view A pointer to an price tree view.
 */
void gnc_tree_view_price_refilter (GncTreeViewPrice *view);
/** @} */


/** @name Price Tree View Get/Set Functions
 @{ */

/** This function determines if an price in the price tree view
 *  has any visible children.
 *
 *  @param view A pointer to an price tree view.
 *
 *  @param price A pointer to the price to check.
 *
 *  @return The number of children of the specified price. Returns 0
 *  on error.
 */
gint gnc_tree_view_price_count_children (GncTreeViewPrice *view,
        GNCPrice *price);



GNCPrice * gnc_tree_view_price_get_price_from_column (GtkTreeViewColumn *column,
        GtkTreeModel *f_model,
        GtkTreeIter  *f_iter);



/** This function returns the price associated with the specified
 *  path.  This function is useful in selection callbacks on an
 *  price tree widget.
 *
 *  @param view A pointer to an price tree view.
 *
 *  @param path A path specifying a node in the price tree.
 *
 *  @return The price associated with this path.
 */
GNCPrice * gnc_tree_view_price_get_price_from_path (GncTreeViewPrice *view,
        GtkTreePath *path);


/** This function returns the price in the price tree view at the
 *  current location of the cursor. (The outline frame. Usually is
 *  selected and therefore filled in, but not always.)
 *
 *  @param view A pointer to an price tree view.
 *
 *  @return The price at the cursor.
 */
GNCPrice * gnc_tree_view_price_get_cursor_price (GncTreeViewPrice *view);


/** This function returns the price associated with the selected
 *  item in the price tree view.
 *
 *  @note It only makes sense to call this function when the price
 *  tree is set to select a single item.  There is a different
 *  function to use when the tree supports multiple selections.
 *
 *  @param view A pointer to an price tree view.
 *
 *  @return The selected price, or NULL if no price was selected.
 */
GNCPrice * gnc_tree_view_price_get_selected_price (GncTreeViewPrice *view);


/** This function selects an price in the price tree view.  All
 *  other prices will be unselected.  In addition, this function
 *  collapses the entitre tree and then expands only the path to the
 *  selected price, making the item easy to find.  In general, this
 *  routine only need be called when initially putting up a window
 *  containing an price tree view widget.
 *
 *  @note It only makes sense to call this function when the price
 *  tree is set to select a single item.  There is a different
 *  function to use when the tree supports multiple selections.
 *
 *  @param view A pointer to an price tree view.
 *
 *  @param price A pointer to the price to select.
 */
void gnc_tree_view_price_set_selected_price (GncTreeViewPrice *view,
        GNCPrice *price);


/** This function returns a list of the prices associated with the
 *  selected items in the price tree view.
 *
 *  @note It only makes sense to call this function when the price
 *  tree is set to select multiple items.  There is a different
 *  function to use when the tree supports single selection.
 *
 *  @param view A pointer to an price tree view.
 *
 *  @return A list of prices, or NULL if no price was selected.
 */
GList * gnc_tree_view_price_get_selected_prices (GncTreeViewPrice *view);


/** This function selects a set of prices in the price tree view.
 *  All other prices will be unselected.  In addition, this function
 *  collapses the entitre tree and then expands only the path to the
 *  selected prices, making them easy to find.  In general, this
 *  routine only need be called when initially putting up a window
 *  containing an price tree view widget.
 *
 *  @note It only makes sense to call this function when the price
 *  tree is set to select a single item.  There is a different
 *  function to use when the tree supports multiple selections.
 *
 *  @note It is the responsibility of the caller to free the returned
 *  list.
 *
 *  @param view A pointer to an price tree view.
 *
 *  @param price_list A pointer to the list of prices to select.
 *
 *  @param show_last Force the window to scroll to the last price
 *  selected.
 */
void gnc_tree_view_price_set_selected_prices (GncTreeViewPrice *view,
        GList *price_list,
        gboolean show_last);
/** @} */

G_END_DECLS

#endif /* __GNC_TREE_VIEW_PRICE_H */

/** @} */
/** @} */
