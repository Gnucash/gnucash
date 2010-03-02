/*
 * gnc-tree-model-price.h -- GtkTreeModel implementation to display
 *	prices in a GtkTreeView.
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
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
/** @addtogroup GuiTreeModel
 * @{ */
/** @file gnc-tree-model-price.h
    @brief GtkTreeModel implementation for gnucash price database.
    @author Jan Arne Petersen <jpetersen@uni-bonn.de>
    @author David Hampton <hampton@employees.org>
*/

#ifndef __GNC_TREE_MODEL_PRICE_H
#define __GNC_TREE_MODEL_PRICE_H

#include <gtk/gtktreemodel.h>
#include "gnc-tree-model.h"

#include "gnc-pricedb.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_MODEL_PRICE            (gnc_tree_model_price_get_type ())
#define GNC_TREE_MODEL_PRICE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_MODEL_PRICE, GncTreeModelPrice))
#define GNC_TREE_MODEL_PRICE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_MODEL_PRICE, GncTreeModelPriceClass))
#define GNC_IS_TREE_MODEL_PRICE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_MODEL_PRICE))
#define GNC_IS_TREE_MODEL_PRICE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_MODEL_PRICE))
#define GNC_TREE_MODEL_PRICE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_MODEL_PRICE, GncTreeModelPriceClass))
#define GNC_TREE_MODEL_PRICE_NAME            "GncTreeModelPrice"


typedef enum
{
    GNC_TREE_MODEL_PRICE_COL_COMMODITY,
    GNC_TREE_MODEL_PRICE_COL_CURRENCY,
    GNC_TREE_MODEL_PRICE_COL_DATE,
    GNC_TREE_MODEL_PRICE_COL_SOURCE,
    GNC_TREE_MODEL_PRICE_COL_TYPE,
    GNC_TREE_MODEL_PRICE_COL_VALUE,

    GNC_TREE_MODEL_PRICE_COL_LAST_VISIBLE = GNC_TREE_MODEL_PRICE_COL_VALUE,

    /* internal hidden columns */
    GNC_TREE_MODEL_PRICE_COL_VISIBILITY,
    GNC_TREE_MODEL_PRICE_NUM_COLUMNS
} GncTreeModelPriceColumn;

/* typedefs & structures */

/** The instance data structure for a price tree model. */
typedef struct
{
    GncTreeModel gnc_tree_model;	/**< The parent object data. */
    int stamp;			/**< The state of the model. Any state
					 *   change increments this number. */
} GncTreeModelPrice;


/** The class data structure for a price tree model. */
typedef struct
{
    GncTreeModelClass gnc_tree_model;/**< The parent object data. */
} GncTreeModelPriceClass;


/** Get the type of a price tree plugin.
 *
 *  @return A GType.
 */
GType gnc_tree_model_price_get_type (void);


/** @name Account Tree Model Constructors
 @{ */

/** Create a new GtkTreeModel for manipulating gnucash commodity prices.
 *
 *  @param book The book that holds these prices.
 *
 *  @param price_db A pointer to the price database for this tree.
 *  All namespaces and commodities in the book, and all prices in this
 *  tree will be included. */
GtkTreeModel *gnc_tree_model_price_new (QofBook *book, GNCPriceDB *price_db);
/** @} */


/** @name Price Tree Model Filter Helper Functions
 @{ */

/** Determine whether or not the specified GtkTreeIter points to a
 *  "commodity namespace".  This routine should only be called from a
 *  price tree view filter function.  The model and iter values will
 *  be provided as part of the call to the filter.
 *
 *  @param model A pointer to the price tree model.
 *
 *  @param iter A GtkTreeIter corresponding to a single item in the
 *  model.
 *
 *  @return TRUE if the iter points to a commodity namespace, FALSE
 *  otherwise. */
gboolean gnc_tree_model_price_iter_is_namespace (GncTreeModelPrice *model,
        GtkTreeIter *iter);


/** Determine whether or not the specified GtkTreeIter points to a
 *  commodity.  This routine should only be called from a price
 *  tree view filter function.  The model and iter values will be
 *  provided as part of the call to the filter.
 *
 *  @param model A pointer to the price tree model.
 *
 *  @param iter A GtkTreeIter corresponding to a single item in the
 *  model.
 *
 *  @return TRUE if the iter points to a commodity, FALSE
 *  otherwise. */
gboolean gnc_tree_model_price_iter_is_commodity (GncTreeModelPrice *model,
        GtkTreeIter *iter);


/** Determine whether or not the specified GtkTreeIter points to a
 *  price.  This routine should only be called from a price tree
 *  view filter function.  The model and iter values will be provided
 *  as part of the call to the filter.
 *
 *  @param model A pointer to the price tree model.
 *
 *  @param iter A GtkTreeIter corresponding to a single item in the
 *  model.
 *
 *  @return TRUE if the iter points to a price, FALSE
 *  otherwise. */
gboolean gnc_tree_model_price_iter_is_price (GncTreeModelPrice *model,
        GtkTreeIter *iter);


/** Convert a model/iter pair to a gnucash commodity namespace.  This
 *  routine should only be called from a commodity tree view filter
 *  function.  The model and iter values will be provided as part of
 *  the call to the filter.
 *
 *  @param model A pointer to the commodity tree model.
 *
 *  @param iter A GtkTreeIter corresponding to a single namespace in
 *  the model.
 *
 *  @return A pointer to the corresponding namespace. */
gnc_commodity_namespace *gnc_tree_model_price_get_namespace (GncTreeModelPrice *model,
        GtkTreeIter *iter);


/** Convert a model/iter pair to a gnucash commodity.  This routine
 *  should only be called from a commodity tree view filter function.
 *  The model and iter values will be provided as part of the call to
 *  the filter.
 *
 *  @param model A pointer to the commodity tree model.
 *
 *  @param iter A GtkTreeIter corresponding to a single commodity in
 *  the model.
 *
 *  @return A pointer to the corresponding commodity. */
gnc_commodity *gnc_tree_model_price_get_commodity (GncTreeModelPrice *model,
        GtkTreeIter *iter);


/** Convert a model/iter pair to a gnucash price.  This routine should
 *  only be called from a price tree view filter function.  The model
 *  and iter values will be provided as part of the call to the
 *  filter.
 *
 *  @param model A pointer to the price tree model.
 *
 *  @param iter A GtkTreeIter corresponding to a single price in
 *  the model.
 *
 *  @return A pointer to the corresponding price. */
GNCPrice *gnc_tree_model_price_get_price (GncTreeModelPrice *model,
        GtkTreeIter *iter);
/** @} */



/** @name Commodity Tree Model Lookup Functions
 @{ */

/** Convert a commodity namespace pointer into a GtkTreeIter.
 *
 *  @param model A pointer to the price tree model.
 *
 *  @param namespace A pointer to the gnucash commodity namespace.
 *
 *  @param iter A pointer to a GtkTreeIter.  This iter will be filled
 *  in to point where the namespace appears in the price tree.
 *
 *  @return TRUE if the returned iter is valid, FALSE otherwise. */
gboolean gnc_tree_model_price_get_iter_from_namespace (GncTreeModelPrice *model,
        gnc_commodity_namespace *namespace,
        GtkTreeIter *iter);

/** Convert a commodity pointer into a GtkTreeIter.
 *
 *  @param model A pointer to the price tree model.
 *
 *  @param commodity A pointer to the gnucash commodity.
 *
 *  @param iter A pointer to a GtkTreeIter.  This iter will be filled
 *  in to point where the commodity appears in the price tree.
 *
 *  @return TRUE if the returned iter is valid, FALSE otherwise. */
gboolean gnc_tree_model_price_get_iter_from_commodity (GncTreeModelPrice *model,
        gnc_commodity *commodity,
        GtkTreeIter *iter);

/** Convert a price pointer into a GtkTreeIter.
 *
 *  @param model A pointer to the price tree model.
 *
 *  @param price A pointer to the gnucash price.
 *
 *  @param iter A pointer to a GtkTreeIter.  This iter will be filled
 *  in to point where the price appears in the price tree.
 *
 *  @return TRUE if the returned iter is valid, FALSE otherwise. */
gboolean gnc_tree_model_price_get_iter_from_price (GncTreeModelPrice *model,
        GNCPrice *price,
        GtkTreeIter *iter);

/** Convert a commodity namespace pointer into a GtkTreePath.
 *
 *  @param model A pointer to the price tree model.
 *
 *  @param namespace A pointer to the gnucash commodity namespace.
 *
 *  @return A pointer to a GtkTreePath describing the location of this
 *  namespace.  This pointer must be freed by the caller when no
 *  longer needed.  This routine will return NULL if the namespace
 *  does not exist in the tree. */
GtkTreePath *gnc_tree_model_price_get_path_from_namespace (GncTreeModelPrice *model,
        gnc_commodity_namespace *namespace);

/** Convert a commodity pointer into a GtkTreePath.
 *
 *  @param model A pointer to the price tree model.
 *
 *  @param commodity A pointer to the gnucash commodity.
 *
 *  @return A pointer to a GtkTreePath describing the location of this
 *  commodity.  This pointer must be freed by the caller when no
 *  longer needed.  This routine will return NULL if the commodity
 *  does not exist in the tree. */
GtkTreePath *gnc_tree_model_price_get_path_from_commodity (GncTreeModelPrice *model,
        gnc_commodity *commodity);

/** Convert a price pointer into a GtkTreePath.
 *
 *  @param model A pointer to the price tree model.
 *
 *  @param price A pointer to the commodity price.
 *
 *  @return A pointer to a GtkTreePath describing the location of this
 *  price.  This pointer must be freed by the caller when no longer
 *  needed.  This routine will return NULL if the price does not exist
 *  in the tree. */
GtkTreePath *gnc_tree_model_price_get_path_from_price (GncTreeModelPrice *model,
        GNCPrice *price);
/** @} */


G_END_DECLS

#endif /* __GNC_TREE_MODEL_PRICE_H */

/** @} */
/** @} */
