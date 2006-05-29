/********************************************************************\
 * gnc-tree-model-transaction.h -- GtkTreeModel implementation to   *
 *                        display Transactions in a GtkTreeView.    *
 * Copyright (C) 2006 Chris Shoemaker <c.shoemaker@cox.net>         *
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


#ifndef __GNC_TREE_MODEL_TRANSACTION_H
#define __GNC_TREE_MODEL_TRANSACTION_H

#include <gtk/gtktreemodel.h>
#include "gnc-tree-model.h"

#include "Query.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_MODEL_TRANSACTION            (gnc_tree_model_transaction_get_type ())
#define GNC_TREE_MODEL_TRANSACTION(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_MODEL_TRANSACTION, GncTreeModelTransaction))
#define GNC_TREE_MODEL_TRANSACTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_MODEL_TRANSACTION, GncTreeModelTransactionClass))
#define GNC_IS_TREE_MODEL_TRANSACTION(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_MODEL_TRANSACTION))
#define GNC_IS_TREE_MODEL_TRANSACTION_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_MODEL_TRANSACTION))
#define GNC_TREE_MODEL_TRANSACTION_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_MODEL_TRANSACTION, GncTreeModelTransactionClass))
#define GNC_TREE_MODEL_TRANSACTION_NAME            "GncTreeModelTransaction"


typedef enum {
    GNC_TREE_MODEL_TRANSACTION_COL_GUID,
    GNC_TREE_MODEL_TRANSACTION_COL_DATE,
    GNC_TREE_MODEL_TRANSACTION_COL_NUM,
    GNC_TREE_MODEL_TRANSACTION_COL_DESCRIPTION,
    //GNC_TREE_MODEL_TRANSACTION_COL_REC,
    GNC_TREE_MODEL_TRANSACTION_NUM_COLUMNS,
} GncTreeModelTransactionColumn;

/* typedefs & structures */
typedef struct GncTreeModelTransactionPrivate GncTreeModelTransactionPrivate;

typedef struct {
    GncTreeModel gnc_tree_model;
    GncTreeModelTransactionPrivate *priv;
    int stamp;
} GncTreeModelTransaction;

typedef struct {
    GncTreeModelClass gnc_tree_model;
} GncTreeModelTransactionClass;

/* Standard g_object type */
GType gnc_tree_model_transaction_get_type (void);

QofBook *gnc_tree_model_transaction_get_book(GncTreeModelTransaction *model);

/** @name Transaction Tree Model Constructors
 @{ */

/** Create a new GtkTreeModel for manipulating GnuCash transactions. */
GncTreeModelTransaction *
gnc_tree_model_transaction_new_from_query (Query *query);
GncTreeModelTransaction *
gnc_tree_model_transaction_new_from_account (Account *acc);
/** @} */


/** @name Transaction Tree Model Get/Set Functions
  @{ */

#if 0
/** Convert a model/iter pair to a gnucash split.  This routine should
 *  only be called from an split tree view filter function.  The
 *  model and iter values will be provided as part of the call to the
 *  filter.
 *
 *  @param model A pointer to the split tree model.
 *
 *  @param iter A gtk_tree_iter corresponding to a single split in
 *  the model.
 *
 *  @return A pointer to the corresponding split.
 */
Transaction *gnc_tree_model_transaction_get_split (
    GncTreeModelTransaction *model, GtkTreeIter *iter);


/** Convert a model/split pair into a gtk_tree_model_iter.  This
 *  routine should only be called from the file
 *  gnc-tree-view-split.c.
 *
 *  @internal
 *
 *  @param model The model that an split belongs to.
 *
 *  @param split The split to convert.
 *
 *  @param iter A pointer to an iter.  This iter will be rewritten to
 *  contain the results of the query.
 *
 *  @return TRUE if the split was found and the iter filled
 *  in. FALSE otherwise.
 */
gboolean gnc_tree_model_transaction_get_iter_from_trans (
    GncTreeModelTransaction *model,
    Transaction *trans,
    GtkTreeIter *iter);


/** Convert a model/split pair into a gtk_tree_model_path.  This
 *  routine should only be called from the file
 *  gnc-tree-view-split.c.
 *
 *  @internal
 *
 *  @param model The model that an split belongs to.
 *
 *  @param split The split to convert.
 *
 *  @return A pointer to a path describing the split.  It is the
 *  responsibility of the caller to free this path when done.
 */
GtkTreePath *gnc_tree_model_transaction_get_path_from_trans (
    GncTreeModelTransaction *model, Transaction *trans);
#endif

gboolean gnc_tree_model_transaction_set_blank_split_parent(
    GncTreeModelTransaction *model, Transaction *trans);

Account *
gnc_tree_model_transaction_get_anchor(GncTreeModelTransaction *model);

gboolean
gnc_tree_model_transaction_get_split_and_trans (
    GncTreeModelTransaction *model, GtkTreeIter *iter,
    gboolean *is_split, gboolean *is_blank,
    Split **split, Transaction **trans);

void
gnc_tree_model_transaction_commit_split(GncTreeModelTransaction *model,
                                        Split *split);
gint gtmt_sort_by_date(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b,
                       gpointer user_data);

gboolean
gnc_tree_model_transaction_get_iter_from_trans(
    GncTreeModelTransaction *model, Transaction *trans, GtkTreeIter *iter);

/** @} */

G_END_DECLS

#endif /* __GNC_TREE_MODEL_TRANSACTION_H */

/** @} */
/** @} */
