/********************************************************************\
 * gnc-tree-model-split-reg.h -- GtkTreeView implementation to      *
 *                     display registers   in a GtkTreeView.        *
 *                                                                  *
 * Copyright (C) 2012 Robert Fewell                                 *
 * Copyright (C) 2006-2007 Chris Shoemaker <c.shoemaker@cox.net>    *
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


#ifndef __GNC_TREE_MODEL_SPLIT_REG_H
#define __GNC_TREE_MODEL_SPLIT_REG_H

#include <gtk/gtk.h>
#include "gnc-tree-model.h"

#include "Query.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_MODEL_SPLIT_REG            (gnc_tree_model_split_reg_get_type ())
#define GNC_TREE_MODEL_SPLIT_REG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_MODEL_SPLIT_REG, GncTreeModelSplitReg))
#define GNC_TREE_MODEL_SPLIT_REG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_MODEL_SPLIT_REG, GncTreeModelSplitRegClass))
#define GNC_IS_TREE_MODEL_SPLIT_REG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_MODEL_SPLIT_REG))
#define GNC_IS_TREE_MODEL_SPLIT_REG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_MODEL_SPLIT_REG))
#define GNC_TREE_MODEL_SPLIT_REG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_MODEL_SPLIT_REG, GncTreeModelSplitRegClass))
#define GNC_TREE_MODEL_SPLIT_REG_NAME            "GncTreeModelSplitReg"


/** @brief Register types
 *
 * "registers" are single-account display windows.
 * "ledgers" are multiple-account display windows */
typedef enum
{
    BANK_REGISTER2,
    CASH_REGISTER2,
    ASSET_REGISTER2,
    CREDIT_REGISTER2,
    LIABILITY_REGISTER2,
    INCOME_REGISTER2,
    EXPENSE_REGISTER2,
    EQUITY_REGISTER2,
    STOCK_REGISTER2,
    CURRENCY_REGISTER2,
    RECEIVABLE_REGISTER2,
    PAYABLE_REGISTER2,
    TRADING_REGISTER2,
    NUM_SINGLE_REGISTER_TYPES2,

    GENERAL_LEDGER2 = NUM_SINGLE_REGISTER_TYPES2,
    INCOME_LEDGER2,
    PORTFOLIO_LEDGER2,
    SEARCH_LEDGER2,

    NUM_REGISTER_TYPES2
} SplitRegisterType2;

/** Register styles */
typedef enum
{
    REG_STYLE_LEDGER2,
    REG_STYLE_AUTO_LEDGER2,
    REG_STYLE_JOURNAL2
} SplitRegisterStyle2;


typedef enum
{
    GNC_TREE_MODEL_SPLIT_REG_COL_GUID,
    GNC_TREE_MODEL_SPLIT_REG_COL_DATE,
    GNC_TREE_MODEL_SPLIT_REG_COL_NUMACT,
    GNC_TREE_MODEL_SPLIT_REG_COL_DESCNOTES,
    GNC_TREE_MODEL_SPLIT_REG_COL_TRANSVOID,

    GNC_TREE_MODEL_SPLIT_REG_COL_LAST_VISIBLE = GNC_TREE_MODEL_SPLIT_REG_COL_TRANSVOID,

    /* internal hidden columns */
    GNC_TREE_MODEL_SPLIT_REG_COL_COLOR,

    GNC_TREE_MODEL_SPLIT_REG_NUM_COLUMNS
} GncTreeModelSplitRegColumn;

/* typedefs & structures */
typedef struct GncTreeModelSplitRegPrivate GncTreeModelSplitRegPrivate;

/** The instance data structure for an account tree model. */
typedef struct
{
    GncTreeModel gnc_tree_model;    /**< The parent object data. */
    GncTreeModelSplitRegPrivate *priv;
    int stamp;                      /**< The state of the model. Any state
                                     *   change increments this number. */

    SplitRegisterType2 type;       /* This may be the wrong place for these, may be the view ? */
    SplitRegisterStyle2 style;


} GncTreeModelSplitReg;


/** The class data structure for an account tree model. */
typedef struct
{
    GncTreeModelClass gnc_tree_model;   /**< The parent object data. */
} GncTreeModelSplitRegClass;



/** Get the type of split register tree plugin.
 *
 *  @return A GType.
 */
GType gnc_tree_model_split_reg_get_type (void);


/** @name Split Register Tree Model Constructors
 @{ */

/** Create a new GtkTreeModel for manipulating gnucash splits.
 *
 *  @param The Query
 */
GncTreeModelSplitReg *gnc_tree_model_split_reg_new_from_account (Account *acc);
/** @} */

/** @name Split Register Tree Model Constructors
 @{ */

/** Create a new GtkTreeModel for manipulating gnucash splits.
 *
 *  @param The Query
 */
GncTreeModelSplitReg *gnc_tree_model_split_reg_new_from_query (Query *query);
/** @} */

/** Get the Account of GtkTreeModel.
 *
 *  @param The model
 */
Account * gnc_tree_model_split_reg_get_anchor(GncTreeModelSplitReg *model);
/** @} */

GtkListStore *
gnc_tree_model_split_reg_get_description_list(GncTreeModelSplitReg *model);

GtkListStore *
gnc_tree_model_split_reg_get_notes_list(GncTreeModelSplitReg *model);

GtkListStore *
gnc_tree_model_split_reg_get_memo_list(GncTreeModelSplitReg *model);

GtkListStore *
gnc_tree_model_split_reg_get_numact_list(GncTreeModelSplitReg *model);

GtkListStore *
gnc_tree_model_split_reg_get_acct_list(GncTreeModelSplitReg *model);

void
gnc_tree_model_split_reg_get_num_list(GncTreeModelSplitReg *model);

void
gnc_tree_model_split_reg_get_action_list(GncTreeModelSplitReg *model);


gboolean
gnc_tree_model_split_reg_get_split_and_trans (
    GncTreeModelSplitReg *model, GtkTreeIter *iter,
    gboolean *is_trow1, gboolean *is_trow2, gboolean *is_split,
    gboolean *is_blank, Split **split, Transaction **trans);


G_END_DECLS

#endif /* __GNC_TREE_MODEL_SPLIT_REG_H */

/** @} */
/** @} */
