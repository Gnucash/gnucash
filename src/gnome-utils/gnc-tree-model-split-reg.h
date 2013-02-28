/********************************************************************\
 * gnc-tree-model-split-reg.h -- GtkTreeView implementation to      *
 *                     display registers   in a GtkTreeView.        *
 *                                                                  *
 * Copyright (C) 2006-2007 Chris Shoemaker <c.shoemaker@cox.net>    *
 * Copyright (C) 2012 Robert Fewell                                 *
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
    BANK_REGISTER2,             //0
    CASH_REGISTER2,             //1
    ASSET_REGISTER2,            //2
    CREDIT_REGISTER2,           //3
    LIABILITY_REGISTER2,        //4
    INCOME_REGISTER2,           //5
    EXPENSE_REGISTER2,          //6
    EQUITY_REGISTER2,           //7
    STOCK_REGISTER2,            //8
    CURRENCY_REGISTER2,         //9
    RECEIVABLE_REGISTER2,       //10
    PAYABLE_REGISTER2,          //11
    TRADING_REGISTER2,          //12
    NUM_SINGLE_REGISTER_TYPES2, //13

    GENERAL_LEDGER2 = NUM_SINGLE_REGISTER_TYPES2, //13
    INCOME_LEDGER2,             //14
    PORTFOLIO_LEDGER2,          //15
    SEARCH_LEDGER2,             //16

    NUM_REGISTER_TYPES2         //17
} SplitRegisterType2;


/** Register styles */
typedef enum
{
    REG2_STYLE_LEDGER,      //0
    REG2_STYLE_AUTO_LEDGER, //1
    REG2_STYLE_JOURNAL      //2
} SplitRegisterStyle2;


typedef enum
{
    GNC_TREE_MODEL_SPLIT_REG_COL_GUID,      //0
    GNC_TREE_MODEL_SPLIT_REG_COL_DATE,      //1
    GNC_TREE_MODEL_SPLIT_REG_COL_DUEDATE,   //2
    GNC_TREE_MODEL_SPLIT_REG_COL_NUMACT,    //3
    GNC_TREE_MODEL_SPLIT_REG_COL_DESCNOTES, //4
    GNC_TREE_MODEL_SPLIT_REG_COL_TRANSVOID, //5
    GNC_TREE_MODEL_SPLIT_REG_COL_RECN,      //6

    GNC_TREE_MODEL_SPLIT_REG_COL_LAST_VISIBLE = GNC_TREE_MODEL_SPLIT_REG_COL_RECN, //6

    /* internal hidden columns */
    GNC_TREE_MODEL_SPLIT_REG_COL_RO,        //7

    GNC_TREE_MODEL_SPLIT_REG_NUM_COLUMNS    //8
} GncTreeModelSplitRegColumn;

/* typedefs & structures */
typedef struct GncTreeModelSplitRegPrivate GncTreeModelSplitRegPrivate;

/** The instance data structure for an account tree model. */
typedef struct 
{
    GncTreeModel                 gnc_tree_model;        /**< The parent object data. */
    GncTreeModelSplitRegPrivate *priv;
    gint                         stamp;                 /**< The state of the model. Any state change increments this number. */

    SplitRegisterType2           type;                  /**<FIXME ? This may be the wrong place for these, may be the view ? */
    SplitRegisterStyle2          style;                 /**<FIXME ? This may be the wrong place for these, may be the view ? */
    gboolean                     use_double_line;       /**<FIXME ? As above, whether to use two lines per transaction */

    gboolean                     is_template;

    gboolean                     use_accounting_labels; /**< whether to use accounting Labels */
    gboolean                     separator_changed;     /**< whether the separator has changed */ 
    gboolean                     alt_colors_by_txn;     /**< whether to use alternative colors by transaction */ 
    gboolean                     use_theme_colors;      /**< whether to use theme colors */

    gboolean                     read_only;             /**< register is read only */


}GncTreeModelSplitReg;


/** The class data structure for an account tree model. */
typedef struct
{
    GncTreeModelClass gnc_tree_model;                   /**< The parent object data. */

    /* This signal is emitted to refresh the view */
    void (*refresh_view) (GncTreeModelSplitReg *model, gpointer user_data);

    /* This signal is emitted to refresh the status bar */
    void (*refresh_status_bar) (GncTreeModelSplitReg *model, gpointer user_data);

    /* This signal is emitted before a transaction delete, the pointer has
       the transaction */
    void (*trans_delete) (GncTreeModelSplitReg *model, gpointer item);

} GncTreeModelSplitRegClass;


/** Callback function type - Used to get parent window */
typedef GtkWidget *(*SRGetParentCallback2) (gpointer user_data);

/** Get the type of split register tree plugin.
 *
 *  @return A GType.
 */
GType gnc_tree_model_split_reg_get_type (void);

/** Create new model and set options for register. */
GncTreeModelSplitReg *
gnc_tree_model_split_reg_new (SplitRegisterType2 reg_type, SplitRegisterStyle2 style,
                        gboolean use_double_line, gboolean is_template);

/** Load the model from a slist and set default account for register. */
void gnc_tree_model_split_reg_load (GncTreeModelSplitReg *model, GList * slist, Account *default_account);

/** FIXME Not sure what this is for yet. */
void gnc_tree_model_split_reg_set_template_account (GncTreeModelSplitReg *model, Account *template_account);

/** Destroy the model. */
void gnc_tree_model_split_reg_destroy (GncTreeModelSplitReg *model);

/** Sets the user data and callback hooks for the register. */
void gnc_tree_model_split_reg_set_data (GncTreeModelSplitReg *model, gpointer user_data,
                                  SRGetParentCallback2 get_parent);

/** Returns the parent Window of the register. */
GtkWidget * gnc_tree_model_split_reg_get_parent (GncTreeModelSplitReg *model);

/** Set style and type for register. */
void gnc_tree_model_split_reg_config (GncTreeModelSplitReg *model, SplitRegisterType2 newtype,
                                      SplitRegisterStyle2 newstyle, gboolean use_double_line);

/** Return the default account for this register model. */
Account * gnc_tree_model_split_reg_get_anchor (GncTreeModelSplitReg *model);

/** Commit the blank split. */
void gnc_tree_model_split_reg_commit_blank_split (GncTreeModelSplitReg *model);

/** Set display general ledger and show sub accounts. */
void gnc_tree_model_split_reg_set_display (GncTreeModelSplitReg *model, gboolean subacc, gboolean gl);

/* These are to do with autocompletion */
GtkListStore * gnc_tree_model_split_reg_get_description_list (GncTreeModelSplitReg *model);

GtkListStore * gnc_tree_model_split_reg_get_notes_list (GncTreeModelSplitReg *model);

GtkListStore * gnc_tree_model_split_reg_get_memo_list (GncTreeModelSplitReg *model);

GtkListStore * gnc_tree_model_split_reg_get_numact_list (GncTreeModelSplitReg *model);

GtkListStore * gnc_tree_model_split_reg_get_acct_list (GncTreeModelSplitReg *model);

void gnc_tree_model_split_reg_get_num_list (GncTreeModelSplitReg *model);

void gnc_tree_model_split_reg_get_action_list (GncTreeModelSplitReg *model);

void gnc_tree_model_split_reg_update_completion (GncTreeModelSplitReg *model);


/* Get the split and transaction */
gboolean gnc_tree_model_split_reg_get_split_and_trans (
          GncTreeModelSplitReg *model, GtkTreeIter *iter,
          gboolean *is_trow1, gboolean *is_trow2, gboolean *is_split,
          gboolean *is_blank, Split **split, Transaction **trans);

/* Return FALSE if failure */
gboolean gnc_tree_model_split_reg_set_blank_split_parent (
          GncTreeModelSplitReg *model, Transaction *trans, gboolean remove_only);

/* Return the blank split */
Split * gnc_tree_model_split_get_blank_split (GncTreeModelSplitReg *model);

/* Return the blank trans */
Transaction * gnc_tree_model_split_get_blank_trans (GncTreeModelSplitReg *model);

/* If 'trans' is NULL, use split's parent.  If 'split' is NULL, just
   get the transaction iter. */
gboolean gnc_tree_model_split_reg_get_iter_from_trans_and_split (
          GncTreeModelSplitReg *model, Transaction *trans, Split *split,
          GtkTreeIter *iter1, GtkTreeIter *iter2);

/* Return the row color for the view */
gchar * gnc_tree_model_split_reg_get_row_color (GncTreeModelSplitReg *model, gboolean is_trow1,
         gboolean is_trow2, gboolean is_split, gint num);

/* Return TRUE if this transaction is read only for the view */
gboolean
gnc_tree_model_split_reg_get_read_only (GncTreeModelSplitReg *model, Transaction *trans);

/*FIXME this may not be required in the long run, return TRUE if this is a sub account view */
gboolean
gnc_tree_model_split_reg_get_sub_account (GncTreeModelSplitReg *model);

/* Return the tree path, if split and trans are null, last in list returned */
GtkTreePath * gnc_tree_model_split_reg_get_path_to_split_and_trans (
               GncTreeModelSplitReg *model, Split *split, Transaction *trans);

/* Returns TRUE if iter is a blank transaction */
gboolean gnc_tree_model_split_reg_is_blank_trans (GncTreeModelSplitReg *model, GtkTreeIter *iter);

/*****************************************************************************/

G_END_DECLS

#endif /* __GNC_TREE_MODEL_SPLIT_REG_H */
