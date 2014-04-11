/* 
 * gnc-tree-model-account.h -- GtkTreeModel implementation to
 *	display accounts in a GtkTreeView.
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
/** @addtogroup GuiTreeModel GnuCash Tree Model
    @{ */
/** @file gnc-tree-model-account.h
    @brief GtkTreeModel implementation for gnucash account tree.
    @author Jan Arne Petersen <jpetersen@uni-bonn.de>
    @author David Hampton <hampton@employees.org>
*/

#ifndef __GNC_TREE_MODEL_ACCOUNT_H
#define __GNC_TREE_MODEL_ACCOUNT_H

#include <gtk/gtktreemodel.h>
#include "gnc-tree-model.h"

#include "Account.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_MODEL_ACCOUNT            (gnc_tree_model_account_get_type ())
#define GNC_TREE_MODEL_ACCOUNT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_MODEL_ACCOUNT, GncTreeModelAccount))
#define GNC_TREE_MODEL_ACCOUNT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_MODEL_ACCOUNT, GncTreeModelAccountClass))
#define GNC_IS_TREE_MODEL_ACCOUNT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_MODEL_ACCOUNT))
#define GNC_IS_TREE_MODEL_ACCOUNT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_MODEL_ACCOUNT))
#define GNC_TREE_MODEL_ACCOUNT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_MODEL_ACCOUNT, GncTreeModelAccountClass))
#define GNC_TREE_MODEL_ACCOUNT_NAME            "GncTreeModelAccount"


typedef enum {
	GNC_TREE_MODEL_ACCOUNT_COL_NAME,
	GNC_TREE_MODEL_ACCOUNT_COL_TYPE,
	GNC_TREE_MODEL_ACCOUNT_COL_COMMODITY,	
	GNC_TREE_MODEL_ACCOUNT_COL_CODE,
	GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION,
	GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM,
	GNC_TREE_MODEL_ACCOUNT_COL_PRESENT,
	GNC_TREE_MODEL_ACCOUNT_COL_PRESENT_REPORT,
	GNC_TREE_MODEL_ACCOUNT_COL_BALANCE,
	GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_REPORT,
	GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_PERIOD,
	GNC_TREE_MODEL_ACCOUNT_COL_CLEARED,
	GNC_TREE_MODEL_ACCOUNT_COL_CLEARED_REPORT,
	GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED,
	GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED_REPORT,
	GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN,
	GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN_REPORT,
	GNC_TREE_MODEL_ACCOUNT_COL_TOTAL,
	GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_REPORT,
	GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_PERIOD,
	GNC_TREE_MODEL_ACCOUNT_COL_NOTES,
	GNC_TREE_MODEL_ACCOUNT_COL_TAX_INFO,
	GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER,

	GNC_TREE_MODEL_ACCOUNT_COL_LAST_VISIBLE = GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER,

	/* internal hidden columns */
	GNC_TREE_MODEL_ACCOUNT_COL_COLOR_PRESENT,
	GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE,
	GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE_PERIOD,
	GNC_TREE_MODEL_ACCOUNT_COL_COLOR_CLEARED,
	GNC_TREE_MODEL_ACCOUNT_COL_COLOR_RECONCILED,
	GNC_TREE_MODEL_ACCOUNT_COL_COLOR_FUTURE_MIN,
	GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL,
	GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL_PERIOD,

	GNC_TREE_MODEL_ACCOUNT_NUM_COLUMNS
} GncTreeModelAccountColumn;

/* typedefs & structures */

/** The instance data structure for an account tree model. */
typedef struct {
	GncTreeModel gnc_tree_model;	/**< The parent object data. */
	int stamp;			/**< The state of the model. Any state
					 *   change increments this number. */
} GncTreeModelAccount;


/** The class data structure for an account tree model. */
typedef struct {
	GncTreeModelClass gnc_tree_model;/**< The parent object data. */
} GncTreeModelAccountClass;



/** Get the type of an account tree plugin.
 *
 *  @return A GType.
 */
GType gnc_tree_model_account_get_type (void);


/** @name Account Tree Model Constructors 
 @{ */

/** Create a new GtkTreeModel for manipulating gnucash accounts.
 *
 *  @param root The account group to put at the top level of the tree
 *  hierarchy. */
GtkTreeModel *gnc_tree_model_account_new (Account *root);
/** @} */


/** @name Account Tree Model Get/Set Functions 
  @{ */

/** Convert a model/iter pair to a gnucash account.  This routine should
 *  only be called from an account tree view filter function.  The
 *  model and iter values will be provided as part of the call to the
 *  filter.
 *
 *  @param model A pointer to the account tree model.
 *
 *  @param iter A gtk_tree_iter corresponding to a single account in
 *  the model.
 *
 *  @return A pointer to the corresponding account.
 */
Account *gnc_tree_model_account_get_account (GncTreeModelAccount *model,
					     GtkTreeIter *iter);


/** Convert a model/account pair into a gtk_tree_model_iter.  This
 *  routine should only be called from the file
 *  gnc-tree-view-account.c.
 *
 *  @internal
 *
 *  @param model The model that an account belongs to.
 *
 *  @param account The account to convert.
 *
 *  @param iter A pointer to an iter.  This iter will be rewritten to
 *  contain the results of the query.
 *
 *  @return TRUE if the account was found and the iter filled
 *  in. FALSE otherwise.
 */
gboolean gnc_tree_model_account_get_iter_from_account (GncTreeModelAccount *model,
						       Account *account,
						       GtkTreeIter *iter);


/** Convert a model/account pair into a gtk_tree_model_path.  This
 *  routine should only be called from the file
 *  gnc-tree-view-account.c.
 *
 *  @internal
 *
 *  @param model The model that an account belongs to.
 *
 *  @param account The account to convert.
 *
 *  @return A pointer to a path describing the account.  It is the
 *  responsibility of the caller to free this path when done.
 */
GtkTreePath *gnc_tree_model_account_get_path_from_account (GncTreeModelAccount *model,
							   Account *account);
/** @} */

G_END_DECLS

#endif /* __GNC_TREE_MODEL_ACCOUNT_H */

/** @} */
/** @} */
