/* 
 * gnc-tree-model-account.h -- GtkTreeModel implementation to
 *	display accounts in a GtkTreeView.
 *
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#ifndef __GNC_TREE_MODEL_ACCOUNT_H
#define __GNC_TREE_MODEL_ACCOUNT_H

#include <gtk/gtktreemodel.h>

#include "Group.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_MODEL_ACCOUNT            (gnc_tree_model_account_get_type ())
#define GNC_TREE_MODEL_ACCOUNT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_MODEL_ACCOUNT, GncTreeModelAccount))
#define GNC_TREE_MODEL_ACCOUNT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_MODEL_ACCOUNT, GncTreeModelAccountClass))
#define GNC_IS_TREE_MODEL_ACCOUNT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_MODEL_ACCOUNT))
#define GNC_IS_TREE_MODEL_ACCOUNT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_MODEL_ACCOUNT))
#define GNC_TREE_MODEL_ACCOUNT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_MODEL_ACCOUNT, GncTreeModelAccountClass))

typedef enum {
	GNC_TREE_MODEL_ACCOUNT_COL_NAME,
	GNC_TREE_MODEL_ACCOUNT_COL_TYPE,
	GNC_TREE_MODEL_ACCOUNT_COL_COMMODITY,	
	GNC_TREE_MODEL_ACCOUNT_COL_CODE,
	GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION,
	GNC_TREE_MODEL_ACCOUNT_COL_PRESENT,
	GNC_TREE_MODEL_ACCOUNT_COL_PRESENT_REPORT,
	GNC_TREE_MODEL_ACCOUNT_COL_BALANCE,
	GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_REPORT,
	GNC_TREE_MODEL_ACCOUNT_COL_CLEARED,
	GNC_TREE_MODEL_ACCOUNT_COL_CLEARED_REPORT,
	GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED,
	GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED_REPORT,
	GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN,
	GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN_REPORT,
	GNC_TREE_MODEL_ACCOUNT_COL_TOTAL,
	GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_REPORT,
	GNC_TREE_MODEL_ACCOUNT_COL_NOTES,
	GNC_TREE_MODEL_ACCOUNT_COL_TAX_INFO,

	GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM,
	GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER,

	/* internal hidden columns */
	GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT,
	GNC_TREE_MODEL_ACCOUNT_COL_COLOR_PRESENT,
	GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE,
	GNC_TREE_MODEL_ACCOUNT_COL_COLOR_CLEARED,
	GNC_TREE_MODEL_ACCOUNT_COL_COLOR_RECONCILED,
	GNC_TREE_MODEL_ACCOUNT_COL_COLOR_FUTURE_MIN,
	GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL,

	GNC_TREE_MODEL_ACCOUNT_NUM_COLUMNS
} GncTreeModelAccountColumn;

/* typedefs & structures */
typedef struct GncTreeModelAccountPrivate GncTreeModelAccountPrivate;

typedef struct {
	GtkObject parent;

	GncTreeModelAccountPrivate *priv;

	int stamp;
} GncTreeModelAccount;

typedef struct {
	GtkObjectClass parent;
} GncTreeModelAccountClass;

/* function prototypes */
GType         gnc_tree_model_account_get_type              (void);

GtkTreeModel *gnc_tree_model_account_new                   (AccountGroup *group);
void          gnc_tree_model_account_set_root              (GncTreeModelAccount *model,
                                                            AccountGroup *group);

Account      *gnc_tree_model_account_get_account           (GncTreeModelAccount *model,
                                                            GtkTreeIter *iter);

void          gnc_tree_model_account_set_toplevel          (GncTreeModelAccount *model,
                                                            Account *toplevel);
Account      *gnc_tree_model_account_get_toplevel          (GncTreeModelAccount *model);

GtkTreePath  *gnc_tree_model_account_get_path_from_account (GncTreeModelAccount *model,
                                                            Account *account);
void          gnc_tree_model_account_get_iter_from_account (GncTreeModelAccount *model,
                                                            Account *account,
                                                            GtkTreeIter *iter);

G_END_DECLS

#endif /* __GNC_TREE_MODEL_ACCOUNT_H */
