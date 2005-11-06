/* 
 * gnc-tree-model-example-account.h -- GtkTreeModel implementation to
 *	display examnple accounts in a GtkTreeView.
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

#ifndef __GNC_TREE_MODEL_EXAMPLE_ACCOUNT_H
#define __GNC_TREE_MODEL_EXAMPLE_ACCOUNT_H

#include <gtk/gtktreemodel.h>

#include "io-example-account.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_MODEL_EXAMPLE_ACCOUNT            (gnc_tree_model_example_account_get_type ())
#define GNC_TREE_MODEL_EXAMPLE_ACCOUNT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_MODEL_EXAMPLE_ACCOUNT, GncTreeModelExampleAccount))
#define GNC_TREE_MODEL_EXAMPLE_ACCOUNT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_MODEL_EXAMPLE_ACCOUNT, GncTreeModelExampleAccountClass))
#define GNC_IS_TREE_MODEL_EXAMPLE_ACCOUNT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_MODEL_EXAMPLE_ACCOUNT))
#define GNC_IS_TREE_MODEL_EXAMPLE_ACCOUNT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_MODEL_EXAMPLE_ACCOUNT))
#define GNC_TREE_MODEL_EXAMPLE_ACCOUNT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_MODEL_EXAMPLE_ACCOUNT, GncTreeModelExampleAccountClass))

typedef enum {
	GNC_TREE_MODEL_EXAMPLE_ACCOUNT_COL_TITLE,
	GNC_TREE_MODEL_EXAMPLE_ACCOUNT_COL_SHORT_DESCRIPTION,
	GNC_TREE_MODEL_EXAMPLE_ACCOUNT_COL_LONG_DESCRIPTION,
	GNC_TREE_MODEL_EXAMPLE_ACCOUNT_NUM_COLUMNS
} GncTreeModelExampleAccountColumn;

/* typedefs & structures */
typedef struct {
	GObject gobject;
	int stamp;
} GncTreeModelExampleAccount;

typedef struct {
	GObjectClass gobject;
} GncTreeModelExampleAccountClass;

/* function prototypes */
GType              gnc_tree_model_example_account_get_type     (void);

GtkTreeModel      *gnc_tree_model_example_account_new          (GSList *accounts);
void               gnc_tree_model_example_account_set_accounts (GncTreeModelExampleAccount *model,
                                                                GSList *accounts);

GncExampleAccount *gnc_tree_model_example_account_get_account  (GncTreeModelExampleAccount *model,
		                                                GtkTreeIter *iter);

G_END_DECLS

#endif /* __GNC_TREE_MODEL_EXAMPLE_ACCOUNT_H */
