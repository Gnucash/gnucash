/* 
 * gnc-tree-model-example-account.h -- GtkTreeModel implementation to display examnple accounts in a GtkTreeView.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
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
typedef struct GncTreeModelExampleAccountPrivate GncTreeModelExampleAccountPrivate;

typedef struct {
	GObject parent;

	GncTreeModelExampleAccountPrivate *priv;

	int stamp;
} GncTreeModelExampleAccount;

typedef struct {
	GObjectClass parent;
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
