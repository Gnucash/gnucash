/* 
 * gnc-tree-model-account.h -- GtkTreeModel implementation to display accounts in a GtkTreeView.
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
#define GNC_TREE_MODEL_EXAMPLE_ACCOUNT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_MODEL_EXAMPLE_ACCOUNT, GNCTreeModelExampleAccount))
#define GNC_TREE_MODEL_EXAMPLE_ACCOUNT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_MODEL_EXAMPLE_ACCOUNT, GNCTreeModelExampleAccountClass))
#define GNC_IS_TREE_MODEL_EXAMPLE_ACCOUNT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_MODEL_EXAMPLE_ACCOUNT))
#define GNC_IS_TREE_MODEL_EXAMPLE_ACCOUNT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_MODEL_EXAMPLE_ACCOUNT))
#define GNC_TREE_MODEL_EXAMPLE_ACCOUNT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_MODEL_EXAMPLE_ACCOUNT, GNCTreeModelExampleAccountClass))

typedef enum {
	GNC_TREE_MODEL_EXAMPLE_ACCOUNT_COL_SELECTED,
	GNC_TREE_MODEL_EXAMPLE_ACCOUNT_COL_TITLE,
	GNC_TREE_MODEL_EXAMPLE_ACCOUNT_COL_SHORT_DESCRIPTION,
	GNC_TREE_MODEL_EXAMPLE_ACCOUNT_COL_LONG_DESCRIPTION,
	GNC_TREE_MODEL_EXAMPLE_ACCOUNT_NUM_COLUMNS
} GNCTreeModelExampleAccountColumn;

/* typedefs & structures */
typedef struct GNCTreeModelExampleAccountPrivate GNCTreeModelExampleAccountPrivate;

typedef struct {
	GObject parent;

	GNCTreeModelExampleAccountPrivate *priv;

	int stamp;
} GNCTreeModelExampleAccount;

typedef struct {
	GObjectClass parent;
} GNCTreeModelExampleAccountClass;

/* function prototypes */
GType                       gnc_tree_model_example_account_get_type     (void);

GNCTreeModelExampleAccount *gnc_tree_model_example_account_new          (GSList *accounts);
void                        gnc_tree_model_example_account_set_accounts (GNCTreeModelExampleAccount *model,
                                                                         GSList *accounts);

GncExampleAccount	   *gnc_tree_model_example_account_get_account  (GNCTreeModelExampleAccount *model,
		                                                         GtkTreeIter *iter);
gboolean                    gnc_tree_model_example_account_is_selected  (GNCTreeModelExampleAccount *model,
									 GtkTreeIter *iter);
void                        gnc_tree_model_example_account_set_selected (GNCTreeModelExampleAccount *model,
		                                                         GtkTreeIter *iter,
									 gboolean selected);

G_END_DECLS

#endif /* __GNC_TREE_MODEL_EXAMPLE_ACCOUNT_H */
