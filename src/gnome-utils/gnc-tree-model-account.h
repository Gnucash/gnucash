/* 
 * gnc-tree-model-account.h -- GtkTreeModel implementation to display accounts in a GtkTreeView.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#ifndef __GNC_TREE_MODEL_ACCOUNT_H
#define __GNC_TREE_MODEL_ACCOUNT_H

#include <gtk/gtktreemodel.h>

#include "Group.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_MODEL_ACCOUNT            (gnc_tree_model_account_get_type ())
#define GNC_TREE_MODEL_ACCOUNT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_MODEL_ACCOUNT, GNCTreeModelAccount))
#define GNC_TREE_MODEL_ACCOUNT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_MODEL_ACCOUNT, GNCTreeModelAccountClass))
#define GNC_IS_TREE_MODEL_ACCOUNT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_MODEL_ACCOUNT))
#define GNC_IS_TREE_MODEL_ACCOUNT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_MODEL_ACCOUNT))
#define GNC_TREE_MODEL_ACCOUNT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_MODEL_ACCOUNT, GNCTreeModelAccountClass))

typedef enum {
	GNC_TREE_MODEL_ACCOUNT_COL_TYPE,
	GNC_TREE_MODEL_ACCOUNT_COL_NAME,
	GNC_TREE_MODEL_ACCOUNT_COL_CODE,
	GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION,
	GNC_TREE_MODEL_ACCOUNT_COL_NOTES,
	GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM,
	GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER,
	GNC_TREE_MODEL_ACCOUNT_NUM_COLUMNS
} GNCTreeModelAccountColumn;

/* typedefs & structures */
typedef struct GNCTreeModelAccountPrivate GNCTreeModelAccountPrivate;

typedef struct {
	GObject parent;

	GNCTreeModelAccountPrivate *priv;

	int stamp;
} GNCTreeModelAccount;

typedef struct {
	GObjectClass parent;
} GNCTreeModelAccountClass;

/* function prototypes */
GType                gnc_tree_model_account_get_type (void);

GNCTreeModelAccount *gnc_tree_model_account_new         (AccountGroup *group);
void                 gnc_tree_model_account_set_root    (GNCTreeModelAccount *model,
						         AccountGroup *group);

Account             *gnc_tree_model_account_get_account (GNCTreeModelAccount *model,
                                                         GtkTreeIter *iter);

G_END_DECLS

#endif /* __GNC_TREE_MODEL_ACCOUNT_H */
