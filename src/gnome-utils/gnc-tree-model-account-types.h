/* 
 * gnc-tree-model-account-types.h -- GtkTreeModel implementation to display account types in a GtkTreeView.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#ifndef __GNC_TREE_MODEL_ACCOUNT_TYPES_H
#define __GNC_TREE_MODEL_ACCOUNT_TYPES_H

#include <gtk/gtktreemodel.h>

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_MODEL_ACCOUNT_TYPES         (gnc_tree_model_account_types_get_type ())
#define GNC_TREE_MODEL_ACCOUNT_TYPES(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_TREE_MODEL_ACCOUNT_TYPES, GncTreeModelAccountTypes))
#define GNC_TREE_MODEL_ACCOUNT_TYPES_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_TREE_MODEL_ACCOUNT_TYPES, GncTreeModelAccountTypesClass))
#define GNC_IS_TREE_MODEL_ACCOUNT_TYPES(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_TREE_MODEL_ACCOUNT_TYPES))
#define GNC_IS_TREE_MODEL_ACCOUNT_TYPES_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_TREE_MODEL_ACCOUNT_TYPES))
#define GNC_TREE_MODEL_ACCOUNT_TYPES_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_TREE_MODEL_ACCOUNT_TYPES, GncTreeModelAccountTypesClass))

typedef enum {
	GNC_TREE_MODEL_ACCOUNT_TYPES_COL_TYPE,
	GNC_TREE_MODEL_ACCOUNT_TYPES_COL_NAME,
	GNC_TREE_MODEL_ACCOUNT_TYPES_COL_SELECTED,
	GNC_TREE_MODEL_ACCOUNT_TYPES_NUM_COLUMNS
} GncTreeModelAccountTypesColumn;

/* typedefs & structures */
typedef struct GncTreeModelAccountTypesPrivate GncTreeModelAccountTypesPrivate;

typedef struct {
	GObject parent;

	GncTreeModelAccountTypesPrivate *priv;

	int stamp;
} GncTreeModelAccountTypes;

typedef struct {
	GObjectClass parent;
} GncTreeModelAccountTypesClass;

/* function prototypes */
GType           gnc_tree_model_account_types_get_type     (void);

GtkTreeModel   *gnc_tree_model_account_types_new          (guint32 selected);

guint32		gnc_tree_model_account_types_get_selected (GncTreeModelAccountTypes *model);
void		gnc_tree_model_account_types_set_selected (GncTreeModelAccountTypes *model,
							   guint32 selected);


G_END_DECLS

#endif /* __GNC_TREE_MODEL_ACCOUNT_H */
