/* 
 * gnc-plugin_page-account-tree.h -- 
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#ifndef __GNC_PLUGIN_PAGE_ACCOUNT_TREE_H
#define __GNC_PLUGIN_PAGE_ACCOUNT_TREE_H

#include <gtk/gtkwindow.h>

#include "gnc-plugin-page.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_PAGE_ACCOUNT_TREE            (gnc_plugin_page_account_tree_get_type ())
#define GNC_PLUGIN_PAGE_ACCOUNT_TREE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_PAGE_ACCOUNT_TREE, GncPluginPageAccountTree))
#define GNC_PLUGIN_PAGE_ACCOUNT_TREE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_PAGE_ACCOUNT_TREE, GncPluginPageAccountTreeClass))
#define GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_PAGE_ACCOUNT_TREE))
#define GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_PAGE_ACCOUNT_TREE))
#define GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_PAGE_ACCOUNT_TREE, GncPluginPageAccountTreeClass))

#define GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME "gnc-plugin_page-account-tree"

/* typedefs & structures */
typedef struct GncPluginPageAccountTreePrivate GncPluginPageAccountTreePrivate;

typedef struct {
	GObject parent;

	GncPluginPageAccountTreePrivate *priv;
} GncPluginPageAccountTree;

typedef struct {
	GObjectClass parent;
} GncPluginPageAccountTreeClass;

/* function prototypes */
GType          gnc_plugin_page_account_tree_get_type (void);

GncPluginPage *gnc_plugin_page_account_tree_new  (void);

G_END_DECLS

#endif /* __GNC_PLUGIN_PAGE_ACCOUNT_TREE_H */
