/* 
 * gnc-plugin-manager.h -- Manage gnucash plugins.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#ifndef __GNC_PLUGIN_MANAGER_H
#define __GNC_PLUGIN_MANAGER_H

#include "gnc-plugin.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_MANAGER            (gnc_plugin_manager_get_type ())
#define GNC_PLUGIN_MANAGER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_MANAGER, GncPluginManager))
#define GNC_PLUGIN_MANAGER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_MANAGER, GncPluginManagerClass))
#define GNC_IS_PLUGIN_MANAGER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_MANAGER))
#define GNC_IS_PLUGIN_MANAGER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_MANAGER))
#define GNC_PLUGIN_MANAGER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_MANAGER, GncPluginManagerClass))

/* typedefs & structures */
typedef struct GncPluginManagerPrivate GncPluginManagerPrivate;

typedef struct {
	GObject parent;

	GncPluginManagerPrivate *priv;
} GncPluginManager;

typedef struct {
	GObjectClass parent;

	/* Signals */
	void (* plugin_added) (GncPluginManager *plugin_manager, GncPlugin *plugin);
	void (* plugin_removed) (GncPluginManager *plugin_manager, GncPlugin *plugin);
} GncPluginManagerClass;

/* function prototypes */
GType             gnc_plugin_manager_get_type      (void);

GncPluginManager *gnc_plugin_manager_get           (void);

void              gnc_plugin_manager_add_plugin    (GncPluginManager *manager,
						    GncPlugin *plugin);
void		  gnc_plugin_manager_remove_plugin (GncPluginManager *manager,
						    GncPlugin *plugin);

GList            *gnc_plugin_manager_get_plugins   (GncPluginManager *manager);

G_END_DECLS

#endif /* __GNC_PLUGIN_MANAGER_H */
