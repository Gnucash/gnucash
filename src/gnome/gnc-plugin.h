/* 
 * gnc-plugin.h -- A module or plugin which can add more functionality to gnucash.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#ifndef __GNC_PLUGIN_H
#define __GNC_PLUGIN_H

#include "gnc-main-window.h"
#include "gnc-plugin-page.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN          (gnc_plugin_get_type ())
#define GNC_PLUGIN(o)            (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_PLUGIN, GncPlugin))
#define GNC_IS_PLUGIN(o)         (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_PLUGIN))
#define GNC_PLUGIN_GET_IFACE(o)  (G_TYPE_INSTANCE_GET_INTERFACE ((o), GNC_TYPE_PLUGIN, GncPluginIface))

/* typedefs & structures */
typedef struct GncPlugin GncPlugin; /* dummy typedef */

typedef struct {
	GTypeInterface parent;

	/* Virtual Table */
	void (* add_to_window) (GncPlugin *plugin, GncMainWindow *window, GQuark type);
	void (* remove_from_window) (GncPlugin *plugin, GncMainWindow *window, GQuark type);

	const gchar *(* get_name) (GncPlugin *plugin);

	GncPluginPage *(* create_page) (GncPlugin *plugin, const gchar *uri);
} GncPluginIface;

/* function prototypes */
GType                 gnc_plugin_get_type        (void);

void                  gnc_plugin_add_to_window   (GncPlugin *plugin,
					          GncMainWindow *window,
					          GQuark type);
void                  gnc_plugin_remove_from_window (GncPlugin *plugin,
					             GncMainWindow *window,
					             GQuark type);

G_CONST_RETURN gchar *gnc_plugin_get_name        (GncPlugin *plugin);

GncPluginPage        *gnc_plugin_create_page     (GncPlugin *plugin,
						  const gchar *uri);

G_END_DECLS

#endif /* __GNC_PLUGIN_H */
