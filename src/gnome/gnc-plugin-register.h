/* 
 * gnc-plugin-register.h -- 
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#ifndef __GNC_PLUGIN_REGISTER_H
#define __GNC_PLUGIN_REGISTER_H

#include <gtk/gtkwindow.h>

#include "gnc-plugin.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_REGISTER            (gnc_plugin_register_get_type ())
#define GNC_PLUGIN_REGISTER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_REGISTER, GncPluginRegister))
#define GNC_PLUGIN_REGISTER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_REGISTER, GncPluginRegisterClass))
#define GNC_IS_PLUGIN_REGISTER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_REGISTER))
#define GNC_IS_PLUGIN_REGISTER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_REGISTER))
#define GNC_PLUGIN_REGISTER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_REGISTER, GncPluginRegisterClass))

#define GNC_PLUGIN_REGISTER_NAME "gnc-plugin-register"

/* typedefs & structures */
typedef struct GncPluginRegisterPrivate GncPluginRegisterPrivate;

typedef struct {
	GObject parent;

	GncPluginRegisterPrivate *priv;
} GncPluginRegister;

typedef struct {
	GObjectClass parent;
} GncPluginRegisterClass;

/* function prototypes */
GType      gnc_plugin_register_get_type (void);

GncPlugin *gnc_plugin_register_new      (void);

G_END_DECLS

#endif /* __GNC_PLUGIN_REGISTER_H */
