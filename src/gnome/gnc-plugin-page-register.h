/* 
 * gnc-plugin-page-register.h -- 
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#ifndef __GNC_PLUGIN_PAGE_REGISTER_H
#define __GNC_PLUGIN_PAGE_REGISTER_H

#include <gtk/gtkwindow.h>

#include "gnc-plugin-page.h"

#include "gnc-ledger-display.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_PAGE_REGISTER            (gnc_plugin_page_register_get_type ())
#define GNC_PLUGIN_PAGE_REGISTER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_PAGE_REGISTER, GncPluginPageRegister))
#define GNC_PLUGIN_PAGE_REGISTER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_PAGE_REGISTER, GncPluginPageRegisterClass))
#define GNC_IS_PLUGIN_PAGE_REGISTER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_PAGE_REGISTER))
#define GNC_IS_PLUGIN_PAGE_REGISTER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_PAGE_REGISTER))
#define GNC_PLUGIN_PAGE_REGISTER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_PAGE_REGISTER, GncPluginPageRegisterClass))

#define GNC_PLUGIN_PAGE_REGISTER_NAME "gnc-plugin-page-register"

/* typedefs & structures */
typedef struct GncPluginPageRegisterPrivate GncPluginPageRegisterPrivate;

typedef struct {
	GObject parent;

	GncPluginPageRegisterPrivate *priv;
} GncPluginPageRegister;

typedef struct {
	GObjectClass parent;
} GncPluginPageRegisterClass;

/* function prototypes */
GType          gnc_plugin_page_register_get_type (void);

GncPluginPage *gnc_plugin_page_register_new  (GNCLedgerDisplay *ld);

G_END_DECLS

#endif /* __GNC_PLUGIN_PAGE_REGISTER_H */
