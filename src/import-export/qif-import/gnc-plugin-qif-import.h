/* 
 * gnc-plugin-qif-import.h -- 
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#ifndef __GNC_PLUGIN_QIF_IMPORT_H
#define __GNC_PLUGIN_QIF_IMPORT_H

#include <gtk/gtkwindow.h>

#include "gnc-plugin.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_QIF_IMPORT            (gnc_plugin_qif_import_get_type ())
#define GNC_PLUGIN_QIF_IMPORT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_QIF_IMPORT, GncPluginQifImport))
#define GNC_PLUGIN_QIF_IMPORT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_QIF_IMPORT, GncPluginQifImportClass))
#define GNC_IS_PLUGIN_QIF_IMPORT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_QIF_IMPORT))
#define GNC_IS_PLUGIN_QIF_IMPORT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_QIF_IMPORT))
#define GNC_PLUGIN_QIF_IMPORT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_QIF_IMPORT, GncPluginQifImportClass))

#define GNC_PLUGIN_QIF_IMPORT_NAME "gnc-plugin-qif-import"

/* typedefs & structures */
typedef struct GncPluginQifImportPrivate GncPluginQifImportPrivate;

typedef struct {
	GObject parent;

	GncPluginQifImportPrivate *priv;
} GncPluginQifImport;

typedef struct {
	GObjectClass parent;
} GncPluginQifImportClass;

/* function prototypes */
GType      gnc_plugin_qif_import_get_type (void);

GncPlugin *gnc_plugin_qif_import_new      (void);

G_END_DECLS

#endif /* __GNC_PLUGIN_QIF_IMPORT_H */
