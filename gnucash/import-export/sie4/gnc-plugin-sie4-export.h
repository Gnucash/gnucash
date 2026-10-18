/*
 * gnc-plugin-sie4-export.h -- SIE4 export plugin
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 */

#ifndef GNC_PLUGIN_SIE4_EXPORT_H
#define GNC_PLUGIN_SIE4_EXPORT_H

#include <gtk/gtk.h>

#include "gnc-plugin.h"

G_BEGIN_DECLS

#define GNC_TYPE_PLUGIN_SIE4_EXPORT (gnc_plugin_sie4_export_get_type ())
G_DECLARE_FINAL_TYPE (GncPluginSie4Export, gnc_plugin_sie4_export,
                      GNC, PLUGIN_SIE4_EXPORT, GncPlugin)

#define GNC_PLUGIN_SIE4_EXPORT_NAME "gnc-plugin-sie4-export"

GncPlugin *gnc_plugin_sie4_export_new (void);

void gnc_plugin_sie4_export_create_plugin (void);

G_END_DECLS

#endif /* GNC_PLUGIN_SIE4_EXPORT_H */
