/*
 * gnc-plugin-bi-import.h -- Invoice Importer Plugin
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/**
 * @addtogroup Tools
 * @{
 * @file gnc-plugin-bi-import.h
 * @brief Plugin registration of the bi-import module
 * @author Copyright (C) 2009 Sebastian Held <sebastian.held@gmx.de>
 */

#ifndef GNC_PLUGIN_BI_IMPORT_H
#define GNC_PLUGIN_BI_IMPORT_H

#include <glib.h>

#include "gnc-plugin.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_BI_IMPORT            (gnc_plugin_bi_import_get_type())
G_DECLARE_FINAL_TYPE (GncPluginBiImport, gnc_plugin_bi_import, GNC, PLUGIN_BI_IMPORT, GncPlugin)

#define GNC_PLUGIN_BI_IMPORT_NAME "gnc-plugin-bi-import"

/* function prototypes */

/**
 * @return A new GncPluginbi_import object
 */
GncPlugin* gnc_plugin_bi_import_new (void);

/**
 * Create a new GncPluginbi_import object and register it.
 */
void gnc_plugin_bi_import_create_plugin (void);

G_END_DECLS

/** @} */

#endif /* GNC_PLUGIN_BI_IMPORT_H */
