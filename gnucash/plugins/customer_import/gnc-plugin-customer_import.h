/*
 * gnc-plugin-customer_import.h --
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
 * @file gnc-plugin-customer_import.h
 * @brief Plugin registration of the customer_import module
 * @author Copyright (C) 2009 Sebastian Held <sebastian.held@gmx.de>
 */

#ifndef GNC_PLUGIN_customer_import_H
#define GNC_PLUGIN_customer_import_H

#include <glib.h>

#include "gnc-plugin.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_customer_import            (gnc_plugin_customer_import_get_type())
#define GNC_PLUGIN_customer_import(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GNC_TYPE_PLUGIN_customer_import, GncPlugincustomer_import))
#define GNC_PLUGIN_customer_import_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GNC_TYPE_PLUGIN_customer_import, GncPlugincustomer_importClass))
#define GNC_IS_PLUGIN_customer_import(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), GNC_TYPE_PLUGIN_customer_import))
#define GNC_IS_PLUGIN_customer_import_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GNC_TYPE_PLUGIN_customer_import))
#define GNC_PLUGIN_customer_import_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GNC_TYPE_PLUGIN_customer_import, GncPlugincustomer_importClass))

#define GNC_PLUGIN_customer_import_NAME "gnc-plugin-customer_import"

/* typedefs & structures */
typedef struct
{
    GncPlugin gnc_plugin;
} GncPlugincustomer_import;

typedef struct
{
    GncPluginClass gnc_plugin;
} GncPlugincustomer_importClass;

/* function prototypes */
/**
 * @return The glib runtime type of an customer_import plugin page
 **/
GType gnc_plugin_customer_import_get_type (void);

/**
 * @return A new GncPlugincustomer_import object
 */
GncPlugin* gnc_plugin_customer_import_new (void);

/**
 * Create a new GncPlugincustomer_import object and register it.
 */
void gnc_plugin_customer_import_create_plugin (void);

G_END_DECLS

/** @} */

#endif /* GNC_PLUGIN_customer_import_H */
