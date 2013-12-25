/*
 * gnc-plugin-example.h --
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
 * @file gnc-plugin-example.h
 * @brief Plugin registration of the example module
 * @author Copyright (C) 2009 ?enter your name here? <your-email@example.com>
 */

#ifndef GNC_PLUGIN_example_H
#define GNC_PLUGIN_example_H

#include <glib.h>

#include "gnc-plugin.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_example            (gnc_plugin_example_get_type())
#define GNC_PLUGIN_example(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GNC_TYPE_PLUGIN_example, GncPluginexample))
#define GNC_PLUGIN_example_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GNC_TYPE_PLUGIN_example, GncPluginexampleClass))
#define GNC_IS_PLUGIN_example(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), GNC_TYPE_PLUGIN_example))
#define GNC_IS_PLUGIN_example_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GNC_TYPE_PLUGIN_example))
#define GNC_PLUGIN_example_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GNC_TYPE_PLUGIN_example, GncPluginexampleClass))

#define GNC_PLUGIN_example_NAME "gnc-plugin-example"

/* typedefs & structures */
typedef struct {
    GncPlugin gnc_plugin;
} GncPluginexample;

typedef struct {
    GncPluginClass gnc_plugin;
} GncPluginexampleClass;

/* function prototypes */
/**
 * @return The glib runtime type of an example plugin page
 **/
GType gnc_plugin_example_get_type (void);

/**
 * @return A new GncPluginexample object
 */
GncPlugin* gnc_plugin_example_new (void);

/**
 * Create a new GncPluginexample object and register it.
 */
void gnc_plugin_example_create_plugin (void);

G_END_DECLS

/** @} */

#endif /* GNC_PLUGIN_example_H */
