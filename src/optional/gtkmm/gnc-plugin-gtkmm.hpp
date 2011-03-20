/*
 * gnc-plugin-gtkmm.h --
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
 * @addtogroup GUI
 * @{
 * @addtogroup Gtkmm
 * @{
 * @file
 * @brief Plugin registration of the Gtkmm module
 * @author Copyright (C) 2011 Christian Stimming <christian@cstimming.de>
 */

#ifndef GNC_PLUGIN_GTKMM_H
#define GNC_PLUGIN_GTKMM_H

#include <glib.h>
extern "C" {
#include "gnome-utils/gnc-plugin.h"
}

namespace gncmm
{

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_GTKMM            (gnc_plugin_gtkmm_get_type())
#define GNC_PLUGIN_GTKMM(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GNC_TYPE_PLUGIN_GTKMM, GncPluginGtkmm))
#define GNC_PLUGIN_GTKMM_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass), GNC_TYPE_PLUGIN_GTKMM, GncPluginGtkmmClass))
#define GNC_IS_PLUGIN_GTKMM(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), GNC_TYPE_PLUGIN_GTKMM))
#define GNC_IS_PLUGIN_GTKMM_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass), GNC_TYPE_PLUGIN_GTKMM))
#define GNC_PLUGIN_GTKMM_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), GNC_TYPE_PLUGIN_GTKMM, GncPluginGtkmmClass))

#define GNC_PLUGIN_GTKMM_NAME "gnc-plugin-gtkmm"

/* typedefs & structures */
struct GncPluginGtkmm
{
    GncPlugin gnc_plugin;
};

struct GncPluginGtkmmClass
{
    GncPluginClass gnc_plugin;
};

/* function prototypes */

/**
 * @return The glib runtime type of an gtkmm plugin page
 **/
GType gnc_plugin_gtkmm_get_type(void);

/**
 * @return A new GncPluginGtkmm object
 */
GncPlugin* gnc_plugin_gtkmm_new(void);

/**
 * Create a new GncPluginGtkmm object and register it.
 */
void gnc_plugin_gtkmm_create_plugin(void);

G_END_DECLS

} // END namespace gncmm

/** @} */
/** @} */

#endif /* GNC_PLUGIN_GTKMM_H */
