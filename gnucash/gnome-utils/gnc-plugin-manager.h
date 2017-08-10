/*
 * gnc-plugin-manager.h -- Manage gnucash plugins.
 *
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
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

/** @addtogroup Plugins
    @{ */
/** @addtogroup PluginManager Plugin Management Functions
    @{ */
/** @file gnc-plugin-manager.h
    @brief  Plugin management functions for the GnuCash UI
    @author Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>

    The plugin manager maintains a list of all non-content plugins
    that have been instantiated by various parts of Gnucash.  The
    manager will provide this list upon request, it will also look up
    individual plugins on request.  The main client of this manager is
    the main gnucash window code.  All plugins registered here will
    automatically be installed in each top level gnucash window that
    is created.

    This code installs a hook to be called when the gnucash user
    interface shuts down, and at that time it will unref any plugins
    that are still in its plugin list.

    Note: This code maintains a list of plugins (which provide user
    interface items), not a list of plugin-pages (which provide window
    content).
*/

#ifndef __GNC_PLUGIN_MANAGER_H
#define __GNC_PLUGIN_MANAGER_H

#include "gnc-plugin.h"

G_BEGIN_DECLS

/** @name Basic Object Implementation */
/** @{ */

/* type macros */
#define GNC_TYPE_PLUGIN_MANAGER            (gnc_plugin_manager_get_type ())
#define GNC_PLUGIN_MANAGER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_MANAGER, GncPluginManager))
#define GNC_PLUGIN_MANAGER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_MANAGER, GncPluginManagerClass))
#define GNC_IS_PLUGIN_MANAGER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_MANAGER))
#define GNC_IS_PLUGIN_MANAGER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_MANAGER))
#define GNC_PLUGIN_MANAGER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_MANAGER, GncPluginManagerClass))

/* typedefs & structures */
typedef struct
{
    GObject gobject;
} GncPluginManager;

typedef struct
{
    GObjectClass gobject;

    /* Signals */
    void (* plugin_added)
    (GncPluginManager *plugin_manager, GncPlugin *plugin);
    void (* plugin_removed)
    (GncPluginManager *plugin_manager, GncPlugin *plugin);
} GncPluginManagerClass;

/** Retrieve the GType value for the gnucash plugin manager.
 *
 *  @return The GType that corresponds to an object of this type.
 */
GType gnc_plugin_manager_get_type (void);

/** @} */



/** @name Management Functions */
/** @{ */

/** Retrieve a pointer to the plugin manager.  This object is a
 *  singleton, that can only be retrieved via this function.  Once you
 *  have a pointer to the manager, you can call it to add/remove
 *  plugins, etc.
 *
 *  @return A pointer to the plugin manager object.
 */
GncPluginManager *gnc_plugin_manager_get (void);


/** Add a plugin to the list maintained by the plugin manager.
 *
 *  @param manager A pointer to the plugin manager.  Retrieve this by
 *  calling gnc_plugin_manager_get().
 *
 *  @param plugin A pointer to the plugin to add.
 *
 *  @note This function assumes ownership of this plugin.  Do not unref
 *  the plugin after passing it off to the plugin manager.
 */
void gnc_plugin_manager_add_plugin (GncPluginManager *manager,
                                    GncPlugin *plugin);


/** Remove a plugin from the list maintained by the plugin manager.
 *
 *  @param manager A pointer to the plugin manager.  Retrieve this by
 *  calling gnc_plugin_manager_get().
 *
 *  @param plugin A pointer to the plugin to add.
 */
void gnc_plugin_manager_remove_plugin (GncPluginManager *manager,
                                       GncPlugin *plugin);


/** Get a list of all plugins being held by the plugin manager.  This
 *  function is used by the main gnucash window code to get the list
 *  of plugins that need to be added to a new top level window.
 *
 *  @param manager A pointer to the plugin manager.  Retrieve this by
 *  calling gnc_plugin_manager_get().
 *
 *  @return A list of plugins.  This list is owned by the caller, and
 *  the must be frees when the caller is finished with it.
 */
GList *gnc_plugin_manager_get_plugins (GncPluginManager *manager);


/** Find a plugin by name from the list of plugins being held by the
 * plugin manager.
 *
 *  @param manager A pointer to the plugin manager.  Retrieve this by
 *  calling gnc_plugin_manager_get().
 *
 *  @param name The name of the plugin to find.
 *
 *  @return A pointer to the requested plugin, or NULL if the plugin
 *  couldn't be found.
 */
GncPlugin *gnc_plugin_manager_get_plugin (GncPluginManager *manager,
        const gchar *name);
/** @} */


G_END_DECLS

#endif /* __GNC_PLUGIN_MANAGER_H */

/** @} */
/** @} */
