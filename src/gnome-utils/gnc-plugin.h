/*
 * gnc-plugin.h -- A module or plugin which can add more
 *	functionality to GnuCash.
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003,2005 David Hampton <hampton@employees.org>
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

/** @addtogroup GUI
    @{ */

/** @defgroup WindowsAndPlugin Window/Plugin Structure
 @{ */

/**
    @addtogroup Windows Windows
    @ingroup WindowsAndPlugin
*/

/**
    @addtogroup Plugins Plugins
    @ingroup WindowsAndPlugin
 @{
*/

/**
    @addtogroup MenuPlugins Menu Only Plugins
    @ingroup Plugins
*/

/**
    @addtogroup ContentPlugins Content Plugins
    @ingroup Plugins
*/

/** @} */
/** @} */
/** @} */

/** @addtogroup MenuPlugins
    @{ */
/** @addtogroup MenuPluginBase Common object and functions
    @{ */
/** @file gnc-plugin.h
    @brief Functions for adding plugins to a GnuCash window.
    @author Copyright (C) 2003 Jan Arne Petersen
    @author Copyright (C) 2003,2005 David Hampton <hampton@employees.org>

    A GncPlugin is the basic object for adding a menu item or items to
    the GnuCash user interface.  This object should be instantiated
    once at startup time and passed to the plugin manager.  Whenever a
    new window is opened, the main window code will ask the plugin
    manager for a list of all plugins, and will add each plugin to the
    new window by calling the gnc_plugin_add_to_window function.  This
    function handles installing the plugin's actions, and then calls
    the plugin to allow it to perform any plugin specific actions.
    When a main window is closed, the gnc_plugin_remove_from_window
    function is called, which first calls the plugin to perform plugin
    specific actions and then removes the plugin's actions from the
    window.
*/

#ifndef __GNC_PLUGIN_H
#define __GNC_PLUGIN_H

#include "gnc-main-window.h"
#include "gnc-plugin-page.h"
#include <gconf/gconf-client.h>

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN            (gnc_plugin_get_type ())
#define GNC_PLUGIN(o)              (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_PLUGIN, GncPlugin))
#define GNC_PLUGIN_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN, GncPluginClass))
#define GNC_IS_PLUGIN(o)           (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_PLUGIN))
#define GNC_IS_PLUGIN_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN))
#define GNC_PLUGIN_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_PLUGIN, GncPluginClass))

#define GNC_PLUGIN_NAME "GncPlugin"

/* typedefs & structures */

/** The instance data structure for a menu-only plugin. */
typedef struct
{
    /** The parent object for this widget */
    GObject gobject;
} GncPlugin;

/** The class data structure for a menu-only plugin. */
typedef struct
{
    /** The parent class for this widget. */
    GObjectClass gobject;
    /** The textual name of this plugin. */
    const gchar *plugin_name;

    /*  Actions section */

    /** A name for the set of actions that will be added by this
     *  plugin.  The actual name is irrelevant, as long as it is
     *  unique within GnuCash. */
    const gchar *actions_name;
    /** An array of actions that should automatically be added to
     *  any GnuCash "main" content window that is opened. */
    GtkActionEntry *actions;
    /** The number of actions in the actions array. */
    guint n_actions;
    /** A NULL terminated list of actions that should be considered
     *  important.  In the toolbar, these actions will display the
     *  action name when the toolbar is in "text beside icons"
     *  mode. */
    const gchar **important_actions;
    /** The relative name of the XML file describing the
     *  menu/toolbar action items. */
    const gchar *ui_filename;

    /*  GConf section */

    /** The partial section name that will be used in GConf for
     *  any preferences that are automatically stored for this
     *  page.  This will be converted to a full section name by
     *  prefixing the string "/apps/gnucash/" to whatever is
     *  here. */
    const gchar* gconf_section;
    /** A callback that will be invoked when any key in the
     *  specified GConf section is changed.
     *
     *  @param client A pointer to the gconf client instance.
     *
     *  @param cnxn_id The id number for this callback function.
     *
     *  @param entry A pointer to the changed data.
     *
     *  @param user_data A pointer to the GncWindow where the
     *  plugin is installed. */
    void (* gconf_notifications)
    (GConfClient *client, guint cnxn_id, GConfEntry *entry, gpointer user_data);

    /*  Virtual Table */

    /** A callback that will be invoked when this plugin is added
     *  to a window.  This allows the plugin to perform any
     *  special actions at insertion time.
     *
     *  @param user_data A pointer to the this GncPlugin data
     *  structure.
     *
     *  @param window A pointer to the window in which this plugin
     *  has just been installed.
     *
     *  @param type An identifier for the type of window
     *  specified.  Currently the only type is a "main" content
     *  window. */
    void (* add_to_window)
    (GncPlugin *plugin, GncMainWindow *window, GQuark type);

    /** A callback that will be invoked when this plugin is
     *  removed from a window.  This allows the plugin to perform
     *  any special actions at removal time.
     *
     *  @param user_data A pointer to the this GncPlugin data
     *  structure.
     *
     *  @param window A pointer to the window from which this
     *  plugin is about to be removed.
     *
     *  @param type An identifier for the type of window
     *  specified.  Currently the only type is a "main" content
     *  window. */
    void (* remove_from_window)
    (GncPlugin *plugin, GncMainWindow *window, GQuark type);
} GncPluginClass;

/* function prototypes */

/** Get the type of a menu-only plugin.
 *
 *  @return A GType.
 */
GType gnc_plugin_get_type (void);


/** Add the specified plugin to the specified window.  This function
 *  will add the page's user interface from the window, set up gconf
 *  notifications if the page uses gconf, and call the plugin to
 *  perform any plugin specific actions.
 *
 *  @param plugin The plugin to be added.
 *
 *  @param window Add the plugin to this window.
 *
 *  @param type An identifier for the type of window specified.
 */
void gnc_plugin_add_to_window (GncPlugin *plugin,
                               GncMainWindow *window,
                               GQuark type);


/** Remove the specified plugin from the specified window.  This
 *  function will call the plugin to perform any plugin specific
 *  actions, remove any gconf notifications that were set up for the
 *  page, and remove the page's user interface from the window.
 *
 *  @param plugin The plugin to be removed.
 *
 *  @param window The window the plugin should be removed from.
 *
 *  @param type An identifier for the type of window specified.
 */
void gnc_plugin_remove_from_window (GncPlugin *plugin,
                                    GncMainWindow *window,
                                    GQuark type);


/** Retrieve the textual name of a plugin.
 *
 *  @param plugin The plugin whose name should be returned.
 *
 *  @return A string containing the name of this plugin
 */
const gchar *gnc_plugin_get_name (GncPlugin *plugin);


/** A structure for defining alternate action names for use in the
 *  toolbar.  All toolbar buttons are homogeneous in size and are sized
 *  to fit the longest label.  Therefore, this structure should be
 *  used if an action name is more than one word.  This way the menu
 *  can have the label "Whizzy Feature", while the toolbar button only
 *  has the label "Whizzy". */
typedef struct
{
    /** The name of the action. */
    const char *action_name;
    /** The alternate toolbar label to use */
    const char *label;
} action_toolbar_labels;


/** Add "short" labels to existing actions.  The "short" label is the
 *  string used on toolbar buttons when the action is visible.  All
 *  toolbar buttons are homogeneous in size and are sized to fit the
 *  longest label.  Therefore, this structure should be used if an
 *  action name is more than one word.  This way the menu can have the
 *  label "Whizzy Feature", while the toolbar button only has the
 *  label "Whizzy".
 *
 *  @param action_group The group of all actions associated with a
 *  plugin or plugin page.  All actions to me modified must be in this
 *  group.
 *
 *  @param toolbar_labels A pointer to a NULL terminated array of data
 *  action_toolbar_labels items.
 */
void gnc_plugin_init_short_names (GtkActionGroup *action_group,
                                  action_toolbar_labels *toolbar_labels);


/** Mark certain actions as "important".  This means that their labels
 *  will appear when the toolbar is set to "Icons and important text"
 *  (e.g. GTK_TOOLBAR_BOTH_HORIZ) mode.
 *
 *  @param action_group The group of all actions associated with a
 *  plugin or plugin page.  All actions to me modified must be in this
 *  group.
 *
 *  @param name A list of actions names to be marked important.  This
 *  list must be NULL terminated.
 */
void gnc_plugin_set_important_actions (GtkActionGroup *action_group,
                                       const gchar **names);


/** Update a property on a set of existing GtkActions.  This function
 *  can be easily used to make a list of actions visible, invisible,
 *  sensitive, or insensitive.
 *
 *  @param action_group The group of all actions associated with a
 *  plugin or plugin page.  All actions to be modified must be
 *  contained in this group.
 *
 *  @param action_names A NULL terminated list of actions names that
 *  should modified.
 *
 *  @param property_name The property name to be changed on the
 *  specified actions. The only two GtkAction properties that it makes
 *  sense to modify are "visible" and "sensitive".
 *
 *  @param value A boolean specifying the new state for the specified
 *  property.
 */
void gnc_plugin_update_actions (GtkActionGroup *action_group,
                                const gchar **action_names,
                                const gchar *property_name,
                                gboolean value);


/** Load a new set of actions into an existing UI.  The actions from
 *  the provided group will be merged into the pre-existing ui, as
 *  directed by the specified file.
 *
 *  @param ui_merge A pointer to the UI manager data structure for a
 *  window.
 *
 *  @param action_group The set of actions provided by a given plugin.
 *
 *  @param filename The name of the ui description file.  This file
 *  name will be searched for in the ui directory.
 *
 *  @return The merge_id number for the newly merged UI.  If an error
 *  occurred, the return value is 0.
 */
gint gnc_plugin_add_actions (GtkUIManager *ui_merge,
                             GtkActionGroup *action_group,
                             const gchar *filename);
G_END_DECLS

#endif /* __GNC_PLUGIN_H */

/** @} */
/** @} */
