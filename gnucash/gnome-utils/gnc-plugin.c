/*
 * gnc-plugin.c --
 *
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

/** @addtogroup MenuPlugins
    @{ */
/** @addtogroup MenuPluginBase Common object and functions
    @{ */
/** @file gnc-plugin.c
    @brief Functions for adding plugins to a Gnucash window.
    @author Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
    @author Copyright (C) 2003,2005 David Hampton <hampton@employees.org>
*/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-plugin.h"
#include "gnc-engine.h"
#include "gnc-filepath-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-gtk-utils.h"

/** The debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;
/** A pointer to the parent class of a plugin. */
static gpointer parent_class = NULL;

static void gnc_plugin_class_init (GncPluginClass *klass);
static void gnc_plugin_init       (GncPlugin *plugin_page,
                                   void *data);
static void gnc_plugin_finalize   (GObject *object);


/** The instance private data for a menu-only plugin.  This data
 *  structure is unused. */
typedef struct GncPluginPrivate
{
    gpointer dummy;
} GncPluginPrivate;

GNC_DEFINE_TYPE_WITH_CODE(GncPlugin, gnc_plugin, G_TYPE_OBJECT,
		        G_ADD_PRIVATE(GncPlugin))

#define GNC_PLUGIN_GET_PRIVATE(o)  \
   ((GncPluginPrivate*)gnc_plugin_get_instance_private((GncPlugin*)o))

/** Initialize the class for the new gnucash plugin object.  This will
 *  set up any function pointers that override functions in the parent
 *  class, and also installs the proprieties that are unique to this
 *  class.
 *
 *  @param klass The new class structure created by the object system.
 */
static void
gnc_plugin_class_init (GncPluginClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);
    gobject_class->finalize = gnc_plugin_finalize;
}


/** Initialize a new instance of a gnucash menu-only plugin.  This
 *  function adds the object to the tracking system.
 *
 *  @param plugin_page The new object instance created by the object
 *  system.
 *
 *  @param klass A pointer to the class data structure for this
 *  object. */
static void
gnc_plugin_init (GncPlugin *plugin_page, void *data)
{
    GncPluginClass *klass = (GncPluginClass*)data;

    gnc_gobject_tracking_remember(G_OBJECT(plugin_page), \
                                  G_OBJECT_CLASS(klass));
}


/** Finalize the gnucash plugin object.  This function is called from
 *  the G_Object level to complete the destruction of the object.  It
 *  should release any memory not previously released by the destroy
 *  function (i.e. the private data structure), then chain up to the
 *  parent's destroy function.
 *
 *  @param object The object being destroyed. */
static void
gnc_plugin_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN (object));

    gnc_gobject_tracking_forget(object);
    G_OBJECT_CLASS (parent_class)->finalize (object);
}


/** Add the specified plugin from the specified window.  This function
 *  will add the page's user interface from the window and call the
 *  plugin to perform any plugin specific actions.
 *
 *  See gnc-plugin.h for documentation on the function arguments. */
void
gnc_plugin_add_to_window (GncPlugin *plugin,
                          GncMainWindow *window,
                          GQuark type)
{
    GncPluginClass *klass;
    GSimpleActionGroup *simple_action_group;

    g_return_if_fail (GNC_IS_PLUGIN (plugin));
    klass = GNC_PLUGIN_GET_CLASS (plugin);
    ENTER (": plugin %s(%p), window %p", gnc_plugin_get_name(plugin),
           plugin, window);

    /*
     * Update window with additional UI items
     */
    if (klass->actions_name)
    {
        DEBUG ("%s: %d actions to merge",
               klass->actions_name, klass->n_actions);
        gnc_main_window_merge_actions (window, klass->actions_name,
                                       klass->actions, klass->n_actions,
                                       klass->display_items, klass->n_display_items,
                                       plugin);
    }

    /*
     * Do plugin specific actions.
     */
    if (GNC_PLUGIN_GET_CLASS (plugin)->add_to_window)
    {
        DEBUG ("Calling child class function %p", GNC_PLUGIN_GET_CLASS (plugin)->add_to_window);
        GNC_PLUGIN_GET_CLASS (plugin)->add_to_window (plugin, window, type);
    }
    LEAVE ("");
}


/*  Remove the specified plugin from the specified window.  This
 *  function will call the plugin to perform any plugin specific
 *  actions and remove the page's user interface from the window.
 *
 *  See gnc-plugin.h for documentation on the function arguments. */
void
gnc_plugin_remove_from_window (GncPlugin *plugin,
                               GncMainWindow *window,
                               GQuark type)
{
    GncPluginClass *klass;

    g_return_if_fail (GNC_IS_PLUGIN (plugin));
    klass = GNC_PLUGIN_GET_CLASS (plugin);
    ENTER (": plugin %s(%p), window %p", gnc_plugin_get_name(plugin),
           plugin, window);

    /*
     * Do plugin specific actions.
     */
    if (GNC_PLUGIN_GET_CLASS (plugin)->remove_from_window)
    {
        DEBUG ("Calling child class function %p",
               GNC_PLUGIN_GET_CLASS (plugin)->remove_from_window);
        GNC_PLUGIN_GET_CLASS (plugin)->remove_from_window (plugin, window, type);
    }

    /*
     * Update window to remove UI items
     */
    if (klass->actions_name && !window->just_plugin_prefs)
    {
        DEBUG ("%s: %d actions to unmerge",
               klass->actions_name, (klass->n_actions));
        gnc_main_window_unmerge_actions (window, klass->actions_name);
    }
    LEAVE ("");
}


/** Retrieve the textual name of a plugin.
 */
const gchar *
gnc_plugin_get_name (GncPlugin *plugin)
{
    g_return_val_if_fail (GNC_IS_PLUGIN (plugin), NULL);
    return (GNC_PLUGIN_GET_CLASS(plugin)->plugin_name);
}


/************************************************************
 *                    Utility Functions                     *
 ************************************************************/


/** Add "short" labels to existing actions.  The "short" label is the
 *  string used on toolbar buttons when the action is visible.
 *
 *  See gnc-plugin.h for documentation on the function arguments. */
void
gnc_plugin_init_short_names (GHashTable *display_item_hash,
                             GncToolBarShortNames *toolbar_labels)
{
    g_return_if_fail (display_item_hash != NULL);
    g_return_if_fail (toolbar_labels != NULL);

    for (gint i = 0; toolbar_labels[i].action_name; i++)
    {
        GncDisplayItem *gdi;

        PINFO("action_name is '%s', short_label is '%s'",
              toolbar_labels[i].action_name, toolbar_labels[i].short_label);

        gdi = (GncDisplayItem*)g_hash_table_lookup (display_item_hash,
                                                    toolbar_labels[i].action_name);
        if (gdi)
            gdi->short_label = toolbar_labels[i].short_label;
    }
}


/*  Update the sensitivity of an action */
void
gnc_plugin_set_actions_enabled (GSimpleActionGroup *simple_action_group,
                                const gchar **action_names, gboolean enable)
{
    g_return_if_fail (simple_action_group != NULL);

    for (gint i = 0; action_names[i]; i++)
    {
        GAction *action = g_action_map_lookup_action (G_ACTION_MAP(simple_action_group),
                                                      action_names[i]);
        if (action)
             g_simple_action_set_enabled (G_SIMPLE_ACTION(action), enable);
        else
            PERR("No such action with name '%s' in action group %p)",
                  action_names[i], simple_action_group);
    }
}


void
gnc_plugin_update_action_labels (GtkWidget *menubar, GtkWidget *toolbar,
                                 const GncActionUpdate *updates,
                                 gint n_updates, GtkWidget *statusbar)
{
    g_return_if_fail (menubar != NULL);
    g_return_if_fail (toolbar != NULL);
    g_return_if_fail (updates != NULL || n_updates == 0);

    for (gint i = 0; i < n_updates; i++)
    {
        const GncActionUpdate *update = &updates[i];

        GtkWidget *menu_item = gnc_find_menu_item (menubar, update->action_name);
        GtkWidget *tool_item = gnc_find_toolbar_item (toolbar, update->action_name);

        PINFO("Found menu item %p, tool_item %p, for action_name '%s', vis %d",
               menu_item, tool_item, update->action_name, update->visible);

        if (update->visible)
        {
            if (menu_item && GTK_IS_ACTIONABLE(menu_item) && (update->action_label))
            {
                gtk_actionable_set_action_name (GTK_ACTIONABLE(menu_item), NULL);

                gtk_menu_item_set_label (GTK_MENU_ITEM(menu_item), _(update->action_label));
                gtk_menu_item_set_use_underline (GTK_MENU_ITEM(menu_item), TRUE);

                if (update->action_tooltip)
                {
                    gtk_widget_set_tooltip_text (GTK_WIDGET(menu_item), _(update->action_tooltip));
                    gnc_menu_item_setup_tooltip_to_statusbar_callback (GTK_WIDGET(menu_item), statusbar);
                }
                PINFO("Menu item %p, label '%s', tooltip '%s'",
                       menu_item, update->action_label, update->action_tooltip);

                gtk_widget_set_visible (GTK_WIDGET(menu_item), TRUE);
            }

            if (tool_item && GTK_IS_ACTIONABLE(tool_item))
            {
                gboolean use_short_names = GPOINTER_TO_INT(g_object_get_data (G_OBJECT(tool_item), "use-short"));
                if (!use_short_names)
                {
                    gtk_tool_button_set_label (GTK_TOOL_BUTTON(tool_item), _(update->action_label));
                    gtk_tool_button_set_use_underline (GTK_TOOL_BUTTON(tool_item), TRUE);
                }

                if (update->action_tooltip)
                {
                    gtk_tool_item_set_tooltip_text (GTK_TOOL_ITEM(tool_item), _(update->action_tooltip));
                    gnc_tool_item_setup_tooltip_to_statusbar_callback (GTK_WIDGET(tool_item), statusbar);
                }
                PINFO("Tool item %p, label '%s', tooltip '%s', use_short %d",
                       menu_item, update->action_label, update->action_tooltip, use_short_names);

                gtk_widget_set_visible (GTK_WIDGET(tool_item), TRUE);
            }
        }
        else
        {
            if (menu_item)
                gtk_widget_set_visible (GTK_WIDGET(menu_item), FALSE);
            if (tool_item)
                gtk_widget_set_visible (GTK_WIDGET(tool_item), FALSE);
        }
    }
}


void
gnc_plugin_update_display_menu_items (GHashTable *display_item_hash,
                                      GtkWidget *menubar,
                                      GtkWidget *statusbar)
{
    GList *display_list;

    g_return_if_fail (display_item_hash != NULL);
    g_return_if_fail (menubar != NULL);

    ENTER("display_item_hash %p, menubar %p", display_item_hash, menubar);

    display_list = gnc_menu_get_items (menubar);

    for (GList *ptr = display_list; ptr; ptr = g_list_next (ptr))
    {
        GncDisplayItem *gdi;
        GtkWidget *item = GTK_WIDGET(ptr->data);
        const gchar *action_name = (const gchar*)g_object_get_data (G_OBJECT(item), "myaction-name");

        PINFO("action_name is '%s'", action_name);

        if (!action_name)
            continue;

        gdi = (GncDisplayItem*)g_hash_table_lookup (display_item_hash, action_name);

        if (!gdi)
            continue;

        PINFO("action_name is '%s', label '%s'", action_name, _(gdi->label));

        gtk_menu_item_set_label (GTK_MENU_ITEM(item), _(gdi->label));
        gtk_menu_item_set_use_underline (GTK_MENU_ITEM(item), TRUE);

        if (gdi->accelerator)
        {
            GtkWidget *child = gtk_bin_get_child (GTK_BIN (item));
            guint  accel_key = 0;
            GdkModifierType accel_mods;

            gtk_accelerator_parse (gdi->accelerator, &accel_key, &accel_mods);

            if (accel_key > 0)
                gtk_accel_label_set_accel (GTK_ACCEL_LABEL(child), accel_key, accel_mods);
        }
        if (gdi->tooltip)
        {
            gtk_widget_set_tooltip_text (item, gdi->tooltip);
            gnc_menu_item_setup_tooltip_to_statusbar_callback (item, statusbar);
        }
    }
    g_list_free (display_list);
    LEAVE("");
}


void
gnc_plugin_update_display_toolbar_items (GHashTable *display_item_hash,
                                         GtkWidget *toolbar,
                                         GtkWidget *statusbar)
{
    g_return_if_fail (display_item_hash != NULL);
    g_return_if_fail (toolbar != NULL);

    ENTER("display_item_hash %p, toolbar %p", display_item_hash, toolbar);

    for (gint i = 0; i < gtk_toolbar_get_n_items (GTK_TOOLBAR(toolbar)); i++)
    {
        GtkToolItem *item = gtk_toolbar_get_nth_item (GTK_TOOLBAR(toolbar), i);

        if (GTK_IS_ACTIONABLE(item))
        {
            const gchar *item_action_name = gtk_actionable_get_action_name (GTK_ACTIONABLE(item));
            GncDisplayItem *gdi;
            gchar *ptr;

            PINFO("item_action_name is '%s'", item_action_name);

            if (!item_action_name)
                continue;

            ptr = g_strrstr (item_action_name, ".");

            if (!ptr)
                continue;

            gdi = (GncDisplayItem*)g_hash_table_lookup (display_item_hash, ptr + 1);

            if (!gdi)
                continue;

            PINFO("item_action_name is '%s', label '%s', short '%s'",
                  item_action_name, _(gdi->label), _(gdi->short_label));

            if (gdi->short_label)
            {
                gtk_tool_button_set_label (GTK_TOOL_BUTTON(item), _(gdi->short_label));
                g_object_set_data (G_OBJECT(item), "use-short", GINT_TO_POINTER(1));
            }
            else
            {
                gtk_tool_button_set_label (GTK_TOOL_BUTTON(item), _(gdi->label));
                g_object_set_data (G_OBJECT(item), "use-short", GINT_TO_POINTER(0));
            }
            gtk_tool_button_set_use_underline (GTK_TOOL_BUTTON(item), TRUE);

            if (gdi->tooltip)
            {
                 gtk_tool_item_set_tooltip_text (GTK_TOOL_ITEM(item), _(gdi->tooltip));
                 gnc_tool_item_setup_tooltip_to_statusbar_callback (GTK_WIDGET(item), statusbar);
            }

            if (gdi->icon_name)
                gtk_tool_button_set_icon_name (GTK_TOOL_BUTTON(item), gdi->icon_name);
        }
    }
    LEAVE("");
}


void
gnc_plugin_add_to_display_hash (GHashTable *display_item_hash,
                                const GncDisplayItem *display_items,
                                gint n_display_items)
{
    g_return_if_fail (display_item_hash != NULL);
    g_return_if_fail (display_items != NULL || n_display_items == 0);

    ENTER("display_item_has %p, display_items %p, n_display_itesm %d",
          display_item_hash, display_items, n_display_items);

    for (gint i = 0; n_display_items == -1 ? display_items[i].action_name != NULL : i < n_display_items; i++)
    {
        if (!g_hash_table_lookup (display_item_hash, display_items[i].action_name))
        {
            GncDisplayItem *gdi = g_new0 (GncDisplayItem, 1);

            gdi->action_name = display_items[i].action_name;
            gdi->icon_name = display_items[i].icon_name;
            gdi->label = display_items[i].label;
            gdi->accelerator = display_items[i].accelerator;
            gdi->tooltip = display_items[i].tooltip;
            gdi->short_label = NULL;
            g_hash_table_insert (display_item_hash, g_strdup (display_items[i].action_name), gdi);
        }
    }
    LEAVE("display_item_hash size %d", g_hash_table_size (display_item_hash));
}

/** @} */
/** @} */
