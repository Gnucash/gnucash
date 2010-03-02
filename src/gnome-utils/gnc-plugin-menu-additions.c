/*
 * gnc-plugin-menu-additions.c --
 * Copyright (C) 2005 David Hampton hampton@employees.org>
 *
 * From:
 * gnc-menu-extensions.c -- functions to build dynamic menus
 * Copyright (C) 1999 Rob Browning
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
/** @addtogroup PluginMenuAdditions Non-GtkAction Menu Support
    @{ */
/** @file gnc-plugin-menu-additions.c
    @brief Functions providing menu items from scheme code.
    @author Copyright (C) 2005 David Hampton <hampton@employees.org>
*/

#include "config.h"

#include <gtk/gtk.h>
#include <string.h>
#include "swig-runtime.h"

#include "guile-util.h"
#include "gnc-engine.h"
#include "gnc-main-window.h"
#include "gnc-plugin-menu-additions.h"
#include "gnc-window.h"
#include "gnc-gconf-utils.h"
#include "gnc-ui.h"
#include "gnc-menu-extensions.h"

static GObjectClass *parent_class = NULL;

static void gnc_plugin_menu_additions_class_init (GncPluginMenuAdditionsClass *klass);
static void gnc_plugin_menu_additions_init (GncPluginMenuAdditions *plugin);
static void gnc_plugin_menu_additions_finalize (GObject *object);

static void gnc_plugin_menu_additions_add_to_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);
static void gnc_plugin_menu_additions_remove_from_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);

/* Command callbacks */

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

#define PLUGIN_ACTIONS_NAME "gnc-plugin-menu-additions-actions"

/** Private data for this plugin.  This data structure is unused. */
typedef struct GncPluginMenuAdditionsPrivate
{
    gpointer dummy;
} GncPluginMenuAdditionsPrivate;

#define GNC_PLUGIN_MENU_ADDITIONS_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_MENU_ADDITIONS, GncPluginMenuAdditionsPrivate))


/** Per-window private data for this plugin.  This plugin is unique in
 *  that it manages its own menu items. */
typedef struct _GncPluginMenuAdditionsPerWindow
{
    /** The menu/toolbar action information associated with a specific
        window.  This plugin must maintain its own data because of the
        way the menus are currently built. */
    GncMainWindow  *window;
    GtkUIManager   *ui_manager;
    GtkActionGroup *group;
    gint merge_id;
} GncPluginMenuAdditionsPerWindow;

/************************************************************
 *                  Object Implementation                   *
 ************************************************************/

GType
gnc_plugin_menu_additions_get_type (void)
{
    static GType gnc_plugin_menu_additions_type = 0;

    if (gnc_plugin_menu_additions_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncPluginMenuAdditionsClass),
            NULL,		/* base_init */
            NULL,		/* base_finalize */
            (GClassInitFunc) gnc_plugin_menu_additions_class_init,
            NULL,		/* class_finalize */
            NULL,		/* class_data */
            sizeof (GncPluginMenuAdditions),
            0,
            (GInstanceInitFunc) gnc_plugin_menu_additions_init
        };

        gnc_plugin_menu_additions_type = g_type_register_static (GNC_TYPE_PLUGIN,
                                         "GncPluginMenuAdditions",
                                         &our_info, 0);
    }

    return gnc_plugin_menu_additions_type;
}

static void
gnc_plugin_menu_additions_class_init (GncPluginMenuAdditionsClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_menu_additions_finalize;

    /* plugin info */
    plugin_class->plugin_name   = GNC_PLUGIN_MENU_ADDITIONS_NAME;

    /* function overrides */
    plugin_class->add_to_window = gnc_plugin_menu_additions_add_to_window;
    plugin_class->remove_from_window = gnc_plugin_menu_additions_remove_from_window;

    g_type_class_add_private(klass, sizeof(GncPluginMenuAdditionsPrivate));
}

static void
gnc_plugin_menu_additions_init (GncPluginMenuAdditions *plugin)
{
    ENTER("plugin %p", plugin);
    LEAVE("");
}

static void
gnc_plugin_menu_additions_finalize (GObject *object)
{
    GncPluginMenuAdditions *plugin;
    GncPluginMenuAdditionsPrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_MENU_ADDITIONS (object));

    ENTER("plugin %p", object);
    plugin = GNC_PLUGIN_MENU_ADDITIONS (object);
    priv = GNC_PLUGIN_MENU_ADDITIONS_GET_PRIVATE (plugin);

    G_OBJECT_CLASS (parent_class)->finalize (object);
    LEAVE("");
}


/*  Create a new menu_additions plugin.  This plugin attaches the menu
 *  items from Scheme code to any window that is opened.
 *
 *  @return A pointer to the new object.
 */
GncPlugin *
gnc_plugin_menu_additions_new (void)
{
    GncPlugin *plugin_page = NULL;

    ENTER("");
    plugin_page = GNC_PLUGIN (g_object_new (GNC_TYPE_PLUGIN_MENU_ADDITIONS, NULL));
    LEAVE("plugin %p", plugin_page);
    return plugin_page;
}

/************************************************************
 *              Plugin Function Implementation              *
 ************************************************************/

static SCM
gnc_main_window_to_scm (GncMainWindow *window)
{
    static swig_type_info * main_window_type = NULL;

    if (!window)
        return SCM_BOOL_F;

    if (!main_window_type)
        main_window_type = SWIG_TypeQuery("_p_GncMainWindow");

    return SWIG_NewPointerObj(window, main_window_type, 0);
}


/** The user has selected one of the items added by this plugin.
 *  Invoke the callback function that was registered along with the
 *  menu item.
 *
 *  @param action A pointer to the action selected by the user.  This
 *  action represents one of the items in the file history menu.
 *
 *  @param data A pointer to the gnc-main-window data to be used by
 *  this function.  This is mainly to find out which window it was
 *  that had a menu selected.
 */
static void
gnc_plugin_menu_additions_action_cb (GtkAction *action,
                                     GncMainWindowActionData *data)
{

    g_return_if_fail(GTK_IS_ACTION(action));
    g_return_if_fail(data != NULL);

    gnc_extension_invoke_cb(data->data, gnc_main_window_to_scm(data->window));
}


/** Compare two extension menu items and indicate which should appear
 *  first in the menu listings.  If only one of them is a submenu,
 *  choose that one.  Otherwise, the sort_keys are collation keys
 *  produced by g_utf8_collate_key(), so compare them with strcmp.
 *
 *  @param a A menu extension.
 *
 *  @param b A second menu extension.
 *
 *  @return -1 if extension 'a' should appear first. 1 if extension
 *  'b' should appear first. */
static gint
gnc_menu_additions_sort (ExtensionInfo *a, ExtensionInfo *b)
{
    if (a->type == b->type)
        return strcmp(a->sort_key, b->sort_key);
    else if (a->type == GTK_UI_MANAGER_MENU)
        return -1;
    else if (b->type == GTK_UI_MANAGER_MENU)
        return 1;
    else
        return 0;
}


/** Initialize the hash table of accelerator key maps.
 *
 *  @param unused Unused.
 *
 *  @return an empty hash table. */
static gpointer
gnc_menu_additions_init_accel_table (gpointer unused)
{
    return g_hash_table_new_full(g_str_hash, g_str_equal, NULL, g_free);
}


/** Examine an extension menu item and see if it already has an
 *  accelerator key defined (in the source).  If so, add this key to
 *  the map of already used accelerator keys.  These maps are
 *  maintained per path, so accelerator keys may be duplicated across
 *  different menus but are guaranteed to be unique within any given
 *  menu.
 *
 *  @param info A menu extension.
 *
 *  @param table A hash table of accelerator maps. */
static void
gnc_menu_additions_do_preassigned_accel (ExtensionInfo *info, GHashTable *table)
{
    gchar *map, *new_map, *accel_key;
    const gchar *ptr;

    ENTER("Checking %s/%s [%s]", info->path, info->ae.label, info->ae.name);
    if (info->accel_assigned)
    {
        LEAVE("Already processed");
        return;
    }

    if (!g_utf8_validate(info->ae.label, -1, NULL))
    {
        g_warning("Extension menu label '%s' is not valid utf8.", info->ae.label);
        info->accel_assigned = TRUE;
        LEAVE("Label is invalid utf8");
        return;
    }

    /* Was an accelerator pre-assigned in the source? */
    ptr = g_utf8_strchr(info->ae.label, -1, '_');
    if (ptr == NULL)
    {
        LEAVE("not preassigned");
        return;
    }

    accel_key = g_utf8_strdown(g_utf8_next_char(ptr), 1);
    DEBUG("Accelerator preassigned: '%s'", accel_key);

    /* Now build a new map. Old one freed automatically. */
    map = g_hash_table_lookup(table, info->path);
    if (map == NULL)
        map = "";
    new_map = g_strconcat(map, accel_key, (gchar *)NULL);
    DEBUG("path '%s', map '%s' -> '%s'", info->path, map, new_map);
    g_hash_table_replace(table, info->path, new_map);

    info->accel_assigned = TRUE;
    g_free(accel_key);
    LEAVE("preassigned");
}


/** Examine an extension menu item and see if it needs to have an
 *  accelerator key assigned to it.  If so, find the first character
 *  in the menu name that isn't already assigned as an accelerator in
 *  the same menu, assign it to this item, and add it to the map of
 *  already used accelerator keys.  These maps are maintained per
 *  path, so accelerator keys may be duplicated across different menus
 *  but are guaranteed to be unique within any given menu.
 *
 *  @param info A menu extension.
 *
 *  @param table A hash table of accelerator maps. */
static void
gnc_menu_additions_assign_accel (ExtensionInfo *info, GHashTable *table)
{
    gchar *map, *new_map, *new_label, *start, buf[16];
    const gchar *ptr;
    gunichar uni;
    gint len;

    ENTER("Checking %s/%s [%s]", info->path, info->ae.label, info->ae.name);
    if (info->accel_assigned)
    {
        LEAVE("Already processed");
        return;
    }

    /* Get map of used keys */
    map = g_hash_table_lookup(table, info->path);
    if (map == NULL)
        map = g_strdup("");
    DEBUG("map '%s', path %s", map, info->path);

    for (ptr = info->ae.label; *ptr; ptr = g_utf8_next_char(ptr))
    {
        uni = g_utf8_get_char(ptr);
        if (!g_unichar_isalpha(uni))
            continue;
        uni = g_unichar_tolower(uni);
        len = g_unichar_to_utf8(uni, buf);
        buf[len] = '\0';
        DEBUG("Testing character '%s'", buf);
        if (!g_utf8_strchr(map, -1, uni))
            break;
    }

    if (ptr == NULL)
    {
        /* Ran out of characters. Nothing to do. */
        info->accel_assigned = TRUE;
        LEAVE("All characters already assigned");
        return;
    }

    /* Now build a new string in the form "<start>_<end>". */
    start = g_strndup(info->ae.label, ptr - info->ae.label);
    DEBUG("start %p, len %ld, text '%s'", start, g_utf8_strlen(start, -1), start);
    new_label = g_strconcat(start, "_", ptr, (gchar *)NULL);
    g_free(start);
    DEBUG("label '%s' -> '%s'", info->ae.label, new_label);
    g_free((gchar *)info->ae.label);
    info->ae.label = new_label;

    /* Now build a new map. Old one freed automatically. */
    new_map = g_strconcat(map, buf, (gchar *)NULL);
    DEBUG("map '%s' -> '%s'", map, new_map);
    g_hash_table_replace(table, info->path, new_map);

    info->accel_assigned = TRUE;
    LEAVE("assigned");
}


/** Add one extension item to the UI manager.  This function creates a
 *  per-callback data structure for easy access to the opaque Scheme
 *  data block in the callback.  It then adds the action to the UI
 *  manager.
 *
 *  @param ext_info The extension info data block.
 *
 *  @param per_window The per-window data block maintained by the
 *  plugin. */
static void
gnc_menu_additions_menu_setup_one (ExtensionInfo *ext_info,
                                   GncPluginMenuAdditionsPerWindow *per_window)
{
    GncMainWindowActionData *cb_data;

    DEBUG( "Adding %s/%s [%s] as [%s]", ext_info->path, ext_info->ae.label,
           ext_info->ae.name, ext_info->typeStr );

    cb_data = g_new0 (GncMainWindowActionData, 1);
    cb_data->window = per_window->window;
    cb_data->data = ext_info->extension;

    if (ext_info->type == GTK_UI_MANAGER_MENUITEM)
        ext_info->ae.callback = (GCallback)gnc_plugin_menu_additions_action_cb;

    gtk_action_group_add_actions_full(per_window->group, &ext_info->ae, 1,
                                      cb_data, g_free);
    gtk_ui_manager_add_ui(per_window->ui_manager, per_window->merge_id,
                          ext_info->path, ext_info->ae.label, ext_info->ae.name,
                          ext_info->type, FALSE);
    gtk_ui_manager_ensure_update(per_window->ui_manager);
}


/** Initialize the report menu and other additional menus.  This
 *  function is called as part of the initialization of a window, when
 *  the plugin menu items are being added to the menu structure.
 *
 *  @param plugin A pointer to the gnc-plugin object responsible for
 *  adding/removing the additional menu items.
 *
 *  @param window A pointer the gnc-main-window where this plugin
 *  should add its actions.
 *
 *  @param type Unused
 */
static void
gnc_plugin_menu_additions_add_to_window (GncPlugin *plugin,
        GncMainWindow *window,
        GQuark type)
{
    GncPluginMenuAdditionsPerWindow per_window;
    static GOnce accel_table_init = G_ONCE_INIT;
    static GHashTable *table;
    GSList *menu_list;

    ENTER(" ");

    per_window.window = window;
    per_window.ui_manager = window->ui_merge;
    per_window.group = gtk_action_group_new ("MenuAdditions" );
    gnc_gtk_action_group_set_translation_domain (per_window.group, GETTEXT_PACKAGE);
    per_window.merge_id = gtk_ui_manager_new_merge_id(window->ui_merge);
    gtk_ui_manager_insert_action_group(window->ui_merge, per_window.group, 0);

    menu_list = g_slist_sort(gnc_extensions_get_menu_list(),
                             (GCompareFunc)gnc_menu_additions_sort);

    /* Assign accelerators */
    table = g_once(&accel_table_init, gnc_menu_additions_init_accel_table, NULL);
    g_slist_foreach(menu_list,
                    (GFunc)gnc_menu_additions_do_preassigned_accel, table);
    g_slist_foreach(menu_list, (GFunc)gnc_menu_additions_assign_accel, table);

    /* Add to window. */
    g_slist_foreach(menu_list, (GFunc)gnc_menu_additions_menu_setup_one,
                    &per_window);

    /* Tell the window code about the actions that were just added
     * behind its back (so to speak) */
    gnc_main_window_manual_merge_actions (window, PLUGIN_ACTIONS_NAME,
                                          per_window.group, per_window.merge_id);

    LEAVE(" ");
}


/** Tear down the report menu and other additional menus.  This
 *  function is called as part of the cleanup of a window, while the
 *  plugin menu items are being removed from the menu structure.
 *
 *  @param plugin A pointer to the gnc-plugin object responsible for
 *  adding/removing the additional menu items.
 *
 *  @param window A pointer the gnc-main-window that is being destroyed.
 *
 *  @param type Unused
 */
static void
gnc_plugin_menu_additions_remove_from_window (GncPlugin *plugin,
        GncMainWindow *window,
        GQuark type)
{
    GtkActionGroup *group;

    ENTER(" ");

    /* Have to remove our actions manually. Its only automatic if the
     * actions name is installed into the plugin class. */
    group = gnc_main_window_get_action_group(window, PLUGIN_ACTIONS_NAME);
    if (group)
        gtk_ui_manager_remove_action_group(window->ui_merge, group);

    /* Note: This code does not clean up the per-callback data structures
     * that are created by the gnc_menu_additions_menu_setup_one()
     * function. Its not much memory and shouldn't be a problem. */

    LEAVE(" ");
}

/** @} */
/** @} */
