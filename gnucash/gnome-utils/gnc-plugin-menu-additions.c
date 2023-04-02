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
/** @addtogroup PluginMenuAdditions Non-GAction Menu Support
    @{ */
/** @file gnc-plugin-menu-additions.c
    @brief Functions providing menu items from scheme code.
    @author Copyright (C) 2005 David Hampton <hampton@employees.org>
*/

#include <config.h>

#include <gtk/gtk.h>
#include <string.h>
#include "swig-runtime.h"
#include "guile-mappings.h"

#include "gnc-engine.h"
#include "gnc-plugin-menu-additions.h"
#include "gnc-window.h"
#include "gnc-ui.h"
#include "gnc-menu-extensions.h"
#include "gnc-gtk-utils.h"

static GObjectClass *parent_class = NULL;

static void gnc_plugin_menu_additions_class_init (GncPluginMenuAdditionsClass *klass);
static void gnc_plugin_menu_additions_init (GncPluginMenuAdditions *plugin);
static void gnc_plugin_menu_additions_finalize (GObject *object);

static void gnc_plugin_menu_additions_add_to_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);
static void gnc_plugin_menu_additions_remove_from_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);
static void gnc_plugin_menu_additions_action_new_cb (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

/* Command callbacks */

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

#define PLUGIN_ACTIONS_NAME "gnc-plugin-menu-additions-actions"

/** Private data for this plugin.  This data structure is unused. */
typedef struct GncPluginMenuAdditionsPrivate
{
    GHashTable *item_hash;
} GncPluginMenuAdditionsPrivate;

#define GNC_PLUGIN_MENU_ADDITIONS_GET_PRIVATE(o)  \
   ((GncPluginMenuAdditionsPrivate*)gnc_plugin_menu_additions_get_instance_private((GncPluginMenuAdditions*)o))


/** Per-window private data for this plugin.  This plugin is unique in
 *  that it manages its own menu items. */
typedef struct _GncPluginMenuAdditionsPerWindow
{
    /** The menu/toolbar action information associated with a specific
        window.  This plugin must maintain its own data because of the
        way the menus are currently built. */
    GncMainWindow  *window;
    GHashTable     *item_hash;
    GHashTable     *build_menu_hash;
    GMenu          *report_menu;
    GMenu          *sub_menu;
} GncPluginMenuAdditionsPerWindow;

/** An array of all of the actions provided by the account tree
 *  plugin. */
static GActionEntry gnc_plugin_actions [] =
{
    { "AdditionsAction", gnc_plugin_menu_additions_action_new_cb, "s", NULL, NULL },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_n_actions = G_N_ELEMENTS(gnc_plugin_actions);

/************************************************************
 *                  Object Implementation                   *
 ************************************************************/

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginMenuAdditions, gnc_plugin_menu_additions, GNC_TYPE_PLUGIN)

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
    plugin_class->add_to_window      = gnc_plugin_menu_additions_add_to_window;
    plugin_class->remove_from_window = gnc_plugin_menu_additions_remove_from_window;
    plugin_class->actions_name       = PLUGIN_ACTIONS_NAME;
    plugin_class->actions            = gnc_plugin_actions;
    plugin_class->n_actions          = gnc_plugin_n_actions;
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
    GncPluginMenuAdditionsPrivate *priv;
    g_return_if_fail (GNC_IS_PLUGIN_MENU_ADDITIONS(object));

    ENTER("plugin %p", object);

    priv = GNC_PLUGIN_MENU_ADDITIONS_GET_PRIVATE(object);

    g_hash_table_destroy (priv->item_hash);

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

static void
gnc_plugin_menu_additions_action_new_cb (GSimpleAction *simple,
                                         GVariant      *parameter,
                                         gpointer       user_data)
{
    GncMainWindowActionData *data = user_data;
    GncPluginMenuAdditionsPrivate *priv;
    GncMainWindowActionData *cb_data;
    gsize length;
    const gchar *action_name;

    g_return_if_fail (G_IS_SIMPLE_ACTION(simple));

    ENTER("");
    priv = GNC_PLUGIN_MENU_ADDITIONS_GET_PRIVATE(data->data);

    action_name = g_variant_get_string (parameter, &length);

    PINFO("action name is '%s'", action_name);

    cb_data = g_hash_table_lookup (priv->item_hash, action_name);

    if (cb_data)
    {
        PINFO("Found action in table");
        gnc_extension_invoke_cb (cb_data->data, gnc_main_window_to_scm (cb_data->window));
    }
    LEAVE("");
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
    else if (a->type == GNC_SUB_MENU_ITEM)
        return -1;
    else if (b->type == GNC_SUB_MENU_ITEM)
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

    ENTER("Checking %s/%s [%s]", info->path, info->action_label, info->action_name);
    if (info->accel_assigned)
    {
        LEAVE("Already processed");
        return;
    }

    if (!g_utf8_validate(info->action_label, -1, NULL))
    {
        g_warning ("Extension menu label '%s' is not valid utf8.", info->action_label);
        info->accel_assigned = TRUE;
        LEAVE("Label is invalid utf8");
        return;
    }

    /* Was an accelerator pre-assigned in the source? */
    ptr = g_utf8_strchr (info->action_label, -1, '_');
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
    gboolean map_allocated = FALSE;

    ENTER("Checking %s/%s [%s]", info->path, info->action_label, info->action_name);
    if (info->accel_assigned)
    {
        LEAVE("Already processed");
        return;
    }

    /* Get map of used keys */
    map = g_hash_table_lookup(table, info->path);
    if (map == NULL)
    {
        map = g_strdup("");
        map_allocated = TRUE;
    }
    DEBUG("map '%s', path %s", map, info->path);

    for (ptr = info->action_label; *ptr; ptr = g_utf8_next_char(ptr))
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
        if (map_allocated)
        {
            g_free(map);
        }
        LEAVE("All characters already assigned");
        return;
    }

    /* Now build a new string in the form "<start>_<end>". */
    start = g_strndup (info->action_label, ptr - info->action_label);
    DEBUG("start %p, len %ld, text '%s'", start, g_utf8_strlen(start, -1), start);
    new_label = g_strconcat(start, "_", ptr, (gchar *)NULL);
    g_free(start);
    DEBUG("label '%s' -> '%s'", info->action_label, new_label);
    g_free((gchar *)info->action_label);
    info->action_label = new_label;

    /* Now build a new map. Old one freed automatically. */
    new_map = g_strconcat(map, buf, (gchar *)NULL);
    DEBUG("map '%s' -> '%s'", map, new_map);
    g_hash_table_replace(table, info->path, new_map);

    info->accel_assigned = TRUE;
    if (map_allocated)
    {
        g_free(map);
    }
    LEAVE("assigned");
}

static GMenuItem *
setup_tooltip_for_gmenu_item (ExtensionInfo *ext_info)
{
    GMenuItem *gmenu_item = NULL;

    if (g_strcmp0 (ext_info->typeStr, "menuitem") == 0)
    {
        gmenu_item = g_menu_item_new (ext_info->action_label, NULL);
        g_menu_item_set_action_and_target_value (gmenu_item, "gnc-plugin-menu-additions-actions.AdditionsAction",
                                                 g_variant_new_string (ext_info->action_name));

        g_menu_item_set_attribute (gmenu_item, GNC_MENU_ATTRIBUTE_TOOLTIP, "s", ext_info->action_tooltip);
    }

    if (g_strcmp0 (ext_info->typeStr, "menu") == 0)
    {
        GMenuModel *sub_menu = G_MENU_MODEL(g_menu_new ());

        gmenu_item = g_menu_item_new_submenu (ext_info->action_label, sub_menu);
        g_object_set_data (G_OBJECT(gmenu_item), "sub-menu", sub_menu);
    }
    return gmenu_item;
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
    GMenuItem *item_path, *item_with_full_path;
    gchar *full_path = NULL;
    GMenuItem *gmenu_item = NULL;

    DEBUG("Adding %s/%s [%s] as [%s]", ext_info->path, ext_info->action_label,
           ext_info->action_name, ext_info->typeStr );

    cb_data = g_new0 (GncMainWindowActionData, 1);
    cb_data->window = per_window->window;
    cb_data->data = ext_info->extension;

    g_hash_table_insert (per_window->item_hash, g_strdup (ext_info->action_name), cb_data);

    if (g_str_has_suffix (ext_info->path, _("_Custom")))
        return;

    full_path = g_strconcat (ext_info->path, "/", ext_info->action_label, NULL);

    item_path = g_hash_table_lookup (per_window->build_menu_hash, ext_info->path);
    item_with_full_path = g_hash_table_lookup (per_window->build_menu_hash, full_path);

    if (!item_path && !item_with_full_path)
    {
        gmenu_item = setup_tooltip_for_gmenu_item (ext_info);

        g_menu_append_item (G_MENU(per_window->report_menu), gmenu_item);
    }

    if (item_path && !item_with_full_path)
    {
        GMenuModel *sub_menu = G_MENU_MODEL(g_object_get_data (G_OBJECT(item_path), "sub-menu"));

        gmenu_item = setup_tooltip_for_gmenu_item (ext_info);

        g_menu_append_item (G_MENU(sub_menu), gmenu_item);
    }
    g_hash_table_insert (per_window->build_menu_hash, g_strdup (full_path), gmenu_item);

    g_free (full_path);
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
    GncPluginMenuAdditionsPrivate *priv = GNC_PLUGIN_MENU_ADDITIONS_GET_PRIVATE(plugin);
    GncPluginMenuAdditionsPerWindow per_window;
    static GOnce accel_table_init = G_ONCE_INIT;
    static GHashTable *table;
    GSList *menu_list;
    GMenuModel *menubar_model = gnc_main_window_get_menu_model (window);
    GncMenuModelSearch *gsm = g_new0 (GncMenuModelSearch, 1);

    ENTER(" ");

    per_window.window = window;
    per_window.item_hash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);

    per_window.build_menu_hash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    per_window.report_menu = g_menu_new ();

    menu_list = g_slist_sort (gnc_extensions_get_menu_list(),
                              (GCompareFunc)gnc_menu_additions_sort);

    /* Assign accelerators */
    table = g_once (&accel_table_init, gnc_menu_additions_init_accel_table, NULL);
    g_slist_foreach (menu_list,
                    (GFunc)gnc_menu_additions_do_preassigned_accel, table);
    g_slist_foreach (menu_list, (GFunc)gnc_menu_additions_assign_accel, table);

    /* Add to window. */
    g_slist_foreach (menu_list, (GFunc)gnc_menu_additions_menu_setup_one,
                     &per_window);

    priv->item_hash = per_window.item_hash;

    // add the report menu to the window
    gsm->search_action_label = NULL;
    gsm->search_action_name = "ReportsPlaceholder0";
    gsm->search_action_target = NULL;

    if (gnc_menubar_model_find_item (menubar_model, gsm))
    {
        g_menu_insert_section (G_MENU(gsm->model), gsm->index, NULL, G_MENU_MODEL(per_window.report_menu));
    }
    else
        PERR("Could not find 'ReportsAction' in menu model");

    g_hash_table_destroy (per_window.build_menu_hash);

    g_slist_free (menu_list);

    g_free (gsm);

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
    GSimpleActionGroup *simple_action_group;

    ENTER(" ");

    /* Have to remove our actions manually. Its only automatic if the
     * actions name is installed into the plugin class. */
    simple_action_group = gnc_main_window_get_action_group (window, PLUGIN_ACTIONS_NAME);

    if (simple_action_group && !window->just_plugin_prefs)
        gtk_widget_insert_action_group (GTK_WIDGET(window), PLUGIN_ACTIONS_NAME, NULL);

    LEAVE(" ");
}

/** @} */
/** @} */
