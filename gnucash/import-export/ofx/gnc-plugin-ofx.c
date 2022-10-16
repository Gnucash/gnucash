/*
 * gnc-plugin-ofx.c --
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-gtk-utils.h"
#include "gnc-ofx-import.h"
#include "gnc-plugin-ofx.h"
#include "gnc-plugin-manager.h"

static void gnc_plugin_ofx_class_init (GncPluginOfxClass *klass);
static void gnc_plugin_ofx_init (GncPluginOfx *plugin);
static void gnc_plugin_ofx_finalize (GObject *object);
static void gnc_plugin_ofx_add_to_window (GncPlugin *plugin,
                                          GncMainWindow *window,
                                          GQuark type);

/* Command callbacks */
static void gnc_plugin_ofx_cmd_import (GSimpleAction *simple, GVariant *parameter, gpointer user_data);


#define PLUGIN_ACTIONS_NAME "gnc-plugin-ofx-actions"

static GActionEntry gnc_plugin_actions [] =
{
    { "OfxImportAction", gnc_plugin_ofx_cmd_import, NULL, NULL, NULL },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_n_actions = G_N_ELEMENTS(gnc_plugin_actions);

static GncDisplayItem gnc_plugin_display_items [] =
{
    {
        "OfxImportAction", "go-previous", N_("Import _OFX/QFX..."), NULL,
        N_("Process an OFX/QFX response file")
    },
};
/** The number of display items provided by this plugin. */
static guint gnc_plugin_n_display_items = G_N_ELEMENTS(gnc_plugin_display_items);

typedef struct GncPluginOfxPrivate
{
    gpointer dummy;
} GncPluginOfxPrivate;

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginOfx, gnc_plugin_ofx, GNC_TYPE_PLUGIN)

#define GNC_PLUGIN_OFX_GET_PRIVATE(o)  \
   ((GncPluginOfxPrivate*)gnc_plugin_ofx_get_instance_private((GncPluginOfx*)o))

static GObjectClass *parent_class = NULL;

GncPlugin *
gnc_plugin_ofx_new (void)
{
    return GNC_PLUGIN (g_object_new (GNC_TYPE_PLUGIN_OFX, NULL));
}

static void
gnc_plugin_ofx_class_init (GncPluginOfxClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_ofx_finalize;

    /* plugin info */
    plugin_class->plugin_name  = GNC_PLUGIN_OFX_NAME;

    /* widget addition/removal */
    plugin_class->actions_name    = PLUGIN_ACTIONS_NAME;
    plugin_class->actions         = gnc_plugin_actions;
    plugin_class->n_actions       = gnc_plugin_n_actions;
    plugin_class->display_items   = gnc_plugin_display_items;
    plugin_class->n_display_items = gnc_plugin_n_display_items;
    plugin_class->add_to_window   = gnc_plugin_ofx_add_to_window;
}

static void
gnc_plugin_ofx_init (GncPluginOfx *plugin)
{
}

static GtkWidget *
add_menu_item (GtkWidget *submenu, const gchar *action_name, gboolean prepend)
{
    GtkWidget *menu_item = gtk_menu_item_new_with_label (action_name);
    gchar *full_action_name = g_strconcat (PLUGIN_ACTIONS_NAME, ".", action_name, NULL);

    if (prepend)
        gtk_menu_shell_prepend (GTK_MENU_SHELL(submenu), menu_item);
    else
        gtk_menu_shell_append (GTK_MENU_SHELL(submenu), menu_item);
    gtk_actionable_set_action_name (GTK_ACTIONABLE(menu_item), full_action_name);
    gtk_menu_item_set_use_underline (GTK_MENU_ITEM(menu_item), TRUE);

    g_object_set_data_full (G_OBJECT(menu_item), "myaction-name",
                            g_strdup (action_name), g_free);

    gtk_widget_show (menu_item);
    g_free (full_action_name);
    return menu_item;
}

static void
gnc_plugin_ofx_add_main_menus (GncMainWindow *window)
{
    GtkWidget *menu = gnc_main_window_get_menu (window);
    GtkWidget *file_import_item = gnc_find_menu_item (menu, "FileImportAction");
    GtkWidget *file_import_sub = gtk_menu_item_get_submenu (GTK_MENU_ITEM(file_import_item));

    add_menu_item (file_import_sub, "OfxImportAction", FALSE);
}

/**
 * Called when this plugin is added to a main window.  Connect a few callbacks
 * here to track page changes.
 */
static void
gnc_plugin_ofx_add_to_window (GncPlugin *plugin, GncMainWindow *window,
                               GQuark type)
{
    gnc_plugin_ofx_add_main_menus (window);
}

static void
gnc_plugin_ofx_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN_OFX (object));

    G_OBJECT_CLASS (parent_class)->finalize (object);
}

/************************************************************
 *              Plugin Function Implementation              *
 ************************************************************/

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_ofx_cmd_import (GSimpleAction *simple,
                           GVariant      *parameter,
                           gpointer       user_data)
{
    GncMainWindowActionData *data = user_data;
    gnc_file_ofx_import (GTK_WINDOW (data->window));
}


/************************************************************
 *                    Plugin Bootstrapping                   *
 ************************************************************/

void
gnc_plugin_ofx_create_plugin (void)
{
    GncPlugin *plugin = gnc_plugin_ofx_new ();

    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), plugin);
}
