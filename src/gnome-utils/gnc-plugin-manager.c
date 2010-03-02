/*
 * gnc-plugin-manager.c --
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

#include "config.h"

#include <gtk/gtk.h>

#include "gnc-plugin-manager.h"
#include "gnc-engine.h"
#include "gnc-hooks.h"

static QofLogModule log_module = GNC_MOD_GUI;

static void gnc_plugin_manager_class_init (GncPluginManagerClass *klass);
static void gnc_plugin_manager_init (GncPluginManager *plugin);
static void gnc_plugin_manager_dispose (GObject *object);
static void gnc_plugin_manager_finalize (GObject *object);
static void gnc_plugin_manager_shutdown (gpointer dummy, gpointer dummy2);

typedef struct GncPluginManagerPrivate
{
    GList *plugins;
    GHashTable *plugins_table;
}  GncPluginManagerPrivate;

#define GNC_PLUGIN_MANAGER_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_MANAGER, GncPluginManagerPrivate))

enum
{
    PLUGIN_ADDED,
    PLUGIN_REMOVED,
    LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };
static GncPluginManager *singleton = NULL;

static GObjectClass *parent_class = NULL;

GType
gnc_plugin_manager_get_type (void)
{
    static GType gnc_plugin_manager_type = 0;

    if (gnc_plugin_manager_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncPluginManagerClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_plugin_manager_class_init,
            NULL,
            NULL,
            sizeof (GncPluginManager),
            0,
            (GInstanceInitFunc) gnc_plugin_manager_init
        };

        gnc_plugin_manager_type = g_type_register_static (G_TYPE_OBJECT,
                                  "GncPluginManager",
                                  &our_info, 0);
    }

    return gnc_plugin_manager_type;
}

GncPluginManager *
gnc_plugin_manager_get (void)
{
    if (singleton == NULL)
    {
        singleton = g_object_new (GNC_TYPE_PLUGIN_MANAGER,
                                  NULL);
        gnc_hook_add_dangler (HOOK_UI_SHUTDOWN,
                              gnc_plugin_manager_shutdown, NULL);
    }

    return singleton;
}

void
gnc_plugin_manager_add_plugin (GncPluginManager *manager,
                               GncPlugin *plugin)
{
    GncPluginManagerPrivate *priv;
    gint index;

    ENTER (" ");
    g_return_if_fail (GNC_IS_PLUGIN_MANAGER (manager));
    g_return_if_fail (GNC_IS_PLUGIN (plugin));

    priv = GNC_PLUGIN_MANAGER_GET_PRIVATE(manager);
    index = g_list_index (priv->plugins, plugin);

    if (index >= 0)
        return;

    priv->plugins = g_list_append (priv->plugins, plugin);
    g_hash_table_insert (priv->plugins_table,
                         g_strdup( GNC_PLUGIN_GET_CLASS(plugin)->plugin_name ),
                         plugin);

    g_signal_emit (G_OBJECT (manager), signals[PLUGIN_ADDED], 0, plugin);
    LEAVE ("added %s to GncPluginManager", gnc_plugin_get_name(plugin));
}

void
gnc_plugin_manager_remove_plugin (GncPluginManager *manager,
                                  GncPlugin *plugin)
{
    GncPluginManagerPrivate *priv;
    gint index;

    ENTER (" ");
    g_return_if_fail (GNC_IS_PLUGIN_MANAGER (manager));
    g_return_if_fail (GNC_IS_PLUGIN (plugin));

    priv = GNC_PLUGIN_MANAGER_GET_PRIVATE(manager);
    index = g_list_index (priv->plugins, plugin);

    if (index < 0)
        return;

    priv->plugins = g_list_remove (priv->plugins, plugin);
    g_hash_table_remove (priv->plugins_table,
                         GNC_PLUGIN_GET_CLASS(plugin)->plugin_name);

    g_signal_emit (G_OBJECT (manager), signals[PLUGIN_REMOVED], 0, plugin);

    LEAVE ("removed %s from GncPluginManager",
           gnc_plugin_get_name(plugin));
    g_object_unref (plugin);
}

GList *
gnc_plugin_manager_get_plugins (GncPluginManager *manager)
{
    GncPluginManagerPrivate *priv;

    g_return_val_if_fail (GNC_IS_PLUGIN_MANAGER (manager), NULL);

    priv = GNC_PLUGIN_MANAGER_GET_PRIVATE(manager);
    return g_list_copy (priv->plugins);
}

GncPlugin *
gnc_plugin_manager_get_plugin (GncPluginManager *manager,
                               const gchar *name)
{
    GncPluginManagerPrivate *priv;

    g_return_val_if_fail (GNC_IS_PLUGIN_MANAGER (manager), NULL);
    g_return_val_if_fail (name != NULL, NULL);

    priv = GNC_PLUGIN_MANAGER_GET_PRIVATE(manager);
    return GNC_PLUGIN (g_hash_table_lookup (priv->plugins_table, name));
}


static void
gnc_plugin_manager_class_init (GncPluginManagerClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->dispose = gnc_plugin_manager_dispose;
    object_class->finalize = gnc_plugin_manager_finalize;

    g_type_class_add_private(klass, sizeof(GncPluginManagerPrivate));

    signals[PLUGIN_ADDED] = g_signal_new ("plugin-added",
                                          G_OBJECT_CLASS_TYPE (klass),
                                          G_SIGNAL_RUN_FIRST,
                                          G_STRUCT_OFFSET (GncPluginManagerClass, plugin_added),
                                          NULL, NULL,
                                          g_cclosure_marshal_VOID__OBJECT,
                                          G_TYPE_NONE,
                                          1,
                                          GNC_TYPE_PLUGIN);
    signals[PLUGIN_REMOVED] = g_signal_new ("plugin-removed",
                                            G_OBJECT_CLASS_TYPE (klass),
                                            G_SIGNAL_RUN_FIRST,
                                            G_STRUCT_OFFSET (GncPluginManagerClass, plugin_removed),
                                            NULL, NULL,
                                            g_cclosure_marshal_VOID__OBJECT,
                                            G_TYPE_NONE,
                                            1,
                                            GNC_TYPE_PLUGIN);
}

static void
gnc_plugin_manager_init (GncPluginManager *manager)
{
    GncPluginManagerPrivate *priv;

    priv = GNC_PLUGIN_MANAGER_GET_PRIVATE(manager);
    priv->plugins_table = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
}

static void
gnc_plugin_manager_dispose (GObject *object)
{
    GncPluginManager *manager = GNC_PLUGIN_MANAGER (object);
    GncPluginManagerPrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_MANAGER (manager));

    priv = GNC_PLUGIN_MANAGER_GET_PRIVATE(manager);
    if (priv->plugins_table)
    {
        g_hash_table_destroy (priv->plugins_table);
        priv->plugins_table = NULL;

        g_list_foreach (priv->plugins, (GFunc)g_object_unref, NULL);
        g_list_free (priv->plugins);
        priv->plugins = NULL;
    }

    G_OBJECT_CLASS (parent_class)->dispose (object);
}

static void
gnc_plugin_manager_finalize (GObject *object)
{
    GncPluginManager *manager;
    GncPluginManagerPrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_MANAGER (object));

    manager = GNC_PLUGIN_MANAGER (object);
    priv = GNC_PLUGIN_MANAGER_GET_PRIVATE (manager);

    G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
gnc_plugin_manager_shutdown (gpointer dummy, gpointer dummy2)
{
    if (singleton != NULL)
    {
        g_object_unref(singleton);
        singleton = NULL;
    }
}
