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

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-plugin-manager.h"
#include "gnc-engine.h"
#include "gnc-hooks.h"

static QofLogModule log_module = GNC_MOD_GUI;

static void gnc_plugin_manager_dispose (GObject *object);
static void gnc_plugin_manager_finalize (GObject *object);
static void gnc_plugin_manager_shutdown (gpointer dummy, gpointer dummy2);

/* typedefs & structures */
struct _GncPluginManager
{
    GObject gobject;

    GList *plugins;
    GHashTable *plugins_table;
}  GncPluginManagerPrivate;

G_DEFINE_TYPE(GncPluginManager, gnc_plugin_manager, G_TYPE_OBJECT)

enum
{
    PLUGIN_ADDED,
    PLUGIN_REMOVED,
    LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };
static GncPluginManager *singleton = NULL;

GncPluginManager *
gnc_plugin_manager_get (void)
{
    if (singleton == NULL)
    {
        singleton = g_object_new (GNC_TYPE_PLUGIN_MANAGER,
                                  NULL);
        gnc_hook_add_dangler (HOOK_SHUTDOWN,
                              gnc_plugin_manager_shutdown, NULL, NULL);
    }

    return singleton;
}

void
gnc_plugin_manager_add_plugin (GncPluginManager *manager,
                               GncPlugin *plugin)
{
    gint index;

    ENTER (" ");
    g_return_if_fail (GNC_IS_PLUGIN_MANAGER (manager));
    g_return_if_fail (GNC_IS_PLUGIN (plugin));

    index = g_list_index (manager->plugins, plugin);

    if (index >= 0)
        return;

    manager->plugins = g_list_append (manager->plugins, plugin);
    g_hash_table_insert (manager->plugins_table,
                         g_strdup( GNC_PLUGIN_GET_CLASS(plugin)->plugin_name ),
                         plugin);

    g_signal_emit (G_OBJECT (manager), signals[PLUGIN_ADDED], 0, plugin);
    LEAVE ("added %s to GncPluginManager", gnc_plugin_get_name(plugin));
}

void
gnc_plugin_manager_remove_plugin (GncPluginManager *manager,
                                  GncPlugin *plugin)
{
    gint index;

    ENTER (" ");
    g_return_if_fail (GNC_IS_PLUGIN_MANAGER (manager));
    g_return_if_fail (GNC_IS_PLUGIN (plugin));

    index = g_list_index (manager->plugins, plugin);

    if (index < 0)
        return;

    manager->plugins = g_list_remove (manager->plugins, plugin);
    g_hash_table_remove (manager->plugins_table,
                         GNC_PLUGIN_GET_CLASS(plugin)->plugin_name);

    g_signal_emit (G_OBJECT (manager), signals[PLUGIN_REMOVED], 0, plugin);

    LEAVE ("removed %s from GncPluginManager",
           gnc_plugin_get_name(plugin));
    g_object_unref (plugin);
}

GList *
gnc_plugin_manager_get_plugins (GncPluginManager *manager)
{
    g_return_val_if_fail (GNC_IS_PLUGIN_MANAGER (manager), NULL);

    return g_list_copy (manager->plugins);
}

GncPlugin *
gnc_plugin_manager_get_plugin (GncPluginManager *manager,
                               const gchar *name)
{
    g_return_val_if_fail (GNC_IS_PLUGIN_MANAGER (manager), NULL);
    g_return_val_if_fail (name != NULL, NULL);

    return GNC_PLUGIN (g_hash_table_lookup (manager->plugins_table, name));
}

static void
gnc_plugin_manager_class_init (GncPluginManagerClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    object_class->dispose = gnc_plugin_manager_dispose;
    object_class->finalize = gnc_plugin_manager_finalize;

    signals[PLUGIN_ADDED] = g_signal_new ("plugin-added",
                                          G_OBJECT_CLASS_TYPE (klass),
                                          G_SIGNAL_RUN_FIRST,
                                          0,
                                          NULL, NULL,
                                          g_cclosure_marshal_VOID__OBJECT,
                                          G_TYPE_NONE,
                                          1,
                                          GNC_TYPE_PLUGIN);
    signals[PLUGIN_REMOVED] = g_signal_new ("plugin-removed",
                                            G_OBJECT_CLASS_TYPE (klass),
                                            G_SIGNAL_RUN_FIRST,
                                            0,
                                            NULL, NULL,
                                            g_cclosure_marshal_VOID__OBJECT,
                                            G_TYPE_NONE,
                                            1,
                                            GNC_TYPE_PLUGIN);
}

static void
gnc_plugin_manager_init (GncPluginManager *manager)
{
    manager->plugins_table = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
}

static void
gnc_plugin_manager_dispose (GObject *object)
{
    GncPluginManager *manager = GNC_PLUGIN_MANAGER (object);

    g_return_if_fail (GNC_IS_PLUGIN_MANAGER (manager));

    if (manager->plugins_table)
    {
        g_hash_table_destroy (manager->plugins_table);
        manager->plugins_table = NULL;

        g_list_foreach (manager->plugins, (GFunc)g_object_unref, NULL);
        g_list_free (manager->plugins);
        manager->plugins = NULL;
    }

    G_OBJECT_CLASS (gnc_plugin_manager_parent_class)->dispose (object);
}

static void
gnc_plugin_manager_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN_MANAGER (object));

    G_OBJECT_CLASS (gnc_plugin_manager_parent_class)->finalize (object);
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
