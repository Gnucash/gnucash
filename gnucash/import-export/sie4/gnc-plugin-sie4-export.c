/*
 * gnc-plugin-sie4-export.c -- SIE4 export plugin
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-plugin-sie4-export.h"
#include "gnc-main-window.h"
#include "gnc-plugin-manager.h"

#include "dialog-sie4-export.h"

static void gnc_plugin_sie4_export_finalize (GObject *object);
static void gnc_plugin_sie4_export_cmd (GSimpleAction *simple,
                                        GVariant *parameter,
                                        gpointer user_data);

#define PLUGIN_ACTIONS_NAME "gnc-plugin-sie4-export-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-sie4-export.ui"

static GActionEntry gnc_plugin_actions [] =
{
    { "Sie4ExportAction", gnc_plugin_sie4_export_cmd, NULL, NULL, NULL },
};

static guint gnc_plugin_n_actions = G_N_ELEMENTS(gnc_plugin_actions);

static const gchar *gnc_plugin_load_ui_items [] =
{
    "FilePlaceholder5",
    NULL,
};

struct _GncPluginSie4Export
{
    GncPlugin gnc_plugin;
};

G_DEFINE_TYPE(GncPluginSie4Export, gnc_plugin_sie4_export, GNC_TYPE_PLUGIN)

GncPlugin *
gnc_plugin_sie4_export_new (void)
{
    return GNC_PLUGIN (g_object_new (GNC_TYPE_PLUGIN_SIE4_EXPORT, NULL));
}

static void
gnc_plugin_sie4_export_class_init (GncPluginSie4ExportClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    object_class->finalize = gnc_plugin_sie4_export_finalize;

    plugin_class->plugin_name  = GNC_PLUGIN_SIE4_EXPORT_NAME;
    plugin_class->actions_name = PLUGIN_ACTIONS_NAME;
    plugin_class->actions      = gnc_plugin_actions;
    plugin_class->n_actions    = gnc_plugin_n_actions;
    plugin_class->ui_filename  = PLUGIN_UI_FILENAME;
    plugin_class->ui_updates   = gnc_plugin_load_ui_items;
}

static void
gnc_plugin_sie4_export_init (GncPluginSie4Export *plugin)
{
}

static void
gnc_plugin_sie4_export_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN_SIE4_EXPORT (object));

    G_OBJECT_CLASS (gnc_plugin_sie4_export_parent_class)->finalize (object);
}

static void
gnc_plugin_sie4_export_cmd (GSimpleAction *simple,
                            GVariant *parameter,
                            gpointer user_data)
{
    GncMainWindowActionData *data = user_data;

    g_return_if_fail (data != NULL);

    gnc_file_sie4_export (GTK_WINDOW (data->window));
}

void
gnc_plugin_sie4_export_create_plugin (void)
{
    GncPlugin *plugin = gnc_plugin_sie4_export_new ();

    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), plugin);
}
