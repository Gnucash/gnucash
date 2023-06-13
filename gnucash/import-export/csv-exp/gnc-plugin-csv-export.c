/*
 * gnc-plugin-csv-export.c -- csv export plugin
 * Copyright (C) 2012 Robert Fewell
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

#include "gnc-plugin-csv-export.h"
#include "gnc-plugin-manager.h"

#include "assistant-csv-export.h"

#include "gnc-plugin-page-register.h"
#include "Query.h"

static void gnc_plugin_csv_export_finalize (GObject *object);

/* Command callbacks */
static void gnc_plugin_csv_export_tree_cmd (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_csv_export_trans_cmd (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_csv_export_register_cmd (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

#define PLUGIN_ACTIONS_NAME "gnc-plugin-csv-export-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-csv-export.ui"

static GActionEntry gnc_plugin_actions [] =
{
    { "CsvExportTreeAction", gnc_plugin_csv_export_tree_cmd, NULL, NULL, NULL },
    { "CsvExportTransAction", gnc_plugin_csv_export_trans_cmd, NULL, NULL, NULL },
    { "CsvExportRegisterAction", gnc_plugin_csv_export_register_cmd, NULL, NULL, NULL },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_n_actions = G_N_ELEMENTS(gnc_plugin_actions);

/** The default menu items that need to be add to the menu */
static const gchar *gnc_plugin_load_ui_items [] =
{
    "FilePlaceholder5",
    NULL,
};

struct _GncPluginCsvExport
{
    GncPlugin gnc_plugin;
};

G_DEFINE_TYPE(GncPluginCsvExport, gnc_plugin_csv_export, GNC_TYPE_PLUGIN)

GncPlugin *
gnc_plugin_csv_export_new (void)
{
    return GNC_PLUGIN (g_object_new (GNC_TYPE_PLUGIN_CSV_EXPORT, NULL));
}

static void
gnc_plugin_csv_export_class_init (GncPluginCsvExportClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    object_class->finalize = gnc_plugin_csv_export_finalize;

    /* plugin info */
    plugin_class->plugin_name  = GNC_PLUGIN_CSV_EXPORT_NAME;

    /* widget addition/removal */
    plugin_class->actions_name    = PLUGIN_ACTIONS_NAME;
    plugin_class->actions         = gnc_plugin_actions;
    plugin_class->n_actions       = gnc_plugin_n_actions;
    plugin_class->ui_filename     = PLUGIN_UI_FILENAME;
    plugin_class->ui_updates      = gnc_plugin_load_ui_items;
}

static void
gnc_plugin_csv_export_init (GncPluginCsvExport *plugin)
{
}

static void
gnc_plugin_csv_export_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN_CSV_EXPORT (object));

    G_OBJECT_CLASS (gnc_plugin_csv_export_parent_class)->finalize (object);
}

/************************************************************
 *              Plugin Function Implementation              *
 ************************************************************/

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/
static void
gnc_plugin_csv_export_tree_cmd (GSimpleAction *simple,
                                GVariant *parameter,
                                gpointer user_data)
{
    gnc_file_csv_export(XML_EXPORT_TREE);
}

static void
gnc_plugin_csv_export_trans_cmd (GSimpleAction *simple,
                                 GVariant *parameter,
                                 gpointer user_data)
{
    gnc_file_csv_export(XML_EXPORT_TRANS);
}

static void
gnc_plugin_csv_export_register_cmd (GSimpleAction *simple,
                                    GVariant *parameter,
                                    gpointer user_data)
{
    GncMainWindowActionData *data = user_data;
    Query   *query;
    Account *acc;

    GncPluginPage *page = gnc_main_window_get_current_page (data->window);

    if (GNC_IS_PLUGIN_PAGE_REGISTER(page))
    {
        query = gnc_plugin_page_register_get_query (page);
        acc = gnc_plugin_page_register_get_account (GNC_PLUGIN_PAGE_REGISTER(page));
        gnc_file_csv_export_register (XML_EXPORT_REGISTER, query, acc);
    }
}

/************************************************************
 *                    Plugin Bootstrapping                   *
 ************************************************************/
void
gnc_plugin_csv_export_create_plugin (void)
{
    GncPlugin *plugin = gnc_plugin_csv_export_new ();

    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), plugin);
}
