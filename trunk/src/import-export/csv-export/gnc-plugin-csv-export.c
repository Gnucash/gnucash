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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-plugin-csv-export.h"
#include "gnc-plugin-manager.h"

#include "assistant-csv-export.h"

static void gnc_plugin_csv_export_class_init (GncPluginCsvExportClass *klass);
static void gnc_plugin_csv_export_init (GncPluginCsvExport *plugin);
static void gnc_plugin_csv_export_finalize (GObject *object);

/* Command callbacks */
static void gnc_plugin_csv_export_tree_cmd (GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_csv_export_trans_cmd (GtkAction *action, GncMainWindowActionData *data);

#define PLUGIN_ACTIONS_NAME "gnc-plugin-csv-export-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-csv-export-ui.xml"

static GtkActionEntry gnc_plugin_actions [] =
{
    {
        "CsvExportTreeAction", GTK_STOCK_CONVERT, N_("Export Account T_ree to CSV..."), NULL,
        N_("Export the Account Tree to a CSV file"),
        G_CALLBACK (gnc_plugin_csv_export_tree_cmd)
    },
    {
        "CsvExportTransAction", GTK_STOCK_CONVERT, N_("Export _Transactions to CSV..."), NULL,
        N_("Export the Transactions to a CSV file"),
        G_CALLBACK (gnc_plugin_csv_export_trans_cmd)
    },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);

typedef struct GncPluginCsvExportPrivate
{
    gpointer dummy;
} GncPluginCsvExportPrivate;

#define GNC_PLUGIN_CSV_EXPORT_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_CSV_EXPORT, GncPluginCsvExportPrivate))

static GObjectClass *parent_class = NULL;

GType
gnc_plugin_csv_export_get_type (void)
{
    static GType gnc_plugin_csv_export_type = 0;

    if (gnc_plugin_csv_export_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncPluginCsvExportClass),
            NULL,		/* base_init */
            NULL,		/* base_finalize */
            (GClassInitFunc) gnc_plugin_csv_export_class_init,
            NULL,		/* class_finalize */
            NULL,		/* class_data */
            sizeof (GncPluginCsvExport),
            0,		/* n_preallocs */
            (GInstanceInitFunc) gnc_plugin_csv_export_init,
        };

        gnc_plugin_csv_export_type = g_type_register_static (GNC_TYPE_PLUGIN,
                                     "GncPluginCsvExport",
                                     &our_info, 0);
    }

    return gnc_plugin_csv_export_type;
}

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

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_csv_export_finalize;

    /* plugin info */
    plugin_class->plugin_name  = GNC_PLUGIN_CSV_EXPORT_NAME;

    /* widget addition/removal */
    plugin_class->actions_name = PLUGIN_ACTIONS_NAME;
    plugin_class->actions      = gnc_plugin_actions;
    plugin_class->n_actions    = gnc_plugin_n_actions;
    plugin_class->ui_filename  = PLUGIN_UI_FILENAME;

    g_type_class_add_private(klass, sizeof(GncPluginCsvExportPrivate));
}

static void
gnc_plugin_csv_export_init (GncPluginCsvExport *plugin)
{
}

static void
gnc_plugin_csv_export_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN_CSV_EXPORT (object));

    G_OBJECT_CLASS (parent_class)->finalize (object);
}

/************************************************************
 *              Plugin Function Implementation              *
 ************************************************************/

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/
static void
gnc_plugin_csv_export_tree_cmd (GtkAction *action,
                                GncMainWindowActionData *data)
{
    gnc_file_csv_export(XML_EXPORT_TREE);
}

static void
gnc_plugin_csv_export_trans_cmd (GtkAction *action,
                                 GncMainWindowActionData *data)
{
    gnc_file_csv_export(XML_EXPORT_TRANS);
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
