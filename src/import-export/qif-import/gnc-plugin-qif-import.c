/*
 * gnc-plugin-qif-import.c --
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
#include <glib/gi18n.h>

#include "dialog-preferences.h"
#include "druid-qif-import.h"
#include "gnc-druid-test.h"
#include "gnc-plugin-manager.h"
#include "gnc-plugin-qif-import.h"

static void gnc_plugin_qif_import_class_init (GncPluginQifImportClass *klass);
static void gnc_plugin_qif_import_init (GncPluginQifImport *plugin);
static void gnc_plugin_qif_import_finalize (GObject *object);

/* Command callbacks */
static void gnc_plugin_qif_import_cmd_new_qif_import (GtkAction *action, GncMainWindowActionData *data);
/* static void gnc_plugin_qif_test_druid (GtkAction *action, GncMainWindowActionData *data); */


#define PLUGIN_ACTIONS_NAME "gnc-plugin-qif-import-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-qif-import-ui.xml"

static GtkActionEntry gnc_plugin_actions [] =
{
    {
        "QIFImportAction", GTK_STOCK_CONVERT, N_("Import _QIF..."), NULL,
        N_("Import a Quicken QIF file"),
        G_CALLBACK (gnc_plugin_qif_import_cmd_new_qif_import)
    },
    /*
    	{ "QIFTestDruid", GTK_STOCK_CONVERT, "_Test Druid...", NULL,
    	  "Test the new Druid", G_CALLBACK(gnc_plugin_qif_test_druid) },
    */
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);

typedef struct GncPluginQifImportPrivate
{
    gpointer dummy;
} GncPluginQifImportPrivate;

#define GNC_PLUGIN_QIF_IMPORT_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_QIF_IMPORT, GncPluginQifImportPrivate))

static GObjectClass *parent_class = NULL;

GType
gnc_plugin_qif_import_get_type (void)
{
    static GType gnc_plugin_qif_import_type = 0;

    if (gnc_plugin_qif_import_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncPluginQifImportClass),
            NULL,		/* base_init */
            NULL,		/* base_finalize */
            (GClassInitFunc) gnc_plugin_qif_import_class_init,
            NULL,		/* class_finalize */
            NULL,		/* class_data */
            sizeof (GncPluginQifImport),
            0,		/* n_preallocs */
            (GInstanceInitFunc) gnc_plugin_qif_import_init,
        };

        gnc_plugin_qif_import_type = g_type_register_static (GNC_TYPE_PLUGIN,
                                     "GncPluginQifImport",
                                     &our_info, 0);
    }

    return gnc_plugin_qif_import_type;
}

GncPlugin *
gnc_plugin_qif_import_new (void)
{
    return GNC_PLUGIN (g_object_new (GNC_TYPE_PLUGIN_QIF_IMPORT, NULL));
}

static void
gnc_plugin_qif_import_class_init (GncPluginQifImportClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_qif_import_finalize;

    /* plugin info */
    plugin_class->plugin_name  = GNC_PLUGIN_QIF_IMPORT_NAME;

    /* widget addition/removal */
    plugin_class->actions_name = PLUGIN_ACTIONS_NAME;
    plugin_class->actions      = gnc_plugin_actions;
    plugin_class->n_actions    = gnc_plugin_n_actions;
    plugin_class->ui_filename  = PLUGIN_UI_FILENAME;

    g_type_class_add_private(klass, sizeof(GncPluginQifImportPrivate));
}

static void
gnc_plugin_qif_import_init (GncPluginQifImport *plugin)
{
}

static void
gnc_plugin_qif_import_finalize (GObject *object)
{
    GncPluginQifImport *plugin;
    GncPluginQifImportPrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_QIF_IMPORT (object));

    plugin = GNC_PLUGIN_QIF_IMPORT (object);
    priv = GNC_PLUGIN_QIF_IMPORT_GET_PRIVATE(plugin);

    G_OBJECT_CLASS (parent_class)->finalize (object);
}

/************************************************************
 *              Plugin Function Implementation              *
 ************************************************************/

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_qif_import_cmd_new_qif_import (GtkAction *action,
        GncMainWindowActionData *data)
{
    gnc_ui_qif_import_druid_make ();
}

/*
static void
gnc_plugin_qif_test_druid (GtkAction *action, GncMainWindowActionData *data)
{
	gnc_druid_gnome_test();
}
*/

/************************************************************
 *                    Plugin Bootstrapping                   *
 ************************************************************/

void
gnc_plugin_qif_import_create_plugin (void)
{
    GncPlugin *plugin = gnc_plugin_qif_import_new ();

    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), plugin);
    gnc_preferences_add_to_page ("qif.glade", "prefs_table",
                                 _("Online Banking"));
}
