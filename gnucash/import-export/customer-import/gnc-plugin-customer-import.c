/*
 * gnc-plugin-customer-import.c --
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

/**
 * @internal
 * @file gnc-plugin-customer-import.c
 * @brief Plugin registration of the customer_import plugin
 * @author Copyright (C) 2009 Sebastian Held <sebastian.held@gmx.de>
 */

#include <config.h>

//#include <glade/glade.h>
//#include <glade/glade-xml.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"

#include "gnc-plugin-customer-import.h"
#include "dialog-customer-import-gui.h"
#include "gnc-plugin-manager.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

static void gnc_plugin_customer_import_class_init         (GncPlugincustomer_importClass *klass);
static void gnc_plugin_customer_import_init               (GncPlugincustomer_import *plugin);
static void gnc_plugin_customer_import_finalize           (GObject *object);

/* Command callbacks */
static void gnc_plugin_customer_import_cmd_test (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

#define PLUGIN_ACTIONS_NAME "gnc-plugin-customer-import-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-customer-import.ui"

static GActionEntry gnc_plugin_actions [] =
{
    { "customer_importAction", gnc_plugin_customer_import_cmd_test, NULL, NULL, NULL },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_n_actions = G_N_ELEMENTS(gnc_plugin_actions);

static GncDisplayItem gnc_plugin_display_items [] =
{
    /* Menu Items */
    { "ImportMenuAction", NULL, N_("_Import"), NULL, NULL },
    /* Menu entry with label and tooltip */
    { "customer_importAction", "go-previous", N_("Import _Customers & Vendors..."),
      NULL, N_("Import Customers and Vendors from a CSV text file.")
    },
};
/** The number of display items provided by this plugin. */
static guint gnc_plugin_n_display_items = G_N_ELEMENTS(gnc_plugin_display_items);

/************************************************************
 *                   Object Implementation                  *
 ************************************************************/

G_DEFINE_TYPE(GncPlugincustomer_import, gnc_plugin_customer_import, GNC_TYPE_PLUGIN);

GncPlugin *
gnc_plugin_customer_import_new (void)
{
    return GNC_PLUGIN (g_object_new (GNC_TYPE_PLUGIN_customer_import, (gchar*) NULL));
}

static void
gnc_plugin_customer_import_class_init (GncPlugincustomer_importClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS(klass);

    object_class->finalize = gnc_plugin_customer_import_finalize;

    /* plugin info */
    plugin_class->plugin_name  = GNC_PLUGIN_customer_import_NAME;

    /* widget addition/removal */
    plugin_class->actions_name    = PLUGIN_ACTIONS_NAME;
    plugin_class->actionsb        = gnc_plugin_actions;
    plugin_class->n_actionsb      = gnc_plugin_n_actions;
    plugin_class->display_items   = gnc_plugin_display_items;
    plugin_class->n_display_items = gnc_plugin_n_display_items;
    plugin_class->ui_filename     = PLUGIN_UI_FILENAME;
}

static void
gnc_plugin_customer_import_init (GncPlugincustomer_import *plugin)
{
}

static void
gnc_plugin_customer_import_finalize (GObject *object)
{
}

/************************************************************
 *                    Plugin Bootstrapping                   *
 ************************************************************/

void
gnc_plugin_customer_import_create_plugin (void)
{
    GncPlugin *plugin = gnc_plugin_customer_import_new ();

    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), plugin);
}

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_customer_import_cmd_test (GSimpleAction *simple,
                                     GVariant      *parameter,
                                     gpointer       user_data)
{
    GncMainWindowActionData *data = user_data;

    ENTER ("action %p, main window data %p", simple, data);
    PINFO ("customer_import");

    gnc_plugin_customer_import_showGUI (GTK_WINDOW(data->window));

    LEAVE (" ");
}
