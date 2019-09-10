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

#include "gnc-ofx-import.h"
#include "gnc-plugin-ofx.h"
#include "gnc-plugin-manager.h"

static void gnc_plugin_ofx_class_init (GncPluginOfxClass *klass);
static void gnc_plugin_ofx_init (GncPluginOfx *plugin);
static void gnc_plugin_ofx_finalize (GObject *object);

/* Command callbacks */
static void gnc_plugin_ofx_cmd_import (GtkAction *action, GncMainWindowActionData *data);


#define PLUGIN_ACTIONS_NAME "gnc-plugin-ofx-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-ofx-ui.xml"

static GtkActionEntry gnc_plugin_actions [] =
{
    {
        "OfxImportAction", "go-previous", N_("Import _OFX/QFX..."), NULL,
        N_("Process an OFX/QFX response file"),
        G_CALLBACK (gnc_plugin_ofx_cmd_import)
    },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);

typedef struct GncPluginOfxPrivate
{
    gpointer dummy;
} GncPluginOfxPrivate;

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginOfx, gnc_plugin_ofx, GNC_TYPE_PLUGIN)

#define GNC_PLUGIN_OFX_GET_PRIVATE(o)  \
   ((GncPluginOfxPrivate*)g_type_instance_get_private((GTypeInstance*)o, GNC_TYPE_PLUGIN_OFX))

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
    plugin_class->actions_name = PLUGIN_ACTIONS_NAME;
    plugin_class->actions      = gnc_plugin_actions;
    plugin_class->n_actions    = gnc_plugin_n_actions;
    plugin_class->ui_filename  = PLUGIN_UI_FILENAME;
}

static void
gnc_plugin_ofx_init (GncPluginOfx *plugin)
{
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
gnc_plugin_ofx_cmd_import (GtkAction *action,
                           GncMainWindowActionData *data)
{
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
