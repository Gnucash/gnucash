/*
 * gnc-plugin-stylesheets.c --
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-style-sheet.h"
#include "gnc-gnome-utils.h"
#include "gnc-plugin-stylesheets.h"
#include "gnc-plugin-manager.h"
#include "gnc-engine.h"

static void gnc_plugin_stylesheets_class_init (GncPluginStylesheetsClass *klass);
static void gnc_plugin_stylesheets_init (GncPluginStylesheets *plugin);
static void gnc_plugin_stylesheets_finalize (GObject *object);


/* Command callbacks */
static void gnc_plugin_stylesheets_cmd_edit_style_sheet (GtkAction *action,
        GncMainWindowActionData *data);


#define PLUGIN_ACTIONS_NAME "gnc-plugin-stylesheets-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-stylesheets-ui.xml"

static GtkActionEntry gnc_plugin_actions [] =
{
    /* Menu Items */
    {
        "EditStyleSheetsAction", NULL, N_("St_yle Sheets"), NULL,
        N_("Edit report style sheets."),
        G_CALLBACK (gnc_plugin_stylesheets_cmd_edit_style_sheet)
    },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);


typedef struct GncPluginStylesheetsPrivate
{
    gpointer dummy;
} GncPluginStylesheetsPrivate;

#define GNC_PLUGIN_STYLESHEETS_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_STYLESHEETS, GncPluginStylesheetsPrivate))

static GObjectClass *parent_class = NULL;

/************************************************************
 *                   Object Implementation                  *
 ************************************************************/

GType
gnc_plugin_stylesheets_get_type (void)
{
    static GType gnc_plugin_stylesheets_type = 0;

    if (gnc_plugin_stylesheets_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncPluginStylesheetsClass),
            NULL,		/* base_init */
            NULL,		/* base_finalize */
            (GClassInitFunc) gnc_plugin_stylesheets_class_init,
            NULL,		/* class_finalize */
            NULL,		/* class_data */
            sizeof (GncPluginStylesheets),
            0,		/* n_preallocs */
            (GInstanceInitFunc) gnc_plugin_stylesheets_init,
        };

        gnc_plugin_stylesheets_type = g_type_register_static (GNC_TYPE_PLUGIN,
                                      "GncPluginStylesheets",
                                      &our_info, 0);
    }

    return gnc_plugin_stylesheets_type;
}

GncPlugin *
gnc_plugin_stylesheets_new (void)
{
    return GNC_PLUGIN (g_object_new (GNC_TYPE_PLUGIN_STYLESHEETS, NULL));
}

static void
gnc_plugin_stylesheets_class_init (GncPluginStylesheetsClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_stylesheets_finalize;

    /* plugin info */
    plugin_class->plugin_name  = GNC_PLUGIN_STYLESHEETS_NAME;

    /* widget addition/removal */
    plugin_class->actions_name  	   = PLUGIN_ACTIONS_NAME;
    plugin_class->actions       	   = gnc_plugin_actions;
    plugin_class->n_actions     	   = gnc_plugin_n_actions;
    plugin_class->ui_filename   	   = PLUGIN_UI_FILENAME;

    g_type_class_add_private(klass, sizeof(GncPluginStylesheetsPrivate));
}

static void
gnc_plugin_stylesheets_init (GncPluginStylesheets *plugin)
{
}

static void
gnc_plugin_stylesheets_finalize (GObject *object)
{
    GncPluginStylesheets *plugin;
    GncPluginStylesheetsPrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_STYLESHEETS (object));

    plugin = GNC_PLUGIN_STYLESHEETS (object);
    priv = GNC_PLUGIN_STYLESHEETS_GET_PRIVATE(plugin);

    G_OBJECT_CLASS (parent_class)->finalize (object);
}

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_stylesheets_cmd_edit_style_sheet (GtkAction *action,
        GncMainWindowActionData *data)
{
    gnc_style_sheet_dialog_open();
}

/************************************************************
 *                    Plugin Bootstrapping                   *
 ************************************************************/

void
gnc_plugin_stylesheets_create_plugin (void)
{
    GncPlugin *plugin = gnc_plugin_stylesheets_new ();

    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), plugin);
}
