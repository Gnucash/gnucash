/*
 * gnc-plugin-example.c --
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
 * @file gnc-plugin-example.c
 * @brief Plugin registration of the example plugin
 * @author Copyright (C) 2009 ?enter your name here? <your-email@example.com>
 */

#include <config.h>

#include <glib/gi18n.h>

#include "gnc-plugin-example.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

static void gnc_plugin_example_finalize           (GObject *object);

/* Command callbacks */
static void gnc_plugin_example_cmd_test (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
+{

#define PLUGIN_ACTIONS_NAME "gnc-plugin-example-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-example.ui"

static GActionEntry gnc_plugin_actions [] =
{
    { "exampleAction", gnc_plugin_example_cmd_test, NULL, NULL, NULL },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_n_actions = G_N_ELEMENTS(gnc_plugin_actions);

/************************************************************
 *                   Object Implementation                  *
 ************************************************************/

struct _GncPluginExample
{
    GncPlugin gnc_plugin;
};

G_DEFINE_TYPE(GncPluginExample, gnc_plugin_example, GNC_TYPE_PLUGIN)

GncPlugin *
gnc_plugin_example_new (void)
{
    return GNC_PLUGIN (g_object_new (GNC_TYPE_PLUGIN_EXAMPLE, (gchar*) NULL));
}

static void
gnc_plugin_example_class_init (GncPluginExampleClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS(klass);

    object_class->finalize = gnc_plugin_example_finalize;

    /* plugin info */
    plugin_class->plugin_name  = GNC_PLUGIN_example_NAME;

    /* widget addition/removal */
    plugin_class->actions_name    = PLUGIN_ACTIONS_NAME;
    plugin_class->actions         = gnc_plugin_actions;
    plugin_class->n_actions       = gnc_plugin_n_actions;
    plugin_class->ui_filename     = PLUGIN_UI_FILENAME;
}

static void
gnc_plugin_example_init (GncPluginExample *plugin)
{
}

static void
gnc_plugin_example_finalize (GObject *object)
{
}

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_example_cmd_test (GSimpleAction *simple,
                             GVariant      *parameter,
                             gpointer       user_data)
{
    GncMainWindowActionData *data = user_data;
    ENTER("action %p, main window data %p", simple, data);
    PINFO("example");
    LEAVE(" ");
}
