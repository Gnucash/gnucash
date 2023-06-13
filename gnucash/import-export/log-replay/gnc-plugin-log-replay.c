/*
 * gnc-plugin-log-replay.c --
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
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
#include <glib/gi18n.h>

#include "gnc-log-replay.h"
#include "gnc-plugin-log-replay.h"
#include "gnc-plugin-manager.h"
#include "gnc-component-manager.h"

static void gnc_plugin_log_replay_finalize (GObject *object);

/* Command callbacks */
static void gnc_plugin_log_replay_cmd_new_log_replay (GSimpleAction *simple, GVariant *parameter, gpointer user_data);


#define PLUGIN_ACTIONS_NAME "gnc-plugin-log-replay-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-log-replay.ui"

static GActionEntry gnc_plugin_actions [] =
{
    { "LogReplayAction", gnc_plugin_log_replay_cmd_new_log_replay, NULL, NULL, NULL },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_n_actions = G_N_ELEMENTS(gnc_plugin_actions);

/** The default menu items that need to be add to the menu */
static const gchar *gnc_plugin_load_ui_items [] =
{
    "FilePlaceholder1",
    NULL,
};

struct _GncPluginLogReplay
{
    GncPlugin gnc_plugin;
};

G_DEFINE_TYPE(GncPluginLogReplay, gnc_plugin_log_replay, GNC_TYPE_PLUGIN)

GncPlugin *
gnc_plugin_log_replay_new (void)
{
    return GNC_PLUGIN (g_object_new (GNC_TYPE_PLUGIN_LOG_REPLAY, NULL));
}

static void
gnc_plugin_log_replay_class_init (GncPluginLogReplayClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    object_class->finalize = gnc_plugin_log_replay_finalize;

    /* plugin info */
    plugin_class->plugin_name  = GNC_PLUGIN_LOG_REPLAY_NAME;

    /* widget addition/removal */
    plugin_class->actions_name    = PLUGIN_ACTIONS_NAME;
    plugin_class->actions         = gnc_plugin_actions;
    plugin_class->n_actions       = gnc_plugin_n_actions;
    plugin_class->ui_filename     = PLUGIN_UI_FILENAME;
    plugin_class->ui_updates      = gnc_plugin_load_ui_items;
}

static void
gnc_plugin_log_replay_init (GncPluginLogReplay *plugin)
{
}

static void
gnc_plugin_log_replay_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN_LOG_REPLAY (object));

    G_OBJECT_CLASS (gnc_plugin_log_replay_parent_class)->finalize (object);
}

/************************************************************
 *              Plugin Function Implementation              *
 ************************************************************/

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_log_replay_cmd_new_log_replay (GSimpleAction *simple,
                                          GVariant      *parameter,
                                          gpointer       user_data)
{
    GncMainWindowActionData *data = user_data;
    gnc_suspend_gui_refresh();
    gnc_file_log_replay (GTK_WINDOW (data->window));
    gnc_resume_gui_refresh();
}

/************************************************************
 *                    Plugin Bootstrapping                   *
 ************************************************************/

void
gnc_plugin_log_replay_create_plugin (void)
{
    GncPlugin *plugin = gnc_plugin_log_replay_new ();

    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), plugin);
}
