/* 
 * gnc-plugin-file-history.c -- 
 * Copyright (C) 2003 David Hampton hampton@employees.org>
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#include "config.h"

#include <string.h>
#include <glib/gprintf.h>
#include <libgnome/libgnome.h>

#include "gnc-file.h"
#include "gnc-file-history.h"
#include "gnc-main-window.h"
#include "gnc-plugin-file-history.h"
#include "gnc-window.h"
#include "messages.h"

static GList *active_pages = NULL;
static GObjectClass *parent_class = NULL;

#define FILENAME_STRING "filename"

static void gnc_plugin_file_history_class_init (GncPluginFileHistoryClass *klass);
static void gnc_plugin_file_history_init (GncPluginFileHistory *plugin);
static void gnc_plugin_file_history_finalize (GObject *object);

static void gnc_plugin_file_history_add_to_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);

/* Command callbacks */
static void gnc_plugin_file_history_cmd_open_file (EggAction *action, GncPlugin *plugin);


#define PLUGIN_ACTIONS_NAME "gnc-plugin-file-history-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-file-history-ui.xml"

static EggActionEntry gnc_plugin_actions [] = {
	{ "FileOpenRecentAction", N_("Open _Recent"), NULL, NULL, NULL, NULL },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);


struct GncPluginFileHistoryPrivate
{
	gpointer dummy;
};


/************************************************************
 *                     Other Functions                      *
 ************************************************************/

static gchar *
gnc_history_generate_label (int index, const gchar *filename)
{
	const gchar *src;
	gchar *result, *dst;
	gunichar  unichar;

	/* raw byte length, not num characters */
	result = g_malloc(strlen(filename) * 2);

	dst = result + g_sprintf(result, "_%d ", index % 10);
	for (src = filename; *src; src = g_utf8_next_char(src)) {
	  unichar = g_utf8_get_char(src);
	  dst += g_unichar_to_utf8 (unichar, dst);

	  if (unichar == '_')
	    dst += g_unichar_to_utf8 ('_', dst);
	}

	*dst = '\0';
	return result;
}

static void
gnc_history_update_menus (GncPlugin *plugin)
{
	GncMainWindow *window;
	EggActionGroup *action_group;
	EggAction *action;
	gchar action_name[40], *label_name, *old_filename;
	const GList *history_list, *tmp;
	GValue label = { 0 };
	guint i;

	static EggActionEntry new_actions =
	  { NULL, "", NULL, NULL, NULL,
	    G_CALLBACK (gnc_plugin_file_history_cmd_open_file) };

	/* Get the history list */
	history_list = gnc_history_get_file_list();
	if (history_list == NULL)
	  return;

	/* Get the action group */
	window = GNC_MAIN_WINDOW(plugin->window);
	action_group =
	  gnc_main_window_get_action_group(window, PLUGIN_ACTIONS_NAME);

	/* Build the menu */
	g_value_init (&label, G_TYPE_STRING);
	for (tmp = history_list, i = 1; tmp; tmp = g_list_next(tmp), i++) {
	  /* Find or create the action object */
	  g_sprintf(action_name, "RecentFile%dAction", i % 10);
	  action = egg_action_group_get_action (action_group, action_name);
	  if (action == NULL) {
	    new_actions.name = action_name;
	    egg_action_group_add_actions (action_group, &new_actions, 1, plugin);
	    action = egg_action_group_get_action (action_group, action_name);
	  }

	  /* set the menu label (w/accelerator) */
	  label_name = gnc_history_generate_label(i, tmp->data);
	  g_value_set_string (&label, label_name);
	  g_object_set_property (G_OBJECT(action), "label", &label);
	  g_free(label_name);

	  /* set the filename for the callback function */
	  old_filename = g_object_get_data(G_OBJECT(action), FILENAME_STRING);
	  if (old_filename)
	    g_free(old_filename);
	  g_object_set_data(G_OBJECT(action), FILENAME_STRING, g_strdup(tmp->data));
	}

	gnc_main_window_actions_updated (window);
}

static void
gnc_plugin_file_history_update_helper (GncPlugin *plugin,
				       gpointer user_data)
{
	gnc_history_update_menus (plugin);
}

static void
gnc_plugin_file_history_update_all (void)
{
	g_list_foreach(active_pages,
		       (GFunc)gnc_plugin_file_history_update_helper,
		       NULL);
}

/************************************************************
 *                  Object Implementation                   *
 ************************************************************/

GType
gnc_plugin_file_history_get_type (void)
{
	static GType gnc_plugin_file_history_type = 0;

	if (gnc_plugin_file_history_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncPluginFileHistoryClass),
			NULL,		/* base_init */
			NULL,		/* base_finalize */
			(GClassInitFunc) gnc_plugin_file_history_class_init,
			NULL,		/* class_finalize */
			NULL,		/* class_data */
			sizeof (GncPluginFileHistory),
			0,
			(GInstanceInitFunc) gnc_plugin_file_history_init
		};

		gnc_plugin_file_history_type = g_type_register_static (GNC_TYPE_PLUGIN,
								       "GncPluginFileHistory",
								       &our_info, 0);
	}

	return gnc_plugin_file_history_type;
}

static void
gnc_plugin_file_history_class_init (GncPluginFileHistoryClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_plugin_file_history_finalize;

	/* plugin info */
	plugin_class->plugin_name   = GNC_PLUGIN_FILE_HISTORY_NAME;

	/* function overrides */
	plugin_class->add_to_window = gnc_plugin_file_history_add_to_window;

	/* widget addition/removal */
	plugin_class->actions_name  = PLUGIN_ACTIONS_NAME;
	plugin_class->actions       = gnc_plugin_actions;
	plugin_class->n_actions     = gnc_plugin_n_actions;
	plugin_class->ui_filename   = PLUGIN_UI_FILENAME;

	/* hook in callback functions */
	gnc_history_set_callback (gnc_plugin_file_history_update_all);
}

static void
gnc_plugin_file_history_init (GncPluginFileHistory *plugin)
{
	plugin->priv = g_new0 (GncPluginFileHistoryPrivate, 1);
}

static void
gnc_plugin_file_history_finalize (GObject *object)
{
	GncPluginFileHistory *plugin;

	g_return_if_fail (GNC_IS_PLUGIN_FILE_HISTORY (object));

	plugin = GNC_PLUGIN_FILE_HISTORY (object);
	g_return_if_fail (plugin->priv != NULL);

	g_free (plugin->priv);

	active_pages = g_list_remove (active_pages, plugin);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

GncPlugin *
gnc_plugin_file_history_new (void)
{
	GncPlugin *plugin_page = NULL;

	plugin_page = GNC_PLUGIN (g_object_new (GNC_TYPE_PLUGIN_FILE_HISTORY, NULL));
	active_pages = g_list_append (active_pages, plugin_page);
	return plugin_page;
}

/************************************************************
 *              Plugin Function Implementation              *
 ************************************************************/

static void
gnc_plugin_file_history_add_to_window (GncPlugin *plugin,
				       GncMainWindow *window,
				       GQuark type)
{
	gnc_history_update_menus (plugin);
}

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_file_history_cmd_open_file (EggAction *action,
				       GncPlugin *plugin)
{
	gchar *filename;

	g_return_if_fail(EGG_IS_ACTION(action));
	g_return_if_fail(GNC_IS_PLUGIN(plugin));

	filename = g_object_get_data(G_OBJECT(action), FILENAME_STRING);
	gnc_window_set_progressbar_window (GNC_WINDOW(plugin->window));
	gnc_file_open_file (filename); /* also opens new account page */
	gnc_window_set_progressbar_window (NULL);
	gnc_main_window_update_title (plugin->window);
	/* FIXME GNOME 2 Port (update the title etc.) */
}
