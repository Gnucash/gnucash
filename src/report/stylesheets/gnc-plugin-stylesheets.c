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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#include "config.h"

#include "dialog-style-sheet.h"
#include "egg-action-group.h"
#include "gnc-gnome-utils.h"
#include "gnc-plugin-page-account-tree.h"
#include "gnc-plugin-stylesheets.h"
#include "gnc-plugin-manager.h"
#include "gnc-trace.h"
#include "messages.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;
static GList *active_plugins = NULL;

static void gnc_plugin_stylesheets_class_init (GncPluginStylesheetsClass *klass);
static void gnc_plugin_stylesheets_init (GncPluginStylesheets *plugin);
static void gnc_plugin_stylesheets_finalize (GObject *object);

static void gnc_plugin_stylesheets_add_to_window (GncPlugin *plugin,
						  GncMainWindow *window,
						  GQuark type);
static void gnc_plugin_stylesheets_remove_from_window (GncPlugin *plugin,
						       GncMainWindow *window,
						       GQuark type);

/* Callbacks on other objects */
static void gnc_plugin_stylesheets_main_window_page_changed (GncMainWindow *window,
							     GncPluginPage *page);

/* Command callbacks */
static void gnc_plugin_stylesheets_cmd_edit_style_sheet (EggAction *action,
							 GncMainWindowActionData *data);


#define PLUGIN_ACTIONS_NAME "gnc-plugin-stylesheets-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-stylesheets-ui.xml"

static EggActionEntry gnc_plugin_actions [] = {
  /* Menu Items */
  { "EditStyleSheetsAction", N_("_Style Sheets..."), NULL, NULL,
    N_("Edit report style sheets."),
    G_CALLBACK (gnc_plugin_stylesheets_cmd_edit_style_sheet) },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);


static const gchar *account_tree_actions[] = {
  "EditStyleSheetsAction",
  NULL
};


struct GncPluginStylesheetsPrivate
{
  gpointer dummy;
};

static GObjectClass *parent_class = NULL;

/************************************************************
 *                   Object Implementation                  *
 ************************************************************/

GType
gnc_plugin_stylesheets_get_type (void)
{
  static GType gnc_plugin_stylesheets_type = 0;

  if (gnc_plugin_stylesheets_type == 0) {
    static const GTypeInfo our_info = {
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

#if DEBUG_REFERENCE_COUNTING
static void
dump_model (GncPluginStylesheets *plugin, gpointer dummy)
{
    g_warning("GncPluginStylesheets %p still exists.", plugin);
}

static gint
gnc_plugin_stylesheets_report_references (void)
{
  g_list_foreach(active_plugins, (GFunc)dump_model, NULL);
  return 0;
}
#endif

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
  plugin_class->add_to_window 	   = gnc_plugin_stylesheets_add_to_window;
  plugin_class->remove_from_window = gnc_plugin_stylesheets_remove_from_window;

#if DEBUG_REFERENCE_COUNTING
  gtk_quit_add (0,
		(GtkFunction)gnc_plugin_stylesheets_report_references,
		NULL);
#endif
}

static void
gnc_plugin_stylesheets_init (GncPluginStylesheets *plugin)
{
  plugin->priv = g_new0 (GncPluginStylesheetsPrivate, 1);

  active_plugins = g_list_append (active_plugins, plugin);
}

static void
gnc_plugin_stylesheets_finalize (GObject *object)
{
  GncPluginStylesheets *plugin;

  g_return_if_fail (GNC_IS_PLUGIN_STYLESHEETS (object));

  plugin = GNC_PLUGIN_STYLESHEETS (object);
  active_plugins = g_list_remove (active_plugins, plugin);

  g_return_if_fail (plugin->priv != NULL);

  g_free (plugin->priv);

  G_OBJECT_CLASS (parent_class)->finalize (object);
}

/*
 * The gnc_plugin_add_to_window() function has already added our
 * actions to the main window.  STYLESHEETS include this function so that it
 * can attach callbacks to the window and track page changes within
 * each window.  Sneaky, huh?
 */
static void
gnc_plugin_stylesheets_add_to_window (GncPlugin *plugin,
				      GncMainWindow *window,
				      GQuark type)
{
  g_signal_connect (G_OBJECT(window), "page_changed",
		    G_CALLBACK (gnc_plugin_stylesheets_main_window_page_changed),
		    plugin);
}

static void
gnc_plugin_stylesheets_remove_from_window (GncPlugin *plugin,
					   GncMainWindow *window,
					   GQuark type)
{
  g_signal_handlers_disconnect_by_func(G_OBJECT(window),
				       G_CALLBACK (gnc_plugin_stylesheets_main_window_page_changed),
				       plugin);
}

/************************************************************
 *                     Object Callbacks                     *
 ************************************************************/

/** Whenever the current page has changed, update the stylesheets menus based
 *  upon the page that is currently selected. */
static void
gnc_plugin_stylesheets_main_window_page_changed (GncMainWindow *window,
						 GncPluginPage *page)
{
  EggActionGroup *action_group;
  const gchar    *page_name;

  ENTER("main window %p, page %p", window, page);
  action_group = gnc_main_window_get_action_group(window,PLUGIN_ACTIONS_NAME);
  g_return_if_fail(action_group != NULL);

  /* Reset everything to known state */
  gnc_gnome_utils_update_actions(action_group, account_tree_actions,
				 "visible", FALSE);

  /* Any page selected? */
  if (page == NULL) {
    LEAVE("no page");
    return;
  }

  /* Selectively make items visible */
  page_name = gnc_plugin_page_get_name(page);
  if (strcmp(page_name, GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME) == 0) {
    DEBUG("account tree page");
    gnc_gnome_utils_update_actions(action_group, account_tree_actions,
				   "visible", TRUE);
  }

  LEAVE(" ");
}
/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_stylesheets_cmd_edit_style_sheet (EggAction *action,
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
