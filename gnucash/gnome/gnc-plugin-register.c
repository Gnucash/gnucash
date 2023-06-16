/*
 * gnc-plugin-register.c --
 *
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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include "gnc-component-manager.h"
#include "gnc-plugin-register.h"
#include "gnc-plugin-page-register.h"
#include "gnc-prefs.h"


static void gnc_plugin_register_finalize (GObject *object);

static void gnc_plugin_register_add_to_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);
static void gnc_plugin_register_remove_from_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);

/* Command callbacks */
static void gnc_plugin_register_cmd_general_ledger (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

#define PLUGIN_ACTIONS_NAME "gnc-plugin-register-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-register.ui"

static GActionEntry gnc_plugin_actions [] =
{
    { "ToolsGeneralJournalAction", gnc_plugin_register_cmd_general_ledger, NULL, NULL, NULL },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_n_actions = G_N_ELEMENTS(gnc_plugin_actions);

/** The default menu items that need to be add to the menu */
static const gchar *gnc_plugin_load_ui_items [] =
{
    "ToolsPlaceholder2",
    NULL,
};

struct _GncPluginRegister
{
    GncPlugin gnc_plugin;
};

G_DEFINE_TYPE(GncPluginRegister, gnc_plugin_register, GNC_TYPE_PLUGIN)

static QofLogModule log_module = GNC_MOD_GUI;

/************************************************************
 *                     Other Functions                      *
 ************************************************************/

/** This function is called whenever an entry in the general register
 *  preferences group is changed.  It does nothing more than kick off a
 *  gui refresh which should be delivered to any open register page.
 *  The register pages will then reread their preferences and
 *  update the screen.
 *
 *  @prefs Unused.
 *
 *  @pref Unused.
 *
 *  @user_data Unused.
 */
static void
gnc_plugin_register_pref_changed (gpointer prefs, gchar *pref,
                                   gpointer user_data)
{
    ENTER("");
    gnc_gui_refresh_all ();
    LEAVE("");
}

/************************************************************
 *                  Object Implementation                   *
 ************************************************************/

GncPlugin *
gnc_plugin_register_new (void)
{
    GncPluginRegister *plugin;

    /* Reference the register page plugin to ensure it exists in
     * the gtk type system. */
    GNC_TYPE_PLUGIN_PAGE_REGISTER;

    plugin = g_object_new (GNC_TYPE_PLUGIN_REGISTER,
                           NULL);

    return GNC_PLUGIN (plugin);
}

static void
gnc_plugin_register_class_init (GncPluginRegisterClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    object_class->finalize = gnc_plugin_register_finalize;

    /* plugin info */
    plugin_class->plugin_name  = GNC_PLUGIN_REGISTER_NAME;

    /* function overrides */
    plugin_class->add_to_window = gnc_plugin_register_add_to_window;
    plugin_class->remove_from_window =
        gnc_plugin_register_remove_from_window;

    /* widget addition/removal */
    plugin_class->actions_name    = PLUGIN_ACTIONS_NAME;
    plugin_class->actions         = gnc_plugin_actions;
    plugin_class->n_actions       = gnc_plugin_n_actions;
    plugin_class->ui_filename     = PLUGIN_UI_FILENAME;
    plugin_class->ui_updates      = gnc_plugin_load_ui_items;
}

static void
gnc_plugin_register_init (GncPluginRegister *plugin)
{
}

static void
gnc_plugin_register_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN_REGISTER (object));

    G_OBJECT_CLASS (gnc_plugin_register_parent_class)->finalize (object);
}

/************************************************************
 *              Plugin Function Implementation              *
 ************************************************************/

/** Initialize the registeru for a window.  This function is
 *  called as part of the initialization of a window, after all the
 *  plugin menu items have been added to the menu structure.  Its job
 *  is to correctly initialize the register.  It does this by
 *  installing a function that listens for preference changes. Each
 *  time a preference changes, it kicks off a gui refresh.
 *
 *  @param plugin A pointer to the gnc-plugin object responsible for
 *  adding/removing the register.
 *
 *  @param window A pointer to the gnc-main-window that is being initialized.
 *
 *  @param type Unused
 */
static void
gnc_plugin_register_add_to_window (GncPlugin *plugin,
                                   GncMainWindow *window,
                                   GQuark type)
{
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL_REGISTER, NULL,
                           gnc_plugin_register_pref_changed, window);
}


/** Finalize the register for this window.  This function is
 *  called as part of the destruction of a window.
 *
 *  @param plugin A pointer to the gnc-plugin object responsible for
 *  adding/removing the register.  It stops listening for
 *  changes in the register preferences.
 *
 *  @param window A pointer the gnc-main-window that is being destroyed.
 *
 *  @param type Unused
 */
static void
gnc_plugin_register_remove_from_window (GncPlugin *plugin,
                                        GncMainWindow *window,
                                        GQuark type)
{
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL_REGISTER, NULL,
                                 gnc_plugin_register_pref_changed, window);
}


/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_register_cmd_general_ledger (GSimpleAction *simple,
                                        GVariant *parameter,
                                        gpointer user_data)
{
    GncMainWindowActionData *data = user_data;
    GncPluginPage *page;

    g_return_if_fail (data != NULL);

    page = gnc_plugin_page_register_new_gl ();
    gnc_main_window_open_page (data->window, page);
}
