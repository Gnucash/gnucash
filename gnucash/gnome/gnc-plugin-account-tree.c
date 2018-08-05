/*
 * gnc-plugin-account-tree.c --
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

/** @addtogroup MenuPlugins
    @{ */
/** @addtogroup GncPluginAccountTree An Account Tree Plugin
    @{ */
/** @file gnc-plugin-account-tree.c
    @brief Provide the menus to create a chart of account page.
    @author Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
*/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include "gnc-plugin-account-tree.h"
#include "gnc-plugin-page-account-tree.h"
#include "dialog-find-account.h"

static void gnc_plugin_account_tree_class_init (GncPluginAccountTreeClass *klass);
static void gnc_plugin_account_tree_init (GncPluginAccountTree *plugin);
static void gnc_plugin_account_tree_finalize (GObject *object);
static void gnc_plugin_account_tree_add_to_window (GncPlugin *plugin,
                                                   GncMainWindow *window, GQuark type);

/* Command callbacks */
static void gnc_plugin_account_tree_cmd_new_account_tree (GtkAction *action, GncMainWindowActionData *data);

#define PLUGIN_ACTIONS_NAME "gnc-plugin-account-tree-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-account-tree-ui.xml"

/** An array of all of the actions provided by the account tree
 *  plugin. */
static GtkActionEntry gnc_plugin_actions [] =
{
    {
        "ViewAccountTreeAction", NULL, N_("New Accounts _Page"), NULL,
        N_("Open a new Account Tree page"),
        G_CALLBACK (gnc_plugin_account_tree_cmd_new_account_tree)
    },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);


/** The instance private data structure for an account tree plugin. */
typedef struct GncPluginAccountTreePrivate
{
    gpointer dummy;
} GncPluginAccountTreePrivate;

#define GNC_PLUGIN_ACCOUNT_TREE_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_ACCOUNT_TREE, GncPluginAccountTreePrivate))

/** A pointer to the parent class of a plugin page. */
static GObjectClass *parent_class = NULL;


/*  Get the type of the account tree menu plugin. */
GType
gnc_plugin_account_tree_get_type (void)
{
    static GType gnc_plugin_account_tree_type = 0;

    if (gnc_plugin_account_tree_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncPluginAccountTreeClass),
            NULL,		/* base_init */
            NULL,		/* base_finalize */
            (GClassInitFunc) gnc_plugin_account_tree_class_init,
            NULL,		/* class_finalize */
            NULL,		/* class_data */
            sizeof (GncPluginAccountTree),
            0,		/* n_preallocs */
            (GInstanceInitFunc) gnc_plugin_account_tree_init
        };

        gnc_plugin_account_tree_type = g_type_register_static (GNC_TYPE_PLUGIN,
                                       "GncPluginAccountTree",
                                       &our_info, 0);
    }

    return gnc_plugin_account_tree_type;
}


/*  Create a new account tree menu plugin. */
GncPlugin *
gnc_plugin_account_tree_new (void)
{
    GncPluginAccountTree *plugin;

    /* Reference the account tree page plugin to ensure it exists
     * in the gtk type system. */
    GNC_TYPE_PLUGIN_PAGE_ACCOUNT_TREE;

    plugin = g_object_new (GNC_TYPE_PLUGIN_ACCOUNT_TREE,
                           NULL);

    return GNC_PLUGIN (plugin);
}


static void
gnc_plugin_account_tree_main_window_page_changed (GncMainWindow *window,
        GncPluginPage *plugin_page, gpointer user_data)
{
    // We continue only if the plugin_page is a valid
    if (!plugin_page || !GNC_IS_PLUGIN_PAGE(plugin_page))
        return;

    if (gnc_main_window_get_current_page (window) == plugin_page)
    {
        if (!GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(plugin_page))
            return;

        // The page changed signal is emitted multiple times so we need
        // to use an idle_add to change the focus to the tree view
        g_idle_remove_by_data (GNC_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page));
        g_idle_add ((GSourceFunc)gnc_plugin_page_account_tree_focus,
                      GNC_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page));
    }
}


/** Initialize the class for a new account tree plugin.  This will set
 *  up any function pointers that override functions in the parent
 *  class, and also configure the private data storage for this
 *  widget.
 *
 *  @param klass The new class structure created by the object system.
 */
static void
gnc_plugin_account_tree_class_init (GncPluginAccountTreeClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_account_tree_finalize;

    /* plugin info */
    plugin_class->plugin_name  = GNC_PLUGIN_ACCOUNT_TREE_NAME;

    /* function overrides */
    plugin_class->add_to_window = gnc_plugin_account_tree_add_to_window;

    /* widget addition/removal */
    plugin_class->actions_name = PLUGIN_ACTIONS_NAME;
    plugin_class->actions      = gnc_plugin_actions;
    plugin_class->n_actions    = gnc_plugin_n_actions;
    plugin_class->ui_filename  = PLUGIN_UI_FILENAME;

    g_type_class_add_private(klass, sizeof(GncPluginAccountTreePrivate));
}


/** Initialize a new instance of a gnucash content plugin.  This
 *  function currently does nothing.
 *
 *  @param page The new object instance created by the object
 *  system. */
static void
gnc_plugin_account_tree_init (GncPluginAccountTree *plugin)
{
}


/** Finalize the account tree plugin object.  This function is called
 *  from the G_Object level to complete the destruction of the object.
 *  It should release any memory not previously released by the
 *  destroy function (i.e. the private data structure), then chain up
 *  to the parent's destroy function.  This function currently does
 *  nothing.
 *
 *  @param object The object being destroyed. */
static void
gnc_plugin_account_tree_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN_ACCOUNT_TREE (object));

    G_OBJECT_CLASS (parent_class)->finalize (object);
}


/**
 * Called when this plugin is added to a main window.  Connect a few callbacks
 * here to track page changes.
 *
 */
static void gnc_plugin_account_tree_add_to_window (GncPlugin *plugin,
        GncMainWindow *mainwindow,
        GQuark type)
{
    g_signal_connect(mainwindow, "page_changed",
                     G_CALLBACK(gnc_plugin_account_tree_main_window_page_changed),
                     plugin);
}
/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_account_tree_cmd_new_account_tree (GtkAction *action,
        GncMainWindowActionData *data)
{
    GncPluginPage *page;

    g_return_if_fail (data != NULL);

    page = gnc_plugin_page_account_tree_new ();
    gnc_main_window_open_page (data->window, page);
}

/** @} */
/** @} */
