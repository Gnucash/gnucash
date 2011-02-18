/*
 * gnc-plugin-gtkmm.cpp --
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
 * @file
 * @brief Plugin registration of the Gtkmm module
 * @author Copyright (C) 2011 Christian Stimming <christian@cstimming.de>
 */

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
extern "C" {
#include "gnome-utils/gnc-plugin-manager.h"
#include "gnome-utils/gnc-main-window.h"
#include "engine/Account.h"
}

#include "gnc-plugin-gtkmm.hpp"
#include <gtkmm.h>

namespace gncmm
{

// This static indicates the debugging module that this .o belongs to.
static QofLogModule log_module = G_LOG_DOMAIN;

static void gnc_plugin_gtkmm_class_init (GncPluginGtkmmClass *klass);
static void gnc_plugin_gtkmm_init (GncPluginGtkmm *plugin);
static void gnc_plugin_gtkmm_finalize (GObject *object);

/* Command callbacks */
static void gnc_plugin_gtkmm_cmd_something (GtkAction *action, GncMainWindowActionData *data);

#define PLUGIN_ACTIONS_NAME "gnc-plugin-gtkmm-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-gtkmm-ui.xml"

static GtkActionEntry gnc_plugin_actions [] =
{
    {
        // FIXME: I've intentionally remove the i18n markers so that
        // translators don't have to deal with these example strings!
        "GtkmmSomethingAction", NULL, "Do _Something in C++/gtkmm...", NULL,
        "This demonstrates how to integrate C++/gtkmm in a plugin of gnucash",
        G_CALLBACK (gnc_plugin_gtkmm_cmd_something)
    },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);

struct GncPluginGtkmmPrivate
{
    gpointer dummy;
};

#define GNC_PLUGIN_GTKMM_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_GTKMM, GncPluginGtkmmPrivate))

static GObjectClass *parent_class = NULL;

GType
gnc_plugin_gtkmm_get_type (void)
{
    static GType gnc_plugin_gtkmm_type = 0;

    if (gnc_plugin_gtkmm_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncPluginGtkmmClass),
            NULL,		/* base_init */
            NULL,		/* base_finalize */
            (GClassInitFunc) gnc_plugin_gtkmm_class_init,
            NULL,		/* class_finalize */
            NULL,		/* class_data */
            sizeof (GncPluginGtkmm),
            0,		/* n_preallocs */
            (GInstanceInitFunc) gnc_plugin_gtkmm_init,
        };

        gnc_plugin_gtkmm_type = g_type_register_static (GNC_TYPE_PLUGIN,
                                "GncPluginGtkmm",
                                &our_info, GTypeFlags(0));
    }

    return gnc_plugin_gtkmm_type;
}

GncPlugin *
gnc_plugin_gtkmm_new (void)
{
    return GNC_PLUGIN (g_object_new (GNC_TYPE_PLUGIN_GTKMM, NULL));
}

static void
gnc_plugin_gtkmm_class_init (GncPluginGtkmmClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    parent_class = reinterpret_cast<GObjectClass *>(g_type_class_peek_parent (klass));

    object_class->finalize = gnc_plugin_gtkmm_finalize;

    // plugin info
    plugin_class->plugin_name  = GNC_PLUGIN_GTKMM_NAME;

    // widget addition/removal
    plugin_class->actions_name = PLUGIN_ACTIONS_NAME;
    plugin_class->actions      = gnc_plugin_actions;
    plugin_class->n_actions    = gnc_plugin_n_actions;
    plugin_class->ui_filename  = PLUGIN_UI_FILENAME;

    g_type_class_add_private(klass, sizeof(GncPluginGtkmmPrivate));
}

static void
gnc_plugin_gtkmm_init (GncPluginGtkmm *plugin)
{
}

static void
gnc_plugin_gtkmm_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN_GTKMM (object));

    GncPluginGtkmm *plugin = GNC_PLUGIN_GTKMM (object);
    GncPluginGtkmmPrivate *priv = GNC_PLUGIN_GTKMM_GET_PRIVATE(plugin);

    G_OBJECT_CLASS (parent_class)->finalize (object);
}

/************************************************************
 *              Plugin Function Implementation              *
 ************************************************************/

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_gtkmm_cmd_something (GtkAction *action,
                                GncMainWindowActionData *data)
{
    // Now we're gonna do something.
    ENTER("action %p, main window data %p", action, data);
    g_return_if_fail(data);

//     ::Account *c_account = NULL;//main_window_to_account(data->window);
//     Glib::RefPtr<Glib::Object> account_ptr = Glib::wrap(c_account);

    LEAVE("");
}


/************************************************************
 *                    Plugin Bootstrapping                   *
 ************************************************************/

void
gnc_plugin_gtkmm_create_plugin (void)
{
    GncPlugin *plugin = gnc_plugin_gtkmm_new ();

    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), plugin);
}

} // END namespace gncmm
