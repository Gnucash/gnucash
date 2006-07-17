/* 
 * gnc-plugin-transactions.c -- 
 *
 * Copyright (C) 2006 Chris Shoemaker <c.shoemaker@cox.net>
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
#include <string.h>

#include "gnc-component-manager.h"
#include "gnc-plugin-transactions.h"
#include "gnc-engine.h"
//#include "gnc-plugin-page-transactions.h"

static void gnc_plugin_transactions_class_init(
    GncPluginTransactionsClass *klass);
static void gnc_plugin_transactions_init(GncPluginTransactions *plugin);
static void gnc_plugin_transactions_finalize(GObject *object);

#define PLUGIN_ACTIONS_NAME "gnc-plugin-transactions-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-transactions-ui.xml"
#define GCONF_TRANSACTIONS_SECTION "general/transactions"

static GtkActionEntry gnc_plugin_actions [] = {
    /*
    { "ToolsGeneralLedgerAction", NULL, N_("_General Ledger"), NULL,
      N_("Open a general ledger window"),
      G_CALLBACK (gnc_plugin_transactions_cmd_general_ledger) },
    */
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);

typedef struct GncPluginTransactionsPrivate
{
    gpointer dummy;
} GncPluginTransactionsPrivate;

#define GNC_PLUGIN_TRANSACTIONS_GET_PRIVATE(o)                          \
    (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_TRANSACTIONS, GncPluginTransactionsPrivate))

static GObjectClass *parent_class = NULL;
static QofLogModule log_module = GNC_MOD_GUI;

/************************************************************
 *                     Other Functions                      *
 ************************************************************/

/** This function is called whenever an entry in the general transactions
 *  section of gconf is changed.  It does nothing more than kick off a
 *  gui refresh which should be delivered to any open transactions page.
 *  The transactions pages will then reread their settings from gconf and
 *  update the screen.
 *
 *  @client Unused.
 *
 *  @cnxn_id Unused.
 *
 *  @entry Unused.
 *
 *  @user_data Unused.
 */
static void
gnc_plugin_transactions_gconf_changed(GConfClient *client,
                                      guint cnxn_id,
                                      GConfEntry *entry,
                                      gpointer user_data)
{
    ENTER("");
    gnc_gui_refresh_all ();
    LEAVE("");
}

/************************************************************
 *                  Object Implementation                   *
 ************************************************************/

GType
gnc_plugin_transactions_get_type (void)
{
    static GType gnc_plugin_transactions_type = 0;
    
    if (gnc_plugin_transactions_type == 0) {
        static const GTypeInfo our_info = {
            sizeof (GncPluginTransactionsClass),
            NULL,		/* base_init */
            NULL,		/* base_finalize */
            (GClassInitFunc) gnc_plugin_transactions_class_init,
            NULL,		/* class_finalize */
            NULL,		/* class_data */
            sizeof (GncPluginTransactions),
            0,		/* n_preallocs */
            (GInstanceInitFunc) gnc_plugin_transactions_init
        };
	
        gnc_plugin_transactions_type = g_type_register_static(
            GNC_TYPE_PLUGIN,
            "GncPluginTransactions",
            &our_info, 0);
    }
    
    return gnc_plugin_transactions_type;
}

GncPlugin *
gnc_plugin_transactions_new (void)
{
    GncPluginTransactions *plugin;
    
    /* Reference the transactions page plugin to ensure it exists in
     * the gtk type system. */
    //GNC_TYPE_PLUGIN_PAGE_TRANSACTIONS;
    
    plugin = g_object_new (GNC_TYPE_PLUGIN_TRANSACTIONS,
                           NULL);
    
    return GNC_PLUGIN (plugin);
}

static void
gnc_plugin_transactions_class_init (GncPluginTransactionsClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);
    
    parent_class = g_type_class_peek_parent (klass);
    
    object_class->finalize = gnc_plugin_transactions_finalize;
    
    /* plugin info */
    plugin_class->plugin_name  = GNC_PLUGIN_TRANSACTIONS_NAME;
    
    /* widget addition/removal */
    plugin_class->actions_name = PLUGIN_ACTIONS_NAME;
    plugin_class->actions      = gnc_plugin_actions;
    plugin_class->n_actions    = gnc_plugin_n_actions;
    plugin_class->ui_filename  = PLUGIN_UI_FILENAME;
    
    plugin_class->gconf_section = GCONF_TRANSACTIONS_SECTION;
    plugin_class->gconf_notifications = gnc_plugin_transactions_gconf_changed;
    
    g_type_class_add_private(klass, sizeof(GncPluginTransactionsPrivate));
}

static void
gnc_plugin_transactions_init (GncPluginTransactions *plugin)
{
}

static void
gnc_plugin_transactions_finalize (GObject *object)
{
    GncPluginTransactions *plugin;
    GncPluginTransactionsPrivate *priv;
    
    g_return_if_fail (GNC_IS_PLUGIN_TRANSACTIONS (object));
    
    plugin = GNC_PLUGIN_TRANSACTIONS (object);
    priv = GNC_PLUGIN_TRANSACTIONS_GET_PRIVATE(plugin);
    
    G_OBJECT_CLASS (parent_class)->finalize (object);
}
