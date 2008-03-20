/* 
 * gnc-plugin-hbci.c -- 
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
 * Copyright (C) 2002 Christian Stimming                            *
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

#include "gnc-engine.h"
#include "druid-hbci-initial.h"
#include "gnc-plugin-manager.h"
#include "gnc-gnome-utils.h"
#include "gnc-hbci-getbalance.h"
#include "gnc-hbci-gettrans.h"
#include "gnc-hbci-transfer.h"
#include "gnc-file-aqb-import.h"
#include "gnc-plugin-hbci.h"
#include "gnc-plugin-manager.h"
#include "gnc-plugin-page-account-tree.h"
#include "gnc-plugin-page-register.h"

/* for gnc_gconf_ */
#include "gnc-gconf-utils.h"
#include "hbci-interaction.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = "gnucash-hbci";

static void gnc_plugin_hbci_class_init (GncPluginHbciClass *klass);
static void gnc_plugin_hbci_init (GncPluginHbci *plugin);
static void gnc_plugin_hbci_finalize (GObject *object);

static void gnc_plugin_hbci_add_to_window (GncPlugin *plugin,
					   GncMainWindow *window,
					   GQuark type);
static void gnc_plugin_hbci_remove_from_window (GncPlugin *plugin,
						GncMainWindow *window,
						GQuark type);

/* Callbacks on other objects */
static void gnc_plugin_hbci_main_window_page_added   (GncMainWindow *window,
						      GncPluginPage *page);
static void gnc_plugin_hbci_main_window_page_changed (GncMainWindow *window,
						      GncPluginPage *page);
static void gnc_plugin_hbci_account_selected         (GncPluginPage *plugin_page,
						      Account *account,
						      gpointer user_data);

/* Command callbacks */
static void gnc_plugin_hbci_cmd_setup (GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_hbci_cmd_get_balance (GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_hbci_cmd_get_transactions (GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_hbci_cmd_issue_transaction (GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_hbci_cmd_issue_inttransaction (GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_hbci_cmd_issue_direct_debit (GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_hbci_cmd_mt940_import (GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_hbci_cmd_mt942_import (GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_hbci_cmd_dtaus_import (GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_hbci_cmd_csv_import (GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_hbci_cmd_dtaus_importsend (GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_hbci_cmd_csv_importsend (GtkAction *action, GncMainWindowActionData *data);


#define PLUGIN_ACTIONS_NAME "gnc-plugin-hbci-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-hbci-ui.xml"

static GtkActionEntry gnc_plugin_actions [] = {
  /* Menus */
  { "OnlineActionsAction", NULL, N_("_Online Actions"), NULL, NULL, NULL },

  /* Menu Items */
  { "HbciSetupAction", NULL, N_("_Online Banking Setup..."), NULL,
    N_("Initial setup of Online Banking access (HBCI, or OFX DirectConnect, using AqBanking)"),
    G_CALLBACK (gnc_plugin_hbci_cmd_setup) },
  { "HbciGetBalanceAction", NULL, N_("Get _Balance"), NULL,
    N_("Get the account balance online through Online Banking"),
    G_CALLBACK (gnc_plugin_hbci_cmd_get_balance) },
  { "HbciGetTransAction", NULL, N_("Get _Transactions..."), NULL,
    N_("Get the transactions online through Online Banking"),
    G_CALLBACK (gnc_plugin_hbci_cmd_get_transactions) },
  { "HbciIssueTransAction", NULL, N_("_Issue Transaction..."), NULL,
    N_("Issue a new transaction online through Online Banking"),
    G_CALLBACK (gnc_plugin_hbci_cmd_issue_transaction) },
  { "HbciIssueIntTransAction", NULL, N_("I_nternal Transaction..."), NULL,
    N_("Issue a new bank-internal transaction online through Online Banking"),
    G_CALLBACK (gnc_plugin_hbci_cmd_issue_inttransaction) },
  { "HbciIssueDirectDebitAction", NULL, N_("_Direct Debit..."), NULL,
    N_("Issue a new direct debit note online through Online Banking"),
    G_CALLBACK (gnc_plugin_hbci_cmd_issue_direct_debit) },

  /* File -> Import menu item */
  { "Mt940ImportAction", GTK_STOCK_CONVERT, N_("Import _MT940"), NULL,
    N_("Import a MT940 file into GnuCash"),
    G_CALLBACK (gnc_plugin_hbci_cmd_mt940_import) },
  { "Mt942ImportAction", GTK_STOCK_CONVERT, N_("Import MT94_2"), NULL,
    N_("Import a MT942 file into GnuCash"),
    G_CALLBACK (gnc_plugin_hbci_cmd_mt942_import) },
  { "DtausImportAction", GTK_STOCK_CONVERT, N_("Import _DTAUS"), NULL,
    N_("Import a DTAUS file into GnuCash"),
    G_CALLBACK (gnc_plugin_hbci_cmd_dtaus_import) },
#ifdef CSV_IMPORT_FUNCTIONAL
  { "CsvImportAction", GTK_STOCK_CONVERT, N_("Import _CSV"), NULL,
    N_("Import a CSV file into GnuCash"),
    G_CALLBACK (gnc_plugin_hbci_cmd_csv_import) },
  { "CsvImportSendAction", GTK_STOCK_CONVERT, N_("Import CSV and s_end..."), NULL,
    N_("Import a CSV file into GnuCash and send the transfers online through Online Banking"),
    G_CALLBACK (gnc_plugin_hbci_cmd_csv_importsend) },
#endif
  { "DtausImportSendAction", GTK_STOCK_CONVERT, N_("Import DTAUS and _send..."), NULL,
    N_("Import a DTAUS file into GnuCash and send the transfers online through Online Banking"),
    G_CALLBACK (gnc_plugin_hbci_cmd_dtaus_importsend) },

};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);

static const gchar *account_tree_actions[] = {
  "HbciSetupAction",
  "HbciGetBalanceAction",
  "HbciGetTransAction",
  "HbciIssueTransAction",
  "HbciIssueIntTransAction",
  "HbciIssueDirectDebitAction",
  NULL
};

static const gchar *register_actions[] = {
  "HbciSetupAction",
  "HbciGetBalanceAction",
  "HbciGetTransAction",
  "HbciIssueTransAction",
  "HbciIssueIntTransAction",
  "HbciIssueDirectDebitAction",
  NULL
};

static const gchar *need_account_actions[] = {
  "HbciGetBalanceAction",
  "HbciGetTransAction",
  "HbciIssueTransAction",
  "HbciIssueIntTransAction",
  "HbciIssueDirectDebitAction",
  NULL
};

typedef struct GncPluginHbciPrivate
{
  gpointer dummy;
} GncPluginHbciPrivate;

#define GNC_PLUGIN_HBCI_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_HBCI, GncPluginHbciPrivate))

static GObjectClass *parent_class = NULL;

/************************************************************
 *                   Object Implementation                  *
 ************************************************************/

GType
gnc_plugin_hbci_get_type (void)
{
  static GType gnc_plugin_hbci_type = 0;

  if (gnc_plugin_hbci_type == 0) {
    static const GTypeInfo our_info = {
      sizeof (GncPluginHbciClass),
		NULL,		/* base_init */
		NULL,		/* base_finalize */
		(GClassInitFunc) gnc_plugin_hbci_class_init,
		NULL,		/* class_finalize */
		NULL,		/* class_data */
		sizeof (GncPluginHbci),
		0,		/* n_preallocs */
		(GInstanceInitFunc) gnc_plugin_hbci_init,
    };

    gnc_plugin_hbci_type = g_type_register_static (GNC_TYPE_PLUGIN,
						   "GncPluginHbci",
						   &our_info, 0);
  }

  return gnc_plugin_hbci_type;
}

GncPlugin *
gnc_plugin_hbci_new (void)
{
  return GNC_PLUGIN (g_object_new (GNC_TYPE_PLUGIN_HBCI, NULL));
}

static void
gnc_plugin_hbci_class_init (GncPluginHbciClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

  parent_class = g_type_class_peek_parent (klass);

  object_class->finalize = gnc_plugin_hbci_finalize;

  /* plugin info */
  plugin_class->plugin_name  = GNC_PLUGIN_HBCI_NAME;

  /* widget addition/removal */
  plugin_class->actions_name  	   = PLUGIN_ACTIONS_NAME;
  plugin_class->actions       	   = gnc_plugin_actions;
  plugin_class->n_actions     	   = gnc_plugin_n_actions;
  plugin_class->ui_filename   	   = PLUGIN_UI_FILENAME;
  plugin_class->add_to_window 	   = gnc_plugin_hbci_add_to_window;
  plugin_class->remove_from_window = gnc_plugin_hbci_remove_from_window;

  g_type_class_add_private(klass, sizeof(GncPluginHbciPrivate));
}

static void
gnc_plugin_hbci_init (GncPluginHbci *plugin)
{
}

static void
gnc_plugin_hbci_finalize (GObject *object)
{
  GncPluginHbci *plugin;
  GncPluginHbciPrivate *priv;

  g_return_if_fail (GNC_IS_PLUGIN_HBCI (object));

  plugin = GNC_PLUGIN_HBCI (object);
  priv = GNC_PLUGIN_HBCI_GET_PRIVATE(plugin);

  G_OBJECT_CLASS (parent_class)->finalize (object);
}

/*
 * The gnc_plugin_add_to_window() function has already added our
 * actions to the main window.  HBCI include this function so that it
 * can attach callbacks to the window and track page changes within
 * each window.  Sneaky, huh?
 */
static void
gnc_plugin_hbci_add_to_window (GncPlugin *plugin,
			       GncMainWindow *window,
			       GQuark type)
{
  g_signal_connect (G_OBJECT(window), "page_added",
		    G_CALLBACK (gnc_plugin_hbci_main_window_page_added),
		    plugin);
  g_signal_connect (G_OBJECT(window), "page_changed",
		    G_CALLBACK (gnc_plugin_hbci_main_window_page_changed),
		    plugin);
}

static void
gnc_plugin_hbci_remove_from_window (GncPlugin *plugin,
				    GncMainWindow *window,
				    GQuark type)
{
  g_signal_handlers_disconnect_by_func(G_OBJECT(window),
				       G_CALLBACK (gnc_plugin_hbci_main_window_page_changed),
				       plugin);
  g_signal_handlers_disconnect_by_func(G_OBJECT(window),
				       G_CALLBACK (gnc_plugin_hbci_main_window_page_added),
				       plugin);
}

/************************************************************
 *                    Auxiliary Functions                   *
 ************************************************************/

/** Given a pointer to a main window, try and extract an Account from
 *  it.  If the current page is an "account tree" page, get the
 *  account corresponding to the selected account.  (What if multiple
 *  accounts are selected?)  If the current page is a "register" page,
 *  get the head account for the register. (Returns NULL for a general
 *  ledger or search register.)
 *
 *  @param window A pointer to a GncMainWindow object.
 *
 *  @return A pointer to an account, if one can be determined from the
 *  current page. NULL otherwise. */
static Account *
main_window_to_account (GncMainWindow *window)
{
  GncPluginPage  *page;
  const gchar    *page_name;
  Account        *account = NULL;
  const gchar    *account_name;

  ENTER("main window %p", window);
  if (!GNC_IS_MAIN_WINDOW(window)) {
    LEAVE("no main_window");
    return NULL;
  }

  /* Ensure we are called from a register page. */
  page = gnc_main_window_get_current_page(window);
  if (!GNC_IS_PLUGIN_PAGE(page)) {
    LEAVE("no plugin_page");
    return NULL;
  }
  page_name = gnc_plugin_page_get_plugin_name(page);
  if (!page_name) {
    LEAVE("no page_name of plugin_page");
    return NULL;
  }

  if (safe_strcmp(page_name, GNC_PLUGIN_PAGE_REGISTER_NAME) == 0) {
    DEBUG("register page");
    account =
      gnc_plugin_page_register_get_account (GNC_PLUGIN_PAGE_REGISTER(page));
  } else if (safe_strcmp(page_name, GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME) == 0) {
    DEBUG("account tree page");
    account =
      gnc_plugin_page_account_tree_get_current_account (GNC_PLUGIN_PAGE_ACCOUNT_TREE(page));
  } else {
    account = NULL;
  }
  account_name = account ? xaccAccountGetName(account) : NULL;
  LEAVE("account %s(%p)", account_name ? account_name : "(null)", account);
  return account;
}

/************************************************************
 *                     Object Callbacks                     *
 ************************************************************/

/** An account had been (de)selected in an "account tree" page.
 *  Update the hbci mennus appropriately. */
static void
gnc_plugin_hbci_account_selected (GncPluginPage *plugin_page,
				  Account *account,
				  gpointer user_data)
{
  GtkActionGroup *action_group;
  GncMainWindow  *window;

  g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));
  window = GNC_MAIN_WINDOW(plugin_page->window);
  g_return_if_fail (GNC_IS_MAIN_WINDOW (window));
  action_group = gnc_main_window_get_action_group(window, PLUGIN_ACTIONS_NAME);
  g_return_if_fail (GTK_IS_ACTION_GROUP (action_group));
  gnc_plugin_update_actions(action_group, need_account_actions,
			    "sensitive", account != NULL);
}

/** A new page has been added to a main window.  Connect a signal to
 *  it so that hbci can track when accounts are selected. */
static void
gnc_plugin_hbci_main_window_page_added (GncMainWindow *window,
					GncPluginPage *page)
{
  const gchar    *page_name;

  ENTER("main window %p, page %p", window, page);
  if (!GNC_IS_PLUGIN_PAGE(page)) { 
    LEAVE("no plugin_page");
    return;
  }

  page_name = gnc_plugin_page_get_plugin_name(page);
  if (!page_name) {
    LEAVE("no page_name of plugin_page");
    return;
  }

  if (safe_strcmp(page_name, GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME) == 0) {
    DEBUG("account tree page, adding signal");
    g_signal_connect (G_OBJECT(page),
		      "account_selected",
		      G_CALLBACK (gnc_plugin_hbci_account_selected),
		      NULL);

  }
  LEAVE(" ");
}

/** Whenever the current page has changed, update the hbci menus based
 *  upon the page that is currently selected. */
static void
gnc_plugin_hbci_main_window_page_changed (GncMainWindow *window,
					  GncPluginPage *page)
{
  GtkActionGroup *action_group;
  const gchar    *page_name;
  Account        *account;

  ENTER("main window %p, page %p", window, page);
  if (!GNC_IS_MAIN_WINDOW (window)) {
    LEAVE("no main_window");
    return;
  }

  action_group = gnc_main_window_get_action_group(window,PLUGIN_ACTIONS_NAME);
  if (!GTK_IS_ACTION_GROUP (action_group)) {
    LEAVE("no action_group");
    return;
  }

  /* Reset everything to known state */
  gnc_plugin_update_actions(action_group, need_account_actions,
			    "sensitive", FALSE);
  gnc_plugin_update_actions(action_group, account_tree_actions,
			    "visible", FALSE);
  gnc_plugin_update_actions(action_group, register_actions,
			    "visible", FALSE);

  /* Any page selected? */
  if (page == NULL) {
    LEAVE("no page");
    return;
  }

  /* Selectively make items visible */
  page_name = gnc_plugin_page_get_plugin_name(page);
  g_return_if_fail (page_name);
  if (safe_strcmp(page_name, GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME) == 0) {
    DEBUG("account tree page");
    gnc_plugin_update_actions(action_group, account_tree_actions,
			      "visible", TRUE);
  } else if (safe_strcmp(page_name, GNC_PLUGIN_PAGE_REGISTER_NAME) == 0) {
    DEBUG("register page");
    gnc_plugin_update_actions(action_group, register_actions,
			      "visible", TRUE);
  }

  /* Only make items sensitive if an account can be determined */
  account = main_window_to_account (window);
  if (account) {
    gnc_plugin_update_actions(action_group, need_account_actions,
			      "sensitive", TRUE);
  }
  LEAVE(" ");
}
/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_hbci_cmd_setup (GtkAction *action,
			   GncMainWindowActionData *data)
{
  ENTER("action %p, main window data %p", action, data);
  gnc_hbci_initial_druid ();
  LEAVE(" ");
}

static void
gnc_plugin_hbci_cmd_get_balance (GtkAction *action,
				 GncMainWindowActionData *data)
{
  Account *account;

  ENTER("action %p, main window data %p", action, data);
  account = main_window_to_account(data->window);
  if (account == NULL) {
    LEAVE("no account");
    return;
  }

  gnc_hbci_getbalance(GTK_WIDGET(data->window), account);
  LEAVE(" ");
}

static void
gnc_plugin_hbci_cmd_get_transactions (GtkAction *action,
				      GncMainWindowActionData *data)
{
  Account *account;

  ENTER("action %p, main window data %p", action, data);
  account = main_window_to_account(data->window);
  if (account == NULL) {
    LEAVE("no account");
    return;
  }

  gnc_hbci_gettrans(GTK_WIDGET(data->window), account);
  LEAVE(" ");
}

static void
gnc_plugin_hbci_cmd_issue_transaction (GtkAction *action,
					GncMainWindowActionData *data)
{
  Account *account;

  ENTER("action %p, main window data %p", action, data);
  account = main_window_to_account(data->window);
  if (account == NULL) {
    LEAVE("no account");
    return;
  }

  gnc_hbci_maketrans(GTK_WIDGET(data->window), account, SINGLE_TRANSFER);
  LEAVE(" ");
}

static void
gnc_plugin_hbci_cmd_issue_inttransaction (GtkAction *action,
					GncMainWindowActionData *data)
{
  Account *account;

  ENTER("action %p, main window data %p", action, data);
  account = main_window_to_account(data->window);
  if (account == NULL) {
    LEAVE("no account");
    return;
  }

  gnc_hbci_maketrans(GTK_WIDGET(data->window), account, SINGLE_INTERNAL_TRANSFER);
  LEAVE(" ");
}

static void
gnc_plugin_hbci_cmd_issue_direct_debit (GtkAction *action,
					GncMainWindowActionData *data)
{
  Account *account;

  ENTER("action %p, main window data %p", action, data);
  account = main_window_to_account(data->window);
  if (account == NULL) {
    LEAVE("no account");
    return;
  }
  gnc_hbci_maketrans (GTK_WIDGET(data->window), account, SINGLE_DEBITNOTE);
  LEAVE(" ");
}

static void
gnc_plugin_hbci_cmd_dtaus_importsend (GtkAction *action,
				  GncMainWindowActionData *data)
{
  char *format = gnc_gconf_get_string(GCONF_SECTION, KEY_FORMAT_DTAUS, NULL);
  gnc_file_aqbanking_import ("dtaus", format ? format : "default", TRUE);
  g_free (format);
}
static void
gnc_plugin_hbci_cmd_csv_importsend (GtkAction *action,
				  GncMainWindowActionData *data)
{
  char *format = gnc_gconf_get_string(GCONF_SECTION, KEY_FORMAT_CSV, NULL);
  gnc_file_aqbanking_import ("csv", format ? format : "default", TRUE);
  g_free (format);
}

static void
gnc_plugin_hbci_cmd_mt940_import (GtkAction *action,
				  GncMainWindowActionData *data)
{
  char *format = gnc_gconf_get_string(GCONF_SECTION, KEY_FORMAT_SWIFT940, NULL);
  gnc_file_aqbanking_import ("swift", format ? format : "swift-mt940", FALSE);
  g_free (format);
}
static void
gnc_plugin_hbci_cmd_mt942_import (GtkAction *action,
				  GncMainWindowActionData *data)
{
  char *format = gnc_gconf_get_string(GCONF_SECTION, KEY_FORMAT_SWIFT942, NULL);
  gnc_file_aqbanking_import ("swift", format ? format : "swift-mt942", FALSE);
  g_free (format);
}
static void
gnc_plugin_hbci_cmd_dtaus_import (GtkAction *action,
				  GncMainWindowActionData *data)
{
  char *format = gnc_gconf_get_string(GCONF_SECTION, KEY_FORMAT_DTAUS, NULL);
  gnc_file_aqbanking_import ("dtaus", format ? format : "default", FALSE);
  g_free (format);
}
static void
gnc_plugin_hbci_cmd_csv_import (GtkAction *action,
				  GncMainWindowActionData *data)
{
  char *format = gnc_gconf_get_string(GCONF_SECTION, KEY_FORMAT_CSV, NULL);
  gnc_file_aqbanking_import ("csv", format ? format : "default", FALSE);
  g_free (format);
}
/************************************************************
 *                    Plugin Bootstrapping                   *
 ************************************************************/

void
gnc_plugin_hbci_create_plugin (void)
{
  GncPlugin *plugin = gnc_plugin_hbci_new ();

  gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), plugin);
}
