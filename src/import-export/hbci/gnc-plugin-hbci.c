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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#include "config.h"

#include "druid-hbci-initial.h"
#include "egg-action-group.h"
#include "gnc-plugin-manager.h"
#include "gnc-gnome-utils.h"
#include "gnc-hbci-getbalance.h"
#include "gnc-hbci-gettrans.h"
#include "gnc-hbci-transfer.h"
#include "gnc-plugin-hbci.h"
#include "gnc-plugin-manager.h"
#include "gnc-plugin-page-account-tree.h"
#include "gnc-plugin-page-register.h"
#include "gnc-trace.h"
#include "messages.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_HBCI;
static GList *active_plugins = NULL;

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
static void gnc_plugin_hbci_cmd_setup (EggAction *action, GncMainWindowActionData *data);
static void gnc_plugin_hbci_cmd_get_balance (EggAction *action, GncMainWindowActionData *data);
static void gnc_plugin_hbci_cmd_get_transactions (EggAction *action, GncMainWindowActionData *data);
static void gnc_plugin_hbci_cmd_issue_transaction (EggAction *action, GncMainWindowActionData *data);
static void gnc_plugin_hbci_cmd_issue_direct_debit (EggAction *action, GncMainWindowActionData *data);


#define PLUGIN_ACTIONS_NAME "gnc-plugin-hbci-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-hbci-ui.xml"

static EggActionEntry gnc_plugin_actions [] = {
  /* Menus */
  { "OnlineActionsAction", N_("_Online Actions"), NULL, NULL, NULL, NULL },

  /* Menu Items */
  { "HbciSetupAction", N_("HBCI Setup..."), NULL, NULL,
    N_("Gather initial HBCI information"),
    G_CALLBACK (gnc_plugin_hbci_cmd_setup) },
  { "HbciGetBalanceAction", N_("HBCI Get _Balance"), NULL, NULL,
    N_("Get the account balance online through HBCI"),
    G_CALLBACK (gnc_plugin_hbci_cmd_get_balance) },
  { "HbciGetTransAction", N_("HBCI Get _Transactions"), NULL, NULL,
    N_("Get the transactions online through HBCI"),
    G_CALLBACK (gnc_plugin_hbci_cmd_get_transactions) },
  { "HbciIssueTransAction", N_("HBCI _Issue Transactions"), NULL, NULL,
    N_("Issue a new transaction online through HBCI"),
    G_CALLBACK (gnc_plugin_hbci_cmd_issue_transaction) },
  { "HbciIssueDirectDebitAction", N_("HBCI Issue _Direct Debit"), NULL, NULL,
    N_("Issue a new direct debit note online through HBCI"),
    G_CALLBACK (gnc_plugin_hbci_cmd_issue_direct_debit) },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);


static const gchar *account_tree_actions[] = {
  "HbciSetupAction",
  "HbciGetBalanceAction",
  "HbciGetTransAction",
  NULL
};

static const gchar *register_actions[] = {
  "HbciGetBalanceAction",
  "HbciGetTransAction",
  "HbciIssueTransAction",
  "HbciIssueDirectDebitAction",
  NULL
};

static const gchar *need_account_actions[] = {
  "HbciGetBalanceAction",
  "HbciGetTransAction",
  "HbciIssueTransAction",
  "HbciIssueDirectDebitAction",
  NULL
};

struct GncPluginHbciPrivate
{
  gpointer dummy;
};

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

#if DEBUG_REFERENCE_COUNTING
static void
dump_model (GncPluginHbci *plugin, gpointer dummy)
{
    g_warning("GncPluginHbci %p still exists.", plugin);
}

static gint
gnc_plugin_hbci_report_references (void)
{
  g_list_foreach(active_plugins, (GFunc)dump_model, NULL);
  return 0;
}
#endif

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

#if DEBUG_REFERENCE_COUNTING
	gtk_quit_add (0,
		      (GtkFunction)gnc_plugin_hbci_report_references,
		      NULL);
#endif
}

static void
gnc_plugin_hbci_init (GncPluginHbci *plugin)
{
  plugin->priv = g_new0 (GncPluginHbciPrivate, 1);

  active_plugins = g_list_append (active_plugins, plugin);
}

static void
gnc_plugin_hbci_finalize (GObject *object)
{
  GncPluginHbci *plugin;

  g_return_if_fail (GNC_IS_PLUGIN_HBCI (object));

  plugin = GNC_PLUGIN_HBCI (object);
  active_plugins = g_list_remove (active_plugins, plugin);

  g_return_if_fail (plugin->priv != NULL);

  g_free (plugin->priv);

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

  ENTER("main window %p", window);
  g_return_val_if_fail (GNC_IS_MAIN_WINDOW(window), NULL);

  /* Ensure we are called from a register page. */
  page = gnc_main_window_get_current_page(window);
  page_name = gnc_plugin_page_get_name(page);

  if (strcmp(page_name, GNC_PLUGIN_PAGE_REGISTER_NAME) == 0) {
    DEBUG("register page");
    account =
      gnc_plugin_page_register_get_account (GNC_PLUGIN_PAGE_REGISTER(page));
  } else if (strcmp(page_name, GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME) == 0) {
    DEBUG("account tree page");
    account =
      gnc_plugin_page_account_tree_get_current_account (GNC_PLUGIN_PAGE_ACCOUNT_TREE(page));
  } else {
    account = NULL;
  }
  LEAVE("account %s(%p)", xaccAccountGetName(account), account);
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
  EggActionGroup *action_group;
  GncMainWindow  *window;

  window = GNC_MAIN_WINDOW(plugin_page->window);
  action_group = gnc_main_window_get_action_group(window, PLUGIN_ACTIONS_NAME);
  gnc_gnome_utils_update_actions(action_group, need_account_actions,
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
  page_name = gnc_plugin_page_get_name(page);
  if (strcmp(page_name, GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME) == 0) {
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
  EggActionGroup *action_group;
  const gchar    *page_name;
  Account        *account;

  ENTER("main window %p, page %p", window, page);
  action_group = gnc_main_window_get_action_group(window,PLUGIN_ACTIONS_NAME);
  g_return_if_fail(action_group != NULL);

  /* Reset everything to known state */
  gnc_gnome_utils_update_actions(action_group, need_account_actions,
				 "sensitive", FALSE);
  gnc_gnome_utils_update_actions(action_group, account_tree_actions,
				 "visible", FALSE);
  gnc_gnome_utils_update_actions(action_group, register_actions,
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
  } else if (strcmp(page_name, GNC_PLUGIN_PAGE_REGISTER_NAME) == 0) {
    DEBUG("register page");
    gnc_gnome_utils_update_actions(action_group, register_actions,
				   "visible", TRUE);
  }

  /* Only make items sensitive is an account can be determined */
  account = main_window_to_account (window);
  if (account) {
    gnc_gnome_utils_update_actions(action_group, need_account_actions,
				   "sensitive", TRUE);
  }
  LEAVE(" ");
}
/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_hbci_cmd_setup (EggAction *action,
			   GncMainWindowActionData *data)
{
  ENTER("action %p, main window data %p", action, data);
  gnc_hbci_initial_druid ();
  LEAVE(" ");
}

static void
gnc_plugin_hbci_cmd_get_balance (EggAction *action,
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
gnc_plugin_hbci_cmd_get_transactions (EggAction *action,
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
gnc_plugin_hbci_cmd_issue_transaction (EggAction *action,
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
gnc_plugin_hbci_cmd_issue_direct_debit (EggAction *action,
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

/************************************************************
 *                    Plugin Bootstrapping                   *
 ************************************************************/

void
gnc_plugin_hbci_create_plugin (void)
{
  GncPlugin *plugin = gnc_plugin_hbci_new ();

  gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), plugin);
}
