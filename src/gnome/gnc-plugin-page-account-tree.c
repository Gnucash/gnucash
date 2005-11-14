/* 
 * gnc-plugin-page-account-tree.c -- 
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003,2005 David Hampton <hampton@employees.org>
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

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup GncPluginPageAccountTree An Account Tree Plugin
    @{ */
/** @file gnc-plugin-page-account-tree.c
    @brief  utility functions for the GnuCash UI
    @author Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
            Copyright (C) 2003,2005 David Hampton <hampton@employees.org>
*/

#include "config.h"

#include <gtk/gtk.h>
#ifndef HAVE_GLIB26
#include "gkeyfile.h"
#endif
#include "gnc-plugin-page-account-tree.h"
#include "gnc-plugin-page-register.h"

#include "Scrub.h"
#include "Transaction.h"
#include "dialog-account.h"
#include "dialog-options.h"
#include "dialog-transfer.h"
#include "druid-merge.h"
#include "gnc-component-manager.h"
#include "gnc-gnome-utils.h"
#include "gnc-html.h"
#include "gnc-icons.h"
#include "gnc-plugin-account-tree.h"
#include "gnc-session.h"
#include "gnc-split-reg.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "lot-viewer.h"
#include "option-util.h"
#include "window-reconcile.h"
#include "window-main-summarybar.h"

#include "messages.h"
#include "gnc-engine.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

#define PLUGIN_PAGE_ACCT_TREE_CM_CLASS "plugin-page-acct-tree"
#define GCONF_SECTION "window/pages/account_tree"

enum {
  ACCOUNT_SELECTED,
  LAST_SIGNAL
};

typedef struct GncPluginPageAccountTreePrivate
{
	GtkWidget *widget;
	GtkTreeView *tree_view;

	GNCOptionDB * odb;
	SCM         options; 
	int         options_id;
	GNCOptionWin * editor_dialog;

	GtkWidget *options_db;
	gint       component_id;
} GncPluginPageAccountTreePrivate;

#define GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_PAGE_ACCOUNT_TREE, GncPluginPageAccountTreePrivate))

static GObjectClass *parent_class = NULL;

/************************************************************
 *                        Prototypes                        *
 ************************************************************/
/* Plugin Actions */
static void gnc_plugin_page_account_tree_class_init (GncPluginPageAccountTreeClass *klass);
static void gnc_plugin_page_account_tree_init (GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_finalize (GObject *object);

static GtkWidget *gnc_plugin_page_account_tree_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_account_tree_destroy_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_account_tree_save_page (GncPluginPage *plugin_page, GKeyFile *file, const gchar *group);
static GncPluginPage *gnc_plugin_page_account_tree_recreate_page (GtkWidget *window, GKeyFile *file, const gchar *group);

/* Callbacks */
static gboolean gnc_plugin_page_account_tree_button_press_cb (GtkWidget *widget,
							      GdkEventButton *event,
			       				      GncPluginPage *page);
static void gnc_plugin_page_account_tree_double_click_cb (GtkTreeView        *treeview,
							  GtkTreePath        *path,
							  GtkTreeViewColumn  *col,
							  GncPluginPageAccountTree *page);

static void gnc_plugin_page_account_tree_selection_changed_cb (GtkTreeSelection *selection,
							       GncPluginPageAccountTree *page);

/* Command callbacks */
static void gnc_plugin_page_account_tree_cmd_new_account (GtkAction *action, GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_cmd_file_hierarchy_merge (GtkAction *action, GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_cmd_open_account (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_open_subaccounts (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_edit_account (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_delete_account (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_view_options (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_reconcile (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_transfer (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_stock_split (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_lots (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_scrub (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_scrub_sub (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_scrub_all (GtkAction *action, GncPluginPageAccountTree *page);


static void gnc_plugin_page_acct_tree_options_new(GncPluginPageAccountTreePrivate *priv);


static guint plugin_page_signals[LAST_SIGNAL] = { 0 };


static GtkActionEntry gnc_plugin_page_account_tree_actions [] = {
	/* Toplevel */
	{ "FakeToplevel", NULL, "", NULL, NULL, NULL },

	/* File menu */
	{ "FileNewAccountAction", GNC_STOCK_NEW_ACCOUNT, N_("New _Account..."), NULL,
	  N_("Create a new Account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_new_account) },
	{ "FileAddAccountHierarchyDruidAction", GNC_STOCK_NEW_ACCOUNT, N_("New Account _Hierarchy..."), NULL,
	  N_("Extend the current book by merging with new account type categories"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_file_hierarchy_merge) },
	{ "FileOpenAccountAction", GNC_STOCK_OPEN_ACCOUNT, N_("Open _Account"), NULL,
	  N_("Open the selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_open_account) },
	{ "FileOpenSubaccountsAction", GNC_STOCK_OPEN_ACCOUNT, N_("Open _Subaccounts"), NULL,
	  N_("Open the selected account and all its subaccounts"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_open_subaccounts) },

	/* Edit menu */
	{ "EditEditAccountAction", GNC_STOCK_EDIT_ACCOUNT, N_("_Edit Account"), "<control>e",
	  N_("Edit the selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_edit_account) },
	{ "EditDeleteAccountAction", GNC_STOCK_DELETE_ACCOUNT, N_("_Delete Account"), NULL,
	  N_("Delete selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_delete_account) },
	{ "EditAccountViewOptionsAction", GTK_STOCK_PROPERTIES, N_("Account Tree _Options"), NULL,
	  N_("Edit the account view options"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_view_options) },

	/* Actions menu */
	{ "ActionsReconcileAction", NULL, N_("_Reconcile..."), "<control>r",
	  N_("Reconcile the selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_reconcile) },
	{ "ActionsTransferAction", NULL, N_("_Transfer..."), "<control>t",
	  N_("Transfer funds from one account to another"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_transfer) },
	{ "ActionsStockSplitAction", NULL, N_("Stoc_k Split..."), NULL,
	  N_("Record a stock split or a stock merger"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_stock_split) },
	{ "ActionsLotsAction", NULL, N_("View _Lots..."), NULL,
	  N_("Bring up the lot viewer/editor window"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_lots) },
	{ "ScrubAction", NULL, N_("Check & Repair A_ccount"), NULL,
	  N_("Check for and repair unbalanced transactions and orphan splits " "in this account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_scrub) },
	{ "ScrubSubAction", NULL, N_("Check & Repair Su_baccount"), NULL,
	  N_("Check for and repair unbalanced transactions and orphan splits "
             "in this account and its subaccounts"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_scrub_sub) },
	{ "ScrubAllAction", NULL, N_("Check & Repair A_ll"), NULL,
	  N_("Check for and repair unbalanced transactions and orphan splits " "in all accounts"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_scrub_all) },

        /* Popup menu */

	{ "PopupOptionsAction", GTK_STOCK_PROPERTIES, N_("_Options"), NULL,
	  N_("Edit the account view options"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_view_options) },
};
static guint gnc_plugin_page_account_tree_n_actions = G_N_ELEMENTS (gnc_plugin_page_account_tree_actions);

static const gchar *actions_requiring_account[] = {
	"FileOpenAccountAction",
	"FileOpenSubaccountsAction",
	"EditEditAccountAction",
	"EditDeleteAccountAction",
	"ActionsReconcileAction",
	"ActionsLotsAction",
	NULL
};


/** Short labels for use on the toolbar buttons. */
static action_toolbar_labels toolbar_labels[] = {
  { "FileOpenAccountAction", 	    N_("Open") },
  { "EditEditAccountAction", 	    N_("Edit") },
  { "EditAccountViewOptionsAction", N_("Options") },
  { "FileNewAccountAction",    	    N_("New") },
  { "EditDeleteAccountAction", 	    N_("Delete") },
  { NULL, NULL },
};


GType
gnc_plugin_page_account_tree_get_type (void)
{
	static GType gnc_plugin_page_account_tree_type = 0;

	if (gnc_plugin_page_account_tree_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncPluginPageAccountTreeClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_plugin_page_account_tree_class_init,
			NULL,
			NULL,
			sizeof (GncPluginPageAccountTree),
			0,
			(GInstanceInitFunc) gnc_plugin_page_account_tree_init
		};
		
		gnc_plugin_page_account_tree_type = g_type_register_static (GNC_TYPE_PLUGIN_PAGE,
								            GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME,
								            &our_info, 0);
	}

	return gnc_plugin_page_account_tree_type;
}

GncPluginPage *
gnc_plugin_page_account_tree_new (void)
{
	GncPluginPageAccountTree *plugin_page;

	ENTER(" ");
	plugin_page = g_object_new (GNC_TYPE_PLUGIN_PAGE_ACCOUNT_TREE,
			      NULL);

	LEAVE("new account tree page %p", plugin_page);
	return GNC_PLUGIN_PAGE (plugin_page);
}

static void
gnc_plugin_page_account_tree_class_init (GncPluginPageAccountTreeClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GncPluginPageClass *gnc_plugin_class = GNC_PLUGIN_PAGE_CLASS(klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_plugin_page_account_tree_finalize;

	gnc_plugin_class->tab_icon        = GNC_STOCK_ACCOUNT;
	gnc_plugin_class->plugin_name     = GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME;
	gnc_plugin_class->create_widget   = gnc_plugin_page_account_tree_create_widget;
	gnc_plugin_class->destroy_widget  = gnc_plugin_page_account_tree_destroy_widget;
	gnc_plugin_class->save_page       = gnc_plugin_page_account_tree_save_page;
	gnc_plugin_class->recreate_page   = gnc_plugin_page_account_tree_recreate_page;

	g_type_class_add_private(klass, sizeof(GncPluginPageAccountTreePrivate));

	plugin_page_signals[ACCOUNT_SELECTED] =
	  g_signal_new ("account_selected",
			G_OBJECT_CLASS_TYPE (object_class),
			G_SIGNAL_RUN_FIRST,
			G_STRUCT_OFFSET (GncPluginPageAccountTreeClass, account_selected),
			NULL, NULL,
			g_cclosure_marshal_VOID__POINTER,
			G_TYPE_NONE, 1,
			G_TYPE_POINTER);
}

static void
gnc_plugin_page_account_tree_init (GncPluginPageAccountTree *plugin_page)
{
	GtkActionGroup *action_group;
	GncPluginPageAccountTreePrivate *priv;
	GncPluginPage *parent;
	const gchar *url = NULL;
	int options_id;
	SCM find_options;
	SCM temp;
	URLType type;

	ENTER("page %p", plugin_page);
	priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(plugin_page);

	/* Init parent declared variables */
	parent = GNC_PLUGIN_PAGE(plugin_page);
	g_object_set(G_OBJECT(plugin_page),
		     "page-name",      _("Accounts"),
		     "page-uri",       "default:",
		     "ui-description", "gnc-plugin-page-account-tree-ui.xml",
		     NULL);

	/* change me when the system supports multiple books */
	gnc_plugin_page_add_book(parent, gnc_get_current_book());

	/* Create menu and toolbar information */
	action_group =
	  gnc_plugin_page_create_action_group(parent,
					      "GncPluginPageAccountTreeActions");
	gtk_action_group_add_actions(action_group,
				     gnc_plugin_page_account_tree_actions,
				     gnc_plugin_page_account_tree_n_actions,
				     plugin_page);
	gnc_plugin_init_short_names (action_group, toolbar_labels);

	
	/* get the options and the window ID */ 
	priv->options = SCM_BOOL_F;
	scm_gc_protect_object(priv->options);
	priv->editor_dialog = NULL;

	if(!url) {
	  gnc_plugin_page_acct_tree_options_new(priv);
	} else {
	  char * location = NULL;
	  char * label = NULL;

	  /* if an URL is specified, it should look like 
	   * gnc-acct-tree:id=17 .  We want to get the number out,
	   * then look up the options in the global DB. */
	  type = gnc_html_parse_url(NULL, url, &location, &label);
	  if (!safe_strcmp (type, URL_TYPE_ACCTTREE) &&
	      location && (strlen(location) > 3) && 
	      !strncmp("id=", location, 3)) {
	    sscanf(location+3, "%d", &options_id);
	    find_options = scm_c_eval_string("gnc:find-acct-tree-window-options");
	    temp = scm_call_1(find_options, scm_int2num(options_id));

	    if(temp != SCM_BOOL_F) {
	      scm_gc_unprotect_object(priv->options);
	      priv->options = temp;
	      scm_gc_protect_object(priv->options);
	      priv->options_id = options_id;
	    } else {
	      gnc_plugin_page_acct_tree_options_new(priv);
	    }
	  } else {
	    gnc_plugin_page_acct_tree_options_new(priv);
	  }

	  g_free (location);
	  g_free (label);
	}

	priv->odb     = gnc_option_db_new(priv->options);

	LEAVE("page %p, priv %p, action group %p",
	      plugin_page, priv, action_group);
}

static void
gnc_plugin_page_account_tree_finalize (GObject *object)
{
	GncPluginPageAccountTree *page;
	GncPluginPageAccountTreePrivate *priv;
	SCM  free_tree;

	ENTER("object %p", object);
	page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (object);
	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
	priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
	g_return_if_fail (priv != NULL);

	if (priv->editor_dialog) {
	  gnc_options_dialog_destroy(priv->editor_dialog);
	  priv->editor_dialog = NULL;
	}

	gnc_option_db_destroy(priv->odb);

	free_tree = scm_c_eval_string("gnc:free-acct-tree-window");
	scm_call_1(free_tree, scm_int2num(priv->options_id));
	priv->options_id = 0;

	scm_gc_unprotect_object(priv->options);

	G_OBJECT_CLASS (parent_class)->finalize (object);
	LEAVE(" ");
}

Account *
gnc_plugin_page_account_tree_get_current_account (GncPluginPageAccountTree *page)
{
	GncPluginPageAccountTreePrivate *priv;
	Account *account;

	priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
	ENTER("page %p (tree view %p)", page, priv->tree_view);
	account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(priv->tree_view));
	if (account == NULL) {
		LEAVE("no account");
		return NULL;
	}

	LEAVE("account %p", account);
	return account;
}


/* Virtual Functions */

static void
gnc_plugin_page_account_refresh_cb (GHashTable *changes, gpointer user_data)
{
  GncPluginPageAccountTree *page = user_data;
  GncPluginPageAccountTreePrivate *priv;

  g_return_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page));

  /* We're only looking for forced updates here. */
  if (changes)
    return;

  priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
  gtk_widget_queue_draw(priv->widget);
}

static void
gnc_plugin_page_account_tree_close_cb (gpointer user_data)
{
  GncPluginPage *plugin_page;
  GncPluginPageAccountTree *page;

  plugin_page = GNC_PLUGIN_PAGE(user_data);
  page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page);
  gnc_main_window_close_page(plugin_page);
}

static GtkWidget *
gnc_plugin_page_account_tree_create_widget (GncPluginPage *plugin_page)
{
	GncPluginPageAccountTree *page;
	GncPluginPageAccountTreePrivate *priv;
	GtkTreeSelection *selection;
	GtkTreeView *tree_view;
	GtkWidget *scrolled_window;

	ENTER("page %p", plugin_page);
	page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page);
	priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
	if (priv->widget != NULL) {
		LEAVE("widget = %p", priv->widget);
		return priv->widget;
	}

	priv->widget = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (priv->widget);

	scrolled_window = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
					GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_show (scrolled_window);
	gtk_box_pack_start (GTK_BOX (priv->widget), scrolled_window,
			    TRUE, TRUE, 0);

	tree_view = gnc_tree_view_account_new(FALSE);
	g_object_set(G_OBJECT(tree_view),
		     "gconf-section", GCONF_SECTION,
		     "show-column-menu", TRUE,
		     NULL);

	priv->tree_view = tree_view;
	selection = gtk_tree_view_get_selection(tree_view);
	g_signal_connect (G_OBJECT (selection), "changed",
			  G_CALLBACK (gnc_plugin_page_account_tree_selection_changed_cb), page);
	g_signal_connect (G_OBJECT (tree_view), "button-press-event",
			  G_CALLBACK (gnc_plugin_page_account_tree_button_press_cb), page);
	g_signal_connect (G_OBJECT (tree_view), "row-activated",
			  G_CALLBACK (gnc_plugin_page_account_tree_double_click_cb), page);

	gtk_tree_view_set_headers_visible(tree_view, TRUE);
	gnc_plugin_page_account_tree_selection_changed_cb (NULL, page);
	gtk_widget_show (GTK_WIDGET (tree_view));
	gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET(tree_view));

	priv->component_id =
	  gnc_register_gui_component(PLUGIN_PAGE_ACCT_TREE_CM_CLASS,
				     gnc_plugin_page_account_refresh_cb,
				     gnc_plugin_page_account_tree_close_cb,
				     page);
	gnc_gui_component_set_session (priv->component_id,
				       gnc_get_current_session());

	plugin_page->summarybar = gnc_main_window_summary_new();
	gtk_box_pack_end (GTK_BOX (priv->widget), plugin_page->summarybar,
			  FALSE, FALSE, 0);
	gtk_widget_show(plugin_page->summarybar);

	LEAVE("widget = %p", priv->widget);
	return priv->widget;
}

static void
gnc_plugin_page_account_tree_destroy_widget (GncPluginPage *plugin_page)
{
	GncPluginPageAccountTree *page;
	GncPluginPageAccountTreePrivate *priv;

	ENTER("page %p", plugin_page);
	page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page);
	priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);

	if (priv->widget) {
	  g_object_unref(G_OBJECT(priv->widget));
	  priv->widget = NULL;
	}

	if (priv->component_id) {
	  gnc_unregister_gui_component(priv->component_id);
	  priv->component_id = 0;
	}

	LEAVE("widget destroyed");
}

#define ACCT_COUNT "Number of Open Accounts"
#define ACCT_OPEN  "Open Account %d"
#define ACCT_SELECTED  "Selected Account"

typedef struct foo {
  GKeyFile *key_file;
  const gchar *group_name;
  int count;
} bar_t;


/** Save information about an expanded row.  This function is called
 *  via a gtk_tree_view_map_expanded_rows, which calls it once per
 *  expanded row.  Its job is to write the full account name of the
 *  row out to the state file.
 *
 *  @param tree_view A pointer to the GtkTreeView embedded in an
 *  account tree page.
 *
 *  @param path A pointer to a particular entry in the tree.
 *
 *  @param data A pointer to a data structure holding the information
 *  related to the state file. */
static void
tree_save_expanded_row (GtkTreeView *tree_view,
			GtkTreePath *path,
			gpointer user_data)
{
	Account *account;
	bar_t *bar = user_data;
	gchar *key;
	gchar *account_name;

	account = gnc_tree_view_account_get_account_from_path (GNC_TREE_VIEW_ACCOUNT(tree_view), path);
	if (account == NULL)
	  return;

	account_name = xaccAccountGetFullName (account, gnc_get_account_separator ());
	if (account_name == NULL)
	  return;

	key = g_strdup_printf(ACCT_OPEN, ++bar->count);
	g_key_file_set_string(bar->key_file, bar->group_name, key, account_name);
	g_free(key);
	g_free(account_name);
}


/** Save information about the selected row.  Its job is to write the
 *  full account name of the row out to the state file.
 *
 *  @param tree_view A pointer to the GtkTreeView embedded in an
 *  account tree page.
 *
 *  @param path A pointer to a particular entry in the tree.
 *
 *  @param data A pointer to a data structure holding the information
 *  related to the state file. */
static void
tree_save_selected_row (GncTreeViewAccount *view,
			gpointer user_data)
{
	Account *account;
	bar_t *bar = user_data;
	gchar *account_name;

	account = gnc_tree_view_account_get_selected_account(view);
	if (account == NULL)
	  return;

	account_name = xaccAccountGetFullName (account, gnc_get_account_separator ());
	if (account_name == NULL)
	  return;

	g_key_file_set_string(bar->key_file, bar->group_name, ACCT_SELECTED, account_name);
	g_free(account_name);
}


/** Save enough information about this account tree page that it can
 *  be recreated next time the user starts gnucash.
 *
 *  @param page The page to save.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be written.
 *
 *  @param group_name The group name to use when saving data. */
static void
gnc_plugin_page_account_tree_save_page (GncPluginPage *plugin_page,
					GKeyFile *key_file,
					const gchar *group_name)
{
	GncPluginPageAccountTree *account_page;
	GncPluginPageAccountTreePrivate *priv;
	bar_t bar;
	
	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(plugin_page));
	g_return_if_fail (key_file != NULL);
	g_return_if_fail (group_name != NULL);

	ENTER("page %p, key_file %p, group_name %s", plugin_page, key_file,
	      group_name);

	account_page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(plugin_page);
	priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(account_page);

	bar.key_file = key_file;
	bar.group_name = group_name;
	bar.count = 0;
	tree_save_selected_row(GNC_TREE_VIEW_ACCOUNT(priv->tree_view), &bar);
	gtk_tree_view_map_expanded_rows(priv->tree_view,
					tree_save_expanded_row, &bar);
	g_key_file_set_integer(key_file, group_name, ACCT_COUNT, bar.count);
	LEAVE(" ");
}


/** Expand a row in the tree that was expanded when the user last quit
 *  gnucash.  Its job is to map from account name to tree row and
 *  expand the row.
 *
 *  @param tree_view A pointer to the GtkTreeView embedded in an
 *  account tree page.
 *
 *  @param account_name A pointer to the full account name. */
static void
tree_restore_expanded_row (GtkTreeView *tree_view,
			   const gchar *account_name)
{
  Account *account;
  QofBook *book;

  book = qof_session_get_book(qof_session_get_current_session());
  account = xaccGetAccountFromFullName(xaccGetAccountGroup(book),
				       account_name,
				       gnc_get_account_separator());
  if (account)
    gnc_tree_view_account_expand_to_account(GNC_TREE_VIEW_ACCOUNT(tree_view),
					    account);
}


/** Select the row in the tree that was selected when the user last
 *  quit gnucash.  Its job is to map from account name to tree row and
 *  select the row.
 *
 *  @param tree_view A pointer to the GtkTreeView embedded in an
 *  account tree page.
 *
 *  @param account_name A pointer to the full account name. */
static void
tree_restore_selected_row (GtkTreeView *tree_view,
			   const gchar *account_name)
{
  Account *account;
  QofBook *book;

  book = qof_session_get_book(qof_session_get_current_session());
  account = xaccGetAccountFromFullName(xaccGetAccountGroup(book),
				       account_name,
				       gnc_get_account_separator());
  if (account)
    gnc_tree_view_account_set_selected_account(GNC_TREE_VIEW_ACCOUNT(tree_view),
					       account);
}


/** Create a new account tree page based on the information saved
 *  during a previous instantiation of gnucash.
 *
 *  @param window The window where this page should be installed.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be read.
 *
 *  @param group_name The group name to use when restoring data. */
static GncPluginPage *
gnc_plugin_page_account_tree_recreate_page (GtkWidget *window,
					    GKeyFile *key_file,
					    const gchar *group_name)
{
	GncPluginPageAccountTree *account_page;
	GncPluginPageAccountTreePrivate *priv;
	GncPluginPage *page;
	GError *error = NULL;
	gchar *key, *value;
	gint i, count;
	
	g_return_val_if_fail(key_file, NULL);
	g_return_val_if_fail(group_name, NULL);
	ENTER("key_file %p, group_name %s", key_file, group_name);

	/* Create the new page. */
	page = gnc_plugin_page_account_tree_new();
	account_page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(page);
	priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(account_page);

	/* Install it now so we can them manipulate the created widget */
	gnc_main_window_open_page(GNC_MAIN_WINDOW(window), page);

	/* Expanded accounts */
	count = g_key_file_get_integer(key_file, group_name, ACCT_COUNT, &error);
	if (error) {
	  g_warning("error reading group %s key %s: %s",
		    group_name, ACCT_COUNT, error->message);
	  g_error_free(error);
	  LEAVE("bad value");
	  return page;
	}
	for (i = 1; i <= count; i++) {
	  key = g_strdup_printf(ACCT_OPEN, i);
	  value = g_key_file_get_string(key_file, group_name, key, &error);
	  if (error) {
	    g_warning("error reading group %s key %s: %s",
		      group_name, key, error->message);
	    g_error_free(error);
	    error = NULL;
	  } else {
	    tree_restore_expanded_row(priv->tree_view, value);
	    g_free(value);
	  }
	}

	/* Selected account (if any) */
	value = g_key_file_get_string(key_file, group_name, ACCT_SELECTED, NULL);
	if (value) {
	  tree_restore_selected_row(priv->tree_view, value);
	  g_free(value);
	}
	LEAVE(" ");
	return page;
}


/* Callbacks */

/** This button press handler calls the common button press handler
 *  for all pages.  The GtkTreeView eats all button presses and
 *  doesn't pass them up the widget tree, even when doesn't do
 *  anything with them.  The only way to get access to the button
 *  presses in an account tree page is here on the tree view widget.
 *  Button presses on all other pages are caught by the signal
 *  registered in gnc-main-window.c. */
static gboolean
gnc_plugin_page_account_tree_button_press_cb (GtkWidget *widget,
					      GdkEventButton *event,
	       				      GncPluginPage *page)
{
  gboolean result;

  g_return_val_if_fail(GNC_IS_PLUGIN_PAGE(page), FALSE);

  ENTER("widget %p, event %p, page %p", widget, event, page);
  result = gnc_main_window_button_press_cb(widget, event, page);
  LEAVE(" ");
  return result;
}

static void
gnc_plugin_page_account_tree_double_click_cb (GtkTreeView        *treeview,
					      GtkTreePath        *path,
					      GtkTreeViewColumn  *col,
					      GncPluginPageAccountTree *page)
{
	GtkWidget *window;
	GncPluginPage *new_page;
	Account *account;

	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
	account = gnc_tree_view_account_get_account_from_path (GNC_TREE_VIEW_ACCOUNT(treeview), path);
	if (account == NULL)
	  return;

	window = GNC_PLUGIN_PAGE (page)->window;
	new_page = gnc_plugin_page_register_new (account, FALSE);
	gnc_main_window_open_page (GNC_MAIN_WINDOW(window), new_page);
}

static void
gnc_plugin_page_account_tree_selection_changed_cb (GtkTreeSelection *selection,
						   GncPluginPageAccountTree *page)
{
	GtkActionGroup *action_group;
	GtkTreeView *view;
	Account *account = NULL;
	gboolean sensitive;

	g_return_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page));

	if (!selection) {
		sensitive = FALSE;
	} else {
		g_return_if_fail(GTK_IS_TREE_SELECTION(selection));
		view = gtk_tree_selection_get_tree_view (selection);
		account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(view));
		sensitive = (account != NULL);

		/* Check here for placeholder accounts, etc. */
	}

	action_group = gnc_plugin_page_get_action_group(GNC_PLUGIN_PAGE(page));
	gnc_plugin_update_actions (action_group, actions_requiring_account,
				   "sensitive", sensitive);
	g_signal_emit (page, plugin_page_signals[ACCOUNT_SELECTED], 0, account);
}
	

/* Command callbacks */
static void
gnc_plugin_page_account_tree_cmd_new_account (GtkAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);

	gnc_ui_new_account_window_with_default (NULL, account);
}

static void
gnc_plugin_page_account_tree_cmd_file_hierarchy_merge (GtkAction *action, GncPluginPageAccountTree *page)
{
	gnc_ui_qof_book_merge_druid();
}

static void
gnc_plugin_page_account_tree_cmd_open_account (GtkAction *action,
					       GncPluginPageAccountTree *page)
{
	GtkWidget *window;
	GncPluginPage *new_page;
	Account *account;

	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
	account = gnc_plugin_page_account_tree_get_current_account (page);
	if (account == NULL)
	  return;

	window = GNC_PLUGIN_PAGE (page)->window;
	new_page = gnc_plugin_page_register_new (account, FALSE);
	gnc_main_window_open_page (GNC_MAIN_WINDOW(window), new_page);
}

static void
gnc_plugin_page_account_tree_cmd_open_subaccounts (GtkAction *action,
						   GncPluginPageAccountTree *page)
{
	GtkWidget *window;
	GncPluginPage *new_page;
	Account *account;

	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
	account = gnc_plugin_page_account_tree_get_current_account (page);
	if (account == NULL)
	  return;

	window = GNC_PLUGIN_PAGE (page)->window;
	new_page = gnc_plugin_page_register_new (account, TRUE);
	gnc_main_window_open_page (GNC_MAIN_WINDOW(window), new_page);
}

static void
gnc_plugin_page_account_tree_cmd_edit_account (GtkAction *action, GncPluginPageAccountTree *page)
{
	Account *account;

	ENTER("action %p, page %p", action, page);

	account = gnc_plugin_page_account_tree_get_current_account (page);
	g_return_if_fail (account != NULL);

	gnc_ui_edit_account_window (account);
	LEAVE(" ");
}

/********************************************************************
 * delete_account_helper
 * See if this account has any splits present.  Set the user data
 * and return the same value to stop walking the account tree if
 * appropriate.
 ********************************************************************/
typedef struct _delete_helper
{
	gboolean has_splits;
	gboolean has_ro_splits;
} delete_helper_t;

static gpointer
delete_account_helper (Account * account, gpointer data)
{
	delete_helper_t *helper_res = data;
	GList *splits;

	splits = xaccAccountGetSplitList (account);
	if (splits) {
		helper_res->has_splits = TRUE;
		while (splits) {
			Split *s = splits->data;
			Transaction *txn = xaccSplitGetParent (s);
			if (xaccTransGetReadOnly (txn)) {
				helper_res->has_ro_splits = TRUE;
				break;
			}
			splits = splits->next;
		}
	}

	return GINT_TO_POINTER (helper_res->has_splits || helper_res->has_ro_splits);
}

static void
gnc_plugin_page_account_tree_cmd_delete_account (GtkAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);

	const char *no_splits_no_children = _("Are you sure you want to delete the %s account?");
	const char *no_splits = _("Are you sure you want to delete the %s\n"
				  "account and all its children?");
	const char *acct_has_splits =
		_("This account contains transactions.  Are you sure you\n"
		  "want to delete the %s account?");
	const char *child_has_splits =
		_("One (or more) children of this account contain\n"
		  "transactions.  Are you sure you want to delete the\n"
		  "%s account and all its children?");
	const char *acct_has_ro_splits =
		_("This account contains read-only transactions.  You " "may not delete %s.");
	const char *child_has_ro_splits =
		_("One (or more) children of this account contains "
		  "read-only transactions.  You may not delete %s.");
	const char *format;
	char *name;
	GList *splits;

	g_return_if_fail (account != NULL);

	name = xaccAccountGetFullName (account, gnc_get_account_separator ());
	if (!name)
		name = g_strdup ("");

	if ((splits = xaccAccountGetSplitList (account)) != NULL) {
		/* Check for RO txns -- if there are any, disallow deletion */
		for (; splits; splits = splits->next) {
			Split *s = splits->data;
			Transaction *txn = xaccSplitGetParent (s);
			if (xaccTransGetReadOnly (txn)) {
				gnc_error_dialog (NULL, acct_has_ro_splits, name);
				return;
			}
		}
		format = acct_has_splits;
	} else {
		AccountGroup *children;
		delete_helper_t delete_res = { FALSE, FALSE };

		children = xaccAccountGetChildren (account);
		xaccGroupForEachAccount (children, delete_account_helper, &delete_res, TRUE);

		/* Check for RO txns in the children -- disallow deletion if there are any */
		if (delete_res.has_ro_splits) {
			gnc_error_dialog (NULL, child_has_ro_splits, name);
			return;

		} else if (delete_res.has_splits)
			format = child_has_splits;
		else
			format = children ? no_splits : no_splits_no_children;
	}

	if (gnc_verify_dialog (NULL, FALSE, format, name)) {
		gnc_suspend_gui_refresh ();

		xaccAccountBeginEdit (account);
		xaccAccountDestroy (account);

		gnc_resume_gui_refresh ();
	}
	g_free (name);
}

/******************************/
/*       Options Dialog       */
/******************************/

static void
gnc_plugin_page_account_tree_options_apply_cb (GNCOptionWin * propertybox,
					       gpointer user_data)
{
  GncPluginPageAccountTreePrivate *priv = user_data;
  if(!priv)
    return;

  ENTER(" ");
  gnc_option_db_commit(priv->odb);
  LEAVE(" ");
}

static void
gnc_plugin_page_account_tree_options_help_cb (GNCOptionWin * propertybox,
					      gpointer user_data)
{
  GtkWidget *dialog;

  dialog = gtk_message_dialog_new (NULL,
				   GTK_DIALOG_DESTROY_WITH_PARENT,
				   GTK_MESSAGE_INFO,
				   GTK_BUTTONS_OK,
				   "Set the account tree options you want using this dialog.");

  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}

static void
gnc_plugin_page_account_tree_options_close_cb (GNCOptionWin * propertybox,
					       gpointer user_data)
{
  GncPluginPageAccountTreePrivate *priv = user_data;
  if(!priv)
    return;

  gnc_options_dialog_destroy(priv->editor_dialog);
  priv->editor_dialog = NULL;
}

static void
gnc_plugin_page_acct_tree_options_new (GncPluginPageAccountTreePrivate *priv)
{
  SCM func, opts_and_id;

  scm_gc_unprotect_object(priv->options);
  func = scm_c_eval_string("gnc:make-new-acct-tree-window");
  opts_and_id = scm_call_0(func);
  priv->options = SCM_CAR(opts_and_id);
  scm_gc_protect_object(priv->options);
  priv->options_id = scm_num2int(SCM_CDR(opts_and_id), SCM_ARG1, __FUNCTION__);
}

/*********************/

static void
gnc_plugin_page_account_tree_cmd_view_options (GtkAction *action, GncPluginPageAccountTree *page)
{
  GncPluginPageAccountTreePrivate *priv;

  g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
  priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);

  if (!priv->editor_dialog) {
    priv->editor_dialog = gnc_options_dialog_new(_("Account Tree Options"));
    gnc_build_options_dialog_contents(priv->editor_dialog, 
				      priv->odb);
    
    gnc_options_dialog_set_apply_cb(priv->editor_dialog, 
				    gnc_plugin_page_account_tree_options_apply_cb,
				    priv);
    gnc_options_dialog_set_help_cb(priv->editor_dialog, 
				   gnc_plugin_page_account_tree_options_help_cb,
				   priv);
    gnc_options_dialog_set_close_cb(priv->editor_dialog, 
				    gnc_plugin_page_account_tree_options_close_cb,
				    priv);
  }
  gtk_window_present(GTK_WINDOW(gnc_options_dialog_widget(priv->editor_dialog)));
}

static void
gnc_plugin_page_account_tree_cmd_reconcile (GtkAction *action,
					    GncPluginPageAccountTree *page)
{
	GtkWidget *window;
	Account *account;
	RecnWindow *recnData;

	account = gnc_plugin_page_account_tree_get_current_account (page);
	g_return_if_fail (account != NULL);

	window = GNC_PLUGIN_PAGE (page)->window;
	recnData = recnWindow (window, account);
	gnc_ui_reconcile_window_raise (recnData);
}

static void
gnc_plugin_page_account_tree_cmd_transfer (GtkAction *action,
					   GncPluginPageAccountTree *page)
{
	GtkWidget *window;
	Account *account;

	account = gnc_plugin_page_account_tree_get_current_account (page);
	window = GNC_PLUGIN_PAGE (page)->window;
	gnc_xfer_dialog (window, account);
}

static void
gnc_plugin_page_account_tree_cmd_stock_split (GtkAction *action,
					      GncPluginPageAccountTree *page)
{
	GtkWidget *window;
	Account *account;

	account = gnc_plugin_page_account_tree_get_current_account (page);
	window = GNC_PLUGIN_PAGE (page)->window;
	gnc_stock_split_dialog (window, account);
}

static void
gnc_plugin_page_account_tree_cmd_lots (GtkAction *action,
				       GncPluginPageAccountTree *page)
{
	Account *account;

	account = gnc_plugin_page_account_tree_get_current_account (page);
	gnc_lot_viewer_dialog (account);
}

static void
gnc_plugin_page_account_tree_cmd_scrub (GtkAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);

	g_return_if_fail (account != NULL);

	gnc_suspend_gui_refresh ();

	xaccAccountScrubOrphans (account);
	xaccAccountScrubImbalance (account);

	gnc_resume_gui_refresh ();
}

static void
gnc_plugin_page_account_tree_cmd_scrub_sub (GtkAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);

	g_return_if_fail (account != NULL);

	gnc_suspend_gui_refresh ();

	xaccAccountTreeScrubOrphans (account);
	xaccAccountTreeScrubImbalance (account);

	gnc_resume_gui_refresh ();
}

static void
gnc_plugin_page_account_tree_cmd_scrub_all (GtkAction *action, GncPluginPageAccountTree *page)
{
	AccountGroup *group = gnc_get_current_group ();

	gnc_suspend_gui_refresh ();

	xaccGroupScrubOrphans (group);
	xaccGroupScrubImbalance (group);
}

/** @} */
/** @} */
