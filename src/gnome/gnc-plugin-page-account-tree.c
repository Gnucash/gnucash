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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup GncPluginPageAccountTree An Account Tree Plugin
    @{ */
/** @file gnc-plugin-page-account-tree.c
    @brief Functions providing a chart of account page.
    @author Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
            Copyright (C) 2003,2005 David Hampton <hampton@employees.org>
*/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#ifndef HAVE_GLIB26
#include "gkeyfile.h"
#endif
#include "gnc-plugin-page-account-tree.h"
#include "gnc-plugin-page-register.h"

#include "Scrub.h"
#include "Transaction.h"
#include "dialog-account.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "druid-merge.h"
#include "gnc-account-sel.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-gnome-utils.h"
#include "gnc-html.h"
#include "gnc-icons.h"
#include "gnc-plugin-account-tree.h"
#include "gnc-session.h"
#include "gnc-split-reg.h"
#include "gnc-tree-view-account.h"
#include "gnc-tree-model-account-types.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "lot-viewer.h"
#include "window-reconcile.h"
#include "window-main-summarybar.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

#define PLUGIN_PAGE_ACCT_TREE_CM_CLASS "plugin-page-acct-tree"
#define GCONF_SECTION "window/pages/account_tree"

#define FILTER_TREE_VIEW "types_tree_view"

enum {
  ACCOUNT_SELECTED,
  LAST_SIGNAL
};

typedef struct GncPluginPageAccountTreePrivate
{
	GtkWidget   *widget;
	GtkTreeView *tree_view;
	gint         component_id;

	struct {
	  GtkWidget    *dialog;
	  GtkTreeModel *model;
	  guint32    	visible_types;
	  guint32    	original_visible_types;
	  gboolean   	hide_zero_total;
	  gboolean   	original_hide_zero_total;
	  gulong        selection_changed_cb_id;
	} fd;

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
void gppat_populate_tmas_list(GtkToggleButton *dmrb, gpointer tmas);
void gppat_set_insensitive_iff_rb_active(GtkToggleButton *b, GtkWidget *widget);

/* "Filter By" dialog callbacks */
gboolean gnc_plugin_page_account_tree_filter_accounts(Account *account, gpointer user_data);
void gppat_filter_hide_zero_toggled_cb (GtkToggleButton *togglebutton, GncPluginPageAccountTree *page);
void gppat_filter_clear_all_cb (GtkWidget *button, GncPluginPageAccountTree *page);
void gppat_filter_select_all_cb (GtkWidget *button, GncPluginPageAccountTree *page);
void gppat_filter_select_default_cb (GtkWidget *button, GncPluginPageAccountTree *page);
void gppat_filter_response_cb (GtkWidget *dialog, gint response, GncPluginPageAccountTree *page);


/* Command callbacks */
static void gnc_plugin_page_account_tree_cmd_new_account (GtkAction *action, GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_cmd_file_hierarchy_merge (GtkAction *action, GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_cmd_open_account (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_open_subaccounts (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_edit_account (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_delete_account (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_view_filter_by (GtkAction *action, GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_cmd_reconcile (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_transfer (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_stock_split (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_lots (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_scrub (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_scrub_sub (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_scrub_all (GtkAction *action, GncPluginPageAccountTree *page);


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
	{ "EditDeleteAccountAction", GNC_STOCK_DELETE_ACCOUNT, N_("_Delete Account..."), NULL,
	  N_("Delete selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_delete_account) },

	/* View menu */
	{ "ViewFilterByAction", NULL, N_("_Filter By..."), NULL, NULL,
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_view_filter_by) },

	/* Actions menu */
	{ "ActionsReconcileAction", NULL, N_("_Reconcile..."), NULL,
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
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_page_account_tree_n_actions = G_N_ELEMENTS (gnc_plugin_page_account_tree_actions);


/** Actions that require an account to be selected before they are
 *  enabled. */
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

	/* Visisble types */
	priv->fd.visible_types = -1; /* Start with all types */
	priv->fd.hide_zero_total = FALSE;
	
	LEAVE("page %p, priv %p, action group %p",
	      plugin_page, priv, action_group);
}

static void
gnc_plugin_page_account_tree_finalize (GObject *object)
{
	GncPluginPageAccountTree *page;
	GncPluginPageAccountTreePrivate *priv;

	ENTER("object %p", object);
	page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (object);
	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
	priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
	g_return_if_fail (priv != NULL);

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

	gnc_tree_view_account_set_filter (GNC_TREE_VIEW_ACCOUNT(tree_view),
					  gnc_plugin_page_account_tree_filter_accounts,
					  plugin_page, NULL);

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

#define ACCT_COUNT    "Number of Open Accounts"
#define ACCT_OPEN     "Open Account %d"
#define ACCT_SELECTED "Selected Account"
#define HIDE_ZERO     "Hide Zero Total"
#define ACCT_TYPES    "Account Types"

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

	g_key_file_set_integer(key_file, group_name, ACCT_TYPES, priv->fd.visible_types);
	g_key_file_set_boolean(key_file, group_name, HIDE_ZERO, priv->fd.hide_zero_total);
	
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
	gboolean hide;
	
	g_return_val_if_fail(key_file, NULL);
	g_return_val_if_fail(group_name, NULL);
	ENTER("key_file %p, group_name %s", key_file, group_name);

	/* Create the new page. */
	page = gnc_plugin_page_account_tree_new();
	account_page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(page);
	priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(account_page);

	/* Install it now so we can them manipulate the created widget */
	gnc_main_window_open_page(GNC_MAIN_WINDOW(window), page);

	/* Filter information. Ignore missing keys. */
	hide = g_key_file_get_boolean(key_file, group_name, HIDE_ZERO, &error);
	if (error) {
	  g_warning("error reading group %s key %s: %s",
		    group_name, HIDE_ZERO, error->message);
	  g_error_free(error);
	  error = NULL;
	  hide = FALSE;
	}
	priv->fd.hide_zero_total = hide;

	i = g_key_file_get_integer(key_file, group_name, ACCT_TYPES, &error);
	if (error) {
	  g_warning("error reading group %s key %s: %s",
		    group_name, ACCT_TYPES, error->message);
	  g_error_free(error);
	  error = NULL;
	  i = -1;
	}
	priv->fd.visible_types = i;

	/* Expanded accounts. Skip if count key missing. */
	count = g_key_file_get_integer(key_file, group_name, ACCT_COUNT, &error);
	if (error == NULL) {
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
	} else {
	  g_warning("error reading group %s key %s: %s",
		    group_name, ACCT_COUNT, error->message);
	  g_error_free(error);
	}

	/* Selected account (if any) */
	value = g_key_file_get_string(key_file, group_name, ACCT_SELECTED, NULL);
	if (value) {
	  tree_restore_selected_row(priv->tree_view, value);
	  g_free(value);
	}

	/* Update tree view for any changes */
	gnc_tree_view_account_refilter(GNC_TREE_VIEW_ACCOUNT(priv->tree_view));

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
	GncPluginPageAccountTreePrivate *priv;
	GtkWidget *window;
	GncPluginPage *new_page;
	Account *account;

	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
	account = gnc_tree_view_account_get_account_from_path (GNC_TREE_VIEW_ACCOUNT(treeview), path);
	if (account == NULL)
	  return;

	priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
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

/***
 *** The OK button of a Delete Account dialog is insensitive if
 *** and only if a sensitive account selector contains no accounts.
 ***/
static void
set_ok_sensitivity(GtkWidget *dialog)
{
  GtkWidget *button;
  gpointer dmas, tmas;
  gboolean sensitive;

  dmas = g_object_get_data(G_OBJECT(dialog), "dmas");
  tmas = g_object_get_data(G_OBJECT(dialog), "tmas");
  sensitive = (((NULL == dmas) ||
		(!GTK_WIDGET_IS_SENSITIVE(GTK_WIDGET(dmas)) ||
		 GTK_LIST(GNC_ACCOUNT_SEL(dmas)->combo->list)->children)) &&
	       ((NULL == tmas) ||
		(!GTK_WIDGET_IS_SENSITIVE(GTK_WIDGET(tmas)) ||
		 GTK_LIST(GNC_ACCOUNT_SEL(tmas)->combo->list)->children)));

  button = gnc_glade_lookup_widget(dialog, "deletebutton");
  gtk_widget_set_sensitive(button, sensitive);
}

/***
 *** GNCAccountSel has an odd habit of adding a
 *** blank item when its list is otherwise empty.
 ***/

static void
exclude_account(GtkWidget *item,
		gpointer name)
{
  char *text;

  gtk_label_get(GTK_LABEL(GTK_BIN(item)->child), &text);
  if ((0 == strlen(text)) || (0 == strcmp(text, name))) {
    gtk_widget_destroy(GTK_WIDGET(item));
  }
}

static void
exclude_account_subtree(GtkWidget *item,
			gpointer prefix)
{
  char *text;

  gtk_label_get(GTK_LABEL(GTK_BIN(item)->child), &text);
  if ((0 == strlen(text)) || 0 == strncmp(text, prefix, strlen(prefix))) {
    gtk_widget_destroy(GTK_WIDGET(item));
  }
}

static gint
compare_listitem_text(gconstpointer item,
		      gconstpointer entrytext)
{
  char *text;

  gtk_label_get(GTK_LABEL(GTK_BIN(item)->child), &text);
  return strcmp(text, entrytext);
}

static void
gppat_populate_gas_list(GtkWidget *dialog,
			GNCAccountSel *gas,
			gboolean exclude_subaccounts)
{
  GtkList *list;
  GtkEntry *entry;
  gpointer name, filter;

  g_return_if_fail(GTK_IS_DIALOG(dialog));
  if (gas == NULL)
    return;
  list = GTK_LIST(gas->combo->list);
  entry = GTK_ENTRY(gas->combo->entry);
  name = g_object_get_data(G_OBJECT(dialog), "name");
  filter = g_object_get_data(G_OBJECT(dialog), "filter");

  /* Setting the account type filter triggers GNCAccountSel population. */
  gnc_account_sel_set_acct_filters (gas, filter);

  /* Accounts to be deleted must be removed. */
  gtk_container_foreach(GTK_CONTAINER(list), (exclude_subaccounts ?
					      exclude_account_subtree :
					      exclude_account), name);

  /* The entry widget may need to be reset. */
  if (NULL == g_list_find_custom(list->children, 
				 gtk_entry_get_text(entry),
				 compare_listitem_text)) {
    gtk_entry_set_text(entry, "");
    gtk_list_select_item(list, 0);
  }

  /* The sensitivity of the OK button needs to be reevaluated. */
  set_ok_sensitivity(GTK_WIDGET(dialog));
}

void
gppat_populate_tmas_list(GtkToggleButton *damrb,
			 gpointer tmas)
{
  GtkWidget *dialog;

  /* Cannot move transactions to subaccounts if they are to be deleted. */
  dialog = gnc_glade_lookup_widget(GTK_WIDGET(damrb), "Delete Account");
  gppat_populate_gas_list(dialog, tmas, !gtk_toggle_button_get_active(damrb));
}

void
gppat_set_insensitive_iff_rb_active(GtkToggleButton *b, GtkWidget *widget)
{
  gtk_widget_set_sensitive(widget, !gtk_toggle_button_get_active(b));
  set_ok_sensitivity(gtk_widget_get_toplevel(widget));
}

static void
gnc_plugin_page_account_tree_cmd_delete_account (GtkAction *action, GncPluginPageAccountTree *page)
{
  Account *account = gnc_plugin_page_account_tree_get_current_account (page);
  gchar *acct_name;
  GList *splits;
  AccountGroup *children;
  delete_helper_t delete_res = { FALSE, FALSE };
  GtkWidget *widget;
  GtkWidget *window;
  GtkWidget *dialog = NULL;
  GNCAccountSel *damas = NULL; /* descendant account move account selector */
  GNCAccountSel *dtmas = NULL; /* descendant transaction move account selector */
  GNCAccountSel *tmas = NULL; /* transaction move account selector */
  gint response;
  Account *ta = NULL; /* transaction adopter */
  Account *daa = NULL; /* descendant account adopter */
  Account *dta = NULL; /* descendant transaction adopter */

  if (NULL == account)
    return;

  window = gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page));
  acct_name = xaccAccountGetFullName(account, gnc_get_account_separator ());
  if (!acct_name) {
    acct_name = g_strdup (_("(no name)"));
  }

  splits = xaccAccountGetSplitList(account);
  children = xaccAccountGetChildren(account);

  /*
   * If the account has transactions or child accounts then conduct a
   * dialog to allow the user to specify what should be done with them.
   */
  if ((NULL != splits) || (NULL != children)) {
    GList *filter;
    GladeXML *xml;
    GtkWidget *label;
    gchar *message;

    xml = gnc_glade_xml_new ("account.glade", "Delete Account");
    dialog = glade_xml_get_widget (xml, "Delete Account");
    gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(window));
    glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func, dialog);

    label = glade_xml_get_widget (xml, "header");
    message = g_strdup_printf(_("Deleting account %s"), acct_name);
    gtk_label_set_text(GTK_LABEL(label), message);
    g_free(message);

    /*
     * Reparent only to accounts of the same
     * type as the one being deleted.
     */
    filter = g_list_prepend(NULL, (gpointer)xaccAccountGetType(account));
    g_object_set_data(G_OBJECT(dialog), "filter", filter);
    g_object_set_data(G_OBJECT(dialog), "name", acct_name);

    /*
     * Adjust the dialog based on whether the account has
     * transactions.
     */
    if (splits) {
      delete_helper_t delete_res2 = { FALSE, FALSE };

      tmas = GNC_ACCOUNT_SEL(glade_xml_get_widget (xml, "tmas"));
      gppat_populate_gas_list(dialog, tmas, FALSE);

      delete_account_helper(account, &delete_res2);
      if (delete_res2.has_ro_splits) {
	gtk_widget_destroy(glade_xml_get_widget (xml, "trans_rw"));
	widget = glade_xml_get_widget (xml, "tdrb");
	gtk_widget_set_sensitive(widget, FALSE);
      } else {
	gtk_widget_destroy(glade_xml_get_widget (xml, "trans_ro"));
      }
    } else {
      gtk_widget_destroy(glade_xml_get_widget (xml, "transactions"));
    }

    /*
     * Adjust the dialog based on whether the account has children.
     */
    if (children != NULL) {
      /*
       * Check for RO txns in descendants
       */
      xaccGroupForEachAccount(children, delete_account_helper,
			      &delete_res, TRUE);
      if (delete_res.has_ro_splits) {
	gtk_widget_destroy(glade_xml_get_widget (xml, "sa_trans_rw"));
	widget = glade_xml_get_widget (xml, "dtdrb");
	gtk_widget_set_sensitive(widget, FALSE);
      } else if (delete_res.has_splits) {
	gtk_widget_destroy(glade_xml_get_widget (xml, "sa_trans_ro"));
      } else {
	gtk_widget_destroy(glade_xml_get_widget (xml, "subaccount_trans"));
      }
      damas = GNC_ACCOUNT_SEL(glade_xml_get_widget (xml, "damas"));
      gppat_populate_gas_list(dialog, damas, TRUE);
      dtmas = GNC_ACCOUNT_SEL(glade_xml_get_widget (xml, "dtmas"));
      gppat_populate_gas_list(dialog, dtmas, TRUE);
    } else {
      gtk_widget_destroy(glade_xml_get_widget (xml, "subaccounts"));
      gtk_widget_destroy(glade_xml_get_widget (xml, "subaccount_trans"));
    }

    /* default to cancel */
    gtk_dialog_set_default_response (GTK_DIALOG(dialog), GTK_RESPONSE_CANCEL);

    /*
     * Note that one effect of the modal dialog is preventing
     * the account selectors from being repopulated.
     */ 
    gtk_widget_show_all(dialog);
    response = gtk_dialog_run(GTK_DIALOG(dialog));
    if (GTK_RESPONSE_ACCEPT != response) {
      /* Account deletion is cancelled, so clean up and return. */
      gtk_widget_destroy(dialog);
      g_list_free(filter);
      g_free(acct_name);
      return;
    }
    if (tmas && GTK_WIDGET_IS_SENSITIVE(tmas))
      ta = gnc_account_sel_get_account(tmas);
    if (damas && GTK_WIDGET_IS_SENSITIVE(damas))
      daa = gnc_account_sel_get_account(damas);
    if (dtmas && GTK_WIDGET_IS_SENSITIVE(dtmas))
      dta = gnc_account_sel_get_account(dtmas);
    gtk_widget_destroy(dialog);
    g_list_free(filter);
  } /* (NULL != splits) || (NULL != children) */

  /*
   * Present a message to the user which specifies what will be
   * deleted and what will be reparented, then ask for verification.
   */
  {
    const char *format = _("The account %s will be deleted.");
    char *lines[8];
    char *message;
    char *name;
    int i = 0;

    lines[0] = g_strdup_printf(format, acct_name);
    if (splits) {
      if (ta) {
	name = xaccAccountGetFullName(ta, gnc_get_account_separator ());
	format = _("All transactions in this account will be moved to "
		   "the account %s.");
	lines[++i] = g_strdup_printf(format, name);
      } else if (splits) {
	format = _("All transactions in this account will be deleted.");
	lines[++i] = g_strdup_printf("%s", format);
      }
    }
    if (children) {
      if (daa) {
	name = xaccAccountGetFullName(daa, gnc_get_account_separator ());
	format = _("All of its sub-accounts will be moved to "
		   "the account %s.");
	lines[++i] = g_strdup_printf(format, name);
      } else {
	format = _("All of its subaccounts will be deleted.");
	lines[++i] = g_strdup_printf("%s", format);
	if (dta) {
	  name = xaccAccountGetFullName(ta, gnc_get_account_separator ());
	  format = _("All sub-account transactions will be moved to "
		     "the account %s.");
	  lines[++i] = g_strdup_printf(format, name);
	} else if (delete_res.has_splits) {
	  format = _("All sub-account transactions will be deleted.");
	  lines[++i] = g_strdup_printf("%s", format);
	}
      }
    }
    lines[++i] = _("Are you sure you want to do this?");
    lines[i] = NULL;
    i--; /* Don't try to free the constant question. */
    message = g_strjoinv(" ", lines);
    while (i--) {
	g_free(lines[i]);
    }

    dialog =  gtk_message_dialog_new(GTK_WINDOW(window),
				       GTK_DIALOG_DESTROY_WITH_PARENT,
				       GTK_MESSAGE_QUESTION,
				       GTK_BUTTONS_NONE,
				       message);
    g_free(message);
    gtk_dialog_add_buttons(GTK_DIALOG(dialog),
			     GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
			     GTK_STOCK_DELETE, GTK_RESPONSE_ACCEPT,
			     (gchar *)NULL);
    gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_CANCEL);
    response = gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);

    if (GTK_RESPONSE_ACCEPT == response) {
      gnc_suspend_gui_refresh ();
      xaccAccountBeginEdit (account);
      if (NULL != daa) {
	GList *acct_list, *ptr;

	xaccAccountBeginEdit (daa);
	acct_list = xaccGroupGetSubAccounts(children);
	for (ptr = acct_list; ptr; ptr = g_list_next(ptr))
	  xaccAccountInsertSubAccount (daa, ptr->data);
	g_list_free(acct_list);
	xaccAccountCommitEdit (daa);
      } else if (NULL != dta) {
	/* Move the splits of its subaccounts, if any. */
	xaccGroupForEachAccount (children,
				 (gpointer (*)(Account *, gpointer))
				 xaccAccountMoveAllSplits,
				 dta, TRUE);
      }
      if (NULL != ta) {
	/* Move the splits of the account to be deleted. */
	xaccAccountMoveAllSplits (account, ta);
      }
      /*
       * Finally, delete the account, any subaccounts it may still
       * have, and any splits it or its subaccounts may still have.
       */
      xaccAccountDestroy (account);
      gnc_resume_gui_refresh ();
    }
  }
  g_free(acct_name);
}

/*********************/

/** This function tells the account tree view whether or not to filter
 *  out a particular account.  Accounts may be filtered if the user
 *  has decided not to display that particular account type, or if the
 *  user has requested taht accoutns with a zero total not be shown.
 *
 *  @param account The account that was toggled.
 *
 *  @param user_data A pointer to the account tree page.
 *
 *  @return TRUE if the account should be visible.  FALSE if the
 *  account should be hidden. */
gboolean
gnc_plugin_page_account_tree_filter_accounts (Account *account, gpointer user_data)
{
  
  GncPluginPageAccountTree *page = user_data;
  GncPluginPageAccountTreePrivate *priv;
  GNCAccountType acct_type;
  gnc_numeric total;
  gboolean result;

  g_return_val_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page), TRUE);

  ENTER("account %p:%s, page %p", account, xaccAccountGetName(account), page);

  priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
  if (priv->fd.hide_zero_total) {
    total = xaccAccountGetBalanceInCurrency (account, NULL, TRUE);
    if (gnc_numeric_zero_p(total)) {
      LEAVE(" hide: zero balance");
      return FALSE;
    }
  }
  
  acct_type = xaccAccountGetType(account);
  result = (priv->fd.visible_types & (1 << acct_type)) ? TRUE : FALSE;
  LEAVE(" %s", result ? "show" : "hide");
  return result;
}

/** The "hide zero totals" button in the Filter dialog changed state.
 *  Update the page to reflect these changes.
 *
 *  @param button The GtkCheckButton that was toggled.
 *
 *  @param page A pointer to the account tree page to update. */
void
gppat_filter_hide_zero_toggled_cb (GtkToggleButton *button,
				   GncPluginPageAccountTree *page)
{
  GncPluginPageAccountTreePrivate *priv;

  g_return_if_fail(GTK_IS_TOGGLE_BUTTON(button));
  g_return_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page));

  ENTER("button %p, page %p", button, page);
  priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
  priv->fd.hide_zero_total = gtk_toggle_button_get_active(button);
  gnc_tree_view_account_refilter(GNC_TREE_VIEW_ACCOUNT(priv->tree_view));
  LEAVE("hide_zero %d", priv->fd.hide_zero_total);
}

/** The "clear all account types" button in the Filter dialog was
 *  clicked.  Clear all account types shown, and update the visible
 *  page.
 *
 *  @param button The button that was clicked.
 *
 *  @param page A pointer to the account tree page to update. */
void
gppat_filter_clear_all_cb (GtkWidget *button,
			   GncPluginPageAccountTree *page)
{
  GncPluginPageAccountTreePrivate *priv;
  GtkTreeSelection *selection;
  GtkTreeView *view;

  g_return_if_fail(GTK_IS_BUTTON(button));
  g_return_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page));

  ENTER("button %p, page %p", button, page);
  priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
  view = GTK_TREE_VIEW(gnc_glade_lookup_widget(button, FILTER_TREE_VIEW));
  selection = gtk_tree_view_get_selection(view);
  g_signal_handler_block(selection, priv->fd.selection_changed_cb_id);
  priv->fd.visible_types = 0;
  gnc_tree_model_account_types_set_selection(view, priv->fd.visible_types);
  g_signal_handler_unblock(selection, priv->fd.selection_changed_cb_id);
  gnc_tree_view_account_refilter(GNC_TREE_VIEW_ACCOUNT(priv->tree_view));
  LEAVE("types 0x%x", priv->fd.visible_types);
}

/** The "select all account types" button in the Filter dialog was
 *  clicked.  Make all account types visible, and update the page.
 *
 *  @param button The button that was clicked.
 *
 *  @param page A pointer to the account tree page to update. */
void
gppat_filter_select_all_cb (GtkWidget *button,
			    GncPluginPageAccountTree *page)
{
  GncPluginPageAccountTreePrivate *priv;
  GtkTreeSelection *selection;
  GtkTreeView *view;

  g_return_if_fail(GTK_IS_BUTTON(button));
  g_return_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page));

  ENTER("button %p, page %p", button, page);
  priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
  view = GTK_TREE_VIEW(gnc_glade_lookup_widget(button, FILTER_TREE_VIEW));
  selection = gtk_tree_view_get_selection(view);
  g_signal_handler_block(selection, priv->fd.selection_changed_cb_id);
  priv->fd.visible_types = -1;
  gnc_tree_model_account_types_set_selection(view, priv->fd.visible_types);
  g_signal_handler_unblock(selection, priv->fd.selection_changed_cb_id);
  gnc_tree_view_account_refilter(GNC_TREE_VIEW_ACCOUNT(priv->tree_view));
  LEAVE("types 0x%x", priv->fd.visible_types);
}

/** The "select default account types" button in the Filter dialog was
 *  clicked.  Set all account types to their default visibility (which
 *  happens to be visible for all of them), and update the page.
 *
 *  @param button The button that was clicked.
 *
 *  @param page A pointer to the account tree page to update. */
void
gppat_filter_select_default_cb (GtkWidget *button,
				GncPluginPageAccountTree *page)
{
  ENTER("button %p, page %p", button, page);
  gppat_filter_select_all_cb(button, page);
  LEAVE(" ");
}

/** The account type selection in the Filter dialog was changed.
 *  Reread the set of selected (i.e. visible) accounts and update the
 *  page.
 *
 *  @param button The button that was clicked.
 *
 *  @param page A pointer to the account tree page to update. */
static void
gppat_filter_selection_changed_cb  (GtkTreeSelection *selection,
				    GncPluginPageAccountTree *page)
{
  GncPluginPageAccountTreePrivate *priv;
  GtkTreeView *view;

  g_return_if_fail(GTK_IS_TREE_SELECTION(selection));
  g_return_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page));

  ENTER("selection %p, page %p", selection, page);
  priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
  view = gtk_tree_selection_get_tree_view(selection);
  priv->fd.visible_types = gnc_tree_model_account_types_get_selection(view);
  gnc_tree_view_account_refilter(GNC_TREE_VIEW_ACCOUNT(priv->tree_view));
  LEAVE("types 0x%x", priv->fd.visible_types);
}

/** The Filter dialog was closed.  CHeck to see if this was done via
 *  the OK button.  If so, make the changes permanent.  If not, revert
 *  any changes.
 *
 *  @param dialog A pointer to the "Filter By" dialog.
 *
 *  @param response The response code from closing the dialog.
 *
 *  @param page A pointer to the account tree page to update. */
void
gppat_filter_response_cb (GtkWidget *dialog,
			  gint       response,
			  GncPluginPageAccountTree *page)
{
  GncPluginPageAccountTreePrivate *priv;
  GtkWidget *view;
  guint32 types;

  g_return_if_fail(GTK_IS_DIALOG(dialog));
  g_return_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page));

  ENTER("dialog %p, response %d, page %p", dialog, response, page);
  view = gnc_glade_lookup_widget(dialog, FILTER_TREE_VIEW);

  priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
  if (response != GTK_RESPONSE_OK) {
    priv->fd.visible_types = priv->fd.original_visible_types;
    priv->fd.hide_zero_total = priv->fd.original_hide_zero_total;
    gnc_tree_view_account_refilter(GNC_TREE_VIEW_ACCOUNT(priv->tree_view));
  }
  types = gnc_tree_model_account_types_get_selection(GTK_TREE_VIEW(view));

  /* Clean up and delete dialog */
  priv->fd.selection_changed_cb_id = 0;
  g_atomic_pointer_compare_and_exchange((gpointer *)&priv->fd.dialog,
					dialog, NULL);
  gtk_widget_destroy(dialog);
  LEAVE("types 0x%x", types);
}

static void
gnc_plugin_page_account_tree_cmd_view_filter_by (GtkAction *action,
						 GncPluginPageAccountTree *page)
{
  GncPluginPageAccountTreePrivate *priv;
  GtkWidget *dialog, *button;
  GtkTreeView *view;
  GtkTreeSelection *selection;
  GladeXML *xml;
  gchar *title;

  g_return_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page));
  ENTER("(action %p, page %p)", action, page);

  priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
  if (priv->fd.dialog) {
    gtk_window_present(GTK_WINDOW(priv->fd.dialog));
    LEAVE("existing dialog");
    return;
  }

  /* Create the dialog */
  xml = gnc_glade_xml_new ("account.glade", "Filter By");
  dialog = glade_xml_get_widget (xml, "Filter By");
  priv->fd.dialog = dialog;
  gtk_window_set_transient_for(GTK_WINDOW(dialog),
			       GTK_WINDOW(GNC_PLUGIN_PAGE(page)->window));
  /* Translators: The %s is the name of the plugin page */
  title = g_strdup_printf(_("Filter %s by..."),
			  gnc_plugin_page_get_page_name(GNC_PLUGIN_PAGE(page)));
  gtk_window_set_title(GTK_WINDOW(dialog), title);
  g_free(title);

  /* Remember current state */
  priv->fd.original_visible_types = priv->fd.visible_types;
  priv->fd.original_hide_zero_total = priv->fd.hide_zero_total;

  /* Update the dialog widgets for the current state */
  button = glade_xml_get_widget (xml, "hide_zero");
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button),
			       priv->fd.hide_zero_total);

  view = GTK_TREE_VIEW(glade_xml_get_widget (xml, FILTER_TREE_VIEW));
  priv->fd.model = gnc_tree_model_account_types_master();
  gtk_tree_view_set_model(view, priv->fd.model);
  gtk_tree_view_insert_column_with_attributes
    (view,
     -1, _("Account Types"), gtk_cell_renderer_text_new(),
     "text", GNC_TREE_MODEL_ACCOUNT_TYPES_COL_NAME, NULL);
  selection = gtk_tree_view_get_selection(view);
  gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
  gnc_tree_model_account_types_set_selection(view, priv->fd.visible_types);
  priv->fd.selection_changed_cb_id =
    g_signal_connect(G_OBJECT(selection), "changed",
		     G_CALLBACK(gppat_filter_selection_changed_cb), page);

  /* Wire up the rest of the callbacks */
  glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func, page);

  /* Show it */
  gtk_widget_show_all(dialog);
  LEAVE(" ");
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
