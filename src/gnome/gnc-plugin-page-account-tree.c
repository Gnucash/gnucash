/* 
 * gnc-plugin-page-account-tree.c -- 
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003,2005,2006 David Hampton <hampton@employees.org>
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
    @author Copyright (C) 2003,2005,2006 David Hampton <hampton@employees.org>
*/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "gnc-plugin-page-account-tree.h"
#include "gnc-plugin-page-register.h"

#include "Scrub.h"
#include "Scrub3.h"
#include "Transaction.h"
#include "dialog-account.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "druid-hierarchy.h"
#include "gnc-account-sel.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-gconf-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-gobject-utils.h"
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

#define DELETE_DIALOG_FILTER  "filter"
#define DELETE_DIALOG_ACCOUNT "account"

enum {
  ACCOUNT_SELECTED,
  LAST_SIGNAL
};

typedef struct GncPluginPageAccountTreePrivate
{
	GtkWidget   *widget;
	GtkTreeView *tree_view;
	gint         component_id;
	AccountFilterDialog fd;
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
static void gnc_plugin_page_account_tree_summarybar_position_changed(GConfEntry *entry,
								     gpointer user_data);
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

/* Command callbacks */
static void gnc_plugin_page_account_tree_cmd_new_account (GtkAction *action, GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_cmd_file_new_hierarchy (GtkAction *action, GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_cmd_open_account (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_open_subaccounts (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_edit_account (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_delete_account (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_renumber_accounts (GtkAction *action, GncPluginPageAccountTree *page);
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
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_file_new_hierarchy) },
	{ "FileOpenAccountAction", GNC_STOCK_OPEN_ACCOUNT, N_("Open _Account"), NULL,
	  N_("Open the selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_open_account) },
	{ "FileOpenSubaccountsAction", GNC_STOCK_OPEN_ACCOUNT, N_("Open _Subaccounts"), NULL,
	  N_("Open the selected account and all its subaccounts"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_open_subaccounts) },

	/* Edit menu */
	{ "EditEditAccountAction", GNC_STOCK_EDIT_ACCOUNT, N_("Edit _Account"), "<control>e",
	  N_("Edit the selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_edit_account) },
	{ "EditDeleteAccountAction", GNC_STOCK_DELETE_ACCOUNT, N_("_Delete Account..."), "Delete",
	  N_("Delete selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_delete_account) },
	{ "EditRenumberSubaccountsAction", NULL, N_("_Renumber Subaccounts..."), NULL,
	  N_("Renumber the children of the selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_renumber_accounts) },

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
	{ "ScrubSubAction", NULL, N_("Check & Repair Su_baccounts"), NULL,
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
	const GList *page_list;

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

	/* Is this the first accounts page? */
	page_list =
	  gnc_gobject_tracking_get_list(GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME);
	if (plugin_page == page_list->data) {
	  g_object_set_data(G_OBJECT(plugin_page), PLUGIN_PAGE_IMMUTABLE,
			    GINT_TO_POINTER(1));
	}

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
	priv->fd.show_hidden = FALSE;
	priv->fd.show_zero_total = TRUE;
	
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
	GtkTreeViewColumn *col;

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
	col = gnc_tree_view_find_column_by_name(
	    GNC_TREE_VIEW(tree_view), "description");
	g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
	col = gnc_tree_view_find_column_by_name(
	    GNC_TREE_VIEW(tree_view), "total");
	g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
	gnc_tree_view_configure_columns(GNC_TREE_VIEW(tree_view));
	g_object_set(G_OBJECT(tree_view),
		     "gconf-section", GCONF_SECTION,
		     "show-column-menu", TRUE,
		     NULL);

        /* No name handler; then the user can't click on the name of the
           account to open its register. */
        gnc_tree_view_account_set_code_edited(GNC_TREE_VIEW_ACCOUNT(tree_view),
                                              gnc_tree_view_account_code_edited_cb);
        gnc_tree_view_account_set_description_edited(GNC_TREE_VIEW_ACCOUNT(tree_view),
                                                     gnc_tree_view_account_description_edited_cb);
        gnc_tree_view_account_set_notes_edited(GNC_TREE_VIEW_ACCOUNT(tree_view),
                                               gnc_tree_view_account_notes_edited_cb);

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

	priv->fd.tree_view = GNC_TREE_VIEW_ACCOUNT(priv->tree_view);
	gnc_tree_view_account_set_filter (
            GNC_TREE_VIEW_ACCOUNT(tree_view),
            gnc_plugin_page_account_tree_filter_accounts, &priv->fd, NULL);

	priv->component_id =
	  gnc_register_gui_component(PLUGIN_PAGE_ACCT_TREE_CM_CLASS,
				     gnc_plugin_page_account_refresh_cb,
				     gnc_plugin_page_account_tree_close_cb,
				     page);
	gnc_gui_component_set_session (priv->component_id,
				       gnc_get_current_session());

	plugin_page->summarybar = gnc_main_window_summary_new();
	gtk_box_pack_start (GTK_BOX (priv->widget), plugin_page->summarybar,
			    FALSE, FALSE, 0);
	gtk_widget_show(plugin_page->summarybar);
	gnc_plugin_page_account_tree_summarybar_position_changed(NULL, page);
	gnc_gconf_general_register_cb(KEY_SUMMARYBAR_POSITION,
		gnc_plugin_page_account_tree_summarybar_position_changed,
		page);

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

	gnc_gconf_general_remove_cb(KEY_SUMMARYBAR_POSITION,
		gnc_plugin_page_account_tree_summarybar_position_changed,
		page);

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
	
	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(plugin_page));
	g_return_if_fail (key_file != NULL);
	g_return_if_fail (group_name != NULL);

	ENTER("page %p, key_file %p, group_name %s", plugin_page, key_file,
	      group_name);

	account_page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(plugin_page);
	priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(account_page);

        gnc_tree_view_account_save(GNC_TREE_VIEW_ACCOUNT(priv->tree_view), 
                                   &priv->fd, key_file, group_name);
	LEAVE(" ");
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
	
	g_return_val_if_fail(key_file, NULL);
	g_return_val_if_fail(group_name, NULL);
	ENTER("key_file %p, group_name %s", key_file, group_name);

	/* Create the new page. */
	page = gnc_plugin_page_account_tree_new();
	account_page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(page);
	priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(account_page);

	/* Install it now so we can then manipulate the created widget */
	gnc_main_window_open_page(GNC_MAIN_WINDOW(window), page);

        gnc_tree_view_account_restore(GNC_TREE_VIEW_ACCOUNT(priv->tree_view), 
                                      &priv->fd, key_file, group_name);
	LEAVE(" ");
	return page;
}


/* Callbacks */

static void
gnc_plugin_page_account_tree_summarybar_position_changed(GConfEntry *entry,
							 gpointer user_data)
{
	GncPluginPage *plugin_page;
	GncPluginPageAccountTree *page;
	GncPluginPageAccountTreePrivate *priv;
	GtkPositionType position = GTK_POS_BOTTOM;
	gchar *conf_string;
	
	g_return_if_fail(user_data != NULL);
	
	plugin_page = GNC_PLUGIN_PAGE(user_data);
	page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (user_data);
	priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
	
	conf_string = gnc_gconf_get_string (GCONF_GENERAL,
					    KEY_SUMMARYBAR_POSITION, NULL);
	if (conf_string) {
		position = gnc_enum_from_nick (GTK_TYPE_POSITION_TYPE,
					       conf_string, GTK_POS_BOTTOM);
		g_free (conf_string);
	}

	gtk_box_reorder_child(GTK_BOX(priv->widget),
			      plugin_page->summarybar,
			      (position == GTK_POS_TOP ? 0 : -1) );
}

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

  /* Always return FALSE.  This will let the tree view callback run as
   * well which will select the item under the cursor.  By the time
   * the user sees the menu both callbacks will have run and the menu
   * actions will operate on the just-selected account. */
  return FALSE;
}

static void
gppat_open_account_common (GncPluginPageAccountTree *page,
			   Account *account,
			   gboolean include_subs)
{
	GncPluginPageAccountTreePrivate *priv;
	GtkWidget *window;
	GncPluginPage *new_page;

	if (account == NULL)
	  return;

	priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
	window = GNC_PLUGIN_PAGE (page)->window;
	new_page = gnc_plugin_page_register_new (account, include_subs);
	gnc_main_window_open_page (GNC_MAIN_WINDOW(window), new_page);
}

static void
gnc_plugin_page_account_tree_double_click_cb (GtkTreeView        *treeview,
					      GtkTreePath        *path,
					      GtkTreeViewColumn  *col,
					      GncPluginPageAccountTree *page)
{
	Account *account;

	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
	account = gnc_tree_view_account_get_account_from_path (GNC_TREE_VIEW_ACCOUNT(treeview), path);
	gppat_open_account_common (page, account, FALSE);
}

static void
gnc_plugin_page_account_tree_selection_changed_cb (GtkTreeSelection *selection,
						   GncPluginPageAccountTree *page)
{
	GtkActionGroup *action_group;
	GtkAction *action;
	GtkTreeView *view;
	Account *account = NULL;
	gboolean sensitive;
	gboolean subaccounts;

	g_return_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page));

	if (!selection) {
		sensitive = FALSE;
		subaccounts = FALSE;
	} else {
		g_return_if_fail(GTK_IS_TREE_SELECTION(selection));
		view = gtk_tree_selection_get_tree_view (selection);
		account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(view));
		sensitive = (account != NULL);

		subaccounts = account && (gnc_account_n_children(account) != 0);
		/* Check here for placeholder accounts, etc. */
	}

	action_group = gnc_plugin_page_get_action_group(GNC_PLUGIN_PAGE(page));
	gnc_plugin_update_actions (action_group, actions_requiring_account,
				   "sensitive", sensitive);
	g_signal_emit (page, plugin_page_signals[ACCOUNT_SELECTED], 0, account);

	action = gtk_action_group_get_action (action_group, "EditRenumberSubaccountsAction");
	g_object_set (G_OBJECT(action), "sensitive",
		      sensitive && subaccounts, NULL);

	gnc_plugin_update_actions (action_group, actions_requiring_account,
				   "sensitive", sensitive);
}
	

/* Command callbacks */
static void
gnc_plugin_page_account_tree_cmd_new_account (GtkAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);

	gnc_ui_new_account_window (gnc_get_current_book(), account);
}

static void
gnc_plugin_page_account_tree_cmd_file_new_hierarchy (GtkAction *action, GncPluginPageAccountTree *page)
{
        gnc_ui_hierarchy_druid(FALSE);
}

static void
gnc_plugin_page_account_tree_cmd_open_account (GtkAction *action,
					       GncPluginPageAccountTree *page)
{
	Account *account;

	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
	account = gnc_plugin_page_account_tree_get_current_account (page);
	gppat_open_account_common (page, account, FALSE);
}

static void
gnc_plugin_page_account_tree_cmd_open_subaccounts (GtkAction *action,
						   GncPluginPageAccountTree *page)
{
	Account *account;

	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
	account = gnc_plugin_page_account_tree_get_current_account (page);
	gppat_open_account_common (page, account, TRUE);
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
  gint dmas_cnt, tmas_cnt;
  gboolean sensitive;

  dmas = g_object_get_data(G_OBJECT(dialog), "dmas");
  tmas = g_object_get_data(G_OBJECT(dialog), "tmas");
  dmas_cnt = gnc_account_sel_get_num_account(GNC_ACCOUNT_SEL(dmas));
  tmas_cnt = gnc_account_sel_get_num_account(GNC_ACCOUNT_SEL(tmas));

  sensitive = (((NULL == dmas) ||
		(!GTK_WIDGET_IS_SENSITIVE(GTK_WIDGET(dmas)) || dmas_cnt)) &&
	       ((NULL == tmas) ||
		(!GTK_WIDGET_IS_SENSITIVE(GTK_WIDGET(tmas)) || tmas_cnt)));

  button = gnc_glade_lookup_widget(dialog, "deletebutton");
  gtk_widget_set_sensitive(button, sensitive);
}

static void
gppat_populate_gas_list(GtkWidget *dialog,
			GNCAccountSel *gas,
			gboolean exclude_subaccounts)
{
  Account *account;
  GList *filter;

  g_return_if_fail(GTK_IS_DIALOG(dialog));
  if (gas == NULL)
    return;
  account = g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_ACCOUNT);
  filter = g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_FILTER);

  /* Setting the account type filter triggers GNCAccountSel population. */
  gnc_account_sel_set_acct_filters (gas, filter, NULL);

  /* Accounts to be deleted must be removed. */
  gnc_account_sel_purge_account( gas, account, exclude_subaccounts);

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
  acct_name = xaccAccountGetFullName(account);
  if (!acct_name) {
    acct_name = g_strdup (_("(no name)"));
  }

  splits = xaccAccountGetSplitList(account);

  /*
   * If the account has transactions or child accounts then conduct a
   * dialog to allow the user to specify what should be done with them.
   */
  if ((NULL != splits) || (gnc_account_n_children(account) > 0)) {
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
    g_object_set_data(G_OBJECT(dialog), DELETE_DIALOG_FILTER, filter);
    g_object_set_data(G_OBJECT(dialog), DELETE_DIALOG_ACCOUNT, account);

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
    if (gnc_account_n_children(account) > 0) {
      /*
       * Check for RO txns in descendants
       */
      gnc_account_foreach_descendant_until(account, delete_account_helper,
			      &delete_res);
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
	name = xaccAccountGetFullName(ta);
	format = _("All transactions in this account will be moved to "
		   "the account %s.");
	lines[++i] = g_strdup_printf(format, name);
      } else if (splits) {
	format = _("All transactions in this account will be deleted.");
	lines[++i] = g_strdup_printf("%s", format);
      }
    }
    if (gnc_account_n_children(account) > 0) {
      if (daa) {
	name = xaccAccountGetFullName(daa);
	format = _("All of its sub-accounts will be moved to "
		   "the account %s.");
	lines[++i] = g_strdup_printf(format, name);
      } else {
	format = _("All of its subaccounts will be deleted.");
	lines[++i] = g_strdup_printf("%s", format);
	if (dta) {
	  name = xaccAccountGetFullName(dta);
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
				     "%s", message);
    g_free(message);
    gtk_dialog_add_buttons(GTK_DIALOG(dialog),
			     GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
			     GTK_STOCK_DELETE, GTK_RESPONSE_ACCEPT,
			     (gchar *)NULL);
    gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_CANCEL);
    response = gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);

    if (GTK_RESPONSE_ACCEPT == response) {
      gnc_set_busy_cursor(NULL, TRUE);
      gnc_suspend_gui_refresh ();
      xaccAccountBeginEdit (account);
      if (NULL != daa) {
	GList *acct_list, *ptr;

	xaccAccountBeginEdit (daa);
	acct_list = gnc_account_get_children(account);
	for (ptr = acct_list; ptr; ptr = g_list_next(ptr))
	  gnc_account_append_child (daa, ptr->data);
	g_list_free(acct_list);
	xaccAccountCommitEdit (daa);
      } else if (NULL != dta) {
	/* Move the splits of its subaccounts, if any. */
	gnc_account_foreach_descendant(account,
				       (AccountCb)xaccAccountMoveAllSplits,
				       dta);
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
      gnc_unset_busy_cursor(NULL);
    }
  }
  g_free(acct_name);
}

static void
gnc_plugin_page_account_tree_cmd_renumber_accounts (GtkAction *action,
						    GncPluginPageAccountTree *page)
{
  Account *account;
  GtkWidget *window;

  window = gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page));
  account = gnc_plugin_page_account_tree_get_current_account(page);
  if (!window || !account)
    return;

  gnc_account_renumber_create_dialog(window, account);
}

/*********************/

static void
gnc_plugin_page_account_tree_cmd_view_filter_by (GtkAction *action,
						 GncPluginPageAccountTree *page)
{
  GncPluginPageAccountTreePrivate *priv;

  g_return_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page));
  ENTER("(action %p, page %p)", action, page);

  priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
  account_filter_dialog_create(&priv->fd, GNC_PLUGIN_PAGE(page));
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

	// XXX: Lots are disabled
        if (g_getenv("GNC_AUTO_SCRUB_LOTS") != NULL)
 	    xaccAccountScrubLots(account);

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

	// XXX: Lots are disabled
        if (g_getenv("GNC_AUTO_SCRUB_LOTS") != NULL)
 	    xaccAccountTreeScrubLots(account);

	gnc_resume_gui_refresh ();
}

static void
gnc_plugin_page_account_tree_cmd_scrub_all (GtkAction *action, GncPluginPageAccountTree *page)
{
	Account *root = gnc_get_current_root_account ();

	gnc_suspend_gui_refresh ();

	xaccAccountTreeScrubOrphans (root);
	xaccAccountTreeScrubImbalance (root);
	// XXX: Lots are disabled
        if (g_getenv("GNC_AUTO_SCRUB_LOTS") != NULL)
            xaccAccountTreeScrubLots(root);

	gnc_resume_gui_refresh ();
}

/** @} */
/** @} */
