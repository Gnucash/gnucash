/* 
 * gnc-plugin-page-account-tree.c -- 
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
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

#include <gtk/gtk.h>
#include "egg-action-group.h"

#include "gnc-plugin-page-account-tree.h"
#include "gnc-plugin-page-register.h"

#include "Scrub.h"
#include "Transaction.h"
#include "dialog-account.h"
#include "dialog-options.h"
#include "dialog-transfer.h"
#include "global-options.h"
#include "gnc-book.h"
#include "gnc-component-manager.h"
#include "gnc-gnome-utils.h"
#include "gnc-html.h"
#include "gnc-icons.h"
#include "gnc-plugin-account-tree.h"
#include "gnc-session.h"
#include "gnc-split-reg.h"
#include "gnc-tree-model-account.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "lot-viewer.h"
#include "option-util.h"
#include "window-reconcile.h"
#include "window-register.h"

#include "messages.h"
#include "gnc-engine-util.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;
static GList *active_pages = NULL;

#define PLUGIN_PAGE_ACCT_TREE_CM_CLASS "plugin-page-acct-tree"

enum {
  ACCOUNT_SELECTED,
  LAST_SIGNAL
};


/************************************************************
 *                        Prototypes                        *
 ************************************************************/
/* Plugin Actions */
static void gnc_plugin_page_account_tree_class_init (GncPluginPageAccountTreeClass *klass);
static void gnc_plugin_page_account_tree_init (GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_finalize (GObject *object);

static GtkWidget *gnc_plugin_page_account_tree_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_account_tree_destroy_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_account_tree_merge_actions (GncPluginPage *plugin_page, EggMenuMerge *ui_merge);
static void gnc_plugin_page_account_tree_unmerge_actions (GncPluginPage *plugin_page, EggMenuMerge *ui_merge);

/* Callbacks */
static gboolean gnc_plugin_page_account_tree_popup_menu_cb (GtkTreeView *treeview,
							    GncPluginPageAccountTree *page);
static gboolean gnc_plugin_page_account_tree_button_press_cb (GtkTreeView *treeview,
							      GdkEventButton *event,
			       				      GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_double_click_cb (GtkTreeView        *treeview,
							  GtkTreePath        *path,
							  GtkTreeViewColumn  *col,
							  GncPluginPageAccountTree *page);

static void gnc_plugin_page_account_tree_selection_changed_cb (GtkTreeSelection *selection,
							       GncPluginPageAccountTree *page);

/* Command callbacks */
static void gnc_plugin_page_account_tree_cmd_new_account (EggAction *action, GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_cmd_open_account (EggAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_open_subaccounts (EggAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_edit_account (EggAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_delete_account (EggAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_view_options (EggAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_reconcile (EggAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_transfer (EggAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_stock_split (EggAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_lots (EggAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_scrub (EggAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_scrub_sub (EggAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_scrub_all (EggAction *action, GncPluginPageAccountTree *page);


static void gnc_plugin_page_acct_tree_options_new(GncPluginPageAccountTreePrivate *priv);
static void gnc_plugin_page_account_tree_configure (GncPluginPageAccountTreePrivate *priv);


static guint plugin_page_signals[LAST_SIGNAL] = { 0 };


static EggActionEntry gnc_plugin_page_account_tree_actions [] = {
	/* Toplevel */
	{ "FakeToplevel", "", NULL, NULL, NULL, NULL },

	/* File menu */
	{ "FileNewAccountAction", N_("New Account..."), GNC_STOCK_NEW_ACCOUNT, NULL,
	  N_("Create a new Account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_new_account) },
	{ "FileOpenAccountAction", N_("Open Account"), GNC_STOCK_OPEN_ACCOUNT, "<control>o",
	  N_("Open the selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_open_account) },
	{ "FileOpenSubaccountsAction", N_("Open _Subaccounts"), GNC_STOCK_OPEN_ACCOUNT, NULL,
	  N_("Open the selected account and all its subaccounts"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_open_subaccounts) },

	/* Edit menu */
	{ "EditEditAccountAction", N_("_Edit Acount"), GNC_STOCK_EDIT_ACCOUNT, "<control>e",
	  N_("Edit the selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_edit_account) },
	{ "EditDeleteAccountAction", N_("_Delete Acount"), GNC_STOCK_DELETE_ACCOUNT, NULL,
	  N_("Delete selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_delete_account) },
	{ "EditAccountViewOptionsAction", N_("Account Tree Options"), GTK_STOCK_PROPERTIES, NULL,
	  N_("Edit the account view options"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_view_options) },

	/* Actions menu */
	{ "ActionsReconcileAction", N_("_Reconcile..."), NULL, "<control>r",
	  N_("Reconcile the selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_reconcile) },
	{ "ActionsTransferAction", N_("_Transfer..."), NULL, "<control>t",
	  N_("Transfer funds from one account to another"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_transfer) },
	{ "ActionsStockSplitAction", N_("Stock S_plit..."), NULL, NULL,
	  N_("Record a stock split or a stock merger"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_stock_split) },
	{ "ActionsLotsAction", N_("View _Lots..."), NULL, NULL,
	  N_("Bring up the lot viewer/editor window"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_lots) },
	{ "ScrubMenuAction", N_("Check & Repair"), NULL, NULL, NULL, NULL },
	{ "ScrubAction", N_("Check & Repair A_ccount"), NULL, NULL,
	  N_("Check for and repair unbalanced transactions and orphan splits " "in this account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_scrub) },
	{ "ScrubSubAction", N_("Check & Repair Su_baccount"), NULL, NULL,
	  N_("Check for and repair unbalanced transactions and orphan splits "
             "in this account and its subaccounts"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_scrub_sub) },
	{ "ScrubAllAction", N_("Check & Repair A_ll"), NULL, NULL,
	  N_("Check for and repair unbalanced transactions and orphan splits " "in all accounts"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_scrub_all) },
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

/* DRH - Suggest this be added to libegg */
static action_short_labels short_labels[] = {
  { "FileOpenAccountAction", 	    N_("Open") },
  { "EditEditAccountAction", 	    N_("Edit") },
  { "EditAccountViewOptionsAction", N_("Options") },
  { "FileNewAccountAction",    	    N_("New") },
  { "EditDeleteAccountAction", 	    N_("Delete") },
  { NULL, NULL },
};

struct GncPluginPageAccountTreePrivate
{
	EggActionGroup *action_group;
	guint merge_id;
	EggMenuMerge *ui_merge;

	GtkWidget *widget;
	GtkTreeView *tree_view;

	SCM         euro_change_callback_id;
	SCM         name_change_callback_id;

	GNCOptionDB * odb;
	SCM         options; 
	int         options_id;
	GNCOptionWin * editor_dialog;

	GtkWidget *options_db;
	gint       component_id;
};

static GObjectClass *parent_class = NULL;


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
								            "GncPluginPageAccountTree",
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

#if DEBUG_REFERENCE_COUNTING
static void
dump_model (GncPluginPageAccountTree *page, gpointer dummy)
{
    g_warning("GncPluginPageAccountTree %p still exists.", page);
}

static gint
gnc_plugin_page_account_tree_report_references (void)
{
  g_list_foreach(active_pages, (GFunc)dump_model, NULL);
  return 0;
}
#endif

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
	gnc_plugin_class->merge_actions   = gnc_plugin_page_account_tree_merge_actions;
	gnc_plugin_class->unmerge_actions = gnc_plugin_page_account_tree_unmerge_actions;

	plugin_page_signals[ACCOUNT_SELECTED] =
	  g_signal_new ("account_selected",
			G_OBJECT_CLASS_TYPE (object_class),
			G_SIGNAL_RUN_FIRST,
			G_STRUCT_OFFSET (GncPluginPageAccountTreeClass, account_selected),
			NULL, NULL,
			g_cclosure_marshal_VOID__POINTER,
			G_TYPE_NONE, 1,
			G_TYPE_POINTER);

#if DEBUG_REFERENCE_COUNTING
	gtk_quit_add (0,
		      (GtkFunction)gnc_plugin_page_account_tree_report_references,
		      NULL);
#endif
}

static void
gnc_euro_change (gpointer data)
{
  /* gnc_acct_tree_window_configure (data); */
  gnc_gui_refresh_all ();
}

static void
gnc_plugin_page_acct_tree_view_refresh (gpointer data)
{
  //  gnc_mdi_child_refresh (data);
}

static void
gnc_plugin_page_account_tree_init (GncPluginPageAccountTree *plugin_page)
{
	EggActionGroup *action_group;
	GncPluginPageAccountTreePrivate *priv;
	GncPluginPage *parent;
	const gchar *url = NULL;
	int options_id;
	SCM find_options;
	SCM temp;
	URLType type;

	ENTER("page %p", plugin_page);
	priv = plugin_page->priv = g_new0 (GncPluginPageAccountTreePrivate, 1);

	/* Init parent declared variables */
	parent = GNC_PLUGIN_PAGE(plugin_page);
	parent->title       = g_strdup(_("Accounts"));
	parent->tab_name    = g_strdup(_("Accounts"));
	parent->uri         = g_strdup("default:");

	/* change me when the system supports multiple books */
	gnc_plugin_page_add_book(parent, gnc_get_current_book());

	/* Create menu and toolbar information */
	action_group = egg_action_group_new ("GncPluginPageAccountTreeActions");
	priv->action_group = action_group;
	egg_action_group_add_actions (action_group,
				      gnc_plugin_page_account_tree_actions,
				      gnc_plugin_page_account_tree_n_actions,
				      plugin_page);
	gnc_gnome_utils_init_short_names (action_group, short_labels);

	
	/* get the options and the window ID */ 
	priv->options = SCM_BOOL_F;
	scm_protect_object(priv->options);
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
	      scm_unprotect_object(priv->options);
	      priv->options = temp;
	      scm_protect_object(priv->options);
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

	priv->euro_change_callback_id =
	  gnc_register_option_change_callback(gnc_euro_change, priv,
					      "International",
					      "Enable EURO support");
	priv->name_change_callback_id = 
	  gnc_option_db_register_change_callback(priv->odb, 
						 gnc_plugin_page_acct_tree_view_refresh,
						 priv, 
						 N_("Account Tree"),
						 N_("Name of account view"));
	scm_protect_object(priv->name_change_callback_id);

	active_pages = g_list_append (active_pages, plugin_page);

	LEAVE("page %p, priv %p, action group %p",
	      plugin_page, plugin_page->priv, plugin_page->priv->action_group);
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
	priv = page->priv;
	g_return_if_fail (priv != NULL);

	active_pages = g_list_remove (active_pages, page);

	/* Options stuff */
	gnc_unregister_option_change_callback_id(priv->euro_change_callback_id);

	if (priv->editor_dialog) {
	  gnc_options_dialog_destroy(priv->editor_dialog);
	  priv->editor_dialog = NULL;
	}

	gnc_option_db_destroy(priv->odb);

	free_tree = scm_c_eval_string("gnc:free-acct-tree-window");
	scm_call_1(free_tree, scm_int2num(priv->options_id));
	priv->options_id = 0;

	scm_unprotect_object(priv->options);

	g_free (priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
	LEAVE(" ");
}

Account *
gnc_plugin_page_account_tree_get_current_account (GncPluginPageAccountTree *page)
{
	Account *account;

	ENTER("page %p (tree view %p)", page, page->priv->tree_view);
	account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(page->priv->tree_view));
	if (account == NULL) {
		LEAVE("no account");
		return NULL;
	}

	LEAVE("account %p", account);
	return account;
}


/* Virtual Functions */

static void
gnc_plugin_page_account_tree_close_cb (gpointer user_data)
{
  GncPluginPage *page;

  page = GNC_PLUGIN_PAGE(user_data);
  gnc_main_window_close_page (GNC_MAIN_WINDOW(page->window), page);
}

static GtkWidget *
gnc_plugin_page_account_tree_create_widget (GncPluginPage *plugin_page)
{
	GncPluginPageAccountTree *page;
	GtkTreeSelection *selection;
	GtkTreeView *tree_view;
	GtkWidget *scrolled_window;

	ENTER("page %p", plugin_page);
	page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page);
	if (page->priv->widget != NULL) {
		LEAVE("widget = %p", page->priv->widget);
		return page->priv->widget;
	}

	page->priv->widget = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (page->priv->widget);

	scrolled_window = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
					GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_show (scrolled_window);
	gtk_box_pack_start (GTK_BOX (page->priv->widget), scrolled_window,
			    TRUE, TRUE, 0);

	tree_view = gnc_tree_view_account_new(FALSE);
	page->priv->tree_view = tree_view;
	selection = gtk_tree_view_get_selection(tree_view);
	g_signal_connect (G_OBJECT (selection), "changed",
			  G_CALLBACK (gnc_plugin_page_account_tree_selection_changed_cb), page);
	g_signal_connect (G_OBJECT (tree_view), "popup-menu",
			  G_CALLBACK (gnc_plugin_page_account_tree_popup_menu_cb), page);
	g_signal_connect (G_OBJECT (tree_view), "button-press-event",
			  G_CALLBACK (gnc_plugin_page_account_tree_button_press_cb), page);
	g_signal_connect (G_OBJECT (tree_view), "row-activated",
			  G_CALLBACK (gnc_plugin_page_account_tree_double_click_cb), page);

	gtk_tree_view_set_headers_visible(tree_view, TRUE);
	gnc_plugin_page_account_tree_configure (page->priv);
	gnc_plugin_page_account_tree_selection_changed_cb (NULL, page);
	gtk_widget_show (GTK_WIDGET (tree_view));
	gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET(tree_view));

	page->priv->component_id =
	  gnc_register_gui_component(PLUGIN_PAGE_ACCT_TREE_CM_CLASS,
				     NULL,
				     gnc_plugin_page_account_tree_close_cb,
				     page);
	gnc_gui_component_set_session (page->priv->component_id,
				       gnc_get_current_session());

	LEAVE("widget = %p", page->priv->widget);
	return page->priv->widget;
}

static void
gnc_plugin_page_account_tree_destroy_widget (GncPluginPage *plugin_page)
{
	GncPluginPageAccountTree *page;

	ENTER("page %p", plugin_page);
	page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page);
	if (page->priv->widget) {
	  g_object_unref(G_OBJECT(page->priv->widget));
	  page->priv->widget = NULL;
	}

	if (page->priv->component_id) {
	  gnc_unregister_gui_component(page->priv->component_id);
	  page->priv->component_id = 0;
	}

	LEAVE("widget destroyed");
}

static void
gnc_plugin_page_account_tree_merge_actions (GncPluginPage *plugin_page,
					    EggMenuMerge *ui_merge)
{
	GncPluginPageAccountTree *account_page;
	GncPluginPageAccountTreePrivate *priv;
	
	ENTER("page %p, ui_merge %p", plugin_page, ui_merge);

	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page));

	account_page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(plugin_page);
	priv = account_page->priv;

	priv->ui_merge = ui_merge;
	priv->merge_id =
	  gnc_menu_merge_add_actions (priv->ui_merge,
				      priv->action_group,
				      "gnc-plugin-page-account-tree-ui.xml");
	LEAVE(" ");
}
	
static void
gnc_plugin_page_account_tree_unmerge_actions (GncPluginPage *plugin_page,
					      EggMenuMerge *ui_merge)
{
	GncPluginPageAccountTree *plugin_page_account_tree = GNC_PLUGIN_PAGE_ACCOUNT_TREE(plugin_page);
	
	ENTER("page %p (merge_id %d, action_group %p), ui_merge %p",
	      plugin_page,
	      plugin_page_account_tree->priv->merge_id,
	      plugin_page_account_tree->priv->action_group,
	      ui_merge);
	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page_account_tree));
	g_return_if_fail (plugin_page_account_tree->priv->merge_id != 0);
	g_return_if_fail (plugin_page_account_tree->priv->action_group != NULL);

	egg_menu_merge_remove_ui (ui_merge, plugin_page_account_tree->priv->merge_id);
	egg_menu_merge_remove_action_group (ui_merge, plugin_page_account_tree->priv->action_group);

	plugin_page_account_tree->priv->ui_merge = NULL;
	LEAVE(" ");
}


/* Callbacks */
static gboolean
gnc_plugin_page_account_tree_button_press_cb (GtkTreeView *treeview,
					      GdkEventButton *event,
	       				      GncPluginPageAccountTree *page)
{
	GtkWidget *menu;
	gint button;
	guint32 time;

	ENTER("tree %p, event %p, page %p", treeview, event, page);

	if (event && event->button != 3) {
	  LEAVE("not button 3");
	  return FALSE;
	}

	if (page->priv->ui_merge == NULL) {
	  LEAVE("no ui merge");
	  return FALSE;
	}

	button = event ? event->button : 0;
	time = event ? event->time : 0;

	/* Maybe show a different popup menu if no account is selected. */
	menu = egg_menu_merge_get_widget (page->priv->ui_merge, "/AccountPopup");
	if (!menu) {
	  LEAVE("no menu");
	  return FALSE;
	}

	gtk_menu_popup (GTK_MENU(menu), NULL, NULL, NULL, NULL, button, time);
	LEAVE(" ");
	return TRUE;
}

static gboolean
gnc_plugin_page_account_tree_popup_menu_cb (GtkTreeView *treeview,
					    GncPluginPageAccountTree *page)
{
	gboolean result;

	ENTER("tree %p, page %p", treeview, page);
	result = gnc_plugin_page_account_tree_button_press_cb (treeview, NULL, page);
	LEAVE("result %d", result);
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
	EggActionGroup *action_group;
	EggAction *action;
	GtkTreeView *view;
	Account *account = NULL;
	GValue value = { 0 };
	gboolean sensitive;
	gint i;

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

	g_value_init (&value, G_TYPE_BOOLEAN);
	g_value_set_boolean (&value, sensitive);
	action_group = page->priv->action_group;
	for (i = 0; actions_requiring_account[i]; i++) {
	  	action = egg_action_group_get_action (action_group,
						      actions_requiring_account[i]);
		g_object_set_property (G_OBJECT(action), "sensitive", &value);
	}

	g_signal_emit (page, plugin_page_signals[ACCOUNT_SELECTED], 0, account);
}
	

/* Command callbacks */
static void
gnc_plugin_page_account_tree_cmd_new_account (EggAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);

	gnc_ui_new_account_window_with_default (NULL, account);
}

static void
gnc_plugin_page_account_tree_cmd_open_account (EggAction *action,
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
gnc_plugin_page_account_tree_cmd_open_subaccounts (EggAction *action,
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
gnc_plugin_page_account_tree_cmd_edit_account (EggAction *action, GncPluginPageAccountTree *page)
{
	Account *account;

	ENTER("action %p, page %p (merge_id %d, action_group %p)",
	      action, page, page->priv->merge_id, page->priv->action_group);

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

	return (gpointer) (helper_res->has_splits || helper_res->has_ro_splits);
}

static void
gnc_plugin_page_account_tree_cmd_delete_account (EggAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);

	g_return_if_fail (account != NULL);

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
gnc_plugin_page_account_tree_configure (GncPluginPageAccountTreePrivate *priv)
{
  GtkTreeView *tree_view;
  GSList *list;

  ENTER(" ");
  tree_view = priv->tree_view;
  list = gnc_option_db_lookup_list_option(priv->odb, 
                                          "Account Tree",
                                          "Account fields to display",
                                          NULL);
  gnc_tree_view_account_configure_columns (GNC_TREE_VIEW_ACCOUNT(tree_view), list);
  gnc_free_list_option_value (list);
  LEAVE(" ");
}

static void
gnc_plugin_page_account_tree_options_apply_cb (GNCOptionWin * propertybox,
					       gpointer user_data)
{
  GncPluginPageAccountTreePrivate *priv = user_data;
  if(!priv)
    return;

  ENTER(" ");
  gnc_option_db_commit(priv->odb);
  gnc_plugin_page_account_tree_configure (priv);
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

  scm_unprotect_object(priv->options);
  func = scm_c_eval_string("gnc:make-new-acct-tree-window");
  opts_and_id = scm_call_0(func);
  priv->options = SCM_CAR(opts_and_id);
  scm_protect_object(priv->options);
  priv->options_id = scm_num2int(SCM_CDR(opts_and_id), SCM_ARG1, __FUNCTION__);
}

/*********************/

static void
gnc_plugin_page_account_tree_cmd_view_options (EggAction *action, GncPluginPageAccountTree *page)
{
  GncPluginPageAccountTreePrivate *priv;

  g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
  priv = page->priv;

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
gnc_plugin_page_account_tree_cmd_reconcile (EggAction *action,
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
gnc_plugin_page_account_tree_cmd_transfer (EggAction *action,
					   GncPluginPageAccountTree *page)
{
	GtkWidget *window;
	Account *account;

	account = gnc_plugin_page_account_tree_get_current_account (page);
	window = GNC_PLUGIN_PAGE (page)->window;
	gnc_xfer_dialog (window, account);
}

static void
gnc_plugin_page_account_tree_cmd_stock_split (EggAction *action,
					      GncPluginPageAccountTree *page)
{
	GtkWidget *window;
	Account *account;

	account = gnc_plugin_page_account_tree_get_current_account (page);
	window = GNC_PLUGIN_PAGE (page)->window;
	gnc_stock_split_dialog (window, account);
}

static void
gnc_plugin_page_account_tree_cmd_lots (EggAction *action,
				       GncPluginPageAccountTree *page)
{
	Account *account;

	account = gnc_plugin_page_account_tree_get_current_account (page);
	gnc_lot_viewer_dialog (account);
}

static void
gnc_plugin_page_account_tree_cmd_scrub (EggAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);

	g_return_if_fail (account != NULL);

	gnc_suspend_gui_refresh ();

	xaccAccountScrubOrphans (account);
	xaccAccountScrubImbalance (account);

	gnc_resume_gui_refresh ();
}

static void
gnc_plugin_page_account_tree_cmd_scrub_sub (EggAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);

	g_return_if_fail (account != NULL);

	gnc_suspend_gui_refresh ();

	xaccAccountTreeScrubOrphans (account);
	xaccAccountTreeScrubImbalance (account);

	gnc_resume_gui_refresh ();
}

static void
gnc_plugin_page_account_tree_cmd_scrub_all (EggAction *action, GncPluginPageAccountTree *page)
{
	AccountGroup *group = gnc_get_current_group ();

	gnc_suspend_gui_refresh ();

	xaccGroupScrubOrphans (group);
	xaccGroupScrubImbalance (group);
}
