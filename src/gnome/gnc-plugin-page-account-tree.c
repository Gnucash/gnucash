/* 
 * gnc-plugin-page-account-tree.c -- 
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include "config.h"

#include <gtk/gtkcellrenderertext.h>
#include <gtk/gtktreeview.h>
#include <gtk/gtkvbox.h>
#include "egg-action-group.h"

#include "gnc-plugin-page-account-tree.h"

#include "AccWindow.h"
#include "Scrub.h"
#include "Transaction.h"
#include "dialog-account.h"
#include "dialog-options.h"
#include "dialog-transfer.h"
#include "global-options.h"
#include "gnc-book.h"
#include "gnc-component-manager.h"
#include "gnc-html.h"
#include "gnc-icons.h"
#include "gnc-plugin-account-tree.h"
#include "gnc-split-reg.h"
#include "gnc-tree-model-account.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "option-util.h"
#include "window-reconcile.h"
#include "window-register.h"

#include "messages.h"
#include "gnc-engine-util.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;


static void gnc_plugin_page_account_tree_class_init (GncPluginPageAccountTreeClass *klass);
static void gnc_plugin_page_account_tree_init (GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_finalize (GObject *object);

static Account *gnc_plugin_page_account_tree_get_current_account (GncPluginPageAccountTree *page);

static void gnc_plugin_page_account_tree_plugin_page_init (GncPluginPageIface *iface);

static GtkWidget *gnc_plugin_page_account_tree_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_account_tree_merge_actions (GncPluginPage *plugin_page, EggMenuMerge *ui_merge);
static void gnc_plugin_page_account_tree_unmerge_actions (GncPluginPage *plugin_page, EggMenuMerge *ui_merge);
static G_CONST_RETURN gchar *gnc_plugin_page_account_tree_get_title (GncPluginPage *plugin_page);
static G_CONST_RETURN gchar *gnc_plugin_page_account_tree_get_icon (GncPluginPage *plugin_page);
static G_CONST_RETURN gchar *gnc_plugin_page_account_tree_get_plugin_name (GncPluginPage *plugin_page);
static G_CONST_RETURN gchar *gnc_plugin_page_account_tree_get_uri (GncPluginPage *plugin_page);

/* Callbacks */
static gboolean gnc_plugin_page_account_tree_button_press_cb (GtkWidget *widget,
							      GdkEventButton *event,
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
static void gnc_plugin_page_account_tree_cmd_scrub (EggAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_scrub_sub (EggAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_scrub_all (EggAction *action, GncPluginPageAccountTree *page);


static void gnc_plugin_page_acct_tree_options_new(GncPluginPageAccountTreePrivate *priv);

static EggActionGroupEntry gnc_plugin_page_account_tree_actions [] = {
	/* Toplevel */
	{ "FakeToplevel", "", NULL, NULL, NULL, NULL, NULL },

	/* File menu */
	{ "FileNewAccountAction", N_("New Account..."), GTK_STOCK_ADD, NULL,
	  N_("Create a new Account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_new_account), NULL },
	{ "FileOpenAccountAction", N_("Open Account"), GNC_STOCK_OPEN_ACCOUNT, "<control>o",
	  N_("Open the selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_open_account), NULL },
	{ "FileOpenSubaccountsAction", N_("Open _Subaccounts"), GNC_STOCK_OPEN_ACCOUNT, NULL,
	  N_("Open the selected account and all its subaccounts"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_open_subaccounts), NULL },

	/* Edit menu */
	{ "EditEditAccountAction", N_("_Edit Acount"), GNC_STOCK_EDIT_ACCOUNT, "<control>e",
	  N_("Edit the selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_edit_account), NULL },
	{ "EditDeleteAccountAction", N_("_Delete Acount"), GNC_STOCK_DELETE_ACCOUNT, NULL,
	  N_("Delete selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_delete_account), NULL },
	{ "EditAccountViewOptionsAction", N_("Account Tree Options"), GTK_STOCK_PROPERTIES, NULL,
	  N_("Edit the account view options"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_view_options), NULL },

	/* Actions menu */
	{ "ActionsReconcileAction", N_("_Reconcile..."), NULL, "<control>r",
	  N_("Reconcile the selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_reconcile), NULL },
	{ "ActionsTransferAction", N_("_Transfer..."), NULL, "<control>t",
	  N_("Transfer funds from one account to another"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_transfer), NULL },
	{ "ActionsStockSplitAction", N_("Stock S_plit..."), NULL, NULL,
	  N_("Record a stock split or a stock merger"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_stock_split), NULL },
	{ "ScrubMenuAction", N_("Check & Repair"), NULL, NULL, NULL, NULL, NULL },
	{ "ScrubAction", N_("Check & Repair A_ccount"), NULL, NULL,
	  N_("Check for and repair unbalanced transactions and orphan splits " "in this account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_scrub), NULL },
	{ "ScrubSubAction", N_("Check & Repair Su_baccount"), NULL, NULL,
	  N_("Check for and repair unbalanced transactions and orphan splits "
             "in this account and its subaccounts"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_scrub_sub), NULL },
	{ "ScrubAllAction", N_("Check & Repair A_ll"), NULL, NULL,
	  N_("Check for and repair unbalanced transactions and orphan splits " "in all accounts"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_scrub_all), NULL },
};
static guint gnc_plugin_page_account_tree_n_actions = G_N_ELEMENTS (gnc_plugin_page_account_tree_actions);

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
		
		static const GInterfaceInfo plugin_page_info = {
			(GInterfaceInitFunc) gnc_plugin_page_account_tree_plugin_page_init,
			NULL,
			NULL
		};

		gnc_plugin_page_account_tree_type = g_type_register_static (G_TYPE_OBJECT,
								            "GncPluginPageAccountTree",
								            &our_info, 0);

		g_type_add_interface_static (gnc_plugin_page_account_tree_type,
					     GNC_TYPE_PLUGIN_PAGE,
					     &plugin_page_info);
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

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_plugin_page_account_tree_finalize;
}

/* DRH - Suggest this be added to libegg */
static void
gnc_plugin_page_account_tree_init_short_names (EggActionGroup *action_group)
{
	EggAction *action;
	GValue value = { 0, };

	g_value_init (&value, G_TYPE_STRING);

	/* Add a couple of short labels for the toolbar */
	action = egg_action_group_get_action (action_group, "FileOpenAccountAction");
	g_value_set_static_string (&value, _("Open"));
	g_object_set_property (G_OBJECT(action), "short_label", &value);

	action = egg_action_group_get_action (action_group, "EditEditAccountAction");
	g_value_set_static_string (&value, _("Edit"));
	g_object_set_property (G_OBJECT(action), "short_label", &value);

	action = egg_action_group_get_action (action_group, "EditAccountViewOptionsAction");
	g_value_set_static_string (&value, _("Options"));
	g_object_set_property (G_OBJECT(action), "short_label", &value);

	action = egg_action_group_get_action (action_group, "FileNewAccountAction");
	g_value_set_static_string (&value, _("New"));
	g_object_set_property (G_OBJECT(action), "short_label", &value);

	action = egg_action_group_get_action (action_group, "EditDeleteAccountAction");
	g_value_set_static_string (&value, _("Delete"));
	g_object_set_property (G_OBJECT(action), "short_label", &value);
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
	gint i;
	const gchar *url = NULL;
	int options_id;
	SCM find_options;
	SCM temp;
	URLType type;

	ENTER("page %p", plugin_page);
	priv = plugin_page->priv = g_new0 (GncPluginPageAccountTreePrivate, 1);

	/* Create menu and toolbar information */
	for (i = 0; i < gnc_plugin_page_account_tree_n_actions; i++) {
		gnc_plugin_page_account_tree_actions[i].user_data = plugin_page;
	}

	action_group = egg_action_group_new ("GncPluginPageAccountTreeActions");
	priv->action_group = action_group;
	egg_action_group_add_actions (action_group,
				      gnc_plugin_page_account_tree_actions,
				      gnc_plugin_page_account_tree_n_actions);
	gnc_plugin_page_account_tree_init_short_names (action_group);

	
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

	LEAVE("page %p, priv %p, action group %p",
	      plugin_page, plugin_page->priv, plugin_page->priv->action_group);
}

static void
gnc_plugin_page_account_tree_finalize (GObject *object)
{
	GncPluginPageAccountTree *model;
	GncPluginPageAccountTreePrivate *priv;
	SCM  free_tree;

	ENTER("object %p", object);
	model = GNC_PLUGIN_PAGE_ACCOUNT_TREE (object);
	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (model));
	priv = model->priv;
	g_return_if_fail (priv != NULL);

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

static Account *
gnc_plugin_page_account_tree_get_current_account (GncPluginPageAccountTree *page)
{
	GtkTreeSelection *selection;
	GtkTreeModel *model;
	GtkTreeIter iter;
	Account *account;

	ENTER("page %p (tree view %p)", page, page->priv->tree_view);
	selection = gtk_tree_view_get_selection (page->priv->tree_view);
	if (!gtk_tree_selection_get_selected (selection, &model, &iter))
		return NULL;

	account = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT (model), &iter);

	LEAVE("account %p", account);
	return account;
}


/* Virtual table */
static void
gnc_plugin_page_account_tree_plugin_page_init (GncPluginPageIface *iface)
{
	ENTER(" ");
	iface->create_widget   = gnc_plugin_page_account_tree_create_widget;
	iface->merge_actions   = gnc_plugin_page_account_tree_merge_actions;
	iface->unmerge_actions = gnc_plugin_page_account_tree_unmerge_actions;
	iface->get_title       = gnc_plugin_page_account_tree_get_title;
	iface->get_icon        = gnc_plugin_page_account_tree_get_icon;
	iface->get_plugin_name = gnc_plugin_page_account_tree_get_plugin_name;
	iface->get_uri         = gnc_plugin_page_account_tree_get_uri;
	LEAVE(" ");
}

static GtkWidget *
gnc_plugin_page_account_tree_create_widget (GncPluginPage *plugin_page)
{
	GncPluginPageAccountTree *page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page);
	GtkWidget *scrolled_window;

	ENTER("page %p", plugin_page);
	if (page->priv->widget == NULL) {
		page->priv->widget = gtk_vbox_new (FALSE, 0);
		gtk_widget_show (page->priv->widget);

		scrolled_window = gtk_scrolled_window_new (NULL, NULL);
		gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
						GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
		gtk_widget_show (scrolled_window);
		gtk_box_pack_start (GTK_BOX (page->priv->widget), scrolled_window,
				    TRUE, TRUE, 0);

		page->priv->tree_view = gnc_tree_view_account_new();
		g_signal_connect (G_OBJECT (page->priv->tree_view), "button-press-event",
				  G_CALLBACK (gnc_plugin_page_account_tree_button_press_cb), page);
		gtk_widget_show (GTK_WIDGET (page->priv->tree_view));
		gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET(page->priv->tree_view));
	}

	LEAVE("widget = %p", page->priv->widget);
	return page->priv->widget;
}

static void
gnc_plugin_page_account_tree_merge_actions (GncPluginPage *plugin_page,
					    EggMenuMerge *ui_merge)
{
	GncPluginPageAccountTree *plugin_page_account_tree = GNC_PLUGIN_PAGE_ACCOUNT_TREE(plugin_page);
	
	ENTER("page %p, ui_merge %p", plugin_page, ui_merge);
	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page_account_tree));

	egg_menu_merge_insert_action_group (ui_merge, plugin_page_account_tree->priv->action_group, 0);

	DEBUG("merge_id was %d", plugin_page_account_tree->priv->merge_id);
	plugin_page_account_tree->priv->merge_id =
	  egg_menu_merge_add_ui_from_file (ui_merge,
					   GNC_UI_DIR "/gnc-plugin-page-account-tree-ui.xml",
					   NULL);
	DEBUG("merge_id is %d", plugin_page_account_tree->priv->merge_id);

	egg_menu_merge_ensure_update (ui_merge);

	plugin_page_account_tree->priv->ui_merge = ui_merge;
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

static G_CONST_RETURN gchar *
gnc_plugin_page_account_tree_get_title (GncPluginPage *plugin_page)
{
	return _("Accounts");
}

static G_CONST_RETURN gchar *
gnc_plugin_page_account_tree_get_icon (GncPluginPage *plugin_page)
{
	return GNC_STOCK_ACCOUNT;
}

static G_CONST_RETURN gchar *
gnc_plugin_page_account_tree_get_plugin_name (GncPluginPage *plugin_page)
{
	return GNC_PLUGIN_ACCOUNT_TREE_NAME;
}

static G_CONST_RETURN gchar *
gnc_plugin_page_account_tree_get_uri (GncPluginPage *plugin_page)
{
	return "default:";
}

/* Callbacks */
static gboolean
gnc_plugin_page_account_tree_button_press_cb (GtkWidget *widget,
					      GdkEventButton *event,
	       				      GncPluginPageAccountTree *page)
{
	const gchar *popup;
	gchar *path;
	GtkWidget *menuitem, *menu;

	if (event->button == 3 && page->priv->ui_merge != NULL) {
		/* Maybe show a different popup menu if no account is selected. */
		popup = "AccountPopup";

		path = g_strconcat ("/popups/", popup, NULL);
		menuitem = egg_menu_merge_get_widget (page->priv->ui_merge, path);
		g_free (path);

		g_return_val_if_fail (menuitem != NULL, FALSE);
		menu = gtk_menu_item_get_submenu(GTK_MENU_ITEM(menuitem));
		g_return_val_if_fail (menu != NULL, FALSE);

		gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, event->button, event->time);

		return TRUE;
	}

	return FALSE;
}
	

/* Command callbacks */
static void
gnc_plugin_page_account_tree_cmd_new_account (EggAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);

	gnc_ui_new_account_window_with_default (NULL, account);
}

static void
gnc_plugin_page_account_tree_cmd_open_account (EggAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);
	GNCSplitReg *gsr;

	g_return_if_fail (account != NULL);

	gsr = regWindowSimple (account);
	gnc_split_reg_raise (gsr);
}

static void
gnc_plugin_page_account_tree_cmd_open_subaccounts (EggAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);
	GNCSplitReg *gsr;

	g_return_if_fail (account != NULL);

	gsr = regWindowAccGroup (account);
	gnc_split_reg_raise (gsr);
}

static void
gnc_plugin_page_account_tree_cmd_edit_account (EggAction *action, GncPluginPageAccountTree *page)
{
	Account *account;
	AccountWindow *account_window;

	ENTER("action %p, page %p (merge_id %d, action_group %p)",
	      action, page, page->priv->merge_id, page->priv->action_group);

	account = gnc_plugin_page_account_tree_get_current_account (page);
	g_return_if_fail (account != NULL);

	account_window = gnc_ui_edit_account_window (account);
	gnc_ui_edit_account_window_raise (account_window);
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
gnc_plugin_page_account_tree_cmd_reconcile (EggAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);
	RecnWindow *recnData;

	g_return_if_fail (account != NULL);

	recnData = recnWindow (gnc_ui_get_toplevel (), account);
	gnc_ui_reconcile_window_raise (recnData);
}

static void
gnc_plugin_page_account_tree_cmd_transfer (EggAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);

	gnc_xfer_dialog (gnc_ui_get_toplevel (), account);
}

static void
gnc_plugin_page_account_tree_cmd_stock_split (EggAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);

	gnc_stock_split_dialog (NULL, account);
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
