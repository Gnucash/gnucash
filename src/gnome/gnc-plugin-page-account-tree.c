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
#include "dialog-transfer.h"
#include "gnc-book.h"
#include "gnc-component-manager.h"
#include "gnc-icons.h"
#include "gnc-plugin-account-tree.h"
#include "gnc-split-reg.h"
#include "gnc-tree-model-account.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
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
gnc_plugin_page_account_tree_init (GncPluginPageAccountTree *plugin_page)
{
	EggActionGroup *action_group;
	gint i;

	ENTER("page %p", plugin_page);
	plugin_page->priv = g_new0 (GncPluginPageAccountTreePrivate, 1);

	/* Create menu and toolbar information */
	for (i = 0; i < gnc_plugin_page_account_tree_n_actions; i++) {
		gnc_plugin_page_account_tree_actions[i].user_data = plugin_page;
	}

	action_group = egg_action_group_new ("GncPluginPageAccountTreeActions");
	plugin_page->priv->action_group = action_group;
	egg_action_group_add_actions (action_group, gnc_plugin_page_account_tree_actions,
				      gnc_plugin_page_account_tree_n_actions);
	gnc_plugin_page_account_tree_init_short_names (action_group);

	LEAVE("page %p, priv %p, action group %p",
	      plugin_page, plugin_page->priv, plugin_page->priv->action_group);
}

static void
gnc_plugin_page_account_tree_finalize (GObject *object)
{
	GncPluginPageAccountTree *model = GNC_PLUGIN_PAGE_ACCOUNT_TREE (object);

	ENTER("object %p", object);
	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (model));
	g_return_if_fail (model->priv != NULL);

	g_free (model->priv);

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
	GtkTreeModel *model;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;

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

		model = gnc_tree_model_account_new (gnc_book_get_group (gnc_get_current_book ()));

		page->priv->tree_view = GTK_TREE_VIEW (gtk_tree_view_new ());
		gtk_widget_show (GTK_WIDGET (page->priv->tree_view));
		gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET(page->priv->tree_view));
		
		g_signal_connect (G_OBJECT (page->priv->tree_view), "button-press-event",
      				 G_CALLBACK (gnc_plugin_page_account_tree_button_press_cb), page);
		gtk_tree_view_set_model (page->priv->tree_view, model);
		g_object_unref (model);

		column = gtk_tree_view_column_new ();
		gtk_tree_view_column_set_title (column, _("Account Name"));
		renderer = gtk_cell_renderer_pixbuf_new ();
		g_object_set (renderer, "stock-id", GNC_STOCK_ACCOUNT, NULL);
		gtk_tree_view_column_pack_start (column, renderer, FALSE);
		renderer = gtk_cell_renderer_text_new ();
		gtk_tree_view_column_pack_start (column, renderer, FALSE);
		gtk_tree_view_column_add_attribute (column,
						    renderer,
						    "text", GNC_TREE_MODEL_ACCOUNT_COL_NAME);
		gtk_tree_view_append_column (page->priv->tree_view, column);
		gtk_tree_view_set_expander_column (page->priv->tree_view, column);

		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes (_("Description"),
								   renderer,
								   "text", GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION,
								   NULL);
		gtk_tree_view_append_column (page->priv->tree_view, column);
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

static void
gnc_plugin_page_account_tree_cmd_view_options (EggAction *action, GncPluginPageAccountTree *page)
{
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
