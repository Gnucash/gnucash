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
#include "dialog-account.h"
#include "gnc-book.h"
#include "gnc-plugin-account-tree.h"
#include "gnc-tree-model-account.h"
#include "gnc-ui-util.h"

#include "messages.h"

static void gnc_plugin_page_account_tree_class_init (GncPluginPageAccountTreeClass *klass);
static void gnc_plugin_page_account_tree_init (GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_finalize (GObject *object);

static Account *gnc_plugin_page_account_tree_get_current_account (GncPluginPageAccountTree *page);

static void gnc_plugin_page_account_tree_plugin_page_init (GncPluginPageIface *iface);

static GtkWidget *gnc_plugin_page_account_tree_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_account_tree_merge_actions (GncPluginPage *plugin_page, EggMenuMerge *ui_merge);
static void gnc_plugin_page_account_tree_unmerge_actions (GncPluginPage *plugin_page, EggMenuMerge *ui_merge);
static gchar *gnc_plugin_page_account_tree_get_title (GncPluginPage *plugin_page);
static GdkPixbuf *gnc_plugin_page_account_tree_get_icon (GncPluginPage *plugin_page);
static const gchar *gnc_plugin_page_account_tree_get_plugin_name (GncPluginPage *plugin_page);
static gchar *gnc_plugin_page_account_tree_get_uri (GncPluginPage *plugin_page);

/* Callbacks */
static gboolean gnc_plugin_page_account_tree_button_press_cb (GtkWidget *widget,
							      GdkEventButton *event,
			       				      GncPluginPageAccountTree *page);

/* Command callbacks */
static void gnc_plugin_page_account_tree_cmd_new_account (EggAction *action, GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_cmd_open_account (EggAction *action, gpointer data);
static void gnc_plugin_page_account_tree_cmd_open_subaccounts (EggAction *action, gpointer data);
static void gnc_plugin_page_account_tree_cmd_edit_account (EggAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_delete_account (EggAction *action, gpointer data);
static void gnc_plugin_page_account_tree_cmd_view_options (EggAction *action, gpointer data);
static void gnc_plugin_page_account_tree_cmd_reconcile (EggAction *action, gpointer data);
static void gnc_plugin_page_account_tree_cmd_transfer (EggAction *action, gpointer data);
static void gnc_plugin_page_account_tree_cmd_stock_split (EggAction *action, gpointer data);

static EggActionGroupEntry gnc_plugin_page_account_tree_actions [] = {
	/* Toplevel */
	{ "FakeToplevel", (""), NULL, NULL, NULL, NULL, NULL },

	/* File menu */
	{ "FileNewAccountAction", N_("New Account..."), GTK_STOCK_ADD, NULL,
	  N_("Create a new Account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_new_account), NULL },
	{ "FileOpenAccountAction", N_("Open Account"), GTK_STOCK_OPEN, "<control>o",
	  N_("Open the selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_open_account), NULL },
	{ "FileOpenSubaccountsAction", N_("Open _Subaccounts"), GTK_STOCK_OPEN, NULL,
	  N_("Open the selected account and all its subaccounts"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_open_subaccounts), NULL },

	/* Edit menu */
	{ "EditEditAccountAction", N_("_Edit Acount"), GTK_STOCK_PROPERTIES, "<control>e",
	  N_("Edit the selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_edit_account), NULL },
	{ "EditDeleteAccountAction", N_("_Delete Acount"), GTK_STOCK_REMOVE, NULL,
	  N_("Delete selected account"),
	  G_CALLBACK (gnc_plugin_page_account_tree_cmd_delete_account), NULL },
	{ "EditAccountViewOptionsAction", N_("Options"), GTK_STOCK_PROPERTIES, NULL,
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

	plugin_page = g_object_new (GNC_TYPE_PLUGIN_PAGE_ACCOUNT_TREE,
			      NULL);

	return GNC_PLUGIN_PAGE (plugin_page);
}

static void
gnc_plugin_page_account_tree_class_init (GncPluginPageAccountTreeClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_plugin_page_account_tree_finalize;
}

static void
gnc_plugin_page_account_tree_init (GncPluginPageAccountTree *plugin_page)
{
	gint i;

	plugin_page->priv = g_new0 (GncPluginPageAccountTreePrivate, 1);

	/* Create menu and toolbar information */
	for (i = 0; i < gnc_plugin_page_account_tree_n_actions; i++) {
		gnc_plugin_page_account_tree_actions[i].user_data = plugin_page;
	}

	plugin_page->priv->action_group = egg_action_group_new ("GncPluginPageAccountTreeActions");
	egg_action_group_add_actions (plugin_page->priv->action_group, gnc_plugin_page_account_tree_actions,
				      gnc_plugin_page_account_tree_n_actions);
}

static void
gnc_plugin_page_account_tree_finalize (GObject *object)
{
	GncPluginPageAccountTree *model = GNC_PLUGIN_PAGE_ACCOUNT_TREE (object);

	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (model));
	g_return_if_fail (model->priv != NULL);

	g_free (model->priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

static Account *
gnc_plugin_page_account_tree_get_current_account (GncPluginPageAccountTree *page)
{
	GtkTreeSelection *selection;
	GtkTreeModel *model;
	GtkTreeIter iter;
	Account *account;

	selection = gtk_tree_view_get_selection (page->priv->tree_view);
	if (!gtk_tree_selection_get_selected (selection, &model, &iter))
		return NULL;

	account = gnc_tree_model_account_get_account (GNC_TREE_MODEL_ACCOUNT (model), &iter);

	return account;
}


/* Virtual table */
static void
gnc_plugin_page_account_tree_plugin_page_init (GncPluginPageIface *iface)
{
	iface->create_widget   = gnc_plugin_page_account_tree_create_widget;
	iface->merge_actions   = gnc_plugin_page_account_tree_merge_actions;
	iface->unmerge_actions = gnc_plugin_page_account_tree_unmerge_actions;
	iface->get_title       = gnc_plugin_page_account_tree_get_title;
	iface->get_icon        = gnc_plugin_page_account_tree_get_icon;
	iface->get_plugin_name = gnc_plugin_page_account_tree_get_plugin_name;
	iface->get_uri         = gnc_plugin_page_account_tree_get_uri;
}

static GtkWidget *
gnc_plugin_page_account_tree_create_widget (GncPluginPage *plugin_page)
{
	GncPluginPageAccountTree *page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page);
	GtkWidget *scrolled_window;
	GNCTreeModelAccount *model;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;

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
		
		gtk_tree_view_set_rules_hint (page->priv->tree_view, TRUE);
		g_signal_connect (G_OBJECT (page->priv->tree_view), "button-press-event",
      				 G_CALLBACK (gnc_plugin_page_account_tree_button_press_cb), page);
		gtk_tree_view_set_model (page->priv->tree_view, GTK_TREE_MODEL (model));
		g_object_unref (G_OBJECT (model));

		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes (_("Account Name"),
								   renderer,
								   "text", GNC_TREE_MODEL_ACCOUNT_COL_NAME,
								   NULL);
		gtk_tree_view_append_column (page->priv->tree_view, column);
		gtk_tree_view_set_expander_column (page->priv->tree_view, column);

		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes (_("Description"),
								   renderer,
								   "text", GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION,
								   NULL);
		gtk_tree_view_append_column (page->priv->tree_view, column);
	}

	return page->priv->widget;
}

static void
gnc_plugin_page_account_tree_merge_actions (GncPluginPage *plugin_page,
					    EggMenuMerge *ui_merge)
{
	GncPluginPageAccountTree *plugin_page_account_tree = GNC_PLUGIN_PAGE_ACCOUNT_TREE(plugin_page);
	
	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page_account_tree));

	egg_menu_merge_insert_action_group (ui_merge, plugin_page_account_tree->priv->action_group, 0);

	plugin_page_account_tree->priv->merge_id = egg_menu_merge_add_ui_from_file (ui_merge,
									       GNC_UI_DIR "/gnc-plugin-page-account-tree-ui.xml",
									       NULL);
	egg_menu_merge_ensure_update (ui_merge);

	plugin_page_account_tree->priv->ui_merge = ui_merge;
}
	
static void
gnc_plugin_page_account_tree_unmerge_actions (GncPluginPage *plugin_page,
					      EggMenuMerge *ui_merge)
{
	GncPluginPageAccountTree *plugin_page_account_tree = GNC_PLUGIN_PAGE_ACCOUNT_TREE(plugin_page);
	
	g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page_account_tree));
	g_return_if_fail (plugin_page_account_tree->priv->merge_id != 0);
	g_return_if_fail (plugin_page_account_tree->priv->action_group != NULL);

	egg_menu_merge_remove_action_group (ui_merge, plugin_page_account_tree->priv->action_group);
	egg_menu_merge_remove_ui (ui_merge, plugin_page_account_tree->priv->merge_id);

	plugin_page_account_tree->priv->ui_merge = NULL;
}

static gchar *
gnc_plugin_page_account_tree_get_title (GncPluginPage *plugin_page)
{
	return _("Accounts");
}

static GdkPixbuf *
gnc_plugin_page_account_tree_get_icon (GncPluginPage *plugin_page)
{
	return NULL;
}

static const gchar *
gnc_plugin_page_account_tree_get_plugin_name (GncPluginPage *plugin_page)
{
	return GNC_PLUGIN_ACCOUNT_TREE_NAME;
}

static gchar *
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
	GtkWidget *menu;

	if (event->button == 3 && page->priv->ui_merge != NULL) {
		/* Maybe show a different popup menu if no account is selected. */
		popup = "AccountPopup";

		path = g_strconcat ("/popups/", popup, NULL);
		menu = egg_menu_merge_get_widget (page->priv->ui_merge, path);
		g_free (path);

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
gnc_plugin_page_account_tree_cmd_open_account (EggAction *action, gpointer data)
{
}

static void
gnc_plugin_page_account_tree_cmd_open_subaccounts (EggAction *action, gpointer data)
{
}

static void
gnc_plugin_page_account_tree_cmd_edit_account (EggAction *action, GncPluginPageAccountTree *page)
{
	Account *account = gnc_plugin_page_account_tree_get_current_account (page);
	AccountWindow *account_window;

	g_return_if_fail (account != NULL);

	account_window = gnc_ui_edit_account_window (account);
	gnc_ui_edit_account_window_raise (account_window);
}

static void
gnc_plugin_page_account_tree_cmd_delete_account (EggAction *action, gpointer data)
{
}

static void
gnc_plugin_page_account_tree_cmd_view_options (EggAction *action, gpointer data)
{
}

static void
gnc_plugin_page_account_tree_cmd_reconcile (EggAction *action, gpointer data)
{
}

static void
gnc_plugin_page_account_tree_cmd_transfer (EggAction *action, gpointer data)
{
}

static void
gnc_plugin_page_account_tree_cmd_stock_split (EggAction *action, gpointer data)
{
}

