/* 
 * gnc-plugin-page-register.c -- 
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

#include <gtk/gtkcellrenderertext.h>
#include <gtk/gtktreeview.h>
#include <gtk/gtkvbox.h>
#include "egg-action-group.h"
#include "egg-radio-action.h"
#include "global-options.h"
#include "gnc-icons.h"
#include "gnc-plugin-page-register.h"
#include "gnc-plugin-register.h"
#include "gnc-split-reg.h"

#include "messages.h"

static void gnc_plugin_page_register_class_init (GncPluginPageRegisterClass *klass);
static void gnc_plugin_page_register_init (GncPluginPageRegister *plugin_page);
static void gnc_plugin_page_register_finalize (GObject *object);

/* static Account *gnc_plugin_page_register_get_current_account (GncPluginPageRegister *page); */

static GtkWidget *gnc_plugin_page_register_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_register_destroy_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_register_merge_actions (GncPluginPage *plugin_page, EggMenuMerge *ui_merge);
static void gnc_plugin_page_register_unmerge_actions (GncPluginPage *plugin_page, EggMenuMerge *ui_merge);

static gchar *gnc_plugin_page_register_get_tab_name (GncPluginPage *plugin_page);

/* Callbacks */
#if 0
static gboolean gnc_plugin_page_register_button_press_cb (GtkWidget *widget,
							      GdkEventButton *event,
			       				      GncPluginPageRegister *page);
#endif
/* Command callbacks */
static void gnc_plugin_page_register_cmd_enter_transaction (EggAction *action, GncPluginPageRegister *plugin_page);
static void gnc_plugin_page_register_cmd_cancel_transaction (EggAction *action, GncPluginPageRegister *plugin_page);
static void gnc_plugin_page_register_cmd_delete_transaction (EggAction *action, GncPluginPageRegister *plugin_page);
static void gnc_plugin_page_register_cmd_duplicate_transaction (EggAction *action, GncPluginPageRegister *plugin_page);


/************************************************************/
/*                          Actions                         */
/************************************************************/

static EggActionEntry gnc_plugin_page_register_actions [] =
{
	/* Toplevel */
	{ "FakeToplevel", "", NULL, NULL, NULL, NULL },

	/* Actions menu */
	{ "RecordTransactionAction", N_("_Enter Transaction"), GTK_STOCK_ADD, NULL,
	  N_("Record the current transaction"),
	  G_CALLBACK (gnc_plugin_page_register_cmd_enter_transaction) },
	{ "CancelTransactionAction", N_("_Cancel Transaction"), GTK_STOCK_CANCEL, NULL,
	  N_("_Cancel the current transaction"),
	  G_CALLBACK (gnc_plugin_page_register_cmd_cancel_transaction) },
	{ "DeleteTransactionAction", N_("_Delete Transaction"), GTK_STOCK_DELETE, NULL,
	  N_("Delete the current transaction"),
	  G_CALLBACK (gnc_plugin_page_register_cmd_delete_transaction) },

	{ "DuplicateTransactionAction", N_("D_uplicate Transaction"), GTK_STOCK_COPY, NULL,
	  N_("Make a copy of the current transaction"),
	  G_CALLBACK (gnc_plugin_page_register_cmd_duplicate_transaction) },
};
static guint gnc_plugin_page_register_n_actions = G_N_ELEMENTS (gnc_plugin_page_register_actions);

/************************************************************/
/*                      Data Structures                     */
/************************************************************/

struct GncPluginPageRegisterPrivate
{
	EggActionGroup *action_group;
	guint merge_id;
	EggMenuMerge *ui_merge;

	GNCLedgerDisplay *ld;

	GtkWidget *widget;
};

static GObjectClass *parent_class = NULL;

/************************************************************/
/*                      Implementation                      */
/************************************************************/

GType
gnc_plugin_page_register_get_type (void)
{
	static GType gnc_plugin_page_register_type = 0;

	if (gnc_plugin_page_register_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncPluginPageRegisterClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_plugin_page_register_class_init,
			NULL,
			NULL,
			sizeof (GncPluginPageRegister),
			0,
			(GInstanceInitFunc) gnc_plugin_page_register_init
		};
		
		gnc_plugin_page_register_type = g_type_register_static (GNC_TYPE_PLUGIN_PAGE,
									"GncPluginPageRegister",
									&our_info, 0);
	}

	return gnc_plugin_page_register_type;
}

static GncPluginPage *
gnc_plugin_page_register_new_common (GNCLedgerDisplay *ld)
{
	GncPluginPageRegister *register_page;
	GncPluginPage *plugin_page;

	register_page = g_object_new (GNC_TYPE_PLUGIN_PAGE_REGISTER, NULL);
	register_page->priv->ld = ld;

	plugin_page = GNC_PLUGIN_PAGE(register_page);
	plugin_page->title = gnc_plugin_page_register_get_tab_name(plugin_page);
	plugin_page->tab_name = gnc_plugin_page_register_get_tab_name(plugin_page);

	return plugin_page;
}

GncPluginPage *
gnc_plugin_page_register_new (Account *account, gboolean subaccounts)
{
	GNCLedgerDisplay *ld;

	if (subaccounts)
	  ld = gnc_ledger_display_subaccounts (account);
	else
	  ld = gnc_ledger_display_simple (account);

	return gnc_plugin_page_register_new_common(ld);
}

GncPluginPage *
gnc_plugin_page_register_new_gl (void)
{
	GNCLedgerDisplay *ld;

	ld = gnc_ledger_display_gl ();
	return gnc_plugin_page_register_new_common(ld);
}

GncPluginPage *
gnc_plugin_page_register_new_ledger (GNCLedgerDisplay *ledger)
{
	return gnc_plugin_page_register_new_common(ledger);
}

static void
gnc_plugin_page_register_class_init (GncPluginPageRegisterClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GncPluginPageClass *gnc_plugin_class = GNC_PLUGIN_PAGE_CLASS(klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_plugin_page_register_finalize;

	gnc_plugin_class->tab_icon        = GNC_STOCK_ACCOUNT;
	gnc_plugin_class->plugin_name     = GNC_PLUGIN_REGISTER_NAME;
	gnc_plugin_class->create_widget   = gnc_plugin_page_register_create_widget;
	gnc_plugin_class->destroy_widget  = gnc_plugin_page_register_destroy_widget;
	gnc_plugin_class->merge_actions   = gnc_plugin_page_register_merge_actions;
	gnc_plugin_class->unmerge_actions = gnc_plugin_page_register_unmerge_actions;
}

/* DRH - Suggest this be added to libegg */
static void
gnc_plugin_page_register_init_short_names (EggActionGroup *action_group)
{
	EggAction *action;
	GValue value = { 0, };

	g_value_init (&value, G_TYPE_STRING);

	/* Add a couple of short labels for the toolbar */
	action = egg_action_group_get_action (action_group, "RecordTransactionAction");
	g_value_set_static_string (&value, _("Record"));
	g_object_set_property (G_OBJECT(action), "short_label", &value);

	action = egg_action_group_get_action (action_group, "CancelTransactionAction");
	g_value_set_static_string (&value, _("Cancel"));
	g_object_set_property (G_OBJECT(action), "short_label", &value);

	action = egg_action_group_get_action (action_group, "DeleteTransactionAction");
	g_value_set_static_string (&value, _("Delete"));
	g_object_set_property (G_OBJECT(action), "short_label", &value);

	action = egg_action_group_get_action (action_group, "DuplicateTransactionAction");
	g_value_set_static_string (&value, _("Duplicate"));
	g_object_set_property (G_OBJECT(action), "short_label", &value);
}

static void
gnc_plugin_page_register_init (GncPluginPageRegister *plugin_page)
{
	GncPluginPageRegisterPrivate *priv;
	GncPluginPage *parent;
	EggActionGroup *action_group;

	priv = g_new0 (GncPluginPageRegisterPrivate, 1);
	plugin_page->priv = priv;

	/* Init parent declared variables */
	parent = GNC_PLUGIN_PAGE(plugin_page);
	parent->title       = g_strdup(_("General Ledger"));
	parent->tab_name    = g_strdup(_("General Ledger"));
	parent->uri         = g_strdup("default:");

	/* Create menu and toolbar information */
	action_group = egg_action_group_new ("GncPluginPageRegisterActions");
	priv->action_group = action_group;
	egg_action_group_add_actions (action_group, gnc_plugin_page_register_actions,
				      gnc_plugin_page_register_n_actions, plugin_page);
	gnc_plugin_page_register_init_short_names (action_group);
}

static void
gnc_plugin_page_register_finalize (GObject *object)
{
	GncPluginPageRegister *model = GNC_PLUGIN_PAGE_REGISTER (object);

	g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (model));
	g_return_if_fail (model->priv != NULL);

	g_free (model->priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

/*
static Account *
gnc_plugin_page_register_get_current_account (GncPluginPageRegister *page)
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
*/

/* Virtual Functions */

static GtkWidget *
gnc_plugin_page_register_create_widget (GncPluginPage *plugin_page)
{
	GncPluginPageRegister *page = GNC_PLUGIN_PAGE_REGISTER (plugin_page);
	GncPluginPageRegisterPrivate *priv = page->priv;
	guint numRows;
	GtkWidget *gsr;
	SplitRegister *sr;

	if (priv->widget != NULL)
		return priv->widget;

	priv->widget = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (priv->widget);
	
	numRows = (guint) gnc_lookup_number_option ("_+Advanced",
						    "Number of Rows", 20.0);

	gsr = gnc_split_reg_new(priv->ld, NULL, numRows, 0, 0);
	gtk_widget_show (gsr);
	gtk_container_add (GTK_CONTAINER (priv->widget), gsr);

	sr = gnc_ledger_display_get_split_register( priv->ld );
	gnc_split_register_config( sr, sr->type, sr->style, sr->use_double_line );
	gnc_ledger_display_refresh( priv->ld );

	/* DRH - Probably lots of other stuff from regWindowLedger should end up here. */
	return priv->widget;
}

static void
gnc_plugin_page_register_destroy_widget (GncPluginPage *plugin_page)
{
	GncPluginPageRegister *page = GNC_PLUGIN_PAGE_REGISTER (plugin_page);
	GncPluginPageRegisterPrivate *priv = page->priv;

	if (priv->widget == NULL)
		return;

	gtk_widget_hide(priv->widget);
	gnc_ledger_display_close (priv->ld);
	priv->ld = NULL;
}

static void
gnc_plugin_page_register_merge_actions (GncPluginPage *plugin_page,
					EggMenuMerge *ui_merge)
{
	GncPluginPageRegister *register_page;
	GncPluginPageRegisterPrivate *priv;
	GError *error = NULL;
	
	g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (plugin_page));

	register_page = GNC_PLUGIN_PAGE_REGISTER(plugin_page);
	priv = register_page->priv;
	egg_menu_merge_insert_action_group (ui_merge, priv->action_group, 0);

	priv->merge_id =
	  egg_menu_merge_add_ui_from_file (ui_merge,
					   GNC_UI_DIR "/gnc-plugin-page-register-ui.xml",
					   &error);

	g_assert(priv->merge_id || error);
	if (priv->merge_id) {
	  egg_menu_merge_ensure_update (ui_merge);
	  priv->ui_merge = ui_merge;
	} else {
	  g_critical("Failed to load ui file.\n  Filename %s\n  Error %s",
		     "gnc-plugin-page-register-ui.xml", error->message);
	  g_error_free(error);
	}
}
	
static void
gnc_plugin_page_register_unmerge_actions (GncPluginPage *plugin_page,
					      EggMenuMerge *ui_merge)
{
	GncPluginPageRegister *plugin_page_register = GNC_PLUGIN_PAGE_REGISTER(plugin_page);
	
	g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (plugin_page_register));
	g_return_if_fail (plugin_page_register->priv->merge_id != 0);
	g_return_if_fail (plugin_page_register->priv->action_group != NULL);

	egg_menu_merge_remove_ui (ui_merge, plugin_page_register->priv->merge_id);
	egg_menu_merge_remove_action_group (ui_merge, plugin_page_register->priv->action_group);

	plugin_page_register->priv->ui_merge = NULL;
}

static gchar *
gnc_plugin_page_register_get_tab_name (GncPluginPage *plugin_page)
{
	GNCLedgerDisplayType ledger_type;
  	GNCLedgerDisplay *ld;
	SplitRegister *reg;
	Account *leader;

	g_return_val_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (plugin_page), _("unknown"));

	ld = GNC_PLUGIN_PAGE_REGISTER (plugin_page)->priv->ld;
	reg = gnc_ledger_display_get_split_register (ld);
	ledger_type = gnc_ledger_display_type (ld);
	leader = gnc_ledger_display_leader (ld);

	switch (ledger_type) {
	 case LD_SINGLE:
	  return g_strdup(xaccAccountGetName (leader));

	 case LD_SUBACCOUNT:
	  return g_strdup_printf("%s+", xaccAccountGetName (leader));

	 case LD_GL:
	  switch (reg->type) {
	   case GENERAL_LEDGER:
	   case INCOME_LEDGER:
	    return g_strdup(_("General Ledger"));
	   case PORTFOLIO_LEDGER:
	    return g_strdup(_("Portfolio"));
	   case SEARCH_LEDGER:
	    return g_strdup(_("Search Results"));
	   default:
	    break;
	  }

	 default:
	  break;
	}

	return g_strdup(_("unknown"));
}

/* Callbacks */
#if 0
static gboolean
gnc_plugin_page_register_button_press_cb (GtkWidget *widget,
					  GdkEventButton *event,
					  GncPluginPageRegister *page)
{
	const gchar *popup;
	gchar *path;
	GtkWidget *menu;

	if (event->button == 3 && page->priv->ui_merge != NULL) {
		/* Maybe show a different popup menu if no account is selected. */
		popup = "RegisterPopup";

		path = g_strconcat ("/popups/", popup, NULL);
		menu = egg_menu_merge_get_widget (page->priv->ui_merge, path);
		g_free (path);

		g_return_val_if_fail (menu != NULL, FALSE);

		gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, event->button, event->time);

		return TRUE;
	}

	return FALSE;
}
#endif

/************************************************************/
/*                     Command callbacks                    */
/************************************************************/

static void
gnc_plugin_page_register_cmd_enter_transaction (EggAction *action,
						GncPluginPageRegister *plugin_page)
{
}

static void
gnc_plugin_page_register_cmd_cancel_transaction (EggAction *action,
						 GncPluginPageRegister *plugin_page)
{
}

static void
gnc_plugin_page_register_cmd_delete_transaction (EggAction *action,
						 GncPluginPageRegister *plugin_page)
{
}

static void
gnc_plugin_page_register_cmd_duplicate_transaction (EggAction *action,
						    GncPluginPageRegister *plugin_page)
{
}
