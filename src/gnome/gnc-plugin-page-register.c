/* 
 * gnc-plugin-page-register.c -- 
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include "config.h"

#include <gtk/gtkcellrenderertext.h>
#include <gtk/gtktreeview.h>
#include <gtk/gtkvbox.h>
#include "egg-action-group.h"

#include "gnc-plugin-page-register.h"
#include "gnc-plugin-register.h"
#include "gnc-split-reg.h"

#include "messages.h"

static void gnc_plugin_page_register_class_init (GncPluginPageRegisterClass *klass);
static void gnc_plugin_page_register_init (GncPluginPageRegister *plugin_page);
static void gnc_plugin_page_register_finalize (GObject *object);

/* atic Account *gnc_plugin_page_register_get_current_account (GncPluginPageRegister *page); */

static void gnc_plugin_page_register_plugin_page_init (GncPluginPageIface *iface);

static GtkWidget *gnc_plugin_page_register_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_register_merge_actions (GncPluginPage *plugin_page, EggMenuMerge *ui_merge);
static void gnc_plugin_page_register_unmerge_actions (GncPluginPage *plugin_page, EggMenuMerge *ui_merge);
static G_CONST_RETURN gchar *gnc_plugin_page_register_get_title (GncPluginPage *plugin_page);
static G_CONST_RETURN gchar *gnc_plugin_page_register_get_icon (GncPluginPage *plugin_page);
static G_CONST_RETURN gchar *gnc_plugin_page_register_get_plugin_name (GncPluginPage *plugin_page);
static G_CONST_RETURN gchar *gnc_plugin_page_register_get_uri (GncPluginPage *plugin_page);

/* Callbacks */
#if 0
static gboolean gnc_plugin_page_register_button_press_cb (GtkWidget *widget,
							      GdkEventButton *event,
			       				      GncPluginPageRegister *page);
#endif
/* Command callbacks */
static void gnc_plugin_page_register_cmd_record_transaction (EggAction *action, GncPluginPageRegister *plugin_page);
static void gnc_plugin_page_register_cmd_cancel_transaction (EggAction *action, GncPluginPageRegister *plugin_page);
static void gnc_plugin_page_register_cmd_delete_transaction (EggAction *action, GncPluginPageRegister *plugin_page);
static void gnc_plugin_page_register_cmd_duplicate_transaction (EggAction *action, GncPluginPageRegister *plugin_page);

static EggActionGroupEntry gnc_plugin_page_register_actions [] = {
	/* Toplevel */
	{ "FakeToplevel", "", NULL, NULL, NULL, NULL, NULL },

	/* Actions menu */
	{ "RecordTransactionAction", N_("_Enter Transaction"), GTK_STOCK_OK, NULL,
	  N_("Record the current transaction"),
	  G_CALLBACK (gnc_plugin_page_register_cmd_record_transaction), NULL },
	{ "CancelTransactionAction", N_("_Cancel Transaction"), GTK_STOCK_CANCEL, NULL,
	  N_("_Cancel the current transaction"),
	  G_CALLBACK (gnc_plugin_page_register_cmd_cancel_transaction), NULL },
	{ "DeleteTransactionAction", N_("_Delete Transaction"), GTK_STOCK_DELETE, NULL,
	  N_("Delete the current transaction"),
	  G_CALLBACK (gnc_plugin_page_register_cmd_delete_transaction), NULL },
	{ "DuplicateTransactionAction", N_("D_uplicate Transaction"), GTK_STOCK_COPY, NULL,
	  N_("Make a copy of the current transaction"),
	  G_CALLBACK (gnc_plugin_page_register_cmd_duplicate_transaction), NULL },
};
static guint gnc_plugin_page_register_n_actions = G_N_ELEMENTS (gnc_plugin_page_register_actions);

struct GncPluginPageRegisterPrivate
{
	EggActionGroup *action_group;
	guint merge_id;
	EggMenuMerge *ui_merge;

	GNCLedgerDisplay *ld;

	GtkWidget *widget;
};

static GObjectClass *parent_class = NULL;

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
		
		static const GInterfaceInfo plugin_page_info = {
			(GInterfaceInitFunc) gnc_plugin_page_register_plugin_page_init,
			NULL,
			NULL
		};

		gnc_plugin_page_register_type = g_type_register_static (G_TYPE_OBJECT,
								            "GncPluginPageRegister",
								            &our_info, 0);

		g_type_add_interface_static (gnc_plugin_page_register_type,
					     GNC_TYPE_PLUGIN_PAGE,
					     &plugin_page_info);
	}

	return gnc_plugin_page_register_type;
}

GncPluginPage *
gnc_plugin_page_register_new (GNCLedgerDisplay *ld)
{
	GncPluginPageRegister *plugin_page;

	plugin_page = g_object_new (GNC_TYPE_PLUGIN_PAGE_REGISTER,
			      NULL);
	plugin_page->priv->ld = ld;
	
	return GNC_PLUGIN_PAGE (plugin_page);
}

static void
gnc_plugin_page_register_class_init (GncPluginPageRegisterClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_plugin_page_register_finalize;
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
	EggActionGroup *action_group;
	gint i;

	plugin_page->priv = g_new0 (GncPluginPageRegisterPrivate, 1);

	/* Create menu and toolbar information */
	for (i = 0; i < gnc_plugin_page_register_n_actions; i++) {
		gnc_plugin_page_register_actions[i].user_data = plugin_page;
	}

	action_group = egg_action_group_new ("GncPluginPageRegisterActions");
	plugin_page->priv->action_group = action_group;
	egg_action_group_add_actions (action_group, gnc_plugin_page_register_actions,
				      gnc_plugin_page_register_n_actions);
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

/* Virtual table */
static void
gnc_plugin_page_register_plugin_page_init (GncPluginPageIface *iface)
{
	iface->create_widget   = gnc_plugin_page_register_create_widget;
	iface->merge_actions   = gnc_plugin_page_register_merge_actions;
	iface->unmerge_actions = gnc_plugin_page_register_unmerge_actions;
	iface->get_title       = gnc_plugin_page_register_get_title;
	iface->get_icon        = gnc_plugin_page_register_get_icon;
	iface->get_plugin_name = gnc_plugin_page_register_get_plugin_name;
	iface->get_uri         = gnc_plugin_page_register_get_uri;
}

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
gnc_plugin_page_register_merge_actions (GncPluginPage *plugin_page,
					    EggMenuMerge *ui_merge)
{
	GncPluginPageRegister *plugin_page_register = GNC_PLUGIN_PAGE_REGISTER(plugin_page);
	
	g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (plugin_page_register));

	egg_menu_merge_insert_action_group (ui_merge, plugin_page_register->priv->action_group, 0);

	plugin_page_register->priv->merge_id = egg_menu_merge_add_ui_from_file (ui_merge,
									       GNC_UI_DIR "/gnc-plugin-page-register-ui.xml",
									       NULL);
	egg_menu_merge_ensure_update (ui_merge);

	plugin_page_register->priv->ui_merge = ui_merge;
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

static G_CONST_RETURN gchar *
gnc_plugin_page_register_get_title (GncPluginPage *plugin_page)
{
	return _("General Ledger");
}

static G_CONST_RETURN gchar *
gnc_plugin_page_register_get_icon (GncPluginPage *plugin_page)
{
	return NULL;
}

static G_CONST_RETURN gchar *
gnc_plugin_page_register_get_plugin_name (GncPluginPage *plugin_page)
{
	return GNC_PLUGIN_REGISTER_NAME;
}

static G_CONST_RETURN gchar *
gnc_plugin_page_register_get_uri (GncPluginPage *plugin_page)
{
	return "default:";
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

/* Command callbacks */
static void
gnc_plugin_page_register_cmd_record_transaction (EggAction *action, GncPluginPageRegister *plugin_page)
{
}

static void
gnc_plugin_page_register_cmd_cancel_transaction (EggAction *action, GncPluginPageRegister *plugin_page)
{
}

static void
gnc_plugin_page_register_cmd_delete_transaction (EggAction *action, GncPluginPageRegister *plugin_page)
{
}

static void
gnc_plugin_page_register_cmd_duplicate_transaction (EggAction *action, GncPluginPageRegister *plugin_page)
{
}

