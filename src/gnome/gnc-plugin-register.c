/* 
 * gnc-plugin-register.c -- 
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include "config.h"

#include <string.h>

#include "gnc-plugin-register.h"

#include "gnc-plugin-page-register.h"

#include "messages.h"

static void gnc_plugin_register_class_init (GncPluginRegisterClass *klass);
static void gnc_plugin_register_init (GncPluginRegister *plugin);
static void gnc_plugin_register_finalize (GObject *object);

static void gnc_plugin_register_plugin_init (GncPluginIface *iface);

static void gnc_plugin_register_add_to_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);
static void gnc_plugin_register_remove_from_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);
static const gchar *gnc_plugin_register_get_name (GncPlugin *plugin);
static GncPluginPage *gnc_plugin_register_create_page (GncPlugin *plugin, const gchar *uri);

/* Command callbacks */
static void gnc_plugin_register_cmd_general_ledger (EggAction *action, GncMainWindowActionData *data);

static EggActionGroupEntry gnc_plugin_register_actions [] = {
	{ "ToolsGeneralLedgerAction", N_("_General Ledger"), NULL, NULL,
	  N_("Open a general ledger window"),
	  G_CALLBACK (gnc_plugin_register_cmd_general_ledger), NULL },
};
static guint gnc_plugin_register_n_actions = G_N_ELEMENTS (gnc_plugin_register_actions);

struct GncPluginRegisterPrivate
{
};

static GObjectClass *parent_class = NULL;

GType
gnc_plugin_register_get_type (void)
{
	static GType gnc_plugin_register_type = 0;

	if (gnc_plugin_register_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncPluginRegisterClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_plugin_register_class_init,
			NULL,
			NULL,
			sizeof (GncPluginRegister),
			0,
			(GInstanceInitFunc) gnc_plugin_register_init
		};
		
		static const GInterfaceInfo plugin_info = {
			(GInterfaceInitFunc) gnc_plugin_register_plugin_init,
			NULL,
			NULL
		};

		gnc_plugin_register_type = g_type_register_static (G_TYPE_OBJECT,
								   "GncPluginRegister",
								   &our_info, 0);

		g_type_add_interface_static (gnc_plugin_register_type,
					     GNC_TYPE_PLUGIN,
					     &plugin_info);
	}

	return gnc_plugin_register_type;
}

GncPlugin *
gnc_plugin_register_new (void)
{
	GncPluginRegister *plugin;

	plugin = g_object_new (GNC_TYPE_PLUGIN_REGISTER,
			      NULL);

	return GNC_PLUGIN (plugin);
}

static void
gnc_plugin_register_class_init (GncPluginRegisterClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_plugin_register_finalize;
}

static void
gnc_plugin_register_init (GncPluginRegister *plugin)
{
	plugin->priv = g_new0 (GncPluginRegisterPrivate, 1);
}

static void
gnc_plugin_register_finalize (GObject *object)
{
	GncPluginRegister *model = GNC_PLUGIN_REGISTER (object);

	g_return_if_fail (GNC_IS_PLUGIN_REGISTER (model));
	g_return_if_fail (model->priv != NULL);

	g_free (model->priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
gnc_plugin_register_plugin_init (GncPluginIface *iface)
{
	iface->add_to_window      = gnc_plugin_register_add_to_window;
	iface->remove_from_window = gnc_plugin_register_remove_from_window;
	iface->get_name           = gnc_plugin_register_get_name;
	iface->create_page        = gnc_plugin_register_create_page;
}

static void
gnc_plugin_register_add_to_window (GncPlugin *plugin,
				       GncMainWindow *window,
				       GQuark type)
{
	g_return_if_fail (GNC_IS_PLUGIN_REGISTER (plugin));
	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));

	gnc_main_window_merge_actions (window, "gnc-plugin-register-default-actions",
			               gnc_plugin_register_actions, gnc_plugin_register_n_actions,
				       GNC_UI_DIR "/gnc-plugin-register-ui.xml", plugin);
}
	
static void
gnc_plugin_register_remove_from_window (GncPlugin *plugin,
					    GncMainWindow *window,
					    GQuark type)
{
	g_return_if_fail (GNC_IS_PLUGIN_REGISTER (plugin));
	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));

	gnc_main_window_unmerge_actions (window, "gnc-plugin-register-default-actions");
}

static const gchar *
gnc_plugin_register_get_name (GncPlugin *plugin)
{
	return GNC_PLUGIN_REGISTER_NAME;
}

static GncPluginPage *
gnc_plugin_register_create_page (GncPlugin *plugin,
				     const gchar *uri)
{
	g_return_val_if_fail (GNC_IS_PLUGIN_REGISTER (plugin), NULL);
	g_return_val_if_fail (uri != NULL, NULL);

	/* FIXME add better URI handling */
	if (strcmp ("default:", uri)) {
		return NULL;
	}
	
	return NULL;
}


/* Command callbacks */
static void
gnc_plugin_register_cmd_general_ledger (EggAction *action, GncMainWindowActionData *data)
{
	GNCLedgerDisplay *ld;

	GncPluginPage *page;

	g_return_if_fail (data != NULL);

	ld = gnc_ledger_display_gl ();
	page = gnc_plugin_page_register_new (ld);
	gnc_main_window_open_page (data->window, page);
}

/*static void
gnc_main_window_cmd_tools_general_ledger (EggAction *action, GncMainWindow *window)
{
	GNCLedgerDisplay *ld;
	GNCSplitReg *gsr;
	RegWindow *regData;

	ld = gnc_ledger_display_gl ();
	gsr = gnc_ledger_display_get_user_data (ld);
	if (!gsr) {
		regData = regWindowLedger (ld);
		gnc_register_raise (regData);
	} else {
		gnc_split_reg_raise (gsr);
	}
}

*/
