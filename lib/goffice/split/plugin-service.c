/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * plugin-service.c: Plugin services - reading XML info, activating, etc.
 *                   (everything independent of plugin loading method)
 *
 * Author: Zbigniew Chyla (cyba@gnome.pl)
 */

#include <config.h>
#include "gnumeric.h"
#include "plugin-service-impl.h"

#include "gutils.h"
//#include "workbook.h"
//#include "workbook-view.h"
//#include "func.h"
#include "io-context.h"
#include "error-info.h"
#include "file.h"
//#include "file-priv.h"
#include "plugin.h"
#include "xml-io.h"

#include <gsf/gsf-input.h>
#include <gsf/gsf-output.h>
#include <libxml/globals.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-utils.h>
#include <glib/gi18n.h>

#include <string.h>

static GHashTable *services = NULL;

#if 0
static FileFormatLevel
parse_format_level_str (gchar const *format_level_str, FileFormatLevel def)
{
	FileFormatLevel	format_level;

	if (format_level_str == NULL) {
		format_level = def;
	} else if (g_ascii_strcasecmp (format_level_str, "none") == 0) {
		format_level = FILE_FL_NONE;
	} else if (g_ascii_strcasecmp (format_level_str, "write_only") == 0) {
		format_level = FILE_FL_WRITE_ONLY;
	} else if (g_ascii_strcasecmp (format_level_str, "new") == 0) {
		format_level = FILE_FL_NEW;
	} else if (g_ascii_strcasecmp (format_level_str, "manual") == 0) {
		format_level = FILE_FL_MANUAL;
	} else if (g_ascii_strcasecmp (format_level_str, "manual_remember") == 0) {
		format_level = FILE_FL_MANUAL_REMEMBER;
	} else if (g_ascii_strcasecmp (format_level_str, "auto") == 0) {
		format_level = FILE_FL_AUTO;
	} else {
		format_level = def;
	}

	return format_level;
}

static GHashTable *
get_plugin_file_savers_hash (GnmPlugin *plugin)
{
	GHashTable *hash;

	hash = g_object_get_data (G_OBJECT (plugin), "file_savers_hash");
	if (hash == NULL) {
		hash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
		g_object_set_data_full (
			G_OBJECT (plugin), "file_savers_hash",
			hash, (GDestroyNotify) g_hash_table_destroy);
	}

	return hash;
}
#endif // 0 -- unused, jsled


static void
plugin_service_init (GObject *obj)
{
	GnmPluginService *service = GNM_PLUGIN_SERVICE (obj);

	service->id = NULL;
	service->is_active = FALSE;
	service->is_loaded = FALSE;
	service->plugin = NULL;
	service->cbs_ptr = NULL;
	service->saved_description = NULL;
}

static void
plugin_service_finalize (GObject *obj)
{
	GnmPluginService *service = GNM_PLUGIN_SERVICE (obj);
	GObjectClass *parent_class;

	g_free (service->id);
	service->id = NULL;
	g_free (service->saved_description);
	service->saved_description = NULL;

	parent_class = g_type_class_peek (G_TYPE_OBJECT);
	parent_class->finalize (obj);
}

static void
plugin_service_class_init (GObjectClass *gobject_class)
{
	GnmPluginServiceClass *plugin_service_class = GPS_CLASS (gobject_class);

	gobject_class->finalize = plugin_service_finalize;
	plugin_service_class->read_xml = NULL;
	plugin_service_class->activate = NULL;
	plugin_service_class->deactivate = NULL;
	plugin_service_class->get_description = NULL;
}

GSF_CLASS (GnmPluginService, plugin_service,
	   plugin_service_class_init, plugin_service_init,
           G_TYPE_OBJECT)


/****************************************************************************/

/*
 * PluginServiceGeneral
 */

typedef struct{
	GnmPluginServiceClass plugin_service_class;
} PluginServiceGeneralClass;

struct _PluginServiceGeneral {
	GnmPluginService plugin_service;
	PluginServiceGeneralCallbacks cbs;
};


static void
plugin_service_general_init (GObject *obj)
{
	PluginServiceGeneral *service_general = GNM_PLUGIN_SERVICE_GENERAL (obj);

	GNM_PLUGIN_SERVICE (obj)->cbs_ptr = &service_general->cbs;
	service_general->cbs.plugin_func_init = NULL;
	service_general->cbs.plugin_func_cleanup = NULL;
}

static void
plugin_service_general_activate (GnmPluginService *service, ErrorInfo **ret_error)
{
	PluginServiceGeneral *service_general = GNM_PLUGIN_SERVICE_GENERAL (service);
	ErrorInfo *error = NULL;

	GNM_INIT_RET_ERROR_INFO (ret_error);
	plugin_service_load (service, &error);
	if (error != NULL) {
		*ret_error = error_info_new_str_with_details (
		             _("Error while loading plugin service."),
		             error);
		return;
	}
	g_return_if_fail (service_general->cbs.plugin_func_init != NULL);
	service_general->cbs.plugin_func_init (service, &error);
	if (error != NULL) {
		*ret_error = error_info_new_str_with_details (
		             _("Initializing function inside plugin returned error."),
		             error);
		return;
	}
	service->is_active = TRUE;
}

static void
plugin_service_general_deactivate (GnmPluginService *service, ErrorInfo **ret_error)
{
	PluginServiceGeneral *service_general = GNM_PLUGIN_SERVICE_GENERAL (service);
	ErrorInfo *error = NULL;

	GNM_INIT_RET_ERROR_INFO (ret_error);
	g_return_if_fail (service_general->cbs.plugin_func_cleanup != NULL);
	service_general->cbs.plugin_func_cleanup (service, &error);
	if (error != NULL) {
		*ret_error = error_info_new_str_with_details (
		             _("Cleanup function inside plugin returned error."),
		             error);
		return;
	}
	service->is_active = FALSE;
}

static char *
plugin_service_general_get_description (GnmPluginService *service)
{
	return g_strdup (_("General"));
}

static void
plugin_service_general_class_init (GObjectClass *gobject_class)
{
	GnmPluginServiceClass *plugin_service_class = GPS_CLASS (gobject_class);

	plugin_service_class->activate = plugin_service_general_activate;
	plugin_service_class->deactivate = plugin_service_general_deactivate;
	plugin_service_class->get_description = plugin_service_general_get_description;
}

GSF_CLASS (PluginServiceGeneral, plugin_service_general,
           plugin_service_general_class_init, plugin_service_general_init,
           GNM_PLUGIN_SERVICE_TYPE)

/** -- **/

/*
 * PluginServicePluginLoader
 */

typedef struct{
	GnmPluginServiceClass plugin_service_class;
} PluginServicePluginLoaderClass;

struct _PluginServicePluginLoader {
	GnmPluginService plugin_service;
	PluginServicePluginLoaderCallbacks cbs;
};


static void
plugin_service_plugin_loader_init (GObject *obj)
{
	PluginServicePluginLoader *service_plugin_loader = GNM_PLUGIN_SERVICE_PLUGIN_LOADER (obj);

	GNM_PLUGIN_SERVICE (obj)->cbs_ptr = &service_plugin_loader->cbs;
}

GType
plugin_service_plugin_loader_generate_type (GnmPluginService *service, ErrorInfo **ret_error)
{
	PluginServicePluginLoader *service_plugin_loader = GNM_PLUGIN_SERVICE_PLUGIN_LOADER (service);
	ErrorInfo *error = NULL;
	GType loader_type;

	GNM_INIT_RET_ERROR_INFO (ret_error);
	plugin_service_load (service, &error);
	if (error == NULL) {
		loader_type = service_plugin_loader->cbs.plugin_func_get_loader_type (
			service, &error);
		if (error == NULL)
			return loader_type;
		*ret_error = error;
	} else {
		*ret_error = error_info_new_str_with_details (
		             _("Error while loading plugin service."),
		             error);
	}
	return G_TYPE_NONE;
}

static void
plugin_service_plugin_loader_activate (GnmPluginService *service, ErrorInfo **ret_error)
{
	gchar *full_id;

	GNM_INIT_RET_ERROR_INFO (ret_error);
	full_id = g_strconcat (
		gnm_plugin_get_id (service->plugin), ":", service->id, NULL);
	plugins_register_loader (full_id, service);
	g_free (full_id);
	service->is_active = TRUE;
}

static void
plugin_service_plugin_loader_deactivate (GnmPluginService *service, ErrorInfo **ret_error)
{
	gchar *full_id;

	GNM_INIT_RET_ERROR_INFO (ret_error);
	full_id = g_strconcat (
		gnm_plugin_get_id (service->plugin), ":", service->id, NULL);
	plugins_register_loader (full_id, service);
	g_free (full_id);
	service->is_active = FALSE;
}

static char *
plugin_service_plugin_loader_get_description (GnmPluginService *service)
{
	return g_strdup (_("Plugin loader"));
}

static void
plugin_service_plugin_loader_class_init (GObjectClass *gobject_class)
{
	GnmPluginServiceClass *plugin_service_class = GPS_CLASS (gobject_class);

	plugin_service_class->activate = plugin_service_plugin_loader_activate;
	plugin_service_class->deactivate = plugin_service_plugin_loader_deactivate;
	plugin_service_class->get_description = plugin_service_plugin_loader_get_description;
}

GSF_CLASS (PluginServicePluginLoader, plugin_service_plugin_loader,
           plugin_service_plugin_loader_class_init, plugin_service_plugin_loader_init,
           GNM_PLUGIN_SERVICE_TYPE)


/*
 * PluginServiceUI
 */
typedef struct{
	GnmPluginServiceClass plugin_service_class;
} PluginServiceUIClass;

struct _PluginServiceUI {
	GnmPluginService plugin_service;

	char *file_name;
	GSList *actions;

	gpointer layout_id;
	PluginServiceUICallbacks cbs;
};

static void
plugin_service_ui_init (GObject *obj)
{
	PluginServiceUI *service_ui = GNM_PLUGIN_SERVICE_UI (obj);

	GNM_PLUGIN_SERVICE (obj)->cbs_ptr = &service_ui->cbs;
	service_ui->file_name = NULL;
	service_ui->actions = NULL;
	service_ui->layout_id = NULL;
	service_ui->cbs.plugin_func_exec_action = NULL;
}

static void
plugin_service_ui_finalize (GObject *obj)
{
	PluginServiceUI *service_ui = GNM_PLUGIN_SERVICE_UI (obj);
	GObjectClass *parent_class;

	g_free (service_ui->file_name);
	service_ui->file_name = NULL;
	gnm_slist_free_custom (service_ui->actions, (GFreeFunc)gnm_action_free);
	service_ui->actions = NULL;

	parent_class = g_type_class_peek (GNM_PLUGIN_SERVICE_TYPE);
	parent_class->finalize (obj);
}

static void
cb_ui_service_activate (GnmAction const *action, WorkbookControl *wbc, GnmPluginService *service)
{
	ErrorInfo *load_error = NULL;

	plugin_service_load (service, &load_error);
	if (load_error == NULL) {
		PluginServiceUI *service_ui = GNM_PLUGIN_SERVICE_UI (service);
		ErrorInfo *ignored_error = NULL;

		g_return_if_fail (service_ui->cbs.plugin_func_exec_action != NULL);
		service_ui->cbs.plugin_func_exec_action (
			service, action, wbc, &ignored_error);
		if (ignored_error != NULL) {
			error_info_print (ignored_error);
			error_info_free (ignored_error);
		}
	} else {
		error_info_print (load_error);
		error_info_free (load_error);
	}
}

static void
plugin_service_ui_read_xml (GnmPluginService *service, xmlNode *tree, ErrorInfo **ret_error)
{
	PluginServiceUI *service_ui = GNM_PLUGIN_SERVICE_UI (service);
	char *file_name;
	xmlNode *verbs_node;
	GSList *actions = NULL;

	GNM_INIT_RET_ERROR_INFO (ret_error);
	file_name = xml_node_get_cstr (tree, "file");
	if (file_name == NULL) {
		*ret_error = error_info_new_str (
		             _("Missing file name."));
		return;
	}
	verbs_node = e_xml_get_child_by_name (tree, "actions");
	if (verbs_node != NULL) {
		xmlNode *ptr;
		xmlChar *name, *label, *icon;
		gboolean always_available;
		GnmAction *action;

		for (ptr = verbs_node->xmlChildrenNode; ptr != NULL; ptr = ptr->next) {
			if (xmlIsBlankNode (ptr) || ptr->name == NULL ||
			    strcmp (ptr->name, "action"))
				continue;
			name  = xml_node_get_cstr (ptr, "name");
			label = xml_node_get_cstr (ptr, "label");
			icon  = xml_node_get_cstr (ptr, "icon");
			if (!xml_node_get_bool (ptr, "always_available", &always_available))
				always_available = FALSE;
			action = gnm_action_new (name, label, icon, always_available,
				(GnmActionHandler) cb_ui_service_activate);
			if (NULL != name) xmlFree (name);
			if (NULL != name) xmlFree (label);
			if (NULL != name) xmlFree (icon);
			if (NULL != action)
				GNM_SLIST_PREPEND (actions, action);
		}
	}
	GNM_SLIST_REVERSE (actions);

	service_ui->file_name = file_name;
	service_ui->actions = actions;
}

static void
plugin_service_ui_activate (GnmPluginService *service, ErrorInfo **ret_error)
{
	PluginServiceUI *service_ui = GNM_PLUGIN_SERVICE_UI (service);
	GError *err = NULL;
	char *full_file_name;
	char *xml_ui;
	char const *textdomain;

	GNM_INIT_RET_ERROR_INFO (ret_error);
	full_file_name = g_build_filename (
		gnm_plugin_get_dir_name (service->plugin),
		service_ui->file_name, NULL);
	if (!g_file_get_contents (full_file_name, &xml_ui, NULL, &err)) {
		*ret_error = error_info_new_printf (
		             _("Cannot read UI description from XML file %s."),
		             full_file_name);
		g_free (full_file_name);
		return;
	}
	g_free (full_file_name);

	textdomain = gnm_plugin_get_textdomain (service->plugin);
	service_ui->layout_id = gnm_app_add_extra_ui (
		service_ui->actions,
		xml_ui, textdomain, service);
	service->is_active = TRUE;
}

static void
plugin_service_ui_deactivate (GnmPluginService *service, ErrorInfo **ret_error)
{
	PluginServiceUI *service_ui = GNM_PLUGIN_SERVICE_UI (service);

	GNM_INIT_RET_ERROR_INFO (ret_error);
	gnm_app_remove_extra_ui (service_ui->layout_id);
	service_ui->layout_id = NULL;
	service->is_active = FALSE;
}

static char *
plugin_service_ui_get_description (GnmPluginService *service)
{
	PluginServiceUI *service_ui = GNM_PLUGIN_SERVICE_UI (service);
	int n_actions;

	n_actions = g_slist_length (service_ui->actions);
	return g_strdup_printf (
		ngettext (
			N_("User interface with %d action"),
			N_("User interface with %d actions"),
			n_actions),
		n_actions);
}

static void
plugin_service_ui_class_init (GObjectClass *gobject_class)
{
	GnmPluginServiceClass *plugin_service_class = GPS_CLASS (gobject_class);

	gobject_class->finalize = plugin_service_ui_finalize;
	plugin_service_class->read_xml = plugin_service_ui_read_xml;
	plugin_service_class->activate = plugin_service_ui_activate;
	plugin_service_class->deactivate = plugin_service_ui_deactivate;
	plugin_service_class->get_description = plugin_service_ui_get_description;
}

GSF_CLASS (PluginServiceUI, plugin_service_ui,
           plugin_service_ui_class_init, plugin_service_ui_init,
           GNM_PLUGIN_SERVICE_TYPE)

/**************************************************************************
 * PluginServiceGObjectLoader
 */

static char *
plugin_service_gobject_loader_get_description (GnmPluginService *service)
{
	return g_strdup (_("GObject loader"));
}

static void
plugin_service_gobject_loader_read_xml (GnmPluginService *service,
					G_GNUC_UNUSED xmlNode *tree,
					G_GNUC_UNUSED ErrorInfo **ret_error)
{
	PluginServiceGObjectLoaderClass *gobj_loader_class = GPS_GOBJECT_LOADER_GET_CLASS (service);
	g_return_if_fail (gobj_loader_class->pending != NULL);
	g_hash_table_replace (gobj_loader_class->pending, service->id, service);
}

static void
plugin_service_gobject_loader_class_init (PluginServiceGObjectLoaderClass *gobj_loader_class)
{
	GnmPluginServiceClass *psc = GPS_CLASS (gobj_loader_class);

	psc->get_description	= plugin_service_gobject_loader_get_description;
	psc->read_xml		= plugin_service_gobject_loader_read_xml;
	gobj_loader_class->pending = NULL;
}

GSF_CLASS (PluginServiceGObjectLoader, plugin_service_gobject_loader,
           plugin_service_gobject_loader_class_init, NULL,
           GNM_PLUGIN_SERVICE_SIMPLE_TYPE)

/**************************************************************************
 * PluginServiceSimple
 */

static void
plugin_service_simple_activate (GnmPluginService *service, ErrorInfo **ret_error)
{
	service->is_active = TRUE;
}

static void
plugin_service_simple_deactivate (GnmPluginService *service, ErrorInfo **ret_error)
{
	service->is_active = FALSE;
}

static void
plugin_service_simple_class_init (GObjectClass *gobject_class)
{
	GnmPluginServiceClass *psc = GPS_CLASS (gobject_class);

	psc->activate		= plugin_service_simple_activate;
	psc->deactivate		= plugin_service_simple_deactivate;
}

GSF_CLASS (PluginServiceSimple, plugin_service_simple,
           plugin_service_simple_class_init,
	   NULL,
           GNM_PLUGIN_SERVICE_TYPE)

/* ---------------------------------------------------------------------- */

void
plugin_service_load (GnmPluginService *service, ErrorInfo **ret_error)
{
	g_return_if_fail (IS_GNM_PLUGIN_SERVICE (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);

	if (service->is_loaded)
		return;
	gnm_plugin_load_service (service->plugin, service, ret_error);
	if (*ret_error == NULL)
		service->is_loaded = TRUE;
}

void
plugin_service_unload (GnmPluginService *service, ErrorInfo **ret_error)
{
	ErrorInfo *error = NULL;

	g_return_if_fail (IS_GNM_PLUGIN_SERVICE (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	if (!service->is_loaded) {
		return;
	}
	gnm_plugin_unload_service (service->plugin, service, &error);
	if (error == NULL) {
		service->is_loaded = FALSE;
	} else {
		*ret_error = error;
	}
}

GnmPluginService *
plugin_service_new (GnmPlugin *plugin, xmlNode *tree, ErrorInfo **ret_error)
{
	GnmPluginService *service = NULL;
	char *type_str;
	ErrorInfo *service_error = NULL;
	GnmPluginServiceCreate ctor;

	g_return_val_if_fail (IS_GNM_PLUGIN (plugin), NULL);
	g_return_val_if_fail (tree != NULL, NULL);
	g_return_val_if_fail (strcmp (tree->name, "service") == 0, NULL);

	GNM_INIT_RET_ERROR_INFO (ret_error);
	type_str = xml_node_get_cstr (tree, "type");
	if (type_str == NULL) {
		*ret_error = error_info_new_str (_("No \"type\" attribute on \"service\" element."));
		return NULL;
	}

	ctor = g_hash_table_lookup (services, type_str);
	if (ctor == NULL) {
		*ret_error = error_info_new_printf (_("Unknown service type: %s."), type_str);
		g_free (type_str);
		return NULL;
	}
	g_free (type_str);

	service = g_object_new (ctor(), NULL);
	service->plugin = plugin;
	service->id = xml_node_get_cstr (tree, "id");
	if (service->id == NULL)
		service->id = g_strdup ("default");

	if (GPS_GET_CLASS (service)->read_xml != NULL) {
		GPS_GET_CLASS (service)->read_xml (service, tree, &service_error);
		if (service_error != NULL) {
			*ret_error = error_info_new_str_with_details (
				_("Error reading service information."), service_error);
			g_object_unref (service);
			service = NULL;
		}
	}

	return service;
}

char const *
plugin_service_get_id (GnmPluginService *service)
{
	g_return_val_if_fail (IS_GNM_PLUGIN_SERVICE (service), NULL);

	return service->id;
}

char const *
plugin_service_get_description (GnmPluginService *service)
{
	g_return_val_if_fail (IS_GNM_PLUGIN_SERVICE (service), NULL);

	if (service->saved_description == NULL) {
		service->saved_description = GPS_GET_CLASS (service)->get_description (service);
	}

	return service->saved_description;
}

GnmPlugin *
plugin_service_get_plugin (GnmPluginService *service)
{
	g_return_val_if_fail (IS_GNM_PLUGIN_SERVICE (service), NULL);

	return service->plugin;
}

gpointer
plugin_service_get_cbs (GnmPluginService *service)
{
	g_return_val_if_fail (IS_GNM_PLUGIN_SERVICE (service), NULL);
	g_return_val_if_fail (service->cbs_ptr != NULL, NULL);

	return service->cbs_ptr;
}

void
plugin_service_activate (GnmPluginService *service, ErrorInfo **ret_error)
{
	g_return_if_fail (IS_GNM_PLUGIN_SERVICE (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	if (service->is_active) {
		return;
	}
#ifdef PLUGIN_ALWAYS_LOAD
	{
		ErrorInfo *load_error = NULL;

		plugin_service_load (service, &load_error);
		if (load_error != NULL) {
			*ret_error = error_info_new_str_with_details (
				_("We must load service before activating it (PLUGIN_ALWAYS_LOAD is set) "
				  "but loading failed."), load_error);
			return;
		}
	}
#endif
	GPS_GET_CLASS (service)->activate (service, ret_error);
}

void
plugin_service_deactivate (GnmPluginService *service, ErrorInfo **ret_error)
{
	g_return_if_fail (IS_GNM_PLUGIN_SERVICE (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	if (!service->is_active) {
		return;
	}
	GPS_GET_CLASS (service)->deactivate (service, ret_error);
	if (*ret_error == NULL) {
		ErrorInfo *ignored_error = NULL;

		service->is_active = FALSE;
		/* FIXME */
		plugin_service_unload (service, &ignored_error);
		error_info_free (ignored_error);
	}
}

/*****************************************************************************/

void
plugin_services_init (void)
{
	static struct {
		char const *type_str;
		GnmPluginServiceCreate ctor;
	} const builtin_services[] = {
		{ "general",		plugin_service_general_get_type},
		//{ "clipboard",		plugin_service_clipboard_get_type},
		//{ "file_opener",	plugin_service_file_opener_get_type},
		//{ "file_saver",		plugin_service_file_saver_get_type},
		//{ "function_group",	plugin_service_function_group_get_type},
		{ "plugin_loader",	plugin_service_plugin_loader_get_type},
		{ "ui",			plugin_service_ui_get_type}
/* base classes, not really for direct external use,
 * put here for expositional purposes
 */
#if 0
		{ "gobject_loader",	plugin_service_gobject_loader_get_type}
		{ "simple",		plugin_service_simple_get_type}
#endif
	};
	unsigned i;

	g_return_if_fail (services == NULL);

	services = g_hash_table_new (g_str_hash, g_str_equal);
	for (i = 0; i < G_N_ELEMENTS (builtin_services); i++)
		plugin_service_define (builtin_services[i].type_str,
				       builtin_services[i].ctor);
}

void
plugin_services_shutdown (void)
{
	g_return_if_fail (services != NULL);
	g_hash_table_destroy (services);
	services = NULL;
}

/**
 * Allow the definition of new service types
 **/
void
plugin_service_define (char const *type_str, GnmPluginServiceCreate ctor)
{
	g_return_if_fail (services != NULL);

	g_return_if_fail (NULL == g_hash_table_lookup (services, type_str));

	g_hash_table_insert (services, (gpointer)type_str, ctor);
}
