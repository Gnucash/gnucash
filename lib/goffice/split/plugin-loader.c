/*
 * plugin-loader.c: Base class for plugin loaders.
 *
 * Author: Zbigniew Chyla (cyba@gnome.pl)
 */

#include <config.h>
#include <glib/gi18n.h>
#include "gnumeric.h"
#include "plugin-loader.h"

#include "plugin.h"
#include "plugin-service.h"

#include <gsf/gsf-impl-utils.h>

#define PL_GET_CLASS(loader)	GNM_PLUGIN_LOADER_CLASS (G_OBJECT_GET_CLASS (loader))

static GObjectClass *parent_class = NULL;

static void
gnm_plugin_loader_init (GnmPluginLoader *loader)
{
	g_return_if_fail (IS_GNM_PLUGIN_LOADER (loader));

	loader->plugin = NULL;
	loader->is_base_loaded = FALSE;
	loader->n_loaded_services = 0;
}

static void
gnm_plugin_loader_finalize (GObject *obj)
{
	g_return_if_fail (IS_GNM_PLUGIN_LOADER (obj));

	parent_class->finalize (obj);
}

static void
gnm_plugin_loader_unload_service_general_real (GnmPluginLoader *loader,
                                                    GnmPluginService *service,
                                                    ErrorInfo **ret_error)
{
	PluginServiceGeneralCallbacks *cbs;

	g_return_if_fail (IS_GNM_PLUGIN_LOADER (loader));
	g_return_if_fail (IS_GNM_PLUGIN_SERVICE_GENERAL (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	cbs = plugin_service_get_cbs (service);
	cbs->plugin_func_init = NULL;
	cbs->plugin_func_cleanup = NULL;
}

static void
gnm_plugin_loader_unload_service_plugin_loader_real (GnmPluginLoader *loader,
                                                          GnmPluginService *service,
                                                          ErrorInfo **ret_error)
{
	PluginServicePluginLoaderCallbacks *cbs;

	g_return_if_fail (IS_GNM_PLUGIN_LOADER (loader));
	g_return_if_fail (IS_GNM_PLUGIN_SERVICE_PLUGIN_LOADER (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	cbs = plugin_service_get_cbs (service);
	cbs->plugin_func_get_loader_type = NULL;
}

static void
gnm_plugin_loader_unload_service_ui_real (GnmPluginLoader *loader,
					  GnmPluginService *service,
					  ErrorInfo **ret_error)
{
	PluginServiceUICallbacks *cbs;

	g_return_if_fail (IS_GNM_PLUGIN_LOADER (loader));
	g_return_if_fail (IS_GNM_PLUGIN_SERVICE_UI (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	cbs = plugin_service_get_cbs (service);
	cbs->plugin_func_exec_action = NULL;
}

static void
gnm_plugin_loader_class_init (GObjectClass *gobject_class)
{
	GnmPluginLoaderClass *plugin_loader_class = GNM_PLUGIN_LOADER_CLASS (gobject_class);

	parent_class = g_type_class_peek_parent (gobject_class);

	gobject_class->finalize = gnm_plugin_loader_finalize;

	plugin_loader_class->set_attributes = NULL;
	plugin_loader_class->load_base = NULL;
	plugin_loader_class->unload_base = NULL;
	plugin_loader_class->load_service_general = NULL;
	plugin_loader_class->unload_service_general = gnm_plugin_loader_unload_service_general_real;
        // plugin_loader_class->load_service_file_opener = NULL;
	// plugin_loader_class->unload_service_file_opener = gnm_plugin_loader_unload_service_file_opener_real;
	// plugin_loader_class->load_service_file_saver = NULL;
	// plugin_loader_class->unload_service_file_saver = gnm_plugin_loader_unload_service_file_saver_real;
	// plugin_loader_class->load_service_function_group = NULL;
	// plugin_loader_class->unload_service_function_group = gnm_plugin_loader_unload_service_function_group_real;
	plugin_loader_class->load_service_plugin_loader = NULL;
	plugin_loader_class->unload_service_plugin_loader = gnm_plugin_loader_unload_service_plugin_loader_real;
	plugin_loader_class->load_service_ui = NULL;
	plugin_loader_class->unload_service_ui = gnm_plugin_loader_unload_service_ui_real;
}

GSF_CLASS (GnmPluginLoader, gnm_plugin_loader,
	   gnm_plugin_loader_class_init, gnm_plugin_loader_init,
	   G_TYPE_OBJECT)

void
gnm_plugin_loader_set_attributes (GnmPluginLoader *loader,
                                       GHashTable *attrs,
                                       ErrorInfo **ret_error)
{
	g_return_if_fail (IS_GNM_PLUGIN_LOADER (loader));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	if (PL_GET_CLASS (loader)->set_attributes ) {
		PL_GET_CLASS (loader)->set_attributes (loader, attrs, ret_error);
	} else {
		*ret_error = error_info_new_printf (_("Loader has no set_attributes method.\n"));
	}
}

void
gnm_plugin_loader_set_plugin (GnmPluginLoader *loader, GnmPlugin *plugin)
{
	g_return_if_fail (IS_GNM_PLUGIN_LOADER (loader));
	g_return_if_fail (IS_GNM_PLUGIN (plugin));

	loader->plugin = plugin;
}

void
gnm_plugin_loader_load_base (GnmPluginLoader *loader, ErrorInfo **ret_error)
{
	GnmPluginLoaderClass *gnm_plugin_loader_class;
	ErrorInfo *error = NULL;

	g_return_if_fail (IS_GNM_PLUGIN_LOADER (loader));
	g_return_if_fail (!loader->is_base_loaded);

	GNM_INIT_RET_ERROR_INFO (ret_error);
	gnm_plugin_loader_class = PL_GET_CLASS (loader);
	if (gnm_plugin_loader_class->load_base != NULL) {
		gnm_plugin_loader_class->load_base (loader, &error);
	} else {
		*ret_error = error_info_new_printf (_("Loader has no load_base method.\n"));
	}
	if (error == NULL) {
		loader->is_base_loaded = TRUE;
		plugin_message (3, "Loaded plugin \"%s\".\n", gnm_plugin_get_id (loader->plugin));
	} else {
		*ret_error = error;
	}
}

void
gnm_plugin_loader_unload_base (GnmPluginLoader *loader, ErrorInfo **ret_error)
{
	GnmPluginLoaderClass *gnm_plugin_loader_class;
	ErrorInfo *error = NULL;

	g_return_if_fail (IS_GNM_PLUGIN_LOADER (loader));
	g_return_if_fail (loader->is_base_loaded);

	GNM_INIT_RET_ERROR_INFO (ret_error);
	gnm_plugin_loader_class = PL_GET_CLASS (loader);
	if (gnm_plugin_loader_class->unload_base != NULL) {
		gnm_plugin_loader_class->unload_base (loader, &error);
		if (error == NULL) {
			loader->is_base_loaded = FALSE;
			plugin_message (3, "Unloaded plugin \"%s\".\n", gnm_plugin_get_id (loader->plugin));
		} else {
			*ret_error = error;
		}
	}
}

void
gnm_plugin_loader_load_service (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error)
{
	GnmPluginLoaderClass *gnm_plugin_loader_class;
	void (*load_service_method) (GnmPluginLoader *, GnmPluginService *, ErrorInfo **) = NULL;

	g_return_if_fail (IS_GNM_PLUGIN_LOADER (loader));
	g_return_if_fail (IS_GNM_PLUGIN_SERVICE (service));
	g_return_if_fail (loader->is_base_loaded);

	GNM_INIT_RET_ERROR_INFO (ret_error);
	gnm_plugin_loader_class = PL_GET_CLASS (loader);
	if (IS_GNM_PLUGIN_SERVICE_GENERAL (service)) {
		load_service_method = gnm_plugin_loader_class->load_service_general;
	/*} else if (IS_GNM_PLUGIN_SERVICE_FILE_OPENER (service)) {
		load_service_method = gnm_plugin_loader_class->load_service_file_opener;
	} else if (IS_GNM_PLUGIN_SERVICE_FILE_SAVER (service)) {
		load_service_method = gnm_plugin_loader_class->load_service_file_saver;
	} else if (IS_GNM_PLUGIN_SERVICE_FUNCTION_GROUP (service)) {
		load_service_method = gnm_plugin_loader_class->load_service_function_group;*/
	} else if (IS_GNM_PLUGIN_SERVICE_PLUGIN_LOADER (service)) {
		load_service_method = gnm_plugin_loader_class->load_service_plugin_loader;
	} else if (IS_GNM_PLUGIN_SERVICE_UI (service)) {
		load_service_method = gnm_plugin_loader_class->load_service_ui;
	} else if (IS_GNM_PLUGIN_SERVICE_SIMPLE (service)) {
		load_service_method = NULL;
	} else {
		*ret_error = error_info_new_printf (_("Service '%s' not supported by loader."),
			G_OBJECT_TYPE_NAME (service));
	}
	if (load_service_method != NULL)
		load_service_method (loader, service, ret_error);

	if (*ret_error == NULL)
		loader->n_loaded_services++;
}

void
gnm_plugin_loader_unload_service (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error)
{
	GnmPluginLoaderClass *gnm_plugin_loader_class;
	void (*unload_service_method) (GnmPluginLoader *, GnmPluginService *, ErrorInfo **) = NULL;
	ErrorInfo *error = NULL;

	g_return_if_fail (IS_GNM_PLUGIN_LOADER (loader));
	g_return_if_fail (IS_GNM_PLUGIN_SERVICE (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	gnm_plugin_loader_class = PL_GET_CLASS (loader);

	if (IS_GNM_PLUGIN_SERVICE_GENERAL (service)) {
		unload_service_method = gnm_plugin_loader_class->unload_service_general;
        /*} else if (IS_GNM_PLUGIN_SERVICE_FILE_OPENER (service)) {
		unload_service_method = gnm_plugin_loader_class->unload_service_file_opener;
	} else if (IS_GNM_PLUGIN_SERVICE_FILE_SAVER (service)) {
               unload_service_method = gnm_plugin_loader_class->unload_service_file_saver;
	} else if (IS_GNM_PLUGIN_SERVICE_FUNCTION_GROUP (service)) {
		unload_service_method = gnm_plugin_loader_class->unload_service_function_group;*/
	} else if (IS_GNM_PLUGIN_SERVICE_PLUGIN_LOADER (service)) {
		unload_service_method = gnm_plugin_loader_class->unload_service_plugin_loader;
	} else if (IS_GNM_PLUGIN_SERVICE_UI (service)) {
		unload_service_method = gnm_plugin_loader_class->unload_service_ui;
	} else if (IS_GNM_PLUGIN_SERVICE_SIMPLE (service)) {
		unload_service_method = NULL;
	} else
		*ret_error = error_info_new_printf (_("Service '%s' not supported by loader."),
			G_OBJECT_TYPE_NAME (service));

	if (unload_service_method != NULL)
		unload_service_method (loader, service, &error);
	if (error == NULL) {
		g_return_if_fail (loader->n_loaded_services > 0);
		loader->n_loaded_services--;
		if (loader->n_loaded_services == 0) {
			gnm_plugin_loader_unload_base (loader, &error);
			error_info_free (error);
		}
	} else {
		*ret_error = error;
	}
}

gboolean
gnm_plugin_loader_is_base_loaded (GnmPluginLoader *loader)
{
	g_return_val_if_fail (IS_GNM_PLUGIN_LOADER (loader), FALSE);

	return loader->is_base_loaded;
}
