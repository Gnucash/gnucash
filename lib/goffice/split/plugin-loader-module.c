/*
 * plugin-loader-module.c: Support for "g_module" (shared libraries) plugins.
 *
 * Author: Zbigniew Chyla (cyba@gnome.pl)
 */

#include <config.h>
#include <glib/gi18n.h>
#include "gnumeric.h"
#include "plugin-loader-module.h"
#include "module-plugin-defs.h"

#include "gutils.h"
#include "file.h"
#include "plugin.h"
#include "plugin-service.h"
#include "plugin-loader.h"

#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-output.h>
#include <libxml/parser.h>
#include <libxml/parserInternals.h>
#include <libxml/xmlmemory.h>
#include <string.h>

struct _GnmPluginLoaderModule {
	GnmPluginLoader loader;

	gchar *module_file_name;

	GModule *handle;
	void (*plugin_init_func) (void);
	void (*plugin_cleanup_func) (void);
};

struct _GnmPluginLoaderModuleClass {
	GnmPluginLoaderClass parent_class;
};

#define PARENT_TYPE (gnm_plugin_loader_get_type ())

static GObjectClass *parent_class = NULL;

static void gnm_plugin_loader_module_set_attributes (GnmPluginLoader *loader, GHashTable *attrs, ErrorInfo **ret_error);
static void gnm_plugin_loader_module_load_base (GnmPluginLoader *loader, ErrorInfo **ret_error);
static void gnm_plugin_loader_module_unload_base (GnmPluginLoader *loader, ErrorInfo **ret_error);

static void gnm_plugin_loader_module_load_service_general (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
#if 0
static void gnm_plugin_loader_module_load_service_file_opener (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
static void gnm_plugin_loader_module_load_service_file_saver (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
static void gnm_plugin_loader_module_load_service_function_group (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
#endif // 0 -- unused, jsled
static void gnm_plugin_loader_module_load_service_plugin_loader (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
static void gnm_plugin_loader_module_load_service_ui (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);

static void
gnm_plugin_loader_module_set_attributes (GnmPluginLoader *loader,
                                              GHashTable *attrs,
                                              ErrorInfo **ret_error)
{
	GnmPluginLoaderModule *loader_module = GNM_PLUGIN_LOADER_MODULE (loader);
	gchar *module_file_name = NULL;

	GNM_INIT_RET_ERROR_INFO (ret_error);
	module_file_name = g_hash_table_lookup (attrs, "module_file");
	if (module_file_name != NULL) {
		loader_module->module_file_name = g_strdup (module_file_name);
	} else {
		*ret_error = error_info_new_str (
		             _("Module file name not given."));
	}
}

static void
gnm_plugin_loader_module_load_base (GnmPluginLoader *loader, ErrorInfo **ret_error)
{
	GnmPluginLoaderModule *loader_module = GNM_PLUGIN_LOADER_MODULE (loader);

	GNM_INIT_RET_ERROR_INFO (ret_error);
	if (g_module_supported ()) {
		gchar *full_module_file_name;
		GModule *handle;
		ModulePluginFileStruct *plugin_file_struct = NULL;
		gpointer plugin_init_func = NULL, plugin_cleanup_func = NULL;

		full_module_file_name = g_build_filename (gnm_plugin_get_dir_name (loader->plugin),
							  loader_module->module_file_name, NULL);
		handle = g_module_open (full_module_file_name, 0);
		if (handle != NULL) {
			g_module_symbol (handle, "plugin_file_struct", (gpointer) &plugin_file_struct);
			g_module_symbol (handle, "plugin_init", &plugin_init_func);
			g_module_symbol (handle, "plugin_cleanup", &plugin_cleanup_func);
		}
		if (handle != NULL && plugin_file_struct != NULL &&
		    plugin_file_struct->magic_number == GNUMERIC_MODULE_PLUGIN_MAGIC_NUMBER &&
		    strcmp (plugin_file_struct->version, GNUMERIC_VERSION) == 0) {
			loader_module->handle = handle;
			loader_module->plugin_init_func = plugin_init_func;
			loader_module->plugin_cleanup_func = plugin_cleanup_func;
			if (loader_module->plugin_init_func != NULL) {
				loader_module->plugin_init_func ();
			}
		} else {
			if (handle == NULL) {
				*ret_error = error_info_new_printf (
				             _("Unable to open module file \"%s\"."),
				             full_module_file_name);
				error_info_add_details (*ret_error, error_info_new_str (g_module_error()));
			} else {
				*ret_error = error_info_new_printf (
				             _("Module file \"%s\" has invalid format."),
				             full_module_file_name);
				if (plugin_file_struct == NULL) {
					error_info_add_details (*ret_error,
					                        error_info_new_str (
					                        _("File doesn't contain (\"plugin_file_struct\" symbol).")));
				} else if (plugin_file_struct->magic_number != GNUMERIC_MODULE_PLUGIN_MAGIC_NUMBER) {
 					error_info_add_details (*ret_error,
 					                        error_info_new_str (
					                        _("File has a bad magic number.")));
				} else if (strcmp (plugin_file_struct->version, GNUMERIC_VERSION) == 0) {
					error_info_add_details (*ret_error,
					                        error_info_new_printf (
					                        _("Plugin version \"%s\" is different from application \"%s\"."),
					                        plugin_file_struct->version, GNUMERIC_VERSION));
 				}
				g_module_close (handle);
			}
		}
		g_free (full_module_file_name);
	} else {
		*ret_error = error_info_new_str (
		             _("Dynamic module loading is not supported in this system."));
	}
}

static void
gnm_plugin_loader_module_unload_base (GnmPluginLoader *loader, ErrorInfo **ret_error)
{
	GnmPluginLoaderModule *loader_module = GNM_PLUGIN_LOADER_MODULE (loader);

	GNM_INIT_RET_ERROR_INFO (ret_error);
	if (loader_module->plugin_cleanup_func != NULL) {
		loader_module->plugin_cleanup_func ();
	}
	if (!g_module_close (loader_module->handle)) {
		*ret_error = error_info_new_printf (
		             _("Unable to close module file \"%s\"."),
		             loader_module->module_file_name);
		error_info_add_details (*ret_error, error_info_new_str (g_module_error()));
	}
	loader_module->handle = NULL;
	loader_module->plugin_init_func = NULL;
	loader_module->plugin_cleanup_func = NULL;
}

static void
gnm_plugin_loader_module_init (GnmPluginLoaderModule *loader_module)
{
	g_return_if_fail (IS_GNM_PLUGIN_LOADER_MODULE (loader_module));

	loader_module->module_file_name = NULL;
	loader_module->handle = NULL;
}

static void
gnm_plugin_loader_module_finalize (GObject *obj)
{
	GnmPluginLoaderModule *loader_module = GNM_PLUGIN_LOADER_MODULE (obj);

	g_free (loader_module->module_file_name);
	loader_module->module_file_name = NULL;

	parent_class->finalize (obj);
}

static void
gnm_plugin_loader_module_class_init (GObjectClass *gobject_class)
{
	GnmPluginLoaderClass *gnm_plugin_loader_class = GNM_PLUGIN_LOADER_CLASS (gobject_class);

	parent_class = g_type_class_peek_parent (gobject_class);

	gobject_class->finalize = gnm_plugin_loader_module_finalize;

	gnm_plugin_loader_class->set_attributes = gnm_plugin_loader_module_set_attributes;
	gnm_plugin_loader_class->load_base = gnm_plugin_loader_module_load_base;
	gnm_plugin_loader_class->unload_base = gnm_plugin_loader_module_unload_base;
	gnm_plugin_loader_class->load_service_general = gnm_plugin_loader_module_load_service_general;
	//gnm_plugin_loader_class->load_service_file_opener = gnm_plugin_loader_module_load_service_file_opener;
	//gnm_plugin_loader_class->load_service_file_saver = gnm_plugin_loader_module_load_service_file_saver;
	//gnm_plugin_loader_class->load_service_function_group = gnm_plugin_loader_module_load_service_function_group;
	gnm_plugin_loader_class->load_service_plugin_loader = gnm_plugin_loader_module_load_service_plugin_loader;
	gnm_plugin_loader_class->load_service_ui = gnm_plugin_loader_module_load_service_ui;
}

GSF_CLASS (GnmPluginLoaderModule, gnm_plugin_loader_module,
	   gnm_plugin_loader_module_class_init,
	   gnm_plugin_loader_module_init, PARENT_TYPE)

/*
 * Service - general
 */

typedef struct {
	void (*module_func_init) (ErrorInfo **ret_error);
	void (*module_func_cleanup) (ErrorInfo **ret_error);
} ServiceLoaderDataGeneral;

static void
gnm_plugin_loader_module_func_init (GnmPluginService *service, ErrorInfo **ret_error)
{
	ErrorInfo *error = NULL;
	ServiceLoaderDataGeneral *loader_data;

	g_return_if_fail (IS_GNM_PLUGIN_SERVICE_GENERAL (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	loader_data = g_object_get_data (G_OBJECT (service), "loader_data");
	loader_data->module_func_init (&error);
	*ret_error = error;
}

static void
gnm_plugin_loader_module_func_cleanup (GnmPluginService *service, ErrorInfo **ret_error)
{
	ErrorInfo *error = NULL;
	ServiceLoaderDataGeneral *loader_data;

	g_return_if_fail (IS_GNM_PLUGIN_SERVICE_GENERAL (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	loader_data = g_object_get_data (G_OBJECT (service), "loader_data");
	loader_data->module_func_cleanup (&error);
	*ret_error = error;
}

static void
gnm_plugin_loader_module_load_service_general (GnmPluginLoader *loader,
                                                    GnmPluginService *service,
                                                    ErrorInfo **ret_error)
{
	GnmPluginLoaderModule *loader_module = GNM_PLUGIN_LOADER_MODULE (loader);
	gpointer module_func_init = NULL, module_func_cleanup = NULL;

	g_return_if_fail (IS_GNM_PLUGIN_SERVICE_GENERAL (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	g_module_symbol (loader_module->handle, "plugin_init_general", &module_func_init);
	g_module_symbol (loader_module->handle, "plugin_cleanup_general", &module_func_cleanup);
	if (module_func_init != NULL && module_func_cleanup != NULL) {
		PluginServiceGeneralCallbacks *cbs;
		ServiceLoaderDataGeneral *loader_data;

		cbs = plugin_service_get_cbs (service);
		cbs->plugin_func_init = gnm_plugin_loader_module_func_init;
		cbs->plugin_func_cleanup = gnm_plugin_loader_module_func_cleanup;

		loader_data = g_new (ServiceLoaderDataGeneral, 1);
		loader_data->module_func_init = module_func_init;
		loader_data->module_func_cleanup = module_func_cleanup;
		g_object_set_data_full (
			G_OBJECT (service), "loader_data", loader_data, g_free);
	} else {
		*ret_error = error_info_new_printf (
		             _("Module file \"%s\" has invalid format."),
		             loader_module->module_file_name);
		if (module_func_init == NULL) {
			error_info_add_details (*ret_error,
			                        error_info_new_str (
			                        _("File doesn't contain \"plugin_init_general\" function.")));
		}
		if (module_func_cleanup == NULL) {
			error_info_add_details (*ret_error,
			                        error_info_new_str (
			                        _("File doesn't contain \"plugin_cleanup_general\" function.")));
		}
	}
}

#if 0
/*
 * Service - file_opener
 */
typedef struct {
	gboolean (*module_func_file_probe) (GnmFileOpener const *fo, GsfInput *input,
	                                    FileProbeLevel pl);
	void (*module_func_file_open) (GnmFileOpener const *fo, IOContext *io_context,
	                               WorkbookView *wbv, GsfInput *input);
} ServiceLoaderDataFileOpener;

static gboolean
gnm_plugin_loader_module_func_file_probe (GnmFileOpener const *fo, GnmPluginService *service,
                                               GsfInput *input, FileProbeLevel pl)
{
	ServiceLoaderDataFileOpener *loader_data;

	g_return_val_if_fail (IS_GNM_PLUGIN_SERVICE_FILE_OPENER (service), FALSE);
	g_return_val_if_fail (input != NULL, FALSE);

	loader_data = g_object_get_data (G_OBJECT (service), "loader_data");
	return loader_data->module_func_file_probe (fo, input, pl);
}

static void
gnm_plugin_loader_module_func_file_open (GnmFileOpener const *fo, GnmPluginService *service,
                                              IOContext *io_context, WorkbookView *wbv,
                                              GsfInput *input)
{
	ServiceLoaderDataFileOpener *loader_data;

	g_return_if_fail (IS_GNM_PLUGIN_SERVICE_FILE_OPENER (service));
	g_return_if_fail (input != NULL);

	loader_data = g_object_get_data (G_OBJECT (service), "loader_data");
	loader_data->module_func_file_open (fo, io_context, wbv, input);
}

static void
gnm_plugin_loader_module_load_service_file_opener (GnmPluginLoader *loader,
                                                        GnmPluginService *service,
                                                        ErrorInfo **ret_error)
{
	GnmPluginLoaderModule *loader_module = GNM_PLUGIN_LOADER_MODULE (loader);
	gchar *func_name_file_probe, *func_name_file_open;
	gpointer module_func_file_probe = NULL, module_func_file_open = NULL;

	g_return_if_fail (IS_GNM_PLUGIN_SERVICE_FILE_OPENER (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	func_name_file_probe = g_strconcat (
		plugin_service_get_id (service), "_file_probe", NULL);
	g_module_symbol (loader_module->handle, func_name_file_probe, &module_func_file_probe);
	func_name_file_open = g_strconcat (
		plugin_service_get_id (service), "_file_open", NULL);
	g_module_symbol (loader_module->handle, func_name_file_open, &module_func_file_open);
	if (module_func_file_open != NULL) {
		PluginServiceFileOpenerCallbacks *cbs;
		ServiceLoaderDataFileOpener *loader_data;

		cbs = plugin_service_get_cbs (service);
		cbs->plugin_func_file_probe = gnm_plugin_loader_module_func_file_probe;
		cbs->plugin_func_file_open = gnm_plugin_loader_module_func_file_open;

		loader_data = g_new (ServiceLoaderDataFileOpener, 1);
		loader_data->module_func_file_probe = module_func_file_probe;
		loader_data->module_func_file_open = module_func_file_open;
		g_object_set_data_full (
			G_OBJECT (service), "loader_data", loader_data, g_free);
	} else {
		*ret_error = error_info_new_printf (
		             _("Module file \"%s\" has invalid format."),
		             loader_module->module_file_name);
		error_info_add_details (*ret_error, error_info_new_printf (
			_("File doesn't contain \"%s\" function."), func_name_file_open));
	}
	g_free (func_name_file_probe);
	g_free (func_name_file_open);
}

/*
 * Service - file_saver
 */

typedef struct {
	void (*module_func_file_save) (GnmFileSaver const *fs, IOContext *io_context,
	                               WorkbookView const *wbv, GsfOutput *output);
} ServiceLoaderDataFileSaver;

static void
gnm_plugin_loader_module_func_file_save (GnmFileSaver const *fs, GnmPluginService *service,
                                              IOContext *io_context, WorkbookView const *wbv,
					      GsfOutput *output)
{
	ServiceLoaderDataFileSaver *loader_data;

	g_return_if_fail (IS_GNM_PLUGIN_SERVICE_FILE_SAVER (service));
	g_return_if_fail (GSF_IS_OUTPUT (output));

	loader_data = g_object_get_data (G_OBJECT (service), "loader_data");
	loader_data->module_func_file_save (fs, io_context, wbv, output);
}

static void
gnm_plugin_loader_module_load_service_file_saver (GnmPluginLoader *loader,
                                                       GnmPluginService *service,
                                                       ErrorInfo **ret_error)
{
	GnmPluginLoaderModule *loader_module = GNM_PLUGIN_LOADER_MODULE (loader);
	gchar *func_name_file_save;
	gpointer module_func_file_save = NULL;

	g_return_if_fail (IS_GNM_PLUGIN_SERVICE_FILE_SAVER (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	func_name_file_save = g_strconcat (
		plugin_service_get_id (service), "_file_save", NULL);
	g_module_symbol (loader_module->handle, func_name_file_save, &module_func_file_save);
	if (module_func_file_save != NULL) {
		PluginServiceFileSaverCallbacks *cbs;
		ServiceLoaderDataFileSaver *loader_data;

		cbs = plugin_service_get_cbs (service);
		cbs->plugin_func_file_save = gnm_plugin_loader_module_func_file_save;

		loader_data = g_new (ServiceLoaderDataFileSaver, 1);
		loader_data->module_func_file_save = module_func_file_save;
		g_object_set_data_full (
			G_OBJECT (service), "loader_data", loader_data, g_free);
	} else {
		*ret_error = error_info_new_printf (
		             _("Module file \"%s\" has invalid format."),
		             loader_module->module_file_name);
		error_info_add_details (*ret_error,
					error_info_new_printf (
					_("File doesn't contain \"%s\" function."),
					func_name_file_save));
	}
	g_free (func_name_file_save);
}

/*
 * Service - function_group
 */

typedef struct {
	GnmFuncDescriptor *module_fn_info_array;
	GHashTable *function_indices;
} ServiceLoaderDataFunctionGroup;

static void
function_group_loader_data_free (gpointer data)
{
	ServiceLoaderDataFunctionGroup *ld = data;

	g_hash_table_destroy (ld->function_indices);
	g_free (ld);
}

static gboolean
gnm_plugin_loader_module_func_desc_load (GnmPluginService *service,
					      char const *name,
					      GnmFuncDescriptor *res)
{
	ServiceLoaderDataFunctionGroup *loader_data;
	gpointer func_index_ptr;

	g_return_val_if_fail (IS_GNM_PLUGIN_SERVICE_FUNCTION_GROUP (service), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	loader_data = g_object_get_data (G_OBJECT (service), "loader_data");
	if (g_hash_table_lookup_extended (loader_data->function_indices, (gpointer) name,
	                                  NULL, &func_index_ptr)) {
		int i = GPOINTER_TO_INT (func_index_ptr);
		*res = loader_data->module_fn_info_array[i];
		return TRUE;
	}
	return FALSE;
}

static void
gnm_plugin_loader_module_load_service_function_group (GnmPluginLoader *loader,
                                                           GnmPluginService *service,
                                                           ErrorInfo **ret_error)
{
	GnmPluginLoaderModule *loader_module = GNM_PLUGIN_LOADER_MODULE (loader);
	gchar *fn_info_array_name;
	GnmFuncDescriptor *module_fn_info_array = NULL;

	g_return_if_fail (IS_GNM_PLUGIN_SERVICE_FUNCTION_GROUP (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	fn_info_array_name = g_strconcat (
		plugin_service_get_id (service), "_functions", NULL);
	g_module_symbol (loader_module->handle, fn_info_array_name, (gpointer) &module_fn_info_array);
	if (module_fn_info_array != NULL) {
		PluginServiceFunctionGroupCallbacks *cbs;
		ServiceLoaderDataFunctionGroup *loader_data;
		gint i;

		cbs = plugin_service_get_cbs (service);
		cbs->func_desc_load = &gnm_plugin_loader_module_func_desc_load;

		loader_data = g_new (ServiceLoaderDataFunctionGroup, 1);
		loader_data->module_fn_info_array = module_fn_info_array;
		loader_data->function_indices = g_hash_table_new (&g_str_hash, &g_str_equal);
		for (i = 0; module_fn_info_array[i].name != NULL; i++) {
			g_hash_table_insert (loader_data->function_indices,
			                     (gpointer) module_fn_info_array[i].name,
			                     GINT_TO_POINTER (i));
		}
		g_object_set_data_full (
			G_OBJECT (service), "loader_data", loader_data, function_group_loader_data_free);
	} else {
		*ret_error = error_info_new_printf (
		             _("Module file \"%s\" has invalid format."),
		             loader_module->module_file_name);
		error_info_add_details (*ret_error,
					error_info_new_printf (
					_("File doesn't contain \"%s\" array."),
					fn_info_array_name));
	}
	g_free (fn_info_array_name);
}
#endif // 0

/*
 * Service - plugin_loader
 */

typedef struct {
	GType (*module_func_get_loader_type) (ErrorInfo **ret_error);
} ServiceLoaderDataPluginLoader;

static GType
gnm_plugin_loader_module_func_get_loader_type (GnmPluginService *service,
                                                    ErrorInfo **ret_error)
{
	ServiceLoaderDataPluginLoader *loader_data;
	ErrorInfo *error = NULL;
	GType loader_type;

	g_return_val_if_fail (IS_GNM_PLUGIN_SERVICE_PLUGIN_LOADER (service), 0);

	GNM_INIT_RET_ERROR_INFO (ret_error);
	loader_data = g_object_get_data (G_OBJECT (service), "loader_data");
	loader_type = loader_data->module_func_get_loader_type (&error);
	if (error == NULL) {
		return loader_type;
	} else {
		*ret_error = error;
		return (GType) 0;
	}
}

static void
gnm_plugin_loader_module_load_service_plugin_loader (GnmPluginLoader *loader,
                                                          GnmPluginService *service,
                                                          ErrorInfo **ret_error)
{
	GnmPluginLoaderModule *loader_module = GNM_PLUGIN_LOADER_MODULE (loader);
	gchar *func_name_get_loader_type;
	gpointer module_func_get_loader_type = NULL;

	g_return_if_fail (IS_GNM_PLUGIN_SERVICE_PLUGIN_LOADER (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	func_name_get_loader_type = g_strconcat (
			plugin_service_get_id (service), "_get_loader_type", NULL);
	g_module_symbol (loader_module->handle, func_name_get_loader_type,
	                 &module_func_get_loader_type);
	if (module_func_get_loader_type != NULL) {
		PluginServicePluginLoaderCallbacks *cbs;
		ServiceLoaderDataPluginLoader *loader_data;

		cbs = plugin_service_get_cbs (service);
		cbs->plugin_func_get_loader_type = gnm_plugin_loader_module_func_get_loader_type;

		loader_data = g_new (ServiceLoaderDataPluginLoader, 1);
		loader_data->module_func_get_loader_type = module_func_get_loader_type;
		g_object_set_data_full (
			G_OBJECT (service), "loader_data", loader_data, g_free);
	} else
		*ret_error = error_info_new_printf (
			     _("Module doesn't contain \"%s\" function."),
			     func_name_get_loader_type);
	g_free (func_name_get_loader_type);
}

/*
 * Service - ui
 */

typedef struct {
	ModulePluginUIActions *module_ui_actions_array;
	GHashTable *ui_actions_hash;
} ServiceLoaderDataUI;

static void
ui_loader_data_free (gpointer data)
{
	ServiceLoaderDataUI *ld = data;

	g_hash_table_destroy (ld->ui_actions_hash);
	g_free (ld);
}

static void
gnm_plugin_loader_module_func_exec_action (GnmPluginService *service,
					   GnmAction const *action,
					   WorkbookControl *wbc,
					   ErrorInfo **ret_error)
{
	ServiceLoaderDataUI *loader_data;
	gpointer action_index_ptr;
	int action_index;

	g_return_if_fail (IS_GNM_PLUGIN_SERVICE_UI (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	loader_data = g_object_get_data (G_OBJECT (service), "loader_data");
	if (!g_hash_table_lookup_extended (loader_data->ui_actions_hash, action->id,
	                                   NULL, &action_index_ptr)) {
		*ret_error = error_info_new_printf (_("Unknown action: %s"), action->id);
		return;
	}
	action_index = GPOINTER_TO_INT (action_index_ptr);
	loader_data->module_ui_actions_array [action_index].handler (action, wbc);
}

static void
gnm_plugin_loader_module_load_service_ui (GnmPluginLoader *loader,
					  GnmPluginService *service,
					  ErrorInfo **ret_error)
{
	GnmPluginLoaderModule *loader_module = GNM_PLUGIN_LOADER_MODULE (loader);
	char *ui_actions_array_name;
	ModulePluginUIActions *module_ui_actions_array = NULL;
	PluginServiceUICallbacks *cbs;
	ServiceLoaderDataUI *loader_data;
	gint i;

	g_return_if_fail (IS_GNM_PLUGIN_SERVICE_UI (service));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	ui_actions_array_name = g_strconcat (
		plugin_service_get_id (service), "_ui_actions", NULL);
	g_module_symbol (loader_module->handle, ui_actions_array_name, (gpointer) &module_ui_actions_array);
	if (module_ui_actions_array == NULL) {
		*ret_error = error_info_new_printf (
			_("Module file \"%s\" has invalid format."),
			loader_module->module_file_name);
		error_info_add_details (*ret_error, error_info_new_printf (
			_("File doesn't contain \"%s\" array."), ui_actions_array_name));
		g_free (ui_actions_array_name);
		return;
	}
	g_free (ui_actions_array_name);

	cbs = plugin_service_get_cbs (service);
	cbs->plugin_func_exec_action = gnm_plugin_loader_module_func_exec_action;

	loader_data = g_new (ServiceLoaderDataUI, 1);
	loader_data->module_ui_actions_array = module_ui_actions_array;
	loader_data->ui_actions_hash = g_hash_table_new (g_str_hash, g_str_equal);
	for (i = 0; module_ui_actions_array[i].name != NULL; i++)
		g_hash_table_insert (loader_data->ui_actions_hash,
			(gpointer) module_ui_actions_array[i].name,
			GINT_TO_POINTER (i));
	g_object_set_data_full (G_OBJECT (service),
		"loader_data", loader_data, ui_loader_data_free);
}
