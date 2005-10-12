/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Support for dynamically-loaded Gnumeric plugin components.
 *
 * Authors:
 *  Old plugin engine:
 *    Tom Dyas (tdyas@romulus.rutgers.edu)
 *    Dom Lachowicz (dominicl@seas.upenn.edu)
 *  New plugin engine:
 *    Zbigniew Chyla (cyba@gnome.pl)
 */

#include <config.h>
#include <glib/gi18n.h>
#include "gnumeric.h"
#include "plugin.h"

#include "gui-util.h"
#include "gutils.h"
#include "command-context.h"
#include "file.h"
//#include "workbook.h"
//#include "workbook-view.h"
#include "error-info.h"
#include "plugin-loader.h"
#include "plugin-loader-module.h"
#include "plugin-service.h"
#include "xml-io.h"
#include "gnumeric-gconf.h"

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <locale.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <gmodule.h>
#include <application.h>

#include <libxml/parser.h>
#include <libxml/parserInternals.h>
#include <libxml/xmlmemory.h>
#include <gsf/gsf-impl-utils.h>

#include <glib-object.h>


#define PLUGIN_INFO_FILE_NAME          "plugin.xml"
#define PLUGIN_ID_VALID_CHARS          "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

#define BUILTIN_LOADER_MODULE_ID       "Gnumeric_Builtin:module"


static GHashTable *plugins_marked_for_deactivation_hash = NULL;
static GSList *available_plugins = NULL;
static GHashTable *available_plugins_id_hash = NULL;

static GHashTable *loader_services = NULL;


static void plugin_get_loader_if_needed (GnmPlugin *pinfo, ErrorInfo **ret_error);
static void plugin_info_read (GnmPlugin *pinfo, const gchar *dir_name, ErrorInfo **ret_error);
static void gnm_plugin_load_base (GnmPlugin *plugin, ErrorInfo **ret_error);

/*
 * GnmPlugin
 */

typedef struct {
	gchar *plugin_id;
	GnmPlugin *plugin;             /* don't use directly */
	gboolean force_load;
} PluginDependency;

struct _GnmPlugin {
	GTypeModule parent_instance;

	gboolean has_full_info;
	gchar   *dir_name;
	gchar   *id;

	gchar   *name;
	gchar   *description;
	gboolean require_explicit_enabling;

	gboolean is_active;
	gint use_refcount;
	GSList *dependencies;
	gchar *loader_id;
	GHashTable *loader_attrs;
	GnmPluginLoader *loader;
	GSList *services;

	char *saved_textdomain;
};

typedef struct _GnmPluginClass GnmPluginClass;
struct _GnmPluginClass  {
	GTypeModuleClass parent_class;

	/* signals */
	void (*state_changed) (GnmPluginClass *gpc);
	void (*can_deactivate_changed) (GnmPluginClass *gpc);
};

enum {
	STATE_CHANGED,
	CAN_DEACTIVATE_CHANGED,
	LAST_SIGNAL
};

static guint gnm_plugin_signals[LAST_SIGNAL];
static GObjectClass *parent_class = NULL;

static void plugin_dependency_free (gpointer data);

static void
gnm_plugin_init (GObject *obj)
{
	GnmPlugin *plugin = GNM_PLUGIN (obj);

	plugin->id = NULL;
	plugin->dir_name = NULL;
	plugin->has_full_info = FALSE;
	plugin->saved_textdomain = NULL;
	plugin->require_explicit_enabling = FALSE;
}

static void
gnm_plugin_finalize (GObject *obj)
{
	GnmPlugin *plugin = GNM_PLUGIN (obj);

	g_free (plugin->id);
	plugin->id = NULL;
	g_free (plugin->dir_name);
	plugin->dir_name = NULL;
	if (plugin->has_full_info) {
		plugin->has_full_info = FALSE;
		g_free (plugin->name);
		g_free (plugin->description);
		gnm_slist_free_custom (plugin->dependencies, plugin_dependency_free);
		g_free (plugin->loader_id);
		if (plugin->loader_attrs != NULL) {
			g_hash_table_destroy (plugin->loader_attrs);
		}
		if (plugin->loader != NULL) {
			g_object_unref (plugin->loader);
		}
		gnm_slist_free_custom (plugin->services, g_object_unref);
	}
	g_free (plugin->saved_textdomain);
	plugin->saved_textdomain = NULL;

	parent_class->finalize (obj);
}

static gboolean
gnm_plugin_type_module_load (GTypeModule *module)
{
	GnmPlugin *plugin = GNM_PLUGIN (module);
	ErrorInfo *ignored_error;

	g_return_val_if_fail (plugin->is_active, FALSE);

	gnm_plugin_load_base (plugin, &ignored_error);
	if (ignored_error != NULL) {
		error_info_print (ignored_error);
		error_info_free (ignored_error);
		return FALSE;
	}
	gnm_plugin_use_ref (plugin);
	return TRUE;
}

static void
gnm_plugin_type_module_unload (GTypeModule *module)
{
	GnmPlugin *plugin = GNM_PLUGIN (module);

	g_return_if_fail (plugin->is_active);

	gnm_plugin_use_unref (plugin);
}

static void
gnm_plugin_class_init (GObjectClass *gobject_class)
{
	GTypeModuleClass *type_module_class = G_TYPE_MODULE_CLASS (gobject_class);

	parent_class = g_type_class_peek_parent (gobject_class);

	gobject_class->finalize = gnm_plugin_finalize;

	type_module_class->load = gnm_plugin_type_module_load;
	type_module_class->unload = gnm_plugin_type_module_unload;

	gnm_plugin_signals[STATE_CHANGED] = g_signal_new (
		"state_changed",
		G_TYPE_FROM_CLASS (gobject_class),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GnmPluginClass, state_changed),
		NULL, NULL,
		g_cclosure_marshal_VOID__VOID,
		G_TYPE_NONE, 0);
	gnm_plugin_signals[CAN_DEACTIVATE_CHANGED] = g_signal_new (
		"can_deactivate_changed",
		G_TYPE_FROM_CLASS (gobject_class),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GnmPluginClass, can_deactivate_changed),
		NULL, NULL,
		g_cclosure_marshal_VOID__VOID,
		G_TYPE_NONE, 0);
}

GSF_CLASS (GnmPlugin, gnm_plugin, gnm_plugin_class_init, gnm_plugin_init,
           G_TYPE_TYPE_MODULE)

static GnmPlugin *
plugin_info_new_from_xml (const gchar *dir_name, ErrorInfo **ret_error)
{
	GnmPlugin *plugin;
	ErrorInfo *error;

	GNM_INIT_RET_ERROR_INFO (ret_error);
	plugin = g_object_new (GNM_PLUGIN_TYPE, NULL);
	plugin_info_read (plugin, dir_name, &error);
	if (error == NULL) {
		plugin->has_full_info = TRUE;
	} else {
		*ret_error = error;
		g_object_unref (plugin);
		plugin = NULL;
	}

	return plugin;
}

static GnmPlugin *
plugin_info_new_with_id_and_dir_name_only (const gchar *id, const gchar *dir_name)
{
	GnmPlugin *plugin;

	plugin = g_object_new (GNM_PLUGIN_TYPE, NULL);
	g_type_module_set_name (G_TYPE_MODULE (plugin), id);
	plugin->id = g_strdup (id);
	plugin->dir_name = g_strdup (dir_name);
	plugin->has_full_info = FALSE;

	return plugin;
}


/*
 * PluginFileState - information about plugin.xml files used in previous
 *                   and current Gnumeric session.
 */

typedef struct {
	gchar *dir_name;
	gchar *file_state;
	gchar *plugin_id;
	enum {PLUGIN_OLD_UNUSED, PLUGIN_OLD_USED, PLUGIN_NEW} age;
} PluginFileState;

static gboolean plugin_file_state_hash_changed;
static GHashTable *plugin_file_state_dir_hash;

static gchar *
get_file_state_as_string (const gchar *file_name)
{
	struct stat st;

	if (stat (file_name, &st) == -1) {
		return NULL;
	}

	return g_strdup_printf (
		"%ld:%ld:%ld:%ld",
		(long int) st.st_dev, (long int) st.st_ino,
		(long int) st.st_size, (long int) st.st_mtime);
}

static gchar *
plugin_file_state_as_string (PluginFileState *state)
{
	return g_strdup_printf ("%s|%s|%s", state->plugin_id, state->file_state,
	                        state->dir_name);
}

static PluginFileState *
plugin_file_state_from_string (const gchar *str)
{
	PluginFileState *state;
	gchar **strv;

	strv = g_strsplit (str, "|", 3);
	if (strv[0] == NULL || strv[1] == NULL || strv[2] == NULL) {
		g_strfreev (strv);
		return NULL;
	}
	state = g_new (PluginFileState, 1);
	state->plugin_id = strv[0];
	state->file_state = strv[1];
	state->dir_name = strv[2];
	state->age = PLUGIN_OLD_UNUSED;
	g_free (strv);

	return state;
}

static void
plugin_file_state_free (gpointer data)
{
	PluginFileState *state = data;

	g_free (state->dir_name);
	g_free (state->file_state);
	g_free (state->plugin_id);
	g_free (state);
}

/* --- */

static gboolean
plugin_info_read_full_info_if_needed_error_info (GnmPlugin *pinfo, ErrorInfo **ret_error)
{
	ErrorInfo *read_error;
	gchar *old_id, *old_dir_name;

	GNM_INIT_RET_ERROR_INFO (ret_error);
	if (pinfo->has_full_info) {
		return TRUE;
	}

	old_id = pinfo->id;
	old_dir_name = pinfo->dir_name;
	plugin_info_read (pinfo, old_dir_name, &read_error);
	if (read_error == NULL && strcmp (pinfo->id, old_id) == 0) {
		/* id and dir_name pointers are guaranteed to be valid during plugin's lifetime */
		g_free (pinfo->id);
		g_free (pinfo->dir_name);
		pinfo->id = old_id;
		pinfo->dir_name = old_dir_name;
		pinfo->has_full_info = TRUE;
	} else {
		plugin_message (1, "Can't read plugin.xml file for %s.\n", old_id);
		if (read_error == NULL) {
			read_error = error_info_new_printf (
			             _("File contains plugin info with invalid id (%s), expected %s."),
			             pinfo->id, old_id);
		}
		*ret_error = error_info_new_str_with_details (
		             _("Couldn't read plugin info from file."),
		             read_error);
		g_free (old_id);
		g_free (old_dir_name);
	}

	return *ret_error == NULL;
}

static gboolean
plugin_info_read_full_info_if_needed (GnmPlugin *pinfo)
{
	ErrorInfo *error;

	if (plugin_info_read_full_info_if_needed_error_info (pinfo, &error)) {
		return TRUE;
	} else {
		g_warning ("plugin_info_read_full_info_if_needed: couldn't read plugin info from file.");
		error_info_print (error);
		error_info_free (error);
		return FALSE;
	}
}

/*
 * Accessor functions
 */

/**
 * gnm_plugin_get_textdomain:
 * @plugin      : The plugin
 *
 * Returns plugin's textdomain for use with textdomain(3) and d*gettext(3)
 * functions.
 */
const gchar *
gnm_plugin_get_textdomain (GnmPlugin *plugin)
{
	g_return_val_if_fail (IS_GNM_PLUGIN (plugin), NULL);

	if (plugin->saved_textdomain == NULL) {
		plugin->saved_textdomain = g_strconcat ("gnumeric__", plugin->id, NULL);
	}

	return plugin->saved_textdomain;
}

/**
 * gnm_plugin_is_active:
 * @pinfo      : The plugin
 *
 * Returns : TRUE if @plugin is active and FALSE otherwise.
 */
gboolean
gnm_plugin_is_active (GnmPlugin *plugin)
{
	g_return_val_if_fail (IS_GNM_PLUGIN (plugin), FALSE);

	if (!plugin->has_full_info) {
		return FALSE;
	}
	return plugin->is_active;
}

/**
 * gnm_plugin_get_dir_name:
 * @plugin      : The plugin
 *
 * Returns the name of the directory in which @plugin is located.
 * Returned string is != NULL and stays valid during @plugin's lifetime.
 */
const gchar *
gnm_plugin_get_dir_name (GnmPlugin *pinfo)
{
	g_return_val_if_fail (IS_GNM_PLUGIN (pinfo), NULL);

	return pinfo->dir_name;
}

/**
 * gnm_plugin_get_id:
 * @plugin      : The plugin
 *
 * Returns the ID of @plugin (unique string used for idenfification of
 * plugin).
 * Returned string is != NULL and stays valid during @plugin's lifetime.
 */
const gchar *
gnm_plugin_get_id (GnmPlugin *pinfo)
{
	g_return_val_if_fail (IS_GNM_PLUGIN (pinfo), NULL);

	return pinfo->id;
}

/**
 * gnm_plugin_get_name:
 * @plugin      : The plugin
 *
 * Returns textual name of @plugin. If the real name is not available
 * for some reason, automatically generated string will be returned.
 * Returned string is != NULL and stays valid during @plugin's lifetime.
 */
const gchar *
gnm_plugin_get_name (GnmPlugin *pinfo)
{
	g_return_val_if_fail (IS_GNM_PLUGIN (pinfo), NULL);

	if (!plugin_info_read_full_info_if_needed (pinfo)) {
		return _("Unknown name");
	}
	return pinfo->name;
}

/**
 * gnm_plugin_get_description:
 * @plugin      : The plugin
 *
 * Returns textual description of @plugin or NULL if description is not
 * available.
 * Returned string stays valid during @plugin's lifetime.
 */
const gchar *
gnm_plugin_get_description (GnmPlugin *pinfo)
{
	g_return_val_if_fail (IS_GNM_PLUGIN (pinfo), NULL);

	if (!plugin_info_read_full_info_if_needed (pinfo)) {
		return NULL;
	}
	return pinfo->description;
}

/**
 * gnm_plugin_is_loaded:
 * @pinfo      : The plugin
 *
 * Returns : TRUE if @plugin is loaded and FALSE otherwise.
 */
gboolean
gnm_plugin_is_loaded (GnmPlugin *pinfo)
{
	g_return_val_if_fail (IS_GNM_PLUGIN (pinfo), FALSE);

	if (!pinfo->has_full_info) {
		return FALSE;
	}
	return pinfo->loader != NULL &&
	       gnm_plugin_loader_is_base_loaded (pinfo->loader);
}

/* - */

/**
 * plugins_register_loader:
 * @loader_id     : Loader's id
 * @service       : Plugin service of type "plugin_loader"
 *
 * Registers new type of plugin loader identified by @loader_id (identifier
 * consists of loader's plugin id and service id concatenated using colon).
 * All requests to create new loader object of this type will be passed to
 * @service.
 * This function is intended for use by GnmPluginService objects.
 */
void
plugins_register_loader (const gchar *loader_id, GnmPluginService *service)
{
	g_return_if_fail (loader_id != NULL);
	g_return_if_fail (service != NULL);

	g_hash_table_insert (loader_services, g_strdup (loader_id), service);
}

/**
 * plugins_unregister_loader:
 * @loader_id     : Loader's id
 *
 * Unregisters a type of plugin loader identified by @loader_id. After
 * callingthis function Gnumeric will be unable to load plugins supported
 * by the specified loader.
 * This function is intended for use by GnmPluginService objects.
 */
void
plugins_unregister_loader (const gchar *loader_id)
{
	g_return_if_fail (loader_id != NULL);

	g_hash_table_remove (loader_services, loader_id);
}

static GType
get_loader_type_by_id (const gchar *id_str, ErrorInfo **ret_error)
{
	GnmPluginService *loader_service;
	ErrorInfo *error;
	GType loader_type;

	g_return_val_if_fail (id_str != NULL, G_TYPE_NONE);

	GNM_INIT_RET_ERROR_INFO (ret_error);
	if (strcmp (id_str, BUILTIN_LOADER_MODULE_ID) == 0) {
		return TYPE_GNM_PLUGIN_LOADER_MODULE;
	}
	loader_service = g_hash_table_lookup (loader_services, id_str);
	if (loader_service == NULL) {
		*ret_error = error_info_new_printf (
		             _("Unsupported loader type \"%s\"."),
		             id_str);
		return G_TYPE_NONE;
	}
	loader_type = plugin_service_plugin_loader_generate_type (
	              loader_service, &error);
	if (error != NULL) {
		*ret_error = error_info_new_printf (
		             _("Error while preparing loader \"%s\"."),
		             id_str);
		error_info_add_details (*ret_error, error);
		return G_TYPE_NONE;
	}

	return loader_type;
}

static GnmPlugin *
plugin_dependency_get_plugin (PluginDependency *dep)
{
	g_return_val_if_fail (dep != NULL, NULL);

	if (dep->plugin == NULL)
		dep->plugin = plugins_get_plugin_by_id (dep->plugin_id);
	return dep->plugin;
}

static GSList *
plugin_info_read_dependency_list (xmlNode *tree)
{
	GSList *dependency_list = NULL;
	xmlNode *node;

	g_return_val_if_fail (tree != NULL, NULL);
	g_return_val_if_fail (strcmp (tree->name, "dependencies") == 0, NULL);

	for (node = tree->xmlChildrenNode; node != NULL; node = node->next) {
		if (strcmp (node->name, "dep_plugin") == 0) {
			gchar *plugin_id;

			plugin_id = xmlGetProp (node, (xmlChar *)"id");
			if (plugin_id != NULL) {
				PluginDependency *dep;

				dep = g_new (PluginDependency, 1);
				dep->plugin_id = plugin_id;
				dep->plugin = NULL;
				if (!xml_node_get_bool (node, "force_load", &(dep->force_load)))
					dep->force_load = FALSE;
				GNM_SLIST_PREPEND (dependency_list, dep);
			}
		}
	}

	return g_slist_reverse (dependency_list);
}

static GSList *
plugin_info_read_service_list (GnmPlugin *plugin, xmlNode *tree, ErrorInfo **ret_error)
{
	GSList *service_list = NULL;
	GSList *error_list = NULL;
	xmlNode *node;
	gint i;

	g_return_val_if_fail (tree != NULL, NULL);

	node = e_xml_get_child_by_name (tree, (xmlChar *)"services");
	if (node == NULL)
		return NULL;
	node = node->xmlChildrenNode;
	for (i = 0; node != NULL; i++, node = node->next) {
		if (strcmp (node->name, "service") == 0) {
			GnmPluginService *service;
			ErrorInfo *service_error;

			service = plugin_service_new (plugin, node, &service_error);

			if (service != NULL) {
				g_assert (service_error == NULL);
				GNM_SLIST_PREPEND (service_list, service);
			} else {
				ErrorInfo *error;

				error = error_info_new_printf (
				        _("Error while reading service #%d info."),
				        i);
				error_info_add_details (error, service_error);
				GNM_SLIST_PREPEND (error_list, error);
			}
		}
	}
	if (error_list != NULL) {
		GNM_SLIST_REVERSE (error_list);
		*ret_error = error_info_new_from_error_list (error_list);
		gnm_slist_free_custom (service_list, g_object_unref);
		return NULL;
	} else {
		return g_slist_reverse (service_list);
	}
}

static GHashTable *
plugin_info_read_loader_attrs (xmlNode *tree)
{
	xmlNode *node;
	GHashTable *hash;

	g_return_val_if_fail (tree != NULL, NULL);
	g_return_val_if_fail (strcmp (tree->name, "loader") == 0, NULL);

	hash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
	for (node = tree->xmlChildrenNode; node != NULL; node = node->next) {
		if (strcmp (node->name, "attribute") == 0) {
			gchar *name, *value;

			name = xmlGetProp (node, (xmlChar *)"name");
			if (name != NULL) {
				if (g_hash_table_lookup (hash, name) == NULL) {
					value = xmlGetProp (node, (xmlChar *)"value");
					g_hash_table_insert (hash, name, value);
				} else {
					g_warning ("Duplicated \"%s\" attribute in plugin.xml file.", name);
					g_free (name);
				}
			}
		}
	}

	return hash;
}

static void
plugin_dependency_free (gpointer data)
{
	PluginDependency *dep = data;

	g_return_if_fail (dep != NULL);

	g_free (dep->plugin_id);
	g_free (dep);
}

static void
plugin_info_read (GnmPlugin *plugin, const gchar *dir_name, ErrorInfo **ret_error)
{
	gchar *file_name;
	xmlDocPtr doc;
	gchar *id, *name, *description;
	xmlNode *tree, *information_node, *dependencies_node, *loader_node;
	GSList *dependency_list;
	gchar *loader_id;
	GHashTable *loader_attrs;
	gboolean require_explicit_enabling = FALSE;

	g_return_if_fail (IS_GNM_PLUGIN (plugin));
	g_return_if_fail (dir_name != NULL);

	GNM_INIT_RET_ERROR_INFO (ret_error);
	file_name = g_build_filename (dir_name, PLUGIN_INFO_FILE_NAME, NULL);
	doc = xmlParseFile (file_name);
	if (doc == NULL || doc->xmlRootNode == NULL || strcmp (doc->xmlRootNode->name, "plugin") != 0) {
		if (access (file_name, R_OK) != 0) {
			*ret_error = error_info_new_printf (
			             _("Can't read plugin info file (\"%s\")."),
			             file_name);
		} else {
			*ret_error = error_info_new_printf (
			             _("File \"%s\" is not valid plugin info file."),
			             file_name);
		}
		g_free (file_name);
		xmlFreeDoc (doc);
		return;
	}
	tree = doc->xmlRootNode;
	id = xmlGetProp (tree, (xmlChar *)"id");
	information_node = e_xml_get_child_by_name (tree, (xmlChar *)"information");
	if (information_node != NULL) {
		xmlNode *node;
		xmlChar *val;

		node = e_xml_get_child_by_name_by_lang (information_node, "name");
		if (node != NULL) {
			val = xmlNodeGetContent (node);
			name = g_strdup ((gchar *)val);
			xmlFree (val);
		} else
			name = NULL;

		node = e_xml_get_child_by_name_by_lang (information_node, "description");
		if (node != NULL) {
			val = xmlNodeGetContent (node);
			description = g_strdup ((gchar *)val);
			xmlFree (val);
		} else
			description = NULL;
		if (e_xml_get_child_by_name (information_node, (xmlChar const *)"require_explicit_enabling"))
			require_explicit_enabling = TRUE;
	} else {
		name = NULL;
		description = NULL;
	}
	dependencies_node = e_xml_get_child_by_name (tree, (xmlChar *)"dependencies");
	if (dependencies_node != NULL) {
		dependency_list = plugin_info_read_dependency_list (dependencies_node);
	} else {
		dependency_list = NULL;
	}
	loader_node = e_xml_get_child_by_name (tree, (xmlChar *)"loader");
	if (loader_node != NULL) {
		char *p;

		loader_id = xmlGetProp (loader_node, (xmlChar *)"type");
		if (loader_id != NULL && (p = strchr (loader_id, ':')) != NULL) {
			loader_attrs = plugin_info_read_loader_attrs (loader_node);
			if (strcmp (loader_id, BUILTIN_LOADER_MODULE_ID) != 0) {
				PluginDependency *dep;

				/* Add loader's plugin to the list of dependencies */
				dep = g_new (PluginDependency, 1);
				dep->plugin_id = g_strndup (loader_id, p - loader_id);
				dep->plugin = NULL;
				dep->force_load = FALSE;
				GNM_SLIST_PREPEND (dependency_list, dep);
			}
		} else {
			loader_id = NULL;
			loader_attrs = NULL;
		}
	} else {
		loader_id = NULL;
		loader_attrs = NULL;
	}
	if (id != NULL && name != NULL && loader_id != NULL &&
	    id[strspn (id, PLUGIN_ID_VALID_CHARS)] == '\0') {
		ErrorInfo *services_error = NULL;

		g_type_module_set_name (G_TYPE_MODULE (plugin), id);
		plugin->dir_name = g_strdup (dir_name);
		plugin->id = id;
		plugin->name = name;
		plugin->description = description;
		plugin->require_explicit_enabling = require_explicit_enabling;
		plugin->is_active = FALSE;
		plugin->use_refcount = 0;
		plugin->dependencies = dependency_list;
		plugin->loader_id = loader_id;
		plugin->loader_attrs = loader_attrs;
		plugin->loader = NULL;
		plugin->services = plugin_info_read_service_list (plugin, tree, &services_error);

		if (services_error != NULL) {
			*ret_error = error_info_new_printf (
				_("Errors while reading services for plugin with id=\"%s\"."),
				id);
			error_info_add_details (*ret_error, services_error);
		} else if (plugin->services == NULL)
			*ret_error = error_info_new_printf (
				_("No services defined for plugin with id=\"%s\"."),
				id);
		else
			plugin_message (4, "Read plugin.xml file for %s.\n", plugin->id);
	} else {
		if (id != NULL) {
			GSList *error_list = NULL;

			if (id[strspn (id, PLUGIN_ID_VALID_CHARS)] != '\0') {
				GNM_SLIST_PREPEND (error_list, error_info_new_printf (
					_("Plugin id contains invalid characters (%s)."), id));
			}
			if (name == NULL) {
				GNM_SLIST_PREPEND (error_list, error_info_new_str (
					_("Unknown plugin name.")));
			}
			if (loader_id == NULL) {
				GNM_SLIST_PREPEND (error_list, error_info_new_printf (
					_("No loader defined or loader id invalid for plugin with id=\"%s\"."), id));
			}
			g_assert (error_list != NULL);
			GNM_SLIST_REVERSE (error_list);
			*ret_error = error_info_new_from_error_list (error_list);
		} else
			*ret_error = error_info_new_str (_("Plugin has no id."));

		gnm_slist_free_custom (dependency_list, plugin_dependency_free);
		g_free (plugin->loader_id);
		if (plugin->loader_attrs != NULL)
			g_hash_table_destroy (plugin->loader_attrs);
		g_free (id);
		g_free (name);
		g_free (description);
	}
	g_free (file_name);
	xmlFreeDoc (doc);
}

static void
plugin_get_loader_if_needed (GnmPlugin *pinfo, ErrorInfo **ret_error)
{
	GType loader_type;
	ErrorInfo *error = NULL;

	g_return_if_fail (IS_GNM_PLUGIN (pinfo));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	if (!plugin_info_read_full_info_if_needed_error_info (pinfo, ret_error)) {
		return;
	}
	if (pinfo->loader != NULL) {
		return;
	}
	loader_type = get_loader_type_by_id (pinfo->loader_id, &error);
	if (error == NULL) {
		GnmPluginLoader *loader;
		ErrorInfo *error;

		loader = GNM_PLUGIN_LOADER (g_object_new (loader_type, NULL));
		gnm_plugin_loader_set_attributes (loader, pinfo->loader_attrs, &error);
		if (error == NULL) {
			pinfo->loader = loader;
			gnm_plugin_loader_set_plugin (loader, pinfo);
		} else {
			g_object_unref (loader);
			loader = NULL;
			*ret_error = error_info_new_printf (
			             _("Error initializing plugin loader (\"%s\")."),
			             pinfo->loader_id);
			error_info_add_details (*ret_error, error);
		}
	} else {
		*ret_error = error;
	}
}

/**
 * gnm_plugin_activate:
 * @plugin      : The plugin
 * @ret_error   : Pointer used to report errors
 *
 * Activates @plugin together with all its dependencies.
 * In case of error the plugin won't be activated and detailed error
 * information will be returned using @ret_error.
 */
void
gnm_plugin_activate (GnmPlugin *pinfo, ErrorInfo **ret_error)
{
	GSList *error_list = NULL;
	GSList *l;
	gint i;
	static GSList *activate_stack = NULL;

	g_return_if_fail (IS_GNM_PLUGIN (pinfo));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	if (g_slist_find (activate_stack, pinfo) != NULL) {
		*ret_error = error_info_new_str (
				     _("Detected cyclic plugin dependencies."));
		return;
	}
	if (!plugin_info_read_full_info_if_needed_error_info (pinfo, ret_error)) {
		return;
	}
	if (pinfo->is_active) {
		return;
	}

	/* Activate plugin dependencies */
	GNM_SLIST_PREPEND (activate_stack, pinfo);
	GNM_SLIST_FOREACH (pinfo->dependencies, PluginDependency, dep,
		GnmPlugin *dep_plugin;

		dep_plugin = plugin_dependency_get_plugin (dep);
		if (dep_plugin != NULL) {
			ErrorInfo *dep_error;

			gnm_plugin_activate (dep_plugin, &dep_error);
			if (dep_error != NULL) {
				ErrorInfo *new_error;

				new_error = error_info_new_printf (
					_("Couldn't activate plugin with id=\"%s\"."), dep->plugin_id);
				error_info_add_details (new_error, dep_error);
				GNM_SLIST_PREPEND (error_list, new_error);
			}
		} else {
			GNM_SLIST_PREPEND (error_list, error_info_new_printf (
				_("Couldn't find plugin with id=\"%s\"."), dep->plugin_id));
		}
	);
	g_assert (activate_stack != NULL && activate_stack->data == pinfo);
	activate_stack = g_slist_delete_link (activate_stack, activate_stack);
	if (error_list != NULL) {
		*ret_error = error_info_new_str (
				     _("Error while activating plugin dependencies."));
		error_info_add_details_list (*ret_error, error_list);
		return;
	}

	for (l = pinfo->services, i = 0; l != NULL; l = l->next, i++) {
		GnmPluginService *service = l->data;
		ErrorInfo *service_error;

		plugin_service_activate (service, &service_error);
		if (service_error != NULL) {
			ErrorInfo *error;

			error = error_info_new_printf (
				_("Error while activating plugin service #%d."), i);
			error_info_add_details (error, service_error);
			GNM_SLIST_PREPEND (error_list, error);
		}
	}
	if (error_list != NULL) {
		*ret_error = error_info_new_from_error_list (error_list);
		/* FIXME - deactivate activated services */
		return;
	}
	GNM_SLIST_FOREACH (pinfo->dependencies, PluginDependency, dep,
		gnm_plugin_use_ref (plugin_dependency_get_plugin (dep));
	);
	pinfo->is_active = TRUE;
	g_signal_emit (G_OBJECT (pinfo), gnm_plugin_signals[STATE_CHANGED], 0);
}

/**
 * gnm_plugin_deactivate:
 * @plugin      : The plugin
 * @ret_error   : Pointer used to report errors
 *
 * Dectivates @plugin. Its dependencies will NOT be automatically
 * deactivated.
 * In case of error the plugin won't be deactivated and detailed error
 * information will be returned using @ret_error.
 */
void
gnm_plugin_deactivate (GnmPlugin *pinfo, ErrorInfo **ret_error)
{
	GSList *error_list = NULL;
	GSList *l;
	gint i;

	g_return_if_fail (IS_GNM_PLUGIN (pinfo));

	GNM_INIT_RET_ERROR_INFO (ret_error);
	if (!pinfo->has_full_info || !pinfo->is_active) {
		return;
	}
	if (pinfo->use_refcount > 0) {
		*ret_error = error_info_new_str ("Plugin is still in use.");
		return;
	}
	for (l = pinfo->services, i = 0; l != NULL; l = l->next, i++) {
		GnmPluginService *service = l->data;
		ErrorInfo *service_error;

		plugin_service_deactivate (service, &service_error);
		if (service_error != NULL) {
			ErrorInfo *error;

			error = error_info_new_printf (
				_("Error while deactivating plugin service #%d."), i);
			error_info_add_details (error, service_error);
			GNM_SLIST_PREPEND (error_list, error);
		}
	}
	if (error_list != NULL) {
		*ret_error = error_info_new_from_error_list (error_list);
		/* FIXME - some services are still active (or broken) */
	} else {
		pinfo->is_active = FALSE;
		GNM_SLIST_FOREACH (pinfo->dependencies, PluginDependency, dep,
			gnm_plugin_use_unref (plugin_dependency_get_plugin (dep));
		);
		if (pinfo->loader != NULL) {
			g_object_unref (pinfo->loader);
			pinfo->loader = NULL;
		}
	}
	g_signal_emit (G_OBJECT (pinfo), gnm_plugin_signals[STATE_CHANGED], 0);
}

/**
 * gnm_plugin_can_deactivate:
 * @pinfo       : The plugin
 *
 * Tells if the plugin can be deactivated using gnm_plugin_deactivate.
 *
 * Returns : TRUE if @plugin can be deactivated and FALSE otherwise.
 */
gboolean
gnm_plugin_can_deactivate (GnmPlugin *pinfo)
{
	g_return_val_if_fail (IS_GNM_PLUGIN (pinfo), FALSE);

	if (!pinfo->is_active) {
		return FALSE;
	}
	if (!plugin_info_read_full_info_if_needed (pinfo)) {
		return FALSE;
	}
	return pinfo->use_refcount == 0;
}

static void
gnm_plugin_load_base (GnmPlugin *plugin, ErrorInfo **ret_error)
{
	ErrorInfo *error;
	GSList *error_list = NULL;
	static GSList *load_stack = NULL;

	GNM_INIT_RET_ERROR_INFO (ret_error);
	if (g_slist_find (load_stack, plugin) != NULL) {
		*ret_error = error_info_new_str (
				     _("Detected cyclic plugin dependencies."));
		return;
	}
	if (gnm_plugin_is_loaded (plugin)) {
		return;
	}
	if (!plugin_info_read_full_info_if_needed_error_info (plugin, ret_error)) {
		return;
	}
	plugin_get_loader_if_needed (plugin, &error);
	if (error != NULL) {
		*ret_error = error_info_new_str_with_details (
		             _("Cannot load plugin loader."),
		             error);
		return;
	}

	/* Load plugin dependencies */
	GNM_SLIST_PREPEND (load_stack, plugin);
	GNM_SLIST_FOREACH (plugin->dependencies, PluginDependency, dep,
		GnmPlugin *dep_plugin;
		ErrorInfo *dep_error;

		if (!dep->force_load) {
			continue;
		}
		dep_plugin = plugin_dependency_get_plugin (dep);
		if (dep_plugin != NULL) {
			plugin_get_loader_if_needed (dep_plugin, &dep_error);
			if (dep_error == NULL) {
				gnm_plugin_load_base (dep_plugin, &dep_error);
			} else {
				dep_error = error_info_new_str_with_details (
				             _("Cannot load plugin loader."),
				             dep_error);
			}
			if (dep_error != NULL) {
				ErrorInfo *new_error;

				new_error = error_info_new_printf (
					_("Couldn't load plugin with id=\"%s\"."), dep->plugin_id);
				error_info_add_details (new_error, dep_error);
				GNM_SLIST_PREPEND (error_list, new_error);
			}
		} else {
			GNM_SLIST_PREPEND (error_list, error_info_new_printf (
				_("Couldn't find plugin with id=\"%s\"."), dep->plugin_id));
		}
	);
	g_assert (load_stack != NULL && load_stack->data == plugin);
	load_stack = g_slist_delete_link (load_stack, load_stack);
	if (error_list != NULL) {
		*ret_error = error_info_new_str (
				     _("Error while loading plugin dependencies."));
		error_info_add_details_list (*ret_error, error_list);
		return;
	}

	gnm_plugin_loader_load_base (plugin->loader, &error);
	if (error != NULL) {
		*ret_error = error;
		return;
	}
	g_signal_emit (G_OBJECT (plugin), gnm_plugin_signals[STATE_CHANGED], 0);
}

/**
 * gnm_plugin_load_service:
 * @pinfo       : The plugin
 * @service     : Plugin service
 * @ret_error   : Pointer used to report errors
 *
 * Loads base part of the plugin if is not loaded and then loads given
 * plugin service (prepares necessary part of the plugin for direct use).
 * This function is intended for use by GnmPluginService objects.
 */
void
gnm_plugin_load_service (GnmPlugin *pinfo, GnmPluginService *service, ErrorInfo **ret_error)
{
	g_return_if_fail (IS_GNM_PLUGIN (pinfo));
	g_return_if_fail (service != NULL);

	GNM_INIT_RET_ERROR_INFO (ret_error);
	gnm_plugin_load_base (pinfo, ret_error);
	if (*ret_error != NULL) {
		return;
	}
	gnm_plugin_loader_load_service (pinfo->loader, service, ret_error);
}

/**
 * gnm_plugin_unload_service:
 * @pinfo       : The plugin
 * @service     : Plugin service
 * @ret_error   : Pointer used to report errors
 *
 * ...
 * This function is intended for use by GnmPluginService objects.
 */
void
gnm_plugin_unload_service (GnmPlugin *pinfo, GnmPluginService *service, ErrorInfo **ret_error)
{
	g_return_if_fail (IS_GNM_PLUGIN (pinfo));
	g_return_if_fail (pinfo->loader != NULL);
	g_return_if_fail (service != NULL);

	GNM_INIT_RET_ERROR_INFO (ret_error);
	if (!plugin_info_read_full_info_if_needed_error_info (pinfo, ret_error)) {
		return;
	}
	gnm_plugin_loader_unload_service (pinfo->loader, service, ret_error);
}

/**
 * gnm_plugin_use_ref:
 * @plugin      : The plugin
 */
void
gnm_plugin_use_ref (GnmPlugin *plugin)
{
	g_return_if_fail (IS_GNM_PLUGIN (plugin));
	g_return_if_fail (plugin->is_active);

	plugin->use_refcount++;
	if (plugin->use_refcount == 1) {
		g_signal_emit (G_OBJECT (plugin), gnm_plugin_signals[CAN_DEACTIVATE_CHANGED], 0);
	}
}

/**
 * gnm_plugin_use_unref:
 * @plugin      : The plugin
 */
void
gnm_plugin_use_unref (GnmPlugin *plugin)
{
	g_return_if_fail (IS_GNM_PLUGIN (plugin));
	g_return_if_fail (plugin->is_active);
	g_return_if_fail (plugin->use_refcount > 0);

	plugin->use_refcount--;
	if (plugin->use_refcount == 0) {
		g_signal_emit (G_OBJECT (plugin), gnm_plugin_signals[CAN_DEACTIVATE_CHANGED], 0);
	}
}

/**
 * gnm_plugin_get_dependencies_ids:
 * @plugin      : The plugin
 *
 * Returns the list of identifiers of plugins that @plugin depends on.
 * All these plugins will be automatically activated before activating
 * the @plugin itself.
 * The caller must free the returned list together with the strings it
 * points to (use gnm_slist_free_custom (list, g_free) to do this).
 */
GSList *
gnm_plugin_get_dependencies_ids (GnmPlugin *plugin)
{
	GSList *list = NULL;

	GNM_SLIST_FOREACH (plugin->dependencies, PluginDependency, dep,
		GNM_SLIST_PREPEND (list, g_strdup (dep->plugin_id));
	);

	return g_slist_reverse (list);
}

/**
 * gnm_plugin_get_services:
 * @plugin      : The plugin
 *
 */
GSList *
gnm_plugin_get_services (GnmPlugin *plugin)
{
	g_return_val_if_fail (IS_GNM_PLUGIN (plugin), NULL);

	return plugin->services;
}

/*
 * May return NULL without errors (is XML file doesn't exist)
 */
static GnmPlugin *
plugin_info_read_for_dir (const gchar *dir_name, ErrorInfo **ret_error)
{
	GnmPlugin *pinfo = NULL;
	gchar *file_name;
	gchar *file_state;
	PluginFileState *state;
	ErrorInfo *plugin_error;

	g_return_val_if_fail (dir_name != NULL, NULL);

	GNM_INIT_RET_ERROR_INFO (ret_error);
	file_name = g_build_filename (dir_name, PLUGIN_INFO_FILE_NAME, NULL);
	file_state = get_file_state_as_string (file_name);
	if (file_state == NULL) {
		g_free (file_name);
		return NULL;
	}
	state = g_hash_table_lookup (plugin_file_state_dir_hash, dir_name);
	if (state != NULL && strcmp (state->file_state, file_state) == 0) {
		pinfo = plugin_info_new_with_id_and_dir_name_only (state->plugin_id, state->dir_name);
		state->age = PLUGIN_OLD_USED;
	} else if ((pinfo = plugin_info_new_from_xml (dir_name, &plugin_error)) != NULL) {
		g_assert (plugin_error == NULL);
		if (state == NULL) {
			state = g_new (PluginFileState, 1);
			state->dir_name = g_strdup (dir_name);
			state->file_state = g_strdup (file_state);
			state->plugin_id = g_strdup (gnm_plugin_get_id (pinfo));
			state->age = PLUGIN_NEW;
			g_hash_table_insert (plugin_file_state_dir_hash, state->dir_name, state);
		} else {
			if (strcmp (state->plugin_id, pinfo->id) == 0) {
				state->age = PLUGIN_OLD_USED;
			} else {
				state->age = PLUGIN_NEW;
			}
			g_free (state->file_state);
			g_free (state->plugin_id);
			state->file_state = g_strdup (file_state);
			state->plugin_id = g_strdup (gnm_plugin_get_id (pinfo));
		}
		plugin_file_state_hash_changed = TRUE;
	} else {
		*ret_error = error_info_new_printf (
		             _("Errors occurred while reading plugin informations from file \"%s\"."),
		             file_name);
		error_info_add_details (*ret_error, plugin_error);
	}
	g_free (file_name);
	g_free (file_state);

	return pinfo;
}

/*
 * May return partial list and some error info.
 */
static GSList *
plugin_info_list_read_for_subdirs_of_dir (const gchar *dir_name, ErrorInfo **ret_error)
{
	GSList *plugin_info_list = NULL;
	GDir *dir;
	char const *d_name;
	GSList *error_list = NULL;

	g_return_val_if_fail (dir_name != NULL, NULL);

	GNM_INIT_RET_ERROR_INFO (ret_error);
	dir = g_dir_open (dir_name, 0, NULL);
	if (dir == NULL)
		return NULL;

	while ((d_name = g_dir_read_name (dir)) != NULL) {
		gchar *full_entry_name;
		ErrorInfo *error = NULL;
		GnmPlugin *pinfo;

		if (strcmp (d_name, ".") == 0 || strcmp (d_name, "..") == 0)
			continue;
		full_entry_name = g_build_filename (dir_name, d_name, NULL);
		pinfo = plugin_info_read_for_dir (full_entry_name, &error);
		if (pinfo != NULL) {
			GNM_SLIST_PREPEND (plugin_info_list, pinfo);
		}
		if (error != NULL) {
			GNM_SLIST_PREPEND (error_list, error);
		}
		g_free (full_entry_name);
	}
	if (error_list != NULL) {
		GNM_SLIST_REVERSE (error_list);
		*ret_error = error_info_new_from_error_list (error_list);
	}
	g_dir_close (dir);

	return g_slist_reverse (plugin_info_list);
}

/*
 * May return partial list and some error info.
 */
static GSList *
plugin_info_list_read_for_subdirs_of_dir_list (GSList *dir_list, ErrorInfo **ret_error)
{
	GSList *plugin_info_list = NULL;
	GSList *dir_iterator;
	GSList *error_list = NULL;

	GNM_INIT_RET_ERROR_INFO (ret_error);
	for (dir_iterator = dir_list; dir_iterator != NULL; dir_iterator = dir_iterator->next) {
		gchar *dir_name;
		ErrorInfo *error = NULL;
		GSList *dir_plugin_info_list;

		dir_name = (gchar *) dir_iterator->data;
		dir_plugin_info_list = plugin_info_list_read_for_subdirs_of_dir (dir_name, &error);
		if (error != NULL) {
			GNM_SLIST_PREPEND (error_list, error);
		}
		if (dir_plugin_info_list != NULL) {
			GNM_SLIST_CONCAT (plugin_info_list, dir_plugin_info_list);
		}
	}
	if (error_list != NULL) {
		GNM_SLIST_REVERSE (error_list);
		*ret_error = error_info_new_from_error_list (error_list);
	}

	return plugin_info_list;
}

static GSList *
gnumeric_extra_plugin_dirs (void)
{
	GSList *extra_dirs;
	gchar const *plugin_path_env;

	extra_dirs = gnm_string_slist_copy (gnm_app_prefs->plugin_extra_dirs);
	plugin_path_env = g_getenv ("GNUMERIC_PLUGIN_PATH");
	if (plugin_path_env != NULL) {
		GNM_SLIST_CONCAT (extra_dirs, gnm_strsplit_to_slist (plugin_path_env, ":"));
	}

	return extra_dirs;
}

/*
 * May return partial list and some error info.
 */
static GSList *
plugin_info_list_read_for_all_dirs (ErrorInfo **ret_error)
{
	GSList *dir_list;
	GSList *plugin_info_list;
	ErrorInfo *error;

	GNM_INIT_RET_ERROR_INFO (ret_error);
	dir_list = gnm_slist_create (gnm_sys_plugin_dir (),
				     gnm_usr_plugin_dir (),
				     NULL);
	GNM_SLIST_CONCAT (dir_list, gnumeric_extra_plugin_dirs ());
	plugin_info_list = plugin_info_list_read_for_subdirs_of_dir_list (dir_list, &error);
	g_slist_foreach (dir_list, (GFunc)g_free, NULL);
	g_slist_free (dir_list);
	*ret_error = error;

	return plugin_info_list;
}

/**
 * plugin_db_activate_plugin_list:
 * @plugins     : The list of plugins
 * @ret_error   : Pointer used to report errors
 *
 * Activates all plugins in the list. If some of the plugins cannot be
 * activated, the function reports this via @ret_error (errors don't
 * affect plugins activated successfully).
 */
void
plugin_db_activate_plugin_list (GSList *plugins, ErrorInfo **ret_error)
{
	GSList *error_list = NULL;

	GNM_INIT_RET_ERROR_INFO (ret_error);
	GNM_SLIST_FOREACH (plugins, GnmPlugin, pinfo,
		ErrorInfo *error;

		gnm_plugin_activate (pinfo, &error);
		if (error != NULL) {
			ErrorInfo *new_error;

			new_error = error_info_new_printf (
			            _("Couldn't activate plugin \"%s\" (ID: %s)."),
			            pinfo->name, pinfo->id);
			error_info_add_details (new_error, error);
			GNM_SLIST_PREPEND (error_list, new_error);
		}
	);
	if (error_list != NULL) {
		GNM_SLIST_REVERSE (error_list);
		*ret_error = error_info_new_from_error_list (error_list);
	}
}

/**
 * plugin_db_deactivate_plugin_list:
 * @plugins     : The list of plugins
 * @ret_error   : Pointer used to report errors
 *
 * Deactivates all plugins in the list. If some of the plugins cannot be
 * deactivated, the function reports this via @ret_error (errors don't
 * affect plugins deactivated successfully).
 */
void
plugin_db_deactivate_plugin_list (GSList *plugins, ErrorInfo **ret_error)
{
	GSList *error_list = NULL;

	GNM_INIT_RET_ERROR_INFO (ret_error);
	GNM_SLIST_FOREACH (plugins, GnmPlugin, pinfo,
		ErrorInfo *error;

		gnm_plugin_deactivate (pinfo, &error);
		if (error != NULL) {
			ErrorInfo *new_error;

			new_error = error_info_new_printf (
			            _("Couldn't deactivate plugin \"%s\" (ID: %s)."),
			            pinfo->name, pinfo->id);
			error_info_add_details (new_error, error);
			GNM_SLIST_PREPEND (error_list, new_error);
		}
	);
	if (error_list != NULL) {
		GNM_SLIST_REVERSE (error_list);
		*ret_error = error_info_new_from_error_list (error_list);
	}
}

/**
 * plugins_get_available_plugins:
 *
 * Returns the list of available plugins. The returned value must not be
 * freed and stays valid until calling plugins_rescan or plugins_shutdown.
 */
GSList *
plugins_get_available_plugins (void)
{
	return available_plugins;
}

/**
 * plugins_get_plugin_by_id:
 * @plugin_id    : String containing plugin ID
 *
 * Returns GnmPlugin object for plugin with ID equal to @plugin_id or NULL
 * if there's no plugin available with given id.
 * Function returns "borrowed" reference, use g_object_ref if you want to
 * be sure that plugin won't disappear.
 */
GnmPlugin *
plugins_get_plugin_by_id (const gchar *plugin_id)
{
	g_return_val_if_fail (plugin_id != NULL, NULL);

	return g_hash_table_lookup (available_plugins_id_hash, plugin_id);
}

/**
 * plugin_db_mark_plugin_for_deactivation:
 * ...
 */
void
plugin_db_mark_plugin_for_deactivation (GnmPlugin *pinfo, gboolean mark)
{
	g_return_if_fail (IS_GNM_PLUGIN (pinfo));

	if (mark) {
		if (plugins_marked_for_deactivation_hash == NULL) {
			plugins_marked_for_deactivation_hash = g_hash_table_new (&g_str_hash, &g_str_equal);
		}
		g_hash_table_insert (plugins_marked_for_deactivation_hash, pinfo->id, pinfo);
	} else {
		if (plugins_marked_for_deactivation_hash != NULL) {
			g_hash_table_remove (plugins_marked_for_deactivation_hash, pinfo->id);
		}
	}
}

/**
 * plugin_db_is_plugin_marked_for_deactivation:
 * ...
 */
gboolean
plugin_db_is_plugin_marked_for_deactivation (GnmPlugin *pinfo)
{
	return plugins_marked_for_deactivation_hash != NULL &&
	       g_hash_table_lookup (plugins_marked_for_deactivation_hash, pinfo->id) != NULL;
}

static void
ghf_set_state_old_unused (gpointer key, gpointer value, gpointer unused)
{
	PluginFileState *state = value;

	state->age = PLUGIN_OLD_UNUSED;
}

/**
 * plugins_rescan:
 * @ret_error       : Pointer used to report errors
 * @ret_new_plugins : Optional pointer to return list of new plugins
 *
 *
 */
void
plugins_rescan (ErrorInfo **ret_error, GSList **ret_new_plugins)
{
	GSList *error_list = NULL;
	ErrorInfo *error;
	GSList *new_available_plugins;
	GHashTable *new_available_plugins_id_hash;
	GSList *removed_plugins = NULL, *added_plugins = NULL, *still_active_ids = NULL;

	GNM_INIT_RET_ERROR_INFO (ret_error);

	/* re-read plugins list from disk */
	g_hash_table_foreach (plugin_file_state_dir_hash, ghf_set_state_old_unused, NULL);
	new_available_plugins = plugin_info_list_read_for_all_dirs (&error);
	if (error != NULL) {
		GNM_SLIST_PREPEND (error_list, error_info_new_str_with_details (
			_("Errors while reading info about available plugins."), error));
	}

	/* Find and (try to) deactivate not any longer available plugins */
	new_available_plugins_id_hash = g_hash_table_new (g_str_hash, g_str_equal);
	GNM_SLIST_FOREACH (new_available_plugins, GnmPlugin, plugin,
		g_hash_table_insert (
			new_available_plugins_id_hash, (char *) gnm_plugin_get_id (plugin), plugin);
	);
	GNM_SLIST_FOREACH (available_plugins, GnmPlugin, plugin,
		GnmPlugin *found_plugin;

		found_plugin = g_hash_table_lookup (
			new_available_plugins_id_hash, gnm_plugin_get_id (plugin));
		if (found_plugin == NULL ||
		    strcmp (gnm_plugin_get_dir_name (found_plugin),
		            gnm_plugin_get_dir_name (plugin)) != 0) {
			GNM_SLIST_PREPEND (removed_plugins, plugin);
		}
	);
	g_hash_table_destroy (new_available_plugins_id_hash);
	plugin_db_deactivate_plugin_list (removed_plugins, &error);
	if (error != NULL) {
		GNM_SLIST_PREPEND (error_list, error_info_new_str_with_details (
			_("Errors while deactivating plugins that are no longer on disk."), error));
	}
	GNM_SLIST_FOREACH (removed_plugins, GnmPlugin, plugin,
		if (gnm_plugin_is_active (plugin)) {
			GNM_SLIST_PREPEND (still_active_ids, (char *) gnm_plugin_get_id (plugin));
		} else {
			GNM_SLIST_REMOVE (available_plugins, plugin);
			g_hash_table_remove (available_plugins_id_hash, gnm_plugin_get_id (plugin));
			g_object_unref (plugin);
		}
	);
	g_slist_free (removed_plugins);
	if (still_active_ids != NULL) {
		GString *s;

		s = g_string_new (still_active_ids->data);
		GNM_SLIST_FOREACH (still_active_ids->next, char, id,
			g_string_append (s, ", ");
			g_string_append (s, id);
		);
		GNM_SLIST_PREPEND (error_list, error_info_new_printf (
			_("The following plugins are no longer on disk but are still active:\n"
			  "%s.\nYou should restart Gnumeric now."), s->str));
		g_string_free (s, TRUE);
		gnm_slist_free_custom (still_active_ids, g_free);
	}

	/* Find previously not available plugins */
	GNM_SLIST_FOREACH (new_available_plugins, GnmPlugin, plugin,
		GnmPlugin *old_plugin;

		old_plugin = g_hash_table_lookup (
			available_plugins_id_hash, gnm_plugin_get_id (plugin));
		if (old_plugin == NULL) {
			GNM_SLIST_PREPEND (added_plugins, plugin);
			g_object_ref (plugin);
		}
	);
	gnm_slist_free_custom (new_available_plugins, g_object_unref);
	if (ret_new_plugins != NULL) {
		*ret_new_plugins = g_slist_copy (added_plugins);
	}
	GNM_SLIST_FOREACH (added_plugins, GnmPlugin, plugin,
		g_hash_table_insert (
			available_plugins_id_hash, (char *) gnm_plugin_get_id (plugin), plugin);
	);
	GNM_SLIST_CONCAT (available_plugins, added_plugins);

	/* handle errors */
	if (error_list != NULL) {
		*ret_error = error_info_new_from_error_list (g_slist_reverse (error_list));
	}
}

static void
ghf_collect_new_plugins (gpointer ignored,
			 PluginFileState *s, GSList **plugin_list)
{
	if (s->age == PLUGIN_NEW) {
		GnmPlugin *plugin = plugins_get_plugin_by_id (s->plugin_id);
		if (plugin != NULL && !plugin->require_explicit_enabling)
			GNM_SLIST_PREPEND (*plugin_list, plugin);
	}
}

/**
 * plugins_init:
 * @context     : #GnmCmdContext used to report errors
 *
 * Initializes the plugin subsystem. Don't call this function more than
 * once.
 */
void
plugins_init (GnmCmdContext *context)
{
	GSList *error_list = NULL;
	ErrorInfo *error;
	GSList *plugin_list;

	gnm_time_counter_push ();

	loader_services = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);

	/* initialize hash table with information about known plugin.xml files */
	plugin_file_state_dir_hash = g_hash_table_new_full (g_str_hash, g_str_equal, NULL, plugin_file_state_free);
	GNM_SLIST_FOREACH (gnm_app_prefs->plugin_file_states, char, state_str,
		PluginFileState *state;

		state = plugin_file_state_from_string (state_str);
		if (state != NULL)
			g_hash_table_insert (plugin_file_state_dir_hash, state->dir_name, state);
	);
	plugin_file_state_hash_changed = FALSE;

	/* collect information about the available plugins */
	available_plugins = plugin_info_list_read_for_all_dirs (&error);
	available_plugins_id_hash = g_hash_table_new (g_str_hash, g_str_equal);
	GNM_SLIST_FOREACH (available_plugins, GnmPlugin, plugin,
		g_hash_table_insert (
			available_plugins_id_hash,
			(gpointer) gnm_plugin_get_id (plugin), plugin);
	);
	if (error != NULL) {
		GNM_SLIST_PREPEND (error_list, error_info_new_str_with_details (
			_("Errors while reading info about available plugins."), error));
	}

	/* get descriptors for all previously active plugins */
	plugin_list = NULL;
	GNM_SLIST_FOREACH (gnm_app_prefs->active_plugins, char, plugin_id,
		GnmPlugin *plugin = plugins_get_plugin_by_id (plugin_id);
		if (plugin != NULL)
			GNM_SLIST_PREPEND (plugin_list, plugin);
	);

	/* get descriptors for new plugins */
	if (gnm_app_prefs->activate_new_plugins)
	{
		g_hash_table_foreach (
			plugin_file_state_dir_hash,
			(GHFunc) ghf_collect_new_plugins,
			&plugin_list);
	}

	plugin_list = g_slist_reverse (plugin_list);
	plugin_db_activate_plugin_list (plugin_list, &error);
	g_slist_free (plugin_list);
	if (error != NULL) {
		GNM_SLIST_PREPEND (error_list, error_info_new_str_with_details (
			_("Errors while activating plugins."), error));
	}

	/* report initialization errors */
	if (error_list != NULL) {
		GNM_SLIST_REVERSE (error_list);
		error = error_info_new_str_with_details_list (
		        _("Errors while initializing plugin system."),
		        error_list);

		gnm_cmd_context_error_info (context, error);
		error_info_free (error);
	}

	plugin_message (4, "plugins_init() time: %fs\n", gnm_time_counter_pop ());
}

static void
ghf_collect_used_plugin_state_strings (gpointer key, gpointer value, gpointer user_data)
{
	PluginFileState *state = value;
	GSList **strings = user_data;

	if (state->age != PLUGIN_OLD_UNUSED) {
		GNM_SLIST_PREPEND (*strings, plugin_file_state_as_string (state));
	}
}

/**
 * gnm_plugin_try_unref
 *
 * Unref plugin object if it is legal to destroy it. Destruction is
 * not legal if a type or interface has been registered for it. "Once
 * a GTypeModule is initialized, it must exist forever" - docs of
 * g_type_module_unuse().
 */
static void
gnm_plugin_try_unref (gpointer plugin)
{
	GTypeModule *module = G_TYPE_MODULE (plugin);

	if (!module->type_infos && !module->interface_infos) {
		g_object_unref (plugin);
	}
}

/**
 * plugins_shutdown:
 *
 * Shuts down the plugin subsystem. Call this function only once before
 * exiting the application. Some plugins may be left active or in broken
 * state, so calling plugins_init again will NOT work properly.
 */
void
plugins_shutdown (void)
{
	GSList *active_list = NULL, *used_plugin_state_strings = NULL;
	ErrorInfo *ignored_error;

	/* save active plugins list */
	GNM_SLIST_FOREACH (available_plugins, GnmPlugin, plugin,
		if (gnm_plugin_is_active (plugin) &&
		    !plugin_db_is_plugin_marked_for_deactivation (plugin)) {
			GNM_SLIST_PREPEND (active_list, (gpointer) gnm_plugin_get_id (plugin));
		}
	);
	active_list = g_slist_reverse (active_list);
	gnm_gconf_set_active_plugins (active_list);
	g_slist_free (active_list);

	if (plugins_marked_for_deactivation_hash != NULL) {
		g_hash_table_destroy (plugins_marked_for_deactivation_hash);
	}

	/* deactivate all plugins */
	plugin_db_deactivate_plugin_list (available_plugins, &ignored_error);
	error_info_free (ignored_error);

	/* update information stored in gconf database
	 * about known plugin.xml files and destroy hash table */
	g_hash_table_foreach (
		plugin_file_state_dir_hash,
		ghf_collect_used_plugin_state_strings,
		&used_plugin_state_strings);
	if (plugin_file_state_hash_changed ||
	    g_hash_table_size (plugin_file_state_dir_hash) != g_slist_length (used_plugin_state_strings)) {
		gnm_gconf_set_plugin_file_states (used_plugin_state_strings);
		plugin_message (5, "Plugin cache changed\n");
	} else
		gnm_slist_free_custom (used_plugin_state_strings, g_free);

	g_hash_table_destroy (plugin_file_state_dir_hash);
	g_hash_table_destroy (loader_services);
	g_hash_table_destroy (available_plugins_id_hash);
	gnm_slist_free_custom (available_plugins, gnm_plugin_try_unref);

	go_conf_sync ();
}

void
plugin_message (gint level, const gchar *format, ...)
{
#ifdef PLUGIN_DEBUG
	va_list args;

	if (level <= PLUGIN_DEBUG) {
		va_start (args, format);
		vprintf (format, args);
		va_end (args);
	}
#endif
}
