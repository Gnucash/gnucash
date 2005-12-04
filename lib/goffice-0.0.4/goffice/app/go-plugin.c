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

#include <goffice/goffice-config.h>
#include <goffice/goffice-priv.h>
#include <goffice/goffice-paths.h>
#include <goffice/app/go-plugin.h>
#include <goffice/app/go-plugin-service.h>
#include <goffice/app/go-plugin-loader.h>
#include <goffice/app/go-plugin-loader-module.h>
#include <goffice/app/go-cmd-context.h>
#include <goffice/app/error-info.h>

#include <goffice/utils/go-glib-extras.h>
#include <goffice/utils/go-libxml-extras.h>
#include <libxml/parser.h>
#include <libxml/parserInternals.h>
#include <libxml/xmlmemory.h>
#include <gsf/gsf-impl-utils.h>

#include <glib-object.h>
#include <glib/gi18n.h>
#include <gmodule.h>

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <locale.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#define PLUGIN_INFO_FILE_NAME          "plugin.xml"
#define PLUGIN_ID_VALID_CHARS          "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

#define BUILTIN_LOADER_MODULE_ID       "Gnumeric_Builtin:module"

static GHashTable *plugins_marked_for_deactivation_hash = NULL;
static GSList *available_plugins = NULL;
static GSList *go_plugin_dirs = NULL;
static GHashTable *available_plugins_id_hash = NULL;

static GHashTable *loader_services = NULL;
static GType	   go_default_loader_type;

static void plugin_get_loader_if_needed (GOPlugin *plugin, ErrorInfo **ret_error);
static void go_plugin_read (GOPlugin *plugin, const gchar *dir_name, ErrorInfo **ret_error);
static void go_plugin_load_base (GOPlugin *plugin, ErrorInfo **ret_error);

/*
 * GOPlugin
 */

typedef struct {
	gchar *plugin_id;
	GOPlugin *plugin;             /* don't use directly */
	gboolean force_load;
} PluginDependency;

struct _GOPlugin {
	GObject base;

	gboolean has_full_info;
	gchar   *dir_name;
	gchar   *id;

	gchar   *name;
	gchar   *description;
	gboolean require_explicit_enabling;

	gboolean     is_active;
	gint	     use_refcount;
	GTypeModule *type_module;

	GSList *dependencies;
	gchar *loader_id;
	GHashTable *loader_attrs;
	GOPluginLoader *loader;
	GSList *services;

	char *saved_textdomain;
};

typedef struct _GOPluginClass GOPluginClass;
struct _GOPluginClass {
	GTypeModuleClass parent_class;

	/* signals */
	void (*state_changed)		(GOPluginClass *gpc);
	void (*can_deactivate_changed)	(GOPluginClass *gpc);
};

enum {
	STATE_CHANGED,
	CAN_DEACTIVATE_CHANGED,
	LAST_SIGNAL
};

static guint go_plugin_signals[LAST_SIGNAL];
static GObjectClass *parent_class = NULL;

static void plugin_dependency_free (gpointer data);

static void
go_plugin_message (gint level, const gchar *format, ...)
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
static void
go_plugin_init (GObject *obj)
{
	GOPlugin *plugin = GO_PLUGIN (obj);

	plugin->id = NULL;
	plugin->dir_name = NULL;
	plugin->has_full_info = FALSE;
	plugin->saved_textdomain = NULL;
	plugin->require_explicit_enabling = FALSE;
}

static void
go_plugin_finalize (GObject *obj)
{
	GOPlugin *plugin = GO_PLUGIN (obj);

	if (plugin->type_module != NULL) {
		g_type_module_unuse (plugin->type_module);
		plugin->type_module = NULL;
	}

	g_free (plugin->id);
	plugin->id = NULL;
	g_free (plugin->dir_name);
	plugin->dir_name = NULL;
	if (plugin->has_full_info) {
		plugin->has_full_info = FALSE;
		g_free (plugin->name);
		g_free (plugin->description);
		go_slist_free_custom (plugin->dependencies, plugin_dependency_free);
		g_free (plugin->loader_id);
		if (plugin->loader_attrs != NULL) {
			g_hash_table_destroy (plugin->loader_attrs);
		}
		if (plugin->loader != NULL) {
			g_object_unref (plugin->loader);
		}
		go_slist_free_custom (plugin->services, g_object_unref);
	}
	g_free (plugin->saved_textdomain);
	plugin->saved_textdomain = NULL;

	parent_class->finalize (obj);
}


static void
go_plugin_class_init (GObjectClass *gobject_class)
{
	parent_class = g_type_class_peek_parent (gobject_class);

	gobject_class->finalize = go_plugin_finalize;

	go_plugin_signals[STATE_CHANGED] = g_signal_new (
		"state_changed",
		G_TYPE_FROM_CLASS (gobject_class),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GOPluginClass, state_changed),
		NULL, NULL,
		g_cclosure_marshal_VOID__VOID,
		G_TYPE_NONE, 0);
	go_plugin_signals[CAN_DEACTIVATE_CHANGED] = g_signal_new (
		"can_deactivate_changed",
		G_TYPE_FROM_CLASS (gobject_class),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GOPluginClass, can_deactivate_changed),
		NULL, NULL,
		g_cclosure_marshal_VOID__VOID,
		G_TYPE_NONE, 0);
}

GSF_CLASS (GOPlugin, go_plugin, go_plugin_class_init, go_plugin_init,
           G_TYPE_OBJECT)

static GOPlugin *
go_plugin_new_from_xml (const gchar *dir_name, ErrorInfo **ret_error)
{
	GOPlugin *plugin;
	ErrorInfo *error;

	GO_INIT_RET_ERROR_INFO (ret_error);
	plugin = g_object_new (GO_PLUGIN_TYPE, NULL);
	go_plugin_read (plugin, dir_name, &error);
	if (error == NULL) {
		plugin->has_full_info = TRUE;
	} else {
		*ret_error = error;
		g_object_unref (plugin);
		plugin = NULL;
	}

	return plugin;
}

static GOPlugin *
go_plugin_new_with_id_and_dir_name_only (const gchar *id, const gchar *dir_name)
{
	GOPlugin *plugin = g_object_new (GO_PLUGIN_TYPE, NULL);
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
go_plugin_read_full_info_if_needed_error_info (GOPlugin *plugin, ErrorInfo **ret_error)
{
	ErrorInfo *read_error;
	gchar *old_id, *old_dir_name;

	GO_INIT_RET_ERROR_INFO (ret_error);
	if (plugin->has_full_info) {
		return TRUE;
	}

	old_id = plugin->id;
	old_dir_name = plugin->dir_name;
	go_plugin_read (plugin, old_dir_name, &read_error);
	if (read_error == NULL && strcmp (plugin->id, old_id) == 0) {
		/* id and dir_name pointers are guaranteed to be valid during plugin's lifetime */
		g_free (plugin->id);
		g_free (plugin->dir_name);
		plugin->id = old_id;
		plugin->dir_name = old_dir_name;
		plugin->has_full_info = TRUE;
	} else {
		go_plugin_message (1, "Can't read plugin.xml file for %s.\n", old_id);
		if (read_error == NULL) {
			read_error = error_info_new_printf (
			             _("File contains plugin info with invalid id (%s), expected %s."),
			             plugin->id, old_id);
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
go_plugin_read_full_info_if_needed (GOPlugin *plugin)
{
	ErrorInfo *error;

	if (go_plugin_read_full_info_if_needed_error_info (plugin, &error)) {
		return TRUE;
	} else {
		g_warning ("go_plugin_read_full_info_if_needed: couldn't read plugin info from file.");
		error_info_print (error);
		error_info_free (error);
		return FALSE;
	}
}

/*
 * Accessor functions
 */

/**
 * go_plugin_get_textdomain:
 * @plugin      : The plugin
 *
 * Returns plugin's textdomain for use with textdomain(3) and d*gettext(3)
 * functions.
 */
const gchar *
go_plugin_get_textdomain (GOPlugin *plugin)
{
	g_return_val_if_fail (IS_GO_PLUGIN (plugin), NULL);

	if (plugin->saved_textdomain == NULL) {
		plugin->saved_textdomain = g_strconcat ("gnumeric__", plugin->id, NULL);
	}

	return plugin->saved_textdomain;
}

/**
 * go_plugin_is_active:
 * @plugin      : The plugin
 *
 * Returns : TRUE if @plugin is active and FALSE otherwise.
 */
gboolean
go_plugin_is_active (GOPlugin *plugin)
{
	g_return_val_if_fail (IS_GO_PLUGIN (plugin), FALSE);

	if (!plugin->has_full_info) {
		return FALSE;
	}
	return plugin->is_active;
}

/**********************************************************/
typedef GTypeModule		GOPluginTypeModule;
typedef GTypeModuleClass	GOPluginTypeModuleClass;
static gboolean go_plugin_type_module_load   (GTypeModule *module) { return TRUE; }
static void	go_plugin_type_module_unload (GTypeModule *module) { }
static void
go_plugin_type_module_class_init (GTypeModuleClass *gtm_class)
{
	gtm_class->load   = go_plugin_type_module_load;
	gtm_class->unload = go_plugin_type_module_unload;
}
static GSF_CLASS (GOPluginTypeModule, go_plugin_type_module,
	go_plugin_type_module_class_init, NULL,
	G_TYPE_TYPE_MODULE)

/**********************************************************/

/**
 * go_plugin_get_type_module :
 * @plugin : #GOPlugin
 *
 * Return the GTypeModule associated with the plugin creating it if necessary.
 **/
GTypeModule *
go_plugin_get_type_module (GOPlugin *plugin)
{
	g_return_val_if_fail (IS_GO_PLUGIN (plugin), NULL);
	g_return_val_if_fail (plugin->is_active, NULL);

	if (NULL == plugin->type_module) {
		plugin->type_module = g_object_new (go_plugin_type_module_get_type (), NULL);
		g_type_module_use (plugin->type_module);
	}
	return plugin->type_module;
}

/**
 * go_plugin_get_dir_name:
 * @plugin      : The plugin
 *
 * Returns the name of the directory in which @plugin is located.
 * Returned string is != NULL and stays valid during @plugin's lifetime.
 */
const gchar *
go_plugin_get_dir_name (GOPlugin *plugin)
{
	g_return_val_if_fail (IS_GO_PLUGIN (plugin), NULL);

	return plugin->dir_name;
}

/**
 * go_plugin_get_id:
 * @plugin      : The plugin
 *
 * Returns the ID of @plugin (unique string used for idenfification of
 * plugin).
 * Returned string is != NULL and stays valid during @plugin's lifetime.
 */
const gchar *
go_plugin_get_id (GOPlugin *plugin)
{
	g_return_val_if_fail (IS_GO_PLUGIN (plugin), NULL);

	return plugin->id;
}

/**
 * go_plugin_get_name:
 * @plugin      : The plugin
 *
 * Returns textual name of @plugin. If the real name is not available
 * for some reason, automatically generated string will be returned.
 * Returned string is != NULL and stays valid during @plugin's lifetime.
 */
const gchar *
go_plugin_get_name (GOPlugin *plugin)
{
	g_return_val_if_fail (IS_GO_PLUGIN (plugin), NULL);

	if (!go_plugin_read_full_info_if_needed (plugin)) {
		return _("Unknown name");
	}
	return plugin->name;
}

/**
 * go_plugin_get_description:
 * @plugin      : The plugin
 *
 * Returns textual description of @plugin or NULL if description is not
 * available.
 * Returned string stays valid during @plugin's lifetime.
 */
const gchar *
go_plugin_get_description (GOPlugin *plugin)
{
	g_return_val_if_fail (IS_GO_PLUGIN (plugin), NULL);

	if (!go_plugin_read_full_info_if_needed (plugin)) {
		return NULL;
	}
	return plugin->description;
}

/**
 * go_plugin_is_loaded:
 * @plugin      : The plugin
 *
 * Returns : TRUE if @plugin is loaded and FALSE otherwise.
 */
gboolean
go_plugin_is_loaded (GOPlugin *plugin)
{
	g_return_val_if_fail (IS_GO_PLUGIN (plugin), FALSE);
	return	plugin->has_full_info &&
		plugin->loader != NULL &&
		go_plugin_loader_is_base_loaded (plugin->loader);
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
 * This function is intended for use by GOPluginService objects.
 */
void
go_plugins_register_loader (const gchar *loader_id, GOPluginService *service)
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
 * This function is intended for use by GOPluginService objects.
 */
void
go_plugins_unregister_loader (const gchar *loader_id)
{
	g_return_if_fail (loader_id != NULL);

	g_hash_table_remove (loader_services, loader_id);
}

static GType
get_loader_type_by_id (const gchar *id_str, ErrorInfo **ret_error)
{
	GOPluginService *loader_service;
	ErrorInfo *error;
	GType loader_type;

	g_return_val_if_fail (id_str != NULL, G_TYPE_NONE);

	GO_INIT_RET_ERROR_INFO (ret_error);
	if (strcmp (id_str, BUILTIN_LOADER_MODULE_ID) == 0)
		return go_default_loader_type;

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

static GOPlugin *
plugin_dependency_get_plugin (PluginDependency *dep)
{
	g_return_val_if_fail (dep != NULL, NULL);

	if (dep->plugin == NULL)
		dep->plugin = go_plugins_get_plugin_by_id (dep->plugin_id);
	return dep->plugin;
}

static GSList *
go_plugin_read_dependency_list (xmlNode *tree)
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
				GO_SLIST_PREPEND (dependency_list, dep);
			}
		}
	}

	return g_slist_reverse (dependency_list);
}

static GSList *
go_plugin_read_service_list (GOPlugin *plugin, xmlNode *tree, ErrorInfo **ret_error)
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
			GOPluginService *service;
			ErrorInfo *service_error;

			service = plugin_service_new (plugin, node, &service_error);

			if (service != NULL) {
				g_assert (service_error == NULL);
				GO_SLIST_PREPEND (service_list, service);
			} else {
				ErrorInfo *error;

				error = error_info_new_printf (
				        _("Error while reading service #%d info."),
				        i);
				error_info_add_details (error, service_error);
				GO_SLIST_PREPEND (error_list, error);
			}
		}
	}
	if (error_list != NULL) {
		GO_SLIST_REVERSE (error_list);
		*ret_error = error_info_new_from_error_list (error_list);
		go_slist_free_custom (service_list, g_object_unref);
		return NULL;
	} else {
		return g_slist_reverse (service_list);
	}
}

static GHashTable *
go_plugin_read_loader_attrs (xmlNode *tree)
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
go_plugin_read (GOPlugin *plugin, const gchar *dir_name, ErrorInfo **ret_error)
{
	gchar *file_name;
	xmlDocPtr doc;
	gchar *id, *name, *description;
	xmlNode *tree, *information_node, *dependencies_node, *loader_node;
	GSList *dependency_list;
	gchar *loader_id;
	GHashTable *loader_attrs;
	gboolean require_explicit_enabling = FALSE;

	g_return_if_fail (IS_GO_PLUGIN (plugin));
	g_return_if_fail (dir_name != NULL);

	GO_INIT_RET_ERROR_INFO (ret_error);
	file_name = g_build_filename (dir_name, PLUGIN_INFO_FILE_NAME, NULL);
	doc = go_xml_parse_file (file_name);
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
		dependency_list = go_plugin_read_dependency_list (dependencies_node);
	} else {
		dependency_list = NULL;
	}
	loader_node = e_xml_get_child_by_name (tree, (xmlChar *)"loader");
	if (loader_node != NULL) {
		char *p;

		loader_id = xmlGetProp (loader_node, (xmlChar *)"type");
		if (loader_id != NULL && (p = strchr (loader_id, ':')) != NULL) {
			loader_attrs = go_plugin_read_loader_attrs (loader_node);
			if (strcmp (loader_id, BUILTIN_LOADER_MODULE_ID) != 0) {
				PluginDependency *dep;

				/* Add loader's plugin to the list of dependencies */
				dep = g_new (PluginDependency, 1);
				dep->plugin_id = g_strndup (loader_id, p - loader_id);
				dep->plugin = NULL;
				dep->force_load = FALSE;
				GO_SLIST_PREPEND (dependency_list, dep);
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
		plugin->dir_name = g_strdup (dir_name);
		plugin->id = id;
		plugin->name = name;
		plugin->description = description;
		plugin->require_explicit_enabling = require_explicit_enabling;
		plugin->is_active = FALSE;
		plugin->use_refcount = 0;
		plugin->type_module  = NULL;
		plugin->dependencies = dependency_list;
		plugin->loader_id = loader_id;
		plugin->loader_attrs = loader_attrs;
		plugin->loader = NULL;
		plugin->services = go_plugin_read_service_list (plugin, tree, &services_error);

		if (services_error != NULL) {
			*ret_error = error_info_new_printf (
				_("Errors while reading services for plugin with id=\"%s\"."),
				id);
			error_info_add_details (*ret_error, services_error);
		} else
			go_plugin_message (4, "Read plugin.xml file for %s.\n", plugin->id);
	} else {
		if (id != NULL) {
			GSList *error_list = NULL;

			if (id[strspn (id, PLUGIN_ID_VALID_CHARS)] != '\0') {
				GO_SLIST_PREPEND (error_list, error_info_new_printf (
					_("Plugin id contains invalid characters (%s)."), id));
			}
			if (name == NULL) {
				GO_SLIST_PREPEND (error_list, error_info_new_str (
					_("Unknown plugin name.")));
			}
			if (loader_id == NULL) {
				GO_SLIST_PREPEND (error_list, error_info_new_printf (
					_("No loader defined or loader id invalid for plugin with id=\"%s\"."), id));
			}
			g_assert (error_list != NULL);
			GO_SLIST_REVERSE (error_list);
			*ret_error = error_info_new_from_error_list (error_list);
		} else
			*ret_error = error_info_new_str (_("Plugin has no id."));

		go_slist_free_custom (dependency_list, plugin_dependency_free);
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
plugin_get_loader_if_needed (GOPlugin *plugin, ErrorInfo **ret_error)
{
	GType loader_type;
	ErrorInfo *error = NULL;

	g_return_if_fail (IS_GO_PLUGIN (plugin));

	GO_INIT_RET_ERROR_INFO (ret_error);
	if (!go_plugin_read_full_info_if_needed_error_info (plugin, ret_error)) {
		return;
	}
	if (plugin->loader != NULL) {
		return;
	}
	loader_type = get_loader_type_by_id (plugin->loader_id, &error);
	if (error == NULL) {
		ErrorInfo *error;
		GOPluginLoader *loader = g_object_new (loader_type, NULL);
		go_plugin_loader_set_attributes (loader, plugin->loader_attrs, &error);
		if (error == NULL) {
			plugin->loader = loader;
			go_plugin_loader_set_plugin (loader, plugin);
		} else {
			g_object_unref (loader);
			loader = NULL;
			*ret_error = error_info_new_printf (
			             _("Error initializing plugin loader (\"%s\")."),
			             plugin->loader_id);
			error_info_add_details (*ret_error, error);
		}
	} else {
		*ret_error = error;
	}
}

/**
 * go_plugin_activate:
 * @plugin      : The plugin
 * @ret_error   : Pointer used to report errors
 *
 * Activates @plugin together with all its dependencies.
 * In case of error the plugin won't be activated and detailed error
 * information will be returned using @ret_error.
 */
void
go_plugin_activate (GOPlugin *plugin, ErrorInfo **ret_error)
{
	GSList *error_list = NULL;
	GSList *l;
	gint i;
	static GSList *activate_stack = NULL;

	g_return_if_fail (IS_GO_PLUGIN (plugin));

	GO_INIT_RET_ERROR_INFO (ret_error);
	if (g_slist_find (activate_stack, plugin) != NULL) {
		*ret_error = error_info_new_str (
				     _("Detected cyclic plugin dependencies."));
		return;
	}
	if (!go_plugin_read_full_info_if_needed_error_info (plugin, ret_error)) {
		return;
	}
	if (plugin->is_active) {
		return;
	}

	/* Activate plugin dependencies */
	GO_SLIST_PREPEND (activate_stack, plugin);
	GO_SLIST_FOREACH (plugin->dependencies, PluginDependency, dep,
		GOPlugin *dep_plugin;

		dep_plugin = plugin_dependency_get_plugin (dep);
		if (dep_plugin != NULL) {
			ErrorInfo *dep_error;

			go_plugin_activate (dep_plugin, &dep_error);
			if (dep_error != NULL) {
				ErrorInfo *new_error;

				new_error = error_info_new_printf (
					_("Couldn't activate plugin with id=\"%s\"."), dep->plugin_id);
				error_info_add_details (new_error, dep_error);
				GO_SLIST_PREPEND (error_list, new_error);
			}
		} else {
			GO_SLIST_PREPEND (error_list, error_info_new_printf (
				_("Couldn't find plugin with id=\"%s\"."), dep->plugin_id));
		}
	);
	g_assert (activate_stack != NULL && activate_stack->data == plugin);
	activate_stack = g_slist_delete_link (activate_stack, activate_stack);
	if (error_list != NULL) {
		*ret_error = error_info_new_str (
				     _("Error while activating plugin dependencies."));
		error_info_add_details_list (*ret_error, error_list);
		return;
	}

	for (l = plugin->services, i = 0; l != NULL; l = l->next, i++) {
		GOPluginService *service = l->data;
		ErrorInfo *service_error;

		plugin_service_activate (service, &service_error);
		if (service_error != NULL) {
			ErrorInfo *error;

			error = error_info_new_printf (
				_("Error while activating plugin service #%d."), i);
			error_info_add_details (error, service_error);
			GO_SLIST_PREPEND (error_list, error);
		}
	}
	if (error_list != NULL) {
		*ret_error = error_info_new_from_error_list (error_list);
		/* FIXME - deactivate activated services */
		return;
	}
	GO_SLIST_FOREACH (plugin->dependencies, PluginDependency, dep,
		go_plugin_use_ref (plugin_dependency_get_plugin (dep));
	);
	plugin->is_active = TRUE;
	g_signal_emit (G_OBJECT (plugin), go_plugin_signals[STATE_CHANGED], 0);
}

/**
 * go_plugin_deactivate:
 * @plugin      : The plugin
 * @ret_error   : Pointer used to report errors
 *
 * Dectivates @plugin. Its dependencies will NOT be automatically
 * deactivated.
 * In case of error the plugin won't be deactivated and detailed error
 * information will be returned using @ret_error.
 */
void
go_plugin_deactivate (GOPlugin *plugin, ErrorInfo **ret_error)
{
	GSList *error_list = NULL;
	GSList *l;
	gint i;

	g_return_if_fail (IS_GO_PLUGIN (plugin));

	GO_INIT_RET_ERROR_INFO (ret_error);
	if (!plugin->has_full_info || !plugin->is_active) {
		return;
	}
	if (plugin->use_refcount > 0) {
		*ret_error = error_info_new_str ("Plugin is still in use.");
		return;
	}
	for (l = plugin->services, i = 0; l != NULL; l = l->next, i++) {
		GOPluginService *service = l->data;
		ErrorInfo *service_error;

		plugin_service_deactivate (service, &service_error);
		if (service_error != NULL) {
			ErrorInfo *error;

			error = error_info_new_printf (
				_("Error while deactivating plugin service #%d."), i);
			error_info_add_details (error, service_error);
			GO_SLIST_PREPEND (error_list, error);
		}
	}
	if (error_list != NULL) {
		*ret_error = error_info_new_from_error_list (error_list);
		/* FIXME - some services are still active (or broken) */
	} else {
		plugin->is_active = FALSE;
		GO_SLIST_FOREACH (plugin->dependencies, PluginDependency, dep,
			go_plugin_use_unref (plugin_dependency_get_plugin (dep));
		);
		if (plugin->loader != NULL) {
			g_object_unref (plugin->loader);
			plugin->loader = NULL;
		}
	}
	g_signal_emit (G_OBJECT (plugin), go_plugin_signals[STATE_CHANGED], 0);
}

/**
 * go_plugin_can_deactivate:
 * @plugin       : The plugin
 *
 * Tells if the plugin can be deactivated using go_plugin_deactivate.
 *
 * Returns : TRUE if @plugin can be deactivated and FALSE otherwise.
 */
gboolean
go_plugin_can_deactivate (GOPlugin *plugin)
{
	g_return_val_if_fail (IS_GO_PLUGIN (plugin), FALSE);

	if (!plugin->is_active) {
		return FALSE;
	}
	if (!go_plugin_read_full_info_if_needed (plugin)) {
		return FALSE;
	}
	return plugin->use_refcount == 0;
}

static void
go_plugin_load_base (GOPlugin *plugin, ErrorInfo **ret_error)
{
	ErrorInfo *error;
	GSList *error_list = NULL;
	static GSList *load_stack = NULL;

	GO_INIT_RET_ERROR_INFO (ret_error);
	if (g_slist_find (load_stack, plugin) != NULL) {
		*ret_error = error_info_new_str (
				     _("Detected cyclic plugin dependencies."));
		return;
	}
	if (go_plugin_is_loaded (plugin)) {
		return;
	}
	if (!go_plugin_read_full_info_if_needed_error_info (plugin, ret_error)) {
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
	GO_SLIST_PREPEND (load_stack, plugin);
	GO_SLIST_FOREACH (plugin->dependencies, PluginDependency, dep,
		GOPlugin *dep_plugin;
		ErrorInfo *dep_error;

		if (!dep->force_load) {
			continue;
		}
		dep_plugin = plugin_dependency_get_plugin (dep);
		if (dep_plugin != NULL) {
			plugin_get_loader_if_needed (dep_plugin, &dep_error);
			if (dep_error == NULL) {
				go_plugin_load_base (dep_plugin, &dep_error);
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
				GO_SLIST_PREPEND (error_list, new_error);
			}
		} else {
			GO_SLIST_PREPEND (error_list, error_info_new_printf (
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

	go_plugin_loader_load_base (plugin->loader, &error);
	if (error != NULL) {
		*ret_error = error;
		return;
	}
	g_signal_emit (G_OBJECT (plugin), go_plugin_signals[STATE_CHANGED], 0);
}

/**
 * go_plugin_load_service:
 * @plugin       : The plugin
 * @service     : Plugin service
 * @ret_error   : Pointer used to report errors
 *
 * Loads base part of the plugin if is not loaded and then loads given
 * plugin service (prepares necessary part of the plugin for direct use).
 * This function is intended for use by GOPluginService objects.
 */
void
go_plugin_load_service (GOPlugin *plugin, GOPluginService *service, ErrorInfo **ret_error)
{
	g_return_if_fail (IS_GO_PLUGIN (plugin));
	g_return_if_fail (service != NULL);

	GO_INIT_RET_ERROR_INFO (ret_error);
	go_plugin_load_base (plugin, ret_error);
	if (*ret_error != NULL) {
		return;
	}
	go_plugin_loader_load_service (plugin->loader, service, ret_error);
}

/**
 * go_plugin_unload_service:
 * @plugin       : The plugin
 * @service     : Plugin service
 * @ret_error   : Pointer used to report errors
 *
 * ...
 * This function is intended for use by GOPluginService objects.
 */
void
go_plugin_unload_service (GOPlugin *plugin, GOPluginService *service, ErrorInfo **ret_error)
{
	g_return_if_fail (IS_GO_PLUGIN (plugin));
	g_return_if_fail (plugin->loader != NULL);
	g_return_if_fail (service != NULL);

	GO_INIT_RET_ERROR_INFO (ret_error);
	if (!go_plugin_read_full_info_if_needed_error_info (plugin, ret_error)) {
		return;
	}
	go_plugin_loader_unload_service (plugin->loader, service, ret_error);
}

/**
 * go_plugin_use_ref:
 * @plugin      : The plugin
 */
void
go_plugin_use_ref (GOPlugin *plugin)
{
	g_return_if_fail (IS_GO_PLUGIN (plugin));
	g_return_if_fail (plugin->is_active);

	plugin->use_refcount++;
	if (plugin->use_refcount == 1) {
		g_signal_emit (G_OBJECT (plugin), go_plugin_signals[CAN_DEACTIVATE_CHANGED], 0);
	}
}

/**
 * go_plugin_use_unref:
 * @plugin      : The plugin
 */
void
go_plugin_use_unref (GOPlugin *plugin)
{
	g_return_if_fail (IS_GO_PLUGIN (plugin));
	g_return_if_fail (plugin->is_active);
	g_return_if_fail (plugin->use_refcount > 0);

	plugin->use_refcount--;
	if (plugin->use_refcount == 0) {
		g_signal_emit (G_OBJECT (plugin), go_plugin_signals[CAN_DEACTIVATE_CHANGED], 0);
	}
}

/**
 * go_plugin_get_dependencies_ids:
 * @plugin      : The plugin
 *
 * Returns the list of identifiers of plugins that @plugin depends on.
 * All these plugins will be automatically activated before activating
 * the @plugin itself.
 * The caller must free the returned list together with the strings it
 * points to (use go_slist_free_custom (list, g_free) to do this).
 */
GSList *
go_plugin_get_dependencies_ids (GOPlugin *plugin)
{
	GSList *list = NULL;

	GO_SLIST_FOREACH (plugin->dependencies, PluginDependency, dep,
		GO_SLIST_PREPEND (list, g_strdup (dep->plugin_id));
	);

	return g_slist_reverse (list);
}

/**
 * go_plugin_get_services:
 * @plugin      : The plugin
 *
 */
GSList *
go_plugin_get_services (GOPlugin *plugin)
{
	g_return_val_if_fail (IS_GO_PLUGIN (plugin), NULL);

	return plugin->services;
}

/*
 * May return NULL without errors (is XML file doesn't exist)
 */
static GOPlugin *
go_plugin_read_for_dir (const gchar *dir_name, ErrorInfo **ret_error)
{
	GOPlugin *plugin = NULL;
	gchar *file_name;
	gchar *file_state;
	PluginFileState *state;
	ErrorInfo *plugin_error;

	g_return_val_if_fail (dir_name != NULL, NULL);

	GO_INIT_RET_ERROR_INFO (ret_error);
	file_name = g_build_filename (dir_name, PLUGIN_INFO_FILE_NAME, NULL);
	file_state = get_file_state_as_string (file_name);
	if (file_state == NULL) {
		g_free (file_name);
		return NULL;
	}
	state = g_hash_table_lookup (plugin_file_state_dir_hash, dir_name);
	if (state != NULL && strcmp (state->file_state, file_state) == 0) {
		plugin = go_plugin_new_with_id_and_dir_name_only (state->plugin_id, state->dir_name);
		state->age = PLUGIN_OLD_USED;
	} else if ((plugin = go_plugin_new_from_xml (dir_name, &plugin_error)) != NULL) {
		g_assert (plugin_error == NULL);
		if (state == NULL) {
			state = g_new (PluginFileState, 1);
			state->dir_name = g_strdup (dir_name);
			state->file_state = g_strdup (file_state);
			state->plugin_id = g_strdup (go_plugin_get_id (plugin));
			state->age = PLUGIN_NEW;
			g_hash_table_insert (plugin_file_state_dir_hash, state->dir_name, state);
		} else {
			if (strcmp (state->plugin_id, plugin->id) == 0) {
				state->age = PLUGIN_OLD_USED;
			} else {
				state->age = PLUGIN_NEW;
			}
			g_free (state->file_state);
			g_free (state->plugin_id);
			state->file_state = g_strdup (file_state);
			state->plugin_id = g_strdup (go_plugin_get_id (plugin));
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

	return plugin;
}

/*
 * May return partial list and some error info.
 */
static GSList *
go_plugin_list_read_for_subdirs_of_dir (const gchar *dir_name, ErrorInfo **ret_error)
{
	GSList *plugins = NULL;
	GDir *dir;
	char const *d_name;
	GSList *error_list = NULL;

	g_return_val_if_fail (dir_name != NULL, NULL);

	GO_INIT_RET_ERROR_INFO (ret_error);
	dir = g_dir_open (dir_name, 0, NULL);
	if (dir == NULL)
		return NULL;

	while ((d_name = g_dir_read_name (dir)) != NULL) {
		gchar *full_entry_name;
		ErrorInfo *error = NULL;
		GOPlugin *plugin;

		if (strcmp (d_name, ".") == 0 || strcmp (d_name, "..") == 0)
			continue;
		full_entry_name = g_build_filename (dir_name, d_name, NULL);
		plugin = go_plugin_read_for_dir (full_entry_name, &error);
		if (plugin != NULL) {
			GO_SLIST_PREPEND (plugins, plugin);
		}
		if (error != NULL) {
			GO_SLIST_PREPEND (error_list, error);
		}
		g_free (full_entry_name);
	}
	if (error_list != NULL) {
		GO_SLIST_REVERSE (error_list);
		*ret_error = error_info_new_from_error_list (error_list);
	}
	g_dir_close (dir);

	return g_slist_reverse (plugins);
}

/*
 * May return partial list and some error info.
 */
static GSList *
go_plugin_list_read_for_subdirs_of_dir_list (GSList *dir_list, ErrorInfo **ret_error)
{
	GSList *plugins = NULL;
	GSList *dir_iterator;
	GSList *error_list = NULL;

	GO_INIT_RET_ERROR_INFO (ret_error);
	for (dir_iterator = dir_list; dir_iterator != NULL; dir_iterator = dir_iterator->next) {
		gchar *dir_name;
		ErrorInfo *error = NULL;
		GSList *dir_plugin_info_list;

		dir_name = (gchar *) dir_iterator->data;
		dir_plugin_info_list = go_plugin_list_read_for_subdirs_of_dir (dir_name, &error);
		if (error != NULL) {
			GO_SLIST_PREPEND (error_list, error);
		}
		if (dir_plugin_info_list != NULL) {
			GO_SLIST_CONCAT (plugins, dir_plugin_info_list);
		}
	}
	if (error_list != NULL) {
		GO_SLIST_REVERSE (error_list);
		*ret_error = error_info_new_from_error_list (error_list);
	}

	return plugins;
}

/*
 * May return partial list and some error info.
 */
static GSList *
go_plugin_list_read_for_all_dirs (ErrorInfo **ret_error)
{
	return go_plugin_list_read_for_subdirs_of_dir_list (go_plugin_dirs, ret_error);
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
go_plugin_db_activate_plugin_list (GSList *plugins, ErrorInfo **ret_error)
{
	GSList *error_list = NULL;

	GO_INIT_RET_ERROR_INFO (ret_error);
	GO_SLIST_FOREACH (plugins, GOPlugin, plugin,
		ErrorInfo *error;

		go_plugin_activate (plugin, &error);
		if (error != NULL) {
			ErrorInfo *new_error;

			new_error = error_info_new_printf (
			            _("Couldn't activate plugin \"%s\" (ID: %s)."),
			            plugin->name, plugin->id);
			error_info_add_details (new_error, error);
			GO_SLIST_PREPEND (error_list, new_error);
		}
	);
	if (error_list != NULL) {
		GO_SLIST_REVERSE (error_list);
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
go_plugin_db_deactivate_plugin_list (GSList *plugins, ErrorInfo **ret_error)
{
	GSList *error_list = NULL;

	GO_INIT_RET_ERROR_INFO (ret_error);
	GO_SLIST_FOREACH (plugins, GOPlugin, plugin,
		ErrorInfo *error;

		go_plugin_deactivate (plugin, &error);
		if (error != NULL) {
			ErrorInfo *new_error;

			new_error = error_info_new_printf (
			            _("Couldn't deactivate plugin \"%s\" (ID: %s)."),
			            plugin->name, plugin->id);
			error_info_add_details (new_error, error);
			GO_SLIST_PREPEND (error_list, new_error);
		}
	);
	if (error_list != NULL) {
		GO_SLIST_REVERSE (error_list);
		*ret_error = error_info_new_from_error_list (error_list);
	}
}

/**
 * plugins_get_available_plugins:
 *
 * Returns the list of available plugins. The returned value must not be
 * freed and stays valid until calling plugins_rescan or plugins_shutdown.
 **/
GSList *
go_plugins_get_available_plugins (void)
{
	return available_plugins;
}

/**
 * plugins_get_active_plugins:
 *
 * Returns the list of active plugins.  The caller needs to free the list, but
 * not the content.
 **/
GSList *
go_plugins_get_active_plugins (void)
{
	GSList *active_list = NULL;

	GO_SLIST_FOREACH (available_plugins, GOPlugin, plugin,
		if (go_plugin_is_active (plugin) &&
		    !go_plugin_db_is_plugin_marked_for_deactivation (plugin)) {
			GO_SLIST_PREPEND (active_list, (gpointer) go_plugin_get_id (plugin));
		}
	);
	return g_slist_reverse (active_list);
}

/**
 * plugins_get_plugin_by_id:
 * @plugin_id    : String containing plugin ID
 *
 * Returns GOPlugin object for plugin with ID equal to @plugin_id or NULL
 * if there's no plugin available with given id.
 * Function returns "borrowed" reference, use g_object_ref if you want to
 * be sure that plugin won't disappear.
 */
GOPlugin *
go_plugins_get_plugin_by_id (const gchar *plugin_id)
{
	g_return_val_if_fail (plugin_id != NULL, NULL);

	return g_hash_table_lookup (available_plugins_id_hash, plugin_id);
}

/**
 * plugin_db_mark_plugin_for_deactivation:
 * ...
 */
void
go_plugin_db_mark_plugin_for_deactivation (GOPlugin *plugin, gboolean mark)
{
	g_return_if_fail (IS_GO_PLUGIN (plugin));

	if (mark) {
		if (plugins_marked_for_deactivation_hash == NULL) {
			plugins_marked_for_deactivation_hash = g_hash_table_new (&g_str_hash, &g_str_equal);
		}
		g_hash_table_insert (plugins_marked_for_deactivation_hash, plugin->id, plugin);
	} else {
		if (plugins_marked_for_deactivation_hash != NULL) {
			g_hash_table_remove (plugins_marked_for_deactivation_hash, plugin->id);
		}
	}
}

/**
 * plugin_db_is_plugin_marked_for_deactivation:
 * ...
 */
gboolean
go_plugin_db_is_plugin_marked_for_deactivation (GOPlugin *plugin)
{
	return plugins_marked_for_deactivation_hash != NULL &&
	       g_hash_table_lookup (plugins_marked_for_deactivation_hash, plugin->id) != NULL;
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
go_plugins_rescan (ErrorInfo **ret_error, GSList **ret_new_plugins)
{
	GSList *error_list = NULL;
	ErrorInfo *error;
	GSList *new_available_plugins;
	GHashTable *new_available_plugins_id_hash;
	GSList *removed_plugins = NULL, *added_plugins = NULL, *still_active_ids = NULL;

	GO_INIT_RET_ERROR_INFO (ret_error);

	/* re-read plugins list from disk */
	g_hash_table_foreach (plugin_file_state_dir_hash, ghf_set_state_old_unused, NULL);
	new_available_plugins = go_plugin_list_read_for_all_dirs (&error);
	if (error != NULL) {
		GO_SLIST_PREPEND (error_list, error_info_new_str_with_details (
			_("Errors while reading info about available plugins."), error));
	}

	/* Find and (try to) deactivate not any longer available plugins */
	new_available_plugins_id_hash = g_hash_table_new (g_str_hash, g_str_equal);
	GO_SLIST_FOREACH (new_available_plugins, GOPlugin, plugin,
		g_hash_table_insert (
			new_available_plugins_id_hash, (char *) go_plugin_get_id (plugin), plugin);
	);
	GO_SLIST_FOREACH (available_plugins, GOPlugin, plugin,
		GOPlugin *found_plugin;

		found_plugin = g_hash_table_lookup (
			new_available_plugins_id_hash, go_plugin_get_id (plugin));
		if (found_plugin == NULL ||
		    strcmp (go_plugin_get_dir_name (found_plugin),
		            go_plugin_get_dir_name (plugin)) != 0) {
			GO_SLIST_PREPEND (removed_plugins, plugin);
		}
	);
	g_hash_table_destroy (new_available_plugins_id_hash);
	go_plugin_db_deactivate_plugin_list (removed_plugins, &error);
	if (error != NULL) {
		GO_SLIST_PREPEND (error_list, error_info_new_str_with_details (
			_("Errors while deactivating plugins that are no longer on disk."), error));
	}
	GO_SLIST_FOREACH (removed_plugins, GOPlugin, plugin,
		if (go_plugin_is_active (plugin)) {
			GO_SLIST_PREPEND (still_active_ids, (char *) go_plugin_get_id (plugin));
		} else {
			GO_SLIST_REMOVE (available_plugins, plugin);
			g_hash_table_remove (available_plugins_id_hash, go_plugin_get_id (plugin));
			g_object_unref (plugin);
		}
	);
	g_slist_free (removed_plugins);
	if (still_active_ids != NULL) {
		GString *s;

		s = g_string_new (still_active_ids->data);
		GO_SLIST_FOREACH (still_active_ids->next, const char, id,
			g_string_append (s, ", ");
			g_string_append (s, id);
		);
		GO_SLIST_PREPEND (error_list, error_info_new_printf (
			_("The following plugins are no longer on disk but are still active:\n"
			  "%s.\nYou should restart this program now."), s->str));
		g_string_free (s, TRUE);
		go_slist_free_custom (still_active_ids, g_free);
	}

	/* Find previously not available plugins */
	GO_SLIST_FOREACH (new_available_plugins, GOPlugin, plugin,
		GOPlugin *old_plugin;

		old_plugin = g_hash_table_lookup (
			available_plugins_id_hash, go_plugin_get_id (plugin));
		if (old_plugin == NULL) {
			GO_SLIST_PREPEND (added_plugins, plugin);
			g_object_ref (plugin);
		}
	);
	go_slist_free_custom (new_available_plugins, g_object_unref);
	if (ret_new_plugins != NULL) {
		*ret_new_plugins = g_slist_copy (added_plugins);
	}
	GO_SLIST_FOREACH (added_plugins, GOPlugin, plugin,
		g_hash_table_insert (
			available_plugins_id_hash, (char *) go_plugin_get_id (plugin), plugin);
	);
	GO_SLIST_CONCAT (available_plugins, added_plugins);

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
		GOPlugin *plugin = go_plugins_get_plugin_by_id (s->plugin_id);
		if (plugin != NULL && !plugin->require_explicit_enabling)
			GO_SLIST_PREPEND (*plugin_list, plugin);
	}
}

static void
go_plugins_set_dirs (GSList *plugin_dirs)
{
	if (go_plugin_dirs != plugin_dirs) {
		g_slist_foreach (go_plugin_dirs, (GFunc) g_free, NULL);
		g_slist_free (go_plugin_dirs);
		go_plugin_dirs = plugin_dirs;
	}
}

/**
 * go_plugins_init:
 * @context     : #GOCmdContext used to report errors
 *
 * Initializes the plugin subsystem. Don't call this function more than
 * once.
 */
void
go_plugins_init (GOCmdContext *context,
		 GSList const *known_states,
		 GSList const *active_plugins,
		 GSList *plugin_dirs,
		 gboolean activate_new_plugins,
		 GType  default_loader_type)
{
	GSList *error_list = NULL;
	ErrorInfo *error;
	GSList *plugin_list;

	plugin_dirs = g_slist_append (plugin_dirs, go_plugins_get_plugin_dir ());

	go_default_loader_type = default_loader_type;
	go_plugins_set_dirs (plugin_dirs);

	loader_services = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);

	/* initialize hash table with information about known plugin.xml files */
	plugin_file_state_dir_hash = g_hash_table_new_full (g_str_hash, g_str_equal, NULL, plugin_file_state_free);
	GO_SLIST_FOREACH (known_states, char, state_str,
		PluginFileState *state;

		state = plugin_file_state_from_string (state_str);
		if (state != NULL)
			g_hash_table_insert (plugin_file_state_dir_hash, state->dir_name, state);
	);
	plugin_file_state_hash_changed = FALSE;

	/* collect information about the available plugins */
	available_plugins = go_plugin_list_read_for_all_dirs (&error);
	available_plugins_id_hash = g_hash_table_new (g_str_hash, g_str_equal);
	GO_SLIST_FOREACH (available_plugins, GOPlugin, plugin,
		g_hash_table_insert (
			available_plugins_id_hash,
			(gpointer) go_plugin_get_id (plugin), plugin);
	);
	if (error != NULL) {
		GO_SLIST_PREPEND (error_list, error_info_new_str_with_details (
			_("Errors while reading info about available plugins."), error));
	}

	/* get descriptors for all previously active plugins */
	plugin_list = NULL;
	GO_SLIST_FOREACH (active_plugins, char, plugin_id,
		GOPlugin *plugin = go_plugins_get_plugin_by_id (plugin_id);
		if (plugin != NULL)
			GO_SLIST_PREPEND (plugin_list, plugin);
	);

	/* get descriptors for new plugins */
	if (activate_new_plugins)
		g_hash_table_foreach (
			plugin_file_state_dir_hash,
			(GHFunc) ghf_collect_new_plugins,
			&plugin_list);

	plugin_list = g_slist_reverse (plugin_list);
	go_plugin_db_activate_plugin_list (plugin_list, &error);
	g_slist_free (plugin_list);
	if (error != NULL) {
		GO_SLIST_PREPEND (error_list, error_info_new_str_with_details (
			_("Errors while activating plugins."), error));
	}

	/* report initialization errors */
	if (error_list != NULL) {
		GO_SLIST_REVERSE (error_list);
		error = error_info_new_str_with_details_list (
		        _("Errors while initializing plugin system."),
		        error_list);

		go_cmd_context_error_info (context, error);
		error_info_free (error);
	}
}

static void
ghf_collect_used_plugin_state_strings (gpointer key, gpointer value, gpointer user_data)
{
	PluginFileState *state = value;
	GSList **strings = user_data;

	if (state->age != PLUGIN_OLD_UNUSED) {
		GO_SLIST_PREPEND (*strings, plugin_file_state_as_string (state));
	}
}

/**
 * plugins_shutdown:
 *
 * Shuts down the plugin subsystem. Call this function only once before
 * exiting the application. Some plugins may be left active or in broken
 * state, so calling plugins_init again will NOT work properly.
 */
GSList *
go_plugins_shutdown (void)
{
	GSList *used_plugin_state_strings = NULL;
	ErrorInfo *ignored_error;

	if (plugins_marked_for_deactivation_hash != NULL) {
		g_hash_table_destroy (plugins_marked_for_deactivation_hash);
	}

	/* deactivate all plugins */
	go_plugin_db_deactivate_plugin_list (available_plugins, &ignored_error);
	error_info_free (ignored_error);

	/* update information stored in gconf database
	 * about known plugin.xml files and destroy hash table */
	g_hash_table_foreach (
		plugin_file_state_dir_hash,
		ghf_collect_used_plugin_state_strings,
		&used_plugin_state_strings);
	if (!plugin_file_state_hash_changed &&
	    g_hash_table_size (plugin_file_state_dir_hash) == g_slist_length (used_plugin_state_strings)) {
		go_slist_free_custom (used_plugin_state_strings, g_free);
		used_plugin_state_strings = NULL;
	}

	g_hash_table_destroy (plugin_file_state_dir_hash);
	g_hash_table_destroy (loader_services);
	g_hash_table_destroy (available_plugins_id_hash);
	go_slist_free_custom (available_plugins, g_object_unref);

	return used_plugin_state_strings;
}

char *
go_plugins_get_plugin_dir (void)
{
	return g_build_filename (go_sys_lib_dir (), "plugins", NULL);
}
