/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-plugin-service.c: Plugin services - reading XML info, activating, etc.
 *                   (everything independent of plugin loading method)
 *
 * Author: Zbigniew Chyla (cyba@gnome.pl)
 */

#include <goffice/goffice-config.h>
#include "go-plugin-service.h"
#include "go-plugin-service-impl.h"

#include <goffice/app/error-info.h>
#include <goffice/app/go-plugin.h>
#include <goffice/app/file.h>
#include <goffice/app/file-priv.h>
#include <goffice/app/io-context.h>
#include <goffice/utils/go-glib-extras.h>
#include <goffice/utils/go-libxml-extras.h>

#include <gsf/gsf-input.h>
#include <gsf/gsf-output.h>
#include <libxml/globals.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-utils.h>
#include <glib/gi18n.h>

#include <string.h>

static GHashTable *services = NULL;

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
get_plugin_file_savers_hash (GOPlugin *plugin)
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


static void
plugin_service_init (GObject *obj)
{
	GOPluginService *service = GO_PLUGIN_SERVICE (obj);

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
	GOPluginService *service = GO_PLUGIN_SERVICE (obj);
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
	GOPluginServiceClass *plugin_service_class = GPS_CLASS (gobject_class);

	gobject_class->finalize = plugin_service_finalize;
	plugin_service_class->read_xml = NULL;
	plugin_service_class->activate = NULL;
	plugin_service_class->deactivate = NULL;
	plugin_service_class->get_description = NULL;
}

GSF_CLASS (GOPluginService, plugin_service,
	   plugin_service_class_init, plugin_service_init,
           G_TYPE_OBJECT)


/****************************************************************************/

/*
 * PluginServiceGeneral
 */

typedef struct{
	GOPluginServiceClass plugin_service_class;
} PluginServiceGeneralClass;

struct _PluginServiceGeneral {
	GOPluginService plugin_service;
	PluginServiceGeneralCallbacks cbs;
};


static void
plugin_service_general_init (GObject *obj)
{
	PluginServiceGeneral *service_general = GO_PLUGIN_SERVICE_GENERAL (obj);

	GO_PLUGIN_SERVICE (obj)->cbs_ptr = &service_general->cbs;
	service_general->cbs.plugin_func_init = NULL;
	service_general->cbs.plugin_func_cleanup = NULL;
}

static void
plugin_service_general_activate (GOPluginService *service, ErrorInfo **ret_error)
{
	PluginServiceGeneral *service_general = GO_PLUGIN_SERVICE_GENERAL (service);
	ErrorInfo *error = NULL;

	GO_INIT_RET_ERROR_INFO (ret_error);
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
plugin_service_general_deactivate (GOPluginService *service, ErrorInfo **ret_error)
{
	PluginServiceGeneral *service_general = GO_PLUGIN_SERVICE_GENERAL (service);
	ErrorInfo *error = NULL;

	GO_INIT_RET_ERROR_INFO (ret_error);
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
plugin_service_general_get_description (GOPluginService *service)
{
	return g_strdup (_("General"));
}

static void
plugin_service_general_class_init (GObjectClass *gobject_class)
{
	GOPluginServiceClass *plugin_service_class = GPS_CLASS (gobject_class);

	plugin_service_class->activate = plugin_service_general_activate;
	plugin_service_class->deactivate = plugin_service_general_deactivate;
	plugin_service_class->get_description = plugin_service_general_get_description;
}

GSF_CLASS (PluginServiceGeneral, plugin_service_general,
           plugin_service_general_class_init, plugin_service_general_init,
           GO_PLUGIN_SERVICE_TYPE)

/****************************************************************************/

/*
 * PluginServiceFileOpener
 */

typedef struct _GOPluginFileOpener GOPluginFileOpener;
static GOPluginFileOpener *go_plugin_file_opener_new (GOPluginService *service);

struct _InputFileSaveInfo {
	gchar *saver_id_str;
	FileFormatLevel format_level;
};

typedef struct _InputFileSaveInfo InputFileSaveInfo;


typedef struct{
	GOPluginServiceClass plugin_service_class;
} PluginServiceFileOpenerClass;

struct _PluginServiceFileOpener {
	GOPluginService plugin_service;

	gint priority;
	gboolean has_probe;
	gchar *description;
	GSList *suffixes;	/* list of char * */
	GSList *mimes;		/* list of char * */

	GOFileOpener *opener;
	PluginServiceFileOpenerCallbacks cbs;
};


static void
plugin_service_file_opener_init (GObject *obj)
{
	PluginServiceFileOpener *service_file_opener = GO_PLUGIN_SERVICE_FILE_OPENER (obj);

	GO_PLUGIN_SERVICE (obj)->cbs_ptr = &service_file_opener->cbs;
	service_file_opener->description = NULL;
	service_file_opener->suffixes = NULL;
	service_file_opener->mimes = NULL;
	service_file_opener->opener = NULL;
	service_file_opener->cbs.plugin_func_file_probe = NULL;
	service_file_opener->cbs.plugin_func_file_open = NULL;
}

static void
plugin_service_file_opener_finalize (GObject *obj)
{
	PluginServiceFileOpener *service_file_opener = GO_PLUGIN_SERVICE_FILE_OPENER (obj);
	GObjectClass *parent_class;

	g_free (service_file_opener->description);
	service_file_opener->description = NULL;
	go_slist_free_custom (service_file_opener->suffixes, g_free);
	service_file_opener->suffixes = NULL;
	go_slist_free_custom (service_file_opener->mimes, g_free);
	service_file_opener->mimes = NULL;
	if (service_file_opener->opener != NULL) {
		g_object_unref (service_file_opener->opener);
		service_file_opener->opener = NULL;
	}

	parent_class = g_type_class_peek (GO_PLUGIN_SERVICE_TYPE);
	parent_class->finalize (obj);
}

static void
plugin_service_file_opener_read_xml (GOPluginService *service, xmlNode *tree, ErrorInfo **ret_error)
{
	int priority;
	gboolean has_probe;
	xmlNode *information_node;
	gchar *description;

	GO_INIT_RET_ERROR_INFO (ret_error);
	if (xml_node_get_int (tree, "priority", &priority))
		priority = CLAMP (priority, 0, 100);
	else
		priority = 50;

	if (!xml_node_get_bool (tree, "probe", &has_probe))
		has_probe = TRUE;

	information_node = e_xml_get_child_by_name (tree, (xmlChar *)"information");
	if (information_node != NULL) {
		xmlNode *node;
		xmlChar *val;

		node = e_xml_get_child_by_name_by_lang (
		       information_node, "description");
		if (node != NULL) {
			val = xmlNodeGetContent (node);
			description = g_strdup ((gchar *)val);
			xmlFree (val);
		} else {
			description = NULL;
		}
	} else {
		description = NULL;
	}
	if (description != NULL) {
		GSList *suffixes = NULL, *mimes = NULL;
		char *tmp;
		xmlNode *list, *node;
		PluginServiceFileOpener *service_file_opener = GO_PLUGIN_SERVICE_FILE_OPENER (service);

		list = e_xml_get_child_by_name (tree, (xmlChar *)"suffixes");
		if (list != NULL) {
			for (node = list->xmlChildrenNode; node != NULL; node = node->next)
				if (strcmp (node->name, "suffix") == 0 &&
				    (tmp = xmlNodeGetContent (node)) != NULL)
					GO_SLIST_PREPEND (suffixes, tmp);
		}
		GO_SLIST_REVERSE (suffixes);

		list = e_xml_get_child_by_name (tree, (xmlChar *)"mime-types");
		if (list != NULL) {
			for (node = list->xmlChildrenNode; node != NULL; node = node->next)
				if (strcmp (node->name, "mime-type") == 0 &&
				    (tmp = xmlNodeGetContent (node)) != NULL)
					GO_SLIST_PREPEND (mimes, tmp);
		}
		GO_SLIST_REVERSE (mimes);

		service_file_opener->priority = priority;
		service_file_opener->has_probe = has_probe;
		service_file_opener->description = description;
		service_file_opener->suffixes	= suffixes;
		service_file_opener->mimes	= mimes;
	} else {
		*ret_error = error_info_new_str (_("File opener has no description"));
	}
}

static void
plugin_service_file_opener_activate (GOPluginService *service, ErrorInfo **ret_error)
{
	PluginServiceFileOpener *service_file_opener = GO_PLUGIN_SERVICE_FILE_OPENER (service);

	GO_INIT_RET_ERROR_INFO (ret_error);
	service_file_opener->opener = GO_FILE_OPENER (go_plugin_file_opener_new (service));
	go_file_opener_register (service_file_opener->opener,
				  service_file_opener->priority);
	service->is_active = TRUE;
}

static void
plugin_service_file_opener_deactivate (GOPluginService *service, ErrorInfo **ret_error)
{
	PluginServiceFileOpener *service_file_opener = GO_PLUGIN_SERVICE_FILE_OPENER (service);

	GO_INIT_RET_ERROR_INFO (ret_error);
	go_file_opener_unregister (service_file_opener->opener);
	service->is_active = FALSE;
}

static char *
plugin_service_file_opener_get_description (GOPluginService *service)
{
	PluginServiceFileOpener *service_file_opener = GO_PLUGIN_SERVICE_FILE_OPENER (service);

	return g_strdup_printf (
		_("File opener - %s"), service_file_opener->description);
}

static void
plugin_service_file_opener_class_init (GObjectClass *gobject_class)
{
	GOPluginServiceClass *plugin_service_class = GPS_CLASS (gobject_class);

	gobject_class->finalize = plugin_service_file_opener_finalize;
	plugin_service_class->read_xml = plugin_service_file_opener_read_xml;
	plugin_service_class->activate = plugin_service_file_opener_activate;
	plugin_service_class->deactivate = plugin_service_file_opener_deactivate;
	plugin_service_class->get_description = plugin_service_file_opener_get_description;
}

GSF_CLASS (PluginServiceFileOpener, plugin_service_file_opener,
           plugin_service_file_opener_class_init, plugin_service_file_opener_init,
           GO_PLUGIN_SERVICE_TYPE)


/** GOPluginFileOpener class **/

#define TYPE_GO_PLUGIN_FILE_OPENER             (go_plugin_file_opener_get_type ())
#define GO_PLUGIN_FILE_OPENER(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_GO_PLUGIN_FILE_OPENER, GOPluginFileOpener))
#define GO_PLUGIN_FILE_OPENER_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_GO_PLUGIN_FILE_OPENER, GOPluginFileOpenerClass))
#define IS_GO_PLUGIN_FILE_OPENER(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_GO_PLUGIN_FILE_OPENER))

GType go_plugin_file_opener_get_type (void);

typedef struct {
	GOFileOpenerClass parent_class;
} GOPluginFileOpenerClass;

struct _GOPluginFileOpener {
	GOFileOpener parent;

	GOPluginService *service;
};

static void
go_plugin_file_opener_init (GOPluginFileOpener *fo)
{
	fo->service = NULL;
}

static gboolean
go_plugin_file_opener_can_probe (GOFileOpener const *fo, FileProbeLevel pl)
{
	GOPluginFileOpener *pfo = GO_PLUGIN_FILE_OPENER (fo);
	PluginServiceFileOpener *service_file_opener = GO_PLUGIN_SERVICE_FILE_OPENER (pfo->service);
	if (pl == FILE_PROBE_FILE_NAME)
		return service_file_opener->suffixes != NULL;
	return service_file_opener->has_probe;
}

static gboolean
go_plugin_file_opener_probe (GOFileOpener const *fo, GsfInput *input,
                               FileProbeLevel pl)
{
	GOPluginFileOpener *pfo = GO_PLUGIN_FILE_OPENER (fo);
	PluginServiceFileOpener *service_file_opener = GO_PLUGIN_SERVICE_FILE_OPENER (pfo->service);

	g_return_val_if_fail (GSF_IS_INPUT (input), FALSE);

	if (pl == FILE_PROBE_FILE_NAME && service_file_opener->suffixes != NULL) {
		GSList *ptr;
		gchar const *extension;
		gchar *lowercase_extension;

		if (gsf_input_name (input) == NULL)
			return FALSE;
		extension = gsf_extension_pointer (gsf_input_name (input));
		if (extension == NULL)
			return FALSE;

		lowercase_extension = g_utf8_strdown (extension, -1);
		for (ptr = service_file_opener->suffixes; ptr != NULL ; ptr = ptr->next)
			if (0 == strcmp (lowercase_extension, ptr->data))
				break;
		g_free (lowercase_extension);
		return ptr != NULL;
	}

	if (service_file_opener->has_probe) {
		ErrorInfo *ignored_error = NULL;

		plugin_service_load (pfo->service, &ignored_error);
		if (ignored_error != NULL) {
			error_info_print (ignored_error);
			error_info_free (ignored_error);
			return FALSE;
		} else if (service_file_opener->cbs.plugin_func_file_probe == NULL) {
			return FALSE;
		} else {
			gboolean res = service_file_opener->cbs.plugin_func_file_probe (fo, pfo->service, input, pl);
			gsf_input_seek (input, 0, G_SEEK_SET);
			return res;
		}
	} else {
		return FALSE;
	}
}

static void
go_plugin_file_opener_open (GOFileOpener const *fo, gchar const *unused_enc,
			     IOContext *io_context,
			     gpointer FIXME_FIXME_workbook_view,
			     GsfInput *input)

{
	GOPluginFileOpener *pfo = GO_PLUGIN_FILE_OPENER (fo);
	PluginServiceFileOpener *service_file_opener = GO_PLUGIN_SERVICE_FILE_OPENER (pfo->service);
	ErrorInfo *error = NULL;

	g_return_if_fail (GSF_IS_INPUT (input));

	plugin_service_load (pfo->service, &error);
	if (error != NULL) {
		gnumeric_io_error_info_set (io_context, error);
		gnumeric_io_error_push (io_context, error_info_new_str (
		                        _("Error while reading file.")));
		return;
	}

	g_return_if_fail (service_file_opener->cbs.plugin_func_file_open != NULL);
	service_file_opener->cbs.plugin_func_file_open (fo, pfo->service, io_context, FIXME_FIXME_workbook_view, input);
}

static void
go_plugin_file_opener_class_init (GOPluginFileOpenerClass *klass)
{
	GOFileOpenerClass *go_file_opener_klass = GO_FILE_OPENER_CLASS (klass);

	go_file_opener_klass->can_probe = go_plugin_file_opener_can_probe;
	go_file_opener_klass->probe = go_plugin_file_opener_probe;
	go_file_opener_klass->open = go_plugin_file_opener_open;
}

GSF_CLASS (GOPluginFileOpener, go_plugin_file_opener,
	   go_plugin_file_opener_class_init, go_plugin_file_opener_init,
	   TYPE_GO_FILE_OPENER)

static GSList *
go_str_slist_dup (GSList *l)
{
	GSList *res = NULL;
	for ( ; l != NULL ; l = l->next)
		res = g_slist_prepend (res, g_strdup (l->data));
	return g_slist_reverse (res);
}

static GOPluginFileOpener *
go_plugin_file_opener_new (GOPluginService *service)
{
	PluginServiceFileOpener *service_file_opener = GO_PLUGIN_SERVICE_FILE_OPENER (service);
	GOPluginFileOpener *fo;
	gchar *opener_id;

	opener_id = g_strconcat (
		go_plugin_get_id (service->plugin), ":", service->id, NULL);
	fo = GO_PLUGIN_FILE_OPENER (g_object_new (TYPE_GO_PLUGIN_FILE_OPENER, NULL));
	go_file_opener_setup (GO_FILE_OPENER (fo), opener_id,
		service_file_opener->description,
		go_str_slist_dup (service_file_opener->suffixes),
		go_str_slist_dup (service_file_opener->mimes),
		FALSE, NULL, NULL);
	fo->service = service;
	g_free (opener_id);

	return fo;
}

/** -- **/


/*
 * PluginServiceFileSaver
 */

typedef struct _GOPluginFileSaver GOPluginFileSaver;
static GOPluginFileSaver *go_plugin_file_saver_new (GOPluginService *service);


typedef struct{
	GOPluginServiceClass plugin_service_class;
} PluginServiceFileSaverClass;

struct _PluginServiceFileSaver {
	GOPluginService plugin_service;

	gchar *file_extension;
	FileFormatLevel format_level;
	gchar *description;
	gint   default_saver_priority;
	FileSaveScope save_scope;
	gboolean overwrite_files;

	GOFileSaver *saver;
	PluginServiceFileSaverCallbacks cbs;
};


static void
plugin_service_file_saver_init (GObject *obj)
{
	PluginServiceFileSaver *service_file_saver = GO_PLUGIN_SERVICE_FILE_SAVER (obj);

	GO_PLUGIN_SERVICE (obj)->cbs_ptr = &service_file_saver->cbs;
	service_file_saver->file_extension = NULL;
	service_file_saver->description = NULL;
	service_file_saver->cbs.plugin_func_file_save = NULL;
	service_file_saver->saver = NULL;
}

static void
plugin_service_file_saver_finalize (GObject *obj)
{
	PluginServiceFileSaver *service_file_saver = GO_PLUGIN_SERVICE_FILE_SAVER (obj);
	GObjectClass *parent_class;

	g_free (service_file_saver->file_extension);
	service_file_saver->file_extension = NULL;
	g_free (service_file_saver->description);
	service_file_saver->description = NULL;
	if (service_file_saver->saver != NULL) {
		g_object_unref (service_file_saver->saver);
		service_file_saver->saver = NULL;
	}

	parent_class = g_type_class_peek (GO_PLUGIN_SERVICE_TYPE);
	parent_class->finalize (obj);
}

static void
plugin_service_file_saver_read_xml (GOPluginService *service, xmlNode *tree, ErrorInfo **ret_error)
{
	gchar *file_extension;
	xmlNode *information_node;
	gchar *description;
	gchar *format_level_str, *save_scope_str;

	GO_INIT_RET_ERROR_INFO (ret_error);
	file_extension = xml_node_get_cstr (tree, "file_extension");
	format_level_str = xml_node_get_cstr (tree, "format_level");
	save_scope_str = xml_node_get_cstr (tree, "save_scope");
	information_node = e_xml_get_child_by_name (tree, (xmlChar *)"information");
	if (information_node != NULL) {
		xmlNode *node;
		xmlChar *val;

		node = e_xml_get_child_by_name_by_lang (
		       information_node, "description");
		if (node != NULL) {
			val = xmlNodeGetContent (node);
			description = g_strdup ((gchar *)val);
			xmlFree (val);
		} else {
			description = NULL;
		}
	} else {
		description = NULL;
	}
	if (description != NULL) {
		PluginServiceFileSaver *service_file_saver = GO_PLUGIN_SERVICE_FILE_SAVER (service);

		service_file_saver->file_extension = file_extension;
		service_file_saver->description = description;
		service_file_saver->format_level = parse_format_level_str (format_level_str,
		                                                           FILE_FL_WRITE_ONLY);
		if (!xml_node_get_int (tree, "default_saver_priority", &(service_file_saver->default_saver_priority)))
			service_file_saver->default_saver_priority = -1;

		service_file_saver->save_scope = FILE_SAVE_WORKBOOK;
		if (save_scope_str) {
			if (g_ascii_strcasecmp (save_scope_str, "sheet") == 0)
				service_file_saver->save_scope 
					= FILE_SAVE_SHEET;
			else if (g_ascii_strcasecmp (save_scope_str, 
						     "range") == 0) {
				service_file_saver->save_scope 
					= FILE_SAVE_RANGE;
			}
		}
		if (!xml_node_get_bool (tree, "overwrite_files", &(service_file_saver->overwrite_files)))
			service_file_saver->overwrite_files = TRUE;
	} else {
		*ret_error = error_info_new_str (_("File saver has no description"));
		g_free (file_extension);
	}
	g_free (format_level_str);
	g_free (save_scope_str);
}

static void
plugin_service_file_saver_activate (GOPluginService *service, ErrorInfo **ret_error)
{
	PluginServiceFileSaver *service_file_saver = GO_PLUGIN_SERVICE_FILE_SAVER (service);
	GHashTable *file_savers_hash;

	GO_INIT_RET_ERROR_INFO (ret_error);
	service_file_saver->saver = GO_FILE_SAVER (go_plugin_file_saver_new (service));
	if (service_file_saver->default_saver_priority < 0) {
		go_file_saver_register (service_file_saver->saver);
	} else {
		go_file_saver_register_as_default (service_file_saver->saver,
						    service_file_saver->default_saver_priority);
	}
	file_savers_hash = get_plugin_file_savers_hash (service->plugin);
	g_assert (g_hash_table_lookup (file_savers_hash, service->id) == NULL);
	g_hash_table_insert (file_savers_hash, g_strdup (service->id), service_file_saver->saver);
	service->is_active = TRUE;
}

static void
plugin_service_file_saver_deactivate (GOPluginService *service, ErrorInfo **ret_error)
{
	PluginServiceFileSaver *service_file_saver = GO_PLUGIN_SERVICE_FILE_SAVER (service);
	GHashTable *file_savers_hash;

	GO_INIT_RET_ERROR_INFO (ret_error);
	file_savers_hash = get_plugin_file_savers_hash (service->plugin);
	g_hash_table_remove (file_savers_hash, service->id);
	go_file_saver_unregister (service_file_saver->saver);
	service->is_active = FALSE;
}

static char *
plugin_service_file_saver_get_description (GOPluginService *service)
{
	PluginServiceFileSaver *service_file_saver = GO_PLUGIN_SERVICE_FILE_SAVER (service);

	return g_strdup_printf (
		_("File saver - %s"), service_file_saver->description);
}

static void
plugin_service_file_saver_class_init (GObjectClass *gobject_class)
{
	GOPluginServiceClass *plugin_service_class = GPS_CLASS (gobject_class);

	gobject_class->finalize = plugin_service_file_saver_finalize;
	plugin_service_class->read_xml = plugin_service_file_saver_read_xml;
	plugin_service_class->activate = plugin_service_file_saver_activate;
	plugin_service_class->deactivate = plugin_service_file_saver_deactivate;
	plugin_service_class->get_description = plugin_service_file_saver_get_description;
}

GSF_CLASS (PluginServiceFileSaver, plugin_service_file_saver,
           plugin_service_file_saver_class_init, plugin_service_file_saver_init,
           GO_PLUGIN_SERVICE_TYPE)


/** GOPluginFileSaver class **/

#define TYPE_GO_PLUGIN_FILE_SAVER             (go_plugin_file_saver_get_type ())
#define GO_PLUGIN_FILE_SAVER(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_GO_PLUGIN_FILE_SAVER, GOPluginFileSaver))
#define GO_PLUGIN_FILE_SAVER_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_GO_PLUGIN_FILE_SAVER, GOPluginFileSaverClass))
#define IS_GO_PLUGIN_FILE_SAVER(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_GO_PLUGIN_FILE_SAVER))

GType go_plugin_file_saver_get_type (void);

typedef struct {
	GOFileSaverClass parent_class;
} GOPluginFileSaverClass;

struct _GOPluginFileSaver {
	GOFileSaver parent;

	GOPluginService *service;
};

static void
go_plugin_file_saver_init (GOPluginFileSaver *fs)
{
	fs->service = NULL;
}

static void
go_plugin_file_saver_save (GOFileSaver const *fs, IOContext *io_context,
			    gconstpointer FIXME_FIXME_workbook_view,
			    GsfOutput *output)
{
	GOPluginFileSaver *pfs = GO_PLUGIN_FILE_SAVER (fs);
	PluginServiceFileSaver *service_file_saver = GO_PLUGIN_SERVICE_FILE_SAVER (pfs->service);
	ErrorInfo *error = NULL;

	g_return_if_fail (GSF_IS_OUTPUT (output));

	plugin_service_load (pfs->service, &error);
	if (error != NULL) {
		gnumeric_io_error_info_set (io_context, error);
		gnumeric_io_error_push (io_context, error_info_new_str (
		                        _("Error while loading plugin for saving.")));
		if (!gsf_output_error (output))
			gsf_output_set_error (output, 0, _("Failed to load plugin for saving"));
		return;
	}

	g_return_if_fail (service_file_saver->cbs.plugin_func_file_save != NULL);
	service_file_saver->cbs.plugin_func_file_save (fs, pfs->service, io_context, FIXME_FIXME_workbook_view, output);
}

static void
go_plugin_file_saver_class_init (GOPluginFileSaverClass *klass)
{
	GOFileSaverClass *go_file_saver_klass = GO_FILE_SAVER_CLASS (klass);

	go_file_saver_klass->save = go_plugin_file_saver_save;
}

GSF_CLASS (GOPluginFileSaver, go_plugin_file_saver,
	   go_plugin_file_saver_class_init, go_plugin_file_saver_init,
	   TYPE_GO_FILE_SAVER)

static GOPluginFileSaver *
go_plugin_file_saver_new (GOPluginService *service)
{
	GOPluginFileSaver *fs;
	PluginServiceFileSaver *service_file_saver = GO_PLUGIN_SERVICE_FILE_SAVER (service);
	gchar *saver_id;

	saver_id = g_strconcat (
		go_plugin_get_id (service->plugin), ":", service->id, NULL);
	fs = GO_PLUGIN_FILE_SAVER (g_object_new (TYPE_GO_PLUGIN_FILE_SAVER, NULL));
	go_file_saver_setup (GO_FILE_SAVER (fs), saver_id,
	                       service_file_saver->file_extension,
	                       service_file_saver->description,
	                       service_file_saver->format_level,
	                       NULL);
	go_file_saver_set_save_scope (GO_FILE_SAVER (fs),
	                                service_file_saver->save_scope);
	go_file_saver_set_overwrite_files (GO_FILE_SAVER (fs),
	                                     service_file_saver->overwrite_files);
	fs->service = service;
	g_free (saver_id);

	return fs;
}

/*
 * PluginServicePluginLoader
 */

typedef struct{
	GOPluginServiceClass plugin_service_class;
} PluginServicePluginLoaderClass;

struct _PluginServicePluginLoader {
	GOPluginService plugin_service;
	PluginServicePluginLoaderCallbacks cbs;
};


static void
plugin_service_plugin_loader_init (GObject *obj)
{
	PluginServicePluginLoader *service_plugin_loader = GO_PLUGIN_SERVICE_PLUGIN_LOADER (obj);

	GO_PLUGIN_SERVICE (obj)->cbs_ptr = &service_plugin_loader->cbs;
}

GType
plugin_service_plugin_loader_generate_type (GOPluginService *service, ErrorInfo **ret_error)
{
	PluginServicePluginLoader *service_plugin_loader = GO_PLUGIN_SERVICE_PLUGIN_LOADER (service);
	ErrorInfo *error = NULL;
	GType loader_type;

	GO_INIT_RET_ERROR_INFO (ret_error);
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
plugin_service_plugin_loader_activate (GOPluginService *service, ErrorInfo **ret_error)
{
	gchar *full_id;

	GO_INIT_RET_ERROR_INFO (ret_error);
	full_id = g_strconcat (
		go_plugin_get_id (service->plugin), ":", service->id, NULL);
	go_plugins_register_loader (full_id, service);
	g_free (full_id);
	service->is_active = TRUE;
}

static void
plugin_service_plugin_loader_deactivate (GOPluginService *service, ErrorInfo **ret_error)
{
	gchar *full_id;

	GO_INIT_RET_ERROR_INFO (ret_error);
	full_id = g_strconcat (
		go_plugin_get_id (service->plugin), ":", service->id, NULL);
	go_plugins_unregister_loader (full_id);
	g_free (full_id);
	service->is_active = FALSE;
}

static char *
plugin_service_plugin_loader_get_description (GOPluginService *service)
{
	return g_strdup (_("Plugin loader"));
}

static void
plugin_service_plugin_loader_class_init (GObjectClass *gobject_class)
{
	GOPluginServiceClass *plugin_service_class = GPS_CLASS (gobject_class);

	plugin_service_class->activate = plugin_service_plugin_loader_activate;
	plugin_service_class->deactivate = plugin_service_plugin_loader_deactivate;
	plugin_service_class->get_description = plugin_service_plugin_loader_get_description;
}

GSF_CLASS (PluginServicePluginLoader, plugin_service_plugin_loader,
           plugin_service_plugin_loader_class_init, plugin_service_plugin_loader_init,
           GO_PLUGIN_SERVICE_TYPE)

/**************************************************************************
 * PluginServiceGObjectLoader
 */

static char *
plugin_service_gobject_loader_get_description (GOPluginService *service)
{
	return g_strdup (_("GObject loader"));
}

static void
plugin_service_gobject_loader_read_xml (GOPluginService *service,
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
	GOPluginServiceClass *psc = GPS_CLASS (gobj_loader_class);

	psc->get_description	= plugin_service_gobject_loader_get_description;
	psc->read_xml		= plugin_service_gobject_loader_read_xml;
	gobj_loader_class->pending = NULL;
}

GSF_CLASS (PluginServiceGObjectLoader, plugin_service_gobject_loader,
           plugin_service_gobject_loader_class_init, NULL,
           GO_PLUGIN_SERVICE_SIMPLE_TYPE)

/**************************************************************************
 * PluginServiceSimple
 */

static void
plugin_service_simple_activate (GOPluginService *service, ErrorInfo **ret_error)
{
	service->is_active = TRUE;
}

static void
plugin_service_simple_deactivate (GOPluginService *service, ErrorInfo **ret_error)
{
	service->is_active = FALSE;
}

static void
plugin_service_simple_class_init (GObjectClass *gobject_class)
{
	GOPluginServiceClass *psc = GPS_CLASS (gobject_class);

	psc->activate		= plugin_service_simple_activate;
	psc->deactivate		= plugin_service_simple_deactivate;
}

GSF_CLASS (PluginServiceSimple, plugin_service_simple,
           plugin_service_simple_class_init,
	   NULL,
           GO_PLUGIN_SERVICE_TYPE)

/* ---------------------------------------------------------------------- */

void
plugin_service_load (GOPluginService *service, ErrorInfo **ret_error)
{
	g_return_if_fail (IS_GO_PLUGIN_SERVICE (service));

	GO_INIT_RET_ERROR_INFO (ret_error);

	if (service->is_loaded)
		return;
	go_plugin_load_service (service->plugin, service, ret_error);
	if (*ret_error == NULL)
		service->is_loaded = TRUE;
}

void
plugin_service_unload (GOPluginService *service, ErrorInfo **ret_error)
{
	ErrorInfo *error = NULL;

	g_return_if_fail (IS_GO_PLUGIN_SERVICE (service));

	GO_INIT_RET_ERROR_INFO (ret_error);
	if (!service->is_loaded) {
		return;
	}
	go_plugin_unload_service (service->plugin, service, &error);
	if (error == NULL) {
		service->is_loaded = FALSE;
	} else {
		*ret_error = error;
	}
}

GOPluginService *
plugin_service_new (GOPlugin *plugin, xmlNode *tree, ErrorInfo **ret_error)
{
	GOPluginService *service = NULL;
	char *type_str;
	ErrorInfo *service_error = NULL;
	GOPluginServiceCreate ctor;

	g_return_val_if_fail (IS_GO_PLUGIN (plugin), NULL);
	g_return_val_if_fail (tree != NULL, NULL);
	g_return_val_if_fail (strcmp (tree->name, "service") == 0, NULL);

	GO_INIT_RET_ERROR_INFO (ret_error);
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
plugin_service_get_id (GOPluginService *service)
{
	g_return_val_if_fail (IS_GO_PLUGIN_SERVICE (service), NULL);

	return service->id;
}

char const *
plugin_service_get_description (GOPluginService *service)
{
	g_return_val_if_fail (IS_GO_PLUGIN_SERVICE (service), NULL);

	if (service->saved_description == NULL) {
		service->saved_description = GPS_GET_CLASS (service)->get_description (service);
	}

	return service->saved_description;
}

GOPlugin *
plugin_service_get_plugin (GOPluginService *service)
{
	g_return_val_if_fail (IS_GO_PLUGIN_SERVICE (service), NULL);

	return service->plugin;
}

gpointer
plugin_service_get_cbs (GOPluginService *service)
{
	g_return_val_if_fail (IS_GO_PLUGIN_SERVICE (service), NULL);
	g_return_val_if_fail (service->cbs_ptr != NULL, NULL);

	return service->cbs_ptr;
}

void
plugin_service_activate (GOPluginService *service, ErrorInfo **ret_error)
{
	g_return_if_fail (IS_GO_PLUGIN_SERVICE (service));

	GO_INIT_RET_ERROR_INFO (ret_error);
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
plugin_service_deactivate (GOPluginService *service, ErrorInfo **ret_error)
{
	g_return_if_fail (IS_GO_PLUGIN_SERVICE (service));

	GO_INIT_RET_ERROR_INFO (ret_error);
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
		GOPluginServiceCreate ctor;
	} const builtin_services[] = {
		{ "general",		plugin_service_general_get_type},
		{ "file_opener",	plugin_service_file_opener_get_type},
		{ "file_saver",		plugin_service_file_saver_get_type},
		{ "plugin_loader",	plugin_service_plugin_loader_get_type},
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
plugin_service_define (char const *type_str, GOPluginServiceCreate ctor)
{
	g_return_if_fail (services != NULL);

	g_return_if_fail (NULL == g_hash_table_lookup (services, type_str));

	g_hash_table_insert (services, (gpointer)type_str, ctor);
}
