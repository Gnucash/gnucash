/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-plot-engine.c :
 *
 * Copyright (C) 2003-2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

#include <goffice/goffice-config.h>
#include <goffice/graph/gog-plot-engine.h>
#include <goffice/graph/gog-plot-impl.h>
#include <goffice/graph/gog-theme.h>
#include <glib/gi18n.h>
#include <xml-io.h>

#include <gsf/gsf-impl-utils.h>

#include <string.h>

static GSList *refd_plugins;

/***************************************************************************/
/* Support plot engines in plugins */

#include <plugin-service-impl.h>

#define GOG_PLOT_ENGINE_SERVICE_TYPE  (gog_plot_engine_service_get_type ())
#define GOG_PLOT_ENGINE_SERVICE(o)    (G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_PLOT_ENGINE_SERVICE_TYPE, GogPlotEngineService))
#define IS_GOG_PLOT_ENGINE_SERVICE(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_PLOT_ENGINE_SERVICE_TYPE))

static GType gog_plot_engine_service_get_type (void);

typedef PluginServiceGObjectLoader	GogPlotEngineService;
typedef PluginServiceGObjectLoaderClass GogPlotEngineServiceClass;

static GHashTable *pending_engines = NULL;

static char *
gog_plot_engine_service_get_description (GnmPluginService *service)
{
	return g_strdup (_("Plot Engine"));
}

static void
gog_plot_engine_service_class_init (PluginServiceGObjectLoaderClass *gobj_loader_class)
{
	GnmPluginServiceClass *ps_class = GPS_CLASS (gobj_loader_class);

	ps_class->get_description = gog_plot_engine_service_get_description;

	gobj_loader_class->pending =
		pending_engines = g_hash_table_new (g_str_hash, g_str_equal);
}

GSF_CLASS (GogPlotEngineService, gog_plot_engine_service,
           gog_plot_engine_service_class_init, NULL,
           GNM_PLUGIN_SERVICE_GOBJECT_LOADER_TYPE)

GogPlot *
gog_plot_new_by_name (char const *id)
{
	GType type = g_type_from_name (id);

	if (type == 0) {
		ErrorInfo *err = NULL;
		GnmPluginService *service =
			pending_engines
			? g_hash_table_lookup (pending_engines, id)
			: NULL;
		GnmPlugin *plugin;

		if (!service || !service->is_active)
			return NULL;

		g_return_val_if_fail (!service->is_loaded, NULL);

		plugin_service_load (service, &err);
		type = g_type_from_name (id);

		if (err != NULL) {
			error_info_print (err);
			error_info_free	(err);
		}

		g_return_val_if_fail (type != 0, NULL);

		/*
		 * The plugin defined a gtype so it must not be unloaded.
		 */
		plugin = plugin_service_get_plugin (service);
		refd_plugins = g_slist_prepend (refd_plugins, plugin);
		g_object_ref (plugin);
		gnm_plugin_use_ref (plugin);
	}

	return g_object_new (type, NULL);
}

/***************************************************************************/
/* Use a plugin service to define where to find plot types */

#define GOG_PLOT_TYPE_SERVICE_TYPE  (gog_plot_type_service_get_type ())
#define GOG_PLOT_TYPE_SERVICE(o)    (G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_PLOT_TYPE_SERVICE_TYPE, GogPlotTypeService))
#define IS_GOG_PLOT_TYPE_SERVICE(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_PLOT_TYPE_SERVICE_TYPE))

GType gog_plot_type_service_get_type (void);

typedef struct {
	PluginServiceSimple	base;

	GSList	*families, *types;
} GogPlotTypeService;

typedef struct{
	PluginServiceSimpleClass	base;
} GogPlotTypeServiceClass;

static GHashTable *pending_plot_type_files = NULL;
static GObjectClass *plot_type_parent_klass;

static void
cb_pending_plot_types_load (char const *path,
			    GogPlotTypeService *service,
			    G_GNUC_UNUSED gpointer ignored)
{
	xmlNode *ptr, *prop;
	xmlDoc *doc = xmlParseFile (path);
	GogPlotFamily *family = NULL;
	GogPlotType *type;
	int col, row;
	xmlChar *name, *image_file, *description, *engine;

	g_return_if_fail (doc != NULL && doc->xmlRootNode != NULL);

	/* do the families before the types so that they are available */
	for (ptr = doc->xmlRootNode->xmlChildrenNode; ptr ; ptr = ptr->next)
		if (!xmlIsBlankNode (ptr) && ptr->name && !strcmp (ptr->name, "Family")) {
			name	    = xmlGetProp (ptr, "_name");
			image_file  = xmlGetProp (ptr, "sample_image_file");
			family = gog_plot_family_register (name, image_file);
			if (family != NULL)
				service->families = g_slist_prepend (service->families, family);
			if (name != NULL) xmlFree (name);
			if (image_file != NULL) xmlFree (image_file);
		}

	for (ptr = doc->xmlRootNode->xmlChildrenNode; ptr ; ptr = ptr->next)
		if (!xmlIsBlankNode (ptr) && ptr->name && !strcmp (ptr->name, "Type")) {
			name   = xmlGetProp (ptr, "family");
			if (name != NULL) {
				family = gog_plot_family_by_name  (name);
				xmlFree (name);
				if (family == NULL)
					continue;
			}

			name	    = xmlGetProp (ptr, "_name");
			image_file  = xmlGetProp (ptr, "sample_image_file");
			description = xmlGetProp (ptr, "_description");
			engine	    = xmlGetProp (ptr, "engine");
			if (xml_node_get_int (ptr, "col", &col) &&
			    xml_node_get_int (ptr, "row", &row)) {
				type = gog_plot_type_register (family, col, row,
					name, image_file, description, engine);
				if (type != NULL) {
					service->types = g_slist_prepend (service->types, type);
					for (prop = ptr->xmlChildrenNode ; prop != NULL ; prop = prop->next)
						if (!xmlIsBlankNode (prop) &&
						    prop->name && !strcmp (prop->name, "property")) {
							xmlChar *prop_name = xmlGetProp (prop, "name");

							if (prop_name == NULL) {
								g_warning ("missing name for property entry");
								continue;
							}

							if (type->properties == NULL)
								type->properties = g_hash_table_new_full (g_str_hash, g_str_equal,
													  xmlFree, xmlFree);
							g_hash_table_replace (type->properties,
								prop_name, xmlNodeGetContent (prop));
						}
				}
			}
			if (name != NULL) xmlFree (name);
			if (image_file != NULL) xmlFree (image_file);
			if (description != NULL) xmlFree (description);
			if (engine != NULL) xmlFree (engine);
		}

	xmlFreeDoc (doc);
}

static void
pending_plot_types_load (void)
{
	if (pending_plot_type_files != NULL) {
		GHashTable *tmp = pending_plot_type_files;
		pending_plot_type_files = NULL;
		g_hash_table_foreach (tmp,
			(GHFunc) cb_pending_plot_types_load, NULL);
		g_hash_table_destroy (tmp);
	}
}

static void
gog_plot_type_service_read_xml (GnmPluginService *service, xmlNode *tree, ErrorInfo **ret_error)
{
	char    *path;
	xmlNode *ptr;

	for (ptr = tree->xmlChildrenNode; ptr != NULL; ptr = ptr->next)
		if (0 == xmlStrcmp (ptr->name, "file") &&
		    NULL != (path = xmlNodeGetContent (ptr))) {
			if (!g_path_is_absolute (path)) {
				char const *dir = gnm_plugin_get_dir_name (
					plugin_service_get_plugin (service));
				char *tmp = g_build_filename (dir, path, NULL);
				g_free (path);
				path = tmp;
			}
			if (pending_plot_type_files == NULL)
				pending_plot_type_files = g_hash_table_new_full (
					g_str_hash, g_str_equal, g_free, g_object_unref);
			g_object_ref (service);
			g_hash_table_replace (pending_plot_type_files, path, service);
		}
}

static char *
gog_plot_type_service_get_description (GnmPluginService *service)
{
	return g_strdup (_("Plot Type"));
}

static void
gog_plot_type_service_finalize (GObject *obj)
{
	GogPlotTypeService *service = GOG_PLOT_TYPE_SERVICE (obj);
	GSList *ptr;

	for (ptr = service->families ; ptr != NULL ; ptr = ptr->next) {
	}
	g_slist_free (service->families);
	service->families = NULL;

	for (ptr = service->types ; ptr != NULL ; ptr = ptr->next) {
	}
	g_slist_free (service->types);
	service->types = NULL;

	(plot_type_parent_klass->finalize) (obj);
}

static void
gog_plot_type_service_init (GObject *obj)
{
	GogPlotTypeService *service = GOG_PLOT_TYPE_SERVICE (obj);

	service->families = NULL;
	service->types = NULL;
}

static void
gog_plot_type_service_class_init (GObjectClass *gobject_klass)
{
	GnmPluginServiceClass *ps_class = GPS_CLASS (gobject_klass);

	plot_type_parent_klass = g_type_class_peek_parent (gobject_klass);
	gobject_klass->finalize		= gog_plot_type_service_finalize;
	ps_class->read_xml		= gog_plot_type_service_read_xml;
	ps_class->get_description	= gog_plot_type_service_get_description;
}

GSF_CLASS (GogPlotTypeService, gog_plot_type_service,
           gog_plot_type_service_class_init, gog_plot_type_service_init,
           GNM_PLUGIN_SERVICE_SIMPLE_TYPE)

/***************************************************************************/
/* Use a plugin service to define themes */

#define GOG_THEME_SERVICE_TYPE  (gog_theme_service_get_type ())
#define GOG_THEME_SERVICE(o)    (G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_THEME_SERVICE_TYPE, GogThemeService))
#define IS_GOG_THEME_SERVICE(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_THEME_SERVICE_TYPE))

GType gog_theme_service_get_type (void);

typedef PluginServiceSimple GogThemeService;
typedef PluginServiceSimpleClass GogThemeServiceClass;

static void
gog_theme_service_read_xml (GnmPluginService *service, xmlNode *tree, ErrorInfo **ret_error)
{
	char    *path;
	xmlNode *ptr;

	for (ptr = tree->xmlChildrenNode; ptr != NULL; ptr = ptr->next)
		if (0 == xmlStrcmp (ptr->name, "file") &&
		    NULL != (path = xmlNodeGetContent (ptr))) {
			if (!g_path_is_absolute (path)) {
				char const *dir = gnm_plugin_get_dir_name (
					plugin_service_get_plugin (service));
				char *tmp = g_build_filename (dir, path, NULL);
				g_free (path);
				path = tmp;
			}
			gog_theme_register_file (
				plugin_service_get_description (service), path);
		}
}

static char *
gog_theme_service_get_description (GnmPluginService *service)
{
	return g_strdup (_("Chart Theme"));
}

static void
gog_theme_service_class_init (GnmPluginServiceClass *ps_class)
{
	ps_class->read_xml	  = gog_theme_service_read_xml;
	ps_class->get_description = gog_theme_service_get_description;
}

GSF_CLASS (GogThemeService, gog_theme_service,
           gog_theme_service_class_init, NULL,
           GNM_PLUGIN_SERVICE_SIMPLE_TYPE)

/***************************************************************************/

void
gog_plugin_services_init (void)
{
	plugin_service_define ("plot_engine", &gog_plot_engine_service_get_type);
	plugin_service_define ("plot_type",   &gog_plot_type_service_get_type);
	plugin_service_define ("chart_theme",  &gog_theme_service_get_type);
}

void
gog_plugin_services_shutdown (void)
{
	g_slist_foreach (refd_plugins, (GFunc)gnm_plugin_use_unref, NULL);
	g_slist_foreach (refd_plugins, (GFunc)g_object_unref, NULL);
	g_slist_free (refd_plugins);
}

/***************************************************************************/

static GHashTable *plot_families = NULL;

static void
gog_plot_family_free (GogPlotFamily *family)
{
	g_free (family->name);			family->name = NULL;
	g_free (family->sample_image_file);	family->sample_image_file = NULL;
	if (family->types) {
		g_hash_table_destroy (family->types);
		family->types = NULL;
	}
	g_free (family);
}

static void
gog_plot_type_free (GogPlotType *type)
{
	g_free (type->name);
	g_free (type->sample_image_file);
	g_free (type->description);
	g_free (type->engine);
	g_free (type);
}

static void
create_plot_families (void)
{
	if (!plot_families)
		plot_families = g_hash_table_new_full
			(g_str_hash, g_str_equal,
			 NULL, (GDestroyNotify) gog_plot_family_free);
}

GHashTable const *
gog_plot_families (void)
{
	create_plot_families ();
	pending_plot_types_load ();
	return plot_families;
}
GogPlotFamily *
gog_plot_family_by_name (char const *name)
{
	create_plot_families ();
	pending_plot_types_load ();
	return g_hash_table_lookup (plot_families, name);
}

GogPlotFamily *
gog_plot_family_register (char const *name, char const *sample_image_file)
{
	GogPlotFamily *res;

	g_return_val_if_fail (name != NULL, NULL);
	g_return_val_if_fail (sample_image_file != NULL, NULL);

	create_plot_families ();
	g_return_val_if_fail (g_hash_table_lookup (plot_families, name) == NULL, NULL);

	res = g_new0 (GogPlotFamily, 1);
	res->name	       = g_strdup (name);
	res->sample_image_file = g_strdup (sample_image_file);
	res->types = g_hash_table_new_full (g_str_hash, g_str_equal,
		NULL, (GDestroyNotify) gog_plot_type_free);
	g_hash_table_insert (plot_families, res->name, res);

	return res;
}

GogPlotType *
gog_plot_type_register (GogPlotFamily *family, int col, int row,
		       char const *name, char const *sample_image_file,
		       char const *description, char const *engine)
{
	GogPlotType *res;
	
	g_return_val_if_fail (family != NULL, NULL);
	
	res = g_new0 (GogPlotType, 1);
	res->name = g_strdup (name);
	res->sample_image_file = g_strdup (sample_image_file);
	res->description = g_strdup (description);
	res->engine = g_strdup (engine);

	res->col = col;
	res->row = row;
	res->family = family;
	g_hash_table_replace (family->types, res->name, res);

	return res;
}

