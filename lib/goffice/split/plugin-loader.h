#ifndef GNUMERIC_PLUGIN_LOADER_H
#define GNUMERIC_PLUGIN_LOADER_H

#include <glib.h>
#include <glib-object.h>
#include <libxml/tree.h>
#include "gnumeric.h"
#include "error-info.h"
#include "plugin.h"

#define TYPE_GNM_PLUGIN_LOADER             (gnm_plugin_loader_get_type ())
#define GNM_PLUGIN_LOADER(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_GNM_PLUGIN_LOADER, GnmPluginLoader))
#define GNM_PLUGIN_LOADER_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_GNM_PLUGIN_LOADER, GnmPluginLoaderClass))
#define IS_GNM_PLUGIN_LOADER(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_GNM_PLUGIN_LOADER))
#define IS_GNM_PLUGIN_LOADER_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), TYPE_GNM_PLUGIN_LOADER))

typedef struct _GnmPluginLoaderClass GnmPluginLoaderClass;

struct _GnmPluginLoader {
	GObject object;

	GnmPlugin *plugin;
	gboolean   is_base_loaded;
	gint	   n_loaded_services;
};

struct _GnmPluginLoaderClass {
	GObjectClass parent_class;

	void (*set_attributes) (GnmPluginLoader *loader, GHashTable *attrs, ErrorInfo **ret_error);
	void (*load_base) (GnmPluginLoader *loader, ErrorInfo **ret_error);
	void (*unload_base) (GnmPluginLoader *loader, ErrorInfo **ret_error);
	void (*load_service_general) (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
	void (*unload_service_general) (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
  //void (*load_service_file_opener) (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
  //void (*unload_service_file_opener) (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
  //void (*load_service_file_saver) (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
  //void (*unload_service_file_saver) (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
  //	void (*load_service_function_group) (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
  //	void (*unload_service_function_group) (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
	void (*load_service_plugin_loader) (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
	void (*unload_service_plugin_loader) (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
	void (*load_service_ui) (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
	void (*unload_service_ui) (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
};

GType gnm_plugin_loader_get_type (void);

void gnm_plugin_loader_set_attributes (GnmPluginLoader *loader,
                                            GHashTable *attrs,
                                            ErrorInfo **ret_error);
void gnm_plugin_loader_set_plugin (GnmPluginLoader *loader, GnmPlugin *plugin);
void gnm_plugin_loader_load_base (GnmPluginLoader *loader, ErrorInfo **ret_error);
void gnm_plugin_loader_unload_base (GnmPluginLoader *loader, ErrorInfo **ret_error);
void gnm_plugin_loader_load_service (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
void gnm_plugin_loader_unload_service (GnmPluginLoader *loader, GnmPluginService *service, ErrorInfo **ret_error);
gboolean gnm_plugin_loader_is_base_loaded (GnmPluginLoader *loader);

#endif /* GNUMERIC_PLUGIN_LOADER_H */
