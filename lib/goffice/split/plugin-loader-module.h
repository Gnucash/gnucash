#ifndef GNUMERIC_PLUGIN_LOADER_MODULE_H
#define GNUMERIC_PLUGIN_LOADER_MODULE_H

#include <glib-object.h>

#define TYPE_GNM_PLUGIN_LOADER_MODULE            (gnm_plugin_loader_module_get_type ())
#define GNM_PLUGIN_LOADER_MODULE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_GNM_PLUGIN_LOADER_MODULE, GnmPluginLoaderModule))
#define GNM_PLUGIN_LOADER_MODULE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_GNM_PLUGIN_LOADER_MODULE, GnmPluginLoaderModuleClass))
#define IS_GNM_PLUGIN_LOADER_MODULE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_GNM_PLUGIN_LOADER_MODULE))
#define IS_GNM_PLUGIN_LOADER_MODULE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), TYPE_GNM_PLUGIN_LOADER_MODULE))

typedef struct _GnmPluginLoaderModule GnmPluginLoaderModule;
typedef struct _GnmPluginLoaderModuleClass GnmPluginLoaderModuleClass;

GType gnm_plugin_loader_module_get_type (void);

#endif /* GNUMERIC_PLUGIN_LOADER_MODULE_H */
