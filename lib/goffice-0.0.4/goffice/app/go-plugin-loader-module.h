#ifndef GO_PLUGIN_LOADER_MODULE_H
#define GO_PLUGIN_LOADER_MODULE_H

#include <goffice/app/goffice-app.h>
#include <glib-object.h>
#include <gmodule.h>

G_BEGIN_DECLS

#define GO_PLUGIN_LOADER_MODULE_TYPE		(go_plugin_loader_module_get_type ())
#define GO_PLUGIN_LOADER_MODULE(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GO_PLUGIN_LOADER_MODULE_TYPE, GOPluginLoaderModule))
#define IS_GO_PLUGIN_LOADER_MODULE(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_PLUGIN_LOADER_MODULE_TYPE))
#define GO_PLUGIN_LOADER_MODULE_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GO_PLUGIN_LOADER_MODULE_TYPE, GOPluginLoaderModuleClass))
#define IS_GO_PLUGIN_LOADER_MODULE_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GO_PLUGIN_LOADER_MODULE_TYPE))

typedef struct {
	GObject	base;

	gchar *module_file_name;
	GModule *handle;

	void (*plugin_init)	(GOPlugin *plugin, GOCmdContext *cc);
	void (*plugin_shutdown) (GOPlugin *plugin, GOCmdContext *cc);
} GOPluginLoaderModule;
typedef GObjectClass GOPluginLoaderModuleClass;

GType go_plugin_loader_module_get_type (void);

void go_plugin_loader_module_register_version (char const *id, char const *ver);

G_END_DECLS

#endif /* GO_PLUGIN_LOADER_MODULE_H */
