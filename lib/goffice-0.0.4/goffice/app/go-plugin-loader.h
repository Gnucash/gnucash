#ifndef GO_PLUGIN_LOADER_H
#define GO_PLUGIN_LOADER_H

#include <glib.h>
#include <glib-object.h>
#include <libxml/tree.h>
#include <goffice/app/go-plugin.h>
#include <goffice/app/goffice-app.h>

G_BEGIN_DECLS

#define GO_PLUGIN_LOADER_TYPE		(go_plugin_loader_get_type ())
#define GO_PLUGIN_LOADER(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GO_PLUGIN_LOADER_TYPE, GOPluginLoader))
#define IS_GO_PLUGIN_LOADER(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_PLUGIN_LOADER_TYPE))
#define GO_PLUGIN_LOADER_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GO_PLUGIN_LOADER_TYPE, GOPluginLoaderClass))
#define IS_GO_PLUGIN_LOADER_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GO_PLUGIN_LOADER_TYPE))

typedef struct {
	GTypeInterface base;

	void (*load_base)		(GOPluginLoader *l, ErrorInfo **err);
	void (*unload_base)		(GOPluginLoader *l, ErrorInfo **err);
	void (*set_attributes)		(GOPluginLoader *l, GHashTable *attrs, ErrorInfo **err);
	gboolean (*service_load)	(GOPluginLoader *l, GOPluginService *s, ErrorInfo **err);
	gboolean (*service_unload)	(GOPluginLoader *l, GOPluginService *s, ErrorInfo **err);

	void (*load_service_file_opener)	(GOPluginLoader *l, GOPluginService *s, ErrorInfo **err);
	void (*unload_service_file_opener)	(GOPluginLoader *l, GOPluginService *s, ErrorInfo **err);

	void (*load_service_file_saver)		(GOPluginLoader *l, GOPluginService *s, ErrorInfo **err);
	void (*unload_service_file_saver)	(GOPluginLoader *l, GOPluginService *s, ErrorInfo **err);

	void (*load_service_plugin_loader)	(GOPluginLoader *l, GOPluginService *s, ErrorInfo **err);
	void (*unload_service_plugin_loader)	(GOPluginLoader *l, GOPluginService *s, ErrorInfo **err);
} GOPluginLoaderClass;

GType	   go_plugin_loader_get_type (void);
void	   go_plugin_loader_set_attributes (GOPluginLoader *l, GHashTable *attrs,
					    ErrorInfo **err);
GOPlugin *go_plugin_loader_get_plugin	   (GOPluginLoader *l);
void	   go_plugin_loader_set_plugin	   (GOPluginLoader *l, GOPlugin *p);
void	   go_plugin_loader_load_base	   (GOPluginLoader *l, ErrorInfo **err);
void	   go_plugin_loader_unload_base	   (GOPluginLoader *l, ErrorInfo **err);
void	   go_plugin_loader_load_service   (GOPluginLoader *l, GOPluginService *s, ErrorInfo **err);
void	   go_plugin_loader_unload_service (GOPluginLoader *l, GOPluginService *s, ErrorInfo **err);
gboolean   go_plugin_loader_is_base_loaded (GOPluginLoader *l);

G_END_DECLS

#endif /* GO_PLUGIN_LOADER_H */
