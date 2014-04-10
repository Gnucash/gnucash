#ifndef PLUGIN_SERVICE_H
#define PLUGIN_SERVICE_H

#include <goffice/app/goffice-app.h>
#include <goffice/app/go-plugin.h>
#include <glib.h>
#include <gmodule.h>
#include <libxml/tree.h>
#include <gsf/gsf.h>

G_BEGIN_DECLS

#define GO_PLUGIN_SERVICE_TYPE         (plugin_service_get_type ())
#define GO_PLUGIN_SERVICE(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_PLUGIN_SERVICE_TYPE, GOPluginService))
#define IS_GO_PLUGIN_SERVICE(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_PLUGIN_SERVICE_TYPE))

GType plugin_service_get_type (void);

#define GO_PLUGIN_SERVICE_GENERAL_TYPE  (plugin_service_general_get_type ())
#define GO_PLUGIN_SERVICE_GENERAL(o)    (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_PLUGIN_SERVICE_GENERAL_TYPE, PluginServiceGeneral))
#define IS_GO_PLUGIN_SERVICE_GENERAL(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_PLUGIN_SERVICE_GENERAL_TYPE))

GType plugin_service_general_get_type (void);
typedef struct _PluginServiceGeneral PluginServiceGeneral;
typedef struct {
	void (*plugin_func_init) (GOPluginService *service, ErrorInfo **ret_error);
	void (*plugin_func_cleanup) (GOPluginService *service, ErrorInfo **ret_error);
} PluginServiceGeneralCallbacks;

#define GO_PLUGIN_SERVICE_FILE_OPENER_TYPE  (plugin_service_file_opener_get_type ())
#define GO_PLUGIN_SERVICE_FILE_OPENER(o)    (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_PLUGIN_SERVICE_FILE_OPENER_TYPE, PluginServiceFileOpener))
#define IS_GO_PLUGIN_SERVICE_FILE_OPENER(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_PLUGIN_SERVICE_FILE_OPENER_TYPE))

GType plugin_service_file_opener_get_type (void);
typedef struct _PluginServiceFileOpener PluginServiceFileOpener;
typedef struct {
	/* plugin_func_file_probe may be NULL */
	gboolean (*plugin_func_file_probe) (
	         GOFileOpener const *fo, GOPluginService *service,
	         GsfInput *input, FileProbeLevel pl);
	void     (*plugin_func_file_open) (
	         GOFileOpener const *fo, GOPluginService *service,
	         IOContext *io_context, gpointer fixme_workbook_view,
		 GsfInput *input);
} PluginServiceFileOpenerCallbacks;


#define GO_PLUGIN_SERVICE_FILE_SAVER_TYPE  (plugin_service_file_saver_get_type ())
#define GO_PLUGIN_SERVICE_FILE_SAVER(o)    (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_PLUGIN_SERVICE_FILE_SAVER_TYPE, PluginServiceFileSaver))
#define IS_GO_PLUGIN_SERVICE_FILE_SAVER(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_PLUGIN_SERVICE_FILE_SAVER_TYPE))

GType plugin_service_file_saver_get_type (void);
typedef struct _PluginServiceFileSaver PluginServiceFileSaver;
typedef struct {
	void  (*plugin_func_file_save) (
	      GOFileSaver const *fs, GOPluginService *service,
	      IOContext *io_context, gconstpointer fixme_workbook_view,
	      GsfOutput *output);
} PluginServiceFileSaverCallbacks;

#define GO_PLUGIN_SERVICE_PLUGIN_LOADER_TYPE  (plugin_service_plugin_loader_get_type ())
#define GO_PLUGIN_SERVICE_PLUGIN_LOADER(o)    (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_PLUGIN_SERVICE_PLUGIN_LOADER_TYPE, PluginServicePluginLoader))
#define IS_GO_PLUGIN_SERVICE_PLUGIN_LOADER(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_PLUGIN_SERVICE_PLUGIN_LOADER_TYPE))

GType plugin_service_plugin_loader_get_type (void);
typedef struct _PluginServicePluginLoader PluginServicePluginLoader;
typedef struct {
	GType (*plugin_func_get_loader_type) (
	      GOPluginService *service, ErrorInfo **ret_error);
} PluginServicePluginLoaderCallbacks;

GType plugin_service_plugin_loader_generate_type (GOPluginService *service,
                                                  ErrorInfo **ret_error);

/****************************************************************************/

#define GO_PLUGIN_SERVICE_GOBJECT_LOADER_TYPE  (plugin_service_gobject_loader_get_type ())
#define GO_PLUGIN_SERVICE_GOBJECT_LOADER(o)    (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_PLUGIN_SERVICE_GOBJECT_LOADER_TYPE, PluginServiceGObjectLoader))
#define IS_GO_PLUGIN_SERVICE_GOBJECT_LOADER(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_PLUGIN_SERVICE_GOBJECT_LOADER_TYPE))

GType plugin_service_gobject_loader_get_type (void);
typedef struct _PluginServiceGObjectLoader PluginServiceGObjectLoader;

/****************************************************************************/
#define GO_PLUGIN_SERVICE_SIMPLE_TYPE  (plugin_service_simple_get_type ())
#define GO_PLUGIN_SERVICE_SIMPLE(o)    (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_PLUGIN_SERVICE_SIMPLE_TYPE, PluginServiceSimple))
#define IS_GO_PLUGIN_SERVICE_SIMPLE(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_PLUGIN_SERVICE_SIMPLE_TYPE))

GType plugin_service_simple_get_type (void);
typedef struct _PluginServiceSimple PluginServiceSimple;

/****************************************************************************/

GOPluginService  *plugin_service_new (GOPlugin *plugin, xmlNode *tree, ErrorInfo **ret_error);
char const     *plugin_service_get_id (GOPluginService *service);
char const     *plugin_service_get_description (GOPluginService *service);
GOPlugin      *plugin_service_get_plugin (GOPluginService *service);
gpointer	plugin_service_get_cbs (GOPluginService *service);
void		plugin_service_activate (GOPluginService *service, ErrorInfo **ret_error);
void		plugin_service_deactivate (GOPluginService *service, ErrorInfo **ret_error);
void		plugin_service_load   (GOPluginService *service, ErrorInfo **ret_error);
void		plugin_service_unload (GOPluginService *service, ErrorInfo **ret_error);

typedef GType (*GOPluginServiceCreate) (void);
void plugin_services_init     (void);
void plugin_services_shutdown (void);
void plugin_service_define    (char const *type_str,
			       GOPluginServiceCreate ctor);

G_END_DECLS

#endif /* PLUGIN_SERVICE_H */
