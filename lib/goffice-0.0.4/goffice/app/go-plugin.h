#ifndef GO_PLUGIN_H
#define GO_PLUGIN_H

#include <goffice/app/goffice-app.h>
#include <glib-object.h>

G_BEGIN_DECLS

/*
 * Use "#define PLUGIN_DEBUG x" to enable some plugin related debugging
 * messages.
#undef PLUGIN_DEBUG
 * Define PLUGIN_ALWAYS_LOAD to disable loading on demand feature
 */

#define GO_PLUGIN_TYPE	(go_plugin_get_type ())
#define GO_PLUGIN(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GO_PLUGIN_TYPE, GOPlugin))
#define IS_GO_PLUGIN(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_PLUGIN_TYPE))

GType	go_plugin_get_type (void);

void         go_plugin_activate (GOPlugin *plugin, ErrorInfo **ret_error);
void         go_plugin_deactivate (GOPlugin *plugin, ErrorInfo **ret_error);
gboolean     go_plugin_is_active (GOPlugin *plugin);
gboolean     go_plugin_can_deactivate (GOPlugin *plugin);
void         go_plugin_load_service (GOPlugin *plugin, GOPluginService *service, ErrorInfo **ret_error);
void         go_plugin_unload_service (GOPlugin *plugin, GOPluginService *service, ErrorInfo **ret_error);
gboolean     go_plugin_is_loaded (GOPlugin *plugin);
void         go_plugin_use_ref (GOPlugin *plugin);
void         go_plugin_use_unref (GOPlugin *plugin);

GTypeModule *go_plugin_get_type_module	(GOPlugin *plugin);
char const  *go_plugin_get_dir_name	(GOPlugin *plugin);
char const  *go_plugin_get_id		(GOPlugin *plugin);
char const  *go_plugin_get_name		(GOPlugin *plugin);
char const  *go_plugin_get_description	(GOPlugin *plugin);
char const  *go_plugin_get_textdomain	(GOPlugin *plugin);
GSList      *go_plugin_get_services	(GOPlugin *plugin);
GSList      *go_plugin_get_dependencies_ids (GOPlugin *plugin);

/*
 *
 */
void	go_plugins_init	    (GOCmdContext *context,
			     GSList const *known_states,
			     GSList const *active_plugins,
			     GSList *plugin_dirs,
			     gboolean activate_new_plugins,
			     GType  default_loader_type);
GSList *go_plugins_shutdown (void);

void	  go_plugins_register_loader (const gchar *id_str, GOPluginService *service);
void	  go_plugins_unregister_loader (const gchar *id_str);
GOPlugin *go_plugins_get_plugin_by_id (const gchar *plugin_id);
GSList	 *go_plugins_get_available_plugins (void);
GSList	 *go_plugins_get_active_plugins (void);
void	  go_plugins_rescan (ErrorInfo **ret_error, GSList **ret_new_plugins);
char 	 *go_plugins_get_plugin_dir (void);

void	  go_plugin_db_mark_plugin_for_deactivation (GOPlugin *plugin, gboolean mark);
gboolean  go_plugin_db_is_plugin_marked_for_deactivation (GOPlugin *plugin);
void	  go_plugin_db_activate_plugin_list   (GSList *plugins, ErrorInfo **ret_error);
void	  go_plugin_db_deactivate_plugin_list (GSList *plugins, ErrorInfo **ret_error);

G_END_DECLS

#endif /* GO_PLUGIN_H */
