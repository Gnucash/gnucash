#ifndef GNUMERIC_PLUGIN_H
#define GNUMERIC_PLUGIN_H

#include "gnumeric.h"
#include <glib-object.h>

/*
 * Use "#define PLUGIN_DEBUG x" to enable some plugin related debugging
 * messages.
#undef PLUGIN_DEBUG
 * Define PLUGIN_ALWAYS_LOAD to disable loading on demand feature
 */

#define GNM_PLUGIN_TYPE        (gnm_plugin_get_type ())
#define GNM_PLUGIN(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), GNM_PLUGIN_TYPE, GnmPlugin))
#define IS_GNM_PLUGIN(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNM_PLUGIN_TYPE))

GType gnm_plugin_get_type (void);

void         gnm_plugin_activate (GnmPlugin *pinfo, ErrorInfo **ret_error);
void         gnm_plugin_deactivate (GnmPlugin *pinfo, ErrorInfo **ret_error);
gboolean     gnm_plugin_is_active (GnmPlugin *pinfo);
gboolean     gnm_plugin_can_deactivate (GnmPlugin *pinfo);
void         gnm_plugin_load_service (GnmPlugin *pinfo, GnmPluginService *service, ErrorInfo **ret_error);
void         gnm_plugin_unload_service (GnmPlugin *pinfo, GnmPluginService *service, ErrorInfo **ret_error);
gboolean     gnm_plugin_is_loaded (GnmPlugin *pinfo);
void         gnm_plugin_use_ref (GnmPlugin *pinfo);
void         gnm_plugin_use_unref (GnmPlugin *pinfo);

char const  *gnm_plugin_get_dir_name (GnmPlugin *pinfo);
char const  *gnm_plugin_get_id (GnmPlugin *pinfo);
char const  *gnm_plugin_get_name (GnmPlugin *pinfo);
char const  *gnm_plugin_get_description (GnmPlugin *pinfo);
char const  *gnm_plugin_get_textdomain (GnmPlugin *pinfo);
GSList      *gnm_plugin_get_dependencies_ids (GnmPlugin *pinfo);
GSList      *gnm_plugin_get_services (GnmPlugin *pinfo);

/*
 *
 */

void         plugins_init (GnmCmdContext *context);
void         plugins_shutdown (void);
void         plugins_register_loader (const gchar *id_str, GnmPluginService *service);
void         plugins_unregister_loader (const gchar *id_str);
GnmPlugin   *plugins_get_plugin_by_id (const gchar *plugin_id);
GSList      *plugins_get_available_plugins (void);
void         plugins_rescan (ErrorInfo **ret_error, GSList **ret_new_plugins);
void         plugin_db_mark_plugin_for_deactivation (GnmPlugin *pinfo, gboolean mark);
gboolean     plugin_db_is_plugin_marked_for_deactivation (GnmPlugin *pinfo);
void         plugin_db_activate_plugin_list (GSList *plugins, ErrorInfo **ret_error);
void         plugin_db_deactivate_plugin_list (GSList *plugins, ErrorInfo **ret_error);

void plugin_message (gint level, const gchar *format, ...) G_GNUC_PRINTF (2, 3);

#endif /* GNUMERIC_PLUGIN_H */
