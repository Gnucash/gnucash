#ifndef GNUMERIC_MODULE_PLUGIN_DEFS_H
#define GNUMERIC_MODULE_PLUGIN_DEFS_H

//#include <gui-gnumeric.h>	/* for wbcg typedef */
// +jsled -- CNP from gui-gnumeric.h
// typedef struct _WorkbookControl		WorkbookControl;
// -jsled
#include <plugin.h>
//#include <func.h> -- not used?
#include <application.h>
// +jsled -- CNP from application.h
//typedef struct _GnmAction GnmAction;
// -jsled

/*
 * Every plugin should put somewhere a line with:
 * GNUMERIC_MODULE_PLUGIN_INFO_DECL;
 */
#define GNUMERIC_MODULE_PLUGIN_INFO_DECL     ModulePluginFileStruct plugin_file_struct = GNUMERIC_MODULE_PLUGIN_FILE_STRUCT_INITIALIZER

/* This type is intended for use with "ui" service.
 * Plugins should define arrays of structs of the form:
 * ModulePluginUIActions <service-id>_actions[] = { ... };
 */
typedef struct {
	char const *name;
	void (*handler) (GnmAction const *action, WorkbookControl *wbc);
} ModulePluginUIActions;

/* function executed to activate "general" service */
void     plugin_init_general (ErrorInfo **ret_error);
/* function executed to deactivate "general" service */
void     plugin_cleanup_general (ErrorInfo **ret_error);

/* optional function executed immediately after loading a plugin */
void      plugin_init (void);
/* optional function executed before unloading a plugin */
void      plugin_cleanup (void);


#ifdef PLUGIN_ID

static GnmPlugin *gnm_get_current_plugin (void)
{
	static GnmPlugin *plugin = NULL;
	if (plugin == NULL) plugin = plugins_get_plugin_by_id (PLUGIN_ID);
	return plugin;
}
#define PLUGIN (gnm_get_current_plugin ())

/* Use this macro for defining types inside plugins */
#define	PLUGIN_CLASS(name, prefix, class_init, instance_init, parent_type) \
GType \
prefix ## _get_type (void) \
{ \
	GType type = 0; \
	if (type == 0) { \
		static GTypeInfo const object_info = { \
			sizeof (name ## Class), \
			(GBaseInitFunc) NULL, \
			(GBaseFinalizeFunc) NULL, \
			(GClassInitFunc) class_init, \
			(GClassFinalizeFunc) NULL, \
			NULL,	/* class_data */ \
			sizeof (name), \
			0,	/* n_preallocs */ \
			(GInstanceInitFunc) instance_init, \
			NULL \
		}; \
		type = g_type_module_register_type ( \
			G_TYPE_MODULE (gnm_get_current_plugin ()), parent_type, #name, \
			&object_info, 0); \
	} \
	return type; \
}

#endif


/* All fields in this structure are PRIVATE. */
typedef struct {
	guint32 magic_number;
	gchar   version[64];
} ModulePluginFileStruct;

// +jsled -- @@fixme; gnucash version, I guess.
#define GNUMERIC_VERSION "9.8.7"
// -jsled
#define GNUMERIC_MODULE_PLUGIN_MAGIC_NUMBER             0x476e756d
#define GNUMERIC_MODULE_PLUGIN_FILE_STRUCT_INITIALIZER  {GNUMERIC_MODULE_PLUGIN_MAGIC_NUMBER, GNUMERIC_VERSION}

#endif /* GNUMERIC_MODULE_PLUGIN_DEFS_H */
