#ifndef GOFFICE_MODULE_PLUGIN_DEFS_H
#define GOFFICE_MODULE_PLUGIN_DEFS_H

#include <goffice/app/go-plugin.h>
#include <goffice/app/goffice-app.h>
#include <gmodule.h>

G_BEGIN_DECLS

void go_plugin_init	(GOPlugin *p, GOCmdContext *cc); /* optional, called after dlopen */
void go_plugin_shutdown	(GOPlugin *p, GOCmdContext *cc); /* optional, called before close */

typedef struct {
	char const * const key;		/* object being versioned */
	char const * const version;	/* version id (strict equality is required) */
} GOPluginModuleDepend;
typedef struct {
	guint32 const magic_number;
	guint32 const num_depends;
} GOPluginModuleHeader;

/* Cheesy api versioning
 * bump this when external api changes.  eventually we will just push this out
 * into the module's link dependencies */
#define GOFFICE_API_VERSION		"0.0"

#define GOFFICE_MODULE_PLUGIN_MAGIC_NUMBER             0x476e756d

/* convenience header for goffice plugins */
#define GOFFICE_PLUGIN_MODULE_HEADER 					\
G_MODULE_EXPORT GOPluginModuleDepend const go_plugin_depends [] = {	\
    { "goffice", GOFFICE_API_VERSION }					\
};	\
G_MODULE_EXPORT GOPluginModuleHeader const go_plugin_header =  		\
	{ GOFFICE_MODULE_PLUGIN_MAGIC_NUMBER, G_N_ELEMENTS (go_plugin_depends) }

G_END_DECLS

#endif /* GOFFICE_MODULE_PLUGIN_DEFS_H */
