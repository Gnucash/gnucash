/*************************************************************
 * gnc-module.h -- loadable plugin/module system for gnucash
 * Copyright 2001 Linux Developers Group, Inc.
 *************************************************************/

#ifndef GNC_MODULE_H
#define GNC_MODULE_H

#include <glib.h>

typedef void * GNCModule;

#define DEFAULT_MODULE_PATH "/usr/local/gnucash/lib/modules"
#define GNC_MODULE_PREFIX "libgncmod"

/* the basics: initialize the module system, refresh its module
 * database, and get a list of all known modules */
void            gnc_module_system_init(void);
void            gnc_module_system_refresh(void);
GList         * gnc_module_system_modinfo(void);

/* load and unload a module.  gnc_module_system_init() must be called
 * before loading and unloading.
 */
/*@ dependent @*/
GNCModule       gnc_module_load(const gchar * module_name, gint iface);
GNCModule       gnc_module_load_optional(const gchar * module_name, gint iface);
int             gnc_module_unload(GNCModule mod);

#endif
