/*************************************************************
 * gnc-module.h -- loadable plugin/module system for gnucash
 * Copyright 2001 Linux Developers Group, Inc.
 *************************************************************/

#ifndef GNC_MODULE_H
#define GNC_MODULE_H

#include <glib.h>

typedef void * GNCModule;

#define DEFAULT_MODULE_PATH "/usr/local/gnucash/lib/modules"

/* the basics: initialize the module system, refresh its module
 * database, and get a list of all known modules */
void            gnc_module_system_init(void);
void            gnc_module_system_refresh(void);
GList         * gnc_module_system_modinfo(void);

/* load and unload a module.  gnc_module_system_init() must be called
 * before loading and unloading.
 *
 * Note/FIXME: There seems to be no real reason for why the argument
 * module_name is not a const gchar?! It certainly should be const
 * (because of passing string literals), and from a quick glance it is
 * also only used in a const way. */
/*@ dependent @*/
GNCModule       gnc_module_load(gchar * module_name, gint iface);
GNCModule       gnc_module_load_optional(gchar * module_name, gint iface);
int             gnc_module_unload(GNCModule mod);

#endif
