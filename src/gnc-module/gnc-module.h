/*************************************************************
 * gnc-module.h -- loadable plugin/module system for gnucash
 * Copyright 2001 Linux Developers Group, Inc.
 *************************************************************/

#ifndef GNC_MODULE_H
#define GNC_MODULE_H

#include <glib.h>
#include <ltdl.h>

#ifndef lt_ptr
#define lt_ptr lt_ptr_t
#endif

typedef void * GNCModule;

typedef struct {
  char * module_path;
  char * module_description;
  char * module_filepath;
  int    module_interface;
  int    module_age;
  int    module_revision;
} GNCModuleInfo;

#define DEFAULT_MODULE_PATH "/usr/local/gnucash/lib/modules"

/* the basics: initialize the module system, refresh its module 
 * database, and get a list of all known modules */
void            gnc_module_system_init(void);
void            gnc_module_system_refresh(void);
GList         * gnc_module_system_modinfo(void);

/* load and unload a module.  gnc_module_system_init() must be called
 * before loading and unloading. */
GNCModule       gnc_module_load(gchar * module_name, gint interface);
GNCModule       gnc_module_load_optional(gchar * module_name, gint interface);
int             gnc_module_unload(GNCModule mod);
GNCModuleInfo * gnc_module_get_info(const char * lib_path);
int             gnc_module_use_scm_module(gchar * module_path);

/* gnc_module_lookup locates the given 'symbol' in module
 * 'mod'.  'mod' must be previously loaded. */
void          * gnc_module_lookup(GNCModule mod, gchar * symbol);

#endif
