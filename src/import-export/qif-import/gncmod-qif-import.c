/*********************************************************************
 * gncmod-qif-import.c
 * module definition/initialization for old QIF importer (deprecated)
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

#include <glib.h>
#include <guile/gh.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

/* version of the gnc module system interface we require */
int gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int gnc_module_current  = 0;
int gnc_module_revision = 0;
int gnc_module_age      = 0;

char *
gnc_module_path(void) 
{
  return g_strdup("gnucash/import-export/qif-import");
}

char * 
gnc_module_description(void) 
{
  return g_strdup("Gnome GUI and Scheme code for QIF importer");
}

int
gnc_module_init(int refcount) 
{
  if(!gnc_module_load("gnucash/engine", 0)) 
  {
    return FALSE;
  }
  gh_eval_str("(use-modules (gnucash import-export qif-import))");
  return TRUE;
}

int
gnc_module_end(int refcount) 
{
  return TRUE;
}

