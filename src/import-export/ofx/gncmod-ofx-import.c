/*********************************************************************
 * gncmod-ofx-import.c
 * module definition/initialization for ofx importer
 * 
 * Copyright (c) 2002 Benoit Grégoire bock@step.polymtl.ca
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
  return g_strdup("gnucash/import-export/ofx");
}
char *
gnc_module_description(void)
{
  return g_strdup("Gnome GUI and C code for OFX importer using libofx");
}
int
gnc_module_init(int refcount)
{
  if(!gnc_module_load("gnucash/engine", 0))
  {
    return FALSE;
  }
  if(!gnc_module_load("gnucash/app-utils", 0))
  {
    return FALSE;
  }
  if(!gnc_module_load("gnucash/gnome-utils", 0))
  {
    return FALSE;
  }
    if(!gnc_module_load("gnucash/import-export", 0))
  {
    return FALSE;
  }
  gh_eval_str("(use-modules (gnucash import-export ofx))");
  printf("OFX module loaded\n");
  return TRUE;
}

int
gnc_module_end(int refcount)
{
  return TRUE;
}
