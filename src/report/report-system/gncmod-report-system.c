/*********************************************************************
 * gncmod-report-system.c
 * module definition/initialization for the report infrastructure 
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

#include <stdio.h>
#include <guile/gh.h>
#include <glib.h>
#include "gnc-module.h"

/* version of the gnc module system interface we require */
int gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int gnc_module_current  = 0;
int gnc_module_revision = 0;
int gnc_module_age      = 0;

char *
gnc_module_path(void) {
  return g_strdup("gnucash/report/report-system");
}

char * 
gnc_module_description(void) {
  return g_strdup("Core components of Gnucash report generation system");
}

static void
lmod(char * mn) 
{
  char * form = g_strdup_printf("(use-modules %s)\n", mn);
  gh_eval_str(form);
  g_free(form);
}

int
gnc_module_init(int refcount) {
  /* load the engine (we depend on it) */
  if(!gnc_module_load("gnucash/engine", 0)) {
    return FALSE;
  }

  if(!gnc_module_load("gnucash/app-utils", 0)) {
    return FALSE;
  }
  
  lmod("(gnucash report report-system)");

  /* if this is the first time the module's being loaded, initialize
   * the relative date system */
  if(refcount == 0) {
    gh_eval_str("(gnc:reldate-initialize)");
  }

  return TRUE;
}


void
gnc_module_finish(int refcount) {
 
}

