/*********************************************************************
 * gncmod-locale-reports-us.c
 * module definition/initialization for the US reports
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

#include <stdio.h>
#include <guile/gh.h>
#include <glib.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

/* version of the gnc module system interface we require */
int gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int gnc_module_current  = 0;
int gnc_module_revision = 0;
int gnc_module_age      = 0;

char *
gnc_module_path(void) {
  return g_strdup("gnucash/report/locale-specific/us");
}

char * 
gnc_module_description(void) {
  return g_strdup("US income tax reports and related material");
}

int
gnc_module_init(int refcount) {
  /* load us tax info */
  if(!gnc_module_load("gnucash/tax/us", 0)) {
    return FALSE;
  }

  /* load the report system */
  if(!gnc_module_load("gnucash/report/report-system", 0)) {
    return FALSE;
  }

  printf("loaded gnc modules .. \n");

  /* load the report generation scheme code */
  if(gh_eval_str("(use-modules (gnucash report taxtxf))") 
     == SCM_BOOL_F) {
    printf("failed to load (gnucash report taxtxf)\n");
    return FALSE;
  }
  printf("loaded taxtxf module\n");
  
  if(gh_eval_str("(use-modules (gnucash report locale-specific us))") 
     == SCM_BOOL_F) {
    return FALSE;
  }

  return TRUE;
}

int
gnc_module_end(int refcount) {
  return TRUE;
}
