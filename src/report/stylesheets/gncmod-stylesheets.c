/*********************************************************************
 * gncmod-stylesheets.c
 * module definition/initialization for the standard reports 
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
  return g_strdup("gnucash/report/stylesheets");
}

char * 
gnc_module_description(void) {
  return g_strdup("Standard report stylesheet definitions");
}

int
gnc_module_init(int refcount) {
  /* load the report system */
  if(!gnc_module_load("gnucash/report/report-system", 0)) {
    return FALSE;
  }

  /* load the report generation scheme code */
  if(gh_eval_str("(use-modules (gnucash report stylesheets))") ==
     SCM_BOOL_F) {
    return FALSE;
  }
  
  return TRUE;
}


void
gnc_module_finish(int refcount) {
 
}

