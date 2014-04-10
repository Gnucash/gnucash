/*********************************************************************
 * gncmod-standard-reports.c
 * module definition/initialization for the standard reports 
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

#include <stdio.h>
#include <guile/gh.h>
#include <glib.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

/* version of the gnc module system interface we require */
int libgncmod_standard_reports_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_standard_reports_LTX_gnc_module_current  = 0;
int libgncmod_standard_reports_LTX_gnc_module_revision = 0;
int libgncmod_standard_reports_LTX_gnc_module_age      = 0;

/* forward references */
char *libgncmod_standard_reports_LTX_gnc_module_path(void);
char *libgncmod_standard_reports_LTX_gnc_module_description(void);
int libgncmod_standard_reports_LTX_gnc_module_init(int refcount);
int libgncmod_standard_reports_LTX_gnc_module_end(int refcount);


char *
libgncmod_standard_reports_LTX_gnc_module_path(void) {
  return g_strdup("gnucash/report/standard-reports");
}

char * 
libgncmod_standard_reports_LTX_gnc_module_description(void) {
  return g_strdup("Standard income, asset, balance sheet, etc. reports");
}

int
libgncmod_standard_reports_LTX_gnc_module_init(int refcount) {
  /* load the report system */
  if(!gnc_module_load("gnucash/report/report-system", 0)) {
    return FALSE;
  }

  /* load the report generation scheme code */
  if(gh_eval_str("(use-modules (gnucash report standard-reports))") ==
     SCM_BOOL_F) {
    return FALSE;
  }  
  return TRUE;
}

int
libgncmod_standard_reports_LTX_gnc_module_end(int refcount) {
  return TRUE;
}

