/*********************************************************************
 * gncmod-locale-reports-us.c
 * module definition/initialization for the US reports
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

#include "config.h"
#include <stdio.h>
#include <libguile.h>
#include "guile-mappings.h"
#include <glib.h>
#include <locale.h>
#include <string.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

/* version of the gnc module system interface we require */
int libgncmod_locale_reports_us_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_locale_reports_us_LTX_gnc_module_current  = 0;
int libgncmod_locale_reports_us_LTX_gnc_module_revision = 0;
int libgncmod_locale_reports_us_LTX_gnc_module_age      = 0;

/* forward references */
char *libgncmod_locale_reports_us_LTX_gnc_module_path(void);
char *libgncmod_locale_reports_us_LTX_gnc_module_description(void);
int libgncmod_locale_reports_us_LTX_gnc_module_init(int refcount);
int libgncmod_locale_reports_us_LTX_gnc_module_end(int refcount);


char *
libgncmod_locale_reports_us_LTX_gnc_module_path(void) {
  return g_strdup("gnucash/report/locale-specific/us");
}

char * 
libgncmod_locale_reports_us_LTX_gnc_module_description(void) {
  return g_strdup("US income tax reports and related material");
}

int
libgncmod_locale_reports_us_LTX_gnc_module_init(int refcount) {
  /* load us tax info */
  if(!gnc_module_load("gnucash/tax/us", 0)) {
    return FALSE;
  }

  /* load the report system */
  if(!gnc_module_load("gnucash/report/report-system", 0)) {
    return FALSE;
  }

  const char *report_taxtxf;
  /* This is a very simple hack that loads the (new, special) German
     tax definition file in a German locale, or (default) loads the
     previous US tax file. */
  const char *thislocale = setlocale(LC_ALL, NULL);
  if (strncmp(thislocale, "de_DE", 5) == 0) {
    report_taxtxf = "(use-modules (gnucash report taxtxf-de_DE))";
  } else {
    report_taxtxf = "(use-modules (gnucash report taxtxf))";
  }

  /* load the report generation scheme code */
  if(scm_c_eval_string(report_taxtxf) 
     == SCM_BOOL_F) {
    printf("failed to load (gnucash report taxtxf)\n");
    return FALSE;
  }

  /* This is unused and therefore no longer installed and/or loaded */
  /*
  if(scm_c_eval_string("(use-modules (gnucash report locale-specific us))") 
     == SCM_BOOL_F) {
    return FALSE;
    }*/

  return TRUE;
}

int
libgncmod_locale_reports_us_LTX_gnc_module_end(int refcount) {
  return TRUE;
}
