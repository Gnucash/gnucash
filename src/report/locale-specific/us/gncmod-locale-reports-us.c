/*********************************************************************
 * gncmod-locale-reports-us.c
 * module definition/initialization for the US reports
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include "config.h"
#ifdef LOCALE_SPECIFIC_TAX
#include <string.h>
#include <locale.h>
#endif // LOCALE_SPECIFIC_TAX
#include <gmodule.h>
#include <libguile.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

GNC_MODULE_API_DECL(libgncmod_locale_reports_us)

/* version of the gnc module system interface we require */
int libgncmod_locale_reports_us_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_locale_reports_us_gnc_module_current  = 0;
int libgncmod_locale_reports_us_gnc_module_revision = 0;
int libgncmod_locale_reports_us_gnc_module_age      = 0;


char *
libgncmod_locale_reports_us_gnc_module_path(void) {
  /* const char *thislocale = setlocale(LC_ALL, NULL);
  if (strncmp(thislocale, "de_DE", 5) == 0)
    return g_strdup("gnucash/report/locale-specific/de_DE");
    else */
  return g_strdup("gnucash/report/locale-specific/us");
}

char *
libgncmod_locale_reports_us_gnc_module_description(void) {
  return g_strdup("US income tax reports and related material");
}

int
libgncmod_locale_reports_us_gnc_module_init(int refcount) {
  /* load the tax info */
#ifdef LOCALE_SPECIFIC_TAX
  const char *thislocale = setlocale(LC_ALL, NULL);
  /* This is a very simple hack that loads the (new, special) German
     tax definition file in a German locale, or (default) loads the
     previous US tax file. */
  gboolean is_de_DE = (strncmp(thislocale, "de_DE", 5) == 0);
#else
  gboolean is_de_DE = FALSE;
#endif /* LOCALE_SPECIFIC_TAX */
  const char *tax_module = is_de_DE ?
    "gnucash/tax/de_DE" :
    "gnucash/tax/us";
  const char *report_taxtxf = is_de_DE ?
    "(use-modules (gnucash report taxtxf-de_DE))" :
    "(use-modules (gnucash report taxtxf))";
  const char *report_locale = is_de_DE ?
    "(use-modules (gnucash report locale-specific de_DE))" :
    "(use-modules (gnucash report locale-specific us))";

  /* The gchar* cast is only because the function declaration expects
     a non-const string -- probably an api error. */
  if(!gnc_module_load((gchar*)tax_module, 0)) {
    return FALSE;
  }

  /* load the report system */
  if(!gnc_module_load("gnucash/report/report-system", 0)) {
    return FALSE;
  }

  /* load the report generation scheme code */
  if(scm_c_eval_string(report_taxtxf)
     == SCM_BOOL_F) {
    g_warning("failed to load %s\n", report_taxtxf);
    return FALSE;
  }

  /* Load the module scheme code */
  if(scm_c_eval_string(report_locale)
     == SCM_BOOL_F) {
    return FALSE;
  }

  return TRUE;
}

int
libgncmod_locale_reports_us_gnc_module_end(int refcount) {
  return TRUE;
}
