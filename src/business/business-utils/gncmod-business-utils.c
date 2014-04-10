/*********************************************************************
 * gncmod-business-utils.c
 * module definition/initialization for the Business Utilitizes module
 * 
 * Copyright (c) 2003 Derek Atkins <warlord@MIT.EDU>
 *********************************************************************/

#include "config.h"
#include <stdio.h>
#include <libguile.h>
#include <glib.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

/* version of the gnc module system interface we require */
int libgncmod_business_utils_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_business_utils_LTX_gnc_module_current  = 0;
int libgncmod_business_utils_LTX_gnc_module_revision = 0;
int libgncmod_business_utils_LTX_gnc_module_age      = 0;

/* forward references */
char *libgncmod_business_utils_LTX_gnc_module_path(void);
char *libgncmod_business_utils_LTX_gnc_module_description(void);
int libgncmod_business_utils_LTX_gnc_module_init(int refcount);
int libgncmod_business_utils_LTX_gnc_module_end(int refcount);

char *
libgncmod_business_utils_LTX_gnc_module_path(void) 
{
  return g_strdup("gnucash/business-utils");
}

char * 
libgncmod_business_utils_LTX_gnc_module_description(void) 
{
  return g_strdup("The Gnucash business utilities module");
}

int
libgncmod_business_utils_LTX_gnc_module_init(int refcount) 
{
  /* load the business-core (we depend on it) */
  if (!gnc_module_load("gnucash/business-core", 0)) {
    return FALSE;
  }

  /* Load the application utils.. */
  if (!gnc_module_load("gnucash/app-utils", 0)) {
    return FALSE;
  }

  if(refcount == 0) 
  {
    /* initialize known types */
  }
  
  scm_c_eval_string("(use-modules (gnucash business-utils))");

  return TRUE;
}

int
libgncmod_business_utils_LTX_gnc_module_end(int refcount) {
  return TRUE;
}
