/*********************************************************************
 * businessmod-core.c
 * module definition/initialization for the Business GNOME UI module
 * 
 * Copyright (c) 2001 Derek Atkins <warlord@MIT.EDU>
 *********************************************************************/

#include <stdio.h>
#include <guile/gh.h>
#include <glib.h>

#include "gnc-module.h"
#include "gnc-module-api.h"
#include "gw-business-gnome.h"

/* version of the gnc module system interface we require */
int libgncmod_business_gnome_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_business_gnome_LTX_gnc_module_current  = 0;
int libgncmod_business_gnome_LTX_gnc_module_revision = 0;
int libgncmod_business_gnome_LTX_gnc_module_age      = 0;

char *
libgncmod_business_gnome_LTX_gnc_module_path(void) 
{
  return g_strdup("gnucash/business-gnome");
}

char * 
libgncmod_business_gnome_LTX_gnc_module_description(void) 
{
  return g_strdup("The Gnucash business module GNOME UI");
}

int
libgncmod_business_gnome_LTX_gnc_module_init(int refcount) 
{
  /* load business-core: we depend on it -- and it depends on the engine */
  if (!gnc_module_load ("gnucash/business-core", 0)) {
    return FALSE;
  }
  /* We also depend on app-utils, gnome-utils, and gnome-search modules */
  if (!gnc_module_load ("gnucash/app-utils", 0)) {
    return FALSE;
  }
  if (!gnc_module_load ("gnucash/gnome-utils", 0)) {
    return FALSE;
  }
  if (!gnc_module_load ("gnucash/gnome-search", 0)) {
    return FALSE;
  }
  if (!gnc_module_load ("gnucash/report/report-gnome", 0)) {
    return FALSE;
  }
  if (!gnc_module_load ("gnucash/report/standard-reports", 0)) {
    return FALSE;
  }

  gh_eval_str("(use-modules (g-wrapped gw-business-gnome))");
  gh_eval_str("(use-modules (gnucash business-gnome))");

  return TRUE;
}

int
libgncmod_business_gnome_LTX_gnc_module_end(int refcount) {
  return TRUE;
}

