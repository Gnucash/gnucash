/*********************************************************************
 * gncmod-app-utils.c
 * module definition/initialization for the report infrastructure 
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

#include <stdio.h>
#include <guile/gh.h>
#include <glib.h>
#include <libguile/strports.h>
#include <libguile/modules.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

#include "gnc-component-manager.h"

/* version of the gnc module system interface we require */
int libgncmod_app_utils_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_app_utils_LTX_gnc_module_current  = 0;
int libgncmod_app_utils_LTX_gnc_module_revision = 0;
int libgncmod_app_utils_LTX_gnc_module_age      = 0;

char *
libgncmod_app_utils_LTX_gnc_module_path(void) {
  return g_strdup("gnucash/app-utils");
}

char * 
libgncmod_app_utils_LTX_gnc_module_description(void) {
  return g_strdup("Utilities for building gnc applications");
}

static void
lmod(char * mn) 
{
  char * form = g_strdup_printf("(use-modules %s)\n", mn);
  gh_eval_str(form);
  g_free(form);
}

int
libgncmod_app_utils_LTX_gnc_module_init(int refcount)
{
  /* load the engine (we depend on it) */
  if(!gnc_module_load("gnucash/engine", 0)) {
    return FALSE;
  }

  /* load the calculation module (we depend on it) */
  if(!gnc_module_load("gnucash/calculation", 0)) {
    return FALSE;
  }

  /* publish g-wrapped bindings */
  /* load the scheme code */
  lmod("(g-wrapped gw-app-utils)");
  lmod("(gnucash app-utils)");

  if (refcount == 0)
    gnc_component_manager_init ();

  return TRUE;
}

int
libgncmod_app_utils_LTX_gnc_module_end(int refcount)
{
  if (refcount == 0)
    gnc_component_manager_shutdown ();

  return TRUE;
}
