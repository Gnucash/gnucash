/*********************************************************************
 * gnc-mod-engine.c
 * module definition/initialization for the Engine module
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

#include <stdio.h>
#include <guile/gh.h>
#include <glib.h>

#include "gnc-engine.h"
#include "gnc-module-api.h"
#include "gw-engine.h"

/* version of the gnc module system interface we require */
int libgncmod_engine_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_engine_LTX_gnc_module_current  = 0;
int libgncmod_engine_LTX_gnc_module_revision = 0;
int libgncmod_engine_LTX_gnc_module_age      = 0;

char *
libgncmod_engine_LTX_gnc_module_path(void) 
{
  return g_strdup("gnucash/engine");
}

char * 
libgncmod_engine_LTX_gnc_module_description(void) 
{
  return g_strdup("The Gnucash accounting engine");
}

int
libgncmod_engine_LTX_gnc_module_init(int refcount) 
{
  if(refcount == 0) 
  {
    /* initialize the engine on the first load */
    gnc_engine_init(0, NULL);
  }
  
  gh_eval_str("(use-modules (gnucash engine))");

  gh_eval_str("(use-modules (g-wrap gw-glib))");

  gh_eval_str("(use-modules (g-wrapped gw-kvp))");
  gh_eval_str("(use-modules (g-wrapped gw-engine))");

  return TRUE;
}

int
libgncmod_engine_LTX_gnc_module_end(int refcount) {
  return TRUE;
}

