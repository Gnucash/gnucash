/*********************************************************************
 * gncmod-gnome-search
 * GNC Module initialization for the Gnome Search UI
 * 
 * Copyright (c) 2002 Derek Atkins <warlord@MIT.EDU>
 *********************************************************************/

#include <stdio.h>
#include <guile/gh.h>
#include <glib.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

#include "search-core-type.h"

/* version of the gnc module system interface we require */
int libgncmod_gnome_search_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_gnome_search_LTX_gnc_module_current  = 0;
int libgncmod_gnome_search_LTX_gnc_module_revision = 0;
int libgncmod_gnome_search_LTX_gnc_module_age      = 0;

/* forward references */
char *libgncmod_gnome_search_LTX_gnc_module_path(void);
char *libgncmod_gnome_search_LTX_gnc_module_description(void);
int libgncmod_gnome_search_LTX_gnc_module_init(int refcount);
int libgncmod_gnome_search_LTX_gnc_module_end(int refcount);


char *
libgncmod_gnome_search_LTX_gnc_module_path(void) 
{
  return g_strdup("gnucash/gnome-search");
}

char * 
libgncmod_gnome_search_LTX_gnc_module_description(void) 
{
  return g_strdup("The Gnucash Gnome Search UI");
}

int
libgncmod_gnome_search_LTX_gnc_module_init(int refcount) 
{
  /* load the engine (we depend on it) */
  if(!gnc_module_load("gnucash/engine", 0)) {
    return FALSE;
  }

  if (!gnc_module_load("gnucash/gnome-utils", 0)) {
    return FALSE;
  }

  if(refcount == 0) 
  {
    /* initialize known types */
    gnc_search_core_initialize ();
  }
  
  gh_eval_str("(use-modules (g-wrapped gw-gnome-search))");
  gh_eval_str("(use-modules (gnucash gnome-search))");

  return TRUE;
}

int
libgncmod_gnome_search_LTX_gnc_module_end(int refcount) {
  /* XXX Unload the other modules */

  if (refcount == 0) {
    /* Shutdown */
    gnc_search_core_finalize ();
  }

  return TRUE;
}
