/*********************************************************************
 * gncmod-app-file.c
 * module definition/initialization app-level file interface 
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

#include <stdio.h>
#include <guile/gh.h>
#include <glib.h>
#include <libguile/strports.h>
#include <libguile/modules.h>

#include "gnc-file-p.h"
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
  return g_strdup("gnucash/app-file");
}

char * 
gnc_module_description(void) {
  return g_strdup("Application level file interface");
}

static void
lmod(char * mn) 
{
  char * form = g_strdup_printf("(use-modules %s)\n", mn);
  gh_eval_str(form);
  g_free(form);
}

int
gnc_module_init(int refcount) {
  /* load the engine (we depend on it) */
  if(!gnc_module_load("gnucash/engine", 0)) {
    return FALSE;
  }

  /* load the calculation module (we depend on it) */
  if(!gnc_module_load("gnucash/app-utils", 0)) {
    return FALSE;
  }

  lmod ("(g-wrapped gw-app-file)");

  if (refcount == 0)
  {
    gnc_file_init ();
  }

  return TRUE;
}

int
gnc_module_end(int refcount) {
  return TRUE;
}
