/*********************************************************************
 * gncmod-binary-import.c
 * module definition/initialization for importing gnucash binary files
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

/* version of the gnc module system interface we require */
int gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int gnc_module_current  = 0;
int gnc_module_revision = 0;
int gnc_module_age      = 0;

char *
gnc_module_path(void) {
  return g_strdup("gnucash/import-export/binary-import");
}

char * 
gnc_module_description(void) {
  return g_strdup("Utilities importing GnuCash binary files");
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
  if (refcount == 0)
  {
    /* load the engine (we depend on it) */
    if(!gnc_module_load("gnucash/engine", 0)) {
      return FALSE;
    }

    /* load the calculation module (we depend on it) */
    if(!gnc_module_load("gnucash/app-utils", 0)) {
      return FALSE;
    }

    /* load the calculation module (we depend on it) */
    if(!gnc_module_load("gnucash/gnome-utils", 0)) {
      return FALSE;
    }

    /* publish g-wrapped bindings */
    /* load the scheme code */
    lmod("(g-wrapped gw-binary-import)");
    lmod("(gnucash import-export binary-import)");
  }

  return TRUE;
}

int
gnc_module_end(int refcount) {
  return TRUE;
}
