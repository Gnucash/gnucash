/*********************************************************************
 * gncmod-tax-us.c
 * module definition/initialization for us tax info 
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

#include <stdio.h>
#include <guile/gh.h>
#include <glib.h>

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
  return g_strdup("gnucash/tax/us");
}

char * 
gnc_module_description(void) {
  return g_strdup("US income tax information");
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
  lmod("(gnucash tax us)");
  return TRUE;
}

int
gnc_module_end(int refcount) {
  return TRUE;
}
