/*********************************************************************
 * gnc-mod-qifiocore.c
 * module definition/initialization for the QIF i/o module
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
  return g_strdup("gnucash/qif-io/core");
}

char * 
gnc_module_description(void) {
  return g_strdup("Core components of QIF import/export (non-GUI)");
}

int
gnc_module_init(void) {
  /* load the engine (we depend on it) */
  if(!gnc_module_load("gnucash/engine", 0)) {
    return FALSE;
  }

  /* load the QIF Scheme code */
  if(gh_eval_str("(use-modules (gnucash qif-io core))") ==
     SCM_BOOL_F) {
    return FALSE;
  }

  return TRUE;
}

void
gnc_module_on_load(void) {
  /* load the engine (we depend on it) */
  gnc_module_load("gnucash/engine", 0);
  gh_eval_str("(use-modules (gnucash qif-io core))");
}

int
gnc_module_end(void) {
  return TRUE;
}
