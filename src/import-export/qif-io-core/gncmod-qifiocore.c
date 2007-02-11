/*********************************************************************
 * gnc-mod-qifiocore.c
 * module definition/initialization for the QIF i/o module
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include <gmodule.h>
#include <libguile.h>

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
gnc_module_init(int refcount) {
  /* load the engine (we depend on it) */
  if(!gnc_module_load("gnucash/engine", 0)) {
    return FALSE;
  }

  /* load the engine (we depend on it) */
  if(!gnc_module_load("gnucash/app-utils", 0)) {
    return FALSE;
  }

  /* load the QIF Scheme code */
  if(scm_c_eval_string("(use-modules (gnucash import-export qif-io-core))") ==
     SCM_BOOL_F) {
    return FALSE;
  }

  return TRUE;
}

int
gnc_module_end(int refcount) {
  return TRUE;
}
