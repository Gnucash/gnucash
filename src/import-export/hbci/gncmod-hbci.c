/*********************************************************************
 * gncmod-hbci.c
 * module definition/initialization for HBCI support
 * 
 * Copyright (c) 2002 Christian <stimming@tuhh.de>
 *********************************************************************/

#include <stdio.h>
#include <guile/gh.h>
#include <glib.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

/* version of the gnc module system interface we require */
int libgncmod_hbci_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_hbci_LTX_gnc_module_current  = 0;
int libgncmod_hbci_LTX_gnc_module_revision = 0;
int libgncmod_hbci_LTX_gnc_module_age      = 0;

/* forward references */
char *libgncmod_hbci_LTX_gnc_module_path(void);
char *libgncmod_hbci_LTX_gnc_module_description(void);
int libgncmod_hbci_LTX_gnc_module_init(int refcount);
int libgncmod_hbci_LTX_gnc_module_end(int refcount);


char *
libgncmod_hbci_LTX_gnc_module_path(void) {
  return g_strdup("gnucash/import-export/hbci");
}

char * 
libgncmod_hbci_LTX_gnc_module_description(void) {
  return g_strdup("Support for HBCI protocol");
}


int
libgncmod_hbci_LTX_gnc_module_init(int refcount) {
  /* load the engine (we depend on it) */
  if(!gnc_module_load("gnucash/engine", 0)) {
    return FALSE;
  }

  /* load the app-utils (we depend on it) */
  if(!gnc_module_load("gnucash/app-utils", 0)) {
    return FALSE;
  }
  if(!gnc_module_load("gnucash/gnome-utils", 0)) {
    return FALSE;
  }

  /* load the HBCI Scheme code */
  if(gh_eval_str("(use-modules (gnucash import-export hbci))") ==
     SCM_BOOL_F) {
    return FALSE;
  }

  return TRUE;
}

int
libgncmod_hbci_LTX_gnc_module_end(int refcount) {
  return TRUE;
}
