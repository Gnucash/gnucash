/*********************************************************************
 * gncmod-hbci.c
 * module definition/initialization for HBCI support
 * 
 * Copyright (c) 2002 Christian <stimming@tuhh.de>
 *********************************************************************/

#include "config.h"
#include <stdio.h>
#include <glib.h>
#include <libguile.h>

#include "gnc-module.h"
#include "gnc-module-api.h"
#include "gnc-plugin-hbci.h"

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
libgncmod_hbci_LTX_gnc_module_init(int refcount) 
{
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

  if(!gnc_module_load("gnucash/import-export", 0)) {
    return FALSE;
  }

  /* load the HBCI Scheme code */
  scm_c_eval_string("(load-from-path \"hbci/hbci.scm\")");

  /* Add menu items with C callbacks */
  gnc_plugin_hbci_create_plugin();
  
  return TRUE;
}

int
libgncmod_hbci_LTX_gnc_module_end(int refcount) {
  return TRUE;
}
