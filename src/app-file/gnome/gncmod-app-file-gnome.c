/*********************************************************************
 * gncmod-app-file-gnome.c
 * module definition/initialization app-level gnome file interface
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
  return g_strdup("gnucash/app-file/gnome");
}

char * 
gnc_module_description(void) {
  return g_strdup("Application level file interface for Gnome");
}

int
gnc_module_init(int refcount) {
  /* load the calculation module (we depend on it) */
  if(!gnc_module_load("gnucash/app-file", 0)) {
    return FALSE;
  }

  return TRUE;
}

int
gnc_module_end(int refcount) {
  return TRUE;
}
