/*********************************************************************
 * gncmod-register-gnome.c
 * module definition/initialization for Gnome parts of register
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

#include <glib.h>
#include "gnc-module.h"

/* version of the gnc module system interface we require */
int gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int gnc_module_current  = 0;
int gnc_module_revision = 0;
int gnc_module_age      = 0;

char *
gnc_module_path(void) {
  return g_strdup("gnucash/register/register-gnome");
}

char * 
gnc_module_description(void) {
  return g_strdup("Gnome GUI for ledger-like table displays");
}

int
gnc_module_init(int refcount) {
  if(refcount == 0) 
  {
    if(!gnc_module_load("gnucash/engine", 0)) {
      return FALSE;
    }
    if(!gnc_module_load("gnucash/register/register-core", 0)) {
      return FALSE;
    }
  }
  return TRUE;
}


