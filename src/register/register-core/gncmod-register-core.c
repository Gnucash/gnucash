/*********************************************************************
 * gncmod-registercore.c
 * module definition/initialization for core (gui-independent) register
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

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
  return g_strdup("gnucash/register/register-core");
}

char * 
gnc_module_description(void) {
  return g_strdup("Toolkit-independent GUI for ledger-like table displays");
}

int
gnc_module_init(int refcount) {
  if(refcount == 0) 
  {
    if(!gnc_module_load("gnucash/engine", 0)) 
    {
      return FALSE;
    }
  }
  return TRUE;
}


