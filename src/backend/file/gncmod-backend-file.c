/*********************************************************************
 * gncmod-file-backend.c
 * module definition/initialization for the file backend module
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

#include <stdio.h>
#include <glib.h>

#include "gnc-module.h"

/* version of the gnc module system interface we require */
int gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int gnc_module_current  = 0;
int gnc_module_revision = 0;
int gnc_module_age      = 0;

GNCModule engine;

char *
gnc_module_path(void) 
{
  return g_strdup("gnucash/backend/file");
}

char * 
gnc_module_description(void) 
{
  return g_strdup("The binary and XML (v1 and v2) backends for Gnucash");
}

int
gnc_module_init(int refcount) 
{  
  if(refcount == 0) 
  {
    engine = gnc_module_load("gnucash/engine", 0);
    
    if(!engine) return FALSE;
  }
  return TRUE;
}

int
gnc_module_end(int refcount) 
{
  if((refcount == 0) && engine)
  {
    int unload = gnc_module_unload(engine);
    engine = NULL;
    return unload;
  }
  return TRUE;
}

