/*********************************************************************
 * gncmod-file-backend.c
 * module definition/initialization for the file backend module
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

#include <stdio.h>
#include <glib.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

/* version of the gnc module system interface we require */
int libgncmod_backend_file_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_backend_file_LTX_gnc_module_current  = 0;
int libgncmod_backend_file_LTX_gnc_module_revision = 0;
int libgncmod_backend_file_LTX_gnc_module_age      = 0;

GNCModule engine;

/* forward references */
char *libgncmod_backend_file_LTX_gnc_module_path(void);
char *libgncmod_backend_file_LTX_gnc_module_description(void);
int libgncmod_backend_file_LTX_gnc_module_init(int refcount);
int libgncmod_backend_file_LTX_gnc_module_end(int refcount);


char *
libgncmod_backend_file_LTX_gnc_module_path(void) 
{
  return g_strdup("gnucash/backend/file");
}

char * 
libgncmod_backend_file_LTX_gnc_module_description(void) 
{
  return g_strdup("The binary and XML (v1 and v2) backends for Gnucash");
}

int
libgncmod_backend_file_LTX_gnc_module_init(int refcount) 
{  
  engine = gnc_module_load("gnucash/engine", 0);
  if(!engine) return FALSE;

  return TRUE;
}

int
libgncmod_backend_file_LTX_gnc_module_end(int refcount) 
{
  int unload = TRUE;

  if (engine)
    unload = gnc_module_unload(engine);

  if (refcount == 0)
    engine = NULL;

  return unload;
}
