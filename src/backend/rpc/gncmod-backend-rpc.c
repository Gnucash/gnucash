/*********************************************************************
 * gncmod-backend-rpc.c
 * module definition/initialization for the rpc backend module
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

#include <stdio.h>
#include <glib.h>

#include "Backend.h"
#include "RpcBackend.h"

#include "gnc-backend-api.h"
#include "gnc-module.h"
#include "gnc-module-api.h"

/* version of the gnc module system interface we require */
int libgncmod_backend_rpc_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_backend_rpc_LTX_gnc_module_current  = 0;
int libgncmod_backend_rpc_LTX_gnc_module_revision = 0;
int libgncmod_backend_rpc_LTX_gnc_module_age      = 0;

GNCModule engine;

char *
libgncmod_backend_rpc_LTX_gnc_module_path(void) 
{
  return g_strdup("gnucash/backend/postgres");
}

char * 
libgncmod_backend_rpc_LTX_gnc_module_description(void) 
{
  return g_strdup("The Postgres backend for Gnucash");
}

int
libgncmod_backend_rpc_LTX_gnc_module_init(int refcount) 
{  
  engine = gnc_module_load("gnucash/engine", 0);
  if(!engine) return FALSE;

  return TRUE;
}

int
libgncmod_backend_rpc_LTX_gnc_module_end(int refcount) 
{
  int unload = TRUE;

  if (engine)
    unload = gnc_module_unload(engine);    

  if (refcount == 0)
    engine = NULL;

  return unload;
}


/****************************************************************
 * gnc_backend_new 
 * this is the init function that must be defined by every dynamically
 * loadable backend.  the rpc backend doesn't follow backend
 * loader naming conventions yet so we wrap its initializer function
 * temporarily 
 ****************************************************************/

Backend * 
gnc_backend_new(void) {
  return rpcendNew();
}
