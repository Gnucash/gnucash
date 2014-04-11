/*********************************************************************
 * gnc-mod-engine.c
 * module definition/initialization for the Engine module
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

#include "config.h"
#include <stdio.h>
#include <glib.h>
#include <libguile.h>

#include "gnc-engine.h"
#include "gnc-module-api.h"

/* version of the gnc module system interface we require */
int libgncmod_engine_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_engine_LTX_gnc_module_current  = 0;
int libgncmod_engine_LTX_gnc_module_revision = 0;
int libgncmod_engine_LTX_gnc_module_age      = 0;

/* forward references */
char *libgncmod_engine_LTX_gnc_module_path(void);
char *libgncmod_engine_LTX_gnc_module_description(void);
int libgncmod_engine_LTX_gnc_module_init(int refcount);
int libgncmod_engine_LTX_gnc_module_end(int refcount);


char *
libgncmod_engine_LTX_gnc_module_path(void) 
{
  return g_strdup("gnucash/engine");
}

char * 
libgncmod_engine_LTX_gnc_module_description(void) 
{
  return g_strdup("The GnuCash accounting engine");
}

extern SCM scm_init_sw_engine_module(void);

int
libgncmod_engine_LTX_gnc_module_init(int refcount) 
{
  if(refcount == 0) 
  {
    /* initialize the engine on the first load */
    gnc_engine_init(0, NULL);
  }
  
  scm_init_sw_engine_module();
  scm_c_eval_string("(use-modules (sw_engine))");
  scm_c_eval_string("(use-modules (gnucash engine))");

  return TRUE;
}

int
libgncmod_engine_LTX_gnc_module_end(int refcount) {
  return TRUE;
}

