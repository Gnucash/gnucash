/*********************************************************************
 * gncmod-backend-xml.c
 * module definition/initialization for the file backend module
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include <stdio.h>
#include <gmodule.h>
/* #include <glib-gobject.h> */

#include "gnc-module.h"
#include "gnc-module-api.h"

GNC_MODULE_API_DECL(libgncmod_backend_file)

/* version of the gnc module system interface we require */
int libgnc_backend_file_utils_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_backend_file_gnc_module_current  = 0;
int libgncmod_backend_file_gnc_module_revision = 0;
int libgncmod_backend_file_gnc_module_age      = 0;

static GNCModule engine;


char *
libgncmod_backend_file_gnc_module_path(void)
{
  return g_strdup("gnucash/backend/file");
}

char *
libgncmod_backend_file_gnc_module_description(void)
{
  return g_strdup("The binary and XML (v1 and v2) backends for GnuCash");
}

int
libgncmod_backend_file_gnc_module_init(int refcount)
{
  engine = gnc_module_load("gnucash/engine", 0);
  if(!engine) return FALSE;

  /* Need to initialize g-type engine for gconf */
  if (refcount == 0)
    g_type_init();

  return TRUE;
}

int
libgncmod_backend_file_gnc_module_end(int refcount)
{
  int unload = TRUE;

  if (engine)
    unload = libgnc_backend_file_utils_gnc_module_unload(engine);

  if (refcount == 0)
    engine = NULL;

  return unload;
}
