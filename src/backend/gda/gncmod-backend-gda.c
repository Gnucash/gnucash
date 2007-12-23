/*********************************************************************
 * gncmod-backend-gda.c
 * module definition/initialization for the gda backend module
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include <stdio.h>
#include <gmodule.h>
/* #include <glib-gobject.h> */

#include "gnc-module.h"
#include "gnc-module-api.h"

/* version of the gnc module system interface we require */
int gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int gnc_module_current  = 0;
int gnc_module_revision = 0;
int gnc_module_age      = 0;

static GNCModule engine;

gchar *
gnc_module_path(void)
{
  return g_strdup("gnucash/backend/gda");
}

gchar *
gnc_module_description(void)
{
  return g_strdup("The GDA/SQL backend for GnuCash");
}

int
gnc_module_init(int refcount)
{
  engine = gnc_module_load("gnucash/engine", 0);
  if(!engine) return FALSE;

  /* Need to initialize g-type engine for gconf */
  if (refcount == 0) {
    g_type_init();
  }

  return TRUE;
}

int
gnc_module_end(int refcount)
{
  int unload = TRUE;

  if (engine)
    unload = gnc_module_unload(engine);

  if (refcount == 0)
    engine = NULL;

  return unload;
}
