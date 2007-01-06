/*********************************************************************
 * gncmod-netword-utils.c
 * module definition/initialization for network communication utilities
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include <gmodule.h>
#include <libguile.h>

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
  return g_strdup("gnucash/network-utils");
}

char *
gnc_module_description(void) {
  return g_strdup("Utilities for performing network communication");
}

int
gnc_module_init(int refcount) {
  return TRUE;
}

int
gnc_module_end(int refcount) {
  return TRUE;
}
