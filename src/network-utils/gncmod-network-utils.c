/*********************************************************************
 * gncmod-netword-utils.c
 * module definition/initialization for network communication utilities
 * 
 * Copyright (c) 2001 Linux Developers Group, Inc. 
 *********************************************************************/

#include <stdio.h>
#include <guile/gh.h>
#include <glib.h>
#include <libguile/strports.h>
#include <libguile/modules.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

/* version of the gnc module system interface we require */
int libgncmod_network_utils_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_network_utils_LTX_gnc_module_current  = 0;
int libgncmod_network_utils_LTX_gnc_module_revision = 0;
int libgncmod_network_utils_LTX_gnc_module_age      = 0;

/* forward references */
char *libgncmod_network_utils_LTX_gnc_module_path(void);
char *libgncmod_network_utils_LTX_gnc_module_description(void);
int libgncmod_network_utils_LTX_gnc_module_init(int refcount);
int libgncmod_network_utils_LTX_gnc_module_end(int refcount);


char *
libgncmod_network_utils_LTX_gnc_module_path(void) {
  return g_strdup("gnucash/network-utils");
}

char * 
libgncmod_network_utils_LTX_gnc_module_description(void) {
  return g_strdup("Utilities for performing network communication");
}

int
libgncmod_network_utils_LTX_gnc_module_init(int refcount) {
  return TRUE;
}

int
libgncmod_network_utils_LTX_gnc_module_end(int refcount) {
  return TRUE;
}
