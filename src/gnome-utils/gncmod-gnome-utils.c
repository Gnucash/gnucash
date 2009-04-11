/*********************************************************************
 * gncmod-gnome-utils.c
 * module definition/initialization for the gnome utilities
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include "config.h"

#include <gmodule.h>
#include <libguile.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

#include "dialog-options.h"
#include "qof.h"
#include "gnc-gui-query.h"

#include "gnc-druid-gnome.h"
#include "gnc-druid-provider-edge-gnome.h"
#include "gnc-druid-provider-file-gnome.h"
#include "gnc-druid-provider-multifile-gnome.h"

GNC_MODULE_API_DECL(libgncmod_gnome_utils)

/* version of the gnc module system interface we require */
int libgncmod_gnome_utils_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_gnome_utils_gnc_module_current  = 0;
int libgncmod_gnome_utils_gnc_module_revision = 0;
int libgncmod_gnome_utils_gnc_module_age      = 0;


char *
libgncmod_gnome_utils_gnc_module_path(void) {
  return g_strdup("gnucash/gnome-utils");
}

char *
libgncmod_gnome_utils_gnc_module_description(void) {
  return g_strdup("Utilities for using Gnome/Gtk with GnuCash");
}

static void
lmod(char * mn)
{
  char * form = g_strdup_printf("(use-modules %s)\n", mn);
  scm_c_eval_string(form);
  g_free(form);
}

extern SCM scm_init_sw_gnome_utils_module(void);

int
libgncmod_gnome_utils_gnc_module_init(int refcount) {
  /* load the engine (we depend on it) */
  if(!gnc_module_load("gnucash/engine", 0)) {
    return FALSE;
  }

  /* load the calculation module (we depend on it) */
  if(!gnc_module_load("gnucash/calculation", 0)) {
    return FALSE;
  }

  if(!gnc_module_load("gnucash/app-utils", 0)) {
    return FALSE;
  }

  scm_init_sw_gnome_utils_module();
  lmod("(sw_gnome_utils)");
  lmod("(gnucash gnome-utils)");

  /* Initialize the options-ui database */
  if (refcount == 0) {
    gnc_options_ui_initialize ();

    /* register the druid pieces */
    gnc_druid_gnome_register();
    gnc_druid_provider_edge_gnome_register();
    gnc_druid_provider_file_gnome_register();
    gnc_druid_provider_multifile_gnome_register();
  }

  return TRUE;
}

int
libgncmod_gnome_utils_gnc_module_end(int refcount)
{
  return TRUE;
}
