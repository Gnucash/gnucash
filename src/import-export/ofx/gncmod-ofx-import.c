 /**@file
 \brief module definition/initialization for the ofx importer
 \author Copyright (c) 2002 Benoit Grégoire bock@step.polymtl.ca
 */
#include <glib.h>
#include <guile/gh.h>

#include "gnc-ofx-import.h"
#include "gnc-module.h"
#include "gnc-module-api.h"

/* version of the gnc module system interface we require */
int libgncmod_ofx_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_ofx_LTX_gnc_module_current  = 0;
int libgncmod_ofx_LTX_gnc_module_revision = 0;
int libgncmod_ofx_LTX_gnc_module_age      = 0;

//static GNCModule bus_core;
//static GNCModule file;

/* forward references */
char *libgncmod_ofx_LTX_gnc_module_path(void);
char *libgncmod_ofx_LTX_gnc_module_description(void);
int libgncmod_ofx_LTX_gnc_module_init(int refcount);
int libgncmod_ofx_LTX_gnc_module_end(int refcount);


char *
libgncmod_ofx_LTX_gnc_module_path(void)
{
  return g_strdup("gnucash/import-export/ofx");
}
char *
libgncmod_ofx_LTX_gnc_module_description(void)
{
  return g_strdup("Gnome GUI and C code for OFX importer using libofx");
}
int
libgncmod_ofx_LTX_gnc_module_init(int refcount)
{
  if(!gnc_module_load("gnucash/engine", 0))
  {
    return FALSE;
  }
  if(!gnc_module_load("gnucash/app-utils", 0))
  {
    return FALSE;
  }
  if(!gnc_module_load("gnucash/gnome-utils", 0))
  {
    return FALSE;
  }
    if(!gnc_module_load("gnucash/import-export", 0))
  {
    return FALSE;
  }
    gh_eval_str("(load-from-path \"ofx/ofx-import.scm\")");
    gh_new_procedure("gnc:ofx-import",   scm_gnc_file_ofx_import,   0, 0, 0);
  return TRUE;
}

int
libgncmod_ofx_LTX_gnc_module_end(int refcount)
{
  return TRUE;
}
