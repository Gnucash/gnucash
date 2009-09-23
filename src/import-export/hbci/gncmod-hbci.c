/*********************************************************************
 * gncmod-hbci.c
 * module definition/initialization for HBCI support
 *
 * Copyright (c) 2002 Christian <stimming@tuhh.de>
 *********************************************************************/

#include "config.h"

#include <gmodule.h>
#include <libguile.h>
#include <glib/gi18n.h>

#include "gnc-module.h"
#include "gnc-module-api.h"
#include "gnc-plugin-hbci.h"
#include "druid-hbci-initial.h"
#include "gnc-hbci-utils.h"
#include <gwenhywfar/gwenhywfar.h>
#include "dialog-preferences.h"

GNC_MODULE_API_DECL(libgncmod_hbci)

/* version of the gnc module system interface we require */
int libgncmod_hbci_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_hbci_gnc_module_current  = 0;
int libgncmod_hbci_gnc_module_revision = 0;
int libgncmod_hbci_gnc_module_age      = 0;


char *
libgncmod_hbci_gnc_module_path(void) {
  return g_strdup("gnucash/import-export/hbci");
}

char *
libgncmod_hbci_gnc_module_description(void) {
  return g_strdup("Support for Online Banking protocols");
}


int
libgncmod_hbci_gnc_module_init(int refcount)
{
  /* load the engine (we depend on it) */
  if(!gnc_module_load("gnucash/engine", 0)) {
    return FALSE;
  }

  /* load the app-utils (we depend on it) */
  if(!gnc_module_load("gnucash/app-utils", 0)) {
    return FALSE;
  }
  if(!gnc_module_load("gnucash/gnome-utils", 0)) {
    return FALSE;
  }

  if(!gnc_module_load("gnucash/import-export", 0)) {
    return FALSE;
  }

  /* Add menu items with C callbacks */
  gnc_plugin_hbci_create_plugin();

  gnc_preferences_add_to_page("hbciprefs.glade", "hbci_prefs",
			      _("Online Banking"));

  /* Initialize gwen library */
  GWEN_Init();

  return TRUE;
}

int
libgncmod_hbci_gnc_module_end(int refcount) {
  gnc_AB_BANKING_delete(0);

  /* Finalize gwen library */
  GWEN_Fini();

  return TRUE;
}
