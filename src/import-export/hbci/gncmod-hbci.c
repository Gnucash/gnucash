/*********************************************************************
 * gncmod-hbci.c
 * module definition/initialization for HBCI support
 * 
 * Copyright (c) 2002 Christian <stimming@tuhh.de>
 *********************************************************************/

#include "config.h"
#include <stdio.h>
#include <glib.h>
#include <libguile.h>

#include "gnc-module.h"
#include "gnc-module-api.h"
#include "gnc-menu-extensions.h"

#include "gnc-hbci-cb.h"
#include "druid-hbci-initial.h"
#include "gnc-hbci-utils.h"
#include <gwenhywfar/gwenhywfar.h>

/* version of the gnc module system interface we require */
int libgncmod_hbci_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_hbci_LTX_gnc_module_current  = 0;
int libgncmod_hbci_LTX_gnc_module_revision = 0;
int libgncmod_hbci_LTX_gnc_module_age      = 0;

/* forward references */
char *libgncmod_hbci_LTX_gnc_module_path(void);
char *libgncmod_hbci_LTX_gnc_module_description(void);
int libgncmod_hbci_LTX_gnc_module_init(int refcount);
int libgncmod_hbci_LTX_gnc_module_end(int refcount);

static void gnc_hbci_addmenus(void);

char *
libgncmod_hbci_LTX_gnc_module_path(void) {
  return g_strdup("gnucash/import-export/hbci");
}

char * 
libgncmod_hbci_LTX_gnc_module_description(void) {
  return g_strdup("Support for HBCI protocol");
}


int
libgncmod_hbci_LTX_gnc_module_init(int refcount) 
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

  /* load the HBCI Scheme code */
  scm_c_eval_string("(load-from-path \"hbci/hbci.scm\")");

  scm_c_define_gsubr("gnc:hbci-initial-setup", 
		     0, 0, 0, scm_hbci_initial_druid);

  /* Add menu items with C callbacks */
  gnc_hbci_addmenus();

  /* Initialize gwen library */
  GWEN_Init();

  return TRUE;
}

int
libgncmod_hbci_LTX_gnc_module_end(int refcount) {
  gnc_AB_BANKING_delete(0);

  /* Finalize gwen library */
  GWEN_Fini();

  return TRUE;
}

static void
gnc_hbci_addmenus(void)
{
  static GnomeUIInfo reg_online_submenu[] =    
    {
      GNOMEUIINFO_ITEM ( N_("HBCI Get Balance"),
			 N_("Get the account balance online through HBCI"),
			 gnc_hbci_register_menu_getbalance_cb, 
			 GNOME_APP_PIXMAP_NONE),
      GNOMEUIINFO_ITEM ( N_("HBCI Get Transactions"),
			 N_("Get the transactions online through HBCI"),
			 gnc_hbci_register_menu_gettrans_cb, 
			 GNOME_APP_PIXMAP_NONE),
      GNOMEUIINFO_ITEM ( N_("HBCI Issue Transaction"),
			 N_("Issue a new transaction online through HBCI"),
			 gnc_hbci_register_menu_maketrans_cb, 
			 GNOME_APP_PIXMAP_NONE),
      GNOMEUIINFO_ITEM ( N_("HBCI Issue Direct Debit"),
			 N_("Issue a new direct debit note online through HBCI"),
			 gnc_hbci_register_menu_makedebnote_cb, 
			 GNOME_APP_PIXMAP_NONE),
      GNOMEUIINFO_END
    };
    
  static GnomeUIInfo reg_online_menu[] =
    {
      GNOMEUIINFO_SUBTREE( N_("Online Actions"),
			   reg_online_submenu ),
      GNOMEUIINFO_END
    };
    
    
  gnc_add_c_extension (reg_online_menu, WINDOW_NAME_REGISTER "/_Actions/");
}
