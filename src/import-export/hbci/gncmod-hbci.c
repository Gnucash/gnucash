/*********************************************************************
 * gncmod-hbci.c
 * module definition/initialization for HBCI support
 * 
 * Copyright (c) 2002 Christian <stimming@tuhh.de>
 *********************************************************************/

#include <stdio.h>
#include <guile/gh.h>
#include <glib.h>

#include "gnc-module.h"
#include "gnc-module-api.h"
#include "gnc-menu-extensions.h"

#include "gnc-hbci-cb.h"
#include "druid-hbci-initial.h"
//#include "druid-hbci-final.h"

/* version of the gnc module system interface we require */
int gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int gnc_module_current  = 0;
int gnc_module_revision = 0;
int gnc_module_age      = 0;

/* forward references */
char *gnc_module_path(void);
char *gnc_module_description(void);
int gnc_module_init(int refcount);
int gnc_module_end(int refcount);


char *
gnc_module_path(void) {
  return g_strdup("gnucash/import-export/hbci");
}

char * 
gnc_module_description(void) {
  return g_strdup("Support for HBCI protocol");
}


int
gnc_module_init(int refcount) 
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
  gh_eval_str("(load-from-path \"hbci/hbci.scm\")");

  gh_new_procedure("gnc:hbci-initial-setup", 
		   scm_hbci_initial_druid, 0, 0, 0);

  /* Add menu items with C callbacks */
  {
    static GnomeUIInfo reg_online_submenu[] =    
      {
	GNOMEUIINFO_ITEM ( N_("HBCI Get Balance"),
			   N_("Get the account balance online through HBCI"),
			   gnc_hbci_register_menu_getbalance_cb, 
			   GNOME_APP_PIXMAP_NONE),
	GNOMEUIINFO_ITEM ( N_("HBCI Make Transaction"),
			   N_("Invoke a new transaction online through HBCI"),
			   gnc_hbci_register_menu_maketrans_cb, 
			   GNOME_APP_PIXMAP_NONE),
	GNOMEUIINFO_END
      };
    
    static GnomeUIInfo reg_online_menu[] =
      {
	GNOMEUIINFO_SUBTREE( N_("Online Actions"),
			     reg_online_submenu ),
	GNOMEUIINFO_END
      };
    
    
    gnc_add_c_extension (reg_online_menu, WINDOW_NAME_REGISTER "/Actions/");
  }
  
  //gh_new_procedure("gnc:hbci-finish-setup", 
  //scm_hbci_final_druid, 0, 0, 0);
  
  return TRUE;
}

int
gnc_module_end(int refcount) {
  return TRUE;
}
