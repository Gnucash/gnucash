/*********************************************************************
 * businessmod-core.c
 * module definition/initialization for the core Business module
 * 
 * Copyright (c) 2001 Derek Atkins <warlord@MIT.EDU>
 *********************************************************************/

#include <stdio.h>
#include <guile/gh.h>
#include <glib.h>

#include "gnc-module.h"
#include "gnc-module-api.h"
#include "gw-business-core.h"

#include "gncAddressP.h"
#include "gncBillTermP.h"
#include "gncCustomerP.h"
#include "gncEmployeeP.h"
#include "gncEntryP.h"
#include "gncInvoiceP.h"
#include "gncJobP.h"
#include "gncOrderP.h"
#include "gncOwnerP.h"
#include "gncTaxTableP.h"
#include "gncVendorP.h"

/* version of the gnc module system interface we require */
int gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int gnc_module_current  = 0;
int gnc_module_revision = 0;
int gnc_module_age      = 0;

char *
gnc_module_path(void) 
{
  return g_strdup("gnucash/business-core");
}

char * 
gnc_module_description(void) 
{
  return g_strdup("The Gnucash business core");
}

int
gnc_module_init(int refcount) 
{
  /* load the engine (we depend on it) */
  if(!gnc_module_load("gnucash/engine", 0)) {
    return FALSE;
  }

  if(refcount == 0) 
  {
    /* initialize known types */
    gncAddressRegister ();
    gncBillTermRegister ();
    gncCustomerRegister ();
    gncEmployeeRegister ();
    gncEntryRegister ();
    gncInvoiceRegister ();
    gncJobRegister ();
    gncOrderRegister ();
    gncOwnerRegister ();
    gncTaxTableRegister ();
    gncVendorRegister ();
  }
  
  gh_eval_str("(use-modules (g-wrapped gw-business-core))");
  gh_eval_str("(use-modules (gnucash business-core))");

  return TRUE;
}

int
gnc_module_end(int refcount) {
  return TRUE;
}

