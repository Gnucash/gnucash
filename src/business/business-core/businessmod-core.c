/*********************************************************************
 * businessmod-core.c
 * module definition/initialization for the core Business module
 * 
 * Copyright (c) 2001 Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 *
 *********************************************************************/

#include "config.h"
#include <stdio.h>
#include <glib.h>
#include <libguile.h>

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
int libgncmod_business_core_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_business_core_LTX_gnc_module_current  = 0;
int libgncmod_business_core_LTX_gnc_module_revision = 0;
int libgncmod_business_core_LTX_gnc_module_age      = 0;

/* forward references */
char *libgncmod_business_core_LTX_gnc_module_path(void);
char *libgncmod_business_core_LTX_gnc_module_description(void);
int libgncmod_business_core_LTX_gnc_module_init(int refcount);
int libgncmod_business_core_LTX_gnc_module_end(int refcount);

char *
libgncmod_business_core_LTX_gnc_module_path(void) 
{
  return g_strdup("gnucash/business-core");
}

char * 
libgncmod_business_core_LTX_gnc_module_description(void) 
{
  return g_strdup("The Gnucash business core");
}

int
libgncmod_business_core_LTX_gnc_module_init(int refcount) 
{
  /* load the engine (we depend on it) */
  if(!gnc_module_load("gnucash/engine", 0)) {
    return FALSE;
  }

  if(refcount == 0) 
  {
    /* initialize known types */
    gncInvoiceRegister ();
    gncJobRegister ();
    gncBillTermRegister ();
    gncCustomerRegister ();
    gncAddressRegister ();
    gncEmployeeRegister ();
    gncEntryRegister ();
    gncOrderRegister ();
    gncOwnerRegister ();
    gncTaxTableRegister ();
    gncVendorRegister ();
  }
  
  scm_c_eval_string("(use-modules (g-wrapped gw-business-core))");
  scm_c_eval_string("(use-modules (gnucash business-core))");

  return TRUE;
}

int
libgncmod_business_core_LTX_gnc_module_end(int refcount) {
  return TRUE;
}
