/*********************************************************************
 * gncmod-business-backend-file.c
 * module definition/initialization for the file backend module
 * 
 * Copyright (c) 2002 Derek Atkins <warlord@MIT.EDU>
 *********************************************************************/

#include <stdio.h>
#include <glib.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

#include "gnc-customer-xml-v2.h"
#include "gnc-employee-xml-v2.h"
#include "gnc-entry-xml-v2.h"
#include "gnc-invoice-xml-v2.h"
#include "gnc-job-xml-v2.h"
#include "gnc-order-xml-v2.h"
#include "gnc-vendor-xml-v2.h"

/* version of the gnc module system interface we require */
int libgncmod_business_backend_file_LTX_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_business_backend_file_LTX_gnc_module_current  = 0;
int libgncmod_business_backend_file_LTX_gnc_module_revision = 0;
int libgncmod_business_backend_file_LTX_gnc_module_age      = 0;

static GNCModule bus_core;
static GNCModule file;

char *
libgncmod_business_backend_file_LTX_gnc_module_path(void) 
{
  return g_strdup("gnucash/business-core-file");
}

char * 
libgncmod_business_backend_file_LTX_gnc_module_description(void) 
{
  return g_strdup("The XML (v2) parsers for Gnucash business objects");
}

int
libgncmod_business_backend_file_LTX_gnc_module_init(int refcount) 
{  
  bus_core = gnc_module_load("gnucash/business-core", 0);
  if(!bus_core) return FALSE;

  file = gnc_module_load("gnucash/backend/file", 0);
  if(!file) {
    gnc_module_unload (bus_core);
    return FALSE;
  }

  if (refcount == 0) {
    /* Initialize our pointers into the backend subsystem */
    gnc_customer_xml_initialize ();
    gnc_employee_xml_initialize ();
    gnc_entry_xml_initialize ();
    gnc_invoice_xml_initialize ();
    gnc_job_xml_initialize ();
    gnc_order_xml_initialize ();
    gnc_vendor_xml_initialize ();
  }

  return TRUE;
}

int
libgncmod_business_backend_file_LTX_gnc_module_end(int refcount) 
{
  int unload = TRUE;

  if (bus_core)
    unload = gnc_module_unload(bus_core);

  if (file)
    unload = gnc_module_unload(file);

  if (refcount == 0) {
    bus_core = NULL;
    file = NULL;
  }

  return unload;
}
