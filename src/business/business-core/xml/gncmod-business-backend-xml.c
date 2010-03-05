/*********************************************************************
 * gncmod-business-backend-file.c
 * module definition/initialization for the file backend module
 *
 * Copyright (c) 2002 Derek Atkins <warlord@MIT.EDU>
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *
 *********************************************************************/

#include "gncmod-business-backend-xml.h"

#include "config.h"
#include <gmodule.h>

#include "gnc-module.h"
#include "gnc-module-api.h"
#include "gnc-engine.h"
#include "io-gncxml-v2.h"

#include "gnc-address-xml-v2.h"
#include "gnc-bill-term-xml-v2.h"
#include "gnc-customer-xml-v2.h"
#include "gnc-employee-xml-v2.h"
#include "gnc-entry-xml-v2.h"
#include "gnc-invoice-xml-v2.h"
#include "gnc-job-xml-v2.h"
#include "gnc-order-xml-v2.h"
#include "gnc-owner-xml-v2.h"
#include "gnc-tax-table-xml-v2.h"
#include "gnc-vendor-xml-v2.h"

GNC_MODULE_API_DECL(libgncmod_business_backend_xml)

/* version of the gnc module system interface we require */
int libgncmod_business_backend_xml_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_business_backend_xml_gnc_module_current  = 0;
int libgncmod_business_backend_xml_gnc_module_revision = 0;
int libgncmod_business_backend_xml_gnc_module_age      = 0;

static GNCModule bus_core;
static GNCModule file;


char *
libgncmod_business_backend_xml_gnc_module_path(void)
{
    return g_strdup("gnucash/business-core-xml");
}

char *
libgncmod_business_backend_xml_gnc_module_description(void)
{
    return g_strdup("The XML (v2) parsers for GnuCash business objects");
}

void
gnc_module_init_business_core_xml_init(void)
{
    /* Initialize our pointers into the backend subsystem */
    gnc_address_xml_initialize ();
    gnc_billterm_xml_initialize ();
    gnc_customer_xml_initialize ();
    gnc_employee_xml_initialize ();
    gnc_entry_xml_initialize ();
    gnc_invoice_xml_initialize ();
    gnc_job_xml_initialize ();
    gnc_order_xml_initialize ();
    gnc_owner_xml_initialize ();
    gnc_taxtable_xml_initialize ();
    gnc_vendor_xml_initialize ();
}

int
libgncmod_business_backend_xml_gnc_module_init(int refcount)
{
    if (!gnc_engine_is_initialized())
    {
        return FALSE;
    }

    bus_core = gnc_module_load("gnucash/business-core", 0);
    if (!bus_core) return FALSE;

    if (refcount == 0)
    {
        gnc_module_init_business_core_xml_init();
    }

    return TRUE;
}

int
libgncmod_business_backend_xml_gnc_module_end(int refcount)
{
    int unload = TRUE;

    if (bus_core)
        unload = gnc_module_unload(bus_core);

    if (refcount == 0)
    {
        bus_core = NULL;
        file = NULL;
    }

    return unload;
}
