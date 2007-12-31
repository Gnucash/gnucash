/*********************************************************************
 * gncmod-business-backend-gda.c
 * module definition/initialization for the gda backend module
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

#include "config.h"
#include <gmodule.h>

#include <libgda/libgda.h>

#include "gnc-module.h"
#include "gnc-module-api.h"
#include "gnc-engine.h"

#include "gnc-backend-util-gda.h"

#include "gnc-address-gda.h"
#include "gnc-bill-term-gda.h"
#include "gnc-customer-gda.h"
#include "gnc-employee-gda.h"
#include "gnc-entry-gda.h"
#include "gnc-invoice-gda.h"
#include "gnc-job-gda.h"
#include "gnc-order-gda.h"
#include "gnc-owner-gda.h"
#include "gnc-tax-table-gda.h"
#include "gnc-vendor-gda.h"

GNC_MODULE_API_DECL(libgncmod_business_backend_gda)

/* version of the gnc module system interface we require */
int libgncmod_business_backend_gda_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_business_backend_gda_gnc_module_current  = 0;
int libgncmod_business_backend_gda_gnc_module_revision = 0;
int libgncmod_business_backend_gda_gnc_module_age      = 0;

static GNCModule bus_core;


gchar *
libgncmod_business_backend_gda_gnc_module_path(void)
{
    return g_strdup( "gnucash/business-core-gda" );
}

gchar *
libgncmod_business_backend_gda_gnc_module_description(void)
{
    return g_strdup( "The GDA backend for GnuCash business objects" );
}

int
libgncmod_business_backend_gda_gnc_module_init(int refcount)
{
    if(!gnc_engine_is_initialized()) { return FALSE; }

    bus_core = gnc_module_load( "gnucash/business-core", 0 );
    if( !bus_core ) return FALSE;

    if( refcount == 0 ) {
        /* Initialize our pointers into the backend subsystem */
        gnc_address_gda_initialize();
        gnc_billterm_gda_initialize();
        gnc_customer_gda_initialize();
        gnc_employee_gda_initialize();
        gnc_entry_gda_initialize();
        gnc_invoice_gda_initialize();
        gnc_job_gda_initialize();
        gnc_order_gda_initialize();
	    gnc_owner_gda_initialize();
	    gnc_taxtable_gda_initialize();
        gnc_vendor_gda_initialize();
    }

    return TRUE;
}

int
libgncmod_business_backend_gda_gnc_module_end(int refcount)
{
    int unload = TRUE;

    if( bus_core ) {
        unload = gnc_module_unload( bus_core );
	}

    if( refcount == 0 ) {
        bus_core = NULL;
    }

    return unload;
}
