/*********************************************************************
 * gncmod-business-backend-sql.c
 * module definition/initialization for the sql backend module
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

/** @file gncmod-business-backend-sql.c
 *  @brief load and save address data to SQL
 *  @author Copyright (c) 2007-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */

#include "config.h"
#include <gmodule.h>

#include "gnc-module.h"
#include "gnc-module-api.h"
#include "gnc-engine.h"

#include "gnc-backend-util-sql.h"

#include "gnc-address-sql.h"
#include "gnc-bill-term-sql.h"
#include "gnc-customer-sql.h"
#include "gnc-employee-sql.h"
#include "gnc-entry-sql.h"
#include "gnc-invoice-sql.h"
#include "gnc-job-sql.h"
#include "gnc-order-sql.h"
#include "gnc-owner-sql.h"
#include "gnc-tax-table-sql.h"
#include "gnc-vendor-sql.h"

GNC_MODULE_API_DECL(libgncmod_business_backend_sql)

/* version of the gnc module system interface we require */
int libgncmod_business_backend_sql_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_business_backend_sql_gnc_module_current  = 0;
int libgncmod_business_backend_sql_gnc_module_revision = 0;
int libgncmod_business_backend_sql_gnc_module_age      = 0;

static GNCModule bus_core;


gchar *
libgncmod_business_backend_sql_gnc_module_path(void)
{
    return g_strdup( "gnucash/business-core-sql" );
}

gchar *
libgncmod_business_backend_sql_gnc_module_description(void)
{
    return g_strdup( "The SQL backend for GnuCash business objects" );
}

int
libgncmod_business_backend_sql_gnc_module_init(int refcount)
{
    if(!gnc_engine_is_initialized()) { return FALSE; }

    bus_core = gnc_module_load( "gnucash/business-core", 0 );
    if( !bus_core ) return FALSE;

    if( refcount == 0 ) {
        /* Initialize our pointers into the backend subsystem */
        gnc_address_sql_initialize();
        gnc_billterm_sql_initialize();
        gnc_customer_sql_initialize();
        gnc_employee_sql_initialize();
        gnc_entry_sql_initialize();
        gnc_invoice_sql_initialize();
        gnc_job_sql_initialize();
        gnc_order_sql_initialize();
	    gnc_owner_sql_initialize();
	    gnc_taxtable_sql_initialize();
        gnc_vendor_sql_initialize();
    }

    return TRUE;
}

int
libgncmod_business_backend_sql_gnc_module_end(int refcount)
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
