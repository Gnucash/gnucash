/********************************************************************\
 * gnc-vendor-sql.c -- vendor sql backend                           *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/** @file gnc-vendor-sql.c
 *  @brief load and save address data to SQL
 *  @author Copyright (c) 2007-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-commodity.h"

#include "gnc-backend-sql.h"
#include "gnc-commodity-sql.h"
#include "gnc-slots-sql.h"

#include "gnc-commodity.h"
#include "gncBillTermP.h"
#include "gncVendorP.h"
#include "gncTaxTableP.h"
#include "gnc-vendor-sql.h"
#include "gnc-address-sql.h"
#include "gnc-bill-term-sql.h"
#include "gnc-tax-table-sql.h"

#define _GNC_MOD_NAME	GNC_ID_VENDOR

static QofLogModule log_module = G_LOG_DOMAIN;

#define MAX_NAME_LEN 2048
#define MAX_ID_LEN 2048
#define MAX_NOTES_LEN 2048
#define MAX_TAX_INC_LEN 2048

#define TABLE_NAME "vendors"
#define TABLE_VERSION 1

static GncSqlColumnTableEntry col_table[] =
{
	{ "guid",         CT_GUID,          0,               COL_NNUL|COL_PKEY, "guid" },
	{ "name",         CT_STRING,        MAX_NAME_LEN,    COL_NNUL,          NULL, VENDOR_NAME },
	{ "id",           CT_STRING,        MAX_ID_LEN,      COL_NNUL,          NULL, VENDOR_ID },
	{ "notes",        CT_STRING,        MAX_NOTES_LEN,   COL_NNUL,          NULL, VENDOR_NOTES },
	{ "currency",     CT_COMMODITYREF,  0,               COL_NNUL,          NULL, NULL,
			(QofAccessFunc)gncVendorGetCurrency, (QofSetterFunc)gncVendorSetCurrency },
	{ "active",       CT_BOOLEAN,       0,               COL_NNUL,          NULL, NULL,
			(QofAccessFunc)gncVendorGetActive, (QofSetterFunc)gncVendorSetActive },
	{ "tax_override", CT_BOOLEAN,       0,               COL_NNUL,          NULL, VENDOR_TAX_OVERRIDE },
	{ "addr",         CT_ADDRESS,       0,               0,                 NULL, VENDOR_ADDR },
	{ "terms",        CT_BILLTERMREF,   0,               0,                 NULL, VENDOR_TERMS },
	{ "tax_inc",      CT_STRING,        MAX_TAX_INC_LEN, 0,                 NULL, VENDOR_TAX_INC },
	{ "tax_table",    CT_TAXTABLEREF,   0,               0,                 NULL, VENDOR_TAX_TABLE },
	{ NULL }
};

static void
load_single_vendor( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
	GncVendor* pVendor;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );

    guid = gnc_sql_load_guid( be, row );
    pVendor = gncVendorLookup( be->primary_book, guid );
    if( pVendor == NULL ) {
        pVendor = gncVendorCreate( be->primary_book );
    }
    gnc_sql_load_object( be, row, GNC_ID_VENDOR, pVendor, col_table );
    gnc_sql_slots_load( be, QOF_INSTANCE(pVendor) );

    qof_instance_mark_clean( QOF_INSTANCE(pVendor) );
}

static void
load_all_vendors( GncSqlBackend* be )
{
    GncSqlStatement* stmt;
    GncSqlResult* result;
    QofBook* pBook;

	g_return_if_fail( be != NULL );

    pBook = be->primary_book;

    stmt = gnc_sql_create_select_statement( be, TABLE_NAME );
    result = gnc_sql_execute_select_statement( be, stmt );
	gnc_sql_statement_dispose( stmt );
    if( result != NULL ) {
        GncSqlRow* row;

		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
            load_single_vendor( be, row );
			row = gnc_sql_result_get_next_row( result );
		}
		gnc_sql_result_dispose( result );
    }
}

/* ================================================================= */
static void
create_vendor_tables( GncSqlBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_sql_get_table_version( be, TABLE_NAME );
    if( version == 0 ) {
        gnc_sql_create_table( be, TABLE_NAME, TABLE_VERSION, col_table );
    }
}

/* ================================================================= */
static void
save_vendor( GncSqlBackend* be, QofInstance* inst )
{
    GncVendor* v;
    const GUID* guid;
	gint op;
	gboolean is_infant;

	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_VENDOR(inst) );
	g_return_if_fail( be != NULL );

    // Ensure the commodity is in the db
    v = GNC_VENDOR(inst);
    gnc_sql_save_commodity( be, gncVendorGetCurrency( v ) );

	is_infant = qof_instance_get_infant( inst );
	if( qof_instance_get_destroying( inst ) ) {
		op = OP_DB_DELETE;
	} else if( be->is_pristine_db || is_infant ) {
		op = OP_DB_ADD;
	} else {
		op = OP_DB_ADD_OR_UPDATE;
	}
    (void)gnc_sql_do_db_operation( be, op, TABLE_NAME, GNC_ID_VENDOR, v, col_table );

    // Now, commit or delete any slots
    guid = qof_instance_get_guid( inst );
    if( !qof_instance_get_destroying(inst) ) {
        gnc_sql_slots_save( be, guid, is_infant, qof_instance_get_slots( inst ) );
    } else {
        gnc_sql_slots_delete( be, guid );
    }
}

/* ================================================================= */
static gboolean
vendor_should_be_saved( GncVendor *vendor )
{
    const char *id;

	g_return_val_if_fail( vendor != NULL, FALSE );

    /* make sure this is a valid vendor before we save it -- should have an ID */
    id = gncVendorGetID( vendor );
    if( id == NULL || *id == '\0' ) {
        return FALSE;
	}

    return TRUE;
}

static void
write_single_vendor( QofInstance *term_p, gpointer be_p )
{
    GncSqlBackend* be = (GncSqlBackend*)be_p;

	g_return_if_fail( term_p != NULL );
	g_return_if_fail( GNC_IS_VENDOR(term_p) );
	g_return_if_fail( be_p != NULL );

	if( vendor_should_be_saved( GNC_VENDOR(term_p) ) ) {
    	save_vendor( be, term_p );
	}
}

static void
write_vendors( GncSqlBackend* be )
{
	g_return_if_fail( be != NULL );

    qof_object_foreach( GNC_ID_VENDOR, be->primary_book, write_single_vendor, (gpointer)be );
}

/* ================================================================= */
void
gnc_vendor_sql_initialize( void )
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_VENDOR,
        save_vendor,						/* commit */
        load_all_vendors,					/* initial_load */
        create_vendor_tables,				/* create_tables */
		NULL, NULL, NULL,
		write_vendors						/* write */
    };

    qof_object_register_backend( GNC_ID_VENDOR, GNC_SQL_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
