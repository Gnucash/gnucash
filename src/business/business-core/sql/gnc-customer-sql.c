/********************************************************************\
 * gnc-customer-sql.c -- customer sql backend                       *
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

/** @file gnc-customer-sql.c
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

#include "gnc-backend-sql.h"
#include "gnc-slots-sql.h"

#include "gncBillTermP.h"
#include "gncCustomerP.h"
#include "gncTaxTableP.h"
#include "gnc-customer-sql.h"
#include "gnc-address-sql.h"
#include "gnc-bill-term-sql.h"
#include "gnc-tax-table-sql.h"

#define _GNC_MOD_NAME	GNC_ID_CUSTOMER

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "customers"
#define TABLE_VERSION 1

#define MAX_NAME_LEN 2048
#define MAX_ID_LEN 2048
#define MAX_NOTES_LEN 2048

static col_cvt_t col_table[] =
{
	{ "guid",         CT_GUID,          0,             COL_NNUL|COL_PKEY, "guid" },
	{ "name",         CT_STRING,        MAX_NAME_LEN,  COL_NNUL,          NULL, CUSTOMER_NAME },
	{ "id",           CT_STRING,        MAX_ID_LEN,    COL_NNUL,          NULL, CUSTOMER_ID },
	{ "notes",        CT_STRING,        MAX_NOTES_LEN, COL_NNUL,          NULL, CUSTOMER_NOTES },
	{ "active",       CT_BOOLEAN,       0,             COL_NNUL,          NULL, QOF_PARAM_ACTIVE },
	{ "discount",     CT_NUMERIC,       0,             COL_NNUL,          NULL, CUSTOMER_DISCOUNT },
	{ "credit",       CT_NUMERIC,       0,             COL_NNUL,          NULL, CUSTOMER_CREDIT },
	{ "currency",     CT_COMMODITYREF,  0,             COL_NNUL,          NULL, NULL,
			(QofAccessFunc)gncCustomerGetCurrency, (QofSetterFunc)gncCustomerSetCurrency },
	{ "tax_override", CT_BOOLEAN,       0,             COL_NNUL,          NULL, CUSTOMER_TT_OVER },
	{ "addr",         CT_ADDRESS,       0,             0,                 NULL, CUSTOMER_ADDR },
	{ "shipaddr",     CT_ADDRESS,       0,             0,                 NULL, CUSTOMER_SHIPADDR },
	{ "terms",        CT_BILLTERMREF,   0,             0,                 NULL, CUSTOMER_TERMS },
	{ "tax_included", CT_INT,           0,             0,                 NULL, NULL,
			(QofAccessFunc)gncCustomerGetTaxIncluded, (QofSetterFunc)gncCustomerSetTaxIncluded },
	{ "taxtable",     CT_TAXTABLEREF,   0,             0,                 NULL, NULL,
			(QofAccessFunc)gncCustomerGetTaxTable, (QofSetterFunc)gncCustomerSetTaxTable },
	{ NULL }
};

static void
load_single_customer( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
	GncCustomer* pCustomer;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );

    guid = gnc_sql_load_guid( be, row );
    pCustomer = gncCustomerLookup( be->primary_book, guid );
    if( pCustomer == NULL ) {
        pCustomer = gncCustomerCreate( be->primary_book );
    }
    gnc_sql_load_object( be, row, GNC_ID_CUSTOMER, pCustomer, col_table );
    gnc_sql_slots_load( be, QOF_INSTANCE(pCustomer) );

    qof_instance_mark_clean( QOF_INSTANCE(pCustomer) );
}

static void
load_all_customers( GncSqlBackend* be )
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
            load_single_customer( be, row );
			row = gnc_sql_result_get_next_row( result );
		}
		gnc_sql_result_dispose( result );
    }
}

/* ================================================================= */
static void
create_customer_tables( GncSqlBackend* be )
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
save_customer( QofInstance* inst, GncSqlBackend* be )
{
    const GUID* guid;

	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_CUSTOMER(inst) );
	g_return_if_fail( be != NULL );

    (void)gnc_sql_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TABLE_NAME,
                        GNC_ID_CUSTOMER, inst,
                        col_table );

    // Now, commit or delete any slots
    guid = qof_instance_get_guid( inst );
    if( !qof_instance_get_destroying(inst) ) {
        gnc_sql_slots_save( be, guid, qof_instance_get_slots( inst ) );
    } else {
        gnc_sql_slots_delete( be, guid );
    }
}

/* ================================================================= */
static gboolean
customer_should_be_saved( GncCustomer *customer )
{
    const char *id;

	g_return_val_if_fail( customer != NULL, FALSE );

    /* Make sure this is a valid customer before we save it -- should have an ID */
    id = gncCustomerGetID( customer );
    if( id == NULL || *id == '\0' ) {
        return FALSE;
	}

    return TRUE;
}

static void
write_single_customer( QofInstance *term_p, gpointer be_p )
{
    GncSqlBackend* be = (GncSqlBackend*)be_p;

	g_return_if_fail( term_p != NULL );
	g_return_if_fail( GNC_IS_CUSTOMER(term_p) );
	g_return_if_fail( be_p != NULL );

	if( customer_should_be_saved( GNC_CUSTOMER(term_p) ) ) {
    	save_customer( term_p, be );
	}
}

static void
write_customers( GncSqlBackend* be )
{
	g_return_if_fail( be != NULL );

    qof_object_foreach( GNC_ID_CUSTOMER, be->primary_book, write_single_customer, (gpointer)be );
}

/* ================================================================= */
void
gnc_customer_sql_initialize( void )
{
    static GncSqlDataType_t be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_CUSTOMER,
        save_customer,						/* commit */
        load_all_customers,					/* initial_load */
        create_customer_tables,				/* create_tables */
		NULL, NULL, NULL,
		write_customers						/* write */
    };

    qof_object_register_backend( GNC_ID_CUSTOMER, GNC_SQL_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
