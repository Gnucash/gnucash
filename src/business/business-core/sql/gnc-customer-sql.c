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
#define TABLE_VERSION 2

#define MAX_NAME_LEN 2048
#define MAX_ID_LEN 2048
#define MAX_NOTES_LEN 2048

static GncSqlColumnTableEntry col_table[] =
{
    { "guid",         CT_GUID,          0,             COL_NNUL | COL_PKEY, "guid" },
    { "name",         CT_STRING,        MAX_NAME_LEN,  COL_NNUL,          "name" },
    { "id",           CT_STRING,        MAX_ID_LEN,    COL_NNUL,          NULL, CUSTOMER_ID },
    { "notes",        CT_STRING,        MAX_NOTES_LEN, COL_NNUL,          NULL, CUSTOMER_NOTES },
    { "active",       CT_BOOLEAN,       0,             COL_NNUL,          NULL, QOF_PARAM_ACTIVE },
    { "discount",     CT_NUMERIC,       0,             COL_NNUL,          NULL, CUSTOMER_DISCOUNT },
    { "credit",       CT_NUMERIC,       0,             COL_NNUL,          NULL, CUSTOMER_CREDIT },
    {
        "currency",     CT_COMMODITYREF,  0,             COL_NNUL,          NULL, NULL,
        (QofAccessFunc)gncCustomerGetCurrency, (QofSetterFunc)gncCustomerSetCurrency
    },
    { "tax_override", CT_BOOLEAN,       0,             COL_NNUL,          NULL, CUSTOMER_TT_OVER },
    { "addr",         CT_ADDRESS,       0,             0,                 NULL, CUSTOMER_ADDR },
    { "shipaddr",     CT_ADDRESS,       0,             0,                 NULL, CUSTOMER_SHIPADDR },
    { "terms",        CT_BILLTERMREF,   0,             0,                 NULL, CUSTOMER_TERMS },
    {
        "tax_included", CT_INT,           0,             0,                 NULL, NULL,
        (QofAccessFunc)gncCustomerGetTaxIncluded, (QofSetterFunc)gncCustomerSetTaxIncluded
    },
    {
        "taxtable",     CT_TAXTABLEREF,   0,             0,                 NULL, NULL,
        (QofAccessFunc)gncCustomerGetTaxTable, (QofSetterFunc)gncCustomerSetTaxTable
    },
    { NULL }
};

static GncCustomer*
load_single_customer( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
    GncCustomer* pCustomer;

    g_return_val_if_fail( be != NULL, NULL );
    g_return_val_if_fail( row != NULL, NULL );

    guid = gnc_sql_load_guid( be, row );
    pCustomer = gncCustomerLookup( be->primary_book, guid );
    if ( pCustomer == NULL )
    {
        pCustomer = gncCustomerCreate( be->primary_book );
    }
    gnc_sql_load_object( be, row, GNC_ID_CUSTOMER, pCustomer, col_table );
    qof_instance_mark_clean( QOF_INSTANCE(pCustomer) );

    return pCustomer;
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
    if ( result != NULL )
    {
        GList* list = NULL;
        GncSqlRow* row;

        row = gnc_sql_result_get_first_row( result );
        while ( row != NULL )
        {
            GncCustomer* pCustomer = load_single_customer( be, row );
            if ( pCustomer != NULL )
            {
                list = g_list_append( list, pCustomer );
            }
            row = gnc_sql_result_get_next_row( result );
        }
        gnc_sql_result_dispose( result );

        if ( list != NULL )
        {
            gnc_sql_slots_load_for_list( be, list );
        }
    }
}

/* ================================================================= */
static void
create_customer_tables( GncSqlBackend* be )
{
    gint version;

    g_return_if_fail( be != NULL );

    version = gnc_sql_get_table_version( be, TABLE_NAME );
    if ( version == 0 )
    {
        gnc_sql_create_table( be, TABLE_NAME, TABLE_VERSION, col_table );
    }
    else if ( version == 1 )
    {
        /* Upgrade 64 bit int handling */
        gnc_sql_upgrade_table( be, TABLE_NAME, col_table );
        gnc_sql_set_table_version( be, TABLE_NAME, TABLE_VERSION );
    }
}

/* ================================================================= */
static gboolean
save_customer( GncSqlBackend* be, QofInstance* inst )
{
    g_return_val_if_fail( inst != NULL, FALSE );
    g_return_val_if_fail( GNC_CUSTOMER(inst), FALSE );
    g_return_val_if_fail( be != NULL, FALSE );

    return gnc_sql_commit_standard_item( be, inst, TABLE_NAME, GNC_ID_CUSTOMER, col_table );
}

/* ================================================================= */
typedef struct
{
    GncSqlBackend* be;
    gboolean is_ok;
} write_customers_t;

static gboolean
customer_should_be_saved( GncCustomer *customer )
{
    const char *id;

    g_return_val_if_fail( customer != NULL, FALSE );

    /* Make sure this is a valid customer before we save it -- should have an ID */
    id = gncCustomerGetID( customer );
    if ( id == NULL || *id == '\0' )
    {
        return FALSE;
    }

    return TRUE;
}

static void
write_single_customer( QofInstance *term_p, gpointer data_p )
{
    write_customers_t* data = (write_customers_t*)data_p;

    g_return_if_fail( term_p != NULL );
    g_return_if_fail( GNC_IS_CUSTOMER(term_p) );
    g_return_if_fail( data_p != NULL );

    if ( customer_should_be_saved( GNC_CUSTOMER(term_p) ) && data->is_ok )
    {
        data->is_ok = save_customer( data->be, term_p );
    }
}

static gboolean
write_customers( GncSqlBackend* be )
{
    write_customers_t data;

    g_return_val_if_fail( be != NULL, FALSE );

    data.be = be;
    data.is_ok = TRUE;
    qof_object_foreach( GNC_ID_CUSTOMER, be->primary_book, write_single_customer, (gpointer)&data );
    return data.is_ok;
}

/* ================================================================= */
void
gnc_customer_sql_initialize( void )
{
    static GncSqlObjectBackend be_data =
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
