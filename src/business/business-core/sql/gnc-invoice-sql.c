/********************************************************************\
 * gnc-invoice-sql.c - invoice sql backend                          *
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

/** @file gnc-invoice-sql.c
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

#include "gncBillTermP.h"
#include "gncInvoiceP.h"
#include "gnc-invoice-sql.h"
#include "gnc-owner-sql.h"
#include "gnc-bill-term-sql.h"

#define _GNC_MOD_NAME	GNC_ID_INVOICE

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "invoices"
#define TABLE_VERSION 3

#define MAX_ID_LEN 2048
#define MAX_NOTES_LEN 2048
#define MAX_BILLING_ID_LEN 2048

static GncSqlColumnTableEntry col_table[] =
{
    { "guid",         CT_GUID,         0,                  COL_NNUL | COL_PKEY, "guid" },
    { "id",           CT_STRING,       MAX_ID_LEN,         COL_NNUL,          NULL, INVOICE_ID },
    { "date_opened",  CT_TIMESPEC,     0,                  0,                 NULL, INVOICE_OPENED },
    { "date_posted",  CT_TIMESPEC,     0,                  0,                 NULL, INVOICE_POSTED },
    { "notes",        CT_STRING,       MAX_NOTES_LEN,      COL_NNUL,          "notes" },
    { "active",       CT_BOOLEAN,      0,                  COL_NNUL,          NULL, QOF_PARAM_ACTIVE },
    {
        "currency",     CT_COMMODITYREF, 0,                  COL_NNUL,          NULL, NULL,
        (QofAccessFunc)gncInvoiceGetCurrency, (QofSetterFunc)gncInvoiceSetCurrency
    },
    {
        "owner",        CT_OWNERREF,     0,                  0,                 NULL, NULL,
        (QofAccessFunc)gncInvoiceGetOwner, (QofSetterFunc)gncInvoiceSetOwner
    },
    { "terms",        CT_BILLTERMREF,  0,                  0,                 NULL, INVOICE_TERMS },
    { "billing_id",   CT_STRING,       MAX_BILLING_ID_LEN, 0,                 NULL, INVOICE_BILLINGID },
    { "post_txn",     CT_TXREF,        0,                  0,                 NULL, INVOICE_POST_TXN },
    {
        "post_lot",     CT_LOTREF,       0,                  0,                 NULL, NULL,
        (QofAccessFunc)gncInvoiceGetPostedLot, (QofSetterFunc)gncInvoiceSetPostedLot
    },
    { "post_acc",     CT_ACCOUNTREF,   0,                  0,                 NULL, INVOICE_ACC },
    {
        "billto",       CT_OWNERREF,     0,                  0,                 NULL, NULL,
        (QofAccessFunc)gncInvoiceGetBillTo, (QofSetterFunc)gncInvoiceSetBillTo
    },
    {
        "charge_amt",   CT_NUMERIC,      0,                  0,                 NULL, NULL,
        (QofAccessFunc)gncInvoiceGetToChargeAmount, (QofSetterFunc)gncInvoiceSetToChargeAmount
    },
    { NULL }
};

static GncInvoice*
load_single_invoice( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
    GncInvoice* pInvoice;

    g_return_val_if_fail( be != NULL, NULL );
    g_return_val_if_fail( row != NULL, NULL );

    guid = gnc_sql_load_guid( be, row );
    pInvoice = gncInvoiceLookup( be->primary_book, guid );
    if ( pInvoice == NULL )
    {
        pInvoice = gncInvoiceCreate( be->primary_book );
    }
    gnc_sql_load_object( be, row, GNC_ID_INVOICE, pInvoice, col_table );
    qof_instance_mark_clean( QOF_INSTANCE(pInvoice) );

    return pInvoice;
}

static void
load_all_invoices( GncSqlBackend* be )
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
        GncSqlRow* row;
        GList* list = NULL;

        row = gnc_sql_result_get_first_row( result );
        while ( row != NULL )
        {
            GncInvoice* pInvoice = load_single_invoice( be, row );
            if ( pInvoice != NULL )
            {
                list = g_list_append( list, pInvoice );
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
create_invoice_tables( GncSqlBackend* be )
{
    gint version;

    g_return_if_fail( be != NULL );

    version = gnc_sql_get_table_version( be, TABLE_NAME );
    if ( version == 0 )
    {
        gnc_sql_create_table( be, TABLE_NAME, TABLE_VERSION, col_table );
    }
    else if ( version < TABLE_VERSION )
    {
        /* Upgrade:
             1->2: 64 bit int handling
        	 2->3: invoice open date can be NULL
        */
        gnc_sql_upgrade_table( be, TABLE_NAME, col_table );
        gnc_sql_set_table_version( be, TABLE_NAME, TABLE_VERSION );
    }
}

/* ================================================================= */
static gboolean
save_invoice( GncSqlBackend* be, QofInstance* inst )
{
    const GUID* guid;
    GncInvoice* invoice;
    gint op;
    gboolean is_infant;
    gboolean is_ok = TRUE;

    g_return_val_if_fail( inst != NULL, FALSE );
    g_return_val_if_fail( GNC_IS_INVOICE(inst), FALSE );
    g_return_val_if_fail( be != NULL, FALSE );

    invoice = GNC_INVOICE(inst);

    is_infant = qof_instance_get_infant( inst );
    if ( qof_instance_get_destroying( inst ) )
    {
        op = OP_DB_DELETE;
    }
    else if ( be->is_pristine_db || is_infant )
    {
        op = OP_DB_INSERT;
    }
    else
    {
        op = OP_DB_UPDATE;
    }
    if ( op != OP_DB_DELETE )
    {
        // Ensure the commodity is in the db
        is_ok = gnc_sql_save_commodity( be, gncInvoiceGetCurrency( invoice ) );
    }

    if ( is_ok )
    {
        is_ok = gnc_sql_do_db_operation( be, op, TABLE_NAME, GNC_ID_INVOICE, inst, col_table );
    }

    if ( is_ok )
    {
        // Now, commit or delete any slots
        guid = qof_instance_get_guid( inst );
        if ( !qof_instance_get_destroying(inst) )
        {
            is_ok = gnc_sql_slots_save( be, guid, is_infant, qof_instance_get_slots( inst ) );
        }
        else
        {
            is_ok = gnc_sql_slots_delete( be, guid );
        }
    }

    return is_ok;
}

/* ================================================================= */
static gboolean
invoice_should_be_saved( GncInvoice *invoice )
{
    const char *id;

    g_return_val_if_fail( invoice != NULL, FALSE );

    /* make sure this is a valid invoice before we save it -- should have an ID */
    id = gncInvoiceGetID( invoice );
    if ( id == NULL || *id == '\0' )
    {
        return FALSE;
    }

    return TRUE;
}

static void
write_single_invoice( QofInstance *term_p, gpointer data_p )
{
    write_objects_t* s = (write_objects_t*)data_p;

    g_return_if_fail( term_p != NULL );
    g_return_if_fail( GNC_IS_INVOICE(term_p) );
    g_return_if_fail( data_p != NULL );

    if ( s->is_ok && invoice_should_be_saved( GNC_INVOICE(term_p) ) )
    {
        s->is_ok = save_invoice( s->be, term_p );
    }
}

static gboolean
write_invoices( GncSqlBackend* be )
{
    write_objects_t data;

    g_return_val_if_fail( be != NULL, FALSE );

    data.be = be;
    data.is_ok = TRUE;
    qof_object_foreach( GNC_ID_INVOICE, be->primary_book, write_single_invoice, &data );

    return data.is_ok;
}

/* ================================================================= */
static void
load_invoice_guid( const GncSqlBackend* be, GncSqlRow* row,
                   QofSetterFunc setter, gpointer pObject,
                   const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    GUID guid;
    GncInvoice* invoice = NULL;

    g_return_if_fail( be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if ( val != NULL && G_VALUE_HOLDS_STRING( val ) && g_value_get_string( val ) != NULL )
    {
        string_to_guid( g_value_get_string( val ), &guid );
        invoice = gncInvoiceLookup( be->primary_book, &guid );
        if ( invoice != NULL )
        {
            if ( table_row->gobj_param_name != NULL )
            {
                g_object_set( pObject, table_row->gobj_param_name, invoice, NULL );
            }
            else
            {
                (*setter)( pObject, (const gpointer)invoice );
            }
        }
        else
        {
            PWARN( "Invoice ref '%s' not found", g_value_get_string( val ) );
        }
    }
}

static GncSqlColumnTypeHandler invoice_guid_handler
= { load_invoice_guid,
    gnc_sql_add_objectref_guid_col_info_to_list,
    gnc_sql_add_colname_to_list,
    gnc_sql_add_gvalue_objectref_guid_to_slist
  };
/* ================================================================= */
void
gnc_invoice_sql_initialize( void )
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_INVOICE,
        save_invoice,						/* commit */
        load_all_invoices,					/* initial_load */
        create_invoice_tables,				/* create_tables */
        NULL, NULL, NULL,
        write_invoices						/* write */
    };

    qof_object_register_backend( GNC_ID_INVOICE, GNC_SQL_BACKEND, &be_data );

    gnc_sql_register_col_type_handler( CT_INVOICEREF, &invoice_guid_handler );
}
/* ========================== END OF FILE ===================== */
