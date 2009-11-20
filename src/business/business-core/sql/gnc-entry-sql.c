/********************************************************************\
 * gnc-entry-sql.c -- entry sql backend                             *
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

/** @file gnc-entry-sql.c
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

#include "gncEntryP.h"
#include "gncOrderP.h"
#include "gncInvoiceP.h"
#include "gncTaxTableP.h"
#include "gnc-bill-term-sql.h"
#include "gnc-entry-sql.h"
#include "gnc-invoice-sql.h"
#include "gnc-order-sql.h"
#include "gnc-owner-sql.h"
#include "gnc-tax-table-sql.h"

#define _GNC_MOD_NAME	GNC_ID_ENTRY

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "entries"
#define TABLE_VERSION 3
#define MAX_DESCRIPTION_LEN 2048
#define MAX_ACTION_LEN 2048
#define MAX_NOTES_LEN 2048
#define MAX_DISCTYPE_LEN 2048
#define MAX_DISCHOW_LEN 2048

static void entry_set_invoice( gpointer pObject, gpointer val );

static GncSqlColumnTableEntry col_table[] =
{
	{ "guid",          CT_GUID,        0,                   COL_NNUL|COL_PKEY, "guid" },
	{ "date",          CT_TIMESPEC,    0,                   COL_NNUL,          NULL, ENTRY_DATE },
	{ "date_entered",  CT_TIMESPEC,    0,                   0,                 NULL, ENTRY_DATE_ENTERED },
	{ "description",   CT_STRING,      MAX_DESCRIPTION_LEN, 0,                 NULL, ENTRY_DESC },
	{ "action",        CT_STRING,      MAX_ACTION_LEN,      0,                 NULL, ENTRY_ACTION },
	{ "notes",         CT_STRING,      MAX_NOTES_LEN,       0,                 NULL, ENTRY_NOTES },
	{ "quantity",      CT_NUMERIC,     0,                   0,                 NULL, ENTRY_QTY },
	{ "i_acct",        CT_ACCOUNTREF,  0,                   0,                 NULL, ENTRY_IACCT },
	{ "i_price",       CT_NUMERIC,     0,                   0,                 NULL, ENTRY_IPRICE },
	{ "i_discount",    CT_NUMERIC,     0,                   0,                 NULL, NULL,
			(QofAccessFunc)gncEntryGetInvDiscount, (QofSetterFunc)gncEntrySetInvDiscount },
	{ "invoice",       CT_INVOICEREF,  0,                   0,                 NULL, NULL,
			(QofAccessFunc)gncEntryGetInvoice, (QofSetterFunc)entry_set_invoice },
	{ "i_disc_type",   CT_STRING,      MAX_DISCTYPE_LEN,    0,        			NULL, ENTRY_INV_DISC_TYPE },
	{ "i_disc_how",    CT_STRING,      MAX_DISCHOW_LEN,     0,        			NULL, ENTRY_INV_DISC_HOW },
	{ "i_taxable",     CT_BOOLEAN,     0,                   0,        			NULL, ENTRY_INV_TAXABLE },
	{ "i_taxincluded", CT_BOOLEAN,     0,                   0,        			NULL, ENTRY_INV_TAX_INC },
	{ "i_taxtable",    CT_TAXTABLEREF, 0,                   0,        			NULL, NULL,
			(QofAccessFunc)gncEntryGetInvTaxTable, (QofSetterFunc)gncEntrySetInvTaxTable },
	{ "b_acct",        CT_ACCOUNTREF,  0,                   0,        			NULL, ENTRY_BACCT },
	{ "b_price",       CT_NUMERIC,     0,                   0,        			NULL, ENTRY_BPRICE },
	{ "bill",          CT_INVOICEREF,  0,                   0,        			NULL, NULL,
			(QofAccessFunc)gncEntryGetBill, (QofSetterFunc)gncEntrySetBill },
	{ "b_taxable",     CT_BOOLEAN,     0,                   0,        			NULL, ENTRY_BILL_TAXABLE },
	{ "b_taxincluded", CT_BOOLEAN,     0,                   0,        			NULL, ENTRY_BILL_TAX_INC },
	{ "b_taxtable",    CT_TAXTABLEREF, 0,                   0,        			NULL, NULL,
			(QofAccessFunc)gncEntryGetBillTaxTable, (QofSetterFunc)gncEntrySetBillTaxTable },
	{ "b_paytype",     CT_INT,         0,                   0,        			NULL, NULL,
			(QofAccessFunc)gncEntryGetBillPayment, (QofSetterFunc)gncEntrySetBillPayment },
	{ "billable",      CT_BOOLEAN,     0,                   0,        			NULL, ENTRY_BILLABLE },
	{ "billto",        CT_OWNERREF,    0,                   0,        			NULL, ENTRY_BILLTO },
	{ "order_guid",    CT_ORDERREF,    0,                   0,        			NULL, NULL,
			(QofAccessFunc)gncEntryGetOrder, (QofSetterFunc)gncEntrySetOrder },
	{ NULL }
};

static void
entry_set_invoice( gpointer pObject, gpointer val )
{
	GncEntry* entry;
	GncInvoice* invoice;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( GNC_IS_ENTRY(pObject) );
	g_return_if_fail( val != NULL );
	g_return_if_fail( GNC_IS_INVOICE(val) );

	entry = GNC_ENTRY(pObject);
	invoice = GNC_INVOICE(val);

	gncInvoiceAddEntry( invoice, entry );
}

static GncEntry*
load_single_entry( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
	GncEntry* pEntry;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

    guid = gnc_sql_load_guid( be, row );
    pEntry = gncEntryLookup( be->primary_book, guid );
    if( pEntry == NULL ) {
        pEntry = gncEntryCreate( be->primary_book );
    }
    gnc_sql_load_object( be, row, GNC_ID_ENTRY, pEntry, col_table );
    qof_instance_mark_clean( QOF_INSTANCE(pEntry) );

	return pEntry;
}

static void
load_all_entries( GncSqlBackend* be )
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
		GList* list = NULL;

		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
            GncEntry* pEntry = load_single_entry( be, row );
			if( pEntry != NULL ) {
				list = g_list_append( list, pEntry );
			}
			row = gnc_sql_result_get_next_row( result );
		}
		gnc_sql_result_dispose( result );

		if( list != NULL ) {
			gnc_sql_slots_load_for_list( be, list );
		}
    }
}

/* ================================================================= */
static void
create_entry_tables( GncSqlBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_sql_get_table_version( be, TABLE_NAME );
    if( version == 0 ) {
        gnc_sql_create_table( be, TABLE_NAME, TABLE_VERSION, col_table );
    } else if( version < TABLE_VERSION ) {
		/* Upgrade:
		    1->2: 64 bit int handling
			2->3: "entered" -> "date_entered", and it can be NULL
		*/
		gnc_sql_upgrade_table( be, TABLE_NAME, col_table );
		gnc_sql_set_table_version( be, TABLE_NAME, TABLE_VERSION );
    }
}

/* ================================================================= */
static gboolean
save_entry( GncSqlBackend* be, QofInstance* inst )
{
	g_return_val_if_fail( inst != NULL, FALSE );
	g_return_val_if_fail( GNC_IS_ENTRY(inst), FALSE );
	g_return_val_if_fail( be != NULL, FALSE );

    return gnc_sql_commit_standard_item( be, inst, TABLE_NAME, GNC_ID_ENTRY, col_table );
}

/* ================================================================= */
static void
write_single_entry( QofInstance *term_p, gpointer data_p )
{
	write_objects_t* s = (write_objects_t*)data_p;
	GncEntry* entry = GNC_ENTRY(term_p);

	g_return_if_fail( term_p != NULL );
	g_return_if_fail( GNC_IS_ENTRY(term_p) );
	g_return_if_fail( data_p != NULL );

  	/* Only save if attached */
  	if( s->is_ok && (gncEntryGetOrder( entry ) != NULL || gncEntryGetInvoice( entry ) != NULL ||
			gncEntryGetBill( entry ) != NULL) ) {
    	s->is_ok = save_entry( s->be, term_p );
	}
}

static gboolean
write_entries( GncSqlBackend* be )
{
	write_objects_t data;

	g_return_val_if_fail( be != NULL, FALSE );

	data.be = be;
	data.is_ok = TRUE;
    qof_object_foreach( GNC_ID_ENTRY, be->primary_book, write_single_entry, &data );

	return data.is_ok;
}

/* ================================================================= */
void
gnc_entry_sql_initialize( void )
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_ENTRY,
        save_entry,							/* commit */
        load_all_entries,					/* initial_load */
        create_entry_tables,				/* create_tables */
		NULL, NULL, NULL,
		write_entries						/* write */
    };

    qof_object_register_backend( GNC_ID_ENTRY, GNC_SQL_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
