/********************************************************************\
 * gnc-entry-gda.c -- entry libgda backend                          *
 *                                                                  *
 * Copyright (C) 2007-2008 Phil Longstaff (plongstaff@rogers.com)   *
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

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>
#include <libgda/libgda.h>

#include "gnc-backend-util-gda.h"
#include "gnc-slots-gda.h"

#include "gncEntryP.h"
#include "gncOrderP.h"
#include "gncInvoiceP.h"
#include "gncTaxTableP.h"
#include "gnc-entry-gda.h"
#include "gnc-invoice-gda.h"
#include "gnc-order-gda.h"
#include "gnc-owner-gda.h"
#include "gnc-tax-table-gda.h"

#define _GNC_MOD_NAME	GNC_ID_ENTRY

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "entries"
#define TABLE_VERSION 1
#define MAX_DESCRIPTION_LEN 2048
#define MAX_ACTION_LEN 2048
#define MAX_NOTES_LEN 2048
#define MAX_DISCTYPE_LEN 2048
#define MAX_DISCHOW_LEN 2048

static col_cvt_t col_table[] =
{
	{ "guid",          CT_GUID,        0,                   COL_NNUL|COL_PKEY, "guid" },
	{ "date",          CT_TIMESPEC,    0,                   COL_NNUL,          NULL, ENTRY_DATE },
	{ "entered",       CT_TIMESPEC,    0,                   COL_NNUL,          NULL, ENTRY_DATE_ENTERED },
	{ "description",   CT_STRING,      MAX_DESCRIPTION_LEN, 0,                 NULL, ENTRY_DESC },
	{ "action",        CT_STRING,      MAX_ACTION_LEN,      0,                 NULL, ENTRY_ACTION },
	{ "notes",         CT_STRING,      MAX_NOTES_LEN,       0,                 NULL, ENTRY_NOTES },
	{ "quantity",      CT_NUMERIC,     0,                   0,                 NULL, ENTRY_QTY },
	{ "i_acct",        CT_ACCOUNTREF,  0,                   0,                 NULL, ENTRY_IACCT },
	{ "i_price",       CT_NUMERIC,     0,                   0,                 NULL, ENTRY_IPRICE },
	{ "i_discount",    CT_NUMERIC,     0,                   0,                 NULL, NULL,
			(QofAccessFunc)gncEntryGetInvDiscount, (QofSetterFunc)gncEntrySetInvDiscount },
	{ "invoice",       CT_INVOICEREF,  0,                   0,                 NULL, NULL,
			(QofAccessFunc)gncEntryGetInvoice, (QofSetterFunc)gncEntrySetInvoice },
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
load_single_entry( GncGdaBackend* be, GdaDataModel* pModel, int row )
{
    const GUID* guid;
    GUID v_guid;
	GncEntry* pEntry;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );

    guid = gnc_gda_load_guid( be, pModel, row );
    v_guid = *guid;

    pEntry = gncEntryLookup( be->primary_book, &v_guid );
    if( pEntry == NULL ) {
        pEntry = gncEntryCreate( be->primary_book );
    }
    gnc_gda_load_object( be, pModel, row, GNC_ID_ENTRY, pEntry, col_table );
    gnc_gda_slots_load( be, QOF_INSTANCE(pEntry) );

    qof_instance_mark_clean( QOF_INSTANCE(pEntry) );
}

static void
load_all_entries( GncGdaBackend* be )
{
    static GdaQuery* query = NULL;
    GdaObject* ret;
    QofBook* pBook;

	g_return_if_fail( be != NULL );

    pBook = be->primary_book;

    /* First time, create the query */
    if( query == NULL ) {
        query = gnc_gda_create_select_query( be, TABLE_NAME );
    }

    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;

        for( r = 0; r < numRows; r++ ) {
            load_single_entry( be, pModel, r );
		}
    }
}

/* ================================================================= */
static void
create_entry_tables( GncGdaBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_gda_get_table_version( be, TABLE_NAME );
    if( version == 0 ) {
    	GError* error = NULL;

        gnc_gda_create_table( be, TABLE_NAME, TABLE_VERSION, col_table, &error );
        if( error != NULL ) {
            PERR( "Error creating table: %s\n", error->message );
        }
    }
}

/* ================================================================= */
static void
save_entry( QofInstance* inst, GncGdaBackend* be )
{
    const GUID* guid;

	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_ENTRY(inst) );
	g_return_if_fail( be != NULL );

    (void)gnc_gda_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TABLE_NAME,
                        GNC_ID_ENTRY, inst,
                        col_table );

    // Now, commit or delete any slots
    guid = qof_instance_get_guid( inst );
    if( !qof_instance_get_destroying(inst) ) {
        gnc_gda_slots_save( be, guid, qof_instance_get_slots( inst ) );
    } else {
        gnc_gda_slots_delete( be, guid );
    }
}

/* ================================================================= */
static void
write_single_entry( QofInstance *term_p, gpointer be_p )
{
    GncGdaBackend* be = (GncGdaBackend*)be_p;
	GncEntry* entry = GNC_ENTRY(term_p);

	g_return_if_fail( term_p != NULL );
	g_return_if_fail( GNC_IS_ENTRY(term_p) );
	g_return_if_fail( be_p != NULL );

  	/* Only save if attached */
  	if( gncEntryGetOrder( entry ) != NULL || gncEntryGetInvoice( entry ) != NULL ||
			gncEntryGetBill( entry ) != NULL ) {
    	save_entry( term_p, be );
	}
}

static void
write_entries( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    qof_object_foreach( GNC_ID_ENTRY, be->primary_book, write_single_entry, (gpointer)be );
}

/* ================================================================= */
void
gnc_entry_gda_initialize( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_ENTRY,
        save_entry,							/* commit */
        load_all_entries,					/* initial_load */
        create_entry_tables,				/* create_tables */
		NULL, NULL, NULL,
		write_entries						/* write */
    };

    qof_object_register_backend( GNC_ID_ENTRY, GNC_GDA_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
