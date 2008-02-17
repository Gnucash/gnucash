/********************************************************************\
 * gnc-invoice-gda.c - invoice gda backend                          *
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

#include "gnc-commodity.h"

#include "gnc-backend-util-gda.h"
#include "gnc-commodity-gda.h"
#include "gnc-slots-gda.h"

#include "gncBillTermP.h"
#include "gncInvoiceP.h"
#include "gnc-invoice-gda.h"
#include "gnc-owner-gda.h"
#include "gnc-bill-term-gda.h"

#define _GNC_MOD_NAME	GNC_ID_INVOICE

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "invoices"

#define MAX_ID_LEN 50
#define MAX_NOTES_LEN 100
#define MAX_BILLING_ID_LEN 100

static col_cvt_t col_table[] =
{
	{ "guid",         CT_GUID,         0,                  COL_NNUL|COL_PKEY, "guid" },
	{ "id",           CT_STRING,       MAX_ID_LEN,         COL_NNUL,          NULL, INVOICE_ID },
	{ "date_opened",  CT_TIMESPEC,     0,                  COL_NNUL,          NULL, INVOICE_OPENED },
	{ "date_posted",  CT_TIMESPEC,     0,                  0,                 NULL, INVOICE_POSTED },
	{ "notes",        CT_STRING,       MAX_NOTES_LEN,      COL_NNUL,          NULL, INVOICE_NOTES },
	{ "active",       CT_BOOLEAN,      0,                  COL_NNUL,          NULL, QOF_PARAM_ACTIVE },
	{ "currency",     CT_COMMODITYREF, 0,                  COL_NNUL,          NULL, NULL,
			(QofAccessFunc)gncInvoiceGetCurrency, (QofSetterFunc)gncInvoiceSetCurrency },
	{ "owner",        CT_OWNERREF,     0,                  0,                 NULL, NULL,
			(QofAccessFunc)gncInvoiceGetOwner, (QofSetterFunc)gncInvoiceSetOwner },
	{ "terms",        CT_BILLTERMREF,  0,                  0,                 NULL, INVOICE_TERMS },
	{ "billing_id",   CT_STRING,       MAX_BILLING_ID_LEN, 0,                 NULL, INVOICE_BILLINGID },
	{ "post_txn",     CT_TXREF,        0,                  0,                 NULL, INVOICE_POST_TXN },
	{ "post_lot",     CT_LOTREF,       0,                  0,                 NULL, NULL,
			(QofAccessFunc)gncInvoiceGetPostedLot, (QofSetterFunc)gncInvoiceSetPostedLot },
	{ "post_acc",     CT_ACCOUNTREF,   0,                  0,                 NULL, INVOICE_ACC },
	{ "billto",       CT_OWNERREF,     0,                  0,                 NULL, NULL,
			(QofAccessFunc)gncInvoiceGetBillTo, (QofSetterFunc)gncInvoiceSetBillTo },
	{ "charge_amt",   CT_NUMERIC,      0,                  0,                 NULL, NULL,
			(QofAccessFunc)gncInvoiceGetToChargeAmount, (QofSetterFunc)gncInvoiceSetToChargeAmount },
	{ NULL }
};

static void
load_single_invoice( GncGdaBackend* be, GdaDataModel* pModel, int row )
{
    const GUID* guid;
    GUID v_guid;
	GncInvoice* pInvoice;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );

    guid = gnc_gda_load_guid( be, pModel, row );
    v_guid = *guid;

    pInvoice = gncInvoiceLookup( be->primary_book, &v_guid );
    if( pInvoice == NULL ) {
        pInvoice = gncInvoiceCreate( be->primary_book );
    }
    gnc_gda_load_object( be, pModel, row, GNC_ID_INVOICE, pInvoice, col_table );
    gnc_gda_slots_load( be, QOF_INSTANCE(pInvoice) );

    qof_instance_mark_clean( QOF_INSTANCE(pInvoice) );
}

static void
load_all_invoices( GncGdaBackend* be )
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
            load_single_invoice( be, pModel, r );
		}
    }
}

/* ================================================================= */
static void
create_invoice_tables( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    gnc_gda_create_table_if_needed( be, TABLE_NAME, col_table );
}

/* ================================================================= */
static void
save_invoice( QofInstance* inst, GncGdaBackend* be )
{
    const GUID* guid;
	GncInvoice* invoice = GNC_INVOICE(inst);

	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_INVOICE(inst) );
	g_return_if_fail( be != NULL );

    // Ensure the commodity is in the db
    gnc_gda_save_commodity( be, gncInvoiceGetCurrency( invoice ) );

    (void)gnc_gda_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TABLE_NAME,
                        GNC_ID_INVOICE, inst,
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
static gboolean
invoice_should_be_saved( GncInvoice *invoice )
{
    const char *id;

	g_return_val_if_fail( invoice != NULL, FALSE );

    /* make sure this is a valid invoice before we save it -- should have an ID */
    id = gncInvoiceGetID( invoice );
    if( id == NULL || *id == '\0' ) {
        return FALSE;
	}

    return TRUE;
}

static void
write_single_invoice( QofInstance *term_p, gpointer be_p )
{
    GncGdaBackend* be = (GncGdaBackend*)be_p;

	g_return_if_fail( term_p != NULL );
	g_return_if_fail( GNC_IS_INVOICE(term_p) );
	g_return_if_fail( be_p != NULL );

	if( invoice_should_be_saved( GNC_INVOICE(term_p) ) ) {
    	save_invoice( term_p, be );
	}
}

static void
write_invoices( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    qof_object_foreach( GNC_ID_INVOICE, be->primary_book, write_single_invoice, (gpointer)be );
}

/* ================================================================= */
static void
load_invoice_guid( const GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table_row )
{
    const GValue* val;
    GUID guid;
    const GUID* pGuid;
	GncInvoice* invoice = NULL;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    val = gda_data_model_get_value_at_col_name( pModel, table_row->col_name, row );
    if( gda_value_is_null( val ) ) {
        pGuid = NULL;
    } else {
        string_to_guid( g_value_get_string( val ), &guid );
        pGuid = &guid;
    }
	if( pGuid != NULL ) {
		invoice = gncInvoiceLookup( be->primary_book, pGuid );
	}
    if( table_row->gobj_param_name != NULL ) {
		g_object_set( pObject, table_row->gobj_param_name, invoice, NULL );
    } else {
		(*setter)( pObject, (const gpointer)invoice );
    }
}

static col_type_handler_t invoice_guid_handler =
        { load_invoice_guid, gnc_gda_create_objectref_guid_col,
            gnc_gda_get_gvalue_objectref_guid_for_query, gnc_gda_get_gvalue_objectref_guid_cond };
/* ================================================================= */
void
gnc_invoice_gda_initialize( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_INVOICE,
        save_invoice,						/* commit */
        load_all_invoices,					/* initial_load */
        create_invoice_tables,				/* create_tables */
		NULL, NULL, NULL,
		write_invoices						/* write */
    };

    qof_object_register_backend( GNC_ID_INVOICE, GNC_GDA_BACKEND, &be_data );

	gnc_gda_register_col_type_handler( CT_INVOICEREF, &invoice_guid_handler );
}
/* ========================== END OF FILE ===================== */
