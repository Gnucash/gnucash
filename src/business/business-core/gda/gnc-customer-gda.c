/********************************************************************\
 * gnc-customer-gda.c -- customer gda backend                       *
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

#include "gncBillTermP.h"
#include "gncCustomerP.h"
#include "gncTaxTableP.h"
#include "gnc-customer-gda.h"
#include "gnc-address-gda.h"
#include "gnc-bill-term-gda.h"
#include "gnc-tax-table-gda.h"

#define _GNC_MOD_NAME	GNC_ID_CUSTOMER

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "customers"

#define MAX_NAME_LEN 50
#define MAX_ID_LEN 50
#define MAX_NOTES_LEN 50

static col_cvt_t col_table[] =
{
	{ "guid",         CT_GUID,          0,             COL_NNUL, "guid" },
	{ "name",         CT_STRING,        MAX_NAME_LEN,  COL_NNUL, NULL, CUSTOMER_NAME },
	{ "id",           CT_STRING,        MAX_ID_LEN,    COL_NNUL, NULL, CUSTOMER_ID },
	{ "notes",        CT_STRING,        MAX_NOTES_LEN, COL_NNUL, NULL, CUSTOMER_NOTES },
	{ "active",       CT_BOOLEAN,       0,             COL_NNUL, NULL, QOF_PARAM_ACTIVE },
	{ "discount",     CT_NUMERIC,       0,             COL_NNUL, NULL, CUSTOMER_DISCOUNT },
	{ "credit",       CT_NUMERIC,       0,             COL_NNUL, NULL, CUSTOMER_CREDIT },
	{ "currency",     CT_COMMODITYREF,  0,             COL_NNUL, NULL, NULL,
			(QofAccessFunc)gncCustomerGetCurrency, (QofSetterFunc)gncCustomerSetCurrency },
	{ "tax_override", CT_BOOLEAN,       0,             COL_NNUL, NULL, CUSTOMER_TT_OVER },
	{ "addr",         CT_ADDRESS,       0,             0,        NULL, CUSTOMER_ADDR },
	{ "shipaddr",     CT_ADDRESS,       0,             0,        NULL, CUSTOMER_SHIPADDR },
	{ "terms",        CT_BILLTERMREF,   0,             0,        NULL, CUSTOMER_TERMS },
	{ "tax_included", CT_INT,           0,             0,        NULL, NULL,
			(QofAccessFunc)gncCustomerGetTaxIncluded, (QofSetterFunc)gncCustomerSetTaxIncluded },
	{ "taxtable",     CT_TAXTABLEREF,   0,             0,        NULL, NULL,
			(QofAccessFunc)gncCustomerGetTaxTable, (QofSetterFunc)gncCustomerSetTaxTable },
	{ NULL }
};

static void
load_single_customer( GncGdaBackend* be, GdaDataModel* pModel, int row )
{
    const GUID* guid;
    GUID customer_guid;
	GncCustomer* pCustomer;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );

    guid = gnc_gda_load_guid( be, pModel, row );
    customer_guid = *guid;

    pCustomer = gncCustomerLookup( be->primary_book, &customer_guid );
    if( pCustomer == NULL ) {
        pCustomer = gncCustomerCreate( be->primary_book );
    }
    gnc_gda_load_object( be, pModel, row, GNC_ID_CUSTOMER, pCustomer, col_table );
    gnc_gda_slots_load( be, QOF_INSTANCE(pCustomer) );

    qof_instance_mark_clean( QOF_INSTANCE(pCustomer) );
}

static void
load_all_customers( GncGdaBackend* be )
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
            load_single_customer( be, pModel, r );
		}
    }
}

/* ================================================================= */
static void
create_customer_tables( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    gnc_gda_create_table_if_needed( be, TABLE_NAME, col_table );
}

/* ================================================================= */
static void
save_customer( QofInstance* inst, GncGdaBackend* be )
{
    const GUID* guid;

	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_CUSTOMER(inst) );
	g_return_if_fail( be != NULL );

    (void)gnc_gda_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TABLE_NAME,
                        GNC_ID_CUSTOMER, inst,
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
    GncGdaBackend* be = (GncGdaBackend*)be_p;

	g_return_if_fail( term_p != NULL );
	g_return_if_fail( GNC_IS_CUSTOMER(term_p) );
	g_return_if_fail( be_p != NULL );

	if( customer_should_be_saved( GNC_CUSTOMER(term_p) ) ) {
    	save_customer( term_p, be );
	}
}

static void
write_customers( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    qof_object_foreach( GNC_ID_CUSTOMER, be->primary_book, write_single_customer, (gpointer)be );
}

/* ================================================================= */
void
gnc_customer_gda_initialize( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_CUSTOMER,
        save_customer,						/* commit */
        load_all_customers,					/* initial_load */
        create_customer_tables,				/* create_tables */
		NULL, NULL, NULL,
		write_customers						/* write */
    };

    qof_object_register_backend( GNC_ID_CUSTOMER, GNC_GDA_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
