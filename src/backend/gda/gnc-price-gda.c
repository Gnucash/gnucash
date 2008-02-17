/********************************************************************
 * gnc-price-gda.c: load and save data to SQL via libgda            *
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
\********************************************************************/
/** @file gnc-price-gda.c
 *  @brief load and save data to SQL 
 *  @author Copyright (c) 2006 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db using libgda
 */

#include "config.h"

#include <glib.h>
#include <libgda/libgda.h>

#include "qof.h"
#include "gnc-pricedb.h"

#include "gnc-backend-util-gda.h"

#include "gnc-commodity-gda.h"
#include "gnc-price-gda.h"
#include "gnc-slots-gda.h"

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "prices"

#define PRICE_MAX_SOURCE_LEN 50
#define PRICE_MAX_TYPE_LEN 50

static col_cvt_t col_table[] =
{
    { "guid",           CT_GUID,           0,                    COL_NNUL|COL_PKEY, "guid" },
    { "commodity_guid", CT_COMMODITYREF,   0,                    COL_NNUL,          NULL, PRICE_COMMODITY },
    { "currency_guid",  CT_COMMODITYREF,   0,                    COL_NNUL,          NULL, PRICE_CURRENCY },
    { "date",           CT_TIMESPEC,       0,                    COL_NNUL,          NULL, PRICE_DATE },
    { "source",         CT_STRING,         PRICE_MAX_SOURCE_LEN, 0,                 NULL, PRICE_SOURCE },
    { "type",           CT_STRING,         PRICE_MAX_TYPE_LEN,   0,                 NULL, PRICE_TYPE },
    { "value",          CT_NUMERIC,        0,                    COL_NNUL,          NULL, PRICE_VALUE },
    { NULL }
};

/* ================================================================= */

static GNCPrice*
load_single_price( GncGdaBackend* be, GdaDataModel* pModel, int row, GList** pList )
{
	GNCPrice* pPrice;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( pModel != NULL, NULL );
	g_return_val_if_fail( row >= 0, NULL );

    pPrice = gnc_price_create( be->primary_book );

    gnc_gda_load_object( be, pModel, row, GNC_ID_PRICE, pPrice, col_table );
	*pList = g_list_append( *pList, pPrice );

    qof_instance_mark_clean( QOF_INSTANCE(pPrice) );

    return pPrice;
}

static void
load_all_prices( GncGdaBackend* be )
{
    static GdaQuery* query;
    GdaObject* ret;
    QofBook* pBook;
    GNCPriceDB* pPriceDB;

	g_return_if_fail( be != NULL );

    pBook = be->primary_book;
    pPriceDB = gnc_book_get_pricedb( pBook );
    if( query == NULL ) {
        query = gnc_gda_create_select_query( be, TABLE_NAME );
    }
    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;
        GNCPrice* pPrice;
		GList* list = NULL;

        for( r = 0; r < numRows; r++ ) {
            pPrice = load_single_price( be, pModel, r, &list );

            if( pPrice != NULL ) {
                gnc_pricedb_add_price( pPriceDB, pPrice );
            }
        }

		if( list != NULL ) {
			gnc_gda_slots_load_for_list( be, list );
		}
    }
}

/* ================================================================= */
static void
create_prices_tables( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    gnc_gda_create_table_if_needed( be, TABLE_NAME, col_table );
}

/* ================================================================= */

static void
save_price( QofInstance* inst, GncGdaBackend* be )
{
    GNCPrice* pPrice = GNC_PRICE(inst);
	gint op;

	g_return_if_fail( be != NULL );
	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_PRICE(inst) );

    /* Ensure commodity and currency are in the db */
    gnc_gda_save_commodity( be, gnc_price_get_commodity( pPrice ) );
    gnc_gda_save_commodity( be, gnc_price_get_currency( pPrice ) );

	if( qof_instance_get_destroying( inst ) ) {
		op = OP_DB_DELETE;
	} else if( be->is_pristine_db ) {
		op = OP_DB_ADD;
	} else {
		op = OP_DB_ADD_OR_UPDATE;
	}
    (void)gnc_gda_do_db_operation( be, op, TABLE_NAME, GNC_ID_PRICE, pPrice, col_table );
}

static gboolean
write_price( GNCPrice* p, gpointer data )
{
    GncGdaBackend* be = (GncGdaBackend*)data;

	g_return_val_if_fail( p != NULL, FALSE );
	g_return_val_if_fail( data != NULL, FALSE );

    save_price( QOF_INSTANCE(p), be );

    return TRUE;
}

static void
write_prices( GncGdaBackend* be )
{
    GNCPriceDB* priceDB;

	g_return_if_fail( be != NULL );

    priceDB = gnc_book_get_pricedb( be->primary_book );

    gnc_pricedb_foreach_price( priceDB, write_price, be, TRUE );
}

/* ================================================================= */
void
gnc_gda_init_price_handler( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_PRICE,
        save_price,         		/* commit */
        load_all_prices,            /* initial_load */
        create_prices_tables,    	/* create tables */
		NULL, NULL, NULL,
		write_prices				/* write */
    };

    qof_object_register_backend( GNC_ID_PRICE, GNC_GDA_BACKEND, &be_data );
}

/* ========================== END OF FILE ===================== */
