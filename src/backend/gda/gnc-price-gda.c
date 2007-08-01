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

static QofLogModule log_module = GNC_MOD_BACKEND;

#define TABLE_NAME "prices"

static gpointer get_value( gpointer pObject, const QofParam* param );
static void set_value( gpointer pObject, gpointer pValue );
static gpointer get_date( gpointer pObject, const QofParam* param );
static void set_date( gpointer pObject, gpointer pValue );
static gpointer get_currency_guid( gpointer pObject, const QofParam* param );
static void set_currency_guid( gpointer pObject, gpointer pValue );
static gpointer get_commodity_guid( gpointer pObject, const QofParam* param );
static void set_commodity_guid( gpointer pObject, gpointer pValue );

#define PRICE_MAX_SOURCE_LEN 50
#define PRICE_MAX_TYPE_LEN 50

static col_cvt_t col_table[] =
{
    { "guid",            CT_GUID,    0, COL_NNUL|COL_PKEY,    NULL,
            (QofAccessFunc)qof_instance_get_guid,
            (QofSetterFunc)qof_instance_set_guid },
    { "commodity_guid",    CT_GUID,    0, COL_NNUL, NULL,
            get_commodity_guid, set_commodity_guid },
    { "currency_guid",    CT_GUID,    0, COL_NNUL, NULL,
            get_currency_guid, set_currency_guid },
    { "date",            CT_TIMESPEC,    0, COL_NNUL, NULL,
            get_date, set_date },
    { "source",            CT_STRING,    PRICE_MAX_SOURCE_LEN, 0, PRICE_SOURCE },
    { "type",            CT_STRING,    PRICE_MAX_TYPE_LEN, 0, PRICE_TYPE },
    { "value",            CT_NUMERIC,    0, COL_NNUL, NULL, get_value, set_value },
    { NULL }
};

/* ================================================================= */
static gpointer
get_value( gpointer pObject, const QofParam* param )
{
    const GNCPrice* pPrice = GNC_PRICE(pObject);
    static gnc_numeric v;

    v = gnc_price_get_value( pPrice );
    return &v;
}

static void
set_value( gpointer pObject, gpointer pValue )
{
    GNCPrice* pPrice = GNC_PRICE(pObject);
    const gnc_numeric* pNumeric = (const gnc_numeric*)pValue;

    gnc_price_set_value( pPrice, *pNumeric );
}

static gpointer
get_date( gpointer pObject, const QofParam* param )
{
    const GNCPrice* pPrice = GNC_PRICE(pObject);
    static Timespec t;

    t = gnc_price_get_time( pPrice );
    return &t;
}

static void
set_date( gpointer pObject, gpointer pValue )
{
    GNCPrice* pPrice = GNC_PRICE(pObject);
    const Timespec* pTimespec = (const Timespec*)pValue;

    gnc_price_set_time( pPrice, *pTimespec );
}

static gpointer
get_currency_guid( gpointer pObject, const QofParam* param )
{
    const GNCPrice* pPrice = GNC_PRICE(pObject);

    return (gpointer)qof_instance_get_guid(
                            QOF_INSTANCE(gnc_price_get_currency( pPrice )) );
}

static void 
set_currency_guid( gpointer pObject, gpointer pValue )
{
    GNCPrice* pPrice = GNC_PRICE(pObject);
    QofBook* pBook = qof_instance_get_book( QOF_INSTANCE(pPrice) );
    gnc_commodity* pCurrency;
    GUID* guid = (GUID*)pValue;

    pCurrency = gnc_commodity_find_commodity_by_guid( guid, pBook );
    gnc_price_set_currency( pPrice, pCurrency );
}

static gpointer
get_commodity_guid( gpointer pObject, const QofParam* param )
{
    const GNCPrice* pPrice = GNC_PRICE(pObject);

    return (gpointer)qof_instance_get_guid(
                        QOF_INSTANCE(gnc_price_get_commodity( pPrice )) );
}

static void 
set_commodity_guid( gpointer pObject, gpointer pValue )
{
    GNCPrice* pPrice = GNC_PRICE(pObject);
    QofBook* pBook = qof_instance_get_book( QOF_INSTANCE(pPrice) );
    gnc_commodity* pCommodity;
    GUID* guid = (GUID*)pValue;

    pCommodity = gnc_commodity_find_commodity_by_guid( guid, pBook );
    gnc_price_set_commodity( pPrice, pCommodity );
}

static GNCPrice*
load_single_price( GncGdaBackend* be, GdaDataModel* pModel, int row, GNCPrice* pPrice )
{
    if( pPrice == NULL ) {
        pPrice = gnc_price_create( be->primary_book );
    }

    gnc_gda_load_object( pModel, row, GNC_ID_PRICE, pPrice, col_table );

    qof_instance_mark_clean( QOF_INSTANCE(pPrice) );

    return pPrice;
}

static void
load_all_prices( GncGdaBackend* be )
{
    static GdaQuery* query;
    GdaObject* ret;
    QofBook* pBook = be->primary_book;
    GNCPriceDB* pPriceDB = gnc_book_get_pricedb( pBook );

    if( query == NULL ) {
        query = gnc_gda_create_select_query( be, TABLE_NAME );
    }
    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;
        GNCPrice* pPrice;

        for( r = 0; r < numRows; r++ ) {
            pPrice = load_single_price( be, pModel, r, NULL );

            if( pPrice != NULL ) {
                gnc_pricedb_add_price( pPriceDB, pPrice );
            }
        }
    }
}

/* ================================================================= */
static void
create_prices_tables( GncGdaBackend* be )
{
    gnc_gda_create_table_if_needed( be, TABLE_NAME, col_table );
}

/* ================================================================= */

void
gnc_gda_save_price( GncGdaBackend* be, QofInstance* inst )
{
    GNCPrice* pPrice = GNC_PRICE(inst);

    /* Ensure commodity and currency are in the db */
    gnc_gda_save_commodity( be, gnc_price_get_commodity( pPrice ) );
    gnc_gda_save_commodity( be, gnc_price_get_currency( pPrice ) );

    (void)gnc_gda_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TABLE_NAME,
                        GNC_ID_PRICE, pPrice,
                        col_table );
}

/* ================================================================= */
void
gnc_gda_init_price_handler( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_PRICE,
        gnc_gda_save_price,            /* commit */
        load_all_prices,            /* initial_load */
        create_prices_tables    /* create tables */
    };

    qof_object_register_backend( GNC_ID_PRICE, GNC_GDA_BACKEND, &be_data );
}

/* ========================== END OF FILE ===================== */
