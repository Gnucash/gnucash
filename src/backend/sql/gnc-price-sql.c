/********************************************************************
 * gnc-price-sql.c: load and save data to SQL                       *
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
/** @file gnc-price-sql.c
 *  @brief load and save data to SQL 
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */

#include "config.h"

#include <glib.h>

#include "qof.h"
#include "gnc-pricedb.h"

#include "gnc-backend-sql.h"

#include "gnc-commodity-sql.h"
#include "gnc-price-sql.h"
#include "gnc-slots-sql.h"

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "prices"
#define TABLE_VERSION 1

#define PRICE_MAX_SOURCE_LEN 2048
#define PRICE_MAX_TYPE_LEN 2048

static const GncSqlColumnTableEntry col_table[] =
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
load_single_price( GncSqlBackend* be, GncSqlRow* row, GList** pList )
{
	GNCPrice* pPrice;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

    pPrice = gnc_price_create( be->primary_book );

    gnc_sql_load_object( be, row, GNC_ID_PRICE, pPrice, col_table );
	*pList = g_list_append( *pList, pPrice );

    qof_instance_mark_clean( QOF_INSTANCE(pPrice) );

    return pPrice;
}

static void
load_all_prices( GncSqlBackend* be )
{
    GncSqlStatement* stmt;
    GncSqlResult* result;
    QofBook* pBook;
    GNCPriceDB* pPriceDB;

	g_return_if_fail( be != NULL );

    pBook = be->primary_book;
    pPriceDB = gnc_book_get_pricedb( pBook );
    stmt = gnc_sql_create_select_statement( be, TABLE_NAME );
    result = gnc_sql_execute_select_statement( be, stmt );
	gnc_sql_statement_dispose( stmt );
    if( result != NULL ) {
        int r;
        GNCPrice* pPrice;
		GList* list = NULL;
		GncSqlRow* row = gnc_sql_result_get_first_row( result );

        while( row != NULL ) {
            pPrice = load_single_price( be, row, &list );

            if( pPrice != NULL ) {
                gnc_pricedb_add_price( pPriceDB, pPrice );
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
create_prices_tables( GncSqlBackend* be )
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
save_price( GncSqlBackend* be, QofInstance* inst )
{
    GNCPrice* pPrice = GNC_PRICE(inst);
	gint op;

	g_return_if_fail( be != NULL );
	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_PRICE(inst) );

    /* Ensure commodity and currency are in the db */
	gnc_sql_save_commodity( be, gnc_price_get_commodity( pPrice ) );
    gnc_sql_save_commodity( be, gnc_price_get_currency( pPrice ) );

	if( qof_instance_get_destroying( inst ) ) {
		op = OP_DB_DELETE;
	} else if( be->is_pristine_db ) {
		op = OP_DB_ADD;
	} else {
		op = OP_DB_ADD_OR_UPDATE;
	}
    (void)gnc_sql_do_db_operation( be, op, TABLE_NAME, GNC_ID_PRICE, pPrice, col_table );
}

static gboolean
write_price( GNCPrice* p, gpointer data )
{
    GncSqlBackend* be = (GncSqlBackend*)data;

	g_return_val_if_fail( p != NULL, FALSE );
	g_return_val_if_fail( data != NULL, FALSE );

    save_price( be, QOF_INSTANCE(p) );

    return TRUE;
}

static void
write_prices( GncSqlBackend* be )
{
    GNCPriceDB* priceDB;

	g_return_if_fail( be != NULL );

    priceDB = gnc_book_get_pricedb( be->primary_book );

    gnc_pricedb_foreach_price( priceDB, write_price, be, TRUE );
}

/* ================================================================= */
void
gnc_sql_init_price_handler( void )
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_PRICE,
        save_price,         		/* commit */
        load_all_prices,            /* initial_load */
        create_prices_tables,    	/* create tables */
		NULL, NULL, NULL,
		write_prices				/* write */
    };

    qof_object_register_backend( GNC_ID_PRICE, GNC_SQL_BACKEND, &be_data );
}

/* ========================== END OF FILE ===================== */
