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

#include "gnc-backend-gda.h"

#include "gnc-price-gda.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

/* ================================================================= */
static GNCPrice*
load_price( GncGdaBackend* be, GdaDataModel* pModel, int row )
{
	QofBook* pBook = be->primary_book;
	GNCPrice* pPrice;
	GUID guid;
	GUID commodity_guid;
	GUID currency_guid;
	Timespec date;
	const gchar* source = NULL;
	const gchar* type = NULL;
	gnc_numeric value;

	Account* pParent = NULL;
	const gchar* code = NULL;
	const gchar* description = NULL;
	gnc_commodity* pCommodity;
	gnc_commodity* pCurrency;

	col_cvt_t col_conversion_table[] =
	{
		{ "guid",			CT_GUID,	&guid },
		{ "commodity_guid",	CT_GUID,	&commodity_guid },
		{ "currency_guid",	CT_GUID,	&currency_guid },
		{ "time",			CT_DATE,	&date },
		{ "source",			CT_STRING,	&source },
		{ "type",			CT_STRING,	&type },
		{ "value",			CT_NUMERIC,	&value },
		{ NULL }
	};

	gnc_gda_load_object( be, pModel, col_conversion_table, row );

	pCommodity = gnc_commodity_find_commodity_by_guid( &commodity_guid, pBook );
	pCurrency = gnc_commodity_find_commodity_by_guid( &currency_guid, pBook );

	pPrice = gnc_price_create( pBook );
	gnc_price_set_commodity( pPrice, pCommodity );
	gnc_price_set_currency( pPrice, pCurrency );
	gnc_price_set_time( pPrice, date );
	gnc_price_set_source( pPrice, source );
	gnc_price_set_type( pPrice, type );
	gnc_price_set_value( pPrice, value );

	return pPrice;
}

static void
load_prices( GncGdaBackend* be )
{
	GError* error = NULL;

	GdaQuery* query;
	GdaObject* ret;
	QofBook* pBook = be->primary_book;
	GNCPriceDB* pPriceDB = gnc_book_get_pricedb( pBook );

	query = gda_query_new_from_sql( be->pDict, "SELECT * FROM prices", &error );
	if( query == NULL ) {
		printf( "SQL error: %s\n", error->message );
		return;
	}
	error = NULL;
	ret = gda_query_execute( query, NULL, FALSE, &error );

	if( error != NULL ) {
		printf( "SQL error: %s\n", error->message );
	}
	if( GDA_IS_DATA_MODEL( ret ) ) {
		GdaDataModel* pModel = (GdaDataModel*)ret;
		int numRows = gda_data_model_get_n_rows( pModel );
		int r;
		GNCPrice* pPrice;

		for( r = 0; r < numRows; r++ ) {

			pPrice = load_price( be, pModel, r );

			if( pPrice != NULL ) {
				gnc_pricedb_add_price( pPriceDB, pPrice );
			}
		}
	}
}

/* ================================================================= */
static gboolean
price_exists_in_db( GncGdaBackend* be, const gchar* guid )
{
	gchar* cmdbuf;
	int count;

	cmdbuf = g_strdup_printf( "SELECT * FROM prices WHERE guid='%s';", guid );
	count = gnc_gda_execute_select_get_count( be, cmdbuf );
	g_free( cmdbuf );
	if( count == 0 ) {
		return FALSE;
	} else {
		return TRUE;
	}
}

static void
commit_price( GncGdaBackend* be, QofInstance* inst )
{
	GNCPrice* pPrice = (GNCPrice*)inst;
	gnc_commodity* c;
	const GUID* guid = qof_instance_get_guid( inst );
	const GUID* commodity_guid;
	const GUID* currency_guid;
	Timespec date = gnc_price_get_time( pPrice );
	const gchar* source = gnc_price_get_source( pPrice );
	const gchar* type = gnc_price_get_type( pPrice );
	gnc_numeric value = gnc_price_get_value( pPrice );
	GdaQuery* query;
	gchar guid_buf[GUID_ENCODING_LENGTH+1];

	col_cvt_t col_conversion_table[] =
	{
		{ "guid",			CT_GUID,	&guid },
		{ "commodity_guid",	CT_GUID,	&commodity_guid },
		{ "currency_guid",	CT_GUID,	&currency_guid },
		{ "time",			CT_DATE,	&date },
		{ "source",			CT_STRING,	&source },
		{ "type",			CT_STRING,	&type },
		{ "value",			CT_NUMERIC,	&value },
		{ NULL }
	};

	guid_to_string_buff( guid, guid_buf );

	c = gnc_price_get_commodity( pPrice );
	commodity_guid = qof_instance_get_guid( (QofInstance*)c );
	c = gnc_price_get_currency( pPrice );
	currency_guid = qof_instance_get_guid( (QofInstance*)c );

	(void)gnc_gda_do_db_operation( be,
							(inst->do_free ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
							"prices",
							col_conversion_table );
}

/* ================================================================= */
void
gnc_gda_init_price_handler( void )
{
	static GncGdaDataType_t be_data =
	{
		GNC_GDA_BACKEND_VERSION,
		GNC_ID_PRICE,
		commit_price,			/* commit */
		load_prices				/* initial_load */
	};

	qof_object_register_backend( GNC_ID_PRICE, GNC_GDA_BACKEND, &be_data );
}

/* ========================== END OF FILE ===================== */
