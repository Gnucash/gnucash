/********************************************************************
 * gnc-commodity-gda.c: load and save data to SQL via libgda        *
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
/** @file gnc-commodity-gda.c
 *  @brief load and save data to SQL 
 *  @author Copyright (c) 2000 Gnumatic Inc.
 *  @author Copyright (c) 2002 Derek Atkins <warlord@MIT.EDU>
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 *  @author Copyright (c) 2006 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db using libgda
 */

#include "config.h"

#include <glib.h>
#include <libgda/libgda.h>

#include "qof.h"

#include "gnc-backend-gda.h"
#include "gnc-commodity.h"

#include "gnc-commodity-gda.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

static gpointer get_quote_source_name( gpointer pObject );
static void set_quote_source_name( gpointer pObject, const gpointer pValue );

#define TABLE_NAME "commodities"

static col_cvt_t col_table[] = {
	{ "guid",			CT_GUID,	  0, COL_NNUL|COL_PKEY,
			(GNC_GDA_FN_GETTER)qof_entity_get_guid,
			(GNC_GDA_FN_SETTER)qof_entity_set_guid },
	{ "namespace",		CT_STRING,	 40, COL_NNUL,
			(GNC_GDA_FN_GETTER)gnc_commodity_get_namespace,
			(GNC_GDA_FN_SETTER)gnc_commodity_set_namespace },
	{ "mnemonic",		CT_STRING,	 40, COL_NNUL,
			(GNC_GDA_FN_GETTER)gnc_commodity_get_mnemonic,
			(GNC_GDA_FN_SETTER)gnc_commodity_set_mnemonic },
	{ "fullname",		CT_STRING,	100, COL_NNUL,
			(GNC_GDA_FN_GETTER)gnc_commodity_get_fullname,
			(GNC_GDA_FN_SETTER)gnc_commodity_set_fullname },
	{ "cusip",			CT_STRING,	 50, COL_NNUL,
			(GNC_GDA_FN_GETTER)gnc_commodity_get_cusip,
			(GNC_GDA_FN_SETTER)gnc_commodity_set_cusip },
	{ "fraction",		CT_INT,		  0, COL_NNUL,
			(GNC_GDA_FN_GETTER)gnc_commodity_get_fraction,
			(GNC_GDA_FN_SETTER)gnc_commodity_set_fraction },
	{ "quote_flag",		CT_INT,		  0, COL_NNUL,
			(GNC_GDA_FN_GETTER)gnc_commodity_get_quote_flag,
			(GNC_GDA_FN_SETTER)gnc_commodity_set_quote_flag },
	{ "quote_source",	CT_STRING,	 50, 0,
			get_quote_source_name, set_quote_source_name },
	{ "quote_tz",		CT_STRING,	 50, 0,
			(GNC_GDA_FN_GETTER)gnc_commodity_get_quote_tz,
			(GNC_GDA_FN_SETTER)gnc_commodity_set_quote_tz },
	{ NULL }
};

/* ================================================================= */

static gpointer
get_quote_source_name( gpointer pObject )
{
	gnc_commodity* pCommodity = (gnc_commodity*)pObject;

	return (gpointer)gnc_quote_source_get_internal_name(
							gnc_commodity_get_quote_source(pCommodity));
}

static void 
set_quote_source_name( gpointer pObject, const gpointer pValue )
{
	gnc_commodity* pCommodity = (gnc_commodity*)pObject;
	const gchar* quote_source_name = (const gchar*)pValue;
	gnc_quote_source* quote_source;

	quote_source = gnc_quote_source_lookup_by_internal( quote_source_name );
	gnc_commodity_set_quote_source( pCommodity, quote_source );
}

static gnc_commodity*
load_commodity( GncGdaBackend* be, GdaDataModel* pModel, int row )
{
	QofBook* pBook = be->primary_book;
	int col;
	const GValue* val;
	gnc_commodity* pCommodity;

	pCommodity = gnc_commodity_new( pBook, NULL, NULL, NULL, NULL, 100 );

	gnc_gda_load_object( pModel, row, GNC_ID_COMMODITY, pCommodity, col_table );

	return pCommodity;
}

static void
load_commodities( GncGdaBackend* be )
{
	GError* error = NULL;
	gchar* buf;
	GdaQuery* query;
	GdaObject* ret;
	QofBook* pBook = be->primary_book;
	gnc_commodity_table* pTable = gnc_commodity_table_get_table( pBook );

	buf = g_strdup_printf( "SELECT * FROM %s", TABLE_NAME );
	query = gda_query_new_from_sql( be->pDict, buf, &error );
	g_free( buf );
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
		gnc_commodity* pCommodity;

		for( r = 0; r < numRows; r++ ) {
			gnc_commodity* c;

			pCommodity = load_commodity( be, pModel, r );

			if( pCommodity != NULL ) {
				GUID guid;

				guid = *qof_entity_get_guid( (QofEntity*)pCommodity );
				pCommodity = gnc_commodity_table_insert( pTable, pCommodity );
				qof_entity_set_guid( (QofEntity*)pCommodity, &guid );
			}
		}
	}
}
/* ================================================================= */
static void
create_commodities_tables( GncGdaBackend* be )
{
	GdaDictTable* table;
	GError* error = NULL;
	GdaDictDatabase* db;
	
	db = gda_dict_get_database( be->pDict );
	table = gda_dict_database_get_table_by_name( db, TABLE_NAME );
	if( !GDA_IS_DICT_TABLE(table) ) {
		gnc_gda_create_table( be->pConnection, TABLE_NAME, col_table, &error );
	}
}

/* ================================================================= */
static void
commit_commodity( GncGdaBackend* be, QofInstance* inst )
{
	gnc_commodity* pCommodity = (gnc_commodity*)inst;

	(void)gnc_gda_do_db_operation( be,
						(inst->do_free ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
						TABLE_NAME,
						GNC_ID_COMMODITY, pCommodity,
						col_table );
}

/* ================================================================= */
void
gnc_gda_init_commodity_handler( void )
{
	static GncGdaDataType_t be_data =
	{
		GNC_GDA_BACKEND_VERSION,
		GNC_ID_COMMODITY,
		commit_commodity,			/* commit */
		load_commodities,			/* initial_load */
		create_commodities_tables	/* create_tables */
	};

	qof_object_register_backend( GNC_ID_COMMODITY, GNC_GDA_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
