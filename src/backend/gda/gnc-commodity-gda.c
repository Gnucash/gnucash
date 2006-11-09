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

/*
-- Commodities table - stores currencies and stocks/mutual funds
CREATE TABLE commodities (
	guid char(32) NOT NULL,

	namespace text NOT NULL,
	mnemonic text NOT NULL,

	fullname text,
	cusip text,
	fraction int,
	use_quote_source boolean,
	quote_source text,
	quote_tz text,

	PRIMARY KEY(guid)
);
*/
/* ================================================================= */

static gnc_commodity*
load_commodity( GncGdaBackend* be, GdaDataModel* pModel, int row )
{
	QofBook* pBook = be->primary_book;
	int col;
	const GValue* val;
	gnc_commodity* pCommodity;
	GUID guid;
	const gchar* namespace = NULL;
	const gchar* mnemonic = NULL;
	const gchar* fullname = NULL;
	const gchar* cusip = NULL;
	int fraction = 1;
	gboolean quote_flag = FALSE;
	const gchar* quote_source_name = NULL;
	gnc_quote_source* quote_source = NULL;
	const gchar* quote_tz = NULL;
	const gchar* s;

	col_cvt_t col_conversion[] = {
		{ "guid",				CT_GUID,	&guid },
		{ "namespace",			CT_STRING,	&namespace },
		{ "mnemonic",			CT_STRING,	&mnemonic },
		{ "fullname",			CT_STRING,	&fullname },
		{ "cusip",				CT_STRING,	&cusip },
		{ "fraction",			CT_INT,		&fraction },
		{ "use_quote_source",	CT_INT,		&quote_flag },
		{ "quote_source",		CT_STRING,	&quote_source_name },
		{ "quote_tz",			CT_STRING,	&quote_tz },
		{ NULL }
	};

	gnc_gda_load_object( be, pModel, col_conversion, row );

	pCommodity = gnc_commodity_new( pBook, fullname, namespace, mnemonic,
									cusip, fraction );
	gnc_commodity_set_quote_flag( pCommodity, quote_flag );
	quote_source = gnc_quote_source_lookup_by_internal( quote_source_name );
	gnc_commodity_set_quote_source( pCommodity, quote_source );
	gnc_commodity_set_quote_tz( pCommodity, quote_tz );
	qof_entity_set_guid( (QofEntity*)pCommodity, &guid );

	return pCommodity;
}

static void
load_commodities( GncGdaBackend* be )
{
	GError* error = NULL;

	GdaQuery* query;
	GdaObject* ret;
	QofBook* pBook = be->primary_book;
	gnc_commodity_table* pTable = gnc_commodity_table_get_table( pBook );

	query = gda_query_new_from_sql( be->pDict,
									"SELECT * FROM commodities",
									&error );
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
commit_commodity( GncGdaBackend* be, QofInstance* inst )
{
	gnc_commodity* pCommodity = (gnc_commodity*)inst;
	const gchar* mnemonic = gnc_commodity_get_mnemonic(pCommodity);
	const gchar* namespace = gnc_commodity_get_namespace(pCommodity);
	const gchar* fullname = gnc_commodity_get_fullname(pCommodity);
	const gchar* cusip = gnc_commodity_get_cusip(pCommodity);
	int fraction = gnc_commodity_get_fraction(pCommodity);
	const gchar* quote_source = gnc_quote_source_get_user_name(gnc_commodity_get_quote_source(pCommodity));
	const gchar* quote_tz = gnc_commodity_get_quote_tz(pCommodity);
	const GUID* guid = qof_instance_get_guid( inst );
	gboolean quote_flag = gnc_commodity_get_quote_flag(pCommodity);
	GdaQuery* pQuery;

	col_cvt_t col_conversion_table[] = {
		{ "guid",				CT_GUID,	&guid },
		{ "namespace",			CT_STRING,	&namespace },
		{ "mnemonic",			CT_STRING,	&mnemonic },
		{ "fullname",			CT_STRING,	&fullname },
		{ "cusip",				CT_STRING,	&cusip },
		{ "fraction",			CT_INT,		&fraction },
		{ "use_quote_source",	CT_INT,		&quote_flag },
		{ "quote_source",		CT_STRING,	&quote_source },
		{ "quote_tz",			CT_STRING,	&quote_tz },
		{ NULL }
	};

	(void)gnc_gda_do_db_operation( be,
						(inst->do_free ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
						"commodities",
						col_conversion_table );
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
		load_commodities			/* initial_load */
	};

	qof_object_register_backend( GNC_ID_COMMODITY, GNC_GDA_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
