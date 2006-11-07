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
#include <glib/gi18n.h>
#include <libintl.h>
#include <locale.h>
#include <stdio.h>
#include <fcntl.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <dirent.h>
#include <time.h>
#include <libgda/libgda.h>

#include "qof.h"
#include "qofquery-p.h"
#include "qofquerycore-p.h"
#include "qofinstance-p.h"
#include "TransLog.h"
#include "gnc-engine.h"

#include "gnc-filepath-utils.h"

#include "gnc-backend-gda.h"
#include "gnc-gconf-utils.h"

#include "gnc-commodity-gda.h"

#ifndef HAVE_STRPTIME
# include "strptime.h"
#endif

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
load_commodity( QofBook* pBook, GdaDataModel* pModel, int row )
{
	int numCols = gda_data_model_get_n_columns( pModel );
	int col;
	const GValue* val;
	gnc_commodity* pCommodity;
	GUID guid;
	const char* namespace = NULL;
	const char* mnemonic = NULL;
	const char* fullname = NULL;
	const char* cusip = NULL;
	int fraction = 1;
	gboolean quote_flag = FALSE;
	gnc_quote_source* quote_source = NULL;
	const char* quote_tz = NULL;
	const char* s;

	for( col = 0; col < numCols; col++ ) {
		val = gda_data_model_get_value_at( pModel, col, row );
		
		switch( col ) {
			case 0:	/* guid */
				s = g_value_get_string( val );
				string_to_guid( s, &guid );
				break;
			case 1: /* namespace */
				namespace = g_value_get_string( val );
				break;
			case 2: /* mnemonic */
				mnemonic = g_value_get_string( val );
				break;
			case 3: /* fullname */
				fullname = g_value_get_string( val );
				break;
			case 4: /* cusip */
				cusip = g_value_get_string( val );
				break;
			case 5: /* fraction */
				fraction = g_value_get_int( val );
				break;
			case 6: /* use_quote_source */
				quote_flag = g_value_get_boolean( val );
				break;
			case 7: /* quote_source */
				quote_source = gnc_quote_source_lookup_by_internal( g_value_get_string( val ) );
				break;
			case 8: /* quote_tz */
				quote_tz = g_value_get_string( val );
				break;
			default:	/* too many cols */
				*(char*)0 = 0;
		}
	}

	pCommodity = gnc_commodity_new( pBook, fullname, namespace, mnemonic, cusip, fraction );
	memcpy( &((QofInstance*)pCommodity)->entity.guid, &guid, sizeof( GUID ) );
	gnc_commodity_set_quote_flag( pCommodity, quote_flag );
	gnc_commodity_set_quote_source( pCommodity, quote_source );
	gnc_commodity_set_quote_tz( pCommodity, quote_tz );

	return pCommodity;
}

static void
load_commodities( GncGdaBackend* be, QofBook* pBook )
{
	GError* error = NULL;

	GdaQuery* query;
	GdaObject* ret;
	gnc_commodity_table* pTable = gnc_commodity_table_get_table( pBook );

	query = gda_query_new_from_sql( be->pDict, "SELECT * FROM commodities", &error );
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

			pCommodity = load_commodity( pBook, pModel, r );

			if( pCommodity != NULL ) {
				gnc_commodity_table_insert( pTable, pCommodity );
			}
		}
	}
}
/* ================================================================= */
static gboolean
commodity_exists_in_db( GncGdaBackend* be, const char* guid )
{
	char cmdbuf[400];
	int count;

	sprintf( cmdbuf, "SELECT * FROM commodities WHERE guid='%s';", guid );
	count = gnc_gda_execute_select_get_count( be, cmdbuf );
	if( count == 0 ) {
		return FALSE;
	} else {
		return TRUE;
	}
}

static void
commit_commodity( GncGdaBackend* be, QofInstance* inst )
{
	gnc_commodity* pCommodity = (gnc_commodity*)inst;
	const char* mnemonic = gnc_commodity_get_mnemonic(pCommodity);
	const char* namespace = gnc_commodity_get_namespace(pCommodity);
	const char* fullname = gnc_commodity_get_fullname(pCommodity);
	const char* cusip = gnc_commodity_get_cusip(pCommodity);
	int fraction = gnc_commodity_get_fraction(pCommodity);
	const char* quote_source = gnc_quote_source_get_user_name(gnc_commodity_get_quote_source(pCommodity));
	const char* quote_tz = gnc_commodity_get_quote_tz(pCommodity);
	char guid[GUID_ENCODING_LENGTH+1];
	char cmdbuf[1000];
	gboolean quote_flag = gnc_commodity_get_quote_flag(pCommodity);

	guid_to_string_buff( qof_instance_get_guid( inst ), guid );

	if( inst->do_free ) {
		sprintf( cmdbuf, "DELETE FROM commodities WHERE guid='%s';\n", guid );
		printf( "%s\n", cmdbuf );
		gnc_gda_execute_sql( be, cmdbuf );
	} else {
		if( commodity_exists_in_db( be, guid ) ) {
			sprintf( cmdbuf,
				"UPDATE commodities set namespace='%s',mnemonic='%s',fullname='%s', cusip='%s',fraction=%d,use_quote_source=%d,quote_source='%s',quote_tz='%s' WHERE guid='%s';",
			namespace, mnemonic, fullname, cusip, fraction, quote_flag, quote_source, quote_tz, guid );
		} else {
			sprintf( cmdbuf,
				"INSERT INTO commodities VALUES('%s', '%s', '%s','%s','%s',%d,'%d','%s','%s')\n",
				guid, namespace, mnemonic, fullname, cusip, fraction, quote_flag, quote_source, quote_tz );
		}
		printf( "%s\n", cmdbuf );
		gnc_gda_execute_sql( be, cmdbuf );
    }
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
