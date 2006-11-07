/********************************************************************
 * gnc-account-gda.c: load and save data to SQL via libgda          *
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
/** @file gnc-account-gda.c
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
#include "TransLog.h"
#include "gnc-engine.h"
#include "Group.h"
#include "AccountP.h"

#include "gnc-filepath-utils.h"

#include "gnc-backend-gda.h"
#include "gnc-gconf-utils.h"

#include "gnc-account-gda.h"

#ifndef HAVE_STRPTIME
# include "strptime.h"
#endif

/* callback structure */
typedef struct {
	gboolean ok;
	GdaConnection* pConnection;
	QofInstance* inst;
} gda_backend;

static QofLogModule log_module = GNC_MOD_BACKEND;

/* ================================================================= */
static Account*
load_account( QofBook* pBook, GdaDataModel* pModel, int row )
{
	int numCols = gda_data_model_get_n_columns( pModel );
	int col;
	const GValue* val;
	Account* pAccount;
	GUID guid;
	const char* name = NULL;
	int type = ACCT_TYPE_INVALID;
	GUID commodity_guid;
	GUID parent_guid;
	Account* pParent = NULL;
	const char* code = NULL;
	const char* description = NULL;
	const char* s;
	gnc_commodity* pCommodity;

	for( col = 0; col < numCols; col++ ) {
		val = gda_data_model_get_value_at( pModel, col, row );
		
		switch( col ) {
			case 0:	/* guid */
				s = g_value_get_string( val );
				string_to_guid( s, &guid );
				break;
			case 1: /* name */
				name = g_value_get_string( val );
				break;
			case 2: /* type */
				type = g_value_get_int( val );
				break;
			case 3: /* commodity_guid */
				s = g_value_get_string( val );
				string_to_guid( s, &commodity_guid );
				pCommodity = gnc_commodity_find_commodity_by_guid( &commodity_guid, pBook );
				break;
			case 4: /* parent_guid */
				s = g_value_get_string( val );
				string_to_guid( s, &parent_guid );
				pParent = xaccAccountLookup( &parent_guid, pBook );
				break;
			case 5: /* code */
				code = g_value_get_string( val );
				break;
			case 6: /* description */
				description = g_value_get_string( val );
				break;
			default:	/* too many cols */
				*(char*)0 = 0;
		}
	}

	pAccount = xaccMallocAccount( pBook );
	xaccAccountSetGUID( pAccount, &guid );
	xaccAccountSetName( pAccount, name );
	xaccAccountSetType( pAccount, type );
	xaccAccountSetCode( pAccount, code );
	xaccAccountSetDescription( pAccount, description );
	xaccAccountSetCommodity( pAccount, pCommodity );

	if( pParent != NULL ) {
		xaccAccountInsertSubAccount( pParent, pAccount );
	}

	return pAccount;
}

static void
load_accounts( GncGdaBackend* be, QofBook* pBook )
{
	GError* error = NULL;

	GdaQuery* query;
	GdaObject* ret;
	gnc_commodity_table* pTable = gnc_commodity_table_get_table( pBook );

	query = gda_query_new_from_sql( be->pDict, "SELECT * FROM accounts", &error );
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
		Account* pAccount;

		for( r = 0; r < numRows; r++ ) {

			pAccount = load_account( pBook, pModel, r );

			if( pAccount != NULL ) {
				if( xaccAccountGetParent( pAccount ) == NULL ) {
        			xaccGroupInsertAccount(
									gnc_book_get_group( pBook ),
									pAccount );
				}
			}
		}
	}
}

/* ================================================================= */
static gboolean
account_exists_in_db( GncGdaBackend* be, const char* guid )
{
	char cmdbuf[400];
	int count;

	sprintf( cmdbuf, "SELECT * FROM accounts WHERE guid='%s';", guid );
	count = gnc_gda_execute_select_get_count( be, cmdbuf );
	if( count == 0 ) {
		return FALSE;
	} else {
		return TRUE;
	}
}

static void
commit_account( GncGdaBackend* be, QofInstance* inst )
{
	Account* pAcc = (Account*)inst;
	Account* pParent = xaccAccountGetParentAccount( pAcc );
	gnc_commodity* c;
	const GUID* guid = xaccAccountGetGUID( pAcc );
	char guid_buf[GUID_ENCODING_LENGTH+1];
	char commodity_guid_buf[GUID_ENCODING_LENGTH+1];
	const GUID* parent_guid;
	char parent_guid_buf[GUID_ENCODING_LENGTH+1];
	char cmdbuf[300];
	const char* name = xaccAccountGetName(pAcc);
	const char* code = xaccAccountGetCode(pAcc);
	const char* description = xaccAccountGetDescription(pAcc);
	GNCAccountType type = xaccAccountGetType(pAcc);

	c = xaccAccountGetCommodity(pAcc);

	(void)guid_to_string_buff( guid, guid_buf );
	(void)guid_to_string_buff( qof_instance_get_guid( (QofInstance*)c ),
								commodity_guid_buf );
	if( pParent == NULL ) {
		parent_guid_buf[0] = '\0';
	} else {
		parent_guid = xaccAccountGetGUID( pParent );
		(void)guid_to_string_buff( parent_guid, parent_guid_buf );
	}

	if( inst->do_free ) {
		sprintf( cmdbuf, "DELETE FROM accounts WHERE guid='%s';", guid_buf );
		printf( "%s\n", cmdbuf );
		gnc_gda_execute_sql( be, cmdbuf );
	} else {
		if( account_exists_in_db( be, guid_buf ) ) {
			sprintf( cmdbuf, "UPDATE accounts set name='%s',account_type_id=%d,commodity_guid='%s',parent_guid='%s',code='%s',description='%s' WHERE guid='%s';\n",
				name, type, commodity_guid_buf, parent_guid_buf, code, description,
				guid_buf );
		} else {
			sprintf( cmdbuf, "INSERT INTO accounts VALUES('%s','%s',%d,'%s','%s','%s','%s');\n",
				guid_buf,name, type, commodity_guid_buf, parent_guid_buf, code, description );
		}
		printf( "%s\n", cmdbuf );
		gnc_gda_execute_sql( be, cmdbuf );
	}
}

/* ================================================================= */
void
gnc_gda_init_account_handler( void )
{
	static GncGdaDataType_t be_data =
	{
		GNC_GDA_BACKEND_VERSION,
		GNC_ID_ACCOUNT,
		commit_account,				/* commit */
		load_accounts				/* initial_load */
	};

	qof_object_register_backend( GNC_ID_ACCOUNT, GNC_GDA_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
