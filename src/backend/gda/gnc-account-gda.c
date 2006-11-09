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
 *  @author Copyright (c) 2006 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db using libgda
 */

#include "config.h"

#include <glib.h>
#include <libgda/libgda.h>

#include "qof.h"
#include "Group.h"
#include "AccountP.h"

#include "gnc-backend-gda.h"

#include "gnc-account-gda.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

/* ================================================================= */
static Account*
load_account( GncGdaBackend* be, GdaDataModel* pModel, int row )
{
	Account* pAccount;
	GUID guid;
	const gchar* name = NULL;
	gint type = ACCT_TYPE_INVALID;
	GUID commodity_guid;
	GUID parent_guid;
	Account* pParent = NULL;
	const gchar* code = NULL;
	const gchar* description = NULL;
	gnc_commodity* pCommodity;
	QofBook* pBook = be->primary_book;

	col_cvt_t col_conversion_table[] =
	{
		{ "guid",				CT_GUID,	&guid },
		{ "name",				CT_STRING,	&name },
		{ "account_type_id",	CT_INT,		&type },
		{ "commodity_guid",		CT_GUID,	&commodity_guid },
		{ "parent_guid",		CT_GUID,	&parent_guid },
		{ "code",				CT_STRING,	&code },
		{ "description",		CT_STRING,	&description },
		{ NULL }
	};

	gnc_gda_load_object( be, pModel, col_conversion_table, row );

	pCommodity = gnc_commodity_find_commodity_by_guid( &commodity_guid, pBook );
	pParent = xaccAccountLookup( &parent_guid, pBook );

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
load_accounts( GncGdaBackend* be )
{
	GError* error = NULL;

	GdaQuery* query;
	GdaObject* ret;
	QofBook* pBook = be->primary_book;
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

			pAccount = load_account( be, pModel, r );

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
static void
commit_account( GncGdaBackend* be, QofInstance* inst )
{
	Account* pAcc = (Account*)inst;
	Account* pParent = xaccAccountGetParentAccount( pAcc );
	gnc_commodity* c;
	const GUID* guid = xaccAccountGetGUID( pAcc );
	gchar guid_buf[GUID_ENCODING_LENGTH+1];
	const GUID* commodity_guid;
	const GUID* parent_guid;
	const gchar* name = xaccAccountGetName(pAcc);
	const gchar* code = xaccAccountGetCode(pAcc);
	const gchar* description = xaccAccountGetDescription(pAcc);
	GNCAccountType type = xaccAccountGetType(pAcc);

	col_cvt_t col_conversion_table[] =
	{
		{ "guid",				CT_GUID,	&guid },
		{ "name",				CT_STRING,	&name },
		{ "account_type_id",	CT_INT,		&type },
		{ "commodity_guid",		CT_GUID,	&commodity_guid },
		{ "parent_guid",		CT_GUID,	&parent_guid },
		{ "code",				CT_STRING,	&code },
		{ "description",		CT_STRING,	&description },
		{ NULL }
	};

	c = xaccAccountGetCommodity(pAcc);
	commodity_guid = qof_instance_get_guid( (QofInstance*)c );

	if( pParent == NULL ) {
		parent_guid = NULL;
	} else {
		parent_guid = xaccAccountGetGUID( pParent );
	}

	(void)gnc_gda_do_db_operation( be,
							(inst->do_free ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
							"accounts",
							col_conversion_table );
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
