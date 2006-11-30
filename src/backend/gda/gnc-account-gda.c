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
#include "gnc-commodity-gda.h"
#include "gnc-slots-gda.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

#define TABLE_NAME "accounts"

static gpointer get_commodity( gpointer pObject );
static void set_commodity( gpointer pObject, const gpointer pValue );
static gpointer get_parent( gpointer pObject );
static void set_parent( gpointer pObject, const gpointer pValue );

#define ACCOUNT_MAX_NAME_LEN 50
#define ACCOUNT_MAX_TYPE_LEN 50
#define ACCOUNT_MAX_CODE_LEN 100
#define ACCOUNT_MAX_DESCRIPTION_LEN 500

static col_cvt_t col_table[] =
{
	{ "guid",			CT_GUID,	0, COL_NNUL|COL_PKEY,	NULL,
			(GNC_GDA_FN_GETTER)qof_entity_get_guid,
			(GNC_GDA_FN_SETTER)xaccAccountSetGUID },
	{ "name",			CT_STRING,	ACCOUNT_MAX_NAME_LEN, COL_NNUL,	ACCOUNT_NAME_ },
	{ "account_type",	CT_STRING,	ACCOUNT_MAX_TYPE_LEN, COL_NNUL,	ACCOUNT_TYPE_ },
	{ "commodity_guid",	CT_GUID,	0, COL_NNUL,	NULL,
			get_commodity, set_commodity },
	{ "parent_guid",	CT_GUID,	0, 0,	NULL, get_parent, set_parent },
	{ "code",			CT_STRING,	ACCOUNT_MAX_CODE_LEN, 0,	ACCOUNT_CODE_ },
	{ "description",	CT_STRING,	ACCOUNT_MAX_DESCRIPTION_LEN, 0,	ACCOUNT_DESCRIPTION_ },
	{ NULL }
};

/* ================================================================= */
static gpointer
get_commodity( gpointer pObject )
{
	Account* pAccount = (Account*)pObject;

	return (gpointer)qof_instance_get_guid(
						(QofInstance*)xaccAccountGetCommodity( pAccount ) );
}

static void 
set_commodity( gpointer pObject, const gpointer pValue )
{
	Account* pAccount = (Account*)pObject;
	QofBook* pBook = qof_instance_get_book( (QofInstance*)pAccount );
	gnc_commodity* pCommodity;
	GUID* guid = (GUID*)pValue;

	pCommodity = gnc_commodity_find_commodity_by_guid( guid, pBook );
	xaccAccountSetCommodity( pAccount, pCommodity );
}

static gpointer
get_parent( gpointer pObject )
{
	const Account* pAccount = (const Account*)pObject;
	Account* pParent = xaccAccountGetParentAccount( pAccount );
	const GUID* parent_guid;

	if( pParent == NULL ) {
		parent_guid = NULL;
	} else {
		parent_guid = qof_instance_get_guid( (QofInstance*)pParent );
	}

	return (gpointer)parent_guid;
}

static void 
set_parent( gpointer pObject, const gpointer pValue )
{
	Account* pAccount = (Account*)pObject;
	QofBook* pBook = qof_instance_get_book( (QofInstance*)pAccount );
	GUID* guid = (GUID*)pValue;
	Account* pParent;
	
	if( guid != NULL ) {
		pParent = xaccAccountLookup( guid, pBook );
		if( pParent != NULL ) {
			xaccAccountInsertSubAccount( pParent, pAccount );
		}
	}
}

static Account*
load_account( GncGdaBackend* be, GdaDataModel* pModel, int row,
			Account* pAccount )
{
	const GUID* guid;
	GUID acc_guid;

	guid = gnc_gda_load_guid( pModel, row );
	acc_guid = *guid;

	if( pAccount == NULL ) {
		pAccount = xaccAccountLookup( &acc_guid, be->primary_book );
		if( pAccount == NULL ) {
			pAccount = xaccMallocAccount( be->primary_book );
		}
	}
	gnc_gda_load_object( pModel, row, GNC_ID_ACCOUNT, pAccount, col_table );
	gnc_gda_slots_load( be, xaccAccountGetGUID( pAccount ),
							qof_instance_get_slots( (QofInstance*)pAccount ) );

	qof_instance_mark_clean( (QofInstance*)pAccount );

	return pAccount;
}

static void
load_accounts( GncGdaBackend* be )
{
	static GdaQuery* query = NULL;
	GdaObject* ret;
	QofBook* pBook = be->primary_book;
	gnc_commodity_table* pTable = gnc_commodity_table_get_table( pBook );

	/* First time, create the query */
	if( query == NULL ) {
		query = gnc_gda_create_select_query( be, TABLE_NAME );
	}

	ret = gnc_gda_execute_query( be, query );
	if( GDA_IS_DATA_MODEL( ret ) ) {
		GdaDataModel* pModel = (GdaDataModel*)ret;
		int numRows = gda_data_model_get_n_rows( pModel );
		int r;
		Account* pAccount;

		for( r = 0; r < numRows; r++ ) {

			pAccount = load_account( be, pModel, r, NULL );

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
create_account_tables( GncGdaBackend* be )
{
	gnc_gda_create_table_if_needed( be, TABLE_NAME, col_table );
}

/* ================================================================= */
static void
commit_account( GncGdaBackend* be, QofInstance* inst )
{
	Account* pAcc = (Account*)inst;
	const GUID* guid;

	// Ensure the commodity is in the db
	gnc_gda_save_commodity( be, xaccAccountGetCommodity( pAcc ) );

	(void)gnc_gda_do_db_operation( be,
							(inst->do_free ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
							TABLE_NAME,
							GNC_ID_ACCOUNT, pAcc,
							col_table );

	// Delete old slot info
	guid = qof_instance_get_guid( inst );

	// Now, commit or delete any slots
	if( !inst->do_free ) {
		gnc_gda_slots_save( be, guid, qof_instance_get_slots( inst ) );
	} else {
		gnc_gda_slots_delete( be, guid );
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
		load_accounts,				/* initial_load */
		create_account_tables		/* create_tables */
	};

	qof_object_register_backend( GNC_ID_ACCOUNT, GNC_GDA_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
