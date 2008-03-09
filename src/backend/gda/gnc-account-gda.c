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
#include "Account.h"
#include "AccountP.h"
#include "gnc-commodity.h"

#include "gnc-backend-util-gda.h"

#include "gnc-account-gda.h"
#include "gnc-commodity-gda.h"
#include "gnc-slots-gda.h"
#include "gnc-transaction-gda.h"

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "accounts"

static gpointer get_parent( gpointer pObject, const QofParam* );
static void set_parent( gpointer pObject, gpointer pValue );
static void set_parent_guid( gpointer pObject, gpointer pValue );

#define ACCOUNT_MAX_NAME_LEN /*2048*/20
#define ACCOUNT_MAX_TYPE_LEN 2048
#define ACCOUNT_MAX_CODE_LEN 2048
#define ACCOUNT_MAX_DESCRIPTION_LEN 2048

static const col_cvt_t col_table[] =
{
    { "guid",           CT_GUID,         0,                           COL_NNUL|COL_PKEY, "guid" },
    { "name",           CT_STRING,       ACCOUNT_MAX_NAME_LEN,        COL_NNUL,          "name" },
    { "account_type",   CT_STRING,       ACCOUNT_MAX_TYPE_LEN,        COL_NNUL,          NULL, ACCOUNT_TYPE_ },
    { "commodity_guid", CT_COMMODITYREF, 0,                           COL_NNUL,          "commodity" },
    { "parent_guid",    CT_GUID,         0,                           0,                 NULL, NULL, get_parent, set_parent },
    { "code",           CT_STRING,       ACCOUNT_MAX_CODE_LEN,        0,                 "code" },
    { "description",    CT_STRING,       ACCOUNT_MAX_DESCRIPTION_LEN, 0,                 "description" },
    { NULL }
};
static col_cvt_t parent_col_table[] =
{
    { "parent_guid", CT_GUID, 0, 0, NULL, NULL, NULL, set_parent_guid },
    { NULL }
};

typedef struct {
	Account* pAccount;
	GUID guid;
} account_parent_guid_struct;

/* ================================================================= */

static gpointer
get_parent( gpointer pObject, const QofParam* param )
{
    const Account* pAccount;
    const Account* pParent;
    const GUID* parent_guid;

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( GNC_IS_ACCOUNT(pObject), NULL );

    pAccount = GNC_ACCOUNT(pObject);
    pParent = gnc_account_get_parent( pAccount );
    if( pParent == NULL ) {
        parent_guid = NULL;
    } else {
        parent_guid = qof_instance_get_guid( QOF_INSTANCE(pParent) );
    }

    return (gpointer)parent_guid;
}

static void 
set_parent( gpointer pObject, gpointer pValue )
{
    Account* pAccount;
    QofBook* pBook;
    GUID* guid = (GUID*)pValue;
    Account* pParent;
    
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( GNC_IS_ACCOUNT(pObject) );

    pAccount = GNC_ACCOUNT(pObject);
    pBook = qof_instance_get_book( QOF_INSTANCE(pAccount) );
    if( guid != NULL ) {
        pParent = xaccAccountLookup( guid, pBook );
        if( pParent != NULL ) {
            gnc_account_append_child( pParent, pAccount );
        }
    }
}

static void
set_parent_guid( gpointer pObject, gpointer pValue )
{
	account_parent_guid_struct* s = (account_parent_guid_struct*)pObject;
    GUID* guid = (GUID*)pValue;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( pValue != NULL );

	s->guid = *guid;
}

static void
load_balances( GncGdaBackend* be, Account* pAccount )
{
    gnc_numeric start_balance;
	gnc_numeric cleared_balance;
	gnc_numeric reconciled_balance;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pAccount != NULL );

	gnc_gda_get_account_balances( be, pAccount, &start_balance, &cleared_balance, &reconciled_balance );

    g_object_set( pAccount,
				"end-balance", &start_balance,
                "end-cleared-balance", &cleared_balance,
                "end-reconciled-balance", &reconciled_balance,
                NULL);
}

static void
load_account_balances_for_list( GncGdaBackend* be, GList* list )
{
	GList* balance_list;

	g_return_if_fail( be != NULL );

	if( list == NULL ) return;

	balance_list = gnc_gda_get_account_balances_for_list( be, list );
	for( ; balance_list != NULL; balance_list = balance_list->next ) {
		acct_balances_t* acct_balances = (acct_balances_t*)balance_list->data;

    	g_object_set( acct_balances->acct,
					"end-balance", &acct_balances->start_balance,
                	"end-cleared-balance", &acct_balances->cleared_balance,
                	"end-reconciled-balance", &acct_balances->reconciled_balance,
                	NULL);
	}
}

static void
load_single_account( GncGdaBackend* be, GdaDataModel* pModel, int row, GList** pList,
				GList** l_accounts_needing_parents )
{
    const GUID* guid;
    GUID acc_guid;
	Account* pAccount;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );
	g_return_if_fail( l_accounts_needing_parents != NULL );

    guid = gnc_gda_load_guid( be, pModel, row );
    acc_guid = *guid;

    pAccount = xaccAccountLookup( &acc_guid, be->primary_book );
    if( pAccount == NULL ) {
        pAccount = xaccMallocAccount( be->primary_book );
    }
    gnc_gda_load_object( be, pModel, row, GNC_ID_ACCOUNT, pAccount, col_table );
	*pList = g_list_append( *pList, pAccount );
//    gnc_gda_slots_load( be, QOF_INSTANCE(pAccount) );
//    load_balances( be, pAccount );

    qof_instance_mark_clean( QOF_INSTANCE(pAccount) );

	/* If we don't have a parent, it might be because the parent account hasn't
	   been loaded yet.  Remember the account and its parent guid for later. */
	if( gnc_account_get_parent( pAccount ) == NULL ) {
		account_parent_guid_struct* s = g_slice_new( account_parent_guid_struct );
		s->pAccount = pAccount;
		gnc_gda_load_object( be, pModel, row, GNC_ID_ACCOUNT, s, parent_col_table );
		*l_accounts_needing_parents = g_list_prepend( *l_accounts_needing_parents, s );
	}
}

static void
load_all_accounts( GncGdaBackend* be )
{
    static GdaQuery* query = NULL;
    GdaObject* ret;
    QofBook* pBook;
    gnc_commodity_table* pTable;

	g_return_if_fail( be != NULL );

    pBook = be->primary_book;
    pTable = gnc_commodity_table_get_table( pBook );

    /* First time, create the query */
    if( query == NULL ) {
        query = gnc_gda_create_select_query( be, TABLE_NAME );
    }

    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;
        Account* parent;
		GList* l_accounts_needing_parents = NULL;
		GList* list = NULL;

        for( r = 0; r < numRows; r++ ) {
            load_single_account( be, pModel, r, &list, &l_accounts_needing_parents );
        }

		if( list != NULL ) {
			load_account_balances_for_list( be, list );
			gnc_gda_slots_load_for_list( be, list );
		}

		/* While there are items on the list of accounts needing parents,
		   try to see if the parent has now been loaded.  Theory says that if
		   items are removed from the front and added to the back if the
		   parent is still not available, then eventually, the list will
		   shrink to size 0. */
		if( l_accounts_needing_parents != NULL ) {
			gboolean progress_made = TRUE;

			Account* pParent;
			GList* elem;
			
			while( progress_made ) {
				progress_made = FALSE;
				for( elem = l_accounts_needing_parents; elem != NULL; elem = g_list_next( elem ) ) {
					account_parent_guid_struct* s = (account_parent_guid_struct*)elem->data;
					const gchar* name = xaccAccountGetName( s->pAccount );
    				pParent = xaccAccountLookup( &s->guid, be->primary_book );
					if( pParent != NULL ) {
						gnc_account_append_child( pParent, s->pAccount );
						l_accounts_needing_parents = g_list_delete_link( l_accounts_needing_parents, elem );
						progress_made = TRUE;
					}
				}
			}

			/* Any accounts left over must be parented by the root account */
			for( elem = l_accounts_needing_parents; elem != NULL; elem = g_list_next( elem ) ) {
				account_parent_guid_struct* s = (account_parent_guid_struct*)elem->data;
                Account* root;
                root = gnc_book_get_root_account( pBook );
                if( root == NULL ) {
                    root = gnc_account_create_root( pBook );
                }
                gnc_account_append_child( root, s->pAccount ); 
			}
		}
    }
}

/* ================================================================= */
static void
create_account_tables( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    gnc_gda_create_table_if_needed( be, TABLE_NAME, col_table );
}

/* ================================================================= */
void
gnc_gda_save_account( QofInstance* inst, GncGdaBackend* be )
{
    Account* pAcc = GNC_ACCOUNT(inst);
    const GUID* guid;

	g_return_if_fail( be != NULL );
	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_ACCOUNT(inst) );

    // If there is no commodity yet, this might be because a new account name has been entered directly
    // into the register and an account window will be opened.  The account info is not complete yet,
    // but the name has been set, triggering this commit
    if( xaccAccountGetCommodity( pAcc ) != NULL ) {
		gint op;

        // Ensure the commodity is in the db
        gnc_gda_save_commodity( be, xaccAccountGetCommodity( pAcc ) );

		if( qof_instance_get_destroying( inst ) ) {
			op = OP_DB_DELETE;
		} else if( be->is_pristine_db ) {
			op = OP_DB_ADD;
		} else {
			op = OP_DB_ADD_OR_UPDATE;
		}

        (void)gnc_gda_do_db_operation( be, op, TABLE_NAME, GNC_ID_ACCOUNT, pAcc, col_table );

        // Now, commit or delete any slots
        guid = qof_instance_get_guid( inst );
        if( !qof_instance_get_destroying(inst) ) {
            gnc_gda_slots_save( be, guid, qof_instance_get_slots( inst ) );
        } else {
            gnc_gda_slots_delete( be, guid );
        }
    }
}

/* ================================================================= */
static void
load_account_guid( const GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table_row )
{
    const GValue* val;
    GUID guid;
    const GUID* pGuid;
	Account* account = NULL;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    val = gda_data_model_get_value_at_col_name( pModel, table_row->col_name, row );
    if( gda_value_is_null( val ) ) {
        pGuid = NULL;
    } else {
        string_to_guid( g_value_get_string( val ), &guid );
        pGuid = &guid;
    }
	if( pGuid != NULL ) {
		account = xaccAccountLookup( pGuid, be->primary_book );
	}
    if( table_row->gobj_param_name != NULL ) {
		g_object_set( pObject, table_row->gobj_param_name, account, NULL );
    } else {
		(*setter)( pObject, (const gpointer)account );
    }
}

static col_type_handler_t account_guid_handler =
        { load_account_guid, gnc_gda_create_objectref_guid_col,
            gnc_gda_get_gvalue_objectref_guid_for_query, gnc_gda_get_gvalue_objectref_guid_cond };
/* ================================================================= */
void
gnc_gda_init_account_handler( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_ACCOUNT,
        gnc_gda_save_account,				/* commit */
        load_all_accounts,				/* initial_load */
        create_account_tables		/* create_tables */
    };

    qof_object_register_backend( GNC_ID_ACCOUNT, GNC_GDA_BACKEND, &be_data );

	gnc_gda_register_col_type_handler( CT_ACCOUNTREF, &account_guid_handler );
}
/* ========================== END OF FILE ===================== */
