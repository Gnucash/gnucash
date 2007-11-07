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

static QofLogModule log_module = GNC_MOD_BACKEND;

#define TABLE_NAME "accounts"

static gpointer get_commodity( gpointer pObject, const QofParam* );
static void set_commodity( gpointer pObject, gpointer pValue );
static gpointer get_parent( gpointer pObject, const QofParam* );
static void set_parent( gpointer pObject, gpointer pValue );
static void set_parent_guid( gpointer pObject, gpointer pValue );

#define ACCOUNT_MAX_NAME_LEN 50
#define ACCOUNT_MAX_TYPE_LEN 50
#define ACCOUNT_MAX_CODE_LEN 100
#define ACCOUNT_MAX_DESCRIPTION_LEN 500

static col_cvt_t col_table[] =
{
    { "guid",		CT_GUID,	0, COL_NNUL|COL_PKEY,	NULL, NULL,
            (QofAccessFunc)qof_instance_get_guid,
            (QofSetterFunc)xaccAccountSetGUID },
    { "name",		CT_STRING, ACCOUNT_MAX_NAME_LEN, COL_NNUL, "name" },
    { "account_type",	CT_STRING, ACCOUNT_MAX_TYPE_LEN, COL_NNUL, NULL, ACCOUNT_TYPE_ },
    { "commodity_guid",	CT_GUID,	0, COL_NNUL,	NULL, NULL,
            get_commodity, set_commodity },
    { "parent_guid",	CT_GUID,	0, 0,	NULL, NULL, get_parent, set_parent },
    { "code",		CT_STRING, ACCOUNT_MAX_CODE_LEN,	0, "code" },
    { "description",	CT_STRING, ACCOUNT_MAX_DESCRIPTION_LEN, 0, "description" },
    { NULL }
};
static col_cvt_t parent_col_table[] =
{
    { "parent_guid",	CT_GUID,	0, 0,	NULL, NULL, NULL, set_parent_guid },
    { NULL }
};

typedef struct {
	Account* pAccount;
	GUID guid;
} account_parent_guid_struct;

/* ================================================================= */
static gpointer
get_commodity( gpointer pObject, const QofParam* param )
{
    const Account* pAccount = GNC_ACCOUNT(pObject);

    return (gpointer)qof_instance_get_guid(
                        QOF_INSTANCE(xaccAccountGetCommodity( pAccount )) );
}

static void 
set_commodity( gpointer pObject, gpointer pValue )
{
    Account* pAccount = GNC_ACCOUNT(pObject);
    QofBook* pBook = qof_instance_get_book( QOF_INSTANCE(pAccount) );
    gnc_commodity* pCommodity;
    GUID* guid = (GUID*)pValue;

    pCommodity = gnc_commodity_find_commodity_by_guid( guid, pBook );
    xaccAccountSetCommodity( pAccount, pCommodity );
}

static gpointer
get_parent( gpointer pObject, const QofParam* param )
{
    const Account* pAccount = GNC_ACCOUNT(pObject);
    const Account* pParent = gnc_account_get_parent( pAccount );
    const GUID* parent_guid;

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
    Account* pAccount = GNC_ACCOUNT(pObject);
    QofBook* pBook = qof_instance_get_book( QOF_INSTANCE(pAccount) );
    GUID* guid = (GUID*)pValue;
    Account* pParent;
    
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

	s->guid = *guid;
}

static void
load_balances( GncGdaBackend* be, Account* pAccount )
{
    gnc_numeric start_balance;
	gnc_numeric cleared_balance;
	gnc_numeric reconciled_balance;

	gnc_gda_get_account_balances( be, pAccount, &start_balance, &cleared_balance, &reconciled_balance );

    g_object_set( pAccount,
				"end-balance", &start_balance,
                "end-cleared-balance", &cleared_balance,
                "end-reconciled-balance", &reconciled_balance,
                NULL);
}

static Account*
load_single_account( GncGdaBackend* be, GdaDataModel* pModel, int row,
				GList** l_accounts_needing_parents )
{
    const GUID* guid;
    GUID acc_guid;
	Account* pAccount;

    guid = gnc_gda_load_guid( pModel, row );
    acc_guid = *guid;

    pAccount = xaccAccountLookup( &acc_guid, be->primary_book );
    if( pAccount == NULL ) {
        pAccount = xaccMallocAccount( be->primary_book );
    }
    gnc_gda_load_object( pModel, row, GNC_ID_ACCOUNT, pAccount, col_table );
    gnc_gda_slots_load( be, xaccAccountGetGUID( pAccount ),
                        qof_instance_get_slots( QOF_INSTANCE(pAccount) ) );
    load_balances( be, pAccount );

    qof_instance_mark_clean( QOF_INSTANCE(pAccount) );

	/* If we don't have a parent, it might be because the parent account hasn't
	   been loaded yet.  Remember the account and its parent guid for later. */
	if( gnc_account_get_parent( pAccount ) == NULL ) {
		account_parent_guid_struct* s = g_slice_new( account_parent_guid_struct );
		s->pAccount = pAccount;
		gnc_gda_load_object( pModel, row, GNC_ID_ACCOUNT, s, parent_col_table );
		*l_accounts_needing_parents = g_list_prepend( *l_accounts_needing_parents, s );
	}

    return pAccount;
}

static void
load_all_accounts( GncGdaBackend* be )
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
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;
        Account* pAccount;
        Account* parent;
		GList* l_accounts_needing_parents = NULL;

        for( r = 0; r < numRows; r++ ) {
            pAccount = load_single_account( be, pModel, r, &l_accounts_needing_parents );

#if 0
            if( pAccount != NULL ) {

                /* Backwards compatibility.  If there's no parent, see if
                 * this account is of type ROOT.  If not, find or create a
                 * ROOT account and make that the parent. */
                parent = gnc_account_get_parent( pAccount );
                if( parent == NULL ) {
                    int type;

                    type = xaccAccountGetType( pAccount );
                    if( type != ACCT_TYPE_ROOT ) {
                        Account* root;
                        root = gnc_book_get_root_account( pBook );
                        if( root == NULL ) {
                            root = gnc_account_create_root( pBook );
                        }
                        gnc_account_append_child( root, pAccount ); 
                    }
                }
            }
#endif
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
                gnc_account_append_child( root, pAccount ); 
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
void
gnc_gda_save_account( GncGdaBackend* be, QofInstance* inst )
{
    Account* pAcc = GNC_ACCOUNT(inst);
    const GUID* guid;

    // If there is no commodity yet, this might be because a new account name has been entered directly
    // into the register and an account window will be opened.  The account info is not complete yet,
    // but the name has been set, triggering this commit
    if( xaccAccountGetCommodity( pAcc ) != NULL ) {
        // Ensure the commodity is in the db
        gnc_gda_save_commodity( be, xaccAccountGetCommodity( pAcc ) );

        (void)gnc_gda_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TABLE_NAME,
                        GNC_ID_ACCOUNT, pAcc,
                        col_table );

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
}
/* ========================== END OF FILE ===================== */
