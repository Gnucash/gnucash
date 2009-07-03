/********************************************************************
 * gnc-transaction-sql.c: load and save data to SQL                 *
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
/** @file gnc-transaction-sql.c
 *  @brief load and save data to SQL 
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */

#include "config.h"

#include <glib.h>

#include "qof.h"
#include "qofquery-p.h"
#include "qofquerycore-p.h"

#include "Account.h"
#include "Transaction.h"
#include "gnc-lot.h"
#include "engine-helpers.h"

#include "gnc-backend-sql.h"
#include "gnc-transaction-sql.h"
#include "gnc-commodity.h"
#include "gnc-commodity-sql.h"
#include "gnc-slots-sql.h"

#include "gnc-engine.h"

#include "escape.h"

#ifdef S_SPLINT_S
#include "splint-defs.h"
#endif

/*@ unused @*/ static QofLogModule log_module = G_LOG_DOMAIN;

#define TRANSACTION_TABLE "transactions"
#define TX_TABLE_VERSION 2
#define SPLIT_TABLE "splits"
#define SPLIT_TABLE_VERSION 3

typedef struct {
    /*@ dependent @*/ GncSqlBackend* be;
    /*@ dependent @*/ const GUID* guid;
	gboolean is_ok;
} split_info_t;

#define TX_MAX_NUM_LEN 2048
#define TX_MAX_DESCRIPTION_LEN 2048

static const GncSqlColumnTableEntry tx_col_table[] =
{
	/*@ -full_init_block @*/
    { "guid",          CT_GUID,           0,                      COL_NNUL|COL_PKEY, "guid" },
    { "currency_guid", CT_COMMODITYREF,   0,                      COL_NNUL,          NULL, NULL,
			(QofAccessFunc)xaccTransGetCurrency, (QofSetterFunc)xaccTransSetCurrency },
    { "num",           CT_STRING,         TX_MAX_NUM_LEN,         COL_NNUL,          NULL, NULL,
			(QofAccessFunc)xaccTransGetNum, (QofSetterFunc)xaccTransSetNum },
    { "post_date",     CT_TIMESPEC,       0,                      COL_NNUL,          NULL, NULL,
			(QofAccessFunc)xaccTransRetDatePostedTS, (QofSetterFunc)gnc_transaction_set_date_posted },
    { "enter_date",    CT_TIMESPEC,       0,                      COL_NNUL,          NULL, NULL,
			(QofAccessFunc)xaccTransRetDateEnteredTS, (QofSetterFunc)gnc_transaction_set_date_entered },
    { "description",   CT_STRING,         TX_MAX_DESCRIPTION_LEN, 0,                 NULL, NULL,
            (QofAccessFunc)xaccTransGetDescription, (QofSetterFunc)xaccTransSetDescription },
    { NULL }
	/*@ +full_init_block @*/
};

static /*@ dependent @*//*@ null @*/ gpointer get_split_reconcile_state( gpointer pObject );
static void set_split_reconcile_state( gpointer pObject, /*@ null @*/ gpointer pValue );
static void set_split_reconcile_date( gpointer pObject, Timespec ts );
static void set_split_lot( gpointer pObject, /*@ null @*/ gpointer pLot );

#define SPLIT_MAX_MEMO_LEN 2048
#define SPLIT_MAX_ACTION_LEN 2048

static const GncSqlColumnTableEntry split_col_table[] =
{
	/*@ -full_init_block @*/
    { "guid",            CT_GUID,         0,                    COL_NNUL|COL_PKEY, "guid" },
    { "tx_guid",         CT_TXREF,        0,                    COL_NNUL,          NULL, SPLIT_TRANS },
    { "account_guid",    CT_ACCOUNTREF,   0,                    COL_NNUL,          NULL, SPLIT_ACCOUNT },
    { "memo",            CT_STRING,       SPLIT_MAX_MEMO_LEN,   COL_NNUL,          NULL, SPLIT_MEMO },
    { "action",          CT_STRING,       SPLIT_MAX_ACTION_LEN, COL_NNUL,          NULL, SPLIT_ACTION },
    { "reconcile_state", CT_STRING,       1,                    COL_NNUL,          NULL, NULL,
			(QofAccessFunc)get_split_reconcile_state, set_split_reconcile_state },
    { "reconcile_date",  CT_TIMESPEC,     0,                    COL_NNUL,          NULL, NULL,
			(QofAccessFunc)xaccSplitRetDateReconciledTS, (QofSetterFunc)set_split_reconcile_date },
    { "value",           CT_NUMERIC,      0,                    COL_NNUL,          NULL, SPLIT_VALUE },
    { "quantity",        CT_NUMERIC,      0,                    COL_NNUL,          NULL, SPLIT_AMOUNT },
	{ "lot_guid",        CT_LOTREF,       0,                    0,                 NULL, NULL,
			(QofAccessFunc)xaccSplitGetLot, set_split_lot },
    { NULL }
	/*@ +full_init_block @*/
};

static const GncSqlColumnTableEntry guid_col_table[] =
{
	/*@ -full_init_block @*/
    { "tx_guid", CT_GUID, 0, 0, "guid" },
    { NULL }
	/*@ +full_init_block @*/
};

/* ================================================================= */

static /*@ dependent @*//*@ null @*/ gpointer
get_split_reconcile_state( gpointer pObject )
{
    static gchar c[2];

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( GNC_IS_SPLIT(pObject), NULL );

    c[0] = xaccSplitGetReconcile( GNC_SPLIT(pObject) );
    c[1] = '\0';
    return (gpointer)c;
}

static void 
set_split_reconcile_state( gpointer pObject, /*@ null @*/ gpointer pValue )
{
    const gchar* s = (const gchar*)pValue;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( GNC_IS_SPLIT(pObject) );
	g_return_if_fail( pValue != NULL );

    xaccSplitSetReconcile( GNC_SPLIT(pObject), s[0] );
}

static void 
set_split_reconcile_date( gpointer pObject, Timespec ts )
{
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( GNC_IS_SPLIT(pObject) );

    xaccSplitSetDateReconciledTS( GNC_SPLIT(pObject), &ts );
}

static void
set_split_lot( gpointer pObject, /*@ null @*/ gpointer pLot )
{
	GNCLot* lot;
	Split* split;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( GNC_IS_SPLIT(pObject) );

	if( pLot == NULL ) return;

	g_return_if_fail( GNC_IS_LOT(pLot) );

	split = GNC_SPLIT(pObject);
	lot = GNC_LOT(pLot);
	gnc_lot_add_split( lot, split );
}

static /*@ null @*/ Split*
load_single_split( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
    GUID split_guid;
	Split* pSplit;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

    guid = gnc_sql_load_guid( be, row );
	if( guid == NULL ) return NULL;
    split_guid = *guid;

    pSplit = xaccSplitLookup( &split_guid, be->primary_book );
    if( pSplit == NULL ) {
        pSplit = xaccMallocSplit( be->primary_book );
    }

    /* If the split is dirty, don't overwrite it */
    if( !qof_instance_is_dirty( QOF_INSTANCE(pSplit) ) ) {
    	gnc_sql_load_object( be, row, GNC_ID_SPLIT, pSplit, split_col_table );
	}

    /*# -ifempty */g_assert( pSplit == xaccSplitLookup( &split_guid, be->primary_book ) );

	return pSplit;
}

static void
load_splits_for_tx_list( GncSqlBackend* be, GList* list )
{
	GString* sql;
	GncSqlResult* result;

	g_return_if_fail( be != NULL );

	if( list == NULL ) return;

	sql = g_string_sized_new( 40+(GUID_ENCODING_LENGTH+3)*g_list_length( list ) );
	g_string_append_printf( sql, "SELECT * FROM %s WHERE %s IN (", SPLIT_TABLE, guid_col_table[0].col_name );
	(void)gnc_sql_append_guid_list_to_sql( sql, list, G_MAXUINT );
	(void)g_string_append( sql, ")" );

	// Execute the query and load the splits
	result = gnc_sql_execute_select_sql( be, sql->str );
    if( result != NULL ) {
		GList* split_list = NULL;
		GncSqlRow* row;

		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
			Split* s;
            s = load_single_split( be, row );
			if( s != NULL ) {
				split_list = g_list_append( split_list, s );
			}
			row = gnc_sql_result_get_next_row( result );
        }

		if( split_list != NULL ) {
			gnc_sql_slots_load_for_list( be, split_list );
			g_list_free( split_list );
		}

		gnc_sql_result_dispose( result );
    }
	(void)g_string_free( sql, TRUE );
}

static /*@ null @*/ Transaction*
load_single_tx( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
    GUID tx_guid;
	Transaction* pTx;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

    guid = gnc_sql_load_guid( be, row );
	if( guid == NULL ) return NULL;
    tx_guid = *guid;

	// Don't overwrite the transaction if it's already been loaded (and possibly modified).
    pTx = xaccTransLookup( &tx_guid, be->primary_book );
    if( pTx != NULL ) {
		return NULL;
    }

    pTx = xaccMallocTransaction( be->primary_book );
    xaccTransBeginEdit( pTx );
    gnc_sql_load_object( be, row, GNC_ID_TRANS, pTx, tx_col_table );

    g_assert( pTx == xaccTransLookup( &tx_guid, be->primary_book ) );

	return pTx;
}

/**
 * Structure to hold start/end balances for each account.  The values are
 * saved before splits are loaded, and then used to adjust the start balances
 * so that the end balances (which are calculated and correct on initial load)
 * are unchanged.
 */
typedef struct {
	/*@ dependent @*/ Account* acc;
	gnc_numeric start_bal;
	gnc_numeric end_bal;
	gnc_numeric start_cleared_bal;
	gnc_numeric end_cleared_bal;
	gnc_numeric start_reconciled_bal;
	gnc_numeric end_reconciled_bal;
} full_acct_balances_t;

/**
 * Saves the start/end balances for an account.
 *
 * @param acc Account
 * @param pData Pointer to balances info list
 */
static void
save_account_balances( Account* acc, gpointer pData )
{
	GSList** pBal_list = (GSList**)pData;
	full_acct_balances_t* newbal;
	gnc_numeric* pstart;
	gnc_numeric* pend;
	gnc_numeric* pstart_c;
	gnc_numeric* pend_c;
	gnc_numeric* pstart_r;
	gnc_numeric* pend_r;

	newbal = g_malloc( (gsize)sizeof( full_acct_balances_t ) );
	g_assert( newbal != NULL );

	newbal->acc = acc;
	g_object_get( acc,
				"start-balance", &pstart,
				"end-balance", &pend,
				"start-cleared-balance", &pstart_c,
				"end-cleared-balance", &pend_c,
				"start-reconciled-balance", &pstart_r,
				"end-reconciled-balance", &pend_r,
				NULL );
	newbal->start_bal = *pstart;
	newbal->end_bal = *pend;
	newbal->start_cleared_bal = *pstart_c;
	newbal->end_cleared_bal = *pend_c;
	newbal->start_reconciled_bal = *pstart_r;
	newbal->end_reconciled_bal = *pend_r;
	*pBal_list = g_slist_append( *pBal_list, newbal );

	g_free( pstart );
	g_free( pend );
	g_free( pstart_c );
	g_free( pend_c );
	g_free( pstart_r );
	g_free( pend_r );
}

/**
 * Executes a transaction query statement and loads the transactions and all
 * of the splits.
 *
 * @param be SQL backend
 * @param stmt SQL statement
 */
static void
query_transactions( GncSqlBackend* be, GncSqlStatement* stmt )
{
    GncSqlResult* result;

	g_return_if_fail( be != NULL );
	g_return_if_fail( stmt != NULL );

    result = gnc_sql_execute_select_statement( be, stmt );
    if( result != NULL ) {
		GList* tx_list = NULL;
		GList* node;
		GncSqlRow* row;
		Transaction* tx;
		GSList* bal_list = NULL;
		GSList* nextbal;
		Account* root = gnc_book_get_root_account( be->primary_book );

		qof_event_suspend();
		xaccAccountBeginEdit( root );

		// Save the start/ending balances (balance, cleared and reconciled) for
		// every account.
		gnc_account_foreach_descendant( gnc_book_get_root_account( be->primary_book ),
										save_account_balances,
										&bal_list );

		// Load the transactions
		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
            tx = load_single_tx( be, row );
			if( tx != NULL ) {
				tx_list = g_list_append( tx_list, tx );
			}
			row = gnc_sql_result_get_next_row( result );
        }
		gnc_sql_result_dispose( result );

		// Load all splits and slots for the transactions
		if( tx_list != NULL ) {
			gnc_sql_slots_load_for_list( be, tx_list );
			load_splits_for_tx_list( be, tx_list );
		}

		// Commit all of the transactions
		for( node = tx_list; node != NULL; node = node->next ) {
			Transaction* pTx = GNC_TRANSACTION(node->data);
    		xaccTransCommitEdit( pTx );
		}

		// Update the account balances based on the loaded splits.  If the end
		// balance has changed, update the start balance so that the end
		// balance is the same as it was before the splits were loaded.
		// Repeat for cleared and reconciled balances.
		for( nextbal = bal_list; nextbal != NULL; nextbal = nextbal->next ) {
			full_acct_balances_t* balns = (full_acct_balances_t*)nextbal->data;
			gnc_numeric* pnew_end_bal;
			gnc_numeric* pnew_end_c_bal;
			gnc_numeric* pnew_end_r_bal;
			gnc_numeric adj;

			g_object_get( balns->acc,
					"end-balance", &pnew_end_bal,
					"end-cleared-balance", &pnew_end_c_bal,
					"end-reconciled-balance", &pnew_end_r_bal,
					NULL );

			if( !gnc_numeric_eq( *pnew_end_bal, balns->end_bal ) ) {
				adj = gnc_numeric_sub( balns->end_bal, *pnew_end_bal,
									GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD );
				balns->start_bal = gnc_numeric_add( balns->start_bal, adj,
									GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD );
				g_object_set( balns->acc, "start-balance", &balns->start_bal, NULL );
			}
			if( !gnc_numeric_eq( *pnew_end_c_bal, balns->end_cleared_bal ) ) {
				adj = gnc_numeric_sub( balns->end_cleared_bal, *pnew_end_c_bal,
									GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD );
				balns->start_cleared_bal = gnc_numeric_add( balns->start_cleared_bal, adj,
									GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD );
				g_object_set( balns->acc, "start-cleared-balance", &balns->start_cleared_bal, NULL );
			}
			if( !gnc_numeric_eq( *pnew_end_r_bal, balns->end_reconciled_bal ) ) {
				adj = gnc_numeric_sub( balns->end_reconciled_bal, *pnew_end_r_bal,
									GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD );
				balns->start_reconciled_bal = gnc_numeric_add( balns->start_reconciled_bal, adj,
									GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD );
				g_object_set( balns->acc, "start-reconciled-balance", &balns->start_reconciled_bal, NULL );
			}
			xaccAccountRecomputeBalance( balns->acc );
			g_free( pnew_end_bal );
			g_free( pnew_end_c_bal );
			g_free( pnew_end_r_bal );
			g_free( balns );
		}
		if( bal_list != NULL ) {
			g_slist_free( bal_list );
		}

		xaccAccountCommitEdit( root );
		qof_event_resume();
    }
}

/* ================================================================= */
/**
 * Creates the transaction and split tables.
 *
 * @param be SQL backend
 */
static void
create_transaction_tables( GncSqlBackend* be )
{
	gint version;
	gboolean ok;

	g_return_if_fail( be != NULL );

	version = gnc_sql_get_table_version( be, TRANSACTION_TABLE );
    if( version == 0 ) {
        (void)gnc_sql_create_table( be, TRANSACTION_TABLE, TX_TABLE_VERSION, tx_col_table );
    } else if( version == 1 ) {
		/* Upgrade 64 bit int handling */
		gnc_sql_upgrade_table( be, TRANSACTION_TABLE, tx_col_table );
		(void)gnc_sql_set_table_version( be, TRANSACTION_TABLE, TX_TABLE_VERSION );
    }

	version = gnc_sql_get_table_version( be, SPLIT_TABLE );
    if( version == 0 ) {
        (void)gnc_sql_create_table( be, SPLIT_TABLE, SPLIT_TABLE_VERSION, split_col_table );
	    ok = gnc_sql_create_index( be, "splits_tx_guid_index", SPLIT_TABLE, guid_col_table );
	    if( !ok ) {
		    PERR( "Unable to create index\n" );
	    }
    } else if( version < SPLIT_TABLE_VERSION ) {

		/* Perform the various upgrades based on the current version number */
	    switch( version ) {
		case 1:
		    /* Upgrade 64 bit int handling */
		    gnc_sql_upgrade_table( be, SPLIT_TABLE, split_col_table );

			/* fallthrough */

		case 2:
			ok = gnc_sql_create_index( be, "splits_tx_guid_index", SPLIT_TABLE, guid_col_table );
			if( !ok ) {
				PERR( "Unable to create index\n" );
			}
		}
		(void)gnc_sql_set_table_version( be, SPLIT_TABLE, SPLIT_TABLE_VERSION );
    }
}
/* ================================================================= */
/**
 * Callback function to delete slots for a split
 *
 * @param data Split
 * @param user_data split_info_t structure contain operation info
 */
static void
delete_split_slots_cb( gpointer data, gpointer user_data )
{
    split_info_t* split_info = (split_info_t*)user_data;
    Split* pSplit = GNC_SPLIT(data);

	g_return_if_fail( data != NULL );
	g_return_if_fail( GNC_IS_SPLIT(data) );
	g_return_if_fail( user_data != NULL );

	if( split_info->is_ok ) {
    	split_info->is_ok = gnc_sql_slots_delete( split_info->be,
                    			qof_instance_get_guid( QOF_INSTANCE(pSplit) ) );
	}
}

/**
 * Deletes all of the splits for a transaction
 *
 * @param be SQL backend
 * @param pTx Transaction
 * @return TRUE if successful, FALSE if unsuccessful
 */
static gboolean
delete_splits( GncSqlBackend* be, Transaction* pTx )
{
    split_info_t split_info;

	g_return_val_if_fail( be != NULL, FALSE );
	g_return_val_if_fail( pTx != NULL, FALSE );

    if( !gnc_sql_do_db_operation( be, OP_DB_DELETE, SPLIT_TABLE,
                                SPLIT_TABLE, pTx, guid_col_table ) ) {
		return FALSE;
	}
    split_info.be = be;
	split_info.is_ok = TRUE;

    g_list_foreach( xaccTransGetSplitList( pTx ), delete_split_slots_cb, &split_info );

	return split_info.is_ok;
}

/**
 * Commits a split to the database
 *
 * @param be SQL backend
 * @param inst Split
 * @return TRUE if successful, FALSE if error
 */
static gboolean
commit_split( GncSqlBackend* be, QofInstance* inst )
{
	gint op;
	gboolean is_infant;
	gboolean is_ok;

	g_return_val_if_fail( inst != NULL, FALSE );
	g_return_val_if_fail( be != NULL, FALSE );

	is_infant = qof_instance_get_infant( inst );
	if( qof_instance_get_destroying( inst ) ) {
		op = OP_DB_DELETE;
	} else if( be->is_pristine_db || is_infant ) {
		op = OP_DB_INSERT;
	} else {
		op = OP_DB_UPDATE;
	}
    is_ok = gnc_sql_do_db_operation( be, op, SPLIT_TABLE, GNC_ID_SPLIT, inst, split_col_table );
	if( is_ok ) {
		is_ok = gnc_sql_slots_save( be,
                        qof_instance_get_guid( inst ),
						is_infant,
                        qof_instance_get_slots( inst ) );
	}

	return is_ok;
}

static void
save_split_cb( gpointer data, gpointer user_data )
{
    split_info_t* split_info = (split_info_t*)user_data;
    Split* pSplit = GNC_SPLIT(data);

	g_return_if_fail( data != NULL );
	g_return_if_fail( GNC_IS_SPLIT(data) );
	g_return_if_fail( user_data != NULL );

	if( split_info->is_ok ) {
    	split_info->is_ok = commit_split( split_info->be, QOF_INSTANCE(pSplit) );
	}
}

static gboolean
save_splits( GncSqlBackend* be, const GUID* tx_guid, SplitList* pSplitList )
{
    split_info_t split_info;

	g_return_val_if_fail( be != NULL, FALSE );
	g_return_val_if_fail( tx_guid != NULL, FALSE );
	g_return_val_if_fail( pSplitList != NULL, FALSE );

    split_info.be = be;
    split_info.guid = tx_guid;
	split_info.is_ok = TRUE;
    g_list_foreach( pSplitList, save_split_cb, &split_info );

	return split_info.is_ok;
}

static gboolean
save_transaction( GncSqlBackend* be, Transaction* pTx, gboolean do_save_splits )
{
    const GUID* guid;
	gint op;
	gboolean is_infant;
	QofInstance* inst;
	gboolean is_ok = TRUE;

	g_return_val_if_fail( be != NULL, FALSE );
	g_return_val_if_fail( pTx != NULL, FALSE );

	inst = QOF_INSTANCE(pTx);
	is_infant = qof_instance_get_infant( inst );
	if( qof_instance_get_destroying( inst ) ) {
		op = OP_DB_DELETE;
	} else if( be->is_pristine_db || is_infant ) {
		op = OP_DB_INSERT;
	} else {
		op = OP_DB_UPDATE;
	}

	if( op != OP_DB_DELETE ) {
    	// Ensure the commodity is in the db
    	is_ok = gnc_sql_save_commodity( be, xaccTransGetCurrency( pTx ) );
	}

	if( is_ok ) {
    	is_ok = gnc_sql_do_db_operation( be, op, TRANSACTION_TABLE, GNC_ID_TRANS, pTx, tx_col_table );
	}

	if( is_ok ) {
    	// Commit slots and splits
    	guid = qof_instance_get_guid( inst );
    	if( !qof_instance_get_destroying(inst) ) {
        	is_ok = gnc_sql_slots_save( be, guid, is_infant, qof_instance_get_slots( inst ) );
			if( is_ok && do_save_splits ) {
				is_ok = save_splits( be, guid, xaccTransGetSplitList( pTx ) );
			}
    	} else {
        	is_ok = gnc_sql_slots_delete( be, guid );
			if( is_ok ) {
    			is_ok = delete_splits( be, pTx );
			}
    	}
	}

	return is_ok;
}

gboolean
gnc_sql_save_transaction( GncSqlBackend* be, QofInstance* inst )
{
	g_return_val_if_fail( be != NULL, FALSE );
	g_return_val_if_fail( inst != NULL, FALSE );
	g_return_val_if_fail( GNC_IS_TRANS(inst), FALSE );

	return save_transaction( be, GNC_TRANS(inst), /* do_save_splits */TRUE );
}

static gboolean
commit_transaction( GncSqlBackend* be, QofInstance* inst )
{
	g_return_val_if_fail( be != NULL, FALSE );
	g_return_val_if_fail( inst != NULL, FALSE );
	g_return_val_if_fail( GNC_IS_TRANS(inst), FALSE );

	return save_transaction( be, GNC_TRANS(inst), /* do_save_splits */FALSE );
}

/* ================================================================= */
static /*@ dependent @*//*@ null @*/ const GUID*
get_guid_from_query( QofQuery* pQuery )
{
    GList* pOrTerms;
    GList* pAndTerms;
    GList* andTerm;
    QofQueryTerm* pTerm;
    QofQueryPredData* pPredData;
    GSList* pParamPath;

	g_return_val_if_fail( pQuery != NULL, NULL );

    pOrTerms = qof_query_get_terms( pQuery );
    pAndTerms = (GList*)pOrTerms->data;
    andTerm = pAndTerms->next;
    pTerm = (QofQueryTerm*)andTerm->data;

    pPredData = qof_query_term_get_pred_data( pTerm );
    pParamPath = qof_query_term_get_param_path( pTerm );

    if( strcmp( pPredData->type_name, "guid" ) == 0 ) {
        query_guid_t pData = (query_guid_t)pPredData;
        return pData->guids->data;
    } else {
        return NULL;
    }
}

/**
 * Loads all transactions for an account.
 *
 * @param be SQL backend
 * @param account Account
 */
void gnc_sql_transaction_load_tx_for_account( GncSqlBackend* be, Account* account )
{
	const GUID* guid;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
	gchar* query_sql;
    GncSqlStatement* stmt;

	g_return_if_fail( be != NULL );
	g_return_if_fail( account != NULL );

	guid = qof_instance_get_guid( QOF_INSTANCE(account) );
    (void)guid_to_string_buff( guid, guid_buf );
	query_sql = g_strdup_printf(
					"SELECT DISTINCT t.* FROM %s AS t, %s AS s WHERE s.tx_guid=t.guid AND s.account_guid ='%s'",
					TRANSACTION_TABLE, SPLIT_TABLE, guid_buf );
	stmt = gnc_sql_create_statement_from_sql( be, query_sql );
	g_free( query_sql );
	if( stmt != NULL ) {
    	query_transactions( be, stmt );
		gnc_sql_statement_dispose( stmt );
	}
}

/**
 * Loads all transactions.  This might be used during a save-as operation to ensure that
 * all data is in memory and ready to be saved.
 *
 * @param be SQL backend
 */
void gnc_sql_transaction_load_all_tx( GncSqlBackend* be )
{
	gchar* query_sql;
    GncSqlStatement* stmt;

	g_return_if_fail( be != NULL );

	query_sql = g_strdup_printf( "SELECT * FROM %s", TRANSACTION_TABLE );
	stmt = gnc_sql_create_statement_from_sql( be, query_sql );
	g_free( query_sql );
	if( stmt != NULL ) {
    	query_transactions( be, stmt );
		gnc_sql_statement_dispose( stmt );
	}
}

static void
convert_query_comparison_to_sql( QofQueryPredData* pPredData, gboolean isInverted, GString* sql )
{
    if( pPredData->how == QOF_COMPARE_LT
			|| ( isInverted && pPredData->how == QOF_COMPARE_GTE ) ) {
        g_string_append( sql, "<" );
    } else if( pPredData->how == QOF_COMPARE_LTE
			|| ( isInverted && pPredData->how == QOF_COMPARE_GT ) ) {
        g_string_append( sql, "<=" );
    } else if( pPredData->how == QOF_COMPARE_EQUAL
			|| ( isInverted && pPredData->how == QOF_COMPARE_NEQ ) ) {
        g_string_append( sql, "=" );
    } else if( pPredData->how == QOF_COMPARE_GT
			|| ( isInverted && pPredData->how == QOF_COMPARE_LTE ) ) {
        g_string_append( sql, ">" );
    } else if( pPredData->how == QOF_COMPARE_GTE
			|| ( isInverted && pPredData->how == QOF_COMPARE_LT ) ) {
        g_string_append( sql, ">=" );
    } else if( pPredData->how == QOF_COMPARE_NEQ
			|| ( isInverted && pPredData->how == QOF_COMPARE_EQUAL ) ) {
        g_string_append( sql, "~=" );
    } else {
		PERR( "Unknown comparison type\n" );
        g_string_append( sql, "??" );
    }
}

static void
convert_query_term_to_sql( const gchar* fieldName, QofQueryTerm* pTerm, GString* sql )
{
    GSList* pParamPath;
    QofQueryPredData* pPredData;
    gboolean isInverted;
    GSList* name;

	g_return_if_fail( pTerm != NULL );
	g_return_if_fail( sql != NULL );

    pParamPath = qof_query_term_get_param_path( pTerm );
    pPredData = qof_query_term_get_pred_data( pTerm );
    isInverted = qof_query_term_is_inverted( pTerm );

    if( safe_strcmp( pPredData->type_name, QOF_TYPE_GUID ) == 0 ) {
        query_guid_t guid_data = (query_guid_t)pPredData;
		GList* guid_entry;

        g_string_append( sql, "(" );
        g_string_append( sql, fieldName );

		switch( guid_data->options ) {
		case QOF_GUID_MATCH_ANY:
		    if( isInverted ) g_string_append( sql, " NOT IN (" );
		    else g_string_append( sql, " IN (" );
			break;

		case QOF_GUID_MATCH_NONE:
		    if( isInverted ) g_string_append( sql, " IN (" );
		    else g_string_append( sql, " NOT IN (" );
			break;

		default:
			PERR( "Unexpected GUID match type: %d\n", guid_data->options );
		}

		for( guid_entry = guid_data->guids; guid_entry != NULL; guid_entry = guid_entry->next ) {
    		gchar guid_buf[GUID_ENCODING_LENGTH+1];

		    if( guid_entry != guid_data->guids ) g_string_append( sql, "," );
        	(void)guid_to_string_buff( guid_entry->data, guid_buf );
        	g_string_append_printf( sql, "'%s'", guid_buf );
		}
		g_string_append( sql, "))" );

    } else if( safe_strcmp( pPredData->type_name, QOF_TYPE_CHAR ) == 0 ) {
	    query_char_t char_data = (query_char_t)pPredData;
		int i;
		
		if( isInverted ) {
		    g_string_append( sql, "NOT(" );
		}
		if( char_data->options == QOF_CHAR_MATCH_NONE ) {
			g_string_append( sql, "NOT " );
		}
		g_string_append( sql, "(" );
		for( i = 0; char_data->char_list[i] != '\0'; i++ ) {
			if( i != 0 ) {
				g_string_append( sql, " OR " );
			}
			g_string_append( sql, fieldName );
			g_string_append( sql, " = '" );
			g_string_append_c( sql, char_data->char_list[i] );
			g_string_append( sql, "'" );
		}
		g_string_append( sql, ") " );
		if( isInverted ) {
			g_string_append( sql, ") " );
		}

    } else if( safe_strcmp( pPredData->type_name, QOF_TYPE_STRING ) == 0 ) {
        query_string_t string_data = (query_string_t)pPredData;
		sqlEscape* escape = sqlEscape_new();

		if( isInverted ) {
			g_string_append( sql, "NOT(" );
		}
		if( pPredData->how == QOF_COMPARE_NEQ ) {
			g_string_append( sql, "NOT(" );
		}
		g_string_append( sql, fieldName );
		if( string_data->is_regex || string_data->options == QOF_STRING_MATCH_CASEINSENSITIVE ) {
			PWARN( "String is_regex || option = QOF_STRING_MATCH_INSENSITIVE\n" );
		}
//			g_string_append( sql, " ~" );
//		} else {
			g_string_append( sql, " =" );
//		}
//		if( string_data->options == QOF_STRING_MATCH_CASEINSENSITIVE ) {
//			g_string_append( sql, "*" );
//		}
        g_string_append( sql, "'" );
        g_string_append( sql, sqlEscapeString( escape, string_data->matchstring ) );
        g_string_append( sql, "'" );
		if( pPredData->how == QOF_COMPARE_NEQ ) {
			g_string_append( sql, ")" );
		}
		if( isInverted ) {
			g_string_append( sql, ")" );
		}
		sqlEscape_destroy( escape );

	} else {
    	g_string_append( sql, "(" );
    	g_string_append( sql, fieldName );
		convert_query_comparison_to_sql( pPredData, isInverted, sql );

    	if( strcmp( pPredData->type_name, QOF_TYPE_NUMERIC ) == 0 ) {
        	query_numeric_t pData = (query_numeric_t)pPredData;
			double d = gnc_numeric_to_double( pData->amount );
    
        	g_string_append_printf( sql, "%f", d );

    	} else if( safe_strcmp( pPredData->type_name, QOF_TYPE_DATE ) == 0 ) {
        	query_date_t date_data = (query_date_t)pPredData;
			gchar* datebuf;

			datebuf = gnc_sql_convert_timespec_to_string( date_data->date );
        	g_string_append_printf( sql, "'%s'", datebuf );

    	} else if( strcmp( pPredData->type_name, QOF_TYPE_INT32 ) == 0 ) {
        	query_int32_t pData = (query_int32_t)pPredData;

        	g_string_append_printf( sql, "%d", pData->val );

    	} else if( strcmp( pPredData->type_name, QOF_TYPE_INT64 ) == 0 ) {
        	query_int64_t pData = (query_int64_t)pPredData;
    
        	g_string_append_printf( sql, "%" G_GINT64_FORMAT, pData->val );

    	} else if( strcmp( pPredData->type_name, QOF_TYPE_DOUBLE ) == 0 ) {
        	query_double_t pData = (query_double_t)pPredData;

        	g_string_append_printf( sql, "%f", pData->val );

    	} else if( strcmp( pPredData->type_name, QOF_TYPE_BOOLEAN ) == 0 ) {
        	query_boolean_t pData = (query_boolean_t)pPredData;

        	g_string_append_printf( sql, "%d", pData->val );

    	} else {
        	PERR( "Unknown query predicate type: %s\n", pPredData->type_name );
    	}

    	g_string_append( sql, ")" );
	}
}

typedef struct {
    GncSqlStatement* stmt;
	gboolean has_been_run;
} split_query_info_t;

static /*@ null @*/ gpointer
compile_split_query( GncSqlBackend* be, QofQuery* query )
{
    const GUID* acct_guid;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
	split_query_info_t* query_info = NULL;
	gchar* query_sql;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( query != NULL, NULL );

	query_info = g_malloc( (gsize)sizeof(split_query_info_t) );
	g_assert( query_info != NULL );
	query_info->has_been_run = FALSE;

	if( qof_query_has_terms( query ) ) {
        GList* orterms = qof_query_get_terms( query );
        GList* orTerm;
		GString* sql = g_string_new( "" );
		gboolean need_OR = FALSE;

        for( orTerm = orterms; orTerm != NULL; orTerm = orTerm->next ) {
            GList* andterms = (GList*)orTerm->data;
            GList* andTerm;
			gboolean need_AND = FALSE;
			gboolean has_tx_guid_check = FALSE;

            if( need_OR ) {
				g_string_append( sql, " OR " );
			}
            g_string_append( sql, "(" );
            for( andTerm = andterms; andTerm != NULL; andTerm = andTerm->next ) {
				QofQueryTerm* term;
				GSList* paramPath;
				gboolean unknownPath = FALSE;

                term = (QofQueryTerm*)andTerm->data;
				paramPath = qof_query_term_get_param_path( term );

				if( strcmp( paramPath->data, QOF_PARAM_BOOK ) == 0 ) continue;

                if( need_AND ) g_string_append( sql, " AND " );

				if( strcmp( paramPath->data, SPLIT_ACCOUNT ) == 0
						&& strcmp( paramPath->next->data, QOF_PARAM_GUID ) == 0 ) {
                	convert_query_term_to_sql( "s.account_guid", term, sql );

				} else if( strcmp( paramPath->data, SPLIT_RECONCILE ) == 0 ) {
                	convert_query_term_to_sql( "s.reconcile_state", term, sql );

				} else if( strcmp( paramPath->data, SPLIT_TRANS ) == 0 ) {
#if 0
					if( !has_tx_guid_check ) {
						g_string_append( sql, "(splits.tx_guid = transactions.guid) AND " );
						has_tx_guid_check = TRUE;
					}
#endif
					if( strcmp( paramPath->next->data, TRANS_DATE_POSTED ) == 0 ) {
				        convert_query_term_to_sql( "t.post_date", term, sql );
					} else if( strcmp( paramPath->next->data, TRANS_DESCRIPTION ) == 0 ) {
					    convert_query_term_to_sql( "t.description", term, sql );
					} else {
						unknownPath = TRUE;
					}

				} else if( strcmp( paramPath->data, SPLIT_VALUE ) == 0 ) {
                	convert_query_term_to_sql( "s.value_num/s.value_denom", term, sql );

				} else {
					unknownPath = TRUE;
				}

				if( unknownPath ) {
				    GString* name = g_string_new( (gchar*)paramPath->data );
					while( paramPath->next != NULL ) {
					    g_string_append( name, "." );
						g_string_append( name, paramPath->next->data );
						paramPath = paramPath->next;
					}
					PERR( "Unknown SPLIT query field: %s\n", name->str );
					g_string_free( name, TRUE );
				}
				need_AND = TRUE;
            }

			/* If the last char in the string is a '(', then for some reason, there were
			   no terms added to the SQL.  If so, remove it and ignore the OR term. */
			if( sql->str[sql->len-1] == '(' ) {
			    g_string_truncate( sql, sql->len-1 );
				need_OR = FALSE;
			} else {
            	g_string_append( sql, ")" );
				need_OR = TRUE;
			}
        }

		if( sql->len != 0 ) {
			query_sql = g_strdup_printf(
					"SELECT DISTINCT t.* FROM %s AS t, %s AS s WHERE s.tx_guid=t.guid AND %s",
					TRANSACTION_TABLE, SPLIT_TABLE, sql->str );
		} else {
	    	query_sql = g_strdup_printf( "SELECT * FROM %s", TRANSACTION_TABLE );
		}
		query_info->stmt = gnc_sql_create_statement_from_sql( be, query_sql );

		g_string_free( sql, TRUE );
		g_free( query_sql );

	} else {
	    query_sql = g_strdup_printf( "SELECT * FROM %s", TRANSACTION_TABLE );
		query_info->stmt = gnc_sql_create_statement_from_sql( be, query_sql );
		g_free( query_sql );
	}

	return query_info;
}

static void
run_split_query( GncSqlBackend* be, gpointer pQuery )
{
	split_query_info_t* query_info = (split_query_info_t*)pQuery;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pQuery != NULL );

	if( !query_info->has_been_run ) {
    	query_transactions( be, query_info->stmt );
		query_info->has_been_run = TRUE;
		gnc_sql_statement_dispose( query_info->stmt );
		query_info->stmt = NULL;
	}
}

static void
free_split_query( GncSqlBackend* be, gpointer pQuery )
{
	g_return_if_fail( be != NULL );
	g_return_if_fail( pQuery != NULL );

	g_free( pQuery );
}

/* ----------------------------------------------------------------- */
typedef struct {
    /*@ dependent @*/ const GncSqlBackend* be;
	/*@ dependent @*/ Account* acct;
    char reconcile_state;
    gnc_numeric balance;
} single_acct_balance_t;

static void
set_acct_bal_account_from_guid( gpointer pObject, gpointer pValue )
{
    single_acct_balance_t* bal = (single_acct_balance_t*)pObject;
	const GUID* guid = (const GUID*)pValue;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( pValue != NULL );

    bal->acct = xaccAccountLookup( guid, bal->be->primary_book );
}

static void 
set_acct_bal_reconcile_state( gpointer pObject, gpointer pValue )
{
    single_acct_balance_t* bal = (single_acct_balance_t*)pObject;
    const gchar* s = (const gchar*)pValue;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( pValue != NULL );

    bal->reconcile_state = s[0];
}

static void 
set_acct_bal_balance( gpointer pObject, gnc_numeric value )
{
    single_acct_balance_t* bal = (single_acct_balance_t*)pObject;

	g_return_if_fail( pObject != NULL );

    bal->balance = value;
}

static const GncSqlColumnTableEntry acct_balances_col_table[] =
{
	/*@ -full_init_block @*/
    { "account_guid",    CT_GUID,    0, 0, NULL, NULL, NULL, (QofSetterFunc)set_acct_bal_account_from_guid },
    { "reconcile_state", CT_STRING,  1, 0, NULL, NULL, NULL, (QofSetterFunc)set_acct_bal_reconcile_state },
    { "quantity",        CT_NUMERIC, 0, 0, NULL, NULL, NULL, (QofSetterFunc)set_acct_bal_balance },
    { NULL }
	/*@ +full_init_block @*/
};

static /*@ null @*/ single_acct_balance_t*
load_single_acct_balances( const GncSqlBackend* be, GncSqlRow* row )
{
	single_acct_balance_t* bal = NULL;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

	bal = g_malloc( (gsize)sizeof(single_acct_balance_t) );
	g_assert( bal != NULL );

	bal->be = be;
    gnc_sql_load_object( be, row, NULL, bal, acct_balances_col_table );

	return bal;
}

/*@ null @*/ GSList*
gnc_sql_get_account_balances_slist( GncSqlBackend* be )
{
    GncSqlResult* result;
    GncSqlStatement* stmt;
	gchar* buf;
	GSList* bal_slist = NULL;

	g_return_val_if_fail( be != NULL, NULL );

	buf = g_strdup_printf( "SELECT account_guid, reconcile_state, sum(quantity_num) as quantity_num, quantity_denom FROM %s GROUP BY account_guid, reconcile_state, quantity_denom ORDER BY account_guid, reconcile_state",
						SPLIT_TABLE );
	stmt = gnc_sql_create_statement_from_sql( be, buf );
	g_assert( stmt != NULL );
	g_free( buf );
    result = gnc_sql_execute_select_statement( be, stmt );
	gnc_sql_statement_dispose( stmt );
    if( result != NULL ) {
		GncSqlRow* row;
		acct_balances_t* bal = NULL;

		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
			single_acct_balance_t* single_bal;

			// Get the next reconcile state balance and merge with other balances
            single_bal = load_single_acct_balances( be, row );
			if( single_bal != NULL ) {
				if( bal != NULL && bal->acct != single_bal->acct ) {
					bal->cleared_balance = gnc_numeric_add( bal->cleared_balance, bal->reconciled_balance,
													GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD );
					bal->balance = gnc_numeric_add( bal->balance, bal->cleared_balance,
													GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD );
					bal_slist = g_slist_append( bal_slist, bal );
					bal = NULL;
				}
				if( bal == NULL ) {
					bal = g_malloc( (gsize)sizeof(acct_balances_t) );
					g_assert( bal != NULL );

					bal->acct = single_bal->acct;
					bal->balance = gnc_numeric_zero();
					bal->cleared_balance = gnc_numeric_zero();
					bal->reconciled_balance = gnc_numeric_zero();
				}
				if( single_bal->reconcile_state == 'n' ) {
					bal->balance = gnc_numeric_add( bal->balance, single_bal->balance,
													GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD );
				} else if( single_bal->reconcile_state == 'c' ) {
					bal->cleared_balance = gnc_numeric_add( bal->cleared_balance, single_bal->balance,
													GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD );
				} else if( single_bal->reconcile_state == 'y' ) {
					bal->reconciled_balance = gnc_numeric_add( bal->reconciled_balance, single_bal->balance,
													GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD );
				}
				g_free( single_bal );
			}
			row = gnc_sql_result_get_next_row( result );
        }

		// Add the final balance
		if( bal != NULL ) {
			bal->cleared_balance = gnc_numeric_add( bal->cleared_balance, bal->reconciled_balance,
													GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD );
			bal->balance = gnc_numeric_add( bal->balance, bal->cleared_balance,
													GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD );
			bal_slist = g_slist_append( bal_slist, bal );
		}
		gnc_sql_result_dispose( result );
    }

    return bal_slist;
}

/* ----------------------------------------------------------------- */
static void
load_tx_guid( const GncSqlBackend* be, GncSqlRow* row,
            /*@ null @*/ QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    GUID guid;
	Transaction* tx;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
	g_assert( val != NULL );
    (void)string_to_guid( g_value_get_string( val ), &guid );
	tx = xaccTransLookup( &guid, be->primary_book );
    if( table_row->gobj_param_name != NULL ) {
		g_object_set( pObject, table_row->gobj_param_name, tx, NULL );
    } else {
		g_return_if_fail( setter != NULL );
		(*setter)( pObject, (const gpointer)tx );
    }
}

static GncSqlColumnTypeHandler tx_guid_handler
	= { load_tx_guid,
		gnc_sql_add_objectref_guid_col_info_to_list,
		gnc_sql_add_colname_to_list,
        gnc_sql_add_gvalue_objectref_guid_to_slist };
/* ================================================================= */
void
gnc_sql_init_transaction_handler( void )
{
    static GncSqlObjectBackend be_data_tx =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_TRANS,
        commit_transaction,          /* commit */
        NULL,
        create_transaction_tables,   /* create tables */
		NULL,                        /* compile_query */
		NULL,                        /* run_query */
		NULL,                        /* free_query */
		NULL                         /* write */
    };
    static GncSqlObjectBackend be_data_split =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_SPLIT,
        commit_split,                /* commit */
        NULL,                        /* initial_load */
        NULL,                        /* create tables */
        compile_split_query,
        run_split_query,
        free_split_query,
		NULL                         /* write */
    };

    (void)qof_object_register_backend( GNC_ID_TRANS, GNC_SQL_BACKEND, &be_data_tx );
    (void)qof_object_register_backend( GNC_ID_SPLIT, GNC_SQL_BACKEND, &be_data_split );

	gnc_sql_register_col_type_handler( CT_TXREF, &tx_guid_handler );
}

/* ========================== END OF FILE ===================== */
