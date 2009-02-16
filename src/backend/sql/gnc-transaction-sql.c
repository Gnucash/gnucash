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

static QofLogModule log_module = G_LOG_DOMAIN;

#define TRANSACTION_TABLE "transactions"
#define TX_TABLE_VERSION 1
#define SPLIT_TABLE "splits"
#define SPLIT_TABLE_VERSION 1

typedef struct {
    GncSqlBackend* be;
    const GUID* guid;
	gboolean is_ok;
} split_info_t;

#define TX_MAX_NUM_LEN 2048
#define TX_MAX_DESCRIPTION_LEN 2048

static const GncSqlColumnTableEntry tx_col_table[] =
{
	/*# -fullinitblock */
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
	/*# +fullinitblock */
};

static gpointer get_split_reconcile_state( gpointer pObject, const QofParam* param );
static void set_split_reconcile_state( gpointer pObject, gpointer pValue );
static void set_split_reconcile_date( gpointer pObject, Timespec ts );
static void set_split_lot( gpointer pObject, gpointer pLot );

#define SPLIT_MAX_MEMO_LEN 2048
#define SPLIT_MAX_ACTION_LEN 2048

static const GncSqlColumnTableEntry split_col_table[] =
{
	/*# -fullinitblock */
    { "guid",            CT_GUID,         0,                    COL_NNUL|COL_PKEY, "guid" },
    { "tx_guid",         CT_TXREF,        0,                    COL_NNUL,          NULL, SPLIT_TRANS },
    { "account_guid",    CT_ACCOUNTREF,   0,                    COL_NNUL,          NULL, SPLIT_ACCOUNT },
    { "memo",            CT_STRING,       SPLIT_MAX_MEMO_LEN,   COL_NNUL,          NULL, SPLIT_MEMO },
    { "action",          CT_STRING,       SPLIT_MAX_ACTION_LEN, COL_NNUL,          NULL, SPLIT_ACTION },
    { "reconcile_state", CT_STRING,       1,                    COL_NNUL,          NULL, NULL,
			get_split_reconcile_state, set_split_reconcile_state },
    { "reconcile_date",  CT_TIMESPEC,     0,                    COL_NNUL,          NULL, NULL,
			(QofAccessFunc)xaccSplitRetDateReconciledTS, (QofSetterFunc)set_split_reconcile_date },
    { "value",           CT_NUMERIC,      0,                    COL_NNUL,          NULL, SPLIT_VALUE },
    { "quantity",        CT_NUMERIC,      0,                    COL_NNUL,          NULL, SPLIT_AMOUNT },
	{ "lot_guid",        CT_LOTREF,       0,                    0,                 NULL, NULL,
			(QofAccessFunc)xaccSplitGetLot, set_split_lot },
    { NULL }
	/*# +fullinitblock */
};

static const GncSqlColumnTableEntry guid_col_table[] =
{
	/*# -fullinitblock */
    { "tx_guid", CT_GUID, 0, 0, "guid" },
    { NULL }
	/*# +fullinitblock */
};

static void retrieve_numeric_value( gpointer pObject, gnc_numeric value );

/* ================================================================= */

static gpointer
get_split_reconcile_state( gpointer pObject, const QofParam* param )
{
    const Split* pSplit = GNC_SPLIT(pObject);
    static gchar c[2];

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( GNC_IS_SPLIT(pObject), NULL );

    c[0] = xaccSplitGetReconcile( pSplit );
    c[1] = '\0';
    return (gpointer)c;
}

static void 
set_split_reconcile_state( gpointer pObject, gpointer pValue )
{
    Split* pSplit = GNC_SPLIT(pObject);
    const gchar* s = (const gchar*)pValue;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( GNC_IS_SPLIT(pObject) );
	g_return_if_fail( pValue != NULL );

    xaccSplitSetReconcile( pSplit, s[0] );
}

static void 
set_split_reconcile_date( gpointer pObject, Timespec ts )
{
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( GNC_IS_SPLIT(pObject) );

    xaccSplitSetDateReconciledTS( GNC_SPLIT(pObject), &ts );
}

static void 
retrieve_numeric_value( gpointer pObject, gnc_numeric value )
{
    gnc_numeric* pResult = (gnc_numeric*)pObject;

	g_return_if_fail( pObject != NULL );

    *pResult = value;
}

static void
set_split_lot( gpointer pObject, gpointer pLot )
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

#if 0
// Table to retrieve just the quantity
static GncSqlColumnTableEntry quantity_table[] =
{
	/*# -fullinitblock */
    { "quantity", CT_NUMERIC, 0, COL_NNUL, NULL, NULL, NULL, (QofSetterFunc)retrieve_numeric_value },
    { NULL }
	/*# +fullinitblock */
};
#endif

static Split*
load_single_split( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
    GUID split_guid;
	Split* pSplit;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

    guid = gnc_sql_load_guid( be, row );
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
		GList* list = NULL;
		GncSqlRow* row;

		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
			Split* s;
            s = load_single_split( be, row );
			if( s != NULL ) {
				list = g_list_append( list, s );
			}
			row = gnc_sql_result_get_next_row( result );
        }

		if( list != NULL ) {
			gnc_sql_slots_load_for_list( be, list );
		}

		gnc_sql_result_dispose( result );
    }
	(void)g_string_free( sql, FALSE );
}

static Transaction*
load_single_tx( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
    GUID tx_guid;
	Transaction* pTx;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

    guid = gnc_sql_load_guid( be, row );
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
	Account* acc;
	gnc_numeric start_bal;
	gnc_numeric end_bal;
	gnc_numeric start_cleared_bal;
	gnc_numeric end_cleared_bal;
	gnc_numeric start_reconciled_bal;
	gnc_numeric end_reconciled_bal;
} full_acct_balances_t;

/**
 * Save the start/end balances for an account.
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
}

/**
 * Executes a transaction query statement and loads the transactions and all
 * of the splits.
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
		}
		g_slist_free( bal_list );

		xaccAccountCommitEdit( root );
		qof_event_resume();
    }
}

/* ================================================================= */
static void
create_transaction_tables( GncSqlBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_sql_get_table_version( be, TRANSACTION_TABLE );
    if( version == 0 ) {
        gnc_sql_create_table( be, TRANSACTION_TABLE, TX_TABLE_VERSION, tx_col_table );
    } else if( version == 1 ) {
		/* Upgrade 64 bit int handling */
		gnc_sql_upgrade_table( be, TRANSACTION_TABLE, tx_col_table );
		gnc_sql_set_table_version( be, TRANSACTION_TABLE, TX_TABLE_VERSION );
    }

	version = gnc_sql_get_table_version( be, SPLIT_TABLE );
    if( version == 0 ) {
        gnc_sql_create_table( be, SPLIT_TABLE, SPLIT_TABLE_VERSION, split_col_table );
    } else if( version == 1 ) {
		/* Upgrade 64 bit int handling */
		gnc_sql_upgrade_table( be, SPLIT_TABLE, split_col_table );
		gnc_sql_set_table_version( be, SPLIT_TABLE, SPLIT_TABLE_VERSION );
    }
}
/* ================================================================= */
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
static const GUID*
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
	gchar* subquery_sql;
	gchar* query_sql;
    GncSqlStatement* stmt;

	g_return_if_fail( be != NULL );
	g_return_if_fail( account != NULL );

	guid = qof_instance_get_guid( QOF_INSTANCE(account) );
    (void)guid_to_string_buff( guid, guid_buf );
	subquery_sql = g_strdup_printf( "SELECT DISTINCT tx_guid FROM %s WHERE account_guid='%s'", SPLIT_TABLE, guid_buf );
	query_sql = g_strdup_printf( "SELECT * FROM %s WHERE guid IN (%s)", TRANSACTION_TABLE, subquery_sql );
	g_free( subquery_sql );
	stmt = gnc_sql_create_statement_from_sql( be, query_sql );
    query_transactions( be, stmt );
	gnc_sql_statement_dispose( stmt );
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
    query_transactions( be, stmt );
	gnc_sql_statement_dispose( stmt );
}

typedef struct {
    GncSqlStatement* stmt;
	Account* acct;
	gboolean has_been_run;
} split_query_info_t;

static gpointer
compile_split_query( GncSqlBackend* be, QofQuery* pQuery )
{
    const GUID* acct_guid;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
	split_query_info_t* query_info;
	gchar* subquery_sql;
	gchar* query_sql;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( pQuery != NULL, NULL );

    acct_guid = get_guid_from_query( pQuery );
    (void)guid_to_string_buff( acct_guid, guid_buf );
	subquery_sql = g_strdup_printf( "SELECT DISTINCT tx_guid FROM %s WHERE account_guid='%s'", SPLIT_TABLE, guid_buf );
	query_sql = g_strdup_printf( "SELECT * FROM %s WHERE guid IN (%s)", TRANSACTION_TABLE, subquery_sql );

	query_info = g_malloc( (gsize)sizeof(split_query_info_t) );
	query_info->stmt = gnc_sql_create_statement_from_sql( be, query_sql );
	query_info->has_been_run = FALSE;
	query_info->acct = xaccAccountLookup( acct_guid, be->primary_book );

	g_free( subquery_sql );

	return query_info;
}

static void
run_split_query( GncSqlBackend* be, gpointer pQuery )
{
	split_query_info_t* query_info = (split_query_info_t*)pQuery;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pQuery != NULL );

	// When the query to load all splits for the account has been run, set the
	// mark so that this account's query is not reexecuted.
	if( !query_info->has_been_run && xaccAccountGetMark( query_info->acct ) == 0 ) {
    	query_transactions( be, query_info->stmt );
		query_info->has_been_run = TRUE;
		gnc_sql_statement_dispose( query_info->stmt );
		query_info->stmt = NULL;
		xaccAccountSetMark( query_info->acct, 1 );
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
    const GncSqlBackend* be;
	Account* acct;
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
	/*# -fullinitblock */
    { "account_guid",    CT_GUID,    0, 0, NULL, NULL, NULL, set_acct_bal_account_from_guid },
    { "reconcile_state", CT_STRING,  1, 0, NULL, NULL, NULL, set_acct_bal_reconcile_state },
    { "quantity",        CT_NUMERIC, 0, 0, NULL, NULL, NULL, (QofSetterFunc)set_acct_bal_balance },
    { NULL }
	/*# +fullinitblock */
};

static single_acct_balance_t*
load_single_acct_balances( const GncSqlBackend* be, GncSqlRow* row )
{
	single_acct_balance_t* bal = NULL;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

	bal = g_malloc( (gsize)sizeof(single_acct_balance_t) );
	bal->be = be;
    gnc_sql_load_object( be, row, NULL, bal, acct_balances_col_table );

	return bal;
}

GSList*
gnc_sql_get_account_balances_slist( GncSqlBackend* be )
{
    GncSqlResult* result;
    GncSqlStatement* stmt;
	gchar* buf;
	GSList* bal_slist = NULL;

	g_return_val_if_fail( be != NULL, NULL );

	buf = g_strdup_printf( "SELECT account_guid, reconcile_state, sum(quantity_num) as quantity_num, quantity_denom FROM %s GROUP BY account_guid, reconcile_state, quantity_denom",
						SPLIT_TABLE );
	stmt = gnc_sql_create_statement_from_sql( be, buf );

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
            QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    GUID guid;
    const GUID* pGuid;
	Transaction* tx = NULL;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if( val == NULL ) {
        pGuid = NULL;
    } else {
        string_to_guid( g_value_get_string( val ), &guid );
        pGuid = &guid;
    }
	if( pGuid != NULL ) {
		tx = xaccTransLookup( pGuid, be->primary_book );
	}
    if( table_row->gobj_param_name != NULL ) {
		g_object_set( pObject, table_row->gobj_param_name, tx, NULL );
    } else {
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

    qof_object_register_backend( GNC_ID_TRANS, GNC_SQL_BACKEND, &be_data_tx );
    qof_object_register_backend( GNC_ID_SPLIT, GNC_SQL_BACKEND, &be_data_split );

	gnc_sql_register_col_type_handler( CT_TXREF, &tx_guid_handler );
}

/* ========================== END OF FILE ===================== */
