/********************************************************************
 * gnc-transaction-gda.c: load and save data to SQL via libgda      *
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
/** @file gnc-transaction-gda.c
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
#include "qofquery-p.h"
#include "qofquerycore-p.h"

#include "Account.h"
#include "Transaction.h"
#include "engine-helpers.h"

#include "gnc-backend-util-gda.h"
#include "gnc-transaction-gda.h"
#include "gnc-commodity.h"
#include "gnc-commodity-gda.h"
#include "gnc-slots-gda.h"

#include "gnc-engine.h"

static QofLogModule log_module = G_LOG_DOMAIN;

#define TRANSACTION_TABLE "transactions"
#define SPLIT_TABLE "splits"

typedef struct {
    GncGdaBackend* be;
    const GUID* guid;
} split_info_t;

#define TX_MAX_NUM_LEN 50
#define TX_MAX_DESCRIPTION_LEN 500

static col_cvt_t tx_col_table[] =
{
    { "guid",          CT_GUID,           0,                      COL_NNUL, "guid" },
    { "currency_guid", CT_COMMODITYREF,   0,                      COL_NNUL, NULL, NULL,
			(QofAccessFunc)xaccTransGetCurrency, (QofSetterFunc)xaccTransSetCurrency },
    { "num",           CT_STRING,         TX_MAX_NUM_LEN,         COL_NNUL, NULL, TRANS_NUM },
    { "post_date",     CT_TIMESPEC,       0,                      COL_NNUL, NULL, NULL,
			(QofAccessFunc)xaccTransRetDatePostedTS, (QofSetterFunc)gnc_transaction_set_date_posted },
    { "enter_date",    CT_TIMESPEC,       0,                      COL_NNUL, NULL, NULL,
			(QofAccessFunc)xaccTransRetDateEnteredTS, (QofSetterFunc)gnc_transaction_set_date_entered },
    { "description",   CT_STRING,         TX_MAX_DESCRIPTION_LEN, 0,        NULL, NULL,
            (QofAccessFunc)xaccTransGetDescription, (QofSetterFunc)xaccTransSetDescription },
    { NULL }
};

static gpointer get_split_reconcile_state( gpointer pObject, const QofParam* param );
static void set_split_reconcile_state( gpointer pObject, gpointer pValue );
static void set_split_reconcile_date( gpointer pObject, Timespec ts );

#define SPLIT_MAX_MEMO_LEN 50
#define SPLIT_MAX_ACTION_LEN 50

static col_cvt_t split_col_table[] =
{
    { "guid",            CT_GUID,         0,                    COL_NNUL, "guid" },
    { "tx_guid",         CT_TXREF,        0,                    COL_NNUL, NULL, SPLIT_TRANS },
    { "account_guid",    CT_ACCOUNTREF,   0,                    COL_NNUL, NULL, SPLIT_ACCOUNT },
    { "memo",            CT_STRING,       SPLIT_MAX_MEMO_LEN,   COL_NNUL, NULL, SPLIT_MEMO },
    { "action",          CT_STRING,       SPLIT_MAX_ACTION_LEN, COL_NNUL, NULL, SPLIT_ACTION },
    { "reconcile_state", CT_STRING,       1,                    COL_NNUL, NULL, NULL,    get_split_reconcile_state, set_split_reconcile_state },
    { "reconcile_date",  CT_TIMESPEC,     0,                    COL_NNUL, NULL, NULL,
			(QofAccessFunc)xaccSplitRetDateReconciledTS, (QofSetterFunc)set_split_reconcile_date },
    { "value",           CT_NUMERIC,      0,                    COL_NNUL, NULL, SPLIT_VALUE },
    { "quantity",        CT_NUMERIC,      0,                    COL_NNUL, NULL, SPLIT_AMOUNT },
    { NULL }
};

static col_cvt_t guid_col_table[] =
{
    { "tx_guid", CT_GUID, 0, 0, "guid" },
    { NULL }
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


// Table to retrieve just the quantity
static col_cvt_t quantity_table[] =
{
    { "quantity", CT_NUMERIC, 0, COL_NNUL, NULL, NULL, NULL, (QofSetterFunc)retrieve_numeric_value },
    { NULL }
};

static gnc_numeric
get_gnc_numeric_from_row( GncGdaBackend* be, GdaDataModel* model, int row )
{
	gnc_numeric val = gnc_numeric_zero();

	g_return_val_if_fail( be != NULL, val );
	g_return_val_if_fail( model != NULL, val );
	g_return_val_if_fail( row >= 0, val );

    gnc_gda_load_object( be, model, row, NULL, &val, quantity_table );

    return val;
}

/*
 * get_account_balance_from_query
 *
 * Given a GDA query which should return a number of rows of gnc_numeric num/denom pairs,
 * return the sum.
 */
static gnc_numeric
get_account_balance_from_query( GncGdaBackend* be, GdaQuery* query )
{
	gnc_numeric bal = gnc_numeric_zero();
    GdaObject* ret;

	g_return_val_if_fail( be != NULL, bal );
	g_return_val_if_fail( query != NULL, bal );

	/* Execute the query */
    ret = gnc_gda_execute_query( be, query );

	/* Loop for all rows, convert each to a gnc_numeric and sum them */
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;

        for( r = 0; r < numRows; r++ ) {
		    gnc_numeric val = get_gnc_numeric_from_row( be, pModel, r );
			bal = gnc_numeric_add( bal, val, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD );
		}
	}
	g_object_unref( G_OBJECT(ret) );

    return bal;
}

void
gnc_gda_get_account_balances( GncGdaBackend* be, Account* pAccount, 
								    gnc_numeric* start_balance,
								    gnc_numeric* cleared_balance,
									gnc_numeric* reconciled_balance )
{
	GdaQuery* query;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
	gchar* sql;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pAccount != NULL );
	g_return_if_fail( start_balance != NULL );
	g_return_if_fail( cleared_balance != NULL );
	g_return_if_fail( reconciled_balance != NULL );

    guid_to_string_buff( qof_instance_get_guid( pAccount ), guid_buf );

	/*
	 * For start balance,
	 *    SELECT SUM(QUANTITY_NUM),QUANTITY_DENOM FROM SPLITS
	 *        WHERE ACCOUNT_GUID=<guid> GROUP BY QUANTITY_DENOM
	 *
	 * This will return one entry per denom.  These can then be made into
	 * gnc_numerics and then added.  With luck, there will only be one entry.
	 */

	//sql = g_strdup_printf( "SELECT SUM(QUANTITY_NUM),QUANTITY_DENOM FROM %s WHERE ACCOUNT_GUID='%s' GROUP BY QUANTITY_DENOM", SPLIT_TABLE, guid_buf );
	sql = g_strdup_printf( "SELECT QUANTITY_NUM,QUANTITY_DENOM FROM %s WHERE ACCOUNT_GUID='%s' GROUP BY QUANTITY_DENOM", SPLIT_TABLE, guid_buf );

	/* Create the query */
	query = gnc_gda_create_query_from_sql( be, sql );
	*start_balance = get_account_balance_from_query( be, query );
	g_object_unref( G_OBJECT(query) );
	g_free( sql );

	/*
	 * For cleared balance,
	 *    SELECT SUM(QUANTITY_NUM),QUANTITY_DENOM FROM SPLITS
	 *        WHERE ACCOUNT_GUID=<guid> AND RECONCILE_STATE='c'
	 *        GROUP BY QUANTITY_DENOM
	 *
	 * This just requires a modification to the query
	 */

	//sql = g_strdup_printf( "SELECT SUM(QUANTITY_NUM),QUANTITY_DENOM FROM %s WHERE ACCOUNT_GUID='%s' AND RECONCILE_STATE='%c' GROUP BY QUANTITY_DENOM", SPLIT_TABLE, guid_buf, CREC );
	sql = g_strdup_printf( "SELECT QUANTITY_NUM,QUANTITY_DENOM FROM %s WHERE ACCOUNT_GUID='%s' AND RECONCILE_STATE='%c' GROUP BY QUANTITY_DENOM", SPLIT_TABLE, guid_buf, CREC );

	query = gnc_gda_create_query_from_sql( be, sql );
    *cleared_balance = get_account_balance_from_query( be, query );
	g_object_unref( G_OBJECT(query) );

	g_free( sql );

	/*
	 * For reconciled balance,
	 *    SELECT SUM(QUANTITY_NUM),QUANTITY_DENOM FROM SPLITS
	 *        WHERE ACCOUNT_GUID=<guid> AND RECONCILE_STATE='c'
	 *        GROUP BY QUANTITY_DENOM
	 *
	 * This just requires a small modification to the cleared balance query
	 */

	//sql = g_strdup_printf( "SELECT SUM(QUANTITY_NUM),QUANTITY_DENOM FROM %s WHERE ACCOUNT_GUID='%s' AND RECONCILE_STATE='%c' GROUP BY QUANTITY_DENOM", SPLIT_TABLE, guid_buf, YREC );
	sql = g_strdup_printf( "SELECT QUANTITY_NUM,QUANTITY_DENOM FROM %s WHERE ACCOUNT_GUID='%s' AND RECONCILE_STATE='%c' GROUP BY QUANTITY_DENOM", SPLIT_TABLE, guid_buf, YREC );

	query = gnc_gda_create_query_from_sql( be, sql );
	*reconciled_balance = get_account_balance_from_query( be, query );
	g_object_unref( G_OBJECT(query) );

	g_free( sql );
}

static void
load_single_split( GncGdaBackend* be, GdaDataModel* pModel, int row, GList** pList )
{
    const GUID* guid;
    GUID split_guid;
	Split* pSplit;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );

    guid = gnc_gda_load_guid( be, pModel, row );
    split_guid = *guid;

    pSplit = xaccSplitLookup( &split_guid, be->primary_book );
    if( pSplit == NULL ) {
        pSplit = xaccMallocSplit( be->primary_book );
    }

    /* If the split is dirty, don't overwrite it */
    if( !qof_instance_is_dirty( QOF_INSTANCE(pSplit) ) ) {
    	gnc_gda_load_object( be, pModel, row, GNC_ID_SPLIT, pSplit, split_col_table );
		*pList = g_list_append( *pList, pSplit );
//    	gnc_gda_slots_load( be, QOF_INSTANCE(pSplit) );
	}

    g_assert( pSplit == xaccSplitLookup( &split_guid, be->primary_book ) );
}

static void
load_all_splits_for_tx( GncGdaBackend* be, const GUID* tx_guid )
{
    GdaObject* ret;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
    GdaQuery* query;
    GdaQueryCondition* cond;
    GValue value;

	g_return_if_fail( be != NULL );
	g_return_if_fail( tx_guid != NULL );

    guid_to_string_buff( tx_guid, guid_buf );
    memset( &value, 0, sizeof( GValue ) );
    g_value_init( &value, G_TYPE_STRING );
    g_value_set_string( &value, guid_buf );
    query = gnc_gda_create_select_query( be, SPLIT_TABLE );
    cond = gnc_gda_create_condition_from_field( query, "tx_guid", &value );
    gda_query_set_condition( query, cond );
    g_object_unref( G_OBJECT(cond) );

    ret = gnc_gda_execute_query( be, query );
    g_object_unref( G_OBJECT(query) );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;
		GList* list = NULL;

        for( r = 0; r < numRows; r++ ) {
            load_single_split( be, pModel, r, &list );
        }

		if( list != NULL ) {
			gnc_gda_slots_load_for_list( be, list );
		}
    }
}

static void
load_splits_for_tx_list( GncGdaBackend* be, GList* list )
{
	g_return_if_fail( be != NULL );

	if( list == NULL ) return;

	for( ; list != NULL; list = list->next ) {
		load_all_splits_for_tx( be, qof_instance_get_guid( QOF_INSTANCE(list->data) ) );
	}
}

static void
load_single_tx( GncGdaBackend* be, GdaDataModel* pModel, int row, GList** pList )
{
    const GUID* guid;
    GUID tx_guid;
	Transaction* pTx;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );

    guid = gnc_gda_load_guid( be, pModel, row );
    tx_guid = *guid;

    pTx = xaccTransLookup( &tx_guid, be->primary_book );
    if( pTx == NULL ) {
        pTx = xaccMallocTransaction( be->primary_book );
    }
    xaccTransBeginEdit( pTx );
    gnc_gda_load_object( be, pModel, row, GNC_ID_TRANS, pTx, tx_col_table );
    gnc_gda_slots_load( be, QOF_INSTANCE(pTx) );
	*pList = g_list_append( *pList, pTx );
//    load_all_splits( be, qof_instance_get_guid( QOF_INSTANCE(pTx) ) );

    qof_instance_mark_clean( QOF_INSTANCE(pTx) );
    xaccTransCommitEdit( pTx );

    g_assert( pTx == xaccTransLookup( &tx_guid, be->primary_book ) );
}

static void
query_transactions( GncGdaBackend* be, GdaQuery* query )
{
    GdaObject* ret;

	g_return_if_fail( be != NULL );
	g_return_if_fail( query != NULL );

    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;
		GList* tx_list = NULL;

        for( r = 0; r < numRows; r++ ) {
            load_single_tx( be, pModel, r, &tx_list );
        }

		if( tx_list != NULL ) {
			load_splits_for_tx_list( be, tx_list );
		}
    }
}

static void
load_tx_by_guid( GncGdaBackend* be, GUID* tx_guid )
{
    GdaQuery* query;
	gchar* sql;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];

	g_return_if_fail( be != NULL );
	g_return_if_fail( tx_guid != NULL );

    guid_to_string_buff( tx_guid, guid_buf );
	sql = g_strdup_printf( "SELECT * FROM %s WHERE guid = %s", TRANSACTION_TABLE, guid_buf );
	query = gnc_gda_create_query_from_sql( be, sql );
	query_transactions( be, query );
}

/* ================================================================= */
static void
create_transaction_tables( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    gnc_gda_create_table_if_needed( be, TRANSACTION_TABLE, tx_col_table );
    gnc_gda_create_table_if_needed( be, SPLIT_TABLE, split_col_table );
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

    gnc_gda_slots_delete( split_info->be,
                    qof_instance_get_guid( QOF_INSTANCE(pSplit) ) );
}

static void
delete_splits( GncGdaBackend* be, Transaction* pTx )
{
    split_info_t split_info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pTx != NULL );

    (void)gnc_gda_do_db_operation( be, OP_DB_DELETE, SPLIT_TABLE,
                                SPLIT_TABLE, pTx, guid_col_table );
    split_info.be = be;

    g_list_foreach( xaccTransGetSplitList( pTx ), delete_split_slots_cb, &split_info );
}

static void
commit_split( QofInstance* inst, GncGdaBackend* be )
{
	gint op;

	g_return_if_fail( inst != NULL );
	g_return_if_fail( be != NULL );

	if( qof_instance_get_destroying( inst ) ) {
		op = OP_DB_DELETE;
	} else if( be->is_pristine_db ) {
		op = OP_DB_ADD;
	} else {
		op = OP_DB_ADD_OR_UPDATE;
	}
    (void)gnc_gda_do_db_operation( be, op, SPLIT_TABLE, GNC_ID_SPLIT, inst, split_col_table );
    gnc_gda_slots_save( be,
                        qof_instance_get_guid( inst ),
                        qof_instance_get_slots( inst ) );
}

static void
save_split_cb( gpointer data, gpointer user_data )
{
    split_info_t* split_info = (split_info_t*)user_data;
    Split* pSplit = GNC_SPLIT(data);

	g_return_if_fail( data != NULL );
	g_return_if_fail( GNC_IS_SPLIT(data) );
	g_return_if_fail( user_data != NULL );

    commit_split( QOF_INSTANCE(pSplit), split_info->be );
}

static void
save_splits( GncGdaBackend* be, const GUID* tx_guid, SplitList* pSplitList )
{
    split_info_t split_info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( tx_guid != NULL );
	g_return_if_fail( pSplitList != NULL );

    split_info.be = be;
    split_info.guid = tx_guid;
    g_list_foreach( pSplitList, save_split_cb, &split_info );
}

void
gnc_gda_save_transaction( QofInstance* inst, GncGdaBackend* be )
{
    Transaction* pTx = GNC_TRANS(inst);
    const GUID* guid;
	gint op;

	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_TRANS(inst) );
	g_return_if_fail( be != NULL );

    // Ensure the commodity is in the db
    gnc_gda_save_commodity( be, xaccTransGetCurrency( pTx ) );

	if( qof_instance_get_destroying( inst ) ) {
		op = OP_DB_DELETE;
	} else if( be->is_pristine_db ) {
		op = OP_DB_ADD;
	} else {
		op = OP_DB_ADD_OR_UPDATE;
	}

    (void)gnc_gda_do_db_operation( be, op, TRANSACTION_TABLE, GNC_ID_TRANS, pTx, tx_col_table );

    guid = qof_instance_get_guid( inst );

    // Delete any old slots and splits for this transaction
	if( !be->is_pristine_db ) {
    	delete_splits( be, pTx );
	}

    if( !qof_instance_get_destroying(inst) ) {
        SplitList* splits;

        // Now, commit any slots and splits
        gnc_gda_slots_save( be, guid, qof_instance_get_slots( inst ) );
        splits = xaccTransGetSplitList( pTx );
        save_splits( be, guid, splits );

        /* Mark the splits as clean */
        splits = xaccTransGetSplitList( pTx );
        for( ; splits != NULL; splits = splits->next ) {
            QofInstance* inst = QOF_INSTANCE(splits->data);

            qof_instance_mark_clean(inst);
        }
    } else {
        gnc_gda_slots_delete( be, guid );
    }
}

void
gnc_gda_transaction_commit_splits( GncGdaBackend* be, Transaction* pTx )
{
    SplitList* splits;
    Split* s;
    QofBackend* qbe = (QofBackend*)be;
    
	g_return_if_fail( be != NULL );
	g_return_if_fail( pTx != NULL );

    splits = xaccTransGetSplitList( pTx );
    for( ; splits != NULL; splits = splits->next ) {
        s = GNC_SPLIT(splits->data);

        qbe->commit( qbe, QOF_INSTANCE(s) );
    }
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

static gpointer
compile_split_query( GncGdaBackend* be, QofQuery* pQuery )
{
	GString* sql;
    const GUID* acct_guid;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
	GdaQuery* query;
	GdaObject* results;
	gchar* buf;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( pQuery != NULL, NULL );

#if 1
    acct_guid = get_guid_from_query( pQuery );
    guid_to_string_buff( acct_guid, guid_buf );
	sql = g_string_new( "" );
	g_string_printf( sql, "SELECT DISTINCT tx_guid FROM %s WHERE account_guid='%s'", SPLIT_TABLE, guid_buf );
	results = gnc_gda_execute_sql( be, sql->str );
    if( GDA_IS_DATA_MODEL( results ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(results);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;

		sql = g_string_sized_new( 40+(GUID_ENCODING_LENGTH+3)*numRows );

		if( numRows != 1 ) {
			g_string_printf( sql, "SELECT * FROM %s WHERE guid IN (", TRANSACTION_TABLE );
		} else {
			g_string_printf( sql, "SELECT * FROM %s WHERE guid =", TRANSACTION_TABLE );
		}

        for( r = 0; r < numRows; r++ ) {
			const GUID* guid;

			guid = gnc_gda_load_tx_guid( be, pModel, r );
    		guid_to_string_buff( guid, guid_buf );
			if( r != 0 ) {
				g_string_append( sql, "," );
			}
			g_string_append( sql, "'" );
			g_string_append( sql, guid_buf );
			g_string_append( sql, "'" );
        }

		if( numRows != 1 ) {
			g_string_append( sql, ")" );
		}
    }

	buf = sql->str;
	g_string_free( sql, FALSE );
	return buf;
#else
#if 1
    acct_guid = get_guid_from_query( pQuery );
    guid_to_string_buff( acct_guid, guid_buf );
    buf = g_strdup_printf( "SELECT * FROM %s WHERE guid IN (SELECT DISTINCT tx_guid FROM %s WHERE account_guid = '%s')",
                            TRANSACTION_TABLE, SPLIT_TABLE, guid_buf );
    return buf;
#else
    GdaQuery* query;
    GdaQuery* subQuery;
    GdaQueryTarget* target;
    GdaQueryField* allFields;
    GdaQueryField* field;
    GValue value;
    GdaQueryCondition* cond;
    GdaQueryField* key;
    GdaQueryField* key_value;

    acct_guid = get_guid_from_query( pQuery );
    guid_to_string_buff( acct_guid, guid_buf );

    /* Subquery */

    /* SELECT */
    subQuery = gda_query_new( be->pDict );
    gda_query_set_query_type( subQuery, GDA_QUERY_TYPE_SELECT );

    /* FROM splits */
    target = gda_query_target_new( subQuery, SPLIT_TABLE );
    gda_query_add_target( subQuery, target, NULL );
    g_object_unref( G_OBJECT(target) );

    /* tx_guid */
    field = gda_query_field_field_new( subQuery, "tx_guid" );
    gda_query_field_set_visible( field, TRUE );
    gda_entity_add_field( GDA_ENTITY(subQuery), GDA_ENTITY_FIELD(field) );
    g_object_unref( G_OBJECT(field) );

    /* WHERE */
    memset( &value, 0, sizeof( GValue ) );
    g_value_init( &value, G_TYPE_STRING );
    g_value_set_string( &value, guid_buf );
    cond = gnc_gda_create_condition_from_field( subQuery, "account_guid", &value );
    gda_query_set_condition( subQuery, cond );
    g_object_unref( G_OBJECT(cond) );

    /* Main query */

    /* SELECT * FROM transactions */
    query = gnc_gda_create_select_query( be, TRANSACTION_TABLE );
    gda_query_add_sub_query( query, subQuery );
    g_object_unref( G_OBJECT(subQuery) );

    /* WHERE */
    cond = gda_query_condition_new( query, GDA_QUERY_CONDITION_LEAF_IN );
    gda_query_set_condition( query, cond );
    g_object_unref( G_OBJECT(cond) );

    key = gda_query_field_field_new( query, "account_guid" );
    gda_query_field_set_visible( key, TRUE );
    gda_query_condition_leaf_set_operator( cond,
                                            GDA_QUERY_CONDITION_OP_LEFT,
                                            GDA_QUERY_FIELD(key) );
    g_object_unref( G_OBJECT(key) );

    key_value = gda_query_field_value_new( query, G_TYPE_STRING );
    gda_query_field_set_visible( key_value, TRUE );
    gda_query_field_value_set_is_parameter( GDA_QUERY_FIELD_VALUE(key_value), TRUE );

    g_object_set( key_value, "value-provider", subQuery, NULL );
    gda_query_condition_leaf_set_operator( cond, GDA_QUERY_CONDITION_OP_RIGHT,
                                                GDA_QUERY_FIELD(key_value) );

    return query;
#endif
#endif
}

static void
run_split_query( GncGdaBackend* be, gpointer pQuery )
{
    GdaQuery* query;
    const gchar* sql;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pQuery != NULL );

#if 1
    sql = (const gchar*)pQuery;

	query = gnc_gda_create_query_from_sql( be, sql );
#else
    query = GDA_QUERY(pQuery);
#endif
    query_transactions( be, query );
}

static void
free_split_query( GncGdaBackend* be, gpointer pQuery )
{
	g_return_if_fail( be != NULL );
	g_return_if_fail( pQuery != NULL );

#if 1
    g_free( pQuery );
#else
    g_object_unref( G_OBJECT(pQuery) );
#endif
}

/* ----------------------------------------------------------------- */
static void
load_tx_guid( const GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table_row )
{
    const GValue* val;
    GUID guid;
    const GUID* pGuid;
	Transaction* tx = NULL;

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
		tx = xaccTransLookup( pGuid, be->primary_book );
	}
    if( table_row->gobj_param_name != NULL ) {
		g_object_set( pObject, table_row->gobj_param_name, tx, NULL );
    } else {
		(*setter)( pObject, (const gpointer)tx );
    }
}

static col_type_handler_t tx_guid_handler =
        { load_tx_guid, gnc_gda_create_objectref_guid_col,
            gnc_gda_get_gvalue_objectref_guid_for_query, gnc_gda_get_gvalue_objectref_guid_cond };
/* ================================================================= */
void
gnc_gda_init_transaction_handler( void )
{
    static GncGdaDataType_t be_data_tx =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_TRANS,
        gnc_gda_save_transaction,            /* commit */
        NULL,                        /* initial_load */
        create_transaction_tables    /* create tables */
    };
    static GncGdaDataType_t be_data_split =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_SPLIT,
        commit_split,                /* commit */
        NULL,                        /* initial_load */
        NULL,                        /* create tables */
        compile_split_query,
        run_split_query,
        free_split_query
    };

    qof_object_register_backend( GNC_ID_TRANS, GNC_GDA_BACKEND, &be_data_tx );
    qof_object_register_backend( GNC_ID_SPLIT, GNC_GDA_BACKEND, &be_data_split );

	gnc_gda_register_col_type_handler( CT_TXREF, &tx_guid_handler );
}

/* ========================== END OF FILE ===================== */
