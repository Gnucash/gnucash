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
#include "engine-helpers.h"

#include "gnc-backend-util-sql.h"
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
} split_info_t;

#define TX_MAX_NUM_LEN 2048
#define TX_MAX_DESCRIPTION_LEN 2048

static const col_cvt_t tx_col_table[] =
{
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
};

static gpointer get_split_reconcile_state( gpointer pObject, const QofParam* param );
static void set_split_reconcile_state( gpointer pObject, gpointer pValue );
static void set_split_reconcile_date( gpointer pObject, Timespec ts );

#define SPLIT_MAX_MEMO_LEN 2048
#define SPLIT_MAX_ACTION_LEN 2048

static const col_cvt_t split_col_table[] =
{
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
    { NULL }
};

static const col_cvt_t guid_col_table[] =
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
get_gnc_numeric_from_row( GncSqlBackend* be, GncSqlRow* row )
{
	gnc_numeric val = gnc_numeric_zero();

	g_return_val_if_fail( be != NULL, val );
	g_return_val_if_fail( row != NULL, val );

    gnc_sql_load_object( be, row, NULL, &val, quantity_table );

    return val;
}

static void
load_single_split( GncSqlBackend* be, GncSqlRow* row, GList** pList )
{
    const GUID* guid;
    GUID split_guid;
	Split* pSplit;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );

    guid = gnc_sql_load_guid( be, row );
    split_guid = *guid;

    pSplit = xaccSplitLookup( &split_guid, be->primary_book );
    if( pSplit == NULL ) {
        pSplit = xaccMallocSplit( be->primary_book );
    }

    /* If the split is dirty, don't overwrite it */
    if( !qof_instance_is_dirty( QOF_INSTANCE(pSplit) ) ) {
    	gnc_sql_load_object( be, row, GNC_ID_SPLIT, pSplit, split_col_table );
		*pList = g_list_append( *pList, pSplit );
	}

    g_assert( pSplit == xaccSplitLookup( &split_guid, be->primary_book ) );
}

static void
load_all_splits_for_tx( GncSqlBackend* be, const GUID* tx_guid )
{
    GncSqlResult* result;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
    GncSqlStatement* stmt;
    GValue value;
	gchar* buf;
	GError* error = NULL;

	g_return_if_fail( be != NULL );
	g_return_if_fail( tx_guid != NULL );

    guid_to_string_buff( tx_guid, guid_buf );
    memset( &value, 0, sizeof( GValue ) );
    g_value_init( &value, G_TYPE_STRING );
    g_value_set_string( &value, guid_buf );

	buf = g_strdup_printf( "SELECT * FROM %s WHERE tx_guid='%s'", SPLIT_TABLE, guid_buf );
	stmt = gnc_sql_create_statement_from_sql( be, buf );
	g_free( buf );

    result = gnc_sql_execute_select_statement( be, stmt );
	gnc_sql_statement_dispose( stmt );
    if( result != NULL ) {
        int r;
		GList* list = NULL;
		GncSqlRow* row;

		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
            load_single_split( be, row, &list );
			row = gnc_sql_result_get_next_row( result );
        }
		gnc_sql_result_dispose( result );

		if( list != NULL ) {
			gnc_sql_slots_load_for_list( be, list );
		}
    }
}

static void
load_splits_for_tx_list( GncSqlBackend* be, GList* list )
{
	GString* sql;
	QofCollection* col;
	GncSqlResult* result;
	gboolean first_guid = TRUE;

	g_return_if_fail( be != NULL );

	if( list == NULL ) return;

	sql = g_string_sized_new( 40+(GUID_ENCODING_LENGTH+3)*g_list_length( list ) );
	g_string_append_printf( sql, "SELECT * FROM %s WHERE %s IN (", SPLIT_TABLE, guid_col_table[0].col_name );
	(void)gnc_sql_append_guid_list_to_sql( sql, list, G_MAXUINT );
	g_string_append( sql, ")" );

	// Execute the query and load the splits
	result = gnc_sql_execute_select_sql( be, sql->str );
    if( result != NULL ) {
		GList* list = NULL;
		GncSqlRow* row;

		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
            load_single_split( be, row, &list );
			row = gnc_sql_result_get_next_row( result );
        }

		if( list != NULL ) {
			gnc_sql_slots_load_for_list( be, list );
		}

		gnc_sql_result_dispose( result );
    }
	g_string_free( sql, FALSE );
}

static void
load_single_tx( GncSqlBackend* be, GncSqlRow* row, GList** pList )
{
    const GUID* guid;
    GUID tx_guid;
	Transaction* pTx;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );

    guid = gnc_sql_load_guid( be, row );
    tx_guid = *guid;

    pTx = xaccTransLookup( &tx_guid, be->primary_book );
    if( pTx == NULL ) {
        pTx = xaccMallocTransaction( be->primary_book );
    }
    xaccTransBeginEdit( pTx );
    gnc_sql_load_object( be, row, GNC_ID_TRANS, pTx, tx_col_table );
	*pList = g_list_append( *pList, pTx );

    g_assert( pTx == xaccTransLookup( &tx_guid, be->primary_book ) );
}

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

		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
            load_single_tx( be, row, &tx_list );
			row = gnc_sql_result_get_next_row( result );
        }
		gnc_sql_result_dispose( result );

		if( tx_list != NULL ) {
			gnc_sql_slots_load_for_list( be, tx_list );
			load_splits_for_tx_list( be, tx_list );
		}

		// Commit all of the transactions
		for( node = tx_list; node != NULL; node = node->next ) {
			Transaction* pTx = GNC_TRANSACTION(node->data);
    		qof_instance_mark_clean( QOF_INSTANCE(pTx) );
    		xaccTransCommitEdit( pTx );
		}
    }
}

static void
load_tx_by_guid( GncSqlBackend* be, GUID* tx_guid )
{
    GncSqlStatement* stmt;
	gchar* sql;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];

	g_return_if_fail( be != NULL );
	g_return_if_fail( tx_guid != NULL );

    guid_to_string_buff( tx_guid, guid_buf );
	sql = g_strdup_printf( "SELECT * FROM %s WHERE guid = %s", TRANSACTION_TABLE, guid_buf );
	stmt = gnc_sql_create_statement_from_sql( be, sql );
	query_transactions( be, stmt );
	gnc_sql_statement_dispose( stmt );
}

/* ================================================================= */
static void
load_all_tx( GncSqlBackend* be )
{
	gchar* sql;
	GncSqlStatement* stmt;

	g_return_if_fail( be != NULL );

	sql = g_strdup_printf( "SELECT * FROM %s", TRANSACTION_TABLE );
	stmt = gnc_sql_create_statement_from_sql( be, sql );
	query_transactions( be, stmt );
	gnc_sql_statement_dispose( stmt );
}

/* ================================================================= */
static void
create_transaction_tables( GncSqlBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_sql_get_table_version( be, TRANSACTION_TABLE );
    if( version == 0 ) {
    	GError* error = NULL;

        gnc_sql_create_table( be, TRANSACTION_TABLE, TX_TABLE_VERSION, tx_col_table, &error );
        if( error != NULL ) {
            PERR( "Error creating table: %s\n", error->message );
        }
    }

	version = gnc_sql_get_table_version( be, SPLIT_TABLE );
    if( version == 0 ) {
    	GError* error = NULL;

        gnc_sql_create_table( be, SPLIT_TABLE, SPLIT_TABLE_VERSION, split_col_table, &error );
        if( error != NULL ) {
            PERR( "Error creating table: %s\n", error->message );
        }
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

    gnc_sql_slots_delete( split_info->be,
                    qof_instance_get_guid( QOF_INSTANCE(pSplit) ) );
}

static void
delete_splits( GncSqlBackend* be, Transaction* pTx )
{
    split_info_t split_info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pTx != NULL );

    (void)gnc_sql_do_db_operation( be, OP_DB_DELETE, SPLIT_TABLE,
                                SPLIT_TABLE, pTx, guid_col_table );
    split_info.be = be;

    g_list_foreach( xaccTransGetSplitList( pTx ), delete_split_slots_cb, &split_info );
}

static void
commit_split( QofInstance* inst, GncSqlBackend* be )
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
    (void)gnc_sql_do_db_operation( be, op, SPLIT_TABLE, GNC_ID_SPLIT, inst, split_col_table );
    gnc_sql_slots_save( be,
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
save_splits( GncSqlBackend* be, const GUID* tx_guid, SplitList* pSplitList )
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
gnc_sql_save_transaction( QofInstance* inst, GncSqlBackend* be )
{
    Transaction* pTx = GNC_TRANS(inst);
    const GUID* guid;
	gint op;

	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_TRANS(inst) );
	g_return_if_fail( be != NULL );

    // Ensure the commodity is in the db
    gnc_sql_save_commodity( be, xaccTransGetCurrency( pTx ) );

	if( qof_instance_get_destroying( inst ) ) {
		op = OP_DB_DELETE;
	} else if( be->is_pristine_db ) {
		op = OP_DB_ADD;
	} else {
		op = OP_DB_ADD_OR_UPDATE;
	}

    (void)gnc_sql_do_db_operation( be, op, TRANSACTION_TABLE, GNC_ID_TRANS, pTx, tx_col_table );

    guid = qof_instance_get_guid( inst );

    // Delete any old slots and splits for this transaction
	if( !be->is_pristine_db ) {
    	delete_splits( be, pTx );
	}

    if( !qof_instance_get_destroying(inst) ) {
        SplitList* splits;

        // Now, commit any slots and splits
        gnc_sql_slots_save( be, guid, qof_instance_get_slots( inst ) );
        splits = xaccTransGetSplitList( pTx );
        save_splits( be, guid, splits );

        /* Mark the splits as clean */
        splits = xaccTransGetSplitList( pTx );
        for( ; splits != NULL; splits = splits->next ) {
            QofInstance* inst = QOF_INSTANCE(splits->data);

            qof_instance_mark_clean(inst);
        }
    } else {
        gnc_sql_slots_delete( be, guid );
    }
}

void
gnc_sql_transaction_commit_splits( GncSqlBackend* be, Transaction* pTx )
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
compile_split_query( GncSqlBackend* be, QofQuery* pQuery )
{
	GString* sql;
    const GUID* acct_guid;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
	GncSqlResult* result;
	gchar* buf;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( pQuery != NULL, NULL );

    acct_guid = get_guid_from_query( pQuery );
    guid_to_string_buff( acct_guid, guid_buf );
	sql = g_string_new( "" );
	g_string_printf( sql, "SELECT DISTINCT tx_guid FROM %s WHERE account_guid='%s'", SPLIT_TABLE, guid_buf );
	result = gnc_sql_execute_select_sql( be, sql->str );
    if( result != NULL ) {
        int numRows;
        int r;
		GncSqlRow* row;

		numRows = gnc_sql_result_get_num_rows( result );
		sql = g_string_sized_new( 40+(GUID_ENCODING_LENGTH+3)*numRows );

		if( numRows != 1 ) {
			g_string_printf( sql, "SELECT * FROM %s WHERE guid IN (", TRANSACTION_TABLE );
		} else {
			g_string_printf( sql, "SELECT * FROM %s WHERE guid =", TRANSACTION_TABLE );
		}

		row = gnc_sql_result_get_first_row( result );
        for( r = 0; row != NULL; r++ ) {
			const GUID* guid;

			guid = gnc_sql_load_tx_guid( be, row );
    		guid_to_string_buff( guid, guid_buf );
			if( r != 0 ) {
				g_string_append( sql, "," );
			}
			g_string_append( sql, "'" );
			g_string_append( sql, guid_buf );
			g_string_append( sql, "'" );
			row = gnc_sql_result_get_next_row( result );
        }
		gnc_sql_result_dispose( result );

		if( numRows != 1 ) {
			g_string_append( sql, ")" );
		}
    }

	buf = sql->str;
	g_string_free( sql, FALSE );
	return buf;
}

static void
run_split_query( GncSqlBackend* be, gpointer pQuery )
{
    GncSqlStatement* stmt;
    gchar* sql;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pQuery != NULL );

    sql = (gchar*)pQuery;

	stmt = gnc_sql_create_statement_from_sql( be, sql );
    query_transactions( be, stmt );
	gnc_sql_statement_dispose( stmt );
}

static void
free_split_query( GncSqlBackend* be, gpointer pQuery )
{
	g_return_if_fail( be != NULL );
	g_return_if_fail( pQuery != NULL );

    g_free( pQuery );
}

/* ----------------------------------------------------------------- */
static void
load_tx_guid( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table_row )
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

static col_type_handler_t tx_guid_handler
	= { load_tx_guid,
		gnc_sql_add_objectref_guid_col_info_to_list,
		gnc_sql_add_colname_to_list,
        gnc_sql_add_gvalue_objectref_guid_to_slist };
/* ================================================================= */
void
gnc_sql_init_transaction_handler( void )
{
    static GncSqlDataType_t be_data_tx =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_TRANS,
        gnc_sql_save_transaction,    /* commit */
        load_all_tx,                 /* initial_load */
        create_transaction_tables    /* create tables */
    };
    static GncSqlDataType_t be_data_split =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_SPLIT,
        commit_split,                /* commit */
        NULL,                        /* initial_load */
        NULL,                        /* create tables */
        compile_split_query,
        run_split_query,
        free_split_query
    };

    qof_object_register_backend( GNC_ID_TRANS, GNC_SQL_BACKEND, &be_data_tx );
    qof_object_register_backend( GNC_ID_SPLIT, GNC_SQL_BACKEND, &be_data_split );

	gnc_sql_register_col_type_handler( CT_TXREF, &tx_guid_handler );
}

/* ========================== END OF FILE ===================== */
