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

#include "gnc-backend-util-gda.h"
#include "gnc-transaction-gda.h"
#include "gnc-commodity.h"
#include "gnc-commodity-gda.h"
#include "gnc-slots-gda.h"

#include "gnc-engine.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

#define TRANSACTION_TABLE "transactions"
#define SPLIT_TABLE "splits"

typedef struct {
    GncGdaBackend* be;
    const GUID* guid;
} split_info_t;

static gpointer get_guid( gpointer pObject, const QofParam* param );
static void set_guid( gpointer pObject, gpointer pValue );
static gpointer get_tx_currency_guid( gpointer pObject, const QofParam* param );
static void set_tx_currency_guid( gpointer pObject, gpointer pValue );
static gpointer get_tx_num( gpointer pObject, const QofParam* param );
static void set_tx_num( gpointer pObject, gpointer pValue );
static gpointer get_tx_post_date( gpointer pObject, const QofParam* param );
static void set_tx_post_date( gpointer pObject, gpointer pValue );
static gpointer get_tx_enter_date( gpointer pObject, const QofParam* param );
static void set_tx_enter_date( gpointer pObject, gpointer pValue );

#define TX_MAX_NUM_LEN 50
#define TX_MAX_DESCRIPTION_LEN 500

static col_cvt_t tx_col_table[] =
{
    { "guid",            CT_GUID,    0, COL_NNUL|COL_PKEY, NULL,
            get_guid, set_guid },
    { "currency_guid",    CT_GUID,    0, COL_NNUL,    NULL,
            get_tx_currency_guid, set_tx_currency_guid },
    { "num",            CT_STRING,    TX_MAX_NUM_LEN, COL_NNUL, NULL,
            get_tx_num, set_tx_num },
    { "post_date",        CT_TIMESPEC, 0, COL_NNUL, NULL,
            get_tx_post_date, set_tx_post_date },
    { "enter_date",        CT_TIMESPEC, 0, COL_NNUL, NULL,
            get_tx_enter_date, set_tx_enter_date },
    { "description",    CT_STRING,    TX_MAX_DESCRIPTION_LEN, 0,    NULL,
            (QofAccessFunc)xaccTransGetDescription,
            (QofSetterFunc)xaccTransSetDescription },
    { NULL }
};

static gpointer get_split_tx_guid( gpointer pObject, const QofParam* param );
static void set_split_tx_guid( gpointer pObject, gpointer pValue );
static gpointer get_split_reconcile_state( gpointer pObject, const QofParam* param );
static void set_split_reconcile_state( gpointer pObject, gpointer pValue );
static gpointer get_split_reconcile_date( gpointer pObject, const QofParam* param );
static void set_split_reconcile_date( gpointer pObject, gpointer pValue );
static gpointer get_split_value( gpointer pObject, const QofParam* param );
static void set_split_value( gpointer pObject, gpointer pValue );
static gpointer get_split_quantity( gpointer pObject, const QofParam* param );
static void set_split_quantity( gpointer pObject, gpointer pValue );
static gpointer get_split_account_guid( gpointer pObject, const QofParam* param );
static void set_split_account_guid( gpointer pObject, gpointer pValue );

#define SPLIT_MAX_MEMO_LEN 50
#define SPLIT_MAX_ACTION_LEN 50

static col_cvt_t split_col_table[] =
{
    { "guid",            CT_GUID,     0, COL_NNUL|COL_PKEY,    NULL,
            get_guid, set_guid },
    { "tx_guid",        CT_GUID,     0, COL_NNUL,    NULL,
            get_split_tx_guid, set_split_tx_guid },
    { "memo",            CT_STRING,     SPLIT_MAX_MEMO_LEN, COL_NNUL,    SPLIT_MEMO },
    { "action",            CT_STRING,     SPLIT_MAX_ACTION_LEN, COL_NNUL,    SPLIT_ACTION },
    { "reconcile_state", CT_STRING,     1, COL_NNUL,    NULL,
            get_split_reconcile_state, set_split_reconcile_state },
    { "reconcile_date",    CT_TIMESPEC, 0, COL_NNUL,    NULL,
            get_split_reconcile_date, set_split_reconcile_date },
    { "value",            CT_NUMERIC,     0, COL_NNUL,    NULL,
            get_split_value, set_split_value },
    { "quantity",        CT_NUMERIC,     0, COL_NNUL,    NULL,
            get_split_quantity, set_split_quantity },
    { "account_guid",    CT_GUID,     0, COL_NNUL,    NULL,
            get_split_account_guid, set_split_account_guid },
    { NULL }
};

static col_cvt_t guid_col_table[] =
{
    { "tx_guid", CT_GUID, 0, 0, NULL, get_guid, set_guid },
    { NULL }
};

/* ================================================================= */
static gpointer
get_guid( gpointer pObject, const QofParam* param )
{
    return (gpointer)qof_instance_get_guid( QOF_INSTANCE(pObject) );
}

static void 
set_guid( gpointer pObject, gpointer pValue )
{
    QofInstance* pInstance = QOF_INSTANCE(pObject);
    GUID* guid = (GUID*)pValue;

    qof_instance_set_guid( pInstance, guid );
}

static gpointer
get_tx_currency_guid( gpointer pObject, const QofParam* param )
{
    const Transaction* pTx = GNC_TRANS(pObject);

    return (gpointer)qof_instance_get_guid(
                        QOF_INSTANCE(xaccTransGetCurrency( pTx )) );
}

static void 
set_tx_currency_guid( gpointer pObject, gpointer pValue )
{
    Transaction* pTx = GNC_TRANS(pObject);
    QofBook* pBook = qof_instance_get_book( QOF_INSTANCE(pTx) );
    gnc_commodity* pCurrency;
    GUID* guid = (GUID*)pValue;

    pCurrency = gnc_commodity_find_commodity_by_guid( guid, pBook );
    xaccTransSetCurrency( pTx, pCurrency );
}

static gpointer
get_tx_num( gpointer pObject, const QofParam* param )
{
    const Transaction* pTx = GNC_TRANS(pObject);
    const gchar* s;

    s = xaccTransGetNum( pTx );
    return (gpointer)s;
}

static void 
set_tx_num( gpointer pObject, gpointer pValue )
{
    Transaction* pTx = GNC_TRANS(pObject);
    const gchar* s = (const gchar*)pValue;

    xaccTransSetNum( pTx, s );
}

static gpointer
get_tx_post_date( gpointer pObject, const QofParam* param )
{
    const Transaction* pTx = GNC_TRANS(pObject);
    static Timespec ts;

    ts = xaccTransRetDatePostedTS( pTx );
    return (gpointer)&ts;
}

static void 
set_tx_post_date( gpointer pObject, gpointer pValue )
{
    Transaction* pTx = GNC_TRANS(pObject);
    Timespec* pTS = (Timespec*)pValue;

    xaccTransSetDatePostedTS( pTx, pTS );
}

static gpointer
get_tx_enter_date( gpointer pObject, const QofParam* param )
{
    const Transaction* pTx = GNC_TRANS(pObject);
    static Timespec ts;

    ts = xaccTransRetDateEnteredTS( pTx );
    return (gpointer)&ts;
}

static void 
set_tx_enter_date( gpointer pObject, gpointer pValue )
{
    Transaction* pTx = GNC_TRANS(pObject);
    Timespec* pTS = (Timespec*)pValue;

    xaccTransSetDateEnteredTS( pTx, pTS );
}

static gpointer
get_split_tx_guid( gpointer pObject, const QofParam* param )
{
    const Split* pSplit = GNC_SPLIT(pObject);
    Transaction* pTx = xaccSplitGetParent( pSplit );

    return (gpointer)qof_instance_get_guid( QOF_INSTANCE(pTx) );
}

static void 
set_split_tx_guid( gpointer pObject, gpointer pValue )
{
    Split* pSplit = GNC_SPLIT(pObject);
    QofBook* pBook = qof_instance_get_book( QOF_INSTANCE(pSplit) );
    GUID* guid = (GUID*)pValue;
    Transaction* pTx = xaccTransLookup( guid, pBook );

    xaccSplitSetParent( pSplit, pTx );
}

static gpointer
get_split_reconcile_state( gpointer pObject, const QofParam* param )
{
    const Split* pSplit = GNC_SPLIT(pObject);
    static gchar c[2];

    c[0] = xaccSplitGetReconcile( pSplit );
    c[1] = '\0';
    return (gpointer)c;
}

static void 
set_split_reconcile_state( gpointer pObject, gpointer pValue )
{
    Split* pSplit = GNC_SPLIT(pObject);
    const gchar* s = (const gchar*)pValue;

    xaccSplitSetReconcile( pSplit, s[0] );
}

static gpointer
get_split_reconcile_date( gpointer pObject, const QofParam* param )
{
    const Split* pSplit = GNC_SPLIT(pObject);
    static Timespec ts;

    ts = xaccSplitRetDateReconciledTS( pSplit );
    return (gpointer)&ts;
}

static void 
set_split_reconcile_date( gpointer pObject, gpointer pValue )
{
    Split* pSplit = GNC_SPLIT(pObject);
    Timespec* pTS = (Timespec*)pValue;

    xaccSplitSetDateReconciledTS( pSplit, pTS );
}

static gpointer
get_split_value( gpointer pObject, const QofParam* param )
{
    const Split* pSplit = GNC_SPLIT(pObject);
    static gnc_numeric v;

    v = xaccSplitGetValue( pSplit );
    return (gpointer)&v;
}

static void 
set_split_value( gpointer pObject, gpointer pValue )
{
    Split* pSplit = GNC_SPLIT(pObject);
    gnc_numeric* pV = (gnc_numeric*)pValue;

    xaccSplitSetValue( pSplit, *pV );
}

static gpointer
get_split_quantity( gpointer pObject, const QofParam* param )
{
    const Split* pSplit = GNC_SPLIT(pObject);
    static gnc_numeric v;

    v = xaccSplitGetAmount( pSplit );
    return (gpointer)&v;
}

static void 
set_split_quantity( gpointer pObject, gpointer pValue )
{
    Split* pSplit = GNC_SPLIT(pObject);
    gnc_numeric* pV = (gnc_numeric*)pValue;

    xaccSplitSetAmount( pSplit, *pV );
}

static gpointer
get_split_account_guid( gpointer pObject, const QofParam* param )
{
    const Split* pSplit = GNC_SPLIT(pObject);
    Account* pAccount = xaccSplitGetAccount( pSplit );

    return (gpointer)qof_instance_get_guid( QOF_INSTANCE(pAccount) );
}

static void 
set_split_account_guid( gpointer pObject, gpointer pValue )
{
    Split* pSplit = GNC_SPLIT(pObject);
    QofBook* pBook = qof_instance_get_book( QOF_INSTANCE(pSplit) );
    GUID* guid = (GUID*)pValue;
    Account* pAccount = xaccAccountLookup( guid, pBook );

    xaccSplitSetAccount( pSplit, pAccount );
}

static Split*
load_split( GncGdaBackend* be, GdaDataModel* pModel, int row, Split* pSplit )
{
    const GUID* guid;
    GUID split_guid;

    guid = gnc_gda_load_guid( pModel, row );
    split_guid = *guid;

    if( pSplit == NULL ) {
        pSplit = xaccSplitLookup( &split_guid, be->primary_book );
        if( pSplit == NULL ) {
            pSplit = xaccMallocSplit( be->primary_book );
        }
    }

    /* If the split is dirty, don't overwrite it */
    if( qof_instance_is_dirty( QOF_INSTANCE(pSplit) ) ) {
        return pSplit;
    }

    gnc_gda_load_object( pModel, row, GNC_ID_SPLIT, pSplit, split_col_table );
    gnc_gda_slots_load( be, qof_instance_get_guid( QOF_INSTANCE(pSplit) ),
                            qof_instance_get_slots( QOF_INSTANCE(pSplit) ) );

    g_assert( pSplit == xaccSplitLookup( &split_guid, be->primary_book ) );

    qof_instance_mark_clean( QOF_INSTANCE(pSplit) );

    return pSplit;
}

static void
load_splits( GncGdaBackend* be, const GUID* guid )
{
    GdaObject* ret;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
    GdaQuery* query;
    GdaQueryCondition* cond;
    GValue value;

    guid_to_string_buff( guid, guid_buf );
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

        for( r = 0; r < numRows; r++ ) {
            load_split( be, pModel, r, NULL );
        }
    }
}

static Transaction*
load_tx( GncGdaBackend* be, GdaDataModel* pModel, int row, Transaction* pTx )
{
    const GUID* guid;
    GUID tx_guid;

    guid = gnc_gda_load_guid( pModel, row );
    tx_guid = *guid;

    if( pTx == NULL ) {
        pTx = xaccTransLookup( &tx_guid, be->primary_book );
        if( pTx == NULL ) {
            pTx = xaccMallocTransaction( be->primary_book );
        }
    }
    xaccTransBeginEdit( pTx );
    gnc_gda_load_object( pModel, row, GNC_ID_TRANS, pTx, tx_col_table );
    gnc_gda_slots_load( be, qof_instance_get_guid( QOF_INSTANCE(pTx) ),
                            qof_instance_get_slots( QOF_INSTANCE(pTx) ) );
    load_splits( be, qof_instance_get_guid( QOF_INSTANCE(pTx) ) );

    qof_instance_mark_clean( QOF_INSTANCE(pTx) );
    xaccTransCommitEdit( pTx );

    g_assert( pTx == xaccTransLookup( &tx_guid, be->primary_book ) );

    return pTx;
}

static void
query_transactions( GncGdaBackend* be, GdaQuery* query )
{
    GdaObject* ret;

    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;

        for( r = 0; r < numRows; r++ ) {
            load_tx( be, pModel, r, NULL );
        }
    }
}

/* ================================================================= */
static void
create_transaction_tables( GncGdaBackend* be )
{
    gnc_gda_create_table_if_needed( be, TRANSACTION_TABLE, tx_col_table );
    gnc_gda_create_table_if_needed( be, SPLIT_TABLE, split_col_table );
}
/* ================================================================= */
static void
delete_split_slots_cb( gpointer data, gpointer user_data )
{
    split_info_t* split_info = (split_info_t*)user_data;
    Split* pSplit = GNC_SPLIT(data);

    gnc_gda_slots_delete( split_info->be,
                    qof_instance_get_guid( QOF_INSTANCE(pSplit) ) );
}

static void
delete_splits( GncGdaBackend* be, Transaction* pTx )
{
    split_info_t split_info;

    (void)gnc_gda_do_db_operation( be, OP_DB_DELETE, SPLIT_TABLE,
                                SPLIT_TABLE, pTx, guid_col_table );
    split_info.be = be;

    g_list_foreach( xaccTransGetSplitList( pTx ), delete_split_slots_cb, &split_info );
}

static void
commit_split( GncGdaBackend* be, QofInstance* inst )
{
    Split* pSplit = GNC_SPLIT(inst);

    (void)gnc_gda_do_db_operation( be,
                        (inst->do_free ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        SPLIT_TABLE,
                        GNC_ID_SPLIT, pSplit,
                        split_col_table );
    gnc_gda_slots_save( be,
                        qof_instance_get_guid( QOF_INSTANCE(pSplit) ),
                        qof_instance_get_slots( QOF_INSTANCE(pSplit) ) );
}

static void
save_split_cb( gpointer data, gpointer user_data )
{
    split_info_t* split_info = (split_info_t*)user_data;
    Split* pSplit = GNC_SPLIT(data);

    commit_split( split_info->be, QOF_INSTANCE(pSplit) );
}

static void
save_splits( GncGdaBackend* be, const GUID* tx_guid, SplitList* pSplitList )
{
    split_info_t split_info;

    split_info.be = be;
    split_info.guid = tx_guid;
    g_list_foreach( pSplitList, save_split_cb, &split_info );
}

static void
commit_transaction( GncGdaBackend* be, QofInstance* inst )
{
    Transaction* pTx = GNC_TRANS(inst);
    const GUID* guid;

    // Ensure the commodity is in the db
    gnc_gda_save_commodity( be, xaccTransGetCurrency( pTx ) );

    (void)gnc_gda_do_db_operation( be,
                        (inst->do_free ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TRANSACTION_TABLE,
                        GNC_ID_TRANS, pTx,
                        tx_col_table );

    guid = qof_instance_get_guid( inst );

    // Delete any old slots and splits for this transaction
    delete_splits( be, pTx );

    if( !inst->do_free ) {
        SplitList* splits;

        // Now, commit any slots and splits
        gnc_gda_slots_save( be, guid, qof_instance_get_slots( inst ) );
        splits = xaccTransGetSplitList( pTx );
        save_splits( be, guid, splits );

        /* Mark the splits as clean */
        splits = xaccTransGetSplitList( pTx );
        for( ; splits != NULL; splits = splits->next ) {
            QofInstance* inst = QOF_INSTANCE(splits->data);

            inst->dirty = FALSE;
        }
    } else {
        gnc_gda_slots_delete( be, guid );
    }
}

void gnc_gda_transaction_commit_splits( GncGdaBackend* be, Transaction* pTx )
{
    SplitList* splits;
    Split* s;
    QofBackend* qbe = (QofBackend*)be;
    
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
    GList* pOrTerms = qof_query_get_terms( pQuery );
    GList* pAndTerms;
    GList* andTerm;
    QofQueryTerm* pTerm;
    QofQueryPredData* pPredData;
    GSList* pParamPath;

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
compile_split_query( GncGdaBackend* pBackend, QofQuery* pQuery )
{
    gchar* buf;
    const GUID* acct_guid;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];

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
    subQuery = gda_query_new( pBackend->pDict );
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
    query = gnc_gda_create_select_query( pBackend, TRANSACTION_TABLE );
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
}

static void
run_split_query( GncGdaBackend* be, gpointer pQuery )
{
    GdaQuery* query;
#if 1
    GError* error = NULL;
    const gchar* sql = (const gchar*)pQuery;

    query = gda_query_new_from_sql( be->pDict, sql, &error );
    if( query == NULL ) {
        g_critical( "SQL error: %s\n", error->message );
        return;
    }
    error = NULL;
#else
    query = GDA_QUERY(pQuery);
#endif
    query_transactions( be, query );
}

static void
free_split_query( GncGdaBackend* pBackend, gpointer pQuery )
{
#if 1
    g_free( pQuery );
#else
    g_object_unref( G_OBJECT(pQuery) );
#endif
}

/* ================================================================= */
void
gnc_gda_init_transaction_handler( void )
{
    static GncGdaDataType_t be_data_tx =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_TRANS,
        commit_transaction,            /* commit */
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
}

/* ========================== END OF FILE ===================== */
