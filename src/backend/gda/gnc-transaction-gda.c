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

#include "gnc-backend-gda.h"
#include "gnc-transaction-gda.h"
#include "gnc-commodity.h"
#include "gnc-commodity-gda.h"
#include "gnc-slots-gda.h"

#include "gnc-engine.h"

#include "Account.h"
#include "Transaction.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

#define TRANSACTION_TABLE "transactions"
#define SPLIT_TABLE "splits"

typedef struct {
	GncGdaBackend* be;
	const GUID* guid;
} split_info_t;

static gpointer get_guid( gpointer pObject );
static void set_guid( gpointer pObject, const gpointer pValue );
static void retrieve_guid( gpointer pObject, const gpointer pValue );
static gpointer get_tx_currency( gpointer pObject );
static void set_tx_currency( gpointer pObject, const gpointer pValue );
static gpointer get_tx_post_date( gpointer pObject );
static void set_tx_post_date( gpointer pObject, const gpointer pValue );
static gpointer get_tx_enter_date( gpointer pObject );
static void set_tx_enter_date( gpointer pObject, const gpointer pValue );

static col_cvt_t tx_col_table[] =
{
	{ "guid",			CT_GUID,	  0, COL_NNUL|COL_PKEY, NULL,
			get_guid, set_guid },
	{ "currency_guid",	CT_GUID,	  0, COL_NNUL,	NULL,
			get_tx_currency, set_tx_currency },
	{ "num",			CT_STRING,	 50, COL_NNUL, TRANS_NUM },
	{ "post_date",		CT_TIMESPEC,  0, COL_NNUL, NULL,
			get_tx_post_date, set_tx_post_date },
	{ "enter_date",		CT_TIMESPEC,  0, COL_NNUL, NULL,
			get_tx_enter_date, set_tx_enter_date },
	{ "description",	CT_STRING,	500, 0,	NULL,
			(GNC_GDA_FN_GETTER)xaccTransGetDescription,
			(GNC_GDA_FN_SETTER)xaccTransSetDescription },
	{ NULL }
};

// Table to retrieve just the guid
static col_cvt_t guid_table[] =
{
	{ "guid", CT_GUID, 0, 0, NULL, NULL, retrieve_guid },
	{ NULL }
};

static gpointer get_split_tx_guid( gpointer pObject );
static void set_split_tx_guid( gpointer pObject, const gpointer pValue );
static gpointer get_split_reconcile_state( gpointer pObject );
static void set_split_reconcile_state( gpointer pObject, const gpointer pValue );
static gpointer get_split_reconcile_date( gpointer pObject );
static void set_split_reconcile_date( gpointer pObject, const gpointer pValue );
static gpointer get_split_value( gpointer pObject );
static void set_split_value( gpointer pObject, const gpointer pValue );
static gpointer get_split_quantity( gpointer pObject );
static void set_split_quantity( gpointer pObject, const gpointer pValue );
static gpointer get_split_account( gpointer pObject );
static void set_split_account( gpointer pObject, const gpointer pValue );

static col_cvt_t split_col_table[] =
{
	{ "guid",			CT_GUID,	  0, COL_NNUL|COL_PKEY,	NULL,
			get_guid, set_guid },
	{ "tx_guid",		CT_GUID,	  0, COL_NNUL,	NULL,
			get_split_tx_guid, set_split_tx_guid },
	{ "memo",			CT_STRING,	 50, COL_NNUL,	SPLIT_MEMO },
	{ "action",			CT_STRING,	 50, COL_NNUL,	SPLIT_ACTION },
	{ "reconcile_state", CT_STRING,	  1, COL_NNUL,	NULL,
			get_split_reconcile_state, set_split_reconcile_state },
	{ "reconcile_date",	CT_TIMESPEC,  0, COL_NNUL,	NULL,
			get_split_reconcile_date, set_split_reconcile_date },
	{ "value",			CT_NUMERIC,	  0, COL_NNUL,	NULL,
			get_split_value, set_split_value },
	{ "quantity",		CT_NUMERIC,	  0, COL_NNUL,	NULL,
			get_split_quantity, set_split_quantity },
	{ "account_guid",	CT_GUID,	  0, COL_NNUL,	NULL,
			get_split_account, set_split_account },
	{ NULL }
};

static col_cvt_t guid_col_table[] =
{
	{ "tx_guid", CT_GUID, 0, 0, NULL, get_guid, set_guid },
	{ NULL }
};

/* ================================================================= */
static gpointer
get_guid( gpointer pObject )
{
	return (gpointer)qof_entity_get_guid( (QofEntity*)pObject );
}

static void 
set_guid( gpointer pObject, const gpointer pValue )
{
	QofEntity* pEntity = (QofEntity*)pObject;
	GUID* guid = (GUID*)pValue;

	qof_entity_set_guid( pEntity, guid );
}

static void 
retrieve_guid( gpointer pObject, const gpointer pValue )
{
	GUID** ppGuid = (GUID**)pObject;
	GUID* guid = (GUID*)pValue;

	*ppGuid = guid;
}

static gpointer
get_tx_currency( gpointer pObject )
{
	Transaction* pTx = (Transaction*)pObject;

	return (gpointer)qof_instance_get_guid(
						(QofInstance*)xaccTransGetCurrency( pTx ) );
}

static void 
set_tx_currency( gpointer pObject, const gpointer pValue )
{
	Transaction* pTx = (Transaction*)pObject;
	QofBook* pBook = qof_instance_get_book( (QofInstance*)pTx );
	gnc_commodity* pCurrency;
	GUID* guid = (GUID*)pValue;

	pCurrency = gnc_commodity_find_commodity_by_guid( guid, pBook );
	xaccTransSetCurrency( pTx, pCurrency );
}

static gpointer
get_tx_post_date( gpointer pObject )
{
	Transaction* pTx = (Transaction*)pObject;
	static Timespec ts;

	ts = xaccTransRetDatePostedTS( pTx );
	return (gpointer)&ts;
}

static void 
set_tx_post_date( gpointer pObject, const gpointer pValue )
{
	Transaction* pTx = (Transaction*)pObject;
	Timespec* pTS = (Timespec*)pValue;

	xaccTransSetDatePostedTS( pTx, pTS );
}

static gpointer
get_tx_enter_date( gpointer pObject )
{
	Transaction* pTx = (Transaction*)pObject;
	static Timespec ts;

	ts = xaccTransRetDateEnteredTS( pTx );
	return (gpointer)&ts;
}

static void 
set_tx_enter_date( gpointer pObject, const gpointer pValue )
{
	Transaction* pTx = (Transaction*)pObject;
	Timespec* pTS = (Timespec*)pValue;

	xaccTransSetDateEnteredTS( pTx, pTS );
}

static gpointer
get_split_tx_guid( gpointer pObject )
{
	Split* pSplit = (Split*)pObject;
	Transaction* pTx = xaccSplitGetParent( pSplit );

	return (gpointer)qof_instance_get_guid( (QofInstance*)pTx );
}

static void 
set_split_tx_guid( gpointer pObject, const gpointer pValue )
{
	Split* pSplit = (Split*)pObject;
	QofBook* pBook = qof_instance_get_book( (QofInstance*)pSplit );
	GUID* guid = (GUID*)pValue;
	Transaction* pTx = xaccTransLookup( guid, pBook );

	xaccSplitSetParent( pSplit, pTx );
}

static gpointer
get_split_reconcile_state( gpointer pObject )
{
	Split* pSplit = (Split*)pObject;
	static gchar c[2];

	c[0] = xaccSplitGetReconcile( pSplit );
	c[1] = '\0';
	return (gpointer)c;
}

static void 
set_split_reconcile_state( gpointer pObject, const gpointer pValue )
{
	Split* pSplit = (Split*)pObject;
	const gchar* s = (const gchar*)pValue;

	xaccSplitSetReconcile( pSplit, s[0] );
}

static gpointer
get_split_reconcile_date( gpointer pObject )
{
	Split* pSplit = (Split*)pObject;
	static Timespec ts;

	ts = xaccSplitRetDateReconciledTS( pSplit );
	return (gpointer)&ts;
}

static void 
set_split_reconcile_date( gpointer pObject, const gpointer pValue )
{
	Split* pSplit = (Split*)pObject;
	Timespec* pTS = (Timespec*)pValue;

	xaccSplitSetDateReconciledTS( pSplit, pTS );
}

static gpointer
get_split_value( gpointer pObject )
{
	Split* pSplit = (Split*)pObject;
	static gnc_numeric v;

	v = xaccSplitGetValue( pSplit );
	return (gpointer)&v;
}

static void 
set_split_value( gpointer pObject, const gpointer pValue )
{
	Split* pSplit = (Split*)pObject;
	gnc_numeric* pV = (gnc_numeric*)pValue;

	xaccSplitSetValue( pSplit, *pV );
}

static gpointer
get_split_quantity( gpointer pObject )
{
	Split* pSplit = (Split*)pObject;
	static gnc_numeric v;

	v = xaccSplitGetAmount( pSplit );
	return (gpointer)&v;
}

static void 
set_split_quantity( gpointer pObject, const gpointer pValue )
{
	Split* pSplit = (Split*)pObject;
	gnc_numeric* pV = (gnc_numeric*)pValue;

	xaccSplitSetAmount( pSplit, *pV );
}

static gpointer
get_split_account( gpointer pObject )
{
	Split* pSplit = (Split*)pObject;
	Account* pAccount = xaccSplitGetAccount( pSplit );

	return (gpointer)qof_instance_get_guid( (QofInstance*)pAccount );
}

static void 
set_split_account( gpointer pObject, const gpointer pValue )
{
	Split* pSplit = (Split*)pObject;
	QofBook* pBook = qof_instance_get_book( (QofInstance*)pSplit );
	GUID* guid = (GUID*)pValue;
	Account* pAccount = xaccAccountLookup( guid, pBook );

	xaccSplitSetAccount( pSplit, pAccount );
}

static Split*
load_split( GncGdaBackend* be, GdaDataModel* pModel, int row, Split* pSplit )
{
	const GUID* guid;
	GUID split_guid;

	gnc_gda_load_object( pModel, row, GNC_ID_SPLIT, &guid, guid_table );
	split_guid = *guid;

	if( pSplit == NULL ) {
		pSplit = xaccSplitLookup( &split_guid, be->primary_book );
		if( pSplit == NULL ) {
			pSplit = xaccMallocSplit( be->primary_book );
		}
	}
	gnc_gda_load_object( pModel, row, GNC_ID_SPLIT, pSplit, split_col_table );
	gnc_gda_slots_load( be, qof_instance_get_guid( (QofInstance*)pSplit ),
							qof_instance_get_slots( (QofInstance*)pSplit ) );

	g_assert( pSplit == xaccSplitLookup( &split_guid, be->primary_book ) );

	return pSplit;
}

static void
load_splits( GncGdaBackend* be, const GUID* guid )
{
	GError* error = NULL;
	gchar* buf;
	GdaQuery* query;
	GdaObject* ret;
	gchar guid_buf[GUID_ENCODING_LENGTH+1];

	guid_to_string_buff( guid, guid_buf );
	buf = g_strdup_printf( "SELECT * FROM %s where tx_guid='%s'",
						SPLIT_TABLE, guid_buf );
	query = gda_query_new_from_sql( be->pDict, buf, &error );
	g_free( buf );
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

	gnc_gda_load_object( pModel, row, GNC_ID_TRANS, &guid, guid_table );
	tx_guid = *guid;

	if( pTx == NULL ) {
		pTx = xaccTransLookup( &tx_guid, be->primary_book );
		if( pTx == NULL ) {
			pTx = xaccMallocTransaction( be->primary_book );
		}
	}
	xaccTransBeginEdit( pTx );
	gnc_gda_load_object( pModel, row, GNC_ID_TRANS, pTx, tx_col_table );
	gnc_gda_slots_load( be, qof_instance_get_guid( (QofInstance*)pTx ),
							qof_instance_get_slots( (QofInstance*)pTx ) );
	load_splits( be, qof_instance_get_guid( (QofInstance*)pTx ) );
	xaccTransCommitEdit( pTx );

	g_assert( pTx == xaccTransLookup( &tx_guid, be->primary_book ) );

	return pTx;
}

static void
load_transactions( GncGdaBackend* be, const GUID* guid )
{
	GError* error = NULL;
	gchar* buf;
	GdaQuery* query;
	GdaObject* ret;
	gchar guid_buf[GUID_ENCODING_LENGTH+1];

	guid_to_string_buff( guid, guid_buf );
	buf = g_strdup_printf( "SELECT * FROM %s where guid='%s'",
						TRANSACTION_TABLE, guid_buf );
	query = gda_query_new_from_sql( be->pDict, buf, &error );
	g_free( buf );
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

		for( r = 0; r < numRows; r++ ) {
			load_tx( be, pModel, r, NULL );
		}
	}
}

static void
query_transactions( GncGdaBackend* be, const gchar* sql )
{
	GError* error = NULL;
	GdaQuery* query;
	GdaObject* ret;

	query = gda_query_new_from_sql( be->pDict, sql, &error );
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
delete_splits( GncGdaBackend* be, Transaction* pTx )
{
	(void)gnc_gda_do_db_operation( be, OP_DB_DELETE, SPLIT_TABLE,
								SPLIT_TABLE, pTx, guid_col_table );
}

static void
save_split( gpointer data, gpointer user_data )
{
	split_info_t* split_info = (split_info_t*)user_data;
	Split* pSplit = (Split*)data;

	(void)gnc_gda_do_db_operation( split_info->be, OP_DB_ADD, SPLIT_TABLE,
									GNC_ID_SPLIT, pSplit, split_col_table );
}

static void
save_splits( GncGdaBackend* be, const GUID* tx_guid, SplitList* pSplitList )
{
	split_info_t split_info;

	split_info.be = be;
	split_info.guid = tx_guid;
	g_list_foreach( pSplitList, save_split, &split_info );
}

static void
commit_transaction( GncGdaBackend* be, QofInstance* inst )
{
	Transaction* pTx = (Transaction*)inst;

	// Ensure the commodity is in the db
	gnc_gda_save_commodity( be, xaccTransGetCurrency( pTx ) );

	(void)gnc_gda_do_db_operation( be,
							(inst->do_free ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
							TRANSACTION_TABLE,
							GNC_ID_TRANS, pTx,
							tx_col_table );

	// Delete any old splits for this transaction
	delete_splits( be, pTx );

	// Now, commit any slots
	save_splits( be, qof_instance_get_guid( inst ),
				xaccTransGetSplitList( pTx ) );
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

	acct_guid = get_guid_from_query( pQuery );
	guid_to_string_buff( acct_guid, guid_buf );
	buf = g_strdup_printf( "SELECT * FROM %s WHERE guid IN (SELECT DISTINCT tx_guid FROM %s WHERE account_guid = '%s')",
							TRANSACTION_TABLE, SPLIT_TABLE, guid_buf );
	return buf;
}

static void
run_split_query( GncGdaBackend* pBackend, gpointer pQuery )
{
	const gchar* sql = (const gchar*)pQuery;

	query_transactions( pBackend, sql );
}

static void
free_split_query( GncGdaBackend* pBackend, gpointer pQuery )
{
	g_free( pQuery );
}

/* ================================================================= */
void
gnc_gda_init_transaction_handler( void )
{
	static GncGdaDataType_t be_data_tx =
	{
		GNC_GDA_BACKEND_VERSION,
		GNC_ID_TRANS,
		commit_transaction,			/* commit */
		NULL,						/* initial_load */
		create_transaction_tables	/* create tables */
	};
	static GncGdaDataType_t be_data_split =
	{
		GNC_GDA_BACKEND_VERSION,
		GNC_ID_SPLIT,
		NULL,						/* commit */
		NULL,						/* initial_load */
		NULL,						/* create tables */
		compile_split_query,
		run_split_query,
		free_split_query
	};

	qof_object_register_backend( GNC_ID_TRANS, GNC_GDA_BACKEND, &be_data_tx );
	qof_object_register_backend( GNC_ID_SPLIT, GNC_GDA_BACKEND, &be_data_split );
}

/* ========================== END OF FILE ===================== */
