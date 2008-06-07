/********************************************************************
 * gnc-backend-sql.c: load and save data to SQL                     *
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
/** @file gnc-backend-sql.c
 *  @brief load and save data to SQL 
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */

#include "config.h"

#include <errno.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>

#include "qof.h"
#include "qofquery-p.h"
#include "qofquerycore-p.h"
#include "Account.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "SX-book.h"
#include "Recurrence.h"

#include "gnc-backend-util-sql.h"
#include "gnc-gconf-utils.h"

#include "gnc-account-sql.h"
#include "gnc-book-sql.h"
#include "gnc-budget-sql.h"
#include "gnc-commodity-sql.h"
#include "gnc-lots-sql.h"
#include "gnc-price-sql.h"
#include "gnc-pricedb.h"
#include "gnc-recurrence-sql.h"
#include "gnc-schedxaction-sql.h"
#include "gnc-slots-sql.h"
#include "gnc-transaction-sql.h"

#include "gnc-backend-sql.h"

static const gchar* convert_search_obj( QofIdType objType );
static void gnc_sql_init_object_handlers( void );
static void update_save_progress( GncSqlBackend* be );

#define TRANSACTION_NAME "trans"

typedef struct {
    QofIdType searchObj;
    gpointer pCompiledQuery;
} gnc_sql_query_info;

/* callback structure */
typedef struct {
    gboolean ok;
    GncSqlBackend* be;
    QofInstance* inst;
    QofQuery* pQuery;
    gpointer pCompiledQuery;
    gnc_sql_query_info* pQueryInfo;
} sql_backend;

static QofLogModule log_module = G_LOG_DOMAIN;

#define SQLITE_PROVIDER_NAME "SQLite"
#define URI_PREFIX "gda://"

/* ================================================================= */

void
gnc_sql_init( GncSqlBackend* be )
{
	gnc_sql_register_standard_col_type_handlers();
	gnc_sql_init_object_handlers();
}

/* ================================================================= */

static void
create_tables_cb( const gchar* type, gpointer data_p, gpointer be_p )
{
    GncSqlDataType_t* pData = data_p;
    GncSqlBackend* be = be_p;

    g_return_if_fail( type != NULL && data_p != NULL && be_p != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );

    if( pData->create_tables != NULL ) {
        (pData->create_tables)( be );
    }
}

/* ================================================================= */

static const gchar* fixed_load_order[] =
{ GNC_ID_BOOK, GNC_ID_COMMODITY, GNC_ID_ACCOUNT, NULL };

static void
initial_load_cb( const gchar* type, gpointer data_p, gpointer be_p )
{
    GncSqlDataType_t* pData = data_p;
    GncSqlBackend* be = be_p;
	int i;

    g_return_if_fail( type != NULL && data_p != NULL && be_p != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );

	// Don't need to load anything if it has already been loaded with the fixed order
	for( i = 0; fixed_load_order[i] != NULL; i++ ) {
    	if( g_ascii_strcasecmp( type, fixed_load_order[i] ) == 0 ) return;
	}

    if( pData->initial_load != NULL ) {
        (pData->initial_load)( be );
    }
}

void
gnc_sql_load( GncSqlBackend* be, QofBook *book )
{
    GncSqlDataType_t* pData;
	int i;
	Account* root;
	GError* error = NULL;

	g_return_if_fail( be != NULL );
	g_return_if_fail( book != NULL );

    ENTER( "be=%p, book=%p", be, book );

    g_assert( be->primary_book == NULL );
    be->primary_book = book;

	// Set up table version information
//	gnc_sql_init_version_info( be );

    // Call all object backends to create any required tables
//    qof_object_foreach_backend( GNC_SQL_BACKEND, create_tables_cb, be );

    /* Load any initial stuff */
    be->loading = TRUE;
    
    /* Some of this needs to happen in a certain order */
	for( i = 0; fixed_load_order[i] != NULL; i++ ) {
    	pData = qof_object_lookup_backend( fixed_load_order[i], GNC_SQL_BACKEND );
    	if( pData->initial_load != NULL ) {
        	(pData->initial_load)( be );
		}
    }

	root = gnc_book_get_root_account( book );
	gnc_account_foreach_descendant( root, (AccountCb)xaccAccountBeginEdit, NULL );

    qof_object_foreach_backend( GNC_SQL_BACKEND, initial_load_cb, be );

	gnc_account_foreach_descendant( root, (AccountCb)xaccAccountCommitEdit, NULL );

    be->loading = FALSE;

	// Mark the book as clean
	qof_book_mark_saved( book );

    LEAVE( "" );
}

/* ================================================================= */

static gint
compare_namespaces(gconstpointer a, gconstpointer b)
{
    const gchar *sa = (const gchar *) a;
    const gchar *sb = (const gchar *) b;

    return( safe_strcmp( sa, sb ) );
}

static gint
compare_commodity_ids(gconstpointer a, gconstpointer b)
{
    const gnc_commodity *ca = (const gnc_commodity *) a;
    const gnc_commodity *cb = (const gnc_commodity *) b;
  
    return( safe_strcmp( gnc_commodity_get_mnemonic( ca ),
                     	 gnc_commodity_get_mnemonic( cb ) ) );
}

static void
write_commodities( GncSqlBackend* be, QofBook* book )
{
    gnc_commodity_table* tbl;
    GList* namespaces;
    GList* lp;

	g_return_if_fail( be != NULL );
	g_return_if_fail( book != NULL );

    tbl = gnc_book_get_commodity_table( book );
    namespaces = gnc_commodity_table_get_namespaces( tbl );
    if( namespaces != NULL ) {
        namespaces = g_list_sort( namespaces, compare_namespaces );
    }
    for( lp = namespaces; lp != NULL; lp = lp->next ) {
        GList* comms;
        GList* lp2;
        
        comms = gnc_commodity_table_get_commodities( tbl, lp->data );
        comms = g_list_sort( comms, compare_commodity_ids );

        for( lp2 = comms; lp2 != NULL; lp2 = lp2->next ) {
	    	gnc_sql_save_commodity( be, GNC_COMMODITY(lp2->data) );
        }
    }
}

static void
write_account_tree( GncSqlBackend* be, Account* root )
{
    GList* descendants;
    GList* node;

	g_return_if_fail( be != NULL );
	g_return_if_fail( root != NULL );

    descendants = gnc_account_get_descendants( root );
    for( node = descendants; node != NULL; node = g_list_next(node) ) {
        gnc_sql_save_account( QOF_INSTANCE(GNC_ACCOUNT(node->data)), be );
		update_save_progress( be );
    }
    g_list_free( descendants );
}

static void
write_accounts( GncSqlBackend* be )
{
	g_return_if_fail( be != NULL );

    write_account_tree( be, gnc_book_get_root_account( be->primary_book ) );
}

static int
write_tx( Transaction* tx, gpointer data )
{
    GncSqlBackend* be = (GncSqlBackend*)data;

	g_return_val_if_fail( tx != NULL, 0 );
	g_return_val_if_fail( data != NULL, 0 );

    gnc_sql_save_transaction( QOF_INSTANCE(tx), be );
	update_save_progress( be );

    return 0;
}

static void
write_transactions( GncSqlBackend* be )
{
	g_return_if_fail( be != NULL );
	
    xaccAccountTreeForEachTransaction( gnc_book_get_root_account( be->primary_book ),
                                       write_tx,
                                       (gpointer)be );
}

static void
write_template_transactions( GncSqlBackend* be )
{
    Account* ra;

	g_return_if_fail( be != NULL );

    ra = gnc_book_get_template_root( be->primary_book );
    if( gnc_account_n_descendants( ra ) > 0 ) {
        write_account_tree( be, ra );
        xaccAccountTreeForEachTransaction( ra, write_tx, (gpointer)be );
    }
}

static void
write_schedXactions( GncSqlBackend* be )
{
    GList* schedXactions;
    SchedXaction* tmpSX;

	g_return_if_fail( be != NULL );

    schedXactions = gnc_book_get_schedxactions( be->primary_book )->sx_list;

    for( ; schedXactions != NULL; schedXactions = schedXactions->next ) {
        tmpSX = schedXactions->data;
		gnc_sql_save_schedxaction( QOF_INSTANCE( tmpSX ), be );
    }
}

static void
write_cb( const gchar* type, gpointer data_p, gpointer be_p )
{
    GncSqlDataType_t* pData = data_p;
    GncSqlBackend* be = (GncSqlBackend*)be_p;

    g_return_if_fail( type != NULL && data_p != NULL && be_p != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );

    if( pData->write != NULL ) {
        (pData->write)( be );
    }
}

static void
update_save_progress( GncSqlBackend* be )
{
	if( be->be.percentage != NULL ) {
		gint percent_done;

		be->operations_done++;
		percent_done = be->operations_done * 100 / be->obj_total;
		if( percent_done > 100 ) {
			percent_done = 100;
		}
		(be->be.percentage)( NULL, percent_done );
	}
}

void
gnc_sql_sync_all( GncSqlBackend* be, QofBook *book )
{
    GError* error = NULL;
    gint row;
    gint numTables;
	gboolean status;

	g_return_if_fail( be != NULL );
	g_return_if_fail( book != NULL );

    ENTER( "book=%p, primary=%p", book, be->primary_book );

#if 0
    /* Destroy the current contents of the database */
	mstore = gda_connection_get_meta_store( be->pConnection );
	tables = gda_connection_get_meta_store_data( be->pConnection, GDA_CONNECTION_META_TABLES, &error, 0 );
//    tables = gda_connection_get_schema( be->pConnection,
                                        //GDA_CONNECTION_SCHEMA_TABLES,
                                        //NULL,
                                        //&error );
    if( error != NULL ) {
        PERR( "SQL error: %s\n", error->message );
    }
    numTables = gda_data_model_get_n_rows( tables );
    for( row = 0; row < numTables; row++ ) {
        const GValue* row_value;
        const gchar* table_name;
		GdaServerOperation* op;

        row_value = gda_data_model_get_value_at( tables, 0, row );
        table_name = g_value_get_string( row_value );
        error = NULL;
		op = gda_prepare_drop_table( be->pConnection, table_name, &error );
		if( error != NULL ) {
			PERR( "Unable to create op: %s\n", error->message );
		}
		if( op != NULL ) {
			error = NULL;
			gda_perform_drop_table( op, &error );
            if( error != NULL ) {
                PERR( "SQL error: %s\n", error->message );
            }
        }
    }
#endif

	gnc_sql_reset_version_info( be );

    /* Create new tables */
	be->is_pristine_db = TRUE;
    qof_object_foreach_backend( GNC_SQL_BACKEND, create_tables_cb, be );

    /* Save all contents */
	be->primary_book = book;
	be->obj_total = 0;
    be->obj_total += 1 + gnc_account_n_descendants( gnc_book_get_root_account( book ) );
	be->obj_total += gnc_book_count_transactions( book );
	be->operations_done = 0;

	error = NULL;
	gnc_sql_connection_begin_transaction( be->conn );

	// FIXME: should write the set of commodities that are used 
    //write_commodities( be, book );
	gnc_sql_save_book( QOF_INSTANCE(book), be );
    write_accounts( be );
    write_transactions( be );
    write_template_transactions( be );
    write_schedXactions( be );
    qof_object_foreach_backend( GNC_SQL_BACKEND, write_cb, be );

	gnc_sql_connection_commit_transaction( be->conn );
	be->is_pristine_db = FALSE;

	// Mark the book as clean
	qof_book_mark_saved( book );

    LEAVE( "book=%p", book );
}

/* ================================================================= */
/* Routines to deal with the creation of multiple books. */

void
gnc_sql_begin_edit( QofBackend *qbe, QofInstance *inst )
{
	g_return_if_fail( qbe != NULL );
	g_return_if_fail( inst != NULL );
}

void
gnc_sql_rollback_edit( QofBackend *qbe, QofInstance *inst )
{
	g_return_if_fail( qbe != NULL );
	g_return_if_fail( inst != NULL );
}

static void
commit_cb( const gchar* type, gpointer data_p, gpointer be_data_p )
{
    GncSqlDataType_t* pData = data_p;
    sql_backend* be_data = be_data_p;

    g_return_if_fail( type != NULL && pData != NULL && be_data != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );

    /* If this has already been handled, or is not the correct handler, return */
    if( strcmp( pData->type_name, be_data->inst->e_type ) != 0 ) return;
    if( be_data->ok ) return;

    if( pData->commit != NULL ) {
        (pData->commit)( be_data->inst, be_data->be );
        be_data->ok = TRUE;
    }
}

/* Commit_edit handler - find the correct backend handler for this object
 * type and call its commit handler
 */
void
gnc_sql_commit_edit( QofBackend *qbe, QofInstance *inst )
{
	GncSqlBackend* be = (GncSqlBackend*)qbe;
    sql_backend be_data;
	GError* error;
	gboolean status;

	g_return_if_fail( be != NULL );
	g_return_if_fail( inst != NULL );

    /* During initial load where objects are being created, don't commit
    anything */
    if( be->loading ) {
	    return;
	}

	// The engine has a PriceDB object but it isn't in the database
	if( strcmp( inst->e_type, "PriceDB" ) == 0 ) {
    	qof_instance_mark_clean(inst);
    	qof_book_mark_saved( be->primary_book );
		return;
	}

    ENTER( " " );

    DEBUG( "%s dirty = %d, do_free=%d\n",
             (inst->e_type ? inst->e_type : "(null)"),
             qof_instance_get_dirty_flag(inst), qof_instance_get_destroying(inst) );

    if( !qof_instance_get_dirty_flag(inst) && !qof_instance_get_destroying(inst) && GNC_IS_TRANS(inst) ) {
        gnc_sql_transaction_commit_splits( be, GNC_TRANS(inst) );
    }

    if( !qof_instance_get_dirty_flag(inst) && !qof_instance_get_destroying(inst) ) return;

	error = NULL;
	gnc_sql_connection_begin_transaction( be->conn );

    be_data.ok = FALSE;
    be_data.be = be;
    be_data.inst = inst;
    qof_object_foreach_backend( GNC_SQL_BACKEND, commit_cb, &be_data );

    if( !be_data.ok ) {
        PERR( "gnc_sql_commit_edit(): Unknown object type '%s'\n", inst->e_type );
		gnc_sql_connection_rollback_transaction( be->conn );

		// Don't let unknown items still mark the book as being dirty
    	qof_instance_mark_clean(inst);
    	qof_book_mark_saved( be->primary_book );
        return;
    }
	gnc_sql_connection_commit_transaction( be->conn );

    qof_instance_mark_clean(inst);
    qof_book_mark_saved( be->primary_book );

	LEAVE( "" );
}
/* ---------------------------------------------------------------------- */

/* Query processing */

static const gchar*
convert_search_obj( QofIdType objType )
{
    return (gchar*)objType;
}

static void
handle_and_term( QofQueryTerm* pTerm, gchar* sql )
{
    GSList* pParamPath;
    QofQueryPredData* pPredData;
    gboolean isInverted;
    GSList* name;
    gchar val[GUID_ENCODING_LENGTH+1];

	g_return_if_fail( pTerm != NULL );
	g_return_if_fail( sql != NULL );

    pParamPath = qof_query_term_get_param_path( pTerm );
    pPredData = qof_query_term_get_pred_data( pTerm );
    isInverted = qof_query_term_is_inverted( pTerm );

    strcat( sql, "(" );
    if( isInverted ) {
        strcat( sql, "!" );
    }

    for( name = pParamPath; name != NULL; name = name->next ) {
        if( name != pParamPath ) strcat( sql, "." );
        strcat( sql, name->data );
    }

    if( pPredData->how == QOF_COMPARE_LT ) {
        strcat( sql, "<" );
    } else if( pPredData->how == QOF_COMPARE_LTE ) {
        strcat( sql, "<=" );
    } else if( pPredData->how == QOF_COMPARE_EQUAL ) {
        strcat( sql, "=" );
    } else if( pPredData->how == QOF_COMPARE_GT ) {
        strcat( sql, ">" );
    } else if( pPredData->how == QOF_COMPARE_GTE ) {
        strcat( sql, ">=" );
    } else if( pPredData->how == QOF_COMPARE_NEQ ) {
        strcat( sql, "~=" );
    } else {
        strcat( sql, "??" );
    }

    if( strcmp( pPredData->type_name, "string" ) == 0 ) {
        query_string_t pData = (query_string_t)pPredData;
        strcat( sql, "'" );
        strcat( sql, pData->matchstring );
        strcat( sql, "'" );
    } else if( strcmp( pPredData->type_name, "date" ) == 0 ) {
        query_date_t pData = (query_date_t)pPredData;

        (void)gnc_timespec_to_iso8601_buff( pData->date, val );
        strcat( sql, "'" );
        strncat( sql, val, 4+1+2+1+2 );
        strcat( sql, "'" );
    } else if( strcmp( pPredData->type_name, "numeric" ) == 0 ) {
        query_numeric_t pData = (query_numeric_t)pPredData;
    
        strcat( sql, "numeric" );
    } else if( strcmp( pPredData->type_name, "guid" ) == 0 ) {
        query_guid_t pData = (query_guid_t)pPredData;
        (void)guid_to_string_buff( pData->guids->data, val );
        strcat( sql, "'" );
        strcat( sql, val );
        strcat( sql, "'" );
    } else if( strcmp( pPredData->type_name, "gint32" ) == 0 ) {
        query_int32_t pData = (query_int32_t)pPredData;

        sprintf( val, "%d", pData->val );
        strcat( sql, val );
    } else if( strcmp( pPredData->type_name, "gint64" ) == 0 ) {
        query_int64_t pData = (query_int64_t)pPredData;
    
        sprintf( val, "%" G_GINT64_FORMAT, pData->val );
        strcat( sql, val );
    } else if( strcmp( pPredData->type_name, "double" ) == 0 ) {
        query_double_t pData = (query_double_t)pPredData;

        sprintf( val, "%f", pData->val );
        strcat( sql, val );
    } else if( strcmp( pPredData->type_name, "boolean" ) == 0 ) {
        query_boolean_t pData = (query_boolean_t)pPredData;

        sprintf( val, "%d", pData->val );
        strcat( sql, val );
    } else {
        g_assert( FALSE );
    }

    strcat( sql, ")" );
}

static void
compile_query_cb( const gchar* type, gpointer data_p, gpointer be_data_p )
{
    GncSqlDataType_t* pData = data_p;
    sql_backend* be_data = be_data_p;

    g_return_if_fail( type != NULL && pData != NULL && be_data != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );

	// Is this the right item?
    if( strcmp( type, be_data->pQueryInfo->searchObj ) != 0 ) return;
    if( be_data->ok ) return;

    if( pData->compile_query != NULL ) {
        be_data->pQueryInfo->pCompiledQuery = (pData->compile_query)(
                                                            be_data->be,
                                                            be_data->pQuery );
        be_data->ok = TRUE;
    }
}

static gpointer
gnc_sql_compile_query( QofBackend* pBEnd, QofQuery* pQuery )
{
    GncSqlBackend *be = (GncSqlBackend*)pBEnd;
    GList* pBookList;
    QofIdType searchObj;
    gchar sql[1000];
    sql_backend be_data;
    gnc_sql_query_info* pQueryInfo;

	g_return_val_if_fail( pBEnd != NULL, NULL );
	g_return_val_if_fail( pQuery != NULL, NULL );

	ENTER( " " );

    searchObj = qof_query_get_search_for( pQuery );

    pQueryInfo = g_malloc( sizeof( gnc_sql_query_info ) );

    // Try various objects first
    be_data.ok = FALSE;
    be_data.be = be;
    be_data.pQuery = pQuery;
    pQueryInfo->searchObj = searchObj;
    be_data.pQueryInfo = pQueryInfo;

    qof_object_foreach_backend( GNC_SQL_BACKEND, compile_query_cb, &be_data );
    if( be_data.ok ) {
		LEAVE( "" );
        return be_data.pQueryInfo;
    }

    pBookList = qof_query_get_books( pQuery );

    /* Convert search object type to table name */
    sprintf( sql, "SELECT * from %s", convert_search_obj( searchObj ) );
    if( !qof_query_has_terms( pQuery ) ) {
        strcat( sql, ";" );
    } else {
        GList* pOrTerms = qof_query_get_terms( pQuery );
        GList* orTerm;

        strcat( sql, " WHERE " );

        for( orTerm = pOrTerms; orTerm != NULL; orTerm = orTerm->next ) {
            GList* pAndTerms = (GList*)orTerm->data;
            GList* andTerm;

            if( orTerm != pOrTerms ) strcat( sql, " OR " );
            strcat( sql, "(" );
            for( andTerm = pAndTerms; andTerm != NULL; andTerm = andTerm->next ) {
                if( andTerm != pAndTerms ) strcat( sql, " AND " );
                handle_and_term( (QofQueryTerm*)andTerm->data, sql );
            }
            strcat( sql, ")" );
        }
    }

    DEBUG( "Compiled: %s\n", sql );
    pQueryInfo->pCompiledQuery =  g_strdup( sql );

	LEAVE( "" );

    return pQueryInfo;
}

static void
free_query_cb( const gchar* type, gpointer data_p, gpointer be_data_p )
{
    GncSqlDataType_t* pData = data_p;
    sql_backend* be_data = be_data_p;

    g_return_if_fail( type != NULL && pData != NULL && be_data != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );
    if( be_data->ok ) return;
    if( strcmp( type, be_data->pQueryInfo->searchObj ) != 0 ) return;

    if( pData->free_query != NULL ) {
        (pData->free_query)( be_data->be, be_data->pCompiledQuery );
        be_data->ok = TRUE;
    }
}

static void
gnc_sql_free_query( QofBackend* pBEnd, gpointer pQuery )
{
    GncSqlBackend *be = (GncSqlBackend*)pBEnd;
    gnc_sql_query_info* pQueryInfo = (gnc_sql_query_info*)pQuery;
    sql_backend be_data;

	g_return_if_fail( pBEnd != NULL );
	g_return_if_fail( pQuery != NULL );

	ENTER( " " );

    // Try various objects first
    be_data.ok = FALSE;
    be_data.be = be;
    be_data.pCompiledQuery = pQuery;
    be_data.pQueryInfo = pQueryInfo;

    qof_object_foreach_backend( GNC_SQL_BACKEND, free_query_cb, &be_data );
    if( be_data.ok ) {
		LEAVE( "" );
        return;
    }

    DEBUG( "%s\n", (gchar*)pQueryInfo->pCompiledQuery );
    g_free( pQueryInfo->pCompiledQuery );
    g_free( pQueryInfo );

	LEAVE( "" );
}

static void
run_query_cb( const gchar* type, gpointer data_p, gpointer be_data_p )
{
    GncSqlDataType_t* pData = data_p;
    sql_backend* be_data = be_data_p;

    g_return_if_fail( type != NULL && pData != NULL && be_data != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );
    if( be_data->ok ) return;

	// Is this the right item?
    if( strcmp( type, be_data->pQueryInfo->searchObj ) != 0 ) return;

    if( pData->run_query != NULL ) {
        (pData->run_query)( be_data->be, be_data->pCompiledQuery );
        be_data->ok = TRUE;
    }
}

static void
gnc_sql_run_query( QofBackend* pBEnd, gpointer pQuery )
{
    GncSqlBackend *be = (GncSqlBackend*)pBEnd;
    gnc_sql_query_info* pQueryInfo = (gnc_sql_query_info*)pQuery;
    sql_backend be_data;

	g_return_if_fail( pBEnd != NULL );
	g_return_if_fail( pQuery != NULL );
    g_return_if_fail( !be->in_query );

	ENTER( " " );

    be->loading = TRUE;
    be->in_query = TRUE;

    qof_event_suspend();

    // Try various objects first
    be_data.ok = FALSE;
    be_data.be = be;
    be_data.pCompiledQuery = pQueryInfo->pCompiledQuery;
    be_data.pQueryInfo = pQueryInfo;

    qof_object_foreach_backend( GNC_SQL_BACKEND, run_query_cb, &be_data );
    be->loading = FALSE;
    be->in_query = FALSE;
    qof_event_resume();
//    if( be_data.ok ) {
//		LEAVE( "" );
//       	return;
//    }

	// Mark the book as clean
	qof_instance_mark_clean( QOF_INSTANCE(be->primary_book) );

//    DEBUG( "%s\n", (gchar*)pQueryInfo->pCompiledQuery );

	LEAVE( "" );
}

/* ================================================================= */
static void
gnc_sql_init_object_handlers( void )
{
    gnc_sql_init_book_handler();
    gnc_sql_init_commodity_handler();
    gnc_sql_init_account_handler();
    gnc_sql_init_budget_handler();
    gnc_sql_init_price_handler();
    gnc_sql_init_transaction_handler();
    gnc_sql_init_slots_handler();
	gnc_sql_init_recurrence_handler();
    gnc_sql_init_schedxaction_handler();
    gnc_sql_init_lot_handler();
}

/* ================================================================= */

G_MODULE_EXPORT void
qof_backend_module_init(void)
{
}

/* ========================== END OF FILE ===================== */
