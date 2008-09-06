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

#include <stdlib.h>
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

#include "gnc-gconf-utils.h"

#include "gnc-backend-sql.h"

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

static const gchar* convert_search_obj( QofIdType objType );
static void gnc_sql_init_object_handlers( void );
static void update_save_progress( GncSqlBackend* be );
static void register_standard_col_type_handlers( void );
static void reset_version_info( GncSqlBackend* be );
static GncSqlStatement* build_insert_statement( GncSqlBackend* be,
                        			const gchar* table_name,
                        			QofIdTypeConst obj_name, gpointer pObject,
                        			const GncSqlColumnTableEntry* table );
static GncSqlStatement* build_update_statement( GncSqlBackend* be,
                        			const gchar* table_name,
                        			QofIdTypeConst obj_name, gpointer pObject,
                        			const GncSqlColumnTableEntry* table );
static GncSqlStatement* build_delete_statement( GncSqlBackend* be,
                        			const gchar* table_name,
                        			QofIdTypeConst obj_name, gpointer pObject,
                        			const GncSqlColumnTableEntry* table );

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
	register_standard_col_type_handlers();
	gnc_sql_init_object_handlers();
}

/* ================================================================= */

static void
create_tables_cb( const gchar* type, gpointer data_p, gpointer be_p )
{
    GncSqlObjectBackend* pData = data_p;
    GncSqlBackend* be = be_p;

    g_return_if_fail( type != NULL && data_p != NULL && be_p != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );

    if( pData->create_tables != NULL ) {
        (pData->create_tables)( be );
    }
}

/* ================================================================= */

static const gchar* fixed_load_order[] =
{ GNC_ID_BOOK, GNC_ID_COMMODITY, GNC_ID_ACCOUNT, GNC_ID_LOT, NULL };

static void
initial_load_cb( const gchar* type, gpointer data_p, gpointer be_p )
{
    GncSqlObjectBackend* pData = data_p;
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
    GncSqlObjectBackend* pData;
	int i;
	Account* root;
	GError* error = NULL;

	g_return_if_fail( be != NULL );
	g_return_if_fail( book != NULL );

    ENTER( "be=%p, book=%p", be, book );

    g_assert( be->primary_book == NULL );
    be->primary_book = book;

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
        gnc_sql_save_account( be, QOF_INSTANCE(GNC_ACCOUNT(node->data)) );
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

    gnc_sql_save_transaction( be, QOF_INSTANCE(tx) );
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
		gnc_sql_save_schedxaction( be, QOF_INSTANCE( tmpSX ) );
    }
}

static void
write_cb( const gchar* type, gpointer data_p, gpointer be_p )
{
    GncSqlObjectBackend* pData = data_p;
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

	reset_version_info( be );

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
	gnc_sql_save_book( be, QOF_INSTANCE(book) );
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
gnc_sql_begin_edit( GncSqlBackend *be, QofInstance *inst )
{
	g_return_if_fail( be != NULL );
	g_return_if_fail( inst != NULL );
}

void
gnc_sql_rollback_edit( GncSqlBackend *be, QofInstance *inst )
{
	g_return_if_fail( be != NULL );
	g_return_if_fail( inst != NULL );
}

static void
commit_cb( const gchar* type, gpointer data_p, gpointer be_data_p )
{
    GncSqlObjectBackend* pData = data_p;
    sql_backend* be_data = be_data_p;

    g_return_if_fail( type != NULL && pData != NULL && be_data != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );

    /* If this has already been handled, or is not the correct handler, return */
    if( strcmp( pData->type_name, be_data->inst->e_type ) != 0 ) return;
    if( be_data->ok ) return;

    if( pData->commit != NULL ) {
        (pData->commit)( be_data->be, be_data->inst );
        be_data->ok = TRUE;
    }
}

/* Commit_edit handler - find the correct backend handler for this object
 * type and call its commit handler
 */
void
gnc_sql_commit_edit( GncSqlBackend *be, QofInstance *inst )
{
    sql_backend be_data;
	GError* error;
	gboolean status;
	gboolean is_dirty;
	gboolean is_destroying;
	gboolean is_infant;

	g_return_if_fail( be != NULL );
	g_return_if_fail( inst != NULL );

    /* During initial load where objects are being created, don't commit
    anything, but do mark the object as clean. */
    if( be->loading ) {
		qof_instance_mark_clean( inst );
	    return;
	}

	// The engine has a PriceDB object but it isn't in the database
	if( strcmp( inst->e_type, "PriceDB" ) == 0 ) {
    	qof_instance_mark_clean( inst );
    	qof_book_mark_saved( be->primary_book );
		return;
	}

    ENTER( " " );

	is_dirty = qof_instance_get_dirty_flag( inst );
	is_destroying = qof_instance_get_destroying( inst );
	is_infant = qof_instance_get_infant( inst );

    DEBUG( "%s dirty = %d, do_free = %d, infant = %d\n",
             (inst->e_type ? inst->e_type : "(null)"),
             is_dirty, is_destroying, is_infant );

    if( !is_dirty && !is_destroying ) {
		LEAVE( "!dirty OR !destroying" );
		return;
	}

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
		LEAVE( "Rolled back" );
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
    GncSqlObjectBackend* pData = data_p;
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
    GncSqlObjectBackend* pData = data_p;
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
    GncSqlObjectBackend* pData = data_p;
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

static void register_table_version( const GncSqlBackend* be, const gchar* table_name, gint version );
static gint get_table_version( const GncSqlBackend* be, const gchar* table_name );

/* ================================================================= */
static gint64
get_integer_value( const GValue* value )
{
	g_return_val_if_fail( value != NULL, 0 );

	if( G_VALUE_HOLDS_INT(value) ) {
		return g_value_get_int( value );
	} else if( G_VALUE_HOLDS_UINT(value) ) {
		return g_value_get_uint( value );
	} else if( G_VALUE_HOLDS_LONG(value) ) {
		return g_value_get_long( value );
	} else if( G_VALUE_HOLDS_ULONG(value) ) {
		return g_value_get_ulong( value );
	} else if( G_VALUE_HOLDS_INT64(value) ) {
		return g_value_get_int64( value );
	} else if( G_VALUE_HOLDS_UINT64(value) ) {
		return g_value_get_uint64( value );
	} else {
		PWARN( "Unknown type: %s", G_VALUE_TYPE_NAME( value ) );
	}

	return 0;
}

/* ----------------------------------------------------------------- */
static gpointer
get_autoinc_id( gpointer pObject, const QofParam* param )
{
    // Just need a 0 to force a new recurrence id
    return (gpointer)0;
}

static void
set_autoinc_id( gpointer pObject, gpointer pValue )
{
    // Nowhere to put the ID
}

QofAccessFunc
gnc_sql_get_getter( QofIdTypeConst obj_name, const GncSqlColumnTableEntry* table_row )
{
    QofAccessFunc getter;

	g_return_val_if_fail( obj_name != NULL, NULL );
	g_return_val_if_fail( table_row != NULL, NULL );

	if( (table_row->flags & COL_AUTOINC) != 0 ) {
		getter = get_autoinc_id;
    } else if( table_row->qof_param_name != NULL ) {
        getter = qof_class_get_parameter_getter( obj_name,
                                                table_row->qof_param_name );
    } else {
        getter = table_row->getter;
    }

    return getter;
}

/* ----------------------------------------------------------------- */
void
gnc_sql_add_colname_to_list( const GncSqlColumnTableEntry* table_row, GList** pList )
{
	(*pList) = g_list_append( (*pList), g_strdup( table_row->col_name ) );
}

/* ----------------------------------------------------------------- */
void
gnc_sql_add_subtable_colnames_to_list( const GncSqlColumnTableEntry* table_row, const GncSqlColumnTableEntry* subtable,
								GList** pList )
{
	const GncSqlColumnTableEntry* subtable_row;
	gchar* buf;

	for( subtable_row = subtable; subtable_row->col_name != NULL; subtable_row++ ) {
		buf = g_strdup_printf( "%s_%s", table_row->col_name, subtable_row->col_name );
		(*pList) = g_list_append( (*pList), buf );
	}
}

static GncSqlColumnInfo*
create_column_info( const GncSqlColumnTableEntry* table_row, const gchar* type,
							gint size )
{
	GncSqlColumnInfo* info;

	info = g_new0( GncSqlColumnInfo, 1 );
	info->name = table_row->col_name;
	info->type_name = type;
	info->size = size;
	info->is_primary_key = (table_row->flags & COL_PKEY) ? TRUE : FALSE;
	info->null_allowed = (table_row->flags & COL_NNUL) ? FALSE : TRUE;

	return info;
}

/* ----------------------------------------------------------------- */
static void
load_string( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    const gchar* s;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if( val == NULL ) {
        s = NULL;
    } else {
        s = g_value_get_string( val );
    }
    if( table_row->gobj_param_name != NULL ) {
		g_object_set( pObject, table_row->gobj_param_name, s, NULL );
    } else {
		(*setter)( pObject, (const gpointer)s );
    }
}

static void
add_string_col_info_to_list( const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,
								GList** pList )
{
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = create_column_info( table_row,
					gnc_sql_connection_get_column_type_name( be->conn,
											G_TYPE_STRING, table_row->size ),
				    table_row->size );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_string_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    QofAccessFunc getter;
    gchar* s;
	GValue* value = g_new0( GValue, 1 );
	gchar* buf;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

    memset( value, 0, sizeof( GValue ) );
	if( table_row->gobj_param_name != NULL ) {
		g_object_get( pObject, table_row->gobj_param_name, &s, NULL );
	} else {
    	getter = gnc_sql_get_getter( obj_name, table_row );
    	s = (gchar*)(*getter)( pObject, NULL );
	}
	g_value_init( value, G_TYPE_STRING );
    if( s ) {
        g_value_set_string( value, s );
	}

	(*pList) = g_slist_append( (*pList), value );
}

static GncSqlColumnTypeHandler string_handler
    = { load_string,
		add_string_col_info_to_list,
		gnc_sql_add_colname_to_list,
        add_gvalue_string_to_slist };
/* ----------------------------------------------------------------- */
typedef gint (*IntAccessFunc)( const gpointer );
typedef void (*IntSetterFunc)( const gpointer, gint );

static void
load_int( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    gint int_value;
	IntSetterFunc i_setter;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if( val == NULL ) {
        int_value = 0;
    } else {
        int_value = get_integer_value( val );
    }
    if( table_row->gobj_param_name != NULL ) {
		g_object_set( pObject, table_row->gobj_param_name, int_value, NULL );
    } else {
		i_setter = (IntSetterFunc)setter;
    	(*i_setter)( pObject, int_value );
    }
}

static void
add_int_col_info_to_list( const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,
								GList** pList )
{
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = create_column_info( table_row,
					gnc_sql_connection_get_column_type_name( be->conn,
											G_TYPE_INT, table_row->size ),
				    0 );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_int_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    gint int_value;
    IntAccessFunc i_getter;
	GValue* value;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	value = g_new0( GValue, 1 );
    g_value_init( value, G_TYPE_INT );

	if( table_row->gobj_param_name != NULL ) {
		g_object_get_property( pObject, table_row->gobj_param_name, value );
	} else {
    	i_getter = (IntAccessFunc)gnc_sql_get_getter( obj_name, table_row );
    	int_value = (*i_getter)( pObject );
    	g_value_set_int( value, int_value );
	}

	(*pList) = g_slist_append( (*pList), value );
}

static GncSqlColumnTypeHandler int_handler
	= { load_int,
		add_int_col_info_to_list,
		gnc_sql_add_colname_to_list,
		add_gvalue_int_to_slist };
/* ----------------------------------------------------------------- */
typedef gboolean (*BooleanAccessFunc)( const gpointer );
typedef void (*BooleanSetterFunc)( const gpointer, gboolean );

static void
load_boolean( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    gint int_value;
	BooleanSetterFunc b_setter;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if( val == NULL ) {
        int_value = 0;
    } else {
        int_value = g_value_get_int( val );
    }
    if( table_row->gobj_param_name != NULL ) {
		g_object_set( pObject, table_row->gobj_param_name, int_value, NULL );
    } else {
		b_setter = (BooleanSetterFunc)setter;
    	(*b_setter)( pObject, int_value ? TRUE : FALSE );
    }
}

static void
add_boolean_col_info_to_list( const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,
								GList** pList )
{
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = create_column_info( table_row,
					gnc_sql_connection_get_column_type_name( be->conn,
											G_TYPE_INT, table_row->size ),
				    0 );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_boolean_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    gint int_value;
    BooleanAccessFunc b_getter;
	GValue* value;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

    value = g_new0( GValue, 1 );

	if( table_row->gobj_param_name != NULL ) {
		g_object_get( pObject, table_row->gobj_param_name, &int_value, NULL );
	} else {
    	b_getter = (BooleanAccessFunc)gnc_sql_get_getter( obj_name, table_row );
    	int_value = ((*b_getter)( pObject )) ? 1 : 0;
	}
    g_value_init( value, G_TYPE_INT );
    g_value_set_int( value, int_value );

	(*pList) = g_slist_append( (*pList), value );
}

static GncSqlColumnTypeHandler boolean_handler
	= { load_boolean,
		add_boolean_col_info_to_list,
		gnc_sql_add_colname_to_list,
		add_gvalue_boolean_to_slist };
/* ----------------------------------------------------------------- */
typedef gint64 (*Int64AccessFunc)( const gpointer );
typedef void (*Int64SetterFunc)( const gpointer, gint64 );

static void
load_int64( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    gint64 i64_value = 0;
	Int64SetterFunc i64_setter = (Int64SetterFunc)setter;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( setter != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if( val != NULL ) {
        i64_value = get_integer_value( val );
    }
    (*i64_setter)( pObject, i64_value );
}

static void
add_int64_col_info_to_list( const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,
								GList** pList )
{
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = create_column_info( table_row,
					gnc_sql_connection_get_column_type_name( be->conn,
											G_TYPE_INT64, table_row->size ),
				    0 );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_int64_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
				const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    gint64 i64_value;
    Int64AccessFunc getter;
	GValue* value;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

    value = g_new0( GValue, 1 );
	if( table_row->gobj_param_name != NULL ) {
		g_object_get( pObject, table_row->gobj_param_name, &i64_value, NULL );
	} else {
    	getter = (Int64AccessFunc)gnc_sql_get_getter( obj_name, table_row );
    	i64_value = (*getter)( pObject );
	}
    g_value_init( value, G_TYPE_INT64 );
    g_value_set_int64( value, i64_value );

	(*pList) = g_slist_append( (*pList), value );
}

static GncSqlColumnTypeHandler int64_handler
	= { load_int64,
		add_int64_col_info_to_list,
		gnc_sql_add_colname_to_list,
		add_gvalue_int64_to_slist };
/* ----------------------------------------------------------------- */

static void
load_double( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    gdouble d_value;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if( val == NULL ) {
        (*setter)( pObject, (gpointer)NULL );
    } else {
		if( G_VALUE_HOLDS(val, G_TYPE_INT) ) {
			d_value = g_value_get_int( val );
		} else {
			d_value = g_value_get_double( val );
		}
        (*setter)( pObject, (gpointer)&d_value );
    }
}

static void
add_double_col_info_to_list( const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,
								GList** pList )
{
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = create_column_info( table_row,
					gnc_sql_connection_get_column_type_name( be->conn,
											G_TYPE_DOUBLE, table_row->size ),
				    0 );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_double_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
						const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    QofAccessFunc getter;
    gdouble* pDouble;
    gdouble d_value;
	GValue* value;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

	value = g_new0( GValue, 1 );
    getter = gnc_sql_get_getter( obj_name, table_row );
    pDouble = (*getter)( pObject, NULL );
    if( pDouble != NULL ) {
        d_value = *pDouble;
        g_value_init( value, G_TYPE_DOUBLE );
        g_value_set_double( value, d_value );
    } else {
        g_value_init( value, G_TYPE_DOUBLE );
		g_value_set_double( value, 0 );
	}

	(*pList) = g_slist_append( (*pList), value );
}

static GncSqlColumnTypeHandler double_handler
	= { load_double,
		add_double_col_info_to_list,
		gnc_sql_add_colname_to_list,
		add_gvalue_double_to_slist };
/* ----------------------------------------------------------------- */

static void
load_guid( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    GUID guid;
    const GUID* pGuid;

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
    if( table_row->gobj_param_name != NULL ) {
		g_object_set( pObject, table_row->gobj_param_name, pGuid, NULL );
    } else {
		(*setter)( pObject, (const gpointer)pGuid );
    }
}

static void
add_guid_col_info_to_list( const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,
								GList** pList )
{
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = create_column_info( table_row, "CHAR", GUID_ENCODING_LENGTH );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_guid_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
					const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    QofAccessFunc getter;
    const GUID* guid;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
	GValue* value;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    value = g_new0( GValue, 1 );
	if( table_row->gobj_param_name != NULL ) {
		g_object_get( pObject, table_row->gobj_param_name, &guid, NULL );
	} else {
    	getter = gnc_sql_get_getter( obj_name, table_row );
    	guid = (*getter)( pObject, NULL );
	}
    g_value_init( value, G_TYPE_STRING );
    if( guid != NULL ) {
        (void)guid_to_string_buff( guid, guid_buf );
        g_value_set_string( value, guid_buf );
	}

	(*pList) = g_slist_append( (*pList), value );
}

static GncSqlColumnTypeHandler guid_handler
	= { load_guid,
		add_guid_col_info_to_list,
		gnc_sql_add_colname_to_list,
        add_gvalue_guid_to_slist };
/* ----------------------------------------------------------------- */

void
gnc_sql_add_gvalue_objectref_guid_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
						const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    QofAccessFunc getter;
    const GUID* guid = NULL;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
	QofInstance* inst;
	GValue* value;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

	value = g_new0( GValue, 1 );
	if( table_row->gobj_param_name != NULL ) {
		g_object_get( pObject, table_row->gobj_param_name, &inst, NULL );
	} else {
    	getter = gnc_sql_get_getter( obj_name, table_row );
    	inst = (*getter)( pObject, NULL );
	}
	if( inst != NULL ) {
		guid = qof_instance_get_guid( inst );
	}
    g_value_init( value, G_TYPE_STRING );
    if( guid != NULL ) {
        (void)guid_to_string_buff( guid, guid_buf );
        g_value_set_string( value, guid_buf );
	}

	(*pList) = g_slist_append( (*pList), value );
}

void
gnc_sql_add_objectref_guid_col_info_to_list( const GncSqlBackend* be,
								const GncSqlColumnTableEntry* table_row,
								GList** pList )
{
	add_guid_col_info_to_list( be, table_row, pList );
}

/* ----------------------------------------------------------------- */
typedef Timespec (*TimespecAccessFunc)( const gpointer );
typedef void (*TimespecSetterFunc)( const gpointer, Timespec );

#define TIMESPEC_STR_FORMAT "%04d%02d%02d%02d%02d%02d"
#define TIMESPEC_COL_SIZE (4+2+2+2+2+2)

static void
load_timespec( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    GDate* date;
    Timespec ts = {0, 0};
	TimespecSetterFunc ts_setter;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

	ts_setter = (TimespecSetterFunc)setter;
    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if( val == NULL ) {
        (*ts_setter)( pObject, ts );
    } else {
		if( G_VALUE_HOLDS_STRING( val ) ) {
			const gchar* s = g_value_get_string( val );
			gchar* buf;
			buf = g_strdup_printf( "%c%c%c%c-%c%c-%c%c %c%c:%c%c:%c%c",
									s[0], s[1], s[2], s[3],
									s[4], s[5],
									s[6], s[7],
									s[8], s[9],
									s[10], s[11],
									s[12], s[13] );
		    ts = gnc_iso8601_to_timespec_gmt( buf );
			(*ts_setter)( pObject, ts );
			g_free( buf );

		} else {
			PWARN( "Unknown timespec type: %s", G_VALUE_TYPE_NAME( val ) );
        }
    }
}

static void
add_timespec_col_info_to_list( const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,
								GList** pList )
{
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = create_column_info( table_row, "CHAR", TIMESPEC_COL_SIZE );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_timespec_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    TimespecAccessFunc ts_getter;
    Timespec ts;
	gchar* datebuf;
	time_t time;
	struct tm* tm;
	gint year;
	GValue* value;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

    value = g_new0( GValue, 1 );
    ts_getter = (TimespecAccessFunc)gnc_sql_get_getter( obj_name, table_row );
    ts = (*ts_getter)( pObject );

	time = timespecToTime_t( ts );
	tm = gmtime( &time );	

	if( tm->tm_year < 60 ) year = tm->tm_year + 2000;
	else year = tm->tm_year + 1900;

	datebuf = g_strdup_printf( TIMESPEC_STR_FORMAT,
					year, tm->tm_mon+1, tm->tm_mday, tm->tm_hour, tm->tm_min, tm->tm_sec );
    g_value_init( value, G_TYPE_STRING );
	g_value_take_string( value, datebuf );

	(*pList) = g_slist_append( (*pList), value );
}

static GncSqlColumnTypeHandler timespec_handler
	= { load_timespec,
		add_timespec_col_info_to_list,
		gnc_sql_add_colname_to_list,
		add_gvalue_timespec_to_slist };
/* ----------------------------------------------------------------- */
#define DATE_COL_SIZE 8

static void
load_date( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    GDate* date;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if( val == NULL ) {
		date = g_date_new_dmy( 1, 1, 1970 );
        (*setter)( pObject, date );
		g_date_free( date );
    } else {
		if( G_VALUE_HOLDS_STRING( val ) ) {
			// Format of date is YYYYMMDD
			const gchar* s = g_value_get_string( val );
			gchar buf[5];
			guint year, month, day;

			strncpy( buf, &s[0], 4 );
			buf[4] = '\0';
			year = atoi( buf );
			strncpy( buf, &s[4], 2 );
			buf[2] = '\0';
			month = atoi( buf );
			day = atoi( &s[6] );

			if( year != 0 || month != 0 || day != 0 ) {
				date = g_date_new_dmy( day, month, year );
				(*setter)( pObject, date );
				g_date_free( date );
			}
		} else {
			PWARN( "Unknown date type: %s", G_VALUE_TYPE_NAME( val ) );
        }
    }
}

static void
add_date_col_info_to_list( const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,
								GList** pList )
{
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = create_column_info( table_row, "CHAR", DATE_COL_SIZE );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_date_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
				const gpointer pObject,
                const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    GDate* date;
    QofAccessFunc getter;
	gchar* buf;
	GValue* value;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    value = g_new0( GValue, 1 );
    getter = gnc_sql_get_getter( obj_name, table_row );
    date = (GDate*)(*getter)( pObject, NULL );
	buf = g_strdup_printf( "%04d%02d%02d",
					g_date_get_year( date ), g_date_get_month( date ), g_date_get_day( date ) );
    g_value_init( value, G_TYPE_STRING );
    g_value_take_string( value, buf );

	(*pList) = g_slist_append( (*pList), value );
}

static GncSqlColumnTypeHandler date_handler
	= { load_date,
		add_date_col_info_to_list,
		gnc_sql_add_colname_to_list,
		add_gvalue_date_to_slist };
/* ----------------------------------------------------------------- */
typedef gnc_numeric (*NumericGetterFunc)( const gpointer );
typedef void (*NumericSetterFunc)( gpointer, gnc_numeric );

static const GncSqlColumnTableEntry numeric_col_table[] =
{
    { "num",    CT_INT64, 0, COL_NNUL, "guid" },
    { "denom",  CT_INT64, 0, COL_NNUL, "guid" },
	{ NULL }
};

static void
load_numeric( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    gchar* buf;
    gint64 num, denom;
    gnc_numeric n;
    gboolean isNull = FALSE;
	NumericSetterFunc n_setter = (NumericSetterFunc)setter;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    buf = g_strdup_printf( "%s_num", table_row->col_name );
    val = gnc_sql_row_get_value_at_col_name( row, buf );
    g_free( buf );
    if( val == NULL ) {
        isNull = TRUE;
        num = 0;
    } else {
        num = get_integer_value( val );
    }
    buf = g_strdup_printf( "%s_denom", table_row->col_name );
    val = gnc_sql_row_get_value_at_col_name( row, buf );
    g_free( buf );
    if( val == NULL ) {
        isNull = TRUE;
        denom = 1;
    } else {
        denom = get_integer_value( val );
    }
    n = gnc_numeric_create( num, denom );
    if( !isNull ) {
        (*n_setter)( pObject, n );
    }
}

static void
add_numeric_col_info_to_list( const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,
								GList** pList )
{
	GncSqlColumnInfo* info;
    gchar* buf;
	const GncSqlColumnTableEntry* subtable_row;
	const gchar* type;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	for( subtable_row = numeric_col_table; subtable_row->col_name != NULL; subtable_row++ ) {
    	buf = g_strdup_printf( "%s_%s", table_row->col_name, subtable_row->col_name );
		info = g_new0( GncSqlColumnInfo, 1 );
		info->name = buf;
		info->type_name = gnc_sql_connection_get_column_type_name( be->conn,
											G_TYPE_INT64, table_row->size );
		info->is_primary_key = (table_row->flags & COL_PKEY) ? TRUE : FALSE;
		info->null_allowed = (table_row->flags & COL_NNUL) ? FALSE : TRUE;
		*pList = g_list_append( *pList, info );
	}
}

static void
add_numeric_colname_to_list( const GncSqlColumnTableEntry* table_row, GList** pList )
{
	gnc_sql_add_subtable_colnames_to_list( table_row, numeric_col_table, pList );
}

static void
add_gvalue_numeric_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
					const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    NumericGetterFunc getter;
    gnc_numeric n;
    GValue* num_value;
    GValue* denom_value;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

//	if( table_row->gobj_param_name != NULL ) {
//		g_object_get( pObject, table_row->gobj_param_name, &s, NULL );
//	} else {
    	getter = (NumericGetterFunc)gnc_sql_get_getter( obj_name, table_row );
    	n = (*getter)( pObject );
//	}

    num_value = g_new0( GValue, 1 );
    g_value_init( num_value, G_TYPE_INT64 );
    g_value_set_int64( num_value, gnc_numeric_num( n ) );
    denom_value = g_new0( GValue, 1 );
    g_value_init( denom_value, G_TYPE_INT64 );
    g_value_set_int64( denom_value, gnc_numeric_denom( n ) );

	(*pList) = g_slist_append( (*pList), num_value );
	(*pList) = g_slist_append( (*pList), denom_value );
}

static GncSqlColumnTypeHandler numeric_handler
	= { load_numeric,
		add_numeric_col_info_to_list,
		add_numeric_colname_to_list,
		add_gvalue_numeric_to_slist };
/* ================================================================= */

static GHashTable* g_columnTypeHash = NULL;

void
gnc_sql_register_col_type_handler( const gchar* colType, const GncSqlColumnTypeHandler* handler )
{
	g_return_if_fail( colType != NULL );
	g_return_if_fail( handler != NULL );

	if( g_columnTypeHash == NULL ) {
		g_columnTypeHash = g_hash_table_new( g_str_hash, g_str_equal );
	}

	g_hash_table_insert( g_columnTypeHash, (gpointer)colType, (gpointer)handler );
	DEBUG( "Col type %s registered\n", colType );
}

static GncSqlColumnTypeHandler*
get_handler( const GncSqlColumnTableEntry* table_row )
{
    GncSqlColumnTypeHandler* pHandler;

	g_return_val_if_fail( table_row != NULL, NULL );
	g_return_val_if_fail( table_row->col_type != NULL, NULL );

	pHandler = g_hash_table_lookup( g_columnTypeHash, table_row->col_type );
	if( pHandler == NULL ) {
        g_assert( FALSE );
    }

    return pHandler;
}

static void
register_standard_col_type_handlers( void )
{
	gnc_sql_register_col_type_handler( CT_STRING, &string_handler );
    gnc_sql_register_col_type_handler( CT_BOOLEAN, &boolean_handler );
    gnc_sql_register_col_type_handler( CT_INT, &int_handler );
    gnc_sql_register_col_type_handler( CT_INT64, &int64_handler );
    gnc_sql_register_col_type_handler( CT_DOUBLE, &double_handler );
    gnc_sql_register_col_type_handler( CT_GUID, &guid_handler );
    gnc_sql_register_col_type_handler( CT_TIMESPEC, &timespec_handler );
    gnc_sql_register_col_type_handler( CT_GDATE, &date_handler );
    gnc_sql_register_col_type_handler( CT_NUMERIC, &numeric_handler );
}

void 
_retrieve_guid_( gpointer pObject, gpointer pValue )
{
    GUID* pGuid = (GUID*)pObject;
    GUID* guid = (GUID*)pValue;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( pValue != NULL );

	memcpy( pGuid, guid, sizeof( GUID ) );
}


// Table to retrieve just the guid
static GncSqlColumnTableEntry guid_table[] =
{
    { "guid", CT_GUID, 0, 0, NULL, NULL, NULL, _retrieve_guid_ },
    { NULL }
};

const GUID*
gnc_sql_load_guid( const GncSqlBackend* be, GncSqlRow* row )
{
	static GUID guid;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

    gnc_sql_load_object( be, row, NULL, &guid, guid_table );

    return &guid;
}

// Table to retrieve just the guid
static GncSqlColumnTableEntry tx_guid_table[] =
{
    { "tx_guid", CT_GUID, 0, 0, NULL, NULL, NULL, _retrieve_guid_ },
    { NULL }
};

const GUID*
gnc_sql_load_tx_guid( const GncSqlBackend* be, GncSqlRow* row )
{
    static GUID guid;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

    gnc_sql_load_object( be, row, NULL, &guid, tx_guid_table );

    return &guid;
}

void
gnc_sql_load_object( const GncSqlBackend* be, GncSqlRow* row,
                    QofIdTypeConst obj_name, gpointer pObject,
                    const GncSqlColumnTableEntry* table_row )
{
    int col;
    QofSetterFunc setter;
    GncSqlColumnTypeHandler* pHandler;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    for( col = 0; table_row[col].col_name != NULL; col++ ) {
		if( (table_row[col].flags & COL_AUTOINC) != 0 ) {
			setter = set_autoinc_id;
        } else if( table_row[col].qof_param_name != NULL ) {
            setter = qof_class_get_parameter_setter( obj_name,
                                                table_row[col].qof_param_name );
        } else {
            setter = table_row[col].setter;
        }
        pHandler = get_handler( &table_row[col] );
        pHandler->load_fn( be, row, setter, pObject, &table_row[col] );
    }
}

/* ================================================================= */
GncSqlStatement*
gnc_sql_create_select_statement( const GncSqlBackend* be, const gchar* table_name )
{
	gchar* sql;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( table_name != NULL, NULL );

	sql = g_strdup_printf( "SELECT * FROM %s", table_name );
	return gnc_sql_create_statement_from_sql( be, sql );
}

static GncSqlStatement*
create_single_col_select_statement( const GncSqlBackend* be,
							const gchar* table_name,
							const GncSqlColumnTableEntry* table_row )
{
	gchar* sql;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( table_name != NULL, NULL );
	g_return_val_if_fail( table_row != NULL, NULL );

	sql = g_strdup_printf( "SELECT %s FROM %s", table_row->col_name, table_name );
	return gnc_sql_create_statement_from_sql( be, sql );
}

/* ================================================================= */

GncSqlResult*
gnc_sql_execute_select_statement( GncSqlBackend* be, GncSqlStatement* stmt )
{
    GError* error = NULL;
    GncSqlResult* result;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( stmt != NULL, NULL );

    result = gnc_sql_connection_execute_select_statement( be->conn, stmt );
    if( error != NULL ) {
        PERR( "SQL error: %s\n%s\n", gnc_sql_statement_to_sql( stmt ), error->message );
		qof_backend_set_error( &be->be, ERR_BACKEND_SERVER_ERR );
    }

    return result;
}

GncSqlStatement*
gnc_sql_create_statement_from_sql( const GncSqlBackend* be, gchar* sql )
{
    GError* error = NULL;
	GncSqlStatement* stmt;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( sql != NULL, NULL );

	stmt = gnc_sql_connection_create_statement_from_sql( be->conn, sql );
    if( stmt == NULL ) {
        PERR( "SQL error: %s\n%s\n", sql, error->message );
    }

	return stmt;
}

GncSqlResult*
gnc_sql_execute_select_sql( const GncSqlBackend* be, gchar* sql )
{
	GncSqlStatement* stmt;
    GError* error = NULL;
	GncSqlResult* result = NULL;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( sql != NULL, NULL );

	stmt = gnc_sql_create_statement_from_sql( be, sql );
    if( stmt == NULL ) {
		return NULL;
    }
	result = gnc_sql_connection_execute_select_statement( be->conn, stmt );
    if( error != NULL ) {
        PERR( "SQL error: %s\n%s\n", sql, error->message );
    }

	return result;
}

static gint
execute_nonselect_sql( const GncSqlBackend* be, gchar* sql )
{
	GncSqlStatement* stmt;
	gint result;

	g_return_val_if_fail( be != NULL, 0 );
	g_return_val_if_fail( sql != NULL, 0 );

	stmt = gnc_sql_create_statement_from_sql( be, sql );
    if( stmt == NULL ) {
		return 0;
    }
	result = gnc_sql_connection_execute_nonselect_statement( be->conn, stmt );
	gnc_sql_statement_dispose( stmt );
	return result;
}

static int
execute_statement_get_count( GncSqlBackend* be, GncSqlStatement* stmt )
{
    GncSqlResult* result;
	int count = 0;

	g_return_val_if_fail( be != NULL, 0 );
	g_return_val_if_fail( stmt != NULL, 0 );

    result = gnc_sql_execute_select_statement( be, stmt );
    if( result != NULL ) {
        count = gnc_sql_result_get_num_rows( result );
		gnc_sql_result_dispose( result );
    }

    return count;
}

guint
gnc_sql_append_guid_list_to_sql( GString* sql, GList* list, guint maxCount )
{
	gchar guid_buf[GUID_ENCODING_LENGTH+1];
	gboolean first_guid = TRUE;
	guint count;

	g_return_val_if_fail( sql != NULL, 0 );

	if( list == NULL ) return 0;

	for( count = 0; list != NULL && count < maxCount; list = list->next, count++ ) {
		QofInstance* inst = QOF_INSTANCE(list->data);
    	guid_to_string_buff( qof_instance_get_guid( inst ), guid_buf );

		if( !first_guid ) {
			g_string_append( sql, "," );
		}
		g_string_append( sql, "'" );
		g_string_append( sql, guid_buf );
		g_string_append( sql, "'" );
		first_guid = FALSE;
    }

	return count;
}
/* ================================================================= */

gboolean
gnc_sql_object_is_it_in_db( GncSqlBackend* be, const gchar* table_name,
                    QofIdTypeConst obj_name, gpointer pObject,
                    const GncSqlColumnTableEntry* table )
{
    GncSqlStatement* sqlStmt;
    int count;
    GncSqlColumnTypeHandler* pHandler;
	GSList* list = NULL;

	g_return_val_if_fail( be != NULL, FALSE );
	g_return_val_if_fail( table_name != NULL, FALSE );
	g_return_val_if_fail( obj_name != NULL, FALSE );
	g_return_val_if_fail( pObject != NULL, FALSE );
	g_return_val_if_fail( table != NULL, FALSE );

    /* SELECT * FROM */
    sqlStmt = create_single_col_select_statement( be, table_name, table );

    /* WHERE */
    pHandler = get_handler( table );
	pHandler->add_gvalue_to_slist_fn( be, obj_name, pObject, table, &list );
	gnc_sql_statement_add_where_cond( sqlStmt, obj_name, pObject, &table[0], (GValue*)(list->data) );

    count = execute_statement_get_count( be, sqlStmt );
	gnc_sql_statement_dispose( sqlStmt );
    if( count == 0 ) {
        return FALSE;
    } else {
        return TRUE;
    }
}

gboolean
gnc_sql_do_db_operation( GncSqlBackend* be,
                        E_DB_OPERATION op,
                        const gchar* table_name,
                        QofIdTypeConst obj_name, gpointer pObject,
                        const GncSqlColumnTableEntry* table )
{
    GncSqlStatement* stmt;

	g_return_val_if_fail( be != NULL, FALSE );
	g_return_val_if_fail( table_name != NULL, FALSE );
	g_return_val_if_fail( obj_name != NULL, FALSE );
	g_return_val_if_fail( pObject != NULL, FALSE );
	g_return_val_if_fail( table != NULL, FALSE );

	if( op == OP_DB_INSERT ) {
        stmt = build_insert_statement( be, table_name, obj_name, pObject, table );
    } else if( op == OP_DB_UPDATE ) {
        stmt = build_update_statement( be, table_name, obj_name, pObject, table );
    } else if( op == OP_DB_DELETE ) {
        stmt = build_delete_statement( be, table_name, obj_name, pObject, table );
    } else {
        g_assert( FALSE );
    }
    if( stmt != NULL ) {
		gnc_sql_connection_execute_nonselect_statement( be->conn, stmt );
		gnc_sql_statement_dispose( stmt );

        return TRUE;
    } else {
        return FALSE;
    }
}

static GSList*
create_gslist_from_values( GncSqlBackend* be,
                            QofIdTypeConst obj_name, gpointer pObject,
                            const GncSqlColumnTableEntry* table )
{
	GSList* list = NULL;
	GncSqlColumnTypeHandler* pHandler;
	const GncSqlColumnTableEntry* table_row;

    for( table_row = table; table_row->col_name != NULL; table_row++ ) {
		if(( table_row->flags & COL_AUTOINC ) == 0 ) {
    		pHandler = get_handler( table_row );
			pHandler->add_gvalue_to_slist_fn( be, obj_name, pObject, table_row, &list );
		}
    }

	return list;
}

gchar*
gnc_sql_get_sql_value( const GncSqlConnection* conn, const GValue* value )
{
	if( value != NULL && G_IS_VALUE( value ) ) {
		GType type = G_VALUE_TYPE(value);

		if( G_VALUE_HOLDS_STRING(value) ) {
			if( g_value_get_string( value ) != NULL ) {
				gchar* before_str;
				gchar* after_str;
				before_str = g_value_dup_string( value );
				after_str = gnc_sql_connection_quote_string( conn, before_str );
				g_free( before_str );
				return after_str;
			} else {
				return g_strdup( "NULL" );
			}
		} else if( type == G_TYPE_INT64 ) {
			return g_strdup_printf( "%" G_GINT64_FORMAT, g_value_get_int64( value ) );

		} else if( type == G_TYPE_INT ) {
			return g_strdup_printf( "%d", g_value_get_int( value ) );

		} else if( type == G_TYPE_DOUBLE ) {
			return g_strdup_printf( "%g", g_value_get_double( value ) );

		} else if( g_value_type_transformable( type, G_TYPE_STRING ) ) {
			GValue* string;
			gchar* str;
			
			string = g_value_init( g_new0( GValue, 1 ), G_TYPE_STRING );
			g_value_transform( value, string );
			str = g_value_dup_string( string );
			g_value_unset( string );
			g_free( string );
			PWARN( "using g_value_transform(), gtype = '%s'\n", g_type_name( type ) );
			return str;
		} else {
			PWARN( "not transformable, gtype = '%s'\n", g_type_name( type ) );
			return "$$$";
		}
	} else {
		PWARN( "value is NULL or not G_IS_VALUE()\n" );
		return "";
	}
}

static GncSqlStatement*
build_insert_statement( GncSqlBackend* be,
                        const gchar* table_name,
                        QofIdTypeConst obj_name, gpointer pObject,
                        const GncSqlColumnTableEntry* table )
{
	GncSqlStatement* stmt;
	GString* sql;
	GSList* values;
	GSList* node;
	gchar* sqlbuf;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( table_name != NULL, NULL );
	g_return_val_if_fail( obj_name != NULL, NULL );
	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( table != NULL, NULL );

	sqlbuf = g_strdup_printf( "INSERT INTO %s VALUES(", table_name );
	sql = g_string_new( sqlbuf );
	g_free( sqlbuf );
	values = create_gslist_from_values( be, obj_name, pObject, table );
	for( node = values; node != NULL; node = node->next ) {
		GValue* value = (GValue*)node->data;
		gchar* value_str;
		if( node != values ) {
			g_string_append( sql, "," );
		}
		value_str = gnc_sql_get_sql_value( be->conn, value );
		g_string_append( sql, value_str );
		g_free( value_str );
		g_value_reset( value );
		g_free( value );
	}
	g_slist_free( values );
	g_string_append( sql, ")" );

	stmt = gnc_sql_connection_create_statement_from_sql( be->conn, sql->str );
	return stmt;
}

static GncSqlStatement*
build_update_statement( GncSqlBackend* be,
                        const gchar* table_name,
                        QofIdTypeConst obj_name, gpointer pObject,
                        const GncSqlColumnTableEntry* table )
{
	GncSqlStatement* stmt;
	GString* sql;
	GSList* values;
	GList* colnames = NULL;
	GSList* value;
	GList* colname;
	gboolean firstCol;
	const GncSqlColumnTableEntry* table_row = table;
	gchar* sqlbuf;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( table_name != NULL, NULL );
	g_return_val_if_fail( obj_name != NULL, NULL );
	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( table != NULL, NULL );

    // Get all col names and all values
	for( ; table_row->col_name != NULL; table_row++ ) {
    	GncSqlColumnTypeHandler* pHandler;

		// Add col names to the list
		pHandler = get_handler( table_row );
		pHandler->add_colname_to_list_fn( table_row, &colnames );
	}
	values = create_gslist_from_values( be, obj_name, pObject, table );

	// Create the SQL statement
	sqlbuf = g_strdup_printf( "UPDATE %s SET ", table_name );
	sql = g_string_new( sqlbuf );
	g_free( sqlbuf );

	firstCol = TRUE;
	for( colname = colnames->next, value = values->next;
					colname != NULL && value != NULL;
					colname = colname->next, value = value->next ) {
		gchar* value_str;
		if( !firstCol ) {
			g_string_append( sql, "," );
		}
		g_string_append( sql, (gchar*)colname->data );
		g_string_append( sql, "=" );
		value_str = gnc_sql_get_sql_value( be->conn, (GValue*)(value->data) );
		g_string_append( sql, value_str );
		g_free( value_str );
		firstCol = FALSE;
	}
	g_list_free( colnames );
	if( value != NULL || colname != NULL ) {
		PERR( "Mismatch in number of column names and values" );
	}

	stmt = gnc_sql_connection_create_statement_from_sql( be->conn, sql->str );
	gnc_sql_statement_add_where_cond( stmt, obj_name, pObject, &table[0], (GValue*)(values->data) );
	g_slist_free( values );

	return stmt;
}

static GncSqlStatement*
build_delete_statement( GncSqlBackend* be,
                        const gchar* table_name,
                        QofIdTypeConst obj_name, gpointer pObject,
                        const GncSqlColumnTableEntry* table )
{
	GncSqlStatement* stmt;
	GString* sql;
    GncSqlColumnTypeHandler* pHandler;
	GSList* list = NULL;
	gchar* sqlbuf;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( table_name != NULL, NULL );
	g_return_val_if_fail( obj_name != NULL, NULL );
	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( table != NULL, NULL );

	sqlbuf = g_strdup_printf( "DELETE FROM %s ", table_name );
	sql = g_string_new( sqlbuf );
	g_free( sqlbuf );
	stmt = gnc_sql_connection_create_statement_from_sql( be->conn, sql->str );

    /* WHERE */
    pHandler = get_handler( table );
	pHandler->add_gvalue_to_slist_fn( be, obj_name, pObject, table, &list );
	gnc_sql_statement_add_where_cond( stmt, obj_name, pObject, &table[0], (GValue*)(list->data) );

	return stmt;
}

/* ================================================================= */
void
gnc_sql_commit_standard_item( GncSqlBackend* be, QofInstance* inst, const gchar* tableName,
                        	QofIdTypeConst obj_name, const GncSqlColumnTableEntry* col_table )
{
	const GUID* guid;
	gboolean is_infant;
	gint op;

	is_infant = qof_instance_get_infant( inst );
	if( qof_instance_get_destroying( inst ) ) {
		op = OP_DB_DELETE;
	} else if( be->is_pristine_db || is_infant ) {
		op = OP_DB_INSERT;
	} else {
		op = OP_DB_UPDATE;
	}
    (void)gnc_sql_do_db_operation( be, op, tableName, obj_name, inst, col_table );

    // Now, commit any slots
    guid = qof_instance_get_guid( inst );
    if( !qof_instance_get_destroying(inst) ) {
        gnc_sql_slots_save( be, guid, is_infant, qof_instance_get_slots( inst ) );
    } else {
        gnc_sql_slots_delete( be, guid );
    }
}

/* ================================================================= */

static gboolean
create_table( const GncSqlBackend* be, const gchar* table_name,
				const GncSqlColumnTableEntry* col_table )
{
	GList* col_info_list = NULL;
    
	g_return_val_if_fail( be != NULL, FALSE );
	g_return_val_if_fail( table_name != NULL, FALSE );
	g_return_val_if_fail( col_table != NULL, FALSE );
    
    for( ; col_table->col_name != NULL; col_table++ ) {
        GncSqlColumnTypeHandler* pHandler;

        pHandler = get_handler( col_table );
        pHandler->add_col_info_to_list_fn( be, col_table, &col_info_list );
    }
	gnc_sql_connection_create_table( be->conn, table_name, col_info_list );
	return TRUE;
}

gboolean
gnc_sql_create_table( const GncSqlBackend* be, const gchar* table_name,
					gint table_version, const GncSqlColumnTableEntry* col_table )
{
	gboolean ok;

	ok = create_table( be, table_name, col_table );
	if( ok ) {
		register_table_version( be, table_name, table_version );
	}
	return ok;
}

void
gnc_sql_create_index( const GncSqlBackend* be, const gchar* index_name,
					const gchar* table_name,
                    const GncSqlColumnTableEntry* col_table )
{
    g_return_if_fail( be != NULL );
	g_return_if_fail( index_name != NULL );
	g_return_if_fail( table_name != NULL );
	g_return_if_fail( col_table != NULL );
    
	gnc_sql_connection_create_index( be->conn, index_name, table_name,
								col_table );
}

gint
gnc_sql_get_table_version( const GncSqlBackend* be, const gchar* table_name )
{
	g_return_val_if_fail( be != NULL, 0 );
	g_return_val_if_fail( table_name != NULL, 0 );

	/* If the db is pristine because it's being saved, the table does not
	 * exist.  This gets around a GDA-3 bug where deleting all tables and
	 * updating the meta-data leaves the meta-data still thinking 1 table
	 * exists.
	 */
	if( be->is_pristine_db ) {
		return 0;
	}

	return get_table_version( be, table_name );
	}

/* ================================================================= */
#define VERSION_TABLE_NAME "versions"
#define MAX_TABLE_NAME_LEN 50
#define TABLE_COL_NAME "table_name"
#define VERSION_COL_NAME "table_version"

static GncSqlColumnTableEntry version_table[] =
{
    { TABLE_COL_NAME,   CT_STRING, MAX_TABLE_NAME_LEN },
	{ VERSION_COL_NAME, CT_INT },
    { NULL }
};

/**
 * Sees if the version table exists, and if it does, loads the info into
 * the version hash table.  Otherwise, it creates an empty version table.
 *
 * @param be Backend struct
 */
void
gnc_sql_init_version_info( GncSqlBackend* be )
{
	g_return_if_fail( be != NULL );

	be->versions = g_hash_table_new_full( g_str_hash, g_str_equal, g_free, NULL );

	if( gnc_sql_connection_does_table_exist( be->conn, VERSION_TABLE_NAME ) ) {
		GncSqlResult* result;
		gchar* sql;

		sql = g_strdup_printf( "SELECT * FROM %s", VERSION_TABLE_NAME );
		result = gnc_sql_execute_select_sql( be, sql );
		if( result != NULL ) {
			const GValue* name;
			const GValue* version;
			GncSqlRow* row;

			row = gnc_sql_result_get_first_row( result );
			while( row != NULL ) {
    			name = gnc_sql_row_get_value_at_col_name( row, TABLE_COL_NAME );
				version = gnc_sql_row_get_value_at_col_name( row, VERSION_COL_NAME );
				g_hash_table_insert( be->versions,
									g_strdup( g_value_get_string( name ) ),
									GINT_TO_POINTER(g_value_get_int( version )) );
				row = gnc_sql_result_get_next_row( result );
			}
			gnc_sql_result_dispose( result );
		}
	} else {
		gboolean ok;

		ok = create_table( be, VERSION_TABLE_NAME, version_table );
	}
}

/**
 * Resets the version table information by removing all version table info.
 * It also recreates the version table in the db.
 *
 * @param be Backend struct
 */
static void
reset_version_info( GncSqlBackend* be )
{
	gboolean ok;

	g_return_if_fail( be != NULL );

	ok = create_table( be, VERSION_TABLE_NAME, version_table );
	if( be->versions == NULL ) {
		be->versions = g_hash_table_new_full( g_str_hash, g_str_equal, g_free, NULL );
	} else {
		g_hash_table_remove_all( be->versions );
	}
}

/**
 * Finalizes the version table info by destroying the hash table.
 *
 * @param be Backend struct
 */
void
gnc_sql_finalize_version_info( GncSqlBackend* be )
{
	g_return_if_fail( be != NULL );

	g_hash_table_destroy( be->versions );
}

/**
 * Registers the version for a table.  Registering involves updating the
 * db version table and also the hash table.
 *
 * @param be Backend struct
 * @param table_name Table name
 * @param version Version number
 */
static void
register_table_version( const GncSqlBackend* be, const gchar* table_name, gint version )
{
	gchar* sql;
	gint cur_version;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_name != NULL );
	g_return_if_fail( version > 0 );

	cur_version = get_table_version( be, table_name );
	if( cur_version != version ) {
		if( cur_version == 0 ) {
			sql = g_strdup_printf( "INSERT INTO %s VALUES('%s',%d)", VERSION_TABLE_NAME,
								table_name, version );
		} else {
			sql = g_strdup_printf( "UPDATE %s SET %s=%d WHERE %s='%s'", VERSION_TABLE_NAME,
								VERSION_COL_NAME, version,
								TABLE_COL_NAME, table_name );
		}
		execute_nonselect_sql( be, sql );
	}

	g_hash_table_insert( be->versions, g_strdup( table_name ), GINT_TO_POINTER(version) );
}

/**
 * Returns the registered version number for a table.
 *
 * @param be Backend struct
 * @param table_name Table name
 * @return Version number
 */
static gint
get_table_version( const GncSqlBackend* be, const gchar* table_name )
{
	g_return_val_if_fail( be != NULL, 0 );
	g_return_val_if_fail( table_name != NULL, 0 );

	return GPOINTER_TO_INT(g_hash_table_lookup( be->versions, table_name ));
}
/* ========================== END OF FILE ===================== */
