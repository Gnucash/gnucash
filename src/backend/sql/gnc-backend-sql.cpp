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
extern "C"
{
#include <stdlib.h>
#include "config.h"

#include <errno.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>

#include <qof.h>
#include <qofquery-p.h>
#include <qofquerycore-p.h>
#include <Account.h>
#include <TransLog.h>
#include <gnc-engine.h>
#include <SX-book.h>
#include <Recurrence.h>
#include <gncBillTerm.h>
#include <gncTaxTable.h>
#include <gncInvoice.h>
#include "gnc-prefs.h"
#include "gnc-pricedb.h"


#if defined( S_SPLINT_S )
#include "splint-defs.h"
#endif
}
#include "gnc-backend-sql.h"

#include "gnc-account-sql.h"
#include "gnc-book-sql.h"
#include "gnc-budget-sql.h"
#include "gnc-commodity-sql.h"
#include "gnc-lots-sql.h"
#include "gnc-price-sql.h"
#include "gnc-recurrence-sql.h"
#include "gnc-schedxaction-sql.h"
#include "gnc-slots-sql.h"
#include "gnc-transaction-sql.h"

#include "gnc-address-sql.h"
#include "gnc-bill-term-sql.h"
#include "gnc-customer-sql.h"
#include "gnc-employee-sql.h"
#include "gnc-entry-sql.h"
#include "gnc-invoice-sql.h"
#include "gnc-job-sql.h"
#include "gnc-order-sql.h"
#include "gnc-owner-sql.h"
#include "gnc-tax-table-sql.h"
#include "gnc-vendor-sql.h"

static void gnc_sql_init_object_handlers( void );
static void update_progress( GncSqlBackend* be );
static void finish_progress( GncSqlBackend* be );
static void register_standard_col_type_handlers( void );
static gboolean reset_version_info( GncSqlBackend* be );
/*@ null @*/
static GncSqlStatement* build_insert_statement( GncSqlBackend* be,
        const gchar* table_name,
        QofIdTypeConst obj_name, gpointer pObject,
        const GncSqlColumnTableEntry* table );
/*@ null @*/
static GncSqlStatement* build_update_statement( GncSqlBackend* be,
        const gchar* table_name,
        QofIdTypeConst obj_name, gpointer pObject,
        const GncSqlColumnTableEntry* table );
/*@ null @*/
static GncSqlStatement* build_delete_statement( GncSqlBackend* be,
        const gchar* table_name,
        QofIdTypeConst obj_name, gpointer pObject,
        const GncSqlColumnTableEntry* table );

static GList *post_load_commodities = NULL;

#define TRANSACTION_NAME "trans"

typedef struct
{
    /*@ dependent @*/ QofIdType searchObj;
    /*@ dependent @*/
    gpointer pCompiledQuery;
} gnc_sql_query_info;

/* callback structure */
typedef struct
{
    gboolean is_known;
    gboolean is_ok;
    /*@ dependent @*/
    GncSqlBackend* be;
    /*@ dependent @*/
    QofInstance* inst;
    /*@ dependent @*/
    QofQuery* pQuery;
    /*@ dependent @*/
    gpointer pCompiledQuery;
    /*@ owned @*/
    gnc_sql_query_info* pQueryInfo;
} sql_backend;

static QofLogModule log_module = G_LOG_DOMAIN;

#define SQLITE_PROVIDER_NAME "SQLite"

/* ================================================================= */

void
gnc_sql_init( /*@ unused @*/ GncSqlBackend* be )
{
    static gboolean initialized = FALSE;

    if ( !initialized )
    {
        register_standard_col_type_handlers();
        gnc_sql_init_object_handlers();
        initialized = TRUE;
    }
}

/* ================================================================= */

static void
create_tables_cb( const gchar* type, gpointer data_p, gpointer be_p )
{
    GncSqlObjectBackend* pData = static_cast<decltype(pData)>(data_p);
    GncSqlBackend* be = static_cast<decltype(be)>(be_p);

    g_return_if_fail( type != NULL && data_p != NULL && be_p != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );

    if ( pData->create_tables != NULL )
    {
        update_progress( be );
        (pData->create_tables)( be );
    }
}

/* ================================================================= */

/* Main object load order */
static const gchar* fixed_load_order[] =
{ GNC_ID_BOOK, GNC_ID_COMMODITY, GNC_ID_ACCOUNT, GNC_ID_LOT, NULL };

/* Load order for objects from other modules */
static const gchar** other_load_order = NULL;

void
gnc_sql_set_load_order( const gchar** load_order )
{
    other_load_order = load_order;
}

static void
initial_load_cb( const gchar* type, gpointer data_p, gpointer be_p )
{
    GncSqlObjectBackend* pData = static_cast<decltype(pData)>(data_p);
    GncSqlBackend* be = static_cast<decltype(be)>(be_p);
    gint i;

    g_return_if_fail( type != NULL && data_p != NULL && be_p != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );

    // Don't need to load anything if it has already been loaded with the fixed order
    for ( i = 0; fixed_load_order[i] != NULL; i++ )
    {
        update_progress( be );
        if ( g_ascii_strcasecmp( type, fixed_load_order[i] ) == 0 ) return;
    }
    if ( other_load_order != NULL )
    {
        for ( i = 0; other_load_order[i] != NULL; i++ )
        {
            update_progress( be );
            if ( g_ascii_strcasecmp( type, other_load_order[i] ) == 0 ) return;
        }
    }

    if ( pData->initial_load != NULL )
    {
        (pData->initial_load)( be );
    }
}

void
gnc_sql_push_commodity_for_postload_processing (GncSqlBackend *be,
						gpointer comm)
{
    post_load_commodities = g_list_prepend(post_load_commodities, comm);
}

static void
commit_commodity (gpointer data)
{
    gnc_commodity *comm = GNC_COMMODITY (data);
    gnc_sql_commit_commodity (comm);
}

void
gnc_sql_load( GncSqlBackend* be, /*@ dependent @*/ QofBook *book, QofBackendLoadType loadType )
{
    GncSqlObjectBackend* pData;
    gint i;
    Account* root;

    g_return_if_fail( be != NULL );
    g_return_if_fail( book != NULL );

    ENTER( "be=%p, book=%p", be, book );

    be->loading = TRUE;

    if ( loadType == LOAD_TYPE_INITIAL_LOAD )
    {
        g_assert( be->book == NULL );
        be->book = book;

        /* Load any initial stuff. Some of this needs to happen in a certain order */
        for ( i = 0; fixed_load_order[i] != NULL; i++ )
        {
            pData = static_cast<decltype(pData)>(qof_object_lookup_backend(
                                                     fixed_load_order[i],
                                                     GNC_SQL_BACKEND));
            if ( pData->initial_load != NULL )
            {
                update_progress( be );
                (pData->initial_load)( be );
            }
        }
        if ( other_load_order != NULL )
        {
            for ( i = 0; other_load_order[i] != NULL; i++ )
            {
                pData =
                    static_cast<decltype(pData)>(qof_object_lookup_backend(
                                                          other_load_order[i],
                                                          GNC_SQL_BACKEND));
                if ( pData->initial_load != NULL )
                {
                    update_progress( be );
                    (pData->initial_load)( be );
                }
            }
        }

        root = gnc_book_get_root_account( book );
        gnc_account_foreach_descendant( root, (AccountCb)xaccAccountBeginEdit, NULL );

        qof_object_foreach_backend( GNC_SQL_BACKEND, initial_load_cb, be );

        gnc_account_foreach_descendant( root, (AccountCb)xaccAccountCommitEdit, NULL );
    }
    else if ( loadType == LOAD_TYPE_LOAD_ALL )
    {
        // Load all transactions
        gnc_sql_transaction_load_all_tx( be );
    }

    be->loading = FALSE;
    g_list_free_full (post_load_commodities, commit_commodity);
    post_load_commodities = NULL;

    /* Mark the sessoion as clean -- though it should never be marked
     * dirty with this backend
     */
    qof_book_mark_session_saved( book );
    finish_progress( be );

    LEAVE( "" );
}

/* ================================================================= */

static gboolean
write_account_tree( GncSqlBackend* be, Account* root )
{
    GList* descendants;
    /*@ dependent @*/
    GList* node;
    gboolean is_ok = TRUE;

    g_return_val_if_fail( be != NULL, FALSE );
    g_return_val_if_fail( root != NULL, FALSE );

    is_ok = gnc_sql_save_account( be, QOF_INSTANCE(root) );
    if ( is_ok )
    {
        descendants = gnc_account_get_descendants( root );
        for ( node = descendants; node != NULL && is_ok; node = g_list_next(node) )
        {
            is_ok = gnc_sql_save_account( be, QOF_INSTANCE(GNC_ACCOUNT(node->data)) );
            if ( !is_ok ) break;
        }
        g_list_free( descendants );
    }
    update_progress( be );

    return is_ok;
}

static gboolean
write_accounts( GncSqlBackend* be )
{
    gboolean is_ok;

    g_return_val_if_fail( be != NULL, FALSE );

    update_progress( be );
    is_ok = write_account_tree( be, gnc_book_get_root_account( be->book ) );
    if ( is_ok )
    {
        update_progress( be );
        is_ok = write_account_tree( be, gnc_book_get_template_root( be->book ) );
    }

    return is_ok;
}

static int
write_tx( Transaction* tx, gpointer data )
{
    write_objects_t* s = (write_objects_t*)data;

    g_return_val_if_fail( tx != NULL, 0 );
    g_return_val_if_fail( data != NULL, 0 );

    s->is_ok = gnc_sql_save_transaction( s->be, QOF_INSTANCE(tx) );
    update_progress( s->be );

    if ( s->is_ok )
    {
        return 0;
    }
    else
    {
        return 1;
    }
}

static gboolean
write_transactions( GncSqlBackend* be )
{
    write_objects_t data;

    g_return_val_if_fail( be != NULL, FALSE );

    data.be = be;
    data.is_ok = TRUE;
    (void)xaccAccountTreeForEachTransaction(
        gnc_book_get_root_account( be->book ), write_tx, &data );
    update_progress( be );
    return data.is_ok;
}

static gboolean
write_template_transactions( GncSqlBackend* be )
{
    Account* ra;
    write_objects_t data;

    g_return_val_if_fail( be != NULL, FALSE );

    data.is_ok = TRUE;
    data.be = be;
    ra = gnc_book_get_template_root( be->book );
    if ( gnc_account_n_descendants( ra ) > 0 )
    {
        (void)xaccAccountTreeForEachTransaction( ra, write_tx, &data );
        update_progress( be );
    }

    return data.is_ok;
}

static gboolean
write_schedXactions( GncSqlBackend* be )
{
    GList* schedXactions;
    SchedXaction* tmpSX;
    gboolean is_ok = TRUE;

    g_return_val_if_fail( be != NULL, FALSE );

    schedXactions = gnc_book_get_schedxactions( be->book )->sx_list;

    for ( ; schedXactions != NULL && is_ok; schedXactions = schedXactions->next )
    {
        tmpSX = static_cast<decltype(tmpSX)>(schedXactions->data);
        is_ok = gnc_sql_save_schedxaction( be, QOF_INSTANCE( tmpSX ) );
    }
    update_progress( be );

    return is_ok;
}

static void
write_cb( const gchar* type, gpointer data_p, gpointer be_p )
{
    GncSqlObjectBackend* pData = static_cast<decltype(pData)>(data_p);
    GncSqlBackend* be = static_cast<decltype(be)>(be_p);

    g_return_if_fail( type != NULL && data_p != NULL && be_p != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );

    if ( pData->write != NULL )
    {
        (void)(pData->write)( be );
        update_progress( be );
    }
}

static void
update_progress( GncSqlBackend* be )
{
    if ( be->be.percentage != NULL )
        (be->be.percentage)( NULL, 101.0 );
}

static void
finish_progress( GncSqlBackend* be )
{
    if ( be->be.percentage != NULL )
        (be->be.percentage)( NULL, -1.0 );
}

void
gnc_sql_sync_all( GncSqlBackend* be, /*@ dependent @*/ QofBook *book )
{
    gboolean is_ok;

    g_return_if_fail( be != NULL );
    g_return_if_fail( book != NULL );

    ENTER( "book=%p, be->book=%p", book, be->book );
    update_progress( be );
    (void)reset_version_info( be );

    /* Create new tables */
    be->is_pristine_db = TRUE;
    qof_object_foreach_backend( GNC_SQL_BACKEND, create_tables_cb, be );

    /* Save all contents */
    be->book = book;
    be->obj_total = 0;
    be->obj_total += 1 + gnc_account_n_descendants( gnc_book_get_root_account( book ) );
    be->obj_total += gnc_book_count_transactions( book );
    be->operations_done = 0;

    is_ok = gnc_sql_connection_begin_transaction( be->conn );

    // FIXME: should write the set of commodities that are used
    //write_commodities( be, book );
    if ( is_ok )
    {
        is_ok = gnc_sql_save_book( be, QOF_INSTANCE(book) );
    }
    if ( is_ok )
    {
        is_ok = write_accounts( be );
    }
    if ( is_ok )
    {
        is_ok = write_transactions( be );
    }
    if ( is_ok )
    {
        is_ok = write_template_transactions( be );
    }
    if ( is_ok )
    {
        is_ok = write_schedXactions( be );
    }
    if ( is_ok )
    {
        qof_object_foreach_backend( GNC_SQL_BACKEND, write_cb, be );
    }
    if ( is_ok )
    {
        is_ok = gnc_sql_connection_commit_transaction( be->conn );
    }
    if ( is_ok )
    {
        be->is_pristine_db = FALSE;

        /* Mark the session as clean -- though it shouldn't ever get
	 * marked dirty with this backend
	 */
        qof_book_mark_session_saved( book );
    }
    else
    {
        if (!qof_backend_check_error ((QofBackend*)be))
            qof_backend_set_error( (QofBackend*)be, ERR_BACKEND_SERVER_ERR );
        is_ok = gnc_sql_connection_rollback_transaction( be->conn );
    }
    finish_progress( be );
    LEAVE( "book=%p", book );
}

/* ================================================================= */
/* Routines to deal with the creation of multiple books. */

void
gnc_sql_begin_edit( GncSqlBackend *be, QofInstance *inst )
{
    g_return_if_fail( be != NULL );
    g_return_if_fail( inst != NULL );

    ENTER( " " );
    LEAVE( "" );
}

void
gnc_sql_rollback_edit( GncSqlBackend *be, QofInstance *inst )
{
    g_return_if_fail( be != NULL );
    g_return_if_fail( inst != NULL );

    ENTER( " " );
    LEAVE( "" );
}

static void
commit_cb( const gchar* type, gpointer data_p, gpointer be_data_p )
{
    GncSqlObjectBackend* pData = static_cast<decltype(pData)>(data_p);
    sql_backend* be_data = static_cast<decltype(be_data)>(be_data_p);

    g_return_if_fail( type != NULL && pData != NULL && be_data != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );

    /* If this has already been handled, or is not the correct handler, return */
    if ( strcmp( pData->type_name, be_data->inst->e_type ) != 0 ) return;
    if ( be_data->is_known ) return;

    if ( pData->commit != NULL )
    {
        be_data->is_ok = (pData->commit)( be_data->be, be_data->inst );
        be_data->is_known = TRUE;
    }
}

/* Commit_edit handler - find the correct backend handler for this object
 * type and call its commit handler
 */
void
gnc_sql_commit_edit( GncSqlBackend *be, QofInstance *inst )
{
    sql_backend be_data;
    gboolean is_dirty;
    gboolean is_destroying;
    gboolean is_infant;

    g_return_if_fail( be != NULL );
    g_return_if_fail( inst != NULL );

    if ( qof_book_is_readonly( be->book ) )
    {
        qof_backend_set_error( (QofBackend*)be, ERR_BACKEND_READONLY );
        (void)gnc_sql_connection_rollback_transaction( be->conn );
        return;
    }
    /* During initial load where objects are being created, don't commit
    anything, but do mark the object as clean. */
    if ( be->loading )
    {
        qof_instance_mark_clean( inst );
        return;
    }

    // The engine has a PriceDB object but it isn't in the database
    if ( strcmp( inst->e_type, "PriceDB" ) == 0 )
    {
        qof_instance_mark_clean( inst );
        qof_book_mark_session_saved( be->book );
        return;
    }

    ENTER( " " );

    is_dirty = qof_instance_get_dirty_flag( inst );
    is_destroying = qof_instance_get_destroying( inst );
    is_infant = qof_instance_get_infant( inst );

    DEBUG( "%s dirty = %d, do_free = %d, infant = %d\n",
    (inst->e_type ? inst->e_type : "(null)"),
    is_dirty, is_destroying, is_infant );

    if ( !is_dirty && !is_destroying )
    {
        LEAVE( "!dirty OR !destroying" );
        return;
    }

    if ( !gnc_sql_connection_begin_transaction( be->conn ) )
    {
        PERR( "gnc_sql_commit_edit(): begin_transaction failed\n" );
        LEAVE( "Rolled back - database transaction begin error" );
        return;
    }

    be_data.is_known = FALSE;
    be_data.be = be;
    be_data.inst = inst;
    be_data.is_ok = TRUE;

    qof_object_foreach_backend( GNC_SQL_BACKEND, commit_cb, &be_data );

    if ( !be_data.is_known )
    {
        PERR( "gnc_sql_commit_edit(): Unknown object type '%s'\n", inst->e_type );
        (void)gnc_sql_connection_rollback_transaction( be->conn );

        // Don't let unknown items still mark the book as being dirty
        qof_book_mark_session_saved( be->book );
        qof_instance_mark_clean(inst);
        LEAVE( "Rolled back - unknown object type" );
        return;
    }
    if ( !be_data.is_ok )
    {
        // Error - roll it back
        (void)gnc_sql_connection_rollback_transaction( be->conn );

        // This *should* leave things marked dirty
        LEAVE( "Rolled back - database error" );
        return;
    }

    (void)gnc_sql_connection_commit_transaction( be->conn );

    qof_book_mark_session_saved( be->book );
    qof_instance_mark_clean(inst);

    LEAVE( "" );
}
/* ---------------------------------------------------------------------- */

/* Query processing */
static void
handle_and_term( QofQueryTerm* pTerm, GString* sql )
{
    GSList* pParamPath;
    QofQueryPredData* pPredData;
    gboolean isInverted;
    GSList* name;
    gchar val[G_ASCII_DTOSTR_BUF_SIZE];

    g_return_if_fail( pTerm != NULL );
    g_return_if_fail( sql != NULL );

    pParamPath = qof_query_term_get_param_path( pTerm );
    pPredData = qof_query_term_get_pred_data( pTerm );
    isInverted = qof_query_term_is_inverted( pTerm );

    if ( strcmp( pPredData->type_name, QOF_TYPE_GUID ) == 0 )
    {
        query_guid_t guid_data = (query_guid_t)pPredData;
        GList* guid_entry;

        for ( name = pParamPath; name != NULL; name = name->next )
        {
            if ( name != pParamPath ) g_string_append( sql, "." );
            g_string_append(sql, static_cast<char*>(name->data));
        }

        if ( guid_data->options == QOF_GUID_MATCH_ANY )
        {
            if ( isInverted ) g_string_append( sql, " NOT " );
            g_string_append( sql, " IN (" );
        }
        for ( guid_entry = guid_data->guids; guid_entry != NULL; guid_entry = guid_entry->next )
        {
            if ( guid_entry != guid_data->guids ) g_string_append( sql, "." );
            (void)guid_to_string_buff(static_cast<GncGUID*>(guid_entry->data),
                                      val);
            g_string_append( sql, "'" );
            g_string_append( sql, val );
            g_string_append( sql, "'" );
        }
        if ( guid_data->options == QOF_GUID_MATCH_ANY )
        {
            g_string_append( sql, ")" );
        }
    }

    g_string_append( sql, "(" );
    if ( isInverted )
    {
        g_string_append( sql, "!" );
    }

    for ( name = pParamPath; name != NULL; name = name->next )
    {
        if ( name != pParamPath ) g_string_append( sql, "." );
        g_string_append(sql, static_cast<char*>(name->data) );
    }

    if ( pPredData->how == QOF_COMPARE_LT )
    {
        g_string_append( sql, "<" );
    }
    else if ( pPredData->how == QOF_COMPARE_LTE )
    {
        g_string_append( sql, "<=" );
    }
    else if ( pPredData->how == QOF_COMPARE_EQUAL )
    {
        g_string_append( sql, "=" );
    }
    else if ( pPredData->how == QOF_COMPARE_GT )
    {
        g_string_append( sql, ">" );
    }
    else if ( pPredData->how == QOF_COMPARE_GTE )
    {
        g_string_append( sql, ">=" );
    }
    else if ( pPredData->how == QOF_COMPARE_NEQ )
    {
        g_string_append( sql, "~=" );
    }
    else
    {
        g_string_append( sql, "??" );
    }

    if ( strcmp( pPredData->type_name, "string" ) == 0 )
    {
        query_string_t pData = (query_string_t)pPredData;
        g_string_append( sql, "'" );
        g_string_append( sql, pData->matchstring );
        g_string_append( sql, "'" );
    }
    else if ( strcmp( pPredData->type_name, "date" ) == 0 )
    {
        query_date_t pData = (query_date_t)pPredData;

        (void)gnc_timespec_to_iso8601_buff( pData->date, val );
        g_string_append( sql, "'" );
        //g_string_append( sql, val, 4+1+2+1+2 );
        g_string_append( sql, "'" );
    }
    else if ( strcmp( pPredData->type_name, "numeric" ) == 0 )
    {
        /* query_numeric_t pData = (query_numeric_t)pPredData; */

        g_string_append( sql, "numeric" );
    }
    else if ( strcmp( pPredData->type_name, QOF_TYPE_GUID ) == 0 )
    {
    }
    else if ( strcmp( pPredData->type_name, "gint32" ) == 0 )
    {
        query_int32_t pData = (query_int32_t)pPredData;

        sprintf( val, "%d", pData->val );
        g_string_append( sql, val );
    }
    else if ( strcmp( pPredData->type_name, "gint64" ) == 0 )
    {
        query_int64_t pData = (query_int64_t)pPredData;

        sprintf( val, "%" G_GINT64_FORMAT, pData->val );
        g_string_append( sql, val );
    }
    else if ( strcmp( pPredData->type_name, "double" ) == 0 )
    {
        query_double_t pData = (query_double_t)pPredData;

        g_ascii_dtostr( val, sizeof(val), pData->val );
        g_string_append( sql, val );
    }
    else if ( strcmp( pPredData->type_name, "boolean" ) == 0 )
    {
        query_boolean_t pData = (query_boolean_t)pPredData;

        sprintf( val, "%d", pData->val );
        g_string_append( sql, val );
    }
    else
    {
        g_assert( FALSE );
    }

    g_string_append( sql, ")" );
}

static void
compile_query_cb( const gchar* type, gpointer data_p, gpointer be_data_p )
{
    GncSqlObjectBackend* pData = static_cast<decltype(pData)>(data_p);
    sql_backend* be_data = static_cast<decltype(be_data)>(be_data_p);

    g_return_if_fail( type != NULL && pData != NULL && be_data != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );

    // Is this the right item?
    if ( strcmp( type, be_data->pQueryInfo->searchObj ) != 0 ) return;
    if ( be_data->is_ok ) return;

    if ( pData->compile_query != NULL )
    {
        be_data->pQueryInfo->pCompiledQuery = (pData->compile_query)(
            be_data->be,
            be_data->pQuery );
        be_data->is_ok = TRUE;
    }
}

gchar* gnc_sql_compile_query_to_sql( GncSqlBackend* be, QofQuery* query );

/*@ null @*/
gpointer
gnc_sql_compile_query( QofBackend* pBEnd, QofQuery* pQuery )
{
    GncSqlBackend *be = (GncSqlBackend*)pBEnd;
    QofIdType searchObj;
    sql_backend be_data;
    gnc_sql_query_info* pQueryInfo;

    g_return_val_if_fail( pBEnd != NULL, NULL );
    g_return_val_if_fail( pQuery != NULL, NULL );

    ENTER( " " );

//gnc_sql_compile_query_to_sql( be, pQuery );
    searchObj = qof_query_get_search_for( pQuery );

    pQueryInfo = static_cast<decltype(pQueryInfo)>(
        g_malloc(sizeof(gnc_sql_query_info)));
    g_assert( pQueryInfo != NULL );
    pQueryInfo->pCompiledQuery = NULL;
    pQueryInfo->searchObj = searchObj;

    // Try various objects first
    be_data.is_ok = FALSE;
    be_data.be = be;
    be_data.pQuery = pQuery;
    be_data.pQueryInfo = pQueryInfo;

    qof_object_foreach_backend( GNC_SQL_BACKEND, compile_query_cb, &be_data );
    if ( be_data.is_ok )
    {
        LEAVE( "" );
        return be_data.pQueryInfo;
    }

    LEAVE( "" );

    return pQueryInfo;
}

static const gchar*
convert_search_obj( QofIdType objType )
{
    return (gchar*)objType;
}

gchar*
gnc_sql_compile_query_to_sql( GncSqlBackend* be, QofQuery* query )
{
    QofIdType searchObj;
    GString* sql;

    g_return_val_if_fail( be != NULL, NULL );
    g_return_val_if_fail( query != NULL, NULL );

    searchObj = qof_query_get_search_for( query );

    /* Convert search object type to table name */
    sql = g_string_new( "" );
    g_string_append( sql, "SELECT * FROM " );
    g_string_append( sql, convert_search_obj( searchObj ) );
    if ( !qof_query_has_terms( query ) )
    {
        g_string_append( sql, ";" );
    }
    else
    {
        GList* orterms = qof_query_get_terms( query );
        GList* orTerm;

        g_string_append( sql, " WHERE " );

        for ( orTerm = orterms; orTerm != NULL; orTerm = orTerm->next )
        {
            GList* andterms = (GList*)orTerm->data;
            GList* andTerm;

            if ( orTerm != orterms ) g_string_append( sql, " OR " );
            g_string_append( sql, "(" );
            for ( andTerm = andterms; andTerm != NULL; andTerm = andTerm->next )
            {
                if ( andTerm != andterms ) g_string_append( sql, " AND " );
                handle_and_term( (QofQueryTerm*)andTerm->data, sql );
            }
            g_string_append( sql, ")" );
        }
    }

    DEBUG( "Compiled: %s\n", sql->str );
    return g_string_free( sql, FALSE );
}

static void
free_query_cb( const gchar* type, gpointer data_p, gpointer be_data_p )
{
    GncSqlObjectBackend* pData = static_cast<decltype(pData)>(data_p);
    sql_backend* be_data = static_cast<decltype(be_data)>(be_data_p);

    g_return_if_fail( type != NULL && pData != NULL && be_data != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );
    if ( be_data->is_ok ) return;
    if ( strcmp( type, be_data->pQueryInfo->searchObj ) != 0 ) return;

    if ( pData->free_query != NULL )
    {
        (pData->free_query)( be_data->be, be_data->pCompiledQuery );
        be_data->is_ok = TRUE;
    }
}

void
gnc_sql_free_query( QofBackend* pBEnd, gpointer pQuery )
{
    GncSqlBackend *be = (GncSqlBackend*)pBEnd;
    gnc_sql_query_info* pQueryInfo = (gnc_sql_query_info*)pQuery;
    sql_backend be_data;

    g_return_if_fail( pBEnd != NULL );
    g_return_if_fail( pQuery != NULL );

    ENTER( " " );

    // Try various objects first
    be_data.is_ok = FALSE;
    be_data.be = be;
    be_data.pCompiledQuery = pQuery;
    be_data.pQueryInfo = pQueryInfo;

    qof_object_foreach_backend( GNC_SQL_BACKEND, free_query_cb, &be_data );
    if ( be_data.is_ok )
    {
        LEAVE( "" );
        return;
    }

    if ( pQueryInfo->pCompiledQuery != NULL )
    {
        DEBUG( "%s\n", (gchar*)pQueryInfo->pCompiledQuery );
        g_free( pQueryInfo->pCompiledQuery );
    }
    g_free( pQueryInfo );

    LEAVE( "" );
}

static void
run_query_cb( const gchar* type, gpointer data_p, gpointer be_data_p )
{
    GncSqlObjectBackend* pData = static_cast<decltype(pData)>(data_p);
    sql_backend* be_data = static_cast<decltype(be_data)>(be_data_p);

    g_return_if_fail( type != NULL && pData != NULL && be_data != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );
    if ( be_data->is_ok ) return;

    // Is this the right item?
    if ( strcmp( type, be_data->pQueryInfo->searchObj ) != 0 ) return;

    if ( pData->run_query != NULL )
    {
        (pData->run_query)( be_data->be, be_data->pCompiledQuery );
        be_data->is_ok = TRUE;
    }
}

void
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
    be_data.is_ok = FALSE;
    be_data.be = be;
    be_data.pCompiledQuery = pQueryInfo->pCompiledQuery;
    be_data.pQueryInfo = pQueryInfo;

    qof_object_foreach_backend( GNC_SQL_BACKEND, run_query_cb, &be_data );
    be->loading = FALSE;
    be->in_query = FALSE;
    qof_event_resume();
//    if( be_data.is_ok ) {
//        LEAVE( "" );
//        return;
//    }

    // Mark the book as clean
    qof_instance_mark_clean( QOF_INSTANCE(be->book) );

//    DEBUG( "%s\n", (gchar*)pQueryInfo->pCompiledQuery );

    LEAVE( "" );
}

/* ================================================================= */
/* Order in which business objects need to be loaded */
static const gchar* business_fixed_load_order[] =
{ GNC_ID_BILLTERM, GNC_ID_TAXTABLE, GNC_ID_INVOICE, NULL };

static void
business_core_sql_init(void)
{
    /* Initialize our pointers into the backend subsystem */
    gnc_address_sql_initialize();
    gnc_billterm_sql_initialize();
    gnc_customer_sql_initialize();
    gnc_employee_sql_initialize();
    gnc_entry_sql_initialize();
    gnc_invoice_sql_initialize();
    gnc_job_sql_initialize();
    gnc_order_sql_initialize();
    gnc_owner_sql_initialize();
    gnc_taxtable_sql_initialize();
    gnc_vendor_sql_initialize();

    gnc_sql_set_load_order( business_fixed_load_order );
}

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

    /* And the business objects */
    business_core_sql_init();
}

/* ================================================================= */

gint64
gnc_sql_get_integer_value( const GValue* value )
{
    g_return_val_if_fail( value != NULL, 0 );

    if ( G_VALUE_HOLDS_INT(value) )
    {
        return (gint64)g_value_get_int( value );
    }
    else if ( G_VALUE_HOLDS_UINT(value) )
    {
        return (gint64)g_value_get_uint( value );
    }
    else if ( G_VALUE_HOLDS_LONG(value) )
    {
        return (gint64)g_value_get_long( value );
    }
    else if ( G_VALUE_HOLDS_ULONG(value) )
    {
        return (gint64)g_value_get_ulong( value );
    }
    else if ( G_VALUE_HOLDS_INT64(value) )
    {
        return g_value_get_int64( value );
    }
    else if ( G_VALUE_HOLDS_UINT64(value) )
    {
        return (gint64)g_value_get_uint64( value );
    }
    else if ( G_VALUE_HOLDS_STRING( value ) )
    {
        return g_ascii_strtoll( g_value_get_string( value ), NULL, 10 );
    }
    else
    {
        PWARN( "Unknown type: %s", G_VALUE_TYPE_NAME( value ) );
    }

    return 0;
}

/* ----------------------------------------------------------------- */
/*@ null @*/ static gpointer
get_autoinc_id(void *object, const QofParam* param)
{
    // Just need a 0 to force a new autoinc value
    return (gpointer)0;
}

static void
set_autoinc_id(void* object, void *item)
{
    // Nowhere to put the ID
}

/*@ null @*/ QofAccessFunc
gnc_sql_get_getter( QofIdTypeConst obj_name, const GncSqlColumnTableEntry* table_row )
{
    QofAccessFunc getter;

    g_return_val_if_fail( obj_name != NULL, NULL );
    g_return_val_if_fail( table_row != NULL, NULL );

    if ( (table_row->flags & COL_AUTOINC) != 0 )
    {
        getter = get_autoinc_id;
    }
    else if ( table_row->qof_param_name != NULL )
    {
        getter = qof_class_get_parameter_getter( obj_name,
        table_row->qof_param_name );
    }
    else
    {
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

    for ( subtable_row = subtable; subtable_row->col_name != NULL; subtable_row++ )
    {
        buf = g_strdup_printf( "%s_%s", table_row->col_name, subtable_row->col_name );
        (*pList) = g_list_append( (*pList), buf );
    }
}

static GncSqlColumnInfo*
create_column_info( const GncSqlColumnTableEntry* table_row, GncSqlBasicColumnType type,
gint size, gboolean is_unicode )
{
    GncSqlColumnInfo* info;

    info = g_new0( GncSqlColumnInfo, 1 );
    g_assert( info != NULL );
    info->name = g_strdup( table_row->col_name );
    info->type = type;
    info->size = size;
    info->is_primary_key = ((table_row->flags & COL_PKEY) != 0) ? TRUE : FALSE;
    info->null_allowed = ((table_row->flags & COL_NNUL) != 0) ? FALSE : TRUE;
    info->is_unicode = is_unicode;
    info->is_autoinc = ((table_row->flags & COL_AUTOINC) != 0) ? TRUE : FALSE;

    return info;
}

/* ----------------------------------------------------------------- */
static void
load_string( const GncSqlBackend* be, GncSqlRow* row,
/*@ null @*/ QofSetterFunc setter, gpointer pObject,
const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    const gchar* s;

    g_return_if_fail( be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( table_row->gobj_param_name != NULL || setter != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    g_return_if_fail( val != NULL );
    s = g_value_get_string( val );
    if ( table_row->gobj_param_name != NULL )
    {
	if (QOF_IS_INSTANCE (pObject))
	    qof_instance_increase_editlevel (QOF_INSTANCE (pObject));
        g_object_set( pObject, table_row->gobj_param_name, s, NULL );
	if (QOF_IS_INSTANCE (pObject))
	    qof_instance_decrease_editlevel (QOF_INSTANCE (pObject));
    }
    else
    {
        g_return_if_fail( setter != NULL );
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

    info = create_column_info( table_row, BCT_STRING, table_row->size, TRUE );

    *pList = g_list_append( *pList, info );
}

static void
add_gvalue_string_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    QofAccessFunc getter;
    gchar* s = NULL;
    GValue* value;

    g_return_if_fail( be != NULL );
    g_return_if_fail( obj_name != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( pList != NULL );

    value = g_new0( GValue, 1 );
    g_assert( value != NULL );
    memset( value, 0, sizeof( GValue ) );
    if ( table_row->gobj_param_name != NULL )
    {
        g_object_get( pObject, table_row->gobj_param_name, &s, NULL );
    }
    else
    {
        getter = gnc_sql_get_getter( obj_name, table_row );
        if ( getter != NULL )
        {
            s = (gchar*)(*getter)( pObject, NULL );
            if ( s != NULL )
            {
                s = g_strdup( s );
            }
        }
    }
    (void)g_value_init( value, G_TYPE_STRING );
    if ( s )
    {
        g_value_take_string( value, s );
    }

    (*pList) = g_slist_append( (*pList), value );
}

static GncSqlColumnTypeHandler string_handler
=
{
    load_string,
    add_string_col_info_to_list,
    gnc_sql_add_colname_to_list,
    add_gvalue_string_to_slist
};
/* ----------------------------------------------------------------- */
typedef gint (*IntAccessFunc)( const gpointer );
typedef void (*IntSetterFunc)( const gpointer, gint );

static void
load_int( const GncSqlBackend* be, GncSqlRow* row,
          /*@ null @*/ QofSetterFunc setter, gpointer pObject,
          const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    gint int_value;
    IntSetterFunc i_setter;

    g_return_if_fail( be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( table_row->gobj_param_name != NULL || setter != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if ( val == NULL )
    {
        int_value = 0;
    }
    else
    {
        int_value = (gint)gnc_sql_get_integer_value( val );
    }
    if ( table_row->gobj_param_name != NULL )
    {
	if (QOF_IS_INSTANCE (pObject))
	    qof_instance_increase_editlevel (QOF_INSTANCE (pObject));
        g_object_set( pObject, table_row->gobj_param_name, int_value, NULL );
	if (QOF_IS_INSTANCE (pObject))
	    qof_instance_decrease_editlevel (QOF_INSTANCE (pObject));
    }
    else
    {
        g_return_if_fail( setter != NULL );
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

    info = create_column_info( table_row, BCT_INT, 0, FALSE );

    *pList = g_list_append( *pList, info );
}

static void
add_gvalue_int_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                         const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    gint int_value = 0;
    IntAccessFunc i_getter;
    GValue* value;

    g_return_if_fail( be != NULL );
    g_return_if_fail( obj_name != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( pList != NULL );

    value = g_new0( GValue, 1 );
    g_assert( value != NULL );
    (void)g_value_init( value, G_TYPE_INT );

    if ( table_row->gobj_param_name != NULL )
    {
        g_object_get_property(G_OBJECT(pObject), table_row->gobj_param_name,
                              value );
    }
    else
    {
        i_getter = (IntAccessFunc)gnc_sql_get_getter( obj_name, table_row );
        if ( i_getter != NULL )
        {
            int_value = (*i_getter)( pObject );
        }
        g_value_set_int( value, int_value );
    }

    (*pList) = g_slist_append( (*pList), value );
}

static GncSqlColumnTypeHandler int_handler
=
{
    load_int,
    add_int_col_info_to_list,
    gnc_sql_add_colname_to_list,
    add_gvalue_int_to_slist
};
/* ----------------------------------------------------------------- */
typedef gboolean (*BooleanAccessFunc)( const gpointer );
typedef void (*BooleanSetterFunc)( const gpointer, gboolean );

static void
load_boolean( const GncSqlBackend* be, GncSqlRow* row,
              /*@ null @*/ QofSetterFunc setter, gpointer pObject,
              const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    gint int_value;
    BooleanSetterFunc b_setter;

    g_return_if_fail( be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( table_row->gobj_param_name != NULL || setter != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if ( val == NULL )
    {
        int_value = 0;
    }
    else
    {
        int_value = (gint)gnc_sql_get_integer_value( val );
    }
    if ( table_row->gobj_param_name != NULL )
    {
	if (QOF_IS_INSTANCE (pObject))
	    qof_instance_increase_editlevel (QOF_INSTANCE (pObject));
        g_object_set( pObject, table_row->gobj_param_name, int_value, NULL );
	if (QOF_IS_INSTANCE (pObject))
	    qof_instance_decrease_editlevel (QOF_INSTANCE (pObject));
    }
    else
    {
        g_return_if_fail( setter != NULL );
        b_setter = (BooleanSetterFunc)setter;
        (*b_setter)( pObject, (int_value != 0) ? TRUE : FALSE );
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

    info = create_column_info( table_row, BCT_INT, 0, FALSE );

    *pList = g_list_append( *pList, info );
}

static void
add_gvalue_boolean_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                             const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    gint int_value = 0;
    BooleanAccessFunc b_getter;
    GValue* value;

    g_return_if_fail( be != NULL );
    g_return_if_fail( obj_name != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( pList != NULL );

    value = g_new0( GValue, 1 );
    g_assert( value != NULL );

    if ( table_row->gobj_param_name != NULL )
    {
        g_object_get( pObject, table_row->gobj_param_name, &int_value, NULL );
    }
    else
    {
        b_getter = (BooleanAccessFunc)gnc_sql_get_getter( obj_name, table_row );
        if ( b_getter != NULL )
        {
            int_value = ((*b_getter)( pObject )) ? 1 : 0;
        }
    }
    (void)g_value_init( value, G_TYPE_INT );
    g_value_set_int( value, int_value );

    (*pList) = g_slist_append( (*pList), value );
}

static GncSqlColumnTypeHandler boolean_handler
=
{
    load_boolean,
    add_boolean_col_info_to_list,
    gnc_sql_add_colname_to_list,
    add_gvalue_boolean_to_slist
};
/* ----------------------------------------------------------------- */
typedef gint64 (*Int64AccessFunc)( const gpointer );
typedef void (*Int64SetterFunc)( const gpointer, gint64 );

static void
load_int64( const GncSqlBackend* be, GncSqlRow* row,
            /*@ null @*/ QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    gint64 i64_value = 0;
    Int64SetterFunc i64_setter = (Int64SetterFunc)setter;

    g_return_if_fail( be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( table_row->gobj_param_name != NULL || setter != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if ( val != NULL )
    {
        i64_value = gnc_sql_get_integer_value( val );
    }
    if ( table_row->gobj_param_name != NULL )
    {
	if (QOF_IS_INSTANCE (pObject))
	    qof_instance_increase_editlevel (QOF_INSTANCE (pObject));
        g_object_set( pObject, table_row->gobj_param_name, i64_value, NULL );
	if (QOF_IS_INSTANCE (pObject))
	    qof_instance_decrease_editlevel (QOF_INSTANCE (pObject));
    }
    else
    {
        (*i64_setter)( pObject, i64_value );
    }
}

static void
add_int64_col_info_to_list( const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,
                            GList** pList )
{
    GncSqlColumnInfo* info;

    g_return_if_fail( be != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( pList != NULL );

    info = create_column_info( table_row, BCT_INT64, 0, FALSE );

    *pList = g_list_append( *pList, info );
}

static void
add_gvalue_int64_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                           const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    gint64 i64_value = 0;
    Int64AccessFunc getter;
    GValue* value;

    g_return_if_fail( be != NULL );
    g_return_if_fail( obj_name != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( pList != NULL );

    value = g_new0( GValue, 1 );
    g_assert( value != NULL );
    if ( table_row->gobj_param_name != NULL )
    {
        g_object_get( pObject, table_row->gobj_param_name, &i64_value, NULL );
    }
    else
    {
        getter = (Int64AccessFunc)gnc_sql_get_getter( obj_name, table_row );
        if ( getter != NULL )
        {
            i64_value = (*getter)( pObject );
        }
    }
    (void)g_value_init( value, G_TYPE_INT64 );
    g_value_set_int64( value, i64_value );

    (*pList) = g_slist_append( (*pList), value );
}

static GncSqlColumnTypeHandler int64_handler
=
{
    load_int64,
    add_int64_col_info_to_list,
    gnc_sql_add_colname_to_list,
    add_gvalue_int64_to_slist
};
/* ----------------------------------------------------------------- */

static void
load_double( const GncSqlBackend* be, GncSqlRow* row,
             /*@ null @*/ QofSetterFunc setter, gpointer pObject,
             const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    gdouble d_value;

    g_return_if_fail( be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( table_row->gobj_param_name != NULL || setter != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if ( val == NULL )
    {
        (*setter)( pObject, (gpointer)NULL );
    }
    else
    {
        if ( G_VALUE_HOLDS(val, G_TYPE_INT) )
        {
            d_value = (gdouble)g_value_get_int( val );
        }
        else if ( G_VALUE_HOLDS(val, G_TYPE_FLOAT) )
        {
            d_value = g_value_get_float( val );
        }
        else if (G_VALUE_HOLDS(val, G_TYPE_DOUBLE) )
        {
            d_value = g_value_get_double( val );
        }
        else
        {
            PWARN( "Unknown float value type: %s\n", g_type_name( G_VALUE_TYPE(val) ) );
            d_value = 0;
        }
        if ( table_row->gobj_param_name != NULL )
        {
	if (QOF_IS_INSTANCE (pObject))
	    qof_instance_increase_editlevel (QOF_INSTANCE (pObject));
            g_object_set( pObject, table_row->gobj_param_name, d_value, NULL );
	    if (QOF_IS_INSTANCE (pObject))
	    qof_instance_decrease_editlevel (QOF_INSTANCE (pObject));
        }
        else
        {
            (*setter)( pObject, (gpointer)&d_value );
        }
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

    info = create_column_info( table_row, BCT_DOUBLE, 0, FALSE );

    *pList = g_list_append( *pList, info );
}

static void
add_gvalue_double_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                            const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    QofAccessFunc getter;
    gdouble* pDouble = NULL;
    gdouble d_value;
    GValue* value;

    g_return_if_fail( be != NULL );
    g_return_if_fail( obj_name != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );

    value = g_new0( GValue, 1 );
    g_assert( value != NULL );
    getter = gnc_sql_get_getter( obj_name, table_row );
    if ( getter != NULL )
    {
        pDouble = static_cast<decltype(pDouble)>((*getter)(pObject, NULL ));
    }
    if ( pDouble != NULL )
    {
        d_value = *pDouble;
        (void)g_value_init( value, G_TYPE_DOUBLE );
        g_value_set_double( value, d_value );
    }
    else
    {
        (void)g_value_init( value, G_TYPE_DOUBLE );
        g_value_set_double( value, 0.0 );
    }

    (*pList) = g_slist_append( (*pList), value );
}

static GncSqlColumnTypeHandler double_handler
=
{
    load_double,
    add_double_col_info_to_list,
    gnc_sql_add_colname_to_list,
    add_gvalue_double_to_slist
};
/* ----------------------------------------------------------------- */

static void
load_guid( const GncSqlBackend* be, GncSqlRow* row,
           /*@ null @*/ QofSetterFunc setter, gpointer pObject,
           const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    GncGUID guid;
    const GncGUID* pGuid;

    g_return_if_fail( be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( table_row->gobj_param_name != NULL || setter != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if ( val == NULL || g_value_get_string( val ) == NULL )
    {
        pGuid = NULL;
    }
    else
    {
        (void)string_to_guid( g_value_get_string( val ), &guid );
        pGuid = &guid;
    }
    if ( pGuid != NULL )
    {
        if ( table_row->gobj_param_name != NULL )
        {
	if (QOF_IS_INSTANCE (pObject))
	    qof_instance_increase_editlevel (QOF_INSTANCE (pObject));
            g_object_set( pObject, table_row->gobj_param_name, pGuid, NULL );
	    if (QOF_IS_INSTANCE (pObject))
	    qof_instance_decrease_editlevel (QOF_INSTANCE (pObject));
        }
        else
        {
            g_return_if_fail( setter != NULL );
            (*setter)( pObject, (const gpointer)pGuid );
        }
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

    info = create_column_info( table_row, BCT_STRING, GUID_ENCODING_LENGTH, FALSE );

    *pList = g_list_append( *pList, info );
}

static void
add_gvalue_guid_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                          const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    QofAccessFunc getter;
    GncGUID* guid = NULL;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
    GValue* value;

    g_return_if_fail( be != NULL );
    g_return_if_fail( obj_name != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );

    value = g_new0( GValue, 1 );
    g_assert( value != NULL );
    if ( table_row->gobj_param_name != NULL )
    {
        g_object_get( pObject, table_row->gobj_param_name, &guid, NULL );
    }
    else
    {
        getter = gnc_sql_get_getter( obj_name, table_row );
        if ( getter != NULL )
        {
            guid = static_cast<decltype(guid)>((*getter)( pObject, NULL ));
        }
    }
    (void)g_value_init( value, G_TYPE_STRING );
    if ( guid != NULL )
    {
        (void)guid_to_string_buff( guid, guid_buf );
        g_value_set_string( value, guid_buf );
    }

    (*pList) = g_slist_append( (*pList), value );

}

static GncSqlColumnTypeHandler guid_handler
=
{
    load_guid,
    add_guid_col_info_to_list,
    gnc_sql_add_colname_to_list,
    add_gvalue_guid_to_slist
};
/* ----------------------------------------------------------------- */

void
gnc_sql_add_gvalue_objectref_guid_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
        const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    QofAccessFunc getter;
    const GncGUID* guid = NULL;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
    QofInstance* inst = NULL;
    GValue* value;

    g_return_if_fail( be != NULL );
    g_return_if_fail( obj_name != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );

    value = g_new0( GValue, 1 );
    g_assert( value != NULL );
    if ( table_row->gobj_param_name != NULL )
    {
        g_object_get( pObject, table_row->gobj_param_name, &inst, NULL );
    }
    else
    {
        getter = gnc_sql_get_getter( obj_name, table_row );
        if ( getter != NULL )
        {
            inst = static_cast<decltype(inst)>((*getter)( pObject, NULL ));
        }
    }
    if ( inst != NULL )
    {
        guid = qof_instance_get_guid( inst );
    }
    (void)g_value_init( value, G_TYPE_STRING );
    if ( guid != NULL )
    {
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

/* This is required because we're passing be->timespace_format to
 * g_strdup_printf.
 */
#pragma GCC diagnostic ignored "-Wformat-nonliteral"
gchar*
gnc_sql_convert_timespec_to_string( const GncSqlBackend* be, Timespec ts )
{
    time64 time;
    struct tm* tm;
    gint year;
    gchar* datebuf;

    time = timespecToTime64( ts );
    tm = gnc_gmtime( &time );

    year = tm->tm_year + 1900;

    datebuf = g_strdup_printf( be->timespec_format,
                               year, tm->tm_mon + 1, tm->tm_mday, tm->tm_hour, tm->tm_min, tm->tm_sec );
    gnc_tm_free (tm);
    return datebuf;
}
#pragma GCC diagnostic warning "-Wformat-nonliteral"

static void
load_timespec( const GncSqlBackend* be, GncSqlRow* row,
               /*@ null @*/ QofSetterFunc setter, gpointer pObject,
               const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    Timespec ts = {0, 0};
    TimespecSetterFunc ts_setter;
    gboolean isOK = FALSE;

    g_return_if_fail( be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( table_row->gobj_param_name != NULL || setter != NULL );

    ts_setter = (TimespecSetterFunc)setter;
    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if ( val == NULL )
    {
        isOK = TRUE;
    }
    else
    {
        if ( G_VALUE_HOLDS_INT64( val ) )
        {
	    timespecFromTime64 (&ts, (time64)(g_value_get_int64 (val)));
	    isOK = TRUE;
	}
	else if (G_VALUE_HOLDS_STRING (val))
	{
            const gchar* s = g_value_get_string( val );
            if ( s != NULL )
            {
                gchar* buf;
                buf = g_strdup_printf( "%c%c%c%c-%c%c-%c%c %c%c:%c%c:%c%c",
                                       s[0], s[1], s[2], s[3],
                                       s[4], s[5],
                                       s[6], s[7],
                                       s[8], s[9],
                                       s[10], s[11],
                                       s[12], s[13] );
                ts = gnc_iso8601_to_timespec_gmt( buf );
                g_free( buf );
                isOK = TRUE;
            }
        }
        else
        {
            PWARN( "Unknown timespec type: %s", G_VALUE_TYPE_NAME( val ) );
        }
    }
    if ( isOK )
    {
        if (table_row->gobj_param_name != NULL)
        {
	if (QOF_IS_INSTANCE (pObject))
	    qof_instance_increase_editlevel (QOF_INSTANCE (pObject));
            g_object_set( pObject, table_row->gobj_param_name, &ts, NULL );
	    if (QOF_IS_INSTANCE (pObject))
	    qof_instance_decrease_editlevel (QOF_INSTANCE (pObject));
        }
        else
        {
            (*ts_setter)( pObject, ts );
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

    info = create_column_info( table_row, BCT_DATETIME, TIMESPEC_COL_SIZE, FALSE );

    *pList = g_list_append( *pList, info );
}

static void
add_gvalue_timespec_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                              const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    TimespecAccessFunc ts_getter;
    Timespec ts;
    gchar* datebuf;
    GValue* value;

    g_return_if_fail( be != NULL );
    g_return_if_fail( obj_name != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( pList != NULL );

    if ( table_row->gobj_param_name != NULL )
    {
        Timespec* pts;
        g_object_get( pObject, table_row->gobj_param_name, &pts, NULL );
        ts = *pts;
    }
    else
    {
        ts_getter = (TimespecAccessFunc)gnc_sql_get_getter( obj_name, table_row );
        g_return_if_fail( ts_getter != NULL );
        ts = (*ts_getter)( pObject );
    }

    value = g_new0( GValue, 1 );
    g_assert( value != NULL );
    (void)g_value_init( value, G_TYPE_STRING );
    if ( ts.tv_sec != 0 || ts.tv_nsec != 0 )
    {
        datebuf = gnc_sql_convert_timespec_to_string( be, ts );
        g_value_take_string( value, datebuf );
    }

    (*pList) = g_slist_append( (*pList), value );
}

static GncSqlColumnTypeHandler timespec_handler
=
{
    load_timespec,
    add_timespec_col_info_to_list,
    gnc_sql_add_colname_to_list,
    add_gvalue_timespec_to_slist
};
/* ----------------------------------------------------------------- */
#define DATE_COL_SIZE 8

static void
load_date( const GncSqlBackend* be, GncSqlRow* row,
           /*@ null @*/ QofSetterFunc setter, gpointer pObject,
           const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;

    g_return_if_fail( be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( table_row->gobj_param_name != NULL || setter != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if ( val != NULL )
    {
	if (G_VALUE_HOLDS_INT64 (val))
	{
	    gint64 time = g_value_get_int64 (val);
	    Timespec ts = {time, 0};
	    struct tm tm;
	    gint day, month, year;
	    GDate date = timespec_to_gdate(ts);
	    if ( table_row->gobj_param_name != NULL )
	    {
		if (QOF_IS_INSTANCE (pObject))
		    qof_instance_increase_editlevel (QOF_INSTANCE (pObject));
		g_object_set( pObject, table_row->gobj_param_name, date, NULL );
		if (QOF_IS_INSTANCE (pObject))
		    qof_instance_increase_editlevel (QOF_INSTANCE (pObject));
	    }
	    else
	    {
		(*setter)( pObject, &date );
	    }
	}
        else if ( G_VALUE_HOLDS_STRING( val ) )
        {
            // Format of date is YYYYMMDD
            const gchar* s = g_value_get_string( val );
	    GDate *date;
            if ( s != NULL )
            {
                gchar buf[5];
                GDateDay day;
                GDateMonth month;
                GDateYear year;

                strncpy( buf, &s[0], 4 );
                buf[4] = '\0';
                year = (GDateYear)atoi( buf );
                strncpy( buf, &s[4], 2 );
                buf[2] = '\0';
                month = static_cast<decltype(month)>(atoi( buf ));
                strncpy( buf, &s[6], 2 );
                day = (GDateDay)atoi( buf );

                if ( year != 0 || month != 0 || day != (GDateDay)0 )
                {
                    date = g_date_new_dmy( day, month, year );
                    if ( table_row->gobj_param_name != NULL )
                    {
			if (QOF_IS_INSTANCE (pObject))
			    qof_instance_increase_editlevel (QOF_INSTANCE (pObject));
                        g_object_set (pObject, table_row->gobj_param_name,
				      date, NULL);
			if (QOF_IS_INSTANCE (pObject))
			    qof_instance_increase_editlevel (QOF_INSTANCE (pObject));
                    }
                    else
                    {
                        (*setter)( pObject, date );
                    }
                    g_date_free( date );
                }
            }
        }
        else
        {
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

    info = create_column_info( table_row, BCT_DATE, DATE_COL_SIZE, FALSE );

    *pList = g_list_append( *pList, info );
}

static void
add_gvalue_date_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                          const gpointer pObject,
                          const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    GDate* date = NULL;
    QofAccessFunc getter;
    gchar* buf;
    GValue* value;

    g_return_if_fail( be != NULL );
    g_return_if_fail( obj_name != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );

    value = g_new0( GValue, 1 );
    g_assert( value != NULL );
    (void)g_value_init( value, G_TYPE_STRING );
    if ( table_row->gobj_param_name != NULL )
    {
        g_object_get( pObject, table_row->gobj_param_name, &date, NULL );
    }
    else
    {
        getter = gnc_sql_get_getter( obj_name, table_row );
        if ( getter != NULL )
        {
            date = (GDate*)(*getter)( pObject, NULL );
        }
    }
    if ( date && g_date_valid( date ) )
    {
        buf = g_strdup_printf( "%04d%02d%02d",
                               g_date_get_year( date ), g_date_get_month( date ), g_date_get_day( date ) );
        g_value_take_string( value, buf );
    }

    (*pList) = g_slist_append( (*pList), value );
}

static GncSqlColumnTypeHandler date_handler
=
{
    load_date,
    add_date_col_info_to_list,
    gnc_sql_add_colname_to_list,
    add_gvalue_date_to_slist
};
/* ----------------------------------------------------------------- */
typedef gnc_numeric (*NumericGetterFunc)( const gpointer );
typedef void (*NumericSetterFunc)( gpointer, gnc_numeric );

static const GncSqlColumnTableEntry numeric_col_table[] =
{
    /*@ -full_init_block @*/
    { "num",    CT_INT64, 0, COL_NNUL, "guid" },
    { "denom",  CT_INT64, 0, COL_NNUL, "guid" },
    { NULL }
    /*@ +full_init_block @*/
};

static void
load_numeric( const GncSqlBackend* be, GncSqlRow* row,
              /*@ null @*/ QofSetterFunc setter, gpointer pObject,
              const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    gchar* buf;
    gint64 num, denom;
    gnc_numeric n;
    gboolean isNull = FALSE;

    g_return_if_fail( be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( table_row->gobj_param_name != NULL || setter != NULL );

    buf = g_strdup_printf( "%s_num", table_row->col_name );
    val = gnc_sql_row_get_value_at_col_name( row, buf );
    g_free( buf );
    if ( val == NULL )
    {
        isNull = TRUE;
        num = 0;
    }
    else
    {
        num = gnc_sql_get_integer_value( val );
    }
    buf = g_strdup_printf( "%s_denom", table_row->col_name );
    val = gnc_sql_row_get_value_at_col_name( row, buf );
    g_free( buf );
    if ( val == NULL )
    {
        isNull = TRUE;
        denom = 1;
    }
    else
    {
        denom = gnc_sql_get_integer_value( val );
    }
    n = gnc_numeric_create( num, denom );
    if ( !isNull )
    {
        if ( table_row->gobj_param_name != NULL )
        {
	    if (QOF_IS_INSTANCE (pObject))
		qof_instance_increase_editlevel (QOF_INSTANCE (pObject));
            g_object_set( pObject, table_row->gobj_param_name, &n, NULL );
	    if (QOF_IS_INSTANCE (pObject))
		qof_instance_increase_editlevel (QOF_INSTANCE (pObject));
        }
        else
        {
            NumericSetterFunc n_setter = (NumericSetterFunc)setter;
            (*n_setter)( pObject, n );
        }
    }
}

static void
add_numeric_col_info_to_list( const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,
                              GList** pList )
{
    GncSqlColumnInfo* info;
    gchar* buf;
    const GncSqlColumnTableEntry* subtable_row;

    g_return_if_fail( be != NULL );
    g_return_if_fail( table_row != NULL );
    g_return_if_fail( pList != NULL );

    for ( subtable_row = numeric_col_table; subtable_row->col_name != NULL; subtable_row++ )
    {
        buf = g_strdup_printf( "%s_%s", table_row->col_name, subtable_row->col_name );
        info = g_new0( GncSqlColumnInfo, 1 );
        g_assert( info != NULL );
        info->name = buf;
        info->type = BCT_INT64;
        info->is_primary_key = ((table_row->flags & COL_PKEY) != 0) ? TRUE : FALSE;
        info->null_allowed = ((table_row->flags & COL_NNUL) != 0) ? FALSE : TRUE;
        info->is_unicode = FALSE;
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

    if ( table_row->gobj_param_name != NULL )
    {
        gnc_numeric *s;
        g_object_get( pObject, table_row->gobj_param_name, &s, NULL );
        n = *s;
    }
    else
    {
        getter = (NumericGetterFunc)gnc_sql_get_getter( obj_name, table_row );
        if ( getter != NULL )
        {
            n = (*getter)( pObject );
        }
        else
        {
            n = gnc_numeric_zero();
        }
    }

    num_value = g_new0( GValue, 1 );
    g_assert( num_value != NULL );
    (void)g_value_init( num_value, G_TYPE_INT64 );
    g_value_set_int64( num_value, gnc_numeric_num( n ) );
    denom_value = g_new0( GValue, 1 );
    g_assert( denom_value != NULL );
    (void)g_value_init( denom_value, G_TYPE_INT64 );
    g_value_set_int64( denom_value, gnc_numeric_denom( n ) );

    (*pList) = g_slist_append( (*pList), num_value );
    (*pList) = g_slist_append( (*pList), denom_value );
}

static GncSqlColumnTypeHandler numeric_handler
= { load_numeric,
    add_numeric_col_info_to_list,
    add_numeric_colname_to_list,
    add_gvalue_numeric_to_slist
  };
/* ================================================================= */

static /*@ null @*//*@ only @*/ GHashTable* g_columnTypeHash = NULL;

void
gnc_sql_register_col_type_handler( const gchar* colType, const GncSqlColumnTypeHandler* handler )
{
    g_return_if_fail( colType != NULL );
    g_return_if_fail( handler != NULL );

    if ( g_columnTypeHash == NULL )
    {
        g_columnTypeHash = g_hash_table_new( g_str_hash, g_str_equal );
        g_assert( g_columnTypeHash != NULL );
    }

    DEBUG( "Col type %s registered\n", colType );
    g_hash_table_insert( g_columnTypeHash, (gpointer)colType, (gpointer)handler );
}

/*@ dependent @*//*@ null @*/ static GncSqlColumnTypeHandler*
get_handler( const GncSqlColumnTableEntry* table_row )
{
    GncSqlColumnTypeHandler* pHandler;

    g_return_val_if_fail( table_row != NULL, NULL );
    g_return_val_if_fail( table_row->col_type != NULL, NULL );

    if ( g_columnTypeHash != NULL )
    {
        pHandler = static_cast<decltype(pHandler)>(
            g_hash_table_lookup(g_columnTypeHash, table_row->col_type));
        g_assert( pHandler != NULL );
    }
    else
    {
        pHandler = NULL;
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
_retrieve_guid_( gpointer pObject, /*@ null @*/ gpointer pValue )
{
    GncGUID* pGuid = (GncGUID*)pObject;
    GncGUID* guid = (GncGUID*)pValue;

    g_return_if_fail( pObject != NULL );
    g_return_if_fail( pValue != NULL );

    memcpy( pGuid, guid, sizeof( GncGUID ) );
}


// Table to retrieve just the guid
static GncSqlColumnTableEntry guid_table[] =
{
    /*@ -full_init_block @*/
    { "guid", CT_GUID, 0, 0, NULL, NULL, NULL, _retrieve_guid_ },
    { NULL }
    /*@ +full_init_block @*/
};

/*@ null @*/
const GncGUID*
gnc_sql_load_guid( const GncSqlBackend* be, GncSqlRow* row )
{
    static GncGUID guid;

    g_return_val_if_fail( be != NULL, NULL );
    g_return_val_if_fail( row != NULL, NULL );

    gnc_sql_load_object( be, row, NULL, &guid, guid_table );

    return &guid;
}

// Table to retrieve just the guid
static GncSqlColumnTableEntry tx_guid_table[] =
{
    /*@ -full_init_block @*/
    { "tx_guid", CT_GUID, 0, 0, NULL, NULL, NULL, _retrieve_guid_ },
    { NULL }
    /*@ +full_init_block @*/
};

/*@ null @*//*@ dependent @*/
const GncGUID*
gnc_sql_load_tx_guid( const GncSqlBackend* be, GncSqlRow* row )
{
    static GncGUID guid;

    g_return_val_if_fail( be != NULL, NULL );
    g_return_val_if_fail( row != NULL, NULL );

    gnc_sql_load_object( be, row, NULL, &guid, tx_guid_table );

    return &guid;
}

void
gnc_sql_load_object( const GncSqlBackend* be, GncSqlRow* row,
                     /*@ null @*/ QofIdTypeConst obj_name, gpointer pObject,
                     const GncSqlColumnTableEntry* table )
{
    QofSetterFunc setter;
    GncSqlColumnTypeHandler* pHandler;
    const GncSqlColumnTableEntry* table_row;

    g_return_if_fail( be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table != NULL );

    for ( table_row = table; table_row->col_name != NULL; table_row++ )
    {
        if ( (table_row->flags & COL_AUTOINC) != 0 )
        {
            setter = set_autoinc_id;
        }
        else if ( table_row->qof_param_name != NULL )
        {
            g_assert( obj_name != NULL );
            setter = qof_class_get_parameter_setter( obj_name,
                     table_row->qof_param_name );
        }
        else
        {
            setter = table_row->setter;
        }
        pHandler = get_handler( table_row );
        g_assert( pHandler != NULL );
        pHandler->load_fn( be, row, setter, pObject, table_row );
    }
}

/* ================================================================= */
/*@ null @*/ GncSqlStatement*
gnc_sql_create_select_statement( GncSqlBackend* be, const gchar* table_name )
{
    gchar* sql;
    GncSqlStatement* stmt;

    g_return_val_if_fail( be != NULL, NULL );
    g_return_val_if_fail( table_name != NULL, NULL );

    sql = g_strdup_printf( "SELECT * FROM %s", table_name );
    stmt = gnc_sql_create_statement_from_sql( be, sql );
    g_free( sql );
    return stmt;
}

/*@ null @*/ static GncSqlStatement*
create_single_col_select_statement( GncSqlBackend* be,
                                    const gchar* table_name,
                                    const GncSqlColumnTableEntry* table_row )
{
    gchar* sql;
    GncSqlStatement* stmt;

    g_return_val_if_fail( be != NULL, NULL );
    g_return_val_if_fail( table_name != NULL, NULL );
    g_return_val_if_fail( table_row != NULL, NULL );

    sql = g_strdup_printf( "SELECT %s FROM %s", table_row->col_name, table_name );
    stmt = gnc_sql_create_statement_from_sql( be, sql );
    g_free( sql );
    return stmt;
}

/* ================================================================= */

/*@ null @*/ GncSqlResult*
gnc_sql_execute_select_statement( GncSqlBackend* be, GncSqlStatement* stmt )
{
    GncSqlResult* result;

    g_return_val_if_fail( be != NULL, NULL );
    g_return_val_if_fail( stmt != NULL, NULL );

    result = gnc_sql_connection_execute_select_statement( be->conn, stmt );
    if ( result == NULL )
    {
        PERR( "SQL error: %s\n", gnc_sql_statement_to_sql( stmt ) );
        qof_backend_set_error( &be->be, ERR_BACKEND_SERVER_ERR );
    }

    return result;
}

/*@ null @*/ GncSqlStatement*
gnc_sql_create_statement_from_sql( GncSqlBackend* be, const gchar* sql )
{
    GncSqlStatement* stmt;

    g_return_val_if_fail( be != NULL, NULL );
    g_return_val_if_fail( sql != NULL, NULL );

    stmt = gnc_sql_connection_create_statement_from_sql( be->conn, sql );
    if ( stmt == NULL )
    {
        PERR( "SQL error: %s\n", sql );
        qof_backend_set_error( &be->be, ERR_BACKEND_SERVER_ERR );
    }

    return stmt;
}

/*@ null @*/ GncSqlResult*
gnc_sql_execute_select_sql( GncSqlBackend* be, const gchar* sql )
{
    GncSqlStatement* stmt;
    GncSqlResult* result = NULL;

    g_return_val_if_fail( be != NULL, NULL );
    g_return_val_if_fail( sql != NULL, NULL );

    stmt = gnc_sql_create_statement_from_sql( be, sql );
    if ( stmt == NULL )
    {
        return NULL;
    }
    result = gnc_sql_connection_execute_select_statement( be->conn, stmt );
    gnc_sql_statement_dispose( stmt );
    if ( result == NULL )
    {
        PERR( "SQL error: %s\n", sql );
        qof_backend_set_error( &be->be, ERR_BACKEND_SERVER_ERR );
    }

    return result;
}

gint
gnc_sql_execute_nonselect_sql( GncSqlBackend* be, const gchar* sql )
{
    GncSqlStatement* stmt;
    gint result;

    g_return_val_if_fail( be != NULL, 0 );
    g_return_val_if_fail( sql != NULL, 0 );

    stmt = gnc_sql_create_statement_from_sql( be, sql );
    if ( stmt == NULL )
    {
        return -1;
    }
    result = gnc_sql_connection_execute_nonselect_statement( be->conn, stmt );
    gnc_sql_statement_dispose( stmt );
    return result;
}

static guint
execute_statement_get_count( GncSqlBackend* be, GncSqlStatement* stmt )
{
    GncSqlResult* result;
    guint count = 0;

    g_return_val_if_fail( be != NULL, 0 );
    g_return_val_if_fail( stmt != NULL, 0 );

    result = gnc_sql_execute_select_statement( be, stmt );
    if ( result != NULL )
    {
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

    if ( list == NULL ) return 0;

    for ( count = 0; list != NULL && count < maxCount; list = list->next, count++ )
    {
        QofInstance* inst = QOF_INSTANCE(list->data);
        (void)guid_to_string_buff( qof_instance_get_guid( inst ), guid_buf );

        if ( !first_guid )
        {
            (void)g_string_append( sql, "," );
        }
        (void)g_string_append( sql, "'" );
        (void)g_string_append( sql, guid_buf );
        (void)g_string_append( sql, "'" );
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
    guint count;
    GncSqlColumnTypeHandler* pHandler;
    GSList* list = NULL;

    g_return_val_if_fail( be != NULL, FALSE );
    g_return_val_if_fail( table_name != NULL, FALSE );
    g_return_val_if_fail( obj_name != NULL, FALSE );
    g_return_val_if_fail( pObject != NULL, FALSE );
    g_return_val_if_fail( table != NULL, FALSE );

    /* SELECT * FROM */
    sqlStmt = create_single_col_select_statement( be, table_name, table );
    g_assert( sqlStmt != NULL );

    /* WHERE */
    pHandler = get_handler( table );
    g_assert( pHandler != NULL );
    pHandler->add_gvalue_to_slist_fn( be, obj_name, pObject, table, &list );
    g_assert( list != NULL );
    gnc_sql_statement_add_where_cond( sqlStmt, obj_name, pObject, &table[0], (GValue*)(list->data) );

    count = execute_statement_get_count( be, sqlStmt );
    gnc_sql_statement_dispose( sqlStmt );
    if ( count == 0 )
    {
        return FALSE;
    }
    else
    {
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
    GncSqlStatement* stmt = NULL;
    gboolean ok = FALSE;

    g_return_val_if_fail( be != NULL, FALSE );
    g_return_val_if_fail( table_name != NULL, FALSE );
    g_return_val_if_fail( obj_name != NULL, FALSE );
    g_return_val_if_fail( pObject != NULL, FALSE );
    g_return_val_if_fail( table != NULL, FALSE );

    if ( op == OP_DB_INSERT )
    {
        stmt = build_insert_statement( be, table_name, obj_name, pObject, table );
    }
    else if ( op == OP_DB_UPDATE )
    {
        stmt = build_update_statement( be, table_name, obj_name, pObject, table );
    }
    else if ( op == OP_DB_DELETE )
    {
        stmt = build_delete_statement( be, table_name, obj_name, pObject, table );
    }
    else
    {
        g_assert( FALSE );
    }
    if ( stmt != NULL )
    {
        gint result;

        result = gnc_sql_connection_execute_nonselect_statement( be->conn, stmt );
        if ( result == -1 )
        {
            PERR( "SQL error: %s\n", gnc_sql_statement_to_sql( stmt ) );
            qof_backend_set_error( &be->be, ERR_BACKEND_SERVER_ERR );
        }
        else
        {
            ok = TRUE;
        }
        gnc_sql_statement_dispose( stmt );
    }

    return ok;
}

static GSList*
create_gslist_from_values( GncSqlBackend* be,
                           QofIdTypeConst obj_name, gpointer pObject,
                           const GncSqlColumnTableEntry* table )
{
    GSList* list = NULL;
    GncSqlColumnTypeHandler* pHandler;
    const GncSqlColumnTableEntry* table_row;

    for ( table_row = table; table_row->col_name != NULL; table_row++ )
    {
        if (( table_row->flags & COL_AUTOINC ) == 0 )
        {
            pHandler = get_handler( table_row );
            g_assert( pHandler != NULL );
            pHandler->add_gvalue_to_slist_fn( be, obj_name, pObject, table_row, &list );
        }
    }

    g_assert( list != NULL );
    return list;
}

gchar*
gnc_sql_get_sql_value( const GncSqlConnection* conn, const GValue* value )
{
    if ( value != NULL && G_IS_VALUE( value ) )
    {
        GType type = G_VALUE_TYPE(value);

        if ( G_VALUE_HOLDS_STRING(value) )
        {
            if ( g_value_get_string( value ) != NULL )
            {
                gchar* before_str;
                gchar* after_str;
                before_str = g_value_dup_string( value );
                after_str = gnc_sql_connection_quote_string( conn, before_str );
                g_free( before_str );
                return after_str;
            }
            else
            {
                return g_strdup( "NULL" );
            }
        }
        else if ( type == G_TYPE_INT64 )
        {
            return g_strdup_printf( "%" G_GINT64_FORMAT, g_value_get_int64( value ) );

        }
        else if ( type == G_TYPE_INT )
        {
            return g_strdup_printf( "%d", g_value_get_int( value ) );

        }
        else if ( type == G_TYPE_DOUBLE )
        {
            gchar doublestr[G_ASCII_DTOSTR_BUF_SIZE];
            g_ascii_dtostr( doublestr, sizeof(doublestr),
                            g_value_get_double( value ));
            return g_strdup( doublestr );

        }
        else if ( g_value_type_transformable( type, G_TYPE_STRING ) )
        {
            GValue* string;
            gchar* str;

            string = g_new0( GValue, 1 );
            g_assert( string != NULL );
            (void)g_value_init( string, G_TYPE_STRING );
            (void)g_value_transform( value, string );
            str = g_value_dup_string( string );
            g_value_unset( string );
            g_free( string );
            PWARN( "using g_value_transform(), gtype = '%s'\n", g_type_name( type ) );
            return str;
        }
        else
        {
            PWARN( "not transformable, gtype = '%s'\n", g_type_name( type ) );
            return g_strdup( "$$$" );
        }
    }
    else
    {
        PWARN( "value is NULL or not G_IS_VALUE()\n" );
        return g_strdup( "" );
    }
}

static void
free_gvalue_list( GSList* list )
{
    GSList* node;
    GValue* value;

    for ( node = list; node != NULL; node = node->next )
    {
        value = (GValue*)node->data;

        g_value_unset( value );
        g_free( value );
    }
    g_slist_free( list );
}

/*@ null @*/ static GncSqlStatement*
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
    GList* colnames = NULL;
    GList* colname;
    const GncSqlColumnTableEntry* table_row;

    g_return_val_if_fail( be != NULL, NULL );
    g_return_val_if_fail( table_name != NULL, NULL );
    g_return_val_if_fail( obj_name != NULL, NULL );
    g_return_val_if_fail( pObject != NULL, NULL );
    g_return_val_if_fail( table != NULL, NULL );

    sqlbuf = g_strdup_printf( "INSERT INTO %s(", table_name );
    sql = g_string_new( sqlbuf );
    g_free( sqlbuf );

    // Get all col names and all values
    for ( table_row = table; table_row->col_name != NULL; table_row++ )
    {
        if (( table_row->flags & COL_AUTOINC ) == 0 )
        {
            GncSqlColumnTypeHandler* pHandler;

            // Add col names to the list
            pHandler = get_handler( table_row );
            g_assert( pHandler != NULL );
            pHandler->add_colname_to_list_fn( table_row, &colnames );
        }
    }
    g_assert( colnames != NULL );

    for ( colname = colnames; colname != NULL; colname = colname->next )
    {
        if ( colname != colnames )
        {
            g_string_append( sql, "," );
        }
        g_string_append( sql, (gchar*)colname->data );
        g_free( colname->data );
    }
    g_list_free( colnames );

    g_string_append( sql, ") VALUES(" );
    values = create_gslist_from_values( be, obj_name, pObject, table );
    for ( node = values; node != NULL; node = node->next )
    {
        GValue* value = (GValue*)node->data;
        gchar* value_str;
        if ( node != values )
        {
            (void)g_string_append( sql, "," );
        }
        value_str = gnc_sql_get_sql_value( be->conn, value );
        (void)g_string_append( sql, value_str );
        g_free( value_str );
        (void)g_value_reset( value );
    }
    free_gvalue_list( values );
    (void)g_string_append( sql, ")" );

    stmt = gnc_sql_connection_create_statement_from_sql( be->conn, sql->str );
    (void)g_string_free( sql, TRUE );

    return stmt;
}

/*@ null @*/ static GncSqlStatement*
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
    const GncSqlColumnTableEntry* table_row;
    gchar* sqlbuf;

    g_return_val_if_fail( be != NULL, NULL );
    g_return_val_if_fail( table_name != NULL, NULL );
    g_return_val_if_fail( obj_name != NULL, NULL );
    g_return_val_if_fail( pObject != NULL, NULL );
    g_return_val_if_fail( table != NULL, NULL );

    // Get all col names and all values
    for ( table_row = table; table_row->col_name != NULL; table_row++ )
    {
        if (( table_row->flags & COL_AUTOINC ) == 0 )
        {
            GncSqlColumnTypeHandler* pHandler;

            // Add col names to the list
            pHandler = get_handler( table_row );
            g_assert( pHandler != NULL );
            pHandler->add_colname_to_list_fn( table_row, &colnames );
        }
    }
    g_assert( colnames != NULL );
    values = create_gslist_from_values( be, obj_name, pObject, table );

    // Create the SQL statement
    sqlbuf = g_strdup_printf( "UPDATE %s SET ", table_name );
    sql = g_string_new( sqlbuf );
    g_free( sqlbuf );

    firstCol = TRUE;
    for ( colname = colnames->next, value = values->next;
            colname != NULL && value != NULL;
            colname = colname->next, value = value->next )
    {
        gchar* value_str;
        if ( !firstCol )
        {
            (void)g_string_append( sql, "," );
        }
        (void)g_string_append( sql, (gchar*)colname->data );
        (void)g_string_append( sql, "=" );
        value_str = gnc_sql_get_sql_value( be->conn, (GValue*)(value->data) );
        (void)g_string_append( sql, value_str );
        g_free( value_str );
        firstCol = FALSE;
    }
    for ( colname = colnames; colname != NULL; colname = colname->next )
    {
        g_free( colname->data );
    }
    g_list_free( colnames );
    if ( value != NULL || colname != NULL )
    {
        PERR( "Mismatch in number of column names and values" );
    }

    stmt = gnc_sql_connection_create_statement_from_sql( be->conn, sql->str );
    gnc_sql_statement_add_where_cond( stmt, obj_name, pObject, &table[0], (GValue*)(values->data) );
    free_gvalue_list( values );
    (void)g_string_free( sql, TRUE );

    return stmt;
}

/*@ null @*/ static GncSqlStatement*
build_delete_statement( GncSqlBackend* be,
                        const gchar* table_name,
                        QofIdTypeConst obj_name, gpointer pObject,
                        const GncSqlColumnTableEntry* table )
{
    GncSqlStatement* stmt;
    GncSqlColumnTypeHandler* pHandler;
    GSList* list = NULL;
    gchar* sqlbuf;

    g_return_val_if_fail( be != NULL, NULL );
    g_return_val_if_fail( table_name != NULL, NULL );
    g_return_val_if_fail( obj_name != NULL, NULL );
    g_return_val_if_fail( pObject != NULL, NULL );
    g_return_val_if_fail( table != NULL, NULL );

    sqlbuf = g_strdup_printf( "DELETE FROM %s ", table_name );
    stmt = gnc_sql_connection_create_statement_from_sql( be->conn, sqlbuf );
    g_free( sqlbuf );

    /* WHERE */
    pHandler = get_handler( table );
    g_assert( pHandler != NULL );
    pHandler->add_gvalue_to_slist_fn( be, obj_name, pObject, table, &list );
    g_assert( list != NULL );
    gnc_sql_statement_add_where_cond( stmt, obj_name, pObject, &table[0], (GValue*)(list->data) );
    free_gvalue_list( list );

    return stmt;
}

/* ================================================================= */
gboolean
gnc_sql_commit_standard_item( GncSqlBackend* be, QofInstance* inst, const gchar* tableName,
                              QofIdTypeConst obj_name, const GncSqlColumnTableEntry* col_table )
{
    const GncGUID* guid;
    gboolean is_infant;
    E_DB_OPERATION op;
    gboolean is_ok;

    is_infant = qof_instance_get_infant( inst );
    if ( qof_instance_get_destroying( inst ) )
    {
        op = OP_DB_DELETE;
    }
    else if ( be->is_pristine_db || is_infant )
    {
        op = OP_DB_INSERT;
    }
    else
    {
        op = OP_DB_UPDATE;
    }
    is_ok = gnc_sql_do_db_operation( be, op, tableName, obj_name, inst, col_table );

    if ( is_ok )
    {
        // Now, commit any slots
        guid = qof_instance_get_guid( inst );
        if ( !qof_instance_get_destroying(inst) )
        {
            is_ok = gnc_sql_slots_save( be, guid, is_infant, inst);
        }
        else
        {
            is_ok = gnc_sql_slots_delete( be, guid );
        }
    }

    return is_ok;
}

/* ================================================================= */

static gboolean
do_create_table( const GncSqlBackend* be, const gchar* table_name,
                 const GncSqlColumnTableEntry* col_table )
{
    GList* col_info_list = NULL;
    gboolean ok = FALSE;

    g_return_val_if_fail( be != NULL, FALSE );
    g_return_val_if_fail( table_name != NULL, FALSE );
    g_return_val_if_fail( col_table != NULL, FALSE );

    for ( ; col_table->col_name != NULL; col_table++ )
    {
        GncSqlColumnTypeHandler* pHandler;

        pHandler = get_handler( col_table );
        g_assert( pHandler != NULL );
        pHandler->add_col_info_to_list_fn( be, col_table, &col_info_list );
    }
    g_assert( col_info_list != NULL );
    ok = gnc_sql_connection_create_table( be->conn, table_name, col_info_list );
    return ok;
}

gboolean
gnc_sql_create_table( GncSqlBackend* be, const gchar* table_name,
                      gint table_version, const GncSqlColumnTableEntry* col_table )
{
    gboolean ok;

    g_return_val_if_fail( be != NULL, FALSE );
    g_return_val_if_fail( table_name != NULL, FALSE );
    g_return_val_if_fail( col_table != NULL, FALSE );

    DEBUG( "Creating %s table\n", table_name );

    ok = do_create_table( be, table_name, col_table );
    if ( ok )
    {
        ok = gnc_sql_set_table_version( be, table_name, table_version );
    }
    return ok;
}

gboolean
gnc_sql_create_temp_table( const GncSqlBackend* be, const gchar* table_name,
                           const GncSqlColumnTableEntry* col_table )
{
    g_return_val_if_fail( be != NULL, FALSE );
    g_return_val_if_fail( table_name != NULL, FALSE );
    g_return_val_if_fail( col_table != NULL, FALSE );

    return do_create_table( be, table_name, col_table );
}

gboolean
gnc_sql_create_index( const GncSqlBackend* be, const gchar* index_name,
                      const gchar* table_name,
                      const GncSqlColumnTableEntry* col_table )
{
    gboolean ok;

    g_return_val_if_fail( be != NULL, FALSE );
    g_return_val_if_fail( index_name != NULL, FALSE );
    g_return_val_if_fail( table_name != NULL, FALSE );
    g_return_val_if_fail( col_table != NULL, FALSE );

    ok = gnc_sql_connection_create_index( be->conn, index_name, table_name,
                                          col_table );
    return ok;
}

gint
gnc_sql_get_table_version( const GncSqlBackend* be, const gchar* table_name )
{
    g_return_val_if_fail( be != NULL, 0 );
    g_return_val_if_fail( table_name != NULL, 0 );

    /* If the db is pristine because it's being saved, the table does not exist. */
    if ( be->is_pristine_db )
    {
        return 0;
    }

    return GPOINTER_TO_INT(g_hash_table_lookup( be->versions, table_name ));
}

/* Create a temporary table, copy the data from the old table, delete the
   old table, then rename the new one. */
void
gnc_sql_upgrade_table( GncSqlBackend* be, const gchar* table_name,
                       const GncSqlColumnTableEntry* col_table )
{
    gchar* sql;
    gchar* temp_table_name;

    g_return_if_fail( be != NULL );
    g_return_if_fail( table_name != NULL );
    g_return_if_fail( col_table != NULL );

    DEBUG( "Upgrading %s table\n", table_name );

    temp_table_name = g_strdup_printf( "%s_new", table_name );
    (void)gnc_sql_create_temp_table( be, temp_table_name, col_table );
    sql = g_strdup_printf( "INSERT INTO %s SELECT * FROM %s",
                           temp_table_name, table_name );
    (void)gnc_sql_execute_nonselect_sql( be, sql );
    g_free( sql );

    sql = g_strdup_printf( "DROP TABLE %s", table_name );
    (void)gnc_sql_execute_nonselect_sql( be, sql );
    g_free( sql );

    sql = g_strdup_printf( "ALTER TABLE %s RENAME TO %s", temp_table_name, table_name );
    (void)gnc_sql_execute_nonselect_sql( be, sql );
    g_free( sql );
    g_free( temp_table_name );
}

/* Adds one or more columns to an existing table. */
gboolean gnc_sql_add_columns_to_table( GncSqlBackend* be, const gchar* table_name,
                                       const GncSqlColumnTableEntry* new_col_table )
{
    GList* col_info_list = NULL;
    gboolean ok = FALSE;

    g_return_val_if_fail( be != NULL, FALSE );
    g_return_val_if_fail( table_name != NULL, FALSE );
    g_return_val_if_fail( new_col_table != NULL, FALSE );

    for ( ; new_col_table->col_name != NULL; new_col_table++ )
    {
        GncSqlColumnTypeHandler* pHandler;

        pHandler = get_handler( new_col_table );
        g_assert( pHandler != NULL );
        pHandler->add_col_info_to_list_fn( be, new_col_table, &col_info_list );
    }
    g_assert( col_info_list != NULL );
    ok = gnc_sql_connection_add_columns_to_table( be->conn, table_name, col_info_list );
    return ok;
}

/* ================================================================= */
#define VERSION_TABLE_NAME "versions"
#define MAX_TABLE_NAME_LEN 50
#define TABLE_COL_NAME "table_name"
#define VERSION_COL_NAME "table_version"

static GncSqlColumnTableEntry version_table[] =
{
    /*@ -full_init_block @*/
    { TABLE_COL_NAME,   CT_STRING, MAX_TABLE_NAME_LEN, COL_PKEY | COL_NNUL },
    { VERSION_COL_NAME, CT_INT,    0,                  COL_NNUL },
    { NULL }
    /*@ +full_init_block @*/
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

    if ( be->versions != NULL )
    {
        g_hash_table_destroy( be->versions );
    }
    be->versions = g_hash_table_new_full( g_str_hash, g_str_equal, g_free, NULL );

    if ( gnc_sql_connection_does_table_exist( be->conn, VERSION_TABLE_NAME ) )
    {
        GncSqlResult* result;
        gchar* sql;

        sql = g_strdup_printf( "SELECT * FROM %s", VERSION_TABLE_NAME );
        result = gnc_sql_execute_select_sql( be, sql );
        g_free( sql );
        if ( result != NULL )
        {
            const GValue* name;
            const GValue* version;
            GncSqlRow* row;

            row = gnc_sql_result_get_first_row( result );
            while ( row != NULL )
            {
                name = gnc_sql_row_get_value_at_col_name( row, TABLE_COL_NAME );
                version = gnc_sql_row_get_value_at_col_name( row, VERSION_COL_NAME );
                g_hash_table_insert( be->versions,
                                     g_strdup( g_value_get_string( name ) ),
                                     GINT_TO_POINTER((gint)g_value_get_int64( version )) );
                row = gnc_sql_result_get_next_row( result );
            }
            gnc_sql_result_dispose( result );
        }
    }
    else
    {
        do_create_table( be, VERSION_TABLE_NAME, version_table );
	gnc_sql_set_table_version( be, "Gnucash",
				   gnc_prefs_get_long_version() );
	gnc_sql_set_table_version( be, "Gnucash-Resave",
				   GNUCASH_RESAVE_VERSION );
    }
}

/**
 * Resets the version table information by removing all version table info.
 * It also recreates the version table in the db.
 *
 * @param be Backend struct
 * @return TRUE if successful, FALSE if error
 */
static gboolean
reset_version_info( GncSqlBackend* be )
{
    gboolean ok;

    g_return_val_if_fail( be != NULL, FALSE );

    ok = do_create_table( be, VERSION_TABLE_NAME, version_table );
    if ( be->versions == NULL )
    {
        be->versions = g_hash_table_new_full( g_str_hash, g_str_equal, g_free, NULL );
    }
    else
    {
        g_hash_table_remove_all( be->versions );
    }

    gnc_sql_set_table_version( be, "Gnucash", gnc_prefs_get_long_version() );
    gnc_sql_set_table_version( be, "Gnucash-Resave", GNUCASH_RESAVE_VERSION );
    return ok;
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

    if ( be->versions != NULL )
    {
        g_hash_table_destroy( be->versions );
        be->versions = NULL;
    }
}

/**
 * Registers the version for a table.  Registering involves updating the
 * db version table and also the hash table.
 *
 * @param be Backend struct
 * @param table_name Table name
 * @param version Version number
 * @return TRUE if successful, FALSE if unsuccessful
 */
gboolean
gnc_sql_set_table_version( GncSqlBackend* be, const gchar* table_name, gint version )
{
    gchar* sql;
    gint cur_version;
    gint status;

    g_return_val_if_fail( be != NULL, FALSE );
    g_return_val_if_fail( table_name != NULL, FALSE );
    g_return_val_if_fail( version > 0, FALSE );

    cur_version = gnc_sql_get_table_version( be, table_name );
    if ( cur_version != version )
    {
        if ( cur_version == 0 )
        {
            sql = g_strdup_printf( "INSERT INTO %s VALUES('%s',%d)", VERSION_TABLE_NAME,
                                   table_name, version );
        }
        else
        {
            sql = g_strdup_printf( "UPDATE %s SET %s=%d WHERE %s='%s'", VERSION_TABLE_NAME,
                                   VERSION_COL_NAME, version,
                                   TABLE_COL_NAME, table_name );
        }
        status = gnc_sql_execute_nonselect_sql( be, sql );
        if ( status == -1 )
        {
            PERR( "SQL error: %s\n", sql );
            qof_backend_set_error( &be->be, ERR_BACKEND_SERVER_ERR );
        }
        g_free( sql );
    }

    g_hash_table_insert( be->versions, g_strdup( table_name ), GINT_TO_POINTER(version) );

    return TRUE;
}

/* ========================== END OF FILE ===================== */
