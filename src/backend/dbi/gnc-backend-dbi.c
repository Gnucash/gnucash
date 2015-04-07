/********************************************************************
 * gnc-backend-dbi.c: load and save data to SQL via libdbi          *
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
/** @file gnc-backend-dbi.c
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db using libdbi
 */

#include "config.h"

#include <errno.h>
#include <glib.h>
#include <glib/gstdio.h>
#if !HAVE_GMTIME_R
#include "gmtime_r.h"
#endif

#include "gnc-backend-dbi-priv.h"

#include "qof.h"
#include "qofquery-p.h"
#include "qofquerycore-p.h"
#include "Account.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "SX-book.h"
#include "Recurrence.h"

#include "gnc-uri-utils.h"
#include "gnc-filepath-utils.h"
#include <gnc-path.h>
#include "gnc-locale-utils.h"

#include "gnc-backend-dbi.h"

#include "gnc-prefs.h"

#ifdef S_SPLINT_S
#include "splint-defs.h"
#endif

#ifdef G_OS_WIN32
#include <winsock2.h>
#define GETPID() GetCurrentProcessId()
#else
#include <limits.h>
#include <unistd.h>
#define GETPID() getpid()
#endif

#if LIBDBI_VERSION >= 900
#define HAVE_LIBDBI_R 1
static dbi_inst dbi_instance = NULL;
#else
#define HAVE_LIBDBI_R 0
#endif

/* For direct access to dbi data structs, sadly needed for datetime */
#include <dbi/dbi-dev.h>

#define GNC_HOST_NAME_MAX 255
#define TRANSACTION_NAME "trans"

static QofLogModule log_module = G_LOG_DOMAIN;

static gchar lock_table[] = "gnclock";

#define FILE_URI_TYPE "file"
#define FILE_URI_PREFIX (FILE_URI_TYPE "://")
#define SQLITE3_URI_TYPE "sqlite3"
#define SQLITE3_URI_PREFIX (SQLITE3_URI_TYPE "://")
#define PGSQL_DEFAULT_PORT 5432

static /*@ null @*/ gchar* conn_create_table_ddl_sqlite3( GncSqlConnection* conn,
        const gchar* table_name,
        const GList* col_info_list );
static GSList* conn_get_table_list( dbi_conn conn, const gchar* dbname );
static GSList* conn_get_table_list_sqlite3( dbi_conn conn, const gchar* dbname );
static void append_sqlite3_col_def( GString* ddl, GncSqlColumnInfo* info );
static GSList *conn_get_index_list_sqlite3( dbi_conn conn );
static void conn_drop_index_sqlite3 (dbi_conn conn, const gchar *index );
static provider_functions_t provider_sqlite3 =
{
    conn_create_table_ddl_sqlite3,
    conn_get_table_list_sqlite3,
    append_sqlite3_col_def,
    conn_get_index_list_sqlite3,
    conn_drop_index_sqlite3
};
#define SQLITE3_TIMESPEC_STR_FORMAT "%04d%02d%02d%02d%02d%02d"

static /*@ null @*/ gchar* conn_create_table_ddl_mysql( GncSqlConnection* conn,
        const gchar* table_name,
        const GList* col_info_list );
static void append_mysql_col_def( GString* ddl, GncSqlColumnInfo* info );
static GSList *conn_get_index_list_mysql( dbi_conn conn );
static void conn_drop_index_mysql (dbi_conn conn, const gchar *index );
static provider_functions_t provider_mysql =
{
    conn_create_table_ddl_mysql,
    conn_get_table_list,
    append_mysql_col_def,
    conn_get_index_list_mysql,
    conn_drop_index_mysql
};
#define MYSQL_TIMESPEC_STR_FORMAT "%04d%02d%02d%02d%02d%02d"

static /*@ null @*/ gchar* conn_create_table_ddl_pgsql( GncSqlConnection* conn,
        const gchar* table_name,
        const GList* col_info_list );
static GSList* conn_get_table_list_pgsql( dbi_conn conn, const gchar* dbname );
static void append_pgsql_col_def( GString* ddl, GncSqlColumnInfo* info );
static GSList *conn_get_index_list_pgsql( dbi_conn conn );
static void conn_drop_index_pgsql (dbi_conn conn, const gchar *index );

static provider_functions_t provider_pgsql =
{
    conn_create_table_ddl_pgsql,
    conn_get_table_list_pgsql,
    append_pgsql_col_def,
    conn_get_index_list_pgsql,
    conn_drop_index_pgsql
};
#define PGSQL_TIMESPEC_STR_FORMAT "%04d%02d%02d %02d%02d%02d"

static gboolean gnc_dbi_lock_database( QofBackend *qbe, gboolean ignore_lock );
static void gnc_dbi_unlock( QofBackend *qbe );
static gboolean save_may_clobber_data( QofBackend* qbe );

static /*@ null @*/ gchar* create_index_ddl( GncSqlConnection* conn,
        const gchar* index_name,
        const gchar* table_name,
        const GncSqlColumnTableEntry* col_table );
static /*@ null @*/ gchar* add_columns_ddl( GncSqlConnection* conn,
        const gchar* table_name,
        GList* col_info_list );
static GncSqlConnection* create_dbi_connection( /*@ observer @*/ provider_functions_t* provider, /*@ observer @*/ QofBackend* qbe, /*@ observer @*/ dbi_conn conn );
static GncDbiTestResult conn_test_dbi_library( dbi_conn conn );
#define GNC_DBI_PROVIDER_SQLITE (&provider_sqlite3)
#define GNC_DBI_PROVIDER_MYSQL (&provider_mysql)
#define GNC_DBI_PROVIDER_PGSQL (&provider_pgsql)


#define DBI_MAX_CONN_ATTEMPTS 5

/* ================================================================= */

/* Free the contents of a GSList, then free the list. Don't use this
 * if the elements of the list weren't created with g_new! */
static void
gnc_table_slist_free( GSList *table_list )
{
    GSList *list;
    for ( list = table_list; list != NULL; list = g_slist_next( list ))
    {
        g_free( list->data );
    }
    g_slist_free( table_list );
}

static void
gnc_dbi_set_error( GncDbiSqlConnection* dbi_conn, gint last_error,
                   gint error_repeat, gboolean retry )
{
    g_return_if_fail( dbi_conn != NULL );

    dbi_conn->last_error = last_error;
    if ( error_repeat > 0 )
        dbi_conn->error_repeat = dbi_conn->error_repeat + error_repeat;
    else
        dbi_conn->error_repeat = 0;
    dbi_conn->retry = retry;
}

static void
gnc_dbi_init_error( GncDbiSqlConnection* dbi_conn )
{
    gnc_dbi_set_error( dbi_conn, ERR_BACKEND_NO_ERR, 0, FALSE );
}

/* Check if the dbi connection is valid. If not attempt to re-establish it
 * Returns TRUE is there is a valid connection in the end or FALSE otherwise
 */
static gboolean
gnc_dbi_verify_conn( GncDbiSqlConnection* dbi_conn )
{
    if ( dbi_conn->conn_ok )
        return TRUE;

    /* We attempt to connect only once here. The error function will automatically
     * re-attempt up until DBI_MAX_CONN_ATTEMPTS time to connect if this call fails.
     * After all these attempts, conn_ok will indicate if there is a valid connection
     * or not.
     */
    gnc_dbi_init_error( dbi_conn );
    dbi_conn->conn_ok = TRUE;
    (void)dbi_conn_connect( dbi_conn->conn );

    return dbi_conn->conn_ok;
}

/* ================================================================= */

static void
create_tables_cb( const gchar* type, gpointer data_p, gpointer be_p )
{
    GncSqlObjectBackend* pData = data_p;
    GncDbiBackend* be = be_p;

    g_return_if_fail( type != NULL && data_p != NULL && be_p != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );

    if ( pData->create_tables != NULL )
    {
        (pData->create_tables)( &be->sql_be );
    }
}

static void
sqlite3_error_fn( dbi_conn conn, /*@ unused @*/ void* user_data )
{
    const gchar* msg;

    (void)dbi_conn_error( conn, &msg );
    PERR( "DBI error: %s\n", msg );
    gnc_dbi_set_error( conn, ERR_BACKEND_MISC, 0, FALSE );
}

static void
gnc_dbi_sqlite3_session_begin( QofBackend *qbe, QofSession *session,
                               const gchar *book_id, gboolean ignore_lock,
                               gboolean create, gboolean force )
{
    GncDbiBackend *be = (GncDbiBackend*)qbe;
    gint result;
    gchar* dirname = NULL;
    gchar* basename = NULL;
    gchar *filepath = NULL;
    gchar *msg = " ";
    gboolean file_exists;
    GncDbiTestResult dbi_test_result = GNC_DBI_PASS;

    g_return_if_fail( qbe != NULL );
    g_return_if_fail( session != NULL );
    g_return_if_fail( book_id != NULL );

    ENTER (" ");

    /* Remove uri type if present */
    filepath = gnc_uri_get_path ( book_id );
    file_exists = g_file_test( filepath,
                               G_FILE_TEST_IS_REGULAR | G_FILE_TEST_EXISTS );
    if ( !create && !file_exists )
    {
        qof_backend_set_error( qbe, ERR_FILEIO_FILE_NOT_FOUND );
        qof_backend_set_message(qbe, "Sqlite3 file %s not found", filepath);
        PWARN ("Sqlite3 file %s not found", filepath);
        goto exit;
    }

    if ( create && !force && file_exists )
    {
        qof_backend_set_error (qbe, ERR_BACKEND_STORE_EXISTS);
        msg = "Might clobber, no force";
        PWARN ("%s", msg);
        goto exit;
    }


    if ( be->conn != NULL )
    {
        dbi_conn_close( be->conn );
    }

    #if HAVE_LIBDBI_R
    if (dbi_instance)
        be->conn = dbi_conn_new_r( "sqlite3", dbi_instance );
    else
        PERR ("Attempt to connect with an uninitialized dbi_instance");
    #else
    be->conn = dbi_conn_new( "sqlite3" );
    #endif

    if ( be->conn == NULL )
    {
        PERR( "Unable to create sqlite3 dbi connection\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_BAD_URL );
        goto exit;
    }

    dirname = g_path_get_dirname( filepath );
    basename = g_path_get_basename( filepath );
    dbi_conn_error_handler( be->conn, sqlite3_error_fn, be );
    /* dbi-sqlite3 documentation says that sqlite3 doesn't take a "host" option */
    result = dbi_conn_set_option( be->conn, "host", "localhost" );
    if ( result < 0 )
    {
        PERR( "Error setting 'host' option\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
        goto exit;
    }
    result = dbi_conn_set_option( be->conn, "dbname", basename );
    if ( result < 0 )
    {
        PERR( "Error setting 'dbname' option\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
        goto exit;
    }
    result = dbi_conn_set_option( be->conn, "sqlite3_dbdir", dirname );
    if ( result < 0 )
    {
        PERR( "Error setting 'sqlite3_dbdir' option\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
        goto exit;
    }
    result = dbi_conn_connect( be->conn );

    if ( result < 0 )
    {
        PERR( "Unable to connect to %s: %d\n", book_id, result );
        qof_backend_set_error( qbe, ERR_BACKEND_BAD_URL );
        goto exit;
    }

    dbi_test_result = conn_test_dbi_library( be->conn );
    switch ( dbi_test_result )
    {
    case GNC_DBI_PASS:
        break;

    case GNC_DBI_FAIL_SETUP:
        qof_backend_set_error( qbe, ERR_SQL_DBI_UNTESTABLE );
        qof_backend_set_message( qbe,
                                 "SQLite3: Failed to setup for large number test" );
        break;

    case GNC_DBI_FAIL_TEST:
        qof_backend_set_error( qbe, ERR_SQL_BAD_DBI );
        qof_backend_set_message( qbe,
                                 "SQLite3 DBI library fails large number test" );
        break;
    }
    if ( dbi_test_result != GNC_DBI_PASS )
    {
        if ( create && !file_exists ) /* File didn't exist before, but it */
        {
            /* does now, and we don't want to */
            dbi_conn_close( be->conn );/* leave it lying around. */
            be->conn = NULL;
            g_unlink( filepath );
        }
        msg = "Bad DBI Library";
        goto exit;
    }
    if ( !gnc_dbi_lock_database( qbe, ignore_lock ) )
    {
        qof_backend_set_error( qbe, ERR_BACKEND_LOCKED );
        msg = "Locked";
        goto exit;
    }

    if ( be->sql_be.conn != NULL )
    {
        gnc_sql_connection_dispose( be->sql_be.conn );
    }
    be->sql_be.conn = create_dbi_connection( GNC_DBI_PROVIDER_SQLITE, qbe, be->conn );
    be->sql_be.timespec_format = SQLITE3_TIMESPEC_STR_FORMAT;

    /* We should now have a proper session set up.
     * Let's start logging */
    xaccLogSetBaseName (filepath);
    PINFO ("logpath=%s", filepath ? filepath : "(null)");

exit:
    if ( filepath != NULL ) g_free ( filepath );
    if ( basename != NULL ) g_free( basename );
    if ( dirname != NULL ) g_free( dirname );
    LEAVE ( "%s", msg );
}

static GSList*
conn_get_index_list_sqlite3( dbi_conn conn )
{
    GSList *list = NULL;
    const gchar *errmsg;
    dbi_result result = dbi_conn_query( conn, "SELECT name FROM sqlite_master WHERE type = 'index' AND name NOT LIKE 'sqlite_autoindex%'" );
    if ( dbi_conn_error( conn, &errmsg ) != DBI_ERROR_NONE )
    {
        g_print( "Index Table Retrieval Error: %s\n", errmsg );
        return NULL;
    }
    while ( dbi_result_next_row( result ) != 0 )
    {
        const gchar* index_name;

        index_name = dbi_result_get_string_idx( result, 1 );
        list = g_slist_prepend( list, strdup( index_name ) );
    }
    dbi_result_free( result );
    return list;
}

static void
conn_drop_index_sqlite3 (dbi_conn conn, const gchar *index )
{
    dbi_result result = dbi_conn_queryf (conn, "DROP INDEX %s", index);
    if ( result )
        dbi_result_free( result );
}

static void
mysql_error_fn( dbi_conn conn, void* user_data )
{
    GncDbiBackend *be = (GncDbiBackend*)user_data;
    GncDbiSqlConnection *dbi_conn = (GncDbiSqlConnection*)be->sql_be.conn;
    const gchar* msg;
    gint err_num;
#ifdef G_OS_WIN32
    const guint backoff_msecs = 1;
#else
    const guint backoff_usecs = 1000;
#endif

    err_num = dbi_conn_error( conn, &msg );

    /* Note: the sql connection may not have been initialized yet
     *       so let's be careful with using it
     */

    /* Database doesn't exist. When this error is triggered the
     * GncDbiSqlConnection may not exist yet either, so don't use it here
     */
    if ( err_num == 1049 )          // Database doesn't exist
    {
        PINFO( "DBI error: %s\n", msg );
        be->exists = FALSE;
        return;
    }

    /* All the other error handling code assumes the GncDbiSqlConnection
     *  has been initialized. So let's assert it exits here, otherwise
     * simply return.
     */
    if (!dbi_conn)
    {
        PINFO( "DBI error: %s\n", msg );
        PINFO( "Note: GbcDbiSqlConnection not yet initialized. Skipping further error processing." );
        return;
    }

    /* Test for other errors */
    if ( err_num == 2006 )     // Server has gone away
    {
        PINFO( "DBI error: %s - Reconnecting...\n", msg );
        if (dbi_conn)
            gnc_dbi_set_error( dbi_conn, ERR_BACKEND_CONN_LOST, 1, TRUE );
        dbi_conn->conn_ok = TRUE;
        (void)dbi_conn_connect( conn );
    }
    else if ( err_num == 2003 )     // Unable to connect
    {
        if (dbi_conn->error_repeat >= DBI_MAX_CONN_ATTEMPTS )
        {
            PERR( "DBI error: %s - Giving up after %d consecutive attempts.\n", msg, DBI_MAX_CONN_ATTEMPTS );
            if (dbi_conn)
                gnc_dbi_set_error( dbi_conn, ERR_BACKEND_CANT_CONNECT, 0, FALSE );
            dbi_conn->conn_ok = FALSE;
        }
        else
        {
#ifdef G_OS_WIN32
	    Sleep (backoff_msecs * 2 << dbi_conn->error_repeat);
#else
	    usleep (backoff_usecs * 2 << dbi_conn->error_repeat);
#endif
            PINFO( "DBI error: %s - Reconnecting...\n", msg );
            if (dbi_conn)
                gnc_dbi_set_error( dbi_conn, ERR_BACKEND_CANT_CONNECT, 1, TRUE );
            dbi_conn->conn_ok = TRUE;
            (void)dbi_conn_connect( conn );
        }
    }
    else                            // Any other error
    {
        PERR( "DBI error: %s\n", msg );
        if (dbi_conn)
            gnc_dbi_set_error( dbi_conn, ERR_BACKEND_MISC, 0, FALSE );
    }
}

/**
 * Sets standard db options in a dbi_conn.
 *
 * @param qbe QOF backend
 * @param conn dbi_conn connection
 * @param host Hostname
 * @param port Port number
 * @param dbname Database name
 * @param username User name
 * @param password Password
 * @return TRUE if successful, FALSE if error
 */
static gboolean
set_standard_connection_options( QofBackend* qbe, dbi_conn conn, const gchar* host, int port,
                                 const gchar* dbname, const gchar* username, const gchar* password )
{
    gint result;

    result = dbi_conn_set_option( conn, "host", host );
    if ( result < 0 )
    {
        PERR( "Error setting 'host' option\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
        return FALSE;
    }
    result = dbi_conn_set_option_numeric( conn, "port", port );
    if ( result < 0 )
    {
        PERR( "Error setting 'port' option\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
        return FALSE;
    }
    result = dbi_conn_set_option( conn, "dbname", dbname );
    if ( result < 0 )
    {
        PERR( "Error setting 'dbname' option\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
        return FALSE;
    }
    result = dbi_conn_set_option( conn, "username", username );
    if ( result < 0 )
    {
        PERR( "Error setting 'username' option\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
        return FALSE;
    }
    result = dbi_conn_set_option( conn, "password", password );
    if ( result < 0 )
    {
        PERR( "Error setting 'password' option\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
        return FALSE;
    }

    result = dbi_conn_set_option( conn, "encoding", "UTF-8" );
    if ( result < 0 )
    {
        PERR( "Error setting 'encoding' option\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
        return FALSE;
    }

    return TRUE;
}


static gboolean
gnc_dbi_lock_database ( QofBackend* qbe, gboolean ignore_lock )
{

    GncDbiBackend *qe = (GncDbiBackend*)qbe;
    dbi_conn dcon = qe->conn;
    dbi_result result;
    const gchar *dbname = dbi_conn_get_option( dcon, "dbname" );
    /* Create the table if it doesn't exist */
    result = dbi_conn_get_table_list( dcon, dbname, lock_table);
    if (!( result && dbi_result_get_numrows( result ) ))
    {
        if ( result )
        {
            dbi_result_free( result );
            result = NULL;
        }
        result = dbi_conn_queryf( dcon, "CREATE TABLE %s ( Hostname varchar(%d), PID int )", lock_table, GNC_HOST_NAME_MAX );
        if ( dbi_conn_error( dcon, NULL ) )
        {
            const gchar *errstr;
            dbi_conn_error( dcon, &errstr );
            PERR( "Error %s creating lock table", errstr );
            qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
            if ( result )
            {
                dbi_result_free( result );
                result = NULL;
            }
            return FALSE;
        }
        if ( result )
        {
            dbi_result_free( result );
            result = NULL;
        }
    }
    if (result)
    {
        dbi_result_free( result );
        result = NULL;
    }

    /* Protect everything with a single transaction to prevent races */
    if ( (result = dbi_conn_query( dcon, "BEGIN" )) )
    {
        /* Check for an existing entry; delete it if ignore_lock is true, otherwise fail */
        gchar hostname[ GNC_HOST_NAME_MAX + 1 ];
        if (result)
        {
            dbi_result_free( result );
            result = NULL;
        }
        result = dbi_conn_queryf( dcon, "SELECT * FROM %s", lock_table );
        if ( result && dbi_result_get_numrows( result ) )
        {
            dbi_result_free( result );
            result = NULL;
            if ( !ignore_lock )
            {
                qof_backend_set_error( qbe, ERR_BACKEND_LOCKED );
                /* FIXME: After enhancing the qof_backend_error mechanism, report in the dialog what is the hostname of the machine holding the lock. */
                dbi_conn_query( dcon, "ROLLBACK" );
                return FALSE;
            }
            result = dbi_conn_queryf( dcon, "DELETE FROM %s", lock_table );
            if ( !result)
            {
                qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
                qof_backend_set_message( qbe, "Failed to delete lock record" );
                result = dbi_conn_query( dcon, "ROLLBACK" );
                if (result)
                {
                    dbi_result_free( result );
                    result = NULL;
                }
                return FALSE;
            }
            if (result)
            {
                dbi_result_free( result );
                result = NULL;
            }
        }
        /* Add an entry and commit the transaction */
        memset( hostname, 0, sizeof(hostname) );
        gethostname( hostname, GNC_HOST_NAME_MAX );
        result = dbi_conn_queryf( dcon,
                                  "INSERT INTO %s VALUES ('%s', '%d')",
                                  lock_table, hostname, (int)GETPID() );
        if ( !result)
        {
            qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
            qof_backend_set_message( qbe, "Failed to create lock record" );
            result = dbi_conn_query( dcon, "ROLLBACK" );
            if (result)
            {
                dbi_result_free( result );
                result = NULL;
            }
            return FALSE;
        }
        if (result)
        {
            dbi_result_free( result );
            result = NULL;
        }
        result = dbi_conn_query( dcon, "COMMIT" );
        if (result)
        {
            dbi_result_free( result );
            result = NULL;
        }
        return TRUE;
    }
    /* Couldn't get a transaction (probably couldn't get a lock), so fail */
    qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
    qof_backend_set_message( qbe, "SQL Backend failed to obtain a transaction" );
    if (result)
    {
        dbi_result_free( result );
        result = NULL;
    }
    return FALSE;
}
static void
gnc_dbi_unlock( QofBackend *qbe )
{
    GncDbiBackend *qe = (GncDbiBackend*)qbe;
    dbi_conn dcon = qe->conn;
    dbi_result result;
    const gchar *dbname = NULL;

    g_return_if_fail( dcon != NULL );
    g_return_if_fail( dbi_conn_error( dcon, NULL ) == 0 );

    dbname = dbi_conn_get_option( dcon, "dbname" );
    /* Check if the lock table exists */
    g_return_if_fail( dbname != NULL );
    result = dbi_conn_get_table_list( dcon, dbname, lock_table);
    if (!( result && dbi_result_get_numrows( result ) ))
    {
        if (result)
        {
            dbi_result_free( result );
            result = NULL;
        }
        PWARN("No lock table in database, so not unlocking it.");
        return;
    }
    dbi_result_free( result );

    result = dbi_conn_query( dcon, "BEGIN" );
    if ( result )
    {
        /* Delete the entry if it's our hostname and PID */
        gchar hostname[ GNC_HOST_NAME_MAX + 1 ];

        dbi_result_free( result );
        result = NULL;
        memset( hostname, 0, sizeof(hostname) );
        gethostname( hostname, GNC_HOST_NAME_MAX );
        result = dbi_conn_queryf( dcon, "SELECT * FROM %s WHERE Hostname = '%s' AND PID = '%d'", lock_table, hostname, (int)GETPID() );
        if ( result && dbi_result_get_numrows( result ) )
        {
            if (result)
            {
                dbi_result_free( result );
                result = NULL;
            }
            result = dbi_conn_queryf( dcon, "DELETE FROM %s", lock_table );
            if ( !result)
            {
                PERR("Failed to delete the lock entry");
                qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
                result = dbi_conn_query( dcon, "ROLLBACK" );
                if (result)
                {
                    dbi_result_free( result );
                    result = NULL;
                }
                return;
            }
            else
            {
                dbi_result_free( result );
                result = NULL;
            }
            result = dbi_conn_query( dcon, "COMMIT" );
            if (result)
            {
                dbi_result_free( result );
                result = NULL;
            }
            return;
        }
        result = dbi_conn_query( dcon, "ROLLBACK" );
        if (result)
        {
            dbi_result_free( result );
            result = NULL;
        }
        PWARN("There was no lock entry in the Lock table");
        return;
    }
    if (result)
    {
        dbi_result_free( result );
        result = NULL;
    }
    PWARN("Unable to get a lock on LOCK, so failed to clear the lock entry.");
    qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
}

static void
gnc_dbi_mysql_session_begin( QofBackend* qbe, QofSession *session,
                             const gchar *book_id, gboolean ignore_lock,
                             gboolean create, gboolean force )
{
    GncDbiBackend *be = (GncDbiBackend*)qbe;
    gchar* protocol = NULL;
    gchar* host = NULL;
    gchar* dbname = NULL;
    gchar* username = NULL;
    gchar* password = NULL;
    gchar* basename = NULL;
    gchar* translog_path = NULL;
    gint portnum = 0;
    gint result;
    gboolean success = FALSE;
    GncDbiTestResult dbi_test_result = GNC_DBI_PASS;

    g_return_if_fail( qbe != NULL );
    g_return_if_fail( session != NULL );
    g_return_if_fail( book_id != NULL );

    ENTER (" ");

    /* Split the book-id
     * Format is protocol://username:password@hostname:port/dbname
       where username, password and port are optional) */
    gnc_uri_get_components ( book_id, &protocol, &host, &portnum,
                             &username, &password, &dbname );

    // Try to connect to the db.  If it doesn't exist and the create
    // flag is TRUE, we'll need to connect to the 'mysql' db and execute the
    // CREATE DATABASE ddl statement there.
    if ( be->conn != NULL )
    {
        dbi_conn_close( be->conn );
    }
#if HAVE_LIBDBI_R
    if (dbi_instance)
        be->conn = dbi_conn_new_r( "mysql", dbi_instance );
    else
        PERR ("Attempt to connect with an uninitialized dbi_instance");
#else
    be->conn = dbi_conn_new( "mysql" );
#endif
    if ( be->conn == NULL )
    {
        PERR( "Unable to create mysql dbi connection\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_BAD_URL );
        goto exit;
    }
    dbi_conn_error_handler( be->conn, mysql_error_fn, be );
    if ( !set_standard_connection_options( qbe, be->conn, host, portnum, dbname, username, password ) )
    {
        goto exit;
    }
    be->exists = TRUE;
    result = dbi_conn_connect( be->conn );
    if ( result == 0 )
    {
        dbi_test_result = conn_test_dbi_library( be->conn );
        switch ( dbi_test_result )
        {
        case GNC_DBI_PASS:
            break;

        case GNC_DBI_FAIL_SETUP:
            qof_backend_set_error( qbe, ERR_SQL_DBI_UNTESTABLE );
            qof_backend_set_message( qbe,
                                     "DBI library large number test incomplete" );
            break;

        case GNC_DBI_FAIL_TEST:
            qof_backend_set_error( qbe, ERR_SQL_BAD_DBI );
            qof_backend_set_message( qbe,
                                     "DBI library fails large number test" );
            break;
        }
        if ( GNC_DBI_PASS != dbi_test_result )
        {
            goto exit;
        }
        if (create && !force && save_may_clobber_data( qbe ) )
        {
            qof_backend_set_error ( qbe, ERR_BACKEND_STORE_EXISTS );
            PWARN("Databse already exists, Might clobber it.");
            goto exit;
        }

        success = gnc_dbi_lock_database ( qbe, ignore_lock );
    }
    else
    {

        if ( be->exists )
        {
            PERR( "Unable to connect to database '%s'\n", dbname );
            qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
            goto exit;
        }

        // The db does not already exist.  Connect to the 'mysql' db and try to create it.
        if ( create )
        {
            dbi_result dresult;
            result = dbi_conn_set_option( be->conn, "dbname", "mysql" );
            if ( result < 0 )
            {
                PERR( "Error setting 'dbname' option\n" );
                qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
                goto exit;
            }
            result = dbi_conn_connect( be->conn );
            if ( result < 0 )
            {
                PERR( "Unable to connect to 'mysql' database\n" );
                qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
                goto exit;
            }
            dresult = dbi_conn_queryf( be->conn, "CREATE DATABASE %s CHARACTER SET utf8", dbname );
            if ( dresult == NULL )
            {
                PERR( "Unable to create database '%s'\n", dbname );
                qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
                goto exit;
            }
            dbi_conn_close( be->conn );

            // Try again to connect to the db
            #if HAVE_LIBDBI_R
	    if (dbi_instance)
	        be->conn = dbi_conn_new_r( "mysql", dbi_instance );
	    else
	        PERR ("Attempt to connect with an uninitialized dbi_instance");
            #else
            be->conn = dbi_conn_new( "mysql" );
            #endif

            if ( be->conn == NULL )
            {
                PERR( "Unable to create mysql dbi connection\n" );
                qof_backend_set_error( qbe, ERR_BACKEND_BAD_URL );
                goto exit;
            }
            dbi_conn_error_handler( be->conn, mysql_error_fn, be );
            if ( !set_standard_connection_options( qbe, be->conn, host, 0, dbname, username, password ) )
            {
                goto exit;
            }
            result = dbi_conn_connect( be->conn );
            if ( result < 0 )
            {
                PERR( "Unable to create database '%s'\n", dbname );
                qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
                goto exit;
            }
            dbi_test_result = conn_test_dbi_library( be->conn );
            switch ( dbi_test_result )
            {
            case GNC_DBI_PASS:
                break;

            case GNC_DBI_FAIL_SETUP:
                qof_backend_set_error( qbe, ERR_SQL_DBI_UNTESTABLE );
                qof_backend_set_message( qbe,
                                         "MySql: Failed to setup for large number test" );
                break;

            case GNC_DBI_FAIL_TEST:
                qof_backend_set_error( qbe, ERR_SQL_BAD_DBI );
                qof_backend_set_message( qbe,
                                         "MySql DBI library fails large number test" );
                break;
            }
            if ( dbi_test_result != GNC_DBI_PASS )
            {
                dbi_conn_queryf( be->conn, "DROP DATABASE %s", dbname );
                goto exit;
            }
            success = gnc_dbi_lock_database ( qbe, ignore_lock );
        }
        else
        {
            qof_backend_set_error( qbe, ERR_BACKEND_NO_SUCH_DB );
            qof_backend_set_message( qbe, "Database %s not found", dbname );
        }
    }

    if ( success )
    {
        dbi_result dresult;

        if ( be->sql_be.conn != NULL )
        {
            gnc_sql_connection_dispose( be->sql_be.conn );
        }
        be->sql_be.conn = create_dbi_connection( GNC_DBI_PROVIDER_MYSQL, qbe, be->conn );
    }
    be->sql_be.timespec_format = MYSQL_TIMESPEC_STR_FORMAT;

    /* We should now have a proper session set up.
     * Let's start logging */
    basename = g_strjoin("_", protocol, host, username, dbname, NULL);
    translog_path = gnc_build_translog_path (basename);
    xaccLogSetBaseName (translog_path);
    PINFO ("logpath=%s", translog_path ? translog_path : "(null)");

exit:
    g_free( protocol );
    g_free( host );
    g_free( username );
    g_free( password );
    g_free( basename );
    g_free( translog_path );
    g_free( dbname );

    LEAVE (" ");
}

static GSList*
conn_get_index_list_mysql( dbi_conn conn )
{
    GSList *index_list = NULL;
    dbi_result table_list;
    const char *errmsg;
    const gchar *dbname = dbi_conn_get_option( conn, "dbname" );
    g_return_val_if_fail( conn != NULL, NULL );
    table_list = dbi_conn_get_table_list( conn, dbname, NULL );
    if ( dbi_conn_error( conn, &errmsg ) != DBI_ERROR_NONE )
    {
        g_print( "Table Retrieval Error: %s\n", errmsg );
        return NULL;
    }
    while ( dbi_result_next_row( table_list ) != 0 )
    {
        dbi_result result;
        const gchar *table_name = dbi_result_get_string_idx( table_list, 1 );
        result = dbi_conn_queryf( conn,
                                  "SHOW INDEXES IN %s WHERE Key_name != 'PRIMARY'",
                                  table_name );
        if ( dbi_conn_error( conn, &errmsg ) != DBI_ERROR_NONE )
        {
            g_print( "Index Table Retrieval Error: %s\n", errmsg );
            continue;
        }

        while ( dbi_result_next_row( result ) != 0 )
        {
            const gchar*  index_name = dbi_result_get_string_idx( result, 3 );
            index_list = g_slist_prepend( index_list, g_strjoin( " ", index_name, table_name, NULL ) );
        }
        dbi_result_free( result );
    }

    return index_list;
}

static void
conn_drop_index_mysql (dbi_conn conn, const gchar *index )
{
    dbi_result result;
    gchar **index_table_split = g_strsplit (index, " ", 2);
    int splitlen = -1;

    /* Check if the index split can be valid */
    while (index_table_split[++splitlen] != NULL)
    { /* do nothing, just count split members */ }

    if (splitlen != 2)
    {
        g_print ("Drop index error: invalid MySQL index format (<index> <table>): %s", index);
        return;
    }

    result = dbi_conn_queryf (conn, "DROP INDEX %s ON %s",
                              index_table_split[0], index_table_split[1]);
    if ( result )
        dbi_result_free( result );

    g_strfreev (index_table_split);
}

static void
pgsql_error_fn( dbi_conn conn, void* user_data )
{
    GncDbiBackend *be = (GncDbiBackend*)user_data;
    GncDbiSqlConnection *dbi_conn = (GncDbiSqlConnection*)be->sql_be.conn;
    const gchar* msg;
#ifdef G_OS_WIN32
    const guint backoff_msecs = 1;
#else
    const guint backoff_usecs = 1000;
#endif

    (void)dbi_conn_error( conn, &msg );
    if ( g_str_has_prefix( msg, "FATAL:  database" ) &&
            g_str_has_suffix( msg, "does not exist\n" ) )
    {
        PINFO( "DBI error: %s\n", msg );
        be->exists = FALSE;
        gnc_dbi_set_error( dbi_conn, ERR_BACKEND_NO_SUCH_DB, 0, FALSE );
    }
    else if ( g_strrstr( msg, "server closed the connection unexpectedly" ) ) // Connection lost
    {
        if ( dbi_conn == NULL )
        {
            PWARN( "DBI Error: Connection lost, connection pointer invalid");
            return;
        }
        PINFO( "DBI error: %s - Reconnecting...\n", msg );
        gnc_dbi_set_error( dbi_conn, ERR_BACKEND_CONN_LOST, 1, TRUE );
        dbi_conn->conn_ok = TRUE;
        (void)dbi_conn_connect( conn );
    }
    else if ( dbi_conn &&
              ( g_str_has_prefix( msg, "connection pointer is NULL" ) ||
                g_str_has_prefix(msg, "could not connect to server" ) ) )     // No connection
    {
        if (dbi_conn->error_repeat >= DBI_MAX_CONN_ATTEMPTS )
        {
            PERR( "DBI error: %s - Giving up after %d consecutive attempts.\n", msg, DBI_MAX_CONN_ATTEMPTS );
            gnc_dbi_set_error( dbi_conn, ERR_BACKEND_CANT_CONNECT, 0, FALSE );
            dbi_conn->conn_ok = FALSE;
        }
        else
        {
#ifdef G_OS_WIN32
	    Sleep (backoff_msecs * 2 << dbi_conn->error_repeat);
#else
	    usleep (backoff_usecs * 2 << dbi_conn->error_repeat);
#endif
            PINFO( "DBI error: %s - Reconnecting...\n", msg );
            gnc_dbi_set_error( dbi_conn, ERR_BACKEND_CANT_CONNECT, 1, TRUE );
            dbi_conn->conn_ok = TRUE;
            (void)dbi_conn_connect( conn );
        }
    }
    else
    {
        PERR( "DBI error: %s\n", msg );
        gnc_dbi_set_error( dbi_conn, ERR_BACKEND_MISC, 0, FALSE );
    }
}

static void
gnc_dbi_postgres_session_begin( QofBackend *qbe, QofSession *session,
                                const gchar *book_id, gboolean ignore_lock,
                                gboolean create, gboolean force )
{
    GncDbiBackend *be = (GncDbiBackend*)qbe;
    gint result = 0;
    gchar* protocol = NULL;
    gchar* host = NULL;
    gchar *dbname = NULL, *dbnamelc = NULL;
    gchar* username = NULL;
    gchar* password = NULL;
    gchar* basename = NULL;
    gchar* translog_path = NULL;
    gboolean success = FALSE;
    gint portnum = 0;
    GncDbiTestResult dbi_test_result = GNC_DBI_PASS;

    g_return_if_fail( qbe != NULL );
    g_return_if_fail( session != NULL );
    g_return_if_fail( book_id != NULL );

    ENTER (" ");

    /* Split the book-id
     * Format is protocol://username:password@hostname:port/dbname
       where username, password and port are optional) */
    gnc_uri_get_components ( book_id, &protocol, &host, &portnum,
                             &username, &password, &dbname );
    if ( portnum == 0 )
        portnum = PGSQL_DEFAULT_PORT;
    /* Postgres's SQL interface coerces identifiers to lower case, but the
     * C interface is case-sensitive. This results in a mixed-case dbname
     * being created (with a lower case name) but then dbi can't conect to
     * it. To work around this, coerce the name to lowercase first. */
    dbnamelc = g_utf8_strdown( dbname, -1 );

    // Try to connect to the db.  If it doesn't exist and the create
    // flag is TRUE, we'll need to connect to the 'postgres' db and execute the
    // CREATE DATABASE ddl statement there.
    if ( be->conn != NULL )
    {
        dbi_conn_close( be->conn );
    }

    #if HAVE_LIBDBI_R
    if (dbi_instance)
        be->conn = dbi_conn_new_r( "pgsql", dbi_instance );
    else
        PERR ("Attempt to connect with an uninitialized dbi_instance");
    #else
    be->conn = dbi_conn_new( "pgsql" );
    #endif

    if ( be->conn == NULL )
    {
        PERR( "Unable to create pgsql dbi connection\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_BAD_URL );
        goto exit;
    }
    dbi_conn_error_handler( be->conn, pgsql_error_fn, be );
    if ( !set_standard_connection_options( qbe, be->conn, host, portnum, dbnamelc, username, password ) )
    {
        goto exit;
    }
    be->exists = TRUE;
    result = dbi_conn_connect( be->conn );
    if ( result == 0 )
    {
        dbi_test_result = conn_test_dbi_library( be->conn );
        switch ( dbi_test_result )
        {
        case GNC_DBI_PASS:
            break;

        case GNC_DBI_FAIL_SETUP:
            qof_backend_set_error( qbe, ERR_SQL_DBI_UNTESTABLE );
            qof_backend_set_message( qbe,
                                     "Postgresql: Failed to setup for large number test" );
            break;

        case GNC_DBI_FAIL_TEST:
            qof_backend_set_error( qbe, ERR_SQL_BAD_DBI );
            qof_backend_set_message( qbe,
                                     "Postgresql DBI library fails large number test" );
            break;
        }
        if ( dbi_test_result != GNC_DBI_PASS )
        {
            goto exit;
        }
        if (create && !force && save_may_clobber_data( qbe ) )
        {
            qof_backend_set_error ( qbe, ERR_BACKEND_STORE_EXISTS );
            PWARN("Databse already exists, Might clobber it.");
            goto exit;
        }

        success = gnc_dbi_lock_database ( qbe, ignore_lock );
    }
    else
    {

        if ( be->exists )
        {
            PERR( "Unable to connect to database '%s'\n", dbname );
            qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
            goto exit;
        }

        // The db does not already exist.  Connect to the 'postgres' db and try to create it.
        if ( create )
        {
            dbi_result dresult;
            result = dbi_conn_set_option( be->conn, "dbname", "postgres" );
            if ( result < 0 )
            {
                PERR( "Error setting 'dbname' option\n" );
                qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
                goto exit;
            }
            result = dbi_conn_connect( be->conn );
            if ( result < 0 )
            {
                PERR( "Unable to connect to 'postgres' database\n" );
                qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
                goto exit;
            }
            dresult = dbi_conn_queryf( be->conn, "CREATE DATABASE %s WITH TEMPLATE template0 ENCODING 'UTF8'", dbnamelc );
            if ( dresult == NULL )
            {
                PERR( "Unable to create database '%s'\n", dbname );
                qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
                goto exit;
            }
            dbi_conn_queryf( be->conn, "ALTER DATABASE %s SET standard_conforming_strings TO on", dbnamelc );
            dbi_conn_close( be->conn );

            // Try again to connect to the db
            #if HAVE_LIBDBI_R
            if (dbi_instance)
                be->conn = dbi_conn_new_r( "pgsql", dbi_instance );
            else
	        PERR ("Attempt to connect with an uninitialized dbi_instance");
            #else
            be->conn = dbi_conn_new( "pgsql" );
            #endif

            if ( be->conn == NULL )
            {
                PERR( "Unable to create pgsql dbi connection\n" );
                qof_backend_set_error( qbe, ERR_BACKEND_BAD_URL );
                goto exit;
            }
            dbi_conn_error_handler( be->conn, pgsql_error_fn, be );
            if ( !set_standard_connection_options( qbe, be->conn, host, PGSQL_DEFAULT_PORT, dbnamelc, username, password ) )
            {
                goto exit;
            }
            result = dbi_conn_connect( be->conn );
            if ( result < 0 )
            {
                PERR( "Unable to create database '%s'\n", dbname );
                qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
                goto exit;
            }
            dbi_test_result = conn_test_dbi_library( be->conn );
            switch ( dbi_test_result )
            {
            case GNC_DBI_PASS:
                break;

            case GNC_DBI_FAIL_SETUP:
                qof_backend_set_error( qbe, ERR_SQL_DBI_UNTESTABLE );
                qof_backend_set_message( qbe,
                                         "DBI library large number test incomplete" );
                break;

            case GNC_DBI_FAIL_TEST:
                qof_backend_set_error( qbe, ERR_SQL_BAD_DBI );
                qof_backend_set_message( qbe,
                                         "DBI library fails large number test" );
                break;
            }
            if ( GNC_DBI_PASS != dbi_test_result )
            {
                dbi_conn_select_db( be->conn, "template1" );
                dbi_conn_queryf( be->conn, "DROP DATABASE %s", dbnamelc );
                goto exit;
            }
            success = gnc_dbi_lock_database ( qbe, ignore_lock );
        }
        else
        {
            qof_backend_set_error( qbe, ERR_BACKEND_NO_SUCH_DB );
            qof_backend_set_message( qbe, "Database %s not found", dbname );
        }
    }
    if ( success )
    {
        if ( be->sql_be.conn != NULL )
        {
            gnc_sql_connection_dispose( be->sql_be.conn );
        }
        be->sql_be.conn = create_dbi_connection( GNC_DBI_PROVIDER_PGSQL, qbe, be->conn );
    }
    be->sql_be.timespec_format = PGSQL_TIMESPEC_STR_FORMAT;

    /* We should now have a proper session set up.
     * Let's start logging */
    basename = g_strjoin("_", protocol, host, username, dbname, NULL);
    translog_path = gnc_build_translog_path (basename);
    xaccLogSetBaseName (translog_path);
    PINFO ("logpath=%s", translog_path ? translog_path : "(null)");

exit:
    g_free( protocol );
    g_free( host );
    g_free( username );
    g_free( password );
    g_free( basename );
    g_free( translog_path );
    g_free( dbname );
    g_free( dbnamelc );

    LEAVE (" ");
}

static GSList*
conn_get_index_list_pgsql( dbi_conn conn )
{
    GSList *list = NULL;
    const gchar *errmsg;
    dbi_result result;
    PINFO ( "Retrieving postgres index list\n");
    result = dbi_conn_query( conn, "SELECT relname FROM pg_class AS a INNER JOIN pg_index AS b ON (b.indexrelid = a.oid) INNER JOIN pg_namespace AS c ON (a.relnamespace = c.oid) WHERE reltype = '0' AND indisprimary = 'f' AND nspname = 'public'" );
    if ( dbi_conn_error( conn, &errmsg ) != DBI_ERROR_NONE )
    {
        g_print( "Index Table Retrieval Error: %s\n", errmsg );
        return NULL;
    }
    while ( dbi_result_next_row( result ) != 0 )
    {
        const gchar* index_name;

        index_name = dbi_result_get_string_idx( result, 1 );
        list = g_slist_prepend( list, strdup( index_name ) );
    }
    dbi_result_free( result );
    return list;
}

static void
conn_drop_index_pgsql (dbi_conn conn, const gchar *index )
{
    dbi_result result = dbi_conn_queryf (conn, "DROP INDEX %s", index);
    if ( result )
        dbi_result_free( result );
}


/* ================================================================= */

static void
gnc_dbi_session_end( QofBackend *be_start )
{
    GncDbiBackend *be = (GncDbiBackend*)be_start;

    g_return_if_fail( be_start != NULL );

    ENTER (" ");

    if ( be->conn != NULL )
    {
        gnc_dbi_unlock( be_start );
        dbi_conn_close( be->conn );
        be->conn = NULL;
    }
    if ( be->sql_be.conn != NULL )
    {
        gnc_sql_connection_dispose( be->sql_be.conn );
        be->sql_be.conn = NULL;
    }
    gnc_sql_finalize_version_info( &be->sql_be );

    LEAVE (" ");
}

static void
gnc_dbi_destroy_backend( /*@ only @*/ QofBackend *be )
{
    g_return_if_fail( be != NULL );

    /* Stop transaction logging */
    xaccLogSetBaseName (NULL);

    qof_backend_destroy( be );

    g_free( be );
}

/* ================================================================= */

/* GNUCASH_RESAVE_VERSION indicates the earliest database version
 * compatible with this version of Gnucash; the stored value is the
 * earliest version of Gnucash conpatible with the database. If the
 * GNUCASH_RESAVE_VERSION for this Gnucash is newer than the Gnucash
 * version which created the database, a resave is offered. If the
 * version of this Gnucash is older than the saved resave version,
 * then the database will be loaded read-only. A resave will update
 * both values to match this version of Gnucash.
 */
static void
gnc_dbi_load( QofBackend* qbe, /*@ dependent @*/ QofBook *book, QofBackendLoadType loadType )
{
    GncDbiBackend *be = (GncDbiBackend*)qbe;

    g_return_if_fail( qbe != NULL );
    g_return_if_fail( book != NULL );

    ENTER( "be=%p, book=%p", be, book );

    if ( loadType == LOAD_TYPE_INITIAL_LOAD )
    {
        g_assert( be->primary_book == NULL );
        be->primary_book = book;

        // Set up table version information
        gnc_sql_init_version_info (&be->sql_be);

        // Call all object backends to create any required tables
        qof_object_foreach_backend( GNC_SQL_BACKEND, create_tables_cb, be );
    }

    gnc_sql_load( &be->sql_be, book, loadType );

    if ( GNUCASH_RESAVE_VERSION > gnc_sql_get_table_version( &be->sql_be, "Gnucash" ) )
    {
        /* The database was loaded with an older database schema or
         * data semantics. In order to ensure consistency, the whole
         * thing needs to be saved anew. */
        qof_backend_set_error( qbe, ERR_SQL_DB_TOO_OLD );
    }
    else if ( GNUCASH_RESAVE_VERSION < gnc_sql_get_table_version( &be->sql_be,
              "Gnucash-Resave"))
    {
        /* Worse, the database was created with a newer version. We
         * can't safely write to this database, so the user will have
         * to do a "save as" to make one that we can write to.
         */
        qof_backend_set_error( qbe, ERR_SQL_DB_TOO_NEW );
    }


    LEAVE( "" );
}

/* ================================================================= */

static gboolean
save_may_clobber_data( QofBackend* qbe )
{
    GncDbiBackend* be = (GncDbiBackend*)qbe;
    const gchar* dbname;
    dbi_result result;
    gboolean retval = FALSE;

    /* Data may be clobbered iff the number of tables != 0 */
    dbname = dbi_conn_get_option( be->conn, "dbname" );
    result = dbi_conn_get_table_list( be->conn, dbname, NULL );
    if ( result )
    {
        retval =  dbi_result_get_numrows( result ) > 0;
        dbi_result_free( result );
    }
    return retval;
}

static dbi_result
conn_table_manage_backup (GncDbiSqlConnection *conn,
                          gchar *table_name, TableOpType op )
{
    gchar *new_name = g_strdup_printf( "%s_%s", table_name, "back" );
    dbi_result result = NULL;
    switch ( op )
    {
    case backup:
        result = dbi_conn_queryf( conn->conn, "ALTER TABLE %s RENAME TO %s",
                                  table_name, new_name );
        break;
    case rollback:
        result = dbi_conn_queryf( conn->conn,
                                  "ALTER TABLE %s RENAME TO %s",
                                  new_name, table_name );
        break;
    case drop_backup:
        result = dbi_conn_queryf( conn->conn, "DROP TABLE %s",
                                  new_name );
        break;
    default:
        break;
    }
    g_free( new_name );
    return result;
}

/**
 * Perform a specified SQL operation on every table in a
 * database. Possible operations are:
 * * drop: to DROP all tables from the database
 * * empty: to DELETE all records from each table in the database.
 * * backup: Rename every table from "name" to "name_back"
 * * drop_backup: DROP the backup tables.
 * * rollback: DROP the new table "name" and rename "name_back" to
 *   "name", restoring the database to its previous state.
 *
 * The intent of the last two is to be able to move an existing table
 * aside, query its contents with a transformation (in 2.4.x this is
 * already done as the contents are loaded completely when a Qof
 * session is started), save them to a new table according to a new
 * database format, and finally drop the backup table; if there's an
 * error during the process, rollback allows returning the table to
 * its original state.
 *
 * @param sql_conn: The sql connection (via dbi) to which the
 * transactions will be sent
 * @param tables: GList of tables to operate on.
 * @param op: The operation to perform.
 * @return Success (TRUE) or failure.
 */

static gboolean
conn_table_operation( GncSqlConnection *sql_conn, GSList *table_name_list,
                      TableOpType op )
{
    GSList* node;
    gboolean result = TRUE;
    GncDbiSqlConnection *conn = (GncDbiSqlConnection*)(sql_conn);
    GSList *full_table_name_list = NULL;
    const gchar *dbname = dbi_conn_get_option( conn->conn, "dbname" );

    g_return_val_if_fail( table_name_list != NULL, FALSE );
    if ( op == rollback )
        full_table_name_list =
            conn->provider->get_table_list( conn->conn, dbname );

    for ( node = table_name_list; node != NULL && result; node = node->next )
    {
        gchar* table_name = (gchar*)node->data;
        dbi_result result;
        /* Ignore the lock table */
        if ( g_strcmp0(table_name, lock_table) == 0)
        {
            continue;
        }
        do
        {
            gnc_dbi_init_error( conn );
            switch ( op )
            {
            case rollback:
                if (g_slist_find(full_table_name_list, table_name))
                {
                    result = dbi_conn_queryf( conn->conn, "DROP TABLE %s",
                                              table_name );
                    if ( result )
                        break;
                }
                /* Fall through */
            case backup:
            case drop_backup:
                result = conn_table_manage_backup( conn, table_name, op );
                break;
            case empty:
                result = dbi_conn_queryf( conn->conn, "DELETE FROM TABLE %s",
                                          table_name );
                break;
            case drop:
            default:
                result = dbi_conn_queryf( conn->conn, "DROP TABLE %s",
                                          table_name );
                break;
            }
        }
        while ( conn->retry );
        if ( result != NULL )
        {
            if ( dbi_result_free( result ) < 0 )
            {
                PERR( "Error in dbi_result_free() result\n" );
                result = FALSE;
            }
        }
    }
    gnc_table_slist_free( full_table_name_list );
    return result;
}

/**
 * Safely resave a database by renaming all of its tables, recreating
 * everything, and then dropping the backup tables only if there were
 * no errors. If there are errors, drop the new tables and restore the
 * originals.
 *
 * @param qbe: QofBackend for the session.
 * @param book: QofBook to be saved in the database.
 */
static void
gnc_dbi_safe_sync_all( QofBackend *qbe, QofBook *book )
{
    GncDbiBackend *be = (GncDbiBackend*)qbe;
    GncDbiSqlConnection *conn = (GncDbiSqlConnection*)(((GncSqlBackend*)be)->conn);
    GSList *table_list, *index_list, *iter;
    const gchar* dbname = NULL;

    g_return_if_fail( be != NULL );
    g_return_if_fail( book != NULL );

    ENTER( "book=%p, primary=%p", book, be->primary_book );
    dbname = dbi_conn_get_option( be->conn, "dbname" );
    table_list = conn->provider->get_table_list( conn->conn, dbname );
    if ( !conn_table_operation( (GncSqlConnection*)conn, table_list,
                                backup ) )
    {
        qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
        conn_table_operation( (GncSqlConnection*)conn, table_list,
                              rollback );
        LEAVE( "Failed to rename tables" );
        gnc_table_slist_free( table_list );
        return;
    }
    index_list = conn->provider->get_index_list( conn->conn );
    for ( iter = index_list; iter != NULL; iter = g_slist_next( iter) )
    {
        const char *errmsg;
        conn->provider->drop_index (conn->conn, iter->data);
        if ( DBI_ERROR_NONE != dbi_conn_error( conn->conn, &errmsg ) )
        {
            qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
            gnc_table_slist_free( index_list );
            conn_table_operation( (GncSqlConnection*)conn, table_list,
                                  rollback );
            gnc_table_slist_free( table_list );
            LEAVE( "Failed to drop indexes %s", errmsg  );
            return;
        }
    }
    gnc_table_slist_free( index_list );

    be->is_pristine_db = TRUE;
    be->primary_book = book;

    gnc_sql_sync_all( &be->sql_be, book );
    if ( ERR_BACKEND_NO_ERR != qof_backend_get_error( qbe ) )
    {
        conn_table_operation( (GncSqlConnection*)conn, table_list,
                              rollback );
        LEAVE( "Failed to create new database tables" );
        return;
    }
    conn_table_operation( (GncSqlConnection*)conn, table_list,
                          drop_backup );
    gnc_table_slist_free( table_list );
    LEAVE("book=%p", book);
}
/* ================================================================= */
static void
gnc_dbi_begin_edit( QofBackend *qbe, QofInstance *inst )
{
    GncDbiBackend* be = (GncDbiBackend*)qbe;

    g_return_if_fail( be != NULL );
    g_return_if_fail( inst != NULL );

    gnc_sql_begin_edit( &be->sql_be, inst );
}

static void
gnc_dbi_rollback_edit( QofBackend *qbe, QofInstance *inst )
{
    GncDbiBackend* be = (GncDbiBackend*)qbe;

    g_return_if_fail( be != NULL );
    g_return_if_fail( inst != NULL );

    gnc_sql_rollback_edit( &be->sql_be, inst );
}

static void
gnc_dbi_commit_edit( QofBackend *qbe, QofInstance *inst )
{
    GncDbiBackend* be = (GncDbiBackend*)qbe;

    g_return_if_fail( be != NULL );
    g_return_if_fail( inst != NULL );

    gnc_sql_commit_edit( &be->sql_be, inst );
}

/* ================================================================= */

static void
init_sql_backend( GncDbiBackend* dbi_be )
{
    QofBackend* be;

    be = (QofBackend*)dbi_be;

    be->session_end = gnc_dbi_session_end;
    be->destroy_backend = gnc_dbi_destroy_backend;

    be->load = gnc_dbi_load;

    /* The gda backend treats accounting periods transactionally. */
    be->begin = gnc_dbi_begin_edit;
    be->commit = gnc_dbi_commit_edit;
    be->rollback = gnc_dbi_rollback_edit;

    /* The gda backend will not be multi-user (for now)... */
    be->events_pending = NULL;
    be->process_events = NULL;

    /* The SQL/DBI backend doesn't need to be synced until it is
     * configured for multiuser access. */
    be->sync = gnc_dbi_safe_sync_all;
    be->safe_sync = gnc_dbi_safe_sync_all;
    be->load_config = NULL;
    be->get_config = NULL;

    be->compile_query = gnc_sql_compile_query;
    be->run_query = gnc_sql_run_query;
    be->free_query = gnc_sql_free_query;

    be->export_fn = NULL;

    gnc_sql_init( &dbi_be->sql_be );

    dbi_be->sql_be.conn = NULL;
    dbi_be->sql_be.book = NULL;
}

static QofBackend*
new_backend( void (*session_begin)( QofBackend *, QofSession *, const gchar *,
                                    /*@ unused @*/ gboolean,
                                    /*@ unused @*/ gboolean,
                                    /*@ unused @*/ gboolean ) )
{
    GncDbiBackend *dbi_be;
    QofBackend *be;

    dbi_be = g_new0( GncDbiBackend, 1 );
    g_assert( dbi_be != NULL );

    be = (QofBackend*)dbi_be;
    qof_backend_init( be );

    be->session_begin = session_begin;
    init_sql_backend( dbi_be );

    return be;
}

static QofBackend*
gnc_dbi_backend_sqlite3_new( void )
{
    return new_backend( gnc_dbi_sqlite3_session_begin );
}

static QofBackend*
gnc_dbi_backend_mysql_new( void )
{
    return new_backend( gnc_dbi_mysql_session_begin );
}

static QofBackend*
gnc_dbi_backend_postgres_new( void )
{
    return new_backend( gnc_dbi_postgres_session_begin );
}

static void
gnc_dbi_provider_free( /*@ only @*/ QofBackendProvider *prov )
{
    g_return_if_fail( prov != NULL );

    g_free( prov );
}

/*
 * Checks to see whether the file is an sqlite file or not
 *
 */
static gboolean
gnc_dbi_check_sqlite3_file( const gchar *uri )
{
    FILE* f;
    gchar buf[50];
    G_GNUC_UNUSED size_t chars_read;
    gint status;
    gchar *filename;

    // BAD if the path is null
    g_return_val_if_fail( uri != NULL, FALSE );

    filename = gnc_uri_get_path ( uri );
    f = g_fopen( filename, "r" );
    g_free ( filename );

    // OK if the file doesn't exist - new file
    if ( f == NULL )
    {
        PINFO( "doesn't exist (errno=%d) -> DBI", errno );
        return TRUE;
    }

    // OK if file has the correct header
    chars_read = fread( buf, sizeof(buf), 1, f );
    status = fclose( f );
    if ( status < 0 )
    {
        PERR( "Error in fclose(): %d\n", errno );
    }
    if ( g_str_has_prefix( buf, "SQLite format 3" ) )
    {
        PINFO( "has SQLite format string -> DBI" );
        return TRUE;
    }
    PINFO( "exists, does not have SQLite format string -> not DBI" );

    // Otherwise, BAD
    return FALSE;
}

void
gnc_module_init_backend_dbi(void)
{
    QofBackendProvider *prov;
    const gchar* driver_dir;
    int num_drivers;
    gboolean have_sqlite3_driver = FALSE;
    gboolean have_mysql_driver = FALSE;
    gboolean have_pgsql_driver = FALSE;

    /* Initialize libdbi and see which drivers are available.  Only register qof backends which
       have drivers available. */
    driver_dir = g_getenv( "GNC_DBD_DIR" );
    if ( driver_dir == NULL )
    {
        PINFO( "GNC_DBD_DIR not set: using libdbi built-in default\n");
    }

    /* dbi_initialize returns -1 in case of errors */
#if HAVE_LIBDBI_R
    if (dbi_instance)
        return;
    num_drivers = dbi_initialize_r( driver_dir, &dbi_instance );
#else
    num_drivers = dbi_initialize( driver_dir );
#endif
    if ( num_drivers <= 0 )
    {
	gchar *dir = g_build_filename(gnc_path_get_libdir(), "dbd", NULL);
#if HAVE_LIBDBI_R
	if (dbi_instance)
	    return;
	num_drivers = dbi_initialize_r( dir, &dbi_instance );
#else
	num_drivers = dbi_initialize( dir );
#endif
	g_free (dir);
    }
    if ( num_drivers <= 0 )
    {
        PWARN( "No DBD drivers found\n" );
    }
    else
    {
        dbi_driver driver = NULL;
        PINFO( "%d DBD drivers found\n", num_drivers );

        do
        {
            #if HAVE_LIBDBI_R
            driver = dbi_driver_list_r( driver, dbi_instance );
            #else
            driver = dbi_driver_list( driver );
            #endif

            if ( driver != NULL )
            {
                const gchar* name = dbi_driver_get_name( driver );

                PINFO( "Driver: %s\n", name );
                if ( strcmp( name, "sqlite3" ) == 0 )
                {
                    have_sqlite3_driver = TRUE;
                }
                else if ( strcmp( name, "mysql" ) == 0 )
                {
                    have_mysql_driver = TRUE;
                }
                else if ( strcmp( name, "pgsql" ) == 0 )
                {
                    have_pgsql_driver = TRUE;
                }
            }
        }
        while ( driver != NULL );
    }

    if ( have_sqlite3_driver )
    {
        prov = g_new0( QofBackendProvider, 1 );
        g_assert( prov != NULL );

        prov->provider_name = "GnuCash Libdbi (SQLITE3) Backend";
        prov->access_method = FILE_URI_TYPE;
        prov->backend_new = gnc_dbi_backend_sqlite3_new;
        prov->provider_free = gnc_dbi_provider_free;
        prov->check_data_type = gnc_dbi_check_sqlite3_file;
        qof_backend_register_provider( prov );

        prov = g_new0( QofBackendProvider, 1 );
        g_assert( prov != NULL );

        prov->provider_name = "GnuCash Libdbi (SQLITE3) Backend";
        prov->access_method = SQLITE3_URI_TYPE;
        prov->backend_new = gnc_dbi_backend_sqlite3_new;
        prov->provider_free = gnc_dbi_provider_free;
        prov->check_data_type = gnc_dbi_check_sqlite3_file;
        qof_backend_register_provider( prov );
    }

    if ( have_mysql_driver )
    {
        prov = g_new0( QofBackendProvider, 1 );
        g_assert( prov != NULL );

        prov->provider_name = "GnuCash Libdbi (MYSQL) Backend";
        prov->access_method = "mysql";
        prov->backend_new = gnc_dbi_backend_mysql_new;
        prov->provider_free = gnc_dbi_provider_free;
        prov->check_data_type = NULL;
        qof_backend_register_provider( prov );
    }

    if ( have_pgsql_driver )
    {
        prov = g_new0( QofBackendProvider, 1 );
        g_assert( prov != NULL );

        prov->provider_name = "GnuCash Libdbi (POSTGRESQL) Backend";
        prov->access_method = "postgres";
        prov->backend_new = gnc_dbi_backend_postgres_new;
        prov->provider_free = gnc_dbi_provider_free;
        prov->check_data_type = NULL;
        qof_backend_register_provider( prov );
    }

    /* If needed, set log level to DEBUG so that SQl statements will be put into
       the gnucash.trace file. */
    /*    qof_log_set_level( log_module, QOF_LOG_DEBUG ); */
}

#ifndef GNC_NO_LOADABLE_MODULES
G_MODULE_EXPORT void
qof_backend_module_init( void )
{
    gnc_module_init_backend_dbi();
}

G_MODULE_EXPORT void
qof_backend_module_finalize( void )
{
    gnc_module_finalize_backend_dbi();
}
#endif /* GNC_NO_LOADABLE_MODULES */

void
gnc_module_finalize_backend_dbi( void )
{
#if HAVE_LIBDBI_R
  if (dbi_instance)
  {
      dbi_shutdown_r(dbi_instance);
      dbi_instance = NULL;
  }
#else
    dbi_shutdown();
#endif
}

/* --------------------------------------------------------- */
typedef struct
{
    GncSqlRow base;

    /*@ dependent @*/
    dbi_result result;
    GList* gvalue_list;
} GncDbiSqlRow;

static void
row_dispose( /*@ only @*/ GncSqlRow* row )
{
    GncDbiSqlRow* dbi_row = (GncDbiSqlRow*)row;
    GList* node;

    if ( dbi_row->gvalue_list != NULL )
    {
        for ( node = dbi_row->gvalue_list; node != NULL; node = node->next )
        {
            GValue* value;
            if ( !G_IS_VALUE(node->data) )
                continue;
            value = (GValue*)node->data;
            if ( G_VALUE_HOLDS_STRING(value) )
            {
                g_free( (gpointer)g_value_get_string( value ) );
            }
            g_free( value );
        }
        g_list_free( dbi_row->gvalue_list );
    }
    g_free( dbi_row );
}

static /*@ null @*/ const GValue*
row_get_value_at_col_name( GncSqlRow* row, const gchar* col_name )
{
    GncDbiSqlRow* dbi_row = (GncDbiSqlRow*)row;
    gushort type;
    guint attrs;
    GValue* value;

    type = dbi_result_get_field_type( dbi_row->result, col_name );
    attrs = dbi_result_get_field_attribs( dbi_row->result, col_name );
    value = g_new0( GValue, 1 );
    g_assert( value != NULL );

    switch ( type )
    {
    case DBI_TYPE_INTEGER:
        (void)g_value_init( value, G_TYPE_INT64 );
        g_value_set_int64( value, dbi_result_get_longlong( dbi_row->result, col_name ) );
        break;
    case DBI_TYPE_DECIMAL:
        gnc_push_locale( LC_NUMERIC, "C" );
        if ( (attrs & DBI_DECIMAL_SIZEMASK) == DBI_DECIMAL_SIZE4 )
        {
            (void)g_value_init( value, G_TYPE_FLOAT );
            g_value_set_float( value, dbi_result_get_float( dbi_row->result, col_name ) );
        }
        else if ( (attrs & DBI_DECIMAL_SIZEMASK) == DBI_DECIMAL_SIZE8 )
        {
            (void)g_value_init( value, G_TYPE_DOUBLE );
            g_value_set_double( value, dbi_result_get_double( dbi_row->result, col_name ) );
        }
        else
        {
            PERR( "Field %s: strange decimal length attrs=%d\n", col_name, attrs );
        }
        gnc_pop_locale( LC_NUMERIC );
        break;
    case DBI_TYPE_STRING:
        (void)g_value_init( value, G_TYPE_STRING );
        g_value_take_string( value, dbi_result_get_string_copy( dbi_row->result, col_name ) );
        break;
    case DBI_TYPE_DATETIME:
        if ( dbi_result_field_is_null( dbi_row->result, col_name ) )
        {
            return NULL;
        }
	else
	{
	    /* A seriously evil hack to work around libdbi bug #15
	     * https://sourceforge.net/p/libdbi/bugs/15/. When libdbi
	     * v0.9 is widely available this can be replaced with
	     * dbi_result_get_as_longlong.
	     */
	    dbi_result_t *result = (dbi_result_t*)(dbi_row->result);
	    guint64 row = dbi_result_get_currow (result);
	    guint idx = dbi_result_get_field_idx (result, col_name) - 1;
	    time64 time = result->rows[row]->field_values[idx].d_datetime;
	    (void)g_value_init( value, G_TYPE_INT64 );
	    g_value_set_int64 (value, time);
	}
        break;
    default:
        PERR( "Field %s: unknown DBI_TYPE: %d\n", col_name, type );
        g_free( value );
        return NULL;
    }

    dbi_row->gvalue_list = g_list_prepend( dbi_row->gvalue_list, value );
    return value;
}

static GncSqlRow*
create_dbi_row( /*@ dependent @*/ dbi_result result )
{
    GncDbiSqlRow* row;

    row = g_new0( GncDbiSqlRow, 1 );
    g_assert( row != NULL );

    row->base.getValueAtColName = row_get_value_at_col_name;
    row->base.dispose = row_dispose;
    row->result = result;

    return (GncSqlRow*)row;
}
/* --------------------------------------------------------- */
typedef struct
{
    GncSqlResult base;

    /*@ observer @*/
    GncDbiSqlConnection* dbi_conn;
    /*@ owned @*/
    dbi_result result;
    guint num_rows;
    guint cur_row;
    GncSqlRow* row;
} GncDbiSqlResult;

static void
result_dispose( /*@ only @*/ GncSqlResult* result )
{
    GncDbiSqlResult* dbi_result = (GncDbiSqlResult*)result;

    if ( dbi_result->row != NULL )
    {
        gnc_sql_row_dispose( dbi_result->row );
    }
    if ( dbi_result->result != NULL )
    {
        gint status;

        status = dbi_result_free( dbi_result->result );
        if ( status < 0 )
        {
            PERR( "Error in dbi_result_free() result\n" );
            qof_backend_set_error( dbi_result->dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
        }
    }
    g_free( result );
}

static guint
result_get_num_rows( GncSqlResult* result )
{
    GncDbiSqlResult* dbi_result = (GncDbiSqlResult*)result;

    return dbi_result->num_rows;
}

static /*@ null @*/ GncSqlRow*
result_get_first_row( GncSqlResult* result )
{
    GncDbiSqlResult* dbi_result = (GncDbiSqlResult*)result;

    if ( dbi_result->row != NULL )
    {
        gnc_sql_row_dispose( dbi_result->row );
        dbi_result->row = NULL;
    }
    if ( dbi_result->num_rows > 0 )
    {
        gint status = dbi_result_first_row( dbi_result->result );
        if ( status == 0 )
        {
            PERR( "Error in dbi_result_first_row()\n" );
            qof_backend_set_error( dbi_result->dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
        }
        dbi_result->cur_row = 1;
        dbi_result->row = create_dbi_row( dbi_result->result );
        return dbi_result->row;
    }
    else
    {
        return NULL;
    }
}

static /*@ null @*/ GncSqlRow*
result_get_next_row( GncSqlResult* result )
{
    GncDbiSqlResult* dbi_result = (GncDbiSqlResult*)result;

    if ( dbi_result->row != NULL )
    {
        gnc_sql_row_dispose( dbi_result->row );
        dbi_result->row = NULL;
    }
    if ( dbi_result->cur_row < dbi_result->num_rows )
    {
        gint status = dbi_result_next_row( dbi_result->result );
        if ( status == 0 )
        {
            PERR( "Error in dbi_result_first_row()\n" );
            qof_backend_set_error( dbi_result->dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
        }
        dbi_result->cur_row++;
        dbi_result->row = create_dbi_row( dbi_result->result );
        return dbi_result->row;
    }
    else
    {
        return NULL;
    }
}

static GncSqlResult*
create_dbi_result( /*@ observer @*/ GncDbiSqlConnection* dbi_conn, /*@ owned @*/ dbi_result result )
{
    GncDbiSqlResult* dbi_result;

    dbi_result = g_new0( GncDbiSqlResult, 1 );
    g_assert( dbi_result != NULL );

    dbi_result->base.dispose = result_dispose;
    dbi_result->base.getNumRows = result_get_num_rows;
    dbi_result->base.getFirstRow = result_get_first_row;
    dbi_result->base.getNextRow = result_get_next_row;
    dbi_result->result = result;
    dbi_result->num_rows = (guint)dbi_result_get_numrows( result );
    dbi_result->cur_row = 0;
    dbi_result->dbi_conn = dbi_conn;

    return (GncSqlResult*)dbi_result;
}
/* --------------------------------------------------------- */
typedef struct
{
    GncSqlStatement base;

    GString* sql;
    /*@ observer @*/
    GncSqlConnection* conn;
} GncDbiSqlStatement;

static void
stmt_dispose( /*@ only @*/ GncSqlStatement* stmt )
{
    GncDbiSqlStatement* dbi_stmt = (GncDbiSqlStatement*)stmt;

    if ( dbi_stmt->sql != NULL )
    {
        (void)g_string_free( dbi_stmt->sql, TRUE );
    }
    g_free( stmt );
}

static gchar*
stmt_to_sql( GncSqlStatement* stmt )
{
    GncDbiSqlStatement* dbi_stmt = (GncDbiSqlStatement*)stmt;

    return dbi_stmt->sql->str;
}

static void
stmt_add_where_cond( GncSqlStatement* stmt, /*@ unused @*/ QofIdTypeConst type_name,
                     /*@ unused @*/ gpointer obj, const GncSqlColumnTableEntry* table_row, GValue* value )
{
    GncDbiSqlStatement* dbi_stmt = (GncDbiSqlStatement*)stmt;
    gchar* buf;
    gchar* value_str;

    value_str = gnc_sql_get_sql_value( dbi_stmt->conn, value );
    buf = g_strdup_printf( " WHERE %s = %s", table_row->col_name,
                           value_str );
    g_free( value_str );
    (void)g_string_append( dbi_stmt->sql, buf );
    g_free( buf );
}

static GncSqlStatement*
create_dbi_statement( /*@ observer @*/ GncSqlConnection* conn, const gchar* sql )
{
    GncDbiSqlStatement* stmt;

    stmt = g_new0( GncDbiSqlStatement, 1 );
    g_assert( stmt != NULL );

    stmt->base.dispose = stmt_dispose;
    stmt->base.toSql = stmt_to_sql;
    stmt->base.addWhereCond = stmt_add_where_cond;
    stmt->sql = g_string_new( sql );
    stmt->conn = conn;

    return (GncSqlStatement*)stmt;
}
/* --------------------------------------------------------- */
static void
conn_dispose( /*@ only @*/ GncSqlConnection* conn )
{
    //GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;

    g_free( conn );
}

static /*@ null @*/ GncSqlResult*
conn_execute_select_statement( GncSqlConnection* conn, GncSqlStatement* stmt )
{
    GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
    GncDbiSqlStatement* dbi_stmt = (GncDbiSqlStatement*)stmt;
    dbi_result result;

    DEBUG( "SQL: %s\n", dbi_stmt->sql->str );
    gnc_push_locale( LC_NUMERIC, "C" );
    do
    {
        gnc_dbi_init_error( dbi_conn );
        result = dbi_conn_query( dbi_conn->conn, dbi_stmt->sql->str );
    }
    while ( dbi_conn->retry );
    if ( result == NULL )
    {
        PERR( "Error executing SQL %s\n", dbi_stmt->sql->str );
        return NULL;
    }
    gnc_pop_locale( LC_NUMERIC );
    return create_dbi_result( dbi_conn, result );
}

static gint
conn_execute_nonselect_statement( GncSqlConnection* conn, GncSqlStatement* stmt )
{
    GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
    GncDbiSqlStatement* dbi_stmt = (GncDbiSqlStatement*)stmt;
    dbi_result result;
    gint num_rows;
    gint status;

    DEBUG( "SQL: %s\n", dbi_stmt->sql->str );
    do
    {
        gnc_dbi_init_error( dbi_conn );
        result = dbi_conn_query( dbi_conn->conn, dbi_stmt->sql->str );
    }
    while ( dbi_conn->retry );
    if ( result == NULL )
    {
        PERR( "Error executing SQL %s\n", dbi_stmt->sql->str );
        return -1;
    }
    num_rows = (gint)dbi_result_get_numrows_affected( result );
    status = dbi_result_free( result );
    if ( status < 0 )
    {
        PERR( "Error in dbi_result_free() result\n" );
        qof_backend_set_error( dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
    }
    return num_rows;
}

static GncSqlStatement*
conn_create_statement_from_sql( /*@ observer @*/ GncSqlConnection* conn, const gchar* sql )
{
    //GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;

    return create_dbi_statement( conn, sql );
}

static gboolean
conn_does_table_exist( GncSqlConnection* conn, const gchar* table_name )
{
    GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
    gint nTables;
    dbi_result tables;
    const gchar* dbname;
    gint status;

    g_return_val_if_fail( conn != NULL, FALSE );
    g_return_val_if_fail( table_name != NULL, FALSE );

    dbname = dbi_conn_get_option( dbi_conn->conn, "dbname" );
    tables = dbi_conn_get_table_list( dbi_conn->conn, dbname, table_name );
    nTables = (gint)dbi_result_get_numrows( tables );
    status = dbi_result_free( tables );
    if ( status < 0 )
    {
        PERR( "Error in dbi_result_free() result\n" );
        qof_backend_set_error( dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
    }

    if ( nTables == 1 )
    {
        return TRUE;
    }
    else
    {
        return FALSE;
    }
}

static gboolean
conn_begin_transaction( /*@ unused @*/ GncSqlConnection* conn )
{
    GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
    dbi_result result;
    gint status;
    gboolean success = FALSE;

    DEBUG( "BEGIN\n" );

    if ( !gnc_dbi_verify_conn (dbi_conn) )
    {
        PERR( "gnc_dbi_verify_conn() failed\n" );
        qof_backend_set_error( dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
        return FALSE;
    }

    do
    {
        gnc_dbi_init_error( dbi_conn );
        result = dbi_conn_queryf( dbi_conn->conn, "BEGIN" );
    }
    while ( dbi_conn->retry );

    success = ( result != NULL );
    status = dbi_result_free( result );
    if ( status < 0 )
    {
        PERR( "Error in dbi_result_free() result\n" );
        qof_backend_set_error( dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
    }
    if ( !success )
    {
        PERR( "BEGIN transaction failed()\n" );
        qof_backend_set_error( dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
    }

    return success;
}

static gboolean
conn_rollback_transaction( /*@ unused @*/ GncSqlConnection* conn )
{
    GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
    dbi_result result;
    gint status;
    gboolean success = FALSE;

    DEBUG( "ROLLBACK\n" );
    result = dbi_conn_queryf( dbi_conn->conn, "ROLLBACK" );
    success = ( result != NULL );

    status = dbi_result_free( result );
    if ( status < 0 )
    {
        PERR( "Error in dbi_result_free() result\n" );
        qof_backend_set_error( dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
    }
    if ( !success )
    {
        PERR( "Error in conn_rollback_transaction()\n" );
        qof_backend_set_error( dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
    }

    return success;
}

static gboolean
conn_commit_transaction( /*@ unused @*/ GncSqlConnection* conn )
{
    GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
    dbi_result result;
    gint status;
    gboolean success = FALSE;

    DEBUG( "COMMIT\n" );
    result = dbi_conn_queryf( dbi_conn->conn, "COMMIT" );
    success = ( result != NULL );

    status = dbi_result_free( result );
    if ( status < 0 )
    {
        PERR( "Error in dbi_result_free() result\n" );
        qof_backend_set_error( dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
    }
    if ( !success )
    {
        PERR( "Error in conn_commit_transaction()\n" );
        qof_backend_set_error( dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
    }

    return success;
}

static /*@ null @*/ gchar*
create_index_ddl( GncSqlConnection* conn,
                  const gchar* index_name,
                  const gchar* table_name,
                  const GncSqlColumnTableEntry* col_table )
{
    GString* ddl;
    const GncSqlColumnTableEntry* table_row;

    g_return_val_if_fail( conn != NULL, NULL );
    g_return_val_if_fail( index_name != NULL, NULL );
    g_return_val_if_fail( table_name != NULL, NULL );
    g_return_val_if_fail( col_table != NULL, NULL );

    ddl = g_string_new( "" );
    g_string_printf( ddl, "CREATE INDEX %s ON %s (", index_name, table_name );
    for ( table_row = col_table; table_row->col_name != NULL; ++table_row )
    {
        if ( table_row != col_table )
        {
            (void)g_string_append( ddl, ", " );
        }
        g_string_append_printf( ddl, "%s", table_row->col_name );
    }
    (void)g_string_append( ddl, ")" );

    return g_string_free( ddl, FALSE );
}

static /*@ null @*/ gchar*
add_columns_ddl( GncSqlConnection* conn,
                 const gchar* table_name,
                 GList* col_info_list )
{
    GString* ddl;
    const GList* list_node;
    guint col_num;
    GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;

    g_return_val_if_fail( conn != NULL, NULL );
    g_return_val_if_fail( table_name != NULL, NULL );
    g_return_val_if_fail( col_info_list != NULL, NULL );

    ddl = g_string_new( "" );
    g_string_printf( ddl, "ALTER TABLE %s ", table_name );
    for ( list_node = col_info_list, col_num = 0; list_node != NULL;
            list_node = list_node->next, col_num++ )
    {
        GncSqlColumnInfo* info = (GncSqlColumnInfo*)(list_node->data);

        if ( col_num != 0 )
        {
            (void)g_string_append( ddl, ", " );
        }
        g_string_append( ddl, "ADD COLUMN " );
        dbi_conn->provider->append_col_def( ddl, info );
        g_free( info->name );
        g_free( info );
    }

    return g_string_free( ddl, FALSE );
}

static void
append_sqlite3_col_def( GString* ddl, GncSqlColumnInfo* info )
{
    gchar* type_name;

    if ( info->type == BCT_INT )
    {
        type_name = "integer";
    }
    else if ( info->type == BCT_INT64 )
    {
        type_name = "bigint";
    }
    else if ( info->type == BCT_DOUBLE )
    {
        type_name = "float8";
    }
    else if ( info->type == BCT_STRING || info->type == BCT_DATE
              || info->type == BCT_DATETIME )
    {
        type_name = "text";
    }
    else
    {
        PERR( "Unknown column type: %d\n", info->type );
        type_name = "";
    }
    g_string_append_printf( ddl, "%s %s", info->name, type_name );
    if ( info->size != 0 )
    {
        (void)g_string_append_printf( ddl, "(%d)", info->size );
    }
    if ( info->is_primary_key )
    {
        (void)g_string_append( ddl, " PRIMARY KEY" );
    }
    if ( info->is_autoinc )
    {
        (void)g_string_append( ddl, " AUTOINCREMENT" );
    }
    if ( !info->null_allowed )
    {
        (void)g_string_append( ddl, " NOT NULL" );
    }
}

static /*@ null @*/ gchar*
conn_create_table_ddl_sqlite3( GncSqlConnection* conn,
                               const gchar* table_name,
                               const GList* col_info_list )
{
    GString* ddl;
    const GList* list_node;
    guint col_num;

    g_return_val_if_fail( conn != NULL, NULL );
    g_return_val_if_fail( table_name != NULL, NULL );
    g_return_val_if_fail( col_info_list != NULL, NULL );

    ddl = g_string_new( "" );
    g_string_printf( ddl, "CREATE TABLE %s (", table_name );
    for ( list_node = col_info_list, col_num = 0; list_node != NULL;
            list_node = list_node->next, col_num++ )
    {
        GncSqlColumnInfo* info = (GncSqlColumnInfo*)(list_node->data);

        if ( col_num != 0 )
        {
            (void)g_string_append( ddl, ", " );
        }
        append_sqlite3_col_def( ddl, info );
        g_free( info->name );
        g_free( info );
    }
    (void)g_string_append( ddl, ")" );

    return g_string_free( ddl, FALSE );
}

static void
append_mysql_col_def( GString* ddl, GncSqlColumnInfo* info )
{
    gchar* type_name;

    if ( info->type == BCT_INT )
    {
        type_name = "integer";
    }
    else if ( info->type == BCT_INT64 )
    {
        type_name = "bigint";
    }
    else if ( info->type == BCT_DOUBLE )
    {
        type_name = "double";
    }
    else if ( info->type == BCT_STRING )
    {
        type_name = "varchar";
    }
    else if ( info->type == BCT_DATE )
    {
        info->size = 0;
        type_name = "date";
    }
    else if ( info->type == BCT_DATETIME )
    {
        info->size = 0;
        type_name = "TIMESTAMP NULL DEFAULT 0";
    }
    else
    {
        PERR( "Unknown column type: %d\n", info->type );
        type_name = "";
    }
    g_string_append_printf( ddl, "%s %s", info->name, type_name );
    if ( info->size != 0 )
    {
        g_string_append_printf( ddl, "(%d)", info->size );
    }
    if ( info->is_unicode )
    {
        (void)g_string_append( ddl, " CHARACTER SET utf8" );
    }
    if ( info->is_primary_key )
    {
        (void)g_string_append( ddl, " PRIMARY KEY" );
    }
    if ( info->is_autoinc )
    {
        (void)g_string_append( ddl, " AUTO_INCREMENT" );
    }
    if ( !info->null_allowed )
    {
        (void)g_string_append( ddl, " NOT NULL" );
    }
}

static /*@ null @*/ gchar*
conn_create_table_ddl_mysql( GncSqlConnection* conn, const gchar* table_name,
                             const GList* col_info_list )
{
    GString* ddl;
    const GList* list_node;
    guint col_num;

    g_return_val_if_fail( conn != NULL, NULL );
    g_return_val_if_fail( table_name != NULL, NULL );
    g_return_val_if_fail( col_info_list != NULL, NULL );

    ddl = g_string_new( "" );
    g_string_printf( ddl, "CREATE TABLE %s (", table_name );
    for ( list_node = col_info_list, col_num = 0; list_node != NULL;
            list_node = list_node->next, col_num++ )
    {
        GncSqlColumnInfo* info = (GncSqlColumnInfo*)(list_node->data);

        if ( col_num != 0 )
        {
            (void)g_string_append( ddl, ", " );
        }
        append_mysql_col_def( ddl, info );
        g_free( info->name );
        g_free( info );
    }
    (void)g_string_append( ddl, ")" );

    return g_string_free( ddl, FALSE );
}

static void
append_pgsql_col_def( GString* ddl, GncSqlColumnInfo* info )
{
    gchar* type_name;

    if ( info->type == BCT_INT )
    {
        if ( info->is_autoinc )
        {
            type_name = "serial";
        }
        else
        {
            type_name = "integer";
        }
    }
    else if ( info->type == BCT_INT64 )
    {
        type_name = "int8";
    }
    else if ( info->type == BCT_DOUBLE )
    {
        type_name = "double precision";
    }
    else if ( info->type == BCT_STRING )
    {
        type_name = "varchar";
    }
    else if ( info->type == BCT_DATE )
    {
        info->size = 0;
        type_name = "date";
    }
    else if ( info->type == BCT_DATETIME )
    {
        info->size = 0;
        type_name = "timestamp without time zone";
    }
    else
    {
        PERR( "Unknown column type: %d\n", info->type );
        type_name = "";
    }
    g_string_append_printf( ddl, "%s %s", info->name, type_name );
    if ( info->size != 0 )
    {
        g_string_append_printf( ddl, "(%d)", info->size );
    }
    if ( info->is_primary_key )
    {
        (void)g_string_append( ddl, " PRIMARY KEY" );
    }
    if ( !info->null_allowed )
    {
        (void)g_string_append( ddl, " NOT NULL" );
    }
}

static /*@ null @*/ gchar*
conn_create_table_ddl_pgsql( GncSqlConnection* conn, const gchar* table_name,
                             const GList* col_info_list )
{
    GString* ddl;
    const GList* list_node;
    guint col_num;
    gboolean is_unicode = FALSE;

    g_return_val_if_fail( conn != NULL, NULL );
    g_return_val_if_fail( table_name != NULL, NULL );
    g_return_val_if_fail( col_info_list != NULL, NULL );

    ddl = g_string_new( "" );
    g_string_printf( ddl, "CREATE TABLE %s (", table_name );
    for ( list_node = col_info_list, col_num = 0; list_node != NULL;
            list_node = list_node->next, col_num++ )
    {
        GncSqlColumnInfo* info = (GncSqlColumnInfo*)(list_node->data);

        if ( col_num != 0 )
        {
            (void)g_string_append( ddl, ", " );
        }
        append_pgsql_col_def( ddl, info );
        is_unicode = is_unicode || info->is_unicode;
        g_free( info->name );
        g_free( info );
    }
    (void)g_string_append( ddl, ")" );
    if ( is_unicode )
    {
    }

    return g_string_free( ddl, FALSE );
}

static gboolean
conn_create_table( GncSqlConnection* conn, const gchar* table_name,
                   GList* col_info_list )
{
    GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
    gchar* ddl;
    dbi_result result;

    g_return_val_if_fail( conn != NULL, FALSE );
    g_return_val_if_fail( table_name != NULL, FALSE );
    g_return_val_if_fail( col_info_list != NULL, FALSE );


    ddl = dbi_conn->provider->create_table_ddl( conn, table_name,
            col_info_list );
    g_list_free( col_info_list );
    if ( ddl != NULL )
    {
        gint status;

        DEBUG( "SQL: %s\n", ddl );
        result = dbi_conn_query( dbi_conn->conn, ddl );
        g_free( ddl );
        status = dbi_result_free( result );
        if ( status < 0 )
        {
            PERR( "Error in dbi_result_free() result\n" );
            qof_backend_set_error( dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
        }
    }
    else
    {
        return FALSE;
    }

    return TRUE;
}

static gboolean
conn_create_index( /*@ unused @*/ GncSqlConnection* conn, /*@ unused @*/ const gchar* index_name,
                                  /*@ unused @*/ const gchar* table_name, /*@ unused @*/ const GncSqlColumnTableEntry* col_table )
{
    GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
    gchar* ddl;
    dbi_result result;

    g_return_val_if_fail( conn != NULL, FALSE );
    g_return_val_if_fail( index_name != NULL, FALSE );
    g_return_val_if_fail( table_name != NULL, FALSE );
    g_return_val_if_fail( col_table != NULL, FALSE );

    ddl = create_index_ddl( conn, index_name, table_name, col_table );
    if ( ddl != NULL )
    {
        gint status;

        DEBUG( "SQL: %s\n", ddl );
        result = dbi_conn_query( dbi_conn->conn, ddl );
        g_free( ddl );
        status = dbi_result_free( result );
        if ( status < 0 )
        {
            PERR( "Error in dbi_result_free() result\n" );
            qof_backend_set_error( dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
        }
    }
    else
    {
        return FALSE;
    }

    return TRUE;
}

static gboolean
conn_add_columns_to_table( /*@ unused @*/ GncSqlConnection* conn, /*@ unused @*/ const gchar* table_name,
        GList* col_info_list )
{
    GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
    gchar* ddl;
    dbi_result result;

    g_return_val_if_fail( conn != NULL, FALSE );
    g_return_val_if_fail( table_name != NULL, FALSE );
    g_return_val_if_fail( col_info_list != NULL, FALSE );

    ddl = add_columns_ddl( conn, table_name, col_info_list );
    if ( ddl != NULL )
    {
        gint status;

        DEBUG( "SQL: %s\n", ddl );
        result = dbi_conn_query( dbi_conn->conn, ddl );
        g_free( ddl );
        status = dbi_result_free( result );
        if ( status < 0 )
        {
            PERR( "Error in dbi_result_free() result\n" );
            qof_backend_set_error( dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
        }
    }
    else
    {
        return FALSE;
    }

    return TRUE;
}

static /*@ null @*/ gchar*
conn_quote_string( const GncSqlConnection* conn, gchar* unquoted_str )
{
    GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
    gchar* quoted_str;
    size_t size;

    size = dbi_conn_quote_string_copy( dbi_conn->conn, unquoted_str,
                                       &quoted_str );
    if ( size != 0 )
    {
        return quoted_str;
    }
    else
    {
        return NULL;
    }
}

static GSList*
conn_get_table_list( dbi_conn conn, const gchar* dbname )
{
    dbi_result tables;
    GSList* list = NULL;

    tables = dbi_conn_get_table_list( conn, dbname, NULL );
    while ( dbi_result_next_row( tables ) != 0 )
    {
        const gchar* table_name;

        table_name = dbi_result_get_string_idx( tables, 1 );
        list = g_slist_prepend( list, strdup( table_name ) );
    }
    dbi_result_free( tables );
    return list;
}

static GSList*
conn_get_table_list_sqlite3( dbi_conn conn, const gchar* dbname )
{
    gboolean change_made;

    /* Return the list, but remove the tables that sqlite3 adds for
     * its own use. */
    GSList* list = conn_get_table_list( conn, dbname );
    change_made = TRUE;
    while ( list != NULL && change_made )
    {
        GSList* node;

        change_made = FALSE;
        for ( node = list; node != NULL; node = node->next )
        {
            const gchar* table_name = (const gchar*)node->data;

            if ( strcmp( table_name, "sqlite_sequence" ) == 0 )
            {
                g_free( node->data );
                list = g_slist_delete_link( list, node );
                change_made = TRUE;
                break;
            }
        }
    }
    return list;
}

static GSList*
conn_get_table_list_pgsql( dbi_conn conn, const gchar* dbname )
{
    gboolean change_made;

    /* Return the list, but remove the tables that postgresql adds from the information schema. */
    GSList* list = conn_get_table_list( conn, dbname );
    change_made = TRUE;
    while ( list != NULL && change_made )
    {
        GSList* node;

        change_made = FALSE;
        for ( node = list; node != NULL; node = node->next )
        {
            const gchar* table_name = (const gchar*)node->data;

            if ( strcmp( table_name, "sql_features" ) == 0 ||
                    strcmp( table_name, "sql_implementation_info" ) == 0 ||
                    strcmp( table_name, "sql_languages" ) == 0 ||
                    strcmp( table_name, "sql_packages" ) == 0 ||
                    strcmp( table_name, "sql_parts" ) == 0 ||
                    strcmp( table_name, "sql_sizing" ) == 0 ||
                    strcmp( table_name, "sql_sizing_profiles" ) == 0 )
            {
                g_free( node->data );
                list = g_slist_delete_link( list, node );
                change_made = TRUE;
                break;
            }
        }
    }
    return list;
}

/** Users discovered a bug in some distributions of libdbi, where if
 * it is compiled on certain versions of gcc with the -ffast-math
 * compiler option it fails to correctly handle saving of 64-bit
 * values. This function tests for the problem.
 * @param: conn: The just-opened dbi_conn
 * @returns: GNC_DBI_PASS if the dbi library is safe to use,
 * GNC_DBI_FAIL_SETUP if the test could not be completed, or
 * GNC_DBI_FAIL_TEST if the bug was found.
 */
static GncDbiTestResult
conn_test_dbi_library( dbi_conn conn )
{
    gint64 testlonglong = -9223372036854775807LL, resultlonglong = 0;
    guint64 testulonglong = 9223372036854775807LLU, resultulonglong = 0;
    gdouble testdouble = 1.7976921348623157E+307, resultdouble = 0.0;
    dbi_result result;
    gchar doublestr[G_ASCII_DTOSTR_BUF_SIZE], *querystr;
    GncDbiTestResult retval = GNC_DBI_PASS;
    memset( doublestr, 0, sizeof(doublestr));

    result = dbi_conn_query( conn, "CREATE TEMPORARY TABLE numtest "
                             "( test_int BIGINT, test_unsigned BIGINT,"
                             " test_double FLOAT8 )" );
    if ( result == NULL )
    {
        PWARN("Test_DBI_Library: Create table failed");
        return GNC_DBI_FAIL_SETUP;
    }
    dbi_result_free( result );
    g_ascii_dtostr( doublestr, sizeof(doublestr), testdouble );
    querystr = g_strdup_printf( "INSERT INTO numtest VALUES (%" G_GINT64_FORMAT
                                ", %" G_GUINT64_FORMAT ", %s)",
                                testlonglong, testulonglong, doublestr );
    result = dbi_conn_query( conn, querystr );
    g_free( querystr );
    if ( result == NULL )
    {
        PWARN("Test_DBI_Library: Failed to insert test row into table" );
        return GNC_DBI_FAIL_SETUP;
    }
    dbi_result_free( result );
    gnc_push_locale( LC_NUMERIC, "C");
    result = dbi_conn_query( conn, "SELECT * FROM numtest" );
    if ( result == NULL )
    {
        const char *errmsg;
        dbi_conn_error( conn, &errmsg );
        PWARN("Test_DBI_Library: Failed to retrieve test row into table: %s",
              errmsg );
        result = dbi_conn_query( conn, "DROP TABLE numtest" );
        gnc_pop_locale( LC_NUMERIC );
        return GNC_DBI_FAIL_SETUP;
    }
    while ( dbi_result_next_row( result ))
    {
        resultlonglong = dbi_result_get_longlong( result, "test_int" );
        resultulonglong = dbi_result_get_ulonglong( result, "test_unsigned" );
        resultdouble = dbi_result_get_double( result, "test_double" );
    }
    gnc_pop_locale( LC_NUMERIC );
    if ( testlonglong != resultlonglong )
    {
        PWARN( "Test_DBI_Library: LongLong Failed %" G_GINT64_FORMAT " != % " G_GINT64_FORMAT,
               testlonglong, resultlonglong );
        retval = GNC_DBI_FAIL_TEST;
    }
    if ( testulonglong != resultulonglong )
    {
        PWARN( "Test_DBI_Library: Unsigned longlong Failed %" G_GUINT64_FORMAT " != %" G_GUINT64_FORMAT,
               testulonglong, resultulonglong );
        retval = GNC_DBI_FAIL_TEST;
    }
    /* A bug in libdbi stores only 7 digits of precision */
    if ( testdouble >= resultdouble + 0.000001e307 ||
            testdouble <= resultdouble - 0.000001e307 )
    {
        PWARN( "Test_DBI_Library: Double Failed %17e != %17e",
               testdouble, resultdouble );
        retval = GNC_DBI_FAIL_TEST;
    }
    return retval;
}


static GncSqlConnection*
create_dbi_connection( /*@ observer @*/ provider_functions_t* provider,
                                        /*@ observer @*/ QofBackend* qbe,
                                        /*@ observer @*/ dbi_conn conn )
{
    GncDbiSqlConnection* dbi_conn;

    dbi_conn = g_new0( GncDbiSqlConnection, 1 );
    g_assert( dbi_conn != NULL );

    dbi_conn->base.dispose = conn_dispose;
    dbi_conn->base.executeSelectStatement = conn_execute_select_statement;
    dbi_conn->base.executeNonSelectStatement = conn_execute_nonselect_statement;
    dbi_conn->base.createStatementFromSql = conn_create_statement_from_sql;
    dbi_conn->base.doesTableExist = conn_does_table_exist;
    dbi_conn->base.beginTransaction = conn_begin_transaction;
    dbi_conn->base.rollbackTransaction = conn_rollback_transaction;
    dbi_conn->base.commitTransaction = conn_commit_transaction;
    dbi_conn->base.createTable = conn_create_table;
    dbi_conn->base.createIndex = conn_create_index;
    dbi_conn->base.addColumnsToTable = conn_add_columns_to_table;
    dbi_conn->base.quoteString = conn_quote_string;
    dbi_conn->qbe = qbe;
    dbi_conn->conn = conn;
    dbi_conn->provider = provider;
    dbi_conn->conn_ok = TRUE;
    gnc_dbi_init_error(dbi_conn);

    return (GncSqlConnection*)dbi_conn;
}

/* ========================== END OF FILE ===================== */
