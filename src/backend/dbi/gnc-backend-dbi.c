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
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#if !HAVE_GMTIME_R
#include "gmtime_r.h"
#endif

#include <dbi/dbi.h>

#include "gnc-backend-sql.h"

#include "qof.h"
#include "qofquery-p.h"
#include "qofquerycore-p.h"
#include "Account.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "SX-book.h"
#include "Recurrence.h"

#include "gnc-gconf-utils.h"
#include "gnc-uri-utils.h"

#include "gnc-backend-dbi.h"

#ifdef S_SPLINT_S
#include "splint-defs.h"
#endif

#define TRANSACTION_NAME "trans"

static QofLogModule log_module = G_LOG_DOMAIN;

#define FILE_URI_TYPE "file"
#define FILE_URI_PREFIX (FILE_URI_TYPE "://")
#define SQLITE3_URI_TYPE "sqlite3"
#define SQLITE3_URI_PREFIX (SQLITE3_URI_TYPE "://")
#define PGSQL_DEFAULT_PORT 5432

typedef gchar* (*CREATE_TABLE_DDL_FN)( GncSqlConnection* conn,
                                       const gchar* table_name,
                                       const GList* col_info_list );
typedef GSList* (*GET_TABLE_LIST_FN)( dbi_conn conn, const gchar* dbname );
typedef struct
{
    CREATE_TABLE_DDL_FN		create_table_ddl;
    GET_TABLE_LIST_FN		get_table_list;
} provider_functions_t;

static /*@ null @*/ gchar* conn_create_table_ddl_sqlite3( GncSqlConnection* conn,
        const gchar* table_name,
        const GList* col_info_list );
static GSList* conn_get_table_list( dbi_conn conn, const gchar* dbname );
static provider_functions_t provider_sqlite3 =
{
    conn_create_table_ddl_sqlite3,
    conn_get_table_list
};
#define SQLITE3_TIMESPEC_STR_FORMAT "%04d%02d%02d%02d%02d%02d"

static /*@ null @*/ gchar* conn_create_table_ddl_mysql( GncSqlConnection* conn,
        const gchar* table_name,
        const GList* col_info_list );
static provider_functions_t provider_mysql =
{
    conn_create_table_ddl_mysql,
    conn_get_table_list
};
#define MYSQL_TIMESPEC_STR_FORMAT "%04d%02d%02d%02d%02d%02d"

static /*@ null @*/ gchar* conn_create_table_ddl_pgsql( GncSqlConnection* conn,
        const gchar* table_name,
        const GList* col_info_list );
static GSList* conn_get_table_list_pgsql( dbi_conn conn, const gchar* dbname );
static provider_functions_t provider_pgsql =
{
    conn_create_table_ddl_pgsql,
    conn_get_table_list_pgsql
};
#define PGSQL_TIMESPEC_STR_FORMAT "%04d%02d%02d %02d%02d%02d"

static /*@ null @*/ gchar* create_index_ddl( GncSqlConnection* conn,
        const gchar* index_name,
        const gchar* table_name,
        const GncSqlColumnTableEntry* col_table );
static GncSqlConnection* create_dbi_connection( /*@ observer @*/ provider_functions_t* provider, /*@ observer @*/ QofBackend* qbe, /*@ observer @*/ dbi_conn conn );

#define GNC_DBI_PROVIDER_SQLITE (&provider_sqlite3)
#define GNC_DBI_PROVIDER_MYSQL (&provider_mysql)
#define GNC_DBI_PROVIDER_PGSQL (&provider_pgsql)

struct GncDbiBackend_struct
{
    GncSqlBackend sql_be;

    dbi_conn conn;

    /*@ dependent @*/
    QofBook *primary_book;	/* The primary, main open book */
    gboolean	loading;		/* We are performing an initial load */
    gboolean  in_query;
    gboolean  supports_transactions;
    gboolean  is_pristine_db;	// Are we saving to a new pristine db?
    gboolean  exists;         // Does the database exist?

    gint obj_total;			// Total # of objects (for percentage calculation)
    gint operations_done;		// Number of operations (save/load) done
//  GHashTable* versions;		// Version number for each table
};
typedef struct GncDbiBackend_struct GncDbiBackend;

typedef struct
{
    GncSqlConnection base;

    /*@ observer @*/
    QofBackend* qbe;
    /*@ observer @*/
    dbi_conn conn;
    /*@ observer @*/
    provider_functions_t* provider;
    gint last_error;        // Code of the last error that occurred. This is set in the error callback function
    gint error_repeat;      // Used in case of transient errors. After such error, another attempt at the
    // original call is allowed. error_repeat tracks the number of attempts and can
    // be used to prevent infinite loops.
    gboolean retry;         // Signals the calling function that it should retry (the error handler detected
    // transient error and managed to resolve it, but it can't run the original query)
} GncDbiSqlConnection;

#define DBI_MAX_CONN_ATTEMPTS 5

/* ================================================================= */

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
    GncDbiBackend *be = (GncDbiBackend*)user_data;
    GncDbiSqlConnection *dbi_conn = (GncDbiSqlConnection*)be->sql_be.conn;
    const gchar* msg;

    (void)dbi_conn_error( conn, &msg );
    PERR( "DBI error: %s\n", msg );
    gnc_dbi_set_error( dbi_conn, ERR_BACKEND_MISC, 0, FALSE );
}

static void
gnc_dbi_sqlite3_session_begin( QofBackend *qbe, QofSession *session,
                               const gchar *book_id,
                               /*@ unused @*/ gboolean ignore_lock,
                               gboolean create_if_nonexistent )
{
    GncDbiBackend *be = (GncDbiBackend*)qbe;
    gint result;
    gchar* dirname;
    gchar* basename;
    gchar *filepath = NULL;

    g_return_if_fail( qbe != NULL );
    g_return_if_fail( session != NULL );
    g_return_if_fail( book_id != NULL );

    ENTER (" ");

    /* Remove uri type if present */
    filepath = gnc_uri_get_path ( book_id );

    if ( !create_if_nonexistent
            && !g_file_test( filepath, G_FILE_TEST_IS_REGULAR | G_FILE_TEST_EXISTS ) )
    {
        qof_backend_set_error( qbe, ERR_FILEIO_FILE_NOT_FOUND );
        LEAVE(" ");
        return;
    }

    if ( be->conn != NULL )
    {
        dbi_conn_close( be->conn );
    }
    be->conn = dbi_conn_new( "sqlite3" );
    if ( be->conn == NULL )
    {
        PERR( "Unable to create sqlite3 dbi connection\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_BAD_URL );
        LEAVE( " " );
        return;
    }

    dirname = g_path_get_dirname( filepath );
    basename = g_path_get_basename( filepath );
    g_free ( filepath );
    dbi_conn_error_handler( be->conn, sqlite3_error_fn, be );
    result = dbi_conn_set_option( be->conn, "host", "localhost" );
    if ( result < 0 )
    {
        PERR( "Error setting 'host' option\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
        LEAVE( " " );
        return;
    }
    result = dbi_conn_set_option( be->conn, "dbname", basename );
    if ( result < 0 )
    {
        PERR( "Error setting 'dbname' option\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
        LEAVE( " " );
        return;
    }
    result = dbi_conn_set_option( be->conn, "sqlite3_dbdir", dirname );
    if ( result < 0 )
    {
        PERR( "Error setting 'sqlite3_dbdir' option\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
        LEAVE( " " );
        return;
    }
    result = dbi_conn_connect( be->conn );
    g_free( basename );
    g_free( dirname );
    if ( result < 0 )
    {
        PERR( "Unable to connect to %s: %d\n", book_id, result );
        qof_backend_set_error( qbe, ERR_BACKEND_BAD_URL );
        LEAVE( " " );
        return;
    }

    if ( be->sql_be.conn != NULL )
    {
        gnc_sql_connection_dispose( be->sql_be.conn );
    }
    be->sql_be.conn = create_dbi_connection( GNC_DBI_PROVIDER_SQLITE, qbe, be->conn );
    be->sql_be.timespec_format = SQLITE3_TIMESPEC_STR_FORMAT;

    LEAVE (" ");
}

static void
mysql_error_fn( dbi_conn conn, void* user_data )
{
    GncDbiBackend *be = (GncDbiBackend*)user_data;
    GncDbiSqlConnection *dbi_conn = (GncDbiSqlConnection*)be->sql_be.conn;
    const gchar* msg;
    gint err_num;

    err_num = dbi_conn_error( conn, &msg );
    if ( err_num == 1049 )          // Database doesn't exist
    {
        PINFO( "DBI error: %s\n", msg );
        be->exists = FALSE;
        gnc_dbi_set_error( dbi_conn, ERR_BACKEND_NO_SUCH_DB, 0, FALSE );
    }
    else if ( err_num == 2006 )     // Server has gone away
    {
        if (dbi_conn->error_repeat > DBI_MAX_CONN_ATTEMPTS )
        {
            PERR( "DBI error: %s - Failed to reconnect after %d attempts.\n", msg, DBI_MAX_CONN_ATTEMPTS );
            gnc_dbi_set_error( dbi_conn, ERR_BACKEND_CANT_CONNECT, 0, FALSE );
        }
        else
        {
            PINFO( "DBI error: %s - Reconnecting...\n", msg );
            gnc_dbi_set_error( dbi_conn, ERR_BACKEND_CONN_LOST, 1, TRUE );

            (void)dbi_conn_connect( conn );
        }
    }
    else                            // Any other error
    {
        PERR( "DBI error: %s\n", msg );
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

    return TRUE;
}

static void
gnc_dbi_mysql_session_begin( QofBackend* qbe, QofSession *session,
                             const gchar *book_id,
                             /*@ unused @*/gboolean ignore_lock,
                             gboolean create_if_nonexistent )
{
    GncDbiBackend *be = (GncDbiBackend*)qbe;
    gchar* protocol = NULL;
    gchar* host = NULL;
    gchar* dbname = NULL;
    gchar* username = NULL;
    gchar* password = NULL;
    gint portnum = 0;
    gint result;
    gboolean success = FALSE;

    g_return_if_fail( qbe != NULL );
    g_return_if_fail( session != NULL );
    g_return_if_fail( book_id != NULL );

    ENTER (" ");

    /* Split the book-id
     * Format is protocol://username:password@hostname:port/dbname
       where username, password and port are optional) */
    gnc_uri_get_components ( book_id, &protocol, &host, &portnum,
                             &username, &password, &dbname );

    // Try to connect to the db.  If it doesn't exist and the create_if_nonexistent
    // flag is TRUE, we'll need to connect to the 'mysql' db and execute the
    // CREATE DATABASE ddl statement there.
    if ( be->conn != NULL )
    {
        dbi_conn_close( be->conn );
    }
    be->conn = dbi_conn_new( "mysql" );
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
        success = TRUE;
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
        if ( create_if_nonexistent )
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
            be->conn = dbi_conn_new( "mysql" );
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
            success = TRUE;
        }
        else
        {
            qof_backend_set_error( qbe, ERR_BACKEND_NO_SUCH_DB );
        }
    }

    if ( success )
    {
        if ( be->sql_be.conn != NULL )
        {
            gnc_sql_connection_dispose( be->sql_be.conn );
        }
        be->sql_be.conn = create_dbi_connection( GNC_DBI_PROVIDER_MYSQL, qbe, be->conn );
    }
    be->sql_be.timespec_format = MYSQL_TIMESPEC_STR_FORMAT;
exit:
    g_free( protocol );
    g_free( host );
    g_free( username );
    g_free( password );
    g_free( dbname );

    LEAVE (" ");
}

static void
pgsql_error_fn( dbi_conn conn, void* user_data )
{
    GncDbiBackend *be = (GncDbiBackend*)user_data;
    GncDbiSqlConnection *dbi_conn = (GncDbiSqlConnection*)be->sql_be.conn;
    const gchar* msg;

    (void)dbi_conn_error( conn, &msg );
    if ( g_str_has_prefix( msg, "FATAL:  database" ) &&
            g_str_has_suffix( msg, "does not exist\n" ) )
    {
        PINFO( "DBI error: %s\n", msg );
        be->exists = FALSE;
        gnc_dbi_set_error( dbi_conn, ERR_BACKEND_NO_SUCH_DB, 0, FALSE );
    }
    else
    {
        PERR( "DBI error: %s\n", msg );
        gnc_dbi_set_error( dbi_conn, ERR_BACKEND_MISC, 0, FALSE );
    }
}

static void
gnc_dbi_postgres_session_begin( QofBackend *qbe, QofSession *session,
                                const gchar *book_id,
                                /*@ unused @*/ gboolean ignore_lock,
                                gboolean create_if_nonexistent )
{
    GncDbiBackend *be = (GncDbiBackend*)qbe;
    gint result = 0;
    gchar* protocol = NULL;
    gchar* host = NULL;
    gchar* dbname = NULL;
    gchar* username = NULL;
    gchar* password = NULL;
    gboolean success = FALSE;
    gint portnum = 0;

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

    // Try to connect to the db.  If it doesn't exist and the create_if_nonexistent
    // flag is TRUE, we'll need to connect to the 'postgres' db and execute the
    // CREATE DATABASE ddl statement there.
    if ( be->conn != NULL )
    {
        dbi_conn_close( be->conn );
    }
    be->conn = dbi_conn_new( "pgsql" );
    if ( be->conn == NULL )
    {
        PERR( "Unable to create pgsql dbi connection\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_BAD_URL );
        goto exit;
    }
    dbi_conn_error_handler( be->conn, pgsql_error_fn, be );
    if ( !set_standard_connection_options( qbe, be->conn, host, portnum, dbname, username, password ) )
    {
        goto exit;
    }
    be->exists = TRUE;
    result = dbi_conn_connect( be->conn );
    if ( result == 0 )
    {
        success = TRUE;
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
        if ( create_if_nonexistent )
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
            dresult = dbi_conn_queryf( be->conn, "CREATE DATABASE %s WITH ENCODING 'UTF8'", dbname );
            if ( dresult == NULL )
            {
                PERR( "Unable to create database '%s'\n", dbname );
                qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
                goto exit;
            }
            dbi_conn_close( be->conn );

            // Try again to connect to the db
            be->conn = dbi_conn_new( "pgsql" );
            if ( be->conn == NULL )
            {
                PERR( "Unable to create pgsql dbi connection\n" );
                qof_backend_set_error( qbe, ERR_BACKEND_BAD_URL );
                goto exit;
            }
            dbi_conn_error_handler( be->conn, pgsql_error_fn, be );
            if ( !set_standard_connection_options( qbe, be->conn, host, PGSQL_DEFAULT_PORT, dbname, username, password ) )
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
            success = TRUE;
        }
        else
        {
            qof_backend_set_error( qbe, ERR_BACKEND_NO_SUCH_DB );
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
exit:
    g_free( protocol );
    g_free( host );
    g_free( username );
    g_free( password );
    g_free( dbname );

    LEAVE (" ");
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

    qof_backend_destroy( be );

    g_free( be );
}

/* ================================================================= */

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
        gnc_sql_init_version_info( &be->sql_be );

        // Call all object backends to create any required tables
        qof_object_foreach_backend( GNC_SQL_BACKEND, create_tables_cb, be );
    }

    gnc_sql_load( &be->sql_be, book, loadType );

    LEAVE( "" );
}

/* ================================================================= */

static gboolean
gnc_dbi_save_may_clobber_data( QofBackend* qbe )
{
    GncDbiBackend* be = (GncDbiBackend*)qbe;
    const gchar* dbname;
    GSList* table_name_list;
    gint numTables = 0;
    gint status;

    /* Data may be clobbered iff the number of tables != 0 */
    dbname = dbi_conn_get_option( be->conn, "dbname" );
    table_name_list = ((GncDbiSqlConnection*)(be->sql_be.conn))->provider->get_table_list( be->conn, dbname );
    if ( table_name_list != NULL )
    {
        GSList* node;
        numTables = g_slist_length( table_name_list );
        for ( node = table_name_list; node != NULL; node = node->next )
        {
            g_free( node->data );
        }
        g_slist_free( table_name_list );
    }

    return (numTables != 0);
}

static void
gnc_dbi_sync_all( QofBackend* qbe, /*@ dependent @*/ QofBook *book )
{
    GncDbiBackend* be = (GncDbiBackend*)qbe;
    GSList* table_name_list;
    const gchar* dbname;
    gint status;

    g_return_if_fail( be != NULL );
    g_return_if_fail( book != NULL );

    ENTER( "book=%p, primary=%p", book, be->primary_book );

    /* Destroy the current contents of the database */
    dbname = dbi_conn_get_option( be->conn, "dbname" );
    table_name_list = ((GncDbiSqlConnection*)(be->sql_be.conn))->provider->get_table_list( be->conn, dbname );
    if ( table_name_list != NULL )
    {
        GSList* node;

        for ( node = table_name_list; node != NULL; node = node->next )
        {
            const gchar* table_name = (const gchar*)node->data;
            dbi_result result;

            do
            {
                gnc_dbi_init_error( ((GncDbiSqlConnection*)(be->sql_be.conn)) );
                result = dbi_conn_queryf( be->conn, "DROP TABLE %s", table_name );
            }
            while ( ((GncDbiSqlConnection*)(be->sql_be.conn))->retry );
            if ( result != NULL )
            {
                status = dbi_result_free( result );
                if ( status < 0 )
                {
                    PERR( "Error in dbi_result_free() result\n" );
                    qof_backend_set_error( qbe, ERR_BACKEND_SERVER_ERR );
                }
            }
            g_free( node->data );
        }
        g_slist_free( table_name_list );
    }

    /* Save all contents */
    be->is_pristine_db = TRUE;
    be->primary_book = book;
    gnc_sql_sync_all( &be->sql_be, book );

    LEAVE( "book=%p", book );
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
    be->save_may_clobber_data = gnc_dbi_save_may_clobber_data;

    /* The gda backend treats accounting periods transactionally. */
    be->begin = gnc_dbi_begin_edit;
    be->commit = gnc_dbi_commit_edit;
    be->rollback = gnc_dbi_rollback_edit;

    be->counter = NULL;

    /* The gda backend will not be multi-user (for now)... */
    be->events_pending = NULL;
    be->process_events = NULL;

    be->sync = gnc_dbi_sync_all;
    be->load_config = NULL;
    be->get_config = NULL;

    be->compile_query = gnc_sql_compile_query;
    be->run_query = gnc_sql_run_query;
    be->free_query = gnc_sql_free_query;

    be->export_fn = NULL;

    gnc_sql_init( &dbi_be->sql_be );
}

static QofBackend*
new_backend( void (*session_begin)( QofBackend *, QofSession *, const gchar *,
                                    /*@ unused @*/ gboolean, /*@ unused @*/ gboolean ) )
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
    size_t chars_read;
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
#define DEFAULT_DBD_DIR "/usr/lib/dbd"
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
        PWARN( "GNC_DBD_DIR not set: using %s\n", DEFAULT_DBD_DIR );
        driver_dir = DEFAULT_DBD_DIR;
    }

    num_drivers = dbi_initialize( driver_dir );
    if ( num_drivers == 0 )
    {
        PWARN( "No DBD drivers found\n" );
    }
    else
    {
        dbi_driver driver = NULL;
        PINFO( "%d DBD drivers found\n", num_drivers );

        do
        {
            driver = dbi_driver_list( driver );
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
        prov->partial_book_supported = FALSE;
        prov->backend_new = gnc_dbi_backend_sqlite3_new;
        prov->provider_free = gnc_dbi_provider_free;
        prov->check_data_type = gnc_dbi_check_sqlite3_file;
        qof_backend_register_provider( prov );

        prov = g_new0( QofBackendProvider, 1 );
        g_assert( prov != NULL );

        prov->provider_name = "GnuCash Libdbi (SQLITE3) Backend";
        prov->access_method = SQLITE3_URI_TYPE;
        prov->partial_book_supported = FALSE;
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
        prov->partial_book_supported = FALSE;
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
        prov->partial_book_supported = FALSE;
        prov->backend_new = gnc_dbi_backend_postgres_new;
        prov->provider_free = gnc_dbi_provider_free;
        prov->check_data_type = NULL;
        qof_backend_register_provider( prov );
    }

    /* For now, set log level to DEBUG so that SQl statements will be put into
       the gnucash.trace file. */
    qof_log_set_level( log_module, QOF_LOG_DEBUG );
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
    dbi_shutdown();
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
            GValue* value = (GValue*)node->data;
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
    time_t time;
    struct tm tm_struct;

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
            time = dbi_result_get_datetime( dbi_row->result, col_name );
            (void)gmtime_r( &time, &tm_struct );
            (void)g_value_init( value, G_TYPE_STRING );
            g_value_take_string( value,
                                 g_strdup_printf( "%d%02d%02d%02d%02d%02d",
                                                  1900 + tm_struct.tm_year, tm_struct.tm_mon + 1, tm_struct.tm_mday,
                                                  tm_struct.tm_hour, tm_struct.tm_min, tm_struct.tm_sec ) );
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

static GValue*
create_gvalue_from_string( /*@ only @*/ gchar* s )
{
    GValue* s_gval;

    s_gval = g_new0( GValue, 1 );
    g_assert( s_gval != NULL );

    (void)g_value_init( s_gval, G_TYPE_STRING );
    g_value_take_string( s_gval, s );

    return s_gval;
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

    DEBUG( "BEGIN\n" );

    do
    {
        gnc_dbi_init_error( dbi_conn );
        result = dbi_conn_queryf( dbi_conn->conn, "BEGIN" );
    }
    while ( dbi_conn->retry );

    status = dbi_result_free( result );
    if ( status < 0 )
    {
        PERR( "Error in dbi_result_free() result\n" );
        qof_backend_set_error( dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
    }

    return TRUE;
}

static gboolean
conn_rollback_transaction( /*@ unused @*/ GncSqlConnection* conn )
{
    GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
    dbi_result result;
    gint status;

    DEBUG( "ROLLBACK\n" );
    result = dbi_conn_queryf( dbi_conn->conn, "ROLLBACK" );
    status = dbi_result_free( result );
    if ( status < 0 )
    {
        PERR( "Error in dbi_result_free() result\n" );
        qof_backend_set_error( dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
    }

    return TRUE;
}

static gboolean
conn_commit_transaction( /*@ unused @*/ GncSqlConnection* conn )
{
    GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
    dbi_result result;
    gint status;

    DEBUG( "COMMIT\n" );
    result = dbi_conn_queryf( dbi_conn->conn, "COMMIT" );
    status = dbi_result_free( result );
    if ( status < 0 )
    {
        PERR( "Error in dbi_result_free() result\n" );
        qof_backend_set_error( dbi_conn->qbe, ERR_BACKEND_SERVER_ERR );
    }

    return TRUE;
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
conn_create_table_ddl_sqlite3( GncSqlConnection* conn,
                               const gchar* table_name,
                               const GList* col_info_list )
{
    GString* ddl;
    const GList* list_node;
    guint col_num;
    gchar* type_name;

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
        g_free( info->name );
        g_free( info );
    }
    (void)g_string_append( ddl, ")" );

    return g_string_free( ddl, FALSE );
}

static /*@ null @*/ gchar*
conn_create_table_ddl_mysql( GncSqlConnection* conn, const gchar* table_name,
                             const GList* col_info_list )
{
    GString* ddl;
    const GList* list_node;
    guint col_num;
    gchar* type_name;

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
            type_name = "timestamp";
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
        g_free( info->name );
        g_free( info );
    }
    (void)g_string_append( ddl, ")" );

    return g_string_free( ddl, FALSE );
}

static /*@ null @*/ gchar*
conn_create_table_ddl_pgsql( GncSqlConnection* conn, const gchar* table_name,
                             const GList* col_info_list )
{
    GString* ddl;
    const GList* list_node;
    guint col_num;
    gchar* type_name;
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
    gint status;

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
        dbi_result result;

        table_name = dbi_result_get_string_idx( tables, 1 );
        list = g_slist_prepend( list, strdup( table_name ) );
    }
    dbi_result_free( tables );
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
    dbi_conn->base.quoteString = conn_quote_string;
    dbi_conn->qbe = qbe;
    dbi_conn->conn = conn;
    dbi_conn->provider = provider;

    gnc_dbi_init_error(dbi_conn);

    return (GncSqlConnection*)dbi_conn;
}

/* ========================== END OF FILE ===================== */
