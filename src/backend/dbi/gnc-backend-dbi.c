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

#include <dbi/dbi.h>

#include "gnc-backend-util-sql.h"
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

#include "gnc-backend-dbi.h"

#define TRANSACTION_NAME "trans"

static QofLogModule log_module = G_LOG_DOMAIN;

static GncSqlConnection* create_dbi_connection( dbi_conn conn );

/* ================================================================= */

static void
create_tables_cb( const gchar* type, gpointer data_p, gpointer be_p )
{
    GncSqlDataType_t* pData = data_p;
    GncDbiBackend* be = be_p;

    g_return_if_fail( type != NULL && data_p != NULL && be_p != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );

    if( pData->create_tables != NULL ) {
        (pData->create_tables)( &be->sql_be );
    }
}

static void
error_fn( dbi_conn conn, void* user_data )
{
    GncDbiBackend *be = (GncDbiBackend*)user_data;
	const gchar* msg;

	dbi_conn_error( conn, &msg );
	PERR( "DBI error: %s\n", msg );
}

static void
gnc_dbi_session_begin( QofBackend *qbe, QofSession *session, 
	                   const gchar *book_id,
                       gboolean ignore_lock,
				       gboolean create_if_nonexistent )
{
    GncDbiBackend *be = (GncDbiBackend*)qbe;
    GError* error = NULL;
    gchar* dsn;
    gchar* username;
    gchar* password;
	gchar* provider;
	gboolean uriOK;
	gint result;
	gchar* dirname;
	gchar* basename;

	g_return_if_fail( qbe != NULL );
	g_return_if_fail( session != NULL );
	g_return_if_fail( book_id != NULL );

    ENTER (" ");

	dirname = g_path_get_dirname( book_id );
	basename = g_path_get_basename( book_id );

	be->conn = dbi_conn_new( "sqlite3" );
	if( be->conn == NULL ) {
		PERR( "Unable to create sqlite3 dbi connection\n" );
        qof_backend_set_error( qbe, ERR_BACKEND_BAD_URL );
		LEAVE( " " );
		return;
	}
	dbi_conn_error_handler( be->conn, error_fn, be );
	dbi_conn_set_option( be->conn, "host", "localhost" );
	dbi_conn_set_option( be->conn, "dbname", basename );
	dbi_conn_set_option( be->conn, "sqlite3_dbdir", dirname );
	result = dbi_conn_connect( be->conn );
	g_free( basename );
	g_free( dirname );
	if( result < 0 ) {
		PERR( "Unable to connect to %s: %d\n", book_id, result );
        qof_backend_set_error( qbe, ERR_BACKEND_BAD_URL );
        LEAVE( " " );
        return;
	}

	be->sql_be.conn = create_dbi_connection( be->conn );

    LEAVE (" ");
}

/* ================================================================= */

static void
gnc_dbi_session_end( QofBackend *be_start )
{
    GncDbiBackend *be = (GncDbiBackend*)be_start;

	g_return_if_fail( be_start != NULL );

    ENTER (" ");

    dbi_conn_close( be->conn );
	gnc_sql_finalize_version_info( &be->sql_be );

    LEAVE (" ");
}

static void
gnc_dbi_destroy_backend( QofBackend *be )
{
	g_return_if_fail( be != NULL );

    g_free( be );
}

/* ================================================================= */

static void
gnc_dbi_load( QofBackend* qbe, QofBook *book )
{
    GncDbiBackend *be = (GncDbiBackend*)qbe;
    GncSqlDataType_t* pData;
	int i;
	Account* root;

	g_return_if_fail( qbe != NULL );
	g_return_if_fail( book != NULL );

    ENTER( "be=%p, book=%p", be, book );

    g_assert( be->primary_book == NULL );
    be->primary_book = book;

	// Set up table version information
	gnc_sql_init_version_info( &be->sql_be );

    // Call all object backends to create any required tables
    qof_object_foreach_backend( GNC_SQL_BACKEND, create_tables_cb, be );

	gnc_sql_load( &be->sql_be, book );

    LEAVE( "" );
}

/* ================================================================= */

static gboolean
gnc_dbi_save_may_clobber_data( QofBackend* qbe )
{
    GncDbiBackend* be = (GncDbiBackend*)qbe;
	const gchar* dbname;
	dbi_result tables;
	gint numTables;

	/* Data may be clobbered iff the number of tables != 0 */
	dbname = dbi_conn_get_option( be->conn, "dbname" );
	tables = dbi_conn_get_table_list( be->conn, dbname, NULL );
    numTables = dbi_result_get_numrows( tables );
	dbi_result_free( tables );

	return (numTables != 0);
}

static void
gnc_dbi_sync_all( QofBackend* fbe, QofBook *book )
{
    GncDbiBackend* be = (GncDbiBackend*)fbe;
    dbi_result tables;
    GError* error = NULL;
    gint row;
    gint numTables;
	gboolean status;
	const gchar* dbname;

	g_return_if_fail( be != NULL );
	g_return_if_fail( book != NULL );

    ENTER( "book=%p, primary=%p", book, be->primary_book );

    /* Destroy the current contents of the database */
	dbname = dbi_conn_get_option( be->conn, "dbname" );
	tables = dbi_conn_get_table_list( be->conn, dbname, NULL );
	while( dbi_result_next_row( tables ) ) {
		const gchar* table_name;
		dbi_result result;

		table_name = dbi_result_get_string_idx( tables, 1 );
		result = dbi_conn_queryf( be->conn, "DROP TABLE %s", table_name );
	}

    /* Save all contents */
	be->is_pristine_db = TRUE;
	be->primary_book = book;
	gnc_sql_sync_all( &be->sql_be, book );

    LEAVE( "book=%p", book );
}

/* ================================================================= */

static QofBackend*
gnc_dbi_backend_new(void)
{
    GncDbiBackend *gnc_be;
    QofBackend *be;
    static gboolean initialized = FALSE;

    gnc_be = g_new0(GncDbiBackend, 1);
    be = (QofBackend*) gnc_be;
    qof_backend_init(be);

    be->session_begin = gnc_dbi_session_begin;
    be->session_end = gnc_dbi_session_end;
    be->destroy_backend = gnc_dbi_destroy_backend;

    be->load = gnc_dbi_load;
    be->save_may_clobber_data = gnc_dbi_save_may_clobber_data;

    /* The gda backend treats accounting periods transactionally. */
    be->begin = gnc_sql_begin_edit;
    be->commit = gnc_sql_commit_edit;
    be->rollback = gnc_sql_rollback_edit;

    be->counter = NULL;

    /* The gda backend will not be multi-user (for now)... */
    be->events_pending = NULL;
    be->process_events = NULL;

    be->sync = gnc_dbi_sync_all;
    be->load_config = NULL;
    be->get_config = NULL;

    be->export = NULL;

    gnc_be->primary_book = NULL;

    if( !initialized ) {
#define DEFAULT_DBD_DIR "/usr/lib/dbd"
		const gchar* driver_dir;
        int num_drivers;

		driver_dir = g_getenv( "GNC_DBD_DIR" );
		if( driver_dir == NULL ) {
			PWARN( "GNC_DBD_DIR not set: using %s\n", DEFAULT_DBD_DIR );
			driver_dir = DEFAULT_DBD_DIR;
		}

        num_drivers = dbi_initialize( driver_dir );
		gnc_sql_init( &gnc_be->sql_be );
        initialized = TRUE;
    }

    return be;
}

static void
gnc_dbi_provider_free( QofBackendProvider *prov )
{
	g_return_if_fail( prov != NULL );

    prov->provider_name = NULL;
    prov->access_method = NULL;
    g_free (prov);
}

/*
 * Checks to see whether the file is an sqlite file or not
 *
 */
static gboolean
gnc_dbi_check_sqlite_file( const gchar *path )
{
	FILE* f;
	gchar buf[50];

	// BAD if the path is null
	g_return_val_if_fail( path != NULL, FALSE );

	f = g_fopen( path, "r" );

	// OK if the file doesn't exist - new file
	if( f == NULL ) {
		PINFO( "doesn't exist (errno=%d) -> DBI", errno );
		return TRUE;
	}

	// OK if file has the correct header
	fread( buf, sizeof(buf), 1, f );
	fclose( f );
	if( g_str_has_prefix( buf, "SQLite format 3" ) ) {
		PINFO( "has SQLite format string -> DBI" );
		return TRUE;
	}
	PINFO( "exists, does not have SQLite format string -> not DBI" );

	// Otherwise, BAD
	return FALSE;
}

G_MODULE_EXPORT void
qof_backend_module_init(void)
{
    QofBackendProvider *prov;

    prov = g_new0 (QofBackendProvider, 1);
    prov->provider_name = "GnuCash Libdbi Backend";
    prov->access_method = "file";
    prov->partial_book_supported = FALSE;
    prov->backend_new = gnc_dbi_backend_new;
    prov->provider_free = gnc_dbi_provider_free;
    prov->check_data_type = gnc_dbi_check_sqlite_file;
    qof_backend_register_provider (prov);
}

/* --------------------------------------------------------- */
typedef struct
{
	GncSqlRow base;

	dbi_result result;
	GList* gvalue_list;
} GncDbiSqlRow;

static void
row_dispose( GncSqlRow* row )
{
	GncDbiSqlRow* dbi_row = (GncDbiSqlRow*)row;
	GList* node;

	if( dbi_row->gvalue_list != NULL ) {
		for( node = dbi_row->gvalue_list; node != NULL; node = node->next ) {
			GValue* value = (GValue*)node->data;
			if( G_VALUE_HOLDS_STRING(value) ) {
				g_free( (gpointer)g_value_get_string( value ) );
			}
			g_free( value );
		}
		g_list_free( dbi_row->gvalue_list );
	}
	g_free( dbi_row );
}

static const GValue*
row_get_value_at_col_name( GncSqlRow* row, const gchar* col_name )
{
	GncDbiSqlRow* dbi_row = (GncDbiSqlRow*)row;
	gushort type;
	GValue* value;

	type = dbi_result_get_field_type( dbi_row->result, col_name );
	value = g_new0( GValue, 1 );
	switch( type ) {
		case DBI_TYPE_INTEGER:
			g_value_init( value, G_TYPE_INT );
			g_value_set_int( value, dbi_result_get_int( dbi_row->result, col_name ) );
			break;
		case DBI_TYPE_DECIMAL:
			g_value_init( value, G_TYPE_DOUBLE );
			g_value_set_double( value, dbi_result_get_double( dbi_row->result, col_name ) );
			break;
		case DBI_TYPE_STRING:
			g_value_init( value, G_TYPE_STRING );
			g_value_take_string( value, dbi_result_get_string_copy( dbi_row->result, col_name ) );
			break;
		default:
			PERR( "Unknown DBI_TYPE: %d\n", type );
			g_free( value );
			return NULL;
	}
	
	dbi_row->gvalue_list = g_list_prepend( dbi_row->gvalue_list, value );
	return value;
}

static GncSqlRow*
create_dbi_row( dbi_result result )
{
	GncDbiSqlRow* row;

	row = g_new0( GncDbiSqlRow, 1 );
	row->base.getValueAtColName = row_get_value_at_col_name;
	row->base.dispose = row_dispose;
	row->result = result;

	return (GncSqlRow*)row;
}
/* --------------------------------------------------------- */
typedef struct
{
	GncSqlResult base;

	dbi_result result;
	gint num_rows;
	gint cur_row;
	GncSqlRow* row;
} GncDbiSqlResult;

static void
result_dispose( GncSqlResult* result )
{
	GncDbiSqlResult* dbi_result = (GncDbiSqlResult*)result;

	if( dbi_result->row != NULL ) {
		gnc_sql_row_dispose( dbi_result->row );
	}
	if( dbi_result->result != NULL ) {
		dbi_result_free( dbi_result->result );
	}
	g_free( result );
}

static gint
result_get_num_rows( GncSqlResult* result )
{
	GncDbiSqlResult* dbi_result = (GncDbiSqlResult*)result;

	return dbi_result->num_rows;
}

static GncSqlRow*
result_get_first_row( GncSqlResult* result )
{
	GncDbiSqlResult* dbi_result = (GncDbiSqlResult*)result;

	if( dbi_result->row != NULL ) {
		gnc_sql_row_dispose( dbi_result->row );
		dbi_result->row = NULL;
	}
	if( dbi_result->num_rows > 0 ) {
		dbi_result_first_row( dbi_result->result );
		dbi_result->cur_row = 1;
		dbi_result->row = create_dbi_row( dbi_result->result );
		return dbi_result->row;
	} else {
		return NULL;
	}
}

static GncSqlRow*
result_get_next_row( GncSqlResult* result )
{
	GncDbiSqlResult* dbi_result = (GncDbiSqlResult*)result;

	if( dbi_result->row != NULL ) {
		gnc_sql_row_dispose( dbi_result->row );
		dbi_result->row = NULL;
	}
	if( dbi_result->cur_row < dbi_result->num_rows ) {
		dbi_result_next_row( dbi_result->result );
		dbi_result->cur_row++;
		dbi_result->row = create_dbi_row( dbi_result->result );
		return dbi_result->row;
	} else {
		return NULL;
	}
}

static GncSqlResult*
create_dbi_result( dbi_result result )
{
	GncDbiSqlResult* dbi_result;

	dbi_result = g_new0( GncDbiSqlResult, 1 );
	dbi_result->base.dispose = result_dispose;
	dbi_result->base.getNumRows = result_get_num_rows;
	dbi_result->base.getFirstRow = result_get_first_row;
	dbi_result->base.getNextRow = result_get_next_row;
	dbi_result->result = result;
	dbi_result->num_rows = dbi_result_get_numrows( result );
	dbi_result->cur_row = 0;

	return (GncSqlResult*)dbi_result;
}
/* --------------------------------------------------------- */
typedef struct
{
	GncSqlStatement base;

	GString* sql;
	GncSqlConnection* conn;
} GncDbiSqlStatement;

static void
stmt_dispose( GncSqlStatement* stmt )
{
	GncDbiSqlStatement* dbi_stmt = (GncDbiSqlStatement*)stmt;

	if( dbi_stmt->sql != NULL ) {
		g_string_free( dbi_stmt->sql, TRUE );
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
stmt_add_where_cond( GncSqlStatement* stmt, QofIdTypeConst type_name,
					gpointer obj, const col_cvt_t* table_row, GValue* value )
{
	GncDbiSqlStatement* dbi_stmt = (GncDbiSqlStatement*)stmt;
	gchar* buf;

	buf = g_strdup_printf( " WHERE %s = %s", table_row->col_name,
						gnc_sql_get_sql_value( dbi_stmt->conn, value ) );
	g_string_append( dbi_stmt->sql, buf );
	g_free( buf );
}

static GncSqlStatement*
create_dbi_statement( GncSqlConnection* conn, gchar* sql )
{
	GncDbiSqlStatement* stmt;

	stmt = g_new0( GncDbiSqlStatement, 1 );
	stmt->base.dispose = stmt_dispose;
	stmt->base.toSql = stmt_to_sql;
	stmt->base.addWhereCond = stmt_add_where_cond;
	stmt->sql = g_string_new( sql );
	g_free( sql );
	stmt->conn = conn;

	return (GncSqlStatement*)stmt;
}
/* --------------------------------------------------------- */
typedef struct
{
	GncSqlConnection base;

	dbi_conn conn;
} GncDbiSqlConnection;

static void
conn_dispose( GncSqlConnection* conn )
{
	GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;

	g_free( conn );
}

static GncSqlResult*
conn_execute_select_statement( GncSqlConnection* conn, GncSqlStatement* stmt )
{
	GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
	GncDbiSqlStatement* dbi_stmt = (GncDbiSqlStatement*)stmt;
	dbi_result result;
														
	result = dbi_conn_query( dbi_conn->conn, dbi_stmt->sql->str );
	if( result == NULL ) {
		PERR( "Error executing SQL %s\n", dbi_stmt->sql->str );
		return NULL;
	}
	return create_dbi_result( result );
}

static gint
conn_execute_nonselect_statement( GncSqlConnection* conn, GncSqlStatement* stmt )
{
	GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
	GncDbiSqlStatement* dbi_stmt = (GncDbiSqlStatement*)stmt;
	dbi_result result;
	gint num_rows;

	result = dbi_conn_query( dbi_conn->conn, dbi_stmt->sql->str );
	if( result == NULL ) {
		PERR( "Error executing SQL %s\n", dbi_stmt->sql->str );
		return 0;
	}
	num_rows = dbi_result_get_numrows_affected( result );
	dbi_result_free( result );
	return num_rows;
}

static GncSqlStatement*
conn_create_statement_from_sql( GncSqlConnection* conn, gchar* sql )
{
	GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;

	return create_dbi_statement( conn, sql );
}

static GValue*
create_gvalue_from_string( gchar* s )
{
	GValue* s_gval;

	s_gval = g_new0( GValue, 1 );
	g_value_init( s_gval, G_TYPE_STRING );
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

	g_return_val_if_fail( conn != NULL, FALSE );
	g_return_val_if_fail( table_name != NULL, FALSE );

	dbname = dbi_conn_get_option( dbi_conn->conn, "dbname" );
	tables = dbi_conn_get_table_list( dbi_conn->conn, dbname, table_name );
	nTables = dbi_result_get_numrows( tables );
	dbi_result_free( tables );

	if( nTables == 1 ) {
		return TRUE;
	} else {
		return FALSE;
	}
}

static void
conn_begin_transaction( GncSqlConnection* conn )
{
	GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
}

static void
conn_rollback_transaction( GncSqlConnection* conn )
{
	GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
}

static void
conn_commit_transaction( GncSqlConnection* conn )
{
	GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
}

static const gchar*
conn_get_column_type_name( GncSqlConnection* conn, GType type, gint size )
{
	GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;

	switch( type ) {
		case G_TYPE_INT:
		case G_TYPE_INT64:
			return "integer";
			break;
		case G_TYPE_DOUBLE:
			return "double";
			break;
		case G_TYPE_STRING:
			return "string";
			break;
		default:
			PERR( "Unknown GType: %d\n", type );
			return "";
	}
}

static void
add_table_column( GString* ddl, const GncSqlColumnInfo* info )
{
    gchar* buf;
	GError* error = NULL;
	gboolean ok;

	g_return_if_fail( ddl != NULL );
	g_return_if_fail( info != NULL );

	g_string_append_printf( ddl, "%s %s", info->name, info->type_name );
    if( info->size != 0 ) {
		g_string_append_printf( ddl, "(%d)", info->size );
    }
	if( info->is_primary_key ) {
		g_string_append( ddl, " PRIMARY KEY" );
	}
	if( !info->null_allowed ) {
		g_string_append( ddl, " NOT NULL" );
	}
}

static void
conn_create_table( GncSqlConnection* conn, const gchar* table_name,
				const GList* col_info_list )
{
	GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
	GString* ddl;
	const GList* list_node;
	guint col_num;
	dbi_result result;

	g_return_if_fail( conn != NULL );
	g_return_if_fail( table_name != NULL );
	g_return_if_fail( col_info_list != NULL );
    
	ddl = g_string_new( "" );
	g_string_printf( ddl, "CREATE TABLE %s (", table_name );
    for( list_node = col_info_list, col_num = 0; list_node != NULL;
				list_node = list_node->next, col_num++ ) {
		GncSqlColumnInfo* info = (GncSqlColumnInfo*)(list_node->data);

		if( col_num != 0 ) {
			g_string_append( ddl, ", " );
		}
		add_table_column( ddl, info );
    }
	g_string_append( ddl, ")" );
        
	result = dbi_conn_query( dbi_conn->conn, ddl->str );
	dbi_result_free( result );
	g_string_free( ddl, TRUE );
}

static void
conn_create_index( GncSqlConnection* conn, const gchar* index_name,
					const gchar* table_name, const col_cvt_t* col_table )
{
#if 0
    GdaServerOperation *op;
    GdaServerProvider *server;
	GdaConnection* cnn;
	GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
	GError* error = NULL;
    
    g_return_if_fail( conn != NULL );
	g_return_if_fail( index_name != NULL );
	g_return_if_fail( table_name != NULL );
	g_return_if_fail( col_table != NULL );
    
	cnn = gda_conn->conn;
	g_return_if_fail( cnn != NULL );
    g_return_if_fail( GDA_IS_CONNECTION(cnn) );
    g_return_if_fail( gda_connection_is_opened(cnn) );

    server = gda_conn->server;
	g_return_if_fail( server != NULL );
    
    op = gda_server_provider_create_operation( server, cnn, 
                           GDA_SERVER_OPERATION_CREATE_INDEX, NULL, &error );
    if( error != NULL ) {
		PERR( "gda_server_provider_create_operation(): %s\n", error->message );
	}
    if( op != NULL && GDA_IS_SERVER_OPERATION(op) ) {
        gint col;
		gboolean ok;
        
		ok = gda_server_operation_set_value_at( op, index_name, &error,
								"/INDEX_DEF_P/INDEX_NAME" );
    	if( error != NULL ) {
			PERR( "set INDEX_NAME: %s\n", error->message );
		}
		if( !ok ) return;
		ok = gda_server_operation_set_value_at( op, "", &error,
								"/INDEX_DEF_P/INDEX_TYPE" );
    	if( error != NULL ) {
			PERR( "set INDEX_TYPE: %s\n", error->message );
		}
		if( !ok ) return;
		ok = gda_server_operation_set_value_at( op, "TRUE", &error,
								"/INDEX_DEF_P/INDEX_IFNOTEXISTS" );
    	if( error != NULL ) {
			PERR( "set INDEX_IFNOTEXISTS: %s\n", error->message );
		}
		if( !ok ) return;
		ok = gda_server_operation_set_value_at( op, table_name, &error,
								"/INDEX_DEF_P/INDEX_ON_TABLE" );
    	if( error != NULL ) {
			PERR( "set INDEX_ON_TABLE: %s\n", error->message );
		}
		if( !ok ) return;

        for( col = 0; col_table[col].col_name != NULL; col++ ) {
			guint item;

			if( col != 0 ) {
				item = gda_server_operation_add_item_to_sequence( op, "/INDEX_FIELDS_S" );
				g_assert( item == col );
			}
			ok = gda_server_operation_set_value_at( op, col_table->col_name, &error,
													"/INDEX_FIELDS_S/%d/INDEX_FIELD", col );
			if( error != NULL ) {
				PERR( "set INDEX_FIELD %s: %s\n", col_table->col_name, error->message );
			}
			if( !ok ) break;
        }
        
        if( !gda_server_provider_perform_operation( server, cnn, op, &error ) ) {
            g_object_unref( op );
            return;
        }

        g_object_unref( op );
    }
#endif
}

static gchar*
conn_quote_string( const GncSqlConnection* conn, gchar* unquoted_str )
{
	GncDbiSqlConnection* dbi_conn = (GncDbiSqlConnection*)conn;
	gchar* quoted_str;
	gint size;

	size = dbi_conn_quote_string_copy( dbi_conn->conn, unquoted_str,
									&quoted_str );
	if( size != 0 ) {
		return quoted_str;
	} else {
		return NULL;
	}
}

static GncSqlConnection*
create_dbi_connection( dbi_conn conn )
{
	GncDbiSqlConnection* dbi_conn;

	dbi_conn = g_new0( GncDbiSqlConnection, 1 );
	dbi_conn->base.dispose = conn_dispose;
	dbi_conn->base.executeSelectStatement = conn_execute_select_statement;
	dbi_conn->base.executeNonSelectStatement = conn_execute_nonselect_statement;
	dbi_conn->base.createStatementFromSql = conn_create_statement_from_sql;
	dbi_conn->base.doesTableExist = conn_does_table_exist;
	dbi_conn->base.beginTransaction = conn_begin_transaction;
	dbi_conn->base.rollbackTransaction = conn_rollback_transaction;
	dbi_conn->base.commitTransaction = conn_commit_transaction;
	dbi_conn->base.getColumnTypeName = conn_get_column_type_name;
	dbi_conn->base.createTable = conn_create_table;
	dbi_conn->base.createIndex = conn_create_index;
	dbi_conn->base.quoteString = conn_quote_string;
	dbi_conn->conn = conn;

	return (GncSqlConnection*)dbi_conn;
}

/* ========================== END OF FILE ===================== */
