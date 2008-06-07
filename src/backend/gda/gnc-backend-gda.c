/********************************************************************
 * gnc-backend-gda.c: load and save data to SQL via libgda          *
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
/** @file gnc-backend-gda.c
 *  @brief load and save data to SQL 
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db using libgda
 */

#include "config.h"

#include <errno.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>

#include <libgda/libgda.h>
#include <libgda/gda-easy.h>
#include <sql-parser/gda-sql-parser.h>

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

#include "gnc-backend-gda.h"

#define TRANSACTION_NAME "trans"

static QofLogModule log_module = G_LOG_DOMAIN;

#define SQLITE_PROVIDER_NAME "SQLite"
#define URI_PREFIX "gda://"
static GncSqlConnection* create_gda_connection( GdaConnection* conn );

/* ================================================================= */

static void
create_tables_cb( const gchar* type, gpointer data_p, gpointer be_p )
{
    GncSqlDataType_t* pData = data_p;
    GncGdaBackend* be = be_p;

    g_return_if_fail( type != NULL && data_p != NULL && be_p != NULL );
    g_return_if_fail( pData->version == GNC_SQL_BACKEND_VERSION );

    if( pData->create_tables != NULL ) {
        (pData->create_tables)( &be->sql_be );
    }
}

/*
 * Parse the gda uri.  If successful, return TRUE.
 */

static gboolean parse_uri( const gchar* book_id,
					gchar** pProvider, gchar** pDsn, gchar** pUsername, gchar** pPassword )
{
	gchar* uri_id;
	gchar* book_info;
	const gchar* provider;
	GdaDataModel* providers;
	gint numProviders;
	gboolean provider_found;
	gchar* dsn;
	gint i;

	*pProvider = NULL;
	*pDsn = NULL;
	*pUsername = NULL;
	*pPassword = NULL;

	book_info = g_strdup( book_id );
	uri_id = book_info;

	/* If there is no gda:// prefix, we just have a sqlite file name.
	 * Otherwise, the string will be one of:
	 *
	 *    sqlite:<filename>
	 *    mysql:<dbname>
	 *    pgsql:<dbname>
	 *    @<gda_connectionname>
	 */
	if( g_str_has_prefix( uri_id, URI_PREFIX ) ) {
		uri_id += strlen( URI_PREFIX );

		if( uri_id[0] == '@' ) {
			*pDsn = g_strdup( &uri_id[1] );
			g_free( book_info );
			return TRUE;
		}

		provider = uri_id;
	    dsn = strchr( uri_id, ':' );
		if( dsn == 0 ) {
			g_free( book_info );
			return FALSE;
		}
		*dsn = '\0';
		dsn++;

	} else {
		provider = SQLITE_PROVIDER_NAME;
		dsn = uri_id;
	}

	// Get a list of all of the providers.  If the requested provider is on the list, use it.
	// Note that we need a case insensitive comparison here
	providers = gda_config_list_providers();
	numProviders = gda_data_model_get_n_rows( providers );

	provider_found = FALSE;
	for( i = 0; i < numProviders; i++ ) {
		const GValue* providerValue = gda_data_model_get_value_at( providers, 0, i );
		const gchar* s = g_value_get_string( providerValue );

		if( g_ascii_strcasecmp( s, provider ) == 0 ) {
			provider_found = TRUE;
			provider = s;
			break;
		}
	}
	if( provider_found ) {
		gchar* cnc;

		*pProvider = g_strdup( provider );

		// If the provider is SQLite, split the file name into DB_DIR and
		// DB_NAME
		if( strcmp( provider, SQLITE_PROVIDER_NAME ) == 0 ) {
			gchar* dirname;
			gchar* basename;

			dirname = g_path_get_dirname( dsn );

			basename = g_path_get_basename( dsn );
				
			// Remove .db from the base name if it exists
			if( g_str_has_suffix( basename, ".db" ) ) {
				gchar* bn = g_strdup( basename );
				gchar* suffix = g_strrstr( bn, ".db" );
				*suffix = '\0';

				cnc = g_strdup_printf( "DB_DIR=%s;DB_NAME=%s", dirname, bn );
				g_free( bn );
			} else {
				cnc = g_strdup_printf( "DB_DIR=%s;DB_NAME=%s",
											dirname, basename );
			}
			g_free( dirname );
			g_free( basename );
		} else {
			cnc = g_strdup( dsn );
		}
		*pDsn = cnc;
		g_free( book_info );
		return TRUE;
	} else {
		g_free( book_info );
		return FALSE;
	}
}

static void
gnc_gda_session_begin( QofBackend *be_start, QofSession *session, 
	                   const gchar *book_id,
                       gboolean ignore_lock,
				       gboolean create_if_nonexistent )
{
    GncGdaBackend *be = (GncGdaBackend*)be_start;
    GError* error = NULL;
    gchar* dsn;
    gchar* username;
    gchar* password;
	gchar* provider;
	gboolean uriOK;

	g_return_if_fail( be_start != NULL );
	g_return_if_fail( session != NULL );
	g_return_if_fail( book_id != NULL );

    ENTER (" ");

	be->pConnection = NULL;

	// FIXME: better username/password handling

    /* Split book_id into provider and connection string.  If there's no
	provider, use "file" */
	uriOK = parse_uri( book_id, &provider, &dsn, &username, &password );
	if( !uriOK ) {
        qof_backend_set_error( be_start, ERR_BACKEND_BAD_URL );

        LEAVE( " " );
        return;
	}

	if( provider == NULL ) {
		be->pConnection = gda_connection_open_from_dsn( dsn, "", 0, &error );
	} else {
		be->pConnection = gda_connection_open_from_string(
											provider,
											dsn,
											"",
											0,
											&error );

		if( be->pConnection == NULL ) {
			GdaServerOperation* op = gda_prepare_create_database(
													provider,
													dsn,
													&error );
			if( op != NULL ) {
				gboolean isOK;
				isOK = gda_perform_create_database( op, &error );
				if( isOK ) {
					be->pConnection = gda_connection_open_from_string(
											provider,
											dsn,
											"",
											0,
											&error );
				}
			}
		}
	}

	if( provider != NULL ) g_free( provider );
	if( dsn != NULL ) g_free( dsn );
	if( username != NULL ) g_free( username );
	if( password != NULL ) g_free( password );

    if( be->pConnection == NULL ) {
        PERR( "SQL error: %s\n", error->message );
        qof_backend_set_error( be_start, ERR_BACKEND_NO_SUCH_DB );

        LEAVE( " " );
        return;
    }

	be->supports_transactions = gda_connection_supports_feature( be->pConnection, GDA_CONNECTION_FEATURE_TRANSACTIONS );

    // Set up the dictionary
	gda_connection_update_meta_store( be->pConnection, NULL, &error );

	be->parser = gda_sql_parser_new();

	be->sql_be.conn = create_gda_connection( be->pConnection );

    LEAVE (" ");
}

/* ================================================================= */

static void
gnc_gda_session_end( QofBackend *be_start )
{
    GncGdaBackend *be = (GncGdaBackend*)be_start;

	g_return_if_fail( be_start != NULL );

    ENTER (" ");

	if( be->parser != NULL ) {
		g_object_unref( be->parser );
		be->parser = NULL;
	}
    if( be->pConnection != NULL ) {
		if( gda_connection_is_opened( be->pConnection ) ) {
        	gda_connection_close( be->pConnection );
		}
		g_object_unref( be->pConnection );
    	be->pConnection = NULL;
    }
	gnc_sql_finalize_version_info( &be->sql_be );

    LEAVE (" ");
}

static void
gnc_gda_destroy_backend( QofBackend *be )
{
	g_return_if_fail( be != NULL );

    g_free( be );
}

/* ================================================================= */

static void
gnc_gda_load( QofBackend* qbe, QofBook *book )
{
    GncGdaBackend *be = (GncGdaBackend*)qbe;
    GncSqlDataType_t* pData;
	int i;
	Account* root;
	GError* error = NULL;

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
gnc_gda_save_may_clobber_data( QofBackend* qbe )
{
    GncGdaBackend* be = (GncGdaBackend*)qbe;
    GdaDataModel* tables;
    GError* error = NULL;
	gint numTables;
	GdaMetaStore* mstore;

	/* Data may be clobbered iff the number of tables != 0 */
	mstore = gda_connection_get_meta_store( be->pConnection );
	tables = gda_connection_get_meta_store_data( be->pConnection, GDA_CONNECTION_META_TABLES, &error, 0 );
#if 0
    tables = gda_connection_get_schema( be->pConnection,
                                        GDA_CONNECTION_SCHEMA_TABLES,
                                        NULL,
                                        &error );
#endif
    if( error != NULL ) {
        PERR( "SQL error: %s\n", error->message );
    }
    numTables = gda_data_model_get_n_rows( tables );

	return (numTables != 0);
}

static void
gnc_gda_sync_all( QofBackend* fbe, QofBook *book )
{
    GncGdaBackend* be = (GncGdaBackend*)fbe;
    GdaDataModel* tables;
    GError* error = NULL;
    gint row;
    gint numTables;
	gboolean status;
	GdaMetaStore* mstore;

	g_return_if_fail( be != NULL );
	g_return_if_fail( book != NULL );

    ENTER( "book=%p, primary=%p", book, be->primary_book );

    /* Destroy the current contents of the database */
	mstore = gda_connection_get_meta_store( be->pConnection );
	tables = gda_connection_get_meta_store_data( be->pConnection, GDA_CONNECTION_META_TABLES, &error, 0 );
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

	gnc_sql_reset_version_info( &be->sql_be );

    /* Create new tables */
	be->is_pristine_db = TRUE;
    qof_object_foreach_backend( GNC_SQL_BACKEND, create_tables_cb, be );

    /* Save all contents */
	be->primary_book = book;
	gnc_sql_sync_all( &be->sql_be, book );

    LEAVE( "book=%p", book );
}

/* ================================================================= */

static QofBackend*
gnc_gda_backend_new(void)
{
    GncGdaBackend *gnc_be;
    QofBackend *be;
    static gboolean initialized = FALSE;

    gnc_be = g_new0(GncGdaBackend, 1);
    be = (QofBackend*) gnc_be;
    qof_backend_init(be);

    be->session_begin = gnc_gda_session_begin;
    be->session_end = gnc_gda_session_end;
    be->destroy_backend = gnc_gda_destroy_backend;

    be->load = gnc_gda_load;
    be->save_may_clobber_data = gnc_gda_save_may_clobber_data;

    /* The gda backend treats accounting periods transactionally. */
    be->begin = gnc_sql_begin_edit;
    be->commit = gnc_sql_commit_edit;
    be->rollback = gnc_sql_rollback_edit;

    /* The gda backend uses queries to load data ... */
#if 0
    be->compile_query = gnc_gda_compile_query;
    be->free_query = gnc_gda_free_query;
    be->run_query = gnc_gda_run_query;
#endif

    be->counter = NULL;

    /* The gda backend will not be multi-user (for now)... */
    be->events_pending = NULL;
    be->process_events = NULL;

    be->sync = gnc_gda_sync_all;
    be->load_config = NULL;
    be->get_config = NULL;

    be->export = NULL;

    gnc_be->primary_book = NULL;

    if( !initialized ) {
        gda_init();
		gnc_sql_init( &gnc_be->sql_be );
        initialized = TRUE;
    }

    return be;
}

static void
gnc_gda_provider_free( QofBackendProvider *prov )
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
gnc_gda_check_sqlite_file( const gchar *path )
{
	FILE* f;
	gchar buf[50];

	// BAD if the path is null
	g_return_val_if_fail( path != NULL, FALSE );

	if( g_str_has_suffix( path, ".db" ) ) {
		f = g_fopen( path, "r" );

		// OK if the file doesn't exist - new file
		if( f == NULL ) {
			PINFO( "Has '.db', doesn't exist (errno=%d) -> GDA", errno );
			return TRUE;
		}

		// OK if file has the correct header
		fread( buf, sizeof(buf), 1, f );
		fclose( f );
		if( g_str_has_prefix( buf, "SQLite format" ) ) {
			PINFO( "Has '.db', exists, has SQLite format string -> GDA" );
			return TRUE;
		}
		PINFO( "Has '.db', exists, does not have SQLite format string -> not GDA" );
	} else {
		f = g_fopen( path, "r" );

		// BAD if the file exists - not ours
		if( f != NULL ) {
			fclose( f );
			PINFO( "No '.db', exists -> not GDA" );
			return FALSE;
		}

		// OK - new file
		PINFO( "No '.db', doesn't exist (errno=%d) -> GDA", errno );
		return TRUE;
	}

	// Otherwise, BAD
	return FALSE;
}

G_MODULE_EXPORT void
qof_backend_module_init(void)
{
    QofBackendProvider *prov;

    prov = g_new0 (QofBackendProvider, 1);
    prov->provider_name = "GnuCash LibGDA Backend";
    prov->access_method = "gda";
    prov->partial_book_supported = FALSE;
    prov->backend_new = gnc_gda_backend_new;
    prov->provider_free = gnc_gda_provider_free;
    prov->check_data_type = NULL;
    qof_backend_register_provider (prov);

    prov = g_new0 (QofBackendProvider, 1);
    prov->provider_name = "GnuCash LibGDA Backend";
    prov->access_method = "file";
    prov->partial_book_supported = FALSE;
    prov->backend_new = gnc_gda_backend_new;
    prov->provider_free = gnc_gda_provider_free;
    prov->check_data_type = gnc_gda_check_sqlite_file;
    qof_backend_register_provider (prov);
}

/* --------------------------------------------------------- */
typedef struct
{
	GncSqlRow base;

	GdaDataModel* model;
	int row_num;
} GncGdaSqlRow;

static void
row_dispose( GncSqlRow* row )
{
	GncGdaSqlRow* gda_row = (GncGdaSqlRow*)row;

	g_object_unref( gda_row->model );
	gda_row->model = NULL;
	g_free( gda_row );
}

static const GValue*
row_get_value_at_col_name( GncSqlRow* row, const gchar* col_name )
{
	GncGdaSqlRow* gda_row = (GncGdaSqlRow*)row;

	return gda_data_model_get_value_at_col_name( gda_row->model, col_name, gda_row->row_num );
}

static GncSqlRow*
create_gda_row( GdaDataModel* model, int rowNum )
{
	GncGdaSqlRow* row;

	row = g_new0( GncGdaSqlRow, 1 );
	row->base.getValueAtColName = row_get_value_at_col_name;
	row->base.dispose = row_dispose;
	row->model = model;
	g_object_ref( model );
	row->row_num = rowNum;

	return (GncSqlRow*)row;
}
/* --------------------------------------------------------- */
typedef struct
{
	GncSqlResult base;

	GdaDataModel* model;
	gint next_row;
	gint num_rows;
} GncGdaSqlResult;

static void
result_dispose( GncSqlResult* result )
{
	GncGdaSqlResult* gda_result = (GncGdaSqlResult*)result;

	g_object_unref( gda_result->model );
	gda_result->model = NULL;
	g_free( result );
}

static gint
result_get_num_rows( GncSqlResult* result )
{
	GncGdaSqlResult* gda_result = (GncGdaSqlResult*)result;

	return gda_result->num_rows;
}

static GncSqlRow*
result_get_first_row( GncSqlResult* result )
{
	GncGdaSqlResult* gda_result = (GncGdaSqlResult*)result;

	if( gda_result->num_rows > 0 ) {
		gda_result->next_row = 1;
		return create_gda_row( gda_result->model, 0 );
	} else {
		return NULL;
	}
}

static GncSqlRow*
result_get_next_row( GncSqlResult* result )
{
	GncGdaSqlResult* gda_result = (GncGdaSqlResult*)result;

	if( gda_result->next_row < gda_result->num_rows ) {
		return create_gda_row( gda_result->model, gda_result->next_row++ );
	} else {
		return NULL;
	}
}

static GncSqlResult*
create_gda_result( GdaDataModel* model )
{
	GncGdaSqlResult* result;

	result = g_new0( GncGdaSqlResult, 1 );
	result->base.dispose = result_dispose;
	result->base.getNumRows = result_get_num_rows;
	result->base.getFirstRow = result_get_first_row;
	result->base.getNextRow = result_get_next_row;
	result->model = model;
	result->num_rows = gda_data_model_get_n_rows( model );
	result->next_row = 0;

	return (GncSqlResult*)result;
}
/* --------------------------------------------------------- */
typedef struct
{
	GncSqlStatement base;

	GString* sql;
} GncGdaSqlStatement;

static void
stmt_dispose( GncSqlStatement* stmt )
{
	GncGdaSqlStatement* gda_stmt = (GncGdaSqlStatement*)stmt;

	if( gda_stmt->sql != NULL ) {
		g_string_free( gda_stmt->sql, TRUE );
	}
	g_free( stmt );
}

static gchar*
stmt_to_sql( GncSqlStatement* stmt )
{
	GncGdaSqlStatement* gda_stmt = (GncGdaSqlStatement*)stmt;

	return gda_stmt->sql->str;
}

static void
stmt_add_where_cond( GncSqlStatement* stmt, QofIdTypeConst type_name,
					gpointer obj, const col_cvt_t* table_row, GValue* value )
{
	GncGdaSqlStatement* gda_stmt = (GncGdaSqlStatement*)stmt;
	gchar* buf;

	buf = g_strdup_printf( " WHERE %s = %s", table_row->col_name,
						gnc_sql_get_sql_value( value ) );
	g_string_append( gda_stmt->sql, buf );
	g_free( buf );
}

static GncSqlStatement*
create_gda_statement( const gchar* sql )
{
	GncGdaSqlStatement* stmt;

	stmt = g_new0( GncGdaSqlStatement, 1 );
	stmt->base.dispose = stmt_dispose;
	stmt->base.toSql = stmt_to_sql;
	stmt->base.addWhereCond = stmt_add_where_cond;
	stmt->sql = g_string_new( sql );

	return (GncSqlStatement*)stmt;
}
/* --------------------------------------------------------- */
typedef struct
{
	GncSqlConnection base;

	GdaConnection* conn;
	GdaSqlParser* parser;
	GdaServerProvider* server;
} GncGdaSqlConnection;

static void
conn_dispose( GncSqlConnection* conn )
{
	GncGdaSqlConnection* gda_conn = (GncGdaSqlConnection*)conn;

	gda_connection_close( gda_conn->conn );
	g_object_unref( gda_conn->conn );
	g_free( conn );
}

static GncSqlResult*
conn_execute_select_statement( GncSqlConnection* conn, GncSqlStatement* stmt )
{
	GncGdaSqlConnection* gda_conn = (GncGdaSqlConnection*)conn;
	GncGdaSqlStatement* gda_stmt = (GncGdaSqlStatement*)stmt;
	GdaDataModel* model;
	GdaStatement* real_stmt;
	GError* error = NULL;
														
	real_stmt = gda_sql_parser_parse_string( gda_conn->parser, gda_stmt->sql->str, NULL, &error );
	if( error != NULL ) {
		PERR( "Error parsing SQL %s\n%s\n", gda_stmt->sql->str, error->message );
		return NULL;
	}
	model = gda_connection_statement_execute_select( gda_conn->conn,
												real_stmt, NULL, &error );
	if( error != NULL ) {
		PERR( "Error executing SQL %s\n%s\n", gda_stmt->sql->str, error->message );
		return NULL;
	}
	return create_gda_result( model );
}

static gint
conn_execute_nonselect_statement( GncSqlConnection* conn, GncSqlStatement* stmt )
{
	GncGdaSqlConnection* gda_conn = (GncGdaSqlConnection*)conn;
	GncGdaSqlStatement* gda_stmt = (GncGdaSqlStatement*)stmt;
	GdaStatement* real_stmt;
	gint result;
	GError* error = NULL;

	real_stmt = gda_sql_parser_parse_string( gda_conn->parser, gda_stmt->sql->str, NULL, &error );
	if( error != NULL ) {
		PERR( "Error parsing SQL %s\n%s\n", gda_stmt->sql->str, error->message );
		return 0;
	}
	result = gda_connection_statement_execute_non_select( gda_conn->conn,
												real_stmt, NULL, NULL, &error );
	if( error != NULL ) {
		PERR( "Error executing SQL %s\n%s\n", gda_stmt->sql->str, error->message );
		return 0;
	}
	return result;
}

static GncSqlStatement*
conn_create_statement_from_sql( GncSqlConnection* conn, gchar* sql )
{
	GncGdaSqlConnection* gda_conn = (GncGdaSqlConnection*)conn;

	return create_gda_statement( sql );
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
	GncGdaSqlConnection* gda_conn = (GncGdaSqlConnection*)conn;
	GError* error = NULL;
	GValue* table_name_value;
	GdaDataModel* model;
	gint nTables;
	GdaMetaStore* mstore;

	g_return_val_if_fail( conn != NULL, FALSE );
	g_return_val_if_fail( table_name != NULL, FALSE );

#if 0
	/* If the db is pristine because it's being saved, the table does not
	 * exist.
	 */
	if( conn->is_pristine_db ) {
		return FALSE;
	}
#endif

	table_name_value = create_gvalue_from_string( g_strdup( table_name ) );
	mstore = gda_connection_get_meta_store( gda_conn->conn );
	model = gda_connection_get_meta_store_data( gda_conn->conn, GDA_CONNECTION_META_TABLES, &error, 1, "name", table_name_value );
	g_free( table_name_value );
	nTables = gda_data_model_get_n_rows( model );
	g_object_unref( model );

	if( nTables == 1 ) {
		return TRUE;
	} else {
		return FALSE;
	}
}

static void
conn_begin_transaction( GncSqlConnection* conn )
{
	GncGdaSqlConnection* gda_conn = (GncGdaSqlConnection*)conn;
}

static void
conn_rollback_transaction( GncSqlConnection* conn )
{
	GncGdaSqlConnection* gda_conn = (GncGdaSqlConnection*)conn;
}

static void
conn_commit_transaction( GncSqlConnection* conn )
{
	GncGdaSqlConnection* gda_conn = (GncGdaSqlConnection*)conn;
}

static const gchar*
conn_get_column_type_name( GncSqlConnection* conn, GType type, gint size )
{
	GncGdaSqlConnection* gda_conn = (GncGdaSqlConnection*)conn;

	return gda_server_provider_get_default_dbms_type(
									gda_conn->server, gda_conn->conn, type );
}

static void
add_table_column( GdaServerOperation* op, const GncSqlColumnInfo* info,
					guint col_num )
{
    gchar* buf;
	GError* error = NULL;
	gboolean ok;

	g_return_if_fail( op != NULL );
	g_return_if_fail( info != NULL );

	ok = gda_server_operation_set_value_at( op, info->name, &error,
									"/FIELDS_A/@COLUMN_NAME/%d", col_num );
	if( error != NULL ) {
		PWARN( "Error setting NAME for %s: %s\n", info->name, error->message );
	}
	if( !ok ) return;
	ok = gda_server_operation_set_value_at( op, info->type_name, &error,
									"/FIELDS_A/@COLUMN_TYPE/%d", col_num );
	if( error != NULL ) {
		PWARN( "Error setting TYPE for %s: %s\n", info->name, error->message );
	}
	if( !ok ) return;
    if( info->size != 0 ) {
        buf = g_strdup_printf( "%d", info->size );
		ok = gda_server_operation_set_value_at( op, buf, &error,
									"/FIELDS_A/@COLUMN_SIZE/%d", col_num );
		if( error != NULL ) {
			PWARN( "Error setting SIZE for %s: %s\n", info->name, error->message );
		}
        g_free( buf );
		if( !ok ) return;
    }
	ok = gda_server_operation_set_value_at( op,
									(info->is_primary_key) ? "TRUE" : "FALSE",
									&error,
									"/FIELDS_A/@COLUMN_PKEY/%d", col_num );
	if( error != NULL ) {
		PWARN( "Error setting PKEY for %s: %s\n", info->name, error->message );
	}
	if( !ok ) return;
	ok = gda_server_operation_set_value_at( op,
									(info->null_allowed) ? "FALSE" : "TRUE",
									&error,
									"/FIELDS_A/@COLUMN_NNUL/%d", col_num );
	if( error != NULL ) {
		PWARN( "Error setting NNUL for %s: %s\n", info->name, error->message );
	}
	if( !ok ) return;
#if 0
	ok = gda_server_operation_set_value_at( op,
									(flags & COL_AUTOINC) ? "TRUE" : "FALSE",
									&error,
									"/FIELDS_A/@COLUMN_AUTOINC/%d", col_num );
	if( error != NULL ) {
		PWARN( "Error setting AUTOINC for %s: %s\n", arg, error->message );
	}
	if( !ok ) return;
	ok = gda_server_operation_set_value_at( op,
									(flags & COL_UNIQUE) ? "TRUE" : "FALSE",
									&error,
									"/FIELDS_A/@COLUMN_UNIQUE/%d", col_num );
	if( error != NULL ) {
		PWARN( "Error setting UNIQUE for %s: %s\n", arg, error->message );
	}
#endif
}

static void
conn_create_table( GncSqlConnection* conn, const gchar* table_name,
				const GList* col_info_list )
{
	GncGdaSqlConnection* gda_conn = (GncGdaSqlConnection*)conn;
    GdaServerOperation *op;
	GdaConnection* cnn;
	GError* error = NULL;
	const GList* list_node;
	guint col_num;

	g_return_if_fail( conn != NULL );
	g_return_if_fail( table_name != NULL );
	g_return_if_fail( col_info_list != NULL );
    
	cnn = gda_conn->conn;
    
    op = gda_server_provider_create_operation( gda_conn->server, cnn, 
                           GDA_SERVER_OPERATION_CREATE_TABLE, NULL, &error );
    if( op != NULL && GDA_IS_SERVER_OPERATION(op) ) {
        gint col;
		gboolean ok;
		GdaServerOperationNode* node;
		gint i;
		GdaDataModel* model;
		GError* error = NULL;
		gint numRows;

		ok = gda_server_operation_set_value_at( op, table_name, &error,
								"/TABLE_DEF_P/TABLE_NAME" );
		if( error != NULL ) {
			PWARN( "Setting TABLE_NAME %s\n%s\n", table_name, error->message );
		}
		if( !ok ) return;

		/* Remove any pre-created colums */
		node = gda_server_operation_get_node_info( op, "/FIELDS_A" );
		model = node->model;
		if( model != NULL ) {
			numRows = gda_data_model_get_n_rows( model );
			for( i = 0; i < numRows; i++ ) {
				gda_data_model_remove_row( model, i, &error );
				if( error != NULL ) {
					PWARN( "Removing server op row: %s\n", error->message );
				}
			}
		}
        
        for( list_node = col_info_list, col_num = 0; list_node != NULL;
				list_node = list_node->next, col_num++ ) {
			GncSqlColumnInfo* info = (GncSqlColumnInfo*)(list_node->data);

			add_table_column( op, info, col_num );
        }
        
        if( !gda_server_provider_perform_operation( gda_conn->server, cnn, op, &error ) ) {
			PWARN( "gda_server_provider_perform_operation(): %s\n%s\n",
					table_name, error->message );
            g_object_unref( op );
            return;
        }

        g_object_unref( op );
    }
}

static void
conn_create_index( GncSqlConnection* conn, const gchar* index_name,
					const gchar* table_name, const col_cvt_t* col_table )
{
    GdaServerOperation *op;
    GdaServerProvider *server;
	GdaConnection* cnn;
	GncGdaSqlConnection* gda_conn = (GncGdaSqlConnection*)conn;
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
}

static GncSqlConnection*
create_gda_connection( GdaConnection* conn )
{
	GncGdaSqlConnection* gda_conn;

	gda_conn = g_new0( GncGdaSqlConnection, 1 );
	gda_conn->base.dispose = conn_dispose;
	gda_conn->base.executeSelectStatement = conn_execute_select_statement;
	gda_conn->base.executeNonSelectStatement = conn_execute_nonselect_statement;
	gda_conn->base.createStatementFromSql = conn_create_statement_from_sql;
	gda_conn->base.doesTableExist = conn_does_table_exist;
	gda_conn->base.beginTransaction = conn_begin_transaction;
	gda_conn->base.rollbackTransaction = conn_rollback_transaction;
	gda_conn->base.commitTransaction = conn_commit_transaction;
	gda_conn->base.getColumnTypeName = conn_get_column_type_name;
	gda_conn->base.createTable = conn_create_table;
	gda_conn->base.createIndex = conn_create_index;
	gda_conn->conn = conn;
	gda_conn->parser = gda_sql_parser_new();
	gda_conn->server = gda_connection_get_provider_obj( conn );

	return (GncSqlConnection*)gda_conn;
}

/* ========================== END OF FILE ===================== */
