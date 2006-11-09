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
 *  @author Copyright (c) 2006 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db using libgda
 */

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <libintl.h>
#include <locale.h>
#include <stdio.h>
#include <fcntl.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <dirent.h>
#include <time.h>
#include <libgda/libgda.h>

#include "qof.h"
#include "qofquery-p.h"
#include "qofquerycore-p.h"
#include "TransLog.h"
#include "gnc-engine.h"

#include "gnc-filepath-utils.h"

#include "gnc-backend-gda.h"
#include "gnc-gconf-utils.h"

#include "gnc-account-gda.h"
#include "gnc-budget-gda.h"
#include "gnc-commodity-gda.h"
#include "gnc-price-gda.h"
#include "gnc-transaction-gda.h"

#ifndef HAVE_STRPTIME
# include "strptime.h"
#endif

static const gchar* convert_search_obj( QofIdType objType );
static void gnc_gda_init_object_handlers( void );

/* callback structure */
typedef struct {
	gboolean ok;
	GncGdaBackend* be;
	QofInstance* inst;
} gda_backend;

static QofLogModule log_module = GNC_MOD_BACKEND;

/* ================================================================= */
void
gnc_gda_load_object( GncGdaBackend* be, GdaDataModel* pModel,
					const col_cvt_t* table, int row )
{
	int col;
	const GValue* val;
	const gchar* s;
	const GdaTimestamp* pTimestamp;
	gchar *buf;
	gint num;
	gint denom;
	GUID* guid;

	for( col = 0; table[col].col_name != NULL; col++ ) {
		if( table[col].col_type != CT_NUMERIC ) {
			val = gda_data_model_get_value_at_col_name( pModel, table[col].col_name, row );
		}
		
		switch( table[col].col_type ) {
			case CT_STRING:
				*(const gchar**)table[col].pData = g_value_get_string( val );
				break;

			case CT_INT:
				*(gint*)table[col].pData = g_value_get_int( val );
				break;

			case CT_GUID:
				s = g_value_get_string( val );
				string_to_guid( s, (GUID*)table[col].pData );
				break;
				
			case CT_DATE:
				s = g_value_get_string( val );
				*(Timespec*)table[col].pData = gnc_iso8601_to_timespec_gmt( s );
				break;

			case CT_NUMERIC:
				buf = g_strdup_printf( "%s_num", table[col].col_name );
				val = gda_data_model_get_value_at_col_name( pModel, buf, row );
				g_free( buf );
				num = g_value_get_int( val );
				buf = g_strdup_printf( "%s_denom", table[col].col_name );
				val = gda_data_model_get_value_at_col_name( pModel, buf, row );
				g_free( buf );
				denom = g_value_get_int( val );
				*(gnc_numeric*)table[col].pData = gnc_numeric_create( num, denom );
				break;

			default:	/* undefined col */
				g_assert( FALSE );
		}
	}
}

/* ================================================================= */
GdaObject*
gnc_gda_execute_query( GncGdaBackend* be, GdaQuery* query )
{
	GError* error = NULL;

	GdaObject* ret;

	ret = gda_query_execute( query, NULL, FALSE, &error );

	if( error != NULL ) {
		printf( "SQL error: %s\n", error->message );
	}

	return ret;
}

GdaObject*
gnc_gda_execute_sql( GncGdaBackend* be, const gchar* sql )
{
	GError* error = NULL;

	GdaQuery* query;

	query = gda_query_new_from_sql( be->pDict, sql, &error );
	if( query == NULL ) {
		printf( "SQL error: %s\n", error->message );
		return NULL;
	}
	return gnc_gda_execute_query( be, query );
}

int
gnc_gda_execute_select_get_count( GncGdaBackend* be, const gchar* sql )
{
	GError* error = NULL;
	int count = 0;

	GdaObject* ret;

	ret = gnc_gda_execute_sql( be, sql );
	if( GDA_IS_DATA_MODEL(ret) ) {
		GdaDataModel* pModel = (GdaDataModel*)ret;
		count = gda_data_model_get_n_rows( pModel );
	}

	if( error != NULL ) {
		printf( "SQL error: %s\n", error->message );
	}

	return count;
}
/* ================================================================= */
static gchar*
render_col_value( GncGdaBackend* be, const col_cvt_t* table_row, gboolean include_name )
{
	gchar* buf;
	gchar guid_buf[GUID_ENCODING_LENGTH+1];
	gchar iso8601_buf[33];
	gchar date_buf[33];
	gnc_numeric v;
	gnc_commodity* pCommodity;
	const GUID* guid;
	const gchar* col_name = "";
	const gchar* equals = "";

	// CT_NUMERIC are special because they are actually stored as 2 cols
	if( include_name && table_row->col_type != CT_NUMERIC ) {
		col_name = table_row->col_name;
		equals = "=";
	}
	switch( table_row->col_type ) {
		case CT_STRING:
			if( *(char**)table_row->pData != NULL ) {
				buf = g_strdup_printf( "%s%s'%s'",
								col_name, equals,
								*(gchar**)table_row->pData );
			} else {
				buf = g_strdup_printf( "%s%sNULL", col_name, equals );
			}
			break;

		case CT_GUID:
			(void)guid_to_string_buff( *(GUID**)table_row->pData, guid_buf );
			buf = g_strdup_printf( "%s%s'%s'", col_name, equals, guid_buf );
			break;

		case CT_DATE:
			(void)gnc_timespec_to_iso8601_buff( *(Timespec*)table_row->pData,
												iso8601_buf );
			strncpy( date_buf, iso8601_buf, 4+1+2+1+2 );
			buf = g_strdup_printf( "%s%s'%s'", col_name, equals, date_buf );
			break;

		case CT_INT:
			buf = g_strdup_printf( "%s%s%d",
									col_name, equals,
									*(gint*)table_row->pData );
			break;

		case CT_NUMERIC:
			v = *(gnc_numeric*)table_row->pData;
			if( !include_name ) {
				buf = g_strdup_printf( "%lld, %lld",
								gnc_numeric_num( v ), gnc_numeric_denom( v ) );
			} else {
				buf = g_strdup_printf( "%s_num=%lld, %s_denom=%lld",
						table_row->col_name, gnc_numeric_num( v ),
						table_row->col_name, gnc_numeric_denom( v ) );
			}
			break;

		default:
			g_assert( FALSE );
	}

	return buf;
}

static gboolean
object_exists_in_db( GncGdaBackend* be, const gchar* table_name, const col_cvt_t* table )
{
	gchar* sql;
	int count;
	gchar* key_value;

	key_value = render_col_value( be, table, TRUE );

	sql = g_strdup_printf( "SELECT * FROM %s WHERE %s;",
							table_name, key_value );
	g_free( key_value );

	count = gnc_gda_execute_select_get_count( be, sql );
	g_free( sql );
	if( count == 0 ) {
		return FALSE;
	} else {
		return TRUE;
	}
}

gboolean
gnc_gda_do_db_operation( GncGdaBackend* be,
						E_DB_OPERATION op,
						const gchar* table_name,
						const col_cvt_t* table )
{
	GdaQuery* pQuery;

	if( op == OP_DB_ADD_OR_UPDATE ) {
		if( object_exists_in_db( be, table_name, table ) ) {
			pQuery = gnc_gda_build_update_query( be, table_name, table );
		} else {
			pQuery = gnc_gda_build_insert_query( be, table_name, table );
		}
	} else if( op == OP_DB_DELETE ) {
		pQuery = gnc_gda_build_delete_query( be, table_name, table );
	} else {
		g_assert( FALSE );
	}
	if( pQuery != NULL ) {
		gnc_gda_execute_query( be, pQuery );
		g_object_unref( G_OBJECT(pQuery) );

		return TRUE;
	} else {
		return FALSE;
	}
}

GdaQuery*
gnc_gda_build_insert_query( GncGdaBackend* be,
							const gchar* table_name,
							const col_cvt_t* table )
{
	char sql[1000];
	GError* error = NULL;
	GdaQuery* query;
	int col;
	gchar* col_value;

	sprintf( sql, "INSERT INTO %s VALUES(", table_name );

	for( col = 0; table[col].col_name != NULL; col++ ) {
		if( col != 0 ) strcat( sql, "," );
		col_value = render_col_value( be, &table[col], FALSE );
		strcat( sql, col_value );
		g_free( col_value );
	}

	strcat( sql, ");" );

	query = gda_query_new_from_sql( be->pDict, sql, &error );
	if( query == NULL ) {
		printf( "SQL error: %s\n", error->message );
	}
	return query;
}

GdaQuery*
gnc_gda_build_update_query( GncGdaBackend* be,
							const gchar* table_name,
							const col_cvt_t* table )
{
	char sql[1000];
	GError* error = NULL;
	GdaQuery* query;
	int col;
	gchar* col_value;

	sprintf( sql, "UPDATE %s SET ", table_name );

	for( col = 1; table[col].col_name != NULL; col++ ) {
		if( col != 1 ) strcat( sql, "," );

		col_value = render_col_value( be, &table[col], TRUE );
		strcat( sql, col_value );
		g_free( col_value );
	}

	strcat( sql, " WHERE " );

	col_value = render_col_value( be, &table[0], TRUE );
	strcat( sql, col_value );
	g_free( col_value );
	strcat( sql, ";" );

	query = gda_query_new_from_sql( be->pDict, sql, &error );
	if( query == NULL ) {
		printf( "SQL error: %s\n", error->message );
	}
	return query;
}

GdaQuery*
gnc_gda_build_delete_query( GncGdaBackend* be,
							const gchar* table_name,
							const col_cvt_t* table )
{
	gchar* sql;
	GError* error = NULL;
	GdaQuery* query;
	gchar* col_value;

	col_value = render_col_value( be, &table[0], TRUE );
	sql = g_strdup_printf( "DELETE FROM %s WHERE %s;", table_name, col_value );
	g_free( col_value );

	query = gda_query_new_from_sql( be->pDict, sql, &error );
	g_free( sql );
	if( query == NULL ) {
		printf( "SQL error: %s\n", error->message );
	}
	return query;
}

/* ================================================================= */

static void
gnc_gda_session_begin(QofBackend *be_start, QofSession *session, 
                   const gchar *book_id,
                   gboolean ignore_lock, gboolean create_if_nonexistent)
{
    GncGdaBackend *be = (GncGdaBackend*) be_start;
	GError* error = NULL;
	gda_backend be_data;
	gchar* book_info;
	gchar* dsn;
	gchar* username;
	gchar* password;

    ENTER (" ");

	be->pClient = gda_client_new();

	/* Split book_id into provider and connection string */
	book_info = g_strdup( book_id );
	dsn = strchr( book_info, ':' );
	*dsn = '\0';
	dsn += 3;
	username = strchr( dsn, ':' );
	if( username != NULL ) {
		*username++ = '\0';
	} else {
		username = "";
	}
	password = strchr( username, ':' );
	if( password != NULL ) {
		*password++ = '\0';
	} else {
		password = "";
	}

	be->pConnection = gda_client_open_connection( be->pClient,
						dsn,
						username, password,
						0,
						&error );
	g_free( book_info );

	if( be->pConnection == NULL ) {
		printf( "SQL error: %s\n", error->message );
		qof_backend_set_error( be_start, ERR_BACKEND_NO_SUCH_DB );
		return;
	}

	be->pDict = gda_dict_new();
	gda_dict_set_connection( be->pDict, be->pConnection );
	gda_dict_update_dbms_meta_data( be->pDict, 0, NULL, &error );
	if( error != NULL ) {
		printf( "SQL error: %s\n", error->message );
	}

    LEAVE (" ");
}

/* ================================================================= */

static void
gnc_gda_session_end(QofBackend *be_start)
{
    GncGdaBackend *be = (GncGdaBackend*)be_start;
    ENTER (" ");

	if( be->pDict != NULL ) {
		g_object_unref( G_OBJECT(be->pDict) );
		be->pDict = NULL;
	}
	if( be->pConnection != NULL && gda_connection_is_opened( be->pConnection ) ) {
		gda_connection_close( be->pConnection );
	}
	be->pConnection = NULL;
	if( be->pClient != NULL ) {
		g_object_unref( G_OBJECT(be->pClient ) );
		be->pClient = NULL;
	}

    LEAVE (" ");
}

static void
gnc_gda_destroy_backend(QofBackend *be)
{
    g_free(be);
}

/* ================================================================= */

static void
initial_load_cb( const gchar* type, gpointer data_p, gpointer be_data_p )
{
	GncGdaDataType_t* pData = data_p;
	gda_backend* be_data = be_data_p;

	g_return_if_fail( type != NULL && pData != NULL && be_data != NULL );
	g_return_if_fail( pData->version == GNC_GDA_BACKEND_VERSION );

	if( pData->initial_load != NULL ) {
		(pData->initial_load)( be_data->be );
	}
}

static void
gnc_gda_load(QofBackend* be_start, QofBook *book)
{
    GncGdaBackend *be = (GncGdaBackend*)be_start;
	gda_backend be_data;

    ENTER (" ");

	g_assert( be->primary_book == NULL );
	be->primary_book = book;

	/* Load any initial stuff */
	be->loading = TRUE;

	be_data.ok = FALSE;
	be_data.be = be;
	be_data.inst = NULL;
	qof_object_foreach_backend( GNC_GDA_BACKEND, initial_load_cb, &be_data );

	be->loading = FALSE;

	LEAVE( "" );
}

/* ================================================================= */

static void
gnc_gda_sync_all(QofBackend* be, QofBook *book)
{
    GncGdaBackend *fbe = (GncGdaBackend *) be;
    ENTER ("book=%p, primary=%p", book, fbe->primary_book);

    LEAVE ("book=%p", book);
}

/* ================================================================= */
/* Routines to deal with the creation of multiple books. */


static void
gnc_gda_begin_edit (QofBackend *be, QofInstance *inst)
{
}

static void
gnc_gda_rollback_edit (QofBackend *be, QofInstance *inst)
{
}

static void
commit_cb( const gchar* type, gpointer data_p, gpointer be_data_p )
{
	GncGdaDataType_t* pData = data_p;
	gda_backend* be_data = be_data_p;

	g_return_if_fail( type != NULL && pData != NULL && be_data != NULL );
	g_return_if_fail( pData->version == GNC_GDA_BACKEND_VERSION );

	/* If this has already been handled, or is not the correct handler, return */
	g_return_if_fail( strcmp( pData->type_name, be_data->inst->entity.e_type ) == 0 );
	g_return_if_fail( !be_data->ok );

	if( pData->commit != NULL ) {
		(pData->commit)( be_data->be, be_data->inst );
		be_data->ok = TRUE;
	}
}

/* Commit_edit handler - find the correct backend handler for this object
 * type and call its commit handler
 */
static void
gnc_gda_commit_edit (QofBackend *be_start, QofInstance *inst)
{
    GncGdaBackend *be = (GncGdaBackend*)be_start;
	gda_backend be_data;

	/* During initial load where objects are being created, don't commit
	anything */

	if( be->loading ) return;

	printf( "gda_commit_edit(): %s dirty = %d, do_free=%d\n", inst->entity.e_type, inst->dirty, inst->do_free );

	if( !inst->dirty ) return;

	be_data.ok = FALSE;
	be_data.be = be;
	be_data.inst = inst;
	qof_object_foreach_backend( GNC_GDA_BACKEND, commit_cb, &be_data );

	if( !be_data.ok ) {
		printf( "gnc_gda_commit_edit(): Unknown object type %s\n",
				inst->entity.e_type );
		return;
	}

	qof_instance_mark_clean( inst );
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
	GSList* pParamPath = qof_query_term_get_param_path( pTerm );
	QofQueryPredData* pPredData = qof_query_term_get_pred_data( pTerm );
	gboolean isInverted = qof_query_term_is_inverted( pTerm );
	GSList* name;
	gchar val[GUID_ENCODING_LENGTH+1];

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

		sprintf( val, "%lld", pData->val );
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

static gpointer
gnc_gda_compile_query(QofBackend* pBEnd, QofQuery* pQuery)
{
    GncGdaBackend *be = (GncGdaBackend*)pBEnd;
	GList* pBookList;
	QofIdType searchObj;
	gchar sql[1000];

	pBookList = qof_query_get_books( pQuery );
	searchObj = qof_query_get_search_for( pQuery );

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

	printf( "Compiled: %s\n", sql );
	return g_strdup( sql );
}

static void
gnc_gda_free_query(QofBackend* pBEnd, gpointer pQuery)
{
    GncGdaBackend *be = (GncGdaBackend*)pBEnd;

	printf( "gda_free_query(): %s\n", (gchar*)pQuery );
}

static void
gnc_gda_run_query(QofBackend* pBEnd, gpointer pQuery)
{
    GncGdaBackend *be = (GncGdaBackend*)pBEnd;
	printf( "gda_run_query(): %s\n", (gchar*)pQuery );
}

/* ================================================================= */
static void
gnc_gda_init_object_handlers( void )
{
	gnc_gda_init_commodity_handler();
	gnc_gda_init_account_handler();
	gnc_gda_init_budget_handler();
	gnc_gda_init_price_handler();
	gnc_gda_init_transaction_handler();
}

/* ================================================================= */

static QofBackend*
gnc_gda_backend_new(void)
{
	GncGdaBackend *gnc_be;
	QofBackend *be;

	gnc_be = g_new0(GncGdaBackend, 1);
	be = (QofBackend*) gnc_be;
	qof_backend_init(be);

	be->session_begin = gnc_gda_session_begin;
	be->session_end = gnc_gda_session_end;
	be->destroy_backend = gnc_gda_destroy_backend;

	be->load = gnc_gda_load;
	be->save_may_clobber_data = NULL;

	/* The gda backend treats accounting periods transactionally. */
	be->begin = gnc_gda_begin_edit;
	be->commit = gnc_gda_commit_edit;
	be->rollback = gnc_gda_rollback_edit;

	/* The gda backend uses queries to load data ... */
	be->compile_query = gnc_gda_compile_query;
	be->free_query = gnc_gda_free_query;
	be->run_query = gnc_gda_run_query;

	be->counter = NULL;

	/* The gda backend will not be multi-user (for now)... */
	be->events_pending = NULL;
	be->process_events = NULL;

	be->sync = gnc_gda_sync_all;
	be->load_config = NULL;
	be->get_config = NULL;

    be->export = NULL;

	gnc_be->primary_book = NULL;

	return be;
}

static void
gnc_gda_provider_free (QofBackendProvider *prov)
{
    prov->provider_name = NULL;
    prov->access_method = NULL;
    g_free (prov);
}

G_MODULE_EXPORT const gchar *
g_module_check_init(GModule *module)
{
	QofBackendProvider *prov;

	prov = g_new0 (QofBackendProvider, 1);
    prov->provider_name = "GnuCash LibGDA Backend (MySQL)";
    prov->access_method = "gda";
    prov->partial_book_supported = FALSE;
    prov->backend_new = gnc_gda_backend_new;
    prov->provider_free = gnc_gda_provider_free;
	prov->check_data_type = NULL;
    qof_backend_register_provider (prov);

	gda_init( "gnucash", "2.0", 0, NULL );

	gnc_gda_init_object_handlers();
    return NULL;
}

/* ========================== END OF FILE ===================== */
