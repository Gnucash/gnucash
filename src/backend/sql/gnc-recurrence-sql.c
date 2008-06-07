/********************************************************************
 * gnc-recurrence-sql.c: load and save data to SQL                  *
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
/** @file gnc-recurrence-sql.c
 *  @brief load and save data to SQL 
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */

#include "config.h"

#include <glib.h>

#include "qof.h"
#include "gnc-engine.h"
#include "Recurrence.h"

#include "gnc-backend-util-sql.h"

#include "gnc-recurrence-sql.h"

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "recurrences"
#define TABLE_VERSION 1

#define BUDGET_MAX_RECURRENCE_PERIOD_TYPE_LEN 2048

typedef struct {
    GncSqlBackend* be;
    const GUID* guid;
	Recurrence* pRecurrence;
} recurrence_info_t;

static gpointer get_obj_guid( gpointer pObject, const QofParam* param );
static void set_obj_guid( gpointer pObject, gpointer pValue );
static gint get_recurrence_mult( gpointer pObject );
static void set_recurrence_mult( gpointer pObject, gint value );
static gpointer get_recurrence_period_type( gpointer pObject, const QofParam* );
static void set_recurrence_period_type( gpointer pObject, gpointer pValue );
static gpointer get_recurrence_period_start( gpointer pObject, const QofParam* );
static void set_recurrence_period_start( gpointer pObject, gpointer pValue );

static const col_cvt_t col_table[] =
{
    { "obj_guid",                CT_GUID,   0,                                     COL_NNUL, NULL, NULL,
            get_obj_guid, set_obj_guid },
    { "recurrence_mult",         CT_INT,    0,                                     COL_NNUL, NULL, NULL,
            (QofAccessFunc)get_recurrence_mult, (QofSetterFunc)set_recurrence_mult },
    { "recurrence_period_type",  CT_STRING, BUDGET_MAX_RECURRENCE_PERIOD_TYPE_LEN, COL_NNUL, NULL, NULL,
			get_recurrence_period_type, set_recurrence_period_type },
    { "recurrence_period_start", CT_GDATE,  0,                                     COL_NNUL, NULL, NULL,
            get_recurrence_period_start, set_recurrence_period_start },
    { NULL }
};

/* Special column table because we need to be able to access the table by
a column other than the primary key */
static const col_cvt_t guid_col_table[] =
{
    { "obj_guid", CT_GUID, 0, 0, NULL, NULL, get_obj_guid, set_obj_guid },
    { NULL }
};

/* ================================================================= */

static gpointer
get_obj_guid( gpointer pObject, const QofParam* param )
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;

	g_return_val_if_fail( pObject != NULL, NULL );

    return (gpointer)pInfo->guid;
}

static void
set_obj_guid( gpointer pObject, gpointer pValue )
{
    // Nowhere to put the GUID
}

static gint
get_recurrence_mult( gpointer pObject )
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;
	
	g_return_val_if_fail( pObject != NULL, 0 );
	g_return_val_if_fail( pInfo->pRecurrence != NULL, 0 );

	return pInfo->pRecurrence->mult;
}

static void
set_recurrence_mult( gpointer pObject, gint value )
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( pInfo->pRecurrence != NULL );

    pInfo->pRecurrence->mult = value;
}

static gpointer
get_recurrence_period_type( gpointer pObject, const QofParam* param )
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( pInfo->pRecurrence != NULL, NULL );

    return (gpointer)recurrencePeriodTypeToString(
                            recurrenceGetPeriodType( pInfo->pRecurrence ) );
}

static void
set_recurrence_period_type( gpointer pObject, gpointer pValue )
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( pInfo->pRecurrence != NULL );
	g_return_if_fail( pValue != NULL );

    pInfo->pRecurrence->ptype = recurrencePeriodTypeFromString( (gchar*)pValue );
}

static gpointer
get_recurrence_period_start( gpointer pObject, const QofParam* param )
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;
    static GDate date;

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( pInfo->pRecurrence != NULL, NULL );

    date = recurrenceGetDate( pInfo->pRecurrence );
    return (gpointer)&date;
}

static void
set_recurrence_period_start( gpointer pObject, gpointer pValue )
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;
    GDate* date = (GDate*)pValue;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( pInfo->pRecurrence != NULL );
	g_return_if_fail( pValue != NULL );

    pInfo->pRecurrence->start = *date;
}

/* ================================================================= */

void
gnc_sql_recurrence_save( GncSqlBackend* be, const GUID* guid, const Recurrence* r )
{
    recurrence_info_t recurrence_info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( guid != NULL );
	g_return_if_fail( r != NULL );

	gnc_sql_recurrence_delete( be, guid );

    recurrence_info.be = be;
    recurrence_info.guid = guid;
	recurrence_info.pRecurrence = (Recurrence*)r;
    (void)gnc_sql_do_db_operation( be, OP_DB_ADD, TABLE_NAME,
                                TABLE_NAME, &recurrence_info, col_table );
}

void
gnc_sql_recurrence_save_list( GncSqlBackend* be, const GUID* guid, GList* schedule )
{
    recurrence_info_t recurrence_info;
	GList* l;

	g_return_if_fail( be != NULL );
	g_return_if_fail( guid != NULL );

	gnc_sql_recurrence_delete( be, guid );

    recurrence_info.be = be;
    recurrence_info.guid = guid;
	for( l = schedule; l != NULL; l = g_list_next( l ) ) {
		recurrence_info.pRecurrence = (Recurrence*)l->data;
    	(void)gnc_sql_do_db_operation( be, OP_DB_ADD, TABLE_NAME,
                                TABLE_NAME, &recurrence_info, col_table );
	}
}

void
gnc_sql_recurrence_delete( GncSqlBackend* be, const GUID* guid )
{
    recurrence_info_t recurrence_info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( guid != NULL );

    recurrence_info.be = be;
    recurrence_info.guid = guid;
    (void)gnc_sql_do_db_operation( be, OP_DB_DELETE, TABLE_NAME,
                                TABLE_NAME, &recurrence_info, guid_col_table );
}

static void
load_recurrence( GncSqlBackend* be, GncSqlRow* row, Recurrence* r )
{
    recurrence_info_t recurrence_info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( r != NULL );

    recurrence_info.be = be;
	recurrence_info.pRecurrence = r;

    gnc_sql_load_object( be, row, TABLE_NAME, &recurrence_info, col_table );
	g_free( row );
}

static GncSqlResult*
gnc_sql_set_recurrences_from_db( GncSqlBackend* be, const GUID* guid )
{
    gchar* buf;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
    gchar* field_name;
    GncSqlStatement* stmt;
    GValue value;
	GError* error = NULL;
	GncSqlResult* result;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( guid != NULL, NULL );

    guid_to_string_buff( guid, guid_buf );
    memset( &value, 0, sizeof( value ) );
    g_value_init( &value, G_TYPE_STRING );
    g_value_set_string( &value, guid_buf );

	buf = g_strdup_printf( "SELECT * FROM %s WHERE obj_guid='%s'", TABLE_NAME, guid_buf );
	stmt = gnc_sql_connection_create_statement_from_sql( be->conn, buf );
	g_free( buf );

    result = gnc_sql_execute_select_statement( be, stmt );
	gnc_sql_statement_dispose( stmt );
	return result;
}

void
gnc_sql_recurrence_load( GncSqlBackend* be, const GUID* guid, Recurrence* pRecurrence )
{
	GncSqlResult* result;

	g_return_if_fail( be != NULL );
	g_return_if_fail( guid != NULL );
	g_return_if_fail( pRecurrence != NULL );

	result = gnc_sql_set_recurrences_from_db( be, guid );
    if( result != NULL ) {
        int numRows = gnc_sql_result_get_num_rows( result );

		if( numRows > 0 ) {
			if( numRows > 1 ) {
				g_warning( "More than 1 recurrence found: first one used" );
			}
			load_recurrence( be, gnc_sql_result_get_first_row( result ),
							pRecurrence );
		} else {
			g_warning( "No recurrences found" );
		}
		gnc_sql_result_dispose( result );
    }
}

void
gnc_sql_recurrence_load_list( GncSqlBackend* be, const GUID* guid, GList** pSchedule )
{
	GncSqlResult* result;

	g_return_if_fail( be != NULL );
	g_return_if_fail( guid != NULL );
	g_return_if_fail( pSchedule != NULL );

	result = gnc_sql_set_recurrences_from_db( be, guid );
    if( result != NULL ) {
        GncSqlRow* row = gnc_sql_result_get_first_row( result );

		while( row != NULL ) {
			Recurrence* pRecurrence = g_new0( Recurrence, 1 );
			load_recurrence( be, row, pRecurrence );
			*pSchedule = g_list_append( *pSchedule, pRecurrence );
			row = gnc_sql_result_get_next_row( result );
		}
		gnc_sql_result_dispose( result );
    }
}

/* ================================================================= */
static void
create_recurrence_tables( GncSqlBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_sql_get_table_version( be, TABLE_NAME );
    if( version == 0 ) {
    	GError* error = NULL;

        gnc_sql_create_table( be, TABLE_NAME, TABLE_VERSION, col_table, &error );
        if( error != NULL ) {
            PERR( "Error creating table: %s\n", error->message );
        }
    }
}

/* ================================================================= */
void
gnc_sql_init_recurrence_handler( void )
{
    static GncSqlDataType_t be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_ACCOUNT,
        NULL,                    /* commit - cannot occur */
        NULL,                    /* initial_load - cannot occur */
        create_recurrence_tables        /* create_tables */
    };

    qof_object_register_backend( TABLE_NAME, GNC_SQL_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
