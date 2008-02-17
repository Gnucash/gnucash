/********************************************************************
 * gnc-recurrence-gda.c: load and save data to SQL via libgda       *
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
/** @file gnc-recurrence-gda.c
 *  @brief load and save data to SQL 
 *  @author Copyright (c) 2006, 2007 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db using libgda
 */

#include "config.h"

#include <glib.h>
#include <libgda/libgda.h>

#include "qof.h"
#include "gnc-engine.h"
#include "Recurrence.h"

#include "gnc-backend-util-gda.h"

#include "gnc-recurrence-gda.h"

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "recurrences"

#define BUDGET_MAX_RECURRENCE_PERIOD_TYPE_LEN 50

typedef struct {
    GncGdaBackend* be;
    const GUID* guid;
	Recurrence* pRecurrence;
} recurrence_info_t;

static gpointer get_obj_guid( gpointer pObject, const QofParam* param );
static void set_obj_guid( gpointer pObject, gpointer pValue );
static gpointer get_recurrence_mult( gpointer pObject, const QofParam* );
static void set_recurrence_mult( gpointer pObject, gpointer pValue );
static gpointer get_recurrence_period_type( gpointer pObject, const QofParam* );
static void set_recurrence_period_type( gpointer pObject, gpointer pValue );
static gpointer get_recurrence_period_start( gpointer pObject, const QofParam* );
static void set_recurrence_period_start( gpointer pObject, gpointer pValue );

static col_cvt_t col_table[] =
{
    { "obj_guid",                CT_GUID,   0,                                     COL_NNUL, NULL, NULL,
            get_obj_guid, set_obj_guid },
    { "recurrence_mult",         CT_INT,    0,                                     COL_NNUL, NULL, NULL,
            get_recurrence_mult, set_recurrence_mult },
    { "recurrence_period_type",  CT_STRING, BUDGET_MAX_RECURRENCE_PERIOD_TYPE_LEN, COL_NNUL, NULL, NULL,
			get_recurrence_period_type, set_recurrence_period_type },
    { "recurrence_period_start", CT_GDATE,  0,                                     COL_NNUL, NULL, NULL,
            get_recurrence_period_start, set_recurrence_period_start },
    { NULL }
};

/* Special column table because we need to be able to access the table by
a column other than the primary key */
static col_cvt_t guid_col_table[] =
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

static gpointer
get_recurrence_mult( gpointer pObject, const QofParam* param )
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;
    static guint m;
	
	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( pInfo->pRecurrence != NULL, NULL );

	m = pInfo->pRecurrence->mult;
    return GUINT_TO_POINTER(m);
}

static void
set_recurrence_mult( gpointer pObject, gpointer pValue )
{
    recurrence_info_t* pInfo = (recurrence_info_t*)pObject;
    guint m = GPOINTER_TO_UINT(pValue);

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( pInfo->pRecurrence != NULL );

    pInfo->pRecurrence->mult = m;
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
gnc_gda_recurrence_save( GncGdaBackend* be, const GUID* guid, const Recurrence* r )
{
    recurrence_info_t recurrence_info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( guid != NULL );
	g_return_if_fail( r != NULL );

	gnc_gda_recurrence_delete( be, guid );

    recurrence_info.be = be;
    recurrence_info.guid = guid;
	recurrence_info.pRecurrence = (Recurrence*)r;
    (void)gnc_gda_do_db_operation( be, OP_DB_ADD, TABLE_NAME,
                                TABLE_NAME, &recurrence_info, col_table );
}

void
gnc_gda_recurrence_save_list( GncGdaBackend* be, const GUID* guid, GList* schedule )
{
    recurrence_info_t recurrence_info;
	GList* l;

	g_return_if_fail( be != NULL );
	g_return_if_fail( guid != NULL );

	gnc_gda_recurrence_delete( be, guid );

    recurrence_info.be = be;
    recurrence_info.guid = guid;
	for( l = schedule; l != NULL; l = g_list_next( l ) ) {
		recurrence_info.pRecurrence = (Recurrence*)l->data;
    	(void)gnc_gda_do_db_operation( be, OP_DB_ADD, TABLE_NAME,
                                TABLE_NAME, &recurrence_info, col_table );
	}
}

void
gnc_gda_recurrence_delete( GncGdaBackend* be, const GUID* guid )
{
    recurrence_info_t recurrence_info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( guid != NULL );

    recurrence_info.be = be;
    recurrence_info.guid = guid;
    (void)gnc_gda_do_db_operation( be, OP_DB_DELETE, TABLE_NAME,
                                TABLE_NAME, &recurrence_info, guid_col_table );
}

static void
load_recurrence( GncGdaBackend* be, GdaDataModel* pModel, gint row, Recurrence* r )
{
    recurrence_info_t recurrence_info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );
	g_return_if_fail( r != NULL );

    recurrence_info.be = be;
	recurrence_info.pRecurrence = r;

    gnc_gda_load_object( be, pModel, row, TABLE_NAME, &recurrence_info, col_table );
}

static GdaObject*
gnc_gda_set_recurrences_from_db( GncGdaBackend* be, const GUID* guid )
{
    gchar* buf;
    GdaObject* ret;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
    gchar* field_name;
    static GdaQuery* query = NULL;
    GdaQueryCondition* cond;
    GdaQueryField* key_value;
    GValue value;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( guid != NULL, NULL );

    guid_to_string_buff( guid, guid_buf );

    /* First time, create the query */
    if( query == NULL ) {
        GdaQueryTarget* target;
        GdaQueryField* key;

        /* SELECT */
        query = gnc_gda_create_select_query( be, TABLE_NAME );
        target = gda_query_get_target_by_alias( query, TABLE_NAME );

        /* WHERE */
        cond = gda_query_condition_new( query, GDA_QUERY_CONDITION_LEAF_EQUAL );
        gda_query_set_condition( query, cond );

        field_name = g_strdup_printf( "%s.%s",
                        gda_query_target_get_alias( target ), "obj_guid" );
        key = gda_query_field_field_new( query, field_name );
        g_free( field_name );
        gda_query_field_set_visible( key, TRUE );
        gda_query_condition_leaf_set_operator( cond,
                                                GDA_QUERY_CONDITION_OP_LEFT,
                                                GDA_QUERY_FIELD(key) );
        g_object_unref( G_OBJECT(key) );

        key_value = gda_query_field_value_new( query, G_TYPE_STRING );
        gda_query_field_set_visible( key_value, TRUE );
        gda_query_condition_leaf_set_operator( cond, GDA_QUERY_CONDITION_OP_RIGHT,
                                                GDA_QUERY_FIELD(key_value) );
        g_object_unref( G_OBJECT(key_value) );
    }

    /* Fill in the guid value */
    cond = gda_query_get_condition( query );
    key_value = gda_query_condition_leaf_get_operator( cond, 
                                                GDA_QUERY_CONDITION_OP_RIGHT );
    memset( &value, 0, sizeof( value ) );
    g_value_init( &value, G_TYPE_STRING );
    g_value_set_string( &value, guid_buf );
    gda_query_field_value_set_value( GDA_QUERY_FIELD_VALUE(key_value), &value );

    ret = gnc_gda_execute_query( be, query );

	return ret;
}

void
gnc_gda_recurrence_load( GncGdaBackend* be, const GUID* guid, Recurrence* pRecurrence )
{
	GdaObject* ret;

	g_return_if_fail( be != NULL );
	g_return_if_fail( guid != NULL );
	g_return_if_fail( pRecurrence != NULL );

	ret = gnc_gda_set_recurrences_from_db( be, guid );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );

		if( numRows > 0 ) {
			if( numRows > 1 ) {
				g_warning( "More than 1 recurrence found: first one used" );
			}
			load_recurrence( be, pModel, 0, pRecurrence );
		} else {
			g_warning( "No recurrences found" );
		}
    }
}

void
gnc_gda_recurrence_load_list( GncGdaBackend* be, const GUID* guid, GList** pSchedule )
{
	GdaObject* ret;

	g_return_if_fail( be != NULL );
	g_return_if_fail( guid != NULL );
	g_return_if_fail( pSchedule != NULL );

	ret = gnc_gda_set_recurrences_from_db( be, guid );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;

		for( r = 0; r < numRows; r++ ) {
			Recurrence* pRecurrence = g_new0( Recurrence, 1 );
			load_recurrence( be, pModel, 0, pRecurrence );
			*pSchedule = g_list_append( *pSchedule, pRecurrence );
		}
    }
}

/* ================================================================= */
static void
create_recurrence_tables( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    gnc_gda_create_table_if_needed( be, TABLE_NAME, col_table );
}

/* ================================================================= */
void
gnc_gda_init_recurrence_handler( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_ACCOUNT,
        NULL,                    /* commit - cannot occur */
        NULL,                    /* initial_load - cannot occur */
        create_recurrence_tables        /* create_tables */
    };

    qof_object_register_backend( TABLE_NAME, GNC_GDA_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
