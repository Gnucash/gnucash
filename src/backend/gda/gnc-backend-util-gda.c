/********************************************************************
 * gnc-backend-util-gda.c: load and save data to SQL via libgda     *
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
/** @file gnc-backend-util-gda.c
 *  @brief load and save data to SQL - utility functions
 *  @author Copyright (c) 2006 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file contains utility routines to support saving/restoring
 * data to/from an SQL db using libgda
 */

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <libgda/libgda.h>

#include "qof.h"
#include "qofquery-p.h"
#include "qofquerycore-p.h"
#include "TransLog.h"
#include "gnc-engine.h"

#include "gnc-commodity.h"

#include "gnc-backend-util-gda.h"
#include "gnc-gconf-utils.h"

static void add_table_column( GdaServerProvider* server, GdaConnection* cnn,
            xmlNodePtr array_data, const gchar* arg, const gchar* dbms_type,
            gint size, gint flags );

typedef struct {
    QofIdType searchObj;
    gpointer pCompiledQuery;
} gnc_gda_query_info;

/* callback structure */
typedef struct {
    gboolean ok;
    GncGdaBackend* be;
    QofInstance* inst;
    QofQuery* pQuery;
    gpointer pCompiledQuery;
    gnc_gda_query_info* pQueryInfo;
} gda_backend;

typedef void (*GNC_GDA_LOAD_FN)( GncGdaBackend* be, GdaDataModel* pModel, gint row,
                                QofSetterFunc setter, gpointer pObject,
                                const col_cvt_t* table );
typedef void (*GNC_GDA_CREATE_COL_FN)( GdaServerProvider* server,
                        GdaConnection* cnn, xmlNodePtr array_data,
                        const col_cvt_t* table_row, gboolean pkey );
typedef void (*GNC_GDA_GET_GVALUE_QUERY_FN)( GncGdaBackend* be,
                QofIdTypeConst obj_name, gpointer pObject,
                const col_cvt_t* table_row, GdaQuery* query );
typedef GdaQueryCondition* (*GNC_GDA_GET_GVALUE_COND_FN)( GncGdaBackend* be,
                QofIdTypeConst obj_name, gpointer pObject,
                const col_cvt_t* table_row, GdaQuery* query );

typedef struct {
    GNC_GDA_LOAD_FN             load_fn;
    GNC_GDA_CREATE_COL_FN       create_col_fn;
    GNC_GDA_GET_GVALUE_QUERY_FN get_gvalue_query_fn;
    GNC_GDA_GET_GVALUE_COND_FN  get_gvalue_cond_fn;
} col_type_handler_t;


/* ================================================================= */
static void
add_field_to_query( GdaQuery* query, const gchar* col_name, const GValue* value )
{
    GdaQueryField* field;
    GdaQueryField* field_value;

    field = gda_query_field_field_new( query, col_name );
    gda_query_field_set_visible( field, TRUE );

    field_value = gda_query_field_value_new( query, G_VALUE_TYPE(value) );
    gda_query_field_set_visible( field_value, TRUE );
    gda_query_field_value_set_value( GDA_QUERY_FIELD_VALUE(field_value), value );
    g_object_set( field, "value-provider", field_value, NULL );
    g_object_unref( G_OBJECT(field_value) );

    gda_entity_add_field( GDA_ENTITY(query), GDA_ENTITY_FIELD(field) );
    g_object_unref( G_OBJECT(field) );
}

GdaQueryCondition*
gnc_gda_create_condition_from_field( GdaQuery* query, const gchar* col_name,
                                const GValue* value )
{
    GdaQueryCondition* cond;
    GdaQueryField* key;
    GdaQueryField* key_value;

    cond = gda_query_condition_new( query, GDA_QUERY_CONDITION_LEAF_EQUAL );

    key = gda_query_field_field_new( query, col_name );
    gda_query_field_set_visible( key, TRUE );
    gda_query_condition_leaf_set_operator( cond,
                                            GDA_QUERY_CONDITION_OP_LEFT,
                                            GDA_QUERY_FIELD(key) );
    g_object_unref( G_OBJECT(key) );

    key_value = gda_query_field_value_new( query, G_VALUE_TYPE(value) );
    gda_query_field_set_visible( key_value, TRUE );
    gda_query_condition_leaf_set_operator( cond, GDA_QUERY_CONDITION_OP_RIGHT,
                                                GDA_QUERY_FIELD(key_value) );
    g_object_unref( G_OBJECT(key_value) );

    gda_query_field_value_set_value( GDA_QUERY_FIELD_VALUE(key_value), value );

    return cond;
}
/* ----------------------------------------------------------------- */
static QofAccessFunc
get_getter( QofIdTypeConst obj_name, const col_cvt_t* table_row )
{
    QofAccessFunc getter;

    if( table_row->param_name != NULL ) {
        getter = qof_class_get_parameter_getter( obj_name,
                                                table_row->param_name );
    } else {
        getter = table_row->getter;
    }

    return getter;
}

static void
load_string( GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table )
{
    const GValue* val;
    const gchar* s;

    val = gda_data_model_get_value_at_col_name( pModel, table->col_name, row );
    if( gda_value_is_null( val ) ) {
        s = NULL;
    } else {
        s = g_value_get_string( val );
    }
    if( table->gobj_param_name != NULL ) {
		g_object_set( pObject, table->gobj_param_name, s, NULL );
    } else {
		(*setter)( pObject, (const gpointer)s );
    }
}

static void
get_gvalue_string( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GValue* value )
{
    QofAccessFunc getter;
    gchar* s;

    memset( value, 0, sizeof( GValue ) );
	if( table_row->gobj_param_name != NULL ) {
		g_object_get( pObject, table_row->gobj_param_name, &s, NULL );
	} else {
    	getter = get_getter( obj_name, table_row );
    	s = (gchar*)(*getter)( pObject, NULL );
	}
    if( s ) {
        g_value_init( value, G_TYPE_STRING );
        g_value_set_string( value, s );
    }
}

static void
get_gvalue_string_for_query( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_string( be, obj_name, pObject, table_row, &value );
    add_field_to_query( query, table_row->col_name, &value );
}

static GdaQueryCondition*
get_gvalue_string_cond( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_string( be, obj_name, pObject, table_row, &value );
    return gnc_gda_create_condition_from_field( query, table_row->col_name,
                                            &value );
}

static void
create_string_col( GdaServerProvider* server, GdaConnection* cnn,
            xmlNodePtr array_data, const col_cvt_t* table_row, gboolean pkey )
{
    const gchar* dbms_type;

    dbms_type = gda_server_provider_get_default_dbms_type( server,
                                                        cnn, G_TYPE_STRING );
    add_table_column( server, cnn, array_data, table_row->col_name,
                    dbms_type, table_row->size, table_row->flags | pkey ? COL_PKEY : 0 );
}

static col_type_handler_t string_handler
    = { load_string, create_string_col,
        get_gvalue_string_for_query, get_gvalue_string_cond };
/* ----------------------------------------------------------------- */

static void
load_int( GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table )
{
    const GValue* val;
    gint int_value;

    val = gda_data_model_get_value_at_col_name( pModel, table->col_name, row );
    if( gda_value_is_null( val ) ) {
        int_value = 0;
    } else {
        int_value = g_value_get_int( val );
    }
    if( table->gobj_param_name != NULL ) {
		g_object_set( pObject, table->gobj_param_name, int_value, NULL );
    } else {
    	(*setter)( pObject, GINT_TO_POINTER(int_value) );
    }
}

static void
get_gvalue_int( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GValue* value )
{
    gint int_value;
    QofAccessFunc getter;

    memset( value, 0, sizeof( GValue ) );

    getter = get_getter( obj_name, table_row );
    int_value = GPOINTER_TO_INT((*getter)( pObject, NULL ));
    g_value_init( value, G_TYPE_INT );
    g_value_set_int( value, int_value );
}

static void
get_gvalue_int_for_query( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_int( be, obj_name, pObject, table_row, &value );
    add_field_to_query( query, table_row->col_name, &value );
}

static GdaQueryCondition*
get_gvalue_int_cond( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_int( be, obj_name, pObject, table_row, &value );
    return gnc_gda_create_condition_from_field( query, table_row->col_name, &value );
}

static void
create_int_col( GdaServerProvider* server, GdaConnection* cnn,
            xmlNodePtr array_data, const col_cvt_t* table_row, gboolean pkey )
{
    const gchar* dbms_type;

    dbms_type = gda_server_provider_get_default_dbms_type( server,
                                                        cnn, G_TYPE_INT );
    add_table_column( server, cnn, array_data, table_row->col_name,
                    dbms_type, table_row->size, table_row->flags | pkey ? COL_PKEY : 0 );
}

static col_type_handler_t int_handler =
        { load_int, create_int_col,
            get_gvalue_int_for_query, get_gvalue_int_cond };
/* ----------------------------------------------------------------- */

static void
load_boolean( GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table )
{
    const GValue* val;
    gint int_value;

    val = gda_data_model_get_value_at_col_name( pModel, table->col_name, row );
    if( gda_value_is_null( val ) ) {
        int_value = 0;
    } else {
        int_value = g_value_get_int( val );
    }
    if( table->gobj_param_name != NULL ) {
		g_object_set( pObject, table->gobj_param_name, int_value, NULL );
    } else {
    	(*setter)( pObject, GINT_TO_POINTER(int_value) );
    }
}

static void
get_gvalue_boolean( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GValue* value )
{
    gint int_value;
    QofAccessFunc getter;

    memset( value, 0, sizeof( GValue ) );

    getter = get_getter( obj_name, table_row );
    int_value = GPOINTER_TO_INT((*getter)( pObject, NULL ));
    g_value_init( value, G_TYPE_INT );
    g_value_set_int( value, int_value );
}

static void
get_gvalue_boolean_for_query( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_int( be, obj_name, pObject, table_row, &value );
    add_field_to_query( query, table_row->col_name, &value );
}

static GdaQueryCondition*
get_gvalue_boolean_cond( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_int( be, obj_name, pObject, table_row, &value );
    return gnc_gda_create_condition_from_field( query, table_row->col_name, &value );
}

static void
create_boolean_col( GdaServerProvider* server, GdaConnection* cnn,
            xmlNodePtr array_data, const col_cvt_t* table_row, gboolean pkey )
{
    const gchar* dbms_type;

    dbms_type = gda_server_provider_get_default_dbms_type( server,
                                                        cnn, G_TYPE_INT );
    add_table_column( server, cnn, array_data, table_row->col_name,
                    dbms_type, table_row->size, table_row->flags | pkey ? COL_PKEY : 0 );
}

static col_type_handler_t boolean_handler =
        { load_boolean, create_boolean_col,
            get_gvalue_boolean_for_query, get_gvalue_boolean_cond };
/* ----------------------------------------------------------------- */

static void
load_int64( GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table )
{
    const GValue* val;
    gint64 i64_value;

    val = gda_data_model_get_value_at_col_name( pModel, table->col_name, row );
    if( gda_value_is_null( val ) ) {
        (*setter)( pObject, NULL );
    } else {    
        i64_value = g_value_get_int64( val );
        (*setter)( pObject, (gpointer)&i64_value );
    }
}

static void
get_gvalue_int64( GncGdaBackend* be, QofIdTypeConst obj_name, gpointer pObject,
                const col_cvt_t* table_row, GValue* value )
{
    gint64* pInt64;
    gint64 i64_value;
    QofAccessFunc getter;

    memset( value, 0, sizeof( GValue ) );
    getter = get_getter( obj_name, table_row );
    pInt64 = (*getter)( pObject, NULL );
    if( pInt64 != NULL ) {
        i64_value = *pInt64;
        g_value_init( value, G_TYPE_INT64 );
        g_value_set_int64( value, i64_value );
    }
}

static void
get_gvalue_int64_for_query( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_int64( be, obj_name, pObject, table_row, &value );
    add_field_to_query( query, table_row->col_name, &value );
}

static GdaQueryCondition*
get_gvalue_int64_cond( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_int64( be, obj_name, pObject, table_row, &value );
    return gnc_gda_create_condition_from_field( query, table_row->col_name, &value );
}

static void
create_int64_col( GdaServerProvider* server, GdaConnection* cnn,
            xmlNodePtr array_data, const col_cvt_t* table_row, gboolean pkey )
{
    const gchar* dbms_type;

    dbms_type = gda_server_provider_get_default_dbms_type( server,
                                                        cnn, G_TYPE_INT64 );
    add_table_column( server, cnn, array_data, table_row->col_name,
                    dbms_type, table_row->size, table_row->flags | pkey ? COL_PKEY : 0 );
}

static col_type_handler_t int64_handler =
        { load_int64, create_int64_col,
            get_gvalue_int64_for_query, get_gvalue_int64_cond };
/* ----------------------------------------------------------------- */

static void
load_double( GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table )
{
    const GValue* val;
    gdouble d_value;

    val = gda_data_model_get_value_at_col_name( pModel, table->col_name, row );
    if( gda_value_is_null( val ) ) {
        (*setter)( pObject, (gpointer)NULL );
    } else {
        d_value = g_value_get_double( val );
        (*setter)( pObject, (gpointer)&d_value );
    }
}

static void
get_gvalue_double( GncGdaBackend* be, QofIdTypeConst obj_name, gpointer pObject,
                const col_cvt_t* table_row, GValue* value )
{
    QofAccessFunc getter;
    gdouble* pDouble;
    gdouble d_value;

    memset( value, 0, sizeof( GValue ) );

    getter = get_getter( obj_name, table_row );
    pDouble = (*getter)( pObject, NULL );
    if( pDouble != NULL ) {
        d_value = *pDouble;
        g_value_init( value, G_TYPE_DOUBLE );
        g_value_set_double( value, d_value );
    }
}

static void
get_gvalue_double_for_query( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_double( be, obj_name, pObject, table_row, &value );
    add_field_to_query( query, table_row->col_name, &value );
}

static GdaQueryCondition*
get_gvalue_double_cond( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_double( be, obj_name, pObject, table_row, &value );
    return gnc_gda_create_condition_from_field( query, table_row->col_name, &value );
}

static void
create_double_col( GdaServerProvider* server, GdaConnection* cnn,
            xmlNodePtr array_data, const col_cvt_t* table_row, gboolean pkey )
{
    const gchar* dbms_type;

    dbms_type = gda_server_provider_get_default_dbms_type( server,
                                                        cnn, G_TYPE_INT64 );
    add_table_column( server, cnn, array_data, table_row->col_name,
                    dbms_type, table_row->size, table_row->flags | pkey ? COL_PKEY : 0 );
}

static col_type_handler_t double_handler =
        { load_double, create_double_col,
            get_gvalue_double_for_query, get_gvalue_double_cond };
/* ----------------------------------------------------------------- */

static void
load_guid( GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table )
{
    const GValue* val;
    GUID guid;
    const GUID* pGuid;

    val = gda_data_model_get_value_at_col_name( pModel, table->col_name, row );
    if( gda_value_is_null( val ) ) {
        pGuid = NULL;
    } else {
        string_to_guid( g_value_get_string( val ), &guid );
        pGuid = &guid;
    }
    if( table->gobj_param_name != NULL ) {
		g_object_set( pObject, table->gobj_param_name, pGuid, NULL );
    } else {
		(*setter)( pObject, (const gpointer)pGuid );
    }
}

static void
get_gvalue_guid( GncGdaBackend* be, QofIdTypeConst obj_name, gpointer pObject,
                const col_cvt_t* table_row, GValue* value )
{
    QofAccessFunc getter;
    const GUID* guid;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];

    memset( value, 0, sizeof( GValue ) );

	if( table_row->gobj_param_name != NULL ) {
		g_object_get( pObject, table_row->gobj_param_name, &guid, NULL );
	} else {
    	getter = get_getter( obj_name, table_row );
    	guid = (*getter)( pObject, NULL );
	}
    if( guid != NULL ) {
        (void)guid_to_string_buff( guid, guid_buf );
        g_value_init( value, G_TYPE_STRING );
        g_value_set_string( value, guid_buf );
    }
}

static void
get_gvalue_guid_for_query( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_guid( be, obj_name, pObject, table_row, &value );
    add_field_to_query( query, table_row->col_name, &value );
}

static GdaQueryCondition*
get_gvalue_guid_cond( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_guid( be, obj_name, pObject, table_row, &value );
    return gnc_gda_create_condition_from_field( query, table_row->col_name, &value );
}

static void
create_guid_col( GdaServerProvider* server, GdaConnection* cnn,
            xmlNodePtr array_data, const col_cvt_t* table_row, gboolean pkey )
{
    add_table_column( server, cnn, array_data, table_row->col_name,
                    "char", GUID_ENCODING_LENGTH, table_row->flags | pkey ? COL_PKEY : 0 );
}

static col_type_handler_t guid_handler =
        { load_guid, create_guid_col,
            get_gvalue_guid_for_query, get_gvalue_guid_cond };
/* ----------------------------------------------------------------- */

static void
load_account_guid( GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table )
{
    const GValue* val;
    GUID guid;
    const GUID* pGuid;

    val = gda_data_model_get_value_at_col_name( pModel, table->col_name, row );
    if( gda_value_is_null( val ) ) {
        pGuid = NULL;
    } else {
        string_to_guid( g_value_get_string( val ), &guid );
        pGuid = &guid;
    }
    if( table->gobj_param_name != NULL ) {
		g_object_set( pObject, table->gobj_param_name, pGuid, NULL );
    } else {
		(*setter)( pObject, (const gpointer)pGuid );
    }
}

static void
get_gvalue_account_guid( GncGdaBackend* be, QofIdTypeConst obj_name, gpointer pObject,
                const col_cvt_t* table_row, GValue* value )
{
    QofAccessFunc getter;
    const GUID* guid;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];

    memset( value, 0, sizeof( GValue ) );

	if( table_row->gobj_param_name != NULL ) {
		g_object_get( pObject, table_row->gobj_param_name, &guid, NULL );
	} else {
    	getter = get_getter( obj_name, table_row );
    	guid = (*getter)( pObject, NULL );
	}
    if( guid != NULL ) {
        (void)guid_to_string_buff( guid, guid_buf );
        g_value_init( value, G_TYPE_STRING );
        g_value_set_string( value, guid_buf );
    }
}

static void
get_gvalue_account_guid_for_query( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_guid( be, obj_name, pObject, table_row, &value );
    add_field_to_query( query, table_row->col_name, &value );
}

static GdaQueryCondition*
get_gvalue_account_guid_cond( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_account_guid( be, obj_name, pObject, table_row, &value );
    return gnc_gda_create_condition_from_field( query, table_row->col_name, &value );
}

static void
create_account_guid_col( GdaServerProvider* server, GdaConnection* cnn,
            xmlNodePtr array_data, const col_cvt_t* table_row, gboolean pkey )
{
    add_table_column( server, cnn, array_data, table_row->col_name,
                    "char", GUID_ENCODING_LENGTH, table_row->flags | pkey ? COL_PKEY : 0 );
}

static col_type_handler_t account_guid_handler =
        { load_account_guid, create_account_guid_col,
            get_gvalue_account_guid_for_query, get_gvalue_account_guid_cond };
/* ----------------------------------------------------------------- */

static void
load_commodity_guid( GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table )
{
    const GValue* val;
    GUID guid;
    const GUID* pGuid;
	gnc_commodity* commodity = NULL;

    val = gda_data_model_get_value_at_col_name( pModel, table->col_name, row );
    if( gda_value_is_null( val ) ) {
        pGuid = NULL;
    } else {
        string_to_guid( g_value_get_string( val ), &guid );
        pGuid = &guid;
    }
	if( pGuid != NULL ) {
		commodity = gnc_commodity_find_commodity_by_guid( pGuid, be->primary_book );
	}
    if( table->gobj_param_name != NULL ) {
		g_object_set( pObject, table->gobj_param_name, commodity, NULL );
    } else {
		(*setter)( pObject, (const gpointer)commodity );
    }
}

static void
get_gvalue_commodity_guid( GncGdaBackend* be, QofIdTypeConst obj_name, gpointer pObject,
                const col_cvt_t* table_row, GValue* value )
{
    QofAccessFunc getter;
    const GUID* guid = NULL;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
	gnc_commodity* commodity;

    memset( value, 0, sizeof( GValue ) );

	if( table_row->gobj_param_name != NULL ) {
		g_object_get( pObject, table_row->gobj_param_name, &commodity, NULL );
	} else {
    	getter = get_getter( obj_name, table_row );
    	commodity = (*getter)( pObject, NULL );
	}
	if( commodity != NULL ) {
		guid = qof_instance_get_guid( QOF_INSTANCE(commodity) );
	}
    if( guid != NULL ) {
        (void)guid_to_string_buff( guid, guid_buf );
        g_value_init( value, G_TYPE_STRING );
        g_value_set_string( value, guid_buf );
    }
}

static void
get_gvalue_commodity_guid_for_query( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_commodity_guid( be, obj_name, pObject, table_row, &value );
    add_field_to_query( query, table_row->col_name, &value );
}

static GdaQueryCondition*
get_gvalue_commodity_guid_cond( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_commodity_guid( be, obj_name, pObject, table_row, &value );
    return gnc_gda_create_condition_from_field( query, table_row->col_name, &value );
}

static void
create_commodity_guid_col( GdaServerProvider* server, GdaConnection* cnn,
            xmlNodePtr array_data, const col_cvt_t* table_row, gboolean pkey )
{
    add_table_column( server, cnn, array_data, table_row->col_name,
                    "char", GUID_ENCODING_LENGTH, table_row->flags | pkey ? COL_PKEY : 0 );
}

static col_type_handler_t commodity_guid_handler =
        { load_commodity_guid, create_commodity_guid_col,
            get_gvalue_commodity_guid_for_query, get_gvalue_commodity_guid_cond };
/* ----------------------------------------------------------------- */

static void
load_tx_guid( GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table )
{
    const GValue* val;
    GUID guid;
    const GUID* pGuid;

    val = gda_data_model_get_value_at_col_name( pModel, table->col_name, row );
    if( gda_value_is_null( val ) ) {
        pGuid = NULL;
    } else {
        string_to_guid( g_value_get_string( val ), &guid );
        pGuid = &guid;
    }
    if( table->gobj_param_name != NULL ) {
		g_object_set( pObject, table->gobj_param_name, pGuid, NULL );
    } else {
		(*setter)( pObject, (const gpointer)pGuid );
    }
}

static void
get_gvalue_tx_guid( GncGdaBackend* be, QofIdTypeConst obj_name, gpointer pObject,
                const col_cvt_t* table_row, GValue* value )
{
    QofAccessFunc getter;
    const GUID* guid;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];

    memset( value, 0, sizeof( GValue ) );

	if( table_row->gobj_param_name != NULL ) {
		g_object_get( pObject, table_row->gobj_param_name, &guid, NULL );
	} else {
    	getter = get_getter( obj_name, table_row );
    	guid = (*getter)( pObject, NULL );
	}
    if( guid != NULL ) {
        (void)guid_to_string_buff( guid, guid_buf );
        g_value_init( value, G_TYPE_STRING );
        g_value_set_string( value, guid_buf );
    }
}

static void
get_gvalue_tx_guid_for_query( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_tx_guid( be, obj_name, pObject, table_row, &value );
    add_field_to_query( query, table_row->col_name, &value );
}

static GdaQueryCondition*
get_gvalue_tx_guid_cond( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_tx_guid( be, obj_name, pObject, table_row, &value );
    return gnc_gda_create_condition_from_field( query, table_row->col_name, &value );
}

static void
create_tx_guid_col( GdaServerProvider* server, GdaConnection* cnn,
            xmlNodePtr array_data, const col_cvt_t* table_row, gboolean pkey )
{
    add_table_column( server, cnn, array_data, table_row->col_name,
                    "char", GUID_ENCODING_LENGTH, table_row->flags | pkey ? COL_PKEY : 0 );
}

static col_type_handler_t tx_guid_handler =
        { load_tx_guid, create_tx_guid_col,
            get_gvalue_tx_guid_for_query, get_gvalue_tx_guid_cond };
/* ----------------------------------------------------------------- */
static void
load_timespec( GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table )
{
    const GValue* val;
    GDate* date;
    Timespec ts;

    val = gda_data_model_get_value_at_col_name( pModel, table->col_name, row );
    if( gda_value_is_null( val ) ) {
        (*setter)( pObject, NULL );
    } else {
		if( G_VALUE_HOLDS_STRING( val ) ) {
			const gchar* s = g_value_get_string( val );
			gchar* buf;
			buf = g_strdup_printf( "%c%c%c%c-%c%c-%c%c 00:00:00",
									s[6], s[7], s[8], s[9],
									s[0], s[1],
									s[3], s[4] );
		    ts = gnc_iso8601_to_timespec_gmt( buf );
			(*setter)( pObject, &ts );
			g_free( buf );

		} else if( G_VALUE_HOLDS_BOXED( val ) ) {
        	date = (GDate*)g_value_get_boxed( val );
        	if( date != NULL ) {
            	ts = gnc_dmy2timespec( g_date_get_day( date ),
                                g_date_get_month( date ),
                                g_date_get_year( date ) );
            	(*setter)( pObject, &ts );
			}
		} else {
			g_warning( "Unknown timespec type: %s", G_VALUE_TYPE_NAME( val ) );
        }
    }
}

static void
get_gvalue_timespec( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GValue* value )
{
    QofAccessFunc getter;
    Timespec* pTimespec;

    memset( value, 0, sizeof( GValue ) );

    getter = get_getter( obj_name, table_row );
    pTimespec = (Timespec*)(*getter)( pObject, NULL );
    if( pTimespec != NULL ) {
        GDate* date;
        gint y, m, d;
        gchar iso8601_buf[33];

        date = g_date_new();
        (void)gnc_timespec_to_iso8601_buff( *pTimespec, iso8601_buf );
        sscanf( iso8601_buf, "%d-%d-%d", &y, &m, &d );
        g_date_set_dmy( date, d, m, y );
        g_value_init( value, G_TYPE_DATE );
        g_value_set_boxed( value, date );
    }
}

static void
get_gvalue_timespec_for_query( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_timespec( be, obj_name, pObject, table_row, &value );
    add_field_to_query( query, table_row->col_name, &value );
}

static GdaQueryCondition*
get_gvalue_timespec_cond( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_timespec( be, obj_name, pObject, table_row, &value );
    return gnc_gda_create_condition_from_field( query, table_row->col_name, &value );
}

static void
create_timespec_col( GdaServerProvider* server, GdaConnection* cnn,
            xmlNodePtr array_data, const col_cvt_t* table_row, gboolean pkey )
{
    const gchar* dbms_type;

    dbms_type = gda_server_provider_get_default_dbms_type( server,
                                                        cnn, G_TYPE_DATE );
    add_table_column( server, cnn, array_data, table_row->col_name,
                    dbms_type, table_row->size, table_row->flags | pkey ? COL_PKEY : 0 );
}

static col_type_handler_t timespec_handler =
        { load_timespec, create_timespec_col,
            get_gvalue_timespec_for_query, get_gvalue_timespec_cond };
/* ----------------------------------------------------------------- */
static void
load_date( GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table )
{
    const GValue* val;
    GDate* date;

    val = gda_data_model_get_value_at_col_name( pModel, table->col_name, row );
    if( gda_value_is_null( val ) ) {
		date = g_date_new_dmy( 1, 1, 1970 );
        (*setter)( pObject, date );
		g_date_free( date );
    } else {
		if( G_VALUE_HOLDS_STRING( val ) ) {
			const gchar* s = g_value_get_string( val );
			guint year = atoi( &s[6] );
			guint month = atoi( &s[0] );
			guint day = atoi( &s[3] );

			date = g_date_new_dmy( day, month, year );
			(*setter)( pObject, date );
			g_date_free( date );

		} else if( G_VALUE_HOLDS_BOXED( val ) ) {
        	date = (GDate*)g_value_get_boxed( val );
        	if( date != NULL ) {
            	(*setter)( pObject, date );
				g_date_free( date );
			}
		} else {
			g_warning( "Unknown timespec type: %s", G_VALUE_TYPE_NAME( val ) );
        }
    }
}

static void
get_gvalue_date( GncGdaBackend* be, QofIdTypeConst obj_name, gpointer pObject,
                const col_cvt_t* table_row, GValue* value )
{
    GDate* date;
    QofAccessFunc getter;

    memset( value, 0, sizeof( GValue ) );

    getter = get_getter( obj_name, table_row );
    date = (GDate*)(*getter)( pObject, NULL );
    if( date != NULL ) {
        g_value_init( value, G_TYPE_DATE );
        g_value_set_boxed( value, date );
    }
}

static void
get_gvalue_date_for_query( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_date( be, obj_name, pObject, table_row, &value );
    add_field_to_query( query, table_row->col_name, &value );
}

static GdaQueryCondition*
get_gvalue_date_cond( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;

    get_gvalue_date( be, obj_name, pObject, table_row, &value );
    return gnc_gda_create_condition_from_field( query, table_row->col_name, &value );
}

static col_type_handler_t date_handler =
        { load_date, create_timespec_col,
            get_gvalue_date_for_query, get_gvalue_date_cond };
/* ----------------------------------------------------------------- */
static gint64
get_integer_value( const GValue* value )
{
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
		g_warning( "Unknown type: %s", G_VALUE_TYPE_NAME( value ) );
	}

	return 0;
}

static void
load_numeric( GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table )
{
    const GValue* val;
    gchar* buf;
    gint64 num, denom;
    gnc_numeric n;
    gboolean isNull = FALSE;

    buf = g_strdup_printf( "%s_num", table->col_name );
    val = gda_data_model_get_value_at_col_name( pModel, buf, row );
    g_free( buf );
    if( gda_value_is_null( val ) ) {
        isNull = TRUE;
        num = 0;
    } else {
        num = get_integer_value( val );
    }
    buf = g_strdup_printf( "%s_denom", table->col_name );
    val = gda_data_model_get_value_at_col_name( pModel, buf, row );
    g_free( buf );
    if( gda_value_is_null( val ) ) {
        isNull = TRUE;
        denom = 1;
    } else {
        denom = get_integer_value( val );
    }
    n = gnc_numeric_create( num, denom );
    if( isNull ) {
        (*setter)( pObject, NULL );
    } else {
        (*setter)( pObject, &n );
    }
}

static void
get_gvalue_numeric( GncGdaBackend* be, QofIdTypeConst obj_name, gpointer pObject,
                const col_cvt_t* table_row, GValue* value )
{
    QofAccessFunc getter;
    gnc_numeric* n;

    memset( value, 0, sizeof( GValue ) );

    getter = get_getter( obj_name, table_row );
    n = (gnc_numeric*)(*getter)( pObject, NULL );
    if( n != NULL ) {
        g_value_init( value, gnc_numeric_get_type() );
        g_value_set_boxed( value, n );
    }
}

static void
get_gvalue_numeric_for_query( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;
    GValue num_value;
    GValue denom_value;
    gnc_numeric* n;
    gchar* s;

    memset( &value, 0, sizeof( GValue ) );
    memset( &num_value, 0, sizeof( GValue ) );
    memset( &denom_value, 0, sizeof( GValue ) );

    get_gvalue_numeric( be, obj_name, pObject, table_row, &value );
    if( G_VALUE_TYPE(&value) != 0 ) {
        n = g_value_get_boxed( &value );
        g_value_init( &num_value, G_TYPE_INT64 );
        g_value_set_int64( &num_value, gnc_numeric_num( *n ) );
        g_value_init( &denom_value, G_TYPE_INT64 );
        g_value_set_int64( &denom_value, gnc_numeric_denom( *n ) );
    }

    s = g_strdup_printf( "%s_num", table_row->col_name );
    add_field_to_query( query, s, &num_value );
    g_free( s );
    s = g_strdup_printf( "%s_denom", table_row->col_name );
    add_field_to_query( query, s, &denom_value );
    g_free( s );
}

static GdaQueryCondition*
get_gvalue_numeric_cond( GncGdaBackend* be, QofIdTypeConst obj_name,
                gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;
    GValue num_value;
    GValue denom_value;
    gnc_numeric* n;
    gchar* s;
    GdaQueryCondition* num_cond;
    GdaQueryCondition* denom_cond;
    GdaQueryCondition* cond;

    memset( &value, 0, sizeof( GValue ) );
    memset( &num_value, 0, sizeof( GValue ) );
    memset( &denom_value, 0, sizeof( GValue ) );

    get_gvalue_numeric( be, obj_name, pObject, table_row, &value );
    if( G_VALUE_TYPE(&value) != 0 ) {
        n = g_value_get_boxed( &value );
        g_value_init( &num_value, G_TYPE_INT64 );
        g_value_set_int64( &num_value, gnc_numeric_num( *n ) );
        g_value_init( &denom_value, G_TYPE_INT64 );
        g_value_set_int64( &denom_value, gnc_numeric_denom( *n ) );
    }

    s = g_strdup_printf( "%s_num", table_row->col_name );
    num_cond = gnc_gda_create_condition_from_field( query, table_row->col_name, &value );
    g_free( s );
    s = g_strdup_printf( "%s_denom", table_row->col_name );
    denom_cond = gnc_gda_create_condition_from_field( query, table_row->col_name, &value );
    g_free( s );

    cond = gda_query_condition_new( query, GDA_QUERY_CONDITION_NODE_AND );
    gda_query_condition_node_add_child( cond, num_cond, NULL );
    gda_query_condition_node_add_child( cond, denom_cond, NULL );

    return cond;
}

static void
create_numeric_col( GdaServerProvider* server, GdaConnection* cnn,
            xmlNodePtr array_data, const col_cvt_t* table_row, gboolean pkey )
{
    const gchar* dbms_type;
    gchar* buf;

    dbms_type = gda_server_provider_get_default_dbms_type( server, cnn,
                                                            G_TYPE_INT64 );
    buf = g_strdup_printf( "%s_num", table_row->col_name );
    add_table_column( server, cnn, array_data, buf, dbms_type,
                        table_row->size, table_row->flags );
    g_free( buf );
    buf = g_strdup_printf( "%s_denom", table_row->col_name );
    add_table_column( server, cnn, array_data, buf, dbms_type,
                        table_row->size, table_row->flags );
    g_free( buf );
}

static col_type_handler_t numeric_handler =
        { load_numeric, create_numeric_col,
            get_gvalue_numeric_for_query, get_gvalue_numeric_cond };
/* ================================================================= */

static col_type_handler_t*
get_handler( int col_type )
{
    col_type_handler_t* pHandler;

    switch( col_type ) {
        case CT_STRING:
            pHandler = &string_handler;
            break;

        case CT_BOOLEAN:
            pHandler = &boolean_handler;
            break;

        case CT_INT:
            pHandler = &int_handler;
            break;

        case CT_INT64:
            pHandler = &int64_handler;
            break;

        case CT_DOUBLE:
            pHandler = &double_handler;
            break;

        case CT_GUID:
            pHandler = &guid_handler;
            break;
                
		case CT_GUID_A:
            pHandler = &account_guid_handler;
            break;
                
		case CT_GUID_C:
            pHandler = &commodity_guid_handler;
            break;
                
		case CT_GUID_T:
            pHandler = &tx_guid_handler;
            break;
                
        case CT_TIMESPEC:
            pHandler = &timespec_handler;
            break;

        case CT_GDATE:
            pHandler = &date_handler;
            break;

        case CT_NUMERIC:
            pHandler = &numeric_handler;
            break;

        default:    /* undefined col type */
            g_assert( FALSE );
    }

    return pHandler;
}

static void retrieve_guid( gpointer pObject, gpointer pValue );

static void 
retrieve_guid( gpointer pObject, gpointer pValue )
{
    GUID* pGuid = (GUID*)pObject;
    GUID* guid = (GUID*)pValue;

	memcpy( pGuid, guid, sizeof( GUID ) );
}


// Table to retrieve just the guid
static col_cvt_t guid_table[] =
{
    { "guid", CT_GUID, 0, 0, NULL, NULL, NULL, retrieve_guid },
    { NULL }
};

const GUID*
gnc_gda_load_guid( GncGdaBackend* be, GdaDataModel* pModel, gint row )
{
	static GUID guid;

    gnc_gda_load_object( be, pModel, row, NULL, &guid, guid_table );

    return &guid;
}

// Table to retrieve just the guid
static col_cvt_t tx_guid_table[] =
{
    { "tx_guid", CT_GUID, 0, 0, NULL, NULL, NULL, retrieve_guid },
    { NULL }
};

const GUID*
gnc_gda_load_tx_guid( GncGdaBackend* be, GdaDataModel* pModel, gint row )
{
    static GUID guid;

    gnc_gda_load_object( be, pModel, row, NULL, &guid, tx_guid_table );

    return &guid;
}

void
gnc_gda_load_object( GncGdaBackend* be, GdaDataModel* pModel, gint row,
                    QofIdTypeConst obj_name, gpointer pObject,
                    const col_cvt_t* table )
{
    int col;
    QofSetterFunc setter;
    col_type_handler_t* pHandler;

    for( col = 0; table[col].col_name != NULL; col++ ) {
        if( table[col].param_name != NULL ) {
            setter = qof_class_get_parameter_setter( obj_name,
                                                    table[col].param_name );
        } else {
            setter = table[col].setter;
        }
        pHandler = get_handler( table[col].col_type );
        pHandler->load_fn( be, pModel, row, setter, pObject, &table[col] );
    }
}

/* ================================================================= */
GdaQuery*
gnc_gda_create_select_query( const GncGdaBackend* be, const gchar* table_name )
{
    GdaQuery* query;
    GdaQueryTarget* target;
    GdaQueryField* allFields;

    /* SELECT */
    query = gda_query_new( be->pDict );
    gda_query_set_query_type( query, GDA_QUERY_TYPE_SELECT );

    /* FROM */
    target = gda_query_target_new( query, table_name );
    gda_query_add_target( query, target, NULL );
    g_object_unref( G_OBJECT(target) );

    /* all fields */
    allFields = gda_query_field_all_new( query, table_name );
    gda_query_field_set_visible( allFields, TRUE );
    gda_entity_add_field( GDA_ENTITY(query), GDA_ENTITY_FIELD(allFields) );
    g_object_unref( G_OBJECT(allFields) );

    return query;
}

/* ================================================================= */
GdaObject*
gnc_gda_execute_query( GncGdaBackend* be, GdaQuery* query )
{
    GError* error = NULL;

    GdaObject* ret;

    ret = gda_query_execute( query, NULL, FALSE, &error );

    if( error != NULL ) {
        g_critical( "SQL error: %s\n", error->message );
    }

    return ret;
}

GdaQuery*
gnc_gda_create_query_from_sql( GncGdaBackend* be, const gchar* sql )
{
    GError* error = NULL;

    GdaQuery* query;

    query = gda_query_new_from_sql( be->pDict, sql, &error );
    if( query == NULL ) {
        g_critical( "SQL error: %s\n", error->message );
    }

	return query;
}

GdaObject*
gnc_gda_execute_sql( GncGdaBackend* be, const gchar* sql )
{
	GdaQuery* query;

	query = gnc_gda_create_query_from_sql( be, sql );
	if( query != NULL ) {
	    return gnc_gda_execute_query( be, query );
	} else {
		return NULL;
	}
}

int
gnc_gda_execute_select_get_count( GncGdaBackend* be, const gchar* sql )
{
    GError* error = NULL;
    int count = 0;

    GdaObject* ret;

    ret = gnc_gda_execute_sql( be, sql );
    if( GDA_IS_DATA_MODEL(ret) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        count = gda_data_model_get_n_rows( pModel );
    }

    if( error != NULL ) {
        g_critical( "SQL error: %s\n", error->message );
    }

    return count;
}

int
gnc_gda_execute_query_get_count( GncGdaBackend* be, GdaQuery* query )
{
    int count = 0;

    GdaObject* ret;

    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL(ret) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        count = gda_data_model_get_n_rows( pModel );
    }

    return count;
}
/* ================================================================= */
static void
get_col_gvalue_for_query( GncGdaBackend* be, QofIdTypeConst obj_name,
                        gpointer pObject, const col_cvt_t* table_row,
                        GdaQuery* query )
{
    col_type_handler_t* pHandler;

    pHandler = get_handler( table_row->col_type );
    pHandler->get_gvalue_query_fn( be, obj_name, pObject, table_row, query );
}

static void
get_col_gvalue_for_condition( GncGdaBackend* be, QofIdTypeConst obj_name,
                        gpointer pObject, const col_cvt_t* table_row,
                        GdaQuery* query )
{
    col_type_handler_t* pHandler;
    GdaQueryCondition* cond;

    pHandler = get_handler( table_row->col_type );
    cond = pHandler->get_gvalue_cond_fn( be, obj_name, pObject, table_row, query );

    gda_query_set_condition( query, cond );
    g_object_unref( G_OBJECT(cond) );
}

gboolean
gnc_gda_object_is_it_in_db( GncGdaBackend* be, const gchar* table_name,
                    QofIdTypeConst obj_name, gpointer pObject,
                    const col_cvt_t* table )
{
    GdaQuery* query;
    int count;

    /* SELECT * FROM */
    query = gnc_gda_create_select_query( be, table_name );

    /* WHERE */
    get_col_gvalue_for_condition( be, obj_name, pObject, &table[0], query );

    count = gnc_gda_execute_query_get_count( be, query );
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
                        QofIdTypeConst obj_name, gpointer pObject,
                        const col_cvt_t* table )
{
    GdaQuery* pQuery;

    if( op == OP_DB_ADD_OR_UPDATE ) {
        if( gnc_gda_object_is_it_in_db( be, table_name, obj_name, pObject, table ) ) {
            pQuery = gnc_gda_build_update_query( be, table_name, obj_name, pObject, table );
        } else {
            pQuery = gnc_gda_build_insert_query( be, table_name, obj_name, pObject, table );
        }
    } else if( op == OP_DB_DELETE ) {
        pQuery = gnc_gda_build_delete_query( be, table_name, obj_name, pObject, table );
    } else if( op == OP_DB_ADD ) {
        pQuery = gnc_gda_build_insert_query( be, table_name, obj_name, pObject, table );
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

#define INITIAL_SQL_BUF_LEN 500

GdaQuery*
gnc_gda_build_insert_query( GncGdaBackend* be,
                            const gchar* table_name,
                            QofIdTypeConst obj_name, gpointer pObject,
                            const col_cvt_t* table )
{
    GdaQuery* query;
    int col;

    GdaQueryTarget* target;

    /* INSERT */
    query = gda_query_new( be->pDict );
    gda_query_set_query_type( query, GDA_QUERY_TYPE_INSERT );

    /* INTO */
    target = gda_query_target_new( query, table_name );
    gda_query_add_target( query, target, NULL );

    /* VALUES */
    for( col = 0; table[col].col_name != NULL; col++ ) {
		if(( table[col].flags & COL_AUTOINC ) == 0 ) {
	        get_col_gvalue_for_query( be, obj_name, pObject, &table[col], query );
		}
    }

    return query;
}

GdaQuery*
gnc_gda_build_update_query( GncGdaBackend* be,
                            const gchar* table_name,
                            QofIdTypeConst obj_name, gpointer pObject,
                            const col_cvt_t* table )
{
    GdaQuery* query;
    int col;

    GdaQueryTarget* target;

    /* UPDATE */
    query = gda_query_new( be->pDict );
    gda_query_set_query_type( query, GDA_QUERY_TYPE_UPDATE );
    target = gda_query_target_new( query, table_name );
    gda_query_add_target( query, target, NULL );

    /* SET */
    for( col = 1; table[col].col_name != NULL; col++ ) {
        get_col_gvalue_for_query( be, obj_name, pObject, &table[col], query );
    }

    /* WHERE */
    get_col_gvalue_for_condition( be, obj_name, pObject, &table[0], query );

    return query;
}

GdaQuery*
gnc_gda_build_delete_query( GncGdaBackend* be,
                            const gchar* table_name,
                            QofIdTypeConst obj_name, gpointer pObject,
                            const col_cvt_t* table )
{
    GdaQuery* query;
    GdaQueryTarget* target;

    /* DELETE */
    query = gda_query_new( be->pDict );
    gda_query_set_query_type( query, GDA_QUERY_TYPE_DELETE );

    /* FROM */
    target = gda_query_target_new( query, table_name );
    gda_query_add_target( query, target, NULL );
    g_object_unref( G_OBJECT(target) );

    /* WHERE */
    get_col_gvalue_for_condition( be, obj_name, pObject, &table[0], query );

    return query;
}

/* ================================================================= */
static void
add_table_column( GdaServerProvider* server, GdaConnection* cnn,
            xmlNodePtr array_data, const gchar* arg, const gchar* dbms_type,
            gint size, gint flags )
{
    xmlNodePtr array_row, array_value;
    gchar* buf;

    array_row = xmlNewChild( array_data, NULL, "gda_array_row", NULL );
    array_value = xmlNewChild( array_row, NULL, "gda_array_value", arg );
    xmlSetProp( array_value, "colid", "COLUMN_NAME" );
    array_value = xmlNewChild( array_row, NULL, "gda_array_value", dbms_type );
    xmlSetProp( array_value, "colid", "COLUMN_TYPE" );
    if( size != 0 ) {
        buf = g_strdup_printf( "%d", size );
        array_value = xmlNewChild( array_row, NULL, "gda_array_value", buf );
        xmlSetProp( array_value, "colid", "COLUMN_SIZE" );
        g_free( buf );
    }
    if( (flags & COL_PKEY) != 0 ) {
        array_value = xmlNewChild( array_row, NULL, "gda_array_value", "TRUE" );
        xmlSetProp( array_value, "colid", "COLUMN_PKEY" );
    }
    if( (flags & COL_NNUL) != 0 ) {
        array_value = xmlNewChild( array_row, NULL, "gda_array_value", "TRUE" );
        xmlSetProp( array_value, "colid", "COLUMN_NNUL" );
    }
    if( (flags & COL_AUTOINC) != 0 ) {
        array_value = xmlNewChild( array_row, NULL, "gda_array_value", "TRUE" );
        xmlSetProp( array_value, "colid", "COLUMN_AUTOINC" );
    }
    if( (flags & COL_UNIQUE) != 0 ) {
        array_value = xmlNewChild( array_row, NULL, "gda_array_value", "TRUE" );
        xmlSetProp( array_value, "colid", "COLUMN_UNIQUE" );
    }
}

gboolean
gnc_gda_create_table( GdaConnection* cnn, const gchar* table_name,
                    col_cvt_t* col_table, GError** error )
{
    GdaServerOperation *op;
    GdaServerProvider *server;
    
    g_return_val_if_fail( GDA_IS_CONNECTION(cnn), FALSE );
    g_return_val_if_fail( gda_connection_is_opened(cnn), FALSE );
    
    server = gda_connection_get_provider_obj( cnn );
    
    op = gda_server_provider_create_operation( server, cnn, 
                           GDA_SERVER_OPERATION_CREATE_TABLE, NULL, error );
    if( GDA_IS_SERVER_OPERATION(op) ) {
        gint col;
        GType type;
        xmlDocPtr parameters;
        xmlNodePtr root;
        xmlNodePtr table, op_data, array_data;
        
        if( table_name == NULL ) {
            g_message( "Table name is NULL!" );      
            g_set_error( error,
                    GDA_GENERAL_ERROR, GDA_GENERAL_OBJECT_NAME_ERROR, 
                    "Couldn't create table with a NULL string" );
            return FALSE;    
        }
        
    
        /* Initation of the xmlDoc */
        parameters = xmlNewDoc( "1.0" );
        
        root = xmlNewDocNode( parameters, NULL, "serv_op_data", NULL );
        xmlDocSetRootElement( parameters, root );
        table = xmlNewChild( root, NULL, "op_data", table_name );
        xmlSetProp( table, "path", "/TABLE_DEF_P/TABLE_NAME" );

        op_data = xmlNewChild( root, NULL, "op_data", NULL );
        xmlSetProp( op_data, "path", "/FIELDS_A" );
        array_data = xmlNewChild( op_data, NULL, "gda_array_data", NULL );

        type = 0;
        
        for( col = 0; col_table[col].col_name != NULL; col++ ) {
            col_type_handler_t* pHandler;

            pHandler = get_handler( col_table[col].col_type );

            pHandler->create_col_fn( server, cnn, array_data, &col_table[col], col == 0 );
        }
        
        if( !gda_server_operation_load_data_from_xml(op, root, error ) ) {
            /* error */
            g_set_error( error, GDA_GENERAL_ERROR, GDA_GENERAL_OPERATION_ERROR, 
                     "The XML operation doesn't exist or could't be loaded" );
            g_object_unref( op );
            xmlFreeDoc( parameters );
            return FALSE;
        } else {
            if( !gda_server_provider_perform_operation( server, cnn, op, error ) ) {
                /* error */
                g_set_error( error,
                    GDA_GENERAL_ERROR, GDA_GENERAL_OPERATION_ERROR, 
                    "The Server couldn't perform the CREATE TABLE operation!" );
                g_object_unref( op );
                xmlFreeDoc( parameters );
                return FALSE;
            }
        }

        g_object_unref( op );
        xmlFreeDoc( parameters );
    } else {
        g_set_error( error, GDA_GENERAL_ERROR, GDA_GENERAL_OBJECT_NAME_ERROR, 
                "The Server doesn't support the CREATE TABLE operation!" );
        return FALSE;
    }
    return TRUE;
}

void gnc_gda_create_table_if_needed( GncGdaBackend* be,
                        const gchar* table_name, col_cvt_t* col_table )
{
    GdaDictTable* table;
    GError* error = NULL;
    GdaDictDatabase* db;
    
    db = gda_dict_get_database( be->pDict );
    table = gda_dict_database_get_table_by_name( db, table_name );
    if( !GDA_IS_DICT_TABLE(table) ) {
        gnc_gda_create_table( be->pConnection, table_name, col_table, &error );
        if( error != NULL ) {
            g_critical( "Error creating table: %s\n", error->message );
        }
    }
}
/* ================================================================= */
#if 0
static gboolean
create_or_drop_db( GdaConnection* cnn, GdaServerOperationType opType,
                const gchar* db_name, GError** error )
{
    typedef struct {
        GdaServerOperationType opType;
        const gchar* op_name;
        const gchar* op_path_name;
    } S_ServerOpInfo;
#define NUMOF(X) (sizeof(X)/sizeof(X[0]))
    static S_ServerOpInfo s_op_info[] =
    {
        { GDA_SERVER_OPERATION_CREATE_DB, "CREATE DB", "DB_DEF_P" },
        { GDA_SERVER_OPERATION_DROP_DB,   "DROP DB",   "DB_DESC_P" }
    };
#define NUM_OPS NUMOF(s_op_info)
    S_ServerOpInfo* op_info;
    GdaServerOperation *op;
    GdaServerProvider *server;
    gchar* buf;
    gint i;
    
    g_return_val_if_fail( GDA_IS_CONNECTION(cnn), FALSE );
    g_return_val_if_fail( gda_connection_is_opened(cnn), FALSE );
    
    if( db_name == NULL ) {
        g_message( "Database name is NULL!" );      
        g_set_error( error,
                GDA_GENERAL_ERROR, GDA_GENERAL_OBJECT_NAME_ERROR, 
                "Couldn't create or drop database with a NULL string" );
        return FALSE;    
    }

    for( i = 0, op_info = NULL; i < NUM_OPS; i++ ) {
        if( s_op_info[i].opType == opType ) {
            op_info = &s_op_info[i];
            break;
        }
    }

    g_return_val_if_fail( op_info != NULL, FALSE );
    
    server = gda_connection_get_provider_obj( cnn );
    op = gda_server_provider_create_operation( server, cnn, opType, NULL,
                                            error );
    if( GDA_IS_SERVER_OPERATION(op) ) {
        xmlDocPtr parameters;
        xmlNodePtr root;
        xmlNodePtr db;
    
        /* Initation of the xmlDoc */
        parameters = xmlNewDoc( "1.0" );
        
        root = xmlNewDocNode( parameters, NULL, "serv_op_data", NULL );
        xmlDocSetRootElement( parameters, root );
        db = xmlNewChild( root, NULL, "op_data", db_name );
        buf = g_strdup_printf( "/%s/DB_NAME", op_info->op_path_name );
        xmlSetProp( db, "path", buf );
        g_free( buf );

        if( !gda_server_operation_load_data_from_xml(op, root, error ) ) {
            /* error */
            g_set_error( error, GDA_GENERAL_ERROR, GDA_GENERAL_OPERATION_ERROR, 
                     "The XML operation doesn't exist or could't be loaded" );
            g_object_unref( op );
            xmlFreeDoc( parameters );
            return FALSE;
        } else {
            if( !gda_server_provider_perform_operation( server, cnn, op, error ) ) {
                /* error */
                buf = g_strdup_printf( "The server couldn't perform the %s operation",
                                        op_info->op_name );
                g_set_error( error,
                    GDA_GENERAL_ERROR, GDA_GENERAL_OPERATION_ERROR, 
                    buf );
                g_free( buf );
                g_object_unref( op );
                xmlFreeDoc( parameters );
                return FALSE;
            }
        }

        g_object_unref( op );
        xmlFreeDoc( parameters );
    } else {
        buf = g_strdup_printf( "The server doesn't support the %s operation",
                                op_info->op_name );
        g_set_error( error, GDA_GENERAL_ERROR, GDA_GENERAL_OBJECT_NAME_ERROR, 
                    buf );
        g_free( buf );
        return FALSE;
    }
    return TRUE;
}

static gboolean
drop_db( GncGdaBackend* be, const gchar* db_name, GError** error )
{
    return create_or_drop_db( be->pConnection, GDA_SERVER_OPERATION_DROP_DB,
                                db_name, error );
}

static gboolean
create_db( GncGdaBackend* be, const gchar* db_name, GError** error )
{
    return create_or_drop_db( be->pConnection, GDA_SERVER_OPERATION_CREATE_DB,
                                db_name, error );
}
#endif
/* ========================== END OF FILE ===================== */
