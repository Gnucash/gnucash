/********************************************************************
 * gnc-backend-util-sql.c: load and save data to SQL                *
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
/** @file gnc-backend-util-sql.c
 *  @brief load and save data to SQL - utility functions
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file contains utility routines to support saving/restoring
 * data to/from an SQL db
 */

#include <stdlib.h>
#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>

#include "qof.h"
#include "qofquery-p.h"
#include "qofquerycore-p.h"
#include "TransLog.h"
#include "gnc-engine.h"

#include "gnc-backend-util-sql.h"
#include "gnc-gconf-utils.h"

static QofLogModule log_module = G_LOG_DOMAIN;

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

static GValue*
create_gvalue_from_string( gchar* s )
{
	GValue* s_gval;

	s_gval = g_new0( GValue, 1 );
	g_value_init( s_gval, G_TYPE_STRING );
	g_value_take_string( s_gval, s );

	return s_gval;
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
gnc_sql_get_getter( QofIdTypeConst obj_name, const col_cvt_t* table_row )
{
    QofAccessFunc getter;

	g_return_val_if_fail( obj_name != NULL, NULL );
	g_return_val_if_fail( table_row != NULL, NULL );

	if( (table_row->flags & COL_AUTOINC) != 0 ) {
		getter = get_autoinc_id;
    } else if( table_row->param_name != NULL ) {
        getter = qof_class_get_parameter_getter( obj_name,
                                                table_row->param_name );
    } else {
        getter = table_row->getter;
    }

    return getter;
}

/* ----------------------------------------------------------------- */
void
gnc_sql_add_colname_to_list( const col_cvt_t* table_row, GList** pList )
{
	(*pList) = g_list_append( (*pList), g_strdup( table_row->col_name ) );
}

/* ----------------------------------------------------------------- */
void
gnc_sql_add_subtable_colnames_to_list( const col_cvt_t* table_row, const col_cvt_t* subtable,
								GList** pList )
{
	const col_cvt_t* subtable_row;
	gchar* buf;

	for( subtable_row = subtable; subtable_row->col_name != NULL; subtable_row++ ) {
		buf = g_strdup_printf( "%s_%s", table_row->col_name, subtable_row->col_name );
		(*pList) = g_list_append( (*pList), buf );
	}
}

GncSqlColumnInfo*
gnc_sql_create_column_info( const col_cvt_t* table_row, const gchar* type,
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
            const col_cvt_t* table_row )
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
add_string_col_info_to_list( const GncSqlBackend* be, const col_cvt_t* table_row,
								GList** pList )
{
    const gchar* dbms_type;
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = gnc_sql_create_column_info( table_row,
					gnc_sql_connection_get_column_type_name( be->conn,
											G_TYPE_STRING, table_row->size ),
				    table_row->size );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_string_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                const gpointer pObject, const col_cvt_t* table_row, GSList** pList )
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
    if( s ) {
		buf = g_strdup_printf( "'%s'", s );
        g_value_init( value, G_TYPE_STRING );
        g_value_take_string( value, buf );
    } else {
        g_value_init( value, G_TYPE_STRING );
		g_value_set_string( value, "NULL" );
	}

	(*pList) = g_slist_append( (*pList), value );
}

static col_type_handler_t string_handler
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
            const col_cvt_t* table_row )
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
add_int_col_info_to_list( const GncSqlBackend* be, const col_cvt_t* table_row,
								GList** pList )
{
    const gchar* dbms_type;
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = gnc_sql_create_column_info( table_row,
					gnc_sql_connection_get_column_type_name( be->conn,
											G_TYPE_INT, table_row->size ),
				    0 );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_int_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                const gpointer pObject, const col_cvt_t* table_row, GSList** pList )
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

    i_getter = (IntAccessFunc)gnc_sql_get_getter( obj_name, table_row );
    int_value = (*i_getter)( pObject );
    g_value_init( value, G_TYPE_INT );
    g_value_set_int( value, int_value );

	(*pList) = g_slist_append( (*pList), value );
}

static col_type_handler_t int_handler
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
            const col_cvt_t* table_row )
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
add_boolean_col_info_to_list( const GncSqlBackend* be, const col_cvt_t* table_row,
								GList** pList )
{
    const gchar* dbms_type;
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = gnc_sql_create_column_info( table_row,
					gnc_sql_connection_get_column_type_name( be->conn,
											G_TYPE_INT, table_row->size ),
				    0 );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_boolean_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                const gpointer pObject, const col_cvt_t* table_row, GSList** pList )
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

    b_getter = (BooleanAccessFunc)gnc_sql_get_getter( obj_name, table_row );
    int_value = ((*b_getter)( pObject )) ? 1 : 0;
    g_value_init( value, G_TYPE_INT );
    g_value_set_int( value, int_value );

	(*pList) = g_slist_append( (*pList), value );
}

static col_type_handler_t boolean_handler
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
            const col_cvt_t* table_row )
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
add_int64_col_info_to_list( const GncSqlBackend* be, const col_cvt_t* table_row,
								GList** pList )
{
    const gchar* dbms_type;
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = gnc_sql_create_column_info( table_row,
					gnc_sql_connection_get_column_type_name( be->conn,
											G_TYPE_INT64, table_row->size ),
				    0 );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_int64_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
				const gpointer pObject, const col_cvt_t* table_row, GSList** pList )
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
    getter = (Int64AccessFunc)gnc_sql_get_getter( obj_name, table_row );
    i64_value = (*getter)( pObject );
    g_value_init( value, G_TYPE_INT64 );
    g_value_set_int64( value, i64_value );

	(*pList) = g_slist_append( (*pList), value );
}

static col_type_handler_t int64_handler
	= { load_int64,
		add_int64_col_info_to_list,
		gnc_sql_add_colname_to_list,
		add_gvalue_int64_to_slist };
/* ----------------------------------------------------------------- */

static void
load_double( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table_row )
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
add_double_col_info_to_list( const GncSqlBackend* be, const col_cvt_t* table_row,
								GList** pList )
{
    const gchar* dbms_type;
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = gnc_sql_create_column_info( table_row,
					gnc_sql_connection_get_column_type_name( be->conn,
											G_TYPE_DOUBLE, table_row->size ),
				    0 );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_double_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
						const gpointer pObject, const col_cvt_t* table_row, GSList** pList )
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
        g_value_init( value, G_TYPE_STRING );
		g_value_set_string( value, "NULL" );
	}

	(*pList) = g_slist_append( (*pList), value );
}

static col_type_handler_t double_handler
	= { load_double,
		add_double_col_info_to_list,
		gnc_sql_add_colname_to_list,
		add_gvalue_double_to_slist };
/* ----------------------------------------------------------------- */

static void
load_guid( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table_row )
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
add_guid_col_info_to_list( const GncSqlBackend* be, const col_cvt_t* table_row,
								GList** pList )
{
    const gchar* dbms_type;
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = gnc_sql_create_column_info( table_row, "CHAR", GUID_ENCODING_LENGTH );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_guid_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
					const gpointer pObject, const col_cvt_t* table_row, GSList** pList )
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
    if( guid != NULL ) {
		gchar* buf;
        (void)guid_to_string_buff( guid, guid_buf );
		buf = g_strdup_printf( "'%s'", guid_buf );
        g_value_init( value, G_TYPE_STRING );
        g_value_take_string( value, buf );
    } else {
        g_value_init( value, G_TYPE_STRING );
		g_value_set_string( value, "NULL" );
	}

	(*pList) = g_slist_append( (*pList), value );
}

static col_type_handler_t guid_handler
	= { load_guid,
		add_guid_col_info_to_list,
		gnc_sql_add_colname_to_list,
        add_gvalue_guid_to_slist };
/* ----------------------------------------------------------------- */

void
gnc_sql_add_gvalue_objectref_guid_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
						const gpointer pObject, const col_cvt_t* table_row, GSList** pList )
{
    QofAccessFunc getter;
    const GUID* guid = NULL;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
	QofInstance* inst;
	GValue* value;
	gchar* buf;

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
    if( guid != NULL ) {
        (void)guid_to_string_buff( guid, guid_buf );
		buf = g_strdup_printf( "'%s'", guid_buf );
        g_value_init( value, G_TYPE_STRING );
        g_value_take_string( value, buf );
    } else {
        g_value_init( value, G_TYPE_STRING );
		g_value_set_string( value, "NULL" );
	}

	(*pList) = g_slist_append( (*pList), value );
}

void
gnc_sql_add_objectref_guid_col_info_to_list( const GncSqlBackend* be,
								const col_cvt_t* table_row,
								GList** pList )
{
	add_guid_col_info_to_list( be, table_row, pList );
}

/* ----------------------------------------------------------------- */
typedef Timespec (*TimespecAccessFunc)( const gpointer );
typedef void (*TimespecSetterFunc)( const gpointer, Timespec );

#define TIMESPEC_STR_FORMAT "'%04d%02d%02d%02d%02d%02d'"
#define TIMESPEC_COL_SIZE (4+2+2+2+2+2)

static void
load_timespec( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table_row )
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
									s[9], s[10],
									s[11], s[12],
									s[13], s[14] );
		    ts = gnc_iso8601_to_timespec_gmt( buf );
			(*ts_setter)( pObject, ts );
			g_free( buf );

		} else {
			PWARN( "Unknown timespec type: %s", G_VALUE_TYPE_NAME( val ) );
        }
    }
}

static void
add_timespec_col_info_to_list( const GncSqlBackend* be, const col_cvt_t* table_row,
								GList** pList )
{
    const gchar* dbms_type;
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = gnc_sql_create_column_info( table_row,
					"CHAR", TIMESPEC_COL_SIZE );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_timespec_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                const gpointer pObject, const col_cvt_t* table_row, GSList** pList )
{
    TimespecAccessFunc ts_getter;
    Timespec ts;
	gchar* datebuf;
	time_t time;
	struct tm tm;
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
	(void)gmtime_r( &time, &tm );	

	if( tm.tm_year < 60 ) year = tm.tm_year + 2000;
	else year = tm.tm_year + 1900;

	datebuf = g_strdup_printf( TIMESPEC_STR_FORMAT,
					year, tm.tm_mon+1, tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec );
    g_value_init( value, G_TYPE_STRING );
	g_value_take_string( value, datebuf );

	(*pList) = g_slist_append( (*pList), value );
}

static col_type_handler_t timespec_handler
	= { load_timespec,
		add_timespec_col_info_to_list,
		gnc_sql_add_colname_to_list,
		add_gvalue_timespec_to_slist };
/* ----------------------------------------------------------------- */
#define DATE_COL_SIZE 8

static void
load_date( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table_row )
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

			date = g_date_new_dmy( day, month, year );
			(*setter)( pObject, date );
			g_date_free( date );

		} else {
			PWARN( "Unknown timespec type: %s", G_VALUE_TYPE_NAME( val ) );
        }
    }
}

static void
add_date_col_info_to_list( const GncSqlBackend* be, const col_cvt_t* table_row,
								GList** pList )
{
    const gchar* dbms_type;
	GncSqlColumnInfo* info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	info = gnc_sql_create_column_info( table_row,
					"CHAR", DATE_COL_SIZE );

	*pList = g_list_append( *pList, info );
}

static void
add_gvalue_date_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
				const gpointer pObject,
                const col_cvt_t* table_row, GSList** pList )
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
	buf = g_strdup_printf( "'%04d%02d%02d'",
					g_date_get_year( date ), g_date_get_month( date ), g_date_get_day( date ) );
    g_value_init( value, G_TYPE_STRING );
    g_value_take_string( value, buf );

	(*pList) = g_slist_append( (*pList), value );
}

static col_type_handler_t date_handler
	= { load_date,
		add_date_col_info_to_list,
		gnc_sql_add_colname_to_list,
		add_gvalue_date_to_slist };
/* ----------------------------------------------------------------- */
typedef gnc_numeric (*NumericGetterFunc)( const gpointer );
typedef void (*NumericSetterFunc)( gpointer, gnc_numeric );

static const col_cvt_t numeric_col_table[] =
{
    { "num",    CT_INT64, 0, COL_NNUL, "guid" },
    { "denom",  CT_INT64, 0, COL_NNUL, "guid" },
	{ NULL }
};

static void
load_numeric( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table_row )
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
add_numeric_col_info_to_list( const GncSqlBackend* be, const col_cvt_t* table_row,
								GList** pList )
{
    const gchar* dbms_type;
	GncSqlColumnInfo* info;
    gchar* buf;
	const col_cvt_t* subtable_row;
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
add_numeric_colname_to_list( const col_cvt_t* table_row, GList** pList )
{
	gnc_sql_add_subtable_colnames_to_list( table_row, numeric_col_table, pList );
}

static void
add_gvalue_numeric_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
					const gpointer pObject, const col_cvt_t* table_row, GSList** pList )
{
    NumericGetterFunc getter;
    gnc_numeric n;
    GValue* num_value;
    GValue* denom_value;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    getter = (NumericGetterFunc)gnc_sql_get_getter( obj_name, table_row );
    n = (*getter)( pObject );

    num_value = g_new0( GValue, 1 );
    g_value_init( num_value, G_TYPE_INT64 );
    g_value_set_int64( num_value, gnc_numeric_num( n ) );
    denom_value = g_new0( GValue, 1 );
    g_value_init( denom_value, G_TYPE_INT64 );
    g_value_set_int64( denom_value, gnc_numeric_denom( n ) );

	(*pList) = g_slist_append( (*pList), num_value );
	(*pList) = g_slist_append( (*pList), denom_value );
}

static col_type_handler_t numeric_handler
	= { load_numeric,
		add_numeric_col_info_to_list,
		add_numeric_colname_to_list,
		add_gvalue_numeric_to_slist };
/* ================================================================= */

static GHashTable* g_columnTypeHash = NULL;

void
gnc_sql_register_col_type_handler( const gchar* colType, const col_type_handler_t* handler )
{
	g_return_if_fail( colType != NULL );
	g_return_if_fail( handler != NULL );

	if( g_columnTypeHash == NULL ) {
		g_columnTypeHash = g_hash_table_new( g_str_hash, g_str_equal );
	}

	g_hash_table_insert( g_columnTypeHash, (gpointer)colType, (gpointer)handler );
	DEBUG( "Col type %s registered\n", colType );
}

static col_type_handler_t*
get_handler( const col_cvt_t* table_row )
{
    col_type_handler_t* pHandler;

	g_return_val_if_fail( table_row != NULL, NULL );
	g_return_val_if_fail( table_row->col_type != NULL, NULL );

	pHandler = g_hash_table_lookup( g_columnTypeHash, table_row->col_type );
	if( pHandler == NULL ) {
        g_assert( FALSE );
    }

    return pHandler;
}

void
gnc_sql_register_standard_col_type_handlers( void )
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
static col_cvt_t guid_table[] =
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
static col_cvt_t tx_guid_table[] =
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
                    const col_cvt_t* table_row )
{
    int col;
    QofSetterFunc setter;
    col_type_handler_t* pHandler;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    for( col = 0; table_row[col].col_name != NULL; col++ ) {
		if( (table_row[col].flags & COL_AUTOINC) != 0 ) {
			setter = set_autoinc_id;
        } else if( table_row[col].param_name != NULL ) {
            setter = qof_class_get_parameter_setter( obj_name,
                                                    table_row[col].param_name );
        } else {
            setter = table_row[col].setter;
        }
        pHandler = get_handler( &table_row[col] );
        pHandler->load_fn( be, row, setter, pObject, &table_row[col] );
    }
}

/* ================================================================= */
GncSqlStatement*
gnc_sql_create_select_statement( const GncSqlBackend* be, const gchar* table_name,
							const col_cvt_t* table_row )
{
	gchar* sql;

	sql = g_strdup_printf( "SELECT * FROM %s", table_name );
	return gnc_sql_create_statement_from_sql( be, sql );
}

static GncSqlStatement*
create_single_col_select_statement( const GncSqlBackend* be,
							const gchar* table_name,
							const col_cvt_t* table_row )
{
	gchar* sql;

	sql = g_strdup_printf( "SELECT %s FROM %s", table_row->col_name, table_name );
	return gnc_sql_create_statement_from_sql( be, sql );
}

/* ================================================================= */
gint
gnc_sql_execute_statement( GncSqlBackend* be, GncSqlStatement* stmt )
{
    GError* error = NULL;
    gint numRows;

	g_return_val_if_fail( be != NULL, -1 );
	g_return_val_if_fail( stmt != NULL, -1 );

	numRows = gnc_sql_connection_execute_nonselect_statement( be->conn, stmt );
    if( error != NULL ) {
        PERR( "SQL error: %s\n%s\n", gnc_sql_statement_to_sql( stmt ), error->message );
		qof_backend_set_error( &be->be, ERR_BACKEND_SERVER_ERR );
    }

    return numRows;
}

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

GncSqlResult*
gnc_sql_execute_sql_statement( GncSqlBackend* be, GncSqlStatement* sqlStmt )
{
	return gnc_sql_execute_select_statement( be, sqlStmt );
}

GncSqlStatement*
gnc_sql_create_statement_from_sql( const GncSqlBackend* be, const gchar* sql )
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
gnc_sql_execute_select_sql( const GncSqlBackend* be, const gchar* sql )
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

gint
gnc_sql_execute_nonselect_sql( const GncSqlBackend* be, const gchar* sql )
{
	GncSqlStatement* stmt;
    GError* error = NULL;
	gint result;

	g_return_val_if_fail( be != NULL, 0 );
	g_return_val_if_fail( sql != NULL, 0 );

	stmt = gnc_sql_create_statement_from_sql( be, sql );
    if( stmt == NULL ) {
		return 0;
    }
	result = gnc_sql_connection_execute_nonselect_statement( be->conn, stmt );
	gnc_sql_statement_dispose( stmt );
    if( error != NULL ) {
        PERR( "SQL error: %s\n%s\n", sql, error->message );
    }
	return result;
}

int
gnc_sql_execute_select_get_count( const GncSqlBackend* be, const gchar* sql )
{
    int count = 0;
    GncSqlResult* result;

	g_return_val_if_fail( be != NULL, 0 );
	g_return_val_if_fail( sql != NULL, 0 );

    result = gnc_sql_execute_select_sql( be, sql );
    if( result != NULL ) {
        count = gnc_sql_result_get_num_rows( result );
		gnc_sql_result_dispose( result );
    }

    return count;
}

int
gnc_sql_execute_statement_get_count( GncSqlBackend* be, GncSqlStatement* stmt )
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
                    const col_cvt_t* table )
{
    GncSqlStatement* sqlStmt;
    int count;
    col_type_handler_t* pHandler;
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

    count = gnc_sql_execute_statement_get_count( be, sqlStmt );
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
                        const col_cvt_t* table )
{
    GncSqlStatement* sqlStmt;

	g_return_val_if_fail( be != NULL, FALSE );
	g_return_val_if_fail( table_name != NULL, FALSE );
	g_return_val_if_fail( obj_name != NULL, FALSE );
	g_return_val_if_fail( pObject != NULL, FALSE );
	g_return_val_if_fail( table != NULL, FALSE );

    if( op == OP_DB_ADD_OR_UPDATE ) {
        if( gnc_sql_object_is_it_in_db( be, table_name, obj_name, pObject, table ) ) {
            sqlStmt = gnc_sql_build_update_statement( be, table_name, obj_name, pObject, table );
        } else {
            sqlStmt = gnc_sql_build_insert_statement( be, table_name, obj_name, pObject, table );
        }
    } else if( op == OP_DB_DELETE ) {
        sqlStmt = gnc_sql_build_delete_statement( be, table_name, obj_name, pObject, table );
    } else if( op == OP_DB_ADD ) {
        sqlStmt = gnc_sql_build_insert_statement( be, table_name, obj_name, pObject, table );
    } else {
        g_assert( FALSE );
    }
    if( sqlStmt != NULL ) {
        gnc_sql_execute_statement( be, sqlStmt );

        return TRUE;
    } else {
        return FALSE;
    }
}

#define INITIAL_SQL_BUF_LEN 500

static GSList*
create_gslist_from_values( GncSqlBackend* be,
                            QofIdTypeConst obj_name, gpointer pObject,
                            const col_cvt_t* table )
{
	GSList* list = NULL;
	col_type_handler_t* pHandler;
	const col_cvt_t* table_row;

    for( table_row = table; table_row->col_name != NULL; table_row++ ) {
		if(( table_row->flags & COL_AUTOINC ) == 0 ) {
    		pHandler = get_handler( table_row );
			pHandler->add_gvalue_to_slist_fn( be, obj_name, pObject, table_row, &list );
		}
    }

	return list;
}

gchar*
gnc_sql_get_sql_value( const GValue* value )
{
	if( value != NULL && G_IS_VALUE( value ) ) {
		if( g_value_type_transformable( G_VALUE_TYPE(value), G_TYPE_STRING ) ) {
			GValue *string;
			gchar *str;
			
			string = g_value_init( g_new0( GValue, 1 ), G_TYPE_STRING );
			g_value_transform( value, string );
			str = g_value_dup_string( string );
			g_free( string );
			return str;
		} else {
			return "$$$";
		}
	} else {
		return "";
	}
}

GncSqlStatement*
gnc_sql_build_insert_statement( GncSqlBackend* be,
                            const gchar* table_name,
                            QofIdTypeConst obj_name, gpointer pObject,
                            const col_cvt_t* table )
{
	GncSqlStatement* stmt;
	GString* sql;
	GSList* values;
	GSList* node;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( table_name != NULL, NULL );
	g_return_val_if_fail( obj_name != NULL, NULL );
	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( table != NULL, NULL );

	sql = g_string_new( g_strdup_printf( "INSERT INTO %s VALUES(", table_name ) );
	values = create_gslist_from_values( be, obj_name, pObject, table );
	for( node = values; node != NULL; node = node->next ) {
		GValue* value = (GValue*)node->data;
		if( node != values ) {
			g_string_append( sql, "," );
		}
		g_string_append( sql, gnc_sql_get_sql_value( value ) );
	}
	g_string_append( sql, ")" );

	stmt = gnc_sql_connection_create_statement_from_sql( be->conn, sql->str );
	return stmt;
}

GncSqlStatement*
gnc_sql_build_update_statement( GncSqlBackend* be,
                            const gchar* table_name,
                            QofIdTypeConst obj_name, gpointer pObject,
                            const col_cvt_t* table )
{
	GncSqlStatement* stmt;
	GString* sql;
	GSList* values;
	GList* colnames = NULL;
	GSList* value;
	GList* colname;
	gboolean firstCol;
	const col_cvt_t* table_row = table;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( table_name != NULL, NULL );
	g_return_val_if_fail( obj_name != NULL, NULL );
	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( table != NULL, NULL );

    // Get all col names and all values
	for( ; table_row->col_name != NULL; table_row++ ) {
    	col_type_handler_t* pHandler;

		// Add col names to the list
		pHandler = get_handler( table_row );
		pHandler->add_colname_to_list_fn( table_row, &colnames );
	}
	values = create_gslist_from_values( be, obj_name, pObject, table );

	// Create the SQL statement
	sql = g_string_new( g_strdup_printf( "UPDATE %s SET ", table_name ) );

	firstCol = TRUE;
	for( colname = colnames->next, value = values->next;
					colname != NULL && value != NULL;
					colname = colname->next, value = value->next ) {
		if( !firstCol ) {
			g_string_append( sql, "," );
		}
		g_string_append( sql, (gchar*)colname->data );
		g_string_append( sql, "=" );
		g_string_append( sql, gnc_sql_get_sql_value( (GValue*)(value->data) ) );
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

GncSqlStatement*
gnc_sql_build_delete_statement( GncSqlBackend* be,
                            const gchar* table_name,
                            QofIdTypeConst obj_name, gpointer pObject,
                            const col_cvt_t* table )
{
	GncSqlStatement* stmt;
	GString* sql;
    col_type_handler_t* pHandler;
	GSList* list = NULL;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( table_name != NULL, NULL );
	g_return_val_if_fail( obj_name != NULL, NULL );
	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( table != NULL, NULL );

	sql = g_string_new( g_strdup_printf( "DELETE FROM %s ", table_name ) );

	stmt = gnc_sql_connection_create_statement_from_sql( be->conn, sql->str );

    /* WHERE */
    pHandler = get_handler( table );
	pHandler->add_gvalue_to_slist_fn( be, obj_name, pObject, table, &list );
	gnc_sql_statement_add_where_cond( stmt, obj_name, pObject, &table[0], (GValue*)(list->data) );

	return stmt;
}

/* ================================================================= */
#if 0
void
gnc_gda_add_table_column( GdaServerOperation* op, const gchar* arg, const gchar* dbms_type,
            gint size, gint flags, guint col_num )
{
    gchar* buf;
	GError* error = NULL;
	gboolean ok;

	g_return_if_fail( op != NULL );
	g_return_if_fail( arg != NULL );
	g_return_if_fail( dbms_type != NULL );

	ok = gda_server_operation_set_value_at( op, arg, &error, "/FIELDS_A/@COLUMN_NAME/%d", col_num );
	if( !ok ) return;
	ok = gda_server_operation_set_value_at( op, dbms_type, &error, "/FIELDS_A/@COLUMN_TYPE/%d", col_num );
	if( !ok ) return;
    if( size != 0 ) {
        buf = g_strdup_printf( "%d", size );
		ok = gda_server_operation_set_value_at( op, buf, &error, "/FIELDS_A/@COLUMN_SIZE/%d", col_num );
        g_free( buf );
		if( !ok ) return;
    }
	ok = gda_server_operation_set_value_at( op,
										(flags & COL_PKEY) ? "TRUE" : "FALSE",
										&error, "/FIELDS_A/@COLUMN_PKEY/%d", col_num );
	if( error != NULL ) {
		PWARN( "Error setting PKEY for %s: %s\n", arg, error->message );
	}
	if( !ok ) return;
	ok = gda_server_operation_set_value_at( op,
										(flags & COL_NNUL) ? "TRUE" : "FALSE",
										&error, "/FIELDS_A/@COLUMN_NNUL/%d", col_num );
	if( error != NULL ) {
		PWARN( "Error setting NNUL for %s: %s\n", arg, error->message );
	}
	if( !ok ) return;
	ok = gda_server_operation_set_value_at( op,
										(flags & COL_AUTOINC) ? "TRUE" : "FALSE",
										&error, "/FIELDS_A/@COLUMN_AUTOINC/%d", col_num );
	if( error != NULL ) {
		PWARN( "Error setting AUTOINC for %s: %s\n", arg, error->message );
	}
	if( !ok ) return;
	ok = gda_server_operation_set_value_at( op,
										(flags & COL_UNIQUE) ? "TRUE" : "FALSE",
										&error, "/FIELDS_A/@COLUMN_UNIQUE/%d", col_num );
	if( error != NULL ) {
		PWARN( "Error setting UNIQUE for %s: %s\n", arg, error->message );
	}
}
#endif

static gboolean
create_table( const GncSqlBackend* be, const gchar* table_name,
				const col_cvt_t* col_table, GError** pError )
{
	GList* col_info_list = NULL;
    
	g_return_val_if_fail( be != NULL, FALSE );
	g_return_val_if_fail( table_name != NULL, FALSE );
	g_return_val_if_fail( col_table != NULL, FALSE );
	g_return_val_if_fail( pError != NULL, FALSE );
    
    for( ; col_table->col_name != NULL; col_table++ ) {
        col_type_handler_t* pHandler;

        pHandler = get_handler( col_table );
        pHandler->add_col_info_to_list_fn( be, col_table, &col_info_list );
    }
	gnc_sql_connection_create_table( be->conn, table_name, col_info_list );
	return TRUE;
}

gboolean
gnc_sql_create_table( const GncSqlBackend* be, const gchar* table_name,
					gint table_version, const col_cvt_t* col_table, GError** error )
{
	gboolean ok;

	ok = create_table( be, table_name, col_table, error );
	if( ok ) {
		register_table_version( be, table_name, table_version );
	}
	return ok;
}

void
gnc_sql_create_index( const GncSqlBackend* be, const gchar* index_name,
					const gchar* table_name,
                    const col_cvt_t* col_table )
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

static col_cvt_t version_table[] =
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

	be->versions = g_hash_table_new( g_str_hash, g_str_equal );

	if( gnc_sql_connection_does_table_exist( be->conn, VERSION_TABLE_NAME ) ) {
		GncSqlResult* result;
		gchar* sql;

		sql = g_strdup_printf( "SELECT * FROM %s", VERSION_TABLE_NAME );
		result = gnc_sql_execute_select_sql( be, sql );
		g_free( sql );
		if( result != NULL ) {
			const GValue* name;
			const GValue* version;
			GncSqlRow* row;

			row = gnc_sql_result_get_first_row( result );
			while( row != NULL ) {
    			name = gnc_sql_row_get_value_at_col_name( row, TABLE_COL_NAME );
				version = gnc_sql_row_get_value_at_col_name( row, VERSION_COL_NAME );
				g_hash_table_insert( be->versions,
									(gpointer)g_value_get_string( name ),
									GINT_TO_POINTER(g_value_get_int( version )) );
				row = gnc_sql_result_get_next_row( result );
			}
			gnc_sql_result_dispose( result );
		}
	} else {
		gboolean ok;
		GError* error = NULL;

		ok = create_table( be, VERSION_TABLE_NAME, version_table, &error );
		if( error != NULL ) {
			PERR( "Error creating versions table: %s\n", error->message );
		}
	}
}

/**
 * Resets the version table information by removing all version table info.
 * It also recreates the version table in the db.
 *
 * @param be Backend struct
 */
void
gnc_sql_reset_version_info( GncSqlBackend* be )
{
	gboolean ok;
	GError* error = NULL;

	g_return_if_fail( be != NULL );

	ok = create_table( be, VERSION_TABLE_NAME, version_table, &error );
	if( error != NULL ) {
		PERR( "Error creating versions table: %s\n", error->message );
	}
	if( be->versions == NULL ) {
		be->versions = g_hash_table_new( g_str_hash, g_str_equal );
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
		(void)gnc_sql_execute_nonselect_sql( be, sql );
		g_free( sql );
	}

	g_hash_table_insert( be->versions, (gpointer)table_name, GINT_TO_POINTER(version) );
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
