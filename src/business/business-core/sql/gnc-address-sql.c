/********************************************************************\
 * gnc-address-sql.c -- address sql backend implementation          *
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
 *                                                                  *
\********************************************************************/

/** @file gnc-address-sql.c
 *  @brief load and save address data to SQL
 *  @author Copyright (c) 2007-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-engine.h"

#include "gnc-backend-sql.h"
#include "gnc-address-sql.h"

static QofLogModule log_module = G_LOG_DOMAIN;

#define ADDRESS_MAX_NAME_LEN 1024
#define ADDRESS_MAX_ADDRESS_LINE_LEN 1024
#define ADDRESS_MAX_PHONE_LEN 128
#define ADDRESS_MAX_FAX_LEN 128
#define ADDRESS_MAX_EMAIL_LEN 256

static GncSqlColumnTableEntry col_table[] =
{
	{ "name",  CT_STRING, ADDRESS_MAX_NAME_LEN,         COL_NNUL, NULL, ADDRESS_NAME },
	{ "addr1", CT_STRING, ADDRESS_MAX_ADDRESS_LINE_LEN, COL_NNUL, NULL, ADDRESS_ONE },
	{ "addr2", CT_STRING, ADDRESS_MAX_ADDRESS_LINE_LEN, COL_NNUL, NULL, ADDRESS_TWO },
	{ "addr3", CT_STRING, ADDRESS_MAX_ADDRESS_LINE_LEN, COL_NNUL, NULL, ADDRESS_THREE },
	{ "addr4", CT_STRING, ADDRESS_MAX_ADDRESS_LINE_LEN, COL_NNUL, NULL, ADDRESS_FOUR },
	{ "phone", CT_STRING, ADDRESS_MAX_PHONE_LEN,        COL_NNUL, NULL, ADDRESS_PHONE },
	{ "fax",   CT_STRING, ADDRESS_MAX_FAX_LEN,          COL_NNUL, NULL, ADDRESS_FAX },
	{ "email", CT_STRING, ADDRESS_MAX_EMAIL_LEN,        COL_NNUL, NULL, ADDRESS_EMAIL },
	{ NULL }
};

typedef void (*AddressSetterFunc)( gpointer, GncAddress* );
typedef GncAddress* (*AddressGetterFunc)( const gpointer );

static void
load_address( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    gchar* buf;
    GncAddress* addr;
	AddressSetterFunc a_setter = (AddressSetterFunc)setter;
	const GncSqlColumnTableEntry* subtable;
	const gchar* s;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    addr = gncAddressCreate( be->primary_book, NULL );
	for( subtable = col_table; subtable->col_name != NULL; subtable++ ) {
    	buf = g_strdup_printf( "%s_%s", table_row->col_name, subtable->col_name );
    	val = gnc_sql_row_get_value_at_col_name( row, buf );
    	g_free( buf );
    	if( val == NULL ) {
        	s = NULL;
    	} else {
        	s = g_value_get_string( val );
    	}
    	if( subtable->gobj_param_name != NULL ) {
			g_object_set( addr, subtable->gobj_param_name, s, NULL );
    	} else {
	        if( subtable->qof_param_name != NULL ) {
            	setter = qof_class_get_parameter_setter( GNC_ID_ADDRESS, subtable->qof_param_name );
        	} else {
            	setter = subtable->setter;
        	}
			(*setter)( addr, (const gpointer)s );
    	}
    }
    (*a_setter)( pObject, addr );
}

static void
add_address_col_info_to_list( const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,
								GList** pList )
{
	GncSqlColumnInfo* info;
    gchar* buf;
	const GncSqlColumnTableEntry* subtable_row;
	const gchar* type;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

	for( subtable_row = col_table; subtable_row->col_name != NULL; subtable_row++ ) {
    	buf = g_strdup_printf( "%s_%s", table_row->col_name, subtable_row->col_name );
		info = g_new0( GncSqlColumnInfo, 1 );
		info->name = buf;
		info->type_name = gnc_sql_connection_get_column_type_name( be->conn,
											G_TYPE_STRING, subtable_row->size );
		info->is_primary_key = (table_row->flags & COL_PKEY) ? TRUE : FALSE;
		info->size = subtable_row->size;
		info->null_allowed = (table_row->flags & COL_NNUL) ? FALSE : TRUE;
		*pList = g_list_append( *pList, info );
	}
}

static void
add_address_colname_to_list( const GncSqlColumnTableEntry* table_row, GList** pList )
{
	gnc_sql_add_subtable_colnames_to_list( table_row, col_table, pList );
}

static void
get_gvalue_address( const GncSqlBackend* be, QofIdTypeConst obj_name, const gpointer pObject,
                	const GncSqlColumnTableEntry* table_row, GValue* value )
{
    AddressGetterFunc getter;
    GncAddress* addr;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( value != NULL );

    memset( value, 0, sizeof( GValue ) );

	getter = (AddressGetterFunc)gnc_sql_get_getter( obj_name, table_row );
    addr = (*getter)( pObject );
    g_value_init( value, gnc_address_get_type() );
    g_value_set_object( value, addr );
}

static void
add_gvalue_address_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
          			const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    GValue value;
    GValue* subfield_value;
    GncAddress* addr;
    gchar* s;
    QofAccessFunc getter;
	const GncSqlColumnTableEntry* subtable_row;
	gchar* buf;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    memset( &value, 0, sizeof( GValue ) );
    get_gvalue_address( be, obj_name, pObject, table_row, &value );

    if( G_VALUE_TYPE(&value) != 0 ) {
        addr = g_value_get_object( &value );
		for( subtable_row = col_table; subtable_row->col_name != NULL; subtable_row++ ) {
    		buf = g_strdup_printf( "%s_%s", table_row->col_name, subtable_row->col_name );
			
			subfield_value = g_new0( GValue, 1 );
			if( subtable_row->gobj_param_name != NULL ) {
				g_object_get( addr, subtable_row->gobj_param_name, &s, NULL );
			} else {
    			getter = gnc_sql_get_getter( GNC_ID_ADDRESS, subtable_row );
    			s = (gchar*)(*getter)( addr, NULL );
			}
    		if( s ) {
        		g_value_init( subfield_value, G_TYPE_STRING );
        		g_value_take_string( subfield_value,
									g_strdup_printf( "'%s'", s ) );
			} else {
				g_value_init( subfield_value, G_TYPE_STRING );
				g_value_set_string( subfield_value, "NULL" );
			}
			(*pList) = g_slist_append( (*pList), subfield_value );
			g_free( buf );
		}
    }
}

static col_type_handler_t address_handler
	= { load_address,
		add_address_col_info_to_list,
		add_address_colname_to_list,
		add_gvalue_address_to_slist };

/* ================================================================= */
void
gnc_address_sql_initialize( void )
{
	gnc_sql_register_col_type_handler( CT_ADDRESS, &address_handler );
}
/* ========================== END OF FILE ===================== */
