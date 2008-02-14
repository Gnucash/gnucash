/********************************************************************\
 * gnc-address-gda.c -- address gda backend implementation          *
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

/** @file gnc-address-gda.c
 *  @brief load and save address data to SQL via libgda
 *  @author Copyright (c) 2007 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database via libgda
 */

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include <libgda/libgda.h>

#include "gnc-engine.h"
#include "gnc-address-gda.h"

#include "gnc-backend-util-gda.h"

static QofLogModule log_module = G_LOG_DOMAIN;

#define ADDRESS_MAX_NAME_LEN 50
#define ADDRESS_MAX_ADDRESS_LINE_LEN 50
#define ADDRESS_MAX_PHONE_LEN 40
#define ADDRESS_MAX_FAX_LEN 40
#define ADDRESS_MAX_EMAIL_LEN 40

static col_cvt_t col_table[] =
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
load_address( const GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table_row )
{
    const GValue* val;
    gchar* buf;
    GncAddress* addr;
	AddressSetterFunc a_setter = (AddressSetterFunc)setter;
	const col_cvt_t* subtable;
	const gchar* s;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    addr = gncAddressCreate( be->primary_book, NULL );
	for( subtable = col_table; subtable->col_name != NULL; subtable++ ) {
    	buf = g_strdup_printf( "%s_%s", table_row->col_name, subtable->col_name );
    	val = gda_data_model_get_value_at_col_name( pModel, buf, row );
    	g_free( buf );
    	if( gda_value_is_null( val ) ) {
        	s = NULL;
    	} else {
        	s = g_value_get_string( val );
    	}
    	if( subtable->gobj_param_name != NULL ) {
			g_object_set( addr, subtable->gobj_param_name, s, NULL );
    	} else {
	        if( subtable->param_name != NULL ) {
            	setter = qof_class_get_parameter_setter( GNC_ID_ADDRESS, subtable->param_name );
        	} else {
            	setter = subtable->setter;
        	}
			(*setter)( addr, (const gpointer)s );
    	}
    }
    (*a_setter)( pObject, addr );
}

static void
get_gvalue_address( const GncGdaBackend* be, QofIdTypeConst obj_name, const gpointer pObject,
                	const col_cvt_t* table_row, GValue* value )
{
    AddressGetterFunc getter;
    GncAddress* addr;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( value != NULL );

    memset( value, 0, sizeof( GValue ) );

	getter = (AddressGetterFunc)gnc_gda_get_getter( obj_name, table_row );
    addr = (*getter)( pObject );
    g_value_init( value, gnc_address_get_type() );
    g_value_set_object( value, addr );
}

static void
get_gvalue_address_for_query( const GncGdaBackend* be, QofIdTypeConst obj_name,
                			const gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;
    GValue subfield_value;
    GncAddress* addr;
    gchar* s;
    QofAccessFunc getter;
	const col_cvt_t* subtable_row;
	gchar* buf;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( query != NULL );

    memset( &value, 0, sizeof( GValue ) );
    get_gvalue_address( be, obj_name, pObject, table_row, &value );

    if( G_VALUE_TYPE(&value) != 0 ) {
        addr = g_value_get_object( &value );
		for( subtable_row = col_table; subtable_row->col_name != NULL; subtable_row++ ) {
    		buf = g_strdup_printf( "%s_%s", table_row->col_name, subtable_row->col_name );
			
    		memset( &subfield_value, 0, sizeof( GValue ) );
			if( subtable_row->gobj_param_name != NULL ) {
				g_object_get( addr, subtable_row->gobj_param_name, &s, NULL );
			} else {
    			getter = gnc_gda_get_getter( GNC_ID_ADDRESS, subtable_row );
    			s = (gchar*)(*getter)( addr, NULL );
			}
    		if( s ) {
        		g_value_init( &subfield_value, G_TYPE_STRING );
        		g_value_set_string( &subfield_value, s );
			}
    		gnc_gda_add_field_to_query( query, buf, &subfield_value );
			g_free( buf );
		}
    }
}

static GdaQueryCondition*
get_gvalue_address_cond( const GncGdaBackend* be, QofIdTypeConst obj_name,
                const gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;
	GValue subfield_value;
    GncAddress* addr;
    gchar* buf;
    GdaQueryCondition* sub_cond;
    GdaQueryCondition* cond;
	const col_cvt_t* subtable_row;
	gchar* s;
	QofAccessFunc getter;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( obj_name != NULL, NULL );
	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( table_row != NULL, NULL );
	g_return_val_if_fail( query != NULL, NULL );

    memset( &value, 0, sizeof( GValue ) );
    get_gvalue_address( be, obj_name, pObject, table_row, &value );

    cond = gda_query_condition_new( query, GDA_QUERY_CONDITION_NODE_AND );
    if( G_VALUE_TYPE(&value) != 0 ) {
        addr = g_value_get_object( &value );
		for( subtable_row = col_table; subtable_row->col_name != NULL; subtable_row++ ) {
    		buf = g_strdup_printf( "%s_%s", table_row->col_name, subtable_row->col_name );
			
    		memset( &subfield_value, 0, sizeof( GValue ) );
			if( subtable_row->gobj_param_name != NULL ) {
				g_object_get( addr, subtable_row->gobj_param_name, &s, NULL );
			} else {
    			getter = gnc_gda_get_getter( GNC_ID_ADDRESS, subtable_row );
    			s = (gchar*)(*getter)( addr, NULL );
			}
    		if( s ) {
        		g_value_init( &subfield_value, G_TYPE_STRING );
        		g_value_set_string( &subfield_value, s );
			}
    		sub_cond = gnc_gda_create_condition_from_field( query, buf, &subfield_value );
    		gda_query_condition_node_add_child( cond, sub_cond, NULL );
			g_free( buf );
		}
    }

    return cond;
}

static void
create_address_col( GdaServerProvider* server, GdaConnection* cnn,
            xmlNodePtr array_data, const col_cvt_t* table_row, gboolean pkey )
{
    const gchar* dbms_type;
    gchar* buf;
	const col_cvt_t* subtable_row;

	g_return_if_fail( server != NULL );
	g_return_if_fail( cnn != NULL );
	g_return_if_fail( array_data != NULL );
	g_return_if_fail( table_row != NULL );

    dbms_type = gda_server_provider_get_default_dbms_type( server, cnn,
                                                            G_TYPE_STRING );
	for( subtable_row = col_table; subtable_row->col_name != NULL; subtable_row++ ) {
    	buf = g_strdup_printf( "%s_%s", table_row->col_name, subtable_row->col_name );
    	gnc_gda_add_table_column( server, cnn, array_data, buf, dbms_type, subtable_row->size, subtable_row->flags );
    	g_free( buf );
    }
}

static col_type_handler_t address_handler =
        { load_address, create_address_col,
            get_gvalue_address_for_query, get_gvalue_address_cond };

/* ================================================================= */
void
gnc_address_gda_initialize( void )
{
	gnc_gda_register_col_type_handler( CT_ADDRESS, &address_handler );
}
/* ========================== END OF FILE ===================== */
