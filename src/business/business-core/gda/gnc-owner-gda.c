/********************************************************************\
 * gnc-owner-xml-v2.c -- owner xml i/o implementation           *
 *                                                                  *
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>                *
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

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include <libgda/libgda.h>
#include "gnc-backend-util-gda.h"

#include "gnc-owner-gda.h"
#include "gncCustomerP.h"
#include "gncJobP.h"
#include "gncEmployeeP.h"
#include "gncVendorP.h"

static QofLogModule log_module = G_LOG_DOMAIN;

typedef void (*OwnerSetterFunc)( gpointer, GncOwner* );
typedef GncOwner* (*OwnerGetterFunc)( const gpointer );

static void
load_owner( const GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table_row )
{
    const GValue* val;
    gchar* buf;
	GncOwnerType type;
    GUID guid;
	QofBook* book;
	GncOwner owner;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

	book = be->primary_book;
    buf = g_strdup_printf( "%s_type", table_row->col_name );
    val = gda_data_model_get_value_at_col_name( pModel, buf, row );
	type = (GncOwnerType)g_value_get_int( val );
    g_free( buf );
    buf = g_strdup_printf( "%s_guid", table_row->col_name );
    val = gda_data_model_get_value_at_col_name( pModel, buf, row );
    g_free( buf );

    val = gda_data_model_get_value_at_col_name( pModel, table_row->col_name, row );
    if( !gda_value_is_null( val ) ) {
        string_to_guid( g_value_get_string( val ), &guid );
    }

    switch( type ) {
  	case GNC_OWNER_CUSTOMER:
  		{
    		GncCustomer *cust = gncCustomerLookup( book, &guid );
    		if( cust == NULL ) {
      			cust = gncCustomerCreate( book );
      			gncCustomerSetGUID( cust, &guid );
    		}
    		gncOwnerInitCustomer( &owner, cust );
    		break; 
  		}

  	case GNC_OWNER_JOB:
  		{
    		GncJob *job = gncJobLookup( book, &guid );
    		if( job == NULL ) {
      			job = gncJobCreate( book );
      			gncJobSetGUID( job, &guid );
    		}
    		gncOwnerInitJob( &owner, job );
    		break; 
  		}

  	case GNC_OWNER_VENDOR:
  		{
    		GncVendor *vendor = gncVendorLookup( book, &guid );
    		if( vendor == NULL ) {
      			vendor = gncVendorCreate( book );
    		  	gncVendorSetGUID( vendor, &guid );
    		}
    		gncOwnerInitVendor( &owner, vendor );
    		break; 
  		}

  	case GNC_OWNER_EMPLOYEE:
  		{
    		GncEmployee *employee = gncEmployeeLookup( book, &guid );
    		if( employee == NULL ) {
      			employee = gncEmployeeCreate( book );
      			gncEmployeeSetGUID( employee, &guid );
    		}
    		gncOwnerInitEmployee( &owner, employee );
    		break; 
  		}

  	default:
    	PWARN("Invalid owner type: %d\n", type );
	}

	if( table_row->gobj_param_name != NULL ) {
		g_object_set( pObject, table_row->gobj_param_name, &owner, NULL );
	} else {
    	(*setter)( pObject, &owner );
	}
}

static void
get_gvalue_owner( const GncGdaBackend* be, QofIdTypeConst obj_name, const gpointer pObject,
                	const col_cvt_t* table_row, GValue* value )
{
    OwnerGetterFunc getter;
    GncOwner* owner;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( value != NULL );

    memset( value, 0, sizeof( GValue ) );

	getter = (OwnerGetterFunc)gnc_gda_get_getter( obj_name, table_row );
    owner = (*getter)( pObject );
    g_value_init( value, G_TYPE_POINTER );
    g_value_set_object( value, owner );
}

static void
get_gvalue_owner_for_query( const GncGdaBackend* be, QofIdTypeConst obj_name,
                			const gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;
    GValue subfield_value;
    GncOwner* owner;
	gchar* buf;
    const GUID* guid;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
	GncOwnerType type;
	QofInstance* inst = NULL;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( query != NULL );

    memset( &value, 0, sizeof( GValue ) );
    get_gvalue_owner( be, obj_name, pObject, table_row, &value );

    if( G_VALUE_TYPE(&value) != 0 ) {
        owner = g_value_get_object( &value );
    	buf = g_strdup_printf( "%s_type", table_row->col_name );
    	memset( &subfield_value, 0, sizeof( GValue ) );
        g_value_init( &subfield_value, G_TYPE_INT );
        type = gncOwnerGetType( owner );
        g_value_set_int( &subfield_value, type );
    	gnc_gda_add_field_to_query( query, buf, &subfield_value );
		g_free( buf );

    	buf = g_strdup_printf( "%s_guid", table_row->col_name );
    	memset( &subfield_value, 0, sizeof( GValue ) );
    	switch( type ) {
  		case GNC_OWNER_CUSTOMER:
    		inst = QOF_INSTANCE(gncOwnerGetCustomer( owner ));
    		break; 

  		case GNC_OWNER_JOB:
    		inst = QOF_INSTANCE(gncOwnerGetJob( owner ));
    		break; 

  		case GNC_OWNER_VENDOR:
    		inst = QOF_INSTANCE(gncOwnerGetVendor( owner ));
    		break; 

  		case GNC_OWNER_EMPLOYEE:
    		inst = QOF_INSTANCE(gncOwnerGetEmployee( owner ));
    		break; 

  		default:
    		PWARN("Invalid owner type: %d\n", type );
		}
		if( inst != NULL ) {
			guid = qof_instance_get_guid( inst );
    		if( guid != NULL ) {
        		(void)guid_to_string_buff( guid, guid_buf );
        		g_value_init( &subfield_value, G_TYPE_STRING );
        		g_value_set_string( &subfield_value, guid_buf );
    			gnc_gda_add_field_to_query( query, buf, &subfield_value );
    		}
		}
		g_free( buf );
    }
}

static GdaQueryCondition*
get_gvalue_owner_cond( const GncGdaBackend* be, QofIdTypeConst obj_name,
                const gpointer pObject, const col_cvt_t* table_row, GdaQuery* query )
{
    GValue value;
    GValue subfield_value;
    GncOwner* owner;
	gchar* buf;
    const GUID* guid;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
	GncOwnerType type;
	QofInstance* inst = NULL;
    GdaQueryCondition* sub_cond;
    GdaQueryCondition* cond;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( obj_name != NULL, NULL );
	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( table_row != NULL, NULL );
	g_return_val_if_fail( query != NULL, NULL );

    memset( &value, 0, sizeof( GValue ) );
    get_gvalue_owner( be, obj_name, pObject, table_row, &value );
    cond = gda_query_condition_new( query, GDA_QUERY_CONDITION_NODE_AND );

    if( G_VALUE_TYPE(&value) != 0 ) {
        owner = g_value_get_object( &value );
    	buf = g_strdup_printf( "%s_type", table_row->col_name );
    	memset( &subfield_value, 0, sizeof( GValue ) );
        g_value_init( &subfield_value, G_TYPE_INT );
        type = gncOwnerGetType( owner );
        g_value_set_int( &subfield_value, type );
    	sub_cond = gnc_gda_create_condition_from_field( query, buf, &subfield_value );
    	gda_query_condition_node_add_child( cond, sub_cond, NULL );
		g_free( buf );

    	buf = g_strdup_printf( "%s_guid", table_row->col_name );
    	memset( &subfield_value, 0, sizeof( GValue ) );
    	switch( type ) {
  		case GNC_OWNER_CUSTOMER:
    		inst = QOF_INSTANCE(gncOwnerGetCustomer( owner ));
    		break; 

  		case GNC_OWNER_JOB:
    		inst = QOF_INSTANCE(gncOwnerGetJob( owner ));
    		break; 

  		case GNC_OWNER_VENDOR:
    		inst = QOF_INSTANCE(gncOwnerGetVendor( owner ));
    		break; 

  		case GNC_OWNER_EMPLOYEE:
    		inst = QOF_INSTANCE(gncOwnerGetEmployee( owner ));
    		break; 

  		default:
    		PWARN("Invalid owner type: %d\n", type );
		}
		if( inst != NULL ) {
			guid = qof_instance_get_guid( inst );
    		if( guid != NULL ) {
        		(void)guid_to_string_buff( guid, guid_buf );
        		g_value_init( &subfield_value, G_TYPE_STRING );
        		g_value_set_string( &subfield_value, guid_buf );
    			sub_cond = gnc_gda_create_condition_from_field( query, buf, &subfield_value );
		    	gda_query_condition_node_add_child( cond, sub_cond, NULL );
    		}
		}
		g_free( buf );
    }

    return cond;
}

static void
create_owner_col( GdaServerProvider* server, GdaConnection* cnn,
            xmlNodePtr array_data, const col_cvt_t* table_row )
{
    const gchar* dbms_type;
    gchar* buf;
	const col_cvt_t* subtable_row;

	g_return_if_fail( server != NULL );
	g_return_if_fail( cnn != NULL );
	g_return_if_fail( array_data != NULL );
	g_return_if_fail( table_row != NULL );

    dbms_type = gda_server_provider_get_default_dbms_type( server, cnn, G_TYPE_INT );
    buf = g_strdup_printf( "%s_type", table_row->col_name );
   	gnc_gda_add_table_column( server, cnn, array_data, buf, dbms_type, table_row->size, table_row->flags );
   	g_free( buf );
    dbms_type = gda_server_provider_get_default_dbms_type( server, cnn,
                                                            G_TYPE_STRING );
    buf = g_strdup_printf( "%s_guid", table_row->col_name );
   	gnc_gda_add_table_column( server, cnn, array_data, buf, dbms_type, GUID_ENCODING_LENGTH, table_row->flags );
   	g_free( buf );
}

static col_type_handler_t owner_handler =
        { load_owner, create_owner_col,
            get_gvalue_owner_for_query, get_gvalue_owner_cond };

/* ================================================================= */
void
gnc_owner_gda_initialize( void )
{
	gnc_gda_register_col_type_handler( CT_OWNERREF, &owner_handler );
}
/* ========================== END OF FILE ===================== */
