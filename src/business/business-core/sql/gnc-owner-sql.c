/********************************************************************\
 * gnc-owner-sql.c -- owner sql implementation                      *
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

/** @file gnc-owner-sql.c
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

#include "gnc-backend-sql.h"

#include "gnc-owner-sql.h"
#include "gncCustomerP.h"
#include "gncJobP.h"
#include "gncEmployeeP.h"
#include "gncVendorP.h"

static QofLogModule log_module = G_LOG_DOMAIN;

typedef void (*OwnerSetterFunc)( gpointer, GncOwner* );
typedef GncOwner* (*OwnerGetterFunc)( const gpointer );

static void
load_owner( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    gchar* buf;
	GncOwnerType type;
    GUID guid;
	QofBook* book;
	GncOwner owner;
	GUID* pGuid;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

	book = be->primary_book;
    buf = g_strdup_printf( "%s_type", table_row->col_name );
    val = gnc_sql_row_get_value_at_col_name( row, buf );
	type = (GncOwnerType)g_value_get_int( val );
    g_free( buf );
    buf = g_strdup_printf( "%s_guid", table_row->col_name );
    val = gnc_sql_row_get_value_at_col_name( row, buf );
    g_free( buf );

    if( val != NULL ) {
        string_to_guid( g_value_get_string( val ), &guid );
    }
	pGuid = &guid;

    switch( type ) {
  	case GNC_OWNER_CUSTOMER:
  		{
    		GncCustomer *cust = gncCustomerLookup( book, pGuid );
    		if( cust == NULL ) {
      			cust = gncCustomerCreate( book );
      			gncCustomerSetGUID( cust, &guid );
    		}
    		gncOwnerInitCustomer( &owner, cust );
    		break; 
  		}

  	case GNC_OWNER_JOB:
  		{
    		GncJob *job = gncJobLookup( book, pGuid );
    		if( job == NULL ) {
      			job = gncJobCreate( book );
      			gncJobSetGUID( job, &guid );
    		}
    		gncOwnerInitJob( &owner, job );
    		break; 
  		}

  	case GNC_OWNER_VENDOR:
  		{
    		GncVendor *vendor = gncVendorLookup( book, pGuid );
    		if( vendor == NULL ) {
      			vendor = gncVendorCreate( book );
    		  	gncVendorSetGUID( vendor, &guid );
    		}
    		gncOwnerInitVendor( &owner, vendor );
    		break; 
  		}

  	case GNC_OWNER_EMPLOYEE:
  		{
    		GncEmployee *employee = gncEmployeeLookup( book, pGuid );
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
add_owner_col_info_to_list( const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,
								GList** pList )
{
	GncSqlColumnInfo* info;
    gchar* buf;
	const gchar* type;

	g_return_if_fail( be != NULL );
	g_return_if_fail( table_row != NULL );
	g_return_if_fail( pList != NULL );

    buf = g_strdup_printf( "%s_type", table_row->col_name );
	info = g_new0( GncSqlColumnInfo, 1 );
	info->name = buf;
	info->type_name = gnc_sql_connection_get_column_type_name( be->conn,
											G_TYPE_INT, table_row->size );
	info->is_primary_key = (table_row->flags & COL_PKEY) ? TRUE : FALSE;
	info->null_allowed = (table_row->flags & COL_NNUL) ? FALSE : TRUE;
	*pList = g_list_append( *pList, info );

   	buf = g_strdup_printf( "%s_guid", table_row->col_name );
	info = g_new0( GncSqlColumnInfo, 1 );
	info->name = buf;
	info->type_name = gnc_sql_connection_get_column_type_name( be->conn,
										G_TYPE_STRING, GUID_ENCODING_LENGTH );
	info->size = GUID_ENCODING_LENGTH;
	info->is_primary_key = (table_row->flags & COL_PKEY) ? TRUE : FALSE;
	info->null_allowed = (table_row->flags & COL_NNUL) ? FALSE : TRUE;
	*pList = g_list_append( *pList, info );
}

static void
add_colname_to_list( const GncSqlColumnTableEntry* table_row, GList** pList )
{
    gchar* buf;

    buf = g_strdup_printf( "%s_type", table_row->col_name );
	(*pList) = g_list_append( (*pList), buf );
    buf = g_strdup_printf( "%s_guid", table_row->col_name );
	(*pList) = g_list_append( (*pList), buf );
}

static void
add_gvalue_owner_to_slist( const GncSqlBackend* be, QofIdTypeConst obj_name,
                	const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList )
{
    GValue* subfield_value;
    GncOwner* owner;
	gchar* buf;
    const GUID* guid;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
	GncOwnerType type;
	QofInstance* inst = NULL;
    OwnerGetterFunc getter;

	g_return_if_fail( be != NULL );
	g_return_if_fail( obj_name != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

	getter = (OwnerGetterFunc)gnc_sql_get_getter( obj_name, table_row );
    owner = (*getter)( pObject );

	if( owner != NULL ) {
    	buf = g_strdup_printf( "%s_type", table_row->col_name );
		subfield_value = g_new0( GValue, 1 );
        g_value_init( subfield_value, G_TYPE_INT );
        type = gncOwnerGetType( owner );
        g_value_set_int( subfield_value, type );
		(*pList) = g_slist_append( (*pList), subfield_value );
		g_free( buf );

    	buf = g_strdup_printf( "%s_guid", table_row->col_name );
		subfield_value = g_new0( GValue, 1 );
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
        g_value_init( subfield_value, G_TYPE_STRING );
		if( inst != NULL ) {
			guid = qof_instance_get_guid( inst );
    		if( guid != NULL ) {
        		(void)guid_to_string_buff( guid, guid_buf );
        		g_value_take_string( subfield_value, g_strdup_printf( "%s", guid_buf ) );
    		} else {
				g_value_set_string( subfield_value, "NULL" );
			}
    	} else {
			g_value_set_string( subfield_value, "NULL" );
		}
		(*pList) = g_slist_append( (*pList), subfield_value );
		g_free( buf );
    } else {
		subfield_value = g_new0( GValue, 1 );
		g_value_init( subfield_value, G_TYPE_STRING );
		g_value_set_string( subfield_value, "NULL" );
		(*pList) = g_slist_append( (*pList), subfield_value );
		subfield_value = g_new0( GValue, 1 );
		g_value_init( subfield_value, G_TYPE_STRING );
		g_value_set_string( subfield_value, "NULL" );
		(*pList) = g_slist_append( (*pList), subfield_value );
	}
}

static col_type_handler_t owner_handler
	= { load_owner,
		add_owner_col_info_to_list,
		add_colname_to_list,
        add_gvalue_owner_to_slist };

/* ================================================================= */
void
gnc_owner_sql_initialize( void )
{
	gnc_sql_register_col_type_handler( CT_OWNERREF, &owner_handler );
}
/* ========================== END OF FILE ===================== */
