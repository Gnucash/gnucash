/********************************************************************\
 * gnc-order-sql.c -- order sql backend                             *
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

/** @file gnc-order-sql.c
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
#include "gnc-slots-sql.h"

#include "gncOrderP.h"

#include "gnc-order-sql.h"
#include "gnc-owner-sql.h"

#define _GNC_MOD_NAME	GNC_ID_ORDER

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "orders"
#define TABLE_VERSION 1

#define MAX_ID_LEN 2048
#define MAX_NOTES_LEN 2048
#define MAX_REFERENCE_LEN 2048

static GncSqlColumnTableEntry col_table[] =
{
	{ "guid",        CT_GUID,     0,                 COL_NNUL|COL_PKEY, "guid" },
	{ "id",          CT_STRING,   MAX_ID_LEN,        COL_NNUL,          NULL, ORDER_ID },
	{ "notes",       CT_STRING,   MAX_NOTES_LEN,     COL_NNUL,          "notes" },
	{ "reference",   CT_STRING,   MAX_REFERENCE_LEN, COL_NNUL,          NULL, ORDER_REFERENCE },
	{ "active",      CT_BOOLEAN,  0,                 COL_NNUL,          NULL, QOF_PARAM_ACTIVE },
	{ "date_opened", CT_TIMESPEC, 0,                 COL_NNUL,          NULL, ORDER_OPENED },
	{ "date_closed", CT_TIMESPEC, 0,                 COL_NNUL,          NULL, ORDER_CLOSED },
	{ "owner",       CT_OWNERREF, 0,                 COL_NNUL,          NULL, ORDER_OWNER },
	{ NULL },
};

static GncOrder*
load_single_order( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
	GncOrder* pOrder;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

    guid = gnc_sql_load_guid( be, row );
    pOrder = gncOrderLookup( be->primary_book, guid );
    if( pOrder == NULL ) {
        pOrder = gncOrderCreate( be->primary_book );
    }
    gnc_sql_load_object( be, row, GNC_ID_ORDER, pOrder, col_table );
    qof_instance_mark_clean( QOF_INSTANCE(pOrder) );

	return pOrder;
}

static void
load_all_orders( GncSqlBackend* be )
{
    GncSqlStatement* stmt;
    GncSqlResult* result;
    QofBook* pBook;

	g_return_if_fail( be != NULL );

    pBook = be->primary_book;

    stmt = gnc_sql_create_select_statement( be, TABLE_NAME );
    result = gnc_sql_execute_select_statement( be, stmt );
	gnc_sql_statement_dispose( stmt );
    if( result != NULL ) {
        GncSqlRow* row;
		GList* list = NULL;

		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
            GncOrder* pOrder = load_single_order( be, row );
			if( pOrder != NULL ) {
				list = g_list_append( list, pOrder );
			}
			row = gnc_sql_result_get_next_row( result );
		}
		gnc_sql_result_dispose( result );

		if( list != NULL ) {
			gnc_sql_slots_load_for_list( be, list );
		}
    }
}

/* ================================================================= */
static void
create_order_tables( GncSqlBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_sql_get_table_version( be, TABLE_NAME );
    if( version == 0 ) {
        gnc_sql_create_table( be, TABLE_NAME, TABLE_VERSION, col_table );
    }
}

/* ================================================================= */
static gboolean
save_order( GncSqlBackend* be, QofInstance* inst )
{
	g_return_val_if_fail( inst != NULL, FALSE );
	g_return_val_if_fail( GNC_IS_ORDER(inst), FALSE );
	g_return_val_if_fail( be != NULL, FALSE );

    return gnc_sql_commit_standard_item( be, inst, TABLE_NAME, GNC_ID_ORDER, col_table );
}

/* ================================================================= */
static gboolean
order_should_be_saved( GncOrder *order )
{
    const char *id;

	g_return_val_if_fail( order != NULL, FALSE );

    /* make sure this is a valid order before we save it -- should have an ID */
    id = gncOrderGetID( order );
    if( id == NULL || *id == '\0' ) {
        return FALSE;
	}

    return TRUE;
}

static void
write_single_order( QofInstance *term_p, gpointer data_p )
{
	write_objects_t* s = (write_objects_t*)data_p;

	g_return_if_fail( term_p != NULL );
	g_return_if_fail( GNC_IS_ORDER(term_p) );
	g_return_if_fail( data_p != NULL );

	if( s->is_ok && order_should_be_saved( GNC_ORDER(term_p) ) ) {
    	s->is_ok = save_order( s->be, term_p );
	}
}

static gboolean
write_orders( GncSqlBackend* be )
{
	write_objects_t data;

	g_return_val_if_fail( be != NULL, FALSE );

	data.be = be;
	data.is_ok = TRUE;
    qof_object_foreach( GNC_ID_ORDER, be->primary_book, write_single_order, &data );

	return data.is_ok;
}

/* ================================================================= */
static void
load_order_guid( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    GUID guid;
	GncOrder* order = NULL;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if( val != NULL && G_VALUE_HOLDS_STRING( val ) && g_value_get_string( val ) != NULL ) {
        string_to_guid( g_value_get_string( val ), &guid );
		order = gncOrderLookup( be->primary_book, &guid );
		if( order != NULL ) {
        	if( table_row->gobj_param_name != NULL ) {
		    	g_object_set( pObject, table_row->gobj_param_name, order, NULL );
        	} else {
		    	(*setter)( pObject, (const gpointer)order );
        	}
		} else {
	    	PWARN( "Order ref '%s' not found", g_value_get_string( val ) );
		}
	}
}

static GncSqlColumnTypeHandler order_guid_handler
	= { load_order_guid,
		gnc_sql_add_objectref_guid_col_info_to_list,
		gnc_sql_add_colname_to_list,
		gnc_sql_add_gvalue_objectref_guid_to_slist };
/* ================================================================= */
void
gnc_order_sql_initialize( void )
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_ORDER,
        save_order,						/* commit */
        load_all_orders,				/* initial_load */
        create_order_tables,			/* create_tables */
		NULL, NULL, NULL,
		write_orders					/* write */
    };

    qof_object_register_backend( GNC_ID_ORDER, GNC_SQL_BACKEND, &be_data );

	gnc_sql_register_col_type_handler( CT_ORDERREF, &order_guid_handler );
}
/* ========================== END OF FILE ===================== */
