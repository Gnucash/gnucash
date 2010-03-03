/********************************************************************
 * gnc-lots-sql.c: load and save data to SQL                        *
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
/** @file gnc-lots-sql.c
 *  @brief load and save data to SQL 
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */

#include "config.h"

#include <glib.h>

#include "qof.h"
#include "Account.h"
#include "gnc-lot.h"

#include "gnc-backend-sql.h"
#include "gnc-slots-sql.h"

#include "gnc-lots-sql.h"

#if defined( S_SPLINT_S )
#include "splint-defs.h"
#endif

/*@ unused @*/ static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "lots"
#define TABLE_VERSION 2

static /*@ dependent @*//*@ null @*/ gpointer get_lot_account( gpointer pObject );
static void set_lot_account( gpointer pObject, /*@ null @*/ gpointer pValue );

static const GncSqlColumnTableEntry col_table[] =
{
	/*@ -full_init_block @*/
    { "guid",         CT_GUID,       0, COL_NNUL|COL_PKEY, "guid" },
    { "account_guid", CT_ACCOUNTREF, 0, 0,                 NULL, NULL,
		(QofAccessFunc)get_lot_account,   set_lot_account },
    { "is_closed",    CT_BOOLEAN,    0, COL_NNUL,          "is-closed" },
    { NULL }
	/*@ +full_init_block @*/
};

/* ================================================================= */
static /*@ dependent @*//*@ null @*/ gpointer
get_lot_account( gpointer pObject )
{
    const GNCLot* lot;
    Account* pAccount;

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( GNC_IS_LOT(pObject), NULL );

    lot = GNC_LOT(pObject);
    pAccount = gnc_lot_get_account( lot );
    return pAccount;
}

static void 
set_lot_account( gpointer pObject, /*@ null @*/ gpointer pValue )
{
    GNCLot* lot;
    Account* pAccount;

	g_return_if_fail( pObject != NULL && GNC_IS_LOT(pObject) );
	g_return_if_fail( pValue == NULL || GNC_IS_ACCOUNT(pValue) );

    lot = GNC_LOT(pObject);
    pAccount = GNC_ACCOUNT(pValue);
	if( pAccount != NULL ) {
    	xaccAccountInsertLot( pAccount, lot );
	}
}

static /*@ dependent @*//*@ null @*/ GNCLot*
load_single_lot( GncSqlBackend* be, GncSqlRow* row )
{
	GNCLot* lot;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

    lot = gnc_lot_new( be->primary_book );

	gnc_lot_begin_edit( lot );
    gnc_sql_load_object( be, row, GNC_ID_LOT, lot, col_table );
	gnc_lot_commit_edit( lot );

	return lot;
}

static void
load_all_lots( GncSqlBackend* be )
{
    GncSqlStatement* stmt;
    GncSqlResult* result;

	g_return_if_fail( be != NULL );

    stmt = gnc_sql_create_select_statement( be, TABLE_NAME );
	if( stmt != NULL ) {
    	result = gnc_sql_execute_select_statement( be, stmt );
		gnc_sql_statement_dispose( stmt );
    	if( result != NULL ) {
        	GncSqlRow* row = gnc_sql_result_get_first_row( result );
			GNCLot* lot;
			gchar* sql;

        	while( row != NULL ) {
            	lot = load_single_lot( be, row );
				row = gnc_sql_result_get_next_row( result );
        	}
			gnc_sql_result_dispose( result );

			sql = g_strdup_printf( "SELECT DISTINCT guid FROM %s", TABLE_NAME );
			gnc_sql_slots_load_for_sql_subquery( be, sql, (BookLookupFn)gnc_lot_lookup );
			g_free( sql );
    	}
	}
}

/* ================================================================= */
static void
create_lots_tables( GncSqlBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_sql_get_table_version( be, TABLE_NAME );
    if( version == 0 ) {
		/* The table doesn't exist, so create it */
        (void)gnc_sql_create_table( be, TABLE_NAME, TABLE_VERSION, col_table );
	} else if( version == 1 ) {
		/* Version 1 -> 2 removes the 'NOT NULL' constraint on the account_guid
		field. 

		Create a temporary table, copy the data from the old table, delete the
		old table, then rename the new one. */

		gnc_sql_upgrade_table( be, TABLE_NAME, col_table );
		(void)gnc_sql_set_table_version( be, TABLE_NAME, TABLE_VERSION );
    }
}

/* ================================================================= */

static gboolean
commit_lot( GncSqlBackend* be, QofInstance* inst )
{
	g_return_val_if_fail( be != NULL, FALSE );
	g_return_val_if_fail( inst != NULL, FALSE );
	g_return_val_if_fail( GNC_IS_LOT(inst), FALSE );

    return gnc_sql_commit_standard_item( be, inst, TABLE_NAME, GNC_ID_LOT, col_table );
}

static void
do_save_lot( QofInstance* inst, gpointer data )
{
	write_objects_t* s = (write_objects_t*)data;

	if( s->is_ok ) {
		s->is_ok = commit_lot( s->be, inst );
	}
}

static gboolean
write_lots( GncSqlBackend* be )
{
	write_objects_t data;

	g_return_val_if_fail( be != NULL, FALSE );

	data.be = be;
	data.is_ok = TRUE;
    qof_collection_foreach( qof_book_get_collection( be->primary_book, GNC_ID_LOT ),
                            (QofInstanceForeachCB)do_save_lot, &data );
	return data.is_ok;
}

/* ================================================================= */
static void
load_lot_guid( const GncSqlBackend* be, GncSqlRow* row,
            /*@ null @*/ QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    GUID guid;
	GNCLot* lot;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if( val != NULL && G_VALUE_HOLDS_STRING( val ) && g_value_get_string( val ) != NULL ) {
    	(void)string_to_guid( g_value_get_string( val ), &guid );
		lot = gnc_lot_lookup( &guid, be->primary_book );
		if( lot != NULL ) {
        	if( table_row->gobj_param_name != NULL ) {
		    	g_object_set( pObject, table_row->gobj_param_name, lot, NULL );
        	} else {
		    	g_return_if_fail( setter != NULL );
		    	(*setter)( pObject, (const gpointer)lot );
        	}
		} else {
	    	PWARN( "Lot ref '%s' not found", g_value_get_string( val ) );
		}
	}
}

static GncSqlColumnTypeHandler lot_guid_handler
	= { load_lot_guid,
		gnc_sql_add_objectref_guid_col_info_to_list,
		gnc_sql_add_colname_to_list,
        gnc_sql_add_gvalue_objectref_guid_to_slist };
/* ================================================================= */
void
gnc_sql_init_lot_handler( void )
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_LOT,
        commit_lot,            /* commit */
        load_all_lots,         /* initial_load */
        create_lots_tables,    /* create tables */
		NULL, NULL, NULL,
		write_lots             /* save all */
    };

    (void)qof_object_register_backend( GNC_ID_LOT, GNC_SQL_BACKEND, &be_data );

	gnc_sql_register_col_type_handler( CT_LOTREF, &lot_guid_handler );
}

/* ========================== END OF FILE ===================== */
