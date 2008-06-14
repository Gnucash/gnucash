/********************************************************************\
 * gnc-tax-table-sql.c -- tax table sql implementation              *
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

/** @file gnc-tax-table-sql.c
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

#include "gncEntry.h"
#include "gncTaxTableP.h"

#include "gnc-tax-table-sql.h"

#define _GNC_MOD_NAME	GNC_ID_TAXTABLE

static QofLogModule log_module = G_LOG_DOMAIN;

static void set_invisible( gpointer data, gboolean value );

typedef struct {
    GncSqlBackend* be;
    const GUID* guid;
} guid_info_t;

static gpointer get_obj_guid( gpointer pObject, const QofParam* param );
static void set_obj_guid( gpointer pObject, gpointer pValue );
static gpointer get_child( gpointer pObject, const QofParam* param );
static void set_parent( gpointer pObject, gpointer pValue );

#define MAX_NAME_LEN 50

#define TT_TABLE_NAME "taxtables"
#define TT_TABLE_VERSION 1

static col_cvt_t tt_col_table[] =
{
	{ "guid",      CT_GUID,        0,            COL_NNUL|COL_PKEY, "guid" },
	{ "name",      CT_STRING,      MAX_NAME_LEN, COL_NNUL,          NULL, GNC_TT_NAME },
	{ "refcount",  CT_INT64,       0,            COL_NNUL,          NULL, GNC_TT_REFCOUNT },
	{ "invisible", CT_BOOLEAN,     0,            COL_NNUL,          NULL, NULL,
			(QofAccessFunc)gncTaxTableGetInvisible, (QofSetterFunc)set_invisible },
	{ "child",     CT_TAXTABLEREF, 0,			 0,                 NULL, NULL,
			get_child, (QofSetterFunc)gncTaxTableSetChild },
	{ "parent",    CT_TAXTABLEREF, 0,			 0,                 NULL, NULL,
			(QofAccessFunc)gncTaxTableGetParent, set_parent },
	{ NULL }
};

#define TTENTRIES_TABLE_NAME "taxtable_entries"
#define TTENTRIES_TABLE_VERSION 1

static col_cvt_t ttentries_col_table[] =
{
	{ "taxtable", CT_TAXTABLEREF, 0, COL_NNUL, NULL, NULL,
			(QofAccessFunc)gncTaxTableEntryGetTable, set_obj_guid },
	{ "account",  CT_ACCOUNTREF,  0, COL_NNUL, NULL, NULL,
			(QofAccessFunc)gncTaxTableEntryGetAccount, (QofSetterFunc)gncTaxTableEntrySetAccount },
	{ "amount",   CT_NUMERIC,     0, COL_NNUL, NULL, NULL,
			(QofAccessFunc)gncTaxTableEntryGetAmount, (QofSetterFunc)gncTaxTableEntrySetAmount },
	{ "type",     CT_INT,         0, COL_NNUL, NULL, NULL,
			(QofAccessFunc)gncTaxTableEntryGetType, (QofSetterFunc)gncTaxTableEntrySetType },
	{ NULL }
};

/* Special column table because we need to be able to access the table by
a column other than the primary key */
static col_cvt_t guid_col_table[] =
{
    { "taxtable", CT_GUID, 0, 0, NULL, NULL, get_obj_guid, set_obj_guid },
    { NULL }
};

static gpointer
get_obj_guid( gpointer pObject, const QofParam* param )
{
    guid_info_t* pInfo = (guid_info_t*)pObject;

	g_return_val_if_fail( pInfo != NULL, NULL );

    return (gpointer)pInfo->guid;
}

static void
set_obj_guid( gpointer pObject, gpointer pValue )
{
    // Nowhere to put the GUID
}

static void
set_invisible( gpointer data, gboolean value )
{
	GncTaxTable* tt = GNC_TAXTABLE(data);

	g_return_if_fail( data != NULL );
	g_return_if_fail( GNC_IS_TAXTABLE(data) );

	if( value ) {
		gncTaxTableMakeInvisible( tt );
	}
}

static gpointer
get_child( gpointer pObject, const QofParam* param )
{
	GncTaxTable* tt = GNC_TAXTABLE(pObject);

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( GNC_IS_TAXTABLE(pObject), NULL );

	return gncTaxTableGetChild( tt );
}

static void
set_parent( gpointer data, gpointer value )
{
	GncTaxTable* tt = GNC_TAXTABLE(data);
	GncTaxTable* parent;

	g_return_if_fail( data != NULL );
	g_return_if_fail( GNC_IS_TAXTABLE(data) );

	if( value != NULL ) {
		parent = GNC_TAXTABLE(value);
		gncTaxTableSetParent( tt, parent );
	}
}

static void
load_single_ttentry( GncSqlBackend* be, GncSqlRow* row, GncTaxTable* tt )
{
	GncTaxTableEntry* e = gncTaxTableEntryCreate();

	g_return_if_fail( be != NULL );
	g_return_if_fail( row!= NULL );
	g_return_if_fail( tt != NULL );

    gnc_sql_load_object( be, row, GNC_ID_TAXTABLE, e, ttentries_col_table );
	gncTaxTableAddEntry( tt, e );
}

static void
load_taxtable_entries( GncSqlBackend* be, GncTaxTable* tt )
{
    GncSqlResult* result;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
    GValue value;
	gchar* buf;
	GncSqlStatement* stmt;
	GError* error = NULL;

	g_return_if_fail( be != NULL );
	g_return_if_fail( tt != NULL );

    guid_to_string_buff( qof_instance_get_guid( QOF_INSTANCE(tt) ), guid_buf );
    memset( &value, 0, sizeof( GValue ) );
    g_value_init( &value, G_TYPE_STRING );
    g_value_set_string( &value, guid_buf );
	buf = g_strdup_printf( "SELECT * FROM %s WHERE taxtable='%s'", TTENTRIES_TABLE_NAME, guid_buf );
	stmt = gnc_sql_connection_create_statement_from_sql( be->conn, buf );
    result = gnc_sql_execute_select_statement( be, stmt );
	gnc_sql_statement_dispose( stmt );
    if( result != NULL ) {
        GncSqlRow* row;

		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
            load_single_ttentry( be, row, tt );
			row = gnc_sql_result_get_next_row( result );
        }
		gnc_sql_result_dispose( result );
    }
}

static void
load_single_taxtable( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
	GncTaxTable* tt;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );

    guid = gnc_sql_load_guid( be, row );
    tt = gncTaxTableLookup( be->primary_book, guid );
    if( tt == NULL ) {
        tt = gncTaxTableCreate( be->primary_book );
    }
    gnc_sql_load_object( be, row, GNC_ID_TAXTABLE, tt, tt_col_table );
    gnc_sql_slots_load( be, QOF_INSTANCE(tt) );
	load_taxtable_entries( be, tt );

    qof_instance_mark_clean( QOF_INSTANCE(tt) );
}

static void
load_all_taxtables( GncSqlBackend* be )
{
    GncSqlStatement* stmt;
    GncSqlResult* result;

	g_return_if_fail( be != NULL );

    /* First time, create the query */
    stmt = gnc_sql_create_select_statement( be, TT_TABLE_NAME );
    result = gnc_sql_execute_select_statement( be, stmt );
	gnc_sql_statement_dispose( stmt );
    if( result != NULL ) {
        GncSqlRow* row;

		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
            load_single_taxtable( be, row );
			row = gnc_sql_result_get_next_row( result );
		}
		gnc_sql_result_dispose( result );
    }
}

/* ================================================================= */
static void
create_taxtable_tables( GncSqlBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_sql_get_table_version( be, TT_TABLE_NAME );
    if( version == 0 ) {
        gnc_sql_create_table( be, TT_TABLE_NAME, TT_TABLE_VERSION, tt_col_table );
    }

	version = gnc_sql_get_table_version( be, TTENTRIES_TABLE_NAME );
    if( version == 0 ) {
        gnc_sql_create_table( be, TTENTRIES_TABLE_NAME, TTENTRIES_TABLE_VERSION, ttentries_col_table );
    }
}

/* ================================================================= */
static void
delete_all_tt_entries( GncSqlBackend* be, const GUID* guid )
{
    guid_info_t guid_info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( guid != NULL );

    guid_info.be = be;
    guid_info.guid = guid;
    (void)gnc_sql_do_db_operation( be, OP_DB_DELETE, TTENTRIES_TABLE_NAME,
                                TTENTRIES_TABLE_NAME, &guid_info, guid_col_table );
}

static void
save_tt_entries( GncSqlBackend* be, const GUID* guid, GList* entries )
{
	GList* entry;

	g_return_if_fail( be != NULL );
	g_return_if_fail( guid != NULL );

    /* First, delete the old slots for this object */
    delete_all_tt_entries( be, guid );

	for( entry = entries; entry != NULL; entry = entry->next ) {
		GncTaxTableEntry* e = (GncTaxTableEntry*)entry->data;
    	(void)gnc_sql_do_db_operation( be,
                        OP_DB_ADD_OR_UPDATE,
                        TTENTRIES_TABLE_NAME,
                        GNC_ID_TAXTABLE, e,
                        ttentries_col_table );
    }
}

static void
save_taxtable( QofInstance* inst, GncSqlBackend* be )
{
    GncTaxTable* tt = GNC_TAXTABLE(inst);
    const GUID* guid;

	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_TAXTABLE(inst) );
	g_return_if_fail( be != NULL );

    (void)gnc_sql_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TT_TABLE_NAME,
                        GNC_ID_TAXTABLE, tt,
                        tt_col_table );

    // Now, commit or delete any slots and tax table entries
    guid = qof_instance_get_guid( inst );
    if( !qof_instance_get_destroying(inst) ) {
        gnc_sql_slots_save( be, guid, qof_instance_get_slots( inst ) );
		save_tt_entries( be, guid, gncTaxTableGetEntries( tt ) );
    } else {
        gnc_sql_slots_delete( be, guid );
		delete_all_tt_entries( be, guid );
    }
}

/* ================================================================= */
static void
write_taxtables( GncSqlBackend* be )
{
	g_return_if_fail( be != NULL );

    qof_object_foreach( GNC_ID_TAXTABLE, be->primary_book, (QofInstanceForeachCB)save_taxtable, (gpointer)be );
}

/* ================================================================= */
static void
load_taxtable_guid( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table_row )
{
    const GValue* val;
    GUID guid;
    const GUID* pGuid;
	GncTaxTable* taxtable = NULL;

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
	if( pGuid != NULL ) {
		taxtable = gncTaxTableLookup( be->primary_book, pGuid );
	}
    if( table_row->gobj_param_name != NULL ) {
		g_object_set( pObject, table_row->gobj_param_name, taxtable, NULL );
    } else {
		(*setter)( pObject, (const gpointer)taxtable );
    }
}

static col_type_handler_t taxtable_guid_handler
	= { load_taxtable_guid,
		gnc_sql_add_objectref_guid_col_info_to_list,
		gnc_sql_add_colname_to_list,
		gnc_sql_add_gvalue_objectref_guid_to_slist };
/* ================================================================= */
void
gnc_taxtable_sql_initialize( void )
{
    static GncSqlDataType_t be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_TAXTABLE,
        save_taxtable,						/* commit */
        load_all_taxtables,					/* initial_load */
        create_taxtable_tables,				/* create_tables */
		NULL, NULL, NULL,
		write_taxtables						/* write */
    };

    qof_object_register_backend( GNC_ID_TAXTABLE, GNC_SQL_BACKEND, &be_data );

	gnc_sql_register_col_type_handler( CT_TAXTABLEREF, &taxtable_guid_handler );
}
/* ========================== END OF FILE ===================== */

