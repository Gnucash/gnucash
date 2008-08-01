/********************************************************************\
 * gnc-bill-term-sql.c -- billing term sql backend                  *
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

/** @file gnc-bill-term-sql.c
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

#include "gncBillTermP.h"
#include "gncInvoice.h"
#include "gnc-bill-term-sql.h"
#include "qof.h"

#define _GNC_MOD_NAME	GNC_ID_BILLTERM

static QofLogModule log_module = G_LOG_DOMAIN;

#define MAX_NAME_LEN 2048
#define MAX_DESCRIPTION_LEN 2048
#define MAX_TYPE_LEN 2048

static void set_invisible( gpointer data, gboolean value );

#define TABLE_NAME "billterms"
#define TABLE_VERSION 1

static GncSqlColumnTableEntry col_table[] =
{
	{ "guid",         CT_GUID,        0,                   COL_NNUL|COL_PKEY, "guid" },
	{ "name",         CT_STRING,      MAX_NAME_LEN,        COL_NNUL,          NULL, GNC_BILLTERM_NAME },
	{ "description",  CT_STRING,      MAX_DESCRIPTION_LEN, COL_NNUL,          NULL, GNC_BILLTERM_DESC },
	{ "refcount",     CT_INT,         0,                   COL_NNUL,          NULL, NULL,
			(QofAccessFunc)gncBillTermGetRefcount,  (QofSetterFunc)gncBillTermSetRefcount },
	{ "invisible",    CT_BOOLEAN,     0,                   COL_NNUL,          NULL, NULL,
			(QofAccessFunc)gncBillTermGetInvisible, (QofSetterFunc)set_invisible },
	{ "parent",       CT_BILLTERMREF, 0,                   0,                 NULL, NULL,
			(QofAccessFunc)gncBillTermGetParent,    (QofSetterFunc)gncBillTermSetParent },
	{ "child",        CT_BILLTERMREF, 0,                   0,                 NULL, NULL,
			(QofAccessFunc)gncBillTermReturnChild,  (QofSetterFunc)gncBillTermSetChild },
	{ "type",         CT_STRING,      MAX_TYPE_LEN,        COL_NNUL,          NULL, GNC_BILLTERM_TYPE },
	{ "duedays",      CT_INT,         0,                   0,                 0,    GNC_BILLTERM_DUEDAYS },
	{ "discountdays", CT_INT,         0,                   0,                 0,    GNC_BILLTERM_DISCDAYS },
	{ "discount",     CT_NUMERIC,     0,                   0,                 0,    GNC_BILLTERM_DISCOUNT },
	{ "cutoff",       CT_INT,         0,                   0,                 0,    GNC_BILLTERM_CUTOFF },
	{ NULL }
};

static void
set_invisible( gpointer data, gboolean value )
{
	GncBillTerm* term = GNC_BILLTERM(data);

	g_return_if_fail( term != NULL );

	if( value ) {
		gncBillTermMakeInvisible( term );
	}
}

static GncBillTerm*
load_single_billterm( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
	GncBillTerm* pBillTerm;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

    guid = gnc_sql_load_guid( be, row );
    pBillTerm = gncBillTermLookup( be->primary_book, guid );
    if( pBillTerm == NULL ) {
        pBillTerm = gncBillTermCreate( be->primary_book );
    }
    gnc_sql_load_object( be, row, GNC_ID_BILLTERM, pBillTerm, col_table );
    qof_instance_mark_clean( QOF_INSTANCE(pBillTerm) );

	return pBillTerm;
}

static void
load_all_billterms( GncSqlBackend* be )
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
            GncBillTerm* pBillTerm = load_single_billterm( be, row );
			if( pBillTerm != NULL ) {
				list = g_list_append( list, pBillTerm );
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
do_save_billterm( QofInstance* inst, gpointer p2 )
{
	gnc_sql_save_billterm( (GncSqlBackend*)p2, inst );
}

static void
write_billterms( GncSqlBackend* be )
{
	g_return_if_fail( be != NULL );

    qof_object_foreach( GNC_ID_BILLTERM, be->primary_book, do_save_billterm, (gpointer)be );
}

/* ================================================================= */
static void
create_billterm_tables( GncSqlBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_sql_get_table_version( be, TABLE_NAME );
    if( version == 0 ) {
        gnc_sql_create_table( be, TABLE_NAME, TABLE_VERSION, col_table );
    }
}

/* ================================================================= */
void
gnc_sql_save_billterm( GncSqlBackend* be, QofInstance* inst )
{
	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_BILLTERM(inst) );
	g_return_if_fail( be != NULL );

    gnc_sql_commit_standard_item( be, inst, TABLE_NAME, GNC_ID_BILLTERM, col_table );
}

/* ================================================================= */
static void
load_billterm_guid( const GncSqlBackend* be, GncSqlRow* row,
            QofSetterFunc setter, gpointer pObject,
            const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    GUID guid;
    const GUID* pGuid;
	GncBillTerm* term = NULL;

	g_return_if_fail( be != NULL );
	g_return_if_fail( row != NULL );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if( val == NULL || !G_VALUE_HOLDS_STRING( val ) || g_value_get_string( val ) == NULL ) {
        pGuid = NULL;
    } else {
        string_to_guid( g_value_get_string( val ), &guid );
        pGuid = &guid;
    }
	if( pGuid != NULL ) {
		term = gncBillTermLookup( be->primary_book, pGuid );
	}
    if( table_row->gobj_param_name != NULL ) {
		g_object_set( pObject, table_row->gobj_param_name, term, NULL );
    } else {
		(*setter)( pObject, (const gpointer)term );
    }
}

static col_type_handler_t billterm_guid_handler
	= { load_billterm_guid,
		gnc_sql_add_objectref_guid_col_info_to_list,
		gnc_sql_add_colname_to_list,
		gnc_sql_add_gvalue_objectref_guid_to_slist };
/* ================================================================= */
void
gnc_billterm_sql_initialize( void )
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_BILLTERM,
        gnc_sql_save_billterm,				/* commit */
        load_all_billterms,					/* initial_load */
        create_billterm_tables,				/* create_tables */
		NULL, NULL, NULL,
		write_billterms						/* write */
    };

    qof_object_register_backend( GNC_ID_BILLTERM, GNC_SQL_BACKEND, &be_data );

	gnc_sql_register_col_type_handler( CT_BILLTERMREF, &billterm_guid_handler );
}
/* ========================== END OF FILE ===================== */
