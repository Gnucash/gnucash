/********************************************************************
 * gnc-commodity-sql.c: load and save data to SQL                   *
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
/** @file gnc-commodity-sql.c
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */

#include "config.h"

#include <glib.h>

#include "qof.h"

#include "gnc-backend-sql.h"
#include "gnc-commodity.h"

#include "gnc-commodity-sql.h"
#include "gnc-slots-sql.h"

#if defined( S_SPLINT_S )
#include "splint-defs.h"
#endif

/*@ unused @*/ static QofLogModule log_module = G_LOG_DOMAIN;

static /*@ dependent @*//*@ null @*/ gpointer get_quote_source_name( gpointer pObject );
static void set_quote_source_name( gpointer pObject, /*@ null @*/ gpointer pValue );

#define COMMODITIES_TABLE "commodities"
#define TABLE_VERSION 1

#define COMMODITY_MAX_NAMESPACE_LEN 2048
#define COMMODITY_MAX_MNEMONIC_LEN 2048
#define COMMODITY_MAX_FULLNAME_LEN 2048
#define COMMODITY_MAX_CUSIP_LEN 2048
#define COMMODITY_MAX_QUOTESOURCE_LEN 2048
#define COMMODITY_MAX_QUOTE_TZ_LEN 2048

static const GncSqlColumnTableEntry col_table[] =
{
    /*@ -full_init_block @*/
    { "guid",         CT_GUID,    0,                             COL_NNUL | COL_PKEY, "guid" },
    {
        "namespace",    CT_STRING,  COMMODITY_MAX_NAMESPACE_LEN,   COL_NNUL,          NULL, NULL,
        (QofAccessFunc)gnc_commodity_get_namespace,
        (QofSetterFunc)gnc_commodity_set_namespace
    },
    { "mnemonic",     CT_STRING,  COMMODITY_MAX_MNEMONIC_LEN,    COL_NNUL,          "mnemonic" },
    { "fullname",     CT_STRING,  COMMODITY_MAX_FULLNAME_LEN,    0,                 "fullname" },
    { "cusip",        CT_STRING,  COMMODITY_MAX_CUSIP_LEN,       0,                 "cusip" },
    { "fraction",     CT_INT,     0,                             COL_NNUL,          "fraction" },
    { "quote_flag",   CT_BOOLEAN, 0,                             COL_NNUL,          "quote_flag" },
    {
        "quote_source", CT_STRING,  COMMODITY_MAX_QUOTESOURCE_LEN, 0,                 NULL, NULL,
        (QofAccessFunc)get_quote_source_name, set_quote_source_name
    },
    { "quote_tz",     CT_STRING,  COMMODITY_MAX_QUOTE_TZ_LEN,    0,                 "quote-tz" },
    { NULL }
    /*@ +full_init_block @*/
};

/* ================================================================= */

static /*@ dependent @*//*@ null @*/ gpointer
get_quote_source_name( gpointer pObject )
{
    const gnc_commodity* pCommodity;

    g_return_val_if_fail( pObject != NULL, NULL );
    g_return_val_if_fail( GNC_IS_COMMODITY(pObject), NULL );

    pCommodity = GNC_COMMODITY(pObject);
    return (gpointer)gnc_quote_source_get_internal_name(
               gnc_commodity_get_quote_source(pCommodity));
}

static void
set_quote_source_name( gpointer pObject, gpointer pValue )
{
    gnc_commodity* pCommodity;
    const gchar* quote_source_name = (const gchar*)pValue;
    gnc_quote_source* quote_source;

    g_return_if_fail( pObject != NULL );
    g_return_if_fail( GNC_IS_COMMODITY(pObject) );

    if ( pValue == NULL ) return;

    pCommodity = GNC_COMMODITY(pObject);
    quote_source = gnc_quote_source_lookup_by_internal( quote_source_name );
    gnc_commodity_set_quote_source( pCommodity, quote_source );
}

static /*@ dependent @*/ gnc_commodity*
load_single_commodity( GncSqlBackend* be, GncSqlRow* row )
{
    QofBook* pBook = be->book;
    gnc_commodity* pCommodity;

    pCommodity = gnc_commodity_new( pBook, NULL, NULL, NULL, NULL, 100 );
    gnc_commodity_begin_edit( pCommodity );
    gnc_sql_load_object( be, row, GNC_ID_COMMODITY, pCommodity, col_table );
    gnc_commodity_commit_edit( pCommodity );

    return pCommodity;
}

static void
load_all_commodities( GncSqlBackend* be )
{
    GncSqlStatement* stmt;
    GncSqlResult* result;
    gnc_commodity_table* pTable;

    pTable = gnc_commodity_table_get_table( be->book );
    stmt = gnc_sql_create_select_statement( be, COMMODITIES_TABLE );
    if ( stmt == NULL ) return;
    result = gnc_sql_execute_select_statement( be, stmt );
    gnc_sql_statement_dispose( stmt );
    if ( result != NULL )
    {
        gnc_commodity* pCommodity;
        GncSqlRow* row = gnc_sql_result_get_first_row( result );
        gchar* sql;

        while ( row != NULL )
        {
            pCommodity = load_single_commodity( be, row );

            if ( pCommodity != NULL )
            {
                GncGUID guid;

                guid = *qof_instance_get_guid( QOF_INSTANCE(pCommodity) );
                pCommodity = gnc_commodity_table_insert( pTable, pCommodity );
		if (qof_instance_is_dirty (QOF_INSTANCE (pCommodity)))
		    gnc_sql_push_commodity_for_postload_processing (be, (gpointer)pCommodity);
                qof_instance_set_guid( QOF_INSTANCE(pCommodity), &guid );
            }
            row = gnc_sql_result_get_next_row( result );
        }
        gnc_sql_result_dispose( result );

        sql = g_strdup_printf( "SELECT DISTINCT guid FROM %s", COMMODITIES_TABLE );
        gnc_sql_slots_load_for_sql_subquery( be, sql,
                                             (BookLookupFn)gnc_commodity_find_commodity_by_guid );
        g_free( sql );
    }
}
/* ================================================================= */
static void
create_commodities_tables( GncSqlBackend* be )
{
    gint version;

    g_return_if_fail( be != NULL );

    version = gnc_sql_get_table_version( be, COMMODITIES_TABLE );
    if ( version == 0 )
    {
        (void)gnc_sql_create_table( be, COMMODITIES_TABLE, TABLE_VERSION, col_table );
    }
}

/* ================================================================= */
static gboolean
do_commit_commodity( GncSqlBackend* be, QofInstance* inst, gboolean force_insert )
{
    const GncGUID* guid;
    gboolean is_infant;
    gint op;
    gboolean is_ok;

    is_infant = qof_instance_get_infant( inst );
    if ( qof_instance_get_destroying( inst ) )
    {
        op = OP_DB_DELETE;
    }
    else if ( be->is_pristine_db || is_infant || force_insert )
    {
        op = OP_DB_INSERT;
    }
    else
    {
        op = OP_DB_UPDATE;
    }
    is_ok = gnc_sql_do_db_operation( be, op, COMMODITIES_TABLE, GNC_ID_COMMODITY, inst, col_table );

    if ( is_ok )
    {
        // Now, commit any slots
        guid = qof_instance_get_guid( inst );
        if ( !qof_instance_get_destroying(inst) )
        {
            is_ok = gnc_sql_slots_save( be, guid, is_infant, qof_instance_get_slots( inst ) );
        }
        else
        {
            is_ok = gnc_sql_slots_delete( be, guid );
        }
    }

    return is_ok;
}

static gboolean
commit_commodity( GncSqlBackend* be, QofInstance* inst )
{
    g_return_val_if_fail( be != NULL, FALSE );
    g_return_val_if_fail( inst != NULL, FALSE );
    g_return_val_if_fail( GNC_IS_COMMODITY(inst), FALSE );

    return do_commit_commodity( be, inst, FALSE );
}

static gboolean
is_commodity_in_db( GncSqlBackend* be, gnc_commodity* pCommodity )
{
    g_return_val_if_fail( be != NULL, FALSE );
    g_return_val_if_fail( pCommodity != NULL, FALSE );

    return gnc_sql_object_is_it_in_db( be, COMMODITIES_TABLE, GNC_ID_COMMODITY,
                                       pCommodity, col_table );
}

gboolean
gnc_sql_save_commodity( GncSqlBackend* be, gnc_commodity* pCommodity )
{
    gboolean is_ok = TRUE;

    g_return_val_if_fail( be != NULL, FALSE );
    g_return_val_if_fail( pCommodity != NULL, FALSE );

    if ( !is_commodity_in_db( be, pCommodity ) )
    {
        is_ok = do_commit_commodity( be, QOF_INSTANCE(pCommodity), TRUE );
    }

    return is_ok;
}

void
gnc_sql_commit_commodity (gnc_commodity *pCommodity)
{
    g_return_if_fail (pCommodity != NULL);
    g_return_if_fail (GNC_IS_COMMODITY (pCommodity));
    gnc_commodity_begin_edit (pCommodity);
    gnc_commodity_commit_edit (pCommodity);
}

/* ----------------------------------------------------------------- */

static void
load_commodity_guid( const GncSqlBackend* be, GncSqlRow* row,
                     /*@ null @*/ QofSetterFunc setter, gpointer pObject,
                     const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    GncGUID guid;
    gnc_commodity* commodity = NULL;

    g_return_if_fail( be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if ( val != NULL && G_VALUE_HOLDS_STRING( val ) && g_value_get_string( val ) != NULL )
    {
        (void)string_to_guid( g_value_get_string( val ), &guid );
        commodity = gnc_commodity_find_commodity_by_guid( &guid, be->book );
        if ( commodity != NULL )
        {
            if ( table_row->gobj_param_name != NULL )
            {
		qof_instance_increase_editlevel (pObject);
                g_object_set( pObject, table_row->gobj_param_name, commodity, NULL );
		qof_instance_decrease_editlevel (pObject);
            }
            else if ( setter != NULL )
            {
                (*setter)( pObject, (const gpointer)commodity );
            }
        }
        else
        {
            PWARN( "Commodity ref '%s' not found", g_value_get_string( val ) );
        }
    }
}

static GncSqlColumnTypeHandler commodity_guid_handler
= { load_commodity_guid,
    gnc_sql_add_objectref_guid_col_info_to_list,
    gnc_sql_add_colname_to_list,
    gnc_sql_add_gvalue_objectref_guid_to_slist
  };
/* ================================================================= */
void
gnc_sql_init_commodity_handler( void )
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_COMMODITY,
        commit_commodity,            /* commit */
        load_all_commodities,        /* initial_load */
        create_commodities_tables,   /* create_tables */
        NULL,                        /* compile_query */
        NULL,                        /* run_query */
        NULL,                        /* free_query */
        NULL                         /* write */
    };

    (void)qof_object_register_backend( GNC_ID_COMMODITY, GNC_SQL_BACKEND, &be_data );

    gnc_sql_register_col_type_handler( CT_COMMODITYREF, &commodity_guid_handler );
}
/* ========================== END OF FILE ===================== */
