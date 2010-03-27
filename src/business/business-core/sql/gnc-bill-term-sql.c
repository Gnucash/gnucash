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
static gpointer bt_get_parent( gpointer data );
static void bt_set_parent( gpointer data, gpointer value );
static void bt_set_parent_guid( gpointer data, gpointer value );

#define TABLE_NAME "billterms"
#define TABLE_VERSION 2

static GncSqlColumnTableEntry col_table[] =
{
    { "guid",         CT_GUID,        0,                   COL_NNUL | COL_PKEY, "guid" },
    { "name",         CT_STRING,      MAX_NAME_LEN,        COL_NNUL,          "name" },
    { "description",  CT_STRING,      MAX_DESCRIPTION_LEN, COL_NNUL,          NULL, GNC_BILLTERM_DESC },
    {
        "refcount",     CT_INT,         0,                   COL_NNUL,          NULL, NULL,
        (QofAccessFunc)gncBillTermGetRefcount,  (QofSetterFunc)gncBillTermSetRefcount
    },
    {
        "invisible",    CT_BOOLEAN,     0,                   COL_NNUL,          NULL, NULL,
        (QofAccessFunc)gncBillTermGetInvisible, (QofSetterFunc)set_invisible
    },
    {
        "parent",       CT_GUID,       0,                   0,                 NULL, NULL,
        (QofAccessFunc)bt_get_parent,    (QofSetterFunc)bt_set_parent
    },
#if 0
    {
        "child",        CT_BILLTERMREF, 0,                   0,                 NULL, NULL,
        (QofAccessFunc)gncBillTermReturnChild,  (QofSetterFunc)gncBillTermSetChild
    },
#endif
    { "type",         CT_STRING,      MAX_TYPE_LEN,        COL_NNUL,          NULL, GNC_BILLTERM_TYPE },
    { "duedays",      CT_INT,         0,                   0,                 0,    GNC_BILLTERM_DUEDAYS },
    { "discountdays", CT_INT,         0,                   0,                 0,    GNC_BILLTERM_DISCDAYS },
    { "discount",     CT_NUMERIC,     0,                   0,                 0,    GNC_BILLTERM_DISCOUNT },
    { "cutoff",       CT_INT,         0,                   0,                 0,    GNC_BILLTERM_CUTOFF },
    { NULL }
};

static GncSqlColumnTableEntry billterm_parent_col_table[] =
{
    { "parent", CT_GUID, 0, 0, NULL, NULL, NULL, (QofSetterFunc)bt_set_parent_guid },
    { NULL }
};

typedef struct {
	/*@ dependent @*/ GncBillTerm* billterm;
	GncGUID guid;
    gboolean have_guid;
} billterm_parent_guid_struct;

static void
set_invisible( gpointer data, gboolean value )
{
    GncBillTerm* term = GNC_BILLTERM(data);

    g_return_if_fail( term != NULL );

    if ( value )
    {
        gncBillTermMakeInvisible( term );
    }
}

static /*@ null @*//*@ dependent @*/ gpointer
bt_get_parent( gpointer pObject )
{
    const GncBillTerm* billterm;
    const GncBillTerm* pParent;
    const GncGUID* parent_guid;

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( GNC_IS_BILLTERM(pObject), NULL );

    billterm = GNC_BILLTERM(pObject);
    pParent = gncBillTermGetParent( billterm );
    if( pParent == NULL ) {
        parent_guid = NULL;
    } else {
        parent_guid = qof_instance_get_guid( QOF_INSTANCE(pParent) );
    }

    return (gpointer)parent_guid;
}

static void
bt_set_parent( gpointer data, gpointer value )
{
    GncBillTerm* billterm;
    GncBillTerm* parent;
    QofBook* pBook;
    GncGUID* guid = (GncGUID*)value;

    g_return_if_fail( data != NULL );
    g_return_if_fail( GNC_IS_BILLTERM(data) );

    billterm = GNC_BILLTERM(data);
    pBook = qof_instance_get_book( QOF_INSTANCE(billterm) );
    if ( guid != NULL )
    {
        parent = gncBillTermLookup( pBook, guid );
        if( parent != NULL ) {
            gncBillTermSetParent( billterm, parent );
            gncBillTermSetChild( parent, billterm );
        }
    }
}

static void
bt_set_parent_guid( gpointer pObject, /*@ null @*/ gpointer pValue )
{
	billterm_parent_guid_struct* s = (billterm_parent_guid_struct*)pObject;
    GncGUID* guid = (GncGUID*)pValue;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( pValue != NULL );

	s->guid = *guid;
    s->have_guid = TRUE;
}

static GncBillTerm*
load_single_billterm( GncSqlBackend* be, GncSqlRow* row,
					GList** l_billterms_needing_parents )
{
    const GncGUID* guid;
    GncBillTerm* pBillTerm;

    g_return_val_if_fail( be != NULL, NULL );
    g_return_val_if_fail( row != NULL, NULL );

    guid = gnc_sql_load_guid( be, row );
    pBillTerm = gncBillTermLookup( be->primary_book, guid );
    if ( pBillTerm == NULL )
    {
        pBillTerm = gncBillTermCreate( be->primary_book );
    }
    gnc_sql_load_object( be, row, GNC_ID_BILLTERM, pBillTerm, col_table );

    /* If the billterm doesn't have a parent, it might be because it hasn't been loaded yet.
       If so, add this billterm to the list of billterms with no parent, along with the parent
       GncGUID so that after they are all loaded, the parents can be fixed up. */
    if ( gncBillTermGetParent( pBillTerm ) == NULL )
    {
		billterm_parent_guid_struct* s = g_malloc( (gsize)sizeof(billterm_parent_guid_struct) );
		g_assert( s != NULL );

		s->billterm = pBillTerm;
        s->have_guid = FALSE;
		gnc_sql_load_object( be, row, GNC_ID_TAXTABLE, s, billterm_parent_col_table );
        if ( s->have_guid )
        {
		    *l_billterms_needing_parents = g_list_prepend( *l_billterms_needing_parents, s );
        }
        else
        {
            g_free( s );
        }
    }

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
    if ( result != NULL )
    {
        GncSqlRow* row;
        GList* list = NULL;
        GList* l_billterms_needing_parents = NULL;

        row = gnc_sql_result_get_first_row( result );
        while ( row != NULL )
        {
            GncBillTerm* pBillTerm = load_single_billterm( be, row, &l_billterms_needing_parents );
            if ( pBillTerm != NULL )
            {
                list = g_list_append( list, pBillTerm );
            }
            row = gnc_sql_result_get_next_row( result );
        }
        gnc_sql_result_dispose( result );

        if ( list != NULL )
        {
            gnc_sql_slots_load_for_list( be, list );
        }

		/* While there are items on the list of billterms needing parents,
		   try to see if the parent has now been loaded.  Theory says that if
		   items are removed from the front and added to the back if the
		   parent is still not available, then eventually, the list will
		   shrink to size 0. */
		if( l_billterms_needing_parents != NULL ) {
			gboolean progress_made = TRUE;
            GncTaxTable* root;	
			Account* pParent;
			GList* elem;
				
			while( progress_made ) {
				progress_made = FALSE;
				for( elem = l_billterms_needing_parents; elem != NULL; elem = g_list_next( elem ) ) {
					billterm_parent_guid_struct* s = (billterm_parent_guid_struct*)elem->data;
                    bt_set_parent( s->billterm, &s->guid );
					l_billterms_needing_parents = g_list_delete_link( l_billterms_needing_parents, elem );
					progress_made = TRUE;
				}
			}
        }
    }
}

/* ================================================================= */
typedef struct
{
    GncSqlBackend* be;
    gboolean is_ok;
} write_billterms_t;

static void
do_save_billterm( QofInstance* inst, gpointer p2 )
{
    write_billterms_t* data = (write_billterms_t*)p2;

    if ( data->is_ok )
    {
        data->is_ok = gnc_sql_save_billterm( data->be, inst );
    }
}

static gboolean
write_billterms( GncSqlBackend* be )
{
    write_billterms_t data;

    g_return_val_if_fail( be != NULL, FALSE );

    data.be = be;
    data.is_ok = TRUE;
    qof_object_foreach( GNC_ID_BILLTERM, be->primary_book, do_save_billterm, &data );
    return data.is_ok;
}

/* ================================================================= */
static void
create_billterm_tables( GncSqlBackend* be )
{
    gint version;

    g_return_if_fail( be != NULL );

    version = gnc_sql_get_table_version( be, TABLE_NAME );
    if ( version == 0 )
    {
        gnc_sql_create_table( be, TABLE_NAME, TABLE_VERSION, col_table );
    }
    else if ( version == 1 )
    {
        /* Upgrade 64 bit int handling */
        gnc_sql_upgrade_table( be, TABLE_NAME, col_table );
        gnc_sql_set_table_version( be, TABLE_NAME, TABLE_VERSION );
    }
}

/* ================================================================= */
gboolean
gnc_sql_save_billterm( GncSqlBackend* be, QofInstance* inst )
{
    g_return_val_if_fail( inst != NULL, FALSE );
    g_return_val_if_fail( GNC_IS_BILLTERM(inst), FALSE );
    g_return_val_if_fail( be != NULL, FALSE );

    return gnc_sql_commit_standard_item( be, inst, TABLE_NAME, GNC_ID_BILLTERM, col_table );
}

/* ================================================================= */
static void
load_billterm_guid( const GncSqlBackend* be, GncSqlRow* row,
                    QofSetterFunc setter, gpointer pObject,
                    const GncSqlColumnTableEntry* table_row )
{
    const GValue* val;
    GncGUID guid;
    GncBillTerm* term = NULL;

    g_return_if_fail( be != NULL );
    g_return_if_fail( row != NULL );
    g_return_if_fail( pObject != NULL );
    g_return_if_fail( table_row != NULL );

    val = gnc_sql_row_get_value_at_col_name( row, table_row->col_name );
    if ( val != NULL && G_VALUE_HOLDS_STRING( val ) && g_value_get_string( val ) != NULL )
    {
        string_to_guid( g_value_get_string( val ), &guid );
        term = gncBillTermLookup( be->primary_book, &guid );
        if ( term != NULL )
        {
            if ( table_row->gobj_param_name != NULL )
            {
                g_object_set( pObject, table_row->gobj_param_name, term, NULL );
            }
            else
            {
                (*setter)( pObject, (const gpointer)term );
            }
        }
        else
        {
            PWARN( "Billterm ref '%s' not found", g_value_get_string( val ) );
        }
    }
}

static GncSqlColumnTypeHandler billterm_guid_handler
= { load_billterm_guid,
    gnc_sql_add_objectref_guid_col_info_to_list,
    gnc_sql_add_colname_to_list,
    gnc_sql_add_gvalue_objectref_guid_to_slist
  };
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
