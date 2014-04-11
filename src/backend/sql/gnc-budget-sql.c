/********************************************************************
 * gnc-budget-sql.c: load and save data to SQL                      *
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
/** @file gnc-budget-sql.c
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

#include "Recurrence.h"

#include "gnc-budget-sql.h"
#include "gnc-slots-sql.h"
#include "gnc-recurrence-sql.h"

#include "gnc-budget.h"

#if defined( S_SPLINT_S )
#include "splint-defs.h"
#endif

#define BUDGET_TABLE "budgets"
#define TABLE_VERSION 1

/*@ unused @*/ static QofLogModule log_module = G_LOG_DOMAIN;

#define BUDGET_MAX_NAME_LEN 2048
#define BUDGET_MAX_DESCRIPTION_LEN 2048

static const GncSqlColumnTableEntry col_table[] =
{
	/*@ -full_init_block @*/
    { "guid",        CT_GUID,   0,                          COL_NNUL|COL_PKEY, "guid" },
    { "name",        CT_STRING, BUDGET_MAX_NAME_LEN,        COL_NNUL,          "name" },
    { "description", CT_STRING, BUDGET_MAX_DESCRIPTION_LEN, 0,                 "description" },
    { "num_periods", CT_INT,    0,                          COL_NNUL,          "num_periods" },
    { NULL }
	/*@ +full_init_block @*/
};

/* ================================================================= */
static /*@ dependent @*//*@ null @*/ GncBudget*
load_single_budget( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
	GncBudget* pBudget = NULL;
	Recurrence* r;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

    guid = gnc_sql_load_guid( be, row );
	if( guid != NULL ) {
    	pBudget = gnc_budget_lookup( guid, be->primary_book );
	}
    if( pBudget == NULL ) {
        pBudget = gnc_budget_new( be->primary_book );
    }

	gnc_budget_begin_edit( pBudget );
    gnc_sql_load_object( be, row, GNC_ID_BUDGET, pBudget, col_table );
	r = gnc_sql_recurrence_load( be, gnc_budget_get_guid( pBudget ) );
	if( r != NULL ) {
		gnc_budget_set_recurrence( pBudget, r );
		g_free( r );
	}
	gnc_budget_commit_edit( pBudget );

	return pBudget;
}

static void
load_all_budgets( GncSqlBackend* be )
{
    GncSqlStatement* stmt;
    GncSqlResult* result;
	GList* list = NULL;

	g_return_if_fail( be != NULL );

    stmt = gnc_sql_create_select_statement( be, BUDGET_TABLE );
	if( stmt != NULL ) {
    	result = gnc_sql_execute_select_statement( be, stmt );
		gnc_sql_statement_dispose( stmt );
		if( result != NULL ) {
			GncSqlRow* row = gnc_sql_result_get_first_row( result );
			GncBudget* b;

        	while( row != NULL ) {
            	b = load_single_budget( be, row );
				if( b != NULL ) {
					list = g_list_append( list, b );
				}
				row = gnc_sql_result_get_next_row( result );
        	}
			gnc_sql_result_dispose( result );

			if( list != NULL ) {
				gnc_sql_slots_load_for_list( be, list );
				g_list_free( list );
			}
		}
    }
}

/* ================================================================= */
static void
create_budget_tables( GncSqlBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_sql_get_table_version( be, BUDGET_TABLE );
    if( version == 0 ) {
        (void)gnc_sql_create_table( be, BUDGET_TABLE, TABLE_VERSION, col_table );
    }
}

/* ================================================================= */
static gboolean
save_budget( GncSqlBackend* be, QofInstance* inst )
{
    GncBudget* pBudget = GNC_BUDGET(inst);
    const GUID* guid;
	gint op;
	gboolean is_infant;
	gboolean is_ok;

	g_return_val_if_fail( be != NULL, FALSE );
	g_return_val_if_fail( inst != NULL, FALSE );
	g_return_val_if_fail( GNC_IS_BUDGET(inst), FALSE );

	is_infant = qof_instance_get_infant( inst );
	if( qof_instance_get_destroying( inst ) ) {
		op = OP_DB_DELETE;
	} else if( be->is_pristine_db || is_infant ) {
		op = OP_DB_INSERT;
	} else {
		op = OP_DB_UPDATE;
	}
    is_ok = gnc_sql_do_db_operation( be, op, BUDGET_TABLE, GNC_ID_BUDGET, pBudget, col_table );

    // Now, commit any slots and recurrence
	if( is_ok ) {
    	guid = qof_instance_get_guid( inst );
    	if( !qof_instance_get_destroying(inst) ) {
			is_ok = gnc_sql_recurrence_save( be, guid, gnc_budget_get_recurrence( pBudget ) );
			if( is_ok ) {
        		is_ok = gnc_sql_slots_save( be, guid, is_infant, qof_instance_get_slots( inst ) );
			}
    	} else {
        	is_ok = gnc_sql_recurrence_delete( be, guid );
			if( is_ok ) {
        		(void)gnc_sql_slots_delete( be, guid );
			}
    	}
	}

	return is_ok;
}

static void
do_save_budget( QofInstance* inst, gpointer data )
{
	write_objects_t* s = (write_objects_t*)data;

	if( s->is_ok ) {
		s->is_ok = save_budget( s->be, inst );
	}
}

static gboolean
write_budgets( GncSqlBackend* be )
{
	write_objects_t data;

	g_return_val_if_fail( be != NULL, FALSE );

	data.be = be;
	data.is_ok = TRUE;
    qof_collection_foreach( qof_book_get_collection( be->primary_book, GNC_ID_BUDGET ),
                            (QofInstanceForeachCB)do_save_budget, &data );

	return data.is_ok;
}

/* ================================================================= */
void
gnc_sql_init_budget_handler( void )
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_BUDGET,
        save_budget,    		        /* commit */
        load_all_budgets,               /* initial_load */
        create_budget_tables,	        /* create_tables */
		NULL,                           /* compile_query */
		NULL,                           /* run_query */
		NULL,                           /* free_query */
		write_budgets					/* write */
    };

    (void)qof_object_register_backend( GNC_ID_BUDGET, GNC_SQL_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
