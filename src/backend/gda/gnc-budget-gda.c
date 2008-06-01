/********************************************************************
 * gnc-budget-gda.c: load and save data to SQL via libgda           *
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
/** @file gnc-budget-gda.c
 *  @brief load and save data to SQL 
 *  @author Copyright (c) 2006 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db using libgda
 */

#include "config.h"

#include <glib.h>
#include <libgda/libgda.h>

#include "qof.h"

#include "gnc-backend-util-gda.h"

#include "Recurrence.h"

#include "gnc-budget-gda.h"
#include "gnc-slots-gda.h"
#include "gnc-recurrence-gda.h"

#include "gnc-budget.h"

#define BUDGET_TABLE "budgets"
#define TABLE_VERSION 1

static QofLogModule log_module = G_LOG_DOMAIN;

#define BUDGET_MAX_NAME_LEN 2048
#define BUDGET_MAX_DESCRIPTION_LEN 2048

static const col_cvt_t col_table[] =
{
    { "guid",        CT_GUID,   0,                          COL_NNUL|COL_PKEY, "guid" },
    { "name",        CT_STRING, BUDGET_MAX_NAME_LEN,        COL_NNUL,          NULL, "name" },
    { "description", CT_STRING, BUDGET_MAX_DESCRIPTION_LEN, 0,                 NULL, "description" },
    { "num_periods", CT_INT,    0,                          COL_NNUL,          NULL, "num_periods" },
    { NULL }
};

/* ================================================================= */
static void
load_single_budget( GncGdaBackend* be, GdaDataModel* pModel, int row, GList** pList )
{
    const GUID* guid;
    GUID budget_guid;
	GncBudget* pBudget;
	Recurrence* r;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );

    guid = gnc_gda_load_guid( be, pModel, row );
    budget_guid = *guid;

    pBudget = gnc_budget_lookup( &budget_guid, be->primary_book );
    if( pBudget == NULL ) {
        pBudget = gnc_budget_new( be->primary_book );
    }

    gnc_gda_load_object( be, pModel, row, GNC_ID_BUDGET, pBudget, col_table );
	r = g_new0( Recurrence, 1 );
	gnc_gda_recurrence_load( be, gnc_budget_get_guid( pBudget ), r );
	*pList = g_list_append( *pList, pBudget );

    qof_instance_mark_clean( QOF_INSTANCE(pBudget) );
}

static void
load_all_budgets( GncGdaBackend* be )
{
    static GdaQuery* query;
    GdaObject* ret;

	g_return_if_fail( be != NULL );

    if( query == NULL ) {
        query = gnc_gda_create_select_query( be, BUDGET_TABLE );
    }
    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;
		GList* list = NULL;

        for( r = 0; r < numRows; r++ ) {
            load_single_budget( be, pModel, r, &list );
        }

		if( list != NULL ) {
			gnc_gda_slots_load_for_list( be, list );
		}
    }
}

/* ================================================================= */
static void
create_budget_tables( GncGdaBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_gda_get_table_version( be, BUDGET_TABLE );
    if( version == 0 ) {
    	GError* error = NULL;

        gnc_gda_create_table( be, BUDGET_TABLE, TABLE_VERSION, col_table, &error );
        if( error != NULL ) {
            PERR( "Error creating table: %s\n", error->message );
        }
    }
}

/* ================================================================= */
static void
save_budget( QofInstance* inst, GncGdaBackend* be )
{
    GncBudget* pBudget = GNC_BUDGET(inst);
    const GUID* guid;
	gint op;

	g_return_if_fail( be != NULL );
	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_BUDGET(inst) );

	if( qof_instance_get_destroying( inst ) ) {
		op = OP_DB_DELETE;
	} else if( be->is_pristine_db ) {
		op = OP_DB_ADD;
	} else {
		op = OP_DB_ADD_OR_UPDATE;
	}
    (void)gnc_gda_do_db_operation( be, op, BUDGET_TABLE, GNC_ID_BUDGET, pBudget, col_table );

    // Now, commit any slots and recurrence
    guid = qof_instance_get_guid( inst );
    if( !qof_instance_get_destroying(inst) ) {
		gnc_gda_recurrence_save( be, guid, gnc_budget_get_recurrence( pBudget ) );
        gnc_gda_slots_save( be, guid, qof_instance_get_slots( inst ) );
    } else {
        gnc_gda_recurrence_delete( be, guid );
        gnc_gda_slots_delete( be, guid );
    }
}

static void
write_budgets( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    qof_collection_foreach( qof_book_get_collection( be->primary_book, GNC_ID_BUDGET ),
                            (QofInstanceForeachCB)save_budget, be );
}

/* ================================================================= */
void
gnc_gda_init_budget_handler( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_BUDGET,
        save_budget,    		        /* commit */
        load_all_budgets,               /* initial_load */
        create_budget_tables,	        /* create_tables */
		NULL, NULL, NULL,
		write_budgets					/* write */
    };

    qof_object_register_backend( GNC_ID_BUDGET, GNC_GDA_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
