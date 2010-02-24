/********************************************************************
 * gnc-schedxaction-sql.c: load and save data to SQL                *
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
/** @file gnc-schedxaction-sql.c
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

#include "gnc-schedxaction-sql.h"
#include "gnc-slots-sql.h"

#include "SchedXaction.h"
#include "SX-book.h"
#include "Recurrence.h"

#include "gnc-recurrence-sql.h"
#include "gnc-transaction-sql.h"

#ifdef S_SPLINT_S
#include "splint-defs.h"
#endif

#define SCHEDXACTION_TABLE "schedxactions"
#define TABLE_VERSION 1

/*@ unused @*/ static QofLogModule log_module = G_LOG_DOMAIN;

#define SX_MAX_NAME_LEN 2048

static gboolean get_autocreate( gpointer pObject );
static void set_autocreate( gpointer pObject, gboolean value );
static gboolean get_autonotify( gpointer pObject );
static void set_autonotify( gpointer pObject, gboolean value );
static gint get_instance_count( gpointer pObject );
static /*@ null @*/ gpointer get_template_act_guid( gpointer pObject );
static void set_template_act_guid( gpointer pObject, /*@ null @*/ gpointer pValue );

static const GncSqlColumnTableEntry col_table[] =
{
	/*@ -full_init_block @*/
    { "guid",              CT_GUID,    0,               COL_NNUL|COL_PKEY, "guid" },
    { "name",              CT_STRING,  SX_MAX_NAME_LEN, 0,                 NULL, GNC_SX_NAME },
	{ "enabled",           CT_BOOLEAN, 0,               COL_NNUL,          NULL, NULL,
			(QofAccessFunc)xaccSchedXactionGetEnabled, (QofSetterFunc)xaccSchedXactionSetEnabled },
    { "start_date",        CT_GDATE,   0,               0,                 NULL, GNC_SX_START_DATE },
    { "end_date",          CT_GDATE,   0,               0,                 NULL, NULL,
			(QofAccessFunc)xaccSchedXactionGetEndDate, (QofSetterFunc)xaccSchedXactionSetEndDate },
    { "last_occur",        CT_GDATE,   0,               0,                 NULL, GNC_SX_LAST_DATE },
    { "num_occur",         CT_INT,     0,               COL_NNUL,          NULL, GNC_SX_NUM_OCCUR },
    { "rem_occur",         CT_INT,     0,               COL_NNUL,          NULL, GNC_SX_REM_OCCUR },
    { "auto_create",       CT_BOOLEAN, 0,               COL_NNUL,          NULL, NULL,
			(QofAccessFunc)get_autocreate,        (QofSetterFunc)set_autocreate },
    { "auto_notify",       CT_BOOLEAN, 0,               COL_NNUL,          NULL, NULL,
			(QofAccessFunc)get_autonotify,        (QofSetterFunc)set_autonotify },
    { "adv_creation",      CT_INT,     0,               COL_NNUL,          NULL, NULL,
            (QofAccessFunc)xaccSchedXactionGetAdvanceCreation,
            (QofSetterFunc)xaccSchedXactionSetAdvanceCreation },
    { "adv_notify",        CT_INT,     0,               COL_NNUL,          NULL, NULL,
            (QofAccessFunc)xaccSchedXactionGetAdvanceReminder,
            (QofSetterFunc)xaccSchedXactionSetAdvanceReminder },
	{ "instance_count",    CT_INT,     0,               COL_NNUL,          NULL, NULL,
			(QofAccessFunc)get_instance_count, (QofSetterFunc)gnc_sx_set_instance_count },
    { "template_act_guid", CT_GUID,    0,               COL_NNUL,          NULL, NULL,
			(QofAccessFunc)get_template_act_guid, set_template_act_guid },
    { NULL }
	/*@ +full_init_block @*/
};

/* ================================================================= */

static gboolean
get_autocreate( gpointer pObject )
{
    const SchedXaction* pSx;
    gboolean autoCreate;
    gboolean autoNotify;

	g_return_val_if_fail( pObject != NULL, FALSE );
	g_return_val_if_fail( GNC_IS_SX(pObject), FALSE );

    pSx = GNC_SX(pObject);
    xaccSchedXactionGetAutoCreate( pSx, &autoCreate, &autoNotify );
    return autoCreate;
}

static void 
set_autocreate( gpointer pObject, gboolean value )
{
    SchedXaction* pSx;
    gboolean autoNotify;
	gboolean dummy;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( GNC_IS_SX(pObject) );

    pSx = GNC_SX(pObject);
    xaccSchedXactionGetAutoCreate( pSx, &dummy, &autoNotify );
    xaccSchedXactionSetAutoCreate( pSx, value, autoNotify );
}

static gboolean
get_autonotify( gpointer pObject )
{
    const SchedXaction* pSx;
    gboolean autoCreate;
    gboolean autoNotify;

	g_return_val_if_fail( pObject != NULL, FALSE );
	g_return_val_if_fail( GNC_IS_SX(pObject), FALSE );

    pSx = GNC_SX(pObject);
    xaccSchedXactionGetAutoCreate( pSx, &autoCreate, &autoNotify );
    return autoNotify;
}

static void 
set_autonotify( gpointer pObject, gboolean value )
{
    SchedXaction* pSx;
    gboolean autoCreate;
    gboolean dummy;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( GNC_IS_SX(pObject) );

    pSx = GNC_SX(pObject);
    xaccSchedXactionGetAutoCreate( pSx, &autoCreate, &dummy );
    xaccSchedXactionSetAutoCreate( pSx, autoCreate, value );
}

static gint
get_instance_count( gpointer pObject )
{
	g_return_val_if_fail( pObject != NULL, FALSE );
	g_return_val_if_fail( GNC_IS_SX(pObject), FALSE );

    return gnc_sx_get_instance_count( GNC_SX(pObject), NULL );
}

static gpointer
get_template_act_guid( gpointer pObject )
{
    const SchedXaction* pSx;

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( GNC_IS_SX(pObject), NULL );

    pSx = GNC_SX(pObject);
    return (gpointer)xaccAccountGetGUID( pSx->template_acct );
}

static void 
set_template_act_guid( gpointer pObject, gpointer pValue )
{
    SchedXaction* pSx;
    QofBook* pBook;
    GUID* guid = (GUID*)pValue;
	Account* pAcct;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( GNC_IS_SX(pObject) );
	g_return_if_fail( pValue != NULL );

    pSx = GNC_SX(pObject);
    pBook = qof_instance_get_book( QOF_INSTANCE(pSx) );
	pAcct = xaccAccountLookup( guid, pBook );
	sx_set_template_account( pSx, pAcct );
}

/* ================================================================= */
static /*@ null @*/ SchedXaction*
load_single_sx( GncSqlBackend* be, GncSqlRow* row )
{
    const GUID* guid;
	SchedXaction* pSx;
	GList* schedule;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( row != NULL, NULL );

    guid = gnc_sql_load_guid( be, row );
	g_assert( guid != NULL );
    pSx = xaccSchedXactionMalloc( be->primary_book );

	gnc_sx_begin_edit( pSx );
    gnc_sql_load_object( be, row, GNC_SX_ID, pSx, col_table );
	schedule = gnc_sql_recurrence_load_list( be, guid );
	gnc_sx_set_schedule( pSx, schedule );
	gnc_sx_commit_edit( pSx );
	gnc_sql_transaction_load_tx_for_account( be, pSx->template_acct );

    return pSx;
}

static void
load_all_sxes( GncSqlBackend* be )
{
    GncSqlStatement* stmt = NULL;
    GncSqlResult* result;

	g_return_if_fail( be != NULL );

    stmt = gnc_sql_create_select_statement( be, SCHEDXACTION_TABLE );
	if( stmt == NULL ) return;
    result = gnc_sql_execute_select_statement( be, stmt );
	gnc_sql_statement_dispose( stmt );
    if( result != NULL ) {
		GncSqlRow* row;
     	SchedXactions *sxes;
		GList* list = NULL;
     	sxes = gnc_book_get_schedxactions( be->primary_book );

		row = gnc_sql_result_get_first_row( result );
        while( row != NULL ) {
            SchedXaction* sx;
			
			sx = load_single_sx( be, row );
			if( sx != NULL ) {
		    	gnc_sxes_add_sx( sxes, sx );
				list = g_list_prepend( list, sx );
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

/* ================================================================= */
static void
create_sx_tables( GncSqlBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_sql_get_table_version( be, SCHEDXACTION_TABLE );
    if( version == 0 ) {
        (void)gnc_sql_create_table( be, SCHEDXACTION_TABLE, TABLE_VERSION, col_table );
    }
}

/* ================================================================= */
gboolean
gnc_sql_save_schedxaction( GncSqlBackend* be, QofInstance* inst )
{
    SchedXaction* pSx;
    const GUID* guid;
	gint op;
	gboolean is_infant;
	gboolean is_ok;

	g_return_val_if_fail( be != NULL, FALSE );
	g_return_val_if_fail( inst != NULL, FALSE );
	g_return_val_if_fail( GNC_IS_SX(inst), FALSE );

    pSx = GNC_SX(inst);

	is_infant = qof_instance_get_infant( inst );
	if( qof_instance_get_destroying( inst ) ) {
		op = OP_DB_DELETE;
	} else if( be->is_pristine_db || is_infant ) {
		op = OP_DB_INSERT;
	} else {
		op = OP_DB_UPDATE;
	}
    is_ok = gnc_sql_do_db_operation( be, op, SCHEDXACTION_TABLE, GNC_SX_ID, pSx, col_table );
    guid = qof_instance_get_guid( inst );
	if( op == OP_DB_INSERT || op == OP_DB_UPDATE ) {
		gnc_sql_recurrence_save_list( be, guid, gnc_sx_get_schedule( pSx ) );
	} else {
		gnc_sql_recurrence_delete( be, guid );
	}

	if( is_ok ) {
    	// Now, commit any slots
		if( op == OP_DB_INSERT || op == OP_DB_UPDATE ) {
        	is_ok = gnc_sql_slots_save( be, guid, is_infant, qof_instance_get_slots( inst ) );
    	} else {
        	is_ok = gnc_sql_slots_delete( be, guid );
    	}
	}

	return is_ok;
}

/* ================================================================= */
void
gnc_sql_init_schedxaction_handler( void )
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_SCHEDXACTION,
        gnc_sql_save_schedxaction,    /* commit */
        load_all_sxes,                /* initial_load */
        create_sx_tables,             /* create_tables */
		NULL,                         /* compile_query */
		NULL,                         /* run_query */
		NULL,                         /* free_query */
		NULL                          /* write */
    };

    (void)qof_object_register_backend( GNC_ID_SCHEDXACTION, GNC_SQL_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
