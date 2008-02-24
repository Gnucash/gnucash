/********************************************************************
 * gnc-schedxaction-gda.c: load and save data to SQL via libgda     *
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
/** @file gnc-schedxaction-gda.c
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

#include "gnc-schedxaction-gda.h"
#include "gnc-slots-gda.h"

#include "SchedXaction.h"
#include "SX-book.h"
#include "Recurrence.h"

#include "gnc-recurrence-gda.h"

#define SCHEDXACTION_TABLE "schedxactions"

static QofLogModule log_module = G_LOG_DOMAIN;

#define SX_MAX_NAME_LEN 2048

static gpointer get_autocreate( gpointer pObject, const QofParam* param );
static void set_autocreate( gpointer pObject, gpointer pValue );
static gpointer get_autonotify( gpointer pObject, const QofParam* param );
static void set_autonotify( gpointer pObject, gpointer pValue );
static gpointer get_template_act_guid( gpointer pObject, const QofParam* param );
static void set_template_act_guid( gpointer pObject, gpointer pValue );

static col_cvt_t col_table[] =
{
    { "guid",              CT_GUID,    0,               COL_NNUL|COL_PKEY, "guid" },
    { "name",              CT_STRING,  SX_MAX_NAME_LEN, 0,                 NULL, GNC_SX_NAME },
    { "start_date",        CT_GDATE,   0,               COL_NNUL,          NULL, GNC_SX_START_DATE },
    { "last_occur",        CT_GDATE,   0,               0,                 NULL, GNC_SX_LAST_DATE },
    { "num_occur",         CT_INT,     0,               COL_NNUL,          NULL, GNC_SX_NUM_OCCUR },
    { "rem_occur",         CT_INT,     0,               COL_NNUL,          NULL, GNC_SX_REM_OCCUR },
    { "auto_create",       CT_BOOLEAN, 0,               COL_NNUL,          NULL, NULL,
			get_autocreate,        set_autocreate },
    { "auto_notify",       CT_BOOLEAN, 0,               COL_NNUL,          NULL, NULL,
			get_autonotify,        set_autonotify },
    { "adv_creation",      CT_INT,     0,               COL_NNUL,          NULL, NULL,
            (QofAccessFunc)xaccSchedXactionGetAdvanceCreation,
            (QofSetterFunc)xaccSchedXactionSetAdvanceCreation },
    { "adv_notify",        CT_INT,     0,               COL_NNUL,          NULL, NULL,
            (QofAccessFunc)xaccSchedXactionGetAdvanceReminder,
            (QofSetterFunc)xaccSchedXactionSetAdvanceReminder },
    { "template_act_guid", CT_GUID,    0,               COL_NNUL,          NULL, NULL,
			get_template_act_guid, set_template_act_guid },
    { NULL }
};

/* ================================================================= */

static gpointer
get_autocreate( gpointer pObject, const QofParam* param )
{
    const SchedXaction* pSx = GNC_SX(pObject);
    gboolean autoCreate;
    gboolean autoNotify;

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( GNC_IS_SX(pObject), NULL );

    xaccSchedXactionGetAutoCreate( pSx, &autoCreate, &autoNotify );
    return GINT_TO_POINTER(autoCreate);
}

static void 
set_autocreate( gpointer pObject, gpointer pValue )
{
    SchedXaction* pSx = GNC_SX(pObject);
    gboolean autoCreate;
    gboolean autoNotify;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( GNC_IS_SX(pObject) );

    xaccSchedXactionGetAutoCreate( pSx, &autoCreate, &autoNotify );
    autoCreate = GPOINTER_TO_INT(pValue);
    xaccSchedXactionSetAutoCreate( pSx, autoCreate, autoNotify );
}

static gpointer
get_autonotify( gpointer pObject, const QofParam* param )
{
    const SchedXaction* pSx = GNC_SX(pObject);
    gboolean autoCreate;
    gboolean autoNotify;

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( GNC_IS_SX(pObject), NULL );

    xaccSchedXactionGetAutoCreate( pSx, &autoCreate, &autoNotify );
    return GINT_TO_POINTER(autoNotify);
}

static void 
set_autonotify( gpointer pObject, gpointer pValue )
{
    SchedXaction* pSx = GNC_SX(pObject);
    gboolean autoCreate;
    gboolean autoNotify;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( GNC_IS_SX(pObject) );

    xaccSchedXactionGetAutoCreate( pSx, &autoCreate, &autoNotify );
    autoNotify = GPOINTER_TO_INT(pValue);
    xaccSchedXactionSetAutoCreate( pSx, autoCreate, autoNotify );
}

static gpointer
get_template_act_guid( gpointer pObject, const QofParam* param )
{
    const SchedXaction* pSx = GNC_SX(pObject);

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( GNC_IS_SX(pObject), NULL );

    return (gpointer)xaccAccountGetGUID( pSx->template_acct );
}

static void 
set_template_act_guid( gpointer pObject, gpointer pValue )
{
    SchedXaction* pSx = GNC_SX(pObject);
    QofBook* pBook;
    GUID* guid = (GUID*)pValue;
	Account* pAcct;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( GNC_IS_SX(pObject) );
	g_return_if_fail( pValue != NULL );

    pBook = qof_instance_get_book( QOF_INSTANCE(pSx) );
	pAcct = xaccAccountLookup( guid, pBook );
	sx_set_template_account( pSx, pAcct );
}

/* ================================================================= */
static SchedXaction*
load_single_sx( GncGdaBackend* be, GdaDataModel* pModel, int row, GList** pList )
{
    const GUID* guid;
    GUID sx_guid;
	SchedXaction* pSx;
	GList* schedule = NULL;

	g_return_val_if_fail( be != NULL, NULL );
	g_return_val_if_fail( pModel != NULL, NULL );
	g_return_val_if_fail( row >= 0, NULL );

    guid = gnc_gda_load_guid( be, pModel, row );
    sx_guid = *guid;

    pSx = xaccSchedXactionMalloc( be->primary_book );

    gnc_gda_load_object( be, pModel, row, /*GNC_ID_SCHEDXACTION*/GNC_SX_ID, pSx, col_table );
	gnc_gda_recurrence_load_list( be, guid, &schedule );
	gnc_sx_set_schedule( pSx, schedule );
//    gnc_gda_slots_load( be, QOF_INSTANCE(pSx) );
	*pList = g_list_append( *pList, pSx );

    qof_instance_mark_clean( QOF_INSTANCE(pSx) );

    return pSx;
}

static void
load_all_sxes( GncGdaBackend* be )
{
    static GdaQuery* query;
    GdaObject* ret;

	g_return_if_fail( be != NULL );

    if( query == NULL ) {
        query = gnc_gda_create_select_query( be, SCHEDXACTION_TABLE );
    }
    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;
     	SchedXactions *sxes;
		GList* list = NULL;
     	sxes = gnc_book_get_schedxactions( be->primary_book );

        for( r = 0; r < numRows; r++ ) {
            SchedXaction* sx;
			
			sx = load_single_sx( be, pModel, r, &list );
			if( sx != NULL ) {
		    	gnc_sxes_add_sx(sxes, sx);
			}
        }

		if( list != NULL ) {
			gnc_gda_slots_load_for_list( be, list );
		}
    }
}

/* ================================================================= */
static void
create_sx_tables( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    gnc_gda_create_table_if_needed( be, SCHEDXACTION_TABLE, col_table );
}

/* ================================================================= */
void
gnc_gda_save_schedxaction( QofInstance* inst, GncGdaBackend* be )
{
    SchedXaction* pSx = GNC_SX(inst);
    const GUID* guid;
	gint op;

	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_SX(inst) );
	g_return_if_fail( be != NULL );

	if( qof_instance_get_destroying( inst ) ) {
		op = OP_DB_DELETE;
	} else if( be->is_pristine_db ) {
		op = OP_DB_ADD;
	} else {
		op = OP_DB_ADD_OR_UPDATE;
	}
    (void)gnc_gda_do_db_operation( be, op, SCHEDXACTION_TABLE, /*GNC_ID_SCHEDXACTION*/GNC_SX_ID, pSx, col_table );
	gnc_gda_recurrence_save_list( be, guid, gnc_sx_get_schedule( pSx ) );

    // Now, commit any slots
    guid = qof_instance_get_guid( inst );
    if( !qof_instance_get_destroying(inst) ) {
        gnc_gda_slots_save( be, guid, qof_instance_get_slots( inst ) );
    } else {
        gnc_gda_slots_delete( be, guid );
    }
}

/* ================================================================= */
void
gnc_gda_init_schedxaction_handler( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_SCHEDXACTION,
        gnc_gda_save_schedxaction,                /* commit */
        load_all_sxes,                /* initial_load */
        create_sx_tables        /* create_tables */
    };

    qof_object_register_backend( GNC_ID_SCHEDXACTION, GNC_GDA_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
