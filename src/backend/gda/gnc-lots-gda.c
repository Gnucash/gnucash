/********************************************************************
 * gnc-lots-gda.c: load and save data to SQL via libgda             *
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
/** @file gnc-lots-gda.c
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
#include "gnc-lot.h"

#include "gnc-backend-util-gda.h"
#include "gnc-slots-gda.h"

#include "gnc-lots-gda.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

#define TABLE_NAME "lots"

static gpointer get_lot_account( gpointer pObject, const QofParam* param );
static void set_lot_account( gpointer pObject, gpointer pValue );
static gpointer get_lot_is_closed( gpointer pObject, const QofParam* param );
static void set_lot_is_closed( gpointer pObject, gpointer pValue );

static col_cvt_t col_table[] =
{
    { "guid",            CT_GUID,    0, COL_NNUL|COL_PKEY,    NULL, NULL,
            (QofAccessFunc)qof_instance_get_guid,
            (QofSetterFunc)qof_instance_set_guid },
    { "account_guid",    CT_GUID,      0, COL_NNUL,    NULL, NULL,
            get_lot_account, set_lot_account },
    { "is_closed",        CT_STRING,  1, COL_NNUL, NULL, NULL,
            get_lot_is_closed, set_lot_is_closed },
    { NULL }
};

/* ================================================================= */
static gpointer
get_lot_account( gpointer pObject, const QofParam* param )
{
    const GNCLot* lot = GNC_LOT(pObject);
    const Account* pAccount = gnc_lot_get_account( lot );

    return (gpointer)qof_instance_get_guid( QOF_INSTANCE(pAccount) );
}

static void 
set_lot_account( gpointer pObject, gpointer pValue )
{
    GNCLot* lot = GNC_LOT(pObject);
    QofBook* pBook = qof_instance_get_book( QOF_INSTANCE(lot) );
    GUID* guid = (GUID*)pValue;
    Account* pAccount = xaccAccountLookup( guid, pBook );

    xaccAccountInsertLot( pAccount, lot );
}

static gpointer
get_lot_is_closed( gpointer pObject, const QofParam* param )
{
    GNCLot* lot = GNC_LOT(pObject);
    static gboolean is_closed; 

    is_closed = gnc_lot_is_closed( lot );
    return &is_closed;
}

static void
set_lot_is_closed( gpointer pObject, gpointer pValue )
{
    GNCLot* lot = GNC_LOT(pObject);
    const gboolean* pBoolean = (const gboolean*)pValue;

    lot->is_closed = *pBoolean;
}

static GNCLot*
load_single_lot( GncGdaBackend* be, GdaDataModel* pModel, int row, GNCLot* lot )
{
    if( lot == NULL ) {
        lot = gnc_lot_new( be->primary_book );
    }

    gnc_gda_load_object( pModel, row, GNC_ID_LOT, lot, col_table );
    gnc_gda_slots_load( be, qof_instance_get_guid( QOF_INSTANCE(lot) ),
                            qof_instance_get_slots( QOF_INSTANCE(lot) ) );

    qof_instance_mark_clean( QOF_INSTANCE(lot) );

    return lot;
}

static void
load_all_lots( GncGdaBackend* be )
{
    static GdaQuery* query;
    GdaObject* ret;

    if( query == NULL ) {
        query = gnc_gda_create_select_query( be, TABLE_NAME );
    }
    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;
        GNCLot* lot;

        for( r = 0; r < numRows; r++ ) {
            lot = load_single_lot( be, pModel, r, NULL );
        }
    }
}

/* ================================================================= */
static void
create_lots_tables( GncGdaBackend* be )
{
    gnc_gda_create_table_if_needed( be, TABLE_NAME, col_table );
}

/* ================================================================= */

static void
commit_lot( GncGdaBackend* be, QofInstance* inst )
{
    GNCLot* lot = GNC_LOT(inst);

    (void)gnc_gda_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TABLE_NAME,
                        GNC_ID_LOT, lot,
                        col_table );

    // Now, commit any slots
    gnc_gda_slots_save( be, qof_instance_get_guid( inst ),
                        qof_instance_get_slots( inst ) );
}

/* ================================================================= */
void
gnc_gda_init_lot_handler( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_LOT,
        commit_lot,            /* commit */
        load_all_lots,            /* initial_load */
        create_lots_tables    /* create tables */
    };

    qof_object_register_backend( GNC_ID_LOT, GNC_GDA_BACKEND, &be_data );
}

/* ========================== END OF FILE ===================== */
