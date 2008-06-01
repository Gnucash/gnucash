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

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "lots"
#define TABLE_VERSION 1

static gpointer get_lot_account( gpointer pObject, const QofParam* param );
static void set_lot_account( gpointer pObject, gpointer pValue );
static void set_lot_is_closed( gpointer pObject, gboolean value );

static const col_cvt_t col_table[] =
{
    { "guid",         CT_GUID,    0, COL_NNUL|COL_PKEY, "guid" },
    { "account_guid", CT_GUID,    0, COL_NNUL,          NULL, NULL, get_lot_account,   set_lot_account },
    { "is_closed",    CT_BOOLEAN, 0, COL_NNUL,          NULL, NULL,
		(QofAccessFunc)gnc_lot_is_closed, (QofSetterFunc)set_lot_is_closed },
    { NULL }
};

/* ================================================================= */
static gpointer
get_lot_account( gpointer pObject, const QofParam* param )
{
    const GNCLot* lot = GNC_LOT(pObject);
    const Account* pAccount;

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( GNC_IS_LOT(pObject), NULL );

    pAccount = gnc_lot_get_account( lot );
    return (gpointer)qof_instance_get_guid( QOF_INSTANCE(pAccount) );
}

static void 
set_lot_account( gpointer pObject, gpointer pValue )
{
    GNCLot* lot = GNC_LOT(pObject);
    QofBook* pBook;
    GUID* guid = (GUID*)pValue;
    Account* pAccount;

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( GNC_IS_LOT(pObject) );
	g_return_if_fail( pValue != NULL );

    pBook = qof_instance_get_book( QOF_INSTANCE(lot) );
    pAccount = xaccAccountLookup( guid, pBook );
    xaccAccountInsertLot( pAccount, lot );
}

static void
set_lot_is_closed( gpointer pObject, gboolean closed )
{
    GNCLot* lot = GNC_LOT(pObject);

	g_return_if_fail( pObject != NULL );
	g_return_if_fail( GNC_IS_LOT(pObject) );

    lot->is_closed = closed;
}

static void
load_single_lot( GncGdaBackend* be, GdaDataModel* pModel, int row, GList** pList )
{
	GNCLot* lot;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );

    lot = gnc_lot_new( be->primary_book );

    gnc_gda_load_object( be, pModel, row, GNC_ID_LOT, lot, col_table );
	*pList = g_list_append( *pList, lot );

    qof_instance_mark_clean( QOF_INSTANCE(lot) );
}

static void
load_all_lots( GncGdaBackend* be )
{
    static GdaQuery* query;
    GdaObject* ret;

	g_return_if_fail( be != NULL );

    if( query == NULL ) {
        query = gnc_gda_create_select_query( be, TABLE_NAME );
    }
    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;
		GList* list = NULL;

        for( r = 0; r < numRows; r++ ) {
            load_single_lot( be, pModel, r, &list );
        }

		if( list != NULL ) {
			gnc_gda_slots_load_for_list( be, list );
		}
    }
}

/* ================================================================= */
static void
create_lots_tables( GncGdaBackend* be )
{
	gint version;

	g_return_if_fail( be != NULL );

	version = gnc_gda_get_table_version( be, TABLE_NAME );
    if( version == 0 ) {
    	GError* error = NULL;

        gnc_gda_create_table( be, TABLE_NAME, TABLE_VERSION, col_table, &error );
        if( error != NULL ) {
            PERR( "Error creating table: %s\n", error->message );
        }
    }
}

/* ================================================================= */

static void
commit_lot( QofInstance* inst, GncGdaBackend* be )
{
	gint op;

	g_return_if_fail( be != NULL );
	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_LOT(inst) );

	if( qof_instance_get_destroying( inst ) ) {
		op = OP_DB_DELETE;
	} else if( be->is_pristine_db ) {
		op = OP_DB_ADD;
	} else {
		op = OP_DB_ADD_OR_UPDATE;
	}
    (void)gnc_gda_do_db_operation( be, op, TABLE_NAME, GNC_ID_LOT, inst, col_table );

    // Now, commit any slots
    gnc_gda_slots_save( be, qof_instance_get_guid( inst ),
                        qof_instance_get_slots( inst ) );
}

/* ----------------------------------------------------------------- */
static void
load_lot_guid( const GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table_row )
{
    const GValue* val;
    GUID guid;
    const GUID* pGuid;
	GNCLot* lot = NULL;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );
	g_return_if_fail( pObject != NULL );
	g_return_if_fail( table_row != NULL );

    val = gda_data_model_get_value_at_col_name( pModel, table_row->col_name, row );
    if( gda_value_is_null( val ) ) {
        pGuid = NULL;
    } else {
        string_to_guid( g_value_get_string( val ), &guid );
        pGuid = &guid;
    }
	if( pGuid != NULL ) {
		lot = gnc_lot_lookup( pGuid, be->primary_book );
	}
    if( table_row->gobj_param_name != NULL ) {
		g_object_set( pObject, table_row->gobj_param_name, lot, NULL );
    } else {
		(*setter)( pObject, (const gpointer)lot );
    }
}

static col_type_handler_t lot_guid_handler =
        { load_lot_guid, gnc_gda_create_objectref_guid_col,
            gnc_gda_get_gvalue_objectref_guid_for_query, gnc_gda_get_gvalue_objectref_guid_cond };
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

	gnc_gda_register_col_type_handler( CT_LOTREF, &lot_guid_handler );
}

/* ========================== END OF FILE ===================== */
