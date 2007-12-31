/********************************************************************\
 * gnc-order-gda.c -- order gda backend                             *
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

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include <libgda/libgda.h>

#include "gnc-backend-util-gda.h"
#include "gnc-slots-gda.h"

#include "gncOrderP.h"

#include "gnc-order-gda.h"
#include "gnc-owner-gda.h"

#define _GNC_MOD_NAME	GNC_ID_ORDER

static QofLogModule log_module = GNC_MOD_BACKEND;

#define TABLE_NAME "orders"

#define MAX_ID_LEN 50
#define MAX_NOTES_LEN 50
#define MAX_REFERENCE_LEN 50

static col_cvt_t col_table[] =
{
	{ "guid",        CT_GUID,     0,                 COL_NNUL, "guid" },
	{ "id",          CT_STRING,   MAX_ID_LEN,        COL_NNUL, NULL, ORDER_ID },
	{ "notes",       CT_STRING,   MAX_NOTES_LEN,     COL_NNUL, NULL, ORDER_NOTES },
	{ "reference",   CT_STRING,   MAX_REFERENCE_LEN, COL_NNUL, NULL, ORDER_REFERENCE },
	{ "active",      CT_BOOLEAN,  0,                 COL_NNUL, NULL, QOF_PARAM_ACTIVE },
	{ "date_opened", CT_TIMESPEC, 0,                 COL_NNUL, NULL, ORDER_OPENED },
	{ "date_closed", CT_TIMESPEC, 0,                 COL_NNUL, NULL, ORDER_CLOSED },
	{ "owner",       CT_OWNERREF, 0,                 COL_NNUL, NULL, ORDER_OWNER },
	{ NULL },
};

static GncOrder*
load_single_order( GncGdaBackend* be, GdaDataModel* pModel, int row )
{
    const GUID* guid;
    GUID v_guid;
	GncOrder* pOrder;

    guid = gnc_gda_load_guid( be, pModel, row );
    v_guid = *guid;

    pOrder = gncOrderLookup( be->primary_book, &v_guid );
    if( pOrder == NULL ) {
        pOrder = gncOrderCreate( be->primary_book );
    }
    gnc_gda_load_object( be, pModel, row, GNC_ID_ORDER, pOrder, col_table );
    gnc_gda_slots_load( be, qof_instance_get_guid( QOF_INSTANCE( pOrder )),
                        qof_instance_get_slots( QOF_INSTANCE(pOrder) ) );

    qof_instance_mark_clean( QOF_INSTANCE(pOrder) );

    return pOrder;
}

static void
load_all_orders( GncGdaBackend* be )
{
    static GdaQuery* query = NULL;
    GdaObject* ret;
    QofBook* pBook = be->primary_book;

    /* First time, create the query */
    if( query == NULL ) {
        query = gnc_gda_create_select_query( be, TABLE_NAME );
    }

    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;

        for( r = 0; r < numRows; r++ ) {
            (void)load_single_order( be, pModel, r );
		}
    }
}

/* ================================================================= */
static void
create_order_tables( GncGdaBackend* be )
{
    gnc_gda_create_table_if_needed( be, TABLE_NAME, col_table );
}

/* ================================================================= */
static void
save_order( GncGdaBackend* be, QofInstance* inst )
{
    GncOrder* v = GNC_ORDER(inst);
    const GUID* guid;

    (void)gnc_gda_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TABLE_NAME,
                        GNC_ID_ORDER, v,
                        col_table );

    // Now, commit or delete any slots
    guid = qof_instance_get_guid( inst );
    if( !qof_instance_get_destroying(inst) ) {
        gnc_gda_slots_save( be, guid, qof_instance_get_slots( inst ) );
    } else {
        gnc_gda_slots_delete( be, guid );
    }
}

/* ================================================================= */
static void
load_order_guid( const GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table )
{
    const GValue* val;
    GUID guid;
    const GUID* pGuid;
	GncOrder* order = NULL;

    val = gda_data_model_get_value_at_col_name( pModel, table->col_name, row );
    if( gda_value_is_null( val ) ) {
        pGuid = NULL;
    } else {
        string_to_guid( g_value_get_string( val ), &guid );
        pGuid = &guid;
    }
	if( pGuid != NULL ) {
		order = gncOrderLookup( be->primary_book, pGuid );
	}
    if( table->gobj_param_name != NULL ) {
		g_object_set( pObject, table->gobj_param_name, order, NULL );
    } else {
		(*setter)( pObject, (const gpointer)order );
    }
}

static col_type_handler_t order_guid_handler =
        { load_order_guid, gnc_gda_create_objectref_guid_col,
            gnc_gda_get_gvalue_objectref_guid_for_query, gnc_gda_get_gvalue_objectref_guid_cond };
/* ================================================================= */
void
gnc_order_gda_initialize( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_ORDER,
        save_order,						/* commit */
        load_all_orders,				/* initial_load */
        create_order_tables				/* create_tables */
    };

    qof_object_register_backend( GNC_ID_ORDER, GNC_GDA_BACKEND, &be_data );

	gnc_gda_register_col_type_handler( CT_ORDERREF, &order_guid_handler );
}
/* ========================== END OF FILE ===================== */
