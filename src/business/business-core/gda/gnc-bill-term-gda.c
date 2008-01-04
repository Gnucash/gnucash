/********************************************************************\
 * gnc-bill-term-gda.c -- billing term gda backend                  *
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

#include "gncBillTermP.h"
#include "gncInvoice.h"
#include "gnc-bill-term-gda.h"
#include "qof.h"

#define _GNC_MOD_NAME	GNC_ID_BILLTERM

static QofLogModule log_module = GNC_MOD_BACKEND;

#define MAX_NAME_LEN 50
#define MAX_DESCRIPTION_LEN 50
#define MAX_TYPE_LEN 50

static void set_invisible( gpointer data, gpointer value );

#define TABLE_NAME "billterms"

static col_cvt_t col_table[] =
{
	{ "guid",         CT_GUID,        0,                   COL_NNUL, "guid" },
	{ "name",         CT_STRING,      MAX_NAME_LEN,        COL_NNUL, NULL, GNC_BILLTERM_NAME },
	{ "description",  CT_STRING,      MAX_DESCRIPTION_LEN, COL_NNUL, NULL, GNC_BILLTERM_DESC },
	{ "refcount",     CT_INT,         0,                   COL_NNUL, NULL, NULL,
			(QofAccessFunc)gncBillTermGetRefcount,  (QofSetterFunc)gncBillTermSetRefcount },
	{ "invisible",    CT_BOOLEAN,     0,                   COL_NNUL, NULL, NULL,
			(QofAccessFunc)gncBillTermGetInvisible, (QofSetterFunc)set_invisible },
	{ "parent",       CT_BILLTERMREF, 0,                   0,        NULL, NULL,
			(QofAccessFunc)gncBillTermGetParent,    (QofSetterFunc)gncBillTermSetParent },
	{ "child",        CT_BILLTERMREF, 0,                   0,        NULL, NULL,
			(QofAccessFunc)gncBillTermReturnChild,  (QofSetterFunc)gncBillTermSetChild },
	{ "type",         CT_STRING,      MAX_TYPE_LEN,        COL_NNUL, NULL, GNC_BILLTERM_TYPE },
	{ "duedays",      CT_INT,         0,                   0,        0,    GNC_BILLTERM_DUEDAYS },
	{ "discountdays", CT_INT,         0,                   0,        0,    GNC_BILLTERM_DISCDAYS },
	{ "discount",     CT_NUMERIC,     0,                   0,        0,    GNC_BILLTERM_DISCOUNT },
	{ "cutoff",       CT_INT,         0,                   0,        0,    GNC_BILLTERM_CUTOFF },
	{ NULL }
};

static void
set_invisible( gpointer data, gpointer value )
{
	GncBillTerm* term = GNC_BILLTERM(data);
	gboolean b = GPOINTER_TO_INT(value);

	g_return_if_fail( term != NULL );

	if( b ) {
		gncBillTermMakeInvisible( term );
	}
}

static void
load_single_billterm( GncGdaBackend* be, GdaDataModel* pModel, int row )
{
    const GUID* guid;
    GUID v_guid;
	GncBillTerm* pBillTerm;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );

    guid = gnc_gda_load_guid( be, pModel, row );
    v_guid = *guid;

    pBillTerm = gncBillTermLookup( be->primary_book, &v_guid );
    if( pBillTerm == NULL ) {
        pBillTerm = gncBillTermCreate( be->primary_book );
    }
    gnc_gda_load_object( be, pModel, row, GNC_ID_BILLTERM, pBillTerm, col_table );
    gnc_gda_slots_load( be, qof_instance_get_guid( QOF_INSTANCE( pBillTerm )),
                        qof_instance_get_slots( QOF_INSTANCE(pBillTerm) ) );

    qof_instance_mark_clean( QOF_INSTANCE(pBillTerm) );
}

static void
load_all_billterms( GncGdaBackend* be )
{
    static GdaQuery* query = NULL;
    GdaObject* ret;
    QofBook* pBook;

	g_return_if_fail( be != NULL );

    pBook = be->primary_book;

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
            load_single_billterm( be, pModel, r );
		}
    }
}

/* ================================================================= */
static void
write_billterms( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    qof_object_foreach( GNC_ID_BILLTERM, be->primary_book, (QofInstanceForeachCB)gnc_gda_save_billterm, (gpointer)be );
}

/* ================================================================= */
static void
create_billterm_tables( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    gnc_gda_create_table_if_needed( be, TABLE_NAME, col_table );
}

/* ================================================================= */
void
gnc_gda_save_billterm( QofInstance* inst, GncGdaBackend* be )
{
    const GUID* guid;

	g_return_if_fail( inst != NULL );
	g_return_if_fail( !GNC_IS_BILLTERM(inst) );
	g_return_if_fail( be != NULL );

    (void)gnc_gda_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TABLE_NAME,
                        GNC_ID_BILLTERM, inst,
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
load_billterm_guid( const GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table_row )
{
    const GValue* val;
    GUID guid;
    const GUID* pGuid;
	GncBillTerm* term = NULL;

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
		term = gncBillTermLookup( be->primary_book, pGuid );
	}
    if( table_row->gobj_param_name != NULL ) {
		g_object_set( pObject, table_row->gobj_param_name, term, NULL );
    } else {
		(*setter)( pObject, (const gpointer)term );
    }
}

static col_type_handler_t billterm_guid_handler =
        { load_billterm_guid, gnc_gda_create_objectref_guid_col,
            gnc_gda_get_gvalue_objectref_guid_for_query, gnc_gda_get_gvalue_objectref_guid_cond };
/* ================================================================= */
void
gnc_billterm_gda_initialize( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_BILLTERM,
        gnc_gda_save_billterm,				/* commit */
        load_all_billterms,					/* initial_load */
        create_billterm_tables,				/* create_tables */
		NULL, NULL, NULL,
		write_billterms						/* write */
    };

    qof_object_register_backend( GNC_ID_BILLTERM, GNC_GDA_BACKEND, &be_data );

	gnc_gda_register_col_type_handler( CT_BILLTERMREF, &billterm_guid_handler );
}
/* ========================== END OF FILE ===================== */
