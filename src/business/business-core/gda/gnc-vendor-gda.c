/********************************************************************\
 * gnc-vendor-gda.c -- vendor gda backend                           *
 *                                                                  *
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>                *
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

#include "gnc-commodity.h"

#include "gnc-backend-util-gda.h"
#include "gnc-commodity-gda.h"
#include "gnc-slots-gda.h"

#include "gnc-commodity.h"
#include "gncBillTermP.h"
#include "gncVendorP.h"
#include "gncTaxTableP.h"
#include "gnc-vendor-gda.h"
#include "gnc-address-gda.h"
#include "gnc-bill-term-gda.h"
#include "gnc-tax-table-gda.h"

#define _GNC_MOD_NAME	GNC_ID_VENDOR

static QofLogModule log_module = GNC_MOD_BACKEND;

#define MAX_NAME_LEN 50
#define MAX_ID_LEN 50
#define MAX_NOTES_LEN 100
#define MAX_TAX_INC_LEN 50

#define TABLE_NAME "vendors"

static col_cvt_t col_table[] =
{
	{ "guid",         CT_GUID,          0,               COL_NNUL, "guid" },
	{ "name",         CT_STRING,        MAX_NAME_LEN,    COL_NNUL, NULL, VENDOR_NAME },
	{ "id",           CT_STRING,        MAX_ID_LEN,      COL_NNUL, NULL, VENDOR_ID },
	{ "notes",        CT_STRING,        MAX_NOTES_LEN,   COL_NNUL, NULL, VENDOR_NOTES },
	{ "currency",     CT_COMMODITYREF,  0,               COL_NNUL, NULL, NULL,
			(QofAccessFunc)gncVendorGetCurrency, (QofSetterFunc)gncVendorSetCurrency },
	{ "active",       CT_BOOLEAN,       0,               COL_NNUL, NULL, NULL,
			(QofAccessFunc)gncVendorGetActive, (QofSetterFunc)gncVendorSetActive },
	{ "tax_override", CT_BOOLEAN,       0,               COL_NNUL, NULL, VENDOR_TAX_OVERRIDE },
	{ "addr",         CT_ADDRESS,       0,               0,        NULL, VENDOR_ADDR },
	{ "terms",        CT_BILLTERMREF,   0,               0,        NULL, VENDOR_TERMS },
	{ "tax_inc",      CT_STRING,        MAX_TAX_INC_LEN, 0,        NULL, VENDOR_TAX_INC },
	{ "tax_table",    CT_TAXTABLEREF,   0,               0,        NULL, VENDOR_TAX_TABLE },
	{ NULL }
};

static void
load_single_vendor( GncGdaBackend* be, GdaDataModel* pModel, int row )
{
    const GUID* guid;
    GUID v_guid;
	GncVendor* pVendor;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );

    guid = gnc_gda_load_guid( be, pModel, row );
    v_guid = *guid;

    pVendor = gncVendorLookup( be->primary_book, &v_guid );
    if( pVendor == NULL ) {
        pVendor = gncVendorCreate( be->primary_book );
    }
    gnc_gda_load_object( be, pModel, row, GNC_ID_VENDOR, pVendor, col_table );
    gnc_gda_slots_load( be, QOF_INSTANCE(pVendor) );

    qof_instance_mark_clean( QOF_INSTANCE(pVendor) );
}

static void
load_all_vendors( GncGdaBackend* be )
{
    static GdaQuery* query = NULL;
    GdaObject* ret;

	g_return_if_fail( be != NULL );

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
            (void)load_single_vendor( be, pModel, r );
		}
    }
}

/* ================================================================= */
static void
create_vendor_tables( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    gnc_gda_create_table_if_needed( be, TABLE_NAME, col_table );
}

/* ================================================================= */
static void
save_vendor( QofInstance* inst, GncGdaBackend* be )
{
    GncVendor* v = GNC_VENDOR(inst);
    const GUID* guid;

	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_VENDOR(inst) );
	g_return_if_fail( be != NULL );

    // Ensure the commodity is in the db
    gnc_gda_save_commodity( be, gncVendorGetCurrency( v ) );

    (void)gnc_gda_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TABLE_NAME,
                        GNC_ID_VENDOR, v,
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
static gboolean
vendor_should_be_saved( GncVendor *vendor )
{
    const char *id;

	g_return_val_if_fail( vendor != NULL, FALSE );

    /* make sure this is a valid vendor before we save it -- should have an ID */
    id = gncVendorGetID( vendor );
    if( id == NULL || *id == '\0' ) {
        return FALSE;
	}

    return TRUE;
}

static void
write_single_vendor( QofInstance *term_p, gpointer be_p )
{
    GncGdaBackend* be = (GncGdaBackend*)be_p;

	g_return_if_fail( term_p != NULL );
	g_return_if_fail( GNC_IS_VENDOR(term_p) );
	g_return_if_fail( be_p != NULL );

	if( vendor_should_be_saved( GNC_VENDOR(term_p) ) ) {
    	save_vendor( term_p, be );
	}
}

static void
write_vendors( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    qof_object_foreach( GNC_ID_VENDOR, be->primary_book, write_single_vendor, (gpointer)be );
}

/* ================================================================= */
void
gnc_vendor_gda_initialize( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_VENDOR,
        save_vendor,						/* commit */
        load_all_vendors,					/* initial_load */
        create_vendor_tables,				/* create_tables */
		NULL, NULL, NULL,
		write_vendors						/* write */
    };

    qof_object_register_backend( GNC_ID_VENDOR, GNC_GDA_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
