/********************************************************************
 * gnc-commodity-gda.c: load and save data to SQL via libgda        *
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
/** @file gnc-commodity-gda.c
 *  @brief load and save data to SQL 
 *  @author Copyright (c) 2000 Gnumatic Inc.
 *  @author Copyright (c) 2002 Derek Atkins <warlord@MIT.EDU>
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
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
#include "gnc-commodity.h"

#include "gnc-commodity-gda.h"
#include "gnc-slots-gda.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

static gpointer get_quote_source_name( gpointer pObject, const QofParam* );
static void set_quote_source_name( gpointer pObject, gpointer pValue );

#define COMMODITIES_TABLE "commodities"

#define COMMODITY_MAX_NAMESPACE_LEN 40
#define COMMODITY_MAX_MNEMONIC_LEN 40
#define COMMODITY_MAX_FULLNAME_LEN 100
#define COMMODITY_MAX_CUSIP_LEN 50
#define COMMODITY_MAX_QUOTESOURCE_LEN 50
#define COMMODITY_MAX_QUOTE_TZ_LEN 50

static col_cvt_t col_table[] = {
    { "guid",            CT_GUID,    0, COL_NNUL|COL_PKEY,    NULL,
            (QofAccessFunc)qof_instance_get_guid,
            (QofSetterFunc)qof_instance_set_guid },
    { "namespace",        CT_STRING,    COMMODITY_MAX_NAMESPACE_LEN, COL_NNUL,    NULL,
            (QofAccessFunc)gnc_commodity_get_namespace,
            (QofSetterFunc)gnc_commodity_set_namespace },
    { "mnemonic",        CT_STRING,    COMMODITY_MAX_MNEMONIC_LEN, COL_NNUL,    NULL,
            (QofAccessFunc)gnc_commodity_get_mnemonic,
            (QofSetterFunc)gnc_commodity_set_mnemonic },
    { "fullname",        CT_STRING,    COMMODITY_MAX_FULLNAME_LEN, COL_NNUL,    NULL,
            (QofAccessFunc)gnc_commodity_get_fullname,
            (QofSetterFunc)gnc_commodity_set_fullname },
    { "cusip",            CT_STRING,    COMMODITY_MAX_CUSIP_LEN, COL_NNUL,    NULL,
            (QofAccessFunc)gnc_commodity_get_cusip,
            (QofSetterFunc)gnc_commodity_set_cusip },
    { "fraction",        CT_INT,        0, COL_NNUL,    NULL,
            (QofAccessFunc)gnc_commodity_get_fraction,
            (QofSetterFunc)gnc_commodity_set_fraction },
    { "quote_flag",        CT_INT,        0, COL_NNUL,    NULL,
            (QofAccessFunc)gnc_commodity_get_quote_flag,
            (QofSetterFunc)gnc_commodity_set_quote_flag },
    { "quote_source",    CT_STRING,    COMMODITY_MAX_QUOTESOURCE_LEN, 0,    NULL,
            get_quote_source_name, set_quote_source_name },
    { "quote_tz",        CT_STRING,    COMMODITY_MAX_QUOTE_TZ_LEN, 0,    NULL,
            (QofAccessFunc)gnc_commodity_get_quote_tz,
            (QofSetterFunc)gnc_commodity_set_quote_tz },
    { NULL }
};

/* ================================================================= */

static gpointer
get_quote_source_name( gpointer pObject, const QofParam* param )
{
    const gnc_commodity* pCommodity = GNC_COMMODITY(pObject);

    return (gpointer)gnc_quote_source_get_internal_name(
                            gnc_commodity_get_quote_source(pCommodity));
}

static void 
set_quote_source_name( gpointer pObject, gpointer pValue )
{
    gnc_commodity* pCommodity = GNC_COMMODITY(pObject);
    const gchar* quote_source_name = (const gchar*)pValue;
    gnc_quote_source* quote_source;

    quote_source = gnc_quote_source_lookup_by_internal( quote_source_name );
    gnc_commodity_set_quote_source( pCommodity, quote_source );
}

static gnc_commodity*
load_commodity( GncGdaBackend* be, GdaDataModel* pModel, int row,
                gnc_commodity* pCommodity )
{
    QofBook* pBook = be->primary_book;
    int col;
    const GValue* val;

    if( pCommodity == NULL ) {
        pCommodity = gnc_commodity_new( pBook, NULL, NULL, NULL, NULL, 100 );
    }

    gnc_gda_load_object( pModel, row, GNC_ID_COMMODITY, pCommodity, col_table );
    gnc_gda_slots_load( be, qof_instance_get_guid( QOF_INSTANCE(pCommodity) ),
                            qof_instance_get_slots( QOF_INSTANCE(pCommodity) ) );

    qof_instance_mark_clean( QOF_INSTANCE(pCommodity) );

    return pCommodity;
}

static void
load_commodities( GncGdaBackend* be )
{
    static GdaQuery* query;
    GdaObject* ret;
    gnc_commodity_table* pTable = gnc_commodity_table_get_table( be->primary_book );

    if( query == NULL ) {
        query = gnc_gda_create_select_query( be, COMMODITIES_TABLE );
    }
    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;
        gnc_commodity* pCommodity;

        for( r = 0; r < numRows; r++ ) {
            gnc_commodity* c;

            pCommodity = load_commodity( be, pModel, r, NULL );

            if( pCommodity != NULL ) {
                GUID guid;

                guid = *qof_instance_get_guid( QOF_INSTANCE(pCommodity) );
                pCommodity = gnc_commodity_table_insert( pTable, pCommodity );
                qof_instance_set_guid( QOF_INSTANCE(pCommodity), &guid );
            }
        }
    }
}
/* ================================================================= */
static void
create_commodities_tables( GncGdaBackend* be )
{
    gnc_gda_create_table_if_needed( be, COMMODITIES_TABLE, col_table );
}

/* ================================================================= */
static void
commit_commodity( GncGdaBackend* be, QofInstance* inst )
{
    const GUID* guid;

    (void)gnc_gda_do_db_operation( be,
                        (inst->do_free ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        COMMODITIES_TABLE,
                        GNC_ID_COMMODITY, (gnc_commodity*)inst,
                        col_table );

    // Delete old slot info
    guid = qof_instance_get_guid( inst );

    // Now, commit or delete any slots
    if( !inst->do_free ) {
        gnc_gda_slots_save( be, guid, qof_instance_get_slots( inst ) );
    } else {
        gnc_gda_slots_delete( be, guid );
    }
}

static gboolean
is_commodity_in_db( GncGdaBackend* be, gnc_commodity* pCommodity )
{
    return gnc_gda_object_is_it_in_db( be, COMMODITIES_TABLE, GNC_ID_COMMODITY,
                                pCommodity, col_table );
}

void gnc_gda_save_commodity( GncGdaBackend* be, gnc_commodity* pCommodity )
{
    if( !is_commodity_in_db( be, pCommodity ) ) {
        commit_commodity( be, QOF_INSTANCE(pCommodity) );
    }
}

/* ================================================================= */
void
gnc_gda_init_commodity_handler( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_COMMODITY,
        commit_commodity,            /* commit */
        load_commodities,            /* initial_load */
        create_commodities_tables    /* create_tables */
    };

    qof_object_register_backend( GNC_ID_COMMODITY, GNC_GDA_BACKEND, &be_data );
}
/* ========================== END OF FILE ===================== */
