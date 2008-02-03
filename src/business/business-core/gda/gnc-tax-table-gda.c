/********************************************************************\
 * gnc-tax-table-xml-v2.c -- tax table xml i/o implementation       *
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

#include "gnc-backend-util-gda.h"
#include "gnc-slots-gda.h"

#include "gncEntry.h"
#include "gncTaxTableP.h"

#include "gnc-tax-table-gda.h"

#define _GNC_MOD_NAME	GNC_ID_TAXTABLE

static QofLogModule log_module = GNC_MOD_BACKEND;

static void set_invisible( gpointer data, gpointer value );

typedef struct {
    GncGdaBackend* be;
    const GUID* guid;
} guid_info_t;

static gpointer get_obj_guid( gpointer pObject, const QofParam* param );
static void set_obj_guid( gpointer pObject, gpointer pValue );
static gpointer get_child( gpointer pObject, const QofParam* param );
static void set_parent( gpointer pObject, gpointer pValue );

#define MAX_NAME_LEN 50

#define TT_TABLE_NAME "taxtables"

static col_cvt_t tt_col_table[] =
{
	{ "guid",      CT_GUID,        0,            COL_NNUL, "guid" },
	{ "name",      CT_STRING,      MAX_NAME_LEN, COL_NNUL, NULL, GNC_TT_NAME },
	{ "refcount",  CT_INT64,       0,            COL_NNUL, NULL, GNC_TT_REFCOUNT },
	{ "invisible", CT_BOOLEAN,     0,            COL_NNUL, NULL, NULL,
			(QofAccessFunc)gncTaxTableGetInvisible, set_invisible },
	{ "child",     CT_TAXTABLEREF, 0,			 0,        NULL, NULL,
			get_child, (QofSetterFunc)gncTaxTableSetChild },
	{ "parent",    CT_TAXTABLEREF, 0,			 0,        NULL, NULL,
			(QofAccessFunc)gncTaxTableGetParent, set_parent },
	{ NULL }
};

#define TTENTRIES_TABLE_NAME "taxtable_entries"

static col_cvt_t ttentries_col_table[] =
{
	{ "id",       CT_INT,         0, COL_NNUL|COL_AUTOINC },
	{ "taxtable", CT_TAXTABLEREF, 0, COL_NNUL,            NULL, NULL,
			(QofAccessFunc)gncTaxTableEntryGetTable, set_obj_guid },
	{ "account",  CT_ACCOUNTREF,  0, COL_NNUL,            NULL, NULL,
			(QofAccessFunc)gncTaxTableEntryGetAccount, (QofSetterFunc)gncTaxTableEntrySetAccount },
	{ "amount",   CT_NUMERIC,     0, COL_NNUL,            NULL, NULL,
			(QofAccessFunc)gncTaxTableEntryGetAmount, (QofSetterFunc)gncTaxTableEntrySetAmount },
	{ "type",     CT_INT,         0, COL_NNUL,            NULL, NULL,
			(QofAccessFunc)gncTaxTableEntryGetType, (QofSetterFunc)gncTaxTableEntrySetType },
	{ NULL }
};

/* Special column table because we need to be able to access the table by
a column other than the primary key */
static col_cvt_t guid_col_table[] =
{
    { "taxtable", CT_GUID, 0, 0, NULL, NULL, get_obj_guid, set_obj_guid },
    { NULL }
};

static gpointer
get_obj_guid( gpointer pObject, const QofParam* param )
{
    guid_info_t* pInfo = (guid_info_t*)pObject;

	g_return_val_if_fail( pInfo != NULL, NULL );

    return (gpointer)pInfo->guid;
}

static void
set_obj_guid( gpointer pObject, gpointer pValue )
{
    // Nowhere to put the GUID
}

static void
set_invisible( gpointer data, gpointer value )
{
	GncTaxTable* tt = GNC_TAXTABLE(data);
	gboolean b = GPOINTER_TO_INT(value);

	g_return_if_fail( data != NULL );
	g_return_if_fail( GNC_IS_TAXTABLE(data) );

	if( b ) {
		gncTaxTableMakeInvisible( tt );
	}
}

static gpointer
get_child( gpointer pObject, const QofParam* param )
{
	GncTaxTable* tt = GNC_TAXTABLE(pObject);

	g_return_val_if_fail( pObject != NULL, NULL );
	g_return_val_if_fail( GNC_IS_TAXTABLE(pObject), NULL );

	return gncTaxTableGetChild( tt );
}

static void
set_parent( gpointer data, gpointer value )
{
	GncTaxTable* tt = GNC_TAXTABLE(data);
	GncTaxTable* parent;

	g_return_if_fail( data != NULL );
	g_return_if_fail( GNC_IS_TAXTABLE(data) );

	if( value != NULL ) {
		parent = GNC_TAXTABLE(value);
		gncTaxTableSetParent( tt, parent );
	}
}

static void
load_single_ttentry( GncGdaBackend* be, GdaDataModel* pModel, int row, GncTaxTable* tt )
{
	GncTaxTableEntry* e = gncTaxTableEntryCreate();

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );
	g_return_if_fail( tt != NULL );

    gnc_gda_load_object( be, pModel, row, GNC_ID_TAXTABLE, e, ttentries_col_table );
	gncTaxTableAddEntry( tt, e );
}

static void
load_taxtable_entries( GncGdaBackend* be, GncTaxTable* tt )
{
    GdaObject* ret;
    gchar guid_buf[GUID_ENCODING_LENGTH+1];
    GdaQuery* query;
    GdaQueryCondition* cond;
    GValue value;

	g_return_if_fail( be != NULL );
	g_return_if_fail( tt != NULL );

    guid_to_string_buff( qof_instance_get_guid( QOF_INSTANCE(tt) ), guid_buf );
    memset( &value, 0, sizeof( GValue ) );
    g_value_init( &value, G_TYPE_STRING );
    g_value_set_string( &value, guid_buf );
    query = gnc_gda_create_select_query( be, TTENTRIES_TABLE_NAME );
    cond = gnc_gda_create_condition_from_field( query, "taxtable", &value );
    gda_query_set_condition( query, cond );
    g_object_unref( G_OBJECT(cond) );

    ret = gnc_gda_execute_query( be, query );
    g_object_unref( G_OBJECT(query) );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;

        for( r = 0; r < numRows; r++ ) {
            load_single_ttentry( be, pModel, r, tt );
        }
    }
}

static void
load_single_taxtable( GncGdaBackend* be, GdaDataModel* pModel, int row )
{
    const GUID* guid;
    GUID v_guid;
	GncTaxTable* tt;

	g_return_if_fail( be != NULL );
	g_return_if_fail( pModel != NULL );
	g_return_if_fail( row >= 0 );

    guid = gnc_gda_load_guid( be, pModel, row );
    v_guid = *guid;

    tt = gncTaxTableLookup( be->primary_book, &v_guid );
    if( tt == NULL ) {
        tt = gncTaxTableCreate( be->primary_book );
    }
    gnc_gda_load_object( be, pModel, row, GNC_ID_TAXTABLE, tt, tt_col_table );
    gnc_gda_slots_load( be, QOF_INSTANCE(tt) );
	load_taxtable_entries( be, tt );

    qof_instance_mark_clean( QOF_INSTANCE(tt) );
}

static void
load_all_taxtables( GncGdaBackend* be )
{
    static GdaQuery* query = NULL;
    GdaObject* ret;

	g_return_if_fail( be != NULL );

    /* First time, create the query */
    if( query == NULL ) {
        query = gnc_gda_create_select_query( be, TT_TABLE_NAME );
    }

    ret = gnc_gda_execute_query( be, query );
    if( GDA_IS_DATA_MODEL( ret ) ) {
        GdaDataModel* pModel = GDA_DATA_MODEL(ret);
        int numRows = gda_data_model_get_n_rows( pModel );
        int r;

        for( r = 0; r < numRows; r++ ) {
            load_single_taxtable( be, pModel, r );
		}
    }
}

/* ================================================================= */
static void
create_taxtable_tables( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    gnc_gda_create_table_if_needed( be, TT_TABLE_NAME, tt_col_table );
    gnc_gda_create_table_if_needed( be, TTENTRIES_TABLE_NAME, ttentries_col_table );
}

/* ================================================================= */
static void
delete_all_tt_entries( GncGdaBackend* be, const GUID* guid )
{
    guid_info_t guid_info;

	g_return_if_fail( be != NULL );
	g_return_if_fail( guid != NULL );

    guid_info.be = be;
    guid_info.guid = guid;
    (void)gnc_gda_do_db_operation( be, OP_DB_DELETE, TTENTRIES_TABLE_NAME,
                                TTENTRIES_TABLE_NAME, &guid_info, guid_col_table );
}

static void
save_tt_entries( GncGdaBackend* be, const GUID* guid, GList* entries )
{
	GList* entry;

	g_return_if_fail( be != NULL );
	g_return_if_fail( guid != NULL );

    /* First, delete the old slots for this object */
    delete_all_tt_entries( be, guid );

	for( entry = entries; entry != NULL; entry = entry->next ) {
		GncTaxTableEntry* e = (GncTaxTableEntry*)entry->data;
    	(void)gnc_gda_do_db_operation( be,
                        OP_DB_ADD_OR_UPDATE,
                        TTENTRIES_TABLE_NAME,
                        GNC_ID_TAXTABLE, e,
                        ttentries_col_table );
    }
}

static void
save_taxtable( QofInstance* inst, GncGdaBackend* be )
{
    GncTaxTable* tt = GNC_TAXTABLE(inst);
    const GUID* guid;

	g_return_if_fail( inst != NULL );
	g_return_if_fail( GNC_IS_TAXTABLE(inst) );
	g_return_if_fail( be != NULL );

    (void)gnc_gda_do_db_operation( be,
                        (qof_instance_get_destroying(inst) ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
                        TT_TABLE_NAME,
                        GNC_ID_TAXTABLE, tt,
                        tt_col_table );

    // Now, commit or delete any slots and tax table entries
    guid = qof_instance_get_guid( inst );
    if( !qof_instance_get_destroying(inst) ) {
        gnc_gda_slots_save( be, guid, qof_instance_get_slots( inst ) );
		save_tt_entries( be, guid, gncTaxTableGetEntries( tt ) );
    } else {
        gnc_gda_slots_delete( be, guid );
		delete_all_tt_entries( be, guid );
    }
}

/* ================================================================= */
static void
write_taxtables( GncGdaBackend* be )
{
	g_return_if_fail( be != NULL );

    qof_object_foreach( GNC_ID_TAXTABLE, be->primary_book, (QofInstanceForeachCB)save_taxtable, (gpointer)be );
}

/* ================================================================= */
static void
load_taxtable_guid( const GncGdaBackend* be, GdaDataModel* pModel, gint row,
            QofSetterFunc setter, gpointer pObject,
            const col_cvt_t* table_row )
{
    const GValue* val;
    GUID guid;
    const GUID* pGuid;
	GncTaxTable* taxtable = NULL;

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
		taxtable = gncTaxTableLookup( be->primary_book, pGuid );
	}
    if( table_row->gobj_param_name != NULL ) {
		g_object_set( pObject, table_row->gobj_param_name, taxtable, NULL );
    } else {
		(*setter)( pObject, (const gpointer)taxtable );
    }
}

static col_type_handler_t taxtable_guid_handler =
        { load_taxtable_guid, gnc_gda_create_objectref_guid_col,
            gnc_gda_get_gvalue_objectref_guid_for_query, gnc_gda_get_gvalue_objectref_guid_cond };
/* ================================================================= */
void
gnc_taxtable_gda_initialize( void )
{
    static GncGdaDataType_t be_data =
    {
        GNC_GDA_BACKEND_VERSION,
        GNC_ID_TAXTABLE,
        save_taxtable,						/* commit */
        load_all_taxtables,					/* initial_load */
        create_taxtable_tables,				/* create_tables */
		NULL, NULL, NULL,
		write_taxtables						/* write */
    };

    qof_object_register_backend( GNC_ID_TAXTABLE, GNC_GDA_BACKEND, &be_data );

	gnc_gda_register_col_type_handler( CT_TAXTABLEREF, &taxtable_guid_handler );
}
/* ========================== END OF FILE ===================== */

