/********************************************************************
 * gnc-commodity-sql.c: load and save data to SQL                   *
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
/** @file gnc-commodity-sql.c
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */
#include <guid.hpp>
extern "C"
{
#include <config.h>

#include <glib.h>

#include "qof.h"
#include "gnc-commodity.h"
}

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-commodity-sql.h"
#include "gnc-slots-sql.h"

#if defined( S_SPLINT_S )
#include "splint-defs.h"
#endif

static QofLogModule log_module = G_LOG_DOMAIN;

static  gpointer get_quote_source_name (gpointer pObject);
static void set_quote_source_name (gpointer pObject,  gpointer pValue);

#define COMMODITIES_TABLE "commodities"
#define TABLE_VERSION 1

#define COMMODITY_MAX_NAMESPACE_LEN 2048
#define COMMODITY_MAX_MNEMONIC_LEN 2048
#define COMMODITY_MAX_FULLNAME_LEN 2048
#define COMMODITY_MAX_CUSIP_LEN 2048
#define COMMODITY_MAX_QUOTESOURCE_LEN 2048
#define COMMODITY_MAX_QUOTE_TZ_LEN 2048

static const EntryVec col_table
{
    gnc_sql_make_table_entry<CT_GUID>(
        "guid", 0, COL_NNUL | COL_PKEY | COL_UNIQUE, "guid"),
    gnc_sql_make_table_entry<CT_STRING>("namespace",
                                        COMMODITY_MAX_NAMESPACE_LEN, COL_NNUL,
                                     (QofAccessFunc)gnc_commodity_get_namespace,
                                     (QofSetterFunc)gnc_commodity_set_namespace),
    gnc_sql_make_table_entry<CT_STRING>(
        "mnemonic", COMMODITY_MAX_MNEMONIC_LEN, COL_NNUL, "mnemonic"),
    gnc_sql_make_table_entry<CT_STRING>(
        "fullname", COMMODITY_MAX_FULLNAME_LEN, 0, "fullname"),
    gnc_sql_make_table_entry<CT_STRING>(
        "cusip", COMMODITY_MAX_CUSIP_LEN, 0, "cusip"),
    gnc_sql_make_table_entry<CT_INT>("fraction", 0, COL_NNUL, "fraction"),
    gnc_sql_make_table_entry<CT_BOOLEAN>(
        "quote_flag", 0, COL_NNUL, "quote_flag"),
    gnc_sql_make_table_entry<CT_STRING>("quote_source",
                                        COMMODITY_MAX_QUOTESOURCE_LEN, 0,
                                        (QofAccessFunc)get_quote_source_name,
                                        set_quote_source_name),
    gnc_sql_make_table_entry<CT_STRING>(
        "quote_tz", COMMODITY_MAX_QUOTE_TZ_LEN, 0, "quote-tz"),
};

GncSqlCommodityBackend::GncSqlCommodityBackend() :
    GncSqlObjectBackend(TABLE_VERSION, GNC_ID_COMMODITY,
                        COMMODITIES_TABLE, col_table) {}
/* ================================================================= */

static  gpointer
get_quote_source_name (gpointer pObject)
{
    const gnc_commodity* pCommodity;

    g_return_val_if_fail (pObject != NULL, NULL);
    g_return_val_if_fail (GNC_IS_COMMODITY (pObject), NULL);

    pCommodity = GNC_COMMODITY (pObject);
    return (gpointer)gnc_quote_source_get_internal_name (
               gnc_commodity_get_quote_source (pCommodity));
}

static void
set_quote_source_name (gpointer pObject, gpointer pValue)
{
    gnc_commodity* pCommodity;
    const gchar* quote_source_name = (const gchar*)pValue;
    gnc_quote_source* quote_source;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (GNC_IS_COMMODITY (pObject));

    if (pValue == NULL) return;

    pCommodity = GNC_COMMODITY (pObject);
    quote_source = gnc_quote_source_lookup_by_internal (quote_source_name);
    gnc_commodity_set_quote_source (pCommodity, quote_source);
}

static  gnc_commodity*
load_single_commodity (GncSqlBackend* sql_be, GncSqlRow& row)
{
    QofBook* pBook = sql_be->book();
    gnc_commodity* pCommodity;

    pCommodity = gnc_commodity_new (pBook, NULL, NULL, NULL, NULL, 100);
    gnc_commodity_begin_edit (pCommodity);
    gnc_sql_load_object (sql_be, row, GNC_ID_COMMODITY, pCommodity, col_table);
    gnc_commodity_commit_edit (pCommodity);

    return pCommodity;
}

void
GncSqlCommodityBackend::load_all (GncSqlBackend* sql_be)
{
    gnc_commodity_table* pTable;

    pTable = gnc_commodity_table_get_table (sql_be->book());
    std::string sql("SELECT * FROM " COMMODITIES_TABLE);
    auto stmt = sql_be->create_statement_from_sql(sql);
    auto result = sql_be->execute_select_statement(stmt);

    for (auto row : *result)
    {
        auto pCommodity = load_single_commodity (sql_be, row);

        if (pCommodity != NULL)
        {
            GncGUID guid;

            guid = *qof_instance_get_guid (QOF_INSTANCE (pCommodity));
            pCommodity = gnc_commodity_table_insert (pTable, pCommodity);
            if (qof_instance_is_dirty (QOF_INSTANCE (pCommodity)))
                sql_be->commodity_for_postload_processing(pCommodity);
            qof_instance_set_guid (QOF_INSTANCE (pCommodity), &guid);
        }

    }
    std::string pkey(col_table[0]->name());
    sql = "SELECT DISTINCT ";
    sql += pkey + " FROM " COMMODITIES_TABLE;
    gnc_sql_slots_load_for_sql_subquery (sql_be, sql,
					 (BookLookupFn)gnc_commodity_find_commodity_by_guid);
}
/* ================================================================= */
static gboolean
do_commit_commodity (GncSqlBackend* sql_be, QofInstance* inst,
                     gboolean force_insert)
{
    const GncGUID* guid;
    gboolean is_infant;
    E_DB_OPERATION op;
    gboolean is_ok;

    is_infant = qof_instance_get_infant (inst);
    if (qof_instance_get_destroying (inst))
    {
        op = OP_DB_DELETE;
    }
    else if (sql_be->pristine() || is_infant || force_insert)
    {
        op = OP_DB_INSERT;
    }
    else
    {
        op = OP_DB_UPDATE;
    }
    is_ok = sql_be->do_db_operation(op, COMMODITIES_TABLE, GNC_ID_COMMODITY,
                                    inst, col_table);

    if (is_ok)
    {
        // Now, commit any slots
        guid = qof_instance_get_guid (inst);
        if (!qof_instance_get_destroying (inst))
        {
            is_ok = gnc_sql_slots_save (sql_be, guid, is_infant, inst);
        }
        else
        {
            is_ok = gnc_sql_slots_delete (sql_be, guid);
        }
    }

    return is_ok;
}

bool
GncSqlCommodityBackend::commit (GncSqlBackend* sql_be, QofInstance* inst)
{
    g_return_val_if_fail (sql_be != NULL, FALSE);
    g_return_val_if_fail (inst != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_COMMODITY (inst), FALSE);
    auto in_be = instance_in_db(sql_be, inst);
    return do_commit_commodity (sql_be, inst, !in_be);
}

/* ----------------------------------------------------------------- */
template<> void
GncSqlColumnTableEntryImpl<CT_COMMODITYREF>::load (const GncSqlBackend* sql_be,
                                                 GncSqlRow& row,
                                                 QofIdTypeConst obj_name,
                                                 gpointer pObject) const noexcept
{
    load_from_guid_ref(row, obj_name, pObject,
                       [sql_be](GncGUID* g){
                           return gnc_commodity_find_commodity_by_guid(g, sql_be->book());
                       });
}

template<> void
GncSqlColumnTableEntryImpl<CT_COMMODITYREF>::add_to_table(ColVec& vec) const noexcept
{
    add_objectref_guid_to_table(vec);
}

template<> void
GncSqlColumnTableEntryImpl<CT_COMMODITYREF>::add_to_query(QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    add_objectref_guid_to_query(obj_name, pObject, vec);
}

/* ========================== END OF FILE ===================== */
