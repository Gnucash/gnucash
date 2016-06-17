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
#include "config.h"

#include <glib.h>

#include "qof.h"
#include "gnc-commodity.h"
}

#include "gnc-backend-sql.h"
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

class GncSqlCommodityBackend : public GncSqlObjectBackend
{
public:
    GncSqlCommodityBackend(int version, const std::string& type,
                      const std::string& table, const EntryVec& vec) :
        GncSqlObjectBackend(version, type, table, vec) {}
    void load_all(GncSqlBackend*) override;
    bool commit(GncSqlBackend*, QofInstance*) override;
};

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
load_single_commodity (GncSqlBackend* be, GncSqlRow& row)
{
    QofBook* pBook = be->book;
    gnc_commodity* pCommodity;

    pCommodity = gnc_commodity_new (pBook, NULL, NULL, NULL, NULL, 100);
    gnc_commodity_begin_edit (pCommodity);
    gnc_sql_load_object (be, row, GNC_ID_COMMODITY, pCommodity, col_table);
    gnc_commodity_commit_edit (pCommodity);

    return pCommodity;
}

void
GncSqlCommodityBackend::load_all (GncSqlBackend* be)
{
    GncSqlStatement* stmt;
    gnc_commodity_table* pTable;

    pTable = gnc_commodity_table_get_table (be->book);
    stmt = gnc_sql_create_select_statement (be, COMMODITIES_TABLE);
    if (stmt == NULL) return;
    auto result = gnc_sql_execute_select_statement (be, stmt);
    delete stmt;

    for (auto row : *result)
    {
        auto pCommodity = load_single_commodity (be, row);

        if (pCommodity != NULL)
        {
            GncGUID guid;

            guid = *qof_instance_get_guid (QOF_INSTANCE (pCommodity));
            pCommodity = gnc_commodity_table_insert (pTable, pCommodity);
            if (qof_instance_is_dirty (QOF_INSTANCE (pCommodity)))
                gnc_sql_push_commodity_for_postload_processing (be, (gpointer)pCommodity);
            qof_instance_set_guid (QOF_INSTANCE (pCommodity), &guid);
        }

        auto sql = g_strdup_printf ("SELECT DISTINCT guid FROM %s", COMMODITIES_TABLE);
        gnc_sql_slots_load_for_sql_subquery (be, sql,
                                             (BookLookupFn)gnc_commodity_find_commodity_by_guid);
        g_free (sql);
    }
}
/* ================================================================= */
static gboolean
do_commit_commodity (GncSqlBackend* be, QofInstance* inst,
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
    else if (be->is_pristine_db || is_infant || force_insert)
    {
        op = OP_DB_INSERT;
    }
    else
    {
        op = OP_DB_UPDATE;
    }
    is_ok = gnc_sql_do_db_operation (be, op, COMMODITIES_TABLE, GNC_ID_COMMODITY,
                                     inst, col_table);

    if (is_ok)
    {
        // Now, commit any slots
        guid = qof_instance_get_guid (inst);
        if (!qof_instance_get_destroying (inst))
        {
            is_ok = gnc_sql_slots_save (be, guid, is_infant, inst);
        }
        else
        {
            is_ok = gnc_sql_slots_delete (be, guid);
        }
    }

    return is_ok;
}

bool
GncSqlCommodityBackend::commit (GncSqlBackend* be, QofInstance* inst)
{
    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (inst != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_COMMODITY (inst), FALSE);

    return do_commit_commodity (be, inst, FALSE);
}

static gboolean
is_commodity_in_db (GncSqlBackend* be, gnc_commodity* pCommodity)
{
    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (pCommodity != NULL, FALSE);

    return gnc_sql_object_is_it_in_db (be, COMMODITIES_TABLE, GNC_ID_COMMODITY,
                                       pCommodity, col_table);
}

gboolean
gnc_sql_save_commodity (GncSqlBackend* be, gnc_commodity* pCommodity)
{
    gboolean is_ok = TRUE;

    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (pCommodity != NULL, FALSE);

    if (!is_commodity_in_db (be, pCommodity))
    {
        is_ok = do_commit_commodity (be, QOF_INSTANCE (pCommodity), TRUE);
    }

    return is_ok;
}

void
gnc_sql_commit_commodity (gnc_commodity* pCommodity)
{
    g_return_if_fail (pCommodity != NULL);
    g_return_if_fail (GNC_IS_COMMODITY (pCommodity));
    gnc_commodity_begin_edit (pCommodity);
    gnc_commodity_commit_edit (pCommodity);
}

/* ----------------------------------------------------------------- */
template<> void
GncSqlColumnTableEntryImpl<CT_COMMODITYREF>::load (const GncSqlBackend* be,
                                                 GncSqlRow& row,
                                                 QofIdTypeConst obj_name,
                                                 gpointer pObject) const noexcept
{
    load_from_guid_ref(row, obj_name, pObject,
                       [be](GncGUID* g){
                           return gnc_commodity_find_commodity_by_guid(g, be->book);
                       });
}

template<> void
GncSqlColumnTableEntryImpl<CT_COMMODITYREF>::add_to_table(const GncSqlBackend* be,
                                                 ColVec& vec) const noexcept
{
    add_objectref_guid_to_table(be, vec);
}

template<> void
GncSqlColumnTableEntryImpl<CT_COMMODITYREF>::add_to_query(const GncSqlBackend* be,
                                                    QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    add_objectref_guid_to_query(be, obj_name, pObject, vec);
}
/* ================================================================= */
void
gnc_sql_init_commodity_handler (void)
{
    static GncSqlCommodityBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION, GNC_ID_COMMODITY, COMMODITIES_TABLE, col_table};
    gnc_sql_register_backend(&be_data);
}
/* ========================== END OF FILE ===================== */
