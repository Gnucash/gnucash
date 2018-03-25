/********************************************************************\
 * gnc-order-sql.c -- order sql backend                             *
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

/** @file gnc-order-sql.c
 *  @brief load and save address data to SQL
 *  @author Copyright (c) 2007-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */

#include <guid.hpp>
extern "C"
{
#include <config.h>

#include <glib.h>
#include <stdlib.h>
#include <string.h>
#include "gncOrderP.h"
}

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-slots-sql.h"
#include "gnc-order-sql.h"

#define _GNC_MOD_NAME   GNC_ID_ORDER

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "orders"
#define TABLE_VERSION 1

#define MAX_ID_LEN 2048
#define MAX_NOTES_LEN 2048
#define MAX_REFERENCE_LEN 2048

static EntryVec col_table
({
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid"),
    gnc_sql_make_table_entry<CT_STRING>("id", MAX_ID_LEN, COL_NNUL, "id"),
    gnc_sql_make_table_entry<CT_STRING>("notes", MAX_NOTES_LEN, COL_NNUL,
                                        "notes"),
    gnc_sql_make_table_entry<CT_STRING>(
        "reference", MAX_REFERENCE_LEN, COL_NNUL, "reference"),
    gnc_sql_make_table_entry<CT_BOOLEAN>("active", 0, COL_NNUL, "order"),
    gnc_sql_make_table_entry<CT_TIMESPEC>("date_opened", 0, COL_NNUL,
                                          "date-opened"),
    gnc_sql_make_table_entry<CT_TIMESPEC>("date_closed", 0, COL_NNUL,
                                          "date-closed"),
    gnc_sql_make_table_entry<CT_OWNERREF>("owner", 0, COL_NNUL,
                                          ORDER_OWNER, true),
});

GncSqlOrderBackend::GncSqlOrderBackend() :
    GncSqlObjectBackend(TABLE_VERSION, GNC_ID_ORDER,
                        TABLE_NAME, col_table) {}

static GncOrder*
load_single_order (GncSqlBackend* sql_be, GncSqlRow& row)
{
    const GncGUID* guid;
    GncOrder* pOrder;

    g_return_val_if_fail (sql_be != NULL, NULL);

    guid = gnc_sql_load_guid (sql_be, row);
    pOrder = gncOrderLookup (sql_be->book(), guid);
    if (pOrder == NULL)
    {
        pOrder = gncOrderCreate (sql_be->book());
    }
    gnc_sql_load_object (sql_be, row, GNC_ID_ORDER, pOrder, col_table);
    qof_instance_mark_clean (QOF_INSTANCE (pOrder));

    return pOrder;
}

/* Because gncOrderLookup has the arguments backwards: */
static inline GncOrder*
gnc_order_lookup (const GncGUID *guid, const QofBook *book)
{
     QOF_BOOK_RETURN_ENTITY(book, guid, GNC_ID_ORDER, GncOrder);
}

void
GncSqlOrderBackend::load_all (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != NULL);

    std::string sql("SELECT * FROM " TABLE_NAME);
    auto stmt = sql_be->create_statement_from_sql(sql);
    auto result = sql_be->execute_select_statement(stmt);

    for (auto row : *result)
        GncOrder* pOrder = load_single_order (sql_be, row);

    std::string pkey(col_table[0]->name());
    sql = "SELECT DISTINCT ";
    sql += pkey + " FROM " TABLE_NAME;
    gnc_sql_slots_load_for_sql_subquery (sql_be, sql,
					 (BookLookupFn)gnc_order_lookup);
}

/* ================================================================= */
static gboolean
order_should_be_saved (GncOrder* order)
{
    const char* id;

    g_return_val_if_fail (order != NULL, FALSE);

    /* make sure this is a valid order before we save it -- should have an ID */
    id = gncOrderGetID (order);
    if (id == NULL || *id == '\0')
    {
        return FALSE;
    }

    return TRUE;
}

static void
write_single_order (QofInstance* term_p, gpointer data_p)
{
    auto s = reinterpret_cast<write_objects_t*>(data_p);

    g_return_if_fail (term_p != NULL);
    g_return_if_fail (GNC_IS_ORDER (term_p));
    g_return_if_fail (data_p != NULL);

    if (s->is_ok && order_should_be_saved (GNC_ORDER (term_p)))
    {
        s->commit (term_p);
    }
}

bool
GncSqlOrderBackend::write (GncSqlBackend* sql_be)
{
    g_return_val_if_fail (sql_be != NULL, FALSE);
    write_objects_t data{sql_be, true, this};

    qof_object_foreach (GNC_ID_ORDER, sql_be->book(), write_single_order, &data);

    return data.is_ok;
}

/* ================================================================= */
template<> void
GncSqlColumnTableEntryImpl<CT_ORDERREF>::load (const GncSqlBackend* sql_be,
                                                 GncSqlRow& row,
                                                 QofIdTypeConst obj_name,
                                                 gpointer pObject) const noexcept
{
    load_from_guid_ref(row, obj_name, pObject,
                       [sql_be](GncGUID* g){
                           return gncOrderLookup(sql_be->book(), g);
                       });
}

template<> void
GncSqlColumnTableEntryImpl<CT_ORDERREF>::add_to_table(ColVec& vec) const noexcept
{
    add_objectref_guid_to_table(vec);
}

template<> void
GncSqlColumnTableEntryImpl<CT_ORDERREF>::add_to_query(QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    add_objectref_guid_to_query(obj_name, pObject, vec);
}

/* ========================== END OF FILE ===================== */
