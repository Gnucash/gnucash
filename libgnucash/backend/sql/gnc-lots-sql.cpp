/********************************************************************
 * gnc-lots-sql.c: load and save data to SQL                        *
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
/** @file gnc-lots-sql.c
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
#include "Account.h"
#include "gnc-lot.h"

#if defined( S_SPLINT_S )
#include "splint-defs.h"
#endif
}

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-slots-sql.h"

#include "gnc-lots-sql.h"

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "lots"
#define TABLE_VERSION 2

static  gpointer get_lot_account (gpointer pObject);
static void set_lot_account (gpointer pObject,  gpointer pValue);

static const EntryVec col_table
({
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid"),
    gnc_sql_make_table_entry<CT_ACCOUNTREF>("account_guid", 0, 0,
                                            (QofAccessFunc)get_lot_account,
                                            set_lot_account),
    gnc_sql_make_table_entry<CT_BOOLEAN>("is_closed", 0, COL_NNUL, "is-closed")
});

GncSqlLotsBackend::GncSqlLotsBackend() :
    GncSqlObjectBackend(TABLE_VERSION, GNC_ID_LOT,
                        TABLE_NAME, col_table) {}

/* ================================================================= */
static  gpointer
get_lot_account (gpointer pObject)
{
    const GNCLot* lot;
    Account* pAccount;

    g_return_val_if_fail (pObject != NULL, NULL);
    g_return_val_if_fail (GNC_IS_LOT (pObject), NULL);

    lot = GNC_LOT (pObject);
    pAccount = gnc_lot_get_account (lot);
    return pAccount;
}

static void
set_lot_account (gpointer pObject,  gpointer pValue)
{
    GNCLot* lot;
    Account* pAccount;

    g_return_if_fail (pObject != NULL && GNC_IS_LOT (pObject));
    g_return_if_fail (pValue == NULL || GNC_IS_ACCOUNT (pValue));

    lot = GNC_LOT (pObject);
    pAccount = GNC_ACCOUNT (pValue);
    if (pAccount != NULL)
    {
        xaccAccountInsertLot (pAccount, lot);
    }
}

static  GNCLot*
load_single_lot (GncSqlBackend* sql_be, GncSqlRow& row)
{
    GNCLot* lot;

    g_return_val_if_fail (sql_be != NULL, NULL);

    lot = gnc_lot_new (sql_be->book());

    gnc_lot_begin_edit (lot);
    gnc_sql_load_object (sql_be, row, GNC_ID_LOT, lot, col_table);
    gnc_lot_commit_edit (lot);

    return lot;
}

void
GncSqlLotsBackend::load_all (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != NULL);

    std::stringstream sql;
    sql << "SELECT * FROM " << TABLE_NAME;
    auto stmt = sql_be->create_statement_from_sql(sql.str());
    if (stmt != nullptr)
    {
        auto result = sql_be->execute_select_statement(stmt);
        if (result->begin () == nullptr)
            return;
        for (auto row : *result)
            load_single_lot (sql_be, row);

        auto sql = g_strdup_printf ("SELECT DISTINCT guid FROM %s",
                                   TABLE_NAME);
        gnc_sql_slots_load_for_sql_subquery (sql_be, sql, (BookLookupFn)gnc_lot_lookup);
        g_free (sql);
    }
}

/* ================================================================= */
void
GncSqlLotsBackend::create_tables (GncSqlBackend* sql_be)
{
    gint version;

    g_return_if_fail (sql_be != NULL);

    version = sql_be->get_table_version( TABLE_NAME);
    if (version == 0)
    {
        /* The table doesn't exist, so create it */
        (void)sql_be->create_table(TABLE_NAME, TABLE_VERSION, col_table);
    }
    else if (version < m_version)
    {
        /* Version 1 -> 2 removes the 'NOT NULL' constraint on the account_guid
        field.

        Create a temporary table, copy the data from the old table, delete the
        old table, then rename the new one. */

        sql_be->upgrade_table(TABLE_NAME, col_table);
        sql_be->set_table_version (TABLE_NAME, TABLE_VERSION);

        PINFO ("Lots table upgraded from version 1 to version %d\n", TABLE_VERSION);
    }
}

static void
do_save_lot (QofInstance* inst, gpointer data)
{
    auto s = reinterpret_cast<write_objects_t*>(data);

    if (s->is_ok)
    {
        s->commit (inst);
    }
}

bool
GncSqlLotsBackend::write (GncSqlBackend* sql_be)
{
    g_return_val_if_fail (sql_be != NULL, FALSE);
    write_objects_t data{sql_be, true, this};

    qof_collection_foreach (qof_book_get_collection (sql_be->book(), GNC_ID_LOT),
                            (QofInstanceForeachCB)do_save_lot, &data);
    return data.is_ok;
}

/* ================================================================= */
template<> void
GncSqlColumnTableEntryImpl<CT_LOTREF>::load (const GncSqlBackend* sql_be,
                                                 GncSqlRow& row,
                                                 QofIdTypeConst obj_name,
                                                 gpointer pObject) const noexcept
{
    load_from_guid_ref(row, obj_name, pObject,
                       [sql_be](GncGUID* g){
                           return gnc_lot_lookup(g, sql_be->book());
                       });
}

template<> void
GncSqlColumnTableEntryImpl<CT_LOTREF>::add_to_table(ColVec& vec) const noexcept
{
    add_objectref_guid_to_table(vec);
}

template<> void
GncSqlColumnTableEntryImpl<CT_LOTREF>::add_to_query(QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    add_objectref_guid_to_query(obj_name, pObject, vec);
}

/* ========================== END OF FILE ===================== */
