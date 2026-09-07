/********************************************************************
 * gnc-reconciled-balance-sql.cpp: load and save data to SQL          *
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
/** @file gnc-reconciled-balance-sql.cpp
 *  @brief load and save reconciled balances to SQL
 */

#include <guid.hpp>
#include <config.h>

#include <glib.h>

#include "qof.h"
#include "Account.h"
#include "gnc-reconciled-balance.h"

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-slots-sql.h"

#include "gnc-reconciled-balance-sql.h"

[[maybe_unused]] static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "reconciled_balances"
#define TABLE_VERSION 1

#define RB_MAX_NOTES_LEN 2048

static const EntryVec col_table
({
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid"),
    /* Not COL_NNUL: a record outlives the deletion of its account, and
     * the writer omits the column entirely for a null reference. */
    gnc_sql_make_table_entry<CT_ACCOUNTREF>("account_guid", 0, 0, "account"),
    gnc_sql_make_table_entry<CT_TIME>("date", 0, COL_NNUL, "date"),
    gnc_sql_make_table_entry<CT_NUMERIC>("amount", 0, COL_NNUL, "amount"),
    gnc_sql_make_table_entry<CT_STRING>("notes", RB_MAX_NOTES_LEN, 0, "notes"),
});

GncSqlReconciledBalanceBackend::GncSqlReconciledBalanceBackend() :
    GncSqlObjectBackend(TABLE_VERSION, GNC_ID_RECONCILED_BALANCE,
                        TABLE_NAME, col_table) {}

/* ================================================================= */
static GncReconciledBalance*
load_single_reconciled_balance (GncSqlBackend* sql_be, GncSqlRow& row)
{
    g_return_val_if_fail (sql_be != NULL, NULL);

    auto ba = gnc_reconciled_balance_new (sql_be->book());

    gnc_reconciled_balance_begin_edit (ba);
    gnc_sql_load_object (sql_be, row, GNC_ID_RECONCILED_BALANCE, ba, col_table);
    gnc_reconciled_balance_commit_edit (ba);

    return ba;
}

void
GncSqlReconciledBalanceBackend::load_all (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != NULL);

    std::stringstream sql;
    sql << "SELECT * FROM " << TABLE_NAME;
    auto stmt = sql_be->create_statement_from_sql (sql.str());
    if (stmt == nullptr)
        return;

    auto result = sql_be->execute_select_statement (stmt);
    if (result->begin () == nullptr)
        return;

    for (auto row : *result)
        load_single_reconciled_balance (sql_be, row);

    auto subquery = g_strdup_printf ("SELECT DISTINCT guid FROM %s", TABLE_NAME);
    gnc_sql_slots_load_for_sql_subquery (sql_be, subquery,
                                         (BookLookupFn)gnc_reconciled_balance_lookup);
    g_free (subquery);
}

/* ================================================================= */
void
GncSqlReconciledBalanceBackend::create_tables (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != NULL);

    auto version = sql_be->get_table_version (TABLE_NAME);
    if (version == 0)
        (void)sql_be->create_table (TABLE_NAME, TABLE_VERSION, col_table);
}

static void
do_save_reconciled_balance (QofInstance* inst, gpointer data)
{
    auto s = reinterpret_cast<write_objects_t*>(data);

    if (s->is_ok)
        s->commit (inst);
}

bool
GncSqlReconciledBalanceBackend::write (GncSqlBackend* sql_be)
{
    g_return_val_if_fail (sql_be != NULL, FALSE);
    write_objects_t data{sql_be, true, this};

    qof_collection_foreach (qof_book_get_collection (sql_be->book(),
                                                     GNC_ID_RECONCILED_BALANCE),
                            (QofInstanceForeachCB)do_save_reconciled_balance,
                            &data);
    return data.is_ok;
}

/* ========================== END OF FILE ===================== */
