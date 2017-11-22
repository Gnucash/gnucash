/********************************************************************
 * gnc-schedxaction-sql.c: load and save data to SQL                *
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
/** @file gnc-schedxaction-sql.c
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */
extern "C"
{
#include <config.h>

#include <glib.h>

#include "qof.h"
#include "SchedXaction.h"
#include "SX-book.h"
#include "Recurrence.h"

#ifdef S_SPLINT_S
#include "splint-defs.h"
#endif
}

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-schedxaction-sql.h"
#include "gnc-slots-sql.h"
#include "gnc-recurrence-sql.h"
#include "gnc-transaction-sql.h"


#define SCHEDXACTION_TABLE "schedxactions"
#define TABLE_VERSION 1

G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

#define SX_MAX_NAME_LEN 2048

static const EntryVec col_table
({
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid"),
    gnc_sql_make_table_entry<CT_STRING>("name", SX_MAX_NAME_LEN, 0, "name"),
    gnc_sql_make_table_entry<CT_BOOLEAN>("enabled", 0, COL_NNUL, "enabled"),
    gnc_sql_make_table_entry<CT_GDATE>("start_date", 0, 0, "start-date"),
    gnc_sql_make_table_entry<CT_GDATE>("end_date", 0, 0, "end-date"),
    gnc_sql_make_table_entry<CT_GDATE>(
        "last_occur", 0, 0, "last-occurance-date"),
    gnc_sql_make_table_entry<CT_INT>(
        "num_occur", 0, COL_NNUL, "num-occurance"),
    gnc_sql_make_table_entry<CT_INT>("rem_occur", 0, COL_NNUL, "rem-occurance"),
    gnc_sql_make_table_entry<CT_BOOLEAN>(
        "auto_create", 0, COL_NNUL, "auto-create"),
    gnc_sql_make_table_entry<CT_BOOLEAN>(
        "auto_notify", 0, COL_NNUL, "auto-create-notify"),
    gnc_sql_make_table_entry<CT_INT>(
        "adv_creation", 0, COL_NNUL, "advance-creation-days"),
    gnc_sql_make_table_entry<CT_INT>(
        "adv_notify", 0, COL_NNUL, "advance-reminder-days"),
    gnc_sql_make_table_entry<CT_INT>(
        "instance_count", 0, COL_NNUL, "instance-count"),
    gnc_sql_make_table_entry<CT_ACCOUNTREF>(
        "template_act_guid", 0, COL_NNUL, "template-account"),
});

GncSqlSchedXactionBackend::GncSqlSchedXactionBackend() :
    GncSqlObjectBackend(TABLE_VERSION, GNC_ID_SCHEDXACTION,
                        SCHEDXACTION_TABLE, col_table) {}

/* ================================================================= */
static  SchedXaction*
load_single_sx (GncSqlBackend* sql_be, GncSqlRow& row)
{
    const GncGUID* guid;
    SchedXaction* pSx;
    GList* schedule;
    GDate start_date;

    g_return_val_if_fail (sql_be != NULL, NULL);

    guid = gnc_sql_load_guid (sql_be, row);
    g_assert (guid != NULL);
    pSx = xaccSchedXactionMalloc (sql_be->book());

    gnc_sx_begin_edit (pSx);
    gnc_sql_load_object (sql_be, row, GNC_SX_ID, pSx, col_table);
    schedule = gnc_sql_recurrence_load_list (sql_be, guid);
    gnc_sx_set_schedule (pSx, schedule);
    gnc_sx_commit_edit (pSx);
    gnc_sql_transaction_load_tx_for_account (sql_be, pSx->template_acct);

    g_object_get (pSx, "start-date", &start_date, NULL);

    return pSx;
}

void
GncSqlSchedXactionBackend::load_all (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != NULL);

    std::stringstream sql;
    sql << "SELECT * FROM " << SCHEDXACTION_TABLE;
    auto stmt = sql_be->create_statement_from_sql(sql.str());
    if (stmt == NULL) return;
    auto result = sql_be->execute_select_statement(stmt);
    SchedXactions* sxes;
    InstanceVec instances;
    sxes = gnc_book_get_schedxactions (sql_be->book());

    for (auto row : *result)
    {
        SchedXaction* sx;

        sx = load_single_sx (sql_be, row);
        if (sx != nullptr)
        {
            gnc_sxes_add_sx (sxes, sx);
            instances.push_back(QOF_INSTANCE(sx));
        }
    }

    if (!instances.empty())
        gnc_sql_slots_load_for_instancevec (sql_be, instances);
}


/* ================================================================= */
bool
GncSqlSchedXactionBackend::commit (GncSqlBackend* sql_be, QofInstance* inst)
{
    SchedXaction* pSx;
    const GncGUID* guid;
    E_DB_OPERATION op;
    gboolean is_infant;
    gboolean is_ok;

    g_return_val_if_fail (sql_be != NULL, FALSE);
    g_return_val_if_fail (inst != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_SX (inst), FALSE);

    pSx = GNC_SX (inst);

    is_infant = qof_instance_get_infant (inst);
    if (qof_instance_get_destroying (inst))
    {
        op = OP_DB_DELETE;
    }
    else if (sql_be->pristine() || is_infant)
    {
        op = OP_DB_INSERT;
    }
    else
    {
        op = OP_DB_UPDATE;
    }
    is_ok = sql_be->do_db_operation(op, SCHEDXACTION_TABLE, GNC_SX_ID, pSx,
                                    col_table);
    guid = qof_instance_get_guid (inst);
    if (op == OP_DB_INSERT || op == OP_DB_UPDATE)
    {
        gnc_sql_recurrence_save_list (sql_be, guid, gnc_sx_get_schedule (pSx));
    }
    else
    {
        gnc_sql_recurrence_delete (sql_be, guid);
    }

    if (is_ok)
    {
        // Now, commit any slots
        if (op == OP_DB_INSERT || op == OP_DB_UPDATE)
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

/* ========================== END OF FILE ===================== */
