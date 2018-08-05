/********************************************************************
 * gnc-budget-sql.c: load and save data to SQL                      *
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
/** @file gnc-budget-sql.c
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
#include "Recurrence.h"
#include "gnc-budget.h"

#if defined( S_SPLINT_S )
#include "splint-defs.h"
#endif
}

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-budget-sql.h"
#include "gnc-slots-sql.h"
#include "gnc-recurrence-sql.h"

#define BUDGET_TABLE "budgets"
#define TABLE_VERSION 1
#define AMOUNTS_TABLE "budget_amounts"
#define AMOUNTS_TABLE_VERSION 1

static QofLogModule log_module = G_LOG_DOMAIN;

#define BUDGET_MAX_NAME_LEN 2048
#define BUDGET_MAX_DESCRIPTION_LEN 2048

static const EntryVec col_table
{
    gnc_sql_make_table_entry<CT_GUID>(
        "guid", 0, COL_NNUL | COL_PKEY, "guid"),
    gnc_sql_make_table_entry<CT_STRING>(
        "name", BUDGET_MAX_NAME_LEN, COL_NNUL, "name"),
    gnc_sql_make_table_entry<CT_STRING>(
        "description", BUDGET_MAX_DESCRIPTION_LEN, 0, "description"),
    gnc_sql_make_table_entry<CT_INT>(
        "num_periods", 0, COL_NNUL, "num_periods"),
};

static  QofInstance* get_budget (gpointer pObj);
static void set_budget (gpointer pObj, gpointer val);
static  QofInstance* get_account (gpointer pObj);
static void set_account (gpointer pObj, gpointer val);
static gint get_period_num (gpointer pObj);
static void set_period_num (gpointer pObj, gpointer val);
static gnc_numeric get_amount (gpointer pObj);
static void set_amount (gpointer pObj, gnc_numeric value);

GncSqlBudgetBackend::GncSqlBudgetBackend() :
    GncSqlObjectBackend(TABLE_VERSION, GNC_ID_BUDGET,
                        BUDGET_TABLE, col_table) {}

typedef struct
{
    GncBudget* budget;
    Account* account;
    guint period_num;
} budget_amount_info_t;

static const EntryVec budget_amounts_col_table
{
    gnc_sql_make_table_entry<CT_INT>(
        "id", 0, COL_NNUL | COL_PKEY | COL_AUTOINC),
    gnc_sql_make_table_entry<CT_BUDGETREF>("budget_guid",  0, COL_NNUL,
                                           (QofAccessFunc)get_budget,
                                           (QofSetterFunc)set_budget),
    gnc_sql_make_table_entry<CT_ACCOUNTREF>("account_guid", 0, COL_NNUL,
                                            (QofAccessFunc)get_account,
                                            (QofSetterFunc)set_account),
    gnc_sql_make_table_entry<CT_INT>("period_num", 0, COL_NNUL,
                                     (QofAccessFunc)get_period_num,
                                     (QofSetterFunc)set_period_num),
    gnc_sql_make_table_entry<CT_NUMERIC>("amount", 0, COL_NNUL,
                                         (QofAccessFunc)get_amount,
                                         (QofSetterFunc)set_amount),
};

/* ================================================================= */
static QofInstance*
get_budget (gpointer pObj)
{
    budget_amount_info_t* info = (budget_amount_info_t*)pObj;

    g_return_val_if_fail (pObj != NULL, NULL);

    return QOF_INSTANCE (info->budget);
}

static void
set_budget (gpointer pObj, gpointer val)
{
}

static  QofInstance*
get_account (gpointer pObj)
{
    budget_amount_info_t* info = (budget_amount_info_t*)pObj;

    g_return_val_if_fail (pObj != NULL, NULL);

    return QOF_INSTANCE (info->account);
}

static void
set_account (gpointer pObj, gpointer val)
{
    budget_amount_info_t* info = (budget_amount_info_t*)pObj;

    g_return_if_fail (pObj != NULL);
    g_return_if_fail (val != NULL);
    g_return_if_fail (GNC_IS_ACCOUNT (val));

    info->account = GNC_ACCOUNT (val);
}

static gint
get_period_num (gpointer pObj)
{
    budget_amount_info_t* info = (budget_amount_info_t*)pObj;

    g_return_val_if_fail (pObj != NULL, 0);

    return info->period_num;
}

static void
set_period_num (gpointer pObj, gpointer val)
{
    budget_amount_info_t* info = (budget_amount_info_t*)pObj;

    g_return_if_fail (pObj != NULL);

    info->period_num = GPOINTER_TO_UINT (val);
}

static gnc_numeric
get_amount (gpointer pObj)
{
    budget_amount_info_t* info = (budget_amount_info_t*)pObj;

    g_return_val_if_fail (pObj != NULL, gnc_numeric_zero ());

    return gnc_budget_get_account_period_value (info->budget, info->account,
                                                info->period_num);
}

static void
set_amount (gpointer pObj, gnc_numeric value)
{
    budget_amount_info_t* info = (budget_amount_info_t*)pObj;

    g_return_if_fail (pObj != NULL);

    gnc_budget_set_account_period_value (info->budget, info->account,
                                         info->period_num, value);
}

/*----------------------------------------------------------------*/
/**
 * Loads the budget amounts for a budget.
 *
 * @param sql_be SQL backend
 * @param budget Budget
 */
static void
load_budget_amounts (GncSqlBackend* sql_be, GncBudget* budget)
{
    gchar guid_buf[GUID_ENCODING_LENGTH + 1];

    g_return_if_fail (sql_be != NULL);
    g_return_if_fail (budget != NULL);

    (void)guid_to_string_buff (qof_instance_get_guid (QOF_INSTANCE (budget)),
                               guid_buf);
    auto sql = g_strdup_printf ("SELECT * FROM %s WHERE budget_guid='%s'",
                                AMOUNTS_TABLE, guid_buf);
    auto stmt = sql_be->create_statement_from_sql(sql);
    g_free (sql);
    if (stmt != nullptr)
    {
        auto result = sql_be->execute_select_statement(stmt);
        budget_amount_info_t info = { budget, NULL, 0 };

        for (auto row : *result)
            gnc_sql_load_object (sql_be, row, NULL, &info, budget_amounts_col_table);
    }
}

/**
 * Deletes the budget amounts for a budget.
 *
 * @param sql_be SQL backend
 * @param budget Budget
 */
static gboolean
delete_budget_amounts (GncSqlBackend* sql_be, GncBudget* budget)
{
    gchar guid_buf[GUID_ENCODING_LENGTH + 1];

    g_return_val_if_fail (sql_be != NULL, FALSE);
    g_return_val_if_fail (budget != NULL, FALSE);

    (void)guid_to_string_buff (qof_instance_get_guid (QOF_INSTANCE (budget)),
                               guid_buf);
    std::stringstream sql;
    sql << "DELETE FROM " << AMOUNTS_TABLE << " WHERE budget_guid='"<<
        guid_buf << "'";
    auto stmt = sql_be->create_statement_from_sql(sql.str());
    sql_be->execute_nonselect_statement(stmt);

    return true;
}

/**
 * Saves the budget amounts for a budget.
 *
 * @param sql_be SQL backend
 * @param budget Budget
 */
static gboolean
save_budget_amounts (GncSqlBackend* sql_be, GncBudget* budget)
{
    GList* descendants;
    GList* node;
    budget_amount_info_t info;
    guint num_periods;
    gboolean is_ok = TRUE;;

    g_return_val_if_fail (sql_be != NULL, FALSE);
    g_return_val_if_fail (budget != NULL, FALSE);

    // Delete the amounts, then save
    delete_budget_amounts (sql_be, budget);

    info.budget = budget;
    num_periods = gnc_budget_get_num_periods (budget);
    descendants = gnc_account_get_descendants (gnc_book_get_root_account (
                                                   sql_be->book()));
    for (node = descendants; node != NULL && is_ok; node = g_list_next (node))
    {
        guint i;

        info.account = GNC_ACCOUNT (node->data);
        for (i = 0; i < num_periods && is_ok; i++)
        {
            if (gnc_budget_is_account_period_value_set (budget, info.account, i))
            {
                info.period_num = i;
                is_ok = sql_be->do_db_operation(OP_DB_INSERT, AMOUNTS_TABLE,
                                                "", &info,
                                                 budget_amounts_col_table);
            }
        }
    }
    g_list_free (descendants);

    return is_ok;
}
/*----------------------------------------------------------------*/
static  GncBudget*
load_single_budget (GncSqlBackend* sql_be, GncSqlRow& row)
{
    const GncGUID* guid;
    GncBudget* pBudget = NULL;
    Recurrence* r;

    g_return_val_if_fail (sql_be != NULL, NULL);

    guid = gnc_sql_load_guid (sql_be, row);
    if (guid != NULL)
    {
        pBudget = gnc_budget_lookup (guid, sql_be->book());
    }
    if (pBudget == NULL)
    {
        pBudget = gnc_budget_new (sql_be->book());
    }

    gnc_budget_begin_edit (pBudget);
    gnc_sql_load_object (sql_be, row, GNC_ID_BUDGET, pBudget, col_table);
    load_budget_amounts (sql_be, pBudget);
    r = gnc_sql_recurrence_load (sql_be, gnc_budget_get_guid (pBudget));
    if (r != NULL)
    {
        gnc_budget_set_recurrence (pBudget, r);
        g_free (r);
    }
    gnc_budget_commit_edit (pBudget);

    return pBudget;
}

void
GncSqlBudgetBackend::load_all (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != NULL);

    std::string sql("SELECT * FROM " BUDGET_TABLE);
    auto stmt = sql_be->create_statement_from_sql(sql);
    auto result = sql_be->execute_select_statement(stmt);
    for (auto row : *result)
        auto b = load_single_budget (sql_be, row);

    std::string pkey(col_table[0]->name());
    sql = "SELECT DISTINCT ";
    sql += pkey + " FROM " BUDGET_TABLE;
    gnc_sql_slots_load_for_sql_subquery (sql_be, sql,
					 (BookLookupFn)gnc_budget_lookup);
}

/* ================================================================= */
void
GncSqlBudgetBackend::create_tables (GncSqlBackend* sql_be)
{
    gint version;

    g_return_if_fail (sql_be != NULL);

    version = sql_be->get_table_version( BUDGET_TABLE);
    if (version == 0)
    {
        (void)sql_be->create_table(BUDGET_TABLE, TABLE_VERSION, col_table);
    }

    version = sql_be->get_table_version( AMOUNTS_TABLE);
    if (version == 0)
    {
        (void)sql_be->create_table(AMOUNTS_TABLE, AMOUNTS_TABLE_VERSION,
                                    budget_amounts_col_table);
    }
}

/* ================================================================= */
bool
GncSqlBudgetBackend::commit (GncSqlBackend* sql_be, QofInstance* inst)
{
    GncBudget* pBudget = GNC_BUDGET (inst);
    const GncGUID* guid;
    E_DB_OPERATION op;
    gboolean is_infant;
    gboolean is_ok;

    g_return_val_if_fail (sql_be != NULL, FALSE);
    g_return_val_if_fail (inst != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_BUDGET (inst), FALSE);

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
    is_ok = sql_be->do_db_operation(op, BUDGET_TABLE, GNC_ID_BUDGET, pBudget,
                                    col_table);

    // Now, commit any slots and recurrence
    if (is_ok)
    {
        guid = qof_instance_get_guid (inst);
        if (!qof_instance_get_destroying (inst))
        {
            is_ok = save_budget_amounts (sql_be, pBudget);
            if (is_ok)
            {
                is_ok = gnc_sql_recurrence_save (sql_be, guid,
                                                 gnc_budget_get_recurrence (pBudget));
            }
            if (is_ok)
            {
                is_ok = gnc_sql_slots_save (sql_be, guid, is_infant, inst);
            }
        }
        else
        {
            is_ok = delete_budget_amounts (sql_be, pBudget);
            if (is_ok)
            {
                is_ok = gnc_sql_recurrence_delete (sql_be, guid);
            }
            if (is_ok)
            {
                (void)gnc_sql_slots_delete (sql_be, guid);
            }
        }
    }

    return is_ok;
}

static void
do_save (QofInstance* inst, gpointer data)
{
    write_objects_t* s = (write_objects_t*)data;

    if (s->is_ok)
    {
        s->is_ok = s->obe->commit (s->be, inst);
    }
}

bool
GncSqlBudgetBackend::write (GncSqlBackend* sql_be)
{
    write_objects_t data;

    g_return_val_if_fail (sql_be != NULL, FALSE);

    data.be = sql_be;
    data.is_ok = TRUE;
    data.obe = this;
    qof_collection_foreach (qof_book_get_collection (sql_be->book(), GNC_ID_BUDGET),
                            (QofInstanceForeachCB)do_save, &data);

    return data.is_ok;
}

/* ================================================================= */
template<> void
GncSqlColumnTableEntryImpl<CT_BUDGETREF>::load (const GncSqlBackend* sql_be,
                                                 GncSqlRow& row,
                                                 QofIdTypeConst obj_name,
                                                 gpointer pObject) const noexcept
{
    load_from_guid_ref(row, obj_name, pObject,
                       [sql_be](GncGUID* g){
                            return gnc_budget_lookup (g, sql_be->book());
                        });
}

template<> void
GncSqlColumnTableEntryImpl<CT_BUDGETREF>::add_to_table(ColVec& vec) const noexcept
{
    add_objectref_guid_to_table(vec);
}

template<> void
GncSqlColumnTableEntryImpl<CT_BUDGETREF>::add_to_query(QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    add_objectref_guid_to_query(obj_name, pObject, vec);
}

/* ========================== END OF FILE ===================== */
