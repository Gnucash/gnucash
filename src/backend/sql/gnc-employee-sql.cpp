/********************************************************************\
 * gnc-employee-sql.c -- employee sql implementation                *
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

/** @file gnc-employee-sql.c
 *  @brief load and save address data to SQL
 *  @author Copyright (c) 2007-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */
extern "C"
{
#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-commodity.h"
#include "gncEmployeeP.h"
}

#include "gnc-backend-sql.h"
#include "gnc-slots-sql.h"
#include "gnc-commodity-sql.h"
#include "gnc-employee-sql.h"

#define _GNC_MOD_NAME   GNC_ID_EMPLOYEE

static QofLogModule log_module = G_LOG_DOMAIN;

#define MAX_USERNAME_LEN 2048
#define MAX_ID_LEN 2048
#define MAX_LANGUAGE_LEN 2048
#define MAX_ACL_LEN 2048

#define TABLE_NAME "employees"
#define TABLE_VERSION 2

static EntryVec col_table
({
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid"),
    gnc_sql_make_table_entry<CT_STRING>(
        "username", MAX_USERNAME_LEN, COL_NNUL, "username"),
    gnc_sql_make_table_entry<CT_STRING>("id", MAX_ID_LEN, COL_NNUL, "id"),
    gnc_sql_make_table_entry<CT_STRING>(
        "language", MAX_LANGUAGE_LEN, COL_NNUL, "language"),
    gnc_sql_make_table_entry<CT_STRING>("acl", MAX_ACL_LEN, COL_NNUL, "acl"),
    gnc_sql_make_table_entry<CT_BOOLEAN>("active", 0, COL_NNUL, "active"),
    gnc_sql_make_table_entry<CT_COMMODITYREF>(
        "currency", 0, COL_NNUL, "currency"),
    gnc_sql_make_table_entry<CT_ACCOUNTREF>(
        "ccard_guid", 0, 0, "credit-card-account"),
    gnc_sql_make_table_entry<CT_NUMERIC>("workday", 0, COL_NNUL, "workday"),
    gnc_sql_make_table_entry<CT_NUMERIC>("rate", 0, COL_NNUL, "rate"),
    gnc_sql_make_table_entry<CT_ADDRESS>("addr", 0, 0, "address"),
});

class GncSqlEmployeeBackend : public GncSqlObjectBackend
{
public:
    GncSqlEmployeeBackend(int version, const std::string& type,
                      const std::string& table, const EntryVec& vec) :
        GncSqlObjectBackend(version, type, table, vec) {}
    void load_all(GncSqlBackend*) override;
    void create_tables(GncSqlBackend*) override;
    bool commit(GncSqlBackend*, QofInstance*) override;
    bool write(GncSqlBackend*) override;
};

static GncEmployee*
load_single_employee (GncSqlBackend* be, GncSqlRow& row)
{
    const GncGUID* guid;
    GncEmployee* pEmployee;

    g_return_val_if_fail (be != NULL, NULL);

    guid = gnc_sql_load_guid (be, row);
    pEmployee = gncEmployeeLookup (be->book, guid);
    if (pEmployee == NULL)
    {
        pEmployee = gncEmployeeCreate (be->book);
    }
    gnc_sql_load_object (be, row, GNC_ID_EMPLOYEE, pEmployee, col_table);
    qof_instance_mark_clean (QOF_INSTANCE (pEmployee));

    return pEmployee;
}

void
GncSqlEmployeeBackend::load_all (GncSqlBackend* be)
{
    GncSqlStatement* stmt;

    g_return_if_fail (be != NULL);

    stmt = gnc_sql_create_select_statement (be, TABLE_NAME);
    auto result = gnc_sql_execute_select_statement (be, stmt);
    delete stmt;

    GList* list = NULL;

    for (auto row : *result)
    {
        GncEmployee* pEmployee = load_single_employee (be, row);
        if (pEmployee != NULL)
        {
            list = g_list_append (list, pEmployee);
        }
    }

    if (list != NULL)
    {
        gnc_sql_slots_load_for_list (be, list);
        g_list_free (list);
    }

}

/* ================================================================= */
void
GncSqlEmployeeBackend::create_tables (GncSqlBackend* be)
{
    gint version;

    g_return_if_fail (be != NULL);

    version = gnc_sql_get_table_version (be, TABLE_NAME);
    if (version == 0)
    {
        gnc_sql_create_table (be, TABLE_NAME, TABLE_VERSION, col_table);
    }
    else if (version == 1)
    {
        /* Upgrade 64 bit int handling */
        gnc_sql_upgrade_table (be, TABLE_NAME, col_table);
        gnc_sql_set_table_version (be, TABLE_NAME, TABLE_VERSION);

        PINFO ("Employees table upgraded from version 1 to version %d\n",
               TABLE_VERSION);
    }
}

/* ================================================================= */
bool
GncSqlEmployeeBackend::commit (GncSqlBackend* be, QofInstance* inst)
{
    GncEmployee* emp;
    const GncGUID* guid;
    E_DB_OPERATION op;
    gboolean is_infant;
    gboolean is_ok = TRUE;

    g_return_val_if_fail (inst != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_EMPLOYEE (inst), FALSE);
    g_return_val_if_fail (be != NULL, FALSE);

    emp = GNC_EMPLOYEE (inst);

    is_infant = qof_instance_get_infant (inst);
    if (qof_instance_get_destroying (inst))
    {
        op = OP_DB_DELETE;
    }
    else if (be->is_pristine_db || is_infant)
    {
        op = OP_DB_INSERT;
    }
    else
    {
        op = OP_DB_UPDATE;
    }
    if (op != OP_DB_DELETE)
    {
        // Ensure the commodity is in the db
        is_ok = gnc_sql_save_commodity (be, gncEmployeeGetCurrency (emp));
    }

    if (is_ok)
    {
        is_ok = gnc_sql_do_db_operation (be, op, TABLE_NAME, GNC_ID_EMPLOYEE, emp,
                                         col_table);
    }

    if (is_ok)
    {
        // Now, commit or delete any slots
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

/* ================================================================= */
static gboolean
employee_should_be_saved (GncEmployee* employee)
{
    const char* id;

    g_return_val_if_fail (employee != NULL, FALSE);

    /* make sure this is a valid employee before we save it -- should have an ID */
    id = gncEmployeeGetID (employee);
    if (id == NULL || *id == '\0')
    {
        return FALSE;
    }

    return TRUE;
}

static void
write_single_employee (QofInstance* term_p, gpointer data_p)
{
    write_objects_t* s = (write_objects_t*)data_p;

    g_return_if_fail (term_p != NULL);
    g_return_if_fail (GNC_IS_EMPLOYEE (term_p));
    g_return_if_fail (data_p != NULL);

    if (s->is_ok && employee_should_be_saved (GNC_EMPLOYEE (term_p)))
    {
        s->is_ok = s->obe->commit (s->be, term_p);
    }
}

bool
GncSqlEmployeeBackend::write (GncSqlBackend* be)
{
    write_objects_t data;

    g_return_val_if_fail (be != NULL, FALSE);

    data.be = be;
    data.is_ok = TRUE;
    data.obe = this;
    qof_object_foreach (GNC_ID_EMPLOYEE, be->book, write_single_employee, &data);

    return data.is_ok;
}

/* ================================================================= */
void
gnc_employee_sql_initialize (void)
{
    static GncSqlEmployeeBackend be_data {
        GNC_SQL_BACKEND_VERSION, GNC_ID_EMPLOYEE, TABLE_NAME, col_table};
    gnc_sql_register_backend(&be_data);
}
/* ========================== END OF FILE ===================== */
