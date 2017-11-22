/********************************************************************\
 * gnc-vendor-sql.c -- vendor sql backend                           *
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

/** @file gnc-vendor-sql.c
 *  @brief load and save address data to SQL
 *  @author Copyright (c) 2007-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */
extern "C"
{
#include <config.h>

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-commodity.h"
#include "gncBillTermP.h"
#include "gncVendorP.h"
#include "gncTaxTableP.h"
}

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-vendor-sql.h"
#include "gnc-bill-term-sql.h"
#include "gnc-tax-table-sql.h"
#include "gnc-commodity-sql.h"
#include "gnc-slots-sql.h"

#define _GNC_MOD_NAME   GNC_ID_VENDOR

G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

#define MAX_NAME_LEN 2048
#define MAX_ID_LEN 2048
#define MAX_NOTES_LEN 2048
#define MAX_TAX_INC_LEN 2048

#define TABLE_NAME "vendors"
#define TABLE_VERSION 1

static EntryVec col_table
({
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid"),
    gnc_sql_make_table_entry<CT_STRING>("name", MAX_NAME_LEN, COL_NNUL, "name"),
    gnc_sql_make_table_entry<CT_STRING>("id", MAX_ID_LEN, COL_NNUL, "id"),
    gnc_sql_make_table_entry<CT_STRING>("notes", MAX_NOTES_LEN, COL_NNUL,
                                        "notes"),
    gnc_sql_make_table_entry<CT_COMMODITYREF>("currency", 0, COL_NNUL,
                                              "currency"),
    gnc_sql_make_table_entry<CT_BOOLEAN>("active", 0, COL_NNUL, "active"),
    gnc_sql_make_table_entry<CT_BOOLEAN>("tax_override", 0, COL_NNUL,
                                         "tax-table-override"),
    gnc_sql_make_table_entry<CT_ADDRESS>("addr", 0, 0, "address"),
    gnc_sql_make_table_entry<CT_BILLTERMREF>("terms", 0, 0, "terms"),
    gnc_sql_make_table_entry<CT_STRING>("tax_inc", MAX_TAX_INC_LEN, 0,
                                        "tax-included-string"),
    gnc_sql_make_table_entry<CT_TAXTABLEREF>("tax_table", 0, 0, "tax-table"),
});

GncSqlVendorBackend::GncSqlVendorBackend() :
    GncSqlObjectBackend(TABLE_VERSION, GNC_ID_VENDOR,
                        TABLE_NAME, col_table) {}

static GncVendor*
load_single_vendor (GncSqlBackend* sql_be, GncSqlRow& row)
{
    const GncGUID* guid;
    GncVendor* pVendor;

    g_return_val_if_fail (sql_be != NULL, NULL);

    guid = gnc_sql_load_guid (sql_be, row);
    pVendor = gncVendorLookup (sql_be->book(), guid);
    if (pVendor == NULL)
    {
        pVendor = gncVendorCreate (sql_be->book());
    }
    gnc_sql_load_object (sql_be, row, GNC_ID_VENDOR, pVendor, col_table);
    qof_instance_mark_clean (QOF_INSTANCE (pVendor));

    return pVendor;
}

void
GncSqlVendorBackend::load_all (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != NULL);

    std::stringstream sql;
    sql << "SELECT * FROM " << TABLE_NAME;
    auto stmt = sql_be->create_statement_from_sql(sql.str());
    auto result = sql_be->execute_select_statement(stmt);
    InstanceVec instances;

    for (auto row : *result)
    {
        GncVendor* pVendor = load_single_vendor (sql_be, row);
        if (pVendor != nullptr)
            instances.push_back(QOF_INSTANCE(pVendor));
    }

    if (!instances.empty())
        gnc_sql_slots_load_for_instancevec (sql_be, instances);
}

/* ================================================================= */
bool
GncSqlVendorBackend::commit (GncSqlBackend* sql_be, QofInstance* inst)
{
    GncVendor* v;
    const GncGUID* guid;
    E_DB_OPERATION op;
    gboolean is_infant;
    gboolean is_ok = TRUE;

    g_return_val_if_fail (inst != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_VENDOR (inst), FALSE);
    g_return_val_if_fail (sql_be != NULL, FALSE);

    v = GNC_VENDOR (inst);

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
    if (op != OP_DB_DELETE)
    {
        // Ensure the commodity is in the db
        is_ok = sql_be->save_commodity (gncVendorGetCurrency(v));
    }
    if (is_ok)
    {
        is_ok = sql_be->do_db_operation(op, TABLE_NAME, GNC_ID_VENDOR, v,
                                        col_table);
    }

    if (is_ok)
    {
        // Now, commit or delete any slots
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

/* ================================================================= */
static gboolean
vendor_should_be_saved (GncVendor* vendor)
{
    const char* id;

    g_return_val_if_fail (vendor != NULL, FALSE);

    /* make sure this is a valid vendor before we save it -- should have an ID */
    id = gncVendorGetID (vendor);
    if (id == NULL || *id == '\0')
    {
        return FALSE;
    }

    return TRUE;
}

static void
write_single_vendor (QofInstance* term_p, gpointer data_p)
{
    auto s = reinterpret_cast<write_objects_t*>(data_p);

    g_return_if_fail (term_p != NULL);
    g_return_if_fail (GNC_IS_VENDOR (term_p));
    g_return_if_fail (data_p != NULL);

    if (vendor_should_be_saved (GNC_VENDOR (term_p)))
    {
        s->commit (term_p);
    }
}

bool
GncSqlVendorBackend::write (GncSqlBackend* sql_be)
{
    g_return_val_if_fail (sql_be != NULL, FALSE);
    write_objects_t data{sql_be, true, this};

    qof_object_foreach (GNC_ID_VENDOR, sql_be->book(), write_single_vendor, &data);

    return data.is_ok;
}

/* ========================== END OF FILE ===================== */
