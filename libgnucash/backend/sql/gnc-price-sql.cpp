/********************************************************************
 * gnc-price-sql.c: load and save data to SQL                       *
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
/** @file gnc-price-sql.c
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2009 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */
extern "C"
{
#include <config.h>

#include <glib.h>

#include "qof.h"
#include "gnc-pricedb.h"

#if defined( S_SPLINT_S )
#include "splint-defs.h"
#endif
}

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-commodity-sql.h"
#include "gnc-price-sql.h"
#include "gnc-slots-sql.h"


static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "prices"
#define TABLE_VERSION 3

#define PRICE_MAX_SOURCE_LEN 2048
#define PRICE_MAX_TYPE_LEN 2048

static const EntryVec col_table
({
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid"),
    gnc_sql_make_table_entry<CT_COMMODITYREF>("commodity_guid", 0, COL_NNUL,
                                              "commodity"),
    gnc_sql_make_table_entry<CT_COMMODITYREF>("currency_guid", 0, COL_NNUL,
                                              "currency"),
    gnc_sql_make_table_entry<CT_TIMESPEC>("date", 0, COL_NNUL, "date"),
    gnc_sql_make_table_entry<CT_STRING>("source", PRICE_MAX_SOURCE_LEN, 0,
                                        "source"),
    gnc_sql_make_table_entry<CT_STRING>("type", PRICE_MAX_TYPE_LEN, 0, "type"),
    gnc_sql_make_table_entry<CT_NUMERIC>("value", 0, COL_NNUL, "value")
});

GncSqlPriceBackend::GncSqlPriceBackend() :
    GncSqlObjectBackend(TABLE_VERSION, GNC_ID_PRICE,
                        TABLE_NAME, col_table) {}

/* ================================================================= */

static  GNCPrice*
load_single_price (GncSqlBackend* sql_be, GncSqlRow& row)
{
    GNCPrice* pPrice;

    g_return_val_if_fail (sql_be != NULL, NULL);

    pPrice = gnc_price_create (sql_be->book());

    gnc_price_begin_edit (pPrice);
    gnc_sql_load_object (sql_be, row, GNC_ID_PRICE, pPrice, col_table);
    gnc_price_commit_edit (pPrice);

    return pPrice;
}

void
GncSqlPriceBackend::load_all (GncSqlBackend* sql_be)
{
    QofBook* pBook;
    GNCPriceDB* pPriceDB;

    g_return_if_fail (sql_be != NULL);

    pBook = sql_be->book();
    pPriceDB = gnc_pricedb_get_db (pBook);
    std::stringstream sql;
    sql << "SELECT * FROM " << TABLE_NAME;
    auto stmt = sql_be->create_statement_from_sql(sql.str());
    if (stmt != nullptr)
    {
        auto result = sql_be->execute_select_statement(stmt);
        if (result->begin() == result->end())
            return;

        GNCPrice* pPrice;
        gchar* sql;

        gnc_pricedb_set_bulk_update (pPriceDB, TRUE);
        for (auto row : *result)
        {
            pPrice = load_single_price (sql_be, row);

            if (pPrice != NULL)
            {
                (void)gnc_pricedb_add_price (pPriceDB, pPrice);
                gnc_price_unref (pPrice);
            }
        }
        gnc_pricedb_set_bulk_update (pPriceDB, FALSE);

        sql = g_strdup_printf ("SELECT DISTINCT guid FROM %s", TABLE_NAME);
        gnc_sql_slots_load_for_sql_subquery (sql_be, sql, (BookLookupFn)gnc_price_lookup);
        g_free (sql);
    }
}

/* ================================================================= */
void
GncSqlPriceBackend::create_tables (GncSqlBackend* sql_be)
{
    gint version;

    g_return_if_fail (sql_be != NULL);

    version = sql_be->get_table_version( TABLE_NAME);
    if (version == 0)
    {
        (void)sql_be->create_table(TABLE_NAME, TABLE_VERSION, col_table);
    }
    else if (version < m_version)
    {
        /*
          1->2: Upgrade 64 bit int handling
          2->3: Use DATETIME instead of TIMESTAMP in MySQL
        */
        sql_be->upgrade_table(TABLE_NAME, col_table);
        sql_be->set_table_version (TABLE_NAME, TABLE_VERSION);

        PINFO ("Prices table upgraded from version 1 to version %d\n", TABLE_VERSION);
    }
}

/* ================================================================= */

bool
GncSqlPriceBackend::commit (GncSqlBackend* sql_be, QofInstance* inst)
{
    GNCPrice* pPrice = GNC_PRICE (inst);
    E_DB_OPERATION op;
    gboolean is_infant;
    gboolean is_ok = TRUE;

    g_return_val_if_fail (sql_be != NULL, FALSE);
    g_return_val_if_fail (inst != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_PRICE (inst), FALSE);

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
        /* Ensure commodity and currency are in the db */
        (void)sql_be->save_commodity(gnc_price_get_commodity(pPrice));
        is_ok = sql_be->save_commodity(gnc_price_get_currency(pPrice));
    }

    if (is_ok)
    {
        is_ok = sql_be->do_db_operation(op, TABLE_NAME, GNC_ID_PRICE, pPrice,
                                        col_table);
    }

    return is_ok;
}

static gboolean
write_price (GNCPrice* p, gpointer data)
{
    auto s = reinterpret_cast<write_objects_t*>(data);

    g_return_val_if_fail (p != NULL, FALSE);
    g_return_val_if_fail (data != NULL, FALSE);

    if (s->is_ok && gnc_price_get_source (p) != PRICE_SOURCE_TEMP)
    {
        s->commit (QOF_INSTANCE(p));
    }

    return s->is_ok;
}

bool
GncSqlPriceBackend::write (GncSqlBackend* sql_be)
{
    g_return_val_if_fail (sql_be != NULL, FALSE);
    write_objects_t data{sql_be, true, this};

    auto priceDB = gnc_pricedb_get_db (sql_be->book());
    return gnc_pricedb_foreach_price (priceDB, write_price, &data, TRUE);
}

/* ========================== END OF FILE ===================== */
