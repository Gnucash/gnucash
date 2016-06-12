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
#include "config.h"

#include <glib.h>

#include "qof.h"
#include "gnc-pricedb.h"

#if defined( S_SPLINT_S )
#include "splint-defs.h"
#endif
}
#include "gnc-backend-sql.h"

#include "gnc-commodity-sql.h"
#include "gnc-price-sql.h"
#include "gnc-slots-sql.h"


static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "prices"
#define TABLE_VERSION 2

#define PRICE_MAX_SOURCE_LEN 2048
#define PRICE_MAX_TYPE_LEN 2048

static const GncSqlColumnTableEntry col_table[] =
{
    { "guid",           CT_GUID,           0,                    COL_NNUL | COL_PKEY, "guid" },
    { "commodity_guid", CT_COMMODITYREF,   0,                    COL_NNUL,          "commodity" },
    { "currency_guid",  CT_COMMODITYREF,   0,                    COL_NNUL,          "currency" },
    { "date",           CT_TIMESPEC,       0,                    COL_NNUL,          "date" },
    { "source",         CT_STRING,         PRICE_MAX_SOURCE_LEN, 0,                 "source" },
    { "type",           CT_STRING,         PRICE_MAX_TYPE_LEN,   0,                 "type" },
    { "value",          CT_NUMERIC,        0,                    COL_NNUL,          "value" },
    { NULL }
};

/* ================================================================= */

static  GNCPrice*
load_single_price (GncSqlBackend* be, GncSqlRow* row)
{
    GNCPrice* pPrice;

    g_return_val_if_fail (be != NULL, NULL);
    g_return_val_if_fail (row != NULL, NULL);

    pPrice = gnc_price_create (be->book);

    gnc_price_begin_edit (pPrice);
    gnc_sql_load_object (be, row, GNC_ID_PRICE, pPrice, col_table);
    gnc_price_commit_edit (pPrice);

    return pPrice;
}

static void
load_all_prices (GncSqlBackend* be)
{
    GncSqlStatement* stmt;
    GncSqlResult* result;
    QofBook* pBook;
    GNCPriceDB* pPriceDB;

    g_return_if_fail (be != NULL);

    pBook = be->book;
    pPriceDB = gnc_pricedb_get_db (pBook);
    stmt = gnc_sql_create_select_statement (be, TABLE_NAME);
    if (stmt != NULL)
    {
        result = gnc_sql_execute_select_statement (be, stmt);
        gnc_sql_statement_dispose (stmt);
        if (result != NULL)
        {
            GNCPrice* pPrice;
            GncSqlRow* row = gnc_sql_result_get_first_row (result);
            gchar* sql;

            gnc_pricedb_set_bulk_update (pPriceDB, TRUE);
            while (row != NULL)
            {
                pPrice = load_single_price (be, row);

                if (pPrice != NULL)
                {
                    (void)gnc_pricedb_add_price (pPriceDB, pPrice);
                    gnc_price_unref (pPrice);
                }
                row = gnc_sql_result_get_next_row (result);
            }
            gnc_sql_result_dispose (result);
            gnc_pricedb_set_bulk_update (pPriceDB, FALSE);

            sql = g_strdup_printf ("SELECT DISTINCT guid FROM %s", TABLE_NAME);
            gnc_sql_slots_load_for_sql_subquery (be, sql, (BookLookupFn)gnc_price_lookup);
            g_free (sql);
        }
    }
}

/* ================================================================= */
static void
create_prices_tables (GncSqlBackend* be)
{
    gint version;

    g_return_if_fail (be != NULL);

    version = gnc_sql_get_table_version (be, TABLE_NAME);
    if (version == 0)
    {
        (void)gnc_sql_create_table (be, TABLE_NAME, TABLE_VERSION, col_table);
    }
    else if (version == 1)
    {
        /* Upgrade 64 bit int handling */
        gnc_sql_upgrade_table (be, TABLE_NAME, col_table);
        (void)gnc_sql_set_table_version (be, TABLE_NAME, TABLE_VERSION);

        PINFO ("Prices table upgraded from version 1 to version %d\n", TABLE_VERSION);
    }
}

/* ================================================================= */

static gboolean
save_price (GncSqlBackend* be, QofInstance* inst)
{
    GNCPrice* pPrice = GNC_PRICE (inst);
    E_DB_OPERATION op;
    gboolean is_infant;
    gboolean is_ok = TRUE;

    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (inst != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_PRICE (inst), FALSE);

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
        /* Ensure commodity and currency are in the db */
        (void)gnc_sql_save_commodity (be, gnc_price_get_commodity (pPrice));
        is_ok = gnc_sql_save_commodity (be, gnc_price_get_currency (pPrice));
    }

    if (is_ok)
    {
        is_ok = gnc_sql_do_db_operation (be, op, TABLE_NAME, GNC_ID_PRICE, pPrice,
                                         col_table);
    }

    return is_ok;
}

static gboolean
write_price (GNCPrice* p, gpointer data)
{
    write_objects_t* s = (write_objects_t*)data;

    g_return_val_if_fail (p != NULL, FALSE);
    g_return_val_if_fail (data != NULL, FALSE);

    if (s->is_ok && gnc_price_get_source (p) != PRICE_SOURCE_TEMP)
    {
        s->is_ok = save_price (s->be, QOF_INSTANCE (p));
    }

    return s->is_ok;
}

static gboolean
write_prices (GncSqlBackend* be)
{
    GNCPriceDB* priceDB;
    write_objects_t data;

    g_return_val_if_fail (be != NULL, FALSE);

    priceDB = gnc_pricedb_get_db (be->book);

    data.be = be;
    data.is_ok = TRUE;
    return gnc_pricedb_foreach_price (priceDB, write_price, &data, TRUE);
}

/* ================================================================= */
void
gnc_sql_init_price_handler (void)
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_PRICE,
        save_price,                 /* commit */
        load_all_prices,            /* initial_load */
        create_prices_tables,       /* create tables */
        NULL, NULL, NULL,
        write_prices                /* write */
    };

    (void)qof_object_register_backend (GNC_ID_PRICE, GNC_SQL_BACKEND, &be_data);
}

/* ========================== END OF FILE ===================== */
