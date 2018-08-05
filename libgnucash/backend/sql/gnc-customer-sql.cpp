/********************************************************************\
 * gnc-customer-sql.c -- customer sql backend                       *
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

/** @file gnc-customer-sql.c
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

#include "gncBillTermP.h"
#include "gncCustomerP.h"
#include "gncTaxTableP.h"
}

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-slots-sql.h"
#include "gnc-customer-sql.h"
#include "gnc-bill-term-sql.h"
#include "gnc-tax-table-sql.h"

#define _GNC_MOD_NAME   GNC_ID_CUSTOMER

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "customers"
#define TABLE_VERSION 2

#define MAX_NAME_LEN 2048
#define MAX_ID_LEN 2048
#define MAX_NOTES_LEN 2048

static EntryVec col_table
({
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid" ),
    gnc_sql_make_table_entry<CT_STRING>("name", MAX_NAME_LEN, COL_NNUL, "name"),
    gnc_sql_make_table_entry<CT_STRING>("id", MAX_ID_LEN, COL_NNUL,
                                        CUSTOMER_ID, true),
    gnc_sql_make_table_entry<CT_STRING>("notes", MAX_NOTES_LEN, COL_NNUL,
                                        CUSTOMER_NOTES, true),
    gnc_sql_make_table_entry<CT_BOOLEAN>("active", 0, COL_NNUL,
                                         QOF_PARAM_ACTIVE, true),
    gnc_sql_make_table_entry<CT_NUMERIC>("discount", 0, COL_NNUL,
                                         CUSTOMER_DISCOUNT, true),
    gnc_sql_make_table_entry<CT_NUMERIC>("credit", 0, COL_NNUL,
                                         CUSTOMER_CREDIT, true),
    gnc_sql_make_table_entry<CT_COMMODITYREF>("currency", 0, COL_NNUL,
                                         (QofAccessFunc)gncCustomerGetCurrency,
                                         (QofSetterFunc)gncCustomerSetCurrency),
    gnc_sql_make_table_entry<CT_BOOLEAN>("tax_override", 0, COL_NNUL,
                                         CUSTOMER_TT_OVER, true),
    gnc_sql_make_table_entry<CT_ADDRESS>("addr", 0, 0, CUSTOMER_ADDR,
                                         true),
    gnc_sql_make_table_entry<CT_ADDRESS>("shipaddr", 0, 0, CUSTOMER_SHIPADDR,
                                         true),
    gnc_sql_make_table_entry<CT_BILLTERMREF>("terms", 0, 0, CUSTOMER_TERMS,
                                             true),
    gnc_sql_make_table_entry<CT_INT>("tax_included", 0, 0,
                                     (QofAccessFunc)gncCustomerGetTaxIncluded,
                                     (QofSetterFunc)gncCustomerSetTaxIncluded),
    gnc_sql_make_table_entry<CT_TAXTABLEREF>("taxtable", 0, 0,
                                         (QofAccessFunc)gncCustomerGetTaxTable,
                                         (QofSetterFunc)gncCustomerSetTaxTable),
});

GncSqlCustomerBackend::GncSqlCustomerBackend() :
    GncSqlObjectBackend(TABLE_VERSION, GNC_ID_CUSTOMER,
                        TABLE_NAME, col_table) {}

static GncCustomer*
load_single_customer (GncSqlBackend* sql_be, GncSqlRow& row)
{
    const GncGUID* guid;
    GncCustomer* pCustomer;

    g_return_val_if_fail (sql_be != NULL, NULL);

    guid = gnc_sql_load_guid (sql_be, row);
    pCustomer = gncCustomerLookup (sql_be->book(), guid);
    if (pCustomer == NULL)
    {
        pCustomer = gncCustomerCreate (sql_be->book());
    }
    gnc_sql_load_object (sql_be, row, GNC_ID_CUSTOMER, pCustomer, col_table);
    qof_instance_mark_clean (QOF_INSTANCE (pCustomer));

    return pCustomer;
}

/* Because gncCustomerLookup has the arguments backwards: */
static inline GncCustomer*
gnc_customer_lookup (const GncGUID *guid, const QofBook *book)
{
     QOF_BOOK_RETURN_ENTITY(book, guid, GNC_ID_CUSTOMER, GncCustomer);
}

void
GncSqlCustomerBackend::load_all (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != NULL);

    std::string sql("SELECT * FROM " TABLE_NAME);
    auto stmt = sql_be->create_statement_from_sql(sql);
    auto result = sql_be->execute_select_statement(stmt);

    for (auto row : *result)
        GncCustomer* pCustomer = load_single_customer (sql_be, row);

    std::string pkey(col_table[0]->name());
    sql = "SELECT DISTINCT ";
    sql += pkey + " FROM " TABLE_NAME;
    gnc_sql_slots_load_for_sql_subquery (sql_be, sql,
					 (BookLookupFn)gnc_customer_lookup);
}

/* ================================================================= */
void
GncSqlCustomerBackend::create_tables (GncSqlBackend* sql_be)
{
    gint version;

    g_return_if_fail (sql_be != NULL);

    version = sql_be->get_table_version( TABLE_NAME);
    if (version == 0)
    {
        sql_be->create_table(TABLE_NAME, TABLE_VERSION, col_table);
    }
    else if (version < m_version)
    {
        /* Upgrade 64 bit int handling */
        sql_be->upgrade_table(TABLE_NAME, col_table);
        sql_be->set_table_version (TABLE_NAME, TABLE_VERSION);

        PINFO ("Customers table upgraded from version 1 to version %d\n",
               TABLE_VERSION);
    }
}

/* ================================================================= */
static gboolean
customer_should_be_saved (GncCustomer* customer)
{
    const char* id;

    g_return_val_if_fail (customer != NULL, FALSE);

    /* Make sure this is a valid customer before we save it -- should have an ID */
    id = gncCustomerGetID (customer);
    if (id == NULL || *id == '\0')
    {
        return FALSE;
    }

    return TRUE;
}

static void
write_single_customer (QofInstance* term_p, gpointer data_p)
{
    auto data = reinterpret_cast<write_objects_t*>(data_p);

    g_return_if_fail (term_p != NULL);
    g_return_if_fail (GNC_IS_CUSTOMER (term_p));
    g_return_if_fail (data_p != NULL);

    if (customer_should_be_saved (GNC_CUSTOMER (term_p)))
    {
        data->commit (term_p);
    }
}

bool
GncSqlCustomerBackend::write (GncSqlBackend* sql_be)
{
    write_objects_t data;

    g_return_val_if_fail (sql_be != NULL, FALSE);

    data.be = sql_be;
    data.is_ok = TRUE;
    data.obe = this;
    qof_object_foreach (GNC_ID_CUSTOMER, sql_be->book(), write_single_customer,
                        (gpointer)&data);
    return data.is_ok;
}

/* ========================== END OF FILE ===================== */
