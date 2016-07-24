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
#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gncBillTermP.h"
#include "gncCustomerP.h"
#include "gncTaxTableP.h"
}

#include "gnc-backend-sql.h"
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

class GncSqlCustomerBackend : public GncSqlObjectBackend
{
public:
    GncSqlCustomerBackend(int version, const std::string& type,
                      const std::string& table, const EntryVec& vec) :
        GncSqlObjectBackend(version, type, table, vec) {}
    void load_all(GncSqlBackend*) override;
    void create_tables(GncSqlBackend*) override;
    bool write(GncSqlBackend*) override;
};

static GncCustomer*
load_single_customer (GncSqlBackend* be, GncSqlRow& row)
{
    const GncGUID* guid;
    GncCustomer* pCustomer;

    g_return_val_if_fail (be != NULL, NULL);

    guid = gnc_sql_load_guid (be, row);
    pCustomer = gncCustomerLookup (be->book(), guid);
    if (pCustomer == NULL)
    {
        pCustomer = gncCustomerCreate (be->book());
    }
    gnc_sql_load_object (be, row, GNC_ID_CUSTOMER, pCustomer, col_table);
    qof_instance_mark_clean (QOF_INSTANCE (pCustomer));

    return pCustomer;
}

void
GncSqlCustomerBackend::load_all (GncSqlBackend* be)
{
    g_return_if_fail (be != NULL);

    auto stmt = gnc_sql_create_select_statement (be, TABLE_NAME);
    auto result = gnc_sql_execute_select_statement (be, stmt);
    InstanceVec instances;

    for (auto row : *result)
    {
        GncCustomer* pCustomer = load_single_customer (be, row);
        if (pCustomer != nullptr)
            instances.push_back(QOF_INSTANCE(pCustomer));
    }

    if (!instances.empty())
        gnc_sql_slots_load_for_instancevec (be, instances);
}

/* ================================================================= */
void
GncSqlCustomerBackend::create_tables (GncSqlBackend* be)
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
        be->set_table_version (TABLE_NAME, TABLE_VERSION);

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
GncSqlCustomerBackend::write (GncSqlBackend* be)
{
    write_objects_t data;

    g_return_val_if_fail (be != NULL, FALSE);

    data.be = be;
    data.is_ok = TRUE;
    data.obe = this;
    qof_object_foreach (GNC_ID_CUSTOMER, be->book(), write_single_customer,
                        (gpointer)&data);
    return data.is_ok;
}

/* ================================================================= */
void
gnc_customer_sql_initialize (void)
{
    static GncSqlCustomerBackend be_data {
        GNC_SQL_BACKEND_VERSION, GNC_ID_CUSTOMER, TABLE_NAME, col_table};
    gnc_sql_register_backend(&be_data);
}
/* ========================== END OF FILE ===================== */
