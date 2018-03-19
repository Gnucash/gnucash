/********************************************************************\
 * gnc-entry-sql.c -- entry sql backend                             *
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

/** @file gnc-entry-sql.c
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

#include "gncEntryP.h"
#include "gncOrderP.h"
#include "gncInvoiceP.h"
#include "gncTaxTableP.h"
}

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-slots-sql.h"
#include "gnc-bill-term-sql.h"
#include "gnc-entry-sql.h"
#include "gnc-invoice-sql.h"
#include "gnc-order-sql.h"
#include "gnc-tax-table-sql.h"

#define _GNC_MOD_NAME   GNC_ID_ENTRY

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "entries"
#define TABLE_VERSION 4
#define MAX_DESCRIPTION_LEN 2048
#define MAX_ACTION_LEN 2048
#define MAX_NOTES_LEN 2048
#define MAX_DISCTYPE_LEN 2048
#define MAX_DISCHOW_LEN 2048

static void entry_set_invoice (gpointer pObject, gpointer val);
static void entry_set_bill (gpointer pObject, gpointer val);

static EntryVec col_table
({
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid"),
    gnc_sql_make_table_entry<CT_TIME64>("date", 0, COL_NNUL, ENTRY_DATE,
                                          true),
    gnc_sql_make_table_entry<CT_TIME64>("date_entered", 0, 0,
                                          ENTRY_DATE_ENTERED, true),
    gnc_sql_make_table_entry<CT_STRING>(
        "description", MAX_DESCRIPTION_LEN, 0, "description"),
    gnc_sql_make_table_entry<CT_STRING>("action", MAX_ACTION_LEN, 0,
                                        ENTRY_ACTION, true),
    gnc_sql_make_table_entry<CT_STRING>("notes", MAX_NOTES_LEN, 0, ENTRY_NOTES,
                                        true),
    gnc_sql_make_table_entry<CT_NUMERIC>("quantity", 0, 0, ENTRY_QTY,
                                         true),
    gnc_sql_make_table_entry<CT_ACCOUNTREF>("i_acct", 0, 0, ENTRY_IACCT,
                                        true),
    gnc_sql_make_table_entry<CT_NUMERIC>("i_price", 0, 0, ENTRY_IPRICE,
                                         true),
    gnc_sql_make_table_entry<CT_NUMERIC>("i_discount", 0, 0,
                                         (QofAccessFunc)gncEntryGetInvDiscount,
                                         (QofSetterFunc)gncEntrySetInvDiscount),
    gnc_sql_make_table_entry<CT_INVOICEREF>("invoice", 0, 0,
                                            (QofAccessFunc)gncEntryGetInvoice,
                                            (QofSetterFunc)entry_set_invoice),
    gnc_sql_make_table_entry<CT_STRING>("i_disc_type", MAX_DISCTYPE_LEN, 0,
                                        ENTRY_INV_DISC_TYPE, true),
    gnc_sql_make_table_entry<CT_STRING>("i_disc_how", MAX_DISCHOW_LEN, 0,
                                        ENTRY_INV_DISC_HOW, true),
    gnc_sql_make_table_entry<CT_BOOLEAN>("i_taxable", 0, 0, ENTRY_INV_TAXABLE,
                                         true),
    gnc_sql_make_table_entry<CT_BOOLEAN>("i_taxincluded", 0, 0,
                                         ENTRY_INV_TAX_INC, true),
    gnc_sql_make_table_entry<CT_TAXTABLEREF>("i_taxtable", 0, 0,
                                         (QofAccessFunc)gncEntryGetInvTaxTable,
                                         (QofSetterFunc)gncEntrySetInvTaxTable),
    gnc_sql_make_table_entry<CT_ACCOUNTREF>("b_acct", 0, 0, ENTRY_BACCT,
                                            true),
    gnc_sql_make_table_entry<CT_NUMERIC>("b_price", 0, 0, ENTRY_BPRICE,
                                         true),
    gnc_sql_make_table_entry<CT_INVOICEREF>("bill", 0, 0,
                                            (QofAccessFunc)gncEntryGetBill,
                                            (QofSetterFunc)entry_set_bill),
    gnc_sql_make_table_entry<CT_BOOLEAN>("b_taxable", 0, 0, ENTRY_BILL_TAXABLE,
                                         true),
    gnc_sql_make_table_entry<CT_BOOLEAN>("b_taxincluded", 0, 0,
                                         ENTRY_BILL_TAX_INC, true),
    gnc_sql_make_table_entry<CT_TAXTABLEREF>("b_taxtable", 0, 0,
                                        (QofAccessFunc)gncEntryGetBillTaxTable,
                                        (QofSetterFunc)gncEntrySetBillTaxTable),
    gnc_sql_make_table_entry<CT_INT>("b_paytype", 0, 0,
                                     (QofAccessFunc)gncEntryGetBillPayment,
                                     (QofSetterFunc)gncEntrySetBillPayment),
    gnc_sql_make_table_entry<CT_BOOLEAN>("billable", 0, 0, ENTRY_BILLABLE,
                                         true),
    gnc_sql_make_table_entry<CT_OWNERREF>("billto", 0, 0, ENTRY_BILLTO, true),
    gnc_sql_make_table_entry<CT_ORDERREF>("order_guid", 0, 0,
                                          (QofAccessFunc)gncEntryGetOrder,
                                          (QofSetterFunc)gncEntrySetOrder),
});

GncSqlEntryBackend::GncSqlEntryBackend() :
    GncSqlObjectBackend(TABLE_VERSION, GNC_ID_ENTRY,
                        TABLE_NAME, col_table) {}

static void
entry_set_invoice (gpointer pObject, gpointer val)
{
    GncEntry* entry;
    GncInvoice* invoice;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (GNC_IS_ENTRY (pObject));
    g_return_if_fail (val != NULL);
    g_return_if_fail (GNC_IS_INVOICE (val));

    entry = GNC_ENTRY (pObject);
    invoice = GNC_INVOICE (val);

    gncInvoiceAddEntry (invoice, entry);
}

static void
entry_set_bill (gpointer pObject, gpointer val)
{
    GncEntry* entry;
    GncInvoice* bill;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (GNC_IS_ENTRY (pObject));
    g_return_if_fail (val != NULL);
    g_return_if_fail (GNC_IS_INVOICE (val));

    entry = GNC_ENTRY (pObject);
    bill = GNC_INVOICE (val);

    gncBillAddEntry (bill, entry);
}

static GncEntry*
load_single_entry (GncSqlBackend* sql_be, GncSqlRow& row)
{
    const GncGUID* guid;
    GncEntry* pEntry;

    g_return_val_if_fail (sql_be != NULL, NULL);

    guid = gnc_sql_load_guid (sql_be, row);
    pEntry = gncEntryLookup (sql_be->book(), guid);
    if (pEntry == NULL)
    {
        pEntry = gncEntryCreate (sql_be->book());
    }
    gnc_sql_load_object (sql_be, row, GNC_ID_ENTRY, pEntry, col_table);
    qof_instance_mark_clean (QOF_INSTANCE (pEntry));

    return pEntry;
}

void
GncSqlEntryBackend::load_all (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != NULL);

    std::stringstream sql;
    sql << "SELECT * FROM " << TABLE_NAME;
    auto stmt = sql_be->create_statement_from_sql(sql.str());
    auto result = sql_be->execute_select_statement(stmt);
    InstanceVec instances;

    for (auto row : *result)
    {
        GncEntry* pEntry = load_single_entry (sql_be, row);
        if (pEntry != nullptr)
            instances.push_back(QOF_INSTANCE(pEntry));
    }

    if (!instances.empty())
        gnc_sql_slots_load_for_instancevec(sql_be, instances);
}

/* ================================================================= */
void
GncSqlEntryBackend::create_tables (GncSqlBackend* sql_be)
{
    gint version;

    g_return_if_fail (sql_be != NULL);

    version = sql_be->get_table_version( TABLE_NAME);
    if (version == 0)
    {
        sql_be->create_table(TABLE_NAME, TABLE_VERSION, col_table);
    }
    else if (version < TABLE_VERSION)
    {
        /* Upgrade:
            1->2: 64 bit int handling
            2->3: "entered" -> "date_entered", and it can be NULL
            3->4: Use DATETIME instead of TIMESTAMP in MySQL
        */
        sql_be->upgrade_table(TABLE_NAME, col_table);
        sql_be->set_table_version (TABLE_NAME, TABLE_VERSION);

        PINFO ("Entries table upgraded from version %d to version %d\n", version,
               TABLE_VERSION);
    }
}

/* ================================================================= */
static void
write_single_entry (QofInstance* term_p, gpointer data_p)
{
    write_objects_t* s = (write_objects_t*)data_p;
    GncEntry* entry = GNC_ENTRY (term_p);

    g_return_if_fail (term_p != NULL);
    g_return_if_fail (GNC_IS_ENTRY (term_p));
    g_return_if_fail (data_p != NULL);

    /* Only save if attached */
    if (s->is_ok && (gncEntryGetOrder (entry) != NULL ||
                     gncEntryGetInvoice (entry) != NULL ||
                     gncEntryGetBill (entry) != NULL))
    {
        s->commit (term_p);
    }
}

bool
GncSqlEntryBackend::write (GncSqlBackend* sql_be)
{
    g_return_val_if_fail (sql_be != NULL, FALSE);
    write_objects_t data{sql_be, true, this};

    qof_object_foreach (GNC_ID_ENTRY, sql_be->book(), write_single_entry, &data);

    return data.is_ok;
}


/* ========================== END OF FILE ===================== */
