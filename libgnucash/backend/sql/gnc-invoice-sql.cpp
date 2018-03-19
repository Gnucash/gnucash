/********************************************************************\
 * gnc-invoice-sql.c - invoice sql backend                          *
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

/** @file gnc-invoice-sql.c
 *  @brief load and save address data to SQL
 *  @author Copyright (c) 2007-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */
#include <guid.hpp>
extern "C"
{
#include <config.h>

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-commodity.h"

#include "gncBillTermP.h"
#include "gncInvoiceP.h"
}

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-commodity-sql.h"
#include "gnc-slots-sql.h"
#include "gnc-invoice-sql.h"
#include "gnc-bill-term-sql.h"

#define _GNC_MOD_NAME   GNC_ID_INVOICE

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "invoices"
#define TABLE_VERSION 4

#define MAX_ID_LEN 2048
#define MAX_NOTES_LEN 2048
#define MAX_BILLING_ID_LEN 2048

static EntryVec col_table
({
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid"),
    gnc_sql_make_table_entry<CT_STRING>("id", MAX_ID_LEN, COL_NNUL, INVOICE_ID,
                                        true),
    gnc_sql_make_table_entry<CT_TIME64>("date_opened", 0, 0, INVOICE_OPENED,
                                          true),
    gnc_sql_make_table_entry<CT_TIME64>("date_posted", 0, 0, INVOICE_POSTED,
                                          true),
    gnc_sql_make_table_entry<CT_STRING>("notes", MAX_NOTES_LEN, COL_NNUL,
                                        "notes"),
    gnc_sql_make_table_entry<CT_BOOLEAN>("active", 0, COL_NNUL,
                                         QOF_PARAM_ACTIVE, true),
    gnc_sql_make_table_entry<CT_COMMODITYREF>("currency", 0, COL_NNUL,
                                          (QofAccessFunc)gncInvoiceGetCurrency,
                                          (QofSetterFunc)gncInvoiceSetCurrency),
    gnc_sql_make_table_entry<CT_OWNERREF>("owner", 0, 0,
                                          (QofAccessFunc)gncInvoiceGetOwner,
                                          (QofSetterFunc)gncInvoiceSetOwner),
    gnc_sql_make_table_entry<CT_BILLTERMREF>("terms", 0, 0, INVOICE_TERMS,
                                             true),
    gnc_sql_make_table_entry<CT_STRING>("billing_id", MAX_BILLING_ID_LEN, 0,
                                        INVOICE_BILLINGID, true),
    gnc_sql_make_table_entry<CT_TXREF>("post_txn", 0, 0, INVOICE_POST_TXN,
                                       true),
    gnc_sql_make_table_entry<CT_LOTREF>("post_lot", 0, 0,
                                        (QofAccessFunc)gncInvoiceGetPostedLot,
                                        (QofSetterFunc)gncInvoiceSetPostedLot),
    gnc_sql_make_table_entry<CT_ACCOUNTREF>("post_acc", 0, 0, INVOICE_ACC,
                                            true),
    gnc_sql_make_table_entry<CT_OWNERREF>("billto", 0, 0,
                                          (QofAccessFunc)gncInvoiceGetBillTo,
                                          (QofSetterFunc)gncInvoiceSetBillTo),
    gnc_sql_make_table_entry<CT_NUMERIC>("charge_amt", 0, 0,
                                    (QofAccessFunc)gncInvoiceGetToChargeAmount,
                                    (QofSetterFunc)gncInvoiceSetToChargeAmount),
});

GncSqlInvoiceBackend::GncSqlInvoiceBackend() :
    GncSqlObjectBackend(TABLE_VERSION, GNC_ID_INVOICE,
                        TABLE_NAME, col_table) {}

static GncInvoice*
load_single_invoice (GncSqlBackend* sql_be, GncSqlRow& row)
{
    const GncGUID* guid;
    GncInvoice* pInvoice;

    g_return_val_if_fail (sql_be != NULL, NULL);

    guid = gnc_sql_load_guid (sql_be, row);
    pInvoice = gncInvoiceLookup (sql_be->book(), guid);
    if (pInvoice == NULL)
    {
        pInvoice = gncInvoiceCreate (sql_be->book());
    }
    gnc_sql_load_object (sql_be, row, GNC_ID_INVOICE, pInvoice, col_table);
    qof_instance_mark_clean (QOF_INSTANCE (pInvoice));

    return pInvoice;
}

void
GncSqlInvoiceBackend::load_all (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != NULL);

    std::stringstream sql;
    sql << "SELECT * FROM " << TABLE_NAME;
    auto stmt = sql_be->create_statement_from_sql(sql.str());
    auto result = sql_be->execute_select_statement(stmt);
    InstanceVec instances;

    for (auto row : *result)
    {
        GncInvoice* pInvoice = load_single_invoice (sql_be, row);
        if (pInvoice != nullptr)
            instances.push_back(QOF_INSTANCE(pInvoice));
    }

    if (!instances.empty())
        gnc_sql_slots_load_for_instancevec (sql_be, instances);
}

/* ================================================================= */
void
GncSqlInvoiceBackend::create_tables (GncSqlBackend* sql_be)
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
             2->3: invoice open date can be NULL
             3->4: Use DATETIME instead of TIMESTAMP in MySQL
        */
        sql_be->upgrade_table(TABLE_NAME, col_table);
        sql_be->set_table_version (TABLE_NAME, TABLE_VERSION);

        PINFO ("Invoices table upgraded from version %d to version %d\n", version,
               TABLE_VERSION);
    }
}

/* ================================================================= */
bool
GncSqlInvoiceBackend::commit (GncSqlBackend* sql_be, QofInstance* inst)
{
    const GncGUID* guid;
    GncInvoice* invoice;
    E_DB_OPERATION op;
    gboolean is_infant;
    gboolean is_ok = TRUE;

    g_return_val_if_fail (inst != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_INVOICE (inst), FALSE);
    g_return_val_if_fail (sql_be != NULL, FALSE);

    invoice = GNC_INVOICE (inst);

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
        is_ok = sql_be->save_commodity(gncInvoiceGetCurrency(invoice));
    }

    if (is_ok)
    {
        is_ok = sql_be->do_db_operation(op, TABLE_NAME, GNC_ID_INVOICE, inst,
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
invoice_should_be_saved (GncInvoice* invoice)
{
    const char* id;

    g_return_val_if_fail (invoice != NULL, FALSE);

    /* make sure this is a valid invoice before we save it -- should have an ID */
    id = gncInvoiceGetID (invoice);
    if (id == NULL || *id == '\0')
    {
        return FALSE;
    }

    return TRUE;
}

static void
write_single_invoice (QofInstance* term_p, gpointer data_p)
{
    auto s = reinterpret_cast<write_objects_t*>(data_p);

    g_return_if_fail (term_p != NULL);
    g_return_if_fail (GNC_IS_INVOICE (term_p));
    g_return_if_fail (data_p != NULL);

    if (s->is_ok && invoice_should_be_saved (GNC_INVOICE (term_p)))
    {
        s->commit (term_p);
    }
}

bool
GncSqlInvoiceBackend::write (GncSqlBackend* sql_be)
{
    g_return_val_if_fail (sql_be != NULL, FALSE);
    write_objects_t data{sql_be, true, this};

    qof_object_foreach (GNC_ID_INVOICE, sql_be->book(), write_single_invoice, &data);

    return data.is_ok;
}

/* ================================================================= */
template<> void
GncSqlColumnTableEntryImpl<CT_INVOICEREF>::load (const GncSqlBackend* sql_be,
                                                 GncSqlRow& row,
                                                 QofIdTypeConst obj_name,
                                                 gpointer pObject) const noexcept
{
     load_from_guid_ref(row, obj_name, pObject,
                       [sql_be](GncGUID* g){
                            return gncInvoiceLookup (sql_be->book(), g);
                        });
}

template<> void
GncSqlColumnTableEntryImpl<CT_INVOICEREF>::add_to_table(ColVec& vec) const noexcept
{
    add_objectref_guid_to_table(vec);
}

template<> void
GncSqlColumnTableEntryImpl<CT_INVOICEREF>::add_to_query(QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    add_objectref_guid_to_query(obj_name, pObject, vec);
}

/* ========================== END OF FILE ===================== */
