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
#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-commodity.h"
#include "gncBillTermP.h"
#include "gncVendorP.h"
#include "gncTaxTableP.h"
}

#include "gnc-vendor-sql.h"
#include "gnc-address-sql.h"
#include "gnc-bill-term-sql.h"
#include "gnc-tax-table-sql.h"
#include "gnc-backend-sql.h"
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

static GncSqlColumnTableEntry col_table[] =
{
    { "guid",         CT_GUID,          0,               COL_NNUL | COL_PKEY, "guid" },
    { "name",         CT_STRING,        MAX_NAME_LEN,    COL_NNUL,            "name" },
    { "id",           CT_STRING,        MAX_ID_LEN,      COL_NNUL,            "id" },
    { "notes",        CT_STRING,        MAX_NOTES_LEN,   COL_NNUL,            "notes" },
    { "currency",     CT_COMMODITYREF,  0,               COL_NNUL,            "currency" },
    { "active",       CT_BOOLEAN,       0,               COL_NNUL,            "active" },
    { "tax_override", CT_BOOLEAN,       0,               COL_NNUL,            "tax-table-override" },
    { "addr",         CT_ADDRESS,       0,               0,                   "address" },
    { "terms",        CT_BILLTERMREF,   0,               0,                   "terms" },
    { "tax_inc",      CT_STRING,        MAX_TAX_INC_LEN, 0,                   "tax-included-string" },
    { "tax_table",    CT_TAXTABLEREF,   0,               0,                   "tax-table" },
    { NULL }
};

static GncVendor*
load_single_vendor (GncSqlBackend* be, GncSqlRow* row)
{
    const GncGUID* guid;
    GncVendor* pVendor;

    g_return_val_if_fail (be != NULL, NULL);
    g_return_val_if_fail (row != NULL, NULL);

    guid = gnc_sql_load_guid (be, row);
    pVendor = gncVendorLookup (be->book, guid);
    if (pVendor == NULL)
    {
        pVendor = gncVendorCreate (be->book);
    }
    gnc_sql_load_object (be, row, GNC_ID_VENDOR, pVendor, col_table);
    qof_instance_mark_clean (QOF_INSTANCE (pVendor));

    return pVendor;
}

static void
load_all_vendors (GncSqlBackend* be)
{
    GncSqlStatement* stmt;
    GncSqlResult* result;

    g_return_if_fail (be != NULL);

    stmt = gnc_sql_create_select_statement (be, TABLE_NAME);
    result = gnc_sql_execute_select_statement (be, stmt);
    gnc_sql_statement_dispose (stmt);
    if (result != NULL)
    {
        GncSqlRow* row;
        GList* list = NULL;

        row = gnc_sql_result_get_first_row (result);
        while (row != NULL)
        {
            GncVendor* pVendor = load_single_vendor (be, row);
            if (pVendor != NULL)
            {
                list = g_list_append (list, pVendor);
            }
            row = gnc_sql_result_get_next_row (result);
        }
        gnc_sql_result_dispose (result);

        if (list != NULL)
        {
            gnc_sql_slots_load_for_list (be, list);
            g_list_free (list);
        }
    }
}

/* ================================================================= */
static void
create_vendor_tables (GncSqlBackend* be)
{
    gint version;

    g_return_if_fail (be != NULL);

    version = gnc_sql_get_table_version (be, TABLE_NAME);
    if (version == 0)
    {
        gnc_sql_create_table (be, TABLE_NAME, TABLE_VERSION, col_table);
    }
}

/* ================================================================= */
static gboolean
save_vendor (GncSqlBackend* be, QofInstance* inst)
{
    GncVendor* v;
    const GncGUID* guid;
    E_DB_OPERATION op;
    gboolean is_infant;
    gboolean is_ok = TRUE;

    g_return_val_if_fail (inst != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_VENDOR (inst), FALSE);
    g_return_val_if_fail (be != NULL, FALSE);

    v = GNC_VENDOR (inst);

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
        is_ok = gnc_sql_save_commodity (be, gncVendorGetCurrency (v));
    }
    if (is_ok)
    {
        is_ok = gnc_sql_do_db_operation (be, op, TABLE_NAME, GNC_ID_VENDOR, v,
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
    write_objects_t* s = (write_objects_t*)data_p;

    g_return_if_fail (term_p != NULL);
    g_return_if_fail (GNC_IS_VENDOR (term_p));
    g_return_if_fail (data_p != NULL);

    if (s->is_ok && vendor_should_be_saved (GNC_VENDOR (term_p)))
    {
        s->is_ok = save_vendor (s->be, term_p);
    }
}

static gboolean
write_vendors (GncSqlBackend* be)
{
    write_objects_t data;

    g_return_val_if_fail (be != NULL, FALSE);

    data.be = be;
    data.is_ok = TRUE;
    qof_object_foreach (GNC_ID_VENDOR, be->book, write_single_vendor, &data);

    return data.is_ok;
}

/* ================================================================= */
void
gnc_vendor_sql_initialize (void)
{
    static GncSqlObjectBackend be_data =
    {
        GNC_SQL_BACKEND_VERSION,
        GNC_ID_VENDOR,
        save_vendor,                        /* commit */
        load_all_vendors,                   /* initial_load */
        create_vendor_tables,               /* create_tables */
        NULL, NULL, NULL,
        write_vendors                       /* write */
    };

    qof_object_register_backend (GNC_ID_VENDOR, GNC_SQL_BACKEND, &be_data);
}
/* ========================== END OF FILE ===================== */
