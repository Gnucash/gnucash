/********************************************************************\
 * gnc-coowner-sql.c -- co-owner sql implementation                 *
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
 *  @author Copyright (c) 2022 Ralf Zerres <ralf.zerres@mail.de>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database
 */
#include <glib.h>

extern "C"
{
#include <config.h>

#include <stdlib.h>
#include <string.h>

#include "gnc-commodity.h"
#include "gncBillTermP.h"
#include "gncCoOwnerP.h"
#include "gncTaxTableP.h"
}

#include "gnc-bill-term-sql.h"
#include "gnc-commodity-sql.h"
#include "gnc-coowner-sql.h"
#include "gnc-slots-sql.h"
#include "gnc-tax-table-sql.h"

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"

#define _GNC_MOD_NAME   GNC_ID_COOWNER

static QofLogModule log_module = G_LOG_DOMAIN;

#define MAX_USERNAME_LEN 2048
#define MAX_ID_LEN 2048
#define MAX_LANGUAGE_LEN 2048
#define MAX_ACL_LEN 2048
#define MAX_NAME_LEN 2048

#define TABLE_NAME "coowner"
#define TABLE_VERSION 2

static EntryVec col_table
({
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid"),
    gnc_sql_make_table_entry<CT_STRING>("id", MAX_ID_LEN, COL_NNUL, "id"),
    gnc_sql_make_table_entry<CT_STRING>("acl", MAX_ACL_LEN, COL_NNUL, "acl"),
    gnc_sql_make_table_entry<CT_BOOLEAN>("active", 0, COL_NNUL, "active"),
    gnc_sql_make_table_entry<CT_ADDRESS>("addr", 0, 0, "address"),
    gnc_sql_make_table_entry<CT_NUMERIC>("apt_share", 0, COL_NNUL, "apt_share"),
    gnc_sql_make_table_entry<CT_NUMERIC>("apt_unit", 0, COL_NNUL, "apt_unit"),
    gnc_sql_make_table_entry<CT_ACCOUNTREF>(
	"ccard_guid", 0, 0, "credit-card-account"),
    gnc_sql_make_table_entry<CT_COMMODITYREF>(
	"currency", 0, COL_NNUL, "currency"),
    gnc_sql_make_table_entry<CT_NUMERIC>(
	"distribution_key", 0, COL_NNUL, "distribution_key"),
    gnc_sql_make_table_entry<CT_STRING>(
	"language", MAX_LANGUAGE_LEN, COL_NNUL, "language"),
    gnc_sql_make_table_entry<CT_STRING>("name", MAX_NAME_LEN, COL_NNUL, "name"),
    gnc_sql_make_table_entry<CT_BOOLEAN>(
	"tax_override", 0, COL_NNUL, COOWNER_TAXTABLE_OVERRIDE, true),
    gnc_sql_make_table_entry<CT_STRING>(
	"username", MAX_USERNAME_LEN, COL_NNUL, "username"),
    gnc_sql_make_table_entry<CT_INT>(
	 "tax_included", 0, 0,
	 (QofAccessFunc)gncCoOwnerGetTaxIncluded,
	 (QofSetterFunc)gncCoOwnerSetTaxIncluded),
    gnc_sql_make_table_entry<CT_TAXTABLEREF>(
	  "taxtable", 0, 0,
	  (QofAccessFunc)gncCoOwnerGetTaxTable,
	  (QofSetterFunc)gncCoOwnerSetTaxTable),
});

GncSqlCoOwnerBackend::GncSqlCoOwnerBackend() :
    GncSqlObjectBackend(TABLE_VERSION, GNC_ID_COOWNER,
			TABLE_NAME, col_table) {}

static GncCoOwner*
load_single_coowner (GncSqlBackend* sql_be, GncSqlRow& row)
{
    const GncGUID* guid;
    GncCoOwner* pCoOwner;

    g_return_val_if_fail (sql_be != NULL, NULL);

    guid = gnc_sql_load_guid (sql_be, row);
    pCoOwner= gncCoOwnerLookup (sql_be->book(), guid);
    if (pCoOwner == NULL)
    {
	pCoOwner = gncCoOwnerCreate (sql_be->book());
    }
    gnc_sql_load_object (sql_be, row, GNC_ID_COOWNER, pCoOwner, col_table);
    qof_instance_mark_clean (QOF_INSTANCE (pCoOwner));

    return pCoOwner;
}
/* Because gncCoOwnerLookup has the arguments backwards: */
static inline GncCoOwner*
gnc_coowner_lookup (const GncGUID *guid, const QofBook *book)
{
    QOF_BOOK_RETURN_ENTITY(book, guid, GNC_ID_COOWNER, GncCoOwner);
}

void
GncSqlCoOwnerBackend::load_all (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != NULL);

    std::string sql("SELECT * FROM " TABLE_NAME);
    auto stmt = sql_be->create_statement_from_sql(sql);
    auto result = sql_be->execute_select_statement(stmt);

    for (auto row : *result)
	load_single_coowner (sql_be, row);

    std::string pkey(col_table[0]->name());
    sql = "SELECT DISTINCT ";
    sql += pkey + " FROM " TABLE_NAME;
    gnc_sql_slots_load_for_sql_subquery (sql_be, sql,
					 (BookLookupFn)gnc_coowner_lookup);
}


/* ================================================================= */
void
GncSqlCoOwnerBackend::create_tables (GncSqlBackend* sql_be)
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

	PINFO ("CoOwners table upgraded from version 1 to version %d\n",
	       TABLE_VERSION);
    }
}

/* ================================================================= */
bool
GncSqlCoOwnerBackend::commit (GncSqlBackend* sql_be, QofInstance* inst)
{
    GncCoOwner* coowner;
    const GncGUID* guid;
    E_DB_OPERATION op;
    gboolean is_infant;
    gboolean is_ok = TRUE;

    g_return_val_if_fail (inst != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_COOWNER (inst), FALSE);
    g_return_val_if_fail (sql_be != NULL, FALSE);

    coowner = GNC_COOWNER (inst);

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
	is_ok = sql_be->save_commodity(gncCoOwnerGetCurrency (coowner));
    }

    if (is_ok)
    {
	is_ok = sql_be->do_db_operation(op, TABLE_NAME, GNC_ID_COOWNER, coowner,
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
coowner_should_be_saved (GncCoOwner* coowner)
{
    const char* id;

    g_return_val_if_fail (coowner != NULL, FALSE);

    /* make sure this is a valid coowner before we save it -- should have an ID */
    id = gncCoOwnerGetID (coowner);
    if (id == NULL || *id == '\0')
    {
	return FALSE;
    }

    return TRUE;
}

static void
write_single_coowner (QofInstance* term_p, gpointer data_p)
{
    write_objects_t* s = (write_objects_t*)data_p;

    g_return_if_fail (term_p != NULL);
    g_return_if_fail (GNC_IS_COOWNER (term_p));
    g_return_if_fail (data_p != NULL);

    if (s->is_ok && coowner_should_be_saved (GNC_COOWNER (term_p)))
    {
	s->is_ok = s->obe->commit (s->be, term_p);
    }
}

bool
GncSqlCoOwnerBackend::write (GncSqlBackend* sql_be)
{
    write_objects_t data;

    g_return_val_if_fail (sql_be != NULL, FALSE);

    data.be = sql_be;
    data.is_ok = TRUE;
    data.obe = this;
    qof_object_foreach (GNC_ID_COOWNER, sql_be->book(), write_single_coowner, &data);

    return data.is_ok;
}


/* ========================== END OF FILE ===================== */
