/********************************************************************\
 * gnc-tax-table-sql.c -- tax table sql implementation              *
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

/** @file gnc-tax-table-sql.c
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

#include "gncEntry.h"
#include "gncTaxTableP.h"
}

#include <string>
#include <vector>
#include <algorithm>

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-slots-sql.h"
#include "gnc-tax-table-sql.h"

#define _GNC_MOD_NAME   GNC_ID_TAXTABLE

static QofLogModule log_module = G_LOG_DOMAIN;

typedef struct
{
    GncSqlBackend* be;
    const GncGUID* guid;
} guid_info_t;

static gpointer get_obj_guid (gpointer pObject, const QofParam* param);
static void set_obj_guid (gpointer pObject, gpointer pValue);
static gpointer bt_get_parent (gpointer pObject);
static void tt_set_parent (gpointer pObject, gpointer pValue);
static void tt_set_parent_guid (gpointer pObject, gpointer pValue);

#define MAX_NAME_LEN 50

#define TT_TABLE_NAME "taxtables"
#define TT_TABLE_VERSION 2

static EntryVec tt_col_table
({
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid" ),
    gnc_sql_make_table_entry<CT_STRING>("name", MAX_NAME_LEN, COL_NNUL, "name" ),
    gnc_sql_make_table_entry<CT_INT64>("refcount", 0, COL_NNUL, "ref-count" ),
    gnc_sql_make_table_entry<CT_BOOLEAN>("invisible", 0, COL_NNUL, "invisible" ),
    /*  gnc_sql_make_table_entry<CT_TAXTABLEREF>("child", 0, 0,
                get_child, (QofSetterFunc)gncTaxTableSetChild ), */
    gnc_sql_make_table_entry<CT_GUID>("parent", 0, 0,
        (QofAccessFunc)bt_get_parent, tt_set_parent
    ),
});

static EntryVec tt_parent_col_table
({
    gnc_sql_make_table_entry<CT_GUID>("parent", 0, 0, nullptr,
                                      tt_set_parent_guid ),
});

#define TTENTRIES_TABLE_NAME "taxtable_entries"
#define TTENTRIES_TABLE_VERSION 3

static EntryVec ttentries_col_table
({
    gnc_sql_make_table_entry<CT_INT>(
        "id", 0, COL_PKEY | COL_NNUL | COL_AUTOINC),
    gnc_sql_make_table_entry<CT_TAXTABLEREF>("taxtable", 0, COL_NNUL,
                                        (QofAccessFunc)gncTaxTableEntryGetTable,
                                             set_obj_guid),
    gnc_sql_make_table_entry<CT_ACCOUNTREF>("account", 0, COL_NNUL,
                                     (QofAccessFunc)gncTaxTableEntryGetAccount,
                                     (QofSetterFunc)gncTaxTableEntrySetAccount),
    gnc_sql_make_table_entry<CT_NUMERIC>("amount", 0, COL_NNUL,
                                      (QofAccessFunc)gncTaxTableEntryGetAmount,
                                      (QofSetterFunc)gncTaxTableEntrySetAmount),
    gnc_sql_make_table_entry<CT_INT>("type", 0, COL_NNUL,
                                     (QofAccessFunc)gncTaxTableEntryGetType,
                                     (QofSetterFunc)gncTaxTableEntrySetType),
});

/* Special column table because we need to be able to access the table by
a column other than the primary key */
static EntryVec guid_col_table
({
    gnc_sql_make_table_entry<CT_GUID>("taxtable", 0, 0,
                                      get_obj_guid, set_obj_guid),
});

GncSqlTaxTableBackend::GncSqlTaxTableBackend() :
    GncSqlObjectBackend(TT_TABLE_VERSION, GNC_ID_TAXTABLE,
                        TT_TABLE_NAME, tt_col_table) {}

struct TaxTblParentGuid
{
    GncTaxTable* tt;
    GncGUID guid;
    bool have_guid;
};

using TaxTblParentGuidPtr = TaxTblParentGuid*;
using TaxTblParentGuidVec = std::vector<TaxTblParentGuidPtr>;

static gpointer
get_obj_guid (gpointer pObject, const QofParam* param)
{
    guid_info_t* pInfo = (guid_info_t*)pObject;

    g_return_val_if_fail (pInfo != NULL, NULL);

    return (gpointer)pInfo->guid;
}

static void
set_obj_guid (gpointer pObject, gpointer pValue)
{
    // Nowhere to put the GncGUID
}

static  gpointer
bt_get_parent (gpointer pObject)
{
    const GncTaxTable* tt;
    const GncTaxTable* pParent;
    const GncGUID* parent_guid;

    g_return_val_if_fail (pObject != NULL, NULL);
    g_return_val_if_fail (GNC_IS_TAXTABLE (pObject), NULL);

    tt = GNC_TAXTABLE (pObject);
    pParent = gncTaxTableGetParent (tt);
    if (pParent == NULL)
    {
        parent_guid = NULL;
    }
    else
    {
        parent_guid = qof_instance_get_guid (QOF_INSTANCE (pParent));
    }

    return (gpointer)parent_guid;
}

static void
tt_set_parent (gpointer data, gpointer value)
{
    GncTaxTable* tt;
    GncTaxTable* parent;
    QofBook* pBook;
    GncGUID* guid = (GncGUID*)value;

    g_return_if_fail (data != NULL);
    g_return_if_fail (GNC_IS_TAXTABLE (data));

    tt = GNC_TAXTABLE (data);
    pBook = qof_instance_get_book (QOF_INSTANCE (tt));
    if (guid != NULL)
    {
        parent = gncTaxTableLookup (pBook, guid);
        if (parent != NULL)
        {
            gncTaxTableSetParent (tt, parent);
            gncTaxTableSetChild (parent, tt);
        }
    }
}

static void
tt_set_parent_guid (gpointer pObject,  gpointer pValue)
{
    g_return_if_fail (pObject != NULL);
    g_return_if_fail (pValue != NULL);

    auto s = static_cast<TaxTblParentGuidPtr>(pObject);
    s->guid = *static_cast<GncGUID*>(pValue);
    s->have_guid = true;
}

static void
load_single_ttentry (GncSqlBackend* sql_be, GncSqlRow& row, GncTaxTable* tt)
{
    GncTaxTableEntry* e = gncTaxTableEntryCreate ();

    g_return_if_fail (sql_be != NULL);
    g_return_if_fail (tt != NULL);

    gnc_sql_load_object (sql_be, row, GNC_ID_TAXTABLE, e, ttentries_col_table);
    gncTaxTableAddEntry (tt, e);
}

static void
load_taxtable_entries (GncSqlBackend* sql_be, GncTaxTable* tt)
{
    gchar guid_buf[GUID_ENCODING_LENGTH + 1];
    GValue value;
    gchar* buf;

    g_return_if_fail (sql_be != NULL);
    g_return_if_fail (tt != NULL);

    guid_to_string_buff (qof_instance_get_guid (QOF_INSTANCE (tt)), guid_buf);
    memset (&value, 0, sizeof (GValue));
    g_value_init (&value, G_TYPE_STRING);
    g_value_set_string (&value, guid_buf);
    buf = g_strdup_printf ("SELECT * FROM %s WHERE taxtable='%s'",
                           TTENTRIES_TABLE_NAME, guid_buf);
    auto stmt = sql_be->create_statement_from_sql (buf);
    g_free (buf);
    auto result = sql_be->execute_select_statement(stmt);
    for (auto row : *result)
        load_single_ttentry (sql_be, row, tt);
}

static void
load_single_taxtable (GncSqlBackend* sql_be, GncSqlRow& row,
                      TaxTblParentGuidVec& l_tt_needing_parents)
{
    const GncGUID* guid;
    GncTaxTable* tt;

    g_return_if_fail (sql_be != NULL);

    guid = gnc_sql_load_guid (sql_be, row);
    tt = gncTaxTableLookup (sql_be->book(), guid);
    if (tt == nullptr)
    {
        tt = gncTaxTableCreate (sql_be->book());
    }
    gnc_sql_load_object (sql_be, row, GNC_ID_TAXTABLE, tt, tt_col_table);
    gnc_sql_slots_load (sql_be, QOF_INSTANCE (tt));
    load_taxtable_entries (sql_be, tt);

    /* If the tax table doesn't have a parent, it might be because it hasn't
       been loaded yet.  if so, add this tax table to the list of tax tables
       with no parent, along with the parent GncGUID so that after they are all
       loaded, the parents can be fixed up. */
    if (gncTaxTableGetParent (tt) == NULL)
    {
        TaxTblParentGuid s;

        s.tt = tt;
        s.have_guid = false;
        gnc_sql_load_object (sql_be, row, GNC_ID_TAXTABLE, &s,
                             tt_parent_col_table);
        if (s.have_guid)
            l_tt_needing_parents.push_back(new TaxTblParentGuid(s));

    }

    qof_instance_mark_clean (QOF_INSTANCE (tt));
}

void
GncSqlTaxTableBackend::load_all (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != NULL);

    /* First time, create the query */
    std::stringstream sql;
    sql << "SELECT * FROM " << TT_TABLE_NAME;
    auto stmt = sql_be->create_statement_from_sql(sql.str());
    auto result = sql_be->execute_select_statement(stmt);
    TaxTblParentGuidVec tt_needing_parents;

    for (auto row : *result)
        load_single_taxtable (sql_be, row, tt_needing_parents);

    /* While there are items on the list of taxtables needing parents,
       try to see if the parent has now been loaded.  Theory says that if
       items are removed from the front and added to the back if the
       parent is still not available, then eventually, the list will
       shrink to size 0. */
    if (!tt_needing_parents.empty())
    {
        bool progress_made = true;
	std::reverse(tt_needing_parents.begin(),
		     tt_needing_parents.end());
	auto end = tt_needing_parents.end();
        while (progress_made)
        {
            progress_made = false;
            end = std::remove_if(tt_needing_parents.begin(), end,
				 [&](TaxTblParentGuidPtr s)
				 {
				     auto pBook = qof_instance_get_book (QOF_INSTANCE (s->tt));
				     auto parent = gncTaxTableLookup (pBook,
								      &s->guid);
				     if (parent != nullptr)
				     {
					 tt_set_parent (s->tt, &s->guid);
					 progress_made = true;
					 delete s;
					 return true;
				     }
				     return false;
				 });

        }
    }
}

/* ================================================================= */
void
GncSqlTaxTableBackend::create_tables (GncSqlBackend* sql_be)
{
    gint version;

    g_return_if_fail (sql_be != NULL);

    version = sql_be->get_table_version( TT_TABLE_NAME);
    if (version == 0)
    {
        sql_be->create_table(TT_TABLE_NAME, TT_TABLE_VERSION, tt_col_table);
    }
    else if (version < m_version)
    {
        /* Upgrade 64 bit int handling */
        sql_be->upgrade_table(TT_TABLE_NAME, tt_col_table);
        sql_be->set_table_version (TT_TABLE_NAME, TT_TABLE_VERSION);
        PINFO ("Taxtables table upgraded from version 1 to version %d\n",
               TT_TABLE_VERSION);
    }

    version = sql_be->get_table_version( TTENTRIES_TABLE_NAME);
    if (version == 0)
    {
        sql_be->create_table(TTENTRIES_TABLE_NAME, TTENTRIES_TABLE_VERSION,
                              ttentries_col_table);
    }
    else if (version < TTENTRIES_TABLE_VERSION)
    {
        /* Upgrade 64 bit int handling */
        sql_be->upgrade_table(TTENTRIES_TABLE_NAME, ttentries_col_table);
        sql_be->set_table_version (TTENTRIES_TABLE_NAME, TTENTRIES_TABLE_VERSION);
        PINFO ("Taxtable entries table upgraded from version 1 to version %d\n",
               TTENTRIES_TABLE_VERSION);
    }
}

/* ================================================================= */
static gboolean
delete_all_tt_entries (GncSqlBackend* sql_be, const GncGUID* guid)
{
    guid_info_t guid_info;

    g_return_val_if_fail (sql_be != NULL, FALSE);
    g_return_val_if_fail (guid != NULL, FALSE);

    guid_info.be = sql_be;
    guid_info.guid = guid;
    return sql_be->do_db_operation(OP_DB_DELETE, TTENTRIES_TABLE_NAME,
                                   TTENTRIES_TABLE_NAME, &guid_info, guid_col_table);
}

static gboolean
save_tt_entries (GncSqlBackend* sql_be, const GncGUID* guid, GList* entries)
{
    GList* entry;
    gboolean is_ok;

    g_return_val_if_fail (sql_be != NULL, FALSE);
    g_return_val_if_fail (guid != NULL, FALSE);

    /* First, delete the old entries for this object */
    is_ok = delete_all_tt_entries (sql_be, guid);

    for (entry = entries; entry != NULL && is_ok; entry = entry->next)
    {
        GncTaxTableEntry* e = (GncTaxTableEntry*)entry->data;
        is_ok = sql_be->do_db_operation(OP_DB_INSERT, TTENTRIES_TABLE_NAME,
                                        GNC_ID_TAXTABLE, e,
                                        ttentries_col_table);
    }

    return is_ok;
}

bool
GncSqlTaxTableBackend::commit (GncSqlBackend* sql_be, QofInstance* inst)
{
    GncTaxTable* tt;
    const GncGUID* guid;
    E_DB_OPERATION op;
    gboolean is_infant;
    gboolean is_ok;

    g_return_val_if_fail (inst != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_TAXTABLE (inst), FALSE);
    g_return_val_if_fail (sql_be != NULL, FALSE);

    tt = GNC_TAXTABLE (inst);

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
    is_ok = sql_be->do_db_operation(op, TT_TABLE_NAME, GNC_ID_TAXTABLE, tt,
                                    tt_col_table);

    if (is_ok)
    {
        // Now, commit or delete any slots and tax table entries
        guid = qof_instance_get_guid (inst);
        if (!qof_instance_get_destroying (inst))
        {
            is_ok = gnc_sql_slots_save (sql_be, guid, is_infant, inst);
            if (is_ok)
            {
                is_ok = save_tt_entries (sql_be, guid, gncTaxTableGetEntries (tt));
            }
        }
        else
        {
            is_ok = gnc_sql_slots_delete (sql_be, guid);
            if (is_ok)
            {
                is_ok = delete_all_tt_entries (sql_be, guid);
            }
        }
    }

    return is_ok;
}

/* ================================================================= */
static void
save_next_taxtable (QofInstance* inst, gpointer data)
{
    auto s = reinterpret_cast<write_objects_t*>(data);

    if (s->is_ok)
    {
        s->commit (inst);
    }
}

bool
GncSqlTaxTableBackend::write (GncSqlBackend* sql_be)
{
    g_return_val_if_fail (sql_be != NULL, FALSE);
    write_objects_t data{sql_be, true, this};

    qof_object_foreach (GNC_ID_TAXTABLE, sql_be->book(), save_next_taxtable, &data);

    return data.is_ok;
}

/* ================================================================= */
template<> void
GncSqlColumnTableEntryImpl<CT_TAXTABLEREF>::load (const GncSqlBackend* sql_be,
                                                  GncSqlRow& row,
                                                  QofIdTypeConst obj_name,
                                                  gpointer pObject) const noexcept
{
    load_from_guid_ref(row, obj_name, pObject,
                       [sql_be](GncGUID* g){
                           return gncTaxTableLookup(sql_be->book(), g);
                       });
}

template<> void
GncSqlColumnTableEntryImpl<CT_TAXTABLEREF>::add_to_table(ColVec& vec) const noexcept
{
    add_objectref_guid_to_table(vec);
}

template<> void
GncSqlColumnTableEntryImpl<CT_TAXTABLEREF>::add_to_query(QofIdTypeConst obj_name,
                                                         const gpointer pObject,
                                                         PairVec& vec) const noexcept
{
    add_objectref_guid_to_query(obj_name, pObject, vec);
}

/* ========================== END OF FILE ===================== */
