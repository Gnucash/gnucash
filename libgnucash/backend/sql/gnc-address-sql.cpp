/********************************************************************\
 * gnc-address-sql.c -- address sql backend implementation          *
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

/** @file gnc-address-sql.c
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

#include "gnc-engine.h"

#include "gncAddress.h"
}
#include <cstdlib>
#include <cstring>
#include <sstream>
#include "gnc-sql-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"

G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

#define ADDRESS_MAX_NAME_LEN 1024
#define ADDRESS_MAX_ADDRESS_LINE_LEN 1024
#define ADDRESS_MAX_PHONE_LEN 128
#define ADDRESS_MAX_FAX_LEN 128
#define ADDRESS_MAX_EMAIL_LEN 256

static EntryVec col_table
({
    std::make_shared<GncSqlColumnTableEntryImpl<CT_STRING>>(
        "name",  CT_STRING, ADDRESS_MAX_NAME_LEN, COL_NNUL, "name"),
    gnc_sql_make_table_entry<CT_STRING>(
        "addr1", ADDRESS_MAX_ADDRESS_LINE_LEN, COL_NNUL, "addr1"),
    gnc_sql_make_table_entry<CT_STRING>(
        "addr2", ADDRESS_MAX_ADDRESS_LINE_LEN, COL_NNUL, "addr2"),
    gnc_sql_make_table_entry<CT_STRING>(
        "addr3", ADDRESS_MAX_ADDRESS_LINE_LEN, COL_NNUL, "addr3"),
    gnc_sql_make_table_entry<CT_STRING>(
        "addr4", ADDRESS_MAX_ADDRESS_LINE_LEN, COL_NNUL, "addr4"),
    gnc_sql_make_table_entry<CT_STRING>(
        "phone", ADDRESS_MAX_PHONE_LEN, COL_NNUL, "phone"),
    gnc_sql_make_table_entry<CT_STRING>(
        "fax", ADDRESS_MAX_FAX_LEN, COL_NNUL, "fax" ),
    gnc_sql_make_table_entry<CT_STRING>(
        "email", ADDRESS_MAX_EMAIL_LEN, COL_NNUL, "email"),
});

typedef void (*AddressSetterFunc) (gpointer, GncAddress*);
typedef GncAddress* (*AddressGetterFunc) (const gpointer);

template<> void
GncSqlColumnTableEntryImpl<CT_ADDRESS>::load (const GncSqlBackend* sql_be,
                                              GncSqlRow& row,
                                              QofIdTypeConst obj_name,
                                              gpointer pObject) const noexcept
{
    const gchar* s;


    g_return_if_fail (sql_be != NULL);
    g_return_if_fail (pObject != NULL);

    auto addr = gncAddressCreate (sql_be->book(), QOF_INSTANCE(pObject));

    for (auto const& subtable_row : col_table)
    {
        auto buf = std::string{m_col_name} + "_" + subtable_row->m_col_name;
        try
        {
            auto val = row.get_string_at_col (buf.c_str());
            auto sub_setter = subtable_row->get_setter(GNC_ID_ADDRESS);
            set_parameter (addr, val.c_str(), sub_setter,
                           subtable_row->m_gobj_param_name);
        }
        catch (std::invalid_argument)
        {
            return;
        }
    }
    set_parameter (pObject, addr,
                   reinterpret_cast<AddressSetterFunc>(get_setter(obj_name)),
                   m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_ADDRESS>::add_to_table(ColVec& vec) const noexcept
{
    for (auto const& subtable_row : col_table)
    {
        auto buf = std::string{m_col_name} + "_" + subtable_row->m_col_name;
        GncSqlColumnInfo info(buf.c_str(), BCT_STRING, subtable_row->m_size,
                              true, false, m_flags & COL_PKEY, m_flags & COL_NNUL);
        vec.emplace_back(std::move(info));
    }
}

/* char is unusual in that we get a pointer but don't deref it to pass
 * it to operator<<().
 */
template<> void
GncSqlColumnTableEntryImpl<CT_ADDRESS>::add_to_query(QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    auto addr(get_row_value_from_object<char*>(obj_name, pObject));
    if (addr == nullptr) return;

    for (auto const& subtable_row : col_table)
    {
        auto s = subtable_row->get_row_value_from_object<char*>(GNC_ID_ADDRESS,
                                                                addr);
        if (s == nullptr)
            continue;
        auto buf = std::string{m_col_name} + "_" + subtable_row->m_col_name;
        vec.emplace_back(make_pair(buf, quote_string(s)));
    }
}
/* ========================== END OF FILE ===================== */
