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
#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-engine.h"

#include "gncAddress.h"
}
#include "gnc-backend-sql.h"
#include "gnc-address-sql.h"

G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

#define ADDRESS_MAX_NAME_LEN 1024
#define ADDRESS_MAX_ADDRESS_LINE_LEN 1024
#define ADDRESS_MAX_PHONE_LEN 128
#define ADDRESS_MAX_FAX_LEN 128
#define ADDRESS_MAX_EMAIL_LEN 256

static EntryVec col_table
({
    { "name",  CT_STRING, ADDRESS_MAX_NAME_LEN,         COL_NNUL, "name" },
    { "addr1", CT_STRING, ADDRESS_MAX_ADDRESS_LINE_LEN, COL_NNUL, "addr1" },
    { "addr2", CT_STRING, ADDRESS_MAX_ADDRESS_LINE_LEN, COL_NNUL, "addr2" },
    { "addr3", CT_STRING, ADDRESS_MAX_ADDRESS_LINE_LEN, COL_NNUL, "addr3" },
    { "addr4", CT_STRING, ADDRESS_MAX_ADDRESS_LINE_LEN, COL_NNUL, "addr4" },
    { "phone", CT_STRING, ADDRESS_MAX_PHONE_LEN,        COL_NNUL, "phone" },
    { "fax",   CT_STRING, ADDRESS_MAX_FAX_LEN,          COL_NNUL, "fax" },
    { "email", CT_STRING, ADDRESS_MAX_EMAIL_LEN,        COL_NNUL, "email" },
});

typedef void (*AddressSetterFunc) (gpointer, GncAddress*);
typedef GncAddress* (*AddressGetterFunc) (const gpointer);

static void
load_address (const GncSqlBackend* be, GncSqlRow* row,
              QofSetterFunc setter, gpointer pObject,
              const GncSqlColumnTableEntry& table_row)
{
    const GValue* val;
    gchar* buf;
    GncAddress* addr;
    AddressSetterFunc a_setter = (AddressSetterFunc)setter;
    const gchar* s;


    g_return_if_fail (be != NULL);
    g_return_if_fail (row != NULL);
    g_return_if_fail (pObject != NULL);

    addr = gncAddressCreate (be->book, QOF_INSTANCE(pObject));
    for (auto const& subtable_row : col_table)
    {
        buf = g_strdup_printf ("%s_%s", table_row.col_name,
                               subtable_row.col_name);
        val = gnc_sql_row_get_value_at_col_name (row, buf);
        g_free (buf);
        if (val == NULL)
        {
            s = NULL;
        }
        else
        {
            s = g_value_get_string (val);
        }
        if (subtable_row.gobj_param_name != NULL)
        {
            g_object_set (addr, subtable_row.gobj_param_name, s, NULL);
        }
        else
        {
            if (subtable_row.qof_param_name != NULL)
            {
                setter = qof_class_get_parameter_setter (GNC_ID_ADDRESS,
                                                         subtable_row.qof_param_name);
            }
            else
            {
                setter = subtable_row.setter;
            }
            (*setter) (addr, (const gpointer)s);
        }
    }
    if (table_row.gobj_param_name != NULL)
    {
        qof_instance_increase_editlevel (pObject);
        g_object_set (pObject, table_row.gobj_param_name, addr, NULL);
        qof_instance_decrease_editlevel (pObject);
    }
    else
    {
        (*a_setter) (pObject, addr);
    }
}

static void
add_address_col_info_to_list(const GncSqlBackend* be,
                             const GncSqlColumnTableEntry& table_row,
                             ColVec& vec)
{
    GncSqlColumnInfo* info;
    gchar* buf;

    g_return_if_fail (be != NULL);

    for (auto const& subtable_row : col_table)
    {
        buf = g_strdup_printf ("%s_%s", table_row.col_name, subtable_row.col_name);

        GncSqlColumnInfo info(buf, BCT_STRING, subtable_row.size, true, false,
                              table_row.flags & COL_PKEY,
                              table_row.flags & COL_NNUL);
        vec.emplace_back(std::move(info));
    }
}

static void
add_value_address_to_vec (const GncSqlBackend* be, QofIdTypeConst obj_name,
                          const gpointer pObject,
                          const GncSqlColumnTableEntry& table_row,
                          PairVec& vec)
{
    auto addr = get_row_value_from_object<GncAddress*>(obj_name, pObject,
                                                       table_row);


    if (addr == nullptr)
        return;
    for (auto const& subtable_row : col_table)
    {
        auto s = get_row_value_from_object<char*>(GNC_ID_ADDRESS, addr,
                                                  subtable_row);
        if (s == nullptr)
            continue;
        std::ostringstream buf;
        buf << table_row.col_name << "_" << subtable_row.col_name;
        vec.emplace_back(make_pair(buf.str(), std::string{s}));
    }
}

static GncSqlColumnTypeHandler address_handler
= { load_address,
    add_address_col_info_to_list,
    add_value_address_to_vec
  };

/* ================================================================= */
void
gnc_address_sql_initialize (void)
{
    gnc_sql_register_col_type_handler (CT_ADDRESS, &address_handler);
}
/* ========================== END OF FILE ===================== */
