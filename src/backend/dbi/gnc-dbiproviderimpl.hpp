/************************************************************************
 * gnc-dbiproviderimpl.hpp: Encapsulate differences among Dbi backends. *
 *                                                                      *
 * Copyright 2016 John Ralls <jralls@ceridwen.us>                       *
 *                                                                      *
 * This program is free software; you can redistribute it and/or        *
 * modify it under the terms of the GNU General Public License as       *
 * published by the Free Software Foundation; either version 2 of       *
 * the License, or (at your option) any later version.                  *
 *                                                                      *
 * This program is distributed in the hope that it will be useful,      *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 * GNU General Public License for more details.                         *
 *                                                                      *
 * You should have received a copy of the GNU General Public License    *
 * along with this program; if not, contact:                            *
 *                                                                      *
 * Free Software Foundation           Voice:  +1-617-542-5942           *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652           *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                       *
\***********************************************************************/
#ifndef __GNC_DBISQLPROVIDERIMPL_HPP__
#define __GNC_DBISQLPROVIDERIMPL_HPP__
#include <guid.hpp>
extern "C"
{
#include <config.h>
}
#include "gnc-backend-dbi.hpp"
#include "gnc-dbiprovider.hpp"

enum class DbType
{
    DBI_SQLITE,
    DBI_MYSQL,
    DBI_PGSQL
};

template <DbType T>
class GncDbiProviderImpl : public GncDbiProvider
{
public:
    std::string create_table_ddl(const GncSqlConnection* conn,
                                 const std::string& table_name,
                                 const ColVec& info_vec);
    StrVec get_table_list(dbi_conn conn,
                                            const std::string& dbname);
    void append_col_def(std::string& ddl, const GncSqlColumnInfo& info);
    StrVec get_index_list (dbi_conn conn);
    void drop_index(dbi_conn conn, const std::string& index);
};

template<> void
GncDbiProviderImpl<DbType::DBI_SQLITE>::append_col_def(std::string& ddl,
                                           const GncSqlColumnInfo& info)
{
    const char* type_name = nullptr;

    if (info.m_type == BCT_INT)
    {
        type_name = "integer";
    }
    else if (info.m_type == BCT_INT64)
    {
        type_name = "bigint";
    }
    else if (info.m_type == BCT_DOUBLE)
    {
        type_name = "float8";
    }
    else if (info.m_type == BCT_STRING || info.m_type == BCT_DATE
              || info.m_type == BCT_DATETIME)
    {
        type_name = "text";
    }
    else
    {
        PERR ("Unknown column type: %d\n", info.m_type);
        type_name = "";
    }
    ddl += (info.m_name + " " + type_name);
    if (info.m_size != 0)
    {
        ddl += "(" + std::to_string(info.m_size) + ")";
    }
    if (info.m_primary_key)
    {
        ddl += " PRIMARY KEY";
    }
    if (info.m_autoinc)
    {
        ddl += " AUTOINCREMENT";
    }
    if (info.m_not_null)
    {
        ddl += " NOT NULL";
    }
}

template <DbType P> std::string
GncDbiProviderImpl<P>::create_table_ddl (const GncSqlConnection* conn,
                                              const std::string& table_name,
                                              const ColVec& info_vec)
{
    std::string ddl;
    unsigned int col_num = 0;

    g_return_val_if_fail (conn != nullptr, ddl);
    ddl += "CREATE TABLE " + table_name + "(";
    for (auto const& info : info_vec)
    {
        if (col_num++ != 0)
        {
            ddl += ", ";
        }
        append_col_def (ddl, info);
    }
    ddl += ")";

    return ddl;
}

template<> void
GncDbiProviderImpl<DbType::DBI_MYSQL>::append_col_def (std::string& ddl,
                                           const GncSqlColumnInfo& info)
{
    const char* type_name = nullptr;

    if (info.m_type == BCT_INT)
    {
        type_name = "integer";
    }
    else if (info.m_type == BCT_INT64)
    {
        type_name = "bigint";
    }
    else if (info.m_type == BCT_DOUBLE)
    {
        type_name = "double";
    }
    else if (info.m_type == BCT_STRING)
    {
        type_name = "varchar";
    }
    else if (info.m_type == BCT_DATE)
    {
        type_name = "date";
    }
    else if (info.m_type == BCT_DATETIME)
    {
        type_name = "TIMESTAMP NULL DEFAULT 0";
    }
    else
    {
        PERR ("Unknown column type: %d\n", info.m_type);
        type_name = "";
    }
    ddl += info.m_name + " " + type_name;
    if (info.m_size != 0 && info.m_type == BCT_STRING)
    {
        ddl += "(" + std::to_string(info.m_size) + ")";
    }
    if (info.m_unicode)
    {
        ddl += " CHARACTER SET utf8";
    }
    if (info.m_primary_key)
    {
        ddl += " PRIMARY KEY";
    }
    if (info.m_autoinc)
    {
        ddl += " AUTO_INCREMENT";
    }
    if (info.m_not_null)
    {
        ddl += " NOT NULL";
    }
}


template<> void
GncDbiProviderImpl<DbType::DBI_PGSQL>::append_col_def (std::string& ddl,
                                           const GncSqlColumnInfo& info)
{
    const char* type_name = nullptr;

    if (info.m_type == BCT_INT)
    {
        if (info.m_autoinc)
        {
            type_name = "serial";
        }
        else
        {
            type_name = "integer";
        }
    }
    else if (info.m_type == BCT_INT64)
    {
        type_name = "int8";
    }
    else if (info.m_type == BCT_DOUBLE)

    {
        type_name = "double precision";
    }
    else if (info.m_type == BCT_STRING)
    {
        type_name = "varchar";
    }
    else if (info.m_type == BCT_DATE)
    {
        type_name = "date";
    }
    else if (info.m_type == BCT_DATETIME)
    {
        type_name = "timestamp without time zone";
    }
    else
    {
        PERR ("Unknown column type: %d\n", info.m_type);
        type_name = "";
    }
    ddl += info.m_name + " " + type_name;
    if (info.m_size != 0 && info.m_type == BCT_STRING)
    {
        ddl += "(" + std::to_string(info.m_size) + ")";
    }
    if (info.m_primary_key)
    {
        ddl += " PRIMARY KEY";
    }
    if (info.m_not_null)
    {
        ddl += " NOT NULL";
    }
}

static StrVec
conn_get_table_list (dbi_conn conn, const std::string& dbname)
{
    StrVec retval;
    auto tables = dbi_conn_get_table_list (conn, dbname.c_str(), nullptr);
    while (dbi_result_next_row (tables) != 0)
    {
        std::string table_name {dbi_result_get_string_idx (tables, 1)};
        retval.push_back(table_name);
    }
    dbi_result_free (tables);
    return retval;
}

template<> StrVec
GncDbiProviderImpl<DbType::DBI_SQLITE>::get_table_list (dbi_conn conn,
                                            const std::string& dbname)
{
    /* Return the list, but remove the tables that sqlite3 adds for
     * its own use. */
    auto list = conn_get_table_list (conn, dbname);
    auto end = std::remove(list.begin(), list.end(), "sqlite_sequence");
    list.erase(end, list.end());
    return list;
}

template<> StrVec
GncDbiProviderImpl<DbType::DBI_MYSQL>::get_table_list (dbi_conn conn,
                                               const std::string& dbname)
{
    return conn_get_table_list (conn, dbname);
}

template<> StrVec
GncDbiProviderImpl<DbType::DBI_PGSQL>::get_table_list (dbi_conn conn,
                                           const std::string& dbname)
{
    auto list = conn_get_table_list (conn, dbname);
    auto end = std::remove_if (list.begin(), list.end(),
                               [](std::string& table_name){
                                   return table_name == "sql_features" ||
                                   table_name == "sql_implementation_info" ||
                                   table_name == "sql_languages" ||
                                   table_name == "sql_packages" ||
                                   table_name == "sql_parts" ||
                                   table_name == "sql_sizing" ||
                                   table_name == "sql_sizing_profiles";
                               });
    list.erase(end, list.end());
    return list;
}

#endif //__GNC_DBISQLPROVIDERIMPL_HPP__
