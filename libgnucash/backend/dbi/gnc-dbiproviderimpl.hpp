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
#include <string>
#include <algorithm>
#include <vector>

#include "gnc-backend-dbi.hpp"
#include "gnc-dbiprovider.hpp"
#include "gnc-backend-dbi.h"
#include <gnc-sql-column-table-entry.hpp>

using StrVec = std::vector<std::string>;

template <DbType T>
class GncDbiProviderImpl : public GncDbiProvider
{
public:
    StrVec get_table_list(dbi_conn conn, const std::string& table);
    void append_col_def(std::string& ddl, const GncSqlColumnInfo& info);
    StrVec get_index_list (dbi_conn conn);
    void drop_index(dbi_conn conn, const std::string& index);
};

template <DbType T> GncDbiProviderPtr
make_dbi_provider()
{
    return GncDbiProviderPtr(new GncDbiProviderImpl<T>);
}

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
        type_name = "DATETIME NULL DEFAULT '1970-01-01 00:00:00'";
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
conn_get_table_list (dbi_conn conn, const std::string& dbname,
                     const std::string& table)
{
    StrVec retval;
    const char* tableptr = (table.empty() ? nullptr : table.c_str());
    auto tables = dbi_conn_get_table_list (conn, dbname.c_str(), tableptr);
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
                                                        const std::string& table)
{
    /* Return the list, but remove the tables that sqlite3 adds for
     * its own use. */
    std::string dbname (dbi_conn_get_option (conn, "dbname"));
    auto list = conn_get_table_list (conn, dbname, table);
    auto end = std::remove(list.begin(), list.end(), "sqlite_sequence");
    list.erase(end, list.end());
    return list;
}

template<> StrVec
GncDbiProviderImpl<DbType::DBI_MYSQL>::get_table_list (dbi_conn conn,
                                                       const std::string& table)
{
    std::string dbname (dbi_conn_get_option (conn, "dbname"));
    dbname.insert((std::string::size_type)0, 1, '`');
    dbname += '`';
    return conn_get_table_list (conn, dbname, table);
}

template<> StrVec
GncDbiProviderImpl<DbType::DBI_PGSQL>::get_table_list (dbi_conn conn,
                                                       const std::string& table)
{
    const char* query_no_regex = "SELECT relname FROM pg_class WHERE relname"
                          "!~ '^(pg|sql)_' AND relkind = 'r' ORDER BY relname";
    std::string query_with_regex = "SELECT relname FROM pg_class WHERE relname LIKE '";
    query_with_regex += table + "' AND relkind = 'r' ORDER BY relname";
    dbi_result result;
    if (table.empty())
        result = dbi_conn_query (conn, query_no_regex);
    else
        result = dbi_conn_query (conn, query_with_regex.c_str());

    StrVec list;
    const char* errmsg;
    if (dbi_conn_error (conn, &errmsg) != DBI_ERROR_NONE)
    {
        PWARN ("Table List Retrieval Error: %s\n", errmsg);
        return list;
    }

    while (dbi_result_next_row (result) != 0)
    {
        std::string index_name {dbi_result_get_string_idx (result, 1)};
        list.push_back(index_name);
    }
    dbi_result_free (result);
    return list;
}

template<> StrVec
GncDbiProviderImpl<DbType::DBI_SQLITE>::get_index_list (dbi_conn conn)
{
    StrVec retval;
    const char* errmsg;
    dbi_result result = dbi_conn_query (conn,
                                        "SELECT name FROM sqlite_master WHERE type = 'index' AND name NOT LIKE 'sqlite_autoindex%'");
    if (dbi_conn_error (conn, &errmsg) != DBI_ERROR_NONE)
    {
        PWARN ("Index Table Retrieval Error: %s\n", errmsg);
        return retval;
    }
    while (dbi_result_next_row (result) != 0)
    {
        std::string index_name {dbi_result_get_string_idx (result, 1)};
        retval.push_back(index_name);
    }
    dbi_result_free (result);
    return retval;
}

template<> StrVec
GncDbiProviderImpl<DbType::DBI_MYSQL>::get_index_list (dbi_conn conn)
{
    StrVec retval;
    const char* errmsg;
    auto tables = get_table_list(conn, "");
    for (auto table_name : tables)
    {
        auto result = dbi_conn_queryf (conn,
                                       "SHOW INDEXES IN %s WHERE Key_name != 'PRIMARY'",
                                       table_name.c_str());
        if (dbi_conn_error (conn, &errmsg) != DBI_ERROR_NONE)
        {
            PWARN ("Index Table Retrieval Error: %s on table %s\n",
                   errmsg, table_name.c_str());
            continue;
        }

        while (dbi_result_next_row (result) != 0)
        {
            std::string index_name {dbi_result_get_string_idx (result, 3)};
            retval.push_back(index_name + " " + table_name);
        }
        dbi_result_free (result);
    }

    return retval;
}

template<> StrVec
GncDbiProviderImpl<DbType::DBI_PGSQL>::get_index_list (dbi_conn conn)
{
    StrVec retval;
    const char* errmsg;
    PINFO ("Retrieving postgres index list\n");
    auto result = dbi_conn_query (conn,
                                  "SELECT relname FROM pg_class AS a INNER JOIN pg_index AS b ON (b.indexrelid = a.oid) INNER JOIN pg_namespace AS c ON (a.relnamespace = c.oid) WHERE reltype = '0' AND indisprimary = 'f' AND nspname = 'public'");
    if (dbi_conn_error (conn, &errmsg) != DBI_ERROR_NONE)
    {
        PWARN("Index Table Retrieval Error: %s\n", errmsg);
        return retval;
    }
    while (dbi_result_next_row (result) != 0)
    {
        std::string index_name {dbi_result_get_string_idx (result, 1)};
        retval.push_back(index_name);
    }
    dbi_result_free (result);
    return retval;
}

template <DbType P> void
GncDbiProviderImpl<P>::drop_index(dbi_conn conn, const std::string& index)
{
    dbi_result result = dbi_conn_queryf (conn, "DROP INDEX %s", index.c_str());
    if (result)
        dbi_result_free (result);
}

template<> void
GncDbiProviderImpl<DbType::DBI_MYSQL>::drop_index (dbi_conn conn, const std::string& index)
{

    auto sep = index.find(' ', 0);
    if (index.find(' ', sep + 1) != std::string::npos)
    {
        PWARN("Drop index error: invalid MySQL index format (<index> <table>): %s",
              index.c_str());
        return;
    }

    auto result = dbi_conn_queryf (conn, "DROP INDEX %s ON %s",
                                   index.substr(0, sep).c_str(),
                                   index.substr(sep + 1).c_str());
    if (result)
        dbi_result_free (result);
}
#endif //__GNC_DBISQLPROVIDERIMPL_HPP__
