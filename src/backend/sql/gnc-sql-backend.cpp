/********************************************************************
 * gnc-sql-backend.cpp: Implementation of GncSqlBackend             *
 *                                                                  *
 * Copyright 2016 John Ralls <jralls@ceridwen.us>                   *
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
\********************************************************************/
extern "C"
{
#include <config.h>
#include <gnc-prefs.h>
}

#include <algorithm>

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-sql-result.hpp"
#include "gnc-backend-sql.h"

static QofLogModule log_module = G_LOG_DOMAIN;
#define VERSION_TABLE_NAME "versions"
#define MAX_TABLE_NAME_LEN 50
#define TABLE_COL_NAME "table_name"
#define VERSION_COL_NAME "table_version"

static EntryVec version_table
{
    gnc_sql_make_table_entry<CT_STRING>(
        TABLE_COL_NAME, MAX_TABLE_NAME_LEN, COL_PKEY | COL_NNUL),
    gnc_sql_make_table_entry<CT_INT>(VERSION_COL_NAME, 0, COL_NNUL)
};

GncSqlBackend::GncSqlBackend(GncSqlConnection *conn, QofBook* book,
                             const char* format) :
    qof_be {nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr, nullptr, nullptr, nullptr, ERR_BACKEND_NO_ERR, nullptr, 0,
            nullptr}, m_conn{conn}, m_book{book}, m_loading{false},
        m_in_query{false}, m_is_pristine_db{false}, m_timespec_format{format}
{
    if (conn != nullptr)
        connect (conn);
}

void
GncSqlBackend::connect(GncSqlConnection *conn) noexcept
{
    if (m_conn != nullptr && m_conn != conn)
        delete m_conn;
    finalize_version_info();
    m_conn = conn;
}

GncSqlStatementPtr
GncSqlBackend::create_statement_from_sql(const std::string& str) const noexcept
{
    auto stmt = m_conn->create_statement_from_sql(str);
    if (stmt == nullptr)
    {
        PERR ("SQL error: %s\n", str.c_str());
        qof_backend_set_error ((QofBackend*)this, ERR_BACKEND_SERVER_ERR);
    }
    return stmt;
}

GncSqlResultPtr
GncSqlBackend::execute_select_statement(const GncSqlStatementPtr& stmt) const noexcept
{
    auto result = m_conn->execute_select_statement(stmt);
    if (result == nullptr)
    {
        PERR ("SQL error: %s\n", stmt->to_sql());
        qof_backend_set_error ((QofBackend*)this, ERR_BACKEND_SERVER_ERR);
    }
    return result;
}

int
GncSqlBackend::execute_nonselect_statement(const GncSqlStatementPtr& stmt) const noexcept
{
    auto result = m_conn->execute_nonselect_statement(stmt);
    if (result == -1)
    {
        PERR ("SQL error: %s\n", stmt->to_sql());
        qof_backend_set_error ((QofBackend*)this, ERR_BACKEND_SERVER_ERR);
    }
    return result;
}

std::string
GncSqlBackend::quote_string(const std::string& str) const noexcept
{
    return m_conn->quote_string(str);
}

bool
GncSqlBackend::create_table(const std::string& table_name,
                            const EntryVec& col_table) const noexcept
{
    ColVec info_vec;
    gboolean ok = FALSE;

    for (auto const& table_row : col_table)
    {
        table_row->add_to_table (this, info_vec);
    }
    return m_conn->create_table (table_name, info_vec);

}

bool
GncSqlBackend::create_table(const std::string& table_name, int table_version,
                            const EntryVec& col_table) noexcept
{
    if (create_table (table_name, col_table))
        return set_table_version (table_name, table_version);
    return false;
}

bool
GncSqlBackend::create_index(const std::string& index_name,
                            const std::string& table_name,
                            const EntryVec& col_table) const noexcept
{
    return m_conn->create_index(index_name, table_name, col_table);
}

bool
GncSqlBackend::add_columns_to_table(const std::string& table_name,
                                    const EntryVec& col_table) const noexcept
{
    ColVec info_vec;

    for (auto const& table_row : col_table)
    {
        table_row->add_to_table (this, info_vec);
    }
    return m_conn->add_columns_to_table(table_name, info_vec);
}

void
GncSqlBackend::update_progress() const noexcept
{
    if (qof_be.percentage != nullptr)
        (qof_be.percentage) (nullptr, 101.0);
}

void
GncSqlBackend::finish_progress() const noexcept
{
    if (qof_be.percentage != nullptr)
        (qof_be.percentage) (nullptr, -1.0);
}

/**
 * Sees if the version table exists, and if it does, loads the info into
 * the version hash table.  Otherwise, it creates an empty version table.
 *
 * @param be Backend struct
 */
void
GncSqlBackend::init_version_info() noexcept
{

    if (m_conn->does_table_exist (VERSION_TABLE_NAME))
    {
        std::string sql {"SELECT * FROM "};
        sql += VERSION_TABLE_NAME;
        auto stmt = m_conn->create_statement_from_sql(sql);
        auto result = m_conn->execute_select_statement (stmt);
        for (const auto& row : *result)
        {
            auto name = row.get_string_at_col (TABLE_COL_NAME);
            unsigned int version = row.get_int_at_col (VERSION_COL_NAME);
            m_versions.push_back(std::make_pair(name, version));
        }
    }
    else
    {
        create_table (VERSION_TABLE_NAME, version_table);
        set_table_version("Gnucash", gnc_prefs_get_long_version ());
        set_table_version("Gnucash-Resave", GNUCASH_RESAVE_VERSION);
    }
}

/**
 * Resets the version table information by removing all version table info.
 * It also recreates the version table in the db.
 *
 * @param be Backend struct
 * @return TRUE if successful, FALSE if error
 */
bool
GncSqlBackend::reset_version_info() noexcept
{
    bool ok = true;
    if (!m_conn->does_table_exist (VERSION_TABLE_NAME))
        ok = create_table (VERSION_TABLE_NAME, version_table);
    m_versions.clear();
    set_table_version ("Gnucash", gnc_prefs_get_long_version ());
    set_table_version ("Gnucash-Resave", GNUCASH_RESAVE_VERSION);
    return ok;
}

/**
 * Finalizes the version table info by destroying the hash table.
 *
 * @param be Backend struct
 */
void
GncSqlBackend::finalize_version_info() noexcept
{
    m_versions.clear();
}

unsigned int
GncSqlBackend::get_table_version(const std::string& table_name) const noexcept
{
    /* If the db is pristine because it's being saved, the table does not exist. */
    if (m_is_pristine_db)
        return 0;

    auto version = std::find_if(m_versions.begin(), m_versions.end(),
                                [table_name](const VersionPair& version) {
                                    return version.first == table_name; });
    if (version != m_versions.end())
        return version->second;
    return 0;
}

/**
 * Registers the version for a table.  Registering involves updating the
 * db version table and also the hash table.
 *
 * @param be Backend struct
 * @param table_name Table name
 * @param version Version number
 * @return TRUE if successful, FALSE if unsuccessful
 */
bool
GncSqlBackend::set_table_version (const std::string& table_name,
                                  uint_t version) noexcept
{
    g_return_val_if_fail (version > 0, false);

    unsigned int cur_version{0};
    std::stringstream sql;
    auto ver_entry = std::find_if(m_versions.begin(), m_versions.end(),
                                [table_name](const VersionPair& ver) {
                                    return ver.first == table_name; });
    if (ver_entry != m_versions.end())
        cur_version = ver_entry->second;
    if (cur_version != version)
    {
        if (cur_version == 0)
        {
            sql << "INSERT INTO " << VERSION_TABLE_NAME << " VALUES('" <<
                table_name << "'," << version <<")";
            m_versions.push_back(std::make_pair(table_name, version));
        }
        else
        {
            sql << "UPDATE " <<  VERSION_TABLE_NAME << " SET " <<
                VERSION_COL_NAME << "=" << version << " WHERE " <<
                TABLE_COL_NAME << "='" << table_name << "'";
            ver_entry->second = version;
        }
        auto stmt = create_statement_from_sql(sql.str());
        auto status = execute_nonselect_statement (stmt);
        if (status == -1)
        {
            PERR ("SQL error: %s\n", sql.str().c_str());
            qof_backend_set_error ((QofBackend*)this, ERR_BACKEND_SERVER_ERR);
            return false;
        }
    }

    return true;
}

void
GncSqlBackend::upgrade_table (const std::string& table_name,
                              const EntryVec& col_table) noexcept
{
    DEBUG ("Upgrading %s table\n", table_name.c_str());

    auto temp_table_name = table_name + "_new";
    create_table (temp_table_name, col_table);
    std::stringstream sql;
    sql << "INSERT INTO " << temp_table_name << " SELECT * FROM " << table_name;
    auto stmt = create_statement_from_sql(sql.str());
    execute_nonselect_statement(stmt);

    sql.str("");
    sql << "DROP TABLE " << table_name;
    stmt = create_statement_from_sql(sql.str());
    execute_nonselect_statement(stmt);

    sql.str("");
    sql << "ALTER TABLE " << temp_table_name << " RENAME TO " << table_name;
    stmt = create_statement_from_sql(sql.str());
    execute_nonselect_statement(stmt);
}

/* This is required because we're passing be->timespace_format to
 * g_strdup_printf.
 */
#pragma GCC diagnostic ignored "-Wformat-nonliteral"
std::string
GncSqlBackend::time64_to_string (time64 t) const noexcept
{
    auto tm = gnc_gmtime (&t);

    auto year = tm->tm_year + 1900;

    auto datebuf = g_strdup_printf (m_timespec_format,
                                    year, tm->tm_mon + 1, tm->tm_mday,
                                    tm->tm_hour, tm->tm_min, tm->tm_sec);
    gnc_tm_free (tm);
    std::string date{datebuf};
    g_free(datebuf);
    return date;
}
