/***********************************************************************\
 * gnc-sql-backend.hpp: Qof Backend for SQL Databases                  *
 *                                                                     *
 * Copyright 2016 John Ralls <jralls@ceridwen.us>                      *
 *                                                                     *
 * This program is free software; you can redistribute it and/or       *
 * modify it under the terms of the GNU General Public License as      *
 * published by the Free Software Foundation; either version 2 of      *
 * the License, or (at your option) any later version.                 *
 *                                                                     *
 * This program is distributed in the hope that it will be useful,     *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of      *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *
 * GNU General Public License for more details.                        *
 *                                                                     *
 * You should have received a copy of the GNU General Public License   *
 * along with this program; if not, contact:                           *
 *                                                                     *
 * Free Software Foundation           Voice:  +1-617-542-5942          *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652          *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                      *
\***********************************************************************/

#ifndef __GNC_SQL_BACKEND_HPP__
#define __GNC_SQL_BACKEND_HPP__

extern "C"
{
#include <qof.h>
#include <qofbackend-p.h>
#include <Account.h>
}
#include <memory>
#include <exception>
#include <sstream>
#include <vector>
class GncSqlColumnTableEntry;
using GncSqlColumnTableEntryPtr = std::shared_ptr<GncSqlColumnTableEntry>;
using EntryVec = std::vector<GncSqlColumnTableEntryPtr>;
class GncSqlObjectBackend;
using GncSqlObjectBackendPtr = GncSqlObjectBackend*;
using OBEEntry = std::tuple<std::string, GncSqlObjectBackendPtr>;
using OBEVec = std::vector<OBEEntry>;
class GncSqlConnection;
class GncSqlStatement;
using GncSqlStatementPtr = std::unique_ptr<GncSqlStatement>;
class GncSqlResult;
using GncSqlResultPtr = GncSqlResult*;
using VersionPair = std::pair<const std::string, unsigned int>;
using VersionVec = std::vector<VersionPair>;
using uint_t = unsigned int;

/**
 *
 * Main SQL backend structure.
 */
class GncSqlBackend
{
public:
    GncSqlBackend(GncSqlConnection *conn, QofBook* book,
                  const char* format = nullptr);
    virtual ~GncSqlBackend() = default;
    /** Connect the backend to a GncSqlConnection.
     * Sets up version info. Calling with nullptr clears the connection and
     * destroys the version info.
     */
    void connect(GncSqlConnection *conn) noexcept;
    /**
     * Initializes DB table version information.
     */
    void init_version_info() noexcept;
    bool reset_version_info() noexcept;
    /**
     * Finalizes DB table version information.
     */
    void finalize_version_info() noexcept;
    /* FIXME: These are just pass-throughs of m_conn functions. */
    GncSqlStatementPtr create_statement_from_sql(const std::string& str) const noexcept;
    /** Executes an SQL SELECT statement and returns the result rows.  If an
     * error occurs, an entry is added to the log, an error status is returned
     * to qof and nullptr is returned.
     *
     * @param statement Statement
     * @return Results, or nullptr if an error has occured
     */
    GncSqlResultPtr execute_select_statement(const GncSqlStatementPtr& stmt) const noexcept;
    int execute_nonselect_statement(const GncSqlStatementPtr& stmt) const noexcept;
    std::string quote_string(const std::string&) const noexcept;
    /**
     * Creates a table in the database
     *
     * @param table_name Table name
     * @param col_table DB table description
     * @return TRUE if successful, FALSE if unsuccessful
     */
    bool create_table(const std::string& table_name, const EntryVec& col_table) const noexcept;
    /**
     * Creates a table in the database and sets its version
     *
     * @param table_name Table name
     * @param table_version Table version
     * @param col_table DB table description
     * @return TRUE if successful, FALSE if unsuccessful
     */
    bool create_table(const std::string& table_name, int table_version,
                      const EntryVec& col_table) noexcept;
    /**
     * Create/update all tables in the database
     */
    void create_tables() noexcept;

    /**
     * Creates an index in the database
     *
     * @param index_name Index name
     * @param table_name Table name
     * @param col_table Columns that the index should index
     * @return TRUE if successful, FALSE if unsuccessful
     */
    bool create_index(const std::string& index_name,
                      const std::string& table_name,
                      const EntryVec& col_table) const noexcept;
    /**
     * Adds one or more columns to an existing table.
     *
     * @param table_name SQL table name
     * @param new_col_table Column table for new columns
     * @return TRUE if successful, FALSE if unsuccessful
     */
    bool add_columns_to_table(const std::string& table_name,
                              const EntryVec& col_table) const noexcept;
    /**
     * Upgrades a table to a new structure.
     *
     * The upgrade is done by creating a new table with the new structure,
     * SELECTing the old data into the new table, deleting the old table, then
     * renaming the new table.  Therefore, this will only work if the new table
     * structure is similar enough to the old table that the SELECT will work.
     *
     * @param table_name SQL table name
     * @param col_table Column table
     */
    void upgrade_table (const std::string& table_name,
                        const EntryVec& col_table) noexcept;
    /**
     * Returns the version number for a DB table.
     *
     * @param table_name Table name
     * @return Version number, or 0 if the table does not exist
     */
    uint_t get_table_version(const std::string& table_name) const noexcept;
    bool set_table_version (const std::string& table_name, uint_t version) noexcept;
    /**
     * Converts a time64 value to a string value for the database.
     *
     * @param t time64 to be converted.
     * @return String representation of the Timespec
     */
    std::string time64_to_string (time64 t) const noexcept;
    QofBook* book() const noexcept { return m_book; }
    void set_loading(bool loading) noexcept { m_loading = loading; }
    bool pristine() const noexcept { return m_is_pristine_db; }
    void update_progress() const noexcept;
    void finish_progress() const noexcept;

    friend void gnc_sql_load (GncSqlBackend* sql_be,  QofBook* book, QofBackendLoadType loadType);
    friend void gnc_sql_sync_all (GncSqlBackend* sql_be,  QofBook* book);
    friend void gnc_sql_commit_edit (GncSqlBackend* sql_be, QofInstance* inst);

 protected:
    QofBackend qof_be;           /**< QOF backend. Not a pointer, nor really a member */
    GncSqlConnection* m_conn;  /**< SQL connection */
    QofBook* m_book;           /**< The primary, main open book */
    bool m_loading;        /**< We are performing an initial load */
    bool m_in_query;       /**< We are processing a query */
    bool m_is_pristine_db; /**< Are we saving to a new pristine db? */
    const char* m_timespec_format; /**< Server-specific date-time string format */
    VersionVec m_versions;    /**< Version number for each table */
};

#endif //__GNC_SQL_BACKEND_HPP__
