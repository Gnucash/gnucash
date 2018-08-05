/***********************************************************************\
 * gnc-sql-connection.hpp: Encapsulate a SQL database connection.      *
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

#ifndef __GNC_SQL_CONNECTION_HPP__
#define __GNC_SQL_CONNECTION_HPP__

extern "C"
{
#include <qof.h>
}
#include <memory>
#include <string>
#include <vector>

class GncSqlResult;
using GncSqlResultPtr = GncSqlResult*;
class GncSqlColumnTableEntry;
using GncSqlColumnTableEntryPtr = std::shared_ptr<GncSqlColumnTableEntry>;
using EntryVec = std::vector<GncSqlColumnTableEntryPtr>;
using PairVec = std::vector<std::pair<std::string, std::string>>;
struct GncSqlColumnInfo;
using ColVec = std::vector<GncSqlColumnInfo>;

/**
 * SQL statement provider.
 */
class GncSqlStatement
{
public:
    virtual ~GncSqlStatement() {}
    virtual const char* to_sql() const = 0;
    virtual void add_where_cond (QofIdTypeConst, const PairVec&) = 0;
};

using GncSqlStatementPtr = std::unique_ptr<GncSqlStatement>;

/**
 * Encapsulate the connection to the database. This is an abstract class; the
 * implementation is database-specific.
 */
class GncSqlConnection
{
public:
    /** Returns NULL if error */
    virtual ~GncSqlConnection() = default;
    virtual GncSqlResultPtr execute_select_statement (const GncSqlStatementPtr&)
        noexcept = 0;
    /** Returns false if error */
    virtual int execute_nonselect_statement (const GncSqlStatementPtr&)
        noexcept = 0;
    virtual GncSqlStatementPtr create_statement_from_sql (const std::string&)
        const noexcept = 0;
    /** Returns true if successful */
    virtual bool does_table_exist (const std::string&) const noexcept = 0;
    /** Returns TRUE if successful, false if error */
    virtual bool begin_transaction () noexcept = 0;
    /** Returns TRUE if successful, FALSE if error */
    virtual bool rollback_transaction () noexcept = 0;
    /** Returns TRUE if successful, FALSE if error */
    virtual bool commit_transaction () noexcept = 0;
    /** Returns TRUE if successful, FALSE if error */
    virtual bool create_table (const std::string&, const ColVec&)
        const noexcept = 0;
    /** Returns TRUE if successful, FALSE if error */
    virtual bool create_index (const std::string&, const std::string&,
                               const EntryVec&) const noexcept = 0;
    /** Returns TRUE if successful, FALSE if error */
    virtual bool add_columns_to_table (const std::string&, const ColVec&)
        const noexcept = 0;
    virtual std::string quote_string (const std::string&)
        const noexcept = 0;
    /** Get the connection error value.
     * If not 0 will normally be meaningless outside of implementation code.
     */
    virtual int dberror() const noexcept = 0;
    virtual void set_error(QofBackendError error, unsigned int repeat,
                           bool retry) noexcept = 0;
    virtual bool verify() noexcept = 0;
    virtual bool retry_connection(const char* msg) noexcept = 0;

};


#endif //__GNC_SQL_CONNECTION_HPP__
