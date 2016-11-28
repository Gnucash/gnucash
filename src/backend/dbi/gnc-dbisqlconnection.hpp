/********************************************************************
 * gnc-dbisqlconnection.hpp: Encapsulate libdbi dbi_conn            *
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
#ifndef _GNC_DBISQLCONNECTION_HPP_
#define _GNC_DBISQLCONNECTION_HPP_

#include <string>
#include <vector>

#include <gnc-sql-connection.hpp>
#include "gnc-backend-dbi.hpp"
#include "gnc-dbisqlresult.hpp"
#include "gnc-dbiprovider.hpp"
#include "gnc-backend-dbi.h"

using StrVec = std::vector<std::string>;
class GncDbiProvider;

/**
 * Encapsulate a libdbi dbi_conn connection.
 */
class GncDbiSqlConnection : public GncSqlConnection
{
public:
    GncDbiSqlConnection (DbType type, QofBackend* qbe, dbi_conn conn,
                         bool ignore_lock);
    ~GncDbiSqlConnection() override;
    GncSqlResultPtr execute_select_statement (const GncSqlStatementPtr&)
        noexcept override;
    int execute_nonselect_statement (const GncSqlStatementPtr&)
        noexcept override;
    GncSqlStatementPtr create_statement_from_sql (const std::string&)
        const noexcept override;
    bool does_table_exist (const std::string&) const noexcept override;
    bool begin_transaction () noexcept override;
    bool rollback_transaction () const noexcept override;
    bool commit_transaction () const noexcept override;
    bool create_table (const std::string&, const ColVec&) const noexcept override;
    bool create_index (const std::string&, const std::string&, const EntryVec&)
        const noexcept override;
    bool add_columns_to_table (const std::string&, const ColVec&)
        const noexcept override;
    std::string quote_string (const std::string&) const noexcept override;
    int dberror() const noexcept override {
        return dbi_conn_error(m_conn, nullptr); }
    QofBackend* qbe () const noexcept { return m_qbe; }
    dbi_conn conn() const noexcept { return m_conn; }
    inline void set_error(int error, unsigned int repeat,
			  bool retry) noexcept override
    {
        m_last_error = error;
        m_error_repeat = repeat;
        m_retry = retry;
    }
    inline void init_error() noexcept
    {
        set_error(ERR_BACKEND_NO_ERR, 0, false);
    }
    /** Check if the dbi connection is valid. If not attempt to re-establish it
     * Returns TRUE is there is a valid connection in the end or FALSE otherwise
     */
    bool verify() noexcept override;
    bool retry_connection(const char* msg) noexcept override;
    dbi_result table_manage_backup(const std::string& table_name, TableOpType op);
    bool table_operation (TableOpType op) noexcept;
    std::string add_columns_ddl(const std::string& table_name,
                                const ColVec& info_vec) const noexcept;
    bool drop_indexes() noexcept;
private:
    QofBackend* m_qbe;
    dbi_conn m_conn;
    std::unique_ptr<GncDbiProvider> m_provider;
    /** Used by the error handler routines to flag if the connection is ok to
     * use
     */
    bool m_conn_ok;
    /** Code of the last error that occurred. This is set in the error callback
     * function.
     */
    int m_last_error;
    /** Used in case of transient errors. After such error, another attempt at
     * the original call is allowed. error_repeat tracks the number of attempts
     * and can be used to prevent infinite loops.
     */
    unsigned int m_error_repeat;
    /** Signals the calling function that it should retry (the error handler
     * detected transient error and managed to resolve it, but it can't run the
     * original query)
     */
    gboolean m_retry;
    bool lock_database(bool ignore_lock);
    void unlock_database();

};

#endif //_GNC_DBISQLCONNECTION_HPP_
