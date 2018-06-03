/********************************************************************
 * gnc-dbisqlconnection.cpp: Encapsulate libdbi dbi_conn            *
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

#include <guid.hpp>
extern "C"
{
#include <config.h>
#include <platform.h>
#include <gnc-locale-utils.h>
}

#include <string>
#include <regex>
#include <sstream>

#include "gnc-dbisqlconnection.hpp"

static QofLogModule log_module = G_LOG_DOMAIN;
// gnc-dbiproviderimpl.hpp has templates that need log_module defined.
#include "gnc-dbiproviderimpl.hpp"

static const unsigned int DBI_MAX_CONN_ATTEMPTS = 5;
const std::string lock_table = "gnclock";

/* --------------------------------------------------------- */
class GncDbiSqlStatement : public GncSqlStatement
{
public:
    GncDbiSqlStatement(const GncSqlConnection* conn, const std::string& sql) :
        m_conn{conn}, m_sql {sql} {}
    ~GncDbiSqlStatement() {}
    const char* to_sql() const override;
    void add_where_cond(QofIdTypeConst, const PairVec&) override;

private:
    const GncSqlConnection* m_conn;
    std::string m_sql;
};


const char*
GncDbiSqlStatement::to_sql() const
{
    return m_sql.c_str();
}

void
GncDbiSqlStatement::add_where_cond(QofIdTypeConst type_name,
                                   const PairVec& col_values)
{
    m_sql += " WHERE ";
    for (auto colpair : col_values)
    {
        if (colpair != *col_values.begin())
            m_sql += " AND ";
        if (colpair.second == "NULL")
            m_sql += colpair.first + " IS " + colpair.second;
        else
            m_sql += colpair.first + " = " + colpair.second;
    }
}

GncDbiSqlConnection::GncDbiSqlConnection (DbType type, QofBackend* qbe,
                                          dbi_conn conn, bool ignore_lock) :
    m_qbe{qbe}, m_conn{conn},
    m_provider{type == DbType::DBI_SQLITE ?
            make_dbi_provider<DbType::DBI_SQLITE>() :
            type == DbType::DBI_MYSQL ?
            make_dbi_provider<DbType::DBI_MYSQL>() :
            make_dbi_provider<DbType::DBI_PGSQL>()},
    m_conn_ok{true}, m_last_error{ERR_BACKEND_NO_ERR}, m_error_repeat{0},
    m_retry{false}, m_sql_savepoint{0}
{
    if (!lock_database(ignore_lock))
        throw std::runtime_error("Failed to lock database!");
    if (!check_and_rollback_failed_save())
    {
        unlock_database();
        throw std::runtime_error("A failed safe-save was detected and rolling it back failed.");
    }
}

bool
GncDbiSqlConnection::lock_database (bool ignore_lock)
{
    const char *errstr;
    /* Protect everything with a single transaction to prevent races */
    if (!begin_transaction())
        return false;
    auto tables = m_provider->get_table_list(m_conn, lock_table);
    if (tables.empty())
    {
        auto result = dbi_conn_queryf (m_conn,
                                       "CREATE TABLE %s ( Hostname varchar(%d), PID int )",
                                       lock_table.c_str(),
                                       GNC_HOST_NAME_MAX);
        if (result)
        {
            dbi_result_free (result);
            result = nullptr;
        }
        if (dbi_conn_error (m_conn, &errstr))
        {
            PERR ("Error %s creating lock table", errstr);
            qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
            return false;
        }
    }

    /* Check for an existing entry; delete it if ignore_lock is true, otherwise fail */
    char hostname[ GNC_HOST_NAME_MAX + 1 ];
    auto result = dbi_conn_queryf (m_conn, "SELECT * FROM %s",
                                   lock_table.c_str());
    if (result && dbi_result_get_numrows (result))
    {
        dbi_result_free (result);
        result = nullptr;
        if (!ignore_lock)
        {
            qof_backend_set_error (m_qbe, ERR_BACKEND_LOCKED);
            /* FIXME: After enhancing the qof_backend_error mechanism, report in the dialog what is the hostname of the machine holding the lock. */
            rollback_transaction();
            return false;
        }
        result = dbi_conn_queryf (m_conn, "DELETE FROM %s", lock_table.c_str());
        if (!result)
        {
            qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
            m_qbe->set_message("Failed to delete lock record");
            rollback_transaction();
            return false;
        }
        dbi_result_free (result);
        result = nullptr;
    }
    /* Add an entry and commit the transaction */
    memset (hostname, 0, sizeof (hostname));
    gethostname (hostname, GNC_HOST_NAME_MAX);
    result = dbi_conn_queryf (m_conn,
                              "INSERT INTO %s VALUES ('%s', '%d')",
                              lock_table.c_str(), hostname, (int)GETPID ());
    if (!result)
    {
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
        m_qbe->set_message("Failed to create lock record");
        rollback_transaction();
        return false;
    }
    dbi_result_free (result);
    return commit_transaction();
}

void
GncDbiSqlConnection::unlock_database ()
{
    if (m_conn == nullptr) return;
    g_return_if_fail (dbi_conn_error (m_conn, nullptr) == 0);

    auto tables = m_provider->get_table_list (m_conn, lock_table);
    if (tables.empty())
    {
        PWARN ("No lock table in database, so not unlocking it.");
        return;
    }
    if (begin_transaction())
    {
        /* Delete the entry if it's our hostname and PID */
        char hostname[ GNC_HOST_NAME_MAX + 1 ];

        memset (hostname, 0, sizeof (hostname));
        gethostname (hostname, GNC_HOST_NAME_MAX);
        auto result = dbi_conn_queryf (m_conn,
                                       "SELECT * FROM %s WHERE Hostname = '%s' "
                                       "AND PID = '%d'", lock_table.c_str(),
                                       hostname,
                                       (int)GETPID ());
        if (result && dbi_result_get_numrows (result))
        {
            if (result)
            {
                dbi_result_free (result);
                result = nullptr;
            }
            result = dbi_conn_queryf (m_conn, "DELETE FROM %s",
                                      lock_table.c_str());
            if (!result)
            {
                PERR ("Failed to delete the lock entry");
                m_qbe->set_error (ERR_BACKEND_SERVER_ERR);
                rollback_transaction();
                return;
            }
            else
            {
                dbi_result_free (result);
                result = nullptr;
            }
            commit_transaction();
            return;
        }
        rollback_transaction();
        PWARN ("There was no lock entry in the Lock table");
        return;
    }
    PWARN ("Unable to get a lock on LOCK, so failed to clear the lock entry.");
    m_qbe->set_error (ERR_BACKEND_SERVER_ERR);
}

bool
GncDbiSqlConnection::check_and_rollback_failed_save()
{
    auto backup_tables = m_provider->get_table_list(m_conn, "%back");
    if (backup_tables.empty())
        return true;
    auto merge_tables = m_provider->get_table_list(m_conn, "%_merge");
    if (!merge_tables.empty())
    {
        PERR("Merge tables exist in the database indicating a previous"
             "attempt to recover from a failed safe-save. Automatic"
             "recovery is beyond GnuCash's ability, you must recover"
             "by hand or restore from a good backup.");
        return false;
    }
    return table_operation(recover);
}

GncDbiSqlConnection::~GncDbiSqlConnection()
{
    if (m_conn)
    {
        unlock_database();
        dbi_conn_close(m_conn);
        m_conn = nullptr;
    }
}

GncSqlResultPtr
GncDbiSqlConnection::execute_select_statement (const GncSqlStatementPtr& stmt)
    noexcept
{
    dbi_result result;

    DEBUG ("SQL: %s\n", stmt->to_sql());
    auto locale = gnc_push_locale (LC_NUMERIC, "C");
    do
    {
        init_error ();
        result = dbi_conn_query (m_conn, stmt->to_sql());
    }
    while (m_retry);
    if (result == nullptr)
    {
        PERR ("Error executing SQL %s\n", stmt->to_sql());
        if(m_last_error)
            m_qbe->set_error(m_last_error);
        else
            m_qbe->set_error(ERR_BACKEND_SERVER_ERR);
    }
    gnc_pop_locale (LC_NUMERIC, locale);
    return GncSqlResultPtr(new GncDbiSqlResult (this, result));
}

int
GncDbiSqlConnection::execute_nonselect_statement (const GncSqlStatementPtr& stmt)
    noexcept
{
    dbi_result result;

    DEBUG ("SQL: %s\n", stmt->to_sql());
    do
    {
        init_error ();
        result = dbi_conn_query (m_conn, stmt->to_sql());
    }
    while (m_retry);
    if (result == nullptr && m_last_error)
    {
        PERR ("Error executing SQL %s\n", stmt->to_sql());
        if(m_last_error)
            m_qbe->set_error(m_last_error);
        else
            m_qbe->set_error(ERR_BACKEND_SERVER_ERR);
        return -1;
    }
    if (!result)
        return 0;
    auto num_rows = (gint)dbi_result_get_numrows_affected (result);
    auto status = dbi_result_free (result);
    if (status < 0)
    {
        PERR ("Error in dbi_result_free() result\n");
        if(m_last_error)
            m_qbe->set_error(m_last_error);
        else
            m_qbe->set_error(ERR_BACKEND_SERVER_ERR);
    }
    return num_rows;
}

GncSqlStatementPtr
GncDbiSqlConnection::create_statement_from_sql (const std::string& sql)
    const noexcept
{
    return std::unique_ptr<GncSqlStatement>{new GncDbiSqlStatement (this, sql)};
}

bool
GncDbiSqlConnection::does_table_exist (const std::string& table_name)
    const noexcept
{
    return ! m_provider->get_table_list(m_conn, table_name).empty();
}

bool
GncDbiSqlConnection::begin_transaction () noexcept
{
    dbi_result result;

    DEBUG ("BEGIN\n");

    if (!verify ())
    {
        PERR ("gnc_dbi_verify_conn() failed\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
        return false;
    }

    do
    {
        init_error ();
        if (m_sql_savepoint == 0)
            result = dbi_conn_queryf (m_conn, "BEGIN");
        else
        {
            std::ostringstream savepoint;
            savepoint << "savepoint_" << m_sql_savepoint;
            result = dbi_conn_queryf(m_conn, "SAVEPOINT %s",
                                     savepoint.str().c_str());
        }
    }
    while (m_retry);

    if (!result)
    {
        PERR ("BEGIN transaction failed()\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
        return false;
    }
    if (dbi_result_free (result) < 0)
    {
        PERR ("Error in dbi_result_free() result\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
        return false;
    }
    ++m_sql_savepoint;
    return true;
}

bool
GncDbiSqlConnection::rollback_transaction () noexcept
{
    DEBUG ("ROLLBACK\n");
    if (m_sql_savepoint == 0) return false;
    dbi_result result;
    if (m_sql_savepoint == 1)
        result = dbi_conn_query (m_conn, "ROLLBACK");
    else
    {
        std::ostringstream savepoint;
        savepoint << "savepoint_" << m_sql_savepoint - 1;
        result = dbi_conn_queryf(m_conn, "ROLLBACK TO SAVEPOINT %s",
                                 savepoint.str().c_str());
    }
    if (!result)
    {
        PERR ("Error in conn_rollback_transaction()\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
        return false;
    }

    if (dbi_result_free (result) < 0)
    {
        PERR ("Error in dbi_result_free() result\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
        return false;
    }

    --m_sql_savepoint;
    return true;
}

bool
GncDbiSqlConnection::commit_transaction () noexcept
{
    DEBUG ("COMMIT\n");
    if (m_sql_savepoint == 0) return false;
    dbi_result result;
    if (m_sql_savepoint == 1)
        result = dbi_conn_queryf (m_conn, "COMMIT");
    else
    {
        std::ostringstream savepoint;
        savepoint << "savepoint_" << m_sql_savepoint - 1;
        result = dbi_conn_queryf(m_conn, "RELEASE SAVEPOINT %s",
                                 savepoint.str().c_str());
    }

    if (!result)
    {
        PERR ("Error in conn_commit_transaction()\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
        return false;
    }

    if (dbi_result_free (result) < 0)
    {
        PERR ("Error in dbi_result_free() result\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
        return false;
    }
    --m_sql_savepoint;
    return true;
}


bool
GncDbiSqlConnection::create_table (const std::string& table_name,
                                   const ColVec& info_vec) const noexcept
{
    std::string ddl;
    unsigned int col_num = 0;

    ddl += "CREATE TABLE " + table_name + "(";
    for (auto const& info : info_vec)
    {
        if (col_num++ != 0)
        {
            ddl += ", ";
        }
        m_provider->append_col_def (ddl, info);
    }
    ddl += ")";

    if (ddl.empty())
        return false;

    DEBUG ("SQL: %s\n", ddl.c_str());
    auto result = dbi_conn_query (m_conn, ddl.c_str());
    auto status = dbi_result_free (result);
    if (status < 0)
    {
        PERR ("Error in dbi_result_free() result\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
    }

    return true;
}

static std::string
create_index_ddl (const GncSqlConnection* conn, const std::string& index_name,
                  const std::string& table_name, const EntryVec& col_table)
{
    std::string ddl;
    ddl += "CREATE INDEX " + index_name + " ON " + table_name + "(";
    for (auto const table_row : col_table)
    {
        if (table_row != *col_table.begin())
        {
            ddl =+ ", ";
        }
        ddl += table_row->name();
    }
    ddl += ")";
    return ddl;
}

bool
GncDbiSqlConnection::create_index(const std::string& index_name,
                                  const std::string& table_name,
                                  const EntryVec& col_table) const noexcept
{
    auto ddl = create_index_ddl (this, index_name, table_name, col_table);
    if (ddl.empty())
        return false;
    DEBUG ("SQL: %s\n", ddl.c_str());
    auto result = dbi_conn_query (m_conn, ddl.c_str());
    auto status = dbi_result_free (result);
    if (status < 0)
    {
        PERR ("Error in dbi_result_free() result\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
    }

    return true;
}

bool
GncDbiSqlConnection::add_columns_to_table(const std::string& table_name,
                                          const ColVec& info_vec)
    const noexcept
{
    auto ddl = add_columns_ddl(table_name, info_vec);
    if (ddl.empty())
        return false;

    DEBUG ("SQL: %s\n", ddl.c_str());
    auto result = dbi_conn_query (m_conn, ddl.c_str());
    auto status = dbi_result_free (result);
    if (status < 0)
    {
        PERR( "Error in dbi_result_free() result\n" );
        qof_backend_set_error(m_qbe, ERR_BACKEND_SERVER_ERR );
    }

    return true;
}

std::string
GncDbiSqlConnection::quote_string (const std::string& unquoted_str)
    const noexcept
{
    char* quoted_str;
    size_t size;

    size = dbi_conn_quote_string_copy (m_conn, unquoted_str.c_str(),
                                       &quoted_str);
    if (quoted_str == nullptr)
        return std::string{""};
    std::string retval{quoted_str};
    free(quoted_str);
    return retval;
}


/** Check if the dbi connection is valid. If not attempt to re-establish it
 * Returns TRUE is there is a valid connection in the end or FALSE otherwise
 */
bool
GncDbiSqlConnection::verify () noexcept
{
    if (m_conn_ok)
        return true;

    /* We attempt to connect only once here. The error function will
     * automatically re-attempt up until DBI_MAX_CONN_ATTEMPTS time to connect
     * if this call fails.  After all these attempts, conn_ok will indicate if
     * there is a valid connection or not.
     */
    init_error ();
    m_conn_ok = true;
    (void)dbi_conn_connect (m_conn);

    return m_conn_ok;
}

bool
GncDbiSqlConnection::retry_connection(const char* msg)
    noexcept
{
    while (m_retry && m_error_repeat <= DBI_MAX_CONN_ATTEMPTS)
    {
        m_conn_ok = false;
        if (dbi_conn_connect(m_conn) == 0)
        {
            init_error();
            m_conn_ok = true;
            return true;
        }
#ifdef G_OS_WIN32
        const guint backoff_msecs = 1;
        Sleep (backoff_msecs * 2 << ++m_error_repeat);
#else
        const guint backoff_usecs = 1000;
        usleep (backoff_usecs * 2 << ++m_error_repeat);
#endif
        PINFO ("DBI error: %s - Reconnecting...\n", msg);

    }
    PERR ("DBI error: %s - Giving up after %d consecutive attempts.\n", msg,
          DBI_MAX_CONN_ATTEMPTS);
    m_conn_ok = false;
    return false;
}

bool
GncDbiSqlConnection::rename_table(const std::string& old_name,
                                  const std::string& new_name)
{
    std::string sql = "ALTER TABLE " + old_name + " RENAME TO " + new_name;
    auto stmt = create_statement_from_sql(sql);
    return execute_nonselect_statement(stmt) >= 0;
}

bool
GncDbiSqlConnection::drop_table(const std::string& table)
{
    std::string sql = "DROP TABLE " + table;
    auto stmt = create_statement_from_sql(sql);
    return execute_nonselect_statement(stmt) >= 0;
}

bool
GncDbiSqlConnection::merge_tables(const std::string& table,
                                  const std::string& other)
{
    auto merge_table = table + "_merge";
    std::string sql = "CREATE TABLE " + merge_table + " AS SELECT * FROM " +
        table + " UNION SELECT * FROM " + other;
    auto stmt = create_statement_from_sql(sql);
    if (execute_nonselect_statement(stmt) < 0)
        return false;
    if (!drop_table(table))
        return false;
    if (!rename_table(merge_table, table))
        return false;
    return drop_table(other);
}

/**
 * Perform a specified SQL operation on every table in a
 * database. Possible operations are:
 * * drop: to DROP all tables from the database
 * * empty: to DELETE all records from each table in the database.
 * * backup: Rename every table from "name" to "name_back"
 * * drop_backup: DROP the backup tables.
 * * rollback: DROP the new table "name" and rename "name_back" to
 *   "name", restoring the database to its previous state.
 *
 * The intent of the last two is to be able to move an existing table
 * aside, query its contents with a transformation (in 2.4.x this is
 * already done as the contents are loaded completely when a Qof
 * session is started), save them to a new table according to a new
 * database format, and finally drop the backup table; if there's an
 * error during the process, rollback allows returning the table to
 * its original state.
 *
 * @param sql_conn: The sql connection (via dbi) to which the
 * transactions will be sent
 * @param table_namess: StrVec of tables to operate on.
 * @param op: The operation to perform.
 * @return Success (TRUE) or failure.
 */

bool
GncDbiSqlConnection::table_operation(TableOpType op) noexcept
{
    auto backup_tables = m_provider->get_table_list(m_conn, "%_back");
    auto all_tables = m_provider->get_table_list(m_conn, "");
    /* No operations on the lock table */
    auto new_end = std::remove(all_tables.begin(), all_tables.end(), lock_table);
    all_tables.erase(new_end, all_tables.end());
    StrVec data_tables;
    data_tables.reserve(all_tables.size() - backup_tables.size());
    std::set_difference(all_tables.begin(), all_tables.end(),
                        backup_tables.begin(), backup_tables.end(),
                        std::back_inserter(data_tables));
    switch(op)
    {
    case backup:
        if (!backup_tables.empty())
        {
            PERR("Unable to backup database, an existing backup is present.");
            qof_backend_set_error(m_qbe, ERR_BACKEND_DATA_CORRUPT);
            return false;
        }
        for (auto table : data_tables)
            if (!rename_table(table, table +"_back"))
                return false; /* Error, trigger rollback. */
        break;
    case drop_backup:
        for (auto table : backup_tables)
        {
            auto data_table = table.substr(0, table.find("_back"));
            if (std::find(data_tables.begin(), data_tables.end(),
                          data_table) != data_tables.end())
                drop_table(table); /* Other table exists, OK. */
            else /* No data table, restore the backup */
                rename_table(table, data_table);
        }
        break;
    case rollback:
        for (auto table : backup_tables)
        {
            auto data_table = table.substr(0, table.find("_back"));
            if (std::find(data_tables.begin(), data_tables.end(),
                          data_table) != data_tables.end())
                drop_table(data_table); /* Other table exists, OK. */
            rename_table(table, data_table);
        }
        break;
    case recover:
        for (auto table : backup_tables)
        {
            auto data_table = table.substr(0, table.find("_back"));
            if (std::find(data_tables.begin(), data_tables.end(),
                          data_table) != data_tables.end())
            {
                if (!merge_tables(data_table, table))
                    return false;
            }
            else
            {
                if (!rename_table(table, data_table))
                    return false;
            }
        }
        break;
    }
   return true;
}

bool
GncDbiSqlConnection::drop_indexes() noexcept
{
    auto index_list = m_provider->get_index_list (m_conn);
    for (auto index : index_list)
    {
        const char* errmsg;
        m_provider->drop_index (m_conn, index);
        if (DBI_ERROR_NONE != dbi_conn_error (m_conn, &errmsg))
        {
            PERR("Failed to drop indexes %s", errmsg);
            return false;
        }
    }
    return true;
}

std::string
GncDbiSqlConnection::add_columns_ddl(const std::string& table_name,
                                     const ColVec& info_vec) const noexcept
{
    std::string ddl;

    ddl += "ALTER TABLE " + table_name;
    for (auto const& info : info_vec)
    {
        if (info != *info_vec.begin())
        {
            ddl += ", ";
        }
        ddl += "ADD COLUMN ";
        m_provider->append_col_def (ddl, info);
    }
    return ddl;
}
