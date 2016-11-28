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
        m_sql += colpair.first + " = " +
            m_conn->quote_string (colpair.second.c_str());
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
    m_retry{false}
{
    if (!lock_database(ignore_lock))
        throw std::runtime_error("Failed to lock database!");
}

bool
GncDbiSqlConnection::lock_database (bool ignore_lock)
{
    const char *errstr;
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

    /* Protect everything with a single transaction to prevent races */
    auto result = dbi_conn_query (m_conn, "BEGIN");
    if (dbi_conn_error (m_conn, &errstr))
    {
    /* Couldn't get a transaction (probably couldn't get a lock), so fail */
        std::string err{"SQL Backend failed to obtain a transaction: "};
        err += errstr;
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
        qof_backend_set_message (m_qbe, err.c_str());
        return false;
    }
    dbi_result_free(result);
    result = nullptr;
    /* Check for an existing entry; delete it if ignore_lock is true, otherwise fail */
    char hostname[ GNC_HOST_NAME_MAX + 1 ];
    result = dbi_conn_queryf (m_conn, "SELECT * FROM %s", lock_table.c_str());
    if (result && dbi_result_get_numrows (result))
    {
        dbi_result_free (result);
        result = nullptr;
        if (!ignore_lock)
        {
            qof_backend_set_error (m_qbe, ERR_BACKEND_LOCKED);
            /* FIXME: After enhancing the qof_backend_error mechanism, report in the dialog what is the hostname of the machine holding the lock. */
            dbi_conn_query (m_conn, "ROLLBACK");
            return false;
        }
        result = dbi_conn_queryf (m_conn, "DELETE FROM %s", lock_table.c_str());
        if (!result)
        {
            qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
            qof_backend_set_message (m_qbe, "Failed to delete lock record");
            result = dbi_conn_query (m_conn, "ROLLBACK");
            if (result)
                dbi_result_free (result);
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
        qof_backend_set_message (m_qbe, "Failed to create lock record");
        result = dbi_conn_query (m_conn, "ROLLBACK");
        if (result)
            dbi_result_free (result);
        return false;
    }
    dbi_result_free (result);
    result = dbi_conn_query (m_conn, "COMMIT");
    if (!result)
    {
        auto errnum = dbi_conn_error(m_conn, &errstr);
        qof_backend_set_error(m_qbe, ERR_BACKEND_SERVER_ERR);
        std::string err{"Failed to commit transaction: "};
        err += errstr;
        qof_backend_set_message(m_qbe, err.c_str());
        return false;
    }
    dbi_result_free (result);
    return true;
}

void
GncDbiSqlConnection::unlock_database ()
{
    GncDbiBackend* qe = reinterpret_cast<decltype(qe)>(m_qbe);

    if (m_conn == nullptr) return;
    g_return_if_fail (dbi_conn_error (m_conn, nullptr) == 0);

    auto tables = m_provider->get_table_list (m_conn, lock_table);
    if (tables.empty())
    {
        PWARN ("No lock table in database, so not unlocking it.");
        return;
    }
    auto result = dbi_conn_query (m_conn, "BEGIN");
    if (result)
    {
        /* Delete the entry if it's our hostname and PID */
        char hostname[ GNC_HOST_NAME_MAX + 1 ];

        dbi_result_free (result);
        result = nullptr;
        memset (hostname, 0, sizeof (hostname));
        gethostname (hostname, GNC_HOST_NAME_MAX);
        result = dbi_conn_queryf (m_conn,
                                  "SELECT * FROM %s WHERE Hostname = '%s' AND PID = '%d'", lock_table.c_str(), hostname,
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
                qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
                result = dbi_conn_query (m_conn, "ROLLBACK");
                if (result)
                {
                    dbi_result_free (result);
                    result = nullptr;
                }
                return;
            }
            else
            {
                dbi_result_free (result);
                result = nullptr;
            }
            result = dbi_conn_query (m_conn, "COMMIT");
            if (result)
            {
                dbi_result_free (result);
                result = nullptr;
            }
            return;
        }
        result = dbi_conn_query (m_conn, "ROLLBACK");
        if (result)
        {
            dbi_result_free (result);
            result = nullptr;
        }
        PWARN ("There was no lock entry in the Lock table");
        return;
    }
    if (result)
    {
        dbi_result_free (result);
        result = nullptr;
    }
    PWARN ("Unable to get a lock on LOCK, so failed to clear the lock entry.");
    qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
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
    gnc_push_locale (LC_NUMERIC, "C");
    do
    {
        init_error ();
        result = dbi_conn_query (m_conn, stmt->to_sql());
    }
    while (m_retry);
    if (result == nullptr)
        PERR ("Error executing SQL %s\n", stmt->to_sql());
    gnc_pop_locale (LC_NUMERIC);
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
        return -1;
    }
    if (!result)
        return 0;
    auto num_rows = (gint)dbi_result_get_numrows_affected (result);
    auto status = dbi_result_free (result);
    if (status < 0)
    {
        PERR ("Error in dbi_result_free() result\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
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
        return FALSE;
    }

    do
    {
        init_error ();
        result = dbi_conn_queryf (m_conn, "BEGIN");
    }
    while (m_retry);

    auto success = (result != nullptr);
    auto status = dbi_result_free (result);
    if (status < 0)
    {
        PERR ("Error in dbi_result_free() result\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
    }
    if (!success)
    {
        PERR ("BEGIN transaction failed()\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
    }

    return success;
}

bool
GncDbiSqlConnection::rollback_transaction () const noexcept
{
    DEBUG ("ROLLBACK\n");
    const char* command =  "ROLLBACK";
    auto result = dbi_conn_query (m_conn, command);
    auto success = (result != nullptr);

    auto status = dbi_result_free (result);
    if (status < 0)
    {
        PERR ("Error in dbi_result_free() result\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
    }
    if (!success)
    {
        PERR ("Error in conn_rollback_transaction()\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
    }

    return success;
}

bool
GncDbiSqlConnection::commit_transaction () const noexcept
{
    DEBUG ("COMMIT\n");
    auto result = dbi_conn_queryf (m_conn, "COMMIT");
    auto success = (result != nullptr);

    auto status = dbi_result_free (result);
    if (status < 0)
    {
        PERR ("Error in dbi_result_free() result\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
    }
    if (!success)
    {
        PERR ("Error in conn_commit_transaction()\n");
        qof_backend_set_error (m_qbe, ERR_BACKEND_SERVER_ERR);
    }

    return success;
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


dbi_result
GncDbiSqlConnection::table_manage_backup (const std::string& table_name,
                                          TableOpType op)
{
    auto new_name = table_name + "_back";
    dbi_result result = nullptr;
    switch (op)
    {
    case TableOpType::backup:
        result = dbi_conn_queryf (m_conn, "ALTER TABLE %s RENAME TO %s",
                                  table_name.c_str(), new_name.c_str());
        break;
    case TableOpType::rollback:
        result = dbi_conn_queryf (m_conn,
                                  "ALTER TABLE %s RENAME TO %s",
                                  new_name.c_str(), table_name.c_str());
        break;
    case TableOpType::drop_backup:
        result = dbi_conn_queryf (m_conn, "DROP TABLE %s",
                                  new_name.c_str());
        break;
    default:
        break;
    }
    return result;
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
    static const std::regex backupre (".*_back");
    bool retval{true};
    for (auto table : m_provider->get_table_list(m_conn, ""))
    {
        dbi_result result;
        /* Skip the lock table and existing backup tables; the former we don't
         * want to touch, the latter are handled by table_manage_backup. It
         * would be nicer to handle this with the get_table_list query, but that
         * can accept only SQL LIKE patterns (not even regexps) and there's no
         * way to have a negative one.
         */
        if (table == lock_table || std::regex_match(table, backupre))
        {
            continue;
        }
        do
        {
            init_error();
            switch (op)
            {
            case rollback:
            {
                auto all_tables = m_provider->get_table_list(m_conn, "");
                if (std::find(all_tables.begin(),
                              all_tables.end(), table) != all_tables.end())
                {
                    result = dbi_conn_queryf (m_conn, "DROP TABLE %s",
                                              table.c_str());
                    if (result)
                        break;
                }
            }
            /* Fall through to rename the _back tables back.*/
            case backup:
            case drop_backup:
                result = table_manage_backup (table, op);
                break;
            case empty:
                result = dbi_conn_queryf (m_conn, "DELETE FROM TABLE %s",
                                          table.c_str());
                break;
            case drop:
            default:
                result = dbi_conn_queryf (m_conn, "DROP TABLE %s", table.c_str());
                break;
            }
        }
        while (m_retry);

        if (result != nullptr)
        {
            if (dbi_result_free (result) < 0)
            {
                PERR ("Error in dbi_result_free() result\n");
                retval = false;
            }
        }
    }
    return retval;
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
