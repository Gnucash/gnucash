/********************************************************************
 * gnc-backend-dbi.hpp: load and save data to SQL via libdbi     *
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

/* Private structures and variables for gnc-backend-dbi.c and its unit tests */
#ifndef GNC_BACKEND_DBI_HPP
#define GNC_BACKEND_DBI_HPP
extern "C"
{
#include <dbi/dbi.h>
#ifdef G_OS_WIN32
#include <winsock2.h>
#define GETPID() GetCurrentProcessId()
#else
#include <limits.h>
#include <unistd.h>
#define GETPID() getpid()
#endif
}
#include <gnc-sql-backend.hpp>
#include <gnc-sql-connection.hpp>

class GncSqlRow;

#define GNC_HOST_NAME_MAX 255

/**
 * Options to conn_table_operation
 * @var drop Drop (remove without recourse) the table from the database
 * @var empty Delete all of the records from the table
 * @var backup Rename the table "name" to "name_back"
 * @var rollback drop the name table if it exists and rename name_back to name
 * @var drop_backup Drop the backup table
 */
enum TableOpType
{
    drop = 0,
    empty,
    backup,
    rollback,
    drop_backup
};

/**
 * Return values from conn_test_dbi_library
 * @var GNC_DBI_PASS Did not find the large numbers bug
 * @var GNC_DBI_FAIL_SETUP Could not completed the test
 * @var GNC_DBI_FAIL_TEST Found the large numbers bug
 */
typedef enum
{
    GNC_DBI_PASS = 0,
    GNC_DBI_FAIL_SETUP,
    GNC_DBI_FAIL_TEST
} GncDbiTestResult;

/**
 * Supported Dbi Backends.
 */
enum class DbType
{
    DBI_SQLITE, /**< Sqlite3 */
    DBI_MYSQL,  /**< MySQL and probably MariaDB */
    DBI_PGSQL   /**< Postgresql */
};

/**
 * Implementations of GncSqlBackend.
 */
class GncDbiBackend : public GncSqlBackend
{
public:
    GncDbiBackend(GncSqlConnection *conn, QofBook* book) :
        GncSqlBackend(conn, book), m_exists{false} {}
    bool connected() const noexcept { return m_conn != nullptr; }
    /** FIXME: Just a pass-through to m_conn: */
    void set_error(int error, unsigned int repeat,  bool retry) noexcept
    {
        m_conn->set_error(error, repeat, retry);
    }
    void retry_connection(const char* msg) const noexcept
    {
        m_conn->retry_connection(msg);
    }
    /*-----*/
    bool exists() { return m_exists; }
    void set_exists(bool exists) { m_exists = exists; }
    friend void gnc_dbi_load(QofBackend*, QofBook*, QofBackendLoadType);
    friend void gnc_dbi_safe_sync_all(QofBackend*, QofBook*);
private:
    bool m_exists;         // Does the database exist?
};


void gnc_dbi_safe_sync_all (QofBackend* qbe, QofBook* book);

/* external access required for tests */
std::string adjust_sql_options_string(const std::string&);



#endif //GNC_BACKEND_DBI_HPP
