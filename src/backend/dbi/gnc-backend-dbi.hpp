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
#include <gnc-backend-sql.h>
#define GNC_HOST_NAME_MAX 255

/**
 * Options to conn_table_operation
 * @var drop Drop (remove without recourse) the table from the database
 * @var empty Delete all of the records from the table
 * @var backup Rename the table "name" to "name_back"
 * @var rollback drop the name table if it exists and rename name_back to name
 * @var drop_backup Drop the backup table
 */
typedef enum
{
    drop = 0,
    empty,
    backup,
    rollback,
    drop_backup
} TableOpType;

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

class GncDbiProvider
{
public:
    virtual ~GncDbiProvider() = default;
    virtual std::string create_table_ddl(const GncSqlConnection* conn,
                                         const std::string& table_name,
                                         const ColVec& info_vec) = 0;
    virtual std::vector<std::string> get_table_list(dbi_conn conn,
                                                    const std::string& dbname) = 0;
    virtual void append_col_def(std::string& ddl,
                                const GncSqlColumnInfo& info) = 0;
    virtual std::vector<std::string> get_index_list (dbi_conn conn) = 0;
    virtual void drop_index(dbi_conn conn, const std::string& index) = 0;
};

/**
 * Implementations of GncSqlBackend.
 */
class GncDbiBackend : public GncSqlBackend
{
public:
    GncDbiBackend(GncSqlConnection *conn, QofBook* book,
                  const char* format = nullptr) :
        GncSqlBackend(conn, book, format), m_exists{false} {}
    bool connected() const noexcept { return m_conn != nullptr; }
    /** FIXME: Just a pass-through to m_conn: */
    void set_error(int error, int repeat,  bool retry) noexcept
    {
        m_conn->set_error(error, repeat, retry);
    }
    void retry_connection(const char* msg) const noexcept
    {
        m_conn->retry_connection(msg);
    }
    /* Worst of all: */
    GncSqlConnection* conn() { return m_conn; }
    /*-----*/
    bool exists() { return m_exists; }
    void set_exists(bool exists) { m_exists = exists; }
    friend void gnc_dbi_load(QofBackend*, QofBook*, QofBackendLoadType);
    friend void gnc_dbi_safe_sync_all(QofBackend*, QofBook*);
private:
    bool m_exists;         // Does the database exist?
};

class GncDbiSqlConnection : public GncSqlConnection
{
public:
    GncDbiSqlConnection (GncDbiProvider* provider, QofBackend* qbe,
                         dbi_conn conn, const char* lock_table) :
        m_qbe{qbe}, m_conn{conn}, m_provider{provider}, m_conn_ok{true},
        m_last_error{ERR_BACKEND_NO_ERR}, m_error_repeat{0}, m_retry{false},
        m_lock_table{lock_table} {}
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
    GncDbiProvider* provider() { return m_provider; }
    inline void set_error(int error, int repeat,  bool retry) noexcept override
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
    /* FIXME: These three friend functions should really be members, but doing
     * that is too invasive just yet. */
    bool table_operation (const std::vector<std::string>& table_name_list,
                          TableOpType op) noexcept;
    std::string add_columns_ddl(const std::string& table_name,
                                const ColVec& info_vec) const noexcept;
    friend void gnc_dbi_safe_sync_all (QofBackend* qbe, QofBook* book);

private:
    QofBackend* m_qbe;
    dbi_conn m_conn;
    GncDbiProvider* m_provider;
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
    int m_error_repeat;
    /** Signals the calling function that it should retry (the error handler
     * detected transient error and managed to resolve it, but it can't run the
     * original query)
     */
    gboolean m_retry;
    const char* m_lock_table;
    void unlock_database();

};

void gnc_dbi_safe_sync_all (QofBackend* qbe, QofBook* book);
std::string add_columns_ddl(const GncSqlConnection* conn,
                            const std::string& table_name,
                            const ColVec& info_vec);

/* external access required for tests */
std::string adjust_sql_options_string(const std::string&);

/**
 * An iterable wrapper for dbi_result; allows using C++11 range for.
 */
class GncDbiSqlResult : public GncSqlResult
{
public:
    GncDbiSqlResult(const GncDbiSqlConnection* conn, dbi_result result) :
        m_conn{conn}, m_dbi_result{result}, m_iter{this}, m_row{&m_iter},
        m_sentinel{nullptr} {}
    ~GncDbiSqlResult();
    uint64_t size() const noexcept;
    int dberror() { return m_conn->dberror(); }
    GncSqlRow& begin();
    GncSqlRow& end() { return m_sentinel; }
protected:
    class IteratorImpl : public GncSqlResult::IteratorImpl
        {
        public:
            ~IteratorImpl() = default;
            IteratorImpl(GncDbiSqlResult* inst) : m_inst{inst} {}
            virtual GncSqlRow& operator++();
            virtual GncSqlRow& operator++(int) { return ++(*this); };
            virtual GncSqlResult* operator*() { return m_inst; }
            virtual int64_t get_int_at_col (const char* col) const;
            virtual float get_float_at_col (const char* col) const;
            virtual double get_double_at_col (const char* col) const;
            virtual std::string get_string_at_col (const char* col)const;
            virtual time64 get_time64_at_col (const char* col) const;
            virtual bool is_col_null(const char* col) const noexcept
            {
                return dbi_result_field_is_null(m_inst->m_dbi_result, col);
            }
        private:
            GncDbiSqlResult* m_inst;
        };
private:
    const GncDbiSqlConnection* m_conn;
    dbi_result m_dbi_result;
    IteratorImpl m_iter;
    GncSqlRow m_row;
    GncSqlRow m_sentinel;

};


#endif //GNC_BACKEND_DBI_HPP
