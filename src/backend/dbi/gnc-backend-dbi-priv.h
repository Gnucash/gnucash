/********************************************************************
 * gnc-backend-dbi-priv.h: load and save data to SQL via libdbi     *
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
#ifndef GNC_BACKEND_DBI_PRIV_H
#define GNC_BACKEND_DBI_PRIV_H
#ifdef __cplusplus
extern "C"
{
#endif
#include <dbi/dbi.h>
#ifdef __cplusplus
}
#endif
#include "gnc-backend-sql.h"

enum class DbType
{
    DBI_SQLITE,
    DBI_MYSQL,
    DBI_PGSQL
};

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

typedef gchar* (*CREATE_TABLE_DDL_FN)   (const GncSqlConnection* conn,
                                         const gchar* table_name,
                                         const ColVec& info_vec);
typedef GSList* (*GET_TABLE_LIST_FN)    (dbi_conn conn, const gchar* dbname);
typedef void    (*APPEND_COLUMN_DEF_FN) (GString* ddl,
                                         const GncSqlColumnInfo& info);
typedef GSList* (*GET_INDEX_LIST_FN)    (dbi_conn conn);
typedef void    (*DROP_INDEX_FN)        (dbi_conn conn, const gchar* index);
typedef struct
{
    CREATE_TABLE_DDL_FN     create_table_ddl;
    GET_TABLE_LIST_FN       get_table_list;
    APPEND_COLUMN_DEF_FN    append_col_def;
    GET_INDEX_LIST_FN       get_index_list;
    DROP_INDEX_FN           drop_index;
} provider_functions_t;

/**
 * Implementations of GncSqlBackend.
 */
struct GncDbiBackend_struct
{
    GncSqlBackend sql_be;

    dbi_conn conn;

    QofBook* primary_book;  /* The primary, main open book */
    gboolean    loading;        /* We are performing an initial load */
    gboolean  in_query;
    gboolean  supports_transactions;
    gboolean  is_pristine_db;   // Are we saving to a new pristine db?
    gboolean  exists;         // Does the database exist?

    gint obj_total;         // Total # of objects (for percentage calculation)
    gint operations_done;       // Number of operations (save/load) done
//  GHashTable* versions;       // Version number for each table
};

typedef struct GncDbiBackend_struct GncDbiBackend;

class GncDbiSqlConnection : public GncSqlConnection
{
public:
    GncDbiSqlConnection (provider_functions_t* provider, QofBackend* qbe,
                         dbi_conn conn) :
        m_qbe{qbe}, m_conn{conn}, m_provider{provider}, m_conn_ok{true},
        m_last_error{ERR_BACKEND_NO_ERR}, m_error_repeat{0}, m_retry{false} {}
    ~GncDbiSqlConnection() override = default;
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
    QofBackend* qbe () const noexcept { return m_qbe; }
    dbi_conn conn() const noexcept { return m_conn; }
    provider_functions_t* provider() { return m_provider; }
    inline void set_error (int error, int repeat,  bool retry) noexcept
    {
        m_last_error = error;
        m_error_repeat = repeat;
        m_retry = retry;
    }
    inline void init_error () noexcept
    {
        set_error(ERR_BACKEND_NO_ERR, 0, false);
    }
    /** Check if the dbi connection is valid. If not attempt to re-establish it
     * Returns TRUE is there is a valid connection in the end or FALSE otherwise
     */
    bool verify() noexcept;
    bool retry_connection(const char* msg) noexcept;
    dbi_result table_manage_backup(const std::string& table_name, TableOpType op);
    /* FIXME: These three friend functions should really be members, but doing
     * that is too invasive just yet. */
    friend gboolean conn_table_operation (GncSqlConnection* sql_conn,
                                          GSList* table_name_list,
                                          TableOpType op);
    friend void gnc_dbi_safe_sync_all (QofBackend* qbe, QofBook* book);
    friend gchar* add_columns_ddl(const GncSqlConnection* conn,
                                  const gchar* table_name,
                                  const ColVec& info_vec);

private:
    QofBackend* m_qbe;
    dbi_conn m_conn;
    provider_functions_t* m_provider;
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

};

gboolean conn_table_operation (GncSqlConnection* sql_conn,
                               GSList* table_name_list, TableOpType op);
void gnc_dbi_safe_sync_all (QofBackend* qbe, QofBook* book);
gchar* add_columns_ddl(const GncSqlConnection* conn, const gchar* table_name,
                       const ColVec& info_vec);

/* external access required for tests */
std::string adjust_sql_options_string(const std::string&);


class GncDbiSqlResult : public GncSqlResult
{
public:
    GncDbiSqlResult(const GncDbiSqlConnection* conn, dbi_result result) :
        m_conn{conn}, m_dbi_result{result}, m_iter{this}, m_row{&m_iter},
        m_sentinel{nullptr} {}
    ~GncDbiSqlResult();
    int dberror();
    uint64_t size() const noexcept;
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


#endif //GNC_BACKEND_DBI_PRIV_H
