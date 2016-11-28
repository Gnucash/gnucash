/********************************************************************
 * utest-gnc-backend-sql.c:                                         *
 *             GLib g_test test suite for gnc-backend-sql.c.        *
 * Copyright 2012 John Ralls <jralls@ceridwen.us>           *
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
********************************************************************/
extern "C"
{
#include "config.h"
#include <string.h>
#include <glib.h>
#include <unittest-support.h>
}
/* Add specific headers for this class */
#include "../gnc-sql-connection.hpp"
#include "../gnc-sql-backend.hpp"
#include "../gnc-sql-result.hpp"

static const gchar* suitename = "/backend/sql/gnc-backend-sql";
void test_suite_gnc_backend_sql (void);

class GncMockSqlBackend : public GncSqlBackend
{
public:
    GncMockSqlBackend(GncSqlConnection* conn, QofBook* book) :
        GncSqlBackend(conn, book) {}
    void session_begin(QofSession*, const char*, bool, bool, bool) override {}
    void session_end() override {}
    void safe_sync(QofBook* book) override { sync(book); }
};

class GncMockSqlConnection;

class GncMockSqlResult : public GncSqlResult
{
public:
    GncMockSqlResult(const GncMockSqlConnection* conn) :
        m_conn{conn}, m_iter{this}, m_row{&m_iter} {}
    uint64_t size() const noexcept { return 1; }
    GncSqlRow& begin() { return m_row; }
    GncSqlRow& end() { return m_row; }
protected:
    class IteratorImpl : public GncSqlResult::IteratorImpl
        {
        public:
            ~IteratorImpl() = default;
            IteratorImpl(GncMockSqlResult* inst) : m_inst{inst} {}
            virtual GncSqlRow& operator++() { return m_inst->m_row; }
            virtual GncSqlRow& operator++(int) { return ++(*this); };
            virtual GncSqlResult* operator*() { return m_inst; }
            virtual int64_t get_int_at_col (const char* col) const
            { return 1LL; }
            virtual float get_float_at_col (const char* col) const
            { return 1.0; }
            virtual double get_double_at_col (const char* col) const
            { return 1.0; }
            virtual std::string get_string_at_col (const char* col)const
            { return std::string{"foo"}; }
            virtual time64 get_time64_at_col (const char* col) const
            { return 1466270857LL; }
            virtual bool is_col_null(const char* col) const noexcept
            { return false; }
        private:
            GncMockSqlResult* m_inst;
        };
private:
    const GncMockSqlConnection* m_conn;
    IteratorImpl m_iter;
    GncSqlRow m_row;
};

class GncMockSqlStatement : public GncSqlStatement
{
public:
    const char* to_sql() const { return "SELECT * FROM foo"; }
    void add_where_cond (QofIdTypeConst, const PairVec&) {}
};


class GncMockSqlConnection : public GncSqlConnection
{
public:
    GncMockSqlConnection() : m_result{this} {}
    GncSqlResultPtr execute_select_statement (const GncSqlStatementPtr&)
        noexcept override { return &m_result; }
    int execute_nonselect_statement (const GncSqlStatementPtr&)
        noexcept override { return 1; }
    GncSqlStatementPtr create_statement_from_sql (const std::string&)
        const noexcept override {
        return std::unique_ptr<GncMockSqlStatement>(new GncMockSqlStatement); }
    bool does_table_exist (const std::string&) const noexcept override {
        return true; }
    bool begin_transaction () noexcept override { return true;}
    bool rollback_transaction () const noexcept override { return true; }
    bool commit_transaction () const noexcept override { return true; }
    bool create_table (const std::string&, const ColVec&)
        const noexcept override { return false; }
    bool create_index (const std::string&, const std::string&,
                       const EntryVec&) const noexcept override { return false; }
    bool add_columns_to_table (const std::string&, const ColVec&)
        const noexcept override { return false; }
    virtual std::string quote_string (const std::string& str)
        const noexcept override { return std::string{str}; }
    int dberror() const noexcept override { return 0; }
    void set_error(int error, unsigned int repeat, bool retry) noexcept override { return; }
    bool verify() noexcept override { return true; }
    bool retry_connection(const char* msg) noexcept override { return true; }
private:
    GncMockSqlResult m_result;
};

/* gnc_sql_init
void
gnc_sql_init (GncSqlBackend* sql_be)// C: 1 */
/* static void
test_gnc_sql_init (Fixture *fixture, gconstpointer pData)
{
}*/
/* create_tables_cb
static void
create_tables_cb (const gchar* type, gpointer data_p, gpointer be_p)// 2
*/
/* static void
test_create_tables_cb (Fixture *fixture, gconstpointer pData)
{
}*/
/* initial_load_cb
static void
initial_load_cb (const gchar* type, gpointer data_p, gpointer be_p)// 2
*/
/* static void
test_initial_load_cb (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_load
void
gnc_sql_load (GncSqlBackend* sql_be,  QofBook *book, QofBackendLoadType loadType)// C: 1 */
/* static void
test_gnc_sql_load (Fixture *fixture, gconstpointer pData)
{
}*/
/* write_account_tree
static gboolean
write_account_tree (GncSqlBackend* sql_be, Account* root)// 3
*/
/* static void
test_write_account_tree (Fixture *fixture, gconstpointer pData)
{
}*/
/* write_accounts
static gboolean
write_accounts (GncSqlBackend* sql_be)// 2
*/
/* static void
test_write_accounts (Fixture *fixture, gconstpointer pData)
{
}*/
/* write_tx
static int
write_tx (Transaction* tx, gpointer data)// 3
*/
/* static void
test_write_tx (Fixture *fixture, gconstpointer pData)
{
}*/
/* write_transactions
static gboolean
write_transactions (GncSqlBackend* sql_be)// 2
*/
/* static void
test_write_transactions (Fixture *fixture, gconstpointer pData)
{
}*/
/* write_template_transactions
static gboolean
write_template_transactions (GncSqlBackend* sql_be)// 2
*/
/* static void
test_write_template_transactions (Fixture *fixture, gconstpointer pData)
{
}*/
/* write_schedXactions
static gboolean
write_schedXactions (GncSqlBackend* sql_be)// 2
*/
/* static void
test_write_schedXactions (Fixture *fixture, gconstpointer pData)
{
}*/
/* write_cb
static void
write_cb (const gchar* type, gpointer data_p, gpointer be_p)// 2
*/
/* static void
test_write_cb (Fixture *fixture, gconstpointer pData)
{
}*/
/* update_progress
static void
update_progress (GncSqlBackend* sql_be)// 17
*/
/* static void
test_update_progress (Fixture *fixture, gconstpointer pData)
{
}*/
/* finish_progress
static void
finish_progress (GncSqlBackend* sql_be)// 4
*/
/* static void
test_finish_progress (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_sync_all
void
gnc_sql_sync_all (GncSqlBackend* sql_be,  QofBook *book)// C: 2 in 1 */
/* static void
test_gnc_sql_sync_all (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_begin_edit
void
gnc_sql_begin_edit (GncSqlBackend *sql_be, QofInstance *inst)// C: 1 */
/* static void
test_gnc_sql_begin_edit (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_rollback_edit
void
gnc_sql_rollback_edit (GncSqlBackend *sql_be, QofInstance *inst)// C: 1 */
/* static void
test_gnc_sql_rollback_edit (Fixture *fixture, gconstpointer pData)
{
}*/
/* commit_cb
static void
commit_cb (const gchar* type, gpointer data_p, gpointer be_data_p)// 2
*/
/* static void
test_commit_cb (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_commit_edit
void
gnc_sql_commit_edit (GncSqlBackend *sql_be, QofInstance *inst)// C: 1 */
static void
test_dirty_cb (QofBook* book, gboolean dirty, gpointer data)
{
    g_assert (data != NULL);
    if (dirty)
        ++* (guint*)data;
    else if (* (guint*)data)
        --* (guint*)data;
}

static void
test_gnc_sql_commit_edit (void)
{
    QofInstance* inst;
    guint dirty_called = 0;
    GncMockSqlConnection conn;
    const char* msg1 =
        "[GncSqlBackend::commit()] Unknown object type 'null'\n";
    GLogLevelFlags loglevel = static_cast<decltype (loglevel)>
                              (G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL);
    const char* logdomain = "gnc.backend.sql";
    TestErrorStruct check1 = { loglevel, const_cast<char*> (logdomain),
                               const_cast<char*> (msg1), 0
                             };
    guint hdlr1;

    test_add_error (&check1);
    hdlr1 = g_log_set_handler (logdomain, loglevel,
                               (GLogFunc)test_list_handler, NULL);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_list_handler, NULL);

    qof_object_initialize ();
    auto book = qof_book_new();
    auto sql_be = new GncMockSqlBackend
        (&conn, book);
    inst  = static_cast<decltype (inst)> (g_object_new (QOF_TYPE_INSTANCE, NULL));
    qof_instance_init_data (inst, QOF_ID_NULL, book);
    qof_book_set_dirty_cb (book, test_dirty_cb, &dirty_called);
    qof_instance_set_dirty_flag (inst, TRUE);
    qof_book_mark_session_dirty (book);

    g_assert (qof_instance_get_dirty_flag (inst));
    g_assert (qof_book_session_not_saved (book));
    g_assert_cmpint (dirty_called, == , 1);
    sql_be->commit(inst);
    g_assert (!qof_instance_get_dirty_flag (inst));
    g_assert (!qof_book_session_not_saved (book));
    g_assert_cmpint (dirty_called, == , 0);
    g_assert_cmpint (check1.hits, == , 2);

    qof_book_mark_session_dirty (book);

    g_assert (!qof_instance_get_dirty_flag (QOF_INSTANCE (book)));
    g_assert (qof_book_session_not_saved (book));
    g_assert_cmpint (dirty_called, == , 1);
    sql_be->commit(QOF_INSTANCE (book));
    g_assert (!qof_instance_get_dirty_flag (QOF_INSTANCE (book)));
    g_assert (qof_book_session_not_saved (book));
    g_assert_cmpint (dirty_called, == , 1);
    g_assert_cmpint (check1.hits, == , 2);

    qof_instance_set_dirty_flag (QOF_INSTANCE (book), TRUE);

    g_assert (qof_instance_get_dirty_flag (QOF_INSTANCE (book)));
    g_assert (qof_book_session_not_saved (book));
    g_assert_cmpint (dirty_called, == , 1);
    sql_be->commit(QOF_INSTANCE (book));
    g_assert (!qof_instance_get_dirty_flag (QOF_INSTANCE (book)));
    g_assert (!qof_book_session_not_saved (book));
    g_assert_cmpint (dirty_called, == , 0);
    g_assert_cmpint (check1.hits, == , 2);

    g_log_remove_handler (logdomain, hdlr1);
    g_object_unref (inst);
    g_object_unref (book);
    delete sql_be;
}
/* handle_and_term
static void
handle_and_term (QofQueryTerm* pTerm, GString* sql)// 2
*/
/* static void
test_handle_and_term (Fixture *fixture, gconstpointer pData)
{
}*/
/* compile_query_cb
static void
compile_query_cb (const gchar* type, gpointer data_p, gpointer be_data_p)// 2
*/
/* static void
test_compile_query_cb (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* gnc_sql_compile_query
gpointer
gnc_sql_compile_query (QofBackend* qof_be, QofQuery* pQuery)// 4
*/
/* static void
test_gnc_sql_compile_query (Fixture *fixture, gconstpointer pData)
{
}*/
/* convert_search_obj
static const gchar*
convert_search_obj (QofIdType objType)// 3
*/
/* static void
test_convert_search_obj (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* gnc_sql_compile_query_to_sql
gchar*
gnc_sql_compile_query_to_sql (GncSqlBackend* sql_be, QofQuery* query)// 3
*/
/* static void
test_gnc_sql_compile_query_to_sql (Fixture *fixture, gconstpointer pData)
{
}*/
/* free_query_cb
static void
free_query_cb (const gchar* type, gpointer data_p, gpointer be_data_p)// 2
*/
/* static void
test_free_query_cb (Fixture *fixture, gconstpointer pData)
{
}*/
// Not Used
/* gnc_sql_free_query
void
gnc_sql_free_query (QofBackend* qof_be, gpointer pQuery)// 1
*/
/* static void
test_gnc_sql_free_query (Fixture *fixture, gconstpointer pData)
{
}*/
/* run_query_cb
static void
run_query_cb (const gchar* type, gpointer data_p, gpointer be_data_p)// 2
*/
/* static void
test_run_query_cb (Fixture *fixture, gconstpointer pData)
{
}*/
// Not Used
/* gnc_sql_run_query
void
gnc_sql_run_query (QofBackend* pBEnd, gpointer pQuery)// 1
*/
/* static void
test_gnc_sql_run_query (Fixture *fixture, gconstpointer pData)
{
}*/
/* business_core_sql_init
static void
business_core_sql_init(void)// 2
*/
/* static void
test_business_core_sql_init (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_init_object_handlers
static void
gnc_sql_init_object_handlers (void)// 3
*/
/* static void
test_gnc_sql_init_object_handlers (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* get_autoinc_id
get_autoinc_id()// 2
*/
/* static void
test_get_autoinc_id (Fixture *fixture, gconstpointer pData)
{
}*/
/* set_autoinc_id
static void
set_autoinc_id()// 2
*/
/* static void
test_set_autoinc_id (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_get_getter
gnc_sql_get_getter (QofIdTypeConst obj_name, const GncSqlColumnTableEntry& table_row)// C: 3 in 2 */
/* static void
test_gnc_sql_get_getter (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_string
static void
load_string (const GncSqlBackend* sql_be, GncSqlRow& row,
const GncSqlColumnTableEntry& table_row)// 2
*/
/* static void
test_load_string (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_int
static void
load_int (const GncSqlBackend* sql_be, GncSqlRow& row,// 4
*/
/* static void
test_load_int (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_int_col_info_to_list
static void
add_int_col_info_to_list (const GncSqlBackend* sql_be, const GncSqlColumnTableEntry& table_row,// 2
*/
/* static void
test_add_int_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_gvalue_int_to_slist
static void
add_gvalue_int_to_slist (const GncSqlBackend* sql_be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_gvalue_int_to_slist (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_boolean
static void
load_boolean (const GncSqlBackend* sql_be, GncSqlRow& row,// 2
*/
/* static void
test_load_boolean (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_boolean_col_info_to_list
static void
add_boolean_col_info_to_list (const GncSqlBackend* sql_be, const GncSqlColumnTableEntry& table_row,// 2
*/
/* static void
test_add_boolean_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_gvalue_boolean_to_slist
static void
add_gvalue_boolean_to_slist (const GncSqlBackend* sql_be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_gvalue_boolean_to_slist (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_int64
static void
load_int64 (const GncSqlBackend* sql_be, GncSqlRow& row,// 2
*/
/* static void
test_load_int64 (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_int64_col_info_to_list
static void
add_int64_col_info_to_list (const GncSqlBackend* sql_be, const GncSqlColumnTableEntry& table_row,// 2
*/
/* static void
test_add_int64_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_gvalue_int64_to_slist
static void
add_gvalue_int64_to_slist (const GncSqlBackend* sql_be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_gvalue_int64_to_slist (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_double
static void
load_double (const GncSqlBackend* sql_be, GncSqlRow& row,// 2
*/
/* static void
test_load_double (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_double_col_info_to_list
static void
add_double_col_info_to_list (const GncSqlBackend* sql_be, const GncSqlColumnTableEntry& table_row,// 2
*/
/* static void
test_add_double_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_gvalue_double_to_slist
static void
add_gvalue_double_to_slist (const GncSqlBackend* sql_be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_gvalue_double_to_slist (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_guid
static void
load_guid (const GncSqlBackend* sql_be, GncSqlRow& row,// 3
*/
/* static void
test_load_guid (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_guid_col_info_to_list
static void
add_guid_col_info_to_list (const GncSqlBackend* sql_be, const GncSqlColumnTableEntry& table_row,// 3
*/
/* static void
test_add_guid_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_gvalue_guid_to_slist
static void
add_gvalue_guid_to_slist (const GncSqlBackend* sql_be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_gvalue_guid_to_slist (Fixture *fixture, gconstpointer pData)
{
}*/
// Not Used
/* gnc_sql_add_objectref_guid_to_vec
void
gnc_sql_add_objectref_guid_to_vec (QofIdTypeConst obj_name,// 1
*/
/* static void
test_gnc_sql_add_objectref_guid_to_vec (Fixture *fixture, gconstpointer pData)
{
}*/
// Not Used
/* gnc_sql_add_objectref_guid_col_info_to_list
void
gnc_sql_add_objectref_guid_col_info_to_list (,// 1
*/
/* static void
test_gnc_sql_add_objectref_guid_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_timespec
static void
load_timespec (const GncSqlBackend* sql_be, GncSqlRow& row,// 2
*/
/* static void
test_load_timespec (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_timespec_col_info_to_list
static void
add_timespec_col_info_to_list (const GncSqlBackend* sql_be, const GncSqlColumnTableEntry& table_row,// 2
*/
/* static void
test_add_timespec_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_value_timespec_to_vec
static void
add_gvalue_timespec_to_slist (const GncSqlBackend* sql_be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_value_timespec_to_vec (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_date
static void
load_date (const GncSqlBackend* sql_be, GncSqlRow& row,// 2
*/
/* static void
test_load_date (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_date_col_info_to_list
static void
add_date_col_info_to_list (const GncSqlBackend* sql_be, const GncSqlColumnTableEntry& table_row,// 2
*/
/* static void
test_add_date_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_value_date_to_vec
static void
add_value_date_to_vec (const GncSqlBackend* sql_be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_value_date_to_vec (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_numeric
static void
load_numeric (const GncSqlBackend* sql_be, GncSqlRow& row,// 2
*/
/* static void
test_load_numeric (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_numeric_col_info_to_list
static void
add_numeric_col_info_to_list (const GncSqlBackend* sql_be, const GncSqlColumnTableEntry& table_row,// 2
*/
/* static void
test_add_numeric_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_value_numeric_to_vec
static void
add_value_numeric_to_vec (const GncSqlBackend* sql_be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_value_numeric_to_vec (Fixture *fixture, gconstpointer pData)
{
}*/
/* get_handler
get_handler (const GncSqlColumnTableEntry& table_row)// C: 1 */
/* static void
test_get_handler (Fixture *fixture, gconstpointer pData)
{
}*/
/* register_standard_col_type_handlers
static void
register_standard_col_type_handlers (void)// 3
*/
/* static void
test_register_standard_col_type_handlers (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* _retrieve_guid_
void
_retrieve_guid_ (gpointer pObject,  gpointer pValue)// 3
*/
/* static void
test__retrieve_guid_ (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_load_guid
const GncGUID*
gnc_sql_load_guid (const GncSqlBackend* sql_be, GncSqlRow& row)// C: 15 in 14 */
/* static void
test_gnc_sql_load_guid (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_load_object
void
gnc_sql_load_object (const GncSqlBackend* sql_be, GncSqlRow& row,// C: 29 in 19 */
/* static void
test_gnc_sql_load_object (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_create_select_statement
gnc_sql_create_select_statement (GncSqlBackend* sql_be, const gchar* table_name)// C: 16 in 16 */
/* static void
test_gnc_sql_create_select_statement (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* create_single_col_select_statement
create_single_col_select_statement (GncSqlBackend* sql_be,// 2
*/
/* static void
test_create_single_col_select_statement (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_execute_select_statement
gnc_sql_execute_select_statement (GncSqlBackend* sql_be, GncSqlStatement* stmt)// C: 25 in 19 */
/* static void
test_gnc_sql_execute_select_statement (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_create_statement_from_sql
gnc_sql_create_statement_from_sql (GncSqlBackend* sql_be, const gchar* sql)// C: 11 in 3 */
/* static void
test_gnc_sql_create_statement_from_sql (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_execute_select_sql
gnc_sql_execute_select_sql (GncSqlBackend* sql_be, const gchar* sql)// C: 1 */
/* static void
test_gnc_sql_execute_select_sql (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_execute_nonselect_sql
gint
gnc_sql_execute_nonselect_sql (GncSqlBackend* sql_be, const gchar* sql)// C: 1 */
/* static void
test_gnc_sql_execute_nonselect_sql (Fixture *fixture, gconstpointer pData)
{
}*/
/* execute_statement_get_count
static guint
execute_statement_get_count (GncSqlBackend* sql_be, GncSqlStatement* stmt)// 2
*/
/* static void
test_execute_statement_get_count (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_object_is_it_in_db
gboolean
gnc_sql_object_is_it_in_db (GncSqlBackend* sql_be, const gchar* table_name,// C: 1 */
/* static void
test_gnc_sql_object_is_it_in_db (Fixture *fixture, gconstpointer pData)
{
}*/
/* GncSqlBackend::do_db_operation
gboolean
GncSqlBackend::do_db_operation (GncSqlBackend* sql_be,// C: 22 in 12 */
/* static void
test_gnc_sql_do_db_operation (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_get_sql_value
gchar*
gnc_sql_get_sql_value (const GncSqlConnection* conn, const GValue* value)// C: 1 */
/* static void
test_gnc_sql_get_sql_value (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* build_insert_statement
build_insert_statement (GncSqlBackend* sql_be,// 3
*/
/* static void
test_build_insert_statement (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* build_update_statement
build_update_statement (GncSqlBackend* sql_be,// 3
*/
/* static void
test_build_update_statement (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* build_delete_statement
build_delete_statement (GncSqlBackend* sql_be,// 3
*/
/* static void
test_build_delete_statement (Fixture *fixture, gconstpointer pData)
{
}*/
/* do_create_table
static gboolean
do_create_table (const GncSqlBackend* sql_be, const gchar* table_name,// 5
*/
/* static void
test_do_create_table (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* gnc_sql_create_temp_table
gboolean
gnc_sql_create_temp_table (const GncSqlBackend* sql_be, const gchar* table_name,// 2
*/
/* static void
test_gnc_sql_create_temp_table (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_create_index
gboolean
gnc_sql_create_index (const GncSqlBackend* sql_be, const gchar* index_name,// C: 7 in 2 */
/* static void
test_gnc_sql_create_index (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_upgrade_table
void
gnc_sql_upgrade_table (GncSqlBackend* sql_be, const gchar* table_name,// C: 12 in 10 */
/* static void
test_gnc_sql_upgrade_table (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_add_columns_to_table
gboolean gnc_sql_add_columns_to_table (GncSqlBackend* sql_be, const gchar* table_name,// C: 1 */
/* static void
test_gnc_sql_add_columns_to_table (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_init_version_info
void
gnc_sql_init_version_info (GncSqlBackend* sql_be)// C: 1 */
/* static void
test_gnc_sql_init_version_info (Fixture *fixture, gconstpointer pData)
{
}*/
/* reset_version_info
static gboolean
reset_version_info (GncSqlBackend* sql_be)// 3
*/
/* static void
test_reset_version_info (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_finalize_version_info
void
gnc_sql_finalize_version_info (GncSqlBackend* sql_be)// C: 1 */
/* static void
test_gnc_sql_finalize_version_info (Fixture *fixture, gconstpointer pData)
{
}*/

void
test_suite_gnc_backend_sql (void)
{

// GNC_TEST_ADD (suitename, "gnc sql init", Fixture, nullptr, test_gnc_sql_init,  teardown);
// GNC_TEST_ADD (suitename, "create tables cb", Fixture, nullptr, test_create_tables_cb,  teardown);
// GNC_TEST_ADD (suitename, "initial load cb", Fixture, nullptr, test_initial_load_cb,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql load", Fixture, nullptr, test_gnc_sql_load,  teardown);
// GNC_TEST_ADD (suitename, "write account tree", Fixture, nullptr, test_write_account_tree,  teardown);
// GNC_TEST_ADD (suitename, "write accounts", Fixture, nullptr, test_write_accounts,  teardown);
// GNC_TEST_ADD (suitename, "write tx", Fixture, nullptr, test_write_tx,  teardown);
// GNC_TEST_ADD (suitename, "write transactions", Fixture, nullptr, test_write_transactions,  teardown);
// GNC_TEST_ADD (suitename, "write template transactions", Fixture, nullptr, test_write_template_transactions,  teardown);
// GNC_TEST_ADD (suitename, "write schedXactions", Fixture, nullptr, test_write_schedXactions,  teardown);
// GNC_TEST_ADD (suitename, "write cb", Fixture, nullptr, test_write_cb,  teardown);
// GNC_TEST_ADD (suitename, "update progress", Fixture, nullptr, test_update_progress,  teardown);
// GNC_TEST_ADD (suitename, "finish progress", Fixture, nullptr, test_finish_progress,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql sync all", Fixture, nullptr, test_gnc_sql_sync_all,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql begin edit", Fixture, nullptr, test_gnc_sql_begin_edit,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql rollback edit", Fixture, nullptr, test_gnc_sql_rollback_edit,  teardown);
// GNC_TEST_ADD (suitename, "commit cb", Fixture, nullptr, test_commit_cb,  teardown);
    GNC_TEST_ADD_FUNC (suitename, "gnc sql commit edit", test_gnc_sql_commit_edit);
// GNC_TEST_ADD (suitename, "handle and term", Fixture, nullptr, test_handle_and_term,  teardown);
// GNC_TEST_ADD (suitename, "compile query cb", Fixture, nullptr, test_compile_query_cb,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql compile query", Fixture, nullptr, test_gnc_sql_compile_query,  teardown);
// GNC_TEST_ADD (suitename, "convert search obj", Fixture, nullptr, test_convert_search_obj,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql compile query to sql", Fixture, nullptr, test_gnc_sql_compile_query_to_sql,  teardown);
// GNC_TEST_ADD (suitename, "free query cb", Fixture, nullptr, test_free_query_cb,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql free query", Fixture, nullptr, test_gnc_sql_free_query,  teardown);
// GNC_TEST_ADD (suitename, "run query cb", Fixture, nullptr, test_run_query_cb,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql run query", Fixture, nullptr, test_gnc_sql_run_query,  teardown);
// GNC_TEST_ADD (suitename, "business core sql init", Fixture, nullptr, test_business_core_sql_init,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql init object handlers", Fixture, nullptr, test_gnc_sql_init_object_handlers,  teardown);
// GNC_TEST_ADD (suitename, "get autoinc id", Fixture, nullptr, test_get_autoinc_id,  teardown);
// GNC_TEST_ADD (suitename, "set autoinc id", Fixture, nullptr, test_set_autoinc_id,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql get getter", Fixture, nullptr, test_gnc_sql_get_getter,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql add subtable colnames to list", Fixture, nullptr, test_gnc_sql_add_subtable_colnames_to_list,  teardown);
// GNC_TEST_ADD (suitename, "load string", Fixture, nullptr, test_load_string,  teardown);
// GNC_TEST_ADD (suitename, "add string col info to list", Fixture, nullptr, test_add_string_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add value string to vec", Fixture, nullptr, test_add_value_string_to_vec,  teardown);
// GNC_TEST_ADD (suitename, "load int", Fixture, nullptr, test_load_int,  teardown);
// GNC_TEST_ADD (suitename, "add int col info to list", Fixture, nullptr, test_add_int_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add value int to vec", Fixture, nullptr, test_add_value_int_to_vec,  teardown);
// GNC_TEST_ADD (suitename, "load boolean", Fixture, nullptr, test_load_boolean,  teardown);
// GNC_TEST_ADD (suitename, "add boolean col info to list", Fixture, nullptr, test_add_boolean_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add value boolean to vec", Fixture, nullptr, test_add_value_boolean_to_vec,  teardown);
// GNC_TEST_ADD (suitename, "load int64", Fixture, nullptr, test_load_int64,  teardown);
// GNC_TEST_ADD (suitename, "add int64 col info to list", Fixture, nullptr, test_add_int64_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add value int64 to vec", Fixture, nullptr, test_add_value_int64_to_vec,  teardown);
// GNC_TEST_ADD (suitename, "load double", Fixture, nullptr, test_load_double,  teardown);
// GNC_TEST_ADD (suitename, "add double col info to list", Fixture, nullptr, test_add_double_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add value double to vec", Fixture, nullptr, test_add_value_double_to_vec,  teardown);
// GNC_TEST_ADD (suitename, "load guid", Fixture, nullptr, test_load_guid,  teardown);
// GNC_TEST_ADD (suitename, "add guid col info to list", Fixture, nullptr, test_add_guid_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add value guid to vec", Fixture, nullptr, test_add_value_guid_to_vec,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql add gvalue objectref guid to slist", Fixture, nullptr, test_gnc_sql_add_objectref_guid_to_vec,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql add objectref guid col info to list", Fixture, nullptr, test_gnc_sql_add_objectref_guid_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "load timespec", Fixture, nullptr, test_load_timespec,  teardown);
// GNC_TEST_ADD (suitename, "add timespec col info to list", Fixture, nullptr, test_add_timespec_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add value timespec to vec", Fixture, nullptr, test_add_value_timespec_to_vec,  teardown);
// GNC_TEST_ADD (suitename, "load date", Fixture, nullptr, test_load_date,  teardown);
// GNC_TEST_ADD (suitename, "add date col info to list", Fixture, nullptr, test_add_date_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add value date to vec", Fixture, nullptr, test_add_value_date_to_vec,  teardown);
// GNC_TEST_ADD (suitename, "load numeric", Fixture, nullptr, test_load_numeric,  teardown);
// GNC_TEST_ADD (suitename, "add numeric col info to list", Fixture, nullptr, test_add_numeric_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add numeric colname to list", Fixture, nullptr, test_add_numeric_colname_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add value numeric to vec", Fixture, nullptr, test_add_value_numeric_to_vec,  teardown);
// GNC_TEST_ADD (suitename, "get handler", Fixture, nullptr, test_get_handler,  teardown);
// GNC_TEST_ADD (suitename, "register standard col type handlers", Fixture, nullptr, test_register_standard_col_type_handlers,  teardown);
// GNC_TEST_ADD (suitename, " retrieve guid ", Fixture, nullptr, test__retrieve_guid_,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql load guid", Fixture, nullptr, test_gnc_sql_load_guid,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql load tx guid", Fixture, nullptr, test_gnc_sql_load_tx_guid,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql load object", Fixture, nullptr, test_gnc_sql_load_object,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql create select statement", Fixture, nullptr, test_gnc_sql_create_select_statement,  teardown);
// GNC_TEST_ADD (suitename, "create single col select statement", Fixture, nullptr, test_create_single_col_select_statement,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql execute select statement", Fixture, nullptr, test_gnc_sql_execute_select_statement,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql create statement from sql", Fixture, nullptr, test_gnc_sql_create_statement_from_sql,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql execute select sql", Fixture, nullptr, test_gnc_sql_execute_select_sql,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql execute nonselect sql", Fixture, nullptr, test_gnc_sql_execute_nonselect_sql,  teardown);
// GNC_TEST_ADD (suitename, "execute statement get count", Fixture, nullptr, test_execute_statement_get_count,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql append guid list to sql", Fixture, nullptr, test_gnc_sql_append_guid_list_to_sql,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql object is it in db", Fixture, nullptr, test_gnc_sql_object_is_it_in_db,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql do db operation", Fixture, nullptr, test_gnc_sql_do_db_operation,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql get sql value", Fixture, nullptr, test_gnc_sql_get_sql_value,  teardown);
// GNC_TEST_ADD (suitename, "build insert statement", Fixture, nullptr, test_build_insert_statement,  teardown);
// GNC_TEST_ADD (suitename, "build update statement", Fixture, nullptr, test_build_update_statement,  teardown);
// GNC_TEST_ADD (suitename, "build delete statement", Fixture, nullptr, test_build_delete_statement,  teardown);
// GNC_TEST_ADD (suitename, "do create table", Fixture, nullptr, test_do_create_table,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql create index", Fixture, nullptr, test_gnc_sql_create_index,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql upgrade table", Fixture, nullptr, test_gnc_sql_upgrade_table,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql add columns to table", Fixture, nullptr, test_gnc_sql_add_columns_to_table,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql init version info", Fixture, nullptr, test_gnc_sql_init_version_info,  teardown);
// GNC_TEST_ADD (suitename, "reset version info", Fixture, nullptr, test_reset_version_info,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql finalize version info", Fixture, nullptr, test_gnc_sql_finalize_version_info,  teardown);

}
