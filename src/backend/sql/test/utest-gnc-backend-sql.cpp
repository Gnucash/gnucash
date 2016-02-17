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
#include "../gnc-backend-sql.h"

static const gchar* suitename = "/backend/sql/gnc-backend-sql";
void test_suite_gnc_backend_sql (void);

/* gnc_sql_init
void
gnc_sql_init (GncSqlBackend* be)// C: 1 */
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
/* gnc_sql_set_load_order
void
gnc_sql_set_load_order (const gchar** load_order)// 2
*/
/* static void
test_gnc_sql_set_load_order (Fixture *fixture, gconstpointer pData)
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
gnc_sql_load (GncSqlBackend* be,  QofBook *book, QofBackendLoadType loadType)// C: 1 */
/* static void
test_gnc_sql_load (Fixture *fixture, gconstpointer pData)
{
}*/
/* write_account_tree
static gboolean
write_account_tree (GncSqlBackend* be, Account* root)// 3
*/
/* static void
test_write_account_tree (Fixture *fixture, gconstpointer pData)
{
}*/
/* write_accounts
static gboolean
write_accounts (GncSqlBackend* be)// 2
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
write_transactions (GncSqlBackend* be)// 2
*/
/* static void
test_write_transactions (Fixture *fixture, gconstpointer pData)
{
}*/
/* write_template_transactions
static gboolean
write_template_transactions (GncSqlBackend* be)// 2
*/
/* static void
test_write_template_transactions (Fixture *fixture, gconstpointer pData)
{
}*/
/* write_schedXactions
static gboolean
write_schedXactions (GncSqlBackend* be)// 2
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
update_progress (GncSqlBackend* be)// 17
*/
/* static void
test_update_progress (Fixture *fixture, gconstpointer pData)
{
}*/
/* finish_progress
static void
finish_progress (GncSqlBackend* be)// 4
*/
/* static void
test_finish_progress (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_sync_all
void
gnc_sql_sync_all (GncSqlBackend* be,  QofBook *book)// C: 2 in 1 */
/* static void
test_gnc_sql_sync_all (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_begin_edit
void
gnc_sql_begin_edit (GncSqlBackend *be, QofInstance *inst)// C: 1 */
/* static void
test_gnc_sql_begin_edit (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_rollback_edit
void
gnc_sql_rollback_edit (GncSqlBackend *be, QofInstance *inst)// C: 1 */
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
gnc_sql_commit_edit (GncSqlBackend *be, QofInstance *inst)// C: 1 */
static void
test_dirty_cb (QofBook* book, gboolean dirty, gpointer data)
{
    g_assert (data != NULL);
    if (dirty)
        ++* (guint*)data;
    else if (* (guint*)data)
        --* (guint*)data;
}

static gboolean
fake_connection_function (GncSqlConnection* conn)
{
    return TRUE;
}

static void
test_gnc_sql_commit_edit (void)
{
    GncSqlBackend be;
    QofInstance* inst;
    guint dirty_called = 0;
    GncSqlConnection conn;
    const char* msg1 =
        "[gnc_sql_commit_edit()] gnc_sql_commit_edit(): Unknown object type 'null'\n";
    const char* msg2 =
        "[gnc_sql_commit_edit()] gnc_sql_commit_edit(): Unknown object type 'Book'\n";
    GLogLevelFlags loglevel = static_cast<decltype (loglevel)>
                              (G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL);
    const char* logdomain = "gnc.backend.sql";
    TestErrorStruct check1 = { loglevel, const_cast<char*> (logdomain),
                               const_cast<char*> (msg1), 0
                             };
    TestErrorStruct check2 = { loglevel, const_cast<char*> (logdomain),
                               const_cast<char*> (msg2), 0
                             };
    guint hdlr1;

    test_add_error (&check1);
    test_add_error (&check2);
    hdlr1 = g_log_set_handler (logdomain, loglevel,
                               (GLogFunc)test_list_handler, NULL);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_list_handler, NULL);

    qof_object_initialize ();
    be.book = qof_book_new ();
    be.conn = &conn;
    conn.beginTransaction = fake_connection_function;
    conn.rollbackTransaction = fake_connection_function;
    conn.commitTransaction = fake_connection_function;
    inst  = static_cast<decltype (inst)> (g_object_new (QOF_TYPE_INSTANCE, NULL));
    qof_instance_init_data (inst, QOF_ID_NULL, be.book);
    be.loading = FALSE;
    qof_book_set_dirty_cb (be.book, test_dirty_cb, &dirty_called);
    qof_instance_set_dirty_flag (inst, TRUE);
    qof_book_mark_session_dirty (be.book);

    g_assert (qof_instance_get_dirty_flag (inst));
    g_assert (qof_book_session_not_saved (be.book));
    g_assert_cmpint (dirty_called, == , 1);
    gnc_sql_commit_edit (&be, inst);
    g_assert (!qof_instance_get_dirty_flag (inst));
    g_assert (!qof_book_session_not_saved (be.book));
    g_assert_cmpint (dirty_called, == , 0);
    g_assert_cmpint (check1.hits, == , 2);
    g_assert_cmpint (check2.hits, == , 0);

    qof_book_mark_session_dirty (be.book);

    g_assert (!qof_instance_get_dirty_flag (QOF_INSTANCE (be.book)));
    g_assert (qof_book_session_not_saved (be.book));
    g_assert_cmpint (dirty_called, == , 1);
    gnc_sql_commit_edit (&be, QOF_INSTANCE (be.book));
    g_assert (!qof_instance_get_dirty_flag (QOF_INSTANCE (be.book)));
    g_assert (qof_book_session_not_saved (be.book));
    g_assert_cmpint (dirty_called, == , 1);
    g_assert_cmpint (check1.hits, == , 2);
    g_assert_cmpint (check2.hits, == , 0);

    qof_instance_set_dirty_flag (QOF_INSTANCE (be.book), TRUE);

    g_assert (qof_instance_get_dirty_flag (QOF_INSTANCE (be.book)));
    g_assert (qof_book_session_not_saved (be.book));
    g_assert_cmpint (dirty_called, == , 1);
    gnc_sql_commit_edit (&be, QOF_INSTANCE (be.book));
    g_assert (!qof_instance_get_dirty_flag (QOF_INSTANCE (be.book)));
    g_assert (!qof_book_session_not_saved (be.book));
    g_assert_cmpint (dirty_called, == , 0);
    g_assert_cmpint (check1.hits, == , 2);
    g_assert_cmpint (check2.hits, == , 2);

    g_log_remove_handler (logdomain, hdlr1);
    g_object_unref (inst);
    g_object_unref (be.book);
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
gnc_sql_compile_query (QofBackend* pBEnd, QofQuery* pQuery)// 4
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
gnc_sql_compile_query_to_sql (GncSqlBackend* be, QofQuery* query)// 3
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
gnc_sql_free_query (QofBackend* pBEnd, gpointer pQuery)// 1
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
/* gnc_sql_get_integer_value
gint64
gnc_sql_get_integer_value (const GValue* value)// C: 1 */
/* static void
test_gnc_sql_get_integer_value (Fixture *fixture, gconstpointer pData)
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
gnc_sql_get_getter (QofIdTypeConst obj_name, const GncSqlColumnTableEntry* table_row)// C: 3 in 2 */
/* static void
test_gnc_sql_get_getter (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* gnc_sql_add_colname_to_list
void
gnc_sql_add_colname_to_list (const GncSqlColumnTableEntry* table_row, GList** pList)// 9
*/
/* static void
test_gnc_sql_add_colname_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_add_subtable_colnames_to_list
void
gnc_sql_add_subtable_colnames_to_list (const GncSqlColumnTableEntry* table_row, const GncSqlColumnTableEntry* subtable,
GList** pList)// C: 1 */
/* static void
test_gnc_sql_add_subtable_colnames_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* create_column_info
static GncSqlColumnInfo*
create_column_info (const GncSqlColumnTableEntry* table_row, GncSqlBasicColumnType type,
gint size, gboolean is_unicode)// 9
*/
/* static void
test_create_column_info (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_string
static void
load_string (const GncSqlBackend* be, GncSqlRow* row,
const GncSqlColumnTableEntry* table_row)// 2
*/
/* static void
test_load_string (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_string_col_info_to_list
static void
add_string_col_info_to_list (const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,
GList** pList)// 2
*/
/* static void
test_add_string_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_gvalue_string_to_slist
static void
add_gvalue_string_to_slist (const GncSqlBackend* be, QofIdTypeConst obj_name,
const gpointer pObject, const GncSqlColumnTableEntry* table_row, GSList** pList)// 2
*/
/* static void
test_add_gvalue_string_to_slist (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_int
static void
load_int (const GncSqlBackend* be, GncSqlRow* row,// 4
*/
/* static void
test_load_int (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_int_col_info_to_list
static void
add_int_col_info_to_list (const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,// 2
*/
/* static void
test_add_int_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_gvalue_int_to_slist
static void
add_gvalue_int_to_slist (const GncSqlBackend* be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_gvalue_int_to_slist (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_boolean
static void
load_boolean (const GncSqlBackend* be, GncSqlRow* row,// 2
*/
/* static void
test_load_boolean (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_boolean_col_info_to_list
static void
add_boolean_col_info_to_list (const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,// 2
*/
/* static void
test_add_boolean_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_gvalue_boolean_to_slist
static void
add_gvalue_boolean_to_slist (const GncSqlBackend* be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_gvalue_boolean_to_slist (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_int64
static void
load_int64 (const GncSqlBackend* be, GncSqlRow* row,// 2
*/
/* static void
test_load_int64 (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_int64_col_info_to_list
static void
add_int64_col_info_to_list (const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,// 2
*/
/* static void
test_add_int64_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_gvalue_int64_to_slist
static void
add_gvalue_int64_to_slist (const GncSqlBackend* be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_gvalue_int64_to_slist (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_double
static void
load_double (const GncSqlBackend* be, GncSqlRow* row,// 2
*/
/* static void
test_load_double (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_double_col_info_to_list
static void
add_double_col_info_to_list (const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,// 2
*/
/* static void
test_add_double_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_gvalue_double_to_slist
static void
add_gvalue_double_to_slist (const GncSqlBackend* be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_gvalue_double_to_slist (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_guid
static void
load_guid (const GncSqlBackend* be, GncSqlRow* row,// 3
*/
/* static void
test_load_guid (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_guid_col_info_to_list
static void
add_guid_col_info_to_list (const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,// 3
*/
/* static void
test_add_guid_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_gvalue_guid_to_slist
static void
add_gvalue_guid_to_slist (const GncSqlBackend* be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_gvalue_guid_to_slist (Fixture *fixture, gconstpointer pData)
{
}*/
// Not Used
/* gnc_sql_add_gvalue_objectref_guid_to_slist
void
gnc_sql_add_gvalue_objectref_guid_to_slist (const GncSqlBackend* be, QofIdTypeConst obj_name,// 1
*/
/* static void
test_gnc_sql_add_gvalue_objectref_guid_to_slist (Fixture *fixture, gconstpointer pData)
{
}*/
// Not Used
/* gnc_sql_add_objectref_guid_col_info_to_list
void
gnc_sql_add_objectref_guid_col_info_to_list (const GncSqlBackend* be,// 1
*/
/* static void
test_gnc_sql_add_objectref_guid_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_convert_timespec_to_string
gchar*
gnc_sql_convert_timespec_to_string (const GncSqlBackend* be, Timespec ts)// C: 1 */

#define numtests 6
static void
test_gnc_sql_convert_timespec_to_string ()
{
    GncSqlBackend be = {{
            nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr, ERR_BACKEND_NO_ERR, nullptr, 0, nullptr, nullptr
        },
        nullptr, nullptr, FALSE, FALSE, FALSE, 0, 0, nullptr,
        "%4d-%02d-%02d %02d:%02d:%02d"
    };
    const char* date[numtests] = {"1995-03-11 19:17:26",
                                  "2001-04-20 11:44:07",
                                  "1964-02-29 09:15:23",
                                  "1959-04-02 00:00:00",
                                  "2043-11-22 05:32:45",
                                  "2153-12-18 01:15:30"
                                 };
    int i;
    for (i = 0; i < numtests; i++)
    {

        Timespec ts = gnc_iso8601_to_timespec_gmt (date[i]);
        gchar* datestr = gnc_sql_convert_timespec_to_string (&be, ts);
        g_assert_cmpstr (date[i], == , datestr);

        g_free (datestr);
    }

}
/* load_timespec
static void
load_timespec (const GncSqlBackend* be, GncSqlRow* row,// 2
*/
/* static void
test_load_timespec (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_timespec_col_info_to_list
static void
add_timespec_col_info_to_list (const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,// 2
*/
/* static void
test_add_timespec_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_gvalue_timespec_to_slist
static void
add_gvalue_timespec_to_slist (const GncSqlBackend* be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_gvalue_timespec_to_slist (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_date
static void
load_date (const GncSqlBackend* be, GncSqlRow* row,// 2
*/
/* static void
test_load_date (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_date_col_info_to_list
static void
add_date_col_info_to_list (const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,// 2
*/
/* static void
test_add_date_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_gvalue_date_to_slist
static void
add_gvalue_date_to_slist (const GncSqlBackend* be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_gvalue_date_to_slist (Fixture *fixture, gconstpointer pData)
{
}*/
/* load_numeric
static void
load_numeric (const GncSqlBackend* be, GncSqlRow* row,// 2
*/
/* static void
test_load_numeric (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_numeric_col_info_to_list
static void
add_numeric_col_info_to_list (const GncSqlBackend* be, const GncSqlColumnTableEntry* table_row,// 2
*/
/* static void
test_add_numeric_col_info_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_numeric_colname_to_list
static void
add_numeric_colname_to_list (const GncSqlColumnTableEntry* table_row, GList** pList)// 2
*/
/* static void
test_add_numeric_colname_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_gvalue_numeric_to_slist
static void
add_gvalue_numeric_to_slist (const GncSqlBackend* be, QofIdTypeConst obj_name,// 2
*/
/* static void
test_add_gvalue_numeric_to_slist (Fixture *fixture, gconstpointer pData)
{
}*/
/* get_handler
get_handler (const GncSqlColumnTableEntry* table_row)// C: 1 */
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
gnc_sql_load_guid (const GncSqlBackend* be, GncSqlRow* row)// C: 15 in 14 */
/* static void
test_gnc_sql_load_guid (Fixture *fixture, gconstpointer pData)
{
}*/
// Not Used
/* gnc_sql_load_tx_guid
const GncGUID*
gnc_sql_load_tx_guid (const GncSqlBackend* be, GncSqlRow* row)// 1
*/
/* static void
test_gnc_sql_load_tx_guid (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_load_object
void
gnc_sql_load_object (const GncSqlBackend* be, GncSqlRow* row,// C: 29 in 19 */
/* static void
test_gnc_sql_load_object (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_create_select_statement
gnc_sql_create_select_statement (GncSqlBackend* be, const gchar* table_name)// C: 16 in 16 */
/* static void
test_gnc_sql_create_select_statement (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* create_single_col_select_statement
create_single_col_select_statement (GncSqlBackend* be,// 2
*/
/* static void
test_create_single_col_select_statement (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_execute_select_statement
gnc_sql_execute_select_statement (GncSqlBackend* be, GncSqlStatement* stmt)// C: 25 in 19 */
/* static void
test_gnc_sql_execute_select_statement (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_create_statement_from_sql
gnc_sql_create_statement_from_sql (GncSqlBackend* be, const gchar* sql)// C: 11 in 3 */
/* static void
test_gnc_sql_create_statement_from_sql (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_execute_select_sql
gnc_sql_execute_select_sql (GncSqlBackend* be, const gchar* sql)// C: 1 */
/* static void
test_gnc_sql_execute_select_sql (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_execute_nonselect_sql
gint
gnc_sql_execute_nonselect_sql (GncSqlBackend* be, const gchar* sql)// C: 1 */
/* static void
test_gnc_sql_execute_nonselect_sql (Fixture *fixture, gconstpointer pData)
{
}*/
/* execute_statement_get_count
static guint
execute_statement_get_count (GncSqlBackend* be, GncSqlStatement* stmt)// 2
*/
/* static void
test_execute_statement_get_count (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_append_guid_list_to_sql
guint
gnc_sql_append_guid_list_to_sql (GString* sql, GList* list, guint maxCount)// C: 2 in 2 */
/* static void
test_gnc_sql_append_guid_list_to_sql (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_object_is_it_in_db
gboolean
gnc_sql_object_is_it_in_db (GncSqlBackend* be, const gchar* table_name,// C: 1 */
/* static void
test_gnc_sql_object_is_it_in_db (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_do_db_operation
gboolean
gnc_sql_do_db_operation (GncSqlBackend* be,// C: 22 in 12 */
/* static void
test_gnc_sql_do_db_operation (Fixture *fixture, gconstpointer pData)
{
}*/
/* create_gslist_from_values
static GSList*
create_gslist_from_values (GncSqlBackend* be,// 3
*/
/* static void
test_create_gslist_from_values (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_get_sql_value
gchar*
gnc_sql_get_sql_value (const GncSqlConnection* conn, const GValue* value)// C: 1 */
/* static void
test_gnc_sql_get_sql_value (Fixture *fixture, gconstpointer pData)
{
}*/
/* free_gvalue_list
static void
free_gvalue_list (GSList* list)// 4
*/
/* static void
test_free_gvalue_list (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* build_insert_statement
build_insert_statement (GncSqlBackend* be,// 3
*/
/* static void
test_build_insert_statement (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* build_update_statement
build_update_statement (GncSqlBackend* be,// 3
*/
/* static void
test_build_update_statement (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* build_delete_statement
build_delete_statement (GncSqlBackend* be,// 3
*/
/* static void
test_build_delete_statement (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_commit_standard_item
gboolean
gnc_sql_commit_standard_item (GncSqlBackend* be, QofInstance* inst, const gchar* tableName,// C: 7 in 7 */
/* static void
test_gnc_sql_commit_standard_item (Fixture *fixture, gconstpointer pData)
{
}*/
/* do_create_table
static gboolean
do_create_table (const GncSqlBackend* be, const gchar* table_name,// 5
*/
/* static void
test_do_create_table (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_create_table
gboolean
gnc_sql_create_table (GncSqlBackend* be, const gchar* table_name,// C: 22 in 19 */
/* static void
test_gnc_sql_create_table (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* gnc_sql_create_temp_table
gboolean
gnc_sql_create_temp_table (const GncSqlBackend* be, const gchar* table_name,// 2
*/
/* static void
test_gnc_sql_create_temp_table (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_create_index
gboolean
gnc_sql_create_index (const GncSqlBackend* be, const gchar* index_name,// C: 7 in 2 */
/* static void
test_gnc_sql_create_index (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_get_table_version
gint
gnc_sql_get_table_version (const GncSqlBackend* be, const gchar* table_name)// C: 24 in 20 */
/* static void
test_gnc_sql_get_table_version (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_upgrade_table
void
gnc_sql_upgrade_table (GncSqlBackend* be, const gchar* table_name,// C: 12 in 10 */
/* static void
test_gnc_sql_upgrade_table (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_add_columns_to_table
gboolean gnc_sql_add_columns_to_table (GncSqlBackend* be, const gchar* table_name,// C: 1 */
/* static void
test_gnc_sql_add_columns_to_table (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_init_version_info
void
gnc_sql_init_version_info (GncSqlBackend* be)// C: 1 */
/* static void
test_gnc_sql_init_version_info (Fixture *fixture, gconstpointer pData)
{
}*/
/* reset_version_info
static gboolean
reset_version_info (GncSqlBackend* be)// 3
*/
/* static void
test_reset_version_info (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_finalize_version_info
void
gnc_sql_finalize_version_info (GncSqlBackend* be)// C: 1 */
/* static void
test_gnc_sql_finalize_version_info (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_sql_set_table_version
gboolean
gnc_sql_set_table_version (GncSqlBackend* be, const gchar* table_name, gint version)// C: 12 in 10 */
/* static void
test_gnc_sql_set_table_version (Fixture *fixture, gconstpointer pData)
{
}*/


void
test_suite_gnc_backend_sql (void)
{

// GNC_TEST_ADD (suitename, "gnc sql init", Fixture, nullptr, test_gnc_sql_init,  teardown);
// GNC_TEST_ADD (suitename, "create tables cb", Fixture, nullptr, test_create_tables_cb,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql set load order", Fixture, nullptr, test_gnc_sql_set_load_order,  teardown);
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
// GNC_TEST_ADD (suitename, "gnc sql get integer value", Fixture, nullptr, test_gnc_sql_get_integer_value,  teardown);
// GNC_TEST_ADD (suitename, "get autoinc id", Fixture, nullptr, test_get_autoinc_id,  teardown);
// GNC_TEST_ADD (suitename, "set autoinc id", Fixture, nullptr, test_set_autoinc_id,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql get getter", Fixture, nullptr, test_gnc_sql_get_getter,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql add colname to list", Fixture, nullptr, test_gnc_sql_add_colname_to_list,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql add subtable colnames to list", Fixture, nullptr, test_gnc_sql_add_subtable_colnames_to_list,  teardown);
// GNC_TEST_ADD (suitename, "create column info", Fixture, nullptr, test_create_column_info,  teardown);
// GNC_TEST_ADD (suitename, "load string", Fixture, nullptr, test_load_string,  teardown);
// GNC_TEST_ADD (suitename, "add string col info to list", Fixture, nullptr, test_add_string_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add gvalue string to slist", Fixture, nullptr, test_add_gvalue_string_to_slist,  teardown);
// GNC_TEST_ADD (suitename, "load int", Fixture, nullptr, test_load_int,  teardown);
// GNC_TEST_ADD (suitename, "add int col info to list", Fixture, nullptr, test_add_int_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add gvalue int to slist", Fixture, nullptr, test_add_gvalue_int_to_slist,  teardown);
// GNC_TEST_ADD (suitename, "load boolean", Fixture, nullptr, test_load_boolean,  teardown);
// GNC_TEST_ADD (suitename, "add boolean col info to list", Fixture, nullptr, test_add_boolean_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add gvalue boolean to slist", Fixture, nullptr, test_add_gvalue_boolean_to_slist,  teardown);
// GNC_TEST_ADD (suitename, "load int64", Fixture, nullptr, test_load_int64,  teardown);
// GNC_TEST_ADD (suitename, "add int64 col info to list", Fixture, nullptr, test_add_int64_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add gvalue int64 to slist", Fixture, nullptr, test_add_gvalue_int64_to_slist,  teardown);
// GNC_TEST_ADD (suitename, "load double", Fixture, nullptr, test_load_double,  teardown);
// GNC_TEST_ADD (suitename, "add double col info to list", Fixture, nullptr, test_add_double_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add gvalue double to slist", Fixture, nullptr, test_add_gvalue_double_to_slist,  teardown);
// GNC_TEST_ADD (suitename, "load guid", Fixture, nullptr, test_load_guid,  teardown);
// GNC_TEST_ADD (suitename, "add guid col info to list", Fixture, nullptr, test_add_guid_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add gvalue guid to slist", Fixture, nullptr, test_add_gvalue_guid_to_slist,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql add gvalue objectref guid to slist", Fixture, nullptr, test_gnc_sql_add_gvalue_objectref_guid_to_slist,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql add objectref guid col info to list", Fixture, nullptr, test_gnc_sql_add_objectref_guid_col_info_to_list,  teardown);
    GNC_TEST_ADD_FUNC (suitename, "gnc sql convert timespec to string",
                       test_gnc_sql_convert_timespec_to_string);
// GNC_TEST_ADD (suitename, "load timespec", Fixture, nullptr, test_load_timespec,  teardown);
// GNC_TEST_ADD (suitename, "add timespec col info to list", Fixture, nullptr, test_add_timespec_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add gvalue timespec to slist", Fixture, nullptr, test_add_gvalue_timespec_to_slist,  teardown);
// GNC_TEST_ADD (suitename, "load date", Fixture, nullptr, test_load_date,  teardown);
// GNC_TEST_ADD (suitename, "add date col info to list", Fixture, nullptr, test_add_date_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add gvalue date to slist", Fixture, nullptr, test_add_gvalue_date_to_slist,  teardown);
// GNC_TEST_ADD (suitename, "load numeric", Fixture, nullptr, test_load_numeric,  teardown);
// GNC_TEST_ADD (suitename, "add numeric col info to list", Fixture, nullptr, test_add_numeric_col_info_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add numeric colname to list", Fixture, nullptr, test_add_numeric_colname_to_list,  teardown);
// GNC_TEST_ADD (suitename, "add gvalue numeric to slist", Fixture, nullptr, test_add_gvalue_numeric_to_slist,  teardown);
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
// GNC_TEST_ADD (suitename, "create gslist from values", Fixture, nullptr, test_create_gslist_from_values,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql get sql value", Fixture, nullptr, test_gnc_sql_get_sql_value,  teardown);
// GNC_TEST_ADD (suitename, "free gvalue list", Fixture, nullptr, test_free_gvalue_list,  teardown);
// GNC_TEST_ADD (suitename, "build insert statement", Fixture, nullptr, test_build_insert_statement,  teardown);
// GNC_TEST_ADD (suitename, "build update statement", Fixture, nullptr, test_build_update_statement,  teardown);
// GNC_TEST_ADD (suitename, "build delete statement", Fixture, nullptr, test_build_delete_statement,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql commit standard item", Fixture, nullptr, test_gnc_sql_commit_standard_item,  teardown);
// GNC_TEST_ADD (suitename, "do create table", Fixture, nullptr, test_do_create_table,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql create table", Fixture, nullptr, test_gnc_sql_create_table,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql create temp table", Fixture, nullptr, test_gnc_sql_create_temp_table,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql create index", Fixture, nullptr, test_gnc_sql_create_index,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql get table version", Fixture, nullptr, test_gnc_sql_get_table_version,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql upgrade table", Fixture, nullptr, test_gnc_sql_upgrade_table,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql add columns to table", Fixture, nullptr, test_gnc_sql_add_columns_to_table,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql init version info", Fixture, nullptr, test_gnc_sql_init_version_info,  teardown);
// GNC_TEST_ADD (suitename, "reset version info", Fixture, nullptr, test_reset_version_info,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql finalize version info", Fixture, nullptr, test_gnc_sql_finalize_version_info,  teardown);
// GNC_TEST_ADD (suitename, "gnc sql set table version", Fixture, nullptr, test_gnc_sql_set_table_version,  teardown);

}
