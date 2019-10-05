/********************************************************************
 * gnc-sql-backend.cpp: Implementation of GncSqlBackend             *
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
extern "C"
{
#include <config.h>
#include <gnc-prefs.h>
#include <gnc-engine.h>
#include <gnc-commodity.h>
#include <SX-book.h>
#include <Recurrence.h>
#include <gncBillTerm.h>
#include <gncTaxTable.h>
#include <gncInvoice.h>
#include <gnc-pricedb.h>
}

#include <algorithm>
#include <cassert>

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-sql-result.hpp"

#include "gnc-account-sql.h"
#include "gnc-book-sql.h"
#include "gnc-budget-sql.h"
#include "gnc-commodity-sql.h"
#include "gnc-lots-sql.h"
#include "gnc-price-sql.h"
#include "gnc-recurrence-sql.h"
#include "gnc-schedxaction-sql.h"
#include "gnc-slots-sql.h"
#include "gnc-transaction-sql.h"

#include "gnc-bill-term-sql.h"
#include "gnc-customer-sql.h"
#include "gnc-employee-sql.h"
#include "gnc-entry-sql.h"
#include "gnc-invoice-sql.h"
#include "gnc-job-sql.h"
#include "gnc-order-sql.h"
#include "gnc-tax-table-sql.h"
#include "gnc-vendor-sql.h"

static QofLogModule log_module = G_LOG_DOMAIN;
#define VERSION_TABLE_NAME "versions"
#define MAX_TABLE_NAME_LEN 50
#define TABLE_COL_NAME "table_name"
#define VERSION_COL_NAME "table_version"

using StrVec = std::vector<std::string>;

static EntryVec version_table
{
    gnc_sql_make_table_entry<CT_STRING>(
        TABLE_COL_NAME, MAX_TABLE_NAME_LEN, COL_PKEY | COL_NNUL),
    gnc_sql_make_table_entry<CT_INT>(VERSION_COL_NAME, 0, COL_NNUL)
};

GncSqlBackend::GncSqlBackend(GncSqlConnection *conn, QofBook* book) :
    QofBackend {}, m_conn{conn}, m_book{book}, m_loading{false},
    m_in_query{false}, m_is_pristine_db{false}
{
    if (conn != nullptr)
        connect (conn);
}

void
GncSqlBackend::connect(GncSqlConnection *conn) noexcept
{
    if (m_conn != nullptr && m_conn != conn)
        delete m_conn;
    finalize_version_info();
    m_conn = conn;
}

GncSqlStatementPtr
GncSqlBackend::create_statement_from_sql(const std::string& str) const noexcept
{
    auto stmt = m_conn->create_statement_from_sql(str);
    if (stmt == nullptr)
    {
        PERR ("SQL error: %s\n", str.c_str());
        qof_backend_set_error ((QofBackend*)this, ERR_BACKEND_SERVER_ERR);
    }
    return stmt;
}

GncSqlResultPtr
GncSqlBackend::execute_select_statement(const GncSqlStatementPtr& stmt) const noexcept
{
    auto result = m_conn->execute_select_statement(stmt);
    if (result == nullptr)
    {
        PERR ("SQL error: %s\n", stmt->to_sql());
        qof_backend_set_error ((QofBackend*)this, ERR_BACKEND_SERVER_ERR);
    }
    return result;
}

int
GncSqlBackend::execute_nonselect_statement(const GncSqlStatementPtr& stmt) const noexcept
{
    auto result = m_conn->execute_nonselect_statement(stmt);
    if (result == -1)
    {
        PERR ("SQL error: %s\n", stmt->to_sql());
        qof_backend_set_error ((QofBackend*)this, ERR_BACKEND_SERVER_ERR);
    }
    return result;
}

std::string
GncSqlBackend::quote_string(const std::string& str) const noexcept
{
    return m_conn->quote_string(str);
}

bool
GncSqlBackend::create_table(const std::string& table_name,
                            const EntryVec& col_table) const noexcept
{
    ColVec info_vec;
    gboolean ok = FALSE;

    for (auto const& table_row : col_table)
    {
        table_row->add_to_table (info_vec);
    }
    return m_conn->create_table (table_name, info_vec);

}

bool
GncSqlBackend::create_table(const std::string& table_name, int table_version,
                            const EntryVec& col_table) noexcept
{
    if (create_table (table_name, col_table))
        return set_table_version (table_name, table_version);
    return false;
}

bool
GncSqlBackend::create_index(const std::string& index_name,
                            const std::string& table_name,
                            const EntryVec& col_table) const noexcept
{
    return m_conn->create_index(index_name, table_name, col_table);
}

bool
GncSqlBackend::add_columns_to_table(const std::string& table_name,
                                    const EntryVec& col_table) const noexcept
{
    ColVec info_vec;

    for (auto const& table_row : col_table)
    {
        table_row->add_to_table (info_vec);
    }
    return m_conn->add_columns_to_table(table_name, info_vec);
}

void
GncSqlBackend::update_progress(double pct) const noexcept
{
    if (m_percentage != nullptr)
        (m_percentage) (nullptr, pct);
}

void
GncSqlBackend::finish_progress() const noexcept
{
    if (m_percentage != nullptr)
        (m_percentage) (nullptr, -1.0);
}

void
GncSqlBackend::create_tables() noexcept
{
    for(auto entry : m_backend_registry)
    {
        update_progress(101.0);
        std::get<1>(entry)->create_tables(this);
    }
}

/* Main object load order */
static const StrVec fixed_load_order
{ GNC_ID_BOOK, GNC_ID_COMMODITY, GNC_ID_ACCOUNT, GNC_ID_LOT, GNC_ID_TRANS };

/* Order in which business objects need to be loaded */
static const StrVec business_fixed_load_order =
{ GNC_ID_BILLTERM, GNC_ID_TAXTABLE, GNC_ID_INVOICE };

void
GncSqlBackend::ObjectBackendRegistry::load_remaining(GncSqlBackend* sql_be)
{

    auto num_types = m_registry.size();
    auto num_done = fixed_load_order.size() + business_fixed_load_order.size();

    for (auto entry : m_registry)
    {
        std::string type;
        GncSqlObjectBackendPtr obe = nullptr;
        std::tie(type, obe) = entry;

        /* Don't need to load anything if it has already been loaded with
         * the fixed order.
         */
        if (std::find(fixed_load_order.begin(), fixed_load_order.end(),
                      type) != fixed_load_order.end()) continue;
        if (std::find(business_fixed_load_order.begin(),
                      business_fixed_load_order.end(),
                      type) != business_fixed_load_order.end()) continue;

        num_done++;
        sql_be->update_progress(num_done * 100 / num_types);
        obe->load_all (sql_be);
    }
}

typedef struct
{
    QofIdType searchObj;
    gpointer pCompiledQuery;
} gnc_sql_query_info;

/* callback structure */
typedef struct
{
    gboolean is_known;
    gboolean is_ok;
    GncSqlBackend* sql_be;
    QofInstance* inst;
    QofQuery* pQuery;
    gpointer pCompiledQuery;
    gnc_sql_query_info* pQueryInfo;
} sql_backend;


void
GncSqlBackend::load (QofBook* book, QofBackendLoadType loadType)
{
    Account* root;

    g_return_if_fail (book != NULL);

    ENTER ("sql_be=%p, book=%p", this, book);

    m_loading = TRUE;

    if (loadType == LOAD_TYPE_INITIAL_LOAD)
    {
        assert (m_book == nullptr);
        m_book = book;

        auto num_types = m_backend_registry.size();
        auto num_done = 0;

        /* Load any initial stuff. Some of this needs to happen in a certain order */
        for (auto type : fixed_load_order)
        {
            num_done++;
            auto obe = m_backend_registry.get_object_backend(type);
            if (obe)
            {
                update_progress(num_done * 100 / num_types);
                obe->load_all(this);
            }
        }
        for (auto type : business_fixed_load_order)
        {
            num_done++;
            auto obe = m_backend_registry.get_object_backend(type);
            if (obe)
            {
                update_progress(num_done * 100 / num_types);
                obe->load_all(this);
            }
        }

        root = gnc_book_get_root_account( book );
        gnc_account_foreach_descendant(root, (AccountCb)xaccAccountBeginEdit,
                                       nullptr);

        m_backend_registry.load_remaining(this);

        gnc_account_foreach_descendant(root, (AccountCb)xaccAccountCommitEdit,
                                       nullptr);
    }
    else if (loadType == LOAD_TYPE_LOAD_ALL)
    {
        // Load all transactions
        auto obe = m_backend_registry.get_object_backend (GNC_ID_TRANS);
        obe->load_all (this);
    }

    m_loading = FALSE;
    std::for_each(m_postload_commodities.begin(), m_postload_commodities.end(),
                 [](gnc_commodity* comm) {
                      gnc_commodity_begin_edit(comm);
                      gnc_commodity_commit_edit(comm);
                  });
    m_postload_commodities.clear();

    /* Mark the sessoion as clean -- though it should never be marked
     * dirty with this backend
     */
    qof_book_mark_session_saved (book);
    finish_progress();

    LEAVE ("");
}

/* ================================================================= */

bool
GncSqlBackend::write_account_tree(Account* root)
{
    GList* descendants;
    GList* node;
    bool is_ok = true;

    g_return_val_if_fail (root != nullptr, false);

    auto obe = m_backend_registry.get_object_backend(GNC_ID_ACCOUNT);
    is_ok = obe->commit (this, QOF_INSTANCE (root));
    if (is_ok)
    {
        descendants = gnc_account_get_descendants (root);
        for (node = descendants; node != NULL && is_ok; node = g_list_next (node))
        {
            is_ok = obe->commit(this, QOF_INSTANCE (GNC_ACCOUNT (node->data)));
            if (!is_ok) break;
        }
        g_list_free (descendants);
    }
    update_progress(101.0);

    return is_ok;
}

bool
GncSqlBackend::write_accounts()
{
    update_progress(101.0);
    auto is_ok = write_account_tree (gnc_book_get_root_account (m_book));
    if (is_ok)
    {
        update_progress(101.0);
        is_ok = write_account_tree (gnc_book_get_template_root(m_book));
    }

    return is_ok;
}

static gboolean // Can't be bool because of signature for xaccAccountTreeForEach
write_tx (Transaction* tx, gpointer data)
{
    auto s = static_cast<write_objects_t*>(data);

    g_return_val_if_fail (tx != NULL, 0);
    g_return_val_if_fail (data != NULL, 0);

    s->commit (QOF_INSTANCE (tx));
    auto splitbe = s->be->get_object_backend(GNC_ID_SPLIT);
    for (auto split_node = xaccTransGetSplitList (tx);
         split_node != nullptr && s->is_ok;
         split_node = g_list_next (split_node))
    {
        s->is_ok = splitbe->commit(s->be, QOF_INSTANCE(split_node->data));
    }
    s->be->update_progress (101.0);
    return (s->is_ok ? 0 : 1);
}

bool
GncSqlBackend::write_transactions()
{
    auto obe = m_backend_registry.get_object_backend(GNC_ID_TRANS);
    write_objects_t data{this, TRUE, obe.get()};

    (void)xaccAccountTreeForEachTransaction (
        gnc_book_get_root_account (m_book), write_tx, &data);
    update_progress(101.0);
    return data.is_ok;
}

bool
GncSqlBackend::write_template_transactions()
{
    auto obe = m_backend_registry.get_object_backend(GNC_ID_TRANS);
    write_objects_t data{this, true, obe.get()};
    auto ra = gnc_book_get_template_root (m_book);
    if (gnc_account_n_descendants (ra) > 0)
    {
        (void)xaccAccountTreeForEachTransaction (ra, write_tx, &data);
        update_progress(101.0);
    }

    return data.is_ok;
}

bool
GncSqlBackend::write_schedXactions()
{
    GList* schedXactions;
    SchedXaction* tmpSX;
    bool is_ok = true;

    schedXactions = gnc_book_get_schedxactions (m_book)->sx_list;
    auto obe = m_backend_registry.get_object_backend(GNC_ID_SCHEDXACTION);

    for (; schedXactions != NULL && is_ok; schedXactions = schedXactions->next)
    {
        tmpSX = static_cast<decltype (tmpSX)> (schedXactions->data);
        is_ok = obe->commit (this, QOF_INSTANCE (tmpSX));
    }
    update_progress(101.0);

    return is_ok;
}

#pragma GCC diagnostic warning "-Wformat-nonliteral"

void
GncSqlBackend::sync(QofBook* book)
{
    g_return_if_fail (book != NULL);

    reset_version_info();
    ENTER ("book=%p, sql_be->book=%p", book, m_book);
    update_progress(101.0);

    /* Create new tables */
    m_is_pristine_db = true;
    create_tables();

    /* Save all contents */
    m_book = book;
    auto is_ok = m_conn->begin_transaction();

    // FIXME: should write the set of commodities that are used
    // write_commodities(sql_be, book);
    if (is_ok)
    {
        auto obe = m_backend_registry.get_object_backend(GNC_ID_BOOK);
        is_ok = obe->commit (this, QOF_INSTANCE (book));
    }
    if (is_ok)
    {
        is_ok = write_accounts();
    }
    if (is_ok)
    {
        is_ok = write_transactions();
    }
    if (is_ok)
    {
        is_ok = write_template_transactions();
    }
    if (is_ok)
    {
        is_ok = write_schedXactions();
    }
    if (is_ok)
    {
        for (auto entry : m_backend_registry)
            std::get<1>(entry)->write (this);
    }
    if (is_ok)
    {
        is_ok = m_conn->commit_transaction();
    }
    if (is_ok)
    {
        m_is_pristine_db = false;

        /* Mark the session as clean -- though it shouldn't ever get
         * marked dirty with this backend
         */
        qof_book_mark_session_saved(book);
    }
    else
    {
        set_error (ERR_BACKEND_SERVER_ERR);
        m_conn->rollback_transaction ();
    }
    finish_progress();
    LEAVE ("book=%p", book);
}

/* ================================================================= */
/* Routines to deal with the creation of multiple books. */

void
GncSqlBackend::begin(QofInstance* inst)
{
    //g_return_if_fail (inst != NULL);

    //ENTER (" ");
    //LEAVE ("");
}

void
GncSqlBackend::rollback(QofInstance* inst)
{
    //g_return_if_fail (inst != NULL);

    //ENTER (" ");
    //LEAVE ("");
}

void
GncSqlBackend::commodity_for_postload_processing(gnc_commodity* commodity)
{
    m_postload_commodities.push_back(commodity);
}

GncSqlObjectBackendPtr
GncSqlBackend::get_object_backend(const std::string& type) const noexcept
{
    return m_backend_registry.get_object_backend(type);
}


/* Commit_edit handler - find the correct backend handler for this object
 * type and call its commit handler
 */
void
GncSqlBackend::commit (QofInstance* inst)
{
    sql_backend be_data;
    gboolean is_dirty;
    gboolean is_destroying;
    gboolean is_infant;

    g_return_if_fail (inst != NULL);

    if (qof_book_is_readonly(m_book))
    {
        set_error (ERR_BACKEND_READONLY);
        (void)m_conn->rollback_transaction ();
        return;
    }
    /* During initial load where objects are being created, don't commit
    anything, but do mark the object as clean. */
    if (m_loading)
    {
        qof_instance_mark_clean (inst);
        return;
    }

    // The engine has a PriceDB object but it isn't in the database
    if (strcmp (inst->e_type, "PriceDB") == 0)
    {
        qof_instance_mark_clean (inst);
        qof_book_mark_session_saved (m_book);
        return;
    }

    ENTER (" ");

    is_dirty = qof_instance_get_dirty_flag (inst);
    is_destroying = qof_instance_get_destroying (inst);
    is_infant = qof_instance_get_infant (inst);

    DEBUG ("%s dirty = %d, do_free = %d, infant = %d\n",
           (inst->e_type ? inst->e_type : "(null)"),
           is_dirty, is_destroying, is_infant);

    if (!is_dirty && !is_destroying)
    {
        LEAVE ("!dirty OR !destroying");
        return;
    }

    if (!m_conn->begin_transaction ())
    {
        PERR ("begin_transaction failed\n");
        LEAVE ("Rolled back - database transaction begin error");
        return;
    }

    bool is_ok = true;

    auto obe = m_backend_registry.get_object_backend(std::string{inst->e_type});
    if (obe != nullptr)
        is_ok = obe->commit(this, inst);
    else
    {
        PERR ("Unknown object type '%s'\n", inst->e_type);
        (void)m_conn->rollback_transaction ();

        // Don't let unknown items still mark the book as being dirty
        qof_book_mark_session_saved(m_book);
        qof_instance_mark_clean (inst);
        LEAVE ("Rolled back - unknown object type");
        return;
    }
    if (!is_ok)
    {
        // Error - roll it back
        (void)m_conn->rollback_transaction();

        // This *should* leave things marked dirty
        LEAVE ("Rolled back - database error");
        return;
    }

    (void)m_conn->commit_transaction ();

    qof_book_mark_session_saved(m_book);
    qof_instance_mark_clean (inst);

    LEAVE ("");
}


/**
 * Sees if the version table exists, and if it does, loads the info into
 * the version hash table.  Otherwise, it creates an empty version table.
 *
 * @param be Backend struct
 */
void
GncSqlBackend::init_version_info() noexcept
{

    if (m_conn->does_table_exist (VERSION_TABLE_NAME))
    {
        std::string sql {"SELECT * FROM "};
        sql += VERSION_TABLE_NAME;
        auto stmt = m_conn->create_statement_from_sql(sql);
        auto result = m_conn->execute_select_statement (stmt);
        for (const auto& row : *result)
        {
            auto name = row.get_string_at_col (TABLE_COL_NAME);
            unsigned int version = row.get_int_at_col (VERSION_COL_NAME);
            m_versions.push_back(std::make_pair(name, version));
        }
    }
    else
    {
        create_table (VERSION_TABLE_NAME, version_table);
        set_table_version("Gnucash", gnc_prefs_get_long_version ());
        set_table_version("Gnucash-Resave", GNUCASH_RESAVE_VERSION);
    }
}

/**
 * Resets the version table information by removing all version table info.
 * It also recreates the version table in the db.
 *
 * @param be Backend struct
 * @return TRUE if successful, FALSE if error
 */
bool
GncSqlBackend::reset_version_info() noexcept
{
    bool ok = create_table (VERSION_TABLE_NAME, version_table);
    m_versions.clear();
    set_table_version ("Gnucash", gnc_prefs_get_long_version ());
    set_table_version ("Gnucash-Resave", GNUCASH_RESAVE_VERSION);
    return ok;
}

/**
 * Finalizes the version table info by destroying the hash table.
 *
 * @param be Backend struct
 */
void
GncSqlBackend::finalize_version_info() noexcept
{
    m_versions.clear();
}

unsigned int
GncSqlBackend::get_table_version(const std::string& table_name) const noexcept
{
    /* If the db is pristine because it's being saved, the table does not exist. */
    if (m_is_pristine_db)
        return 0;

    auto version = std::find_if(m_versions.begin(), m_versions.end(),
                                [table_name](const VersionPair& version) {
                                    return version.first == table_name; });
    if (version != m_versions.end())
        return version->second;
    return 0;
}

/**
 * Registers the version for a table.  Registering involves updating the
 * db version table and also the hash table.
 *
 * @param be Backend struct
 * @param table_name Table name
 * @param version Version number
 * @return TRUE if successful, FALSE if unsuccessful
 */
bool
GncSqlBackend::set_table_version (const std::string& table_name,
                                  uint_t version) noexcept
{
    g_return_val_if_fail (version > 0, false);

    unsigned int cur_version{0};
    std::stringstream sql;
    auto ver_entry = std::find_if(m_versions.begin(), m_versions.end(),
                                [table_name](const VersionPair& ver) {
                                    return ver.first == table_name; });
    if (ver_entry != m_versions.end())
        cur_version = ver_entry->second;
    if (cur_version != version)
    {
        if (cur_version == 0)
        {
            sql << "INSERT INTO " << VERSION_TABLE_NAME << " VALUES('" <<
                table_name << "'," << version <<")";
            m_versions.push_back(std::make_pair(table_name, version));
        }
        else
        {
            sql << "UPDATE " <<  VERSION_TABLE_NAME << " SET " <<
                VERSION_COL_NAME << "=" << version << " WHERE " <<
                TABLE_COL_NAME << "='" << table_name << "'";
            ver_entry->second = version;
        }
        auto stmt = create_statement_from_sql(sql.str());
        auto status = execute_nonselect_statement (stmt);
        if (status == -1)
        {
            PERR ("SQL error: %s\n", sql.str().c_str());
            qof_backend_set_error ((QofBackend*)this, ERR_BACKEND_SERVER_ERR);
            return false;
        }
    }

    return true;
}

void
GncSqlBackend::upgrade_table (const std::string& table_name,
                              const EntryVec& col_table) noexcept
{
    DEBUG ("Upgrading %s table\n", table_name.c_str());

    auto temp_table_name = table_name + "_new";
    create_table (temp_table_name, col_table);
    std::stringstream sql;
    sql << "INSERT INTO " << temp_table_name << " SELECT * FROM " << table_name;
    auto stmt = create_statement_from_sql(sql.str());
    execute_nonselect_statement(stmt);

    sql.str("");
    sql << "DROP TABLE " << table_name;
    stmt = create_statement_from_sql(sql.str());
    execute_nonselect_statement(stmt);

    sql.str("");
    sql << "ALTER TABLE " << temp_table_name << " RENAME TO " << table_name;
    stmt = create_statement_from_sql(sql.str());
    execute_nonselect_statement(stmt);
}

static inline PairVec
get_object_values (QofIdTypeConst obj_name,
                   gpointer pObject, const EntryVec& table)
{
    PairVec vec;

    for (auto const& table_row : table)
    {
        if (!(table_row->is_autoincr()))
        {
            table_row->add_to_query (obj_name, pObject, vec);
        }
    }
    return vec;
}

bool
GncSqlBackend::object_in_db (const char* table_name, QofIdTypeConst obj_name,
                             const gpointer pObject, const EntryVec& table) const noexcept
{
    guint count;
    g_return_val_if_fail (table_name != nullptr, false);
    g_return_val_if_fail (obj_name != nullptr, false);
    g_return_val_if_fail (pObject != nullptr, false);

    /* SELECT * FROM */
    auto sql = std::string{"SELECT "} + table[0]->name() + " FROM " + table_name;
    auto stmt = create_statement_from_sql(sql.c_str());
    assert (stmt != nullptr);

    /* WHERE */
    PairVec values{get_object_values(obj_name, pObject, table)};
    /* We want only the first item in the table, which should be the PK. */
    values.resize(1);
    stmt->add_where_cond(obj_name, values);
    auto result = execute_select_statement (stmt);
    return (result != nullptr && result->size() > 0);
}

bool
GncSqlBackend::do_db_operation (E_DB_OPERATION op, const char* table_name,
                                QofIdTypeConst obj_name, gpointer pObject,
                                const EntryVec& table) const noexcept
{
    GncSqlStatementPtr stmt;

    g_return_val_if_fail (table_name != nullptr, false);
    g_return_val_if_fail (obj_name != nullptr, false);
    g_return_val_if_fail (pObject != nullptr, false);

    switch(op)
    {
        case  OP_DB_INSERT:
        stmt = build_insert_statement (table_name, obj_name, pObject, table);
        break;
        case OP_DB_UPDATE:
        stmt = build_update_statement (table_name, obj_name, pObject, table);
        break;
        case OP_DB_DELETE:
        stmt = build_delete_statement (table_name, obj_name, pObject, table);
        break;
    }
    if (stmt == nullptr)
        return false;
    return (execute_nonselect_statement(stmt) != -1);
}

bool
GncSqlBackend::save_commodity(gnc_commodity* comm) noexcept
{
    if (comm == nullptr) return false;
    QofInstance* inst = QOF_INSTANCE(comm);
    auto obe = m_backend_registry.get_object_backend(std::string(inst->e_type));
    if (obe && !obe->instance_in_db(this, inst))
        return obe->commit(this, inst);
    return true;
}

GncSqlStatementPtr
GncSqlBackend::build_insert_statement (const char* table_name,
                                       QofIdTypeConst obj_name,
                                       gpointer pObject,
                                       const EntryVec& table) const noexcept
{
    GncSqlStatementPtr stmt;
    PairVec col_values;
    std::ostringstream sql;

    g_return_val_if_fail (table_name != nullptr, nullptr);
    g_return_val_if_fail (obj_name != nullptr, nullptr);
    g_return_val_if_fail (pObject != nullptr, nullptr);
    PairVec values{get_object_values(obj_name, pObject, table)};

    sql << "INSERT INTO " << table_name <<"(";
    for (auto const& col_value : values)
    {
        if (col_value != *values.begin())
            sql << ",";
        sql << col_value.first;
    }

    sql << ") VALUES(";
    for (auto col_value : values)
    {
        if (col_value != *values.begin())
            sql << ",";
        sql << col_value.second;
    }
    sql << ")";

    stmt = create_statement_from_sql(sql.str());
    return stmt;
}

GncSqlStatementPtr
GncSqlBackend::build_update_statement(const gchar* table_name,
                                      QofIdTypeConst obj_name, gpointer pObject,
                                      const EntryVec& table) const noexcept
{
    GncSqlStatementPtr stmt;
    std::ostringstream sql;

    g_return_val_if_fail (table_name != nullptr, nullptr);
    g_return_val_if_fail (obj_name != nullptr, nullptr);
    g_return_val_if_fail (pObject != nullptr, nullptr);


    PairVec values{get_object_values (obj_name, pObject, table)};

    // Create the SQL statement
    sql <<  "UPDATE " << table_name << " SET ";

    for (auto const& col_value : values)
    {
        if (col_value != *values.begin())
            sql << ",";
        sql << col_value.first << "=" <<
            col_value.second;
    }

    stmt = create_statement_from_sql(sql.str());
    /* We want our where condition to be just the first column and
     * value, i.e. the guid of the object.
     */
    values.erase(values.begin() + 1, values.end());
    stmt->add_where_cond(obj_name, values);
    return stmt;
}

GncSqlStatementPtr
GncSqlBackend::build_delete_statement(const gchar* table_name,
                                      QofIdTypeConst obj_name,
                                      gpointer pObject,
                                      const EntryVec& table) const noexcept
{
    std::ostringstream sql;

    g_return_val_if_fail (table_name != nullptr, nullptr);
    g_return_val_if_fail (obj_name != nullptr, nullptr);
    g_return_val_if_fail (pObject != nullptr, nullptr);

    sql << "DELETE FROM " << table_name;
    auto stmt = create_statement_from_sql (sql.str());

    /* WHERE */
    PairVec values;
    table[0]->add_to_query (obj_name, pObject, values);
    PairVec col_values{values[0]};
    stmt->add_where_cond (obj_name, col_values);

    return stmt;
}

GncSqlBackend::ObjectBackendRegistry::ObjectBackendRegistry()
{
    register_backend(std::make_shared<GncSqlBookBackend>());
    register_backend(std::make_shared<GncSqlCommodityBackend>());
    register_backend(std::make_shared<GncSqlAccountBackend>());
    register_backend(std::make_shared<GncSqlBudgetBackend>());
    register_backend(std::make_shared<GncSqlPriceBackend>());
    register_backend(std::make_shared<GncSqlTransBackend>());
    register_backend(std::make_shared<GncSqlSplitBackend>());
    register_backend(std::make_shared<GncSqlSlotsBackend>());
    register_backend(std::make_shared<GncSqlRecurrenceBackend>());
    register_backend(std::make_shared<GncSqlSchedXactionBackend>());
    register_backend(std::make_shared<GncSqlLotsBackend>());
    register_backend(std::make_shared<GncSqlBillTermBackend>());
    register_backend(std::make_shared<GncSqlCustomerBackend>());
    register_backend(std::make_shared<GncSqlEmployeeBackend>());
    register_backend(std::make_shared<GncSqlEntryBackend>());
    register_backend(std::make_shared<GncSqlInvoiceBackend>());
    register_backend(std::make_shared<GncSqlJobBackend>());
    register_backend(std::make_shared<GncSqlOrderBackend>());
    register_backend(std::make_shared<GncSqlTaxTableBackend>());
    register_backend(std::make_shared<GncSqlVendorBackend>());
}

void
GncSqlBackend::ObjectBackendRegistry::register_backend(OBEEntry&& entry) noexcept
{
    m_registry.emplace_back(entry);
}

void
GncSqlBackend::ObjectBackendRegistry::register_backend(GncSqlObjectBackendPtr obe) noexcept
{
    m_registry.emplace_back(make_tuple(std::string{obe->type()}, obe));
}

GncSqlObjectBackendPtr
GncSqlBackend::ObjectBackendRegistry::get_object_backend(const std::string& type) const
{
    auto entry = std::find_if(m_registry.begin(), m_registry.end(),
                              [type](const OBEEntry& entry){
                                  return type == std::get<0>(entry);
                              });
    if (entry == m_registry.end())
        return nullptr;

    return std::get<1>(*entry);
}
