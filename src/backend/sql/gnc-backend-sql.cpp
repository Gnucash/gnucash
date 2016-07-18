/********************************************************************
 * gnc-backend-sql.c: load and save data to SQL                     *
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
/** @file gnc-backend-sql.c
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */
#include <guid.hpp>
extern "C"
{
#include <stdlib.h>
#include "config.h"

#include <errno.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>

#include <qof.h>
#include <qofquery-p.h>
#include <qofquerycore-p.h>
#include <Account.h>
#include <TransLog.h>
#include <gnc-engine.h>
#include <SX-book.h>
#include <Recurrence.h>
#include <gncBillTerm.h>
#include <gncTaxTable.h>
#include <gncInvoice.h>
#include "gnc-prefs.h"
#include "gnc-pricedb.h"


#if defined( S_SPLINT_S )
#include "splint-defs.h"
#endif
}

#include <tuple>
#include <iomanip>

#include "gnc-backend-sql.h"

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

static void gnc_sql_init_object_handlers (void);
static void update_progress (GncSqlBackend* be);
static void finish_progress (GncSqlBackend* be);
static gboolean reset_version_info (GncSqlBackend* be);
static GncSqlStatementPtr build_insert_statement (GncSqlBackend* be,
                                                  const gchar* table_name,
                                                  QofIdTypeConst obj_name,
                                                  gpointer pObject,
                                                  const EntryVec& table);
static GncSqlStatementPtr build_update_statement (GncSqlBackend* be,
                                                  const gchar* table_name,
                                                  QofIdTypeConst obj_name,
                                                  gpointer pObject,
                                                  const EntryVec& table);
static GncSqlStatementPtr build_delete_statement (GncSqlBackend* be,
                                                  const gchar* table_name,
                                                  QofIdTypeConst obj_name,
                                                  gpointer pObject,
                                                  const EntryVec& table);

static GList* post_load_commodities = NULL;

#define TRANSACTION_NAME "trans"

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
    GncSqlBackend* be;
    QofInstance* inst;
    QofQuery* pQuery;
    gpointer pCompiledQuery;
    gnc_sql_query_info* pQueryInfo;
} sql_backend;

static QofLogModule log_module = G_LOG_DOMAIN;

#define SQLITE_PROVIDER_NAME "SQLite"

/* ================================================================= */
static OBEVec backend_registry;
void
gnc_sql_register_backend(OBEEntry&& entry)
{
    backend_registry.emplace_back(entry);
}

void
gnc_sql_register_backend(GncSqlObjectBackendPtr obe)
{
    backend_registry.emplace_back(make_tuple(std::string{obe->type()}, obe));
}

const OBEVec&
gnc_sql_get_backend_registry()
{
    return backend_registry;
}

GncSqlObjectBackendPtr
gnc_sql_get_object_backend(const std::string& type)
{
    auto entry = std::find_if(backend_registry.begin(),
                              backend_registry.end(),
                              [type](const OBEEntry& entry){
                                  return type == std::get<0>(entry);
                              });
    auto obe = std::get<1>(*entry);
    if (entry != backend_registry.end())
        return obe;
    return nullptr;
}

void
gnc_sql_init(GncSqlBackend* be)
{
    static gboolean initialized = FALSE;

    if (!initialized)
    {
        gnc_sql_init_object_handlers ();
        initialized = TRUE;
    }
}

/* ================================================================= */

static void
create_tables(const OBEEntry& entry, GncSqlBackend* be)
{
    std::string type;
    GncSqlObjectBackendPtr obe = nullptr;
    std::tie(type, obe) = entry;
    g_return_if_fail (obe->is_version (GNC_SQL_BACKEND_VERSION));
    update_progress(be);
    obe->create_tables(be);
}

/* ================================================================= */

/* Main object load order */
static const StrVec fixed_load_order
{ GNC_ID_BOOK, GNC_ID_COMMODITY, GNC_ID_ACCOUNT, GNC_ID_LOT };

/* Order in which business objects need to be loaded */
static const StrVec business_fixed_load_order =
{ GNC_ID_BILLTERM, GNC_ID_TAXTABLE, GNC_ID_INVOICE };

static void
initial_load(const OBEEntry& entry, GncSqlBackend* be)
{
    std::string type;
    GncSqlObjectBackendPtr obe = nullptr;
    std::tie(type, obe) = entry;
    g_return_if_fail(obe->is_version (GNC_SQL_BACKEND_VERSION));

    /* Don't need to load anything if it has already been loaded with
     * the fixed order.
     */
    if (std::find(fixed_load_order.begin(), fixed_load_order.end(),
                  type) != fixed_load_order.end()) return;
    if (std::find(business_fixed_load_order.begin(), business_fixed_load_order.end(),
                  type) != business_fixed_load_order.end()) return;

    obe->load_all (be);
}

void
gnc_sql_push_commodity_for_postload_processing (GncSqlBackend* be,
                                                gpointer comm)
{
    post_load_commodities = g_list_prepend (post_load_commodities, comm);
}

static void
commit_commodity (gpointer data)
{
    gnc_commodity* comm = GNC_COMMODITY (data);
    gnc_sql_commit_commodity (comm);
}

void
gnc_sql_load (GncSqlBackend* be,  QofBook* book, QofBackendLoadType loadType)
{
    Account* root;

    g_return_if_fail (be != NULL);
    g_return_if_fail (book != NULL);

    ENTER ("be=%p, book=%p", be, book);

    be->loading = TRUE;

    if (loadType == LOAD_TYPE_INITIAL_LOAD)
    {
        g_assert (be->book == NULL);
        be->book = book;

        /* Load any initial stuff. Some of this needs to happen in a certain order */
        for (auto type : fixed_load_order)
        {
            auto obe = gnc_sql_get_object_backend(type);
            if (obe)
            {
                update_progress(be);
                obe->load_all (be);
            }
        }
        for (auto type : business_fixed_load_order)
        {
            auto obe = gnc_sql_get_object_backend(type);
            if (obe)
            {
                update_progress(be);
                obe->load_all (be);
            }
        }

        root = gnc_book_get_root_account( book );
        gnc_account_foreach_descendant(root, (AccountCb)xaccAccountBeginEdit,
                                       nullptr);

        for (auto entry : backend_registry)
            initial_load(entry, be);

        gnc_account_foreach_descendant(root, (AccountCb)xaccAccountCommitEdit,
                                       nullptr);
    }
    else if (loadType == LOAD_TYPE_LOAD_ALL)
    {
        // Load all transactions
        auto obe = gnc_sql_get_object_backend (GNC_ID_TRANS);
        obe->load_all (be);
    }

    be->loading = FALSE;
    g_list_free_full (post_load_commodities, commit_commodity);
    post_load_commodities = NULL;

    /* Mark the sessoion as clean -- though it should never be marked
     * dirty with this backend
     */
    qof_book_mark_session_saved (book);
    finish_progress (be);

    LEAVE ("");
}

/* ================================================================= */

static gboolean
write_account_tree (GncSqlBackend* be, Account* root)
{
    GList* descendants;
    GList* node;
    gboolean is_ok = TRUE;

    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (root != NULL, FALSE);

    auto obe = gnc_sql_get_object_backend (GNC_ID_ACCOUNT);
    is_ok = obe->commit (be, QOF_INSTANCE (root));
    if (is_ok)
    {
        descendants = gnc_account_get_descendants (root);
        for (node = descendants; node != NULL && is_ok; node = g_list_next (node))
        {
            is_ok = obe->commit(be, QOF_INSTANCE (GNC_ACCOUNT (node->data)));
            if (!is_ok) break;
        }
        g_list_free (descendants);
    }
    update_progress (be);

    return is_ok;
}

static gboolean
write_accounts (GncSqlBackend* be)
{
    gboolean is_ok;

    g_return_val_if_fail (be != NULL, FALSE);

    update_progress (be);
    is_ok = write_account_tree (be, gnc_book_get_root_account (be->book));
    if (is_ok)
    {
        update_progress (be);
        is_ok = write_account_tree (be, gnc_book_get_template_root (be->book));
    }

    return is_ok;
}

static gboolean
write_tx (Transaction* tx, gpointer data)
{
    auto s = static_cast<write_objects_t*>(data);

    g_return_val_if_fail (tx != NULL, 0);
    g_return_val_if_fail (data != NULL, 0);

    s->commit (QOF_INSTANCE (tx));
    auto splitbe = gnc_sql_get_object_backend (GNC_ID_SPLIT);
    for (auto split_node = xaccTransGetSplitList (tx);
         split_node != nullptr && s->is_ok;
         split_node = g_list_next (split_node))
    {
        s->is_ok = splitbe->commit(s->be, QOF_INSTANCE(split_node->data));
    }
    update_progress (s->be);
    return (s->is_ok ? 0 : 1);
}

static gboolean
write_transactions (GncSqlBackend* be)
{
    g_return_val_if_fail (be != NULL, FALSE);

    auto obe = gnc_sql_get_object_backend(GNC_ID_TRANS);
    write_objects_t data{be, true, obe};

    (void)xaccAccountTreeForEachTransaction (
        gnc_book_get_root_account (be->book), write_tx, &data);
    update_progress (be);
    return data.is_ok;
}

static gboolean
write_template_transactions (GncSqlBackend* be)
{
    g_return_val_if_fail (be != NULL, FALSE);

    auto obe = gnc_sql_get_object_backend(GNC_ID_TRANS);
    write_objects_t data{be, true, obe};
    auto ra = gnc_book_get_template_root (be->book);
    if (gnc_account_n_descendants (ra) > 0)
    {
        (void)xaccAccountTreeForEachTransaction (ra, write_tx, &data);
        update_progress (be);
    }

    return data.is_ok;
}

static gboolean
write_schedXactions (GncSqlBackend* be)
{
    GList* schedXactions;
    SchedXaction* tmpSX;
    gboolean is_ok = TRUE;

    g_return_val_if_fail (be != NULL, FALSE);

    schedXactions = gnc_book_get_schedxactions (be->book)->sx_list;
    auto obe = gnc_sql_get_object_backend(GNC_ID_SCHEDXACTION);

    for (; schedXactions != NULL && is_ok; schedXactions = schedXactions->next)
    {
        tmpSX = static_cast<decltype (tmpSX)> (schedXactions->data);
        is_ok = obe->commit (be, QOF_INSTANCE (tmpSX));
    }
    update_progress (be);

    return is_ok;
}

static void
update_progress (GncSqlBackend* be)
{
    if (be->be.percentage != NULL)
        (be->be.percentage) (NULL, 101.0);
}

static void
finish_progress (GncSqlBackend* be)
{
    if (be->be.percentage != NULL)
        (be->be.percentage) (NULL, -1.0);
}

void
gnc_sql_sync_all (GncSqlBackend* be,  QofBook* book)
{
    gboolean is_ok;

    g_return_if_fail (be != NULL);
    g_return_if_fail (book != NULL);

    ENTER ("book=%p, be->book=%p", book, be->book);
    update_progress (be);
    (void)reset_version_info (be);

    /* Create new tables */
    be->is_pristine_db = TRUE;
    for(auto entry : backend_registry)
        create_tables(entry, be);

    /* Save all contents */
    be->book = book;
    is_ok = be->conn->begin_transaction ();

    // FIXME: should write the set of commodities that are used
    //write_commodities( be, book );
    if (is_ok)
    {
        auto obe = gnc_sql_get_object_backend(GNC_ID_BOOK);
        is_ok = obe->commit (be, QOF_INSTANCE (book));
    }
    if (is_ok)
    {
        is_ok = write_accounts (be);
    }
    if (is_ok)
    {
        is_ok = write_transactions (be);
    }
    if (is_ok)
    {
        is_ok = write_template_transactions (be);
    }
    if (is_ok)
    {
        is_ok = write_schedXactions (be);
    }
    if (is_ok)
    {
        for (auto entry : backend_registry)
            std::get<1>(entry)->write (be);
    }
    if (is_ok)
    {
        is_ok = be->conn->commit_transaction ();
    }
    if (is_ok)
    {
        be->is_pristine_db = FALSE;

        /* Mark the session as clean -- though it shouldn't ever get
         * marked dirty with this backend
         */
        qof_book_mark_session_saved (book);
    }
    else
    {
        if (!qof_backend_check_error ((QofBackend*)be))
            qof_backend_set_error ((QofBackend*)be, ERR_BACKEND_SERVER_ERR);
        is_ok = be->conn->rollback_transaction ();
    }
    finish_progress (be);
    LEAVE ("book=%p", book);
}

/* ================================================================= */
/* Routines to deal with the creation of multiple books. */

void
gnc_sql_begin_edit (GncSqlBackend* be, QofInstance* inst)
{
    g_return_if_fail (be != NULL);
    g_return_if_fail (inst != NULL);

    ENTER (" ");
    LEAVE ("");
}

void
gnc_sql_rollback_edit (GncSqlBackend* be, QofInstance* inst)
{
    g_return_if_fail (be != NULL);
    g_return_if_fail (inst != NULL);

    ENTER (" ");
    LEAVE ("");
}

static void
commit(const OBEEntry& entry, sql_backend* be_data)
{
    std::string type;
    GncSqlObjectBackendPtr obe= nullptr;
    std::tie(type, obe) = entry;
    g_return_if_fail (obe->is_version (GNC_SQL_BACKEND_VERSION));

    /* If this has already been handled, or is not the correct
     * handler, return
     */
    if (type != std::string{be_data->inst->e_type}) return;
    if (be_data->is_known) return;

    be_data->is_ok = obe->commit (be_data->be, be_data->inst);
    be_data->is_known = TRUE;
}

/* Commit_edit handler - find the correct backend handler for this object
 * type and call its commit handler
 */
void
gnc_sql_commit_edit (GncSqlBackend* be, QofInstance* inst)
{
    sql_backend be_data;
    gboolean is_dirty;
    gboolean is_destroying;
    gboolean is_infant;

    g_return_if_fail (be != NULL);
    g_return_if_fail (inst != NULL);

    if (qof_book_is_readonly (be->book))
    {
        qof_backend_set_error ((QofBackend*)be, ERR_BACKEND_READONLY);
        (void)be->conn->rollback_transaction ();
        return;
    }
    /* During initial load where objects are being created, don't commit
       anything, but do mark the object as clean. */
    if (be->loading)
    {
        qof_instance_mark_clean (inst);
        return;
    }

    // The engine has a PriceDB object but it isn't in the database
    if (strcmp (inst->e_type, "PriceDB") == 0)
    {
        qof_instance_mark_clean (inst);
        qof_book_mark_session_saved (be->book);
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

    if (!be->conn->begin_transaction ())
    {
        PERR ("gnc_sql_commit_edit(): begin_transaction failed\n");
        LEAVE ("Rolled back - database transaction begin error");
        return;
    }

    be_data.is_known = FALSE;
    be_data.be = be;
    be_data.inst = inst;
    be_data.is_ok = TRUE;

    for (auto entry : backend_registry)
        commit(entry, &be_data);

    if (!be_data.is_known)
    {
        PERR ("gnc_sql_commit_edit(): Unknown object type '%s'\n", inst->e_type);
        (void)be->conn->rollback_transaction ();

        // Don't let unknown items still mark the book as being dirty
        qof_book_mark_session_saved (be->book);
        qof_instance_mark_clean (inst);
        LEAVE ("Rolled back - unknown object type");
        return;
    }
    if (!be_data.is_ok)
    {
        // Error - roll it back
        (void)be->conn->rollback_transaction ();

        // This *should* leave things marked dirty
        LEAVE ("Rolled back - database error");
        return;
    }

    (void)be->conn->commit_transaction ();

    qof_book_mark_session_saved (be->book);
    qof_instance_mark_clean (inst);

    LEAVE ("");
}
/* ---------------------------------------------------------------------- */

/* Query processing */
static void
handle_and_term (QofQueryTerm* pTerm, GString* sql)
{
    GSList* pParamPath;
    QofQueryPredData* pPredData;
    gboolean isInverted;
    GSList* name;
    gchar val[G_ASCII_DTOSTR_BUF_SIZE];

    g_return_if_fail (pTerm != NULL);
    g_return_if_fail (sql != NULL);

    pParamPath = qof_query_term_get_param_path (pTerm);
    pPredData = qof_query_term_get_pred_data (pTerm);
    isInverted = qof_query_term_is_inverted (pTerm);

    if (strcmp (pPredData->type_name, QOF_TYPE_GUID) == 0)
    {
        query_guid_t guid_data = (query_guid_t)pPredData;
        GList* guid_entry;

        for (name = pParamPath; name != NULL; name = name->next)
        {
            if (name != pParamPath) g_string_append (sql, ".");
            g_string_append (sql, static_cast<char*> (name->data));
        }

        if (guid_data->options == QOF_GUID_MATCH_ANY)
        {
            if (isInverted) g_string_append (sql, " NOT ");
            g_string_append (sql, " IN (");
        }
        for (guid_entry = guid_data->guids; guid_entry != NULL;
             guid_entry = guid_entry->next)
        {
            if (guid_entry != guid_data->guids) g_string_append (sql, ".");
            (void)guid_to_string_buff (static_cast<GncGUID*> (guid_entry->data),
                                       val);
            g_string_append (sql, "'");
            g_string_append (sql, val);
            g_string_append (sql, "'");
        }
        if (guid_data->options == QOF_GUID_MATCH_ANY)
        {
            g_string_append (sql, ")");
        }
    }

    g_string_append (sql, "(");
    if (isInverted)
    {
        g_string_append (sql, "!");
    }

    for (name = pParamPath; name != NULL; name = name->next)
    {
        if (name != pParamPath) g_string_append (sql, ".");
        g_string_append (sql, static_cast<char*> (name->data));
    }

    if (pPredData->how == QOF_COMPARE_LT)
    {
        g_string_append (sql, "<");
    }
    else if (pPredData->how == QOF_COMPARE_LTE)
    {
        g_string_append (sql, "<=");
    }
    else if (pPredData->how == QOF_COMPARE_EQUAL)
    {
        g_string_append (sql, "=");
    }
    else if (pPredData->how == QOF_COMPARE_GT)
    {
        g_string_append (sql, ">");
    }
    else if (pPredData->how == QOF_COMPARE_GTE)
    {
        g_string_append (sql, ">=");
    }
    else if (pPredData->how == QOF_COMPARE_NEQ)
    {
        g_string_append (sql, "~=");
    }
    else
    {
        g_string_append (sql, "??");
    }

    if (strcmp (pPredData->type_name, "string") == 0)
    {
        query_string_t pData = (query_string_t)pPredData;
        g_string_append (sql, "'");
        g_string_append (sql, pData->matchstring);
        g_string_append (sql, "'");
    }
    else if (strcmp (pPredData->type_name, "date") == 0)
    {
        query_date_t pData = (query_date_t)pPredData;

        (void)gnc_timespec_to_iso8601_buff (pData->date, val);
        g_string_append (sql, "'");
        //g_string_append( sql, val, 4+1+2+1+2 );
        g_string_append (sql, "'");
    }
    else if (strcmp (pPredData->type_name, "numeric") == 0)
    {
        /* query_numeric_t pData = (query_numeric_t)pPredData; */

        g_string_append (sql, "numeric");
    }
    else if (strcmp (pPredData->type_name, QOF_TYPE_GUID) == 0)
    {
    }
    else if (strcmp (pPredData->type_name, "gint32") == 0)
    {
        query_int32_t pData = (query_int32_t)pPredData;

        sprintf (val, "%d", pData->val);
        g_string_append (sql, val);
    }
    else if (strcmp (pPredData->type_name, "gint64") == 0)
    {
        query_int64_t pData = (query_int64_t)pPredData;

        sprintf (val, "%" G_GINT64_FORMAT, pData->val);
        g_string_append (sql, val);
    }
    else if (strcmp (pPredData->type_name, "double") == 0)
    {
        query_double_t pData = (query_double_t)pPredData;

        g_ascii_dtostr (val, sizeof (val), pData->val);
        g_string_append (sql, val);
    }
    else if (strcmp (pPredData->type_name, "boolean") == 0)
    {
        query_boolean_t pData = (query_boolean_t)pPredData;

        sprintf (val, "%d", pData->val);
        g_string_append (sql, val);
    }
    else
    {
        g_assert (FALSE);
    }

    g_string_append (sql, ")");
}
#if 0 //The query compilation code was never tested so it isn't implemnted for GncSqlObjectBackend.
static void
compile_query(const OBEEntry& entry, sql_backend* be_data)
{
    std::string type;
    GncSqlObjectBackendPtr obe = nullptr;
    std::tie(type, obe) = entry;
    g_return_if_fail (obe->is_version (GNC_SQL_BACKEND_VERSION));

    // Is this the right item?
    if (type != std::string{be_data->pQueryInfo->searchObj}) return;
    if (be_data->is_ok) return;

    be_data->pQueryInfo->pCompiledQuery = (obe->compile_query)(
        be_data->be,
        be_data->pQuery);
    be_data->is_ok = TRUE;
}

gchar* gnc_sql_compile_query_to_sql (GncSqlBackend* be, QofQuery* query);

gpointer
gnc_sql_compile_query (QofBackend* pBEnd, QofQuery* pQuery)
{
    GncSqlBackend* be = (GncSqlBackend*)pBEnd;
    QofIdType searchObj;
    sql_backend be_data;
    gnc_sql_query_info* pQueryInfo;

    g_return_val_if_fail (pBEnd != NULL, NULL);
    g_return_val_if_fail (pQuery != NULL, NULL);

    ENTER (" ");

//gnc_sql_compile_query_to_sql( be, pQuery );
    searchObj = qof_query_get_search_for (pQuery);

    pQueryInfo = static_cast<decltype (pQueryInfo)> (
        g_malloc (sizeof (gnc_sql_query_info)));
    g_assert (pQueryInfo != NULL);
    pQueryInfo->pCompiledQuery = NULL;
    pQueryInfo->searchObj = searchObj;

    // Try various objects first
    be_data.is_ok = FALSE;
    be_data.be = be;
    be_data.pQuery = pQuery;
    be_data.pQueryInfo = pQueryInfo;

    for (auto entry : backend_registry)
        compile_query(entry, &be_data);
    if (be_data.is_ok)
    {
        LEAVE ("");
        return be_data.pQueryInfo;
    }

    LEAVE ("");

    return pQueryInfo;
}

static const gchar*
convert_search_obj (QofIdType objType)
{
    return (gchar*)objType;
}

gchar*
gnc_sql_compile_query_to_sql (GncSqlBackend* be, QofQuery* query)
{
    QofIdType searchObj;
    GString* sql;

    g_return_val_if_fail (be != NULL, NULL);
    g_return_val_if_fail (query != NULL, NULL);

    searchObj = qof_query_get_search_for (query);

    /* Convert search object type to table name */
    sql = g_string_new ("");
    g_string_append (sql, "SELECT * FROM ");
    g_string_append (sql, convert_search_obj (searchObj));
    if (!qof_query_has_terms (query))
    {
        g_string_append (sql, ";");
    }
    else
    {
        GList* orterms = qof_query_get_terms (query);
        GList* orTerm;

        g_string_append (sql, " WHERE ");

        for (orTerm = orterms; orTerm != NULL; orTerm = orTerm->next)
        {
            GList* andterms = (GList*)orTerm->data;
            GList* andTerm;

            if (orTerm != orterms) g_string_append (sql, " OR ");
            g_string_append (sql, "(");
            for (andTerm = andterms; andTerm != NULL; andTerm = andTerm->next)
            {
                if (andTerm != andterms) g_string_append (sql, " AND ");
                handle_and_term ((QofQueryTerm*)andTerm->data, sql);
            }
            g_string_append (sql, ")");
        }
    }

    DEBUG ("Compiled: %s\n", sql->str);
    return g_string_free (sql, FALSE);
}

static void
free_query(const OBEEntry& entry, sql_backend* be_data)
{
    std::string type;
    GncSqlObjectBackendPtr obe= nullptr;
    std::tie(type, obe) = entry;
    g_return_if_fail (obe->version == GNC_SQL_BACKEND_VERSION);
    if (be_data->is_ok) return;
    if (type != std::string{be_data->pQueryInfo->searchObj}) return;

    if (obe->free_query != nullptr)
    {
        (obe->free_query)(be_data->be, be_data->pCompiledQuery);
        be_data->is_ok = TRUE;
    }
}

void
gnc_sql_free_query (QofBackend* pBEnd, gpointer pQuery)
{
    GncSqlBackend* be = (GncSqlBackend*)pBEnd;
    gnc_sql_query_info* pQueryInfo = (gnc_sql_query_info*)pQuery;
    sql_backend be_data;

    g_return_if_fail (pBEnd != NULL);
    g_return_if_fail (pQuery != NULL);

    ENTER (" ");

    // Try various objects first
    be_data.is_ok = FALSE;
    be_data.be = be;
    be_data.pCompiledQuery = pQuery;
    be_data.pQueryInfo = pQueryInfo;

    for (auto entry : backend_registry)
        free_query(entry, &be_data);
    if (be_data.is_ok)
    {
        LEAVE ("");
        return;
    }

    if (pQueryInfo->pCompiledQuery != NULL)
    {
        DEBUG ("%s\n", (gchar*)pQueryInfo->pCompiledQuery);
        g_free (pQueryInfo->pCompiledQuery);
    }
    g_free (pQueryInfo);

    LEAVE ("");
}

static void
run_query(const OBEEntry& entry, sql_backend* be_data)
{
    std::string type;
    GncSqlObjectBackendPtr obe = nullptr;
    std::tie(type, obe) = entry;
    g_return_if_fail (obe->version == GNC_SQL_BACKEND_VERSION);
    if (be_data->is_ok) return;

    // Is this the right item?
    if (type != std::string{be_data->pQueryInfo->searchObj}) return;

    if (obe->run_query != nullptr)
    {
        (obe->run_query)(be_data->be, be_data->pCompiledQuery);
        be_data->is_ok = TRUE;
    }
}

void
gnc_sql_run_query (QofBackend* pBEnd, gpointer pQuery)
{
    GncSqlBackend* be = (GncSqlBackend*)pBEnd;
    gnc_sql_query_info* pQueryInfo = (gnc_sql_query_info*)pQuery;
    sql_backend be_data;

    g_return_if_fail (pBEnd != NULL);
    g_return_if_fail (pQuery != NULL);
    g_return_if_fail (!be->in_query);

    ENTER (" ");

    be->loading = TRUE;
    be->in_query = TRUE;

    qof_event_suspend ();

    // Try various objects first
    be_data.is_ok = FALSE;
    be_data.be = be;
    be_data.pCompiledQuery = pQueryInfo->pCompiledQuery;
    be_data.pQueryInfo = pQueryInfo;
    for (auto entry : backend_registry)
        run_query(entry, &be_data);
    be->loading = FALSE;
    be->in_query = FALSE;
    qof_event_resume ();
//    if( be_data.is_ok ) {
//        LEAVE( "" );
//        return;
//    }

    // Mark the book as clean
    qof_instance_mark_clean (QOF_INSTANCE (be->book));

//    DEBUG( "%s\n", (gchar*)pQueryInfo->pCompiledQuery );

    LEAVE ("");
}
#endif //if 0: query creation isn't used yet, code never tested.
/* ================================================================= */

static void
business_core_sql_init (void)
{
    /* Initialize our pointers into the backend subsystem */
    gnc_billterm_sql_initialize ();
    gnc_customer_sql_initialize ();
    gnc_employee_sql_initialize ();
    gnc_entry_sql_initialize ();
    gnc_invoice_sql_initialize ();
    gnc_job_sql_initialize ();
    gnc_order_sql_initialize ();
    gnc_taxtable_sql_initialize ();
    gnc_vendor_sql_initialize ();
}

static void
gnc_sql_init_object_handlers (void)
{
    gnc_sql_init_book_handler ();
    gnc_sql_init_commodity_handler ();
    gnc_sql_init_account_handler ();
    gnc_sql_init_budget_handler ();
    gnc_sql_init_price_handler ();
    gnc_sql_init_transaction_handler ();
    gnc_sql_init_slots_handler ();
    gnc_sql_init_recurrence_handler ();
    gnc_sql_init_schedxaction_handler ();
    gnc_sql_init_lot_handler ();

    /* And the business objects */
    business_core_sql_init ();
}

/* ================================================================= */
static gpointer
get_autoinc_id (void* object, const QofParam* param)
{
    // Just need a 0 to force a new autoinc value
    return (gpointer)0;
}

static void
set_autoinc_id (void* object, void* item)
{
    // Nowhere to put the ID
}

QofAccessFunc
GncSqlColumnTableEntry::get_getter (QofIdTypeConst obj_name) const noexcept
{
    QofAccessFunc getter;

    g_return_val_if_fail (obj_name != NULL, NULL);

    if (m_flags & COL_AUTOINC)
    {
        getter = get_autoinc_id;
    }
    else if (m_qof_param_name != NULL)
    {
        getter = qof_class_get_parameter_getter (obj_name, m_qof_param_name);
    }
    else
    {
        getter = m_getter;
    }

    return getter;
}

QofSetterFunc
GncSqlColumnTableEntry::get_setter(QofIdTypeConst obj_name) const noexcept
{
    QofSetterFunc setter = nullptr;
    if (m_flags & COL_AUTOINC)
    {
        setter = set_autoinc_id;
    }
    else if (m_qof_param_name != nullptr)
    {
        g_assert (obj_name != NULL);
        setter = qof_class_get_parameter_setter (obj_name, m_qof_param_name);
    }
    else
    {
        setter = m_setter;
    }
    return setter;
}

void
GncSqlColumnTableEntry::add_objectref_guid_to_query (const GncSqlBackend* be,
                                                     QofIdTypeConst obj_name,
                                                     const gpointer pObject,
                                                     PairVec& vec) const noexcept
{
    auto inst = get_row_value_from_object<QofInstance*>(obj_name, pObject);
    if (inst == nullptr) return;
    auto guid = qof_instance_get_guid (inst);
    if (guid != nullptr)
        vec.emplace_back (std::make_pair (std::string{m_col_name},
                                          std::string{guid_to_string(guid)}));
}

void
GncSqlColumnTableEntry::add_objectref_guid_to_table (const GncSqlBackend* be,
                                                     ColVec& vec) const noexcept
{
    g_return_if_fail (be != NULL);

    GncSqlColumnInfo info{*this, BCT_STRING, GUID_ENCODING_LENGTH, FALSE};
    vec.emplace_back(std::move(info));
}


/* ----------------------------------------------------------------- */
template<> void
GncSqlColumnTableEntryImpl<CT_STRING>::load (const GncSqlBackend* be,
                                             GncSqlRow& row,
                                             QofIdTypeConst obj_name,
                                             gpointer pObject) const noexcept
{
    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != NULL || get_setter(obj_name) != NULL);

    try
    {
        auto s = row.get_string_at_col (m_col_name);
        set_parameter(pObject, s.c_str(), get_setter(obj_name), m_gobj_param_name);
    }
    catch (std::invalid_argument) {}
}

template<> void
GncSqlColumnTableEntryImpl<CT_STRING>::add_to_table(const GncSqlBackend* be,
                                                  ColVec& vec) const noexcept
{
    g_return_if_fail (be != NULL);

    GncSqlColumnInfo info{*this, BCT_STRING, m_size, TRUE};
    vec.emplace_back(std::move(info));
}

/* char is unusual in that we get a pointer but don't deref it to pass
 * it to operator<<().
 */
template<> void
GncSqlColumnTableEntryImpl<CT_STRING>::add_to_query(const GncSqlBackend* be,
                                                    QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    auto s = get_row_value_from_object<char*>(obj_name, pObject);

    if (s != nullptr)
    {
        std::ostringstream stream;
        stream << s;
        vec.emplace_back (std::make_pair (std::string{m_col_name}, stream.str()));
        return;
    }
}

/* ----------------------------------------------------------------- */
typedef gint (*IntAccessFunc) (const gpointer);
typedef void (*IntSetterFunc) (const gpointer, gint);

template<> void
GncSqlColumnTableEntryImpl<CT_INT>::load (const GncSqlBackend* be, GncSqlRow& row,
                                          QofIdTypeConst obj_name,
                                          gpointer pObject) const noexcept
{

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != NULL || get_setter(obj_name) != NULL);

    auto val = row.get_int_at_col(m_col_name);
    set_parameter(pObject, val,
                  reinterpret_cast<IntSetterFunc>(get_setter(obj_name)), m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_INT>::add_to_table(const GncSqlBackend* be,
                                                 ColVec& vec) const noexcept
{
    g_return_if_fail (be != NULL);

    GncSqlColumnInfo info{*this, BCT_INT, 0, FALSE};
    vec.emplace_back(std::move(info));
}

template<> void
GncSqlColumnTableEntryImpl<CT_INT>::add_to_query(const GncSqlBackend* be,
                                                    QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    add_value_to_vec<int>(be, obj_name, pObject, vec);
}

/* ----------------------------------------------------------------- */
typedef gboolean (*BooleanAccessFunc) (const gpointer);
typedef void (*BooleanSetterFunc) (const gpointer, gboolean);

template<> void
GncSqlColumnTableEntryImpl<CT_BOOLEAN>::load (const GncSqlBackend* be,
                                              GncSqlRow& row,
                                              QofIdTypeConst obj_name,
                                              gpointer pObject)
    const noexcept
{
    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != NULL || get_setter(obj_name) != NULL);

    auto val = row.get_int_at_col (m_col_name);
    set_parameter(pObject, val,
                  reinterpret_cast<BooleanSetterFunc>(get_setter(obj_name)),
                  m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_BOOLEAN>::add_to_table(const GncSqlBackend* be,
                                                 ColVec& vec) const noexcept
{
    g_return_if_fail (be != NULL);

    GncSqlColumnInfo info{*this, BCT_INT, 0, FALSE};
    vec.emplace_back(std::move(info));
}

template<> void
GncSqlColumnTableEntryImpl<CT_BOOLEAN>::add_to_query(const GncSqlBackend* be,
                                                    QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    add_value_to_vec<int>(be, obj_name, pObject, vec);
}

/* ----------------------------------------------------------------- */
typedef gint64 (*Int64AccessFunc) (const gpointer);
typedef void (*Int64SetterFunc) (const gpointer, gint64);

template<> void
GncSqlColumnTableEntryImpl<CT_INT64>::load (const GncSqlBackend* be,
                                            GncSqlRow& row,
                                            QofIdTypeConst obj_name,
                                            gpointer pObject)
    const noexcept
{
    g_return_if_fail (m_gobj_param_name != nullptr || get_setter(obj_name) != nullptr);

    auto val = row.get_int_at_col (m_col_name);
    set_parameter(pObject, val,
                  reinterpret_cast<Int64SetterFunc>(get_setter(obj_name)),
                  m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_INT64>::add_to_table(const GncSqlBackend* be,
                                                   ColVec& vec) const noexcept
{
    g_return_if_fail (be != NULL);

    GncSqlColumnInfo info{*this, BCT_INT64, 0, FALSE};
    vec.emplace_back(std::move(info));
}

template<> void
GncSqlColumnTableEntryImpl<CT_INT64>::add_to_query(const GncSqlBackend* be,
                                                   QofIdTypeConst obj_name,
                                                   const gpointer pObject,
                                                   PairVec& vec) const noexcept
{
    add_value_to_vec<int64_t>(be, obj_name, pObject, vec);
}
/* ----------------------------------------------------------------- */

template<> void
GncSqlColumnTableEntryImpl<CT_DOUBLE>::load (const GncSqlBackend* be,
                                             GncSqlRow& row,
                                             QofIdTypeConst obj_name,
                                             gpointer pObject)
    const noexcept
{
    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != nullptr || get_setter(obj_name) != nullptr);
    double val;
    try
    {
        val = static_cast<double>(row.get_int_at_col(m_col_name));
    }
    catch (std::invalid_argument)
    {
        try
        {
            val = static_cast<double>(row.get_float_at_col(m_col_name));
        }
        catch (std::invalid_argument)
        {
            try
            {
                val = row.get_double_at_col(m_col_name);
            }
            catch (std::invalid_argument)
            {
                val = 0.0;
            }
        }
    }
    set_parameter(pObject, val, get_setter(obj_name), m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_DOUBLE>::add_to_table(const GncSqlBackend* be,
                                                 ColVec& vec) const noexcept
{
    g_return_if_fail (be != NULL);

    GncSqlColumnInfo info{*this, BCT_DOUBLE, 0, FALSE};
    vec.emplace_back(std::move(info));
}

template<> void
GncSqlColumnTableEntryImpl<CT_DOUBLE>::add_to_query(const GncSqlBackend* be,
                                                    QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    add_value_to_vec<double*>(be, obj_name, pObject, vec);
}

/* ----------------------------------------------------------------- */

template<> void
GncSqlColumnTableEntryImpl<CT_GUID>::load (const GncSqlBackend* be,
                                           GncSqlRow& row,
                                           QofIdTypeConst obj_name,
                                           gpointer pObject)
    const noexcept
{

    GncGUID guid;
    const GncGUID* pGuid;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != nullptr || get_setter(obj_name) != nullptr);

    std::string str;
    try
    {
        str = row.get_string_at_col(m_col_name);
    }
    catch (std::invalid_argument)
    {
        return;
    }
    (void)string_to_guid (str.c_str(), &guid);
    set_parameter(pObject, &guid, get_setter(obj_name), m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_GUID>::add_to_table(const GncSqlBackend* be,
                                                 ColVec& vec) const noexcept
{
    g_return_if_fail (be != NULL);

    GncSqlColumnInfo info{*this, BCT_STRING, GUID_ENCODING_LENGTH, FALSE};
    vec.emplace_back(std::move(info));
}

template<> void
GncSqlColumnTableEntryImpl<CT_GUID>::add_to_query(const GncSqlBackend* be,
                                                    QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    auto s = get_row_value_from_object<GncGUID*>(obj_name, pObject);

    if (s != nullptr)
    {

        vec.emplace_back (std::make_pair (std::string{m_col_name},
                                          std::string{guid_to_string(s)}));
        return;
    }
}
/* ----------------------------------------------------------------- */
typedef Timespec (*TimespecAccessFunc) (const gpointer);
typedef void (*TimespecSetterFunc) (const gpointer, Timespec*);

#define TIMESPEC_STR_FORMAT "%04d%02d%02d%02d%02d%02d"
#define TIMESPEC_COL_SIZE (4+2+2+2+2+2)

/* This is required because we're passing be->timespace_format to
 * g_strdup_printf.
 */
#pragma GCC diagnostic ignored "-Wformat-nonliteral"
gchar*
gnc_sql_convert_timespec_to_string (const GncSqlBackend* be, Timespec ts)
{
    time64 time;
    struct tm* tm;
    gint year;
    gchar* datebuf;

    time = timespecToTime64 (ts);
    tm = gnc_gmtime (&time);

    year = tm->tm_year + 1900;

    datebuf = g_strdup_printf (be->timespec_format,
                               year, tm->tm_mon + 1, tm->tm_mday, tm->tm_hour, tm->tm_min, tm->tm_sec);
    gnc_tm_free (tm);
    return datebuf;
}
#pragma GCC diagnostic warning "-Wformat-nonliteral"

template<> void
GncSqlColumnTableEntryImpl<CT_TIMESPEC>::load (const GncSqlBackend* be,
                                               GncSqlRow& row,
                                               QofIdTypeConst obj_name,
                                               gpointer pObject) const noexcept
{

    Timespec ts = {0, 0};
    gboolean isOK = FALSE;


    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != nullptr || get_setter(obj_name) != nullptr);

    try
    {
        auto val = row.get_time64_at_col(m_col_name);
        timespecFromTime64 (&ts, val);
    }
    catch (std::invalid_argument)
    {
        try
        {
            auto val = row.get_string_at_col(m_col_name);
            auto s = val.c_str();
            auto buf = g_strdup_printf ("%c%c%c%c-%c%c-%c%c %c%c:%c%c:%c%c",
                                        s[0], s[1], s[2], s[3], s[4], s[5],
                                        s[6], s[7], s[8], s[9], s[10], s[11],
                                        s[12], s[13]);
            ts = gnc_iso8601_to_timespec_gmt (buf);
            g_free (buf);
        }
        catch (std::invalid_argument)
        {
            return;
        }
    }
    set_parameter(pObject, &ts,
                  reinterpret_cast<TimespecSetterFunc>(get_setter(obj_name)),
                  m_gobj_param_name);
 }

template<> void
GncSqlColumnTableEntryImpl<CT_TIMESPEC>::add_to_table(const GncSqlBackend* be,
                                                 ColVec& vec) const noexcept
{
    g_return_if_fail (be != nullptr);

    GncSqlColumnInfo info{*this, BCT_DATETIME, TIMESPEC_COL_SIZE, FALSE};
    vec.emplace_back(std::move(info));
}

template<> void
GncSqlColumnTableEntryImpl<CT_TIMESPEC>::add_to_query(const GncSqlBackend* be,
                                                      QofIdTypeConst obj_name,
                                                      const gpointer pObject,
                                                      PairVec& vec) const noexcept
{
    TimespecAccessFunc ts_getter;
    Timespec ts;
/* Can't use get_row_value_from_object because g_object_get returns a
 * Timespec* and the getter returns a Timespec. Will be fixed by the
 * replacement of timespecs with time64s.
 */
    g_return_if_fail (be != NULL);
    g_return_if_fail (obj_name != NULL);
    g_return_if_fail (pObject != NULL);

    if (m_gobj_param_name != NULL)
    {
        Timespec* pts;
        g_object_get (pObject, m_gobj_param_name, &pts, NULL);
        ts = *pts;
    }
    else
    {
        ts_getter = (TimespecAccessFunc)get_getter (obj_name);
        g_return_if_fail (ts_getter != NULL);
        ts = (*ts_getter) (pObject);
    }

    if (ts.tv_sec != 0 || ts.tv_nsec != 0)
    {
        char* datebuf = gnc_sql_convert_timespec_to_string (be, ts);
        vec.emplace_back (std::make_pair (std::string{m_col_name},
                                          std::string{datebuf}));
        return;
    }
}

/* ----------------------------------------------------------------- */
#define DATE_COL_SIZE 8

template<> void
GncSqlColumnTableEntryImpl<CT_GDATE>::load (const GncSqlBackend* be,
                                            GncSqlRow& row,
                                            QofIdTypeConst obj_name,
                                            gpointer pObject) const noexcept
{
    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != nullptr || get_setter(obj_name) != nullptr);
    if (row.is_col_null(m_col_name))
        return;
    GDate date;
    g_date_clear (&date, 1);
    try
    {
	/* timespec_to_gdate applies the tz, and gdates are saved
	 * as ymd, so we don't want that.
	 */
	auto time = row.get_time64_at_col(m_col_name);
	auto tm = gnc_gmtime(&time);
	g_date_set_dmy(&date, tm->tm_mday,
		       static_cast<GDateMonth>(tm->tm_mon + 1),
		       tm->tm_year + 1900);
	free(tm);
    }
    catch (std::invalid_argument)
    {
        try
        {
            std::string str = row.get_string_at_col(m_col_name);
            if (str.empty()) return;
            auto year = static_cast<GDateYear>(stoi (str.substr (0,4)));
            auto month = static_cast<GDateMonth>(stoi (str.substr (4,2)));
            auto day = static_cast<GDateDay>(stoi (str.substr (6,2)));

            if (year != 0 || month != 0 || day != (GDateDay)0)
                g_date_set_dmy(&date, day, month, year);

        }
        catch (std::invalid_argument)
        {
            return;
        }
    }
    set_parameter(pObject, &date, get_setter(obj_name), m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_GDATE>::add_to_table(const GncSqlBackend* be,
                                                 ColVec& vec) const noexcept
{
    g_return_if_fail (be != NULL);

    GncSqlColumnInfo info{*this,  BCT_DATE, DATE_COL_SIZE, FALSE};
    vec.emplace_back(std::move(info));
}

template<> void
GncSqlColumnTableEntryImpl<CT_GDATE>::add_to_query(const GncSqlBackend* be,
                                                    QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    GDate *date = get_row_value_from_object<GDate*>(obj_name, pObject);

    if (date && g_date_valid (date))
    {
        std::ostringstream buf;
        buf << std::setfill ('0') << std::setw (4) << g_date_get_year (date) <<
            std::setw (2) << g_date_get_month (date) <<
            std::setw (2) << static_cast<int>(g_date_get_day (date));
        vec.emplace_back (std::make_pair (std::string{m_col_name}, buf.str()));
        return;
    }
}

/* ----------------------------------------------------------------- */
typedef gnc_numeric (*NumericGetterFunc) (const gpointer);
typedef void (*NumericSetterFunc) (gpointer, gnc_numeric*);

static const EntryVec numeric_col_table =
{
    gnc_sql_make_table_entry<CT_INT64>("num", 0, COL_NNUL, "guid"),
    gnc_sql_make_table_entry<CT_INT64>("denom", 0, COL_NNUL, "guid")
};

template<> void
GncSqlColumnTableEntryImpl<CT_NUMERIC>::load (const GncSqlBackend* be,
                                              GncSqlRow& row,
                                              QofIdTypeConst obj_name,
                                              gpointer pObject) const noexcept
{


    g_return_if_fail (pObject != NULL);
    g_return_if_fail (m_gobj_param_name != nullptr || get_setter(obj_name) != nullptr);
    gnc_numeric n;
    try
    {
        auto buf = g_strdup_printf ("%s_num", m_col_name);
        auto num = row.get_int_at_col (buf);
        g_free (buf);
        buf = g_strdup_printf ("%s_denom", m_col_name);
        auto denom = row.get_int_at_col (buf);
        n = gnc_numeric_create (num, denom);
    }
    catch (std::invalid_argument)
    {
        return;
    }
    set_parameter(pObject, &n,
                  reinterpret_cast<NumericSetterFunc>(get_setter(obj_name)),
                  m_gobj_param_name);
}

template<> void
GncSqlColumnTableEntryImpl<CT_NUMERIC>::add_to_table(const GncSqlBackend* be,
                                                     ColVec& vec) const noexcept
{
    g_return_if_fail (be != NULL);

    for (auto const& subtable_row : numeric_col_table)
    {
        gchar* buf = g_strdup_printf("%s_%s", m_col_name,
                                     subtable_row->m_col_name);
        GncSqlColumnInfo info(buf, BCT_INT64, 0, false, false,
                              m_flags & COL_PKEY, m_flags & COL_NNUL);
        vec.emplace_back(std::move(info));
    }
}

template<> void
GncSqlColumnTableEntryImpl<CT_NUMERIC>::add_to_query(const GncSqlBackend* be,
                                                     QofIdTypeConst obj_name,
                                                     const gpointer pObject,
                                                     PairVec& vec) const noexcept
{
/* We can't use get_row_value_from_object for the same reason as Timespec. */
    NumericGetterFunc getter;
    gnc_numeric n;

    g_return_if_fail (be != NULL);
    g_return_if_fail (obj_name != NULL);
    g_return_if_fail (pObject != NULL);

    if (m_gobj_param_name != nullptr)
    {
        gnc_numeric* s;
        g_object_get (pObject, m_gobj_param_name, &s, NULL);
        n = *s;
    }
    else
    {
        getter = reinterpret_cast<NumericGetterFunc>(get_getter (obj_name));
        if (getter != NULL)
        {
            n = (*getter) (pObject);
        }
        else
        {
            n = gnc_numeric_zero ();
        }
    }

    std::ostringstream buf;
    std::string num_col{m_col_name};
    std::string denom_col{m_col_name};
    num_col += "_num";
    denom_col += "_denom";
    buf << gnc_numeric_num (n);
    vec.emplace_back (std::make_pair (num_col, buf.str ()));
    buf.str ("");
    buf << gnc_numeric_denom (n);
    vec.emplace_back (denom_col, buf.str ());
}

/* ================================================================= */

void
_retrieve_guid_ (gpointer pObject,  gpointer pValue)
{
    GncGUID* pGuid = (GncGUID*)pObject;
    GncGUID* guid = (GncGUID*)pValue;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (pValue != NULL);

    memcpy (pGuid, guid, sizeof (GncGUID));
}


// Table to retrieve just the guid
static EntryVec guid_table
{
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, 0, nullptr, _retrieve_guid_)
};

const GncGUID*
gnc_sql_load_guid (const GncSqlBackend* be, GncSqlRow& row)
{
    static GncGUID guid;

    g_return_val_if_fail (be != NULL, NULL);

    gnc_sql_load_object (be, row, NULL, &guid, guid_table);

    return &guid;
}

// Table to retrieve just the guid
static EntryVec tx_guid_table
{
    gnc_sql_make_table_entry<CT_GUID>("tx_guid", 0, 0, nullptr, _retrieve_guid_)
 };

void
gnc_sql_load_object (const GncSqlBackend* be, GncSqlRow& row,
                     QofIdTypeConst obj_name, gpointer pObject,
                     const EntryVec& table)
{
    QofSetterFunc setter;

    g_return_if_fail (be != NULL);
    g_return_if_fail (pObject != NULL);

    for (auto const& table_row : table)
    {
        table_row->load (be, row, obj_name, pObject);
    }
}

/* ================================================================= */
GncSqlStatementPtr
gnc_sql_create_select_statement (GncSqlBackend* be, const gchar* table_name)
{
    g_return_val_if_fail (be != NULL, NULL);
    g_return_val_if_fail (table_name != NULL, NULL);

    auto sql = g_strdup_printf ("SELECT * FROM %s", table_name);
    auto stmt = gnc_sql_create_statement_from_sql (be, sql);
    g_free (sql);
    return stmt;
}

static GncSqlStatementPtr
create_single_col_select_statement (GncSqlBackend* be,
                                    const gchar* table_name,
                                    const GncSqlColumnTableEntryPtr table_row)
{
    g_return_val_if_fail (be != NULL, NULL);
    g_return_val_if_fail (table_name != NULL, NULL);

    auto sql = std::string{"SELECT "} + table_row->name() + " FROM " + table_name;
    return gnc_sql_create_statement_from_sql (be, sql.c_str());
}

/* ================================================================= */

GncSqlResultPtr
gnc_sql_execute_select_statement (GncSqlBackend* be,
                                  const GncSqlStatementPtr& stmt)
{

    g_return_val_if_fail (be != NULL, NULL);
    g_return_val_if_fail (stmt != NULL, NULL);

    auto result = be->conn->execute_select_statement (stmt);
    if (result == NULL)
    {
        PERR ("SQL error: %s\n", stmt->to_sql());
        if (!qof_backend_check_error(&be->be))
            qof_backend_set_error (&be->be, ERR_BACKEND_SERVER_ERR);
    }

    return result;
}

GncSqlStatementPtr
gnc_sql_create_statement_from_sql (GncSqlBackend* be, const gchar* sql)
{
    g_return_val_if_fail (be != NULL, NULL);
    g_return_val_if_fail (sql != NULL, NULL);

    auto stmt = be->conn->create_statement_from_sql (sql);
    if (stmt == nullptr)
    {
        PERR ("SQL error: %s\n", sql);
        if (!qof_backend_check_error(&be->be))
            qof_backend_set_error (&be->be, ERR_BACKEND_SERVER_ERR);
    }

    return stmt;
}

GncSqlResultPtr
gnc_sql_execute_select_sql (GncSqlBackend* be, const gchar* sql)
{
    g_return_val_if_fail (be != NULL, NULL);
    g_return_val_if_fail (sql != NULL, NULL);

    auto stmt = gnc_sql_create_statement_from_sql (be, sql);
    if (stmt == nullptr)
    {
        return nullptr;
    }
    auto result = be->conn->execute_select_statement (stmt);
    if (result == nullptr)
    {
        PERR ("SQL error: %s\n", sql);
        if (!qof_backend_check_error(&be->be))
            qof_backend_set_error (&be->be, ERR_BACKEND_SERVER_ERR);
    }

    return result;
}

gint
gnc_sql_execute_nonselect_sql (GncSqlBackend* be, const gchar* sql)
{
    g_return_val_if_fail (be != NULL, 0);
    g_return_val_if_fail (sql != NULL, 0);

    auto stmt = gnc_sql_create_statement_from_sql (be, sql);
    if (stmt == NULL)
    {
        return -1;
    }
    auto result = be->conn->execute_nonselect_statement (stmt);
    return result;
}

guint
gnc_sql_append_guid_list_to_sql (GString* sql, GList* list, guint maxCount)
{
    gchar guid_buf[GUID_ENCODING_LENGTH + 1];
    gboolean first_guid = TRUE;
    guint count;

    g_return_val_if_fail (sql != NULL, 0);

    if (list == NULL) return 0;

    for (count = 0; list != NULL && count < maxCount; list = list->next, count++)
    {
        QofInstance* inst = QOF_INSTANCE (list->data);
        (void)guid_to_string_buff (qof_instance_get_guid (inst), guid_buf);

        if (!first_guid)
        {
            (void)g_string_append (sql, ",");
        }
        (void)g_string_append (sql, "'");
        (void)g_string_append (sql, guid_buf);
        (void)g_string_append (sql, "'");
        first_guid = FALSE;
    }

    return count;
}
/* ================================================================= */
static PairVec
get_object_values (GncSqlBackend* be, QofIdTypeConst obj_name,
                   gpointer pObject, const EntryVec& table)
{
    PairVec vec;

    for (auto const& table_row : table)
    {
        if (!(table_row->is_autoincr()))
        {
            table_row->add_to_query (be, obj_name, pObject, vec);
        }
    }
    return vec;
}

gboolean
gnc_sql_object_is_it_in_db (GncSqlBackend* be, const gchar* table_name,
                            QofIdTypeConst obj_name, gpointer pObject,
                            const EntryVec& table)
{
    guint count;
    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (table_name != NULL, FALSE);
    g_return_val_if_fail (obj_name != NULL, FALSE);
    g_return_val_if_fail (pObject != NULL, FALSE);

    /* SELECT * FROM */
    auto stmt = create_single_col_select_statement (be, table_name, table[0]);
    g_assert (stmt != NULL);

    /* WHERE */
    PairVec values{get_object_values(be, obj_name, pObject, table)};
    stmt->add_where_cond(obj_name, values);
    auto result = gnc_sql_execute_select_statement (be, stmt);
    if (result != NULL)
    {
        auto retval = result->size() > 0;
        delete result;
        return retval;
    }
    return false;
}

gboolean
gnc_sql_do_db_operation (GncSqlBackend* be,
                         E_DB_OPERATION op,
                         const gchar* table_name,
                         QofIdTypeConst obj_name, gpointer pObject,
                         const EntryVec& table)
{
    GncSqlStatementPtr stmt;
    gboolean ok = FALSE;

    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (table_name != NULL, FALSE);
    g_return_val_if_fail (obj_name != NULL, FALSE);
    g_return_val_if_fail (pObject != NULL, FALSE);

    if (op == OP_DB_INSERT)
    {
        stmt = build_insert_statement (be, table_name, obj_name, pObject, table);
    }
    else if (op == OP_DB_UPDATE)
    {
        stmt = build_update_statement (be, table_name, obj_name, pObject, table);
    }
    else if (op == OP_DB_DELETE)
    {
        stmt = build_delete_statement (be, table_name, obj_name, pObject, table);
    }
    else
    {
        g_assert (FALSE);
    }
    if (stmt != nullptr)
    {
        auto result = be->conn->execute_nonselect_statement (stmt);
        if (result == -1)
        {
            PERR ("SQL error: %s\n", stmt->to_sql());
            if (!qof_backend_check_error(&be->be))
                qof_backend_set_error (&be->be, ERR_BACKEND_SERVER_ERR);
        }
        else
        {
            ok = TRUE;
        }
    }

    return ok;
}

static GncSqlStatementPtr
build_insert_statement (GncSqlBackend* be,
                        const gchar* table_name,
                        QofIdTypeConst obj_name, gpointer pObject,
                        const EntryVec& table)
{
    GncSqlStatementPtr stmt;
    PairVec col_values;
    std::ostringstream sql;

    g_return_val_if_fail (be != NULL, NULL);
    g_return_val_if_fail (table_name != NULL, NULL);
    g_return_val_if_fail (obj_name != NULL, NULL);
    g_return_val_if_fail (pObject != NULL, NULL);
    PairVec values{get_object_values(be, obj_name, pObject, table)};

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
        sql << be->conn->quote_string(col_value.second);
    }
    sql << ")";

    stmt = be->conn->create_statement_from_sql(sql.str());
    return stmt;
}

static GncSqlStatementPtr
build_update_statement (GncSqlBackend* be,
                        const gchar* table_name,
                        QofIdTypeConst obj_name, gpointer pObject,
                        const EntryVec& table)
{
    GncSqlStatementPtr stmt;
    std::ostringstream sql;

    g_return_val_if_fail (be != NULL, NULL);
    g_return_val_if_fail (table_name != NULL, NULL);
    g_return_val_if_fail (obj_name != NULL, NULL);
    g_return_val_if_fail (pObject != NULL, NULL);


    PairVec values{get_object_values (be, obj_name, pObject, table)};

    // Create the SQL statement
    sql <<  "UPDATE " << table_name << " SET ";

    for (auto const& col_value : values)
    {
        if (col_value != *values.begin())
            sql << ",";
        sql << col_value.first << "=" <<
            be->conn->quote_string(col_value.second);
    }

    stmt = be->conn->create_statement_from_sql(sql.str());
    /* We want our where condition to be just the first column and
     * value, i.e. the guid of the object.
     */
    values.erase(values.begin() + 1, values.end());
    stmt->add_where_cond(obj_name, values);
    return stmt;
}

static GncSqlStatementPtr
build_delete_statement (GncSqlBackend* be,
                        const gchar* table_name,
                        QofIdTypeConst obj_name, gpointer pObject,
                        const EntryVec& table)
{
    std::ostringstream sql;

    g_return_val_if_fail (be != NULL, NULL);
    g_return_val_if_fail (table_name != NULL, NULL);
    g_return_val_if_fail (obj_name != NULL, NULL);
    g_return_val_if_fail (pObject != NULL, NULL);

    sql << "DELETE FROM " << table_name;
    auto stmt = be->conn->create_statement_from_sql (sql.str());

    /* WHERE */
    PairVec values;
    table[0]->add_to_query (be, obj_name, pObject, values);
    PairVec col_values{values[0]};
    stmt->add_where_cond (obj_name, col_values);

    return stmt;
}

/* ================================================================= */
bool
GncSqlObjectBackend::commit (GncSqlBackend* be, QofInstance* inst)
{
    const GncGUID* guid;
    gboolean is_infant;
    E_DB_OPERATION op;
    gboolean is_ok;

    is_infant = qof_instance_get_infant (inst);
    if (qof_instance_get_destroying (inst))
    {
        op = OP_DB_DELETE;
    }
    else if (be->is_pristine_db || is_infant)
    {
        op = OP_DB_INSERT;
    }
    else
    {
        op = OP_DB_UPDATE;
    }
    is_ok = gnc_sql_do_db_operation (be, op, m_table_name.c_str(),
                                     m_type_name.c_str(), inst, m_col_table);

    if (is_ok)
    {
        // Now, commit any slots
        guid = qof_instance_get_guid (inst);
        if (!qof_instance_get_destroying (inst))
        {
            is_ok = gnc_sql_slots_save (be, guid, is_infant, inst);
        }
        else
        {
            is_ok = gnc_sql_slots_delete (be, guid);
        }
    }

    return is_ok;
}

/* ================================================================= */

static gboolean
do_create_table (const GncSqlBackend* be, const gchar* table_name,
                 const EntryVec& col_table)
{
    ColVec info_vec;
    gboolean ok = FALSE;

    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (table_name != NULL, FALSE);

    for (auto const& table_row : col_table)
    {
        table_row->add_to_table (be, info_vec);
    }
    ok = be->conn->create_table (table_name, info_vec);
    return ok;
}

gboolean
gnc_sql_create_table (GncSqlBackend* be, const char* table_name,
                      gint table_version, const EntryVec& col_table)
{
    gboolean ok;

    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (table_name != NULL, FALSE);

    DEBUG ("Creating %s table\n", table_name);

    ok = do_create_table (be, table_name, col_table);
    if (ok)
    {
        ok = gnc_sql_set_table_version (be, table_name, table_version);
    }
    return ok;
}

void
GncSqlObjectBackend::create_tables (GncSqlBackend* be)
{
    g_return_if_fail (be != nullptr);
    int version = gnc_sql_get_table_version (be, m_table_name.c_str());
    if (version == 0) //No tables, otherwise version will be >= 1. 
        gnc_sql_create_table (be, m_table_name.c_str(),
                              m_version, m_col_table);
    else if (version != m_version)
        PERR("Version mismatch in table %s, expecting %d but backend is %d."
             "Table creation aborted.", m_table_name.c_str(), m_version, version);
}

gboolean
gnc_sql_create_temp_table (const GncSqlBackend* be, const gchar* table_name,
                           const EntryVec& col_table)
{
    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (table_name != NULL, FALSE);

    return do_create_table (be, table_name, col_table);
}

gboolean
gnc_sql_create_index (const GncSqlBackend* be, const gchar* index_name,
                      const gchar* table_name,
                      const EntryVec& col_table)
{
    gboolean ok;

    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (index_name != NULL, FALSE);
    g_return_val_if_fail (table_name != NULL, FALSE);

    ok = be->conn->create_index (index_name, table_name, col_table);
    return ok;
}

gint
gnc_sql_get_table_version (const GncSqlBackend* be, const gchar* table_name)
{
    g_return_val_if_fail (be != NULL, 0);
    g_return_val_if_fail (table_name != NULL, 0);

    /* If the db is pristine because it's being saved, the table does not exist. */
    if (be->is_pristine_db)
    {
        return 0;
    }

    return GPOINTER_TO_INT (g_hash_table_lookup (be->versions, table_name));
}

/* Create a temporary table, copy the data from the old table, delete the
   old table, then rename the new one. */
void
gnc_sql_upgrade_table (GncSqlBackend* be, const gchar* table_name,
                       const EntryVec& col_table)
{
    gchar* sql;
    gchar* temp_table_name;

    g_return_if_fail (be != NULL);
    g_return_if_fail (table_name != NULL);

    DEBUG ("Upgrading %s table\n", table_name);

    temp_table_name = g_strdup_printf ("%s_new", table_name);
    (void)gnc_sql_create_temp_table (be, temp_table_name, col_table);
    sql = g_strdup_printf ("INSERT INTO %s SELECT * FROM %s",
                           temp_table_name, table_name);
    (void)gnc_sql_execute_nonselect_sql (be, sql);
    g_free (sql);

    sql = g_strdup_printf ("DROP TABLE %s", table_name);
    (void)gnc_sql_execute_nonselect_sql (be, sql);
    g_free (sql);

    sql = g_strdup_printf ("ALTER TABLE %s RENAME TO %s", temp_table_name,
                           table_name);
    (void)gnc_sql_execute_nonselect_sql (be, sql);
    g_free (sql);
    g_free (temp_table_name);
}

/* Adds one or more columns to an existing table. */
gboolean gnc_sql_add_columns_to_table (GncSqlBackend* be, const gchar* table_name,
                                       const EntryVec& new_col_table)
{
    ColVec info_vec;
    gboolean ok = FALSE;

    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (table_name != NULL, FALSE);

    for (auto const& table_row : new_col_table)
    {
        table_row->add_to_table (be, info_vec);
    }
    ok = be->conn->add_columns_to_table(table_name, info_vec);
    return ok;
}

/* ================================================================= */
#define VERSION_TABLE_NAME "versions"
#define MAX_TABLE_NAME_LEN 50
#define TABLE_COL_NAME "table_name"
#define VERSION_COL_NAME "table_version"

static EntryVec version_table
{
    gnc_sql_make_table_entry<CT_STRING>(
        TABLE_COL_NAME, MAX_TABLE_NAME_LEN, COL_PKEY | COL_NNUL),
    gnc_sql_make_table_entry<CT_INT>(VERSION_COL_NAME, 0, COL_NNUL)
};

/**
 * Sees if the version table exists, and if it does, loads the info into
 * the version hash table.  Otherwise, it creates an empty version table.
 *
 * @param be Backend struct
 */
void
gnc_sql_init_version_info (GncSqlBackend* be)
{
    g_return_if_fail (be != NULL);

    if (be->versions != NULL)
    {
        g_hash_table_destroy (be->versions);
    }
    be->versions = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);

    if (be->conn->does_table_exist (VERSION_TABLE_NAME))
    {
        auto sql = g_strdup_printf ("SELECT * FROM %s", VERSION_TABLE_NAME);
        auto result = gnc_sql_execute_select_sql (be, sql);
        g_free (sql);
        for (const auto& row : *result)
        {
            auto name = row.get_string_at_col (TABLE_COL_NAME);
            auto version = row.get_int_at_col (VERSION_COL_NAME);
            g_hash_table_insert (be->versions, g_strdup (name.c_str()),
                                 GINT_TO_POINTER (version));
        }
    }
    else
    {
        do_create_table (be, VERSION_TABLE_NAME, version_table);
        gnc_sql_set_table_version (be, "Gnucash",
                                   gnc_prefs_get_long_version ());
        gnc_sql_set_table_version (be, "Gnucash-Resave",
                                   GNUCASH_RESAVE_VERSION);
    }
}

/**
 * Resets the version table information by removing all version table info.
 * It also recreates the version table in the db.
 *
 * @param be Backend struct
 * @return TRUE if successful, FALSE if error
 */
static gboolean
reset_version_info (GncSqlBackend* be)
{
    gboolean ok;

    g_return_val_if_fail (be != NULL, FALSE);

    ok = do_create_table (be, VERSION_TABLE_NAME, version_table);
    if (be->versions == NULL)
    {
        be->versions = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    }
    else
    {
        g_hash_table_remove_all (be->versions);
    }

    gnc_sql_set_table_version (be, "Gnucash", gnc_prefs_get_long_version ());
    gnc_sql_set_table_version (be, "Gnucash-Resave", GNUCASH_RESAVE_VERSION);
    return ok;
}

/**
 * Finalizes the version table info by destroying the hash table.
 *
 * @param be Backend struct
 */
void
gnc_sql_finalize_version_info (GncSqlBackend* be)
{
    g_return_if_fail (be != NULL);

    if (be->versions != NULL)
    {
        g_hash_table_destroy (be->versions);
        be->versions = NULL;
    }
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
gboolean
gnc_sql_set_table_version (GncSqlBackend* be, const gchar* table_name,
                           gint version)
{
    gchar* sql;
    gint cur_version;
    gint status;

    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (table_name != NULL, FALSE);
    g_return_val_if_fail (version > 0, FALSE);

    cur_version = gnc_sql_get_table_version (be, table_name);
    if (cur_version != version)
    {
        if (cur_version == 0)
        {
            sql = g_strdup_printf ("INSERT INTO %s VALUES('%s',%d)", VERSION_TABLE_NAME,
                                   table_name, version);
        }
        else
        {
            sql = g_strdup_printf ("UPDATE %s SET %s=%d WHERE %s='%s'", VERSION_TABLE_NAME,
                                   VERSION_COL_NAME, version,
                                   TABLE_COL_NAME, table_name);
        }
        status = gnc_sql_execute_nonselect_sql (be, sql);
        if (status == -1)
        {
            PERR ("SQL error: %s\n", sql);
            if (!qof_backend_check_error(&be->be))
                qof_backend_set_error (&be->be, ERR_BACKEND_SERVER_ERR);
        }
        g_free (sql);
    }

    g_hash_table_insert (be->versions, g_strdup (table_name),
                         GINT_TO_POINTER (version));

    return TRUE;
}

/* This is necessary for 64-bit builds because g++ complains
 * that reinterpret_casting a void* (64 bits) to an int (32 bits)
 * loses precision, so we have to explicitly dispose of the precision.
 * FIXME: We shouldn't be storing ints in ptrs in the first place.
 */
#ifdef __LP64__
template <> int
GncSqlColumnTableEntry::get_row_value_from_object<int>(QofIdTypeConst obj_name,
                                                       const gpointer pObject,
                                                       std::false_type) const
{
    g_return_val_if_fail(obj_name != nullptr && pObject != nullptr, 0);
    int result = 0;
    if (m_gobj_param_name != nullptr)
        g_object_get(pObject, m_gobj_param_name, &result, NULL );
    else
    {
        QofAccessFunc getter = get_getter(obj_name);
        if (getter != nullptr)
        {
            auto value = ((getter)(pObject, nullptr));
            result = reinterpret_cast<uint64_t>(value) &
                UINT64_C(0x00000000FFFFFFFF);
        }
    }
    return result;
}
#endif

GncSqlRow&
GncSqlRow::operator++()
{
    auto& new_row =  m_iter->operator++();
    if (new_row != *this)
        m_iter = nullptr;
    return new_row;
}

/*
  GncSqlResult*
  GncSqlRow::operator*()
  {
  return m_iter->operator*();
  }
*/
/* ========================== END OF FILE ===================== */
