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
#include <qofbackend-p.h>
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
#include "gnc-pricedb.h"


#if defined( S_SPLINT_S )
#include "splint-defs.h"
#endif
}

#include <tuple>
#include <iomanip>

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
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
static GncSqlStatementPtr build_insert_statement (GncSqlBackend* sql_be,
                                                  const gchar* table_name,
                                                  QofIdTypeConst obj_name,
                                                  gpointer pObject,
                                                  const EntryVec& table);
static GncSqlStatementPtr build_update_statement (GncSqlBackend* sql_be,
                                                  const gchar* table_name,
                                                  QofIdTypeConst obj_name,
                                                  gpointer pObject,
                                                  const EntryVec& table);
static GncSqlStatementPtr build_delete_statement (GncSqlBackend* sql_be,
                                                  const gchar* table_name,
                                                  QofIdTypeConst obj_name,
                                                  gpointer pObject,
                                                  const EntryVec& table);

#define TRANSACTION_NAME "trans"

static QofLogModule log_module = G_LOG_DOMAIN;

#define SQLITE_PROVIDER_NAME "SQLite"

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

gchar* gnc_sql_compile_query_to_sql (GncSqlBackend* sql_be, QofQuery* query);

gpointer
gnc_sql_compile_query (QofBackend* qof_be, QofQuery* pQuery)
{
    GncSqlBackend* sql_be = reinterpret_cast<decltype(sql_be)>(qof_be);
    QofIdType searchObj;
    sql_backend be_data;
    gnc_sql_query_info* pQueryInfo;

    g_return_val_if_fail (sql_be != NULL, NULL);
    g_return_val_if_fail (pQuery != NULL, NULL);

    ENTER (" ");

//gnc_sql_compile_query_to_sql( sql_be, pQuery );
    searchObj = qof_query_get_search_for (pQuery);

    pQueryInfo = static_cast<decltype (pQueryInfo)> (
        g_malloc (sizeof (gnc_sql_query_info)));
    g_assert (pQueryInfo != NULL);
    pQueryInfo->pCompiledQuery = NULL;
    pQueryInfo->searchObj = searchObj;

    // Try various objects first
    be_data.is_ok = FALSE;
    be_data.be = sql_be;
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
gnc_sql_compile_query_to_sql (GncSqlBackend* sql_be, QofQuery* query)
{
    QofIdType searchObj;
    GString* sql;

    g_return_val_if_fail (sql_be != NULL, NULL);
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
gnc_sql_free_query (QofBackend* qof_be, gpointer pQuery)
{
    GncSqlBackend* sql_be = reinterpret_cast<decltype(sql_be)>(qof_be);
    gnc_sql_query_info* pQueryInfo = (gnc_sql_query_info*)pQuery;
    sql_backend be_data;

    g_return_if_fail (sql_be != NULL);
    g_return_if_fail (pQuery != NULL);

    ENTER (" ");

    // Try various objects first
    be_data.is_ok = FALSE;
    be_data.be = sql_be;
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
gnc_sql_run_query (QofBackend* qof_be, gpointer pQuery)
{
    GncSqlBackend* sql_be = reinterpret_cast<decltype(sql_be)>(qof_be);
    gnc_sql_query_info* pQueryInfo = (gnc_sql_query_info*)pQuery;
    sql_backend be_data;

    g_return_if_fail (sql_be != NULL);
    g_return_if_fail (pQuery != NULL);
    g_return_if_fail (!sql_be->in_query);

    ENTER (" ");

    sql_be->loading = TRUE;
    sql_be->in_query = TRUE;

    qof_event_suspend ();

    // Try various objects first
    be_data.is_ok = FALSE;
    be_data.be = sql_be;
    be_data.pCompiledQuery = pQueryInfo->pCompiledQuery;
    be_data.pQueryInfo = pQueryInfo;
    for (auto entry : backend_registry)
        run_query(entry, &be_data);
    sql_be->loading = FALSE;
    sql_be->in_query = FALSE;
    qof_event_resume ();
//    if( be_data.is_ok ) {
//        LEAVE( "" );
//        return;
//    }

    // Mark the book as clean
    qof_instance_mark_clean (QOF_INSTANCE (sql_be->book()));

//    DEBUG( "%s\n", (gchar*)pQueryInfo->pCompiledQuery );

    LEAVE ("");
}
#endif //if 0: query creation isn't used yet, code never tested.

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
gnc_sql_load_guid (const GncSqlBackend* sql_be, GncSqlRow& row)
{
    static GncGUID guid;

    g_return_val_if_fail (sql_be != NULL, NULL);

    gnc_sql_load_object (sql_be, row, NULL, &guid, guid_table);

    return &guid;
}

// Table to retrieve just the guid
static EntryVec tx_guid_table
{
    gnc_sql_make_table_entry<CT_GUID>("tx_guid", 0, 0, nullptr, _retrieve_guid_)
 };

void
gnc_sql_load_object (const GncSqlBackend* sql_be, GncSqlRow& row,
                     QofIdTypeConst obj_name, gpointer pObject,
                     const EntryVec& table)
{
    QofSetterFunc setter;

    g_return_if_fail (sql_be != NULL);
    g_return_if_fail (pObject != NULL);

    for (auto const& table_row : table)
    {
        table_row->load (sql_be, row, obj_name, pObject);
    }
}

/* ================================================================= */
static GncSqlStatementPtr
create_single_col_select_statement (GncSqlBackend* sql_be,
                                    const gchar* table_name,
                                    const GncSqlColumnTableEntryPtr table_row)
{
    g_return_val_if_fail (sql_be != NULL, NULL);
    g_return_val_if_fail (table_name != NULL, NULL);

    auto sql = std::string{"SELECT "} + table_row->name() + " FROM " + table_name;
    return sql_be->create_statement_from_sql(sql.c_str());
}

/* ================================================================= */

uint_t
gnc_sql_append_guids_to_sql (std::stringstream& sql, const InstanceVec& instances)
{
    char guid_buf[GUID_ENCODING_LENGTH + 1];

    for (auto inst : instances)
    {
        (void)guid_to_string_buff (qof_instance_get_guid (inst), guid_buf);

        if (inst != *(instances.begin()))
        {
            sql << ",";
        }
        sql << "'" << guid_buf << "'";
    }

    return instances.size();
}
/* ================================================================= */
static PairVec
get_object_values (GncSqlBackend* sql_be, QofIdTypeConst obj_name,
                   gpointer pObject, const EntryVec& table)
{
    PairVec vec;

    for (auto const& table_row : table)
    {
        if (!(table_row->is_autoincr()))
        {
            table_row->add_to_query (sql_be, obj_name, pObject, vec);
        }
    }
    return vec;
}

gboolean
gnc_sql_object_is_it_in_db (GncSqlBackend* sql_be, const gchar* table_name,
                            QofIdTypeConst obj_name, gpointer pObject,
                            const EntryVec& table)
{
    guint count;
    g_return_val_if_fail (sql_be != NULL, FALSE);
    g_return_val_if_fail (table_name != NULL, FALSE);
    g_return_val_if_fail (obj_name != NULL, FALSE);
    g_return_val_if_fail (pObject != NULL, FALSE);

    /* SELECT * FROM */
    auto stmt = create_single_col_select_statement (sql_be, table_name, table[0]);
    g_assert (stmt != NULL);

    /* WHERE */
    PairVec values{get_object_values(sql_be, obj_name, pObject, table)};
    stmt->add_where_cond(obj_name, values);
    auto result = sql_be->execute_select_statement (stmt);
    if (result != NULL)
    {
        auto retval = result->size() > 0;
        return retval;
    }
    return false;
}

gboolean
gnc_sql_do_db_operation (GncSqlBackend* sql_be,
                         E_DB_OPERATION op,
                         const gchar* table_name,
                         QofIdTypeConst obj_name, gpointer pObject,
                         const EntryVec& table)
{
    GncSqlStatementPtr stmt;
    bool ok = false;

    g_return_val_if_fail (sql_be != NULL, FALSE);
    g_return_val_if_fail (table_name != NULL, FALSE);
    g_return_val_if_fail (obj_name != NULL, FALSE);
    g_return_val_if_fail (pObject != NULL, FALSE);

    if (op == OP_DB_INSERT)
    {
        stmt = build_insert_statement (sql_be, table_name, obj_name, pObject, table);
    }
    else if (op == OP_DB_UPDATE)
    {
        stmt = build_update_statement (sql_be, table_name, obj_name, pObject, table);
    }
    else if (op == OP_DB_DELETE)
    {
        stmt = build_delete_statement (sql_be, table_name, obj_name, pObject, table);
    }
    else
    {
        g_assert (FALSE);
    }
    if (sql_be->execute_nonselect_statement (stmt) != -1)
        ok = true;

    return ok;
}

static GncSqlStatementPtr
build_insert_statement (GncSqlBackend* sql_be,
                        const gchar* table_name,
                        QofIdTypeConst obj_name, gpointer pObject,
                        const EntryVec& table)
{
    GncSqlStatementPtr stmt;
    PairVec col_values;
    std::ostringstream sql;

    g_return_val_if_fail (sql_be != NULL, NULL);
    g_return_val_if_fail (table_name != NULL, NULL);
    g_return_val_if_fail (obj_name != NULL, NULL);
    g_return_val_if_fail (pObject != NULL, NULL);
    PairVec values{get_object_values(sql_be, obj_name, pObject, table)};

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
        sql << sql_be->quote_string(col_value.second);
    }
    sql << ")";

    stmt = sql_be->create_statement_from_sql(sql.str());
    return stmt;
}

static GncSqlStatementPtr
build_update_statement (GncSqlBackend* sql_be,
                        const gchar* table_name,
                        QofIdTypeConst obj_name, gpointer pObject,
                        const EntryVec& table)
{
    GncSqlStatementPtr stmt;
    std::ostringstream sql;

    g_return_val_if_fail (sql_be != NULL, NULL);
    g_return_val_if_fail (table_name != NULL, NULL);
    g_return_val_if_fail (obj_name != NULL, NULL);
    g_return_val_if_fail (pObject != NULL, NULL);


    PairVec values{get_object_values (sql_be, obj_name, pObject, table)};

    // Create the SQL statement
    sql <<  "UPDATE " << table_name << " SET ";

    for (auto const& col_value : values)
    {
        if (col_value != *values.begin())
            sql << ",";
        sql << col_value.first << "=" <<
            sql_be->quote_string(col_value.second);
    }

    stmt = sql_be->create_statement_from_sql(sql.str());
    /* We want our where condition to be just the first column and
     * value, i.e. the guid of the object.
     */
    values.erase(values.begin() + 1, values.end());
    stmt->add_where_cond(obj_name, values);
    return stmt;
}

static GncSqlStatementPtr
build_delete_statement (GncSqlBackend* sql_be,
                        const gchar* table_name,
                        QofIdTypeConst obj_name, gpointer pObject,
                        const EntryVec& table)
{
    std::ostringstream sql;

    g_return_val_if_fail (sql_be != NULL, NULL);
    g_return_val_if_fail (table_name != NULL, NULL);
    g_return_val_if_fail (obj_name != NULL, NULL);
    g_return_val_if_fail (pObject != NULL, NULL);

    sql << "DELETE FROM " << table_name;
    auto stmt = sql_be->create_statement_from_sql (sql.str());

    /* WHERE */
    PairVec values;
    table[0]->add_to_query (sql_be, obj_name, pObject, values);
    PairVec col_values{values[0]};
    stmt->add_where_cond (obj_name, col_values);

    return stmt;
}

/* ========================== END OF FILE ===================== */
