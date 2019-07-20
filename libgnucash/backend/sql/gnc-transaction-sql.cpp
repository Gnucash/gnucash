/********************************************************************
 * gnc-transaction-sql.c: load and save data to SQL                 *
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
/** @file gnc-transaction-sql.c
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */
#include <guid.hpp>
extern "C"
{
#include <config.h>

#include <glib/gi18n.h>

#include "qof.h"
#include "qofquery-p.h"
#include "qofquerycore-p.h"

#include "Account.h"
#include "Transaction.h"
#include <Scrub.h>
#include "gnc-lot.h"
#include "engine-helpers.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"

#ifdef S_SPLINT_S
#include "splint-defs.h"
#endif
}

#include <string>
#include <sstream>

#include "escape.h"

#include <gnc-datetime.hpp>
#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-transaction-sql.h"
#include "gnc-commodity-sql.h"
#include "gnc-slots-sql.h"

#define SIMPLE_QUERY_COMPILATION 1

static QofLogModule log_module = G_LOG_DOMAIN;

#define TRANSACTION_TABLE "transactions"
#define TX_TABLE_VERSION 4
#define SPLIT_TABLE "splits"
#define SPLIT_TABLE_VERSION 5

struct split_info_t : public write_objects_t
{
    split_info_t () = default;
    split_info_t (GncSqlBackend* sql_be, bool o,
                  GncSqlObjectBackend* e, const GncGUID* g):
        write_objects_t(sql_be, o, e), guid{g} {}
    const GncGUID* guid;
};

#define TX_MAX_NUM_LEN 2048
#define TX_MAX_DESCRIPTION_LEN 2048

static const EntryVec tx_col_table
{
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid"),
    gnc_sql_make_table_entry<CT_COMMODITYREF>("currency_guid", 0, COL_NNUL,
                                              "currency"),
    gnc_sql_make_table_entry<CT_STRING>("num", TX_MAX_NUM_LEN, COL_NNUL, "num"),
    gnc_sql_make_table_entry<CT_TIME>("post_date", 0, 0, "post-date"),
    gnc_sql_make_table_entry<CT_TIME>("enter_date", 0, 0, "enter-date"),
    gnc_sql_make_table_entry<CT_STRING>("description", TX_MAX_DESCRIPTION_LEN,
                                        0, "description"),
};

static  gpointer get_split_reconcile_state (gpointer pObject);
static void set_split_reconcile_state (gpointer pObject,  gpointer pValue);
static void set_split_lot (gpointer pObject,  gpointer pLot);

#define SPLIT_MAX_MEMO_LEN 2048
#define SPLIT_MAX_ACTION_LEN 2048

static const EntryVec split_col_table
{
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid"),
    gnc_sql_make_table_entry<CT_TXREF>("tx_guid", 0, COL_NNUL, "transaction"),
    gnc_sql_make_table_entry<CT_ACCOUNTREF>("account_guid", 0, COL_NNUL,
                                            "account"),
    gnc_sql_make_table_entry<CT_STRING>("memo", SPLIT_MAX_MEMO_LEN, COL_NNUL,
                                        "memo"),
    gnc_sql_make_table_entry<CT_STRING>("action", SPLIT_MAX_ACTION_LEN,
                                        COL_NNUL, "action"),
    gnc_sql_make_table_entry<CT_STRING>("reconcile_state", 1, COL_NNUL,
                                       (QofAccessFunc)get_split_reconcile_state,
                                        set_split_reconcile_state),
    gnc_sql_make_table_entry<CT_TIME>("reconcile_date", 0, 0,
                                          "reconcile-date"),
    gnc_sql_make_table_entry<CT_NUMERIC>("value", 0, COL_NNUL, "value"),
    gnc_sql_make_table_entry<CT_NUMERIC>("quantity", 0, COL_NNUL, "amount"),
    gnc_sql_make_table_entry<CT_LOTREF>("lot_guid", 0, 0,
                                        (QofAccessFunc)xaccSplitGetLot,
                                        set_split_lot),
};

static const EntryVec post_date_col_table
{
    gnc_sql_make_table_entry<CT_TIME>("post_date", 0, 0, "post-date"),
};

static const EntryVec account_guid_col_table
{
    gnc_sql_make_table_entry<CT_ACCOUNTREF>("account_guid", 0, COL_NNUL,
                                            "account"),
};

static const EntryVec tx_guid_col_table
{
    gnc_sql_make_table_entry<CT_GUID>("tx_guid", 0, 0, "guid"),
};

GncSqlTransBackend::GncSqlTransBackend() :
    GncSqlObjectBackend(TX_TABLE_VERSION, GNC_ID_TRANS,
                        TRANSACTION_TABLE, tx_col_table) {}

GncSqlSplitBackend::GncSqlSplitBackend() :
    GncSqlObjectBackend(SPLIT_TABLE_VERSION, GNC_ID_SPLIT,
                        SPLIT_TABLE, split_col_table) {}

/* These functions exist but have not been tested.
   #if LOAD_TRANSACTIONS_AS_NEEDED
   compile_split_query,
   run_split_query,
   free_split_query,
*/

/* ================================================================= */

static  gpointer
get_split_reconcile_state (gpointer pObject)
{
    static gchar c[2];

    g_return_val_if_fail (pObject != NULL, NULL);
    g_return_val_if_fail (GNC_IS_SPLIT (pObject), NULL);

    c[0] = xaccSplitGetReconcile (GNC_SPLIT (pObject));
    c[1] = '\0';
    return (gpointer)c;
}

static void
set_split_reconcile_state (gpointer pObject,  gpointer pValue)
{
    const gchar* s = (const gchar*)pValue;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (GNC_IS_SPLIT (pObject));
    g_return_if_fail (pValue != NULL);

    xaccSplitSetReconcile (GNC_SPLIT (pObject), s[0]);
}

static void
set_split_lot (gpointer pObject,  gpointer pLot)
{
    GNCLot* lot;
    Split* split;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (GNC_IS_SPLIT (pObject));

    if (pLot == NULL) return;

    g_return_if_fail (GNC_IS_LOT (pLot));

    split = GNC_SPLIT (pObject);
    lot = GNC_LOT (pLot);
    gnc_lot_add_split (lot, split);
}

static  Split*
load_single_split (GncSqlBackend* sql_be, GncSqlRow& row)
{
    const GncGUID* guid;
    GncGUID split_guid;
    Split* pSplit = NULL;
    gboolean bad_guid = FALSE;

    g_return_val_if_fail (sql_be != NULL, NULL);

    guid = gnc_sql_load_guid (sql_be, row);
    if (guid == NULL) return NULL;
    if (guid_equal (guid, guid_null ()))
    {
        PWARN ("Bad GUID, creating new");
        split_guid = guid_new_return ();
    }
    else
    {
        split_guid = *guid;
        pSplit = xaccSplitLookup (&split_guid, sql_be->book());
    }

    if (pSplit)
        return pSplit; //Already loaded, nothing to do.

    pSplit = xaccMallocSplit (sql_be->book());
    gnc_sql_load_object (sql_be, row, GNC_ID_SPLIT, pSplit, split_col_table);

    /*# -ifempty */
    if (pSplit != xaccSplitLookup (&split_guid, sql_be->book()))
    {
        gchar guidstr[GUID_ENCODING_LENGTH + 1];
        guid_to_string_buff (qof_instance_get_guid (pSplit), guidstr);
        PERR ("A malformed split with id %s was found in the dataset.", guidstr);
        qof_backend_set_error ((QofBackend*)sql_be, ERR_BACKEND_DATA_CORRUPT);
        pSplit = NULL;
    }
    return pSplit;
}
static void
load_splits_for_transactions (GncSqlBackend* sql_be, std::string selector)
{
    g_return_if_fail (sql_be != NULL);

    const std::string spkey(split_col_table[0]->name());
    const std::string sskey(tx_guid_col_table[0]->name());
    const std::string tpkey(tx_col_table[0]->name());

    std::string sql("SELECT ");
    if (selector.empty())
    {
        sql += SPLIT_TABLE ".* FROM " SPLIT_TABLE " INNER JOIN "
            TRANSACTION_TABLE " ON " SPLIT_TABLE "." + sskey + " = "
            TRANSACTION_TABLE "." + tpkey;
        selector = "(SELECT DISTINCT " + tpkey + " FROM " TRANSACTION_TABLE ")";
    }
    else
        sql += " * FROM " SPLIT_TABLE " WHERE " + sskey + " IN " + selector;

    // Execute the query and load the splits
    auto stmt = sql_be->create_statement_from_sql(sql);
    auto result = sql_be->execute_select_statement (stmt);

    for (auto row : *result)
        load_single_split (sql_be, row);
    sql = "SELECT DISTINCT ";
    sql += spkey + " FROM " SPLIT_TABLE " WHERE " + sskey + " IN " + selector;
    gnc_sql_slots_load_for_sql_subquery(sql_be, sql,
                                        (BookLookupFn)xaccSplitLookup);
}

static  Transaction*
load_single_tx (GncSqlBackend* sql_be, GncSqlRow& row)
{
    const GncGUID* guid;
    GncGUID tx_guid;
    Transaction* pTx;

    g_return_val_if_fail (sql_be != NULL, NULL);

    guid = gnc_sql_load_guid (sql_be, row);
    if (guid == NULL) return NULL;
    tx_guid = *guid;

    pTx = xaccTransLookup (&tx_guid, sql_be->book());
    if (pTx)
        return nullptr; // Nothing to do. 

    pTx = xaccMallocTransaction (sql_be->book());
    xaccTransBeginEdit (pTx);
    gnc_sql_load_object (sql_be, row, GNC_ID_TRANS, pTx, tx_col_table);

    if (pTx != xaccTransLookup (&tx_guid, sql_be->book()))
    {
        gchar guidstr[GUID_ENCODING_LENGTH + 1];
        guid_to_string_buff (qof_instance_get_guid (pTx), guidstr);
        PERR ("A malformed transaction with id %s was found in the dataset.", guidstr);
        qof_backend_set_error ((QofBackend*)sql_be, ERR_BACKEND_DATA_CORRUPT);
        pTx = NULL;
    }

    return pTx;
}

/**
 * Structure to hold start/end balances for each account.  The values are
 * saved before splits are loaded, and then used to adjust the start balances
 * so that the end balances (which are calculated and correct on initial load)
 * are unchanged.
 */
typedef struct
{
    Account* acc;
    gnc_numeric start_bal;
    gnc_numeric end_bal;
    gnc_numeric start_cleared_bal;
    gnc_numeric end_cleared_bal;
    gnc_numeric start_reconciled_bal;
    gnc_numeric end_reconciled_bal;
} full_acct_balances_t;

/**
 * Executes a transaction query statement and loads the transactions and all
 * of the splits.
 *
 * @param sql_be SQL backend
 * @param stmt SQL statement
 */
static void
query_transactions (GncSqlBackend* sql_be, std::string selector)
{
    g_return_if_fail (sql_be != NULL);

    const std::string tpkey(tx_col_table[0]->name());
    std::string sql("SELECT * FROM " TRANSACTION_TABLE);

    if (!selector.empty() && selector[0] == '(')
        sql += " WHERE " + tpkey + " IN " + selector;
    else if (!selector.empty()) // plain condition
        sql += " WHERE " + selector;
    auto stmt = sql_be->create_statement_from_sql(sql);
    auto result = sql_be->execute_select_statement(stmt);
    if (result->begin() == result->end())
    {
        PINFO("Query %s returned no results", sql.c_str());
        return;
    }

    Transaction* tx;

    // Load the transactions
    InstanceVec instances;
    instances.reserve(result->size());
    for (auto row : *result)
    {
        tx = load_single_tx (sql_be, row);
        if (tx != nullptr)
        {
            xaccTransScrubPostedDate (tx);
            instances.push_back(QOF_INSTANCE(tx));
        }
    }

    // Load all splits and slots for the transactions
    if (!instances.empty())
    {
        const std::string tpkey(tx_col_table[0]->name());
        if (!selector.empty() && (selector[0] != '('))
        {
            auto tselector = std::string ("(SELECT DISTINCT ");
            tselector += tpkey + " FROM " TRANSACTION_TABLE " WHERE " + selector + ")";
            selector = tselector;
        }

        load_splits_for_transactions (sql_be, selector);

        if (selector.empty())
        {
            selector = "SELECT DISTINCT ";
            selector += tpkey + " FROM " TRANSACTION_TABLE;
        }
        gnc_sql_slots_load_for_sql_subquery (sql_be, selector,
					     (BookLookupFn)xaccTransLookup);
    }

    // Commit all of the transactions
    for (auto instance : instances)
         xaccTransCommitEdit(GNC_TRANSACTION(instance));

}


/* ================================================================= */
/**
 * Creates the transaction and split tables.
 *
 * @param sql_be SQL backend
 */
void
GncSqlTransBackend::create_tables (GncSqlBackend* sql_be)
{
    gint version;
    gboolean ok;

    g_return_if_fail (sql_be != NULL);

    version = sql_be->get_table_version( m_table_name.c_str());
    if (version == 0)
    {
        (void)sql_be->create_table(TRANSACTION_TABLE, TX_TABLE_VERSION,
                                    tx_col_table);
        ok = sql_be->create_index ("tx_post_date_index", TRANSACTION_TABLE,
                                   post_date_col_table);
        if (!ok)
        {
            PERR ("Unable to create index\n");
        }
    }
    else if (version < m_version)
    {
        /* Upgrade:
            1->2: 64 bit int handling
            2->3: allow dates to be NULL
            3->4: Use DATETIME instead of TIMESTAMP in MySQL
        */
        sql_be->upgrade_table(m_table_name.c_str(), tx_col_table);
        sql_be->set_table_version (m_table_name.c_str(), m_version);
        PINFO ("Transactions table upgraded from version %d to version %d\n",
               version, m_version);
    }
}
void
GncSqlSplitBackend::create_tables (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != nullptr);

    auto version = sql_be->get_table_version( m_table_name.c_str());
    if (version == 0)
    {
        (void)sql_be->create_table(m_table_name.c_str(),
                                    m_version, m_col_table);
        if (!sql_be->create_index("splits_tx_guid_index",
                                   m_table_name.c_str(), tx_guid_col_table))
            PERR ("Unable to create index\n");
        if (!sql_be->create_index("splits_account_guid_index",
                                   m_table_name.c_str(),
                                   account_guid_col_table))
            PERR ("Unable to create index\n");
    }
    else if (version < SPLIT_TABLE_VERSION)
    {

        /* Upgrade:
           1->2: 64 bit int handling
           3->4: Split reconcile date can be NULL
           4->5: Use DATETIME instead of TIMESTAMP in MySQL
        */
        sql_be->upgrade_table(m_table_name.c_str(), split_col_table);
        if (!sql_be->create_index("splits_tx_guid_index",
                                   m_table_name.c_str(),
                                   tx_guid_col_table))
            PERR ("Unable to create index\n");
        if (!sql_be->create_index("splits_account_guid_index",
                                   m_table_name.c_str(),
                                   account_guid_col_table))
            PERR ("Unable to create index\n");
        sql_be->set_table_version (m_table_name.c_str(), m_version);
        PINFO ("Splits table upgraded from version %d to version %d\n", version,
               m_version);
    }
}
/* ================================================================= */
/**
 * Callback function to delete slots for a split
 *
 * @param data Split
 * @param user_data split_info_t structure contain operation info
 */
static void
delete_split_slots_cb (gpointer data, gpointer user_data)
{
    split_info_t* split_info = (split_info_t*)user_data;
    Split* pSplit = GNC_SPLIT (data);

    g_return_if_fail (data != NULL);
    g_return_if_fail (GNC_IS_SPLIT (data));
    g_return_if_fail (user_data != NULL);

    if (split_info->is_ok)
    {
        split_info->is_ok = gnc_sql_slots_delete (split_info->be,
                                                  qof_instance_get_guid (QOF_INSTANCE (pSplit)));
    }
}

/**
 * Deletes all of the splits for a transaction
 *
 * @param sql_be SQL backend
 * @param pTx Transaction
 * @return TRUE if successful, FALSE if unsuccessful
 */
static gboolean
delete_splits (GncSqlBackend* sql_be, Transaction* pTx)
{
    split_info_t split_info;

    g_return_val_if_fail (sql_be != NULL, FALSE);
    g_return_val_if_fail (pTx != NULL, FALSE);

    if (!sql_be->do_db_operation(OP_DB_DELETE, SPLIT_TABLE,
                                 SPLIT_TABLE, pTx, tx_guid_col_table))
    {
        return FALSE;
    }
    split_info.be = sql_be;
    split_info.is_ok = TRUE;

    g_list_foreach (xaccTransGetSplitList (pTx), delete_split_slots_cb,
                    &split_info);

    return split_info.is_ok;
}

/**
 * Commits a split to the database
 *
 * @param sql_be SQL backend
 * @param inst Split
 * @return TRUE if successful, FALSE if error
 */
bool
GncSqlSplitBackend::commit (GncSqlBackend* sql_be, QofInstance* inst)
{
    E_DB_OPERATION op;
    gboolean is_infant;
    gboolean is_ok;
    GncGUID* guid = (GncGUID*)qof_instance_get_guid (inst);

    g_return_val_if_fail (inst != NULL, FALSE);
    g_return_val_if_fail (sql_be != NULL, FALSE);

    is_infant = qof_instance_get_infant (inst);
    if (qof_instance_get_destroying (inst))
    {
        op = OP_DB_DELETE;
    }
    else if (sql_be->pristine() || is_infant)
    {
        op = OP_DB_INSERT;
    }
    else
    {
        op = OP_DB_UPDATE;
    }

    if (guid_equal (guid, guid_null ()))
    {
        *guid = guid_new_return ();
        qof_instance_set_guid (inst, guid);
    }

    is_ok = sql_be->do_db_operation(op, SPLIT_TABLE, GNC_ID_SPLIT,
                                    inst, split_col_table);

    if (is_ok && !qof_instance_get_destroying (inst))
    {
        is_ok = gnc_sql_slots_save (sql_be, guid, is_infant, inst);
    }

    return is_ok;
}


bool
GncSqlTransBackend::commit (GncSqlBackend* sql_be, QofInstance* inst)
{
    E_DB_OPERATION op;
    gboolean is_ok = TRUE;
    const char* err = NULL;

    g_return_val_if_fail (sql_be != NULL, FALSE);
    g_return_val_if_fail (inst != NULL, FALSE);

    auto pTx = GNC_TRANS(inst);
    auto is_infant = qof_instance_get_infant (inst);
    if (qof_instance_get_destroying (inst))
    {
        op = OP_DB_DELETE;
    }
    else if (sql_be->pristine() || is_infant)
    {
        op = OP_DB_INSERT;
    }
    else
    {
        op = OP_DB_UPDATE;
    }

    if (op != OP_DB_DELETE)
    {
        gnc_commodity* commodity = xaccTransGetCurrency (pTx);
        // Ensure the commodity is in the db
        is_ok = sql_be->save_commodity(commodity);
        if (! is_ok)
        {
            err = "Commodity save failed: Probably an invalid or missing currency";
            qof_backend_set_error ((QofBackend*)sql_be, ERR_BACKEND_DATA_CORRUPT);
        }
    }

    if (is_ok)
    {
        is_ok = sql_be->do_db_operation(op, TRANSACTION_TABLE, GNC_ID_TRANS,
                                        pTx, tx_col_table);
        if (! is_ok)
        {
            err = "Transaction header save failed. Check trace log for SQL errors";
        }
    }

    if (is_ok)
    {
        // Commit slots
        auto guid = qof_instance_get_guid (inst);
        if (!qof_instance_get_destroying (inst))
        {
            is_ok = gnc_sql_slots_save (sql_be, guid, is_infant, inst);
            if (! is_ok)
            {
                err = "Slots save failed. Check trace log for SQL errors";
            }
        }
        else
        {
            is_ok = gnc_sql_slots_delete (sql_be, guid);
            if (! is_ok)
            {
                err = "Slots delete failed. Check trace log for SQL errors";
            }
            if (is_ok)
            {
                is_ok = delete_splits (sql_be, pTx);
                if (! is_ok)
                {
                    err = "Split delete failed. Check trace log for SQL errors";
                }
            }
        }
    }
    if (! is_ok)
    {
        Split* split = xaccTransGetSplit (pTx, 0);
        Account* acc = xaccSplitGetAccount (split);
        /* FIXME: This needs to be implemented
        const char *message1 = "Transaction %s dated %s in account %s not saved due to %s.%s";
        const char *message2 = "\nDatabase may be corrupted, check your data carefully.";
        qof_error_format_secondary_text( GTK_MESSAGE_DIALOG( msg ),
                              message1,
                             xaccTransGetDescription( pTx ),
                              qof_print_date( xaccTransGetDate( pTx ) ),
                              xaccAccountGetName( acc ),
                              err,
                              message2 );
        */
        PERR ("Transaction %s dated %s in account %s not saved due to %s.\n",
              xaccTransGetDescription (pTx),
              qof_print_date (xaccTransGetDate (pTx)),
              xaccAccountGetName (acc),
              err);
    }
    return is_ok;
}

/* ================================================================= */
/**
 * Loads all transactions for an account.
 *
 * @param sql_be SQL backend
 * @param account Account
 */
void gnc_sql_transaction_load_tx_for_account (GncSqlBackend* sql_be,
                                              Account* account)
{
    const GncGUID* guid;
    gchar guid_buf[GUID_ENCODING_LENGTH + 1];
    gchar* query_sql;

    g_return_if_fail (sql_be != NULL);
    g_return_if_fail (account != NULL);

    guid = qof_instance_get_guid (QOF_INSTANCE (account));

    const std::string tpkey(tx_col_table[0]->name());    //guid
    const std::string spkey(split_col_table[0]->name()); //guid
    const std::string stkey(split_col_table[1]->name()); //txn_guid
    const std::string sakey(split_col_table[2]->name()); //account_guid
    std::string sql("(SELECT DISTINCT ");
    sql += stkey + " FROM " SPLIT_TABLE " WHERE " + sakey + " = '";
    sql += gnc::GUID(*guid).to_string() + "')";
    query_transactions (sql_be, sql);
}

/**
 * Loads all transactions.  This might be used during a save-as operation to ensure that
 * all data is in memory and ready to be saved.
 *
 * @param sql_be SQL backend
 */
void
GncSqlTransBackend::load_all (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != NULL);

    auto root = gnc_book_get_root_account (sql_be->book());
    gnc_account_foreach_descendant(root, (AccountCb)xaccAccountBeginEdit,
                                   nullptr);
    query_transactions (sql_be, "");
    gnc_account_foreach_descendant(root, (AccountCb)xaccAccountCommitEdit,
                                   nullptr);
}

static void
convert_query_comparison_to_sql (QofQueryPredData* pPredData,
                                 gboolean isInverted, std::stringstream& sql)
{
    if (pPredData->how == QOF_COMPARE_LT
        || (isInverted && pPredData->how == QOF_COMPARE_GTE))
        sql << "<";
    else if (pPredData->how == QOF_COMPARE_LTE
             || (isInverted && pPredData->how == QOF_COMPARE_GT))
        sql << "<=";
    else if (pPredData->how == QOF_COMPARE_EQUAL
             || (isInverted && pPredData->how == QOF_COMPARE_NEQ))
        sql << "=";
    else if (pPredData->how == QOF_COMPARE_GT
             || (isInverted && pPredData->how == QOF_COMPARE_LTE))
        sql << ">";
    else if (pPredData->how == QOF_COMPARE_GTE
             || (isInverted && pPredData->how == QOF_COMPARE_LT))
        sql << ">=";
    else if (pPredData->how == QOF_COMPARE_NEQ
             || (isInverted && pPredData->how == QOF_COMPARE_EQUAL))
        sql <<  "~=";
    else
    {
        PERR ("Unknown comparison type\n");
        sql << "??";
    }
}

static void
convert_query_term_to_sql (const GncSqlBackend* sql_be, const gchar* fieldName,
                           QofQueryTerm* pTerm, std::stringstream& sql)
{
    QofQueryPredData* pPredData;
    gboolean isInverted;

    g_return_if_fail (pTerm != NULL);

    pPredData = qof_query_term_get_pred_data (pTerm);
    isInverted = qof_query_term_is_inverted (pTerm);

    if (g_strcmp0 (pPredData->type_name, QOF_TYPE_GUID) == 0)
    {
        query_guid_t guid_data = (query_guid_t)pPredData;
        GList* guid_entry;
        sql << "(" << fieldName;

        switch (guid_data->options)
        {
        case QOF_GUID_MATCH_ANY:
            sql << (isInverted ? " NOT IN (" : " IN (");
            break;

        case QOF_GUID_MATCH_NONE:
            sql << (isInverted ? " IN (" : " NOT IN (");
            break;

        default:
            PERR ("Unexpected GncGUID match type: %d\n", guid_data->options);
        }

        for (guid_entry = guid_data->guids; guid_entry != NULL;
             guid_entry = guid_entry->next)
        {
            gchar guid_buf[GUID_ENCODING_LENGTH + 1];

            if (guid_entry != guid_data->guids) sql << ",";
            (void)guid_to_string_buff (static_cast<GncGUID*> (guid_entry->data),
                                       guid_buf);
            sql << guid_buf;
        }
        sql << "))";

    }
    else if (g_strcmp0 (pPredData->type_name, QOF_TYPE_CHAR) == 0)
    {
        query_char_t char_data = (query_char_t)pPredData;
        int i;

        if (isInverted) sql <<  "NOT(";
        if (char_data->options == QOF_CHAR_MATCH_NONE) sql << "NOT ";
        sql << "(";
        for (i = 0; char_data->char_list[i] != '\0'; i++)
        {
            if (i != 0) sql << " OR ";
            sql << fieldName << " = '" << char_data->char_list[i] << "'";
        }
        sql << ") ";
        if (isInverted) sql << ") ";
    }
    else if (g_strcmp0 (pPredData->type_name, QOF_TYPE_STRING) == 0)
    {
        query_string_t string_data = (query_string_t)pPredData;
        sqlEscape* escape = sqlEscape_new ();

        if (isInverted ||  pPredData->how == QOF_COMPARE_NEQ)
            sql << "NOT(";
        sql << fieldName;
        if (string_data->is_regex ||
            string_data->options == QOF_STRING_MATCH_CASEINSENSITIVE)
        {
            PWARN ("String is_regex || option = QOF_STRING_MATCH_INSENSITIVE\n");
        }
//          sql << " ~" ;
//      } else {
        sql << " =";
//      }
//      if( string_data->options == QOF_STRING_MATCH_CASEINSENSITIVE ) {
//          sql+= "*";
//      }
        sql << "'" << sqlEscapeString (escape, string_data->matchstring) << "'";
        if (pPredData->how == QOF_COMPARE_NEQ) sql << ")";
        if (isInverted) sql << ")";
        sqlEscape_destroy (escape);

    }
    else
    {
        sql << "(" << fieldName;
        convert_query_comparison_to_sql (pPredData, isInverted, sql);

        if (strcmp (pPredData->type_name, QOF_TYPE_NUMERIC) == 0)
        {
            query_numeric_t pData = (query_numeric_t)pPredData;
            sql << gnc_numeric_to_double (pData->amount);
        }
        else if (g_strcmp0 (pPredData->type_name, QOF_TYPE_DATE) == 0)
        {
            query_date_t date_data = (query_date_t)pPredData;

            GncDateTime time(date_data->date);
            sql << time.format_iso8601();
        }
        else if (strcmp (pPredData->type_name, QOF_TYPE_INT32) == 0)
        {
            query_int32_t pData = (query_int32_t)pPredData;
            sql << pData->val;
        }
        else if (strcmp (pPredData->type_name, QOF_TYPE_INT64) == 0)
        {
            query_int64_t pData = (query_int64_t)pPredData;
            sql << pData->val;
        }
        else if (strcmp (pPredData->type_name, QOF_TYPE_DOUBLE) == 0)
        {
            query_double_t pData = (query_double_t)pPredData;
            sql << pData->val;
        }
        else if (strcmp (pPredData->type_name, QOF_TYPE_BOOLEAN) == 0)
        {
            query_boolean_t pData = (query_boolean_t)pPredData;
            sql << pData->val;
        }
        else
        {
            PERR ("Unknown query predicate type: %s\n", pPredData->type_name);
        }

        sql << ")";
    }
}

typedef struct
{
    GncSqlStatementPtr stmt;
    gboolean has_been_run;
} split_query_info_t;

/* ----------------------------------------------------------------- */
typedef struct
{
    const GncSqlBackend* sql_be;
    Account* acct;
    char reconcile_state;
    gnc_numeric balance;
} single_acct_balance_t;

static void
set_acct_bal_account_from_guid (gpointer pObject, gpointer pValue)
{
    single_acct_balance_t* bal = (single_acct_balance_t*)pObject;
    const GncGUID* guid = (const GncGUID*)pValue;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (pValue != NULL);

    bal->acct = xaccAccountLookup (guid, bal->sql_be->book());
}

static void
set_acct_bal_reconcile_state (gpointer pObject, gpointer pValue)
{
    single_acct_balance_t* bal = (single_acct_balance_t*)pObject;
    const gchar* s = (const gchar*)pValue;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (pValue != NULL);

    bal->reconcile_state = s[0];
}

static void
set_acct_bal_balance (gpointer pObject, gnc_numeric value)
{
    single_acct_balance_t* bal = (single_acct_balance_t*)pObject;

    g_return_if_fail (pObject != NULL);

    bal->balance = value;
}

static const EntryVec acct_balances_col_table
{
    gnc_sql_make_table_entry<CT_GUID>("account_guid", 0, 0, nullptr,
                                (QofSetterFunc)set_acct_bal_account_from_guid),
    gnc_sql_make_table_entry<CT_STRING>("reconcile_state", 1, 0, nullptr,
                                (QofSetterFunc)set_acct_bal_reconcile_state),
    gnc_sql_make_table_entry<CT_NUMERIC>("quantity", 0, 0, nullptr,
                                         (QofSetterFunc)set_acct_bal_balance),
};

/* ----------------------------------------------------------------- */
template<> void
GncSqlColumnTableEntryImpl<CT_TXREF>::load (const GncSqlBackend* sql_be,
                                            GncSqlRow& row,
                                            QofIdTypeConst obj_name,
                                            gpointer pObject) const noexcept
{
    const gchar* guid_str;

    g_return_if_fail (sql_be != NULL);
    g_return_if_fail (pObject != NULL);

    try
    {
        auto val = row.get_string_at_col (m_col_name);
        GncGUID guid;
        Transaction *tx = nullptr;
        if (string_to_guid (val.c_str(), &guid))
            tx = xaccTransLookup (&guid, sql_be->book());

        // If the transaction is not found, try loading it
	std::string tpkey(tx_col_table[0]->name());
        if (tx == nullptr)
        {
	    std::string sql = tpkey + " = '" + val + "'";
            query_transactions ((GncSqlBackend*)sql_be, sql);
            tx = xaccTransLookup (&guid, sql_be->book());
        }

        if (tx != nullptr)
            set_parameter (pObject, tx, get_setter(obj_name), m_gobj_param_name);
    }
    catch (std::invalid_argument&) {}
}

template<> void
GncSqlColumnTableEntryImpl<CT_TXREF>::add_to_table(ColVec& vec) const noexcept
{
    add_objectref_guid_to_table(vec);
}

template<> void
GncSqlColumnTableEntryImpl<CT_TXREF>::add_to_query(QofIdTypeConst obj_name,
                                                   const gpointer pObject,
                                                   PairVec& vec) const noexcept
{
    add_objectref_guid_to_query(obj_name, pObject, vec);
}

/* ========================== END OF FILE ===================== */
