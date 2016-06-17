/********************************************************************
 * gnc-account-sql.c: load and save data to SQL                     *
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
/** @file gnc-account-sql.c
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */
#include <guid.hpp>
extern "C"
{
#include "config.h"

#include <glib.h>

#include "qof.h"
#include "Account.h"
#include "AccountP.h"
#include "gnc-commodity.h"

#if defined( S_SPLINT_S )
#include "splint-defs.h"
#endif
}
#include "gnc-backend-sql.h"

#include "gnc-account-sql.h"
#include "gnc-commodity-sql.h"
#include "gnc-slots-sql.h"
#include "gnc-transaction-sql.h"

static QofLogModule log_module = G_LOG_DOMAIN;

#define TABLE_NAME "accounts"
#define TABLE_VERSION 1

static  gpointer get_parent (gpointer pObject);
static void set_parent (gpointer pObject,  gpointer pValue);
static void set_parent_guid (gpointer pObject,  gpointer pValue);

#define ACCOUNT_MAX_NAME_LEN 2048
#define ACCOUNT_MAX_TYPE_LEN 2048
#define ACCOUNT_MAX_CODE_LEN 2048
#define ACCOUNT_MAX_DESCRIPTION_LEN 2048

static const EntryVec col_table
{
    gnc_sql_make_table_entry<CT_GUID>("guid", 0, COL_NNUL | COL_PKEY, "guid" ),
    gnc_sql_make_table_entry<CT_STRING>(
        "name", ACCOUNT_MAX_NAME_LEN, COL_NNUL, "name"),
    gnc_sql_make_table_entry<CT_STRING>("account_type", ACCOUNT_MAX_TYPE_LEN,
                                        COL_NNUL, ACCOUNT_TYPE_, true),
    gnc_sql_make_table_entry<CT_COMMODITYREF>(
        "commodity_guid", 0, 0, "commodity"),
    gnc_sql_make_table_entry<CT_INT>(
        "commodity_scu", 0, COL_NNUL, "commodity-scu"),
    gnc_sql_make_table_entry<CT_BOOLEAN>(
        "non_std_scu", 0, COL_NNUL, "non-std-scu"),
        gnc_sql_make_table_entry<CT_GUID>("parent_guid", 0, 0,
                                          (QofAccessFunc)get_parent,
                                          (QofSetterFunc)set_parent),
    gnc_sql_make_table_entry<CT_STRING>(
        "code", ACCOUNT_MAX_CODE_LEN, 0, "code"),
    gnc_sql_make_table_entry<CT_STRING>(
        "description", ACCOUNT_MAX_DESCRIPTION_LEN, 0, "description"),
    gnc_sql_make_table_entry<CT_BOOLEAN>("hidden", 0, 0, "hidden"),
    gnc_sql_make_table_entry<CT_BOOLEAN>("placeholder", 0, 0, "placeholder"),
};
static EntryVec parent_col_table
({
    gnc_sql_make_table_entry<CT_GUID>(
        "parent_guid", 0, 0, nullptr, (QofSetterFunc)set_parent_guid),
});

class GncSqlAccountBackend : public GncSqlObjectBackend
{
public:
    GncSqlAccountBackend(int version, const std::string& type,
                         const std::string& table, const EntryVec& vec) :
        GncSqlObjectBackend(version, type, table, vec) {}
    void load_all(GncSqlBackend*) override;
    bool commit(GncSqlBackend*, QofInstance*) override;
};



typedef struct
{
    Account* pAccount;
    GncGUID guid;
} account_parent_guid_struct;

/* ================================================================= */

static  gpointer
get_parent (gpointer pObject)
{
    const Account* pAccount;
    const Account* pParent;
    const GncGUID* parent_guid;

    g_return_val_if_fail (pObject != NULL, NULL);
    g_return_val_if_fail (GNC_IS_ACCOUNT (pObject), NULL);

    pAccount = GNC_ACCOUNT (pObject);
    pParent = gnc_account_get_parent (pAccount);
    if (pParent == NULL)
    {
        parent_guid = NULL;
    }
    else
    {
        parent_guid = qof_instance_get_guid (QOF_INSTANCE (pParent));
    }

    return (gpointer)parent_guid;
}

static void
set_parent (gpointer pObject,  gpointer pValue)
{
    Account* pAccount;
    QofBook* pBook;
    GncGUID* guid = (GncGUID*)pValue;
    Account* pParent;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (GNC_IS_ACCOUNT (pObject));

    pAccount = GNC_ACCOUNT (pObject);
    pBook = qof_instance_get_book (QOF_INSTANCE (pAccount));
    if (guid != NULL)
    {
        pParent = xaccAccountLookup (guid, pBook);
        if (pParent != NULL)
        {
            gnc_account_append_child (pParent, pAccount);
        }
    }
}

static void
set_parent_guid (gpointer pObject,  gpointer pValue)
{
    account_parent_guid_struct* s = (account_parent_guid_struct*)pObject;
    GncGUID* guid = (GncGUID*)pValue;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (pValue != NULL);

    s->guid = *guid;
}

static  Account*
load_single_account (GncSqlBackend* be, GncSqlRow& row,
                     GList** l_accounts_needing_parents)
{
    const GncGUID* guid;
    Account* pAccount = NULL;

    g_return_val_if_fail (be != NULL, NULL);
    g_return_val_if_fail (l_accounts_needing_parents != NULL, NULL);

    guid = gnc_sql_load_guid (be, row);
    if (guid != NULL)
    {
        pAccount = xaccAccountLookup (guid, be->book);
    }
    if (pAccount == NULL)
    {
        pAccount = xaccMallocAccount (be->book);
    }
    xaccAccountBeginEdit (pAccount);
    gnc_sql_load_object (be, row, GNC_ID_ACCOUNT, pAccount, col_table);
    xaccAccountCommitEdit (pAccount);

    /* If we don't have a parent and this isn't the root account, it might be because the parent
       account hasn't been loaded yet.  Remember the account and its parent guid for later. */
    if (gnc_account_get_parent (pAccount) == NULL
        && pAccount != gnc_book_get_root_account (be->book))
    {
        account_parent_guid_struct* s = static_cast<decltype (s)> (
                                            g_malloc (sizeof (account_parent_guid_struct)));
        g_assert (s != NULL);

        s->pAccount = pAccount;
        gnc_sql_load_object (be, row, GNC_ID_ACCOUNT, s, parent_col_table);
        *l_accounts_needing_parents = g_list_prepend (*l_accounts_needing_parents, s);
    }

    return pAccount;
}

void
GncSqlAccountBackend::load_all (GncSqlBackend* be)
{
    GncSqlStatement* stmt = NULL;
    QofBook* pBook;
    GList* l_accounts_needing_parents = NULL;
    GSList* bal_slist;
    GSList* bal;

    g_return_if_fail (be != NULL);

    ENTER ("");

    pBook = be->book;

    stmt = gnc_sql_create_select_statement (be, TABLE_NAME);
    if (stmt == NULL)
    {
        LEAVE ("stmt == NULL");
        return;
    }
    auto result = gnc_sql_execute_select_statement (be, stmt);
    delete stmt;
    for (auto row : *result)
        load_single_account (be, row, &l_accounts_needing_parents);

    auto sql = g_strdup_printf ("SELECT DISTINCT guid FROM %s", TABLE_NAME);
    gnc_sql_slots_load_for_sql_subquery (be, sql, (BookLookupFn)xaccAccountLookup);
    g_free (sql);

    /* While there are items on the list of accounts needing parents,
       try to see if the parent has now been loaded.  Theory says that if
       items are removed from the front and added to the back if the
       parent is still not available, then eventually, the list will
       shrink to size 0. */
    if (l_accounts_needing_parents != NULL)
    {
        gboolean progress_made = TRUE;
        Account* root;
        Account* pParent;
        GList* elem;

        while (progress_made)
        {
            progress_made = FALSE;
            for (elem = l_accounts_needing_parents; elem != NULL;)
            {
                account_parent_guid_struct* s = (account_parent_guid_struct*)elem->data;
                pParent = xaccAccountLookup (&s->guid, be->book);
                if (pParent != NULL)
                {
                    GList* next_elem;

                    gnc_account_append_child (pParent, s->pAccount);
                    next_elem = g_list_next (elem);
                    l_accounts_needing_parents = g_list_delete_link (l_accounts_needing_parents,
                                                                     elem);
                    g_free (s);
                    elem = next_elem;
                    progress_made = TRUE;
                }
                else
                {
                    /* Can't be up in the for loop because the 'then' clause reads inside a node freed
                       by g_list_delete_link(). */
                    elem = g_list_next (elem);
                }
            }
        }

        /* Any non-ROOT accounts left over must be parented by the root account */
        root = gnc_book_get_root_account (pBook);
        while (l_accounts_needing_parents != NULL)
        {
            account_parent_guid_struct* s = (account_parent_guid_struct*)
                l_accounts_needing_parents->data;
            if (xaccAccountGetType (s->pAccount) != ACCT_TYPE_ROOT)
            {
                gnc_account_append_child (root, s->pAccount);
            }
            g_free (s);
            l_accounts_needing_parents = g_list_delete_link (l_accounts_needing_parents,
                                                             l_accounts_needing_parents);
        }
    }

    /* Load starting balances */
    bal_slist = gnc_sql_get_account_balances_slist (be);
    for (bal = bal_slist; bal != NULL; bal = bal->next)
    {
        acct_balances_t* balances = (acct_balances_t*)bal->data;

        qof_instance_increase_editlevel (balances->acct);
        g_object_set (balances->acct,
                      "start-balance", &balances->balance,
                      "start-cleared-balance", &balances->cleared_balance,
                      "start-reconciled-balance", &balances->reconciled_balance,
                      NULL);

        qof_instance_decrease_editlevel (balances->acct);
    }
    if (bal_slist != NULL)
    {
        g_slist_free (bal_slist);
    }

    LEAVE ("");
}

/* ================================================================= */
bool
GncSqlAccountBackend::commit (GncSqlBackend* be, QofInstance* inst)
{
    Account* pAcc = GNC_ACCOUNT (inst);
    const GncGUID* guid;
    gboolean is_infant;
    gboolean is_ok = FALSE;
    gnc_commodity* commodity;
    E_DB_OPERATION op;

    g_return_val_if_fail (be != NULL, FALSE);
    g_return_val_if_fail (inst != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_ACCOUNT (inst), FALSE);

    ENTER ("inst=%p", inst);

    is_infant = qof_instance_get_infant (inst);

    // If there is no commodity yet, this might be because a new account name
    // has been entered directly into the register and an account window will
    // be opened.  The account info is not complete yet, but the name has been
    // set, triggering this commit
    commodity = xaccAccountGetCommodity (pAcc);

    is_ok = TRUE;
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

    // If not deleting the account, ensure the commodity is in the db
    if (op != OP_DB_DELETE && commodity != NULL)
    {
        is_ok = gnc_sql_save_commodity (be, commodity);
    }

    if (is_ok)
    {
        is_ok = gnc_sql_do_db_operation (be, op, TABLE_NAME, GNC_ID_ACCOUNT, pAcc,
                                         col_table);
    }

    if (is_ok)
    {
        // Now, commit or delete any slots
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

    LEAVE ("is_ok=%d", is_ok);

    return is_ok;
}

/* ================================================================= */

template<> void
GncSqlColumnTableEntryImpl<CT_ACCOUNTREF>::load (const GncSqlBackend* be,
                                                 GncSqlRow& row,
                                                 QofIdTypeConst obj_name,
                                                 gpointer pObject) const noexcept
{
    load_from_guid_ref(row, obj_name, pObject,
                       [be](GncGUID* g){
                           return xaccAccountLookup(g, be->book);
                       });
}

template<> void
GncSqlColumnTableEntryImpl<CT_ACCOUNTREF>::add_to_table(const GncSqlBackend* be,
                                                 ColVec& vec) const noexcept
{
    add_objectref_guid_to_table(be, vec);
}

template<> void
GncSqlColumnTableEntryImpl<CT_ACCOUNTREF>::add_to_query(const GncSqlBackend* be,
                                                    QofIdTypeConst obj_name,
                                                    const gpointer pObject,
                                                    PairVec& vec) const noexcept
{
    add_objectref_guid_to_query(be, obj_name, pObject, vec);
}
/* ================================================================= */
void
gnc_sql_init_account_handler (void)
{
    static GncSqlAccountBackend be_data{
        GNC_SQL_BACKEND_VERSION, GNC_ID_ACCOUNT, TABLE_NAME, col_table};
    gnc_sql_register_backend(&be_data);
}
/* ========================== END OF FILE ===================== */
