/********************************************************************\
 * account.c -- implements account handling for postgres backend    *
 * Copyright (c) 2000, 2001, 2002 Linas Vepstas <linas@linas.org>   *
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

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include <libpq-fe.h>

#include "AccountP.h"
#include "qof.h"
#include "gnc-commodity.h"
#include "gnc-pricedb.h"

#include "account.h"
#include "book.h"
#include "base-autogen.h"
#include "kvp-sql.h"
#include "PostgresBackend.h"
#include "price.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

#include "putil.h"

/* ============================================================= */
/* ============================================================= */
/*                    ACCOUNT STUFF                              */
/*      (UTILITIES FIRST, THEN SETTERS, THEN GETTERS)            */
/* ============================================================= */
/* ============================================================= */

/* ============================================================= */
/* the pgendStoreAccount() routine stores an account to the
 * database.  That is, the engine data is written out to the
 * database.  It does not do any of the account children; nor
 * does it handle any of the splits or transactions associated
 * with the account.  It does, however, store the associated
 * commodity.
 *
 * If do_mark is set to TRUE, then this routine sets a mark
 * to terminate recursion.  That is,  it will only store the
 * account once; a second call on a marked account will simply
 * return.  Be sure to clear the mark when done!
 *
 * If the do_check_version flag is set, then this routine
 * will compare the engine and sql db version numbrs, and
 * perform the store only if the engine version is equal
 * or newer than the sql version.
 *
 * This routine doesn't perform any locks, and shouldn't be
 * used outside of locks,
 */

static void
pgendStoreAccountNoLock (PGBackend *be, Account *acct,
                         gboolean do_mark,
                         gboolean do_check_version)
{
    const gnc_commodity *com;
    guint32 a_idata;

    if (!be || !acct) return;
    if ((FALSE == do_mark) && (!qof_instance_get_dirty_flag(acct)))
        return;

    ENTER ("acct=%p, mark=%d", acct, do_mark);

    if (do_mark)
    {
        /* Check to see if we've processed this account recently.
         * If so, then return.  The goal here is to avoid excess
         * hits to the database, leading to poor performance.
         * Note that this marking makes this routine unsafe to use
         * outside a lock (since we never clear the mark)
         */
        if (xaccAccountGetMark (acct)) return;
        xaccAccountSetMark (acct, 1);
    }

    if (do_check_version)
    {
        if (0 < pgendAccountCompareVersion (be, acct)) return;
    }
    /* be sure to update the version !! */
    qof_instance_increment_version(acct, be->version_check);

    a_idata = qof_instance_get_idata(acct);
    if ((0 == a_idata) &&
            (FALSE == kvp_frame_is_empty (xaccAccountGetSlots(acct))))
    {
        a_idata = pgendNewGUIDidx(be);
        qof_instance_set_idata(acct, a_idata);
    }

    pgendPutOneAccountOnly (be, acct);

    /* make sure the account's commodity is in the commodity table */

    /* XXX hack alert FIXME -- it would be more efficient to do
     * this elsewhere, and not here. Furthermore, with this method
     * the transaction currencies must be stored in the same way,
     * as the transactions are traversed individually, and that
     * is even more inefficient.
     *
     * See StoreAllPrices for an example of how to do this.
     */
    com = xaccAccountGetCommodity (acct);
    pgendPutOneCommodityOnly (be, (gnc_commodity *) com);

    if (a_idata)
    {
        pgendKVPDelete (be, a_idata);
        pgendKVPStore (be, a_idata, acct->inst.kvp_data);
    }
    LEAVE(" ");
}

/* ============================================================= */
/* The pgendStoreAccountTree() routine stores the account hierarchy
 * to the sql database.  That is, it stores not oonly the top-level
 * accounts, but all of thier children too.   It also stores the
 * commodities associated with the accounts.  It does *not* store
 * any of the transactions.
 *
 * Note that it checks the version numbers, and only stores
 * those accounts whose version number is equal or newer than
 * what's in the DB.
 *
 * The NoLock version doesn't lock up the tables.
 */

void
pgendStoreAccountTreeNoLock (PGBackend *be, Account *root,
                             gboolean do_mark, gboolean do_check_version)
{
    GList *descendants, *node;

    if (!be || !root) return;
    ENTER("root=%p mark=%d", root, do_mark);

    /* walk the account tree, and store subaccounts */
    pgendStoreAccountNoLock (be, root, do_mark, do_check_version);
    descendants = gnc_account_get_descendants (root);
    for (node = descendants; node; node = node->next)
        pgendStoreAccountNoLock (be, node->data, do_mark, do_check_version);
    g_list_free(descendants);
    LEAVE(" ");
}


void
pgendStoreAccountTree (PGBackend *be, Account *root)
{
    char *p;
    ENTER ("be=%p, root=%p", be, root);
    if (!be || !root) return;

    /* lock it up so that we store atomically */
    p = "BEGIN;\n"
        "LOCK TABLE gncAccount IN EXCLUSIVE MODE;\n"
        "LOCK TABLE gncCommodity IN EXCLUSIVE MODE;\n";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    /* Clear the account marks; this is used to avoid visiting
     * the same account more than once. */
    xaccClearMarkDown (root, 0);

    pgendStoreAccountTreeNoLock (be, root, TRUE, TRUE);

    /* reset the write flags again */
    xaccClearMarkDown (root, 0);

    p = "COMMIT;\n"
        "NOTIFY gncAccount;";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);
    LEAVE(" ");
}

/* ============================================================= */
/*        ACCOUNT GETTERS (SETTERS ARE ABOVE)                    */
/* ============================================================= */

/* ============================================================= */
/* This routine walks the account tree, gets all KVP values */

static void
restore_cb (Account *acc, void * cb_data)
{
    PGBackend *be = (PGBackend *) cb_data;
    guint32 a_idata = qof_instance_get_idata(acc);
    if (0 == a_idata) return;
    acc->inst.kvp_data = pgendKVPFetch (be, a_idata, acc->inst.kvp_data);
}

static void
pgendGetAllAccountKVP (PGBackend *be, Account *root)
{
    if (!root) return;

    restore_cb(root, NULL);
    gnc_account_foreach_descendant(root, restore_cb, be);
}

/* ============================================================= */
/* The pgendGetAllAccounts() routine restores the account hierarchy
 * of *all* accounts in the DB.  Each account is stuffed into
 * its corresponding book.
 *
 * The pgendGetAllAccountsInBook() routine only fetches the accounts
 * for the indicated book.
 */

typedef struct
{
    Account * account;

    char * commodity_string; /* If non-NULL, need to load commodity */
    gboolean need_parent;    /* If TRUE, need to load parent */
    GUID parent_guid;        /* GUID of parent */
} AccountResolveInfo;

typedef struct
{
    QofBook * book;
    GList * resolve_info;
} GetAccountData;

static AccountResolveInfo *
get_resolve_info (AccountResolveInfo * ri)
{
    if (ri) return ri;
    return g_new0 (AccountResolveInfo, 1);
}

static gpointer
get_account_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    GetAccountData * gad = data;
    QofBook * book = gad->book;
    Account *parent;
    Account *acc;
    GUID acct_guid;
    char * commodity_string;
    gnc_commodity * commodity;
    AccountResolveInfo *ri = NULL;

    PINFO ("account GUID=%s", DB_GET_VAL("accountGUID", j));

    FIND_BOOK (book);

    /* Next, lets see if we've already got this account */
    acct_guid = nullguid;  /* just in case the read fails ... */
    string_to_guid (DB_GET_VAL("accountGUID", j), &acct_guid);

    acc = xaccAccountLookup (&acct_guid, book);
    if (!acc)
    {
        acc = xaccMallocAccount(book);
        xaccAccountBeginEdit(acc);
        xaccAccountSetGUID(acc, &acct_guid);
    }
    else
    {
        xaccAccountBeginEdit(acc);
    }

    commodity_string = DB_GET_VAL("commodity", j);
    commodity = gnc_string_to_commodity (commodity_string, book);
    if (!commodity)
    {
        ri = get_resolve_info (ri);

        ri->account = acc;
        ri->commodity_string = g_strdup (commodity_string);
    }

    xaccAccountSetName(acc, DB_GET_VAL("accountName", j));
    xaccAccountSetDescription(acc, DB_GET_VAL("description", j));
    xaccAccountSetCode(acc, DB_GET_VAL("accountCode", j));
    xaccAccountSetType(acc, xaccAccountStringToEnum(DB_GET_VAL("type", j)));
    if (commodity)
        xaccAccountSetCommodity(acc, commodity);
    qof_instance_set_version(acc, atoi(DB_GET_VAL("version", j)));
    qof_instance_set_idata(acc, atoi(DB_GET_VAL("iguid", j)));

    /* try to find the parent account */
    PINFO ("parent GUID=%s", DB_GET_VAL("parentGUID", j));
    acct_guid = nullguid;  /* just in case the read fails ... */
    string_to_guid (DB_GET_VAL("parentGUID", j), &acct_guid);
    if (guid_equal(guid_null(), &acct_guid))
    {
        /* if the parent guid is null, then this
         * account belongs in the top level */
        gnc_account_append_child (gnc_book_get_root_account(book), acc);
    }
    else
    {
        /* if we haven't restored the parent account, create
         * a resolution node for it */
        parent = xaccAccountLookup (&acct_guid, book);
        if (!parent)
        {
            ri = get_resolve_info (ri);

            ri->account = acc;
            ri->need_parent = TRUE;
            ri->parent_guid = acct_guid;
        }
        else
        {
            xaccAccountBeginEdit(parent);
            gnc_account_append_child(parent, acc);
            xaccAccountCommitEdit(parent);
        }
    }

    xaccAccountCommitEdit(acc);

    if (ri)
        gad->resolve_info = g_list_prepend (gad->resolve_info, ri);

    return data;
}

static void
pgendGetAccounts (PGBackend *be, QofBook *book)
{
    GetAccountData gad;

    gad.book = book ? book : be->book;
    gad.resolve_info = NULL;

    pgendGetResults (be, get_account_cb, &gad);

    while (gad.resolve_info)
    {
        AccountResolveInfo *ri = gad.resolve_info->data;

        gad.resolve_info = g_list_remove (gad.resolve_info, ri);

        xaccAccountBeginEdit (ri->account);

        if (ri->commodity_string)
        {
            gnc_commodity * commodity;

            pgendGetCommodity (be, ri->commodity_string);
            commodity = gnc_string_to_commodity (ri->commodity_string,
                                                 gnc_account_get_book(ri->account));

            if (commodity)
            {
                xaccAccountSetCommodity (ri->account, commodity);
            }
            else
            {
                PERR ("Can't find commodity %s", ri->commodity_string);
            }

            g_free (ri->commodity_string);
            ri->commodity_string = NULL;
        }

        if (ri->need_parent)
        {
            Account * parent;

            /* parent could have been pulled in after node was inserted */
            parent = xaccAccountLookup (&ri->parent_guid, gad.book);

            if (!parent)
                parent = pgendCopyAccountToEngine (be, &ri->parent_guid);

            if (parent)
            {
                xaccAccountBeginEdit(parent);
                gnc_account_append_child(parent, ri->account);
                xaccAccountCommitEdit(parent);
            }
            else
            {
                PERR ("no such account: %s", guid_to_string (&ri->parent_guid));
            }
        }

        xaccAccountCommitEdit (ri->account);

        g_free (ri);
    }
}

void
pgendGetAllAccounts (PGBackend *be)
{
    QofBookList *node;
    char * bufp;

    ENTER ("be=%p", be);
    if (!be) return;

    /* get all the books in the database */
    pgendGetAllBooks (be, be->blist);

    /* Make sure commodities table is up to date */
    pgendGetAllCommodities (be);

    /* Get them ALL */
    bufp = "SELECT * FROM gncAccount;";
    SEND_QUERY (be, bufp, );
    pgendGetAccounts (be, NULL);

    for (node = be->blist; node; node = node->next)
    {
        QofBook *book = node->data;
        Account *root = gnc_book_get_root_account(book);
        pgendGetAllAccountKVP (be, root);
    }

    LEAVE (" ");
}

void
pgendGetAllAccountsInBook (PGBackend *be, QofBook *book)
{
    char *p, buff[400];
    Account *root;

    ENTER ("be=%p", be);
    if (!be || !book) return;

    /* first, make sure commodities table is up to date */
    pgendGetAllCommodities (be);

    /* Get everything for this book */

    p = buff;
    p = stpcpy (p, "SELECT * FROM gncAccount WHERE bookGuid='");
    p = guid_to_string_buff (qof_book_get_guid(book), p);
    p = stpcpy (p, "';");
    SEND_QUERY (be, buff, );
    pgendGetAccounts (be, book);

    root = gnc_book_get_root_account(book);
    pgendGetAllAccountKVP (be, root);

    LEAVE (" ");
}


/* ============================================================= */

Account *
pgendCopyAccountToEngine (PGBackend *be, const GUID *acct_guid)
{
    char *pbuff;
    Account *acc = NULL;
    int engine_data_is_newer = 0;
    guint32 a_idata;

    ENTER ("be=%p", be);
    if (!be || !acct_guid) return 0;

    /* disable callbacks into the backend, and events to GUI */
    qof_event_suspend();
    pgendDisable(be);

    /* First, see if we already have such an account */
    acc = pgendAccountLookup (be, acct_guid);

    if (!acc)
    {
        engine_data_is_newer = -1;
    }
    else
    {
        /* save some performance, don't go to the
         * backend if the data is recent. */
        guint32 value = qof_instance_get_version_check(acc);
        if (MAX_VERSION_AGE >= be->version_check - value)
        {
            PINFO ("fresh data, skip check");
            engine_data_is_newer = 0;
        }
        else
        {
            engine_data_is_newer = - pgendAccountCompareVersion (be, acc);
        }
    }

    if (0 > engine_data_is_newer)
    {
        /* build the sql query to get the account */
        pbuff = be->buff;
        pbuff[0] = 0;
        pbuff = stpcpy (pbuff,
                        "SELECT * FROM gncAccount WHERE accountGuid='");
        pbuff = guid_to_string_buff(acct_guid, pbuff);
        pbuff = stpcpy (pbuff, "';");

        SEND_QUERY (be, be->buff, 0);
        pgendGetAccounts (be, NULL);
        acc = pgendAccountLookup (be, acct_guid);

        /* restore any kvp data associated with the transaction and splits */
        if (acc)
        {
            a_idata = qof_instance_get_idata(acc);
            if (a_idata)
            {
                acc->inst.kvp_data = pgendKVPFetch (be, a_idata, acc->inst.kvp_data);
            }

            qof_instance_set_version_check(acc, be->version_check);
        }
    }

    /* re-enable events to the backend and GUI */
    pgendEnable(be);
    qof_event_resume();

    LEAVE (" ");
    return acc;
}

/* ============================================================= */
/* ============================================================= */
/*         HIGHER LEVEL ROUTINES AND BACKEND PROPER              */
/* ============================================================= */
/* ============================================================= */

void
pgend_account_commit_edit (QofBackend * bend,
                           Account * acct)
{
    char *p;
    QofBackendError err;
    PGBackend *be = (PGBackend *)bend;

    ENTER ("be=%p, acct=%p", be, acct);
    if (!be || !acct) return;

    if (!qof_instance_get_dirty_flag(acct))
    {
        LEAVE ("account not written because not dirty");
        return;
    }

    /* lock it up so that we query and store atomically */
    /* its not at all clear to me that this isn't rife with deadlocks. */
    p = "BEGIN;\n"
        "LOCK TABLE gncAccount IN EXCLUSIVE MODE;\n"
        "LOCK TABLE gncCommodity IN EXCLUSIVE MODE;\n";

    SEND_QUERY (be, p,);
    FINISH_QUERY(be->connection);

    /* check to see that the engine version is equal or newer than
     * whats in the database.  It its not, then some other user has
     * made changes, and we must roll back. */
    if (0 < pgendAccountCompareVersion (be, acct))
    {
        qof_instance_set_destroying(acct, FALSE);
        p = "ROLLBACK;";
        SEND_QUERY (be, p,);
        FINISH_QUERY(be->connection);

        /* hack alert -- we should restore the account data from the
         * sql back end at this point ! !!! */
        PWARN(" account data in engine is newer\n"
              " account must be rolled back.  This function\n"
              " is not completely implemented !! \n");
        qof_backend_set_error (&be->be, ERR_BACKEND_MODIFIED);
        LEAVE ("rolled back");
        return;
    }
    /* be sure to update the version !! */
    qof_instance_increment_version(acct, be->version_check);

    if (qof_instance_get_destroying(acct))
    {
        const GUID *guid = xaccAccountGetGUID(acct);
        pgendKVPDelete (be, qof_instance_get_idata(acct));

        p = be->buff;
        *p = 0;
        p = stpcpy (p, "DELETE FROM gncAccount WHERE accountGuid='");
        p = guid_to_string_buff (guid, p);
        p = stpcpy (p, "';");
        err = sendQuery (be, be->buff);
        if (err == ERR_BACKEND_NO_ERR)
        {
            err = finishQuery(be);
            if (err > 0) /* if the number of rows deleted is 0 */
                pgendStoreAuditAccount (be, acct, SQL_DELETE);
        }
    }
    else
    {
        pgendStoreAccountNoLock (be, acct, FALSE, FALSE);
    }

    p = "COMMIT;\n"
        "NOTIFY gncAccount;";
    SEND_QUERY (be, p,);
    FINISH_QUERY(be->connection);

    LEAVE ("commited");
    return;
}

/* ======================== END OF FILE ======================== */
