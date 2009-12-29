/********************************************************************\
 * txn.c -- implements transaction handlers for postgres backend    *
 * Copyright (c) 2000, 2001 Linas Vepstas <linas@linas.org>         *
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <libpq-fe.h>

#include "Account.h"
#include "AccountP.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-pricedb.h"
#include "Transaction.h"
#include "TransactionP.h"

#include "PostgresBackend.h"
#include "account.h"
#include "base-autogen.h"
#include "checkpoint.h"
#include "kvp-sql.h"
#include "price.h"
#include "txn.h"

#include "putil.h"

static QofLogModule log_module = GNC_MOD_TXN;

/* ============================================================= */
/* ============================================================= */
/*            TRANSACTION STUFF                                  */
/* ============================================================= */
/* ============================================================= */
/* The is_trans_empty() routine returns TRUE if this appears to
 * be a fresh, 'null' transaction.  It would be better if somehow
 * we could get the gui to mark this as a fresh transaction, rather
 * than having to scan a bunch of fields.  But, oh well, this is
 * a minor quibble in the grand scheme of things.
 */

static gboolean
is_trans_empty (Transaction *trans)
{
    Split *s;
    if (!trans) return TRUE;
    if (0 != (xaccTransGetDescription(trans))[0]) return FALSE;
    if (0 != (xaccTransGetNum(trans))[0]) return FALSE;
    if (1 != xaccTransCountSplits(trans)) return FALSE;

    s = xaccTransGetSplit(trans, 0);
    if (TRUE != gnc_numeric_zero_p(xaccSplitGetAmount(s))) return FALSE;
    if (TRUE != gnc_numeric_zero_p(xaccSplitGetValue(s))) return FALSE;
    if ('n' != xaccSplitGetReconcile(s)) return FALSE;
    if (0 != (xaccSplitGetMemo(s))[0]) return FALSE;
    if (0 != (xaccSplitGetAction(s))[0]) return FALSE;
    if (TRUE != (kvp_frame_is_empty (xaccSplitGetSlots(s)))) return FALSE;
    return TRUE;
}

/* ============================================================= */
/* The pgendStoreTransactionNoLock() routine traverses the transaction
 * structure and stores/updates it in the database.  If checks the
 * transaction splits as well, updating those.  If the database
 * has splits which the transaction doesn't, those are deleted.
 * Then any new splits are poked into the database.
 *
 * If the do_check_version flag is set, then the database version
 * is compared to the engine version.  If the database version is
 * newer, then the engine transaction is not stored.
 *
 * The pgendStoreTransaction() routine does the same, except that
 * it locks the tables appropriately.
 */

typedef struct
{
    GUID guid;
    char *guid_str;
    guint32 iguid;
} DeleteTransInfo;

static gpointer
delete_list_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    GList * deletelist = (GList *) data;
    GUID guid = nullguid;

    string_to_guid (DB_GET_VAL ("splitGuid", j), &guid);

    /* If the database has splits that the engine doesn't,
     * collect 'em up & we'll have to delete em */
    if (NULL == pgendSplitLookup (be, &guid))
    {
        DeleteTransInfo *dti;

        dti = g_new (DeleteTransInfo, 1);

        dti->guid = guid;
        dti->guid_str = g_strdup (DB_GET_VAL ("splitGuid", j));
        dti->iguid = atoi (DB_GET_VAL ("iguid", j));

        deletelist = g_list_prepend (deletelist, dti);
    }

    return deletelist;
}

void
pgendStoreTransactionNoLock (PGBackend *be, Transaction *trans,
                             gboolean do_check_version)
{
    GList *start, *deletelist = NULL, *node;
    guint32 s_idata, t_idata;
    char * p;

    if (!be || !trans) return;
    ENTER ("trans=%p do_check=%d", trans, do_check_version);

    /* don't update the database if the database is newer ... */
    if (do_check_version)
    {
        if (0 < pgendTransactionCompareVersion (be, trans)) return;
    }
    /* be sure to update the version !! */
    qof_instance_increment_version(trans, be->version_check);

    /* first, we need to see which splits are in the database
     * since what is there may not match what we have cached in
     * the engine. */
    p = be->buff;
    *p = 0;
    p = stpcpy (p, "SELECT splitGuid, iguid FROM gncSplit WHERE transGuid='");
    p = guid_to_string_buff(xaccTransGetGUID(trans), p);
    p = stpcpy (p, "';");

    SEND_QUERY (be, be->buff, );
    deletelist = pgendGetResults (be, delete_list_cb, deletelist);

    /* delete those splits that don't belong */
    p = be->buff;
    *p = 0;
    for (node = deletelist; node; node = node->next)
    {
        DeleteTransInfo *dti = node->data;
        GList *split_node;

        /* find the old split in the saved original */
        if (trans->orig && trans->orig->splits)
            for (split_node = trans->orig->splits; split_node;
                    split_node = split_node->next)
            {
                Split *s = split_node->data;

                if (s && guid_equal (qof_instance_get_guid(s), &dti->guid))
                {
                    pgendStoreAuditSplit (be, s, SQL_DELETE);
                    break;
                }
            }

        p = stpcpy (p, "DELETE FROM gncSplit WHERE splitGuid='");
        p = stpcpy (p, dti->guid_str);
        p = stpcpy (p, "';\n");
    }

    if (p != be->buff)
    {
        PINFO ("%s", be->buff ? be->buff : "(null)");
        SEND_QUERY (be, be->buff, );
        FINISH_QUERY(be->connection);

        /* destroy any associated kvp data as well */
        for (node = deletelist; node; node = node->next)
        {
            DeleteTransInfo *dti = node->data;

            pgendKVPDelete (be, dti->iguid);
        }
    }

    for (node = deletelist; node; node = node->next)
    {
        DeleteTransInfo *dti = node->data;

        g_free (dti->guid_str);
        g_free (dti);
    }
    g_list_free (deletelist);
    deletelist = NULL;

    /* Update the rest */
    start = xaccTransGetSplitList(trans);

    PINFO ("split-list=%p, destroying=%d", start,
           qof_instance_get_destroying(trans));
    if ((start) && !qof_instance_get_destroying(trans))
    {
        gnc_commodity *com;

        for (node = start; node; node = node->next)
        {
            Split * s = node->data;
            s_idata = qof_instance_get_idata(s);
            if ((0 == s_idata) &&
                    (FALSE == kvp_frame_is_empty (xaccSplitGetSlots(s))))
            {
                s_idata = pgendNewGUIDidx(be);
                qof_instance_set_idata(s, s_idata);
            }
            pgendPutOneSplitOnly (be, s);
            if (s_idata)
            {
                pgendKVPDelete (be, s_idata);
                pgendKVPStore (be, s_idata, s->inst.kvp_data);
            }
        }

        t_idata = qof_instance_get_idata(trans);
        if ((0 == t_idata) &&
                (FALSE == kvp_frame_is_empty (xaccTransGetSlots(trans))))
        {
            t_idata = pgendNewGUIDidx(be);
            qof_instance_set_idata(trans, t_idata);
        }

        /* Make sure the commodity is in the table.
         * See account.c for why this might be bad. */
        com = xaccTransGetCurrency (trans);
        pgendPutOneCommodityOnly (be, com);

        pgendPutOneTransactionOnly (be, trans);

        if (t_idata)
        {
            pgendKVPDelete (be, t_idata);
            pgendKVPStore (be, t_idata, trans->inst.kvp_data);
        }
    }
    else
    {
        p = be->buff;
        *p = 0;
        for (node = start; node; node = node->next)
        {
            Split * s = node->data;
            pgendStoreAuditSplit (be, s, SQL_DELETE);
            p = stpcpy (p, "DELETE FROM gncSplit WHERE splitGuid='");
            p = guid_to_string_buff (xaccSplitGetGUID(s), p);
            p = stpcpy (p, "';\n");
        }

        /* If this trans is marked for deletetion, use the 'orig' values
         * as the base for recording the audit.  This wouldn't be normally
         * required, except that otherwise one gets a trashed currency
         * value.
         */
        pgendStoreAuditTransaction (be, trans->orig, SQL_DELETE);
        p = be->buff;
        p = stpcpy (p, "DELETE FROM gncTransaction WHERE transGuid='");
        p = guid_to_string_buff (xaccTransGetGUID(trans), p);
        p = stpcpy (p, "';");
        PINFO ("%s\n", be->buff ? be->buff : "(null)");
        SEND_QUERY (be, be->buff, );
        FINISH_QUERY(be->connection);

        /* destroy any associated kvp data as well */
        for (node = start; node; node = node->next)
        {
            Split * s = node->data;
            s_idata = qof_instance_get_idata(s);
            if (0 != s_idata) pgendKVPDelete (be, s_idata);
        }
        t_idata = qof_instance_get_idata(trans);
        if (0 != t_idata) pgendKVPDelete (be, t_idata);
    }

    LEAVE(" ");
}

#if 0
/* This routine isn't used anywhere, and probably shouldn't
 * be, in part because its balance checkpointing algorithm
 * is wrong. */

static void
pgendStoreTransaction (PGBackend *be, Transaction *trans)
{
    char * bufp;
    if (!be || !trans) return;
    ENTER ("be=%p, trans=%p", be, trans);

    /* lock it up so that we store atomically */
    bufp = "BEGIN;\n"
           "LOCK TABLE gncTransaction IN EXCLUSIVE MODE;\n"
           "LOCK TABLE gncSplit IN EXCLUSIVE MODE;\n";
    SEND_QUERY (be, bufp, );
    FINISH_QUERY(be->connection);

    pgendStoreTransactionNoLock (be, trans, TRUE);

    bufp = "COMMIT;\n"
           "NOTIFY  gncTransaction;";
    SEND_QUERY (be, bufp, );
    FINISH_QUERY(be->connection);

    /* If this is the multi-user mode, we need to update the
     * balances as well.  */
    if ((MODE_POLL == be->session_mode) ||
            (MODE_EVENT == be->session_mode))
    {
        /* hack alert --  we should also recompute
         * the checkpoints for any accounts from which splits have
         * been deleted ... but we don't have these handy here ...
         * is this is actually kinda wrong ...
         */
        pgendTransactionRecomputeCheckpoints (be, trans);
    }

    LEAVE(" ");
}
#endif

/* ============================================================= */
/* The pgendStoreAllTransactions() routine traverses through *all*
 * transactions in the account group, storing these to the database.
 * During the store, it checks the transaction version numbers,
 * and only stores those transactions that were newer in the engine.
 */

static int
trans_traverse_cb (Transaction *trans, void *cb_data)
{
    pgendStoreTransactionNoLock ((PGBackend *) cb_data, trans, TRUE);
    return 0;
}


void
pgendStoreAllTransactions (PGBackend *be, Account *root)
{
    char *p;
    ENTER ("be=%p, root=%p", be, root);
    if (!be || !root) return;

    /* lock it up so that we store atomically */
    p = "BEGIN;\n"
        "LOCK TABLE gncTransaction IN EXCLUSIVE MODE;\n"
        "LOCK TABLE gncSplit IN EXCLUSIVE MODE;\n";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    /* Recursively walk transactions. Start by reseting the write
     * flags. We use this to avoid infinite recursion */
    gnc_account_tree_begin_staged_transaction_traversals(root);
    gnc_account_tree_staged_transaction_traversal (root, 1, trans_traverse_cb, be);

    p = "COMMIT;\n"
        "NOTIFY gncTransaction;";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    /* If this is the multi-user mode, we need to update the
     * balances as well.  */
    if ((MODE_POLL == be->session_mode) ||
            (MODE_EVENT == be->session_mode))
    {
        pgendAccountTreeRecomputeAllCheckpoints(be, root);
    }
    LEAVE(" ");
}

/* ============================================================= */
/*
 * The pgendCopyTransactionToEngine() routine 'copies' data out of
 *    the SQL database and into the engine, for the indicated
 *    Transaction GUID.  It starts by looking for an existing
 *    transaction in the engine with such a GUID.  If found, then
 *    it compares the version of last update to what's in the sql DB.
 *    If the engine data is older, or the engine doesn't yet have
 *    this transaction, then the full update happens.  The full
 *    update sets up the transaction structure, all of the splits
 *    in the transaction, and makes sure that all of the splits
 *    are in the proper accounts.  If the pre-existing tranasaction
 *    in the engine has more splits than what's in the DB, then these
 *    are pruned so that the structure exactly matches what's in the
 *    DB.  This routine then returns -1.
 *
 *    If this routine finds a pre-existing transaction in the engine,
 *    and the version of last modification of this transaction is
 *    equal to or *newer* then what the DB holds, then this routine
 *    returns 0 if equal, and +1 if newer, and does *not* perform any
 *    update.  (Note that 0 is returned for various error conditions.
 *    Thus, testing for 0 is a bad idea.  This is a hack, and should
 *    probably be fixed.
 */

typedef struct
{
    Split * split;
    GUID account_guid;
    gint64 amount;
} SplitResolveInfo;

void
pgendCopySplitsToEngine (PGBackend *be, Transaction *trans)
{
    char *pbuff;
    int i, j, nrows;
    PGresult *result;
    const GUID *trans_guid;
    Account *acc, *previous_acc = NULL;
    GList *node, *db_splits = NULL, *engine_splits, *delete_splits = NULL;
    GList *unresolved_splits = NULL;
    gnc_commodity *currency = NULL;
    gint64 trans_frac = 0;

    trans_guid = xaccTransGetGUID (trans);
    currency = xaccTransGetCurrency (trans);
    trans_frac = gnc_commodity_get_fraction (currency);

    /* build the sql query the splits */
    pbuff = be->buff;
    pbuff[0] = 0;
    pbuff = stpcpy (pbuff,
                    "SELECT * FROM gncSplit WHERE transGuid='");
    pbuff = guid_to_string_buff(trans_guid, pbuff);
    pbuff = stpcpy (pbuff, "';");

    SEND_QUERY (be, be->buff, );
    i = 0;
    nrows = 0;
    do
    {
        GET_RESULTS (be->connection, result);
        {
            int j, jrows;
            int ncols = PQnfields (result);
            jrows = PQntuples (result);
            nrows += jrows;
            PINFO ("query result %d has %d rows and %d cols",
                   i, nrows, ncols);

            for (j = 0; j < jrows; j++)
            {
                Split *s;
                GUID guid;
                Timespec ts;
                gint64 num;
                gnc_numeric value, amount;

                /* --------------------------------------------- */
                /* first, lets see if we've already got this one */
                PINFO ("split GUID=%s", DB_GET_VAL("splitGuid", j));
                guid = nullguid;  /* just in case the read fails ... */
                string_to_guid (DB_GET_VAL("splitGuid", j), &guid);
                s = pgendSplitLookup (be, &guid);
                if (!s)
                {
                    s = xaccMallocSplit(qof_instance_get_book(trans));
                    xaccSplitSetGUID(s, &guid);
                }

                /* next, restore all split data */
                xaccSplitSetMemo(s, DB_GET_VAL("memo", j));
                xaccSplitSetAction(s, DB_GET_VAL("action", j));
                ts = gnc_iso8601_to_timespec_gmt
                     (DB_GET_VAL("date_reconciled", j));
                xaccSplitSetDateReconciledTS (s, &ts);

                xaccSplitSetReconcile (s, (DB_GET_VAL("reconciled", j))[0]);
                qof_instance_set_idata(s, atoi(DB_GET_VAL("iguid", j)));

                /* --------------------------------------------- */
                /* next, find the account that this split goes into */
                guid = nullguid;  /* just in case the read fails ... */
                string_to_guid (DB_GET_VAL("accountGUID", j), &guid);
                acc = pgendAccountLookup (be, &guid);

                if (!acc)
                {
                    SplitResolveInfo *sri = g_new0 (SplitResolveInfo, 1);

                    sri->split = s;
                    sri->account_guid = guid;
                    sri->amount = strtoll (DB_GET_VAL("amount", j), NULL, 0);

                    unresolved_splits = g_list_prepend (unresolved_splits, sri);
                }

                xaccTransAppendSplit (trans, s);

                if (acc)
                {
                    if (acc != previous_acc)
                    {
                        xaccAccountCommitEdit (previous_acc);
                        xaccAccountBeginEdit (acc);
                        previous_acc = acc;
                    }

                    xaccAccountInsertSplit(acc, s);
                }

                /* It's ok to set value without an account, since
                 * the fraction depends on the transaction and not
                 * the account. */
                num = strtoll (DB_GET_VAL("value", j), NULL, 0);
                value = gnc_numeric_create (num, trans_frac);
                xaccSplitSetValue (s, value);

                if (acc)
                {
                    int acct_frac;

                    num = strtoll (DB_GET_VAL("amount", j), NULL, 0);
                    acct_frac = xaccAccountGetCommoditySCU (acc);
                    amount = gnc_numeric_create (num, acct_frac);
                    xaccSplitSetAmount (s, amount);
                }

                /* finally tally them up; we use this below to
                 * clean out deleted splits */
                db_splits = g_list_prepend (db_splits, s);
            }
        }
        i++;
        PQclear (result);
    }
    while (result);

    /* close out dangling edit session */
    xaccAccountCommitEdit (previous_acc);

    /* resolve any splits that didn't have accounts */
    for (node = unresolved_splits; node; node = node->next)
    {
        SplitResolveInfo * sri = node->data;
        Account * account;

        /* account could have been pulled in by a previous
         * iteration of this loop. */
        account = pgendAccountLookup (be, &sri->account_guid);

        if (!account)
        {
            account = pgendCopyAccountToEngine (be, &sri->account_guid);
        }

        if (account)
        {
            gnc_numeric amount;
            int acct_frac;

            xaccAccountBeginEdit (account);
            xaccAccountInsertSplit (account, sri->split);
            xaccAccountCommitEdit (account);

            acct_frac = xaccAccountGetCommoditySCU (account);
            amount = gnc_numeric_create (sri->amount, acct_frac);
            xaccSplitSetAmount (sri->split, amount);
        }
        else
        {
            gchar str1[GUID_ENCODING_LENGTH+1];
            gchar str2[GUID_ENCODING_LENGTH+1];

            guid_to_string_buff(xaccSplitGetGUID (sri->split), str1);
            guid_to_string_buff(&sri->account_guid, str2);

            PERR ("account not found, will delete this split\n"
                  "\t(split with  guid=%s\n"
                  "\twants an acct with guid=%s)\n",
                  str1, str2);

            /* Remove the split from the list */
            db_splits = g_list_remove (db_splits, sri->split);

            xaccSplitDestroy (sri->split);
        }

        g_free (sri);
        node->data = NULL;
    }

    g_list_free (unresolved_splits);
    unresolved_splits = NULL;

    /* ------------------------------------------------- */
    /* destroy any splits that the engine has that the DB didn't */

    i = 0;
    j = 0;
    engine_splits = xaccTransGetSplitList(trans);
    for (node = engine_splits; node; node = node->next)
    {
        /* if not found, mark for deletion */
        if (NULL == g_list_find (db_splits, node->data))
        {
            delete_splits = g_list_prepend (delete_splits, node->data);
            j++;
        }
        i++;
    }
    PINFO ("%d of %d splits marked for deletion", j, i);

    /* now, delete them ... */
    for (node = delete_splits; node; node = node->next)
    {
        xaccSplitDestroy ((Split *) node->data);
    }
    g_list_free (delete_splits);
    g_list_free (db_splits);

}

int
pgendCopyTransactionToEngine (PGBackend *be, const GUID *trans_guid)
{
    char *pbuff;
    Transaction *trans;
    PGresult *result;
    gboolean do_set_guid = FALSE;
    int engine_data_is_newer = 0;
    int j;
    GList *node, *engine_splits;
    guint32 s_idata, t_idata;

    ENTER ("be=%p", be);
    if (!be || !trans_guid) return 0;

    /* disable callbacks into the backend, and events to GUI */
    qof_event_suspend();
    pgendDisable(be);

    /* first, see if we already have such a transaction */
    trans = pgendTransLookup (be, trans_guid);
    if (!trans)
    {
        trans = xaccMallocTransaction(be->book);
        do_set_guid = TRUE;
        engine_data_is_newer = -1;
    }
    else
    {
        /* save some performance, don't go to the
           backend if the data is recent. */
        if (MAX_VERSION_AGE >= be->version_check - qof_instance_get_version_check(trans))
        {
            PINFO ("fresh data, skip check");
            pgendEnable(be);
            qof_event_resume();
            return 0;
        }
    }

    /* build the sql query to get the transaction */
    pbuff = be->buff;
    pbuff[0] = 0;
    pbuff = stpcpy (pbuff, "SELECT * FROM gncTransaction WHERE transGuid='");
    pbuff = guid_to_string_buff(trans_guid, pbuff);
    pbuff = stpcpy (pbuff, "';");

    EXEC_QUERY (be->connection, be->buff, result);
    if (!result)
        return 0;

    {
        int ncols = PQnfields (result);
        int nrows = PQntuples (result);

        PINFO ("query result has %d rows and %d cols", nrows, ncols);

        j = 0;
        if (0 == nrows)
        {
            PQclear (result);
            /* I beleive its a programming error to get this case.
             * Print a warning for now... */
            PERR ("no such transaction in the database. This is unexpected ...\n");
            qof_backend_set_error (&be->be, ERR_SQL_MISSING_DATA);
            pgendEnable(be);
            qof_event_resume();
            return 0;
        }

        if (1 < nrows)
        {
            /* since the guid is primary key, this error is totally
             * and completely impossible, theoretically ... */
            PERR ("!!!!!!!!!!!SQL database is corrupt!!!!!!!\n"
                  "too many transactions with GUID=%s\n",
                  guid_to_string (trans_guid));
            qof_backend_set_error (&be->be, ERR_BACKEND_DATA_CORRUPT);
            pgendEnable(be);
            qof_event_resume();
            return 0;
        }

        /* First order of business is to determine whose data is
         * newer: the engine cache, or the database.  If the
         * database has newer stuff, we update the engine. If the
         * engine is equal or newer, we do nothing in this routine.
         * Of course, we know the database has newer data if this
         * transaction doesn't exist in the engine yet.
         */
        if (!do_set_guid)
        {
            gint32 db_version, cache_version;
            db_version = atoi (DB_GET_VAL("version", j));
            cache_version = qof_instance_get_version (trans);
            if (db_version == cache_version)
            {
                engine_data_is_newer = 0;
            }
            else if (db_version < cache_version)
            {
                engine_data_is_newer = +1;
            }
            else
            {
                engine_data_is_newer = -1;
            }
        }

        /* if the DB data is newer, copy it to engine */
        if (0 > engine_data_is_newer)
        {
            Timespec ts;
            gnc_commodity *currency;

            currency = gnc_string_to_commodity (DB_GET_VAL("currency", j), be->book);
            if (!currency)
            {
                pgendGetCommodity (be, DB_GET_VAL("currency", j));
                currency = gnc_string_to_commodity (DB_GET_VAL("currency", j),
                                                    be->book);
            }

            if (!currency)
            {
                PERR ("currency not found: %s", DB_GET_VAL("currency", j));
            }

            xaccTransBeginEdit (trans);
            if (do_set_guid) xaccTransSetGUID (trans, trans_guid);
            xaccTransSetNum (trans, DB_GET_VAL("num", j));
            xaccTransSetDescription (trans, DB_GET_VAL("description", j));
            ts = gnc_iso8601_to_timespec_gmt (DB_GET_VAL("date_posted", j));
            xaccTransSetDatePostedTS (trans, &ts);
            ts = gnc_iso8601_to_timespec_gmt (DB_GET_VAL("date_entered", j));
            xaccTransSetDateEnteredTS (trans, &ts);
            qof_instance_set_version (trans, atoi(DB_GET_VAL("version", j)));
            xaccTransSetCurrency (trans, currency);
            qof_instance_set_idata(trans, atoi(DB_GET_VAL("iguid", j)));
        }
    }

    PQclear (result);

    /* set timestamp as 'recent' for this data */
    qof_instance_set_version_check(trans, be->version_check);

    /* if engine data was newer, we are done */
    if (0 <= engine_data_is_newer)
    {
        pgendEnable(be);
        qof_event_resume();
        return engine_data_is_newer;
    }

    /* ------------------------------------------------- */
    /* If we are here, then the sql database contains data that is
     * newer than what we have in the engine.  And so, below,
     * we finish the job of yanking data out of the db.
     */
    pgendCopySplitsToEngine (be, trans);

    /* ------------------------------------------------- */
    /* restore any kvp data associated with the transaction and splits */

    t_idata = qof_instance_get_idata(trans);
    if (0 != t_idata)
    {
        if (!kvp_frame_is_empty (trans->inst.kvp_data))
        {
            kvp_frame_delete (trans->inst.kvp_data);
            trans->inst.kvp_data = kvp_frame_new ();
        }

        trans->inst.kvp_data = pgendKVPFetch (be, t_idata, trans->inst.kvp_data);
    }

    engine_splits = xaccTransGetSplitList(trans);
    for (node = engine_splits; node; node = node->next)
    {
        Split *s = node->data;
        s_idata = qof_instance_get_idata(s);
        if (0 != s_idata)
        {
            if (!kvp_frame_is_empty (s->inst.kvp_data))
            {
                kvp_frame_delete (s->inst.kvp_data);
                s->inst.kvp_data = kvp_frame_new ();
            }

            s->inst.kvp_data = pgendKVPFetch (be, s_idata, s->inst.kvp_data);
        }
    }

    /* ------------------------------------------------- */

    xaccTransCommitEdit (trans);

    /* re-enable events to the backend and GUI */
    pgendEnable(be);
    qof_event_resume();

    LEAVE (" ");
    return -1;
}

/* ============================================================= */
/* This routine 'synchronizes' the Transaction structure
 * associated with the GUID.  Data is pulled out of the database,
 * the versions are compared, and updates made, if needed.
 * The splits are handled as well ...
 *
 * hack alert unfinished, incomplete
 * hack alert -- philosophically speaking, not clear that this is the
 * right metaphor.  Its OK to poke date into the engine, but writing
 * data out to the database should make use of versioning, and this
 * routine doesn't.
 *
 * THIS IS NOT USED ANYWHERE should probably go away.  Although
 * this kind of a routine could be handy for resyncing after a lost
 * contact to the backend.  Note, however, that it would
 * mangle balance checkpoints, and these would need to be
 * recomputed.
 */

#if 0

static void
pgendSyncTransaction (PGBackend *be, GUID *trans_guid)
{
    Transaction *trans;
    int engine_data_is_newer = 0;

    ENTER ("be=%p", be);
    if (!be || !trans_guid) return;

    /* disable callbacks into the backend, and events to GUI */
    qof_event_suspend();
    pgendDisable(be);

    engine_data_is_newer = xxxpgendCopyTransactionToEngine (be, trans_guid);

    /* if engine data was newer, we save to the db. */
    if (0 < engine_data_is_newer)
    {
        /* XXX hack alert -- fixme */
        PERR ("Data in the local cache is newer than the data in\n"
              "\tthe database.  Thus, the local data will be sent\n"
              "\tto the database.  This mode of operation is\n"
              "\tguarenteed to clobber other user's updates.\n");

        trans = pgendTransLookup (be, trans_guid);

        /* hack alert -- basically, we should use the pgend_commit_transaction
         * routine instead, and in fact, 'StoreTransaction'
         * pretty much shouldn't be allowed to exist in this
         * framework */
        pgendStoreTransaction (be, trans);

        qof_event_resume();
        return;
    }

    /* re-enable events to the backend and GUI */
    pgendEnable(be);
    qof_event_resume();

    LEAVE (" ");
}

#endif


/* ============================================================= */
/* ============================================================= */
/*         HIGHER LEVEL ROUTINES AND BACKEND PROPER              */
/* ============================================================= */
/* ============================================================= */


/* ============================================================= */

void
pgend_trans_commit_edit (QofBackend * bend,
                         Transaction * trans,
                         Transaction * oldtrans)
{
    char * bufp;
    int rollback = 0;
    PGBackend *be = (PGBackend *)bend;

    ENTER ("be=%p, trans=%p", be, trans);
    if (!be || !trans) return;

    /* lock it up so that we query and store atomically */
    bufp = "BEGIN;\n"
           "LOCK TABLE gncTransaction IN EXCLUSIVE MODE;\n"
           "LOCK TABLE gncSplit IN EXCLUSIVE MODE;\n";
    SEND_QUERY (be, bufp, );
    FINISH_QUERY(be->connection);

    /* Check to see if this is a 'new' transaction, or not.
     * The hallmark of a 'new' transaction is that all the
     * fields are empty.  If its new, then we just go ahead
     * and commit.  If its old, then we need some consistency
     * checks.
     */
    if (FALSE == is_trans_empty (oldtrans))
    {
        /* See if the database is in the state that we last left it in.
         * Basically, the database should contain the 'old transaction'.
         * If it doesn't, then someone else has modified this transaction,
         * and thus, any further action on our part would be unsafe.  It
         * is recommended that this be spit back at the GUI, and let a
         * human decide what to do next.
         *
         * We could directly compare all of the data ... but instead,
         * its more efficient to just compare the version number.
         */

#ifdef COMPARE_ALL_TRANSACTION_DATA
        {
            int ndiffs;
            GList *start, *node;

            ndiffs = pgendCompareOneTransactionOnly (be, oldtrans);
            if (0 < ndiffs) rollback++;

            /* be sure to check the old splits as well ... */
            start = xaccTransGetSplitList (oldtrans);
            for (node = start; node; node = node->next)
            {
                Split * s = node->data;
                ndiffs = pgendCompareOneSplitOnly (be, s);
                if (0 < ndiffs) rollback++;
            }
        }
#else
        /* roll things back is sql version is newer */
        if (0 < pgendTransactionCompareVersion (be, oldtrans))
        {
            rollback = 1;
        }

        /* first, see if someone else has already deleted this transaction */
        if (-1 < pgendTransactionGetDeletedVersion (be, oldtrans))
        {
            if (rollback)
            {
                /* Although this situation should never happen, we'll try
                 * to gracefully handle it anyway, because otherwuise the
                 * transaction becomes un-modifiable, undeleteable.
                 * (This situation might occur with the right combo of bugs
                 * and crashes. We've fixed the bugs, but ...
                 */
                char buf[80];
                gnc_timespec_to_iso8601_buff (xaccTransRetDatePostedTS (trans),
                                              buf);

                PERR ("The impossible has happened, and thats not good!\n"
                      "\tThe SQL database contains an active transaction that\n"
                      "\talso appears in the audit trail as deleted !!\n"
                      "\tWill try to delete transaction for good\n"
                      "\ttransaction is '%s' %s\n",
                      xaccTransGetDescription (trans), buf);
                rollback = 0;
                qof_instance_set_destroying(trans, TRUE);
            }
            else
            {
                rollback = 1;
            }
        }
#endif

        if (rollback)
        {
            bufp = "ROLLBACK;";
            SEND_QUERY (be, bufp,);
            FINISH_QUERY(be->connection);

            PINFO ("old transaction didn't match DB, edit rolled back)\n");

            /* What happens here:  We return to the engine with an
             * error code.  This causes the engine to call
             * xaccTransRollback(), with then invokes our backend rollback
             * routine.  Our rollback routine updates from the latest in
             * the sql database, and voila! we are good to go.
             */
            qof_backend_set_error (&be->be, ERR_BACKEND_MODIFIED);
            return;
        }
    }

    /* if we are here, we are good to go */
    pgendStoreTransactionNoLock (be, trans, FALSE);

    bufp = "COMMIT;\n"
           "NOTIFY gncTransaction;";
    SEND_QUERY (be, bufp,);
    FINISH_QUERY(be->connection);

    /* If this is the multi-user mode, we need to update the
     * balances as well.  */
    if ((MODE_POLL == be->session_mode) ||
            (MODE_EVENT == be->session_mode))
    {
        GList *node;

        /* loop over the old accounts, as they used to be. */
        for (node = xaccTransGetSplitList(trans->orig); node; node = node->next)
        {
            Split *s = (Split *) node->data;
            Account *acc = xaccSplitGetAccount (s);
            pgendAccountRecomputeOneCheckpoint (be, acc,
                                                trans->orig->date_posted);
        }

        /* set checkpoints for the new accounts */
        pgendTransactionRecomputeCheckpoints (be, trans);
    }

    LEAVE ("commited");
    return;
}

/* ============================================================= */
/* transaction rollback routine.  This routine can be invoked
 * in one of two ways: if the user canceled an edited transaction
 * by hand, from the gui, or automatically, due to a multi-user
 * edit conflict.  In this latter case, the commit_edit routine
 * above failed, and returned to the engine.  Then the engine
 * xaccTransRollback routine got invoked, which called us.
 * What we do here is to copy the transaction out of the dataabse
 * and into the engine.  This will bring the local engine up
 * to sync from the changes that other users had made.
 */

void
pgend_trans_rollback_edit (QofBackend * bend, Transaction * trans)
{
    PGBackend *be = (PGBackend *)bend;
    const GUID * trans_guid;

    if (!be || !trans) return;
    ENTER ("be=%p, trans=%p", be, trans);

    /* First, lets see if the other user had deleted this transaction.
     * If so, then we want to delete it from the local cache as well.
     */
    if (-1 < pgendTransactionGetDeletedVersion (be, trans))
    {
        LEAVE ("destroyed");
        qof_backend_set_error (&be->be, ERR_BACKEND_MOD_DESTROY);
        return;
    }

    trans_guid = xaccTransGetGUID (trans);
    pgendCopyTransactionToEngine (be, trans_guid);

    LEAVE ("rolled back");
    return;
}

/* ======================== END OF FILE ======================== */
