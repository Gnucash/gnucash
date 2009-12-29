/********************************************************************\
 * txnmass.c -- implements mass transaction fetch                   *
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <libpq-fe.h>

#include "Account.h"
#include "AccountP.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "Transaction.h"
#include "TransactionP.h"

#include "checkpoint.h"
#include "kvp-sql.h"
#include "PostgresBackend.h"
#include "txnmass.h"

#include "putil.h"

static QofLogModule log_module = GNC_MOD_TXN;

/* ============================================================= */

static gpointer
get_mass_trans_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    QofBook *book = data;
    GList *xaction_list = be->tmp_return;
    Transaction *trans;
    gnc_commodity *currency = NULL;
    Timespec ts;
    GUID trans_guid;

    FIND_BOOK (book);

    /* first, see if we already have such a transaction */
    string_to_guid (DB_GET_VAL("transGUID", j), &trans_guid);
    trans = xaccTransLookup (&trans_guid, book);
    if (trans)
    {
        /* If transaction already exists, determine whose data is
         * newer: the engine cache, or the database.  If the
         * engine has newer stuff, ignore the database contents.
         */

        gint32 db_version, cache_version;
        db_version = atoi (DB_GET_VAL("version", j));
        cache_version = qof_instance_get_version (trans);
        if (db_version < cache_version)
        {
            xaccTransBeginEdit (trans);
            xaction_list = g_list_prepend (xaction_list, trans);
            be->tmp_return = xaction_list;
            return data;
        }
        xaccTransBeginEdit (trans);
    }
    else
    {
        trans = xaccMallocTransaction(book);
        xaccTransBeginEdit (trans);
        xaccTransSetGUID (trans, &trans_guid);
    }

    xaccTransSetNum (trans, DB_GET_VAL("num", j));
    xaccTransSetDescription (trans, DB_GET_VAL("description", j));
    ts = gnc_iso8601_to_timespec_gmt (DB_GET_VAL("date_posted", j));
    xaccTransSetDatePostedTS (trans, &ts);
    ts = gnc_iso8601_to_timespec_gmt (DB_GET_VAL("date_entered", j));
    xaccTransSetDateEnteredTS (trans, &ts);
    qof_instance_set_version (trans, atoi(DB_GET_VAL("version", j)));
    qof_instance_set_idata(trans, atoi(DB_GET_VAL("iguid", j)));

    currency = gnc_string_to_commodity (DB_GET_VAL("currency", j), book);

    xaccTransSetCurrency (trans, currency);

    /* set timestamp as 'recent' for this data */
    qof_instance_set_version_check(trans, be->version_check);

    xaction_list = g_list_prepend (xaction_list, trans);

    be->tmp_return = xaction_list;
    return data;
}

/* ============================================================= */

static gpointer
get_mass_entry_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    QofBook *book = data;
    Transaction *trans = NULL;
    Account *acc;
    Split *s;
    GUID guid;
    Timespec ts;
    gnc_commodity *modity;
    gint64 acct_frac;
    gint64 num;
    gnc_numeric value, amount;
    gint64 trans_frac = 0;

    FIND_BOOK (book);

    /* --------------------------------------------- */
    PINFO ("split GUID=%s", DB_GET_VAL("splitGuid", j));
    guid = nullguid;  /* just in case the read fails ... */
    string_to_guid (DB_GET_VAL("splitGuid", j), &guid);
    s = xaccSplitLookup (&guid, book);
    if (!s)
    {
        s = xaccMallocSplit(book);
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

    guid = nullguid;  /* just in case the read fails ... */
    string_to_guid (DB_GET_VAL("transGUID", j), &guid);
    trans = xaccTransLookup (&guid, book);
    if (!trans)
    {
        PERR ("trans not found, will delete this split\n"
              "\t(split with  guid=%s\n"
              "\twants a trans with guid=%s\n"
              "\tin book with guid=%s)\n",
              DB_GET_VAL("splitGuid", j),
              DB_GET_VAL("transGUID", j),
              DB_GET_VAL("bookGUID", j)
             );
        xaccSplitDestroy (s);
        return data;
    }

    xaccTransAppendSplit (trans, s);

    /* --------------------------------------------- */
    /* next, find the account that this split goes into */
    guid = nullguid;  /* just in case the read fails ... */
    string_to_guid (DB_GET_VAL("accountGUID", j), &guid);
    acc = xaccAccountLookup (&guid, book);
    if (!acc)
    {
        PERR ("account not found, will delete this split\n"
              "\t(split with  guid=%s\n"
              "\twants an acct with guid=%s\n"
              "\tin book with guid=%s)\n",
              DB_GET_VAL("splitGuid", j),
              DB_GET_VAL("accountGUID", j),
              DB_GET_VAL("bookGUID", j)
             );
        xaccSplitDestroy (s);
        return data;
    }

    /* We must set value after split has been inserted into account,
     * since engine references the account SCU to set the value. */
    xaccAccountInsertSplit(acc, s);

    /* we don't know the fraction until after we inserted into the account */
    num = strtoll (DB_GET_VAL("amount", j), NULL, 0);
    modity = xaccAccountGetCommodity (acc);
    acct_frac = gnc_commodity_get_fraction (modity);
    amount = gnc_numeric_create (num, acct_frac);
    xaccSplitSetAmount (s, amount);

    num = strtoll (DB_GET_VAL("value", j), NULL, 0);
    trans_frac = gnc_commodity_get_fraction (xaccTransGetCurrency(trans));
    value = gnc_numeric_create (num, trans_frac);
    xaccSplitSetValue (s, value);

    return data;
}

/* ============================================================= */

void
pgendGetMassTransactions (PGBackend *be, QofBook *book)
{
    char *p, buff[900];
    GList *node, *xaction_list = NULL;
    Account *root;
    guint32 t_idata, s_idata;

    qof_event_suspend();
    pgendDisable(be);

    /* design note: someday, we might get a performance boost by adding
     * a bookguid to the transaction table */
    p = buff;
    p = stpcpy (p, "SELECT DISTINCT gncTransaction.*, gncAccount.bookGuid as bookGuid "
                " FROM gncTransaction, gncSplit, gncAccount "
                " WHERE gncTransaction.transGuid = gncSplit.transGuid AND "
                " gncSplit.accountGuid = gncAccount.accountGuid AND "
                " gncAccount.bookGuid = '");
    p = guid_to_string_buff(qof_book_get_guid (book), p);
    p = stpcpy (p, "';");
    SEND_QUERY (be, buff, );

    /* restore the transactions */
    root = gnc_book_get_root_account (book);
    xaccAccountBeginEdit (root);

    be->tmp_return = NULL;
    pgendGetResults (be, get_mass_trans_cb, book);
    xaction_list = be->tmp_return;

    p = buff;
    p = stpcpy (p, "SELECT gncSplit.*, gncAccount.bookGuid as bookGuid "
                " FROM gncSplit, gncAccount "
                " WHERE gncSplit.accountGuid = gncAccount.accountGuid AND "
                " gncAccount.bookGuid = '");
    p = guid_to_string_buff(qof_book_get_guid (book), p);
    p = stpcpy (p, "';");
    SEND_QUERY (be, buff, );
    pgendGetResults (be, get_mass_entry_cb, book);

    for (node = xaction_list; node; node = node->next)
    {
        Transaction *trans = (Transaction *)node->data;
        GList *splits, *snode;

        /* ------------------------------------------------- */
        /* Restore any kvp data associated with the transaction and splits.
         * We won't do this en-mass, as there currently seems to be no
         * performance advantage to doing so */

        t_idata = qof_instance_get_idata(trans);
        if (t_idata)
        {
            trans->inst.kvp_data = pgendKVPFetch (be, t_idata, trans->inst.kvp_data);
        }

        splits = xaccTransGetSplitList(trans);
        for (snode = splits; snode; snode = snode->next)
        {
            Split *s = snode->data;
            s_idata = qof_instance_get_idata(s);
            if (s_idata)
            {
                s->inst.kvp_data = pgendKVPFetch (be, s_idata, s->inst.kvp_data);
            }
        }

        /* ------------------------------------------------- */
        xaccTransCommitEdit (trans);
    }
    g_list_free(xaction_list);

    xaccAccountCommitEdit (root);

    pgendEnable(be);
    qof_event_resume();
}

/* ======================== END OF FILE ======================== */
