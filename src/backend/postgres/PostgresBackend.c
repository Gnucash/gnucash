/********************************************************************\
 * PostgresBackend.c -- implements postgres backend - main file     *
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
#include <glib/gi18n.h>
#include <ctype.h>
#include <netdb.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#if HAVE_LANGINFO_CODESET
#include <langinfo.h>
#endif
#include <libpq-fe.h>

#include "AccountP.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-pricedb.h"
#include "gnc-pricedb-p.h"
#include "TransactionP.h"

#include "account.h"
#include "book.h"
#include "builder.h"
#include "checkpoint.h"
#include "events.h"
#include "gncquery.h"
#include "kvp-sql.h"
#include "PostgresBackend.h"
#include "price.h"
#include "txn.h"
#include "txnmass.h"
#include "upgrade.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

#include "putil.h"

static void pgendInit (PGBackend *be);

static const char * pgendSessionGetMode (PGBackend *be);

GUID nullguid;

/* hack alert -- this is the query buffer size, it can be overflowed.
 * Ideally, its dynamically resized.  On the other hand, Postgres
 * rejects queries longer than 8192 bytes, (according to the
 * documentation) so there's not much point in getting fancy ...
 */
#define QBUFSIZE 16350

/* ============================================================= */
/* misc bogus utility routines */

static char *
pgendGetHostname (PGBackend *be)
{
    char * p;

    p = be->buff;
    *p = 0;
    if (0 == gethostname (p, QBUFSIZE / 3))
    {
        extern int h_errno;
        struct hostent *hent;

        hent = gethostbyname (be->buff);
        if (hent)
        {
            strcpy (be->buff, hent->h_name);
        }
        else
        {
            PERR ("can't get domainname: %s", hstrerror(h_errno));
        }
    }
    else
    {
        *p = 0;
        PERR ("can't get hostname");
    }

    return be->buff;
}

static char *
pgendGetUsername (PGBackend *be)
{
    uid_t uid = getuid();
    struct passwd *pw = getpwuid (uid);
    if (pw) return (pw->pw_name);
    return NULL;
}

static char *
pgendGetUserGecos (PGBackend *be)
{
    uid_t uid = getuid();
    struct passwd *pw = getpwuid (uid);
    if (pw) return (pw->pw_gecos);
    return NULL;
}

/* ============================================================= */

Account *
pgendAccountLookup (PGBackend *be, const GUID *acct_guid)
{
    GList *node;
    Account * acc = NULL;

    ENTER("guid = %s", guid_to_string(acct_guid));
    for (node = be->blist; node; node = node->next)
    {
        QofBook *book = node->data;
        acc = xaccAccountLookup (acct_guid, book);
        if (acc)
        {
            LEAVE("acc = %p", acc);
            return acc;
        }
    }

    LEAVE("acc = (null)");
    return NULL;
}

Transaction *
pgendTransLookup (PGBackend *be, const GUID *txn_guid)
{
    GList *node;
    Transaction * txn = NULL;

    ENTER("guid = %s", guid_to_string(txn_guid));
    for (node = be->blist; node; node = node->next)
    {
        QofBook *book = node->data;
        txn = xaccTransLookup (txn_guid, book);
        if (txn)
        {
            LEAVE("txt = %p", txn);
            return txn;
        }
    }

    LEAVE("txn = (null");
    return NULL;
}

Split *
pgendSplitLookup (PGBackend *be, const GUID *split_guid)
{
    GList *node;
    Split * split = NULL;

    ENTER("guid = %s", guid_to_string(split_guid));
    for (node = be->blist; node; node = node->next)
    {
        QofBook *book = node->data;
        split = xaccSplitLookup (split_guid, book);
        if (split)
        {
            LEAVE("split = %p", split);
            return split;
        }
    }

    LEAVE("split = (null)");
    return NULL;
}

struct _iter
{
    const GUID *guid;
    QofInstance *ent;
};

static void
cforeach (QofCollection *col, gpointer data)
{
    struct _iter *iter = data;
    if (iter->ent) return;
    iter->ent = qof_collection_lookup_entity (col, iter->guid);
}

QofIdType
pgendGUIDType (PGBackend *be, const GUID *guid)
{
    GList *node;
    struct _iter iter;

    iter.guid = guid;
    iter.ent = NULL;

    ENTER("guid = %s", guid_to_string(guid));
    for (node = be->blist; node; node = node->next)
    {
        QofBook *book = node->data;
        qof_book_foreach_collection (book, cforeach, &iter);
        if (iter.ent)
        {
            LEAVE("tip = %s", iter.ent->e_type);
            return iter.ent->e_type;
        }
    }

    LEAVE("tip = NULL");
    return GNC_ID_NONE;
}

/* ============================================================= */

QofBook *
pgendGetBook(PGBackend *pbe)
{
    QofBook *book;

    ENTER(" ");
    book = qof_session_get_book(pbe->session);

    LEAVE("book = %p", book);
    return book;
}

static void
pgend_set_book (PGBackend *be, QofBook *book)
{
    GList *node;
    be->book = book;

    for (node = be->blist; node; node = node->next)
    {
        if (book == node->data) return;
    }

    be->blist = g_list_prepend (be->blist, book);
}

/* ============================================================= */
/* This routine finds the commodity by parsing a string
 * of the form NAMESPACE::MNEMONIC
 */
/* FIXME: replace w/ gnc_commodity_table_lookup_unique */
gnc_commodity *
gnc_string_to_commodity (const char *str, QofBook *book)
{
    gnc_commodity_table *comtab;
    gnc_commodity *com;
    char *space, *name;

    comtab = gnc_book_get_commodity_table (book);

    space = g_strdup(str);
    name = strchr (space, ':'); /* BUG */

    if (!name)
    {
        PERR ("bad commodity string: %s", str ? str : "(null)");
        g_free (space);
        return NULL;
    }

    *name = 0;
    name += 2;

    com = gnc_commodity_table_lookup(comtab, space, name);
    g_free (space);
    return com;
}

/* ============================================================= */
/* send the query, process the results */

gpointer
pgendGetResults (PGBackend *be,
                 gpointer (*handler) (PGBackend *, PGresult *, int, gpointer),
                 gpointer data)
{
    PGresult *result;
    int i = 0;
    be->nrows = 0;
    do
    {
        GET_RESULTS (be->connection, result);
        {
            int j, jrows;
            int ncols = PQnfields (result);
            jrows = PQntuples (result);
            be->nrows += jrows;
            PINFO ("query result %d has %d rows and %d cols",
                   i, jrows, ncols);

            for (j = 0; j < jrows; j++)
            {
                data = handler (be, result, j, data);
            }
        }
        i++;
        PQclear (result);
    }
    while (result);

    return data;
}

/* ============================================================= */
/* version number callback for pgendGetResults */

static gpointer
get_version_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    int version = atoi(DB_GET_VAL ("version", j));
    int incoming = GPOINTER_TO_INT (data);
    if (version < incoming) version = incoming;
    return GINT_TO_POINTER (version);
}

/* ============================================================= */
/* include the auto-generated code */

#include "base-autogen.h"
#include "base-autogen.c"

static const char *table_audit_str =
#include "table-audit.c"
    ;

static const char *table_create_str =
#include "table-create.c"
    ;

static const char *table_version_str =
#include "table-version.c"
    ;

static const char *sql_functions_str =
#include "functions.c"
    ;

#if 0
static const char *table_drop_str =
#include "table-drop.c"
    ;
#endif

/* ============================================================= */
/*             QUERY STUFF                                       */
/* ============================================================= */
/* The pgendRunQuery() routine performs a search on the SQL database for
 * all of the splits that correspond to gnc-style query, and then
 * integrates them into the engine cache.  It then performs a 'closure'
 * in order to maintain accurate balances.  Warning: this routine
 * is a bit of a pig, and should be replaced with a better algorithm.
 * See below.
 *
 * The problem that this routine is trying to solve is the need to
 * to run a query *and* maintain consistent balance checkpoints
 * within the engine data. As a by-product, it can pull in a vast
 * amount of sql data into the engine.  The steps of the algorithm
 * are:
 *
 * 1) convert the engine style query to an SQL query string.
 * 2) run the SQL query to get the splits that satisfy the query
 * 3) pull the transaction ids out of the matching splits,
 * 4) fetch the corresponding transactions, put them into the engine.
 * 5) get the balance checkpoint with the latest date earlier
 *    than the earliest transaction,
 * 6) get all splits later than the checkpoint start,
 * 7) go to step 3) until a consistent set of transactions
 *    has been pulled into the engine.
 *
 * Note regarding step 4):
 * We only ever pull complete transactions out of the engine,
 * and never dangling splits. This helps make sure that the
 * splits always balance in a transaction; it also allows the
 * ledger to operate in 'journal' mode.
 *
 * Note regarding step 6):
 * During the fill-out up to the checkpoint, new transactions may
 * pulled in.  These splits may link accounts we haven't seen before,
 * which is why we need to go back to step 3.
 *
 * The process may pull in a huge amount of data.
 *
 * Oops: mega-bug: if the checkpoints on all accounts don't share
 * a common set of dates, then the above process will 'walk' until
 * the start of time, essentially pulling *all* data, and not that
 * efficiently, either.  This is a killer bug with this implementation.
 * We can work around it by fixing checkpoints in time ...
 *
 * There are certainly alternate possible implementations.  In one
 * alternate, 'better' implementation, we don't fill out to to the
 * checkpoint for all accounts, but only for the one being displayed.
 * However, doing so would require considerable jiggering in the
 * engine proper, where we'd have to significantly modify
 * RecomputeBalance() to do the 'right thing' when it has access to
 * only some of the splits.   Yow.  Wait til after gnucash-1.6 for
 * this tear-up.
 */

typedef struct
{
    Transaction * trans;
    char * commodity_string;
} TransResolveInfo;

typedef struct
{
    GList * xaction_list;
    GList * resolve_list;
} QueryData;

static gpointer
query_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    QueryData *qd = data;
    GUID trans_guid;
    Transaction *trans;
    gnc_commodity *currency;
    Timespec ts;

    /* find the transaction this goes into */
    trans_guid = nullguid;  /* just in case the read fails ... */
    string_to_guid (DB_GET_VAL("transGUID", j), &trans_guid);

    /* use markers to avoid redundant traversals of transactions we've
     * already checked recently. */
    trans = pgendTransLookup (be, &trans_guid);
    if (NULL != trans)
    {
        if (0 != trans->marker)
        {
            return qd;
        }
        else
        {
            gint32 db_version, cache_version;
            db_version = atoi (DB_GET_VAL("version", j));
            cache_version = qof_instance_get_version (trans);
            if (db_version <= cache_version)
            {
                return qd;
            }
            xaccTransBeginEdit (trans);
        }
    }
    else
    {
        trans = xaccMallocTransaction(pgendGetBook(be));
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

    currency = gnc_string_to_commodity (DB_GET_VAL("currency", j),
                                        pgendGetBook(be));
    if (currency)
        xaccTransSetCurrency (trans, currency);
    else
    {
        TransResolveInfo * ri = g_new0 (TransResolveInfo, 1);

        ri->trans = trans;
        ri->commodity_string = g_strdup (DB_GET_VAL("currency", j));

        qd->resolve_list = g_list_prepend (qd->resolve_list, ri);
    }

    trans->marker = 1;
    qd->xaction_list = g_list_prepend (qd->xaction_list, trans);

    return qd;
}

typedef struct acct_earliest
{
    Account *acct;
    Timespec ts;
} AcctEarliest;

static int ncalls = 0;

static void
pgendFillOutToCheckpoint (PGBackend *be, const char *query_string)
{
    int call_count = ncalls;
    int nact = 0;
    QueryData qd;
    GList *node, *anode, *acct_list = NULL;

    qd.xaction_list = NULL;
    qd.resolve_list = NULL;

    ENTER (" ");
    if (!be)
    {
        LEAVE("");
        return;
    }

    if (0 == ncalls)
    {
        START_CLOCK (9, "starting at level 0");
    }
    else
    {
        REPORT_CLOCK (9, "call count %d", call_count);
    }
    ncalls ++;

    SEND_QUERY (be, query_string, );
    pgendGetResults (be, query_cb, &qd);
    REPORT_CLOCK (9, "fetched results at call %d", call_count);

    for (node = qd.resolve_list; node; node = node->next)
    {
        TransResolveInfo * ri = node->data;
        gnc_commodity * commodity;

        pgendGetCommodity (be, ri->commodity_string);
        commodity = gnc_string_to_commodity (ri->commodity_string,
                                             pgendGetBook(be));

        if (commodity)
        {
            xaccTransSetCurrency (ri->trans, commodity);
        }
        else
        {
            PERR ("Can't find commodity %s", ri->commodity_string);
        }

        g_free (ri->commodity_string);
        ri->commodity_string = NULL;

        g_free (ri);
        node->data = NULL;
    }
    g_list_free (qd.resolve_list);
    qd.resolve_list = NULL;

    /* restore the splits for these transactions */
    for (node = qd.xaction_list; node; node = node->next)
    {
        Transaction *trans = (Transaction *) node->data;

        pgendCopySplitsToEngine (be, trans);
    }

    /* restore any kvp data associated with the transaction and splits */
    for (node = qd.xaction_list; node; node = node->next)
    {
        Transaction *trans = (Transaction *) node->data;
        GList *engine_splits, *snode;

        trans->inst.kvp_data = pgendKVPFetch(be, qof_instance_get_idata(trans),
                                             trans->inst.kvp_data);

        engine_splits = xaccTransGetSplitList(trans);
        for (snode = engine_splits; snode; snode = snode->next)
        {
            Split *s = snode->data;
            s->inst.kvp_data = pgendKVPFetch(be, qof_instance_get_idata(s),
                                             s->inst.kvp_data);
        }

        xaccTransCommitEdit (trans);
    }

    /* run the fill-out algorithm */
    for (node = qd.xaction_list; node; node = node->next)
    {
        Transaction *trans = (Transaction *) node->data;
        GList *split_list, *snode;
        Timespec ts;

        ts = xaccTransRetDatePostedTS (trans);

        /* Back off by a second to disambiguate time.
         * This is safe, because the fill-out will recurse
         * if something got into this one-second gap. */
        ts.tv_sec --;
        split_list = xaccTransGetSplitList (trans);
        for (snode = split_list; snode; snode = snode->next)
        {
            int found = 0;
            Split *s = (Split *) snode->data;
            Account *acc = xaccSplitGetAccount (s);
            GList *splits;

            /* make sure the earliest split is first */
            xaccAccountSortSplits (acc, TRUE);

            splits = xaccAccountGetSplitList (acc);

            /* See if we already have a split (acc_split)
             * earlier than this transaction. Then either:
             *
             *   1) The acc_split was pulled in by the same
             *      query which brought in the current split
             *      and we will get to acc_split later.
             *   2) The acc_split was loaded already and
             *      the account starting balance is correct
             *      up to that point.
             *
             * Either way, we can ignore the current split
             * for the purposes of checkpointing.
             */
            if (splits)
            {
                Split *acc_split = splits->data;
                Transaction *t = xaccSplitGetParent (acc_split);
                Timespec ts_2 = xaccTransRetDatePostedTS (t);

                if (timespec_cmp (&ts_2, &ts) < 0)
                    continue;
            }

            /* lets see if we have a record of this account already */
            for (anode = acct_list; anode; anode = anode->next)
            {
                AcctEarliest * ae = (AcctEarliest *) anode->data;
                if (ae->acct == acc)
                {
                    if (0 > timespec_cmp(&ts, &(ae->ts)))
                    {
                        ae->ts = ts;
                    }
                    found = 1;
                    break;
                }
            }

            /* if not found, make note of this account, and the date */
            if (0 == found)
            {
                AcctEarliest * ae = g_new (AcctEarliest, 1);
                ae->acct = acc;
                ae->ts = ts;
                acct_list = g_list_prepend (acct_list, ae);
            }
        }
    }
    g_list_free (qd.xaction_list);
    qd.xaction_list = NULL;

    REPORT_CLOCK (9, "done gathering at call %d", call_count);
    if (NULL == acct_list)
    {
        LEAVE("");
        return;
    }

    /* OK, at this point, we have a list of accounts, including the
     * date of the earliest split in that account.  Now, we need to
     * do two queries: first, get the running balances to that point,
     * and then all of the splits from that date onwards.
     */
    nact = 0;
    for (anode = acct_list; anode; anode = anode->next)
    {
        char *p;
        AcctEarliest * ae = (AcctEarliest *) anode->data;
        pgendAccountGetBalance (be, ae->acct, ae->ts);

        /* n.b. date_posted compare must be strictly greater than, since the
         * GetBalance goes to less-then-or-equal-to because of the BETWEEN
         * that appears in the gncSubTotalBalance sql function. */
        p = be->buff;
        *p = 0;
        p = stpcpy (p,
                    "SELECT DISTINCT gncTransaction.* "
                    "FROM gncSplit, gncTransaction WHERE "
                    "gncSplit.transGuid = gncTransaction.transGuid AND "
                    "gncSplit.accountGuid='");
        p = guid_to_string_buff(xaccAccountGetGUID(ae->acct), p);
        p = stpcpy (p, "' AND gncTransaction.date_posted > '");
        p = gnc_timespec_to_iso8601_buff (ae->ts, p);
        p = stpcpy (p, "';");

        pgendFillOutToCheckpoint (be, be->buff);

        g_free (ae);
        nact ++;
    }
    g_list_free(acct_list);

    REPORT_CLOCK (9, "done w/ fillout at call %d, handled %d accounts",
                  call_count, nact);
    LEAVE (" ");
}

static gpointer
pgendCompileQuery (QofBackend *bend, Query *q)
{
    return q;
}

static void
pgendFreeQuery (QofBackend *bend, gpointer q)
{
}

static void
pgendRunQuery (QofBackend *bend, gpointer q_p)
{
    PGBackend *be = (PGBackend *)bend;
    Query *q = (Query*) q_p;
    const char * sql_query_string;
    Account *root;
    sqlQuery *sq;

    ENTER ("be=%p, qry=%p", be, q);
    if (!be || !q)
    {
        LEAVE("(null) args");
        return;
    }
    be->version_check = (guint32) time(0);

    qof_event_suspend();
    pgendDisable(be);

    /* first thing we do is convert the gnc-engine query into
     * an sql string. */
    sq = sqlQuery_new();
    sql_query_string = sqlQuery_build (sq, q);

    root = gnc_book_get_root_account(pgendGetBook(be));

    /* stage transactions, save some postgres overhead */
    gnc_account_tree_begin_staged_transaction_traversals (root);

    /* We will be doing a bulk insertion of transactions below.
     * We can gain a tremendous performance improvement,
     * for example, a factor of 10x when querying 3000 transactions,
     * by opening all accounts for editing before we start, and
     * closing them all only after we're done.  This is because
     * an account must be open for editing in order to insert a split,
     * and when the commit is made, the splits are sorted in date order.
     * If we're sloppy, then there's an ordering for every insertion.
     * By defering the Commit, we defer the sort, and thus save gobs.
     * Of course, this hurts 'shallow' queries some, but I beleive
     * by not very much.
     */
    ncalls = 0;
    xaccAccountBeginEdit(root);
    pgendFillOutToCheckpoint (be, sql_query_string);
    xaccAccountCommitEdit(root);
    PINFO ("number of calls to fill out=%d", ncalls);

    sql_Query_destroy(sq);

    pgendEnable(be);
    qof_event_resume();

    LEAVE (" ");
}


/* ============================================================= */
/* The pgendGetAllTransactions() routine sucks *all* of the
 *    transactions out of the database.  This is a potential
 *    CPU and memory-burner; its use is not suggested for anything
 *    but single-user mode.
 *
 *    NB. This routine has been obsoleted by the more efficient
 *    pgendGetMassTransactions().  We keep it around here ...
 *    for a rainy day...
 */

#if 0
static gpointer
get_all_trans_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    GList *xaction_list = (GList *) data;
    GUID *trans_guid;

    /* find the transaction this goes into */
    trans_guid = guid_malloc();
    *trans_guid = nullguid;  /* just in case the read fails ... */
    string_to_guid (DB_GET_VAL("transGUID", j), trans_guid);

    xaction_list = g_list_prepend (xaction_list, trans_guid);
    return xaction_list;
}


static void
pgendGetAllTransactions (PGBackend *be, Account *root)
{
    GList *node, *xaction_list = NULL;

    qof_event_suspend();
    pgendDisable(be);

    SEND_QUERY (be, "SELECT transGuid FROM gncTransaction;", );
    xaction_list = pgendGetResults (be, get_all_trans_cb, xaction_list);

    /* restore the transactions */
    xaccAccountBeginEdit (root);
    for (node = xaction_list; node; node = node->next)
    {
        xxxpgendCopyTransactionToEngine (be, (GUID *)node->data);
        guid_free (node->data);
    }
    g_list_free(xaction_list);
    xaccAccountCommitEdit (root);

    pgendEnable(be);
    qof_event_resume();
}
#endif


/* ============================================================= */
/* ============================================================= */
/*         HIGHER LEVEL ROUTINES AND BACKEND PROPER              */
/* ============================================================= */
/* ============================================================= */


/* ============================================================= */
/* hack alert -- the sane-ness of this algorithm should be reviewed.
 * I can't vouch that there aren't any subtle issues or race conditions
 * lurking in this.  Anyway, with that introduction:
 *
 * The pgendSync() routine 'synchronizes' the accounts & commodities
 * cached in the engine to those in the database.  It does this first
 * by writing out all of the accounts and transactions, from the
 * top-group down, and then re-reading from the database.  This
 * write-then-read cycle has the effect of merging the engine data
 * into the sql database.  Note that version checking is done during
 * the writing: only accounts and transactions that are 'newer' in
 * the engine are written out.  Then during the read cycle, anything
 * in the DB that is newer than what's in the engine is sucked back
 * into the engine.
 *
 * There are three scenarios to contemplate with the update with
 * this 'sync' operation:
 *
 * 1) Database merge:  the user has two substantialy similar copies
 *    of the same data; the first copy was read into the engine earlier,
 *    and now, in this routine, it is being written into the second.
 *    Because the merge uses version numbers, this merge should be
 *    'safe' in that only the newer copy of any account or transaction
 *    is merged.  But this 'safety' can break down, in certain cases;
 *    see below.
 * 1a) Same situation as above, except the 'first' copy is a file
 *    that resulted because the user was kicked off-line (off-network)
 *    and saved the data to a file.  Now, coming back on-line, they
 *    are merging the file data back into the central store.
 *
 * This merge is *not* safe when two different users made a change
 * to the same account or transaction.  This routine does not check
 * for such conflicts or report them.  Hack alert: this is a bug that
 * should be fixed.
 *
 * This routine should also check for deleted transactions (that
 * other users have deleted, but are still present in out cache).
 */


static void
pgendSync (QofBackend *bend, QofBook *book)
{
    PGBackend *be = (PGBackend *)bend;
    Account *root = gnc_book_get_root_account(book);

    ENTER ("be=%p, root=%p", be, root);

    pgend_set_book (be, book);
    be->version_check = (guint32) time(0);

    /* For the multi-user modes, we allow a save only once,
     * when the database is created for the first time.
     * Ditto for the single-user update mode: it should never
     * wander out of sync.
     */
    if ((MODE_SINGLE_FILE != be->session_mode) &&
            (FALSE == be->freshly_created_db))
    {
        LEAVE("no sync");
        return;
    }
    be->freshly_created_db = FALSE;

    pgendStoreBook (be, book);

    /* store the account group hierarchy, and then all transactions */
    pgendStoreAccountTree (be, root);
    pgendStoreAllTransactions (be, root);

    /* don't send events  to GUI, don't accept callbacks to backend */
    qof_event_suspend();
    pgendDisable(be);

    pgendKVPInit(be);
    pgendGetAllAccountsInBook (be, book);
    if ((MODE_SINGLE_FILE != be->session_mode) &&
            (MODE_SINGLE_UPDATE != be->session_mode))
    {
        Timespec ts = gnc_iso8601_to_timespec_gmt (CK_BEFORE_LAST_DATE);
        pgendAccountTreeGetAllBalances (be, root, ts);
    }
    else
    {
        /* in single user mode, read all the transactions */
        pgendGetMassTransactions (be, book);
    }

    /* hack alert -- In some deranged theory, we should be
     * syncing prices here, as well as syncing any/all other
     * engine structures that need to be stored.  But instead,
     * price sync is handled as a separate routine ...
     */

    /* re-enable events */
    pgendEnable(be);
    qof_event_resume();

    LEAVE(" ");
}

/* ============================================================= */
/* The pgendSyncSingleFile() routine syncs the engine and database.
 *    In single file mode, we treat 'sync' as 'file save'.
 *    We start by deleting *everything*, and then writing
 *    everything out.  This is rather nasty, ugly and dangerous,
 *    but that's the nature of single-file mode.  Note: we
 *    have to delete everything because in this mode, there is
 *    no other way of finding out that an account, transaction
 *    or split was deleted. i.e. there's no other way to delete.
 *    So start with a clean slate.
 *
 *    The use of this routine/this mode is 'depricated'.
 *    Its handy for testing, sanity-checking, and as a failsafe,
 *    but its use shouldn't be encouraged.
 */

static int
trans_traverse_cb (Transaction *trans, void *cb_data)
{
    pgendStoreTransactionNoLock ((PGBackend *) cb_data, trans, TRUE);
    return 0;
}

static void
pgendSyncSingleFile (QofBackend *bend, QofBook *book)
{
    char book_guid[40];
    char buff[4000];
    char *p;
    PGBackend *be = (PGBackend *)bend;
    Account *root = gnc_book_get_root_account(book);

    ENTER ("be=%p, root=%p", be, root);

    pgend_set_book (be, book);

    /* hack alert -- we shouldn't be doing a global delete,
     * we should only be deleting the stuff that's in this particular
     * book .... */
    p = "BEGIN;\n"
        "LOCK TABLE gncBook IN EXCLUSIVE MODE;\n"
        "LOCK TABLE gncAccount IN EXCLUSIVE MODE;\n"
        "LOCK TABLE gncCommodity IN EXCLUSIVE MODE;\n"
        "LOCK TABLE gncTransaction IN EXCLUSIVE MODE;\n"
        "LOCK TABLE gncSplit IN EXCLUSIVE MODE;\n";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    guid_to_string_buff (qof_book_get_guid(book), book_guid);

    /* First, we delete all of the accounts, splits and transactions
     * associated with this book.  Its very tempting to just delete
     * everything in the SQL db, but we note that there may be several
     * books stored here, and we want to delete only one book.
     */
    /* do the one-book equivalent of "DELETE FROM gncTransaction;" */
    p = buff;
    p = stpcpy (p, "DELETE FROM gncTransaction WHERE "
                "  gncTransaction.transGuid = gncSplit.transGuid AND "
                "  gncSplit.accountGuid = gncAccount.accountGuid AND "
                "  gncAccount.bookGuid = '");
    p = stpcpy (p, book_guid);
    p = stpcpy (p, "';");
    SEND_QUERY (be, buff, );
    FINISH_QUERY(be->connection);

    /* do the one-book equivalent of "DELETE FROM gncSplit;" */
    p = buff;
    p = stpcpy (p, "DELETE FROM gncSplit WHERE "
                "  gncSplit.accountGuid = gncAccount.accountGuid AND "
                "  gncAccount.bookGuid = '");
    p = stpcpy (p, book_guid);
    p = stpcpy (p, "';");
    SEND_QUERY (be, buff, );
    FINISH_QUERY(be->connection);


    /* do the one-book equivalent of "DELETE FROM gncAccount;" */
    p = buff;
    p = stpcpy (p, "DELETE FROM gncAccount WHERE bookGuid='");
    p = stpcpy (p, book_guid);
    p = stpcpy (p, "';");
    SEND_QUERY (be, buff, );
    FINISH_QUERY(be->connection);

    /* store the book struct */
    pgendStoreBookNoLock (be, book, TRUE);

    /* Store accounts and commodities */
    xaccClearMarkDown (root, 0);
    pgendStoreAccountTreeNoLock (be, root, TRUE, TRUE);
    xaccClearMarkDown (root, 0);

    /* Recursively walk transactions. Start by reseting the write
     * flags. We use this to avoid infinite recursion */
    gnc_account_tree_begin_staged_transaction_traversals(root);
    gnc_account_tree_staged_transaction_traversal (root, 1, trans_traverse_cb, be);

    /* hack alert -- In some deranged theory, we should be
     * syncing prices here, as well as syncing any/all other
     * engine structures that need to be stored.  But instead,
     * price sync is handled as a separate routine ...
     */


    p = "COMMIT;";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    LEAVE(" ");
}

/* ============================================================= */
/* Please read the comment for pgendSync to truly understand
 * how this routine works.  Its somewhat subtle.
 */

static void
pgendSyncPriceDB (QofBackend *bend, QofBook *book)
{
    PGBackend *be = (PGBackend *)bend;
    ENTER ("be=%p", be);
    if (!be) return;

    pgend_set_book (be, book);
    be->version_check = (guint32) time(0);

    /* for the multi-user modes, we allow a save only once,
     * when the database is created for the first time */
    if ((MODE_SINGLE_FILE != be->session_mode) &&
            (MODE_SINGLE_UPDATE != be->session_mode) &&
            (FALSE == be->freshly_created_prdb))
    {
        LEAVE("no sync");
        return;
    }
    be->freshly_created_prdb = FALSE;

    pgendStorePriceDB (be, book);

    /* don't send events  to GUI, don't accept callbacks to backend */
    qof_event_suspend();
    pgendDisable(be);

    pgendGetAllPricesInBook (be, book);

    /* re-enable events */
    pgendEnable(be);
    qof_event_resume();

    LEAVE(" ");
}

/* ============================================================= */
/* The pgendSyncPriceSingleFile() routine syncs the prices in the
 *    engine with the database.
 *    In single file mode, we treat 'sync' as 'file save'.
 *    We start by deleting *everything*, and then writing
 *    everything out.  This is rather nasty, ugly and dangerous,
 *    but that's the nature of single-file mode.  Note: we
 *    have to delete everything because in this mode, there is
 *    no other way of finding out that a price was deleted.
 *    i.e. there's no other way to delete.
 *    So start with a clean slate.
 *
 *    The use of this routine/this mode is 'depricated'.
 *    Its handy for testing, sanity-checking, and as a failsafe,
 *    but its use shouldn't be encouraged.
 */

static void
pgendSyncPriceDBSingleFile (QofBackend *bend, QofBook *book)
{
    char buff[400], *p;
    PGBackend *be = (PGBackend *)bend;
    ENTER ("be=%p", be);

    pgend_set_book (be, book);

    p = buff;
    p = stpcpy (p, "BEGIN;\n"
                "LOCK TABLE gncPrice IN EXCLUSIVE MODE;\n"
                "DELETE FROM gncPrice WHERE bookGuid='\n");
    p = guid_to_string_buff (qof_book_get_guid(book), p);
    p = stpcpy (p, "';");
    SEND_QUERY (be, buff, );
    FINISH_QUERY(be->connection);

    /* Store accounts and commodities */
    pgendStorePriceDBNoLock (be, book);

    p = "COMMIT;";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    LEAVE(" ");
}

/* ============================================================= */

static const char *
pgendSessionGetMode (PGBackend *be)
{
    switch (be->session_mode)
    {
    case MODE_SINGLE_FILE:
        return "SINGLE-FILE";
    case MODE_SINGLE_UPDATE:
        return "SINGLE-UPDATE";
    case MODE_POLL:
        return "POLL";
    case MODE_EVENT:
        return "EVENT";
        /* quiet compiler warnings about MODE_NONE */
    default:
        return "ERROR";
    }
    return "ERROR";
}

/* ============================================================= */
/* Instead of loading the book, just set the lock error */

static void
pgend_book_load_single_lockerr (QofBackend *bend, QofBook *book)
{
    PGBackend *be = (PGBackend *)bend;

    if (!be) return;

    qof_backend_set_error (&be->be, ERR_BACKEND_LOCKED);
}

/* ============================================================= */
/* The get_session_cb() routine can determine whether we can start
 *    a session of the desired type.
 *    The logic used is as follows:
 *    -- if there is any (other) session at all, and we want single
 *       (exclusive) access, then fail.
 *    -- if we want any kind of session, and there is a single
 *       (exclusive) session going, then fail.
 *    -- otherwise, suceed.
 *    Return TRUE if we can get a session.
 *
 *    This routine does not lock, but may be used inside a
 *    test-n-set atomic operation.
 */

static gpointer
get_session_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    char *lock_holder = (char *) data;
    char *mode = DB_GET_VAL("session_mode", j);

    if ((MODE_SINGLE_FILE == be->session_mode) ||
            (MODE_SINGLE_UPDATE == be->session_mode) ||
            (0 == strcasecmp (mode, "SINGLE-FILE")) ||
            (0 == strcasecmp (mode, "SINGLE-UPDATE")))
    {
        char * hostname = DB_GET_VAL("hostname", j);
        char * username = DB_GET_VAL("login_name", j);
        char * gecos = DB_GET_VAL("gecos", j);
        char * datestr = DB_GET_VAL("time_on", j);

        PWARN ("This database is already opened by \n"
               "\t%s@%s (%s) in mode %s on %s \n",
               username ? username : "(null)",
               hostname ? hostname : "(null)",
               gecos ? gecos : "(null)",
               mode ? mode : "(null)",
               datestr ? datestr : "(null)");

        PWARN ("The above messages should be handled by the GUI\n");

        if (lock_holder) return lock_holder;
        lock_holder = g_strdup (DB_GET_VAL("sessionGUID", j));
    }
    return lock_holder;
}

static gboolean
pgendSessionCanStart (PGBackend *be, int break_lock)
{
    gboolean retval = TRUE;
    char *p, *lock_holder;

    ENTER (" ");
    /* Find out if there are any open sessions.
     * If 'time_off' is infinity, then user hasn't logged off yet  */
    p = "SELECT * FROM gncSession "
        "WHERE time_off='INFINITY';";
    SEND_QUERY (be, p, FALSE);
    lock_holder = pgendGetResults (be, get_session_cb, NULL);

    if (lock_holder) retval = FALSE;

    /* If just one other user has a lock, then will go ahead and
     * break the lock... If the user approved.  I don't like this
     * but that's what the GUI is set up to do ...
     */
    PINFO ("break_lock=%d nrows=%d lock_holder=%s\n",
           break_lock, be->nrows,
           lock_holder ? lock_holder : "(null)");
    if (break_lock && (1 == be->nrows) && lock_holder)
    {
        p = be->buff;
        *p = 0;
        p = stpcpy (p, "UPDATE gncSession SET time_off='NOW' "
                    "WHERE sessionGuid='");
        p = stpcpy (p, lock_holder);
        p = stpcpy (p, "';");

        SEND_QUERY (be, be->buff, retval);
        FINISH_QUERY(be->connection);
        retval = TRUE;
    }

    if (lock_holder) g_free (lock_holder);

    LEAVE (" ");
    return retval;
}


/* ============================================================= */
/* The pgendSessionValidate() routine determines whether a valid
 *    session could be obtained.  It checks to see if:
 *    1) Database appers to have gnucash data in it
 *    2) the session table can be locked and updated to start
 *       a session.  The update is handled as an atomic test-n-set.
 *    Return TRUE if we have a session.
 */

static gpointer
is_gnucash_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    if (TRUE == GPOINTER_TO_INT (data)) return GINT_TO_POINTER (TRUE);

    if (0 == strcmp ("gncsession", (DB_GET_VAL ("tablename", j))))
        return GINT_TO_POINTER (TRUE);
    return GINT_TO_POINTER (FALSE);
}

static gboolean
pgendSessionValidate (PGBackend *be, int break_lock)
{
    gboolean retval = FALSE;
    char *p;
    ENTER(" ");

    if (MODE_NONE == be->session_mode) return FALSE;

    /* check to see if this database actually contains
     * GnuCash data... */
    p = "SELECT * FROM pg_tables; ";
    SEND_QUERY (be, p, FALSE);
    retval = GPOINTER_TO_INT (pgendGetResults (be,
                              is_gnucash_cb,
                              GINT_TO_POINTER (FALSE)));
    if (FALSE == retval)
    {
        qof_backend_set_error (&be->be, ERR_BACKEND_DATA_CORRUPT);
        return FALSE;
    }

    /* Lock it up so that we test-n-set atomically
     * i.e. we want to avoid a race condition when testing
     * for the single-user session.
     */
    p = "BEGIN;"
        "LOCK TABLE gncSession IN EXCLUSIVE MODE; ";
    SEND_QUERY (be, p, FALSE);

    /* Instead of doing a simple FINISH_QUERY(be->connection);
     * We need to see if we actually have permission to lock
     * the table. If its not lockable, then user doesn't have
     * perms. */
    retval = TRUE;
    {
        int i = 0;
        PGresult *result;
        do
        {
            ExecStatusType status;
            result = PQgetResult(be->connection);
            if (!result) break;
            PINFO ("clearing result %d", i);
            status = PQresultStatus(result);
            if (PGRES_COMMAND_OK != status)
            {
                PINFO("cannot lock:\n"
                      "\t%s", PQerrorMessage(be->connection));
                retval = FALSE;
            }
            PQclear(result);
            i++;
        }
        while (result);
    }

    if (FALSE == retval)
    {
        qof_backend_set_error (&be->be, ERR_BACKEND_PERM);
        p = "ROLLBACK;";
        SEND_QUERY (be, p, FALSE);
        FINISH_QUERY(be->connection);
        return FALSE;
    }


    /* Check to see if we can start a session of the desired type.  */
    if (FALSE == pgendSessionCanStart (be, break_lock))
    {
        /* This error should be treated just like the
         * file-lock error from the GUI perspective:
         * (The GUI allows users to break the lock, if desired).
         */
        be->be.load = pgend_book_load_single_lockerr;
        qof_backend_set_error (&be->be, ERR_BACKEND_LOCKED);
        retval = FALSE;
    }
    else
    {

        /* make note of the session. */
        be->sessionGuid = guid_malloc();
        guid_new (be->sessionGuid);
        guid_to_string_buff (be->sessionGuid, be->session_guid_str);
        pgendStoreOneSessionOnly (be, (void *) - 1, SQL_INSERT);
        retval = TRUE;
    }

    p = "COMMIT;\n"
        "NOTIFY gncSession;";
    SEND_QUERY (be, p, FALSE);
    FINISH_QUERY(be->connection);

    LEAVE(" ");
    return retval;
}

/* ============================================================= */
/* The pgendSessionEnd() routine will log the end of session in
 *    the session table of the database.
 */

static void
pgendSessionEnd (PGBackend *be)
{
    char *p;

    if (!be->sessionGuid) return;

#if 0
    /* vacuuming w/ analyze can improve performance 20%.
     * Should this really be done on every session end?
     * The postgres manual recommends once every night! */
    p = be->buff;
    *p = 0;
    p = stpcpy (p, "VACUUM ANALYZE;\n");

    SEND_QUERY (be, be->buff, );
    FINISH_QUERY(be->connection);
#endif

    p = be->buff;
    *p = 0;
    p = stpcpy (p, "UPDATE gncSession SET time_off='NOW' "
                "WHERE sessionGuid='");
    p = stpcpy (p, be->session_guid_str);
    p = stpcpy (p, "';\n"
                "NOTIFY gncSession;");

    SEND_QUERY (be, be->buff, );
    FINISH_QUERY(be->connection);

    guid_free (be->sessionGuid);
    be->sessionGuid = NULL;
    guid_to_string_buff (&nullguid, be->session_guid_str);
}

/* ============================================================= */
/* The pgend_session_end() routine is the main entrypoint into
 *    this backend for terminating a session.  It logs the
 *    end of the session into the gncsession table,  disconnects
 *    from the database, and finally frees all malloced memory.
 */

static void
pgend_session_end (QofBackend *bend)
{
    int i;
    PGBackend *be = (PGBackend *)bend;
    if (!be) return;

    ENTER("be=%p", be);

    /* mode-specific shutdowns */
    switch (be->session_mode)
    {
    case MODE_SINGLE_FILE:
    case MODE_SINGLE_UPDATE:
        /* although the book may be open in 'single-user' mode right now,
         * it might be opened in multi-user mode next time. Thus, update
         * the account balance checkpoints just in case.
         */
        /* pgendAccountTreeRecomputeAllCheckpoints (be, be->root); */
        break;

    case MODE_POLL:
        break;

    case MODE_EVENT:
        break;

    default:
        PERR ("bad mode specified");
        break;
    }

    /* prevent further callbacks into backend */
    pgendDisable(be);
    be->be.session_begin = NULL;
    be->be.session_end = NULL;

    /* note the logoff time in the session directory */
    pgendSessionEnd (be);

    /* disconnect from the backend */
    if (be->connection) PQfinish (be->connection);
    be->connection = 0;

    if (be->dbName)
    {
        g_free(be->dbName);
        be->dbName = NULL;
    }
    if (be->portno)
    {
        g_free(be->portno);
        be->portno = NULL;
    }
    if (be->hostname)
    {
        g_free(be->hostname);
        be->hostname = NULL;
    }

    sqlBuilder_destroy (be->builder);
    be->builder = NULL;
    g_free (be->buff);
    be->buff = NULL;

    /* free the path strings */
    for (i = 0; i < be->path_cache_size; i++)
    {
        if ((be->path_cache)[i]) g_free ((be->path_cache)[i]);
        (be->path_cache)[i] = NULL;
    }
    g_free (be->path_cache);
    be->path_cache = NULL;
    be->path_cache_size = 0;
    be->ipath_max = 0;

    LEAVE("be=%p", be);
}

/* ============================================================= */
/* The pgend_book_load_poll() routine loads account info from
 *    the database into the engine.   Its to be used only for
 *    the poll & event style load, where only the accounts,
 *    and never the transactions, need to be loaded.
 */
static void
pgend_book_load_poll (QofBackend *bend, QofBook *book)
{
    Timespec ts = gnc_iso8601_to_timespec_gmt (CK_BEFORE_LAST_DATE);
    Account *root;
    PGBackend *be = (PGBackend *)bend;

    if (!be) return;

    /* don't send events  to GUI, don't accept callbacks to backend */
    qof_event_suspend();
    pgendDisable(be);
    be->version_check = (guint32) time(0);

    pgendKVPInit(be);

    if (be->blist)
    {
        PWARN ("old book list not empty--clearing it out ");
        /* XXX not clear what this means ... should we free old books ?? */
        g_list_free (be->blist);
        be->blist = NULL;
    }
    pgendBookRestore (be, book);
    pgend_set_book (be, book);

    PINFO("Book GUID = %s\n",
          guid_to_string(qof_book_get_guid(book)));

    pgendGetAllAccountsInBook (be, book);

    root = gnc_book_get_root_account (book);
    xaccAccountBeginEdit (root);
    pgendAccountTreeGetAllBalances (be, root, ts);
    xaccAccountCommitEdit (root);

    /* re-enable events */
    pgendEnable(be);
    qof_event_resume();
}

/* ============================================================= */
/* The pgend_book_load_single() routine loads the engine with
 *    data from the database.  Used only in single-user mode,
 *    it loads account *and* transaction data.  Single-user
 *    mode doesn't require balance checkpoints, to these are
 *    not handled.
 */

static void
pgend_book_load_single (QofBackend *bend, QofBook *book)
{
    PGBackend *be = (PGBackend *)bend;

    if (!be) return;


    /* don't send events  to GUI, don't accept callbacks to backend */
    qof_event_suspend();
    pgendDisable(be);
    be->version_check = (guint32) time(0);

    pgendKVPInit(be);

    if (be->blist)
    {
        PWARN ("old book list not empty--clearing it out ");
        /* XXX not clear what this means ... should we free old books ?? */
        g_list_free (be->blist);
        be->blist = NULL;
    }
    pgendBookRestore (be, book);
    pgend_set_book (be, book);

    pgendGetAllAccountsInBook (be, book);

    pgendGetMassTransactions (be, book);

    /* re-enable events */
    pgendEnable(be);
    qof_event_resume();
}

/* ============================================================= */
/* The pgend_price_load_single() routine loads the engine with
 *    price data from the database.
 */

static void
pgend_price_load_single (QofBackend *bend, QofBook *book)
{
    PGBackend *be = (PGBackend *)bend;

    ENTER("be = %p", bend);

    if (!be || !book)
    {
        LEAVE("(null) args");
        return;
    }

    pgend_set_book (be, book);

    /* don't send events  to GUI, don't accept callbacks to backend */
    qof_event_suspend();
    pgendDisable(be);
    be->version_check = (guint32) time(0);

    pgendGetAllPricesInBook (be, book);

    /* re-enable events */
    pgendEnable(be);
    qof_event_resume();
    LEAVE(" ");
}

/* ============================================================= */
/*
 * These are the QofBackend Entry Points, which call into the various
 * functions herein..  These are generic, and (eventually) pluggable.
 *
 */

static void
pgend_do_load_single (QofBackend *bend, QofBook *book)
{
    ENTER ("be=%p", bend);
    pgend_book_load_single (bend, book);
    pgend_price_load_single (bend, book);

    /* XXX: Other things to load here.... (dynamic data) */
    LEAVE ("be=%p", bend);
}

static void
pgendDoSyncSingleFile (QofBackend *bend, QofBook *book)
{
    ENTER ("be=%p", bend);
    pgendSyncSingleFile (bend, book);
    pgendSyncPriceDBSingleFile (bend, book);

    /* XXX: Other data types */
    LEAVE ("be=%p", bend);
}

static void
pgendDoSync (QofBackend *bend, QofBook *book)
{
    ENTER ("be=%p", bend);
    pgendSync (bend, book);
    pgendSyncPriceDB (bend, book);

    /* XXX: Other data types */
    LEAVE ("be=%p", bend);
}

static void
pgend_do_begin (QofBackend *bend, QofInstance *inst)
{
    PGBackend *be = (PGBackend*)bend;
    QofIdTypeConst type = inst->e_type;

    ENTER ("be=%p, type=%s", bend, type);
    // if (!safe_strcmp (type, GNC_ID_PERIOD))
    //   return pgend_book_transfer_begin (bend, object);

    switch (be->session_mode)
    {
    case MODE_EVENT:
    case MODE_POLL:
    case MODE_SINGLE_UPDATE:
        if (!safe_strcmp (type, GNC_ID_PRICE))
            return pgend_price_begin_edit (bend, (GNCPrice *) inst);

    case MODE_SINGLE_FILE:
    case MODE_NONE:
        break;
    }

    /* XXX: Add dynamic plug-in here */
    LEAVE ("be=%p, type=%s", bend, type);
}

static void
pgend_do_commit (QofBackend *bend, QofInstance *inst)
{
    PGBackend *be = (PGBackend*)bend;
    QofIdTypeConst type = inst->e_type;

    ENTER ("be=%p, type=%s", bend, type);
    // if (!safe_strcmp (type, GNC_ID_PERIOD))
    //  return pgend_book_transfer_commit (bend, object);

    switch (be->session_mode)
    {
    case MODE_EVENT:
    case MODE_POLL:
    case MODE_SINGLE_UPDATE:

        if (!safe_strcmp (type, GNC_ID_TRANS))
        {
            Transaction *txn = (Transaction*) inst;
            return pgend_trans_commit_edit (bend, txn, txn->orig);
        }

        if (!safe_strcmp (type, GNC_ID_PRICE))
            return pgend_price_commit_edit (bend, (GNCPrice *) inst);

        if (!safe_strcmp (type, GNC_ID_ACCOUNT))
            return pgend_account_commit_edit (bend, (Account *) inst);

    case MODE_SINGLE_FILE:
    case MODE_NONE:
        break;

    }

    /* XXX: Add dynamic plug-in here */
    LEAVE ("be=%p, type=%s", bend, type);
}

static void
pgend_do_rollback (QofBackend *bend, QofInstance *inst)
{
    PGBackend *be = (PGBackend*)bend;
    QofIdTypeConst type = inst->e_type;

    ENTER ("be=%p, type=%s", bend, type);
    switch (be->session_mode)
    {
    case MODE_EVENT:
    case MODE_POLL:

        if (!safe_strcmp (type, GNC_ID_TRANS))
            return pgend_trans_rollback_edit (bend, (Transaction *)inst);

    case MODE_SINGLE_UPDATE:
    case MODE_SINGLE_FILE:
    case MODE_NONE:
        break;
    }

    /* XXX: Add dynamic plug-in here */
    LEAVE ("be=%p, type=%s", bend, type);
}

/* ============================================================= */
/* The pgend_session_begin() routine implements the main entrypoint
 *    into the SQL backend code.
 *
 *    1) It parses the URL to find the database, username, password, etc.
 *    2) It makes the first contact to the database, and tries to
 *       initiate a user session.
 *    3) It creates the GnuCash tables for the first time, if these
 *       need to be created.
 *    4) It logs the user session in the database (gncsession table).
 *    5) loads data from the database into the engine.
 */

static gpointer
has_results_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    return GINT_TO_POINTER (TRUE);
}

static void
pgend_create_db (PGBackend *be)
{
    /* We do this in pieces, so as not to exceed the max length
     * for postgres queries (which is 8192). */

    SEND_QUERY (be, table_create_str, );
    FINISH_QUERY(be->connection);
    SEND_QUERY (be, table_version_str, );
    FINISH_QUERY(be->connection);
    SEND_QUERY (be, table_audit_str, );
    FINISH_QUERY(be->connection);
    SEND_QUERY (be, sql_functions_str, );
    FINISH_QUERY(be->connection);
    be->freshly_created_db = TRUE;
    be->freshly_created_prdb = TRUE;
}

static void
pgend_session_begin (QofBackend *backend,
                     QofSession *session,
                     const char * sessionid,
                     gboolean ignore_lock,
                     gboolean create_new_db)
{
    int rc;
    PGBackend *be;
    char *url, *start, *end;
    char *password = NULL;
    char *pg_options = NULL;
    char *pg_tty = NULL;
    char *p;
    gboolean db_exists = FALSE;

    if (!backend) return;
    be = (PGBackend*)backend;

    ENTER("be=%p, sessionid=%s", be,
          sessionid ? sessionid : "(null)");

    /* close any dangling sessions from before; reinitialize */
    pgend_session_end ((QofBackend *) be);
    pgendInit (be);

    be->session = session;

    if (be->blist)
    {
        /* XXX not clear what this means ... should we free old books ?? */
        PWARN ("old book list not empty ");
        g_list_free (be->blist);
        be->blist = NULL;
    }

    /* Parse the sessionid for the hostname, port number and db name.
     * The expected URL format is
     * postgres://some.host.com/db_name
     * postgres://some.host.com:portno/db_name
     * postgres://localhost/db_name
     * postgres://localhost:nnn/db_name
     *
     * Also parse urls of the form
     * postgres://some.host.com/db_name?pgkey=pgval&pgkey=pgval
     * e.g.
     * postgres://some.host.com/db_name?user=r00t&pass=3733t&mode=multi-user
     */

    if (strncmp (sessionid, "postgres://", 11))
    {
        qof_backend_set_error (&be->be, ERR_BACKEND_BAD_URL);
        return;
    }
    url = g_strdup(sessionid);
    start = url + 11;
    end = strchr (start, ':');
    if (end)
    {
        /* if colon found, then extract port number */
        *end = 0x0;
        be->hostname = g_strdup (start);
        start = end + 1;
        end = strchr (start, '/');
        if (!end)
        {
            g_free(url);
            return;
        }
        *end = 0;
        be->portno = g_strdup (start);
    }
    else
    {
        end = strchr (start, '/');
        if (!end)
        {
            g_free(url);
            return;
        }
        *end = 0;
        be->hostname = g_strdup (start);
    }
    start = end + 1;
    if (0x0 == *start)
    {
        qof_backend_set_error (&be->be, ERR_BACKEND_BAD_URL);
        g_free(url);
        return;
    }

    /* dbname is the last thing before the url-encoded data */
    end = strchr (start, '?');
    if (end) *end = 0;
    be->dbName = g_strdup (start);

    /* loop and parse url-encoded data */
    while (end)
    {
        start = end + 1;
        end = strchr (start, '&');
        if (end) *end = 0;

        /* mode keyword */
        if (0 == strncasecmp (start, "mode=", 5))
        {
            start += 5;
            if (0 == strcasecmp (start, "single-file"))
            {
                be->session_mode = MODE_SINGLE_FILE;
            }
            else if (0 == strcasecmp (start, "single-update"))
            {
                be->session_mode = MODE_SINGLE_UPDATE;
            }
            else if (0 == strcasecmp (start, "multi-user-poll"))
            {
                be->session_mode = MODE_POLL;
            }
            else if (0 == strcasecmp (start, "multi-user"))
            {
                be->session_mode = MODE_EVENT;
            }
            else
            {
                PWARN ("the following message should be shown in a gui");
                PWARN ("unknown mode %s, will use multi-user mode",
                       start ? start : "(null)");
                qof_backend_set_message((QofBackend *)be, _("Unknown database access mode '%s'. Using default mode: multi-user."),
                                        start ? start : "(null)");
                be->session_mode = MODE_EVENT;
            }

        }
        else

            /* username and password */
            if ((0 == strncasecmp (start, "username=", 9)) ||
                    (0 == strncasecmp (start, "user=", 5)) ||
                    (0 == strncasecmp (start, "login=", 6)))
            {
                start = strchr (start, '=') + 1;
                be->username = g_strdup (start);
            }
            else

                if ((0 == strncasecmp (start, "password=", 9)) ||
                        (0 == strncasecmp (start, "passwd=", 7)) ||
                        (0 == strncasecmp (start, "pass=", 5)) ||
                        (0 == strncasecmp (start, "pwd=", 4)))
                {
                    start = strchr (start, '=') + 1;
                    password = start;
                    if (0 == strcmp (password, "''")) password = "";
                }
                else

                    /* postgres-specific options and debug tty  */
                    if (0 == strncasecmp (start, "options=", 8))
                    {
                        start = strchr (start, '=') + 1;
                        pg_options = start;
                    }
                    else

                        if (0 == strncasecmp (start, "tty=", 4))
                        {
                            start = strchr (start, '=') + 1;
                            pg_tty = start;
                        }
                        else

                            /* ignore other postgres-specific keywords */
                            if ((0 == strncasecmp (start, "host=", 5)) ||
                                    (0 == strncasecmp (start, "port=", 5)) ||
                                    (0 == strncasecmp (start, "dbname=", 7)) ||
                                    (0 == strncasecmp (start, "authtype=", 9)))
                            {
                                PWARN ("the following message should be shown in a gui");
                                PWARN ("ignoring the postgres keyword %s",
                                       start ? start : "(null)");
                            }
                            else
                            {
                                PWARN ("the following message should be shown in a gui");
                                PWARN ("unknown keyword %s, ignoring",
                                       start ? start : "(null)");
                            }
    }


    /* handle localhost as a special case */
    if (!safe_strcmp("localhost", be->hostname))
    {
        g_free (be->hostname);
        be->hostname = NULL;
    }

    /* ---------------------------------------------------------------- */
    /* New login algorithm.  If we haven't been told that we'll
     * need to be creating a database, then lets try to connect,
     * and see if that succeeds.  If it fails, we'll tell gui
     * to ask user if the DB needs to be created.
     */
    if (FALSE == create_new_db)
    {
        be->connection = PQsetdbLogin (be->hostname,
                                       be->portno,
                                       pg_options, /* trace/debug options */
                                       pg_tty, /* file or tty for debug output */
                                       be->dbName,
                                       be->username,  /* login */
                                       password);  /* pwd */

        /* check the connection status */
        if (CONNECTION_BAD == PQstatus(be->connection))
        {

            PINFO("Connection to database '%s' failed:\n"
                  "\t%s",
                  be->dbName ? be->dbName : "(null)",
                  PQerrorMessage(be->connection));

            PQfinish (be->connection);

            /* The connection may have failed either because the
             * database doesn't exist, or because there was a
             * network problem, or because the user supplied a
             * bad password or username. Try to tell these apart.
             */
            be->connection = PQsetdbLogin (be->hostname,
                                           be->portno,
                                           pg_options, /* trace/debug options */
                                           pg_tty, /* file or tty for debug output */
                                           "template1",
                                           be->username,  /* login */
                                           password);  /* pwd */

            /* check the connection status */
            if (CONNECTION_BAD == PQstatus(be->connection))
            {
                char * msg = PQerrorMessage(be->connection);
                PWARN("Connection to database 'template1' failed:\n"
                      "\t%s", msg);

                PQfinish (be->connection);
                be->connection = NULL;

                /* I wish that postgres returned usable error codes.
                 * Alas, it does not, so we just bomb out.
                 */
                qof_backend_set_error (&be->be, ERR_BACKEND_CANT_CONNECT);
                qof_backend_set_message(&be->be, _("From the Postgresql Server: %s"), msg);
                return;
            }

            /* If we are here, then we've successfully connected to the
             * server.  Now, check to see if database exists */
            p = be->buff;
            *p = 0;
            p = stpcpy (p, "SELECT datname FROM pg_database "
                        " WHERE datname='");
            p = stpcpy (p, be->dbName);
            p = stpcpy (p, "';");

            SEND_QUERY (be, be->buff, );
            db_exists = GPOINTER_TO_INT(pgendGetResults(be, has_results_cb,
                                        GINT_TO_POINTER (FALSE)));

            PQfinish (be->connection);
            be->connection = NULL;

            if (db_exists)
            {
                /* Weird.  We couldn't connect to the database, but it
                 * does seem to exist.  I presume that this is some
                 * sort of access control problem. */
                qof_backend_set_error (&be->be, ERR_BACKEND_PERM);
                return;
            }

            /* Let GUI know that we connected, but we couldn't find it. */
            qof_backend_set_error (&be->be, ERR_BACKEND_NO_SUCH_DB);
            return;
        }

        /* Check to see if we have a database version that we can
         * live with */
        rc = pgendDBVersionIsCurrent (be);
        if (rc < 0)
        {
            /* The server is newer than we are, or another error occured,
             * we don't know how to talk to it.  The err code is already set. */
            PQfinish (be->connection);
            be->connection = NULL;
            return;
        }
        if (rc > 0)
        {
            /* The server is older than we are; ask user if they want to
             * upgrade the database contents. */
            PQfinish (be->connection);
            be->connection = NULL;
            qof_backend_set_error (&be->be, ERR_SQL_DB_TOO_OLD);
            return;
        }
    }
    else
    {
        /* If we are here, then we've been asked to create the
         * database.  Well, lets do that.  But first make sure
         * it really doesn't exist */

        be->connection = PQsetdbLogin (be->hostname,
                                       be->portno,
                                       pg_options, /* trace/debug options */
                                       pg_tty, /* file or tty for debug output */
                                       "template1",
                                       be->username,  /* login */
                                       password);  /* pwd */

        /* check the connection status */
        if (CONNECTION_BAD == PQstatus(be->connection))
        {
            PERR("Connection to database '%s' failed:\n"
                 "\t%s",
                 be->dbName ? be->dbName : "(null)",
                 PQerrorMessage(be->connection));

            PQfinish (be->connection);
            be->connection = NULL;

            /* I wish that postgres returned usable error codes.
             * Alas, it does not, so we just bomb out.
             */
            qof_backend_set_error (&be->be, ERR_BACKEND_CANT_CONNECT);
            return;
        }

        /* If we are here, then we've successfully connected to the
         * server.  Now, check to see if database exists */
        p = be->buff;
        *p = 0;
        p = stpcpy (p, "SELECT datname FROM pg_database "
                    " WHERE datname='");
        p = stpcpy (p, be->dbName);
        p = stpcpy (p, "';");

        SEND_QUERY (be, be->buff, );
        db_exists = GPOINTER_TO_INT (pgendGetResults (be, has_results_cb,
                                     GINT_TO_POINTER (FALSE)));

        if (FALSE == db_exists)
        {
#if HAVE_LANGINFO_CODESET
            char* encoding = nl_langinfo(CODESET);
#else
            char* encoding = "SQL_ASCII";
#endif
            if (!strcmp (encoding, "ANSI_X3.4-1968"))
                encoding = "SQL_ASCII";

            /* create the database */
            p = be->buff;
            *p = 0;
            p = stpcpy (p, "CREATE DATABASE ");
            p = stpcpy (p, be->dbName);
            p = stpcpy (p, " WITH ENCODING = '");
            p = stpcpy (p, encoding);
            p = stpcpy (p, "';");
            SEND_QUERY (be, be->buff, );
            FINISH_QUERY(be->connection);
            PQfinish (be->connection);

            /* now connect to the newly created database */
            be->connection = PQsetdbLogin (be->hostname,
                                           be->portno,
                                           pg_options, /* trace/debug options */
                                           pg_tty, /* file or tty for debug output */
                                           be->dbName,
                                           be->username,  /* login */
                                           password);  /* pwd */

            /* check the connection status */
            if (CONNECTION_BAD == PQstatus(be->connection))
            {
                PERR("Can't connect to the newly created database '%s':\n"
                     "\t%s",
                     be->dbName ? be->dbName : "(null)",
                     PQerrorMessage(be->connection));
                PQfinish (be->connection);
                be->connection = NULL;
                /* We just created the database! If we can't connect now,
                 * the server is insane! */
                qof_backend_set_error (&be->be, ERR_BACKEND_SERVER_ERR);
                return;
            }

            /* Finally, create all the tables and indexes. */
            pgend_create_db (be);
        }
        else
        {
            gboolean gncaccount_exists;

            /* Database exists, although we were asked to create it.
             * We interpret this to mean that either it's downlevel,
             * and user wants us to upgrade it, or we are installing
             * gnucash tables into an existing database. So do one or
             * the other. */

            PQfinish (be->connection);

            /* Connect to the database */
            be->connection = PQsetdbLogin (be->hostname,
                                           be->portno,
                                           pg_options, /* trace/debug options */
                                           pg_tty, /* file or tty for debug output */
                                           be->dbName,
                                           be->username,  /* login */
                                           password);  /* pwd */

            /* check the connection status */
            if (CONNECTION_BAD == PQstatus(be->connection))
            {
                PINFO("Can't connect to the database '%s':\n"
                      "\t%s",
                      be->dbName ? be->dbName : "(null)",
                      PQerrorMessage(be->connection));
                PQfinish (be->connection);
                be->connection = NULL;

                /* Well, if we are here, we were connecting just fine,
                 * just not to this database. Therefore, it must be a
                 * permission problem.
                 */
                qof_backend_set_error (&be->be, ERR_BACKEND_PERM);
                return;
            }

            /* See if the gncaccount table exists. If it does not,
             * create all the tables. We assume that there will always
             * be a gncaccount table. */
            p = "SELECT tablename FROM pg_tables WHERE tablename='gncaccount';";
            SEND_QUERY (be, p, );
            gncaccount_exists =
                GPOINTER_TO_INT (pgendGetResults (be, has_results_cb, FALSE));

            if (!gncaccount_exists)
            {
                pgend_create_db (be);
            }
            else
            {
                rc = pgendDBVersionIsCurrent (be);
                if (0 > rc)
                {
                    /* The server is newer than we are, or another error
                     * occured, we don't know how to talk to it. The err
                     * code is already set. */
                    PQfinish (be->connection);
                    be->connection = NULL;
                    return;
                }
                if (0 < rc)
                {
                    gboolean someones_still_on;
                    /* The server is older than we are; lets upgrade */
                    /* But first, make sure all users are logged off ... */
                    p = "BEGIN;\n"
                        "LOCK TABLE gncSession IN ACCESS EXCLUSIVE MODE;\n"
                        "SELECT time_off FROM gncSession WHERE time_off ='infinity';\n";
                    SEND_QUERY (be, p, );
                    someones_still_on =
                        GPOINTER_TO_INT (pgendGetResults (be, has_results_cb,
                                                          GINT_TO_POINTER (FALSE)));
                    if (someones_still_on)
                    {
                        p = "COMMIT;\n";
                        SEND_QUERY (be, p, );
                        FINISH_QUERY(be->connection);
                        qof_backend_set_error (&be->be, ERR_SQL_DB_BUSY);
                        return;
                    }
                    p = "COMMIT;\n";
                    SEND_QUERY (be, p, );
                    FINISH_QUERY(be->connection);
                    pgendUpgradeDB (be);
                }
                else
                {
                    /* Wierd. We were asked to create something that exists.
                     * This shouldn't really happen ... */
                    PWARN ("Asked to create the database %s,\n"
                           "\tbut it already exists!\n"
                           "\tThis shouldn't really happen.",
                           be->dbName);
                }
            }
        }
    }

    /* free url only after login completed */
    g_free(url);

    /* ---------------------------------------------------------------- */

    /* set the datestyle to something we can parse */
    p = "SET DATESTYLE='ISO';";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);


    /* OK, lets see if we can get a valid session */
    rc = pgendSessionValidate (be, ignore_lock);

    /* set up pointers for appropriate behaviour */
    if (rc)
    {
        switch (be->session_mode)
        {
        case MODE_SINGLE_FILE:
            pgendEnable(be);
            be->be.load         = pgend_do_load_single;
            be->be.begin        = pgend_do_begin;
            be->be.commit       = pgend_do_commit;
            be->be.rollback     = pgend_do_rollback;

            be->be.compile_query        = NULL;
            be->be.free_query           = NULL;
            be->be.run_query            = NULL;
            be->be.price_lookup         = NULL;

            be->be.sync                 = pgendDoSyncSingleFile;
            be->be.export               = NULL;
            be->be.percentage           = NULL;
            be->be.events_pending       = NULL;
            be->be.process_events       = NULL;
            PWARN ("mode=single-file is final beta -- \n"
                   "we've fixed all known bugs but that doesn't mean\n"
                   "there aren't any! We think it's safe to use.\n");
            break;

        case MODE_SINGLE_UPDATE:
            pgendEnable(be);
            be->be.load         = pgend_do_load_single;
            be->be.begin        = pgend_do_begin;
            be->be.commit       = pgend_do_commit;
            be->be.rollback     = pgend_do_rollback;
            be->be.compile_query    = NULL;
            be->be.free_query	    = NULL;
            be->be.run_query        = NULL;
            be->be.price_lookup     = NULL;

            be->be.sync                 = pgendDoSync;
            be->be.export               = NULL;
            be->be.percentage           = NULL;
            be->be.events_pending       = NULL;
            be->be.process_events       = NULL;
            PWARN ("mode=single-update is final beta -- \n"
                   "we've fixed all known bugs but that doesn't mean\n"
                   "there aren't any! We think it's safe to use.\n");
            break;

        case MODE_POLL:
            pgendEnable(be);
            be->be.load         = pgend_book_load_poll;
            be->be.begin		= pgend_do_begin;
            be->be.commit		= pgend_do_commit;
            be->be.rollback		= pgend_do_rollback;

            be->be.compile_query	= pgendCompileQuery;
            be->be.free_query		= pgendFreeQuery;
            be->be.run_query            = pgendRunQuery;
            be->be.price_lookup         = pgendPriceFind;

            be->be.sync                 = pgendDoSync;
            be->be.export               = NULL;
            be->be.percentage           = NULL;
            be->be.events_pending       = NULL;
            be->be.process_events       = NULL;

            PWARN ("mode=multi-user-poll is beta -- \n"
                   "we've fixed all known bugs but that doesn't mean\n"
                   "there aren't any! If something seems weird, let us know.\n");
            break;

        case MODE_EVENT:
            pgendEnable(be);

            pgendSessionGetPid (be);
            pgendSessionSetupNotifies (be);

            be->be.load             = pgend_book_load_poll;
            be->be.begin            = pgend_do_begin;
            be->be.commit           = pgend_do_commit;
            be->be.rollback         = pgend_do_rollback;

            be->be.compile_query	= pgendCompileQuery;
            be->be.free_query		= pgendFreeQuery;
            be->be.run_query        = pgendRunQuery;
            be->be.price_lookup     = pgendPriceFind;

            be->be.sync             = pgendDoSync;
            be->be.export           = NULL;
            be->be.percentage       = NULL;
            be->be.events_pending   = pgendEventsPending;
            be->be.process_events   = pgendProcessEvents;

            PWARN ("mode=multi-user is beta -- \n"
                   "we've fixed all known bugs but that doesn't mean\n"
                   "there aren't any! If something seems weird, let us know.\n");

            break;

        default:
            PERR ("bad mode specified");
            break;
        }
    }

    LEAVE("be=%p, sessionid=%s", be,
          sessionid ? sessionid : "(null)");
}

/* ============================================================= */

void
pgendDisable (PGBackend *be)
{
    ENTER("be = %p", be);
    if (0 > be->nest_count)
    {
        PERR ("too many nested enables");
    }
    be->nest_count ++;
    PINFO("nest count=%d", be->nest_count);
    if (1 < be->nest_count)
    {
        LEAVE("be->nest_count > 1: %d", be->nest_count);
        return;
    }

    /* save hooks */
    be->snr.load	                = be->be.load;
    be->snr.session_end          = be->be.session_end;
    be->snr.destroy_backend      = be->be.destroy_backend;
    be->snr.begin	        = be->be.begin;
    be->snr.commit 	        = be->be.commit;
    be->snr.rollback	        = be->be.rollback;
    be->snr.compile_query        = be->be.compile_query;
    be->snr.run_query            = be->be.run_query;
    be->snr.price_lookup         = be->be.price_lookup;
    be->snr.sync                 = be->be.sync;
    be->snr.export               = be->be.export;
    be->snr.percentage           = be->be.percentage;
    be->snr.events_pending       = be->be.events_pending;
    be->snr.process_events       = be->be.process_events;

    be->be.load                 = NULL;
    be->be.session_end          = NULL;
    be->be.destroy_backend      = NULL;
    be->be.begin		       = NULL;
    be->be.commit 	       = NULL;
    be->be.rollback	       = NULL;
    be->be.compile_query        = NULL;
    be->be.run_query            = NULL;
    be->be.price_lookup         = NULL;
    be->be.sync                 = NULL;
    be->be.export               = NULL;
    be->be.percentage           = NULL;
    be->be.events_pending       = NULL;
    be->be.process_events       = NULL;

    LEAVE(" ");
}

/* ============================================================= */

void
pgendEnable (PGBackend *be)
{
    ENTER(" ");
    if (0 >= be->nest_count)
    {
        PERR ("too many nested disables");
    }
    be->nest_count --;
    PINFO("nest count=%d", be->nest_count);
    if (be->nest_count) return;

    /* restore hooks */
    be->be.load                 = be->snr.load;
    be->be.session_end          = be->snr.session_end;
    be->be.destroy_backend      = be->snr.destroy_backend;
    be->be.begin  	       = be->snr.begin;
    be->be.commit 	       = be->snr.commit;
    be->be.rollback  	       = be->snr.rollback;
    be->be.compile_query        = be->snr.compile_query;
    be->be.run_query            = be->snr.run_query;
    be->be.price_lookup         = be->snr.price_lookup;
    be->be.sync                 = be->snr.sync;
    be->be.export               = be->snr.export;
    be->be.percentage           = be->snr.percentage;
    be->be.events_pending       = be->snr.events_pending;
    be->be.process_events       = be->snr.process_events;

    LEAVE(" ");
}

/* ============================================================= */
/* The pgendInit() routine initializes the backend private
 *    structures, mallocs any needed memory, etc.
 */

static void
pgendInit (PGBackend *be)
{
    int i;
    Timespec ts;

    ENTER(" ");

    /* initialize global variable */
    nullguid = *(guid_null());

    /* access mode */
    be->session_mode = MODE_EVENT;
    be->sessionGuid = NULL;
    guid_to_string_buff (&nullguid, be->session_guid_str);

    /* generic backend handlers */
    qof_backend_init((QofBackend*)be);

    be->be.session_begin = pgend_session_begin;
    be->be.session_end = pgend_session_end;

    be->nest_count = 0;
    pgendDisable(be);

    be->be.last_err = ERR_BACKEND_NO_ERR;

    /* postgres specific data */
    be->hostname = NULL;
    be->portno = NULL;
    be->dbName = NULL;
    be->username = NULL;
    be->connection = NULL;
    be->freshly_created_db = FALSE;
    be->freshly_created_prdb = FALSE;

    be->my_pid = 0;
    be->do_account = 0;
    be->do_book = 0;
    be->do_checkpoint = 0;
    be->do_price = 0;
    be->do_session = 0;
    be->do_transaction = 0;

    ts.tv_sec = time (0);
    ts.tv_nsec = 0;

    be->last_account = ts;
    be->last_price = ts;
    be->last_transaction = ts;

    be->version_check = (guint32) ts.tv_sec;

    be->builder = sqlBuilder_new();

    be->buff = g_malloc (QBUFSIZE);
    be->bufflen = QBUFSIZE;
    be->nrows = 0;

#define INIT_CACHE_SZ 1000
    be->path_cache = (char **) g_malloc (INIT_CACHE_SZ * sizeof(char *));
    be->path_cache_size = INIT_CACHE_SZ;
    for (i = 0; i < be->path_cache_size; i++)
    {
        (be->path_cache)[i] = NULL;
    }
    be->ipath_max = 0;

    be->session = NULL;
    be->book = NULL;
    be->blist = NULL;
    LEAVE(" ");
}

/* ============================================================= */

QofBackend *
pgendNew (void)
{
    PGBackend *be;

    ENTER(" ");
    be = g_new0 (PGBackend, 1);
    pgendInit (be);

    LEAVE(" ");
    return (QofBackend *) be;
}

static void
pg_provider_free (QofBackendProvider *prov)
{
    prov->provider_name = NULL;
    prov->access_method = NULL;
    g_free (prov);
}

G_MODULE_EXPORT void
qof_backend_module_init(void)
{
    QofBackendProvider *prov;

    prov = g_new0(QofBackendProvider, 1);
    prov->provider_name = "The Postgres backend for GnuCash";
    prov->access_method = "postgres";
    prov->partial_book_supported = FALSE;
    prov->backend_new = pgendNew;
    prov->provider_free = pg_provider_free;
    prov->check_data_type = NULL;
    qof_backend_register_provider (prov);
}

/* ======================== END OF FILE ======================== */
