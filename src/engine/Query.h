/********************************************************************\
 * Query.h : api for finding transactions                           *
 * Copyright 2000 Bill Gribble <grib@billgribble.com>               *
 * Copyright 2002 Linas Vepstas <linas@linas.org>                   *
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

#ifndef GNUCASH_QUERY_H
#define GNUCASH_QUERY_H

#include <sys/types.h>
#include <time.h>
#include <glib.h>
#include "qof.h"
#include "Account.h"

/*
 * This function defines a compatibility API from the old Query API to
 * the new Query API.  Note that it is not a 100% complete equivalent.
 * Many functions have a one-to-one mapping in the new API, but many
 * others do not.
 */

typedef QofQuery Query;

#define xaccMallocQuery()	qof_query_create_for(GNC_ID_SPLIT)
#define xaccFreeQuery		qof_query_destroy
#define xaccQueryCopy		qof_query_copy
#define xaccQuerySetBook	qof_query_set_book

#define xaccQueryInvert		qof_query_invert
#define xaccQueryMerge		qof_query_merge
#define xaccQueryClear		qof_query_clear

/* The xaccQueryHasTerms() routine returns the number of 'OR' terms in the query.
 * The xaccQueryNumTerms() routine returns the total number of terms in the query.
 */

#define xaccQueryHasTerms	qof_query_has_terms
#define xaccQueryNumTerms	qof_query_num_terms


#define xaccQuerySetSortIncreasing	qof_query_set_sort_increasing

#define xaccQuerySetMaxSplits	qof_query_set_max_results
#define xaccQueryGetMaxSplits	qof_query_get_max_results

#define xaccQueryEqual		qof_query_equal

typedef enum
{
    QUERY_TXN_MATCH_ALL = 1, /* match all accounts */
    QUERY_TXN_MATCH_ANY = 2  /* match any account */
} query_txn_match_t;

/* After the query has been set up, call one of these to run the query.
 *    XXX The routines below should be replaced by a query
 *    that explicitly asks for a list of the desired item.
 *
 * The xaccQueryGetSplits() routine returns all splits matching the
 *    query.  Any given split will appear at most once in the result;
 *    however, several splits from one transaction may appear in the list.
 *    The caller MUST NOT change the GList.
 *
 * The xaccQueryGetSplitsUniqueTrans() routine returns splits matching
 *    the query, but only one matching split per transaction will be
 *    returned.  In other words, any given transaction will be
 *    represented at most once in the returned list.  The caller must
 *    free the GList.
 *
 * The xaccQueryGetTransactions() routine returns a list of
 *    transactions that match the query.  The GList must be freed by
 *    the caller. The query_run_t argument is used to provide account
 *    matching in the following way:
 *
 * The xaccQueryGetLots() routine is just like GetTransactions() except
 *    it returns a list of Lots.
 *
 *    query_txn_match_t describes how to match accounts when querying
 *    for transactions with xaccQueryGetTransactions().
 *    What is the difference between 'ANY' and 'ALL', you
 *    may ask?  First, let us recall that a transaction consists
 *    of splits, and each split belongs to exactly one account.
 *    Specifying "MATCH_ALL"  means that *every* account that
 *    shows up in the query must also show up in some split in
 *    the transaction (in order for that transaction to be
 *    selected).  By contrast, specifying 'ANY' means that
 *    any account in the query must show up in some split
 *    in the transaction (in order for the transaction to
 *    be selected).  Thus, 'ANY' acts as a boolean-OR when
 *    matching accounts, whereas 'AND' acts as a boolean-AND
 *    for matching accounts.  Whew. Got that?
 */
#define xaccQueryGetSplits	qof_query_run
SplitList   * xaccQueryGetSplitsUniqueTrans(Query *q);
TransList   * xaccQueryGetTransactions(Query * q, query_txn_match_t type);
LotList     * xaccQueryGetLots(Query * q, query_txn_match_t type);

/*******************************************************************
 *  match-adding API
 *******************************************************************/

void xaccQueryAddAccountMatch(Query *, AccountList *,
                              QofGuidMatch how, QofQueryOp op);

void xaccQueryAddAccountGUIDMatch(Query *, AccountGUIDList *,
                                  QofGuidMatch, QofQueryOp);

void xaccQueryAddSingleAccountMatch(Query *, Account *, QofQueryOp);

void xaccQueryAddStringMatch (Query* q, const char *matchstring,
                              gboolean case_sens, gboolean use_regexp,
                              QofQueryOp op,
                              const char * path, ...);
void
xaccQueryAddDescriptionMatch(Query *q, const char *m, gboolean c, gboolean r,
                             QofQueryOp o);
void
xaccQueryAddNumberMatch(Query *q, const char *m, gboolean c, gboolean r,
                        QofQueryOp o);
void
xaccQueryAddActionMatch(Query *q, const char *m, gboolean c, gboolean r,
                        QofQueryOp o);
void
xaccQueryAddMemoMatch(Query *q, const char *m, gboolean c, gboolean r,
                      QofQueryOp o);
void
xaccQueryAddValueMatch(Query *q, gnc_numeric amt, QofNumericMatch sgn,
                       QofQueryCompare how, QofQueryOp op);
void
xaccQueryAddSharePriceMatch(Query *q, gnc_numeric amt, QofQueryCompare how,
                            QofQueryOp op);
void
xaccQueryAddSharesMatch(Query *q, gnc_numeric amt, QofQueryCompare how,
                        QofQueryOp op);
void
xaccQueryAddBalanceMatch(Query *q, QofQueryCompare bal, QofQueryOp op);

void xaccQueryAddNumericMatch (Query *q, gnc_numeric amount,
                               QofNumericMatch sign, QofQueryCompare how,
                               QofQueryOp op, const char * path, ...);

/** The DateMatch queries match transactions whose posted date
 *    is in a date range.  If use_start is TRUE, then a matching
 *    posted date will be greater than the start date.   If
 *    use_end is TRUE, then a match occurs for posted dates earlier
 *    than the end date.  If both flags are set, then *both*
 *    conditions must hold ('and').  If neither flag is set, then
 *    all transactions are matched.
 */

void xaccQueryAddDateMatch(Query * q, gboolean use_start,
                           int sday, int smonth, int syear,
                           gboolean use_end, int eday, int emonth, int eyear,
                           QofQueryOp op);
void xaccQueryAddDateMatchTS(Query * q,
                             gboolean use_start, Timespec sts,
                             gboolean use_end, Timespec ets,
                             QofQueryOp op);
void xaccQueryAddDateMatchTT(Query * q,
                             gboolean use_start, time_t stt,
                             gboolean use_end, time_t ett,
                             QofQueryOp op);
void xaccQueryGetDateMatchTS (Query * q,
                              Timespec * sts,
                              Timespec * ets);
void xaccQueryGetDateMatchTT (Query * q,
                              time_t * stt,
                              time_t * ett);

typedef enum
{
    CLEARED_NONE       = 0x0000,
    CLEARED_NO         = 0x0001,
    CLEARED_CLEARED    = 0x0002,
    CLEARED_RECONCILED = 0x0004,
    CLEARED_FROZEN     = 0x0008,
    CLEARED_VOIDED     = 0x0010,
    CLEARED_ALL        = 0x001F
} cleared_match_t;

void xaccQueryAddClearedMatch(Query * q, cleared_match_t how, QofQueryOp op);
void xaccQueryAddGUIDMatch(Query * q, const GUID *guid,
                           QofIdType id_type, QofQueryOp op);

/** given kvp value is on right side of comparison */
void xaccQueryAddKVPMatch(Query *q, GSList *path, const KvpValue *value,
                          QofQueryCompare how, QofIdType id_type,
                          QofQueryOp op);

/*******************************************************************
 *  compatibility interface with old Query API
 *******************************************************************/
time_t xaccQueryGetEarliestDateFound(Query * q);
time_t xaccQueryGetLatestDateFound(Query * q);

#endif
