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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef GNUCASH_QUERY_H
#define GNUCASH_QUERY_H

#include <sys/types.h>
#include <time.h>
#include <glib.h>

#include "date.h" 
#include "gnc-engine.h" 
#include "GNCId.h" 
#include "guid.h" 
#include "kvp_frame.h" 

#include "QueryNew.h"
#include "QueryCore.h"

/*
 * This function defines a compatibility API from the old Query API to
 * the new Query API.  Note that it is not a 100% complete equivalent.
 * Many functions have a one-to-one mapping in the new API, but many
 * others do not.
 */

typedef QueryNew Query;

#define xaccMallocQuery()	gncQueryCreateFor(GNC_ID_SPLIT)
#define xaccFreeQuery		gncQueryDestroy
#define xaccQueryCopy		gncQueryCopy
#define xaccQuerySetBook	gncQuerySetBook

#define xaccQueryInvert		gncQueryInvert
#define xaccQueryMerge		gncQueryMerge
#define xaccQueryClear		gncQueryClear

/* The xaccQueryHasTerms() routine returns the number of 'OR' terms in the query.
 * The xaccQueryNumTerms() routine returns the total number of terms in the query.
 */

// void xaccQueryPurgeTerms(Query * q, pd_type_t type);
// gboolean      xaccQueryHasTermType(Query * q, pd_type_t type);
#define xaccQueryHasTerms	gncQueryHasTerms
#define xaccQueryNumTerms	gncQueryNumTerms
// #define xaccQueryGetTerms	gncQueryGetTerms


#define xaccQuerySetSortIncreasing	gncQuerySetSortIncreasing

#define xaccQuerySetMaxSplits	gncQuerySetMaxResults
#define xaccQueryGetMaxSplits	gncQueryGetMaxResults

#define xaccQueryEqual		gncQueryEqual

typedef enum {
  QUERY_TXN_MATCH_ALL=1,   /* match all accounts */
  QUERY_TXN_MATCH_ANY=2    /* match any account */
} query_txn_match_t;

/* After the query has been set up, call one of these to run the query. 
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
#define xaccQueryGetSplits	gncQueryRun
SplitList   * xaccQueryGetSplitsUniqueTrans(Query *q);
TransList   * xaccQueryGetTransactions(Query * q, query_txn_match_t type);
LotList     * xaccQueryGetLots(Query * q, query_txn_match_t type);

/*******************************************************************
 *  match-adding API 
 *******************************************************************/

void xaccQueryAddAccountMatch(Query *, AccountList *,
                              guid_match_t how, QueryOp op);

void xaccQueryAddAccountGUIDMatch(Query *, AccountGUIDList *,
                                  guid_match_t, QueryOp);

void xaccQueryAddSingleAccountMatch(Query *, Account *, QueryOp);

void xaccQueryAddStringMatch (Query* q, const char *matchstring,
			      int case_sens, int use_regexp, QueryOp op,
			      const char * path, ...);

#define xaccQueryAddDescriptionMatch(q,m,c,r,o) \
	xaccQueryAddStringMatch ((q), (m), (c), (r), (o), SPLIT_TRANS, \
				TRANS_DESCRIPTION, NULL)
#define xaccQueryAddNumberMatch(q,m,c,r,o) \
	xaccQueryAddStringMatch ((q), (m), (c), (r), (o), SPLIT_TRANS, \
				TRANS_NUM, NULL)
#define xaccQueryAddActionMatch(q,m,c,r,o) \
	xaccQueryAddStringMatch ((q), (m), (c), (r), (o), SPLIT_ACTION, \
				NULL)
#define xaccQueryAddMemoMatch(q,m,c,r,o) \
	xaccQueryAddStringMatch ((q), (m), (c), (r), (o), SPLIT_MEMO, \
				NULL)

void xaccQueryAddNumericMatch (Query *q, gnc_numeric amount,
			       numeric_match_t sign, query_compare_t how,
			       QueryOp op, const char * path, ...);

#define xaccQueryAddValueMatch(q,amt,sgn,how,op) \
	xaccQueryAddNumericMatch ((q), (amt), (sgn), (how), (op), \
				SPLIT_VALUE, NULL)

#define xaccQueryAddSharePriceMatch(q,amt,how,op) \
	xaccQueryAddNumericMatch ((q), (amt), NUMERIC_MATCH_ANY, (how), (op), \
				SPLIT_SHARE_PRICE, NULL)
 
#define xaccQueryAddSharesMatch(q,amt,how,op) \
	xaccQueryAddNumericMatch ((q), (amt), NUMERIC_MATCH_ANY, (how), (op), \
				SPLIT_AMOUNT, NULL)

#define xaccQueryAddBalanceMatch(q,bal,op) \
	xaccQueryAddNumericMatch ((q), gnc_numeric_zero(), NUMERIC_MATCH_ANY, \
				((bal) ? COMPARE_EQUAL : COMPARE_NEQ), (op), \
				SPLIT_TRANS, TRANS_IMBALANCE, NULL)

/* The DateMatch queries match transactions whose posted date
 *    is in a date range.  If use_start is TRUE, then a matching
 *    posted date will be greater than the start date.   If 
 *    use_end is TRUE, then a match occurs for posted dates earlier 
 *    than the end date.  If both flags are set, then *both* 
 *    conditions must hold ('and').  If neither flag is set, then 
 *    all transactions are matched.
 */

void xaccQueryAddDateMatch(Query * q, 
                           int use_start, int sday, int smonth, int syear, 
                           int use_end, int eday, int emonth, int eyear,
                           QueryOp op);
void xaccQueryAddDateMatchTS(Query * q, 
                             int use_start, Timespec sts, 
                             int use_end, Timespec ets,
                             QueryOp op);
void xaccQueryAddDateMatchTT(Query * q, 
                             int use_start, time_t stt, 
                             int use_end, time_t ett,
                             QueryOp op);
typedef enum {
  CLEARED_NO         = 1 << 0,
  CLEARED_CLEARED    = 1 << 1,
  CLEARED_RECONCILED = 1 << 2, 
  CLEARED_FROZEN     = 1 << 3,
  CLEARED_VOIDED     = 1 << 4
} cleared_match_t;

void xaccQueryAddClearedMatch(Query * q, cleared_match_t how, QueryOp op);
void xaccQueryAddGUIDMatch(Query * q, const GUID *guid,
                           GNCIdType id_type, QueryOp op);
void xaccQueryAddGUIDMatchGL (QueryNew *q, GList *param_list,
			      GUID guid, QueryOp op);

/* given kvp value is on right side of comparison */
void xaccQueryAddKVPMatch(Query *q, GSList *path, const kvp_value *value,
                          query_compare_t how, GNCIdType id_type,
                          QueryOp op);

void xaccQuerySetSortOrder(Query *q, GList *p1, GList *p2, GList *p3);

/*******************************************************************
 *  compatibility interface with old Query API 
 *******************************************************************/
time_t xaccQueryGetEarliestDateFound(Query * q);
time_t xaccQueryGetLatestDateFound(Query * q);

#endif
