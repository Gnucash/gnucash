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

typedef struct query_s Query;

typedef enum {
  QUERY_AND=1,
  QUERY_OR,
  QUERY_NAND,
  QUERY_NOR,
  QUERY_XOR
} QueryOp;

typedef enum {
  BY_STANDARD=1,
  BY_DATE,
  BY_DATE_ROUNDED,
  BY_DATE_ENTERED,
  BY_DATE_ENTERED_ROUNDED,
  BY_DATE_RECONCILED,
  BY_DATE_RECONCILED_ROUNDED,
  BY_NUM,
  BY_AMOUNT,
  BY_MEMO,
  BY_DESC,
  BY_RECONCILE,
  BY_ACCOUNT_FULL_NAME,
  BY_ACCOUNT_CODE,
  BY_CORR_ACCOUNT_FULL_NAME,
  BY_CORR_ACCOUNT_CODE,
  BY_NONE
} sort_type_t;  

typedef enum {
  PD_ACCOUNT=1, 
  PD_AMOUNT,
  PD_BALANCE,
  PD_BOOK,
  PD_CLEARED,
  PD_DATE,
  PD_GUID,
  PD_KVP,
  PD_STRING,
  PD_MISC
} pd_type_t;

typedef enum {
  PR_ACCOUNT=1,
  PR_ACTION,
  PR_BALANCE,
  PR_BOOK,
  PR_CLEARED,
  PR_DATE,
  PR_DESC,
  PR_GUID,
  PR_KVP,
  PR_MEMO,
  PR_NUM,
  PR_PRICE,
  PR_SHRS,   /* FIXME: misnamed, should be PR_QUANT or PR_AMOUNT */
  PR_VALUE,   
  PR_MISC,
} pr_type_t;

typedef enum {
  ACCT_MATCH_ALL=1,
  ACCT_MATCH_ANY,
  ACCT_MATCH_NONE
} acct_match_t;

typedef enum
{
  AMT_MATCH_ATLEAST=1,
  AMT_MATCH_ATMOST, 
  AMT_MATCH_EXACTLY
} amt_match_t;

typedef enum {
  AMT_SGN_MATCH_EITHER=1,
  AMT_SGN_MATCH_CREDIT, 
  AMT_SGN_MATCH_DEBIT
} amt_match_sgn_t;

typedef enum {
  BOOK_MATCH_ANY=1,
  BOOK_MATCH_NONE
} book_match_t;

typedef enum {
  CLEARED_NO         = 1 << 0,
  CLEARED_CLEARED    = 1 << 1,
  CLEARED_RECONCILED = 1 << 2, 
  CLEARED_FROZEN     = 1 << 3,
  CLEARED_VOIDED     = 1 << 4
} cleared_match_t;

enum {
  STRING_MATCH_CASE   = 1 << 0,
  STRING_MATCH_REGEXP = 1 << 1
};

typedef enum {
  BALANCE_BALANCED   = 1 << 0,
  BALANCE_UNBALANCED = 1 << 1
} balance_match_t;

typedef enum {
  KVP_MATCH_LT=1,
  KVP_MATCH_LTE,
  KVP_MATCH_EQ,
  KVP_MATCH_GTE,
  KVP_MATCH_GT
} kvp_match_t;

typedef enum {
  KVP_MATCH_SPLIT   = 1 << 0,
  KVP_MATCH_TRANS   = 1 << 1,
  KVP_MATCH_ACCOUNT = 1 << 2
} kvp_match_where_t;


typedef enum {
  QUERY_MATCH_ALL=1,   /* match all accounts */
  QUERY_MATCH_ANY=2    /* match any account */
} query_run_t;


/*******************************************************************
 *  basic Query API
 *******************************************************************/

Query       * xaccMallocQuery(void);
void          xaccFreeQuery(Query *);
Query       * xaccQueryCopy(Query *q);
void          xaccQuerySetBook(Query * q, GNCBook *);
void          DxaccQuerySetGroup(Query * q, AccountGroup *);  /* depricated */

Query       * xaccQueryInvert(Query * q1);
Query       * xaccQueryMerge(Query * q1, Query * q2, QueryOp op);
void          xaccQueryClear(Query * q);

/* The xaccQueryHasTerms() routine returns the number of 'OR' terms in the query.
 * The xaccQueryNumTerms() routine returns the total number of terms in the query.
 */

void          xaccQueryPurgeTerms(Query * q, pd_type_t type);
int           xaccQueryHasTerms(Query * q);
gboolean      xaccQueryHasTermType(Query * q, pd_type_t type);
GList       * xaccQueryGetTerms(Query * q);
int           xaccQueryNumTerms(Query * q); 


/* After the query has been set up, call one of these to run the query. 
 *
 * The xaccQueryGetSplits() routine returns all splits matching the 
 *    query.  Any given split will appear at most once in the result;
 *    however, several splits from one transaction may appear in the list.
 *
 * The xaccQueryGetSplitsUniqueTrans() routine returns splits matching
 *    the query, but only one matching split per transaction will be 
 *    returned.  In other words, any given transaction will be 
 *    represented at most once in the returned list.
 *
 * The xaccQueryGetTransactions() routine returns a list of 
 *    transactions that match the query.  The query_run_t 
 *    argument is used to provide account matching in the 
 *    following way:
 *
 *    query_run_t describes how to match accounts when querying 
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
SplitList   * xaccQueryGetSplits(Query * q);
SplitList   * xaccQueryGetSplitsUniqueTrans(Query *q);
TransList   * xaccQueryGetTransactions(Query * q, query_run_t type);

/* compare two queries for equality. this is a simplistic
 * implementation -- logical equivalences between different
 * and/or trees are ignored. */
gboolean xaccQueryEqual(Query *q1, Query *q2);

/* handy for debugging */
void    xaccQueryPrint(Query *q);

/*******************************************************************
 *  match-adding API 
 *******************************************************************/

void xaccQueryAddAccountMatch(Query *, AccountList *,
                              acct_match_t how, QueryOp op);
void xaccQueryAddAccountGUIDMatch(Query *, AccountGUIDList *,
                                  acct_match_t, QueryOp);
void xaccQueryAddSingleAccountMatch(Query *, Account *, QueryOp);

void xaccQueryAddBookMatch(Query * q, BookList *,
                              acct_match_t how, QueryOp op);
void xaccQueryAddBookGUIDMatch(Query *, BookGUIDList *,
                                  acct_match_t how, QueryOp op);
void xaccQueryAddSingleBookMatch(Query *, GNCBook *, QueryOp);

void xaccQueryAddDescriptionMatch(Query * q, const char * matchstring, 
                                  int case_sens, int use_regexp, QueryOp op);
void xaccQueryAddNumberMatch(Query * q, const char * matchstring, 
                             int case_sens, int use_regexp, QueryOp op);
void xaccQueryAddActionMatch(Query * q, const char * matchstring, 
                             int case_sens, int use_regexp, QueryOp op);

/* ?????????? why are these depricated ??????????? */
void DxaccQueryAddValueMatch(Query * q, double amount, 
                              amt_match_sgn_t amt_sgn,
                              amt_match_t how, QueryOp op);
void DxaccQueryAddSharePriceMatch(Query * q, double amount, 
                                  amt_match_t how, QueryOp op);
void DxaccQueryAddSharesMatch(Query * q, double amount, 
                              amt_match_t how, QueryOp op);

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
void xaccQueryAddMemoMatch(Query * q, const char * matchstring, 
                           int case_sens, int use_regexp, QueryOp op);
void xaccQueryAddClearedMatch(Query * q, cleared_match_t how, QueryOp op);
void xaccQueryAddBalanceMatch(Query * q, balance_match_t how, QueryOp op);
void xaccQueryAddGUIDMatch(Query * q, const GUID *guid,
                           GNCIdType id_type, QueryOp op);
/* given kvp value is on right side of comparison */
void xaccQueryAddKVPMatch(Query *q, GSList *path, const kvp_value *value,
                          kvp_match_t how, kvp_match_where_t where,
                          QueryOp op);


/*******************************************************************
 *  sort-related functions 
 *******************************************************************/

void xaccQuerySetSortOrder(Query * q, sort_type_t primary, 
                           sort_type_t secondary, sort_type_t tertiary);
sort_type_t xaccQueryGetPrimarySortOrder(Query * q);
sort_type_t xaccQueryGetSecondarySortOrder(Query * q);
sort_type_t xaccQueryGetTertiarySortOrder(Query * q);

void xaccQuerySetSortIncreasing(Query * q,
                                gboolean prim_increasing,
                                gboolean sec_increasing, 
				gboolean tert_increasing);
gboolean xaccQueryGetSortPrimaryIncreasing (Query *q);
gboolean xaccQueryGetSortSecondaryIncreasing (Query *q);
gboolean xaccQueryGetSortTertiaryIncreasing (Query *q);

void xaccQuerySetMaxSplits(Query * q, int n);
int  xaccQueryGetMaxSplits(Query * q);


/*******************************************************************
 *  compatibility interface with old Query API 
 *******************************************************************/
time_t xaccQueryGetEarliestDateFound(Query * q);
time_t xaccQueryGetLatestDateFound(Query * q);


#endif
