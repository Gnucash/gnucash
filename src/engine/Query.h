/********************************************************************\
 * Query.h : api for finding transactions                           *
 * Copyright 2000 Bill Gribble <grib@billgribble.com>               *
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

#ifndef __GNUCASH_QUERY_H__
#define __GNUCASH_QUERY_H__

#include <sys/types.h>
#include <time.h>
#include <glib.h>
#include <regex.h>

#include "gnc-common.h" 
#include "Account.h" 
#include "Transaction.h" 

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
  BY_DATE_ENTERED,
  BY_DATE_RECONCILED,
  BY_NUM,
  BY_AMOUNT,
  BY_MEMO,
  BY_DESC,
  BY_RECONCILE,
  BY_ACCOUNT_NAME,
  BY_ACCOUNT_CODE,
  BY_CORR_ACCOUNT_NAME,
  BY_CORR_ACCOUNT_CODE,
  BY_NONE
} sort_type_t;  

typedef enum {
  PD_DATE=1,
  PD_AMOUNT,
  PD_ACCOUNT, 
  PD_STRING,
  PD_CLEARED,
  PD_BALANCE,
  PD_GUID,
  PD_MISC
} pd_type_t;

typedef enum {
  PR_ACCOUNT=1,
  PR_ACTION,
  PR_AMOUNT,
  PR_BALANCE,
  PR_CLEARED,
  PR_DATE,
  PR_DESC,
  PR_GUID,
  PR_MEMO,
  PR_MISC,
  PR_NUM,
  PR_PRICE,
  PR_SHRS
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
  CLEARED_NO         = 1 << 0,
  CLEARED_CLEARED    = 1 << 1,
  CLEARED_RECONCILED = 1 << 2, 
  CLEARED_FROZEN     = 1 << 3
} cleared_match_t;

enum {
  STRING_MATCH_CASE   = 1 << 0,
  STRING_MATCH_REGEXP = 1 << 1
};

typedef enum {
  BALANCE_BALANCED   = 1 << 0,
  BALANCE_UNBALANCED = 1 << 1
} balance_match_t;

/* query_run_t describes whether to require all splits or 
 * any to match for a transaction to be returned by 
 * xaccQueryGetTransactions */

typedef enum {
  QUERY_MATCH_ALL=1,
  QUERY_MATCH_ANY=2
} query_run_t;

typedef struct _querystruct Query;

typedef struct {
  pd_type_t       type;
  pr_type_t       term_type;
  int             sense;
} BasePredicateData;

typedef struct {
  pd_type_t       type;
  pr_type_t       term_type;
  int             sense;
  int             use_start;
  Timespec        start;
  int             use_end;
  Timespec        end;
} DatePredicateData;

typedef struct {
  pd_type_t       type;
  pr_type_t       term_type;
  int             sense;
  amt_match_t     how;
  amt_match_sgn_t amt_sgn;
  double          amount;
} AmountPredicateData;

typedef struct {
  pd_type_t       type;
  pr_type_t       term_type;
  int             sense;
  acct_match_t    how;
  GList           *accounts;
  GList           *account_guids;
} AccountPredicateData;

typedef struct {
  pd_type_t       type;
  pr_type_t       term_type;
  int             sense;
  int             case_sens;
  int             use_regexp;
  char           *matchstring;
  regex_t         compiled;
} StringPredicateData;

typedef struct {
  pd_type_t       type;
  pr_type_t       term_type;
  int             sense;
  cleared_match_t how;
} ClearedPredicateData;

typedef struct {
  pd_type_t       type;
  pr_type_t       term_type;
  int             sense;
  balance_match_t how;
} BalancePredicateData;

typedef struct {
  pd_type_t       type;
  pr_type_t       term_type;
  int             sense;
  GUID            guid;
} GUIDPredicateData;

typedef struct {
  pd_type_t       type;
  pr_type_t       term_type;
  int             sense;
  int             how;
  int             data;
} MiscPredicateData;

typedef union { 
  pd_type_t            type;
  BasePredicateData    base;
  DatePredicateData    date;
  AmountPredicateData  amount;
  AccountPredicateData acct;
  StringPredicateData  str;
  ClearedPredicateData cleared;
  BalancePredicateData balance;
  GUIDPredicateData    guid;
  MiscPredicateData    misc;
} PredicateData;

typedef int (* Predicate)(Split * to_test, PredicateData * test_data);

typedef struct {
  PredicateData data;
  Predicate     p;
} QueryTerm;


/*******************************************************************
 *  basic Query API
 *******************************************************************/

Query       * xaccMallocQuery(void);
void          xaccFreeQuery(Query *);
Query       * xaccQueryCopy(Query *q);
void          xaccQuerySetGroup(Query * q, AccountGroup * group);
AccountGroup *xaccQueryGetGroup(Query * q);

Query       * xaccQueryInvert(Query * q1);
Query       * xaccQueryMerge(Query * q1, Query * q2, QueryOp op);
void          xaccQueryClear(Query * q);

void          xaccQueryPurgeTerms(Query * q, pd_type_t type);
int           xaccQueryHasTerms(Query * q);
gboolean      xaccQueryHasTermType(Query * q, pd_type_t type);
GList       * xaccQueryGetTerms(Query * q);

/* after the query has been set up, call this to run the query */
GList       * xaccQueryGetSplits(Query * q);
GList       * xaccQueryGetSplitsUniqueTrans(Query *q);
GList       * xaccQueryGetTransactions(Query * q, query_run_t type);

/* handy for debugging */
void    xaccQueryPrint(Query *q);

/*******************************************************************
 *  match-adding API 
 *******************************************************************/

void xaccQueryAddAccountMatch(Query * q, GList * accounts,
                              acct_match_t how, QueryOp op);
void xaccQueryAddSingleAccountMatch(Query * q, Account * acct, 
                                    QueryOp op);

void xaccQueryAddDescriptionMatch(Query * q, const char * matchstring, 
                                  int case_sens, int use_regexp, QueryOp op);
void xaccQueryAddNumberMatch(Query * q, const char * matchstring, 
                             int case_sens, int use_regexp, QueryOp op);
void xaccQueryAddActionMatch(Query * q, const char * matchstring, 
                             int case_sens, int use_regexp, QueryOp op);
void DxaccQueryAddAmountMatch(Query * q, double amount, 
                              amt_match_sgn_t amt_sgn,
                              amt_match_t how, QueryOp op);
void DxaccQueryAddSharePriceMatch(Query * q, double amount, 
                                  amt_match_t how, QueryOp op);
void DxaccQueryAddSharesMatch(Query * q, double amount, 
                              amt_match_t how, QueryOp op);
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
void xaccQueryAddGUIDMatch(Query * q, const GUID *guid, QueryOp op);
void xaccQueryAddMiscMatch(Query * q, Predicate p, int how, int data,
                           QueryOp op);

void xaccQueryAddPredicate (Query * q, PredicateData *pred, QueryOp op);


/*******************************************************************
 *  sort-related functions 
 *******************************************************************/

void xaccQuerySetSortOrder(Query * q, sort_type_t primary, 
                           sort_type_t secondary, sort_type_t tertiary);
void xaccQuerySetSortIncreasing(Query * q, gboolean prim_increasing,
			   gboolean sec_increasing, 
				gboolean tert_increasing);
void xaccQuerySetMaxSplits(Query * q, int n);
int  xaccQueryGetMaxSplits(Query * q);


/*******************************************************************
 *  compatibility interface with old Query API 
 *******************************************************************/
time_t xaccQueryGetEarliestDateFound(Query * q);
time_t xaccQueryGetLatestDateFound(Query * q);


/* This is useful for network systems */
Predicate xaccQueryGetPredicate (pr_type_t term_type);

#endif
