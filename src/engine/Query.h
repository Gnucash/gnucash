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
  QUERY_AND,
  QUERY_OR,
  QUERY_NAND,
  QUERY_NOR,
  QUERY_XOR
} QueryOp;

typedef enum {
  BY_STANDARD,
  BY_DATE,
  BY_DATE_ENTERED,
  BY_DATE_RECONCILED,
  BY_NUM,
  BY_AMOUNT,
  BY_MEMO,
  BY_DESC,
  BY_RECONCILE,
  BY_NONE
} sort_type_t;  

typedef enum { PD_DATE, PD_AMOUNT, PD_ACCOUNT, 
               PD_STRING, PD_CLEARED,  PD_MISC } pd_type_t;

typedef enum { ACCT_MATCH_ALL, ACCT_MATCH_ANY, ACCT_MATCH_NONE } acct_match_t;
typedef enum { AMT_MATCH_ATLEAST, AMT_MATCH_ATMOST, 
               AMT_MATCH_EXACTLY } amt_match_t;
typedef enum { AMT_SGN_MATCH_EITHER, AMT_SGN_MATCH_CREDIT, 
               AMT_SGN_MATCH_DEBIT } amt_match_sgn_t;

enum { CLEARED_NO=1, CLEARED_CLEARED=2, CLEARED_RECONCILED=4, 
       CLEARED_FROZEN=8 };

enum { STRING_MATCH_CASE=1, STRING_MATCH_REGEXP=2};

typedef struct _querystruct Query;

typedef struct {
  pd_type_t    type;
  Timespec     start;
  Timespec     end;
} DatePredicateData;

typedef struct {
  pd_type_t       type;
  amt_match_t     how;
  amt_match_sgn_t amt_sgn;
  double          amount;
} AmountPredicateData;

typedef struct {
  pd_type_t    type;
  acct_match_t how;
  Account      ** accounts;
} AccountPredicateData;

typedef struct {
  pd_type_t      type;
  int            case_sens;
  int            use_regexp;
  char           * matchstring;
  regex_t        compiled;
} StringPredicateData;

typedef struct {
  pd_type_t  type;
  int        how;
  int        data;
} MiscPredicateData;

typedef struct {
  pd_type_t  type;
  int        how;
} ClearedPredicateData;


typedef union { 
  pd_type_t            type;
  DatePredicateData    date;
  AmountPredicateData  amount;
  AccountPredicateData acct;
  StringPredicateData  str;
  ClearedPredicateData cleared;
  MiscPredicateData    misc;
} PredicateData;

typedef int (* Predicate)(Split * to_test, PredicateData * test_data);

typedef struct {
  Predicate     p;
  PredicateData data;
  int           sense;
} QueryTerm;


/*******************************************************************
 *  basic Query API
 *******************************************************************/

Query   * xaccMallocQuery(void);
void    xaccInitQuery(Query * q, QueryTerm * qt);
void    xaccFreeQuery(Query *);
void    xaccQuerySetGroup(Query * q, AccountGroup * group);
Query   * xaccQueryInvert(Query * q1);
Query   * xaccQueryMerge(Query * q1, Query * q2, QueryOp op);
void    xaccQueryClear(Query * q);
void    xaccQueryPurgeTerms(Query * q, pd_type_t type);

int     xaccQueryHasTerms(Query * q);

Split   ** xaccQueryGetSplits(Query * q);

/*******************************************************************
 *  match-adding API 
 *******************************************************************/

void xaccQueryAddAccountMatch(Query * q, Account ** acclist,
                              acct_match_t how, QueryOp op);
void xaccQueryAddSingleAccountMatch(Query * q, Account * acct, 
                                    QueryOp op);

void xaccQueryAddDescriptionMatch(Query * q, char * matchstring, 
                                  int case_sens, int use_regexp, QueryOp op);
void xaccQueryAddNumberMatch(Query * q, char * matchstring, 
                             int case_sens, int use_regexp, QueryOp op);
void xaccQueryAddActionMatch(Query * q, char * matchstring, 
                             int case_sens, int use_regexp, QueryOp op);
void xaccQueryAddAmountMatch(Query * q, double amount, 
                             amt_match_sgn_t amt_sgn,
                             amt_match_t how, QueryOp op);
void xaccQueryAddSharePriceMatch(Query * q, double amount, 
                                 amt_match_t how, QueryOp op);
void xaccQueryAddSharesMatch(Query * q, double amount, 
                             amt_match_t how, QueryOp op);
void xaccQueryAddDateMatch(Query * q, 
                           int syear, int smonth, int sday, 
                           int eyear, int emonth, int eday,
                           QueryOp op);
void xaccQueryAddDateMatchTS(Query * q, 
                             Timespec sts, Timespec ets,
                             QueryOp op);
void xaccQueryAddDateMatchTT(Query * q, 
                             time_t stt, time_t ett,
                             QueryOp op);
void xaccQueryAddMemoMatch(Query * q, char * matchstring, 
                           int case_sens, int use_regexp, QueryOp op);
void xaccQueryAddMiscMatch(Query * q, Predicate p, int how, int data,
                           QueryOp op);
void xaccQueryAddClearedMatch(Query * q, int how, 
                              QueryOp op);


/*******************************************************************
 *  sort-related functions 
 *******************************************************************/

void xaccQuerySetSortOrder(Query * q, sort_type_t primary, 
                           sort_type_t secondary, sort_type_t tertiary);
void xaccQuerySetSortIncreasing(Query * q, gboolean increasing);
void xaccQuerySetMaxSplits(Query * q, int n);


/*******************************************************************
 *  compatibility interface with old Query API 
 *******************************************************************/
time_t xaccQueryGetEarliestDateFound(Query * q);
time_t xaccQueryGetLatestDateFound(Query * q);


#endif
