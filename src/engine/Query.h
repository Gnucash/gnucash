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
  BY_NUM,
  BY_AMOUNT,
  BY_MEMO,
  BY_DESC,
  BY_NONE
} sort_type_t;  

typedef enum { PD_DATE, PD_AMOUNT, PD_ACCOUNT, 
               PD_TRANS, PD_SPLIT, PD_STRING, PD_MISC } pd_type_t;

typedef enum { ACCT_MATCH_ALL, ACCT_MATCH_ANY, ACCT_MATCH_NONE } acct_match_t;
typedef enum { AMT_MATCH_ATLEAST, AMT_MATCH_ATMOST, 
               AMT_MATCH_EXACTLY } amt_match_t;
typedef enum { AMT_SGN_MATCH_EITHER, AMT_SGN_MATCH_CREDIT, 
               AMT_SGN_MATCH_DEBIT } amt_match_sgn_t;

enum { STRING_MATCH_CASE=1, STRING_MATCH_REGEXP=2};

/* the Query makes a subset of all splits based on 3 things: 
 *   - an AND-OR tree of predicates which combine to make a 
 *     split filter 
 *   - a sorting order for the matched splits
 *   - a chop limit which gives the maximum number of sorted
 *     splits to return. */

typedef struct {
  /* terms is a list of the OR-terms in a sum-of-products 
   * logical expression. */
  GList *  terms;  
  
  /* sorting and chopping is independent of the search filter */
  sort_type_t primary_sort;
  sort_type_t secondary_sort;
  sort_type_t tertiary_sort;
  int         max_splits;
  
  /* cache the results so we don't have to run the whole search 
   * again until it's really necessary */
  int      changed;
  AccountGroup * acct_group;
  Split ** split_list;
} Query;

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
  pd_type_t    type;
  Transaction  * trans;
} TransPredicateData;

typedef struct {
  pd_type_t    type;
  Split        * split;
} SplitPredicateData;

typedef union { 
  pd_type_t            type;
  DatePredicateData    date;
  AmountPredicateData  amount;
  AccountPredicateData acct;
  TransPredicateData   trans;
  SplitPredicateData   split;
  StringPredicateData  str;
  MiscPredicateData    misc;
} PredicateData;

typedef int (* Predicate)(Split * foo, PredicateData * bar);

typedef struct {
  Predicate     p;
  PredicateData data;
  int           sense;
} QueryTerm;


Query   * xaccMallocQuery(void);
void    xaccInitQuery(Query * q, QueryTerm * initial_term);
void    xaccFreeQuery(Query *);

Query  * xaccQueryInvert(Query * q1);
Query  * xaccQueryMerge(Query * q1, Query * q2, QueryOp op);
void   xaccQuerySetGroup(Query * q, AccountGroup * group);
void   xaccQuerySwapTerms(Query * q1, Query * q2);
void   xaccQuerySingleTerm(Query * q, QueryTerm * qt);
void   xaccQueryClear(Query * q);
void   xaccQueryPurgeTerms(Query * q, pd_type_t type);

int    xaccQueryHasTerms(Query * q);

Split ** xaccQueryGetSplits(Query * q);

/*******************************************************************
 *  match-adding API 
 *******************************************************************/

void xaccQueryAddAccountMatch(Query * q, Account ** acclist,
                              acct_match_t how, QueryOp op);
void xaccQueryAddSingleAccountMatch(Query * q, Account * acct, 
                                    QueryOp op);

void xaccQueryAddTransMatch(Query * q, Transaction * t, int how, QueryOp op);
void xaccQueryAddSplitMatch(Query * q, Split * t, int how, QueryOp op);

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


/*******************************************************************
 *  predicates for standard match types
 *******************************************************************/

int  xaccAccountMatchPredicate(Split * s, PredicateData * pd);
int  xaccTransMatchPredicate(Split * s, PredicateData * pd);
int  xaccSplitMatchPredicate(Split * s, PredicateData * pd);
int  xaccDescriptionMatchPredicate(Split * s, PredicateData * pd);
int  xaccActionMatchPredicate(Split * s, PredicateData * pd);
int  xaccNumberMatchPredicate(Split * s, PredicateData * pd);
int  xaccAmountMatchPredicate(Split * s, PredicateData * pd);
int  xaccDateMatchPredicate(Split * s, PredicateData * pd);
int  xaccMemoMatchPredicate(Split * s, PredicateData * pd);
int  xaccSharePriceMatchPredicate(Split * s, PredicateData * pd);
int  xaccSharesMatchPredicate(Split * s, PredicateData * pd);

/*******************************************************************
 *  sort-related functions 
 *******************************************************************/

void xaccQuerySetSortOrder(Query * q, sort_type_t primary, 
                           sort_type_t secondary, sort_type_t tertiary);
void xaccQuerySetMaxSplits(Query * q, int n);


/*******************************************************************
 *  compatibility interface with old Query API 
 *******************************************************************/
time_t xaccQueryGetEarliestDateFound(Query * q);
time_t xaccQueryGetLatestDateFound(Query * q);


#endif
