/********************************************************************\
 * QueryP.h : private, non-portable Query API structs               *
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

#ifndef GNUCASH_QUERY_P_H
#define GNUCASH_QUERY_P_H

#include "Query.h"

#if 0

typedef struct
{
    pd_type_t       type;
    pr_type_t       term_type;
    int             sense;
} BasePredicateData;

typedef struct
{
    pd_type_t       type;
    pr_type_t       term_type;
    int             sense;
    acct_match_t    how;
    AccountList     *accounts;
    AccountGUIDList *account_guids;
} AccountPredicateData;

typedef struct
{
    pd_type_t       type;
    pr_type_t       term_type;
    int             sense;
    amt_match_t     how;
    amt_match_sgn_t amt_sgn;
    double          amount;
} AmountPredicateData;

typedef struct
{
    pd_type_t       type;
    pr_type_t       term_type;
    int             sense;
    balance_match_t how;
} BalancePredicateData;

typedef struct
{
    pd_type_t       type;
    pr_type_t       term_type;
    int             sense;
    book_match_t    how;
    BookList        *books;
    BookGUIDList    *book_guids;
} BookPredicateData;

typedef struct
{
    pd_type_t       type;
    pr_type_t       term_type;
    int             sense;
    cleared_match_t how;
} ClearedPredicateData;

typedef struct
{
    pd_type_t       type;
    pr_type_t       term_type;
    int             sense;
    GUID            guid;
    QofIdType       id_type;
} GUIDPredicateData;

typedef struct
{
    pd_type_t       type;
    pr_type_t       term_type;
    int             sense;
    int             use_start;
    Timespec        start;
    int             use_end;
    Timespec        end;
} DatePredicateData;

typedef struct
{
    pd_type_t         type;
    pr_type_t         term_type;
    int               sense;
    kvp_match_t       how;
    kvp_match_where_t where;
    GSList           *path;
    KvpValue        *value;
} KVPPredicateData;

typedef struct
{
    pd_type_t       type;
    pr_type_t       term_type;
    int             sense;
    int             how;
    int             data;
} MiscPredicateData;

typedef struct
{
    pd_type_t       type;
    pr_type_t       term_type;
    int             sense;
    int             case_sens;
    int             use_regexp;
    char           *matchstring;
    regex_t         compiled;
} StringPredicateData;

typedef union
{
    pd_type_t            type;
    BasePredicateData    base;
    AccountPredicateData acct;
    AmountPredicateData  amount;
    BalancePredicateData balance;
    BookPredicateData    book;
    ClearedPredicateData cleared;
    DatePredicateData    date;
    GUIDPredicateData    guid;
    KVPPredicateData     kvp;
    StringPredicateData  str;
    MiscPredicateData    misc;
} PredicateData;

typedef int (* Predicate)(Split * to_test, PredicateData * test_data);

typedef struct
{
    PredicateData data;
    Predicate     p;
} QueryTerm;

void xaccQueryAddMiscMatch(Query * q, Predicate p, int how, int data,
                           QofQueryOp op);

void xaccQueryAddPredicate (Query * q, PredicateData *pred, QofQueryOp op);

/* This is useful for network systems */
Predicate xaccQueryGetPredicate (pr_type_t term_type);

#endif /* 0 */

#endif /* GNUCASH_QUERY_P_H */
