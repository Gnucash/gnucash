/*
 * FILE:
 * Query.h
 *
 * DESCRIPTION:
 * Provide a simple query engine interface.
 *
 * HISTORY:
 * created by Linas Vepstas Sept 1998
 * Copyright (c) 1998 Linas Vepstas
 */

#ifndef __GNUCASH_QUERY_H__
#define __GNUCASH_QUERY_H__

#include "Account.h"
#include "Transaction.h"

typedef struct _Query Query;

/* sorting orders */
enum {
  BY_DATE,
  BY_NUM,
  BY_AMOUNT
};

Query * xaccMallocQuery (void);
void    xaccInitQuery (Query *);
void    xaccFreeQuery (Query *);

/* The xaccSetAccountList() method is used to define the set
 *    of accounts the should be queried.
 */
void xaccQuerySetAccounts (Query *, Account **list);
void xaccQueryAddAccount (Query *, Account *acc);

/* The xaccQuerySetMaxSplits() method sets the maximum number
 *    of splits to return as a result of a query.
 */
void  xaccQuerySetMaxSplits (Query *, int);

/* The xaccQueryGetSplits() method returns a list of splits
 * matching the query and sorting criteria previously set up.
 */
Split ** xaccQueryGetSplits (Query *);

#endif /* __GNUCASH_QUERY_H__ */
