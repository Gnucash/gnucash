/*
 * FILE:
 * Period.c
 *
 * FUNCTION:
 * Implement accounting periods.
 *
 * CAUTION: this is currently a non-functioning, experimental implementation
 * of the design described in src/doc/book.txt

Open questions: how do we deal with the backends ???
 *
 * HISTORY:
 * created by Linas Vepstas November 2001
 * Copyright (c) 2001 Linas Vepstas <linas@linas.org>
 */


#ifndef XACC_PERIOD_H
#define XACC_PERIOD_H

#include "gnc-book.h"
#include "gnc-engine.h"
#include "Query.h"

/* The gnc_book_partition() uses the result of the indicated query
 *    to partition an existing book into two parts.  It returns 
 *    a newly created book, containing a copy of all of the accounts,
 *    and it moves all of the transactions returned by the query to 
 *    the copied accounts.  

The intent is that the 'typical' query will be a date that splits 
the book into a 'before and after'; but in fact, any general query 
will do.

Note that this routine is 'special' in that it works hard to make sure
that the partitioned accounts, transactions and splits are really
moved to a new book -- things like entity tables must be cleared
and repopulated correctly.

An equity transfer is made ...
key-value pairs are set ...
The sched xactions are not copied ...

 */
GNCBook * gnc_book_partition (GNCBook *, Query *);

#endif XACC_PERIOD_H

#include "gnc-book-p.h"
#include "GroupP.h"
#include "TransactionP.h"


/* ================================================================ */

static void
reparent (Transaction *trans, GNCBook *book)
{
   GList *node;

   for (node = trans->splits; node; node = node->next)
   {
      Account *twin;
      Split *s = node->data;

      twin = xaccAccountLookupTwin (s->acc, book);
   }
}

/* ================================================================ */

GNCBook * 
gnc_book_partition (GNCBook *existing_book, Query *query)
{
   GList *split_list, *snode;
   GNCBook *partition_book;
   AccountGroup *part_topgrp;

   if (!existing_book || !query) return NULL;

   partition_book = gnc_book_new();

   /* first, copy all of the accounts */
   xaccGroupCopyGroup (partition_book->topgroup, existing_book->topgroup);

   /* next, run the query */
   split_list = xaccQueryGetSplitsUniqueTrans (query);

   /* and start moving transactions over */
   for (snode = split_list; snode; snode=snode->next)
   {
      GList *tnode;
      Split *s = snode->data;
      Transaction *trans = s->parent;

   }

   return partition_book;
}

/* ================================================================ */

/* ============================= END OF FILE ====================== */
