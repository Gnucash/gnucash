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

This routine intentionally does not copy scheduled/recurring 
transactions.

TBD:
-- Make an equity transfer so that we can carry forward the balances.
-- set kvp values indicating that the books were split.

 */
GNCBook * gnc_book_partition (GNCBook *, Query *);

/* The gnc_book_insert_trans() routine takes an existing transaction
 *    that is located in one book, and moves it to another book.
 *    It moves all of the splits as well.  In the course of the 
 *    move, the transaction is literally deleted from the first 
 *    book as its placed into the second.  The transaction and
 *    split GUID's are not changed in the move.  This routine 
 *    assumes that twin accounts already exist in both books 
 *    (and can be located with the standard twining proceedure).
 */

void gnc_book_insert_trans (GNCBook *book, Transaction *trans);

#endif /* XACC_PERIOD_H */

#include "AccountP.h"
#include "gnc-book-p.h"
#include "gnc-engine-util.h"
#include "gnc-event-p.h"
#include "GroupP.h"
#include "kvp-util-p.h"
#include "TransactionP.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_ENGINE;

/* ================================================================ */
/* reparent transaction to new book, and do it so backends 
 * handle it correctly.
*/

void
gnc_book_insert_trans (GNCBook *book, Transaction *trans)
{
   Transaction *newtrans;
   GList *node;

   if (!trans || !book) return;
   
   /* if this is the same book, its a no-op. */
   if (trans->book == book) return;

   newtrans = xaccDupeTransaction (trans);

   /* Utterly wipe out the transaction from the old book. */
   xaccTransBeginEdit (trans);
   xaccTransDestroy (trans);
   xaccTransCommitEdit (trans);

   /* fiddle the transaction into place in the new book */
   xaccStoreEntity(book->entity_table, newtrans, &newtrans->guid, GNC_ID_TRANS);
   newtrans->book = book;

   xaccTransBeginEdit (newtrans);
   for (node = newtrans->splits; node; node = node->next)
   {
      Account *twin;
      Split *s = node->data;

      /* move the split into the new book ... */
      s->book = book;
      xaccStoreEntity(book->entity_table, s, &s->guid, GNC_ID_SPLIT);

      /* find the twin account, and re-parent to that. */
      twin = xaccAccountLookupTwin (s->acc, book);
      if (!twin)
      {
         PERR ("near-fatal: twin account not found");
      }

      /* force to null, so remove doesn't occur */
      xaccSplitSetAccount (s, NULL);  
      xaccAccountInsertSplit (twin, s);
      twin->balance_dirty = TRUE;
      twin->sort_dirty = TRUE;
   }

   xaccTransCommitEdit (newtrans);
   gnc_engine_generate_event (&newtrans->guid, GNC_EVENT_CREATE);

}

/* ================================================================ */

GNCBook * 
gnc_book_partition (GNCBook *existing_book, Query *query)
{
   time_t now;
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
   xaccAccountGroupBeginEdit (partition_book->topgroup);
   xaccAccountGroupBeginEdit (existing_book->topgroup);
   for (snode = split_list; snode; snode=snode->next)
   {
      GList *tnode;
      Split *s = snode->data;
      Transaction *trans = s->parent;

      gnc_book_insert_trans (partition_book, trans);

   }
   xaccAccountGroupCommitEdit (existing_book->topgroup);
   xaccAccountGroupCommitEdit (partition_book->topgroup);

   /* make note of the sibling books */
   now = time(0);
   gnc_kvp_gemini (existing_book->kvp_data, NULL, &partition_book->guid, now);
   gnc_kvp_gemini (partition_book->kvp_data, NULL, &existing_book->guid, now);

   return partition_book;
}

/* ================================================================ */

/* ============================= END OF FILE ====================== */
