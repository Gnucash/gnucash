/*
 * FILE:
 * Period.c
 *
 * FUNCTION:
 * Implement accounting periods.
 *
 * CAUTION: this is currently a semi-functional, untested implementation
 * of the design described in src/doc/book.txt

Open questions: how do we deal with the backends ???
 *
 * HISTORY:
 * created by Linas Vepstas November 2001
 * Copyright (c) 2001 Linas Vepstas <linas@linas.org>
 */

#include "AccountP.h"
#include "gnc-book-p.h"
#include "gnc-engine-util.h"
#include "gnc-event-p.h"
#include "GroupP.h"
#include "kvp-util-p.h"
#include "Period.h"
#include "TransactionP.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_ENGINE;

/* ================================================================ */
/* Reparent transaction to new book, and do it so backends 
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

   /* First, copy the book's KVP tree */
   kvp_frame_delete (partition_book->kvp_data);
   partition_book->kvp_data = kvp_frame_copy (existing_book->kvp_data);

   /* Next, copy all of the accounts */
   xaccGroupCopyGroup (partition_book->topgroup, existing_book->topgroup);

   /* Next, run the query */
   xaccQuerySetGroup (query, existing_book->topgroup);
   split_list = xaccQueryGetSplitsUniqueTrans (query);

   /* And start moving transactions over */
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
/* find nearest equity account */

static Account *
find_nearest_equity_acct (Account *acc)
{
   AccountList *acc_list, *node;
   AccountGroup *parent;
   Account *next_up, *candidate;

   /* see if we can find an equity account that is peered to this account */
   parent = xaccAccountGetParent (acc);
   g_return_val_if_fail (parent, NULL);

   acc_list = xaccGroupGetAccountList (parent);
   for (node=acc_list; node; node=node->next)
   {
      candidate = (Account *) node->data;
      if (EQUITY == xaccAccountGetType (candidate)) return candidate;
   }

   /* If we got to here, we did not find a peer equity account. 
    * So go up one layer, and look there */
   next_up = xaccGroupGetParentAccount (parent);
   if (next_up) 
   {
      candidate = find_nearest_equity_acct (next_up);
      if (candidate) return candidate;
   }

   /* If we got to here, then we are at the top group, and there is no 
    * equity account to be found.  So we need to create one. */
   
   candidate = xaccMallocAccount (xaccGroupGetBook(parent));
   xaccAccountBeginEdit (candidate);
   xaccGroupInsertAccount (parent, candidate);
   xaccAccountSetType (candidate, EQUITY);
   xaccAccountSetName (candidate, xaccAccountGetTypeStr(EQUITY));
   xaccAccountCommitEdit (candidate);
   
   return candidate;
}

/* ================================================================ */
/* traverse all accounts, get account balances */

static void
add_closing_balances (AccountGroup *closed_grp, GNCBook *open_book)
{
   AccountList *acc_list, *node;

   /* walk accounts in closed book */
   acc_list = xaccGroupGetAccountList (closed_grp);
   for (node=acc_list; node; node=node->next)
   {
      Account * candidate = (Account *) node->data;
      GNCAccountType tip = xaccAccountGetType (candidate);

      if (EQUITY == tip)
      {
      }
   }

#if 0
   for each

   

#endif
}

/* ================================================================ */
/* split a book into two by date */

GNCBook * 
gnc_book_calve_period (GNCBook *existing_book, Timespec calve_date)
{
   Query *query;
   GNCBook *partition_book;
   kvp_frame *exist_cwd, *partn_cwd;
   kvp_value *vvv;
   Timespec ts;

   /* Get all transactions that are *earlier* than the calve date,
    * and put them in the new book.  */
   query = xaccMallocQuery();
   xaccQueryAddDateMatchTS (query, FALSE, calve_date, 
                                   TRUE, calve_date,
                                   QUERY_OR);
   partition_book = gnc_book_partition (existing_book, query);

   xaccFreeQuery (query);

   /* Now add the various identifying kvp's */
   /* cwd == 'current working directory' */
   exist_cwd = kvp_frame_get_frame_slash (existing_book->kvp_data, "/book/");
   partn_cwd = kvp_frame_get_frame_slash (partition_book->kvp_data, "/book/");
   
   /* Mark the boundary date between the books */
   vvv = kvp_value_new_timespec (calve_date);
   kvp_frame_set_slot_nc (exist_cwd, "start-date", vvv);
   kvp_frame_set_slot_nc (partn_cwd, "end-date", vvv);

   /* Mark partition as being closed */
   ts.tv_sec = time(0);
   ts.tv_nsec = 0;
   vvv = kvp_value_new_timespec (ts);
   kvp_frame_set_slot_nc (partn_cwd, "close-date", vvv);

   /* Set up pointers to each book from the other. */
   vvv = kvp_value_new_guid (&existing_book->guid);
   kvp_frame_set_slot_nc (partn_cwd, "next-book", vvv);

   vvv = kvp_value_new_guid (&partition_book->guid);
   kvp_frame_set_slot_nc (exist_cwd, "prev-book", vvv);

   return partition_book;
}

/* ============================= END OF FILE ====================== */
