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
 * Copyright (c) 2001, 2002 Linas Vepstas <linas@linas.org>
 */

#include "AccountP.h"
#include "BackendP.h"
#include "gnc-book-p.h"
#include "gnc-engine-util.h"
#include "gnc-event-p.h"
#include "GroupP.h"
#include "kvp-util-p.h"
#include "Period.h"
#include "TransactionP.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_BOOK;

/* ================================================================ */
/* Reparent transaction to new book.  This routine does this by 
 * deleting the transaction in the old book, and creating a copy
 * in the new book.  While technically correct, this is maybe too 
 * much churn on the backend ... 
 */

void
gnc_book_insert_trans_clobber (GNCBook *book, Transaction *trans)
{
   Transaction *newtrans;
   GList *node;

   if (!trans || !book) return;
   
   /* if this is the same book, its a no-op. */
   if (trans->book == book) return;

   newtrans = xaccDupeTransaction (trans);
   for (node = newtrans->splits; node; node = node->next)
   {
      Split *s = node->data;
      s->parent = newtrans;
   }

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
      else
      {
        /* force to null, so remove doesn't occur */
        xaccSplitSetAccount (s, NULL);  
        xaccAccountInsertSplit (twin, s);
        twin->balance_dirty = TRUE;
        twin->sort_dirty = TRUE;
      }
   }

   xaccTransCommitEdit (newtrans);
   gnc_engine_generate_event (&newtrans->guid, GNC_EVENT_CREATE);
}

/* ================================================================ */
/* Reparent transaction to new book.  This routine does this by simply
 * moving GUID's to the new book's entity tables.
 */

void
gnc_book_insert_trans (GNCBook *book, Transaction *trans)
{
   GList *node;

   if (!trans || !book) return;
   
   /* if this is the same book, its a no-op. */
   if (trans->book == book) return;

   /* fiddle the transaction into place in the new book */
   xaccTransBeginEdit (trans);

   xaccRemoveEntity (trans->book->entity_table, &trans->guid);
   trans->book = book;
   xaccStoreEntity(book->entity_table, trans, &trans->guid, GNC_ID_TRANS);

   for (node = trans->splits; node; node = node->next)
   {
      Account *twin;
      Split *s = node->data;

      /* move the split into the new book ... */
      xaccRemoveEntity (s->book->entity_table, &s->guid);
      s->book = book;
      xaccStoreEntity(book->entity_table, s, &s->guid, GNC_ID_SPLIT);

      /* find the twin account, and re-parent to that. */
      twin = xaccAccountLookupTwin (s->acc, book);
      if (!twin)
      {
         PERR ("near-fatal: twin account not found");
      }
      else
      {
        /* force to null, so remove doesn't occur */
        xaccSplitSetAccount (s, NULL);  
        xaccAccountInsertSplit (twin, s);
        twin->balance_dirty = TRUE;
        twin->sort_dirty = TRUE;
      }
   }

   xaccTransCommitEdit (trans);
   gnc_engine_generate_event (&trans->guid, GNC_EVENT_MODIFY);
}

/* ================================================================ */

void 
gnc_book_partition (GNCBook *dest_book, GNCBook *src_book, Query *query)
{
   Backend *be;
   time_t now;
   GList *split_list, *snode;

   if (!src_book || !dest_book || !query) return;
   ENTER (" src_book=%p dest_book=%p", src_book, dest_book);

   be = src_book->backend;
   if (be && be->book_transfer_begin)
   {
      (*be->book_transfer_begin)(be, dest_book);
   }
   
   /* First, copy the book's KVP tree */
   /* hack alert -- FIXME -- this should really be a merge, not a
    * clobber copy, but I am too lazy to write a kvp merge routine,
    * and it is not needed for the current usage. */
   kvp_frame_delete (dest_book->kvp_data);
   dest_book->kvp_data = kvp_frame_copy (src_book->kvp_data);

   /* Next, copy all of the accounts */
   /* hack alert -- FIXME -- this should really be a merge, not a
    * clobber copy, but I am too lazy to write an account-group merge 
    * routine, and it is not needed for the current usage. */
   xaccAccountGroupBeginEdit (dest_book->topgroup);
   xaccAccountGroupBeginEdit (src_book->topgroup);
   xaccGroupCopyGroup (dest_book->topgroup, src_book->topgroup);

   /* Next, run the query */
   xaccQuerySetGroup (query, src_book->topgroup);
   split_list = xaccQueryGetSplitsUniqueTrans (query);

   /* And start moving transactions over */
   for (snode = split_list; snode; snode=snode->next)
   {
      Split *s = snode->data;
      Transaction *trans = s->parent;

      gnc_book_insert_trans (dest_book, trans);
   }
   xaccAccountGroupCommitEdit (src_book->topgroup);
   xaccAccountGroupCommitEdit (dest_book->topgroup);

   /* make note of the sibling books */
   now = time(0);
   gnc_kvp_gemini (src_book->kvp_data, NULL, &dest_book->guid, now);
   gnc_kvp_gemini (dest_book->kvp_data, NULL, &src_book->guid, now);

   if (be && be->book_transfer_commit)
   {
      (*be->book_transfer_commit)(be, dest_book);
   }
   LEAVE (" ");
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
add_closing_balances (AccountGroup *closed_grp, 
                      GNCBook *open_book,
                      GNCBook *closed_book,
                      Account *equity_account,
                      Timespec *post_date, Timespec *date_entered, 
                      const char *desc)
{
   AccountList *acc_list, *node;

   if (!closed_grp) return;
   ENTER (" enter=%s post=%s desc=%s", gnc_print_date(*date_entered),
       gnc_print_date (*post_date), desc);

   /* walk accounts in closed book */
   acc_list = xaccGroupGetAccountList (closed_grp);
   for (node=acc_list; node; node=node->next)
   {
      kvp_frame *cwd;
      kvp_value *vvv;
      Account *twin;
      AccountGroup *childs;
      Account * candidate = (Account *) node->data;
      GNCAccountType tip = xaccAccountGetType (candidate);

      /* find the peer account of this account in the open book  */
      twin = xaccAccountLookupTwin (candidate, open_book);

      /* -------------------------------- */
      /* add KVP to open account, indicating the progenitor
       * of this account. */
      xaccAccountBeginEdit (twin);
      cwd = xaccAccountGetSlots (twin);
      cwd = kvp_frame_get_frame_slash (cwd, "/book/");

      vvv = kvp_value_new_guid (xaccAccountGetGUID (candidate));
      kvp_frame_set_slot_nc (cwd, "prev-acct", vvv);
      
      vvv = kvp_value_new_guid (&closed_book->guid);
      kvp_frame_set_slot_nc (cwd, "prev-book", vvv);
      
      xaccAccountCommitEdit (twin);

      /* -------------------------------- */
      /* add KVP to closed account, indicating where 
       * the next book is. */
      xaccAccountBeginEdit (candidate);
      cwd = xaccAccountGetSlots (candidate);
      cwd = kvp_frame_get_frame_slash (cwd, "/book/");

      vvv = kvp_value_new_guid (&open_book->guid);
      kvp_frame_set_slot_nc (cwd, "next-book", vvv);
      
      vvv = kvp_value_new_guid (xaccAccountGetGUID (twin));
      kvp_frame_set_slot_nc (cwd, "next-acct", vvv);

      /* -------------------------------- */
      /* We need to carry a balance on any account that is not
       * and income or expense or equity account */
      if ((INCOME != tip) && (EXPENSE != tip) && (EQUITY != tip)) 
      {
         Split *se, *st;
         Transaction *trans;
         Account *equity;
         gnc_numeric baln;

         baln = xaccAccountGetBalance (candidate);

         /* find the equity account into which we'll poke the 
          * balancing transaction */
         if (NULL == equity_account)
         {
            equity = find_nearest_equity_acct (twin);
         }
         else
         {
            equity = equity_account;
         }

         /* -------------------------------- */
         /* create the balancing transaction */
         trans = xaccMallocTransaction (open_book);
         xaccTransBeginEdit (trans);
         st = xaccMallocSplit(open_book);
         xaccAccountInsertSplit (twin, st);
         
         se = xaccMallocSplit(open_book);
         xaccAccountInsertSplit (equity, se);

         xaccSplitSetValue (st, baln);
         xaccSplitSetValue (se, gnc_numeric_neg(baln));

         xaccTransSetDatePostedTS (trans, post_date);
         xaccTransSetDateEnteredTS (trans, date_entered);
         xaccTransSetDescription (trans, desc);

         /* add KVP data showing where the balancing 
          * transaction came from */
         cwd = xaccTransGetSlots (trans);
         cwd = kvp_frame_get_frame_slash (cwd, "/book/");

         vvv = kvp_value_new_guid (&closed_book->guid);
         kvp_frame_set_slot_nc (cwd, "closed-book", vvv);
         
         vvv = kvp_value_new_guid (xaccAccountGetGUID(candidate));
         kvp_frame_set_slot_nc (cwd, "closed-acct", vvv);
         
         xaccTransCommitEdit (trans);

         /* -------------------------------- */
         /* add KVP to closed account, indicating where the
          * balance was carried forward to. */
         xaccAccountBeginEdit (candidate);
         cwd = xaccAccountGetSlots (candidate);
         cwd = kvp_frame_get_frame_slash (cwd, "/book/");

         vvv = kvp_value_new_guid (xaccTransGetGUID(trans));
         kvp_frame_set_slot_nc (cwd, "balancing-trans", vvv);
         xaccAccountCommitEdit (candidate);
      }

      /* we left an open dangling above ... */
      xaccAccountCommitEdit (candidate);


      /* recurse down to the children */
      childs = xaccAccountGetChildren(candidate);
      if (childs) 
      {
         PINFO ("add closing baln to subaccts of %s", 
                 candidate->description);
         add_closing_balances (childs, open_book, closed_book,
                          equity_account,
                          post_date, date_entered, desc);
      }
   }
   LEAVE (" ");
}

/* ================================================================ */
/* split a book into two by date */

GNCBook * 
gnc_book_close_period (GNCBook *existing_book, Timespec calve_date,
                       Account *equity_account,
                       const char * memo)
{
   Query *query;
   GNCBook *closing_book;
   kvp_frame *exist_cwd, *partn_cwd;
   kvp_value *vvv;
   Timespec ts;

   if (!existing_book) return NULL;
   ENTER (" date=%s memo=%s", gnc_print_date(calve_date), memo);

   /* Get all transactions that are *earlier* than the calve date,
    * and put them in the new book.  */
   query = xaccMallocQuery();
   xaccQueryAddDateMatchTS (query, FALSE, calve_date, 
                                   TRUE, calve_date,
                                   QUERY_OR);
   closing_book = gnc_book_new();
   closing_book->book_open = 'n';
   gnc_book_partition (closing_book, existing_book, query);

   xaccFreeQuery (query);

   /* Now add the various identifying kvp's */
   /* cwd == 'current working directory' */
   exist_cwd = kvp_frame_get_frame_slash (existing_book->kvp_data, "/book/");
   partn_cwd = kvp_frame_get_frame_slash (closing_book->kvp_data, "/book/");
   
   /* Mark the boundary date between the books */
   vvv = kvp_value_new_timespec (calve_date);
   kvp_frame_set_slot_nc (exist_cwd, "open-date", vvv);
   kvp_frame_set_slot_nc (partn_cwd, "close-date", vvv);

   /* Mark partition as being closed */
   ts.tv_sec = time(0);
   ts.tv_nsec = 0;
   vvv = kvp_value_new_timespec (ts);
   kvp_frame_set_slot_nc (partn_cwd, "log-date", vvv);

   /* Set up pointers to each book from the other. */
   vvv = kvp_value_new_guid (&existing_book->guid);
   kvp_frame_set_slot_nc (partn_cwd, "next-book", vvv);

   vvv = kvp_value_new_guid (&closing_book->guid);
   kvp_frame_set_slot_nc (exist_cwd, "prev-book", vvv);

   /* add in transactions to equity accounts that will
    * hold the colsing balances */
   add_closing_balances (gnc_book_get_group(closing_book), 
                        existing_book, closing_book,
                        equity_account,
                        &calve_date, &ts, memo);
   LEAVE (" ");
   return closing_book;
}

/* ============================= END OF FILE ====================== */
