/********************************************************************\
 * Period.c -- Implement accounting Periods                         *
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
/*
 * FILE:
 * Period.c
 *
 * FUNCTION:
 * Implement accounting periods, using design described in 
 * src/doc/books.txt
 *
 * CAUTION: probably buggy.
 *
 * HISTORY:
 * Created by Linas Vepstas November 2001
 * Copyright (c) 2001-2003 Linas Vepstas <linas@linas.org>
 */

#include "AccountP.h"
#include "gnc-engine-util.h"
#include "gnc-event-p.h"
#include "gnc-lot.h"
#include "gnc-lot-p.h"
#include "gnc-pricedb.h"
#include "Group.h"
#include "GroupP.h"
#include "kvp-util-p.h"
#include "Period.h"
#include "TransactionP.h"
#include "qofbackend-p.h"
#include "qofbook.h"
#include "qofbook-p.h"
#include "qofid-p.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_BOOK;

/* ================================================================ */
/* Reparent transaction to new book.  This routine does this by 
 * deleting the transaction in the old book, and creating a copy
 * in the new book.  While technically correct, this is maybe too 
 * much churn on the backend ... 
 */

void
gnc_book_insert_trans_clobber (QofBook *book, Transaction *trans)
{
   Transaction *newtrans;
   GList *node;

   if (!trans || !book) return;
   
   /* If this is the same book, its a no-op. */
   if (trans->book == book) return;

   ENTER ("trans=%p %s", trans, trans->description);
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
   qof_entity_store(book->entity_table, newtrans, &newtrans->guid, GNC_ID_TRANS);
   newtrans->book = book;

   xaccTransBeginEdit (newtrans);
   for (node = newtrans->splits; node; node = node->next)
   {
      Account *twin;
      Split *s = node->data;

      /* move the split into the new book ... */
      s->book = book;
      qof_entity_store(book->entity_table, s, &s->guid, GNC_ID_SPLIT);

      /* find the twin account, and re-parent to that. */
      twin = xaccAccountLookupTwin (s->acc, book);
      if (!twin)
      {
         PERR ("near-fatal: twin account not found");
      }
      else
      {
        xaccAccountInsertSplit (twin, s);
        twin->balance_dirty = TRUE;
        twin->sort_dirty = TRUE;
      }
   }

   xaccTransCommitEdit (newtrans);
   gnc_engine_generate_event (&newtrans->guid, GNC_ID_TRANS, GNC_EVENT_CREATE);
   LEAVE ("trans=%p %s", trans, trans->description);
}

/* ================================================================ */
/* Reparent transaction to new book.  This routine does this by 
 * moving GUID's to the new book's entity tables.
 */

void
gnc_book_insert_trans (QofBook *book, Transaction *trans)
{
   GList *node;

   if (!trans || !book) return;
   
   /* If this is the same book, its a no-op. */
   if (trans->book == book) return;

   /* If the old and new book don't share backends, then clobber-copy;
    * i.e. destroy it in one backend, create it in another.  */
   if (book->backend != trans->book->backend)
   {
      gnc_book_insert_trans_clobber (book, trans);
      return;
   }
   ENTER ("trans=%p %s", trans, trans->description);

   /* Fiddle the transaction into place in the new book */
   xaccTransBeginEdit (trans);

   qof_entity_remove (trans->book->entity_table, &trans->guid);
   trans->book = book;
   qof_entity_store(book->entity_table, trans, &trans->guid, GNC_ID_TRANS);

   for (node = trans->splits; node; node = node->next)
   {
      Account *twin;
      Split *s = node->data;

      /* Move the splits over (only if they haven't already been moved). */
      if (s->book != book)
      {
         qof_entity_remove (s->book->entity_table, &s->guid);
         s->book = book;
         qof_entity_store(book->entity_table, s, &s->guid, GNC_ID_SPLIT);
      }

      /* Find the twin account, and re-parent to that. */
      twin = xaccAccountLookupTwin (s->acc, book);
      if (!twin)
      {
         PERR ("near-fatal: twin account not found");
      }
      else
      {
        /* Move the split too, if it hasn't been moved already */
        if (s->acc != twin)
        {
           xaccAccountInsertSplit (twin, s);
           twin->balance_dirty = TRUE;
           twin->sort_dirty = TRUE;
        }
      }
   }

   xaccTransCommitEdit (trans);
   gnc_engine_generate_event (&trans->guid, GNC_ID_TRANS, GNC_EVENT_MODIFY);
   LEAVE ("trans=%p %s", trans, trans->description);
}

/* ================================================================ */
/* Reparent lot to new book.  This routine does this by 
 * completely deleting and recreating the lot.
 */

void
gnc_book_insert_lot_clobber (QofBook *book, GNCLot *lot)
{
   PERR ("Not Implemented");
}

/* ================================================================ */
/* Reparent lot to new book.  This routine does this by 
 * moving GUID's to the new book's entity tables.
 */

void
gnc_book_insert_lot (QofBook *book, GNCLot *lot)
{
   SplitList *snode;
   Account *twin;
   if (!lot || !book) return;
   
   /* If this is the same book, its a no-op. */
   if (lot->book == book) return;

   if (book->backend != lot->book->backend)
   {
      gnc_book_insert_lot_clobber (book, lot);
      return;
   }
   ENTER ("lot=%p", lot);
   qof_entity_remove (lot->book->entity_table, &lot->guid);
   lot->book = book;
   qof_entity_store(book->entity_table, lot, &lot->guid, GNC_ID_LOT);

   /* Move the splits over (only if they haven't already been moved). */
   for (snode = lot->splits; snode; snode=snode->next)
   {
      Split *s = snode->data;
      if (s->book != book)
      {
         qof_entity_remove (s->book->entity_table, &s->guid);
         s->book = book;
         qof_entity_store(book->entity_table, s, &s->guid, GNC_ID_SPLIT);
      }
   }

   twin = xaccAccountLookupTwin (lot->account, book);
   if (!twin)
   {
      PERR ("near-fatal: twin account not found");
   }
   else
   {
      xaccAccountInsertLot (twin, lot);
   }
   LEAVE ("lot=%p", lot);
}

/* ================================================================ */
/* The following routines determine whether a given lot or 
 * transaction is linked or related to another lot that is 'open'.
 * These return true if so.
 *
 * An 'open transaction' is a transaction that has a split 
 * that belongs to an 'open lot'.  An 'open lot' is one that
 * is not closed, OR ONE THAT HAS a split in it that belongs to 
 * an open transaction. 
 *
 * The need for this recursive definition is that some lots, 
 * even though themselves closed, are participants in transactions
 * that cannot be moved to a closed book, and thus, by association 
 * can't be moved either.
 *
 * Lots contain pointers to splits, and transactions contain 
 * pointers to splits.  Together, these form a graph, which may
 * be cyclic.  We want to walk the entire graph, and determine
 * whether there are any open lots in it.  The walk must be 
 * recursive,  and because it might be cyclic, we use a marker
 * to break the cycles.  
 */

static gboolean trans_has_open_lot_tree (Transaction *trans);
static gboolean lot_has_open_trans_tree (GNCLot *lot);

static gboolean
trans_has_open_lot_tree (Transaction *trans)
{
   SplitList *split_list, *node;

   if (trans->marker) return FALSE;
   trans->marker = 1;

   split_list = xaccTransGetSplitList (trans);
   for (node = split_list; node; node=node->next)
   {
      Split *s = node->data;
      GNCLot *lot = xaccSplitGetLot(s);
      if (NULL == lot) continue;
      if (FALSE == gnc_lot_is_closed(lot)) return TRUE;
      if (lot_has_open_trans_tree (lot)) return TRUE;
   }
   return FALSE;
}

static gboolean 
lot_has_open_trans_tree (GNCLot *lot)
{
   SplitList *split_list, *snode;

   if (lot->marker) return FALSE;
   lot->marker = 1;

   if (FALSE == gnc_lot_is_closed(lot)) return TRUE;

   split_list = gnc_lot_get_split_list (lot);
   for (snode = split_list; snode; snode=snode->next)
   {
      Split *s = snode->data;
      Transaction *trans = s->parent;
      if (trans_has_open_lot_tree (trans)) return TRUE;
   }
   return FALSE;
}

/* ================================================================ */
/* The following routines remove 'open lots' and 'open transactions'
 * from the lists passed in.
 */

static LotList *
lot_list_preen_open_lots (LotList *lot_list)
{
   LotList *lnode;

   for (lnode=lot_list; lnode; )
   {
      GNCLot *lot = lnode->data;
      LotList *lnext = lnode->next;

      if (lot_has_open_trans_tree (lot))
      {
         lot_list = g_list_remove_link (lot_list, lnode);
         /* XXX freeing this node somehow leads to glib g_list
          * memory corruption which later takes down the system. 
          * I don't see why.  */
         /* g_list_free_1 (lnode); */
      }
      lnode = lnext;
   }
   return lot_list;
}

static TransList *
trans_list_preen_open_lots (TransList *trans_list)
{
   TransList *tnode;

   for (tnode=trans_list; tnode; )
   {
      Transaction *trans = tnode->data;
      TransList *tnext = tnode->next;

      if (trans_has_open_lot_tree (trans))
      {
         trans_list = g_list_remove_link (trans_list, tnode);
         /* XXX freeing this node somehow leads to glib g_list
          * memory corruption which later takes down the system. 
          * I don't see why.  */
         /* g_list_free_1 (tnode); */
      }
      tnode = tnext;
   }
   return trans_list;
}

/* ================================================================ */
/* clear the markers for the above routines */

static void
clear_markers (AccountGroup *grp)
{
   GList *node;

   if (!grp) return;
                                                                                
   for (node = grp->accounts; node; node = node->next)
   {
      Account *account = node->data;
      GList *lp;
                                                                                
      /* recursively do sub-accounts */
      clear_markers (account->children);
                                                                                
      for (lp = account->splits; lp; lp = lp->next)
      {
        Split *s = lp->data;
        Transaction *trans = s->parent;
        GNCLot *lot = s->lot;
        trans->marker = 0;
        if (lot) lot->marker = 0;
      }
   }
}

/* ================================================================ */
/* Return a unique list of lots that are involved with the listed
 * transactions.
 */

static LotList *
create_lot_list_from_trans_list (TransList *trans_list)
{
   LotList *lot_list = NULL;
   TransList *tnode;

   for (tnode=trans_list; tnode; tnode=tnode->next)
   {
      Transaction *trans = tnode->data;
      SplitList *split_list = xaccTransGetSplitList (trans);
      SplitList *snode;
      for (snode = split_list; snode; snode=snode->next)
      {
         Split *s = snode->data;
         GNCLot *lot = xaccSplitGetLot(s);
         if (NULL == lot) continue;
         if (g_list_find (lot_list, lot)) continue;
         lot_list = g_list_prepend (lot_list, lot);
      }
   }
   return lot_list;
}

/* ================================================================ */

void 
gnc_book_partition_pricedb (QofBook *dest_book, QofBook *src_book, QofQuery *query)
{
   GNCPriceDB *src_pdb, *dest_pdb;
   GList *price_list, *pnode;

   if (!src_book || !dest_book || !query) return;
   ENTER (" src_book=%p dest_book=%p", src_book, dest_book);

   src_pdb = gnc_pricedb_get_db (src_book);
   dest_pdb = gnc_pricedb_get_db (dest_book);

   gnc_pricedb_begin_edit (src_pdb);
   gnc_pricedb_begin_edit (dest_pdb);

   qof_query_set_book (query, src_book);
   price_list = qof_query_run (query);

   for (pnode = price_list; pnode; pnode=pnode->next)
   {
      GNCPrice *pr = pnode->data;
printf ("duude got price =%p\n", pr);
      gnc_price_ref (pr);
      gnc_pricedb_remove_price (src_pdb, pr);
      gnc_pricedb_add_price (dest_pdb, pr);
      gnc_price_unref (pr);
   }

   gnc_pricedb_commit_edit (dest_pdb);
   gnc_pricedb_commit_edit (src_pdb);

   LEAVE (" src_book=%p dest_book=%p", src_book, dest_book);
}

/* ================================================================ */

void 
gnc_book_partition_txn (QofBook *dest_book, QofBook *src_book, QofQuery *query)
{
   gnc_commodity_table *src_tbl, *dst_tbl;
   AccountGroup *src_grp, *dst_grp;
   time_t now;
   TransList *trans_list, *tnode;
   LotList *lot_list, *lnode;

   if (!src_book || !dest_book || !query) return;
   ENTER (" src_book=%p dest_book=%p", src_book, dest_book);

   /* First, copy the book's KVP tree */
   /* hack alert -- FIXME -- this should really be a merge, not a
    * clobber copy, but I am too lazy to write a kvp merge routine,
    * and it is not needed for the current usage. */
   kvp_frame_delete (dest_book->kvp_data);
   dest_book->kvp_data = kvp_frame_copy (src_book->kvp_data);

   /* Next, copy the commodity tables */
   src_tbl = gnc_commodity_table_get_table (src_book);
   dst_tbl = gnc_commodity_table_get_table (dest_book);
   gnc_commodity_table_copy (dst_tbl, src_tbl);

   /* Next, copy all of the accounts */
   /* hack alert -- FIXME -- this should really be a merge, not a
    * clobber copy, but I am too lazy to write an account-group merge 
    * routine, and it is not needed for the current usage. */
   src_grp = xaccGetAccountGroup (src_book);
   dst_grp = xaccGetAccountGroup (dest_book);
   xaccAccountGroupBeginEdit (dst_grp);
   xaccAccountGroupBeginEdit (src_grp);
   xaccGroupCopyGroup (dst_grp, src_grp);
   xaccAccountGroupCommitEdit (src_grp);
   xaccAccountGroupCommitEdit (dst_grp);

   /* Next, run the query */
   xaccAccountGroupBeginEdit (dst_grp);
   xaccAccountGroupBeginEdit (src_grp);
   qof_query_set_book (query, src_book);
   trans_list = qof_query_run (query);

   /* Preen: remove open lots/ open trnasactions */
   clear_markers (src_grp);
   trans_list = trans_list_preen_open_lots (trans_list);
   lot_list = create_lot_list_from_trans_list (trans_list);
   lot_list = lot_list_preen_open_lots (lot_list);

   /* Move closed lots over to destination. Do this before moving 
    * the txn's, so that the lots don't get trashed.  */
   for (lnode = lot_list; lnode; lnode = lnode->next)
   {
      GNCLot *lot = lnode->data;
      gnc_book_insert_lot (dest_book, lot);
   }

   /* Move the transactions over to the destination book. */
   for (tnode = trans_list; tnode; tnode=tnode->next)
   {
      Transaction *trans = tnode->data;
      gnc_book_insert_trans (dest_book, trans);
   }

   xaccAccountGroupCommitEdit (src_grp);
   xaccAccountGroupCommitEdit (dst_grp);

   /* Make note of the sibling books */
   now = time(0);
   gnc_kvp_gemini (src_book->kvp_data, now, "book_guid", &dest_book->guid, NULL);
   gnc_kvp_gemini (dest_book->kvp_data, now, "book_guid", &src_book->guid, NULL);

   LEAVE (" ");
}

/* ================================================================ */
/* Find nearest equity account */

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
      if ((EQUITY == xaccAccountGetType (candidate)) &&
          gnc_commodity_equiv(xaccAccountGetCommodity(acc),
                              xaccAccountGetCommodity(candidate)))
      {
         return candidate;
      }
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
   xaccAccountSetCommodity (candidate, xaccAccountGetCommodity(acc));
   xaccAccountCommitEdit (candidate);
   
   return candidate;
}

/* ================================================================ */
/* Traverse all accounts, get account balances */

static void
add_closing_balances (AccountGroup *closed_grp, 
                      QofBook *open_book,
                      QofBook *closed_book,
                      Account *equity_account,
                      Timespec *post_date, Timespec *date_entered, 
                      const char *desc)
{
   AccountList *acc_list, *node;

   if (!closed_grp) return;
   ENTER (" enter=%s post=%s desc=%s", gnc_print_date(*date_entered),
       gnc_print_date (*post_date), desc);
   xaccAccountBeginEdit (equity_account);

   /* Walk accounts in closed book */
   acc_list = xaccGroupGetAccountList (closed_grp);
   for (node=acc_list; node; node=node->next)
   {
      KvpFrame *cwd;
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
      kvp_frame_set_guid (cwd, "/book/prev-acct", xaccAccountGetGUID (candidate));
      kvp_frame_set_guid (cwd, "/book/prev-book", &closed_book->guid);

      xaccAccountSetSlots_nc (twin, twin->kvp_data);
      
      /* -------------------------------- */
      /* add KVP to closed account, indicating where 
       * the next book is. */
      xaccAccountBeginEdit (candidate);
      cwd = xaccAccountGetSlots (candidate);
      kvp_frame_set_guid (cwd, "/book/next-book", &open_book->guid);
      kvp_frame_set_guid (cwd, "/book/next-acct", xaccAccountGetGUID (twin));

      xaccAccountSetSlots_nc (candidate, candidate->kvp_data);

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

         /* Find the equity account into which we'll poke the 
          * balancing transaction */
         if (NULL == equity_account)
         {
            equity = find_nearest_equity_acct (twin);
            xaccAccountBeginEdit (equity);
         }
         else
         {
            equity = equity_account;
         }

         /* -------------------------------- */
         /* Create the balancing transaction */
         trans = xaccMallocTransaction (open_book);
         xaccTransBeginEdit (trans);

         xaccTransSetDatePostedTS (trans, post_date);
         xaccTransSetDateEnteredTS (trans, date_entered);
         xaccTransSetDescription (trans, desc);
         xaccTransSetCurrency (trans, xaccAccountGetCommodity(equity));

         st = xaccMallocSplit(open_book);
         xaccTransAppendSplit(trans, st);
         xaccAccountInsertSplit (twin, st);
         
         se = xaccMallocSplit(open_book);
         xaccTransAppendSplit(trans, se);
         xaccAccountInsertSplit (equity, se);

         xaccSplitSetAmount (st, baln);
         xaccSplitSetValue (st, baln);
         xaccSplitSetAmount (se, gnc_numeric_neg(baln));
         xaccSplitSetValue (se, gnc_numeric_neg(baln));

         /* Add KVP data showing where the balancing 
          * transaction came from */
         cwd = xaccTransGetSlots (trans);
         kvp_frame_set_guid (cwd, "/book/closed-book", &closed_book->guid);
         kvp_frame_set_guid (cwd, "/book/closed-acct", xaccAccountGetGUID(candidate));
         
         xaccTransCommitEdit (trans);

         if (NULL == equity_account)
         {
            xaccAccountCommitEdit (equity);
         }
         /* -------------------------------- */
         /* Add KVP to closed account, indicating where the
          * balance was carried forward to. */
         cwd = xaccAccountGetSlots (candidate);
         kvp_frame_set_guid (cwd, "/book/balancing-trans", xaccTransGetGUID(trans));
      }

      /* We left an open dangling above ... */
      xaccAccountCommitEdit (candidate);
      xaccAccountCommitEdit (twin);

      /* Recurse down to the children */
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
   xaccAccountCommitEdit (equity_account);
   LEAVE (" ");
}

/* ================================================================ */

static void 
period_begin_edit (QofBook *src_book, QofBook *dest_book)
{
   QofBackend *be;
   be = src_book->backend;
   if (be && be->begin)
   {
      (*be->begin)(be, GNC_ID_PERIOD, dest_book);
   }
}
   
static void 
period_commit_edit (QofBook *src_book, QofBook *dest_book)
{
   QofBackend *be;
   be = src_book->backend;
   if (be && be->commit)
   {
      (*be->commit)(be, GNC_ID_PERIOD, dest_book);
   }
}

/* ================================================================ */
/* Split a book into two by date */

QofBook * 
gnc_book_close_period (QofBook *existing_book, Timespec calve_date,
                       Account *equity_account,
                       const char * memo)
{
   QofQuery *txn_query, *prc_query;
   QofQueryPredData *pred_data;
   GSList *param_list;
   QofBook *closing_book;
   KvpFrame *exist_cwd, *partn_cwd;
   Timespec ts;

   if (!existing_book) return NULL;
   ENTER (" date=%s memo=%s", gnc_print_date(calve_date), memo);

   /* Setup closuing book */
   closing_book = qof_book_new();
   qof_book_set_backend (closing_book, existing_book->backend);
   closing_book->book_open = 'n';

   period_begin_edit (existing_book, closing_book);

   /* Get all transactions that are *earlier* than the calve date,
    * and put them in the new book.  */
   txn_query = qof_query_create_for (GNC_ID_TRANS);
   pred_data = qof_query_date_predicate (QOF_COMPARE_LTE,
                                         QOF_DATE_MATCH_NORMAL,
                                         calve_date);
   param_list = qof_query_build_param_list (TRANS_DATE_POSTED, NULL);
   qof_query_add_term (txn_query, param_list, pred_data, QOF_QUERY_FIRST_TERM);

   gnc_book_partition_txn (closing_book, existing_book, txn_query);
   qof_query_destroy (txn_query);

   /* Move prices over too */
   prc_query = qof_query_create_for (GNC_ID_PRICE);
   pred_data = qof_query_date_predicate (QOF_COMPARE_LTE,
                                         QOF_DATE_MATCH_NORMAL,
                                         calve_date);
   param_list = qof_query_build_param_list (PRICE_DATE, NULL);
   qof_query_add_term (prc_query, param_list, pred_data, QOF_QUERY_FIRST_TERM);

   gnc_book_partition_pricedb (closing_book, existing_book, prc_query);
   qof_query_destroy (prc_query);

   /* Now add the various identifying kvp's */
   /* cwd == 'current working directory' */
   exist_cwd = existing_book->kvp_data;
   partn_cwd = closing_book->kvp_data;
   
   /* Mark the boundary date between the books */
   kvp_frame_set_timespec (exist_cwd, "/book/open-date", calve_date);
   kvp_frame_set_timespec (partn_cwd, "/book/close-date", calve_date);

   /* Mark partition as being closed */
   ts.tv_sec = time(0);
   ts.tv_nsec = 0;
   kvp_frame_set_timespec (partn_cwd, "/book/log-date", ts);

   /* Set up pointers to each book from the other. */
   kvp_frame_set_guid (partn_cwd, "/book/next-book", &existing_book->guid);
   kvp_frame_set_guid (exist_cwd, "/book/prev-book", &closing_book->guid);

   /* add in transactions to equity accounts that will
    * hold the colsing balances */
   add_closing_balances (xaccGetAccountGroup(closing_book), 
                        existing_book, closing_book,
                        equity_account,
                        &calve_date, &ts, memo);

   period_commit_edit (existing_book, closing_book);

   LEAVE (" ");
   return closing_book;
}

/* ============================= END OF FILE ====================== */
