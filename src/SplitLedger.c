/* 
 * FILE:
 * SplitLedger.c 
 *
 * FUNCTION:
 * copy transaction data from engine into split-register object.
 *
 *
 * DESIGN NOTES:
 * Some notes about the "blank split":
 * Q: What is the "blank split"?
 * A: A new, empty split appended to the bottom of the ledger
 *    window.  The blank split provides an area where the user
 *    can type in new split/transaction info.  
 *    The "blank split" is treated in a special way for a number
 *    of reasons:
 *    (1) it must always appear as the bottom-most split
 *        in the Ledger window,
 *    (2) it must be committed if the user edits it, and 
 *        a new blank split must be created.
 *    (3) it must be deleted when the ledger window is closed.
 * To implement the above, the register "user_hook" is used
 * to store the blank split with the register window structures.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 */

/********************************************************************\
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include <assert.h>
#include <stdio.h>

#include "SplitLedger.h"
#include "messages.h"
#include "MultiLedger.h"
#include "splitreg.h"
#include "table-allgui.h"
#include "Transaction.h"

#define BUFSIZE 1024

/* ======================================================== */
/* this callback gets called when the user clicks on the gui
 * in such a way as to leave the current transaction, and to 
 * go to a new one.  So, save the current transaction.
 */

static void
LedgerMoveCursor  (Table *table, void * client_data)
{
   SplitRegister *reg = (SplitRegister *) client_data;
   xaccSRSaveRegEntry (reg);
}

/* ======================================================== */

static void
LedgerDestroy (SplitRegister *reg)
{
   /* be sure to destroy the "blank split" */
   if (reg->user_hook) {
      Split *split;

      split = (Split *) (reg->user_hook);

      /* split destroy will automatically remove it
       * from its parent account */
      xaccSplitDestroy (split);
      reg->user_hook = NULL;
   }
}

/* ======================================================== */

Split * 
xaccSRGetCurrentSplit (SplitRegister *reg)
{
   CellBlock *cursor;
   Split *split;

   /* get the handle to the current split and transaction */
   cursor = reg->table->current_cursor;
   assert (cursor);
   split = (Split *) (cursor->user_data);

   return split;
}

/* ======================================================== */
/* Copy from the register object to the engine */

void 
xaccSRSaveRegEntry (SplitRegister *reg)
{
   Split *split;
   Transaction *trans;
   Account * acc;
   unsigned int changed;
   int i;

   /* use the changed flag to avoid heavy-weight updates
    * of the split & transaction fields. This will help
    * cut down on uneccessary register redraws.  */
   changed = xaccSplitRegisterGetChangeFlag (reg);
   if (!changed) return;
   
   /* get the handle to the current split and transaction */
   split = xaccSRGetCurrentSplit (reg);
printf ("save split is %p \n", split);
   if (!split) {
      int vr, vc;
      Split *s;
      /* If we were asked to save data for a row for which there
       * is no associated split, then assume that this was a row
       * that was set aside for adding splits to an existing 
       * transaction.  The pre-existing transaction will be the 
       * one in the row(s) immediately above.  Therefore, get  
       * the cursor location; subtract one row, and get the 
       * associated transaction.  We will then create a new
       * split, copy the row contents to that split, and 
       * append the split to the pre-existing transaction. 
       */
      vr = reg->table->current_cursor_virt_row;
      vc = reg->table->current_cursor_virt_col;
      vr --;
      if ((0 > vr) || (0 > vc)) {
         printf ("Internal Error: SaveRegEntry(): bad row \n");
         return;
      }
      s = (Split *) reg->table->user_data[vr][vc];
      if (!s) {
         printf ("Internal Error: SaveRegEntry(): no parent \n");
         return;
      }
      trans = xaccSplitGetParent (s);
      acc = xaccSplitGetAccount (s);

      split = xaccMallocSplit ();
      xaccTransBeginEdit (trans);
      xaccTransAppendSplit (trans, split);
      xaccAccountInsertSplit (acc, split);
   } else {
      trans = xaccSplitGetParent (split);
      xaccTransBeginEdit (trans);
   }

   /* copy the contents from the cursor to the split */
   if (MOD_DATE & changed) 
      xaccTransSetDate (trans, reg->dateCell->date.tm_mday,
                               reg->dateCell->date.tm_mon+1,
                               reg->dateCell->date.tm_year+1900);

   if (MOD_NUM & changed) 
      xaccTransSetNum (trans, reg->numCell->value);
   
   if (MOD_DESC & changed) 
      xaccTransSetDescription (trans, reg->descCell->cell.value);

   if (MOD_RECN & changed) {
      xaccSplitSetReconcile (split, reg->recnCell->value[0]);
   }

   if (MOD_TAMNT & changed) {
      double new_amount;
      new_amount = (reg->creditTransCell->amount) - (reg->debitTransCell->amount);
      if ((EQUITY_REGISTER == (reg->type & REG_TYPE_MASK)) ||
          (STOCK_REGISTER  == (reg->type & REG_TYPE_MASK)) ||
          (PORTFOLIO       == (reg->type & REG_TYPE_MASK))) 
      { 
         xaccSplitSetShareAmount (split, new_amount);
      } else {
         xaccSplitSetValue (split, new_amount);
      }
   }

   if (MOD_TPRIC & changed) {
      xaccSplitSetSharePrice (split, reg->priceTransCell->amount);
   }

   if (MOD_TVALU & changed) {
      xaccSplitSetValue (split, -(reg->valueTransCell->amount));
   }

   /* -------------------------------------------------------------- */

   if (MOD_ACTN & changed) 
      xaccSplitSetAction (split, reg->actionCell->cell.value);

   /* OK, the handling of transfers gets complicated because it 
    * depends on what was displayed to the user.  For a multi-line
    * display, we just reparent the indicated split, its it,
    * and that's that.  For a two-line display, we want to reparent
    * the "other" split, but only if there is one ...
    */
   if ((MOD_XFRM | MOD_TXFRM) & changed) {
      Account *old_acc=NULL, *new_acc=NULL;
      Split *split_to_modify = NULL;

      if (reg->type & REG_MULTI_LINE) {
         split_to_modify = split;
      }
      if (reg->type & REG_DOUBLE_LINE) {
         split_to_modify = xaccGetOtherSplit(split);
      }

      /* split to modify may be null if its a mutli-split transaction,
       * and a single-line or two-line display.  Then do nothing */
      if (split_to_modify) {
         /* do some reparenting. Insertion into new account will automatically
          * delete from the old account */
         old_acc = xaccSplitGetAccount (split_to_modify);
         if (MOD_XFRM & changed) {
           new_acc = xaccGetAccountByName (trans, reg->xfrmCell->cell.value);
         } else  {
            new_acc = xaccGetAccountByName (trans, reg->xfrmTransCell->cell.value);
         }
         xaccAccountInsertSplit (new_acc, split_to_modify);
   
         /* make sure any open windows of the old account get redrawn */
         xaccAccountDisplayRefresh (old_acc);
      }
   }

   if (MOD_XTO & changed) {
      /* hack alert -- implement this */
   }

   if (MOD_MEMO & changed) 
      xaccSplitSetMemo (split, reg->memoCell->value);

   if (MOD_AMNT & changed) {
      double new_amount;
      new_amount = (reg->creditCell->amount) - (reg->debitCell->amount);
      new_amount = -new_amount;
      if ((EQUITY_REGISTER == (reg->type & REG_TYPE_MASK)) ||
          (STOCK_REGISTER  == (reg->type & REG_TYPE_MASK)) ||
          (PORTFOLIO       == (reg->type & REG_TYPE_MASK))) 
      { 
         xaccSplitSetShareAmount (split, new_amount);
      } else {
         xaccSplitSetValue (split, new_amount);
      }
   }

   if (MOD_PRIC & changed) {
      xaccSplitSetSharePrice (split, reg->priceCell->amount);
   }

   if (MOD_VALU & changed) {
      xaccSplitSetValue (split, reg->valueCell->amount);
   }


   xaccTransCommitEdit (trans);

printf ("finished saving split %s of trans %s \n", 
xaccSplitGetMemo(split),
xaccTransGetDescription(trans));

   /* if the modified split is the "blank split", 
    * then it is now an official part of the account.
    * Set user_hook to null, so that we can be sure of 
    * getting a new split.
    */
   split = xaccTransGetSplit (trans, 0);
   if (split == ((Split *) (reg->user_hook))) {
      reg->user_hook = NULL;
   }

   /* refresh the register windows */
   /* This split belongs to a transaction that might be displayed
    * in any number of windows.  Changing any one split is likely
    * to affect any account windows associated with the other splits
    * in this transaction.  So basically, send redraw events to all
    * of the splits.
    */
   i = 0;
   split = xaccTransGetSplit (trans, i);
   while (split) {
      acc = xaccSplitGetAccount (split);
      xaccAccountDisplayRefresh (acc);
      i++;
      split = xaccTransGetSplit (trans, i);
   }
}

/* ======================================================== */

#define LOAD_XFRM(cellname) {							\
   char * accname=NULL;								\
										\
   /* Show the transfer-from account name.                               */	\
   /* What gets displayed depends on the display format.                 */	\
   /* For a multi-line display, show the account for each member split.  */	\
   /* For a one or two-line display, show the other account, but only    */	\
   /* if there are exactly two splits.                                   */	\
   if (reg->type & REG_MULTI_LINE) {						\
      accname = xaccAccountGetName (xaccSplitGetAccount (split));		\
      xaccSetComboCellValue (reg->cellname, accname);				\
   } else {									\
      Split *s = xaccGetOtherSplit (split);					\
      if (s) {									\
         accname = xaccAccountGetName (xaccSplitGetAccount (s));		\
      } else {									\
         accname = SPLIT_STR;							\
      }										\
      xaccSetComboCellValue (reg->cellname, accname);				\
   } 										\
}
   
/* ======================================================== */

static void
xaccSRLoadTransEntry (SplitRegister *reg, Split *split, int do_commit)
{
   char buff[2];
   time_t secs;
   double baln;

   if (!split) {
      /* we interpret a NULL split as a blank split */
      xaccSetDateCellValueSecs (reg->dateCell, 0);
      xaccSetBasicCellValue (reg->numCell, "");
      xaccSetComboCellValue (reg->xfrmTransCell, "");
      xaccSetQuickFillCellValue (reg->descCell, "");
      xaccSetBasicCellValue (reg->recnCell, "");
      xaccSetDebCredCellValue (reg->debitTransCell, 
                               reg->creditTransCell, 0.0);
      xaccSetPriceCellValue (reg->priceTransCell, 0.0);
      xaccSetPriceCellValue (reg->valueTransCell, 0.0);
      xaccSetPriceCellValue  (reg->shrsCell,  0.0);
      xaccSetPriceCellValue (reg->balanceCell, 0.0);

   } else {
      double amt;
      Transaction *trans = xaccSplitGetParent (split);
   
      secs = xaccTransGetDate (trans);
      xaccSetDateCellValueSecs (reg->dateCell, secs);
   
      xaccSetBasicCellValue (reg->numCell, xaccTransGetNum (trans));
      LOAD_XFRM (xfrmTransCell);
      xaccSetQuickFillCellValue (reg->descCell, xaccTransGetDescription (trans));
   
      buff[0] = xaccSplitGetReconcile (split);
      buff[1] = 0x0;
      xaccSetBasicCellValue (reg->recnCell, buff);
   
      if ((EQUITY_REGISTER == (reg->type & REG_TYPE_MASK)) ||
          (STOCK_REGISTER  == (reg->type & REG_TYPE_MASK)) ||
          (PORTFOLIO       == (reg->type & REG_TYPE_MASK))) 
      { 
         amt = xaccSplitGetShareAmount (split);
      } else {
         amt = xaccSplitGetValue (split);
      }
      xaccSetDebCredCellValue (reg->debitTransCell, reg->creditTransCell, amt);
      xaccSetPriceCellValue (reg->priceTransCell, xaccSplitGetSharePrice (split));
      xaccSetPriceCellValue (reg->valueTransCell, xaccSplitGetValue (split));
   
      /* For income and expense acounts, we have to reverse
       * the meaning of balance, since, in a dual entry
       * system, income will show up as a credit to a
       * bank account, and a debit to the income account.
       * Thus, positive and negative are interchanged */
      baln = xaccSplitGetBalance (split);
      if ((INCOME_REGISTER == (reg->type & REG_TYPE_MASK)) ||
          (EXPENSE_REGISTER == (reg->type & REG_TYPE_MASK))) { 
         baln = -baln;
      }
      xaccSetPriceCellValue (reg->balanceCell, baln);
   
      xaccSetPriceCellValue (reg->shrsCell,  xaccSplitGetShareBalance (split));
   }

   reg->table->current_cursor->user_data = (void *) split;

   /* copy cursor contents into the table */
   if (do_commit) {
      xaccCommitCursor (reg->table);
   }
}

/* ======================================================== */

static void
xaccSRLoadSplitEntry (SplitRegister *reg, Split *split, int do_commit)
{
   char buff[2];

   if (!split) {
      /* we interpret a NULL split as a blank split */
      xaccSetComboCellValue (reg->actionCell, "");
      xaccSetBasicCellValue (reg->memoCell, "");
      xaccSetComboCellValue (reg->xfrmCell, "");
      xaccSetDebCredCellValue (reg->debitCell, 
                               reg->creditCell, 0.0);
      xaccSetPriceCellValue (reg->priceCell, 0.0);
      xaccSetPriceCellValue (reg->valueCell, 0.0);

   } else {
      double amt;
   
      xaccSetComboCellValue (reg->actionCell, xaccSplitGetAction (split));
      LOAD_XFRM (xfrmCell);
      xaccSetBasicCellValue (reg->memoCell, xaccSplitGetMemo (split));
   
      buff[0] = xaccSplitGetReconcile (split);
      buff[1] = 0x0;
      xaccSetBasicCellValue (reg->recsCell, buff);
   
      if ((EQUITY_REGISTER == (reg->type & REG_TYPE_MASK)) ||
          (STOCK_REGISTER  == (reg->type & REG_TYPE_MASK)) ||
          (PORTFOLIO       == (reg->type & REG_TYPE_MASK))) 
      { 
         amt = xaccSplitGetShareAmount (split);
      } else {
         amt = xaccSplitGetValue (split);
      }
      xaccSetDebCredCellValue (reg->debitCell, reg->creditCell, -amt);
      xaccSetPriceCellValue (reg->priceCell, xaccSplitGetSharePrice (split));
      xaccSetPriceCellValue (reg->valueCell, -xaccSplitGetValue (split));
   }

   reg->table->current_cursor->user_data = (void *) split;

   /* copy cursor contents into the table */
   if (do_commit) {
      xaccCommitCursor (reg->table);
   }
}

/* ======================================================== */

void
xaccSRLoadRegEntry (SplitRegister *reg, Split *split)
{
   xaccSRLoadTransEntry (reg, split, 0);
   xaccSRLoadSplitEntry (reg, split, 0);

   /* copy cursor contents into the table */
   xaccCommitCursor (reg->table);
}

/* ======================================================== */

void
xaccSRLoadRegister (SplitRegister *reg, Split **slist, 
                      Account *default_source_acc)
{
   int i;
   Split *split;
   Table *table;
   int save_cursor_phys_row;
   int num_phys_rows;
   int num_phys_cols;
   int num_virt_rows;
   int phys_row;
   int vrow;
   int double_line, multi_line;

   table = reg->table;
   double_line = (reg->type) & REG_DOUBLE_LINE;
   multi_line  = (reg->type) & REG_MULTI_LINE;

   /* disable move callback -- we con't want the cascade of 
    * callbacks while we are fiddling with loading the register */
   table->move_cursor = NULL;

   /* save the current cursor location; we want to restore 
    * it after the reload.  */
   save_cursor_phys_row = table->current_cursor_phys_row;
   xaccMoveCursorGUI (table, -1, -1);

   /* set table size to number of items in list */
   /* compute the corresponding number of physical & virtual rows. */
   /* num_phys_cols is easy ... just the total number of columns 
    * in the header */
   num_phys_cols = reg->header->numCols;

   /* num_phys_rows is the number of rows in all the cursors */
   num_phys_rows = reg->header->numRows;

   /* Count the number of rows needed.  
    * the count will be equal to 
    * +1   for the header
    * +n   that is, one (transaction) row for each split passed in,
    * +n   one blank edit row for each transaction
    * +p   where p is the sum total of all the splits in the transaction
    * +2   an editable transaction and split at the end.
    */
   num_virt_rows = 1;

   i=0;
   split = slist[0]; 
   while (split) {
      Transaction *trans;
      int j;

      trans = xaccSplitGetParent (split);
      if (!trans) {
         /* hack assert */
         printf ("Internal Error: xaccSRLoadRegister(): "
                 "Split without a parent \n");
         break;
      } 

      /* add one row for a transaction */
      num_virt_rows ++;
      num_phys_rows += reg->trans_cursor->numRows; 
      
      if (double_line) {
         /* add one row */
         num_virt_rows ++;
         num_phys_rows += reg->split_cursor->numRows; 
      }
      if (multi_line) {
         /* add a row for each split, minus one, plus one */
         j = xaccTransCountSplits (trans);
         num_virt_rows += j;
         num_phys_rows += j * reg->split_cursor->numRows; 
      }

      i++;
      split = slist[i];
   }

   /* If user_hook is null, then we haven't set up the blank split yet,
    * so add two lines for it: one blank transaction, one blank split.  
    * But if we have set it up yet, then we've counted one split too 
    * many: the blank-blank at the very end.  Subtract it back out.
    */
   if (multi_line) {
      if (!(reg->user_hook)) {
         i++;
         num_virt_rows += 2; 
         num_phys_rows += reg->trans_cursor->numRows;
         num_phys_rows += reg->split_cursor->numRows;
      } else {
         num_virt_rows -= 1; 
         num_phys_rows -= reg->split_cursor->numRows;
      }
   }

   if (double_line) {
      /* add two rows */
      if (!(reg->user_hook)) {
         i++;
         num_virt_rows ++;
         num_phys_rows += reg->trans_cursor->numRows; 
         num_virt_rows ++;
         num_phys_rows += reg->split_cursor->numRows; 
      }
   }

   if (!multi_line && !double_line) {
      if (!(reg->user_hook)) {
         i++;
         num_virt_rows += 1; 
         num_phys_rows += reg->trans_cursor->numRows;
      }
   }

   /* num_virt_cols is always one. */
   xaccSetTableSize (table, num_phys_rows, num_phys_cols, num_virt_rows, 1);

   /* make sure that the header is loaded */
   xaccSetCursor (table, reg->header, 0, 0, 0, 0);

printf ("load register of %d virtual entries %d phys rows ----------- \n", i, num_phys_rows);
   /* populate the table */
   i=0;
   vrow = 1;   /* header is vrow zero */
   phys_row = reg->header->numRows;
   split = slist[0]; 
   while (split) {

      /* do not load the blank split */
      if (split != ((Split *) reg->user_hook)) {
         Transaction *trans;
         Split * secondary;
         int j = 0;

printf ("load trans %d at phys row %d \n", i, phys_row);
         xaccSetCursor (table, reg->trans_cursor, phys_row, 0, vrow, 0);
         xaccMoveCursor (table, phys_row, 0);
         xaccSRLoadTransEntry (reg, split, 1);
         vrow ++;
         phys_row += reg->trans_cursor->numRows; 
   
         if (double_line) {
            xaccSetCursor (table, reg->split_cursor, phys_row, 0, vrow, 0);
            xaccMoveCursor (table, phys_row, 0);
            xaccSRLoadSplitEntry (reg, split, 1);
            vrow ++;
            phys_row += reg->split_cursor->numRows; 
         }

         if (multi_line) {
            /* loop over all of the splits in the transaction */
            /* the do..while will automaticaly put a blank (null) split at the end */
            trans = xaccSplitGetParent (split);
            j = 0;
            do {
               secondary = xaccTransGetSplit (trans, j);
               if (secondary != split) {
printf ("load split %d at phys row %d \n", j, phys_row);
                  xaccSetCursor (table, reg->split_cursor, phys_row, 0, vrow, 0);
                  xaccMoveCursor (table, phys_row, 0);
                  xaccSRLoadSplitEntry (reg, secondary, 1);
                  vrow ++;
                  phys_row += reg->split_cursor->numRows; 
               }
               j++;
            } while (secondary);
         } 
      }

      i++; 
      split = slist[i];
   }

   /* add the "blank split" at the end.  We use either the blank
    * split we've cached away previously in "user_hook", or we create
    * a new one, as needed. */
   if (reg->user_hook) {
      split = (Split *) reg->user_hook;
   } else {
      Transaction *trans;
      trans = xaccMallocTransaction ();
      xaccTransBeginEdit (trans);
      xaccTransSetDateToday (trans);
      xaccTransCommitEdit (trans);
      split = xaccTransGetSplit (trans, 0);
      xaccAccountInsertSplit (default_source_acc, split);
      reg->user_hook =  (void *) split;
      reg->destroy = LedgerDestroy;
   }

   /* do the transaction row of the blank split */
   xaccSetCursor (table, reg->trans_cursor, phys_row, 0, vrow, 0);
   xaccMoveCursor (table, phys_row, 0);
   xaccSRLoadRegEntry (reg, split);
   vrow ++;
   phys_row += reg->trans_cursor->numRows; 
   
   /* do the split row of the blank split */
   if (double_line || multi_line) {
      Transaction *trans;
      trans = xaccSplitGetParent (split);
      split = xaccTransGetSplit (trans, 1);
      xaccSetCursor (table, reg->split_cursor, phys_row, 0, vrow, 0);
      xaccMoveCursor (table, phys_row, 0);
      xaccSRLoadRegEntry (reg, split);
      vrow ++;
      phys_row += reg->split_cursor->numRows; 
   }
   
   /* restore the cursor to its original location */
   if (phys_row <= save_cursor_phys_row) {
       save_cursor_phys_row = phys_row - reg->split_cursor->numRows;
   }
   if (save_cursor_phys_row < (reg->header->numRows)) {
      save_cursor_phys_row = reg->header->numRows;
   }
   xaccMoveCursorGUI (table, save_cursor_phys_row, 0);
   xaccRefreshTableGUI (table);

   /* enable callback for cursor user-driven moves */
   table->move_cursor = LedgerMoveCursor;
   table->client_data = (void *) reg;
}

/* ======================================================== */
/* walk account tree recursively, pulling out all the names */

static void 
LoadXferCell (ComboCell *cell,  AccountGroup *grp)
{
   Account * acc;
   int n;

   if (!grp) return;

   /* build the xfer menu out of account names */
   /* traverse sub-accounts recursively */
   n = 0;
   acc = xaccGroupGetAccount (grp, n);
   while (acc) {
      xaccAddComboCellMenuItem (cell, xaccAccountGetName (acc));
      LoadXferCell (cell, xaccAccountGetChildren (acc));
      n++;
      acc = xaccGroupGetAccount (grp, n);
   }
}

/* ======================================================== */

void xaccLoadXferCell (ComboCell *cell,  AccountGroup *grp)
{
   xaccAddComboCellMenuItem (cell, "");
   LoadXferCell (cell, grp);
}

/* =======================  end of file =================== */
