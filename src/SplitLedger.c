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
 *
 * This callback is centrally involved in the redraw sequence.
 * When the user moves from one cell to another, the following 
 * sequence of events get triggered and cascade down:
 *    enterCB () {
 *      VerifyCursorPosition() {
 *        MoveCursor() {  
 *         callback for move() which is this function (LedgerMoveCursor) {
 *           SaveRegEntry() {...}
 *           RedrawRegEntry() {
 *              SRLoadRegister() {
 *                SRLoadRegEntry() {
 *                   MoveCursor () { }
 *                }
 *             }
 *          } }}}}
 */

static void
LedgerMoveCursor  (Table *table, 
                   int *p_new_phys_row, 
                   int *p_new_phys_col, 
                   void * client_data)
{
   int new_phys_row = *p_new_phys_row;
   int new_phys_col = *p_new_phys_col;
   SplitRegister *reg = (SplitRegister *) client_data;
   int style;

printf ("LedgerMoveCursor start calback %d %d \n",
new_phys_row, new_phys_col);
   /* commit the contents of the cursor into the database */
   xaccSRSaveRegEntry (reg);
   xaccSRRedrawRegEntry (reg); 
printf ("LedgerMoveCursor after redraw %d %d \n",
new_phys_row, new_phys_col);

   /* if auto-expansion is enabled, we need to redraw the register
    * to expand out the splits at the new location.  We do some
    * tomfoolery here to trick the code into expanding the new location.
    * This little futz is sleazy, but it does suceed in getting the 
    * LoadRegister code into expanding the appropriate split.
    * 
    */   
   style = ((reg->type) & REG_STYLE_MASK);
   if ((REG_SINGLE_DYNAMIC == style) ||
       (REG_DOUBLE_DYNAMIC == style)) 
   {
      Split *split, *oldsplit;
      oldsplit = xaccSRGetCurrentSplit (reg);
      split = xaccGetUserData (reg->table, new_phys_row, new_phys_col);
      reg->table->current_cursor->user_data = (void *) split;

      /* if a null split, provide a hint for where the cursor should go */
      if (NULL == split) {
         reg->cursor_phys_row = new_phys_row;
         // reg->cursor_virt_row = reg->table->current_cursor_virt_row;
         reg->user_hack = (void *) xaccSplitGetParent (oldsplit);
      }
      xaccRegisterRefresh (reg);

      /* indicate what row we *should* have gone to */
      *p_new_phys_row = table->current_cursor_phys_row;
printf ("LedgerMoveCursor after dynamic %d %d stored val %d\n",
*p_new_phys_row, new_phys_col,
reg->cursor_phys_row
);
   }
}

/* ======================================================== */
/* this callback gets called when the user clicks on the gui
 * in such a way as to leave the current transaction, and to 
 * go to a new one.  It is called to verify what the cordinates
 * of the new cell will be.  It really applies only for auto-expansion,
 * where we need to calculate the coords of the target cell.
 */

static void
LedgerTraverse  (Table *table, 
                   int *p_new_phys_row, 
                   int *p_new_phys_col, 
                   void * client_data)
{
   int new_phys_row = *p_new_phys_row;
   int new_phys_col = *p_new_phys_col;
   SplitRegister *reg = (SplitRegister *) client_data;
   int style;

   /* if auto-expansion is enabled, we need to redraw the register
    * to expand out the splits at the new location.  We do some
    * tomfoolery here to trick the code into expanding the new location.
    * This little futz is sleazy, but it does suceed in getting the 
    * LoadRegister code into expanding the appropriate split.
    */   
   style = ((reg->type) & REG_STYLE_MASK);
   if ((REG_SINGLE_DYNAMIC == style) ||
       (REG_DOUBLE_DYNAMIC == style)) 
   {
      Split *split, *oldsplit;
printf ("enter LedgerTraverse with %d %d \n", new_phys_row , new_phys_col);
      oldsplit = xaccSRGetCurrentSplit (reg);
      split = xaccGetUserData (reg->table, new_phys_row, new_phys_col);
      reg->table->current_cursor->user_data = (void *) split;

      /* if a null split, provide a hint for where the cursor should go */
      if (NULL == split) {
         reg->cursor_phys_row = new_phys_row;
         // reg->cursor_virt_row = reg->table->current_cursor_virt_row;
         reg->user_hack = (void *) xaccSplitGetParent (oldsplit);
      }

      xaccRegisterCountHack (reg);
      reg->table->current_cursor->user_data = (void *) oldsplit;

printf ("leave LedgerTraverse with %d \n", reg->cursor_phys_row);
      /* indicate what row we *should* go to */
      *p_new_phys_row = reg->cursor_phys_row;
   }
}

/* ======================================================== */

static void
LedgerDestroy (SplitRegister *reg)
{
   /* be sure to destroy the "blank split" */
   if (reg->user_hook) {
      Split *split;
      Transaction *trans;

      split = (Split *) (reg->user_hook);

      /* split destroy will automatically remove it
       * from its parent account */
      trans = xaccSplitGetParent (split);
      xaccTransBeginEdit (trans);
      xaccTransDestroy (trans);
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
   if (!cursor) return NULL;
   split = (Split *) (cursor->user_data);

   return split;
}

/* ======================================================== */

void 
xaccSRRedrawRegEntry (SplitRegister *reg) 
{
   Split *split;
   Transaction *trans;
   Account * acc;
   unsigned int changed;
   int i;

   /* use the changed flag to avoid heavy-weight redraws
    * This will help cut down on uneccessary register redraws.  */
   changed = xaccSplitRegisterGetChangeFlag (reg);
   if (!changed) return;

   split = xaccSRGetCurrentSplit (reg);
   trans = xaccSplitGetParent (split);

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
/* Copy from the register object to the engine */

void 
xaccSRSaveRegEntry (SplitRegister *reg)
{
   Split *split;
   Transaction *trans;
   Account * acc;
   unsigned int changed;
   int style;

   /* use the changed flag to avoid heavy-weight updates
    * of the split & transaction fields. This will help
    * cut down on uneccessary register redraws.  */
   changed = xaccSplitRegisterGetChangeFlag (reg);
   if (!changed) return;

   style = (reg->type) & REG_STYLE_MASK;   

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

      assert (reg->table->current_cursor);
      reg->table->current_cursor->user_data = (void *) split;

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

   if (MOD_ACTN & changed) 
      xaccSplitSetAction (split, reg->actionCell->cell.value);

   /* -------------------------------------------------------------- */
   /* OK, the handling of transfers gets complicated because it 
    * depends on what was displayed to the user.  For a multi-line
    * display, we just reparent the indicated split, its it,
    * and that's that.  For a two-line display, we want to reparent
    * the "other" split, but only if there is one ...
    * XFRM is the straight split, MXFRM is the mirrored split.
    */
   if (MOD_XFRM & changed) {
      Split *split_to_modify = NULL;

      split_to_modify = split;

      /* split to modify may be null if its a mutli-split transaction,
       * and a single-line or two-line display.  Then do nothing */
      if (split_to_modify) {
         Account *old_acc=NULL, *new_acc=NULL;

         /* do some reparenting. Insertion into new account will automatically
          * delete from the old account */
         old_acc = xaccSplitGetAccount (split_to_modify);
         new_acc = xaccGetAccountByName (trans, reg->xfrmCell->cell.value);
         xaccAccountInsertSplit (new_acc, split_to_modify);
   
         /* make sure any open windows of the old account get redrawn */
         xaccAccountDisplayRefresh (old_acc);
      }
   }

   if (MOD_MXFRM & changed) {
      Split *split_to_modify = NULL;

      split_to_modify = xaccGetOtherSplit(split);

      /* split to modify may be null if its a mutli-split transaction,
       * and a single-line or two-line display.  Then do nothing */
      if (split_to_modify) {
         Account *old_acc=NULL, *new_acc=NULL;

         /* do some reparenting. Insertion into new account will automatically
          * delete from the old account */
         old_acc = xaccSplitGetAccount (split_to_modify);
         new_acc = xaccGetAccountByName (trans, reg->mxfrmCell->cell.value);
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

   /* The AMNT and NAMNT updates only differ by sign.  Basically, 
    * the split and transaction cursors show minus the quants that
    * the single and double cursors show, and so when updates happen,
    * the extra minus sign must also be handled.
    */
   if ((MOD_AMNT | MOD_NAMNT) & changed) {
      double new_amount;
      if (MOD_AMNT & changed) {
         new_amount = (reg->creditCell->amount) - (reg->debitCell->amount);
      } else {
         new_amount = (reg->ndebitCell->amount) - (reg->ncreditCell->amount);
      }
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
      xaccSplitSetValue (split, (reg->valueCell->amount));
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
}

/* ======================================================== */

static void
xaccSRLoadTransEntry (SplitRegister *reg, Split *split, int do_commit)
{
   char buff[2];
   time_t secs;
   double baln;
   int typo = reg->type & REG_TYPE_MASK;
   /* int style = reg->type & REG_STYLE_MASK; */

   /* don't even bother doing a load if there is no current cursor */
   if (!(reg->table->current_cursor)) return;

   if (!split) {
      /* we interpret a NULL split as a blank split */
      xaccSetDateCellValueSecs (reg->dateCell, 0);
      xaccSetBasicCellValue (reg->numCell, "");
      xaccSetQuickFillCellValue (reg->descCell, "");
      xaccSetBasicCellValue (reg->recnCell, "");
      xaccSetPriceCellValue  (reg->shrsCell,  0.0);
      xaccSetPriceCellValue (reg->balanceCell, 0.0);

      xaccSetComboCellValue (reg->actionCell, "");
      xaccSetBasicCellValue (reg->memoCell, "");
      xaccSetComboCellValue (reg->xfrmCell, "");
      xaccSetComboCellValue (reg->mxfrmCell, "");
      xaccSetDebCredCellValue (reg->debitCell, 
                               reg->creditCell, 0.0);
      xaccSetDebCredCellValue (reg->ndebitCell, 
                               reg->ncreditCell, 0.0);
      xaccSetPriceCellValue (reg->priceCell, 0.0);
      xaccSetPriceCellValue (reg->valueCell, 0.0);

   } else {
      double amt;
      char * accname=NULL;
      Transaction *trans = xaccSplitGetParent (split);
   
      secs = xaccTransGetDate (trans);
      xaccSetDateCellValueSecs (reg->dateCell, secs);
   
      xaccSetBasicCellValue (reg->numCell, xaccTransGetNum (trans));
      xaccSetQuickFillCellValue (reg->descCell, xaccTransGetDescription (trans));
   
      buff[0] = xaccSplitGetReconcile (split);
      buff[1] = 0x0;
      xaccSetBasicCellValue (reg->recnCell, buff);
   
      /* For income and expense acounts, we have to reverse
       * the meaning of balance, since, in a dual entry
       * system, income will show up as a credit to a
       * bank account, and a debit to the income account.
       * Thus, positive and negative are interchanged */
      baln = xaccSplitGetBalance (split);
      if ((INCOME_REGISTER == typo) ||
          (EXPENSE_REGISTER == typo)) { 
         baln = -baln;
      }
      xaccSetPriceCellValue (reg->balanceCell, baln);
   
      xaccSetPriceCellValue (reg->shrsCell,  xaccSplitGetShareBalance (split));

      xaccSetComboCellValue (reg->actionCell, xaccSplitGetAction (split));

      /* Show the transfer-from account name.                            
       * What gets displayed depends on the display format.                
       * For a multi-line display, show the account for each member split.  
       * For a one or two-line display, show the other account, but only    
       * if there are exactly two splits.                                   
       * xfrm is the "straight" display, "mxfrm" is the "mirrored" display.
       */
      accname = xaccAccountGetName (xaccSplitGetAccount (split));
      xaccSetComboCellValue (reg->xfrmCell, accname);
      {
         Split *s = xaccGetOtherSplit (split);
         if (s) {
            accname = xaccAccountGetName (xaccSplitGetAccount (s));
         } else {
            /* determine whether s is null because threre are three
             * or more splits, or whether there is only one ... */
            s = xaccTransGetSplit (xaccSplitGetParent(split), 1);
            if (s) {
               accname = SPLIT_STR;    /* three or more .. */
            } else {
               accname  = "";          /* none ... */
            }
         }
         xaccSetComboCellValue (reg->mxfrmCell, accname);
      }
   
      xaccSetBasicCellValue (reg->memoCell, xaccSplitGetMemo (split));
   
      buff[0] = xaccSplitGetReconcile (split);
      buff[1] = 0x0;
      xaccSetBasicCellValue (reg->recnCell, buff);
   
      if ((EQUITY_REGISTER == typo) ||
          (STOCK_REGISTER  == typo) ||
          (PORTFOLIO       == typo)) 
      { 
         amt = xaccSplitGetShareAmount (split);
      } else {
         amt = xaccSplitGetValue (split);
      }
      xaccSetDebCredCellValue (reg->debitCell, reg->creditCell, amt);
      xaccSetDebCredCellValue (reg->ndebitCell, reg->ncreditCell, -amt);
      xaccSetPriceCellValue (reg->priceCell, xaccSplitGetSharePrice (split));
      xaccSetPriceCellValue (reg->valueCell, xaccSplitGetValue (split));
   }

   reg->table->current_cursor->user_data = (void *) split;

   /* copy cursor contents into the table */
   if (do_commit) {
      xaccCommitCursor (reg->table);
   }
}

/* ======================================================== */

#define xaccSRLoadSplitEntry  xaccSRLoadTransEntry

#ifdef LATER
static void
xaccSRLoadSplitEntry (SplitRegister *reg, Split *split, int do_commit)
{
   char buff[2];

   if (!split) {
   } else {
   }

   reg->table->current_cursor->user_data = (void *) split;

   /* copy cursor contents into the table */
   if (do_commit) {
      xaccCommitCursor (reg->table);
   }
}
#endif 

/* ======================================================== */

void
xaccSRLoadRegEntry (SplitRegister *reg, Split *split)
{
   xaccSRLoadTransEntry (reg, split, 0);
   /* xaccSRLoadSplitEntry (reg, split, 0); */

   /* copy cursor contents into the table */
   xaccCommitCursor (reg->table);
}

/* ======================================================== */

void
xaccSRCountRows (SplitRegister *reg, Split **slist, 
                      Account *default_source_acc)
{
   int i;
   Split *split=NULL;
   Split *save_current_split=NULL;
   int save_cursor_phys_row = -1;
   int save_cursor_virt_row = -1;
   Table *table;
   int num_phys_rows;
   int num_virt_rows;
   int style;
   int multi_line, dynamic;
   CellBlock *lead_cursor;

   table = reg->table;
   style = (reg->type) & REG_STYLE_MASK;
   multi_line  = (REG_MULTI_LINE == style);
   dynamic = ((REG_SINGLE_DYNAMIC == style) || (REG_DOUBLE_DYNAMIC == style));
   if ((REG_SINGLE_LINE == style) ||
       (REG_SINGLE_DYNAMIC == style)) {
      lead_cursor = reg->single_cursor;
   } else {
      lead_cursor = reg->double_cursor;
   }

   /* save the current cursor location; we do this by saving
    * a pointer to the currently edited split; we restore the 
    * cursor to this location when we are done. */
   save_current_split = xaccSRGetCurrentSplit (reg);
   if (NULL == save_current_split) {
      save_cursor_phys_row = reg->cursor_phys_row;
      save_cursor_virt_row = reg->cursor_virt_row;
   }

   /* num_phys_rows is the number of rows in all the cursors.
    * num_virt_rows is the number of cursors (including the header).
    * Count the number of rows needed.  
    * the phys row count will be equal to 
    * +1   for the header
    * +n   that is, one (transaction) row for each split passed in,
    * +n   one blank edit row for each transaction
    * +p   where p is the sum total of all the splits in the transaction
    * +2   an editable transaction and split at the end.
    */
   num_phys_rows = reg->header->numRows;
   num_virt_rows = 1;

   i=0;
   if (slist) {
      split = slist[0]; 
   } else {
      split = NULL;
   }
   while (split) {
      /* do not count the blank split */
      if (split != ((Split *) reg->user_hook)) {
         Transaction *trans;
         int do_expand = 0;

         /* lets determine where to locate the cursor ... */
         if (split == save_current_split) {
            save_cursor_phys_row = num_phys_rows;
            save_cursor_virt_row = num_virt_rows;
         }

         /* if multi-line, then show all splits.  If dynamic then
          * show all splits only if this is the hot split. 
          */
         do_expand = multi_line;
         do_expand = do_expand || 
                     (dynamic && xaccIsPeerSplit(split,save_current_split)); 
         if (NULL == save_current_split) {
            trans = xaccSplitGetParent (split);
            do_expand = do_expand || (trans == reg->user_hack);
         }

         if (do_expand) 
         {
            Split * secondary;
            int j = 0;

            /* add one row for a transaction */
            num_virt_rows ++;
            num_phys_rows += reg->trans_cursor->numRows; 

            /* Add a row for each split, minus one, plus one.
             * Essentially, do the following:
             * j = xaccTransCountSplits (trans);
             * num_virt_rows += j;
             * num_phys_rows += j * reg->split_cursor->numRows; 
             * except that we also have to find teh saved cursor row,
             * Thus, we need a real looop over the splits.
             * The do..while will automaticaly put a blank (null) 
             * split at the end 
             */
            trans = xaccSplitGetParent (split);
            j = 0;
            do {
               secondary = xaccTransGetSplit (trans, j);
               if (secondary != split) {

                  /* lets determine where to locate the cursor ... */
                  if (secondary == save_current_split) {
                     save_cursor_phys_row = num_phys_rows;
                     save_cursor_virt_row = num_virt_rows;
                  }
                  num_virt_rows ++;
                  num_phys_rows += reg->split_cursor->numRows; 
               }
               j++;
            } while (secondary);
         } else {

            /* the simple case ... add one row for a transaction */
            num_virt_rows ++;
            num_phys_rows += lead_cursor->numRows; 
         }
      }
      i++;
      split = slist[i];
   }

   /* ---------------------------------------------------------- */
   /* the "blank split", if it exists, is at the end */
   if (reg->user_hook) {
      split = (Split *) reg->user_hook;

      /* lets determine where to locate the cursor ... */
      if (split == save_current_split) {
         save_cursor_phys_row = num_phys_rows;
         save_cursor_virt_row = num_virt_rows;
      }
   }

   if (multi_line) {
      num_virt_rows += 2; 
      num_phys_rows += reg->trans_cursor->numRows;
      num_phys_rows += reg->split_cursor->numRows;
   } else {
      num_virt_rows += 1; 
      num_phys_rows += lead_cursor->numRows;
   }

   /* check to make sure we got a good cursor position */
   if ((num_phys_rows <= save_cursor_phys_row) ||
       (num_virt_rows <= save_cursor_virt_row)) 
   {
       save_cursor_phys_row = num_phys_rows - reg->split_cursor->numRows;
       save_cursor_virt_row = num_virt_rows - 1;
   }
   if ((save_cursor_phys_row < (reg->header->numRows)) ||
       (save_cursor_virt_row < 1))
   {
      save_cursor_phys_row = reg->header->numRows;
      save_cursor_virt_row = 1;
   }

   /* finally, record the values */
   reg->num_phys_rows = num_phys_rows;
   reg->num_virt_rows = num_virt_rows;
   reg->cursor_phys_row = save_cursor_phys_row;
   reg->cursor_virt_row = save_cursor_virt_row;
}

/* ======================================================== */

void
xaccSRLoadRegister (SplitRegister *reg, Split **slist, 
                      Account *default_source_acc)
{
   int i = 0;
   Split *split=NULL, *last_split=NULL;
   Split *save_current_split=NULL;
   Table *table;
   int phys_row;
   int vrow;
   int style;
   int multi_line, dynamic;
   CellBlock *lead_cursor;

   table = reg->table;
   style = (reg->type) & REG_STYLE_MASK;
   multi_line  = (REG_MULTI_LINE == style);
   dynamic = ((REG_SINGLE_DYNAMIC == style) || (REG_DOUBLE_DYNAMIC == style));
   if ((REG_SINGLE_LINE == style) ||
       (REG_SINGLE_DYNAMIC == style)) {
      lead_cursor = reg->single_cursor;
   } else {
      lead_cursor = reg->double_cursor;
   }

   /* count the number of rows */
   xaccSRCountRows (reg, slist, default_source_acc);

   /* save the current cursor location; we do this by saving
    * a pointer to the currently edited split; we restore the 
    * cursor to this location when we are done. */
   save_current_split = xaccSRGetCurrentSplit (reg);

   /* disable move callback -- we con't want the cascade of 
    * callbacks while we are fiddling with loading the register */
   table->move_cursor = NULL;
   xaccMoveCursorGUI (table, -1, -1);

   /* resize the table to the sizes we just counted above */
   /* num_virt_cols is always one. */
   xaccSetTableSize (table, reg->num_phys_rows, reg->num_cols, 
                            reg->num_virt_rows, 1);

   /* make sure that the header is loaded */
   xaccSetCursor (table, reg->header, 0, 0, 0, 0);

printf ("load register of %d phys rows ----------- \n", reg->num_phys_rows);

   /* populate the table */
   i=0;
   vrow = 1;   /* header is vrow zero */
   phys_row = reg->header->numRows;
   if (slist) {
      split = slist[0]; 
   } else {
      split = NULL;
   }
   while (split) {

      /* do not load the blank split */
      if (split != ((Split *) reg->user_hook)) {
         Transaction *trans;
         int do_expand;

printf ("load trans %d at phys row %d \n", i, phys_row);
   
         /* if multi-line, then show all splits.  If dynamic then
          * show all splits only if this is the hot split. 
          */
         do_expand = multi_line;
         do_expand = do_expand || 
                     (dynamic && xaccIsPeerSplit(split,save_current_split)); 
         if (NULL == save_current_split) {
            trans = xaccSplitGetParent (split);
            do_expand = do_expand || (trans == reg->user_hack);
         }

         if (do_expand) 
         {
            Split * secondary;
            int j = 0;

            xaccSetCursor (table, reg->trans_cursor, phys_row, 0, vrow, 0);
            xaccMoveCursor (table, phys_row, 0);
            xaccSRLoadTransEntry (reg, split, 1);
            vrow ++;
            phys_row += reg->trans_cursor->numRows; 

            /* loop over all of the splits in the transaction */
            /* the do..while will automaticaly put a blank (null) split at the end */
            trans = xaccSplitGetParent (split);
            j = 0;
            do {
               secondary = xaccTransGetSplit (trans, j);

               if (secondary != split) {
                  xaccSetCursor (table, reg->split_cursor, phys_row, 0, vrow, 0);
                  xaccMoveCursor (table, phys_row, 0);
                  xaccSRLoadSplitEntry (reg, secondary, 1);
printf ("load split %d at phys row %d addr=%p \n", j, phys_row, secondary);
                  vrow ++;
                  phys_row += reg->split_cursor->numRows; 
               }

               j++;
            } while (secondary);

         } else {
            /* the simple case ... */
            xaccSetCursor (table, lead_cursor, phys_row, 0, vrow, 0);
            xaccMoveCursor (table, phys_row, 0);
            xaccSRLoadTransEntry (reg, split, 1);
            vrow ++;
            phys_row += lead_cursor->numRows; 
         }
      }

      last_split = split;
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
      double last_price = 0.0;

      trans = xaccMallocTransaction ();
      xaccTransBeginEdit (trans);
      xaccTransSetDateToday (trans);
      xaccTransCommitEdit (trans);
      split = xaccTransGetSplit (trans, 0);
      xaccAccountInsertSplit (default_source_acc, split);
      reg->user_hook =  (void *) split;
      reg->destroy = LedgerDestroy;

      /* kind of a cheesy hack to get the price on the last split right
       * when doing stock accounts.   This will guess incorrectly for a 
       * ledger showing multiple stocks, but seems cool for a single stock.
       */
      last_price = xaccSplitGetSharePrice (last_split);
      xaccSplitSetSharePrice (split, last_price);
   }

   /* do the split row of the blank split */
   if (multi_line) {
      Transaction *trans;

      /* do the transaction row of the blank split */
      xaccSetCursor (table, reg->trans_cursor, phys_row, 0, vrow, 0);
      xaccMoveCursor (table, phys_row, 0);
      xaccSRLoadTransEntry (reg, split, 1);
      vrow ++;
      phys_row += reg->trans_cursor->numRows; 
   
      trans = xaccSplitGetParent (split);
      split = xaccTransGetSplit (trans, 1);
      xaccSetCursor (table, reg->split_cursor, phys_row, 0, vrow, 0);
      xaccMoveCursor (table, phys_row, 0);
      xaccSRLoadSplitEntry (reg, split, 1);
      vrow ++;
      phys_row += reg->split_cursor->numRows; 
   } else {
      xaccSetCursor (table, lead_cursor, phys_row, 0, vrow, 0);
      xaccMoveCursor (table, phys_row, 0);
      xaccSRLoadTransEntry (reg, split, 1);
      vrow ++;
      phys_row += lead_cursor->numRows; 
   }

   /* restor the cursor to its rightful position */
   xaccMoveCursorGUI (table, reg->cursor_phys_row, 0);
   xaccRefreshTableGUI (table);

   /* enable callback for cursor user-driven moves */
   table->move_cursor = LedgerMoveCursor;
   table->traverse = LedgerTraverse;
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
