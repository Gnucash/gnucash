/* 
 * FILE:
 * Ledger.c 
 *
 * FUNCTION:
 * copy transaction data from engine into ledger object
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

#include <stdio.h>

#include "Ledger.h"
#include "messages.h"
#include "register.h"
#include "table-allgui.h"
#include "Transaction.h"

#define BUFSIZE 1024

/* ======================================================== */

static void
LedgerMoveCursor  (Table *table, void * client_data)
{
   BasicRegister *reg = (BasicRegister *) client_data;
   xaccSaveRegEntry (reg);
}

/* ======================================================== */

static void
LedgerDestroy (BasicRegister *reg)
{
   /* be sure to destroy the "blank split" */
   if (reg->user_hook) {
      Split *split;
      Transaction * trans;
      Account * acc;

      split = (Split *) (reg->user_hook);

      acc = xaccSplitGetAccount (split);
      xaccAccountRemoveSplit (acc, split);

      trans = xaccSplitGetParent (split);
      xaccFreeTransaction (trans);
      reg->user_hook = NULL;
   }
}

/* ======================================================== */

Split * 
xaccGetCurrentSplit (BasicRegister *reg)
{
   CellBlock *cursor;
   Split *split;

   /* get the handle to the current split and transaction */
   cursor = reg->table->current_cursor;
   split = (Split *) cursor->user_data;

   return split;
}

/* ======================================================== */
/* a split always has a partner */

static char * 
GetOtherAccName (Split *split)
{
   Account *acc = NULL;
   Transaction *trans;
   trans = xaccSplitGetParent (split);

   if (0 == xaccTransIsSource (trans, split)) {
      Split *split = xaccTransGetSourceSplit (trans);
      acc = xaccSplitGetAccount (split);
   } else {
      Split *split = xaccTransGetDestSplit (trans, 0);
      if (NULL != split) {
          Split *nother_split = xaccTransGetDestSplit (trans, 1);

         /* if only one split, then use that */
         if (NULL == nother_split) {
            acc = xaccSplitGetAccount (split);
         } else {
            return SPLIT_STR;
         }
      } 
   }
   if (acc) return xaccAccountGetName (acc);
   return "";
}

/* ======================================================== */

void 
xaccSaveRegEntry (BasicRegister *reg)
{
   Split *split;
   Transaction *trans;
   Account * acc;
   unsigned int changed;

   /* get the handle to the current split and transaction */
   split = xaccGetCurrentSplit (reg);
   if (!split) return;
   trans = xaccSplitGetParent (split);

   /* use the changed flag to avoid heavy-weight updates
    * of the split & transaction fields. This will help
    * cut down on uneccessary register redraws.  */
   changed = xaccGetChangeFlag (reg);
   
   /* copy the contents from the cursor to the split */

   if (MOD_DATE & changed) 
      xaccTransSetDate (trans, reg->dateCell->date.tm_mday,
                               reg->dateCell->date.tm_mon+1,
                               reg->dateCell->date.tm_year+1900);

   if (MOD_NUM & changed) 
      xaccTransSetNum (trans, reg->numCell->value);
   
   if (MOD_DESC & changed) 
      xaccTransSetDescription (trans, reg->descCell->cell.value);

   if (MOD_RECN & changed) 
      xaccSplitSetReconcile (split, reg->recnCell->value[0]);

   if (MOD_AMNT & changed) {
      double new_amount;
      new_amount = (reg->creditCell->amount) - (reg->debitCell->amount);
      xaccSplitSetValue (split, new_amount);
      xaccSplitRebalance (split);
   }

   if (MOD_SHRS & changed) {
      /* hack alert -- implement this */
   }

   if (MOD_PRIC & changed) {
      /* hack alert -- implement this */
   }

   if (MOD_MEMO & changed) 
      xaccSplitSetMemo (split, reg->memoCell->value);

   if (MOD_ACTN & changed) 
      xaccSplitSetAction (split, reg->actionCell->cell.value);

   if (MOD_XFRM & changed) 
      xaccMoveFarEndByName (split, reg->xfrmCell->cell.value);

   if (MOD_XTO & changed) {
      /* hack alert -- implement this */
   }

printf ("finished saving %s \n", xaccTransGetDescription(trans));
   if (changed) {

      /* if the modified split is the "blank split", 
       * then it is now an official part of the account.
       * Set user_hook to null, so that we can be sure of 
       * getting a new split.
       */
      if (split == ((Split *) (reg->user_hook))) {
         reg->user_hook = NULL;
      }

      /* refresh the register windows *only* if something changed */
      acc = xaccSplitGetAccount (split);
      accRefresh (acc);
   }
}

/* ======================================================== */

void
xaccLoadRegEntry (BasicRegister *reg, Split *split)
{
   Transaction *trans;
   char *accname;
   char buff[2];
   Date *d;

   if (!split) return;
   trans = xaccSplitGetParent (split);

   d = xaccTransGetDate (trans);
   xaccSetDateCellValue (reg->dateCell, d->day, d->month, d->year);

   xaccSetBasicCellValue (reg->numCell, xaccTransGetNum (trans));
   xaccSetComboCellValue (reg->actionCell, xaccSplitGetAction (split));
   xaccSetQuickFillCellValue (reg->descCell, xaccTransGetDescription (trans));
   xaccSetBasicCellValue (reg->memoCell, xaccSplitGetMemo (split));

   buff[0] = xaccSplitGetReconcile (split);
   buff[1] = 0x0;
   xaccSetBasicCellValue (reg->recnCell, buff);

   /* the transfer account */
   accname = GetOtherAccName (split);
   xaccSetComboCellValue (reg->xfrmCell, accname);

   xaccSetDebCredCellValue (reg->debitCell, 
                            reg->creditCell, xaccSplitGetValue (split));

   xaccSetAmountCellValue (reg->balanceCell, xaccSplitGetBalance (split));

   xaccSetAmountCellValue (reg->priceCell, xaccSplitGetSharePrice (split));
   xaccSetPriceCellValue  (reg->shrsCell,  xaccSplitGetShareBalance (split));
   xaccSetAmountCellValue (reg->valueCell, xaccSplitGetValue (split));

   reg->table->current_cursor->user_data = (void *) split;

   /* copy cursor contents into the table */
   xaccCommitCursor (reg->table);
}

/* ======================================================== */

void
xaccLoadRegister (BasicRegister *reg, Split **slist, 
                  Account *default_source_acc)
{
   int i;
   Split *split;
   Transaction *trans;
   Table *table;
   int save_cursor_phys_row;
   int num_phys_rows;
   int num_phys_cols;
   int num_virt_rows;
   int phys_row;

   table = reg->table;

   /* disable move callback -- we con't want the cascade of 
    * callbacks while we are fiddling with loading the register */
   table->move_cursor = NULL;

   /* save the current cursor location; we want to restore 
    * it after the reload.  */
   save_cursor_phys_row = table->current_cursor_phys_row;
   xaccMoveCursorGUI (table, -1, -1);

   /* set table size to number of items in list */
   i=0;
   while (slist[i]) i++;

   /* compute the corresponding number of physical & virtual rows. */
   /* number of virtual rows is number of splits,
    * plus one for the header  */
   num_virt_rows = i+1;

   /* plus one for the blank new entry split. */
   if (!(reg->user_hook)) num_virt_rows ++;

   /* num_phys_cols is easy ... just the total number os cells */
   num_phys_cols = reg->header->numCols;

   /* num_phys_rows is the number of rows in all the cursors */
   num_phys_rows = reg->header->numRows;
   num_phys_rows += (num_virt_rows-1) * (reg->cursor->numRows);

   /* num_virt_cols is always one. */
   xaccSetTableSize (table, num_phys_rows, num_phys_cols, num_virt_rows, 1);

   /* make sure that the header is loaded */
   xaccSetCursor (table, reg->header, 0, 0, 0, 0);

printf ("load reg of %d entries --------------------------- \n",i);
   /* populate the table */
   i=0;
   split = slist[0]; 
   while (split) {
      phys_row = reg->header->numRows;
      phys_row += i * (reg->cursor->numRows);

      /* i+1 because header is virt row zero */
      xaccSetCursor (table, reg->cursor, phys_row, 0, i+1, 0);
      xaccMoveCursor (table, phys_row, 0);
      xaccLoadRegEntry (reg, split);

      i++;
      split = slist[i];
   }

   /* add the "blank split" at the end */
   if (!(reg->user_hook)) {
      trans = xaccMallocTransaction ();
      xaccTransSetDateToday (trans);
      split = xaccTransGetSourceSplit (trans);
      xaccAccountInsertSplit (default_source_acc, split);
      reg->user_hook =  (void *) split;
      reg->destroy = LedgerDestroy;

      phys_row = reg->header->numRows;
      phys_row += i * (reg->cursor->numRows);
      xaccSetCursor (table, reg->cursor, phys_row, 0, i+1, 0);
      xaccMoveCursor (table, phys_row, 0);
   
      xaccLoadRegEntry (reg, split);
      i++;
   }
   
   /* restore the cursor to its original location */
   phys_row = reg->header->numRows;
   phys_row += i * (reg->cursor->numRows);

   if (phys_row <= save_cursor_phys_row) {
       save_cursor_phys_row = phys_row - reg->cursor->numRows;
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
   xaccAddComboCellMenuItem (cell, SPLIT_STR);
   LoadXferCell (cell, grp);
}

/* =======================  end of file =================== */
