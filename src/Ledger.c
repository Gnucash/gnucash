/* 
 * FILE:
 * Ledger.c 
 *
 * FUNCTION:
 * copy transaction data into ledger
 */

#include "Ledger.h"
#include "register.h"
#include "Transaction.h"

#define BUFSIZE 1024

/* ======================================================== */

Split * xaccGetCurrentSplit (BasicRegister *reg)
{
   CellBlock *cursor;
   Split *split;

   /* get the handle to the current split and transaction */
   cursor = reg->table->cursor;
   split = (Split *) cursor->user_data;

   return split;
}

/* ======================================================== */

void 
xaccSaveRegEntry (BasicRegister *reg)
{
   Split *split;
   Transaction *trans;

   /* get the handle to the current split and transaction */
   split = xaccGetCurrentSplit (reg);
   if (!split) return;
   trans = (Transaction *) (split->parent);
   
   /* copy the contents from the cursor to the split */
   xaccTransSetDate (trans, reg->dateCell->date.tm_mday,
                            reg->dateCell->date.tm_mon+1,
                            reg->dateCell->date.tm_year+1900);

   xaccTransSetNum (trans, reg->numCell->value);
   xaccTransSetDescription (trans, reg->descCell->cell.value);
   xaccSplitSetMemo (split, reg->memoCell->value);
   xaccSplitSetAction (split, reg->actionCell->cell.value);
   xaccSplitSetReconcile (split, reg->recnCell->value[0]);
}

/* ======================================================== */

void
xaccLoadRegEntry (BasicRegister *reg, Split *split)
{
   Transaction *trans;
   char buff[2];

   if (!split) return;
   trans = (Transaction *) (split->parent);

   xaccSetDateCellValue (reg->dateCell, trans->date.day, 
                                        trans->date.month,
                                        trans->date.year);

   xaccSetBasicCellValue (reg->numCell, trans->num);
   xaccSetBasicCellValue (&(reg->actionCell->cell), split->action);
   xaccSetQuickFillCellValue (reg->descCell, trans->description);
   xaccSetBasicCellValue (reg->memoCell, split->memo);

   buff[0] = split->reconciled;
   buff[1] = 0x0;
   xaccSetBasicCellValue (reg->recnCell, buff);

   xaccSetDebCredCellValue (reg->debitCell, 
                            reg->creditCell, split->damount);

   xaccSetAmountCellValue (reg->balanceCell, split->balance);

   reg->table->cursor->user_data = (void *) split;

   /* copy cursor contents into the table */
   xaccCommitCursor (reg->table);
}

/* ======================================================== */

void
xaccLoadRegister (BasicRegister *reg, Split **slist)
{
   int i;
   Split *split;
   Transaction *trans;
   char buff[BUFSIZE];
   Table *table;

   table = reg->table;

   /* set table size to number of items in list */
   i=0;
   while (slist[i]) i++;
   xaccSetTableSize (table, i, 1);

   /* populate the table */
   i=0;
   split = slist[0]; 
   while (split) {

      xaccMoveCursor (table, i, 0);
      xaccLoadRegEntry (reg, split);

      i++;
      split = slist[i];
   }
   xaccRefreshTableGUI (table);
}

/* ======================================================== */

void xaccLoadXferCell (ComboCell *cell,  AccountGroup *grp)
{
   Account * acc;
   int n;

   if (!grp) return;

   /* build the xfer menu out of account names */
   /* traverse sub-accounts ecursively */
   n = 0;
   acc = getAccount (grp, n);
   while (acc) {
      xaccAddComboCellMenuItem (cell, acc->accountName);
      xaccLoadXferCell (cell, acc->children);
      n++;
      acc = getAccount (grp, n);
   }
}

/* =======================  end of file =================== */
