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

void 
xaccCommitEdits (BasicRegister *reg)
{
   CellBlock *cursor;
   Split *split;
   Transaction *trans;

   cursor = reg->table->cursor;

   split = (Split *) cursor->user_data;
   if (!split) return;

   trans = (Transaction *) (split->parent);
   
   if (trans->num) free (trans->num);
   trans->num = strdup (reg->numCell->value);
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

      trans = (Transaction *) (split->parent);

      xaccMoveCursor (table, i, 0);

      sprintf (buff, "%2d/%2d/%4d", trans->date.day, 
                                    trans->date.month,
                                    trans->date.year);

      xaccSetBasicCellValue (reg->dateCell, buff);

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

      table->cursor->user_data = (void *) split;

      /* copy cursor contents into the table */
      xaccCommitCursor (table);

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
