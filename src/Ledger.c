/* 
 * FILE:
 * Ledger.c 
 *
 * FUNCTION:
 * copy transaction data into ledger
 */

#include "Ledger.h"
#include "messages.h"
#include "register.h"
#include "Transaction.h"

#define BUFSIZE 1024

/* ======================================================== */

static void
LedgerMoveCursor  (struct _Table *_table, void * client_data)
{
   Table * table =  (Table *)_table;
   BasicRegister *reg = (BasicRegister *) client_data;
   xaccSaveRegEntry (reg);
}

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
/* a split always has a partner */

static char * 
GetOtherAccName (Split *split)
{
   Account *acc = NULL;
   Transaction *trans;
   trans = (Transaction *) (split->parent);

   if (split != &(trans->credit_split)) {
      acc = (Account *) trans->credit_split.acc;
   } else {
      if (trans->debit_splits) {
         if (NULL != trans->debit_splits[0]) {
            /* if only one split, then use that */
            if (NULL == trans->debit_splits[1]) {
               acc = (Account *) trans->debit_splits[0]->acc;
            } else {
               return SPLIT_STR;
            }
         }
      } 
   }
   if (acc) return acc->accountName;
   return "";
}

/* ======================================================== */

static void
ModifyXfer (Split * split, const char * new_acc_name)
{
   char * xfr;
   Split *partner_split;
   Transaction *trans = (Transaction *) (split->parent);
   Account * acc;

   if (0 == strcmp (SPLIT_STR, new_acc_name)) return;

   /* check to see whether old an new destination are 
    * the same. If they are, then its a no-op */
   xfr = GetOtherAccName (split);
   if (0 == strcmp (xfr, new_acc_name)) return;

   /* if the new desitnation does not match the current dest,
    * then move the far end of the split to the new location.
    */
printf ("xfr from %s to %s \n", xfr,  new_acc_name);

   if (split != &(trans->credit_split)) {
      partner_split = &(trans->credit_split);

      /* remove the partner split from the old account */
      acc = (Account *) (partner_split->acc);
      xaccRemoveSplit (acc, partner_split);
      accRefresh (acc);

      /* insert the partner split into the new account */
      acc = (Account *) split->acc;
      acc = xaccGetPeerAccountFromName (acc, new_acc_name);
      xaccInsertSplit (acc, partner_split);
      accRefresh (acc);

      /* loop over all of the debit splits, and refresh,
       * since they were all affected. */
      if (trans->debit_splits) {
         int i = 0;
         partner_split = trans->debit_splits[i];
         while (partner_split) {
            acc = (Account *) (partner_split->acc);
            accRefresh (acc);
            i++;
            partner_split = trans->debit_splits[i];
         }
      }
   } else {
      /* perform that transfer *only* if there is
       * one split */
      if (trans->debit_splits) {
         if (0x0 != trans->debit_splits[0]) {
            if (0x0 == trans->debit_splits[1]) {
               partner_split = trans->debit_splits[0];

               /* remove the partner split from the old account */
               acc = (Account *) (partner_split->acc);
               xaccRemoveSplit (acc, partner_split);
               accRefresh (acc);

               /* insert the partner split into the new account */
               acc = (Account *) split->acc;
               acc = xaccGetPeerAccountFromName (acc, new_acc_name);
               xaccInsertSplit (acc, partner_split);
               accRefresh (acc);
            }
         }
      }
   }
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
   trans = (Transaction *) (split->parent);

   /* use the changed flag to avoid heavy-weight updates
    * of the split & transaction fields. This will help
    * cut down on uneccessary register redraws.  */
   changed = xaccGetChangeFlag (reg);
   
printf ("saving %s \n", trans->description);
   /* copy the contents from the cursor to the split */

   if (MOD_DATE & changed) {
      xaccTransSetDate (trans, reg->dateCell->date.tm_mday,
                               reg->dateCell->date.tm_mon+1,
                               reg->dateCell->date.tm_year+1900);

      /* hack alert -- should resort split array's */
   }

   if (MOD_NUM & changed) 
      xaccTransSetNum (trans, reg->numCell->value);
   
   if (MOD_DESC & changed) 
      xaccTransSetDescription (trans, reg->descCell->cell.value);

   if (MOD_RECN & changed) 
      xaccSplitSetReconcile (split, reg->recnCell->value[0]);

   if (MOD_AMNT & changed) {
      /* hack alert copy in new amount */
      xaccTransRecomputeAmount (trans);
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
      ModifyXfer (split, reg->xfrmCell->cell.value);

   if (MOD_XTO & changed) {
      /* hack alert -- implement this */
   }

   /* refresh the register windows *only* if something changed */
   if (changed) {
      acc = (Account *) split->acc;
      accRefresh (acc);
      acc = (Account *) trans->credit_split.acc;
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

   if (!split) return;
   trans = (Transaction *) (split->parent);

printf ("load cell %s %2d/%2d/%4d \n", trans->description,
trans->date.day, trans->date.month, trans->date.year);


   xaccSetDateCellValue (reg->dateCell, trans->date.day, 
                                        trans->date.month,
                                        trans->date.year);

   xaccSetBasicCellValue (reg->numCell, trans->num);
   xaccSetComboCellValue (reg->actionCell, split->action);
   xaccSetQuickFillCellValue (reg->descCell, trans->description);
   xaccSetBasicCellValue (reg->memoCell, split->memo);

   buff[0] = split->reconciled;
   buff[1] = 0x0;
   xaccSetBasicCellValue (reg->recnCell, buff);

   /* the transfer account */
   accname = GetOtherAccName (split);
   xaccSetComboCellValue (reg->xfrmCell, accname);

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
   int save_cursor_row;

   table = reg->table;

   /* disable move callback -- we con't want the cascade of 
    * callbacks while we are fiddling with loading the register */
   table->move_cursor = NULL;

   /* save the current cursor location; we want to restore 
    * it after the reload.  */
   save_cursor_row = table->current_cursor_row;
   xaccMoveCursorGUI (table, -1, -1);

   /* set table size to number of items in list */
   i=0;
   while (slist[i]) i++;
   xaccSetTableSize (table, i+1, 1);

printf ("load reg of %d entries --------------------------- \n",i);
   /* populate the table */
   i=0;
   split = slist[0]; 
   while (split) {

      table->current_cursor_row = i;
      table->current_cursor_col = 0;
      xaccLoadRegEntry (reg, split);

      i++;
      split = slist[i];
   }

   /* add new, disconnected transaction at the end */
   trans = xaccMallocTransaction ();
   table->current_cursor_row = i;
   table->current_cursor_col = 0;
   xaccLoadRegEntry (reg, &(trans->credit_split));
   i++;
   
   /* restore the cursor to it original location */
   if (i <= save_cursor_row)  save_cursor_row = i - 1;
   if (0 > save_cursor_row)  save_cursor_row = 0;
   xaccMoveCursorGUI (table, save_cursor_row, 0);
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
   acc = getAccount (grp, n);
   while (acc) {
      xaccAddComboCellMenuItem (cell, acc->accountName);
      LoadXferCell (cell, acc->children);
      n++;
      acc = getAccount (grp, n);
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
