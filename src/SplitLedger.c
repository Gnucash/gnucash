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
 * To implement the above, the register "user_data" is used
 * to store an SRInfo structure containing the blank split.
 *
 * =====================================================================
 * Some notes on Commit/Rollback:
 * 
 * There's an engine component and a gui component to the commit/rollback
 * scheme.  On the engine side, one must always call BeginEdit()
 * before starting to edit a transaction.  When you think you're done,
 * you can call CommitEdit() to commit the changes, or RollbackEdit() to
 * go back to how things were before you started the edit.  Think of it as
 * a one-shot mega-undo for that transaction.
 * 
 * Note that the query engine uses the original values, not the currently
 * edited values, when performing a sort.  This allows your to e.g. edit
 * the date without having the transaction hop around in the gui while you
 * do it.
 * 
 * On the gui side, commits are now performed on a per-transaction basis,
 * rather than a per-split (per-journal-entry) basis.  This means that
 * if you have a transaction with a lot of splits in it, you can edit them
 * all you want without having to commit one before moving to the next.
 * 
 * Similarly, the "cancel" button will now undo the changes to all of the
 * lines in the transaction display, not just to one line (one split) at a
 * time.
 * 
 * =====================================================================
 * Some notes on Reloads & Redraws:
 * 
 * Reloads and redraws tend to be heavyweight. We try to use change flags
 * as much as possible in this code, but imagine the following scenario:
 *
 * Create two bank accounts.  Transfer money from one to the other.
 * Open two registers, showing each account. Change the amount in one window.
 * Note that the other window also redraws, to show the new correct amount.
 * 
 * Since you changed an amount value, potentially *all* displayed
 * balances change in *both* register windows (as well as the ledger
 * balance in the main window).  Three or more windows may be involved
 * if you have e.g. windows open for bank, employer, taxes and your
 * entering a paycheck (or correcting a typo in an old paycheck).
 * Changing a date might even cause all entries in all three windows
 * to be re-ordered.
 *
 * The only thing I can think of is a bit stored with every table
 * entry, stating 'this entry has changed since lst time, redraw it'.
 * But that still doesn't avoid the overhead of reloading the table
 * from the engine.
 * 
 *
 * HISTORY:
 * Copyright (c) 1998-2000 Linas Vepstas */

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

#define _GNU_SOURCE

#include <stdio.h>
#include <time.h>

#include "top-level.h"

#include "ui-callbacks.h"
#include "messages.h"
#include "SplitLedger.h"
#include "MultiLedger.h"
#include "FileDialog.h"
#include "Refresh.h"
#include "splitreg.h"
#include "table-allgui.h"
#include "util.h"

#define BUFSIZE 1024

typedef struct _SRInfo SRInfo;
struct _SRInfo
{
  /* The blank split at the bottom of the register */
  Split * blank_split;

  /* The currently open transaction, if any */
  Transaction *pending_trans;

  /* A transaction used to remember where to put the cursor */
  Transaction *cursor_hint_trans;

  /* A split used to remember where to put the cursor */
  Split *cursor_hint_split;

  /* A column used to remember which column to put the cursor */
  int cursor_hint_phys_col;

  /* The default account where new splits are added */
  Account *default_source_account;

  /* The last date recorded in the blank split */
  time_t last_date_entered;

  /* User data for users of SplitRegisters */
  void *user_data;

  /* hook to get parent widget */
  SRGetParentCallback get_parent;

  /* hook to set help string */
  SRSetHelpCallback set_help;
};


/* ======================================================== */
/* The force_double_entry_awareness flag controls how the 
 * register behaves if the user failed to specify a transfer-to
 * account when creating a new split. What it does is simple,
 * although it can lead to some confusion to the user.
 * If this flag is set, then any new split will be put into
 * the leader account. What happens visually is that it appears
 * as if there are two transactions, one debiting and one crediting
 * this account by exactly the same amount. Thus, the user is forced
 * to deal with this somewhat nutty situation.
 *
 * If this flag is *not* set, then the split just sort of 
 * hangs out, without belonging to any account. This will 
 * of course lead to a ledger that fails to balance.
 * Bummer, duude !
 *
 * hack alert -- this flag should really be made a configurable 
 * item in some config script.
 */

static int force_double_entry_awareness = 0;

/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_LEDGER;

/* The character used to separate accounts. */
static char account_separator = ':';

/* The reverse balance callback, if any. */
static SRReverseBalanceCallback reverse_balance = NULL;

/* static prototypes */
static void xaccSRLoadRegEntry (SplitRegister *reg, Split *split);
static Transaction * xaccSRGetTrans (SplitRegister *reg,
                                     int phys_row, int phys_col);


/* The routines below create, access, and destroy the SRInfo structure
 * used by SplitLedger routines to store data for a particular register.
 * This is the only code that should access the user_data member of a
 * SplitRegister directly. If additional user data is needed, just add
 * it to the SRInfo structure above. */
static void
xaccSRInitRegisterData(SplitRegister *reg)
{
  SRInfo *info;

  if (reg == NULL)
    return;

  /* calloc initializes to 0 */
  info = calloc(1, sizeof(SRInfo));
  assert(info != NULL);

  info->last_date_entered = time(NULL);

  reg->user_data = info;
}

static void
xaccSRDestroyRegisterData(SplitRegister *reg)
{
  if (reg == NULL)
    return;

  if (reg->user_data != NULL)
    free(reg->user_data);

  reg->user_data = NULL;
}

static SRInfo *
xaccSRGetInfo(SplitRegister *reg)
{
  assert(reg != NULL);

  if (reg->user_data == NULL)
    xaccSRInitRegisterData(reg);

  return (SRInfo *) reg->user_data;
}

static gncUIWidget
xaccSRGetParent(SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);

  if (reg == NULL)
    return NULL;

  if (info->get_parent == NULL)
    return NULL;

  return (info->get_parent)(info->user_data);
}

void
xaccSRSetData(SplitRegister *reg, void *user_data,
              SRGetParentCallback get_parent,
              SRSetHelpCallback set_help)
{
  SRInfo *info = xaccSRGetInfo(reg);

  assert(reg != NULL);

  info->user_data = user_data;
  info->get_parent = get_parent;
  info->set_help = set_help;
}

void
xaccSRSetAccountSeparator(char separator)
{
  account_separator = separator;
}

void
xaccSRSetReverseBalanceCallback(SRReverseBalanceCallback callback)
{
  reverse_balance = callback;
}


/* copies basic split values from 'from' split to 'to' split.
 * doesn't copy reconciled flag, or open the parent transactions
 * for editing. Does *not* insert the 'to' split into an account!! */
static void
gnc_copy_split(Split *from, Split *to)
{
  if ((from == NULL) || (to == NULL))
    return;

  xaccSplitSetMemo(to, xaccSplitGetMemo(from));
  xaccSplitSetAction(to, xaccSplitGetAction(from));
  xaccSplitSetDocref(to, xaccSplitGetDocref(from));
  xaccSplitSetSharePriceAndAmount(to,
                                  xaccSplitGetSharePrice(from),
                                  xaccSplitGetShareAmount(from));
}

/* copies the basic transaction values and the splits from the
 * 'from' trans to the 'to' trans. Any existing splits in the
 * 'to' trans are deleted. Does *not* open the 'to' trans for
 * editing!!! Splits are copied using gnc_copy_split above.
 * The new splits will be in exactly the same order as in
 * the 'from' transaction. */
static void
gnc_copy_trans(Transaction *from, Transaction *to)
{
  Split *from_split, *to_split;
  Timespec timespec;
  int num_splits;
  int i;

  if ((from == NULL) || (to == NULL))
    return;

  /* remove the old splits */
  to_split = xaccTransGetSplit(to, 0);
  while (to_split != NULL)
  {
    xaccSplitDestroy(to_split);
    to_split = xaccTransGetSplit(to, 0);
  }

  /* add in the new splits */
  num_splits = xaccTransCountSplits(from);
  for (i = 0; i < num_splits; i++)
  {
    from_split = xaccTransGetSplit(from, i);

    to_split = xaccMallocSplit();
    gnc_copy_split(from_split, to_split);

    xaccTransAppendSplit(to, to_split);
  }

  /* now do the transaction-specific values */
  xaccTransGetDateTS(from, &timespec);
  xaccTransSetDateTS(to, &timespec);
  xaccTransSetDescription(to, xaccTransGetDescription(from));
  xaccTransSetDocref(to, xaccTransGetDocref(from));
}

/* ======================================================== */
/* this callback gets called when the user clicks on the gui
 * in such a way as to leave the current virtual cursor, and 
 * go to a new one. So, save the current transaction.
 *
 * This callback is centrally involved in the redraw sequence.
 * When the user moves from one cell to another, the following 
 * sequence of events get triggered and cascade down:
 *  VerifyCursorPosition() {
 *    MoveCursor() {  
 *      callback for move() which is this function (LedgerMoveCursor) {
 *        SaveRegEntry() {...}
 *        RedrawRegEntry() {
 *          SRLoadRegister() {
 *            SRLoadRegEntry() {
 *              MoveCursor () { }
 *            }
 *          }
 *        }}}}
 */

static void
LedgerMoveCursor (Table *table, 
                  int *p_new_phys_row, 
                  int *p_new_phys_col, 
                  void * client_data)
{
  int new_phys_row = *p_new_phys_row;
  int new_phys_col = *p_new_phys_col;
  SplitRegister *reg = (SplitRegister *) client_data;
  SRInfo *info = xaccSRGetInfo(reg);
  Transaction *pending_trans;
  Transaction *new_trans;
  Transaction *trans;
  Split *new_split;
  gncBoolean saved;
  Locator *locator;
  int new_cell_row;
  int new_cell_col;
  int phys_row_offset;
  int phys_col_offset;
  int style;

  PINFO ("LedgerMoveCursor(): start callback %d %d \n",
         new_phys_row, new_phys_col);

  /* The transaction we are coming from */
  trans = xaccSRGetCurrentTrans(reg);

  /* The change in physical location */
  phys_row_offset = new_phys_row - table->current_cursor_phys_row;
  phys_col_offset = new_phys_col - table->current_cursor_phys_col;

  /* The transaction where we are moving to */
  new_trans = xaccSRGetTrans(reg, new_phys_row, new_phys_col);

  /* The split we are moving to */
  new_split = xaccGetUserData(reg->table, new_phys_row, new_phys_col);

  /* The cell offset we are moving to */
  locator = table->locators[new_phys_row][new_phys_col];
  new_cell_row = locator->phys_row_offset;
  new_cell_col = locator->phys_col_offset;

  /* commit the contents of the cursor into the database */
  saved = xaccSRSaveRegEntry (reg, new_trans);
  if ((info->pending_trans != NULL) &&
      (info->pending_trans == trans) &&
      (trans != new_trans))
  {
    if (xaccTransIsOpen(trans))
      xaccTransCommitEdit (trans);
    info->pending_trans = NULL;
    pending_trans = NULL;
    saved = GNC_T;
  }
  else
    pending_trans = info->pending_trans;

  /* redrawing the register can muck everything up */
  if (saved) {
    int virt_row, virt_col;

    xaccSRRedrawRegEntry (reg);

    /* if the transaction is no longer in the register,
     * we commit it now. This is a hack that can go away
     * once we have transaction ids. */
    if ((pending_trans != info->pending_trans) &&
        xaccTransIsOpen(pending_trans))
      xaccTransCommitEdit (pending_trans);

    /* if the split we were going to is still in the register,
     * then it may have moved. Find out where it is now. */
    if (xaccSRGetTransSplitRowCol (reg, new_trans, new_split,
                                   &virt_row, &virt_col)) {
      RevLocator *rev_locator;

      rev_locator = table->rev_locators[virt_row][virt_col];

      new_phys_row = rev_locator->phys_row;
      new_phys_col = rev_locator->phys_col;

      new_phys_row += new_cell_row;
      new_phys_col += new_cell_col;
    }
    /* otherwise, the split is not in the register and we
     * have to figure out where to go. We make a guess based
     * on the change in physical location that was going to
     * happen before the refresh. */
    else {
      new_phys_row = table->current_cursor_phys_row + phys_row_offset;
      new_phys_col = table->current_cursor_phys_col + phys_col_offset;
    }

    /* just because I'm paranoid doesn't
     * mean they're not out to get me! */
    if (new_phys_row < reg->num_header_rows)
      new_phys_row = reg->num_header_rows;
    else if (new_phys_row >= table->num_phys_rows)
      new_phys_row = table->num_phys_rows - 1;

    if (new_phys_col < 0)
      new_phys_col = 0;
    else if (new_phys_col >= table->num_phys_cols)
      new_phys_col = table->num_phys_cols - 1;

    gnc_table_find_valid_cell_horiz(table, &new_phys_row, &new_phys_col, GNC_F);

    *p_new_phys_row = new_phys_row;
    *p_new_phys_col = new_phys_col;
  }

  PINFO ("LedgerMoveCursor(): after redraw %d %d \n",
         new_phys_row, new_phys_col);

  reg->cursor_phys_row = new_phys_row;

  locator = table->locators[new_phys_row][new_phys_col];
  reg->cursor_virt_row = locator->virt_row;

  /* if auto-expansion is enabled, we need to redraw the register
   * to expand out the splits at the new location. We use the
   * cursor_hint data members to tell the refresh routine where
   * to go. */
  style = ((reg->type) & REG_STYLE_MASK);
  if ((REG_SINGLE_DYNAMIC == style) ||
      (REG_DOUBLE_DYNAMIC == style)) 
  {
    new_trans = xaccSRGetTrans(reg, new_phys_row, new_phys_col);
    info->cursor_hint_trans = new_trans;

    new_split = xaccGetUserData (reg->table, new_phys_row, new_phys_col);
    info->cursor_hint_split = new_split;

    info->cursor_hint_phys_col = new_phys_col;

    xaccRegisterRefresh (reg);

    /* indicate what row we should go to */
    *p_new_phys_row = table->current_cursor_phys_row;
    *p_new_phys_col = table->current_cursor_phys_col;

    PINFO ("LedgerMoveCursor(): after dynamic %d %d stored val %d\n",
           *p_new_phys_row, *p_new_phys_col, reg->cursor_phys_row);
  }
}

/* ======================================================== */
/* this callback gets called when the user clicks on the gui
 * in such a way as to leave the current transaction, and to 
 * go to a new one. It is called to verify what the coordinates
 * of the new cell will be. It currently does nothing.
 */

static void
LedgerTraverse (Table *table, 
                int *p_new_phys_row, 
                int *p_new_phys_col, 
                void * client_data)
{
  SplitRegister *reg = client_data;
  SRInfo *info = xaccSRGetInfo(reg);
  Transaction *trans, *new_trans;
  int phys_row = *p_new_phys_row;
  int phys_col = *p_new_phys_col;
  int virt_row, virt_col;
  unsigned int changed;
  GNCVerifyResult result;

  trans = xaccSRGetCurrentTrans(reg);
  if (trans == NULL)
    return;

  /* no changes, no worries */
  changed = xaccSplitRegisterGetChangeFlag(reg);
  if (!changed && (info->pending_trans != trans))
    return;

  /* Now see if we are changing cursors. If not, no problems */
  if ((phys_row >= 0) && (phys_col >= 0) &&
      (phys_row < table->num_phys_rows) && (phys_col < table->num_phys_cols))
  {
    virt_row = table->locators[phys_row][phys_col]->virt_row;
    virt_col = table->locators[phys_row][phys_col]->virt_col;

    if ((virt_row == table->current_cursor_virt_row) &&
        (virt_col == table->current_cursor_virt_col))
      return;
  }

  /* Same transaction, no problem */
  new_trans = xaccSRGetTrans(reg, phys_row, phys_col);
  if (trans == new_trans)
    return;

  /* Ok, we are changing transactions and the current transaction has
   * changed. See what the user wants to do. */

  result = gnc_verify_cancel_dialog_parented(xaccSRGetParent(reg),
                                             TRANS_CHANGED_MSG,
                                             GNC_VERIFY_YES);

  switch (result)
  {
    case GNC_VERIFY_NO:
      xaccSRCancelCursorTransChanges(reg);
      break;
    case GNC_VERIFY_CANCEL:
      *p_new_phys_row = table->current_cursor_phys_row;
      *p_new_phys_col = table->current_cursor_phys_col;
      break;
    default:
      break;
  }
}

/* ======================================================== */

static void
LedgerSetHelp (Table *table, const char *help_str, void *client_data)
{
  SplitRegister *reg = client_data;
  SRInfo *info = xaccSRGetInfo(reg);

  if (info->set_help == NULL)
    return;

  info->set_help(info->user_data, help_str);
}

/* ======================================================== */

static void
LedgerDestroy (SplitRegister *reg)
{
   SRInfo *info = xaccSRGetInfo(reg);
   Transaction *trans;

   /* be sure to destroy the "blank split" */
   if (info->blank_split) {
      /* split destroy will automatically remove it
       * from its parent account */
      trans = xaccSplitGetParent (info->blank_split);

      /* Make sure we don't commit this below */
      if (trans == info->pending_trans)
        info->pending_trans = NULL;

      xaccTransBeginEdit (trans, 1);
      xaccTransDestroy (trans);
      xaccTransCommitEdit (trans);

      info->blank_split = NULL;
   }

   /* be sure to take care of any open transactions */
   if (info->pending_trans) {
      /* Committing this should have been taken care of by
       * xaccLedgerDisplayClose. But, we'll check again. */
      if (xaccTransIsOpen(info->pending_trans))
        xaccTransCommitEdit (info->pending_trans);

      info->pending_trans = NULL;
   }

   xaccSRDestroyRegisterData(reg);
}

/* ======================================================== */

static Transaction *
xaccSRGetTrans (SplitRegister *reg, int phys_row, int phys_col)
{
  Split *split;
  int virt_row, virt_col;

  if (reg == NULL)
    return NULL;

  if ((phys_row < 0) || (phys_col < 0) ||
      (phys_row >= reg->table->num_phys_rows) ||
      (phys_col >= reg->table->num_phys_cols))
    return NULL;

  split = xaccGetUserData(reg->table, phys_row, phys_col);
  if (split != NULL)
    return xaccSplitGetParent(split);

  /* Split is blank. Assume it is the blank split of a multi-line
   * transaction. Go back one row to find a split in the transaction. */
  virt_row = reg->table->locators[phys_row][phys_col]->virt_row;
  virt_col = reg->table->locators[phys_row][phys_col]->virt_col;

  virt_row --;
  if ((0 > virt_row) || (0 > virt_col)) {
    PERR ("Internal Error: xaccSRGetTrans(): bad row \n");
    return NULL;
  }

  split = (Split *) reg->table->user_data[virt_row][virt_col];
  if (split == NULL) {
    PERR ("Internal Error: xaccSRGetTrans(): no parent \n");
    return NULL;
  }

  return xaccSplitGetParent(split);
}

/* ======================================================== */

Transaction *
xaccSRGetCurrentTrans (SplitRegister *reg)
{
  Split *split;
  int phys_row, phys_col;
  int virt_row, virt_col;

  if (reg == NULL)
    return NULL;

  split = xaccSRGetCurrentSplit (reg);
  if (split != NULL)
    return xaccSplitGetParent(split);

  /* Split is blank. Assume it is the blank split of a multi-line
   * transaction. Go back one row to find a split in the transaction. */
  phys_row = reg->table->current_cursor_phys_row;
  phys_col = reg->table->current_cursor_phys_col;

  if ((phys_row < 0) || (phys_col < 0) ||
      (phys_row >= reg->table->num_phys_rows) ||
      (phys_col >= reg->table->num_phys_cols))
    return NULL;

  virt_row = reg->table->locators[phys_row][phys_col]->virt_row;
  virt_col = reg->table->locators[phys_row][phys_col]->virt_col;

  virt_row --;
  if ((0 > virt_row) || (0 > virt_col)) {
    PERR ("Internal Error: xaccSRGetCurrentTrans(): bad row \n");
    return NULL;
  }

  split = (Split *) reg->table->user_data[virt_row][virt_col];

  return xaccSplitGetParent(split);
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

Split * 
xaccSRGetBlankSplit (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);

  return info->blank_split;
}

/* ======================================================== */

gncBoolean
xaccSRGetSplitRowCol (SplitRegister *reg, Split *split,
                      int *virt_row, int *virt_col)
{
  Table *table = reg->table;
  int v_row, v_col;
  Split *s;

  for (v_row = 1; v_row < table->num_virt_rows; v_row++)
    for (v_col = 0; v_col < table->num_virt_cols; v_col++)
    {
      s = (Split *) table->user_data[v_row][v_col];

      if (s == split)
      {
        if (virt_row != NULL)
          *virt_row = v_row;
        if (virt_col != NULL)
          *virt_col = v_col;

        return GNC_T;
      }
    }

  return GNC_F;
}

/* ======================================================== */

gncBoolean
xaccSRGetTransSplitRowCol (SplitRegister *reg,
                           Transaction *trans, Split *split,
                           int *virt_row, int *virt_col)
{
  Table *table = reg->table;
  gncBoolean found = GNC_F;
  int v_row, v_col;
  Split *s;

  for (v_row = 1; v_row < table->num_virt_rows; v_row++)
    for (v_col = 0; v_col < table->num_virt_cols; v_col++)
    {
      s = (Split *) table->user_data[v_row][v_col];

      if (xaccSplitGetParent(s) == trans)
        found = GNC_T;

      if (found && (s == split))
      {
        if (virt_row != NULL)
          *virt_row = v_row;
        if (virt_col != NULL)
          *virt_col = v_col;

        return GNC_T;
      }
    }

  return GNC_F;
}

/* ======================================================== */

Split *
xaccSRDuplicateCurrent (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  CursorType cursor_type;
  unsigned int changed;
  Transaction *trans;
  Split *return_split;
  Split *split;

  split = xaccSRGetCurrentSplit(reg);
  trans = xaccSRGetCurrentTrans(reg);

  /* This shouldn't happen, but be paranoid. */
  if (trans == NULL)
    return NULL;

  cursor_type = xaccSplitRegisterGetCursorType(reg);

  /* Can't do anything with this. */
  if (cursor_type == CURSOR_NONE)
    return NULL;

  /* This shouldn't happen, but be paranoid. */
  if ((split == NULL) && (cursor_type == CURSOR_TRANS))
    return NULL;

  changed = xaccSplitRegisterGetChangeFlag(reg);

  /* See if we were asked to duplicate an unchanged blank split.
   * There's no point in doing that! */
  if (!changed && ((split == NULL) || (split == info->blank_split)))
    return NULL;

  /* If the cursor has been edited, we are going to have to commit
   * it before we can duplicate. Make sure the user wants to do that. */
  if (changed)
  {
    GNCVerifyResult result;

    result = gnc_ok_cancel_dialog_parented(xaccSRGetParent(reg),
                                           TRANS_CHANGED_MSG,
                                           GNC_VERIFY_OK);

    if (result == GNC_VERIFY_CANCEL)
      return NULL;

    xaccSRSaveRegEntry(reg, NULL);

    /* If the split is NULL, then we were on a blank split row
     * in an expanded transaction. The new split (created by
     * xaccSRSaveRegEntry above) will be the last split in the
     * current transaction, as it was just added. */
    if (split == NULL)
      split = xaccTransGetSplit(trans, xaccTransCountSplits(trans) - 1);
  }

  /* Ok, we are now ready to make the copy. */

  if (cursor_type == CURSOR_SPLIT)
  {
    Split *new_split;
    Account *account;

    /* We are on a split in an expanded transaction.
     * Just copy the split and add it to the transaction. */

    new_split = xaccMallocSplit();

    gnc_copy_split(split, new_split);

    account = xaccSplitGetAccount(split);

    xaccTransBeginEdit(trans, GNC_T);
    xaccAccountBeginEdit(account, GNC_T);

    xaccTransAppendSplit(trans, new_split);
    xaccAccountInsertSplit(account, new_split);

    xaccAccountCommitEdit(account);
    xaccTransCommitEdit(trans);

    return_split = new_split;
  }
  else
  {
    Transaction *new_trans;
    int num_splits;
    int i;

    /* We are on a transaction row. Copy the whole transaction. */

    new_trans = xaccMallocTransaction();

    xaccTransBeginEdit(new_trans, GNC_T);

    gnc_copy_trans(trans, new_trans);

    num_splits = xaccTransCountSplits(trans);
    return_split = NULL;

    /* Link the new splits into the accounts. */
    for (i = 0; i < num_splits; i++)
    {
      Account *account;
      Split *old_split;
      Split *new_split;

      old_split = xaccTransGetSplit(trans, i);
      account = xaccSplitGetAccount(old_split);

      new_split = xaccTransGetSplit(new_trans, i);

      xaccAccountBeginEdit(account, GNC_T);
      xaccAccountInsertSplit(account, new_split);
      xaccAccountCommitEdit(account);

      /* Returned split is the transaction split */
      if (old_split == split)
        return_split = new_split;
    }

    xaccTransCommitEdit(new_trans);

    /* This shouldn't happen, but be paranoid. */
    if (return_split == NULL)
      return_split = xaccTransGetSplit(new_trans, 0);
  }

  /* Refresh the GUI. */
  gnc_transaction_ui_refresh(trans);
  gnc_refresh_main_window();

  return return_split;
}

/* ======================================================== */

void
xaccSRDeleteCurrentSplit (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Split *split, *s;
  Transaction *trans;
  int i, num_splits;
  Account *account, **affected_accounts;

  /* get the current split based on cursor position */
  split = xaccSRGetCurrentSplit(reg);
  if (split == NULL)
    return;

  /* If we are deleting the blank split, just cancel. The user is
   * allowed to delete the blank split as a method for discarding
   * any edits they may have made to it. */
  if (split == info->blank_split)
  {
    xaccSRCancelCursorSplitChanges(reg);
    return;
  }

  /* make a copy of all of the accounts that will be  
   * affected by this deletion, so that we can update
   * their register windows after the deletion. */
  trans = xaccSplitGetParent(split);
  num_splits = xaccTransCountSplits(trans);
  affected_accounts = (Account **) malloc((num_splits + 1) *
                                          sizeof(Account *));
  assert(affected_accounts != NULL);

  for (i = 0; i < num_splits; i++) 
  {
    s = xaccTransGetSplit(trans, i);
    affected_accounts[i] = xaccSplitGetAccount(s);
  }
  affected_accounts[num_splits] = NULL;

  account = xaccSplitGetAccount(split);

  xaccTransBeginEdit(trans, 1);
  xaccAccountBeginEdit(account, 1);
  xaccSplitDestroy(split);
  xaccAccountCommitEdit(account);
  xaccTransCommitEdit(trans);

  /* Check pending transaction */
  if (trans == info->pending_trans)
    info->pending_trans = NULL;

  gnc_account_list_ui_refresh(affected_accounts);

  free(affected_accounts);

  gnc_refresh_main_window ();
}

/* ======================================================== */

void
xaccSRDeleteCurrentTrans (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Split *split, *s;
  Transaction *trans;
  int i, num_splits;
  Account *account, **affected_accounts;

  /* get the current split based on cursor position */
  split = xaccSRGetCurrentSplit(reg);
  if (split == NULL)
    return;

  /* If we just deleted the blank split, clean up. The user is
   * allowed to delete the blank split as a method for discarding
   * any edits they may have made to it. */
  if (split == info->blank_split)
  {
    trans = xaccSplitGetParent (info->blank_split);
    account = xaccSplitGetAccount(split);

    /* Make sure we don't commit this later on */
    if (trans == info->pending_trans)
      info->pending_trans = NULL;

    xaccTransBeginEdit (trans, 1);
    xaccTransDestroy (trans);
    xaccTransCommitEdit (trans);

    info->blank_split = NULL;

    xaccAccountDisplayRefresh(account);
    return;
  }

  /* make a copy of all of the accounts that will be  
   * affected by this deletion, so that we can update
   * their register windows after the deletion.
   */
  trans = xaccSplitGetParent(split);
  num_splits = xaccTransCountSplits(trans);
  affected_accounts = (Account **) malloc((num_splits + 1) *
                                          sizeof(Account *));
  assert(affected_accounts != NULL);

  for (i = 0; i < num_splits; i++) 
  {
    s = xaccTransGetSplit(trans, i);
    affected_accounts[i] = xaccSplitGetAccount(s);
  }
  affected_accounts[num_splits] = NULL;

  xaccTransBeginEdit(trans, 1);
  xaccTransDestroy(trans);
  xaccTransCommitEdit(trans);

  /* Check pending transaction */
  if (trans == info->pending_trans)
    info->pending_trans = NULL;

  gnc_account_list_ui_refresh(affected_accounts);

  free(affected_accounts);

  gnc_refresh_main_window ();
}

/* ======================================================== */

void
xaccSREmptyCurrentTrans (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Split *split, *s;
  Split **splits;
  Transaction *trans;
  int i, num_splits;
  Account *account, **affected_accounts;

  /* get the current split based on cursor position */
  split = xaccSRGetCurrentSplit(reg);
  if (split == NULL)
    return;

  /* If we just deleted the blank split, clean up. The user is
   * allowed to delete the blank split as a method for discarding
   * any edits they may have made to it. */
  if (split == info->blank_split)
  {
    trans = xaccSplitGetParent (info->blank_split);
    account = xaccSplitGetAccount(split);

    /* Make sure we don't commit this later on */
    if (trans == info->pending_trans)
      info->pending_trans = NULL;

    xaccTransBeginEdit (trans, 1);
    xaccTransDestroy (trans);
    xaccTransCommitEdit (trans);

    info->blank_split = NULL;

    xaccAccountDisplayRefresh(account);
    return;
  }

  /* make a copy of all of the accounts that will be  
   * affected by this deletion, so that we can update
   * their register windows after the deletion.
   */
  trans = xaccSplitGetParent(split);
  num_splits = xaccTransCountSplits(trans);
  affected_accounts = (Account **) malloc((num_splits + 1) *
                                          sizeof(Account *));
  splits = calloc(num_splits, sizeof(Split *));

  assert(affected_accounts != NULL);
  assert(splits != NULL);

  for (i = 0; i < num_splits; i++) 
  {
    s = xaccTransGetSplit(trans, i);
    splits[i] = s;
    affected_accounts[i] = xaccSplitGetAccount(s);
  }
  affected_accounts[num_splits] = NULL;

  xaccTransBeginEdit(trans, 1);
  for (i = 0; i < num_splits; i++)
    if (splits[i] != split)
      xaccSplitDestroy(splits[i]);
  xaccTransCommitEdit(trans);

  /* Check pending transaction */
  if (trans == info->pending_trans)
    info->pending_trans = NULL;

  gnc_account_list_ui_refresh(affected_accounts);

  free(affected_accounts);
  free(splits);

  gnc_refresh_main_window ();
}

/* ======================================================== */

void
xaccSRCancelCursorSplitChanges (SplitRegister *reg)
{
  Split * split;
  unsigned int changed;
  int row = reg->table->current_cursor_phys_row;
  int col = reg->table->current_cursor_phys_col;

  changed = xaccSplitRegisterGetChangeFlag(reg);
  if (!changed)
    return;

  /* We're just cancelling the current split here, not the transaction */
  /* When cancelling edits, reload the cursor from the transaction */
  split = xaccSRGetCurrentSplit(reg);
  xaccSRLoadRegEntry(reg, split);
  xaccSplitRegisterClearChangeFlag(reg);

  if (gnc_table_find_valid_cell_horiz(reg->table, &row, &col, GNC_F))
    xaccMoveCursorGUI(reg->table, row, col);

  xaccRefreshTableGUI(reg->table);
}

/* ======================================================== */

void
xaccSRCancelCursorTransChanges (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Transaction *trans;
  Split *split;
  int i, num_splits = 0, more_splits = 0;
  Account **affected_accounts = NULL;

  /* Get the currently open transaction, rollback the edits on it, and
   * then repaint everything. To repaint everything, make a note of
   * all of the accounts that will be affected by this rollback. Geez,
   * there must be some easier way of doing redraw notification. */
  trans = info->pending_trans;

  if (!xaccTransIsOpen(trans))
  {
    xaccSRCancelCursorSplitChanges(reg);
    return;
  }

  num_splits = xaccTransCountSplits (trans);
  affected_accounts = (Account **) malloc ((num_splits+1) *
                                           sizeof (Account *));

  for (i = 0; i < num_splits; i++)
  {
    split = xaccTransGetSplit (trans, i);
    affected_accounts[i] = xaccSplitGetAccount (split);
  }
  affected_accounts[num_splits] = NULL;

  xaccTransRollbackEdit (trans);

  /* and do some more redraw, for the new set of accounts .. */
  more_splits = xaccTransCountSplits (trans);
  affected_accounts = (Account **) realloc (affected_accounts, 
                                            (more_splits+num_splits+1) *
                                            sizeof (Account *));

  for (i = 0; i < more_splits; i++)
  {
    split = xaccTransGetSplit (trans, i);
    affected_accounts[i+num_splits] = xaccSplitGetAccount (split);
  }
  affected_accounts[num_splits+more_splits] = NULL;

  xaccAccListDisplayRefresh (affected_accounts);
  free (affected_accounts);

  info->pending_trans = NULL;

  gnc_refresh_main_window ();
}

/* ======================================================== */

void 
xaccSRRedrawRegEntry (SplitRegister *reg) 
{
   Transaction *trans;

   trans = xaccSRGetCurrentTrans (reg);

   /* refresh the register windows */
   /* This split belongs to a transaction that might be displayed
    * in any number of windows. Changing any one split is likely
    * to affect any account windows associated with the other splits
    * in this transaction. So basically, send redraw events to all
    * of the splits. */
   gnc_transaction_ui_refresh(trans);
   gnc_refresh_main_window();
}

/* ======================================================== */
/* Copy from the register object to the engine */

gncBoolean
xaccSRSaveRegEntry (SplitRegister *reg, Transaction *new_trans)
{
   Account *account_refresh[5];
   SRInfo *info = xaccSRGetInfo(reg);
   Split *split;
   Transaction *trans;
   unsigned int changed;
   int style;
   int i;

   /* use the changed flag to avoid heavy-weight updates
    * of the split & transaction fields. This will help
    * cut down on uneccessary register redraws.  */
   changed = xaccSplitRegisterGetChangeFlag (reg);
   if (!changed)
     return GNC_F;

   /* HACK. This list will be used to refresh changed accounts.
    * The list is 5 long for: 2 accounts for xfrm, 2 accounts
    * for mxfrm, 1 account for NULL. This can go away once we
    * have engine change callbacks. */
   for (i = 0; i < 5; i++)
     account_refresh[i] = NULL;

   style = (reg->type) & REG_STYLE_MASK;   

   /* get the handle to the current split and transaction */
   split = xaccSRGetCurrentSplit (reg);
   trans = xaccSRGetCurrentTrans (reg);
   if (trans == NULL)
     return GNC_F;

   ENTER ("xaccSRSaveRegEntry(): save split is %p \n", split);

   /* determine whether we should commit the pending transaction */
   if (info->pending_trans != trans) {
     if (xaccTransIsOpen (info->pending_trans))
       xaccTransCommitEdit (info->pending_trans);
     xaccTransBeginEdit (trans, 0);
     info->pending_trans = trans;
   }

   /* If we are committing the blank split, add it to the account now */
   if (xaccTransGetSplit(trans, 0) == info->blank_split)
     xaccAccountInsertSplit (info->default_source_account, info->blank_split);

   if (split == NULL) {
      /* If we were asked to save data for a row for which there is no
       * associated split, then assume that this was a row that was
       * set aside for adding splits to an existing transaction.
       * xaccSRGetCurrent will handle this case, too. We will create
       * a new split, copy the row contents to that split, and append
       * the split to the pre-existing transaction. */

      split = xaccMallocSplit ();
      xaccTransAppendSplit (trans, split);

      if (force_double_entry_awareness)
        xaccAccountInsertSplit (info->default_source_account, split);

      assert (reg->table->current_cursor);
      reg->table->current_cursor->user_data = (void *) split;
   }

   DEBUG ("xaccSRSaveRegEntry(): updating trans addr=%p\n", trans);

   /* copy the contents from the cursor to the split */
   if (MOD_DATE & changed) {
      /* commit any pending changes */
      xaccCommitDateCell (reg->dateCell);
      DEBUG ("xaccSRSaveRegEntry(): MOD_DATE DMY= %2d/%2d/%4d \n",
                               reg->dateCell->date.tm_mday,
                               reg->dateCell->date.tm_mon+1,
                               reg->dateCell->date.tm_year+1900);

      xaccTransSetDate (trans, reg->dateCell->date.tm_mday,
                               reg->dateCell->date.tm_mon+1,
                               reg->dateCell->date.tm_year+1900);
   }

   if (MOD_NUM & changed) {
      DEBUG ("xaccSRSaveRegEntry(): MOD_NUM: %s\n", reg->numCell->cell.value);
      xaccTransSetNum (trans, reg->numCell->cell.value);
   }

   if (MOD_DESC & changed) {
      DEBUG ("xaccSRSaveRegEntry(): MOD_DESC: %s\n",
             reg->descCell->cell.value);
      xaccTransSetDescription (trans, reg->descCell->cell.value);
   }

   if (MOD_RECN & changed) {
      DEBUG ("xaccSRSaveRegEntry(): MOD_RECN: %c\n", reg->recnCell->value[0]);
      xaccSplitSetReconcile (split, reg->recnCell->value[0]);
   }

   if (MOD_ACTN & changed) {
      DEBUG ("xaccSRSaveRegEntry(): MOD_ACTN: %s\n",
             reg->actionCell->cell.value);
      xaccSplitSetAction (split, reg->actionCell->cell.value);
   }

   if (MOD_MEMO & changed) {
      DEBUG ("xaccSRSaveRegEntry(): MOD_MEMO: %s\n",
             reg->memoCell->cell.value);
      xaccSplitSetMemo (split, reg->memoCell->cell.value);
   }

   /* -------------------------------------------------------------- */
   /* OK, the handling of transfers gets complicated because it 
    * depends on what was displayed to the user.  For a multi-line
    * display, we just reparent the indicated split, its it,
    * and that's that.  For a two-line display, we want to reparent
    * the "other" split, but only if there is one ...
    * XFRM is the straight split, MXFRM is the mirrored split.
    * XTO is the straight split, too :) Only one should be in
    * a given cursor.
    */
   if ((MOD_XFRM | MOD_XTO) & changed) {
      Account *old_acc=NULL, *new_acc=NULL;
      char *new_name;

      if (MOD_XFRM & changed) {
        DEBUG ("xaccSRSaveRegEntry(): MOD_XFRM: %s\n",
               reg->xfrmCell->cell.value);
      }
      else {
        DEBUG ("xaccSRSaveRegEntry(): MOD_XTO: %s\n",
               reg->xtoCell->cell.value);
      }

      /* do some reparenting. Insertion into new account will automatically
       * delete this split from the old account */
      old_acc = xaccSplitGetAccount (split);

      if (MOD_XFRM & changed)
        new_name = reg->xfrmCell->cell.value;
      else
        new_name = reg->xtoCell->cell.value;

      new_acc = xaccGetAccountByFullName (trans, new_name, account_separator);

      if ((new_acc != NULL) && (old_acc != new_acc))
      {
        char *currency = NULL;
        char *security = NULL;

        currency = xaccAccountGetCurrency(new_acc);
        currency = xaccTransIsCommonCurrency(trans, currency);

        if (currency == NULL) {
          security = xaccAccountGetSecurity(new_acc);
          security = xaccTransIsCommonCurrency(trans, security);
        }

        if ((currency != NULL) || (security != NULL)) {
          xaccAccountInsertSplit (new_acc, split);

          /* HACK. */
          account_refresh[0] = old_acc;
          account_refresh[1] = new_acc;
        }
        else {
          char *message = NULL;

          asprintf(&message, REG_CURR_MSG, xaccAccountGetName(new_acc));
          assert(message != NULL);

          gnc_warning_dialog_parented(xaccSRGetParent(reg), message);
          free(message);
        }
      }
   }

   if (MOD_MXFRM & changed) {
      Split *other_split = NULL;

      DEBUG ("xaccSRSaveRegEntry(): MOD_MXFRM: %s\n",
             reg->mxfrmCell->cell.value);

      other_split = xaccGetOtherSplit(split);

      /* other_split may be null for two very different reasons:
       * (1) the parent transaction has three or more splits in it,
       *     and so the "other" split is ambiguous, and thus null.
       * (2) the parent transaction has only this one split as a child.
       *     and "other" is null because there is no other.
       *
       * In the case (2), we want to create the other split, so that 
       * the user's request to transfer actually works out.
       */

      if (!other_split) {
         other_split = xaccTransGetSplit (trans, 1);
         if (!other_split) {
            double  amt = xaccSplitGetShareAmount (split);
            double  prc = xaccSplitGetSharePrice (split);

            other_split = xaccMallocSplit ();

            xaccSplitSetMemo (other_split, xaccSplitGetMemo (split));
            xaccSplitSetAction (other_split, xaccSplitGetAction (split));
            xaccSplitSetSharePriceAndAmount (other_split, prc, -amt);

            xaccTransAppendSplit (trans, other_split);
         }
      }

      if (other_split) {
         Account *old_acc=NULL, *new_acc=NULL;

         /* do some reparenting. Insertion into new account will automatically
          * delete from the old account */
         old_acc = xaccSplitGetAccount (other_split);
         new_acc = xaccGetAccountByFullName (trans, reg->mxfrmCell->cell.value,
                                             account_separator);

         if ((new_acc != NULL) && (old_acc != new_acc))
         {
           char *currency = NULL;
           char *security = NULL;

           currency = xaccAccountGetCurrency(new_acc);
           currency = xaccTransIsCommonCurrency(trans, currency);

           if (currency == NULL) {
             security = xaccAccountGetSecurity(new_acc);
             security = xaccTransIsCommonCurrency(trans, security);
           }

           if ((currency != NULL) || (security != NULL)) {
             xaccAccountInsertSplit (new_acc, other_split);

             /* HACK. */
             if (account_refresh[0] == NULL) {
               account_refresh[0] = old_acc;
               account_refresh[1] = new_acc;
             }
             else {
               account_refresh[2] = old_acc;
               account_refresh[3] = new_acc;
             }
           }
           else {
             char *message = NULL;

             asprintf(&message, REG_CURR_MSG, xaccAccountGetName(new_acc));
             assert(message != NULL);

             gnc_warning_dialog_parented(xaccSRGetParent(reg), message);
             free(message);
           }
         }
      }
   }

   if (((MOD_AMNT | MOD_PRIC | MOD_VALU) & changed) &&
       ((STOCK_REGISTER    == (reg->type & REG_TYPE_MASK)) ||
	(CURRENCY_REGISTER == (reg->type & REG_TYPE_MASK)) ||
	(PORTFOLIO_LEDGER  == (reg->type & REG_TYPE_MASK)))) {

     double value;
     double price;
     double new_amount;

     if (MOD_VALU & changed)
       value = xaccGetPriceCellValue(reg->valueCell);
     else
       value = xaccSplitGetValue(split);

     if (MOD_PRIC & changed)
       price = xaccGetPriceCellValue(reg->priceCell);
     else
       price = xaccSplitGetSharePrice(split);

      if (MOD_AMNT & changed) {
        double credit = xaccGetPriceCellValue(reg->creditCell);
        double debit  = xaccGetPriceCellValue(reg->debitCell);
        new_amount = credit - debit;
      }
      else
        new_amount = xaccSplitGetShareAmount(split);

     if (value != price*new_amount) {
       int i;
       int choice;
       int default_value;
       char *radio_list[4] = { NULL, NULL, NULL, NULL };

       if (MOD_AMNT & changed)
         asprintf(&radio_list[0], "%s (%s)", AMT_STR, CHANGED_STR);
       else
         radio_list[0] = strdup(AMT_STR);

       if (MOD_PRIC & changed)
         asprintf(&radio_list[1], "%s (%s)", PRICE_STR, CHANGED_STR);
       else
         radio_list[1] = strdup(PRICE_STR);

       if (MOD_VALU & changed)
         asprintf(&radio_list[2], "%s (%s)", VALUE_STR, CHANGED_STR);
       else
         radio_list[2] = strdup(VALUE_STR);

       for (i = 0; i < 3; i++)
         assert(radio_list[i] != NULL);

       if (!(MOD_AMNT & changed))
         default_value = 0;
       else if (!(MOD_PRIC & changed))
         default_value = 1;
       else if (!(MOD_VALU & changed))
         default_value = 2;
       else
         default_value = 0;

       choice = gnc_choose_radio_option_dialog_parented(xaccSRGetParent(reg),
                                                        TRANS_RECALC_TITLE,
                                                        TRANS_RECALC_MSG,
                                                        default_value,
                                                        radio_list);

       for (i = 0; i < 3; i++)
         free(radio_list[i]);

       switch(choice)
       {
         case 0: /* Modify number of shares */
           if (price == 0)
             break;

	   new_amount = value/price;

           xaccSetDebCredCellValue (reg->debitCell,
                                    reg->creditCell, new_amount);
	   changed |= MOD_AMNT;
	   break;
         case 1: /* Modify the share price */
           if (new_amount == 0)
             break;

	   price = value/new_amount;

	   if (price < 0) {
	     price = -price;
	     xaccSetPriceCellValue(reg->valueCell, -value);  
	     changed |= MOD_VALU;
	   }
	   xaccSetPriceCellValue(reg->priceCell, price);
	   changed |= MOD_PRIC;
	   break;
         case 2: /* Modify total value */
	   value = price*new_amount;

	   xaccSetPriceCellValue(reg->valueCell, value);
	   changed |= MOD_VALU;
	   break;
         default:
	   break;
       }
     }
   }

   /* The AMNT and NAMNT updates only differ by sign. Basically, 
    * the split cursors show minus the quants that the single,
    * double and transaction cursors show, and so when updates
    * happen, the extra minus sign must also be handled. */
   if ((MOD_AMNT | MOD_NAMNT) & changed) {
      double new_amount;
      double credit;
      double debit;

      if (MOD_AMNT & changed) {
         credit = xaccGetPriceCellValue(reg->creditCell);
         debit  = xaccGetPriceCellValue(reg->debitCell);
         new_amount = credit - debit;
      } else {
         credit = xaccGetPriceCellValue(reg->ncreditCell);
         debit  = xaccGetPriceCellValue(reg->ndebitCell);
         new_amount = debit - credit;
      }

      DEBUG ("xaccSRSaveRegEntry(): MOD_AMNT: %f\n", new_amount);

      if ((EQUITY_REGISTER   == (reg->type & REG_TYPE_MASK)) ||
          (STOCK_REGISTER    == (reg->type & REG_TYPE_MASK)) ||
          (CURRENCY_REGISTER == (reg->type & REG_TYPE_MASK)) ||
          (PORTFOLIO_LEDGER  == (reg->type & REG_TYPE_MASK)))
        xaccSplitSetShareAmount (split, new_amount);
      else
        xaccSplitSetValue (split, new_amount);
   }

   if (MOD_PRIC & changed) {
      double price;

      price = xaccGetPriceCellValue(reg->priceCell);

      DEBUG ("xaccSRSaveRegEntry(): MOD_PRIC: %f\n", price);

      xaccSplitSetSharePrice (split, price);
   }

   if (MOD_VALU & changed) {
      double value = xaccGetPriceCellValue(reg->valueCell);

      DEBUG ("xaccSRSaveRegEntry(): MOD_VALU: %f\n", value);

      xaccSplitSetValue (split, value);
   }

   PINFO ("xaccSRSaveRegEntry(): finished saving split %s of trans %s \n", 
          xaccSplitGetMemo(split), xaccTransGetDescription(trans));

   /* If the modified split is the "blank split", then it is now an
    * official part of the account. Set the blank split to NULL, so
    * we can be sure of getting a new split. Also, save the date for
    * the new blank split. */
   split = xaccTransGetSplit (trans, 0);
   if (split == info->blank_split)
   {
     info->blank_split = NULL;
     info->last_date_entered = xaccTransGetDate(trans);
   }

   /* If the new transaction is different from the current,
    * commit the current and set the pending transaction to NULL. */
   if (trans != new_trans) {
     xaccTransCommitEdit (trans);
     info->pending_trans = NULL;
   }

   xaccSplitRegisterClearChangeFlag(reg);

   if (account_refresh[0] != NULL) {
     gnc_account_list_ui_refresh(account_refresh);
     gnc_refresh_main_window();
   }

   return GNC_T;
}

/* ======================================================== */

static void
xaccSRLoadRegEntry (SplitRegister *reg, Split *split)
{
   SRInfo *info = xaccSRGetInfo(reg);
   int typo = reg->type & REG_TYPE_MASK;
   char buff[2];
   double baln;

   /* don't even bother doing a load if there is no current cursor */
   if (!(reg->table->current_cursor)) return;

   ENTER ("SRLoadTransEntry(): s=%p\n", split);

   if (!split) {
      /* we interpret a NULL split as a blank split */
      xaccSetDateCellValueSecs (reg->dateCell, 0);
      xaccSetNumCellValue (reg->numCell, "");
      xaccSetQuickFillCellValue (reg->descCell, "");
      xaccSetBasicCellValue (reg->recnCell, "");
      xaccSetPriceCellValue  (reg->shrsCell,  0.0);
      xaccSetPriceCellValue (reg->balanceCell, 0.0);

      xaccSetComboCellValue (reg->actionCell, "");
      xaccSetQuickFillCellValue (reg->memoCell, "");
      xaccSetComboCellValue (reg->xfrmCell, "");
      xaccSetComboCellValue (reg->mxfrmCell, "");
      xaccSetComboCellValue (reg->xtoCell, "");
      xaccSetDebCredCellValue (reg->debitCell, 
                               reg->creditCell, 0.0);
      xaccSetDebCredCellValue (reg->ndebitCell, 
                               reg->ncreditCell, 0.0);
      xaccSetPriceCellValue (reg->priceCell, 0.0);
      xaccSetPriceCellValue (reg->valueCell, 0.0);

   } else {
      long long secs;
      double amt;
      char * accname=NULL;
      Transaction *trans = xaccSplitGetParent (split);

      secs = xaccTransGetDateL (trans);
      xaccSetDateCellValueSecsL (reg->dateCell, secs);

      xaccSetNumCellValue (reg->numCell, xaccTransGetNum (trans));
      xaccSetQuickFillCellValue (reg->descCell,
                                 xaccTransGetDescription (trans));

      buff[0] = xaccSplitGetReconcile (split);
      buff[1] = 0x0;
      xaccSetBasicCellValue (reg->recnCell, buff);

      /* If the reverse_balance callback is present use that.
       * Otherwise, reverse income and expense by default. */
      baln = xaccSplitGetBalance (split);
      if (reverse_balance != NULL) {
        Account *account;

        account = xaccSplitGetAccount(split);
        if (account == NULL)
          account = info->default_source_account;

        if (reverse_balance(account))
          baln = -baln;
      }
      else if ((INCOME_REGISTER == typo) ||
               (EXPENSE_REGISTER == typo)) { 
         baln = -baln;
      }
      xaccSetPriceCellValue (reg->balanceCell, baln);

      xaccSetPriceCellValue (reg->shrsCell, xaccSplitGetShareBalance (split));

      xaccSetComboCellValue (reg->actionCell, xaccSplitGetAction (split));

      /* Show the transfer-from account name.                            
       * What gets displayed depends on the display format.                
       * For a multi-line display, show the account for each member split.  
       * For a one or two-line display, show the other account, but only    
       * if there are exactly two splits.
       *
       * xfrm is the "straight" display, "mxfrm" is the "mirrored" display.
       * xto is the "transfer to" display in single or double mode, or
       * on the transaction cursor in an expanded mode. If we have a
       * default source account, auto-fill the xto field with it.
       */
      accname = xaccAccountGetFullName (xaccSplitGetAccount (split),
                                        account_separator);
      xaccSetComboCellValue (reg->xfrmCell, accname);
      if ((safe_strcmp(accname, "") == 0) &&
          (info->default_source_account != NULL)) {
        char * xtoname;

        xtoname = xaccAccountGetFullName(info->default_source_account,
                                         account_separator);
        xaccSetComboCellValue (reg->xtoCell, xtoname);
        free(xtoname);
      }
      else
        xaccSetComboCellValue (reg->xtoCell, accname);
      free(accname);

      {
         Split *s = xaccGetOtherSplit (split);
         gncBoolean need_to_free = GNC_F;

         if (s) {
            accname = xaccAccountGetFullName (xaccSplitGetAccount (s),
                                              account_separator);
            need_to_free = GNC_T;
         } else {
            /* determine whether s is null because threre are three
             * or more splits, or whether there is only one ... */
            s = xaccTransGetSplit (xaccSplitGetParent(split), 1);
            if (s) {
               accname = SPLIT_STR;   /* three or more .. */
            } else {
               accname = "";          /* none ... */
            }
         }
         xaccSetComboCellValue (reg->mxfrmCell, accname);
         if (need_to_free)
           free(accname);
      }

      xaccSetQuickFillCellValue (reg->memoCell, xaccSplitGetMemo (split));

      buff[0] = xaccSplitGetReconcile (split);
      buff[1] = 0x0;
      xaccSetBasicCellValue (reg->recnCell, buff);

      if ((EQUITY_REGISTER   == typo) ||
          (STOCK_REGISTER    == typo) ||
          (CURRENCY_REGISTER == typo) ||
          (PORTFOLIO_LEDGER  == typo)) 
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
   xaccCommitCursor (reg->table);

   LEAVE("SRLoadTransEntry()\n");
}

/* ======================================================== */

static void
xaccSRCountRows (SplitRegister *reg, Split **slist,
                 Transaction *find_trans, Split *find_split,
                 gncBoolean *ext_found_trans,
                 gncBoolean *ext_found_split)
{
   SRInfo *info = xaccSRGetInfo(reg);
   CellBlock *lead_cursor;
   Split *split = NULL;
   Locator *locator;
   Table *table;

   gncBoolean found_split = GNC_F;
   gncBoolean found_trans = GNC_F;
   gncBoolean multi_line;
   gncBoolean dynamic;

   int save_cursor_phys_row;
   int save_cursor_virt_row;
   int save_cell_row;
   int num_phys_rows;
   int num_virt_rows;
   int style;
   int i;

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

   /* save the current cursor location; if we can't find the
    * requested transaction/split pair, we restore the 
    * cursor to this location when we are done. */
   save_cursor_phys_row = reg->cursor_phys_row;
   save_cursor_virt_row = reg->cursor_virt_row;

   /* save the current cell row offset */
   locator = table->locators[reg->table->current_cursor_phys_row]
                            [reg->table->current_cursor_phys_col];
   save_cell_row = locator->phys_row_offset;
   if (save_cell_row < 0)
     save_cell_row = 0;

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
      if (split != info->blank_split) {
         Transaction *trans;
         gncBoolean do_expand;

         /* lets determine where to locate the cursor ... */
         if (!found_split) {
           /* Check to see if we find a perfect match */
           if (split == find_split) {
             save_cursor_phys_row = num_phys_rows;
             save_cursor_virt_row = num_virt_rows;
             found_split = GNC_T;
             found_trans = GNC_T;
           }
           /* Otherwise, check for a close match. This could happen
            * if we are collapsing from multi-line to single, e.g. */
           else if (xaccSplitGetParent(split) == find_trans) {
             save_cursor_phys_row = num_phys_rows;
             save_cursor_virt_row = num_virt_rows;
             found_trans = GNC_T;
           }
         }

         /* if multi-line, then show all splits. If dynamic then
          * show all splits only if this is the hot split. */
         do_expand = multi_line;
         do_expand = do_expand ||
                     (dynamic && xaccIsPeerSplit(split, find_split)); 
         if (dynamic && (NULL == find_split)) {
            trans = xaccSplitGetParent (split);
            do_expand = do_expand || (trans == find_trans);
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
                 if (!found_split) {
                   /* Check if we find a perfect match. We have to check
                    * the transaction in case the split is NULL (blank). */
                   if ((secondary == find_split) &&
                       (trans == find_trans)) {
                     save_cursor_phys_row = num_phys_rows;
                     save_cursor_virt_row = num_virt_rows;
                     found_split = GNC_T;
                     found_trans = GNC_T;
                   }
                 }

                 num_virt_rows ++;
                 num_phys_rows += reg->split_cursor->numRows; 
               }
               j++;
            } while (secondary);
         } else {
           /* Try to get as close as possible to the original cell row. */
           if (found_split && (split == find_split) &&
               (save_cell_row < lead_cursor->numRows))
             save_cursor_phys_row += save_cell_row;

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
   if (info->blank_split != NULL) {
      /* lets determine where to locate the cursor ... */
      if (!found_split && info->blank_split == find_split) {
         save_cursor_phys_row = num_phys_rows;
         save_cursor_virt_row = num_virt_rows;
         found_split = GNC_T;
         found_trans = GNC_T;
      }
      else if (!found_split &&
               (xaccSplitGetParent(info->blank_split) == find_trans)) {
         save_cursor_phys_row = num_phys_rows;
         save_cursor_virt_row = num_virt_rows;
         found_trans = GNC_T;
      }
   }

   if (multi_line) {
      if (!found_split && (find_split == NULL) &&
          (xaccSplitGetParent(info->blank_split) == find_trans)) {
        save_cursor_phys_row = num_phys_rows + 1;
        save_cursor_virt_row = num_virt_rows + 1;
        found_split = GNC_T;
        found_trans = GNC_T;
      }
      num_virt_rows += 2;
      num_phys_rows += reg->trans_cursor->numRows;
      num_phys_rows += reg->split_cursor->numRows;
   } else {
      num_virt_rows += 1;
      num_phys_rows += lead_cursor->numRows;
   }

   /* check to make sure we got a good cursor position */
   if ((num_phys_rows <= save_cursor_phys_row) ||
       (num_virt_rows <= save_cursor_virt_row)) {
     save_cursor_phys_row = num_phys_rows - reg->split_cursor->numRows;
     save_cursor_virt_row = num_virt_rows;
   }

   if ((save_cursor_phys_row < (reg->header->numRows)) ||
       (save_cursor_virt_row < 1)) {
     save_cursor_phys_row = reg->header->numRows;
     save_cursor_virt_row = 1;
   }

   /* finally, record the values */
   reg->num_phys_rows = num_phys_rows;
   reg->num_virt_rows = num_virt_rows;
   reg->cursor_phys_row = save_cursor_phys_row;
   reg->cursor_virt_row = save_cursor_virt_row;

   if (ext_found_split != NULL)
     *ext_found_split = found_split;
   if (ext_found_trans != NULL)
     *ext_found_trans = found_trans;
}

/* ======================================================== */

void
xaccSRLoadRegister (SplitRegister *reg, Split **slist, 
                    Account *default_source_acc)
{
   SRInfo *info = xaccSRGetInfo(reg);
   SplitRegisterBuffer *reg_buffer;
   CellBlock *lead_cursor;
   Transaction *find_trans;
   Split *last_split = NULL;
   Split *find_split;
   Split *split;
   Table *table;

   gncBoolean found_pending = GNC_F;
   gncBoolean found_split = GNC_F;
   gncBoolean found_trans = GNC_F;
   gncBoolean multi_line;
   gncBoolean dynamic;

   unsigned int changed;
   int save_phys_col;
   int type, style;
   int phys_row;
   int vrow;
   int i;

   xaccSplitRegisterConfigColors (reg);

   info->default_source_account = default_source_acc;

   table = reg->table;
   type  = (reg->type) & REG_TYPE_MASK;
   style = (reg->type) & REG_STYLE_MASK;
   multi_line  = (REG_MULTI_LINE == style);
   dynamic = ((REG_SINGLE_DYNAMIC == style) || (REG_DOUBLE_DYNAMIC == style));
   if ((REG_SINGLE_LINE == style) ||
       (REG_SINGLE_DYNAMIC == style)) {
      lead_cursor = reg->single_cursor;
   } else {
      lead_cursor = reg->double_cursor;
   }

   /* figure out where we are going to. */
   if (info->cursor_hint_trans != NULL) {
     find_trans = info->cursor_hint_trans;
     find_split = info->cursor_hint_split;
     save_phys_col = info->cursor_hint_phys_col;
   }
   else {
     find_trans = xaccSRGetCurrentTrans (reg);
     find_split = xaccSRGetCurrentSplit (reg);
     save_phys_col = table->current_cursor_phys_col;
   }

   /* paranoia */
   if (save_phys_col < 0)
     save_phys_col = 0;
   if (save_phys_col >= table->num_phys_cols)
     save_phys_col = table->num_phys_cols - 1;

   /* count the number of rows, looking for the place we want to go. */
   xaccSRCountRows (reg, slist,
                    find_trans, find_split,
                    &found_trans, &found_split);

   /* If the current cursor has changed, and the 'current split'
    * is still among the living, we save the values for later
    * restoration. */
   changed = xaccSplitRegisterGetChangeFlag(reg);
   if (found_split && changed && (find_split == xaccSRGetCurrentSplit(reg)))
   {
     reg_buffer = xaccMallocSplitRegisterBuffer();
     xaccSplitRegisterSaveCursor(reg, reg_buffer);
   }
   else
     reg_buffer = NULL;

   /* disable move callback -- we don't want the cascade of 
    * callbacks while we are fiddling with loading the register */
   table->move_cursor = NULL;
   xaccMoveCursorGUI (table, -1, -1);

   /* resize the table to the sizes we just counted above */
   /* num_virt_cols is always one. */
   xaccSetTableSize (table, reg->num_phys_rows, reg->num_cols, 
                            reg->num_virt_rows, 1);

   /* make sure that the header is loaded */
   xaccSetCursor (table, reg->header, 0, 0, 0, 0);

   PINFO ("xaccSRLoadRegister(): "
          "load register of %d phys rows ----------- \n", reg->num_phys_rows);

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

     if (info->pending_trans == xaccSplitGetParent (split))
       found_pending = GNC_T;

      /* do not load the blank split */
      if (split != info->blank_split) {
         Transaction *trans;
         gncBoolean do_expand;

         PINFO ("xaccSRLoadRegister(): "
                "load trans %d at phys row %d \n", i, phys_row);

         /* if multi-line, then show all splits. If dynamic then
          * show all splits only if this is the hot split. */
         do_expand = multi_line;
         do_expand = do_expand || 
                     (dynamic && xaccIsPeerSplit(split, find_split)); 
         if (dynamic && (NULL == find_split)) {
            trans = xaccSplitGetParent (split);
            do_expand = do_expand || (trans == find_trans);
         }
         if (dynamic && !found_trans && (vrow == reg->cursor_virt_row)) {
           reg->cursor_phys_row = phys_row;
           do_expand = GNC_T;
         }

         if (do_expand) 
         {
            Split * secondary;
            int j = 0;

            xaccSetCursor (table, reg->trans_cursor, phys_row, 0, vrow, 0);
            xaccMoveCursor (table, phys_row, 0);
            xaccSRLoadRegEntry (reg, split);
            vrow ++;
            phys_row += reg->trans_cursor->numRows; 

            /* loop over all of the splits in the transaction. The
             * do..while will automatically put a blank (null) split
             * at the end. */
            trans = xaccSplitGetParent (split);
            j = 0;
            do {
               secondary = xaccTransGetSplit (trans, j);

               if (secondary != split) {
                  xaccSetCursor (table, reg->split_cursor,
                                 phys_row, 0, vrow, 0);
                  xaccMoveCursor (table, phys_row, 0);
                  xaccSRLoadRegEntry (reg, secondary);
                  PINFO ("xaccSRLoadRegister(): "
                         "load split %d at phys row %d addr=%p \n", 
                          j, phys_row, secondary);
                  vrow ++;
                  phys_row += reg->split_cursor->numRows; 
               }

               j++;
            } while (secondary);

         } else {
            /* the simple case ... */
            xaccSetCursor (table, lead_cursor, phys_row, 0, vrow, 0);
            xaccMoveCursor (table, phys_row, 0);
            xaccSRLoadRegEntry (reg, split);
            vrow ++;
            phys_row += lead_cursor->numRows; 
         }
      }
      else {
        PINFO ("xaccSRLoadRegister(): skip trans %d (blank split) \n", i);
      }

      last_split = split;
      i++; 
      split = slist[i];
   }

   /* add the "blank split" at the end. We use either the blank
    * split or we create a new one, as needed. */
   if (info->blank_split != NULL) {
      split = info->blank_split;
      if (info->pending_trans == xaccSplitGetParent(split))
        found_pending = GNC_T;
   } else {
      Transaction *trans;

      trans = xaccMallocTransaction ();

      xaccTransBeginEdit (trans, 1);
      xaccTransSetDateSecs(trans, info->last_date_entered);
      xaccTransCommitEdit (trans);

      split = xaccTransGetSplit (trans, 0);
      info->blank_split = split;

      reg->destroy = LedgerDestroy;
   }

   /* do the split row of the blank split */
   if (multi_line) {
      Transaction *trans;

      /* do the transaction row of the blank split */
      xaccSetCursor (table, reg->trans_cursor, phys_row, 0, vrow, 0);
      xaccMoveCursor (table, phys_row, 0);
      xaccSRLoadRegEntry (reg, split);
      vrow ++;
      phys_row += reg->trans_cursor->numRows; 

      trans = xaccSplitGetParent (split);
      split = xaccTransGetSplit (trans, 1);
      xaccSetCursor (table, reg->split_cursor, phys_row, 0, vrow, 0);
      xaccMoveCursor (table, phys_row, 0);
      xaccSRLoadRegEntry (reg, split);
      vrow ++;
      phys_row += reg->split_cursor->numRows; 
   } else {
      xaccSetCursor (table, lead_cursor, phys_row, 0, vrow, 0);
      xaccMoveCursor (table, phys_row, 0);
      xaccSRLoadRegEntry (reg, split);
      vrow ++;
      phys_row += lead_cursor->numRows; 
   }

   /* restore the cursor to its rightful position */
   {
     int row = reg->cursor_phys_row;
     int col = save_phys_col;

     if (gnc_table_find_valid_cell_horiz(table, &row, &col, GNC_F))
     {
       xaccMoveCursorGUI(table, row, col);
       reg->cursor_phys_row = row;

       if (reg_buffer != NULL)
       {
         xaccSplitRegisterRestoreCursorChanged(reg, reg_buffer);
         xaccCommitCursor (table);
       }
     }

     if (reg_buffer != NULL)
     {
       xaccDestroySplitRegisterBuffer(reg_buffer);
       reg_buffer = NULL;
     }
   }

   /* If we didn't find the pending transaction, it was removed
    * from the account. It might not even exist any more.
    * Make sure we don't access it. */
   if (!found_pending)
     info->pending_trans = NULL;

   /* clear out the hint transaction and split. We want
    * to know if it has been set from the move callback. */
   info->cursor_hint_trans = NULL;
   info->cursor_hint_split = NULL;

   xaccRefreshTableGUI (table);

   /* set the completion character for the xfer cells */
   xaccComboCellSetCompleteChar (reg->mxfrmCell, account_separator);
   xaccComboCellSetCompleteChar (reg->xfrmCell, account_separator);

   /* enable callback for cursor user-driven moves */
   table->move_cursor = LedgerMoveCursor;
   table->traverse = LedgerTraverse;
   table->set_help = LedgerSetHelp;
   table->client_data = (void *) reg;
}

/* ======================================================== */
/* walk account tree recursively, pulling out all the names */

static void 
LoadXferCell (ComboCell *cell,  
              AccountGroup *grp,
              char *base_currency, char *base_security)
{
  gncBoolean load_everything;
  Account * acc;
  char *name;
  int n;

  ENTER ("LoadXferCell()\n");

  if (!grp) return;

  load_everything = ((base_security == NULL) && (base_security == NULL));

  /* Build the xfer menu out of account names.
   * Traverse sub-accounts recursively.
   * Valid transfers can occur only between accounts
   * with the same base currency.
   */
  n = 0;
  acc = xaccGroupGetAccount (grp, n);
  while (acc) {
    char *curr, *secu;

    curr = xaccAccountGetCurrency (acc);
    secu = xaccAccountGetSecurity (acc);
    if (secu && (0x0 == secu[0])) secu = 0x0;

    DEBUG ("LoadXferCell(): curr=%s secu=%s acct=%s\n", 
           curr, secu, xaccAccountGetName (acc));

    if ( load_everything || 
         (!safe_strcmp(curr,base_currency)) ||
         (!safe_strcmp(curr,base_security)) ||
         (secu && (!safe_strcmp(secu,base_currency))) ||
         (secu && (!safe_strcmp(secu,base_security))) )
    {
      name = xaccAccountGetFullName (acc, account_separator);
      if (name != NULL)
      {
        xaccAddComboCellMenuItem (cell, name);
        free(name);
      }
    }
    LoadXferCell (cell, xaccAccountGetChildren (acc), 
                  base_currency, base_security);
    n++;
    acc = xaccGroupGetAccount (grp, n);
  }

  LEAVE ("LoadXferCell()\n");
}

/* ======================================================== */

static void
xaccLoadXferCell (ComboCell *cell,  
                  AccountGroup *grp, 
                  Account *base_account)
{
  char *curr, *secu;

  curr = xaccAccountGetCurrency (base_account);
  secu = xaccAccountGetSecurity (base_account);

  if ((secu != NULL) && (secu[0] = 0))
    secu = NULL;

  xaccClearComboCellMenu (cell);
  xaccAddComboCellMenuItem (cell, "");
  LoadXferCell (cell, grp, curr, secu);
}

/* ======================================================== */

void
xaccSRLoadXferCells (SplitRegister *reg, Account *base_account)
{
  AccountGroup *group;

  group = xaccGetAccountRoot(base_account);
  if (group == NULL)
    group = gncGetCurrentGroup();

  if (group == NULL)
    return;

  xaccLoadXferCell(reg->xfrmCell, group, base_account);
  xaccLoadXferCell(reg->mxfrmCell, group, base_account);
  xaccLoadXferCell(reg->xtoCell, group, base_account);
}

/* =======================  end of file =================== */
