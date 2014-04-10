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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

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
 * Copyright (c) 1998-2000 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 */

#define _GNU_SOURCE

#include <config.h>

#include <stdio.h>
#include <time.h>
#include <glib.h>
#include <guile/gh.h>

#include "top-level.h"

#include "ui-callbacks.h"
#include "SplitLedger.h"
#include "MultiLedger.h"
#include "FileDialog.h"
#include "Refresh.h"
#include "splitreg.h"
#include "table-allgui.h"
#include "guile-util.h"
#include "messages.h"
#include "util.h"


typedef struct _SRInfo SRInfo;
struct _SRInfo
{
  /* The blank split at the bottom of the register */
  GUID blank_split_guid;

  /* The currently open transaction, if any */
  GUID pending_trans_guid;

  /* A transaction used to remember where to put the cursor */
  Transaction *cursor_hint_trans;

  /* A split used to remember where to put the cursor */
  Split *cursor_hint_split;

  /* A split used to remember where to put the cursor */
  Split *cursor_hint_trans_split;

  /* A column used to remember which column to put the cursor */
  int cursor_hint_phys_col;

  /* If the hints were set by the traverse callback */
  gboolean hint_set_by_traverse;

  /* A flag indicating if the last traversal was 'exact'.
   * See table-allgui.[ch] for details. */
  gboolean exact_traversal;

  /* The default account where new splits are added */
  Account *default_source_account;

  /* The last date recorded in the blank split */
  time_t last_date_entered;

  /* true if the current blank split has been edited and commited */
  gboolean blank_split_edited;

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

/* The copied split or transaction, if any */
static CursorType copied_type = CURSOR_NONE;
static SCM copied_item = SCM_UNDEFINED;
static GUID copied_leader_guid;


/** static prototypes *****************************************************/

static Split * xaccSRGetTransSplit (SplitRegister *reg,
                                    int phys_row, int phys_col);
static void xaccSRLoadRegEntry (SplitRegister *reg, Split *split);
static gboolean xaccSRSaveRegEntryToSCM (SplitRegister *reg,
                                         SCM trans_scm, SCM split_scm);
static Transaction * xaccSRGetTrans (SplitRegister *reg,
                                     int phys_row, int phys_col);
static Split * xaccSRGetCurrentTransSplit (SplitRegister *reg);
static GList * xaccSRSaveChangedCells (SplitRegister *reg, Transaction *trans,
                                       Split *split);


/** implementations *******************************************************/

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

  info = g_new0(SRInfo, 1);

  info->last_date_entered = time(NULL);

  reg->user_data = info;
}

static void
xaccSRDestroyRegisterData(SplitRegister *reg)
{
  if (reg == NULL)
    return;

  g_free(reg->user_data);

  reg->user_data = NULL;
}

static SRInfo *
xaccSRGetInfo(SplitRegister *reg)
{
  assert(reg != NULL);

  if (reg->user_data == NULL)
    xaccSRInitRegisterData(reg);

  return reg->user_data;
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


static GList *
gnc_trans_prepend_account_list(Transaction *trans, GList *accounts)
{
  Account *account;
  Split *split;
  int i = 0;

  if (trans == NULL)
    return accounts;

  do
  {
    split = xaccTransGetSplit(trans, i);

    if (split == NULL)
      return accounts;

    account = xaccSplitGetAccount(split);
    if (account != NULL)
      accounts = g_list_prepend(accounts, account);

    i++;
  } while(1);
}

static GList *
gnc_trans_prepend_split_list(Transaction *trans, GList *splits)
{
  Split *split;
  int i = 0;

  if (trans == NULL)
    return splits;

  do
  {
    split = xaccTransGetSplit(trans, i);

    if (split == NULL)
      return splits;

    splits = g_list_prepend(splits, split);

    i++;
  } while(1);
}

static int
gnc_trans_split_index(Transaction *trans, Split *split)
{
  Split *s;
  int i = 0;

  do
  {
    s = xaccTransGetSplit(trans, i);

    if (s == split)
      return i;

    if (s == NULL)
      return -1;

    i++;
  } while(1);
}

/* Uses the scheme split copying routines */
static void
gnc_copy_split_onto_split(Split *from, Split *to)
{
  SCM split_scm;

  if ((from == NULL) || (to == NULL))
    return;

  split_scm = gnc_copy_split(from);
  if (split_scm == SCM_UNDEFINED)
    return;

  gnc_copy_split_scm_onto_split(split_scm, to);
}

/* Uses the scheme transaction copying routines */
static void
gnc_copy_trans_onto_trans(Transaction *from, Transaction *to,
                          gboolean do_commit)
{
  SCM trans_scm;

  if ((from == NULL) || (to == NULL))
    return;

  trans_scm = gnc_copy_trans(from);
  if (trans_scm == SCM_UNDEFINED)
    return;

  gnc_copy_trans_scm_onto_trans(trans_scm, to, do_commit);
}

static Split *
gnc_find_split_in_trans_by_memo(Transaction *trans, const char *memo,
                                Transaction *dest_trans, gboolean unit_price)
{
  int num_splits = xaccTransCountSplits(trans);
  int i;

  for (i = num_splits - 1; i >= 0; i--)
  {
    Split *split = xaccTransGetSplit(trans, i);

    if (unit_price && (xaccSplitGetSharePrice(split) != 1.0))
      continue;

    if (safe_strcmp(memo, xaccSplitGetMemo(split)) == 0)
    {
      Account *account = xaccSplitGetAccount(split);
      const char *currency, *security;

      if (account == NULL)
        return split;

      currency = xaccAccountGetCurrency(account);
      if (xaccTransIsCommonCurrency(dest_trans, currency))
        return split;

      security = xaccAccountGetSecurity(account);
      if (xaccTransIsCommonCurrency(dest_trans, security))
        return split;
    }
  }

  return NULL;
}

/* This routine is for finding a matching split in an account or related
 * accounts by matching on the memo field. This routine is used for auto-
 * filling in registers with a default leading account. The dest_trans
 * is a transaction used for currency checking. */
static Split *
gnc_find_split_in_account_by_memo(Account *account, const char *memo,
                                  Transaction *dest_trans, gboolean unit_price)
{
  Split **splits;
  Split **orig;

  if (account == NULL)
    return NULL;

  splits = xaccAccountGetSplitList(account);
  if ((splits == NULL) || (*splits == NULL))
    return NULL;

  orig = splits;

  while (*splits != NULL)
    splits++;

  do
  {
    Transaction *trans;
    Split *split;

    splits--;

    trans = xaccSplitGetParent(*splits);

    split = gnc_find_split_in_trans_by_memo(trans, memo, dest_trans,
                                            unit_price);
    if (split != NULL)
      return split;

  } while (splits != orig);

  return NULL;
}

/* This routine is for finding a matching transaction in an account by
 * matching on the description field. This routine is used for auto-filling
 * in registers with a default leading account. The dest_trans is a
 * transaction used for currency checking. */
static Transaction *
gnc_find_trans_in_account_by_desc(Account *account, const char *description)
{
  Split **splits;
  Split **orig;

  if (account == NULL)
    return NULL;

  splits = xaccAccountGetSplitList(account);
  if ((splits == NULL) || (*splits == NULL))
    return NULL;

  orig = splits;

  while (*splits != NULL)
    splits++;

  do
  {
    Transaction *trans;

    splits--;

    trans = xaccSplitGetParent(*splits);

    if (safe_strcmp(description, xaccTransGetDescription(trans)) == 0)
      return trans;

  } while (splits != orig);

  return NULL;
}

static Split *
gnc_find_split_in_reg_by_memo(SplitRegister *reg, const char *memo,
                              Transaction *dest_tran, gboolean unit_price)
{
  Table *table;
  int virt_row, virt_col;
  int num_rows, num_cols;
  Transaction *last_trans;

  if (reg == NULL)
    return NULL;

  table = reg->table;
  if (table == NULL)
    return NULL;

  num_rows = table->num_virt_rows;
  num_cols = table->num_virt_cols;

  last_trans = NULL;

  for (virt_row = num_rows - 1; virt_row >= 0; virt_row--)
    for (virt_col = num_cols - 1; virt_col >= 0; virt_col--)
    {
      Split *split;
      Transaction *trans;

      split = gnc_table_get_user_data_virtual (table, virt_row, virt_col);
      trans = xaccSplitGetParent(split);

      if (trans == last_trans)
        continue;

      split = gnc_find_split_in_trans_by_memo(trans, memo, dest_tran,
                                              unit_price);
      if (split != NULL)
        return split;

      last_trans = trans;
    }

  return NULL;
}

static Transaction *
gnc_find_trans_in_reg_by_desc(SplitRegister *reg, const char *description)
{
  Table *table;
  int virt_row, virt_col;
  int num_rows, num_cols;
  Transaction *last_trans;

  if (reg == NULL)
    return NULL;

  table = reg->table;
  if (table == NULL)
    return NULL;

  num_rows = table->num_virt_rows;
  num_cols = table->num_virt_cols;

  last_trans = NULL;

  for (virt_row = num_rows - 1; virt_row >= 0; virt_row--)
    for (virt_col = num_cols - 1; virt_col >= 0; virt_col--)
    {
      Split *split;
      Transaction *trans;

      split = gnc_table_get_user_data_virtual (table, virt_row, virt_col);
      trans = xaccSplitGetParent(split);

      if (trans == last_trans)
        continue;

      if (safe_strcmp(description, xaccTransGetDescription(trans)) == 0)
        return trans;

      last_trans = trans;
    }

  return NULL;
}

/* ======================================================== */
/* This callback gets called when the user clicks on the gui
 * in such a way as to leave the current virtual cursor, and 
 * go to a new one. So, save the current transaction.
 *
 * This callback is centrally involved in the redraw sequence.
 * When the user moves from one cell to another, causing a
 * change in the current virtual cursor, the following 
 * sequence of events get triggered and cascade down:
 *  xaccVerifyCursorPosition() {
 *    doMoveCursor() {
 *      callback for move() which is this function (LedgerMoveCursor) {
 *        xaccSRSaveRegEntry() {...}
 *        RedrawRegEntry() {
 *          SRLoadRegister() {
 *            SRLoadRegEntry()
 *            xaccMoveCursor()
 *            ...
 *          }
 *        }}}}
 */

static void
LedgerMoveCursor (Table *table,
                  int *p_new_phys_row,
                  int *p_new_phys_col)
{
  int new_phys_row = *p_new_phys_row;
  int new_phys_col = *p_new_phys_col;
  SplitRegister *reg = table->user_data;
  SRInfo *info = xaccSRGetInfo(reg);
  Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
  Transaction *new_trans;
  Transaction *trans;
  PhysicalCell *pcell;
  Split *trans_split;
  Split *new_split;
  int new_cell_row;
  int new_cell_col;
  int phys_row_offset;
  int phys_col_offset;
  gboolean saved;

  PINFO ("start callback %d %d \n", new_phys_row, new_phys_col);

  /* The transaction we are coming from */
  trans = xaccSRGetCurrentTrans(reg);

  /* The change in physical location */
  phys_row_offset = new_phys_row - table->current_cursor_phys_row;
  phys_col_offset = new_phys_col - table->current_cursor_phys_col;

  if (!info->hint_set_by_traverse)
  {
    /* The transaction where we are moving to */
    new_trans = xaccSRGetTrans(reg, new_phys_row, new_phys_col);

    /* The split we are moving to */
    new_split = gnc_table_get_user_data_physical(reg->table,
                                                 new_phys_row, new_phys_col);

    /* The split at the transaction line we are moving to */
    trans_split = xaccSRGetTransSplit(reg, new_phys_row, new_phys_col);
  }
  else
  {
    new_trans = info->cursor_hint_trans;
    new_split = info->cursor_hint_split;
    trans_split = info->cursor_hint_trans_split;
  }

  /* The cell offset we are moving to */
  if ((new_phys_row < 0) || (new_phys_col < 0) ||
      (new_phys_row >= table->num_phys_rows) ||
      (new_phys_col >= table->num_phys_cols))
  {
    new_cell_row = 0;
    new_cell_col = 0;
  }
  else
  {
    pcell = gnc_table_get_physical_cell (table, new_phys_row, new_phys_col);

    new_cell_row = pcell->virt_loc.phys_row_offset;
    new_cell_col = pcell->virt_loc.phys_col_offset;
  }

  /* commit the contents of the cursor into the database */
  saved = xaccSRSaveRegEntry (reg, trans != new_trans);
  if ((pending_trans != NULL) &&
      (pending_trans == trans) &&
      (trans != new_trans))
  {
    if (xaccTransIsOpen(trans))
      xaccTransCommitEdit (trans);
    info->pending_trans_guid = *xaccGUIDNULL();
    pending_trans = NULL;
    saved = TRUE;
  }

  /* redrawing the register can muck everything up */
  if (saved) {
    int virt_row, virt_col;

    xaccSRRedrawRegEntry (reg);

    /* if the split we were going to is still in the register,
     * then it may have moved. Find out where it is now. */
    if (xaccSRGetTransSplitRowCol (reg, new_trans, trans_split, new_split,
                                   &virt_row, &virt_col)) {
      VirtualCell *vcell;

      vcell = gnc_table_get_virtual_cell (table, virt_row, virt_col);

      new_phys_row = vcell->phys_loc.phys_row;
      new_phys_col = vcell->phys_loc.phys_col;

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

  gnc_table_find_valid_cell_horiz(table, &new_phys_row, &new_phys_col,
                                  info->exact_traversal);

  *p_new_phys_row = new_phys_row;
  *p_new_phys_col = new_phys_col;

  PINFO ("after redraw %d %d \n",
         new_phys_row, new_phys_col);

  reg->cursor_phys_row = new_phys_row;

  pcell = gnc_table_get_physical_cell (table, new_phys_row, new_phys_col);
  reg->cursor_virt_row = pcell->virt_loc.virt_row;

  /* if auto-expansion is enabled, we need to redraw the register
   * to expand out the splits at the new location. We use the
   * cursor_hint data members to tell the refresh routine where
   * to go. */
  if ((REG_SINGLE_DYNAMIC == reg->style) ||
      (REG_DOUBLE_DYNAMIC == reg->style)) 
  {
    new_trans = xaccSRGetTrans(reg, new_phys_row, new_phys_col);
    info->cursor_hint_trans = new_trans;

    trans_split = xaccSRGetTransSplit(reg, new_phys_row, new_phys_col);
    info->cursor_hint_trans_split = trans_split;

    new_split = gnc_table_get_user_data_physical (reg->table,
                                                  new_phys_row, new_phys_col);
    info->cursor_hint_split = new_split;

    info->cursor_hint_phys_col = new_phys_col;

    xaccRegisterRefresh (reg);

    /* indicate what row we should go to */
    *p_new_phys_row = table->current_cursor_phys_row;
    *p_new_phys_col = table->current_cursor_phys_col;

    info->cursor_hint_trans = xaccSRGetCurrentTrans (reg);
    info->cursor_hint_split = xaccSRGetCurrentSplit (reg);
    info->cursor_hint_trans_split = xaccSRGetCurrentTransSplit (reg);
    info->cursor_hint_phys_col = -1;

    PINFO ("after dynamic %d %d stored val %d\n",
           *p_new_phys_row, *p_new_phys_col, reg->cursor_phys_row);
  }
  else
  {
    info->cursor_hint_trans = new_trans;
    info->cursor_hint_split = new_split;
    info->cursor_hint_trans_split = trans_split;
    info->cursor_hint_phys_col = -1;
  }

  info->hint_set_by_traverse = FALSE;
}

/* This function determines if auto-completion is appropriate and,
 * if so, performs it. This should only be called by LedgerTraverse. */
static void
LedgerAutoCompletion(SplitRegister *reg, gncTableTraversalDir dir,
                     int *p_new_phys_row, int *p_new_phys_col)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
  CursorType cursor_type;
  CellType cell_type;
  Transaction *trans;
  guint32 changed;
  double amount;
  Split *split;
  int new_row;
  int new_col;

  /* auto-completion is only triggered by a tab out */
  if (dir != GNC_TABLE_TRAVERSE_RIGHT)
    return;

  split = xaccSRGetCurrentSplit(reg);
  trans = xaccSRGetCurrentTrans(reg);
  if (trans == NULL)
    return;

  cursor_type = xaccSplitRegisterGetCursorType(reg);
  cell_type = xaccSplitRegisterGetCellType(reg);
  changed = xaccSplitRegisterGetChangeFlag(reg);

  switch (cursor_type)
  {
    case CURSOR_TRANS: {
      Transaction *auto_trans;
      GList *refresh_accounts;
      char *desc;

      /* we must be on the blank split */
      if (split != blank_split)
        return;

      /* and leaving the description cell */
      if (cell_type != DESC_CELL)
        return;

      /* nothing but the date, num, and description should be changed */
      if ((changed & ~(MOD_DATE | MOD_NUM | MOD_DESC)) != 0)
        return;

      /* and the description should be changed */
      if ((changed & MOD_DESC) == 0)
        return;

      /* to a non-empty value */
      desc = reg->descCell->cell.value;
      if ((desc == NULL) || (*desc == '\0'))
        return;

      /* find a transaction to auto-complete on */
      if (info->default_source_account != NULL)
      {
        Account *account = info->default_source_account;

        auto_trans = gnc_find_trans_in_account_by_desc(account, desc);
      }
      else
        auto_trans = gnc_find_trans_in_reg_by_desc(reg, desc);

      if (auto_trans == NULL)
        return;

      xaccTransBeginEdit(trans, FALSE);
      gnc_copy_trans_onto_trans(auto_trans, trans, FALSE);

      if (info->default_source_account != NULL)
      {
        int num_splits;
        int i;

        blank_split = NULL;

        num_splits = xaccTransCountSplits(trans);
        for (i = 0; i < num_splits; i++)
        {
          Split *s = xaccTransGetSplit(trans, i);

          if (info->default_source_account == xaccSplitGetAccount(s))
          {
            blank_split = s;
            info->blank_split_guid = *xaccSplitGetGUID(blank_split);
            break;
          }
        }

        if (blank_split == NULL)
        {
          blank_split = xaccTransGetSplit(trans, 0);
          info->blank_split_guid = *xaccSplitGetGUID(blank_split);
        }
      }
      else
      {
        blank_split = xaccTransGetSplit(trans, 0);
        info->blank_split_guid = *xaccSplitGetGUID(blank_split);
      }

      if ((pending_trans != NULL) && (pending_trans != trans))
        if (xaccTransIsOpen(pending_trans))
          xaccTransCommitEdit(pending_trans);

      pending_trans = trans;
      info->pending_trans_guid = *xaccTransGetGUID(pending_trans);

      info->blank_split_edited = TRUE;

      refresh_accounts = xaccSRSaveChangedCells(reg, trans, blank_split);

      /* Refresh the GUI. */
      refresh_accounts = gnc_trans_prepend_account_list(trans,
                                                        refresh_accounts);

      gnc_refresh_main_window();
      gnc_account_glist_ui_refresh(refresh_accounts);

      g_list_free(refresh_accounts);

      /* now move to the non-empty amount column */
      amount = xaccSplitGetShareAmount (blank_split);
      cell_type = (amount >= 0) ? DEBT_CELL : CRED_CELL;

      if (xaccSplitRegisterGetCellRowCol (reg, cell_type, &new_row, &new_col))
      {
        *p_new_phys_row = new_row;
        *p_new_phys_col = new_col;
      }
    }

    break;

    case CURSOR_SPLIT: {
      char *memo, *fullname;
      gboolean unit_price;
      Split *auto_split;

      /* we must be on a blank split of a transaction */
      if (split != NULL)
        return;

      /* and leaving the memo cell */
      if (cell_type != MEMO_CELL)
        return;

      /* nothing but the action and memo should be changed */
      if ((changed & ~(MOD_ACTN | MOD_MEMO)) != 0)
        return;

      /* and the memo should be changed */
      if ((changed & MOD_MEMO) == 0)
        return;

      /* to a non-empty value */
      memo = reg->memoCell->cell.value;
      if ((memo == NULL) || (*memo == '\0'))
        return;

      /* if there is no price field, only auto-complete from splits with
       * a unit share price. */
      unit_price = !xaccSplitRegisterGetCellRowCol(reg, PRIC_CELL, NULL, NULL);

      /* find a split to auto-complete on */
      if (info->default_source_account != NULL)
      {
        Account *account = info->default_source_account;

        auto_split = gnc_find_split_in_account_by_memo(account, memo, trans,
                                                       unit_price);
      }
      else
        auto_split = gnc_find_split_in_reg_by_memo(reg, memo, trans,
                                                   unit_price);

      if (auto_split == NULL)
        return;

      /* the auto-complete code below is taken from xaccSRLoadRegEntry */

      /* auto-complete the action field if it wasn't changed */
      if (!(MOD_ACTN & changed))
        xaccSetComboCellValue (reg->actionCell,
                               xaccSplitGetAction (auto_split));

      /* auto-complete the account name */
      fullname = xaccAccountGetFullName (xaccSplitGetAccount (auto_split),
                                         account_separator);
      xaccSetComboCellValue (reg->xfrmCell, fullname);
      xaccBasicCellSetChanged(&(reg->xfrmCell->cell), TRUE);

      amount = xaccSplitGetValue (auto_split);

      xaccSetDebCredCellValue (reg->ndebitCell, reg->ncreditCell, -amount);
      xaccBasicCellSetChanged (&(reg->ndebitCell->cell), TRUE);
      xaccBasicCellSetChanged (&(reg->ncreditCell->cell), TRUE);

      /* copy cursor contents into the table */
      gnc_table_commit_cursor (reg->table);

      /* and refresh the gui */
      gnc_table_refresh_gui (reg->table);

      /* now move to the non-empty amount column */
      amount = xaccSplitGetShareAmount (auto_split);
      cell_type = (amount < 0) ? NDEBT_CELL : NCRED_CELL;

      if (xaccSplitRegisterGetCellRowCol (reg, cell_type, &new_row, &new_col))
      {
        *p_new_phys_row = new_row;
        *p_new_phys_col = new_col;
      }
    }

    break;

    default:
      break;
  }
}

/* ======================================================== */
/* This callback gets called when the user clicks on the gui
 * in such a way as to leave the current transaction, and to 
 * go to a new one. It is called to verify what the coordinates
 * of the new cell will be.
 */

static void
LedgerTraverse (Table *table,
                int *p_new_phys_row,
                int *p_new_phys_col,
                gncTableTraversalDir dir)
{
  SplitRegister *reg = table->user_data;
  SRInfo *info = xaccSRGetInfo(reg);
  Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
  Transaction *trans, *new_trans;
  int phys_row = *p_new_phys_row;
  int phys_col = *p_new_phys_col;
  int virt_row, virt_col;
  GNCVerifyResult result;
  guint32 changed;
  Split *split;

  info->exact_traversal = (dir == GNC_TABLE_TRAVERSE_POINTER);

  split = xaccSRGetCurrentSplit(reg);
  trans = xaccSRGetCurrentTrans(reg);
  if (trans == NULL)
    return;

  /* no changes, make sure we aren't going off the end */
  changed = xaccSplitRegisterGetChangeFlag(reg);
  if (!changed && (pending_trans != trans))
  {
    if (gnc_table_physical_cell_valid(table, phys_row, phys_col, FALSE))
      return;

    if (phys_row < reg->num_header_rows)
      phys_row = reg->num_header_rows;
    if (phys_row >= table->num_phys_rows)
      phys_row = table->num_phys_rows - 1;

    gnc_table_find_valid_cell_horiz(table, &phys_row, &phys_col,
                                    info->exact_traversal);

    *p_new_phys_row = phys_row;
    *p_new_phys_col = phys_col;

    return;
  }

  /* Now see if we are changing cursors. If not, we may be able to
   * auto-complete. */
  if ((phys_row >= 0) && (phys_col >= 0) &&
      (phys_row < table->num_phys_rows) && (phys_col < table->num_phys_cols))
  {
    PhysicalCell *pcell;

    pcell = gnc_table_get_physical_cell (table, phys_row, phys_col);

    virt_row = pcell->virt_loc.virt_row;
    virt_col = pcell->virt_loc.virt_col;

    if ((virt_row == table->current_cursor_virt_row) &&
        (virt_col == table->current_cursor_virt_col))
    {
      LedgerAutoCompletion(reg, dir, p_new_phys_row, p_new_phys_col);
      return;
    }
  }

  if (changed && (split == NULL) && (dir == GNC_TABLE_TRAVERSE_RIGHT))
  {
    /* If we are here, then: (a) the current cursor has been
     * edited, and (b) we are on the blank split of a multi-line
     * transaction, and (c) we are tabbing out of the last cell
     * on the line. Thus, we want to go ahead and add the new
     * split and end up on the new blank split of the current
     * transaction. */
    info->cursor_hint_trans = trans;
    info->cursor_hint_split = split;
    info->cursor_hint_trans_split = xaccSRGetCurrentTransSplit (reg);
    info->cursor_hint_phys_col = -1;
    info->hint_set_by_traverse = TRUE;

    return;
  }

  /* Check for going off the end */
  if (phys_row < reg->num_header_rows)
  {
    phys_row = reg->num_header_rows;
    gnc_table_find_valid_cell_horiz(table, &phys_row, &phys_col,
                                    info->exact_traversal);
  }
  if (phys_row >= table->num_phys_rows)
  {
    phys_row = table->num_phys_rows - 1;
    gnc_table_find_valid_cell_horiz(table, &phys_row, &phys_col,
                                    info->exact_traversal);
  }

  /* Same transaction, no problem */
  new_trans = xaccSRGetTrans(reg, phys_row, phys_col);
  if (trans == new_trans)
  {
    *p_new_phys_row = phys_row;
    *p_new_phys_col = phys_col;

    return;
  }

  /* Ok, we are changing transactions and the current transaction has
   * changed. See what the user wants to do. */

  result = gnc_verify_cancel_dialog_parented(xaccSRGetParent(reg),
                                             TRANS_CHANGED_MSG,
                                             GNC_VERIFY_YES);

  switch (result)
  {
    case GNC_VERIFY_NO: {
      Split *new_split;
      Split *trans_split;

      new_split = gnc_table_get_user_data_physical(reg->table,
                                                   phys_row, phys_col);
      trans_split = xaccSRGetTransSplit(reg, phys_row, phys_col);

      xaccSRCancelCursorTransChanges(reg);

      if (xaccSRGetTransSplitRowCol (reg, new_trans, trans_split, new_split,
                                     &virt_row, &virt_col))
      {
        VirtualCell *vcell;

        vcell = gnc_table_get_virtual_cell (table, virt_row, virt_col);

        phys_row = vcell->phys_loc.phys_row;
      }

      if (phys_row >= table->num_phys_rows)
        phys_row = table->num_phys_rows - 1;
      if (phys_col >= table->num_phys_cols)
        phys_col = table->num_phys_cols - 1;

      *p_new_phys_row = phys_row;
      *p_new_phys_col = phys_col;
    }

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
LedgerSetHelp (Table *table, const char *help_str)
{
  SplitRegister *reg = table->user_data;
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
   Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
   Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
   Transaction *trans;

   /* be sure to destroy the "blank split" */
   if (blank_split != NULL) {
      /* split destroy will automatically remove it
       * from its parent account */
      trans = xaccSplitGetParent (blank_split);

      /* Make sure we don't commit this below */
      if (trans == pending_trans)
      {
        info->pending_trans_guid = *xaccGUIDNULL();
        pending_trans = NULL;
      }

      xaccTransBeginEdit (trans, TRUE);
      xaccTransDestroy (trans);
      xaccTransCommitEdit (trans);

      info->blank_split_guid = *xaccGUIDNULL();
      blank_split = NULL;
   }

   /* be sure to take care of any open transactions */
   if (pending_trans != NULL) {
      if (xaccTransIsOpen(pending_trans))
        xaccTransRollbackEdit (pending_trans);

      info->pending_trans_guid = *xaccGUIDNULL();
      pending_trans = NULL;
   }

   xaccSRDestroyRegisterData(reg);
}

/* ======================================================== */

static Transaction *
xaccSRGetTrans (SplitRegister *reg, int phys_row, int phys_col)
{
  Split *split;
  PhysicalCell *pcell;
  int virt_row, virt_col;

  if (reg == NULL)
    return NULL;

  pcell = gnc_table_get_physical_cell (reg->table, phys_row, phys_col);
  if (pcell == NULL)
    return NULL;

  split = gnc_table_get_user_data_physical (reg->table, phys_row, phys_col);
  if (split != NULL)
    return xaccSplitGetParent(split);

  /* Split is blank. Assume it is the blank split of a multi-line
   * transaction. Go back one row to find a split in the transaction. */
  virt_row = pcell->virt_loc.virt_row;
  virt_col = pcell->virt_loc.virt_col;

  virt_row --;
  if ((0 > virt_row) || (0 > virt_col)) {
    PERR ("bad row \n");
    return NULL;
  }

  split = gnc_table_get_user_data_virtual (reg->table, virt_row, virt_col);
  if (split == NULL) {
    PERR ("no parent \n");
    return NULL;
  }

  return xaccSplitGetParent(split);
}

/* ======================================================== */

static Split *
xaccSRGetTransSplit (SplitRegister *reg, int phys_row, int phys_col)
{
  CursorType cursor_type;
  int virt_row, virt_col;
  PhysicalCell *pcell;

  if (reg == NULL)
    return NULL;

  pcell = gnc_table_get_physical_cell (reg->table, phys_row, phys_col);
  if (pcell == NULL)
    return NULL;

  virt_row = pcell->virt_loc.virt_row;
  virt_col = pcell->virt_loc.virt_col;

  while (1)
  {
    cursor_type = xaccSplitRegisterGetCursorTypeRowCol (reg,
                                                        virt_row, virt_col);
    if (cursor_type == CURSOR_TRANS)
      return gnc_table_get_user_data_virtual (reg->table, virt_row, virt_col);

    virt_row --;

    if ((0 > virt_row) || (0 > virt_col)) {
      PERR ("bad row \n");
      return NULL;
    }
  }
}

/* ======================================================== */

static Split *
xaccSRGetCurrentTransSplit (SplitRegister *reg)
{
  Split *split;
  PhysicalCell *pcell;
  CursorType cursor_type;
  int phys_row, phys_col;
  int virt_row, virt_col;

  if (reg == NULL)
    return NULL;

  split = xaccSRGetCurrentSplit (reg);
  cursor_type = xaccSplitRegisterGetCursorType (reg);
  if (cursor_type == CURSOR_TRANS)
    return split;

  /* Split is not associated with a transaction cursor. Assume it is a
   * split of a multi-line transaction. Go back until we find one that
   * is on a transaction cursor. */
  phys_row = reg->table->current_cursor_phys_row;
  phys_col = reg->table->current_cursor_phys_col;

  pcell = gnc_table_get_physical_cell (reg->table, phys_row, phys_col);
  if (pcell == NULL)
    return NULL;

  virt_row = pcell->virt_loc.virt_row;
  virt_col = pcell->virt_loc.virt_col;

  while (1)
  {
    virt_row --;

    if ((0 > virt_row) || (0 > virt_col)) {
      PERR ("bad row \n");
      return NULL;
    }

    cursor_type = xaccSplitRegisterGetCursorTypeRowCol (reg,
                                                        virt_row, virt_col);
    if (cursor_type == CURSOR_TRANS)
      return gnc_table_get_user_data_virtual (reg->table, virt_row, virt_col);
  }
}

/* ======================================================== */

Transaction *
xaccSRGetCurrentTrans (SplitRegister *reg)
{
  Split *split;
  PhysicalCell *pcell;
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

  pcell = gnc_table_get_physical_cell (reg->table, phys_row, phys_col);
  if (pcell == NULL)
    return NULL;

  virt_row = pcell->virt_loc.virt_row;
  virt_col = pcell->virt_loc.virt_col;

  virt_row --;
  if ((0 > virt_row) || (0 > virt_col)) {
    PERR ("bad row \n");
    return NULL;
  }

  split = gnc_table_get_user_data_virtual (reg->table, virt_row, virt_col);

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
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);

  return blank_split;
}

/* ======================================================== */

gboolean
xaccSRGetSplitRowCol (SplitRegister *reg, Split *split,
                      int *virt_row, int *virt_col)
{
  Table *table = reg->table;
  int v_row, v_col;
  Split *s;

  for (v_row = 1; v_row < table->num_virt_rows; v_row++)
    for (v_col = 0; v_col < table->num_virt_cols; v_col++)
    {
      s = gnc_table_get_user_data_virtual (table, v_row, v_col);

      if (s == split)
      {
        if (virt_row != NULL)
          *virt_row = v_row;
        if (virt_col != NULL)
          *virt_col = v_col;

        return TRUE;
      }
    }

  return FALSE;
}

/* ======================================================== */

gboolean
xaccSRGetTransSplitRowCol (SplitRegister *reg, Transaction *trans,
                           Split *trans_split, Split *split,
                           int *virt_row, int *virt_col)
{
  Table *table = reg->table;
  gboolean found_trans = FALSE;
  gboolean found_trans_split = FALSE;
  gboolean found_something = FALSE;
  CursorType cursor_type;
  int v_row, v_col;
  Transaction *t;
  Split *s;

  for (v_row = 1; v_row < table->num_virt_rows; v_row++)
    for (v_col = 0; v_col < table->num_virt_cols; v_col++)
    {
      s = gnc_table_get_user_data_virtual (table, v_row, v_col);
      t = xaccSplitGetParent(s);

      cursor_type = xaccSplitRegisterGetCursorTypeRowCol(reg, v_row, v_col);

      if (t == trans)
        found_trans = TRUE;

      if ((cursor_type == CURSOR_TRANS) && (s == trans_split))
        found_trans_split = TRUE;

      if (found_trans && (s == split))
      {
        if (virt_row != NULL)
          *virt_row = v_row;
        if (virt_col != NULL)
          *virt_col = v_col;

        found_something = TRUE;
      }

      if (found_trans_split && (s == split))
      {
        if (virt_row != NULL)
          *virt_row = v_row;
        if (virt_col != NULL)
          *virt_col = v_col;

        return TRUE;
      }
    }

  return found_something;
}

/* ======================================================== */

Split *
xaccSRDuplicateCurrent (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  CursorType cursor_type;
  Transaction *trans;
  Split *return_split;
  Split *trans_split;
  guint32 changed;
  Split *split;

  split = xaccSRGetCurrentSplit(reg);
  trans = xaccSRGetCurrentTrans(reg);
  trans_split = xaccSRGetCurrentTransSplit(reg);

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
  if (!changed && ((split == NULL) || (split == blank_split)))
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

    xaccSRSaveRegEntry(reg, TRUE);

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

    /* We are on a split in an expanded transaction.
     * Just copy the split and add it to the transaction. */

    new_split = xaccMallocSplit();

    xaccTransBeginEdit(trans, TRUE);
    xaccTransAppendSplit(trans, new_split);
    xaccTransCommitEdit(trans);

    gnc_copy_split_onto_split(split, new_split);

    return_split = new_split;

    info->cursor_hint_split = new_split;
  }
  else
  {
    Transaction *new_trans;
    int split_index;
    int trans_split_index;

    /* We are on a transaction row. Copy the whole transaction. */

    split_index = gnc_trans_split_index(trans, split);
    trans_split_index = gnc_trans_split_index(trans, trans_split);

    /* we should *always* find the split, but be paranoid */
    if (split_index < 0)
      return NULL;

    new_trans = xaccMallocTransaction();

    gnc_copy_trans_onto_trans(trans, new_trans, TRUE);

    xaccTransBeginEdit(new_trans, TRUE);
    xaccTransSetDateSecs(new_trans, info->last_date_entered);
    xaccTransCommitEdit(new_trans);

    /* This shouldn't happen, but be paranoid. */
    if (split_index >= xaccTransCountSplits(new_trans))
      split_index = 0;

    return_split = xaccTransGetSplit(new_trans, split_index);
    trans_split = xaccTransGetSplit(new_trans, trans_split_index);

    info->cursor_hint_trans = new_trans;
    info->cursor_hint_split = return_split;
    info->cursor_hint_trans_split = trans_split;
  }

  /* Refresh the GUI. */
  gnc_refresh_main_window();
  gnc_transaction_ui_refresh(trans);

  return return_split;
}

/* ======================================================== */

void
xaccSRCopyCurrent (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  CursorType cursor_type;
  Transaction *trans;
  guint32 changed;
  Split *split;
  SCM new_item;

  split = xaccSRGetCurrentSplit(reg);
  trans = xaccSRGetCurrentTrans(reg);

  /* This shouldn't happen, but be paranoid. */
  if (trans == NULL)
    return;

  cursor_type = xaccSplitRegisterGetCursorType(reg);

  /* Can't do anything with this. */
  if (cursor_type == CURSOR_NONE)
    return;

  /* This shouldn't happen, but be paranoid. */
  if ((split == NULL) && (cursor_type == CURSOR_TRANS))
    return;

  changed = xaccSplitRegisterGetChangeFlag(reg);

  /* See if we were asked to copy an unchanged blank split. Don't. */
  if (!changed && ((split == NULL) || (split == blank_split)))
    return;

  /* Ok, we are now ready to make the copy. */

  if (cursor_type == CURSOR_SPLIT)
  {
    /* We are on a split in an expanded transaction. Just copy the split. */
    new_item = gnc_copy_split(split);

    if (new_item != SCM_UNDEFINED)
    {
      if (changed)
        xaccSRSaveRegEntryToSCM(reg, SCM_UNDEFINED, new_item);

      copied_leader_guid = *xaccGUIDNULL();
    }
  }
  else
  {
    /* We are on a transaction row. Copy the whole transaction. */
    new_item = gnc_copy_trans(trans);

    if (new_item != SCM_UNDEFINED)
    {
      if (changed)
      {
        int split_index;
        SCM split_scm;

        split_index = gnc_trans_split_index(trans, split);
        if (split_index >= 0)
          split_scm = gnc_trans_scm_get_split_scm(new_item, split_index);
        else
          split_scm = SCM_UNDEFINED;

        xaccSRSaveRegEntryToSCM(reg, new_item, split_scm);
      }

      copied_leader_guid = *xaccAccountGetGUID(info->default_source_account);
    }
  }

  if (new_item == SCM_UNDEFINED)
    return;

  /* unprotect the old object, if any */
  if (copied_item != SCM_UNDEFINED)
    scm_unprotect_object(copied_item);

  copied_item = new_item;
  scm_protect_object(copied_item);

  copied_type = cursor_type;
}

/* ======================================================== */

void
xaccSRCutCurrent (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  CursorType cursor_type;
  Transaction *trans;
  guint32 changed;
  Split *split;

  split = xaccSRGetCurrentSplit(reg);
  trans = xaccSRGetCurrentTrans(reg);

  /* This shouldn't happen, but be paranoid. */
  if (trans == NULL)
    return;

  cursor_type = xaccSplitRegisterGetCursorType(reg);

  /* Can't do anything with this. */
  if (cursor_type == CURSOR_NONE)
    return;

  /* This shouldn't happen, but be paranoid. */
  if ((split == NULL) && (cursor_type == CURSOR_TRANS))
    return;

  changed = xaccSplitRegisterGetChangeFlag(reg);

  /* See if we were asked to cut an unchanged blank split. Don't. */
  if (!changed && ((split == NULL) || (split == blank_split)))
    return;

  xaccSRCopyCurrent(reg);

  if (cursor_type == CURSOR_SPLIT)
    xaccSRDeleteCurrentSplit(reg);
  else
    xaccSRDeleteCurrentTrans(reg);
}

/* ======================================================== */

void
xaccSRPasteCurrent (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  GList *accounts = NULL;
  CursorType cursor_type;
  Transaction *trans;
  Split *trans_split;
  Split *split;

  if (copied_type == CURSOR_NONE)
    return;

  split = xaccSRGetCurrentSplit(reg);
  trans = xaccSRGetCurrentTrans(reg);

  trans_split = xaccSRGetCurrentTransSplit(reg);

  /* This shouldn't happen, but be paranoid. */
  if (trans == NULL)
    return;

  cursor_type = xaccSplitRegisterGetCursorType(reg);

  /* Can't do anything with this. */
  if (cursor_type == CURSOR_NONE)
    return;

  /* This shouldn't happen, but be paranoid. */
  if ((split == NULL) && (cursor_type == CURSOR_TRANS))
    return;

  if (cursor_type == CURSOR_SPLIT) {
    if (copied_type == CURSOR_TRANS)
      return;

    accounts = gnc_trans_prepend_account_list(trans, NULL);

    if (split == NULL)
    { /* We are on a null split in an expanded transaction. */
      split = xaccMallocSplit();

      xaccTransBeginEdit(trans, TRUE);
      xaccTransAppendSplit(trans, split);
      xaccTransCommitEdit(trans);
    }

    gnc_copy_split_scm_onto_split(copied_item, split);
  }
  else {
    const GUID *new_guid;
    int trans_split_index;
    int split_index;
    int num_splits;

    if (copied_type == CURSOR_SPLIT)
      return;

    accounts = gnc_trans_prepend_account_list(trans, NULL);

    /* in pasting, the old split is deleted. */
    if (split == blank_split)
    {
      info->blank_split_guid = *xaccGUIDNULL();
      blank_split = NULL;
    }

    split_index = gnc_trans_split_index(trans, split);
    trans_split_index = gnc_trans_split_index(trans, trans_split);

    if ((info->default_source_account != NULL) &&
        (xaccGUIDType(&copied_leader_guid) != GNC_ID_NULL))
    {
      new_guid = xaccAccountGetGUID(info->default_source_account);
      gnc_copy_trans_scm_onto_trans_swap_accounts(copied_item, trans,
                                                  &copied_leader_guid,
                                                  new_guid, TRUE);
    }
    else
      gnc_copy_trans_scm_onto_trans(copied_item, trans, TRUE);

    num_splits = xaccTransCountSplits(trans);
    if (split_index >= num_splits)
      split_index = 0;

    info->cursor_hint_trans = trans;
    info->cursor_hint_split = xaccTransGetSplit(trans, split_index);
    info->cursor_hint_trans_split = xaccTransGetSplit(trans,
                                                      trans_split_index);
  }

  accounts = gnc_trans_prepend_account_list(trans, accounts);

  /* Refresh the GUI. */
  gnc_refresh_main_window();
  gnc_account_glist_ui_refresh(accounts);

  g_list_free(accounts);
}

/* ======================================================== */

void
xaccSRDeleteCurrentSplit (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
  Transaction *trans;
  Account *account;
  GList *accounts;
  Split *split;

  /* get the current split based on cursor position */
  split = xaccSRGetCurrentSplit(reg);
  if (split == NULL)
    return;

  /* If we are deleting the blank split, just cancel. The user is
   * allowed to delete the blank split as a method for discarding
   * any edits they may have made to it. */
  if (split == blank_split)
  {
    xaccSRCancelCursorSplitChanges(reg);
    return;
  }

  /* make a copy of all of the accounts that will be  
   * affected by this deletion, so that we can update
   * their register windows after the deletion. */
  trans = xaccSplitGetParent(split);

  accounts = gnc_trans_prepend_account_list(trans, NULL);

  account = xaccSplitGetAccount(split);

  xaccTransBeginEdit(trans, TRUE);
  xaccAccountBeginEdit(account, TRUE);
  xaccSplitDestroy(split);
  xaccAccountCommitEdit(account);
  xaccTransCommitEdit(trans);

  /* Check pending transaction */
  if (trans == pending_trans)
  {
    info->pending_trans_guid = *xaccGUIDNULL();
    pending_trans = NULL;
  }

  gnc_refresh_main_window ();
  gnc_account_glist_ui_refresh(accounts);

  g_list_free(accounts);
}

/* ======================================================== */

void
xaccSRDeleteCurrentTrans (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
  Transaction *trans;
  Account *account;
  GList *accounts;
  Split *split;

  /* get the current split based on cursor position */
  split = xaccSRGetCurrentSplit(reg);
  if (split == NULL)
    return;

  /* If we just deleted the blank split, clean up. The user is
   * allowed to delete the blank split as a method for discarding
   * any edits they may have made to it. */
  if (split == blank_split)
  {
    trans = xaccSplitGetParent (blank_split);
    account = xaccSplitGetAccount(split);

    /* Make sure we don't commit this later on */
    if (trans == pending_trans)
    {
      info->pending_trans_guid = *xaccGUIDNULL();
      pending_trans = NULL;
    }

    xaccTransBeginEdit (trans, TRUE);
    xaccTransDestroy (trans);
    xaccTransCommitEdit (trans);

    info->blank_split_guid = *xaccGUIDNULL();
    blank_split = NULL;

    xaccAccountDisplayRefresh(account);
    return;
  }

  /* make a copy of all of the accounts that will be  
   * affected by this deletion, so that we can update
   * their register windows after the deletion. */
  trans = xaccSplitGetParent(split);

  accounts = gnc_trans_prepend_account_list(trans, NULL);

  xaccTransBeginEdit(trans, TRUE);
  xaccTransDestroy(trans);
  xaccTransCommitEdit(trans);

  /* Check pending transaction */
  if (trans == pending_trans)
  {
    info->pending_trans_guid = *xaccGUIDNULL();
    pending_trans = NULL;
  }

  gnc_refresh_main_window ();
  gnc_account_glist_ui_refresh(accounts);

  g_list_free(accounts);
}

/* ======================================================== */

void
xaccSREmptyCurrentTrans (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
  Transaction *trans;
  Account *account;
  GList *accounts;
  GList *splits;
  GList *node;
  Split *split;

  /* get the current split based on cursor position */
  split = xaccSRGetCurrentSplit(reg);
  if (split == NULL)
    return;

  /* If we just deleted the blank split, clean up. The user is
   * allowed to delete the blank split as a method for discarding
   * any edits they may have made to it. */
  if (split == blank_split)
  {
    trans = xaccSplitGetParent (blank_split);
    account = xaccSplitGetAccount(split);

    /* Make sure we don't commit this later on */
    if (trans == pending_trans)
    {
      info->pending_trans_guid = *xaccGUIDNULL();
      pending_trans = NULL;
    }

    xaccTransBeginEdit (trans, TRUE);
    xaccTransDestroy (trans);
    xaccTransCommitEdit (trans);

    info->blank_split_guid = *xaccGUIDNULL();
    blank_split = NULL;

    xaccAccountDisplayRefresh(account);
    return;
  }

  /* make a copy of all of the accounts that will be  
   * affected by this deletion, so that we can update
   * their register windows after the deletion. */
  trans = xaccSplitGetParent(split);

  accounts = gnc_trans_prepend_account_list(trans, NULL);
  splits = gnc_trans_prepend_split_list(trans, NULL);

  xaccTransBeginEdit(trans, TRUE);
  for (node = splits; node; node = node->next)
    if (node->data != split)
      xaccSplitDestroy(node->data);
  xaccTransCommitEdit(trans);

  /* Check pending transaction */
  if (trans == pending_trans)
  {
    info->pending_trans_guid = *xaccGUIDNULL();
    pending_trans = NULL;
  }

  gnc_refresh_main_window ();
  gnc_account_glist_ui_refresh(accounts);

  g_list_free(accounts);
  g_list_free(splits);
}

/* ======================================================== */

void
xaccSRCancelCursorSplitChanges (SplitRegister *reg)
{
  Split * split;
  guint32 changed;
  int row = reg->table->current_cursor_phys_row;
  int col = reg->table->current_cursor_phys_col;

  changed = xaccSplitRegisterGetChangeFlag(reg);
  if (!changed)
    return;

  /* We're just cancelling the current split here, not the transaction.
   * When cancelling edits, reload the cursor from the transaction. */
  split = xaccSRGetCurrentSplit(reg);
  xaccSRLoadRegEntry(reg, split);
  xaccSplitRegisterClearChangeFlag(reg);

  if (gnc_table_find_valid_cell_horiz(reg->table, &row, &col, FALSE))
    gnc_table_move_cursor_gui(reg->table, row, col);

  gnc_table_refresh_gui(reg->table);
}

/* ======================================================== */

void
xaccSRCancelCursorTransChanges (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
  Transaction *trans;
  GList *accounts;

  /* Get the currently open transaction, rollback the edits on it, and
   * then repaint everything. To repaint everything, make a note of
   * all of the accounts that will be affected by this rollback. */
  trans = pending_trans;

  if (!xaccTransIsOpen(trans))
  {
    xaccSRCancelCursorSplitChanges(reg);
    return;
  }

  accounts = gnc_trans_prepend_account_list(trans, NULL);

  xaccTransRollbackEdit (trans);

  accounts = gnc_trans_prepend_account_list(trans, accounts);

  gnc_account_glist_ui_refresh(accounts);

  g_list_free(accounts);

  info->pending_trans_guid = *xaccGUIDNULL();
  pending_trans = NULL;

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
   gnc_refresh_main_window();
   gnc_transaction_ui_refresh(trans);
}

/* ======================================================== */
/* Copy from the register object to scheme. This needs to be
 * in sync with xaccSRSaveRegEntry and xaccSRSaveChangedCells. */

static gboolean
xaccSRSaveRegEntryToSCM (SplitRegister *reg, SCM trans_scm, SCM split_scm)
{
  Transaction *trans;
  guint32 changed;

  /* use the changed flag to avoid heavy-weight updates
   * of the split & transaction fields. This will help
   * cut down on uneccessary register redraws. */
  changed = xaccSplitRegisterGetChangeFlag (reg);
  if (!changed)
    return FALSE;

  /* get the handle to the current split and transaction */
  trans = xaccSRGetCurrentTrans (reg);
  if (trans == NULL)
    return FALSE;

  /* copy the contents from the cursor to the split */
  if (MOD_DATE & changed) {
    Timespec ts;

    xaccDateCellGetDate(reg->dateCell, &ts);
    gnc_trans_scm_set_date(trans_scm, &ts);
  }

  if (MOD_NUM & changed)
    gnc_trans_scm_set_num(trans_scm, reg->numCell->cell.value);

  if (MOD_DESC & changed)
    gnc_trans_scm_set_description(trans_scm, reg->descCell->cell.value);

  if (MOD_RECN & changed)
    gnc_split_scm_set_reconcile_state(split_scm,
                                      xaccRecnCellGetFlag(reg->recnCell));

  if (MOD_ACTN & changed)
    gnc_split_scm_set_action(split_scm, reg->actionCell->cell.value);

  if (MOD_MEMO & changed)
    gnc_split_scm_set_memo(split_scm, reg->memoCell->cell.value);

  if ((MOD_XFRM | MOD_XTO) & changed) {
    Account *new_account;
    char *new_name;

    if (MOD_XFRM & changed)
      new_name = reg->xfrmCell->cell.value;
    else
      new_name = reg->xtoCell->cell.value;

    new_account = xaccGetAccountByFullName(trans, new_name, account_separator);

    if (new_account != NULL)
      gnc_split_scm_set_account(split_scm, new_account);
  }

  if (MOD_MXFRM & changed) {
    SCM other_split_scm;

    other_split_scm = gnc_trans_scm_get_other_split_scm(trans_scm, split_scm);

    if (other_split_scm == SCM_UNDEFINED) {
      if (gnc_trans_scm_get_num_splits(trans_scm) == 1) {
        Split *temp_split;
        char *temp_string;
        double price;
        double amount;

        temp_split = xaccMallocSplit ();
        other_split_scm = gnc_copy_split(temp_split);
        xaccSplitDestroy(temp_split);

        temp_string = gnc_split_scm_get_memo(split_scm);
        if (temp_string != NULL) {
          gnc_split_scm_set_memo(other_split_scm, temp_string);
          free(temp_string);
        }

        temp_string = gnc_split_scm_get_action(split_scm);
        if (temp_string != NULL) {
          gnc_split_scm_set_action(other_split_scm, temp_string);
          free(temp_string);
        }

        price = gnc_split_scm_get_share_price(other_split_scm);
        amount = gnc_split_scm_get_share_amount(other_split_scm);
        gnc_split_scm_set_share_price_and_amount(other_split_scm,
                                                 price, amount);

        gnc_trans_scm_append_split_scm(trans_scm, other_split_scm);
      }
    }

    if (other_split_scm != SCM_UNDEFINED) {
      Account *new_account;

      new_account = xaccGetAccountByFullName(trans, reg->mxfrmCell->cell.value,
                                             account_separator);

      if (new_account != NULL)
        gnc_split_scm_set_account(other_split_scm, new_account);
    }
  }

  if ((MOD_AMNT | MOD_NAMNT) & changed) {
    double new_amount;
    double price;
    double credit;
    double debit;

    if (MOD_AMNT & changed) {
      credit = xaccGetPriceCellValue(reg->creditCell);
      debit  = xaccGetPriceCellValue(reg->debitCell);
      new_amount = debit - credit;
    } else {
      credit = xaccGetPriceCellValue(reg->ncreditCell);
      debit  = xaccGetPriceCellValue(reg->ndebitCell);
      new_amount = -(debit - credit);
    }

    price = gnc_split_scm_get_share_price(split_scm);

    if ((STOCK_REGISTER    == (reg->type)) ||
        (CURRENCY_REGISTER == (reg->type)) ||
        (PORTFOLIO_LEDGER  == (reg->type)))
      ;
    else
      new_amount = new_amount / price;

    gnc_split_scm_set_share_price_and_amount(split_scm, price, new_amount);
  }

  if (MOD_PRIC & changed) {
    double price;
    double amount;

    price = xaccGetPriceCellValue(reg->priceCell);
    amount = gnc_split_scm_get_share_amount(split_scm);

    gnc_split_scm_set_share_price_and_amount(split_scm, price, amount);
  }

  if (MOD_VALU & changed) {
    double value = xaccGetPriceCellValue(reg->valueCell);
    double price = gnc_split_scm_get_share_price(split_scm);

    value = value / price;

    gnc_split_scm_set_share_price_and_amount(split_scm, price, value);
  }

  return TRUE;
}

/* ======================================================== */
/* Copy from the register object to the engine */

gboolean
xaccSRSaveRegEntry (SplitRegister *reg, gboolean do_commit)
{
   GList *refresh_accounts;
   SRInfo *info = xaccSRGetInfo(reg);
   Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
   Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
   Transaction *blank_trans = xaccSplitGetParent(blank_split);
   Transaction *trans;
   guint32 changed;
   Split *split;

   /* get the handle to the current split and transaction */
   split = xaccSRGetCurrentSplit (reg);
   trans = xaccSRGetCurrentTrans (reg);
   if (trans == NULL)
     return FALSE;

   /* use the changed flag to avoid heavy-weight updates
    * of the split & transaction fields. This will help
    * cut down on uneccessary register redraws. */
   changed = xaccSplitRegisterGetChangeFlag (reg);
   if (!changed)
   {
     if (!do_commit)
       return FALSE;

     if (trans == blank_trans)
     {
       if (xaccTransIsOpen(trans) || (info->blank_split_edited))
       {
         info->last_date_entered = xaccTransGetDate(trans);
         info->blank_split_guid = *xaccGUIDNULL();
         info->blank_split_edited = FALSE;
         blank_split = NULL;
       }
       else
         return FALSE;
     }
     else if (!xaccTransIsOpen(trans))
       return FALSE;

     if (xaccTransIsOpen(trans))
       xaccTransCommitEdit(trans);

     if (pending_trans == trans)
     {
       pending_trans = NULL;
       info->pending_trans_guid = *xaccGUIDNULL();
     }

     return TRUE;
   }

   ENTER ("xaccSRSaveRegEntry(): save split is %p \n", split);

   /* determine whether we should commit the pending transaction */
   if (pending_trans != trans) {
     if (xaccTransIsOpen (pending_trans))
       xaccTransCommitEdit (pending_trans);
     xaccTransBeginEdit (trans, FALSE);
     pending_trans = trans;
     info->pending_trans_guid = *xaccTransGetGUID(trans);
   }

   /* If we are committing the blank split, add it to the account now */
   if (trans == blank_trans)
   {
     xaccAccountInsertSplit (info->default_source_account, blank_split);
     xaccTransSetDateEnteredSecs(trans, time(NULL));
   }

   if (split == NULL) {
      /* If we were asked to save data for a row for which there is no
       * associated split, then assume that this was a row that was
       * set aside for adding splits to an existing transaction.
       * xaccSRGetCurrent will handle this case, too. We will create
       * a new split, copy the row contents to that split, and append
       * the split to the pre-existing transaction. */
     Split *trans_split;

      split = xaccMallocSplit ();
      xaccTransAppendSplit (trans, split);

      if (force_double_entry_awareness)
        xaccAccountInsertSplit (info->default_source_account, split);

      assert (reg->table->current_cursor);
      reg->table->current_cursor->user_data = split;

      trans_split = xaccSRGetCurrentTransSplit (reg);
      if ((info->cursor_hint_trans == trans) &&
          (info->cursor_hint_trans_split == trans_split) &&
          (info->cursor_hint_split == NULL))
        info->cursor_hint_split = split;
   }

   DEBUG ("updating trans addr=%p\n", trans);

   refresh_accounts = xaccSRSaveChangedCells (reg, trans, split);

   PINFO ("finished saving split %s of trans %s \n", 
          xaccSplitGetMemo(split), xaccTransGetDescription(trans));

   /* If the modified split is the "blank split", then it is now an
    * official part of the account. Set the blank split to NULL, so
    * we can be sure of getting a new split. Also, save the date for
    * the new blank split. */
   if (trans == blank_trans)
   {
     if (do_commit)
     {
       info->blank_split_guid = *xaccGUIDNULL();
       blank_split = NULL;
       info->last_date_entered = xaccTransGetDate(trans);
     }
     else
       info->blank_split_edited = TRUE;
   }

   /* If requested, commit the current transaction and set the pending
    * transaction to NULL. */
   if (do_commit)
   {
     xaccTransCommitEdit (trans);
     if (pending_trans == trans)
     {
       pending_trans = NULL;
       info->pending_trans_guid = *xaccGUIDNULL();
     }
   }

   xaccSplitRegisterClearChangeFlag(reg);

   if (refresh_accounts != NULL) {
     gnc_refresh_main_window();
     gnc_account_glist_ui_refresh(refresh_accounts);
     g_list_free(refresh_accounts);
   }

   return TRUE;
}

/* ======================================================== */

static GList *
xaccSRSaveChangedCells (SplitRegister *reg, Transaction *trans, Split *split)
{
  GList *refresh_accounts = NULL;
  guint32 changed;

  changed = xaccSplitRegisterGetChangeFlag (reg);

  /* copy the contents from the cursor to the split */
  if (MOD_DATE & changed) {
    /* commit any pending changes */
    xaccCommitDateCell (reg->dateCell);
    DEBUG ("MOD_DATE DMY= %2d/%2d/%4d \n",
                      reg->dateCell->date.tm_mday,
                      reg->dateCell->date.tm_mon+1,
                      reg->dateCell->date.tm_year+1900);

    xaccTransSetDate (trans,
                      reg->dateCell->date.tm_mday,
                      reg->dateCell->date.tm_mon+1,
                      reg->dateCell->date.tm_year+1900);
  }

  if (MOD_NUM & changed) {
    DEBUG ("MOD_NUM: %s\n", reg->numCell->cell.value);
    xaccTransSetNum (trans, reg->numCell->cell.value);
    xaccSetNumCellLastNum(reg->numCell, reg->numCell->cell.value);
  }

  if (MOD_DESC & changed) {
    DEBUG ("MOD_DESC: %s\n",
           reg->descCell->cell.value);
    xaccTransSetDescription (trans, reg->descCell->cell.value);
  }

  if (MOD_RECN & changed) {
    DEBUG ("MOD_RECN: %c\n", xaccRecnCellGetFlag(reg->recnCell));
    xaccSplitSetReconcile (split, xaccRecnCellGetFlag(reg->recnCell));
  }

  if (MOD_ACTN & changed) {
    DEBUG ("MOD_ACTN: %s\n",
           reg->actionCell->cell.value);
    xaccSplitSetAction (split, reg->actionCell->cell.value);
  }

  if (MOD_MEMO & changed) {
    DEBUG ("MOD_MEMO: %s\n",
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
   * XTO is the straight split, too :) Only one of XFRM or XTO
   * should be in a given cursor.
   */
  if ((MOD_XFRM | MOD_XTO) & changed) {
    Account *old_acc=NULL, *new_acc=NULL;
    char *new_name;

    if (MOD_XFRM & changed) {
      DEBUG ("MOD_XFRM: %s\n",
             reg->xfrmCell->cell.value);
    }
    else {
      DEBUG ("MOD_XTO: %s\n",
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
      const char *currency = NULL;
      const char *security = NULL;

      currency = xaccAccountGetCurrency(new_acc);
      currency = xaccTransIsCommonCurrency(trans, currency);

      if (currency == NULL) {
        security = xaccAccountGetSecurity(new_acc);
        security = xaccTransIsCommonCurrency(trans, security);
      }

      if ((currency != NULL) || (security != NULL)) {
        xaccAccountInsertSplit (new_acc, split);

        refresh_accounts = g_list_prepend(refresh_accounts, old_acc);
        refresh_accounts = g_list_prepend(refresh_accounts, new_acc);
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

    DEBUG ("MOD_MXFRM: %s\n",
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
        const char *currency = NULL;
        const char *security = NULL;

        currency = xaccAccountGetCurrency(new_acc);
        currency = xaccTransIsCommonCurrency(trans, currency);

        if (currency == NULL) {
          security = xaccAccountGetSecurity(new_acc);
          security = xaccTransIsCommonCurrency(trans, security);
        }

        if ((currency != NULL) || (security != NULL)) {
          xaccAccountInsertSplit (new_acc, other_split);

          refresh_accounts = g_list_prepend(refresh_accounts, old_acc);
          refresh_accounts = g_list_prepend(refresh_accounts, new_acc);
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
      ((STOCK_REGISTER    == (reg->type)) ||
       (CURRENCY_REGISTER == (reg->type)) ||
       (PORTFOLIO_LEDGER  == (reg->type)))) {

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
      new_amount = debit - credit;
    }
    else
      new_amount = xaccSplitGetShareAmount(split);

    if (!DEQEPS(value, price * new_amount, 1.0e-15)) {
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
          if (DEQEPS(0.0, new_amount, 1.0e-15))
            break;

          price = value / new_amount;

          if (price < 0) {
            price = -price;
            xaccSetPriceCellValue(reg->valueCell, -value);  
            changed |= MOD_VALU;
          }
          xaccSetPriceCellValue(reg->priceCell, price);
          changed |= MOD_PRIC;
          break;
        case 2: /* Modify total value */
          value = price * new_amount;

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
      new_amount = debit - credit;
    } else {
      credit = xaccGetPriceCellValue(reg->ncreditCell);
      debit  = xaccGetPriceCellValue(reg->ndebitCell);
      new_amount = -(debit - credit);
    }

    DEBUG ("MOD_AMNT: %f\n", new_amount);

    if ((STOCK_REGISTER    == (reg->type)) ||
        (CURRENCY_REGISTER == (reg->type)) ||
        (PORTFOLIO_LEDGER  == (reg->type)))
      xaccSplitSetShareAmount (split, new_amount);
    else
      xaccSplitSetValue (split, new_amount);
  }

  if (MOD_PRIC & changed) {
    double price;

    price = xaccGetPriceCellValue(reg->priceCell);

    DEBUG ("MOD_PRIC: %f\n", price);

    xaccSplitSetSharePrice (split, price);
  }

  if (MOD_VALU & changed) {
    double value = xaccGetPriceCellValue(reg->valueCell);

    DEBUG ("MOD_VALU: %f\n", value);

    xaccSplitSetValue (split, value);
  }

  return refresh_accounts;
}

/* ======================================================== */

static void
xaccSRLoadRegEntry (SplitRegister *reg, Split *split)
{
   SRInfo *info = xaccSRGetInfo(reg);
   Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
   SplitRegisterType reg_type = reg->type;
   double baln;

   /* don't even bother doing a load if there is no current cursor */
   if (!(reg->table->current_cursor)) return;

   ENTER ("SRLoadTransEntry(): s=%p\n", split);

   if (!split) {
      /* we interpret a NULL split as a blank split */
      xaccSetDateCellValueSecs (reg->dateCell, 0);
      xaccSetNumCellValue (reg->numCell, "");
      xaccSetQuickFillCellValue (reg->descCell, "");
      xaccRecnCellSetFlag (reg->recnCell, NREC);
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

      xaccRecnCellSetFlag (reg->recnCell, xaccSplitGetReconcile (split));

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
      else if ((INCOME_REGISTER == reg_type) ||
               (EXPENSE_REGISTER == reg_type)) { 
         baln = -baln;
      }

      if (split == blank_split)
        xaccSetPriceCellBlank (reg->balanceCell);
      else
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
         gboolean need_to_free = FALSE;

         if (s) {
            accname = xaccAccountGetFullName (xaccSplitGetAccount (s),
                                              account_separator);
            need_to_free = TRUE;
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

      if ((STOCK_REGISTER    == reg_type) ||
          (CURRENCY_REGISTER == reg_type) ||
          (PORTFOLIO_LEDGER  == reg_type)) 
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

   reg->table->current_cursor->user_data = split;

   /* copy cursor contents into the table */
   gnc_table_commit_cursor (reg->table);

   LEAVE("SRLoadTransEntry()\n");
}

/* ======================================================== */

static void
xaccSRCountRows (SplitRegister *reg,
                 Split        **slist,
                 Transaction   *find_trans,
                 Split         *find_split,
                 Split         *find_trans_split,
                 gboolean      *ext_found_trans,
                 gboolean      *ext_found_split,
                 gboolean      *ext_found_trans_split,
                 gboolean      *ext_on_blank_split)
{
   SRInfo *info = xaccSRGetInfo(reg);
   Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
   CellBlock *lead_cursor;
   PhysicalCell *pcell;
   Transaction *trans;
   Split *split;
   Table *table;

   gboolean found_split = FALSE;
   gboolean found_trans = FALSE;
   gboolean found_trans_split = FALSE;
   gboolean on_blank_split = FALSE;
   gboolean did_expand = FALSE;
   gboolean on_trans_split;
   gboolean multi_line;
   gboolean dynamic;

   SplitRegisterStyle style;
   int save_cursor_phys_row;
   int save_cursor_virt_row;
   int save_cell_row = -1;
   int num_phys_rows;
   int num_virt_rows;
   int i;

   table = reg->table;
   style = reg->style;
   multi_line  = (REG_MULTI_LINE == style);
   dynamic = ((REG_SINGLE_DYNAMIC == style) || (REG_DOUBLE_DYNAMIC == style));
   if ((REG_SINGLE_LINE == style) || (REG_SINGLE_DYNAMIC == style))
      lead_cursor = reg->single_cursor;
   else
      lead_cursor = reg->double_cursor;

   /* save the current cursor location; if we can't find the
    * requested transaction/split pair, we restore the 
    * cursor to this location when we are done. */
   save_cursor_phys_row = reg->cursor_phys_row;
   save_cursor_virt_row = reg->cursor_virt_row;

   /* save the current cell row offset */
   pcell = gnc_table_get_physical_cell (table,
                                        table->current_cursor_phys_row,
                                        table->current_cursor_phys_col);
   if (pcell)
     save_cell_row = pcell->virt_loc.phys_row_offset;
   if (save_cell_row < 0)
     save_cell_row = 0;

   /* num_phys_rows is the number of rows in all the cursors.
    * num_virt_rows is the number of cursors (including the header).
    * Count the number of rows needed.
    * the phys row count will be equal to: (sort of)
    * +1   for the header
    * +n   that is, one (transaction) row for each split passed in,
    * +n   one blank edit row for each transaction
    * +p   where p is the sum total of all the splits in the transaction
    * +2   an editable transaction and split at the end.
    */
   num_phys_rows = reg->header->numRows;
   num_virt_rows = 1;

   /* Look for the transaction split */
   i=0;
   if (slist)
     split = slist[0]; 
   else
     split = NULL;

   while (split) {
     if (split == find_trans_split)
     {
       found_trans_split = TRUE;
       break;
     }
     i++;
     split = slist[i];
   }

   split = blank_split;
   if ((split != NULL) && (split == find_trans_split))
     found_trans_split = TRUE;

   /* now count the rows */
   i=0;
   if (slist)
     split = slist[0]; 
   else
     split = NULL;

   while (split) {
      /* do not count the blank split */
      if (split != blank_split) {
         gboolean do_expand;

         trans = xaccSplitGetParent(split);

         /* lets determine where to locate the cursor ... */
         on_trans_split = (find_trans_split == split);

         if (!found_split) {
           /* Check to see if we find a perfect match */
           if (split == find_split) {
             save_cursor_phys_row = num_phys_rows;
             save_cursor_virt_row = num_virt_rows;
             if (on_trans_split || !found_trans_split)
               found_split = TRUE;
             found_trans = TRUE;
           }
           /* Otherwise, check for a close match. This could happen
            * if, e.g., we are collapsing from multi-line to single. */
           else if (on_trans_split) {
             save_cursor_phys_row = num_phys_rows;
             save_cursor_virt_row = num_virt_rows;
             if (trans == find_trans)
               found_trans = TRUE;
           }
           else if (!found_trans && (trans == find_trans)) {
             save_cursor_phys_row = num_phys_rows;
             save_cursor_virt_row = num_virt_rows;
             found_trans = TRUE;
           }
         }

         /* if multi-line, then show all splits. If dynamic then
          * show all splits only if this is the hot split. */
         do_expand = multi_line;
         do_expand = do_expand || (dynamic && (split == find_trans_split));
         if (dynamic && !found_trans_split)
            do_expand = do_expand || (trans == find_trans);
         do_expand = do_expand && !did_expand;

         /* make sure we only expand once on dynamic */
         if (dynamic && do_expand)
           did_expand = TRUE;

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
             * except that we also have to find the saved cursor row,
             * Thus, we need a real loop over the splits.
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
                     if (on_trans_split || !found_trans_split)
                       found_split = TRUE;
                     found_trans = TRUE;
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
   split = blank_split;
   trans = xaccSplitGetParent(split);

   on_trans_split = (find_trans_split == split);
   if (on_trans_split)
     found_trans_split = TRUE;

   if (split != NULL) {
      /* lets determine where to locate the cursor ... */
      if (!found_split && (split == find_split)) {
         save_cursor_phys_row = num_phys_rows;
         save_cursor_virt_row = num_virt_rows;
         if (on_trans_split || !found_trans_split)
           found_split = TRUE;
         found_trans = TRUE;
         on_blank_split = TRUE;
      }
      else if (!found_split && (trans == find_trans)) {
         save_cursor_phys_row = num_phys_rows;
         save_cursor_virt_row = num_virt_rows;
         found_trans = TRUE;
         on_blank_split = TRUE;
      }
   }

   if (multi_line || (dynamic && info->blank_split_edited)) {
      if (multi_line || (dynamic && on_blank_split)) {
        Split *secondary;
        int j;

        /* The code below is copied from the main loop above */

        /* add one row for a transaction */
        num_virt_rows ++;
        num_phys_rows += reg->trans_cursor->numRows; 

        /* add in the splits */
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
                if (on_trans_split || !found_trans_split)
                  found_split = TRUE;
                found_trans = TRUE;
              }
            }

            num_virt_rows ++;
            num_phys_rows += reg->split_cursor->numRows; 
          }
          j++;
        } while (secondary);
      }
      else {
        num_virt_rows += 1;
        num_phys_rows += reg->trans_cursor->numRows;
      }
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

   if (ext_found_trans != NULL)
     *ext_found_trans = found_trans;
   if (ext_found_split != NULL)
     *ext_found_split = found_split;
   if (ext_found_trans_split != NULL)
     *ext_found_trans_split = found_trans_split;
   if (ext_on_blank_split != NULL)
     *ext_on_blank_split = on_blank_split;
}

/* ======================================================== */

void
xaccSRLoadRegister (SplitRegister *reg, Split **slist, 
                    Account *default_source_acc)
{
   SRInfo *info = xaccSRGetInfo(reg);
   Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
   Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
   SplitRegisterBuffer *reg_buffer;
   CellBlock *lead_cursor;
   Transaction *find_trans;
   Split *last_split = NULL;
   Split *find_trans_split;
   Split *find_split;
   Split *split;
   Table *table;

   gboolean found_pending = FALSE;
   gboolean found_split = FALSE;
   gboolean found_trans = FALSE;
   gboolean found_trans_split = FALSE;
   gboolean did_expand = FALSE;
   gboolean on_blank_split;
   gboolean multi_line;
   gboolean dynamic;

   SplitRegisterType type;
   SplitRegisterStyle style;
   guint32 changed;
   int save_phys_col;
   int phys_row;
   int vrow;
   int i;

   xaccSplitRegisterConfigColors (reg);

   /* make sure we have a blank split */
   if (blank_split == NULL) {
     Transaction *trans;

     trans = xaccMallocTransaction ();

     xaccTransBeginEdit (trans, TRUE);
     xaccTransSetDateSecs(trans, info->last_date_entered);
     xaccTransCommitEdit (trans);

     blank_split = xaccTransGetSplit (trans, 0);
     info->blank_split_guid = *xaccSplitGetGUID (blank_split);

     info->blank_split_edited = FALSE;
   }

   info->default_source_account = default_source_acc;

   table = reg->table;
   type  = reg->type;
   style = reg->style;
   multi_line  = (REG_MULTI_LINE == style);
   dynamic = ((REG_SINGLE_DYNAMIC == style) || (REG_DOUBLE_DYNAMIC == style));
   if ((REG_SINGLE_LINE == style) ||
       (REG_SINGLE_DYNAMIC == style)) {
      lead_cursor = reg->single_cursor;
   } else {
      lead_cursor = reg->double_cursor;
   }

   /* figure out where we are going to. */
   find_trans = info->cursor_hint_trans;
   find_split = info->cursor_hint_split;
   find_trans_split = info->cursor_hint_trans_split;

   if (info->cursor_hint_phys_col < 0)
     save_phys_col = table->current_cursor_phys_col;
   else
     save_phys_col = info->cursor_hint_phys_col;

   /* paranoia */
   if (save_phys_col < 0)
     save_phys_col = 0;
   if (save_phys_col >= table->num_phys_cols)
     save_phys_col = table->num_phys_cols - 1;

   /* count the number of rows, looking for the place we want to go. */
   xaccSRCountRows (reg, slist,
                    find_trans, find_split, find_trans_split,
                    &found_trans, &found_split, &found_trans_split,
                    &on_blank_split);

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
   gnc_table_move_cursor_gui (table, -1, -1);

   /* resize the table to the sizes we just counted above */
   /* num_virt_cols is always one. */
   gnc_table_set_size (table, reg->num_phys_rows, reg->num_cols, 
                       reg->num_virt_rows, 1);

   /* make sure that the header is loaded */
   gnc_table_set_cursor (table, reg->header, 0, 0, 0, 0);

   PINFO ("load register of %d phys rows ----------- \n", reg->num_phys_rows);

   /* populate the table */
   i=0;
   vrow = 1;   /* header is vrow zero */
   phys_row = reg->header->numRows;

   if (slist)
     split = slist[0]; 
   else
     split = NULL;

   while (split) {

     if (pending_trans == xaccSplitGetParent (split))
       found_pending = TRUE;

      /* do not load the blank split */
      if (split != blank_split) {
         Transaction *trans;
         gboolean do_expand;

         PINFO ("load trans %d at phys row %d \n", i, phys_row);

         trans = xaccSplitGetParent (split);

         /* if multi-line, then show all splits. If dynamic then
          * show all splits only if this is the hot split. */
         do_expand = multi_line;
         do_expand = do_expand || (dynamic && (split == find_trans_split));
         if (dynamic && !found_trans_split)
            do_expand = do_expand || (trans == find_trans);

         if (dynamic && !found_trans && !found_trans_split &&
             (vrow == reg->cursor_virt_row)) {
           reg->cursor_phys_row = phys_row;
           do_expand = TRUE;
         }

         /* make sure we only expand once on dynamic */
         do_expand = do_expand && !did_expand;

         if (dynamic && do_expand)
           did_expand = TRUE;

         if (do_expand) 
         {
            Split * secondary;
            int j = 0;

            gnc_table_set_cursor (table, reg->trans_cursor,
                                  phys_row, 0, vrow, 0);
            gnc_table_move_cursor (table, phys_row, 0);
            xaccSRLoadRegEntry (reg, split);
            vrow ++;
            phys_row += reg->trans_cursor->numRows; 

            /* loop over all of the splits in the transaction. The
             * do..while will automatically put a blank (null) split
             * at the end. */
            j = 0;
            do {
               secondary = xaccTransGetSplit (trans, j);

               if (secondary != split) {
                  gnc_table_set_cursor (table, reg->split_cursor,
                                        phys_row, 0, vrow, 0);
                  gnc_table_move_cursor (table, phys_row, 0);
                  xaccSRLoadRegEntry (reg, secondary);
                  PINFO ("load split %d at phys row %d addr=%p \n", 
                          j, phys_row, secondary);
                  vrow ++;
                  phys_row += reg->split_cursor->numRows; 
               }

               j++;
            } while (secondary);

         } else {
            /* the simple case ... */
            gnc_table_set_cursor (table, lead_cursor, phys_row, 0, vrow, 0);
            gnc_table_move_cursor (table, phys_row, 0);
            xaccSRLoadRegEntry (reg, split);
            vrow ++;
            phys_row += lead_cursor->numRows; 
         }
      }
      else {
        PINFO ("skip trans %d (blank split) \n", i);
      }

      last_split = split;
      i++; 
      split = slist[i];
   }

   /* add the blank split at the end. */
   split = blank_split;
   if (pending_trans == xaccSplitGetParent(split))
     found_pending = TRUE;

   if (multi_line || (dynamic && info->blank_split_edited)) {
      /* do the transaction row of the blank split */
      gnc_table_set_cursor (table, reg->trans_cursor, phys_row, 0, vrow, 0);
      gnc_table_move_cursor (table, phys_row, 0);
      xaccSRLoadRegEntry (reg, split);
      vrow ++;
      phys_row += reg->trans_cursor->numRows; 

      if (multi_line || (dynamic && on_blank_split)) {
        Transaction *trans;
        Split *secondary;
        int j;

        trans = xaccSplitGetParent (split);
        j = 0;
        do {
          secondary = xaccTransGetSplit (trans, j);

          if (secondary != split) {
            gnc_table_set_cursor (table, reg->split_cursor,
                                  phys_row, 0, vrow, 0);
            gnc_table_move_cursor (table, phys_row, 0);
            xaccSRLoadRegEntry (reg, secondary);
            PINFO ("load split %d at phys row %d addr=%p \n", 
                   j, phys_row, secondary);
            vrow ++;
            phys_row += reg->split_cursor->numRows; 
          }

          j++;
        } while (secondary);
      }
   } else {
      gnc_table_set_cursor (table, lead_cursor, phys_row, 0, vrow, 0);
      gnc_table_move_cursor (table, phys_row, 0);
      xaccSRLoadRegEntry (reg, split);
      vrow ++;
      phys_row += lead_cursor->numRows; 
   }

   /* restore the cursor to its rightful position */
   {
     int row = reg->cursor_phys_row;
     int col = save_phys_col;

     if (gnc_table_find_valid_cell_horiz(table, &row, &col, FALSE))
     {
       gnc_table_move_cursor_gui(table, row, col);
       reg->cursor_phys_row = row;

       if (reg_buffer != NULL)
       {
         xaccSplitRegisterRestoreCursorChanged(reg, reg_buffer);
         gnc_table_commit_cursor (table);
       }
     }

     if (reg_buffer != NULL)
     {
       xaccDestroySplitRegisterBuffer(reg_buffer);
       reg_buffer = NULL;
     }
   }

   /* If we didn't find the pending transaction, it was removed
    * from the account. */
   if (!found_pending)
   {
     if (xaccTransIsOpen(pending_trans))
       xaccTransCommitEdit(pending_trans);

     info->pending_trans_guid = *xaccGUIDNULL();
     pending_trans = NULL;
   }

   /* Set up the hint transaction, split, transaction split, and column. */
   info->cursor_hint_trans = xaccSRGetCurrentTrans (reg);
   info->cursor_hint_split = xaccSRGetCurrentSplit (reg);
   info->cursor_hint_trans_split = xaccSRGetCurrentTransSplit (reg);
   info->cursor_hint_phys_col = -1;
   info->hint_set_by_traverse = FALSE;
   info->exact_traversal = FALSE;

   gnc_table_refresh_gui (table);

   /* set the completion character for the xfer cells */
   xaccComboCellSetCompleteChar (reg->mxfrmCell, account_separator);
   xaccComboCellSetCompleteChar (reg->xfrmCell, account_separator);
   xaccComboCellSetCompleteChar (reg->xtoCell, account_separator);

   /* enable callback for cursor user-driven moves */
   table->move_cursor = LedgerMoveCursor;
   table->traverse = LedgerTraverse;
   table->set_help = LedgerSetHelp;
   table->user_data = reg;

   reg->destroy = LedgerDestroy;
}

/* ======================================================== */
/* walk account tree recursively, pulling out all the names */

static void 
LoadXferCell (ComboCell *cell,  
              AccountGroup *grp,
              const char *base_currency,
              const char *base_security)
{
  gboolean load_everything;
  Account * acc;
  char *name;
  int n;

  ENTER ("LoadXferCell()\n");

  if (!grp) return;

  load_everything = ((base_currency == NULL) && (base_security == NULL));

  /* Build the xfer menu out of account names.
   * Traverse sub-accounts recursively.
   * Valid transfers can occur only between accounts
   * with the same base currency.
   */
  n = 0;
  acc = xaccGroupGetAccount (grp, n);
  while (acc) {
    const char *curr, *secu;

    curr = xaccAccountGetCurrency (acc);
    secu = xaccAccountGetSecurity (acc);
    if (secu && (0x0 == secu[0])) secu = NULL;

    DEBUG ("curr=%s secu=%s acct=%s\n", 
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
  const char *curr, *secu;

  curr = xaccAccountGetCurrency (base_account);
  secu = xaccAccountGetSecurity (base_account);

  if ((secu != NULL) && (secu[0] == 0))
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

/* ======================================================== */

gboolean
xaccSRHasPendingChanges (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
  guint32 changed;
  
  if (reg == NULL)
    return FALSE;

  changed = xaccSplitRegisterGetChangeFlag (reg);
  if (changed)
    return TRUE;

  return xaccTransIsOpen(pending_trans);
}

/* =======================  end of file =================== */
