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

#include "config.h"

#include <glib.h>
#include <guile/gh.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "Account.h"
#include "AccWindow.h"
#include "FileDialog.h"
#include "MultiLedger.h"
#include "Refresh.h"
#include "Scrub.h"
#include "SplitLedger.h"
#include "global-options.h"
#include "gnc-engine-util.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "guile-util.h"
#include "messages.h"
#include "splitreg.h"
#include "table-allgui.h"


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

  /* Used to remember where to put the cursor */
  CursorClass cursor_hint_cursor_class;

  /* If the hints were set by the traverse callback */
  gboolean hint_set_by_traverse;

  /* If traverse is to the newly created split */
  gboolean traverse_to_new;

  /* A flag indicating if the last traversal was 'exact'.
   * See table-allgui.[ch] for details. */
  gboolean exact_traversal;

  /* Indicates that the current transaction is expanded
   * in ledger mode. Meaningless in other modes. */
  gboolean trans_expanded;

  /* The default account where new splits are added */
  Account *default_source_account;

  /* The last date recorded in the blank split */
  time_t last_date_entered;

  /* true if the current blank split has been edited and commited */
  gboolean blank_split_edited;

  /* true if the demarcation between 'past' and 'future' transactions
   * should be visible */
  gboolean show_present_divider;

  /* true if we are loading the register for the first time */
  gboolean first_pass;

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
static CursorClass copied_class = CURSOR_CLASS_NONE;
static SCM copied_item = SCM_UNDEFINED;
static GUID copied_leader_guid;

/* Flag for determining colorization of negative amounts. */
static gboolean use_red_for_negative = TRUE;


/** static prototypes *****************************************************/

static Split * xaccSRGetTransSplit (SplitRegister *reg,
                                    VirtualCellLocation vcell_loc,
                                    VirtualCellLocation *trans_split_loc);
static gboolean xaccSRSaveRegEntryToSCM (SplitRegister *reg,
                                         SCM trans_scm, SCM split_scm,
                                         gboolean use_cut_semantics);
static Transaction * xaccSRGetTrans (SplitRegister *reg,
                                     VirtualCellLocation vcell_loc);
static Split * xaccSRGetCurrentTransSplit (SplitRegister *reg,
                                           VirtualCellLocation *vcell_loc);
static GList * xaccSRSaveChangedCells (SplitRegister *reg, Transaction *trans,
                                       Split *split);
static gboolean xaccSRFindSplit (SplitRegister *reg,
                                 Transaction *trans, Split *trans_split,
                                 Split *split, CursorClass cursor_class,
                                 VirtualCellLocation *vcell_loc);
static Split * sr_get_split (SplitRegister *reg,
                             VirtualCellLocation vcell_loc);
static void xaccSRSetTransVisible (SplitRegister *reg,
                                   VirtualCellLocation vcell_loc,
                                   gboolean visible,
                                   gboolean only_blank_split);


/** implementations *******************************************************/

static time_t
get_today_midnight (void)
{
  time_t present;
  struct tm tm;

  present = time (NULL);

  tm = *localtime (&present);

  tm.tm_sec  = 0;
  tm.tm_min  = 0;
  tm.tm_hour = 0;

  return mktime (&tm);
}

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

  info->blank_split_guid = *xaccGUIDNULL ();
  info->pending_trans_guid = *xaccGUIDNULL ();
  info->last_date_entered = get_today_midnight ();
  info->first_pass = TRUE;

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
  if (!reg)
    return NULL;

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

  return info->get_parent(info->user_data);
}

void
xaccSRSetData(SplitRegister *reg, void *user_data,
              SRGetParentCallback get_parent,
              SRSetHelpCallback set_help)
{
  SRInfo *info = xaccSRGetInfo(reg);

  g_return_if_fail (reg != NULL);

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
  GList *node;

  if (trans == NULL)
    return accounts;

  for (node = xaccTransGetSplitList (trans); node; node = node->next)
  {
    Split *split = node->data;
    Account *account;

    account = xaccSplitGetAccount(split);
    if (account != NULL)
      accounts = g_list_prepend(accounts, account);
  }

  return accounts;
}

static GList *
gnc_trans_prepend_split_list(Transaction *trans, GList *splits)
{
  GList *node;

  if (trans == NULL)
    return splits;

  for (node = xaccTransGetSplitList (trans); node; node = node->next)
  {
    Split *split = node->data;

    if (split == NULL)
      return splits;

    splits = g_list_prepend(splits, split);
  }

  return splits;
}

static int
gnc_trans_split_index(Transaction *trans, Split *split)
{
  GList *node;
  int i;

  for (i = 0, node = xaccTransGetSplitList (trans); node;
       i++, node = node->next)
  {
    Split *s = node->data;

    if (s == split)
      return i;
  }

  return -1;
}

/* Uses the scheme split copying routines */
static void
gnc_copy_split_onto_split(Split *from, Split *to, gboolean use_cut_semantics)
{
  SCM split_scm;

  if ((from == NULL) || (to == NULL))
    return;

  split_scm = gnc_copy_split(from, use_cut_semantics);
  if (split_scm == SCM_UNDEFINED)
    return;

  gnc_copy_split_scm_onto_split(split_scm, to);
}

/* Uses the scheme transaction copying routines */
static void
gnc_copy_trans_onto_trans(Transaction *from, Transaction *to,
                          gboolean use_cut_semantics,
                          gboolean do_commit)
{
  SCM trans_scm;

  if ((from == NULL) || (to == NULL))
    return;

  trans_scm = gnc_copy_trans(from, use_cut_semantics);
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

    if (unit_price)
    {
      gnc_numeric price = xaccSplitGetSharePrice(split);
      if (!gnc_numeric_equal (price, gnc_numeric_create (1, 1)))
        continue;
    }

    if (safe_strcmp(memo, xaccSplitGetMemo(split)) == 0)
    {
      Account *account = xaccSplitGetAccount(split);
      const gnc_commodity *currency, *security;

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
  GList *slp;

  if (account == NULL) return NULL;

  for(slp = g_list_last(xaccAccountGetSplitList(account));
      slp;
      slp = slp->prev)
  {
    Split *split = slp->data;
    Transaction *trans = xaccSplitGetParent(split);

    split = gnc_find_split_in_trans_by_memo(trans, memo, dest_trans,
                                            unit_price);

    if (split != NULL) return split;
  }

  return NULL;
}

/* This routine is for finding a matching transaction in an account by
 * matching on the description field. This routine is used for auto-filling
 * in registers with a default leading account. The dest_trans is a
 * transaction used for currency checking. */
static Transaction *
gnc_find_trans_in_account_by_desc(Account *account, const char *description)
{
  GList *slp;

  if (account == NULL) return NULL;

  for(slp = g_list_last(xaccAccountGetSplitList(account));
      slp;
      slp = slp->prev)
  {
    Split *split = slp->data;
    Transaction *trans = xaccSplitGetParent(split);

    if (safe_strcmp(description, xaccTransGetDescription(trans)) == 0)
      return trans;
  }

  return NULL;
}

static Split *
gnc_find_split_in_reg_by_memo(SplitRegister *reg, const char *memo,
                              Transaction *dest_tran, gboolean unit_price)
{
  int virt_row, virt_col;
  int num_rows, num_cols;
  Transaction *last_trans;

  if (reg == NULL)
    return NULL;

  if (reg->table == NULL)
    return NULL;

  num_rows = reg->table->num_virt_rows;
  num_cols = reg->table->num_virt_cols;

  last_trans = NULL;

  for (virt_row = num_rows - 1; virt_row >= 0; virt_row--)
    for (virt_col = num_cols - 1; virt_col >= 0; virt_col--)
    {
      Split *split;
      Transaction *trans;
      VirtualCellLocation vcell_loc = { virt_row, virt_col };

      split = sr_get_split (reg, vcell_loc);
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
  int virt_row, virt_col;
  int num_rows, num_cols;
  Transaction *last_trans;

  if (reg == NULL)
    return NULL;

  if (reg->table == NULL)
    return NULL;

  num_rows = reg->table->num_virt_rows;
  num_cols = reg->table->num_virt_cols;

  last_trans = NULL;

  for (virt_row = num_rows - 1; virt_row >= 0; virt_row--)
    for (virt_col = num_cols - 1; virt_col >= 0; virt_col--)
    {
      Split *split;
      Transaction *trans;
      VirtualCellLocation vcell_loc = { virt_row, virt_col };

      split = sr_get_split (reg, vcell_loc);
      trans = xaccSplitGetParent(split);

      if (trans == last_trans)
        continue;

      if (safe_strcmp(description, xaccTransGetDescription(trans)) == 0)
        return trans;

      last_trans = trans;
    }

  return NULL;
}

static Split *
sr_get_split (SplitRegister *reg, VirtualCellLocation vcell_loc)
{
  GUID *guid;

  if (reg == NULL)
    return NULL;

  guid = gnc_table_get_vcell_data (reg->table, vcell_loc);
  if (guid == NULL)
    return NULL;

  return xaccSplitLookup (guid);
}

static int
gnc_split_get_value_denom (Split *split)
{
  int denom;

  denom = xaccAccountGetCurrencySCU (xaccSplitGetAccount (split));
  if (denom == 0)
  {
    const gnc_commodity *commodity = gnc_locale_default_currency ();
    denom = gnc_commodity_get_fraction (commodity);
    if (denom == 0)
      denom = 100;
  }

  return denom;
}

static int
gnc_split_get_quantity_denom (Split *split)
{
  int denom;

  denom = xaccAccountGetSecuritySCU (xaccSplitGetAccount (split));
  if (denom == 0)
  {
    const gnc_commodity *commodity = gnc_locale_default_currency ();
    denom = gnc_commodity_get_fraction (commodity);
    if (denom == 0)
      denom = 100;
  }

  return denom;
}

static void
sr_set_cell_fractions (SplitRegister *reg, Split *split)
{
  SRInfo *info = xaccSRGetInfo (reg);
  Account *account;

  account = xaccSplitGetAccount (split);

  if (account == NULL)
    account = info->default_source_account;

  if (account)
  {
    xaccSetPriceCellFraction (reg->sharesCell,
                              xaccAccountGetSecuritySCU (account));
    xaccSetPriceCellFraction (reg->debitCell,
                              xaccAccountGetCurrencySCU (account));
    xaccSetPriceCellFraction (reg->creditCell,
                              xaccAccountGetCurrencySCU (account));

    return;
  }

  {
    const gnc_commodity *commodity;
    int fraction;

    xaccSetPriceCellFraction (reg->sharesCell, 10000);

    commodity = gnc_locale_default_currency ();
    fraction = gnc_commodity_get_fraction (commodity);

    xaccSetPriceCellFraction (reg->debitCell, fraction);
    xaccSetPriceCellFraction (reg->creditCell, fraction);
  }
}

static CellBlock *
sr_get_passive_cursor (SplitRegister *reg)
{
  switch (reg->style)
  {
    case REG_STYLE_LEDGER:
    case REG_STYLE_AUTO_LEDGER:
      return reg->use_double_line ?
        reg->cursor_ledger_double : reg->cursor_ledger_single;

    case REG_STYLE_JOURNAL:
      return reg->use_double_line ?
        reg->cursor_journal_double : reg->cursor_journal_single;
  }

  PWARN ("bad register style");

  return NULL;
}

static CellBlock *
sr_get_active_cursor (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo (reg);

  switch (reg->style)
  {
    case REG_STYLE_LEDGER:
      if (!info->trans_expanded)
        return reg->use_double_line ?
          reg->cursor_ledger_double : reg->cursor_ledger_single;

      /* fall through */
    case REG_STYLE_AUTO_LEDGER:
    case REG_STYLE_JOURNAL:
      return reg->use_double_line ?
        reg->cursor_journal_double : reg->cursor_journal_single;
  }

  PWARN ("bad register style");

  return NULL;
}

void
xaccSRExpandCurrentTrans (SplitRegister *reg, gboolean expand)
{
  SRInfo *info = xaccSRGetInfo (reg);

  if (!reg)
    return;

  if (reg->style == REG_STYLE_AUTO_LEDGER ||
      reg->style == REG_STYLE_JOURNAL)
    return;

  /* ok, so I just wanted an excuse to use exclusive-or */
  if (!(expand ^ info->trans_expanded))
    return;

  if (!expand)
  {
    VirtualLocation virt_loc;

    virt_loc = reg->table->current_cursor_loc;
    xaccSRGetTransSplit (reg, virt_loc.vcell_loc, &virt_loc.vcell_loc);

    if (gnc_table_find_close_valid_cell (reg->table, &virt_loc, FALSE))
      gnc_table_move_cursor_gui (reg->table, virt_loc);
    else
    {
      PERR ("Can't find place to go!");
      return;
    }
  }

  xaccSRSetTransVisible (reg, reg->table->current_cursor_loc.vcell_loc,
                         expand, FALSE);

  gnc_table_refresh_gui (reg->table);

  info->trans_expanded = expand;
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
 *  xaccVerifyCursorPosition()
 *    doMoveCursor()
 *      callback for move() (i.e., LedgerMoveCursor)
 *        xaccSRSaveRegEntry()
 *        RedrawRegEntry()
 *          SRLoadRegister()
 *            xaccMoveCursor()
 */
static void
LedgerMoveCursor (Table *table, VirtualLocation *p_new_virt_loc)
{
  VirtualLocation new_virt_loc = *p_new_virt_loc;
  VirtualCellLocation old_trans_split_loc;
  SplitRegister *reg = table->user_data;
  SRInfo *info = xaccSRGetInfo(reg);
  Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
  Transaction *new_trans;
  Transaction *old_trans;
  Split *old_trans_split;
  Split *new_trans_split;
  Split *new_split;
  CursorClass new_class;
  CursorClass old_class;
  gboolean exact_traversal;
  gboolean do_refresh;
  gboolean saved;

  PINFO ("start callback %d %d \n",
         new_virt_loc.vcell_loc.virt_row,
         new_virt_loc.vcell_loc.virt_col);

  /* The transaction we are coming from */
  old_trans = xaccSRGetCurrentTrans (reg);
  old_trans_split = xaccSRGetCurrentTransSplit (reg, &old_trans_split_loc);
  old_class = xaccSplitRegisterGetCurrentCursorClass (reg);

  exact_traversal = info->exact_traversal;

  if (info->traverse_to_new)
  {
    if (old_class == CURSOR_CLASS_SPLIT)
      new_trans = old_trans;
    else
      new_trans = NULL;

    new_split = NULL;
    new_trans_split = NULL;
    new_class = CURSOR_CLASS_NONE;
  }
  else if (!info->hint_set_by_traverse)
  {
    /* The transaction where we are moving to */
    new_trans = xaccSRGetTrans(reg, new_virt_loc.vcell_loc);

    /* The split we are moving to */
    new_split = sr_get_split(reg, new_virt_loc.vcell_loc);

    /* The split at the transaction line we are moving to */
    new_trans_split = xaccSRGetTransSplit(reg, new_virt_loc.vcell_loc, NULL);

    new_class = xaccSplitRegisterGetCursorClass (reg, new_virt_loc.vcell_loc);
  }
  else
  {
    new_trans = info->cursor_hint_trans;
    new_split = info->cursor_hint_split;
    new_trans_split = info->cursor_hint_trans_split;
    new_class = info->cursor_hint_cursor_class;
  }

  info->hint_set_by_traverse = FALSE;

  /* commit the contents of the cursor into the database */
  saved = xaccSRSaveRegEntry (reg, old_trans != new_trans);
  if ((pending_trans != NULL)      &&
      (pending_trans == old_trans) &&
      (old_trans != new_trans))
  {
    if (xaccTransIsOpen (old_trans))
      xaccTransCommitEdit (old_trans);
    info->pending_trans_guid = *xaccGUIDNULL();
    pending_trans = NULL;
    saved = TRUE;
  }

  /* redrawing the register can muck everything up */
  if (saved)
  {
    VirtualCellLocation vcell_loc;

    info->cursor_hint_trans = new_trans;
    info->cursor_hint_split = new_split;
    info->cursor_hint_trans_split = new_trans_split;
    info->cursor_hint_cursor_class = new_class;

    xaccSRRedrawRegEntry (reg);

    /* if the split we were going to is still in the register,
     * then it may have moved. Find out where it is now. */
    if (xaccSRFindSplit (reg, new_trans, new_trans_split,
                         new_split, new_class, &vcell_loc))
    {
      VirtualCell *vcell;

      vcell = gnc_table_get_virtual_cell (table, vcell_loc);

      new_virt_loc.vcell_loc = vcell_loc;
    }
    else
      new_virt_loc.vcell_loc = reg->table->current_cursor_loc.vcell_loc;
  }
  else if (info->traverse_to_new)
  {
    new_trans = info->cursor_hint_trans;
    new_split = info->cursor_hint_split;
    new_trans_split = info->cursor_hint_trans_split;
    new_class = info->cursor_hint_cursor_class;
  }

  gnc_table_find_close_valid_cell (table, &new_virt_loc, exact_traversal);

  *p_new_virt_loc = new_virt_loc;

  PINFO ("after move %d %d \n",
         new_virt_loc.vcell_loc.virt_row,
         new_virt_loc.vcell_loc.virt_col);

  /* if the register was reloaded, then everything should be fine :)
   * otherwise, we may need to change some visibility settings. */
  if (saved)
  {
    sr_set_cell_fractions (reg, new_split);
    return;
  }

  /* in the mult-line and dynamic modes, we need to hide the old
   * and show the new. */
  if (((REG_STYLE_AUTO_LEDGER == reg->style) ||
       (REG_STYLE_JOURNAL     == reg->style)) &&
      (old_trans_split != new_trans_split))
  {
    VirtualCellLocation vc_loc;

    vc_loc = old_trans_split_loc;
    gnc_table_set_virt_cell_cursor (table, vc_loc,
                                    sr_get_passive_cursor (reg));
    xaccSRSetTransVisible (reg, vc_loc, FALSE,
                           reg->style == REG_STYLE_JOURNAL);

    xaccSRGetTransSplit (reg, new_virt_loc.vcell_loc, &vc_loc);
    gnc_table_set_virt_cell_cursor (table, vc_loc,
                                    sr_get_active_cursor (reg));
    xaccSRSetTransVisible (reg, vc_loc, TRUE,
                           reg->style == REG_STYLE_JOURNAL);

    do_refresh = TRUE;
  }
  else
    do_refresh = FALSE;

  info->cursor_hint_trans = new_trans;
  info->cursor_hint_split = new_split;
  info->cursor_hint_trans_split = new_trans_split;
  info->cursor_hint_cursor_class = new_class;

  sr_set_cell_fractions (reg, new_split);

  gnc_table_find_close_valid_cell (table, p_new_virt_loc, exact_traversal);

  if (do_refresh)
  {
    gnc_table_refresh_gui (table);
    gnc_table_leave_update (table, table->current_cursor_loc);
  }
}

/* This function determines if auto-completion is appropriate and,
 * if so, performs it. This should only be called by LedgerTraverse. */
static gboolean
LedgerAutoCompletion(SplitRegister *reg, gncTableTraversalDir dir,
                     VirtualLocation *p_new_virt_loc)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  Transaction *blank_trans = xaccSplitGetParent (blank_split);
  VirtualLocation new_virt_loc;
  CursorClass cursor_class;
  CellType cell_type;
  Transaction *trans;
  gnc_numeric amount;
  guint32 changed;
  Split *split;

  /* auto-completion is only triggered by a tab out */
  if (dir != GNC_TABLE_TRAVERSE_RIGHT)
    return FALSE;

  split = xaccSRGetCurrentSplit(reg);
  trans = xaccSRGetCurrentTrans(reg);
  if (trans == NULL)
    return FALSE;

  cursor_class = xaccSplitRegisterGetCurrentCursorClass (reg);
  cell_type = xaccSplitRegisterGetCurrentCellType (reg);
  changed = xaccSplitRegisterGetChangeFlag (reg);
  changed |= xaccSplitRegisterGetConditionalChangeFlag (reg);

  switch (cursor_class)
  {
    case CURSOR_CLASS_TRANS: {
      Transaction *auto_trans;
      GList *refresh_accounts;
      char *desc;

      /* there must be a blank transaction * */
      if (blank_trans == NULL)
        return FALSE;

      /* we must be on the blank split */
      if (trans != blank_trans)
        return FALSE;

      /* and leaving the description cell */
      if (cell_type != DESC_CELL)
        return FALSE;

      /* nothing but the date, num, and description should be changed */
      if ((changed & ~(MOD_DATE | MOD_NUM | MOD_DESC)) != 0)
        return FALSE;

      /* and the description should be changed */
      if ((changed & MOD_DESC) == 0)
        return FALSE;

      /* to a non-empty value */
      desc = reg->descCell->cell.value;
      if ((desc == NULL) || (*desc == '\0'))
        return FALSE;

      /* find a transaction to auto-complete on */
      if (info->default_source_account != NULL)
      {
        Account *account = info->default_source_account;

        auto_trans = gnc_find_trans_in_account_by_desc(account, desc);
      }
      else
        auto_trans = gnc_find_trans_in_reg_by_desc(reg, desc);

      if (auto_trans == NULL)
        return FALSE;

      xaccTransBeginEdit (trans, TRUE);
      gnc_copy_trans_onto_trans (auto_trans, trans, FALSE, FALSE);

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
      cell_type = (gnc_numeric_negative_p (amount)) ? CRED_CELL : DEBT_CELL;

      if (xaccSplitRegisterGetCurrentCellLoc (reg, cell_type, &new_virt_loc))
        *p_new_virt_loc = new_virt_loc;
    }

    break;

    case CURSOR_CLASS_SPLIT: {
      char *memo, *fullname;
      gboolean unit_price;
      Split *auto_split;

      /* we must be on a blank split of a transaction */
      if (split != NULL)
        return FALSE;

      /* and leaving the memo cell */
      if (cell_type != MEMO_CELL)
        return FALSE;

      /* nothing but the action memo, and amounts should be changed */
      if ((changed & ~(MOD_ACTN | MOD_MEMO | MOD_AMNT)) != 0)
        return FALSE;

      /* and the memo should be changed */
      if ((changed & MOD_MEMO) == 0)
        return FALSE;

      /* to a non-empty value */
      memo = reg->memoCell->cell.value;
      if ((memo == NULL) || (*memo == '\0'))
        return FALSE;

      /* if there is no price field, only auto-complete from splits with
       * a unit share price. */
      unit_price = !xaccSplitRegisterGetCurrentCellLoc(reg, PRIC_CELL, NULL);

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
        return FALSE;

      /* the auto-complete code below is taken from xaccSRGetEntryHandler */

      /* auto-complete the action field if it wasn't changed */
      if (!(MOD_ACTN & changed))
        xaccSetComboCellValue (reg->actionCell,
                               xaccSplitGetAction (auto_split));

      /* auto-complete the account name */
      fullname = xaccAccountGetFullName (xaccSplitGetAccount (auto_split),
                                         account_separator);
      xaccSetComboCellValue (reg->xfrmCell, fullname);
      g_free(fullname);

      xaccBasicCellSetChanged(&(reg->xfrmCell->cell), TRUE);

      if (!(changed & MOD_AMNT))
      {
        amount = xaccSplitGetValue (auto_split);

        xaccSetDebCredCellValue (reg->debitCell, reg->creditCell, amount);
        xaccBasicCellSetChanged (&(reg->debitCell->cell), TRUE);
        xaccBasicCellSetChanged (&(reg->creditCell->cell), TRUE);
      }

      /* and refresh the gui */
      gnc_table_refresh_gui (reg->table);

      /* now move to the non-empty amount column */
      amount = xaccSplitGetShareAmount (auto_split);
      cell_type = (gnc_numeric_negative_p (amount)) ? CRED_CELL : DEBT_CELL;

      if (xaccSplitRegisterGetCurrentCellLoc (reg, cell_type, &new_virt_loc))
        *p_new_virt_loc = new_virt_loc;
    }

    break;

    default:
      break;
  }

  return TRUE;
}

/* ======================================================== */
/* This callback gets called when the user clicks on the gui
 * in such a way as to leave the current transaction, and to 
 * go to a new one. It is called to verify what the coordinates
 * of the new cell will be.
 */

static gboolean
LedgerTraverse (Table *table,
                VirtualLocation *p_new_virt_loc,
                gncTableTraversalDir dir)
{
  SplitRegister *reg = table->user_data;
  SRInfo *info = xaccSRGetInfo(reg);
  Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
  VirtualLocation virt_loc = *p_new_virt_loc;
  Transaction *trans, *new_trans;
  GNCVerifyResult result;
  guint32 changed;
  Split *split;

  info->exact_traversal = (dir == GNC_TABLE_TRAVERSE_POINTER);

  split = xaccSRGetCurrentSplit (reg);
  trans = xaccSRGetCurrentTrans (reg);
  if (trans == NULL)
    return FALSE;

  /* no changes, make sure we aren't going off the end */
  changed = xaccSplitRegisterGetChangeFlag (reg);
  if (!changed && (pending_trans != trans))
  {
    gnc_table_find_close_valid_cell (table, &virt_loc, info->exact_traversal);

    *p_new_virt_loc = virt_loc;

    return FALSE;
  }

  /* See if we are leaving an account field */
  do
  {
    CellType cell_type;
    ComboCell *cell;
    Account *account;
    char *name;

    cell_type = xaccSplitRegisterGetCurrentCellType (reg);

    if (!(cell_type == XFRM_CELL ||
          cell_type == XTO_CELL  ||
          cell_type == MXFRM_CELL))
      break;

    cell = NULL;

    switch (cell_type)
    {
      case XFRM_CELL:
        if (changed & MOD_XFRM)
          cell = reg->xfrmCell;
        break;
      case XTO_CELL:
        if (changed & MOD_XTO)
          cell = reg->xtoCell;
        break;
      case MXFRM_CELL:
        if (changed & MOD_MXFRM)
          cell = reg->mxfrmCell;
        break;
      default:
        break;
    }

    if (!cell)
      break;

    name = cell->cell.value;
    if (!name || *name == '\0')
      break;

    account = xaccGetAccountFromFullName (gncGetCurrentGroup (),
                                          cell->cell.value,
                                          account_separator);
    if (account)
      break;

    {
      const char *format = _("The account %s does not exist.\n"
                             "Would you like to create it?");
      char *message;
      gboolean result;

      message = g_strdup_printf (format, name);

      result = gnc_verify_dialog_parented (xaccSRGetParent (reg),
                                           message, TRUE);
      if (!result)
        break;
    }

    account = gnc_ui_new_accounts_from_name_window (name);
    if (!account)
      break;

    name = xaccAccountGetFullName (account, account_separator);
    xaccSetComboCellValue (cell, name);
    xaccBasicCellSetChanged (&cell->cell, TRUE);
    g_free (name);

  } while (FALSE);

  /* See if we are tabbing off the end of the very last line */
  do
  {
    VirtualLocation virt_loc;

    if (!changed && !info->blank_split_edited)
      break;

    if (dir != GNC_TABLE_TRAVERSE_RIGHT)
      break;

    virt_loc = table->current_cursor_loc;
    if (gnc_table_move_vertical_position (table, &virt_loc, 1))
      break;

    virt_loc = table->current_cursor_loc;
    if (gnc_table_move_tab (table, &virt_loc, TRUE))
      break;

    *p_new_virt_loc = table->current_cursor_loc;
    (p_new_virt_loc->vcell_loc.virt_row)++;
    p_new_virt_loc->phys_row_offset = 0;
    p_new_virt_loc->phys_col_offset = 0;

    info->traverse_to_new = TRUE;

    return FALSE;

  } while (FALSE);

  /* Now see if we are changing cursors. If not, we may be able to
   * auto-complete. */
  if (!gnc_table_virtual_cell_out_of_bounds (table, virt_loc.vcell_loc))
  {
    if (LedgerAutoCompletion(reg, dir, p_new_virt_loc))
      return FALSE;
  }

  /* See if we are tabbing off the end of the blank split */
  do
  {
    VirtualLocation virt_loc;

    if (!changed)
      break;

    if (split)
      break;

    if (dir != GNC_TABLE_TRAVERSE_RIGHT)
      break;

    virt_loc = table->current_cursor_loc;
    if (gnc_table_move_tab (table, &virt_loc, TRUE))
      break;

    /* If we are here, then: (a) the current cursor has been
     * edited, and (b) we are on the blank split of a multi-line
     * transaction, and (c) we are tabbing out of the last cell
     * on the line. Thus, we want to go ahead and add the new
     * split and end up on the new blank split of the current
     * transaction. */
    info->cursor_hint_trans = trans;
    info->cursor_hint_split = split;
    info->cursor_hint_trans_split = xaccSRGetCurrentTransSplit (reg, NULL);
    info->cursor_hint_cursor_class = CURSOR_CLASS_SPLIT;
    info->hint_set_by_traverse = TRUE;

    return FALSE;

  } while (FALSE);

  /* Check for going off the end */
  gnc_table_find_close_valid_cell (table, &virt_loc, info->exact_traversal);

  /* Same transaction, no problem */
  new_trans = xaccSRGetTrans(reg, virt_loc.vcell_loc);
  if (trans == new_trans)
  {
    *p_new_virt_loc = virt_loc;

    return FALSE;
  }

  /* Ok, we are changing transactions and the current transaction has
   * changed. See what the user wants to do. */
  {
    const char *message = _("The current transaction has been changed.\n"
                            "Would you like to record it?");
    result = gnc_verify_cancel_dialog_parented(xaccSRGetParent(reg),
                                               message, GNC_VERIFY_YES);
  }

  switch (result)
  {
    case GNC_VERIFY_YES:
    case GNC_VERIFY_NO:
      {
        VirtualCellLocation vcell_loc;
        Split *new_split;
        Split *trans_split;
        CursorClass new_class;

        if ((result == GNC_VERIFY_YES) && xaccSRCheckReconciled (reg))
          break;

        new_split = sr_get_split(reg, virt_loc.vcell_loc);
        trans_split = xaccSRGetTransSplit(reg, virt_loc.vcell_loc, NULL);
        new_class = xaccSplitRegisterGetCursorClass (reg, virt_loc.vcell_loc);

        xaccSRCancelCursorTransChanges(reg);

        if (xaccSRFindSplit (reg, new_trans, trans_split,
                             new_split, new_class, &vcell_loc))
          virt_loc.vcell_loc = vcell_loc;

        gnc_table_find_close_valid_cell (table, &virt_loc,
                                         info->exact_traversal);

        *p_new_virt_loc = virt_loc;
      }

      break;

    case GNC_VERIFY_CANCEL:
      return TRUE;

    default:
      break;
  }

  return FALSE;
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
xaccSRGetTrans (SplitRegister *reg, VirtualCellLocation vcell_loc)
{
  Split *split;

  if (!reg || !reg->table)
    return NULL;

  split = sr_get_split (reg, vcell_loc);

  if (split != NULL)
    return xaccSplitGetParent(split);

  /* Split is blank. Assume it is the blank split of a multi-line
   * transaction. Go back one row to find a split in the transaction. */
  vcell_loc.virt_row--;

  split = sr_get_split (reg, vcell_loc);

  /* This split could be NULL during register initialization. */
  if (split == NULL)
    return NULL;

  return xaccSplitGetParent(split);
}

/* ======================================================== */

static Split *
xaccSRGetTransSplit (SplitRegister *reg,
                     VirtualCellLocation vcell_loc,
                     VirtualCellLocation *trans_split_loc)
{
  CursorClass cursor_class;

  if (reg == NULL)
    return NULL;

  while (TRUE)
  {
    if ((0 > vcell_loc.virt_row) || (0 > vcell_loc.virt_col)) {
      PERR ("bad row \n");
      return NULL;
    }

    cursor_class = xaccSplitRegisterGetCursorClass (reg, vcell_loc);

    if (cursor_class == CURSOR_CLASS_TRANS)
    {
      if (trans_split_loc)
        *trans_split_loc = vcell_loc;

      return sr_get_split (reg, vcell_loc);
    }

    vcell_loc.virt_row--;
  }
}

/* ======================================================== */

static Split *
xaccSRGetCurrentTransSplit (SplitRegister *reg,
                            VirtualCellLocation *trans_split_loc)
{
  VirtualCellLocation vcell_loc;

  if (reg == NULL)
    return NULL;

  vcell_loc = reg->table->current_cursor_loc.vcell_loc;

  return xaccSRGetTransSplit (reg, vcell_loc, trans_split_loc);
}

/* ======================================================== */

Transaction *
xaccSRGetCurrentTrans (SplitRegister *reg)
{
  Split *split;
  VirtualCellLocation vcell_loc;

  if (reg == NULL)
    return NULL;

  split = xaccSRGetCurrentSplit (reg);
  if (split != NULL)
    return xaccSplitGetParent(split);

  /* Split is blank. Assume it is the blank split of a multi-line
   * transaction. Go back one row to find a split in the transaction. */
  vcell_loc = reg->table->current_cursor_loc.vcell_loc;

  vcell_loc.virt_row --;

  split = sr_get_split (reg, vcell_loc);

  return xaccSplitGetParent(split);
}

/* ======================================================== */

Split *
xaccSRGetCurrentSplit (SplitRegister *reg)
{
  if (reg == NULL)
    return NULL;

  return sr_get_split (reg, reg->table->current_cursor_loc.vcell_loc);
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
xaccSRGetSplitVirtLoc (SplitRegister *reg, Split *split,
                       VirtualCellLocation *vcell_loc)
{
  Table *table;
  int v_row;
  int v_col;

  if ((reg == NULL) || (split == NULL))
    return FALSE;

  table = reg->table;

  /* go backwards because typically you search for splits at the end
   * and because we find split rows before transaction rows. */

  for (v_row = table->num_virt_rows - 1; v_row > 0; v_row--)
    for (v_col = 0; v_col < table->num_virt_cols; v_col++)
    {
      VirtualCellLocation vc_loc = { v_row, v_col };
      VirtualCell *vcell;
      Split *s;

      vcell = gnc_table_get_virtual_cell (table, vc_loc);
      if (vcell == NULL)
        continue;

      if (!vcell->visible)
        continue;

      s = xaccSplitLookup (vcell->vcell_data);

      if (s == split)
      {
        if (vcell_loc)
          *vcell_loc = vc_loc;

        return TRUE;
      }
    }

  return FALSE;
}

/* ======================================================== */

gboolean
xaccSRGetSplitAmountVirtLoc (SplitRegister *reg, Split *split,
                             VirtualLocation *virt_loc)
{
  VirtualLocation v_loc;
  CursorClass cursor_class;
  CellType cell_type;
  gnc_numeric value;

  if (!xaccSRGetSplitVirtLoc (reg, split, &v_loc.vcell_loc))
    return FALSE;

  cursor_class = xaccSplitRegisterGetCursorClass (reg, v_loc.vcell_loc);

  value = xaccSplitGetValue (split);

  switch (cursor_class)
  {
    case CURSOR_CLASS_SPLIT:
    case CURSOR_CLASS_TRANS:
      cell_type = (gnc_numeric_negative_p (value)) ? CRED_CELL : DEBT_CELL;
      break;
    default:
      return FALSE;
  }

  if (!xaccSplitRegisterGetCellLoc (reg, cell_type, v_loc.vcell_loc, &v_loc))
    return FALSE;

  if (virt_loc == NULL)
    return TRUE;

  *virt_loc = v_loc;

  return TRUE;
}

/* ======================================================== */

static gboolean
xaccSRFindSplit (SplitRegister *reg,
                 Transaction *trans, Split *trans_split,
                 Split *split, CursorClass find_class,
                 VirtualCellLocation *vcell_loc)
{
  Table *table = reg->table;
  gboolean found_trans = FALSE;
  gboolean found_trans_split = FALSE;
  gboolean found_something = FALSE;
  CursorClass cursor_class;
  int v_row, v_col;
  Transaction *t;
  Split *s;

  for (v_row = 1; v_row < table->num_virt_rows; v_row++)
    for (v_col = 0; v_col < table->num_virt_cols; v_col++)
    {
      VirtualCellLocation vc_loc = { v_row, v_col };

      s = sr_get_split (reg, vc_loc);
      t = xaccSplitGetParent(s);

      cursor_class = xaccSplitRegisterGetCursorClass(reg, vc_loc);

      if (t == trans)
        found_trans = TRUE;

      if ((cursor_class == CURSOR_CLASS_TRANS) && (s == trans_split))
        found_trans_split = TRUE;

      if (found_trans && (s == split))
      {
        if (vcell_loc != NULL)
          *vcell_loc = vc_loc;

        found_something = TRUE;
      }

      if (found_trans_split && (s == split))
      {
        if (vcell_loc != NULL)
          *vcell_loc = vc_loc;

        if (cursor_class == find_class)
          return TRUE;
      }
    }

  return found_something;
}

/* ======================================================== */

static void
xaccSRSetTransVisible (SplitRegister *reg,
                       VirtualCellLocation vcell_loc,
                       gboolean visible,
                       gboolean only_blank_split)
{
  CursorClass cursor_class;

  while (TRUE)
  {
    vcell_loc.virt_row++;

    cursor_class = xaccSplitRegisterGetCursorClass (reg, vcell_loc);
    if (cursor_class != CURSOR_CLASS_SPLIT)
      return;

    if (only_blank_split && sr_get_split (reg, vcell_loc))
      continue;

    gnc_table_set_virt_cell_visible (reg->table, vcell_loc, visible);
  }
}

/* ======================================================== */

Split *
xaccSRDuplicateCurrent (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  CursorClass cursor_class;
  Transaction *trans;
  Split *return_split;
  Split *trans_split;
  guint32 changed;
  Split *split;

  split = xaccSRGetCurrentSplit(reg);
  trans = xaccSRGetCurrentTrans(reg);
  trans_split = xaccSRGetCurrentTransSplit(reg, NULL);

  /* This shouldn't happen, but be paranoid. */
  if (trans == NULL)
    return NULL;

  cursor_class = xaccSplitRegisterGetCurrentCursorClass (reg);

  /* Can't do anything with this. */
  if (cursor_class == CURSOR_CLASS_NONE)
    return NULL;

  /* This shouldn't happen, but be paranoid. */
  if ((split == NULL) && (cursor_class == CURSOR_CLASS_TRANS))
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
    const char *message = _("The current transaction has been changed.\n"
                            "Would you like to record it?");
    GNCVerifyResult result;

    result = gnc_ok_cancel_dialog_parented(xaccSRGetParent(reg),
                                           message, GNC_VERIFY_OK);

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

  if (cursor_class == CURSOR_CLASS_SPLIT)
  {
    Split *new_split;

    /* We are on a split in an expanded transaction.
     * Just copy the split and add it to the transaction. */

    new_split = xaccMallocSplit();

    xaccTransBeginEdit(trans, TRUE);
    xaccTransAppendSplit(trans, new_split);
    xaccTransCommitEdit(trans);

    gnc_copy_split_onto_split(split, new_split, FALSE);

    return_split = new_split;

    info->cursor_hint_split = new_split;
    info->cursor_hint_cursor_class = CURSOR_CLASS_SPLIT;
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

    gnc_copy_trans_onto_trans(trans, new_trans, FALSE, TRUE);

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
    info->cursor_hint_cursor_class = CURSOR_CLASS_TRANS;
  }

  /* Refresh the GUI. */
  gnc_refresh_main_window();
  gnc_transaction_ui_refresh(trans);

  return return_split;
}

/* ======================================================== */

static void
xaccSRCopyCurrentInternal (SplitRegister *reg, gboolean use_cut_semantics)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  CursorClass cursor_class;
  Transaction *trans;
  guint32 changed;
  Split *split;
  SCM new_item;

  split = xaccSRGetCurrentSplit(reg);
  trans = xaccSRGetCurrentTrans(reg);

  /* This shouldn't happen, but be paranoid. */
  if (trans == NULL)
    return;

  cursor_class = xaccSplitRegisterGetCurrentCursorClass (reg);

  /* Can't do anything with this. */
  if (cursor_class == CURSOR_CLASS_NONE)
    return;

  /* This shouldn't happen, but be paranoid. */
  if ((split == NULL) && (cursor_class == CURSOR_CLASS_TRANS))
    return;

  changed = xaccSplitRegisterGetChangeFlag(reg);

  /* See if we were asked to copy an unchanged blank split. Don't. */
  if (!changed && ((split == NULL) || (split == blank_split)))
    return;

  /* Ok, we are now ready to make the copy. */

  if (cursor_class == CURSOR_CLASS_SPLIT)
  {
    /* We are on a split in an expanded transaction. Just copy the split. */
    new_item = gnc_copy_split(split, use_cut_semantics);

    if (new_item != SCM_UNDEFINED)
    {
      if (changed)
        xaccSRSaveRegEntryToSCM(reg, SCM_UNDEFINED, new_item,
                                use_cut_semantics);

      copied_leader_guid = *xaccGUIDNULL();
    }
  }
  else
  {
    /* We are on a transaction row. Copy the whole transaction. */
    new_item = gnc_copy_trans(trans, use_cut_semantics);

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

        xaccSRSaveRegEntryToSCM(reg, new_item, split_scm, use_cut_semantics);
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

  copied_class = cursor_class;
}

/* ======================================================== */

void
xaccSRCopyCurrent (SplitRegister *reg)
{
  xaccSRCopyCurrentInternal (reg, FALSE);
}

/* ======================================================== */

void
xaccSRCutCurrent (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  CursorClass cursor_class;
  Transaction *trans;
  guint32 changed;
  Split *split;

  split = xaccSRGetCurrentSplit(reg);
  trans = xaccSRGetCurrentTrans(reg);

  /* This shouldn't happen, but be paranoid. */
  if (trans == NULL)
    return;

  cursor_class = xaccSplitRegisterGetCurrentCursorClass (reg);

  /* Can't do anything with this. */
  if (cursor_class == CURSOR_CLASS_NONE)
    return;

  /* This shouldn't happen, but be paranoid. */
  if ((split == NULL) && (cursor_class == CURSOR_CLASS_TRANS))
    return;

  changed = xaccSplitRegisterGetChangeFlag(reg);

  /* See if we were asked to cut an unchanged blank split. Don't. */
  if (!changed && ((split == NULL) || (split == blank_split)))
    return;

  xaccSRCopyCurrentInternal(reg, TRUE);

  if (cursor_class == CURSOR_CLASS_SPLIT)
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
  CursorClass cursor_class;
  Transaction *trans;
  Split *trans_split;
  Split *split;

  if (copied_class == CURSOR_CLASS_NONE)
    return;

  split = xaccSRGetCurrentSplit(reg);
  trans = xaccSRGetCurrentTrans(reg);

  trans_split = xaccSRGetCurrentTransSplit(reg, NULL);

  /* This shouldn't happen, but be paranoid. */
  if (trans == NULL)
    return;

  cursor_class = xaccSplitRegisterGetCurrentCursorClass (reg);

  /* Can't do anything with this. */
  if (cursor_class == CURSOR_CLASS_NONE)
    return;

  /* This shouldn't happen, but be paranoid. */
  if ((split == NULL) && (cursor_class == CURSOR_CLASS_TRANS))
    return;

  if (cursor_class == CURSOR_CLASS_SPLIT) {
    const char *message = _("You are about to overwrite an existing split.\n"
                            "Are you sure you want to do that?");
    gboolean result;

    if (copied_class == CURSOR_CLASS_TRANS)
      return;

    if (split != NULL)
      result = gnc_verify_dialog_parented(xaccSRGetParent(reg),
                                          message, FALSE);
    else
      result = TRUE;

    if (!result)
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
    const char *message = _("You are about to overwrite an existing "
                            "transaction.\n"
                            "Are you sure you want to do that?");
    gboolean result;

    const GUID *new_guid;
    int trans_split_index;
    int split_index;
    int num_splits;

    if (copied_class == CURSOR_CLASS_SPLIT)
      return;

    if (split != blank_split)
      result = gnc_verify_dialog_parented(xaccSRGetParent(reg),
                                          message, FALSE);
    else
      result = TRUE;

    if (!result)
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
    info->cursor_hint_cursor_class = CURSOR_CLASS_TRANS;
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
  xaccAccountBeginEdit(account);
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
  guint32 changed;
  VirtualLocation virt_loc;

  if (reg == NULL)
    return;

  virt_loc = reg->table->current_cursor_loc;

  changed = xaccSplitRegisterGetChangeFlag (reg);
  if (!changed)
    return;

  /* We're just cancelling the current split here, not the transaction.
   * When cancelling edits, reload the cursor from the transaction. */
  xaccSplitRegisterClearChangeFlag (reg);

  if (gnc_table_find_close_valid_cell (reg->table, &virt_loc, FALSE))
    gnc_table_move_cursor_gui (reg->table, virt_loc);

  gnc_table_refresh_gui (reg->table);
}

/* ======================================================== */

void
xaccSRCancelCursorTransChanges (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
  GList *accounts;

  /* Get the currently open transaction, rollback the edits on it, and
   * then repaint everything. To repaint everything, make a note of
   * all of the accounts that will be affected by this rollback. */
  if (!xaccTransIsOpen(pending_trans))
  {
    xaccSRCancelCursorSplitChanges (reg);
    return;
  }

  if (!pending_trans)
    return;

  accounts = gnc_trans_prepend_account_list (pending_trans, NULL);

  xaccTransRollbackEdit (pending_trans);

  accounts = gnc_trans_prepend_account_list (pending_trans, accounts);

  gnc_account_glist_ui_refresh (accounts);

  g_list_free (accounts);

  info->pending_trans_guid = *xaccGUIDNULL ();

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
xaccSRSaveRegEntryToSCM (SplitRegister *reg, SCM trans_scm, SCM split_scm,
                         gboolean use_cut_semantics)
{
  SCM other_split_scm = SCM_UNDEFINED;
  Transaction *trans;
  guint32 changed;

  /* use the changed flag to avoid heavy-weight updates
   * of the split & transaction fields. This will help
   * cut down on uneccessary register redraws. */
  changed = xaccSplitRegisterGetChangeFlag (reg);
  if (!changed)
    return FALSE;

  changed |= xaccSplitRegisterGetConditionalChangeFlag (reg);

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

  if (MOD_NOTES & changed)
    gnc_trans_scm_set_notes(trans_scm, reg->notesCell->cell.value);

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

  if (reg->style == REG_STYLE_LEDGER)
    other_split_scm = gnc_trans_scm_get_other_split_scm(trans_scm, split_scm);

  if (MOD_MXFRM & changed) {
    other_split_scm = gnc_trans_scm_get_other_split_scm(trans_scm, split_scm);

    if (other_split_scm == SCM_UNDEFINED) {
      if (gnc_trans_scm_get_num_splits(trans_scm) == 1) {
        Split *temp_split;

        temp_split = xaccMallocSplit ();
        other_split_scm = gnc_copy_split(temp_split, use_cut_semantics);
        xaccSplitDestroy(temp_split);

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

  if (MOD_AMNT & changed) {
    gnc_numeric new_value;
    gnc_numeric credit;
    gnc_numeric debit;

    credit = xaccGetPriceCellValue (reg->creditCell);
    debit  = xaccGetPriceCellValue (reg->debitCell);
    new_value = gnc_numeric_sub_fixed (debit, credit);

    gnc_split_scm_set_value (split_scm, new_value);
  }

  if (MOD_PRIC & changed) {
    /* do nothing for now */
  }

  if (MOD_SHRS & changed) {
    gnc_numeric shares = xaccGetPriceCellValue(reg->sharesCell);

    gnc_split_scm_set_quantity (split_scm, shares);
  }

  if ((MOD_AMNT | MOD_PRIC | MOD_SHRS) & changed)
  {
    if (other_split_scm != SCM_UNDEFINED)
    {
      gnc_numeric num;

      num = gnc_split_scm_get_quantity (split_scm);
      gnc_split_scm_set_quantity (other_split_scm, gnc_numeric_neg (num));

      num = gnc_split_scm_get_value (split_scm);
      gnc_split_scm_set_value (other_split_scm, gnc_numeric_neg (num));
    }
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
     xaccTransBeginEdit (trans, TRUE);
     pending_trans = trans;
     info->pending_trans_guid = *xaccTransGetGUID(trans);
   }

   /* If we are committing the blank split, add it to the account now */
   if (trans == blank_trans)
   {
     xaccAccountInsertSplit (info->default_source_account, blank_split);
     xaccTransSetDateEnteredSecs(trans, time(NULL));
   }

   if (split == NULL)
   {
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

     gnc_table_set_virt_cell_data (reg->table,
                                   reg->table->current_cursor_loc.vcell_loc,
                                   xaccSplitGetGUID (split));

     trans_split = xaccSRGetCurrentTransSplit (reg, NULL);
     if ((info->cursor_hint_trans == trans) &&
         (info->cursor_hint_trans_split == trans_split) &&
         (info->cursor_hint_split == NULL))
     {
       info->cursor_hint_split = split;
       info->cursor_hint_cursor_class = CURSOR_CLASS_SPLIT;
     }
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
  Split *other_split = NULL;
  guint32 changed;

  changed = xaccSplitRegisterGetChangeFlag (reg);
  changed |= xaccSplitRegisterGetConditionalChangeFlag (reg);

  /* copy the contents from the cursor to the split */
  if (MOD_DATE & changed) {
    Timespec ts;

    /* commit any pending changes */
    xaccCommitDateCell (reg->dateCell);

    DEBUG ("MOD_DATE: %s", reg->dateCell->cell.value);

    xaccDateCellGetDate (reg->dateCell, &ts);

    xaccTransSetDateTS (trans, &ts);
  }

  if (MOD_NUM & changed) {
    DEBUG ("MOD_NUM: %s\n", reg->numCell->cell.value);
    xaccTransSetNum (trans, reg->numCell->cell.value);
    xaccSetNumCellLastNum(reg->numCell, reg->numCell->cell.value);
  }

  if (MOD_DESC & changed) {
    DEBUG ("MOD_DESC: %s", reg->descCell->cell.value);
    xaccTransSetDescription (trans, reg->descCell->cell.value);
  }

  if (MOD_NOTES & changed) {
    DEBUG ("MOD_NOTES: %s", reg->notesCell->cell.value);
    xaccTransSetNotes (trans, reg->notesCell->cell.value);
  }

  if (MOD_RECN & changed) {
    DEBUG ("MOD_RECN: %c", xaccRecnCellGetFlag(reg->recnCell));
    xaccSplitSetReconcile (split, xaccRecnCellGetFlag(reg->recnCell));
  }

  if (MOD_ACTN & changed) {
    DEBUG ("MOD_ACTN: %s", reg->actionCell->cell.value);
    xaccSplitSetAction (split, reg->actionCell->cell.value);
  }

  if (MOD_MEMO & changed) {
    DEBUG ("MOD_MEMO: %s", reg->memoCell->cell.value);
    xaccSplitSetMemo (split, reg->memoCell->cell.value);
  }

  /* -------------------------------------------------------------- */
  /* OK, the handling of transfers gets complicated because it depends
   * on what was displayed to the user.  For a multi-line display, we
   * just reparent the indicated split, its it, and that's that. For
   * a two-line display, we want to reparent the "other" split, but
   * only if there is one.  XFRM is the straight split, MXFRM is the
   * mirrored split.  XTO is the straight split, too :) Only one of
   * XFRM or XTO should be in a given cursor. */
  if ((MOD_XFRM | MOD_XTO) & changed)
  {
    Account *old_acc;
    Account *new_acc;
    char *new_name;

    if (MOD_XFRM & changed)
    {
      DEBUG ("MOD_XFRM: %s", reg->xfrmCell->cell.value);
    }
    else
    {
      DEBUG ("MOD_XTO: %s", reg->xtoCell->cell.value);
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
      const gnc_commodity * currency = NULL;
      const gnc_commodity * security = NULL;

      currency = xaccAccountGetCurrency(new_acc);
      currency = xaccTransIsCommonExclSCurrency(trans, currency, split);

      if (currency == NULL)
      {
        security = xaccAccountGetSecurity(new_acc);
        security = xaccTransIsCommonExclSCurrency(trans, security, split);
      }

      if ((currency != NULL) || (security != NULL))
      {
        xaccAccountInsertSplit (new_acc, split);

        refresh_accounts = g_list_prepend(refresh_accounts, old_acc);
        refresh_accounts = g_list_prepend(refresh_accounts, new_acc);
      }
      else
      {
        const char *format = _("You cannot transfer funds from the %s "
                               "account.\nIt does not have a matching "
                               "currency.\nTo transfer funds between "
                               "accounts with different currencies\n"
                               "you need an intermediate currency account.\n"
                               "Please see the GnuCash online manual.");
        char *message;

        message = g_strdup_printf(format, xaccAccountGetName(new_acc));

        gnc_warning_dialog_parented(xaccSRGetParent(reg), message);
        g_free(message);
      }
    }
  }

  if (reg->style == REG_STYLE_LEDGER)
    other_split = xaccGetOtherSplit (split);

  if (MOD_MXFRM & changed)
  {
    DEBUG ("MOD_MXFRM: %s", reg->mxfrmCell->cell.value);

    other_split = xaccGetOtherSplit (split);

    /* other_split may be null for two very different reasons:
     * (1) the parent transaction has three or more splits in it,
     *     and so the "other" split is ambiguous, and thus null.
     * (2) the parent transaction has only this one split as a child.
     *     and "other" is null because there is no other.
     *
     * In the case (2), we want to create the other split, so that 
     * the user's request to transfer actually works out.
     */

    if (!other_split)
    {
      other_split = xaccTransGetSplit (trans, 1);
      if (!other_split)
      {
        other_split = xaccMallocSplit ();
        xaccTransAppendSplit (trans, other_split);
      }
    }

    if (other_split)
    {
      Account *old_acc=NULL, *new_acc=NULL;

      /* do some reparenting. Insertion into new account will automatically
       * delete from the old account */
      old_acc = xaccSplitGetAccount (other_split);
      new_acc = xaccGetAccountByFullName (trans, reg->mxfrmCell->cell.value,
                                          account_separator);

      if ((new_acc != NULL) && (old_acc != new_acc))
      {
        const gnc_commodity * currency = NULL;
        const gnc_commodity * security = NULL;

        currency = xaccAccountGetCurrency(new_acc);
        currency = xaccTransIsCommonExclSCurrency(trans, 
						  currency, other_split);

        if (currency == NULL)
        {
          security = xaccAccountGetSecurity(new_acc);
          security = xaccTransIsCommonExclSCurrency(trans, 
						    security, other_split);
        }

        if ((currency != NULL) || (security != NULL))
        {
          xaccAccountInsertSplit (new_acc, other_split);

          refresh_accounts = g_list_prepend(refresh_accounts, old_acc);
          refresh_accounts = g_list_prepend(refresh_accounts, new_acc);
        }
        else
        {
          const char *format = _("You cannot transfer funds from the %s "
                                 "account.\nIt does not have a matching "
                                 "currency.\nTo transfer funds between "
                                 "accounts with different currencies\n"
                                 "you need an intermediate currency account.\n"
                                 "Please see the GnuCash online manual.");
          char *message;

          message = g_strdup_printf(format, xaccAccountGetName(new_acc));

          gnc_warning_dialog_parented(xaccSRGetParent(reg), message);
          g_free(message);
        }
      }
    }
  }

  if (((MOD_AMNT | MOD_PRIC | MOD_SHRS) & changed) &&
      ((STOCK_REGISTER    == (reg->type)) ||
       (CURRENCY_REGISTER == (reg->type)) ||
       (PORTFOLIO_LEDGER  == (reg->type))))
  {
    gnc_numeric calc_value;
    gnc_numeric value;
    gnc_numeric price;
    gnc_numeric amount;
    int denom;

    if (MOD_SHRS & changed)
      amount = xaccGetPriceCellValue(reg->sharesCell);
    else
      amount = xaccSplitGetShareAmount(split);

    if (MOD_PRIC & changed)
      price = xaccGetPriceCellValue(reg->priceCell);
    else
      price = xaccSplitGetSharePrice(split);

    if (MOD_AMNT & changed)
    {
      gnc_numeric credit = xaccGetPriceCellValue(reg->creditCell);
      gnc_numeric debit  = xaccGetPriceCellValue(reg->debitCell);
      value = gnc_numeric_sub_fixed (debit, credit);
    }
    else
      value = xaccSplitGetValue(split);

    calc_value = gnc_numeric_mul (price, amount,
                                  GNC_DENOM_AUTO, GNC_DENOM_LCD);

    denom = gnc_split_get_value_denom (split);

    if (!gnc_numeric_same (value, calc_value, denom, GNC_RND_ROUND))
    {
      int i;
      int choice;
      int default_value;
      char *radio_list[4] = { NULL, NULL, NULL, NULL };
      const char *title = _("Recalculate Transaction");
      const char *message = _("The values entered for this transaction "
                              "are inconsistent.\nWhich value would you "
                              "like to have recalculated?");

      if (MOD_SHRS & changed)
        radio_list[0] = g_strdup_printf("%s (%s)", _("Shares"), _("Changed"));
      else
        radio_list[0] = g_strdup(_("Shares"));

      if (MOD_PRIC & changed)
        radio_list[1] = g_strdup_printf("%s (%s)", _("Price"), _("Changed"));
      else
        radio_list[1] = g_strdup(_("Price"));

      if (MOD_AMNT & changed)
        radio_list[2] = g_strdup_printf("%s (%s)", _("Value"), _("Changed"));
      else
        radio_list[2] = g_strdup(_("Value"));

      if (!(MOD_PRIC & changed))
        default_value = 1;
      if (!(MOD_SHRS & changed))
        default_value = 0;
      else if (!(MOD_AMNT & changed))
        default_value = 2;
      else
        default_value = 1;

      choice = gnc_choose_radio_option_dialog_parented(xaccSRGetParent(reg),
                                                       title,
                                                       message,
                                                       default_value,
                                                       radio_list);

      for (i = 0; i < 3; i++)
        g_free(radio_list[i]);

      switch(choice)
      {
        case 0: /* Modify number of shares */
          if (gnc_numeric_zero_p (price))
            break;

          denom = gnc_split_get_quantity_denom (split);

          amount = gnc_numeric_div (value, price, denom, GNC_RND_ROUND);

          xaccSetPriceCellValue(reg->sharesCell, amount);
          changed |= MOD_SHRS;
          break;
        case 1: /* Modify the share price */
          if (gnc_numeric_zero_p (amount))
            break;

          price = gnc_numeric_div (value, amount,
                                   GNC_DENOM_AUTO,
                                   GNC_DENOM_EXACT);

          if (gnc_numeric_negative_p (price)) {
            price = gnc_numeric_neg (price);
            xaccSetDebCredCellValue (reg->debitCell, reg->creditCell,
                                     gnc_numeric_neg (value));
            changed |= MOD_AMNT;
          }
          xaccSetPriceCellValue(reg->priceCell, price);
          changed |= MOD_PRIC;
          break;
        case 2: /* Modify total value */
          denom = gnc_split_get_value_denom (split);

          value = gnc_numeric_mul (price, amount, denom, GNC_RND_ROUND);

          xaccSetDebCredCellValue (reg->debitCell, reg->creditCell, value);
          changed |= MOD_AMNT;
          break;
        default:
          break;
      }
    }
  }

  if (MOD_SHRS & changed)
  {
    gnc_numeric amount = xaccGetPriceCellValue (reg->sharesCell);
    gnc_numeric price  = xaccGetPriceCellValue (reg->priceCell);

    DEBUG ("MOD_SHRS");

    xaccSplitSetShareAmount (split, amount);
    xaccSplitSetSharePrice (split, price);
  }

  if (MOD_PRIC & changed)
  {
    gnc_numeric price;

    price = xaccGetPriceCellValue (reg->priceCell);

    DEBUG ("MOD_PRIC");

    xaccSplitSetSharePrice (split, price);
  }

  if (MOD_AMNT & changed)
  {
    gnc_numeric new_amount;
    gnc_numeric credit;
    gnc_numeric debit;

    credit = xaccGetPriceCellValue(reg->creditCell);
    debit  = xaccGetPriceCellValue(reg->debitCell);
    new_amount = gnc_numeric_sub_fixed (debit, credit);

    DEBUG ("MOD_AMNT");

    xaccSplitSetValue (split, new_amount);
  }

  if ((MOD_AMNT | MOD_PRIC | MOD_SHRS) & changed)
  {
    xaccSplitScrub (split);

    if (other_split)
    {
      gnc_numeric amount = xaccSplitGetShareAmount (split);
      gnc_numeric price = xaccSplitGetSharePrice (split);

      amount = gnc_numeric_neg (amount);

      xaccSplitSetSharePriceAndAmount (other_split, price, amount);
      xaccSplitScrub (other_split);
    }
  }

  return refresh_accounts;
}

/* ======================================================== */

static gnc_numeric
get_trans_total_value (SplitRegister *reg, Transaction *trans)
{
  GList *node;
  Account *account;
  gnc_numeric total = gnc_numeric_zero ();

  SRInfo *info = xaccSRGetInfo(reg);
  account = info->default_source_account;

  if (!account)
    return total;

  total = gnc_numeric_convert (total, xaccAccountGetCurrencySCU (account),
                               GNC_RND_ROUND);

  for (node = xaccTransGetSplitList (trans); node; node = node->next)
  {
    Split *split = node->data;

    if (xaccSplitGetAccount (split) != account)
      continue;

    total = gnc_numeric_add_fixed (total, xaccSplitGetValue (split));
  }

  return total;
}

static gnc_numeric
get_trans_total_shares (SplitRegister *reg, Transaction *trans)
{
  GList *node;
  Account *account;
  gnc_numeric total = gnc_numeric_zero ();

  SRInfo *info = xaccSRGetInfo(reg);
  account = info->default_source_account;

  if (!account)
    return total;

  total = gnc_numeric_convert (total, xaccAccountGetSecuritySCU (account),
                               GNC_RND_ROUND);

  for (node = xaccTransGetSplitList (trans); node; node = node->next)
  {
    Split *split = node->data;

    if (xaccSplitGetAccount (split) != account)
      continue;

    total = gnc_numeric_add_fixed (total, xaccSplitGetShareAmount (split));
  }

  return total;
}

static Split *
get_trans_last_split (SplitRegister *reg, Transaction *trans)
{
  GList *node;
  Account *account;
  Split *last_split = NULL;

  SRInfo *info = xaccSRGetInfo(reg);
  account = info->default_source_account;

  if (!account)
    return last_split;

  for (node = xaccTransGetSplitList (trans); node; node = node->next)
  {
    Split *split = node->data;

    if (xaccSplitGetAccount (split) != account)
      continue;

    if (!last_split)
    {
      last_split = split;
      continue;
    }

    if (xaccSplitDateOrder (last_split, split) < 0)
      last_split = split;
  }

  return last_split;
}

static gnc_numeric
get_trans_total_share_balance (SplitRegister *reg, Transaction *trans)
{
  Split *last_split;

  last_split = get_trans_last_split (reg, trans);

  return xaccSplitGetShareBalance (last_split);
}

static gnc_numeric
get_trans_total_balance (SplitRegister *reg, Transaction *trans)
{
  Split *last_split;

  last_split = get_trans_last_split (reg, trans);

  return xaccSplitGetBalance (last_split);
}

/* ======================================================== */

const char *
xaccSRGetEntryHandler (VirtualLocation virt_loc, short _cell_type,
                       gboolean *conditionally_changed, gpointer user_data)
{
  CellType cell_type = _cell_type;
  SplitRegister *reg = user_data;
  const char *value = "";
  Transaction *trans;
  Split *split;

  if (conditionally_changed)
    *conditionally_changed = FALSE;

  split = sr_get_split (reg, virt_loc.vcell_loc);
  if (split == NULL)
  {
    gnc_numeric imbalance;

    trans = xaccSRGetTrans (reg, virt_loc.vcell_loc);
    imbalance = xaccTransGetImbalance (trans);

    if (gnc_numeric_zero_p (imbalance))
      return value;

    switch (cell_type)
    {
      case CRED_CELL:
      case DEBT_CELL:
        imbalance = gnc_numeric_neg (imbalance);

        if (gnc_numeric_negative_p (imbalance) && (cell_type == DEBT_CELL))
          return "";

        if (gnc_numeric_positive_p (imbalance) && (cell_type == CRED_CELL))
          return "";

        if (conditionally_changed)
          *conditionally_changed = TRUE;

        imbalance = gnc_numeric_abs (imbalance);

        return xaccPrintAmount (imbalance,
                                gnc_split_value_print_info (split, FALSE));

      default:
        return value;
    }
  }

  trans = xaccSplitGetParent (split);

  switch (cell_type)
  {
    case DATE_CELL:
      {
        Timespec ts;

        xaccTransGetDateTS (trans, &ts);

        return gnc_print_date (ts);
      }
      break;

    case NUM_CELL:
      return xaccTransGetNum (trans);
      break;

    case DESC_CELL:
      return xaccTransGetDescription (trans);
      break;

    case NOTES_CELL:
      return xaccTransGetNotes (trans);
      break;

    case RECN_CELL:
      {
        static char s[2];

        s[0] = xaccSplitGetReconcile (split);
        s[1] = '\0';

        return s;
      }
      break;

    case SHRBALN_CELL:
    case TSHRBALN_CELL:
      {
        SRInfo *info = xaccSRGetInfo(reg);
        Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
        gnc_numeric balance;

        if (split == blank_split)
          return "";

        if (cell_type == SHRBALN_CELL)
          balance = xaccSplitGetShareBalance (split);
        else
          balance = get_trans_total_share_balance (reg, trans);

        return xaccPrintAmount (balance,
                                gnc_split_quantity_print_info (split, FALSE));
      }
      break;

    case BALN_CELL:
    case TBALN_CELL:
      {
        SRInfo *info = xaccSRGetInfo(reg);
        Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
        gnc_numeric balance;

        if (split == blank_split)
          return "";

        /* If the reverse_balance callback is present use that.
         * Otherwise, reverse income and expense by default. */
        if (cell_type == BALN_CELL)
          balance = xaccSplitGetBalance (split);
        else
          balance = get_trans_total_balance (reg, trans);

        if (reverse_balance != NULL)
        {
          Account *account;

          account = xaccSplitGetAccount(split);
          if (account == NULL)
            account = info->default_source_account;

          if (reverse_balance(account))
            balance = gnc_numeric_neg (balance);
        }

        return xaccPrintAmount (balance,
                                gnc_split_value_print_info (split, FALSE));
      }
      break;

    case ACTN_CELL:
      return xaccSplitGetAction (split);
      break;

    case XFRM_CELL:
    case XTO_CELL:
      {
        static char *name = NULL;

        g_free(name);

        name = xaccAccountGetFullName (xaccSplitGetAccount (split),
                                       account_separator);

        return name;
      }
      break;

    case MEMO_CELL:
      return xaccSplitGetMemo (split);
      break;

    case CRED_CELL:
    case DEBT_CELL:
      {
        gnc_numeric amount;

        amount = xaccSplitGetValue (split);
        if (gnc_numeric_zero_p (amount))
          return "";

        if (gnc_numeric_negative_p (amount) && (cell_type == DEBT_CELL))
          return "";

        if (gnc_numeric_positive_p (amount) && (cell_type == CRED_CELL))
          return "";

        amount = gnc_numeric_abs (amount);

        return xaccPrintAmount (amount,
                                gnc_split_value_print_info (split, FALSE));
      }

    case PRIC_CELL:
      {
        gnc_numeric price;

        price = xaccSplitGetSharePrice (split);

        return xaccPrintAmount (price, gnc_default_price_print_info ());
      }

    case SHRS_CELL:
      {
        gnc_numeric shares;

        shares = xaccSplitGetShareAmount (split);

        if (gnc_numeric_zero_p (shares))
          return "";

        return xaccPrintAmount (shares,
                                gnc_split_quantity_print_info (split, FALSE));
      }

    case MXFRM_CELL:
      {
         Split *s = xaccGetOtherSplit (split);
         static char *name = NULL;

         g_free (name);

         if (s)
           name = xaccAccountGetFullName (xaccSplitGetAccount (s),
                                          account_separator);
         else
         {
           /* determine whether s is null because threre are three
            * or more splits, or whether there is only one ... */
           s = xaccTransGetSplit (xaccSplitGetParent(split), 1);
           if (s)
             name = g_strdup (_("Split")); /* three or more */
           else
             name = g_strdup ("");         /* none */
         }

         return name;
      }

    case TCRED_CELL:
    case TDEBT_CELL:
      {
        gnc_numeric total;

        total = get_trans_total_value (reg, trans);
        if (gnc_numeric_zero_p (total))
          return "";

        if (gnc_numeric_negative_p (total) && (cell_type == TDEBT_CELL))
          return "";

        if (gnc_numeric_positive_p (total) && (cell_type == TCRED_CELL))
          return "";

        total = gnc_numeric_abs (total);

        return xaccPrintAmount (total,
                                gnc_split_value_print_info (split, FALSE));
      }

    case TSHRS_CELL:
      {
        gnc_numeric total;

        total = get_trans_total_shares (reg, trans);

        return xaccPrintAmount (total,
                                gnc_split_quantity_print_info (split, FALSE));
      }

    default:
      return "";
      break;
  }
}

CellIOFlags
xaccSRGetIOFlagsHandler (VirtualLocation virt_loc, gpointer user_data)
{
  SplitRegister *reg = user_data;
  CellType cell_type;

  cell_type = xaccSplitRegisterGetCellType (reg, virt_loc);

  switch (cell_type)
  {
    case DATE_CELL:
    case NUM_CELL:
    case DESC_CELL:
    case ACTN_CELL:
    case XFRM_CELL:
    case XTO_CELL:
    case MEMO_CELL:
    case CRED_CELL:
    case DEBT_CELL:
    case MXFRM_CELL:
    case NOTES_CELL:
      return XACC_CELL_ALLOW_ALL;

    case RECN_CELL:
      return XACC_CELL_ALLOW_ALL | XACC_CELL_ALLOW_EXACT_ONLY;

    case PRIC_CELL:
    case SHRS_CELL:
      {
        Split *split = sr_get_split (reg, virt_loc.vcell_loc);
        GNCAccountType account_type;
        CursorClass cursor_class;
        Account *account;
        guint32 changed;

        if (!split)
          return XACC_CELL_ALLOW_ALL;

        cursor_class = xaccSplitRegisterGetCursorClass (reg,
                                                        virt_loc.vcell_loc);
        if (cursor_class != CURSOR_CLASS_SPLIT)
          return XACC_CELL_ALLOW_ALL;

        changed = xaccSplitRegisterGetChangeFlag (reg);
        if (MOD_XFRM & changed)
        {
          account = xaccGetAccountFromFullName (gncGetCurrentGroup (),
                                                reg->xfrmCell->cell.value,
                                                account_separator);
          if (!account)
            account = xaccSplitGetAccount (split);
        }
        else
          account = xaccSplitGetAccount (split);

        if (!account)
          return XACC_CELL_ALLOW_ALL;

        account_type = xaccAccountGetType (account);

        if (account_type == STOCK  ||
            account_type == MUTUAL ||
            account_type == CURRENCY)
          return XACC_CELL_ALLOW_ALL;

        return XACC_CELL_ALLOW_SHADOW;
      }

    default:
      return XACC_CELL_ALLOW_NONE;
  }
}

guint32
xaccSRGetFGColorHandler (VirtualLocation virt_loc, gpointer user_data)
{
  SplitRegister *reg = user_data;
  const guint32 black = 0x000000;
  const guint32 red   = 0xff0000;
  Transaction *trans;
  VirtualCell *vcell;
  gboolean is_current;
  CellType cell_type;
  Split *split;

  if (!use_red_for_negative)
    return black;

  vcell = gnc_table_get_virtual_cell (reg->table, virt_loc.vcell_loc);
  if (!vcell || !vcell->cellblock)
    return black;

  split = xaccSplitLookup (vcell->vcell_data);
  if (split == NULL)
    return black;

  trans = xaccSplitGetParent (split);

  cell_type = xaccSplitRegisterGetCellType (reg, virt_loc);

  is_current = virt_cell_loc_equal (reg->table->current_cursor_loc.vcell_loc,
                                    virt_loc.vcell_loc);

  switch (cell_type)
  {
    case SHRS_CELL:
    case TSHRS_CELL:
      {
        gnc_numeric shares;

        if (cell_type == TSHRS_CELL)
          shares = get_trans_total_shares (reg, trans);
        else if (is_current)
          shares = xaccGetPriceCellValue (reg->sharesCell);
        else
          shares = xaccSplitGetShareAmount (split);

        if (gnc_numeric_negative_p (shares))
          return red;

        return black;
      }
      break;

    case SHRBALN_CELL:
    case TSHRBALN_CELL:
      {
        gnc_numeric balance;

        if (cell_type == SHRBALN_CELL)
          balance = xaccSplitGetShareBalance (split);
        else
          balance = get_trans_total_share_balance (reg, trans);

        if (gnc_numeric_negative_p (balance))
          return red;

        return black;
      }
      break;

    case BALN_CELL:
    case TBALN_CELL:
      {
        gnc_numeric balance;

        /* If the reverse_balance callback is present use that.
         * Otherwise, reverse income and expense by default. */
        if (cell_type == BALN_CELL)
          balance = xaccSplitGetBalance (split);
        else
          balance = get_trans_total_balance (reg, trans);

        if (reverse_balance != NULL)
        {
          Account *account;

          account = xaccSplitGetAccount (split);

          if (reverse_balance (account))
            balance = gnc_numeric_neg (balance);
        }

        if (gnc_numeric_negative_p (balance))
          return red;

        return black;
      }
      break;

    default:
      break;
  }

  return black;
}

/* ======================================================== */

static SplitRegisterColors reg_colors =
{
  0xffffff, /* white */
  0xffffff,
  0xffffff,

  0xffffff,
  0xffffff,
  0xffffff,
  0xffffff,

  FALSE /* double mode alternate by physical row */
};

void
xaccSetSplitRegisterColors (SplitRegisterColors reg_colors_new)
{
  reg_colors = reg_colors_new;
}

guint32
xaccSRGetBGColorHandler (VirtualLocation virt_loc, gpointer user_data)
{
  SplitRegister *reg = user_data;
  VirtualCell *vcell;
  guint32 bg_color;
  gboolean is_current;

  bg_color = 0xffffff; /* white */

  if (!reg)
    return bg_color;

  vcell = gnc_table_get_virtual_cell (reg->table, virt_loc.vcell_loc);
  if (!vcell || !vcell->cellblock)
    return bg_color;

  if ((virt_loc.phys_col_offset < vcell->cellblock->start_col) ||
      (virt_loc.phys_col_offset > vcell->cellblock->stop_col))
    return bg_color;

  is_current = virt_cell_loc_equal (reg->table->current_cursor_loc.vcell_loc,
                                    virt_loc.vcell_loc);

  switch ((CursorType) vcell->cellblock->cursor_type)
  {
    case CURSOR_TYPE_HEADER:
      return reg_colors.header_bg_color;

    case CURSOR_TYPE_SINGLE_JOURNAL:
    case CURSOR_TYPE_SINGLE_LEDGER:
      if (is_current)
        return vcell->start_primary_color ?
          reg_colors.primary_active_bg_color :
          reg_colors.secondary_active_bg_color;

      return vcell->start_primary_color ?
        reg_colors.primary_bg_color : reg_colors.secondary_bg_color;

    case CURSOR_TYPE_DOUBLE_LEDGER:
    case CURSOR_TYPE_DOUBLE_JOURNAL:
      if (is_current)
      {
        if (reg_colors.double_alternate_virt)
          return vcell->start_primary_color ?
            reg_colors.primary_active_bg_color :
            reg_colors.secondary_active_bg_color;

        return (virt_loc.phys_row_offset % 2 == 0) ?
          reg_colors.primary_active_bg_color :
          reg_colors.secondary_active_bg_color;
      }

      if (reg_colors.double_alternate_virt)
        return vcell->start_primary_color ?
          reg_colors.primary_bg_color :
          reg_colors.secondary_bg_color;

      return (virt_loc.phys_row_offset % 2 == 0) ?
        reg_colors.primary_bg_color :
        reg_colors.secondary_bg_color;

    case CURSOR_TYPE_SPLIT:
      {
        Transaction *trans;
        gnc_numeric imbalance;

        trans = xaccSRGetTrans (reg, virt_loc.vcell_loc);
        imbalance = xaccTransGetImbalance (trans);

        if (!gnc_numeric_zero_p (imbalance))
          return 0xffff00;

        if (is_current)
          return reg_colors.split_active_bg_color;

        return reg_colors.split_bg_color;
      }

    default:
      PWARN("Unexpected cursor type: %d\n", vcell->cellblock->cursor_type);
      return bg_color;
  }
}

/* ======================================================== */

G_INLINE_FUNC void
sr_add_transaction (SplitRegister *reg,
                    Transaction *trans,
                    Split *split,
                    CellBlock *lead_cursor,
                    gboolean visible_splits,
                    gboolean start_primary_color,
                    gboolean sort_splits,
                    gboolean add_blank,
                    Transaction *find_trans,
                    Split *find_split,
                    CursorClass find_class,
                    int *new_split_row,
                    VirtualCellLocation *vcell_loc);

G_INLINE_FUNC void
sr_add_transaction (SplitRegister *reg,
                    Transaction *trans,
                    Split *split,
                    CellBlock *lead_cursor,
                    gboolean visible_splits,
                    gboolean start_primary_color,
                    gboolean sort_splits,
                    gboolean add_blank,
                    Transaction *find_trans,
                    Split *find_split,
                    CursorClass find_class,
                    int *new_split_row,
                    VirtualCellLocation *vcell_loc)
{
  GList *node;

  if (split == find_split)
    *new_split_row = vcell_loc->virt_row;

  gnc_table_set_vcell (reg->table, lead_cursor, xaccSplitGetGUID (split),
                       TRUE, start_primary_color, *vcell_loc);
  vcell_loc->virt_row++;

  if (sort_splits)
  {
    /* first debits */
    for (node = xaccTransGetSplitList (trans); node; node = node->next)
    {
      Split *secondary = node->data;

      if (gnc_numeric_negative_p (xaccSplitGetValue (secondary)))
        continue;

      if (secondary == find_split && find_class == CURSOR_CLASS_SPLIT)
        *new_split_row = vcell_loc->virt_row;

      gnc_table_set_vcell (reg->table, reg->cursor_split,
                           xaccSplitGetGUID (secondary),
                           visible_splits, TRUE, *vcell_loc);
      vcell_loc->virt_row++;
    }

    /* then credits */
    for (node = xaccTransGetSplitList (trans); node; node = node->next)
    {
      Split *secondary = node->data;

      if (!gnc_numeric_negative_p (xaccSplitGetValue (secondary)))
        continue;

      if (secondary == find_split && find_class == CURSOR_CLASS_SPLIT)
        *new_split_row = vcell_loc->virt_row;

      gnc_table_set_vcell (reg->table, reg->cursor_split,
                           xaccSplitGetGUID (secondary),
                           visible_splits, TRUE, *vcell_loc);
      vcell_loc->virt_row++;
    }
  }
  else
  {
    for (node = xaccTransGetSplitList (trans); node; node = node->next)
    {
      Split *secondary = node->data;

      if (secondary == find_split && find_class == CURSOR_CLASS_SPLIT)
        *new_split_row = vcell_loc->virt_row;

      gnc_table_set_vcell (reg->table, reg->cursor_split,
                           xaccSplitGetGUID (secondary),
                           visible_splits, TRUE, *vcell_loc);
      vcell_loc->virt_row++;
    }
  }

  if (!add_blank)
    return;

  if (find_trans == trans && find_split == NULL &&
      find_class == CURSOR_CLASS_SPLIT)
        *new_split_row = vcell_loc->virt_row;

  /* Add blank transaction split */
  gnc_table_set_vcell (reg->table, reg->cursor_split,
                       xaccSplitGetGUID (NULL), FALSE, TRUE, *vcell_loc);
  vcell_loc->virt_row++;

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
  GHashTable *trans_table = NULL;
  CellBlock *lead_cursor;
  Transaction *find_trans;
  Transaction *trans;
  CursorClass find_class;
  Split *find_trans_split;
  Split *find_split;
  Split *split;
  Table *table;

  gboolean start_primary_color = TRUE;
  gboolean found_pending = FALSE;
  gboolean found_divider = FALSE;
  gboolean multi_line;
  gboolean dynamic;

  VirtualCellLocation vcell_loc;
  VirtualLocation save_loc;

  guint32 changed;
  int new_trans_split_row = -1;
  int new_trans_row = -1;
  int new_split_row = -1;
  time_t present;
  int i;

  /* make sure we have a blank split */
  if (blank_split == NULL)
  {
    Transaction *trans;

    trans = xaccMallocTransaction ();

    xaccTransBeginEdit (trans, TRUE);
    xaccTransSetDateSecs (trans, info->last_date_entered);
    blank_split = xaccMallocSplit ();
    xaccTransAppendSplit (trans, blank_split);
    xaccTransCommitEdit (trans);

    info->blank_split_guid = *xaccSplitGetGUID (blank_split);

    info->blank_split_edited = FALSE;
  }

  info->default_source_account = default_source_acc;

  table = reg->table;

  gnc_table_leave_update (table, table->current_cursor_loc);

  multi_line = (reg->style == REG_STYLE_JOURNAL);
  dynamic    = (reg->style == REG_STYLE_AUTO_LEDGER);

  lead_cursor = sr_get_passive_cursor (reg);

  /* figure out where we are going to. */
  if (info->traverse_to_new)
  {
    find_trans = xaccSplitGetParent (blank_split);
    find_split = NULL;
    find_trans_split = blank_split;
    find_class = info->cursor_hint_cursor_class;
  }
  else
  {
    find_trans = info->cursor_hint_trans;
    find_split = info->cursor_hint_split;
    find_trans_split = info->cursor_hint_trans_split;
    find_class = info->cursor_hint_cursor_class;
  }

  save_loc = table->current_cursor_loc;

  /* If the current cursor has changed we save the values for later
   * possible restoration. */
  changed = xaccSplitRegisterGetChangeFlag (reg);
  changed |= xaccSplitRegisterGetConditionalChangeFlag (reg);
  if (changed && (find_split == xaccSRGetCurrentSplit (reg)))
  {
    reg_buffer = xaccMallocSplitRegisterBuffer ();
    xaccSplitRegisterSaveCursor (reg, reg_buffer);
  }
  else
    reg_buffer = NULL;

  /* disable move callback -- we don't want the cascade of 
   * callbacks while we are fiddling with loading the register */
  table->move_cursor = NULL;

  /* invalidate the cursor */
  {
    VirtualLocation virt_loc;

    virt_loc.vcell_loc.virt_row = -1;
    virt_loc.vcell_loc.virt_col = -1;
    virt_loc.phys_row_offset = -1;
    virt_loc.phys_col_offset = -1;

    gnc_table_move_cursor_gui (table, virt_loc);
  }

  /* make sure that the header is loaded */
  vcell_loc.virt_row = 0;
  vcell_loc.virt_col = 0;
  gnc_table_set_vcell (table, reg->cursor_header, NULL, TRUE, TRUE, vcell_loc);
  vcell_loc.virt_row++;

  /* get the current time and reset the dividing row */
  {
    struct tm *tm;

    present = time (NULL);

    tm = localtime (&present);
    tm->tm_sec = 59;
    tm->tm_min = 59;
    tm->tm_hour = 23;

    present = mktime (tm);
  }

  table->dividing_row = -1;

  if (multi_line)
    trans_table = g_hash_table_new (g_direct_hash, g_direct_equal);

  /* populate the table */
  if (slist)
    split = slist[0]; 
  else
    split = NULL;

  for (i = 0; split; i++, split = slist[i])
  {
    trans = xaccSplitGetParent (split);

    if (pending_trans == trans)
      found_pending = TRUE;

    /* do not load the blank split */
    if (split == blank_split)
      continue;

    if (multi_line)
    {
      if (g_hash_table_lookup (trans_table, trans))
        continue;
      g_hash_table_insert (trans_table, trans, trans);
    }

    if (info->show_present_divider &&
        !found_divider &&
        (present < xaccTransGetDate (trans)))
    {
      table->dividing_row = vcell_loc.virt_row;
      found_divider = TRUE;
    }

    /* If this is the first load of the register,
     * fill up the quickfill cells. */
    if (info->first_pass)
    {
      GList *node;

      xaccQuickFillAddCompletion (reg->descCell,
                                  xaccTransGetDescription (trans));

      xaccQuickFillAddCompletion (reg->notesCell,
                                  xaccTransGetNotes (trans));

      for (node = xaccTransGetSplitList (trans); node; node = node->next)
      {
        Split *s = node->data;
        xaccQuickFillAddCompletion (reg->memoCell, xaccSplitGetMemo (s));
      }
    }

    if (trans == find_trans)
      new_trans_row = vcell_loc.virt_row;

    if (split == find_trans_split)
      new_trans_split_row = vcell_loc.virt_row;

    sr_add_transaction (reg, trans, split, lead_cursor, multi_line,
                        start_primary_color, TRUE, TRUE, find_trans,
                        find_split, find_class, &new_split_row, &vcell_loc);

    if (!multi_line)
      start_primary_color = !start_primary_color;
  }

  if (multi_line)
    g_hash_table_destroy (trans_table);

  /* add the blank split at the end. */
  split = blank_split;
  trans = xaccSplitGetParent (split);
  if (pending_trans == trans)
    found_pending = TRUE;

  if (trans == find_trans)
    new_trans_row = vcell_loc.virt_row;

  if (split == find_trans_split)
    new_trans_split_row = vcell_loc.virt_row;

  sr_add_transaction (reg, trans, split, lead_cursor, multi_line,
                      start_primary_color, FALSE, info->blank_split_edited,
                      find_trans, find_split, find_class, &new_split_row,
                      &vcell_loc);

  /* resize the table to the sizes we just counted above */
  /* num_virt_cols is always one. */
  gnc_table_set_size (table, vcell_loc.virt_row, 1);

  /* restore the cursor to its rightful position */
  {
    VirtualLocation trans_split_loc;
    Split *trans_split;

    if (new_split_row > 0)
      save_loc.vcell_loc.virt_row = new_split_row;
    else if (new_trans_split_row > 0)
      save_loc.vcell_loc.virt_row = new_trans_split_row;
    else if (new_trans_row > 0)
      save_loc.vcell_loc.virt_row = new_trans_row;

    trans_split_loc = save_loc;

    trans_split = xaccSRGetTransSplit (reg, save_loc.vcell_loc,
                                       &trans_split_loc.vcell_loc);

    if (dynamic || multi_line || info->trans_expanded)
    {
      gnc_table_set_virt_cell_cursor (table, trans_split_loc.vcell_loc,
                                      sr_get_active_cursor (reg));
      xaccSRSetTransVisible (reg, trans_split_loc.vcell_loc, TRUE, multi_line);

      info->trans_expanded = (reg->style == REG_STYLE_LEDGER);
    }
    else
    {
      save_loc = trans_split_loc;
      info->trans_expanded = FALSE;
    }

    if (gnc_table_find_close_valid_cell (table, &save_loc, FALSE))
    {
      gnc_table_move_cursor_gui (table, save_loc);
      new_split_row = save_loc.vcell_loc.virt_row;

      if (changed && (find_split == xaccSRGetCurrentSplit (reg)))
        xaccSplitRegisterRestoreCursorChanged (reg, reg_buffer);
    }

    if (reg_buffer != NULL)
      xaccDestroySplitRegisterBuffer (reg_buffer);

    reg_buffer = NULL;
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
  info->cursor_hint_trans_split = xaccSRGetCurrentTransSplit (reg, NULL);
  info->cursor_hint_cursor_class = xaccSplitRegisterGetCurrentCursorClass(reg);
  info->hint_set_by_traverse = FALSE;
  info->traverse_to_new = FALSE;
  info->exact_traversal = FALSE;
  info->first_pass = FALSE;

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
LoadXferCell (ComboCell * cell,  
              AccountGroup * grp,
              const gnc_commodity * base_currency,
              const gnc_commodity * base_security)
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
    const gnc_commodity * curr, * secu;

    curr = xaccAccountGetCurrency (acc);
    secu = xaccAccountGetSecurity (acc);

    DEBUG ("curr=%p secu=%p acct=%s\n", 
           curr, secu, xaccAccountGetName (acc));

    if (load_everything || 
        (gnc_commodity_equiv(curr,base_currency)) ||
        (gnc_commodity_equiv(curr,base_security)) ||
        (secu && (gnc_commodity_equiv(secu,base_currency))) ||
        (secu && (gnc_commodity_equiv(secu,base_security))))
    {
      name = xaccAccountGetFullName (acc, account_separator);
      if (name != NULL)
      {
        xaccAddComboCellMenuItem (cell, name);
        g_free(name);
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
  const gnc_commodity * curr, * secu;

  curr = xaccAccountGetCurrency (base_account);
  secu = xaccAccountGetSecurity (base_account);

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

/* ======================================================== */

gboolean
xaccSRCheckReconciled (SplitRegister *reg)
{
  Split *split;
  guint32 changed;
  gboolean confirm;
  char *message = _("You are about to change a reconciled transaction.\n"
                    "Are you sure you want to do that?");

  if (reg == NULL)
    return TRUE;

  changed = xaccSplitRegisterGetChangeFlag(reg);
  if (!changed)
    return TRUE;

  split = xaccSRGetCurrentSplit(reg);
  if (split == NULL)
    return TRUE;

  switch (xaccSplitGetReconcile (split))
  {
    case YREC:
    case FREC:
      break;
    default:
      return TRUE;
  }

  confirm = gnc_lookup_boolean_option("Register",
                                      "Confirm before changing reconciled",
                                      TRUE);
  if (!confirm)
    return TRUE;

  return gnc_verify_dialog_parented(xaccSRGetParent(reg), message, FALSE);
}

/* ======================================================== */

void
xaccSRShowPresentDivider (SplitRegister *reg, gboolean show_present)
{
  SRInfo *info = xaccSRGetInfo(reg);

  if (reg == NULL)
    return;

  info->show_present_divider = show_present;
}

/* ======================================================== */

void
xaccSetSplitRegisterColorizeNegative (gboolean use_red)
{
  use_red_for_negative = use_red;
}

/* =======================  end of file =================== */
