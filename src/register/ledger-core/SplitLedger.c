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
 * Provide view for SplitRegister object.
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
 * go back to how things were before you started the edit. Think of it as
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
#include "Scrub.h"
#include "SplitLedger.h"
#include "combocell.h"
#include "datecell.h"
#include "global-options.h"
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"
#include "split-register-p.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "guile-util.h"
#include "messages.h"
#include "numcell.h"
#include "pricecell.h"
#include "quickfillcell.h"
#include "recncell.h"
#include "splitreg.h"
#include "table-allgui.h"


/** static variables ******************************************************/

/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_LEDGER;

/* The reverse balance callback, if any. */
static SRReverseBalanceCallback reverse_balance = NULL;

/* The copied split or transaction, if any */
static CursorClass copied_class = CURSOR_CLASS_NONE;
static SCM copied_item = SCM_UNDEFINED;
static GUID copied_leader_guid;

/* Flag for determining colorization of negative amounts. */
static gboolean use_red_for_negative = TRUE;


/** static prototypes *****************************************************/

static gboolean xaccSRSaveRegEntryToSCM (SplitRegister *reg,
                                         SCM trans_scm, SCM split_scm,
                                         gboolean use_cut_semantics);
static void xaccSRActuallySaveChangedCells (SplitRegister *reg,
                                            Transaction *trans, Split *split);
static void sr_set_last_num (SplitRegister *reg, const char *num);
static void sr_split_auto_calc (SplitRegister *reg, Split *split);


/** implementations *******************************************************/

static void
xaccSRDestroyRegisterData (SplitRegister *reg)
{
  if (reg == NULL)
    return;

  g_free (reg->user_data);

  reg->user_data = NULL;
}

void
xaccSRSetData (SplitRegister *reg, void *user_data,
               SRGetParentCallback get_parent,
               SRSetHelpCallback set_help)
{
  SRInfo *info = xaccSRGetInfo (reg);

  g_return_if_fail (reg != NULL);

  info->user_data = user_data;
  info->get_parent = get_parent;
  info->set_help = set_help;
}

void
xaccSRSetReverseBalanceCallback (SRReverseBalanceCallback callback)
{
  reverse_balance = callback;
}

static int
gnc_trans_split_index (Transaction *trans, Split *split)
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
void
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

static int
gnc_split_get_value_denom (Split *split)
{
  gnc_commodity *currency;
  int denom;

  currency = xaccTransGetCurrency (xaccSplitGetParent (split));
  denom = gnc_commodity_get_fraction (currency);
  if (denom == 0)
  {
    gnc_commodity *commodity = gnc_default_currency ();
    denom = gnc_commodity_get_fraction (commodity);
    if (denom == 0)
      denom = 100;
  }

  return denom;
}

static int
gnc_split_get_amount_denom (Split *split)
{
  int denom;

  denom = xaccAccountGetCommoditySCU (xaccSplitGetAccount (split));
  if (denom == 0)
  {
    gnc_commodity *commodity = gnc_default_currency ();
    denom = gnc_commodity_get_fraction (commodity);
    if (denom == 0)
      denom = 100;
  }

  return denom;
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

  info->trans_expanded = expand;

  gnc_table_set_virt_cell_cursor (reg->table,
                                  reg->table->current_cursor_loc.vcell_loc,
                                  sr_get_active_cursor (reg));

  xaccSRSetTransVisible (reg, reg->table->current_cursor_loc.vcell_loc,
                         expand, FALSE);

  {
    VirtualLocation virt_loc;

    virt_loc = reg->table->current_cursor_loc;

    if (!expand || !gnc_table_virtual_loc_valid (reg->table, virt_loc, FALSE))
    {
      if (gnc_table_find_close_valid_cell (reg->table, &virt_loc, FALSE))
        gnc_table_move_cursor_gui (reg->table, virt_loc);
      else
      {
        PERR ("Can't find place to go!");
        return;
      }
    }
  }

  gnc_table_refresh_gui (reg->table, TRUE);

  if (expand)
    xaccSRShowTrans (reg, reg->table->current_cursor_loc.vcell_loc);
}

gboolean
xaccSRCurrentTransExpanded (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo (reg);

  if (!reg)
    return FALSE;

  if (reg->style == REG_STYLE_AUTO_LEDGER ||
      reg->style == REG_STYLE_JOURNAL)
    return FALSE;

  return info->trans_expanded;
}

/* ======================================================== */

static void
LedgerSetHelp (Table *table, const char *help_str)
{
  /* FIXME */
  return;
#if 0
  SplitRegister *reg = table->user_data;
  SRInfo *info = xaccSRGetInfo(reg);

  if (info->set_help == NULL)
    return;

  info->set_help(info->user_data, help_str);
#endif
}

/* ======================================================== */

static void
LedgerDestroy (SplitRegister *reg)
{
   SRInfo *info = xaccSRGetInfo(reg);
   Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
   Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
   Transaction *trans;

   gnc_suspend_gui_refresh ();

   /* be sure to destroy the "blank split" */
   if (blank_split != NULL)
   {
      /* split destroy will automatically remove it
       * from its parent account */
      trans = xaccSplitGetParent (blank_split);

      /* Make sure we don't commit this below */
      if (trans == pending_trans)
      {
        info->pending_trans_guid = *xaccGUIDNULL ();
        pending_trans = NULL;
      }

      xaccTransBeginEdit (trans);
      xaccTransDestroy (trans);
      xaccTransCommitEdit (trans);

      info->blank_split_guid = *xaccGUIDNULL ();
      blank_split = NULL;
   }

   /* be sure to take care of any open transactions */
   if (pending_trans != NULL)
   {
      if (xaccTransIsOpen (pending_trans))
        xaccTransCommitEdit (pending_trans);

      info->pending_trans_guid = *xaccGUIDNULL ();
      pending_trans = NULL;
   }

   xaccSRDestroyRegisterData (reg);

   gnc_resume_gui_refresh ();
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

  return xaccSplitGetParent (split);
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

Split *
xaccSRDuplicateCurrent (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  CursorClass cursor_class;
  Transaction *trans;
  Split *return_split;
  Split *trans_split;
  gboolean changed;
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

  changed = gnc_table_current_cursor_changed (reg->table, FALSE);

  /* See if we were asked to duplicate an unchanged blank split.
   * There's no point in doing that! */
  if (!changed && ((split == NULL) || (split == blank_split)))
    return NULL;

  gnc_suspend_gui_refresh ();

  /* If the cursor has been edited, we are going to have to commit
   * it before we can duplicate. Make sure the user wants to do that. */
  if (changed)
  {
    const char *message = _("The current transaction has been changed.\n"
                            "Would you like to record it?");
    GNCVerifyResult result;

    result = gnc_ok_cancel_dialog_parented (xaccSRGetParent(reg),
                                            message, GNC_VERIFY_OK);

    if (result == GNC_VERIFY_CANCEL)
    {
      gnc_resume_gui_refresh ();
      return NULL;
    }

    xaccSRSaveRegEntry (reg, TRUE);

    /* If the split is NULL, then we were on a blank split row
     * in an expanded transaction. The new split (created by
     * xaccSRSaveRegEntry above) will be the last split in the
     * current transaction, as it was just added. */
    if (split == NULL)
      split = xaccTransGetSplit (trans, xaccTransCountSplits (trans) - 1);
  }

  /* Ok, we are now ready to make the copy. */

  if (cursor_class == CURSOR_CLASS_SPLIT)
  {
    Split *new_split;

    /* We are on a split in an expanded transaction.
     * Just copy the split and add it to the transaction. */

    new_split = xaccMallocSplit ();

    xaccTransBeginEdit (trans);
    xaccTransAppendSplit (trans, new_split);
    gnc_copy_split_onto_split (split, new_split, FALSE);
    xaccTransCommitEdit (trans);

    return_split = new_split;

    info->cursor_hint_split = new_split;
    info->cursor_hint_cursor_class = CURSOR_CLASS_SPLIT;
  }
  else
  {
    Transaction *new_trans;
    int trans_split_index;
    int split_index;
    const char *in_num = NULL;
    char *out_num;
    time_t date;

    /* We are on a transaction row. Copy the whole transaction. */

    date = info->last_date_entered;
    if (gnc_strisnum (xaccTransGetNum (trans)))
    {
      Account *account = sr_get_default_account (reg);

      if (account)
        in_num = xaccAccountGetLastNum (account);
      else
        in_num = xaccTransGetNum (trans);
    }

    if (!gnc_dup_trans_dialog (xaccSRGetParent (reg),
                               &date, in_num, &out_num))
    {
      gnc_resume_gui_refresh ();
      return NULL;
    }

    split_index = gnc_trans_split_index (trans, split);
    trans_split_index = gnc_trans_split_index (trans, trans_split);

    /* we should *always* find the split, but be paranoid */
    if (split_index < 0)
    {
      gnc_resume_gui_refresh ();
      return NULL;
    }

    new_trans = xaccMallocTransaction ();

    xaccTransBeginEdit (new_trans);
    gnc_copy_trans_onto_trans (trans, new_trans, FALSE, FALSE);
    xaccTransSetDateSecs (new_trans, date);
    xaccTransSetNum (new_trans, out_num);
    xaccTransCommitEdit (new_trans);

    if (xaccSetNumCellLastNum ((NumCell *)
                               gnc_register_get_cell (reg, NUM_CELL), out_num))
      sr_set_last_num (reg, out_num);

    g_free (out_num);

    /* This shouldn't happen, but be paranoid. */
    if (split_index >= xaccTransCountSplits (new_trans))
      split_index = 0;

    return_split = xaccTransGetSplit (new_trans, split_index);
    trans_split = xaccTransGetSplit (new_trans, trans_split_index);

    info->cursor_hint_trans = new_trans;
    info->cursor_hint_split = return_split;
    info->cursor_hint_trans_split = trans_split;
    info->cursor_hint_cursor_class = CURSOR_CLASS_TRANS;

    info->trans_expanded = FALSE;
  }

  /* Refresh the GUI. */
  gnc_resume_gui_refresh ();

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
  gboolean changed;
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

  changed = gnc_table_current_cursor_changed (reg->table, FALSE);

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

      copied_leader_guid = info->default_account;
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
  gboolean changed;
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

  changed = gnc_table_current_cursor_changed (reg->table, FALSE);

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

  if (cursor_class == CURSOR_CLASS_SPLIT)
  {
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

    gnc_suspend_gui_refresh ();

    xaccTransBeginEdit(trans);
    if (split == NULL)
    { /* We are on a null split in an expanded transaction. */
      split = xaccMallocSplit();
      xaccTransAppendSplit(trans, split);
    }

    gnc_copy_split_scm_onto_split(copied_item, split);
    xaccTransCommitEdit(trans);
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

    gnc_suspend_gui_refresh ();

    /* in pasting, the old split is deleted. */
    if (split == blank_split)
    {
      info->blank_split_guid = *xaccGUIDNULL();
      blank_split = NULL;
    }

    split_index = gnc_trans_split_index(trans, split);
    trans_split_index = gnc_trans_split_index(trans, trans_split);

    if ((sr_get_default_account (reg) != NULL) &&
        (xaccGUIDType(&copied_leader_guid) != GNC_ID_NULL))
    {
      new_guid = &info->default_account;
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

  /* Refresh the GUI. */
  gnc_resume_gui_refresh ();
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

  gnc_suspend_gui_refresh ();

  /* make a copy of all of the accounts that will be  
   * affected by this deletion, so that we can update
   * their register windows after the deletion. */
  trans = xaccSplitGetParent(split);

  account = xaccSplitGetAccount(split);

  xaccTransBeginEdit(trans);
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

  gnc_resume_gui_refresh ();
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

    gnc_suspend_gui_refresh ();

    xaccTransBeginEdit (trans);
    xaccTransDestroy (trans);
    xaccTransCommitEdit (trans);

    info->blank_split_guid = *xaccGUIDNULL();
    blank_split = NULL;

    gnc_resume_gui_refresh ();

    return;
  }

  info->trans_expanded = FALSE;

  gnc_suspend_gui_refresh ();

  /* make a copy of all of the accounts that will be  
   * affected by this deletion, so that we can update
   * their register windows after the deletion. */
  trans = xaccSplitGetParent(split);

  xaccTransBeginEdit(trans);
  xaccTransDestroy(trans);
  xaccTransCommitEdit(trans);

  /* Check pending transaction */
  if (trans == pending_trans)
  {
    info->pending_trans_guid = *xaccGUIDNULL();
    pending_trans = NULL;
  }

  gnc_resume_gui_refresh ();
}

/* ======================================================== */

void
xaccSREmptyCurrentTrans (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo (reg);
  Split *blank_split = xaccSplitLookup (&info->blank_split_guid);
  Transaction *pending_trans = xaccTransLookup (&info->pending_trans_guid);
  Transaction *trans;
  Account *account;
  GList *splits;
  GList *node;
  Split *split;

  /* get the current split based on cursor position */
  split = xaccSRGetCurrentSplit (reg);
  if (split == NULL)
    return;

  /* If we just deleted the blank split, clean up. The user is
   * allowed to delete the blank split as a method for discarding
   * any edits they may have made to it. */
  if (split == blank_split)
  {
    trans = xaccSplitGetParent (blank_split);
    account = xaccSplitGetAccount (split);

    /* Make sure we don't commit this later on */
    if (trans == pending_trans)
    {
      info->pending_trans_guid = *xaccGUIDNULL ();
      pending_trans = NULL;
    }

    gnc_suspend_gui_refresh ();

    xaccTransBeginEdit (trans);
    xaccTransDestroy (trans);
    xaccTransCommitEdit (trans);

    info->blank_split_guid = *xaccGUIDNULL ();
    blank_split = NULL;

    gnc_resume_gui_refresh ();
    return;
  }

  gnc_suspend_gui_refresh ();

  trans = xaccSplitGetParent (split);

  splits = g_list_copy (xaccTransGetSplitList (trans));

  xaccTransBeginEdit (trans);
  for (node = splits; node; node = node->next)
    if (node->data != split)
      xaccSplitDestroy (node->data);
  xaccTransCommitEdit (trans);

  /* Check pending transaction */
  if (trans == pending_trans)
  {
    info->pending_trans_guid = *xaccGUIDNULL ();
    pending_trans = NULL;
  }

  gnc_resume_gui_refresh ();

  g_list_free (splits);
}

/* ======================================================== */

void
xaccSRCancelCursorSplitChanges (SplitRegister *reg)
{
  VirtualLocation virt_loc;

  if (reg == NULL)
    return;

  virt_loc = reg->table->current_cursor_loc;

  if (!gnc_table_current_cursor_changed (reg->table, FALSE))
    return;

  /* We're just cancelling the current split here, not the transaction.
   * When cancelling edits, reload the cursor from the transaction. */
  gnc_table_clear_current_cursor_changes (reg->table);

  if (gnc_table_find_close_valid_cell (reg->table, &virt_loc, FALSE))
    gnc_table_move_cursor_gui (reg->table, virt_loc);

  gnc_table_refresh_gui (reg->table, TRUE);
}

/* ======================================================== */

void
xaccSRCancelCursorTransChanges (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo(reg);
  Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);

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

  gnc_suspend_gui_refresh ();

  xaccTransRollbackEdit (pending_trans);

  info->pending_trans_guid = *xaccGUIDNULL ();

  gnc_resume_gui_refresh ();
}

/* ======================================================== */

void
xaccSRRedrawReg (SplitRegister *reg) 
{
  xaccLedgerDisplayRefreshByReg (reg);
}

/* ======================================================== */
/* Copy from the register object to scheme. This needs to be
 * in sync with xaccSRSaveRegEntry and xaccSRSaveChangedCells. */


/* jsled: This will need to be modified, as well. */
static gboolean
xaccSRSaveRegEntryToSCM (SplitRegister *reg, SCM trans_scm, SCM split_scm,
                         gboolean use_cut_semantics)
{
  SCM other_split_scm = SCM_UNDEFINED;
  Transaction *trans;

  /* use the changed flag to avoid heavy-weight updates
   * of the split & transaction fields. This will help
   * cut down on uneccessary register redraws. */
  if (!gnc_table_current_cursor_changed (reg->table, FALSE))
    return FALSE;

  /* get the handle to the current split and transaction */
  trans = xaccSRGetCurrentTrans (reg);
  if (trans == NULL)
    return FALSE;

  /* copy the contents from the cursor to the split */
  if (gnc_register_get_cell_changed (reg, DATE_CELL, TRUE))
  {
    BasicCell *cell;
    Timespec ts;

    cell = gnc_register_get_cell (reg, DATE_CELL);
    xaccDateCellGetDate((DateCell *) cell, &ts);

    gnc_trans_scm_set_date(trans_scm, &ts);
  }

  if (gnc_register_get_cell_changed (reg, NUM_CELL, TRUE))
  {
    const char *value;

    value = gnc_register_get_cell_value (reg, NUM_CELL);
    gnc_trans_scm_set_num (trans_scm, value);
  }

  if (gnc_register_get_cell_changed (reg, DESC_CELL, TRUE))
  {
    const char *value;

    value = gnc_register_get_cell_value (reg, DESC_CELL);
    gnc_trans_scm_set_description (trans_scm, value);
  }

  if (gnc_register_get_cell_changed (reg, NOTES_CELL, TRUE))
  {
    const char *value;

    value = gnc_register_get_cell_value (reg, NOTES_CELL);
    gnc_trans_scm_set_notes (trans_scm, value);
  }

  if (gnc_register_get_cell_changed (reg, RECN_CELL, TRUE))
  {
    BasicCell *cell;
    char flag;

    cell = gnc_register_get_cell (reg, RECN_CELL);
    flag = xaccRecnCellGetFlag ((RecnCell *) cell);

    gnc_split_scm_set_reconcile_state(split_scm, flag);
  }

  if (gnc_register_get_cell_changed (reg, ACTN_CELL, TRUE))
  {
    const char *value;

    value = gnc_register_get_cell_value (reg, ACTN_CELL);
    gnc_split_scm_set_action (split_scm, value);
  }

  if (gnc_register_get_cell_changed (reg, MEMO_CELL, TRUE))
  {
    const char *value;

    value = gnc_register_get_cell_value (reg, MEMO_CELL);
    gnc_split_scm_set_memo (split_scm, value);
  }

  if (gnc_register_get_cell_changed (reg, XFRM_CELL, TRUE))
  {
    Account *new_account;
    const char *new_name;

    new_name = gnc_register_get_cell_value (reg, XFRM_CELL);

    new_account = xaccGetAccountFromFullName (gncGetCurrentGroup (),
                                              new_name,
                                              gnc_get_account_separator ());

    if (new_account != NULL)
      gnc_split_scm_set_account (split_scm, new_account);
  }

  if (reg->style == REG_STYLE_LEDGER)
    other_split_scm = gnc_trans_scm_get_other_split_scm (trans_scm, split_scm);

  if (gnc_register_get_cell_changed (reg, MXFRM_CELL, TRUE))
  {
    other_split_scm = gnc_trans_scm_get_other_split_scm (trans_scm, split_scm);

    if (other_split_scm == SCM_UNDEFINED)
    {
      if (gnc_trans_scm_get_num_splits(trans_scm) == 1)
      {
        Split *temp_split;

        temp_split = xaccMallocSplit ();
        other_split_scm = gnc_copy_split (temp_split, use_cut_semantics);
        xaccSplitDestroy (temp_split);

        gnc_trans_scm_append_split_scm (trans_scm, other_split_scm);
      }
    }

    if (other_split_scm != SCM_UNDEFINED)
    {
      Account *new_account;
      const char *name;

      name = gnc_register_get_cell_value (reg, MXFRM_CELL);

      new_account = xaccGetAccountFromFullName (gncGetCurrentGroup (),
                                                name,
                                                gnc_get_account_separator ());

      if (new_account != NULL)
        gnc_split_scm_set_account (other_split_scm, new_account);
    }
  }

  if (gnc_register_get_cell_changed (reg, DEBT_CELL, TRUE) ||
      gnc_register_get_cell_changed (reg, CRED_CELL, TRUE))
  {
    BasicCell *cell;
    gnc_numeric new_value;
    gnc_numeric credit;
    gnc_numeric debit;

    cell = gnc_register_get_cell (reg, CRED_CELL);
    credit = xaccGetPriceCellValue ((PriceCell *) cell);

    cell = gnc_register_get_cell (reg, DEBT_CELL);
    debit = xaccGetPriceCellValue ((PriceCell *) cell);

    new_value = gnc_numeric_sub_fixed (debit, credit);

    gnc_split_scm_set_value (split_scm, new_value);
  }

  if (gnc_register_get_cell_changed (reg, PRIC_CELL, TRUE))
  {
    /* do nothing for now */
  }

  if (gnc_register_get_cell_changed (reg, SHRS_CELL, TRUE))
  {
    BasicCell *cell;
    gnc_numeric shares;

    cell = gnc_register_get_cell (reg, SHRS_CELL);

    shares = xaccGetPriceCellValue ((PriceCell *) cell);

    gnc_split_scm_set_amount (split_scm, shares);
  }

  if (gnc_register_get_cell_changed (reg, DEBT_CELL, TRUE) ||
      gnc_register_get_cell_changed (reg, CRED_CELL, TRUE) ||
      gnc_register_get_cell_changed (reg, PRIC_CELL, TRUE) ||
      gnc_register_get_cell_changed (reg, SHRS_CELL, TRUE))
  {
    if (other_split_scm != SCM_UNDEFINED)
    {
      gnc_numeric num;

      num = gnc_split_scm_get_amount (split_scm);
      gnc_split_scm_set_amount (other_split_scm, gnc_numeric_neg (num));

      num = gnc_split_scm_get_value (split_scm);
      gnc_split_scm_set_value (other_split_scm, gnc_numeric_neg (num));
    }
  }

  return TRUE;
}

/* ======================================================== */
/* Copy from the register object to the engine */
/* jsled: okay... the fun.  actually, not really the fun, but scan
   this to see if anything jumps out */
gboolean
xaccSRSaveRegEntry (SplitRegister *reg, gboolean do_commit)
{
   SRInfo *info = xaccSRGetInfo(reg);
   Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
   Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);
   Transaction *blank_trans = xaccSplitGetParent(blank_split);
   Transaction *trans;
   const char *memo;
   const char *desc;
   Split *split;

   /* get the handle to the current split and transaction */
   split = xaccSRGetCurrentSplit (reg);
   trans = xaccSRGetCurrentTrans (reg);
   if (trans == NULL)
     return FALSE;

   /* use the changed flag to avoid heavy-weight updates
    * of the split & transaction fields. This will help
    * cut down on uneccessary register redraws. */
   if (!gnc_table_current_cursor_changed (reg->table, FALSE))
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

   gnc_suspend_gui_refresh ();

   /* determine whether we should commit the pending transaction */
   if (pending_trans != trans)
   {
     if (xaccTransIsOpen (pending_trans))
       xaccTransCommitEdit (pending_trans);

     xaccTransBeginEdit (trans);
     pending_trans = trans;
     info->pending_trans_guid = *xaccTransGetGUID(trans);
   }

   /* If we are committing the blank split, add it to the account now */
   if (trans == blank_trans)
   {
     xaccAccountInsertSplit (sr_get_default_account (reg), blank_split);
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

   if ( reg->template ) {
           xaccSRSaveChangedTemplateCells( reg, trans, split );
   } else {
           xaccSRSaveChangedCells (reg, trans, split);
   }

   memo = xaccSplitGetMemo (split);
   memo = memo ? memo : "(null)";
   desc = xaccTransGetDescription (trans);
   desc = desc ? desc : "(null)";
   PINFO ("finished saving split %s of trans %s \n", memo, desc);

   /* If the modified split is the "blank split", then it is now an
    * official part of the account. Set the blank split to NULL, so
    * we can be sure of getting a new split. Also, save the date for
    * the new blank split. */
   if (trans == blank_trans)
   {
     if (do_commit)
     {
       info->blank_split_guid = *xaccGUIDNULL ();
       blank_split = NULL;
       info->last_date_entered = xaccTransGetDate (trans);
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
       info->pending_trans_guid = *xaccGUIDNULL ();
     }
   }

   gnc_table_clear_current_cursor_changes (reg->table);

   gnc_resume_gui_refresh ();

   return TRUE;
}

/* ======================================================== */

static void
sr_set_last_num (SplitRegister *reg, const char *num)
{
  Account *account;

  account = sr_get_default_account (reg);
  if (!account)
    return;

  xaccAccountSetLastNum (account, num);
}

/* ======================================================== */

static void
sr_split_auto_calc (SplitRegister *reg, Split *split)
{
  PriceCell *cell;
  gboolean recalc_shares = FALSE;
  gboolean recalc_price = FALSE;
  gboolean recalc_value = FALSE;
  GNCAccountType account_type;
  gboolean price_changed;
  gboolean amount_changed;
  gboolean shares_changed;
  gnc_numeric calc_value;
  gnc_numeric value;
  gnc_numeric price;
  gnc_numeric amount;
  Account *account;
  int denom;

  /*  First, check if this is an account other than STOCK or
   *  MUTUAL type. If it is, this is a bank balancing split, so
   *  don't recalc anything.*/

  account = xaccSplitGetAccount (split);
  account_type = xaccAccountGetType (account);

  if (account_type != STOCK  &&
      account_type != MUTUAL &&
      account_type != CURRENCY)
    return;

  price_changed = gnc_register_get_cell_changed (reg, PRIC_CELL, TRUE);
  amount_changed = (gnc_register_get_cell_changed (reg, DEBT_CELL, TRUE) ||
                    gnc_register_get_cell_changed (reg, CRED_CELL, TRUE));
  shares_changed = gnc_register_get_cell_changed (reg, SHRS_CELL, TRUE);

  if (shares_changed)
  {
    cell = (PriceCell *) gnc_register_get_cell (reg, SHRS_CELL);
    amount = xaccGetPriceCellValue (cell);
  }
  else
    amount = xaccSplitGetAmount (split);

  if (price_changed)
  {
    cell = (PriceCell *) gnc_register_get_cell (reg, PRIC_CELL);
    price = xaccGetPriceCellValue (cell);
  }
  else
    price = xaccSplitGetSharePrice (split);

  if (amount_changed)
  {
    gnc_numeric credit;
    gnc_numeric debit;

    cell = (PriceCell *) gnc_register_get_cell (reg, CRED_CELL);
    credit = xaccGetPriceCellValue (cell);

    cell = (PriceCell *) gnc_register_get_cell (reg, DEBT_CELL);
    debit = xaccGetPriceCellValue (cell);

    value = gnc_numeric_sub_fixed (debit, credit);
  }
  else
    value = xaccSplitGetValue (split);

  /* Check if precisely one value is zero. If so, we can assume that the
   * zero value needs to be recalculated.   */

  if (!gnc_numeric_zero_p (amount))
  {
    if (gnc_numeric_zero_p (price))
    {
      if (!gnc_numeric_zero_p (value))
        recalc_price = TRUE;
    }
    else if (gnc_numeric_zero_p (value))
      recalc_value = TRUE;
  }
  else if (!gnc_numeric_zero_p (price))
    if (!gnc_numeric_zero_p (value))
      recalc_shares = TRUE;

  /* If we have not already flagged a recalc, check if this is a split
   * which has 2 of the 3 values changed. */

  if((!recalc_shares) &&
     (!recalc_price)  &&
     (!recalc_value))
  {
    if (price_changed && amount_changed)
    {
      if (!shares_changed)
        recalc_shares = TRUE;
    }
    else if (amount_changed && shares_changed)
      recalc_price = TRUE;
    else if (price_changed && shares_changed)
      recalc_value = TRUE;
  }

  calc_value = gnc_numeric_mul (price, amount, GNC_DENOM_AUTO, GNC_DENOM_LCD);

  denom = gnc_split_get_value_denom (split);

  /*  Now, if we have not flagged one of the recalcs, and value and
   *  calc_value are not the same number, then we need to ask for
   *  help from the user. */

  if (!recalc_shares &&
      !recalc_price &&
      !recalc_value &&
      !gnc_numeric_same (value, calc_value, denom, GNC_RND_ROUND))
  {
    int choice;
    int default_value;
    GList *node;
    GList *radio_list = NULL;
    const char *title = _("Recalculate Transaction");
    const char *message = _("The values entered for this transaction "
                            "are inconsistent.\nWhich value would you "
                            "like to have recalculated?");

    if (shares_changed)
      radio_list = g_list_append (radio_list,
                                  g_strdup_printf ("%s (%s)",
                                                   _("Shares"), _("Changed")));
    else
      radio_list = g_list_append (radio_list, g_strdup (_("Shares")));

    if (price_changed)
      radio_list = g_list_append (radio_list,
                                  g_strdup_printf ("%s (%s)",
                                                   _("Price"), _("Changed")));
    else
      radio_list = g_list_append (radio_list, g_strdup (_("Price")));

    if (amount_changed)
      radio_list = g_list_append (radio_list,
                                  g_strdup_printf ("%s (%s)",
                                                   _("Value"), _("Changed")));
    else
      radio_list = g_list_append (radio_list, g_strdup (_("Value")));

    if (!price_changed)
      default_value = 1;
    else if (!shares_changed)
      default_value = 0;
    else if (!amount_changed)
      default_value = 2;
    else
      default_value = 1;

    choice = gnc_choose_radio_option_dialog_parented (xaccSRGetParent(reg),
                                                      title,
                                                      message,
                                                      default_value,
                                                      radio_list);
    for (node = radio_list; node; node = node->next)
      g_free (node->data);

    g_list_free (radio_list);

    switch(choice)
    {
      case 0: /* Modify number of shares */
        recalc_shares = TRUE;
        break;
      case 1: /* Modify the share price */
        recalc_price = TRUE;
        break;
      case 2: /* Modify total value */
        recalc_value = TRUE;
        break;
      default:
        break;
    }
  }

  if (recalc_shares)
    if (!gnc_numeric_zero_p (price))
    {
      BasicCell *cell;

      denom = gnc_split_get_amount_denom (split);

      amount = gnc_numeric_div (value, price, denom, GNC_RND_ROUND);

      cell = gnc_register_get_cell (reg, SHRS_CELL);
      xaccSetPriceCellValue ((PriceCell *) cell, amount);
      gnc_basic_cell_set_changed (cell, TRUE);

      if (amount_changed)
      {
        cell = gnc_register_get_cell (reg, PRIC_CELL);
        gnc_basic_cell_set_changed (cell, FALSE);
      }
    }

  if (recalc_price)
    if (!gnc_numeric_zero_p (amount))
    {
      BasicCell *price_cell;

      price = gnc_numeric_div (value, amount,
                               GNC_DENOM_AUTO,
                               GNC_DENOM_EXACT);

      if (gnc_numeric_negative_p (price))
      {
        BasicCell *debit_cell;
        BasicCell *credit_cell;

        debit_cell = gnc_register_get_cell (reg, DEBT_CELL);
        credit_cell = gnc_register_get_cell (reg, CRED_CELL);

        price = gnc_numeric_neg (price);

        xaccSetDebCredCellValue ((PriceCell *) debit_cell,
                                 (PriceCell *) credit_cell,
                                 gnc_numeric_neg (value));

        gnc_basic_cell_set_changed (debit_cell, TRUE);
        gnc_basic_cell_set_changed (credit_cell, TRUE);
      }

      price_cell = gnc_register_get_cell (reg, PRIC_CELL);
      xaccSetPriceCellValue ((PriceCell *) price_cell, price);
      gnc_basic_cell_set_changed (price_cell, TRUE);
    }

  if (recalc_value)
  {
    BasicCell *debit_cell;
    BasicCell *credit_cell;

    debit_cell = gnc_register_get_cell (reg, DEBT_CELL);
    credit_cell = gnc_register_get_cell (reg, CRED_CELL);

    denom = gnc_split_get_value_denom (split);

    value = gnc_numeric_mul (price, amount, denom, GNC_RND_ROUND);

    xaccSetDebCredCellValue ((PriceCell *) debit_cell,
                             (PriceCell *) credit_cell, value);

    gnc_basic_cell_set_changed (debit_cell, TRUE);
    gnc_basic_cell_set_changed (credit_cell, TRUE);

    if (shares_changed)
    {
      BasicCell *cell;

      cell = gnc_register_get_cell (reg, PRIC_CELL);
      gnc_basic_cell_set_changed (cell, FALSE);
    }
  }
}

/* ======================================================== */

void
xaccSRSaveChangedTemplateCells( SplitRegister *reg,
                                Transaction *trans,
                                Split *split )
{
  SRInfo	*info = xaccSRGetInfo( reg );
  Split		*other_split = NULL;
  kvp_frame	*kvpf;
  AccountGroup	*template_ag;
  Account	*template_acc;
  kvp_value	*tag_val;
  BasicCell     *cell;

  DEBUG (" ");

  template_acc = reg->templateAcct;

  if ( gnc_register_get_cell_changed (reg, DATE_CELL, TRUE) ||
       gnc_register_get_cell_changed (reg, NUM_CELL, TRUE)  ||
       gnc_register_get_cell_changed (reg, RECN_CELL, TRUE) )
  {
    PERR( "unexpected changed fields in a template register\n" );
  }

  /* We'll be using the Split's KVP frame a lot */
  kvpf = xaccSplitGetSlots( split );

  if ( gnc_register_get_cell_changed (reg, XFRM_CELL, TRUE) )
  {
    /* FIXME: This should probably do the same checks as
       xaccSRSaveChangedCells regarding account types
       [between different currency accounts, mainly] */
    const char        *new_name;
    Account           *acct;
    AccountGroup      *acctGrp;
    const GUID        *acctGUID;

    /* save the account GUID into the kvp_data. */
    new_name = gnc_register_get_cell_value (reg, XFRM_CELL);
    acctGrp = gnc_book_get_group (gncGetCurrentBook());
    acct = xaccGetAccountFromFullName (acctGrp, new_name,
                                       gnc_get_account_separator());
    if ( acct == NULL ) {
      PERR( "unknown group \"%s\"\n", new_name );
      return;
    }
    acctGUID = xaccAccountGetGUID( acct );
    kvp_frame_set_slot( kvpf, "sched-xaction/xfrm",
                        kvp_value_new_guid( acctGUID ) );
    kvpf = xaccSplitGetSlots( split );

    cell = gnc_register_get_cell (reg, XFRM_CELL);
    gnc_basic_cell_set_changed (cell, FALSE);

    /* DEBUG */
    if ( 0 ) {
      GList *foo = xaccAccountGetSplitList( template_acc );
      if ( foo ) {
        printf( "Splits:\n" );
        do {
          printf ("\tsplit: \"%s\"\n", xaccSplitGetMemo( (Split*)foo->data ) );
        } while ( (foo = foo->next) );
      } else {
        printf( "No Splits.\n" );
      }
    }

    /* set the actual account to the fake account for these templates */
    xaccAccountInsertSplit (template_acc, split);

    /* DEBUG */
    if ( 0 ) { 
      GList *foo = xaccAccountGetSplitList( template_acc );
      if ( foo ) {
        printf( "Splits:\n" );
        do {
          printf ("\tsplit: \"%s\"\n", xaccSplitGetMemo( (Split*)foo->data ) );
        } while ( (foo = foo->next) );
      } else {
        printf( "No Splits.\n" );
      }
    }
  }

  if ( gnc_register_get_cell_changed (reg, MXFRM_CELL, TRUE) )
  {
    DEBUG( "Template: Got MXFRM changed\n" );

    cell = gnc_register_get_cell (reg, MXFRM_CELL);
    gnc_basic_cell_set_changed (cell, FALSE);
  }

  if ( gnc_register_get_cell_changed (reg, FCRED_CELL, TRUE) ||
       gnc_register_get_cell_changed (reg, FDEBT_CELL, TRUE) )
  {
    const char *value;
    char *amountStr = "x + y/42";
    gnc_numeric new_amount;
    gnc_numeric credit;
    gnc_numeric debit;

    DEBUG( "kvp_frame before: %s\n", kvp_frame_to_string( kvpf ) );

    /* amountStr = gnc_numeric_to_string( new_amount ); */

    value = gnc_register_get_cell_value (reg, FCRED_CELL);
    kvp_frame_set_slot( kvpf, "sched-xaction/credit_formula",
                        kvp_value_new_string( value ) );

    value = gnc_register_get_cell_value (reg, FDEBT_CELL);
    kvp_frame_set_slot( kvpf, "sched-xaction/debit_formula",
                        kvp_value_new_string( value ) );

    DEBUG( "kvp_frame  after: %s\n", kvp_frame_to_string( kvpf ) );

    /* set the amount to an innocuous value */
    xaccSplitSetValue (split, gnc_numeric_create(0, 1) );
  }

  if ( gnc_register_get_cell_changed (reg, SHRS_CELL, TRUE) )
  {
    char       *sharesStr = "(x + y)/42";

    /* FIXME: shares cells are numeric by definition. */
    DEBUG( "kvp_frame before: %s\n", kvp_frame_to_string( kvpf ) );
              
    /* sharesStr = gnc_numeric_to_string( sharesStr ); */
    kvp_frame_set_slot( kvpf, "sched-xaction/shares",
                        kvp_value_new_string( sharesStr ) );
    DEBUG( "kvp_frame  after: %s\n", kvp_frame_to_string( kvpf ) );
    /* set the shares to an innocuous value */
    xaccSplitSetSharePriceAndAmount (split,
                                     gnc_numeric_create(0, 1),
                                     gnc_numeric_create(0, 1) );

    cell = gnc_register_get_cell (reg, SHRS_CELL);
    gnc_basic_cell_set_changed (cell, FALSE);
  }

  xaccSRActuallySaveChangedCells( reg, trans, split );
}

static void
xaccSRActuallySaveChangedCells (SplitRegister *reg,
                                Transaction *trans, Split *split)
{
  SRInfo *info = xaccSRGetInfo (reg);
  Split *other_split = NULL;

  /* copy the contents from the cursor to the split */
  if (gnc_register_get_cell_changed (reg, DATE_CELL, TRUE))
  {
    BasicCell *cell;
    const char *value;
    Timespec ts;

    cell = gnc_register_get_cell (reg, DATE_CELL);
    value = gnc_basic_cell_get_value (cell);

    /* commit any pending changes */
    xaccCommitDateCell ((DateCell *) cell);

    DEBUG ("DATE: %s", value ? value : "(null)");

    xaccDateCellGetDate ((DateCell *) cell, &ts);

    xaccTransSetDatePostedTS (trans, &ts);
  }

  if (gnc_register_get_cell_changed (reg, NUM_CELL, TRUE))
  {
    BasicCell *cell;
    const char *value;

    value = gnc_register_get_cell_value (reg, NUM_CELL);

    DEBUG ("NUM: %s\n", value ? value : "(null)");

    xaccTransSetNum (trans, value);

    cell = gnc_register_get_cell (reg, NUM_CELL);

    if (xaccSetNumCellLastNum ((NumCell *) cell, value))
    {
      SRInfo *info = xaccSRGetInfo (reg);
      Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
      Transaction *blank_trans = xaccSplitGetParent (blank_split);

      if (trans != blank_trans)
        sr_set_last_num (reg, gnc_basic_cell_get_value (cell));
    }
  }

  if (gnc_register_get_cell_changed (reg, DESC_CELL, TRUE))
  {
    const char *value;

    value = gnc_register_get_cell_value (reg, DESC_CELL);

    DEBUG ("DESC: %s", value ? value : "(null)");

    xaccTransSetDescription (trans, value);
  }

  if (gnc_register_get_cell_changed (reg, NOTES_CELL, TRUE))
  {
    const char *value;

    value = gnc_register_get_cell_value (reg, NOTES_CELL);

    DEBUG ("NOTES: %s", value ? value : "(null)");

    xaccTransSetNotes (trans, value);
  }

  if (gnc_register_get_cell_changed (reg, RECN_CELL, TRUE))
  {
    RecnCell *cell;

    cell = (RecnCell *) gnc_register_get_cell (reg, RECN_CELL);

    DEBUG ("RECN: %c", xaccRecnCellGetFlag (cell));

    xaccSplitSetReconcile (split, xaccRecnCellGetFlag (cell));
  }

  if (gnc_register_get_cell_changed (reg, ACTN_CELL, TRUE))
  {
    const char *value;

    value = gnc_register_get_cell_value (reg, ACTN_CELL);

    DEBUG ("ACTN: %s", value ? value : "(null)");

    xaccSplitSetAction (split, value);
  }

  if (gnc_register_get_cell_changed (reg, MEMO_CELL, TRUE))
  {
    const char *value;

    value = gnc_register_get_cell_value (reg, MEMO_CELL);

    DEBUG ("MEMO: %s", value ? value : "(null)");

    xaccSplitSetMemo (split, value);
  }

  /* -------------------------------------------------------------- */
  /* OK, the handling of transfers gets complicated because it depends
   * on what was displayed to the user.  For a multi-line display, we
   * just reparent the indicated split, its it, and that's that. For
   * a two-line display, we want to reparent the "other" split, but
   * only if there is one.  XFRM is the straight split, MXFRM is the
   * mirrored split. */

  /* jsled: this is where it starts to get fun. in the template
   * register, we save the XFRM account in the kvp frame.
   * also, when loading, we load from the kvp data. */
  if (gnc_register_get_cell_changed (reg, XFRM_CELL, TRUE))
  {
    Account *old_acc;
    Account *new_acc;
    const char *new_name;

    new_name = gnc_register_get_cell_value (reg, XFRM_CELL);

    DEBUG ("XFRM: %s", new_name ? new_name : "(null)");

    /* do some reparenting. Insertion into new account will automatically
     * delete this split from the old account */
    old_acc = xaccSplitGetAccount (split);

    new_acc = xaccGetAccountFromFullName (gncGetCurrentGroup (),
                                          new_name,
                                          gnc_get_account_separator ());

    if ((new_acc != NULL) && (old_acc != new_acc))
      xaccAccountInsertSplit (new_acc, split);
  }

  if (reg->style == REG_STYLE_LEDGER && !info->trans_expanded)
    other_split = xaccSplitGetOtherSplit (split);

  if (gnc_register_get_cell_changed (reg, MXFRM_CELL, TRUE))
  {
    const char *name;

    name = gnc_register_get_cell_value (reg, MXFRM_CELL);

    DEBUG ("MXFRM: %s", name ? name : "(null)");

    other_split = xaccSplitGetOtherSplit (split);

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
      Account *old_acc, *new_acc;

      /* do some reparenting. Insertion into new account will automatically
       * delete from the old account */
      old_acc = xaccSplitGetAccount (other_split);
      new_acc = xaccGetAccountFromFullName (gncGetCurrentGroup (),
                                            name,
                                            gnc_get_account_separator ());

      if ((new_acc != NULL) && (old_acc != new_acc))
        xaccAccountInsertSplit (new_acc, other_split);
    }
  }

  /* If we have a stock, currency or portfolio register type, and
   * changes have been made to the number of shares, the price, or the
   * value, then we need to do some calculations to make sure it all
   * balances properly.*/
  if ((gnc_register_get_cell_changed (reg, DEBT_CELL, TRUE) ||
       gnc_register_get_cell_changed (reg, CRED_CELL, TRUE) ||
       gnc_register_get_cell_changed (reg, PRIC_CELL, TRUE) ||
       gnc_register_get_cell_changed (reg, SHRS_CELL, TRUE)) &&
      ((STOCK_REGISTER    == (reg->type)) ||
       (CURRENCY_REGISTER == (reg->type)) ||
       (PORTFOLIO_LEDGER  == (reg->type))))
    sr_split_auto_calc (reg, split);

  if (gnc_register_get_cell_changed (reg, SHRS_CELL, TRUE))
  {
    PriceCell *cell;
    gnc_numeric amount;

    cell = (PriceCell *) gnc_register_get_cell (reg, SHRS_CELL);
    amount = xaccGetPriceCellValue (cell);

    DEBUG ("SHRS");

    xaccSplitSetAmount (split, amount);
  }

  if (gnc_register_get_cell_changed (reg, PRIC_CELL, TRUE))
  {
    PriceCell *cell;
    gnc_numeric price;

    cell = (PriceCell *) gnc_register_get_cell (reg, PRIC_CELL);
    price = xaccGetPriceCellValue (cell);

    DEBUG ("PRIC");

    xaccSplitSetSharePrice (split, price);
  }

  if (gnc_register_get_cell_changed (reg, DEBT_CELL, TRUE) ||
      gnc_register_get_cell_changed (reg, CRED_CELL, TRUE))
  {
    PriceCell *cell;
    gnc_numeric new_amount;
    gnc_numeric credit;
    gnc_numeric debit;

    cell = (PriceCell *) gnc_register_get_cell (reg, CRED_CELL);
    credit = xaccGetPriceCellValue (cell);

    cell = (PriceCell *) gnc_register_get_cell (reg, DEBT_CELL);
    debit  = xaccGetPriceCellValue (cell);

    new_amount = gnc_numeric_sub_fixed (debit, credit);

    xaccSplitSetValue (split, new_amount);
  }

  if (gnc_register_get_cell_changed (reg, DEBT_CELL, TRUE) ||
      gnc_register_get_cell_changed (reg, CRED_CELL, TRUE) ||
      gnc_register_get_cell_changed (reg, PRIC_CELL, TRUE) ||
      gnc_register_get_cell_changed (reg, SHRS_CELL, TRUE))
  {
    xaccSplitScrub (split);

    if (other_split)
    {
      gnc_numeric value = xaccSplitGetValue (split);

      value = gnc_numeric_neg (value);

      xaccSplitSetValue (other_split, value);

      xaccSplitScrub (other_split);
    }
  }
}

void
xaccSRSaveChangedCells (SplitRegister *reg, Transaction *trans, Split *split)
{
  SRInfo *info = xaccSRGetInfo (reg);
  Split *other_split = NULL;

  xaccSRActuallySaveChangedCells (reg, trans, split);
}

/* ======================================================== */

static gnc_numeric
get_trans_total_amount (SplitRegister *reg, Transaction *trans)
{
  GList *node;
  Account *account;
  gnc_numeric total = gnc_numeric_zero ();

  account = sr_get_default_account (reg);

  if (!account)
    return total;

  total = gnc_numeric_convert (total, xaccAccountGetCommoditySCU (account),
                               GNC_RND_ROUND);

  for (node = xaccTransGetSplitList (trans); node; node = node->next)
  {
    Split *split = node->data;

    if (xaccSplitGetAccount (split) != account)
      continue;

    total = gnc_numeric_add_fixed (total, xaccSplitGetAmount (split));
  }

  return total;
}

static Split *
get_trans_last_split (SplitRegister *reg, Transaction *trans)
{
  GList *node;
  Account *account;
  Split *last_split = NULL;

  account = sr_get_default_account (reg);

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
get_trans_total_balance (SplitRegister *reg, Transaction *trans)
{
  Split *last_split;

  last_split = get_trans_last_split (reg, trans);

  return xaccSplitGetBalance (last_split);
}

/* ======================================================== */

static gboolean
use_security_cells (SplitRegister *reg, VirtualLocation virt_loc)
{
  GNCAccountType account_type;
  CursorClass cursor_class;
  Account *account;
  Split *split;

  split = sr_get_split (reg, virt_loc.vcell_loc);
  if (!split)
    return TRUE;

  cursor_class = xaccSplitRegisterGetCursorClass (reg,
                                                  virt_loc.vcell_loc);
  if (cursor_class != CURSOR_CLASS_SPLIT)
    return TRUE;

  account = NULL;

  if (virt_cell_loc_equal (virt_loc.vcell_loc,
                           reg->table->current_cursor_loc.vcell_loc) &&
      gnc_register_get_cell_changed (reg, XFRM_CELL, FALSE))
  {
    const char *name;

    name = gnc_register_get_cell_value (reg, XFRM_CELL);
    account = xaccGetAccountFromFullName (gncGetCurrentGroup (),
                                          name,
                                          gnc_get_account_separator ());
  }

  if (!account)
    account = xaccSplitGetAccount (split);

  if (!account)
    return TRUE;

  account_type = xaccAccountGetType (account);

  if (account_type == STOCK  ||
      account_type == MUTUAL ||
      account_type == CURRENCY)
    return TRUE;

  return FALSE;
}

const char *
xaccSRTemplateGetEntryHandler( VirtualLocation virt_loc,
                               gboolean translate,
                               gboolean *conditionally_changed,
                               gpointer user_data )
{
  SplitRegister *reg = user_data;
  const char *value = "";
  CellType cell_type;
  Transaction *trans;
  Split *split;
  kvp_frame        *kvpf;
  GUID                *tmpguid;

  cell_type = xaccSplitRegisterGetCellType (reg, virt_loc);

  split = sr_get_split (reg, virt_loc.vcell_loc);
  if (split == NULL)
  {
    return xaccSRGetEntryHandler( virt_loc, translate,
                                  conditionally_changed, user_data );
  }


  trans = xaccSplitGetParent (split);
  kvpf = xaccSplitGetSlots( split );

  switch (cell_type) {
    case XFRM_CELL:
      {
        static char *name = NULL;
                        
        if ( kvpf != NULL ) {
          DEBUG( "kvp_frame: %s\n", kvp_frame_to_string( kvpf ) );
          tmpguid = kvp_value_get_guid
            ( kvp_frame_get_slot( kvpf, "sched-xaction/xfrm" ) );
          DEBUG( "Got the guid \"%s\"\n", guid_to_string( tmpguid ) );
          name = xaccAccountGetFullName (xaccAccountLookup( tmpguid ),
                                         gnc_get_account_separator ());

          DEBUG( "Got the full name: %s\n", name );
        } else {
          name = "";
        }

        return name;
      }
      break;
    case CRED_CELL:
    case DEBT_CELL:
      {
        char        *amtStr;
        gnc_numeric amount;

        if ( kvpf != NULL ) {
          amtStr = kvp_value_get_string
            ( kvp_frame_get_slot( kvpf, "sched-xaction/amnt" ) );
          amount = gnc_numeric_create( 0, 1 );
          string_to_gnc_numeric( amtStr, &amount );
                        
          if (gnc_numeric_zero_p (amount))
            return "";

          if (gnc_numeric_negative_p (amount) && (cell_type == DEBT_CELL))
            return "";

          if (gnc_numeric_positive_p (amount) && (cell_type == CRED_CELL))
            return "";

          amount = gnc_numeric_abs (amount);

          /* return xaccPrintAmount (amount, 
             gnc_split_value_print_info (split, FALSE)); */

          /* jsled_FIXME: This should be fixed
             to be correct for the "fake" account. */
          return xaccPrintAmount( amount,
                                  gnc_default_print_info( FALSE ) );
        } else {
          return "";
        }
      }
      break;
    case FCRED_CELL:
      {
        char *formulaStr;
        if ( kvpf != NULL ) {
          return kvp_value_get_string
            ( kvp_frame_get_slot( kvpf, "sched-xaction/credit_formula" ) );
        }
      }
      break;
    case FDEBT_CELL:
      {
        char *formulaStr;
        if ( kvpf != NULL ) {
          return kvp_value_get_string
            ( kvp_frame_get_slot( kvpf, "sched-xaction/debit_formula" ) );
        }
      }
      break;
    case MXFRM_CELL:
      {
        return "FIXME:MXFRM";
      }
      break;
    default:
      break;
  } /* end switch */

  return xaccSRGetEntryHandler( virt_loc,
                                translate,
                                conditionally_changed,
                                user_data );
}

const char *
xaccSRGetEntryHandler (VirtualLocation virt_loc, gboolean translate,
                       gboolean *conditionally_changed, gpointer user_data)
{
  SplitRegister *reg = user_data;
  const char *value = "";
  CellType cell_type;
  Transaction *trans;
  Split *split;

  if (conditionally_changed)
    *conditionally_changed = FALSE;

  cell_type = xaccSplitRegisterGetCellType (reg, virt_loc);

  split = sr_get_split (reg, virt_loc.vcell_loc);
  if (split == NULL)
  {
    gnc_numeric imbalance;
    gnc_commodity *currency;

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

        currency = xaccTransGetCurrency (trans);
        if (!currency)
          currency = gnc_default_currency ();

        imbalance = gnc_numeric_convert (imbalance,
                                         gnc_commodity_get_fraction (currency),
                                         GNC_RND_ROUND);

        return xaccPrintAmount (imbalance,
                                gnc_split_value_print_info (split, FALSE));
        break;

      default:
        return value;
        break;
    }
  }

  trans = xaccSplitGetParent (split);

  switch (cell_type)
  {
    case DATE_CELL:
      {
        Timespec ts;

        xaccTransGetDatePostedTS (trans, &ts);

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
      if (translate)
        return gnc_get_reconcile_str (xaccSplitGetReconcile (split));
      else
      {
        static char s[2];

        s[0] = xaccSplitGetReconcile (split);
        s[1] = '\0';

        return s;
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
            account = sr_get_default_account (reg);

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
      {
        static char *name = NULL;

        name = xaccAccountGetFullName (xaccSplitGetAccount (split),
                                       gnc_get_account_separator ());

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
      break;

    case PRIC_CELL:
      {
        gnc_numeric price;

        if (!use_security_cells (reg, virt_loc))
          return "";

        price = xaccSplitGetSharePrice (split);
        if (gnc_numeric_zero_p (price))
          return "";

        return xaccPrintAmount (price, gnc_default_price_print_info ());
      }
      break;

    case SHRS_CELL:
      {
        gnc_numeric shares;

        if (!use_security_cells (reg, virt_loc))
          return "";

        shares = xaccSplitGetAmount (split);

        if (gnc_numeric_zero_p (shares))
          return "";

        return xaccPrintAmount (shares,
                                gnc_split_amount_print_info (split, FALSE));
      }
      break;

    case MXFRM_CELL:
      {
         Split *s = xaccSplitGetOtherSplit (split);
         static char *name = NULL;

         if (s)
           name = xaccAccountGetFullName (xaccSplitGetAccount (s),
                                          gnc_get_account_separator ());
         else
         {
           /* for multi-split transactions and stock splits,
            * use a special value. */
           s = xaccTransGetSplit (xaccSplitGetParent(split), 1);
           if (s)
             name = g_strdup (SPLIT_TRANS_STR);
           else if (safe_strcmp ("stock-split", xaccSplitGetType (split)) == 0)
             name = g_strdup (STOCK_SPLIT_STR);
           else
             name = g_strdup ("");
         }

         return name;
      }
      break;

    case TCRED_CELL:
    case TDEBT_CELL:
      {
        gnc_numeric total;

        total = get_trans_total_amount (reg, trans);
        if (gnc_numeric_zero_p (total))
          return "";

        if (gnc_numeric_negative_p (total) && (cell_type == TDEBT_CELL))
          return "";

        if (gnc_numeric_positive_p (total) && (cell_type == TCRED_CELL))
          return "";

        total = gnc_numeric_abs (total);

        return xaccPrintAmount (total,
                                gnc_split_amount_print_info (split, FALSE));
      }
      break;

    case TSHRS_CELL:
      {
        gnc_numeric total;

        total = get_trans_total_amount (reg, trans);

        return xaccPrintAmount (total,
                                gnc_split_amount_print_info (split, FALSE));
      }
      break;

    default:
      return "";
      break;
  }
}

static GNCAccountType
sr_type_to_account_type(SplitRegisterType sr_type)
{
  switch (sr_type)
  {
    case BANK_REGISTER:
      return BANK;
    case CASH_REGISTER:
      return CASH;
    case ASSET_REGISTER:
      return ASSET;
    case CREDIT_REGISTER:
      return CREDIT;
    case LIABILITY_REGISTER:
      return LIABILITY;
    case INCOME_LEDGER:  
    case INCOME_REGISTER:
      return INCOME;
    case EXPENSE_REGISTER:
      return EXPENSE;
    case STOCK_REGISTER:
    case PORTFOLIO_LEDGER:
      return STOCK;
    case CURRENCY_REGISTER:
      return CURRENCY;
    case GENERAL_LEDGER:  
      return NO_TYPE;
    case EQUITY_REGISTER:
      return EQUITY;
    case SEARCH_LEDGER:
      return NO_TYPE;
    default:
      return NO_TYPE;
  }
}

const char *
xaccSRGetDebitString (SplitRegister *reg)
{
  if (!reg)
    return NULL;

  if (reg->debit_str)
    return reg->debit_str;

  reg->debit_str = gnc_get_debit_string (sr_type_to_account_type (reg->type));

  if (reg->debit_str)
    return reg->debit_str;

  reg->debit_str = g_strdup (_("Debit"));

  return reg->debit_str;
}

const char *
xaccSRGetCreditString (SplitRegister *reg)
{
  if (!reg)
    return NULL;

  if (reg->credit_str)
    return reg->credit_str;

  reg->credit_str =
    gnc_get_credit_string (sr_type_to_account_type (reg->type));

  if (reg->credit_str)
    return reg->credit_str;

  reg->credit_str = g_strdup (_("Credit"));

  return reg->credit_str;
}

const char *
xaccSRGetLabelHandler (VirtualLocation virt_loc, gpointer user_data)
{
  SplitRegister *reg = user_data;
  CellType cell_type;

  cell_type = xaccSplitRegisterGetCellType (reg, virt_loc);

  switch (cell_type)
  {
    case DATE_CELL:
      return _("Date");

    case NUM_CELL:
      return _("Num");

    case DESC_CELL:
      return _("Description");

    case RECN_CELL:
      return _("Reconciled:R") + 11;

    case BALN_CELL:
      return _("Balance");

    case ACTN_CELL:
      return _("Action");

    case XFRM_CELL:
      return _("Account");

    case MEMO_CELL:
      return _("Memo");

    case CRED_CELL:
      return xaccSRGetCreditString (reg);

    case DEBT_CELL:
      return xaccSRGetDebitString (reg);

    case PRIC_CELL:
      if (!use_security_cells (reg, virt_loc))
        return "";

      return _("Price");

    case SHRS_CELL:
      if (!use_security_cells (reg, virt_loc))
        return "";

      return _("Shares");

    case MXFRM_CELL:
      return _("Transfer");

    case TCRED_CELL:
      if (reg->tcredit_str)
        return reg->tcredit_str;

      {
        const char *string = xaccSRGetCreditString (reg);
        if (string)
          reg->tcredit_str = g_strdup_printf (_("Tot %s"), string);
      }

      if (reg->tcredit_str)
        return reg->tcredit_str;

      reg->tcredit_str = g_strdup (_("Tot Credit"));

      return reg->tcredit_str;

    case TDEBT_CELL:
      if (reg->tdebit_str)
        return reg->tdebit_str;

      {
        const char *string = xaccSRGetDebitString (reg);
        if (string)
          reg->tdebit_str = g_strdup_printf (_("Tot %s"), string);
      }

      if (reg->tdebit_str)
        return reg->tdebit_str;

      reg->tdebit_str = g_strdup (_("Tot Debit"));

      return reg->tdebit_str;

    case TSHRS_CELL:
      return _("Tot Shares");

    case TBALN_CELL:
      return _("Balance");

    case NOTES_CELL:
      return _("Notes");

    case FCRED_CELL:
      return _("Credit Formula");
    case FDEBT_CELL:
      return _("Debit Formula");

    case NO_CELL:
      return "";

    default:
      break;
  }

  PERR ("bad cell type: %d", cell_type);

  return "";
}

CellIOFlags
xaccSRTemplateGetIOFlagsHandler( VirtualLocation virt_loc,
				 gpointer user_data )
{
  /* printf( "In xaccSRTemplateGetIOFlagsHandler\n" ); */
  return xaccSRGetIOFlagsHandler( virt_loc,
                                  user_data );
}

CellIOFlags
xaccSRGetIOFlagsHandler (VirtualLocation virt_loc, gpointer user_data)
{
  SplitRegister *reg = user_data;
  CellType cell_type;
  Split *split;

  cell_type = xaccSplitRegisterGetCellType (reg, virt_loc);

  switch (cell_type)
  {
    case DATE_CELL:
    case NUM_CELL:
    case DESC_CELL:
    case ACTN_CELL:
    case XFRM_CELL:
    case MEMO_CELL:
    case MXFRM_CELL:
    case NOTES_CELL:
    case FCRED_CELL:
    case FDEBT_CELL:
      return XACC_CELL_ALLOW_ALL;

    case CRED_CELL:
    case DEBT_CELL:
      split = sr_get_split (reg, virt_loc.vcell_loc);
      if (safe_strcmp ("stock-split", xaccSplitGetType (split)) == 0)
        return XACC_CELL_ALLOW_NONE;

      return XACC_CELL_ALLOW_ALL;

    case RECN_CELL:
      return XACC_CELL_ALLOW_ALL | XACC_CELL_ALLOW_EXACT_ONLY;

    case PRIC_CELL:
    case SHRS_CELL:
      if (use_security_cells (reg, virt_loc))
        return XACC_CELL_ALLOW_ALL;

      return XACC_CELL_ALLOW_SHADOW;

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
          shares = get_trans_total_amount (reg, trans);
        else if (is_current)
          shares = xaccGetPriceCellValue
            ((PriceCell *) gnc_register_get_cell (reg, SHRS_CELL));
        else
          shares = xaccSplitGetAmount (split);

        if (gnc_numeric_negative_p (shares))
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

void
xaccSRGetCellBorderHandler (VirtualLocation virt_loc,
                            PhysicalCellBorders *borders,
                            gpointer user_data)
{
  SplitRegister *reg = user_data;
  CursorClass cursor_class;
  VirtualCell *vcell;

  vcell = gnc_table_get_virtual_cell (reg->table, virt_loc.vcell_loc);
  if (!vcell || !vcell->cellblock)
    return;

  if (virt_loc.phys_col_offset < vcell->cellblock->start_col ||
      virt_loc.phys_col_offset > vcell->cellblock->stop_col)
  {
    borders->top    = CELL_BORDER_LINE_NONE;
    borders->bottom = CELL_BORDER_LINE_NONE;
    borders->left   = CELL_BORDER_LINE_NONE;
    borders->right  = CELL_BORDER_LINE_NONE;
    return;
  }

  cursor_class = xaccCursorNameToClass (vcell->cellblock->cursor_name);

  if (cursor_class == CURSOR_CLASS_TRANS &&
      virt_loc.phys_col_offset == vcell->cellblock->start_col)
    borders->left   = CELL_BORDER_LINE_NONE;

  if (cursor_class == CURSOR_CLASS_TRANS &&
      virt_loc.phys_col_offset == vcell->cellblock->stop_col)
    borders->right  = CELL_BORDER_LINE_NONE;

  if (cursor_class == CURSOR_CLASS_SPLIT)
  {
    borders->top    = CELL_BORDER_LINE_LIGHT;
    borders->bottom = CELL_BORDER_LINE_LIGHT;
    borders->left   = MIN (borders->left,   CELL_BORDER_LINE_LIGHT);
    borders->right  = MIN (borders->right,  CELL_BORDER_LINE_LIGHT);

    if (virt_loc.phys_col_offset == vcell->cellblock->start_col)
      borders->left = CELL_BORDER_LINE_LIGHT;
    if (virt_loc.phys_col_offset == vcell->cellblock->stop_col)
      borders->right = CELL_BORDER_LINE_LIGHT;
  }
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
xaccSRGetBGColorHandler (VirtualLocation virt_loc,
                         gboolean *hatching,
                         gpointer user_data)
{
  SplitRegister *reg = user_data;
  const char *cursor_name;
  VirtualCell *vcell;
  guint32 bg_color;
  gboolean is_current;

  if (hatching)
  {
    CellType cell_type;

    cell_type = xaccSplitRegisterGetCellType (reg, virt_loc);

    if ((cell_type != DEBT_CELL)  &&
        (cell_type != CRED_CELL)  &&
        (cell_type != TDEBT_CELL) &&
        (cell_type != TCRED_CELL) &&
	(cell_type != FCRED_CELL) &&
	(cell_type != FDEBT_CELL) )
      *hatching = FALSE;
    else
    {
      Transaction *trans;

      trans = xaccSRGetTrans (reg, virt_loc.vcell_loc);

      if (trans)
        *hatching = !gnc_numeric_zero_p (xaccTransGetImbalance (trans));
      else
        *hatching = FALSE;
    }
  }

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

  cursor_name = vcell->cellblock->cursor_name;

  if (safe_strcmp (cursor_name, CURSOR_HEADER) == 0)
    return reg_colors.header_bg_color;

  if (safe_strcmp (cursor_name, CURSOR_SINGLE_JOURNAL) == 0 ||
      safe_strcmp (cursor_name, CURSOR_SINGLE_LEDGER) == 0)
  {
    if (is_current)
      return vcell->start_primary_color ?
        reg_colors.primary_active_bg_color :
        reg_colors.secondary_active_bg_color;

    return vcell->start_primary_color ?
      reg_colors.primary_bg_color : reg_colors.secondary_bg_color;
  }

  if (safe_strcmp (cursor_name, CURSOR_DOUBLE_JOURNAL) == 0 ||
      safe_strcmp (cursor_name, CURSOR_DOUBLE_LEDGER) == 0)
  {
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
  }

  if (safe_strcmp (cursor_name, CURSOR_SPLIT) == 0)
  {
    if (is_current)
      return reg_colors.split_active_bg_color;

    return reg_colors.split_bg_color;
  }

  PWARN("Unexpected cursor: %s\n", cursor_name);
  return bg_color;
}

gboolean
xaccSRTemplateConfirmHandler( VirtualLocation virt_loc,
			      gpointer user_data )
{
  return xaccSRConfirmHandler( virt_loc, user_data );
}

gboolean
xaccSRConfirmHandler (VirtualLocation virt_loc,
                      gpointer user_data)
{
  SplitRegister *reg = user_data;
  SRInfo *info = xaccSRGetInfo (reg);
  Split *split;
  char recn;

  /* This assumes we reset the flag whenever we change splits.
   * This happens in LedgerMoveCursor. */
  if (info->change_confirmed)
    return TRUE;

  split = sr_get_split (reg, virt_loc.vcell_loc);
  if (!split)
    return TRUE;

  if (gnc_register_get_cell_changed (reg, RECN_CELL, FALSE))
    recn = xaccRecnCellGetFlag ((RecnCell *)
                                gnc_register_get_cell (reg, RECN_CELL));
  else
    recn = xaccSplitGetReconcile (split);

  if (recn == YREC)
  {
    gboolean confirm;
    char *message = _("You are about to change a reconciled split.\n"
                      "Are you sure you want to do that?");

    confirm = gnc_lookup_boolean_option ("Register",
                                         "Confirm before changing reconciled",
                                         TRUE);
    if (!confirm)
      return TRUE;

    confirm = gnc_verify_dialog_parented (xaccSRGetParent (reg),
                                          message, FALSE);

    info->change_confirmed = confirm;

    return confirm;
  }

  return TRUE;
}

static gboolean
recn_cell_confirm (char old_flag, gpointer data)
{
  SplitRegister *reg = data;

  if (old_flag == YREC)
  {
    const char *message = _("Do you really want to mark this transaction "
                            "not reconciled?\nDoing so might make future "
                            "reconciliation difficult!");
    gboolean confirm;

    confirm = gnc_lookup_boolean_option ("Register",
                                         "Confirm before changing reconciled",
                                         TRUE);
    if (!confirm)
      return TRUE;

    return gnc_verify_dialog_parented (xaccSRGetParent (reg), message, TRUE);
  }

  return TRUE;
}

/* ======================================================== */

G_INLINE_FUNC void
sr_add_transaction (SplitRegister *reg,
                    Transaction *trans,
                    Split *split,
                    CellBlock *lead_cursor,
                    CellBlock *split_cursor,
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
                    CellBlock *split_cursor,
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

      gnc_table_set_vcell (reg->table, split_cursor,
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

      gnc_table_set_vcell (reg->table, split_cursor,
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

      gnc_table_set_vcell (reg->table, split_cursor,
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
  gnc_table_set_vcell (reg->table, split_cursor,
                       xaccSplitGetGUID (NULL), FALSE, TRUE, *vcell_loc);
  vcell_loc->virt_row++;

}

/* ======================================================== */

void
xaccSRLoadRegister (SplitRegister *reg, GList * slist,
                    Account *default_account)
{
  SRInfo *info = xaccSRGetInfo (reg);
  Split *blank_split = xaccSplitLookup (&info->blank_split_guid);
  Transaction *pending_trans = xaccTransLookup (&info->pending_trans_guid);
  CursorBuffer *cursor_buffer;
  GHashTable *trans_table = NULL;
  CellBlock *cursor_header;
  CellBlock *lead_cursor;
  CellBlock *split_cursor;
  Transaction *blank_trans;
  Transaction *find_trans;
  Transaction *trans;
  CursorClass find_class;
  Split *find_trans_split;
  Split *find_split;
  Split *split;
  Table *table;
  GList *node;

  gboolean start_primary_color = TRUE;
  gboolean found_pending = FALSE;
  gboolean found_divider = FALSE;
  gboolean has_last_num = FALSE;
  gboolean multi_line;
  gboolean dynamic;

  VirtualCellLocation vcell_loc;
  VirtualLocation save_loc;

  int new_trans_split_row = -1;
  int new_trans_row = -1;
  int new_split_row = -1;
  time_t present;

  /* make sure we have a blank split */
  if (blank_split == NULL)
  {
    Transaction *trans;

    gnc_suspend_gui_refresh ();

    trans = xaccMallocTransaction ();

    xaccTransBeginEdit (trans);
    xaccTransSetCurrency (trans, gnc_default_currency ()); /* is this lame? */
    xaccTransSetDateSecs (trans, info->last_date_entered);
    blank_split = xaccMallocSplit ();
    xaccTransAppendSplit (trans, blank_split);
    xaccTransCommitEdit (trans);

    info->blank_split_guid = *xaccSplitGetGUID (blank_split);
    info->blank_split_edited = FALSE;

    gnc_resume_gui_refresh ();
  }

  blank_trans = xaccSplitGetParent (blank_split);

  info->default_account = *xaccAccountGetGUID (default_account);

  table = reg->table;

  gnc_table_leave_update (table, table->current_cursor_loc);

  multi_line = (reg->style == REG_STYLE_JOURNAL);
  dynamic    = (reg->style == REG_STYLE_AUTO_LEDGER);

  lead_cursor = sr_get_passive_cursor (reg);
  split_cursor = gnc_table_layout_get_cursor (table->layout, CURSOR_SPLIT);

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
  if (gnc_table_current_cursor_changed (table, TRUE) &&
      (find_split == xaccSRGetCurrentSplit (reg)))
  {
    cursor_buffer = gnc_cursor_buffer_new ();
    gnc_table_save_current_cursor (table, cursor_buffer);
  }
  else
    cursor_buffer = NULL;

  /* disable move callback -- we don't want the cascade of 
   * callbacks while we are fiddling with loading the register */
  table->control->allow_move = FALSE;

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
  cursor_header = gnc_table_layout_get_cursor (table->layout, CURSOR_HEADER);
  gnc_table_set_vcell (table, cursor_header, NULL, TRUE, TRUE, vcell_loc);
  vcell_loc.virt_row++;

  /* get the current time and reset the dividing row */
  {
    struct tm *tm;

    present = time (NULL);

    tm = localtime (&present);
    tm->tm_sec = 59;
    tm->tm_min = 59;
    tm->tm_hour = 23;
    tm->tm_isdst = -1;

    present = mktime (tm);
  }

  if (info->first_pass)
  {
    if (default_account)
    {
      const char *last_num = xaccAccountGetLastNum (default_account);

      if (last_num)
      {
        NumCell *cell;

        cell = (NumCell *) gnc_register_get_cell (reg, NUM_CELL);
        xaccSetNumCellLastNum (cell, last_num);
        has_last_num = TRUE;
      }
    }
  }

  table->dividing_row = -1;

  if (multi_line)
    trans_table = g_hash_table_new (g_direct_hash, g_direct_equal);

  /* populate the table */
  for (node = slist; node; node = node->next) 
  {
    split = node->data;
    trans = xaccSplitGetParent (split);

    if (pending_trans == trans)
      found_pending = TRUE;

    /* do not load the blank split */
    if (split == blank_split)
      continue;

    if (multi_line)
    {
      if (trans == blank_trans)
        continue;

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

      xaccQuickFillAddCompletion ((QuickFillCell *)
                                  gnc_register_get_cell (reg, DESC_CELL),
                                  xaccTransGetDescription (trans));

      xaccQuickFillAddCompletion ((QuickFillCell *)
                                  gnc_register_get_cell (reg, NOTES_CELL),
                                  xaccTransGetNotes (trans));

      if (!has_last_num)
        xaccSetNumCellLastNum ((NumCell *)
                               gnc_register_get_cell (reg, NUM_CELL),
                               xaccTransGetNum (trans));

      for (node = xaccTransGetSplitList (trans); node; node = node->next)
      {
        Split *s = node->data;
        QuickFillCell *cell;

        cell = (QuickFillCell *) gnc_register_get_cell (reg, MEMO_CELL);
        xaccQuickFillAddCompletion (cell, xaccSplitGetMemo (s));
      }
    }

    if (trans == find_trans)
      new_trans_row = vcell_loc.virt_row;

    if (split == find_trans_split)
      new_trans_split_row = vcell_loc.virt_row;

    sr_add_transaction (reg, trans, split, lead_cursor, split_cursor,
                        multi_line, start_primary_color, TRUE, TRUE,
                        find_trans, find_split, find_class,
                        &new_split_row, &vcell_loc);

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

  /* go to blank on first pass */
  if (info->first_pass)
  {
    new_split_row = -1;
    new_trans_split_row = -1;
    new_trans_row = -1;

    save_loc.vcell_loc = vcell_loc;
    save_loc.phys_row_offset = 0;
    save_loc.phys_col_offset = 0;
  }

  sr_add_transaction (reg, trans, split, lead_cursor, split_cursor,
                      multi_line, start_primary_color, FALSE,
                      info->blank_split_edited, find_trans,
                      find_split, find_class, &new_split_row,
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

      if (find_split == xaccSRGetCurrentSplit (reg))
        gnc_table_restore_current_cursor (table, cursor_buffer);
    }

    gnc_cursor_buffer_destroy (cursor_buffer);
    cursor_buffer = NULL;
  }

  /* If we didn't find the pending transaction, it was removed
   * from the account. */
  if (!found_pending)
  {
    if (xaccTransIsOpen (pending_trans))
      xaccTransCommitEdit (pending_trans);

    info->pending_trans_guid = *xaccGUIDNULL ();
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
  info->reg_loaded = TRUE;

  sr_set_cell_fractions (reg, xaccSRGetCurrentSplit (reg));

  gnc_table_refresh_gui (table, TRUE);

  xaccSRShowTrans (reg, table->current_cursor_loc.vcell_loc);

  /* set the completion character for the xfer cells */
  xaccComboCellSetCompleteChar ((ComboCell *)
                                gnc_register_get_cell (reg, MXFRM_CELL),
                                gnc_get_account_separator ());
  xaccComboCellSetCompleteChar ((ComboCell *)
                                gnc_register_get_cell (reg, XFRM_CELL),
                                gnc_get_account_separator ());

  /* set the confirmation callback for the reconcile cell */
  xaccRecnCellSetConfirmCB ((RecnCell *)
                            gnc_register_get_cell (reg, RECN_CELL),
                            recn_cell_confirm, reg);

  /* enable callback for cursor user-driven moves */
  table->control->allow_move = TRUE;

  table->set_help = LedgerSetHelp;

  reg->destroy = LedgerDestroy;

  xaccSRLoadXferCells (reg, default_account);
}

/* ======================================================== */

static void 
LoadXferCell (ComboCell * cell, AccountGroup * grp)
{
  GList *list;
  GList *node;

  ENTER ("\n");

  if (!grp) return;

  /* Build the xfer menu out of account names. */

  list = xaccGroupGetSubAccounts (grp);

  for (node = list; node; node = node->next)
  {
    Account *account = node->data;
    char *name;

    name = xaccAccountGetFullName (account, gnc_get_account_separator ());
    if (name != NULL)
    {
      xaccAddComboCellMenuItem (cell, name);
      g_free(name);
    }
  }

  g_list_free (list);

  LEAVE ("\n");
}

/* ======================================================== */

void
xaccSRLoadXferCells (SplitRegister *reg, Account *base_account)
{
  AccountGroup *group;
  ComboCell *cell;

  group = xaccGetAccountRoot(base_account);
  if (group == NULL)
    group = gncGetCurrentGroup();

  if (group == NULL)
    return;

  cell = (ComboCell *) gnc_register_get_cell (reg, XFRM_CELL);
  xaccClearComboCellMenu (cell);
  LoadXferCell (cell, group);

  cell = (ComboCell *) gnc_register_get_cell (reg, MXFRM_CELL);
  xaccClearComboCellMenu (cell);
  LoadXferCell (cell, group);
}

/* ======================================================== */

gboolean
xaccSRHasPendingChanges (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo (reg);
  Transaction *pending_trans = xaccTransLookup (&info->pending_trans_guid);

  if (reg == NULL)
    return FALSE;

  if (gnc_table_current_cursor_changed (reg->table, FALSE))
    return TRUE;

  return xaccTransIsOpen (pending_trans);
}

/* ======================================================== */

void
xaccSRShowPresentDivider (SplitRegister *reg, gboolean show_present)
{
  SRInfo *info = xaccSRGetInfo (reg);

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

/* ======================================================== */

gboolean
xaccSRFullRefreshOK (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo (reg);

  if (!info)
    return FALSE;

  return info->full_refresh;
}

/* =======================  end of file =================== */
