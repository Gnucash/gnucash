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
 * split-register.c
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
 * Copyright (c) 2000-2001 Dave Peticolas <dave@krondo.com>
 */

#define _GNU_SOURCE

#include "config.h"

#include <glib.h>
#include <guile/gh.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "Account.h"
#include "FileDialog.h"
#include "combocell.h"
#include "datecell.h"
#include "global-options.h"
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"
#include "split-register-p.h"
#include "gnc-ledger-display.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "guile-util.h"
#include "messages.h"
#include "numcell.h"
#include "pricecell.h"
#include "quickfillcell.h"
#include "recncell.h"
#include "split-register.h"
#include "split-register-control.h"
#include "split-register-layout.h"
#include "split-register-model.h"
#include "split-register-model-save.h"
#include "table-allgui.h"


/** static variables ******************************************************/

/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_LEDGER;

/* The copied split or transaction, if any */
static CursorClass copied_class = CURSOR_CLASS_NONE;
static SCM copied_item = SCM_UNDEFINED;
static GUID copied_leader_guid;


/** static prototypes *****************************************************/

static gboolean gnc_split_register_save_to_scm (SplitRegister *reg,
                                                SCM trans_scm, SCM split_scm,
                                                gboolean use_cut_semantics);
static gboolean gnc_split_register_auto_calc (SplitRegister *reg,
                                              Split *split);


/** implementations *******************************************************/

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
gnc_split_register_expand_current_trans (SplitRegister *reg, gboolean expand)
{
  SRInfo *info = gnc_split_register_get_info (reg);

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
    gnc_split_register_get_trans_split (reg, virt_loc.vcell_loc,
                                        &virt_loc.vcell_loc);

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
                                  gnc_split_register_get_active_cursor (reg));

  gnc_split_register_set_trans_visible
    (reg, reg->table->current_cursor_loc.vcell_loc, expand, FALSE);

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
    gnc_split_register_show_trans (reg,
                                   reg->table->current_cursor_loc.vcell_loc);
}

gboolean
gnc_split_register_current_trans_expanded (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info (reg);

  if (!reg)
    return FALSE;

  if (reg->style == REG_STYLE_AUTO_LEDGER ||
      reg->style == REG_STYLE_JOURNAL)
    return FALSE;

  return info->trans_expanded;
}

Transaction *
gnc_split_register_get_current_trans (SplitRegister *reg)
{
  Split *split;
  VirtualCellLocation vcell_loc;

  if (reg == NULL)
    return NULL;

  split = gnc_split_register_get_current_split (reg);
  if (split != NULL)
    return xaccSplitGetParent(split);

  /* Split is blank. Assume it is the blank split of a multi-line
   * transaction. Go back one row to find a split in the transaction. */
  vcell_loc = reg->table->current_cursor_loc.vcell_loc;

  vcell_loc.virt_row--;

  split = gnc_split_register_get_split (reg, vcell_loc);

  return xaccSplitGetParent (split);
}

Split *
gnc_split_register_get_current_split (SplitRegister *reg)
{
  if (reg == NULL)
    return NULL;

  return gnc_split_register_get_split
    (reg, reg->table->current_cursor_loc.vcell_loc);
}

Split *
gnc_split_register_get_blank_split (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info (reg);

  if (!reg) return NULL;

  return xaccSplitLookup (&info->blank_split_guid);
}

gboolean
gnc_split_register_get_split_virt_loc (SplitRegister *reg, Split *split,
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

gboolean
gnc_split_register_get_split_amount_virt_loc (SplitRegister *reg, Split *split,
                                              VirtualLocation *virt_loc)
{
  VirtualLocation v_loc;
  CursorClass cursor_class;
  const char *cell_name;
  gnc_numeric value;

  if (!gnc_split_register_get_split_virt_loc (reg, split, &v_loc.vcell_loc))
    return FALSE;

  cursor_class = gnc_split_register_get_cursor_class (reg, v_loc.vcell_loc);

  value = xaccSplitGetValue (split);

  switch (cursor_class)
  {
    case CURSOR_CLASS_SPLIT:
    case CURSOR_CLASS_TRANS:
      cell_name = (gnc_numeric_negative_p (value)) ? CRED_CELL : DEBT_CELL;
      break;
    default:
      return FALSE;
  }

  if (!gnc_table_get_cell_location (reg->table, cell_name,
                                    v_loc.vcell_loc, &v_loc))
    return FALSE;

  if (virt_loc == NULL)
    return TRUE;

  *virt_loc = v_loc;

  return TRUE;
}

Split *
gnc_split_register_duplicate_current (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info(reg);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  CursorClass cursor_class;
  Transaction *trans;
  Split *return_split;
  Split *trans_split;
  gboolean changed;
  Split *split;

  split = gnc_split_register_get_current_split (reg);
  trans = gnc_split_register_get_current_trans (reg);
  trans_split = gnc_split_register_get_current_trans_split (reg, NULL);

  /* This shouldn't happen, but be paranoid. */
  if (trans == NULL)
    return NULL;

  cursor_class = gnc_split_register_get_current_cursor_class (reg);

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

    result = gnc_ok_cancel_dialog_parented
      (gnc_split_register_get_parent (reg),
       message, GNC_VERIFY_OK);

    if (result == GNC_VERIFY_CANCEL)
    {
      gnc_resume_gui_refresh ();
      return NULL;
    }

    gnc_split_register_save (reg, TRUE);

    /* If the split is NULL, then we were on a blank split row
     * in an expanded transaction. The new split (created by
     * gnc_split_register_save above) will be the last split in the
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
    NumCell *num_cell;
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
      Account *account = gnc_split_register_get_default_account (reg);

      if (account)
        in_num = xaccAccountGetLastNum (account);
      else
        in_num = xaccTransGetNum (trans);
    }

    if (!gnc_dup_trans_dialog (gnc_split_register_get_parent (reg),
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

    num_cell = (NumCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                      NUM_CELL);
    if (gnc_num_cell_set_last_num (num_cell, out_num))
      gnc_split_register_set_last_num (reg, out_num);

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

static void
gnc_split_register_copy_current_internal (SplitRegister *reg,
                                          gboolean use_cut_semantics)
{
  SRInfo *info = gnc_split_register_get_info(reg);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  CursorClass cursor_class;
  Transaction *trans;
  gboolean changed;
  Split *split;
  SCM new_item;

  split = gnc_split_register_get_current_split (reg);
  trans = gnc_split_register_get_current_trans (reg);

  /* This shouldn't happen, but be paranoid. */
  if (trans == NULL)
    return;

  cursor_class = gnc_split_register_get_current_cursor_class (reg);

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
        gnc_split_register_save_to_scm (reg, SCM_UNDEFINED, new_item,
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

        gnc_split_register_save_to_scm (reg, new_item, split_scm,
                                        use_cut_semantics);
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

void
gnc_split_register_copy_current (SplitRegister *reg)
{
  gnc_split_register_copy_current_internal (reg, FALSE);
}

void
gnc_split_register_cut_current (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info (reg);
  Split *blank_split = xaccSplitLookup (&info->blank_split_guid);
  CursorClass cursor_class;
  Transaction *trans;
  gboolean changed;
  Split *split;

  split = gnc_split_register_get_current_split (reg);
  trans = gnc_split_register_get_current_trans (reg);

  /* This shouldn't happen, but be paranoid. */
  if (trans == NULL)
    return;

  cursor_class = gnc_split_register_get_current_cursor_class (reg);

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

  gnc_split_register_copy_current_internal (reg, TRUE);

  if (cursor_class == CURSOR_CLASS_SPLIT)
    gnc_split_register_delete_current_split (reg);
  else
    gnc_split_register_delete_current_trans (reg);
}

void
gnc_split_register_paste_current (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info(reg);
  Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
  CursorClass cursor_class;
  Transaction *trans;
  Split *trans_split;
  Split *split;

  if (copied_class == CURSOR_CLASS_NONE)
    return;

  split = gnc_split_register_get_current_split (reg);
  trans = gnc_split_register_get_current_trans (reg);

  trans_split = gnc_split_register_get_current_trans_split (reg, NULL);

  /* This shouldn't happen, but be paranoid. */
  if (trans == NULL)
    return;

  cursor_class = gnc_split_register_get_current_cursor_class (reg);

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
      result = gnc_verify_dialog_parented (gnc_split_register_get_parent (reg),
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
  else
  {
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
      result = gnc_verify_dialog_parented(gnc_split_register_get_parent(reg),
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

    if ((gnc_split_register_get_default_account (reg) != NULL) &&
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

void
gnc_split_register_delete_current_split (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info (reg);
  Split *blank_split = xaccSplitLookup (&info->blank_split_guid);
  Transaction *pending_trans = xaccTransLookup (&info->pending_trans_guid);
  Transaction *trans;
  Account *account;
  Split *split;

  /* get the current split based on cursor position */
  split = gnc_split_register_get_current_split (reg);
  if (split == NULL)
    return;

  /* If we are deleting the blank split, just cancel. The user is
   * allowed to delete the blank split as a method for discarding
   * any edits they may have made to it. */
  if (split == blank_split)
  {
    gnc_split_register_cancel_cursor_split_changes (reg);
    return;
  }

  gnc_suspend_gui_refresh ();

  /* make a copy of all of the accounts that will be  
   * affected by this deletion, so that we can update
   * their register windows after the deletion. */
  trans = xaccSplitGetParent(split);

  account = xaccSplitGetAccount (split);

  xaccTransBeginEdit (trans);
  xaccAccountBeginEdit (account);
  xaccSplitDestroy (split);
  xaccAccountCommitEdit (account);
  xaccTransCommitEdit (trans);

  /* Check pending transaction */
  if (trans == pending_trans)
  {
    info->pending_trans_guid = *xaccGUIDNULL ();
    pending_trans = NULL;
  }

  gnc_resume_gui_refresh ();
}

void
gnc_split_register_delete_current_trans (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info (reg);
  Split *blank_split = xaccSplitLookup (&info->blank_split_guid);
  Transaction *pending_trans = xaccTransLookup (&info->pending_trans_guid);
  Transaction *trans;
  Account *account;
  Split *split;

  /* get the current split based on cursor position */
  split = gnc_split_register_get_current_split (reg);
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

void
gnc_split_register_emtpy_current_trans (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info (reg);
  Split *blank_split = xaccSplitLookup (&info->blank_split_guid);
  Transaction *pending_trans = xaccTransLookup (&info->pending_trans_guid);
  Transaction *trans;
  Account *account;
  GList *splits;
  GList *node;
  Split *split;

  /* get the current split based on cursor position */
  split = gnc_split_register_get_current_split (reg);
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

void
gnc_split_register_cancel_cursor_split_changes (SplitRegister *reg)
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

void
gnc_split_register_cancel_cursor_trans_changes (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info(reg);
  Transaction *pending_trans = xaccTransLookup(&info->pending_trans_guid);

  /* Get the currently open transaction, rollback the edits on it, and
   * then repaint everything. To repaint everything, make a note of
   * all of the accounts that will be affected by this rollback. */
  if (!xaccTransIsOpen(pending_trans))
  {
    gnc_split_register_cancel_cursor_split_changes (reg);
    return;
  }

  if (!pending_trans)
    return;

  gnc_suspend_gui_refresh ();

  xaccTransRollbackEdit (pending_trans);

  info->pending_trans_guid = *xaccGUIDNULL ();

  gnc_resume_gui_refresh ();
}

void
gnc_split_register_redraw (SplitRegister *reg) 
{
  gnc_ledger_display_refresh_by_split_register (reg);
}

/* Copy from the register object to scheme. This needs to be
 * in sync with gnc_split_register_save and xaccSRSaveChangedCells. */
static gboolean
gnc_split_register_save_to_scm (SplitRegister *reg,
                                SCM trans_scm, SCM split_scm,
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
  trans = gnc_split_register_get_current_trans (reg);
  if (trans == NULL)
    return FALSE;

  /* copy the contents from the cursor to the split */
  if (gnc_table_layout_get_cell_changed (reg->table->layout, DATE_CELL, TRUE))
  {
    BasicCell *cell;
    Timespec ts;

    cell = gnc_table_layout_get_cell (reg->table->layout, DATE_CELL);
    gnc_date_cell_get_date ((DateCell *) cell, &ts);

    gnc_trans_scm_set_date(trans_scm, &ts);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, NUM_CELL, TRUE))
  {
    const char *value;

    value = gnc_table_layout_get_cell_value (reg->table->layout, NUM_CELL);
    gnc_trans_scm_set_num (trans_scm, value);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, DESC_CELL, TRUE))
  {
    const char *value;

    value = gnc_table_layout_get_cell_value (reg->table->layout, DESC_CELL);
    gnc_trans_scm_set_description (trans_scm, value);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, NOTES_CELL, TRUE))
  {
    const char *value;

    value = gnc_table_layout_get_cell_value (reg->table->layout, NOTES_CELL);
    gnc_trans_scm_set_notes (trans_scm, value);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, RECN_CELL, TRUE))
  {
    BasicCell *cell;
    char flag;

    cell = gnc_table_layout_get_cell (reg->table->layout, RECN_CELL);
    flag = gnc_recn_cell_get_flag ((RecnCell *) cell);

    gnc_split_scm_set_reconcile_state(split_scm, flag);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, ACTN_CELL, TRUE))
  {
    const char *value;

    value = gnc_table_layout_get_cell_value (reg->table->layout, ACTN_CELL);
    gnc_split_scm_set_action (split_scm, value);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, MEMO_CELL, TRUE))
  {
    const char *value;

    value = gnc_table_layout_get_cell_value (reg->table->layout, MEMO_CELL);
    gnc_split_scm_set_memo (split_scm, value);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, XFRM_CELL, TRUE))
  {
    Account *new_account;

    new_account = gnc_split_register_get_account (reg, XFRM_CELL);

    if (new_account != NULL)
      gnc_split_scm_set_account (split_scm, new_account);
  }

  if (reg->style == REG_STYLE_LEDGER)
    other_split_scm = gnc_trans_scm_get_other_split_scm (trans_scm, split_scm);

  if (gnc_table_layout_get_cell_changed (reg->table->layout, MXFRM_CELL, TRUE))
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

      new_account = gnc_split_register_get_account (reg, MXFRM_CELL);

      if (new_account != NULL)
        gnc_split_scm_set_account (other_split_scm, new_account);
    }
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout,
                                         DEBT_CELL, TRUE) ||
      gnc_table_layout_get_cell_changed (reg->table->layout,
                                         CRED_CELL, TRUE))
  {
    BasicCell *cell;
    gnc_numeric new_value;
    gnc_numeric credit;
    gnc_numeric debit;

    cell = gnc_table_layout_get_cell (reg->table->layout, CRED_CELL);
    credit = gnc_price_cell_get_value ((PriceCell *) cell);

    cell = gnc_table_layout_get_cell (reg->table->layout, DEBT_CELL);
    debit = gnc_price_cell_get_value ((PriceCell *) cell);

    new_value = gnc_numeric_sub_fixed (debit, credit);

    gnc_split_scm_set_value (split_scm, new_value);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, PRIC_CELL, TRUE))
  {
    /* do nothing for now */
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, SHRS_CELL, TRUE))
  {
    BasicCell *cell;
    gnc_numeric shares;

    cell = gnc_table_layout_get_cell (reg->table->layout, SHRS_CELL);

    shares = gnc_price_cell_get_value ((PriceCell *) cell);

    gnc_split_scm_set_amount (split_scm, shares);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout,
                                         DEBT_CELL, TRUE) ||
      gnc_table_layout_get_cell_changed (reg->table->layout,
                                         CRED_CELL, TRUE) ||
      gnc_table_layout_get_cell_changed (reg->table->layout,
                                         PRIC_CELL, TRUE) ||
      gnc_table_layout_get_cell_changed (reg->table->layout,
                                         SHRS_CELL, TRUE))
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

gboolean
gnc_split_register_save (SplitRegister *reg, gboolean do_commit)
{
   SRInfo *info = gnc_split_register_get_info (reg);
   Split *blank_split = xaccSplitLookup (&info->blank_split_guid);
   Transaction *pending_trans = xaccTransLookup (&info->pending_trans_guid);
   Transaction *blank_trans = xaccSplitGetParent (blank_split);
   Transaction *trans;
   const char *memo;
   const char *desc;
   Split *split;

   /* get the handle to the current split and transaction */
   split = gnc_split_register_get_current_split (reg);
   trans = gnc_split_register_get_current_trans (reg);
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
         info->last_date_entered = xaccTransGetDate (trans);
         info->blank_split_guid = *xaccGUIDNULL ();
         info->blank_split_edited = FALSE;
         blank_split = NULL;
       }
       else
         return FALSE;
     }
     else if (!xaccTransIsOpen (trans))
       return FALSE;

     if (xaccTransIsOpen (trans))
       xaccTransCommitEdit (trans);

     if (pending_trans == trans)
     {
       pending_trans = NULL;
       info->pending_trans_guid = *xaccGUIDNULL ();
     }

     return TRUE;
   }

   ENTER ("save split is %p \n", split);

   if (!gnc_split_register_auto_calc (reg, split))
     return FALSE;

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
     xaccAccountInsertSplit (gnc_split_register_get_default_account (reg),
                             blank_split);
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

     trans_split = gnc_split_register_get_current_trans_split (reg, NULL);
     if ((info->cursor_hint_trans == trans) &&
         (info->cursor_hint_trans_split == trans_split) &&
         (info->cursor_hint_split == NULL))
     {
       info->cursor_hint_split = split;
       info->cursor_hint_cursor_class = CURSOR_CLASS_SPLIT;
     }
   }

   DEBUG ("updating trans addr=%p\n", trans);

   {
     SRSaveData *sd;

     sd = gnc_split_register_save_data_new (trans, split);
     gnc_table_save_cells (reg->table, sd);
     gnc_split_register_save_data_destroy (sd);
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

Account *
gnc_split_register_get_account (SplitRegister *reg, const char * cell_name)
{
  const char *name;

  if (!gnc_table_layout_get_cell_changed (reg->table->layout, cell_name, TRUE))
    return NULL;

  name = gnc_table_layout_get_cell_value (reg->table->layout, cell_name);

  return xaccGetAccountFromFullName (gncGetCurrentGroup (),
                                     name, gnc_get_account_separator ());
}

static gboolean
gnc_split_register_auto_calc (SplitRegister *reg, Split *split)
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

  if (STOCK_REGISTER    != reg->type &&
      CURRENCY_REGISTER != reg->type &&
      PORTFOLIO_LEDGER  != reg->type)
    return TRUE;

  account = gnc_split_register_get_account (reg, XFRM_CELL);

  if (!account)
    account = xaccSplitGetAccount (split);

  if (!account)
    account = gnc_split_register_get_default_account (reg);

  account_type = xaccAccountGetType (account);

  if (account_type != STOCK  &&
      account_type != MUTUAL &&
      account_type != CURRENCY)
    return TRUE;

  price_changed = gnc_table_layout_get_cell_changed (reg->table->layout,
                                                     PRIC_CELL, TRUE);
  amount_changed = (gnc_table_layout_get_cell_changed (reg->table->layout,
                                                       DEBT_CELL, TRUE) ||
                    gnc_table_layout_get_cell_changed (reg->table->layout,
                                                       CRED_CELL, TRUE));
  shares_changed = gnc_table_layout_get_cell_changed (reg->table->layout,
                                                      SHRS_CELL, TRUE);

  if (!price_changed && !amount_changed && !shares_changed)
    return TRUE;

  if (shares_changed)
  {
    cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                    SHRS_CELL);
    amount = gnc_price_cell_get_value (cell);
  }
  else
    amount = xaccSplitGetAmount (split);

  if (price_changed)
  {
    cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                    PRIC_CELL);
    price = gnc_price_cell_get_value (cell);
  }
  else
    price = xaccSplitGetSharePrice (split);

  if (amount_changed)
  {
    gnc_numeric credit;
    gnc_numeric debit;

    cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                    CRED_CELL);
    credit = gnc_price_cell_get_value (cell);

    cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                    DEBT_CELL);
    debit = gnc_price_cell_get_value (cell);

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

    choice = gnc_choose_radio_option_dialog_parented
      (gnc_split_register_get_parent (reg),
       title,
       message,
       default_value,
       radio_list);

    for (node = radio_list; node; node = node->next)
      g_free (node->data);

    g_list_free (radio_list);

    switch (choice)
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
      default: /* Cancel */
        return FALSE;
    }
  }

  if (recalc_shares)
    if (!gnc_numeric_zero_p (price))
    {
      BasicCell *cell;

      denom = gnc_split_get_amount_denom (split);

      amount = gnc_numeric_div (value, price, denom, GNC_RND_ROUND);

      cell = gnc_table_layout_get_cell (reg->table->layout, SHRS_CELL);
      gnc_price_cell_set_value ((PriceCell *) cell, amount);
      gnc_basic_cell_set_changed (cell, TRUE);

      if (amount_changed)
      {
        cell = gnc_table_layout_get_cell (reg->table->layout, PRIC_CELL);
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

        debit_cell = gnc_table_layout_get_cell (reg->table->layout,
                                                DEBT_CELL);

        credit_cell = gnc_table_layout_get_cell (reg->table->layout,
                                                 CRED_CELL);

        price = gnc_numeric_neg (price);

        gnc_price_cell_set_debt_credit_value ((PriceCell *) debit_cell,
                                              (PriceCell *) credit_cell,
                                              gnc_numeric_neg (value));

        gnc_basic_cell_set_changed (debit_cell, TRUE);
        gnc_basic_cell_set_changed (credit_cell, TRUE);
      }

      price_cell = gnc_table_layout_get_cell (reg->table->layout, PRIC_CELL);
      gnc_price_cell_set_value ((PriceCell *) price_cell, price);
      gnc_basic_cell_set_changed (price_cell, TRUE);
    }

  if (recalc_value)
  {
    BasicCell *debit_cell;
    BasicCell *credit_cell;

    debit_cell = gnc_table_layout_get_cell (reg->table->layout, DEBT_CELL);
    credit_cell = gnc_table_layout_get_cell (reg->table->layout, CRED_CELL);

    denom = gnc_split_get_value_denom (split);

    value = gnc_numeric_mul (price, amount, denom, GNC_RND_ROUND);

    gnc_price_cell_set_debt_credit_value ((PriceCell *) debit_cell,
                                          (PriceCell *) credit_cell, value);

    gnc_basic_cell_set_changed (debit_cell, TRUE);
    gnc_basic_cell_set_changed (credit_cell, TRUE);

    if (shares_changed)
    {
      BasicCell *cell;

      cell = gnc_table_layout_get_cell (reg->table->layout, PRIC_CELL);
      gnc_basic_cell_set_changed (cell, FALSE);
    }
  }

  return TRUE;
}

static GNCAccountType
gnc_split_register_type_to_account_type (SplitRegisterType sr_type)
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
gnc_split_register_get_debit_string (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info (reg);

  if (!reg)
    return NULL;

  if (info->debit_str)
    return info->debit_str;

  info->debit_str =
    gnc_get_debit_string
    (gnc_split_register_type_to_account_type (reg->type));

  if (info->debit_str)
    return info->debit_str;

  info->debit_str = g_strdup (_("Debit"));

  return info->debit_str;
}

const char *
gnc_split_register_get_credit_string (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info (reg);

  if (!reg)
    return NULL;

  if (info->credit_str)
    return info->credit_str;

  info->credit_str =
    gnc_get_credit_string
    (gnc_split_register_type_to_account_type (reg->type));

  if (info->credit_str)
    return info->credit_str;

  info->credit_str = g_strdup (_("Credit"));

  return info->credit_str;
}

gboolean
gnc_split_register_changed (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info (reg);
  Transaction *pending_trans = xaccTransLookup (&info->pending_trans_guid);

  if (reg == NULL)
    return FALSE;

  if (gnc_table_current_cursor_changed (reg->table, FALSE))
    return TRUE;

  return xaccTransIsOpen (pending_trans);
}

void
gnc_split_register_show_present_divider (SplitRegister *reg,
                                         gboolean show_present)
{
  SRInfo *info = gnc_split_register_get_info (reg);

  if (reg == NULL)
    return;

  info->show_present_divider = show_present;
}

gboolean
gnc_split_register_full_refresh_ok (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info (reg);

  if (!info)
    return FALSE;

  return info->full_refresh;
}

/* configAction strings into the action cell */
/* hack alert -- this stuff really, really should be in a config file ... */
static void
gnc_split_register_config_action (SplitRegister *reg)
{
  ComboCell *cell;

  cell = (ComboCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                  ACTN_CELL);

  /* setup strings in the action pull-down */
  switch (reg->type)
  {
    case BANK_REGISTER:
      /* broken ! FIXME bg */
    case SEARCH_LEDGER:  
      gnc_combo_cell_add_menu_item (cell, _("Deposit"));
      gnc_combo_cell_add_menu_item (cell, _("Withdraw"));
      gnc_combo_cell_add_menu_item (cell, _("Check"));
      gnc_combo_cell_add_menu_item (cell, _("Int"));
      gnc_combo_cell_add_menu_item (cell, _("ATM"));
      gnc_combo_cell_add_menu_item (cell, _("Teller"));
      /* Action: Point Of Sale */
      gnc_combo_cell_add_menu_item (cell, _("POS"));
      gnc_combo_cell_add_menu_item (cell, _("Phone"));
      gnc_combo_cell_add_menu_item (cell, _("Online"));
      /* Action: Automatic Deposit ?!? */
      gnc_combo_cell_add_menu_item (cell, _("AutoDep"));
      gnc_combo_cell_add_menu_item (cell, _("Wire"));
      gnc_combo_cell_add_menu_item (cell, _("Credit"));
      gnc_combo_cell_add_menu_item (cell, _("Direct Debit"));
      gnc_combo_cell_add_menu_item (cell, _("Transfer"));
      break;
    case CASH_REGISTER:
      gnc_combo_cell_add_menu_item (cell, _("Buy"));
      gnc_combo_cell_add_menu_item (cell, _("Sell"));
      break;
    case ASSET_REGISTER:
      gnc_combo_cell_add_menu_item (cell, _("Buy"));
      gnc_combo_cell_add_menu_item (cell, _("Sell"));
      gnc_combo_cell_add_menu_item (cell, _("Fee"));
      break;
    case CREDIT_REGISTER:
      gnc_combo_cell_add_menu_item (cell, _("ATM"));
      gnc_combo_cell_add_menu_item (cell, _("Buy"));
      gnc_combo_cell_add_menu_item (cell, _("Credit"));
      gnc_combo_cell_add_menu_item (cell, _("Fee"));
      gnc_combo_cell_add_menu_item (cell, _("Int"));
      gnc_combo_cell_add_menu_item (cell, _("Online"));
      gnc_combo_cell_add_menu_item (cell, _("Sell"));
      break;
    case LIABILITY_REGISTER:
      gnc_combo_cell_add_menu_item (cell, _("Buy"));
      gnc_combo_cell_add_menu_item (cell, _("Sell"));
      gnc_combo_cell_add_menu_item (cell, _("Loan"));
      gnc_combo_cell_add_menu_item (cell, _("Int"));
      gnc_combo_cell_add_menu_item (cell, _("Payment"));
      break;
    case INCOME_LEDGER:
    case INCOME_REGISTER:
      gnc_combo_cell_add_menu_item (cell, _("Buy"));
      gnc_combo_cell_add_menu_item (cell, _("Sell"));
      gnc_combo_cell_add_menu_item (cell, _("Int"));
      gnc_combo_cell_add_menu_item (cell, _("Payment"));
      gnc_combo_cell_add_menu_item (cell, _("Rebate"));
      break;
    case EXPENSE_REGISTER:
      gnc_combo_cell_add_menu_item (cell, _("Buy"));
      gnc_combo_cell_add_menu_item (cell, _("Sell"));
      break;
    case GENERAL_LEDGER:
    case EQUITY_REGISTER:
      gnc_combo_cell_add_menu_item (cell, _("Buy"));
      gnc_combo_cell_add_menu_item (cell, _("Sell"));
      gnc_combo_cell_add_menu_item (cell, _("Equity"));
      break;
    case STOCK_REGISTER:
    case PORTFOLIO_LEDGER:
    case CURRENCY_REGISTER:
      gnc_combo_cell_add_menu_item (cell, _("Buy"));
      gnc_combo_cell_add_menu_item (cell, _("Sell"));
      gnc_combo_cell_add_menu_item (cell, _("Price"));
      gnc_combo_cell_add_menu_item (cell, _("Fee"));
      /* Action: Dividend */
      gnc_combo_cell_add_menu_item (cell, _("Div")); 
      gnc_combo_cell_add_menu_item (cell, _("Int"));
      /* Action: Long Term Capital Gains */
      gnc_combo_cell_add_menu_item (cell, _("LTCG"));
      /* Action: Short Term Capital Gains */
      gnc_combo_cell_add_menu_item (cell, _("STCG"));
      gnc_combo_cell_add_menu_item (cell, _("Income"));
      /* Action: Distribution */
      gnc_combo_cell_add_menu_item (cell, _("Dist")); 
      gnc_combo_cell_add_menu_item (cell, _("Split"));
      break;

    default:
      gnc_combo_cell_add_menu_item (cell, _("Buy"));
      gnc_combo_cell_add_menu_item (cell, _("Sell"));
      break;
  }
}

static void
gnc_split_register_config_cells (SplitRegister *reg)
{
  /* The num cell is the transaction number */
  xaccSetBasicCellBlankHelp (gnc_table_layout_get_cell (reg->table->layout,
                                                        NUM_CELL),
                             _("Enter the transaction number, such as the "
                               "check number"));

  /* the xfer cells */
  {
    const char *help = _("Enter the account to transfer from, or choose "
                         "one from the list");
    xaccSetBasicCellBlankHelp (gnc_table_layout_get_cell (reg->table->layout,
                                                          MXFRM_CELL), help);
    xaccSetBasicCellBlankHelp (gnc_table_layout_get_cell (reg->table->layout,
                                                          XFRM_CELL), help);
  }

  {
    const char *help = _("This transaction has multiple splits; "
                         "press the Split button to see them all");

    gnc_combo_cell_add_ignore_string
      ((ComboCell *)
       gnc_table_layout_get_cell (reg->table->layout, MXFRM_CELL),
       _("-- Split Transaction --"), help);
  }

  {
    const char *help = _("This transaction is a stock split; "
                         "press the Split button to see details");

    gnc_combo_cell_add_ignore_string
      ((ComboCell *)
       gnc_table_layout_get_cell (reg->table->layout, MXFRM_CELL),
       _("-- Stock Split --"), help);
  }

  /* the action cell */
  gnc_combo_cell_set_autosize
    ((ComboCell *)
     gnc_table_layout_get_cell (reg->table->layout, ACTN_CELL), TRUE);

  /* the memo cell */
  xaccSetBasicCellBlankHelp
    (gnc_table_layout_get_cell (reg->table->layout, MEMO_CELL),
     _("Enter a description of the split"));

  /* the desc cell */
  xaccSetBasicCellBlankHelp
    (gnc_table_layout_get_cell (reg->table->layout, DESC_CELL),
     _("Enter a description of the transaction"));

  /* the notes cell */
  xaccSetBasicCellBlankHelp
    (gnc_table_layout_get_cell (reg->table->layout, NOTES_CELL),
     _("Enter notes for the transaction"));

  /* the formula cells */
  xaccSetBasicCellBlankHelp
    (gnc_table_layout_get_cell (reg->table->layout, FCRED_CELL),
     _("Enter credit formula for real transaction"));

  xaccSetBasicCellBlankHelp
    (gnc_table_layout_get_cell (reg->table->layout, FDEBT_CELL),
     _("Enter debit formula for real transaction"));

  /* Use 6 decimal places for prices */
  gnc_price_cell_set_fraction
    ((PriceCell *)
     gnc_table_layout_get_cell (reg->table->layout, PRIC_CELL), 1000000);

  /* Initialize shares and share balance cells */
  gnc_price_cell_set_print_info
    ((PriceCell *) gnc_table_layout_get_cell (reg->table->layout, SHRS_CELL),
     gnc_default_share_print_info ());

  gnc_price_cell_set_print_info
    ((PriceCell *) gnc_table_layout_get_cell (reg->table->layout, TSHRS_CELL),
     gnc_default_share_print_info ());

  /* The action cell should accept strings not in the list */
  gnc_combo_cell_set_strict
    ((ComboCell *)
     gnc_table_layout_get_cell (reg->table->layout, ACTN_CELL), FALSE);

  xaccSetBasicCellBlankHelp
    (gnc_table_layout_get_cell (reg->table->layout, ACTN_CELL),
     _("Enter the type of transaction, or choose "
       "one from the list"));

  /* number format for share quantities in stock ledgers */
  switch (reg->type)
  {
    case CURRENCY_REGISTER:
    case STOCK_REGISTER:
    case PORTFOLIO_LEDGER:
      gnc_price_cell_set_print_info
        ((PriceCell *)
         gnc_table_layout_get_cell (reg->table->layout, PRIC_CELL),
         gnc_default_price_print_info ());

      xaccSetBasicCellBlankHelp
        (gnc_table_layout_get_cell (reg->table->layout, PRIC_CELL),
         _("Enter the share price"));

      xaccSetBasicCellBlankHelp
        (gnc_table_layout_get_cell (reg->table->layout, SHRS_CELL),
         _("Enter the number of shares bought or sold"));
      break;

    default:
      break;
  }

  /* add menu items for the action cell */
  gnc_split_register_config_action (reg);
}

static void 
gnc_split_register_init (SplitRegister *reg,
                         SplitRegisterType type,
                         SplitRegisterStyle style,
                         gboolean use_double_line,
                         gboolean is_template)
{
  TableLayout *layout;
  TableModel *model;
  TableControl *control;

  reg->sr_info = NULL;

  reg->type = type;
  reg->style = style;
  reg->use_double_line = use_double_line;
  reg->is_template = is_template;

  layout = gnc_split_register_layout_new (reg);

  if (is_template)
    model = gnc_template_register_model_new ();
  else
    model = gnc_split_register_model_new ();
  model->handler_user_data = reg;

  control = gnc_split_register_control_new ();
  control->user_data = reg;

  reg->table = gnc_table_new (layout, model, control);

  gnc_split_register_config_cells (reg);

  /* Set up header */
  {
    VirtualCellLocation vcell_loc = { 0, 0 };
    CellBlock *header;

    header = gnc_table_layout_get_cursor (reg->table->layout, CURSOR_HEADER);

    gnc_table_set_vcell (reg->table, header, NULL, TRUE, TRUE, vcell_loc);
  }

  /* Set up first and only initial row */
  {
    VirtualLocation vloc;
    CellBlock *cursor;

    vloc.vcell_loc.virt_row = 1;
    vloc.vcell_loc.virt_col = 0;
    vloc.phys_row_offset = 0;
    vloc.phys_col_offset = 0;

    cursor = gnc_table_layout_get_cursor (reg->table->layout,
                                          CURSOR_SINGLE_LEDGER);

    gnc_table_set_vcell (reg->table, cursor, NULL, TRUE, TRUE, vloc.vcell_loc);
    gnc_table_move_cursor (reg->table, vloc);
  }
}

SplitRegister *
gnc_split_register_new (SplitRegisterType type,
                        SplitRegisterStyle style,
                        gboolean use_double_line,
                        gboolean is_template)
{
  SplitRegister * reg;

  reg = g_new0 (SplitRegister, 1);

  if (type >= NUM_SINGLE_REGISTER_TYPES)
    style = REG_STYLE_JOURNAL;

  gnc_split_register_init (reg,
                           type,
                           style,
                           use_double_line,
                           is_template);

  return reg;
}

void
gnc_split_register_config (SplitRegister *reg,
                           SplitRegisterType newtype,
                           SplitRegisterStyle newstyle,
                           gboolean use_double_line)
{
  if (!reg) return;

  reg->type = newtype;

  if (reg->type >= NUM_SINGLE_REGISTER_TYPES)
    newstyle = REG_STYLE_JOURNAL;

  reg->style = newstyle;
  reg->use_double_line = use_double_line;

  gnc_table_realize_gui (reg->table);
}

static void
gnc_split_register_destroy_info (SplitRegister *reg)
{
  SRInfo *info;

  if (reg == NULL)
    return;

  info = reg->sr_info;
  if (!info)
    return;

  g_free (info->debit_str);
  g_free (info->tdebit_str);
  g_free (info->credit_str);
  g_free (info->tcredit_str);

  info->debit_str = NULL;
  info->tdebit_str = NULL;
  info->credit_str = NULL;
  info->tcredit_str = NULL;

  g_free (reg->sr_info);

  reg->sr_info = NULL;
}

void
gnc_split_register_set_data (SplitRegister *reg, void *user_data,
                             SRGetParentCallback get_parent)
{
  SRInfo *info = gnc_split_register_get_info (reg);

  g_return_if_fail (reg != NULL);

  info->user_data = user_data;
  info->get_parent = get_parent;
}

static void
gnc_split_register_cleanup (SplitRegister *reg)
{
   SRInfo *info = gnc_split_register_get_info (reg);
   Split *blank_split = xaccSplitLookup (&info->blank_split_guid);
   Transaction *pending_trans = xaccTransLookup (&info->pending_trans_guid);
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

   gnc_split_register_destroy_info (reg);

   gnc_resume_gui_refresh ();
}

void 
gnc_split_register_destroy (SplitRegister *reg)
{
  GList *node;

  if (!reg)
    return;

  gnc_split_register_cleanup (reg);

  gnc_table_destroy (reg->table);
  reg->table = NULL;

  /* free the memory itself */
  g_free (reg);
}
