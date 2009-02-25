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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/
/*
 * split-register.c
 * author Copyright (c) 1998-2000 Linas Vepstas <linas@linas.org>
 * author Copyright (c) 2000-2001 Dave Peticolas <dave@krondo.com>
 */
#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <libguile.h>

#include "combocell.h"
#include "datecell.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-gconf-utils.h"
#include "split-register-p.h"
#include "gnc-ledger-display.h"
#include "gnc-ui.h"
#include "guile-util.h"
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
#include "dialog-account.h"


/** static variables ******************************************************/

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_LEDGER;

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

  gnc_copy_split_scm_onto_split(split_scm, to, gnc_get_current_book ());
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

  gnc_copy_trans_scm_onto_trans(trans_scm, to, do_commit,
                                gnc_get_current_book ());
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

/* returns TRUE if begin_edit was aborted */
gboolean
gnc_split_register_begin_edit_or_warn(SRInfo *info, Transaction *trans)
{
  ENTER("info=%p, trans=%p", info, trans);

      if (!xaccTransIsOpen(trans)) {
          xaccTransBeginEdit(trans);
          /* This is now the pending transaction */
          info->pending_trans_guid = *xaccTransGetGUID(trans);
          LEAVE("opened and marked pending");
          return FALSE;
      } else {
        Split       *blank_split = xaccSplitLookup (&info->blank_split_guid,
                                                    gnc_get_current_book ());
        Transaction *blank_trans = xaccSplitGetParent (blank_split);

        if (trans == blank_trans) {
          /* This is a brand-new transaction. It is already
           * open, so just mark it as pending. */
          info->pending_trans_guid = *xaccTransGetGUID(trans);
          LEAVE("already open, now pending.");
          return FALSE;
        } else {
          GtkWidget *parent = NULL;
          if (info->get_parent)
              parent = info->get_parent(info->user_data);
          gnc_error_dialog(parent, "%s", _("This transaction is already being edited in another register. Please finish editing it there first."));
          LEAVE("already editing");
          return TRUE;
        }
      }
  LEAVE(" ");
}

void
gnc_split_register_expand_current_trans (SplitRegister *reg, gboolean expand)
{
  SRInfo *info = gnc_split_register_get_info (reg);
  VirtualLocation virt_loc;

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

  gnc_split_register_set_trans_visible(
      reg, reg->table->current_cursor_loc.vcell_loc, expand, FALSE);

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
    return TRUE;

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

  return gnc_split_register_get_split(
      reg, reg->table->current_cursor_loc.vcell_loc);
}

Split *
gnc_split_register_get_blank_split (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info (reg);

  if (!reg) return NULL;

  return xaccSplitLookup (&info->blank_split_guid, gnc_get_current_book ());
}

gboolean
gnc_split_register_get_split_virt_loc (SplitRegister *reg, Split *split,
                                       VirtualCellLocation *vcell_loc)
{
  Table *table;
  int v_row;
  int v_col;

  if (!reg || !split) return FALSE;

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
      if (!vcell || !vcell->visible)
        continue;

      s = xaccSplitLookup (vcell->vcell_data, gnc_get_current_book ());

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
  CursorClass cursor_class;
  Transaction *trans;
  Split *return_split;
  Split *trans_split;
  Split *blank_split;
  gboolean changed;
  Split *split;

  ENTER("reg=%p", reg);

  blank_split = xaccSplitLookup(&info->blank_split_guid,
                                gnc_get_current_book ());
  split = gnc_split_register_get_current_split (reg);
  trans = gnc_split_register_get_current_trans (reg);
  trans_split = gnc_split_register_get_current_trans_split (reg, NULL);

  /* This shouldn't happen, but be paranoid. */
  if (trans == NULL)
  {
    LEAVE("no transaction");
    return NULL;
  }

  cursor_class = gnc_split_register_get_current_cursor_class (reg);

  /* Can't do anything with this. */
  if (cursor_class == CURSOR_CLASS_NONE)
  {
    LEAVE("no cursor class");
    return NULL;
  }

  /* This shouldn't happen, but be paranoid. */
  if ((split == NULL) && (cursor_class == CURSOR_CLASS_TRANS))
  {
    LEAVE("no split with transaction class");
    return NULL;
  }

  changed = gnc_table_current_cursor_changed (reg->table, FALSE);

  /* See if we were asked to duplicate an unchanged blank split.
   * There's no point in doing that! */
  if (!changed && ((split == NULL) || (split == blank_split)))
  {
    LEAVE("skip unchanged blank split");
    return NULL;
  }

  gnc_suspend_gui_refresh ();

  /* If the cursor has been edited, we are going to have to commit
   * it before we can duplicate. Make sure the user wants to do that. */
  if (changed)
  {
    GtkWidget *dialog, *window;
    gint response;
    const char *title = _("Save transaction before duplicating?");
    const char *message =
      _("The current transaction has been changed. Would you like to "
	"record the changes before duplicating the transaction, or "
	"cancel the duplication?");

    window = gnc_split_register_get_parent(reg);
    dialog = gtk_message_dialog_new(GTK_WINDOW(window),
				    GTK_DIALOG_DESTROY_WITH_PARENT,
				    GTK_MESSAGE_QUESTION,
				    GTK_BUTTONS_CANCEL,
				    "%s", title);
    gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
					     "%s", message);
    gtk_dialog_add_button(GTK_DIALOG(dialog),
			  _("_Record"), GTK_RESPONSE_ACCEPT);
    response = gnc_dialog_run(GTK_DIALOG(dialog), "transaction_duplicated");
    gtk_widget_destroy(dialog);

    if (response != GTK_RESPONSE_ACCEPT)
    {
      gnc_resume_gui_refresh ();
      LEAVE("save cancelled");
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

    new_split = xaccMallocSplit (gnc_get_current_book ());

    xaccTransBeginEdit (trans);
    xaccSplitSetParent (new_split, trans);
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
      LEAVE("dup cancelled");
      return NULL;
    }

    split_index = xaccTransGetSplitIndex(trans, split);
    trans_split_index = xaccTransGetSplitIndex(trans, trans_split);

    /* we should *always* find the split, but be paranoid */
    if (split_index < 0)
    {
      gnc_resume_gui_refresh ();
      LEAVE("no split");
      return NULL;
    }

    new_trans = xaccMallocTransaction (gnc_get_current_book ());

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

  LEAVE(" ");
  return return_split;
}

static void
gnc_split_register_copy_current_internal (SplitRegister *reg,
                                          gboolean use_cut_semantics)
{
  SRInfo *info = gnc_split_register_get_info(reg);
  CursorClass cursor_class;
  Transaction *trans;
  Split *blank_split;
  gboolean changed;
  Split *split;
  SCM new_item;

  g_return_if_fail(reg);
  ENTER("reg=%p, use_cut_semantics=%s", reg,
        use_cut_semantics? "TRUE" : "FALSE");

  blank_split = xaccSplitLookup(&info->blank_split_guid,
                                gnc_get_current_book ());
  split = gnc_split_register_get_current_split (reg);
  trans = gnc_split_register_get_current_trans (reg);

  /* This shouldn't happen, but be paranoid. */
  if (trans == NULL)
  {
    LEAVE("no trans");
    return;
  }

  cursor_class = gnc_split_register_get_current_cursor_class (reg);

  /* Can't do anything with this. */
  if (cursor_class == CURSOR_CLASS_NONE)
  {
    LEAVE("no cursor class");
    return;
  }

  /* This shouldn't happen, but be paranoid. */
  if ((split == NULL) && (cursor_class == CURSOR_CLASS_TRANS))
  {
    g_warning("BUG DETECTED: transaction cursor with no anchoring split!");
    LEAVE("transaction cursor with no anchoring split");
    return;
  }

  changed = gnc_table_current_cursor_changed (reg->table, FALSE);

  /* See if we were asked to copy an unchanged blank split. Don't. */
  if (!changed && ((split == NULL) || (split == blank_split)))
  {
    /* We're either on an unedited, brand-new split or an unedited, brand-new
     * transaction (the transaction anchored by the blank split.) */
    /* FIXME: This doesn't work exactly right. When entering a new transaction,
     *        you can edit the description, move to a split row, then move
     *        back to the description, then ask for a copy, and this code will
     *        be reached. It forgets that you changed the row the first time
     *        you were there.  -Charles */
    LEAVE("nothing to copy/cut");
    return;
  }

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

      copied_leader_guid = *guid_null();
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

        split_index = xaccTransGetSplitIndex(trans, split);
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
  {
    g_warning("BUG DETECTED: copy failed");
    LEAVE("copy failed");
    return;
  }

  /* unprotect the old object, if any */
  if (copied_item != SCM_UNDEFINED)
    scm_gc_unprotect_object(copied_item);

  copied_item = new_item;
  scm_gc_protect_object(copied_item);

  copied_class = cursor_class;
  LEAVE("%s %s", use_cut_semantics? "cut" : "copied",
        cursor_class == CURSOR_CLASS_SPLIT? "split" : "transaction");
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
  CursorClass cursor_class;
  Transaction *trans;
  Split *blank_split;
  gboolean changed;
  Split *split;

  blank_split = xaccSplitLookup (&info->blank_split_guid,
                                 gnc_get_current_book ());
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
  CursorClass cursor_class;
  Transaction *trans;
  Transaction *blank_trans;
  Split *blank_split;
  Split *trans_split;
  Split *split;

  ENTER("reg=%p", reg);

  if (copied_class == CURSOR_CLASS_NONE)
  {
    LEAVE("no copied cursor class");
    return;
  }

  blank_split = xaccSplitLookup (&info->blank_split_guid,
                                 gnc_get_current_book ());
  blank_trans = xaccSplitGetParent (blank_split);
  split = gnc_split_register_get_current_split (reg);
  trans = gnc_split_register_get_current_trans (reg);

  trans_split = gnc_split_register_get_current_trans_split (reg, NULL);

  /* This shouldn't happen, but be paranoid. */
  if (trans == NULL)
  {
    LEAVE("no transaction");
    return;
  }

  cursor_class = gnc_split_register_get_current_cursor_class (reg);

  /* Can't do anything with this. */
  if (cursor_class == CURSOR_CLASS_NONE)
  {
    LEAVE("no current cursor class");
    return;
  }

  /* This shouldn't happen, but be paranoid. */
  if ((split == NULL) && (cursor_class == CURSOR_CLASS_TRANS))
  {
    g_warning("BUG DETECTED: transaction cursor with no anchoring split!");
    LEAVE("transaction cursor with no anchoring split");
    return;
  }

  if (cursor_class == CURSOR_CLASS_SPLIT)
  {
    const char *message = _("You are about to overwrite an existing split. "
                            "Are you sure you want to do that?");

    if (copied_class == CURSOR_CLASS_TRANS)
    {
      /* An entire transaction was copied, but we're just on a split. */
      LEAVE("can't copy trans to split");
      return;
    }

    /* Ask before overwriting an existing split. */
    if (split != NULL &&
        !gnc_verify_dialog (gnc_split_register_get_parent (reg),
                            FALSE, "%s", message))
    {
      LEAVE("user cancelled");
      return;
    }

    gnc_suspend_gui_refresh ();

    if (split == NULL)
    { /* We are on a null split in an expanded transaction. */
      split = xaccMallocSplit(gnc_get_current_book ());
      xaccSplitSetParent(split, trans);
    }

    gnc_copy_split_scm_onto_split(copied_item, split,
                                  gnc_get_current_book ());
  }
  else
  {
    const char *message = _("You are about to overwrite an existing "
                            "transaction. "
                            "Are you sure you want to do that?");
    Account * copied_leader;
    const GUID *new_guid;
    int trans_split_index;
    int split_index;
    int num_splits;

    if (copied_class == CURSOR_CLASS_SPLIT)
    {
      LEAVE("can't copy split to transaction");
      return;
    }

    /* Ask before overwriting an existing transaction. */
    if (split != blank_split &&
        !gnc_verify_dialog(gnc_split_register_get_parent(reg),
                           FALSE, "%s", message))
    {
      LEAVE("user cancelled");
      return;
    }

    /* Open the transaction for editing. */
    if (gnc_split_register_begin_edit_or_warn(info, trans))
    {
      LEAVE("can't begin editing");
      return;
    }

    gnc_suspend_gui_refresh ();

    DEBUG("Pasting txn, trans=%p, split=%p, blank_trans=%p, blank_split=%p",
          trans, split, blank_trans, blank_split);

    split_index = xaccTransGetSplitIndex(trans, split);
    trans_split_index = xaccTransGetSplitIndex(trans, trans_split);

    copied_leader = xaccAccountLookup(&copied_leader_guid,
                                      gnc_get_current_book());
    if (copied_leader && (gnc_split_register_get_default_account(reg) != NULL))
    {
      new_guid = &info->default_account;
      gnc_copy_trans_scm_onto_trans_swap_accounts(copied_item, trans,
                                                  &copied_leader_guid,
                                                  new_guid, FALSE,
                                                  gnc_get_current_book ());
    }
    else
      gnc_copy_trans_scm_onto_trans(copied_item, trans, FALSE,
                                    gnc_get_current_book ());

    num_splits = xaccTransCountSplits(trans);
    if (split_index >= num_splits)
      split_index = 0;

    if (trans == blank_trans)
    {
      /* In pasting, the blank split is deleted. Pick a new one. */
      blank_split = xaccTransGetSplit(trans, 0);
      info->blank_split_guid = *xaccSplitGetGUID (blank_split);
      info->blank_split_edited = TRUE;
      DEBUG("replacement blank_split=%p", blank_split);

      /* NOTE: At this point, the blank transaction virtual cell is still
       *       anchored by the old, deleted blank split. The register will
       *       have to be reloaded (redrawn) to correct this. */
    }

    info->cursor_hint_trans = trans;
    info->cursor_hint_split = xaccTransGetSplit(trans, split_index);
    info->cursor_hint_trans_split = xaccTransGetSplit(trans,
                                                      trans_split_index);
    info->cursor_hint_cursor_class = CURSOR_CLASS_TRANS;
  }

  /* Refresh the GUI. */
  gnc_resume_gui_refresh ();
  LEAVE(" ");
}

void
gnc_split_register_delete_current_split (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info (reg);
  Transaction *pending_trans;
  Transaction *trans;
  Split *blank_split;
  Split *split;

  if (!reg) return;

  blank_split = xaccSplitLookup (&info->blank_split_guid,
                                 gnc_get_current_book ());

  pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                   gnc_get_current_book ());

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

  trans = xaccSplitGetParent(split);

  /* Check pending transaction */
  if (trans == pending_trans) {
      g_assert(xaccTransIsOpen(trans));
  } else {
      g_assert(!pending_trans);
      if (gnc_split_register_begin_edit_or_warn(info, trans)) {
        gnc_resume_gui_refresh ();
        return;
      }
  }
  xaccSplitDestroy (split);

  gnc_resume_gui_refresh ();
  gnc_split_register_redraw(reg);
}

void
gnc_split_register_delete_current_trans (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info (reg);
  Transaction *pending_trans;
  Transaction *trans;
  Split *blank_split;
  Split *split;
  gboolean was_open;

  ENTER("reg=%p", reg);
  if (!reg)
  {
    LEAVE("no register");
    return;
  }

  blank_split = xaccSplitLookup (&info->blank_split_guid,
                                 gnc_get_current_book ());
  pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                   gnc_get_current_book ());

  /* get the current split based on cursor position */
  split = gnc_split_register_get_current_split (reg);
  if (split == NULL)
  {
    LEAVE("no split");
    return;
  }

  gnc_suspend_gui_refresh ();
  trans = xaccSplitGetParent(split);

  /* If we just deleted the blank split, clean up. The user is
   * allowed to delete the blank split as a method for discarding
   * any edits they may have made to it. */
  if (split == blank_split) {
      DEBUG("deleting blank split");
      info->blank_split_guid = *guid_null();
  } else {
      info->trans_expanded = FALSE;      
  }
  
  /* Check pending transaction */
  if (trans == pending_trans) {
      DEBUG("clearing pending trans");
      info->pending_trans_guid = *guid_null();
      pending_trans = NULL;
  }

  was_open = xaccTransIsOpen(trans);
  xaccTransDestroy(trans);
  if (was_open)
  {
      DEBUG("committing");
      xaccTransCommitEdit(trans);
  }
  gnc_resume_gui_refresh ();
  LEAVE(" ");
}

void
gnc_split_register_void_current_trans (SplitRegister *reg, const char *reason)
{
  SRInfo *info = gnc_split_register_get_info (reg);
  Transaction *pending_trans;
  Transaction *trans;
  Split *blank_split;
  Split *split;

  if (!reg) return;

  blank_split = xaccSplitLookup (&info->blank_split_guid,
                                 gnc_get_current_book ());
  pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                   gnc_get_current_book ());

  /* get the current split based on cursor position */
  split = gnc_split_register_get_current_split (reg);
  if (split == NULL)
    return;

  /* Bail if trying to void the blank split. */
  if (split == blank_split)
    return;

  /* already voided. */
  if (xaccSplitGetReconcile (split) == VREC)
    return;

  info->trans_expanded = FALSE;

  gnc_suspend_gui_refresh ();

  trans = xaccSplitGetParent(split);
  xaccTransVoid(trans, reason);

  /* Check pending transaction */
  if (trans == pending_trans)
  {
    info->pending_trans_guid = *guid_null();
    pending_trans = NULL;
  }
  if (xaccTransIsOpen(trans)) {
      PERR("We should not be voiding an open transaction.");
      xaccTransCommitEdit(trans);
  }
  gnc_resume_gui_refresh ();
}

void
gnc_split_register_unvoid_current_trans (SplitRegister *reg)
{
  SRInfo *info = gnc_split_register_get_info (reg);
  Transaction *pending_trans;
  Transaction *trans;
  Split *blank_split;
  Split *split;

  if (!reg) return;

  blank_split = xaccSplitLookup (&info->blank_split_guid,
                                 gnc_get_current_book ());
  pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                   gnc_get_current_book ());

  /* get the current split based on cursor position */
  split = gnc_split_register_get_current_split (reg);
  if (split == NULL)
    return;

  /* Bail if trying to unvoid the blank split. */
  if (split == blank_split)
    return;

  /* not voided. */
  if (xaccSplitGetReconcile (split) != VREC)
    return;

  info->trans_expanded = FALSE;

  gnc_suspend_gui_refresh ();

  trans = xaccSplitGetParent(split);

  xaccTransUnvoid(trans);

  /* Check pending transaction */
  if (trans == pending_trans)
  {
    info->pending_trans_guid = *guid_null();
    pending_trans = NULL;
  }

  gnc_resume_gui_refresh ();
}

void
gnc_split_register_empty_current_trans_except_split (SplitRegister *reg, 
                                                     Split *split)
{
  SRInfo *info;
  Transaction *trans;
  Transaction *pending;
  int i = 0;
  Split *s;

  if ((reg == NULL)  || (split == NULL))
    return;

  gnc_suspend_gui_refresh ();
  info = gnc_split_register_get_info(reg);
  pending = xaccTransLookup(&info->pending_trans_guid, gnc_get_current_book());

  trans = xaccSplitGetParent(split);
  if (!pending) {
      if (gnc_split_register_begin_edit_or_warn(info, trans)) {
        gnc_resume_gui_refresh ();
        return;
      }
  } else if (pending == trans) {
      g_assert(xaccTransIsOpen(trans));
  } else g_assert_not_reached();

  while ((s = xaccTransGetSplit(trans, i)) != NULL) {
      if (s != split) 
          xaccSplitDestroy(s);
      else i++;
  }

  gnc_resume_gui_refresh ();
  gnc_split_register_redraw(reg);
}

void
gnc_split_register_empty_current_trans (SplitRegister *reg)
{
  Split *split;

  /* get the current split based on cursor position */
  split = gnc_split_register_get_current_split (reg);
  gnc_split_register_empty_current_trans_except_split (reg, split);
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
  SRInfo *info = gnc_split_register_get_info (reg);
  Transaction *pending_trans;

  pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                   gnc_get_current_book ());

  /* Get the currently open transaction, rollback the edits on it, and
   * then repaint everything. To repaint everything, make a note of
   * all of the accounts that will be affected by this rollback. */
  if (!xaccTransIsOpen (pending_trans))
  {
    gnc_split_register_cancel_cursor_split_changes (reg);
    return;
  }

  if (!pending_trans)
    return;

  gnc_suspend_gui_refresh ();

  xaccTransRollbackEdit (pending_trans);

  info->pending_trans_guid = *guid_null ();

  gnc_resume_gui_refresh ();
  gnc_split_register_redraw(reg);
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

        temp_split = xaccMallocSplit (gnc_get_current_book ());
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
   Transaction *pending_trans;
   Transaction *blank_trans;
   Transaction *trans;
   Account *account;
   Split *blank_split;
   const char *memo;
   const char *desc;
   Split *split;

   ENTER("reg=%p, do_commit=%s", reg, do_commit ? "TRUE" : "FALSE");

   if (!reg)
   {
     LEAVE("no register");
     return FALSE;
   }

   blank_split = xaccSplitLookup (&info->blank_split_guid,
                                  gnc_get_current_book ());

   pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                    gnc_get_current_book ());

   blank_trans = xaccSplitGetParent (blank_split);

   /* get the handle to the current split and transaction */
   split = gnc_split_register_get_current_split (reg);
   trans = gnc_split_register_get_current_trans (reg);
   if (trans == NULL)
   {
     LEAVE("no transaction");
     return FALSE;
   }

   /* use the changed flag to avoid heavy-weight updates
    * of the split & transaction fields. This will help
    * cut down on unnecessary register redraws. */
   if (!gnc_table_current_cursor_changed (reg->table, FALSE))
   {
     if (!do_commit)
     {
       LEAVE("commit unnecessary");
       return FALSE;
     }

     if (!xaccTransIsOpen(trans))
     {
       LEAVE("transaction not open");
       return FALSE;
     }

     if (trans == pending_trans ||
         (trans == blank_trans && info->blank_split_edited)) {
       /* We are going to commit. */

       gnc_suspend_gui_refresh ();

       if (trans == blank_trans) {
           /* We have to clear the blank split before the
            * refresh or a new one won't be created. */
           info->last_date_entered = xaccTransGetDate (trans);
           info->blank_split_guid = *guid_null ();
           info->blank_split_edited = FALSE;
       }

       /* We have to clear the pending guid *before* committing the
        * trans, because the event handler will find it otherwise. */
       if (trans == pending_trans)
           info->pending_trans_guid = *guid_null ();

       PINFO("committing trans (%p)", trans);
       xaccTransCommitEdit(trans);

       gnc_resume_gui_refresh ();
     }
     else
       DEBUG("leaving trans (%p) open", trans);

     LEAVE("unchanged cursor");
     return TRUE;
   }

   DEBUG("save split=%p", split);
   DEBUG("blank_split=%p, blank_trans=%p, pending_trans=%p, trans=%p",
         blank_split, blank_trans, pending_trans, trans);

   /* Act on any changes to the current cell before the save. */
   gnc_split_register_check_cell (reg,
                                  gnc_table_get_current_cell_name (reg->table));

   if (!gnc_split_register_auto_calc (reg, split))
   {
     LEAVE("auto calc failed");
     return FALSE;
   }

   /* Validate the transfer account names */
   (void)gnc_split_register_get_account (reg, MXFRM_CELL);
   (void)gnc_split_register_get_account (reg, XFRM_CELL);

   /* Maybe deal with exchange-rate transfers */
   if (gnc_split_register_handle_exchange (reg, FALSE))
   {
     LEAVE("no exchange rate");
     return TRUE;
   }

   gnc_suspend_gui_refresh ();

   /* determine whether we should commit the pending transaction */
   if (pending_trans != trans)
   {
       // FIXME: How could the pending transaction not be open?
       // FIXME: For that matter, how could an open pending
       // transaction ever not be the current trans?
       if (xaccTransIsOpen (pending_trans)) {
           g_warning("Impossible? commiting pending %p", pending_trans);
           xaccTransCommitEdit (pending_trans);
       } else if (pending_trans) {
           g_critical("BUG DETECTED! pending transaction (%p) not open",
                      pending_trans);
           g_assert_not_reached();
       }

       if (trans == blank_trans) {
           /* Don't begin editing the blank trans, because it's
              already open, but mark it pending now. */
           g_assert(xaccTransIsOpen(blank_trans));
           /* This is now the pending transaction */
           info->pending_trans_guid = *xaccTransGetGUID(blank_trans);
       } else {
           PINFO("beginning edit of trans %p", trans);
           if (gnc_split_register_begin_edit_or_warn(info, trans))
	   {
	       gnc_resume_gui_refresh ();
               LEAVE("transaction opened elsewhere");
               return FALSE;
	   }
       }
       pending_trans = trans;
   }
   g_assert(xaccTransIsOpen(trans));

   /* If we are saving a brand new transaction and the blank split hasn't
    * been edited, then we need to give it a default account. */
   /* Q: Why check 'split == blank_split'? Isn't 'trans == blank_trans'
    *    even better? What if there were some way that we could be on
    *    a row other than the transaction row or blank split row, but
    *    the blank split still hasn't been edited? It seems to be assumed
    *    that it isn't possible, but... -Charles, Jan 2009 */
   if (split == blank_split && !info->blank_split_edited)
   {
     /* If we've reached this point, it means that the blank split is
      * anchoring the transaction - see gnc_split_register_add_transaction()
      * for an explanation - and the transaction has been edited (as evidenced
      * by the earlier check for a changed cursor.) Since the blank split
      * itself has not been edited, we'll have to assign a default account. */
     account = gnc_split_register_get_default_account(reg);
     if (account)
       xaccSplitSetAccount(blank_split, account);
     xaccTransSetDateEnteredSecs(trans, time(NULL));
   }

   if (split == NULL)
   {
     /* If we were asked to save data for a row for which there is no
      * associated split, then assume that this was an "empty" row - see
      * gnc_split_register_add_transaction() for an explanation. This row
      * is used to add splits to an existing transaction, or to add the
      * 2nd through nth split rows to a brand new transaction.
      * xaccSRGetCurrent will handle this case, too. We will create
      * a new split, copy the row contents to that split, and append
      * the split to the pre-existing transaction. */
     Split *trans_split;

     split = xaccMallocSplit (gnc_get_current_book ());
     xaccTransAppendSplit (trans, split);

     gnc_table_set_virt_cell_data (reg->table,
                                   reg->table->current_cursor_loc.vcell_loc,
                                   xaccSplitGetGUID (split));
     DEBUG("assigned cell to new split=%p", split);

     trans_split = gnc_split_register_get_current_trans_split (reg, NULL);
     if ((info->cursor_hint_trans == trans) &&
         (info->cursor_hint_trans_split == trans_split) &&
         (info->cursor_hint_split == NULL))
     {
       info->cursor_hint_split = split;
       info->cursor_hint_cursor_class = CURSOR_CLASS_SPLIT;
     }
   }

   DEBUG("updating trans=%p", trans);

   {
     SRSaveData *sd;

     sd = gnc_split_register_save_data_new (
         trans, split, (info->trans_expanded ||
                        reg->style == REG_STYLE_AUTO_LEDGER ||
                        reg->style == REG_STYLE_JOURNAL));
     gnc_table_save_cells (reg->table, sd);
     gnc_split_register_save_data_destroy (sd);
   }

   memo = xaccSplitGetMemo (split);
   memo = memo ? memo : "(null)";
   desc = xaccTransGetDescription (trans);
   desc = desc ? desc : "(null)";
   PINFO ("finished saving split \"%s\" of trans \"%s\"", memo, desc);

   /* If the modified split is the "blank split", then it is now an
    * official part of the account. Set the blank split to NULL, so we
    * can be sure of getting a new blank split. Also, save the date
    * for the new blank split. */
   if (trans == blank_trans)
   {
     if (do_commit)
     {
       info->blank_split_guid = *guid_null ();
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
     g_assert(trans == blank_trans || trans == pending_trans); 
     if (pending_trans == trans)
     {
       pending_trans = NULL;
       info->pending_trans_guid = *guid_null ();
     }
     xaccTransCommitEdit (trans);
   }

   gnc_table_clear_current_cursor_changes (reg->table);

   gnc_resume_gui_refresh ();

   LEAVE(" ");
   return TRUE;
}


Account *
gnc_split_register_get_account_by_name (SplitRegister *reg, BasicCell * bcell,
					const char *name, gboolean *refresh)
{
  const char *placeholder = _("The account %s does not allow transactions.");
  const char *missing = _("The account %s does not exist. "
			  "Would you like to create it?");
  char *account_name;
  ComboCell *cell = (ComboCell *) bcell;
  Account *account;

  if (!name || (strlen(name) == 0))
    return NULL;

  /* Find the account */
  account = gnc_account_lookup_for_register (gnc_get_current_root_account (), name);
  if (!account)
	  account = gnc_account_lookup_by_code (gnc_get_current_root_account (), name);

  if (!account) {
    /* Ask if they want to create a new one. */
    if (!gnc_verify_dialog (gnc_split_register_get_parent (reg),
			    TRUE, missing, name))
      return NULL;
    
    /* User said yes, they want to create a new account. */
    *refresh = FALSE;
    account = gnc_ui_new_accounts_from_name_window (name);
    if (!account)
      return NULL;
  }

  /* Now have the account. Update the cell with the name as created. */
  *refresh = TRUE;
  account_name = gnc_get_account_name_for_register (account);
  gnc_combo_cell_set_value (cell, account_name);
  gnc_basic_cell_set_changed (&cell->cell, TRUE);
  g_free (account_name);

  /* See if the account (either old or new) is a placeholder. */
  if (xaccAccountGetPlaceholder (account)) {
    gnc_error_dialog (gnc_split_register_get_parent (reg),
		      placeholder, name);
  }

  /* Be seeing you. */
  return account;
}

Account *
gnc_split_register_get_account (SplitRegister *reg, const char * cell_name)
{
  BasicCell *cell;
  const char *name;
  gboolean dummy;

  if (!gnc_table_layout_get_cell_changed (reg->table->layout, cell_name, TRUE))
    return NULL;

  cell = gnc_table_layout_get_cell (reg->table->layout, cell_name);
  if (!cell)
    return NULL;
  name = gnc_basic_cell_get_value (cell);
  return gnc_split_register_get_account_by_name (reg, cell, name, &dummy);
}

static gboolean
gnc_split_register_auto_calc (SplitRegister *reg, Split *split)
{
  PriceCell *cell;
  gboolean recalc_shares = FALSE;
  gboolean recalc_price = FALSE;
  gboolean recalc_value = FALSE;
  gboolean price_changed;
  gboolean amount_changed;  /* please s/amount_changed/value_changed/ */
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

  if (!xaccAccountIsPriced(account))
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


  /* Check if shares and price are BOTH zero (and value is non-zero).
   * If so, we can assume that this is an income-correcting split
   */
  if (gnc_numeric_zero_p(amount) && gnc_numeric_zero_p(price) &&
      !gnc_numeric_zero_p(value))
  {
    /* XXX: should we ask the user? */
    return TRUE;
  }

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
                            "are inconsistent. Which value would you "
                            "like to have recalculated?");

    if (shares_changed)
      radio_list = g_list_append (radio_list,
                                  g_strdup_printf ("%s (%s)",
                                                   _("_Shares"), _("Changed")));
    else
      radio_list = g_list_append (radio_list, g_strdup (_("_Shares")));

    if (price_changed)
      radio_list = g_list_append (radio_list,
                                  g_strdup_printf ("%s (%s)",
                                                   _("_Price"), _("Changed")));
    else
      radio_list = g_list_append (radio_list, g_strdup (_("_Price")));

    if (amount_changed)
      radio_list = g_list_append (radio_list,
                                  g_strdup_printf ("%s (%s)",
                                                   _("_Value"), _("Changed")));
    else
      radio_list = g_list_append (radio_list, g_strdup (_("_Value")));

    if (price_changed) default_value = 2;  /* change the value */
    else  default_value = 1;  /* change the value */

    choice = gnc_choose_radio_option_dialog
      (gnc_split_register_get_parent (reg),
       title,
       message,
       _("_Recalculate"),
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
      return ACCT_TYPE_BANK;
    case CASH_REGISTER:
      return ACCT_TYPE_CASH;
    case ASSET_REGISTER:
      return ACCT_TYPE_ASSET;
    case CREDIT_REGISTER:
      return ACCT_TYPE_CREDIT;
    case LIABILITY_REGISTER:
      return ACCT_TYPE_LIABILITY;
    case PAYABLE_REGISTER:
      return ACCT_TYPE_PAYABLE;
    case RECEIVABLE_REGISTER:
      return ACCT_TYPE_RECEIVABLE;
    case INCOME_LEDGER:  
    case INCOME_REGISTER:
      return ACCT_TYPE_INCOME;
    case EXPENSE_REGISTER:
      return ACCT_TYPE_EXPENSE;
    case STOCK_REGISTER:
    case PORTFOLIO_LEDGER:
      return ACCT_TYPE_STOCK;
    case CURRENCY_REGISTER:
      return ACCT_TYPE_CURRENCY;
    case GENERAL_LEDGER:  
      return ACCT_TYPE_NONE;
    case EQUITY_REGISTER:
      return ACCT_TYPE_EQUITY;
    case SEARCH_LEDGER:
      return ACCT_TYPE_NONE;
    default:
      return ACCT_TYPE_NONE;
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
  Transaction *pending_trans;

  ENTER("reg=%p", reg);

  if (reg == NULL)
  {
    LEAVE("no register");
    return FALSE;
  }

  if (gnc_table_current_cursor_changed (reg->table, FALSE))
  {
    LEAVE("cursor changed");
    return TRUE;
  }

  pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                   gnc_get_current_book ());
  if (xaccTransIsOpen (pending_trans))
  {
    LEAVE("open and pending txn");
    return TRUE;
  }

  LEAVE("register unchanged");
  return FALSE;
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
    case RECEIVABLE_REGISTER:
    case PAYABLE_REGISTER:
      gnc_combo_cell_add_menu_item (cell, _("Invoice"));
      gnc_combo_cell_add_menu_item (cell, _("Payment"));
      gnc_combo_cell_add_menu_item (cell, _("Interest"));
      gnc_combo_cell_add_menu_item (cell, _("Credit"));
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
      gnc_combo_cell_add_menu_item (cell, ACTION_BUY_STR);
      gnc_combo_cell_add_menu_item (cell, ACTION_SELL_STR);
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
  gnc_combo_cell_add_ignore_string
    ((ComboCell *)
     gnc_table_layout_get_cell (reg->table->layout, MXFRM_CELL),
     SPLIT_TRANS_STR);

  gnc_combo_cell_add_ignore_string
    ((ComboCell *)
     gnc_table_layout_get_cell (reg->table->layout, MXFRM_CELL),
     STOCK_SPLIT_STR);

  /* the action cell */
  gnc_combo_cell_set_autosize
    ((ComboCell *)
     gnc_table_layout_get_cell (reg->table->layout, ACTN_CELL), TRUE);

  /* Use 6 decimal places for prices and "exchange rates"  */
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

  /* Initialize the rate cell
   * use a share_print_info to make sure we don't have rounding errors
   */
  gnc_price_cell_set_print_info
    ((PriceCell *) gnc_table_layout_get_cell (reg->table->layout, RATE_CELL),
     gnc_default_share_print_info());

  /* The action cell should accept strings not in the list */
  gnc_combo_cell_set_strict
    ((ComboCell *)
     gnc_table_layout_get_cell (reg->table->layout, ACTN_CELL), FALSE);

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
      break;

    default:
      break;
  }

  /* add menu items for the action cell */
  gnc_split_register_config_action (reg);
}

static void
split_register_gconf_changed (GConfEntry *entry, gpointer user_data)
{
  SplitRegister * reg = user_data;
  SRInfo *info;

  g_return_if_fail(entry && entry->key);
  if (reg == NULL)
    return;

  info = reg->sr_info;
  if (!info)
    return;

  if (g_str_has_suffix(entry->key, KEY_ACCOUNTING_LABELS)) {
    /* Release current strings. Will be reloaded at next reference. */
    g_free (info->debit_str);
    g_free (info->tdebit_str);
    g_free (info->credit_str);
    g_free (info->tcredit_str);

    info->debit_str = NULL;
    info->tdebit_str = NULL;
    info->credit_str = NULL;
    info->tcredit_str = NULL;

  } else if (g_str_has_suffix(entry->key, KEY_ACCOUNT_SEPARATOR)) {
    info->separator_changed = TRUE;
  } else {
    g_warning("split_register_gconf_changed: Unknown gconf key %s", entry->key);
  }
}

static void 
gnc_split_register_init (SplitRegister *reg,
                         SplitRegisterType type,
                         SplitRegisterStyle style,
                         gboolean use_double_line,
                         gboolean do_auto_complete,
                         gboolean is_template)
{
  TableLayout *layout;
  TableModel *model;
  TableControl *control;

  /* Register 'destroy' callback */
  gnc_gconf_general_register_cb(KEY_ACCOUNTING_LABELS,
				split_register_gconf_changed,
				reg);
  gnc_gconf_general_register_cb(KEY_ACCOUNT_SEPARATOR,
				split_register_gconf_changed,
				reg);

  reg->sr_info = NULL;

  reg->type = type;
  reg->style = style;
  reg->use_double_line = use_double_line;
  reg->do_auto_complete = do_auto_complete;
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

    if (gnc_table_find_close_valid_cell (reg->table, &vloc, FALSE))
      gnc_table_move_cursor (reg->table, vloc);
    else
    {
      PERR ("Can't find valid initial location");
    }
  }
}

SplitRegister *
gnc_split_register_new (SplitRegisterType type,
                        SplitRegisterStyle style,
                        gboolean use_double_line,
                        gboolean is_template)
{
  SplitRegister * reg;
  gboolean default_do_auto_complete = TRUE;

  reg = g_new0 (SplitRegister, 1);

  if (type >= NUM_SINGLE_REGISTER_TYPES)
    style = REG_STYLE_JOURNAL;

  gnc_split_register_init (reg,
                           type,
                           style,
                           use_double_line,
                           default_do_auto_complete,
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

  /* If shrinking the transaction split, put the cursor on the first row of the trans */
  if (reg->use_double_line && !use_double_line) {
    VirtualLocation virt_loc = reg->table->current_cursor_loc;
    if (gnc_table_find_close_valid_cell (reg->table, &virt_loc, FALSE)) {
      if (virt_loc.phys_row_offset) {
	gnc_table_move_vertical_position (reg->table, &virt_loc, -virt_loc.phys_row_offset);
	gnc_table_move_cursor_gui (reg->table, virt_loc);
      }
    } else {
      /* WTF?  Go to a known safe location. */
      virt_loc.vcell_loc.virt_row = 1;
      virt_loc.vcell_loc.virt_col = 0;
      virt_loc.phys_row_offset = 0;
      virt_loc.phys_col_offset = 0;
      gnc_table_move_cursor_gui (reg->table, virt_loc);
    }
  }

  reg->type = newtype;

  if (reg->type >= NUM_SINGLE_REGISTER_TYPES)
    newstyle = REG_STYLE_JOURNAL;

  reg->style = newstyle;
  reg->use_double_line = use_double_line;

  gnc_table_realize_gui (reg->table);
}

void
gnc_split_register_set_auto_complete(SplitRegister *reg,
                                     gboolean do_auto_complete)
{
  g_return_if_fail(reg);
  reg->do_auto_complete = do_auto_complete;
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
   Transaction *pending_trans;
   Transaction *blank_trans = NULL;
   Split *blank_split;

   ENTER("reg=%p", reg);

   blank_split = xaccSplitLookup (&info->blank_split_guid,
                                  gnc_get_current_book ());

   pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                    gnc_get_current_book ());

   gnc_suspend_gui_refresh ();

   /* Destroy the transaction containing the "blank split", which was only
    * created to support the area for entering a new transaction. Since the
    * register is closing, this transaction is no longer needed. */
   if (blank_split != NULL)
   {
      gboolean was_open;

      blank_trans = xaccSplitGetParent (blank_split);

      DEBUG("blank_split=%p, blank_trans=%p, pending_trans=%p",
            blank_split, blank_trans, pending_trans);

      /* Destroying the transaction will automatically remove its splits. */
      was_open = xaccTransIsOpen (blank_trans);
      xaccTransDestroy (blank_trans);
      if (was_open)
        xaccTransCommitEdit (blank_trans);

      /* Update the register info. */
      if (blank_trans == pending_trans)
      {
        info->pending_trans_guid = *guid_null ();
        pending_trans = NULL;
      }
      info->blank_split_guid = *guid_null ();
      blank_split = NULL;
   }

   /* be sure to take care of any open transactions */
   if (pending_trans != NULL)
   {
      g_critical("BUG DETECTED: pending_trans=%p, blank_split=%p, blank_trans=%p",
                 pending_trans, blank_split, blank_trans);
      g_assert_not_reached();
      info->pending_trans_guid = *guid_null ();
      /* CAS: It's not clear to me that we'd really want to commit
         here, rather than rollback. But, maybe this is just dead
         code. */
      if (xaccTransIsOpen (pending_trans))
          xaccTransCommitEdit (pending_trans);
      else g_assert_not_reached();

      pending_trans = NULL;
   }

   gnc_split_register_destroy_info (reg);

   gnc_resume_gui_refresh ();

   LEAVE(" ");
}

void
gnc_split_register_destroy (SplitRegister *reg)
{
  g_return_if_fail(reg);

  ENTER("reg=%p", reg);

  gnc_gconf_general_remove_cb(KEY_ACCOUNTING_LABELS,
			      split_register_gconf_changed,
			      reg);
  gnc_gconf_general_remove_cb(KEY_ACCOUNT_SEPARATOR,
			      split_register_gconf_changed,
			      reg);
  gnc_split_register_cleanup (reg);

  gnc_table_destroy (reg->table);
  reg->table = NULL;

  /* free the memory itself */
  g_free (reg);
  LEAVE(" ");
}

void 
gnc_split_register_set_read_only (SplitRegister *reg, gboolean read_only)
{
  gnc_table_model_set_read_only (reg->table->model, read_only);
}
