/********************************************************************\
 * split-register-load.c -- split register loading code             *
 * Copyright (C) 1998-2000 Linas Vepstas <linas@linas.org>          *
 * Copyright (C) 2000 Dave Peticolas                                *
 *                                                                  *
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

#include "config.h"

#include <glib/gi18n.h>

#include "account-quickfill.h"
#include "combocell.h"
#include "gnc-component-manager.h"
#include "qof.h"
#include "gnc-ui-util.h"
#include "gnc-gui-query.h"
#include "numcell.h"
#include "quickfillcell.h"
#include "recncell.h"
#include "split-register.h"
#include "split-register-p.h"


/* This static indicates the debugging module that this .o belongs to. */
/* static short module = MOD_LEDGER; */

static void gnc_split_register_load_xfer_cells (SplitRegister *reg,
						Account *base_account);

static void
gnc_split_register_load_recn_cells (SplitRegister *reg)
{
  RecnCell *cell;
  const char * s;

  if (!reg) return;

  cell = (RecnCell *)
    gnc_table_layout_get_cell (reg->table->layout, RECN_CELL);

  if (!cell) return;

  s = gnc_get_reconcile_valid_flags ();
  gnc_recn_cell_set_valid_flags (cell, s, *s);
  gnc_recn_cell_set_flag_order (cell, gnc_get_reconcile_flag_order ());
  gnc_recn_cell_set_string_getter (cell, gnc_get_reconcile_str);
}

static void
gnc_split_register_load_type_cells (SplitRegister *reg)
{
  RecnCell *cell;

  if (!reg) return;

  cell = (RecnCell *)
    gnc_table_layout_get_cell (reg->table->layout, TYPE_CELL);

  if (!cell) return;

  /* FIXME: These should get moved to an i18n function */
  gnc_recn_cell_set_valid_flags (cell, "IP?", 'I');
  gnc_recn_cell_set_flag_order (cell, "IP");
}

static void
gnc_split_register_add_transaction (SplitRegister *reg,
                                    Transaction *trans,
                                    Split *split,
                                    CellBlock *lead_cursor,
                                    CellBlock *split_cursor,
                                    gboolean visible_splits,
                                    gboolean start_primary_color,
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

  for (node = xaccTransGetSplitList (trans); node; node = node->next) {
      Split *secondary = node->data;

      if (!xaccTransStillHasSplit(trans, secondary)) continue;
      if (secondary == find_split && find_class == CURSOR_CLASS_SPLIT)
        *new_split_row = vcell_loc->virt_row;

      gnc_table_set_vcell (reg->table, split_cursor,
                           xaccSplitGetGUID (secondary),
                           visible_splits, TRUE, *vcell_loc);
      vcell_loc->virt_row++;
  }

  if (add_blank) {
      
      if (find_trans == trans && find_split == NULL &&
          find_class == CURSOR_CLASS_SPLIT) {
          *new_split_row = vcell_loc->virt_row;
      }
      
      /* Add blank transaction split */
      gnc_table_set_vcell(reg->table, split_cursor, xaccSplitGetGUID(NULL), 
                          FALSE, TRUE, *vcell_loc);
      vcell_loc->virt_row++;
  }
}

static gint
_find_split_with_parent_txn(gconstpointer a, gconstpointer b)
{
  Split *split = (Split*)a;
  Transaction *txn = (Transaction*)b;
  return xaccSplitGetParent(split) == txn ? 0 : 1;
}

static void add_quickfill_completions(TableLayout *layout, Transaction *trans, 
                                      gboolean has_last_num)
{
    Split *s;
    int i = 0;      

    gnc_quickfill_cell_add_completion(
        (QuickFillCell *) gnc_table_layout_get_cell(layout, DESC_CELL),
         xaccTransGetDescription(trans));
    
    gnc_quickfill_cell_add_completion(
        (QuickFillCell *) gnc_table_layout_get_cell(layout, NOTES_CELL),
        xaccTransGetNotes(trans));

    if (!has_last_num)
        gnc_num_cell_set_last_num(
            (NumCell *) gnc_table_layout_get_cell(layout, NUM_CELL),
            xaccTransGetNum(trans));
    
    while ((s = xaccTransGetSplit(trans, i)) != NULL) {
        gnc_quickfill_cell_add_completion(
            (QuickFillCell *) gnc_table_layout_get_cell(layout, MEMO_CELL), 
            xaccSplitGetMemo(s));
        i++;
    }
}

void
gnc_split_register_load (SplitRegister *reg, GList * slist,
                         Account *default_account)
{
  SRInfo *info = gnc_split_register_get_info (reg);
  Transaction *pending_trans;
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
  Split *blank_split;
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
  gboolean we_own_slist = FALSE;

  VirtualCellLocation vcell_loc;
  VirtualLocation save_loc;

  int new_trans_split_row = -1;
  int new_trans_row = -1;
  int new_split_row = -1;
  time_t present;

  blank_split = xaccSplitLookup (&info->blank_split_guid,
                                 gnc_get_current_book ());

  pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                   gnc_get_current_book ());

  /* make sure we have a blank split */
  if (blank_split == NULL)
  {
    Transaction *new_trans;
    gnc_commodity * currency = NULL;

    /* Determine the proper currency to use for this transaction.
     * if default_account != NULL and default_account->commodity is
     * a currency, then use that.  Otherwise use the default currency.
     */
    if (default_account != NULL) {
      gnc_commodity * commodity = xaccAccountGetCommodity (default_account);
      if (gnc_commodity_is_currency(commodity))
	currency = commodity;
      else {
	Account *parent_account = default_account;
	/* Account commodity is not a currency, walk up the tree until
	 * we find a parent account that is a currency account and use
	 * it's currency.
	 */
	do {
	  parent_account = gnc_account_get_parent ( parent_account );
	  if (parent_account)
	  {
	    commodity = xaccAccountGetCommodity (parent_account);
	    if (gnc_commodity_is_currency(commodity))
	    {
	      currency = commodity;
	      break;
	    }
	  }
	} while (parent_account && !currency);
      }

      /* If we don't have a currency then pop up a warning dialog */
      if (!currency)
      {
	gnc_info_dialog(NULL, "%s",
			_("Could not determine the account currency for this register.  "
			  "Using the currency obtained from the system's locale setting instead."));
      }
    }

    gnc_suspend_gui_refresh ();

    new_trans = xaccMallocTransaction (gnc_get_current_book ());

    xaccTransBeginEdit (new_trans);
    xaccTransSetCurrency (new_trans, 
                          currency ? currency : gnc_default_currency());
    xaccTransSetDateSecs (new_trans, info->last_date_entered);
    blank_split = xaccMallocSplit (gnc_get_current_book ());
    xaccSplitSetParent(blank_split, new_trans);
    /* We don't want to commit this transaction yet, because the split
       doesn't even belong to an account yet.  But, we don't want to
       set this transaction as the pending transaction either, because
       we want to pretend that it hasn't been changed.  We depend on
       some other code (somewhere) to commit this transaction if we
       really edit it, even though it's not marked as the pending
       transaction. */

    /* Wouldn't it be a bug to open this transaction if there was already a
       pending transaction? */
    g_assert(pending_trans == NULL);

    info->blank_split_guid = *xaccSplitGetGUID (blank_split);
    info->blank_split_edited = FALSE;

    gnc_resume_gui_refresh ();
  }

  blank_trans = xaccSplitGetParent (blank_split);

  info->default_account = *xaccAccountGetGUID (default_account);

  table = reg->table;

  // gnc_table_leave_update (table, table->current_cursor_loc);

  multi_line = (reg->style == REG_STYLE_JOURNAL);
  dynamic    = (reg->style == REG_STYLE_AUTO_LEDGER);

  lead_cursor = gnc_split_register_get_passive_cursor (reg);
  split_cursor = gnc_table_layout_get_cursor (table->layout, CURSOR_SPLIT);

  /* figure out where we are going to. */
  if (info->traverse_to_new)
  {
    find_trans = blank_trans;
    find_split = NULL;
    find_trans_split = blank_split;
  }
  else
  {
    find_trans = info->cursor_hint_trans;
    find_split = info->cursor_hint_split;
    find_trans_split = info->cursor_hint_trans_split;
  }
  find_class = info->cursor_hint_cursor_class;

  save_loc = table->current_cursor_loc;

  /* If the current cursor has changed we save the values for later
   * possible restoration. */
  if (gnc_table_current_cursor_changed (table, TRUE) &&
      (find_split == gnc_split_register_get_current_split (reg)))
  {
    cursor_buffer = gnc_cursor_buffer_new ();
    gnc_table_save_current_cursor (table, cursor_buffer);
  }
  else
    cursor_buffer = NULL;

  /* disable move callback -- we don't want the cascade of 
   * callbacks while we are fiddling with loading the register */
  gnc_table_control_allow_move (table->control, FALSE);

  /* invalidate the cursor */
  {
    VirtualLocation virt_loc;

    gnc_virtual_location_init(&virt_loc);
    gnc_table_move_cursor_gui (table, virt_loc);
  }

  /* make sure that the header is loaded */
  vcell_loc.virt_row = 0;
  vcell_loc.virt_col = 0;
  cursor_header = gnc_table_layout_get_cursor (table->layout, CURSOR_HEADER);
  gnc_table_set_vcell (table, cursor_header, NULL, TRUE, TRUE, vcell_loc);
  vcell_loc.virt_row++;

  /* get the current time and reset the dividing row */
  present = gnc_timet_get_today_end ();

  if (info->first_pass)
  {
    if (default_account)
    {
      const char *last_num = xaccAccountGetLastNum (default_account);

      if (last_num)
      {
        NumCell *cell;

        cell = (NumCell *) gnc_table_layout_get_cell(table->layout, NUM_CELL);
        gnc_num_cell_set_last_num (cell, last_num);
        has_last_num = TRUE;
      }
    }

    /* set the completion character for the xfer cells */
    gnc_combo_cell_set_complete_char(
        (ComboCell *) gnc_table_layout_get_cell(table->layout, MXFRM_CELL),
        gnc_get_account_separator());
  
    gnc_combo_cell_set_complete_char(
        (ComboCell *) gnc_table_layout_get_cell(table->layout, XFRM_CELL),
        gnc_get_account_separator());
  
    /* set the confirmation callback for the reconcile cell */
    gnc_recn_cell_set_confirm_cb(
        (RecnCell *) gnc_table_layout_get_cell(table->layout, RECN_CELL),
        gnc_split_register_recn_cell_confirm, reg);
  
    /* load up account names into the transfer combobox menus */
    gnc_split_register_load_xfer_cells (reg, default_account);
    gnc_split_register_load_recn_cells (reg);
    gnc_split_register_load_type_cells (reg);
  }

  table->model->dividing_row = -1;

  // Ensure that the transaction and splits being edited are in the split
  // list we're about to load.
  if (pending_trans != NULL)
  {
    for (node = xaccTransGetSplitList(pending_trans); node; node = node->next)
    {
      Split *pending_split = (Split*)node->data;
      if (!xaccTransStillHasSplit(pending_trans, pending_split)) continue;
      if (g_list_find(slist, pending_split) != NULL)
        continue;

      if (g_list_find_custom(slist, pending_trans, 
                             _find_split_with_parent_txn) != NULL)
        continue;

      if (!we_own_slist)
      { // lazy-copy
        slist = g_list_copy(slist);
        we_own_slist = TRUE;
      }
      slist = g_list_append(slist, pending_split);
    }
  }

  if (multi_line)
    trans_table = g_hash_table_new (g_direct_hash, g_direct_equal);

  /* populate the table */
  for (node = slist; node; node = node->next) 
  {
    split = node->data;
    trans = xaccSplitGetParent (split);

    if (!xaccTransStillHasSplit(trans, split))
        continue;

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
      table->model->dividing_row = vcell_loc.virt_row;
      found_divider = TRUE;
    }

    /* If this is the first load of the register,
     * fill up the quickfill cells. */
    if (info->first_pass)
        add_quickfill_completions(reg->table->layout, trans, has_last_num);

    if (trans == find_trans)
      new_trans_row = vcell_loc.virt_row;

    if (split == find_trans_split)
      new_trans_split_row = vcell_loc.virt_row;

    gnc_split_register_add_transaction (reg, trans, split,
                                        lead_cursor, split_cursor,
                                        multi_line, start_primary_color,
                                        TRUE,
                                        find_trans, find_split, find_class,
                                        &new_split_row, &vcell_loc);

    if (!multi_line)
      start_primary_color = !start_primary_color;
  }

  if (multi_line)
    g_hash_table_destroy (trans_table);

  /* add the blank split at the end. */
  if (pending_trans == blank_trans)
    found_pending = TRUE;

  if (blank_trans == find_trans)
    new_trans_row = vcell_loc.virt_row;

  if (blank_split == find_trans_split)
    new_trans_split_row = vcell_loc.virt_row;

  /* If we didn't find the pending transaction, it was removed
   * from the account. */
  if (!found_pending)
  {
      info->pending_trans_guid = *guid_null ();
      if (xaccTransIsOpen (pending_trans))
          xaccTransCommitEdit (pending_trans);
      else if (pending_trans) 
          g_assert_not_reached();

      pending_trans = NULL;
  }

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

  gnc_split_register_add_transaction (reg, blank_trans, blank_split,
                                      lead_cursor, split_cursor,
                                      multi_line, start_primary_color,
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

    trans_split =
      gnc_split_register_get_trans_split (reg, save_loc.vcell_loc,
                                          &trans_split_loc.vcell_loc);

    if (dynamic || multi_line || info->trans_expanded)
    {
      gnc_table_set_virt_cell_cursor(
          table, trans_split_loc.vcell_loc,
          gnc_split_register_get_active_cursor (reg));
      gnc_split_register_set_trans_visible (reg, trans_split_loc.vcell_loc,
                                            TRUE, multi_line);

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

      if (find_split == gnc_split_register_get_current_split (reg))
        gnc_table_restore_current_cursor (table, cursor_buffer);
    }

    gnc_cursor_buffer_destroy (cursor_buffer);
    cursor_buffer = NULL;
  }

  /* Set up the hint transaction, split, transaction split, and column. */
  info->cursor_hint_trans = gnc_split_register_get_current_trans (reg);
  info->cursor_hint_split = gnc_split_register_get_current_split (reg);
  info->cursor_hint_trans_split =
    gnc_split_register_get_current_trans_split (reg, NULL);
  info->cursor_hint_cursor_class =
    gnc_split_register_get_current_cursor_class (reg);
  info->hint_set_by_traverse = FALSE;
  info->traverse_to_new = FALSE;
  info->exact_traversal = FALSE;
  info->first_pass = FALSE;
  info->reg_loaded = TRUE;

  gnc_split_register_set_cell_fractions(
      reg, gnc_split_register_get_current_split (reg));

  gnc_table_refresh_gui (table, TRUE);

  gnc_split_register_show_trans (reg, table->current_cursor_loc.vcell_loc);

  /* enable callback for cursor user-driven moves */
  gnc_table_control_allow_move (table->control, TRUE);

  if (we_own_slist)
    g_list_free(slist);
}

/* ===================================================================== */

#define QKEY  "split_reg_shared_quickfill"

static gboolean 
skip_cb (Account *account, gpointer x)
{
  /* commented out as per Bug#340885 Comments 1 and 2, option (2).
  if (xaccAccountIsHidden(account))
    return TRUE;
  */
  return xaccAccountGetPlaceholder (account);
}

static void
gnc_split_register_load_xfer_cells (SplitRegister *reg, Account *base_account)
{
  Account *root = NULL;
  QuickFill *qf;
  ComboCell *cell;
  GtkListStore *store;

  if (base_account)
    root = gnc_account_get_root(base_account);
  if (root == NULL)
    root = gnc_get_current_root_account();
  if (root == NULL)
    return;

  qf = gnc_get_shared_account_name_quickfill (root, QKEY, skip_cb, NULL);
  store = gnc_get_shared_account_name_list_store (root, QKEY, skip_cb, NULL);

  cell = (ComboCell *)
    gnc_table_layout_get_cell (reg->table->layout, XFRM_CELL);
  gnc_combo_cell_use_quickfill_cache (cell, qf);
  gnc_combo_cell_use_list_store_cache (cell, store);

  cell = (ComboCell *)
    gnc_table_layout_get_cell (reg->table->layout, MXFRM_CELL);
  gnc_combo_cell_use_quickfill_cache (cell, qf);
  gnc_combo_cell_use_list_store_cache (cell, store);
}

/* ====================== END OF FILE ================================== */
