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
#include "engine-helpers.h"
#include "gnc-prefs.h"


/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_LEDGER;


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

/** Add a transaction to the register.
 *
 *  Virtual cells are set up to hold the data, beginning at @a vcell_loc and
 *  continuing downward in the same virtual column. The first virtual cell
 *  will be a "leading" cell, followed by a virtual cell for each split of
 *  transaction @a trans. For example, the "transaction journal" register
 *  style keeps all the transaction-level data and totals in the leading
 *  cell, and all split-level data in the cells that follow.
 *
 *  Each of these cells will remember the GncGUID of the ::Split to which it
 *  is associated. The leading virtual cell will be assigned the GncGUID of
 *  the "anchoring split" specified by @a split.
 *
 *  Optionally an extra, empty virtual cell will be assigned to no split.
 *  This should not be confused with the "blank split", because this cell is
 *  not tied to any split at all, not even the "blank split". A null GncGUID
 *  is assigned to it.
 *
 *  The caller can find out which virtual row was used for a particular virtual
 *  cell by using parameters @a find_trans, @a find_split, and @a find_class.
 *  If a match is found, the row is returned in @a new_split_row. Otherwise,
 *  @a new_split_row is not changed.
 *  - To find the virtual cell for a particular split, set @a find_split to
 *    the target ::Split and @a find_class to ::CURSOR_CLASS_SPLIT.
 *  - To find the empty row, set @a find_trans equal to @a trans, @a find_split
 *    to @c NULL, and @a find_class to ::CURSOR_CLASS_SPLIT.
 *  - The leading virtual cell is always placed in the row specified by
 *    @a vcell_loc, but this will not be returned in @a new_split_row unless
 *    @a find_split is set to the anchoring split and @a find_class is not
 *    ::CURSOR_CLASS_SPLIT.
 *
 *  @param reg a ::SplitRegister
 *
 *  @param trans the transaction to add to the register
 *
 *  @param split a ::Split to use as the "anchoring split" in the "leading"
 *  virtual cell
 *
 *  @param lead_cursor the cursor to use for the "leading" virtual cell
 *
 *  @param split_cursor the cursor to use in the split rows that follow
 *
 *  @param visible_splits @c TRUE to make the split rows visible, @c FALSE
 *  otherwise
 *
 *  @param start_primary_color @c TRUE to use the primary color for the
 *  "leading" row, @c FALSE otherwise
 *
 *  @param add_empty @c TRUE if an empty row should be added, @c FALSE
 *  otherwise
 *
 *  @param find_trans the transaction parameter for row searching
 *
 *  @param find_split the split parameter for row searching
 *
 *  @param find_class the cursor class parameter for row searching
 *
 *  @param new_split_row a pointer to be filled with the matching virtual row.
 *
 *  @param vcell_loc the location to begin setting virtual cells. The row
 *  will be changed to the row below the last row filled.
 */
static void
gnc_split_register_add_transaction (SplitRegister *reg,
                                    Transaction *trans,
                                    Split *split,
                                    CellBlock *lead_cursor,
                                    CellBlock *split_cursor,
                                    gboolean visible_splits,
                                    gboolean start_primary_color,
                                    gboolean add_empty,
                                    Transaction *find_trans,
                                    Split *find_split,
                                    CursorClass find_class,
                                    int *new_split_row,
                                    VirtualCellLocation *vcell_loc)
{
    GList *node;

    g_return_if_fail(reg);
    g_return_if_fail(vcell_loc);

    if (split == find_split)
        *new_split_row = vcell_loc->virt_row;

    /* Set the "leading" virtual cell. */
    gnc_table_set_vcell (reg->table, lead_cursor, xaccSplitGetGUID (split),
                         TRUE, start_primary_color, *vcell_loc);
    vcell_loc->virt_row++;

    /* Continue setting up virtual cells in a column, using a row for each
     * split in the transaction. */
    for (node = xaccTransGetSplitList (trans); node; node = node->next)
    {
        Split *secondary = node->data;

        if (!xaccTransStillHasSplit(trans, secondary)) continue;
        if (secondary == find_split && find_class == CURSOR_CLASS_SPLIT)
            *new_split_row = vcell_loc->virt_row;

        gnc_table_set_vcell (reg->table, split_cursor,
                             xaccSplitGetGUID (secondary),
                             visible_splits, TRUE, *vcell_loc);
        vcell_loc->virt_row++;
    }

    /* If requested, add an empty split row at the end. */
    if (add_empty)
    {
        if (find_trans == trans && find_split == NULL &&
                find_class == CURSOR_CLASS_SPLIT)
            *new_split_row = vcell_loc->virt_row;

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
                                      Split *split, gboolean has_last_num)
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
            gnc_get_num_action(trans, split));

    while ((s = xaccTransGetSplit(trans, i)) != NULL)
    {
        gnc_quickfill_cell_add_completion(
            (QuickFillCell *) gnc_table_layout_get_cell(layout, MEMO_CELL),
            xaccSplitGetMemo(s));
        i++;
    }
}

static Split*
create_blank_split (Account *default_account, SRInfo *info)
{
    Transaction *new_trans;
    gboolean currency_from_account = TRUE;
    Split *blank_split = NULL;
    /* Determine the proper currency to use for this transaction.
     * if default_account != NULL and default_account->commodity is
     * a currency, then use that.  Otherwise use the default currency.
     */
    gnc_commodity * currency = gnc_account_or_default_currency(default_account, &currency_from_account);

    if (default_account != NULL && !currency_from_account)
    {
	/* If we don't have a currency then pop up a warning dialog */
	gnc_info_dialog(NULL, "%s",
			_("Could not determine the account currency. "
			  "Using the default currency provided by your system."));
    }

    gnc_suspend_gui_refresh ();

    new_trans = xaccMallocTransaction (gnc_get_current_book ());

    xaccTransBeginEdit (new_trans);
    xaccTransSetCurrency (new_trans, currency);
    xaccTransSetDatePostedSecsNormalized(new_trans, info->last_date_entered);
    blank_split = xaccMallocSplit (gnc_get_current_book ());
    xaccSplitSetParent(blank_split, new_trans);
    /* We don't want to commit this transaction yet, because the split
       doesn't even belong to an account yet.  But, we don't want to
       set this transaction as the pending transaction either, because
       we want to pretend that it hasn't been changed.  We depend on
       some other code (somewhere) to commit this transaction if we
       really edit it, even though it's not marked as the pending
       transaction. */

    info->blank_split_guid = *xaccSplitGetGUID (blank_split);
    info->blank_split_edited = FALSE;
    info->auto_complete = FALSE;
    DEBUG("created new blank_split=%p", blank_split);

    gnc_resume_gui_refresh ();
    return blank_split;
}

static void
change_account_separator (SRInfo *info, Table *table, SplitRegister *reg)
{
    info->separator_changed = FALSE;

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
}

static void
update_info (SRInfo *info, SplitRegister *reg)
{
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
}

void
gnc_split_register_load (SplitRegister *reg, GList * slist,
                         Account *default_account)
{
    SRInfo *info;
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
    gboolean need_divider_upper = FALSE;
    gboolean found_divider_upper = FALSE;
    gboolean found_divider = FALSE;
    gboolean has_last_num = FALSE;
    gboolean multi_line;
    gboolean dynamic;
    gboolean we_own_slist = FALSE;
    gboolean use_autoreadonly = qof_book_uses_autoreadonly(gnc_get_current_book());
    gboolean future_after_blank = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL_REGISTER,
                                                     GNC_PREF_FUTURE_AFTER_BLANK);
    gboolean added_blank_trans = FALSE;

    VirtualCellLocation vcell_loc;
    VirtualLocation save_loc;

    int new_trans_split_row = -1;
    int new_trans_row = -1;
    int new_split_row = -1;
    time64 present, autoreadonly_time = 0;

    g_return_if_fail(reg);
    table = reg->table;
    g_return_if_fail(table);
    info = gnc_split_register_get_info (reg);
    g_return_if_fail(info);

    ENTER("reg=%p, slist=%p, default_account=%p", reg, slist, default_account);

    blank_split = xaccSplitLookup (&info->blank_split_guid,
                                   gnc_get_current_book ());

    pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                     gnc_get_current_book ());

    /* make sure we have a blank split */
    if (blank_split == NULL)
    {
	/* Wouldn't it be a bug to open the new transaction if there was
	 * already a pending transaction?
	*/
	g_assert(pending_trans == NULL);
	blank_split = create_blank_split (default_account, info);
    }
    blank_trans = xaccSplitGetParent (blank_split);

    DEBUG("blank_split=%p, blank_trans=%p, pending_trans=%p",
          blank_split, blank_trans, pending_trans);

    info->default_account = *xaccAccountGetGUID (default_account);

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
        find_class = CURSOR_CLASS_SPLIT;
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
    present = gnc_time64_get_today_end ();
    if (use_autoreadonly)
    {
        GDate *d = qof_book_get_autoreadonly_gdate(gnc_get_current_book());
        // "d" is NULL if use_autoreadonly is FALSE
        autoreadonly_time = d ? timespecToTime64(gdate_to_timespec(*d)) : 0;
        g_date_free(d);
    }

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

        /* load up account names into the transfer combobox menus */
        gnc_split_register_load_xfer_cells (reg, default_account);
        gnc_split_register_load_recn_cells (reg);
        gnc_split_register_load_type_cells (reg);
    }

    if (info->separator_changed)
	change_account_separator (info, table, reg);

    table->model->dividing_row_upper = -1;
    table->model->dividing_row = -1;
    table->model->dividing_row_lower = -1;

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
            {
                // lazy-copy
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
	/* If the transaction has only one split, and it's not our
	 * pending_trans, then it's another register's blank split and
	 * we don't want to see it.
	 */
	else if (xaccTransCountSplits (trans) == 1 &&
		 xaccSplitGetAccount (split) == NULL)
	    continue;


        /* Do not load splits from the blank transaction. */
        if (trans == blank_trans)
            continue;

        if (multi_line)
        {
            /* Skip this split if its transaction has already been loaded. */
            if (g_hash_table_lookup (trans_table, trans))
                continue;

            g_hash_table_insert (trans_table, trans, trans);
        }

        if (info->show_present_divider &&
                use_autoreadonly &&
                !found_divider_upper)
        {
            if (xaccTransGetDate (trans) >= autoreadonly_time)
            {
                table->model->dividing_row_upper = vcell_loc.virt_row;
                found_divider_upper = TRUE;
            }
            else
            {
                need_divider_upper = TRUE;
            }
        }

        if (info->show_present_divider &&
                !found_divider &&
                (xaccTransGetDate (trans) > present))
        {
            table->model->dividing_row = vcell_loc.virt_row;
            found_divider = TRUE;

            if (future_after_blank)
            {
                if (blank_trans == find_trans)
                    new_trans_row = vcell_loc.virt_row;

                if (blank_split == find_trans_split)
                    new_trans_split_row = vcell_loc.virt_row;

                /* go to blank on first pass */
                if (info->first_pass)
                {
                    save_loc.vcell_loc = vcell_loc;
                    save_loc.phys_row_offset = 0;
                    save_loc.phys_col_offset = 0;
                }

                gnc_split_register_add_transaction (reg,
                                            blank_trans, blank_split,
                                            lead_cursor, split_cursor,
                                            multi_line, start_primary_color,
                                            info->blank_split_edited,
                                            find_trans, find_split,
                                            find_class, &new_split_row,
                                            &vcell_loc);

                table->model->dividing_row_lower = vcell_loc.virt_row;

                if (!multi_line)
                    start_primary_color = !start_primary_color;

                added_blank_trans = TRUE;
            }
        }

        /* If this is the first load of the register,
         * fill up the quickfill cells. */
        if (info->first_pass)
            add_quickfill_completions(reg->table->layout, trans, split, has_last_num);

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

    /* No upper divider yet? Store it now */
    if (info->show_present_divider &&
            use_autoreadonly &&
            !found_divider_upper && need_divider_upper)
    {
        table->model->dividing_row_upper = vcell_loc.virt_row;
        found_divider_upper = TRUE;
    }

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

    if (!added_blank_trans) {
        if (blank_trans == find_trans)
            new_trans_row = vcell_loc.virt_row;

        if (blank_split == find_trans_split)
            new_trans_split_row = vcell_loc.virt_row;

        /* go to blank on first pass */
        if (info->first_pass)
        {
            save_loc.vcell_loc = vcell_loc;
            save_loc.phys_row_offset = 0;
            save_loc.phys_col_offset = 0;
        }

        gnc_split_register_add_transaction (reg, blank_trans, blank_split,
                                            lead_cursor, split_cursor,
                                            multi_line, start_primary_color,
                                            info->blank_split_edited,
                                            find_trans, find_split,
                                            find_class, &new_split_row,
                                            &vcell_loc);

        if (future_after_blank)
            table->model->dividing_row_lower = vcell_loc.virt_row;
    }

    /* go to blank on first pass */
    if (info->first_pass)
    {
        new_split_row = -1;
        new_trans_split_row = -1;
        new_trans_row = -1;
    }

    /* resize the table to the sizes we just counted above */
    /* num_virt_cols is always one. */
    gnc_table_set_size (table, vcell_loc.virt_row, 1);

    /* restore the cursor to its rightful position */
    {
        VirtualLocation trans_split_loc;

        if (new_split_row > 0)
            save_loc.vcell_loc.virt_row = new_split_row;
        else if (new_trans_split_row > 0)
            save_loc.vcell_loc.virt_row = new_trans_split_row;
        else if (new_trans_row > 0)
            save_loc.vcell_loc.virt_row = new_trans_row;

        trans_split_loc = save_loc;

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
    }
    gnc_cursor_buffer_destroy (cursor_buffer);
    cursor_buffer = NULL;

    update_info (info, reg);

    gnc_split_register_set_cell_fractions(
        reg, gnc_split_register_get_current_split (reg));

    gnc_table_refresh_gui (table, TRUE);

    gnc_split_register_show_trans (reg, table->current_cursor_loc.vcell_loc);

    /* enable callback for cursor user-driven moves */
    gnc_table_control_allow_move (table->control, TRUE);

    if (we_own_slist)
        g_list_free(slist);

    LEAVE(" ");
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
