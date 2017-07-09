/********************************************************************\
 * split-register-control.c -- split register control object        *
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

#include <glib.h>
#include <glib/gi18n.h>

#include "Scrub.h"
#include "combocell.h"
#include "gnc-component-manager.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnome-utils/gnc-warnings.h"
#include "pricecell.h"
#include "datecell.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "split-register-control.h"
#include "split-register-model-save.h"
#include "split-register-p.h"
#include "table-allgui.h"
#include "engine-helpers.h"


/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_LEDGER;


static gboolean
gnc_split_register_balance_trans (SplitRegister *reg, Transaction *trans)
{
    int choice;
    int default_value;
    Account *default_account;
    Account *other_account;
    Account *root;
    GList *radio_list = NULL;
    const char *title   = _("Rebalance Transaction");
    const char *message = _("The current transaction is not balanced.");
    Split *split;
    Split *other_split;
    gboolean two_accounts;
    gboolean multi_currency;


    if (xaccTransIsBalanced (trans))
        return FALSE;

    if (xaccTransUseTradingAccounts (trans))
    {
        MonetaryList *imbal_list;
        gnc_monetary *imbal_mon;
        imbal_list = xaccTransGetImbalance (trans);

        /* See if the imbalance is only in the transaction's currency */
        if (!imbal_list)
            /* Value imbalance, but not commodity imbalance.  This shouldn't
               be something that scrubbing can cause to happen.  Perhaps someone
               entered invalid splits.  */
            multi_currency = TRUE;
        else
        {
            imbal_mon = imbal_list->data;
            if (!imbal_list->next &&
                    gnc_commodity_equiv(gnc_monetary_commodity(*imbal_mon),
                                        xaccTransGetCurrency(trans)))
                multi_currency = FALSE;
            else
                multi_currency = TRUE;
        }

        /* We're done with the imbalance list, the real work will be done
           by xaccTransScrubImbalance which will get it again. */
        gnc_monetary_list_free(imbal_list);
    }
    else
        multi_currency = FALSE;

    split = xaccTransGetSplit (trans, 0);
    other_split = xaccSplitGetOtherSplit (split);

    if (other_split == NULL)
    {
        /* Attempt to handle the inverted many-to-one mapping */
        split = xaccTransGetSplit (trans, 1);
        if (split) other_split = xaccSplitGetOtherSplit (split);
        else split = xaccTransGetSplit (trans, 0);
    }
    if (other_split == NULL || multi_currency)
    {
        two_accounts = FALSE;
        other_account = NULL;
    }
    else
    {
        two_accounts = TRUE;
        other_account = xaccSplitGetAccount (other_split);
    }

    default_account = gnc_split_register_get_default_account (reg);

    /* If the two pointers are the same, the account from other_split
     * is actually the default account. We must make other_account
     * the account from split instead.   */

    if (default_account == other_account)
        other_account = xaccSplitGetAccount (split);

    /*  If the two pointers are still the same, we have two splits, but
     *  they both refer to the same account. While non-sensical, we don't
     *  object.   */

    if (default_account == other_account)
        two_accounts = FALSE;

    radio_list = g_list_append (radio_list,
                                _("Balance it _manually"));
    radio_list = g_list_append (radio_list,
                                _("Let GnuCash _add an adjusting split"));

    if (reg->type < NUM_SINGLE_REGISTER_TYPES && !multi_currency)
    {
        radio_list = g_list_append (radio_list,
                                    _("Adjust current account _split total"));

        default_value = 2;
        if (two_accounts)
        {
            radio_list = g_list_append (radio_list,
                                        _("Adjust _other account split total"));
            default_value = 3;
        }
    }
    else
        default_value = 0;

    choice = gnc_choose_radio_option_dialog
             (gnc_split_register_get_parent (reg),
              title,
              message,
              _("_Rebalance"),
              default_value,
              radio_list);

    g_list_free (radio_list);

    root = default_account ? gnc_account_get_root(default_account) : NULL;
    switch (choice)
    {
    default:
    case 0:
        break;

    case 1:
        xaccTransScrubImbalance (trans, root, NULL);
        break;

    case 2:
        xaccTransScrubImbalance (trans, root, default_account);
        break;

    case 3:
        xaccTransScrubImbalance (trans, root, other_account);
        break;
    }

    return TRUE;
}

static gboolean
gnc_split_register_old_split_empty_p (SplitRegister *reg, Split *split)
{
    BasicCell *cell;
    gnc_numeric amount;
    const char *string;

    string = gnc_table_layout_get_cell_value (reg->table->layout, MEMO_CELL);
    if ((string != NULL) && (*string != '\0'))
        return FALSE;

    string = gnc_table_layout_get_cell_value (reg->table->layout, XFRM_CELL);
    if ((string != NULL) && (*string != '\0'))
        return FALSE;

    cell = gnc_table_layout_get_cell (reg->table->layout, CRED_CELL);
    if (cell)
    {
        amount = gnc_price_cell_get_value ((PriceCell *) cell);
        if (!gnc_numeric_zero_p (amount))
            return FALSE;
    }

    cell = gnc_table_layout_get_cell (reg->table->layout, DEBT_CELL);
    if (cell)
    {
        amount = gnc_price_cell_get_value ((PriceCell *) cell);
        if (!gnc_numeric_zero_p (amount))
            return FALSE;
    }

    return TRUE;
}

/* Checks a cell for a debit or credit change to see if a new exchange
 * rate is needed. */

static gboolean
gnc_split_register_check_debcred (SplitRegister *reg,
                                  const char *cell_name)
{
    if ((gnc_cell_name_equal (cell_name, DEBT_CELL) &&
            gnc_table_layout_get_cell_changed (reg->table->layout,
                                               DEBT_CELL, FALSE)) ||
         (gnc_cell_name_equal (cell_name, CRED_CELL) &&
            gnc_table_layout_get_cell_changed (reg->table->layout,
                                               CRED_CELL, FALSE)))
    {
        SRInfo *info = gnc_split_register_get_info (reg);
        PriceCell *rate_cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                                RATE_CELL);
        if (gnc_split_reg_has_rate_cell(reg->type) && info->rate_reset != RATE_RESET_DONE)
        {
            /* Debit or credit amount changed, get a new exchange rate */
            info->rate_reset = RATE_RESET_REQD;
            if (info->auto_complete)
            {
                /* It's auto-filled, start with rate from price DB for the date
                   of the transaction. */
                gnc_price_cell_set_value (rate_cell, gnc_numeric_zero());
            }
        }
    }

    return TRUE;
}

/* Checks a cell for an account change and takes any necessary action if
 * one has occurred. Returns TRUE if the check passes, FALSE if it fails. */
static gboolean
gnc_split_register_check_account (SplitRegister *reg,
                                  const char *cell_name)
{
    SRInfo *info;
    ComboCell *cell = NULL;
    Account* new_acct;
    Split *split;
    char *name;

    g_return_val_if_fail(reg, TRUE);

    /* See if we are leaving an account field */
    if (gnc_cell_name_equal (cell_name, XFRM_CELL))
    {
        if (gnc_table_layout_get_cell_changed (reg->table->layout,
                                               XFRM_CELL, FALSE))
            cell = (ComboCell *) gnc_table_layout_get_cell (reg->table->layout,
                    XFRM_CELL);
    }
    else if (gnc_cell_name_equal (cell_name, MXFRM_CELL))
    {
        if (gnc_table_layout_get_cell_changed (reg->table->layout,
                                               MXFRM_CELL, FALSE))
            cell = (ComboCell *) gnc_table_layout_get_cell (reg->table->layout,
                    MXFRM_CELL);
    }

    if (!cell)
        return TRUE;

    /* The account has been changed. */
    name = cell->cell.value;
    DEBUG("Changed to %s", name ? name : "NULL");
    if (!name || *name == '\0' ||
            g_strcmp0 (name, SPLIT_TRANS_STR) == 0 ||
            g_strcmp0 (name, STOCK_SPLIT_STR) == 0)
        return TRUE;

    /* Create the account if necessary. Also checks for a placeholder. */
    info = gnc_split_register_get_info (reg);
    new_acct = gnc_split_register_get_account_by_name (reg,
               (BasicCell *) cell,
               cell->cell.value);
    if (!new_acct)
        return FALSE;

    split = gnc_split_register_get_current_split(reg);
    gnc_split_register_set_cell_fractions (reg, split);

    /* See if we need to reset the exchange rate. */
    if (gnc_split_reg_has_rate_cell(reg->type))
    {
        PriceCell     *rate_cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                                            RATE_CELL);
        Account       *orig_acct = xaccSplitGetAccount(split);
        gnc_commodity *orig_com  = xaccAccountGetCommodity(orig_acct);
        gnc_commodity *last_com  = xaccAccountGetCommodity(info->rate_account);
        gnc_commodity *new_com   = xaccAccountGetCommodity(new_acct);

        if (gnc_commodity_equal(last_com ? last_com : orig_com, new_com))
        {
            DEBUG("Commodity is still %s. Leaving rate unchanged.",
                  new_com  ? gnc_commodity_get_mnemonic(new_com) : "NULL");
        }
        else if (!gnc_commodity_equal(orig_com, new_com))
        {
            /* The commodity has changed but is not the original. Reset the rate. */
            DEBUG("Commodity now %s (originally %s). Clearing rate.",
                  new_com  ? gnc_commodity_get_mnemonic(new_com) : "NULL",
                  orig_com ? gnc_commodity_get_mnemonic(orig_com) : "NULL");

            gnc_price_cell_set_value (rate_cell, gnc_numeric_zero());
            info->rate_account = new_acct;
            info->rate_reset = RATE_RESET_REQD;
        }
        else
        {
            /* Get the original rate from the split. */
            gnc_numeric amt       = xaccSplitGetAmount(split);
            gnc_numeric val       = xaccSplitGetValue(split);
            gnc_numeric orig_rate = gnc_numeric_div(amt, val, GNC_DENOM_AUTO,
                                                    GNC_HOW_DENOM_REDUCE);

            if (!gnc_numeric_check(orig_rate))
            {
                DEBUG("Using original rate of %s.",
                      gnc_num_dbg_to_string(orig_rate));
                gnc_price_cell_set_value (rate_cell, orig_rate);
                info->rate_account = new_acct;
                info->rate_reset = RATE_RESET_NOT_REQD;
            }
            else
            {
                DEBUG("Can't get rate. Using zero.");
                gnc_price_cell_set_value (rate_cell, gnc_numeric_zero());
                info->rate_account = new_acct;
                info->rate_reset = RATE_RESET_REQD;
            }
        }
    }

    return TRUE;
}

static void
gnc_split_register_move_cursor (VirtualLocation *p_new_virt_loc,
                                gpointer user_data)
{
    VirtualLocation new_virt_loc = *p_new_virt_loc;
    VirtualCellLocation old_trans_split_loc;
    SplitRegister *reg = user_data;
    Transaction *pending_trans;
    Transaction *new_trans;
    Transaction *old_trans;
    Split *old_trans_split;
    Split *new_trans_split;
    Split *new_split;
    Split *old_split;
    CursorClass new_class;
    CursorClass old_class;
    gboolean exact_traversal;
    gboolean do_refresh;
    gboolean saved;
    SRInfo *info;

    ENTER("reg=%p, p_new_virt_loc=%p (%d, %d)",
          reg, p_new_virt_loc,
          new_virt_loc.vcell_loc.virt_row,
          new_virt_loc.vcell_loc.virt_col);

    if (!reg)
    {
        LEAVE("no register");
        return;
    }

    info = gnc_split_register_get_info (reg);

    /* The transaction we are coming from */
    old_split = gnc_split_register_get_current_split (reg);
    old_trans = gnc_split_register_get_current_trans (reg);
    old_trans_split =
        gnc_split_register_get_current_trans_split (reg, &old_trans_split_loc);
    old_class = gnc_split_register_get_current_cursor_class (reg);

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
        new_trans = gnc_split_register_get_trans (reg, new_virt_loc.vcell_loc);

        /* The split we are moving to */
        new_split = gnc_split_register_get_split (reg, new_virt_loc.vcell_loc);

        /* The split at the transaction line we are moving to */
        new_trans_split = gnc_split_register_get_trans_split
                          (reg, new_virt_loc.vcell_loc, NULL);

        new_class = gnc_split_register_get_cursor_class (reg,
                    new_virt_loc.vcell_loc);
    }
    else
    {
        new_trans = info->cursor_hint_trans;
        new_split = info->cursor_hint_split;
        new_trans_split = info->cursor_hint_trans_split;
        new_class = info->cursor_hint_cursor_class;
    }

    info->hint_set_by_traverse = FALSE;
    info->reg_loaded = FALSE;

    gnc_suspend_gui_refresh ();

    /* commit the contents of the cursor into the database */
    saved = gnc_split_register_save (reg, old_trans != new_trans);
    pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                     gnc_get_current_book ());
    if ((old_class == CURSOR_CLASS_SPLIT) &&
            old_split &&
            (old_split != new_split) &&
            gnc_split_register_old_split_empty_p(reg, old_split))
    {
        int current_row;

        xaccSplitDestroy(old_split);
        old_split = NULL;

        /*
         * If the user is moving down a row, we've just thrown off the
         * numbers by deleting a split. Correct for that.
         */
        current_row = reg->table->current_cursor_loc.vcell_loc.virt_row;
        if (new_virt_loc.vcell_loc.virt_row > current_row)
            new_virt_loc.vcell_loc.virt_row--;
    }
    else if ((pending_trans != NULL)      &&
             (pending_trans == old_trans) &&
             (old_trans != new_trans))
    {
        if (gnc_split_register_balance_trans (reg, pending_trans))
        {
            /* Trans was unbalanced. */
            new_trans = old_trans;
            new_split = old_split;
            new_trans_split = old_trans_split;
            new_class = old_class;
            new_virt_loc = reg->table->current_cursor_loc;
        }
        else
        {
            /* Trans was balanced. Let it go. */
            info->pending_trans_guid = *guid_null ();
            if (xaccTransIsOpen (pending_trans))
                xaccTransCommitEdit (pending_trans);
            else g_assert_not_reached();

            pending_trans = NULL;
            saved = TRUE;
        }
    }
    else if (old_trans &&
             (old_trans != new_trans) &&
             !xaccTransHasReconciledSplits(old_trans) &&
             !info->first_pass &&
             gnc_split_register_balance_trans (reg, old_trans))
    {
        /* no matter what, stay there so the user can see what happened */
        new_trans = old_trans;
        new_split = old_split;
        new_trans_split = old_trans_split;
        new_class = old_class;
        new_virt_loc = reg->table->current_cursor_loc;
    }

    if (saved)
    {
        info->cursor_hint_trans = new_trans;
        info->cursor_hint_split = new_split;
        info->cursor_hint_trans_split = new_trans_split;
        info->cursor_hint_cursor_class = new_class;
    }

    if (old_split != new_split)
    {
        info->change_confirmed = FALSE;
        info->rate_account = NULL;
        info->rate_reset = RATE_RESET_NOT_REQD;
    }

    gnc_resume_gui_refresh ();

    /* redrawing the register can muck everything up */
    if (saved)
    {
        VirtualCellLocation vcell_loc;

        if (!info->reg_loaded)
            gnc_split_register_redraw (reg);

        /* if the split we were going to is still in the register,
         * then it may have moved. Find out where it is now. */
        if (gnc_split_register_find_split (reg, new_trans, new_trans_split,
                                           new_split, new_class, &vcell_loc))
        {

            gnc_table_get_virtual_cell (reg->table, vcell_loc);
            new_virt_loc.vcell_loc = vcell_loc;
        }
        else
            new_virt_loc.vcell_loc = reg->table->current_cursor_loc.vcell_loc;

        new_trans = gnc_split_register_get_trans (reg, new_virt_loc.vcell_loc);
        new_split = gnc_split_register_get_split (reg, new_virt_loc.vcell_loc);
        new_trans_split = gnc_split_register_get_trans_split(
                              reg, new_virt_loc.vcell_loc, NULL);
        new_class = gnc_split_register_get_cursor_class (reg,
                    new_virt_loc.vcell_loc);
    }
    else if (info->traverse_to_new)
    {
        new_trans = info->cursor_hint_trans;
        new_split = info->cursor_hint_split;
        new_trans_split = info->cursor_hint_trans_split;
        new_class = info->cursor_hint_cursor_class;
    }

    gnc_table_find_close_valid_cell (reg->table, &new_virt_loc, exact_traversal);

    *p_new_virt_loc = new_virt_loc;

    PINFO ("after move %d %d \n",
           new_virt_loc.vcell_loc.virt_row,
           new_virt_loc.vcell_loc.virt_col);

    /* if the register was reloaded, then everything should be fine :)
     * otherwise, we may need to change some visibility settings. */
    if (saved)
    {
        gnc_split_register_set_cell_fractions (reg, new_split);

        LEAVE("saved");
        return;
    }

    /* in the mult-line and dynamic modes, we need to hide the old
     * and show the new. */
    if (gnc_split_register_current_trans_expanded (reg) &&
            (old_trans_split != new_trans_split))
    {
        VirtualCellLocation vc_loc;

        vc_loc = old_trans_split_loc;
        gnc_table_set_virt_cell_cursor(
            reg->table, vc_loc, gnc_split_register_get_passive_cursor (reg));
        gnc_split_register_set_trans_visible (reg, vc_loc, FALSE,
                                              reg->style == REG_STYLE_JOURNAL);

        if ((REG_STYLE_AUTO_LEDGER == reg->style) ||
                (REG_STYLE_JOURNAL     == reg->style))
        {
            gnc_split_register_get_trans_split (reg, new_virt_loc.vcell_loc,
                                                &vc_loc);
            gnc_table_set_virt_cell_cursor
            (reg->table, vc_loc, gnc_split_register_get_active_cursor (reg));
            gnc_split_register_set_trans_visible (reg, vc_loc, TRUE,
                                                  reg->style == REG_STYLE_JOURNAL);
        }

        info->trans_expanded = FALSE;

        do_refresh = TRUE;
    }
    else
        do_refresh = FALSE;

    info->cursor_hint_trans = new_trans;
    info->cursor_hint_split = new_split;
    info->cursor_hint_trans_split = new_trans_split;
    info->cursor_hint_cursor_class = new_class;

    gnc_split_register_set_cell_fractions (reg, new_split);

    gnc_table_find_close_valid_cell (reg->table, p_new_virt_loc,
                                     exact_traversal);

    if (do_refresh)
    {
        VirtualCellLocation vc_loc;

        gnc_table_refresh_gui (reg->table, FALSE);
        gnc_table_leave_update (reg->table, reg->table->current_cursor_loc);

        gnc_split_register_get_trans_split (reg, p_new_virt_loc->vcell_loc,
                                            &vc_loc);
        gnc_split_register_show_trans (reg, vc_loc);
    }

    LEAVE(" ");
}

static Split *
gnc_find_split_in_trans_by_memo (Transaction *trans, const char *memo,
                                 gboolean unit_price)
{
    int i = 0;
    Split *split;

    while ((split = xaccTransGetSplit(trans, i)) != NULL)
    {
        i++;
        if (unit_price)
        {
            gnc_numeric price = xaccSplitGetSharePrice (split);
            if (!gnc_numeric_equal (price, gnc_numeric_create (1, 1)))
                continue;
        }

        if (g_strcmp0 (memo, xaccSplitGetMemo (split)) == 0)
            return split;
    }

    return NULL;
}

static Split *
gnc_find_split_in_account_by_memo (Account *account, const char *memo,
                                   gboolean unit_price)
{
    GList *slp;

    if (account == NULL) return NULL;

    for (slp = g_list_last (xaccAccountGetSplitList (account));
            slp;
            slp = slp->prev)
    {
        Split *split = slp->data;
        Transaction *trans = xaccSplitGetParent (split);

        split = gnc_find_split_in_trans_by_memo (trans, memo, unit_price);

        if (split) return split;
    }

    return NULL;
}

static Split *
gnc_find_split_in_reg_by_memo (SplitRegister *reg, const char *memo,
                               gboolean unit_price)
{
    int virt_row, virt_col;
    int num_rows, num_cols;
    Transaction *last_trans;

    if (!reg || !reg->table)
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

            split = gnc_split_register_get_split (reg, vcell_loc);
            trans = xaccSplitGetParent (split);

            if (trans == last_trans)
                continue;

            split = gnc_find_split_in_trans_by_memo (trans, memo, unit_price);
            if (split != NULL)
                return split;

            last_trans = trans;
        }

    return NULL;
}

static Transaction *
gnc_find_trans_in_reg_by_desc (SplitRegister *reg, const char *description)
{
    int virt_row, virt_col;
    int num_rows, num_cols;
    Transaction *last_trans;

    if (!reg || !reg->table)
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

            split = gnc_split_register_get_split (reg, vcell_loc);
            trans = xaccSplitGetParent(split);

            if (trans == last_trans)
                continue;

            if (g_strcmp0 (description, xaccTransGetDescription (trans)) == 0)
                return trans;

            last_trans = trans;
        }

    return NULL;
}

/* This function determines if auto-completion is appropriate and,
 * if so, performs it. This should only be called by LedgerTraverse. */
static gboolean
gnc_split_register_auto_completion (SplitRegister *reg,
                                    gncTableTraversalDir dir,
                                    VirtualLocation *p_new_virt_loc)
{
    SRInfo *info = gnc_split_register_get_info (reg);
    VirtualLocation new_virt_loc;
    CursorClass cursor_class;
    Transaction *pending_trans;
    Transaction *blank_trans;
    const char *cell_name;
    Transaction *trans;
    Split *blank_split;
    gnc_numeric amount;
    BasicCell *cell;
    Split *split;

    if (!reg->do_auto_complete)
        return FALSE;

    blank_split = xaccSplitLookup (&info->blank_split_guid,
                                   gnc_get_current_book ());
    blank_trans = xaccSplitGetParent (blank_split);

    pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                     gnc_get_current_book ());

    /* auto-completion is only triggered by a tab out */
    if (dir != GNC_TABLE_TRAVERSE_RIGHT)
        return FALSE;

    split = gnc_split_register_get_current_split (reg);
    trans = gnc_split_register_get_current_trans (reg);
    if (trans == NULL)
        return FALSE;

    cursor_class = gnc_split_register_get_current_cursor_class (reg);
    cell_name = gnc_table_get_current_cell_name (reg->table);

    switch (cursor_class)
    {
    case CURSOR_CLASS_TRANS:
    {
        Transaction *auto_trans;
        const char *desc;

        /* there must be a blank transaction * */
        if (blank_trans == NULL)
            return FALSE;

        /* we must be on the blank split */
        if (trans != blank_trans)
            return FALSE;

        /* and leaving the description cell */
        if (!gnc_cell_name_equal (cell_name, DESC_CELL))
            return FALSE;

        /* nothing but the date, num, and description should be changed */
        /* FIXME, this should be refactored. */
        if (gnc_table_layout_get_cell_changed (reg->table->layout,
                                               XFRM_CELL, TRUE)  ||
                gnc_table_layout_get_cell_changed (reg->table->layout,
                        MXFRM_CELL, TRUE) ||
                gnc_table_layout_get_cell_changed (reg->table->layout,
                        PRIC_CELL, TRUE)  ||
                gnc_table_layout_get_cell_changed (reg->table->layout,
                        SHRS_CELL, TRUE)  ||
                gnc_table_layout_get_cell_changed (reg->table->layout,
                        DEBT_CELL, TRUE)  ||
                gnc_table_layout_get_cell_changed (reg->table->layout,
                        CRED_CELL, TRUE)  ||
                gnc_table_layout_get_cell_changed (reg->table->layout,
                        NOTES_CELL, TRUE) ||
                gnc_table_layout_get_cell_changed (reg->table->layout,
                        RECN_CELL, TRUE))
            return FALSE;

        /* and the description should be changed */
        if (!gnc_table_layout_get_cell_changed (reg->table->layout,
                                                DESC_CELL, TRUE))
            return FALSE;

        /* to a non-empty value */
        desc = gnc_table_layout_get_cell_value (reg->table->layout, DESC_CELL);
        if ((desc == NULL) || (*desc == '\0'))
            return FALSE;

        /* find a transaction to auto-complete on */
        if (gnc_split_register_get_default_account (reg) != NULL)
        {
            Account *account = gnc_split_register_get_default_account (reg);

            auto_trans = xaccAccountFindTransByDesc(account, desc);
        }
        else
            auto_trans = gnc_find_trans_in_reg_by_desc(reg, desc);

        if (auto_trans == NULL)
            return FALSE;

        gnc_suspend_gui_refresh ();

        /* We are guaranteed to be on the blank trans, so we can
           discount the possibility that the current transaction is
           being edited in another register. */
        /* now perform the completion */
        if (pending_trans != trans)
        {
            if (!xaccTransIsOpen(trans))
                xaccTransBeginEdit(trans);
            /* This is now the pending transaction */
            info->pending_trans_guid = *xaccTransGetGUID(trans);
            if (pending_trans != NULL)
            {
                if (xaccTransIsOpen (pending_trans))
                    xaccTransCommitEdit (pending_trans);
                else g_assert_not_reached();
            }
        }
        g_assert(xaccTransIsOpen(trans));
        pending_trans = xaccTransLookup(&info->pending_trans_guid,
                                        gnc_get_current_book ());
        g_assert(pending_trans == trans);

        gnc_copy_trans_onto_trans (auto_trans, trans, FALSE, FALSE);
        blank_split = NULL;

        if (gnc_split_register_get_default_account (reg) != NULL)
        {
            Account *default_account =
                gnc_split_register_get_default_account (reg);
            gnc_commodity *trans_cmdty = xaccTransGetCurrency(trans);
            gnc_commodity *acct_cmdty = xaccAccountGetCommodity(default_account);
            Split *s;
            int i = 0;
            if (gnc_commodity_is_currency(acct_cmdty) &&
                !gnc_commodity_equal(trans_cmdty, acct_cmdty))
                xaccTransSetCurrency(trans, acct_cmdty);

            while ((s = xaccTransGetSplit(trans, i)) != NULL)
            {
                if (default_account == xaccSplitGetAccount(s))
                {
                    blank_split = s;
                    info->blank_split_guid = *xaccSplitGetGUID(blank_split);
                    break;
                }
                i++;
            }
        }

        if (blank_split == NULL)
        {
            blank_split = xaccTransGetSplit(trans, 0);
            info->blank_split_guid = *xaccSplitGetGUID(blank_split);
        }
        DEBUG("blank_split=%p", blank_split);

        info->blank_split_edited = TRUE;

        {
            SRSaveData *sd;

            sd = gnc_split_register_save_data_new(
                     trans, blank_split, gnc_split_register_current_trans_expanded (reg));
            gnc_table_save_cells (reg->table, sd);
            gnc_split_register_save_data_destroy (sd);
        }

        gnc_resume_gui_refresh ();

        /* now move to the non-empty amount column unless config setting says not */
        if ( !gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL_REGISTER,
                                 GNC_PREF_TAB_TRANS_MEMORISED) )
        {
            amount = xaccSplitGetAmount (blank_split);
            cell_name = (gnc_numeric_negative_p (amount)) ? CRED_CELL : DEBT_CELL;

            if (gnc_table_get_current_cell_location (reg->table, cell_name,
                    &new_virt_loc))
                *p_new_virt_loc = new_virt_loc;
        }
    }

    break;

    case CURSOR_CLASS_SPLIT:
    {
        char *account_name;
        const char *memo;
        gboolean unit_price;
        Split *auto_split;

        /* we must be on a blank split of a transaction */
        if (split != NULL)
            return FALSE;

        /* and leaving the memo cell */
        if (!gnc_cell_name_equal (cell_name, MEMO_CELL))
            return FALSE;

        /* nothing but the action, memo, and amounts should be changed */
        /* FIXME. This should be refactored. */
        if (gnc_table_layout_get_cell_changed (reg->table->layout,
                                               XFRM_CELL, TRUE)  ||
                gnc_table_layout_get_cell_changed (reg->table->layout,
                        MXFRM_CELL, TRUE) ||
                gnc_table_layout_get_cell_changed (reg->table->layout,
                        PRIC_CELL, TRUE)  ||
                gnc_table_layout_get_cell_changed (reg->table->layout,
                        SHRS_CELL, TRUE)  ||
                gnc_table_layout_get_cell_changed (reg->table->layout,
                        RECN_CELL, TRUE))
            return FALSE;

        /* and the memo should be changed */
        if (!gnc_table_layout_get_cell_changed (reg->table->layout,
                                                MEMO_CELL, TRUE))
            return FALSE;

        /* to a non-empty value */
        memo = gnc_table_layout_get_cell_value (reg->table->layout, MEMO_CELL);
        if ((memo == NULL) || (*memo == '\0'))
            return FALSE;

        /* if there is no price field, only auto-complete from splits with
         * a unit share price. */
        unit_price = !gnc_table_get_current_cell_location (reg->table,
                     PRIC_CELL, NULL);

        /* find a split to auto-complete on */
        if (gnc_split_register_get_default_account (reg) != NULL)
        {
            Account *account = gnc_split_register_get_default_account (reg);

            auto_split = gnc_find_split_in_account_by_memo (account, memo,
                         unit_price);
        }
        else
            auto_split = gnc_find_split_in_reg_by_memo (reg, memo, unit_price);

        if (auto_split == NULL)
            return FALSE;

        /* the auto-complete code below is taken from xaccSRGetEntryHandler */

        /* auto-complete the action field if it wasn't changed */
        if (!gnc_table_layout_get_cell_changed (reg->table->layout,
                                                ACTN_CELL, TRUE))
        {
            cell = gnc_table_layout_get_cell (reg->table->layout, ACTN_CELL);
            gnc_combo_cell_set_value ((ComboCell *) cell,
                                      gnc_get_num_action (NULL, auto_split));
        }

        /* auto-complete the account name */
        cell = gnc_table_layout_get_cell (reg->table->layout, XFRM_CELL);

        account_name = gnc_get_account_name_for_register (xaccSplitGetAccount (auto_split));
        gnc_combo_cell_set_value ((ComboCell *) cell, account_name);
        g_free(account_name);

        gnc_basic_cell_set_changed (cell, TRUE);

        if (!gnc_table_layout_get_cell_changed (reg->table->layout,
                                                DEBT_CELL, TRUE) &&
                !gnc_table_layout_get_cell_changed (reg->table->layout,
                        CRED_CELL, TRUE))
        {
            BasicCell *debit_cell;
            BasicCell *credit_cell;

            amount = xaccSplitGetValue (auto_split);

            debit_cell = gnc_table_layout_get_cell (reg->table->layout,
                                                    DEBT_CELL);
            credit_cell = gnc_table_layout_get_cell (reg->table->layout,
                          CRED_CELL);

            gnc_price_cell_set_debt_credit_value ((PriceCell *) debit_cell,
                                                  (PriceCell *) credit_cell,
                                                  amount);

            gnc_basic_cell_set_changed (debit_cell, TRUE);
            gnc_basic_cell_set_changed (credit_cell, TRUE);
        }

        /* and refresh the gui */
        gnc_table_refresh_gui (reg->table, TRUE);

        /* now move to the non-empty amount column */
        amount = xaccSplitGetAmount (auto_split);
        cell_name = (gnc_numeric_negative_p (amount)) ? CRED_CELL : DEBT_CELL;

        if (gnc_table_get_current_cell_location (reg->table, cell_name,
                &new_virt_loc))
            *p_new_virt_loc = new_virt_loc;
    }

    break;

    default:
        break;
    }

    return TRUE;
}

static void
gnc_split_register_check_stock_action (SplitRegister *reg,
                                       const char *cell_name)
{
    BasicCell *cell;
    gnc_numeric shares;
    gboolean buy, sell;
    const char *name;

    if (!gnc_cell_name_equal (cell_name, ACTN_CELL) ||
            !gnc_table_layout_get_cell_changed (reg->table->layout,
                    ACTN_CELL, FALSE))
        return;

    cell = gnc_table_layout_get_cell (reg->table->layout, ACTN_CELL);
    if (!cell)
        return;
    name = ((ComboCell *)cell)->cell.value;
    if ((name == NULL) || (*name == '\0'))
        return;

    buy  = g_strcmp0 (name, ACTION_BUY_STR)  == 0;
    sell = g_strcmp0 (name, ACTION_SELL_STR) == 0;
    if (!buy && !sell)
        return;

    cell = gnc_table_layout_get_cell (reg->table->layout, SHRS_CELL);
    if (!cell)
        return;
    shares = gnc_price_cell_get_value ((PriceCell *) cell);

    if ((buy  && !gnc_numeric_positive_p (shares)) ||
            (sell &&  gnc_numeric_positive_p (shares)))
    {
        gnc_price_cell_set_value ((PriceCell *)cell, gnc_numeric_neg (shares));
        gnc_basic_cell_set_changed (cell, TRUE);
    }
}

static void
gnc_split_register_check_stock_shares (SplitRegister *reg,
                                       const char *cell_name)
{
    BasicCell *cell;
    gnc_numeric shares;
    gboolean buy;
    const char *name;

    if (!gnc_cell_name_equal (cell_name, SHRS_CELL) ||
            !gnc_table_layout_get_cell_changed (reg->table->layout,
                    SHRS_CELL, FALSE))
        return;

    cell = gnc_table_layout_get_cell (reg->table->layout, SHRS_CELL);
    if (!cell)
        return;
    shares = gnc_price_cell_get_value ((PriceCell *) cell);
    if (gnc_numeric_zero_p (shares))
        return;
    buy  = gnc_numeric_positive_p (shares);

    cell = gnc_table_layout_get_cell (reg->table->layout, ACTN_CELL);
    if (!cell)
        return;
    name = ((ComboCell *)cell)->cell.value;

    if (!g_strcmp0(name, "") ||
            !g_strcmp0(name, buy ? ACTION_SELL_STR : ACTION_BUY_STR))
    {
        gnc_combo_cell_set_value((ComboCell *)cell,
                                 buy ? ACTION_BUY_STR : ACTION_SELL_STR);
        gnc_basic_cell_set_changed (cell, TRUE);
    }
}

/* This function checks a cell for changes and takes appropriate action if a
 * change has occurred. It is recommended to call this function just before
 * leaving a cell. Returns FALSE if control should remain in this cell. For
 * example, the user may have made a mistake and needs another chance to
 * edit the information before moving on. */
gboolean
gnc_split_register_check_cell (SplitRegister *reg, const char *cell_name)
{
    ENTER("reg=%p, cell_name=%s", reg, cell_name ? cell_name : "NULL");

    /* See if we are leaving an account field. */
    if (!gnc_split_register_check_account (reg, cell_name))
    {
        LEAVE("account check failed");
        return FALSE;
    }

    /* See if we are leaving a debit or credit cell */
    if (!gnc_split_register_check_debcred (reg, cell_name))
    {
        LEAVE("debit/credit check failed");
        return FALSE;
    }

    /* See if we are leaving an action field */
    if ((reg->type == STOCK_REGISTER) ||
            (reg->type == PORTFOLIO_LEDGER) ||
            (reg->type == CURRENCY_REGISTER))
    {
        gnc_split_register_check_stock_action (reg, cell_name);
        gnc_split_register_check_stock_shares (reg, cell_name);
    }

    LEAVE(" ");
    return TRUE;
}

static Account *
gnc_split_register_get_account_always (SplitRegister *reg,
                                       const char * cell_name)
{
    BasicCell *cell;
    const char *name;

    cell = gnc_table_layout_get_cell (reg->table->layout, cell_name);
    if (!cell)
        return NULL;
    name = gnc_basic_cell_get_value (cell);

    /* If 'name' is "-- Split Transaction --" then return NULL or the
       register acct */
    if (!g_strcmp0 (name, SPLIT_TRANS_STR))
    {
        return NULL;
    }

    return gnc_split_register_get_account_by_name (reg, cell, name);
}

#if 0 /* Not Used */
static const char *
gnc_split_register_get_cell_string (SplitRegister *reg, const char *cell_name)
{
    BasicCell *cell;

    cell = gnc_table_layout_get_cell (reg->table->layout, cell_name);
    if (!cell)
        return "";

    return gnc_basic_cell_get_value (cell);
}

static Timespec
gnc_split_register_get_cell_date (SplitRegister *reg, const char *cell_name)
{
    DateCell *cell;
    Timespec ts;

    cell = (DateCell*) gnc_table_layout_get_cell (reg->table->layout, cell_name);

    if (cell)
        gnc_date_cell_get_date (cell, &ts);
    else
        timespecFromTime64 (&ts, gnc_time (NULL));

    return ts;
}
#endif /* Not Used */

/* Creates a transfer dialog and fills its values from register cells (if
 * available) or from the provided transaction and split.
 */
static XferDialog *
gnc_split_register_xfer_dialog(SplitRegister *reg, Transaction *txn,
                               Split *split)
{
    XferDialog *xfer;
    CellBlock *cur;
    BasicCell *cell;

    g_return_val_if_fail(reg, NULL);
    g_return_val_if_fail(reg->table, NULL);
    cur = reg->table->current_cursor;

    /* Create the exchange rate dialog. */
    xfer = gnc_xfer_dialog(NULL, NULL);
    g_return_val_if_fail(xfer, NULL);

    /* Set the description. */
    cell = gnc_cellblock_get_cell_by_name(cur, DESC_CELL, NULL, NULL);
    if (cell)
        gnc_xfer_dialog_set_description(xfer, gnc_basic_cell_get_value(cell));
    else
    {
        const char *str = xaccTransGetDescription(txn);
        gnc_xfer_dialog_set_description(xfer, str ? str : "");
    }

    /* Set the memo. */
    cell = gnc_cellblock_get_cell_by_name(cur, MEMO_CELL, NULL, NULL);
    if (cell)
        gnc_xfer_dialog_set_memo(xfer, gnc_basic_cell_get_value(cell));
    else
    {
        const char *str = xaccSplitGetMemo(split);
        gnc_xfer_dialog_set_memo(xfer, str ? str : "");
    }

    /* Set the num. */
    cell = gnc_cellblock_get_cell_by_name(cur, NUM_CELL, NULL, NULL);
    if (cell)
        gnc_xfer_dialog_set_num(xfer, gnc_basic_cell_get_value(cell));
    else
    {
        const char *str = gnc_get_num_action (txn, split);
        gnc_xfer_dialog_set_num(xfer, str ? str : "");
    }

    /* Set the date. */
    cell = gnc_cellblock_get_cell_by_name(cur, DATE_CELL, NULL, NULL);
    if (cell)
    {
        Timespec ts;
        gnc_date_cell_get_date((DateCell*) cell, &ts);
        gnc_xfer_dialog_set_date(xfer, timespecToTime64(ts));
    }
    else
        gnc_xfer_dialog_set_date(xfer, xaccTransGetDate(txn));

    return xfer;
}

/** If needed display the transfer dialog to get a price/exchange rate and
 * adjust the price cell accordingly.
 * If the dialog does not complete successfully, then return TRUE.
 * Return FALSE in all other cases (meaning "move on")
 * @param reg the register to operate on
 * @param force_dialog pop a dialog even if we don't think we need it.
 * @return whether more handling is required.
 */
gboolean
gnc_split_register_handle_exchange (SplitRegister *reg, gboolean force_dialog)
{
    SRInfo *info;
    Transaction *txn;
    Split *split, *osplit;
    Account *xfer_acc, *reg_acc;
    gnc_commodity *txn_cur, *xfer_com, *reg_com;
    gnc_numeric amount, exch_rate;
    XferDialog *xfer;
    gboolean expanded = FALSE;
    PriceCell *rate_cell;
    const char *message;
    CursorClass cursor_class;

    ENTER("reg=%p, force_dialog=%s", reg, force_dialog ? "TRUE" : "FALSE" );

    /* Make sure we NEED this for this type of register */
    if (!gnc_split_reg_has_rate_cell (reg->type))
    {
        if (force_dialog)
        {
            message = _("This register does not support editing exchange rates.");
            gnc_error_dialog(gnc_split_register_get_parent(reg), "%s", message);
        }
        LEAVE("no rate cell");
        return FALSE;
    }

    rate_cell = (PriceCell*) gnc_table_layout_get_cell(
                    reg->table->layout, RATE_CELL);
    if (!rate_cell)
    {
        if (force_dialog)
        {
            message = _("This register does not support editing exchange rates.");
            gnc_error_dialog(gnc_split_register_get_parent(reg), "%s", message);
        }
        LEAVE("null rate cell");
        return FALSE;
    }

    /* See if we already have an exchange rate... */
    info = gnc_split_register_get_info (reg);
    exch_rate = gnc_price_cell_get_value (rate_cell);
    if (!gnc_numeric_zero_p(exch_rate) && !force_dialog &&
        info->rate_reset != RATE_RESET_REQD)
    {
        LEAVE("rate already non-zero");
        return FALSE;
    }

    /* Are we expanded? */
    expanded = gnc_split_register_current_trans_expanded (reg);
    cursor_class = gnc_split_register_get_current_cursor_class (reg);

    /* If we're expanded AND a transaction cursor, there is nothing to do */
    if (expanded && cursor_class == CURSOR_CLASS_TRANS)
    {
        if (force_dialog)
        {
            message = _("You need to select a split in order to modify its exchange "
                        "rate.");
            gnc_error_dialog(gnc_split_register_get_parent(reg), "%s", message);
        }
        LEAVE("expanded with transaction cursor; nothing to do");
        return FALSE;
    }

    /* Grab the xfer account */
    xfer_acc = gnc_split_register_get_account_always(
                   reg, expanded ? XFRM_CELL : MXFRM_CELL);

    /* If this is an un-expanded, multi-split transaction, then warn the user */
    if (force_dialog && !expanded && !xfer_acc)
    {
        message = _("You need to expand the transaction in order to modify its "
                    "exchange rates.");
        gnc_error_dialog (gnc_split_register_get_parent (reg), "%s", message);
        LEAVE("%s", message);
        return TRUE;
    }

    /* No account -- don't run the dialog */
    if (!xfer_acc)
    {
        if (force_dialog)
        {
            message = _("The entered account could not be found.");
            gnc_error_dialog(gnc_split_register_get_parent(reg), "%s", message);
        }
        LEAVE("no xfer account");
        return FALSE;
    }

    /* Grab the txn currency and xfer commodity */
    txn = gnc_split_register_get_current_trans (reg);
    txn_cur = xaccTransGetCurrency (txn);
    xfer_com = xaccAccountGetCommodity (xfer_acc);

    /* Grab the register account and commodity (may be used later) */
    reg_acc = gnc_split_register_get_default_account (reg);
    reg_com = xaccAccountGetCommodity (reg_acc);

    /* Grab the split and perhaps the "other" split (if it is a two-split txn) */
    split = gnc_split_register_get_current_split (reg);
    osplit = xaccSplitGetOtherSplit (split);

    /* Check if the txn- and xfer- commodities are the same */
    if (gnc_commodity_equal (txn_cur, xfer_com))
    {
        /* If we're not forcing the dialog, then there is no reason to
         * go on.  We're using the correct accounts.
         */
        if (!force_dialog)
        {
            LEAVE("txn and account currencies match, and not forcing");
            return FALSE;
        }

        /* Only proceed with two-split, basic, non-expanded registers */
        if (expanded || osplit == NULL)
        {
            message = _("The two currencies involved equal each other.");
            gnc_error_dialog(gnc_split_register_get_parent(reg), "%s", message);
            LEAVE("register is expanded or osplit == NULL; not forcing dialog");
            return FALSE;
        }

        /* If we're forcing, then compare the current account
         * commodity to the transaction currency.
         */
        xfer_acc = reg_acc;
        xfer_com = reg_com;
        if (gnc_commodity_equal (txn_cur, xfer_com))
        {
            message = _("The two currencies involved equal each other.");
            gnc_error_dialog(gnc_split_register_get_parent(reg), "%s", message);
            LEAVE("reg commodity == txn commodity; not forcing");
            return FALSE;
        }
    }

    /* If this is a non-expanded, two-split txn where BOTH splits need
     * conversion rates, then require the user to actually expand the
     * transaction in order to edit it.
     */
    if (!expanded && osplit &&
            gnc_split_register_split_needs_amount (reg, split) &&
            gnc_split_register_split_needs_amount (reg, osplit))
    {
        message = _("You need to expand the transaction in order to modify its "
                    "exchange rates.");
        if (force_dialog)
        {
            gnc_error_dialog (gnc_split_register_get_parent (reg), "%s", message);
        }
        LEAVE("%s", message);
        return TRUE;
    }

    /* Strangely, if we're in a two-split, non-expanded txn, we need
     * to do something really special with the exchange rate!  In
     * particular, we have to pick it up from the _other_ split --
     * right?
     * XXX: perhaps I should pop up an error here?  Or maybe require the
     * user to go into expanded-mode?
     */
    if (!expanded && osplit && !gnc_commodity_equal(reg_com, txn_cur) &&
            !gnc_commodity_equal(reg_com, xfer_com))
    {
        gnc_numeric amt = xaccSplitGetAmount (osplit);
        gnc_numeric val = xaccSplitGetValue (osplit);
        exch_rate = gnc_numeric_div (amt, val, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
    }

    /* Ok, we need to grab the exchange rate */
    amount = gnc_split_register_debcred_cell_value (reg);

    /*
     * If "amount" is zero then we don't need an exchange-rate.. Return
     * FALSE to let the user continue on.
     */
    if (gnc_numeric_zero_p (amount))
    {
        if (force_dialog)
        {
            message = _("The split's amount is zero, so no exchange rate is needed.");
            gnc_error_dialog(gnc_split_register_get_parent(reg), "%s", message);
        }
        LEAVE("amount is zero; no exchange rate needed");
        return FALSE;
    }

    /* If the exch_rate is zero, we're not forcing the dialog, and this is
     * _not_ the blank split, then return FALSE -- this is a "special"
     * gain/loss stock transaction.
     */
    if (gnc_numeric_zero_p(exch_rate) && !force_dialog && split &&
            info->rate_reset != RATE_RESET_REQD &&
            split != gnc_split_register_get_blank_split (reg))
    {
        LEAVE("gain/loss split; no exchange rate needed");
        return FALSE;
    }

    /* Show the exchange-rate dialog */
    xfer = gnc_split_register_xfer_dialog(reg, txn, split);
    gnc_xfer_dialog_is_exchange_dialog(xfer, &exch_rate);
    if (gnc_xfer_dialog_run_exchange_dialog(
                xfer, &exch_rate, amount, reg_acc, txn, xfer_com, expanded))
    {
        /* FIXME: How should the dialog be destroyed? */
        LEAVE("leaving rate unchanged");
        return TRUE;
    }
    /* FIXME: How should the dialog be destroyed? */

    /* Set the RATE_CELL on this cursor and mark it changed */
    gnc_price_cell_set_value (rate_cell, exch_rate);
    gnc_basic_cell_set_changed (&rate_cell->cell, TRUE);
    info->rate_account = xfer_acc;
    info->rate_reset = RATE_RESET_DONE;
    LEAVE("set rate=%s", gnc_num_dbg_to_string(exch_rate));
    return FALSE;
}

/* Returns FALSE if dialog was canceled. */
static gboolean
transaction_changed_confirm(VirtualLocation *p_new_virt_loc,
                            VirtualLocation *virt_loc,
                            SplitRegister *reg, Transaction *new_trans,
                            gboolean exact_traversal)
{
    GtkWidget *dialog, *window;
    gint response;
    const char *title = _("Save the changed transaction?");
    const char *message =
        _("The current transaction has been changed. Would you like to "
          "record the changes before moving to a new transaction, discard the "
          "changes, or return to the changed transaction?");

    window = gnc_split_register_get_parent(reg);
    dialog = gtk_message_dialog_new(GTK_WINDOW(window),
                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_QUESTION,
                                    GTK_BUTTONS_NONE,
                                    "%s", title);
    gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
            "%s", message);
    gtk_dialog_add_buttons(GTK_DIALOG(dialog),
                           _("_Discard Changes"), GTK_RESPONSE_REJECT,
                           _("_Cancel"), GTK_RESPONSE_CANCEL,
                           _("_Record Changes"), GTK_RESPONSE_ACCEPT,
                           NULL);
    response = gnc_dialog_run(GTK_DIALOG(dialog), GNC_PREF_WARN_REG_TRANS_MOD);
    gtk_widget_destroy(dialog);

    switch (response)
    {
    case GTK_RESPONSE_ACCEPT:
        break;

    case GTK_RESPONSE_REJECT:
    {
        VirtualCellLocation vcell_loc;
        Split *new_split;
        Split *trans_split;
        CursorClass new_class;

        new_split = gnc_split_register_get_split (reg, virt_loc->vcell_loc);
        trans_split = gnc_split_register_get_trans_split (reg,
                      virt_loc->vcell_loc,
                      NULL);
        new_class = gnc_split_register_get_cursor_class (reg,
                    virt_loc->vcell_loc);

        gnc_split_register_cancel_cursor_trans_changes (reg);

        if (gnc_split_register_find_split (reg, new_trans, trans_split,
                                           new_split, new_class, &vcell_loc))
            virt_loc->vcell_loc = vcell_loc;

        gnc_table_find_close_valid_cell (reg->table, virt_loc,
                                         exact_traversal);

        *p_new_virt_loc = *virt_loc;
    }
    break;

    case GTK_RESPONSE_CANCEL:
    default:
        return TRUE;
    }

    return FALSE;
}

/** Examine a request to traverse to a new location in the register and
 *  decide whether it should be allowed to proceed, and where the new
 *  location will be.
 *
 *  @param p_new_virt_loc a pointer to storage for the new location
 *
 *  @param dir the direction of the traversal
 *
 *  @param user_data pointer to a ::SplitRegister to traverse
 *
 *  @return @c TRUE if the traversal cannot be completed. Otherwise,
 *  @c FALSE is returned and the new location is stored at @a p_new_virt_loc. */
static gboolean
gnc_split_register_traverse (VirtualLocation *p_new_virt_loc,
                             gncTableTraversalDir dir,
                             gpointer user_data)
{
    SplitRegister *reg = user_data;
    Transaction *pending_trans;
    VirtualLocation virt_loc;
    Transaction *trans, *new_trans;
    gboolean changed;
    SRInfo *info;
    Split *split;
    const char *cell_name;

    g_return_val_if_fail(p_new_virt_loc, TRUE);

    ENTER("reg=%p, p_new_virt_loc=%p (%d,%d), dir=%d",
          reg, p_new_virt_loc, (*p_new_virt_loc).vcell_loc.virt_row,
          (*p_new_virt_loc).vcell_loc.virt_col, dir);

    if (!reg)
    {
        LEAVE("no register");
        return FALSE;
    }

    info = gnc_split_register_get_info (reg);

    if (info->first_pass)
    {
        LEAVE("first pass");
        return FALSE;
    }

    pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                     gnc_get_current_book ());
    virt_loc = *p_new_virt_loc;

    info->exact_traversal = (dir == GNC_TABLE_TRAVERSE_POINTER);

    split = gnc_split_register_get_current_split (reg);
    trans = gnc_split_register_get_current_trans (reg);
    if (trans == NULL)
    {
        LEAVE("no transaction");
        return FALSE;
    }

    /* no changes, make sure we aren't going off the end */
    changed = gnc_table_current_cursor_changed (reg->table, FALSE);
    if (!changed && (pending_trans != trans))
    {
        gnc_table_find_close_valid_cell (reg->table, &virt_loc,
                                         info->exact_traversal);

        *p_new_virt_loc = virt_loc;

        LEAVE("no changes");
        return FALSE;
    }

    /* Get the current cell-name and check it for changes. */
    cell_name = gnc_table_get_current_cell_name (reg->table);
    if (!gnc_split_register_check_cell (reg, cell_name))
    {
        LEAVE("check cell");
        return TRUE;
    }

    /* See if we are tabbing off the end of the very last line */
    do
    {
        VirtualLocation virt_loc;

        if (!changed && !info->blank_split_edited)
            break;

        if (dir != GNC_TABLE_TRAVERSE_RIGHT)
            break;

        virt_loc = reg->table->current_cursor_loc;
        if (gnc_table_move_vertical_position (reg->table, &virt_loc, 1))
            break;

        virt_loc = reg->table->current_cursor_loc;
        if (gnc_table_move_tab (reg->table, &virt_loc, TRUE))
            break;

        /* Deal with the exchange-rate */
        if (gnc_split_register_handle_exchange (reg, FALSE))
        {
            LEAVE("no exchange rate");
            return TRUE;
        }

        *p_new_virt_loc = reg->table->current_cursor_loc;
        (p_new_virt_loc->vcell_loc.virt_row)++;
        p_new_virt_loc->phys_row_offset = 0;
        p_new_virt_loc->phys_col_offset = 0;

        info->traverse_to_new = TRUE;

        LEAVE("off end of last line");
        return FALSE;

    }
    while (FALSE);

    /* Now see if we are changing cursors. If not, we may be able to
     * auto-complete. */
    if (!gnc_table_virtual_cell_out_of_bounds (reg->table, virt_loc.vcell_loc))
    {
        if (gnc_split_register_auto_completion (reg, dir, p_new_virt_loc))
        {
            info->auto_complete = TRUE;
            LEAVE("auto-complete");
            return FALSE;
        }
    }

    /* See if we are tabbing off the end of a blank split */
    do
    {
        VirtualLocation virt_loc;
        int old_virt_row;

        if (!changed)
            break;

        if (split)
            break;

        if (dir != GNC_TABLE_TRAVERSE_RIGHT)
            break;

        virt_loc = reg->table->current_cursor_loc;
        old_virt_row = virt_loc.vcell_loc.virt_row;

        if (gnc_table_move_tab (reg->table, &virt_loc, TRUE) &&
                old_virt_row == virt_loc.vcell_loc.virt_row)
            break;

        /* If we are here, then: (a) the current cursor has been
         * edited, and (b) we are on the blank split of a multi-line
         * transaction, and (c) we are tabbing out of the last cell
         * on the line. Thus, we want to go ahead and add the new
         * split and end up on the new blank split of the current
         * transaction. */

        /* Deal with the exchange-rate */
        if (gnc_split_register_handle_exchange (reg, FALSE))
        {
            LEAVE("no exchange rate");
            return TRUE;
        }

        info->cursor_hint_trans = trans;
        info->cursor_hint_split = split;
        info->cursor_hint_trans_split =
            gnc_split_register_get_current_trans_split (reg, NULL);
        info->cursor_hint_cursor_class = CURSOR_CLASS_SPLIT;
        info->hint_set_by_traverse = TRUE;

        LEAVE("off end of blank split");
        return FALSE;

    }
    while (FALSE);

    {
        int old_virt_row = reg->table->current_cursor_loc.vcell_loc.virt_row;

        /* Check for going off the end */
        gnc_table_find_close_valid_cell (reg->table, &virt_loc,
                                         info->exact_traversal);


        /* Did we change vertical position? */
        if (virt_loc.vcell_loc.virt_row != old_virt_row)
            /* Deal with the exchange-rate */
            if (gnc_split_register_handle_exchange (reg, FALSE))
            {
                LEAVE("no exchange rate");
                return TRUE;
            }
    }


    /* Same transaction, no problem */
    new_trans = gnc_split_register_get_trans (reg, virt_loc.vcell_loc);
    if (trans == new_trans)
    {
        *p_new_virt_loc = virt_loc;
        {
            LEAVE("staying within txn");
            return FALSE;
        }
    }

    /* Ok, we are changing transactions and the current transaction has
     * changed. See what the user wants to do. */
    LEAVE("txn change");
    return transaction_changed_confirm(p_new_virt_loc, &virt_loc, reg,
                                       new_trans, info->exact_traversal);
}

TableControl *
gnc_split_register_control_new (void)
{
    TableControl *control = gnc_table_control_new();

    control->move_cursor = gnc_split_register_move_cursor;
    control->traverse = gnc_split_register_traverse;

    return control;
}

gboolean
gnc_split_register_recn_cell_confirm (char old_flag, gpointer data)
{
    SplitRegister *reg = data;
    GtkWidget *dialog, *window;
    gint response;
    const gchar *title = _("Mark split as unreconciled?");
    const gchar *message =
        _("You are about to mark a reconciled split as unreconciled. Doing "
          "so might make future reconciliation difficult! Continue "
          "with this change?");

    if (old_flag != YREC)
        return TRUE;

    /* Does the user want to be warned? */
    window = gnc_split_register_get_parent(reg);
    dialog =
        gtk_message_dialog_new(GTK_WINDOW(window),
                               GTK_DIALOG_DESTROY_WITH_PARENT,
                               GTK_MESSAGE_WARNING,
                               GTK_BUTTONS_CANCEL,
                               "%s", title);
    gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
            "%s", message);
    gtk_dialog_add_button(GTK_DIALOG(dialog), _("_Unreconcile"),
                          GTK_RESPONSE_YES);
    response = gnc_dialog_run(GTK_DIALOG(dialog), GNC_PREF_WARN_REG_RECD_SPLIT_UNREC);
    gtk_widget_destroy(dialog);
    return (response == GTK_RESPONSE_YES);
}
