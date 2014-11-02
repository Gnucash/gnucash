/********************************************************************\
 * split-register-model.c -- split register model object            *
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

#include "datecell.h"
#include "dialog-utils.h"
#include "gnc-engine.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnome-utils/gnc-warnings.h"
#include "pricecell.h"
#include "recncell.h"
#include "split-register.h"
#include "split-register-model.h"
#include "split-register-model-save.h"
#include "split-register-p.h"
#include "engine-helpers.h"

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_LEDGER;

/* Flag for determining colorization of negative amounts. */
static gboolean use_red_for_negative = TRUE;

/* This returns the balance at runtime of a register at the split defined by virt_loc regardless of
 * sort order. It always assumes that the first txn in the register is starting from a 0 balance.
 * If gboolean subaccounts is TRUE, then it will return the total balance of the parent account
 * and all its subaccounts. FALSE will return the balance of just the parent account of the register. */
static gnc_numeric
gnc_split_register_get_rbaln (VirtualLocation virt_loc, gpointer user_data, gboolean subaccounts)
{
    SplitRegister *reg = user_data;
    Split *split;
    SRInfo *info = gnc_split_register_get_info (reg);
    gnc_numeric balance = gnc_numeric_zero();
    Account *account = NULL;
    Transaction *trans;
    GList *node, *child;
    GList *children = NULL;
    int i, row;

    balance = gnc_numeric_zero();

    /* Return NULL if this is a blank transaction. */
    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    if (split == xaccSplitLookup (&info->blank_split_guid,
                                  gnc_get_current_book ()))
        return gnc_numeric_zero();

    trans = xaccSplitGetParent (split);
    if (!trans)
        return gnc_numeric_zero();

    /* Get a list of accounts for matching */
    account = gnc_split_register_get_default_account(reg);
    if (!account)
        /* Register has no account (perhaps general ledger) so it has no
           well defined balance, return zero. */
        return balance;

    if (subaccounts)
    {
        children = gnc_account_get_descendants(account);
        children = g_list_append(children, account);
    }

    /* Get the row number we're on, then start with the first row. */
    row = virt_loc.vcell_loc.virt_row;
    virt_loc.vcell_loc.virt_row = 0;

    while (virt_loc.vcell_loc.virt_row <= row )
    {
        /* Get new temporary split and its parent transaction */
        split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
        trans = xaccSplitGetParent (split);

        i = 1;
        for (node = xaccTransGetSplitList (trans); node; node = node->next)
        {
            Split *secondary = node->data;
            i++;

            if (subaccounts)
            {
                /* Add up the splits that belong to the transaction if they are
                 * from the lead account or one of the subaccounts. */
                account = xaccSplitGetAccount (secondary);

                for (child = children; child; child = child->next)
                {
                    if (account == child->data)
                    {
                        balance = gnc_numeric_add_fixed(balance, xaccSplitGetAmount(secondary));
                        break;
                    }
                }
            }
            else
            {
                if ( account == xaccSplitGetAccount(secondary) )
                    balance = gnc_numeric_add_fixed( balance, xaccSplitGetAmount(secondary) );
            }
        }
        virt_loc.vcell_loc.virt_row += i;
    }

    if (subaccounts)
        g_list_free(children);

    return balance;
}

static gnc_commodity *
gnc_split_register_get_split_commodity (SplitRegister *reg,
                                        VirtualLocation virt_loc)
{
    CursorClass cursor_class;
    Account *account;
    Split *split;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    if (!split)
        return NULL;

    cursor_class = gnc_split_register_get_cursor_class (reg,
                   virt_loc.vcell_loc);
    if (cursor_class != CURSOR_CLASS_SPLIT)
        return NULL;

    account = NULL;

    if (virt_cell_loc_equal (virt_loc.vcell_loc,
                             reg->table->current_cursor_loc.vcell_loc) &&
            gnc_table_layout_get_cell_changed (reg->table->layout, XFRM_CELL, FALSE))
    {
        const char *name;

        name = gnc_table_layout_get_cell_value (reg->table->layout, XFRM_CELL);
        account = gnc_account_lookup_for_register (gnc_get_current_root_account (), name);
    }

    if (!account)
        account = xaccSplitGetAccount (split);

    if (!account)
        return NULL;
        
    return xaccAccountGetCommodity(account);
}

static gboolean
gnc_split_register_use_security_cells (SplitRegister *reg,
                                       VirtualLocation virt_loc)
{
    CursorClass cursor_class;
    Account *account;
    Split *split;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    if (!split)
        return TRUE;

    cursor_class = gnc_split_register_get_cursor_class (reg,
                   virt_loc.vcell_loc);
    if (cursor_class != CURSOR_CLASS_SPLIT)
        return TRUE;

    account = NULL;

    if (virt_cell_loc_equal (virt_loc.vcell_loc,
                             reg->table->current_cursor_loc.vcell_loc) &&
            gnc_table_layout_get_cell_changed (reg->table->layout, XFRM_CELL, FALSE))
    {
        const char *name;

        name = gnc_table_layout_get_cell_value (reg->table->layout, XFRM_CELL);
        account = gnc_account_lookup_for_register (gnc_get_current_root_account (), name);
    }

    if (!account)
        account = xaccSplitGetAccount (split);

    if (!account)
        return TRUE;

    if (xaccTransUseTradingAccounts (xaccSplitGetParent (split)))
    {
        gnc_commodity *commod = xaccAccountGetCommodity(account);
        if (!gnc_commodity_is_iso(commod) ||
            !gnc_commodity_equal(commod, xaccTransGetCurrency(xaccSplitGetParent(split))))
            return TRUE;
    }

    return xaccAccountIsPriced(account);
}

static const char *
gnc_split_register_get_date_label (VirtualLocation virt_loc,
                                   gpointer user_data)
{
    return _("Date");
}

static const char *
gnc_split_register_get_due_date_label (VirtualLocation virt_loc,
                                       gpointer user_data)
{
    return _("Due Date");
}

static const char *
gnc_split_register_get_num_label (VirtualLocation virt_loc,
                                  gpointer user_data)
{
    SplitRegister *reg = user_data;

    switch (reg->type)
    {
    case RECEIVABLE_REGISTER:
    case PAYABLE_REGISTER:
        return _("Ref");
    default:
        return _("Num");
    }
}

static const char *
gnc_split_register_get_tran_num_label (VirtualLocation virt_loc,
                                  gpointer user_data)
{
    SplitRegister *reg = user_data;

    switch (reg->type)
    {
    case RECEIVABLE_REGISTER:
    case PAYABLE_REGISTER:
        return _("T-Ref");
    case GENERAL_LEDGER:
    case INCOME_LEDGER:
    case SEARCH_LEDGER:
    {
        if (reg->use_tran_num_for_num_field)
            return _("Num");
    }
    default:
        return _("T-Num");
    }
}

static const char *
gnc_split_register_get_desc_label (VirtualLocation virt_loc,
                                   gpointer user_data)
{
    SplitRegister *reg = user_data;

    switch (reg->type)
    {
    case RECEIVABLE_REGISTER:
        return _("Customer");
    case PAYABLE_REGISTER:
        return _("Vendor");
    default:
        return _("Description");
    }
}

static const char *
gnc_split_register_get_recn_label (VirtualLocation virt_loc,
                                   gpointer user_data)
{
    SplitRegister *reg = user_data;

    switch (reg->type)
    {
    case RECEIVABLE_REGISTER:
    case PAYABLE_REGISTER:
        return _("Paid");

    default:
        return _("Reconciled:R") + 11;
    }
}

static const char *
gnc_split_register_get_baln_label (VirtualLocation virt_loc,
                                   gpointer user_data)
{
    return _("Balance");
}

static const char *
gnc_split_register_get_action_label (VirtualLocation virt_loc,
                                     gpointer user_data)
{
    return _("Action");
}

static const char *
gnc_split_register_get_xfrm_label (VirtualLocation virt_loc,
                                   gpointer user_data)
{
    return _("Account");
}

static const char *
gnc_split_register_get_mxfrm_label (VirtualLocation virt_loc,
                                    gpointer user_data)
{
    return _("Transfer");
}

static const char *
gnc_split_register_get_memo_label (VirtualLocation virt_loc,
                                   gpointer user_data)
{
    return _("Memo");
}

static const char *
gnc_split_register_get_type_label (VirtualLocation virt_loc,
                                   gpointer user_data)
{
    return _("Type");
}

static const char *
gnc_split_register_get_debit_label (VirtualLocation virt_loc,
                                    gpointer user_data)
{
    SplitRegister *reg = user_data;

    return gnc_split_register_get_debit_string (reg);
}

static const char *
gnc_split_register_get_credit_label (VirtualLocation virt_loc,
                                     gpointer user_data)
{
    SplitRegister *reg = user_data;

    return gnc_split_register_get_credit_string (reg);
}

static const char *
gnc_split_register_get_price_label (VirtualLocation virt_loc,
                                    gpointer user_data)
{
    SplitRegister *reg = user_data;
    gnc_commodity *commod;

    if (!gnc_split_register_use_security_cells (reg, virt_loc))
        return NULL;

    commod = gnc_split_register_get_split_commodity (reg, virt_loc);
    if (!commod || !gnc_commodity_is_iso(commod))
        return _("Price");
    else
        return _("Exch. Rate");
}

static const char *
gnc_split_register_get_shares_label (VirtualLocation virt_loc,
                                     gpointer user_data)
{
    SplitRegister *reg = user_data;
    gnc_commodity *commod;

    if (!gnc_split_register_use_security_cells (reg, virt_loc))
        return NULL;

    commod = gnc_split_register_get_split_commodity (reg, virt_loc);
    if (!commod || !gnc_commodity_is_iso(commod))
        return _("Shares");
    else
        return _("Oth. Curr.");
}

static const char *
gnc_split_register_get_tcredit_label (VirtualLocation virt_loc,
                                      gpointer user_data)
{
    SplitRegister *reg = user_data;
    SRInfo *info = gnc_split_register_get_info (reg);

    if (info->tcredit_str)
        return info->tcredit_str;

    {
        const char *string = gnc_split_register_get_credit_string (reg);

        if (string)
            info->tcredit_str = g_strdup_printf (_("Tot %s"), string);
    }

    if (info->tcredit_str)
        return info->tcredit_str;

    info->tcredit_str = g_strdup (_("Tot Credit"));

    return info->tcredit_str;
}

static const char *
gnc_split_register_get_tdebit_label (VirtualLocation virt_loc,
                                     gpointer user_data)
{
    SplitRegister *reg = user_data;
    SRInfo *info = gnc_split_register_get_info (reg);

    if (info->tdebit_str)
        return info->tdebit_str;

    {
        const char *string = gnc_split_register_get_debit_string (reg);
        if (string)
            info->tdebit_str = g_strdup_printf (_("Tot %s"), string);
    }

    if (info->tdebit_str)
        return info->tdebit_str;

    info->tdebit_str = g_strdup (_("Tot Debit"));

    return info->tdebit_str;
}

static const char *
gnc_split_register_get_tshares_label (VirtualLocation virt_loc,
                                      gpointer user_data)
{
    return _("Tot Shares");
}

static const char *
gnc_split_register_get_tbalance_label (VirtualLocation virt_loc,
                                       gpointer user_data)
{
    return _("Balance");
}

static const char *
gnc_split_register_get_notes_label (VirtualLocation virt_loc,
                                    gpointer user_data)
{
    return _("Notes");
}

static const char *
gnc_split_register_get_fdebit_label (VirtualLocation virt_loc,
                                     gpointer user_data)
{
    return _("Debit Formula");
}

static const char *
gnc_split_register_get_fcredit_label (VirtualLocation virt_loc,
                                      gpointer user_data)
{
    return _("Credit Formula");
}

static gnc_numeric
get_trans_total_amount (SplitRegister *reg, Transaction *trans)
{
    Account *account = gnc_split_register_get_default_account (reg);
    return xaccTransGetAccountAmount(trans, account);
}

static gnc_numeric
get_trans_total_balance (SplitRegister *reg, Transaction *trans)
{
    Account *account;

    account = gnc_split_register_get_default_account (reg);
    if (!trans || !account) return gnc_numeric_zero();

    return xaccTransGetAccountBalance(trans, account);
}

static guint32
gnc_split_register_get_color_internal (VirtualLocation virt_loc,
                                       SplitRegister *reg,
                                       const guint32 *color_table,
                                       gboolean foreground)
{
    const char *cursor_name;
    VirtualCell *vcell;
    gboolean is_current;
    gboolean double_alternate_virt;
    guint32 colorbase = 0; /* By default return background colors */

    if (foreground)
        colorbase = COLOR_UNKNOWN_FG; /* a bit of enum arithmetic */

    if (!reg)
        return color_table[colorbase + COLOR_UNKNOWN_BG];

    if (gnc_table_virtual_location_in_header (reg->table, virt_loc))
        return color_table[colorbase + COLOR_HEADER_BG];

    vcell = gnc_table_get_virtual_cell (reg->table, virt_loc.vcell_loc);
    if (!vcell || !vcell->cellblock)
        return color_table[colorbase + COLOR_UNKNOWN_BG];

    if ((virt_loc.phys_col_offset < vcell->cellblock->start_col) ||
            (virt_loc.phys_col_offset > vcell->cellblock->stop_col))
        return color_table[colorbase + COLOR_UNKNOWN_BG];

    is_current = virt_cell_loc_equal (reg->table->current_cursor_loc.vcell_loc,
                                      virt_loc.vcell_loc);

    cursor_name = vcell->cellblock->cursor_name;

    if (g_strcmp0 (cursor_name, CURSOR_SINGLE_JOURNAL) == 0 ||
            g_strcmp0 (cursor_name, CURSOR_SINGLE_LEDGER) == 0)
    {
        if (is_current)
            return vcell->start_primary_color ?
                    color_table[colorbase + COLOR_PRIMARY_BG_ACTIVE] :
                    color_table[colorbase + COLOR_SECONDARY_BG_ACTIVE];

        return vcell->start_primary_color ?
                color_table[colorbase + COLOR_PRIMARY_BG] : color_table[colorbase + COLOR_SECONDARY_BG];
    }

    if (g_strcmp0 (cursor_name, CURSOR_DOUBLE_JOURNAL) == 0 ||
            g_strcmp0 (cursor_name, CURSOR_DOUBLE_JOURNAL_NUM_ACTN) == 0 ||
            g_strcmp0 (cursor_name, CURSOR_DOUBLE_LEDGER) == 0 ||
            g_strcmp0 (cursor_name, CURSOR_DOUBLE_LEDGER_NUM_ACTN) == 0)
    {
        double_alternate_virt = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                                    GNC_PREF_ALT_COLOR_BY_TRANS);
        if (is_current)
        {
            if (double_alternate_virt)
                return vcell->start_primary_color ?
                        color_table[colorbase + COLOR_PRIMARY_BG_ACTIVE] :
                        color_table[colorbase + COLOR_SECONDARY_BG_ACTIVE];

            return (virt_loc.phys_row_offset % 2 == 0) ?
                    color_table[colorbase + COLOR_PRIMARY_BG_ACTIVE] :
                    color_table[colorbase + COLOR_SECONDARY_BG_ACTIVE];
        }

        if (double_alternate_virt)
            return vcell->start_primary_color ?
                    color_table[colorbase + COLOR_PRIMARY_BG] :
                    color_table[colorbase + COLOR_SECONDARY_BG];

        return (virt_loc.phys_row_offset % 2 == 0) ?
                color_table[colorbase + COLOR_PRIMARY_BG] :
                color_table[colorbase + COLOR_SECONDARY_BG];
    }

    if (g_strcmp0 (cursor_name, CURSOR_SPLIT) == 0)
    {
        if (is_current)
            return color_table[colorbase + COLOR_SPLIT_BG_ACTIVE];

        return color_table[colorbase + COLOR_SPLIT_BG];
    }

    PWARN ("Unexpected cursor: %s\n", cursor_name);

    return color_table[colorbase + COLOR_UNKNOWN_BG];
}

static guint32
gnc_split_register_get_fg_color_internal (VirtualLocation virt_loc,
                                          SplitRegister *reg,
                                          const guint32 *color_table)
{
    const guint32 red_color = color_table[COLOR_NEGATIVE];
    guint32 fg_color;
    const char * cell_name;
    gboolean is_current;
    gnc_numeric value;
    Split *split;

    fg_color = gnc_split_register_get_color_internal (virt_loc, reg, color_table, TRUE);

    if (!use_red_for_negative)
        return fg_color;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    if (!split)
        return fg_color;

    cell_name = gnc_table_get_cell_name (reg->table, virt_loc);

    if (gnc_cell_name_equal (cell_name, TSHRS_CELL))
        value = get_trans_total_amount (reg, xaccSplitGetParent (split));
    else if (gnc_cell_name_equal (cell_name, SHRS_CELL))
    {
        if (virt_cell_loc_equal (reg->table->current_cursor_loc.vcell_loc,
                                      virt_loc.vcell_loc))
            value = gnc_price_cell_get_value
                     ((PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                             SHRS_CELL));
        else
            value = xaccSplitGetAmount (split);
    }
    else if (gnc_cell_name_equal (cell_name, BALN_CELL))
        value = xaccSplitGetBalance (split);
    else if (gnc_cell_name_equal (cell_name, RBALN_CELL))
        value = gnc_split_register_get_rbaln (virt_loc, reg, TRUE);
    else if (gnc_cell_name_equal (cell_name, TBALN_CELL))
        value = get_trans_total_balance (reg, xaccSplitGetParent (split));

    if ((gnc_cell_name_equal (cell_name, BALN_CELL)) ||
            (gnc_cell_name_equal (cell_name, RBALN_CELL)) ||
            (gnc_cell_name_equal (cell_name, TBALN_CELL)))
        {
            Account *account = xaccSplitGetAccount (split);
            if (gnc_reverse_balance (account))
                value = gnc_numeric_neg (value);
        }

    if (gnc_numeric_negative_p (value))
        return red_color;

    return fg_color;
}

static guint32
gnc_split_register_get_fg_color (VirtualLocation virt_loc,
                                 gpointer user_data)
{
    SplitRegister *reg = user_data;
    return gnc_split_register_get_fg_color_internal (virt_loc, reg, reg_colors_default);
}

static guint32
gnc_split_register_get_gtkrc_fg_color (VirtualLocation virt_loc,
                                       gpointer user_data)
{
    SplitRegister *reg = user_data;
    return gnc_split_register_get_fg_color_internal (virt_loc, reg, reg_colors_gtkrc);
}

static guint32
gnc_split_register_get_bg_color (VirtualLocation virt_loc,
        gboolean *hatching,
        gpointer user_data)
{
    SplitRegister *reg = user_data;

    if (hatching)
        *hatching = FALSE;

    return gnc_split_register_get_color_internal (virt_loc, reg, reg_colors_default, FALSE);
}


static RegisterColor
gnc_split_register_get_gtkrc_bg_color (VirtualLocation virt_loc,
                                       gboolean *hatching,
                                       gpointer user_data)
{
    SplitRegister *reg = user_data;

    if (hatching)
        *hatching = FALSE;

    return gnc_split_register_get_color_internal (virt_loc, reg, reg_colors_gtkrc, FALSE);
}

static guint32
gnc_split_register_get_debcred_bg_color (VirtualLocation virt_loc,
        gboolean *hatching,
        gpointer user_data)
{
    SplitRegister *reg = user_data;

    if (hatching)
    {
        Transaction *trans;

        trans = gnc_split_register_get_trans (reg, virt_loc.vcell_loc);

        if (trans)
            *hatching = !xaccTransIsBalanced (trans);
        else
            *hatching = FALSE;
    }

    return gnc_split_register_get_bg_color (virt_loc, NULL, user_data);
}

static void
gnc_split_register_get_border (VirtualLocation virt_loc,
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

    cursor_class =
        gnc_split_register_cursor_name_to_class (vcell->cellblock->cursor_name);

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

static const char *
gnc_split_register_get_type_entry (VirtualLocation virt_loc,
                                   gboolean translate,
                                   gboolean *conditionally_changed,
                                   gpointer user_data)
{
    SplitRegister *reg = user_data;
    Transaction *trans;
    char type;
    static char s[2];

    trans = gnc_split_register_get_trans (reg, virt_loc.vcell_loc);
    if (!trans)
        return NULL;

    type = xaccTransGetTxnType (trans);

    if (type == TXN_TYPE_NONE)
        type = '?';

    s[0] = type;
    s[1] = '\0';

    return s;
}

static char
gnc_split_register_get_type_value (SplitRegister *reg,
                                   VirtualLocation virt_loc)
{
    RecnCell *cell;

    cell = (RecnCell *)gnc_table_layout_get_cell (reg->table->layout, TYPE_CELL);
    if (!cell)
        return '\0';

    return gnc_recn_cell_get_flag (cell);
}

static const char *
gnc_split_register_get_due_date_entry (VirtualLocation virt_loc,
                                       gboolean translate,
                                       gboolean *conditionally_changed,
                                       gpointer user_data)
{
    SplitRegister *reg = user_data;
    Transaction *trans;
    Split *split;
    Timespec ts;
    gboolean is_current;
    char type;

    is_current = virt_cell_loc_equal (reg->table->current_cursor_loc.vcell_loc,
                                      virt_loc.vcell_loc);

    if (is_current)
    {
        type = gnc_split_register_get_type_value (reg, virt_loc);
    }
    else
    {
        const char *typestr =
            gnc_split_register_get_type_entry (virt_loc, translate,
                                               conditionally_changed, user_data);
        if (typestr != NULL)
            type = *typestr;
        else
            type = '\0';
    }

    /* Only print the due date for invoice transactions */
    if (type != TXN_TYPE_INVOICE)
    {
        //PWARN ("returning NULL due_date entry");
        return NULL;
    }

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    trans = xaccSplitGetParent (split);
    if (!trans)
    {
        //PWARN ("No transaction in due_date entry");
        return NULL;
    }

    xaccTransGetDateDueTS (trans, &ts);
    //PWARN ("returning valid due_date entry");

    return gnc_print_date (ts);
}

static const char *
gnc_split_register_get_date_entry (VirtualLocation virt_loc,
                                   gboolean translate,
                                   gboolean *conditionally_changed,
                                   gpointer user_data)
{
    SplitRegister *reg = user_data;
    Transaction *trans;
    Split *split;
    Timespec ts;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    trans = xaccSplitGetParent (split);
    if (!trans)
        return NULL;

    xaccTransGetDatePostedTS (trans, &ts);

    return gnc_print_date (ts);
}

static char *
gnc_split_register_get_date_help (VirtualLocation virt_loc,
                                  gpointer user_data)
{
    SplitRegister *reg = user_data;
    BasicCell *cell;
    char string[1024];
    GDate date;

    cell = gnc_table_get_cell (reg->table, virt_loc);
    if (!cell || !cell->value || *cell->value == '\0')
        return NULL;

    g_date_clear (&date, 1);
    gnc_date_cell_get_date_gdate ((DateCell *) cell, &date);

    g_date_strftime (string, sizeof (string), _("%A %d %B %Y"), &date);

    return g_strdup (string);
}

static const char *
gnc_split_register_get_inactive_date_entry (VirtualLocation virt_loc,
        gboolean translate,
        gboolean *conditionally_changed,
        gpointer user_data)
{
    /* This seems to be the one that initially gets used, the InactiveDateCell
       is set to, and subsequently displayed. */
    return _("Scheduled");
}

static const char *
gnc_split_register_get_num_entry (VirtualLocation virt_loc,
                                  gboolean translate,
                                  gboolean *conditionally_changed,
                                  gpointer user_data)
{
    SplitRegister *reg = user_data;
    Transaction *trans;
    Split *split;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    trans = xaccSplitGetParent (split);

    return gnc_get_num_action (trans, split); 
}

static const char *
gnc_split_register_get_tran_num_entry (VirtualLocation virt_loc,
                                  gboolean translate,
                                  gboolean *conditionally_changed,
                                  gpointer user_data)
{
    SplitRegister *reg = user_data;
    Transaction *trans;
    Split *split;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    trans = xaccSplitGetParent (split);

    return gnc_get_num_action (trans, NULL);
}

static char *
gnc_split_register_get_num_help (VirtualLocation virt_loc,
                                 gpointer user_data)
{
    SplitRegister *reg = user_data;
    const char *help;

    help = gnc_table_get_entry (reg->table, virt_loc);
    if (!help || *help == '\0')
        switch (reg->type)
        {
        case RECEIVABLE_REGISTER:
        case PAYABLE_REGISTER:
            help = reg->use_tran_num_for_num_field ?
                    _("Enter a reference, such as an invoice or check number, "
                        "common to all entry lines (splits)") :
                    _("Enter a reference, such as an invoice or check number, "
                        "unique to each entry line (split)");
            break;
        default:
            help = reg->use_tran_num_for_num_field ?
                    _("Enter a reference, such as a check number, "
                        "common to all entry lines (splits)") :
                    _("Enter a reference, such as a check number, "
                        "unique to each entry line (split)");
            break;
        }

    return g_strdup (help);
}

static char *
gnc_split_register_get_tran_num_help (VirtualLocation virt_loc,
                                 gpointer user_data)
{
    SplitRegister *reg = user_data;
    const char *help;

    help = gnc_table_get_entry (reg->table, virt_loc);
    if (!help || *help == '\0')
        switch (reg->type)
        {
        case RECEIVABLE_REGISTER:
        case PAYABLE_REGISTER:
            help = _("Enter a transaction reference, such as an invoice "
                    "or check number, common to all entry lines (splits)");
            break;
        default:
            help = _("Enter a transaction reference "
                    "that will be common to all entry lines (splits)");
            break;
        }

    return g_strdup (help);
}

static const char *
gnc_split_register_get_desc_entry (VirtualLocation virt_loc,
                                   gboolean translate,
                                   gboolean *conditionally_changed,
                                   gpointer user_data)
{
    SplitRegister *reg = user_data;
    Transaction *trans;
    Split *split;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    trans = xaccSplitGetParent (split);

    return xaccTransGetDescription (trans);
}

static char *
gnc_split_register_get_desc_help (VirtualLocation virt_loc,
                                  gpointer user_data)
{
    SplitRegister *reg = user_data;
    const char *help;

    help = gnc_table_get_entry (reg->table, virt_loc);
    if (!help || *help == '\0')
        switch (reg->type)
        {
        case RECEIVABLE_REGISTER:
            help = _("Enter the name of the Customer");
            break;
        case PAYABLE_REGISTER:
            help = _("Enter the name of the Vendor");
            break;
        default:
            help = _("Enter a description of the transaction");
            break;
        }
    return g_strdup (help);
}

static const char *
gnc_split_register_get_notes_entry (VirtualLocation virt_loc,
                                    gboolean translate,
                                    gboolean *conditionally_changed,
                                    gpointer user_data)
{
    SplitRegister *reg = user_data;
    Transaction *trans;
    Split *split;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    trans = xaccSplitGetParent (split);

    return xaccTransGetNotes (trans);
}

static char *
gnc_split_register_get_notes_help (VirtualLocation virt_loc,
                                   gpointer user_data)
{
    SplitRegister *reg = user_data;
    const char *help;

    help = gnc_table_get_entry (reg->table, virt_loc);
    if (!help || *help == '\0')
        help = _("Enter notes for the transaction");

    return g_strdup (help);
}

static const char *
gnc_split_register_get_vnotes_entry (VirtualLocation virt_loc,
                                     gboolean translate,
                                     gboolean *conditionally_changed,
                                     gpointer user_data)
{
    SplitRegister *reg = user_data;
    Transaction *trans;
    Split *split;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    trans = xaccSplitGetParent (split);

    if(trans == NULL)
        return NULL;
    else
        return xaccTransGetVoidReason(trans);
}

static char *
gnc_split_register_get_vnotes_help (VirtualLocation virt_loc,
                                    gpointer user_data)
{
    SplitRegister *reg = user_data;
    const char *help;

    help = gnc_table_get_entry (reg->table, virt_loc);
    if (!help || *help == '\0')
        help = _("Reason the transaction was voided");

    return g_strdup (help);
}

static const char *
gnc_split_register_get_rate_entry (VirtualLocation virt_loc,
                                   gboolean translate,
                                   gboolean *conditionally_changed,
                                   gpointer user_data)
{
    SplitRegister *reg = user_data;
    Split *split, *osplit;
    Transaction *txn;
    gnc_numeric amount, value, convrate;
    SRInfo *info = gnc_split_register_get_info (reg);

    if (info->rate_reset == RATE_RESET_REQD && info->auto_complete)
        return "0";
        
    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    if (!split)
        return NULL;

    /* If this is a basic, non-expanded ledger with exactly two splits,
     * and split->txn->curr == split->acc->comm, then use the OTHER
     * split for the rate.
     */
    osplit = xaccSplitGetOtherSplit (split);
    txn = gnc_split_register_get_trans (reg, virt_loc.vcell_loc);

    if (!gnc_split_register_current_trans_expanded (reg) && osplit &&
            !gnc_split_register_needs_conv_rate(reg, txn,
                    xaccSplitGetAccount(split)))
    {
        split = osplit;
    }

    amount = xaccSplitGetAmount (split);
    value = xaccSplitGetValue (split);

    if (gnc_numeric_zero_p (value))
        return "0";

    convrate = gnc_numeric_div (amount, value, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);

    return xaccPrintAmount (convrate, gnc_default_price_print_info ());
}

static const char *
gnc_split_register_get_recn_entry (VirtualLocation virt_loc,
                                   gboolean translate,
                                   gboolean *conditionally_changed,
                                   gpointer user_data)
{
    SplitRegister *reg = user_data;
    Split *split;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    if (!split)
        return NULL;

    if (translate)
        return gnc_get_reconcile_str (xaccSplitGetReconcile (split));
    else
    {
        static char s[2];

        s[0] = xaccSplitGetReconcile (split);
        s[1] = '\0';

        return s;
    }
}

static const char *
gnc_split_register_get_action_entry (VirtualLocation virt_loc,
                                     gboolean translate,
                                     gboolean *conditionally_changed,
                                     gpointer user_data)
{
    SplitRegister *reg = user_data;
    Split *split = gnc_split_register_get_split(reg, virt_loc.vcell_loc);

    return gnc_get_num_action (NULL, split);
}

static char *
gnc_split_register_get_action_help (VirtualLocation virt_loc,
                                    gpointer user_data)
{
    SplitRegister *reg = user_data;
    const char *help;

    help = gnc_table_get_entry (reg->table, virt_loc);
    if (!help || *help == '\0')
        help = reg->use_tran_num_for_num_field ?
        _("Enter an action type, or choose one from the list") :
        _("Enter a reference number, such as the next check number, or choose an action type from the list");

    return g_strdup (help);
}

static const char *
gnc_split_register_get_memo_entry (VirtualLocation virt_loc,
                                   gboolean translate,
                                   gboolean *conditionally_changed,
                                   gpointer user_data)
{
    SplitRegister *reg = user_data;
    Split *split;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);

    return xaccSplitGetMemo (split);
}

static char *
gnc_split_register_get_memo_help (VirtualLocation virt_loc,
                                  gpointer user_data)
{
    SplitRegister *reg = user_data;
    const char *help;

    help = gnc_table_get_entry (reg->table, virt_loc);
    if (!help || *help == '\0')
        help = _("Enter a description of the split");
    return g_strdup (help);
}

static const char *
gnc_split_register_get_balance_entry (VirtualLocation virt_loc,
                                      gboolean translate,
                                      gboolean *conditionally_changed,
                                      gpointer user_data)
{
    SplitRegister *reg = user_data;
    SRInfo *info = gnc_split_register_get_info (reg);
    gnc_numeric balance;
    gboolean is_trans;
    Split *split;
    Account *account;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);

    if (split == xaccSplitLookup (&info->blank_split_guid,
                                  gnc_get_current_book ()))
        return NULL;

    is_trans = gnc_cell_name_equal
               (gnc_table_get_cell_name (reg->table, virt_loc), TBALN_CELL);

    if (is_trans)
        balance = get_trans_total_balance (reg, xaccSplitGetParent (split));
    else
        balance = xaccSplitGetBalance (split);

    account = xaccSplitGetAccount (split);
    if (!account)
        account = gnc_split_register_get_default_account (reg);

    if (gnc_reverse_balance (account))
        balance = gnc_numeric_neg (balance);

    return xaccPrintAmount (balance, gnc_account_print_info (account, FALSE));
}

static const char *
gnc_split_register_get_price_entry (VirtualLocation virt_loc,
                                    gboolean translate,
                                    gboolean *conditionally_changed,
                                    gpointer user_data)
{
    SplitRegister *reg = user_data;
    gnc_numeric price;
    Split *split;

    if (!gnc_split_register_use_security_cells (reg, virt_loc))
        return NULL;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);

    price = xaccSplitGetSharePrice (split);
    if (gnc_numeric_zero_p (price))
        return NULL;

    return xaccPrintAmount (price, gnc_default_price_print_info ());
}

static char *
gnc_split_register_get_price_help (VirtualLocation virt_loc,
                                   gpointer user_data)
{
    SplitRegister *reg = user_data;
    const char *help;

    help = gnc_table_get_entry (reg->table, virt_loc);
    if (!help || *help == '\0')
        help = _("Enter the effective share price");

    return g_strdup (help);
}

static const char *
gnc_split_register_get_shares_entry (VirtualLocation virt_loc,
                                     gboolean translate,
                                     gboolean *conditionally_changed,
                                     gpointer user_data)
{
    SplitRegister *reg = user_data;
    gnc_numeric shares;
    Split *split;

    if (!gnc_split_register_use_security_cells (reg, virt_loc))
        return NULL;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);

    shares = xaccSplitGetAmount (split);
    if (gnc_numeric_zero_p (shares))
        return NULL;

    return xaccPrintAmount (shares, gnc_split_amount_print_info (split, FALSE));
}

static char *
gnc_split_register_get_shares_help (VirtualLocation virt_loc,
                                    gpointer user_data)
{
    SplitRegister *reg = user_data;
    const char *help;

    help = gnc_table_get_entry (reg->table, virt_loc);
    if (!help || *help == '\0')
        help = _("Enter the number of shares bought or sold");

    return g_strdup (help);
}

static const char *
gnc_split_register_get_tshares_entry (VirtualLocation virt_loc,
                                      gboolean translate,
                                      gboolean *conditionally_changed,
                                      gpointer user_data)
{
    SplitRegister *reg = user_data;
    gnc_numeric total;
    Split *split;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);

    total = get_trans_total_amount (reg, xaccSplitGetParent (split));

    return xaccPrintAmount (total, gnc_split_amount_print_info (split, FALSE));
}

static const char *
gnc_split_register_get_xfrm_entry (VirtualLocation virt_loc,
                                   gboolean translate,
                                   gboolean *conditionally_changed,
                                   gpointer user_data)
{
    static char *name = NULL;

    SplitRegister *reg = user_data;
    Split *split;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);

    g_free (name);

    name = gnc_get_account_name_for_register (xaccSplitGetAccount (split));

    return name;
}

static char *
gnc_split_register_get_xfrm_help (VirtualLocation virt_loc,
                                  gpointer user_data)
{
    SplitRegister *reg = user_data;
    const char *help;

    help = gnc_table_get_entry (reg->table, virt_loc);
    if (!help || *help == '\0')
        help = _("Enter the account to transfer from, "
                 "or choose one from the list");

    return g_strdup (help);
}

static const char *
gnc_split_register_get_mxfrm_entry (VirtualLocation virt_loc,
                                    gboolean translate,
                                    gboolean *conditionally_changed,
                                    gpointer user_data)
{
    static char *name = NULL;

    SplitRegister *reg = user_data;
    Split *split;
    Split *s;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    if (!split)
        return NULL;

    s = xaccSplitGetOtherSplit (split);

    g_free (name);

    if (s)
        name = gnc_get_account_name_for_register (xaccSplitGetAccount (s));
    else
    {
        /* For multi-split transactions and stock splits,
         * use a special value. */
        s = xaccTransGetSplit (xaccSplitGetParent(split), 1);

        if (s)
            name = g_strdup (SPLIT_TRANS_STR);
        else if (g_strcmp0 ("stock-split", xaccSplitGetType (split)) == 0)
            name = g_strdup (STOCK_SPLIT_STR);
        else
            name = g_strdup ("");
    }

    return name;
}

static char *
gnc_split_register_get_mxfrm_help (VirtualLocation virt_loc,
                                   gpointer user_data)
{
    const char *help;

    SplitRegister *reg = user_data;
    Split *split;
    Split *s;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    if (!split)
        return NULL;

    s = xaccSplitGetOtherSplit (split);

    if (s)
    {
        help = gnc_split_register_get_mxfrm_entry (virt_loc, FALSE,
                NULL, user_data);
        if (!help || *help == '\0')
            help = _("Enter the account to transfer from, "
                     "or choose one from the list");
    }
    else
    {
        /* For multi-split transactions and stock splits,
         * use a special value. */
        s = xaccTransGetSplit (xaccSplitGetParent(split), 1);

        if (s)
            help = _("This transaction has multiple splits; "
                     "press the Split button to see them all");
        else if (g_strcmp0 ("stock-split", xaccSplitGetType (split)) == 0)
            help = _("This transaction is a stock split; "
                     "press the Split button to see details");
        else
            help = "";
    }

    return g_strdup (help);
}

/* Return the total amount of the transaction for splits of default account
 * and all subaccounts of the register. */
static gnc_numeric
get_trans_total_amount_subaccounts (SplitRegister *reg, Transaction *trans)
{
    GList *children, *child;
    Account *parent;
    gnc_numeric total = gnc_numeric_zero();

    /* Get a list of all subaccounts for matching */
    parent = gnc_split_register_get_default_account(reg);
    if (!parent)
        /* Register has no account, perhaps it's the general ledger.  If it
           has no account then we have no way of picking out the desired splits,
           return zero. */
        return total;
    children = gnc_account_get_descendants(parent);
    children = g_list_append(children, parent);

    for (child = children; child; child = child->next)
    {
        total = gnc_numeric_add_fixed(total, xaccTransGetAccountAmount(trans, child->data));
    }

    g_list_free(children);

    return total;
}

static const char *
gnc_split_register_get_tdebcred_entry (VirtualLocation virt_loc,
                                       gboolean translate,
                                       gboolean *conditionally_changed,
                                       gpointer user_data)
{
    SplitRegister *reg = user_data;
    const char * cell_name;
    gnc_numeric total;
    Split *split;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    if (!split)
        return NULL;

    cell_name = gnc_table_get_cell_name (reg->table, virt_loc);

    switch (reg->type)
    {
    case GENERAL_LEDGER:
    case INCOME_LEDGER:
        total = get_trans_total_amount_subaccounts (reg, xaccSplitGetParent (split));
        break;
    default:
        total = get_trans_total_amount (reg, xaccSplitGetParent (split));
        break;
    }

    if (gnc_numeric_zero_p (total))
        return NULL;

    if (gnc_numeric_negative_p (total) &&
            gnc_cell_name_equal (cell_name, TDEBT_CELL))
        return NULL;

    if (gnc_numeric_positive_p (total) &&
            gnc_cell_name_equal (cell_name, TCRED_CELL))
        return NULL;

    total = gnc_numeric_abs (total);

    return xaccPrintAmount (total, gnc_split_amount_print_info (split, FALSE));
}

/* return TRUE if we have a RATE_CELL; return FALSE if we do not.
 * (note: should match split-register-layout.c)
 */
gboolean
gnc_split_reg_has_rate_cell (SplitRegisterType type)
{
    switch (type)
    {
    case BANK_REGISTER:
    case CASH_REGISTER:
    case ASSET_REGISTER:
    case CREDIT_REGISTER:
    case LIABILITY_REGISTER:
    case INCOME_REGISTER:
    case EXPENSE_REGISTER:
    case EQUITY_REGISTER:
    case TRADING_REGISTER:
    case GENERAL_LEDGER:
    case INCOME_LEDGER:
    case SEARCH_LEDGER:
        return TRUE;

    case STOCK_REGISTER:
    case CURRENCY_REGISTER:
    case PORTFOLIO_LEDGER:
    case RECEIVABLE_REGISTER:
    case PAYABLE_REGISTER:
    default:
        return FALSE;
    }
}

/* returns TRUE if you need to convert the split's value to the local
 * (account) display currency.  Returns FALSE if you can just use the
 * split->value directly.
 */
gboolean
gnc_split_register_needs_conv_rate (SplitRegister *reg,
                                    Transaction *txn, Account *acc)
{
    gnc_commodity *txn_cur, *acc_com;

    /* If there is not a RATE_CELL, then don't do anything */
    if (!gnc_split_reg_has_rate_cell (reg->type))
        return FALSE;

    /* if txn->currency == acc->commodity, then return FALSE */
    acc_com = xaccAccountGetCommodity (acc);
    txn_cur = xaccTransGetCurrency (txn);
    if (txn_cur && acc_com && gnc_commodity_equal (txn_cur, acc_com))
        return FALSE;

    return TRUE;
}

static const char *
gnc_split_register_get_debcred_entry (VirtualLocation virt_loc,
                                      gboolean translate,
                                      gboolean *conditionally_changed,
                                      gpointer user_data)
{
    SplitRegister *reg = user_data;
    gboolean is_debit;
    Split *split;
    Transaction *trans;
    gnc_commodity *currency;

    is_debit = gnc_cell_name_equal
               (gnc_table_get_cell_name (reg->table, virt_loc), DEBT_CELL);

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    trans = gnc_split_register_get_trans (reg, virt_loc.vcell_loc);

    currency = xaccTransGetCurrency (trans);
    if (!currency)
        currency = gnc_default_currency ();

    if (!split)
    {
        gnc_numeric imbalance;
        Account *acc;

        imbalance = xaccTransGetImbalanceValue (trans);

        if (gnc_numeric_zero_p (imbalance))
            return NULL;

        if (xaccTransUseTradingAccounts (trans))
        {
            MonetaryList *imbal_list;
            gnc_monetary *imbal_mon;
            imbal_list = xaccTransGetImbalance (trans);

            if (!imbal_list)
            {
                /* No commodity imbalance, there shouldn't be a value imablance. */
                return NULL;
            }

            if (imbal_list->next)
            {
                /* Multiple currency imbalance. */
                gnc_monetary_list_free(imbal_list);
                return NULL;
            }

            imbal_mon = imbal_list->data;
            if (!gnc_commodity_equal(gnc_monetary_commodity(*imbal_mon), currency))
            {
                /* Imbalance is in wrong currency */
                gnc_monetary_list_free(imbal_list);
                return NULL;
            }

            if (!gnc_numeric_equal (gnc_monetary_value(*imbal_mon), imbalance))
            {
                /* Value and commodity imbalances differ */
                gnc_monetary_list_free(imbal_list);
                return NULL;
            }

            /* Done with the imbalance list */
            gnc_monetary_list_free(imbal_list);
        }

        imbalance = gnc_numeric_neg (imbalance);

        if (gnc_numeric_negative_p (imbalance) && is_debit)
            return NULL;

        if (gnc_numeric_positive_p (imbalance) && !is_debit)
            return NULL;

        if (conditionally_changed)
            *conditionally_changed = TRUE;

        imbalance = gnc_numeric_abs (imbalance);

        acc = gnc_split_register_get_default_account (reg);
        if (gnc_split_register_needs_conv_rate (reg, trans, acc))
        {
            imbalance = gnc_numeric_mul (imbalance,
                                         xaccTransGetAccountConvRate(trans, acc),
                                         gnc_commodity_get_fraction (currency),
                                         GNC_HOW_RND_ROUND_HALF_UP);
        }
        else
        {
            imbalance = gnc_numeric_convert (imbalance,
                                             gnc_commodity_get_fraction (currency),
                                             GNC_HOW_RND_ROUND_HALF_UP);
        }

        return xaccPrintAmount (imbalance, gnc_account_print_info (acc, FALSE));
    }

    {
        gnc_numeric amount;
        gnc_commodity *split_commodity;
        GNCPrintAmountInfo print_info;
        Account *account;
        gnc_commodity * commodity;

        account = gnc_split_register_get_default_account (reg);
        commodity = xaccAccountGetCommodity (account);
        split_commodity = xaccAccountGetCommodity(xaccSplitGetAccount(split));

        if (xaccTransUseTradingAccounts (trans))
        {
            gboolean use_symbol, is_current;
            is_current = virt_cell_loc_equal (reg->table->current_cursor_loc.vcell_loc,
                                              virt_loc.vcell_loc);

            if (reg->type == STOCK_REGISTER ||
                    reg->type == CURRENCY_REGISTER ||
                    reg->type == PORTFOLIO_LEDGER)
            {
                gnc_commodity *amount_commodity;
                /* security register.  If this split has price and shares columns,
                   use the value, otherwise use the amount.  */
                if (gnc_split_register_use_security_cells(reg, virt_loc))
                {
                    amount = xaccSplitGetValue(split);
                    amount_commodity = currency;
                }
                else
                {
                    amount = xaccSplitGetAmount(split);
                    amount_commodity = split_commodity;
                }
                /* Show the currency if it is not the default currency */
                if (is_current ||
                        gnc_commodity_equiv(amount_commodity, gnc_default_currency()))
                    use_symbol = FALSE;
                else
                    use_symbol = TRUE;
                print_info = gnc_commodity_print_info(amount_commodity, use_symbol);
            }
            else
            {
                /* non-security register, always use the split amount. */
                amount = xaccSplitGetAmount(split);
                if (is_current ||
                        gnc_commodity_equiv(split_commodity, commodity))
                    use_symbol = FALSE;
                else
                    use_symbol = TRUE;
                print_info = gnc_commodity_print_info(split_commodity, use_symbol);
            }
        }
        else
        {
            /* If this account is not a stock/mutual/currency account, and
            * currency != the account commodity, then use the SplitAmount
            * instead of the SplitValue.
            */
            switch (reg->type)
            {
            case STOCK_REGISTER:
            case CURRENCY_REGISTER:
            case PORTFOLIO_LEDGER:
                amount = xaccSplitGetValue (split);
                print_info = gnc_commodity_print_info (currency, FALSE);
                break;

            default:
                if (commodity && !gnc_commodity_equal (commodity, currency))
                    /* Convert this to the "local" value */
                    amount = xaccSplitConvertAmount(split, account);
                else
                    amount = xaccSplitGetValue (split);
                print_info = gnc_account_print_info (account, FALSE);
                break;
            }
        }

        if (gnc_numeric_zero_p (amount))
            return NULL;

        if (gnc_numeric_negative_p (amount) && is_debit)
            return NULL;

        if (gnc_numeric_positive_p (amount) && !is_debit)
            return NULL;

        amount = gnc_numeric_abs (amount);

        return xaccPrintAmount (amount, print_info);
    }
}

/* Calculates the register balance for each split at runtime.
 * This works regardless of the sort order. */
static const char *
gnc_split_register_get_rbaln_entry (VirtualLocation virt_loc,
                                    gboolean translate,
                                    gboolean *conditionally_changed,
                                    gpointer user_data)
{
    SplitRegister *reg = user_data;
    SRInfo *info = gnc_split_register_get_info (reg);
    Split *split;
    Transaction *trans;
    gnc_numeric balance;
    Account *account;

    /* Return NULL if this is a blank transaction. */
    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    if (split == xaccSplitLookup (&info->blank_split_guid,
                                  gnc_get_current_book ()))
        return NULL;

    trans = xaccSplitGetParent (split);
    if (!trans)
        return NULL;

    balance = gnc_split_register_get_rbaln (virt_loc, user_data, TRUE);

    account = xaccSplitGetAccount (split);
    if (!account)
        account = gnc_split_register_get_default_account (reg);

    if (gnc_reverse_balance (account))
        balance = gnc_numeric_neg (balance);

    return xaccPrintAmount (balance, gnc_account_print_info (account, FALSE));
}

static gboolean
gnc_split_register_cursor_is_readonly (VirtualLocation virt_loc,
                                       gpointer user_data)
{
    SplitRegister *reg = user_data;
    Split *split;
    Transaction *txn;
    char type;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    if (!split) return FALSE;

    txn = xaccSplitGetParent (split);
    if (!txn) return FALSE;

    if (xaccTransGetReadOnly(txn)
            || xaccTransIsReadonlyByPostedDate(txn))
        return(TRUE);

    type = xaccTransGetTxnType (txn);
    return (type == TXN_TYPE_INVOICE);
}

static CellIOFlags
gnc_split_register_get_inactive_io_flags (VirtualLocation virt_loc,
        gpointer user_data)
{
    if (gnc_split_register_cursor_is_readonly (virt_loc, user_data))
        return XACC_CELL_ALLOW_READ_ONLY;

    return XACC_CELL_ALLOW_NONE;
}

static CellIOFlags
gnc_split_register_get_standard_io_flags (VirtualLocation virt_loc,
        gpointer user_data)
{
    if (gnc_split_register_cursor_is_readonly (virt_loc, user_data))
        return XACC_CELL_ALLOW_READ_ONLY;

    return XACC_CELL_ALLOW_ALL;
}

static CellIOFlags
gnc_split_register_get_recn_io_flags (VirtualLocation virt_loc,
                                      gpointer user_data)
{
    if (gnc_split_register_cursor_is_readonly (virt_loc, user_data))
        return XACC_CELL_ALLOW_READ_ONLY;

    return XACC_CELL_ALLOW_ALL | XACC_CELL_ALLOW_EXACT_ONLY;
}

static CellIOFlags
gnc_split_register_get_ddue_io_flags (VirtualLocation virt_loc,
                                      gpointer user_data)
{
    SplitRegister *reg = user_data;
    char type;

    type = gnc_split_register_get_type_value (reg, virt_loc);

    /* Only print the due date for invoice transactions */
    if (type != TXN_TYPE_INVOICE)
    {
        return XACC_CELL_ALLOW_NONE;
    }

    return XACC_CELL_ALLOW_READ_ONLY;
}

static CellIOFlags
gnc_split_register_get_rate_io_flags (VirtualLocation virt_loc,
                                      gpointer user_data)
{
    return XACC_CELL_ALLOW_SHADOW;
}

static CellIOFlags
gnc_split_register_get_debcred_io_flags (VirtualLocation virt_loc,
        gpointer user_data)
{
    SplitRegister *reg = user_data;
    Split *split;

    if (gnc_split_register_cursor_is_readonly (virt_loc, user_data))
        return XACC_CELL_ALLOW_READ_ONLY;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);

    if (g_strcmp0 ("stock-split", xaccSplitGetType (split)) == 0)
        return XACC_CELL_ALLOW_NONE;

    return XACC_CELL_ALLOW_ALL;
}

static CellIOFlags
gnc_split_register_get_security_io_flags (VirtualLocation virt_loc,
        gpointer user_data)
{
    SplitRegister *reg = user_data;

    if (gnc_split_register_cursor_is_readonly (virt_loc, user_data))
        return XACC_CELL_ALLOW_READ_ONLY;

    if (gnc_split_register_use_security_cells (reg, virt_loc))
        return XACC_CELL_ALLOW_ALL;

    return XACC_CELL_ALLOW_SHADOW;
}

static gboolean
xaccTransWarnReadOnly (const Transaction *trans)
{
    GtkWidget *dialog;
    const gchar *reason;
    const gchar *format =
        _("Cannot modify or delete this transaction. This transaction is "
          "marked read-only because:\n\n'%s'");

    if (!trans) return FALSE;

    reason = xaccTransGetReadOnly (trans);
    if (reason)
    {
        dialog = gtk_message_dialog_new(NULL,
                                        0,
                                        GTK_MESSAGE_ERROR,
                                        GTK_BUTTONS_OK,
                                        format,
                                        reason);
        gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);
        return TRUE;
    }
    return FALSE;
}


static gboolean
gnc_split_register_confirm (VirtualLocation virt_loc, gpointer user_data)
{
    SplitRegister *reg = user_data;
    SRInfo *info = gnc_split_register_get_info (reg);
    Transaction *trans;
    Split *split;
    char recn;
    const char *cell_name;
    gboolean change_ok;

    /* This assumes we reset the flag whenever we change splits.
     * This happens in gnc_split_register_move_cursor(). */
    if (info->change_confirmed)
        return TRUE;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    if (!split)
        return TRUE;

    trans = xaccSplitGetParent (split);
    if (xaccTransWarnReadOnly(trans))
        return FALSE;
    if (gnc_table_layout_get_cell_changed (reg->table->layout, RECN_CELL, FALSE))
        recn = gnc_recn_cell_get_flag
               ((RecnCell *) gnc_table_layout_get_cell (reg->table->layout, RECN_CELL));
    else
        recn = xaccSplitGetReconcile (split);

    /* What Cell are we in */
    cell_name = gnc_table_get_cell_name (reg->table, virt_loc);

    /* These cells can be changed */
    change_ok = (g_strcmp0(cell_name, "notes") == 0) || (g_strcmp0(cell_name, "memo") == 0) || (g_strcmp0(cell_name, "action") == 0);

    if ((recn == YREC || xaccTransHasReconciledSplits (trans)) && !change_ok)
    {
        GtkWidget *dialog, *window;
        gint response;
        const gchar *title;
        const gchar *message;

        if(recn == YREC)
        {
            title = _("Change reconciled split?");
            message =
             _("You are about to change a reconciled split. Doing so might make "
               "future reconciliation difficult! Continue with this change?");
        }
        else
        {
            title = _("Change split linked to a reconciled split?");
            message =
            _("You are about to change a split that is linked to a reconciled split. "
              "Doing so might make future reconciliation difficult! Continue with this change?");
        }

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
        gtk_dialog_add_button(GTK_DIALOG(dialog), _("Chan_ge Split"),
                              GTK_RESPONSE_YES);
        response = gnc_dialog_run(GTK_DIALOG(dialog), GNC_PREF_WARN_REG_RECD_SPLIT_MOD);
        gtk_widget_destroy(dialog);
        if (response != GTK_RESPONSE_YES)
            return FALSE;

        info->change_confirmed = TRUE;
    }

    return TRUE;
}

static gpointer
gnc_split_register_guid_malloc (void)
{
    GncGUID *guid;

    guid = guid_malloc ();

    *guid = *guid_null ();

    return guid;
}

static const char *
gnc_template_register_get_xfrm_entry (VirtualLocation virt_loc,
                                      gboolean translate,
                                      gboolean *conditionally_changed,
                                      gpointer user_data)
{
    static char *name = NULL;

    SplitRegister *reg = user_data;
    Split *split;
    Account *account;
    GncGUID *guid = NULL;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    if (!split)
        return NULL;
    /* Caller either uses the return as a temporary in a boolean
     * expression or g_strdups it, so we keep it static and free the
     * old one on every call to avoid leaks. Ugly, but it works.
     */
    g_free (name);
    qof_instance_get (QOF_INSTANCE (split),
		      "sx-account", &guid,
		      NULL);
    account = xaccAccountLookup (guid, gnc_get_current_book ());
    name = account ? gnc_get_account_name_for_register (account) : NULL;

    return name;
}

static const char *
gnc_template_register_get_fdebt_entry (VirtualLocation virt_loc,
                                       gboolean translate,
                                       gboolean *conditionally_changed,
                                       gpointer user_data)
{
    SplitRegister *reg = user_data;
    Split *split = gnc_split_register_get_split(reg, virt_loc.vcell_loc);
    char *formula = NULL;

    qof_instance_get (QOF_INSTANCE (split),
		      "sx-debit-formula", &formula,
		      NULL);

    return formula;
}

static char *
gnc_split_register_get_fdebt_help (VirtualLocation virt_loc,
                                   gpointer user_data)
{
    SplitRegister *reg = user_data;
    const char *help;

    help = gnc_table_get_entry (reg->table, virt_loc);
    if (!help || *help == '\0')
        help = _("Enter debit formula for real transaction");

    return g_strdup (help);
}

static const char *
gnc_template_register_get_fcred_entry (VirtualLocation virt_loc,
                                       gboolean translate,
                                       gboolean *conditionally_changed,
                                       gpointer user_data)
{
    SplitRegister *reg = user_data;
    Split *split = gnc_split_register_get_split(reg, virt_loc.vcell_loc);
    char *formula = NULL;

    qof_instance_get (QOF_INSTANCE (split),
		      "sx-credit-formula", &formula,
		      NULL);

    return formula;

}

static char *
gnc_split_register_get_fcred_help (VirtualLocation virt_loc,
                                   gpointer user_data)
{
    SplitRegister *reg = user_data;
    const char *help = gnc_table_get_entry (reg->table, virt_loc);

    if (!help || *help == '\0')
        help = _("Enter credit formula for real transaction");

    return g_strdup (help);
}

static char *
gnc_split_register_get_default_help (VirtualLocation virt_loc,
                                     gpointer user_data)
{
    SplitRegister *reg = user_data;
    const char *help = gnc_table_get_entry(reg->table, virt_loc);

    return g_strdup (help);
}

static const char *
gnc_template_register_get_debcred_entry (VirtualLocation virt_loc,
        gboolean translate,
        gboolean *conditionally_changed,
        gpointer user_data)
{
    PERR("The function called always returned either NULL or an empty string "
	 "while issuing dire warnings about how incorrect it is. That code "
	 "has been removed and the function if called raises this error and "
	 "returns NULL");
    return NULL;
#if 0
    SplitRegister *reg = user_data;
    Split *split;

    split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
    if (!split)
        return gnc_split_register_get_debcred_entry (virt_loc,
                translate,
                conditionally_changed,
                user_data);

    kvpf = xaccSplitGetSlots (split);
    PWARN( "We're very close to \"wrong\".  \"Fix it immediately!!!\"" );

    if (kvpf)
    {
        gnc_numeric amount;
        const char * cell_name;
        char *str;

        PWARN("This code is wrong.  Fix it immediately!!!!");
        str = kvp_value_get_string(
                  kvp_frame_get_slot_path(kvpf, "sched-xaction", "amnt", NULL));

        amount = gnc_numeric_zero ();
        string_to_gnc_numeric (str, &amount);

        if (gnc_numeric_zero_p (amount))
            return "";

        cell_name = gnc_table_get_cell_name (reg->table, virt_loc);

        if (gnc_numeric_negative_p (amount) &&
                gnc_cell_name_equal (cell_name, DEBT_CELL))
            return "";

        if (gnc_numeric_positive_p (amount) &&
                gnc_cell_name_equal (cell_name, CRED_CELL))
            return "";

        amount = gnc_numeric_abs (amount);

        /* FIXME: This should be fixed to be correct for the "fake" account. */
        return xaccPrintAmount (amount, gnc_default_print_info (FALSE));
    }

    return NULL;
#endif
}

static void
gnc_split_register_guid_free (gpointer guid)
{
    guid_free (guid);
}

static void
gnc_split_register_guid_copy (gpointer p_to, gconstpointer p_from)
{
    GncGUID *to = p_to;
    const GncGUID *from = p_from;

    g_return_if_fail (to != NULL);
    *to = from ? *from : *guid_null();
}


static void
gnc_split_register_colorize_negative (gpointer gsettings, gchar *key, gpointer unused)
{
    use_red_for_negative = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL,
                                              GNC_PREF_NEGATIVE_IN_RED);
}


static gpointer
gnc_split_register_model_add_hooks (gpointer unused)
{
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED,
                          gnc_split_register_colorize_negative,
                          NULL);
    /* Get the initial value */
    use_red_for_negative = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL,
                                              GNC_PREF_NEGATIVE_IN_RED);
    return NULL;
}


TableModel *
gnc_split_register_model_new (void)
{
    TableModel *model;
    static GOnce once = G_ONCE_INIT;

    g_once(&once, gnc_split_register_model_add_hooks, NULL);

    model = gnc_table_model_new ();

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_date_entry,
                                       DATE_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_due_date_entry,
                                       DDUE_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_num_entry,
                                       NUM_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_tran_num_entry,
                                       TNUM_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_desc_entry,
                                       DESC_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_notes_entry,
                                       NOTES_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_vnotes_entry,
                                       VNOTES_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_rate_entry,
                                       RATE_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_recn_entry,
                                       RECN_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_action_entry,
                                       ACTN_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_memo_entry,
                                       MEMO_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_balance_entry,
                                       BALN_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_balance_entry,
                                       TBALN_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_price_entry,
                                       PRIC_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_shares_entry,
                                       SHRS_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_tshares_entry,
                                       TSHRS_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_xfrm_entry,
                                       XFRM_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_mxfrm_entry,
                                       MXFRM_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_tdebcred_entry,
                                       TDEBT_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_tdebcred_entry,
                                       TCRED_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_type_entry,
                                       TYPE_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_debcred_entry,
                                       DEBT_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_debcred_entry,
                                       CRED_CELL);

    gnc_table_model_set_entry_handler (model,
                                       gnc_split_register_get_rbaln_entry,
                                       RBALN_CELL);


    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_date_label,
                                       DATE_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_due_date_label,
                                       DDUE_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_num_label,
                                       NUM_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_tran_num_label,
                                       TNUM_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_desc_label,
                                       DESC_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_recn_label,
                                       RECN_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_baln_label,
                                       BALN_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_action_label,
                                       ACTN_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_xfrm_label,
                                       XFRM_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_memo_label,
                                       MEMO_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_debit_label,
                                       DEBT_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_credit_label,
                                       CRED_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_price_label,
                                       PRIC_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_shares_label,
                                       SHRS_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_mxfrm_label,
                                       MXFRM_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_tcredit_label,
                                       TCRED_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_tdebit_label,
                                       TDEBT_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_tshares_label,
                                       TSHRS_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_tbalance_label,
                                       TBALN_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_type_label,
                                       TYPE_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_notes_label,
                                       NOTES_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_fdebit_label,
                                       FDEBT_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_fcredit_label,
                                       FCRED_CELL);

    gnc_table_model_set_label_handler (model,
                                       gnc_split_register_get_tbalance_label,
                                       RBALN_CELL);


    gnc_table_model_set_default_help_handler(
        model, gnc_split_register_get_default_help);

    gnc_table_model_set_help_handler (model,
                                      gnc_split_register_get_date_help,
                                      DATE_CELL);

    gnc_table_model_set_help_handler (model,
                                      gnc_split_register_get_date_help,
                                      DDUE_CELL);

    gnc_table_model_set_help_handler (model,
                                      gnc_split_register_get_num_help,
                                      NUM_CELL);

    gnc_table_model_set_help_handler (model,
                                      gnc_split_register_get_tran_num_help,
                                      TNUM_CELL);

    gnc_table_model_set_help_handler (model,
                                      gnc_split_register_get_desc_help,
                                      DESC_CELL);

    gnc_table_model_set_help_handler (model,
                                      gnc_split_register_get_price_help,
                                      PRIC_CELL);

    gnc_table_model_set_help_handler (model,
                                      gnc_split_register_get_shares_help,
                                      SHRS_CELL);

    gnc_table_model_set_help_handler (model,
                                      gnc_split_register_get_action_help,
                                      ACTN_CELL);

    gnc_table_model_set_help_handler (model,
                                      gnc_split_register_get_memo_help,
                                      MEMO_CELL);

    gnc_table_model_set_help_handler (model,
                                      gnc_split_register_get_notes_help,
                                      NOTES_CELL);

    gnc_table_model_set_help_handler (model,
                                      gnc_split_register_get_vnotes_help,
                                      VNOTES_CELL);

    gnc_table_model_set_help_handler (model,
                                      gnc_split_register_get_xfrm_help,
                                      XFRM_CELL);

    gnc_table_model_set_help_handler (model,
                                      gnc_split_register_get_mxfrm_help,
                                      MXFRM_CELL);

    gnc_table_model_set_help_handler (model,
                                      gnc_split_register_get_fcred_help,
                                      FCRED_CELL);

    gnc_table_model_set_help_handler (model,
                                      gnc_split_register_get_fdebt_help,
                                      FDEBT_CELL);


    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_standard_io_flags, DATE_CELL);

    /* FIXME: We really only need a due date for 'invoices', not for
     * 'payments' or 'receipts'.  This implies we really only need the
     * due-date for transactions that credit the ACCT_TYPE_RECEIVABLE or
     * debit the ACCT_TYPE_PAYABLE account type.
     */
    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_rate_io_flags, RATE_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_ddue_io_flags, DDUE_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_standard_io_flags, NUM_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_standard_io_flags, TNUM_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_standard_io_flags, DESC_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_standard_io_flags, ACTN_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_standard_io_flags, XFRM_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_standard_io_flags, MEMO_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_standard_io_flags, MXFRM_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_standard_io_flags, NOTES_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_inactive_io_flags, VNOTES_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_debcred_io_flags, CRED_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_debcred_io_flags, DEBT_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_recn_io_flags, RECN_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_recn_io_flags, TYPE_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_security_io_flags, PRIC_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_security_io_flags, SHRS_CELL);


    gnc_table_model_set_fg_color_handler(
        model, gnc_split_register_get_fg_color, SHRS_CELL);

    gnc_table_model_set_fg_color_handler(
        model, gnc_split_register_get_fg_color, TSHRS_CELL);

    gnc_table_model_set_fg_color_handler(
        model, gnc_split_register_get_fg_color, BALN_CELL);

    gnc_table_model_set_fg_color_handler(
        model, gnc_split_register_get_fg_color, TBALN_CELL);

    gnc_table_model_set_fg_color_handler(
        model, gnc_split_register_get_fg_color, RBALN_CELL);

    gnc_table_model_set_fg_color_handler(
        model, gnc_split_register_get_gtkrc_fg_color, "gtkrc");


    gnc_table_model_set_default_bg_color_handler(
        model, gnc_split_register_get_bg_color);

    gnc_table_model_set_bg_color_handler(
        model, gnc_split_register_get_gtkrc_bg_color, "gtkrc");

    gnc_table_model_set_bg_color_handler(
        model, gnc_split_register_get_debcred_bg_color, DEBT_CELL);

    gnc_table_model_set_bg_color_handler(
        model, gnc_split_register_get_debcred_bg_color, CRED_CELL);

    gnc_table_model_set_bg_color_handler(
        model, gnc_split_register_get_debcred_bg_color, TDEBT_CELL);

    gnc_table_model_set_bg_color_handler(
        model, gnc_split_register_get_debcred_bg_color, TCRED_CELL);

    gnc_table_model_set_bg_color_handler(
        model, gnc_split_register_get_debcred_bg_color, FCRED_CELL);

    gnc_table_model_set_bg_color_handler(
        model, gnc_split_register_get_debcred_bg_color, FDEBT_CELL);


    gnc_table_model_set_default_cell_border_handler(
        model, gnc_split_register_get_border);


    gnc_table_model_set_default_confirm_handler(
        model, gnc_split_register_confirm);

    model->cell_data_allocator   = gnc_split_register_guid_malloc;
    model->cell_data_deallocator = gnc_split_register_guid_free;
    model->cell_data_copy        = gnc_split_register_guid_copy;

    gnc_split_register_model_add_save_handlers (model);

    return model;
}

TableModel *
gnc_template_register_model_new (void)
{
    TableModel *model;

    model = gnc_split_register_model_new ();

    gnc_table_model_set_entry_handler(
        model, gnc_split_register_get_inactive_date_entry, DATE_CELL );

    gnc_table_model_set_entry_handler(
        model, gnc_split_register_get_inactive_date_entry, DDUE_CELL );

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_inactive_io_flags, DATE_CELL );

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_inactive_io_flags, DDUE_CELL );

    gnc_table_model_set_entry_handler(
        model, gnc_template_register_get_xfrm_entry, XFRM_CELL);

    gnc_table_model_set_entry_handler(
        model, gnc_template_register_get_fdebt_entry, FDEBT_CELL);

    gnc_table_model_set_entry_handler(
        model, gnc_template_register_get_fcred_entry, FCRED_CELL);

    gnc_table_model_set_entry_handler(
        model, gnc_template_register_get_debcred_entry, DEBT_CELL);

    gnc_table_model_set_entry_handler(
        model, gnc_template_register_get_debcred_entry, CRED_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_standard_io_flags, FCRED_CELL);

    gnc_table_model_set_io_flags_handler(
        model, gnc_split_register_get_standard_io_flags, FDEBT_CELL);

    gnc_template_register_model_add_save_handlers (model);

    return model;
}
