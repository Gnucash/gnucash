/********************************************************************\
 * split-register-util.c -- split register utilities                *
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

#include "pricecell.h"
#include "split-register-p.h"


static QofLogModule log_module = GNC_MOD_LEDGER;


/* The routines below create, access, and destroy the SRInfo structure
 * used by SplitLedger routines to store data for a particular register.
 * This is the only code that should access the user_data member of a
 * SplitRegister directly. If additional user data is needed, just add
 * it to the SRInfo structure above. */
static void
gnc_split_register_init_info (SplitRegister *reg)
{
    SRInfo *info;

    if (reg == NULL)
        return;

    info = g_new0 (SRInfo, 1);

    info->blank_split_guid = *guid_null ();
    info->pending_trans_guid = *guid_null ();
    info->default_account = *guid_null ();
    info->template_account = *guid_null ();

    info->last_date_entered = gnc_timet_get_today_start ();

    info->first_pass = TRUE;
    info->full_refresh = TRUE;
    info->separator_changed = TRUE;

    reg->sr_info = info;
}

SRInfo *
gnc_split_register_get_info (SplitRegister *reg)
{
    if (!reg)
        return NULL;

    if (reg->sr_info == NULL)
        gnc_split_register_init_info (reg);

    return reg->sr_info;
}

gncUIWidget
gnc_split_register_get_parent (SplitRegister *reg)
{
    SRInfo *info = gnc_split_register_get_info (reg);

    if (reg == NULL)
        return NULL;

    if (info->get_parent == NULL)
        return NULL;

    return info->get_parent (info->user_data);
}

Split *
gnc_split_register_get_split (SplitRegister *reg,
                              VirtualCellLocation vcell_loc)
{
    GUID *guid;

    if (reg == NULL)
        return NULL;

    guid = gnc_table_get_vcell_data (reg->table, vcell_loc);
    if (guid == NULL)
        return NULL;

    return xaccSplitLookup (guid, gnc_get_current_book ());
}

Account *
gnc_split_register_get_default_account (SplitRegister *reg)
{
    SRInfo *info = gnc_split_register_get_info (reg);

    return xaccAccountLookup (&info->default_account,
                              gnc_get_current_book ());
}

void
gnc_split_register_set_template_account (SplitRegister *reg,
        Account *template_account)
{
    SRInfo *info = gnc_split_register_get_info (reg);

    g_return_if_fail (reg != NULL);

    info->template_account = *xaccAccountGetGUID (template_account);
}

Transaction *
gnc_split_register_get_trans (SplitRegister *reg,
                              VirtualCellLocation vcell_loc)
{
    Split *split;

    if (!reg || !reg->table)
        return NULL;

    split = gnc_split_register_get_split (reg, vcell_loc);

    if (split != NULL)
        return xaccSplitGetParent(split);

    /* Split is blank. Assume it is the blank split of a multi-line
     * transaction. Go back one row to find a split in the transaction. */
    vcell_loc.virt_row--;

    split = gnc_split_register_get_split (reg, vcell_loc);

    /* This split could be NULL during register initialization. */
    if (split == NULL)
        return NULL;

    return xaccSplitGetParent(split);
}

Split *
gnc_split_register_get_trans_split (SplitRegister *reg,
                                    VirtualCellLocation vcell_loc,
                                    VirtualCellLocation *trans_split_loc)
{
    CursorClass cursor_class;

    if (reg == NULL)
        return NULL;

    while (TRUE)
    {
        if ((0 > vcell_loc.virt_row) || (0 > vcell_loc.virt_col))
        {
            PERR ("bad row \n");
            return NULL;
        }

        cursor_class = gnc_split_register_get_cursor_class (reg, vcell_loc);

        if (cursor_class == CURSOR_CLASS_TRANS)
        {
            if (trans_split_loc)
                *trans_split_loc = vcell_loc;

            return gnc_split_register_get_split (reg, vcell_loc);
        }

        vcell_loc.virt_row--;
    }
}

Split *
gnc_split_register_get_current_trans_split(
    SplitRegister *reg, VirtualCellLocation *trans_split_loc)
{
    VirtualCellLocation vcell_loc;

    if (reg == NULL)
        return NULL;

    vcell_loc = reg->table->current_cursor_loc.vcell_loc;

    return gnc_split_register_get_trans_split (reg, vcell_loc, trans_split_loc);
}

gboolean
gnc_split_register_find_split (SplitRegister *reg,
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

            s = gnc_split_register_get_split (reg, vc_loc);
            t = xaccSplitGetParent(s);

            cursor_class = gnc_split_register_get_cursor_class (reg, vc_loc);

            if (t == trans)
            {
                /* A register entry for the correct transaction. */
                found_trans = TRUE;

                if (cursor_class == CURSOR_CLASS_TRANS)
                {
                    /* This row is the transaction split for this transaction. */
                    if (s == trans_split)
                        /* It's the copy of the transaction that we want. */
                        found_trans_split = TRUE;
                    else
                        found_trans_split = FALSE;

                    if (find_class == CURSOR_CLASS_TRANS &&
                            (s == split || reg->style == REG_STYLE_JOURNAL))
                    {
                        /* We're looking for a transaction split and this is the split we're looking for
                           or there is only one entry for this transaction in this register (since it's
                           a journal style register) so we must return the only transaction split there is. */
                        if (vcell_loc != NULL)
                            *vcell_loc = vc_loc;
                        return TRUE;
                    }
                }
            }
            else
            {
                /* Not the correct transaction.  We shouldn't get here if these are true, but just
                   to be safe reset them. */
                found_trans = FALSE;
                found_trans_split = FALSE;
            }

            if (found_trans && (s == split) && s)
            {
                /* We're on the right transaction, but perhaps not the copy of it we want, and
                   this is the correct split, return it if we don't find anything better. */
                if (vcell_loc != NULL)
                    *vcell_loc = vc_loc;

                found_something = TRUE;
            }

            if (found_trans_split && (s == split))
            {
                /* We're on the right copy of the right transaction, and this is the split we
                   want, return it (it should be the right class since if we wanted a transaction
                   split we would have returned it above. */
                if (vcell_loc != NULL)
                    *vcell_loc = vc_loc;

                if (cursor_class == find_class)
                    return TRUE;
            }
        }

    return found_something;
}

void
gnc_split_register_show_trans (SplitRegister *reg,
                               VirtualCellLocation start_loc)
{
    VirtualCellLocation end_loc;
    int v_row;

    end_loc = start_loc;

    for (v_row = end_loc.virt_row + 1;
            v_row < reg->table->num_virt_rows; v_row++)
    {
        VirtualCellLocation vc_loc = { v_row, 0 };
        CursorClass cursor_class;

        cursor_class = gnc_split_register_get_cursor_class (reg, vc_loc);
        if (cursor_class == CURSOR_CLASS_TRANS)
            break;

        if (cursor_class != CURSOR_CLASS_SPLIT)
        {
            v_row--;
            break;
        }
    }

    end_loc.virt_row = MIN (v_row, reg->table->num_virt_rows - 1);

    gnc_table_show_range (reg->table, start_loc, end_loc);
}

void
gnc_split_register_set_trans_visible (SplitRegister *reg,
                                      VirtualCellLocation vcell_loc,
                                      gboolean visible,
                                      gboolean only_blank_split)
{
    CursorClass cursor_class;

    while (TRUE)
    {
        vcell_loc.virt_row++;

        cursor_class = gnc_split_register_get_cursor_class (reg, vcell_loc);
        if (cursor_class != CURSOR_CLASS_SPLIT)
            return;

        if (only_blank_split && gnc_split_register_get_split (reg, vcell_loc))
            continue;

        gnc_table_set_virt_cell_visible (reg->table, vcell_loc, visible);
    }
}

void
gnc_split_register_set_cell_fractions (SplitRegister *reg, Split *split)
{
    Account *account;
    Transaction *trans;
    gnc_commodity *currency;
    PriceCell *cell;
    int fraction;

    trans = xaccSplitGetParent (split);
    currency = xaccTransGetCurrency (trans);
    if (!currency)
        currency = gnc_default_currency ();

    fraction = gnc_commodity_get_fraction (currency);

    cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
            DEBT_CELL);
    gnc_price_cell_set_fraction (cell, fraction);

    cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
            CRED_CELL);
    gnc_price_cell_set_fraction (cell, fraction);

    account = xaccSplitGetAccount (split);

    if (account == NULL)
        account = gnc_split_register_get_default_account (reg);

    cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
            SHRS_CELL);

    if (account)
        gnc_price_cell_set_fraction (cell, xaccAccountGetCommoditySCU (account));
    else
        gnc_price_cell_set_fraction (cell, 100000);
}

CellBlock *
gnc_split_register_get_passive_cursor (SplitRegister *reg)
{
    const char *cursor_name = NULL;

    switch (reg->style)
    {
    case REG_STYLE_LEDGER:
    case REG_STYLE_AUTO_LEDGER:
        cursor_name = reg->use_double_line ?
                      CURSOR_DOUBLE_LEDGER : CURSOR_SINGLE_LEDGER;
        break;

    case REG_STYLE_JOURNAL:
        cursor_name = reg->use_double_line ?
                      CURSOR_DOUBLE_JOURNAL : CURSOR_SINGLE_JOURNAL;
        break;
    }

    if (!cursor_name)
    {
        PWARN ("bad register style");
        return NULL;
    }

    return gnc_table_layout_get_cursor (reg->table->layout, cursor_name);
}

CellBlock *
gnc_split_register_get_active_cursor (SplitRegister *reg)
{
    SRInfo *info = gnc_split_register_get_info (reg);
    const char *cursor_name = NULL;

    switch (reg->style)
    {
    case REG_STYLE_LEDGER:
        if (!info->trans_expanded)
        {
            cursor_name = reg->use_double_line ?
                          CURSOR_DOUBLE_LEDGER : CURSOR_SINGLE_LEDGER;
            break;
        }

        /* fall through */
    case REG_STYLE_AUTO_LEDGER:
    case REG_STYLE_JOURNAL:
        cursor_name = reg->use_double_line ?
                      CURSOR_DOUBLE_JOURNAL : CURSOR_SINGLE_JOURNAL;
        break;
    }

    if (!cursor_name)
    {
        PWARN ("bad register style");
        return NULL;
    }

    return gnc_table_layout_get_cursor (reg->table->layout, cursor_name);
}

void
gnc_split_register_set_last_num (SplitRegister *reg, const char *num)
{
    Account *account;

    account = gnc_split_register_get_default_account (reg);
    if (!account)
        return;

    xaccAccountSetLastNum (account, num);
}

static CursorClass
gnc_split_register_cursor_class (SplitRegister *reg,
                                 CellBlock *cursor)
{
    if (cursor == NULL)
        return CURSOR_CLASS_NONE;

    return gnc_split_register_cursor_name_to_class (cursor->cursor_name);
}

CursorClass
gnc_split_register_get_cursor_class (SplitRegister *reg,
                                     VirtualCellLocation vcell_loc)
{
    VirtualCell *vcell;
    Table *table;

    if (reg == NULL)
        return CURSOR_CLASS_NONE;

    table = reg->table;
    if (table == NULL)
        return CURSOR_CLASS_NONE;

    vcell = gnc_table_get_virtual_cell (table, vcell_loc);
    if (vcell == NULL)
        return CURSOR_CLASS_NONE;

    return gnc_split_register_cursor_class (reg, vcell->cellblock);
}

CursorClass
gnc_split_register_get_current_cursor_class (SplitRegister *reg)
{
    Table *table;

    if (reg == NULL)
        return CURSOR_CLASS_NONE;

    table = reg->table;
    if (table == NULL)
        return CURSOR_CLASS_NONE;

    return gnc_split_register_cursor_class (reg, table->current_cursor);
}

CursorClass
gnc_split_register_cursor_name_to_class (const char *cursor_name)
{
    if (cursor_name == NULL)
        return CURSOR_CLASS_NONE;

    if (strcmp (cursor_name, CURSOR_SINGLE_LEDGER) == 0  ||
            strcmp (cursor_name, CURSOR_DOUBLE_LEDGER) == 0  ||
            strcmp (cursor_name, CURSOR_SINGLE_JOURNAL) == 0 ||
            strcmp (cursor_name, CURSOR_DOUBLE_JOURNAL) == 0)
        return CURSOR_CLASS_TRANS;

    if (strcmp (cursor_name, CURSOR_SPLIT) == 0)
        return CURSOR_CLASS_SPLIT;

    return CURSOR_CLASS_NONE;
}
