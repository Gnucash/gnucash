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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <glib.h>

#include "global-options.h"
#include "gnc-engine-util.h"
#include "pricecell.h"
#include "split-register-p.h"


static short module = MOD_LEDGER;


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
  tm.tm_isdst = -1;

  return mktime (&tm);
}

/* The routines below create, access, and destroy the SRInfo structure
 * used by SplitLedger routines to store data for a particular register.
 * This is the only code that should access the user_data member of a
 * SplitRegister directly. If additional user data is needed, just add
 * it to the SRInfo structure above. */
static void
xaccSRInitRegisterData (SplitRegister *reg)
{
  SRInfo *info;

  if (reg == NULL)
    return;

  info = g_new0 (SRInfo, 1);

  info->blank_split_guid = *xaccGUIDNULL ();
  info->pending_trans_guid = *xaccGUIDNULL ();
  info->default_account = *xaccGUIDNULL ();

  info->last_date_entered = get_today_midnight ();

  info->first_pass = TRUE;
  info->full_refresh = TRUE;

  reg->user_data = info;
}

SRInfo *
xaccSRGetInfo (SplitRegister *reg)
{
  if (!reg)
    return NULL;

  if (reg->user_data == NULL)
    xaccSRInitRegisterData (reg);

  return reg->user_data;
}

gncUIWidget
xaccSRGetParent (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo (reg);

  if (reg == NULL)
    return NULL;

  if (info->get_parent == NULL)
    return NULL;

  return info->get_parent (info->user_data);
}

Split *
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

Account *
sr_get_default_account (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo (reg);

  return xaccAccountLookup (&info->default_account);
}

Transaction *
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

Split *
xaccSRGetTransSplit (SplitRegister *reg,
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

Split *
xaccSRGetCurrentTransSplit (SplitRegister *reg,
                            VirtualCellLocation *trans_split_loc)
{
  VirtualCellLocation vcell_loc;

  if (reg == NULL)
    return NULL;

  vcell_loc = reg->table->current_cursor_loc.vcell_loc;

  return xaccSRGetTransSplit (reg, vcell_loc, trans_split_loc);
}

gboolean
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

void
xaccSRShowTrans (SplitRegister *reg,
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

    cursor_class = xaccSplitRegisterGetCursorClass (reg, vc_loc);
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

void
sr_set_cell_fractions (SplitRegister *reg, Split *split)
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
  xaccSetPriceCellFraction (cell, fraction);

  cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                  CRED_CELL);
  xaccSetPriceCellFraction (cell, fraction);

  account = xaccSplitGetAccount (split);

  if (account == NULL)
    account = sr_get_default_account (reg);

  cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                  SHRS_CELL);

  if (account)
    xaccSetPriceCellFraction (cell, xaccAccountGetCommoditySCU (account));
  else
    xaccSetPriceCellFraction (cell, 100000);
}

CellBlock *
sr_get_passive_cursor (SplitRegister *reg)
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
sr_get_active_cursor (SplitRegister *reg)
{
  SRInfo *info = xaccSRGetInfo (reg);
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
