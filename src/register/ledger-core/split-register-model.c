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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <time.h>

#include "Group.h"
#include "datecell.h"
#include "global-options.h"
#include "gnc-engine-util.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "messages.h"
#include "pricecell.h"
#include "recncell.h"
#include "split-register.h"
#include "split-register-model.h"
#include "split-register-model-save.h"
#include "split-register-p.h"


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

/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_LEDGER;

/* Flag for determining colorization of negative amounts. */
static gboolean use_red_for_negative = TRUE;


static gboolean
gnc_split_register_use_security_cells (SplitRegister *reg,
                                       VirtualLocation virt_loc)
{
  GNCAccountType account_type;
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
    account = xaccGetAccountFromFullName (gnc_get_current_group (),
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
  return _("Num");
}

static const char *
gnc_split_register_get_desc_label (VirtualLocation virt_loc,
                                   gpointer user_data)
{
  return _("Description");
}

static const char *
gnc_split_register_get_recn_label (VirtualLocation virt_loc,
                                   gpointer user_data)
{
  return _("Reconciled:R") + 11;
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

  if (!gnc_split_register_use_security_cells (reg, virt_loc))
    return NULL;

  return _("Price");
}

static const char *
gnc_split_register_get_shares_label (VirtualLocation virt_loc,
                                     gpointer user_data)
{
  SplitRegister *reg = user_data;

  if (!gnc_split_register_use_security_cells (reg, virt_loc))
    return NULL;

  return _("Shares");
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
  GList *node;
  Account *account;
  gnc_numeric total = gnc_numeric_zero ();

  account = gnc_split_register_get_default_account (reg);

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

  account = gnc_split_register_get_default_account (reg);

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

static guint32
gnc_split_register_get_shares_fg_color (VirtualLocation virt_loc,
                                        gpointer user_data)
{
  SplitRegister *reg = user_data;
  const guint32 black = 0x000000;
  const guint32 red   = 0xff0000;
  const char * cell_name;
  gboolean is_current;
  gnc_numeric shares;
  Split *split;

  if (!use_red_for_negative)
    return black;

  split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
  if (!split)
    return black;

  cell_name = gnc_table_get_cell_name (reg->table, virt_loc);

  is_current = virt_cell_loc_equal (reg->table->current_cursor_loc.vcell_loc,
                                    virt_loc.vcell_loc);

  if (gnc_cell_name_equal (cell_name, TSHRS_CELL))
    shares = get_trans_total_amount (reg, xaccSplitGetParent (split));
  else if (is_current)
    shares = gnc_price_cell_get_value
      ((PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                SHRS_CELL));
  else
    shares = xaccSplitGetAmount (split);

  if (gnc_numeric_negative_p (shares))
    return red;

  return black;
}

static guint32
gnc_split_register_get_balance_fg_color (VirtualLocation virt_loc,
                                         gpointer user_data)
{
  SplitRegister *reg = user_data;
  const guint32 black = 0x000000;
  const guint32 red   = 0xff0000;
  const char * cell_name;
  gnc_numeric balance;
  Split *split;

  if (!use_red_for_negative)
    return black;

  split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
  if (!split)
    return black;

  cell_name = gnc_table_get_cell_name (reg->table, virt_loc);

  if (gnc_cell_name_equal (cell_name, BALN_CELL))
    balance = xaccSplitGetBalance (split);
  else
    balance = get_trans_total_balance (reg, xaccSplitGetParent (split));

  {
    Account *account;

    account = xaccSplitGetAccount (split);

    if (gnc_reverse_balance (account))
      balance = gnc_numeric_neg (balance);
  }

  if (gnc_numeric_negative_p (balance))
    return red;

  return black;
}

static guint32
gnc_split_register_get_bg_color (VirtualLocation virt_loc,
                                 gboolean *hatching,
                                 gpointer user_data)
{
  SplitRegister *reg = user_data;
  const char *cursor_name;
  VirtualCell *vcell;
  guint32 bg_color;
  gboolean is_current;

  if (hatching)
    *hatching = FALSE;

  bg_color = 0xffffff; /* white */

  if (!reg)
    return bg_color;

  if (gnc_table_virtual_location_in_header (reg->table, virt_loc))
    return reg_colors.header_bg_color;

  vcell = gnc_table_get_virtual_cell (reg->table, virt_loc.vcell_loc);
  if (!vcell || !vcell->cellblock)
    return bg_color;

  if ((virt_loc.phys_col_offset < vcell->cellblock->start_col) ||
      (virt_loc.phys_col_offset > vcell->cellblock->stop_col))
    return bg_color;

  is_current = virt_cell_loc_equal (reg->table->current_cursor_loc.vcell_loc,
                                    virt_loc.vcell_loc);

  cursor_name = vcell->cellblock->cursor_name;

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

  PWARN ("Unexpected cursor: %s\n", cursor_name);

  return bg_color;
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
      *hatching = !gnc_numeric_zero_p (xaccTransGetImbalance (trans));
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
gnc_split_register_get_due_date_entry (VirtualLocation virt_loc,
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

  xaccTransGetDateDueTS (trans, &ts);

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
  struct tm *tm;
  Timespec ts;
  time_t tt;

  cell = gnc_table_get_cell (reg->table, virt_loc);
  if (!cell)
    return NULL;

  if (!cell->value || *cell->value == '\0')
    return NULL;

  gnc_date_cell_get_date ((DateCell *) cell, &ts);
  tt = ts.tv_sec;

  tm = localtime (&tt);

  strftime (string, sizeof (string), "%A %d %B %Y", tm);

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

  return xaccTransGetNum (trans);
}

static char *
gnc_split_register_get_num_help (VirtualLocation virt_loc,
                                 gpointer user_data)
{
  SplitRegister *reg = user_data;
  const char *help;

  help = gnc_table_get_entry (reg->table, virt_loc);
  if (!help || *help == '\0')
    help = _("Enter the transaction number, such as the check number");

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
    help = _("Enter a description of the transaction");

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
  Split *split;

  split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);

  return xaccSplitGetAction (split);
}

static char *
gnc_split_register_get_action_help (VirtualLocation virt_loc,
                                    gpointer user_data)
{
  SplitRegister *reg = user_data;
  const char *help;

  help = gnc_table_get_entry (reg->table, virt_loc);
  if (!help || *help == '\0')
    help = _("Enter the type of transaction, or choose one from the list");

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

  {
    Account *account;

    account = xaccSplitGetAccount (split);
    if (!account)
      account = gnc_split_register_get_default_account (reg);

    if (gnc_reverse_balance (account))
      balance = gnc_numeric_neg (balance);
  }

  return xaccPrintAmount (balance, gnc_split_value_print_info (split, FALSE));
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
  Split *s;

  split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);

  g_free (name);

  name = xaccAccountGetFullName (xaccSplitGetAccount (split),
                                 gnc_get_account_separator ());

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
    name = xaccAccountGetFullName (xaccSplitGetAccount (s),
                                   gnc_get_account_separator ());
  else
  {
    /* For multi-split transactions and stock splits,
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
    else if (safe_strcmp ("stock-split", xaccSplitGetType (split)) == 0)
      help = _("This transaction is a stock split; "
               "press the Split button to see details");
    else
      help = "";
  }

  return g_strdup (help);
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

  total = get_trans_total_amount (reg, xaccSplitGetParent (split));
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

static const char *
gnc_split_register_get_debcred_entry (VirtualLocation virt_loc,
                                      gboolean translate,
                                      gboolean *conditionally_changed,
                                      gpointer user_data)
{
  SplitRegister *reg = user_data;
  gboolean is_debit;
  Split *split;

  is_debit = gnc_cell_name_equal
    (gnc_table_get_cell_name (reg->table, virt_loc), DEBT_CELL);

  split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);

  if (!split)
  {
    Transaction *trans;
    gnc_numeric imbalance;
    gnc_commodity *currency;

    trans = gnc_split_register_get_trans (reg, virt_loc.vcell_loc);
    imbalance = xaccTransGetImbalance (trans);

    if (gnc_numeric_zero_p (imbalance))
      return NULL;

    imbalance = gnc_numeric_neg (imbalance);

    if (gnc_numeric_negative_p (imbalance) && is_debit)
      return NULL;

    if (gnc_numeric_positive_p (imbalance) && !is_debit)
      return NULL;

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
  }

  {
    gnc_numeric amount;

    amount = xaccSplitGetValue (split);
    if (gnc_numeric_zero_p (amount))
      return NULL;

    if (gnc_numeric_negative_p (amount) && is_debit)
      return NULL;

    if (gnc_numeric_positive_p (amount) && !is_debit)
      return NULL;

    amount = gnc_numeric_abs (amount);

    return xaccPrintAmount (amount, gnc_split_value_print_info (split, FALSE));
  }
}

static CellIOFlags
gnc_split_register_get_inactive_io_flags (VirtualLocation virt_loc,
                                          gpointer user_data)
{
  return XACC_CELL_ALLOW_NONE;
}

static CellIOFlags
gnc_split_register_get_standard_io_flags (VirtualLocation virt_loc,
                                          gpointer user_data)
{
  return XACC_CELL_ALLOW_ALL;
}

static CellIOFlags
gnc_split_register_get_recn_io_flags (VirtualLocation virt_loc,
                                      gpointer user_data)
{
  return XACC_CELL_ALLOW_ALL | XACC_CELL_ALLOW_EXACT_ONLY;
}

static CellIOFlags
gnc_split_register_get_debcred_io_flags (VirtualLocation virt_loc,
                                         gpointer user_data)
{
  SplitRegister *reg = user_data;
  Split *split;

  split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);

  if (safe_strcmp ("stock-split", xaccSplitGetType (split)) == 0)
    return XACC_CELL_ALLOW_NONE;

  return XACC_CELL_ALLOW_ALL;
}

static CellIOFlags
gnc_split_register_get_security_io_flags (VirtualLocation virt_loc,
                                          gpointer user_data)
{
  SplitRegister *reg = user_data;

  if (gnc_split_register_use_security_cells (reg, virt_loc))
    return XACC_CELL_ALLOW_ALL;

  return XACC_CELL_ALLOW_SHADOW;
}

static gboolean
gnc_split_register_confirm (VirtualLocation virt_loc, gpointer user_data)
{
  SplitRegister *reg = user_data;
  SRInfo *info = gnc_split_register_get_info (reg);
  Split *split;
  char recn;

  /* This assumes we reset the flag whenever we change splits.
   * This happens in LedgerMoveCursor. */
  if (info->change_confirmed)
    return TRUE;

  split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
  if (!split)
    return TRUE;

  if (gnc_table_layout_get_cell_changed (reg->table->layout, RECN_CELL, FALSE))
    recn = gnc_recn_cell_get_flag
      ((RecnCell *) gnc_table_layout_get_cell (reg->table->layout, RECN_CELL));
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

    confirm = gnc_verify_dialog_parented (gnc_split_register_get_parent (reg),
                                          message, FALSE);

    info->change_confirmed = confirm;

    return confirm;
  }

  return TRUE;
}

static gpointer
gnc_split_register_guid_malloc (void)
{
  GUID *guid;

  guid = xaccGUIDMalloc ();

  *guid = *xaccGUIDNULL ();

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
  kvp_frame *kvpf;
  Split *split;

  split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
  if (!split)
    return NULL;

  kvpf = xaccSplitGetSlots (split);

  g_free (name);

  if (kvpf)
  {
    Account *account;
    GUID *guid;

    guid = kvp_value_get_guid (kvp_frame_get_slot_path
                               (kvpf,
                                "sched-xaction",
                                "account",
                                NULL));

    account = xaccAccountLookup (guid, gnc_get_current_book ());

    name = xaccAccountGetFullName (account, gnc_get_account_separator ());
  }
  else
    name = NULL;

  return name;
}

static const char *
gnc_template_register_get_fdebt_entry (VirtualLocation virt_loc,
                                       gboolean translate,
                                       gboolean *conditionally_changed,
                                       gpointer user_data)
{
  SplitRegister *reg = user_data;
  kvp_frame *kvpf;
  Split *split;

  split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
  kvpf = xaccSplitGetSlots (split);

  return kvp_value_get_string
    (kvp_frame_get_slot_path (kvpf,
                              "sched-xaction",
                              "debit-formula",
                              NULL));
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
  kvp_frame *kvpf;
  Split *split;

  split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
  kvpf = xaccSplitGetSlots (split);

  return kvp_value_get_string
    (kvp_frame_get_slot_path (kvpf,
                              "sched-xaction",
                              "credit-formula",
                              NULL));
}

static char *
gnc_split_register_get_fcred_help (VirtualLocation virt_loc,
                                   gpointer user_data)
{
  SplitRegister *reg = user_data;
  const char *help;

  help = gnc_table_get_entry (reg->table, virt_loc);
  if (!help || *help == '\0')
    help = _("Enter credit formula for real transaction");

  return g_strdup (help);
}

static char *
gnc_split_register_get_default_help (VirtualLocation virt_loc,
                                     gpointer user_data)
{
  SplitRegister *reg = user_data;
  const char *help;

  help = gnc_table_get_entry (reg->table, virt_loc);

  return g_strdup (help);
}

static const char *
gnc_template_register_get_debcred_entry (VirtualLocation virt_loc,
                                         gboolean translate,
                                         gboolean *conditionally_changed,
                                         gpointer user_data)
{
  SplitRegister *reg = user_data;
  kvp_frame *kvpf;
  Split *split;

  split = gnc_split_register_get_split (reg, virt_loc.vcell_loc);
  if (!split)
    return gnc_split_register_get_debcred_entry (virt_loc,
                                                 translate,
                                                 conditionally_changed,
                                                 user_data);

  kvpf = xaccSplitGetSlots (split);

  if (kvpf)
  {
    gnc_numeric amount;
    const char * cell_name;
    char *str;

    PWARN("This code is wrong.  Fix it immediately!!!!");
    str = kvp_value_get_string (kvp_frame_get_slot_path
                                (kvpf,
                                 "sched-xaction",
                                 "amnt",
                                 NULL));

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
}

static void
gnc_split_register_guid_free (gpointer guid)
{
  xaccGUIDFree (guid);
}

static void
gnc_split_register_guid_copy (gpointer p_to, gconstpointer p_from)
{
  GUID *to = p_to;
  const GUID *from = p_from;

  g_return_if_fail (to != NULL);

  if (from == NULL)
    *to = *xaccGUIDNULL ();
  else
    *to = *from;
}

TableModel *
gnc_split_register_model_new (void)
{
  TableModel *model;

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
                                     gnc_split_register_get_desc_entry,
                                     DESC_CELL);

  gnc_table_model_set_entry_handler (model,
                                     gnc_split_register_get_notes_entry,
                                     NOTES_CELL);

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
                                     gnc_split_register_get_debcred_entry,
                                     DEBT_CELL);

  gnc_table_model_set_entry_handler (model,
                                     gnc_split_register_get_debcred_entry,
                                     CRED_CELL);


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
                                     gnc_split_register_get_notes_label,
                                     NOTES_CELL);

  gnc_table_model_set_label_handler (model,
                                     gnc_split_register_get_fdebit_label,
                                     FDEBT_CELL);

  gnc_table_model_set_label_handler (model,
                                     gnc_split_register_get_fcredit_label,
                                     FCRED_CELL);


  gnc_table_model_set_default_help_handler
    (model, gnc_split_register_get_default_help);

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


  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_standard_io_flags,
                                  DATE_CELL);

  /* FIXME: We really only need a due date for 'invoices', not for
   * 'payments' or 'receipts'.  This implies we really only need the
   * due-date for transactions that credit the RECEIVABLE or debit
   * the PAYABLE account type.
   */
  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_standard_io_flags,
                                  DDUE_CELL);

  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_standard_io_flags,
                                  NUM_CELL);

  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_standard_io_flags,
                                  DESC_CELL);

  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_standard_io_flags,
                                  ACTN_CELL);

  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_standard_io_flags,
                                  XFRM_CELL);

  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_standard_io_flags,
                                  MEMO_CELL);

  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_standard_io_flags,
                                  MXFRM_CELL);

  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_standard_io_flags,
                                  NOTES_CELL);

  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_debcred_io_flags,
                                  CRED_CELL);

  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_debcred_io_flags,
                                  DEBT_CELL);

  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_recn_io_flags,
                                  RECN_CELL);

  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_security_io_flags,
                                  PRIC_CELL);

  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_security_io_flags,
                                  SHRS_CELL);


  gnc_table_model_set_fg_color_handler (model,
                                        gnc_split_register_get_shares_fg_color,
                                        SHRS_CELL);

  gnc_table_model_set_fg_color_handler (model,
                                        gnc_split_register_get_shares_fg_color,
                                        TSHRS_CELL);

  gnc_table_model_set_fg_color_handler
                                   (model,
                                    gnc_split_register_get_balance_fg_color,
                                    BALN_CELL);

  gnc_table_model_set_fg_color_handler
                                   (model,
                                    gnc_split_register_get_balance_fg_color,
                                    TBALN_CELL);


  gnc_table_model_set_default_bg_color_handler
                                   (model,
                                    gnc_split_register_get_bg_color);

  gnc_table_model_set_bg_color_handler
                                   (model,
                                    gnc_split_register_get_debcred_bg_color,
                                    DEBT_CELL);

  gnc_table_model_set_bg_color_handler
                                   (model,
                                    gnc_split_register_get_debcred_bg_color,
                                    CRED_CELL);

  gnc_table_model_set_bg_color_handler
                                   (model,
                                    gnc_split_register_get_debcred_bg_color,
                                    TDEBT_CELL);

  gnc_table_model_set_bg_color_handler
                                   (model,
                                    gnc_split_register_get_debcred_bg_color,
                                    TCRED_CELL);

  gnc_table_model_set_bg_color_handler
                                   (model,
                                    gnc_split_register_get_debcred_bg_color,
                                    FCRED_CELL);

  gnc_table_model_set_bg_color_handler
                                   (model,
                                    gnc_split_register_get_debcred_bg_color,
                                    FDEBT_CELL);


  gnc_table_model_set_default_cell_border_handler
                                   (model,
                                    gnc_split_register_get_border);


  gnc_table_model_set_default_confirm_handler (model,
                                               gnc_split_register_confirm);

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

  gnc_table_model_set_entry_handler( model,
                                     gnc_split_register_get_inactive_date_entry,
                                     DATE_CELL );

  gnc_table_model_set_entry_handler( model,
                                     gnc_split_register_get_inactive_date_entry,
                                     DDUE_CELL );

  gnc_table_model_set_io_flags_handler( model,
                                        gnc_split_register_get_inactive_io_flags,
                                        DATE_CELL );

  gnc_table_model_set_io_flags_handler( model,
                                        gnc_split_register_get_inactive_io_flags,
                                        DDUE_CELL );

  gnc_table_model_set_entry_handler (model,
                                     gnc_template_register_get_xfrm_entry,
                                     XFRM_CELL);

  gnc_table_model_set_entry_handler (model,
                                     gnc_template_register_get_fdebt_entry,
                                     FDEBT_CELL);

  gnc_table_model_set_entry_handler (model,
                                     gnc_template_register_get_fcred_entry,
                                     FCRED_CELL);

  gnc_table_model_set_entry_handler (model,
                                     gnc_template_register_get_debcred_entry,
                                     DEBT_CELL);

  gnc_table_model_set_entry_handler (model,
                                     gnc_template_register_get_debcred_entry,
                                     CRED_CELL);

  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_standard_io_flags,
                                  FCRED_CELL);

  gnc_table_model_set_io_flags_handler
                                 (model,
                                  gnc_split_register_get_standard_io_flags,
                                  FDEBT_CELL);

  gnc_template_register_model_add_save_handlers (model);

  return model;
}

void
gnc_split_register_colorize_negative (gboolean use_red)
{
  use_red_for_negative = use_red;
}

void
gnc_split_register_set_colors (SplitRegisterColors reg_colors_new)
{
  reg_colors = reg_colors_new;
}
