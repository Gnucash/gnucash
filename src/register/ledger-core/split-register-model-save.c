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

#include "Scrub.h"
#include "SchedXaction.h"
#include "datecell.h"
#include "gnc-engine-util.h"
#include "numcell.h"
#include "pricecell.h"
#include "recncell.h"
#include "split-register-model-save.h"
#include "split-register-p.h"


struct sr_save_data
{
  Transaction *trans;
  Split *split;

  gboolean handled_dc; /* We have already handled the debit/credit cells. */
  gboolean do_scrub;   /* Scrub other split at the end. */
};

/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_LEDGER;


static void
gnc_split_register_save_date_cell (BasicCell * cell,
                                   gpointer save_data,
                                   gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  const char *value;
  Timespec ts;

  g_return_if_fail (gnc_basic_cell_has_name (cell, DATE_CELL));

  value = gnc_basic_cell_get_value (cell);

  /* commit any pending changes */
  gnc_date_cell_commit ((DateCell *) cell);

  DEBUG ("DATE: %s", value ? value : "(null)");

  gnc_date_cell_get_date ((DateCell *) cell, &ts);

  xaccTransSetDatePostedTS (sd->trans, &ts);
}

static void
gnc_split_register_save_type_cell (BasicCell * cell,
				   gpointer save_data,
				   gpointer user_data)
{
  SRSaveData *sd = save_data;
  char value;

  g_return_if_fail (gnc_basic_cell_has_name (cell, TYPE_CELL));

  value = gnc_recn_cell_get_flag ((RecnCell *)cell);

  xaccTransSetTxnType (sd->trans, value);
}

static void
gnc_split_register_save_due_date_cell (BasicCell * cell,
                                       gpointer save_data,
                                       gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  const char *value;
  Timespec ts;

  g_return_if_fail (gnc_basic_cell_has_name (cell, DDUE_CELL));

  value = gnc_basic_cell_get_value (cell);

  /* commit any pending changes */
  gnc_date_cell_commit ((DateCell *) cell);

  DEBUG ("DATE: %s", value ? value : "(null)");

  gnc_date_cell_get_date ((DateCell *) cell, &ts);

  xaccTransSetDateDueTS (sd->trans, &ts);
}

static void
gnc_split_register_save_num_cell (BasicCell * cell,
                                  gpointer save_data,
                                  gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  const char *value;

  g_return_if_fail (gnc_basic_cell_has_name (cell, NUM_CELL));

  value = gnc_basic_cell_get_value (cell);

  DEBUG ("NUM: %s\n", value ? value : "(null)");

  xaccTransSetNum (sd->trans, value);

  if (gnc_num_cell_set_last_num ((NumCell *) cell, value))
  {
    SRInfo *info = gnc_split_register_get_info (reg);
    Split *blank_split = xaccSplitLookup (&info->blank_split_guid,
                                          gnc_get_current_book ());
    Transaction *blank_trans = xaccSplitGetParent (blank_split);

    if (sd->trans != blank_trans)
      gnc_split_register_set_last_num (reg, gnc_basic_cell_get_value (cell));
  }
}

static void
gnc_split_register_save_desc_cell (BasicCell * cell,
                                   gpointer save_data,
                                   gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  const char *value;

  g_return_if_fail (gnc_basic_cell_has_name (cell, DESC_CELL));

  value = gnc_basic_cell_get_value (cell);

  DEBUG ("DESC: %s", value ? value : "(null)");

  xaccTransSetDescription (sd->trans, value);
}

static void
gnc_split_register_save_notes_cell (BasicCell * cell,
                                    gpointer save_data,
                                    gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  const char *value;

  g_return_if_fail (gnc_basic_cell_has_name (cell, NOTES_CELL));

  value = gnc_basic_cell_get_value (cell);

  DEBUG ("NOTES: %s", value ? value : "(null)");

  xaccTransSetNotes (sd->trans, value);
}

static void
gnc_split_register_save_recn_cell (BasicCell * bcell,
                                   gpointer save_data,
                                   gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  RecnCell *cell = (RecnCell *) bcell;

  g_return_if_fail (gnc_basic_cell_has_name (bcell, RECN_CELL));

  DEBUG ("RECN: %c", gnc_recn_cell_get_flag (cell));

  xaccSplitSetReconcile (sd->split, gnc_recn_cell_get_flag (cell));
}

static void
gnc_split_register_save_actn_cell (BasicCell * cell,
                                   gpointer save_data,
                                   gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  const char *value;

  g_return_if_fail (gnc_basic_cell_has_name (cell, ACTN_CELL));

  value = gnc_basic_cell_get_value (cell);

  DEBUG ("ACTN: %s", value ? value : "(null)");

  xaccSplitSetAction (sd->split, value);
}

static void
gnc_split_register_save_memo_cell (BasicCell * cell,
                                   gpointer save_data,
                                   gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  const char *value;

  g_return_if_fail (gnc_basic_cell_has_name (cell, MEMO_CELL));

  value = gnc_basic_cell_get_value (cell);

  DEBUG ("MEMO: %s", value ? value : "(null)");

  xaccSplitSetMemo (sd->split, value);
}

/* OK, the handling of transfers gets complicated because it depends
 * on what was displayed to the user. For a multi-line display, we
 * just reparent the indicated split. For a two-line display, we want
 * to reparent the "other" split, but only if there is one. XFRM is
 * the straight split, MXFRM is the mirrored split. */
static void
gnc_split_register_save_xfrm_cell (BasicCell * cell,
                                   gpointer save_data,
                                   gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  Account *old_acc;
  Account *new_acc;

  g_return_if_fail (gnc_basic_cell_has_name (cell, XFRM_CELL));

  old_acc = xaccSplitGetAccount (sd->split);

  new_acc = gnc_split_register_get_account (reg, XFRM_CELL);

  if ((new_acc != NULL) && (old_acc != new_acc))
    xaccAccountInsertSplit (new_acc, sd->split);
}

static void
gnc_split_register_save_mxfrm_cell (BasicCell * cell,
                                    gpointer save_data,
                                    gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  Split * other_split;

  g_return_if_fail (gnc_basic_cell_has_name (cell, MXFRM_CELL));

  other_split = xaccSplitGetOtherSplit (sd->split);

  /* other_split may be null for two very different reasons:
   * (1) the parent transaction has three or more splits in it,
   *     and so the "other" split is ambiguous, and thus null.
   * (2) the parent transaction has only this one split as a child.
   *     and "other" is null because there is no other.
   *
   * In the case (2), we want to create the other split, so that 
   * the user's request to transfer actually works out. */

  if (!other_split)
  {
    other_split = xaccTransGetSplit (sd->trans, 1);

    if (!other_split)
    {
      other_split = xaccMallocSplit (gnc_get_current_book ());
      xaccTransAppendSplit (sd->trans, other_split);
    }
  }

  if (other_split)
  {
    Account *old_acc;
    Account *new_acc;

    /* Do some reparenting. Insertion into new account
     * will automatically delete from the old account. */
    old_acc = xaccSplitGetAccount (other_split);
    new_acc = gnc_split_register_get_account (reg, MXFRM_CELL);

    if ((new_acc != NULL) && (old_acc != new_acc))
      xaccAccountInsertSplit (new_acc, other_split);
  }
}

static void
gnc_split_register_save_shares_cell (BasicCell * bcell,
                                     gpointer save_data,
                                     gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  PriceCell *cell = (PriceCell *) bcell;
  gnc_numeric amount;

  g_return_if_fail (gnc_basic_cell_has_name (bcell, SHRS_CELL));

  amount = gnc_price_cell_get_value (cell);

  DEBUG ("SHRS");

  xaccSplitSetAmount (sd->split, amount);

  sd->do_scrub = TRUE;
}

static void
gnc_split_register_save_price_cell (BasicCell * bcell,
                                    gpointer save_data,
                                    gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  PriceCell *cell = (PriceCell *) bcell;
  gnc_numeric price;

  g_return_if_fail (gnc_basic_cell_has_name (bcell, PRIC_CELL));

  price = gnc_price_cell_get_value (cell);

  DEBUG ("PRIC");

  xaccSplitSetSharePrice (sd->split, price);

  sd->do_scrub = TRUE;
}

static void
gnc_split_register_save_debcred_cell (BasicCell * bcell,
                                      gpointer save_data,
                                      gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  PriceCell *cell;
  gnc_numeric new_amount;
  gnc_numeric credit;
  gnc_numeric debit;

  g_return_if_fail (gnc_basic_cell_has_name (bcell, DEBT_CELL) ||
                    gnc_basic_cell_has_name (bcell, CRED_CELL));

  if (sd->handled_dc)
    return;

  cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                  CRED_CELL);
  credit = gnc_price_cell_get_value (cell);

  cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                  DEBT_CELL);
  debit  = gnc_price_cell_get_value (cell);

  new_amount = gnc_numeric_sub_fixed (debit, credit);

  xaccSplitSetValue (sd->split, new_amount);

  sd->handled_dc = TRUE;
  sd->do_scrub = TRUE;
}

static void
gnc_split_register_save_cells (gpointer save_data,
                               gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  Split *other_split;

  g_return_if_fail (sd != NULL);

  if (!sd->do_scrub)
    return;

  other_split = xaccSplitGetOtherSplit (sd->split);

  xaccSplitScrub (sd->split);

  if (other_split)
  {
    gnc_numeric value = xaccSplitGetValue (sd->split);

    value = gnc_numeric_neg (value);

    xaccSplitSetValue (other_split, value);

    xaccSplitScrub (other_split);
  }
}

static void
gnc_template_register_save_unexpected_cell (BasicCell * cell,
                                            gpointer save_data,
                                            gpointer user_data)
{
  PERR ("unexpected changed fields in a template register");
}

static void
gnc_template_register_save_xfrm_cell (BasicCell * cell,
                                      gpointer save_data,
                                      gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  SRInfo *info = gnc_split_register_get_info (reg);
  Account *template_acc;
  const GUID *acctGUID;
  kvp_frame *kvpf;
  Account *acct;

  g_return_if_fail (gnc_basic_cell_has_name (cell, XFRM_CELL));

  kvpf = xaccSplitGetSlots (sd->split);

  /* save the account GUID into the kvp_data. */
  acct = gnc_split_register_get_account (reg, XFRM_CELL);
  if (!acct)
  {
    PERR ("unknown account");
    return;
  }

  acctGUID = xaccAccountGetGUID (acct);

  kvp_frame_set_slot_path (kvpf, kvp_value_new_guid(acctGUID),
                           GNC_SX_ID, GNC_SX_ACCOUNT, NULL);

  kvpf = xaccSplitGetSlots (sd->split);

  template_acc = xaccAccountLookup (&info->template_account,
                                    gnc_get_current_book ());

  /* set the actual account to the fake account for these templates */
  xaccAccountInsertSplit (template_acc, sd->split);
}

static void
gnc_template_register_save_mxfrm_cell (BasicCell * cell,
                                       gpointer save_data,
                                       gpointer user_data)
{
}

static void
gnc_template_register_save_debcred_cell (BasicCell * cell,
                                         gpointer save_data,
                                         gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  kvp_frame *kvpf;
  const char *value;
  gnc_numeric new_amount;
  gnc_numeric credit;
  gnc_numeric debit;

  g_return_if_fail (gnc_basic_cell_has_name (cell, FDEBT_CELL) ||
                    gnc_basic_cell_has_name (cell, FCRED_CELL));

  if (sd->handled_dc)
    return;

  kvpf = xaccSplitGetSlots (sd->split);

  DEBUG ("kvp_frame before: %s\n", kvp_frame_to_string (kvpf));

  /* amountStr = gnc_numeric_to_string (new_amount); */

  value = gnc_table_layout_get_cell_value (reg->table->layout, FCRED_CELL);
  kvp_frame_set_slot_path (kvpf, kvp_value_new_string (value), 
                           GNC_SX_ID,
                           GNC_SX_CREDIT_FORMULA,
                           NULL);

  value = gnc_table_layout_get_cell_value (reg->table->layout, FDEBT_CELL);

  kvp_frame_set_slot_path (kvpf,  
                           kvp_value_new_string (value),
                           GNC_SX_ID, 
                           GNC_SX_DEBIT_FORMULA,
                           NULL);

  DEBUG ("kvp_frame  after: %s\n", kvp_frame_to_string (kvpf));

  /* set the amount to an innocuous value */
  xaccSplitSetValue (sd->split, gnc_numeric_create (0, 1));

  sd->handled_dc = TRUE;
}

static void
gnc_template_register_save_shares_cell (BasicCell * cell,
                                        gpointer save_data,
                                        gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  kvp_frame *kvpf;
  char *sharesStr = "(x + y)/42";

  g_return_if_fail (gnc_basic_cell_has_name (cell, SHRS_CELL));

  kvpf = xaccSplitGetSlots (sd->split);

  /* FIXME: shares cells are numeric by definition. */
  DEBUG ("kvp_frame before: %s\n", kvp_frame_to_string (kvpf));

  /* sharesStr = gnc_numeric_to_string( sharesStr ); */
  kvp_frame_set_slot_path (kvpf,
                           kvp_value_new_string (sharesStr),
                           GNC_SX_ID,
                           GNC_SX_SHARES,
                           NULL);

  DEBUG ("kvp_frame  after: %s\n", kvp_frame_to_string (kvpf));

  /* set the shares to an innocuous value */
  xaccSplitSetSharePriceAndAmount (sd->split,
                                   gnc_numeric_create (0, 1),
                                   gnc_numeric_create (0, 1));
}

void
gnc_split_register_model_add_save_handlers (TableModel *model)
{
  g_return_if_fail (model != NULL);

  gnc_table_model_set_save_handler (model,
                                    gnc_split_register_save_date_cell,
                                    DATE_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_split_register_save_due_date_cell,
                                    DDUE_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_split_register_save_type_cell,
                                    TYPE_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_split_register_save_num_cell,
                                    NUM_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_split_register_save_desc_cell,
                                    DESC_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_split_register_save_notes_cell,
                                    NOTES_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_split_register_save_recn_cell,
                                    RECN_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_split_register_save_actn_cell,
                                    ACTN_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_split_register_save_memo_cell,
                                    MEMO_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_split_register_save_xfrm_cell,
                                    XFRM_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_split_register_save_mxfrm_cell,
                                    MXFRM_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_split_register_save_shares_cell,
                                    SHRS_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_split_register_save_price_cell,
                                    PRIC_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_split_register_save_debcred_cell,
                                    DEBT_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_split_register_save_debcred_cell,
                                    CRED_CELL);

  gnc_table_model_set_post_save_handler (model, gnc_split_register_save_cells);
}

void
gnc_template_register_model_add_save_handlers (TableModel *model)
{
  g_return_if_fail (model != NULL);

  gnc_split_register_model_add_save_handlers (model);

  gnc_table_model_set_save_handler (model,
                                    gnc_template_register_save_unexpected_cell,
                                    DATE_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_template_register_save_unexpected_cell,
                                    DDUE_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_template_register_save_xfrm_cell,
                                    XFRM_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_template_register_save_mxfrm_cell,
                                    MXFRM_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_template_register_save_debcred_cell,
                                    FDEBT_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_template_register_save_debcred_cell,
                                    FCRED_CELL);

  gnc_table_model_set_save_handler (model,
                                    gnc_template_register_save_shares_cell,
                                    SHRS_CELL);
}

SRSaveData *
gnc_split_register_save_data_new (Transaction *trans, Split *split)
{
  SRSaveData *sd;

  g_return_val_if_fail (trans != NULL, NULL);
  g_return_val_if_fail (split != NULL, NULL);

  sd = g_new0 (SRSaveData, 1);

  sd->trans = trans;
  sd->split = split;
  sd->handled_dc = FALSE;
  sd->do_scrub = FALSE;

  return sd;
}

void
gnc_split_register_save_data_destroy (SRSaveData *sd)
{
  g_free (sd);
}
