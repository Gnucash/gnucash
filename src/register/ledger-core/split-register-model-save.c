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
};

/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_LEDGER;


static void
gnc_split_register_save_cells (gpointer save_data,
                               gpointer user_data)
{
  SRSaveData *sd = save_data;
  SplitRegister *reg = user_data;
  SRInfo *info = gnc_split_register_get_info (reg);
  Transaction *trans;
  Split *other_split = NULL;
  Split *split;

  g_return_if_fail (sd != NULL);

  trans = sd->trans;
  split = sd->split;

  /* copy the contents from the cursor to the split */
  if (gnc_table_layout_get_cell_changed (reg->table->layout, DATE_CELL, TRUE))
  {
    BasicCell *cell;
    const char *value;
    Timespec ts;

    cell = gnc_table_layout_get_cell (reg->table->layout, DATE_CELL);
    value = gnc_basic_cell_get_value (cell);

    /* commit any pending changes */
    gnc_date_cell_commit ((DateCell *) cell);

    DEBUG ("DATE: %s", value ? value : "(null)");

    gnc_date_cell_get_date ((DateCell *) cell, &ts);

    xaccTransSetDatePostedTS (trans, &ts);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, NUM_CELL, TRUE))
  {
    BasicCell *cell;
    const char *value;

    value = gnc_table_layout_get_cell_value (reg->table->layout, NUM_CELL);

    DEBUG ("NUM: %s\n", value ? value : "(null)");

    xaccTransSetNum (trans, value);

    cell = gnc_table_layout_get_cell (reg->table->layout, NUM_CELL);

    if (gnc_num_cell_set_last_num ((NumCell *) cell, value))
    {
      SRInfo *info = gnc_split_register_get_info (reg);
      Split *blank_split = xaccSplitLookup(&info->blank_split_guid);
      Transaction *blank_trans = xaccSplitGetParent (blank_split);

      if (trans != blank_trans)
        gnc_split_register_set_last_num (reg, gnc_basic_cell_get_value (cell));
    }
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, DESC_CELL, TRUE))
  {
    const char *value;

    value = gnc_table_layout_get_cell_value (reg->table->layout, DESC_CELL);

    DEBUG ("DESC: %s", value ? value : "(null)");

    xaccTransSetDescription (trans, value);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, NOTES_CELL, TRUE))
  {
    const char *value;

    value = gnc_table_layout_get_cell_value (reg->table->layout, NOTES_CELL);

    DEBUG ("NOTES: %s", value ? value : "(null)");

    xaccTransSetNotes (trans, value);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, RECN_CELL, TRUE))
  {
    RecnCell *cell;

    cell = (RecnCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                   RECN_CELL);

    DEBUG ("RECN: %c", gnc_recn_cell_get_flag (cell));

    xaccSplitSetReconcile (split, gnc_recn_cell_get_flag (cell));
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, ACTN_CELL, TRUE))
  {
    const char *value;

    value = gnc_table_layout_get_cell_value (reg->table->layout, ACTN_CELL);

    DEBUG ("ACTN: %s", value ? value : "(null)");

    xaccSplitSetAction (split, value);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, MEMO_CELL, TRUE))
  {
    const char *value;

    value = gnc_table_layout_get_cell_value (reg->table->layout, MEMO_CELL);

    DEBUG ("MEMO: %s", value ? value : "(null)");

    xaccSplitSetMemo (split, value);
  }

  /* -------------------------------------------------------------- */
  /* OK, the handling of transfers gets complicated because it depends
   * on what was displayed to the user.  For a multi-line display, we
   * just reparent the indicated split, its it, and that's that. For
   * a two-line display, we want to reparent the "other" split, but
   * only if there is one.  XFRM is the straight split, MXFRM is the
   * mirrored split. */

  if (gnc_table_layout_get_cell_changed (reg->table->layout, XFRM_CELL, TRUE))
  {
    Account *old_acc;
    Account *new_acc;

    old_acc = xaccSplitGetAccount (split);

    new_acc = gnc_split_register_get_account (reg, XFRM_CELL);

    if ((new_acc != NULL) && (old_acc != new_acc))
      xaccAccountInsertSplit (new_acc, split);
  }

  if (reg->style == REG_STYLE_LEDGER && !info->trans_expanded)
    other_split = xaccSplitGetOtherSplit (split);

  if (gnc_table_layout_get_cell_changed (reg->table->layout, MXFRM_CELL, TRUE))
  {
    other_split = xaccSplitGetOtherSplit (split);

    /* other_split may be null for two very different reasons:
     * (1) the parent transaction has three or more splits in it,
     *     and so the "other" split is ambiguous, and thus null.
     * (2) the parent transaction has only this one split as a child.
     *     and "other" is null because there is no other.
     *
     * In the case (2), we want to create the other split, so that 
     * the user's request to transfer actually works out.
     */

    if (!other_split)
    {
      other_split = xaccTransGetSplit (trans, 1);
      if (!other_split)
      {
        other_split = xaccMallocSplit ();
        xaccTransAppendSplit (trans, other_split);
      }
    }

    if (other_split)
    {
      Account *old_acc, *new_acc;

      /* do some reparenting. Insertion into new account will automatically
       * delete from the old account */
      old_acc = xaccSplitGetAccount (other_split);
      new_acc = gnc_split_register_get_account (reg, MXFRM_CELL);

      if ((new_acc != NULL) && (old_acc != new_acc))
        xaccAccountInsertSplit (new_acc, other_split);
    }
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, SHRS_CELL, TRUE))
  {
    PriceCell *cell;
    gnc_numeric amount;

    cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                    SHRS_CELL);
    amount = gnc_price_cell_get_value (cell);

    DEBUG ("SHRS");

    xaccSplitSetAmount (split, amount);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout, PRIC_CELL, TRUE))
  {
    PriceCell *cell;
    gnc_numeric price;

    cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                    PRIC_CELL);
    price = gnc_price_cell_get_value (cell);

    DEBUG ("PRIC");

    xaccSplitSetSharePrice (split, price);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout,
                                         DEBT_CELL, TRUE) ||
      gnc_table_layout_get_cell_changed (reg->table->layout,
                                         CRED_CELL, TRUE))
  {
    PriceCell *cell;
    gnc_numeric new_amount;
    gnc_numeric credit;
    gnc_numeric debit;

    cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                    CRED_CELL);
    credit = gnc_price_cell_get_value (cell);

    cell = (PriceCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                    DEBT_CELL);
    debit  = gnc_price_cell_get_value (cell);

    new_amount = gnc_numeric_sub_fixed (debit, credit);

    xaccSplitSetValue (split, new_amount);
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
    xaccSplitScrub (split);

    if (other_split)
    {
      gnc_numeric value = xaccSplitGetValue (split);

      value = gnc_numeric_neg (value);

      xaccSplitSetValue (other_split, value);

      xaccSplitScrub (other_split);
    }
  }
}

static void
gnc_split_register_save_template_cells (gpointer save_data,
                                        gpointer user_data)
{
  SRSaveData    *sd = save_data;
  SplitRegister *reg = user_data;
  SRInfo	*info = gnc_split_register_get_info (reg);
  Transaction   *trans;
  Split         *split;
  Split		*other_split = NULL;
  kvp_frame	*kvpf;
  AccountGroup	*template_ag;
  Account	*template_acc;
  kvp_value	*tag_val;
  BasicCell     *cell;

  DEBUG (" ");

  g_return_if_fail (sd != NULL);

  trans = sd->trans;
  split = sd->split;

  template_acc = xaccAccountLookup (&info->template_account);

  if (gnc_table_layout_get_cell_changed (reg->table->layout,
                                         DATE_CELL, TRUE) ||
      gnc_table_layout_get_cell_changed (reg->table->layout,
                                         NUM_CELL, TRUE)  ||
      gnc_table_layout_get_cell_changed (reg->table->layout,
                                         RECN_CELL, TRUE))
  {
    PERR( "unexpected changed fields in a template register\n" );
  }

  /* We'll be using the Split's KVP frame a lot */
  kvpf = xaccSplitGetSlots (split);

  if (gnc_table_layout_get_cell_changed (reg->table->layout, XFRM_CELL, TRUE))
  {
    Account           *acct;
    const GUID        *acctGUID;

    /* save the account GUID into the kvp_data. */
    acct = gnc_split_register_get_account (reg, XFRM_CELL);
    if (!acct)
    {
      PERR ("unknown account");
      return;
    }

    acctGUID = xaccAccountGetGUID (acct);

    /* FIXME: replace these with #defines - is it ok to #include "SchedXaction.h" ?? */

    kvp_frame_set_slot_path (kvpf, kvp_value_new_guid(acctGUID),
                             GNC_SX_ID, GNC_SX_ACCOUNT, NULL);

    kvpf = xaccSplitGetSlots (split);

    cell = gnc_table_layout_get_cell (reg->table->layout, XFRM_CELL);
    gnc_basic_cell_set_changed (cell, FALSE);

    /* set the actual account to the fake account for these templates */
    xaccAccountInsertSplit (template_acc, split);
  }

  if ( gnc_table_layout_get_cell_changed (reg->table->layout,
                                          MXFRM_CELL, TRUE) )
  {
    DEBUG( "Template: Got MXFRM changed\n" );

    cell = gnc_table_layout_get_cell (reg->table->layout, MXFRM_CELL);
    gnc_basic_cell_set_changed (cell, FALSE);
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout,
                                         FCRED_CELL, TRUE) ||
      gnc_table_layout_get_cell_changed (reg->table->layout,
                                         FDEBT_CELL, TRUE))
  {
    const char *value;
    char *amountStr = "x + y/42";
    gnc_numeric new_amount;
    gnc_numeric credit;
    gnc_numeric debit;

    DEBUG ("kvp_frame before: %s\n", kvp_frame_to_string (kvpf));

    /* amountStr = gnc_numeric_to_string( new_amount ); */

    value = gnc_table_layout_get_cell_value (reg->table->layout, FCRED_CELL);
    kvp_frame_set_slot_path( kvpf,kvp_value_new_string( value ), 
			     GNC_SX_ID,
			     GNC_SX_CREDIT_FORMULA,
                             NULL);

    value = gnc_table_layout_get_cell_value (reg->table->layout, FDEBT_CELL);

    kvp_frame_set_slot_path( kvpf,  
                             kvp_value_new_string( value ),
                             GNC_SX_ID, 
			     GNC_SX_DEBIT_FORMULA,
                             NULL);

    DEBUG( "kvp_frame  after: %s\n", kvp_frame_to_string( kvpf ) );

    /* set the amount to an innocuous value */
    xaccSplitSetValue (split, gnc_numeric_create(0, 1) );
  }

  if (gnc_table_layout_get_cell_changed (reg->table->layout,
                                         SHRS_CELL, TRUE))
  {
    char *sharesStr = "(x + y)/42";

    /* FIXME: shares cells are numeric by definition. */
    DEBUG ("kvp_frame before: %s\n", kvp_frame_to_string (kvpf));

    /* sharesStr = gnc_numeric_to_string( sharesStr ); */
    kvp_frame_set_slot_path( kvpf,
			     kvp_value_new_string( sharesStr ),
			     GNC_SX_ID,
			     GNC_SX_SHARES
                             NULL);

    DEBUG( "kvp_frame  after: %s\n", kvp_frame_to_string( kvpf ) );

    /* set the shares to an innocuous value */
    xaccSplitSetSharePriceAndAmount (split,
                                     gnc_numeric_create(0, 1),
                                     gnc_numeric_create(0, 1));

    cell = gnc_table_layout_get_cell (reg->table->layout, SHRS_CELL);
    gnc_basic_cell_set_changed (cell, FALSE);
  }

  gnc_split_register_save_cells (save_data, user_data);
}

void
gnc_split_register_model_add_save_handlers (TableModel *model)
{
  g_return_if_fail (model != NULL);
  model->save_handler = gnc_split_register_save_cells;
}

void
gnc_template_register_model_add_save_handlers (TableModel *model)
{
  g_return_if_fail (model != NULL);
  model->save_handler = gnc_split_register_save_template_cells;
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

  return sd;
}

void
gnc_split_register_save_data_destroy (SRSaveData *sd)
{
  g_free (sd);
}
