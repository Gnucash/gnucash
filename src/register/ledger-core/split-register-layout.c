/********************************************************************\
 * split-register-layout.c -- split register layout object          *
 * Copyright (C) 1998 Linas Vepstas <linas@linas.org>               *
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

#include "gnc-engine.h"
#include "split-register-layout.h"


/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_REGISTER;


static void
gnc_register_add_cell (TableLayout *layout,
                       const char *cell_name,
                       const char *cell_type_name,
                       const char *sample_text,
                       CellAlignment alignment,
                       gboolean expandable,
                       gboolean span)
{
  BasicCell *cell;

  g_return_if_fail (layout != NULL);
  g_return_if_fail (cell_type_name != NULL);

  cell = gnc_register_make_cell (cell_type_name);

  gnc_basic_cell_set_name (cell, cell_name);
  gnc_basic_cell_set_sample_text (cell, sample_text);
  gnc_basic_cell_set_alignment (cell, alignment);
  gnc_basic_cell_set_expandable (cell, expandable);
  gnc_basic_cell_set_span (cell, span);

  gnc_table_layout_add_cell (layout, cell);
}

static void
copy_cursor_row (TableLayout *layout, CellBlock *to, CellBlock *from, int row)
{
  int col;

  for (col = 0; col < from->num_cols; col++)
  {
    BasicCell *cell;

    cell = gnc_cellblock_get_cell (from, row, col);
    if (!cell || !cell->cell_name)
      continue;

    gnc_table_layout_set_cell (layout, to, cell->cell_name, row, col);
  }
}

static void
gnc_split_register_set_cells (SplitRegister *reg, TableLayout *layout)
{
  CellBlock *curs;
  CellBlock *curs_last;

  switch (reg->type)
  {
    case BANK_REGISTER:
    case CASH_REGISTER:
    case ASSET_REGISTER:
    case CREDIT_REGISTER:
    case LIABILITY_REGISTER:
    case INCOME_REGISTER:
    case EXPENSE_REGISTER:
    case EQUITY_REGISTER:
      {
        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_SINGLE_LEDGER);

        gnc_table_layout_set_cell (layout, curs, DATE_CELL,  0, 0);
        gnc_table_layout_set_cell (layout, curs, NUM_CELL,   0, 1);
        gnc_table_layout_set_cell (layout, curs, DESC_CELL,  0, 2);
        gnc_table_layout_set_cell (layout, curs, MXFRM_CELL, 0, 3);
        gnc_table_layout_set_cell (layout, curs, RECN_CELL,  0, 4);
        if (reg->is_template)
        {
          gnc_table_layout_set_cell (layout, curs, FDEBT_CELL, 0, 5);
          gnc_table_layout_set_cell (layout, curs, FCRED_CELL, 0, 6);
        }
        else
        {
          gnc_table_layout_set_cell (layout, curs, DEBT_CELL,  0, 5);
          gnc_table_layout_set_cell (layout, curs, CRED_CELL,  0, 6);
        }
        gnc_table_layout_set_cell (layout, curs, BALN_CELL,  0, 7);
        gnc_table_layout_set_cell (layout, curs, RATE_CELL, 0, 8);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_DOUBLE_LEDGER);

        copy_cursor_row (layout, curs, curs_last, 0);

        gnc_table_layout_set_cell (layout, curs, ACTN_CELL,  1, 1);
        gnc_table_layout_set_cell (layout, curs, NOTES_CELL, 1, 2);
        gnc_table_layout_set_cell (layout, curs, VNOTES_CELL, 1, 3);

        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_SINGLE_JOURNAL);

        gnc_table_layout_set_cell (layout, curs, DATE_CELL,  0, 0);
        gnc_table_layout_set_cell (layout, curs, NUM_CELL,   0, 1);
        gnc_table_layout_set_cell (layout, curs, DESC_CELL,  0, 2);
        gnc_table_layout_set_cell (layout, curs, TDEBT_CELL, 0, 5);
        gnc_table_layout_set_cell (layout, curs, TCRED_CELL, 0, 6);
        gnc_table_layout_set_cell (layout, curs, TBALN_CELL, 0, 7);
        gnc_table_layout_set_cell (layout, curs, RATE_CELL, 0, 8);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_DOUBLE_JOURNAL);

        copy_cursor_row (layout, curs, curs_last, 0);

        gnc_table_layout_set_cell (layout, curs, NOTES_CELL, 1, 2);
        gnc_table_layout_set_cell (layout, curs, VNOTES_CELL, 1, 3);

        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_SPLIT);

        gnc_table_layout_set_cell (layout, curs, ACTN_CELL, 0, 1);
        gnc_table_layout_set_cell (layout, curs, MEMO_CELL, 0, 2);
        gnc_table_layout_set_cell (layout, curs, XFRM_CELL, 0, 3);
        gnc_table_layout_set_cell (layout, curs, RECN_CELL, 0, 4);
        if (reg->is_template)
        {
          gnc_table_layout_set_cell (layout, curs, FDEBT_CELL, 0, 5);
          gnc_table_layout_set_cell (layout, curs, FCRED_CELL, 0, 6);
        }
        else
        {
          gnc_table_layout_set_cell (layout, curs, DEBT_CELL, 0, 5);
          gnc_table_layout_set_cell (layout, curs, CRED_CELL, 0, 6);
        }
        gnc_table_layout_set_cell (layout, curs, RATE_CELL, 0, 8);

        break;
      }
      /* --------------------------------------------------------- */

    case PAYABLE_REGISTER:
    case RECEIVABLE_REGISTER:
      {
        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_SINGLE_LEDGER);

        gnc_table_layout_set_cell (layout, curs, DATE_CELL,  0, 0);
        gnc_table_layout_set_cell (layout, curs, TYPE_CELL,  0, 1);
        gnc_table_layout_set_cell (layout, curs, DDUE_CELL,  0, 2);
        gnc_table_layout_set_cell (layout, curs, NUM_CELL,   0, 3);
        gnc_table_layout_set_cell (layout, curs, DESC_CELL,  0, 4);
        gnc_table_layout_set_cell (layout, curs, MXFRM_CELL, 0, 5);
        gnc_table_layout_set_cell (layout, curs, DEBT_CELL,  0, 6);
        gnc_table_layout_set_cell (layout, curs, CRED_CELL,  0, 7);
        gnc_table_layout_set_cell (layout, curs, BALN_CELL,  0, 8);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_DOUBLE_LEDGER);

        copy_cursor_row (layout, curs, curs_last, 0);

        gnc_table_layout_set_cell (layout, curs, ACTN_CELL,  1, 3);
        gnc_table_layout_set_cell (layout, curs, MEMO_CELL, 1, 4);

        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_SINGLE_JOURNAL);

        gnc_table_layout_set_cell (layout, curs, DATE_CELL,  0, 0);
        gnc_table_layout_set_cell (layout, curs, TYPE_CELL,  0, 1);
        gnc_table_layout_set_cell (layout, curs, DDUE_CELL,  0, 2);
        gnc_table_layout_set_cell (layout, curs, NUM_CELL,   0, 3);
        gnc_table_layout_set_cell (layout, curs, DESC_CELL,  0, 4);

        gnc_table_layout_set_cell (layout, curs, TDEBT_CELL, 0, 6);
        gnc_table_layout_set_cell (layout, curs, TCRED_CELL, 0, 7);
        gnc_table_layout_set_cell (layout, curs, TBALN_CELL, 0, 8);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_DOUBLE_JOURNAL);

        copy_cursor_row (layout, curs, curs_last, 0);

        gnc_table_layout_set_cell (layout, curs, MEMO_CELL, 1, 4);

        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_SPLIT);

        gnc_table_layout_set_cell (layout, curs, ACTN_CELL, 0, 3);
        gnc_table_layout_set_cell (layout, curs, MEMO_CELL, 0, 4);
        gnc_table_layout_set_cell (layout, curs, XFRM_CELL, 0, 5);
        gnc_table_layout_set_cell (layout, curs, DEBT_CELL, 0, 6);
        gnc_table_layout_set_cell (layout, curs, CRED_CELL, 0, 7);

        break;
      }

      /* --------------------------------------------------------- */
    case INCOME_LEDGER:
    case GENERAL_LEDGER:
    case SEARCH_LEDGER:
      {
        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_SINGLE_LEDGER);

        gnc_table_layout_set_cell (layout, curs, DATE_CELL,  0, 0);
        gnc_table_layout_set_cell (layout, curs, NUM_CELL,   0, 1);
        gnc_table_layout_set_cell (layout, curs, DESC_CELL,  0, 2);
        gnc_table_layout_set_cell (layout, curs, MXFRM_CELL, 0, 3);
        gnc_table_layout_set_cell (layout, curs, RECN_CELL,  0, 4);
        if (reg->is_template)
        {
          gnc_table_layout_set_cell (layout, curs, FDEBT_CELL,  0, 5);
          gnc_table_layout_set_cell (layout, curs, FCRED_CELL,  0, 6);
        }
        else
        {
          gnc_table_layout_set_cell (layout, curs, DEBT_CELL,  0, 5);
          gnc_table_layout_set_cell (layout, curs, CRED_CELL,  0, 6);
        }
        gnc_table_layout_set_cell (layout, curs, RATE_CELL, 0, 7);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_DOUBLE_LEDGER);

        copy_cursor_row (layout, curs, curs_last, 0);

        gnc_table_layout_set_cell (layout, curs, ACTN_CELL,  1, 1);
        gnc_table_layout_set_cell (layout, curs, NOTES_CELL, 1, 2);
        gnc_table_layout_set_cell (layout, curs, VNOTES_CELL, 1, 3);

        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_SINGLE_JOURNAL);

        gnc_table_layout_set_cell (layout, curs, DATE_CELL,  0, 0);
        gnc_table_layout_set_cell (layout, curs, NUM_CELL,   0, 1);
        gnc_table_layout_set_cell (layout, curs, DESC_CELL,  0, 2);
        gnc_table_layout_set_cell (layout, curs, TDEBT_CELL, 0, 5);
        gnc_table_layout_set_cell (layout, curs, TCRED_CELL, 0, 6);
        gnc_table_layout_set_cell (layout, curs, RATE_CELL, 0, 7);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_DOUBLE_JOURNAL);

        copy_cursor_row (layout, curs, curs_last, 0);

        gnc_table_layout_set_cell (layout, curs, NOTES_CELL, 1, 2);
        gnc_table_layout_set_cell (layout, curs, VNOTES_CELL, 1, 3);

        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_SPLIT);

        gnc_table_layout_set_cell (layout, curs, ACTN_CELL, 0, 1);
        gnc_table_layout_set_cell (layout, curs, MEMO_CELL, 0, 2);
        gnc_table_layout_set_cell (layout, curs, XFRM_CELL, 0, 3);
        gnc_table_layout_set_cell (layout, curs, RECN_CELL, 0, 4);
        if (reg->is_template)
        {
          gnc_table_layout_set_cell (layout, curs, FDEBT_CELL,  0, 5);
          gnc_table_layout_set_cell (layout, curs, FCRED_CELL,  0, 6);
        }
        else
        {
          gnc_table_layout_set_cell (layout, curs, DEBT_CELL,  0, 5);
          gnc_table_layout_set_cell (layout, curs, CRED_CELL,  0, 6);
        }
        gnc_table_layout_set_cell (layout, curs, RATE_CELL, 0, 7);

        break;
      }

      /* --------------------------------------------------------- */
    case STOCK_REGISTER:
    case CURRENCY_REGISTER:
      {
        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_SINGLE_LEDGER);

        gnc_table_layout_set_cell (layout, curs, DATE_CELL,  0,  0);
        gnc_table_layout_set_cell (layout, curs, NUM_CELL,   0,  1);
        gnc_table_layout_set_cell (layout, curs, DESC_CELL,  0,  2);
        gnc_table_layout_set_cell (layout, curs, MXFRM_CELL, 0,  3);
        gnc_table_layout_set_cell (layout, curs, RECN_CELL,  0,  4);
        gnc_table_layout_set_cell (layout, curs, SHRS_CELL,  0,  5);
        gnc_table_layout_set_cell (layout, curs, PRIC_CELL,  0,  6);
        gnc_table_layout_set_cell (layout, curs, DEBT_CELL,  0,  7);
        gnc_table_layout_set_cell (layout, curs, CRED_CELL,  0,  8);
        gnc_table_layout_set_cell (layout, curs, BALN_CELL,  0,  9);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_DOUBLE_LEDGER);

        copy_cursor_row (layout, curs, curs_last, 0);

        gnc_table_layout_set_cell (layout, curs, ACTN_CELL,  1, 1);
        gnc_table_layout_set_cell (layout, curs, NOTES_CELL, 1, 2);
        gnc_table_layout_set_cell (layout, curs, VNOTES_CELL, 1, 3);

        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_SINGLE_JOURNAL);

        gnc_table_layout_set_cell (layout, curs, DATE_CELL,  0,  0);
        gnc_table_layout_set_cell (layout, curs, NUM_CELL,   0,  1);
        gnc_table_layout_set_cell (layout, curs, DESC_CELL,  0,  2);
        gnc_table_layout_set_cell (layout, curs, TSHRS_CELL, 0,  5);
        gnc_table_layout_set_cell (layout, curs, TDEBT_CELL, 0,  7);
        gnc_table_layout_set_cell (layout, curs, TCRED_CELL, 0,  8);
        gnc_table_layout_set_cell (layout, curs, TBALN_CELL, 0,  9);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_DOUBLE_JOURNAL);

        copy_cursor_row (layout, curs, curs_last, 0);

        gnc_table_layout_set_cell (layout, curs, NOTES_CELL, 1, 2);
        gnc_table_layout_set_cell (layout, curs, VNOTES_CELL, 1, 3);

        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_SPLIT);

        gnc_table_layout_set_cell (layout, curs, ACTN_CELL, 0, 1);
        gnc_table_layout_set_cell (layout, curs, MEMO_CELL, 0, 2);
        gnc_table_layout_set_cell (layout, curs, XFRM_CELL, 0, 3);
        gnc_table_layout_set_cell (layout, curs, RECN_CELL, 0, 4);
        gnc_table_layout_set_cell (layout, curs, SHRS_CELL, 0, 5);
        gnc_table_layout_set_cell (layout, curs, PRIC_CELL, 0, 6);
        gnc_table_layout_set_cell (layout, curs, DEBT_CELL, 0, 7);
        gnc_table_layout_set_cell (layout, curs, CRED_CELL, 0, 8);

        break;
      }

      /* --------------------------------------------------------- */
    case PORTFOLIO_LEDGER:
      {
        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_SINGLE_LEDGER);

        gnc_table_layout_set_cell (layout, curs, DATE_CELL,  0, 0);
        gnc_table_layout_set_cell (layout, curs, NUM_CELL,   0, 1);
        gnc_table_layout_set_cell (layout, curs, DESC_CELL,  0, 2);
        gnc_table_layout_set_cell (layout, curs, MXFRM_CELL, 0, 3);
        gnc_table_layout_set_cell (layout, curs, RECN_CELL,  0, 4);
        gnc_table_layout_set_cell (layout, curs, SHRS_CELL,  0, 5);
        gnc_table_layout_set_cell (layout, curs, PRIC_CELL,  0, 6);
        gnc_table_layout_set_cell (layout, curs, DEBT_CELL,  0, 7);
        gnc_table_layout_set_cell (layout, curs, CRED_CELL,  0, 8);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_DOUBLE_LEDGER);

        copy_cursor_row (layout, curs, curs_last, 0);

        gnc_table_layout_set_cell (layout, curs, ACTN_CELL,  1, 1);
        gnc_table_layout_set_cell (layout, curs, NOTES_CELL, 1, 2);
        gnc_table_layout_set_cell (layout, curs, VNOTES_CELL, 1, 3);

        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_SINGLE_JOURNAL);

        gnc_table_layout_set_cell (layout, curs, DATE_CELL,  0, 0);
        gnc_table_layout_set_cell (layout, curs, NUM_CELL,   0, 1);
        gnc_table_layout_set_cell (layout, curs, DESC_CELL,  0, 2);
        gnc_table_layout_set_cell (layout, curs, TSHRS_CELL, 0, 5);
        gnc_table_layout_set_cell (layout, curs, TDEBT_CELL, 0, 7);
        gnc_table_layout_set_cell (layout, curs, TCRED_CELL, 0, 8);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_DOUBLE_JOURNAL);

        copy_cursor_row (layout, curs, curs_last, 0);

        gnc_table_layout_set_cell (layout, curs, NOTES_CELL, 1, 2);
        gnc_table_layout_set_cell (layout, curs, VNOTES_CELL, 1, 3);

        curs = gnc_table_layout_get_cursor (layout,
                                            CURSOR_SPLIT);

        gnc_table_layout_set_cell (layout, curs, ACTN_CELL, 0, 1);
        gnc_table_layout_set_cell (layout, curs, MEMO_CELL, 0, 2);
        gnc_table_layout_set_cell (layout, curs, XFRM_CELL, 0, 3);
        gnc_table_layout_set_cell (layout, curs, RECN_CELL, 0, 4);
        gnc_table_layout_set_cell (layout, curs, SHRS_CELL, 0, 5);
        gnc_table_layout_set_cell (layout, curs, PRIC_CELL, 0, 6);
        gnc_table_layout_set_cell (layout, curs, DEBT_CELL, 0, 7);
        gnc_table_layout_set_cell (layout, curs, CRED_CELL, 0, 8);

        break;
      }

      /* --------------------------------------------------------- */
    default:
      PERR ("unknown register type %d \n", reg->type);
      break;
  }
}

static void
gnc_split_register_layout_add_cursors (SplitRegister *reg,
                                       TableLayout *layout)
{
  CellBlock *cursor;
  int num_cols;

  switch (reg->type)
  {
    case BANK_REGISTER:
    case CASH_REGISTER:
    case ASSET_REGISTER:
    case CREDIT_REGISTER:
    case LIABILITY_REGISTER:
    case INCOME_REGISTER:
    case EXPENSE_REGISTER:
    case EQUITY_REGISTER:
      num_cols = 9;
      break;

    case PAYABLE_REGISTER:
    case RECEIVABLE_REGISTER:
      num_cols = 9;
      break;

    case INCOME_LEDGER:
    case GENERAL_LEDGER:
    case SEARCH_LEDGER:
      num_cols = 8;
      break;

    case STOCK_REGISTER:
    case CURRENCY_REGISTER:
      num_cols = 10;
      break;

    case PORTFOLIO_LEDGER:
      num_cols = 9;
      break;

    default:
      PERR("Bad register type");
      g_assert (FALSE);
      return;
  }

  cursor = gnc_cellblock_new (1, num_cols, CURSOR_HEADER);
  gnc_table_layout_add_cursor (layout, cursor);

  /* cursors used in ledger mode */
  cursor = gnc_cellblock_new (1, num_cols, CURSOR_SINGLE_LEDGER);
  gnc_table_layout_add_cursor (layout, cursor);

  gnc_table_layout_set_primary_cursor (layout, cursor);

  cursor = gnc_cellblock_new (2, num_cols, CURSOR_DOUBLE_LEDGER);
  gnc_table_layout_add_cursor (layout, cursor);

  /* cursors used for journal mode */
  cursor = gnc_cellblock_new (1, num_cols, CURSOR_SINGLE_JOURNAL);
  gnc_table_layout_add_cursor (layout, cursor);

  cursor = gnc_cellblock_new (2, num_cols, CURSOR_DOUBLE_JOURNAL);
  gnc_table_layout_add_cursor (layout, cursor);

  cursor = gnc_cellblock_new (1, num_cols, CURSOR_SPLIT);
  gnc_table_layout_add_cursor (layout, cursor);
}

static void
gnc_split_register_layout_add_cells (SplitRegister *reg,
                                     TableLayout *layout)
{
  gnc_register_add_cell (layout,
                         DATE_CELL,
                         DATE_CELL_TYPE_NAME,
                         N_("sample:12/12/2000") + 7,
                         CELL_ALIGN_RIGHT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         DDUE_CELL,
                         DATE_CELL_TYPE_NAME,
                         N_("sample:12/12/2000") + 7,
                         CELL_ALIGN_RIGHT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         NUM_CELL,
                         NUM_CELL_TYPE_NAME,
                         /* Translators: The 'sample:' items are
                            strings which are not displayed, but only
                            used to estimate widths. Please only
                            translate the portion after the ':' and
                            leave the rest ("sample:") as is. */
                         N_("sample:99999") + 7,
                         CELL_ALIGN_LEFT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         DESC_CELL,
                         QUICKFILL_CELL_TYPE_NAME,
                         N_("sample:Description of a transaction") + 7,
                         CELL_ALIGN_LEFT,
                         TRUE,
                         FALSE);

  gnc_register_add_cell (layout,
                         RATE_CELL,
                         PRICE_CELL_TYPE_NAME,
                         NULL,
                         CELL_ALIGN_RIGHT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         RECN_CELL,
                         RECN_CELL_TYPE_NAME,
                         N_("Reconciled:R") + 11,
                         CELL_ALIGN_CENTER,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         BALN_CELL,
                         PRICE_CELL_TYPE_NAME,
                         N_("sample:999,999.000") + 7,
                         CELL_ALIGN_RIGHT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         XFRM_CELL,
                         COMBO_CELL_TYPE_NAME,
                         N_("Transfer"),
                         CELL_ALIGN_LEFT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         MXFRM_CELL,
                         COMBO_CELL_TYPE_NAME,
                         N_("sample:Expenses:Automobile:Gasoline") + 7,
                         CELL_ALIGN_RIGHT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         ACTN_CELL,
                         COMBO_CELL_TYPE_NAME,
                         N_("sample:Expenses:Automobile:Gasoline") + 7,
                         CELL_ALIGN_RIGHT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         MEMO_CELL,
                         QUICKFILL_CELL_TYPE_NAME,
                         N_("sample:Memo field sample text string") + 7,
                         CELL_ALIGN_LEFT,
                         FALSE,
                         TRUE);

  gnc_register_add_cell (layout,
                         DEBT_CELL,
                         PRICE_CELL_TYPE_NAME,
                         N_("sample:999,999.000") + 7,
                         CELL_ALIGN_RIGHT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         CRED_CELL,
                         PRICE_CELL_TYPE_NAME,
                         N_("sample:999,999.000") + 7,
                         CELL_ALIGN_RIGHT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         SHRS_CELL,
                         PRICE_CELL_TYPE_NAME,
                         N_("sample:999,999.000") + 7,
                         CELL_ALIGN_RIGHT,
                         FALSE,
                         FALSE);

  /* Price cell must come after shares cell, as its callback performs
   * a computation on the value set by the shares cell callback. */
  gnc_register_add_cell (layout,
                         PRIC_CELL,
                         PRICE_CELL_TYPE_NAME,
                         N_("sample:999,999.000") + 7,
                         CELL_ALIGN_RIGHT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         TDEBT_CELL,
                         PRICE_CELL_TYPE_NAME,
                         N_("sample:999,999.000") + 7,
                         CELL_ALIGN_RIGHT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         TCRED_CELL,
                         PRICE_CELL_TYPE_NAME,
                         N_("sample:999,999.000") + 7,
                         CELL_ALIGN_RIGHT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         TSHRS_CELL,
                         PRICE_CELL_TYPE_NAME,
                         N_("sample:999,999.000") + 7,
                         CELL_ALIGN_RIGHT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         TBALN_CELL,
                         PRICE_CELL_TYPE_NAME,
                         N_("sample:999,999.000") + 7,
                         CELL_ALIGN_RIGHT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         TYPE_CELL,
                         RECN_CELL_TYPE_NAME,
                         N_("Type:T") + 5,
                         CELL_ALIGN_LEFT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         NOTES_CELL,
                         QUICKFILL_CELL_TYPE_NAME,
                         N_("sample:Notes field sample text string") + 7,
                         CELL_ALIGN_LEFT,
                         FALSE,
                         TRUE);

  gnc_register_add_cell (layout,
                         VNOTES_CELL,
                         BASIC_CELL_TYPE_NAME,
                         N_("sample:No Particular Reason") + 7,
                         CELL_ALIGN_RIGHT,
                         FALSE,
                         TRUE);

  gnc_register_add_cell (layout,
                         FCRED_CELL,
                         FORMULA_CELL_TYPE_NAME,
                         N_("sample:(x + 0.33 * y + (x+y) )") + 7,
                         CELL_ALIGN_LEFT,
                         FALSE,
                         FALSE);

  gnc_register_add_cell (layout,
                         FDEBT_CELL,
                         FORMULA_CELL_TYPE_NAME,
                         N_("sample:(x + 0.33 * y + (x+y) )") + 7,
                         CELL_ALIGN_LEFT,
                         FALSE,
                         FALSE);
}

TableLayout *
gnc_split_register_layout_new (SplitRegister *reg)
{
  TableLayout *layout;

  layout = gnc_table_layout_new ();

  gnc_split_register_layout_add_cells (reg, layout);
  gnc_split_register_layout_add_cursors (reg, layout);
  gnc_split_register_set_cells (reg, layout);

  return layout;
}
