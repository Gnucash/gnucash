/********************************************************************\
 * splitreg.c -- general ledger object build on top of table object *
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

/*
 * FILE:
 * splitreg.c
 *
 * FUNCTION:
 * Implements the register object.
 * Specifies the physical layout of the register cells.
 * See the header file for additional documentation.
 *
 * hack alert -- most of the code in this file should be 
 * replaced by a guile/scheme based config file.
 *
 * HISTORY:
 * Copyright (c) 1998, 1999, 2000 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas <dave@krondo.com>
 */

#include "config.h"

#include <glib.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-engine-util.h"
#include "messages.h"
#include "splitreg.h"
#include "table-allgui.h"

/* FIXME: these shouldn't be here */
#include "combocell.h"
#include "pricecell.h"
#include "recncell.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_REGISTER;

typedef struct
{
  const char *string;
  int offset;
} sample_string;

static sample_string cell_sample_strings[] =
{
  { N_("sample: 12/12/2000"), 7},                    /* date cell */
  { N_("sample:99999"), 7},                          /* num cell */
  { N_("sample:Description of a transaction"), 7},   /* desc cell */
  { N_("Reconciled:R"), 11},                         /* recn cell */
  { N_("sample:999,999.000"), 7},                    /* balance cell */
  { N_("Transfer"), 0},                              /* action cell */
  { N_("sample:Expenses:Automobile:Gasoline"), 7},   /* xfrm cell */
  { N_("sample:Memo field sample text string"), 7},  /* memo cell */
  { N_("sample:999,999.000"), 7},                    /* credit cell */
  { N_("sample:999,999.000"), 7},                    /* debit cell */
  { N_("sample:999,999.000"), 7},                    /* price cell */
  { N_("sample:999,999.000"), 7},                    /* shares cell */
  { N_("sample:Expenses:Automobile:Gasoline"), 7},   /* mxfrm cell */
  { N_("sample:999,999.000"), 7},                    /* tcredit cell */
  { N_("sample:999,999.000"), 7},                    /* tdebit cell */
  { N_("sample:999,999.000"), 7},                    /* tshares cell */
  { N_("sample:999,999.000"), 7},                    /* tbalance cell */
  { N_("sample:Notes field sample text string"), 7}, /* notes cell */
  { N_("sample:(x + 0.33 * y + (x+y) )"), 7 },       /* formula credit cell */
  { N_("sample:(x + 0.33 * y + (x+y) )"), 7 },       /* formula debit cell */
};

static CellAlignment cell_alignments[] =
{
  CELL_ALIGN_RIGHT,  /* date cell */
  CELL_ALIGN_LEFT,   /* num cell */
  CELL_ALIGN_LEFT,   /* desc cell */
  CELL_ALIGN_CENTER, /* recn cell */
  CELL_ALIGN_RIGHT,  /* balance cell */
  CELL_ALIGN_LEFT,   /* action cell */
  CELL_ALIGN_RIGHT,  /* xfrm cell */
  CELL_ALIGN_LEFT,   /* memo cell */
  CELL_ALIGN_RIGHT,  /* credit cell */
  CELL_ALIGN_RIGHT,  /* debit cell */
  CELL_ALIGN_RIGHT,  /* price cell */
  CELL_ALIGN_RIGHT,  /* shares cell */
  CELL_ALIGN_RIGHT,  /* mxfrm cell */
  CELL_ALIGN_RIGHT,  /* tcredit cell */
  CELL_ALIGN_RIGHT,  /* tdebit cell */
  CELL_ALIGN_RIGHT,  /* tshares cell */
  CELL_ALIGN_RIGHT,  /* tbalance cell */
  CELL_ALIGN_LEFT,   /* notes cell */
  CELL_ALIGN_LEFT,   /* formula credit cell */
  CELL_ALIGN_LEFT,   /* formula debit cell */
};


static void
xaccInitSplitRegister (SplitRegister *reg,
                       SplitRegisterType type,
                       SplitRegisterStyle style,
                       gboolean use_double_line,
                       TableControl *control,
                       TableModel *model,
                       gboolean templateMode);


static void
gnc_register_add_cell (SplitRegister *sr,
                       CellType cell_type,
                       const char *cell_type_name)
{
  BasicCell *cell;

  g_return_if_fail (sr != NULL);
  g_return_if_fail (cell_type_name != NULL);

  cell = gnc_register_make_cell (cell_type_name);

  gnc_table_layout_add_cell (sr->table->layout, cell_type, cell);
}

/* ============================================== */
/* configAction strings into the action cell */
/* hack alert -- this stuff really, really should be in a config file ... */

static void
configAction (SplitRegister *reg)
{
  ComboCell *cell;

  cell = (ComboCell *) gnc_table_layout_get_cell (reg->table->layout,
                                                  ACTN_CELL);

  /* setup strings in the action pull-down */
  switch (reg->type)
  {
    case BANK_REGISTER:
      /* broken ! FIXME bg */
    case SEARCH_LEDGER:  
      xaccAddComboCellMenuItem (cell, _("Deposit"));
      xaccAddComboCellMenuItem (cell, _("Withdraw"));
      xaccAddComboCellMenuItem (cell, _("Check"));
      xaccAddComboCellMenuItem (cell, _("Int"));
      xaccAddComboCellMenuItem (cell, _("ATM"));
      xaccAddComboCellMenuItem (cell, _("Teller"));
      /* Action: Point Of Sale */
      xaccAddComboCellMenuItem (cell, _("POS"));
      xaccAddComboCellMenuItem (cell, _("Phone"));
      xaccAddComboCellMenuItem (cell, _("Online"));
      /* Action: Automatic Deposit ?!? */
      xaccAddComboCellMenuItem (cell, _("AutoDep"));
      xaccAddComboCellMenuItem (cell, _("Wire"));
      xaccAddComboCellMenuItem (cell, _("Credit"));
      xaccAddComboCellMenuItem (cell, _("Direct Debit"));
      xaccAddComboCellMenuItem (cell, _("Transfer"));
      break;
    case CASH_REGISTER:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      break;
    case ASSET_REGISTER:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      xaccAddComboCellMenuItem (cell, _("Fee"));
      break;
    case CREDIT_REGISTER:
      xaccAddComboCellMenuItem (cell, _("ATM"));
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Credit"));
      xaccAddComboCellMenuItem (cell, _("Fee"));
      xaccAddComboCellMenuItem (cell, _("Int"));
      xaccAddComboCellMenuItem (cell, _("Online"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      break;
    case LIABILITY_REGISTER:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      xaccAddComboCellMenuItem (cell, _("Loan"));
      xaccAddComboCellMenuItem (cell, _("Int"));
      xaccAddComboCellMenuItem (cell, _("Payment"));
      break;
    case INCOME_LEDGER:
    case INCOME_REGISTER:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      xaccAddComboCellMenuItem (cell, _("Int"));
      xaccAddComboCellMenuItem (cell, _("Payment"));
      xaccAddComboCellMenuItem (cell, _("Rebate"));
      break;
    case EXPENSE_REGISTER:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      break;
    case GENERAL_LEDGER:
    case EQUITY_REGISTER:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      xaccAddComboCellMenuItem (cell, _("Equity"));
      break;
    case STOCK_REGISTER:
    case PORTFOLIO_LEDGER:
    case CURRENCY_REGISTER:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      xaccAddComboCellMenuItem (cell, _("Price"));
      xaccAddComboCellMenuItem (cell, _("Fee"));
      /* Action: Dividend */
      xaccAddComboCellMenuItem (cell, _("Div")); 
      xaccAddComboCellMenuItem (cell, _("Int"));
      /* Action: Long Term Capital Gains */
      xaccAddComboCellMenuItem (cell, _("LTCG"));
      /* Action: Short Term Capital Gains */
      xaccAddComboCellMenuItem (cell, _("STCG"));
      xaccAddComboCellMenuItem (cell, _("Income"));
      /* Action: Distribution */
      xaccAddComboCellMenuItem (cell, _("Dist")); 
      xaccAddComboCellMenuItem (cell, _("Split"));
      break;

    default:
      xaccAddComboCellMenuItem (cell, _("Buy"));
      xaccAddComboCellMenuItem (cell, _("Sell"));
      break;
  }
}

/* ============================================== */

static void
set_cell (SplitRegister *reg, CellBlock *cursor,
          CellType cell_type, short row, short col)
{
  CellBlock *header;
  CellBlock *cursor_sl;
  CellBlockCell *cb_cell;
  sample_string *ss;

  header = gnc_table_layout_get_cursor (reg->table->layout, CURSOR_HEADER);

  ss = &cell_sample_strings[cell_type];

  cursor->start_col = MIN (cursor->start_col, col);
  cursor->stop_col  = MAX (cursor->stop_col,  col);

  header->start_col = MIN (header->start_col, col);
  header->stop_col  = MAX (header->stop_col,  col);

  cb_cell = gnc_cellblock_get_cell (cursor, row, col);

  cb_cell->cell = gnc_table_layout_get_cell (reg->table->layout, cell_type);
  cb_cell->cell_type = cell_type;
  cb_cell->sample_text = g_strdup (_(ss->string) + ss->offset);
  cb_cell->alignment = cell_alignments[cell_type];
  cb_cell->expandable = cell_type == DESC_CELL;
  cb_cell->span = cell_type == MEMO_CELL || cell_type == NOTES_CELL;

  cb_cell = gnc_cellblock_get_cell (header, row, col);

  cursor_sl = gnc_table_layout_get_cursor (reg->table->layout,
                                           CURSOR_SINGLE_LEDGER);

  if (cb_cell && (cursor == cursor_sl))
  {
    cb_cell->cell = gnc_table_layout_get_cell (reg->table->layout, cell_type);
    cb_cell->cell_type = cell_type;
    cb_cell->sample_text = g_strdup (_(ss->string) + ss->offset);
    cb_cell->alignment = cell_alignments[cell_type];
    cb_cell->expandable = cell_type == DESC_CELL;
    cb_cell->span = cell_type == MEMO_CELL || cell_type == NOTES_CELL;
  }
}

static void
copy_cursor_row (SplitRegister *reg, CellBlock *to, CellBlock *from, int row)
{
  int col;

  for (col = 0; col < from->num_cols; col++)
  {
    CellBlockCell *cb_cell;

    cb_cell = gnc_cellblock_get_cell (from, row, col);
    if (cb_cell->cell_type < 0)
      continue;

    set_cell (reg, to, cb_cell->cell_type, row, col);
  }
}

/* ============================================== */

static void
configLayout (SplitRegister *reg)
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
        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_SINGLE_LEDGER);

        set_cell (reg, curs, DATE_CELL,  0, 0);
        set_cell (reg, curs, NUM_CELL,   0, 1);
        set_cell (reg, curs, DESC_CELL,  0, 2);
        set_cell (reg, curs, MXFRM_CELL, 0, 3);
        set_cell (reg, curs, RECN_CELL,  0, 4);
        if ( reg->template ) {
                set_cell( reg, curs, FDEBT_CELL, 0, 5);
                set_cell( reg, curs, FCRED_CELL, 0, 6);
        } else {
                set_cell (reg, curs, DEBT_CELL,  0, 5);
                set_cell (reg, curs, CRED_CELL,  0, 6);
        }
        set_cell (reg, curs, BALN_CELL,  0, 7);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_DOUBLE_LEDGER);

        copy_cursor_row (reg, curs, curs_last, 0);

        set_cell (reg, curs, ACTN_CELL,  1, 1);
        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_SINGLE_JOURNAL);

        set_cell (reg, curs, DATE_CELL,  0, 0);
        set_cell (reg, curs, NUM_CELL,   0, 1);
        set_cell (reg, curs, DESC_CELL,  0, 2);
        set_cell (reg, curs, TDEBT_CELL, 0, 5);
        set_cell (reg, curs, TCRED_CELL, 0, 6);
        set_cell (reg, curs, TBALN_CELL, 0, 7);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_DOUBLE_JOURNAL);

        copy_cursor_row (reg, curs, curs_last, 0);

        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_SPLIT);

        set_cell (reg, curs, ACTN_CELL, 0, 1);
        set_cell (reg, curs, MEMO_CELL, 0, 2);
        set_cell (reg, curs, XFRM_CELL, 0, 3);
        set_cell (reg, curs, RECN_CELL, 0, 4);
        if ( reg->template ) {
                set_cell( reg, curs, FDEBT_CELL, 0, 5);
                set_cell( reg, curs, FCRED_CELL, 0, 6);
        } else {
                set_cell (reg, curs, DEBT_CELL, 0, 5);
                set_cell (reg, curs, CRED_CELL, 0, 6);
        }

        break;
      }

      /* --------------------------------------------------------- */
    case INCOME_LEDGER:
    case GENERAL_LEDGER:
    case SEARCH_LEDGER:
      {
        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_SINGLE_LEDGER);

        set_cell (reg, curs, DATE_CELL,  0, 0);
        set_cell (reg, curs, NUM_CELL,   0, 1);
        set_cell (reg, curs, DESC_CELL,  0, 2);
        set_cell (reg, curs, MXFRM_CELL, 0, 3);
        set_cell (reg, curs, RECN_CELL,  0, 4);
        if ( reg->template ) {
                set_cell (reg, curs, FDEBT_CELL,  0, 5);
                set_cell (reg, curs, FCRED_CELL,  0, 6);
        } else {
                set_cell (reg, curs, DEBT_CELL,  0, 5);
                set_cell (reg, curs, CRED_CELL,  0, 6);
        }

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_DOUBLE_LEDGER);

        copy_cursor_row (reg, curs, curs_last, 0);

        set_cell (reg, curs, ACTN_CELL,  1, 1);
        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_SINGLE_JOURNAL);

        set_cell (reg, curs, DATE_CELL,  0, 0);
        set_cell (reg, curs, NUM_CELL,   0, 1);
        set_cell (reg, curs, DESC_CELL,  0, 2);
        set_cell (reg, curs, TDEBT_CELL, 0, 5);
        set_cell (reg, curs, TCRED_CELL, 0, 6);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_DOUBLE_JOURNAL);

        copy_cursor_row (reg, curs, curs_last, 0);

        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_SPLIT);

        set_cell (reg, curs, ACTN_CELL, 0, 1);
        set_cell (reg, curs, MEMO_CELL, 0, 2);
        set_cell (reg, curs, XFRM_CELL, 0, 3);
        set_cell (reg, curs, RECN_CELL, 0, 4);
        if ( reg->template ) {
                set_cell (reg, curs, FDEBT_CELL,  0, 5);
                set_cell (reg, curs, FCRED_CELL,  0, 6);
        } else {
                set_cell (reg, curs, DEBT_CELL,  0, 5);
                set_cell (reg, curs, CRED_CELL,  0, 6);
        }

        break;
      }

      /* --------------------------------------------------------- */
    case STOCK_REGISTER:
    case CURRENCY_REGISTER:
      {
        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_SINGLE_LEDGER);

        set_cell (reg, curs, DATE_CELL,  0,  0);
        set_cell (reg, curs, NUM_CELL,   0,  1);
        set_cell (reg, curs, DESC_CELL,  0,  2);
        set_cell (reg, curs, MXFRM_CELL, 0,  3);
        set_cell (reg, curs, RECN_CELL,  0,  4);
        set_cell (reg, curs, SHRS_CELL,  0,  5);
        set_cell (reg, curs, PRIC_CELL,  0,  6);
        set_cell (reg, curs, DEBT_CELL,  0,  7);
        set_cell (reg, curs, CRED_CELL,  0,  8);
        set_cell (reg, curs, BALN_CELL,  0,  9);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_DOUBLE_LEDGER);

        copy_cursor_row (reg, curs, curs_last, 0);

        set_cell (reg, curs, ACTN_CELL,  1, 1);
        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_SINGLE_JOURNAL);

        set_cell (reg, curs, DATE_CELL,  0,  0);
        set_cell (reg, curs, NUM_CELL,   0,  1);
        set_cell (reg, curs, DESC_CELL,  0,  2);
        set_cell (reg, curs, TSHRS_CELL, 0,  5);
        set_cell (reg, curs, TDEBT_CELL, 0,  7);
        set_cell (reg, curs, TCRED_CELL, 0,  8);
        set_cell (reg, curs, TBALN_CELL, 0,  9);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_DOUBLE_JOURNAL);

        copy_cursor_row (reg, curs, curs_last, 0);

        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_SPLIT);

        set_cell (reg, curs, ACTN_CELL, 0, 1);
        set_cell (reg, curs, MEMO_CELL, 0, 2);
        set_cell (reg, curs, XFRM_CELL, 0, 3);
        set_cell (reg, curs, RECN_CELL, 0, 4);
        set_cell (reg, curs, SHRS_CELL, 0, 5);
        set_cell (reg, curs, PRIC_CELL, 0, 6);
        set_cell (reg, curs, DEBT_CELL, 0, 7);
        set_cell (reg, curs, CRED_CELL, 0, 8);

        break;
      }

      /* --------------------------------------------------------- */
    case PORTFOLIO_LEDGER:
      {
        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_SINGLE_LEDGER);

        set_cell (reg, curs, DATE_CELL,  0, 0);
        set_cell (reg, curs, NUM_CELL,   0, 1);
        set_cell (reg, curs, DESC_CELL,  0, 2);
        set_cell (reg, curs, MXFRM_CELL, 0, 3);
        set_cell (reg, curs, RECN_CELL,  0, 4);
        set_cell (reg, curs, SHRS_CELL,  0, 5);
        set_cell (reg, curs, PRIC_CELL,  0, 6);
        set_cell (reg, curs, DEBT_CELL,  0, 7);
        set_cell (reg, curs, CRED_CELL,  0, 8);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_DOUBLE_LEDGER);

        copy_cursor_row (reg, curs, curs_last, 0);

        set_cell (reg, curs, ACTN_CELL,  1, 1);
        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_SINGLE_JOURNAL);

        set_cell (reg, curs, DATE_CELL,  0, 0);
        set_cell (reg, curs, NUM_CELL,   0, 1);
        set_cell (reg, curs, DESC_CELL,  0, 2);
        set_cell (reg, curs, TSHRS_CELL, 0, 5);
        set_cell (reg, curs, TDEBT_CELL, 0, 7);
        set_cell (reg, curs, TCRED_CELL, 0, 8);

        curs_last = curs;
        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_DOUBLE_JOURNAL);

        copy_cursor_row (reg, curs, curs_last, 0);

        set_cell (reg, curs, NOTES_CELL, 1, 2);

        curs = gnc_table_layout_get_cursor (reg->table->layout,
                                            CURSOR_SPLIT);

        set_cell (reg, curs, ACTN_CELL, 0, 1);
        set_cell (reg, curs, MEMO_CELL, 0, 2);
        set_cell (reg, curs, XFRM_CELL, 0, 3);
        set_cell (reg, curs, RECN_CELL, 0, 4);
        set_cell (reg, curs, SHRS_CELL, 0, 5);
        set_cell (reg, curs, PRIC_CELL, 0, 6);
        set_cell (reg, curs, DEBT_CELL, 0, 7);
        set_cell (reg, curs, CRED_CELL, 0, 8);

        break;
      }
      /* --------------------------------------------------------- */
    default:
      PERR ("unknown register type %d \n", reg->type);
      break;
  }
}

/* ============================================== */

SplitRegister *
gnc_register_new (SplitRegisterType type,
                  SplitRegisterStyle style,
                  gboolean use_double_line,
                  TableControl *control,
                  TableModel *model,
                  gboolean templateMode)
{
  SplitRegister * reg;

  g_return_val_if_fail (model != NULL, NULL);

  reg = g_new0 (SplitRegister, 1);

  model->handler_user_data = reg;

  if (type >= NUM_SINGLE_REGISTER_TYPES)
    style = REG_STYLE_JOURNAL;

  xaccInitSplitRegister (reg,
                         type,
                         style,
                         use_double_line,
                         control,
                         model,
                         templateMode);

  return reg;
}

/* ============================================== */

static void
mallocCursors (SplitRegister *reg)
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
      num_cols = 8;
      break;

    case INCOME_LEDGER:
    case GENERAL_LEDGER:
    case SEARCH_LEDGER:
      num_cols = 7;
      break;

    case STOCK_REGISTER:
    case CURRENCY_REGISTER:
      num_cols = 11;
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
  gnc_table_layout_add_cursor (reg->table->layout, cursor);

  /* cursors used in ledger mode */
  cursor = gnc_cellblock_new (1, num_cols, CURSOR_SINGLE_LEDGER);
  gnc_table_layout_add_cursor (reg->table->layout, cursor);

  cursor = gnc_cellblock_new (2, num_cols, CURSOR_DOUBLE_LEDGER);
  gnc_table_layout_add_cursor (reg->table->layout, cursor);

  /* cursors used for journal mode */
  cursor = gnc_cellblock_new (1, num_cols, CURSOR_SINGLE_JOURNAL);
  gnc_table_layout_add_cursor (reg->table->layout, cursor);

  cursor = gnc_cellblock_new (2, num_cols, CURSOR_DOUBLE_JOURNAL);
  gnc_table_layout_add_cursor (reg->table->layout, cursor);

  cursor = gnc_cellblock_new (1, num_cols, CURSOR_SPLIT);
  gnc_table_layout_add_cursor (reg->table->layout, cursor);
}

/* ============================================== */

static void 
xaccInitSplitRegister (SplitRegister *reg,
                       SplitRegisterType type,
                       SplitRegisterStyle style,
                       gboolean use_double_line,
                       TableControl *control,
                       TableModel *model,
                       gboolean templateMode)
{
  reg->table = gnc_table_new (control, model);

  reg->user_data = NULL;
  reg->destroy = NULL;

  reg->type = type;
  reg->style = style;
  reg->use_double_line = use_double_line;
  reg->template = templateMode;

  /* --------------------------- */
  /* define the number of columns in the display, malloc the cursors */
  mallocCursors (reg);

  /* --------------------------- */
  /* malloc the workhorse cells */

  gnc_register_add_cell (reg, DATE_CELL,  DATE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, NUM_CELL,   NUM_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, DESC_CELL,  QUICKFILL_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, RECN_CELL,  RECN_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, BALN_CELL,  PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, XFRM_CELL,  COMBO_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, ACTN_CELL,  COMBO_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, MEMO_CELL,  QUICKFILL_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, CRED_CELL,  PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, DEBT_CELL,  PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, PRIC_CELL,  PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, SHRS_CELL,  PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, MXFRM_CELL, COMBO_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, TCRED_CELL, PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, TDEBT_CELL, PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, TSHRS_CELL, PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, TBALN_CELL, PRICE_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, NOTES_CELL, QUICKFILL_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, FCRED_CELL, QUICKFILL_CELL_TYPE_NAME);
  gnc_register_add_cell (reg, FDEBT_CELL, QUICKFILL_CELL_TYPE_NAME);

  /* --------------------------- */

  /* config the layout of the cells in the cursors */
  configLayout (reg);

  /* The num cell is the transaction number */
  xaccSetBasicCellBlankHelp (gnc_table_layout_get_cell (reg->table->layout,
                                                        NUM_CELL),
                             _("Enter the transaction number, such as the "
                               "check number"));

  /* the xfer cells */
  {
    const char *help = _("Enter the account to transfer from, or choose "
                         "one from the list");
    xaccSetBasicCellBlankHelp (gnc_table_layout_get_cell (reg->table->layout,
                                                          MXFRM_CELL), help);
    xaccSetBasicCellBlankHelp (gnc_table_layout_get_cell (reg->table->layout,
                                                          XFRM_CELL), help);
  }

  {
    const char *help = _("This transaction has multiple splits; "
                         "press the Split button to see them all");

    xaccComboCellAddIgnoreString
      ((ComboCell *)
       gnc_table_layout_get_cell (reg->table->layout, MXFRM_CELL),
       _("-- Split Transaction --"), help);
  }

  {
    const char *help = _("This transaction is a stock split; "
                         "press the Split button to see details");

    xaccComboCellAddIgnoreString
      ((ComboCell *)
       gnc_table_layout_get_cell (reg->table->layout, MXFRM_CELL),
       _("-- Stock Split --"), help);
  }

  /* the action cell */
  xaccComboCellSetAutoSize
    ((ComboCell *)
     gnc_table_layout_get_cell (reg->table->layout, ACTN_CELL), TRUE);

  /* the memo cell */
  xaccSetBasicCellBlankHelp
    (gnc_table_layout_get_cell (reg->table->layout, MEMO_CELL),
     _("Enter a description of the split"));

  /* the desc cell */
  xaccSetBasicCellBlankHelp
    (gnc_table_layout_get_cell (reg->table->layout, DESC_CELL),
     _("Enter a description of the transaction"));

  /* the notes cell */
  xaccSetBasicCellBlankHelp
    (gnc_table_layout_get_cell (reg->table->layout, NOTES_CELL),
     _("Enter notes for the transaction"));

  /* the formula cells */
  xaccSetBasicCellBlankHelp
    (gnc_table_layout_get_cell (reg->table->layout, FCRED_CELL),
     _("Enter credit formula for real transaction"));

  xaccSetBasicCellBlankHelp
    (gnc_table_layout_get_cell (reg->table->layout, FDEBT_CELL),
     _("Enter debit formula for real transaction"));

  /* Use 6 decimal places for prices */
  xaccSetPriceCellFraction
    ((PriceCell *)
     gnc_table_layout_get_cell (reg->table->layout, PRIC_CELL), 1000000);

  /* Initialize shares and share balance cells */
  xaccSetPriceCellPrintInfo
    ((PriceCell *) gnc_table_layout_get_cell (reg->table->layout, SHRS_CELL),
     gnc_default_share_print_info ());

  xaccSetPriceCellPrintInfo
    ((PriceCell *) gnc_table_layout_get_cell (reg->table->layout, TSHRS_CELL),
     gnc_default_share_print_info ());

  /* The action cell should accept strings not in the list */
  xaccComboCellSetStrict
    ((ComboCell *)
     gnc_table_layout_get_cell (reg->table->layout, ACTN_CELL), FALSE);

  xaccSetBasicCellBlankHelp
    (gnc_table_layout_get_cell (reg->table->layout, ACTN_CELL),
     _("Enter the type of transaction, or choose "
       "one from the list"));

  /* number format for share quantities in stock ledgers */
  switch (type)
  {
    case CURRENCY_REGISTER:
    case STOCK_REGISTER:
    case PORTFOLIO_LEDGER:
      xaccSetPriceCellPrintInfo
        ((PriceCell *)
         gnc_table_layout_get_cell (reg->table->layout, PRIC_CELL),
         gnc_default_price_print_info ());

      xaccSetBasicCellBlankHelp
        (gnc_table_layout_get_cell (reg->table->layout, PRIC_CELL),
         _("Enter the share price"));

      xaccSetBasicCellBlankHelp
        (gnc_table_layout_get_cell (reg->table->layout, SHRS_CELL),
         _("Enter the number of shares bought or sold"));
      break;
    default:
      break;
  }

  /* add menu items for the action cell */
  configAction (reg);

  /* Set up header */
  {
    VirtualCellLocation vcell_loc = { 0, 0 };
    CellBlock *header;

    header = gnc_table_layout_get_cursor (reg->table->layout, CURSOR_HEADER);

    gnc_table_set_vcell (reg->table, header, NULL, TRUE, TRUE, vcell_loc);
  }

  /* Set up first and only initial row */
  {
    VirtualLocation vloc;
    CellBlock *cursor;

    vloc.vcell_loc.virt_row = 1;
    vloc.vcell_loc.virt_col = 0;
    vloc.phys_row_offset = 0;
    vloc.phys_col_offset = 0;

    cursor = gnc_table_layout_get_cursor (reg->table->layout,
                                          CURSOR_SINGLE_LEDGER);

    gnc_table_set_vcell (reg->table, cursor, NULL, TRUE, TRUE, vloc.vcell_loc);
    gnc_table_move_cursor (reg->table, vloc);
  }
}

/* ============================================== */

void
xaccConfigSplitRegister (SplitRegister *reg,
                         SplitRegisterType newtype,
                         SplitRegisterStyle newstyle,
                         gboolean use_double_line)
{
  if (!reg) return;

  reg->type = newtype;

  if (reg->type >= NUM_SINGLE_REGISTER_TYPES)
    newstyle = REG_STYLE_JOURNAL;

  reg->style = newstyle;
  reg->use_double_line = use_double_line;

  gnc_table_realize_gui (reg->table);
}

/* ============================================== */

void 
xaccDestroySplitRegister (SplitRegister *reg)
{
  GList *node;

  /* give the user a chance to clean up */
  if (reg->destroy)
    (reg->destroy) (reg);

  reg->destroy = NULL;
  reg->user_data = NULL;

  gnc_table_destroy (reg->table);
  reg->table = NULL;

  g_free (reg->debit_str);
  g_free (reg->tdebit_str);
  g_free (reg->credit_str);
  g_free (reg->tcredit_str);

  reg->debit_str = NULL;
  reg->tdebit_str = NULL;
  reg->credit_str = NULL;
  reg->tcredit_str = NULL;

  /* free the memory itself */
  g_free (reg);
}

/* ============================================== */

static CursorClass
sr_cellblock_cursor_class (SplitRegister *reg, CellBlock *cursor)
{
  if (cursor == NULL)
    return CURSOR_CLASS_NONE;

  return xaccCursorNameToClass (cursor->cursor_name);
}

/* ============================================== */

CursorClass
xaccSplitRegisterGetCurrentCursorClass (SplitRegister *reg)
{
  Table *table;

  if (reg == NULL)
    return CURSOR_CLASS_NONE;

  table = reg->table;
  if (table == NULL)
    return CURSOR_CLASS_NONE;

  return sr_cellblock_cursor_class (reg, table->current_cursor);
}

/* ============================================== */

CursorClass
xaccSplitRegisterGetCursorClass (SplitRegister *reg,
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

  return sr_cellblock_cursor_class (reg, vcell->cellblock);
}

/* ============================================== */

CursorClass
xaccCursorNameToClass (const char *cursor_name)
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

/* ============================================== */

CellType
xaccSplitRegisterGetCurrentCellType (SplitRegister *reg)
{
  Table *table;

  if (reg == NULL)
    return NO_CELL;

  table = reg->table;
  if (table == NULL)
    return NO_CELL;

  return xaccSplitRegisterGetCellType(reg, table->current_cursor_loc);
}

/* ============================================== */

static BasicCell *
sr_get_cell (SplitRegister *reg, VirtualLocation virt_loc)
{
  Table *table;
  VirtualCell *vcell;
  CellBlock *cellblock;
  CellBlockCell *cb_cell;

  if (reg == NULL)
    return NULL;

  table = reg->table;
  if (table == NULL)
    return NULL;

  vcell = gnc_table_get_virtual_cell (table, virt_loc.vcell_loc);
  if (vcell == NULL)
    return NULL;

  cellblock = vcell->cellblock;

  cb_cell = gnc_cellblock_get_cell (cellblock,
                                    virt_loc.phys_row_offset,
                                    virt_loc.phys_col_offset);

  if (cb_cell == NULL)
    return NULL;

  return cb_cell->cell;
}

/* ============================================== */

CellType
xaccSplitRegisterGetCellType (SplitRegister *reg, VirtualLocation virt_loc)
{
  BasicCell *cell;

  cell = sr_get_cell (reg, virt_loc);
  if (cell == NULL)
    return NO_CELL;

  return gnc_table_layout_get_cell_type (reg->table->layout, cell);
}

/* ============================================== */

gboolean
xaccSplitRegisterGetCellLoc (SplitRegister *reg, CellType cell_type,
                             VirtualCellLocation vcell_loc,
                             VirtualLocation *virt_loc)
{
  Table *table;
  VirtualCell *vcell;
  CellBlock *cellblock;
  int cell_row, cell_col;

  if (reg == NULL)
    return FALSE;

  table = reg->table;
  if (table == NULL)
    return FALSE;

  vcell = gnc_table_get_virtual_cell (table, vcell_loc);
  if (vcell == NULL)
    return FALSE;

  cellblock = vcell->cellblock;

  for (cell_row = 0; cell_row < cellblock->num_rows; cell_row++)
    for (cell_col = 0; cell_col < cellblock->num_cols; cell_col++)
    {
      CellBlockCell *cb_cell;
      CellType ctype;

      cb_cell = gnc_cellblock_get_cell (cellblock, cell_row, cell_col);
      ctype = gnc_table_layout_get_cell_type (table->layout, cb_cell->cell);

      if (ctype == cell_type)
      {
        if (virt_loc != NULL)
        {
          virt_loc->vcell_loc = vcell_loc;

          virt_loc->phys_row_offset = cell_row;
          virt_loc->phys_col_offset = cell_col;
        }

        return TRUE;
      }
    }

  return FALSE;
}

/* ============================================== */

gboolean
xaccSplitRegisterGetCurrentCellLoc (SplitRegister *reg, CellType cell_type,
                                    VirtualLocation *virt_loc)
{
  Table *table;

  if (reg == NULL)
    return FALSE;

  table = reg->table;
  if (table == NULL)
    return FALSE;

  return xaccSplitRegisterGetCellLoc (reg, cell_type,
                                      table->current_cursor_loc.vcell_loc,
                                      virt_loc);
}

/* keep in sync with CellType enum */
static const char *cell_names[] =
{
  "date",
  "num",
  "description",
  "reconcile",
  "balance",
  "action",
  "account",
  "memo",
  "credit",
  "debit",
  "price",
  "shares",
  "transfer",
  "trans-credit",
  "trans-debit",
  "trans-shares",
  "trans-balance",
  "notes",
  "credit formula",
  "debit formula",
};

const char *
xaccSplitRegisterGetCellTypeName (CellType type)
{
  if (type < 0)
    return NULL;
  if (type >= CELL_TYPE_COUNT)
    return NULL;

  return cell_names[type];
}

CellType
xaccSplitRegisterGetCellTypeFromName (const char *name)
{
  CellType type;

  if (name == NULL)
    return NO_CELL;

  for (type = 0; type < CELL_TYPE_COUNT; type++)
    if (safe_strcmp (name, cell_names[type]) == 0)
      return type;

  return NO_CELL;
}

/* ============ END OF FILE ===================== */
