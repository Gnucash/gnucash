/********************************************************************\
 * splitreg.h -- general ledger object build on top of table object *
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
 * splitreg.h
 *
 * FUNCTION:
 * Implements a basic display register/ledger.
 * This object makes specific cells have specific properties
 * (price, text, date, etc.) and specific names that correspond.
 * It also determines the actual physical layout, arrangement
 * of columns, etc.
 *
 * See src/doc/design/gnucash-design.info for more information.
 *
 * DESIGN HOPES:
 * Should probably move at least some of the layout to a config 
 * file.  Might make good sense to use scheme/guile for the layout.
 *
 * HISTORY:
 * Copyright (c) 1998, 1999, 2000 Linas Vepstas
 */

#ifndef SPLITREG_H
#define SPLITREG_H

#include "Account.h" /* FIXME No Engine headers!!! */

#include "table-allgui.h"

/* defined register types.
 * "registers" are single-account display windows.
 * "ledgers" are multiple-account display windows */
typedef enum
{
  BANK_REGISTER,
  CASH_REGISTER,
  ASSET_REGISTER,
  CREDIT_REGISTER,
  LIABILITY_REGISTER,
  INCOME_REGISTER,
  EXPENSE_REGISTER,
  EQUITY_REGISTER,
  STOCK_REGISTER,
  CURRENCY_REGISTER,
  NUM_SINGLE_REGISTER_TYPES,

  GENERAL_LEDGER = NUM_SINGLE_REGISTER_TYPES,
  INCOME_LEDGER,
  PORTFOLIO_LEDGER,
  SEARCH_LEDGER,

  NUM_REGISTER_TYPES
} SplitRegisterType;

/* Cell Names */
#define DATE_CELL  "date"
#define NUM_CELL   "num"
#define DESC_CELL  "description"
#define RECN_CELL  "reconcile"
#define BALN_CELL  "balance"
#define ACTN_CELL  "action"
#define XFRM_CELL  "account"
#define MEMO_CELL  "memo"
#define DEBT_CELL  "debit"
#define CRED_CELL  "credit"
#define PRIC_CELL  "price"
#define SHRS_CELL  "shares"
#define MXFRM_CELL "transfer"

/* T* cells are transaction summary cells */
#define TDEBT_CELL "trans-debit"
#define TCRED_CELL "trans-credit"
#define TSHRS_CELL "trans-shares"
#define TBALN_CELL "trans-balance"
#define NOTES_CELL "notes"
#define FDEBT_CELL "debit formula"
#define FCRED_CELL "credit-formula"

/*
 * enumerated display styles 
 * REG_SINGLE_LINE    -- show one line per transaction
 * REG_DOUBLE_LINE    -- show two lines per transaction
 * REG_MULTI_LINE     -- show multiple lines per transaction
 * REG_SINGLE_DYNAMIC -- dynamically expand edited transaction,
 *                       all other transactions on one line
 * REG_DOUBLE_DYNAMIC -- dynamically expand edited transaction,
 *                       all other transactions on two lines
 */
typedef enum
{
  REG_STYLE_LEDGER,
  REG_STYLE_AUTO_LEDGER,
  REG_STYLE_JOURNAL
} SplitRegisterStyle;

/* Types of cursors */
typedef enum
{
  CURSOR_CLASS_NONE = -1,
  CURSOR_CLASS_SPLIT,
  CURSOR_CLASS_TRANS,
  NUM_CURSOR_CLASSES
} CursorClass;

#define CURSOR_SINGLE_LEDGER  "cursor-single-ledger"
#define CURSOR_DOUBLE_LEDGER  "cursor-double-ledger"
#define CURSOR_SINGLE_JOURNAL "cursor-single-journal"
#define CURSOR_DOUBLE_JOURNAL "cursor-double-journal"
#define CURSOR_SPLIT          "cursor-split"

typedef struct split_register SplitRegister;

typedef void (*SplitRegisterDestroyCB) (SplitRegister *reg);

struct split_register
{
  /* the table itself that implements the underlying GUI. */
  Table * table;

  SplitRegisterType type;
  SplitRegisterStyle style;
  gboolean use_double_line;

  /* some private data; outsiders should not access this */

  /**
   * A flag indicating a "template" register.
   **/
  gboolean	template; /* FIXME: this should not be here! */

  /**
   * The template account which the transactions in a template
   * splitregister will belong to.
   **/
  Account	*templateAcct; /* FIXME: this should not be here! */

  /* user_data allows users of this object to hang
   * private data onto it */
  gpointer user_data;

  /* The destroy callback gives user's a chance 
   * to free up any associated user_hook data */
  SplitRegisterDestroyCB destroy;

  /* configured strings for debit/credit headers */
  char *debit_str;
  char *credit_str;
  char *tdebit_str;
  char *tcredit_str;
};


SplitRegister *
gnc_register_new (SplitRegisterType type,
                  SplitRegisterStyle style,
                  gboolean use_double_line,
                  TableControl *control,
                  TableModel *model,
                  gboolean templateMode);

void            xaccConfigSplitRegister (SplitRegister *reg,
                                         SplitRegisterType type,
                                         SplitRegisterStyle style,
                                         gboolean use_double_line);

void            xaccDestroySplitRegister (SplitRegister *reg);

/* Returns the type of the current cursor */
CursorClass     xaccSplitRegisterGetCurrentCursorClass (SplitRegister *reg);

/* Returns the type of the cursor at the given virtual row and column. */
CursorClass    xaccSplitRegisterGetCursorClass (SplitRegister *reg,
                                                VirtualCellLocation vcell_loc);

CursorClass     xaccCursorNameToClass (const char *cursor_name);

#endif
