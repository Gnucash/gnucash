/*
 * FILE:
 * splitreg.h
 *
 * FUNCTION:
 * Implements a basic display register/ledger.
 * This object makes specific cell have specific properties
 * (price, text, date, etc).  and specific names that correspond.
 * it also determines the actual phyisical layout, arrengement
 * of columns, etc.
 *
 * Handles splits
 *
 * If the SHOW_TDETAIL flag is set, then transaction blah blah is shown.
 * Otherwise it is not (useful when not displaying splits, gives old-styule reg).
 * Hack alert -- finish documenting this
 *
 * The xaccConfigSplitRegister() subroutine allows the configuration 
 * of the register to be changed on the fly (dynamically).  In particular,
 * the register type, and/or the flags controlling the register display
 * can be changed on the fly ... 
 *
 * DESIGN HOPES:
 * Should probably move at least some of the layout to a config 
 * file.  Might make good sense to use scheme/guile for the layout.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 */

/********************************************************************\
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#ifndef __XACC_SPLITREG_H__
#define __XACC_SPLITREG_H__

#include "basiccell.h"
#include "cellblock.h"
#include "combocell.h"
#include "datecell.h"
#include "quickfillcell.h"
#include "pricecell.h"
#include "table-allgui.h"

/* defined register types */
/* "registers" are single-account display windows.
 * "ledgers" are multiple-account display windows */
#define BANK_REGISTER       1
#define CASH_REGISTER       2
#define ASSET_REGISTER      3
#define CREDIT_REGISTER     4
#define LIABILITY_REGISTER  5
#define INCOME_REGISTER     6
#define EXPENSE_REGISTER    7
#define EQUITY_REGISTER     8
#define STOCK_REGISTER      9

#define GENERAL_LEDGER      10
#define INCOME_LEDGER       11
#define PORTFOLIO           12
#define REG_TYPE_MASK       0xff

/* 
 * enumerated display styles 
 * REG_DOUBLE_LINE  -- show two lines per transaction
 * REG_MULTI_LINE   -- show multiple lines per transaction
 * REG_DYNAMIC      -- dynamically expand edited transaction
 */
 
#define REG_SINGLE_LINE        (1 << 8)
#define REG_DOUBLE_LINE        (2 << 8)
#define REG_MULTI_LINE         (3 << 8)
#define REG_SINGLE_DYNAMIC     (4 << 8)
#define REG_DOUBLE_DYNAMIC     (5 << 8) 
#define REG_STYLE_MASK      (0xff << 8) 

/* modified flags -- indicate which cell values have been modified by user */
#define MOD_NONE   0x0000
#define MOD_DATE   0x0001
#define MOD_NUM    0x0002
#define MOD_DESC   0x0004
#define MOD_RECN   0x0008

#define MOD_ACTN   0x0010
#define MOD_XFRM   0x0020
#define MOD_MXFRM  0x0040
#define MOD_XTO    0x0080
#define MOD_MEMO   0x0100
#define MOD_AMNT   0x0200
#define MOD_NAMNT  0x0400
#define MOD_PRIC   0x0800
#define MOD_VALU   0x1000
#define MOD_NEW    0x2000
#define MOD_ALL    0x3fff

/* The value of NUM_CELLS should be larger than the number of 
 * cells defined in the structure below!
 */
#define NUM_CELLS 25

typedef struct _SplitRegister SplitRegister;

struct _SplitRegister {
   /* the table itself that implements the underlying GUI. */
   Table         * table;

   /* the cursors that define the currently edited row */
   CellBlock     * single_cursor;
   CellBlock     * double_cursor;
   CellBlock     * trans_cursor;
   CellBlock     * split_cursor;
   CellBlock     * header;

   DateCell      * dateCell;
   BasicCell     * numCell;
   QuickFillCell * descCell;
   BasicCell     * recnCell;   /* main transaction line reconcile */
   PriceCell     * shrsCell;
   PriceCell     * balanceCell;
   BasicCell     * nullCell;

   ComboCell     * actionCell;
   ComboCell     * xfrmCell;
   ComboCell     * mxfrmCell;
   ComboCell     * xtoCell;
   BasicCell     * memoCell;
   PriceCell     * creditCell;
   PriceCell     * debitCell;
   PriceCell     * priceCell;
   PriceCell     * valueCell;

   PriceCell     * ncreditCell;
   PriceCell     * ndebitCell;

   /* the type of the register, must be one of the enumerated types
    * above *_REGISTER, *_LEDGER, above */
   int type;

   /* some private data; outsiders should not access this */
   short num_cols;
   short num_header_rows;

   short num_phys_rows;
   short num_virt_rows;
   short cursor_phys_row;
   short cursor_virt_row;
   void * user_hack;

   BasicCell *header_label_cells[NUM_CELLS];

   /* user_hook allows users of this object to hang
    * private data onto it */
   void *user_hook;

   /* The destroy callback gives user's a chance 
    * to free up any associated user_hook data */
   void (* destroy) (SplitRegister *);

};

SplitRegister * xaccMallocSplitRegister (int type);
void            xaccInitSplitRegister (SplitRegister *, int type);
void            xaccConfigSplitRegister (SplitRegister *, int type);
void            xaccDestroySplitRegister (SplitRegister *);

/* returns non-zero value if updates have been made to data */
unsigned int    xaccSplitRegisterGetChangeFlag (SplitRegister *);

#endif /* __XACC_SPLITREG_H__ */

/* ============ END OF FILE ===================== */
