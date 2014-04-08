
#ifdef OBSOLETE 

/*
 * FILE:
 * register.h
 *
 * FUNCTION:
 * Implements a basic display register/ledger.
 * This object makes specific cell have specific properties
 * (price, text, date, etc).  and specific names that correspond.
 * it also determines the actual phyisical layout, arrengement
 * of columns, etc.
 *
 * WARNING: 
 * This register is quasi-broken: it does not display splits.
 * This register is "obsolete", and is here for documentary
 * and exaple reasons only.
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

#ifndef __XACC_REGISTER_H__
#define __XACC_REGISTER_H__

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
#define BANK_REGISTER       0
#define CASH_REGISTER       1
#define ASSET_REGISTER      2
#define CREDIT_REGISTER     3
#define LIABILITY_REGISTER  4
#define INCOME_REGISTER     5
#define EXPENSE_REGISTER    6
#define EQUITY_REGISTER     7
#define STOCK_REGISTER      8

#define GENERAL_LEDGER      9
#define INCOME_LEDGER       10
#define PORTFOLIO           11

/* modified flags -- indicate how values have been modified */
#define MOD_NONE  0x000
#define MOD_DATE  0x001
#define MOD_NUM   0x002
#define MOD_DESC  0x004
#define MOD_RECN  0x008
#define MOD_AMNT  0x010
#define MOD_SHRS  0x020
#define MOD_PRIC  0x040
#define MOD_MEMO  0x080
#define MOD_ACTN  0x100
#define MOD_XFRM  0x200
#define MOD_XTO   0x400
#define MOD_NEW   0x800
#define MOD_ALL   0xfff

#define NUM_CELLS 25

typedef struct _BasicRegister BasicRegister;

struct _BasicRegister {
   /* the table itself that implements the underlying GUI. */
   Table         * table;

   /* the cursors that define the currently edited row */
   CellBlock     * cursor;
   CellBlock     * header;

   /* the individual cells, by function */

   DateCell      * dateCell;
   BasicCell     * numCell;
   QuickFillCell * descCell;
   BasicCell     * recnCell;
   PriceCell     * creditCell;
   PriceCell     * debitCell;
   PriceCell     * shrsCell;
   PriceCell     * priceCell;
   PriceCell     * valueCell;
   BasicCell     * memoCell;
   ComboCell     * actionCell;
   ComboCell     * xfrmCell;
   ComboCell     * xtoCell;
   PriceCell     * balanceCell;

   /* the type of the register, must be one of the enumerated types
    * above *_REGISTER, *_LEDGER, above */
   int type;

   /* some private data; outsiders should not access this */
   short num_cols;
   short num_header_rows;
   char *labels[NUM_CELLS];
   short cols[NUM_CELLS];
   short rows[NUM_CELLS];
   short wids[NUM_CELLS];

   /* user_hook allows users of this object to hang
    * private data onto it */
   void *user_hook;

   /* The destroy callback gives user's a chance 
    * to free up any associated user_hook data */
   void (* destroy) (BasicRegister *);

};

BasicRegister * xaccMallocBasicRegister (int type);
void            xaccInitBasicRegister (BasicRegister *, int type);
void            xaccDestroyBasicRegister (BasicRegister *);

/* returns non-zero value if updates have been made to data */
unsigned int    xaccBasicRegisterGetChangeFlag (BasicRegister *);

#endif /* __XACC_REGISTER_H__ */

#endif /* OBSOLETE */
/* ============ END OF FILE ===================== */
