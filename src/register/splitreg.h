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

#ifndef __XACC_SPLITREG_H__
#define __XACC_SPLITREG_H__

#include "basiccell.h"
#include "cellblock.h"
#include "combocell.h"
#include "datecell.h"
#include "quickfillcell.h"
#include "pricecell.h"
#include "numcell.h"
#include "recncell.h"
#include "table-allgui.h"

/* defined register types */
/* "registers" are single-account display windows.
 * "ledgers" are multiple-account display windows */
typedef enum
{
  BANK_REGISTER       =  1,
  CASH_REGISTER       =  2,
  ASSET_REGISTER      =  3,
  CREDIT_REGISTER     =  4,
  LIABILITY_REGISTER  =  5,
  INCOME_REGISTER     =  6,
  EXPENSE_REGISTER    =  7,
  EQUITY_REGISTER     =  8,
  STOCK_REGISTER      =  9,
  CURRENCY_REGISTER   = 10,

  GENERAL_LEDGER      = 11,
  INCOME_LEDGER       = 12,
  PORTFOLIO_LEDGER    = 13,
  SEARCH_LEDGER       = 14
} SplitRegisterType;

/* These values are used to identify the cells in the register. */
/* Keep these in sync with the cell_names array in splitreg.c.  */
typedef enum
{
  NO_CELL    = -1,
  DATE_CELL  =  0,
  NUM_CELL,
  DESC_CELL,
  RECN_CELL,
  SHRBALN_CELL,
  BALN_CELL,
  ACTN_CELL,
  XFRM_CELL,
  XTO_CELL,
  MEMO_CELL,
  CRED_CELL,
  DEBT_CELL,
  PRIC_CELL,
  SHRS_CELL,

  /* MXFRM is the "mirrored" transfer-from account */
  MXFRM_CELL,

  CELL_TYPE_COUNT
} CellType;

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
  REG_SINGLE_LINE    = 1,
  REG_DOUBLE_LINE    = 2,
  REG_MULTI_LINE     = 3,
  REG_SINGLE_DYNAMIC = 4,
  REG_DOUBLE_DYNAMIC = 5
} SplitRegisterStyle;

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
#define MOD_PRIC   0x0400
#define MOD_SHRS   0x0800
#define MOD_NEW    0x1000
#define MOD_ALL    0x1fff

/* Types of cursors */
typedef enum
{
  CURSOR_SPLIT,
  CURSOR_TRANS,
  CURSOR_NONE
} CursorClass;

/* The value of NUM_CELLS should be larger than the number of 
 * cells defined in the structure below!
 */
#define NUM_CELLS 25

typedef struct _SplitRegisterBuffer SplitRegisterBuffer;
typedef struct _SplitRegister SplitRegister;

typedef void (*SplitRegisterDestroyCB) (SplitRegister *reg);

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
   NumCell       * numCell;
   QuickFillCell * descCell;
   RecnCell      * recnCell;   /* main transaction line reconcile */
   PriceCell     * shrbalnCell;
   PriceCell     * balanceCell;
   BasicCell     * nullCell;

   ComboCell     * actionCell;
   ComboCell     * xfrmCell;
   ComboCell     * mxfrmCell;
   ComboCell     * xtoCell;
   QuickFillCell * memoCell;
   PriceCell     * creditCell;
   PriceCell     * debitCell;
   PriceCell     * priceCell;
   PriceCell     * sharesCell;

   /* The type of the register, must be one of the enumerated types
    * named *_REGISTER, *_LEDGER, above */
   SplitRegisterType type;

   SplitRegisterStyle style;

   /* some private data; outsiders should not access this */
   int num_cols;
   int num_header_rows;

   int num_phys_rows;
   int num_virt_rows;

   int cursor_virt_row;

   BasicCell *header_label_cells[NUM_CELLS];

   /* user_data allows users of this object to hang
    * private data onto it */
   void *user_data;

   /* The destroy callback gives user's a chance 
    * to free up any associated user_hook data */
   SplitRegisterDestroyCB destroy;
};


typedef struct _SplitRegisterColors SplitRegisterColors;

struct _SplitRegisterColors
{
  guint32 single_cursor_active_bg_color;
  guint32 single_cursor_passive_bg_color;
  guint32 single_cursor_passive_bg_color2;

  guint32 double_cursor_active_bg_color;
  guint32 double_cursor_passive_bg_color;
  guint32 double_cursor_passive_bg_color2;

  gboolean double_alternate_virt;

  guint32 trans_cursor_active_bg_color;
  guint32 trans_cursor_passive_bg_color;

  guint32 split_cursor_active_bg_color;
  guint32 split_cursor_passive_bg_color;

  guint32 header_bg_color;
};

typedef char* (*SRStringGetter) (SplitRegisterType);

void            xaccSplitRegisterSetDebitStringGetter(SRStringGetter getter);
void            xaccSplitRegisterSetCreditStringGetter(SRStringGetter getter);

SplitRegister *
xaccMallocSplitRegister (SplitRegisterType type,
                         SplitRegisterStyle style,
                         TableGetEntryHandler entry_handler,
                         VirtCellDataAllocator allocator,
                         VirtCellDataDeallocator deallocator,
                         VirtCellDataCopy copy);

void            xaccConfigSplitRegister (SplitRegister *reg,
                                         SplitRegisterType type,
                                         SplitRegisterStyle style);

void            xaccDestroySplitRegister (SplitRegister *reg);

void            xaccSetSplitRegisterColors (SplitRegisterColors reg_colors);
void            xaccSplitRegisterConfigColors (SplitRegister *reg);

/* returns non-zero value if updates have been made to data */
guint32         xaccSplitRegisterGetChangeFlag (SplitRegister *reg);

/* Clears all change flags in the register. Does not alter values */
void            xaccSplitRegisterClearChangeFlag (SplitRegister *reg);

/* Returns the type of the current cursor */
CursorClass     xaccSplitRegisterGetCurrentCursorClass (SplitRegister *reg);

/* Returns the type of the cursor at the given virtual row and column. */
CursorClass    xaccSplitRegisterGetCursorClass (SplitRegister *reg,
                                                VirtualCellLocation vcell_loc);

/* Returns the type of the current cell */
CellType        xaccSplitRegisterGetCurrentCellType (SplitRegister *reg);

/* Returns the type of the cell at the given virtual row and column. */
CellType        xaccSplitRegisterGetCellType (SplitRegister *reg,
                                              VirtualLocation virt_loc);

/* Returns the virtual location in the current cursor of the
 * given cell using the pointer values. The function returns true if
 * the given cell type is in the current cursor, false otherwise. */
gboolean   xaccSplitRegisterGetCellLoc (SplitRegister *reg,
                                        CellType cell_type,
                                        VirtualCellLocation vcell_loc,
                                        VirtualLocation *virt_loc);

gboolean   xaccSplitRegisterGetCurrentCellLoc (SplitRegister *reg,
                                               CellType cell_type,
                                               VirtualLocation *virt_loc);

/* Functions for working with split register buffers */
SplitRegisterBuffer * xaccMallocSplitRegisterBuffer (void);
void xaccDestroySplitRegisterBuffer (SplitRegisterBuffer *srb);

void xaccSplitRegisterSaveCursor(SplitRegister *sr, SplitRegisterBuffer *srb);
void xaccSplitRegisterRestoreCursorChanged(SplitRegister *sr,
                                           SplitRegisterBuffer *srb);

const char * xaccSplitRegisterGetCellTypeName (CellType type);
CellType     xaccSplitRegisterGetCellTypeFromName (const char *name);


#endif /* __XACC_SPLITREG_H__ */

/* ============ END OF FILE ===================== */
