/*
 * FILE:
 * register.c
 */

#include "messages.h"
#include "register.h"

/* utility defines for cell configuration data */
#define DATE_CELL      0
#define NUM_CELL       1
#define ACTN_CELL      2
#define XFRM_CELL      3
#define XTO_CELL       4
#define DESC_CELL      5
#define MEMO_CELL      6
#define RECN_CELL      7
#define CRED_CELL      8
#define DEBT_CELL      9
#define BALN_CELL     10
#define SHRS_CELL     11
#define PRIC_CELL     12


/* utility defines for setting of cell values */
#define DATE_CELL_C   (reg->cols[DATE_CELL])
#define DATE_CELL_R   (reg->rows[DATE_CELL])
#define DATE_CELL_W   (reg->wids[DATE_CELL])

#define NUM_CELL_C   (reg->cols[NUM_CELL])
#define NUM_CELL_R   (reg->rows[NUM_CELL])
#define NUM_CELL_W   (reg->wids[NUM_CELL])

#define ACTN_CELL_C   (reg->cols[ACTN_CELL])
#define ACTN_CELL_R   (reg->rows[ACTN_CELL])
#define ACTN_CELL_W   (reg->wids[ACTN_CELL])

#define XFRM_CELL_C   (reg->cols[XFRM_CELL])
#define XFRM_CELL_R   (reg->rows[XFRM_CELL])
#define XFRM_CELL_W   (reg->wids[XFRM_CELL])

#define XTO_CELL_C   (reg->cols[XTO_CELL])
#define XTO_CELL_R   (reg->rows[XTO_CELL])
#define XTO_CELL_W   (reg->wids[XTO_CELL])

#define DESC_CELL_C   (reg->cols[DESC_CELL])
#define DESC_CELL_R   (reg->rows[DESC_CELL])
#define DESC_CELL_W   (reg->wids[DESC_CELL])

#define MEMO_CELL_C   (reg->cols[MEMO_CELL])
#define MEMO_CELL_R   (reg->rows[MEMO_CELL])
#define MEMO_CELL_W   (reg->wids[MEMO_CELL])

#define RECN_CELL_C   (reg->cols[RECN_CELL])
#define RECN_CELL_R   (reg->rows[RECN_CELL])
#define RECN_CELL_W   (reg->wids[RECN_CELL])

#define CRED_CELL_C   (reg->cols[CRED_CELL])
#define CRED_CELL_R   (reg->rows[CRED_CELL])
#define CRED_CELL_W   (reg->wids[CRED_CELL])

#define DEBT_CELL_C   (reg->cols[DEBT_CELL])
#define DEBT_CELL_R   (reg->rows[DEBT_CELL])
#define DEBT_CELL_W   (reg->wids[DEBT_CELL])

#define BALN_CELL_C   (reg->cols[BALN_CELL])
#define BALN_CELL_R   (reg->rows[BALN_CELL])
#define BALN_CELL_W   (reg->wids[BALN_CELL])

#define SHRS_CELL_C   (reg->cols[SHRS_CELL])
#define SHRS_CELL_R   (reg->rows[SHRS_CELL])
#define SHRS_CELL_W   (reg->wids[SHRS_CELL])

#define PRIC_CELL_C   (reg->cols[PRIC_CELL])
#define PRIC_CELL_R   (reg->rows[PRIC_CELL])
#define PRIC_CELL_W   (reg->wids[PRIC_CELL])


#define MAX_COLS 8

/* ============================================== */

#define SET(cell,col,row,width,label) {		\
   reg->cols[cell] = col;			\
   reg->rows[cell] = row;			\
   reg->wids[cell] = width;			\
   reg->labels[cell] = label;			\
}

/* ============================================== */

static void
configLayout (BasicRegister *reg, int type)
{
   switch (type) {
      case BANK_REGISTER:
         reg->num_cols = 8;
         reg->num_header_rows = 1;

         SET (DATE_CELL,  0,  0, 11,  DATE_STR);
         SET (NUM_CELL,   1,  0,  7,  NUM_STR);
         SET (ACTN_CELL,  1,  1,  7,  NUM_STR);
         SET (XFRM_CELL,  2,  0, 11,  XFRM_STR);
         SET (XTO_CELL,  -1, -1, 11,  "");
         SET (DESC_CELL,  3,  0, 29,  DESC_STR);
         SET (MEMO_CELL,  3,  1, 29,  DESC_STR);
         SET (RECN_CELL,  4,  0,  1,  "R");
         SET (CRED_CELL,  5,  0,  9,  CREDIT_STR);
         SET (DEBT_CELL,  6,  0,  9,  DEBIT_STR);
         SET (BALN_CELL,  7,  0,  9,  BALN_STR);
         SET (SHRS_CELL, -1, -1,  9,  "");
         SET (PRIC_CELL, -1, -1,  9,  "");

      default:

   }
}

/* ============================================== */
/* define the traversal order */
/* negative cells mean "traverse out of table" */

static void
configTraverse (BasicRegister *reg, int type)
{
   CellBlock *curs = reg->cursor;

   switch (type) {
      case BANK_REGISTER:
         xaccNextRight (curs, DATE_CELL_R, DATE_CELL_C,  NUM_CELL_R,  NUM_CELL_C);
         xaccNextRight (curs,  NUM_CELL_R,  NUM_CELL_C, XFRM_CELL_R, XFRM_CELL_C);
         xaccNextRight (curs, XFRM_CELL_R, XFRM_CELL_C, DESC_CELL_R, DESC_CELL_C);
         xaccNextRight (curs, DESC_CELL_R, DESC_CELL_C, CRED_CELL_R, CRED_CELL_C);
         xaccNextRight (curs, CRED_CELL_R, CRED_CELL_C, DEBT_CELL_R, DEBT_CELL_C);
         xaccNextRight (curs, DEBT_CELL_R, DEBT_CELL_C, ACTN_CELL_R, ACTN_CELL_C);
         xaccNextRight (curs, ACTN_CELL_R, ACTN_CELL_C, MEMO_CELL_R, MEMO_CELL_C);
         xaccNextRight (curs, MEMO_CELL_R, MEMO_CELL_C, -1-DATE_CELL_R, -1-DATE_CELL_C);

      default:

   }
}

/* ============================================== */

BasicRegister * xaccMallocBasicRegister (int type)
{
   BasicRegister * reg;
   reg = (BasicRegister *) malloc (sizeof (BasicRegister));
   xaccInitBasicRegister (reg, type);
   return reg;
}

/* ============================================== */

/* HDR is a utility to set up the header row */
#define HDR(NAME)						\
{								\
   BasicCell *cell;						\
   cell = xaccMallocTextCell();					\
   cell->width = NAME##_CELL_W;					\
   if (1 == reg->num_header_rows) {				\
      xaccAddCell (header, cell, 0, NAME##_CELL_C);		\
   } else {							\
      xaccAddCell (header, cell, NAME##_CELL_R, NAME##_CELL_C);	\
   }								\
   xaccSetBasicCellValue (cell, reg->labels[NAME##_CELL]);	\
}
   
/* BASIC & FANCY macros initialize cells in the register */

#define FANCY(CN,CT,CL) {					\
   reg->CN##Cell = xaccMalloc##CT##Cell();			\
   reg->CN##Cell->cell.width = CL##_CELL_W;			\
   xaccAddCell (curs, &(reg->CN##Cell->cell), CL##_CELL_R, CL##_CELL_C); \
}

#define BASIC(CN,CT,CL) {					\
   reg->CN##Cell = xaccMalloc##CT##Cell();			\
   reg->CN##Cell->width = CL##_CELL_W;				\
   xaccAddCell (curs, reg->CN##Cell, CL##_CELL_R, CL##_CELL_C);	\
}
   

void xaccInitBasicRegister (BasicRegister *reg, int type)
{
   Table * table;
   CellBlock *curs, *header;
   BasicCell *cell;

   /* --------------------------- */
   configLayout (reg, type);

   /* --------------------------- */
   /* define the header */
   header = xaccMallocCellBlock (reg->num_header_rows, reg->num_cols);
   reg->header = header;

   HDR (DATE);
   HDR (NUM);
   HDR (XFRM);
   HDR (DESC);
   HDR (RECN);
   HDR (CRED);
   HDR (DEBT);
   HDR (BALN);
   HDR (PRIC);
   HDR (SHRS);
   
   /* --------------------------- */
   /* define the ledger cursor */
   curs = xaccMallocCellBlock (2, reg->num_cols);
   reg->cursor = curs;
   
   FANCY (date,    Date,      DATE);
   BASIC (num,     Text,      NUM);
   FANCY (action,  Combo,     ACTN);
   FANCY (xfrm,    Combo,     XFRM);
   FANCY (desc,    QuickFill, DESC);
   BASIC (memo,    Text,      MEMO);
   BASIC (recn,    Recn,      RECN);
   FANCY (credit,  Price,     CRED);
   FANCY (debit,   Price,     DEBT);
   FANCY (shrs,    Price,     SHRS);
   FANCY (price,   Price,     PRIC);

   FANCY (balance, Price,     BALN);
   reg->balanceCell->cell.input_output = 0;

   /* -------------------------------- */   
   /* define how traversal works */
   configTraverse (reg, type);

   /* -------------------------------- */   
   /* add menu items for the action cell */

   xaccAddComboCellMenuItem ( reg->actionCell, BUY_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, SELL_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, PRICE_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, INT_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, DIV_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, LTCG_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, STCG_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, DIST_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, SPLIT_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, DEPOSIT_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, WITHDRAW_STR);

   /* -------------------------------- */   
   table = xaccMallocTable ();
   table -> header = header;
   xaccSetCursor (table, curs);
   reg->table = table;
}

/* ============================================== */

unsigned int
xaccGetChangeFlag (BasicRegister *reg)
{

   unsigned int changed = 0;

   changed |= MOD_DATE && reg->dateCell->cell.changed;
   changed |= MOD_NUM  && reg->numCell->changed;
   changed |= MOD_DESC && reg->descCell->cell.changed;
   changed |= MOD_RECN && reg->recnCell->changed;
   changed |= MOD_AMNT && reg->creditCell->cell.changed;
   changed |= MOD_AMNT && reg->debitCell->cell.changed;
   changed |= MOD_SHRS && reg->shrsCell->cell.changed; 
   changed |= MOD_PRIC && reg->priceCell->cell.changed;
   changed |= MOD_MEMO && reg->memoCell->changed;
   changed |= MOD_ACTN && reg->actionCell->cell.changed;
   changed |= MOD_XFRM && reg->xfrmCell->cell.changed;
   changed |= MOD_XTO  && reg->xtoCell->cell.changed; 

   return changed;
}

/* ============ END OF FILE ===================== */
