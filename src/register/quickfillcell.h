
/*
 * FILE:
 * quickfillcell.h
 *
 * FUNCTION:
 * Implements a text cell with quick-fill capabilities.
 *
 * The xaccSetQuickFillCellValue() method sets the
 * current cell value to the indicated string, 
 * simultaneously adding the string to the quick-fill
 * tree.
 *
 * HISTORY:
 * Copyright (c) 1997, 1998 Linas Vepstas
 */

#ifndef __XACC_FILL_CELL_C__
#define __XACC_FILL_CELL_C__

#include "basiccell.h"
#include "QuickFill.h"

typedef struct _QuickFillCell {
   BasicCell cell;
   QuickFill *qfRoot;       /* root of quickfill-tree 
                             * handled by this cell */
   QuickFill *qf;           /* current positin in tree */
} QuickFillCell;

/* installs a callback to handle price recording */
QuickFillCell *  xaccMallocQuickFillCell (void);
void             xaccInitQuickFillCell (QuickFillCell *);

void             xaccSetQuickFillCellValue (QuickFillCell *, char *);

#endif /* __XACC_FILL_CELL_C__ */

/* --------------- end of file ---------------------- */
