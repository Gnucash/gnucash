
/*
 * FILE:
 * quickfillcell.h
 *
 * FUNCTION:
 * implements a text cell with quick-fill capabilities
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
   QuickFill *qf;
} QuickFillCell;

/* installs a callback to handle price recording */
QuickFillCell *  xaccMallocQuickFillCell (void);
void             xaccInitQuickFillCell (QuickFillCell *);


#endif /* __XACC_FILL_CELL_C__ */

/* --------------- end of file ---------------------- */
