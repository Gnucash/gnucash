/*
 * FILE:
 * quickfillcell.h
 *
 * FUNCTION:
 * The QuickFillCell implements a text cell with quick-fill 
 * capabilities.  By "quick fill" we mean a cell that will 
 * automatically sentance-complete the entry after the user 
 * typed a sufficient number of letters to identify a unique 
 * entry. 
 *
 * On the output side, this is just a plain text cell.
 *
 * METHODS:
 * The xaccSetQuickFillCellValue() method sets the
 * current cell value to the indicated string, 
 * simultaneously adding the string to the quick-fill
 * tree.
 *
 * HISTORY:
 * Copyright (c) 1997, 1998 Linas Vepstas
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

#ifndef __XACC_FILL_CELL_C__
#define __XACC_FILL_CELL_C__

#include "basiccell.h"
#include "QuickFill.h"

typedef struct _QuickFillCell
{
   BasicCell cell;
   QuickFill *qfRoot;       /* root of quickfill-tree 
                             * handled by this cell */
   QuickFill *qf;           /* current position in tree */

   QuickFillSort sort;      /* determines order of strings matched.
                             * default is QUICKFILL_LIFO. */
} QuickFillCell;

/* installs a callback to handle price recording */
QuickFillCell *  xaccMallocQuickFillCell (void);
void             xaccInitQuickFillCell (QuickFillCell *);
void             xaccDestroyQuickFillCell (QuickFillCell *);

void             xaccSetQuickFillCellValue (QuickFillCell *, const char *);
void             xaccSetQuickFillSort (QuickFillCell *, QuickFillSort);

/* GUI-dependent */
void             xaccQuickFillGUIInit (QuickFillCell *);

#endif /* __XACC_FILL_CELL_C__ */

/* --------------- end of file ---------------------- */
