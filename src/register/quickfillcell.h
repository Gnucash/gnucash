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

  char *original;          /* original string entered in original case */
} QuickFillCell;

QuickFillCell *  xaccMallocQuickFillCell (void);
void             xaccInitQuickFillCell (QuickFillCell *);
void             xaccDestroyQuickFillCell (QuickFillCell *);

void             xaccSetQuickFillCellValue (QuickFillCell *, const char *);
void             xaccSetQuickFillSort (QuickFillCell *, QuickFillSort);
void             xaccSetQuickFillOriginal (QuickFillCell *, const char *);

/* GUI-dependent */
void             xaccQuickFillGUIInit (QuickFillCell *);

#endif /* __XACC_FILL_CELL_C__ */

/* --------------- end of file ---------------------- */
