/*
 * FILE:
 * quickfillcell.c
 *
 * FUNCTION:
 * Implements a text cell with automatic typed-phrase
 * completion.
 *
 * HISTORY:
 * Copyright (c) 1998  Linas Vepstas
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

#include <stdlib.h>
#include <string.h>

#include "basiccell.h"
#include "quickfillcell.h"

#define SET(cell,str) { 			\
   if ((cell)->value) free ((cell)->value);	\
   (cell)->value = strdup (str);		\
}

/* ================================================ */

static void 
quick_set (BasicCell *_cell,
           const char *val) 
{
   QuickFillCell *cell = (QuickFillCell *) _cell;
   xaccSetQuickFillCellValue (cell, val);
}

/* ================================================ */
/* when entering new cell, reset pointer to root    */

static const char * 
quick_enter (BasicCell *_cell, const char *val) 
{
   QuickFillCell *cell = (QuickFillCell *) _cell;

   cell->qf = cell->qfRoot;
   return val;
}

/* ================================================ */
/* by definition, all text is valid text.  So accept
 * all modifications */

static const char * 
quick_modify (BasicCell *_cell,
        const char *oldval, 
        const char *change, 
        const char *newval)
{
   QuickFillCell *cell = (QuickFillCell *) _cell;
   char * retval = (char *) newval;

   /* if user typed the very first letter into this
    * cell, then make sure that the quick-fill is set to 
    * the root.  Alternately, if user erased all of the 
    * text in the cell, and has just started typing,
    * then make sure that the quick-fill root is also reset
    */
   if (newval) {
      if ((0x0 != newval[0]) && (0x0 == newval[1])) {
         cell->qf = cell->qfRoot;
      }
   }

   /* if change is null, then user is deleting text;
    * otehrwise, they are inserting text. */
   if (change) {
      int i;
      char c;
      
      /* search for best-matching quick-fill string */
      i=0;
      c = change[i];
      while (c) {
         cell->qf = xaccGetQuickFill (cell->qf, c);
         i++;
         c = change[i];
      }

      /* if a match found, return it */
      if (cell->qf) retval = strdup (cell->qf->text);
   }

   SET (&(cell->cell), retval);
   return retval;
}

/* ================================================ */
/* when leaving cell, make sure that text was put into the qf    */

static const char * 
quick_leave (BasicCell *_cell, const char *val) 
{
   QuickFillCell *cell = (QuickFillCell *) _cell;

   cell->qf = cell->qfRoot;
   xaccQFInsertText (cell->qfRoot, val);
   return val;
}

/* ================================================ */

QuickFillCell *
xaccMallocQuickFillCell (void)
{
   QuickFillCell *cell;
   cell = ( QuickFillCell *) malloc (sizeof (QuickFillCell));

   xaccInitQuickFillCell (cell);
   return cell;
}

/* ================================================ */

void
xaccInitQuickFillCell (QuickFillCell *cell)
{
   xaccInitBasicCell (&(cell->cell));

   cell->qfRoot = xaccMallocQuickFill();
   cell->qf = cell->qfRoot;

   cell->cell.enter_cell    = quick_enter;
   cell->cell.modify_verify = quick_modify;
   cell->cell.leave_cell    = quick_leave;
   cell->cell.set_value     = quick_set;
}

/* ================================================ */

void
xaccDestroyQuickFillCell (QuickFillCell *cell)
{
   xaccFreeQuickFill (cell->qfRoot);
   cell->qfRoot = NULL;
   cell->qf = NULL;

   cell->cell.enter_cell    = NULL;
   cell->cell.modify_verify = NULL;
   cell->cell.leave_cell    = NULL;
   cell->cell.set_value     = NULL;

   xaccDestroyBasicCell (&(cell->cell));
}

/* ================================================ */

void
xaccSetQuickFillCellValue (QuickFillCell *cell, const char * value)
{
   xaccQFInsertText (cell->qfRoot, value);
   SET (&(cell->cell), value);
}

/* =============== END OF FILE ==================== */
