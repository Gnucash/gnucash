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
#include <stdio.h>
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
/* when entering new cell, put cursor at end and select everything */

static const char * 
quick_enter (BasicCell *_cell, const char *val,
             int *cursor_position,
             int *start_selection,
             int *end_selection)
{
   QuickFillCell *cell = (QuickFillCell *) _cell;

   cell->qf = cell->qfRoot;

   *cursor_position = -1;
   *start_selection = 0;
   *end_selection = -1;

   return val;
}

/* ================================================ */
/* by definition, all text is valid text.  So accept
 * all modifications */

static const char * 
quick_modify (BasicCell *_cell,
              const char *oldval, 
              const char *change, 
              const char *newval,
              int *cursor_position,
              int *start_selection,
              int *end_selection)
{
#ifndef MOTIF
   QuickFillCell *cell = (QuickFillCell *) _cell;
   const char *retval;
   QuickFill *match;

   /* If deleting, just accept */
   if (change == NULL)
   {
     SET (&(cell->cell), newval);
     return newval;
   }

   /* If we are inserting in the middle, just accept */
   if (*cursor_position < strlen(oldval))
   {
     SET (&(cell->cell), newval);
     return newval;
   }

   match = xaccGetQuickFillStr(cell->qfRoot, newval);

   if ((match == NULL) || (match->text == NULL))
   {
     SET (&(cell->cell), newval);
     return newval;
   }

   retval = strdup(match->text);

   *start_selection = strlen(newval);
   *end_selection = -1;
   *cursor_position += strlen(change);

   SET (&(cell->cell), retval);
   return retval;
#endif

#ifdef MOTIF
   /* The modifications to fix up completion cursor positioning and
    *  stale match handling below are somewhat tortured.  I suspect we
    *  can do better, but this works for now... */

   QuickFillCell *cell = (QuickFillCell *) _cell;
   QuickFill *initial_match_point = cell->qf;
   const char * retval = newval;

   /* if user typed the very first letter into this
    * cell, then make sure that the quick-fill is set to 
    * the root.  Alternately, if user erased all of the 
    * text in the cell, and has just started typing,
    * then make sure that the quick-fill root is also reset */
   if (newval) {
      if (change && (0x0 != newval[0]) && (0x0 == newval[1])) {
         /* the first char is being inserted */
         cell->qf = cell->qfRoot;
      } else if('\0' == newval[0]) {
         /* the last char is being deleted */
         cell->qf = cell->qfRoot;
      }
   }

   /* if change is null, then user is deleting text;
    * otherwise, they are inserting text. */
   if (change) {
      int i;
      char c;
      int previous_match_len = 0;
      
      /* search for best-matching quick-fill string */
      i=0;
      c = change[i];
      while (c && cell->qf) {
         cell->qf = xaccGetQuickFill (cell->qf, c);
         i++;
         c = change[i];
      }

      /* Figure out how long the previous match was.  This allows us
       * to position the cursor incrementally, or DTRT when the user
       * diverges from a quickfill match. */
      {
        QuickFill *qcursor = cell->qfRoot;
        while(qcursor && (qcursor != initial_match_point)) {
          qcursor = xaccGetQuickFill (qcursor, oldval[previous_match_len++]);
        }
      }

      if (cell->qf) {
        /* we have a match, return it and reposition the cursor */
        *cursor_position = previous_match_len + strlen(change);
        retval = strdup (cell->qf->text);
      } else {
        /* do some research to figure out if the failure is because
         * the current change just diverged from a match, in which
         * case we should return everything that matched before this
         * insertion plus the new change, or because the oldval hasn't
         * matched for a while (i.e. we're no longer quickfilling), in
         * which case we should just return the suggested newval. */

        int oldval_match_len = 0;

        /* find out how much of oldval is a quickfill entry. */
        {
          QuickFill *qcursor = cell->qfRoot;
          while(qcursor) {
            qcursor = xaccGetQuickFill (qcursor, oldval[oldval_match_len]);
            oldval_match_len++;
          }
          oldval_match_len--;
        }

        if(oldval_match_len == strlen(oldval)) {
          /* If oldval was quick-fill match in its entirety, then the
           * current "change" is the first divergence.  Accordingly,
           * we need to remove any "post-divergence", stale quickfill
           * bits that reside after the cursor and then append the new
           * change text. */

          char *unmatched =
            (char *) malloc(previous_match_len + strlen(change) + 1);
          
          strncpy(unmatched, oldval, previous_match_len);
          unmatched[previous_match_len] = '\0';
          strcat(unmatched, change);          
          *cursor_position = strlen(unmatched);

          retval = unmatched;

        }
      }
   }
   
   SET (&(cell->cell), retval);
   return retval;
#endif
}

/* ================================================ */
/* when leaving cell, make sure that text was put into the qf    */

static const char * 
quick_leave (BasicCell *_cell, const char *val) 
{
   QuickFillCell *cell = (QuickFillCell *) _cell;

   cell->qf = cell->qfRoot;
   xaccQFInsertText (cell->qfRoot, val, cell->sort);
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
   cell->sort = QUICKFILL_LIFO;

   cell->cell.enter_cell    = quick_enter;
   cell->cell.modify_verify = quick_modify;
   cell->cell.leave_cell    = quick_leave;
   cell->cell.set_value     = quick_set;

   xaccQuickFillGUIInit (cell);
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
   xaccQFInsertText (cell->qfRoot, value, cell->sort);
   SET (&(cell->cell), value);
}

/* ================================================ */

void
xaccSetQuickFillSort (QuickFillCell *cell, QuickFillSort sort)
{
  if (cell == NULL)
    return;

  cell->sort = sort;
}

/* =============== END OF FILE ==================== */
