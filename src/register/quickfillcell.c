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
 * quickfillcell.c
 *
 * FUNCTION:
 * Implements a text cell with automatic typed-phrase
 * completion.
 *
 * HISTORY:
 * Copyright (c) 1998-2000 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <glib.h>

#include "basiccell.h"
#include "quickfillcell.h"

#define SET(cell,str) { 	   \
   g_free ((cell)->value);	   \
   (cell)->value = g_strdup (str); \
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

static gboolean
quick_enter (BasicCell *_cell,
             int *cursor_position,
             int *start_selection,
             int *end_selection)
{
   QuickFillCell *cell = (QuickFillCell *) _cell;

   *cursor_position = -1;
   *start_selection = 0;
   *end_selection = -1;

   xaccSetQuickFillCellOriginal(cell, NULL);

   return TRUE;
}

/* ================================================ */
/* by definition, all text is valid text.  So accept
 * all modifications */

static void
quick_modify (BasicCell *_cell,
              const char *change, 
              const char *newval,
              int *cursor_position,
              int *start_selection,
              int *end_selection)
{
   QuickFillCell *cell = (QuickFillCell *) _cell;
   const char *match_str;
   QuickFill *match;

   /* If deleting, just accept */
   if (change == NULL)
   {
     /* if the new value is a prefix of the original modulo case,
      * just truncate the end of the original. Otherwise, set it
      * to NULL */
     if ((*cursor_position >= strlen(newval)) &&
         (cell->original != NULL) &&
         (strlen(cell->original) >= strlen(newval)) &&
         (strncasecmp(cell->original, newval, strlen(newval)) == 0))
       cell->original[strlen(newval)] = '\0';
     else
       xaccSetQuickFillCellOriginal(cell, NULL);

     SET (&(cell->cell), newval);
     return;
   }

   /* If we are inserting in the middle, just accept */
   if (*cursor_position < strlen(_cell->value))
   {
     SET (&(cell->cell), newval);
     xaccSetQuickFillCellOriginal(cell, NULL);
     return;
   }

   if (cell->original == NULL)
     cell->original = g_strdup(newval);
   else if (strcasecmp(cell->original, _cell->value) == 0)
   {
     char *original = g_strconcat(cell->original, change, NULL);
     g_free(cell->original);
     cell->original = original;
   }
   else
   {
     g_free(cell->original);
     cell->original = NULL;
   }

   match = gnc_quickfill_get_string_match (cell->qf, newval);

   match_str = gnc_quickfill_string (match);

   if (match_str == NULL)
   {
     if (cell->original != NULL)
       newval = cell->original;

     *cursor_position = -1;

     SET (&(cell->cell), newval);
     return;
   }

   *start_selection = strlen(newval);
   *end_selection = -1;
   *cursor_position += strlen(change);

   SET (&(cell->cell), match_str);
}

/* ================================================ */
/* when leaving cell, make sure that text was put into the qf */

static void
quick_leave (BasicCell * _cell) 
{
   QuickFillCell *cell = (QuickFillCell *) _cell;

   gnc_quickfill_insert (cell->qf, _cell->value, cell->sort);
}

/* ================================================ */

QuickFillCell *
xaccMallocQuickFillCell (void)
{
   QuickFillCell *cell;

   cell = g_new0 (QuickFillCell, 1);

   xaccInitQuickFillCell (cell);

   return cell;
}

/* ================================================ */

void
xaccInitQuickFillCell (QuickFillCell *cell)
{
   xaccInitBasicCell (&(cell->cell));

   cell->qf = gnc_quickfill_new ();
   cell->sort = QUICKFILL_LIFO;
   cell->original = NULL;

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
   gnc_quickfill_destroy (cell->qf);
   cell->qf = NULL;

   g_free(cell->original);
   cell->original = NULL;

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
  if (cell == NULL)
    return;

  gnc_quickfill_insert (cell->qf, value, cell->sort);
  SET (&(cell->cell), value);
}

/* ================================================ */

void
xaccSetQuickFillCellSort (QuickFillCell *cell, QuickFillSort sort)
{
  if (cell == NULL)
    return;

  cell->sort = sort;
}

/* ================================================ */

void
xaccSetQuickFillCellOriginal (QuickFillCell *cell, const char *original)
{
  if (cell == NULL)
    return;

  g_free(cell->original);

  if ((original != NULL) && (*original != '\0'))
    cell->original = g_strdup(original);
  else
    cell->original = NULL;
}

/* ================================================ */

void
xaccQuickFillAddCompletion (QuickFillCell *cell, const char *completion)
{
  if (cell == NULL)
    return;

  gnc_quickfill_insert (cell->qf, completion, cell->sort);
}

/* =============== END OF FILE ==================== */
