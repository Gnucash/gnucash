/********************************************************************\
 * quickfillcell.c -- autocompletion based on memorized history     *
 *                                                                  *   
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

#include "config.h"

#include <ctype.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef HAVE_WCTYPE_H
#include <wctype.h>
#endif

#include "basiccell.h"
#include "gnc-ui-util.h"
#include "quickfillcell.h"


static void xaccSetQuickFillCellOriginal (QuickFillCell *cell,
                                          const GdkWChar *original);

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

   xaccSetQuickFillCellOriginal (cell, NULL);

   return TRUE;
}

/* ================================================ */
/* by definition, all text is valid text.  So accept
 * all modifications */

static gboolean
wcstrcaseequal (const GdkWChar *s1, const GdkWChar *s2)
{
  int i;

  if (s1 == s2)
    return TRUE;

  if (!s1 || !s2)
    return FALSE;

  for (i = 0; TRUE; i++)
  {
    GdkWChar a;
    GdkWChar b;

    if (s1[i] == 0 || s2[i] == 0)
      return s1[i] == s2[i];

    a = iswlower (s1[i]) ? towupper (s1[i]) : s1[i];
    b = iswlower (s2[i]) ? towupper (s2[i]) : s2[i];

    if (a != b)
      return FALSE;
  }

  return TRUE;
}

static gboolean
wcstrncaseequal (const GdkWChar *s1, const GdkWChar *s2, int len)
{
  int i;

  if (s1 == s2)
    return TRUE;

  if (!s1 || !s2)
    return FALSE;

  for (i = 0; i < len; i++)
  {
    GdkWChar a;
    GdkWChar b;

    if (s1[i] == 0 || s2[i] == 0)
      return FALSE;

    a = iswlower (s1[i]) ? towupper (s1[i]) : s1[i];
    b = iswlower (s2[i]) ? towupper (s2[i]) : s2[i];

    if (a != b)
      return FALSE;
  }

  return TRUE;
}

static void
quick_modify (BasicCell *_cell,
              const GdkWChar *change,
              int change_len,
              const GdkWChar *newval,
              int newval_len,
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
     if ((*cursor_position >= newval_len) &&
         (cell->original != NULL) &&
         (gnc_wcslen (cell->original) >= newval_len) &&
         wcstrncaseequal (cell->original, newval, newval_len))
       cell->original[newval_len] = 0;
     else
       xaccSetQuickFillCellOriginal(cell, NULL);

     xaccSetBasicCellWCValueInternal (&cell->cell, newval);
     return;
   }

   /* If we are inserting in the middle, just accept */
   if (*cursor_position < _cell->value_len)
   {
     xaccSetBasicCellWCValueInternal (&cell->cell, newval);
     xaccSetQuickFillCellOriginal(cell, NULL);
     return;
   }

   if (cell->original == NULL)
     cell->original = gnc_wcsdup (newval);
   else if (wcstrcaseequal (cell->original, _cell->value_w))
   {
     int orig_len = gnc_wcslen (cell->original);
     GdkWChar *original;
     int i;

     original = g_new0 (GdkWChar, orig_len + change_len + 1);

     for (i = 0; i < orig_len; i++)
       original[i] = cell->original[i];

     for (i = orig_len; i < orig_len + change_len; i++)
       original[i] = change[i - orig_len];

     original[orig_len + change_len] = 0;

     g_free (cell->original);
     cell->original = original;
   }
   else
   {
     g_free (cell->original);
     cell->original = NULL;
   }

   match = gnc_quickfill_get_string_match (cell->qf, newval);

   match_str = gnc_quickfill_string (match);

   if (match_str == NULL)
   {
     if (cell->original != NULL)
       newval = cell->original;

     *cursor_position = -1;

     xaccSetBasicCellWCValueInternal (&cell->cell, newval);
     return;
   }

   *start_selection = newval_len;
   *end_selection = -1;
   *cursor_position += change_len;

   xaccSetBasicCellValueInternal (&cell->cell, match_str);
}

/* ================================================ */
/* when leaving cell, make sure that text was put into the qf */

static void
quick_leave (BasicCell * _cell) 
{
   QuickFillCell *cell = (QuickFillCell *) _cell;

   gnc_quickfill_insert_wc (cell->qf, _cell->value_w, cell->sort);
}

/* ================================================ */

static void
quickfill_cell_destroy (BasicCell *bcell)
{
  QuickFillCell *cell = (QuickFillCell *) bcell;

  gnc_quickfill_destroy (cell->qf);
  cell->qf = NULL;

  g_free (cell->original);
  cell->original = NULL;

  cell->cell.enter_cell    = NULL;
  cell->cell.modify_verify = NULL;
  cell->cell.leave_cell    = NULL;
  cell->cell.set_value     = NULL;
}

/* ================================================ */

static void
xaccInitQuickFillCell (QuickFillCell *cell)
{
  gnc_basic_cell_init (&(cell->cell));

  cell->qf = gnc_quickfill_new ();
  cell->sort = QUICKFILL_LIFO;
  cell->original = NULL;

  cell->cell.destroy = quickfill_cell_destroy;

  cell->cell.enter_cell    = quick_enter;
  cell->cell.modify_verify = quick_modify;
  cell->cell.leave_cell    = quick_leave;
  cell->cell.set_value     = quick_set;

  xaccQuickFillGUIInit (cell);
}

/* ================================================ */

BasicCell *
xaccMallocQuickFillCell (void)
{
  QuickFillCell *cell;

  cell = g_new0 (QuickFillCell, 1);

  xaccInitQuickFillCell (cell);

  return &cell->cell;
}

/* ================================================ */

void
xaccSetQuickFillCellValue (QuickFillCell *cell, const char * value)
{
  if (cell == NULL)
    return;

  xaccSetBasicCellValueInternal (&cell->cell, value);
  gnc_quickfill_insert_wc (cell->qf, cell->cell.value_w, cell->sort);
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

static void
xaccSetQuickFillCellOriginal (QuickFillCell *cell, const GdkWChar *original)
{
  if (cell == NULL)
    return;

  g_free (cell->original);

  if ((original != NULL) && (*original != 0))
    cell->original = gnc_wcsdup (original);
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
