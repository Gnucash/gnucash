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
 * numcell.c
 *
 * FUNCTION:
 * implements a gui-independent number handling cell.
 *
 * HISTORY:
 * Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>
 */

#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include "numcell.h"
#include "util.h"

/* This static indicates the debugging module that this .o belongs to. */
/* static short module = MOD_REGISTER; */


/* ================================================ */
/* Parses the string value and returns true if it is a
 * number. In that case, *num is set to the value parsed. */
static gboolean
parse_num(const char *string, long int *num)
{
  long int number;

  if (string == NULL)
    return FALSE;

  if (!gnc_strisnum(string))
    return FALSE;

  number = strtol(string, NULL, 10);

  if ((number == LONG_MIN) || (number == LONG_MAX))
    return FALSE;

  if (num != NULL)
    *num = number;

  return TRUE;
}

/* ================================================ */
static const char * 
NumEnter (BasicCell *_cell,
          const char * curr,
          int *cursor_position,
          int *start_selection,
          int *end_selection)
{
  NumCell *cell = (NumCell *) _cell;

  if (!cell->next_num_set)
  {
    long int number;

    if (parse_num(curr, &number))
      cell->next_num = number + 1;
  }

  return curr;
}

/* ================================================ */
static const char * 
NumMV (BasicCell *_cell, 
       const char *oldval, 
       const char *change, 
       const char *newval,
       int *cursor_position,
       int *start_selection,
       int *end_selection)
{
  NumCell *cell = (NumCell *) _cell;
  gboolean accel = FALSE;
  gboolean is_num;
  long int number = 0;

  if ((change == NULL) || (change[0] == 0) || /* if we are deleting       */
      (strlen(change) > 1))                   /* or entering > 1 char     */
    /* then just accept the proposed change */
  {
    g_free (cell->cell.value);
    cell->cell.value = g_strdup (newval);
    return newval;
  }

  /* otherwise, it may be an accelerator key. */

  is_num = parse_num(oldval, &number);

  if (is_num && (number < 0))
    is_num = FALSE;

  switch (change[0])
  {
    case '+':
    case '=':
      number++;
      accel = TRUE;
      break;

    case '_':
    case '-':
      number--;
      accel = TRUE;
      break;

    case '}':
    case ']':
      number += 10;
      accel = TRUE;
      break;

    case '{':
    case '[':
      number -= 10;
      accel = TRUE;
      break;
  }

  if (number < 0)
    number = 0;

  /* If there is already a non-number there, don't accelerate. */
  if (accel && !is_num && (safe_strcmp(oldval, "") != 0))
    accel = FALSE;

  if (accel)
  {
    char buff[128];

    if (!is_num)
      number = cell->next_num;

    strcpy(buff, "");
    snprintf(buff, sizeof(buff), "%ld", number);

    if (safe_strcmp(buff, "") == 0)
      return NULL;

    g_free (cell->cell.value);
    cell->cell.value = g_strdup (buff);

    *cursor_position = -1;

    return g_strdup(buff);
  }

  g_free (cell->cell.value);
  cell->cell.value = g_strdup (newval);

  return newval;
}

/* ================================================ */
static const char * 
NumLeave (BasicCell *_cell, const char * curr)
{
  NumCell *cell = (NumCell *) _cell;

  if (!cell->next_num_set)
  {
    long int number;

    if (parse_num(curr, &number))
      cell->next_num = number + 1;
  }

  return NULL;
}

/* ================================================ */
NumCell *
xaccMallocNumCell (void)
{
  NumCell *cell;

  cell = g_new(NumCell, 1);

  xaccInitNumCell (cell);

  return cell;
}

/* ================================================ */
void
xaccDestroyNumCell (NumCell *cell)
{
  xaccDestroyBasicCell (&(cell->cell));
}

/* ================================================ */
static void 
setNumCellValue (BasicCell *_cell, const char *str)
{
  NumCell *cell = (NumCell *) _cell;

  if (!cell->next_num_set)
  {
    long int number;

    if (parse_num(str, &number))
      cell->next_num = number + 1;
  }

  g_free (cell->cell.value);
  cell->cell.value = g_strdup (str);
}

/* ================================================ */
void 
xaccSetNumCellValue (NumCell *cell, const char *str)
{
  setNumCellValue(&cell->cell, str);
}

/* ================================================ */
void
xaccSetNumCellLastNum (NumCell *cell, const char *str)
{
  long int number;

  if (parse_num(str, &number))
  {
    cell->next_num = number + 1;
    cell->next_num_set = TRUE;
  }
}

/* ================================================ */
void
xaccInitNumCell (NumCell *cell)
{
  xaccInitBasicCell (&(cell->cell));

  cell->next_num = 0;
  cell->next_num_set = FALSE;
 
  cell->cell.enter_cell = NumEnter;
  cell->cell.modify_verify = NumMV;
  cell->cell.leave_cell = NumLeave;
  cell->cell.set_value = setNumCellValue;
}
