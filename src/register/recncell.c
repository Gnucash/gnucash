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
 * recncell.c
 * 
 * FUNCTION:
 * Implements a mouse-click cell that allows a series
 * of values to be clicked through.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 */

#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "basiccell.h"
#include "recncell.h"
#include "ui-callbacks.h"
#include "messages.h"

/* hack alert -- I am uncomfortable with including engine
 * stuff here; all code in this directory should really be 
 * independent of the engine.  Its just that we need the
 * defs for YREC, CREC, etc. This is some cleanup we should 
 * do some day.
 */
#include "Transaction.h"

static RecnCellStringGetter string_getter = NULL;


/* ================================================ */

static const char *
RecnCellGetString(char reconciled_flag)
{
  static char str[2] = { 0, 0 };

  if (string_getter != NULL)
    return (string_getter)(reconciled_flag);

  str[0] = reconciled_flag;

  return str;
}

/* ================================================ */

static const char * 
ToggleRecn (BasicCell *_cell,
            const char *cur_val,
            int *cursor_position,
            int *start_selection,
            int *end_selection)
{
  RecnCell *cell = (RecnCell *) _cell;

  /* throw up a popup if the user tries to undo a reconciled transaction
     hack alert -- this sets a new precedent. gnc_verify_dialog is defined
     in both the motif and the gnome subdirs; I don't think I like it that
     way. Now it's in ui-callbacks.h which is UI independent, but that's
     still perhaps not optimal. */

  if (cell->reconciled_flag == YREC)
    if (!gnc_verify_dialog(CHANGE_RECN_MSG, GNC_T))
      return NULL;

  if (cell->reconciled_flag == NREC)
    cell->reconciled_flag = CREC;
  else
    cell->reconciled_flag = NREC;

  xaccRecnCellSetFlag (cell, cell->reconciled_flag);

  return g_strdup (_cell->value);
}

/* ================================================ */

RecnCell *
xaccMallocRecnCell (void)
{
  RecnCell * cell;

  cell = g_new(RecnCell, 1);

  xaccInitRecnCell (cell);

  return cell;
}

/* ================================================ */

void
xaccInitRecnCell (RecnCell *cell)
{
  xaccInitBasicCell(&cell->cell);

  xaccRecnCellSetFlag(cell, NREC);

  cell->cell.enter_cell = ToggleRecn;
}

/* ================================================ */

void
xaccDestroyRecnCell (RecnCell *cell)
{
  xaccDestroyBasicCell (&cell->cell);
}

/* ================================================ */

void
xaccRecnCellSetFlag (RecnCell *cell, char reconciled_flag)
{
  const char *string;

  cell->reconciled_flag = reconciled_flag;

  string = RecnCellGetString(reconciled_flag);

  xaccSetBasicCellValue(&cell->cell, string);
}

/* ================================================ */

char
xaccRecnCellGetFlag (RecnCell *cell)
{
  return cell->reconciled_flag;
}

/* ================================================ */

void
xaccRecnCellSetStringGetter (RecnCellStringGetter getter)
{
  string_getter = getter;
}

/* --------------- end of file ---------------------- */
