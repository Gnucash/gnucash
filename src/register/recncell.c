/********************************************************************\
 * recncell.c -- reconcile checkbox cell                            *
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
#include "gnc-engine-util.h"
#include "gnc-ui.h"
#include "messages.h"
#include "recncell.h"

/* hack alert -- I am uncomfortable with including engine
 * stuff here; all code in this directory should really be 
 * independent of the engine.  Its just that we need the
 * defs for YREC, CREC, etc. This is some cleanup we should 
 * do some day.
 */
#include "Transaction.h"

static RecnCellStringGetter string_getter = NULL;

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_REGISTER;

static void RecnSetValue (BasicCell *_cell, const char *value);


/* ================================================ */

static const char *
RecnCellGetString (char reconciled_flag)
{
  static char str[2] = { 0, 0 };

  if (string_getter != NULL)
    return (string_getter)(reconciled_flag);

  str[0] = reconciled_flag;

  return str;
}

/* ================================================ */

static gboolean
RecnEnter (BasicCell *_cell,
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
  {
    const char *message = _("Do you really want to mark this transaction "
                            "not reconciled?\nDoing so might make future "
                            "reconciliation difficult!");
    if (!gnc_verify_dialog(message, TRUE))
      return FALSE;
  }

  if (cell->reconciled_flag == NREC)
    cell->reconciled_flag = CREC;
  else
    cell->reconciled_flag = NREC;

  xaccRecnCellSetFlag (cell, cell->reconciled_flag);

  return FALSE;
}

/* ================================================ */

static void
xaccInitRecnCell (RecnCell *cell)
{
  xaccInitBasicCell(&cell->cell);

  xaccRecnCellSetFlag(cell, NREC);

  cell->cell.enter_cell = RecnEnter;
  cell->cell.set_value = RecnSetValue;
}

RecnCell *
xaccMallocRecnCell (void)
{
  RecnCell * cell;

  cell = g_new0 (RecnCell, 1);

  xaccInitRecnCell (cell);

  return cell;
}

/* ================================================ */

/* assumes we are given the untranslated form */
static void
RecnSetValue (BasicCell *_cell, const char *value)
{
  RecnCell *cell = (RecnCell *) _cell;
  char flag;

  if (!value || *value == '\0')
  {
    cell->reconciled_flag = NREC;
    g_free (_cell->value);
    _cell->value = g_strdup ("");
    return;
  }

  switch (*value)
  {
    case NREC:
    case CREC:
    case YREC:
    case FREC:
      flag = *value;
      break;

    default:
      PWARN ("unexpected recn flag");
      flag = NREC;
      break;
  }

  xaccRecnCellSetFlag (cell, flag);
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

  g_free (cell->cell.value);
  cell->cell.value = g_strdup (string);
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
