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
#include <time.h>

#include "basiccell.h"
#include "recncell.h"

/* hack alert -- temp defs: we should  should probably include 
 * Transaction.h and also implement a more sophisticated togglig
 * between the various reconcile states. 
 */

#define NREC 'n'
#define CREC 'c'

/* ================================================ */

static const char * 
ToggleRecn (BasicCell *_cell, const char *cur_val)
{
   BasicCell *cell = (BasicCell *) _cell;
   char buff[2];

   if (NREC == cur_val[0]) {
     buff[0] = CREC;
   } else {
     buff[0] = NREC;
   }
   buff[1] = 0x0;
   
   xaccSetBasicCellValue (cell, buff);
   return strdup (buff);
}

/* ================================================ */

BasicCell *
xaccMallocRecnCell (void)
{
   BasicCell *cell;
   cell = xaccMallocBasicCell();
   xaccInitRecnCell (cell);
   return cell;
}

/* ================================================ */

void
xaccInitRecnCell (BasicCell *cell)
{
   char buff[2];

   buff[0] = NREC;
   buff[1] = 0x0;

   if (cell->value) free (cell->value);
   cell ->value = strdup (buff);

   cell->enter_cell = ToggleRecn;
}

/* ================================================ */

void
xaccDestroyRecnCell (BasicCell *cell)
{
   xaccDestroyBasicCell (cell);
}

/* --------------- end of file ---------------------- */
