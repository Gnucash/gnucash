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
#include "ui-callbacks.h"

/* hack alert -- I am uncomfortable with including engine
 * stuff here; all code in this directory should really be 
 * independent of the engine.  Its just that we need the
 * defs for YREC, CREC, etc. This is some cleanup we should 
 * do some day.
 */
#include "Transaction.h"

/* ================================================ */

static const char * 
ToggleRecn (BasicCell *_cell, const char *cur_val)
{
   BasicCell *cell = (BasicCell *) _cell;
   char buff[2];

   /* throw up a popup if the user tries to undo a reconciled transcation
      hack alert -- this sets a new precedent ... verifyBox is defined in
      both the motif and the gtk subdirs; I don't think I like it that way.

      Now it's in ui-callbacks.h which is UI independent, but that's
      still perhaps not optimal...  */
   if(cur_val[0] == YREC) {
     if(!verifyBox("Really change state of reconciled transaction?")) {
       return strdup(cur_val);
     }
   }

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
