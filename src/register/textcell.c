/*
 * FILE:
 * textcell.c
 *
 * FUNCTION:
 * implements the simplest possible cell -- a text cell
 *
 * HISTORY:
 * Copyright (c) 1988 Linas Vepstas
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
#include "textcell.h"

/* ================================================ */
/* by definition, all text is valid text.  So accept
 * all modifications */

static const char * 
TextMV (struct _BasicCell *_cell,
        const char *oldval, 
        const char *change, 
        const char *newval)
{
   BasicCell *cell = (BasicCell *) _cell;

   xaccSetBasicCellValue (cell, newval);
   return newval;
}

/* ================================================ */

BasicCell *
xaccMallocTextCell (void)
{
   BasicCell *cell;
   cell = xaccMallocBasicCell();
   xaccInitTextCell (cell);
   return cell;
}

/* ================================================ */

void
xaccInitTextCell (BasicCell *cell)
{
   xaccInitBasicCell (cell);

   if (cell->value) free (cell->value);
   cell->value = strdup ("");

   cell->modify_verify = TextMV;
}

/* --------------- end of file ---------------------- */
