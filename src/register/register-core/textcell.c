/********************************************************************\
 * textcell.c -- simple plain-ascii text display/input cell         *
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
 * textcell.c
 *
 * FUNCTION:
 * implements the simplest possible cell -- a text cell
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 */

#include <stdlib.h>
#include <string.h>

#include "basiccell.h"
#include "textcell.h"

/* ================================================ */
/* by definition, all text is valid text.  So accept
 * all modifications */

static void
TextMV (struct _BasicCell *_cell,
        const GdkWChar *change,
        int change_len,
        const GdkWChar *newval,
        int newval_len,
        int *cursor_position,
        int *start_selection,
        int *end_selection)
{
  xaccSetBasicCellWCValueInternal (_cell, newval);
}

/* ================================================ */

static void
xaccInitTextCell (BasicCell *cell)
{
  cell->modify_verify = TextMV;
}

/* ================================================ */

BasicCell *
xaccMallocTextCell (void)
{
  BasicCell *cell;

  cell = xaccMallocBasicCell ();
  xaccInitTextCell (cell);

  return cell;
}

/* --------------- end of file ---------------------- */
