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
 * recncell.h
 *
 * FUNCTION:
 * The RecnCell object implements a cell handler
 * that will cycle throguh a series of single-character
 * values when clicked upon by the mouse.
 *
 * hack alert -- ther should be a way of specifying what these values
 * are, instead of having them hard coded as they currently are.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 */

#ifndef __XACC_RECN_CELL_C__
#define __XACC_RECN_CELL_C__

#include "basiccell.h"

/* installs a callback to handle reconcile flag */
BasicCell * xaccMallocRecnCell (void);
void         xaccInitRecnCell (BasicCell *);
void         xaccDestroyRecnCell (BasicCell *);

#endif /* __XACC_RECN_CELL_C__ */

/* --------------- end of file ---------------------- */
