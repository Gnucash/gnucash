/*
 * FILE:
 * textcell.h
 *
 * FUNCTION:
 * The TextCell object implements the simplest possible cell -- 
 * a text cell. The text cell simply accepts any and all typed
 * data from the keyboard, and displays in in the cell.
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

#ifndef __XACC_TEXT_CELL_H__
#define __XACC_TEXT_CELL_H__

#include "basiccell.h"

/* installs a callback to handle text recording */
BasicCell * xaccMallocTextCell (void);
void         xaccInitTextCell (BasicCell *);
void         xaccDestroyTextCell (BasicCell *);

#endif /* __XACC_TEXT_CELL_H__ */

/* --------------- end of file ---------------------- */
