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

/*
 * FILE:
 * numcell.h
 *
 * FUNCTION:
 * The NumCell object implements a number handling cell. It
 * supports a number of accelerator keys for number entry.
 *
 * HISTORY:
 * Copyright (c) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>
 */
 
#ifndef __GNC_NUM_CELL_H__
#define __GNC_NUM_CELL_H__

#include "basiccell.h"

typedef struct _NumCell
{
  BasicCell cell;
  long int max_num;
} NumCell;

NumCell * xaccMallocNumCell (void);
void      xaccInitNumCell (NumCell *);
void      xaccDestroyNumCell (NumCell *);

void      xaccSetNumCellValue (NumCell *cell, const char *str);

#endif /* __GNC_NUM_CELL_H__ */
