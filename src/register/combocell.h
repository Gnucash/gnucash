/*
 * FILE:
 * combocell.h
 *
 * FUNCTION:
 * The ComboCell object implements a cell handler with a 
 * "combination-box" pull-down menu in it.  
 * 
 * On output, the currently selected menu item is displayed.
 * On input, the user can select from a list in the pull-down menu,
 * or use the keyboard to slect a menu entry by typing the first
 * few menu characters.
 *
 * METHODS:
 * The xaccAddComboCellMenuItem() method can be used to add a menu
 * item to the list.
 *
 *
 * HISTORY:
 * Created Jan 1998 Linas Vepstas 
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

#ifndef __XACC_COMBO_CELL_H__
#define __XACC_COMBO_CELL_H__

#include "basiccell.h"

typedef struct _ComboCell {
   BasicCell cell;
   char **   menuitems;         /* not used in the gnome version */
} ComboCell;

ComboCell *  xaccMallocComboCell (void);
void         xaccInitComboCell (ComboCell *);
void         xaccDestroyComboCell (ComboCell *);

void         xaccSetComboCellValue (ComboCell *, const char *);

void         xaccClearComboCellMenu (ComboCell *);
void         xaccAddComboCellMenuItem (ComboCell *, char * menustr);

#endif /* __XACC_COMBO_CELL_H__ */

/* --------------- end of file ---------------------- */
