/********************************************************************\
 * combocell.h -- combo-box pull-down menu cell                     *
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
 * Copyright (c) 2000 Dave Peticolas
 */

#ifndef COMBO_CELL_H
#define COMBO_CELL_H

#include <glib.h>

#include "basiccell.h"
#include "gnc-common.h"


typedef struct _ComboCell
{
  BasicCell cell;
} ComboCell;


BasicCell *  xaccMallocComboCell (void);
void         xaccInitComboCell (ComboCell *cell);

void         xaccSetComboCellValue (ComboCell *cell, const char *value);

void         xaccClearComboCellMenu (ComboCell *cell);
void         xaccAddComboCellMenuItem (ComboCell *cell, char * menustr);

/* Determines whether the cell will accept strings not in the
 * menu. Defaults to strict, i.e., only menu items are accepted. */
void         xaccComboCellSetStrict (ComboCell *cell, gboolean strict);

/* Sets a character used for special completion processing. */
void         xaccComboCellSetCompleteChar (ComboCell *cell,
                                           char complete_char);

/* Add a string to a list of strings which, if the cell has that value,
 * will cause the cell to be uneditable on 'enter'. If the cell has
 * that value, the ignore_help string will be returned by the
 * help handler. */
void         xaccComboCellAddIgnoreString (ComboCell *cell,
                                           const char *ignore_string,
                                           const char *ignore_help);

/* Determines whether the popup list autosizes itself or uses
 * all available space. FALSE by default. */
void         xaccComboCellSetAutoSize (ComboCell *cell, gboolean autosize);

/* Determines whether combocells are automatically raised upon typing.
 * Defaults to false. This is a 'class' method. */
void         xaccComboCellSetAutoPop (gboolean auto_pop_combos);

#endif /* COMBO_CELL_H */

/* --------------- end of file ---------------------- */
