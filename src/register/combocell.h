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

#ifndef __COMBO_CELL_H__
#define __COMBO_CELL_H__

#include "basiccell.h"
#include "gnc-common.h"


typedef struct _ComboCell
{
  BasicCell  cell;
} ComboCell;


ComboCell *  xaccMallocComboCell (void);
void         xaccInitComboCell (ComboCell *cell);
void         xaccDestroyComboCell (ComboCell *cell);

void         xaccSetComboCellValue (ComboCell *cell, const char *value);

void         xaccClearComboCellMenu (ComboCell *cell);
void         xaccAddComboCellMenuItem (ComboCell *cell, char * menustr);

/* Determines whether the cell will accept strings not in the
 * menu. Defaults to strict, i.e., only menu items are accepted. */
void         xaccComboCellSetStrict (ComboCell *cell, gboolean strict);

/* Sets a character used for special completion processing. */
void         xaccComboCellSetCompleteChar (ComboCell *cell,
                                           char complete_char);

/* Sets a string which, if the cell has that value, will be returned
 * on an enter, thus preventing the cell from being edited. This is
 * used for transactions with multiple splits. */
void         xaccComboCellSetIgnoreString (ComboCell *cell,
                                           const char *ignore_string);

/* Sets a string which, if the cell has the ignore value, will be
 * returned as the help string. */
void         xaccComboCellSetIgnoreHelp (ComboCell *cell,
                                         const char *ignore_help);

/* Determines whether combocells are automatically raised upon typing.
 * Defaults to false. This is a 'class' method. */
void         xaccComboCellSetAutoPop (gboolean auto_pop_combos);

#endif /* __COMBO_CELL_H__ */

/* --------------- end of file ---------------------- */
