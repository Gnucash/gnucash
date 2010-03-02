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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/**
 * @file combocell.h
 * @brief The ComboCell object implements a cell handler with a
 *        "combination-box" pull-down menu in it.
 *
 * On output, the currently selected menu item is displayed.
 * On input, the user can select from a list in the pull-down menu,
 * or use the keyboard to slect a menu entry by typing the first
 * few menu characters.
 *
 * @author Created Jan 1998 Linas Vepstas
 * @author Copyright (c) 1998 Linas Vepstas <linas@linas.org>
 * @author Copyright (c) 2000 Dave Peticolas
 */

#ifndef COMBO_CELL_H
#define COMBO_CELL_H

#include <glib.h>

#include "basiccell.h"
#include "QuickFill.h"

typedef struct
{
    BasicCell cell;
    gpointer shared_store;
} ComboCell;


BasicCell *  gnc_combo_cell_new (void);
void         gnc_combo_cell_init (ComboCell *cell);

void         gnc_combo_cell_set_value (ComboCell *cell, const char *value);

void         gnc_combo_cell_clear_menu (ComboCell *cell);

/** Add a menu item to the list. */
void         gnc_combo_cell_add_menu_item (ComboCell *cell, const char * menustr);

/** Add a 'account name' menu item to the list. When testing for
 *  equality with the currently selected item, this function will
 *  ignore the characters normally used to separate account names. */
void         gnc_combo_cell_add_account_menu_item (ComboCell *cell, char * menustr);

/** Enable sorting of the menu item's contents. Loading the item is
 *  much faster with sorting disabled. */
void         gnc_combo_cell_set_sort_enabled (ComboCell *cell, gboolean enabled);

/** Determines whether the cell will accept strings not in the
 * menu. Defaults to strict, i.e., only menu items are accepted. */
void         gnc_combo_cell_set_strict (ComboCell *cell, gboolean strict);

/** Sets a character used for special completion processing. */
void         gnc_combo_cell_set_complete_char (ComboCell *cell,
        gunichar complete_char);

/** Add a string to a list of strings which, if the cell has that value,
 * will cause the cell to be uneditable on 'enter'. */
void         gnc_combo_cell_add_ignore_string (ComboCell *cell,
        const char *ignore_string);

/** Determines whether the popup list autosizes itself or uses
 * all available space. FALSE by default. */
void         gnc_combo_cell_set_autosize (ComboCell *cell, gboolean autosize);

/** Tell the combocell to use a shared QuickFill object.  Using this routine
 *  can dramatically improve performance when creating combocells with a
 *  large number of entries.  For example, users with thousands of accounts
 *  are complaining about 10-second register startup times, of which 98%
 *  of the cpu is spent building the multi-thousand entry quickfill.
 *  When a shared quickfill is specified, the combo-cell will not add to
 *  nor delete the quickfill; it is the users resonsibility to manage the
 *  quickfill object.  The combocell will *not* make a copy of teh quickfill.
 */
void gnc_combo_cell_use_quickfill_cache (ComboCell *cell, QuickFill *shared_qf);

void gnc_combo_cell_use_list_store_cache (ComboCell * cell, gpointer data);

#endif
