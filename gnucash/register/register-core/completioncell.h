/********************************************************************\
 * completion.h -- combo-box used for completion cell               *
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

/** @addtogroup Cell Cell
 * @{
 * @file completion.h
 * @struct CompletionCell
 * @brief The CompletionCell object implements a cell handler with a
 *        "combination-box" pull-down menu in it.
 *
 * On output, the currently selected menu item is displayed.
 * On input, the user can select from a list in the pull-down menu,
 * or use the keyboard to select a menu entry by typing the first
 * few menu characters.
 *
 * @author Copyright (c) 2023 Robert Fewell
 */

#ifndef COMPLETION_CELL_H
#define COMPLETION_CELL_H

#include <glib.h>

#include "basiccell.h"

typedef struct
{
    BasicCell cell;
} CompletionCell;


BasicCell * gnc_completion_cell_new (void);
void gnc_completion_cell_init (CompletionCell* cell);

void gnc_completion_cell_set_value (CompletionCell* cell, const char* value);

void gnc_completion_cell_clear_menu (CompletionCell* cell);

/** Add a menu item to the hash table list. */
void gnc_completion_cell_add_menu_item (CompletionCell* cell,
                                        const char* menustr);

/** Enable sorting of the menu item's contents. */
void gnc_completion_cell_set_sort_enabled (CompletionCell* cell,
                                           gboolean enabled);

/** Determines whether the cell will accept strings not in the
 * menu. Defaults to strict, i.e., only menu items are accepted. */
void gnc_completion_cell_set_strict (CompletionCell* cell, gboolean strict);

/** Determines whether the popup list autosizes itself or uses
 * all available space. FALSE by default. */
void gnc_completion_cell_set_autosize (CompletionCell* cell, gboolean autosize);

/** Register the sort direction. Used to determine in what order the completion should
 * present the list. FALSE by default */
void gnc_completion_cell_reverse_sort (CompletionCell* cell, gboolean is_reversed);

/** @} */
#endif
