/********************************************************************\
 * recncell.h -- reconcile checkbox cell                            *
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
 * recncell.h
 *
 * FUNCTION:
 * The RecnCell object implements a cell handler
 * that will cycle through a series of single-character
 * values when clicked upon by the mouse.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 * Copyright (c) 2001 Derek Atkins
 */

#ifndef RECN_CELL_H
#define RECN_CELL_H

#include <glib.h>

#include "basiccell.h"

typedef gboolean (*RecnCellConfirm) (char old_flag, gpointer data);

typedef struct
{
  BasicCell cell;

  char reconciled_flag; /* The actual flag value */

  char * valid_chars;		/* The list of valid flag types */
  char * char_order;		/* The order of automatic flag selection */
  char 	default_flag;		/* The default flag for unknown user input */

  RecnCellConfirm confirm_cb;
  gpointer confirm_data;
} RecnCell;

BasicCell * gnc_recn_cell_new (void);

void        gnc_recn_cell_set_flag (RecnCell *cell, char reconciled_flag);
char        gnc_recn_cell_get_flag (RecnCell *cell);

void        gnc_recn_cell_set_confirm_cb (RecnCell *cell,
                                          RecnCellConfirm confirm_cb,
                                          gpointer data);
 
/*
 * note that chars is copied into the RecnCell directly, but remains
 * the "property" of the caller.  The caller must maintain the chars
 * pointer, and the caller must setup a mechanism to 'free' the chars
 * pointer.  The rationale is that you may have many RecnCell objects
 * that use the same set of flags -- this saves you an alloc/free for
 * each cell.  - warlord  2001-11-28
 */
void	    gnc_recn_cell_set_valid_chars (RecnCell *cell, const char *chars,
					   char default_flag);
void	    gnc_recn_cell_set_char_order (RecnCell *cell, const char *chars);

#endif
