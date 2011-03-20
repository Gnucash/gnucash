/********************************************************************\
 * checkboxcell.h -- yes/no checkbox cell                           *
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

/*
 * FILE:
 * checkboxcell.h
 *
 * FUNCTION:
 * The CheckboxCell object implements a cell handler
 * that will toggle between yes and no values when clicked upon by the mouse.
 *
 * HISTORY:
 * Copyright (c) 2002 Derek Atkins
 */

#ifndef CHECKBOX_CELL_H
#define CHECKBOX_CELL_H

#include <glib.h>

#include "basiccell.h"

typedef struct
{
    BasicCell cell;

    gboolean flag;

} CheckboxCell;

BasicCell * gnc_checkbox_cell_new (void);

void        gnc_checkbox_cell_set_flag (CheckboxCell *cell, gboolean flag);
gboolean    gnc_checkbox_cell_get_flag (CheckboxCell *cell);
const char* gnc_checkbox_cell_get_string (gboolean flag);

#endif
