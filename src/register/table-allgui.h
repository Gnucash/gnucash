/*
 * FILE:
 * table-allgui.h
 *
 * FUNCTION:
 * Implements the gui-independent parts of the table infrastructure.
 *
 * HISTORY:
 * Copyright (c) 1988 Linas Vepstas
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

#ifndef __XACC_TABLE_ALLGUI_H__
#define __XACC_TABLE_ALLGUI_H__

/* hack-alert, to pick up the table definition.
 * since C lacks the inheritance of C++ 
 * fix this someday.
 */
#define XMOTIF
#ifdef XMOTIF
#include "table-motif.h"
#endif 

#ifdef GNOME
#include "table-gtk.h"
#endif 

extern void 
xaccTableResizeStringArr (Table * table, int num_phys_rows, int num_phys_cols);

extern void 
xaccTableResizeUserData (Table * table, int new_virt_rows, int new_virt_cols);

extern void 
xaccAddCursor (Table *table, CellBlock *curs);

/* count the number of phys rows we'll need, in prep for the malloc */
extern void
xaccTableCount (Table *table, CellBlock *curs);

#endif /* __XACC_TABLE_ALLGUI_H__ */

/* ================== end of file ======================= */
