/*
 * FILE:
 * table-allgui.h
 *
 * FUNCTION:
 * Implements the gui-independent parts of the table infrastructure.
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

#ifndef __XACC_TABLE_ALLGUI_H__
#define __XACC_TABLE_ALLGUI_H__

/* hack alert -- move a portion of the gui-independent
 * table structure definition, currentlu in table-motif.h, 
 * to here.  But C lacks the inheritance of C++, so this
 * is ugly. 
 */

#ifdef MOTIF
#include "table-motif.h"
#endif 

#ifdef GNOME
#include "table-gtk.h"
#endif 

/* free the gui-independent parts of the table structure. */
void  xaccFreeTableEntries (Table *);

/* resize the various arrays that compose the table */
void 
xaccTableResize (Table * table, int num_phys_rows, int num_phys_cols,
                                int new_virt_rows, int new_virt_cols);

/* indicate what handler should be used for a given virtual block */
void 
xaccSetCursor (Table *table, CellBlock *curs,
              int phys_row_origin, int phys_col_origin,
              int virt_row, int virt_col);


/* move the cursor (but not the GUI) to the indicated location. */
void        xaccMoveCursor (Table *, int phys_row, int phys_col);

/* move the cursor GUI to the indicated location. */
void        xaccMoveCursorGUI (Table *, int phys_row, int phys_col);

/* copy text in the cursor cells to the table */
void        xaccCommitCursor (Table *);

/* hackl alert --
 * for all practical purposes, RefreshHeader is identical
 * tp CommitCursor(), except that it acts on cellblock 0,0.
 * it should probably be made obsolete.
 */
void        xaccRefreshHeader (Table *);


/* xaccVerifyCursorPosition checks the location of the cursor 
 * with respect to a physical row/column position, and if the 
 * resulting virtual position has changed, commits the changes in the
 * old position, and the repositions the cursor & gui to the new position.
 */

void
xaccVerifyCursorPosition (Table *table, int phys_row, int phys_col);

#endif /* __XACC_TABLE_ALLGUI_H__ */

/* ================== end of file ======================= */
