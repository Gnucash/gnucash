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
 * table-qt.cpp
 *
 * FUNCTION:
 * Implements the infrastructure for the displayed table.
 * This is the QT implementation;
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 1998 Rob Browning <rlb@cs.utexas.edu>
 */

extern "C" {

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cellblock.h"
#include "table-allgui.h"

/* This static indicates the debugging module that this .o belongs to.  */
// static short module = MOD_QT_REG;

/* ==================================================== */

void
xaccNextTabGroup (Table *table, void * w) {
//   table->next_tab_group = w;
}

/* ==================================================== */

void        
doRefreshCursorGUI (Table * table, CellBlock *curs,
                    int from_row, int from_col, gncBoolean do_scroll)
{
}

void        
xaccRefreshTableGUI (Table * table) {
}

#if 0

static int
verify_cell_interaction_OK(Table *table, const int row, const int col)
{
  /* This was cellCB in a previous (Motif) life. We don't do nearly
     the checking that the Xbae code did because we presume that the
     row/column has to be valid if the user could have clicked on it.
     That's reasonable given the way that CList works.  For one thing,
     header rows are handled separately. */
  
  const CellBlock *arr = table->current_cursor;
  int rel_row, rel_col;
  int invalid = 0;
  
  /* make sure that we've repositioned the cursor to this location,
   * and that we've dragged along any subsidiary GUI's with us. 
   * Do this *before* we use the value of the "current cursor"
   */
  xaccVerifyCursorPosition (table, row, col);

  arr = table->current_cursor;

  /* compute the cell location */
  rel_row = table->locators[row][col]->phys_row_offset;
  rel_col = table->locators[row][col]->phys_col_offset;

  /* verify that cursor offsets are valid.  This may occur if 
   * the app that is using the table has a paritally initialized
   * cursor. (probably due to a prograing error, but maybe they 
   * meant to do this). */
  invalid = invalid || (0 > rel_row);
  invalid = invalid || (0 > rel_col);

  /* check for a cell handler, but only if cell adress is valid */
  /* QT may not need all these checks, but they don't hurt */
  if (arr && !invalid) {
    if (! (arr->cells[rel_row][rel_col])) {
      invalid = TRUE;
    } else {
      /* if cell is marked as output-only,
       * then don't call callbacks */
      if (0 == (XACC_CELL_ALLOW_INPUT &
                ((arr->cells[rel_row][rel_col])->input_output))) {
        invalid = TRUE;
      }
    }
  } else {
    invalid = TRUE;
  }

  return(!invalid);
}

/* ==================================================== */
/* this callback assumes that basic error checking has already
 * been performed. */

static void
cell_entered(Table *table, const int row, const int col) {
}

/* ==================================================== */

static void
compute_string_single_change(const gchar *a, const gchar *b, gchar **result) {
  /* Compute the change from a to b assuming that the changed region
     is contiguous.  This is only a guess, the solution is
     inherently ambiguous. */
  
  const gint a_len = strlen(a); 
  const gint b_len = strlen(b); 
  const gchar *afptr = a, *bfptr = b;
  const gchar *arptr = a + a_len;
  const gchar *brptr = b + b_len;
  
  while(*afptr && *bfptr && (*afptr == *bfptr)) {
    afptr++;
    bfptr++;
  }
  
  while((arptr != afptr) && (brptr != bfptr) && (*arptr == *brptr)) {
    arptr--;
    brptr--;
  }
  if(a_len == b_len) brptr++;

  if(bfptr == brptr) {
    /* deletion or nothing */
    *result = NULL;
    return;
  } else {
    const gint length = (brptr - bfptr);
    *result = (char *) g_malloc(length * sizeof(char) + 1);
    strncpy(*result, bfptr, length);
    (*result)[length] = '\0';
    return;
  }
}
    
/* ==================================================== */
/* this routine calls the individual cell callbacks */

static void
cell_modified(Table *table, const int row, const int col)
{
}

/* ==================================================== */

static void
cell_left(Table *table, const int row, const int col) {

}


/* ==================================================== */



/* ==================================================== */

void *
xaccCreateTable (Table *table, void *parent)  {
  return NULL;
}

/* ==================================================== */
/* if any of the cells have GUI specific components that 
 * need initialization, initialize them now. 
 * The cell realize method, if present on a cell,
 * is how that cell can find out that now is the time to 
 * initialize that GUI.
 */

void        
xaccCreateCursor (Table * table, CellBlock *curs) { 
}

/* ==================================================== */

void        
xaccRefreshTableGUI (Table * table) {
  
  if (!table) return;
  if (!(table->table_widget)) return;

  DEBUGCMD ({int i;
  printf (" refresh numphysrows=%d numphyscols=%d =========================\n", 
     table->num_phys_rows,table->num_phys_cols);
     for (i=0; i<table->num_phys_rows; i++) {
     printf ("cell %d\tcolor: 0x%x\tact:%s\tdescr: %s\tpay: %s\n", i, 
     table->bg_colors[i][3], 
     table->entries[i][2],
     table->entries[i][3],
     table->entries[i][5]);
     }});

  {
    /* The 0'th row of the handlers is defined as the header */
    GtkSheet *reg = GTK_SHEET(table->table_widget);    
    
    gtk_sheet_freeze(reg);
    
    /* Adjust table to have the right number of rows */
    {
      /* maxrow is actually the maximum index, not the total number of rows */
      const glong diffrows = (reg->maxrow + 1) - table->num_phys_rows;
      gint row, col;
      
      if(diffrows > 0) {
        gtk_sheet_add_row(reg, diffrows);
      } else if(diffrows < 0) {
        gtk_sheet_delete_rows(reg, 0, diffrows);
      }
      
      /* Set the per-cell contents, colors, etc. */
      for(row = 0; row < table->num_phys_rows; row++) {
        for(col = 0; col < table->num_phys_cols; col++) {
          update_cell(reg, table, row, col);
        }
      }
    }
    gtk_sheet_thaw(reg); 
  }
}

/* ==================================================== */

void        
doRefreshCursorGUI (Table * table, CellBlock *curs, int from_row, int from_col)
{
  int phys_row, phys_col;
  int to_row, to_col;
  int i,j;
  
  /* if the current cursor is undefined, there is nothing to do. */
  if (!curs) return;
  if ((0 > from_row) || (0 > from_col)) return;
  
  /* compute the physical bounds of the current cursor */
  phys_row = from_row;
  phys_col = from_col;
  from_row -= table->locators[phys_row][phys_col]->phys_row_offset;
  from_col -= table->locators[phys_row][phys_col]->phys_col_offset;
  to_row = from_row + curs->numRows;
  to_col = from_col + curs->numCols;
  
  {
    GtkSheet *reg = GTK_SHEET(table->table_widget);    

    gtk_sheet_freeze(reg);
    
    /* cycle through, cell by cell, copying our values to the widget */
    for (i=from_row; i<to_row; i++) {
      for (j=from_col; j<to_col; j++) {
        update_cell(reg, table, i, j);
      }
    }
    gtk_sheet_thaw(reg);
  }
}

#endif

/* ==================================================== */

int gnc_table_column_width(Table *table, const int col)
{
  return 0;
}

}

/* ================== end of file ======================= */
