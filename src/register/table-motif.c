/*
 * FILE:
 * table-motif.c
 *
 * FUNCTION:
 * Implements the infrastructure for the displayed table.
 * This is the Motif implementation; this needs to be 
 * ported to GTK, etc. 
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

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xbae/Matrix.h>

#include "cellblock.h"
#include "table-allgui.h"
#include "table-motif.h"

static void enterCB (Widget mw, XtPointer cd, XtPointer cb);
static void leaveCB (Widget mw, XtPointer cd, XtPointer cb);
static void modifyCB (Widget mw, XtPointer cd, XtPointer cb);
static void traverseCB (Widget mw, XtPointer cd, XtPointer cb);

/* The XrmQuarks are used to figure out the direction of
 * traversal from cell to cell */

static XrmQuark QPointer, QLeft, QRight, QUp, QDown;
static Boolean haveQuarks = False;


/* ==================================================== */

Table * 
xaccMallocTable (void)
{
   Table *table;
   table = (Table *) malloc (sizeof (Table));
   xaccInitTable (table);
   return table;
}

/* ==================================================== */

void 
xaccInitTable (Table * table)
{
   table->table_widget = 0;
   table->next_tab_group = 0;

   table->num_phys_rows = 0;
   table->num_phys_cols = 0;
   table->num_virt_rows = 0;
   table->num_virt_cols = 0;
   table->num_header_rows = 0;

   table->current_cursor = NULL;
   table->current_cursor_row = -1;
   table->current_cursor_col = -1;

   table->move_cursor = NULL;
   table->client_data = NULL;

   table->header = NULL;
   table->cursors = NULL;
   table->entries = NULL;
   table->user_data = NULL;

   /* invalidate the "previous" traversed cell */
   table->prev_phys_traverse_row = -1;
   table->prev_phys_traverse_col = -1;
}

/* ==================================================== */

void 
xaccSetTableSize (Table * table, int phys_rows, int phys_cols,
                                 int virt_rows, int virt_cols)
{
   xaccTableResizeStringArr (table, phys_rows, phys_cols);
   xaccTableResizeUserData (table, virt_rows, virt_cols);

   /* invalidate the "previous" traversed cell */
   table->prev_phys_traverse_row = -1;
   table->prev_phys_traverse_col = -1;

   /* invalidate the current cursor position, if needed */
   if ((table->current_cursor_row >= table->num_virt_rows) ||
       (table->current_cursor_col >= table->num_virt_cols)) {
      table->current_cursor_row = -1;
      table->current_cursor_col = -1;
   }
}

/* ==================================================== */

void xaccMoveCursor (Table *table, int virt_row, int virt_col)
{
   int i,j;
   int iphys,jphys;
   BasicCell *cell;

   /* call the callback, allowing the app to commit any changes */
   if (table->move_cursor) {
      (table->move_cursor) (table, table->client_data);
   }

   table->current_cursor_row = virt_row;
   table->current_cursor_col = virt_col;
   table->cursor->user_data = NULL;

   if ((0 > virt_row) || (0 > virt_col)) return;
   if (virt_row >= table->num_rows) return;
   if (virt_col >= table->num_cols) return;

   /* update the cell values to reflect the new position */
   /* also, move the cell GUI, if needed */
   for (i=0; i<table->tile_height; i++) {
      iphys = i + table->current_cursor_row * table->tile_height;
      iphys += table->num_header_rows;
      for (j=0; j<table->tile_width; j++) {
         
         cell = table->cursor->cells[i][j];
         if (cell) {
            jphys = j + table->current_cursor_col * table->tile_width;
            xaccSetBasicCellValue (cell, table->entries[iphys][jphys]);
            cell->changed = 0;
         }
      }
   }

   table->cursor->user_data = table->user_data[virt_row][virt_col];
}

/* ==================================================== */

void xaccMoveCursorGUI (Table *table, int virt_row, int virt_col)
{
   int i,j;
   int iphys,jphys;
   BasicCell *cell;

   /* call the callback, allowing the app to commit any changes */
   if (table->move_cursor) {
      (table->move_cursor) (table, table->client_data);
   }

   table->current_cursor_row = virt_row;
   table->current_cursor_col = virt_col;
   table->cursor->user_data = NULL;

   if ((0 > virt_row) || (0 > virt_col)) {
      /* if the location is invalid, then we should take this 
       * as a command to unmap the cursor gui.  So do it .. */
      for (i=0; i<table->tile_height; i++) {
         for (j=0; j<table->tile_width; j++) {
            cell = table->cursor->cells[i][j];
            if (cell) {
               cell->changed = 0;
               if (cell->move) {
                  (cell->move) (cell, -1, -1);
               }
            }
         }
      }
      return;
   }
   if (virt_row >= table->num_rows) return;
   if (virt_col >= table->num_cols) return;

   /* update the cell values to reflect the new position */
   /* also, move the cell GUI, if needed */
   for (i=0; i<table->tile_height; i++) {
      iphys = i + table->current_cursor_row * table->tile_height;
      iphys += table->num_header_rows;
      for (j=0; j<table->tile_width; j++) {
         
         cell = table->cursor->cells[i][j];
         if (cell) {
            jphys = j + table->current_cursor_col * table->tile_width;
            xaccSetBasicCellValue (cell, table->entries[iphys][jphys]);

            /* if a cell has a GUI, move that too */
            if (cell->move) {
               (cell->move) (cell, iphys, jphys);
            }
         }
      }
   }

   table->cursor->user_data = table->user_data[virt_row][virt_col];
}

/* ==================================================== */

void xaccCommitCursor (Table *table)
{
   int i,j;
   int iphys,jphys;
   BasicCell *cell;
   int virt_row, virt_col;

   virt_row = table->current_cursor_row;
   virt_col = table->current_cursor_col;

   if ((0 > virt_row) || (0 > virt_col)) return;
   if (virt_row >= table->num_rows) return;
   if (virt_col >= table->num_cols) return;

   for (i=0; i<table->tile_height; i++) {
      iphys = i + virt_row * table->tile_height;
      iphys += table->num_header_rows;
      for (j=0; j<table->tile_width; j++) {
         
         cell = table->cursor->cells[i][j];
         if (cell) {
            jphys = j + virt_col * table->tile_width;
            if (table->entries[iphys][jphys]) {
               free (table->entries[iphys][jphys]);
            }
            table->entries[iphys][jphys] = strdup (cell->value);
         }
      }
   }

   table->user_data[virt_row][virt_col] = table->cursor->user_data;
}

/* ==================================================== */
/* verifyCursorPosition checks the location of the cursor 
 * with respect to a row/column position, and repositions 
 * the cursor if necessary.
 */

static void
verifyCursorPosition (Table *table, int phys_row, int phys_col)
{
   int virt_row, virt_col;

   /* compute the virtual position */
   virt_row = phys_row;
   virt_row -= table->num_header_rows;
   virt_row /= table->tile_height;

   virt_col = phys_col;
   virt_col /= table->tile_width;

   if ((virt_row != table->current_cursor_row) ||
       (virt_col != table->current_cursor_col)) {

      /* before leaving, the current virtual position,
       * commit any edits that have been accumulated 
       * in the cursor */
      xaccCommitCursor (table);
      xaccMoveCursorGUI (table, virt_row, virt_col);
   }
}

/* ==================================================== */
/* hack alert -- will core dump if numrows has changed, etc. */

static
void
xaccRefreshHeader (Table *table)
{
   int i,j;
   CellBlock *arr;

   if (!(table->entries)) return;

   /* copy header data into entries cache */
   arr = table->header;
   if (arr) {
      for (i=0; i<arr->numRows; i++) {
         for (j=0; j<arr->numCols; j++) {
            if (table->entries[i][j]) free (table->entries[i][j]);
            if (arr->cells[i][j]) {
               if ((arr->cells[i][j])->value) {
                  table->entries[i][j] = strdup ((arr->cells[i][j])->value);
               } else {
                  table->entries[i][j] = strdup ("");
               }
            } else {
               table->entries[i][j] = strdup ("");
            }
         }
      }
   }
}

/* ==================================================== */

void
xaccNextTabGroup (Table *table, Widget w)
{
   table->next_tab_group = w;
}

/* ==================================================== */
/* this routine calls the individual cell callbacks */

static void
cellCB (Widget mw, XtPointer cd, XtPointer cb)
{
   Table *table;
   XbaeMatrixDefaultActionCallbackStruct *cbs;
   int row, col;
   int rel_row, rel_col;
   CellBlock *arr;
   int invalid = 0;

   table = (Table *) cd;
   arr = table->cursor;
   cbs = (XbaeMatrixDefaultActionCallbackStruct *) cb;

   row = cbs->row;
   col = cbs->column;

   /* can't edit outside of the physical space */
   invalid = (0 > row) || (0 > col) ;
   invalid = invalid || (row >= table->num_phys_rows);
   invalid = invalid || (col >= table->num_phys_cols);

   /* header rows cannot be modified */
   invalid = invalid || (row < table->num_header_rows);

   /* compute the cell location */
   rel_row = row;
   rel_col = col;

   /* remove offset for the header rows */
   rel_row -= table->num_header_rows;

   /* check for a cell handler, but only if cell adress is valid */
   if (arr && !invalid) {
      rel_row %= (arr->numRows);
      rel_col %= (arr->numCols);
      if (! (arr->cells[rel_row][rel_col])) {
         invalid = TRUE;
      } else {
         /* if cell is marked as output-only,
          * then don't call callbacks */
         if (0 == (arr->cells[rel_row][rel_col])->input_output) {
            invalid = TRUE;
         }
      }

   } else {
      invalid = TRUE;
   }

   /* oops the callback failed for some reason ... 
    * reject the enter/edit/leave  and return */
   if (invalid) {
      switch (cbs->reason) {
         case XbaeEnterCellReason: {
            XbaeMatrixEnterCellCallbackStruct *ecbs;
            ecbs = (XbaeMatrixEnterCellCallbackStruct *) cbs;
            ecbs->doit = False;
            ecbs->map = False;
            break;
         }
         case XbaeModifyVerifyReason: {
            XbaeMatrixModifyVerifyCallbackStruct *mvcbs;
            mvcbs = (XbaeMatrixModifyVerifyCallbackStruct *) cbs;
            mvcbs->verify->doit = False;
            break;
         }
         case XbaeLeaveCellReason: {
            XbaeMatrixLeaveCellCallbackStruct *lcbs;
            lcbs = (XbaeMatrixLeaveCellCallbackStruct *) cbs;
            /* must set doit to true in order to be able to leave the cell */
            lcbs->doit = True;
            break;
         }
      }
      return;
   }

   /* if we got to here, then there is a cell handler for 
    * this cell. Dispatch for processing. */
   switch (cbs->reason) {
      case XbaeEnterCellReason: {
         verifyCursorPosition (table, row, col);
         enterCB (mw, cd, cb);
         break;
      }
      case XbaeModifyVerifyReason: {
         modifyCB (mw, cd, cb);
         break;
      }
      case XbaeLeaveCellReason: {
         leaveCB (mw, cd, cb);
         break;
      }
   }
}

/* ==================================================== */
/* this callback assumes that basic error checking has already
 * been performed. */

static void
enterCB (Widget mw, XtPointer cd, XtPointer cb)
{
   Table *table;
   CellBlock *arr;
   XbaeMatrixEnterCellCallbackStruct *cbs;
   int row, col;
   int rel_row, rel_col;
   const char * (*enter) (struct _BasicCell *, const char *);

   table = (Table *) cd;
   arr = table->cursor;
   cbs = (XbaeMatrixEnterCellCallbackStruct *) cb;

   /* compute the cell location */
   row = cbs->row;
   col = cbs->column;
   rel_row = row - table->num_header_rows;
   rel_col = col;
   rel_row %= (arr->numRows);
   rel_col %= (arr->numCols);

printf ("enter %d %d \n", row, col);

   /* since we are here, there must be a cell handler.
    * therefore, we accept entry into the cell by default, 
    */
   cbs->doit = True;
   cbs->map = True;

   /* OK, if there is a callback for this cell, call it */
   enter = arr->cells[rel_row][rel_col]->enter_cell;
   if (enter) {
      const char *val;
      char *retval;

      val = table->entries[row][col];
      retval = (char *) enter (arr->cells[rel_row][rel_col], val);
      if (NULL == retval) retval = (char *) val;
      if (val != retval) {
         if (table->entries[row][col]) free (table->entries[row][col]);
         table->entries[row][col] = retval;
         (arr->cells[rel_row][rel_col])->changed = 0xffffffff;
         XbaeMatrixSetCell (mw, row, col, retval);
         XbaeMatrixRefreshCell (mw, row, col);

         /* don't map a text widget */
         cbs->map = False;
         cbs->doit = False;
      }
   }

   /* record this position as the cell that will be
    * traversed out of if a traverse even happens */
   table->prev_phys_traverse_row = row;
   table->prev_phys_traverse_col = col;
}

/* ==================================================== */
/* this routine calls the individual cell callbacks */

static void
modifyCB (Widget mw, XtPointer cd, XtPointer cb)
{
   Table *table;
   CellBlock *arr;
   XbaeMatrixModifyVerifyCallbackStruct *cbs;
   int row, col;
   int rel_row, rel_col;
   const char * (*mv) (struct _BasicCell *, 
                       const char *, 
                       const char *, 
                       const char *);
   const char *oldval, *change;
   char *newval;
   const char *retval;
   int len;

   table = (Table *) cd;
   arr = table->cursor;
   cbs = (XbaeMatrixModifyVerifyCallbackStruct *) cb;

   /* compute the cell location */
   row = cbs->row;
   col = cbs->column;
   rel_row = row;
   rel_col = col;
   rel_row -= table->num_header_rows;
   rel_row %= (arr->numRows);
   rel_col %= (arr->numCols);

   /* accept edits by default, unless the cell handler rejects them */
   cbs->verify->doit = True;

   oldval = cbs->prev_text;
   change = cbs->verify->text->ptr;

   /* first, compute the newval string */
   len = 1;
   if (oldval) len += strlen (oldval);
   if (change) len += strlen (change);
   newval = (char *) malloc (len);

   /* text may be inserted, or deleted, or replaced ... */
   newval[0] = 0;
   strncat (newval, oldval, cbs->verify->startPos);
   if (change) strcat (newval, change);
   strcat (newval, &oldval[(cbs->verify->endPos)]);

   /* OK, if there is a callback for this cell, call it */
   mv = arr->cells[rel_row][rel_col]->modify_verify;
   if (mv) {
      retval = (*mv) (arr->cells[rel_row][rel_col], oldval, change, newval);

      /* if the callback returned a non-null value, allow the edit */
      if (retval) {

         /* update data. bounds check done earlier */
         free (table->entries[row][col]);
         table->entries[row][col] = (char *) retval;
         (arr->cells[rel_row][rel_col])->changed = 0xffffffff;

         /* if the callback modified the display string,
          * update the display cell as well */
         if (retval != newval) {
            XbaeMatrixSetCell (mw, row, col, (char *) retval);
            XbaeMatrixRefreshCell (mw, row, col);
            XbaeMatrixSetCursorPosition (mw, (cbs->verify->endPos) +1);

            /* the default update has already been overridden,
             * so don't allow Xbae to update */
            cbs->verify->doit = False;

            /* avoid wasting memory */
            free (newval);
         }
      } else {
         /* NULL return value means the edit was rejected */
         cbs->verify->doit = False;

         /* avoid wasting memory */
         free(newval);
      }
   } else {
      /* update data. bounds check done earlier */
      free (table->entries[row][col]);
      table->entries[row][col] = newval;
   }
}

/* ==================================================== */

static void
leaveCB (Widget mw, XtPointer cd, XtPointer cb)
{
   Table *table;
   CellBlock *arr;
   XbaeMatrixLeaveCellCallbackStruct *cbs;
   int row, col;
   int rel_row, rel_col;
   const char * (*leave) (struct _BasicCell *, const char *);
   char * newval;

   table = (Table *) cd;
   arr = table->cursor;
   cbs = (XbaeMatrixLeaveCellCallbackStruct *) cb;

   /* compute the cell location */
   row = cbs->row;
   col = cbs->column;
   rel_row = row - table->num_header_rows;
   rel_col = col;
   rel_row %= (arr->numRows);
   rel_col %= (arr->numCols);

printf ("leave %d %d \n", row, col);

   /* by default, accept whatever the final proposed edit is */
   cbs->doit = True;

   /* OK, if there is a callback for this cell, call it */
   leave = arr->cells[rel_row][rel_col]->leave_cell;
   if (leave) {
      const char *val, *retval;

      val = cbs->value;
      retval = leave (arr->cells[rel_row][rel_col], val);

      newval = (char *) retval;
      if (NULL == retval) newval = strdup (val);
      if (val == retval) newval = strdup (val);

      /* if the leave() routine declared a new string, lets use it */
      if ( retval && (retval != val)) {
         cbs->value = strdup (retval);
      }

   } else {
      newval = strdup (cbs->value);
   }

   /* save whatever was returned; but lets check for  
    * changes to avoid roiling the cells too much */
   if (table->entries[row][col]) {
      if (strcmp (table->entries[row][col], newval)) {
         free (table->entries[row][col]);
         table->entries[row][col] = newval;
         (arr->cells[rel_row][rel_col])->changed = 0xffffffff;
      } else {
         /* leave() allocated memory, which we will not be using ... */
         free (newval);
      }
   } else {
      table->entries[row][col] = newval;
      (arr->cells[rel_row][rel_col])->changed = 0xffffffff;
   }
}


/* ==================================================== */

static void
traverseCB (Widget mw, XtPointer cd, XtPointer cb)
{
   Table *table;
   CellBlock *arr;
   XbaeMatrixTraverseCellCallbackStruct *cbs;
   int row, col;
   int rel_row, rel_col;

   table = (Table *) cd;
   arr = table->cursor;
   cbs = (XbaeMatrixTraverseCellCallbackStruct *) cb;

   row = cbs->row;
   col = cbs->column;

   /* If the quark is zero, then it is likely that we are
    * here because we traversed out of a cell that had a 
    * ComboBox in it.  The ComboCell is clever enough to put
    * us back into the register after tabing out of it.
    * However, its not (cannot be) clever enough to pretend
    * that it was a tab group in the register.  Thus,
    * we will emulate that we left a tab group in the register
    * to get here.  To put it more simply, we just set the 
    * row and column to that of the ComboCell, which we had
    * previously recorded, and continue on as if nothing 
    * happened.  
    * BTW -- note that we are emulating a normal, right-moving tab. 
    * Backwards tabs are broken. 
    */
   if (NULLQUARK == cbs->qparam) {
      if ((0==row) && (0==col)) {
        if ((0 <= table->prev_phys_traverse_row) && 
            (0 <= table->prev_phys_traverse_col)) {
          cbs->qparam = QRight;
          row = table->prev_phys_traverse_row;
          col = table->prev_phys_traverse_col;
        }
      }
   }

   verifyCursorPosition (table, row, col);

   /* compute the cell location */
   rel_row = row - table->num_header_rows;
   rel_col = col;
   rel_row %= (arr->numRows);
   rel_col %= (arr->numCols);

   /* process right-moving traversals */
   if (QRight == cbs->qparam) {
      int next_row = arr->right_traverse_r[rel_row][rel_col];
      int next_col = arr->right_traverse_c[rel_row][rel_col];

      /* if we are at the end of the traversal chain,
       * hop out of this tab group, and into the next.
       */
      if ((0 > next_row) || (0 > next_col)) {
         /* reverse the sign of next_row, col to be positive. */
         cbs->next_row    = row - rel_row - next_row -1; 
         cbs->next_column = col - rel_col - next_col -1;
         cbs->qparam      = NULLQUARK; 
         if (table->next_tab_group) {
            XmProcessTraversal (table->next_tab_group, 
                                XmTRAVERSE_CURRENT); 
         }
      } else {
         cbs->next_row    = row - rel_row + next_row; 
         cbs->next_column = col - rel_col + next_col;
      }
   } 

   table->prev_phys_traverse_row = cbs->next_row;
   table->prev_phys_traverse_col = cbs->next_column;
}

/* ==================================================== */

Widget
xaccCreateTable (Table *table, Widget parent, char * name) 
{
   CellBlock *curs;
   unsigned char * alignments;
   short * widths;
   Widget reg;

   if (!table) return 0;

   /* if quarks have not yet been initialized for this 
    * application, initialize them now. */
   if (!haveQuarks) {
      QPointer = XrmPermStringToQuark ("Pointer");
      QLeft    = XrmPermStringToQuark ("Left");
      QRight   = XrmPermStringToQuark ("Right");
      QUp      = XrmPermStringToQuark ("Up");
      QDown    = XrmPermStringToQuark ("Down");
      haveQuarks = True;
   }

   /* if a header exists, get alignments, widths from there */
   alignments = NULL;
   widths = NULL;
   if (table->cursor) {
      alignments = table->cursor->alignments;
      widths = table->cursor->widths;
   }
   if (table->header) {
      alignments = table->header->alignments;
      widths = table->header->widths;
   }

   /* copy header data into entries cache */
   xaccRefreshHeader (table);

   /* create the matrix widget */
   reg = XtVaCreateWidget( name,
                  xbaeMatrixWidgetClass,  parent,
                  XmNcells,               table->entries,
                  XmNfixedRows,           table->num_header_rows,
                  XmNfixedColumns,        0,
                  XmNrows,                table->num_phys_rows,
                  XmNvisibleRows,         15,
                  XmNfill,                True,
                  XmNcolumns,             table->num_phys_cols,
                  XmNcolumnWidths,        widths,
                  XmNcolumnAlignments,    alignments,
                  XmNgridType,            XmGRID_SHADOW_IN,
                  XmNshadowType,          XmSHADOW_ETCHED_IN,
                  XmNverticalScrollBarDisplayPolicy,XmDISPLAY_STATIC,
                  XmNselectScrollVisible, True,
                  XmNtraverseFixedCells,  False,
                  XmNnavigationType,      XmEXCLUSIVE_TAB_GROUP,
                  /* XmNnavigationType,      XmSTICKY_TAB_GROUP,  */
                  NULL);
    
   XtManageChild (reg);

   /* add callbacks that handle cell editing */
   XtAddCallback (reg, XmNenterCellCallback, cellCB, (XtPointer)table);
   XtAddCallback (reg, XmNleaveCellCallback, cellCB, (XtPointer)table);
   XtAddCallback (reg, XmNmodifyVerifyCallback, cellCB, (XtPointer)table);
   XtAddCallback (reg, XmNtraverseCellCallback, traverseCB, (XtPointer)table);

   table->table_widget = reg;

   /* if any of the cells have GUI specific compnents that need 
    * initialization, initialize them now. */

   curs = table->cursor;
   if (curs) {
      int i,j;

      for (i=0; i<curs->numRows; i++) {
         for (j=0; j<curs->numCols; j++) {
            BasicCell *cell;
            cell = curs->cells[i][j];
            if (cell) {
               void (*xt_realize) (struct _BasicCell *, 
                                   void *gui,
                                   int pixel_width);
               xt_realize = cell->realize;
               if (xt_realize) {
                  int pixel_width;
                  pixel_width = XbaeMatrixGetColumnPixelWidth (reg, j);
                  xt_realize (((struct _BasicCell *) cell), 
                              ((void *) reg), pixel_width);
               }
            }
         }
      }
   }

   return (reg);
}

/* ==================================================== */

void        
xaccRefreshTableGUI (Table * table)
{
{int i,j;
printf (" refresh %d %d \n",  table->num_phys_rows,table->num_phys_cols);
for (i=0; i<table->num_phys_rows; i++) {
printf ("cell %d %s \n", i, table->entries[i][3]);
}}
  XtVaSetValues (table->table_widget, XmNrows,    table->num_phys_rows,
                                      XmNcolumns, table->num_phys_cols,
                                      XmNcells,   table->entries, 
                                      NULL);
}

/* ================== end of file ======================= */
