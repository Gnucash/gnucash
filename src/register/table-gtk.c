/*
 * FILE:
 * table-gtk.c
 *
 * FUNCTION:
 * Implements the infrastructure for the displayed table.
 * This is the Gtk implementation;
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 1998 Rob Browning <rlb@cs.utexas.edu>
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

#include <stdio.h>
#include <stdlib.h>

#include <gtk/gtk.h>

#include "cellblock.h"
#include "table-allgui.h"
#include "table-gtk.h"

#if 0
static void enterCB (GtkWidget * mw, gpointer cd, gpointer cb);
static void leaveCB (GtkWidget * mw, gpointer cd, gpointer cb);
static void modifyCB (GtkWidget * mw, gpointer cd, gpointer cb);
static void traverseCB (GtkWidget * mw, gpointer cd, gpointer cb);

/* The XrmQuarks are used to figure out the direction of
 * traversal from cell to cell */

//static XrmQuark QPointer, QLeft, QRight, QUp, QDown;
//static Boolean haveQuarks = False;

#endif

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

   table->num_phys_rows = -1;
   table->num_phys_cols = -1;
   table->num_virt_rows = -1;
   table->num_virt_cols = -1;

   table->current_cursor = NULL;
   table->current_cursor_virt_row = -1;
   table->current_cursor_virt_col = -1;
   table->current_cursor_phys_row = -1;
   table->current_cursor_phys_col = -1;

   table->move_cursor = NULL;
   table->client_data = NULL;

   table->entries = NULL;
   table->locators = NULL;
   table->user_data = NULL;
   table->handlers = NULL;

   /* invalidate the "previous" traversed cell */
   table->prev_phys_traverse_row = -1;
   table->prev_phys_traverse_col = -1;
}

/* ==================================================== */

void 
xaccDestroyTable (Table * table)
{
   /* free the gui-independent parts */
   xaccFreeTableEntries (table);

   /* Let GTK know we're finished with this */
   gtk_widget_unref(table->table_widget);

   /* intialize vars to null value so that any access is voided. */
   xaccInitTable (table);
   free (table);
}

/* ==================================================== */

void 
xaccSetTableSize (Table * table, int phys_rows, int phys_cols,
                                 int virt_rows, int virt_cols)
{
   xaccTableResize (table, phys_rows, phys_cols, virt_rows, virt_cols);

   /* invalidate the "previous" traversed cell */
   table->prev_phys_traverse_row = -1;
   table->prev_phys_traverse_col = -1;

   /* invalidate the current cursor position, if needed */
   if ((table->current_cursor_virt_row >= table->num_virt_rows) ||
       (table->current_cursor_virt_col >= table->num_virt_cols)) {
      table->current_cursor_virt_row = -1;
      table->current_cursor_virt_col = -1;
      table->current_cursor_phys_row = -1;
      table->current_cursor_phys_col = -1;
      table->current_cursor = NULL;
   }
}

/* ==================================================== */

void
xaccNextTabGroup (Table *table, GtkWidget * w)
{
   table->next_tab_group = w;
}

#if 0

/* ==================================================== */
/* this routine calls the individual cell callbacks */
/* hack alert -- assumes that header is first cell */

static void
cellCB (GtkWidget *mw, gpointer cd, gpointer cb)
{
   Table *table;
   XbaeMatrixDefaultActionCallbackStruct *cbs;
   int row, col;
   int rel_row, rel_col;
   CellBlock *arr, *header;
   int invalid = 0;

   table = (Table *) cd;
   arr = table->current_cursor;
   cbs = (XbaeMatrixDefaultActionCallbackStruct *) cb;

   row = cbs->row;
   col = cbs->column;

   /* can't edit outside of the physical space */
   invalid = (0 > row) || (0 > col) ;
   invalid = invalid || (row >= table->num_phys_rows);
   invalid = invalid || (col >= table->num_phys_cols);

   /* header rows cannot be modified */
   header = table->handlers[0][0];
   invalid = invalid || (row < header->numRows);

   /* compute the cell location */
   rel_row = table->locators[row][col]->phys_row_offset;
   rel_col = table->locators[row][col]->phys_col_offset;

   /* check for a cell handler, but only if cell adress is valid */
   if (arr && !invalid) {
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
         default:
            break;
      }
      return;
   }

   /* if we got to here, then there is a cell handler for 
    * this cell. Dispatch for processing. */
   switch (cbs->reason) {
      case XbaeEnterCellReason: {
         xaccVerifyCursorPosition (table, row, col);
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
      default:
         break;
   }
}

/* ==================================================== */
/* this callback assumes that basic error checking has already
 * been performed. */

static void
enterCB (GtkWidget * mw, gpointer cd, gpointer cb)
{
   Table *table;
   CellBlock *arr;
   XbaeMatrixEnterCellCallbackStruct *cbs;
   int row, col;
   int rel_row, rel_col;
   const char * (*enter) (BasicCell *, const char *);

   table = (Table *) cd;
   arr = table->current_cursor;
   cbs = (XbaeMatrixEnterCellCallbackStruct *) cb;

   /* compute the cell location */
   row = cbs->row;
   col = cbs->column;
   rel_row = table->locators[row][col]->phys_row_offset;
   rel_col = table->locators[row][col]->phys_col_offset;

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
modifyCB (GtkWidget * mw, gpointer cd, gpointer cb)
{
   Table *table;
   CellBlock *arr;
   XbaeMatrixModifyVerifyCallbackStruct *cbs;
   int row, col;
   int rel_row, rel_col;
   const char * (*mv) (BasicCell *, 
                       const char *, 
                       const char *, 
                       const char *);
   const char *oldval, *change;
   char *newval;
   const char *retval;
   int len;

   table = (Table *) cd;
   arr = table->current_cursor;
   cbs = (XbaeMatrixModifyVerifyCallbackStruct *) cb;

   /* compute the cell location */
   row = cbs->row;
   col = cbs->column;
   rel_row = table->locators[row][col]->phys_row_offset;
   rel_col = table->locators[row][col]->phys_col_offset;

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
      (arr->cells[rel_row][rel_col])->changed = 0xffffffff;
   }
}

/* ==================================================== */

static void
leaveCB (GtkWidget * mw, gpointer cd, gpointer cb)
{
   Table *table;
   CellBlock *arr;
   XbaeMatrixLeaveCellCallbackStruct *cbs;
   int row, col;
   int rel_row, rel_col;
   const char * (*leave) (BasicCell *, const char *);
   char * newval;

   table = (Table *) cd;
   arr = table->current_cursor;
   cbs = (XbaeMatrixLeaveCellCallbackStruct *) cb;

   /* compute the cell location */
   row = cbs->row;
   col = cbs->column;
   rel_row = table->locators[row][col]->phys_row_offset;
   rel_col = table->locators[row][col]->phys_col_offset;

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
traverseCB (GtkWidget * mw, gpointer cd, gpointer cb)
{
   Table *table;
   CellBlock *arr;
   XbaeMatrixTraverseCellCallbackStruct *cbs;
   int row, col;
   int rel_row, rel_col;

   table = (Table *) cd;
   arr = table->current_cursor;
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

   xaccVerifyCursorPosition (table, row, col);

   /* compute the cell location */
   rel_row = table->locators[row][col]->phys_row_offset;
   rel_col = table->locators[row][col]->phys_col_offset;

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

#endif

/* ==================================================== */

GtkWidget *
xaccCreateTable (Table *table, GtkWidget * parent) 
{
  CellBlock *curs;
  unsigned char * alignments;
  short * widths;
  GtkWidget * reg;
  int num_header_rows = 0;
  int i;
  
  if (!table) return 0;
  
#if 0  
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
#endif
  
  /* The 0'th row of the handlers is defined as the header */
  alignments = NULL;
  widths = NULL;
  curs = table->handlers[0][0];
  alignments = curs->alignments;
  widths = curs->widths;
  num_header_rows = curs->numRows;
  
  /* copy header data into entries cache */
  xaccRefreshHeader (table);
  
  /* create the matrix widget */
  if(num_header_rows > 1) {
    fprintf(stderr,
            "gnucash: GTK implementation only supports one header row.\n");
    num_header_rows = 1;
  }
  
  if(num_header_rows == 0) {
    reg = gtk_clist_new(table->num_phys_cols);
  } else {        
    reg = gtk_clist_new_with_titles(table->num_phys_cols, table->entries[0]);
    gtk_clist_freeze(GTK_CLIST(reg));
    for(i = 0; i < table->num_phys_cols; i++) {

      /* Widths are in units of characters, not pixels, so we have
         this hack.  It should be fixed later... */
      gtk_clist_set_column_width(GTK_CLIST(reg), i, widths[i] * 5);
    }
    gtk_clist_thaw(GTK_CLIST(reg));
  }
  
  gtk_clist_freeze(GTK_CLIST(reg));
  for(i = num_header_rows; i < table->num_phys_rows; i++) {
    gtk_clist_append(GTK_CLIST(reg), table->entries[i]);
  }
  gtk_clist_thaw(GTK_CLIST(reg));
  
  /* size?
     alignments?
     grid type?
     shadow type?
  */
  
  gtk_container_add(GTK_CONTAINER(parent), reg);
  gtk_widget_show(reg);
  
#if 0
  /* add callbacks that handle cell editing */
  
  XtAddCallback (reg, XmNenterCellCallback, cellCB, (gpointer)table);
  XtAddCallback (reg, XmNleaveCellCallback, cellCB, (gpointer)table);
  XtAddCallback (reg, XmNmodifyVerifyCallback, cellCB, (gpointer)table);
  XtAddCallback (reg, XmNtraverseCellCallback, traverseCB, (gpointer)table);
#endif
  
  table->table_widget = reg;
  
#if 0  
  /* if any of the cells have GUI specific components that need 
   * initialization, initialize them now. */
  
  curs = table->current_cursor;
  if (curs) {
    int i,j;
    
    for (i=0; i<curs->numRows; i++) {
      for (j=0; j<curs->numCols; j++) {
        BasicCell *cell;
        cell = curs->cells[i][j];
        if (cell) {
          void (*xt_realize) (BasicCell *, 
                              void *gui,
                              int pixel_width);
          xt_realize = cell->realize;
          if (xt_realize) {
            int pixel_width;
            pixel_width = XbaeMatrixGetColumnPixelWidth (reg, j);
            xt_realize (cell, ((void *) reg), pixel_width);
          }
        }
      }
    }
  }
#endif
  return (reg);
}

/* ==================================================== */

void        
xaccRefreshTableGUI (Table * table)
{
  CellBlock *curs;
  GtkWidget * reg;
  int num_header_rows = 0;
  int i, j;
  
  printf("xaccRefreshTableGUI(%p)\n", table);
  if (!table) return;

  /* The 0'th row of the handlers is defined as the header */
  curs = table->handlers[0][0];
  num_header_rows = curs->numRows;
  reg = table->table_widget;

  printf (" refresh numphysrows=%d numphyscols=%d \n",
          table->num_phys_rows,table->num_phys_cols);
  
  for (i = num_header_rows; i < table->num_phys_rows; i++) {
    printf ("cell %d act:%s descr: %s \n",
            i, table->entries[i][2],
            table->entries[i][3]);
  }

  gtk_clist_freeze(GTK_CLIST(reg));

  while(GTK_CLIST(reg)->rows < table->num_phys_rows) {
    gtk_clist_append(GTK_CLIST(reg), NULL);
  }

  for(i = num_header_rows; i < table->num_phys_rows; i++) {
    for(j = 0; j < table->num_phys_cols; j++) {
      gtk_clist_set_text(GTK_CLIST(reg), i, j, table->entries[i][j]);
    }
  }
  gtk_clist_thaw(GTK_CLIST(reg)); 
}

/* ================== end of file ======================= */

/*
  Local Variables:
  tab-width: 2
  indent-tabs-mode: nil
  mode: c-mode
  c-indentation-style: gnu
  eval: (c-set-offset 'block-open '-)
  End:
*/
