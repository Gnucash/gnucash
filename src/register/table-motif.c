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

#include <assert.h>
#include <stdio.h>
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
static void doRefreshCursorGUI (Table * table, CellBlock *curs, int row, int col);

/* The XrmQuarks are used to figure out the direction of
 * traversal from cell to cell */

static XrmQuark QPointer, QLeft, QRight, QUp, QDown;
static Boolean haveQuarks = False;


/* ==================================================== */

void
xaccNextTabGroup (Table *table, Widget w)
{
   table->next_tab_group = w;
}

/* ==================================================== */

static void 
wrapVerifyCursorPosition (Table *table, int row, int col)
{
   CellBlock *save_curs = table->current_cursor;
   int save_phys_row = table->current_cursor_phys_row;
   int save_phys_col = table->current_cursor_phys_col;

   /* VerifyCursor will do all sorts of gui-independent machinations */
   xaccVerifyCursorPosition (table, row, col);

   if ((save_phys_row != table->current_cursor_phys_row) ||
       (save_phys_col != table->current_cursor_phys_col))
   {
      /* make sure *both* the old and the new cursor rows get redrawn */
      xaccRefreshCursorGUI (table);  
      doRefreshCursorGUI (table, save_curs, save_phys_row, save_phys_col);
   }
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
   CellBlock *arr, *header;
   int invalid = 0;

   table = (Table *) cd;
   cbs = (XbaeMatrixDefaultActionCallbackStruct *) cb;

   row = cbs->row;
   col = cbs->column;

   /* If we are entering this cell, make sure that we've
    * moved the cursor, and that any subsidiary GUI elements
    * properly positioned.  Do this *before* we examine the 
    * value of the "current cursor".  Note that this check
    * could suck up massive cpu, as potentially it may trigger
    * a redraw of the entire register.
    *
    * Hmmm ... actually, theoretically, this is not neeeded.
    * Before we get to here, we *should* have gone through
    * a traverse callback the determine which cell to enter,
    * and then, after the leavecell, the cursor should
    * have been automagically repositioned.  This, the
    * call below should always no-op out.  However, if/when
    * things go berzerk, the call below does at least a partial
    * butt-save for us, so lets leave it in for now.
    */
   if (XbaeEnterCellReason == cbs->reason) 
   {
      wrapVerifyCursorPosition (table, row, col);
   }

   /* can't edit outside of the physical space */
   invalid = (0 > row) || (0 > col) ;
   invalid = invalid || (row >= table->num_phys_rows);
   invalid = invalid || (col >= table->num_phys_cols);

   /* header rows cannot be modified */
   /* hack alert -- assumes that header is first cell */
   /* if 0,0 is not a headr  row, then trouble ... */
   header = table->handlers[0][0];
   invalid = invalid || (row < header->numRows);

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
   arr = table->current_cursor;
   if (arr && !invalid) {
      if (! (arr->cells[rel_row][rel_col])) {
         invalid = TRUE;
      } else {
         /* if cell is marked as output-only,
          * then don't call callbacks */
         if (0 == (XACC_CELL_ALLOW_INPUT & ((arr->cells[rel_row][rel_col])->input_output))) 
         {
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
         enterCB (mw, cd, cb);
         break;
      }
      case XbaeModifyVerifyReason: {
         modifyCB (mw, cd, cb);
         break;
      }
      case XbaeLeaveCellReason: {
         /* Do leaveCB first, so that the cell handler can do whatever
          * it needs to do to commit values into its cell handler. 
          * Then do the verify, which in general asumes that cell handlers
          * are up to date (i.e. the leave has been processed.)
          */
         leaveCB (mw, cd, cb);
         wrapVerifyCursorPosition (table, table->reverify_phys_row,
                                          table->reverify_phys_col);
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
enterCB (Widget mw, XtPointer cd, XtPointer cb)
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

printf ("enter %d %d (relrow=%d relcol=%d) cell=%p\n", row, col, rel_row, rel_col,
arr->cells[rel_row][rel_col]);

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
leaveCB (Widget mw, XtPointer cd, XtPointer cb)
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
traverseCB (Widget mw, XtPointer cd, XtPointer cb)
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
    *
    * BTW -- note that we are emulating a normal, right-moving tab. 
    * Backwards tabs are broken. 
    *
    * Note that the quark may also be zero if we are entering 
    * the register for the first time after a table resize.
    */
   if (NULLQUARK == cbs->qparam) { 
      if ((0==row) && (0==col)) {
        if ((0 <= table->prev_phys_traverse_row) && 
            (0 <= table->prev_phys_traverse_col)) {
          cbs->qparam = QRight;
          row = table->prev_phys_traverse_row;
          col = table->prev_phys_traverse_col;
          if (0 > row) row = 0;
          if (0 > col) col = 0;
        }
      }
   }

   /* process right-moving traversals */
   if (QRight == cbs->qparam) {
      int next_row, next_col;

      /* Don't do a thing unless we verify that the row and column
       * are in bounds. Ordinarily, they are always in bounds, except 
       * in an unusual, arguably buggy situation: If the table has 
       * been recently resized smaller, then the Xbae code might report
       * a traverse out of a cell that was in the larger array, but not
       * in the smaller array.  This is probably an Xbae bug. It 
       * will core dump array access.
       */
      if ((row >= table->num_phys_rows) || 
          (col >= table->num_phys_cols)) {

assert (0);
         table->prev_phys_traverse_row = cbs->next_row;
         table->prev_phys_traverse_col = cbs->next_column;
         return;
      }

      /* compute the cell location */
      rel_row = table->locators[row][col]->phys_row_offset;
      rel_col = table->locators[row][col]->phys_col_offset;

      next_row = arr->right_traverse_r[rel_row][rel_col];
      next_col = arr->right_traverse_c[rel_row][rel_col];

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

   /* 
xxxxxxxxxxx 
hack alert -- 
this may work,. but document it ...
*/
   if (table->traverse) {
      int nr = cbs->next_row;
      int nc = cbs->next_column;
      table->reverify_phys_row = nr;
      table->reverify_phys_col = nc;
      (table->traverse) (table, &nr, &nc, table->client_data);
      cbs->next_row = nr;
      cbs->next_column = nc;
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
   int num_header_rows = 0;

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
   reg = XtVaCreateWidget( name,
                  xbaeMatrixWidgetClass,  parent,
                  XmNcells,               table->entries,
                  XmNfixedRows,           num_header_rows,
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

   return (reg);
}

/* ==================================================== */
/* if any of the cells have GUI specific components that need 
 * initialization, initialize them now. The realize() callback
 * on the cursor cell is how we inform the cell handler that 
 * now is the time to initialize it's GUI.  */

void
xaccCreateCursor (Table *table, CellBlock *curs) 
{
   int i,j;
   Widget reg;
   if (!curs || !table) return;

   /* if Xbae itself is not yet created, then we can't go here either */
   reg = table->table_widget;
   if (!reg) return;

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


/* ==================================================== */

#define NOOP(x)  /* do nothing */

void
xaccMotifResizeTable (Table * table,
                 int new_phys_rows, int new_phys_cols,
                 int new_virt_rows, int new_virt_cols)

{
   XACC_RESIZE_ARRAY ((table->num_phys_rows),
               (table->num_phys_cols),
               new_phys_rows,
               new_phys_cols,
               (table->bg_hues),
               Pixel,
               1,
               NOOP);

   XACC_RESIZE_ARRAY ((table->num_phys_rows),
               (table->num_phys_cols),
               new_phys_rows,
               new_phys_cols,
               (table->fg_hues),
               Pixel,
               0,
               NOOP);
}

/* ==================================================== */

static void
SetupColorTable (Table *table)
{

   Display * dpy;
   Window win;
   XWindowAttributes wattr;
   Colormap cmap;
   XColor * colors;
   int i, ncolors;

   if (!(table->table_widget)) return;

   /* if already initialized, do nothing */
   if (0 != table->ncolors) return;

   /* get the number of colors in our colormap */
   dpy = XtDisplay (table->table_widget);
   win = XtWindow (table->table_widget);
   XGetWindowAttributes (dpy, win, &wattr);
   ncolors = wattr.visual->map_entries;
   cmap = wattr.colormap;
   table->ncolors = ncolors;

   /* If the class is TrueColor, then there is no colormap.
    * Punt for now.
    */
   if (TrueColor == wattr.visual->class) return;

   /* if ncolors is greater than 16K, then we probably
    * have a true-color display, and don't have a colormap.
    * Punt. Hack Alert
    */
   if (16384 < ncolors) return;

   /* get the color values */
   /* hack alert -- remember to free this memory somewhere. */
   colors = (XColor *) malloc ( ncolors * sizeof (XColor));
   table->colors = colors;
   for (i=0; i<ncolors; i++) { colors[i].pixel = i; }
   XQueryColors (dpy, cmap, colors, ncolors);
}

/* ==================================================== */

static Pixel 
GetColormapIndex (Table *table, unsigned int argb)
{
   XColor *colors = table->colors;
   int ncolors = table->ncolors;
   unsigned short r,g,b;
   int i;
   unsigned int metric;
   Pixel idx = 0;

   /* if there's no colormap, then assume True or Direct Color */
   /* hack alert -- will the pixel format be simple argb ??? */
   if (0x0 == colors) return argb;

   r = (argb & 0xff0000) >> 8;
   g = argb & 0xff00;
   b = (argb & 0xff) << 8;

   /* use a manhatten metric to find the closest color */
   metric = 0xffffffff;
   for (i=0; i<ncolors; i++) 
   {
      int pr, pg, pb;
      unsigned int m;
      pr = r - colors[i].red;
      pg = g - colors[i].green;
      pb = b - colors[i].blue;
      if (0 > pr) pr = -pr; 
      if (0 > pg) pg = -pg; 
      if (0 > pb) pb = -pb; 
      m = pr + pg + pb;
      if (m < metric) {
         metric = m;
         idx = colors[i].pixel;
      } 
   }

   /*
    * printf ("Info: GetColormapIndex(): \n"
    *   "\tRequested rgb=0x%x r=0x%x g=0x%x b=0x%x \n"
    *   "\tfound idx=%d 0x%x 0x%x 0x%x\n", argb, r, g, b, idx, 
    *   colors[idx].red, colors[idx].green, colors[idx].blue);
    */

   return idx;
}

/* ==================================================== */

static void        
RefreshColors (Table * table, int from_row, int to_row, int from_col, int to_col)
{
   int iphys, jphys;
   uint32 bg_cache, fg_cache;
   Pixel bg_cache_val, fg_cache_val;
   Pixel white, black;

   /* make sure that the color table is initialized.
    * it would be slightly more efficient if we called 
    * this from a realize method, but we don't have one 
    * of these handy.
    */ 
   SetupColorTable (table);

   /* hack alert -- try to store these values with the table, 
    * for cpu efficiency */
   black = GetColormapIndex (table,  0x0);
   fg_cache = 0x0;
   fg_cache_val = black;

   white = GetColormapIndex (table,  0xffffff);
   bg_cache = 0xffffff;
   bg_cache_val = white;

   for (iphys=from_row; iphys<to_row; iphys++)
   {
      for (jphys=from_col; jphys<to_col; jphys++)
      {
         /* fill in the colormap entry that is the equivalent 
          * of th requested background color */
         if (0xffffff == table->bg_colors[iphys][jphys]) {
            table->bg_hues[iphys][jphys] = white;
         } else
         if (bg_cache == table->bg_colors[iphys][jphys]) {
            table->bg_hues[iphys][jphys] = bg_cache_val;
         } else {
            bg_cache = table->bg_colors[iphys][jphys];
            bg_cache_val = GetColormapIndex (table,  bg_cache);
            table->bg_hues[iphys][jphys] = bg_cache_val;
         }

         /* fill in the colormap entry that is the equivalent 
          * of th requested foreground color */
         if (0xffffff == table->fg_colors[iphys][jphys]) {
            table->fg_hues[iphys][jphys] = white;
         } else
         if (fg_cache == table->fg_colors[iphys][jphys]) {
            table->fg_hues[iphys][jphys] = fg_cache_val;
         } else {
            fg_cache = table->fg_colors[iphys][jphys];
            fg_cache_val = GetColormapIndex (table,  fg_cache);
            table->fg_hues[iphys][jphys] = fg_cache_val;
         }
      }
   }
}

/* ==================================================== */

void        
xaccRefreshTableGUI (Table * table)
{
  if (!table) return;
  if (!(table->table_widget)) return;

{int i;
printf (" refresh numphysrows=%d numphyscols=%d =========================\n", 
table->num_phys_rows,table->num_phys_cols);
for (i=0; i<table->num_phys_rows; i++) {
printf ("cell %d\tcolor: 0x%x\tact:%s\tdescr: %s\tpay: %s\n", i, 
table->bg_colors[i][3], 
table->entries[i][2],
table->entries[i][3],
table->entries[i][5]);
}}

  RefreshColors (table, 0, table->num_phys_rows, 0, table->num_phys_cols);

  XtVaSetValues (table->table_widget, XmNrows,    table->num_phys_rows,
                                      XmNcolumns, table->num_phys_cols,
                                      XmNcells,   table->entries, 
                                      XmNcellBackgrounds, table->bg_hues,
                                      XmNcolors, table->fg_hues,
                                      NULL);

}

/* ==================================================== */

static void        
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

   /* make sure the cached color values are correct */
   RefreshColors (table, from_row, to_row, from_col, to_col);

   /* disable update, so as to avoid unpleasent screen flashing */
   /* Uhh, actually, this doesn't work, as expected ... is Xbae busted?
    * XbaeMatrixDisableRedisplay (table->table_widget);
    */

   /* cycle through, cell by cell, copying our values to the widget */
   for (i=from_row; i<to_row; i++) {
      for (j=from_col; j<to_col; j++) {
          XbaeMatrixSetCell           (table->table_widget, i,j, table->entries[i][j]);
          XbaeMatrixSetCellBackground (table->table_widget, i,j, table->bg_hues[i][j]);
          XbaeMatrixSetCellColor      (table->table_widget, i,j, table->fg_hues[i][j]);
      }
   }

   /* OK, update the window */
   /* Uhh, actually, this doesn't work, as expected ... 
    * If False is used, then not everything gets updated properly,
    * If True is used, then the whole window flashes.
    * So in fact things work best in this enable/disable is left alone.
    * XbaeMatrixEnableRedisplay (table->table_widget, True);
    */
}

/* ==================================================== */

void        
xaccRefreshCursorGUI (Table * table)
{
   doRefreshCursorGUI (table, table->current_cursor,
      table->current_cursor_phys_row,
      table->current_cursor_phys_col);
}

/* ================== end of file ======================= */
