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
#include "util.h"

static void enterCB (Widget mw, XtPointer cd, XtPointer cb);
static void leaveCB (Widget mw, XtPointer cd, XtPointer cb);
static void modifyCB (Widget mw, XtPointer cd, XtPointer cb);
static void traverseCB (Widget mw, XtPointer cd, XtPointer cb);

/* The XrmQuarks are used to figure out the direction of
 * traversal from cell to cell */

static XrmQuark QPointer, QLeft, QRight, QUp, QDown;
static Boolean haveQuarks = False;

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_REGISTER;

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
   int invalid = 0;

   table = (Table *) cd;
   cbs = (XbaeMatrixDefaultActionCallbackStruct *) cb;

   row = cbs->row;
   col = cbs->column;

   /* If we are entering this cell, make sure that we've moved the
    * cursor, and that any subsidiary GUI elements are properly
    * positioned.  Do this *before* we examine the value of the
    * "current cursor".  Note that this check could suck up massive
    * cpu, as potentially it may trigger a redraw of the entire
    * register.
    *
    * Hmmm ... actually, theoretically, this is not neeeded.  Before
    * we get to here, we *should* have gone through a traverse
    * callback the determine which cell to enter, and then, after the
    * leavecell, the cursor should have been automagically
    * repositioned.  Thus, the call below should always no-op out.
    * However, if/when things go berzerk, the call below does at least
    * a partial butt-save for us, so lets leave it in for now.  */

   if (XbaeEnterCellReason == cbs->reason) 
   {
     wrapVerifyCursorPosition (table, row, col);
   }

   invalid = ! gnc_register_cell_valid(table, row, col);

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
enterCB (Widget mw, XtPointer cd, XtPointer cb) {

  Table *table = (Table *) cd;
   XbaeMatrixEnterCellCallbackStruct *cbs =
     (XbaeMatrixEnterCellCallbackStruct *) cb;
   const int row = cbs->row;
   const int col = cbs->column;
   char *new_text = NULL;

   gnc_table_enter_update(table, row, col, &new_text);
   
   if(new_text) {
      XbaeMatrixSetCell (mw, row, col, new_text);
      XbaeMatrixRefreshCell (mw, row, col);
      
      /* don't map a text widget */
      cbs->map = False;
      cbs->doit = False;
   } else {
     cbs->doit = True;
     cbs->map = True;
   }
}
   
/* ==================================================== */
/* this routine calls the individual cell callbacks */

static void
modifyCB (Widget mw, XtPointer cd, XtPointer cb)
{
   Table *table = (Table *) cd;
   XbaeMatrixModifyVerifyCallbackStruct *cbs =
     (XbaeMatrixModifyVerifyCallbackStruct *) cb;

   const int row = cbs->row;
   const int col = cbs->column;
   const char *oldval = cbs->prev_text;
   const char *change = cbs->verify->text->ptr;
   int len = 1;
   const char *retval;
   char *newval;
   
   /* accept edits by default, unless the cell handler rejects them */
   cbs->verify->doit = True;

   /* first, compute the newval string */
   if (oldval) len += strlen (oldval);
   if (change) len += strlen (change);
   newval = (char *) malloc (len);

   /* text may be inserted, or deleted, or replaced ... */
   newval[0] = 0;
   strncat (newval, oldval, cbs->verify->startPos);
   if (change) strcat (newval, change);
   strcat (newval, &oldval[(cbs->verify->endPos)]);

   retval = gnc_table_modify_update(table, row, col, oldval, change, newval);

   if (retval && (retval != newval)) {
     XbaeMatrixSetCell (mw, row, col, (char *) retval);
     XbaeMatrixRefreshCell (mw, row, col);
     XbaeMatrixSetCursorPosition (mw, (cbs->verify->endPos) + 1);
     
     /* the default update has already been overridden,
      * so don't allow Xbae to update */
     cbs->verify->doit = False;

     free (newval);
   } else if(!retval) {
     cbs->verify->doit = False;
     free(newval);
   }
   PINFO("modifyCB(): exit\n");
}

/* ==================================================== */
   
static void
leaveCB (Widget mw, XtPointer cd, XtPointer cb)
{
  Table *table = (Table *) cd;
   XbaeMatrixLeaveCellCallbackStruct *cbs =
     (XbaeMatrixLeaveCellCallbackStruct *) cb;
   const int row = cbs->row;
   const int col = cbs->column;
   char * new_text;

   gnc_table_leave_update(table, row, col, cbs->value, &new_text);

   if(new_text) {
     cbs->value = new_text;
   }

   /* by default, accept whatever the final proposed edit is */
   cbs->doit = True;
}


/* ==================================================== */

static void
traverseCB (Widget mw, XtPointer cd, XtPointer cb)
{
  Table *table = (Table *) cd;
  XbaeMatrixTraverseCellCallbackStruct *cbs =
    (XbaeMatrixTraverseCellCallbackStruct *) cb;
  int row = cbs->row;
  int col = cbs->column;
  
  gncTableTraversalDir dir;

  if(cbs->qparam == QRight) dir = GNC_TABLE_TRAVERSE_RIGHT;
  else if(cbs->qparam == QLeft) dir = GNC_TABLE_TRAVERSE_LEFT;
  else if(cbs->qparam == QUp) dir = GNC_TABLE_TRAVERSE_UP;
  else if(cbs->qparam == QDown) dir = GNC_TABLE_TRAVERSE_DOWN;
  else if(cbs->qparam == QPointer) dir = GNC_TABLE_TRAVERSE_POINTER;
  else if(cbs->qparam == NULLQUARK) {
    /* If the qparm is NULL, then it is likely that we are here
       because we traversed out of a cell that had a ComboBox in it.
       The ComboCell is clever enough to put us back into the register
       after tabing out of it.  However, its not (cannot be) clever
       enough to pretend that it was a tab group in the register.
       Thus, we will emulate that we left a tab group in the register
       to get here.  To put it more simply, we just set the row and
       column to that of the ComboCell, which we had previously
       recorded, and continue on as if nothing happened.
       
       BTW -- note that we are emulating a normal, right-moving tab.
       Backwards tabs are broken.
       
       Note that the quark may also be zero if we are entering the
       register for the first time after a table resize.  */
      
    dir = GNC_TABLE_TRAVERSE_RIGHT;
      
    if ((0==row) && (0==col)) {
      if ((0 <= table->prev_phys_traverse_row) && 
          (0 <= table->prev_phys_traverse_col)) {
        row = table->prev_phys_traverse_row;
        col = table->prev_phys_traverse_col;
        if (0 > row) row = 0;
        if (0 > col) col = 0;
      }
    }
  } else {
    PERR("SERIOUS: Completely invalid traversal direction. (%s)\n",
         __FUNCTION__);
    return;
  }
  
  {
    int exit_register = gnc_table_traverse_update(table, row, col,
                                                  dir,
                                                  &(cbs->next_row),
                                                  &(cbs->next_column));
    if(exit_register) {
      cbs->qparam = NULLQUARK; 
      if (table->next_tab_group) {
        XmProcessTraversal (table->next_tab_group, XmTRAVERSE_CURRENT);
      }
    }
  }
}

/* ==================================================== */

Widget
xaccCreateTable (Table *table, Widget parent, char * name) 
{
   CellBlock *curs;
   unsigned char * alignments;
   short * widths;
   Widget reg, textw;
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

#if (!defined (LesstifVersion)) || (87 < LesstifVersion)
   /* Coredump when using lesstif 0.80 or earlier */
   /* stop it from beeping */
   XtVaGetValues (reg, XmNtextField, &textw, NULL);
   XtVaSetValues (textw, XmNverifyBell, False, NULL);
#endif /* LESSTIF_VERSION */

   return (reg);
}

int
gnc_table_column_width(Table *table, const int col) {
  Widget reg = table->table_widget;
  return(XbaeMatrixGetColumnPixelWidth (reg, col));
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

  RefreshColors (table, 0, table->num_phys_rows, 0, table->num_phys_cols);

  XtVaSetValues (table->table_widget, XmNrows,    table->num_phys_rows,
                                      XmNcolumns, table->num_phys_cols,
                                      XmNcells,   table->entries, 
                                      XmNcellBackgrounds, table->bg_hues,
                                      XmNcolors, table->fg_hues,
                                      NULL);

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

/* ================== end of file ======================= */
