
#include <stdlib.h>

#include <Xbae/Matrix.h>

#include "cell.h"
#include "table.h"

static void enterCB (Widget mw, XtPointer cd, XtPointer cb);
static void leaveCB (Widget mw, XtPointer cd, XtPointer cb);
static void modifyCB (Widget mw, XtPointer cd, XtPointer cb);
static void traverseCB (Widget mw, XtPointer cd, XtPointer cb);

Table * 
xaccMallocTable (int tile_rows, int tile_cols)
{
   Table *table;
   table = (Table *) malloc (sizeof (Table));
   table->header = NULL;
   table->cursor = NULL;
   table->entries = NULL;
   xaccInitTable (table, tile_rows, tile_cols);
   return table;
}

/* ==================================================== */

void 
xaccInitTable (Table * table, int tile_rows, int tile_cols)
{
   int num_header_rows;
   int num_phys_rows;
   int num_phys_cols;
   int i,j;
   
   /* delete old entries */
   num_phys_rows = table->num_phys_rows;
   num_phys_cols = table->num_phys_cols;
   if (table->entries) {
      for (i=0; i<num_phys_rows; i++) {
         if (table->entries[i]) {
            for (j=0; j<num_phys_cols; j++) {
               free (table->entries[i][j]);
            }
            free (table->entries[i]);
         }
      }
      free (table->entries);
   }

   /* compute number of physical rows */
   num_header_rows = 0;
   num_phys_rows = 0;
   num_phys_cols = 0;
   if (table->header) {
      num_header_rows = table->header->numRows;
      num_phys_rows += table->header->numRows;
   }
   if (table->cursor) {
      num_phys_rows += tile_rows * table->cursor->numRows;
      num_phys_cols  = tile_cols * table->cursor->numCols;
   }
   table->num_phys_rows = num_phys_rows;
   table->num_phys_cols = num_phys_cols;
   table->num_header_rows = num_header_rows;

   table->num_tile_rows = tile_rows;
   table->num_tile_cols = tile_cols;

   /* create an empty table */
   table->entries = (char ***) malloc (num_phys_rows * sizeof (char **));
   for (i=0; i<num_phys_rows; i++) {
      table->entries[i] = (char **) malloc (num_phys_cols * sizeof (char *));
      for (j=0; j<num_phys_cols; j++) {
         /* the Xbae matrix hates null cell values, so lets
          * accomodate it by letting empty cells have empty 
          * strings */
         table->entries[i][j] = strdup ("");
      }
   }
}

/* ==================================================== */

void
xaccSetCursor (Table *table, CellBlock *curs)
{
   table->cursor = curs;

   /* set back-pointer to table */
   curs->table = (struct _Table *) table;
}

/* ==================================================== */

void xaccSetTableValue (Table *table, char * val)
{
}

/* ==================================================== */
/* hack alert -- will core dump if numrows has changed, etc. */

static
void
xaccRefreshHeader (Table *table)
{
   int i,j;
   CellBlock *arr;

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
      if (! (arr->cells[rel_row][rel_col])) invalid = TRUE;
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
         case XbaeTraverseCellReason: {
            XbaeMatrixTraverseCellCallbackStruct *tcbs;
            tcbs = (XbaeMatrixTraverseCellCallbackStruct *) cbs;
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
      case XbaeTraverseCellReason: {
         traverseCB (mw, cd, cb);
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
   const char * (*enter) (const char *);

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
      const char *val, *retval;

      val = table->entries[row][col];
      retval = enter (val);
      if (val != retval) {
         if (table->entries[row][col]) free (table->entries[row][col]);
         table->entries[row][col] = (char *) retval;
         XbaeMatrixSetCell (mw, row, col, (char *) retval);
         XbaeMatrixRefreshCell (mw, row, col);

         /* don't map a text widget */
         cbs->map = False;
         cbs->doit = False;
      }
   }
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
   const char * (*mv) (const char *, const char *, const char *);
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
      retval = (*mv) (oldval, change, newval);

      /* if the callback returned a non-null value, allow the edit */
      if (retval) {

         /* update data. bounds check done earlier */
         free (table->entries[row][col]);
         table->entries[row][col] = (char *) retval;

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
      table->entries[row][col] = strdup (newval);
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
   const char * (*leave) (const char *);

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

   /* by default, accept whateve the final roposed edit is */
   cbs->doit = True;

   /* OK, if there is a callback for this cell, call it */
   leave = arr->cells[rel_row][rel_col]->leave_cell;
   if (leave) {
      const char *val, *retval;

      val = cbs->value;
      retval = leave (val);

      if (NULL == retval) retval = "";

      /* save whatever was returned */
      if (table->entries[row][col]) free (table->entries[row][col]);
      table->entries[row][col] = (char *) retval;
      cbs->value = strdup (retval);
   } else {
      if (table->entries[row][col]) free (table->entries[row][col]);
      table->entries[row][col] = strdup (cbs->value);
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

   /* compute the cell location */
   row = cbs->row;
   col = cbs->column;
   rel_row = row - table->num_header_rows;
   rel_col = col;
   rel_row %= (arr->numRows);
   rel_col %= (arr->numCols);

   printf ("traverse %d %d \n", row, col);

}


/* ==================================================== */

Widget
xaccCreateTable (Table *table, Widget parent, char * name) 
{
   unsigned char * alignments;
   short * widths;
   Widget reg;

   if (!table) return 0;

   /* make sure that the table is consistent */
   xaccInitTable (table, table->num_tile_rows, table->num_tile_cols);

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
                  XmNtraverseFixedCells,  False,
                  XmNgridType,            XmGRID_SHADOW_IN,
                  XmNshadowType,          XmSHADOW_ETCHED_IN,
                  XmNverticalScrollBarDisplayPolicy,XmDISPLAY_STATIC,
                  XmNselectScrollVisible, True,
                  XmNnavigationType,      XmEXCLUSIVE_TAB_GROUP,  
                  NULL);
    
   XtManageChild (reg);

   XtAddCallback (reg, XmNenterCellCallback, cellCB, (XtPointer)table);
   XtAddCallback (reg, XmNleaveCellCallback, cellCB, (XtPointer)table);
   XtAddCallback (reg, XmNmodifyVerifyCallback, cellCB, (XtPointer)table);
   XtAddCallback (reg, XmNtraverseCellCallback, cellCB, (XtPointer)table);

   table->reg = reg;
   return (reg);
}

/* ==================================================== */

void        
xaccRefreshTable (Table * table)
{
  XtVaSetValues (table->reg, XmNcells, table->entries, NULL);
}

/* ================== end of file ======================= */
