
#include <stdlib.h>

#include <Xbae/Matrix.h>

#include "cell.h"
#include "table.h"

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

static void
enterCB (Widget mw, XtPointer cd, XtPointer cb)
{
   Table *table;
   XbaeMatrixDefaultActionCallbackStruct *cbs;
   int row, col;

   table = (Table *) cd;
   cbs = (XbaeMatrixDefaultActionCallbackStruct *)cb;

   row = cbs->row;
   col = cbs->column;

   if (XbaeEnterCellReason != cbs->reason) return;

   printf ("enter %d %d \n", row, col);
}

/* ==================================================== */
/* this routine calls the individual cell callbacks */

static void
modifyCB (Widget mw, XtPointer cd, XtPointer cb)
{
   Table *table;
   XbaeMatrixModifyVerifyCallbackStruct *cbs;
   int row, col;
   int rel_row, rel_col;
   CellBlock *arr;

   table = (Table *) cd;
   cbs = (XbaeMatrixModifyVerifyCallbackStruct *) cb;

   row = cbs->row;
   col = cbs->column;

   if (XbaeModifyVerifyReason != cbs->reason) return;

   printf ("modify %d %d %s \n", row, col, cbs->verify->text->ptr);

   /* reject edits by default, unless the cell handler allows them */
   cbs->verify->doit = False;

   /* can't edit outside of the physical space */
   if ((0 > row) || (0 > col)) return;
   if ((row >= table->num_phys_rows) || (col >= table->num_phys_cols)) return;

   /* header rows cannot be modified */
   if (row < table->num_header_rows) return;

   /* compute the cell location */
   rel_row = row;
   rel_col = col;

   /* remove offset for the header rows */
   rel_row -= table->num_header_rows;

   /* prepare to call the cell callback */
   arr = table->cursor;
   if (arr) {
      rel_row %= (arr->numRows);
      rel_col %= (arr->numCols);
      if (arr->cells[rel_row][rel_col]) {
         const char * (*mv) (const char *, const char *, const char *);
         mv = arr->cells[rel_row][rel_col]->modify_verify;

         /* OK, if there is a callback for this cell, call it */
         if (mv) {
            const char *old, *change;
            char *new;
            const char *retval;
            int len;

            old = cbs->prev_text;
            change = cbs->verify->text->ptr;

            /* but first, compute the new string */
            len = 1;
            if (old) len += strlen (old);
            if (change) len += strlen (change);
            new = (char *) malloc (len);

            /* text may be inserted, or deleted, or replaced ... */
            new[0] = 0;
            strncat (new, old, cbs->verify->startPos);
            if (change) strcat (new, change);
            strcat (new, &old[(cbs->verify->endPos)]);

            retval = (*mv) (old, change, new);

            /* if the callback returned a non-null value, allow the edit */
            if (retval) {

               /* update data. bounds check done earlier */
               free (table->entries[row][col]);
               table->entries[row][col] = (char *) retval;

               /* if the callback modified the display string,
                * update the display cell as well */
               if (retval != new) {
                  XbaeMatrixSetCell (mw, row, col, (char *) retval);
                  XbaeMatrixRefreshCell (mw, row, col);
                  XbaeMatrixSetCursorPosition (mw, (cbs->verify->endPos) +1);

                  /* the default update has already been overridden,
                   * so don't allow Xbae to update */
                  cbs->verify->doit = False;

                  /* avoid wasting memory */
                  free (new);
               } else {

                  /* the proposed edit was accepted! */
                  cbs->verify->doit = True;
               }
            } else {
               /* avoid wasting memory */
               free(new);
            }
         }
      }
   }
}

/* ==================================================== */

static void
traverseCB (Widget mw, XtPointer cd, XtPointer cb)
{
   Table *table;
   XbaeMatrixDefaultActionCallbackStruct *cbs;
   int row, col;

   table = (Table *) cd;
   cbs = (XbaeMatrixDefaultActionCallbackStruct *)cb;

   row = cbs->row;
   col = cbs->column;

   if (XbaeTraverseCellReason != cbs->reason) return;

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

   XtAddCallback (reg, XmNenterCellCallback, enterCB, (XtPointer)table);
   XtAddCallback (reg, XmNmodifyVerifyCallback, modifyCB, (XtPointer)table);
   XtAddCallback (reg, XmNtraverseCellCallback, traverseCB, (XtPointer)table);

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
