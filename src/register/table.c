
#include <stdlib.h>

#include <Xbae/Matrix.h>

#include "table.h"

Table * 
xaccMallocTable (int numentries)
{
   Table *table;
   table = (Table *) malloc (sizeof (Table));
   table->header = NULL;
   table->cursor = NULL;
   table->entries = NULL;
   xaccInitTable (table, numentries);
   return table;
}

/* ==================================================== */

void 
xaccInitTable (Table * table, int numentries)
{
   int num_header_rows;
   int num_phys_rows;
   int num_phys_cols;
   int i,j;

   /* delete old entries */
   if (table->entries) {
   }

   table->numEntries = numentries;

   /* compute number of physical rows */
   num_header_rows = 0;
   num_phys_rows = 0;
   num_phys_cols = 0;
   if (table->header) {
      num_header_rows = table->header->numRows;
      num_phys_rows += table->header->numRows;
   }
   if (table->cursor) {
      num_phys_rows += numentries* table->cursor->numRows;
      num_phys_cols = table->cursor->numCols;
   }
   table->num_phys_rows = num_phys_rows;
   table->num_phys_cols = num_phys_cols;

   /* create an empty table */
   table->entries = (char ***) malloc (num_phys_rows * sizeof (char **));
   for (i=0; i<num_phys_rows; i++) {
      table->entries[i] = (char **) malloc (num_phys_cols * sizeof (char *));
      for (j=0; j<num_phys_cols; j++) {
         table->entries[i][j] = strdup ("");
      }
   }
}


/* ==================================================== */

void
xaccCreateTable (Table *table, Widget parent, char * name) 
{
   unsigned char * alignments;
   short * widths;

   if (!table) return;

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

   table->reg = XtVaCreateWidget( name,
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
    
}

/* ==================================================== */

void        
xaccRefreshTable (Table * table)
{
  XtVaSetValues (table->reg, XmNcells, table->entries, NULL);
}

/* ================== end of file ======================= */
