
#ifndef __XACC_TABLE_H__
#define __XACC_TABLE_H__

#include <Xm/Xm.h>
#include "cell.h"
#include "single.h"

/* the table is the complete, displayed table. */
/* It consists of a header, followed by a simple 
 * list of repeated entries 
 */

typedef struct _Table {

  short numEntries;

  CellArray *header;
  CellArray *cursor;

  char ***entries;

  Widget reg;          /* the XbaeMatrix */

  /* private data, cahces, etc. */
  int num_phys_rows;
  int num_phys_cols;
} CellArray;


Table     * xaccMallocTable (int numentries);
void        xaccInitTable (Table *, int entries);
void        xaccDestroyTable (Table *);

void        xaccRefreshTable (Table *);

/* add a cell to the array */
void        xaccSetCursor (Table *, CellArray *);

#endif __XACC_TABLE_H__

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
   int num_phys_rows;
   int num_phys_cols;
   int i,j;

   /* delete old entries */
   if (table->entries) {
   }

   table->numEntries = numentries;

   /* compute number of physical rows */
   num_phys_rows = 0;
   if (header) {
      num_phys_rows += header->numRows;
   }
   if (cursor) {
      num_phys_rows += numentries* cursor->numRows;
      num_phys_cols = cursor->numCols;
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
xaccRefreshTable (Table * table)
{

  XtVaSetValues (table->reg, XmNcells, table->entires, NULL);
}

/* ================== end of file ======================= */
