
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
} Table;


Table     * xaccMallocTable (int numentries);
void        xaccInitTable (Table *, int entries);
void        xaccDestroyTable (Table *);

void        xaccRefreshTable (Table *);

/* add a cell to the array */
void        xaccSetCursor (Table *, CellArray *);

#endif __XACC_TABLE_H__
/* ================== end of file ======================= */
