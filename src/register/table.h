
#ifndef __XACC_TABLE_H__
#define __XACC_TABLE_H__

#include <Xm/Xm.h>
#include "basiccell.h"
#include "cellblock.h"

/* the table is the complete, displayed table. */
/* It consists of a header, followed by a simple 
 * list of repeated entries 
 */

typedef struct _Table {

  int num_tile_rows;
  int num_tile_cols;

  CellBlock *header;
  CellBlock *cursor;

  char ***entries;

  Widget reg;          /* the XbaeMatrix */

  /* private data, caches, etc. */

  /* the "physical" rows/cols are equal to
   * the size of the tile times the number 
   * of tile rows/cols
   */

  int num_header_rows;
  int num_phys_rows;
  int num_phys_cols;
} Table;


Table     * xaccMallocTable (int tile_rows, int tile_cols);
void        xaccInitTable (Table *, int tile_rows, int tile_cols);

/* create the widget */
Widget      xaccCreateTable (Table *, Widget parent, char * name);

void        xaccDestroyTable (Table *);

/* redraw the table */
void        xaccRefreshTable (Table *);

/* add a cell to the array */
void        xaccSetCursor (Table *, CellBlock *);

void        xaccSetTableValue (Table *, char *);

#endif __XACC_TABLE_H__
/* ================== end of file ======================= */
