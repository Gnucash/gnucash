
#include "price.h"
#include "table.h"

main () {

   Table * table;
   CellBlock *curs, *header;
   SingleCell *cell;

   curs = xaccMallocCellBlock (2, 10);
   header = xaccMallocCellBlock (1, 10);

   cell = xaccMallocPriceCell();
   cell->row = 1;
   cell->col = 3;
   xaccAddCell (curs, cell);
   

   table =  xaccMallocTable (15);
   table -> cursor = curs;


}
