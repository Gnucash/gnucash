
#include <stdlib.h>
#include <string.h>

#include "basiccell.h"

BasicCell * xaccMallocBasicCell (void)
{
   BasicCell * cell;
   cell = (BasicCell *) malloc (sizeof (BasicCell));
   xaccInitBasicCell (cell);
   return cell;
}

void xaccInitBasicCell (BasicCell *cell)
{
   cell->type = 0;
   cell->row = 0;
   cell->col = 0;
   cell->width = 0;
   cell->alignment = 0;
   cell->value = 0x0;
   cell->enter_cell = NULL;
   cell->modify_verify = NULL;
   cell->leave_cell = NULL;
   cell->block = NULL;
}

void xaccSetBasicCellValue (BasicCell *cell, char *val)
{

   if (cell->value) free (cell->value);
   cell->value = strdup (val);

}

/* ------------------ end of file ---------------------- */
