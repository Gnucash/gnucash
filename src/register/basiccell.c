
#include <stdlib.h>
#include <string.h>

#include "single.h"

SingleCell * xaccMallocSingleCell (void)
{
   SingleCell * cell;
   cell = (SingleCell *) malloc (sizeof (SingleCell));
   xaccInitSingleCell (cell);
   return cell;
}

void xaccInitSingleCell (SingleCell *cell)
{
   cell->type = 0;
   cell->row = 0;
   cell->col = 0;
   cell->width = 0;
   cell->alignment = 0;
   cell->value = 0x0;
   cell->modify_verify = NULL;
   cell->extdata = NULL;
   cell->block = NULL;
}

void xaccSetSingleCellValue (SingleCell *cell, char *val)
{

   if (cell->value) free (cell->value);
   cell->value = strdup (val);

}

/* ------------------ end of file ---------------------- */
