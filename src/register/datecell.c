
#include <string.h>

#include "datecell.h"
#include "single.h"

/* ================================================ */

static const char * 
DateMV (const char * old, const char *change, const char *new)
{
   return new;
}

/* ================================================ */

SingleCell *
xaccMallocDateCell (void)
{
   SingleCell *cell;
   cell = xaccMallocSingleCell();
   xaccInitDateCell (cell);
   return cell;
}

/* ================================================ */

void
xaccInitDateCell (SingleCell *cell)
{
  if (cell->value) free (cell->value);
  cell ->value = strdup ("1/1/70");

  cell ->modify_verify = DateMV;
}

/* --------------- end of file ---------------------- */
