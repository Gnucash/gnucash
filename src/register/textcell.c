
#include <string.h>

#include "textcell.h"
#include "single.h"

/* ================================================ */
/* by definition, all text is valid text.  So accept
 * all modifications */

static const char * 
TextMV (const char * old, const char *change, const char *new)
{
   return new;
}

/* ================================================ */

SingleCell *
xaccMallocTextCell (void)
{
   SingleCell *cell;
   cell = xaccMallocSingleCell();
   xaccInitTextCell (cell);
   return cell;
}

/* ================================================ */

void
xaccInitTextCell (SingleCell *cell)
{
  if (cell->value) free (cell->value);
  cell->value = strdup ("");

  cell->modify_verify = TextMV;
}

/* --------------- end of file ---------------------- */
