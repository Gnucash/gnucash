
#include <string.h>

#include "basiccell.h"
#include "textcell.h"

/* ================================================ */
/* by definition, all text is valid text.  So accept
 * all modifications */

static const char * 
TextMV (struct _BasicCell *_cell,
        const char *oldval, 
        const char *change, 
        const char *newval)
{
   BasicCell *cell = (BasicCell *) _cell;

   xaccSetBasicCellValue (cell, newval);
   return newval;
}

/* ================================================ */

BasicCell *
xaccMallocTextCell (void)
{
   BasicCell *cell;
   cell = xaccMallocBasicCell();
   xaccInitTextCell (cell);
   return cell;
}

/* ================================================ */

void
xaccInitTextCell (BasicCell *cell)
{
   xaccInitBasicCell (cell);

   if (cell->value) free (cell->value);
   cell->value = strdup ("");

   cell->modify_verify = TextMV;
}

/* --------------- end of file ---------------------- */
