
#include <string.h>
#include <time.h>

#include "basiccell.h"
#include "recncell.h"

/* hack alert -- temp defs should include Transaction.h */
#define NREC 'n'
#define CREC 'y'

/* ================================================ */

static const char * 
ToggleRecn (struct _BasicCell *_cell, const char *cur_val)
{
   BasicCell *cell = (BasicCell *) _cell;
   char buff[2];

   if (NREC == cur_val[0]) {
     buff[0] = CREC;
   } else {
     buff[0] = NREC;
   }
   buff[1] = 0x0;
   
   xaccSetBasicCellValue (cell, buff);
   return strdup (buff);
}

/* ================================================ */

BasicCell *
xaccMallocRecnCell (void)
{
   BasicCell *cell;
   cell = xaccMallocBasicCell();
   xaccInitRecnCell (cell);
   return cell;
}

/* ================================================ */

void
xaccInitRecnCell (BasicCell *cell)
{
   char buff[2];

   buff[0] = NREC;
   buff[1] = 0x0;

   if (cell->value) free (cell->value);
   cell ->value = strdup (buff);

   cell->enter_cell = ToggleRecn;
}

/* --------------- end of file ---------------------- */
