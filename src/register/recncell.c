
#include <string.h>
#include <time.h>

#include "recncell.h"
#include "single.h"

/* hack alert -- temp defs should include Transaction.h */
#define NREC 'n'
#define CREC 'y'

/* ================================================ */

static const char * 
ToggleRecn (struct _SingleCell *_cell, const char *cur_val)
{
   char buff[2];

   if (NREC == cur_val[0]) {
     buff[0] = CREC;
   } else {
     buff[0] = NREC;
   }
   buff[1] = 0x0;
   
   return strdup (buff);
}

/* ================================================ */

SingleCell *
xaccMallocRecnCell (void)
{
   SingleCell *cell;
   cell = xaccMallocSingleCell();
   xaccInitRecnCell (cell);
   return cell;
}

/* ================================================ */

void
xaccInitRecnCell (SingleCell *cell)
{
   char buff[2];

   buff[0] = NREC;
   buff[1] = 0x0;

   if (cell->value) free (cell->value);
   cell ->value = strdup (buff);

   cell->enter_cell = ToggleRecn;
}

/* --------------- end of file ---------------------- */
