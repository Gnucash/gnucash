
#include <string.h>
#include <time.h>

#include "datecell.h"
#include "single.h"

#define DATE_SEP '/'

/* ================================================ */

static const char * 
DateEnter (const char * curr)
{
   short day, month, year;
   char * datestr;
   char * sep;

   /* OK, we just entered a newval cell.  Find out
    * what date that cell thinks it has. 
    */
   day = 1;
   month = 1;
   year = 1970;

printf ("curr is %p \n", curr);
printf ("curr val is %s \n", curr);
   sscanf (curr, "%d/%d/%d", day, month, year);

printf ("enter parsed %d %d %d \n", day, month, year);

   return curr;
}

/* ================================================ */

static const char * 
DateMV (const char * oldval, const char *change, const char *newval)
{
   int accel=0;
   short day, month, year;
   char * datestr;
   char * sep;

   /* if user hit backspace, accept the change */
   if (!change) return newval;

   /* accept any numeric input */
   if (isdigit (change[0])) return newval;

   /* accept the separator character */
   if (DATE_SEP == change[0]) return newval;

   /* otherwise, maybe its an accelerator key.
    * parse the date string */
   day = 1;
   month = 1;
   year = 1970;

   sscanf (newval, "%d/%d/%d", day, month, year);

printf ("parsed %d %d %d \n", day, month, year);

   /* handle accelerator keys */
   switch (change[0]) {
      case '+':
      case '=':
         accel = 1;
         break;

      default:
         /* accept any numeric input */
         if (isdigit (change[0])) return newval;
   }

   if (accel) {
   }

   return newval;
}

/* ================================================ */

static const char * 
DateLeave (const char * curr)
{
   short day, month, year;
   char * datestr;
   char * sep;

   /* otherwise, maybe its an accelerator key.
    * parse the date string */
   day = 1;
   month = 1;
   year = 1970;

   sscanf (curr, "%d/%d/%d", day, month, year);

printf ("leave parsed %d %d %d \n", day, month, year);


   return curr;
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
  time_t secs;
  struct tm *now;
  char buff[30];

  time (&secs);
  now = localtime (&secs);
  sprintf (buff, "%d/%d/%d", now->tm_mday, now->tm_mon+1, now->tm_year+1900);

  if (cell->value) free (cell->value);
  cell ->value = strdup (buff);

  cell ->enter_cell = DateEnter;
  cell ->modify_verify = DateMV;
}

/* --------------- end of file ---------------------- */
