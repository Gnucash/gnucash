
#include <string.h>
#include <time.h>

#include "datecell.h"
#include "single.h"

#define DATE_SEP '/'

/* ================================================ */

static
void xaccParseDate (struct tm *parsed, const char * datestr)
{
   char *dupe, *tmp, *day, *month, *year;
   int iday, imonth, iyear;
   time_t secs;
   struct tm *now;

   dupe = strdup (datestr);
   tmp = dupe;
   day = 0x0;
   month = 0x0;
   year = 0x0;

   /* use strtok to find delimiters */
   if (tmp) {
      day = strtok (tmp, ".,-+/\\()");
      if (day) {
         month = strtok (NULL, ".,-+/\\()");
         if (month) {
            year = strtok (NULL, ".,-+/\\()");
         }
      }
   }
   
   /* if any fields appear blank, use today's date */
   time (&secs);
   now = localtime (&secs);
   iday = now->tm_mday; 
   imonth = now->tm_mon+1;
   iyear = now->tm_year+1900;

   /* get numeric values */
   if (day) iday = atoi (day);
   if (month) imonth = atoi (month);
   if (year) iyear = atoi (year);

   /* check to see if day & month are reversed */
   /* only works for some dates */
   if (12 < imonth) {
      int itmp = imonth;
      imonth = iday;
      iday = itmp;
   }

   free (dupe);

   if (parsed) {
      parsed->tm_mday = iday;
      parsed->tm_mon = imonth-1;
      parsed->tm_year = iyear-1900;
   }

   return;
}

/* ================================================ */

static const char * 
DateEnter (const char * curr)
{
   char * sep;
   struct tm celldate;

   /* OK, we just entered a newval cell.  Find out
    * what date that cell thinks it has. 
    */

   xaccParseDate (&celldate, curr);

printf ("parse %d %d %d \n", celldate.tm_mday, celldate.tm_mon+1,
celldate.tm_year+1900);

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

DateCell *
xaccMallocDateCell (void)
{
   DateCell *cell;
   cell = (DateCell *) malloc (sizeof (DateCell));
   xaccInitDateCell (cell);
   return cell;
}

/* ================================================ */

void
xaccInitDateCell (DateCell *cell)
{
  time_t secs;
  struct tm *now;
  char buff[30];

  xaccInitSingleCell (&(cell->cell));

  /* default value is today's date */
  time (&secs);
  now = localtime (&secs);
  cell->date = *now;
  sprintf (buff, "%d/%d/%d", now->tm_mday, now->tm_mon+1, now->tm_year+1900);

  if (cell->cell.value) free (cell->cell.value);
  cell->cell.value = strdup (buff);

  cell->cell.enter_cell = DateEnter;
  cell->cell.modify_verify = DateMV;
}

/* --------------- end of file ---------------------- */
