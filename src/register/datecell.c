
#include <string.h>
#include <time.h>

#include "basiccell.h"
#include "datecell.h"

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
/* february default is 28, and patched below */
static
char days_in_month[12] = { 31,28,31,30,31,30,31,31,30,31,30,31 };

/* ================================================ */

static void
xaccValidateDate (struct tm *date, int recur)
{
   int day, month, year;

   /* avoid infinite recursion */
   if (1 < recur) return;

   day = date->tm_mday;
   month = date->tm_mon + 1;
   year = date->tm_year + 1900;

   /* adjust days in february for leap year */
   if (((year % 4 == 0) && (year % 100 != 0)) || (year % 400 == 0)) {
      days_in_month[1] = 29;
   } else {
      days_in_month[1] = 28;
   }
  
   /* the "% 12" business is because month might not be valid!*/

   while (day > days_in_month[(month+11) % 12]) {
      day -= days_in_month[(month+11) % 12];
      month++;
   }
   while (day < 1) {
      month--;
      day += days_in_month[(month+11) % 12];
   }
   while (month > 12) {
      month -= 12;
      year++;
   }
   while (month < 1) {
      month += 12;
      year--;
   }

   date->tm_mday = day;
   date->tm_mon = month - 1;
   date->tm_year = year - 1900;

   /* do it again, in case leap-year scrolling messed things up */
   xaccValidateDate (date, ++recur);
}


/* ================================================ */

static const char * 
DateEnter (struct _BasicCell *_cell, const char * curr)
{
   DateCell *cell = (DateCell *) _cell;

   /* OK, we just entered a new cell.  Find out
    * what date that cell thinks it has.   */
   xaccParseDate (&(cell->date), curr);

   return curr;
}

/* ================================================ */

static const char * 
DateMV (struct _BasicCell *_cell, 
        const char *oldval, 
        const char *change, 
        const char *newval)
{
   DateCell *cell = (DateCell *) _cell;
   struct tm *date;
   char buff[30];
   char *datestr;
   int accel=0;

   /* if user hit backspace, accept the change */
   if (!change) return newval;

   /* accept any numeric input */
   if (isdigit (change[0])) return newval;

   /* accept the separator character */
   if (DATE_SEP == change[0]) return newval;

   /* otherwise, maybe its an accelerator key. */
   date = &(cell->date);

   /* handle accelerator keys */
   switch (change[0]) {
      case '+':
      case '=':
         /* increment day */
         date->tm_mday ++;
         accel = 1;
         break;

      case '_':
      case '-':
         /* decrement day */
         date->tm_mday --;
         accel = 1;
         break;

      case '}':
      case ']':
         /* increment month */
         date->tm_mon ++;
         accel = 1;
         break;

      case '{':
      case '[':
         /* decrment month */
         date->tm_mon --;
         accel = 1;
         break;

      case 'M':
      case 'm':
         /* begining of month */
         date->tm_mday = 1;
         break;

      case 'H':
      case 'h':
         /* end of month */
         date->tm_mday = days_in_month[date->tm_mon];
         break;

      case 'Y':
      case 'y':
         /* begining of year */
         date->tm_mday = 1;
         date->tm_mon = 0;
         break;

      case 'R':
      case 'r':
         /* end of year */
         date->tm_mday = 31;
         date->tm_mon = 11;
         break;

      case 'T':
      case 't': {
         /* today */
         time_t secs;
         struct tm *now;

         time (&secs);
         now = localtime (&secs);
         *date = *now;
         break;
      }

      default:
         /* reject other changes */
         return NULL;
   }

   if (accel) {
      xaccValidateDate (date, 0);
   }

   sprintf (buff, "%d/%d/%d", date->tm_mday, 
                              date->tm_mon+1, 
                              date->tm_year+1900);

   xaccSetBasicCellValue (&(cell->cell), buff);
   datestr = strdup (buff);
   
   return datestr;
}

/* ================================================ */

static const char * 
DateLeave (struct _BasicCell *_cell, const char * curr)
{
   DateCell *cell = (DateCell *) _cell;
   char buff[30];
   char * retval;

   /* OK, we are leaving the cell.  Find out
    * what date that cell thinks it has.   */
   xaccParseDate (&(cell->date), curr);

   sprintf (buff, "%d/%d/%d", cell->date.tm_mday, 
                              cell->date.tm_mon+1, 
                              cell->date.tm_year+1900);

   retval = strdup (buff);
   return retval;
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

  xaccInitBasicCell (&(cell->cell));

  /* default value is today's date */
  time (&secs);
  now = localtime (&secs);
  cell->date = *now;
  sprintf (buff, "%d/%d/%d", now->tm_mday, now->tm_mon+1, now->tm_year+1900);

  if (cell->cell.value) free (cell->cell.value);
  cell->cell.value = strdup (buff);

  cell->cell.enter_cell = DateEnter;
  cell->cell.modify_verify = DateMV;
  cell->cell.leave_cell = DateLeave;
}

/* --------------- end of file ---------------------- */
