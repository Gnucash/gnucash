/*
 * FILE:
 * datecell.c
 *
 * FUNCTION:
 * implements a gui-independent date handling cell.
 *
 * HISTORY:
 * Copyright (C) 1997 Robin D. Clark
 * Copyright (c) 1998 Linas Vepstas
 */
/********************************************************************\
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "basiccell.h"
#include "datecell.h"

static void setDateCellValue (struct _BasicCell *, const char *);

#define SET(cell,str) { 			\
   if ((cell)->value) free ((cell)->value);	\
   (cell)->value = strdup (str);		\
}

#define DATE_SEP '/'

/* ================================================ */

static void 
prtDate (char * buff, int day, int month, int year)
{
   sprintf (buff, "%2d/%2d/%4d", month, day, year);
}

/* ================================================ */

static
void xaccParseDate (struct tm *parsed, const char * datestr)
{
   char *dupe, *tmp, *first_field, *second_field, *third_field;
   int iday, imonth, iyear;
   time_t secs;
   struct tm *now;

   dupe = strdup (datestr);
   tmp = dupe;
   first_field = 0x0;
   second_field = 0x0;
   third_field = 0x0;

   /* use strtok to find delimiters */
   if (tmp) {
      first_field = strtok (tmp, ".,-+/\\()");
      if (first_field) {
         second_field = strtok (NULL, ".,-+/\\()");
         if (second_field) {
            third_field = strtok (NULL, ".,-+/\\()");
         }
      }
   }
   
   /* if any fields appear blank, use today's date */
   time (&secs);
   now = localtime (&secs);
   iday = now->tm_mday; 
   imonth = now->tm_mon+1;
   iyear = now->tm_year+1900;

#ifdef _PARSE_DATE_AS_DD_MM_YY
   /* get numeric values */
   if (first_field) iday = atoi (first_field);
   if (second_field) imonth = atoi (second_field);
   if (third_field) iyear = atoi (third_field);
#endif /* _PARSE_DATE_AS_DD_MM_YY */

#define _PARSE_DATE_AS_MM_DD_YY
#ifdef _PARSE_DATE_AS_MM_DD_YY
   /* get numeric values */
   if (first_field) imonth = atoi (first_field);
   if (second_field) iday = atoi (second_field);
   if (third_field) iyear = atoi (third_field);
#endif /* _PARSE_DATE_AS_MM_DD_YY */

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

   prtDate (buff, date->tm_mday, date->tm_mon+1, date->tm_year+1900);

   if (cell->cell.value) free (cell->cell.value);
   cell->cell.value = strdup (buff);

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

   prtDate (buff, cell->date.tm_mday, 
                  cell->date.tm_mon+1, 
                  cell->date.tm_year+1900);

   if (cell->cell.value) free (cell->cell.value);
   cell->cell.value = strdup (buff);

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
   prtDate (buff, now->tm_mday, now->tm_mon+1, now->tm_year+1900);
 
   SET (&(cell->cell), buff);
 
   cell->cell.enter_cell = DateEnter;
   cell->cell.modify_verify = DateMV;
   cell->cell.leave_cell = DateLeave;
   cell->cell.set_value = setDateCellValue;
}

/* ================================================ */

void
xaccDestroyDateCell (DateCell *cell)
{
   cell->date.tm_mday = 0;
   cell->date.tm_mon = 0;
   cell->date.tm_year = 0;
   xaccDestroyBasicCell ( &(cell->cell));
}

/* ================================================ */

void 
xaccSetDateCellValue (DateCell *cell, int day, int mon, int year)
{
   struct tm dada;
   char buff[30];

   dada.tm_mday = day;
   dada.tm_mon = mon-1;
   dada.tm_year = year - 1900;

   xaccValidateDate (&dada, 0);
   cell->date.tm_mday = dada.tm_mday;
   cell->date.tm_mon = dada.tm_mon;
   cell->date.tm_year = dada.tm_year;

   prtDate (buff, dada.tm_mday, dada.tm_mon+1, dada.tm_year+1900);

   if (cell->cell.value) free (cell->cell.value);
   cell->cell.value = strdup (buff);
}

/* ================================================ */

static void 
setDateCellValue (struct _BasicCell *_cell, const char *str)
{
   DateCell *cell = (DateCell *) _cell;
   char buff[30];

   xaccParseDate (&(cell->date), str);

   prtDate (buff, cell->date.tm_mday, 
                  cell->date.tm_mon+1, 
                  cell->date.tm_year+1900);

   if (cell->cell.value) free (cell->cell.value);
   cell->cell.value = strdup (buff);
}

/* ============== END OF FILE ===================== */
