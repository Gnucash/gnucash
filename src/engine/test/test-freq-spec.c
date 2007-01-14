/*
 * Testing routine added by Ben Stanley bds02@uow.edu.au 20010320
 * Try to test Joshua Sled's FreqSpec module.
 *
 */
/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */

#include "config.h"
#include <stdlib.h>
#include <glib.h>
#include "cashobjects.h"
#include "test-stuff.h"
#include "FreqSpec.h"
#include "gnc-engine.h"

static QofBook *book;

static void
test_once (void)
{
   FreqSpec *fs;
   guint32 i, start_julian;
   GDate date1, date2, next_date;

   fs = xaccFreqSpecMalloc(book);

   for( start_julian = 1; start_julian < 1000; ++start_julian ) {
      g_date_set_julian( &date1, start_julian );

      xaccFreqSpecSetOnceDate( fs, &date1 );
      for( i = 0; i <= 2 * start_julian; ++i ) {
         g_date_set_julian( &date2, start_julian + i );
         xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
         do_test( (g_date_compare( &date2, &date1 ) >= 0 &&
               !g_date_valid( &next_date ) ) ||
            g_date_compare( &date1, &next_date ) == 0,
            "once off" );
      }
   }
   fprintf (stdout, " FreqSpec: Single test OK, continuing . . . \r");
   fflush(stdout);
   xaccFreqSpecFree(fs);
}

static void
test_daily (void)
{
   guint32 interval, i, start_julian, j;
   FreqSpec *fs;
   GDate date1, date2, next_date;

   fs = xaccFreqSpecMalloc(book);

   g_date_set_dmy( &date1, 1, 1, 2000 );

   start_julian = g_date_get_julian( &date1 );
   for( interval = 1; interval < 400; ++interval ) {
      xaccFreqSpecSetDaily( fs, &date1, interval );
      for( i = 0; i <= 2 * interval; ++i ) {
         g_date_set_julian( &date2, start_julian + i );
         xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
         do_test_args(
            g_date_get_julian( &next_date ) - g_date_get_julian( &date2 ) ==
               interval - (i%interval),
            "daily repeats",
            __FILE__, __LINE__,
            "interval = %d days, days from start = %d",
            interval, i );
      }
   }

   /* This tests whether we can correctly generate a sequence of dates,
    * and end up in the right place. */
   g_date_set_dmy( &date1, 25, 3, 2001 );
   for( interval = 1; interval < 20; ++interval ) {
      xaccFreqSpecSetDaily( fs, &date1, interval );
      for( j = 0; j < 20; ++j ) { /* j=0 passes by luck, but it's not valid */
         date2 = date1;
         for( i = 0; i < j; ++i ) {
            xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
            date2 = next_date;
         }
         do_test_args( g_date_get_julian( &date2 ) - g_date_get_julian( &date1 ) == interval*j,
            "daily repeats end up in the right place",
            __FILE__, __LINE__, "interval = %d days, iters = %d",
            interval, j );
      }
   }
   fprintf(stdout, " FreqSpec: Daily test OK, continuing . . . \r");
   fflush(stdout);
   xaccFreqSpecFree(fs);
}

static void
test_weekly (void)
{
   guint32 interval, i, start_julian, weekday, j;
   FreqSpec *fs;
   GDate date1, date2, next_date;

   fs = xaccFreqSpecMalloc(book);

   /* Use this to test any specific cases which fail,
    * for easy access in the debugger. */
/*
   g_date_set_dmy( &date1, 2, 1, 1 );
   xaccFreqSpecSetWeekly( fs, &date1, 1 );
   start_julian = g_date_get_julian( &date1 );
   g_date_set_julian( &date2, start_julian + 6 );
   xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
*/
   /* date2 should now be 9/1/1, julian date 9. */

   for( weekday = 1; weekday <= 7; ++weekday ) {
      g_date_set_dmy( &date1, weekday, 1, 1 );
      start_julian = g_date_get_julian( &date1 );
      for( interval = 1; interval <= 52; ++interval ) {
         xaccFreqSpecSetWeekly( fs, &date1, interval );
         for( i = 0; i <= 2 * 7 * interval; ++i ) {
            g_date_set_julian( &date2, start_julian + i );
            xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
            do_test_args(
               g_date_get_julian( &next_date ) - g_date_get_julian( &date2 ) ==
                  interval*7 - (i%(interval*7)),
               "weekly repeats",
               __FILE__, __LINE__,
               "weekday = %d, interval = %d weeks, days from start = %d",
               weekday, interval, i );
         }
      }
   }

   /* This tests whether we can correctly generate a sequence of dates,
    * and end up in the right place. */
   g_date_set_dmy( &date1, 25, 3, 2001 );
   for( interval = 1; interval < 20; ++interval ) {
      xaccFreqSpecSetWeekly( fs, &date1, interval );
      for( j = 0; j < 2*53; ++j ) {
         date2 = date1;
         for( i = 0; i < j; ++i ) {
            xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
            date2 = next_date;
         }
         do_test_args( g_date_get_julian( &date2 ) - g_date_get_julian( &date1 ) == interval*7*j,
            "weekly repeats end up in the right place",
            __FILE__, __LINE__, "interval = %d weeks, iters = %d",
            interval, j );
      }
   }
   fprintf(stdout, " FreqSpec: Weekly test OK, continuing . . . \r");
   fflush(stdout);
   xaccFreqSpecFree(fs);
}

static void
test_monthly (void)
{
   guint32 interval, i, start_julian, monthday, month, j, day_of_year;
   FreqSpec *fs;
   GDate date0, date1, date2, next_date;

   fs = xaccFreqSpecMalloc(book);

   /* Use this to test any specific cases which fail,
    * for easy access in the debugger. */

/*
   g_date_set_dmy( &date1, 1, 1, 1 );
   xaccFreqSpecSetMonthly( fs, &date1, 2 );
   start_julian = g_date_get_julian( &date1 );
   g_date_set_julian( &date2, start_julian + 0 );
   xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
*/

   for( monthday = 1; monthday <= 28; ++monthday ) {
      for( month = 1; month <= 12; ++month ) {
         g_date_set_dmy( &date1, monthday, month, 1 );
         start_julian = g_date_get_julian( &date1 );
         for( interval = 1; interval <= 24; ++interval ) {
            xaccFreqSpecSetMonthly( fs, &date1, interval );
            for( i = 0; i <= 2 * 31 * interval; ++i ) {
               g_date_set_julian( &date2, start_julian + i );
               xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
               do_test_args(
                  g_date_get_day( &next_date ) == g_date_get_day( &date1 ),
                  "monthly repeats - check day",
                  __FILE__, __LINE__,
                  "monthday = %d, month = %d, interval = %d months, days from start = %d",
                  monthday, month, interval, i );
               do_test_args(
                  ( g_date_get_month( &next_date ) + 12 * (g_date_get_year( &next_date )-1) - 1) %
                     interval == (month-1) % interval,
                  "monthly repeats - check month",
                  __FILE__, __LINE__,
                  "monthday = %d, month = %d, interval = %d months, days from start = %d",
                  monthday, month, interval, i );
            }
         }
      }
   }

   /* This tests whether we can correctly generate a sequence of dates,
    * and end up in the right place. */
   g_date_set_dmy( &date0, 1, 1, 2000 );
   for( day_of_year = 1; day_of_year <= 365*5; ++day_of_year ) {
      g_date_set_julian( &date1, g_date_get_julian( &date0 ) + day_of_year-1 );
      for( interval = 1; interval < 20; ++interval ) {
         xaccFreqSpecSetMonthly( fs, &date1, interval );
         for( j = 1; j < 20; ++j ) {
            date2 = date1;
            for( i = 0; i < j; ++i ) {
               xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
               date2 = next_date;
            }
            date2 = date1;
            g_date_add_months( &date2, interval * j );
            do_test_args( g_date_compare( &date2, &next_date ) == 0,
               "monthly repeats end up in the right place",
               __FILE__, __LINE__, "interval = %d months, iters = %d, day_of_year = %d",
               interval, j, day_of_year );
         }
      }
   }
   fprintf(stdout, " FreqSpec: Monthly test OK, continuing . . . \r");
   fflush(stdout);
   xaccFreqSpecFree(fs);
}

static void
test_month_relative (void)
{
   guint32 interval, i, start_julian, monthday, month, j;
   FreqSpec *fs;
   GDate date0, date1, date2, next_date;

   fs = xaccFreqSpecMalloc(book);

   /* Use this to test any specific cases which fail,
    * for easy access in the debugger. */

/*
   g_date_set_dmy( &date1, 1, 1, 1 );
   xaccFreqSpecSetMonthRelative( fs, &date1, 2 );
   start_julian = g_date_get_julian( &date1 );
   g_date_set_julian( &date2, start_julian + 0 );
   xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
*/

   /* This loop does not test for days/occurences which can
    * fail to fall in the same month. */
   for( monthday = 1; monthday <= 28; ++monthday ) {
      for( month = 1; month <= 12; ++month ) {
         g_date_set_dmy( &date1, monthday, month, 1 );
         start_julian = g_date_get_julian( &date1 );
         for( interval = 1; interval <= 24; ++interval ) {
            xaccFreqSpecSetMonthRelative( fs, &date1, interval );
            for( i = 0; i <= 2 * 31 * interval; ++i ) {
               g_date_set_julian( &date2, start_julian + i );
               xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
               do_test_args(
                  g_date_get_weekday( &next_date ) == g_date_get_weekday( &date1 ),
                  "month relative repeats - check weekday",
                  __FILE__, __LINE__,
                  "monthday = %d, month = %d, interval = %d months, days from start = %d, weekday = %d",
                  monthday, month, interval, i, g_date_get_weekday( &date1 ) );
               do_test_args(
                  (g_date_get_day( &next_date )-1)/7 == (g_date_get_day( &date1 )-1)/7,
                  "month relative repeats - check occurrence",
                  __FILE__, __LINE__,
                  "monthday = %d, month = %d, interval = %d months, days from start = %d, occurrence = %d",
                  monthday, month, interval, i, (g_date_get_day( &date1 )-1)/7 );
               do_test_args(
                  ( g_date_get_month( &next_date ) + 12 * (g_date_get_year( &next_date )-1) - 1) %
                     interval == (month-1) % interval,
                  "month relative repeats - check month",
                  __FILE__, __LINE__,
                  "monthday = %d, month = %d, interval = %d months, days from start = %d",
                  monthday, month, interval, i );
            }
         }
      }
   }

   /* This tests whether we can correctly generate a sequence of dates,
    * and end up in the right place. */
   /* Unfortunately, I think that this is going to require specifically
    * written test cases, for the case where the repeat falls in the next
    * (or subsequent) month in the cycle.... */
   
   g_date_set_dmy( &date1, 1, 1, 2000 );
   for( interval = 1; interval < 20; ++interval ) {
      xaccFreqSpecSetMonthRelative( fs, &date1, interval );
      for( j = 1; j < 20; ++j ) {
         date2 = date1;
         for( i = 0; i < j; ++i ) {
            xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
            date2 = next_date;
         }
         date2 = date1;
         g_date_add_months( &date2, interval * j );
         do_test_args( g_date_get_month( &date2 ) == g_date_get_month( &next_date ),
            "month_relative repeats end up in the right place - month",
            __FILE__, __LINE__, "interval = %d months, iters = %d, weekday = %d",
            interval, j, g_date_get_weekday( &date1 ) );
         do_test_args( g_date_get_weekday( &date1 ) == g_date_get_weekday( &next_date ),
            "month_relative repeats end up in the right place - weekday",
            __FILE__, __LINE__, "interval = %d months, iters = %d, weekday = %d",
            interval, j, g_date_get_weekday( &date1 ) );
         do_test_args( (g_date_get_day( &date2 )-1)/7 == (g_date_get_day( &next_date )-1)/7,
            "month_relative repeats end up in the right place - occurrence",
            __FILE__, __LINE__, "interval = %d months, iters = %d, weekday = %d",
            interval, j, g_date_get_weekday( &date1 ) );
      }
   }
   
   /* also test oddball cases */
   /* This is the fifth Sunday of a five Sunday month. */
   g_date_set_dmy( &date0, 29, 4, 2001 );
   xaccFreqSpecSetMonthRelative( fs, &date0, 1 );
   
   xaccFreqSpecGetNextInstance( fs, &date0, &next_date );
   g_date_set_dmy( &date1, 29, 7, 2001 );
   do_test( g_date_compare( &next_date, &date1 ) == 0, "find five-sunday months" );
   date2 = next_date;
   
   xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
   g_date_set_dmy( &date1, 30, 9, 2001 );
   do_test( g_date_compare( &next_date, &date1 ) == 0, "find five-sunday months" );
   date2 = next_date;

   xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
   g_date_set_dmy( &date1, 30, 12, 2001 );
   do_test( g_date_compare( &next_date, &date1 ) == 0, "find five-sunday months" );
   date2 = next_date;
   
   xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
   g_date_set_dmy( &date1, 31, 3, 2002 );
   do_test( g_date_compare( &next_date, &date1 ) == 0, "find five-sunday months" );
   date2 = next_date;
   fprintf(stdout, " FreqSpec: Relative months test OK, continuing . . . \r");
   fflush(stdout);   
   xaccFreqSpecFree(fs);
}

static void test_caseA()
{
   FreqSpec *fs;
   GDate date0, date1, date2, date3;

   fs = xaccFreqSpecMalloc(book);

   g_date_set_dmy(&date0, 31, 12, 1); /* end of year */

   xaccFreqSpecSetMonthly(fs, &date0, 3);  /* quarterly */

   g_date_set_dmy(&date1, 13, 2, 1);  /* Feb 13th */
   xaccFreqSpecGetNextInstance( fs, &date1, &date2 );

   g_date_set_dmy( &date3, 31, 3, 1 ); /* Should get March 31st */
   do_test( g_date_compare( &date2, &date3 ) == 0, "end of quarter" );

   xaccFreqSpecFree(fs);
}

static void
test_composite (void)
{
   FreqSpec *fs, *fs2;
   GDate date0, date1, date2;

   fs = xaccFreqSpecMalloc(book);

   /* Use this to test any specific cases which fail,
    * for easy access in the debugger. */

/*
   g_date_set_dmy( &date1, 1, 1, 1 );
   xaccFreqSpecSetMonthly( fs, &date1, 2 );
   start_julian = g_date_get_julian( &date1 );
   g_date_set_julian( &date2, start_julian + 0 );
   xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
*/

   /* I have not tested this type as thoroughly
    * because I expect that the elements from which it
    * has been constructed have been pretty much tortured
    * in the previous tests. I don't expect anything strange
    * to go on here, at least for now... Maybe I'll put
    * in more extensive tests later. */
   
   xaccFreqSpecSetComposite( fs );

   fs2 = xaccFreqSpecMalloc(book);
   g_date_set_dmy( &date0, 29, 3, 2001 ); /* Wednesday */
   xaccFreqSpecSetWeekly( fs2, &date0, 2 );
   xaccFreqSpecCompositeAdd( fs, fs2 );
   
   fs2 = xaccFreqSpecMalloc(book);
   g_date_set_dmy( &date0, 3, 4, 2001 ); /* Tuesday */
   xaccFreqSpecSetWeekly( fs2, &date0, 2 );
   xaccFreqSpecCompositeAdd( fs, fs2 );
      
   fs2 = xaccFreqSpecMalloc(book);
   g_date_set_dmy( &date0, 7, 4, 2001 ); /* Saturday */
   xaccFreqSpecSetWeekly( fs2, &date0, 2 );
   xaccFreqSpecCompositeAdd( fs, fs2 );
   
   fs2 = 0;

   /* OK - now let's iterate it and see if we get the right sequence of dates. */
   g_date_set_dmy( &date0, 26, 3, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 29, 3, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0,
                 "first date in sequence" );
   
   g_date_set_dmy( &date0, 27, 3, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 29, 3, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0,
                 "first date in sequence" );
   
   g_date_set_dmy( &date0, 28, 3, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 29, 3, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "first date in sequence" );
   
   g_date_set_dmy( &date0, 29, 3, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 3, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "second date in sequence" );

   g_date_set_dmy( &date0, 30, 3, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 3, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "second date in sequence" );

   g_date_set_dmy( &date0, 31, 3, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 3, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "second date in sequence" );

   g_date_set_dmy( &date0, 1, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 3, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "second date in sequence" );

   g_date_set_dmy( &date0, 2, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 3, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "second date in sequence" );

   g_date_set_dmy( &date0, 3, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 7, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "third date in sequence" );

   g_date_set_dmy( &date0, 4, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 7, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "third date in sequence" );

   g_date_set_dmy( &date0, 5, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 7, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "third date in sequence" );

   g_date_set_dmy( &date0, 6, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 7, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "third date in sequence" );

   g_date_set_dmy( &date0, 7, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 12, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "fourth date in sequence" );

   g_date_set_dmy( &date0, 8, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 12, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "fourth date in sequence" );

   g_date_set_dmy( &date0, 9, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 12, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "fourth date in sequence" );

   g_date_set_dmy( &date0, 10, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 12, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "fourth date in sequence" );

   g_date_set_dmy( &date0, 11, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 12, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "fourth date in sequence" );

   g_date_set_dmy( &date0, 12, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 17, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "fifth date in sequence" );

   g_date_set_dmy( &date0, 13, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 17, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "fifth date in sequence" );

   g_date_set_dmy( &date0, 14, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 17, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "fifth date in sequence" );

   g_date_set_dmy( &date0, 15, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 17, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "fifth date in sequence" );

   g_date_set_dmy( &date0, 16, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 17, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "fifth date in sequence" );

   g_date_set_dmy( &date0, 17, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 21, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "sixth date in sequence" );

   g_date_set_dmy( &date0, 18, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 21, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "sixth date in sequence" );

   g_date_set_dmy( &date0, 19, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 21, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "sixth date in sequence" );

   g_date_set_dmy( &date0, 20, 4, 2001 );
   xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
   g_date_set_dmy( &date2, 21, 4, 2001 );
   do_test( g_date_compare( &date1, &date2 ) == 0, "sixth date in sequence" );
   fprintf(stdout, " FreqSpec: Composite months test OK, cleaning up\n");
   fflush(stdout);   
   xaccFreqSpecFree(fs);
}

static void
test_monthly_31st_bug_104844()
{
     gchar date_buf[128];
     GDate start, next, expected;
     FreqSpec *fs = xaccFreqSpecMalloc(book);

     g_date_clear(&next, 1);

     g_date_clear(&start, 1);
     g_date_set_dmy(&start, 31, 1, 2003);
     xaccFreqSpecSetMonthly(fs, &start, 1);

     //g_date_add_days(&start, 1);
     xaccFreqSpecGetNextInstance(fs, &start, &next);
     g_date_clear(&expected, 1);
     g_date_set_dmy(&expected, 28, 2, 2003);
     g_date_strftime(date_buf, 128, "%c", &next);
     do_test(g_date_compare(&expected, &next) == 0, date_buf);

     start = next;
     xaccFreqSpecGetNextInstance(fs, &start, &next);
     g_date_set_dmy(&expected, 31, 3, 2003);
     g_date_strftime(date_buf, 128, "%c", &next);
     do_test(g_date_compare(&expected, &next) == 0, date_buf);

     // test...
     g_date_set_dmy(&start, 31, 1, 2003);
     xaccFreqSpecSetMonthly(fs, &start, 1);
     g_date_set_dmy(&start, 31, 1, 2007);
     xaccFreqSpecGetNextInstance(fs, &start, &next);
     g_date_set_dmy(&expected, 28, 2, 2007);
     g_date_strftime(date_buf, 128, "%c", &next);
     do_test(g_date_compare(&expected, &next) == 0, date_buf);

     start = next;
     xaccFreqSpecGetNextInstance(fs, &start, &next);
     g_date_set_dmy(&expected, 31, 3, 2007);
     g_date_strftime(date_buf, 128, "%c", &next);
     do_test(g_date_compare(&expected, &next) == 0, date_buf);

     xaccFreqSpecFree(fs);
}

int
main (int argc, char **argv)
{
    QofSession *session;

    qof_init();
    g_return_val_if_fail(cashobjects_register(), -1);
    session = qof_session_new ();
    book = qof_session_get_book(session);

    test_monthly_31st_bug_104844();

    test_once();
    test_caseA();
    test_daily();
    test_weekly();
    test_monthly();
    test_month_relative();
    test_composite();

    print_test_results();
    qof_session_end(session);
    qof_close();
    return (get_rv() > 1);
}
