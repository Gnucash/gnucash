
/*
 * Testing routine added by Ben Stanley bds02@uow.edu.au 20010320
 * Try to test Joshua Sled's FreqSpec module.
 *
 */

#include "config.h"

#include <stdlib.h>
#include <glib.h>

#include "test-stuff.h"
#include "FreqSpec.h"

void test_once()
{
	FreqSpec *fs;
	guint32 i, start_julian;
	GDate date1, date2, next_date;

	fs = xaccFreqSpecMalloc();

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

	xaccFreqSpecFree(fs);
}

void test_daily()
{
	guint32 interval, i, start_julian, j;
	FreqSpec *fs;
	GDate date1, date2, next_date;

	fs = xaccFreqSpecMalloc();

	g_date_set_dmy( &date1, 1, 1, 2000 );

	start_julian = g_date_julian( &date1 );
	for( interval = 1; interval < 400; ++interval ) {
		xaccFreqSpecSetDaily( fs, &date1, interval );
		for( i = 0; i <= 2 * interval; ++i ) {
			g_date_set_julian( &date2, start_julian + i );
			xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
			do_test_args(
				g_date_julian( &next_date ) - g_date_julian( &date2 ) ==
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
		for( j = 0; j < 20; ++j ) {
			date2 = date1;
			for( i = 0; i < j; ++i ) {
				xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
				date2 = next_date;
			}
			do_test_args( g_date_julian( &date2 ) - g_date_julian( &date1 ) == interval*j,
				"daily repeats end up in the right place",
				__FILE__, __LINE__, "interval = %d days, iters = %d",
				interval, j );
		}
	}

	xaccFreqSpecFree(fs);
}

void test_weekly()
{
	guint32 interval, i, start_julian, weekday, j;
	FreqSpec *fs;
	GDate date1, date2, next_date;

	fs = xaccFreqSpecMalloc();

	/* Use this to test any specific cases which fail,
	 * for easy access in the debugger. */
/*
	g_date_set_dmy( &date1, 2, 1, 1 );
	xaccFreqSpecSetWeekly( fs, &date1, 1 );
	start_julian = g_date_julian( &date1 );
	g_date_set_julian( &date2, start_julian + 6 );
	xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
*/
	/* date2 should now be 9/1/1, julian date 9. */

	for( weekday = 1; weekday <= 7; ++weekday ) {
		g_date_set_dmy( &date1, weekday, 1, 1 );
		start_julian = g_date_julian( &date1 );
		for( interval = 1; interval <= 52; ++interval ) {
			xaccFreqSpecSetWeekly( fs, &date1, interval );
			for( i = 0; i <= 2 * 7 * interval; ++i ) {
				g_date_set_julian( &date2, start_julian + i );
				xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
				do_test_args(
					g_date_julian( &next_date ) - g_date_julian( &date2 ) ==
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
			do_test_args( g_date_julian( &date2 ) - g_date_julian( &date1 ) == interval*7*j,
				"weekly repeats end up in the right place",
				__FILE__, __LINE__, "interval = %d weeks, iters = %d",
				interval, j );
		}
	}

	xaccFreqSpecFree(fs);
}

void test_monthly()
{
	guint32 interval, i, start_julian, monthday, month, j, day_of_year;
	FreqSpec *fs;
	GDate date0, date1, date2, next_date;

	fs = xaccFreqSpecMalloc();

	/* Use this to test any specific cases which fail,
	 * for easy access in the debugger. */

/*
	g_date_set_dmy( &date1, 1, 1, 1 );
	xaccFreqSpecSetMonthly( fs, &date1, 2 );
	start_julian = g_date_julian( &date1 );
	g_date_set_julian( &date2, start_julian + 0 );
	xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
*/

	for( monthday = 1; monthday <= 28; ++monthday ) {
		for( month = 1; month <= 12; ++month ) {
			g_date_set_dmy( &date1, monthday, month, 1 );
			start_julian = g_date_julian( &date1 );
			for( interval = 1; interval <= 24; ++interval ) {
				xaccFreqSpecSetMonthly( fs, &date1, interval );
				for( i = 0; i <= 2 * 31 * interval; ++i ) {
					g_date_set_julian( &date2, start_julian + i );
					xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
					do_test_args(
						g_date_day( &next_date ) == g_date_day( &date1 ),
						"monthly repeats - check day",
						__FILE__, __LINE__,
						"monthday = %d, month = %d, interval = %d months, days from start = %d",
						monthday, month, interval, i );
					do_test_args(
						( g_date_month( &next_date ) + 12 * (g_date_year( &next_date )-1) - 1) %
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
		g_date_set_julian( &date1, g_date_julian( &date0 ) + day_of_year-1 );
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

	xaccFreqSpecFree(fs);
}

void test_month_relative()
{
	guint32 interval, i, start_julian, monthday, month, j, day_of_year;
	FreqSpec *fs;
	GDate date0, date1, date2, next_date;

	fs = xaccFreqSpecMalloc();

	/* Use this to test any specific cases which fail,
	 * for easy access in the debugger. */

/*
	g_date_set_dmy( &date1, 1, 1, 1 );
	xaccFreqSpecSetMonthRelative( fs, &date1, 2 );
	start_julian = g_date_julian( &date1 );
	g_date_set_julian( &date2, start_julian + 0 );
	xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
*/

	/* This loop does not test for days/occurences which can
	 * fail to fall in the same month. */
	for( monthday = 1; monthday <= 28; ++monthday ) {
		for( month = 1; month <= 12; ++month ) {
			g_date_set_dmy( &date1, monthday, month, 1 );
			start_julian = g_date_julian( &date1 );
			for( interval = 1; interval <= 24; ++interval ) {
				xaccFreqSpecSetMonthRelative( fs, &date1, interval );
				for( i = 0; i <= 2 * 31 * interval; ++i ) {
					g_date_set_julian( &date2, start_julian + i );
					xaccFreqSpecGetNextInstance( fs, &date2, &next_date );
					do_test_args(
						g_date_weekday( &next_date ) == g_date_weekday( &date1 ),
						"month relative repeats - check weekday",
						__FILE__, __LINE__,
						"monthday = %d, month = %d, interval = %d months, days from start = %d, weekday = %d",
						monthday, month, interval, i, g_date_weekday( &date1 ) );
					do_test_args(
						(g_date_day( &next_date )-1)/7 == (g_date_day( &date1 )-1)/7,
						"month relative repeats - check occurrence",
						__FILE__, __LINE__,
						"monthday = %d, month = %d, interval = %d months, days from start = %d, occurrence = %d",
						monthday, month, interval, i, (g_date_day( &date1 )-1)/7 );
					do_test_args(
						( g_date_month( &next_date ) + 12 * (g_date_year( &next_date )-1) - 1) %
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
			do_test_args( g_date_month( &date2 ) == g_date_month( &next_date ),
				"month_relative repeats end up in the right place - month",
				__FILE__, __LINE__, "interval = %d months, iters = %d, weekday = %d",
				interval, j, g_date_weekday( &date1 ) );
			do_test_args( g_date_weekday( &date1 ) == g_date_weekday( &next_date ),
				"month_relative repeats end up in the right place - weekday",
				__FILE__, __LINE__, "interval = %d months, iters = %d, weekday = %d",
				interval, j, g_date_weekday( &date1 ) );
			do_test_args( (g_date_day( &date2 )-1)/7 == (g_date_day( &next_date )-1)/7,
				"month_relative repeats end up in the right place - occurrence",
				__FILE__, __LINE__, "interval = %d months, iters = %d, weekday = %d",
				interval, j, g_date_weekday( &date1 ) );
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
	
	xaccFreqSpecFree(fs);
}

void test_composite()
{
	guint32 interval, i, start_julian, monthday, month, j, day_of_year;
	FreqSpec *fs, *fs2;
	GDate date0, date1, date2, next_date;

	fs = xaccFreqSpecMalloc();

	/* Use this to test any specific cases which fail,
	 * for easy access in the debugger. */

/*
	g_date_set_dmy( &date1, 1, 1, 1 );
	xaccFreqSpecSetMonthly( fs, &date1, 2 );
	start_julian = g_date_julian( &date1 );
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

	fs2 = xaccFreqSpecMalloc();
	g_date_set_dmy( &date0, 29, 3, 2001 ); /* Wednesday */
	xaccFreqSpecSetWeekly( fs2, &date0, 2 );
	xaccFreqSpecCompositeAdd( fs, fs2 );
	
	fs2 = xaccFreqSpecMalloc();
	g_date_set_dmy( &date0, 3, 4, 2001 ); /* Tuesday */
	xaccFreqSpecSetWeekly( fs2, &date0, 2 );
	xaccFreqSpecCompositeAdd( fs, fs2 );
		
	fs2 = xaccFreqSpecMalloc();
	g_date_set_dmy( &date0, 7, 4, 2001 ); /* Saturday */
	xaccFreqSpecSetWeekly( fs2, &date0, 2 );
	xaccFreqSpecCompositeAdd( fs, fs2 );
	
	fs2 = 0;

	/* OK - now let's iterate it and see if we get the right sequence of dates. */
	g_date_set_dmy( &date0, 26, 3, 2001 );
	xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
	g_date_set_dmy( &date2, 29, 3, 2001 );
	do_test( g_date_compare( &date1, &date2 ) == 0, "first date in sequence" );
	
	g_date_set_dmy( &date0, 27, 3, 2001 );
	xaccFreqSpecGetNextInstance( fs, &date0, &date1 );
	g_date_set_dmy( &date2, 29, 3, 2001 );
	do_test( g_date_compare( &date1, &date2 ) == 0, "first date in sequence" );
	
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

	xaccFreqSpecFree(fs);
}

int
main( int argc, char* argv[] )
{
	g_log_set_always_fatal( G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING );

#if 0
	set_success_print(TRUE);
#endif

	test_once();

	test_daily();

	test_weekly();

	test_monthly();

	test_month_relative();

	test_composite();

	print_test_results();
	return get_rv();
}
