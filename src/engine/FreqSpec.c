/********************************************************************\
 * FreqSpec.c -- Frequency specifier implementation.                *
 * Copyright (C) 2001 Joshua Sled <jsled@asynchronous.org>          *
 * Copyright (C) 2001 Ben Stanley <bds02@uow.edu.au>                *
 *                                                                  *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/********************************************************************\
 Current status
 All kinds of repeats work, including composites. This is tested -
 although composites need some more test cases to be put into the test
 suite - ../test/test-freq-spec.c
 FreqSpec objects are currently 'set' to give them the information
 they need. Separate methods for modifying currently existing FreqSpec
 objects are not provided. In the case of composites, you may add FreqSpec
 objects to a composite, and you may access a list of the FreqSpec objects
 which form the composite. This interface allows you to do damage...

 TODO list
 Ben Stanley 2001-04-02
        * Write xaccFreqSpecGetFreqStr (I wonder how this will be
                internationalised... I suspect that this code will need to be
                re-written for each language, because the code will have to
                generate grammar... It's more than just translating strings.)
                However, the first priority is to write one that works for 
                English.
        * Write a function to allow you to query whether a given
                date forms part of the recurrence.
        * Write a method to get the previous recurrence
        * provide XML Load/Save functionality for this object.
        * Figure out xaccFreqSpecIsValidDate - I suspect that this is the
                'query whether a given date forms part of the recurrence' 
                above.
        * FIGURE OUT WHAT'S GOING ON WITH xaccFreqSpecGetUIType AND
                xaccFreqSpecSetUIType.
        * Try to reduce the size of the data structure. There are quite a few
                32 bit fields which could be stored in about 8 bits.
        * Add public methods to allow for recurrences with an interval
                of 1 to be set without reference to an initial 'date' - monthly
                things in particular. Try to reduce the dependence on an initial
                date for the input to set up the recurrence.
        
 Questions:
        Is it best that the public interface stay as GDate, or should it
        really be a timespec? I have no problem with converting GDates to
        timespecs for load/save if that makes life easier.
        
        However, I chose to use GDate internally because I have used a *lot*
        of the date calculating ability of GDate in the internal implementation.
        GDate has simplified this work enormously compared to using struct tm
        and time_t. The engine's timespec object doesn't appear to have the
        required functionality either, so I would need to write the required
        functions for timespec (perhaps by implementing in terms of GDate?).
        
        Hopefully it's not too painful to leave GDate in the public interface
        and change other code to use it.
\********************************************************************/

#include "config.h"

#include <time.h>

#include <glib.h>
#include <string.h>

#ifdef HAVE_LANGINFO_D_FMT
#include <langinfo.h>
#endif

#include "date.h"
#include "FreqSpecP.h"
#include "GNCIdP.h"
#include "gnc-book-p.h"
#include "gnc-engine-util.h"
#include "gnc-event-p.h"
#include "messages.h"

/* I have done this to prevent compiler warnings...
 * This is used to convert a const GDate* to a GDate* for passing
 * to the glib g_date_xxx functions which don't use const...
 * Strangely, most of the rest of glib does use const, so
 * perhaps this will change? When it does, just define this macro to
 * nothing and the compiler will check the constness of each pointer....
 */
#define CONST_HACK (GDate*)

static short module = MOD_SX;
/* 
 *  FIXME: should be in a header file
 */

#ifdef HAVE_LANGINFO_D_FMT
#  define GNC_D_FMT (nl_langinfo (D_FMT))
#else
#  define GNC_D_FMT "%Y-%m-%d"
#endif
 
#define GDATE_STRING_SIZE 25
#define GDATE_STRING_BUF_SIZE (GDATE_STRING_SIZE + 1)
 
#define WDAY_NAME_WIDTH 100
#define WDAY_BUF_WIDTH  (WDAY_NAME_WIDTH + 1)
 
/** PROTOTYPES ******************************************************/

/**
 * Destroys all sub-FreqSpecs in a composite FreqSpec.
 * Assertion error if it's not a COMPOSITE FreqSpec.
 **/
void xaccFreqSpecCompositesClear( FreqSpec *fs );

void subSpecsListMapDelete( gpointer data, gpointer user_data );

/** Local data defs *****/


/** Local Prototypes *****/


static const char *
get_wday_name(guint day)
{
  static gchar wday_name[WDAY_BUF_WIDTH];
  struct tm t;
  t.tm_wday = day;
  strftime(wday_name, WDAY_NAME_WIDTH, "%A", &t);
  return wday_name;
}
 
#if 0
static const char *
get_full_month_name(guint month)
{
  static gchar month_name[WDAY_BUF_WIDTH];
  struct tm t;
  t.tm_mon = month;
  strftime(month_name, WDAY_NAME_WIDTH, "%B", &t);
  return month_name;
}
#endif

static const char *
get_abbrev_month_name(guint month)
{
  static gchar month_name[WDAY_BUF_WIDTH];
  struct tm t;
  t.tm_mon = month;
  strftime(month_name, WDAY_NAME_WIDTH, "%b", &t);
  return month_name;
}

/**
 * Initializes a FreqSpec by setting it's to type INVALID.
 * Use this to initialise a stack object.
 * FreqSpec objects must be initalised before being used by
 * any other method.
 **/

static void
xaccFreqSpecInit( FreqSpec *fs, GNCBook *book )
{
        g_return_if_fail( fs );
        g_return_if_fail (book);

        fs->entity_table = gnc_book_get_entity_table (book);

        xaccGUIDNew( &fs->guid, book );
        xaccStoreEntity( fs->entity_table, fs, &fs->guid, GNC_ID_FREQSPEC );

        fs->type = INVALID;
        fs->uift = UIFREQ_ONCE;

        memset( &(fs->s), 0, sizeof(fs->s) );
}

FreqSpec*
xaccFreqSpecMalloc(GNCBook *book)
{
        FreqSpec        *fs;

        g_return_val_if_fail (book, NULL);

        fs = g_new0(FreqSpec, 1);
        xaccFreqSpecInit( fs, book );
        /* FIXME:event */
        gnc_engine_generate_event( &fs->guid, GNC_EVENT_CREATE );
        return fs;
}


void
xaccFreqSpecCleanUp( FreqSpec *fs )
{
        g_return_if_fail( fs );
        switch ( fs->type ) {
        case INVALID:
        case ONCE:
        case DAILY:
        case WEEKLY:
        case MONTHLY:
        case MONTH_RELATIVE:
                break;
        case COMPOSITE:
                xaccFreqSpecCompositesClear( fs );
                g_list_free( fs->s.composites.subSpecs );
                break;
        default:
                g_return_if_fail(FALSE);
        }
        fs->type = INVALID;
}

void
xaccFreqSpecFree( FreqSpec *fs )
{
        if ( fs == NULL ) return;
        gnc_engine_generate_event( &fs->guid, GNC_EVENT_DESTROY );
        xaccRemoveEntity( fs->entity_table, &fs->guid );

        xaccFreqSpecCleanUp( fs );

        g_free( fs );
}

FreqType
xaccFreqSpecGetType( FreqSpec *fs )
{
        g_return_val_if_fail( fs, INVALID );
        /* Is this really a fail? */
        g_return_val_if_fail( fs->type != INVALID, INVALID );
        return fs->type;
}


UIFreqType
xaccFreqSpecGetUIType( FreqSpec *fs )
{
        g_return_val_if_fail( fs, INVALID );
        return fs->uift;
}

void
xaccFreqSpecSetUIType( FreqSpec *fs, UIFreqType newUIFreqType )
{
        g_return_if_fail( fs );
        fs->uift = newUIFreqType;
}

static inline guint32 min( guint32 a, guint32 b )
{
        return a > b ? b : a;
}

void
xaccFreqSpecGetNextInstance(
        FreqSpec *fs,
        const GDate* in_date,
        GDate* out_date )
{
        GList                *list;

        g_return_if_fail( fs );
        switch( fs->type ) {
        case INVALID:
                g_return_if_fail(FALSE);

        case ONCE:
                if ( g_date_compare( &(fs->s.once.date), CONST_HACK in_date ) > 0 ) {
                        *out_date = fs->s.once.date;
                } else {
                        /* Date is past due. Return an invalid date. */
                        g_date_clear( out_date, 1 );
                }
                break;

        case DAILY: {
                        guint32 julian_in_date, julian_next_repeat, complete_intervals;

                        julian_in_date = g_date_julian( CONST_HACK in_date );
                        complete_intervals =
                                (julian_in_date - fs->s.daily.offset_from_epoch) /
                                        fs->s.daily.interval_days;
                        julian_next_repeat =
                                fs->s.daily.offset_from_epoch +
                                        (complete_intervals + 1) * fs->s.daily.interval_days;
                        g_date_set_julian( out_date, julian_next_repeat );
                } break;

        case WEEKLY: {
                        /* This implementation stores the offset from epoch as the number
                         * of days, not week epoch offset and day in week offset.
                         * It is very similar to the daily repeat representation. */
                        guint32 julian_in_date, julian_next_repeat, complete_intervals;

                        julian_in_date = g_date_julian( CONST_HACK in_date );
                        complete_intervals =
                                (julian_in_date - fs->s.weekly.offset_from_epoch) /
                                        (fs->s.weekly.interval_weeks * 7);
                        julian_next_repeat =
                                fs->s.weekly.offset_from_epoch +
                                        (complete_intervals + 1) * fs->s.weekly.interval_weeks * 7;
                        g_date_set_julian( out_date, julian_next_repeat );

                        /* This code passes the test, but it seems large and complicated...
                         * it uses a separate week offset from epoch and day in week offset. */
/*                        guint32 julian_in_date, julian_next_repeat, complete_intervals,
                                in_weeks_from_epoch, after_repeat_in_week_interval;
                        julian_in_date = g_date_julian( CONST_HACK in_date );
                        in_weeks_from_epoch = (julian_in_date-1) / 7;
                        complete_intervals =
                                (in_weeks_from_epoch -
                                        fs->s.weekly.offset_from_epoch) /
                                        fs->s.weekly.interval_weeks;
                        after_repeat_in_week_interval =
                                ((julian_in_date-1) % 7 >= fs->s.weekly.day_of_week ||
                                        (in_weeks_from_epoch - fs->s.weekly.offset_from_epoch) %
                                                fs->s.weekly.interval_weeks > 0 ) ? 1 : 0;
                        julian_next_repeat =
                                (fs->s.weekly.offset_from_epoch +
                                        (complete_intervals + after_repeat_in_week_interval) *
                                                fs->s.weekly.interval_weeks) * 7 +
                                fs->s.weekly.day_of_week + 1;
                        g_date_set_julian( out_date, julian_next_repeat );
*/
                } break;

        case MONTHLY: {
                        guint32 in_months_from_epoch, after_repeat_in_month_interval,
                                complete_intervals, next_repeat_months_from_epoch, month, year;

                        in_months_from_epoch = (g_date_year( CONST_HACK in_date )-1) * 12 +
                                g_date_month( CONST_HACK in_date ) - 1;
                        complete_intervals =
                                (in_months_from_epoch - fs->s.monthly.offset_from_epoch) /
                                        fs->s.monthly.interval_months;
                        after_repeat_in_month_interval =
                                (g_date_day( CONST_HACK in_date ) >= fs->s.monthly.day_of_month ||
                                        (in_months_from_epoch - fs->s.monthly.offset_from_epoch) %
                                                fs->s.monthly.interval_months > 0 ||
                                        g_date_day( CONST_HACK in_date ) >=
                                                g_date_days_in_month( g_date_month( CONST_HACK in_date ),
                                                        g_date_year( CONST_HACK in_date ) ) )  ? 1 : 0;
                        next_repeat_months_from_epoch =
                                fs->s.monthly.offset_from_epoch +
                                        (complete_intervals + after_repeat_in_month_interval) *
                                                fs->s.monthly.interval_months;
                        /* Hmmm... what happens if the day of the month is greater than the
                         * number of days in this month?
                         * Here I have constrained the day of the month by the number
                         * of days in the month. This is compensated for above by checking if
                         * the input day is the last day of that month, in which case it will
                         * move to the next month interval.
                         */
                        month = next_repeat_months_from_epoch % 12 + 1;
                        year = next_repeat_months_from_epoch / 12 + 1;
                        g_date_set_dmy( out_date,
                                min( fs->s.monthly.day_of_month,
                                        g_date_days_in_month( month, year ) ),
                                month,
                                year );
                } break;
        
        case MONTH_RELATIVE: {
                        guint32 in_months_from_epoch, after_repeat_in_month_interval,
                                complete_intervals, next_repeat_months_from_epoch, month, year,
                                wday_of_1st, day_of_repeat;

                        GDate date1;
                        in_months_from_epoch = (g_date_year( CONST_HACK in_date )-1) * 12 +
                                g_date_month( CONST_HACK in_date ) - 1;
                        complete_intervals =
                                (in_months_from_epoch - fs->s.month_relative.offset_from_epoch) /
                                        fs->s.month_relative.interval_months;
                        month = g_date_month( CONST_HACK in_date );
                        year = g_date_year( CONST_HACK in_date );
                        g_date_set_dmy( &date1, 1, month, year );
                        wday_of_1st = g_date_weekday( &date1 );
                        day_of_repeat = (fs->s.month_relative.occurrence-1)*7 +
                                ((fs->s.month_relative.weekday + 7 - wday_of_1st)%7 + 1);
                        after_repeat_in_month_interval =
                                (g_date_day( CONST_HACK in_date ) >= day_of_repeat ||
                                day_of_repeat > g_date_days_in_month( month, year ) ||
                                (in_months_from_epoch - fs->s.month_relative.offset_from_epoch) %
                                                fs->s.month_relative.interval_months > 0 )  ? 1 : 0;
                        next_repeat_months_from_epoch =
                                fs->s.month_relative.offset_from_epoch +
                                        (complete_intervals + after_repeat_in_month_interval) *
                                                fs->s.month_relative.interval_months;
                        month = next_repeat_months_from_epoch % 12 + 1;
                        year = next_repeat_months_from_epoch / 12 + 1;
                        g_date_set_dmy( &date1, 1, month, year );
                        wday_of_1st = g_date_weekday( &date1 );
                        /* This calculates the day of the month in the month which forms
                         * the next month in the cycle after the given input date.
                         * However, this day may be larger than the number of days in that month... */
                        day_of_repeat = (fs->s.month_relative.occurrence-1)*7 +
                                ((fs->s.month_relative.weekday + 7 - wday_of_1st)%7 + 1);
                        while( day_of_repeat > g_date_days_in_month( month, year ) ) {
                                /* If the repeat occurs after the end of the month, then
                                 * find the next month containing a day which satisfies the request.
                                 * Each candiate month separated by interval_months is considered
                                 * by this loop.*/
                                ++complete_intervals;
                                next_repeat_months_from_epoch =
                                        fs->s.month_relative.offset_from_epoch +
                                                complete_intervals * fs->s.month_relative.interval_months;
                                month = next_repeat_months_from_epoch % 12 + 1;
                                year = next_repeat_months_from_epoch / 12 + 1;
                                g_date_set_dmy( &date1, 1, month, year );
                                wday_of_1st = g_date_weekday( &date1 );
                                day_of_repeat = (fs->s.month_relative.occurrence-1)*7 +
                                        ((fs->s.month_relative.weekday + 7 - wday_of_1st)%7 + 1);
                                /* Hmmm... It would be nice to know that this loop is
                                 * guaranteed to terminate... CHECK ME! */
                        }
                        g_date_set_dmy( out_date, day_of_repeat, month, year );
                } break;
        
        case COMPOSITE:
                list = fs->s.composites.subSpecs;
                if ( !list ) {
                        /* sets date to be invalid */
                        g_date_clear( out_date, 1 );
                        break;
                }
                {
                        /* This implements || composites. */
                        guint32 min_julian = 0xFFFFFFFF; /* the biggest unsigned 32 bit number */
                        guint32 this_julian;
                        do {
                                GDate next_repeat;
                                xaccFreqSpecGetNextInstance(
                                        (FreqSpec*) list->data,
                                        in_date,
                                        &next_repeat );
                                this_julian = g_date_julian( &next_repeat );
                                        
                                min_julian = min( min_julian, this_julian );

                        } while ( (list = g_list_next(list)) );
                        g_date_set_julian( out_date, min_julian );
                }
                break;

        default:
                g_date_clear( out_date, 1 );
                g_return_if_fail(FALSE);
        }
}

/*
char*
xaccFreqSpecIsValidDateRelaxed( FreqSpec *fs, time_t query )
{
        return "FIXME: not implemented yet!";
}
*/

void
xaccFreqSpecSetOnceDate( FreqSpec *fs, const GDate* when )
{
        g_return_if_fail( fs );
        g_return_if_fail( when );
        xaccFreqSpecCleanUp( fs );
        fs->type = ONCE;
        fs->s.once.date = *when;
}

void
xaccFreqSpecSetDaily( FreqSpec *fs,
                      const GDate* initial_date,
                      guint interval_days )
{
        guint32 julian_days_since_epoch;

        g_return_if_fail( fs );
        g_return_if_fail( interval_days > 0 );
        xaccFreqSpecCleanUp( fs );
        fs->type = DAILY;
        fs->s.daily.interval_days = interval_days;

        julian_days_since_epoch = g_date_julian( CONST_HACK initial_date );
        fs->s.daily.offset_from_epoch = julian_days_since_epoch % interval_days;
}

void
xaccFreqSpecSetWeekly( FreqSpec *fs,
                       const GDate* initial_date,
                       guint interval_weeks )
{
/* pick one... make sure that the code in next matches this,
 * and that the fields in the
 * weekly struct match too.
 */
#if 0
/* *
 * This implements weekly by using the fact that 1 week = 7 days.
 * Weeks start at epoch in this representation, not necesarily Monday,
 * so there is not really any difference...
 * The weekly tests pass.
 */
        guint32 julian_days_since_epoch;

        g_return_if_fail( fs );
        g_return_if_fail( interval_weeks > 0 );
        xaccFreqSpecCleanUp( fs );

        fs->type = DAILY;
        fs->s.daily.interval_days = 7 * interval_weeks;

        julian_days_since_epoch = g_date_julian( CONST_HACK initial_date );
        fs->s.daily.offset_from_epoch = julian_days_since_epoch % (7*interval_weeks);
#endif
#if 1
        /* simplest solution */
        guint32 julian_days_since_epoch;

        g_return_if_fail( fs );
        g_return_if_fail( interval_weeks > 0 );
        xaccFreqSpecCleanUp( fs );

        fs->type = WEEKLY;
        fs->s.weekly.interval_weeks = interval_weeks;

        julian_days_since_epoch = g_date_julian( CONST_HACK initial_date );
        fs->s.weekly.offset_from_epoch = julian_days_since_epoch % (7*interval_weeks);
#endif
#if 0
/**
 * Use the weekly implementation, which seems to be more complicated...
 * uses separate weekly and day in week offsets.
 * works.
 */
        guint32 julian_day_initial, weeks_since_epoch;

        g_return_if_fail( fs );
        g_return_if_fail( interval_weeks > 0 );
        xaccFreqSpecCleanUp( fs );

        fs->type = WEEKLY;
        fs->s.weekly.interval_weeks = interval_weeks;

        julian_day_initial = g_date_julian( CONST_HACK initial_date );
        weeks_since_epoch = (julian_day_initial-1) / 7;
        fs->s.weekly.day_of_week = (julian_day_initial-1) % 7;
        fs->s.weekly.offset_from_epoch = weeks_since_epoch % interval_weeks;

        g_return_if_fail( 0 <= fs->s.weekly.day_of_week );
        g_return_if_fail( fs->s.weekly.day_of_week < 7 );
        g_return_if_fail( fs->s.weekly.offset_from_epoch < interval_weeks );
        g_return_if_fail( 0 <= fs->s.weekly.offset_from_epoch );
#endif
}

void
xaccFreqSpecSetMonthly( FreqSpec *fs,
                        const GDate* initial_date,
                        guint interval_months )
{
        guint months_since_epoch;
        g_return_if_fail( fs );
        g_return_if_fail( interval_months > 0 );
        xaccFreqSpecCleanUp( fs );
        fs->type = MONTHLY;
        fs->s.monthly.interval_months = interval_months;

        months_since_epoch = (g_date_year( CONST_HACK initial_date )-1) * 12 +
                g_date_month( CONST_HACK initial_date ) - 1;
        fs->s.monthly.offset_from_epoch = months_since_epoch % interval_months;
        fs->s.monthly.day_of_month = g_date_day( CONST_HACK initial_date );

        g_return_if_fail( fs->s.monthly.offset_from_epoch <
                fs->s.monthly.interval_months );
}

void
xaccFreqSpecSetMonthRelative( FreqSpec *fs,
                              const GDate* initial_date,
                              guint interval_months )
{
        guint months_since_epoch;
        g_return_if_fail( fs );
        g_return_if_fail( interval_months > 0 );
        xaccFreqSpecCleanUp( fs );
        fs->type = MONTH_RELATIVE;
        fs->s.month_relative.interval_months = interval_months;

        months_since_epoch = (g_date_year( CONST_HACK initial_date )-1) * 12 +
                g_date_month( CONST_HACK initial_date ) - 1;
        fs->s.month_relative.offset_from_epoch = months_since_epoch % interval_months;
        
        fs->s.month_relative.weekday = g_date_weekday( CONST_HACK initial_date );
        fs->s.month_relative.occurrence = (g_date_day( CONST_HACK initial_date )-1) / 7 + 1;
        
        g_return_if_fail( fs->s.month_relative.weekday > 0 );
        g_return_if_fail( fs->s.month_relative.weekday <= 7 );
        g_return_if_fail( fs->s.month_relative.occurrence > 0 );
        g_return_if_fail( fs->s.month_relative.occurrence <= 5 );
        g_return_if_fail( fs->s.month_relative.offset_from_epoch <
                fs->s.month_relative.interval_months );
}

void
xaccFreqSpecSetComposite( FreqSpec *fs )
{
        g_return_if_fail( fs );
        xaccFreqSpecCleanUp( fs );
        fs->type = COMPOSITE;
        fs->s.composites.subSpecs = NULL;
}

int
xaccFreqSpecGetOnce( FreqSpec *fs, GDate *outGD )
{
        if ( fs->type != ONCE )
                return -1;
        *outGD = fs->s.once.date;
        return 0;
}

int
xaccFreqSpecGetDaily( FreqSpec *fs, int *outRepeat )
{
        if ( fs->type != DAILY )
                return -1;
        *outRepeat = fs->s.daily.interval_days;
        return 0;
}

int
xaccFreqSpecGetWeekly( FreqSpec *fs, int *outRepeat, int *outDayOfWeek )
{
        if ( fs->type != WEEKLY )
                return -1;
        *outRepeat = fs->s.weekly.interval_weeks;
        *outDayOfWeek = fs->s.weekly.offset_from_epoch % 7;
        return 0;
}

int
xaccFreqSpecGetMonthly( FreqSpec *fs, int *outRepeat, int *outDayOfMonth, int *outMonthOffset )
{
        if ( fs->type != MONTHLY )
                return -1;
        *outRepeat = fs->s.monthly.interval_months;
        *outDayOfMonth = fs->s.monthly.day_of_month;
        *outMonthOffset = fs->s.monthly.offset_from_epoch;
        return 0;
}

/* FIXME: add month-relative getter */

GList*
xaccFreqSpecCompositeGet( FreqSpec *fs )
{
        g_return_val_if_fail( fs, NULL );
        g_return_val_if_fail( fs->type == COMPOSITE, NULL );
        return fs->s.composites.subSpecs;
}

void
xaccFreqSpecCompositeAdd( FreqSpec *fs, FreqSpec *fsToAdd )
{
        g_return_if_fail( fs );
        g_return_if_fail( fs->type == COMPOSITE );
        fs->s.composites.subSpecs =
                g_list_append( fs->s.composites.subSpecs, fsToAdd );
}

void
subSpecsListMapDelete( gpointer data, gpointer user_data )
{
        xaccFreqSpecFree( (FreqSpec*)data );
}

void
xaccFreqSpecCompositesClear( FreqSpec *fs )
{
        g_return_if_fail( fs->type == COMPOSITE );
        g_list_foreach( fs->s.composites.subSpecs,
                        subSpecsListMapDelete, NULL );
}

static GString *
get_dom_string(guint dom)
{
  GString *str = g_string_new(NULL);

  if(dom > 31)
  {
    g_string_sprintf(str, _( "last day"));
  }
  else
  {
    g_string_sprintf(str, "%u", dom);
  }

  return str;
}

    
void
xaccFreqSpecGetFreqStr( FreqSpec *fs, GString *str )
{
        GList *list;
        FreqSpec *tmpFS;
        int tmpInt;
        char *tmpStr;
        int i;

        /* FIXME: fill in. */
        switch( xaccFreqSpecGetUIType( fs ) ) {
        case UIFREQ_ONCE:
                tmpStr = g_new0( char, GDATE_STRING_BUF_SIZE );
                /* this is now a GDate. */
                g_date_strftime( tmpStr, GDATE_STRING_SIZE,
                                 GNC_D_FMT,
                                 &fs->s.once.date );
                g_string_sprintf( str, "Once: %s", tmpStr );
                g_free( tmpStr );
                break;

        case UIFREQ_DAILY:
                g_string_sprintf( str, "Daily" );
                if ( fs->s.daily.interval_days > 1 ) {
                        g_string_sprintfa( str, " (x%u)",
                                           fs->s.daily.interval_days );
                }
                break;

        case UIFREQ_DAILY_MF:
        { 
                FreqSpec *subFS;
                if ( g_list_length( fs->s.composites.subSpecs ) != 5 ) {
                        PERR( "Invalid Daily[M-F] structure." );
                        g_string_sprintf( str, "Daily[M-F]: error" );
                        return;
                }
                /* We assume that all of the weekly FreqSpecs that make up
                   the Daily[M-F] FreqSpec have the same interval. */
                subFS = (FreqSpec*)fs->s.composites.subSpecs->data;
	       
                if ( subFS->s.weekly.interval_weeks > 1 ) {
                        g_string_sprintf( str, _("Weekdays: (x%u)"),
                                           subFS->s.weekly.interval_weeks );
                }
		else
		{
		  g_string_sprintf(str, _("Weekdays"));
		}
	  
        }
        break;

        case UIFREQ_WEEKLY:

                tmpInt = -1;
                tmpStr = g_new0( char, 8 );
                for ( i=0; i<7; i++ ) {
                        tmpStr[i] = '-';
                }

                
                for ( list = xaccFreqSpecCompositeGet( fs );
                      list; list = list->next ) {
                        int dowIdx;

                        tmpFS = (FreqSpec*)list->data;
                        if ( xaccFreqSpecGetType(tmpFS) != WEEKLY ) {
                                g_string_sprintf( str,
                                                  "error: UIFREQ_WEEKLY doesn't contain weekly children" );
                                return;
                        }
                        if ( tmpInt == -1 ) {
                                tmpInt = tmpFS->s.weekly.interval_weeks;
                        }
                        /* put the first letter of the weekday name in
                           the appropriate position. */
                        dowIdx = tmpFS->s.weekly.offset_from_epoch;
                        tmpStr[dowIdx] = *(get_wday_name(dowIdx));
                }

                if ( tmpInt > 1 ) {
		  g_string_sprintf( str, _( "Weekly (x%d): %s"), tmpInt, tmpStr );
                }
		else
		{
		  g_string_sprintf( str, _( "Weekly: %s"), tmpStr );
		}
                g_free( tmpStr );
                break;

        case UIFREQ_BI_WEEKLY:
                g_string_sprintf( str, _("Bi-Weekly, %ss"), get_wday_name(fs->s.weekly.offset_from_epoch % 7) );
                break;

        case UIFREQ_SEMI_MONTHLY:
	{
	  GString *first_dom, *second_dom;
  
	  list = xaccFreqSpecCompositeGet( fs );
	  tmpFS = (FreqSpec*)(g_list_nth( list, 0 )->data);
	 
	  first_dom = get_dom_string(tmpFS->s.monthly.day_of_month);

	  tmpFS = (FreqSpec*)(g_list_nth( list, 1 )->data);
	  second_dom = get_dom_string(tmpFS->s.monthly.day_of_month);


	  if ( tmpFS->s.monthly.interval_months > 1 ) {
	    g_string_sprintf( str, _("Semi-monthly (x%u): %s, %s"), 
			      tmpFS->s.monthly.interval_months,
			      first_dom->str, 
			      second_dom->str);
	  }
	  else
	  {
	    g_string_sprintf( str, _("Semi-monthly: %s, %s"), 
			      first_dom->str,
			      second_dom->str);
	  }
	  g_string_free(first_dom, TRUE);
	  g_string_free(second_dom, TRUE);
            
	  break;
	}

        case UIFREQ_MONTHLY:
               
                if ( fs->s.monthly.interval_months > 1 ) {
                        g_string_sprintf( str, _("Monthly (x%u): %u"),
                                           fs->s.monthly.interval_months,
					  fs->s.monthly.day_of_month);
                }
		else
		{
		  g_string_sprintf( str, _("Monthly: %u"),
				    fs->s.monthly.day_of_month );
		}
                break;

        case UIFREQ_QUARTERLY:
                if ( fs->s.monthly.interval_months != 3 ) {
                        g_string_sprintf( str, _("Quarterly (x%u): %u"),
					  fs->s.monthly.interval_months/3,
					  fs->s.monthly.day_of_month);
                }
		else
		{
		  g_string_sprintf( str, _("Quarterly: %u"),
                                   fs->s.monthly.day_of_month );
		}
                break;

        case UIFREQ_TRI_ANUALLY:

                if ( fs->s.monthly.interval_months != 4 ) {
                        g_string_sprintf( str, _("Tri-Yearly (x%u): %u"),
					  fs->s.monthly.interval_months/4,
					  fs->s.monthly.day_of_month);
                }
		else
		{
                g_string_sprintf( str, _("Tri-Yearly: %u"),
                                   fs->s.monthly.day_of_month );
		}
                break;

        case UIFREQ_SEMI_YEARLY:
                g_string_sprintf( str, "Semi-Yearly" );
                if ( fs->s.monthly.interval_months != 6 ) {
                        if ( (fs->s.monthly.interval_months % 6) != 0 ) {
                                PERR( "ERROR: FreqSpec Semi-Yearly month-interval "
                                      "is not a multiple of 6 [%d]",
                                      fs->s.monthly.interval_months );
                        }
                        g_string_sprintf( str, _("Semi-Yearly(x%u): %u"),
                                           fs->s.monthly.interval_months/6,
					  fs->s.monthly.day_of_month);
                }
		else
		{
		  g_string_sprintf( str, _("Semi-Yearly: %u"),
				     fs->s.monthly.day_of_month );
		}
                break;

        case UIFREQ_YEARLY:
                g_string_sprintf( str, "Yearly" );
                if ( fs->s.monthly.interval_months != 12 ) {
                        if ( (fs->s.monthly.interval_months % 12) != 0 ) {
                                PERR( "ERROR: \"Yearly\" FreqSpec month-interval "
                                      "is not a multiple of 12 [%d]",
                                      fs->s.monthly.interval_months );
                        }

			/*
			 * FIXME: This string *must* be translated for en_GB, en_AU
			 * and everywhere else with the sensible ordering of ddmmyy
			 */

                        g_string_sprintf( str, _("Yearly (x%u): %s/%u"),
                                           fs->s.monthly.interval_months/12,
					   get_abbrev_month_name(fs->s.monthly.offset_from_epoch),
					   fs->s.monthly.day_of_month);
                }
		else
		{
		  g_string_sprintf( str, _("Yearly: %s/%u"),
				     get_abbrev_month_name(fs->s.monthly.offset_from_epoch),
				     fs->s.monthly.day_of_month );
		}
                break;

        default:
                g_string_sprintf( str, _("Unknown") );
                break;
        }
}
