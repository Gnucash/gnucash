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
 * Free Software Foundation      Voice:  +1-617-542-5942            *
 * 51 Franklin Street, Fifth Floor   Fax:    +1-617-542-2652        *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
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

#include <glib.h>
#include <glib/gi18n.h>
#include "glib-compat.h"
#include <string.h>
#include <time.h>

#ifdef HAVE_LANGINFO_D_FMT
#include <langinfo.h>
#endif

#include "FreqSpecP.h"

#define LOG_MOD "gnc.engine.freqspec"
static QofLogModule log_module = LOG_MOD;
#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN LOG_MOD

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

FROM_STRING_FUNC(UIFreqType, ENUM_LIST_UI)
AS_STRING_FUNC(UIFreqType, ENUM_LIST_UI)

FROM_STRING_FUNC(FreqType, ENUM_LIST_TYPE)
AS_STRING_FUNC(FreqType, ENUM_LIST_TYPE)

static int int_cmp( int a, int b );

/*
 * Destroys all sub-FreqSpecs in a composite FreqSpec.
 * Assertion error if it's not a COMPOSITE FreqSpec.
 */
void xaccFreqSpecCompositesClear( FreqSpec *fs );

void subSpecsListMapDelete( gpointer data, gpointer user_data );


static const char *
get_wday_name(guint day)
{
  static gchar wday_name[WDAY_BUF_WIDTH];
  struct tm t;
  memset( &t, 0, sizeof( t ) );
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
  memset( &t, 0, sizeof( t ) );
  t.tm_mon = month;
  strftime(month_name, WDAY_NAME_WIDTH, "%b", &t);
  return month_name;
}

/*
 * Initializes a FreqSpec by setting it's to type INVALID.
 * Use this to initialise a stack object.
 * FreqSpec objects must be initalised before being used by
 * any other method.
 */

static void
xaccFreqSpecInit( FreqSpec *fs, QofBook *book )
{
   QofCollection *col;
   g_return_if_fail( fs );
   g_return_if_fail (book);

   col = qof_book_get_collection (book, QOF_ID_FREQSPEC);
   qof_entity_init (&fs->entity, QOF_ID_FREQSPEC, col);

   fs->type = INVALID;
   fs->uift = UIFREQ_ONCE;

   memset( &(fs->s), 0, sizeof(fs->s) );
}

FreqSpec*
xaccFreqSpecMalloc(QofBook *book)
{
   FreqSpec   *fs;

   g_return_val_if_fail (book, NULL);

   fs = g_new0(FreqSpec, 1);
   xaccFreqSpecInit( fs, book );
   qof_event_gen( &fs->entity, QOF_EVENT_CREATE , NULL);
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
   qof_event_gen( &fs->entity, QOF_EVENT_DESTROY , NULL);
   xaccFreqSpecCleanUp( fs );

   qof_entity_release (&fs->entity);
   g_free( fs );
}

FreqType
xaccFreqSpecGetType( FreqSpec *fs )
{
   g_return_val_if_fail( fs, INVALID );
   /* Is this really a fail? */
   //g_return_val_if_fail( fs->type != INVALID, INVALID );
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
xaccFreqSpecGetNextInstance( FreqSpec *fs,
              const GDate* in_date,
              GDate* out_date )
{
   GList *list;

   g_return_if_fail( fs );
   g_return_if_fail( in_date );
   g_return_if_fail( out_date );
   switch( fs->type ) {
   case INVALID:
      /* this is okay, just lame. */
      g_date_clear( out_date, 1 );
      break;

   case ONCE:
      if ( g_date_compare( &(fs->s.once.date), in_date ) > 0 ) 
      {
         *out_date = fs->s.once.date;
      } else {
         /* Date is past due. Return an invalid date. */
         g_date_clear( out_date, 1 );
      }
      break;

   case DAILY: {
      guint32 julian_in_date, julian_next_repeat, complete_intervals;

      julian_in_date = g_date_get_julian( in_date );
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

      julian_in_date = g_date_get_julian( in_date );
      complete_intervals =
         (julian_in_date - fs->s.weekly.offset_from_epoch) /
         (fs->s.weekly.interval_weeks * 7);
      julian_next_repeat =
         fs->s.weekly.offset_from_epoch +
         (complete_intervals + 1) * fs->s.weekly.interval_weeks * 7;
      g_date_set_julian( out_date, julian_next_repeat );
   } break;

   case MONTHLY: {
      guint32 in_months_from_epoch, after_repeat_in_month_interval,
         complete_intervals, next_repeat_months_from_epoch, month, year;

      in_months_from_epoch = (g_date_get_year( in_date )-1) * 12 +
         g_date_get_month( in_date ) - 1;
      complete_intervals =
         (in_months_from_epoch - fs->s.monthly.offset_from_epoch) /
         fs->s.monthly.interval_months;
      after_repeat_in_month_interval =
         (g_date_get_day( in_date ) >= fs->s.monthly.day_of_month ||
          (in_months_from_epoch - fs->s.monthly.offset_from_epoch) %
          fs->s.monthly.interval_months > 0 ||
          g_date_get_day( in_date ) >=
          g_date_get_days_in_month( g_date_get_month( in_date ),
                      g_date_get_year( in_date ) ) )  ? 1 : 0;
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
                 g_date_get_days_in_month( month, year ) ),
            month,
            year );
   } break;
   
   case MONTH_RELATIVE: {
      guint32 in_months_from_epoch, after_repeat_in_month_interval,
         complete_intervals, next_repeat_months_from_epoch, month, year,
         wday_of_1st, day_of_repeat;

      GDate date1;
      in_months_from_epoch = (g_date_get_year( in_date )-1) * 12 +
         g_date_get_month( in_date ) - 1;
      complete_intervals =
         (in_months_from_epoch - fs->s.month_relative.offset_from_epoch) /
         fs->s.month_relative.interval_months;
      month = g_date_get_month( in_date );
      year = g_date_get_year( in_date );
      g_date_set_dmy( &date1, 1, month, year );
      wday_of_1st = g_date_get_weekday( &date1 );
      day_of_repeat = (fs->s.month_relative.occurrence-1)*7 +
         ((fs->s.month_relative.weekday + 7 - wday_of_1st)%7 + 1);
      after_repeat_in_month_interval =
         (g_date_get_day( in_date ) >= day_of_repeat ||
          day_of_repeat > g_date_get_days_in_month( month, year ) ||
          (in_months_from_epoch - fs->s.month_relative.offset_from_epoch) %
          fs->s.month_relative.interval_months > 0 )  ? 1 : 0;
      next_repeat_months_from_epoch =
         fs->s.month_relative.offset_from_epoch +
         (complete_intervals + after_repeat_in_month_interval) *
         fs->s.month_relative.interval_months;
      month = next_repeat_months_from_epoch % 12 + 1;
      year = next_repeat_months_from_epoch / 12 + 1;
      g_date_set_dmy( &date1, 1, month, year );
      wday_of_1st = g_date_get_weekday( &date1 );
      /* This calculates the day of the month in the month which forms
       * the next month in the cycle after the given input date.
       * However, this day may be larger than the number of days in
       * that month... */
      day_of_repeat = (fs->s.month_relative.occurrence-1)*7 +
         ((fs->s.month_relative.weekday + 7 - wday_of_1st)%7 + 1);
      while( day_of_repeat > g_date_get_days_in_month( month, year ) ) {
         /* If the repeat occurs after the end of the month, then find
          * the next month containing a day which satisfies the
          * request.  Each candiate month separated by interval_months
          * is considered by this loop.*/
         ++complete_intervals;
         next_repeat_months_from_epoch =
            fs->s.month_relative.offset_from_epoch +
            complete_intervals * fs->s.month_relative.interval_months;
         month = next_repeat_months_from_epoch % 12 + 1;
         year = next_repeat_months_from_epoch / 12 + 1;
         g_date_set_dmy( &date1, 1, month, year );
         wday_of_1st = g_date_get_weekday( &date1 );
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
            this_julian = g_date_get_julian( &next_repeat );
               
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
xaccFreqSpecSetNone( FreqSpec *fs )
{
        g_return_if_fail( fs );
        xaccFreqSpecCleanUp( fs );
        fs->type = INVALID;
}

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

   julian_days_since_epoch = g_date_get_julian( initial_date );
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

   julian_days_since_epoch = g_date_get_julian( initial_date );
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

   julian_days_since_epoch = g_date_get_julian( initial_date );
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

   julian_day_initial = g_date_get_julian( initial_date );
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

   months_since_epoch = (g_date_get_year( initial_date )-1) * 12 +
      g_date_get_month( initial_date ) - 1;
   fs->s.monthly.offset_from_epoch = months_since_epoch % interval_months;
   fs->s.monthly.day_of_month = g_date_get_day( initial_date );

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

   months_since_epoch = (g_date_get_year( initial_date )-1) * 12 +
      g_date_get_month( initial_date ) - 1;
   fs->s.month_relative.offset_from_epoch = months_since_epoch % interval_months;
   
   fs->s.month_relative.weekday = g_date_get_weekday( initial_date );
   fs->s.month_relative.occurrence = (g_date_get_day( initial_date )-1) / 7 + 1;
   
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
    /* This is displayed instead of the number of the day of month. */
    g_string_printf(str, _( "last day"));
  }
  else
  {
    g_string_printf(str, "%u", dom);
  }

  return str;
}

static const char*
qofFreqSpecPrintable (gpointer obj)
{
	FreqSpec *fs;
	GString  *str;

	fs = (FreqSpec*)obj;
	g_return_val_if_fail(fs != NULL, NULL);
	str = g_string_new("");
	xaccFreqSpecGetFreqStr(fs, str);
	return str->str;
}

void
xaccFreqSpecGetFreqStr( FreqSpec *fs, GString *str )
{
   GList *list;
   FreqSpec *tmpFS;
   int tmpInt;
   char *tmpStr;
   int i;
#define MAX_FREQ_STR_SIZE 127
   char freqStrBuf[ MAX_FREQ_STR_SIZE + 1];

   memset( freqStrBuf, 0, MAX_FREQ_STR_SIZE + 1 );

   switch( xaccFreqSpecGetUIType( fs ) ) {
   case UIFREQ_NONE:
     snprintf( freqStrBuf, MAX_FREQ_STR_SIZE, _("None") );
     break;

   case UIFREQ_ONCE:
      tmpStr = g_new0( char, GDATE_STRING_BUF_SIZE );
      /* this is now a GDate. */
      g_date_strftime( tmpStr, GDATE_STRING_SIZE,
             GNC_D_FMT,
             &fs->s.once.date );
      /* %s is the strftime-string of the one-time date. */
      snprintf( freqStrBuf, MAX_FREQ_STR_SIZE, _("Once: %s"), tmpStr );
      g_free( tmpStr );
      break;

   case UIFREQ_DAILY:
      if ( fs->s.daily.interval_days > 1 ) 
      {
         snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
              /* %u is the number of intervals */
              _("Daily (x%u)"),
              fs->s.daily.interval_days );
      }
      else 
      {
         snprintf( freqStrBuf, MAX_FREQ_STR_SIZE, _("Daily") );
      }
      break;

   case UIFREQ_DAILY_MF:
   { 
      FreqSpec *subFS;
      if ( g_list_length( fs->s.composites.subSpecs ) != 5 ) {
        g_critical("Invalid Daily[M-F] structure");
        snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
                  "Daily[M-F]: error" );
        return;
      }
      /* We assume that all of the weekly FreqSpecs that make up
         the Daily[M-F] FreqSpec have the same interval. */
      subFS = (FreqSpec*)fs->s.composites.subSpecs->data;
          
      if ( subFS->s.weekly.interval_weeks > 1 ) {
        snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
             /* %u is the number of intervals */
             _("Weekdays: (x%u)"),
             subFS->s.weekly.interval_weeks );
      }
      else
      {
        snprintf(freqStrBuf, MAX_FREQ_STR_SIZE, _("Weekdays"));
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
            snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
                 "error: UIFREQ_WEEKLY doesn't contain weekly children" );
            g_free( tmpStr );
            return;
         }
         if ( tmpInt == -1 ) {
            tmpInt = tmpFS->s.weekly.interval_weeks;
         }
         /* put the first letter of the weekday name in
            the appropriate position. */
         dowIdx = tmpFS->s.weekly.offset_from_epoch % 7;
         tmpStr[dowIdx] = *(get_wday_name(dowIdx));
      }

      if ( tmpInt > 1 ) {
        snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
             /* %d are the number of intervals; %s is
                the name of the weekday */
             _( "Weekly (x%d): %s"), tmpInt, tmpStr );
      }
      else
      {
        snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
             /* Translators: %s is the name of the weekday */
             _( "Weekly: %s"), tmpStr );
      }
      g_free( tmpStr );
      break;

   case UIFREQ_BI_WEEKLY:
     /* %s is the name of the weekday */
     snprintf( freqStrBuf, MAX_FREQ_STR_SIZE, _("Bi-Weekly, %ss"), 
          get_wday_name(fs->s.weekly.offset_from_epoch % 7) );
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
       snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
            /* Translators: %u is the number of intervals;
          %s is the day of month of the starting month
          (or the string "last day"); %s is the day of
          month of the ending month  */
            _("Semi-monthly (x%u): %s, %s"), 
            tmpFS->s.monthly.interval_months,
            first_dom->str, 
            second_dom->str);
     }
     else
     {
       snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
            /* Translators: %s is the day of month of the
          starting month (or the string "last day"); %s
          is the day of month of the ending month  */
            _("Semi-monthly: %s, %s"), 
            first_dom->str,
            second_dom->str);
     }
     g_string_free(first_dom, TRUE);
     g_string_free(second_dom, TRUE);
       
     break;
   }

   case UIFREQ_MONTHLY:
          
      if ( fs->s.monthly.interval_months > 1 ) {
        snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
             /* %u is the number of intervals; %u is
                the day of month  */
             _("Monthly (x%u): %u"),
             fs->s.monthly.interval_months,
             fs->s.monthly.day_of_month);
      }
      else
      {
        snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
             /* %u is the day of month  */
             _("Monthly: %u"),
             fs->s.monthly.day_of_month );
      }
      break;

   case UIFREQ_QUARTERLY:
      if ( fs->s.monthly.interval_months != 3 ) {
        snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
             /* %u is the number of intervals; %u is
                the day of month  */
             _("Quarterly (x%u): %u"),
             fs->s.monthly.interval_months/3,
             fs->s.monthly.day_of_month);
      }
      else
      {
        snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
             /* %u is the day of month  */
             _("Quarterly: %u"),
             fs->s.monthly.day_of_month );
      }
      break;

   case UIFREQ_TRI_ANUALLY:

      if ( fs->s.monthly.interval_months != 4 ) {
        snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
             /* %u is the number of intervals; %u is
                the day of month  */
             _("Tri-Yearly (x%u): %u"),
             fs->s.monthly.interval_months/4,
             fs->s.monthly.day_of_month);
      }
      else
      {
        snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
             /* %u is the day of month  */
             _("Tri-Yearly: %u"),
             fs->s.monthly.day_of_month );
      }
      break;

   case UIFREQ_SEMI_YEARLY:
      if ( fs->s.monthly.interval_months != 6 ) {
         if ( (fs->s.monthly.interval_months % 6) != 0 ) {
              g_critical( "FreqSpec Semi-Yearly month-interval "
                          "is not a multiple of 6 [%d]",
                          fs->s.monthly.interval_months );
         }
         snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
              /* %u is the number of intervals; %u
                 is the day of month  */
              _("Semi-Yearly (x%u): %u"),
              fs->s.monthly.interval_months/6,
              fs->s.monthly.day_of_month);
      }
      else
      {
        snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
             /* %u is the day of month  */
             _("Semi-Yearly: %u"),
             fs->s.monthly.day_of_month );
      }
      break;

   case UIFREQ_YEARLY:
      if ( fs->s.monthly.interval_months != 12 ) {
         if ( (fs->s.monthly.interval_months % 12) != 0 ) {
              g_critical( "Yearly FreqSpec month-interval "
                          "is not a multiple of 12 [%d]",
                          fs->s.monthly.interval_months );
         }

         snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
              /* FIXME: This string *must* be translated for
                 en_GB, en_AU and everywhere else with the
                 sensible ordering of ddmmyy.  Translators
                 note: to switch the last two arguments,
                 write "Yearly (x%1$u): %3$u of month %2$s"
          
                 %u is the number of intervals; %s is the
                 abbreviated name of the month; %u is the
                 day of month. */
              _("Yearly (x%u): %s/%u"),
              fs->s.monthly.interval_months/12,
              get_abbrev_month_name(fs->s.monthly.offset_from_epoch),
              fs->s.monthly.day_of_month);
      }
      else
      {
        snprintf( freqStrBuf, MAX_FREQ_STR_SIZE,
             /* %s is the abbreviated name of the
                month; %u is the day of month  */
             _("Yearly: %s/%u"),
             get_abbrev_month_name(fs->s.monthly.offset_from_epoch),
             fs->s.monthly.day_of_month );
      }
      break;

   default:
      snprintf( freqStrBuf, MAX_FREQ_STR_SIZE, _("Unknown") );
      break;
   }
   g_string_printf( str, "%s", freqStrBuf );
}

static
int
int_cmp( int a, int b )
{
   if ( a < b )
      return -1;
   if ( a == b )
      return 0;
   return 1;
}

/*
 * Returns the "min" FreqSpec sub-element of a composite FreqSpec.
 */
static
FreqSpec*
_gnc_freq_spec_get_min( FreqSpec *fs )
{
   FreqSpec *toRet, *tmpFS;
   GList *l;

   g_assert( xaccFreqSpecGetType(fs) == COMPOSITE );
   toRet = NULL;
   for ( l = xaccFreqSpecCompositeGet(fs);
         l;
         l = l->next ) {
      tmpFS = (FreqSpec*)l->data;

      if ( toRet == NULL ) {
         toRet = tmpFS;
         continue;
      }

      if ( gnc_freq_spec_compare( toRet, tmpFS ) > 0 ) {
         toRet = tmpFS;
      }
   }
   return toRet;
}

int
gnc_freq_spec_compare( FreqSpec *a, FreqSpec *b )
{
   FreqType fta, ftb;
   int tmpInt;

   if ( ! (a && b) ) {
      return 0;
   } else if ( !a && b ) {
      return 1;
   } else if ( a && !b ) {
      return -1;
   } /* else { this else intentionally left blank; both-valid code is
      * below. } */

   fta = xaccFreqSpecGetType( a );
   ftb = xaccFreqSpecGetType( b );

   if ( fta == COMPOSITE ) {
      a = _gnc_freq_spec_get_min( a );
      fta = xaccFreqSpecGetType( a );
   }
   if ( ftb == COMPOSITE ) {
      b = _gnc_freq_spec_get_min( b );
      ftb = xaccFreqSpecGetType( b );
   }

   if ( fta < ftb ) {
      return -1;
   } else if ( fta > ftb ) {
      return 1;
   } /* else { this else intentionally left blank; '='-case code is
      * below. */

   switch ( fta /* == ftb */ ) {
   case INVALID:
      return 0;
      break;
   case ONCE:
      return g_date_compare( &a->s.once.date,
                   &b->s.once.date );
      break;
   case DAILY:
      tmpInt = int_cmp( a->s.daily.interval_days,
              b->s.daily.interval_days );
      if ( tmpInt != 0 ) {
         return tmpInt;
      }
      return int_cmp( a->s.daily.offset_from_epoch,
            b->s.daily.offset_from_epoch );
      break;
   case WEEKLY:
      tmpInt = int_cmp( a->s.weekly.interval_weeks,
              b->s.weekly.interval_weeks );
      if ( tmpInt != 0 ) {
         return tmpInt;
      }
      return int_cmp( a->s.weekly.offset_from_epoch,
            b->s.weekly.offset_from_epoch );
      break;
   case MONTHLY:
      tmpInt = int_cmp( a->s.monthly.interval_months,
              b->s.monthly.interval_months );
      if ( tmpInt != 0 ) {
         return tmpInt;
      }
      return int_cmp( a->s.monthly.day_of_month, 
            b->s.monthly.day_of_month );
      break;
   case MONTH_RELATIVE:
      g_error( "MONTH-RELATIVE dates not supported." );
      break;
   case COMPOSITE:
      /* We shouldn't see a composite after doing the
       * composite-reduction above. */
      g_error( "This code should not be reached." );
      break;
   default:
      g_error( "Unknown freqspec type %d", fta );
      break;
   }
   return 0;
}

/*  QOF routines. */

static int
qofFreqSpecGetMonthDay(FreqSpec *fs)
{
	int outDayOfMonth;

	outDayOfMonth = 0;
	if ( fs->type != MONTHLY ) { return outDayOfMonth; }
	outDayOfMonth = fs->s.monthly.day_of_month;
	return outDayOfMonth;
}
/*
static int
qofFreqSpecGetMonthOffset(FreqSpec *fs)
{
	int outMonthOffset;

	outMonthOffset = 0;
	if ( fs->type != MONTHLY ) { return outMonthOffset; }
	outMonthOffset = fs->s.monthly.offset_from_epoch;
	return outMonthOffset;
}
*/
static Timespec
qofFreqSpecGetBaseDate(FreqSpec *fs)
{
	GDate       *when;
	struct tm   number;
	time_t      start_t;
	Timespec ts = {0,0};

	g_return_val_if_fail( fs != NULL , ts);
	when = g_date_new();
	if(xaccFreqSpecGetOnce(fs, when) == -1) { return ts; }
	g_date_to_struct_tm(when, &number);
	start_t = mktime(&number);
	timespecFromTime_t(&ts, start_t);
	return ts;
}

static char*
qofFreqSpecGetUIType(FreqSpec *fs)
{
	char *type_string;

	g_return_val_if_fail(fs, NULL);
	type_string = g_strdup(UIFreqTypeasString(fs->uift));
	return type_string;
}

static int
qofFreqSpecGetRepeat(FreqSpec *fs)
{
	int repeat, dump, dump2;

	g_return_val_if_fail(fs != NULL, -1);
	repeat = -1;
	dump = dump2 = 0;
	switch(xaccFreqSpecGetType(fs))
	{
		case INVALID: {
			break;
		}
		case ONCE: {
			repeat = 0;
			break;
		}
		case DAILY: {
			xaccFreqSpecGetDaily(fs, &repeat);
			break;
		}
		case WEEKLY: {
			xaccFreqSpecGetWeekly(fs, &repeat, &dump);
			break;
		}
		case MONTHLY: {
			xaccFreqSpecGetMonthly(fs, &repeat, &dump, &dump2);
			break;
		}
		case MONTH_RELATIVE: {
			repeat = 0;
			break;
		}
		case COMPOSITE: {
			repeat = 0;
			break;
		}
		default: {
			break;
		}
	}
	return repeat;
}

/* QOF set routines - may look a little strange as QOF can set parameters in any order. */
/* Initial state:  UIFREQ_ONCE, INVALID, union s memset to zero and value == 0 */

static void 
qofFreqSpecCalculate(FreqSpec *fs, gint value)
{
	GDate *when;

	g_return_if_fail(fs != NULL);
	/* If it's INVALID, nothing can be done until more data is set. */
	if(xaccFreqSpecGetType(fs) == INVALID) { return; }
	/* If it's still UIFREQ_ONCE, nothing needs to be done */
	if(xaccFreqSpecGetUIType(fs) == UIFREQ_ONCE) { return; }
	/* If value is zero, nothing needs to be done. */
	if(value == 0) { return; }
	when = g_date_new();
	xaccFreqSpecGetOnce(fs, when);
	switch (xaccFreqSpecGetUIType(fs)) {
		case UIFREQ_NONE : {
			xaccFreqSpecSetNone(fs);
		}
		break;
		case UIFREQ_ONCE : {
			/*  should be impossible but just to be sure. */
			break;
		}
		case UIFREQ_DAILY : {
			xaccFreqSpecSetDaily(fs, when, value);
			break;
		}
		case UIFREQ_DAILY_MF : {
			
			break;
		}
		case UIFREQ_WEEKLY : {
			xaccFreqSpecSetWeekly(fs, when, value);
			break;
		}
		case UIFREQ_BI_WEEKLY : {
			
			break;
		}
		case UIFREQ_SEMI_MONTHLY : {
			
			break;
		}
		case UIFREQ_MONTHLY : {
			 xaccFreqSpecSetMonthly(fs, when, value);
			break;
		}
		case UIFREQ_QUARTERLY : {
			
			break;
		}
		case UIFREQ_TRI_ANUALLY : {
			
			break;
		}
		case UIFREQ_SEMI_YEARLY : {
			
			break;
		}
		case UIFREQ_YEARLY : {
			
			break;
		}
		default: { break; }
	}
}

static void
qofFreqSpecSetUIType (FreqSpec *fs, const char *type_string)
{
	g_return_if_fail(fs != NULL);
	xaccFreqSpecSetUIType(fs, UIFreqTypefromString(type_string));
	qofFreqSpecCalculate(fs, fs->value);
}

static void
qofFreqSpecSetBaseDate(FreqSpec *fs, Timespec start_date)
{
	time_t      start_t;
	FreqType    type;
	GDate       *when;

	g_return_if_fail( fs != NULL );
	when = g_date_new();
	type = xaccFreqSpecGetType(fs);
	start_t = timespecToTime_t(start_date);
	g_date_set_time_t(when, start_t);
	/* QOF sets this before a type is assigned. */
	if(type == INVALID) {
		fs->type = ONCE;
	}
	xaccFreqSpecSetOnceDate(fs, when);
	/* Now we have a GDate available for the calculation. */
	qofFreqSpecCalculate(fs, fs->value);
}

static void
qofFreqSpecSetRepeat(FreqSpec *fs, gint value)
{
	fs->value = value;
	qofFreqSpecCalculate(fs, value);
}

static QofObject FreqSpecDesc = 
{
	interface_version : QOF_OBJECT_VERSION,
	e_type            : QOF_ID_FREQSPEC,
	type_label        : "Frequency Specification",
	create            : (gpointer)xaccFreqSpecMalloc,
	book_begin        : NULL,
	book_end          : NULL,
	is_dirty          : NULL,
	mark_clean        : NULL,
	foreach           : qof_collection_foreach,
	printable         : qofFreqSpecPrintable,
	version_cmp       : (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean FreqSpecRegister (void)
{
	static QofParam params[] = {
	 { FS_UI_TYPE, QOF_TYPE_STRING, (QofAccessFunc)qofFreqSpecGetUIType, (QofSetterFunc)qofFreqSpecSetUIType },
	 { FS_REPEAT, QOF_TYPE_INT64, (QofAccessFunc)qofFreqSpecGetRepeat, (QofSetterFunc)qofFreqSpecSetRepeat },
	 { FS_BASE_DATE, QOF_TYPE_DATE, (QofAccessFunc)qofFreqSpecGetBaseDate, 
		 (QofSetterFunc)qofFreqSpecSetBaseDate },
	 { FS_MONTH_DAY, QOF_TYPE_STRING, (QofAccessFunc)qofFreqSpecGetMonthDay, NULL },
	 { QOF_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
	 { QOF_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
	 { NULL },
	};
	qof_class_register(QOF_ID_FREQSPEC, (QofSortFunc)gnc_freq_spec_compare, params);
	return qof_object_register(&FreqSpecDesc);
}
