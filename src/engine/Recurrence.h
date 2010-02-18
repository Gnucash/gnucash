/* Recurrence.h:
 *
 *   A Recurrence represents the periodic occurrence of dates, with a
 *   beginning point.  For example, "Every Friday, beginning April 15,
 *   2005" or "The 1st of every 3rd month, beginning April 1, 2001."
 *
 *   Technically, a Recurrence can also represent certain useful
 *   "almost periodic" date sequences.  For example, "The last day of
 *   every month, beginning Feb. 28, 2005."
 *
 *   The main operation you can perform on a Recurrence is to find the
 *   earliest date in the sequence of occurrences that is after some
 *   specified date (often the "previous" occurrence).
 *
 *   In addition, you can use a GList of Recurrences to represent a
 *   sequence containing all the dates in each Recurrence in the list,
 *   and perform the same "next instance" computation for this
 *   sequence.
 *
 * Copyright (C) 2005, Chris Shoemaker <c.shoemaker@cox.net>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef RECURRENCE_H
#define RECURRENCE_H

#include <glib.h>
#include "Account.h"
#include "gnc-numeric.h"

typedef enum
{
    PERIOD_ONCE,         /* Not a true period at all, but convenient here. */
    PERIOD_DAY,
    PERIOD_WEEK,
    PERIOD_MONTH,
    PERIOD_END_OF_MONTH, /* This is actually a period plus a phase. */
    PERIOD_NTH_WEEKDAY,  /* Also a phase, e.g. Second Tueday.       */
    PERIOD_LAST_WEEKDAY, /* Also a phase. */
    PERIOD_YEAR,
    NUM_PERIOD_TYPES,
    PERIOD_INVALID = -1,
} PeriodType;

typedef enum
{
    WEEKEND_ADJ_NONE,
    WEEKEND_ADJ_BACK,    /* Previous weekday */
    WEEKEND_ADJ_FORWARD, /* Next weekday */
    NUM_WEEKEND_ADJS,
    WEEKEND_ADJ_INVALID = -1,
} WeekendAdjust;

/* Recurrences represent both the phase and period of a recurring event. */

typedef struct
{
    GDate start;         /* First date in the recurrence; specifies phase. */
    PeriodType ptype;    /* see PeriodType enum */
    guint16 mult;        /* a period multiplier */
    WeekendAdjust wadj;  /* see WeekendAdjust enum */
} Recurrence;


/* recurrenceSet() will enforce internal consistency by overriding
   inconsistent inputs so that 'r' will _always_ end up being a valid
   recurrence.

     - if the period type is invalid, PERIOD_MONTH is used.

     - if the period type is PERIOD_ONCE, then mult is ignored,
       otherwise, if mult is zero, then mult of 1 is used.

     - if the date is invalid, the current date is used.

     - if the period type specifies phase, the date is made to agree
       with that phase:

         - for PERIOD_END_OF_MONTH, the last day of date's month is used.

         - for PERIOD_NTH_WEEKDAY, a fifth weekday converts to a
           PERIOD_LAST_WEEKDAY

         - for PERIOD_LAST_WEEKDAY, the last day in date's month with
           date's day-of-week is used.

*/
void recurrenceSet(Recurrence *r, guint16 mult, PeriodType pt,
                   const GDate *date, WeekendAdjust wadj);

/* get the fields */
PeriodType recurrenceGetPeriodType(const Recurrence *r);
guint recurrenceGetMultiplier(const Recurrence *r);
GDate recurrenceGetDate(const Recurrence *r);
WeekendAdjust recurrenceGetWeekendAdjust(const Recurrence *r);

/* Get the occurence immediately after refDate.
 *
 * This function has strict and precise post-conditions:
 *
 * Given a valid recurrence and a valid 'refDate', 'nextDate' will be
 * *IN*valid IFF the period_type is PERIOD_ONCE, and 'refDate' is
 * later-than or equal to the single occurrence (start_date).
 *
 * A valid 'nextDate' will _always_ be:
 *    - strictly later than the 'refDate', AND
 *    - later than or equal to the start date of the recurrence, AND
 *    - exactly an integral number of periods away from the start date
 *
 * Furthermore, there will be no date _earlier_ than 'nextDate' for
 * which the three things above are true.
 *
 */
void recurrenceNextInstance(const Recurrence *r, const GDate *refDate,
                            GDate *nextDate);

/* Zero-based.  n == 1 gets the instance after the start date. */
void recurrenceNthInstance(const Recurrence *r, guint n, GDate *date);

/* Get a time coresponding to the beginning (or end if 'end' is true)
   of the nth instance of the recurrence. Also zero-based. */
time_t recurrenceGetPeriodTime(const Recurrence *r, guint n, gboolean end);

/**
 * @return the amount that an Account's value changed between the beginning
 * and end of the nth instance of the Recurrence.
 **/
gnc_numeric recurrenceGetAccountPeriodValue(const Recurrence *r,
        Account *acct, guint n);

/** @return the earliest of the next occurances -- a "composite" recurrence **/
void recurrenceListNextInstance(const GList *r, const GDate *refDate,
                                GDate *nextDate);

/* These four functions are only for xml storage, not user presentation. */
gchar *recurrencePeriodTypeToString(PeriodType pt);
PeriodType recurrencePeriodTypeFromString(const gchar *str);
gchar *recurrenceWeekendAdjustToString(WeekendAdjust wadj);
WeekendAdjust recurrenceWeekendAdjustFromString(const gchar *str);

/* For debugging.  Caller owns the returned string.  Not intl. */
gchar *recurrenceToString(const Recurrence *r);
gchar *recurrenceListToString(const GList *rlist);

/** @return True if the recurrence list is a common "semi-monthly" recurrence. **/
gboolean recurrenceListIsSemiMonthly(GList *recurrences);
/** @return True if the recurrence list is a common "weekly" recurrence. **/
gboolean recurrenceListIsWeeklyMultiple(GList *recurrences);

/**
 * Pretty-print an intentionally-short summary of the period of a (GList of)
 * Recurrences, as might be commonly-created by the GncFrequency widget.  In
 * particular, this routine expects most lists to contain a single
 * Recurrence, but also anticipates 2 "composite" scenarios:
 *
 * @li A list of N PERIOD_WEEK Recurrences.
 * @li A list of 2 PERIOD_MONTH or PERIOD_LAST_WEEKDAY Recurrences,
 *   representing a Semi-Monthly period.
 *
 * @return A caller-owned string.
 **/
gchar *recurrenceListToCompactString(GList *recurrence_list);

/** @return integer representing the relationship between @a a and @a b, with the semantics of qsort. **/
int recurrenceCmp(Recurrence *a, Recurrence *b);
int recurrenceListCmp(GList *a, GList *b);

void recurrenceListFree(GList **recurrence);

#endif  /* RECURRENCE_H */
