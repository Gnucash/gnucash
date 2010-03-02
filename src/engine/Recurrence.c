/* Copyright (C) 2005, Chris Shoemaker <c.shoemaker@cox.net>
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

#include "config.h"
#include <time.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <string.h>
#include "Recurrence.h"
#include "gnc-date.h"
#include "qof.h"
#include "gnc-engine.h"
#include "gnc-gdate-utils.h"
#include "Account.h"

#define LOG_MOD "gnc.engine.recurrence"
static QofLogModule log_module = LOG_MOD;
#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN LOG_MOD

static GDate invalid_gdate;

/* Do not intl. These are used for xml storage. */
static gchar *period_type_strings[NUM_PERIOD_TYPES] =
{
    "once", "day", "week", "month", "end of month",
    "nth weekday", "last weekday", "year",
};
static gchar *weekend_adj_strings[NUM_WEEKEND_ADJS] =
{
    "none", "back", "forward",
};

#define VALID_PERIOD_TYPE(pt)    ((0 <= (pt)) && ((pt) < NUM_PERIOD_TYPES))
#define VALID_WEEKEND_ADJ(wadj)  ((0 <= (wadj)) && ((wadj) < NUM_WEEKEND_ADJS))

PeriodType
recurrenceGetPeriodType(const Recurrence *r)
{
    return r ? r->ptype : PERIOD_INVALID;
}

guint
recurrenceGetMultiplier(const Recurrence *r)
{
    return r ? r->mult : 0;
}

GDate
recurrenceGetDate(const Recurrence *r)
{
    return r ? r->start : invalid_gdate;
}

WeekendAdjust
recurrenceGetWeekendAdjust(const Recurrence *r)
{
    return r ? r->wadj : WEEKEND_ADJ_INVALID;
}

void
recurrenceSet(Recurrence *r, guint16 mult, PeriodType pt, const GDate *_start, WeekendAdjust wadj)
{
    r->ptype = VALID_PERIOD_TYPE(pt) ? pt : PERIOD_MONTH;
    r->mult = (pt == PERIOD_ONCE) ? 0 : (mult > 0 ? mult : 1);

    if (_start && g_date_valid(_start))
    {
        r->start = *_start;
    }
    else
    {
        g_date_set_time_t(&r->start, time(NULL));
    }

    /* Some of the unusual period types also specify phase.  For those
       types, we ensure that the start date agrees with that phase. */
    switch (r->ptype)
    {
    case PERIOD_END_OF_MONTH:
        g_date_set_day(&r->start, g_date_get_days_in_month
                       (g_date_get_month(&r->start),
                        g_date_get_year(&r->start)));
        break;
    case PERIOD_LAST_WEEKDAY:
    {
        GDateDay dim;
        dim = g_date_get_days_in_month(g_date_get_month(&r->start),
                                       g_date_get_year(&r->start));
        while (dim - g_date_get_day(&r->start) >= 7)
            g_date_add_days(&r->start, 7);
    }
    break;
    case PERIOD_NTH_WEEKDAY:
        if ((g_date_get_day(&r->start) - 1) / 7 == 4) /* Fifth week */
            r->ptype = PERIOD_LAST_WEEKDAY;
        break;
    default:
        break;
    }

    switch (r->ptype)
    {
    case PERIOD_MONTH:
    case PERIOD_END_OF_MONTH:
    case PERIOD_YEAR:
        r->wadj = wadj;
        break;
    default:
        r->wadj = WEEKEND_ADJ_NONE;
        break;
    }
}

/* nth_weekday_compare() is a helper function for the
   PERIOD_{NTH,LAST}_WEEKDAY case.  It returns the offset, in days,
   from 'next' to the nth weekday specified by the 'start' date (and
   the period type), in the same month as 'next'.  A negative offset
   means earlier than 'next'; a zero offset means 'next' *is* the nth
   weekday in that month; a positive offset means later than
   'next'. */
static gint
nth_weekday_compare(const GDate *start, const GDate *next, PeriodType pt)
{
    GDateDay sd, nd;
    gint matchday, dim;

    nd = g_date_get_day(next);
    sd = g_date_get_day(start);

    /* matchday has a week part, capped at 3 weeks, and a day part,
       capped at 7 days, so max(matchday) == 3*7 + 7 == 28. */
    matchday = 7 * ((sd - 1) / 7 == 4 ? 3 : (sd - 1) / 7) +
               (nd - g_date_get_weekday(next) + g_date_get_weekday(start) + 7) % 7;
    /* That " + 7" is to avoid negative modulo in case nd < 6. */

    dim = g_date_get_days_in_month(
              g_date_get_month(next), g_date_get_year(next));
    if ((dim - matchday) >= 7 && pt == PERIOD_LAST_WEEKDAY)
        matchday += 7;     /* Go to the fifth week, if needed */

    return matchday - nd;  /* Offset from 'next' to matchday */
}


/* This is the only real algorithm related to recurrences.  It goes:
   Step 1) Go forward one period from the reference date.
   Step 2) Back up to align to the phase of the start date.
*/
void
recurrenceNextInstance(const Recurrence *r, const GDate *ref, GDate *next)
{
    PeriodType pt;
    const GDate *start;
    guint mult;
    WeekendAdjust wadj;

    g_return_if_fail(r);
    g_return_if_fail(ref);
    g_return_if_fail(g_date_valid(&r->start));
    g_return_if_fail(g_date_valid(ref));

    /* If the ref date comes before the start date then the next
       occurrence is always the start date, and we're done. */
    start = &r->start;
    if (g_date_compare(ref, start) < 0)
    {
        g_date_set_julian(next, g_date_get_julian(start));
        return;
    }
    g_date_set_julian(next, g_date_get_julian(ref)); /* start at refDate */

    /* Step 1: move FORWARD one period, passing exactly one occurrence. */
    mult = r->mult;
    pt = r->ptype;
    wadj = r->wadj;
    switch (pt)
    {
    case PERIOD_YEAR:
        mult *= 12;             /* fall-through */
    case PERIOD_MONTH:
    case PERIOD_NTH_WEEKDAY:
    case PERIOD_LAST_WEEKDAY:
    case PERIOD_END_OF_MONTH:
        /* Takes care of short months. */
        if (r->wadj == WEEKEND_ADJ_BACK &&
                (pt == PERIOD_YEAR || pt == PERIOD_MONTH || pt == PERIOD_END_OF_MONTH) &&
                (g_date_get_weekday(next) == G_DATE_SATURDAY || g_date_get_weekday(next) == G_DATE_SUNDAY))
        {
            /* Allows the following Friday-based calculations to proceed if 'next'
               is between Friday and the target day. */
            g_date_subtract_days(next, g_date_get_weekday(next) == G_DATE_SATURDAY ? 1 : 2);
        }
        if (r->wadj == WEEKEND_ADJ_BACK &&
                (pt == PERIOD_YEAR || pt == PERIOD_MONTH || pt == PERIOD_END_OF_MONTH) &&
                g_date_get_weekday(next) == G_DATE_FRIDAY)
        {
            GDate tmp_sat;
            GDate tmp_sun;
            g_date_set_julian(&tmp_sat, g_date_get_julian(next));
            g_date_set_julian(&tmp_sun, g_date_get_julian(next));
            g_date_add_days(&tmp_sat, 1);
            g_date_add_days(&tmp_sun, 2);

            if (pt == PERIOD_END_OF_MONTH)
            {
                if (g_date_is_last_of_month(next) ||
                        g_date_is_last_of_month(&tmp_sat) ||
                        g_date_is_last_of_month(&tmp_sun))
                    g_date_add_months(next, mult);
                else
                    /* one fewer month fwd because of the occurrence in this month */
                    g_date_add_months(next, mult - 1);
            }
            else
            {
                if (g_date_get_day(&tmp_sat) == g_date_get_day(start))
                {
                    g_date_add_days(next, 1);
                    g_date_add_months(next, mult);
                }
                else if (g_date_get_day(&tmp_sun) == g_date_get_day(start))
                {
                    g_date_add_days(next, 2);
                    g_date_add_months(next, mult);
                }
                else if (g_date_get_day(next) >= g_date_get_day(start))
                {
                    g_date_add_months(next, mult);
                }
                else if (g_date_is_last_of_month(next))
                {
                    g_date_add_months(next, mult);
                }
                else if (g_date_is_last_of_month(&tmp_sat))
                {
                    g_date_add_days(next, 1);
                    g_date_add_months(next, mult);
                }
                else if (g_date_is_last_of_month(&tmp_sun))
                {
                    g_date_add_days(next, 2);
                    g_date_add_months(next, mult);
                }
                else
                {
                    /* one fewer month fwd because of the occurrence in this month */
                    g_date_add_months(next, mult - 1);
                }
            }
        }
        else if ( g_date_is_last_of_month(next) ||
                  ((pt == PERIOD_MONTH || pt == PERIOD_YEAR) &&
                   g_date_get_day(next) >= g_date_get_day(start)) ||
                  ((pt == PERIOD_NTH_WEEKDAY || pt == PERIOD_LAST_WEEKDAY) &&
                   nth_weekday_compare(start, next, pt) <= 0) )
            g_date_add_months(next, mult);
        else
            /* one fewer month fwd because of the occurrence in this month */
            g_date_add_months(next, mult - 1);
        break;
    case PERIOD_WEEK:
        mult *= 7;              /* fall-through */
    case PERIOD_DAY:
        g_date_add_days(next, mult);
        break;
    case PERIOD_ONCE:
        g_date_clear(next, 1);  /* We already caught the case where ref is */
        return;                 /* earlier than start, so this is invalid. */
    default:
        PERR("Invalid period type");
    }

    /* Step 2: Back up to align to the base phase. To ensure forward
       progress, we never subtract as much as we added (x % mult < mult). */
    switch (pt)
    {
    case PERIOD_YEAR:
    case PERIOD_MONTH:
    case PERIOD_NTH_WEEKDAY:
    case PERIOD_LAST_WEEKDAY:
    case PERIOD_END_OF_MONTH:
    {
        guint dim, n_months;

        n_months = 12 * (g_date_get_year(next) - g_date_get_year(start)) +
                   (g_date_get_month(next) - g_date_get_month(start));
        g_date_subtract_months(next, n_months % mult);

        /* Ok, now we're in the right month, so we just have to align
           the day in one of the three possible ways. */
        dim = g_date_get_days_in_month(g_date_get_month(next),
                                       g_date_get_year(next));
        if (pt == PERIOD_NTH_WEEKDAY || pt == PERIOD_LAST_WEEKDAY)
            g_date_add_days(next, nth_weekday_compare(start, next, pt));
        else if (pt == PERIOD_END_OF_MONTH || g_date_get_day(start) >= dim)
            g_date_set_day(next, dim);  /* last day in the month */
        else
            g_date_set_day(next, g_date_get_day(start)); /*same day as start*/

        /* Adjust for dates on the weekend. */
        if (pt == PERIOD_YEAR || pt == PERIOD_MONTH || pt == PERIOD_END_OF_MONTH)
        {
            if (g_date_get_weekday(next) == G_DATE_SATURDAY || g_date_get_weekday(next) == G_DATE_SUNDAY)
            {
                switch (wadj)
                {
                case WEEKEND_ADJ_BACK:
                    g_date_subtract_days(next, g_date_get_weekday(next) == G_DATE_SATURDAY ? 1 : 2);
                    break;
                case WEEKEND_ADJ_FORWARD:
                    g_date_add_days(next, g_date_get_weekday(next) == G_DATE_SATURDAY ? 2 : 1);
                    break;
                case WEEKEND_ADJ_NONE:
                default:
                    break;
                }
            }
        }

    }
    break;
    case PERIOD_WEEK:
    case PERIOD_DAY:
        g_date_subtract_days(next, g_date_days_between(start, next) % mult);
        break;
    default:
        PERR("Invalid period type");
    }
}

/* Zero-based index */
void
recurrenceNthInstance(const Recurrence *r, guint n, GDate *date)
{
    GDate ref;
    guint i;

    for (*date = ref = r->start, i = 0; i < n; i++)
    {
        recurrenceNextInstance(r, &ref, date);
        ref = *date;
    }
}

time_t
recurrenceGetPeriodTime(const Recurrence *r, guint period_num, gboolean end)
{
    GDate date;
    recurrenceNthInstance(r, period_num + (end ? 1 : 0), &date);
    if (end)
    {
        g_date_subtract_days(&date, 1);
        return gnc_timet_get_day_end_gdate(&date);
    }
    else
    {
        return gnc_timet_get_day_start_gdate(&date);
    }
}

gnc_numeric
recurrenceGetAccountPeriodValue(const Recurrence *r, Account *acc, guint n)
{
    time_t t1, t2;

    // FIXME: maybe zero is not best error return val.
    g_return_val_if_fail(r && acc, gnc_numeric_zero());
    t1 = recurrenceGetPeriodTime(r, n, FALSE);
    t2 = recurrenceGetPeriodTime(r, n, TRUE);
    return xaccAccountGetBalanceChangeForPeriod (acc, t1, t2, TRUE);
}

void
recurrenceListNextInstance(const GList *rlist, const GDate *ref, GDate *next)
{
    const GList *iter;
    GDate nextSingle;  /* The next date for an individual recurrence */

    g_date_clear(next, 1);

    // empty rlist = no recurrence
    if (rlist == NULL)
    {
        return;
    }

    g_return_if_fail(ref && next && g_date_valid(ref));

    for (iter = rlist; iter; iter = iter->next)
    {
        const Recurrence *r = iter->data;

        recurrenceNextInstance(r, ref, &nextSingle);
        if (!g_date_valid(&nextSingle)) continue;

        if (g_date_valid(next))
            g_date_order(next, &nextSingle); /* swaps dates if needed */
        else
            *next = nextSingle; /* first date is always earliest so far */
    }
}

/* Caller owns the returned memory */
gchar *
recurrenceToString(const Recurrence *r)
{
    gchar *tmpDate;
    gchar *tmpPeriod, *ret;

    g_return_val_if_fail(g_date_valid(&r->start), NULL);
    tmpDate = g_new0(gchar, MAX_DATE_LENGTH + 1);
    g_date_strftime(tmpDate, MAX_DATE_LENGTH, "%x", &r->start);

    if (r->ptype == PERIOD_ONCE)
    {
        ret = g_strdup_printf("once on %s", tmpDate);
        goto done;
    }

    tmpPeriod = period_type_strings[r->ptype];
    if (r->mult > 1)
        ret = g_strdup_printf("Every %d %ss beginning %s",
                              r->mult, tmpPeriod, tmpDate);
    else
        ret = g_strdup_printf("Every %s beginning %s",
                              tmpPeriod, tmpDate);
done:
    g_free(tmpDate);
    return ret;
}

/* caller owns the returned memory */
gchar *
recurrenceListToString(const GList *r)
{
    const GList *iter;
    GString *str;
    gchar *s;

    str = g_string_new("");
    if (r == NULL)
    {
        g_string_append(str, _("None"));
    }
    else
    {
        for (iter = r; iter; iter = iter->next)
        {
            if (iter != r)
            {
                /* translators: " + " is an separator in a list of string-representations of recurrence frequencies */
                g_string_append(str, _(" + "));
            }
            s = recurrenceToString((Recurrence *)iter->data);
            g_string_append(str, s);
            g_free(s);
        }
    }
    return g_string_free(str, FALSE);
}

gchar *
recurrencePeriodTypeToString(PeriodType pt)
{
    return VALID_PERIOD_TYPE(pt) ? g_strdup(period_type_strings[pt]) : NULL;
}

PeriodType
recurrencePeriodTypeFromString(const gchar *str)
{
    int i;

    for (i = 0; i < NUM_PERIOD_TYPES; i++)
        if (safe_strcmp(period_type_strings[i], str) == 0)
            return i;
    return -1;
}

gchar *
recurrenceWeekendAdjustToString(WeekendAdjust wadj)
{
    return VALID_WEEKEND_ADJ(wadj) ? g_strdup(weekend_adj_strings[wadj]) : NULL;
}

WeekendAdjust
recurrenceWeekendAdjustFromString(const gchar *str)
{
    int i;

    for (i = 0; i < NUM_WEEKEND_ADJS; i++)
        if (safe_strcmp(weekend_adj_strings[i], str) == 0)
            return i;
    return -1;
}

gboolean
recurrenceListIsSemiMonthly(GList *recurrences)
{
    if (g_list_length(recurrences) != 2)
        return FALSE;

    // should be a "semi-monthly":
    {
        Recurrence *first = (Recurrence*)g_list_nth_data(recurrences, 0);
        Recurrence *second = (Recurrence*)g_list_nth_data(recurrences, 1);
        PeriodType first_period, second_period;
        first_period = recurrenceGetPeriodType(first);
        second_period = recurrenceGetPeriodType(second);

        if (!((first_period == PERIOD_MONTH
                || first_period == PERIOD_END_OF_MONTH
                || first_period == PERIOD_LAST_WEEKDAY)
                && (second_period == PERIOD_MONTH
                    || second_period == PERIOD_END_OF_MONTH
                    || second_period == PERIOD_LAST_WEEKDAY)))
        {
            /*g_error("unknown 2-recurrence composite with period_types first [%d] second [%d]",
              first_period, second_periodD);*/
            return FALSE;
        }
    }
    return TRUE;
}

gboolean
recurrenceListIsWeeklyMultiple(GList *recurrences)
{
    GList *r_iter;

    for (r_iter = recurrences; r_iter != NULL; r_iter = r_iter->next)
    {
        Recurrence *r = (Recurrence*)r_iter->data;
        if (recurrenceGetPeriodType(r) != PERIOD_WEEK)
        {
            return FALSE;
        }
    }
    return TRUE;
}

static void
_weekly_list_to_compact_string(GList *rs, GString *buf)
{
    int dow_idx;
    char dow_present_bits = 0;
    int multiplier = -1;
    for (; rs != NULL; rs = rs->next)
    {
        Recurrence *r = (Recurrence*)rs->data;
        GDate date = recurrenceGetDate(r);
        GDateWeekday dow = g_date_get_weekday(&date);
        if (dow == G_DATE_BAD_WEEKDAY)
        {
            g_critical("bad weekday pretty-printing recurrence");
            continue;
        }
        dow_present_bits |= (1 << (dow % 7));

        // there's not necessarily a single multiplier, but for all intents
        // and purposes this will be fine.
        multiplier = recurrenceGetMultiplier(r);
    }
    g_string_printf(buf, "%s", _("Weekly"));
    if (multiplier > 1)
    {
        /* translators: %u is the recurrence multipler, i.e. this
        	   event should occur every %u'th week. */
        g_string_append_printf(buf, _(" (x%u)"), multiplier);
    }
    g_string_append_printf(buf, ": ");

    // @@fixme: this is only Sunday-started weeks. :/
    for (dow_idx = 0; dow_idx < 7; dow_idx++)
    {
        if ((dow_present_bits & (1 << dow_idx)) != 0)
        {
            gchar dbuf[10];
            gnc_dow_abbrev(dbuf, 10, dow_idx);
            g_string_append_unichar(buf, g_utf8_get_char(dbuf));
        }
        else
        {
            g_string_append_printf(buf, "-");
        }
    }
}

/* A constant is needed for the array size */
#define abbrev_day_name_bufsize 10
static void
_monthly_append_when(Recurrence *r, GString *buf)
{
    GDate date = recurrenceGetDate(r);
    if (recurrenceGetPeriodType(r) == PERIOD_LAST_WEEKDAY)
    {
        gchar day_name_buf[abbrev_day_name_bufsize];

        gnc_dow_abbrev(day_name_buf, abbrev_day_name_bufsize, g_date_get_weekday(&date) % 7);

        /* translators: %s is an already-localized form of the day of the week. */
        g_string_append_printf(buf, _("last %s"), day_name_buf);
    }
    else
    {
        /* translators: %u is the day of month */
        g_string_append_printf(buf, "%u", g_date_get_day(&date));
    }
}

gchar*
recurrenceListToCompactString(GList *rs)
{
    GString *buf = g_string_sized_new(16);

    if (g_list_length(rs) == 0)
    {
        g_string_printf(buf, "%s", _("None"));
        goto rtn;
    }

    if (g_list_length(rs) > 1)
    {
        if (recurrenceListIsWeeklyMultiple(rs))
        {
            _weekly_list_to_compact_string(rs, buf);
        }
        else if (recurrenceListIsSemiMonthly(rs))
        {
            Recurrence *first, *second;
            first = (Recurrence*)g_list_nth_data(rs, 0);
            second = (Recurrence*)g_list_nth_data(rs, 1);
            if (recurrenceGetMultiplier(first) != recurrenceGetMultiplier(second))
            {
                g_warning("lying about non-equal semi-monthly recurrence multiplier: %d vs. %d",
                          recurrenceGetMultiplier(first), recurrenceGetMultiplier(second));
            }

            g_string_printf(buf, "%s", _("Semi-monthly"));
            g_string_append_printf(buf, " ");
            if (recurrenceGetMultiplier(first) > 1)
            {
                /* translators: %u is the recurrence multiplier */
                g_string_append_printf(buf, _(" (x%u)"), recurrenceGetMultiplier(first));
            }
            g_string_append_printf(buf, ": ");
            _monthly_append_when(first, buf);
            g_string_append_printf(buf, ", ");
            _monthly_append_when(second, buf);
        }
        else
        {
            /* translators: %d is the number of Recurrences in the list. */
            g_string_printf(buf, _("Unknown, %d-size list."), g_list_length(rs));
        }
    }
    else
    {
        Recurrence *r = (Recurrence*)g_list_nth_data(rs, 0);
        guint multiplier = recurrenceGetMultiplier(r);

        switch (recurrenceGetPeriodType(r))
        {
        case PERIOD_ONCE:
        {
            g_string_printf(buf, "%s", _("Once"));
        }
        break;
        case PERIOD_DAY:
        {
            g_string_printf(buf, "%s", _("Daily"));
            if (multiplier > 1)
            {
                /* translators: %u is the recurrence multiplier. */
                g_string_append_printf(buf, _(" (x%u)"), multiplier);
            }
        }
        break;
        case PERIOD_WEEK:
        {
            _weekly_list_to_compact_string(rs, buf);
        }
        break;
        case PERIOD_MONTH:
        case PERIOD_END_OF_MONTH:
        case PERIOD_LAST_WEEKDAY:
        {
            g_string_printf(buf, "%s", _("Monthly"));
            if (multiplier > 1)
            {
                /* translators: %u is the recurrence multiplier. */
                g_string_append_printf(buf, _(" (x%u)"), multiplier);
            }
            g_string_append_printf(buf, ": ");
            _monthly_append_when(r, buf);
        }
        break;
        case PERIOD_NTH_WEEKDAY:
        {
            g_warning("nth weekday not handled");
            g_string_printf(buf, "@fixme: nth weekday not handled");
        }
        break;
        case PERIOD_YEAR:
        {
            g_string_printf(buf, "%s", _("Yearly"));
            if (multiplier > 1)
            {
                /* translators: %u is the recurrence multiplier. */
                g_string_append_printf(buf, _(" (x%u)"), multiplier);
            }
        }
        break;
        default:
            g_error("unknown Recurrence period %d", recurrenceGetPeriodType(r));
            break;
        }
    }

rtn:
    return g_string_free(buf, FALSE);
}

/**
 * The ordering, in increasing degrees of frequent-ness:
 *
 *   day < week < {nth-weekday < month < end-month, last_weekday} < year < once
 *
 * all the monthly types are basically together, but are broken down
 * internally cause they have to be ordered somehow.
 **/
static int cmp_order_indexes[] =
{
    6, // PERIOD_ONCE
    1, // PERIOD_DAY
    2, // PERIOD_WEEK
    // 3, // "semi-monthly" ... Note that this isn't presently used, just the
    //    // way the code worked out. :(
    4, // PERIOD_MONTH
    4, // PERIOD_END_OF_MONTH
    4, // PERIOD_NTH_WEEKDAY
    4, // PERIOD_LAST_WEEKDAY
    5, // PERIOD_YEAR
};

static int cmp_monthly_order_indexes[] =
{
    -1, // PERIOD_ONCE
    -1, // PERIOD_DAY
    -1, // PERIOD_WEEK
    2, // PERIOD_MONTH
    3, // PERIOD_END_OF_MONTH
    1, // PERIOD_NTH_WEEKDAY
    4, // PERIOD_LAST_WEEKDAY
    -1, // PERIOD_YEAR
};

int
recurrenceCmp(Recurrence *a, Recurrence *b)
{
    PeriodType period_a, period_b;
    int a_order_index, b_order_index;

    g_return_val_if_fail(a != NULL && b != NULL, 0);
    g_return_val_if_fail(a != NULL, 1);
    g_return_val_if_fail(b != NULL, -1);

    period_a = recurrenceGetPeriodType(a);
    period_b = recurrenceGetPeriodType(b);

    a_order_index = cmp_order_indexes[period_a];
    b_order_index = cmp_order_indexes[period_b];
    if (a_order_index != b_order_index)
    {
        return a_order_index - b_order_index;
    }
    else if (a_order_index == cmp_order_indexes[PERIOD_MONTH])
    {
        // re-order intra-month options:
        a_order_index = cmp_monthly_order_indexes[period_a];
        b_order_index = cmp_monthly_order_indexes[period_b];
        g_assert(a_order_index != -1 && b_order_index != -1);
        if (a_order_index != b_order_index)
            return a_order_index - b_order_index;
    }
    /* else { the basic periods are equal; compare the multipliers } */

    {
        int a_mult, b_mult;
        a_mult = recurrenceGetMultiplier(a);
        b_mult = recurrenceGetMultiplier(b);

        return a_mult - b_mult;
    }
}

int
recurrenceListCmp(GList *a, GList *b)
{
    Recurrence *most_freq_a, *most_freq_b;

    g_return_val_if_fail(g_list_length(a) != 0 && g_list_length(b) != 0, 0);
    g_return_val_if_fail(g_list_length(a) != 0, -1);
    g_return_val_if_fail(g_list_length(b) != 0, 1);

    most_freq_a = (Recurrence*)g_list_nth_data(g_list_sort(a, (GCompareFunc)recurrenceCmp), 0);
    most_freq_b = (Recurrence*)g_list_nth_data(g_list_sort(b, (GCompareFunc)recurrenceCmp), 0);

    return recurrenceCmp(most_freq_a, most_freq_b);
}

void
recurrenceListFree(GList **recurrences)
{
    g_list_foreach(*recurrences, (GFunc)g_free, NULL);
    g_list_free(*recurrences);
    *recurrences = NULL;
}
