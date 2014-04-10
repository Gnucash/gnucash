/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* format.c - attempts to emulate excel's number formatting ability.
 * Copyright (C) 1998 Chris Lahey, Miguel de Icaza
 *
 * Redid the format parsing routine to make it accept more of the Excel
 * formats.  The number rendeing code from Chris has not been touched,
 * that routine is pretty good.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <goffice/goffice-config.h>
#include "go-format.h"
#include "format-impl.h"
#include "go-format-match.h"
#include "go-color.h"
#include "datetime.h"
#include "go-glib-extras.h"
#include "go-math.h"
#include <glib/gi18n.h>

#include <time.h>
#include <math.h>
#include <locale.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#ifdef HAVE_LANGINFO_H
#  include <langinfo.h>
#endif
#ifdef G_OS_WIN32
#  include <windows.h>
#endif

/* ------------------------------------------------------------------------- */

#ifndef DOUBLE

#define DEFINE_COMMON
#define DOUBLE double
#define SUFFIX(_n) _n
#define PREFIX(_n) DBL_ ## _n
#define FORMAT_e "e"
#define FORMAT_f "f"
#define FORMAT_E "E"
#define FORMAT_G "G"
#define STRTO strtod

#ifdef GOFFICE_WITH_LONG_DOUBLE
#include "format.c"
#undef DEFINE_COMMON
#undef DOUBLE
#undef SUFFIX
#undef PREFIX
#undef FORMAT_e
#undef FORMAT_f
#undef FORMAT_E
#undef FORMAT_G
#undef STRTO

#ifdef HAVE_SUNMATH_H
#include <sunmath.h>
#endif
#define DOUBLE long double
#define SUFFIX(_n) _n ## l
#define PREFIX(_n) LDBL_ ## _n
#define FORMAT_e "Le"
#define FORMAT_f "Lf"
#define FORMAT_E "LE"
#define FORMAT_G "LG"
#define STRTO strtold
#endif

#endif

/* ------------------------------------------------------------------------- */

#undef DEBUG_REF_COUNT

/***************************************************************************/

#ifdef DEFINE_COMMON
static GOFormat *default_percentage_fmt;
static GOFormat *default_money_fmt;
static GOFormat *default_date_fmt;
static GOFormat *default_time_fmt;
static GOFormat *default_date_time_fmt;
static GOFormat *default_general_fmt;


/*
 * Points to the locale information for number display.  All strings are
 * in UTF-8 encoding.
 */
static gboolean locale_info_cached = FALSE;
static GString *lc_decimal = NULL;
static GString *lc_thousand = NULL;
static gboolean lc_precedes;
static gboolean lc_space_sep;
static GString *lc_currency = NULL;

static gboolean date_order_cached = FALSE;

static gboolean boolean_cached = FALSE;
static char const *lc_TRUE = NULL;
static char const *lc_FALSE = NULL;

static double beyond_precision;
#ifdef GOFFICE_WITH_LONG_DOUBLE
static long double beyond_precisionl;
#endif

/* FIXME : use nl_langinfo
 * DAY_{1-7} (LC_TIME)
 * ABDAY_{1-7} (LC_TIME)
 * MON_{1-12} (LC_TIME)
 * ABMON_{1-12} (LC_TIME)
 *
 * The down side is that we can not guarantee the case
 **/
char const * const day_short [] = {
	N_("*Sun"),
	N_("*Mon"),
	N_("*Tue"),
	N_("*Wed"),
	N_("*Thu"),
	N_("*Fri"),
	N_("*Sat"),
	NULL,
};

char const * const day_long [] = {
	N_("Sunday"),
	N_("Monday"),
	N_("Tuesday"),
	N_("Wednesday"),
	N_("Thursday"),
	N_("Friday"),
	N_("Saturday"),
	NULL
};

char const * const month_short [] = {
	N_("*Jan"),
	N_("*Feb"),
	N_("*Mar"),
	N_("*Apr"),
	N_("*May"),
	N_("*Jun"),
	N_("*Jul"),
	N_("*Aug"),
	N_("*Sep"),
	N_("*Oct"),
	N_("*Nov"),
	N_("*Dec"),
	NULL
};

char const * const month_long [] = {
	N_("January"),
	N_("February"),
	N_("March"),
	N_("April"),
	N_("May"),
	N_("June"),
	N_("July"),
	N_("August"),
	N_("September"),
	N_("October"),
	N_("November"),
	N_("December"),
	NULL
};

static GOColor lookup_color (char const *str, char const *end);

char const *
go_setlocale (int category, char const *val)
{
	locale_info_cached = FALSE;
	date_order_cached = FALSE;
	boolean_cached = FALSE;
	return setlocale (category, val);
}

static void
convert1 (GString *res, char const *lstr, char const *name, char const *def)
{
	char *tmp;

	if (lstr == NULL || lstr[0] == 0) {
		g_string_assign (res, def);
		return;
	}

	tmp = g_locale_to_utf8 (lstr, -1, NULL, NULL, NULL);
	if (tmp) {
		g_string_assign (res, tmp);
		g_free (tmp);
		return;
	}

	g_warning ("Failed to convert locale's %s \"%s\" to UTF-8.", name, lstr);
	g_string_assign (res, def);
}

static void
update_lc (void)
{
	struct lconv *lc = localeconv ();

	/*
	 * Extract all information here as lc is not guaranteed to stay
	 * valid after next localeconv call which could be anywhere.
	 */

	convert1 (lc_decimal, lc->decimal_point, "decimal separator", ".");
	if (g_utf8_strlen (lc_decimal->str, -1) != 1)
		g_warning ("Decimal separator is not a single character.");

	convert1 (lc_thousand, lc->mon_thousands_sep, "monetary thousands separator",
		  (lc_decimal->str[0] == ',' ? "." : ","));
	if (g_utf8_strlen (lc_thousand->str, -1) != 1)
		g_warning ("Monetary thousands separator is not a single character.");

	if (g_string_equal (lc_thousand, lc_decimal)) {
		g_string_assign (lc_thousand,
				 (lc_decimal->str[0] == ',') ? "." : ",");
		g_warning ("Monetary thousands separator is the same as the decimal separator; converting '%s' to '%s'",
			   lc_decimal->str, lc_thousand->str);
	}

	/* Use != 0 rather than == 1 so that CHAR_MAX (undefined) is true */
	lc_precedes = (lc->p_cs_precedes != 0);

	/* Use == 1 rather than != 0 so that CHAR_MAX (undefined) is false */
	lc_space_sep = (lc->p_sep_by_space == 1);

	convert1 (lc_currency, lc->currency_symbol, "currency symbol",	"$");

	locale_info_cached = TRUE;
}

GString const *
format_get_decimal (void)
{
	if (!locale_info_cached)
		update_lc ();

	return lc_decimal;
}

GString const *
format_get_thousand (void)
{
	if (!locale_info_cached)
		update_lc ();

	return lc_thousand;
}

/**
 * format_get_currency :
 * @precedes : a pointer to a boolean which is set to TRUE if the currency
 * 		should precede
 * @space_sep: a pointer to a boolean which is set to TRUE if the currency
 * 		should have a space separating it from the the value
 *
 * Play with the default logic so that things come out nicely for the default
 * case.
 */
GString const *
format_get_currency (gboolean *precedes, gboolean *space_sep)
{
	if (!locale_info_cached)
		update_lc ();

	if (precedes)
		*precedes = lc_precedes;

	if (space_sep)
		*space_sep = lc_space_sep;

	return lc_currency;
}

/*
 * format_month_before_day :
 *
 * A quick utility routine to guess whether the default date format
 * uses day/month or month/day
 */
gboolean
format_month_before_day (void)
{
#ifdef HAVE_LANGINFO_H
	static gboolean month_first = TRUE;

	if (!date_order_cached) {
		char const *ptr = nl_langinfo (D_FMT);

		date_order_cached = TRUE;
		month_first = TRUE;
		if (ptr)
			while (*ptr) {
				char c = *ptr++;
				if (c == 'd' || c == 'D') {
					month_first = FALSE;
					break;
				} else if (c == 'm' || c == 'M')
					break;
			}
	}

	return month_first;
#elif defined(G_OS_WIN32)
	TCHAR str[2];

	GetLocaleInfo (LOCALE_USER_DEFAULT, LOCALE_IDATE, str, 2);

	return str[0] != L'1';
#else
	static gboolean warning = TRUE;
	if (warning) {
		g_warning ("Incomplete locale library, dates will be month day year");
		warning = FALSE;
	}
	return TRUE;
#endif
}

/* Use comma as the arg separator unless the decimal point is a
 * comma, in which case use a semi-colon
 */
char
format_get_arg_sep (void)
{
	if (format_get_decimal ()->str[0] == ',')
		return ';';
	return ',';
}

char
format_get_col_sep (void)
{
	if (format_get_decimal ()->str[0] == ',')
		return '\\';
	return ',';
}

char
format_get_row_sep (void)
{
	return ';';
}

char const *
format_boolean (gboolean b)
{
	if (!boolean_cached) {
		lc_TRUE = _("TRUE");
		lc_FALSE = _("FALSE");
		boolean_cached = TRUE;
	}
	return b ? lc_TRUE : lc_FALSE;
}

/**
 * go_set_untranslated_bools :
 * 
 * Short circuit the current locale so that we can import files
 * and still produce error messages in the current LC_MESSAGE
 **/
void
go_set_untranslated_bools (void)
{
	lc_TRUE = "TRUE";
	lc_FALSE = "FALSE";
	boolean_cached = TRUE;
}

/***************************************************************************/

/* WARNING : Global */
static GHashTable *style_format_hash = NULL;

/*
 * The returned string is newly allocated.
 *
 * Current format is an optional date specification followed by an
 * optional number specification.
 *
 * A date specification is an arbitrary sequence of characters (other
 * than '#', '0', '?', or '.') which is copied to the output.  The
 * standard date fields are substituted for.  If it ever finds an a or
 * a p it lists dates in 12 hour time, otherwise, it lists dates in 24
 * hour time.
 *
 * A number specification is as described in the relevant portions of
 * the excel formatting information.  Commas can currently only appear
 * at the end of the number specification.  Fractions are supported
 * but the parsing is not as nice as it should be.
 */


/*
 * Parses the year field at the beginning of the format.  Returns the
 * number of characters used.
 */
static int
append_year (GString *string, gchar const *format, struct tm const *time_split)
{
	int year = time_split->tm_year + 1900;

	if (format[1] != 'y' && format[1] != 'Y') {
		g_string_append_c (string, 'y');
		return 1;
	}

	if ((format[2] != 'y' && format[2] != 'Y') ||
	    (format[3] != 'y' && format[3] != 'Y')) {
		g_string_append_printf (string, "%02d", year % 100);
		return 2;
	}

	g_string_append_printf (string, "%04d", year);
	return 4;
}

/*
 * Parses the month field at the beginning of the format.  Returns the
 * number of characters used.
 */
static int
append_month (GString *string, int n, struct tm const *time_split)
{
	int month = time_split->tm_mon + 1;

	if (n == 1) {
		g_string_append_printf (string, "%d", month);
		return 1;
	}

	if (n == 2) {
		g_string_append_printf (string, "%02d", month);
		return 2;
	}

	if (n == 3) {
		g_string_append (string, _(month_short[month - 1]) + 1);
		return 3;
	}

	if (n == 5) {
		g_string_append_c (string, _(month_short[month - 1])[1]);
		return 5;
	}
	g_string_append (string, _(month_long[month - 1]));
	return 4;
}

/*
 * Parses the day field at the beginning of the format.  Returns the
 * number of characters used.
 */
static int
append_day (GString *string, gchar const *format, struct tm const *time_split)
{
	if (format[1] != 'd' && format[1] != 'D') {
		g_string_append_printf (string, "%d", time_split->tm_mday);
		return 1;
	}

	if (format[2] != 'd' && format[2] != 'D') {
		g_string_append_printf (string, "%02d", time_split->tm_mday);
		return 2;
	}

	if (format[3] != 'd' && format[3] != 'D') {
		/* Note: day-of-week.  */
		g_string_append (string, _(day_short[time_split->tm_wday]) + 1);
		return 3;
	}

	/* Note: day-of-week.  */
	g_string_append (string, _(day_long[time_split->tm_wday]));
	return 4;
}

static void
append_hour (GString *string, int n, struct tm const *time_split,
	     gboolean want_am_pm)
{
	int hour = time_split->tm_hour;

	g_string_append_printf (string, "%0*d", MIN (n, 2),
				(want_am_pm || (n > 2))
				? ((hour + 11) % 12) + 1
				: hour);
}
#endif

static void
SUFFIX(append_hour_elapsed) (GString *string, struct tm *tm, DOUBLE number)
{
	DOUBLE whole_days, frac_days;
	gboolean is_neg;
	int cs;  /* Centi seconds.  */
	int const secs_per_day = 24 * 60 * 60;

	is_neg = (number < 0);
	frac_days = SUFFIX(modf) (number, &whole_days);

	/* ick.  round assuming no more than 100th of a second, we really need
	 * to know the precision earlier */
	cs = (int)SUFFIX(go_fake_round) (SUFFIX(fabs) (frac_days) * secs_per_day * 100);

	/* FIXME: Why limit hours to int? */
	cs /= 100;
	tm->tm_sec = cs % 60;
	cs /= 60;
	tm->tm_min = cs % 60;
	cs /= 60;
	tm->tm_hour = (is_neg ? -cs : cs) + (int)(whole_days * 24);

	g_string_append_printf (string, "%d", tm->tm_hour);
}

#ifdef DEFINE_COMMON
static void
append_minute (GString *string, int n, struct tm const *time_split)
{
	g_string_append_printf (string, "%0*d", n, time_split->tm_min);
}
#endif

static void
SUFFIX(append_minute_elapsed) (GString *string, struct tm *tm, DOUBLE number)
{
	DOUBLE res, int_part;

	res = SUFFIX(modf) (SUFFIX(go_fake_round) (number * 24. * 60.), &int_part);
	tm->tm_min = int_part;
	tm->tm_sec = res * ((res < 0.) ? -60. : 60.);
	g_string_append_printf (string, "%d", tm->tm_min);
}

#ifdef DEFINE_COMMON
static void
append_second (GString *string, int n, struct tm const *time_split)
{
	g_string_append_printf (string, "%0*d", n, time_split->tm_sec);
}
#endif

static void
SUFFIX(append_second_elapsed) (GString *string, DOUBLE number)
{
	g_string_append_printf (string, "%d",
				(int) SUFFIX(go_fake_round) (number * 24. * 3600.));
}

#ifdef DEFINE_COMMON
static GOFormatElement *
format_entry_ctor (GOFormat *container)
{
	GOFormatElement *entry;

	entry = g_new (GOFormatElement, 1);
	entry->container = container;
	entry->restriction_type = '*';
	entry->restriction_value = 0.;
	entry->suppress_minus = FALSE;
	entry->forces_text = FALSE;
	entry->elapsed_time = FALSE;
	entry->want_am_pm = entry->has_fraction = FALSE;
	entry->go_color = 0;
	entry->regexp_str = NULL;
	entry->match_tags = NULL;

	/* symbolic failure */
	g_return_val_if_fail (container != NULL, entry);

	return entry;
}
#endif

#ifdef DEFINE_COMMON
/* WARNING : do not call this for temporary formats generated for 'General' */
static void
format_entry_dtor (gpointer data, gpointer user_data)
{
	GOFormatElement *entry = data;
	format_match_release (entry);
	g_free ((char *)entry->format);
	g_free (entry);
}
#endif

#ifdef DEFINE_COMMON
static void
format_entry_set_fmt (GOFormatElement *entry,
		      gchar const *begin,
		      gchar const *end)
{
	/* empty formats are General if there is a color, or a condition */
	entry->format = (begin != NULL && end != begin)
		? g_strndup (begin, end - begin)
		: g_strdup ((entry->go_color || entry->restriction_type != '*')
			    ? "General" : "");
	format_match_create (entry);
}
#endif

#ifdef DEFINE_COMMON
/*
 * Since the Excel formating codes contain a number of ambiguities, this
 * routine does some analysis on the format first.  This routine should always
 * return, it cannot fail, in the worst case it should just downgrade to
 * simplistic formatting
 */
static void
format_compile (GOFormat *format)
{
	gchar const *fmt, *real_start = NULL;
	GOFormatElement *entry = format_entry_ctor (format);
	int num_entries = 1, counter = 0;
	GSList *ptr;

	for (fmt = format->format; *fmt ; fmt++) {
		if (NULL == real_start && '[' != *fmt)
			real_start = fmt;

		switch (*fmt) {
		case '[': {
			gchar const *begin = fmt + 1;
			gchar const *end = begin;

			/* find end checking for escapes but not quotes ?? */
			for (; end[0] != ']' && end[1] != '\0' ; ++end)
				if (*end == '\\')
					end++;

			/* Check for conditional */
			if (*begin == '<') {
				if (begin[1] == '=') {
					entry->restriction_type = ',';
					begin += 2;
				} else if (begin[1] == '>') {
					entry->restriction_type = '+';
					begin += 2;
				} else {
					entry->restriction_type = '<';
					begin++;
				}
			} else if (*begin == '>') {
				if (begin[1] == '=') {
					entry->restriction_type = '.';
					begin += 2;
				} else {
					entry->restriction_type = '>';
					begin++;
				}
			} else if (*begin == '=') {
				entry->restriction_type = '=';
			} else {
				if (begin[1] == ']' &&
				    (*begin == 'h' || *begin == 'H' ||
				     *begin == 'm' || *begin == 'M' ||
				     *begin == 's' || *begin == 'S'))
					entry->elapsed_time = TRUE;
				else if (*begin != '$' && entry->go_color == 0) {
					entry->go_color = lookup_color (begin, end);
					/* Only the first colour counts */
					if (0 != entry->go_color) {
						fmt = end;
						continue;
					}
				}
				if (NULL == real_start)
					real_start = fmt;
				continue;
			}
			fmt = end;

			/* fall back on 0 for errors */
			errno = 0;
			entry->restriction_value = STRTO (begin, (char **)&end);
			if (errno == ERANGE || begin == end)
				entry->restriction_value = 0.;

			/* this is a guess based on checking the results of
			 * 0.00;[<0]0.00
			 * 0.00;[<=0]0.00
			 *
			 * for -1.2.3
			 **/
			else if (entry->restriction_type == '<')
				entry->suppress_minus = (entry->restriction_value <= 0.);
			else if (entry->restriction_type == ',')
				entry->suppress_minus = (entry->restriction_value < 0.);
			break;
		}

		case '\\' :
			if (fmt[1] != '\0')
				fmt++; /* skip escaped characters */
			break;

		case '\'' :
		case '\"' : {
			/* skip quoted strings */
			char const match = *fmt;
			for (; fmt[0] != match && fmt[1] != '\0'; fmt++)
				if (*fmt == '\\')
					fmt++;
			break;
		}

		case '/':
			if (fmt[1] == '?' || (fmt[1] >= '0' && fmt[1] <= '9')) {
				entry->has_fraction = TRUE;
				fmt++;
			}
			break;

		case 'a': case 'A':
		case 'p': case 'P':
			if (fmt[1] == 'm' || fmt[1] == 'M')
				entry->want_am_pm = TRUE;
			break;

		case 'M': case 'm':
		case 'D': case 'd':
		case 'Y': case 'y':
		case 'S': case 's':
		case 'H': case 'h':
			if (!entry->suppress_minus && !entry->elapsed_time)
				entry->suppress_minus = TRUE;
			break;

		case '@':
			entry->forces_text = TRUE;
			break;

		case ';':
			format_entry_set_fmt (entry, real_start, fmt);
			format->entries = g_slist_append (format->entries, entry);
			num_entries++;

			entry = format_entry_ctor (format);
			real_start = NULL;
			break;

		case '*':
			if (fmt != format->format)
				format->is_var_width = TRUE;
			break;

		default :
			break;
		}
	}

	format_entry_set_fmt (entry, real_start, fmt);
	format->entries = g_slist_append (format->entries, entry);

	for (ptr = format->entries; ptr && counter++ < 4 ; ptr = ptr->next) {
		GOFormatElement *entry = ptr->data;

		/* apply the standard restrictions where things are unspecified */
		if (entry->restriction_type == '*') {
			entry->restriction_value = 0.;
			switch (counter) {
			case 1 : entry->restriction_type = (num_entries > 2) ? '>' : '.';
				 break;
			case 2 : entry->restriction_type = '<'; break;
			case 3 : entry->restriction_type = '='; break;
			case 4 : entry->restriction_type = '@'; break;
			default :
				 break;
			}
		}
	}
}
#endif

#ifdef DEFINE_COMMON
/*
 * This routine is invoked when the last user of the
 * format is gone (ie, refcount has reached zero) just
 * before the GOFormat structure is actually released.
 *
 * resources allocated in format_compile should be disposed here
 */
static void
format_destroy (GOFormat *format)
{
	g_slist_foreach (format->entries, &format_entry_dtor, NULL);
	g_slist_free (format->entries);
	format->entries = NULL;
	if (format->markup != NULL) {
		pango_attr_list_unref (format->markup);
		format->markup = NULL;
	}
}
#endif

#ifdef DEFINE_COMMON
/* used to generate formats when delocalizing so keep the leadings caps */
typedef struct {
	char const *name;
	GOColor	 go_color;
} FormatColor;
static FormatColor const format_colors [] = {
	{ N_("Black"),	 RGBA_BLACK },
	{ N_("Blue"),	 RGBA_BLUE },
	{ N_("Cyan"),	 RGBA_CYAN },
	{ N_("Green"),	 RGBA_GREEN },
	{ N_("Magenta"), RGBA_VIOLET },
	{ N_("Red"),	 RGBA_RED },
	{ N_("White"),	 RGBA_WHITE },
	{ N_("Yellow"),	 RGBA_YELLOW }
};
#endif

#ifdef DEFINE_COMMON
static FormatColor const *
lookup_color_by_name (gchar const *str, gchar const *end,
		      gboolean translate)
{
	int i, len;

	len = end - str;
	for (i = G_N_ELEMENTS (format_colors) ; i-- > 0 ; ) {
		gchar const *name = format_colors[i].name;
		if (translate)
			name = _(name);

		if (0 == g_ascii_strncasecmp (name, str, len) && name[len] == '\0')
			return format_colors + i;
	}
	return NULL;
}
#endif

#ifdef DEFINE_COMMON
static GOColor
lookup_color (gchar const *str, gchar const *end)
{
	FormatColor const *color = lookup_color_by_name (str, end, FALSE);
	return (color != NULL) ? color->go_color : 0;
}
#endif

void
SUFFIX(go_render_number) (GString *result,
			  DOUBLE number,
			  GONumberFormat const *info)
{
	GString const *thousands_sep = format_get_thousand ();
	char num_buf[(PREFIX(MANT_DIG) + PREFIX(MAX_EXP)) * 2 + 1];
	gchar *num = num_buf + sizeof (num_buf) - 1;
	DOUBLE frac_part, int_part;
	int group, zero_count, digit_count = 0;
	int left_req = info->left_req;
	int right_req = info->right_req;
	int left_spaces = info->left_spaces;
	int right_spaces = info->right_spaces;
	int right_allowed = info->right_allowed + info->right_optional;
	int sigdig = 0;

	number = SUFFIX(go_add_epsilon) (number);

	if (right_allowed >= 0 && !info->has_fraction) {
		/* Change "rounding" into "truncating".   */
		/* Note, that we assume number >= 0 here. */
		DOUBLE delta = 5 * SUFFIX(go_pow10) (-right_allowed - 1);
		number += delta;
	}
	frac_part = SUFFIX(modf) (number, &int_part);

	*num = '\0';
	group = (info->group_thousands) ? 3 : -1;
	for (; int_part > SUFFIX(beyond_precision) ; int_part /= 10., digit_count++) {
		if (group-- == 0) {
			int i;
			group = 2;
			for (i = thousands_sep->len - 1; i >= 0; i--)
				*(--num) = thousands_sep->str[i];
		}
		*(--num) = '0';
		sigdig++;
	}

	for (; int_part >= 1. ; int_part /= 10., digit_count++) {
		DOUBLE r = SUFFIX(floor) (int_part);
		int digit = r - SUFFIX(floor) (r / 10) * 10;

		if (group-- == 0) {
			int i;
			group = 2;
			for (i = thousands_sep->len - 1; i >= 0; i--)
				*(--num) = thousands_sep->str[i];
		}
		*(--num) = digit + '0';
		sigdig++;
	}

	if (left_req > digit_count) {
		for (left_spaces -= left_req ; left_spaces-- > 0 ;)
			g_string_append_c (result, ' ');
		for (left_req -= digit_count ; left_req-- > 0 ;)
			g_string_append_c (result, '0');
	}

	g_string_append_len (result, num, num_buf + sizeof (num_buf) - 1 - num);

	/* If the format contains only "#"s to the left of the decimal
	 * point, number in the [0.0,1.0] range are prefixed with a
	 * decimal point
	 */
	if (info->decimal_separator_seen ||
	    (number > 0.0 &&
	     number < 1.0 &&
	     info->right_allowed == 0 &&
	     info->right_optional > 0))
		go_string_append_gstring (result, format_get_decimal ());

	/* TODO : clip this a DBL_DIG */
	/* TODO : What if is a fraction ? */
	right_allowed -= right_req;
	right_spaces  -= right_req;
	while (right_req-- > 0) {
		gint digit;
		frac_part *= 10.0;
		digit = (gint)frac_part;
		frac_part -= digit;
		if (++sigdig > PREFIX(DIG)) digit = 0;
		g_string_append_c (result, digit + '0');
	}

	zero_count = 0;

	while (right_allowed-- > 0) {
		gint digit;
		frac_part *= 10.0;
		digit = (gint)frac_part;
		frac_part -= digit;

		if (++sigdig > PREFIX(DIG)) digit = 0;

		if (digit != 0) {
			right_spaces -= zero_count + 1;
			zero_count = 0;
		} else
			zero_count ++;

		g_string_append_c (result, digit + '0');
	}

	g_string_truncate (result, result->len - zero_count);

	while (right_spaces-- > 0)
		g_string_append_c (result, ' ');
}

static void
SUFFIX(do_render_number) (DOUBLE number, GONumberFormat *info, GString *result)
{
	info->rendered = TRUE;

#if 0
	printf ("Rendering: %g with:\n", number);
	printf ("left_req:    %d\n"
		"right_req:   %d\n"
		"left_spaces: %d\n"
		"right_spaces:%d\n"
		"right_allow: %d\n"
		"supress:     %d\n"
		"decimalseen: %d\n"
		"decimalp:    %s\n",
		info->left_req,
		info->right_req,
		info->left_spaces,
		info->right_spaces,
		info->right_allowed + info->right_optional,
		info->decimal_separator_seen,
		decimal_point);
#endif

	SUFFIX(go_render_number) (result, info->scale * number, info);
}

/*
 * Microsoft Excel has a bug in the handling of year 1900,
 * I quote from http://catless.ncl.ac.uk/Risks/19.64.html#subj9.1
 *
 * > Microsoft EXCEL version 6.0 ("Office 95 version") and version 7.0 ("Office
 * > 97 version") believe that year 1900 is a leap year.  The extra February 29
 * > cause the following problems.
 * >
 * > 1)  All day-of-week before March 1, 1900 are incorrect;
 * > 2)  All date sequence (serial number) on and after March 1, 1900 are incorrect.
 * > 3)  Calculations of number of days across March 1, 1900 are incorrect.
 * >
 * > The risk of the error will cause must be little.  Especially case 1.
 * > However, import or export date using serial date number will be a problem.
 * > If no one noticed anything wrong, it must be that no one did it that way.
 */
static gboolean
SUFFIX(split_time) (struct tm *tm, DOUBLE number, GODateConventions const *date_conv)
{
	guint secs;
	GDate date;

	datetime_serial_to_g (&date,
		datetime_serial_raw_to_serial (number), date_conv);
	g_date_to_struct_tm (&date, tm);

	secs = datetime_serial_raw_to_seconds (number);
	tm->tm_hour = secs / 3600;
	secs -= tm->tm_hour * 3600;
	tm->tm_min  = secs / 60;
	secs -= tm->tm_min * 60;
	tm->tm_sec  = secs;

	return FALSE;
}

#ifdef DEFINE_COMMON
#define NUM_ZEROS 30
static char const zeros[NUM_ZEROS + 1]  = "000000000000000000000000000000";
static char const qmarks[NUM_ZEROS + 1] = "??????????????????????????????";
#endif

#ifdef DEFINE_COMMON
/**
 * go_format_as_number :
 * @fmt : #GOFormatDetails
 *
 * generate an unlocalized number format based on @fmt.
 **/
static GOFormat *
go_format_as_number (GOFormatDetails const *fmt)
{
	int symbol = fmt->currency_symbol_index;
	GString *str, *tmp;
	GOFormat *gf;

	g_return_val_if_fail (fmt->num_decimals >= 0, NULL);
	g_return_val_if_fail (fmt->num_decimals <= NUM_ZEROS, NULL);

	str = g_string_new (NULL);

	/* Currency */
	if (symbol != 0 && go_format_currencies[symbol].precedes) {
		g_string_append (str, go_format_currencies[symbol].symbol);
		if (go_format_currencies[symbol].has_space)
			g_string_append_c (str, ' ');
	}

	if (fmt->thousands_sep)
		g_string_append (str, "#,##0");
	else
		g_string_append_c (str, '0');

	if (fmt->num_decimals > 0) {
		g_string_append_c (str, '.');
		g_string_append_len (str, zeros, fmt->num_decimals);
	}

	/* Currency */
	if (symbol != 0 && !go_format_currencies[symbol].precedes) {
		if (go_format_currencies[symbol].has_space)
			g_string_append_c (str, ' ');
		g_string_append (str, go_format_currencies[symbol].symbol);
	}

	/* There are negatives */
	if (fmt->negative_fmt > 0) {
		size_t prelen = str->len;

		switch (fmt->negative_fmt) {
		case 1 : g_string_append (str, ";[Red]");
			break;
		case 2 : g_string_append (str, "_);(");
			break;
		case 3 : g_string_append (str, "_);[Red](");
			break;
		default :
			g_assert_not_reached ();
		};

		tmp = g_string_new_len (str->str, str->len);
		g_string_append_len (tmp, str->str, prelen);
		g_string_free (str, TRUE);
		str = tmp;

		if (fmt->negative_fmt >= 2)
			g_string_append_c (str, ')');
	}

	gf = go_format_new_from_XL (str->str, FALSE);
	g_string_free (str, TRUE);
	return gf;
}
#endif

#ifdef DEFINE_COMMON
static GOFormat *
style_format_fraction (GOFormatDetails const *fmt)
{
	GString *str = g_string_new (NULL);
	GOFormat *gf;

	if (fmt->fraction_denominator >= 2) {
		g_string_printf (str, "# ?/%d", fmt->fraction_denominator);
	} else {
		g_return_val_if_fail (fmt->num_decimals > 0, NULL);
		g_return_val_if_fail (fmt->num_decimals <= NUM_ZEROS, NULL);

		g_string_append (str, "# ");
		g_string_append_len (str, qmarks, fmt->num_decimals);
		g_string_append_c (str, '/');
		g_string_append_len (str, qmarks, fmt->num_decimals);
	}

	gf = go_format_new_from_XL (str->str, FALSE);
	g_string_free (str, TRUE);
	return gf;
}
#endif

#ifdef DEFINE_COMMON
static GOFormat *
go_format_as_percentage (GOFormatDetails const *fmt)
{
	GString *str;
	GOFormat *gf;

	g_return_val_if_fail (fmt->num_decimals >= 0, NULL);
	g_return_val_if_fail (fmt->num_decimals <= NUM_ZEROS, NULL);

	str = g_string_new (NULL);
	g_string_append_c (str, '0');
	if (fmt->num_decimals > 0) {
		g_string_append_c (str, '.');
		g_string_append_len (str, zeros, fmt->num_decimals);
	}
	g_string_append_c (str, '%');

	gf = go_format_new_from_XL (str->str, FALSE);
	g_string_free (str, TRUE);
	return gf;
}
#endif

#ifdef DEFINE_COMMON
static GOFormat *
go_format_as_scientific (GOFormatDetails const *fmt)
{
	GString *str;
	GOFormat *gf;

	g_return_val_if_fail (fmt->num_decimals >= 0, NULL);
	g_return_val_if_fail (fmt->num_decimals <= NUM_ZEROS, NULL);

	str = g_string_new (NULL);
	g_string_append_c (str, '0');
	if (fmt->num_decimals > 0) {
		g_string_append_c (str, '.');
		g_string_append_len (str, zeros, fmt->num_decimals);
	}
	g_string_append (str, "E+00");

	gf = go_format_new_from_XL (str->str, FALSE);
	g_string_free (str, TRUE);
	return gf;
}
#endif

#ifdef DEFINE_COMMON
static GOFormat *
go_format_as_account (GOFormatDetails const *fmt)
{
	GString *str, *sym, *num;
	GOFormat *gf;
	int symbol = fmt->currency_symbol_index;
	gboolean quote_currency;

	g_return_val_if_fail (fmt->num_decimals >= 0, NULL);
	g_return_val_if_fail (fmt->num_decimals <= NUM_ZEROS, NULL);

	str = g_string_new (NULL);
	/* The number with decimals */
	num = g_string_new ("#,##0");
	if (fmt->num_decimals > 0) {
		g_string_append_c (num, '.');
		g_string_append_len (num, zeros, fmt->num_decimals);
	}

	/* The currency symbols with space after or before */
	sym = g_string_new (NULL);
	quote_currency = (go_format_currencies[symbol].symbol[0] != '[');
	if (go_format_currencies[symbol].precedes) {
		if (quote_currency)
			g_string_append_c (sym, '\"');
		g_string_append (sym, go_format_currencies[symbol].symbol);
		if (quote_currency)
			g_string_append_c (sym, '\"');
		g_string_append (sym, "* ");
		if (go_format_currencies[symbol].has_space)
			g_string_append_c (sym, ' ');
	} else {
		g_string_append (sym, "* ");
		if (go_format_currencies[symbol].has_space)
			g_string_append_c (sym, ' ');
		if (quote_currency)
			g_string_append_c (sym, '\"');
		g_string_append (sym, go_format_currencies[symbol].symbol);
		if (quote_currency)
			g_string_append_c (sym, '\"');
	}

	/* Finally build the correct string */
	if (go_format_currencies[symbol].precedes) {
		g_string_append_printf (str, "_(%s%s_);_(%s(%s);_(%s\"-\"%s_);_(@_)",
					sym->str, num->str,
					sym->str, num->str,
					sym->str, (qmarks + NUM_ZEROS) - fmt->num_decimals);
	} else {
		g_string_append_printf (str, "_(%s%s_);_((%s)%s;_(\"-\"%s%s_);_(@_)",
					num->str, sym->str,
					num->str, sym->str,
					(qmarks + NUM_ZEROS) - fmt->num_decimals, sym->str);
	}

	g_string_free (num, TRUE);
	g_string_free (sym, TRUE);

	gf = go_format_new_from_XL (str->str, FALSE);
	g_string_free (str, TRUE);
	return gf;
}
#endif

#ifdef DEFINE_COMMON
/*
 * Finds the decimal char in @str doing the proper parsing of a
 * format string
 */
static char const *
find_decimal_char (char const *str)
{
	for (;*str; str++){
		if (*str == '.')
			return str;

		if (*str == ',')
			continue;

		switch (*str){
			/* These ones do not have any argument */
		case '#': case '?': case '0': case '%':
		case '-': case '+': case ')': case '£':
		case ':': case '$': case '¥': case '¤':
		case 'M': case 'm': case 'D': case 'd':
		case 'Y': case 'y': case 'S': case 's':
		case '*': case 'h': case 'H': case 'A':
		case 'a': case 'P': case 'p':
			break;

			/* Quoted string */
		case '"':
			for (str++; *str && *str != '"'; str++)
				;
			break;

			/* Escaped char and spacing format */
		case '\\': case '_':
			if (*(str + 1))
				str++;
			break;

			/* Scientific number */
		case 'E': case 'e':
			for (str++; *str;){
				if (*str == '+')
					str++;
				else if (*str == '-')
					str++;
				else if (*str == '0')
					str++;
				else
					break;
			}
		}
	}
	return NULL;
}
#endif

#ifdef DEFINE_COMMON
/* An helper function which modify the number of decimals displayed
 * and recreate the format string by calling the good function */
static GOFormat *
reformat_decimals (GOFormatDetails const *fc,
		   GOFormat * (*format_function) (GOFormatDetails const *fmt),
		   int step)
{
	GOFormatDetails fc_copy;

	/* Be sure that the number of decimals displayed will remain correct */
	if ((fc->num_decimals+step > NUM_ZEROS) || (fc->num_decimals+step <0))
		return NULL;
	fc_copy = *fc;
	fc_copy.num_decimals += step;

	return (*format_function) (&fc_copy);
}
#endif

#ifdef DEFINE_COMMON
/*
 * This routine scans the format_string for a decimal dot,
 * and if it finds it, it removes the first zero after it to
 * reduce the display precision for the number.
 *
 * Returns NULL if the new format would not change things
 */
GOFormat *
go_format_dec_precision (GOFormat const *fmt)
{
	int start;
	char *ret;
	char const *format_string = fmt->format;

	switch (fmt->family) {
	case GO_FORMAT_NUMBER:
	case GO_FORMAT_CURRENCY:
		return reformat_decimals (&fmt->family_info, &go_format_as_number, -1);
	case GO_FORMAT_ACCOUNTING:
		return reformat_decimals (&fmt->family_info, &go_format_as_account, -1);
	case GO_FORMAT_PERCENTAGE:
		return reformat_decimals (&fmt->family_info, &go_format_as_percentage, -1);
	case GO_FORMAT_SCIENTIFIC:
		return reformat_decimals (&fmt->family_info, &go_format_as_scientific, -1);
	case GO_FORMAT_FRACTION: {
		GOFormatDetails fc = fmt->family_info;

		if (fc.fraction_denominator >= 2) {
			if (fc.fraction_denominator > 2 &&
			    ((fc.fraction_denominator & (fc.fraction_denominator - 1)) == 0))
				/* It's a power of two.  */
				fc.fraction_denominator /= 2;
			else if (fc.fraction_denominator > 10 &&
				 fc.fraction_denominator % 10 == 0)
				/* It's probably a power of ten.  */
				fc.fraction_denominator /= 10;
			else
				return NULL;
		} else {
			if (fc.num_decimals <= 1)
				return NULL;
			fc.num_decimals--;
		}
		return style_format_fraction (&fc);
	}

	case GO_FORMAT_TIME:
		/* FIXME: we might have decimals on seconds part.  */
	case GO_FORMAT_DATE:
	case GO_FORMAT_TEXT:
	case GO_FORMAT_SPECIAL:
	case GO_FORMAT_MARKUP:
		/* Nothing to remove for these formats ! */
		return NULL;
	case GO_FORMAT_UNKNOWN:
	case GO_FORMAT_GENERAL:
		; /* Nothing.  */
	}

	/* Use the old code for more special formats to try to remove a
	   decimal */

	/*
	 * Consider General format as 0. with several optional decimal places.
	 * This is WRONG.  FIXME FIXME FIXME
	 * We need to look at the number of decimals in the current value
	 * and use that as a base.
	 */
	if (go_format_is_general (fmt))
		format_string = "0.########";

	start = 0;
	ret = g_strdup (format_string);
	while (1) {
		char *p = (char *)find_decimal_char (ret + start);
		int offset;

		if (!p)
			break;

		/* If there is more than 1 thing after the decimal place
		 * leave the decimal.
		 * If there is only 1 thing after the decimal remove the decimal too.
		 */
		if ((p[1] == '0' || p[1] == '#') && (p[2] == '0' || p[2] == '#'))
			offset = 1, ++p;
		else
			offset = 2;

		strcpy (p, p + offset);

		start = (p + 1) - ret;
	}

	if (start) {
		GOFormat *gf = go_format_new_from_XL (ret, FALSE);
		g_free (ret);
		return gf;
	} else {
		g_free (ret);
		return NULL;
	}
}
#endif

#ifdef DEFINE_COMMON
/**
 * go_format_inc_precision :
 * @fmt : #GOFormat
 * Scans @fmt for the decimal character and when it finds it, it adds a zero
 * after it to force the rendering of the number with one more digit of decimal
 * precision.
 *
 * Returns NULL if the new format would not change things
 **/
GOFormat *
go_format_inc_precision (GOFormat const *fmt)
{
	char const *pre = NULL;
	char const *post = NULL;
	char *res;
	char const *format_string = fmt->format;
	GOFormat *gf;

	switch (fmt->family) {
	case GO_FORMAT_NUMBER:
	case GO_FORMAT_CURRENCY:
		return reformat_decimals (&fmt->family_info, &go_format_as_number, +1);
	case GO_FORMAT_ACCOUNTING:
		return reformat_decimals (&fmt->family_info, &go_format_as_account, +1);
	case GO_FORMAT_PERCENTAGE:
		return reformat_decimals (&fmt->family_info, &go_format_as_percentage, +1);
	case GO_FORMAT_SCIENTIFIC:
		return reformat_decimals (&fmt->family_info, &go_format_as_scientific, +1);
	case GO_FORMAT_FRACTION: {
		GOFormatDetails fc = fmt->family_info;

		if (fc.fraction_denominator >= 2) {
			if (fc.fraction_denominator <= INT_MAX / 2 &&
			    ((fc.fraction_denominator & (fc.fraction_denominator - 1)) == 0))
				/* It's a power of two.  */
				fc.fraction_denominator *= 2;
			else if (fc.fraction_denominator <= INT_MAX / 10 &&
				 fc.fraction_denominator % 10 == 0)
				/* It's probably a power of ten.  */
				fc.fraction_denominator *= 10;
			else
				return NULL;
		} else {
			if (fc.num_decimals >= 5)
				return NULL;
			fc.num_decimals++;
		}
		return style_format_fraction (&fc);
	}

	case GO_FORMAT_TIME:
		/* FIXME: we might have decimals on seconds part.  */
	case GO_FORMAT_DATE:
	case GO_FORMAT_TEXT:
	case GO_FORMAT_SPECIAL:
	case GO_FORMAT_MARKUP:
		/* Nothing to add for these formats ! */
		return NULL;
	case GO_FORMAT_UNKNOWN:
	case GO_FORMAT_GENERAL:
		; /* Nothing.  */
	}

	/* Use the old code for more special formats to try to add a
	   decimal */

	if (go_format_is_general (fmt)) {
		format_string = "0";
		pre = format_string + 1;
		post = pre;
	} else {
		pre = find_decimal_char (format_string);

		/* If there is no decimal append to the last '0' */
		if (pre == NULL) {
			pre = strrchr (format_string, '0');

			/* If there are no 0s append to the ':s' */
			if (pre == NULL) {
				pre = strrchr (format_string, 's');
				if (pre > format_string && pre[-1] == ':') {
					if (pre[1] == 's')
						pre += 2;
					else
						++pre;
				} else
					return NULL;
			} else
				++pre;
			post = pre;
		} else
			post = pre + 1;
	}
	res = g_malloc ((pre - format_string + 1) +
		      1 + /* for the decimal */
		      1 + /* for the extra 0 */
		      strlen (post) +
		      1 /*terminate */);
	if (!res)
		return NULL;

	strncpy (res, format_string, pre - format_string);
	res[pre-format_string + 0] = '.';
	res[pre-format_string + 1] = '0';
	strcpy (res + (pre - format_string) + 2, post);

	gf = go_format_new_from_XL (res, FALSE);
	g_free (res);
	return gf;
}
#endif

#ifdef DEFINE_COMMON
GOFormat *
go_format_toggle_1000sep (GOFormat const *fmt)
{
	GOFormatDetails fc;

	fc = fmt->family_info;
	fc.thousands_sep = !fc.thousands_sep;

	switch (fmt->family) {
	case GO_FORMAT_NUMBER:
	case GO_FORMAT_CURRENCY:
		return go_format_as_number (&fc);

	case GO_FORMAT_ACCOUNTING:
		/*
		 * FIXME: this doesn't actually work as no 1000 seps
		 * are used for accounting.
		 */
		return go_format_as_account (&fc);
	case GO_FORMAT_GENERAL:
		fc.currency_symbol_index = 0;
		return go_format_as_number (&fc);

	default:
		break;
	}

	return NULL;
}
#endif

/*********************************************************************/

void
SUFFIX(go_format_number) (GString *result,
			  DOUBLE number, int col_width, GOFormatElement const *entry,
			  GODateConventions const *date_conv)
{
	gchar const *format = entry->format;
	GONumberFormat info;
	gboolean can_render_number = FALSE;
	gboolean hour_seen = FALSE;
	gboolean time_display_elapsed = FALSE;
	gboolean ignore_further_elapsed = FALSE;

	gunichar fill_char = 0;
	int fill_start = -1;

	gboolean need_time_split = TRUE;
	struct tm tm;
	DOUBLE signed_number;

	memset (&info, 0, sizeof (info));
	signed_number = number;
	if (number < 0.) {
		number = -number;
		if (!entry->suppress_minus)
			g_string_append_c (result, '-');
	}
	info.has_fraction = entry->has_fraction;
	info.scale = 1;

	while (*format) {
		/* This is just g_utf8_get_char, but we're in a hurry.  */
		gunichar c = (*format & 0x80) ? g_utf8_get_char (format) : *(guchar *)format;

		switch (c) {

		case '[':
			/* Currency symbol */
			if (format[1] == '$') {
				gboolean no_locale = TRUE;
				for (format += 2; *format && *format != ']' ; ++format)
					/* strip digits from [$<currency>-{digit}+] */
					if (*format == '-')
						no_locale = FALSE;
					else if (no_locale)
						g_string_append_c (result, *format);
				if (!*format)
					continue;
			} else if (!ignore_further_elapsed)
				time_display_elapsed = TRUE;
			break;

		case '#':
			can_render_number = TRUE;
			if (info.decimal_separator_seen)
				info.right_optional++;
			break;

		case '?':
			can_render_number = TRUE;
			if (info.decimal_separator_seen)
				info.right_spaces++;
			else
				info.left_spaces++;
			break;

		case '0':
			can_render_number = TRUE;
			if (info.decimal_separator_seen){
				info.right_req++;
				info.right_allowed++;
				info.right_spaces++;
			} else {
				info.left_spaces++;
				info.left_req++;
			}
			break;

		case '.': {
			int c = *(format + 1);

			can_render_number = TRUE;
			if (0 && c && (c != '0' && c != '#' && c != '?'))
				/*
				 * Before reinstating this, look at format
				 * "#,##0.\\-" with value 255.
				 */
				number /= 1000;
			else
				info.decimal_separator_seen = TRUE;
			break;
		}

		case ',':
			if (can_render_number) {
				gchar const *tmp = format;
				while (*++tmp == ',')
					;
				if (*tmp == '\0' || *tmp == '.' || *tmp == ';')
					/* NOTE : format-tmp is NEGATIVE */
					info.scale = SUFFIX(go_pow10) (3*(format-tmp));
				info.group_thousands = TRUE;
				format = tmp;
				continue;
			} else
				go_string_append_gstring (result, format_get_thousand ());
			break;

		/* FIXME: this is a gross hack */
		/* FIXME: Missing support for scientific notation.
		 * #00.00e###  that keeps axponent to a multiple of 3
		 * XL seems to just special case that.
		 **/
		case 'E': case 'e': {
			gboolean const is_lower = (*format++ == 'e');
			gboolean shows_plus = FALSE;
			int prec = info.right_optional + info.right_req;

			can_render_number = TRUE;
			if (*format == '+') {
				shows_plus = TRUE;
				format++;
			} else if (*format == '-')
				format++;

			while (*format == '0' || *format == '#')
				format++;

			g_string_append_printf (result,
						is_lower ? "%.*" FORMAT_e : "%.*" FORMAT_E,
						prec, number);
			return;
		}

		case '\\':
			if (format[1] != '\0') {
				if (can_render_number && !info.rendered)
					SUFFIX(do_render_number) (number, &info, result);

				format++;
				g_string_append_len (result, format,
					g_utf8_skip[*(guchar *)format]);
			}
			break;

		case '"': {
			gchar const *tmp = ++format;
			if (can_render_number && !info.rendered)
				SUFFIX(do_render_number) (number, &info, result);

			for (; *tmp && *tmp != '"'; tmp++)
				;
			g_string_append_len (result, format, tmp-format);
			format = tmp;
			if (!*format)
				continue;
			break;
		}

		case '/': /* fractions */
			if (can_render_number && info.left_spaces > info.left_req) {
				int size = 0;
				int numerator = -1, denominator = -1;

				while (format[size + 1] == '?')
					++size;

				/* check for explicit denominator */
				if (size == 0) {
					char *end;

					errno = 0;
					denominator = strtol ((char *)format + 1, &end, 10);
					if (format + 1 != end && errno != ERANGE) {
						size = end - (format + 1);
						format = end;
						numerator = (int)((number - (int)number) * denominator + 0.5);
					}
				} else {
					static int const powers[9] = {
						10, 100, 1000, 10000, 100000,
						1000000, 10000000, 100000000, 1000000000
					};

					format += size + 1;
					if (size > (int)G_N_ELEMENTS (powers))
						size = G_N_ELEMENTS (powers);
					go_continued_fraction (number - (int)number, powers[size - 1],
						&numerator, &denominator);
				}

				if (denominator > 0) {
					gboolean show_zero = TRUE;
					/* improper fractions */
					if (!info.rendered) {
						info.rendered = TRUE;
						numerator += ((int)number) * denominator;
					} else
						show_zero = (number == 0);

					/*
					 * FIXME: the space-aligning here doesn't come out
					 * right except in mono-space fonts.
					 */
					if (numerator > 0 || show_zero) {
						g_string_append_printf (result,
									"%*d/%-*d",
									info.left_spaces, numerator,
									size, denominator);
					} else {
						g_string_append_printf (result,
									"%-*s",
									info.left_spaces + 1 + size,
									"");
					}
					continue;
				}
			}

		case '-':
		case '(':
		case '+':
		case ':':
		case ' ': /* eg # ?/? */
		case '$':
		case 0x00A3 : /* pound */
		case 0x00A5 : /* yen */
		case 0x20AC : /* Euro */
		case ')':
			if (can_render_number && !info.rendered)
				SUFFIX(do_render_number) (number, &info, result);
			g_string_append_unichar (result, c);
			break;

		/* percent */
		case '%':
			if (!info.rendered) {
				number *= 100;
				if (can_render_number)
					SUFFIX(do_render_number) (number, &info, result);
				else
					can_render_number = TRUE;
			}
			g_string_append_c (result, '%');
			break;

		case '_':
			if (can_render_number && !info.rendered)
				SUFFIX(do_render_number) (number, &info, result);
			if (format[1])
				format++;
			g_string_append_c (result, ' ');
			break;

		case '*':
			/* Intentionally forget any previous fill characters
			 * (no need to be smart).
			 * FIXME : make the simplifying assumption that we are
			 * not going to fill in the middle of a number.  This
			 * assumption is WRONG! but ok until we rewrite the
			 * format engine.
			 */
			if (format[1]) {
				if (can_render_number && !info.rendered)
					SUFFIX(do_render_number) (number, &info, result);
				++format;
				fill_char = g_utf8_get_char (format);
				fill_start = result->len;
			}
			break;

		case 'M':
		case 'm': {
			int n;

			/* FIXME : Yuck
			 * This is a problem waiting to happen.
			 * rewrite.
			 */
			for (n = 1; format[1] == 'M' || format[1] == 'm'; format++)
				n++;
			if (format[1] == ']')
				format++;
			if (time_display_elapsed) {
				need_time_split = time_display_elapsed = FALSE;
				ignore_further_elapsed = TRUE;
				SUFFIX(append_minute_elapsed) (result, &tm, number);
				break;
			}

			if (need_time_split)
				need_time_split = SUFFIX(split_time) (&tm, signed_number, date_conv);
			if (hour_seen ||
			    (format[1] == ':' &&
			     (format[2] == 's' || format[2] == 'S'))) {
				append_minute (result, n, &tm);
			} else
				append_month (result, n, &tm);
			break;
		}

		case 'D':
		case 'd':
			if (need_time_split)
				need_time_split = SUFFIX(split_time) (&tm, signed_number, date_conv);
			format += append_day (result, format, &tm) - 1;
			break;

		case 'Y':
		case 'y':
			if (need_time_split)
				need_time_split = SUFFIX(split_time) (&tm, signed_number, date_conv);
			format += append_year (result, format, &tm) - 1;
			break;

		case 'S':
		case 's': {
			int n;

			for (n = 1; format[1] == 's' || format[1] == 'S'; format++)
				n++;
			if (format[1] == ']')
				format++;
			if (time_display_elapsed) {
				need_time_split = time_display_elapsed = FALSE;
				ignore_further_elapsed = TRUE;
				SUFFIX(append_second_elapsed) (result, number);
			} else {
				if (need_time_split)
					need_time_split = SUFFIX(split_time) (&tm, signed_number, date_conv);
				append_second (result, n, &tm);

				if (format[1] == '.') {
					/* HACK for fractional seconds.  */
					DOUBLE days, secs;
					int decs = 0;
					int old_len = result->len;
					format++;
					while (format[1] == '0')
						decs++, format++;

					secs = SUFFIX(modf) (SUFFIX(fabs) (number), &days);
					secs = SUFFIX(modf) (secs * (24 * 60 * 60), &days);

					if (decs > 0)
						g_string_append_printf (result, "%.*" FORMAT_f, decs, secs);
					/* Remove the "0" or "1" before the dot.  */
					g_string_erase (result, old_len, 1);
				}
			}
			break;
		}

		case 'H':
		case 'h': {
			int n;

			for (n = 1; format[1] == 'h' || format[1] == 'H'; format++)
				n++;
			if (format[1] == ']')
				format++;
			if (time_display_elapsed) {
				need_time_split = time_display_elapsed = FALSE;
				ignore_further_elapsed = TRUE;
				SUFFIX(append_hour_elapsed) (result, &tm, number);
			} else {
				/* h == hour optionally in 24 hour mode
				 * h followed by am/pm puts it in 12 hour mode
				 *
				 * more than 2 h eg 'hh' force 12 hour mode.
				 * NOTE : This is a non-XL extension
				 */
				if (need_time_split)
					need_time_split = SUFFIX(split_time) (&tm, signed_number, date_conv);

				append_hour (result, n, &tm, entry->want_am_pm);
			}
			hour_seen = TRUE;
			break;
		}

		case 'A':
		case 'a':
			if (need_time_split)
				need_time_split = SUFFIX(split_time) (&tm, signed_number, date_conv);
			if (tm.tm_hour < 12){
				g_string_append_c (result, *format);
				format++;
				if (*format == 'm' || *format == 'M'){
					g_string_append_c (result, *format);
					if (*(format + 1) == '/')
						format++;
				}
			} else {
				if (*(format + 1) == 'm' || *(format + 1) == 'M')
					format++;
				if (*(format + 1) == '/')
					format++;
			}
			break;

		case 'P': case 'p':
			if (need_time_split)
				need_time_split = SUFFIX(split_time) (&tm, signed_number, date_conv);
			if (tm.tm_hour >= 12){
				g_string_append_c (result, *format);
				if (*(format + 1) == 'm' || *(format + 1) == 'M'){
					format++;
					g_string_append_c (result, *format);
				}
			} else {
				if (*(format + 1) == 'm' || *(format + 1) == 'M')
					format++;
			}
			break;

		default:
			/* TODO : After release check this.
			 * shouldn't we tack on the explicit characters here ?
			 */
			break;
		}
		format = g_utf8_next_char (format);
	}

	if (!info.rendered && can_render_number)
		SUFFIX(do_render_number) (number, &info, result);

	/* This is kinda ugly.  It does not handle variable width fonts */
	if (fill_char != '\0') {
		int count = col_width - result->len;
		while (count-- > 0)
			g_string_insert_unichar (result, fill_start, fill_char);
	}
}

/**
 * go_fmt_general_float:
 *
 * @val : the integer value being formated.
 * @col_width : the approximate width in characters.
 **/
void
SUFFIX(go_fmt_general_float) (GString *result, DOUBLE val, double col_width)
{
	DOUBLE tmp;
	int log_val, prec;

	if (col_width < 0.) {
		g_string_append_printf (result, "%.*" FORMAT_G, PREFIX(DIG), val);
		return;
	}

	if (val < 0.) {
		/* leave space for minus sign */
		/* FIXME : idealy we would use the width of a minus sign */
		col_width -= 1.;
		tmp = SUFFIX(log10) (-val);
	} else
		tmp = (val > 0.) ? SUFFIX(log10) (val) : 0;

	/* leave space for the decimal */
	/* FIXME : idealy we would use the width of a decimal point */
	prec = (int) floor (col_width - .4);
	if (prec < 0)
		prec = 0;

	if (tmp > 0.) {
		log_val = SUFFIX(ceil) (tmp);

		/* Decrease precision to leave space for the E+00 */
		if (log_val > prec)
			for (prec -= 4; log_val >= 100 ; log_val /= 10)
				prec--;
	} else {
		log_val = SUFFIX(floor) (tmp);

		/* Display 0 for cols that are too narrow for scientific
		 * notation with abs (value) < 1 */
		if (col_width < 5. && -log_val >= prec) {
			g_string_append_c (result, '0');
			return;
		}

		/* Include leading zeros eg 0.0x has 2 leading zero */
		if (log_val >= -4)
			prec += log_val;

		/* Decrease precision to leave space for the E+00 */
		else for (prec -= 4; log_val <= -100 ; log_val /= 10)
			prec--;
	}

	if (prec < 1)
		prec = 1;
	else if (prec > PREFIX(DIG))
		prec = PREFIX(DIG);

	g_string_append_printf (result, "%.*" FORMAT_G, prec, val);
}

#ifdef DEFINE_COMMON
/**
 * go_fmt_general_int :
 *
 * @val : the integer value being formated.
 * @col_width : the approximate width in characters.
 */
void
go_fmt_general_int (GString *result, int val, int col_width)
{
	if (col_width > 0) {
		int log_val;

		if (val < 0) {
			/* leave space for minus sign */
			col_width--;
			log_val = ceil (log10 ((unsigned int)-val));
		} else
			log_val = (val > 0) ? ceil (log10 (val)) : 0;

		/* Switch to scientific notation if things are too wide */
		if (log_val > col_width) {
			/* Decrease available width by 5 to account for .+E00 */
			g_string_append_printf (result, "%.*G", col_width - 5, (double)val);
			return;
		}
	}

	/* FIXME: we can do better than this.  */
	g_string_append_printf (result, "%d", val);
}
#endif


#ifdef DEFINE_COMMON
void
number_format_init (void)
{
	style_format_hash = g_hash_table_new (g_str_hash, g_str_equal);

	beyond_precision = go_pow10 (DBL_DIG) + 1;
#ifdef GOFFICE_WITH_LONG_DOUBLE
	beyond_precisionl = go_pow10l (LDBL_DIG) + 1;
#endif

	lc_decimal = g_string_new (NULL);
	lc_thousand = g_string_new (NULL);
	lc_currency = g_string_new (NULL);
}
#endif

#ifdef DEFINE_COMMON
static void
cb_format_leak (gpointer key, gpointer value, gpointer user_data)
{
	GOFormat *format = value;

	fprintf (stderr, "Leaking GOFormat at %p [%s].\n",
		 format, format->format);
}
#endif

#ifdef DEFINE_COMMON
void
number_format_shutdown (void)
{
	g_string_free (lc_decimal, TRUE);
	lc_decimal = NULL;

	g_string_free (lc_thousand, TRUE);
	lc_thousand = NULL;

	g_string_free (lc_currency, TRUE);
	lc_currency = NULL;

	if (default_percentage_fmt) {
		go_format_unref (default_percentage_fmt);
		default_percentage_fmt = NULL;
	}

	if (default_money_fmt) {
		go_format_unref (default_money_fmt);
		default_money_fmt = NULL;
	}

	if (default_date_fmt) {
		go_format_unref (default_date_fmt);
		default_date_fmt = NULL;
	}

	if (default_time_fmt) {
		go_format_unref (default_time_fmt);
		default_time_fmt = NULL;
	}

	if (default_date_time_fmt) {
		go_format_unref (default_date_time_fmt);
		default_date_time_fmt = NULL;
	}

	if (default_general_fmt) {
		go_format_unref (default_general_fmt);
		default_general_fmt = NULL;
	}

	g_hash_table_foreach (style_format_hash, cb_format_leak, NULL);
	g_hash_table_destroy (style_format_hash);
	style_format_hash = NULL;
}
#endif

/****************************************************************************/

#ifdef DEFINE_COMMON
static char *
translate_format_color (GString *res, char const *ptr, gboolean translate_to_en)
{
	char *end;
	FormatColor const *color;

	g_string_append_c (res, '[');

	/*
	 * Special [h*], [m*], [*s] is using for
	 * and [$*] are for currencies.
	 * measuring times, not for specifying colors.
	 */
	if (ptr[1] == 'h' || ptr[1] == 's' || ptr[1] == 'm' || ptr[1] == '$')
		return NULL;

	end = strchr (ptr, ']');
	if (end == NULL)
		return NULL;

	color = lookup_color_by_name (ptr+1, end, translate_to_en);
	if (color != NULL) {
		g_string_append (res, translate_to_en
			? color->name : _(color->name));
		g_string_append_c (res, ']');
		return end;
	}
	return NULL;
}
#endif

#ifdef DEFINE_COMMON
char *
go_format_str_delocalize (char const *descriptor_string)
{
	g_return_val_if_fail (descriptor_string != NULL, NULL);

	if (*descriptor_string == '\0')
		return g_strdup ("");

	if (strcmp (descriptor_string, _("General"))) {
		GString const *thousands_sep = format_get_thousand ();
		GString const *decimal = format_get_decimal ();
		char const *ptr = descriptor_string;
		GString *res = g_string_sized_new (strlen (ptr));

		for ( ; *ptr ; ++ptr) {
			if (strncmp (ptr, decimal->str, decimal->len) == 0) {
				ptr += decimal->len - 1;
				g_string_append_c (res, '.');
			} else if (strncmp (ptr, thousands_sep->str, thousands_sep->len) == 0) {
				ptr += thousands_sep->len - 1;
				g_string_append_c (res, ',');
			} else if (*ptr == '\"') {
				do {
					g_string_append_c (res, *ptr++);
				} while (*ptr && *ptr != '\"');
				if (*ptr)
					g_string_append_c (res, *ptr);
			} else if (*ptr == '[') {
				char *tmp = translate_format_color (res, ptr, TRUE);
				if (tmp != NULL)
					ptr = tmp;
			} else {
				if (*ptr == '\\' && ptr[1] != '\0') {
					ptr++;
					/* Ignore '\' if we probably added it */
					if (strncmp (ptr, decimal->str, decimal->len) != 0 &&
					    strncmp (ptr, thousands_sep->str, thousands_sep->len) != 0)
						g_string_append_c (res, '\\');
				}
				g_string_append_c (res, *ptr);
			}
		}
		return g_string_free (res, FALSE);
	} else
		return g_strdup ("General");
}
#endif

#ifdef DEFINE_COMMON
static gboolean
cb_attrs_as_string (PangoAttribute *a, GString *accum)
{
	PangoColor const *c;

	if (a->start_index >= a->end_index)
		return FALSE;

	switch (a->klass->type) {
	case PANGO_ATTR_FAMILY :
		g_string_append_printf (accum, "[family=%s",
			((PangoAttrString *)a)->value);
		break;
	case PANGO_ATTR_SIZE :
		g_string_append_printf (accum, "[size=%d",
			((PangoAttrInt *)a)->value);
		break;
	case PANGO_ATTR_STYLE :
		g_string_append_printf (accum, "[italic=%d",
			(((PangoAttrInt *)a)->value == PANGO_STYLE_ITALIC) ? 1 : 0);
		break;
	case PANGO_ATTR_WEIGHT :
		g_string_append_printf (accum, "[bold=%d",
			(((PangoAttrInt *)a)->value >= PANGO_WEIGHT_BOLD) ? 1 : 0);
		break;
	case PANGO_ATTR_STRIKETHROUGH :
		g_string_append_printf (accum, "[strikthrough=%d",
			((PangoAttrInt *)a)->value ? 1 : 0);
		break;
	case PANGO_ATTR_UNDERLINE :
		switch (((PangoAttrInt *)a)->value) {
		case PANGO_UNDERLINE_NONE :
			g_string_append (accum, "[underline=none");
			break;
		case PANGO_UNDERLINE_SINGLE :
			g_string_append (accum, "[underline=single");
			break;
		case PANGO_UNDERLINE_DOUBLE :
			g_string_append (accum, "[underline=double");
			break;
		}
		break;

	case PANGO_ATTR_FOREGROUND :
		c = &((PangoAttrColor *)a)->color;
		g_string_append_printf (accum, "[color=%02xx%02xx%02x",
			((c->red & 0xff00) >> 8),
			((c->green & 0xff00) >> 8),
			((c->blue & 0xff00) >> 8));
		break;
	default :
		return FALSE; /* ignored */
	}
	g_string_append_printf (accum, ":%u:%u]", a->start_index, a->end_index);
	return FALSE;
}
#endif

#ifdef DEFINE_COMMON
static PangoAttrList *
go_format_parse_markup (char *str)
{
	PangoAttrList *attrs;
	PangoAttribute *a;
	char *closer, *val, *val_end;
	unsigned len;
	int r, g, b;

	g_return_val_if_fail (*str == '@', NULL);

	attrs = pango_attr_list_new ();
	for (str++ ; *str ; str = closer + 1) {
		g_return_val_if_fail (*str == '[', attrs);
		str++;

		val = strchr (str, '=');
		g_return_val_if_fail (val != NULL, attrs);
		len = val - str;
		val++;

		val_end = strchr (val, ':');
		g_return_val_if_fail (val_end != NULL, attrs);

		closer = strchr (val_end, ']');
		g_return_val_if_fail (closer != NULL, attrs);
		*val_end = '\0';
		*closer = '\0';

		a = NULL;
		switch (len) {
		case 4:
			if (0 == strncmp (str, "size", 4))
				a = pango_attr_size_new (atoi (val));
			else if (0 == strncmp (str, "bold", 4))
				a = pango_attr_weight_new (atoi (val) ? PANGO_WEIGHT_BOLD : PANGO_WEIGHT_NORMAL);
			break;

		case 5:
			if (0 == strncmp (str, "color", 5) &&
			    3 == sscanf (val, "%02xx%02xx%02x", &r, &g, &b))
				a = pango_attr_foreground_new ((r << 8) | r, (g << 8) | g, (b << 8) | b);
			break;

		case 6:
			if (0 == strncmp (str, "family", 6))
				a = pango_attr_family_new (val);
			else if (0 == strncmp (str, "italic", 6))
				a = pango_attr_style_new (atoi (val) ? PANGO_STYLE_ITALIC : PANGO_STYLE_NORMAL);
			break;

		case 9:
			if (0 == strncmp (str, "underline", 9)) {
				if (0 == strcmp (val, "none"))
					a = pango_attr_underline_new (PANGO_UNDERLINE_NONE);
				else if (0 == strcmp (val, "single"))
					a = pango_attr_underline_new (PANGO_UNDERLINE_SINGLE);
				else if (0 == strcmp (val, "double"))
					a = pango_attr_underline_new (PANGO_UNDERLINE_DOUBLE);
			}
			break;

		case 13:
			if (0 == strncmp (str, "strikethrough", 13))
				a = pango_attr_strikethrough_new (atoi (val) != 0);
			break;
		}

		if (a != NULL && val_end != NULL) {
			if (sscanf (val_end+1, "%u:%u]", &a->start_index, &a->end_index) == 2 &&
				a->start_index < a->end_index)
				pango_attr_list_insert (attrs, a);
			else
				pango_attribute_destroy (a);
		}

		*val_end = ':';
		*closer = ']';
	}

	return attrs;
}
#endif

#ifdef DEFINE_COMMON
/**
 * go_format_new_from_XL :
 *
 * Looks up and potentially creates a GOFormat from the supplied string in
 * XL format.
 *
 * @descriptor_string: XL descriptor in UTF-8 encoding.
 **/
GOFormat *
go_format_new_from_XL (char const *descriptor_string, gboolean delocalize)
{
	GOFormat *format;
	char *desc_copy = NULL;

	/* Safety net */
	if (descriptor_string == NULL) {
		g_warning ("Invalid format descriptor string, using General");
		descriptor_string = "General";
	} else if (delocalize)
		descriptor_string = desc_copy = go_format_str_delocalize (descriptor_string);

	format = (GOFormat *) g_hash_table_lookup (style_format_hash, descriptor_string);

	if (!format) {
		format = g_new0 (GOFormat, 1);
		format->format = g_strdup (descriptor_string);
		format->entries = NULL;
		format->family = go_format_classify (format, &format->family_info);
		format->is_var_width = FALSE;
		if (format->family == GO_FORMAT_MARKUP)
			format->markup = go_format_parse_markup (format->format);
		else if (!go_format_is_general (format))
			format_compile (format);
		else
			format->is_var_width = TRUE;

		g_hash_table_insert (style_format_hash, format->format, format);
	}
	format->ref_count++;
#ifdef DEBUG_REF_COUNT
	g_message (__FUNCTION__ " format=%p '%s' ref_count=%d",
		   format, format->format, format->ref_count);
#endif

	g_free (desc_copy);
	return format;
}
#endif

#ifdef DEFINE_COMMON
/**
 * go_format_new_markup :
 * @markup : #PangoAttrList
 * @add_ref :
 *
 * Create a MARKUP format.  If @add_ref is FALSE absorb the reference to
 * @markup, otherwise add a reference.
 */
GOFormat *
go_format_new_markup (PangoAttrList *markup, gboolean add_ref)
{
	GOFormat *format = g_new0 (GOFormat, 1);
	GString *accum = g_string_new ("@");

	pango_attr_list_filter (markup,
		(PangoAttrFilterFunc) cb_attrs_as_string, accum);

	format->format = g_string_free (accum, FALSE);
	format->entries = NULL;
	format->family = GO_FORMAT_MARKUP;
	format->markup = markup;
	if (add_ref)
		pango_attr_list_ref (markup);

	g_hash_table_insert (style_format_hash, format->format, format);
	format->ref_count++;

#ifdef DEBUG_REF_COUNT
	g_message (__FUNCTION__ " format=%p '%s' ref_count=%d",
		   format, format->format, format->ref_count);
#endif

	return format;
}
#endif


#ifdef DEFINE_COMMON
GOFormat *
go_format_new (GOFormatFamily family, GOFormatDetails const *info)
{
	switch (family) {
	case GO_FORMAT_GENERAL:
	case GO_FORMAT_TEXT:
		return go_format_new_from_XL (go_format_builtins[family][0], FALSE);

	case GO_FORMAT_NUMBER: {
		/* Make sure no currency is selected */
		GOFormatDetails info_copy = *info;
		info_copy.currency_symbol_index = 0;
		return go_format_as_number (&info_copy);
	}

	case GO_FORMAT_CURRENCY:
		return go_format_as_number (info);

	case GO_FORMAT_ACCOUNTING:
		return go_format_as_account (info);

	case GO_FORMAT_PERCENTAGE:
		return go_format_as_percentage (info);

	case GO_FORMAT_SCIENTIFIC:
		return go_format_as_scientific (info);

	default:
	case GO_FORMAT_DATE:
	case GO_FORMAT_TIME:
		return NULL;
	};
}
#endif


#ifdef DEFINE_COMMON
/**
 * go_format_str_as_XL
 *
 * The caller is responsible for freeing the resulting string.
 */
char *
go_format_str_as_XL (char const *ptr, gboolean localized)
{
	GString const *thousands_sep, *decimal;
	GString *res;

	g_return_val_if_fail (ptr != NULL,
			      g_strdup (localized ? _("General") : "General"));

	if (!localized)
		return g_strdup (ptr);

	if (!strcmp (ptr, "General"))
		return g_strdup (_("General"));

	thousands_sep = format_get_thousand ();
	decimal = format_get_decimal ();

	res = g_string_sized_new (strlen (ptr));

	/* TODO : XL seems to do an adaptive escaping of
	 * things.
	 * eg '#,##0.00 ' in a locale that uses ' '
	 * as the thousands would become
	 *    '# ##0.00 '
	 * rather than
	 *    '# ##0.00\ '
	 *
	 * TODO : Minimal quotes.
	 * It also seems to have a display mode vs a storage mode.
	 * Internally it adds a few quotes around strings.
	 * Then tries not to display the quotes unless needed.
	 */
	for ( ; *ptr ; ++ptr)
		switch (*ptr) {
		case '.':
			go_string_append_gstring (res, decimal);
			break;
		case ',':
			go_string_append_gstring (res, thousands_sep);
			break;

		case '\"':
			do {
				g_string_append_c (res, *ptr++);
			} while (*ptr && *ptr != '\"');
			if (*ptr)
				g_string_append_c (res, *ptr);
			break;

		case '\\':
			g_string_append_c (res, '\\');
			if (ptr[1] != '\0') {
				g_string_append_c (res, ptr[1]);
				++ptr;
			}
			break;

		case '[': {
			char *tmp = translate_format_color (res, ptr, FALSE);
			if (tmp != NULL)
				ptr = tmp;
			break;
		}

		default:
			if (strncmp (ptr, decimal->str, decimal->len) == 0 ||
			    strncmp (ptr, thousands_sep->str, thousands_sep->len) == 0)
				g_string_append_c (res, '\\');
			g_string_append_c (res, *ptr);
		}

	return g_string_free (res, FALSE);
}
#endif

#ifdef DEFINE_COMMON
/**
 * go_format_as_XL :
 * @gf :
 * @localized : should the string be in cannonical or locale specific form.
 *
 * Return a string which the caller is responsible for freeing.
 */
char *
go_format_as_XL (GOFormat const *fmt, gboolean localized)
{
	g_return_val_if_fail (fmt != NULL,
			      g_strdup (localized ? _("General") : "General"));

	return go_format_str_as_XL (fmt->format, localized);
}
#endif

#ifdef DEFINE_COMMON
gboolean
go_format_eq (GOFormat const *a, GOFormat const *b)
{
	/*
	 * The way we create GOFormat *s ensures that we don't need
	 * to compare anything but pointers.
	 */
	return (a == b);
}
#endif

#ifdef DEFINE_COMMON
/**
 * go_format_ref :
 * @gf :
 *
 * Add a reference to a GOFormat
 **/
GOFormat *
go_format_ref (GOFormat *gf)
{
	g_return_val_if_fail (gf != NULL, NULL);

	gf->ref_count++;
#ifdef DEBUG_REF_COUNT
	g_message (__FUNCTION__ " format=%p '%s' ref_count=%d",
		   gf, gf->format, gf->ref_count);
#endif

	return gf;
}
#endif

#ifdef DEFINE_COMMON
/**
 * go_format_unref :
 * @gf :
 *
 * Remove a reference to a GOFormat, freeing when it goes to zero.
 **/
void
go_format_unref (GOFormat *gf)
{
	if (gf == NULL)
		return;

	g_return_if_fail (gf->ref_count > 0);

	gf->ref_count--;
#ifdef DEBUG_REF_COUNT
	g_message (__FUNCTION__ " format=%p '%s' ref_count=%d",
		   gf, gf->format, gf->ref_count);
#endif
	if (gf->ref_count != 0)
		return;

	g_hash_table_remove (style_format_hash, gf->format);

	format_destroy (gf);
	g_free (gf->format);
	g_free (gf);
}
#endif

#ifdef DEFINE_COMMON
GOFormat *
go_format_general (void)
{
	if (!default_general_fmt)
		default_general_fmt = go_format_new_from_XL (
			go_format_builtins[GO_FORMAT_GENERAL][0], FALSE);
	return default_general_fmt;
}
#endif

#ifdef DEFINE_COMMON
GOFormat *
go_format_default_date (void)
{
	if (!default_date_fmt)
		default_date_fmt = go_format_new_from_XL (
			go_format_builtins[GO_FORMAT_DATE][0], FALSE);
	return default_date_fmt;
}
#endif

#ifdef DEFINE_COMMON
GOFormat *
go_format_default_time (void)
{
	if (!default_time_fmt)
		default_time_fmt = go_format_new_from_XL (
			go_format_builtins[GO_FORMAT_TIME][0], FALSE);
	return default_time_fmt;
}
#endif

#ifdef DEFINE_COMMON
GOFormat *
go_format_default_date_time (void)
{
	if (!default_date_time_fmt)
		default_date_time_fmt = go_format_new_from_XL (
			go_format_builtins[GO_FORMAT_TIME][4], FALSE);
	return default_date_time_fmt;
}
#endif

#ifdef DEFINE_COMMON
GOFormat *
go_format_default_percentage (void)
{
	if (!default_percentage_fmt)
		default_percentage_fmt = go_format_new_from_XL (
			go_format_builtins[GO_FORMAT_PERCENTAGE][1], FALSE);
	return default_percentage_fmt;
}
#endif

#ifdef DEFINE_COMMON
GOFormat *
go_format_default_money (void)
{
	if (!default_money_fmt)
		default_money_fmt = go_format_new_from_XL (
			go_format_builtins[GO_FORMAT_CURRENCY][2], FALSE);
	return default_money_fmt;
}
#endif
