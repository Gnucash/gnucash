/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * number-match.c: This file includes the support for matching
 * entered strings as numbers (by trying to apply one of the existing
 * cell formats).
 *
 * The idea is simple: we create a regular expression from the format
 * string that would match a value entered in that format.  Then, on
 * lookup we try to match the string against every regular expression
 * we have: if a match is found, then we decode the number using a
 * precomputed parallel-list of subexpressions.
 *
 * Author:
 *   Miguel de Icaza (miguel@gnu.org)
 */
#include <config.h>
#include <glib/gi18n.h>
#include "gnumeric.h"
#include "number-match.h"

#include "dates.h"
#include "numbers.h"
#include "gutils.h"
#include "datetime.h"
#include "style.h"
#include "format.h"
#include "value.h"
#include "mathfunc.h"
#include "str.h"
#include "regutf8.h"

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <locale.h>
#include <math.h>
#undef DEBUG_NUMBER_MATCH

/*
 * Takes a list of strings (optionally include an * at the beginning
 * that gets stripped, for i18n purposes). and returns a regexp that
 * would match them
 */
static char *
create_option_list (char const *const *list)
{
	int len = 0;
	char const *const *p;
	char *res;

	for (p = list; *p; p++) {
		char const *v = _(*p);

		if (*v == '*')
			v++;
		len += strlen (v) + 1;
	}
	len += 5;

	res = g_malloc (len);
	res[0] = '(';
	res[1] = 0;
	for (p = list; *p; p++) {
		char const *v = _(*p);

		if (*v == '*')
			v++;

		strcat (res, v);
		if (*(p + 1))
		    strcat (res, "|");
	}
	strcat (res, ")");

	return res;
}

typedef enum {
	MATCH_DAY_FULL 		  = 1,
	MATCH_DAY_NUMBER	  = 2,
	MATCH_MONTH_FULL	  = 3,
	MATCH_MONTH_SHORT	  = 4,
	MATCH_MONTH_NUMBER	  = 5,
	MATCH_YEAR_FULL		  = 6,
	MATCH_YEAR_SHORT	  = 7,
	MATCH_HOUR		  = 8,
	MATCH_MINUTE		  = 9,
	MATCH_SECOND		  = 10,
	MATCH_AMPM		  = 11,
	MATCH_NUMBER		  = 12,
	MATCH_NUMBER_DECIMALS	  = 13,
	MATCH_PERCENT		  = 14,
	MATCH_SKIP		  = 15,
	MATCH_STRING_CONSTANT	  = 16,
	MATCH_CUMMULATIVE_HOURS	  = 17,
	MATCH_CUMMULATIVE_MINUTES = 18,
	MATCH_CUMMULATIVE_SECONDS = 19,
	MATCH_NUMERATOR           = 20,
	MATCH_DENOMINATOR         = 21
} MatchType;

#define append_type(t) do { guint8 x = t; match_types = g_byte_array_append (match_types, &x, 1); } while (0)

/*
 * format_create_regexp:
 * Create a regular expression for the given XL-style format.  Note:
 * the format as well as the regexp are UTF-8 encoded.
 */
static char *
format_create_regexp (unsigned char const *format, GByteArray **dest)
{
	GString *regexp;
	GByteArray *match_types;
	char *str;
	gboolean hour_seen = FALSE;
	gboolean number_seen = FALSE;
	gboolean fraction = FALSE;

	g_return_val_if_fail (format != NULL, NULL);

#ifdef DEBUG_NUMBER_MATCH
	printf ("'%s' = ", format);
#endif
	regexp = g_string_new ("^");
	match_types = g_byte_array_new ();

	for (; *format; format = g_utf8_next_char (format)) {
		gunichar c = g_utf8_get_char (format);
		switch (c) {
		case '*':
			/* FIXME: I don't think this will work for '^'.  */
			if (format[1]) {
				format++;
				g_string_append_c (regexp, '[');
				g_string_append_unichar (regexp, g_utf8_get_char (format));
				g_string_append_c (regexp, ']');
				g_string_append_c (regexp, '*');
			}
			break;

		case 'P': case 'p':
			if (format[1] == 'm' || format[1] == 'M')
				format++;
			break;

		case '\\': {
			if (format[1] != '\0')
				format++;
			gnumeric_regexp_quote1 (regexp, format);
			break;
		}

		case '[' :
			/* Currency symbol */
			if (format[1] == '$') {
				for (format += 2; *format && *format != ']' ; ++format)
					g_string_append_c (regexp, *format);
				if (*format == ']')
					++format;
				break;
			} else if (format[1] == 'h' && format[2] == ']') {
				g_string_append (regexp, "([-+]?[0-9]+)");
				append_type (MATCH_CUMMULATIVE_HOURS);
				hour_seen = TRUE;
				format += 2;
				break;
			} else if (format[1] == 'm' && format[2] == ']') {
				g_string_append (regexp, "([-+]?[0-9]+)");
				append_type (hour_seen ? MATCH_MINUTE : MATCH_CUMMULATIVE_MINUTES);
				format += 2;
				break;
			} else if (format[1] == 's' && format[2] == ']') {
				g_string_append (regexp, "([-+]?[0-9]+)");
				append_type (MATCH_CUMMULATIVE_SECONDS);
				format += 2;
				break;
			}

		case '%':
			g_string_append (regexp, "%");
			append_type (MATCH_PERCENT);
			break;

		case '#': case '0': case '.': case '+': case '?': {
			gboolean include_sep = FALSE;
			gboolean include_decimal = FALSE;

			while (*format == '#' || *format == '0' || *format == '.' ||
			       *format == '-' || *format == 'E' || *format == 'e' ||
			       *format == '+' || *format == '?' || *format == ',') {
				switch (*format) {
				case ',': include_sep = TRUE; break;
				case '.': include_decimal = TRUE; break;
				}
				format++;
			}
			format--;

			if (format[1] == '/' && number_seen)
				append_type (MATCH_NUMERATOR);
			else
				append_type (MATCH_NUMBER);

			if (include_sep) {
				/* Not strictly correct.
				 * There should be a limit of 1-3 digits.
				 * However, that creates problems when
				 * There are formats like
				 *  $#,##0.00
				 * but not
				 *  $###0.00
				 * as a result $1000 would not be recognized.
				 */
				g_string_append (regexp, "([-+]?[0-9]+(");
				gnumeric_regexp_quote (regexp, format_get_thousand ()->str);
				g_string_append (regexp, "[0-9]{3})*)");
				append_type (MATCH_SKIP);
			} else {
				g_string_append (regexp, "([-+]?[0-9]+)");
			}

			if (include_decimal) {
				g_string_append (regexp, "?(");
				gnumeric_regexp_quote (regexp, format_get_decimal ()->str);
				g_string_append (regexp, "[0-9]+([Ee][-+]?[0-9]+)?)");
				append_type (MATCH_NUMBER_DECIMALS);
			}

			number_seen = TRUE;
			break;
		}

		case 'h':
		case 'H':
			hour_seen = TRUE;
			if (format[1] == 'h' || format[1] == 'H')
				format++;

			g_string_append (regexp, "([0-9][0-9]?)");
			append_type (MATCH_HOUR);
			break;

		case 'M':
		case 'm':
			if (hour_seen) {
				if (format[1] == 'm' || format[1] == 'M')
					format++;
				g_string_append (regexp, "([0-9][0-9]?)");
				append_type (MATCH_MINUTE);
				hour_seen = FALSE;
			} else {
				if (format[1] == 'm' || format[1] == 'M') {
					if (format[2] == 'm' || format[2] == 'M') {
						if (format[3] == 'm' || format[3] == 'M') {
							char *l;

							l = create_option_list (month_long);
							g_string_append (regexp, l);
							g_free (l);

							append_type (MATCH_MONTH_FULL);
							format++;
						} else {
							char *l;

							l = create_option_list (month_short);
							g_string_append (regexp, l);
							g_free (l);

							append_type (MATCH_MONTH_SHORT);
						}
						format++;
					} else {
						g_string_append (regexp, "([0-9][0-9]?)");
						append_type (MATCH_MONTH_NUMBER);
					}
					format++;
				} else {
					g_string_append (regexp, "([0-9][0-9]?)");
					append_type (MATCH_MONTH_NUMBER);
				}
			}
			break;

		case 's':
		case 'S':
			/* ICK!
			 * ICK!
			 * 'm' is ambiguous.  It can be months or minutes.
			 */
			{
				int l = match_types->len;
				if (l > 0 && match_types->data[l - 1] == MATCH_MONTH_NUMBER)
					match_types->data[l - 1] = MATCH_MINUTE;
			}

			if (format[1] == 's' || format[1] == 'S')
				format++;
			g_string_append (regexp, "([0-9][0-9]?)");
			append_type (MATCH_SECOND);
			break;

		case 'D':
		case 'd':
			if (format[1] == 'd' || format[1] == 'D') {
				if (format[2] == 'd' || format[2] == 'D') {
					if (format[3] == 'd' || format[3] == 'D') {
						char *l;

						l = create_option_list (day_long);
						g_string_append (regexp, l);
						g_free (l);

						append_type (MATCH_DAY_FULL);
						format++;
					} else {
						char *l;

						l = create_option_list (day_short);
						g_string_append (regexp, l);
						g_free (l);
					}
					format++;
				} else {
					g_string_append (regexp, "([0-9][0-9]?)");
					append_type (MATCH_DAY_NUMBER);
				}
				format++;
			} else {
				g_string_append (regexp, "([0-9][0-9]?)");
				append_type (MATCH_DAY_NUMBER);
			}
			break;

		case 'Y':
		case 'y':
			if (format[1] == 'y' || format[1] == 'Y') {
				if (format[2] == 'y' || format[2] == 'Y') {
					if (format[3] == 'y' || format[3] == 'Y') {
						g_string_append (regexp, "([0-9][0-9][0-9][0-9])");
						append_type (MATCH_YEAR_FULL);
						format++;
					}
					format++;
				} else {
					g_string_append (regexp, "([0-9][0-9]?)");
					append_type (MATCH_YEAR_SHORT);
				}
				format++;
			} else {
				g_string_append (regexp, "([0-9][0-9]?)");
				append_type (MATCH_YEAR_SHORT);
			}
			break;

		case ';':
			/* TODO : Is it ok to only match the first entry ?? */
			/* FIXME: What is this?  */
			while (*format)
				format = g_utf8_next_char (format);
			format = g_utf8_prev_char (format);
			break;

		case 'A': case 'a':
			if (*(format + 1) == 'm' || *(format + 1) == 'M') {
				if (*(format + 2) == '/') {
					if (*(format + 3) == 'P' || *(format + 3) == 'p') {
						if (*(format + 4) == 'm' || *(format + 4) == 'M') {
							format++;
						}
						format++;
					}
					format++;
				}
				format++;
			}
			g_string_append (regexp, "([Aa]|[Pp])[Mm]?");
			append_type (MATCH_AMPM);
			break;

		case '"':
			/* Matches a string */
			format++;
			while (*format != '"') {
				if (*format == 0)
					goto error;
				format = gnumeric_regexp_quote1 (regexp, format);
			}
			break;

		case '@':
			g_string_append (regexp, "(.*)");
			append_type (MATCH_STRING_CONSTANT);
			break;

		case '_':
			if (format[1]) {
				g_string_append (regexp, "[ ]?");
				format++;
			}
			break;

		case '/':
			g_string_append_c (regexp, '/');
			if (number_seen) {
				fraction = TRUE;
				/* Fraction.  Ick.  */
				if (strncmp (regexp->str, "^([-+]?[0-9]+) ", 15) == 0) {
					g_string_erase (regexp, 14, 1);
					g_string_insert (regexp, 13, " +|");
					/* FIXME: The final regexp won't match a plain digit sequence.  */
				}

				while (format[1] == '?' || g_ascii_isdigit (format[1]))
					format++;

				g_string_append (regexp, "([0-9]+) *");
				append_type (MATCH_DENOMINATOR);
			}
			break;

#if 0
		/* these were here explicitly before adding default.
		 * Leave them explicit for now as documentation.
		 */
			/* Default appears fine for this.  */
		case 0x00a3: /* GBP sign. */
		case 0x00a5: /* JPY sign. */
		case 0x20ac: /* EUR sign. */
		case '^':
		case '|':
		case ']':
		case '$':
		case ':':
		case '-':
		case ' ':
		case '(':
		case ')':

#endif
		default :
			gnumeric_regexp_quote1 (regexp, format);
		}
	}

	g_string_append_c (regexp, '$');

	str = g_string_free (regexp, FALSE);
	*dest = match_types;

#ifdef DEBUG_NUMBER_MATCH
	printf ("'%s'\n",str);
#endif
	return str;

 error:
	g_string_free (regexp, TRUE);
	g_byte_array_free (match_types, TRUE);
	return NULL;
}

static void
print_regex_error (int ret)
{
	switch (ret) {
	case REG_BADBR:
		fprintf (stderr,
			 "There was an invalid `\\{...\\}' construct in the regular\n"
			 "expression.  A valid `\\{...\\}' construct must contain either a\n"
			 "single number, or two numbers in increasing order separated by a\n"
			 "comma.\n");
		break;

	case REG_BADPAT:
		fprintf (stderr,
			 "There was a syntax error in the regular expression.\n");
		break;

	case REG_BADRPT:
		fprintf (stderr,
			 "A repetition operator such as `?' or `*' appeared in a bad\n"
			 "position (with no preceding subexpression to act on).\n");
		break;

	case REG_ECOLLATE:
		fprintf (stderr,
			 "The regular expression referred to an invalid collating element\n"
			 "(one not defined in the current locale for string collation).\n");
		break;

	case REG_ECTYPE:
		fprintf (stderr,
			 "The regular expression referred to an invalid character class name.\n");
		break;

#if REG_EESCAPE != REG_BADPAT
	case REG_EESCAPE:
		fprintf (stderr,
			 "The regular expression ended with `\\'.\n");
		break;
#endif

	case REG_ESUBREG:
		fprintf (stderr,
			 "There was an invalid number in the `\\DIGIT' construct.\n");
		break;

	case REG_EBRACK:
		fprintf (stderr,
			 "There were unbalanced square brackets in the regular expression.\n");
		break;

#if REG_EPAREN != REG_BADPAT
	case REG_EPAREN:
		fprintf (stderr,
			 "An extended regular expression had unbalanced parentheses, or a\n"
			 "basic regular expression had unbalanced `\\(' and `\\)'.\n");
		break;
#endif

#if REG_EBRACE != REG_BADPAT
	case REG_EBRACE:
		fprintf (stderr,
			 "The regular expression had unbalanced `\\{' and `\\}'.\n");
		break;
#endif

#ifdef REG_EBOL
	case REG_EBOL:
		fprintf (stderr, "Found ^ not at the beginning.\n");
		break;
#endif

#ifdef REG_EEOL
	case REG_EEOL:
		fprintf (stderr, "Found $ not at the end.\n");
		break;
#endif

	case REG_ERANGE:
		fprintf (stderr,
			 "One of the endpoints in a range expression was invalid.\n");
		break;

	case REG_ESPACE:
		fprintf (stderr,
			 "`regcomp' ran out of memory.\n");
		break;

	default:
		fprintf (stderr, "regexp error %d\n", ret);
	}
}

static GSList *format_match_list = NULL;
static GSList *format_dup_match_list = NULL;
static GSList *format_failed_match_list = NULL;

void
format_match_release (GnmFormat *fmt)
{
	if (fmt->regexp_str != NULL) {
		g_free (fmt->regexp_str);
		go_regfree (&fmt->regexp);
		g_byte_array_free (fmt->match_tags, TRUE);
	}
}

gboolean
format_match_create (GnmFormat *fmt)
{
	GByteArray *match_tags;
	char *regexp;
	go_regex_t r;
	int ret;

	g_return_val_if_fail (fmt != NULL, FALSE);
	g_return_val_if_fail (fmt->regexp_str == NULL, FALSE);
	g_return_val_if_fail (fmt->match_tags == NULL, FALSE);
	g_return_val_if_fail (strcmp (fmt->format, "General"), FALSE);

	regexp = format_create_regexp (fmt->format, &match_tags);
	if (!regexp) {
		fmt->regexp_str = NULL;
		fmt->match_tags = NULL;
		return FALSE;
	}

	ret = go_regcomp (&r, regexp, REG_EXTENDED | REG_ICASE);
	if (ret != 0) {
		g_warning ("expression [%s] produced [%s]", fmt->format, regexp);
		print_regex_error (ret);
		g_free (regexp);
		return FALSE;
	}

	fmt->regexp_str = regexp;
	fmt->regexp     = r;
	fmt->match_tags = match_tags;

	return TRUE;
}

/*
 * value_is_error : Check to see if a string begins with one of the magic
 * error strings.
 *
 * @str : The string to test
 *
 * returns : an error if there is one, or NULL.
 */
static GnmValue *
value_is_error (char const *str)
{
	GnmStdError e;

	for (e = (GnmStdError)0; e < GNM_ERROR_UNKNOWN; e++)
		if (0 == strcmp (str, value_error_name (e, TRUE)))
			return value_new_error_std (NULL, e);

	return NULL;
}

/*
 * Loads the initial formats that we will recognize
 */
void
format_match_init (void)
{
	int i;
	GnmFormat *fmt;
	GHashTable *hash;

	currency_date_format_init ();
	hash = g_hash_table_new (g_str_hash, g_str_equal);

	for (i = 0; cell_formats[i]; i++) {
		char const * const * p = cell_formats[i];

		for (; *p; p++) {
			/*  do not include text formats in the standard set */
			if (!strcmp ("@", *p))
				continue;

			fmt = style_format_new_XL (*p, FALSE);
			if (fmt->regexp_str != NULL) {
				/* TODO : * We could keep track of the regexps
				 * that General would match.  and avoid putting
				 * them in the list. */
				if (g_hash_table_lookup (hash, fmt->regexp_str) == NULL) {
					format_match_list = g_slist_append (format_match_list, fmt);
					g_hash_table_insert (hash, fmt->regexp_str, fmt);
				} else
					format_dup_match_list = g_slist_append (format_dup_match_list, fmt);
			} else
				format_failed_match_list = g_slist_append (format_failed_match_list, fmt);
		}
	}
	g_hash_table_destroy (hash);
}

void
format_match_finish (void)
{
	GSList *l;

	for (l = format_match_list; l; l = l->next)
		style_format_unref (l->data);
	g_slist_free (format_match_list);

	for (l = format_dup_match_list; l; l = l->next)
		style_format_unref (l->data);
	g_slist_free (format_dup_match_list);

	for (l = format_failed_match_list; l; l = l->next)
		style_format_unref (l->data);
	g_slist_free (format_failed_match_list);

	currency_date_format_shutdown ();
}

/*
 * table_lookup:
 *
 * Looks the string in the table passed
 */
static int
table_lookup (char const *str, char const *const *table)
{
	char const *const *p = table;
	int i = 0;

	for (p = table; *p; p++, i++) {
		char const *v  = *p;
		char const *iv = _(*p);

		if (*v == '*') {
			v++;
			iv++;
		}

		if (g_ascii_strcasecmp (str, v) == 0)
			return i;

		if (g_ascii_strcasecmp (str, iv) == 0)
			return i;
	}

	return -1;
}

/*
 * extract_text:
 *
 * Returns a newly allocated string which is a region from
 * STR.   The ranges are defined in the regmatch_t variable MP
 * in the fields rm_so and rm_eo
 */
static char *
extract_text (char const *str, const regmatch_t *mp)
{
	char *p;

	p = g_malloc (mp->rm_eo - mp->rm_so + 1);
	strncpy (p, &str[mp->rm_so], mp->rm_eo - mp->rm_so);
	p[mp->rm_eo - mp->rm_so] = 0;

	return p;
}

/*
 * Given a number of matches described by MP on S,
 * compute the number based on the information on ARRAY
 *
 * Currently the code cannot mix a MATCH_NUMBER with any
 * of the date/time matching.
 */
static GnmValue *
compute_value (char const *s, const regmatch_t *mp,
	       GByteArray *array, GnmDateConventions const *date_conv)
{
	int const len = array->len;
	gnm_float number = 0.0;
	guchar *data = array->data;
	gboolean percentify = FALSE;
	gboolean is_number  = FALSE;
	gboolean is_pm      = FALSE;
	gboolean is_explicit_am = FALSE;
	gboolean is_neg = FALSE;
	gboolean hours_are_cummulative   = FALSE;
	gboolean minutes_are_cummulative = FALSE;
	gboolean seconds_are_cummulative = FALSE;
	gboolean hours_set   = FALSE;
	gboolean minutes_set = FALSE;
	gboolean seconds_set = FALSE;
	int i;
	int month, day, year, year_short;
	int hours, minutes;
	gnm_float seconds;
	int numerator = 0, denominator = 1;

	GString const *thousands_sep = format_get_thousand ();
	GString const *decimal = format_get_decimal ();

	month = day = year = year_short = -1;
	hours = minutes = -1;
	seconds = -1.;

	for (i = 0; i < len; ) {
		MatchType type = *(data++);
		char *str;

		str = extract_text (s, &mp[++i]);

#ifdef DEBUG_NUMBER_MATCH
		printf ("Item[%d] = \'%s\' is a %d\n", i, str, type);
#endif
		switch (type) {
		case MATCH_MONTH_FULL:
			month = table_lookup (str, month_long);
			if (month == -1) {
				g_free (str);
				return NULL;
			}
			month++;
			break;

		case MATCH_MONTH_NUMBER:
			month = atoi (str);
			break;

		case MATCH_MONTH_SHORT:
			month = table_lookup (str, month_short);
			if (month == -1) {
				g_free (str);
				return NULL;
			}
			month++;
			break;

		case MATCH_DAY_FULL:
			/* FIXME: handle weekday */
			break;

		case MATCH_DAY_NUMBER:
			day = atoi (str);
			break;

		case MATCH_NUMERATOR:
			numerator = atoi (str);
			break;

		case MATCH_DENOMINATOR:
			denominator = atoi (str);
			if (denominator <= 0)
				return NULL;
			if (is_neg && numerator < 0)
				return NULL;

			is_number = TRUE;
			if (is_neg)
				number -= numerator / (gnm_float)denominator;
			else
				number += numerator / (gnm_float)denominator;
			break;

		case MATCH_NUMBER:
			if (*str != '\0') {
				char *ptr = str;

				switch (*ptr) {
				case '-':
					is_neg = TRUE;
					ptr++;
					break;
				case '+':
					ptr++;
					/* Fall through.  */
				default:
					is_neg = FALSE;
				}

				number = 0.;
				/* FIXME: this loop is bogus.  */
				while (1) {
					int thisnumber;
					if (number > DBL_MAX / 1000.0) {
						g_free (str);
						return NULL;
					}

					number *= 1000.0;

					errno = 0; /* strtol sets errno, but does not clear it.  */
					thisnumber = strtoul (ptr, &ptr, 10);
					if (errno == ERANGE) {
						g_free (str);
						return NULL;
					}

					number += thisnumber;

					if (strncmp (ptr, thousands_sep->str, thousands_sep->len) != 0)
						break;

					ptr += thousands_sep->len;
				}
				is_number = TRUE;
				if (is_neg) number = -number;
			}
			break;

		case MATCH_NUMBER_DECIMALS: {
			char *exppart = NULL;
			if (strncmp (str, decimal->str, decimal->len) == 0) {
				char *end;
				errno = 0; /* strtognum sets errno, but does not clear it.  */
				if (seconds < 0) {
					gnm_float fraction;

					for (end = str; *end && *end != 'e' && *end != 'E'; )
						end++;
					if (*end) {
						exppart = end + 1;
						*end = 0;
					}

					fraction = strtognum (str, &end);
					if (is_neg)
						number -= fraction;
					else
						number += fraction;
					is_number = TRUE;
				} else
					seconds += strtognum (str, &end);
			}
			if (exppart) {
				char *end;
				int exponent;

				errno = 0; /* strtol sets errno, but does not clear it.  */
				exponent = strtol (exppart, &end, 10);
				number *= gpow10 (exponent);
			}
			break;
		}

		case MATCH_CUMMULATIVE_HOURS:
			hours_are_cummulative = TRUE;
			if (str[0] == '-') is_neg = TRUE;
		case MATCH_HOUR:
			hours_set = TRUE;
			hours = abs (atoi (str));
			break;

		case MATCH_CUMMULATIVE_MINUTES:
			minutes_are_cummulative = TRUE;
			if (str[0] == '-') is_neg = TRUE;
		case MATCH_MINUTE:
			minutes_set = TRUE;
			minutes = abs (atoi (str));
			break;

		case MATCH_CUMMULATIVE_SECONDS :
			seconds_are_cummulative = TRUE;
			if (str[0] == '-') is_neg = TRUE;
		case MATCH_SECOND:
			seconds_set = TRUE;
			seconds = abs (atoi (str));
			break;

		case MATCH_PERCENT:
			percentify = TRUE;
			break;

		case MATCH_YEAR_FULL:
			year = atoi (str);
			break;

		case MATCH_YEAR_SHORT:
			year_short = atoi (str);
			break;

		case MATCH_AMPM:
			if (*str == 'p' || *str == 'P')
				is_pm = TRUE;
			else
				is_explicit_am = TRUE;
			break;

		case MATCH_SKIP:
			break;

		case MATCH_STRING_CONSTANT:
			return value_new_string_str (gnm_string_get_nocopy (str));

		default :
			g_warning ("compute_value: This should not happen.");
			break;
		}

		g_free (str);
	}

	if (is_number) {
		if (percentify)
			number *= 0.01;
		return value_new_float (number);
	}

	if (!(year == -1 && month == -1 && day == -1)) {
		time_t t = time (NULL);
		static time_t lastt;
		static struct tm tm;
		GDate *date;

		if (t != lastt) {
			/*
			 * Since localtime is moderately expensive, do
			 * at most one call per second.  One per day
			 * would be enough but is harder to check for.
			 */
			lastt = t;
			tm = *localtime (&t);
		}

		if (year == -1) {
			if (year_short != -1) {
				/* Window of -75 thru +24 years. */
				/* (TODO: See what current
				 * version of MS Excel uses.) */
				/* Earliest allowable interpretation
				 * is 75 years ago. */
				int earliest_ccyy
					= tm.tm_year + 1900 - 75;
				int earliest_cc = earliest_ccyy / 100;

				g_return_val_if_fail (year_short >= 0 &&
						      year_short <= 99,
						      NULL);
				year = earliest_cc * 100 + year_short;
				/*
				 * Our first guess at year has the same
				 * cc part as EARLIEST_CCYY, so is
				 * guaranteed to be in [earliest_ccyy -
				 * 99, earliest_ccyy + 99].  The yy
				 * part is of course year_short.
				 */
				if (year < earliest_ccyy)
					year += 100;
				/*
				 * year is now guaranteed to be in
				 * [earliest_ccyy, earliest_ccyy + 99],
				 * i.e. -75 thru +24 years from current
				 * year; and year % 100 == short_year.
				 */
			} else if (month != -1) {
				/* Window of -6 thru +5 months. */
				/* (TODO: See what current
				 * version of MS Excel uses.) */
				int earliest_yyymm
					= (tm.tm_year * 12 +
					   tm.tm_mon - 6);
				year = earliest_yyymm / 12;
				/* First estimate of yyy (i.e. years
				 * since 1900) is the yyy part of
				 * earliest_yyymm.  year*12+month-1 is
				 * guaranteed to be in [earliest_yyymm
				 * - 11, earliest_yyymm + 11].
				 */
				year += (year * 12 + month <=
					 earliest_yyymm);
				/* year*12+month-1 is now guaranteed
				 * to be in [earliest_yyymm,
				 * earliest_yyymm + 11], i.e.
				 * representing -6 thru +5 months
				 * from now.
				 */
				year += 1900;
				/* Finally convert from years since
				 * 1900 (yyy) to a proper 4-digit
				 * year.
				 */
			} else
				year = 1900 + tm.tm_year;
		}
		if (month == -1)
			month = tm.tm_mon + 1;
		if (day == -1)
			day = tm.tm_mday;

		if (year < 1900 || !g_date_valid_dmy (day, month, year))
			return NULL;

		date = g_date_new_dmy (day, month, year);
		number = datetime_g_to_serial (date, date_conv);
		g_date_free (date);
	}

	if (!seconds_set && !minutes_set && !hours_set)
		return value_new_int (number);

	if (!seconds_set)
		seconds = 0;

	if (!minutes_set)
		minutes = 0;

	if (!hours_set)
		hours = 0;

	if (!hours_are_cummulative) {
		if (is_pm) {
			if (hours < 12)
				hours += 12;
		} else if (is_explicit_am && hours == 12)
			hours = 0;
	}

	if ((hours < 0 || hours > 23) && !hours_are_cummulative)
		return NULL;

	if ((minutes < 0 || minutes > 59) && !minutes_are_cummulative)
		return NULL;

	if ((seconds < 0 || seconds > 59) && !seconds_are_cummulative)
		return NULL;

	if (hours == 0 && minutes == 0 && seconds == 0)
		return value_new_int (number);

	number += (hours * 3600 + minutes * 60 + seconds) / (3600*24.0);
	if (is_neg) number = -number;

	return value_new_float (number);
}

/**
 * format_match_simple :
 * @s : A String to match against.
 *
 * Attempt to match the the supplied string as a simple value.
 *
 * WARNING WARNING WARNING : This routine should NEVER be changed to match
 * 				VALUE_STRING that will break the parsers
 * 				handling of named expressions.
 */
GnmValue *
format_match_simple (char const *text)
{
	/* Is it a boolean?  */
	if (0 == g_ascii_strcasecmp (text, format_boolean (TRUE)))
		return value_new_bool (TRUE);
	if (0 == g_ascii_strcasecmp (text, format_boolean (FALSE)))
		return value_new_bool (FALSE);

	/* Is it an error?  */
	if (*text == '#') {
		GnmValue *err = value_is_error (text);
		if (err != NULL)
			return err;
	}

	/* Is it an integer?  */
	{
		char *end;
		long l;

		errno = 0; /* strtol sets errno, but does not clear it.  */
		l = strtol (text, &end, 10);
		if (text != end && errno != ERANGE && l == (int)l) {
			/* Allow and ignore spaces at the end.  */
			while (*end == ' ')
				end++;
			if (*end == '\0')
				return value_new_int ((int)l);
		}
	}

	/* Is it a double?  */
	{
		char *end;
		gnm_float d;

		errno = 0; /* strtognum sets errno, but does not clear it.  */
		d = strtognum (text, &end);
		if (text != end && errno != ERANGE) {
			/* Allow and ignore spaces at the end.  */
			while (*end == ' ')
				end++;
			if (*end == '\0')
				return value_new_float (d);
		}
	}

	return NULL;
}

#define NM 40

/**
 * format_match :
 *
 * @text    : The text to parse
 * @cur_fmt : The current format for the value (potentially NULL)
 * @date_conv: optional date convention
 *
 * Attempts to parse the supplied string to see if it matches a known value
 * format.  The caller is responsible for releasing the resulting value.
 **/
GnmValue *
format_match (char const *text, GnmFormat *cur_fmt,
	      GnmDateConventions const *date_conv)
{
	GnmValue  *v;
	GSList *l;
	regmatch_t mp[NM + 1];

	if (text[0] == '\0')
		return value_new_empty ();

	/* If it begins with a '\'' it is a string */
	if (text[0] == '\'')
		return value_new_string (text + 1);

	if (cur_fmt) {
		switch (cur_fmt->family) {
		case FMT_TEXT:
			return value_new_string (text);

		default:
			if (cur_fmt->regexp_str != NULL &&
			    go_regexec (&cur_fmt->regexp, text, NM, mp, 0) != REG_NOMATCH &&
			    NULL != (v = compute_value (text, mp, cur_fmt->match_tags,
							date_conv))) {
#ifdef DEBUG_NUMBER_MATCH
				int i;
				g_print ("matches expression: %s %s\n", cur_fmt->format, cur_fmt->regexp_str);
				for (i = 0; i < NM; i++) {
					char *p;

					if (mp[i].rm_so == -1)
						break;

					p = extract_text (text, &mp[i]);
					g_print ("%d %d->%s\n", mp[i].rm_so, mp[i].rm_eo, p);
				}
#endif

				value_set_fmt (v, cur_fmt);
				return v;
			} else {
#ifdef DEBUG_NUMBER_MATCH
				g_print ("does not match expression: %s %s\n",
					 cur_fmt->format,
					 cur_fmt->regexp_str ? cur_fmt->regexp_str : "(null)");
#endif
			}
		}
	}

	/* Check basic types */
	v = format_match_simple (text);
	if (v != NULL)
		return v;

	/* Fall back to checking the set of canned formats */
	for (l = format_match_list; l; l = l->next) {
		GnmFormat *fmt = l->data;
#ifdef DEBUG_NUMBER_MATCH
		printf ("test: %s \'%s\'\n", fmt->format, fmt->regexp_str);
#endif
		if (go_regexec (&fmt->regexp, text, NM, mp, 0) == REG_NOMATCH)
			continue;

#ifdef DEBUG_NUMBER_MATCH
		{
			int i;
			printf ("matches expression: %s %s\n", fmt->format, fmt->regexp_str);
			for (i = 0; i < NM; i++) {
				char *p;

				if (mp[i].rm_so == -1)
					break;

				p = extract_text (text, &mp[i]);
				printf ("%d %d->%s\n", mp[i].rm_so, mp[i].rm_eo, p);
			}
		}
#endif

		v = compute_value (text, mp, fmt->match_tags, date_conv);

#ifdef DEBUG_NUMBER_MATCH
		if (v) {
			printf ("value = ");
			value_dump (v);
		} else
			printf ("unable to compute value\n");
#endif
		if (v != NULL) {
			value_set_fmt (v, fmt);
			return v;
		}
	}

	return NULL;
}

/**
 * format_match_number :
 *
 * @text    : The text to parse
 * @cur_fmt : The current format for the value (potentially NULL)
 * @date_conv: optional date convention
 *
 * Attempts to parse the supplied string to see if it matches a known value format.
 * Will eventually use the current cell format in preference to canned formats.
 * If @format is supplied it will get a copy of the matching format with no
 * additional references.   The caller is responsible for releasing the
 * resulting value.  Will ONLY return numbers.
 */
GnmValue *
format_match_number (char const *text, GnmFormat *cur_fmt,
		     GnmDateConventions const *date_conv)
{
	GnmValue *res = format_match (text, cur_fmt, date_conv);

	if (res != NULL) {
		if (VALUE_IS_NUMBER (res))
			return res;
		value_release (res);
	}
	return NULL;
}
