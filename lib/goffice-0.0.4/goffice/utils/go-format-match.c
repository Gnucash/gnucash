#include <goffice/goffice-config.h>
#include "go-format-match.h"
#include "go-format.h"
#include "format-impl.h"
#include "regutf8.h"

#include <glib/gi18n.h>
#include <stdio.h>
#include <string.h>

#define append_type(t) do { guint8 x = t; match_types = g_byte_array_append (match_types, &x, 1); } while (0)

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

/* Takes a list of strings (optionally include an * at the beginning
 * that gets stripped, for i18n purposes). and returns a regexp that
 * would match them */
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

/* Create a regular expression for the given XL-style format.
 * NOTE: the format as well as the regexp are UTF-8 encoded. */
static char *
format_create_regexp (gchar const *format, GByteArray **dest)
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
			go_regexp_quote1 (regexp, format);
			break;
		}

		case '[' :
			/* Currency symbol */
			if (format[1] == '$') {
				for (format += 2; *format && *format != ']' ; ++format)
					g_string_append_c (regexp, *format);
				if (*format != ']')
					format--;
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
				go_regexp_quote (regexp, format_get_thousand ()->str);
				g_string_append (regexp, "[0-9]{3})*)");
				append_type (MATCH_SKIP);
			} else {
				g_string_append (regexp, "([-+]?[0-9]+)");
			}

			if (include_decimal) {
				g_string_append (regexp, "?(");
				go_regexp_quote (regexp, format_get_decimal ()->str);
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
				format = go_regexp_quote1 (regexp, format);
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

				g_string_append_c (regexp, '(');
				while (format[1] == '?' || g_ascii_isdigit (format[1])) {
					format++;
					g_string_append (regexp, "[0-9]");
				}

				g_string_append (regexp, ") *");
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
			go_regexp_quote1 (regexp, format);
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

gboolean
format_match_create (GOFormatElement *fmt)
{
	GByteArray *match_tags;
	char *regexp;
	GORegexp r;
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

void
format_match_release (GOFormatElement *fmt)
{
	if (fmt->regexp_str != NULL) {
		g_free (fmt->regexp_str);
		go_regfree (&fmt->regexp);
		g_byte_array_free (fmt->match_tags, TRUE);
	}
}

