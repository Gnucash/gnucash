/*
 * regutf8.c:  UTF-8 regexp routines.
 *
 * Author:
 *   Morten Welinder (terra@gnome.org)
 */

#include <config.h>
#include <regutf8.h>

int
gnumeric_regcomp_XL (go_regex_t *preg, char const *pattern, int cflags)
{
	GString *res = g_string_new (NULL);
	int retval;

	while (*pattern) {
		switch (*pattern) {
		case '~':
			pattern++;
			if (*pattern == '*')
				g_string_append (res, "\\*");
			else
				g_string_append_c (res, *pattern);
			if (*pattern) pattern++;
			break;

		case '*':
			g_string_append (res, ".*");
			pattern++;
			break;

		case '?':
			g_string_append_c (res, '.');
			pattern++;
			break;

		default:
			pattern = gnumeric_regexp_quote1 (res, pattern);
		}
	}

	retval = go_regcomp (preg, res->str, cflags);
	g_string_free (res, TRUE);
	return retval;
}

/*
 * Quote a single UTF-8 encoded character from s into target and return the
 * location of the next character in s.
 */
const char *
gnumeric_regexp_quote1 (GString *target, const char *s)
{
	g_return_val_if_fail (target != NULL, NULL);
	g_return_val_if_fail (s != NULL, NULL);

	switch (*s) {
	case '.': case '[': case '\\':
	case '*': case '+': case '{': case '?':
	case '^': case '$':
	case '(': case '|': case ')':
		g_string_append_c (target, '\\');
		g_string_append_c (target, *s);
		return s + 1;

	case 0:
		return s;

	default:
		do {
			g_string_append_c (target, *s);
			s++;
		} while ((*s & 0xc0) == 0x80);

		return s;
	}
}

/*
 * Regexp quote a UTF-8 string.
 */
void
gnumeric_regexp_quote (GString *target, const char *s)
{
	g_return_if_fail (target != NULL);
	g_return_if_fail (s != NULL);

	while (*s)
		s = gnumeric_regexp_quote1 (target, s);
}
