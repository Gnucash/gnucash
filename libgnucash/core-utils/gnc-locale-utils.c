/********************************************************************\
 * gnc-locale-utils.c -- locale functions		            *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>              *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include <config.h>

#include "gnc-locale-utils.h"

#include <glib.h>
#include <glib/gi18n.h>

#ifdef G_OS_WIN32
#include <windows.h>

static void
gnc_lconv_set_utf8 (char **p_value, wchar_t *w_value, const char *default_value)
{
    g_return_if_fail (p_value != NULL);
    if (w_value && *w_value)
        *p_value = g_utf16_to_utf8 (w_value, -1, NULL, NULL, NULL);
    else
        *p_value = g_strdup (default_value);
}

static LPCWSTR
get_locale_name ()
{
    char* locale = setlocale(LC_MONETARY, NULL);
    int len = 0;
    len = strchr (locale, '.') - locale;
    locale[2] = '-';
    wchar_t *wlocale = g_utf8_to_utf16 (locale, len, NULL, NULL, NULL);
    if (IsValidLocaleName(wlocale))
        return wlocale;
    return LOCALE_NAME_USER_DEFAULT;
}

#define BUFSIZE 10 //Max size used by LOCALE_SGROUPING.
static wchar_t*
get_lconv_wide_val (LCTYPE lctype)
{
    static wchar_t buffer[BUFSIZE];
    LPCWSTR locale = get_locale_name ();
    memset (buffer, 0, BUFSIZE * sizeof(wchar_t));
    int count =  GetLocaleInfoEx (locale, lctype, buffer, BUFSIZE);
    if (!count)
    {
        g_error("GetLocaleInfo failed for %ld, %ld", lctype, GetLastError());
        return NULL;
    }
    if (lctype == LOCALE_SGROUPING || lctype == LOCALE_SMONGROUPING)
    {
        buffer[0] &= 0x0f; // The standard localeconv is a single uint8
        buffer[1] = '\0';
    }
    return buffer;
}
#else
static void
gnc_lconv_set_utf8 (char **p_value, char *default_value)
{
    char *value = *p_value;
    *p_value = NULL;

    if ((value == NULL) || (value[0] == 0))
        value = default_value;

    *p_value = g_locale_to_utf8 (value, -1, NULL, NULL, NULL);

    if (*p_value == NULL)
    {
        // The g_locale_to_utf8 conversion failed. FIXME: Should we rather
        // use an empty string instead of the default_value? Not sure.
        *p_value = default_value;
    }
}

#endif
static void
gnc_lconv_set_char (char *p_value, char default_value)
{
    if ((p_value != NULL) && (*p_value == CHAR_MAX))
        *p_value = default_value;
}

struct lconv *
gnc_localeconv (void)
{
    static struct lconv lc;
    static gboolean lc_set = FALSE;

    if (lc_set)
        return &lc;

    lc = *localeconv();
#ifdef G_OS_WIN32
    gnc_lconv_set_utf8(&lc.decimal_point,
                       get_lconv_wide_val (LOCALE_SDECIMAL), ".");
    gnc_lconv_set_utf8(&lc.thousands_sep,
                       get_lconv_wide_val (LOCALE_STHOUSAND),",");
    gnc_lconv_set_utf8(&lc.grouping,
                       get_lconv_wide_val (LOCALE_SGROUPING), "\003");
    gnc_lconv_set_utf8(&lc.int_curr_symbol,
                       get_lconv_wide_val (LOCALE_SINTLSYMBOL), "USD ");
    gnc_lconv_set_utf8(&lc.currency_symbol,
                       get_lconv_wide_val (LOCALE_SCURRENCY), "$");
    gnc_lconv_set_utf8(&lc.mon_decimal_point,
                       get_lconv_wide_val (LOCALE_SMONDECIMALSEP), ".");
    gnc_lconv_set_utf8(&lc.mon_thousands_sep,
                       get_lconv_wide_val (LOCALE_SMONTHOUSANDSEP), ",");
    gnc_lconv_set_utf8(&lc.mon_grouping,
                       get_lconv_wide_val (LOCALE_SMONGROUPING), "\003");
    gnc_lconv_set_utf8(&lc.negative_sign,
                       get_lconv_wide_val (LOCALE_SNEGATIVESIGN), "-");
    gnc_lconv_set_utf8(&lc.positive_sign,
                       get_lconv_wide_val (LOCALE_SPOSITIVESIGN), "");
#else
    gnc_lconv_set_utf8(&lc.decimal_point, ".");
    gnc_lconv_set_utf8(&lc.thousands_sep, ",");
    gnc_lconv_set_utf8(&lc.grouping, "\003");
    gnc_lconv_set_utf8(&lc.int_curr_symbol, "USD ");
    gnc_lconv_set_utf8(&lc.currency_symbol, "$");
    gnc_lconv_set_utf8(&lc.mon_decimal_point, ".");
    gnc_lconv_set_utf8(&lc.mon_thousands_sep, ",");
    gnc_lconv_set_utf8(&lc.mon_grouping, "\003");
    gnc_lconv_set_utf8(&lc.negative_sign, "-");
    gnc_lconv_set_utf8(&lc.positive_sign, "");
#endif
    gnc_lconv_set_char(&lc.frac_digits, 2);
    gnc_lconv_set_char(&lc.int_frac_digits, 2);
    gnc_lconv_set_char(&lc.p_cs_precedes, 1);
    gnc_lconv_set_char(&lc.p_sep_by_space, 0);
    gnc_lconv_set_char(&lc.n_cs_precedes, 1);
    gnc_lconv_set_char(&lc.n_sep_by_space, 0);
    gnc_lconv_set_char(&lc.p_sign_posn, 1);
    gnc_lconv_set_char(&lc.n_sign_posn, 1);

    lc_set = TRUE;

    return &lc;
}

const char *
gnc_locale_default_iso_currency_code (void)
{
    static char *code = NULL;
    struct lconv *lc;

    if (code)
        return code;

    lc = gnc_localeconv ();

    code = g_strdup (lc->int_curr_symbol);

    /* The int_curr_symbol includes a space at the end! Note: you
     * can't just change "USD " to "USD" in gnc_localeconv, because
     * that is only used if int_curr_symbol was not defined in the
     * current locale. If it was, it will have the space! */
    g_strstrip (code);

    return code;
}

/* Return the number of decimal places for this locale. */
int
gnc_locale_decimal_places (void)
{
    static gboolean got_it = FALSE;
    static int places;
    struct lconv *lc;

    if (got_it)
        return places;

    lc = gnc_localeconv();
    places = lc->frac_digits;

    /* frac_digits is already initialized by gnc_localeconv, hopefully
     * to a reasonable default. */
    got_it = TRUE;

    return places;
}

gchar *gnc_locale_name (void)
{
# ifdef G_OS_WIN32
    return g_win32_getlocale();
# else /* !G_OS_WIN32 */
    return g_strdup (setlocale(LC_MONETARY, NULL));
# endif /* G_OS_WIN32 */

}
