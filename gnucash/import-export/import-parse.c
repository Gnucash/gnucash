/*
 * import-parse.c -- a generic "parser" API for importers..  Allows importers
 * 	to parse dates and numbers, and provides a UI to ask for users to
 * 	resolve ambiguities.
 *
 * Created by:	Derek Atkins <derek@ihtfp.com>
 * Copyright (c) 2003 Derek Atkins <warlord@MIT.EDU>
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <glib.h>
#include <string.h>

/* For regex */
#include <sys/types.h>
#include <regex.h>

#include "gnc-engine.h"
#include "gnc-ui-util.h"

#include "import-parse.h"

static QofLogModule log_module = GNC_MOD_IMPORT;

/* numeric regular expressions */
static regex_t decimal_radix_regex;
static regex_t comma_radix_regex;

/* date regular expressions */
static regex_t date_regex;
static regex_t date_mdy_regex;
static regex_t date_ymd_regex;

static gboolean regex_compiled = FALSE;

static void
compile_regex(void)
{
    int flags = REG_EXTENDED;

    /* compile the numeric regular expressions */
    regcomp(&decimal_radix_regex,
            "^ *\\$?[+-]?\\$?[0-9]+ *$|^ *\\$?[+-]?\\$?[0-9]?[0-9]?[0-9]?(,[0-9][0-9][0-9])*(\\.[0-9]*)? *$|^ *\\$?[+-]?\\$?[0-9]+\\.[0-9]* *$", flags);
    regcomp(&comma_radix_regex,
            "^ *\\$?[+-]?\\$?[0-9]+ *$|^ *\\$?[+-]?\\$?[0-9]?[0-9]?[0-9]?(\\.[0-9][0-9][0-9])*(,[0-9]*)? *$|^ *\\$?[+-]?\\$?[0-9]+,[0-9]* *$", flags);

    /* compile the date-parsing regular expressions */
    regcomp(&date_regex,
            "^ *([0-9]+) *[-/.'] *([0-9]+) *[-/.'] *([0-9]+).*$|^ *([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]).*$", flags);
    regcomp(&date_mdy_regex, "([0-9][0-9])([0-9][0-9])([0-9][0-9][0-9][0-9])", flags);
    regcomp(&date_ymd_regex, "([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9])", flags);

    regex_compiled = TRUE;
}

static gint
my_strntol(const char *str, int len)
{
    gint res = 0;

    g_return_val_if_fail(str, 0);
    g_return_val_if_fail(len, 0);

    while (len--)
    {

        if (*str < '0' || *str > '9')
        {
            str++;
            continue;
        }

        res *= 10;
        res += *(str++) - '0';
    }
    return res;
}

/*
 * based on a trio match (matches in spaces 1, 2, and 3), and a list
 * of possible date formats, return the list of formats that this string
 * could actually be.
 */
static GncImportFormat
check_date_format(const char * str, regmatch_t *match, GncImportFormat fmts)
{
    GncImportFormat res = 0;
    int len0 = 0, len1 = 0, len2 = 0;
    int val0 = 0, val1 = 0, val2 = 0;

    g_return_val_if_fail(match, res);
    g_return_val_if_fail(fmts, res);

    /* Compute the lengths */
    len0 = match[1].rm_eo - match[1].rm_so;
    len1 = match[2].rm_eo - match[2].rm_so;
    len2 = match[3].rm_eo - match[3].rm_so;

    /* compute the numeric values */
    val0 = my_strntol(str + match[1].rm_so, len0);
    val1 = my_strntol(str + match[2].rm_so, len1);
    val2 = my_strntol(str + match[3].rm_so, len2);

    /* Filter out the possibilities.  Hopefully only one will remain */

    if (val0 > 12) import_clear_flag(fmts, GNCIF_DATE_MDY);
    if (val0 > 31) import_clear_flag(fmts, GNCIF_DATE_DMY);
    if (val0 < 1)
    {
        import_clear_flag(fmts, GNCIF_DATE_DMY);
        import_clear_flag(fmts, GNCIF_DATE_MDY);
    }

    if (val1 > 12)
    {
        import_clear_flag(fmts, GNCIF_DATE_DMY);
        import_clear_flag(fmts, GNCIF_DATE_YMD);
    }
    if (val1 > 31)
    {
        import_clear_flag(fmts, GNCIF_DATE_MDY);
        import_clear_flag(fmts, GNCIF_DATE_YDM);
    }

    if (val2 > 12) import_clear_flag(fmts, GNCIF_DATE_YDM);
    if (val2 > 31) import_clear_flag(fmts, GNCIF_DATE_YMD);
    if (val2 < 1)
    {
        import_clear_flag(fmts, GNCIF_DATE_YMD);
        import_clear_flag(fmts, GNCIF_DATE_YDM);
    }

    /* if we've got a 4-character year, make sure the value is greater
     * than 1930 and less than 2100.  XXX: be sure to fix this by 2100!
     */
    if (len0 == 4 && (val0 < 1930 || val0 > 2100))
    {
        import_clear_flag(fmts, GNCIF_DATE_YMD);
        import_clear_flag(fmts, GNCIF_DATE_YDM);
    }
    if (len2 == 4 && (val2 < 1930 || val2 > 2100))
    {
        import_clear_flag(fmts, GNCIF_DATE_MDY);
        import_clear_flag(fmts, GNCIF_DATE_DMY);
    }

    /* If the first string has a length of only 1, then it is definitely
     * not a year (although it could be a month or day).
     */
    if (len0 == 1)
    {
        import_clear_flag(fmts, GNCIF_DATE_YMD);
        import_clear_flag(fmts, GNCIF_DATE_YDM);
    }

    return fmts;
}

GncImportFormat
gnc_import_test_numeric(const char* str, GncImportFormat fmts)
{
    GncImportFormat res = 0;

    g_return_val_if_fail(str, fmts);

    if (!regex_compiled)
        compile_regex();

    if ((fmts & GNCIF_NUM_PERIOD) && !regexec(&decimal_radix_regex, str, 0, NULL, 0))
        res |= GNCIF_NUM_PERIOD;

    if ((fmts & GNCIF_NUM_COMMA) && !regexec(&comma_radix_regex, str, 0, NULL, 0))
        res |= GNCIF_NUM_COMMA;

    return res;
}


GncImportFormat
gnc_import_test_date(const char* str, GncImportFormat fmts)
{
    regmatch_t match[5];
    GncImportFormat res = 0;

    g_return_val_if_fail(str, fmts);
    g_return_val_if_fail(strlen(str) > 1, fmts);

    if (!regex_compiled)
        compile_regex();

    if (!regexec(&date_regex, str, 5, match, 0))
    {
        if (match[1].rm_so != -1)
            res = check_date_format(str, match, fmts);
        else
        {
            /* Hmm, it matches XXXXXXXX, but is this YYYYxxxx or xxxxYYYY?
             * let's try both ways and let the parser check that YYYY is
             * valid.
             */
            char temp[9];

            g_return_val_if_fail(match[4].rm_so != -1, fmts);
            g_return_val_if_fail(match[4].rm_eo - match[4].rm_so == 8, fmts);

            /* make a temp copy of the XXXXXXXX string */
            strncpy(temp, str + match[4].rm_so, 8);
            temp[8] = '\0';

            /* then check it against the ymd or mdy formats, as necessary */
            if (((fmts & GNCIF_DATE_YDM) || (fmts & GNCIF_DATE_YMD)) &&
                    !regexec(&date_ymd_regex, temp, 4, match, 0))
                res |= check_date_format(temp, match, fmts);

            if (((fmts & GNCIF_DATE_DMY) || (fmts & GNCIF_DATE_MDY)) &&
                    !regexec(&date_mdy_regex, temp, 4, match, 0))
                res |= check_date_format(temp, match, fmts);
        }
    }

    return res;
}

gboolean
gnc_import_parse_numeric(const char* str, GncImportFormat fmt, gnc_numeric *val)
{
    g_return_val_if_fail(str, FALSE);
    g_return_val_if_fail(val, FALSE);
    g_return_val_if_fail(fmt, FALSE);
    g_return_val_if_fail(!(fmt & (fmt - 1)), FALSE);

    switch (fmt)
    {
    case GNCIF_NUM_PERIOD:
        return xaccParseAmountExtended(str, TRUE, '-', '.', ',', NULL, "$+",
                                       val, NULL);
    case GNCIF_NUM_COMMA:
        return xaccParseAmountExtended(str, TRUE, '-', ',', '.', NULL, "$+",
                                       val, NULL);
    default:
        PERR("invalid format: %d", fmt);
        return FALSE;
    }
}

/* Handle y2k fixes, etc.
 * obtaining the year "00", "2000", and "19100" all mean the same thing.
 * output is an integer representing the year in the C.E.
 */
static int
fix_year(int y)
{
    /* two-digit numbers less than "70"  are interpretted to be post-2000. */
    if (y < 70)
        return (y + 2000);

    /* fix a common bug in printing post-2000 dates as 19100, etc. */
    if (y > 19000)
        return (1900 + (y - 19000));

    /* At this point we just want to make sure that this is a real date.
     * y _should_ be a 'unix year' (which is the number of years since
     * 1900), but it _COULD_ be a full date (1999, 2001, etc.).  At some
     * point in the future we can't tell the difference, but are we really
     * going to care if this code fails in 3802?
     */
    if (y < 1902)
        return (y + 1900);

    /* y is good as it is */
    return y;
}

gboolean
gnc_import_parse_date(const char *str, GncImportFormat fmt, Timespec *val)
{
    regmatch_t match[5];
    char temp[9];
    const char *datestr;

    int v0 = 0, v1 = 0, v2 = 0;
    int m = 0, d = 0, y = 0;

    g_return_val_if_fail(str, FALSE);
    g_return_val_if_fail(val, FALSE);
    g_return_val_if_fail(fmt, FALSE);
    g_return_val_if_fail(!(fmt & (fmt - 1)), FALSE);

    if (!regexec(&date_regex, str, 5, match, 0))
    {
        if (match[1].rm_so != -1)
            datestr = str;
        else
        {
            /* date is of the form XXXXXXX; save it to a temp string and
             * split it based on the format, either YYYYaabb or aabbYYYY
             */
            g_return_val_if_fail(match[4].rm_so != -1, FALSE);
            g_return_val_if_fail(match[4].rm_eo - match[4].rm_so == 8, FALSE);

            strncpy(temp, str + match[4].rm_so, 8);
            temp[8] = '\0';

            switch (fmt)
            {
            case GNCIF_DATE_DMY:
            case GNCIF_DATE_MDY:
                g_return_val_if_fail(!regexec(&date_mdy_regex, temp, 4, match, 0), FALSE);
                break;
            case GNCIF_DATE_YMD:
            case GNCIF_DATE_YDM:
                g_return_val_if_fail(!regexec(&date_ymd_regex, temp, 4, match, 0), FALSE);
                break;
            default:
                PERR("Invalid date format provided: %d", fmt);
                return FALSE;
            }
            datestr = temp;
        }

        /* datestr points to the date string, and match[123] contains the matches. */

        if (match[1].rm_so == -1 || match[2].rm_so == -1 || match[3].rm_so == -1)
        {
            PERR("can't interpret date %s", str);
            return FALSE;
        }

        /* grab the numerics */
        v0 = my_strntol(datestr + match[1].rm_so, match[1].rm_eo - match[1].rm_so);
        v1 = my_strntol(datestr + match[2].rm_so, match[2].rm_eo - match[2].rm_so);
        v2 = my_strntol(datestr + match[3].rm_so, match[3].rm_eo - match[3].rm_so);

        switch (fmt)
        {
        case GNCIF_DATE_DMY:
            if (v0 > 0 && v0 <= 31 && v1 > 0 && v1 <= 12 && v2 > 0)
            {
                d = v0;
                m = v1;
                y = v2;
            }
            else
                PERR("format is d/m/y but date is %s", str);
            break;

        case GNCIF_DATE_MDY:
            if (v0 > 0 && v0 <= 12 && v1 > 0 && v1 <= 31 && v2 > 0)
            {
                m = v0;
                d = v1;
                y = v2;
            }
            else
                PERR("format is m/d/y but date is %s", str);
            break;

        case GNCIF_DATE_YMD:
            if (v0 > 0 && v1 > 0 && v1 <= 12 && v2 > 0 && v2 <= 31)
            {
                y = v0;
                m = v1;
                d = v2;
            }
            else
                PERR("format is y/m/d but date is %s", str);
            break;

        case GNCIF_DATE_YDM:
            if (v0 > 0 && v1 > 0 && v1 <= 31 && v2 > 0 && v2 <= 12)
            {
                y = v0;
                d = v1;
                m = v2;
            }
            else
                PERR("format is y/d/m but date is %s", str);
            break;

        default:
            PERR("invalid date format: %d", fmt);
        }

        if (!m || !d || !y)
            return FALSE;

        y = fix_year(y);
        *val = gnc_dmy2timespec(d, m, y);
        return TRUE;
    }

    return FALSE;
}
