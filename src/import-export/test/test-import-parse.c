/*
 * test-import-parse.c -- Test the import-parse routines.
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

#include "config.h"
#include <glib.h>
#include <libguile.h>

#include "gnc-module.h"
#include "import-parse.h"

#include "test-stuff.h"

typedef struct
{
    int y;
    int m;
    int d;
} my_ymd_t;

/* make sure the numbers are the same in the two sets of lists */
const char* period_numbers[] = { " $+2000.00", "-2.00", "1,182,183.1827", NULL };
const char* comma_numbers[] = { " $2000,00", "-2,00", "1.182.183,1827", NULL };
const char* period_numbers_ambig[] = { "  -$1,000 ", "100.277", NULL };
const char* comma_numbers_ambig[] = { "  -$1.000 ", "100,277", NULL };

/* Make sure the strings and numbers match... */
const char* dates_ymd[] = { "1999/12/31", "2001-6-17", "20020726",  NULL };
my_ymd_t dates_ymd_vals[] = { {1999, 12, 31}, {2001, 6, 17}, {2002, 7, 26}, {0, 0, 0} };
const char* dates_ydm[] = { "1999/31/12", "2001-17-6", "20012311", NULL };
my_ymd_t dates_ydm_vals[] = { {1999, 12, 31}, {2001, 6, 17}, {2001, 11, 23}, {0, 0, 0} };
const char* dates_mdy[] = { "1/16/2001", "12-31-1999", "01171983", NULL };
my_ymd_t dates_mdy_vals[] = { {2001, 1, 16}, {1999, 12, 31}, {1983, 1, 17}, {0, 0, 0} };
const char* dates_dmy[] = { "16/1/2001", "31-12-1999", "17011976", NULL };
my_ymd_t dates_dmy_vals[] = { {2001, 1, 16}, {1999, 12, 31}, {1976, 1, 17}, {0, 0, 0} };

const char* dates_yxx[] = { "99/1/6", "1999-12'10", "20010306", NULL };
const char* dates_xxy[] = { "1/3/99", "12-10'1999", "03062001", NULL };

static void
run_check(GncImportFormat (*check_fcn)(const char*, GncImportFormat),
const char *numbers[], GncImportFormat formats,
const char* msg, GncImportFormat expected)
{
    while (*numbers)
    {
        do_test(check_fcn(*numbers, formats) == expected, msg);
        numbers++;
    }
}

static void
test_check_numeric(void)
{
    GncImportFormat fmts;

    fmts = GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA | GNCIF_DATE_MDY;

    run_check(gnc_import_test_numeric, period_numbers, fmts,
    "Period numbers", GNCIF_NUM_PERIOD);
    run_check(gnc_import_test_numeric, comma_numbers, fmts,
    "Comma numbers", GNCIF_NUM_COMMA);

    run_check(gnc_import_test_numeric, period_numbers_ambig, fmts,
    "Ambiguous Period numbers", GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA);
    run_check(gnc_import_test_numeric, comma_numbers_ambig, fmts,
    "Ambiguous Comma numbers", GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA);
}

static void
test_check_date(void)
{
    GncImportFormat fmts;

    fmts = GNCIF_DATE_DMY | GNCIF_DATE_MDY | GNCIF_DATE_YMD | GNCIF_DATE_YDM;

    run_check(gnc_import_test_date, dates_ymd, fmts, "y/m/d dates", GNCIF_DATE_YMD);
    run_check(gnc_import_test_date, dates_ydm, fmts, "y/d/m dates", GNCIF_DATE_YDM);
    run_check(gnc_import_test_date, dates_mdy, fmts, "m/d/y dates", GNCIF_DATE_MDY);
    run_check(gnc_import_test_date, dates_dmy, fmts, "d/m/y dates", GNCIF_DATE_DMY);

    run_check(gnc_import_test_date, dates_yxx, fmts, "y/x/x dates",
    GNCIF_DATE_YMD | GNCIF_DATE_YDM);
    run_check(gnc_import_test_date, dates_xxy, fmts, "x/x/y dates",
    GNCIF_DATE_DMY | GNCIF_DATE_MDY);
}

static void
test_numbers(const char* pstr, const char* cstr)
{
    gnc_numeric pval, cval;

    do_test(gnc_import_parse_numeric(pstr, GNCIF_NUM_PERIOD, &pval), "Parsing Period");
    do_test(gnc_import_parse_numeric(cstr, GNCIF_NUM_COMMA, &cval), "Parsing Comma");

    do_test(gnc_numeric_equal(pval, cval), "Numbers equal?");
}

static void
test_number_strings(const char** pstrs, const char** cstrs)
{
    while (*pstrs && *cstrs)
    {
        test_numbers(*pstrs, *cstrs);
        pstrs++;
        cstrs++;
    }
}

static void
test_parse_numeric(void)
{
    test_number_strings(period_numbers, comma_numbers);
    test_number_strings(period_numbers_ambig, comma_numbers_ambig);
}

static void
test_date(const char* str, GncImportFormat fmt, my_ymd_t date)
{
    Timespec ts, ts2;;

    do_test(gnc_import_parse_date(str, fmt, &ts), "Parsing date");
    ts2 = gnc_dmy2timespec(date.d, date.m, date.y);
    do_test(timespec_equal(&ts, &ts2), "Date Equal");
}

static void
test_date_list(const char** strs, GncImportFormat fmt, my_ymd_t *dates)
{
    while (*strs && (*dates).y)
    {
        test_date(*strs, fmt, *dates);
        strs++;
        dates++;
    }
}

static void
test_parse_date(void)
{
    test_date_list(dates_ymd, GNCIF_DATE_YMD, dates_ymd_vals);
    test_date_list(dates_ydm, GNCIF_DATE_YDM, dates_ydm_vals);
    test_date_list(dates_mdy, GNCIF_DATE_MDY, dates_mdy_vals);
    test_date_list(dates_dmy, GNCIF_DATE_DMY, dates_dmy_vals);
}

static void
test_import_parse(void)
{
    test_check_numeric();
    test_check_date();
    test_parse_numeric();
    test_parse_date();
}

static void
main_helper(void *closure, int argc, char **argv)
{
    gnc_module_load("gnucash/import-export", 0);
    test_import_parse();
    print_test_results();
    exit(get_rv());
}

int
main(int argc, char **argv)
{
    scm_boot_guile(argc, argv, main_helper, NULL);
    return 0;
}
