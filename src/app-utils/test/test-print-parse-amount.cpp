/********************************************************************\
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
 *                                                                  *
\********************************************************************/

#include <guid.hpp>
extern "C"
{
#include "config.h"
#include <glib.h>
#include <stdlib.h>
#include <glib/gprintf.h>

#include "gnc-ui-util.h"
#include "gnc-numeric.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include <unittest-support.h>
}

static void
test_num_print_info (gnc_numeric n, GNCPrintAmountInfo print_info, int line)
{
    gnc_numeric n_parsed = gnc_numeric_zero();
    const char *s;
    gboolean ok, print_ok;

    auto msg = "[PrintAmountInternal()] Bad numeric from rounding: GNC_ERROR_OVERFLOW.";
    auto log_domain = "gnc.gui";
    auto loglevel = static_cast<GLogLevelFlags>(G_LOG_LEVEL_WARNING);
    auto check = test_error_struct_new (log_domain, loglevel, msg);

    /* Throws overflows during rounding step in xaccPrintAmount when the "fraction" is high. See bug 665707. */
    auto hdlr = g_log_set_handler (log_domain, loglevel,
                              (GLogFunc)test_checked_handler, &check);
    s = xaccPrintAmount (n, print_info);
    print_ok = (s && s[0] != '\0');
    if (!print_ok)
        return;

    ok = xaccParseAmount (s, print_info.monetary, &n_parsed, NULL);
    g_log_remove_handler (log_domain, hdlr);


    do_test_args (ok, "parsing failure", __FILE__, __LINE__,
                  "num: %s, string %s (line %d)", gnc_numeric_to_string (n), s, line);

    ok = gnc_numeric_equal (n, n_parsed);
    do_test_args (ok, "not equal", __FILE__, __LINE__,
                  "start: %s, string %s, finish: %s (line %d)",
                  gnc_numeric_to_string (n), s,
                  gnc_numeric_to_string (n_parsed), line);
    test_error_struct_free (check);

}

static void
test_num (gnc_numeric n)
{
    GNCPrintAmountInfo print_info;
    int fraction;
    int i;

    print_info.commodity = NULL;
    print_info.min_decimal_places = 0;
    print_info.use_locale = 1;
    print_info.use_symbol = 0;

    for (i = 1, fraction = 10; i < 9; i++, fraction *= 10)
    {
        gnc_numeric n1;

        print_info.use_separators = 1;
        print_info.monetary = 1;
        print_info.max_decimal_places = i;
        print_info.force_fit = 0;
        print_info.round = 0;

        n1 = gnc_numeric_convert (n, fraction, GNC_HOW_RND_ROUND_HALF_UP);
        if (gnc_numeric_check(n1))
        {
            do_test_args((gnc_numeric_check(n1) == GNC_ERROR_OVERFLOW),
                         "BAD NUMERIC CONVERSION", __FILE__, __LINE__,
                         "num: %s, fraction: %d", gnc_numeric_to_string(n), fraction);
            continue;
        }

        test_num_print_info (n1, print_info, __LINE__);

        print_info.monetary = 0;
        test_num_print_info (n1, print_info, __LINE__);

        print_info.use_separators = 0;
        test_num_print_info (n1, print_info, __LINE__);

        print_info.round = 1;
        test_num_print_info (n1, print_info, __LINE__);

        print_info.round = 0;
        print_info.force_fit = 1;
        test_num_print_info (n1, print_info, __LINE__);

        print_info.round = 1;
        test_num_print_info (n1, print_info, __LINE__);
    }
}

#define IS_VALID_NUM(n,m)                                               \
    if (gnc_numeric_check(n)) {                                         \
        do_test_args(gnc_numeric_check(n) == GNC_ERROR_OVERFLOW,        \
                     "BAD NUMERIC", __FILE__, __LINE__,                 \
                     "num: %s (from %s)",                               \
                     gnc_numeric_to_string(n),                          \
                     gnc_numeric_to_string(m));                         \
        continue;                                                       \
    } else { m = n; }

static void
run_tests (void)
{
    int i;

    for (i = 0; i < 50; i++)
    {
        gnc_numeric n;
        gnc_numeric n1;

        n = get_random_gnc_numeric (GNC_DENOM_AUTO);
        IS_VALID_NUM(n, n);
        test_num (n);

        n1 = gnc_numeric_mul (n, n, n.denom, GNC_HOW_RND_ROUND_HALF_UP);
        IS_VALID_NUM(n1, n);
        test_num (n);

        n1 = gnc_numeric_mul (n, n, n.denom, GNC_HOW_RND_ROUND_HALF_UP);
        IS_VALID_NUM(n1, n);
        test_num (n);
    }
}

int
main (int argc, char **argv)
{
    run_tests ();
    print_test_results ();
    exit (get_rv ());
}
