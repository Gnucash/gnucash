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
#include <config.h>

#include <stdlib.h>
#include "gnc-engine.h"

#include "test-engine-stuff.h"

#include "test-file-stuff.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"
#include "test-stuff.h"

#include <optional>

#define GNC_V2_STRING "gnc-v2"
const gchar* gnc_v2_xml_version_string = GNC_V2_STRING;

static const char* test_strings[] =
{
    "FooBar",
    "<Ugly crap>",
    "Something with a & in it",
    "Ugly(*!&@#$NTHOEAUTF\"ntaheu09.h,.        \n\t",
    "\n\t\n\t",
    NULL
};

static void
test_string_converters (void)
{
    int i;

    for (i = 0; test_strings[i]; ++i)
    {
        const char* mark = test_strings[i];
        xmlNodePtr test_node = text_to_dom_tree ("test-string", mark);
        char* backout = dom_tree_to_text (test_node);

        do_test_args (
            g_strcmp0 (backout, mark) == 0,
            "string converting", __FILE__, __LINE__, "with string %s", mark);

        g_free (backout);
        xmlFreeNode (test_node);
    }
}

static void
test_bad_string (void)
{
    const char* badstr = "foo\abar";
    const char* sanitized = "foo?bar";
    xmlNodePtr test_node = text_to_dom_tree ("test-string", badstr);

    char* backout = dom_tree_to_text (test_node);
    do_test_args (g_strcmp0 (backout, sanitized) == 0,
                  "string sanitizing", __FILE__, __LINE__,
                  "with string %s", badstr);

    g_free (backout);
    xmlFreeNode (test_node);
}

template <class T>
using TestcaseVec = std::vector<std::pair<const char*, std::optional<T>>>;

template <class T>
const TestcaseVec<T> test_cases_common = {
    { "1"                     ,   1 },
    { "  \t 2   \t\v\n\f\r  " ,   2 },
    { "              0      " ,   0 },
    { "123"                   , 123 },
    { "123z"                  ,  {} },
    { "a123"                  ,  {} },
    { " 23"                   ,  23 },
    { "\t23"                  ,  23 },
    { "44 "                   ,  44 },
    { "44\t"                  ,  44 },
    { " 56   "                ,  56 },
    { "\t56\t"                ,  56 },
    { "1 2"                   ,  {} },
    { "1 2 \t"                ,  {} },
};

const TestcaseVec<gint64> test_cases_gint64 = {
    { "-44"                   , -44 },
    { "9223372036854775807"   ,  9223372036854775807 }, // maxint64
    { "9223372036854775808"   ,  {} },                  // overflow
    { "-9223372036854775807"  , -9223372036854775807 }, // minint64
};

const TestcaseVec<guint> test_cases_guint = {
    { "-44"                   ,  {} },                // no negative allowed
    { "4294967295"            ,  4294967295 },        // max_uint
    { "4294967296"            ,  {} },                // overflow
};

const TestcaseVec<guint16> test_cases_guint16 = {
    { "-44"                   ,  {} },                // no negative allowed
    { "65535"                 ,  65535 },             // max_int16
    { "65536"                 ,  {} },                // overflow
};

const TestcaseVec<double> test_cases_double = {
    { "-3.5"                   ,  -3.5 },
    { ".5"                     ,   0.5 },
    { "1e10"                   ,  1e10 },
    { "-1e10"                  , -1e10 },
    { "1e-10"                  ,  1e-10 },
    { "-1e-10"                 ,  -1e-10 },
    { "1.7976931348623158e+308", 1.7976931348623158e+308 }, // max_double
    { "1.7976931348623159e+308", {}   },                    // overflow
};

template <typename T>
static void
test_string_to_num (const char *func_name, const TestcaseVec<T>& test_cases,
                    const std::function<bool(const char*,T*)> &func)
{
    auto do_test = [&](std::pair<const char*, std::optional<T>> test_pair)
    {
        T num = 0;
        auto [test_str, test_int] = test_pair;
        auto rv = func (test_str, &num);
        // Output for debugging
        // std::cout << "test_str = [" << test_str << "], test_int = ";
        // if (test_int)
        //     std::cout << *test_int;
        // else
        //     std::cout << "{}";
        // std::cout << ", num = [" << num << "], rv = " << rv << std::endl;
        do_test_args (rv == test_int.has_value(), func_name,
                      __FILE__, __LINE__, "with string %s", test_str);
        if (rv)
            do_test_args (num == *test_int, func_name,
                          __FILE__, __LINE__, "with string %s", test_str);
    };

    std::for_each (test_cases_common<T>.begin(), test_cases_common<T>.end(), do_test);
    std::for_each (test_cases.begin(), test_cases.end(), do_test);
}

int
main (int argc, char** argv)
{
    qof_log_init ();
    fflush (stdout);
    test_string_converters ();
    test_bad_string ();
#if __cpp_lib_to_chars >= 201611L
    // because older strtod code is more liberal and parses "123z" as 123.0
    test_string_to_num<double> ("string_to_double", test_cases_double, string_to_double);
#endif
    test_string_to_num<gint64> ("string_to_gint64", test_cases_gint64, string_to_gint64);
    test_string_to_num<guint16>("string_to_guint16",test_cases_guint16,string_to_guint16);
    test_string_to_num<guint>  ("string_to_guint",  test_cases_guint,  string_to_guint);
    fflush (stdout);
    print_test_results ();
    exit (get_rv ());
}
