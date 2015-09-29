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

#include "config.h"

#include <stdlib.h>

#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-engine.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "test-file-stuff.h"

#define GNC_V2_STRING "gnc-v2"
const gchar *gnc_v2_xml_version_string = GNC_V2_STRING;

static void
test_binary()
{
    int i;
    for (i = 0; i < 20; i++)
    {
        bin_data *test_data1;
        void *test_data2;
        guint64 test_data2_len;
        gchar *converted;

        test_data1 = get_random_binary_data();

        converted = binary_to_string(test_data1->data, test_data1->len);

        if (!converted)
        {
            failure_args("binary_data", __FILE__, __LINE__, "binary_to_string returned NULL");
            continue;
        }

        if (!string_to_binary(converted, &test_data2, &test_data2_len))
        {
            failure_args("binary_data", __FILE__, __LINE__,
                         "string_to_binary returned FALSE with data:\n%s\n",
                         converted);
            continue;
        }

        if (test_data2_len != test_data1->len)
        {
            failure_args("binary_data", __FILE__, __LINE__,
                         "lengths don't match: %" G_GINT64_FORMAT " vs %d",
                         test_data2_len, test_data1->len);
            continue;
        }

        {
            int j;
            guchar *d1 = test_data1->data;
            guchar *d2 = (guchar*)test_data2;

            for (j = 0; j < test_data2_len; j++)
            {
                if (d1[j] != d2[j])
                {
                    failure_args("binary_data", __FILE__, __LINE__,
                                 "data doesn't match at point %d.\n%d vs %d",
                                 i, d1[j], d2[j]);
                    continue;
                }
            }
        }

        success("binary_data");
    }
}


static char *test_strings[] =
{
    "FooBar",
    "<Ugly crap>",
    "Something with a & in it",
    "Ugly(*!&@#$NTHOEAUTF\"ntaheu09.h,.        \n\t",
    "\n\t\n\t",
    NULL
};

static void
test_string_converters(void)
{
    char *mark;
    int i;

    for (i = 0, mark = test_strings[i]; mark; i++, mark = test_strings[i])
    {
        xmlNodePtr test_node;
        gchar *backout;
        test_node = text_to_dom_tree("test-string", mark);

        backout = dom_tree_to_text(test_node);

        do_test_args(
            g_strcmp0(backout, mark) == 0,
            "string converting", __FILE__, __LINE__, "with string %s", mark);
    }
}

static void
test_bad_string (void)
{
    gchar *badstr = "foo\abar";
    gchar *sanitized = "foo?bar";
    gchar *backout;
    xmlNodePtr test_node = text_to_dom_tree ("test-string", badstr);

    backout = dom_tree_to_text (test_node);
    do_test_args (g_strcmp0 (backout, sanitized) == 0,
		  "string sanitizing", __FILE__, __LINE__,
		  "with string %s", badstr);
}

int
main(int argc, char **argv)
{
    qof_log_init();
    test_binary();
    fflush(stdout);
    test_string_converters();
    test_bad_string ();
    fflush(stdout);
    print_test_results();
    exit(get_rv());
}
