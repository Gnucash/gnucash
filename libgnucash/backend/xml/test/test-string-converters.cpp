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
extern "C"
{
#include "config.h"

#include <stdlib.h>
#include "gnc-engine.h"

#include "test-engine-stuff.h"
}

#include "test-file-stuff.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"
#include "test-stuff.h"


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
}

int
main (int argc, char** argv)
{
    qof_log_init ();
    fflush (stdout);
    test_string_converters ();
    test_bad_string ();
    fflush (stdout);
    print_test_results ();
    exit (get_rv ());
}
