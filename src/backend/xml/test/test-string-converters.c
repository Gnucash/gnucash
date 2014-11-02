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
    fflush(stdout);
    test_string_converters();
    test_bad_string ();
    fflush(stdout);
    print_test_results();
    exit(get_rv());
}
