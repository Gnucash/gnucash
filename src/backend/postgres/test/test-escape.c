#include <glib.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "escape.h"
#include "test-stuff.h"

static sqlEscape *escape = NULL;

static void
test_escape (const char *s, const char *expected)
{
    const char *escaped;
    gboolean success;

    escaped = sqlEscapeString (escape, s);

    if (escaped == expected)
        success = TRUE;
    else
        success = (strcmp (escaped, expected) == 0);

    do_test_args (success, "escape test", __FILE__, __LINE__,
                  "bad escaping: expected %s -> %s, got %s",
                  s, expected, escaped);
}

int
main (int argc, char *argv[])
{
    int i;

    random_character_include_funky_chars (TRUE);

    escape = sqlEscape_new ();

    test_escape (NULL, NULL);
    test_escape ("", "");
    test_escape ("'", "\\'");
    test_escape ("\\", "\\\\");

    for (i = 0; i < 200; i++)
    {
        char *s;
        const char *ss;

        s = get_random_string ();

        ss = sqlEscapeString (escape, s);
        sqlEscapeString (escape, ss);

        g_free (s);
    }

    success ("crash test");

    sqlEscape_destroy (escape);

    print_test_results ();
    exit (get_rv ());
}
