
#include "config.h"
#include <glib.h>
#include <libguile.h>

#include "engine-helpers.h"
#include "gnc-module.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "Query.h"
#include "TransLog.h"


static void
test_query (Query *q)
{
    SCM scm_q;
    Query *q2;

    scm_q = gnc_query2scm (q);

    q2 = gnc_scm2query (scm_q);

    if (!xaccQueryEqual (q, q2))
    {
        failure ("queries don't match");
        scm_display (scm_q, SCM_UNDEFINED);
        scm_newline (SCM_UNDEFINED);
        scm_q = gnc_query2scm (q2);
        scm_display (scm_q, SCM_UNDEFINED);
        scm_newline (SCM_UNDEFINED);
        exit (1);
    }
    else
    {
        success ("queries match");
    }

    xaccFreeQuery (q2);
}

static void
run_tests (void)
{
    Query *q;
    int i;

    test_query (NULL);

    q = xaccMallocQuery ();
    test_query (q);
    xaccFreeQuery (q);

    for (i = 0; i < 50; i++)
    {
        q = get_random_query ();
        test_query (q);
        xaccFreeQuery (q);
    }
}

static void
main_helper (void *closure, int argc, char **argv)
{
    gnc_module_load("gnucash/engine", 0);

    xaccLogDisable ();

    /* scm conversion doesn't handle binary atm */
    kvp_exclude_type (KVP_TYPE_BINARY);

    run_tests ();

    print_test_results ();

    exit (get_rv ());
}

int
main (int argc, char **argv)
{
    scm_boot_guile (argc, argv, main_helper, NULL);
    return 0;
}
