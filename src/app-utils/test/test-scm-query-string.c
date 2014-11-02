
#include "config.h"
#include <glib.h>
#include <libguile.h>
#include "guile-mappings.h"

#include "engine-helpers-guile.h"
#include "gnc-module.h"
#include "gnc-guile-utils.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "Query.h"
#include "TransLog.h"


static void
test_query (Query *q, SCM val2str)
{
    SCM scm_q;
    SCM str_q;
    SCM res_q;
    SCM args = SCM_EOL;
    Query *q2;
    gchar *str2 = NULL;

    scm_q = gnc_query2scm (q);
    args = scm_cons (scm_q, SCM_EOL);
    str_q = scm_apply (val2str, args, SCM_EOL);

    args = scm_cons (scm_from_utf8_string ("'"), scm_cons (str_q, SCM_EOL));
    str_q = scm_string_append (args);

    str2 = gnc_scm_to_utf8_string (str_q);
    if (str2)
    {
        res_q = scm_c_eval_string (str2);
    }
    else
    {
        res_q = SCM_BOOL_F;
    }

    q2 = gnc_scm2query (res_q);

    if (!qof_query_equal (q, q2))
    {
        failure ("queries don't match");
        fprintf (stderr, "%s\n\n", str2 ? str2 : "(null)");
        scm_q = gnc_query2scm (q2);
        scm_display (scm_q, SCM_UNDEFINED);
        scm_newline (SCM_UNDEFINED);
        g_free(str2);
        exit (1);
    }
    else
    {
        success ("queries match");
    }
    g_free(str2);
    if (q2) qof_query_destroy (q2);
}

static void
run_tests (void)
{
    Query *q;
    SCM val2str;
    int i;

    val2str = scm_c_eval_string ("gnc:value->string");
    g_return_if_fail (scm_is_procedure (val2str));

    for (i = 0; i < 1000; i++)
    {
        q = get_random_query ();
        test_query (q, val2str);
        qof_query_destroy (q);
        fflush(stdout);
    }

    {
        q = get_random_query ();
        test_query (q, val2str);
        qof_query_destroy (q);
        fflush(stdout);
    }

}

static void
main_helper (void *closure, int argc, char **argv)
{
    gnc_module_system_init ();
    gnc_module_load("gnucash/engine", 0);
    gnc_module_load("gnucash/app-utils", 0);

    xaccLogDisable ();

    /* double->string->double is not idempotent */
    kvp_exclude_type (KVP_TYPE_DOUBLE);

    /* Initialize to a known RNG position */
    srand(1);

    run_tests ();

    print_test_results ();

    exit (get_rv ());
}

int
main (int argc, char **argv)
{
    g_setenv ("GNC_UNINSTALLED", "1", TRUE);
/* When built with clang, guile-1.8.8's scm_c_eval_string truncates all
 * integer values to int32, which causes this test to fail.
 */
#ifndef __clang__
    scm_boot_guile (argc, argv, main_helper, NULL);
#endif
    return 0;
}
