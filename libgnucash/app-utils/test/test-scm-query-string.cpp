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

#include <libguile.h>

extern "C"
{
#include <config.h>
#include <glib.h>
#include "guile-mappings.h"

#include "engine-helpers-guile.h"
#include "gnc-module.h"
#include "gnc-guile-utils.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "Query.h"
#include "TransLog.h"
}

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
        res_q = scm_eval_string (str_q);
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
    scm_boot_guile (argc, argv, main_helper, NULL);
    return 0;
}
