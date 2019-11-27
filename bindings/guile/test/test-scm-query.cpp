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

#include "gnc-engine-guile.h"
#include "gnc-module.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "Query.h"
#include "TransLog.h"
}

static void
test_query (QofQuery *q)
{
    SCM scm_q;
    QofQuery *q2;

    scm_q = gnc_query2scm (q);

    q2 = gnc_scm2query (scm_q);

    if (!qof_query_equal (q, q2))
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

    qof_query_destroy (q2);
}

static void
run_tests (void)
{
    QofQuery *q;
    int i;

    test_query (NULL);

    q = qof_query_create_for(GNC_ID_SPLIT);
    test_query (q);
    qof_query_destroy (q);

    for (i = 0; i < 50; i++)
    {
        q = get_random_query ();
        test_query (q);
        qof_query_destroy (q);
    }
}

static void
main_helper (void *closure, int argc, char **argv)
{
    gnc_engine_init (0, NULL);

    xaccLogDisable ();

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
