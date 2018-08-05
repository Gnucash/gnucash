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
    SCM args = SCM_EOL;

    scm_q = gnc_query2scm (q);
    args = scm_cons (scm_q, SCM_EOL);
    str_q = scm_apply (val2str, args, SCM_EOL);

    args = scm_cons (scm_from_utf8_string ("'"), scm_cons (str_q, SCM_EOL));
    str_q = scm_string_append (args);

    scm_display (str_q, SCM_UNDEFINED);
    scm_newline (SCM_UNDEFINED);
    scm_newline (SCM_UNDEFINED);
}

static void
run_tests (int count)
{
    Query *q;
    SCM val2str;
    int i;

    val2str = scm_c_eval_string ("gnc:value->string");
    g_return_if_fail (scm_is_procedure (val2str));

    for (i = 0; i < count; i++)
    {
        q = get_random_query ();
        test_query (q, val2str);
        qof_query_destroy (q);
    }
    success ("");
}

static void
main_helper (void *closure, int argc, char **argv)
{
    int count = 50;

    gnc_module_load("gnucash/engine", 0);
    gnc_module_load("gnucash/app-utils", 0);

    if (argc > 1)
        count = atoi (argv[1]);

    if (count < 0)
        count = 0;

    xaccLogDisable ();

    run_tests (count);

    print_test_results ();

    exit (get_rv ());
}

int
main (int argc, char **argv)
{
    scm_boot_guile (argc, argv, main_helper, NULL);
    return 0;
}
