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
#include <glib.h>
#include <gtk/gtk.h>
#include <libguile.h>

#include <stdlib.h>
#include "gnc-ui-util.h"

#include "test-stuff.h"

#include "guile-mappings.h"
#include "gnc-guile-utils.h"

#include "../business-gnome-utils.h"

static void
test_basic()
{
    do_test(g_strcmp0(gnc_get_builtin_default_invoice_print_report (),
                      "5123a759ceb9483abf2182d01c140e8d") == 0,
                      "built in invoice report define does not match guid");
}

static void
test_list()
{
    SCM template_menu_name = scm_c_eval_string ("gnc:report-template-menu-name/report-guid");
    SCM get_rpt_guids = scm_c_eval_string ("gnc:custom-report-invoice-template-guids");
    int number_of_defined_invoice_templates = 5;
    int number_of_found_invoice_templates = 0;
    gboolean printable_found = false;
    gboolean builtin_default_found = false;
    const char *builtin_default = gnc_get_builtin_default_invoice_print_report ();

    if (scm_is_procedure (get_rpt_guids))
    {
        SCM reportlist = scm_call_0 (get_rpt_guids);
        SCM rpt_guids = reportlist;

        if (scm_is_list (rpt_guids))
        {
            for (int i = 1; !scm_is_null (rpt_guids); i++)
            {
                gchar *guid_str = scm_to_utf8_string (SCM_CAR(rpt_guids));
                gchar *name = gnc_scm_to_utf8_string (scm_call_2(template_menu_name,
                                                      SCM_CAR(rpt_guids), SCM_BOOL_F));

                if (g_strcmp0 (guid_str, builtin_default) == 0)
                {
                    if (g_strcmp0 (name, "Printable Invoice") == 0)
                        printable_found = true;

                    builtin_default_found = true;
                }
                g_free (guid_str);
                g_free (name);

                number_of_found_invoice_templates = i;
                rpt_guids = SCM_CDR(rpt_guids);
            }
        }
    }
    do_test(number_of_found_invoice_templates == number_of_defined_invoice_templates, "number of built in invoice templates does not match");
    do_test(builtin_default_found == true, "built in default invoice guid not found");
    do_test(printable_found == true, "built in Printable Invoice not found with default guid");
}

static void
real_main(void *closure, int argc, char **argv)
{
    g_setenv ("GNC_UNINSTALLED", "1", TRUE);
    qof_init();
    gnc_engine_init(0, NULL);

    scm_c_use_module ("gnucash reports");
    scm_c_use_module ("gnucash report report-core");

    test_basic();
    test_list();

    print_test_results();
    exit(get_rv());
}

int main(int argc, char **argv)
{
    /* do things this way so we can test scheme function calls from expressions */
    scm_boot_guile(argc, argv, real_main, NULL);
    return 0;
}
