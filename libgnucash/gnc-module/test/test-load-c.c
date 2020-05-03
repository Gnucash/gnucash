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
#include <stdio.h>
#include <stdlib.h>
#include <libguile.h>
#include <unittest-support.h>

#include "gnc-module.h"

static void
guile_main(void *closure, int argc, char ** argv)
{
    GNCModule foo;
    gchar *msg = "Module '../../../libgnucash/gnc-module/test/misc-mods/.libs/libgncmod-futuremodsys.so' requires newer module system\n";
    gchar *logdomain = "gnc.module";
    guint loglevel = G_LOG_LEVEL_WARNING;
    TestErrorStruct check = { loglevel, logdomain, msg };
    g_log_set_handler (logdomain, loglevel,
                       (GLogFunc)test_checked_handler, &check);

    g_test_message("  test-load-c.c: testing module load/unload from C ... ");

    gnc_module_system_init();

    foo = gnc_module_load("gnucash/foo", 0);

    if (!foo)
    {
        g_test_message("  Failed to load foo\n");
        exit(-1);
    }

    if (!gnc_module_unload(foo))
    {
        g_test_message("  Failed to unload foo\n");
        exit(-1);
    }
    g_test_message(" successful.\n");

    exit(0);
}

int
main(int argc, char ** argv)
{
    scm_boot_guile(argc, argv, guile_main, NULL);
    return 0;
}
