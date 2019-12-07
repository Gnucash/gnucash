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
#include <unittest-support.h>
#include "test/mod-ordinary/ordinary.h"
#include "test/mod-withdep/withdep.h"

#include "gnc-module.h"

int
main(int argc, char ** argv)
{
    gint retval = 0;
    GNCModule testmod;

    gnc_module_system_init();

    g_test_message("  test-load-c.c: load module gnucash/ordinary from C ... ");
    testmod = gnc_module_load("gnucash/ordinary", 0);
    if (!testmod)
    {
        g_test_message("  failed\n");
        exit(-1);
    }
    g_test_message(" ok\n");

    g_test_message("  test-load-c.c: call function ordinary_hello in module gnucash/ordinary ... ");
    retval = ordinary_hello();
    if (retval != 10)
    {
        g_test_message("  failed. Expected 10, got %i\n", retval);
        exit(-1);
    }
    g_test_message(" ok\n");

    g_test_message("  test-load-c.c: unload module gnucash/ordinary from C ... ");
    if (!gnc_module_unload(testmod))
    {
        g_test_message("  failed\n");
        exit(-1);
    }
    g_test_message(" ok.\n");

    g_test_message("  test-load-c.c: load module gnucash/withdep from C ... ");
    testmod = gnc_module_load("gnucash/withdep", 0);
    if (!testmod)
    {
        g_test_message("  failed\n");
        exit(-1);
    }
    g_test_message(" ok\n");

    g_test_message("  test-load-c.c: call function withdep_hello in module gnucash/withdep ... ");
    retval = withdep_hello();
    if (retval != 11)
    {
        g_test_message("  failed. Expected 11, got %i\n", retval);
        exit(-1);
    }
    g_test_message(" ok\n");

    g_test_message("  test-load-c.c: call function ordinary_hello in depended on module gnucash/ordinary ... ");
    retval = ordinary_hello();
    if (retval != 10)
    {
        g_test_message("  failed. Expected 10, got %i\n", retval);
        exit(-1);
    }
    g_test_message(" ok\n");

    g_test_message("  test-load-c.c: unload module gnucash/withdep from C ... ");
    if (!gnc_module_unload(testmod))
    {
        g_test_message("  failed\n");
        exit(-1);
    }
    g_test_message(" ok.\n");

    exit(0);
}
