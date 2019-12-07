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

#include "gnc-module.h"

int
main(int argc, char ** argv)
{
    GNCModule foo;
    gchar *msg1 = "Could not locate module gnucash/ordinary interface v.25";
    gchar *msg2 = "Initialization failed for module gnucash/incompatdep";

    g_test_message("  test-incompatdep.c:  loading a module with bad deps ...\n");

    g_test_expect_message ("gnc.module", G_LOG_LEVEL_WARNING, msg1);
    g_test_expect_message ("gnc.module", G_LOG_LEVEL_WARNING, msg2);

    gnc_module_system_init();

    foo = gnc_module_load("gnucash/incompatdep", 0);

    g_test_assert_expected_messages();

    if (!foo)
    {
        printf("  ok\n");
        exit(0);
    }
    else
    {
        printf("  oops! loaded incompatible module\n");
        exit(-1);
    }
}
