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

#include "gnc-module.h"
#include <unittest-support.h>

int
main(int argc, char ** argv)
{
    GNCModule testmod;
    g_test_message("  test-agedver.c:  asking for an old but supported interface ...");

    gnc_module_system_init();

    testmod = gnc_module_load("gnucash/agedver", 5);

    if (testmod)
    {
        printf("  ok\n");
        exit(0);
    }
    else
    {
        printf(" failed\n");
        exit(-1);
    }
}
