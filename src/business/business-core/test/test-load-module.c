/*********************************************************************
 * test-load-module.c
 *
 * Copyright (c) 2001 Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *
 *********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <libguile.h>

#include "gnc-module.h"

static void
guile_main (void *closure, int argc, char ** argv)
{
    GNCModule module;

    printf("  test-load-module.c: loading/unloading business-core module ... ");

    gnc_module_system_init();
    module = gnc_module_load("gnucash/business-core", 0);

    if (!module)
    {
        printf("  Failed to load engine\n");
        exit(-1);
    }

    if (!gnc_module_unload(module))
    {
        printf("  Failed to unload engine\n");
        exit(-1);
    }
    printf(" successful.\n");

    exit(0);
}

int
main (int argc, char ** argv)
{
    scm_boot_guile(argc, argv, guile_main, NULL);
    return 0;
}
