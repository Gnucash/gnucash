/*********************************************************************
 * gncmod-app-utils.c
 * module definition/initialization for the report infrastructure
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/
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
#include <gmodule.h>
#include <libguile.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

#include "gnc-component-manager.h"
#include "gnc-hooks.h"
#include "gnc-exp-parser.h"

GNC_MODULE_API_DECL(libgncmod_app_utils)

/* version of the gnc module system interface we require */
int libgncmod_app_utils_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_app_utils_gnc_module_current  = 0;
int libgncmod_app_utils_gnc_module_revision = 0;
int libgncmod_app_utils_gnc_module_age      = 0;


char *
libgncmod_app_utils_gnc_module_path(void)
{
    return g_strdup("gnucash/app-utils");
}

char *
libgncmod_app_utils_gnc_module_description(void)
{
    return g_strdup("Utilities for building gnc applications");
}

static void
lmod(char * mn)
{
    char * form = g_strdup_printf("(use-modules %s)\n", mn);
    scm_c_eval_string(form);
    g_free(form);
}

static void
app_utils_shutdown(void)
{
    gnc_exp_parser_shutdown();
    gnc_hook_run(HOOK_SAVE_OPTIONS, NULL);
}


extern SCM scm_init_sw_app_utils_module(void);

int
libgncmod_app_utils_gnc_module_init(int refcount)
{
    /* load the engine (we depend on it) */
    if (!gnc_module_load("gnucash/engine", 0))
    {
        return FALSE;
    }

    scm_init_sw_app_utils_module();
    /* publish swig bindings */
    /* load the scheme code */
    lmod("(sw_app_utils)");
    lmod("(gnucash app-utils)");

    if (refcount == 0)
    {
        gnc_component_manager_init ();
        gnc_hook_add_dangler(HOOK_STARTUP, (GFunc)gnc_exp_parser_init, NULL);
        gnc_hook_add_dangler(HOOK_SHUTDOWN, (GFunc)app_utils_shutdown, NULL);
    }

    return TRUE;
}

int
libgncmod_app_utils_gnc_module_end(int refcount)
{
    if (refcount == 0)
        gnc_component_manager_shutdown ();

    return TRUE;
}
