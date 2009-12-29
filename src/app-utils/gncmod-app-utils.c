/*********************************************************************
 * gncmod-app-utils.c
 * module definition/initialization for the report infrastructure
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include "config.h"
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

    /* load the calculation module (we depend on it) */
    if (!gnc_module_load("gnucash/calculation", 0))
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
