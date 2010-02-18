/*********************************************************************
 * gncmod-report-system.c
 * module definition/initialization for the report infrastructure
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include "config.h"
#include <gmodule.h>
#include <libguile.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

GNC_MODULE_API_DECL(libgncmod_report_system)

/* version of the gnc module system interface we require */
int libgncmod_report_system_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_report_system_gnc_module_current  = 0;
int libgncmod_report_system_gnc_module_revision = 0;
int libgncmod_report_system_gnc_module_age      = 0;


char *
libgncmod_report_system_gnc_module_path(void)
{
    return g_strdup("gnucash/report/report-system");
}

char *
libgncmod_report_system_gnc_module_description(void)
{
    return g_strdup("Core components of GnuCash report generation system");
}

extern SCM scm_init_sw_report_system_module(void);

int
libgncmod_report_system_gnc_module_init(int refcount)
{
    /* load the engine (we depend on it) */
    if (!gnc_module_load("gnucash/engine", 0))
    {
        return FALSE;
    }

    if (!gnc_module_load("gnucash/app-utils", 0))
    {
        return FALSE;
    }
    scm_init_sw_report_system_module();

    scm_c_eval_string("(use-modules (gnucash report report-system))");

    /* if this is the first time the module's being loaded, initialize
     * the relative date system */
    if (refcount == 0)
    {
        scm_c_eval_string("(gnc:reldate-initialize)");
    }

    return TRUE;
}

int
libgncmod_report_system_gnc_module_end(int refcount)
{
    return TRUE;
}
