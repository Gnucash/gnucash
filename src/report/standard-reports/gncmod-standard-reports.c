/*********************************************************************
 * gncmod-standard-reports.c
 * module definition/initialization for the standard reports
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include "config.h"
#include <gmodule.h>
#include <libguile.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

GNC_MODULE_API_DECL(libgncmod_standard_reports)

/* version of the gnc module system interface we require */
int libgncmod_standard_reports_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_standard_reports_gnc_module_current  = 0;
int libgncmod_standard_reports_gnc_module_revision = 0;
int libgncmod_standard_reports_gnc_module_age      = 0;


char *
libgncmod_standard_reports_gnc_module_path(void)
{
    return g_strdup("gnucash/report/standard-reports");
}

char *
libgncmod_standard_reports_gnc_module_description(void)
{
    return g_strdup("Standard income, asset, balance sheet, etc. reports");
}

int
libgncmod_standard_reports_gnc_module_init(int refcount)
{
    /* load the report system */
    if (!gnc_module_load("gnucash/report/report-system", 0))
    {
        return FALSE;
    }

    /* load the report generation scheme code */
    if (scm_c_eval_string("(use-modules (gnucash report standard-reports))") ==
            SCM_BOOL_F)
    {
        return FALSE;
    }
    return TRUE;
}

int
libgncmod_standard_reports_gnc_module_end(int refcount)
{
    return TRUE;
}
