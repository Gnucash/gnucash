/*********************************************************************
 * gncmod-ledgercore.c
 * module definition/initialization for core (gui-independent) ledger
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include <gmodule.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

GNC_MODULE_API_DECL(libgncmod_ledger_core)

/* version of the gnc module system interface we require */
int libgncmod_ledger_core_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_ledger_core_gnc_module_current  = 0;
int libgncmod_ledger_core_gnc_module_revision = 0;
int libgncmod_ledger_core_gnc_module_age      = 0;


char *
libgncmod_ledger_core_gnc_module_path(void)
{
    return g_strdup("gnucash/register/ledger-core");
}

char *
libgncmod_ledger_core_gnc_module_description(void)
{
    return g_strdup("Toolkit-independent GUI for financial ledgers");
}

int
libgncmod_ledger_core_gnc_module_init(int refcount)
{
    if (!gnc_module_load("gnucash/engine", 0))
    {
        return FALSE;
    }

    if (!gnc_module_load("gnucash/register/register-core", 0))
    {
        return FALSE;
    }

    if (!gnc_module_load("gnucash/app-utils", 0))
    {
        return FALSE;
    }

    return TRUE;
}
