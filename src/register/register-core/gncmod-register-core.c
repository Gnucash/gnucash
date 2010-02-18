/*********************************************************************
 * gncmod-registercore.c
 * module definition/initialization for core (gui-independent) register
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include "config.h"
#include <gmodule.h>
#include <libguile.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

GNC_MODULE_API_DECL(libgncmod_register_core)

/* version of the gnc module system interface we require */
int libgncmod_register_core_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_register_core_gnc_module_current  = 0;
int libgncmod_register_core_gnc_module_revision = 0;
int libgncmod_register_core_gnc_module_age      = 0;


char *
libgncmod_register_core_gnc_module_path(void)
{
    return g_strdup("gnucash/register/register-core");
}

char *
libgncmod_register_core_gnc_module_description(void)
{
    return g_strdup("Toolkit-independent GUI for ledger-like table displays");
}

int
libgncmod_register_core_gnc_module_init(int refcount)
{
    if (!gnc_module_load("gnucash/engine", 0))
    {
        return FALSE;
    }

    /* FIXME. We need this for the wide-character functions.
     * When fixing, get rid of gnome-utils includes, too. */
    if (!gnc_module_load("gnucash/gnome-utils", 0))
    {
        return FALSE;
    }

    return TRUE;
}
