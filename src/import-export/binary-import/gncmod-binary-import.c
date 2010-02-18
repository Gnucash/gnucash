/*********************************************************************
 * gncmod-binary-import.c
 * module definition/initialization for importing gnucash binary files
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include "config.h"
#include <gmodule.h>
#include <libguile.h>

#include "gnc-hooks.h"
#include "gnc-module.h"
#include "gnc-module-api.h"
#include "druid-commodity.h"

GNC_MODULE_API_DECL(libgncmod_binary_import)

/* version of the gnc module system interface we require */
int libgncmod_binary_import_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_binary_import_gnc_module_current  = 0;
int libgncmod_binary_import_gnc_module_revision = 0;
int libgncmod_binary_import_gnc_module_age      = 0;


char *
libgncmod_binary_import_gnc_module_path(void)
{
    return g_strdup("gnucash/import-export/binary-import");
}

char *
libgncmod_binary_import_gnc_module_description(void)
{
    return g_strdup("Utilities importing GnuCash binary files");
}

int
libgncmod_binary_import_gnc_module_init(int refcount)
{
    /* load the engine (we depend on it) */
    if (!gnc_module_load("gnucash/engine", 0))
    {
        return FALSE;
    }

    /* load the calculation module (we depend on it) */
    if (!gnc_module_load("gnucash/app-utils", 0))
    {
        return FALSE;
    }

    /* load the calculation module (we depend on it) */
    if (!gnc_module_load("gnucash/gnome-utils", 0))
    {
        return FALSE;
    }

    if (refcount == 0)
    {
        gnc_hook_add_dangler(HOOK_BOOK_OPENED, (GFunc)gnc_import_commodities, NULL);
    }
    return TRUE;
}

int
libgncmod_binary_import_gnc_module_end(int refcount)
{
    return TRUE;
}
