/*********************************************************************
 * gncmod-qif-import.c
 * module definition/initialization for old QIF importer (deprecated)
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include "config.h"
#include <gmodule.h>
#include <libguile.h>

#include "gnc-module.h"
#include "gnc-module-api.h"
#include "druid-qif-import.h"
#include "dialog-new-user.h"

#include "gnc-plugin-qif-import.h"

GNC_MODULE_API_DECL(libgncmod_qif_import)

/* version of the gnc module system interface we require */
int libgncmod_qif_import_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_qif_import_gnc_module_current  = 0;
int libgncmod_qif_import_gnc_module_revision = 0;
int libgncmod_qif_import_gnc_module_age      = 0;


char *
libgncmod_qif_import_gnc_module_path(void)
{
    return g_strdup("gnucash/import-export/qif-import");
}

char *
libgncmod_qif_import_gnc_module_description(void)
{
    return g_strdup("Gnome GUI and Scheme code for QIF importer");
}

int
libgncmod_qif_import_gnc_module_init(int refcount)
{
    if (!gnc_module_load("gnucash/engine", 0))
    {
        return FALSE;
    }

    if (!gnc_module_load("gnucash/app-utils", 0))
    {
        return FALSE;
    }

    if (!gnc_module_load("gnucash/gnome-utils", 0))
    {
        return FALSE;
    }

    /* If the recount == 0 then register the qif-import-druid for the new-user
     * dialog.
     */
    if (refcount == 0)
    {
        gnc_new_user_dialog_register_qif_druid
        ((void (*)())gnc_ui_qif_import_druid_make);
    }

    scm_c_eval_string("(use-modules (gnucash import-export qif-import))");

    gnc_plugin_qif_import_create_plugin();

    return TRUE;
}

int
libgncmod_qif_import_gnc_module_end(int refcount)
{
    return TRUE;
}
