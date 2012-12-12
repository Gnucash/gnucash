/**@internal
@file
\brief module definition/initialization for the generic import infrastructure
\author Copyright (c) 2002 Benoit Grégoire bock@step.polymtl.ca
*/

#include "config.h"
#include <gmodule.h>
#include <glib/gi18n.h>

#include "dialog-preferences.h"

#include "gnc-module.h"
#include "gnc-module-api.h"

GNC_MODULE_API_DECL(libgncmod_generic_import)

/* version of the gnc module system interface we require */
int libgncmod_generic_import_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_generic_import_gnc_module_current  = 0;
int libgncmod_generic_import_gnc_module_revision = 0;
int libgncmod_generic_import_gnc_module_age      = 0;

char *
libgncmod_generic_import_gnc_module_path(void)
{
    return g_strdup("gnucash/import-export");
}

char *
libgncmod_generic_import_gnc_module_description(void)
{
    return g_strdup("Gnome GUI and C code for the generic import functions");
}

int
libgncmod_generic_import_gnc_module_init(int refcount)
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

    if (!refcount)
    {
        /* Add to preferences under Online Banking */
        /* The parameters are; glade file, items to add from glade file - last being the dialog, preference tab name */
        gnc_preferences_add_to_page("dialog-import.glade", "atm_fee_adj,auto_add_adj,auto_clear_adj,match_adj,matcher_prefs",
                                    _("Online Banking"));
    }

    return TRUE;
}

int
libgncmod_generic_import_gnc_module_end(int refcount)
{
    return TRUE;
}
