/*********************************************************************
 * businessmod-core.c
 * module definition/initialization for the Business GNOME UI module
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gmodule.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <libguile.h>

#include "gnc-hooks.h"
#include "gnc-module.h"
#include "gnc-module-api.h"

#include "search-core-type.h"
#include "search-owner.h"
#include "gncOwner.h"
#include "business-options-gnome.h"
#include "business-urls.h"

#include "gnc-plugin-manager.h"
#include "gnc-plugin-business.h"

#include "gnc-hooks.h"
#include "dialog-invoice.h"
#include "dialog-preferences.h"

GNC_MODULE_API_DECL(libgncmod_business_gnome)

/* version of the gnc module system interface we require */
int libgncmod_business_gnome_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_business_gnome_gnc_module_current  = 0;
int libgncmod_business_gnome_gnc_module_revision = 0;
int libgncmod_business_gnome_gnc_module_age      = 0;


char *
libgncmod_business_gnome_gnc_module_path(void)
{
    return g_strdup("gnucash/business-gnome");
}

char *
libgncmod_business_gnome_gnc_module_description(void)
{
    return g_strdup("The GnuCash business module GNOME UI");
}

int
libgncmod_business_gnome_gnc_module_init(int refcount)
{
    /* load business-core: we depend on it -- and it depends on the engine */
    if (!gnc_module_load ("gnucash/business-core", 0))
    {
        return FALSE;
    }
    /* We also depend on app-utils, gnome-utils, and gnome-search modules */
    if (!gnc_module_load ("gnucash/app-utils", 0))
    {
        return FALSE;
    }
    if (!gnc_module_load ("gnucash/gnome-utils", 0))
    {
        return FALSE;
    }
    if (!gnc_module_load ("gnucash/gnome-search", 0))
    {
        return FALSE;
    }
    if (!gnc_module_load ("gnucash/report/report-gnome", 0))
    {
        return FALSE;
    }
    //  if (!libgncmod_business_gnome_gnc_module_load ("gnucash/report/standard-reports", 0)) {
    //    return FALSE;
    //  }

    scm_c_eval_string("(use-modules (gnucash business-gnome))");
    scm_c_eval_string("(use-modules (gnucash report business-reports))");

    // temp code until gnc:url-type is wrapped
    /*
    {
        SCM wct_gnc_url_type = scm_c_eval_string("<gnc:url-type>");
        SCM tmp;

        tmp = gw_wcp_assimilate_ptr(GNC_CUSTOMER_MODULE_NAME, wct_gnc_url_type);
        scm_c_define("gnc:url-type-customer", tmp);
        tmp = gw_wcp_assimilate_ptr(GNC_VENDOR_MODULE_NAME, wct_gnc_url_type);
        scm_c_define("gnc:url-type-vendor", tmp);
        tmp = gw_wcp_assimilate_ptr(GNC_EMPLOYEE_MODULE_NAME, wct_gnc_url_type);
        scm_c_define("gnc:url-type-employee", tmp);
        tmp = gw_wcp_assimilate_ptr(GNC_INVOICE_MODULE_NAME, wct_gnc_url_type);
        scm_c_define("gnc:url-type-invoice", tmp);
        tmp = gw_wcp_assimilate_ptr(URL_TYPE_OWNERREPORT, wct_gnc_url_type);
        scm_c_define("gnc:url-type-ownerreport", tmp);
    }
    */

    if (refcount == 0)
    {
        /* Register the Owner search type */
        gnc_search_core_register_type (GNC_OWNER_MODULE_NAME,
                                       (GNCSearchCoreNew) gnc_search_owner_new);
        gnc_business_urls_initialize ();
        gnc_business_options_gnome_initialize ();

        gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (),
                                       gnc_plugin_business_new ());

        gnc_hook_add_dangler(HOOK_BOOK_OPENED,
                             (GFunc)gnc_invoice_remind_bills_due_cb, NULL);

        gnc_preferences_add_page("businessprefs.glade", "business_prefs",
                                 _("Business"));
    }

    return TRUE;
}

int
libgncmod_business_gnome_gnc_module_end(int refcount)
{
    return TRUE;
}
