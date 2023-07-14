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
\********************************************************************/
/** @addtogroup Import_Export
    @{ */
/**@internal
 @file import-commodity-matcher.c
  @brief  A Generic commodity matcher/picker
  @author Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>
 */
#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <stdlib.h>
#include <math.h>

#include "import-commodity-matcher.h"
#include "Account.h"
#include "Transaction.h"
#include "dialog-commodity.h"
#include "gnc-engine.h"
#include "gnc-ui-util.h"

/********************************************************************\
 *   Constants   *
\********************************************************************/


/********************************************************************\
 *   Constants, should ideally be defined a user preference dialog    *
\********************************************************************/

static QofLogModule log_module = GNC_MOD_IMPORT;



gnc_commodity * gnc_import_select_commodity(const char * cusip,
        gboolean ask_on_unknown,
        const char * default_fullname,
        const char * default_mnemonic)
{
    const gnc_commodity_table * commodity_table = gnc_get_current_commodities ();
    gnc_commodity * retval = NULL;
    DEBUG("Default fullname received: %s", default_fullname);
    DEBUG("Default mnemonic received: %s", default_mnemonic);

    g_return_val_if_fail(cusip, NULL);
    DEBUG("Looking for commodity with exchange_code: %s", cusip);

    g_assert(commodity_table);
    GList *namespace_list = gnc_commodity_table_get_namespaces(commodity_table);

    for (GList *n = namespace_list; !retval && n; n = g_list_next (n))
    {
        auto ns = static_cast<const char*>(n->data);
        DEBUG("Looking at namespace %s", ns);
        GList *comm_list = gnc_commodity_table_get_commodities (commodity_table, ns);
        for (GList *m = comm_list; !retval && m; m = g_list_next (m))
        {
            auto com = static_cast<gnc_commodity*>(m->data);
            DEBUG("Looking at commodity %s", gnc_commodity_get_fullname (com));
            if (!g_strcmp0 (gnc_commodity_get_cusip (com), cusip))
            {
                retval = com;
                DEBUG("Commodity %s matches.", gnc_commodity_get_fullname (com));
            }
        }
        g_list_free (comm_list);
    }

    g_list_free(namespace_list);

    if (retval == NULL && ask_on_unknown != 0)
    {
        const gchar *message =
            _("Please select a commodity to match the following exchange "
              "specific code. Please note that the exchange code of the "
              "commodity you select will be overwritten.");
        retval = gnc_ui_select_commodity_modal_full(NULL,
                 NULL,
                 DIAG_COMM_ALL,
                 message,
                 cusip,
                 default_fullname,
                 default_mnemonic);

    }
    /* There seems to be a problem here - if the matched commodity does not
       have a cusip defined (gnc_commodity_get_cusip returns NULL) then
       it does not get overwritten - which is not consistent with the
       message - so Im adding it to do this.  Looks like this is all
       that was needed to fix the cash value used as stock units problem
       for pre-defined commodities which didn't have the cusip defined! */
    if (retval != NULL &&
            gnc_commodity_get_cusip(retval) != NULL &&
            cusip != NULL &&
            (strncmp(gnc_commodity_get_cusip(retval), cusip, strlen(cusip)) != 0))
    {
        gnc_commodity_set_cusip(retval, cusip);
    }
    else if (gnc_commodity_get_cusip(retval) == NULL && cusip != NULL)
    {
        gnc_commodity_set_cusip(retval, cusip);
    }
    return retval;
}
/**@}*/
