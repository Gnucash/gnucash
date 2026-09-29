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

typedef struct
{
    gchar *cusip;
    QofBook *book;
    GncImportCommodityCallback callback;
    gpointer user_data;
} CommoditySelection;

gnc_commodity *
gnc_import_find_commodity_by_cusip (const char *cusip)
{
    if (!cusip)
        return nullptr;
    const auto table = gnc_get_current_commodities ();
    if (!table)
        return nullptr;
    gnc_commodity *result = nullptr;
    auto namespaces = gnc_commodity_table_get_namespaces (table);
    for (auto n = namespaces; !result && n; n = n->next)
    {
        auto commodities = gnc_commodity_table_get_commodities (
            table, static_cast<const char *>(n->data));
        for (auto c = commodities; !result && c; c = c->next)
        {
            auto commodity = static_cast<gnc_commodity *>(c->data);
            if (!g_strcmp0 (gnc_commodity_get_cusip (commodity), cusip))
                result = commodity;
        }
        g_list_free (commodities);
    }
    g_list_free (namespaces);
    return result;
}

static void
commodity_selection_finished (QofBook *book, gnc_commodity *commodity,
                              gpointer user_data)
{
    auto selection = static_cast<CommoditySelection *>(user_data);
    gboolean accepted = book && commodity && book == selection->book &&
        gnc_get_current_book () == selection->book &&
        qof_book_is_open (selection->book);
    if (accepted && selection->cusip)
        gnc_commodity_set_cusip (commodity, selection->cusip);
    selection->callback (accepted ? commodity : nullptr, accepted,
                         selection->user_data);
    g_free (selection->cusip);
    g_object_unref (selection->book);
    g_free (selection);
}

void
gnc_import_select_commodity_async (GtkWidget *parent, const char *cusip,
                                   gboolean ask_on_unknown,
                                   const char *default_fullname,
                                   const char *default_mnemonic,
                                   GncImportCommodityCallback callback,
                                   gpointer user_data)
{
    g_return_if_fail (callback != nullptr);
    auto commodity = gnc_import_find_commodity_by_cusip (cusip);
    if (commodity || !ask_on_unknown)
    {
        callback (commodity, commodity != nullptr, user_data);
        return;
    }

    static const gchar *message =
        N_("Please select a commodity to match the following exchange "
           "specific code. Please note that the exchange code of the "
           "commodity you select will be overwritten.");
    auto selection = g_new0 (CommoditySelection, 1);
    selection->cusip = g_strdup (cusip);
    selection->book = gnc_get_current_book ();
    if (!selection->book)
    {
        g_free (selection->cusip);
        g_free (selection);
        callback (nullptr, FALSE, user_data);
        return;
    }
    g_object_ref (selection->book);
    selection->callback = callback;
    selection->user_data = user_data;
    gnc_ui_select_commodity_async_full (nullptr, parent, DIAG_COMM_ALL,
                                        _(message), cusip, default_fullname,
                                        default_mnemonic,
                                        commodity_selection_finished,
                                        selection);
}



/**@}*/
