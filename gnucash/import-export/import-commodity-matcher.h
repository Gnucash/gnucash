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
/** @file import-commodity-matcher.h
  @brief A Generic commodity matcher/picker
  @author Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>
 */
#ifndef IMPORT_COMMODITY_MATCHER_H
#define IMPORT_COMMODITY_MATCHER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "gnc-commodity.h"
#include "gnc-session.h"
#include <gtk/gtk.h>

/** Find a commodity by CUSIP without showing a user interface or changing
 * any model data. The returned pointer belongs to the current book. */
gnc_commodity *gnc_import_find_commodity_by_cusip (const char *cusip);


typedef void (*GncImportCommoditySelectedCB) (gnc_commodity *commodity,
                                              gboolean accepted,
                                              gpointer user_data);

/** Select a commodity without entering a nested main loop. The callback
 * receives NULL and FALSE when no commodity is available or the user cancels. */
void gnc_import_select_commodity_async (GtkWidget *parent,
                                        const char *cusip,
                                        gboolean ask_on_unknown,
                                        const char *default_fullname,
                                        const char *default_mnemonic,
                                        GCancellable *cancellable,
                                        GncImportCommoditySelectedCB callback,
                                        gpointer user_data);

/** Context-aware variant for asynchronous imports. The context carries only
 * identity between main-loop turns; every mutation acquires a fresh short
 * operation section. */
void gnc_import_select_commodity_async_with_operation_context (
    GtkWidget *parent, const char *cusip, gboolean ask_on_unknown,
    const char *default_fullname, const char *default_mnemonic,
    GCancellable *cancellable,
    GncSessionOperationContext *operation_context,
    GncImportCommoditySelectedCB callback,
    gpointer user_data);


#ifdef __cplusplus
}
#endif

#endif
/**@}*/
