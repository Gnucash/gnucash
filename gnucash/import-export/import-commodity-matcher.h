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

#include <gtk/gtk.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "gnc-commodity.h"

typedef void (*GncImportCommodityCallback) (gnc_commodity *commodity,
                                             gboolean accepted,
                                             gpointer user_data);

/** Find an existing commodity by its exchange identifier without prompting. */
gnc_commodity *gnc_import_find_commodity_by_cusip (const char *cusip);

/** Resolve or select a commodity without entering a nested GTK loop.
 *  The callback's commodity is borrowed and is NULL on cancellation. */
void gnc_import_select_commodity_async (GtkWidget *parent,
                                        const char *cusip,
                                        gboolean ask_on_unknown,
                                        const char *default_fullname,
                                        const char *default_mnemonic,
                                        GncImportCommodityCallback callback,
                                        gpointer user_data);


#ifdef __cplusplus
}
#endif

#endif
/**@}*/
