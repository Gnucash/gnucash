/********************************************************************
 * dialog-commodity.h -- "select" and "new" commodity windows       *
 *                       (GnuCash)                                  *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
 *                                                                  *
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
 ********************************************************************/

/** @addtogroup GUI
    @{ */
/** @addtogroup GuiCommodity Commodity windows
    @{ */
/** @file dialog-commodity.h
    @brief "select" and "new" commodity windows
    @author Copyright (C) 2000 Bill Gribble <grib@billgribble.com>
*/

#ifndef GNC_DIALOG_COMMODITY_H
#define GNC_DIALOG_COMMODITY_H

#include <gtk/gtk.h>
#include "gnc-commodity.h"
#include "gnc-session.h"

#ifdef __cplusplus
extern "C"
{
#endif

/** The dialog commodity types are used to determine what commodity
 *  namespaces the currency dialog will present to a user.  These
 *  values can be safely changed from one release to the next.  Note
 *  that if values are added, the routines in dialog-commodity.c will
 *  need to be updated to match.
 */
typedef enum
{
    DIAG_COMM_CURRENCY,            /**< Dialog box should only allow selection
                                       of a currency. */
    DIAG_COMM_NON_CURRENCY,        /**< Dialog box should allow selection of
                                        anything but a currency. */
    DIAG_COMM_NON_CURRENCY_SELECT, /**< Dialog box should allow selection of
                                    * anything but a currency and should include
                                    * the "ALL" namespace to display all such
                                    * commodities in a single list. */
    DIAG_COMM_ALL,                /**< Dialog box should allow selection of
                                       anything. */
} dialog_commodity_mode;

typedef void (*GncCommoditySelectionCallback) (gnc_commodity *commodity,
                                                gpointer user_data);

/** @name Commodity Selection */
/** @{ */

/** Launch commodity selection without running a nested event loop. The
 * completion callback receives NULL when the user cancels or the supplied
 * cancellable is cancelled. */
void gnc_ui_select_commodity_async_full (gnc_commodity *orig_sel,
                                         GtkWidget *parent,
                                         dialog_commodity_mode mode,
                                         const char *user_message,
                                         const char *cusip,
                                         const char *fullname,
                                         const char *mnemonic,
                                         GCancellable *cancellable,
                                         GncCommoditySelectionCallback callback,
                                         gpointer user_data);
void gnc_ui_select_commodity_async_full_with_operation_context (
    gnc_commodity *orig_sel,
    GtkWidget *parent,
    dialog_commodity_mode mode,
    const char *user_message,
    const char *cusip,
    const char *fullname,
    const char *mnemonic,
    GCancellable *cancellable,
    GncSessionOperationContext *operation_context,
    GncCommoditySelectionCallback callback,
    gpointer user_data);
void gnc_ui_select_commodity_async (gnc_commodity *orig_sel,
                                    GtkWidget *parent,
                                    dialog_commodity_mode mode,
                                    GCancellable *cancellable,
                                    GncCommoditySelectionCallback callback,
                                    gpointer user_data);

/** @} */

/** @name Commodity Creation or Modification */
/** @{ */

/** Launch commodity creation or editing without running a nested event loop.
 * The callback receives NULL when the operation is cancelled. Validation
 * errors keep the window open. */
void gnc_ui_new_commodity_async_full (const char *name_space,
                                      GtkWidget *parent,
                                      const char *cusip,
                                      const char *fullname,
                                      const char *mnemonic,
                                      const char *user_symbol,
                                      int fraction,
                                      GCancellable *cancellable,
                                      GncCommoditySelectionCallback callback,
                                      gpointer user_data);
void gnc_ui_new_commodity_async_full_with_operation_context (
    const char *name_space,
    GtkWidget *parent,
    const char *cusip,
    const char *fullname,
    const char *mnemonic,
    const char *user_symbol,
    int fraction,
    GCancellable *cancellable,
    GncSessionOperationContext *operation_context,
    GncCommoditySelectionCallback callback,
    gpointer user_data);
void gnc_ui_new_commodity_async (const char *default_namespace,
                                 GtkWidget *parent,
                                 GCancellable *cancellable,
                                 GncCommoditySelectionCallback callback,
                                 gpointer user_data);
void gnc_ui_edit_commodity_async (gnc_commodity *commodity,
                                  GtkWidget *parent,
                                  GCancellable *cancellable,
                                  GncCommoditySelectionCallback callback,
                                  gpointer user_data);

/** @} */
/** @name Auxiliary Dialog Functions */
/** @{ */

/** Create an editable GTK4 picker backed by a string model. The picker
 *  combines a text entry with a drop-down list, so callers can retain the
 *  existing namespace-entry workflow without depending on a deprecated
 *  composite selector.
 */
GtkWidget *gnc_ui_commodity_picker_new (void);

/** Initialise a picker that was declared as a GtkBox in a builder file. */
void gnc_ui_commodity_picker_setup (GtkWidget *picker);

/** Return the editable part of a commodity picker. */
GtkEntry *gnc_ui_commodity_picker_get_entry (GtkWidget *picker);

/** Given a commodity picker, fill in the known commodity namespaces and then
 *  select one.
 *
 *  @param picker The widget to populate with information.
 *
 *  @param sel The namespace that should be initially selected when
 *  the combo box appears.
 *
 *  @param mode Determines in which namespaces the user may select a
 *  commodity
 */
void gnc_ui_update_namespace_picker (GtkWidget *picker,
                                     const gchar *sel,
                                     dialog_commodity_mode mode);

/** Given a commodity picker, return the currently selected namespace.
 *
 *  @param picker The picker of namespaces.
 *
 *  @return The currently selected namespace.
 *
 *  @note This string must be freed by with g_free.
 */
gchar *gnc_ui_namespace_picker_ns (GtkWidget *picker);

/** Given a commodity picker, fill in all the known commodities for the
 *  specified namespace, and then select one.
 *
 *  @param picker The widget to populate with information.
 *
 *  @param namespace All commodities with this namespace will be added
 *  to the combo box.
 *
 *  @param sel The commodity that should be initially selected when
 *  the combo box appears.
 */
void gnc_ui_update_commodity_picker (GtkWidget *picker,
                                     const gchar *name_space,
                                     const gchar *sel);
/** @} */

#ifdef __cplusplus
}
#endif

#endif
/** @} */
/** @} */
