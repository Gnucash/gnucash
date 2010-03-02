/*
 * gnc-plugin-page-invoice.h --
 *
 * Copyright (C) 2005,2006 David Hampton <hampton@employees.org>
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
 */

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup InvoicePlugin Business Invoice Page
    @{ */
/** @file gnc-plugin-page-invoice.h
    @brief  utility functions for the GnuCash UI
    @author Copyright (C) 2005,2006 David Hampton <hampton@employees.org>
*/

#ifndef __GNC_PLUGIN_PAGE_INVOICE_H
#define __GNC_PLUGIN_PAGE_INVOICE_H

#include <gtk/gtkwindow.h>

#include "Account.h"
#include "gnc-ledger-display.h"
#include "gnc-plugin-page.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_PAGE_INVOICE            (gnc_plugin_page_invoice_get_type ())
#define GNC_PLUGIN_PAGE_INVOICE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_PAGE_INVOICE, GncPluginPageInvoice))
#define GNC_PLUGIN_PAGE_INVOICE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_PAGE_INVOICE, GncPluginPageInvoiceClass))
#define GNC_IS_PLUGIN_PAGE_INVOICE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_PAGE_INVOICE))
#define GNC_IS_PLUGIN_PAGE_INVOICE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_PAGE_INVOICE))
#define GNC_PLUGIN_PAGE_INVOICE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_PAGE_INVOICE, GncPluginPageInvoiceClass))

#define GNC_PLUGIN_PAGE_INVOICE_NAME "GncPluginPageInvoice"

/* typedefs & structures */
typedef struct
{
    GncPluginPage gnc_plugin_page;
} GncPluginPageInvoice;

typedef struct
{
    GncPluginPageClass gnc_plugin_page;
} GncPluginPageInvoiceClass;

/* function prototypes */

/** Retrieve the type number for an "invoice" plugin page.
 *
 *  @return The type number.
 */
GType gnc_plugin_page_invoice_get_type (void);


/** Create a new "invoice" plugin page, given a pointer to an
 *  InvoiceWindow data structure.  This structure is used to describe
 *  both the "invoice entry" page in a window, and also to describe
 *  the New Invoice dialog.
 *
 *  @param iw A pointer to the invoice to be embedded into a main window.
 *
 *  @return The newly created plugin page.
 */
GncPluginPage *gnc_plugin_page_invoice_new (InvoiceWindow *iw);


/** Update the menu items associated with this invoice page.  This
 *  function should be called whenever the posted state of an invoice
 *  is changed.
 *
 *  @param page A pointer invoice page.
 *
 *  @param is_posted Set this to TRUE if the invoice has been posted.
 *
 *  @param can_unpost Set this to TRUE if the invoice can be un-posted.
 */
void gnc_plugin_page_invoice_update_menus (GncPluginPage *page, gboolean is_posted, gboolean can_unpost);


/** Update the title associated with this invoice page.  This function
 *  should be called whenever the name on an invoice is changed.
 *
 *  @param page A pointer invoice page.
 */
void gnc_plugin_page_invoice_update_title (GncPluginPage *page);

G_END_DECLS
/** @} */
/** @} */

#endif /* __GNC_PLUGIN_PAGE_INVOICE_H */

