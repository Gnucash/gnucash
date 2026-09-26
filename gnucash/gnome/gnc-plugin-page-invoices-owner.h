/*
 * plugin-page-invoices-owner.h -- Page for Invoices Overview 
 * Copyright (C) 2026 Roy Hansen 
 * Author: Roy Hansen <roy@royhansen.no>
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

#ifndef GNC_PLUGIN_PAGE_INVOICES_OWNER_H_
#define GNC_PLUGIN_PAGE_INVOICES_OWNER_H_

#include "gnc-plugin-page.h"

G_BEGIN_DECLS

#define GNC_TYPE_PLUGIN_PAGE_INVOICES_OWNER    (gnc_plugin_page_invoices_owner_get_type ())

#define GNC_PLUGIN_PAGE_INVOICES_OWNER(obj)    (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_PAGE_INVOICES_OWNER, GncPluginPageInvoicesOwner))

#define GNC_IS_PLUGIN_PAGE_INVOICES_OWNER(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_PAGE_INVOICES_OWNER))

struct GncPluginPageInvoicesOwner {
    GncPluginPage gnc_plugin_page;
};

struct GncPluginPageInvoicesOwnerClass {
    GncPluginPageClass gnc_plugin_page;
};

GType gnc_plugin_page_invoices_owner_get_type ();

/**
 * Create a new Invoices/Bill/Vouchers page.
 * If a page of the requested type is already open, use that instead
 * of creating a new one.
 *
 * @param owner_type The owner type (customer/vendor/employee).
 * @return Returns a plugin page.
 */
GncPluginPage *gnc_plugin_page_invoices_owner_new (GncOwnerType owner_type);

G_END_DECLS

#endif /* GNC_PLUGIN_PAGE_INVOICES_OWNER_H_ */
