/*
 * dialog-customer.h -- Dialog(s) for Customer search and entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
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


#ifndef GNC_DIALOG_CUSTOMER_H_
#define GNC_DIALOG_CUSTOMER_H_

typedef struct _customer_window CustomerWindow;

#include "gncCustomer.h"
#include "dialog-search.h"

/* Functions to create and edit a customer */
CustomerWindow * gnc_ui_customer_edit (GtkWindow *parent, GncCustomer *cust);
CustomerWindow * gnc_ui_customer_new (GtkWindow *parent, QofBook *book);

/* Search for customers */
GNCSearchWindow *gnc_customer_search (GtkWindow *parent, GncCustomer *start, QofBook *book);

/*
 * These callbacks are for use with the gnc_general_search widget
 *
 * select() provides a Select Dialog and returns it.
 * edit() opens the existing customer for editing and returns NULL.
 */
GNCSearchWindow * gnc_customer_search_select (GtkWindow *parent, gpointer start, gpointer book);
GNCSearchWindow * gnc_customer_search_edit (GtkWindow *parent, gpointer start, gpointer book);

#endif /* GNC_DIALOG_CUSTOMER_H_ */
