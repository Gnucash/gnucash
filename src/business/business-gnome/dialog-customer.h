/*
 * dialog-customer.h -- Dialog(s) for Customer search and entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */


#ifndef GNC_DIALOG_CUSTOMER_H_
#define GNC_DIALOG_CUSTOMER_H_

typedef struct _customer_window CustomerWindow;

#include "gncCustomer.h"
#include "dialog-search.h"

/* Functions to create and edit a customer */
CustomerWindow * gnc_ui_customer_edit (GncCustomer *cust);
CustomerWindow * gnc_ui_customer_new (GNCBook *book);

/* Search for customers */
GNCSearchWindow *gnc_customer_search (GncCustomer *start, GNCBook *book);

/*
 * These callbacks are for use with the gnc_general_search widget
 *
 * select() provides a Select Dialog and returns it.
 * edit() opens the existing customer for editing and returns NULL.
 */
GNCSearchWindow * gnc_customer_search_select (gpointer start, gpointer book);
GNCSearchWindow * gnc_customer_search_edit (gpointer start, gpointer book);

#endif /* GNC_DIALOG_CUSTOMER_H_ */
