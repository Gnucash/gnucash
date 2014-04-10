/*
 * dialog-order.h -- Dialog(s) for Order search and entry
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */


#ifndef GNC_DIALOG_ORDER_H_
#define GNC_DIALOG_ORDER_H_

typedef struct _order_window OrderWindow;

#include "gncOrder.h"
#include "gncOwner.h"
#include "dialog-search.h"

/* Create and edit an order */
OrderWindow * gnc_ui_order_edit (GncOrder *order);
OrderWindow * gnc_ui_order_new (GncOwner *owner, GNCBook *book);

/* Search for orders */
GNCSearchWindow * gnc_order_search (GncOrder *start, GncOwner *owner,
				    GNCBook *book);

/*
 * These callbacks are for use with the gnc_general_search widget
 *
 * select() provides a Select Dialog and returns it.
 * edit() opens the existing order for editing and returns NULL.
 */
GNCSearchWindow * gnc_order_search_select (gpointer start, gpointer book);
GNCSearchWindow * gnc_order_search_edit (gpointer start, gpointer book);

#endif /* GNC_DIALOG_ORDER_H_ */
