/*
 * business-utils.h -- General GUI Utilities for GNC Business Objects
 *
 * Written By: Derek Atkins <warlord@MIT.EDU>
 * Copyright (C) 2001
 */

#ifndef GNC_BUSINESS_UTILS_H_
#define GNC_BUSINESS_UTILS_H_

#include "gnc-general-select.h"
#include "gnc-book.h"
#include "gncOwner.h"

GtkWidget * gnc_owner_select_create (GtkWidget *label, GtkWidget *hbox,
				     GNCBook *book, GncOwner *owner);

GtkWidget * gnc_owner_edit_create (GtkWidget *label, GtkWidget *hbox,
				   GNCBook *book, GncOwner *owner);

void gnc_owner_get_owner (GtkWidget *widget, GncOwner *owner);

#endif /* GNC_BUSINESS_UTILS_H_ */
