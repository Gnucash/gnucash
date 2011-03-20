/*
 * business-gnome-utils.h -- General GUI Utilities for GNC Business Objects
 *
 * Written By: Derek Atkins <warlord@MIT.EDU>
 * Copyright (C) 2001,2006 Derek Atkins
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

#ifndef GNC_BUSINESS_GNOME_UTILS_H_
#define GNC_BUSINESS_GNOME_UTILS_H_

#include "gncOwner.h"
#include "gncBillTerm.h"
#include "gncTaxTable.h"
#include "gncInvoice.h"

#define GCONF_SECTION_ORDER    "dialogs/business/order"
#define GCONF_SECTION_JOB      "dialogs/business/job"
#define GCONF_SECTION_CUSTOMER "dialogs/business/customer"
#define GCONF_SECTION_VENDOR   "dialogs/business/vendor"
#define GCONF_SECTION_EMPLOYEE "dialogs/business/employee"

GtkWidget * gnc_owner_select_create (GtkWidget *label, GtkWidget *hbox,
                                     QofBook *book, GncOwner *owner);

GtkWidget * gnc_owner_edit_create (GtkWidget *label, GtkWidget *hbox,
                                   QofBook *book, GncOwner *owner);

void gnc_owner_get_owner (GtkWidget *widget, GncOwner *owner);
void gnc_owner_set_owner (GtkWidget *widget, GncOwner *owner);


/* An invoice select widget..
 * the owner, invoice, and label parameters are optional
 */
GtkWidget * gnc_invoice_select_create (GtkWidget *hbox, QofBook *book,
                                       const GncOwner *owner,
                                       GncInvoice *invoice,
                                       GtkWidget *label);

GncInvoice * gnc_invoice_get_invoice (GtkWidget *widget);
void gnc_invoice_set_invoice (GtkWidget *widget, GncInvoice *invoice);
void gnc_invoice_set_owner (GtkWidget *widget, GncOwner *owner);

/* Return a list of account-types based on the owner type */
GList * gnc_business_account_types (GncOwner *owner);

/* Return a list of currencies associated with the owner */
GList * gnc_business_commodities (GncOwner *owner);

/* Fill in a combo box with the appropriate list of accounts */
void gnc_fill_account_select_combo (GtkWidget *combo, QofBook *book,
                                    GList *acct_types,
                                    GList *acct_commodities);


/* Create an optionmenu of available billing terms and attach it to
 * the menu passed in.  If none_ok is true, then add "none" as a
 * choice (with data set to NULL).  Any time the menu changes,
 * 'choice' will be set to the chosen option.  If *choice is non-NULL,
 * then that will be the default option setting when the menu is
 * created.
 */
void gnc_ui_billterms_optionmenu (GtkWidget *omenu, QofBook *book,
                                  gboolean none_ok, GncBillTerm **choice);

/* Same thing except for the tax tables */
void
gnc_ui_taxtables_optionmenu (GtkWidget *omenu, QofBook *book,
                             gboolean none_ok, GncTaxTable **choice);

/* Build an option menu for choosing a GncTaxIncluded */
void gnc_ui_taxincluded_optionmenu (GtkWidget *omenu, GncTaxIncluded *choice);


/* Here are some "generic option menu" utilities that can be used with
 * ANY of the above option-menu types.  In particular the following
 * functions are useful for hooking the above option menus into the
 * GNC Option infrastructure.
 */

void gnc_ui_optionmenu_set_changed_callback (GtkWidget *omenu,
        void (*changed_cb)(GtkWidget*, gpointer),
        gpointer cb_arg);
gpointer gnc_ui_optionmenu_get_value (GtkWidget *omenu);
void gnc_ui_optionmenu_set_value (GtkWidget *omenu, gpointer data);


#endif /* GNC_BUSINESS_GNOME_UTILS_H_ */
