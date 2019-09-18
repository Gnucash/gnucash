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


#define GNC_PREFS_GROUP_INVOICE "dialogs.business.invoice"
#define GNC_PREFS_GROUP_BILL    "dialogs.business.bill"
#define GNC_PREFS_GROUP_VOUCHER "dialogs.business.voucher"

#define GNC_PREF_AUTO_PAY "auto-pay"


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

/* Fill in a combo box with the appropriate list of accounts
 * Returns the default selected account */
Account * gnc_account_select_combo_fill (GtkWidget *combo, QofBook *book,
        GList *acct_types,
        GList *acct_commodities);

/* Returns the currently selected account in the combo box*/
Account * gnc_account_select_combo_get_active (GtkWidget *combo);

/* Create a combo box of available billing terms based on
 * the combo box If none_ok is true, then add "none" as a
 * choice (with data set to NULL).  If initial_choice is non-NULL,
 * then that will be the default option setting when the menu is
 * created.
 *
 * Note: if you are interested in the currently active combo box
 * item, you can use the function gnc_simple_combo_get_value below.
 * This can be used for example in a callback function that triggers
 * on the combo box' "changed" signal"
 */
void gnc_billterms_combo (GtkComboBox *cbox, QofBook *book,
                          gboolean none_ok, GncBillTerm *initial_choice);

/* Same thing except for the tax tables */
void
gnc_taxtables_combo (GtkComboBox *cbox, QofBook *book,
                     gboolean none_ok, GncTaxTable *initial_choice);

/* Build an option menu for choosing a GncTaxIncluded */
void gnc_taxincluded_combo (GtkComboBox *cbox, GncTaxIncluded initial_choice);


/* Here are some "simple combo box" utilities that can be used with
 * ANY of the above combo box types.  In particular the following
 * functions are useful for hooking the above combo boxes into the
 * GNC Option infrastructure.
 */

/** Get the value of the item that is currently selected in the combo box */
gpointer gnc_simple_combo_get_value (GtkComboBox *cbox);

/** Find the item in the combo box whose value is "data"
 *  and make it the active item. */
void gnc_simple_combo_set_value (GtkComboBox *cbox, gpointer data);


#endif /* GNC_BUSINESS_GNOME_UTILS_H_ */
