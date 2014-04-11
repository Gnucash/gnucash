/*
 * dialog-customer.c -- Dialog for Customer entry
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-currency-edit.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "qof.h"

#include "dialog-search.h"
#include "search-param.h"

#include "gncAddress.h"
#include "gncCustomer.h"
#include "gncCustomerP.h"

#include "business-gnome-utils.h"
#include "dialog-customer.h"
#include "dialog-job.h"
#include "dialog-order.h"
#include "dialog-invoice.h"
#include "dialog-payment.h"

#define DIALOG_NEW_CUSTOMER_CM_CLASS "dialog-new-customer"
#define DIALOG_EDIT_CUSTOMER_CM_CLASS "dialog-edit-customer"

#define GCONF_SECTION_SEARCH "dialogs/business/customer_search"

void gnc_customer_taxtable_check_cb (GtkToggleButton *togglebutton,
				     gpointer user_data);

void gnc_customer_window_ok_cb (GtkWidget *widget, gpointer data);
void gnc_customer_window_cancel_cb (GtkWidget *widget, gpointer data);
void gnc_customer_window_help_cb (GtkWidget *widget, gpointer data);
void gnc_customer_window_destroy_cb (GtkWidget *widget, gpointer data);
void gnc_customer_name_changed_cb (GtkWidget *widget, gpointer data);

typedef enum
{
  NEW_CUSTOMER,
  EDIT_CUSTOMER
} CustomerDialogType;

struct _customer_select_window {
  GNCBook *	book;
  QueryNew *	q;
};

struct _customer_window {
  GtkWidget *	dialog;

  GtkWidget *	id_entry;
  GtkWidget *	company_entry;

  GtkWidget *	name_entry;
  GtkWidget *	addr1_entry;
  GtkWidget *	addr2_entry;
  GtkWidget *	addr3_entry;
  GtkWidget *	addr4_entry;
  GtkWidget *	phone_entry;
  GtkWidget *	fax_entry;
  GtkWidget *	email_entry;

  GtkWidget *	shipname_entry;
  GtkWidget *	shipaddr1_entry;
  GtkWidget *	shipaddr2_entry;
  GtkWidget *	shipaddr3_entry;
  GtkWidget *	shipaddr4_entry;
  GtkWidget *	shipphone_entry;
  GtkWidget *	shipfax_entry;
  GtkWidget *	shipemail_entry;

  GtkWidget *	currency_edit;
  GtkWidget *	terms_menu;
  GtkWidget *	discount_amount;
  GtkWidget *	credit_amount;

  GtkWidget *	active_check;
  GtkWidget *	taxincluded_menu;
  GtkWidget *	notes_text;

  GtkWidget *	taxtable_check;
  GtkWidget *	taxtable_menu;

  GncTaxIncluded taxincluded;
  GncBillTerm *	terms;
  CustomerDialogType	dialog_type;
  GUID		customer_guid;
  gint		component_id;
  GNCBook *	book;
  GncCustomer *	created_customer;

  GncTaxTable *	taxtable;
};

void
gnc_customer_taxtable_check_cb (GtkToggleButton *togglebutton,
				gpointer user_data)
{
  CustomerWindow *cw = user_data;

  if (gtk_toggle_button_get_active (togglebutton))
    gtk_widget_set_sensitive (cw->taxtable_menu, TRUE);
  else
    gtk_widget_set_sensitive (cw->taxtable_menu, FALSE);
}

static GncCustomer *
cw_get_customer (CustomerWindow *cw)
{
  if (!cw)
    return NULL;

  return gncCustomerLookup (cw->book, &cw->customer_guid);
}

static void gnc_ui_to_customer (CustomerWindow *cw, GncCustomer *cust)
{
  GtkTextBuffer* text_buffer;
  GtkTextIter start, end;
  gchar *text;
  GncAddress *addr, *shipaddr;

  addr = gncCustomerGetAddr (cust);
  shipaddr = gncCustomerGetShipAddr (cust);

  gnc_suspend_gui_refresh ();

  gncCustomerBeginEdit (cust);

  gncCustomerSetID (cust, gtk_editable_get_chars
		    (GTK_EDITABLE (cw->id_entry), 0, -1));
  gncCustomerSetName (cust, gtk_editable_get_chars
		      (GTK_EDITABLE (cw->company_entry), 0, -1));

  gncAddressSetName (addr, gtk_editable_get_chars
		     (GTK_EDITABLE (cw->name_entry), 0, -1));
  gncAddressSetAddr1 (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (cw->addr1_entry), 0, -1));
  gncAddressSetAddr2 (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (cw->addr2_entry), 0, -1));
  gncAddressSetAddr3 (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (cw->addr3_entry), 0, -1));
  gncAddressSetAddr4 (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (cw->addr4_entry), 0, -1));
  gncAddressSetPhone (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (cw->phone_entry), 0, -1));
  gncAddressSetFax (addr, gtk_editable_get_chars
		    (GTK_EDITABLE (cw->fax_entry), 0, -1));
  gncAddressSetEmail (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (cw->email_entry), 0, -1));

  gncAddressSetName (shipaddr, gtk_editable_get_chars
		     (GTK_EDITABLE (cw->shipname_entry), 0, -1));
  gncAddressSetAddr1 (shipaddr, gtk_editable_get_chars
		      (GTK_EDITABLE (cw->shipaddr1_entry), 0, -1));
  gncAddressSetAddr2 (shipaddr, gtk_editable_get_chars
		      (GTK_EDITABLE (cw->shipaddr2_entry), 0, -1));
  gncAddressSetAddr3 (shipaddr, gtk_editable_get_chars
		      (GTK_EDITABLE (cw->shipaddr3_entry), 0, -1));
  gncAddressSetAddr4 (shipaddr, gtk_editable_get_chars
		      (GTK_EDITABLE (cw->shipaddr4_entry), 0, -1));
  gncAddressSetPhone (shipaddr, gtk_editable_get_chars
		      (GTK_EDITABLE (cw->shipphone_entry), 0, -1));
  gncAddressSetFax (shipaddr, gtk_editable_get_chars
		    (GTK_EDITABLE (cw->shipfax_entry), 0, -1));
  gncAddressSetEmail (shipaddr, gtk_editable_get_chars
		      (GTK_EDITABLE (cw->shipemail_entry), 0, -1));

  gncCustomerSetActive (cust, gtk_toggle_button_get_active
			(GTK_TOGGLE_BUTTON (cw->active_check)));
  gncCustomerSetTaxIncluded (cust, cw->taxincluded);

  text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(cw->notes_text));
  gtk_text_buffer_get_bounds (text_buffer, &start, &end);
  text = gtk_text_buffer_get_text (text_buffer, &start, &end, FALSE);
  gncCustomerSetNotes (cust, text);

  /* Parse and set the currency, terms, discount, and credit amounts */
  gncCustomerSetCurrency (cust,
			  gnc_currency_edit_get_currency (GNC_CURRENCY_EDIT
							     (cw->currency_edit)));
  gncCustomerSetTerms (cust, cw->terms);
  gncCustomerSetDiscount (cust, gnc_amount_edit_get_amount
			  (GNC_AMOUNT_EDIT (cw->discount_amount)));
  gncCustomerSetCredit (cust, gnc_amount_edit_get_amount
			(GNC_AMOUNT_EDIT (cw->credit_amount)));

  gncCustomerSetTaxTableOverride
    (cust, gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cw->taxtable_check)));
  gncCustomerSetTaxTable (cust, cw->taxtable);

  gncCustomerCommitEdit (cust);
  gnc_resume_gui_refresh ();
}

static gboolean check_edit_amount (GtkWidget *dialog, GtkWidget *amount,
				   gnc_numeric *min, gnc_numeric *max,
				   const char * error_message)
{
  if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (amount))) {
    if (error_message)
      gnc_error_dialog (dialog, "%s", error_message);
    return TRUE;
  }
  /* We've got a valid-looking number; check mix/max */
  if (min || max) {
    gnc_numeric val = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (amount));
    if ((min && gnc_numeric_compare (*min, val) > 0) ||
	(max && gnc_numeric_compare (val, *max) > 0)) {
      if (error_message)
	gnc_error_dialog (dialog, "%s", error_message);
      return TRUE;
    }
  }
  return FALSE;
}

static gboolean check_entry_nonempty (GtkWidget *dialog, GtkWidget *entry, 
				      const char * error_message)
{
  const char *res = gtk_entry_get_text (GTK_ENTRY (entry));
  if (safe_strcmp (res, "") == 0) {
    if (error_message)
      gnc_error_dialog (dialog, "%s", error_message);
    return TRUE;
  }
  return FALSE;
}

void
gnc_customer_window_ok_cb (GtkWidget *widget, gpointer data)
{
  CustomerWindow *cw = data;
  gnc_numeric min, max;
  gchar *string;

  /* Check for valid company name */
  if (check_entry_nonempty (cw->dialog, cw->company_entry,
		   _("You must enter a company name. "
		     "If this customer is an individual (and not a company) "
		     "you should set the \"company name\" and \"contact name\" "
		     "the same.")))
    return;

  /* Make sure we have an address */
  if (check_entry_nonempty (cw->dialog, cw->addr1_entry, NULL) &&
      check_entry_nonempty (cw->dialog, cw->addr2_entry, NULL) &&
      check_entry_nonempty (cw->dialog, cw->addr3_entry, NULL) &&
      check_entry_nonempty (cw->dialog, cw->addr4_entry, NULL)) {
    const char *msg = _("You must enter a billing address.");
    gnc_error_dialog (cw->dialog, "%s", msg);
    return;
  }

  /* Verify terms, discount, and credit are valid (or empty) */
  min = gnc_numeric_zero ();
  max = gnc_numeric_create (100, 1);

  if (check_edit_amount (cw->dialog, cw->discount_amount, &min, &max,
			 _("Discount percentage must be between 0-100 "
			   "or you must leave it blank.")))
    return;

  if (check_edit_amount (cw->dialog, cw->credit_amount, &min, NULL,
			 _("Credit must be a positive amount or "
			   "you must leave it blank.")))
    return;

  /* Set the customer id if one has not been chosen */
  if (safe_strcmp (gtk_entry_get_text (GTK_ENTRY (cw->id_entry)), "") == 0) {
    string = g_strdup_printf ("%.6" G_GINT64_FORMAT,
			      gncCustomerNextID (cw->book));
    gtk_entry_set_text (GTK_ENTRY (cw->id_entry), string);
    g_free(string);
  }

  /* Now save it off */
  {
    GncCustomer *customer = cw_get_customer (cw);
    if (customer) {
      gnc_ui_to_customer (cw, customer);
    }
    cw->created_customer = customer;
    cw->customer_guid = *xaccGUIDNULL ();
  }

  gnc_close_gui_component (cw->component_id);
}

void
gnc_customer_window_cancel_cb (GtkWidget *widget, gpointer data)
{
  CustomerWindow *cw = data;

  gnc_close_gui_component (cw->component_id);
}

void
gnc_customer_window_help_cb (GtkWidget *widget, gpointer data)
{
  gnc_gnome_help(HF_HELP, HL_USAGE);
}

void
gnc_customer_window_destroy_cb (GtkWidget *widget, gpointer data)
{
  CustomerWindow *cw = data;
  GncCustomer *customer = cw_get_customer (cw);

  gnc_suspend_gui_refresh ();

  if (cw->dialog_type == NEW_CUSTOMER && customer != NULL) {
    gncCustomerBeginEdit (customer);
    gncCustomerDestroy (customer);
    cw->customer_guid = *xaccGUIDNULL ();
  }

  gnc_unregister_gui_component (cw->component_id);
  gnc_resume_gui_refresh ();

  g_free (cw);
}

void
gnc_customer_name_changed_cb (GtkWidget *widget, gpointer data)
{
  CustomerWindow *cw = data;
  char *fullname, *title;
  const char *id,  *name;

  if (!cw)
    return;

  name = gtk_entry_get_text (GTK_ENTRY (cw->company_entry));
  if (!name || *name == '\0')
    name = _("<No name>");

  id = gtk_entry_get_text (GTK_ENTRY (cw->id_entry));

  fullname = g_strconcat (name, " (", id, ")", (char *)NULL);

  if (cw->dialog_type == EDIT_CUSTOMER)
    title = g_strconcat (_("Edit Customer"), " - ", fullname, (char *)NULL);
  else
    title = g_strconcat (_("New Customer"), " - ", fullname, (char *)NULL);

  gtk_window_set_title (GTK_WINDOW (cw->dialog), title);

  g_free (fullname);
  g_free (title);
}

static void
gnc_customer_window_close_handler (gpointer user_data)
{
  CustomerWindow *cw = user_data;

  gtk_widget_destroy (cw->dialog);
  // cw has already been freed by this point.
  // cw->dialog = NULL;
}

static void
gnc_customer_window_refresh_handler (GHashTable *changes, gpointer user_data)
{
  CustomerWindow *cw = user_data;
  const EventInfo *info;
  GncCustomer *customer = cw_get_customer (cw);

  /* If there isn't a customer behind us, close down */
  if (!customer) {
    gnc_close_gui_component (cw->component_id);
    return;
  }

  /* Next, close if this is a destroy event */
  if (changes) {
    info = gnc_gui_get_entity_events (changes, &cw->customer_guid);
    if (info && (info->event_mask & QOF_EVENT_DESTROY)) {
      gnc_close_gui_component (cw->component_id);
      return;
    }
  }
}

static gboolean
find_handler (gpointer find_data, gpointer user_data)
{
  const GUID *customer_guid = find_data;
  CustomerWindow *cw = user_data;

  return(cw && guid_equal(&cw->customer_guid, customer_guid));
}

static CustomerWindow *
gnc_customer_new_window (GNCBook *bookp, GncCustomer *cust)
{
  CustomerWindow *cw;
  GladeXML *xml;
  GtkWidget *hbox, *edit;
  gnc_commodity *currency;
  GNCPrintAmountInfo print_info;
  
  /*
   * Find an existing window for this customer.  If found, bring it to
   * the front.
   */
  if (cust) {
    GUID customer_guid;

    customer_guid = *gncCustomerGetGUID(cust);
    cw = gnc_find_first_gui_component (DIALOG_EDIT_CUSTOMER_CM_CLASS,
				       find_handler, &customer_guid);
    if (cw) {
      gtk_window_present (GTK_WINDOW(cw->dialog));
      return(cw);
    }
  }
  
  /* Find the default currency */
  if (cust)
    currency = gncCustomerGetCurrency (cust);
  else
    currency = gnc_default_currency ();

  /*
   * No existing customer window found.  Build a new one.
   */
  cw = g_new0 (CustomerWindow, 1);

  cw->book = bookp;

  /* Find the dialog */
  xml = gnc_glade_xml_new ("customer.glade", "Customer Dialog");
  cw->dialog = glade_xml_get_widget (xml, "Customer Dialog");

  g_object_set_data (G_OBJECT (cw->dialog), "dialog_info", cw);

  /* Get entry points */
  cw->id_entry = glade_xml_get_widget (xml, "id_entry");
  cw->company_entry = glade_xml_get_widget (xml, "company_entry");

  cw->name_entry = glade_xml_get_widget (xml, "name_entry");
  cw->addr1_entry = glade_xml_get_widget (xml, "addr1_entry");
  cw->addr2_entry = glade_xml_get_widget (xml, "addr2_entry");
  cw->addr3_entry = glade_xml_get_widget (xml, "addr3_entry");
  cw->addr4_entry = glade_xml_get_widget (xml, "addr4_entry");
  cw->phone_entry = glade_xml_get_widget (xml, "phone_entry");
  cw->fax_entry = glade_xml_get_widget (xml, "fax_entry");
  cw->email_entry = glade_xml_get_widget (xml, "email_entry");

  cw->shipname_entry = glade_xml_get_widget (xml, "shipname_entry");
  cw->shipaddr1_entry = glade_xml_get_widget (xml, "shipaddr1_entry");
  cw->shipaddr2_entry = glade_xml_get_widget (xml, "shipaddr2_entry");
  cw->shipaddr3_entry = glade_xml_get_widget (xml, "shipaddr3_entry");
  cw->shipaddr4_entry = glade_xml_get_widget (xml, "shipaddr4_entry");
  cw->shipphone_entry = glade_xml_get_widget (xml, "shipphone_entry");
  cw->shipfax_entry = glade_xml_get_widget (xml, "shipfax_entry");
  cw->shipemail_entry = glade_xml_get_widget (xml, "shipemail_entry");

  cw->active_check = glade_xml_get_widget (xml, "active_check");
  cw->taxincluded_menu = glade_xml_get_widget (xml, "tax_included_menu");
  cw->notes_text = glade_xml_get_widget (xml, "notes_text");

  cw->terms_menu = glade_xml_get_widget (xml, "terms_menu");

  cw->taxtable_check = glade_xml_get_widget (xml, "taxtable_button");
  cw->taxtable_menu = glade_xml_get_widget (xml, "taxtable_menu");

  /* Currency */
  edit = gnc_currency_edit_new();
  gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(edit), currency);
  cw->currency_edit = edit;

  hbox = glade_xml_get_widget (xml, "currency_box");
  gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

  /* DISCOUNT: Percentage Value */
  edit = gnc_amount_edit_new();
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
  print_info = gnc_integral_print_info ();
  print_info.max_decimal_places = 5;
  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit), 100000);
  cw->discount_amount = edit;
  gtk_widget_show (edit);

  hbox = glade_xml_get_widget (xml, "discount_box");
  gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

  /* CREDIT: Monetary Value */
  edit = gnc_amount_edit_new();
  print_info = gnc_commodity_print_info (currency, FALSE);
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit),
                                gnc_commodity_get_fraction (currency));
  cw->credit_amount = edit;
  gtk_widget_show (edit);

  hbox = glade_xml_get_widget (xml, "credit_box");
  gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

  /* Setup signals */
  glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
                                     cw);

  /* Setup initial values */
  if (cust != NULL) {
    GtkTextBuffer* text_buffer;
    GncAddress *addr, *shipaddr;
    const char *string;

    cw->dialog_type = EDIT_CUSTOMER;
    cw->customer_guid = *gncCustomerGetGUID (cust);

    addr = gncCustomerGetAddr (cust);
    shipaddr = gncCustomerGetShipAddr (cust);

    gtk_entry_set_text (GTK_ENTRY (cw->id_entry), gncCustomerGetID (cust));
    gtk_entry_set_text (GTK_ENTRY (cw->company_entry), gncCustomerGetName (cust));

    /* Setup Address */
    gtk_entry_set_text (GTK_ENTRY (cw->name_entry), gncAddressGetName (addr));
    gtk_entry_set_text (GTK_ENTRY (cw->addr1_entry), gncAddressGetAddr1 (addr));
    gtk_entry_set_text (GTK_ENTRY (cw->addr2_entry), gncAddressGetAddr2 (addr));
    gtk_entry_set_text (GTK_ENTRY (cw->addr3_entry), gncAddressGetAddr3 (addr));
    gtk_entry_set_text (GTK_ENTRY (cw->addr4_entry), gncAddressGetAddr4 (addr));
    gtk_entry_set_text (GTK_ENTRY (cw->phone_entry), gncAddressGetPhone (addr));
    gtk_entry_set_text (GTK_ENTRY (cw->fax_entry), gncAddressGetFax (addr));
    gtk_entry_set_text (GTK_ENTRY (cw->email_entry), gncAddressGetEmail (addr));

    /* Setup Ship-to Address */
    gtk_entry_set_text (GTK_ENTRY (cw->shipname_entry), gncAddressGetName (shipaddr));
    gtk_entry_set_text (GTK_ENTRY (cw->shipaddr1_entry), gncAddressGetAddr1 (shipaddr));
    gtk_entry_set_text (GTK_ENTRY (cw->shipaddr2_entry), gncAddressGetAddr2 (shipaddr));
    gtk_entry_set_text (GTK_ENTRY (cw->shipaddr3_entry), gncAddressGetAddr3 (shipaddr));
    gtk_entry_set_text (GTK_ENTRY (cw->shipaddr4_entry), gncAddressGetAddr4 (shipaddr));
    gtk_entry_set_text (GTK_ENTRY (cw->shipphone_entry), gncAddressGetPhone (shipaddr));
    gtk_entry_set_text (GTK_ENTRY (cw->shipfax_entry), gncAddressGetFax (shipaddr));
    gtk_entry_set_text (GTK_ENTRY (cw->shipemail_entry), gncAddressGetEmail (shipaddr));

    /* Set toggle buttons */
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (cw->active_check),
                                gncCustomerGetActive (cust));

    string = gncCustomerGetNotes (cust);
    text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(cw->notes_text));
    gtk_text_buffer_set_text (text_buffer, string, -1);

    cw->component_id =
      gnc_register_gui_component (DIALOG_EDIT_CUSTOMER_CM_CLASS,
				  gnc_customer_window_refresh_handler,
				  gnc_customer_window_close_handler,
				  cw);
    cw->terms = gncCustomerGetTerms (cust);

  } else {
    cust = gncCustomerCreate (bookp);
    cw->customer_guid = *gncCustomerGetGUID (cust);

    cw->dialog_type = NEW_CUSTOMER;
    cw->component_id =
      gnc_register_gui_component (DIALOG_NEW_CUSTOMER_CM_CLASS,
				  gnc_customer_window_refresh_handler,
				  gnc_customer_window_close_handler,
				  cw);

    /* XXX: get the global-default terms */
    cw->terms = NULL;
  }

  /* I know that cust exists here -- either passed in or just created */

  cw->taxincluded = gncCustomerGetTaxIncluded (cust);
  gnc_ui_taxincluded_optionmenu (cw->taxincluded_menu, &cw->taxincluded);
  gnc_ui_billterms_optionmenu (cw->terms_menu, bookp, TRUE, &cw->terms);

  cw->taxtable = gncCustomerGetTaxTable (cust);
  gnc_ui_taxtables_optionmenu (cw->taxtable_menu, bookp, TRUE, &cw->taxtable);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (cw->taxtable_check),
                                gncCustomerGetTaxTableOverride (cust));
  gnc_customer_taxtable_check_cb (GTK_TOGGLE_BUTTON (cw->taxtable_check), cw);

  /* Set the Discount, and Credit amounts */
  gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (cw->discount_amount),
			      gncCustomerGetDiscount (cust));
  gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (cw->credit_amount),
			      gncCustomerGetCredit (cust));

  gnc_gui_component_watch_entity_type (cw->component_id,
				       GNC_CUSTOMER_MODULE_NAME,
				       QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

  gtk_widget_show_all (cw->dialog);

  return cw;
}

CustomerWindow *
gnc_ui_customer_edit (GncCustomer *cust)
{
  CustomerWindow *cw;

  if (!cust) return NULL;

  cw = gnc_customer_new_window (gncCustomerGetBook(cust), cust);

  return cw;
}

CustomerWindow *
gnc_ui_customer_new (GNCBook *bookp)
{
  CustomerWindow *cw;

  /* Make sure required options exist */
  if (!bookp) return NULL;

  cw = gnc_customer_new_window (bookp, NULL);

  return cw;
}

/* Functions for customer selection widgets */

static void
invoice_customer_cb (gpointer *cust_p, gpointer user_data)
{
  struct _customer_select_window *sw = user_data;
  GncOwner owner;
  GncCustomer *cust;

  g_return_if_fail (cust_p && user_data);

  cust = *cust_p;

  if (!cust)
    return;

  gncOwnerInitCustomer (&owner, cust);
  gnc_invoice_search (NULL, &owner, sw->book);
  return;
}

static void
order_customer_cb (gpointer *cust_p, gpointer user_data)
{
  struct _customer_select_window *sw = user_data;
  GncOwner owner;
  GncCustomer *cust;

  g_return_if_fail (cust_p && user_data);

  cust = *cust_p;

  if (!cust)
    return;

  gncOwnerInitCustomer (&owner, cust);
  gnc_order_search (NULL, &owner, sw->book);
  return;
}

static void
jobs_customer_cb (gpointer *cust_p, gpointer user_data)
{
  struct _customer_select_window *sw = user_data;
  GncOwner owner;
  GncCustomer *cust;

  g_return_if_fail (cust_p && user_data);

  cust = *cust_p;

  if (!cust)
    return;

  gncOwnerInitCustomer (&owner, cust);
  gnc_job_search (NULL, &owner, sw->book);
  return;
}

static void
payment_customer_cb (gpointer *cust_p, gpointer user_data)
{
  struct _customer_select_window *sw = user_data;
  GncOwner owner;
  GncCustomer *cust;

  g_return_if_fail (cust_p && user_data);

  cust = *cust_p;

  if (!cust)
    return;

  gncOwnerInitCustomer (&owner, cust);
  gnc_ui_payment_new (&owner, sw->book);
  return;
}

static void
edit_customer_cb (gpointer *cust_p, gpointer user_data)
{
  GncCustomer *cust;

  g_return_if_fail (cust_p);
  cust = *cust_p;

  if (!cust)
    return;

  gnc_ui_customer_edit (cust);

  return;
}

static gpointer
new_customer_cb (gpointer user_data)
{
  struct _customer_select_window *sw = user_data;
  CustomerWindow *cw;
  
  g_return_val_if_fail (sw, NULL);

  cw = gnc_ui_customer_new (sw->book);
  return cw_get_customer (cw);
}

static void
free_userdata_cb (gpointer user_data)
{
  struct _customer_select_window *sw = user_data;

  g_return_if_fail (sw);

  gncQueryDestroy (sw->q);
  g_free (sw);
}

GNCSearchWindow *
gnc_customer_search (GncCustomer *start, GNCBook *book)
{
  QueryNew *q, *q2 = NULL;
  GNCIdType type = GNC_CUSTOMER_MODULE_NAME;
  struct _customer_select_window *sw;
  static GList *params = NULL;
  static GList *columns = NULL;
  static GNCSearchCallbackButton buttons[] = { 
    { N_("View/Edit Customer"), edit_customer_cb},
    { N_("Customer's Jobs"), jobs_customer_cb},
    //    { N_("Customer's Orders"), order_customer_cb},
    { N_("Customer's Invoices"), invoice_customer_cb},
    { N_("Process Payment"), payment_customer_cb},
    { NULL },
  };
  (void)order_customer_cb;

  g_return_val_if_fail (book, NULL);

  /* Build parameter list in reverse order*/
  if (params == NULL) {
    params = gnc_search_param_prepend (params, _("Shipping Contact"), NULL, type,
				       CUSTOMER_SHIPADDR, ADDRESS_NAME, NULL);
    params = gnc_search_param_prepend (params, _("Billing Contact"), NULL, type,
				       CUSTOMER_ADDR, ADDRESS_NAME, NULL);
    params = gnc_search_param_prepend (params, _("Customer ID"), NULL, type,
				       CUSTOMER_ID, NULL);
    params = gnc_search_param_prepend (params, _("Company Name"), NULL, type,
				       CUSTOMER_NAME, NULL);
  }

  /* Build the column list in reverse order */
  if (columns == NULL) {
    columns = gnc_search_param_prepend (columns, _("Contact"), NULL, type,
					CUSTOMER_ADDR, ADDRESS_NAME, NULL);
    columns = gnc_search_param_prepend (columns, _("Company"), NULL, type,
					CUSTOMER_NAME, NULL);
    columns = gnc_search_param_prepend (columns, _("ID #"), NULL, type,
					CUSTOMER_ID, NULL);
  }

  /* Build the queries */
  q = gncQueryCreateFor (type);
  gncQuerySetBook (q, book);

#if 0
  if (start) {
    q2 = gncQueryCopy (q);
    gncQueryAddGUIDMatch (q2, g_slist_prepend (NULL, QUERY_PARAM_GUID),
			  gncCustomerGetGUID (start), QUERY_AND);
  }
#endif

  /* launch select dialog and return the result */
  sw = g_new0 (struct _customer_select_window, 1);

  sw->book = book;
  sw->q = q;

  return gnc_search_dialog_create (type, _("Find Customer"),
				   params, columns, q, q2, buttons, NULL,
				   new_customer_cb, sw, free_userdata_cb,
				   GCONF_SECTION_SEARCH, NULL);
}

GNCSearchWindow *
gnc_customer_search_select (gpointer start, gpointer book)
{
  if (!book) return NULL;

  return gnc_customer_search (start, book);
}

GNCSearchWindow *
gnc_customer_search_edit (gpointer start, gpointer book)
{
  if (start)
    gnc_ui_customer_edit (start);

  return NULL;
}
