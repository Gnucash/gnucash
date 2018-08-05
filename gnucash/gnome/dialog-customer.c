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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>

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
#include "QuickFill.h"
#include "gnc-addr-quickfill.h"

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

#define GNC_PREFS_GROUP_SEARCH "dialogs.business.customer-search"

void gnc_customer_taxtable_check_cb (GtkToggleButton *togglebutton,
                                     gpointer user_data);

void gnc_customer_window_ok_cb (GtkWidget *widget, gpointer data);
void gnc_customer_window_cancel_cb (GtkWidget *widget, gpointer data);
void gnc_customer_window_help_cb (GtkWidget *widget, gpointer data);
void gnc_customer_window_destroy_cb (GtkWidget *widget, gpointer data);
void gnc_customer_name_changed_cb (GtkWidget *widget, gpointer data);
void gnc_customer_terms_changed_cb (GtkWidget *widget, gpointer data);
void gnc_customer_taxincluded_changed_cb (GtkWidget *widget, gpointer data);
void gnc_customer_taxtable_changed_cb (GtkWidget *widget, gpointer data);
void gnc_customer_addr2_insert_cb(GtkEditable *editable,
                                  gchar *new_text, gint new_text_length,
                                  gint *position, gpointer user_data);
void gnc_customer_addr3_insert_cb(GtkEditable *editable,
                                  gchar *new_text, gint new_text_length,
                                  gint *position, gpointer user_data);
void gnc_customer_addr4_insert_cb(GtkEditable *editable,
                                  gchar *new_text, gint new_text_length,
                                  gint *position, gpointer user_data);
void gnc_customer_shipaddr2_insert_cb(GtkEditable *editable,
                                      gchar *new_text, gint new_text_length,
                                      gint *position, gpointer user_data);
void gnc_customer_shipaddr3_insert_cb(GtkEditable *editable,
                                      gchar *new_text, gint new_text_length,
                                      gint *position, gpointer user_data);
void gnc_customer_shipaddr4_insert_cb(GtkEditable *editable,
                                      gchar *new_text, gint new_text_length,
                                      gint *position, gpointer user_data);
gboolean
gnc_customer_addr2_key_press_cb( GtkEntry *entry, GdkEventKey *event,
                                 gpointer user_data );
gboolean
gnc_customer_addr3_key_press_cb( GtkEntry *entry, GdkEventKey *event,
                                 gpointer user_data );
gboolean
gnc_customer_addr4_key_press_cb( GtkEntry *entry, GdkEventKey *event,
                                 gpointer user_data );
gboolean
gnc_customer_shipaddr2_key_press_cb( GtkEntry *entry, GdkEventKey *event,
                                     gpointer user_data );
gboolean
gnc_customer_shipaddr3_key_press_cb( GtkEntry *entry, GdkEventKey *event,
                                     gpointer user_data );
gboolean
gnc_customer_shipaddr4_key_press_cb( GtkEntry *entry, GdkEventKey *event,
                                     gpointer user_data );

#define ADDR_QUICKFILL "GncAddress-Quickfill"

typedef enum
{
    NEW_CUSTOMER,
    EDIT_CUSTOMER
} CustomerDialogType;

struct _customer_select_window
{
    QofBook  *book;
    QofQuery *q;
};

struct _customer_window
{
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
    GncGUID		customer_guid;
    gint		component_id;
    QofBook *	book;
    GncCustomer *	created_customer;

    GncTaxTable *	taxtable;

    /* stored data for the description quickfill selection function */
    QuickFill *addr2_quickfill;
    QuickFill *addr3_quickfill;
    QuickFill *addr4_quickfill;
    gint addrX_start_selection;
    gint addrX_end_selection;
    guint addrX_selection_source_id;
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

    if (cw->dialog_type == NEW_CUSTOMER)
        qof_event_gen(QOF_INSTANCE(cust), QOF_EVENT_ADD, NULL);

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

static gboolean check_edit_amount (GtkWidget *amount,
                                   gnc_numeric *min, gnc_numeric *max,
                                   const char * error_message)
{
    if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (amount)))
    {
        if (error_message)
            gnc_error_dialog (gnc_ui_get_gtk_window (amount), "%s", error_message);
        return TRUE;
    }
    /* We've got a valid-looking number; check mix/max */
    if (min || max)
    {
        gnc_numeric val = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (amount));
        if ((min && gnc_numeric_compare (*min, val) > 0) ||
                (max && gnc_numeric_compare (val, *max) > 0))
        {
            if (error_message)
                gnc_error_dialog (gnc_ui_get_gtk_window (amount), "%s", error_message);
            return TRUE;
        }
    }
    return FALSE;
}

static gboolean check_entry_nonempty (GtkWidget *entry,
                                      const char * error_message)
{
    const char *res = gtk_entry_get_text (GTK_ENTRY (entry));
    if (g_strcmp0 (res, "") == 0)
    {
        if (error_message)
            gnc_error_dialog (gnc_ui_get_gtk_window (entry), "%s", error_message);
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
    if (check_entry_nonempty (cw->company_entry,
                              _("You must enter a company name. "
                                "If this customer is an individual (and not a company) "
                                "you should enter the same value for:\nIdentification "
                                "- Company Name, and\nPayment Address - Name.")))
        return;

    /* Make sure we have an address */
    if (check_entry_nonempty (cw->addr1_entry, NULL) &&
            check_entry_nonempty (cw->addr2_entry, NULL) &&
            check_entry_nonempty (cw->addr3_entry, NULL) &&
            check_entry_nonempty (cw->addr4_entry, NULL))
    {
        const char *msg = _("You must enter a billing address.");
        gnc_error_dialog (gnc_ui_get_gtk_window (widget), "%s", msg);
        return;
    }

    /* Verify terms, discount, and credit are valid (or empty) */
    min = gnc_numeric_zero ();
    max = gnc_numeric_create (100, 1);

    if (check_edit_amount (cw->discount_amount, &min, &max,
                           _("Discount percentage must be between 0-100 "
                             "or you must leave it blank.")))
        return;

    if (check_edit_amount (cw->credit_amount, &min, NULL,
                           _("Credit must be a positive amount or "
                             "you must leave it blank.")))
        return;

    /* Set the customer id if one has not been chosen */
    if (g_strcmp0 (gtk_entry_get_text (GTK_ENTRY (cw->id_entry)), "") == 0)
    {
        string = gncCustomerNextID (cw->book);
        gtk_entry_set_text (GTK_ENTRY (cw->id_entry), string);
        g_free(string);
    }

    /* Now save it off */
    {
        GncCustomer *customer = cw_get_customer (cw);
        if (customer)
        {
            gnc_ui_to_customer (cw, customer);
        }
        cw->created_customer = customer;
        cw->customer_guid = *guid_null ();
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
    gnc_gnome_help(HF_HELP, HL_USAGE_CUSTOMER);
}

void
gnc_customer_window_destroy_cb (GtkWidget *widget, gpointer data)
{
    CustomerWindow *cw = data;
    GncCustomer *customer = cw_get_customer (cw);

    gnc_suspend_gui_refresh ();

    if (cw->dialog_type == NEW_CUSTOMER && customer != NULL)
    {
        gncCustomerBeginEdit (customer);
        gncCustomerDestroy (customer);
        cw->customer_guid = *guid_null ();
    }

    if (cw->addrX_selection_source_id)
        g_source_remove (cw->addrX_selection_source_id);

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

void
gnc_customer_terms_changed_cb (GtkWidget *widget, gpointer data)
{
    GtkComboBox *cbox = GTK_COMBO_BOX (widget);
    CustomerWindow *cw = data;

    if (!cw) return;
    if (!cbox) return;

    cw->terms = gnc_simple_combo_get_value (cbox);
}

void
gnc_customer_taxincluded_changed_cb (GtkWidget *widget, gpointer data)
{
    GtkComboBox *cbox = GTK_COMBO_BOX (widget);
    CustomerWindow *cw = data;

    if (!cw) return;
    if (!cbox) return;

    cw->taxincluded = GPOINTER_TO_INT (gnc_simple_combo_get_value (cbox));
}

void
gnc_customer_taxtable_changed_cb (GtkWidget *widget, gpointer data)
{
    GtkComboBox *cbox = GTK_COMBO_BOX (widget);
    CustomerWindow *cw = data;

    if (!cw) return;
    if (!cbox) return;

    cw->taxtable = gnc_simple_combo_get_value (cbox);
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
    if (!customer)
    {
        gnc_close_gui_component (cw->component_id);
        return;
    }

    /* Next, close if this is a destroy event */
    if (changes)
    {
        info = gnc_gui_get_entity_events (changes, &cw->customer_guid);
        if (info && (info->event_mask & QOF_EVENT_DESTROY))
        {
            gnc_close_gui_component (cw->component_id);
            return;
        }
    }
}

static gboolean
find_handler (gpointer find_data, gpointer user_data)
{
    const GncGUID *customer_guid = find_data;
    CustomerWindow *cw = user_data;

    return(cw && guid_equal(&cw->customer_guid, customer_guid));
}

static CustomerWindow *
gnc_customer_new_window (GtkWindow *parent, QofBook *bookp, GncCustomer *cust)
{
    CustomerWindow *cw;
    GtkBuilder *builder;
    GtkWidget *hbox, *edit;
    gnc_commodity *currency;
    GNCPrintAmountInfo print_info;

    /*
     * Find an existing window for this customer.  If found, bring it to
     * the front.
     */
    if (cust)
    {
        GncGUID customer_guid;

        customer_guid = *gncCustomerGetGUID(cust);
        cw = gnc_find_first_gui_component (DIALOG_EDIT_CUSTOMER_CM_CLASS,
                                           find_handler, &customer_guid);
        if (cw)
        {
            gtk_window_set_transient_for (GTK_WINDOW(cw->dialog), parent);
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
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-customer.glade", "terms_store");
    gnc_builder_add_from_file (builder, "dialog-customer.glade", "tax_included_store");
    gnc_builder_add_from_file (builder, "dialog-customer.glade", "taxtable_store");
    gnc_builder_add_from_file (builder, "dialog-customer.glade", "customer_dialog");
    cw->dialog = GTK_WIDGET (gtk_builder_get_object (builder, "customer_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW(cw->dialog), parent);

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(cw->dialog), "GncCustomerDialog");

    g_object_set_data (G_OBJECT (cw->dialog), "dialog_info", cw);

    /* Get entry points */
    cw->id_entry = GTK_WIDGET (gtk_builder_get_object (builder, "id_entry"));
    cw->company_entry = GTK_WIDGET (gtk_builder_get_object (builder, "company_entry"));

    cw->name_entry = GTK_WIDGET (gtk_builder_get_object (builder, "name_entry"));
    cw->addr1_entry = GTK_WIDGET (gtk_builder_get_object (builder, "addr1_entry"));
    cw->addr2_entry = GTK_WIDGET (gtk_builder_get_object (builder, "addr2_entry"));
    cw->addr3_entry = GTK_WIDGET (gtk_builder_get_object (builder, "addr3_entry"));
    cw->addr4_entry = GTK_WIDGET (gtk_builder_get_object (builder, "addr4_entry"));
    cw->phone_entry = GTK_WIDGET (gtk_builder_get_object (builder, "phone_entry"));
    cw->fax_entry = GTK_WIDGET (gtk_builder_get_object (builder, "fax_entry"));
    cw->email_entry = GTK_WIDGET (gtk_builder_get_object (builder, "email_entry"));

    cw->shipname_entry = GTK_WIDGET (gtk_builder_get_object (builder, "shipname_entry"));
    cw->shipaddr1_entry = GTK_WIDGET (gtk_builder_get_object (builder, "shipaddr1_entry"));
    cw->shipaddr2_entry = GTK_WIDGET (gtk_builder_get_object (builder, "shipaddr2_entry"));
    cw->shipaddr3_entry = GTK_WIDGET (gtk_builder_get_object (builder, "shipaddr3_entry"));
    cw->shipaddr4_entry = GTK_WIDGET (gtk_builder_get_object (builder, "shipaddr4_entry"));
    cw->shipphone_entry = GTK_WIDGET (gtk_builder_get_object (builder, "shipphone_entry"));
    cw->shipfax_entry = GTK_WIDGET (gtk_builder_get_object (builder, "shipfax_entry"));
    cw->shipemail_entry = GTK_WIDGET (gtk_builder_get_object (builder, "shipemail_entry"));

    cw->active_check = GTK_WIDGET (gtk_builder_get_object (builder, "active_check"));
    cw->taxincluded_menu = GTK_WIDGET (gtk_builder_get_object (builder, "tax_included_menu"));
    cw->notes_text = GTK_WIDGET (gtk_builder_get_object (builder, "notes_text"));

    cw->terms_menu = GTK_WIDGET (gtk_builder_get_object (builder, "terms_menu"));

    cw->taxtable_check = GTK_WIDGET (gtk_builder_get_object (builder, "taxtable_button"));
    cw->taxtable_menu = GTK_WIDGET (gtk_builder_get_object (builder, "taxtable_menu"));

    /* Currency */
    edit = gnc_currency_edit_new();
    gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(edit), currency);
    cw->currency_edit = edit;

    hbox = GTK_WIDGET (gtk_builder_get_object (builder, "currency_box"));
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

    hbox = GTK_WIDGET (gtk_builder_get_object (builder, "discount_box"));
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

    hbox = GTK_WIDGET (gtk_builder_get_object (builder, "credit_box"));
    gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

    /* Setup signals */
    gtk_builder_connect_signals_full( builder,
                                      gnc_builder_connect_full_func,
                                      cw);

    /* Setup initial values */
    if (cust != NULL)
    {
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

    }
    else
    {
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
    gnc_taxincluded_combo (GTK_COMBO_BOX(cw->taxincluded_menu), cw->taxincluded);
    gnc_billterms_combo (GTK_COMBO_BOX(cw->terms_menu), bookp, TRUE, cw->terms);

    cw->taxtable = gncCustomerGetTaxTable (cust);
    gnc_taxtables_combo (GTK_COMBO_BOX(cw->taxtable_menu), bookp, TRUE, cw->taxtable);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (cw->taxtable_check),
                                  gncCustomerGetTaxTableOverride (cust));
    gnc_customer_taxtable_check_cb (GTK_TOGGLE_BUTTON (cw->taxtable_check), cw);

    /* Set up the addr line quickfill */
    cw->addr2_quickfill = gnc_get_shared_address_addr2_quickfill(cw->book, ADDR_QUICKFILL);
    cw->addr3_quickfill = gnc_get_shared_address_addr3_quickfill(cw->book, ADDR_QUICKFILL);
    cw->addr4_quickfill = gnc_get_shared_address_addr4_quickfill(cw->book, ADDR_QUICKFILL);

    /* Set the Discount, and Credit amounts */
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (cw->discount_amount),
                                gncCustomerGetDiscount (cust));
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (cw->credit_amount),
                                gncCustomerGetCredit (cust));

    gnc_gui_component_watch_entity_type (cw->component_id,
                                         GNC_CUSTOMER_MODULE_NAME,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    gtk_widget_show_all (cw->dialog);
    g_object_unref(G_OBJECT(builder));

    return cw;
}

CustomerWindow *
gnc_ui_customer_edit (GtkWindow *parent, GncCustomer *cust)
{
    CustomerWindow *cw;

    if (!cust) return NULL;

    cw = gnc_customer_new_window (parent, gncCustomerGetBook(cust), cust);

    return cw;
}

CustomerWindow *
gnc_ui_customer_new (GtkWindow *parent, QofBook *bookp)
{
    CustomerWindow *cw;

    /* Make sure required options exist */
    if (!bookp) return NULL;

    cw = gnc_customer_new_window (parent, bookp, NULL);

    return cw;
}

/* Functions for customer selection widgets */

static void
invoice_customer_cb (GtkWindow *dialog, gpointer *cust_p, gpointer user_data)
{
    struct _customer_select_window *sw = user_data;
    GncOwner owner;
    GncCustomer *cust;

    g_return_if_fail (cust_p && user_data);

    cust = *cust_p;

    if (!cust)
        return;

    gncOwnerInitCustomer (&owner, cust);
    gnc_invoice_search (dialog, NULL, &owner, sw->book);
    return;
}

static void
order_customer_cb (GtkWindow *dialog, gpointer *cust_p, gpointer user_data)
{
    struct _customer_select_window *sw = user_data;
    GncOwner owner;
    GncCustomer *cust;

    g_return_if_fail (cust_p && user_data);

    cust = *cust_p;

    if (!cust)
        return;

    gncOwnerInitCustomer (&owner, cust);
    gnc_order_search (dialog, NULL, &owner, sw->book);
    return;
}

static void
jobs_customer_cb (GtkWindow *dialog, gpointer *cust_p, gpointer user_data)
{
    struct _customer_select_window *sw = user_data;
    GncOwner owner;
    GncCustomer *cust;

    g_return_if_fail (cust_p && user_data);

    cust = *cust_p;

    if (!cust)
        return;

    gncOwnerInitCustomer (&owner, cust);
    gnc_job_search (dialog, NULL, &owner, sw->book);
    return;
}

static void
payment_customer_cb (GtkWindow *dialog, gpointer *cust_p, gpointer user_data)
{
    struct _customer_select_window *sw = user_data;
    GncOwner owner;
    GncCustomer *cust;

    g_return_if_fail (cust_p && user_data);

    cust = *cust_p;

    if (!cust)
        return;

    gncOwnerInitCustomer (&owner, cust);
    gnc_ui_payment_new (dialog, &owner, sw->book);
    return;
}

static void
edit_customer_cb (GtkWindow *dialog, gpointer *cust_p, gpointer user_data)
{
    GncCustomer *cust;

    g_return_if_fail (cust_p);
    cust = *cust_p;

    if (!cust)
        return;

    gnc_ui_customer_edit (dialog, cust);

    return;
}

static gpointer
new_customer_cb (GtkWindow *dialog, gpointer user_data)
{
    struct _customer_select_window *sw = user_data;
    CustomerWindow *cw;

    g_return_val_if_fail (sw, NULL);

    cw = gnc_ui_customer_new (dialog, sw->book);
    return cw_get_customer (cw);
}

static void
free_userdata_cb (gpointer user_data)
{
    struct _customer_select_window *sw = user_data;

    g_return_if_fail (sw);

    qof_query_destroy (sw->q);
    g_free (sw);
}

GNCSearchWindow *
gnc_customer_search (GtkWindow *parent, GncCustomer *start, QofBook *book)
{
    QofQuery *q, *q2 = NULL;
    QofIdType type = GNC_CUSTOMER_MODULE_NAME;
    struct _customer_select_window *sw;
    static GList *params = NULL;
    static GList *columns = NULL;
    static GNCSearchCallbackButton buttons[] =
    {
        { N_("View/Edit Customer"), edit_customer_cb, NULL, TRUE},
        { N_("Customer's Jobs"), jobs_customer_cb, NULL, TRUE},
        //    { N_("Customer's Orders"), order_customer_cb, NULL, TRUE},
        { N_("Customer's Invoices"), invoice_customer_cb, NULL, TRUE},
        { N_("Process Payment"), payment_customer_cb, NULL, FALSE},
        { NULL },
    };
    (void)order_customer_cb;

    g_return_val_if_fail (book, NULL);

    /* Build parameter list in reverse order */
    if (params == NULL)
    {
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
    if (columns == NULL)
    {
        columns = gnc_search_param_prepend (columns, _("Contact"), NULL, type,
                                            CUSTOMER_ADDR, ADDRESS_NAME, NULL);
        columns = gnc_search_param_prepend (columns, _("Company"), NULL, type,
                                            CUSTOMER_NAME, NULL);
        columns = gnc_search_param_prepend (columns, _("ID #"), NULL, type,
                                            CUSTOMER_ID, NULL);
    }

    /* Build the queries */
    q = qof_query_create_for (type);
    qof_query_set_book (q, book);

#if 0
    if (start)
    {
        q2 = qof_query_copy (q);
        qof_query_add_guid_match (q2, g_slist_prepend (NULL, QOF_PARAM_GUID),
                                  gncCustomerGetGUID (start), QOF_QUERY_AND);
    }
#endif

    /* launch select dialog and return the result */
    sw = g_new0 (struct _customer_select_window, 1);

    sw->book = book;
    sw->q = q;

    return gnc_search_dialog_create (parent, type, _("Find Customer"),
                                     params, columns, q, q2, buttons, NULL,
                                     new_customer_cb, sw, free_userdata_cb,
                                     GNC_PREFS_GROUP_SEARCH, NULL,
                                     "GncFindCustomerDialog");
}

GNCSearchWindow *
gnc_customer_search_select (GtkWindow *parent, gpointer start, gpointer book)
{
    if (!book) return NULL;

    return gnc_customer_search (parent, start, book);
}

GNCSearchWindow *
gnc_customer_search_edit (GtkWindow *parent, gpointer start, gpointer book)
{
    if (start)
        gnc_ui_customer_edit (parent, start);

    return NULL;
}

static gboolean
idle_select_region_addr2(gpointer user_data)
{
    CustomerWindow *wdata = user_data;
    g_return_val_if_fail(user_data, FALSE);

    /* g_warning("Selection start=%d end=%d",wdata->addrX_start_selection, wdata->addrX_end_selection); */

    gtk_editable_select_region(GTK_EDITABLE(wdata->addr2_entry),
                               wdata->addrX_start_selection,
                               wdata->addrX_end_selection);

    wdata->addrX_selection_source_id = 0;
    return FALSE;
}

static gboolean
idle_select_region_addr3(gpointer user_data)
{
    CustomerWindow *wdata = user_data;
    g_return_val_if_fail(user_data, FALSE);

    gtk_editable_select_region(GTK_EDITABLE(wdata->addr3_entry),
                               wdata->addrX_start_selection,
                               wdata->addrX_end_selection);

    wdata->addrX_selection_source_id = 0;
    return FALSE;
}
static gboolean
idle_select_region_addr4(gpointer user_data)
{
    CustomerWindow *wdata = user_data;
    g_return_val_if_fail(user_data, FALSE);

    gtk_editable_select_region(GTK_EDITABLE(wdata->addr4_entry),
                               wdata->addrX_start_selection,
                               wdata->addrX_end_selection);
    wdata->addrX_selection_source_id = 0;
    return FALSE;
}

static gboolean
idle_select_region_shipaddr2(gpointer user_data)
{
    CustomerWindow *wdata = user_data;
    g_return_val_if_fail(user_data, FALSE);

    gtk_editable_select_region(GTK_EDITABLE(wdata->shipaddr2_entry),
                               wdata->addrX_start_selection,
                               wdata->addrX_end_selection);

    wdata->addrX_selection_source_id = 0;
    return FALSE;
}

static gboolean
idle_select_region_shipaddr3(gpointer user_data)
{
    CustomerWindow *wdata = user_data;
    g_return_val_if_fail(user_data, FALSE);

    gtk_editable_select_region(GTK_EDITABLE(wdata->shipaddr3_entry),
                               wdata->addrX_start_selection,
                               wdata->addrX_end_selection);

    wdata->addrX_selection_source_id = 0;
    return FALSE;
}
static gboolean
idle_select_region_shipaddr4(gpointer user_data)
{
    CustomerWindow *wdata = user_data;
    g_return_val_if_fail(user_data, FALSE);

    gtk_editable_select_region(GTK_EDITABLE(wdata->shipaddr4_entry),
                               wdata->addrX_start_selection,
                               wdata->addrX_end_selection);
    wdata->addrX_selection_source_id = 0;
    return FALSE;
}

/* Implementation of the steps common to all address lines. Returns
 * TRUE if anything was inserted by quickfill, otherwise FALSE. */
static gboolean
gnc_customer_addr_common_insert_cb(GtkEditable *editable,
                                   gchar *new_text, gint new_text_length,
                                   gint *position, gpointer user_data, QuickFill *qf)
{
    CustomerWindow *wdata = user_data;
    gchar *concatenated_text;
    QuickFill *match;
    gint prefix_len, concatenated_text_len;

    if (new_text_length <= 0)
        return FALSE;

    /*g_warning("In gnc_customer_addr_common_insert_cb");*/

    {
        gchar *suffix = gtk_editable_get_chars(editable, *position, -1);
        /* If we are inserting in the middle, do nothing */
        if (*suffix)
        {
            g_free(suffix);
            return FALSE;
        }
        g_free(suffix);
    }

    {
        gchar *prefix = gtk_editable_get_chars(editable, 0, *position);
        prefix_len = strlen(prefix);
        concatenated_text = g_strconcat(prefix, new_text, (gchar*) NULL);
        concatenated_text_len = prefix_len + new_text_length;
        g_free(prefix);
    }

    match = gnc_quickfill_get_string_match(qf, concatenated_text);
    g_free(concatenated_text);
    if (match)
    {
        const char* match_str = gnc_quickfill_string(match);
        if (match_str)
        {
            gint match_str_len = strlen(match_str);
            if (match_str_len > concatenated_text_len)
            {
                /* g_warning("Got match \"%s\" match_str_len=%d new_text=%s position=%d prefix_len=%d", match_str, match_str_len, new_text, *position, prefix_len); */

                g_signal_handlers_block_matched (G_OBJECT (editable),
                                                 G_SIGNAL_MATCH_DATA, 0, 0, NULL, NULL, user_data);

                gtk_editable_insert_text(editable,
                                         match_str + prefix_len,
                                         match_str_len - prefix_len,
                                         position);

                g_signal_handlers_unblock_matched (G_OBJECT (editable),
                                                   G_SIGNAL_MATCH_DATA, 0, 0, NULL, NULL, user_data);

                /* stop the current insert */
                g_signal_stop_emission_by_name (G_OBJECT (editable), "insert_text");

                /* set the position */
                *position = concatenated_text_len;

                /* select region on idle, because it would be reset once this function
                   finishes */
                wdata->addrX_start_selection = *position;
                wdata->addrX_end_selection = -1;

                return TRUE;
            }
        }
    }
    return FALSE;
}

void gnc_customer_addr2_insert_cb(GtkEditable *editable,
                                  gchar *new_text, gint new_text_length,
                                  gint *position, gpointer user_data)
{
    CustomerWindow *wdata = user_data;
    gboolean r;

    /* The handling common to all address lines is done in this other
     * function. */
    r = gnc_customer_addr_common_insert_cb(editable, new_text, new_text_length,
                                           position, user_data, wdata->addr2_quickfill);

    /* Did we insert something? Then set up the correct idle handler */
    if (r)
    {
        /* select region on idle, because it would be reset once this function
           finishes */
        wdata->addrX_selection_source_id = g_idle_add(idle_select_region_addr2,
                                           user_data);
    }
}

void gnc_customer_addr3_insert_cb(GtkEditable *editable,
                                  gchar *new_text, gint new_text_length,
                                  gint *position, gpointer user_data)
{
    CustomerWindow *wdata = user_data;
    gboolean r;

    /* The handling common to all address lines is done in this other
     * function. */
    r = gnc_customer_addr_common_insert_cb(editable, new_text, new_text_length,
                                           position, user_data, wdata->addr3_quickfill);

    /* Did we insert something? Then set up the correct idle handler */
    if (r)
    {
        wdata->addrX_selection_source_id = g_idle_add(idle_select_region_addr3,
                                           user_data);
    }
}

void gnc_customer_addr4_insert_cb(GtkEditable *editable,
                                  gchar *new_text, gint new_text_length,
                                  gint *position, gpointer user_data)
{
    CustomerWindow *wdata = user_data;
    gboolean r;

    /* The handling common to all address lines is done in this other
     * function. */
    r = gnc_customer_addr_common_insert_cb(editable, new_text, new_text_length,
                                           position, user_data, wdata->addr4_quickfill);

    /* Did we insert something? Then set up the correct idle handler */
    if (r)
    {
        wdata->addrX_selection_source_id = g_idle_add(idle_select_region_addr4,
                                           user_data);
    }
}

void gnc_customer_shipaddr2_insert_cb(GtkEditable *editable,
                                      gchar *new_text, gint new_text_length,
                                      gint *position, gpointer user_data)
{
    CustomerWindow *wdata = user_data;
    gboolean r;

    /* The handling common to all address lines is done in this other
     * function. */
    r = gnc_customer_addr_common_insert_cb(editable, new_text, new_text_length,
                                           position, user_data, wdata->addr2_quickfill);

    /* Did we insert something? Then set up the correct idle handler */
    if (r)
    {
        wdata->addrX_selection_source_id = g_idle_add(idle_select_region_shipaddr2,
                                           user_data);
    }
}

void gnc_customer_shipaddr3_insert_cb(GtkEditable *editable,
                                      gchar *new_text, gint new_text_length,
                                      gint *position, gpointer user_data)
{
    CustomerWindow *wdata = user_data;
    gboolean r;

    /* The handling common to all address lines is done in this other
     * function. */
    r = gnc_customer_addr_common_insert_cb(editable, new_text, new_text_length,
                                           position, user_data, wdata->addr3_quickfill);

    /* Did we insert something? Then set up the correct idle handler */
    if (r)
    {
        wdata->addrX_selection_source_id = g_idle_add(idle_select_region_shipaddr3,
                                           user_data);
    }
}

void gnc_customer_shipaddr4_insert_cb(GtkEditable *editable,
                                      gchar *new_text, gint new_text_length,
                                      gint *position, gpointer user_data)
{
    CustomerWindow *wdata = user_data;
    gboolean r;

    /* The handling common to all address lines is done in this other
     * function. */
    r = gnc_customer_addr_common_insert_cb(editable, new_text, new_text_length,
                                           position, user_data, wdata->addr4_quickfill);

    /* Did we insert something? Then set up the correct idle handler */
    if (r)
    {
        wdata->addrX_selection_source_id = g_idle_add(idle_select_region_shipaddr4,
                                           user_data);
    }
}

static gboolean
gnc_customer_common_key_press_cb( GtkEntry *entry,
                                  GdkEventKey *event,
                                  gpointer user_data, GtkWidget* editable )
{
    gboolean done_with_input = FALSE;

    /* Most "special" keys are allowed to be handled directly by
     * the entry's key press handler, but in some cases that doesn't
     * seem to work right, so handle them here.
     */
    switch ( event->keyval )
    {
    case GDK_KEY_Tab:
    case GDK_KEY_ISO_Left_Tab:
        if ( !( event->state & GDK_SHIFT_MASK) )    /* Complete on Tab,
                                                  * but not Shift-Tab */
        {
            /* NOT done with input, though, since we need to focus to the next
             * field.  Unselect the current field, though.
             */
            gtk_editable_select_region( GTK_EDITABLE(editable),
                                        0, 0 );
        }
        break;
    }

    return( done_with_input );
}
gboolean
gnc_customer_addr2_key_press_cb( GtkEntry *entry,
                                 GdkEventKey *event,
                                 gpointer user_data )
{
    CustomerWindow *wdata = user_data;
    return gnc_customer_common_key_press_cb(entry, event, user_data,
                                            wdata->addr2_entry);
}
gboolean
gnc_customer_addr3_key_press_cb( GtkEntry *entry,
                                 GdkEventKey *event,
                                 gpointer user_data )
{
    CustomerWindow *wdata = user_data;
    return gnc_customer_common_key_press_cb(entry, event, user_data,
                                            wdata->addr3_entry);
}
gboolean
gnc_customer_addr4_key_press_cb( GtkEntry *entry,
                                 GdkEventKey *event,
                                 gpointer user_data )
{
    CustomerWindow *wdata = user_data;
    return gnc_customer_common_key_press_cb(entry, event, user_data,
                                            wdata->addr4_entry);
}
gboolean
gnc_customer_shipaddr2_key_press_cb( GtkEntry *entry,
                                     GdkEventKey *event,
                                     gpointer user_data )
{
    CustomerWindow *wdata = user_data;
    return gnc_customer_common_key_press_cb(entry, event, user_data,
                                            wdata->shipaddr2_entry);
}
gboolean
gnc_customer_shipaddr3_key_press_cb( GtkEntry *entry,
                                     GdkEventKey *event,
                                     gpointer user_data )
{
    CustomerWindow *wdata = user_data;
    return gnc_customer_common_key_press_cb(entry, event, user_data,
                                            wdata->shipaddr3_entry);
}
gboolean
gnc_customer_shipaddr4_key_press_cb( GtkEntry *entry,
                                     GdkEventKey *event,
                                     gpointer user_data )
{
    CustomerWindow *wdata = user_data;
    return gnc_customer_common_key_press_cb(entry, event, user_data,
                                            wdata->shipaddr4_entry);
}
