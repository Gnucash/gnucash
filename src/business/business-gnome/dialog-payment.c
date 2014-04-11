/*
 * dialog-payment.c -- Dialog for payment entry
 * Copyright (C) 2002,2006 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
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
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "gnc-date-edit.h"
#include "gnc-amount-edit.h"
#include "gnc-gtk-utils.h"
#include "gnc-tree-view-account.h"
#include "Transaction.h"
#include "Account.h"

#include "gncInvoice.h"

#include "dialog-payment.h"
#include "business-gnome-utils.h"

#include "dialog-transfer.h"

#define DIALOG_PAYMENT_CUSTOMER_CM_CLASS "customer-payment-dialog"
#define DIALOG_PAYMENT_VENDOR_CM_CLASS "vendor-payment-dialog"

struct _payment_window {
  GtkWidget *	dialog;

  GtkWidget *	num_entry;
  GtkWidget *	memo_entry;
  GtkWidget *	post_combo;
  GtkWidget *	owner_choice;
  GtkWidget *	invoice_choice;
  GtkWidget *	amount_edit;
  GtkWidget *	date_edit;
  GtkWidget *	acct_tree;

  gint		component_id;
  GNCBook *	book;
  GncOwner	owner;
  GncInvoice *	invoice;
  GList *	acct_types;
  GList *       acct_commodities;
};


void gnc_payment_ok_cb (GtkWidget *widget, gpointer data);
void gnc_payment_cancel_cb (GtkWidget *widget, gpointer data);
void gnc_payment_window_destroy_cb (GtkWidget *widget, gpointer data);


static void
gnc_payment_window_refresh_handler (GHashTable *changes, gpointer data)
{
  PaymentWindow *pw = data;

  gnc_fill_account_select_combo (pw->post_combo, pw->book, pw->acct_types, pw->acct_commodities);
}

static void
gnc_payment_window_close_handler (gpointer data)
{
  PaymentWindow *pw = data;

  if (pw)
    gtk_widget_destroy (pw->dialog);
}

static void
gnc_payment_dialog_invoice_changed(PaymentWindow *pw)
{
  GNCLot *lot;
  gnc_numeric val;

  /* Set the payment amount in the dialog */
  if (pw->invoice) {
    lot = gncInvoiceGetPostedLot (pw->invoice);
    val = gnc_numeric_abs (gnc_lot_get_balance (lot));
  } else {
    val = gnc_numeric_zero();
  }

  gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(pw->amount_edit), val);
}

static void
gnc_payment_dialog_owner_changed(PaymentWindow *pw)
{
  Account *last_acct=NULL;
  GUID *guid=NULL;
  KvpValue* value;
  KvpFrame* slots;

  /* If the owner changed, the invoice selection is invalid */
  pw->invoice = NULL;
  gnc_invoice_set_owner(pw->invoice_choice, &pw->owner);
  /* note that set_owner implies ...set_invoice(...,NULL); */

  /* in case we don't get the callback */
  gnc_payment_dialog_invoice_changed(pw);

  /* XXX: We should set the sensitive flag on the invoice_choice
   * based on whether 'owner' is NULL or not...
   */

  /* Now handle the account tree */
  slots = gncOwnerGetSlots(&pw->owner);
  if (slots)
  {
    value = kvp_frame_get_slot_path(slots, "payment", "last_acct", NULL);
    if (value)
    {
      guid = kvp_value_get_guid(value);
    }
  }

  /* refresh the post and acc available accounts, but cleanup first */
  if (pw->acct_types)
  {
    g_list_free(pw->acct_types);
    pw->acct_types = NULL;
  }

  if (pw->acct_commodities)
  {  
    g_list_free(pw->acct_commodities);
    pw->acct_commodities = NULL;
  }

  pw->acct_types = gnc_business_account_types(&pw->owner);
  pw->acct_commodities = gnc_business_commodities (&pw->owner);
  gnc_fill_account_select_combo (pw->post_combo, pw->book, pw->acct_types, pw->acct_commodities);

  if (guid)
  {
    last_acct = xaccAccountLookup(guid, pw->book);
  }

  /* Set the last-used transfer account */
  if (last_acct) {
    gnc_tree_view_account_set_selected_account(GNC_TREE_VIEW_ACCOUNT(pw->acct_tree),
					       last_acct);
  }
}

static void
gnc_payment_dialog_remember_account(PaymentWindow *pw, Account *acc)
{
  KvpValue* value;
  KvpFrame* slots = gncOwnerGetSlots(&pw->owner);

  if (!acc) return;
  if (!slots) return;

  value = kvp_value_new_guid(xaccAccountGetGUID(acc));
  if (!value) return;
  
  kvp_frame_set_slot_path(slots, value, "payment", "last_acct", NULL);
  kvp_value_delete(value);

  /* XXX: FIXME:  Need a commit_edit here to save the data! */
}


static void
gnc_payment_set_owner (PaymentWindow *pw, GncOwner *owner)
{
  gnc_owner_set_owner (pw->owner_choice, owner);
  gnc_payment_dialog_owner_changed(pw);
}

static int
gnc_payment_dialog_owner_changed_cb (GtkWidget *widget, gpointer data)
{
  PaymentWindow *pw = data;
  GncOwner owner;

  if (!pw) return FALSE;

  gncOwnerCopy (&(pw->owner), &owner);
  gnc_owner_get_owner (pw->owner_choice, &owner);

  /* If this owner really changed, then reset ourselves */
  if (!gncOwnerEqual (&owner, &(pw->owner))) {
    gncOwnerCopy (&owner, &(pw->owner));
    gnc_payment_dialog_owner_changed(pw);
  }

  return FALSE;
}

static int
gnc_payment_dialog_invoice_changed_cb (GtkWidget *widget, gpointer data)
{
  PaymentWindow *pw = data;
  GncInvoice *invoice;

  if (!pw) return FALSE;

  invoice = gnc_invoice_get_invoice (pw->invoice_choice);

  /* If this invoice really changed, then reset ourselves */
  if (invoice != pw->invoice) {
    pw->invoice = invoice;
    gnc_payment_dialog_invoice_changed(pw);
  }

  return FALSE;
}

void
gnc_payment_ok_cb (GtkWidget *widget, gpointer data)
{
  PaymentWindow *pw = data;
  const char *text;
  Account *post, *acc;
  gnc_numeric amount;

  if (!pw)
    return;

  /* Verify the amount is non-zero */
  amount = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (pw->amount_edit));
  if (gnc_numeric_check (amount) || !gnc_numeric_positive_p (amount)) {
    text = _("You must enter the amount of the payment.  "
	     "The payment amount must be greater than zero.");
    gnc_error_dialog (pw->dialog, "%s", text);
    return;
  }

  /* Verify the user has selected an owner */
  gnc_owner_get_owner (pw->owner_choice, &(pw->owner));
  if (pw->owner.owner.undefined == NULL) {
    text = _("You must select a company for payment processing.");
    gnc_error_dialog (pw->dialog, "%s", text);
    return;
  }

  /* Verify the user has selected a transfer account */
  acc = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(pw->acct_tree));
  if (!acc) {
    text = _("You must select a transfer account from the account tree.");
    gnc_error_dialog (pw->dialog, "%s", text);
    return;
  }

  /* Verify the "post" account */
  text = gtk_combo_box_get_active_text(GTK_COMBO_BOX(pw->post_combo));
  if (!text || safe_strcmp (text, "") == 0) {
    text = _("You must enter an account name for posting.");
    gnc_error_dialog (pw->dialog, "%s", text);
    return;
  }

  post = gnc_account_lookup_by_full_name (gnc_book_get_root_account (pw->book), text);

  if (!post) {
    char *msg = g_strdup_printf (
			 _("Your selected post account, %s, does not exist"),
			 text);
    gnc_error_dialog (pw->dialog, "%s", msg);
    g_free (msg);
    return;
  }

  /* Ok, now post the damn thing */
  gnc_suspend_gui_refresh ();
  {
    const char *memo, *num;
    Timespec date;
    gnc_numeric exch = gnc_numeric_create(1,1); //default to "one to one" rate
    
    /* Obtain all our ancillary information */
    memo = gtk_entry_get_text (GTK_ENTRY (pw->memo_entry));
    num = gtk_entry_get_text (GTK_ENTRY (pw->num_entry));
    date = gnc_date_edit_get_date_ts (GNC_DATE_EDIT (pw->date_edit));

    /* If the 'acc' account and the post account don't have the same
       currency, we need to get the user to specify the exchange rate */
    if (!gnc_commodity_equal(xaccAccountGetCommodity(acc), xaccAccountGetCommodity(post)))
    {
        XferDialog* xfer;

        text = _("The transfer and post accounts are associated with different currencies.  Please specify the conversion rate.");

        xfer = gnc_xfer_dialog(pw->dialog, acc);
        gnc_info_dialog(pw->dialog, "%s", text);

        gnc_xfer_dialog_select_to_account(xfer,post);
        gnc_xfer_dialog_set_amount(xfer, amount);
        
        /* All we want is the exchange rate so prevent the user from thinking 
           it makes sense to mess with other stuff */
        gnc_xfer_dialog_set_from_show_button_active(xfer, FALSE);
        gnc_xfer_dialog_set_to_show_button_active(xfer, FALSE);
        gnc_xfer_dialog_hide_from_account_tree(xfer);
        gnc_xfer_dialog_hide_to_account_tree(xfer);
        gnc_xfer_dialog_is_exchange_dialog(xfer, &exch);
        gnc_xfer_dialog_run_until_done(xfer);
    }
        
    /* Now apply the payment */
    gncOwnerApplyPayment (&pw->owner, pw->invoice,
			  post, acc, amount, exch, date, memo, num);                    
    
  }
  gnc_resume_gui_refresh ();

  /* Save the transfer account, acc */
  gnc_payment_dialog_remember_account(pw, acc);

  gnc_ui_payment_window_destroy (pw);
}

void
gnc_payment_cancel_cb (GtkWidget *widget, gpointer data)
{
  PaymentWindow *pw = data;
  gnc_ui_payment_window_destroy (pw);
}

void
gnc_payment_window_destroy_cb (GtkWidget *widget, gpointer data)
{
  PaymentWindow *pw = data;

  if (!pw) return;

  gnc_unregister_gui_component (pw->component_id);

  g_list_free (pw->acct_types);
  g_list_free (pw->acct_commodities);
  g_free (pw);
}

/* Select the list of accounts to show in the tree */
static void
gnc_payment_set_account_types (GncTreeViewAccount *tree)
{
  AccountViewInfo avi;
  int i;

  gnc_tree_view_account_get_view_info (tree, &avi);

  for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
    switch (i) {
    case ACCT_TYPE_BANK:
    case ACCT_TYPE_CASH:
    case ACCT_TYPE_CREDIT:
    case ACCT_TYPE_ASSET:
    case ACCT_TYPE_LIABILITY:
      avi.include_type[i] = TRUE;
      break;
    default:
      avi.include_type[i] = FALSE;
      break;
    }

  gnc_tree_view_account_set_view_info (tree, &avi);
}

static gboolean
find_handler (gpointer find_data, gpointer user_data)
{
  PaymentWindow *pw = user_data;

  return (pw != NULL);
}

static PaymentWindow *
new_payment_window (GncOwner *owner, GNCBook *book, GncInvoice *invoice)
{
  PaymentWindow *pw;
  GladeXML *xml;
  GtkWidget *box, *label;
  char * cm_class = (gncOwnerGetType (owner) == GNC_OWNER_CUSTOMER ?
		     DIALOG_PAYMENT_CUSTOMER_CM_CLASS :
		     DIALOG_PAYMENT_VENDOR_CM_CLASS);

  /*
   * Find an existing payment window.  If found, bring it to
   * the front.  If we have an actual owner, then set it in
   * the window.
   */

  pw = gnc_find_first_gui_component (cm_class, find_handler, NULL);
  if (pw) {
    if (owner->owner.undefined)
      gnc_payment_set_owner (pw, owner);

    gtk_window_present (GTK_WINDOW(pw->dialog));
    return(pw);
  }

  /* Ok, we need a new window */

  pw = g_new0 (PaymentWindow, 1);
  pw->book = book;
  gncOwnerCopy (owner, &(pw->owner));

  /* Compute the post-to account types */
  pw->acct_types = gnc_business_account_types (owner);

  pw->acct_commodities = gnc_business_commodities (owner);

  /* Open and read the XML */
  xml = gnc_glade_xml_new ("payment.glade", "Payment Dialog");
  pw->dialog = glade_xml_get_widget (xml, "Payment Dialog");

  /* Grab the widgets and build the dialog */
  pw->num_entry = glade_xml_get_widget (xml, "num_entry");
  pw->memo_entry = glade_xml_get_widget (xml, "memo_entry");
  pw->post_combo = glade_xml_get_widget (xml, "post_combo");
  gnc_cbe_require_list_item(GTK_COMBO_BOX_ENTRY(pw->post_combo));

  label = glade_xml_get_widget (xml, "owner_label");
  box = glade_xml_get_widget (xml, "owner_box");
  pw->owner_choice = gnc_owner_select_create (label, box, book, owner);

  label = glade_xml_get_widget (xml, "invoice_label");
  box = glade_xml_get_widget (xml, "invoice_box");
  pw->invoice_choice = gnc_invoice_select_create (box, book, owner, invoice, label);

  box = glade_xml_get_widget (xml, "amount_box");
  pw->amount_edit = gnc_amount_edit_new ();
  gtk_box_pack_start (GTK_BOX (box), pw->amount_edit, TRUE, TRUE, 0);
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (pw->amount_edit),
					 TRUE);
  gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (pw->amount_edit), gnc_numeric_zero());

  box = glade_xml_get_widget (xml, "date_box");
  pw->date_edit = gnc_date_edit_new (time(NULL), FALSE, FALSE);
  gtk_box_pack_start (GTK_BOX (box), pw->date_edit, TRUE, TRUE, 0);

  box = glade_xml_get_widget (xml, "acct_window");
  pw->acct_tree = GTK_WIDGET(gnc_tree_view_account_new (FALSE));
  gtk_container_add (GTK_CONTAINER (box), pw->acct_tree);
  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(pw->acct_tree), FALSE);
  gnc_payment_set_account_types (GNC_TREE_VIEW_ACCOUNT (pw->acct_tree));

  /* Set the dialog for the 'new' owner */
  gnc_payment_dialog_owner_changed(pw);

  /* Set the dialog for the 'new' invoice */
  pw->invoice = invoice;
  gnc_payment_dialog_invoice_changed(pw);

  /* Setup signals */
  glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
                                     pw);

  g_signal_connect (G_OBJECT (pw->owner_choice), "changed",
		    G_CALLBACK (gnc_payment_dialog_owner_changed_cb), pw);

  g_signal_connect (G_OBJECT (pw->invoice_choice), "changed",
		    G_CALLBACK (gnc_payment_dialog_invoice_changed_cb), pw);

  /* Register with the component manager */
  pw->component_id =
    gnc_register_gui_component (cm_class,
				gnc_payment_window_refresh_handler,
				gnc_payment_window_close_handler,
				pw);

  /* Watch for any new or changed accounts */
  gnc_gui_component_watch_entity_type (pw->component_id,
				       GNC_ID_ACCOUNT,
				       QOF_EVENT_CREATE | QOF_EVENT_MODIFY | 
				       QOF_EVENT_DESTROY);

  /* Fill in the post_combo and account_tree widgets */
  gnc_fill_account_select_combo (pw->post_combo, pw->book, pw->acct_types, pw->acct_commodities);
  /* Show it all */
  gtk_widget_show_all (pw->dialog);

  /* Warn the user if they have no valid post-to accounts */
  {
    const gchar *text;
    const char *acct_type;

    text = gtk_combo_box_get_active_text(GTK_COMBO_BOX(pw->post_combo));
    if (!text || safe_strcmp (text, "") == 0) {
  
      /* XXX: I know there's only one type here */
      acct_type = xaccAccountGetTypeStr(GPOINTER_TO_INT(pw->acct_types->data));
      gnc_warning_dialog(pw->dialog,
			 _("You have no valid \"Post To\" accounts.  "
			   "Please create an account of type \"%s\" "
			   "before you continue to process this payment.  "
			   "Perhaps you want to create an Invoice or "
			   "Bill first?"),
			 acct_type);
    }
  }

  return pw;
}


void
gnc_ui_payment_window_destroy (PaymentWindow *pw)
{
  if (!pw) return;
  gnc_close_gui_component (pw->component_id);
}

PaymentWindow *
gnc_ui_payment_new_with_invoice (GncOwner *owner, GNCBook *book,
				 GncInvoice *invoice)
{
  GncOwner owner_def;

  if (!book) return NULL;
  if (owner) {
    /* Figure out the company */
    owner = gncOwnerGetEndOwner (owner);
  } else {
    gncOwnerInitCustomer (&owner_def, NULL);
    owner = &owner_def;
  }

  return new_payment_window (owner, book, invoice);
}

PaymentWindow *
gnc_ui_payment_new (GncOwner *owner, GNCBook *book)
{
  return gnc_ui_payment_new_with_invoice (owner, book, NULL);
}

