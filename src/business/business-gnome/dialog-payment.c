/*
 * dialog-payment.c -- Dialog for payment entry
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <gnome.h>

#include "dialog-utils.h"
#include "global-options.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "gnc-engine-util.h"
#include "gnc-date-edit.h"
#include "gnc-amount-edit.h"
#include "gnc-account-tree.h"
#include "Transaction.h"
#include "Account.h"
#include "gnc-numeric.h"

#include "dialog-payment.h"
#include "business-utils.h"

#define DIALOG_PAYMENT_CUSTOMER_CM_CLASS "customer-payment-dialog"
#define DIALOG_PAYMENT_VENDOR_CM_CLASS "vendor-payment-dialog"

struct _payment_window {
  GtkWidget *	dialog;

  GtkWidget *	num_entry;
  GtkWidget *	memo_entry;
  GtkWidget *	post_combo;
  GtkWidget *	owner_choice;
  GtkWidget *	amount_edit;
  GtkWidget *	date_edit;
  GtkWidget *	acct_tree;

  gint		component_id;
  GNCBook *	book;
  GncOwner	owner;
  GList *	acct_types;
};


static void
gnc_payment_window_refresh_handler (GHashTable *changes, gpointer data)
{
  PaymentWindow *pw = data;

  gnc_fill_account_select_combo (pw->post_combo, pw->book, pw->acct_types);
}

static void
gnc_payment_window_close_handler (gpointer data)
{
  PaymentWindow *pw = data;

  if (pw)
    gnome_dialog_close (GNOME_DIALOG (pw->dialog));
}

static void
gnc_payment_set_owner (PaymentWindow *pw, GncOwner *owner)
{
  gnc_owner_set_owner (pw->owner_choice, owner);
}

static void
gnc_payment_ok_cb (GtkWidget *widget, gpointer data)
{
  PaymentWindow *pw = data;
  char *text;
  Account *post, *acc;
  gnc_numeric amount;

  if (!pw)
    return;

  /* Verify the amount is non-zero */
  amount = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (pw->amount_edit));
  if (gnc_numeric_check (amount) || gnc_numeric_zero_p (amount)) {
    text = _("You must enter the amount of the payment.  "
	     "Payments may not be zero.");
    gnc_error_dialog_parented (GTK_WINDOW (pw->dialog), text);
    return;
  }

  /* Verify the user has selected an owner */
  gnc_owner_get_owner (pw->owner_choice, &(pw->owner));
  if (pw->owner.owner.undefined == NULL) {
    text = _("You must select a company for payment processing.");
    gnc_error_dialog_parented (GTK_WINDOW (pw->dialog), text);
    return;
  }

  /* Verify the user has selected a transfer account */
  acc = gnc_account_tree_get_current_account (GNC_ACCOUNT_TREE(pw->acct_tree));
  if (!acc) {
    text = _("You must select a transfer account from the account tree.");
    gnc_error_dialog_parented (GTK_WINDOW (pw->dialog), text);
    return;
  }

  /* Verify the "post" account */
  text = gtk_entry_get_text (GTK_ENTRY ((GTK_COMBO (pw->post_combo))->entry));
  if (!text || safe_strcmp (text, "") == 0) {
    text = _("You must enter an account name for posting.");
    gnc_error_dialog_parented (GTK_WINDOW (pw->dialog), text);
    return;
  }

  post = xaccGetAccountFromFullName (gnc_book_get_group (pw->book),
				     text, gnc_get_account_separator ());

  if (!post) {
    char *msg = g_strdup_printf (
			 _("Your selected post account, %s, does not exist"),
			 text);
    gnc_error_dialog_parented (GTK_WINDOW (pw->dialog), msg);
    g_free (msg);
    return;
  }

  /* Ok, now post the damn thing */
  {
    Transaction *txn;
    Split *split;
    char *memo, *num;
    const char *name;
    gnc_commodity *commodity;
    Timespec date;
    gboolean reverse;
    
    /* Obtain all our ancillary information */
    memo = gtk_entry_get_text (GTK_ENTRY (pw->memo_entry));
    num = gtk_entry_get_text (GTK_ENTRY (pw->num_entry));
    date = gnc_date_edit_get_date_ts (GNC_DATE_EDIT (pw->date_edit));
    name = gncOwnerGetName (gncOwnerGetEndOwner (&(pw->owner)));
    commodity = gncOwnerGetCommodity (&(pw->owner));
    reverse = (gncOwnerGetType (&(pw->owner)) == GNC_OWNER_CUSTOMER);
    
    txn = xaccMallocTransaction (pw->book);
    xaccTransBeginEdit (txn);

    /* Set up the transaction */
    xaccTransSetDescription (txn, name);
    xaccTransSetNum (txn, num);
    xaccTransSetCurrency (txn, commodity);
    xaccTransSetDateEnteredTS (txn, &date);
    xaccTransSetDatePostedTS (txn, &date);
    xaccTransSetTxnType (txn, TXN_TYPE_PAYMENT);

    /* The split for the transfer account */
    split = xaccMallocSplit (pw->book);
    xaccSplitSetMemo (split, memo);
    xaccSplitSetBaseValue (split, reverse ? amount :
			   gnc_numeric_neg (amount), commodity);
    xaccAccountBeginEdit (acc);
    xaccAccountInsertSplit (acc, split);
    xaccAccountCommitEdit (acc);
    xaccTransAppendSplit (txn, split);

    /* The split for the posting account */
    split = xaccMallocSplit (pw->book);
    xaccSplitSetMemo (split, memo);
    xaccSplitSetAction (split, _("Payment"));
    xaccSplitSetBaseValue (split, reverse ? gnc_numeric_neg (amount) :
			   amount, commodity);
    xaccAccountBeginEdit (post);
    xaccAccountInsertSplit (post, split);
    xaccAccountCommitEdit (post);
    xaccTransAppendSplit (txn, split);

    /*
     * Attach the OWNER to the transaction? Or, better yet, attach to
     * the proper lots from this owner in this post account
     */
    
    /* Commit it */
    xaccTransCommitEdit (txn);
  }

  gnc_ui_payment_window_destroy (pw);
}

static void
gnc_payment_cancel_cb (GtkWidget *widget, gpointer data)
{
  PaymentWindow *pw = data;
  gnc_ui_payment_window_destroy (pw);
}

static void
gnc_payment_window_destroy_cb (GtkWidget *widget, gpointer data)
{
  PaymentWindow *pw = data;

  if (!pw) return;

  gnc_unregister_gui_component (pw->component_id);

  g_list_free (pw->acct_types);
  g_free (pw);
}

/* Select the list of accounts to show in the tree */
static void
gnc_payment_set_account_types (GNCAccountTree *tree)
{
  AccountViewInfo avi;
  int i;

  gnc_account_tree_get_view_info (tree, &avi);

  for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
    switch (i) {
    case BANK:
    case CASH:
    case CREDIT:
    case ASSET:
    case LIABILITY:
      avi.include_type[i] = TRUE;
      break;
    default:
      avi.include_type[i] = FALSE;
      break;
    }

  gnc_account_tree_set_view_info (tree, &avi);
}

static gboolean
find_handler (gpointer find_data, gpointer user_data)
{
  PaymentWindow *pw = user_data;

  return (pw != NULL);
}

static PaymentWindow *
new_payment_window (GncOwner *owner, GNCBook *book)
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

  /* Open and read the XML */
  xml = gnc_glade_xml_new ("payment.glade", "Payment Dialog");
  pw->dialog = glade_xml_get_widget (xml, "Payment Dialog");

  /* Grab the widgets and build the dialog */
  pw->num_entry = glade_xml_get_widget (xml, "num_entry");
  pw->memo_entry = glade_xml_get_widget (xml, "memo_entry");
  pw->post_combo = glade_xml_get_widget (xml, "post_combo");

  label = glade_xml_get_widget (xml, "owner_label");
  box = glade_xml_get_widget (xml, "owner_box");
  pw->owner_choice = gnc_owner_select_create (label, box, book, owner);

  box = glade_xml_get_widget (xml, "amount_box");
  pw->amount_edit = gnc_amount_edit_new ();
  gtk_box_pack_start (GTK_BOX (box), pw->amount_edit, TRUE, TRUE, 0);
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (pw->amount_edit),
					 TRUE);

  box = glade_xml_get_widget (xml, "date_box");
  pw->date_edit = gnc_date_edit_new (time(NULL), FALSE, FALSE);
  gtk_box_pack_start (GTK_BOX (box), pw->date_edit, TRUE, TRUE, 0);

  box = glade_xml_get_widget (xml, "acct_window");
  pw->acct_tree = gnc_account_tree_new ();
  gtk_container_add (GTK_CONTAINER (box), pw->acct_tree);

  gtk_clist_column_titles_hide(GTK_CLIST(pw->acct_tree));
  gnc_account_tree_hide_all_but_name(GNC_ACCOUNT_TREE(pw->acct_tree));
  gnc_payment_set_account_types (GNC_ACCOUNT_TREE (pw->acct_tree));

  /* Connect the dialog buttons */
  gnome_dialog_button_connect (GNOME_DIALOG (pw->dialog), 0,
			       gnc_payment_ok_cb, pw);
  gnome_dialog_button_connect (GNOME_DIALOG (pw->dialog), 1,
			       gnc_payment_cancel_cb, pw);

  /* Setup various signal handlers */
  gtk_signal_connect (GTK_OBJECT (pw->dialog), "destroy",
		      gnc_payment_window_destroy_cb, pw);

  /* Register with the component manager */
  pw->component_id =
    gnc_register_gui_component (cm_class,
				gnc_payment_window_refresh_handler,
				gnc_payment_window_close_handler,
				pw);

  /* Watch for any new or changed accounts */
  gnc_gui_component_watch_entity_type (pw->component_id,
				       GNC_ID_ACCOUNT,
				       GNC_EVENT_CREATE | GNC_EVENT_MODIFY | 
				       GNC_EVENT_DESTROY);

  /* Fill in the post_combo and account_tree widgets */
  gnc_account_tree_refresh(GNC_ACCOUNT_TREE(pw->acct_tree));
  gnc_fill_account_select_combo (pw->post_combo, pw->book, pw->acct_types);

  /* Show it all */
  gtk_widget_show_all (pw->dialog);

  return pw;
}


void
gnc_ui_payment_window_destroy (PaymentWindow *pw)
{
  if (!pw) return;
  gnc_close_gui_component (pw->component_id);
}

PaymentWindow *
gnc_ui_payment_new (GncOwner *owner, GNCBook *book)
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

  return new_payment_window (owner, book);
}

