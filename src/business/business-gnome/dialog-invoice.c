/*
 * dialog-invoice.c -- Dialog for Invoice entry
 * Copyright (C) 2001,2002 Derek Atkins
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
#include "gnucash-sheet.h"
#include "window-help.h"
#include "dialog-search.h"
#include "search-param.h"

#include "gncInvoice.h"
#include "gncInvoiceP.h"

#include "gncEntryLedger.h"

#include "dialog-invoice.h"
#include "business-utils.h"
#include "dialog-date-close.h"

#define DIALOG_NEW_INVOICE_CM_CLASS "dialog-new-invoice"
#define DIALOG_EDIT_INVOICE_CM_CLASS "dialog-edit-invoice"
#define DIALOG_VIEW_INVOICE_CM_CLASS "dialog-view-invoice"

typedef enum
{
  NEW_INVOICE,
  EDIT_INVOICE,
  VIEW_INVOICE
} InvoiceDialogType;

struct _invoice_select_window {
  GNCBook *	book;
  GtkWidget *	parent;
  GncOwner *	owner;
  gboolean	no_close;
};

typedef struct _invoice_window {
  GladeXML *	xml;

  GtkWidget *	dialog;

  GtkWidget *	id_entry;
  GtkWidget *	terms_entry;
  GtkWidget *	notes_text;
  GtkWidget *	opened_date;
  GtkWidget *	active_check;

  GtkWidget *	owner_box;
  GtkWidget *	owner_label;
  GtkWidget *	owner_choice;

  GnucashRegister *	reg;
  GncEntryLedger *	ledger;

  InvoiceDialogType	dialog_type;
  GUID		invoice_guid;
  gint		component_id;
  GNCBook *	book;
  GncInvoice *	created_invoice;
  GncOwner	owner;

} InvoiceWindow;

static void gnc_invoice_update_window (InvoiceWindow *iw);

static GncInvoice *
iw_get_invoice (InvoiceWindow *iw)
{
  if (!iw)
    return NULL;

  return gncInvoiceLookup (iw->book, &iw->invoice_guid);
}

static void gnc_ui_to_invoice (InvoiceWindow *iw, GncInvoice *invoice)
{
  Timespec ts;
  time_t tt;

  if (iw->dialog_type == VIEW_INVOICE)
    return;

  gnc_suspend_gui_refresh ();
  
  gncInvoiceSetID (invoice, gtk_editable_get_chars
		 (GTK_EDITABLE (iw->id_entry), 0, -1));
  gncInvoiceSetNotes (invoice, gtk_editable_get_chars
		    (GTK_EDITABLE (iw->notes_text), 0, -1));
  gncInvoiceSetTerms (invoice, gtk_editable_get_chars
		    (GTK_EDITABLE (iw->terms_entry), 0, -1));

  tt = gnome_date_edit_get_date (GNOME_DATE_EDIT (iw->opened_date));
  timespecFromTime_t (&ts, tt);
  gncInvoiceSetDateOpened (invoice, &ts);

  if (iw->active_check)
    gncInvoiceSetActive (invoice, gtk_toggle_button_get_active
			 (GTK_TOGGLE_BUTTON (iw->active_check)));

  gnc_owner_get_owner (iw->owner_choice, &(iw->owner));
  gncInvoiceSetOwner (invoice, &(iw->owner));

  gncInvoiceCommitEdit (invoice);
  gnc_resume_gui_refresh ();
}

static gboolean
gnc_invoice_window_verify_ok (InvoiceWindow *iw)
{
  const char *res;

  /* save the current entry in the ledger? */
  if (!gnc_entry_ledger_check_close (iw->dialog, iw->ledger))
    return FALSE;

  /* Check the ID */
  res = gtk_entry_get_text (GTK_ENTRY (iw->id_entry));
  if (safe_strcmp (res, "") == 0) {
    gnc_error_dialog_parented (GTK_WINDOW (iw->dialog),
			       _("The Invoice must be given an ID."));
    return FALSE;
  }

  /* Check the Owner */
  gnc_owner_get_owner (iw->owner_choice, &(iw->owner));
  res = gncOwnerGetName (&(iw->owner));
  if (res == NULL || safe_strcmp (res, "") == 0) {
    gnc_error_dialog_parented (GTK_WINDOW (iw->dialog),
  			       _("You need to supply Billing Information."));
    return FALSE;
  }

  return TRUE;
}

static gboolean
gnc_invoice_window_ok_save (InvoiceWindow *iw)
{
  if (!gnc_invoice_window_verify_ok (iw))
    return FALSE;

  {
    GncInvoice *invoice = iw_get_invoice (iw);
    if (invoice) {
      gnc_ui_to_invoice (iw, invoice);
    }
    /* Save the invoice to return it later. */
    iw->created_invoice = invoice;
  }
  return TRUE;
}

static void
gnc_invoice_window_ok_cb (GtkWidget *widget, gpointer data)
{
  InvoiceWindow *iw = data;

  if (!gnc_invoice_window_ok_save (iw))
    return;

  /* Ok, we don't need this anymore */
  iw->invoice_guid = *xaccGUIDNULL ();

  gnc_close_gui_component (iw->component_id);
}

static void
gnc_invoice_window_cancel_cb (GtkWidget *widget, gpointer data)
{
  InvoiceWindow *iw = data;

  gnc_close_gui_component (iw->component_id);
}

static void
gnc_invoice_window_help_cb (GtkWidget *widget, gpointer data)
{
  char *help_file = "";		/* XXX: FIXME */

  helpWindow(NULL, NULL, help_file);
}

static void
gnc_invoice_window_print_invoice_cb (GtkWidget *widget, gpointer data)
{
  /* XXX: print this invoice (TBD) */

  fprintf (stderr, "I would print an invoice now... \n");
}

static void
gnc_invoice_window_post_invoice_cb (GtkWidget *widget, gpointer data)
{
  InvoiceWindow *iw = data;
  GncInvoice *invoice;
  char *message, *ddue_label, *post_label, *acct_label;
  Account *acc = NULL;
  GList * acct_types = NULL;
  Timespec ddue, postdate;
  gboolean reverse = FALSE;

  /* Make sure the invoice is ok */
  if (!gnc_invoice_window_verify_ok (iw))
      return;

  invoice = iw_get_invoice (iw);
  if (!invoice)
    return;

  /* Check that there is at least one Entry */
  invoice = iw_get_invoice (iw);
  if (gncInvoiceGetEntries (invoice) == NULL) {
    gnc_error_dialog_parented (GTK_WINDOW (iw->dialog),
			       _("The Invoice must have at least one Entry."));
    return;
  }

  /* Ok, we can post this invoice.  Ask for verification, set the due date,
   * post date, and posted account
   */
  message = _("Do you really want to post the invoice?");
  ddue_label = _("Due Date");
  post_label = _("Post Date");
  acct_label = _("Post to Account");

  /* Determine the type of account to post to */
  switch (gncOwnerGetType (&(iw->owner))) {
  case GNC_OWNER_CUSTOMER:
    acct_types = g_list_prepend (NULL, (gpointer)RECEIVABLE);
    reverse = TRUE;
    break;
  case GNC_OWNER_VENDOR:
    acct_types = g_list_prepend (NULL, (gpointer)PAYABLE);
    break;
  default:
    acct_types = g_list_prepend (NULL, (gpointer)NO_TYPE);
  }

  /* Get the due date and posted account */
  timespecFromTime_t (&postdate, time(NULL));
  ddue = postdate;
  ddue.tv_sec += 3600*24*30;	/* XXX: due in 30 days */
  if (!gnc_dialog_dates_acct_parented (iw->dialog, message, ddue_label,
				      post_label, acct_label, TRUE, acct_types,
				      iw->book, &ddue, &postdate, &acc))
    return;

  /* Save the Due Date */
  gncInvoiceSetDateDue (invoice, &ddue);

  /* Yep, we're posting.  So, save the invoice... 
   * Note that we can safely ignore the return value; we checked
   * the verify_ok earlier, so we know it's ok.
   */
  gnc_invoice_window_ok_save (iw);

  /* ... post it; post date is set to now ... */
  gncInvoicePostToAccount (invoice, acc, &postdate, reverse);

  /* Reset the type; change to read-only! */
  iw->dialog_type = VIEW_INVOICE;
  gnc_entry_ledger_set_readonly (iw->ledger);

  /* ... and redisplay here. */
  gnc_invoice_update_window (iw);
}

static void
gnc_invoice_window_pay_invoice_cb (GtkWidget *widget, gpointer data)
{
  InvoiceWindow *iw = data;
  GncInvoice *invoice;
  char *message, *date_label, *acct_label;
  GList *acct_types;
  Timespec paid_date;
  Account *acc = NULL;

  /* Note that this assumes we're already posted! Should we check?
   * There shuldn't be any way to get to this callback unless we're
   * already posted.
   */

  invoice = iw_get_invoice (iw);
  if (!invoice)
    return;

  message = _("Do you really want to pay the invoice?");
  date_label = _("Date Paid");
  acct_label = _("Pay to Account");

  /* Add "appropriate" accounts */
  acct_types = g_list_prepend (NULL, (gpointer)BANK);
  acct_types = g_list_prepend (acct_types, (gpointer)CASH);
  acct_types = g_list_prepend (acct_types, (gpointer)ASSET);
  acct_types = g_list_prepend (acct_types, (gpointer)LIABILITY);

  timespecFromTime_t (&paid_date, time(NULL));
  if (!gnc_dialog_date_acct_parented (iw->dialog, message, date_label,
				      acct_label, TRUE, acct_types,
				      iw->book, &paid_date, &acc))
    return;

  /* Pay to the account now */
  gncInvoicePayToAccount (invoice, acc, &paid_date);

  /* ... and redisplay here. */
  gnc_invoice_update_window (iw);
}

static void
gnc_invoice_window_destroy_cb (GtkWidget *widget, gpointer data)
{
  InvoiceWindow *iw = data;
  GncInvoice *invoice = iw_get_invoice (iw);

  gnc_suspend_gui_refresh ();

  if (iw->dialog_type == NEW_INVOICE && invoice != NULL) {
    gncInvoiceDestroy (invoice);
    iw->invoice_guid = *xaccGUIDNULL ();
  }

  gnc_entry_ledger_destroy (iw->ledger);
  gnc_unregister_gui_component (iw->component_id);
  gnc_resume_gui_refresh ();

  g_free (iw);
}

static int
gnc_invoice_owner_changed_cb (GtkWidget *widget, gpointer data)
{
  InvoiceWindow *iw = data;
  GncInvoice *invoice;

  if (!iw)
    return FALSE;

  if (iw->dialog_type == VIEW_INVOICE)
    return FALSE;

  gnc_owner_get_owner (iw->owner_choice, &(iw->owner));
  invoice = iw_get_invoice (iw);
  gncInvoiceSetOwner (invoice, &(iw->owner));
  gnc_entry_ledger_reset_query (iw->ledger);

  return FALSE;
}

static int
gnc_invoice_on_close_cb (GnomeDialog *dialog, gpointer data)
{
  InvoiceWindow *iw;
  GncInvoice **created_invoice = data;

  if (data) {
    iw = gtk_object_get_data (GTK_OBJECT (dialog), "dialog_info");
    *created_invoice = iw->created_invoice;
  }

  gtk_main_quit ();

  return FALSE;
}

static void
gnc_invoice_window_close_handler (gpointer user_data)
{
  InvoiceWindow *iw = user_data;

  gnome_dialog_close (GNOME_DIALOG (iw->dialog));
}

static void
gnc_invoice_window_refresh_handler (GHashTable *changes, gpointer user_data)
{
  InvoiceWindow *iw = user_data;
  const EventInfo *info;
  GncInvoice *invoice = iw_get_invoice (iw);

 /* If there isn't a invoice behind us, close down */
  if (!invoice) {
    gnc_close_gui_component (iw->component_id);
    return;
  }

  /* Next, close if this is a destroy event */
  if (changes) {
    info = gnc_gui_get_entity_events (changes, &iw->invoice_guid);
    if (info && (info->event_mask & GNC_EVENT_DESTROY)) {
      gnc_close_gui_component (iw->component_id);
      return;
    }
  }
}

static void
gnc_configure_register_colors (void)
{
  GncEntryLedgerColors reg_colors;

  reg_colors.header_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Header color",
                                 0xffffff);

  reg_colors.primary_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Primary color",
                                 0xffffff);

  reg_colors.secondary_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Secondary color",
                                 0xffffff);

  reg_colors.primary_active_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Primary active color",
                                 0xffffff);

  reg_colors.secondary_active_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Secondary active color",
                                 0xffffff);

  gnc_entry_ledger_set_colors (reg_colors);
}

static void
gnc_invoice_update_window (InvoiceWindow *iw)
{
  GtkWidget *paid_date, *posted_date, *due_date, *acct_entry;
  GncInvoice *invoice;
  GncOwner *owner;
  gboolean is_posted = FALSE;
  gboolean is_paid = FALSE;

  invoice = iw_get_invoice (iw);
  owner = gncInvoiceGetOwner (invoice);

  if (iw->owner_choice) {
    gtk_container_remove (GTK_CONTAINER (iw->owner_box), iw->owner_choice);
    gtk_object_destroy (GTK_OBJECT (iw->owner_choice));
  }

  switch (iw->dialog_type) {
  case VIEW_INVOICE:
  case EDIT_INVOICE:
    iw->owner_choice =
      gnc_owner_edit_create (iw->owner_label, iw->owner_box, iw->book,
			     owner);
    break;
  case NEW_INVOICE:
    iw->owner_choice =
      gnc_owner_select_create (iw->owner_label, iw->owner_box, iw->book,
			       owner);
    break;
  }

  gtk_signal_connect (GTK_OBJECT (iw->owner_choice), "changed",
		      GTK_SIGNAL_FUNC (gnc_invoice_owner_changed_cb),
		      iw);

  gtk_widget_show_all (iw->dialog);

  paid_date = glade_xml_get_widget (iw->xml, "paid_date");
  posted_date = glade_xml_get_widget (iw->xml, "posted_date");
  due_date = glade_xml_get_widget (iw->xml, "due_date");
  acct_entry = glade_xml_get_widget (iw->xml, "acct_entry");

  /* We know that "invoice" (and "owner") exist now */
  do {
    const char *string;
    Timespec ts, ts_zero = {0,0};
    Account *acct;
    time_t tt;
    gint pos = 0;

    gtk_entry_set_text (GTK_ENTRY (iw->terms_entry),
			gncInvoiceGetTerms (invoice));

    string = gncInvoiceGetNotes (invoice);
    gtk_editable_delete_text (GTK_EDITABLE (iw->notes_text), 0, -1);
    gtk_editable_insert_text (GTK_EDITABLE (iw->notes_text), string,
			      strlen (string), &pos);

    if (iw->active_check)
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (iw->active_check),
				    gncInvoiceGetActive (invoice));

    ts = gncInvoiceGetDateOpened (invoice);
    if (timespec_equal (&ts, &ts_zero)) {
      tt = time(NULL);
    } else {
      tt = ts.tv_sec;		/* XXX */
    }
    gnome_date_edit_set_time (GNOME_DATE_EDIT (iw->opened_date), tt);

    /*
     * Next, figure out if we've been posted, and if so set the
     * appropriate bits of information.. Then work on hiding or
     * unhiding as necessary.
     */

    acct = gncInvoiceGetPostedAcc (invoice);
    if (!acct)
      break;

    /* Ok, it's definitely posted. Setup the 'posted-invoice' fields now */
    is_posted = TRUE;
    
    ts = gncInvoiceGetDateDue (invoice);
    gnome_date_edit_set_time (GNOME_DATE_EDIT (due_date), ts.tv_sec);

    ts = gncInvoiceGetDatePosted (invoice);
    gnome_date_edit_set_time (GNOME_DATE_EDIT (posted_date), ts.tv_sec);

    string = xaccAccountGetFullName (acct, gnc_get_account_separator ());
    gtk_entry_set_text (GTK_ENTRY (acct_entry), string);

    /*
     * Check if we're paid.  By paid, we'll have an actual
     * paid-date as well as everything else.  Note that you can be
     * posted without being paid, but not the reverse.
     */

    ts = gncInvoiceGetDatePaid (invoice);
    if (timespec_equal (&ts, &ts_zero))
      break;

    is_paid = TRUE;
    gnome_date_edit_set_time (GNOME_DATE_EDIT (paid_date), ts.tv_sec);

  } while (FALSE);

  if (iw->dialog_type == NEW_INVOICE)
    return;

  /* Hide/show the appropriate widgets based on our posted/paid state */

  if (is_paid == FALSE) {
    GtkWidget *hide;

    hide = glade_xml_get_widget (iw->xml, "paid_label");
    gtk_widget_hide_all (hide);
    gtk_widget_hide_all (paid_date);

    if (is_posted == TRUE) {
      hide = glade_xml_get_widget (iw->xml, "hide1");
      gtk_widget_hide_all (hide);
      hide = glade_xml_get_widget (iw->xml, "hide2");
      gtk_widget_hide_all (hide);

    } else {			/* ! posted */
      hide = glade_xml_get_widget (iw->xml, "due_label");
      gtk_widget_hide_all (hide);
      gtk_widget_hide_all (due_date);

      hide = glade_xml_get_widget (iw->xml, "posted_label");
      gtk_widget_hide_all (hide);
      gtk_widget_hide_all (posted_date);

      hide = glade_xml_get_widget (iw->xml, "acct_label");
      gtk_widget_hide_all (hide);
      gtk_widget_hide_all (acct_entry);

      /* hide the close invoice button -- it needs to be posted first! */
      hide = glade_xml_get_widget (iw->xml, "pay_invoice_button");
      gtk_widget_hide_all (hide);

      /* Also hide the print invoice button, for the same reason */
      hide = glade_xml_get_widget (iw->xml, "print_invoice_button");
      gtk_widget_hide_all (hide);
    }
  }

  if (is_posted) {
    GtkWidget *hide;

    /* Setup viewer for read-only access */
    gtk_widget_set_sensitive (iw->id_entry, FALSE);
    gtk_widget_set_sensitive (iw->terms_entry, FALSE);
    gtk_widget_set_sensitive (iw->opened_date, FALSE);
    gtk_widget_set_sensitive (iw->notes_text, FALSE); /* XXX: should notes remain writable? */

    /* Hide the 'post invoice' button */
    hide = glade_xml_get_widget (iw->xml, "post_invoice_button");
    gtk_widget_hide_all (hide);

    if (is_paid) {
      hide = glade_xml_get_widget (iw->xml, "pay_invoice_button");
      gtk_widget_hide_all (hide);
    }
  }  

  gnc_table_refresh_gui (gnc_entry_ledger_get_table (iw->ledger), TRUE);
}

static InvoiceWindow *
gnc_invoice_new_window (GtkWidget *parent, GNCBook *bookp,
			InvoiceDialogType type, GncInvoice *invoice,
			GncOwner *owner)
{
  InvoiceWindow *iw;
  GladeXML *xml;
  GtkWidget *vbox, *regWidget;
  GncEntryLedger *entry_ledger = NULL;
  GnomeDialog *iwd;

  gnc_configure_register_colors ();

  iw = g_new0 (InvoiceWindow, 1);
  iw->book = bookp;
  iw->dialog_type = type;

  if (type == NEW_INVOICE) {
    invoice = gncInvoiceCreate (bookp);
    gncInvoiceSetOwner (invoice, owner);
  }

  /* Save this for later */
  gncOwnerCopy (owner, &(iw->owner));

  /* Find the dialog */
  iw->xml = xml = gnc_glade_xml_new ("invoice.glade", "Invoice Entry Dialog");
  iw->dialog = glade_xml_get_widget (xml, "Invoice Entry Dialog");
  iwd = GNOME_DIALOG (iw->dialog);

  gtk_object_set_data (GTK_OBJECT (iw->dialog), "dialog_info", iw);

  /* Grab the widgets */
  iw->id_entry = glade_xml_get_widget (xml, "id_entry");
  iw->terms_entry = glade_xml_get_widget (xml, "terms_entry");
  iw->notes_text = glade_xml_get_widget (xml, "notes_text");
  iw->opened_date = glade_xml_get_widget (xml, "opened_date");
  iw->active_check = glade_xml_get_widget (xml, "active_check");
  iw->owner_box = glade_xml_get_widget (xml, "owner_hbox");
  iw->owner_label = glade_xml_get_widget (xml, "owner_label");

  /* default to ok */
  gnome_dialog_editable_enters (iwd, GTK_EDITABLE (iw->id_entry));
  gnome_dialog_set_default (iwd, 0);

  /* Build the ledger */
  switch (type) {
  case EDIT_INVOICE:
    entry_ledger = gnc_entry_ledger_new (iw->book, GNCENTRY_INVOICE_ENTRY);
    break;
  case VIEW_INVOICE:
  default:
    entry_ledger = gnc_entry_ledger_new (iw->book, GNCENTRY_INVOICE_VIEWER);
    break;
  }

  /* Save the ledger... */
  iw->ledger = entry_ledger;

  /* Set the entry_ledger's invoice */
  gnc_entry_ledger_set_default_invoice (entry_ledger, invoice);

  //entries = gncInvoiceGetEntries (invoice);
  /* Set watches on entries */
  //gnc_entry_ledger_load (entry_ledger, entries);

  /* Watch the invoice of operations, here... */
  gnucash_register_set_initial_rows( 10 );
  regWidget = gnucash_register_new (gnc_entry_ledger_get_table (entry_ledger));
  gnc_table_init_gui( regWidget, entry_ledger );
  iw->reg = GNUCASH_REGISTER (regWidget);
  GNUCASH_SHEET (iw->reg->sheet)->window = GTK_WIDGET(iw->dialog);
  gnc_entry_ledger_set_parent (entry_ledger, iw->dialog);

  vbox = glade_xml_get_widget (xml, "ledger_vbox");
  // gtk_box_pack_start (GTK_BOX(vbox), toolbar, FALSE, FALSE, 2);
  gtk_box_pack_start (GTK_BOX(vbox), regWidget, TRUE, TRUE, 2);

  if (parent) {
    gnome_dialog_set_parent (iwd, GTK_WINDOW (parent));
    gtk_window_set_modal (GTK_WINDOW (iw->dialog), TRUE);
  }

  gtk_signal_connect (GTK_OBJECT (iw->dialog), "destroy",
		      GTK_SIGNAL_FUNC(gnc_invoice_window_destroy_cb), iw);

  gnome_dialog_button_connect (iwd, 0,
  			       GTK_SIGNAL_FUNC(gnc_invoice_window_ok_cb), iw);
  gnome_dialog_button_connect (iwd, 1,
  			       GTK_SIGNAL_FUNC(gnc_invoice_window_help_cb), iw);

  gnome_dialog_button_connect
    (iwd, 2, GTK_SIGNAL_FUNC(gnc_invoice_window_print_invoice_cb), iw);

  gnome_dialog_button_connect
    (iwd, 3, GTK_SIGNAL_FUNC(gnc_invoice_window_post_invoice_cb), iw);

  gnome_dialog_button_connect
    (iwd, 4, GTK_SIGNAL_FUNC(gnc_invoice_window_pay_invoice_cb), iw);

  /* Setup initial values */
  iw->invoice_guid = *gncInvoiceGetGUID (invoice);

  {
    char * class_name = NULL;

    switch (type) {
    case VIEW_INVOICE:
    default:
      class_name = DIALOG_VIEW_INVOICE_CM_CLASS;
      /* FALLTHROUGH */
    case EDIT_INVOICE:
      gtk_entry_set_text (GTK_ENTRY (iw->id_entry), gncInvoiceGetID (invoice));
      
      if (class_name == NULL)
	class_name = DIALOG_EDIT_INVOICE_CM_CLASS;
      break;
    }

    iw->component_id =
      gnc_register_gui_component (class_name,
				  gnc_invoice_window_refresh_handler,
				  gnc_invoice_window_close_handler,
				  iw);
  }

  gnc_gui_component_watch_entity_type (iw->component_id,
				       GNC_INVOICE_MODULE_NAME,
				       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  gnc_table_realize_gui (gnc_entry_ledger_get_table (entry_ledger));

  /* Now fill in a lot of the pieces and display properly */
  gnc_invoice_update_window (iw);

  return iw;
}

static InvoiceWindow *
gnc_invoice_window_new_invoice (GtkWidget *parent, GNCBook *bookp,
				GncOwner *owner)
{
  InvoiceWindow *iw;
  GladeXML *xml;
  GnomeDialog *iwd;
  GncInvoice *invoice;

  iw = g_new0 (InvoiceWindow, 1);
  iw->book = bookp;
  iw->dialog_type = NEW_INVOICE;

  invoice = gncInvoiceCreate (bookp);
  gncInvoiceSetOwner (invoice, owner);

  /* Save this for later */
  gncOwnerCopy (owner, &(iw->owner));

  /* Find the dialog */
  iw->xml = xml = gnc_glade_xml_new ("invoice.glade", "New Invoice Dialog");
  iw->dialog = glade_xml_get_widget (xml, "New Invoice Dialog");
  iwd = GNOME_DIALOG (iw->dialog);

  gtk_object_set_data (GTK_OBJECT (iw->dialog), "dialog_info", iw);

  /* Grab the widgets */
  iw->id_entry = glade_xml_get_widget (xml, "id_entry");
  iw->terms_entry = glade_xml_get_widget (xml, "terms_entry");
  iw->notes_text = glade_xml_get_widget (xml, "notes_text");
  iw->opened_date = glade_xml_get_widget (xml, "opened_date");
  iw->owner_box = glade_xml_get_widget (xml, "owner_hbox");
  iw->owner_label = glade_xml_get_widget (xml, "owner_label");

  /* default to ok */
  gnome_dialog_editable_enters (iwd, GTK_EDITABLE (iw->id_entry));
  gnome_dialog_set_default (iwd, 0);

  if (parent) {
    gnome_dialog_set_parent (iwd, GTK_WINDOW (parent));
    gtk_window_set_modal (GTK_WINDOW (iw->dialog), TRUE);
  }

  gtk_signal_connect (GTK_OBJECT (iw->dialog), "destroy",
		      GTK_SIGNAL_FUNC(gnc_invoice_window_destroy_cb), iw);

  gnome_dialog_button_connect (iwd, 0,
  			       GTK_SIGNAL_FUNC(gnc_invoice_window_ok_cb), iw);
  gnome_dialog_button_connect (iwd, 1,
  			       GTK_SIGNAL_FUNC(gnc_invoice_window_cancel_cb), iw);
  gnome_dialog_button_connect (iwd, 2,
  			       GTK_SIGNAL_FUNC(gnc_invoice_window_help_cb), iw);

  /* Setup initial values */
  iw->invoice_guid = *gncInvoiceGetGUID (invoice);

  gtk_entry_set_text (GTK_ENTRY (iw->id_entry),
		      g_strdup_printf ("%.6d", gncInvoiceNextID(bookp)));
      
  iw->component_id =
    gnc_register_gui_component (DIALOG_NEW_INVOICE_CM_CLASS,
				gnc_invoice_window_refresh_handler,
				gnc_invoice_window_close_handler,
				iw);

  gnc_gui_component_watch_entity_type (iw->component_id,
				       GNC_INVOICE_MODULE_NAME,
				       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  /* Now fill in a lot of the pieces and display properly */
  gnc_invoice_update_window (iw);

  return iw;
}

GncInvoice *
gnc_invoice_new (GtkWidget *parent, GncOwner *ownerp, GNCBook *bookp)
{
  InvoiceWindow *iw;
  GncInvoice *created_invoice = NULL;
  GncOwner owner;
  gboolean repeat;

  if (ownerp) {
    do {
      repeat = FALSE;

      switch (gncOwnerGetType (ownerp)) {
      case GNC_OWNER_CUSTOMER:
      case GNC_OWNER_VENDOR:
	gncOwnerCopy (ownerp, &owner);
	break;
      case GNC_OWNER_JOB:
	ownerp = gncJobGetOwner (gncOwnerGetJob (ownerp));
	repeat = TRUE;
	break;
      default:
	g_warning ("Cannot deal with unknown Owner types");
	return NULL;
      }
    } while (repeat);
  } else
    gncOwnerInitCustomer (&owner, NULL); /* XXX: pass in the owner type? */

  /* Make sure required options exist */
  if (!bookp) return NULL;

  iw = gnc_invoice_window_new_invoice (parent, bookp, &owner);

  gtk_signal_connect (GTK_OBJECT (iw->dialog), "close",
		      GTK_SIGNAL_FUNC (gnc_invoice_on_close_cb),
		      &created_invoice);

  gtk_main ();

  if (created_invoice)
    gnc_invoice_edit (parent, created_invoice);

  return created_invoice;
}

void
gnc_invoice_edit (GtkWidget *parent, GncInvoice *invoice)
{
  InvoiceWindow *iw;
  InvoiceDialogType type;

  if (!invoice) return;

  /* Immutable once we've been posted */
  if (gncInvoiceGetPostedAcc (invoice))
    type = VIEW_INVOICE;
  else
    type = EDIT_INVOICE;

  iw = gnc_invoice_new_window (parent, gncInvoiceGetBook(invoice), type, invoice, 
			     gncInvoiceGetOwner (invoice));

  gtk_signal_connect (GTK_OBJECT (iw->dialog), "close",
		      GTK_SIGNAL_FUNC (gnc_invoice_on_close_cb),
		      NULL);

  gtk_main ();

  return;
}

/* Functions for invoice selection widgets */

static gboolean
edit_invoice_cb (gpointer *invoice_p, gpointer user_data)
{
  struct _invoice_select_window *sw = user_data;
  GncInvoice *invoice;

  g_return_val_if_fail (invoice_p && user_data, TRUE);

  invoice = *invoice_p;

  if (!invoice)
    return TRUE;

  gnc_invoice_edit (sw->parent, invoice);
  return TRUE;
}

static gboolean
select_invoice_cb (gpointer *invoice_p, gpointer user_data)
{
  g_return_val_if_fail (invoice_p && user_data, TRUE);
  if (*invoice_p)
    return FALSE;
  return TRUE;
}

static gboolean
new_invoice_cb (GtkWidget *parent, gpointer *invoice_p, gpointer user_data)
{
  struct _invoice_select_window *sw = user_data;
  
  g_return_val_if_fail (invoice_p && user_data, TRUE);

  *invoice_p = gnc_invoice_new (parent, sw->owner, sw->book);
  return sw->no_close;
}

static GncInvoice *
gnc_invoice_select (GtkWidget *parent, GncInvoice *start, GncOwner *owner,
		    GNCBook *book, gboolean provide_select)
{
  static GList *params = NULL;
  gpointer res;
  QueryNew *q, *q2 = NULL;
  GNCSearchCallbackButton buttons[] = { 
    { N_("Select Invoice"), select_invoice_cb},
    { N_("View/Edit Invoice"), edit_invoice_cb},
    { NULL },
  };
  GNCIdType type = GNC_INVOICE_MODULE_NAME;
  struct _invoice_select_window sw;

  g_return_val_if_fail (book, NULL);

  /* Build parameter list in reverse invoice*/
  if (params == NULL) {
    params = gnc_search_param_prepend (params, _("Invoice Notes"), NULL, type,
				       INVOICE_NOTES, NULL);
    params = gnc_search_param_prepend (params, _("Date Paid"), NULL, type,
				       INVOICE_PAID, NULL);
    params = gnc_search_param_prepend (params, _("Is Paid?"), NULL, type,
				       INVOICE_IS_PAID, NULL);
    params = gnc_search_param_prepend (params, _("Date Due"), NULL, type,
				       INVOICE_DUE, NULL);
    params = gnc_search_param_prepend (params, _("Date Posted"), NULL, type,
				       INVOICE_POSTED, NULL);
    params = gnc_search_param_prepend (params, _("Is Posted?"), NULL, type,
				       INVOICE_IS_POSTED, NULL);
    params = gnc_search_param_prepend (params, _("Date Opened"), NULL, type,
				       INVOICE_OPENED, NULL);
    params = gnc_search_param_prepend (params, _("Owner Name "), NULL, type,
				       INVOICE_OWNER, OWNER_NAME, NULL);
    params = gnc_search_param_prepend (params, _("Invoice ID"), NULL, type,
				       INVOICE_ID, NULL);
  }

  /* Build the queries */
  q = gncQueryCreate ();
  gncQuerySetBook (q, book);

  /* If owner is supplied, limit all searches to invoices who's owner
   * is the supplied owner!  Show all invoices by this owner.
   */
  if (owner && gncOwnerGetGUID (owner)) {
    gncQueryAddGUIDMatch (q, g_slist_prepend
			  (g_slist_prepend (NULL, OWNER_GUID),
			   INVOICE_OWNER),
			  gncOwnerGetGUID (owner), QUERY_AND);

    q2 = gncQueryCopy (q);
  }

  if (start) {
    if (q2 == NULL)
      q2 = gncQueryCopy (q);

    gncQueryAddGUIDMatch (q2, g_slist_prepend (NULL, INVOICE_GUID),
			  gncInvoiceGetGUID (start), QUERY_AND);
  }

  /* launch select dialog and return the result */
  sw.book = book;
  sw.parent = parent;
  sw.owner = owner;
  sw.no_close = !provide_select;
  res = gnc_search_dialog_choose_object (parent, type, params, q, q2,
					 (provide_select ? buttons :
					  &(buttons[1])), NULL,
					 new_invoice_cb, &sw);

  gncQueryDestroy (q);
  return res;
}

void
gnc_invoice_find (GtkWidget *parent, GncInvoice *start, GncOwner *owner,
		GNCBook *book)
{
  gnc_invoice_select (parent, start, owner, book, FALSE);
}

GncInvoice *
gnc_invoice_choose (GtkWidget *parent, GncInvoice *start, GncOwner *owner,
		    GNCBook *book)
{
  return gnc_invoice_select (parent, start, owner, book, TRUE);
}

gpointer gnc_invoice_edit_new_select (gpointer bookp, gpointer invoice,
				       GtkWidget *toplevel)
{
  return gnc_invoice_choose (toplevel, invoice, NULL, bookp);
}

gpointer gnc_invoice_edit_new_edit (gpointer bookp, gpointer v,
				     GtkWidget *toplevel)
{
  GncInvoice *invoice = v;

  g_return_val_if_fail (invoice != NULL, NULL);

  gnc_invoice_edit (toplevel, invoice);
  return invoice;
}
