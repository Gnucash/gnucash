/*
 * dialog-vendor.c -- Dialog for Vendor entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <gnome.h>

#include "dialog-utils.h"
#include "global-options.h"
#include "gnc-amount-edit.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "gnc-engine-util.h"
#include "window-help.h"

#include "gncAddress.h"
#include "gncVendor.h"
#include "gncVendorP.h"

#include "dialog-vendor.h"
#include "business-chooser.h"

#define DIALOG_NEW_VENDOR_CM_CLASS "dialog-new-vendor"
#define DIALOG_EDIT_VENDOR_CM_CLASS "dialog-edit-vendor"

typedef enum
{
  NEW_VENDOR,
  EDIT_VENDOR
} VendorDialogType;

struct _vendor_select_window {
  GNCBook *	book;
};

typedef struct _vendor_window {
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

  GtkWidget *	terms_amount;

  GtkWidget *	active_check;
  GtkWidget *	taxincluded_check;
  GtkWidget *	notes_text;

  VendorDialogType	dialog_type;
  GUID		vendor_guid;
  gint		component_id;
  GNCBook *	book;
  GncVendor *	created_vendor;

} VendorWindow;

static GncVendor *
vw_get_vendor (VendorWindow *vw)
{
  if (!vw)
    return NULL;

  return gncVendorLookup (vw->book, &vw->vendor_guid);
}

static void gnc_ui_to_vendor (VendorWindow *vw, GncVendor *vendor)
{
  GncAddress *addr;
  gnc_numeric num;

  addr = gncVendorGetAddr (vendor);

  gnc_suspend_gui_refresh ();

  gncVendorSetID (vendor, gtk_editable_get_chars
		    (GTK_EDITABLE (vw->id_entry), 0, -1));
  gncVendorSetName (vendor, gtk_editable_get_chars
		      (GTK_EDITABLE (vw->company_entry), 0, -1));

  gncAddressSetName (addr, gtk_editable_get_chars
		     (GTK_EDITABLE (vw->name_entry), 0, -1));
  gncAddressSetAddr1 (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (vw->addr1_entry), 0, -1));
  gncAddressSetAddr2 (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (vw->addr2_entry), 0, -1));
  gncAddressSetAddr3 (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (vw->addr3_entry), 0, -1));
  gncAddressSetAddr4 (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (vw->addr4_entry), 0, -1));
  gncAddressSetPhone (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (vw->phone_entry), 0, -1));
  gncAddressSetFax (addr, gtk_editable_get_chars
		    (GTK_EDITABLE (vw->fax_entry), 0, -1));
  gncAddressSetEmail (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (vw->email_entry), 0, -1));

  gncVendorSetActive (vendor, gtk_toggle_button_get_active
			(GTK_TOGGLE_BUTTON (vw->active_check)));
  gncVendorSetTaxIncluded (vendor, gtk_toggle_button_get_active
			     (GTK_TOGGLE_BUTTON (vw->taxincluded_check)));
  gncVendorSetNotes (vendor, gtk_editable_get_chars
		       (GTK_EDITABLE (vw->notes_text), 0, -1));

  /* Parse and set the terms, discount, and credit amounts */
  num = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (vw->terms_amount));
  gncVendorSetTerms (vendor, gnc_numeric_num (num));

  gncVendorCommitEdit (vendor);
  gnc_resume_gui_refresh ();
}

static gboolean check_edit_amount (GtkWidget *dialog, GtkWidget *amount,
				   gnc_numeric *min, gnc_numeric *max,
				   const char * error_message)
{
  if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (amount))) {
    if (error_message)
      gnc_error_dialog_parented (GTK_WINDOW (dialog), error_message);
    return TRUE;
  }
  /* We've got a valid-looking number; check mix/max */
  if (min || max) {
    gnc_numeric val = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (amount));
    if ((min && gnc_numeric_compare (*min, val) > 0) ||
	(max && gnc_numeric_compare (val, *max) > 0)) {
      if (error_message)
	gnc_error_dialog_parented (GTK_WINDOW (dialog), error_message);
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
      gnc_error_dialog_parented (GTK_WINDOW (dialog), error_message);
    return TRUE;
  }
  return FALSE;
}

static void
gnc_vendor_window_ok_cb (GtkWidget *widget, gpointer data)
{
  VendorWindow *vw = data;
  char *res;
  GncVendor *vendor;
  gnc_numeric min, max;

  /* Check for valid id */
  if (check_entry_nonempty (vw->dialog, vw->id_entry,
			    _("The Vendor must be given an ID.")))
    return;

  /* Check for valid company name */
  if (check_entry_nonempty (vw->dialog, vw->company_entry,
		   _("You must enter a company name.")))
    return;

  /* Make sure we have an address */
  if (check_entry_nonempty (vw->dialog, vw->addr1_entry, NULL) &&
      check_entry_nonempty (vw->dialog, vw->addr2_entry, NULL) &&
      check_entry_nonempty (vw->dialog, vw->addr3_entry, NULL) &&
      check_entry_nonempty (vw->dialog, vw->addr4_entry, NULL)) {
    const char *msg = _("You must enter a payment address.");
    gnc_error_dialog_parented (GTK_WINDOW (vw->dialog), msg);
    return;
  }

  /* Verify terms are valid (or empty) */
  min = gnc_numeric_zero ();
  if (check_edit_amount (vw->dialog, vw->terms_amount, &min, NULL,
			 _("Terms must be a positive integer or "
			   "you must leave it blank.")))
    return;

  /* Now save it off */
  {
    GncVendor *vendor = vw_get_vendor (vw);
    if (vendor) {
      gnc_ui_to_vendor (vw, vendor);
    }
    vw->created_vendor = vendor;
    vw->vendor_guid = *xaccGUIDNULL ();
  }

  gnc_close_gui_component (vw->component_id);
}

static void
gnc_vendor_window_cancel_cb (GtkWidget *widget, gpointer data)
{
  VendorWindow *vw = data;

  gnc_close_gui_component (vw->component_id);
}

static void
gnc_vendor_window_help_cb (GtkWidget *widget, gpointer data)
{
  VendorWindow *vw = data;
  char *help_file = "";		/* xxx */

  helpWindow(NULL, NULL, help_file);
}

static void
gnc_vendor_window_destroy_cb (GtkWidget *widget, gpointer data)
{
  VendorWindow *vw = data;
  GncVendor *vendor = vw_get_vendor (vw);

  gnc_suspend_gui_refresh ();

  if (vw->dialog_type == NEW_VENDOR && vendor != NULL) {
    gncVendorDestroy (vendor);
    vw->vendor_guid = *xaccGUIDNULL ();
  }

  gnc_unregister_gui_component (vw->component_id);
  gnc_resume_gui_refresh ();

  g_free (vw);
}

static void
gnc_vendor_name_changed_cb (GtkWidget *widget, gpointer data)
{
  VendorWindow *vw = data;
  char *name, *id, *fullname, *title;

  if (!vw)
    return;

  name = gtk_entry_get_text (GTK_ENTRY (vw->name_entry));
  if (!name || *name == '\0')
    name = _("<No name>");

  id = gtk_entry_get_text (GTK_ENTRY (vw->id_entry));

  fullname = g_strconcat (name, " (", id, ")", NULL);

  if (vw->dialog_type == EDIT_VENDOR)
    title = g_strconcat (_("Edit Vendor"), " - ", fullname, NULL);
  else
    title = g_strconcat (_("New Vendor"), " - ", fullname, NULL);

  gtk_window_set_title (GTK_WINDOW (vw->dialog), title);

  g_free (fullname);
  g_free (title);
}

static int
gnc_vendor_on_close_cb (GnomeDialog *dialog, gpointer data)
{
  VendorWindow *vw;
  GncVendor **created_vendor = data;

  if (data) {
    vw = gtk_object_get_data (GTK_OBJECT (dialog), "dialog_info");
    *created_vendor = vw->created_vendor;
  }

  gtk_main_quit ();

  return FALSE;
}

static void
gnc_vendor_window_close_handler (gpointer user_data)
{
  VendorWindow *vw = user_data;

  gnome_dialog_close (GNOME_DIALOG (vw->dialog));
}

static void
gnc_vendor_window_refresh_handler (GHashTable *changes, gpointer user_data)
{
  VendorWindow *vw = user_data;
  const EventInfo *info;
  GncVendor *vendor = vw_get_vendor (vw);

  /* If there isn't a vendor behind us, close down */
  if (!vendor) {
    gnc_close_gui_component (vw->component_id);
    return;
  }

  /* Next, close if this is a destroy event */
  if (changes) {
    info = gnc_gui_get_entity_events (changes, &vw->vendor_guid);
    if (info && (info->event_mask & GNC_EVENT_DESTROY)) {
      gnc_close_gui_component (vw->component_id);
      return;
    }
  }
}

static VendorWindow *
gnc_vendor_new_window (GtkWidget *parent, GNCBook *bookp,
			 GncVendor *vendor)
{
  VendorWindow *vw;
  GladeXML *xml;
  GtkWidget *hbox, *edit;
  GnomeDialog *vwd;
  gnc_commodity *commodity;
  GNCPrintAmountInfo print_info;

  vw = g_new0 (VendorWindow, 1);

  vw->book = bookp;

  /* Find the dialog */
  xml = gnc_glade_xml_new ("vendor.glade", "Vendor Dialog");
  vw->dialog = glade_xml_get_widget (xml, "Vendor Dialog");
  vwd = GNOME_DIALOG (vw->dialog);

  gtk_object_set_data (GTK_OBJECT (vw->dialog), "dialog_info", vw);

  /* default to ok */
  gnome_dialog_set_default (vwd, 0);

  if (parent)
    gnome_dialog_set_parent (vwd, GTK_WINDOW (parent));

  /* Get entry points */
  vw->id_entry = glade_xml_get_widget (xml, "id_entry");
  vw->company_entry = glade_xml_get_widget (xml, "company_entry");

  vw->name_entry = glade_xml_get_widget (xml, "name_entry");
  vw->addr1_entry = glade_xml_get_widget (xml, "addr1_entry");
  vw->addr2_entry = glade_xml_get_widget (xml, "addr2_entry");
  vw->addr3_entry = glade_xml_get_widget (xml, "addr3_entry");
  vw->addr4_entry = glade_xml_get_widget (xml, "addr4_entry");
  vw->phone_entry = glade_xml_get_widget (xml, "phone_entry");
  vw->fax_entry = glade_xml_get_widget (xml, "fax_entry");
  vw->email_entry = glade_xml_get_widget (xml, "email_entry");

  vw->active_check = glade_xml_get_widget (xml, "active_check");
  vw->taxincluded_check = glade_xml_get_widget (xml, "tax_included_check");
  vw->notes_text = glade_xml_get_widget (xml, "notes_text");

  /* TERMS: Integer Value */
  edit = gnc_amount_edit_new();
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
  print_info = gnc_integral_print_info ();
  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit), 1);
  vw->terms_amount = edit;
  gtk_widget_show (edit);

  hbox = glade_xml_get_widget (xml, "terms_box");
  gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

  /* Setup Dialog for Editing */
  gnome_dialog_set_default (vwd, 0);

  /* Attach <Enter> to default button */
  gnome_dialog_editable_enters (vwd, GTK_EDITABLE (vw->id_entry));
  gnome_dialog_editable_enters (vwd, GTK_EDITABLE (vw->company_entry));

  gnome_dialog_editable_enters (vwd, GTK_EDITABLE (vw->name_entry));
  gnome_dialog_editable_enters (vwd, GTK_EDITABLE (vw->addr1_entry));
  gnome_dialog_editable_enters (vwd, GTK_EDITABLE (vw->addr2_entry));
  gnome_dialog_editable_enters (vwd, GTK_EDITABLE (vw->addr3_entry));
  gnome_dialog_editable_enters (vwd, GTK_EDITABLE (vw->addr4_entry));
  gnome_dialog_editable_enters (vwd, GTK_EDITABLE (vw->phone_entry));
  gnome_dialog_editable_enters (vwd, GTK_EDITABLE (vw->fax_entry));
  gnome_dialog_editable_enters (vwd, GTK_EDITABLE (vw->email_entry));

  /* Set focus to company name */
  gtk_widget_grab_focus (vw->company_entry);

  /* Setup signals */
  gnome_dialog_button_connect
    (vwd, 0, GTK_SIGNAL_FUNC(gnc_vendor_window_ok_cb), vw);
  gnome_dialog_button_connect
    (vwd, 1, GTK_SIGNAL_FUNC(gnc_vendor_window_cancel_cb), vw);
  gnome_dialog_button_connect
    (vwd, 2, GTK_SIGNAL_FUNC(gnc_vendor_window_help_cb), vw);

  gtk_signal_connect (GTK_OBJECT (vw->dialog), "destroy",
		      GTK_SIGNAL_FUNC(gnc_vendor_window_destroy_cb), vw);

  gtk_signal_connect(GTK_OBJECT (vw->id_entry), "changed",
		     GTK_SIGNAL_FUNC(gnc_vendor_name_changed_cb), vw);

  gtk_signal_connect(GTK_OBJECT (vw->company_entry), "changed",
		     GTK_SIGNAL_FUNC(gnc_vendor_name_changed_cb), vw);

  /* Setup initial values */
  if (vendor != NULL) {
    GncAddress *addr;
    const char *string;
    gint pos = 0;

    vw->dialog_type = EDIT_VENDOR;
    vw->vendor_guid = *gncVendorGetGUID (vendor);

    addr = gncVendorGetAddr (vendor);

    gtk_entry_set_text (GTK_ENTRY (vw->id_entry), gncVendorGetID (vendor));
    gtk_entry_set_editable (GTK_ENTRY (vw->id_entry), FALSE);

    gtk_entry_set_text (GTK_ENTRY (vw->company_entry), gncVendorGetName (vendor));

    /* Setup Address */
    gtk_entry_set_text (GTK_ENTRY (vw->name_entry), gncAddressGetName (addr));
    gtk_entry_set_text (GTK_ENTRY (vw->addr1_entry), gncAddressGetAddr1 (addr));
    gtk_entry_set_text (GTK_ENTRY (vw->addr2_entry), gncAddressGetAddr2 (addr));
    gtk_entry_set_text (GTK_ENTRY (vw->addr3_entry), gncAddressGetAddr3 (addr));
    gtk_entry_set_text (GTK_ENTRY (vw->addr4_entry), gncAddressGetAddr4 (addr));
    gtk_entry_set_text (GTK_ENTRY (vw->phone_entry), gncAddressGetPhone (addr));
    gtk_entry_set_text (GTK_ENTRY (vw->fax_entry), gncAddressGetFax (addr));
    gtk_entry_set_text (GTK_ENTRY (vw->email_entry), gncAddressGetEmail (addr));

    /* Set toggle buttons */
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (vw->active_check),
                                gncVendorGetActive (vendor));

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (vw->taxincluded_check),
				  gncVendorGetTaxIncluded (vendor));

    string = gncVendorGetNotes (vendor);
    gtk_editable_delete_text (GTK_EDITABLE (vw->notes_text), 0, -1);
    gtk_editable_insert_text (GTK_EDITABLE (vw->notes_text), string,
			      strlen(string), &pos);

    vw->component_id =
      gnc_register_gui_component (DIALOG_EDIT_VENDOR_CM_CLASS,
				  gnc_vendor_window_refresh_handler,
				  gnc_vendor_window_close_handler,
				  vw);
  } else {
    gnc_numeric num;
    vendor = gncVendorCreate (bookp);
    vw->vendor_guid = *gncVendorGetGUID (vendor);

    vw->dialog_type = NEW_VENDOR;
    gtk_entry_set_text (GTK_ENTRY (vw->id_entry),
			g_strdup_printf ("%.6d", gncVendorNextID(bookp)));
    vw->component_id =
      gnc_register_gui_component (DIALOG_NEW_VENDOR_CM_CLASS,
				  gnc_vendor_window_refresh_handler,
				  gnc_vendor_window_close_handler,
				  vw);
  }


  /* I know that vendor exists here -- either passed in or just created */
  {
    gnc_numeric terms;

    /* Set the Terms amounts */
    terms = gnc_numeric_create (gncVendorGetTerms (vendor), 1);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (vw->terms_amount), terms);
  }

  gnc_gui_component_watch_entity_type (vw->component_id,
				       GNC_ID_NONE,
				       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  gtk_widget_show_all (vw->dialog);

  return vw;
}

GncVendor *
gnc_vendor_new (GtkWidget *parent, GNCBook *bookp)
{
  VendorWindow *vw;
  GncVendor *created_vendor = NULL;

  /* Make sure required options exist */
  if (!bookp) return NULL;

  vw = gnc_vendor_new_window (parent, bookp, NULL);

  gtk_signal_connect (GTK_OBJECT (vw->dialog), "close",
		      GTK_SIGNAL_FUNC (gnc_vendor_on_close_cb),
		      &created_vendor);

  gtk_window_set_modal (GTK_WINDOW (vw->dialog), TRUE);

  gtk_main ();

  return created_vendor;
}

void
gnc_vendor_edit (GtkWidget *parent, GncVendor *vendor)
{
  VendorWindow *vw;

  if (!vendor) return;

  vw = gnc_vendor_new_window (parent, gncVendorGetBook(vendor), vendor);

  gtk_signal_connect (GTK_OBJECT (vw->dialog), "close",
		      GTK_SIGNAL_FUNC (gnc_vendor_on_close_cb),
		      NULL);

  gtk_window_set_modal (GTK_WINDOW (vw->dialog), TRUE);

  gtk_main ();

  return;
}

/* Functions for widgets for vendor selection */

static gpointer gnc_vendor_edit_new_cb (gpointer arg, GtkWidget *toplevel)
{
  struct _vendor_select_window *sw = arg;

  if (!arg) return NULL;

  return gnc_vendor_new (toplevel, sw->book);
}

static void gnc_vendor_edit_edit_cb (gpointer arg, gpointer obj, GtkWidget *toplevel)
{
  GncVendor *vendor = obj;
  struct _vendor_select_window *sw = arg;

  if (!arg || !obj) return;

  gnc_vendor_edit (toplevel, vendor);
}

gpointer gnc_vendor_edit_new_select (gpointer bookp, gpointer vendor,
				       GtkWidget *toplevel)
{
  GNCBook *book = bookp;
  struct _vendor_select_window sw;

  g_return_val_if_fail (bookp != NULL, NULL);

  sw.book = book;

  return
    gnc_ui_business_chooser_new (toplevel, vendor,
				 book, GNC_VENDOR_MODULE_NAME,
				 gnc_vendor_edit_new_cb,
				 gnc_vendor_edit_edit_cb, &sw);
}

gpointer gnc_vendor_edit_new_edit (gpointer bookp, gpointer v,
				     GtkWidget *toplevel)
{
  GNCBook *book = bookp;
  GncVendor *vendor = v;

  g_return_val_if_fail (vendor != NULL, NULL);

  gnc_vendor_edit (toplevel, vendor);
  return vendor;
}
