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
#include "dialog-search.h"
#include "search-param.h"

#include "gncAddress.h"
#include "gncVendor.h"
#include "gncVendorP.h"

#include "dialog-vendor.h"
#include "dialog-job-select.h"
#include "dialog-order.h"
#include "dialog-invoice.h"

#define DIALOG_NEW_VENDOR_CM_CLASS "dialog-new-vendor"
#define DIALOG_EDIT_VENDOR_CM_CLASS "dialog-edit-vendor"

typedef enum
{
  NEW_VENDOR,
  EDIT_VENDOR
} VendorDialogType;

struct _vendor_select_window {
  GNCBook *	book;
  GtkWidget *	parent;
  gboolean	no_close;
};

struct _vendor_window {
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
  GtkWidget *	terms_entry;

  GtkWidget *	active_check;
  GtkWidget *	taxincluded_check;
  GtkWidget *	notes_text;

  VendorDialogType	dialog_type;
  GUID		vendor_guid;
  gint		component_id;
  GNCBook *	book;
  GncVendor *	created_vendor;

};

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
  gncVendorSetTerms (vendor, gtk_editable_get_chars
		       (GTK_EDITABLE (vw->terms_entry), 0, -1));

  gncVendorCommitEdit (vendor);
  gnc_resume_gui_refresh ();
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

  //  /* Verify terms are valid (or empty) */
  //  min = gnc_numeric_zero ();
  //  if (check_edit_amount (vw->dialog, vw->terms_amount, &min, NULL,
  //			 _("Terms must be a positive integer or "
  //			   "you must leave it blank.")))
  //    return;

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

static gboolean
find_handler (gpointer find_data, gpointer user_data)
{
  const GUID *vendor_guid = find_data;
  VendorWindow *vw = user_data;

  return(vw && guid_equal(&vw->vendor_guid, vendor_guid));
}

static VendorWindow *
gnc_vendor_new_window (GtkWidget *parent, GNCBook *bookp,
			 GncVendor *vendor)
{
  VendorWindow *vw;
  GladeXML *xml;
  GnomeDialog *vwd;

  /*
   * Find an existing window for this vendor.  If found, bring it to
   * the front.
   */
  if (vendor) {
    GUID vendor_guid;
    
    vendor_guid = *gncVendorGetGUID (vendor);
    vw = gnc_find_first_gui_component (DIALOG_EDIT_VENDOR_CM_CLASS,
				       find_handler, &vendor_guid);
    if (vw) {
      gtk_window_present (GTK_WINDOW(vw->dialog));
      return(vw);
    }
  }
  
  /*
   * No existing employee window found.  Build a new one.
   */
  vw = g_new0 (VendorWindow, 1);

  vw->book = bookp;

  /* Find the dialog */
  xml = gnc_glade_xml_new ("vendor.glade", "Vendor Dialog");
  vw->dialog = glade_xml_get_widget (xml, "Vendor Dialog");
  vwd = GNOME_DIALOG (vw->dialog);

  gtk_object_set_data (GTK_OBJECT (vw->dialog), "dialog_info", vw);

  /* default to ok */
  gnome_dialog_set_default (vwd, 0);

  if (parent) {
    gnome_dialog_set_parent (vwd, GTK_WINDOW (parent));
    gtk_window_set_modal (GTK_WINDOW (vw->dialog), TRUE);
  }

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
  vw->terms_entry = glade_xml_get_widget (xml, "terms_entry");

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
    vendor = gncVendorCreate (bookp);
    gncVendorSetCommodity (vendor, gnc_default_currency ());
    vw->vendor_guid = *gncVendorGetGUID (vendor);

    vw->dialog_type = NEW_VENDOR;
    gtk_entry_set_text (GTK_ENTRY (vw->id_entry),
			g_strdup_printf ("%.6lld", gncVendorNextID(bookp)));
    vw->component_id =
      gnc_register_gui_component (DIALOG_NEW_VENDOR_CM_CLASS,
				  gnc_vendor_window_refresh_handler,
				  gnc_vendor_window_close_handler,
				  vw);
  }

  /* I know that vendor exists here -- either passed in or just created */

  /* Set the Terms amounts */
  gtk_entry_set_text (GTK_ENTRY (vw->terms_entry), gncVendorGetTerms (vendor));

  gnc_gui_component_watch_entity_type (vw->component_id,
				       GNC_VENDOR_MODULE_NAME,
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

  gtk_main ();

  return created_vendor;
}

VendorWindow *
gnc_ui_vendor_window_create (GncVendor *vendor)
{
  VendorWindow *vw;

  if (!vendor) return NULL;

  vw = gnc_vendor_new_window (NULL, gncVendorGetBook(vendor), vendor);

  return vw;
}

/* Functions for vendor selection widgets */

static gboolean
invoice_vendor_cb (gpointer *vendor_p, gpointer user_data)
{
  struct _vendor_select_window *sw = user_data;
  GncOwner owner;
  GncVendor *vendor;

  g_return_val_if_fail (vendor_p && user_data, TRUE);

  vendor = *vendor_p;

  if (!vendor)
    return TRUE;

  gncOwnerInitVendor (&owner, vendor);
  gnc_invoice_find (NULL, &owner, sw->book);
  return TRUE;
}

static gboolean
order_vendor_cb (gpointer *vendor_p, gpointer user_data)
{
  struct _vendor_select_window *sw = user_data;
  GncOwner owner;
  GncVendor *vendor;

  g_return_val_if_fail (vendor_p && user_data, TRUE);

  vendor = *vendor_p;

  if (!vendor)
    return TRUE;

  gncOwnerInitVendor (&owner, vendor);
  gnc_order_find (NULL, &owner, sw->book);
  return TRUE;
}

static gboolean
jobs_vendor_cb (gpointer *vendor_p, gpointer user_data)
{
  struct _vendor_select_window *sw = user_data;
  GncOwner owner;
  GncVendor *vendor;

  g_return_val_if_fail (vendor_p && user_data, TRUE);

  vendor = *vendor_p;

  if (!vendor)
    return TRUE;

  gncOwnerInitVendor (&owner, vendor);
  gnc_job_find (NULL, &owner, sw->book);
  return TRUE;
}

static gboolean
edit_vendor_cb (gpointer *vendor_p, gpointer user_data)
{
  GncVendor *vendor;

  g_return_val_if_fail (vendor_p && user_data, TRUE);

  vendor = *vendor_p;

  if (!vendor)
    return TRUE;

  gnc_ui_vendor_window_create (vendor);
  return TRUE;
}

static gboolean
select_vendor_cb (gpointer *vendor_p, gpointer user_data)
{
  g_return_val_if_fail (vendor_p && user_data, TRUE);
  if (*vendor_p)
    return FALSE;
  return TRUE;
}

static gboolean
new_vendor_cb (GtkWidget *parent, gpointer *vendor_p, gpointer user_data)
{
  struct _vendor_select_window *sw = user_data;
  
  g_return_val_if_fail (vendor_p && user_data, TRUE);

  *vendor_p = gnc_vendor_new (parent, sw->book);
  return sw->no_close;
}

static GncVendor *
gnc_vendor_select (GtkWidget *parent, GncVendor *start, GNCBook *book,
		   gboolean provide_select)
{
  static GList *params = NULL;
  gpointer res;
  QueryNew *q, *q2 = NULL;
  GNCSearchCallbackButton buttons[] = { 
    { N_("Select Vendor"), select_vendor_cb},
    { N_("View/Edit Vendor"), edit_vendor_cb},
    { N_("Vendor Jobs"), jobs_vendor_cb},
    //    { N_("Vendor Orders"), order_vendor_cb},
    { N_("Vendor Invoices"), invoice_vendor_cb},
    { NULL },
  };
  GNCIdType type = GNC_VENDOR_MODULE_NAME;
  struct _vendor_select_window sw;

  g_return_val_if_fail (book, NULL);

  /* Build parameter list in reverse order*/
  if (params == NULL) {
    params = gnc_search_param_prepend (params, _("Billing Contact"), NULL, type,
				       VENDOR_ADDR, ADDRESS_NAME, NULL);
    params = gnc_search_param_prepend (params, _("Vendor ID"), NULL, type,
				       VENDOR_ID, NULL);
    params = gnc_search_param_prepend (params, _("Vendor Name"), NULL, type,
				       VENDOR_NAME, NULL);
  }

  /* Build the queries */
  q = gncQueryCreate ();
  gncQuerySetBook (q, book);

  if (start) {
    q2 = gncQueryCopy (q);
    gncQueryAddGUIDMatch (q2, g_slist_prepend (NULL, QUERY_PARAM_GUID),
			  gncVendorGetGUID (start), QUERY_AND);
  }

  /* launch select dialog and return the result */
  sw.book = book;
  sw.parent = parent;
  sw.no_close = !provide_select;
  res = gnc_search_dialog_choose_object (parent, type, params, q, q2,
					 (provide_select ? buttons :
					  &(buttons[1])), NULL,
					 new_vendor_cb, &sw);

  gncQueryDestroy (q);
  return res;
}

void
gnc_vendor_find (GncVendor *start, GNCBook *book)
{
  gnc_vendor_select (NULL, start, book, FALSE);
}

GncVendor *
gnc_vendor_choose (GtkWidget *parent, GncVendor *start, GNCBook *book)
{
  return gnc_vendor_select (parent, start, book, TRUE);
}

gpointer gnc_vendor_edit_new_select (gpointer bookp, gpointer vendor,
				       GtkWidget *toplevel)
{
  return gnc_vendor_choose (toplevel, vendor, bookp);
}

gpointer gnc_vendor_edit_new_edit (gpointer bookp, gpointer v,
				     GtkWidget *toplevel)
{
  GncVendor *vendor = v;

  g_return_val_if_fail (vendor != NULL, NULL);

  gnc_ui_vendor_window_create (vendor);
  return vendor;
}
