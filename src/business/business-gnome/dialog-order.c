/*
 * dialog-order.c -- Dialog for Order entry
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

#include "gncOrder.h"
#include "gncOrderP.h"

#include "gncEntryLedger.h"

#include "dialog-order.h"
#include "business-chooser.h"
#include "business-utils.h"
#include "dialog-date-close.h"

#define DIALOG_NEW_ORDER_CM_CLASS "dialog-new-order"
#define DIALOG_EDIT_ORDER_CM_CLASS "dialog-edit-order"
#define DIALOG_VIEW_ORDER_CM_CLASS "dialog-edit-order"

typedef enum
{
  NEW_ORDER,
  EDIT_ORDER,
  VIEW_ORDER
} OrderDialogType;

struct _order_select_window {
  GNCBook *	book;
};

typedef struct _order_window {
  GtkWidget *	dialog;

  GtkWidget *	id_entry;
  GtkWidget *	ref_entry;
  GtkWidget *	owner_choice;
  GtkWidget *	notes_text;
  GtkWidget *	opened_date;
  GtkWidget *	closed_date;
  GtkWidget *	active_check;

  GnucashRegister *	reg;

  OrderDialogType	dialog_type;
  GUID		order_guid;
  gint		component_id;
  GNCBook *	book;
  GncOrder *	created_order;
  GncOwner	owner;

} OrderWindow;

static GncOrder *
ow_get_order (OrderWindow *ow)
{
  if (!ow)
    return NULL;

  return gncOrderLookup (ow->book, &ow->order_guid);
}

static void gnc_ui_to_order (OrderWindow *ow, GncOrder *order)
{
  Timespec ts;
  time_t tt;

  gnc_suspend_gui_refresh ();
  
  gncOrderSetID (order, gtk_editable_get_chars
		 (GTK_EDITABLE (ow->id_entry), 0, -1));
  gncOrderSetNotes (order, gtk_editable_get_chars
		    (GTK_EDITABLE (ow->notes_text), 0, -1));

  tt = gnome_date_edit_get_date (GNOME_DATE_EDIT (ow->opened_date));
  timespecFromTime_t (&ts, tt);
  gncOrderSetDateOpened (order, &ts);

  gncOrderSetActive (order, gtk_toggle_button_get_active
		     (GTK_TOGGLE_BUTTON (ow->active_check)));

  gnc_owner_get_owner (ow->owner_choice, &(ow->owner));
  gncOrderSetOwner (order, &(ow->owner));

  gncOrderCommitEdit (order);
  gnc_resume_gui_refresh ();
}

static gboolean
gnc_order_window_verify_ok (OrderWindow *ow)
{
  const char *res;
  GncOrder *order;

  /* Check the ID */
  res = gtk_entry_get_text (GTK_ENTRY (ow->id_entry));
  if (safe_strcmp (res, "") == 0) {
    gnc_error_dialog_parented (GTK_WINDOW (ow->dialog),
			       _("The Order must be given an ID."));
    return FALSE;
  }

  /* Check the Owner */
  gnc_owner_get_owner (ow->owner_choice, &(ow->owner));
  res = gncOwnerGetName (&(ow->owner));
  if (res == NULL || safe_strcmp (res, "") == 0) {
    gnc_error_dialog_parented (GTK_WINDOW (ow->dialog),
  			       _("You need to supply Billing Information."));
    return FALSE;
  }

  /* Check that there is at least one Entry */
  order = ow_get_order (ow);
  if (gncOrderGetEntries (order) == NULL) {
    gnc_error_dialog_parented (GTK_WINDOW (ow->dialog),
			       _("The Order must have at least one Entry."));
    return FALSE;
  }

  return TRUE;
}

static void
gnc_order_window_ok_cb (GtkWidget *widget, gpointer data)
{
  OrderWindow *ow = data;

  if (!gnc_order_window_verify_ok (ow))
      return;
  
  /* Now save it off */
  {
    GncOrder *order = ow_get_order (ow);
    if (order) {
      gnc_ui_to_order (ow, order);

    }
    ow->created_order = order;
    ow->order_guid = *xaccGUIDNULL ();
  }

  gnc_close_gui_component (ow->component_id);
}

static void
gnc_order_window_cancel_cb (GtkWidget *widget, gpointer data)
{
  OrderWindow *ow = data;

  gnc_close_gui_component (ow->component_id);
}

static void
gnc_order_window_help_cb (GtkWidget *widget, gpointer data)
{
  char *help_file = "";		/* XXX: FIXME */

  helpWindow(NULL, NULL, help_file);
}

static void
gnc_order_window_invoice_cb (GtkWidget *widget, gpointer data)
{
  /* XXX: create a new invoice for this customer/vendor */

  fprintf (stderr, "I would create an invoice now... \n");

  /* XXX: now refresh this window, it's possible a number of
   * entries have changed
   */
}

static void
gnc_order_window_close_order_cb (GtkWidget *widget, gpointer data)
{
  OrderWindow *ow = data;
  GncOrder *order;
  GList *entries;
  char *message, *label;
  gboolean non_inv = FALSE;
  Timespec ts;

  /* Make sure the order is ok */
  if (!gnc_order_window_verify_ok (ow))
      return;

  /* Make sure we can close the order. Are there any uninvoiced entries? */
  order = ow_get_order (ow);
  if (!order)
    return;

  entries = gncOrderGetEntries (order);
  for ( ; entries ; entries = entries->next) {
    GncEntry *entry = entries->data;
    if (gncEntryGetInvoice (entry) == NULL) {
      non_inv = TRUE;
      break;
    }
  }

  if (non_inv) {
    /* Damn; yes.  Well, ask the user to make sure they REALLY want to
     * close this order!
     */

    message = _("This order contains entries that have not been invoiced.\n"
		"Are you sure you want to close it out before\n"
		"you invoice all the entries?");

    if (gnc_verify_dialog_parented (ow->dialog, message, FALSE) == FALSE)
      return;
  }

  /* Ok, we can close this.  Ask for verification and set the closed date */
  message = _("Do you really want to close the order?");
  label = _("Close Date");

  timespecFromTime_t (&ts, time(NULL));
  if (!gnc_dialog_date_close_parented (ow->dialog, message, label, TRUE, &ts))
    return;

  gncOrderSetDateClosed (order, &ts);

  /* And close the order */
  return gnc_order_window_ok_cb (widget, data);
}

static void
gnc_order_window_destroy_cb (GtkWidget *widget, gpointer data)
{
  OrderWindow *ow = data;
  GncOrder *order = ow_get_order (ow);

  gnc_suspend_gui_refresh ();

  if (ow->dialog_type == NEW_ORDER && order != NULL) {
    gncOrderDestroy (order);
    ow->order_guid = *xaccGUIDNULL ();
  }

  gnc_unregister_gui_component (ow->component_id);
  gnc_resume_gui_refresh ();

  g_free (ow);
}

static int
gnc_order_owner_changed_cb (GtkWidget *widget, gpointer data)
{
  OrderWindow *ow = data;

  if (!ow)
    return FALSE;

  gnc_owner_get_owner (ow->owner_choice, &(ow->owner));
  switch (gncOwnerGetType (&(ow->owner))) {
  case GNC_OWNER_JOB:
  {
    char const *msg = gncJobGetReference (gncOwnerGetJob (&(ow->owner)));
    gtk_entry_set_text (GTK_ENTRY (ow->ref_entry), msg ? msg : "");
    break;
  }
  default:
    gtk_entry_set_text (GTK_ENTRY (ow->ref_entry), "");
    break;    
  }
  return FALSE;
}

static int
gnc_order_on_close_cb (GnomeDialog *dialog, gpointer data)
{
  OrderWindow *ow;
  GncOrder **created_order = data;

  if (data) {
    ow = gtk_object_get_data (GTK_OBJECT (dialog), "dialog_info");
    *created_order = ow->created_order;
  }

  gtk_main_quit ();

  return FALSE;
}

static void
gnc_order_window_close_handler (gpointer user_data)
{
  OrderWindow *ow = user_data;

  gnome_dialog_close (GNOME_DIALOG (ow->dialog));
}

static void
gnc_order_window_refresh_handler (GHashTable *changes, gpointer user_data)
{
  OrderWindow *ow = user_data;
  const EventInfo *info;
  GncOrder *order = ow_get_order (ow);

  /* If there isn't a order behind us, close down */
  if (!order) {
    gnc_close_gui_component (ow->component_id);
    return;
  }

  /* Next, close if this is a destroy event */
  if (changes) {
    info = gnc_gui_get_entity_events (changes, &ow->order_guid);
    if (info && (info->event_mask & GNC_EVENT_DESTROY)) {
      gnc_close_gui_component (ow->component_id);
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

static OrderWindow *
gnc_order_new_window (GtkWidget *parent, GNCBook *bookp,
		      OrderDialogType type, GncOrder *order, GncOwner *owner)
{
  OrderWindow *ow;
  GladeXML *xml;
  GtkWidget *label, *hbox, *vbox, *regWidget, *cd_label;
  GncEntryLedger *entry_ledger = NULL;
  GnomeDialog *owd;
  GList *entries;
  gboolean hide_cd = FALSE;

  gnc_configure_register_colors ();

  ow = g_new0 (OrderWindow, 1);
  ow->book = bookp;
  ow->dialog_type = type;

  if (type == NEW_ORDER) {
    order = gncOrderCreate (bookp);
    gncOrderSetOwner (order, owner);
  }

  /* Save this for later */
  gncOwnerCopy (owner, &(ow->owner));

  /* Find the dialog */
  xml = gnc_glade_xml_new ("order.glade", "Order Entry Dialog");
  ow->dialog = glade_xml_get_widget (xml, "Order Entry Dialog");
  owd = GNOME_DIALOG (ow->dialog);

  gtk_object_set_data (GTK_OBJECT (ow->dialog), "dialog_info", ow);

  /* Grab the widgets */
  ow->id_entry = glade_xml_get_widget (xml, "id_entry");
  ow->ref_entry = glade_xml_get_widget (xml, "ref_entry");
  ow->notes_text = glade_xml_get_widget (xml, "notes_text");
  ow->opened_date = glade_xml_get_widget (xml, "opened_date");
  ow->closed_date = glade_xml_get_widget (xml, "closed_date");
  ow->active_check = glade_xml_get_widget (xml, "active_check");
  cd_label = glade_xml_get_widget (xml, "cd_label");

  hbox = glade_xml_get_widget (xml, "owner_hbox");
  label = glade_xml_get_widget (xml, "owner_label");

  /* default to ok */
  gnome_dialog_editable_enters (owd, GTK_EDITABLE (ow->id_entry));
  gnome_dialog_set_default (owd, 0);

  /* Build the ledger */
  switch (type) {
  case NEW_ORDER:
  case EDIT_ORDER:
    entry_ledger = gnc_entry_ledger_new (ow->book, GNCENTRY_ORDER_ENTRY);
    break;
  case VIEW_ORDER:
  default:
    entry_ledger = gnc_entry_ledger_new (ow->book, GNCENTRY_ORDER_VIEWER);
    break;
  }
  entries = gncOrderGetEntries (order);
  /* Set watches on entries */
  gnc_entry_ledger_load (entry_ledger, entries);

  /* Watch the order of operations, here... */
  gnucash_register_set_initial_rows( 10 );
  regWidget = gnucash_register_new (gnc_entry_ledger_get_table (entry_ledger));
  gnc_table_init_gui( regWidget, entry_ledger );
  ow->reg = GNUCASH_REGISTER (regWidget);
  GNUCASH_SHEET (ow->reg->sheet)->window = GTK_WIDGET(ow->dialog);
  gnc_entry_ledger_set_parent (entry_ledger, ow->dialog);

  vbox = glade_xml_get_widget (xml, "ledger_vbox");
  // gtk_box_pack_start (GTK_BOX(vbox), toolbar, FALSE, FALSE, 2);
  gtk_box_pack_start (GTK_BOX(vbox), regWidget, TRUE, TRUE, 2);

  if (parent)
    gnome_dialog_set_parent (owd, GTK_WINDOW (parent));

  gtk_signal_connect (GTK_OBJECT (ow->dialog), "destroy",
		      GTK_SIGNAL_FUNC(gnc_order_window_destroy_cb), ow);

  gnome_dialog_button_connect (owd, 0,
			       GTK_SIGNAL_FUNC(gnc_order_window_ok_cb), ow);
  gnome_dialog_button_connect (owd, 1,
			       GTK_SIGNAL_FUNC(gnc_order_window_cancel_cb), ow);
  gnome_dialog_button_connect (owd, 2,
			       GTK_SIGNAL_FUNC(gnc_order_window_help_cb), ow);

  gnome_dialog_button_connect
    (owd, 3, GTK_SIGNAL_FUNC(gnc_order_window_invoice_cb), ow);

  gnome_dialog_button_connect
    (owd, 4, GTK_SIGNAL_FUNC(gnc_order_window_close_order_cb), ow);

  /* Setup initial values */
  ow->order_guid = *gncOrderGetGUID (order);

  {
    char * class_name = NULL;

    switch (type) {
    case VIEW_ORDER:
    default:
      class_name = DIALOG_VIEW_ORDER_CM_CLASS;
      /* FALLTHROUGH */
    case EDIT_ORDER:
      ow->owner_choice = gnc_owner_edit_create (label, hbox, bookp, owner);
      gtk_entry_set_text (GTK_ENTRY (ow->id_entry), gncOrderGetID (order));
      
      if (class_name == NULL)
	class_name = DIALOG_EDIT_ORDER_CM_CLASS;
      break;

    case NEW_ORDER:
      ow->owner_choice = gnc_owner_select_create (label, hbox, bookp, owner);
      gtk_entry_set_text (GTK_ENTRY (ow->id_entry),
			  g_strdup_printf ("%.6d", gncOrderNextID(bookp)));
      
      class_name = DIALOG_NEW_ORDER_CM_CLASS;
      break;
    }

    ow->component_id =
      gnc_register_gui_component (class_name,
				  gnc_order_window_refresh_handler,
				  gnc_order_window_close_handler,
				  ow);
  }

  gtk_signal_connect (GTK_OBJECT (ow->owner_choice), "changed",
		      GTK_SIGNAL_FUNC (gnc_order_owner_changed_cb),
		      ow);

  /* Set the Reference */
  gnc_order_owner_changed_cb (ow->owner_choice, ow);

  /* We know that "order" (and "owner") exist now */
  {
    const char *string;
    Timespec ts, ts_zero = {0,0};
    time_t tt;
    gint pos = 0;

    string = gncOrderGetNotes (order);
    gtk_editable_delete_text (GTK_EDITABLE (ow->notes_text), 0, -1);
    gtk_editable_insert_text (GTK_EDITABLE (ow->notes_text), string,
			      strlen (string), &pos);

    ts = gncOrderGetDateOpened (order);
    if (timespec_equal (&ts, &ts_zero)) {
      tt = time(NULL);
    } else {
      tt = ts.tv_sec;		/* XXX */
    }
    gnome_date_edit_set_time (GNOME_DATE_EDIT (ow->opened_date), tt);

    ts = gncOrderGetDateClosed (order);
    if (timespec_equal (&ts, &ts_zero)) {
      tt = time(NULL);
      hide_cd = TRUE;
    } else {
      tt = ts.tv_sec;		/* XXX */
    }
    gnome_date_edit_set_time (GNOME_DATE_EDIT (ow->closed_date), tt);

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (ow->active_check),
                                gncOrderGetActive (order));

  }

  gnc_gui_component_watch_entity_type (ow->component_id,
				       GNC_ID_NONE,
				       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  gnc_entry_ledger_set_default_order (entry_ledger, order);
  gnc_table_realize_gui (gnc_entry_ledger_get_table (entry_ledger));
  gtk_widget_show_all (ow->dialog);
  gnc_table_refresh_gui (gnc_entry_ledger_get_table (entry_ledger), TRUE);

  if (hide_cd) {
    GtkWidget *hide;

    gtk_widget_hide_all (ow->closed_date);
    gtk_widget_hide_all (cd_label);

    hide = glade_xml_get_widget (xml, "hide1");
    gtk_widget_hide_all (hide);
    hide = glade_xml_get_widget (xml, "hide2");
    gtk_widget_hide_all (hide);
  }

  if (type == VIEW_ORDER) {
    GtkWidget *hide;

    /* Setup viewer for read-only access */
    gtk_widget_set_sensitive (ow->id_entry, FALSE);
    gtk_widget_set_sensitive (ow->opened_date, FALSE);
    gtk_widget_set_sensitive (ow->closed_date, FALSE);
    gtk_widget_set_sensitive (ow->notes_text, FALSE);

    /* Hide the 'close order' button */
    hide = glade_xml_get_widget (xml, "close_order_button");
    gtk_widget_hide_all (hide);
    hide = glade_xml_get_widget (xml, "new_invoice_button");
    gtk_widget_hide_all (hide);
  }
  
  return ow;
}

GncOrder *
gnc_order_new (GtkWidget *parent, GncOwner *ownerp, GNCBook *bookp)
{
  OrderWindow *ow;
  GncOrder *created_order = NULL;
  GncOwner owner;

  if (ownerp)
    gncOwnerCopy (ownerp, &owner);
  else
    gncOwnerInitJob (&owner, NULL); /* XXX: pass in the owner type? */

  /* Make sure required options exist */
  if (!bookp) return NULL;

  ow = gnc_order_new_window (parent, bookp, NEW_ORDER, NULL, &owner);

  gtk_signal_connect (GTK_OBJECT (ow->dialog), "close",
		      GTK_SIGNAL_FUNC (gnc_order_on_close_cb),
		      &created_order);

  // gtk_window_set_modal (GTK_WINDOW (ow->dialog), TRUE);

  gtk_main ();

  return created_order;
}

void
gnc_order_edit (GtkWidget *parent, GncOrder *order)
{
  OrderWindow *ow;
  OrderDialogType type;

  if (!order) return;

  type = EDIT_ORDER;
  {
    Timespec ts = gncOrderGetDateClosed (order);
    if (ts.tv_sec || ts.tv_nsec)
      type = VIEW_ORDER;
  }

  ow = gnc_order_new_window (parent, gncOrderGetBook(order), type, order, 
			     gncOrderGetOwner (order));

  gtk_signal_connect (GTK_OBJECT (ow->dialog), "close",
		      GTK_SIGNAL_FUNC (gnc_order_on_close_cb),
		      NULL);

  // gtk_window_set_modal (GTK_WINDOW (ow->dialog), TRUE);

  gtk_main ();

  return;
}

/* Functions for widgets for order selection */

static gpointer gnc_order_edit_new_cb (gpointer arg, GtkWidget *toplevel)
{
  struct _order_select_window *sw = arg;

  if (!arg) return NULL;

  return gnc_order_new (toplevel, NULL, sw->book); /* XXX, set owner type? */
}

static void gnc_order_edit_edit_cb (gpointer arg, gpointer obj, GtkWidget *toplevel)
{
  GncOrder *order = obj;

  if (!arg || !obj) return;

  gnc_order_edit (toplevel, order);
}

gpointer gnc_order_edit_new_select (gpointer bookp, gpointer order,
				       GtkWidget *toplevel)
{
  GNCBook *book = bookp;
  struct _order_select_window sw;

  g_return_val_if_fail (bookp != NULL, NULL);

  sw.book = book;

  return
    gnc_ui_business_chooser_new (toplevel, order,
				 book, GNC_ORDER_MODULE_NAME,
				 gnc_order_edit_new_cb,
				 gnc_order_edit_edit_cb, &sw);
}

gpointer gnc_order_edit_new_edit (gpointer bookp, gpointer v,
				     GtkWidget *toplevel)
{
  GncOrder *order = v;

  g_return_val_if_fail (order != NULL, NULL);

  gnc_order_edit (toplevel, order);
  return order;
}
