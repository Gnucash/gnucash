/*
 * dialog-employee.c -- Dialog for Employee entry
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
#include "gncEmployee.h"
#include "gncEmployeeP.h"

#include "dialog-employee.h"
#include "business-chooser.h"

#define DIALOG_NEW_EMPLOYEE_CM_CLASS "dialog-new-employee"
#define DIALOG_EDIT_EMPLOYEE_CM_CLASS "dialog-edit-employee"

typedef enum
{
  NEW_EMPLOYEE,
  EDIT_EMPLOYEE
} EmployeeDialogType;

struct _employee_select_window {
  GNCBook *	book;
};

typedef struct _employee_window {
  GtkWidget *	dialog;

  GtkWidget *	id_entry;
  GtkWidget *	username_entry;

  GtkWidget *	name_entry;
  GtkWidget *	addr1_entry;
  GtkWidget *	addr2_entry;
  GtkWidget *	addr3_entry;
  GtkWidget *	addr4_entry;
  GtkWidget *	phone_entry;
  GtkWidget *	fax_entry;
  GtkWidget *	email_entry;

  GtkWidget *	language_entry;

  GtkWidget *	workday_amount;
  GtkWidget *	rate_amount;

  GtkWidget *	active_check;

  /* ACL? */

  EmployeeDialogType	dialog_type;
  GUID		employee_guid;
  gint		component_id;
  GNCBook *	book;
  GncEmployee *	created_employee;

} EmployeeWindow;

static GncEmployee *
ew_get_employee (EmployeeWindow *ew)
{
  if (!ew)
    return NULL;

  return gncEmployeeLookup (ew->book, &ew->employee_guid);
}

static void gnc_ui_to_employee (EmployeeWindow *ew, GncEmployee *employee)
{
  GncAddress *addr;

  addr = gncEmployeeGetAddr (employee);

  gnc_suspend_gui_refresh ();

  gncEmployeeSetID (employee, gtk_editable_get_chars
		    (GTK_EDITABLE (ew->id_entry), 0, -1));
  gncEmployeeSetUsername (employee, gtk_editable_get_chars
		      (GTK_EDITABLE (ew->username_entry), 0, -1));

  gncAddressSetName (addr, gtk_editable_get_chars
		     (GTK_EDITABLE (ew->name_entry), 0, -1));
  gncAddressSetAddr1 (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (ew->addr1_entry), 0, -1));
  gncAddressSetAddr2 (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (ew->addr2_entry), 0, -1));
  gncAddressSetAddr3 (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (ew->addr3_entry), 0, -1));
  gncAddressSetAddr4 (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (ew->addr4_entry), 0, -1));
  gncAddressSetPhone (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (ew->phone_entry), 0, -1));
  gncAddressSetFax (addr, gtk_editable_get_chars
		    (GTK_EDITABLE (ew->fax_entry), 0, -1));
  gncAddressSetEmail (addr, gtk_editable_get_chars
		      (GTK_EDITABLE (ew->email_entry), 0, -1));

  gncEmployeeSetActive (employee, gtk_toggle_button_get_active
			(GTK_TOGGLE_BUTTON (ew->active_check)));
  gncEmployeeSetLanguage (employee, gtk_editable_get_chars
		       (GTK_EDITABLE (ew->language_entry), 0, -1));

  /* Parse and set the workday and rate amounts */
  gncEmployeeSetWorkday (employee, gnc_amount_edit_get_amount
		       (GNC_AMOUNT_EDIT (ew->workday_amount)));
  gncEmployeeSetRate (employee, gnc_amount_edit_get_amount
		       (GNC_AMOUNT_EDIT (ew->rate_amount)));

  gncEmployeeCommitEdit (employee);
  gnc_resume_gui_refresh ();
}

#if 0
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
#endif

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
gnc_employee_window_ok_cb (GtkWidget *widget, gpointer data)
{
  EmployeeWindow *ew = data;

  /* Check for valid id */
  if (check_entry_nonempty (ew->dialog, ew->id_entry,
			    _("The Employee must be given an ID.")))
    return;

  /* Check for valid username */
  if (check_entry_nonempty (ew->dialog, ew->username_entry,
		   _("You must enter a username.")))
    return;

  /* Check for valid username */
  if (check_entry_nonempty (ew->dialog, ew->name_entry,
		   _("You must enter the employee's name.")))
    return;

  /* Make sure we have an address */
  if (check_entry_nonempty (ew->dialog, ew->addr1_entry, NULL) &&
      check_entry_nonempty (ew->dialog, ew->addr2_entry, NULL) &&
      check_entry_nonempty (ew->dialog, ew->addr3_entry, NULL) &&
      check_entry_nonempty (ew->dialog, ew->addr4_entry, NULL)) {
    const char *msg = _("You must enter an address.");
    gnc_error_dialog_parented (GTK_WINDOW (ew->dialog), msg);
    return;
  }

  /* Now save it off */
  {
    GncEmployee *employee = ew_get_employee (ew);
    if (employee) {
      gnc_ui_to_employee (ew, employee);
    }
    ew->created_employee = employee;
    ew->employee_guid = *xaccGUIDNULL ();
  }

  gnc_close_gui_component (ew->component_id);
}

static void
gnc_employee_window_cancel_cb (GtkWidget *widget, gpointer data)
{
  EmployeeWindow *ew = data;

  gnc_close_gui_component (ew->component_id);
}

static void
gnc_employee_window_help_cb (GtkWidget *widget, gpointer data)
{
  char *help_file = "";		/* xxx */

  helpWindow(NULL, NULL, help_file);
}

static void
gnc_employee_window_destroy_cb (GtkWidget *widget, gpointer data)
{
  EmployeeWindow *ew = data;
  GncEmployee *employee = ew_get_employee (ew);

  gnc_suspend_gui_refresh ();

  if (ew->dialog_type == NEW_EMPLOYEE && employee != NULL) {
    gncEmployeeDestroy (employee);
    ew->employee_guid = *xaccGUIDNULL ();
  }

  gnc_unregister_gui_component (ew->component_id);
  gnc_resume_gui_refresh ();

  g_free (ew);
}

static void
gnc_employee_name_changed_cb (GtkWidget *widget, gpointer data)
{
  EmployeeWindow *ew = data;
  char *name, *id, *fullname, *title;

  if (!ew)
    return;

  name = gtk_entry_get_text (GTK_ENTRY (ew->name_entry));
  if (!name || *name == '\0')
    name = _("<No name>");

  id = gtk_entry_get_text (GTK_ENTRY (ew->id_entry));

  fullname = g_strconcat (name, " (", id, ")", NULL);

  if (ew->dialog_type == EDIT_EMPLOYEE)
    title = g_strconcat (_("Edit Employee"), " - ", fullname, NULL);
  else
    title = g_strconcat (_("New Employee"), " - ", fullname, NULL);

  gtk_window_set_title (GTK_WINDOW (ew->dialog), title);

  g_free (fullname);
  g_free (title);
}

static int
gnc_employee_on_close_cb (GnomeDialog *dialog, gpointer data)
{
  EmployeeWindow *ew;
  GncEmployee **created_employee = data;

  if (data) {
    ew = gtk_object_get_data (GTK_OBJECT (dialog), "dialog_info");
    *created_employee = ew->created_employee;
  }

  gtk_main_quit ();

  return FALSE;
}

static void
gnc_employee_window_close_handler (gpointer user_data)
{
  EmployeeWindow *ew = user_data;

  gnome_dialog_close (GNOME_DIALOG (ew->dialog));
}

static void
gnc_employee_window_refresh_handler (GHashTable *changes, gpointer user_data)
{
  EmployeeWindow *ew = user_data;
  const EventInfo *info;
  GncEmployee *employee = ew_get_employee (ew);

  /* If there isn't a employee behind us, close down */
  if (!employee) {
    gnc_close_gui_component (ew->component_id);
    return;
  }

  /* Next, close if this is a destroy event */
  if (changes) {
    info = gnc_gui_get_entity_events (changes, &ew->employee_guid);
    if (info && (info->event_mask & GNC_EVENT_DESTROY)) {
      gnc_close_gui_component (ew->component_id);
      return;
    }
  }
}

static EmployeeWindow *
gnc_employee_new_window (GtkWidget *parent, GNCBook *bookp,
			 GncEmployee *employee)
{
  EmployeeWindow *ew;
  GladeXML *xml;
  GtkWidget *hbox, *edit;
  GnomeDialog *ewd;
  gnc_commodity *commodity;
  GNCPrintAmountInfo print_info;

  ew = g_new0 (EmployeeWindow, 1);

  ew->book = bookp;

  /* Find the dialog */
  xml = gnc_glade_xml_new ("employee.glade", "Employee Dialog");
  ew->dialog = glade_xml_get_widget (xml, "Employee Dialog");
  ewd = GNOME_DIALOG (ew->dialog);

  gtk_object_set_data (GTK_OBJECT (ew->dialog), "dialog_info", ew);

  /* default to ok */
  gnome_dialog_set_default (ewd, 0);

  if (parent)
    gnome_dialog_set_parent (ewd, GTK_WINDOW (parent));

  /* Get entry points */
  ew->id_entry = glade_xml_get_widget (xml, "id_entry");
  ew->username_entry = glade_xml_get_widget (xml, "username_entry");

  ew->name_entry = glade_xml_get_widget (xml, "name_entry");
  ew->addr1_entry = glade_xml_get_widget (xml, "addr1_entry");
  ew->addr2_entry = glade_xml_get_widget (xml, "addr2_entry");
  ew->addr3_entry = glade_xml_get_widget (xml, "addr3_entry");
  ew->addr4_entry = glade_xml_get_widget (xml, "addr4_entry");
  ew->phone_entry = glade_xml_get_widget (xml, "phone_entry");
  ew->fax_entry = glade_xml_get_widget (xml, "fax_entry");
  ew->email_entry = glade_xml_get_widget (xml, "email_entry");

  ew->language_entry = glade_xml_get_widget (xml, "language_entry");
  ew->active_check = glade_xml_get_widget (xml, "active_check");

  /* WORKDAY: Value */
  edit = gnc_amount_edit_new();
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
  print_info = gnc_integral_print_info ();
  print_info.max_decimal_places = 5;
  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit), 100000);
  ew->workday_amount = edit;
  gtk_widget_show (edit);

  hbox = glade_xml_get_widget (xml, "hours_hbox");
  gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

  /* RATE: Monetary Value */
  edit = gnc_amount_edit_new();
  commodity = gnc_default_currency ();
  print_info = gnc_commodity_print_info (commodity, FALSE);
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit),
                                gnc_commodity_get_fraction (commodity));
  ew->rate_amount = edit;
  gtk_widget_show (edit);

  hbox = glade_xml_get_widget (xml, "rate_hbox");
  gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

  /* Setup Dialog for Editing */
  gnome_dialog_set_default (ewd, 0);

  /* Attach <Enter> to default button */
  gnome_dialog_editable_enters (ewd, GTK_EDITABLE (ew->id_entry));
  gnome_dialog_editable_enters (ewd, GTK_EDITABLE (ew->username_entry));

  gnome_dialog_editable_enters (ewd, GTK_EDITABLE (ew->name_entry));
  gnome_dialog_editable_enters (ewd, GTK_EDITABLE (ew->addr1_entry));
  gnome_dialog_editable_enters (ewd, GTK_EDITABLE (ew->addr2_entry));
  gnome_dialog_editable_enters (ewd, GTK_EDITABLE (ew->addr3_entry));
  gnome_dialog_editable_enters (ewd, GTK_EDITABLE (ew->addr4_entry));
  gnome_dialog_editable_enters (ewd, GTK_EDITABLE (ew->phone_entry));
  gnome_dialog_editable_enters (ewd, GTK_EDITABLE (ew->fax_entry));
  gnome_dialog_editable_enters (ewd, GTK_EDITABLE (ew->email_entry));

  /* Set focus to username */
  gtk_widget_grab_focus (ew->username_entry);

  /* Setup signals */
  gnome_dialog_button_connect
    (ewd, 0, GTK_SIGNAL_FUNC(gnc_employee_window_ok_cb), ew);
  gnome_dialog_button_connect
    (ewd, 1, GTK_SIGNAL_FUNC(gnc_employee_window_cancel_cb), ew);
  gnome_dialog_button_connect
    (ewd, 2, GTK_SIGNAL_FUNC(gnc_employee_window_help_cb), ew);

  gtk_signal_connect (GTK_OBJECT (ew->dialog), "destroy",
		      GTK_SIGNAL_FUNC(gnc_employee_window_destroy_cb), ew);

  gtk_signal_connect(GTK_OBJECT (ew->id_entry), "changed",
		     GTK_SIGNAL_FUNC(gnc_employee_name_changed_cb), ew);

  gtk_signal_connect(GTK_OBJECT (ew->username_entry), "changed",
		     GTK_SIGNAL_FUNC(gnc_employee_name_changed_cb), ew);

  /* Setup initial values */
  if (employee != NULL) {
    GncAddress *addr;

    ew->dialog_type = EDIT_EMPLOYEE;
    ew->employee_guid = *gncEmployeeGetGUID (employee);

    addr = gncEmployeeGetAddr (employee);

    gtk_entry_set_text (GTK_ENTRY (ew->id_entry), gncEmployeeGetID (employee));
    gtk_entry_set_editable (GTK_ENTRY (ew->id_entry), FALSE);

    gtk_entry_set_text (GTK_ENTRY (ew->username_entry), gncEmployeeGetUsername (employee));

    /* Setup Address */
    gtk_entry_set_text (GTK_ENTRY (ew->name_entry), gncAddressGetName (addr));
    gtk_entry_set_text (GTK_ENTRY (ew->addr1_entry), gncAddressGetAddr1 (addr));
    gtk_entry_set_text (GTK_ENTRY (ew->addr2_entry), gncAddressGetAddr2 (addr));
    gtk_entry_set_text (GTK_ENTRY (ew->addr3_entry), gncAddressGetAddr3 (addr));
    gtk_entry_set_text (GTK_ENTRY (ew->addr4_entry), gncAddressGetAddr4 (addr));
    gtk_entry_set_text (GTK_ENTRY (ew->phone_entry), gncAddressGetPhone (addr));
    gtk_entry_set_text (GTK_ENTRY (ew->fax_entry), gncAddressGetFax (addr));
    gtk_entry_set_text (GTK_ENTRY (ew->email_entry), gncAddressGetEmail (addr));

    gtk_entry_set_text (GTK_ENTRY (ew->language_entry),
			gncEmployeeGetLanguage (employee));

    /* Set toggle buttons */
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (ew->active_check),
                                gncEmployeeGetActive (employee));

    ew->component_id =
      gnc_register_gui_component (DIALOG_EDIT_EMPLOYEE_CM_CLASS,
				  gnc_employee_window_refresh_handler,
				  gnc_employee_window_close_handler,
				  ew);
  } else {
    employee = gncEmployeeCreate (bookp);
    ew->employee_guid = *gncEmployeeGetGUID (employee);

    ew->dialog_type = NEW_EMPLOYEE;
    gtk_entry_set_text (GTK_ENTRY (ew->id_entry),
			g_strdup_printf ("%.6d", gncEmployeeNextID(bookp)));
    ew->component_id =
      gnc_register_gui_component (DIALOG_NEW_EMPLOYEE_CM_CLASS,
				  gnc_employee_window_refresh_handler,
				  gnc_employee_window_close_handler,
				  ew);
  }


  /* I know that employee exists here -- either passed in or just created */
  /* Set the workday and rate values */
  gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (ew->workday_amount),
			      gncEmployeeGetWorkday (employee));
  gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (ew->rate_amount),
			      gncEmployeeGetRate (employee));

  /* XXX: Set the ACL */

  gnc_gui_component_watch_entity_type (ew->component_id,
				       GNC_ID_NONE,
				       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  gtk_widget_show_all (ew->dialog);

  return ew;
}

GncEmployee *
gnc_employee_new (GtkWidget *parent, GNCBook *bookp)
{
  EmployeeWindow *ew;
  GncEmployee *created_employee = NULL;

  /* Make sure required options exist */
  if (!bookp) return NULL;

  ew = gnc_employee_new_window (parent, bookp, NULL);

  gtk_signal_connect (GTK_OBJECT (ew->dialog), "close",
		      GTK_SIGNAL_FUNC (gnc_employee_on_close_cb),
		      &created_employee);

  gtk_window_set_modal (GTK_WINDOW (ew->dialog), TRUE);

  gtk_main ();

  return created_employee;
}

void
gnc_employee_edit (GtkWidget *parent, GncEmployee *employee)
{
  EmployeeWindow *ew;

  if (!employee) return;

  ew = gnc_employee_new_window (parent, gncEmployeeGetBook(employee), employee);

  gtk_signal_connect (GTK_OBJECT (ew->dialog), "close",
		      GTK_SIGNAL_FUNC (gnc_employee_on_close_cb),
		      NULL);

  gtk_window_set_modal (GTK_WINDOW (ew->dialog), TRUE);

  gtk_main ();

  return;
}

/* Functions for widgets for employee selection */

static gpointer gnc_employee_edit_new_cb (gpointer arg, GtkWidget *toplevel)
{
  struct _employee_select_window *sw = arg;

  if (!arg) return NULL;

  return gnc_employee_new (toplevel, sw->book);
}

static void gnc_employee_edit_edit_cb (gpointer arg, gpointer obj, GtkWidget *toplevel)
{
  GncEmployee *employee = obj;

  if (!arg || !obj) return;

  gnc_employee_edit (toplevel, employee);
}

gpointer gnc_employee_edit_new_select (gpointer bookp, gpointer employee,
				       GtkWidget *toplevel)
{
  GNCBook *book = bookp;
  struct _employee_select_window sw;

  g_return_val_if_fail (bookp != NULL, NULL);

  sw.book = book;

  return
    gnc_ui_business_chooser_new (toplevel, employee,
				 book, GNC_EMPLOYEE_MODULE_NAME,
				 gnc_employee_edit_new_cb,
				 gnc_employee_edit_edit_cb, &sw);
}

gpointer gnc_employee_edit_new_edit (gpointer bookp, gpointer v,
				     GtkWidget *toplevel)
{
  GncEmployee *employee = v;

  g_return_val_if_fail (employee != NULL, NULL);

  gnc_employee_edit (toplevel, employee);
  return employee;
}
