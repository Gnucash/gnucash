/*
 * dialog-job.c -- Dialog for Job entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <gnome.h>

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-engine-util.h"
#include "window-help.h"

#include "gncBusiness.h"
#include "gncCustomer.h"
#include "gncJob.h"
#include "gncJobP.h"

#include "gnc-general-select.h"
#include "dialog-customer.h"
#include "business-chooser.h"
#include "dialog-job-select.h"
#include "dialog-job.h"

#define DIALOG_NEW_JOB_CM_CLASS "dialog-new-job"
#define DIALOG_EDIT_JOB_CM_CLASS "dialog-edit-job"

typedef enum
{
  NEW_JOB,
  EDIT_JOB
} JobDialogType;

struct _job_select_window {
  GtkWidget *	toplevel;
  GncBusiness *	business;
};

typedef struct _job_window {
  GtkWidget *	dialog;
  GtkWidget *	id_entry;
  GtkWidget *	cust_edit;
  GtkWidget *	name_entry;
  GtkWidget *	desc_entry;
  GtkWidget *	active_check;

  JobDialogType	dialog_type;
  GUID		job_guid;
  gint		component_id;
  GncBusiness *	business;
  GncJob *	created_job;

  GNCGeneralSelectGetStringCB cust_print;

} JobWindow;

static GncJob *
jw_get_job (JobWindow *jw)
{
  if (!jw)
    return NULL;

  return gncBusinessLookupGUID (jw->business, GNC_JOB_MODULE_NAME,
				&jw->job_guid);
}

static void gnc_ui_to_job (JobWindow *jw, GncJob *job)
{
  gnc_suspend_gui_refresh ();
  gncJobSetID (job, gtk_editable_get_chars (GTK_EDITABLE (jw->id_entry),
					    0, -1));
  gncJobSetName (job, gtk_editable_get_chars (GTK_EDITABLE (jw->name_entry),
					      0, -1));
  gncJobSetDesc (job, gtk_editable_get_chars (GTK_EDITABLE (jw->desc_entry),
					      0, -1));
  gncJobSetActive (job, gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON
						      (jw->active_check)));
  {
    GncCustomer *old = gncJobGetCustomer (job);
    GncCustomer *new = gnc_general_select_get_selected (GNC_GENERAL_SELECT
							(jw->cust_edit));
    if (old != new)
      gncJobSetCustomer (job, new);
  }

  gncJobCommitEdit (job);
  gnc_resume_gui_refresh ();
}

static void
gnc_job_window_ok_cb (GtkWidget *widget, gpointer data)
{
  JobWindow *jw = data;
  char *res;
  GncCustomer *cust;

  /* Check for valid id */
  res = gtk_entry_get_text (GTK_ENTRY (jw->id_entry));
  if (safe_strcmp (res, "") == 0) {
    const char *message = _("The Job must be given an ID.");
    gnc_error_dialog_parented(GTK_WINDOW(jw->dialog), message);
    return;
  }

  /* Check for valid name */
  res = gtk_entry_get_text (GTK_ENTRY (jw->name_entry));
  if (safe_strcmp (res, "") == 0) {
    const char *message = _("The Job must be given a name.");
    gnc_error_dialog_parented(GTK_WINDOW(jw->dialog), message);
    return;
  }

  /* Check for customer */
  cust = gnc_general_select_get_selected (GNC_GENERAL_SELECT (jw->cust_edit));
  if (!cust) {
    const char *message = _("You must choose a customer.");
    gnc_error_dialog_parented(GTK_WINDOW(jw->dialog), message);
    return;
  }

  /* Now save it off */
  {
    GncJob *job = jw_get_job (jw);
    if (job) {
      gnc_ui_to_job (jw, job);
    }
    jw->created_job = job;
    jw->job_guid = *xaccGUIDNULL ();
  }

  gnc_close_gui_component (jw->component_id);
}

static void
gnc_job_window_cancel_cb (GtkWidget *widget, gpointer data)
{
  JobWindow *jw = data;

  gnc_close_gui_component (jw->component_id);
}

static void
gnc_job_window_help_cb (GtkWidget *widget, gpointer data)
{
  JobWindow *jw = data;
  char *help_file = "";		/* xxx */

  /* XXX */

  helpWindow(NULL, NULL, help_file);
}

static void
gnc_job_window_destroy_cb (GtkWidget *widget, gpointer data)
{
  JobWindow *jw = data;
  GncJob *job = jw_get_job (jw);

  gnc_suspend_gui_refresh ();

  if (jw->dialog_type == NEW_JOB && job != NULL) {
    gncJobDestroy (job);
    jw->job_guid = *xaccGUIDNULL ();
  }

  gnc_unregister_gui_component (jw->component_id);
  gnc_resume_gui_refresh ();

  g_free (jw);
}

static void
gnc_job_name_changed_cb (GtkWidget *widget, gpointer data)
{
  JobWindow *jw = data;
  char *name, *id, *fullname, *title;

  if (!jw)
    return;

  name = gtk_entry_get_text (GTK_ENTRY (jw->name_entry));
  if (!name || *name == '\0')
    name = _("<No name>");

  id = gtk_entry_get_text (GTK_ENTRY (jw->id_entry));

  fullname = g_strconcat (name, " (", id, ")", NULL);

  if (jw->dialog_type == EDIT_JOB)
    title = g_strconcat (_("Edit Job"), " - ", fullname, NULL);
  else
    title = g_strconcat (_("New Job"), " - ", fullname, NULL);

  gtk_window_set_title (GTK_WINDOW (jw->dialog), title);

  g_free (fullname);
  g_free (title);
}

static int
gnc_job_on_close_cb (GnomeDialog *dialog, gpointer data)
{
  JobWindow *jw;
  GncJob **created_job = data;

  if (data) {
    jw = gtk_object_get_data (GTK_OBJECT (dialog), "dialog_info");
    *created_job = jw->created_job;
  }

  gtk_main_quit ();

  return FALSE;
}

static void
gnc_job_window_close_handler (gpointer user_data)
{
  JobWindow *jw = user_data;

  gnome_dialog_close (GNOME_DIALOG (jw->dialog));
}

static void
gnc_job_window_refresh_handler (GHashTable *changes, gpointer user_data)
{
  JobWindow *jw = user_data;
  const EventInfo *info;
  GncJob *job = jw_get_job (jw);

  /* If there isn't a job behind us, close down */
  if (!job) {
    gnc_close_gui_component (jw->component_id);
    return;
  }

  /* Next, close if this is a destroy event */
  if (changes) {
    info = gnc_gui_get_entity_events (changes, &jw->job_guid);
    if (info && (info->event_mask & GNC_EVENT_DESTROY)) {
      gnc_close_gui_component (jw->component_id);
      return;
    }
  }
}

static JobWindow *
gnc_job_new_window (GtkWidget *parent, GncBusiness *bus, GncCustomer *cust,
		    GncJob *job)
{
  JobWindow *jw;
  GladeXML *xml;
  GtkWidget *cust_box;
  GnomeDialog *jwd;
  GtkObject *jwo;

  jw = g_new0 (JobWindow, 1);
  jw->business = bus;

  /* Load the XML */
  xml = gnc_glade_xml_new ("job.glade", "Job Dialog");

  /* Find the dialog */
  jw->dialog = glade_xml_get_widget (xml, "Job Dialog");
  jwo = GTK_OBJECT (jw->dialog);
  jwd = GNOME_DIALOG (jwo);

  gtk_object_set_data (jwo, "dialog_info", jw);

  /* default to ok XXX */
  gnome_dialog_set_default (jwd, 0);

  if (parent)
    gnome_dialog_set_parent (jwd, GTK_WINDOW (parent));

  /* Get entry points */
  jw->id_entry  = glade_xml_get_widget (xml, "id_entry");
  jw->name_entry = glade_xml_get_widget (xml, "name_entry");
  jw->desc_entry = glade_xml_get_widget (xml, "desc_entry");
  jw->active_check = glade_xml_get_widget (xml, "active_check");

  cust_box = glade_xml_get_widget (xml, "customer_hbox");

  /* Setup signals (XXX) */
  gnome_dialog_button_connect (jwd, 0,
			       GTK_SIGNAL_FUNC(gnc_job_window_ok_cb), jw);
  gnome_dialog_button_connect (jwd, 1,
			       GTK_SIGNAL_FUNC(gnc_job_window_cancel_cb), jw);
  gnome_dialog_button_connect (jwd, 2,
			       GTK_SIGNAL_FUNC(gnc_job_window_help_cb), jw);

  gtk_signal_connect (jwo, "destroy",
		      GTK_SIGNAL_FUNC(gnc_job_window_destroy_cb), jw);

  gtk_signal_connect(GTK_OBJECT (jw->id_entry), "changed",
		     GTK_SIGNAL_FUNC(gnc_job_name_changed_cb), jw);

  gtk_signal_connect(GTK_OBJECT (jw->name_entry), "changed",
		     GTK_SIGNAL_FUNC(gnc_job_name_changed_cb), jw);

  /* grab the printable routine */
  {
    const GncBusinessObject *obj =
      gncBusinessLookup (GNC_CUSTOMER_MODULE_NAME);
    if (!obj)
      printf ("NO CUSTOMER OBJECT LOADED");
    jw->cust_print = obj->printable;
  }

  /* Attach <Enter> to default button */
  gnome_dialog_editable_enters (jwd, GTK_EDITABLE (jw->id_entry));
  gnome_dialog_editable_enters (jwd, GTK_EDITABLE (jw->name_entry));
  gnome_dialog_editable_enters (jwd, GTK_EDITABLE (jw->desc_entry));

  /* Start at the name */
  gtk_widget_grab_focus (jw->name_entry);

  /* Set initial entries */
  if (job != NULL) {
    jw->job_guid = *gncJobGetGUID (job);

    jw->dialog_type = EDIT_JOB;
    jw->cust_edit = gnc_general_select_new (GNC_GENERAL_SELECT_TYPE_EDIT,
					    jw->cust_print,
					    gnc_customer_edit_new_edit, bus);
    gtk_box_pack_start(GTK_BOX(cust_box), jw->cust_edit, TRUE, TRUE, 0);

    gtk_entry_set_text (GTK_ENTRY (jw->id_entry), gncJobGetID (job));
    gtk_entry_set_editable (GTK_ENTRY (jw->id_entry), FALSE);
    gtk_entry_set_text (GTK_ENTRY (jw->name_entry), gncJobGetName (job));
    gtk_entry_set_text (GTK_ENTRY (jw->desc_entry), gncJobGetDesc (job));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (jw->active_check),
                                gncJobGetActive (job));
    gnc_general_select_set_selected (GNC_GENERAL_SELECT(jw->cust_edit),
				     gncJobGetCustomer(job));

    jw->component_id = gnc_register_gui_component (DIALOG_EDIT_JOB_CM_CLASS,
						   gnc_job_window_refresh_handler,
						   gnc_job_window_close_handler,
						   jw);
  } else {
    job = gncJobCreate (bus);
    jw->job_guid = *gncJobGetGUID (job);
      
    jw->dialog_type = NEW_JOB;
    jw->cust_edit = gnc_general_select_new (GNC_GENERAL_SELECT_TYPE_SELECT,
					    jw->cust_print,
					    gnc_customer_edit_new_select, bus);
    gtk_box_pack_start(GTK_BOX(cust_box), jw->cust_edit, TRUE, TRUE, 0);

    gtk_entry_set_text (GTK_ENTRY (jw->id_entry),
			g_strdup_printf ("%.6d", gncJobNextID(bus)));

    gnc_general_select_set_selected (GNC_GENERAL_SELECT(jw->cust_edit),
				     cust);
    jw->component_id = gnc_register_gui_component (DIALOG_NEW_JOB_CM_CLASS,
						   gnc_job_window_refresh_handler,
						   gnc_job_window_close_handler,
						   jw);
  }

  gnc_job_name_changed_cb (NULL, jw);
  gnc_gui_component_watch_entity_type (jw->component_id,
				       GNC_ID_NONE,
				       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  gtk_widget_show_all (jw->dialog);

  return jw;
}

GncJob *
gnc_job_new (GtkWidget *parent, GncBusiness *bus, GncCustomer *customer)
{
  JobWindow *jw;
  GncJob *created_job = NULL;

  /* Make sure required options exist */
  if (!bus) return NULL;

  jw = gnc_job_new_window (parent, bus, customer, NULL);

  gtk_signal_connect (GTK_OBJECT (jw->dialog), "close",
		      GTK_SIGNAL_FUNC (gnc_job_on_close_cb), &created_job);

  gtk_window_set_modal (GTK_WINDOW (jw->dialog), TRUE);

  gtk_main ();

  return created_job;
}

void
gnc_job_edit (GtkWidget *parent, GncJob *job)
{
  JobWindow *jw;

  if (!job) return;

  jw = gnc_job_new_window (parent, gncJobGetBusiness(job),
			   gncJobGetCustomer(job), job);

  gtk_signal_connect (GTK_OBJECT (jw->dialog), "close",
		      GTK_SIGNAL_FUNC (gnc_job_on_close_cb), NULL);

  gtk_window_set_modal (GTK_WINDOW (jw->dialog), TRUE);

  gtk_main ();

  return;
}

/* Functions for widgets for job selection */

static gpointer gnc_job_edit_new_cb (gpointer arg)
{
  struct _job_select_window *sw = arg;

  return gnc_job_new (sw->toplevel, sw->business, NULL);
}

static void gnc_job_edit_edit_cb (gpointer arg, gpointer obj)
{
  GncJob *job = obj;
  struct _job_select_window *sw = arg;

  g_return_if_fail (arg != NULL);
  g_return_if_fail (obj != NULL);

  gnc_job_edit (sw->toplevel, job);
}

gpointer gnc_job_edit_new_select (gpointer job, GtkWidget *toplevel,
				  GncCustomer *cust)
{
  GncBusiness *business;
  GncJob *j = job;
  struct _job_select_window sw;

  if (!cust) return NULL;

  business = gncCustomerGetBusiness (cust);
  sw.toplevel = toplevel;
  sw.business = business;


  return
    gnc_ui_select_job_new (toplevel, business, cust, j);

  /*
  return
    gnc_ui_business_chooser_new (toplevel, job,
				 gncBusinessLookup (business,
						    GNC_JOB_MODULE_NAME),
				 gnc_job_edit_new_cb,
				 gnc_job_edit_edit_cb, &sw, cust);
  */
}
