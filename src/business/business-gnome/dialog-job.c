/*
 * dialog-job.c -- Dialog for Job entry
 * Copyright (C) 2001, 2002 Derek Atkins
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
#include "gncJob.h"
#include "gncJobP.h"

#include "business-utils.h"
#include "dialog-job-select.h"
#include "dialog-job.h"
#include "dialog-order.h"
#include "dialog-invoice.h"

#define DIALOG_NEW_JOB_CM_CLASS "dialog-new-job"
#define DIALOG_EDIT_JOB_CM_CLASS "dialog-edit-job"

typedef enum
{
  NEW_JOB,
  EDIT_JOB
} JobDialogType;

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
  GNCBook *	book;
  GncJob *	created_job;

  GncOwner	owner;

} JobWindow;

static GncJob *
jw_get_job (JobWindow *jw)
{
  if (!jw)
    return NULL;

  return gncJobLookup (jw->book, &jw->job_guid);
}

static void gnc_ui_to_job (JobWindow *jw, GncJob *job)
{
  gnc_suspend_gui_refresh ();
  gncJobSetID (job, gtk_editable_get_chars (GTK_EDITABLE (jw->id_entry),
					    0, -1));
  gncJobSetName (job, gtk_editable_get_chars (GTK_EDITABLE (jw->name_entry),
					      0, -1));
  gncJobSetReference (job, gtk_editable_get_chars
		      (GTK_EDITABLE (jw->desc_entry), 0, -1));
  gncJobSetActive (job, gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON
						      (jw->active_check)));
  {
    GncOwner * old = gncJobGetOwner (job);
    gnc_owner_get_owner (jw->cust_edit, &(jw->owner));
    if (! gncOwnerEqual (old, &(jw->owner)))
      gncJobSetOwner (job, &(jw->owner));
  }

  gncJobCommitEdit (job);
  gnc_resume_gui_refresh ();
}

static gboolean
gnc_job_verify_ok (JobWindow *jw)
{
  const char *res;

  /* Check for valid id */
  res = gtk_entry_get_text (GTK_ENTRY (jw->id_entry));
  if (safe_strcmp (res, "") == 0) {
    const char *message = _("The Job must be given an ID.");
    gnc_error_dialog_parented(GTK_WINDOW(jw->dialog), message);
    return FALSE;
  }

  /* Check for valid name */
  res = gtk_entry_get_text (GTK_ENTRY (jw->name_entry));
  if (safe_strcmp (res, "") == 0) {
    const char *message = _("The Job must be given a name.");
    gnc_error_dialog_parented(GTK_WINDOW(jw->dialog), message);
    return FALSE;
  }

  /* Check for owner */
  gnc_owner_get_owner (jw->cust_edit, &(jw->owner));
  res = gncOwnerGetName (&(jw->owner));
  if (res == NULL || safe_strcmp (res, "") == 0) {
    const char *message = _("You must choose an owner for this job.");
    gnc_error_dialog_parented(GTK_WINDOW(jw->dialog), message);
    return FALSE;
  }

  /* Now save it off */
  {
    GncJob *job = jw_get_job (jw);
    if (job) {
      gnc_ui_to_job (jw, job);
    }
    jw->created_job = job;
  }

  return TRUE;
}

static void
gnc_job_window_ok_cb (GtkWidget *widget, gpointer data)
{
  JobWindow *jw = data;

  if (!gnc_job_verify_ok (jw))
    return;

  /* Make sure this is ok */
  jw->job_guid = *xaccGUIDNULL ();

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
  char *help_file = "";		/* xxx */

  /* XXX */

  helpWindow(NULL, NULL, help_file);
}

static void
gnc_job_order_cb(GtkButton * button, gpointer user_data)
{
  JobWindow *jw = user_data;
  GncOwner owner;

  if (!gnc_job_verify_ok (jw))
    return;

  gncOwnerInitJob (&owner, jw_get_job (jw));
  gnc_order_find (jw->dialog, NULL, &owner, jw->book);
}

static void
gnc_job_invoice_cb(GtkButton * button, gpointer user_data)
{
  JobWindow *jw = user_data;
  GncOwner owner;

  if (!gnc_job_verify_ok (jw))
    return;

  gncOwnerInitJob (&owner, jw_get_job (jw));
  gnc_invoice_find (jw->dialog, NULL, &owner, jw->book);
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
gnc_job_new_window (GtkWidget *parent, GNCBook *bookp, GncOwner *owner,
		    GncJob *job)
{
  JobWindow *jw;
  GladeXML *xml;
  GtkWidget *owner_box, *owner_label;
  GnomeDialog *jwd;
  GtkObject *jwo;

  jw = g_new0 (JobWindow, 1);
  jw->book = bookp;
  gncOwnerCopy (owner, &(jw->owner)); /* save it off now, we know it's valid */

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

  owner_box = glade_xml_get_widget (xml, "customer_hbox");
  owner_label = glade_xml_get_widget (xml, "owner_label");

  /* Connect buttons */
  gnome_dialog_button_connect (jwd, 0,
			       GTK_SIGNAL_FUNC(gnc_job_window_ok_cb), jw);
  gnome_dialog_button_connect (jwd, 1,
			       GTK_SIGNAL_FUNC(gnc_job_window_cancel_cb), jw);
  gnome_dialog_button_connect (jwd, 2,
			       GTK_SIGNAL_FUNC(gnc_job_window_help_cb), jw);

  glade_xml_signal_connect_data
    (xml, "gnc_job_order_cb", GTK_SIGNAL_FUNC (gnc_job_order_cb), jw);

  glade_xml_signal_connect_data
    (xml, "gnc_job_invoice_cb", GTK_SIGNAL_FUNC (gnc_job_invoice_cb), jw);

  /* Setup signals (XXX) */
  gtk_signal_connect (jwo, "destroy",
		      GTK_SIGNAL_FUNC(gnc_job_window_destroy_cb), jw);

  gtk_signal_connect(GTK_OBJECT (jw->id_entry), "changed",
		     GTK_SIGNAL_FUNC(gnc_job_name_changed_cb), jw);

  gtk_signal_connect(GTK_OBJECT (jw->name_entry), "changed",
		     GTK_SIGNAL_FUNC(gnc_job_name_changed_cb), jw);


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
    jw->cust_edit = gnc_owner_edit_create (owner_label, owner_box,
					     bookp, owner);

    gtk_entry_set_text (GTK_ENTRY (jw->id_entry), gncJobGetID (job));
    gtk_entry_set_editable (GTK_ENTRY (jw->id_entry), FALSE);
    gtk_entry_set_text (GTK_ENTRY (jw->name_entry), gncJobGetName (job));
    gtk_entry_set_text (GTK_ENTRY (jw->desc_entry), gncJobGetReference (job));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (jw->active_check),
                                gncJobGetActive (job));

    jw->component_id = gnc_register_gui_component (DIALOG_EDIT_JOB_CM_CLASS,
						   gnc_job_window_refresh_handler,
						   gnc_job_window_close_handler,
						   jw);
  } else {
    job = gncJobCreate (bookp);
    gncJobSetOwner (job, owner);
    jw->job_guid = *gncJobGetGUID (job);
      
    jw->dialog_type = NEW_JOB;
    jw->cust_edit = gnc_owner_select_create (owner_label, owner_box,
					     bookp, owner);

    gtk_entry_set_text (GTK_ENTRY (jw->id_entry),
			g_strdup_printf ("%.6d", gncJobNextID(bookp)));

    jw->component_id = gnc_register_gui_component (DIALOG_NEW_JOB_CM_CLASS,
						   gnc_job_window_refresh_handler,
						   gnc_job_window_close_handler,
						   jw);
  }

  gnc_job_name_changed_cb (NULL, jw);
  gnc_gui_component_watch_entity_type (jw->component_id,
				       GNC_JOB_MODULE_NAME,
				       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  gtk_widget_show_all (jw->dialog);

  return jw;
}

GncJob *
gnc_job_new (GtkWidget *parent, GncOwner *ownerp, GNCBook *bookp)
{
  JobWindow *jw;
  GncJob *created_job = NULL;
  GncOwner owner;

  /* Make sure required options exist */
  if (!bookp) return NULL;

  if (ownerp) {
    g_return_val_if_fail ((gncOwnerGetType (ownerp) == GNC_OWNER_CUSTOMER) ||
			  (gncOwnerGetType (ownerp) == GNC_OWNER_VENDOR),
			  NULL);
    gncOwnerCopy (ownerp, &owner);
  } else
    gncOwnerInitCustomer (&owner, NULL); /* XXX */

  jw = gnc_job_new_window (parent, bookp, &owner, NULL);

  gtk_signal_connect (GTK_OBJECT (jw->dialog), "close",
		      GTK_SIGNAL_FUNC (gnc_job_on_close_cb), &created_job);

  // gtk_window_set_modal (GTK_WINDOW (jw->dialog), TRUE);

  gtk_main ();

  return created_job;
}

void
gnc_job_edit (GtkWidget *parent, GncJob *job)
{
  JobWindow *jw;

  if (!job) return;

  jw = gnc_job_new_window (parent, gncJobGetBook(job),
			   gncJobGetOwner(job), job);

  gtk_signal_connect (GTK_OBJECT (jw->dialog), "close",
		      GTK_SIGNAL_FUNC (gnc_job_on_close_cb), NULL);

  gtk_window_set_modal (GTK_WINDOW (jw->dialog), TRUE);

  gtk_main ();

  return;
}

/* Functions for widgets for job selection */

gpointer
gnc_job_edit_new_edit (gpointer book, gpointer jobp, GtkWidget *toplevel)
{
  GncJob *j = jobp;

  if (!j) return NULL;

  gnc_job_edit (toplevel, j);

  return j;
}

gpointer
gnc_job_select_new_select (gpointer bookp, gpointer jobp, GtkWidget *parent)
{
  GncJob *j = jobp;
  GNCBook *book = bookp;
  GncOwner owner, *ownerp;

  if (!book) return NULL;

  if (j) {
    ownerp = gncJobGetOwner (j);
    gncOwnerCopy (ownerp, &owner);
  } else
    gncOwnerInitCustomer (&owner, NULL); /* XXX */

  return gnc_job_choose (parent, j, &owner, book);
}
