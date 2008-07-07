/*
 * dialog-job.c -- Dialog for Job entry
 * Copyright (C) 2001, 2002 Derek Atkins
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "qof.h"
#include "dialog-search.h"
#include "search-param.h"

#include "gncBusiness.h"
#include "gncJob.h"
#include "gncJobP.h"

#include "business-gnome-utils.h"
#include "dialog-job.h"
#include "dialog-invoice.h"
#include "dialog-payment.h"

#define DIALOG_NEW_JOB_CM_CLASS "dialog-new-job"
#define DIALOG_EDIT_JOB_CM_CLASS "dialog-edit-job"

#define GCONF_SECTION_SEARCH "dialogs/business/job_search"

void gnc_job_window_ok_cb (GtkWidget *widget, gpointer data);
void gnc_job_window_cancel_cb (GtkWidget *widget, gpointer data);
void gnc_job_window_help_cb (GtkWidget *widget, gpointer data);
void gnc_job_window_destroy_cb (GtkWidget *widget, gpointer data);
void gnc_job_name_changed_cb (GtkWidget *widget, gpointer data);

typedef enum
{
  NEW_JOB,
  EDIT_JOB
} JobDialogType;

struct _job_select_window {
  GNCBook *	book;
  GncOwner *	owner;
  QueryNew *	q;
  GncOwner	owner_def;
};

struct _job_window {
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

};

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
  gncJobBeginEdit (job);

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
  gchar *string;

  /* Check for valid name */
  res = gtk_entry_get_text (GTK_ENTRY (jw->name_entry));
  if (safe_strcmp (res, "") == 0) {
    const char *message = _("The Job must be given a name.");
    gnc_error_dialog(jw->dialog, "%s", message);
    return FALSE;
  }

  /* Check for owner */
  gnc_owner_get_owner (jw->cust_edit, &(jw->owner));
  res = gncOwnerGetName (&(jw->owner));
  if (res == NULL || safe_strcmp (res, "") == 0) {
    const char *message = _("You must choose an owner for this job.");
    gnc_error_dialog(jw->dialog, "%s", message);
    return FALSE;
  }

  /* Set a valid id if one was not created */
  res = gtk_entry_get_text (GTK_ENTRY (jw->id_entry));
  if (safe_strcmp (res, "") == 0) {
    string = g_strdup_printf ("%.6" G_GINT64_FORMAT,
			      gncJobNextID(jw->book));
    gtk_entry_set_text (GTK_ENTRY (jw->id_entry), string);
    g_free(string);
  }

  /* Now save it off */
  {
    GncJob *job = jw_get_job (jw);
    if (job) {
      gnc_ui_to_job (jw, job);
    }
  }

  /* Ok, it's been saved... Change to an editor.. */
  jw->dialog_type = EDIT_JOB;

  return TRUE;
}

void
gnc_job_window_ok_cb (GtkWidget *widget, gpointer data)
{
  JobWindow *jw = data;

  /* Make sure this is ok */
  if (!gnc_job_verify_ok (jw))
    return;

  /* Now save off the job so we can return it */
  jw->created_job = jw_get_job (jw);
  jw->job_guid = *xaccGUIDNULL ();

  gnc_close_gui_component (jw->component_id);
}

void
gnc_job_window_cancel_cb (GtkWidget *widget, gpointer data)
{
  JobWindow *jw = data;

  gnc_close_gui_component (jw->component_id);
}

void
gnc_job_window_help_cb (GtkWidget *widget, gpointer data)
{
  gnc_gnome_help(HF_HELP, HL_USAGE);
}


void
gnc_job_window_destroy_cb (GtkWidget *widget, gpointer data)
{
  JobWindow *jw = data;
  GncJob *job = jw_get_job (jw);

  gnc_suspend_gui_refresh ();

  if (jw->dialog_type == NEW_JOB && job != NULL) {
    gncJobBeginEdit (job);
    gncJobDestroy (job);
    jw->job_guid = *xaccGUIDNULL ();
  }

  gnc_unregister_gui_component (jw->component_id);
  gnc_resume_gui_refresh ();

  g_free (jw);
}

void
gnc_job_name_changed_cb (GtkWidget *widget, gpointer data)
{
  JobWindow *jw = data;
  char *fullname, *title;
  const char *name, *id;

  if (!jw)
    return;

  name = gtk_entry_get_text (GTK_ENTRY (jw->name_entry));
  if (!name || *name == '\0')
    name = _("<No name>");

  id = gtk_entry_get_text (GTK_ENTRY (jw->id_entry));

  fullname = g_strconcat (name, " (", id, ")", (char *)NULL);

  if (jw->dialog_type == EDIT_JOB)
    title = g_strconcat (_("Edit Job"), " - ", fullname, (char *)NULL);
  else
    title = g_strconcat (_("New Job"), " - ", fullname, (char *)NULL);

  gtk_window_set_title (GTK_WINDOW (jw->dialog), title);

  g_free (fullname);
  g_free (title);
}

static void
gnc_job_window_close_handler (gpointer user_data)
{
  JobWindow *jw = user_data;

  gtk_widget_destroy (jw->dialog);
  jw->dialog = NULL;
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
    if (info && (info->event_mask & QOF_EVENT_DESTROY)) {
      gnc_close_gui_component (jw->component_id);
      return;
    }
  }
}

static gboolean
find_handler (gpointer find_data, gpointer user_data)
{
  const GUID *job_guid = find_data;
  JobWindow *jw = user_data;

  return(jw && guid_equal(&jw->job_guid, job_guid));
}

static JobWindow *
gnc_job_new_window (GNCBook *bookp, GncOwner *owner, GncJob *job)
{
  JobWindow *jw;
  GladeXML *xml;
  GtkWidget *owner_box, *owner_label;

  /*
   * Find an existing window for this job.  If found, bring it to
   * the front.
   */
  if (job) {
    GUID job_guid;

    job_guid = *gncJobGetGUID (job);
    jw = gnc_find_first_gui_component (DIALOG_EDIT_JOB_CM_CLASS,
				       find_handler, &job_guid);
    if (jw) {
      gtk_window_present (GTK_WINDOW(jw->dialog));
      return(jw);
    }
  }
  
  /*
   * No existing job window found.  Build a new one.
   */
  jw = g_new0 (JobWindow, 1);
  jw->book = bookp;
  gncOwnerCopy (owner, &(jw->owner)); /* save it off now, we know it's valid */

  /* Load the XML */
  xml = gnc_glade_xml_new ("job.glade", "Job Dialog");

  /* Find the dialog */
  jw->dialog = glade_xml_get_widget (xml, "Job Dialog");
  g_object_set_data (G_OBJECT (jw->dialog), "dialog_info", jw);

  /* Get entry points */
  jw->id_entry  = glade_xml_get_widget (xml, "id_entry");
  jw->name_entry = glade_xml_get_widget (xml, "name_entry");
  jw->desc_entry = glade_xml_get_widget (xml, "desc_entry");
  jw->active_check = glade_xml_get_widget (xml, "active_check");

  owner_box = glade_xml_get_widget (xml, "customer_hbox");
  owner_label = glade_xml_get_widget (xml, "owner_label");

  /* Setup signals */
  glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
                                     jw);
  /* Set initial entries */
  if (job != NULL) {
    jw->job_guid = *gncJobGetGUID (job);

    jw->dialog_type = EDIT_JOB;
    jw->cust_edit = gnc_owner_edit_create (owner_label, owner_box,
					     bookp, owner);

    gtk_entry_set_text (GTK_ENTRY (jw->id_entry), gncJobGetID (job));
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

    /* If we are passed a real owner, don't allow the user to change it */
    if (owner->owner.undefined) {
      jw->cust_edit = gnc_owner_edit_create (owner_label, owner_box,
					     bookp, owner);
    } else {
      jw->cust_edit = gnc_owner_select_create (owner_label, owner_box,
					       bookp, owner);
    }

    jw->component_id = gnc_register_gui_component (DIALOG_NEW_JOB_CM_CLASS,
						   gnc_job_window_refresh_handler,
						   gnc_job_window_close_handler,
						   jw);
  }

  gnc_job_name_changed_cb (NULL, jw);
  gnc_gui_component_watch_entity_type (jw->component_id,
				       GNC_JOB_MODULE_NAME,
				       QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

  gtk_widget_show_all (jw->dialog);

  return jw;
}

GncJob *
gnc_ui_job_new_return_handle (GncOwner *owner, GNCBook *book)
{
  JobWindow *jw;
  if (!book) return NULL;
  jw = gnc_ui_job_new (owner, book);
  return jw_get_job (jw);
}

JobWindow *
gnc_ui_job_new (GncOwner *ownerp, GNCBook *bookp)
{
  JobWindow *jw;
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

  jw = gnc_job_new_window (bookp, &owner, NULL);
  return jw;
}

JobWindow *
gnc_ui_job_edit (GncJob *job)
{
  JobWindow *jw;

  if (!job) return NULL;

  jw = gnc_job_new_window (gncJobGetBook(job), gncJobGetOwner(job), job);
  return jw;
}

/* Search functionality */

static void
edit_job_cb (gpointer *job_p, gpointer user_data)
{
  GncJob *job;

  g_return_if_fail (job_p && user_data);

  job = *job_p;

  if (!job)
    return;

  gnc_ui_job_edit (job);
}

static void
invoice_job_cb (gpointer *job_p, gpointer user_data)
{
  struct _job_select_window * sw = user_data;
  GncJob *job;
  GncOwner owner;

  g_return_if_fail (job_p && user_data);

  job = *job_p;
  if (!job)
    return;

  gncOwnerInitJob (&owner, job);
  gnc_invoice_search (NULL, &owner, sw->book);
}

static void
payment_job_cb (gpointer *job_p, gpointer user_data)
{
  struct _job_select_window *sw = user_data;
  GncOwner owner;
  GncJob *job;

  g_return_if_fail (job_p && user_data);

  job = *job_p;

  if (!job)
    return;

  gncOwnerInitJob (&owner, job);
  gnc_ui_payment_new (&owner, sw->book);
  return;
}

static gpointer
new_job_cb (gpointer user_data)
{
  struct _job_select_window *sw = user_data;
  JobWindow *jw;
  
  g_return_val_if_fail (user_data, NULL);

  jw = gnc_ui_job_new (sw->owner, sw->book);
  return jw_get_job (jw);
}

static void
free_userdata_cb (gpointer user_data)
{
  struct _job_select_window *sw = user_data;

  g_return_if_fail (sw);

  gncQueryDestroy (sw->q);
  g_free (sw);
}

GNCSearchWindow *
gnc_job_search (GncJob *start, GncOwner *owner, GNCBook *book)
{
  QueryNew *q, *q2 = NULL;
  GNCIdType type = GNC_JOB_MODULE_NAME;
  struct _job_select_window *sw;
  static GList *params = NULL;
  static GList *columns = NULL;
  static GNCSearchCallbackButton buttons[] = { 
    { N_("View/Edit Job"), edit_job_cb},
    { N_("View Invoices"), invoice_job_cb},
    { N_("Process Payment"), payment_job_cb},
    { NULL },
  };

  g_return_val_if_fail (book, NULL);

  /* Build parameter list in reverse order*/
  if (params == NULL) {
    params = gnc_search_param_prepend (params, _("Owner's Name"), NULL, type,
				       JOB_OWNER, OWNER_NAME, NULL);
    params = gnc_search_param_prepend (params, _("Only Active?"), NULL, type,
				       JOB_ACTIVE, NULL);
    params = gnc_search_param_prepend (params, _("Billing ID"), NULL, type,
				       JOB_REFERENCE, NULL);
    params = gnc_search_param_prepend (params, _("Job Number"), NULL, type,
				       JOB_ID, NULL);
    params = gnc_search_param_prepend (params, _("Job Name"), NULL, type,
				       JOB_NAME, NULL);
  }

  /* Build the column list in reverse order */
  if (columns == NULL) {
    columns = gnc_search_param_prepend (columns, _("Billing ID"), NULL, type,
					JOB_REFERENCE, NULL);
    columns = gnc_search_param_prepend (columns, _("Company"), NULL, type,
					JOB_OWNER, OWNER_NAME, NULL);
    columns = gnc_search_param_prepend (columns, _("Job Name"), NULL, type,
					JOB_NAME, NULL);
    columns = gnc_search_param_prepend (columns, _("ID #"), NULL, type,
					JOB_ID, NULL);
  }

  /* Build the queries */
  q = gncQueryCreateFor (type);
  gncQuerySetBook (q, book);

  /* If we have a start job but, for some reason, not an owner -- grab
   * the owner from the starting job.
   */
  if ((!owner || !gncOwnerGetGUID (owner)) && start)
    owner = gncJobGetOwner (start);

  /* If owner is supplied, limit all searches to invoices who's owner
   * is the supplied owner!  Show all invoices by this owner.
   */
  if (owner && gncOwnerGetGUID (owner)) {
    gncQueryAddGUIDMatch (q, g_slist_prepend
			  (g_slist_prepend (NULL, QUERY_PARAM_GUID),
			   JOB_OWNER),
			  gncOwnerGetGUID (owner), QUERY_AND);

    q2 = gncQueryCopy (q);
  }

#if 0
  if (start) {
    if (q2 == NULL)
      q2 = gncQueryCopy (q);

    gncQueryAddGUIDMatch (q2, g_slist_prepend (NULL, QUERY_PARAM_GUID),
			  gncJobGetGUID (start), QUERY_AND);
  }
#endif

  /* launch select dialog and return the result */
  sw = g_new0 (struct _job_select_window, 1);

  if (owner) {
    gncOwnerCopy (owner, &(sw->owner_def));
    sw->owner = &(sw->owner_def);
  }
  sw->book = book;
  sw->q = q;

  return gnc_search_dialog_create (type, _("Find Job"),
				   params, columns, q, q2, buttons, NULL,
				   new_job_cb, sw, free_userdata_cb,
				   GCONF_SECTION_SEARCH, NULL);
}

/* Functions for widgets for job selection */

GNCSearchWindow *
gnc_job_search_select (gpointer start, gpointer book)
{
  GncJob *j = start;
  GncOwner owner, *ownerp;

  if (!book) return NULL;

  if (j) {
    ownerp = gncJobGetOwner (j);
    gncOwnerCopy (ownerp, &owner);
  } else
    gncOwnerInitCustomer (&owner, NULL); /* XXX */

  return gnc_job_search (start, &owner, book);
}

GNCSearchWindow *
gnc_job_search_edit (gpointer start, gpointer book)
{
  if (start)
    gnc_ui_job_edit (start);

  return NULL;
}
