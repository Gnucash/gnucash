/*
 * dialog-job-select.c -- Job Selection Dialog for GNC Business Objects
 *
 * Written By: Derek Atkins <warlord@MIT.EDU>
 * Copyright (C) 2001, 2002 Derek Atkins
 */

#include "config.h"

#include <gnome.h>

#include "dialog-utils.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gncObject.h"
#include "QueryNew.h"

#include "gncJob.h"
#include "dialog-job-select.h"
#include "dialog-job.h"

struct select_job_window {
  GtkWidget * dialog;
  GtkWidget * customer_combo;
  GtkWidget * customer_entry;
  GtkWidget * job_combo;
  GtkWidget * job_entry;
  GtkWidget * showcust_check;
  GtkWidget * showjobs_check;

  GNCBook *	book;
  GncJob *	job;
  GncOwner	owner;

  QueryNew *	query;
  GNCIdTypeConst owner_type;
};


static void
update_job_select_picker (struct select_job_window *w)
{
  GList *objs = NULL, *iterator;
  GtkWidget *li;
  GncJob *saved_job;
  gboolean show_all = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON
						    (w->showjobs_check));

  /* Clear out the existing choices */
  gtk_list_clear_items (GTK_LIST (GTK_COMBO (w->job_combo)->list), 0, -1);

  /* Get the list of objects */
  if (w->owner.owner.undefined == NULL) {
    w->job = NULL;
  } else {
    /* Save the current job */
    saved_job = w->job;

    switch (gncOwnerGetType (&(w->owner))) {
    case GNC_OWNER_CUSTOMER:
      objs = gncCustomerGetJoblist (gncOwnerGetCustomer (&(w->owner)),
				    show_all);
      break;
    case GNC_OWNER_VENDOR:
      objs = gncVendorGetJoblist (gncOwnerGetVendor (&(w->owner)),
				  show_all);
      break;
    default:
    }

    /* Build a list of strings (objs is pre-sorted, so keep the order!) */
    for (iterator = objs; iterator; iterator = iterator->next) {
      const gchar *label = gncObjectPrintable (GNC_JOB_MODULE_NAME,
					       iterator->data);

      li = gtk_list_item_new_with_label (label);
      gtk_object_set_data (GTK_OBJECT (li), "item-list-pointer", iterator->data);
      gtk_widget_show (li);
      gtk_container_add (GTK_CONTAINER (GTK_COMBO (w->job_combo)->list), li);
    }

    /* And revert */
    w->job = saved_job;
  }

  /* No job list.  Eit */
  if (!objs)
    w->job = NULL;

  else if (g_list_index (objs, w->job) == -1) {
    /* job doesn't exist anymore!?! Choose something reasonable */
    w->job = objs->data;
  }

  /* And set the text label */
  {
    const char * label;
    if (w->job)
      label = gncObjectPrintable (GNC_JOB_MODULE_NAME, w->job);
    else
      label = "";

    gtk_entry_set_text (GTK_ENTRY (w->job_entry), label);
  }

  g_list_free (objs);
}

static void
update_customer_select_picker (struct select_job_window *w)
{
  GList *custs, *iterator;
  GtkWidget *li;
  GncOwner saved_owner;
  gboolean show_all = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON
						    (w->showcust_check));

  /* Clear out the existing choices */
  gtk_list_clear_items (GTK_LIST (GTK_COMBO (w->customer_combo)->list), 0, -1);

  /* Save the existing selection */
  gncOwnerCopy (&(w->owner), &saved_owner);

  /* Get the list of objects */
  /* XXX: use show_all in the query */
  custs = gncQueryRun (w->query, w->owner_type);

  /* Build a list of strings (objs is pre-sorted, so keep the order!) */
  for (iterator = custs; iterator; iterator = iterator->next) {
    const gchar *label = gncObjectPrintable (w->owner_type, iterator->data);

    li = gtk_list_item_new_with_label (label);
    gtk_object_set_data (GTK_OBJECT (li), "item-list-pointer", iterator->data);
    gtk_widget_show (li);
    gtk_container_add (GTK_CONTAINER (GTK_COMBO (w->customer_combo)->list), li);
  }

  /* Revert the saved customer */
  gncOwnerCopy (&saved_owner, &(w->owner));

  if (! custs) {
    /* no customers -- update the job picker (it will clear itself) */

    w->owner.owner.undefined = NULL;
    update_job_select_picker (w);

  } else if (g_list_index (custs, w->owner.owner.undefined) == -1) {
    /* customer doesn't exist anymore!?!  Clear out the job, too */

    w->owner.owner.undefined = custs->data;
    update_job_select_picker (w);
  }      

  /* And now set the current-selected item */
  {
    const char * label;

    if (w->owner.owner.undefined)
      label = gncObjectPrintable (w->owner_type, w->owner.owner.undefined);
    else
      label = "";
  
    gtk_entry_set_text (GTK_ENTRY (w->customer_entry), label);
  }
}

static void
select_job_customer_changed_cb(GtkList *list, GtkWidget *li,
			       gpointer user_data)
{
  struct select_job_window * w = user_data;

  if (!li)
    return;

  w->owner.owner.undefined = 
    gtk_object_get_data (GTK_OBJECT (li), "item-list-pointer");
  w->job = NULL;
  update_job_select_picker (w);
}

static void
select_job_job_changed_cb(GtkList *list, GtkWidget *li, gpointer user_data)
{
  struct select_job_window * w = user_data;

  if (!li)
    return;

  w->job = gtk_object_get_data (GTK_OBJECT (li), "item-list-pointer");
}

static void
gnc_ui_select_job_ok_cb(GtkButton * button, gpointer user_data)
{
  struct select_job_window * w = user_data;

  if(w->job) {
    gnome_dialog_close(GNOME_DIALOG (w->dialog));
  } else {
    gnc_warning_dialog(_("You must select a job.\n"
                         "To create a new one, click \"New\""));
  }
}

static void
gnc_ui_select_job_new_cb(GtkButton * button, gpointer user_data)
{
  struct select_job_window * w = user_data;
  GncJob * new_selection = gnc_job_new (w->dialog, w->book, &(w->owner));

  if (new_selection) {
    gncOwnerCopy (gncJobGetOwner (new_selection), &(w->owner));
    update_customer_select_picker (w);
    w->job = new_selection;
    update_job_select_picker (w);
  }
}

static void
gnc_ui_select_job_edit_cb(GtkButton * button, gpointer user_data)
{
  struct select_job_window * w = user_data;

  if (w->job == NULL)
    return;

  gnc_job_edit (w->dialog, w->job);
  gncOwnerCopy (gncJobGetOwner (w->job), &(w->owner));
  update_customer_select_picker (w);
  update_job_select_picker (w);
}

static void
gnc_ui_select_job_cancel_cb(GtkButton * button, gpointer user_data)
{
  struct select_job_window * w = user_data;

  if (w)
    gnome_dialog_close(GNOME_DIALOG (w->dialog));
}

static void
gnc_ui_select_job_showcust_toggled_cb(GtkCheckButton * entry,
				       gpointer user_data)
{
  struct select_job_window * w = user_data;

  update_customer_select_picker (w);
}

static void
gnc_ui_select_job_showjobs_toggled_cb(GtkCheckButton * entry,
				      gpointer user_data)
{
  struct select_job_window * w = user_data;

  update_job_select_picker (w);
}

static gint
select_job_close (GnomeDialog *dialog, gpointer data)
{
  gtk_main_quit ();
  return FALSE;
}

GncJob *
gnc_ui_select_job_new (GtkWidget * parent, GNCBook *book,
		       GncOwner *ownerp, GncJob *job)
{
  struct select_job_window * win;
  GladeXML *xml;
  GncJob *retval;
  GtkWidget *owner_label;
  const char * type_name;

  g_return_val_if_fail (book != NULL, NULL);

  win = g_new0(struct select_job_window, 1);
  win->book = book;

  if (ownerp) {
    g_return_val_if_fail ((gncOwnerGetType (ownerp) == GNC_OWNER_CUSTOMER) ||
			  (gncOwnerGetType (ownerp) == GNC_OWNER_VENDOR),
			  NULL);
    gncOwnerCopy (ownerp, &(win->owner));
  } else
    gncOwnerInitCustomer (&(win->owner), NULL);	/* XXX */

  switch (gncOwnerGetType (&(win->owner))) {
  case GNC_OWNER_CUSTOMER:
    type_name = GNC_CUSTOMER_MODULE_NAME;
    break;
  case GNC_OWNER_VENDOR:
    type_name = GNC_VENDOR_MODULE_NAME;
    break;
  default:
    g_warning ("Cannot handle this owner type");
    return NULL;
  }
  win->owner_type = type_name;
  win->query = gncQueryCreate ();
  gncQuerySetBook (win->query, book);
  
  xml = gnc_glade_xml_new ("job.glade",
			   "Job Selector Dialog");

  owner_label = glade_xml_get_widget (xml, "owner_label");
  gtk_label_set_text (GTK_LABEL (owner_label),
		      gncObjectGetTypeLabel (type_name));

  /* Grab the widgets */

  win->dialog = glade_xml_get_widget (xml, "Job Selector Dialog");
  win->customer_combo = glade_xml_get_widget (xml, "customer_combo");
  win->customer_entry = glade_xml_get_widget (xml, "customer_entry");
  win->showcust_check = glade_xml_get_widget (xml, "showcust_check");
  win->job_combo = glade_xml_get_widget (xml, "job_combo");
  win->job_entry = glade_xml_get_widget (xml, "job_entry");
  win->showjobs_check = glade_xml_get_widget (xml, "showjobs_check");

  if(parent) {
    gnome_dialog_set_parent(GNOME_DIALOG(win->dialog), GTK_WINDOW(parent));
  }

  /* Setup the signals */
  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_job_ok_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_job_ok_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_job_new_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_job_new_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_job_edit_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_job_edit_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_job_cancel_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_job_cancel_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_job_showjobs_toggled_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_job_showjobs_toggled_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_job_showcust_toggled_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_job_showcust_toggled_cb), win);

  gtk_signal_connect (GTK_OBJECT (GTK_COMBO (win->customer_combo)->list),
		      "select-child",
		      GTK_SIGNAL_FUNC(select_job_customer_changed_cb), win);

  gtk_signal_connect (GTK_OBJECT (GTK_COMBO (win->job_combo)->list),
		      "select-child",
		      GTK_SIGNAL_FUNC(select_job_job_changed_cb), win);

  gtk_signal_connect (GTK_OBJECT(win->dialog), "close",
                      GTK_SIGNAL_FUNC(select_job_close), win);

  /* Setup the menu */
  update_customer_select_picker (win);
  win->job = job;
  update_job_select_picker (win);

  /* Run the widget */
  gtk_window_set_modal(GTK_WINDOW(win->dialog), TRUE);
  gtk_widget_show (win->dialog);
  gtk_main();

  /* exit */
  retval = win->job;
  gncQueryDestroy (win->query);
  g_free(win);

  return retval;
}

