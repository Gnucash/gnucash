/*
 * dialog-job-select.c -- Job Selection Dialog for GNC Business Objects
 *
 * Written By: Derek Atkins <warlord@MIT.EDU>
 * Copyright (C) 2001
 */

#include "config.h"

#include <gnome.h>

#include "dialog-utils.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"

#include "gncBusiness.h"
#include "gncCustomer.h"
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

  GncBusiness *	business;
  GncCustomer *	customer;
  GncJob *	job;

  const GncBusinessObject *job_type, *cust_type;
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
  if (w->customer == NULL) {
    w->job = NULL;
  } else {
    /* Save the current job */
    saved_job = w->job;

    objs = gncCustomerGetJoblist (w->customer, show_all);

    /* Build a list of strings (objs is pre-sorted, so keep the order!) */
    for (iterator = objs; iterator; iterator = iterator->next) {
      const gchar *label = (*(w->job_type->printable))(iterator->data);

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
      label = (*(w->job_type->printable))(w->job);
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
  GncCustomer * saved_cust;
  gboolean show_all = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON
						    (w->showcust_check));

  /* Clear out the existing choices */
  gtk_list_clear_items (GTK_LIST (GTK_COMBO (w->customer_combo)->list), 0, -1);

  /* Save the existing selection */
  saved_cust = w->customer;

  /* Get the list of objects */
  custs = (*(w->cust_type->get_list))(w->business, show_all);

  /* Build a list of strings (objs is pre-sorted, so keep the order!) */
  for (iterator = custs; iterator; iterator = iterator->next) {
    const gchar *label = (*(w->cust_type->printable))(iterator->data);

    li = gtk_list_item_new_with_label (label);
    gtk_object_set_data (GTK_OBJECT (li), "item-list-pointer", iterator->data);
    gtk_widget_show (li);
    gtk_container_add (GTK_CONTAINER (GTK_COMBO (w->customer_combo)->list), li);
  }

  /* Revert the saved customer */
  w->customer = saved_cust;

  if (! custs) {
    /* no customers -- update the job picker (it will clear itself) */

    w->customer = NULL;
    update_job_select_picker (w);

  } else if (g_list_index (custs, w->customer) == -1) {
    /* customer doesn't exist anymore!?!  Clear out the job, too */

    w->customer = custs->data;
    update_job_select_picker (w);
  }      

  /* And now set the current-selected item */
  {
    const char * label;

    if (w->customer)
      label = (*(w->cust_type->printable))(w->customer);
    else
      label = "";
  
    gtk_entry_set_text (GTK_ENTRY (w->customer_entry), label);
  }

  g_list_free (custs);
}

static void
select_job_customer_changed_cb(GtkList *list, GtkWidget *li,
			       gpointer user_data)
{
  struct select_job_window * w = user_data;
  GList *node;

  if (!li)
    return;

  w->customer = gtk_object_get_data (GTK_OBJECT (li), "item-list-pointer");
  w->job = NULL;
  update_job_select_picker (w);
}

static void
select_job_job_changed_cb(GtkList *list, GtkWidget *li, gpointer user_data)
{
  struct select_job_window * w = user_data;
  GList *node;

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
  GncJob * new_selection = gnc_job_new (w->dialog, w->business, w->customer);

  if (new_selection) {
    w->customer = gncJobGetCustomer (new_selection);
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
  w->customer = gncJobGetCustomer (w->job);
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
gnc_ui_select_job_new (GtkWidget * parent, GncBusiness *business,
		       GncCustomer *cust, GncJob *job)
{
  struct select_job_window * win =
    g_new0(struct select_job_window, 1);
  GladeXML *xml;
  GtkWidget *choice_name_label;
  GncJob *retval;

  g_return_val_if_fail (business != NULL, NULL);
  win->business = business;

  win->cust_type = gncBusinessLookup (GNC_CUSTOMER_MODULE_NAME);
  win->job_type = gncBusinessLookup (GNC_JOB_MODULE_NAME);
  
  xml = gnc_glade_xml_new ("job.glade",
			   "Job Selector Dialog");

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
  win->customer = cust;
  update_customer_select_picker (win);
  win->job = job;
  update_job_select_picker (win);

  /* Run the widget */
  gtk_window_set_modal(GTK_WINDOW(win->dialog), TRUE);
  gtk_widget_show (win->dialog);
  gtk_main();

  /* exit */
  retval = win->job;
  g_free(win);

  return retval;
}

