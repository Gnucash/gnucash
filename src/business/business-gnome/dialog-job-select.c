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
#include "gnc-general-select.h"
#include "window-help.h"
#include "gnc-component-manager.h"

#include "gncJob.h"
#include "business-utils.h"
#include "dialog-job-select.h"
#include "dialog-job.h"
#include "dialog-order.h"
#include "dialog-invoice.h"

#define DIALOG_JOB_SELECT_CM_CLASS "dialog-job-select"

struct select_job_window {
  GtkWidget * dialog;
  GtkWidget * owner_hbox;
  GtkWidget * owner_select;
  GtkWidget * job_list;
  GtkWidget * showjobs_check;

  GtkWidget * parent;

  gint		component_id;

  GNCBook *	book;
  GncJob *	job;
  GncOwner	owner;
};


static void
select_job_job_selected_cb(GtkListItem *item, struct select_job_window *w)
{
  w->job = gtk_object_get_data (GTK_OBJECT (item), "item-list-pointer");
}

static void
update_job_select_picker (struct select_job_window *w)
{
  /* Clear out the existing choices */
  gtk_list_clear_items (GTK_LIST (w->job_list), 0, -1);

  /* Clear the watches */
  gnc_gui_component_clear_watches (w->component_id);

  /* 
   * Fill out the list of jobs from the current owner
   */

  if (w->owner.owner.undefined == NULL) {
    /* We have no owner, therefore there are no jobs */
    w->job = NULL;

  } else {
    GList *objs = NULL, *iterator, *itemlist = NULL;
    GtkWidget *li, *this_job = NULL;
    GncJob *saved_job;
    gboolean show_all = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON
						      (w->showjobs_check));

    /* Get the list of jobs from this owner */

    /* Save the current job */
    saved_job = w->job;
    w->job = NULL;

    /* Watch the owner for changes */
    gnc_gui_component_watch_entity (w->component_id,
				    gncOwnerGetGUID (&(w->owner)),
				    GNC_EVENT_MODIFY);

    /* Get the list of jobs */
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

    /* Build a list of ListItems.  objs is pre-sorted, so keep the order! */
    for (iterator = objs; iterator; iterator = iterator->next) {
      const gchar *label = gncObjectPrintable (GNC_JOB_MODULE_NAME,
					       iterator->data);

      li = gtk_list_item_new_with_label (label);
      gtk_object_set_data (GTK_OBJECT (li), "item-list-pointer", iterator->data);
      gtk_signal_connect (GTK_OBJECT (li), "select",
			  GTK_SIGNAL_FUNC (select_job_job_selected_cb), w);
      gtk_widget_show (li);
      itemlist = g_list_prepend (itemlist, li);
      if (iterator->data == saved_job)
	this_job = li;

      /* Watch this item in case it changes */
      gnc_gui_component_watch_entity (w->component_id,
				      gncJobGetGUID (iterator->data),
				      GNC_EVENT_MODIFY);
    }

    /* This will make sure we're in the right order at the end */
    itemlist = g_list_reverse (itemlist);

    /* put the items into the List container */
    if (itemlist)
      gtk_list_prepend_items (GTK_LIST (w->job_list), itemlist);

    /* And select the appropriate item in the list.  Choose the first
     * item if we don't already have one
     */
    if (this_job == NULL && itemlist)
      this_job = itemlist->data;

    if (this_job)
      gtk_list_select_child (GTK_LIST (w->job_list), this_job);

    /* Don't free itemlist -- it's the properly of the List widget! */
    g_list_free (objs);
  }
}

static void
update_owner_select_picker (struct select_job_window *w)
{
  gnc_owner_set_owner (w->owner_select, &(w->owner));
  update_job_select_picker (w);
}

static void
select_job_owner_changed_cb (GtkWidget *widget, struct select_job_window *w)
{
  gnc_owner_get_owner (widget, &(w->owner));
  update_job_select_picker (w);
}

static void
gnc_ui_select_job_ok_cb(GtkButton * button, gpointer user_data)
{
  struct select_job_window * w = user_data;

  if(w->job) {
    gnc_close_gui_component (w->component_id);
  } else {
    gnc_warning_dialog(_("You must select a job.\n"
                         "To create a new one, click \"New\""));
  }
}

static void
gnc_ui_select_job_order_cb(GtkButton * button, gpointer user_data)
{
  struct select_job_window * w = user_data;
  GncOwner owner;

  if(!w->job)
    return;

  gncOwnerInitJob (&owner, w->job);
  gnc_order_find (NULL, &owner, w->book);
}

static void
gnc_ui_select_job_invoice_cb(GtkButton * button, gpointer user_data)
{
  struct select_job_window * w = user_data;
  GncOwner owner;

  if(!w->job)
    return;

  gncOwnerInitJob (&owner, w->job);
  gnc_invoice_find (NULL, &owner, w->book);
}

static void
gnc_ui_select_job_help_cb(GtkButton * button, gpointer user_data)
{
  char *help_file = "";		/* XXX */

  helpWindow(NULL, NULL, help_file);
}

static void
gnc_ui_select_job_new_cb(GtkButton * button, gpointer user_data)
{
  struct select_job_window * w = user_data;
  GncJob * new_selection = gnc_job_new (w->dialog, &(w->owner), w->book);

  if (new_selection) {
    gncOwnerCopy (gncJobGetOwner (new_selection), &(w->owner));
    w->job = new_selection;
  }

  update_owner_select_picker (w);
}

static void
gnc_ui_select_job_edit_cb(GtkButton * button, gpointer user_data)
{
  struct select_job_window * w = user_data;

  if (w->job == NULL)
    return;

  gnc_ui_job_window_create (w->job);
}

static void
gnc_ui_select_job_cancel_cb(GtkButton * button, gpointer user_data)
{
  struct select_job_window * w = user_data;

  if (w) {
    w->job = NULL;
    gnc_close_gui_component (w->component_id);
  }
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

static void
refresh_handler (GHashTable *changes, gpointer data)
{
  struct select_job_window * sw = data;

  update_job_select_picker (sw);
}

static void
close_handler (gpointer data)
{
  struct select_job_window * sw = data;

  gnome_dialog_close (GNOME_DIALOG (sw->dialog));
}

static GncJob *
gnc_job_select (GtkWidget * parent, GncJob *start_job,
		GncOwner *ownerp, GNCBook *book, gboolean provide_select)
{
  struct select_job_window * win;
  GladeXML *xml;
  GncJob *retval;
  GtkWidget *owner_label, *owner_hbox;

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

  xml = gnc_glade_xml_new ("job.glade",
			   "Job Selector Dialog");

  /* Grab the widgets */
  win->dialog = glade_xml_get_widget (xml, "Job Selector Dialog");
  win->job_list = glade_xml_get_widget (xml, "job_list");
  win->showjobs_check = glade_xml_get_widget (xml, "showjobs_check");

  if(parent) {
    gnome_dialog_set_parent(GNOME_DIALOG(win->dialog), GTK_WINDOW(parent));
    gtk_window_set_modal(GTK_WINDOW(win->dialog), TRUE);
    win->parent = parent;
  }

  /* Connect the glade signals */
  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_job_new_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_job_new_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_job_cancel_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_job_cancel_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_job_help_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_job_help_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_job_ok_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_job_ok_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_job_edit_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_job_edit_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_job_order_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_job_order_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_job_invoice_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_job_invoice_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_job_showjobs_toggled_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_job_showjobs_toggled_cb), win);

  owner_label = glade_xml_get_widget (xml, "owner_label");
  owner_hbox = glade_xml_get_widget (xml, "owner_hbox");
  win->owner_select = gnc_owner_select_create (owner_label, owner_hbox,
					       book, &(win->owner));

  gtk_signal_connect (GTK_OBJECT (win->owner_select), "changed",
		      GTK_SIGNAL_FUNC (select_job_owner_changed_cb),
		      win);

  win->component_id = gnc_register_gui_component (DIALOG_JOB_SELECT_CM_CLASS,
						  refresh_handler,
						  close_handler, win);

  gtk_signal_connect (GTK_OBJECT(win->dialog), "close",
                      GTK_SIGNAL_FUNC(select_job_close), win);

  /* Setup the owner and job lists */
  win->job = start_job;
  update_owner_select_picker (win);

  /* Run the widget */
  gtk_widget_show_all (win->dialog);

  if (!provide_select) {
    GtkWidget *wid = glade_xml_get_widget (xml, "select_button");
    gtk_widget_hide_all (wid);
  }

  /* XXX: Hide the Order Button */
  {
    GtkWidget *wid = glade_xml_get_widget (xml, "order_button");
    gtk_widget_hide_all (wid);
  }

  gtk_main();

  /* exit */

  gnc_unregister_gui_component (win->component_id);

  retval = win->job;
  g_free(win);

  return retval;
}

void
gnc_job_find (GncJob *start_job, GncOwner *ownerp, GNCBook *book)
{
  gnc_job_select (NULL, start_job, ownerp, book, FALSE);
}

GncJob *
gnc_job_choose (GtkWidget * parent, GncJob *start_job,
		GncOwner *ownerp, GNCBook *book)
{
  return gnc_job_select (parent, start_job, ownerp, book, TRUE);
}
