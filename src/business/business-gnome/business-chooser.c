/*
 * business-chooser.c -- General Selection Dialog for GNC Business Objects
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
#include "business-chooser.h"

struct business_chooser_window {
  GtkWidget * dialog;
  GtkWidget * choice_combo;
  GtkWidget * choice_entry;
  GtkWidget * showall_check;

  GNCBook * 		book;
  const GncBusinessObject * 	obj_type;
  gnc_business_chooser_new_cb	new_cb;
  gnc_business_chooser_edit_cb	edit_cb;
  gpointer	cb_arg;

  gpointer	selected;
};


static void
chooser_choice_changed_cb(GtkList *list, GtkWidget *li, gpointer user_data)
{
  struct business_chooser_window * w = user_data;

  w->selected = gtk_object_get_data (GTK_OBJECT (li), "list-item-pointer");
}

static void
update_selection_picker (struct business_chooser_window *w)
{
  GList *objs, *iterator;
  GtkWidget *li;
  GList *obj_list;
  gpointer selected;
  gboolean show_all = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON
						    (w->showall_check));

  /* Clear out the existing choices */
  gtk_list_clear_items (GTK_LIST (GTK_COMBO (w->choice_combo)->list), 0, -1);

  /* Save the existing selection */
  selected = w->selected;

  /* Get the list of objects */
  obj_list = (*(w->obj_type->get_list))(w->book, show_all);

  /* Build a list of strings (objs is pre-sorted, so keep the order!) */
  for (iterator = obj_list; iterator; iterator = iterator->next) {
    const gchar *label = (*(w->obj_type->printable))(iterator->data);

    li = gtk_list_item_new_with_label (label);
    gtk_object_set_data (GTK_OBJECT (li), "list-item-pointer", iterator->data);
    gtk_widget_show (li);
    gtk_container_add (GTK_CONTAINER (GTK_COMBO (w->choice_combo)->list), li);
  }

  /* revert the existing selection */
  w->selected = selected;

  /* If no selection, choose the first one in the list */
  if (!w->selected && obj_list)
    w->selected = obj_list->data;

  /* XXX: Should we verify that w->selected is a memeber of obj_list? */

  /* And set the current-selected item */
  gtk_entry_set_text (GTK_ENTRY (w->choice_entry),
		      ((w->selected) ?
		       (*(w->obj_type->printable))(w->selected) : ""));

  g_list_free (obj_list);
}

static void
gnc_ui_business_chooser_ok_cb(GtkButton * button, gpointer user_data)
{
  struct business_chooser_window * w = user_data;

  if(w->selected) {
    gnome_dialog_close(GNOME_DIALOG (w->dialog));
  } else {
    gnc_warning_dialog(_("You must make a selection.\n"
                         "To create a new one, click \"New\""));
  }
}

static void
gnc_ui_business_chooser_new_cb(GtkButton * button, gpointer user_data)
{
  struct business_chooser_window * w = user_data;

  if (w->new_cb) {
    gpointer new_selection;
    GtkWidget *toplevel;

    toplevel = gtk_widget_get_toplevel (GTK_WIDGET (button));
    new_selection = (*(w->new_cb))(w->cb_arg, toplevel);

    if (new_selection) {
      w->selected = new_selection;
      update_selection_picker (w);
    }
  }
}

static void
gnc_ui_business_chooser_edit_cb(GtkButton * button, gpointer user_data)
{
  struct business_chooser_window * w = user_data;

  if (! w->selected) {
    gnc_warning_dialog(_("You must choose a selection to edit.\n"));
    return;
  }

  if (w->edit_cb) {
    GtkWidget *toplevel = gtk_widget_get_toplevel (GTK_WIDGET (button));
    (*(w->edit_cb))(w->cb_arg, w->selected, toplevel);
  }

  update_selection_picker (w);
}

static void
gnc_ui_business_chooser_cancel_cb(GtkButton * button, gpointer user_data)
{
  struct business_chooser_window * w = user_data;

  if (w)
    gnome_dialog_close(GNOME_DIALOG (w->dialog));
}

static void
gnc_ui_business_chooser_showall_toggled_cb(GtkCheckButton * entry,
					       gpointer user_data)
{
  struct business_chooser_window * w = user_data;

  update_selection_picker (w);
}

static gint
business_chooser_close (GnomeDialog *dialog, gpointer data)
{
  gtk_main_quit ();
  return FALSE;
}

gpointer
gnc_ui_business_chooser_new (GtkWidget * parent,
			     gpointer orig_sel,
			     GNCBook *book, const char *type_name,
			     gnc_business_chooser_new_cb new_cb,
			     gnc_business_chooser_edit_cb edit_cb,
			     gpointer cbarg)
{
  struct business_chooser_window * win =
    g_new0(struct business_chooser_window, 1);
  GladeXML *xml;
  GtkWidget *choice_name_label;
  gpointer retval;

  if (!book || !type_name) return NULL;

  xml = gnc_glade_xml_new ("business-chooser.glade",
			   "Business Chooser Dialog");

  /* Grab the widgets */

  win->dialog = glade_xml_get_widget (xml, "Business Chooser Dialog");
  win->choice_combo = glade_xml_get_widget (xml, "choice_combo");
  win->choice_entry = glade_xml_get_widget (xml, "choice_entry");
  win->showall_check = glade_xml_get_widget (xml, "showall_check");

  /* Set the label */
  choice_name_label = glade_xml_get_widget (xml, "choice_name_label");
  gtk_label_set_text (GTK_LABEL (choice_name_label),
		      gncBusinessGetTypeLabel (type_name));

  if(parent) {
    gnome_dialog_set_parent(GNOME_DIALOG(win->dialog), GTK_WINDOW(parent));
  }

  /* Setup the signals */
  glade_xml_signal_connect_data
    (xml, "gnc_ui_business_chooser_ok_cb",
     GTK_SIGNAL_FUNC (gnc_ui_business_chooser_ok_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_business_chooser_new_cb",
     GTK_SIGNAL_FUNC (gnc_ui_business_chooser_new_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_business_chooser_edit_cb",
     GTK_SIGNAL_FUNC (gnc_ui_business_chooser_edit_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_business_chooser_cancel_cb",
     GTK_SIGNAL_FUNC (gnc_ui_business_chooser_cancel_cb), win);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_business_chooser_showall_toggled_cb",
     GTK_SIGNAL_FUNC (gnc_ui_business_chooser_showall_toggled_cb), win);

  gtk_signal_connect (GTK_OBJECT (GTK_COMBO (win->choice_combo)->list),
		      "select-child",
		      GTK_SIGNAL_FUNC(chooser_choice_changed_cb), win);

  gtk_signal_connect (GTK_OBJECT(win->dialog), "close",
                      GTK_SIGNAL_FUNC(business_chooser_close), win);

  /* Save the callbacks */
  win->book = book;
  win->obj_type = gncBusinessLookup (type_name);
  win->new_cb = new_cb;
  win->edit_cb = edit_cb;
  win->cb_arg = cbarg;

  /* Setup the menu */
  win->selected = orig_sel;
  update_selection_picker (win);

  /* Run the widget */
  gtk_window_set_modal(GTK_WINDOW(win->dialog), TRUE);
  gtk_widget_show (win->dialog);
  gtk_main();

  /* And exit */
  retval = win->selected;
  g_free(win);

  return retval;
}

