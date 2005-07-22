/*
 * dialog-preferences.c -- preferences dialog
 * Copyright (C) 2005 David Hampton
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#include "config.h"

#include <gtk/gtk.h>
#include <glade/glade.h>

#include "dialog-utils.h"
#include "gnc-gconf-utils.h"
#include "gnc-trace.h"
#include "gnc-ui.h"
#include "gnc-component-manager.h"
#include "dialog-preferences.h"

#define DIALOG_PREFERENCES_CM_CLASS	"dialog-newpreferences"
#define GCONF_SECTION			"dialogs/preferences"
#define PREFIX_LEN			sizeof("gconf/") - 1

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_PREFS;

void gnc_preferences_response_cb(GtkDialog *dialog, gint response, GtkDialog *unused);
void gnc_reset_warnings_select_all_cb (GtkButton *button, gpointer user_data);
void gnc_reset_warnings_unselect_all_cb (GtkButton *button, gpointer user_data);
void gnc_reset_warnings_response_cb (GtkDialog *dialog, gint arg1, gpointer user_data);


typedef struct addition_t {
  gchar *filename;
  gchar *widgetname;
  gchar *tabname;
  gboolean full_page;
} addition;

GSList *add_ins = NULL;

static void
gnc_preferences_add_page_internal (const gchar *filename,
				   const gchar *widgetname,
				   const gchar *tabname,
				   gboolean full_page)
{
  addition *add_in;

  ENTER("file %s, widget %s, tab %s full page %d",
	filename, widgetname, tabname, full_page);
  add_in = g_malloc(sizeof(addition));
  if (add_in == NULL) {
    g_critical("Unable to allocate memory.\n");
    LEAVE("no memory");
    return;
  }

  add_in->filename   = g_strdup(filename);
  add_in->widgetname = g_strdup(widgetname);
  add_in->tabname    = g_strdup(tabname);
  add_in->full_page  = full_page;
  if (!add_in->filename || !add_in->widgetname || !add_in->tabname) {
    g_critical("Unable to allocate memory.\n");
    g_free(add_in->filename);
    g_free(add_in->widgetname);
    g_free(add_in->tabname);
    LEAVE("no memory");
    return;
  }
  add_ins = g_slist_append(add_ins, add_in);
  LEAVE("");
}


void
gnc_preferences_add_page (const gchar *filename,
			  const gchar *widgetname,
			  const gchar *tabname)
{
  gnc_preferences_add_page_internal(filename, widgetname, tabname, TRUE);
}

void
gnc_preferences_add_to_page (const gchar *filename,
			     const gchar *widgetname,
			     const gchar *tabname)
{
  gnc_preferences_add_page_internal(filename, widgetname, tabname, FALSE);
}

/****************************************/

struct find_data {
  GtkNotebook *notebook;
  const gchar *tabname;
  gint index;
  gboolean exact;
};

struct copy_data {
  GtkTable *table_from;
  GtkTable *table_to;
  gint row_offset;
};

struct page_data {
  GtkNotebook *notebook;
  GList *widgets;
};

static void
gnc_prefs_find_page (GtkWidget *child,
		     gpointer data)
{
  struct find_data *location;
  const gchar *child_tabname;
  gint index;

  g_return_if_fail(child != NULL);
  g_return_if_fail(data != NULL);

  ENTER("");
  location = data;
  if (location->index >= 0) {
    LEAVE("already found");
    return;
  }
  child_tabname = gtk_notebook_get_tab_label_text(location->notebook, child);
  index = gtk_notebook_page_num(location->notebook, child);
  DEBUG("Checking index %d, name %s", index, child_tabname);

  if (location->exact) {
    if (strcmp(location->tabname, child_tabname) == 0) {
      location->index = index;
      LEAVE("index is %d", index);
      return;
    }
    LEAVE("not page %d", index);
    return;
  }

  if (strcmp(location->tabname, child_tabname) > 0) {
    LEAVE("after page %d", index);
    return;
  }

  location->index = index;
  LEAVE("insert at offset %d", index);
}

static void
gnc_prefs_copy_table_entry (GtkWidget *child,
			    gpointer data)
{
  struct copy_data *copydata = data;
  GtkAttachOptions x_opts, y_opts;
  gint bottom, top, left, right, x_pad, y_pad;

  ENTER("child %p, copy data %p", child, data);
  gtk_container_child_get(GTK_CONTAINER(copydata->table_from), child,
			  "bottom-attach", &bottom,
			  "left-attach", &left,
			  "right-attach", &right,
			  "top-attach", &top,
			  "x-options", &x_opts,
			  "x-padding", &x_pad,
			  "y-options", &y_opts,
			  "y-padding", &y_pad,
			  NULL);

  gtk_widget_ref(child);
  gtk_container_remove(GTK_CONTAINER(copydata->table_from), child);
  gtk_table_attach(copydata->table_to, child, left, right,
		   top + copydata->row_offset, bottom + copydata->row_offset,
		   x_opts, y_opts, x_pad, y_pad);
  gtk_widget_unref(child);
  LEAVE(" ");
}

static void
gnc_preferences_build_page (gpointer data,
			    gpointer user_data)
{
  GladeXML *xml;
  GtkWidget *existing_content, *new_content, *label;
  GtkNotebook *notebook;
  struct page_data *page_data;
  addition *add_in;
  struct find_data location;
  struct copy_data copydata;
  gint rows, cols;
  GList *interesting;

  ENTER("add_in %p, notebook %p", data, user_data);
  add_in = (addition *)data;
  page_data = (struct page_data *) user_data;
  notebook = page_data->notebook;

  DEBUG("Opening %s to get %s:", add_in->filename, add_in->widgetname);
  xml = gnc_glade_xml_new(add_in->filename, add_in->widgetname);
  new_content = glade_xml_get_widget(xml, add_in->widgetname);
  DEBUG("done");

  /* Add to the list of interesting widgets */
  interesting = glade_xml_get_widget_prefix(xml, "gconf");
  page_data->widgets = g_list_concat(page_data->widgets, interesting);

  /* Prepare for recursion */
  location.notebook = notebook;
  location.index = -1;
  location.tabname = add_in->tabname;
  location.exact = FALSE;

  if (add_in->full_page) {
    gtk_container_foreach(GTK_CONTAINER(notebook), gnc_prefs_find_page,
			  &location);
    label = gtk_label_new(add_in->tabname);
    gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
    gtk_notebook_insert_page(notebook, new_content, label, location.index);
    LEAVE("added page at index %d", location.index);
    return;
  }

  /* Copied tables must match the size of the main table */
  g_assert(GTK_IS_TABLE(new_content));
  g_object_get(G_OBJECT(new_content), "n-columns", &cols, NULL);
  g_assert(cols == 4);

  /* Does the page exist or must we create it */
  location.exact = TRUE;
  gtk_container_foreach(GTK_CONTAINER(notebook), gnc_prefs_find_page,
			&location);
  if (location.index == -1) {
    /* No existing content with this name.  Create a blank page */
    location.exact = FALSE;
    existing_content = gtk_table_new(0, 4, FALSE);
    gtk_container_foreach(GTK_CONTAINER(notebook), gnc_prefs_find_page,
			  &location);
    label = gtk_label_new(add_in->tabname);
    gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
    gtk_notebook_insert_page(notebook, existing_content, label, location.index);
    gtk_widget_show_all(existing_content);
    DEBUG("created new page %s at index %d", add_in->tabname, location.index);
  } else {
    existing_content = gtk_notebook_get_nth_page(notebook, location.index);
    DEBUG("found existing page %s at index %d", add_in->tabname, location.index);
  }

  /* Maybe add a spacer row */
  g_object_get(G_OBJECT(existing_content), "n-rows", &rows, NULL);
  if (rows > 0)
    rows++;

  /* Now copy all the entries in the table */
  copydata.table_from = GTK_TABLE(new_content);
  copydata.table_to = GTK_TABLE(existing_content);
  copydata.row_offset = rows;
  gtk_container_foreach(GTK_CONTAINER(new_content), gnc_prefs_copy_table_entry,
			&copydata);

  gtk_object_sink(GTK_OBJECT(new_content));
  LEAVE("added to page at index %d", location.index);
}


/*******************************/
/* Dynamically added Callbacks */
/*******************************/

static void
gnc_prefs_radio_button_user_cb (GtkRadioButton *button,
				gpointer user_data)
{
  gchar *key, *button_name;
  gboolean active;

  g_return_if_fail(GTK_IS_RADIO_BUTTON(button));
  active = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button));
  if (!active)
    return;

  /* Copy the widget name and split into gconf key and button value parts */
  key = g_strdup(gtk_widget_get_name(GTK_WIDGET(button)) + PREFIX_LEN);
  button_name = rindex(key, '/');
  *button_name++ = '\0';

  DEBUG("Radio button group %s now set to %s", key, button_name);
  gnc_gconf_set_string(key, NULL, button_name, NULL);
  g_free(key);
}


static void
gnc_prefs_radio_button_gconf_cb (GtkRadioButton *button)
{
  g_return_if_fail(GTK_IS_RADIO_BUTTON(button));
  ENTER("button %p", button);
  g_signal_handlers_block_by_func(G_OBJECT(button),
				 G_CALLBACK(gnc_prefs_radio_button_user_cb),
				 NULL);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), TRUE);
  g_signal_handlers_unblock_by_func(G_OBJECT(button),
			   G_CALLBACK(gnc_prefs_radio_button_user_cb), NULL);
  LEAVE(" ");
}


static void
gnc_prefs_connect_radio_button (GtkRadioButton *button)
{
  gchar *key, *button_name, *value;
  gboolean active;
  GSList *group;

  g_return_if_fail(GTK_IS_RADIO_BUTTON(button));

  /* Copy the widget name and split into gconf key and button name parts */
  key = g_strdup(gtk_widget_get_name(GTK_WIDGET(button)) + PREFIX_LEN);
  button_name = rindex(key, '/');
  *button_name++ = '\0';

  /* Get the current value. */
  value = gnc_gconf_get_string(key, NULL, NULL);
  if (value) {
    active = (strcmp(value, button_name) == 0);
  } else {
    /* Sigh. There's no gconf default for this key. Use the first
     * button in the dialog, which is the last button in the list. */
    group =  gtk_radio_button_get_group(button);
    active = (button != g_slist_nth_data(group, g_slist_length(group)));
  }
  DEBUG(" Radio set %s, button %s initially set to %d",
	key, button_name, active);

  /* Wire up the button */
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), active);
  g_signal_connect(G_OBJECT(button), "toggled",
		   G_CALLBACK(gnc_prefs_radio_button_user_cb), NULL);
  g_free(value);
  g_free(key);
}

/**********/

static void
gnc_prefs_check_button_user_cb (GtkCheckButton *button,
				gpointer user_data)
{
  const gchar *name;
  gboolean active;

  g_return_if_fail(GTK_IS_CHECK_BUTTON(button));
  name = gtk_widget_get_name(GTK_WIDGET(button)) + PREFIX_LEN;
  active = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button));
  DEBUG("Checkbox %s now %sactive", name, active ? "" : "in");
  gnc_gconf_set_bool(name, NULL, active, NULL);
}


static void
gnc_prefs_check_button_gconf_cb (GtkCheckButton *button,
				 gboolean active)
{
  g_return_if_fail(GTK_IS_CHECK_BUTTON(button));
  ENTER("button %p, active %d", button, active);
  g_signal_handlers_block_by_func(G_OBJECT(button),
				 G_CALLBACK(gnc_prefs_check_button_user_cb),
				 NULL);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), active);
  g_signal_handlers_unblock_by_func(G_OBJECT(button),
			   G_CALLBACK(gnc_prefs_check_button_user_cb), NULL);
  LEAVE(" ");
}


static void
gnc_prefs_connect_check_button (GtkCheckButton *button)
{
  const gchar *name;
  gboolean active;

  name = gtk_widget_get_name(GTK_WIDGET(button)) + PREFIX_LEN;
  active = gnc_gconf_get_bool(name, NULL, NULL);
  DEBUG(" Checkbox %s initially %sactive", name, active ? "" : "in");
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), active);
  g_signal_connect(G_OBJECT(button), "toggled",
		   G_CALLBACK(gnc_prefs_check_button_user_cb), NULL);
}

/**********/

static void
gnc_prefs_spin_button_user_cb (GtkSpinButton *spin,
			       gpointer user_data)
{
  const gchar *name;
  gdouble value;

  g_return_if_fail(GTK_IS_SPIN_BUTTON(spin));
  name = gtk_widget_get_name(GTK_WIDGET(spin)) + PREFIX_LEN;
  value = gtk_spin_button_get_value(spin);
  DEBUG(" Spin button %s has value %f", name, value);
  gnc_gconf_set_float(name, NULL, value, NULL);
}


static void
gnc_prefs_spin_button_gconf_cb (GtkSpinButton *spin,
				gdouble value)
{
  g_return_if_fail(GTK_IS_SPIN_BUTTON(spin));
  ENTER("button %p, value %f", spin, value);
  g_signal_handlers_block_by_func(G_OBJECT(spin),
				 G_CALLBACK(gnc_prefs_spin_button_user_cb),
				 NULL);
  gtk_spin_button_set_value(spin, value);
  g_signal_handlers_unblock_by_func(G_OBJECT(spin),
			   G_CALLBACK(gnc_prefs_spin_button_user_cb), NULL);
  LEAVE(" ");
}


static void
gnc_prefs_connect_spin_button (GtkSpinButton *spin)
{
  const gchar *name;
  gdouble value;

  g_return_if_fail(GTK_IS_SPIN_BUTTON(spin));
  name = gtk_widget_get_name(GTK_WIDGET(spin)) + PREFIX_LEN;
  value = gnc_gconf_get_float(name, NULL, NULL);
  gtk_spin_button_set_value(spin, value);
  DEBUG(" Spin button %s has initial value %f", name, value);
  g_signal_connect(G_OBJECT(spin), "value-changed",
		   G_CALLBACK(gnc_prefs_spin_button_user_cb), NULL);
}


/**********/

static void
gnc_prefs_combo_box_user_cb (GtkComboBox *box,
			     gpointer user_data)
{
  const gchar *name;
  gint active;

  g_return_if_fail(GTK_IS_COMBO_BOX(box));
  name = gtk_widget_get_name(GTK_WIDGET(box)) + PREFIX_LEN;
  active = gtk_combo_box_get_active(box);
  DEBUG("Combo box %s set to item %d", name, active);
  gnc_gconf_set_int(name, NULL, active, NULL);
}


static void
gnc_prefs_combo_box_gconf_cb (GtkComboBox *box,
			      gint value)
{
  g_return_if_fail(GTK_IS_COMBO_BOX(box));
  ENTER("box %p, value %d", box, value);
  g_signal_handlers_block_by_func(G_OBJECT(box),
				 G_CALLBACK(gnc_prefs_combo_box_user_cb),
				 NULL);
  gtk_combo_box_set_active(box, value);
  g_signal_handlers_unblock_by_func(G_OBJECT(box),
			   G_CALLBACK(gnc_prefs_combo_box_user_cb), NULL);
  LEAVE(" ");
}


static void
gnc_prefs_connect_combo_box (GtkComboBox *box)
{
  const gchar *name;
  gint active;

  g_return_if_fail(GTK_IS_COMBO_BOX(box));
  name = gtk_widget_get_name(GTK_WIDGET(box)) + PREFIX_LEN;
  active = gnc_gconf_get_int(name, NULL, NULL);
  gtk_combo_box_set_active(GTK_COMBO_BOX(box), active);
  DEBUG(" Combo box %s set to item %d", name, active);
  g_signal_connect(G_OBJECT(box), "changed",
		   G_CALLBACK(gnc_prefs_combo_box_user_cb), NULL);
}


/********************/
/*    Callbacks     */
/********************/

void
gnc_preferences_response_cb(GtkDialog *dialog, gint response, GtkDialog *unused)
{
  switch (response) {
   case GTK_RESPONSE_HELP:
     gnc_gnome_help(HF_CUSTOM, HL_GLOBPREFS);
    break;

   case GTK_RESPONSE_CLOSE:
     gnc_save_window_size(GCONF_SECTION, GTK_WINDOW(dialog));
     gnc_unregister_gui_component_by_data(DIALOG_PREFERENCES_CM_CLASS,
					  dialog);
     gnc_gconf_remove_notification(G_OBJECT(dialog), NULL);
     gtk_widget_destroy(GTK_WIDGET(dialog));
     break;

   default:
     break;
  }
}

/********************/
/*    Creation      */
/********************/

static GtkWidget *
gnc_preferences_dialog_create(void)
{
  GladeXML *xml;
  GtkWidget *dialog, *notebook, *widget;
  struct page_data page_data;
  GList *interesting, *runner;

  ENTER("");
  DEBUG("Opening preferences.glade:");
  xml = gnc_glade_xml_new("preferences.glade", "New Gnucash Preferences");
  dialog = glade_xml_get_widget(xml, "New Gnucash Preferences");
  g_object_set_data(G_OBJECT(dialog), "xml", xml);
  DEBUG("autoconnect");
  glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func,
				    dialog);
  DEBUG("done");

  notebook = glade_xml_get_widget(xml, "notebook1");
  interesting = glade_xml_get_widget_prefix(xml, "gconf");

  page_data.notebook = GTK_NOTEBOOK(notebook);
  page_data.widgets = interesting;
  g_slist_foreach(add_ins, gnc_preferences_build_page, &page_data);

  DEBUG("We have the following interesting widgets:");
  for (runner = page_data.widgets; runner; runner = g_list_next(runner)) {
    widget = GTK_WIDGET(runner->data);
    if (GTK_IS_RADIO_BUTTON(widget)) {
      DEBUG("  %s - radio button", gtk_widget_get_name(widget));
      gnc_prefs_connect_radio_button(GTK_RADIO_BUTTON(widget));
    } else if (GTK_IS_CHECK_BUTTON(widget)) {
      DEBUG("  %s - check button", gtk_widget_get_name(widget));
      gnc_prefs_connect_check_button(GTK_CHECK_BUTTON(widget));
    } else if (GTK_IS_SPIN_BUTTON(widget)) {
      DEBUG("  %s - spin button", gtk_widget_get_name(widget));
      gnc_prefs_connect_spin_button(GTK_SPIN_BUTTON(widget));
    } else if (GTK_IS_COMBO_BOX(widget)) {
      DEBUG("  %s - combo box", gtk_widget_get_name(widget));
      gnc_prefs_connect_combo_box(GTK_COMBO_BOX(widget));
    } else {
      DEBUG("  %s - unsupported %s", gtk_widget_get_name(widget),
	    G_OBJECT_TYPE_NAME(G_OBJECT(widget)));
    }
  }
  DEBUG("Done with interesting widgets.");

  LEAVE("dialog %p", dialog);
  return dialog;
}


static void
gnc_preferences_gconf_changed (GConfClient *client,
			       guint cnxn_id,
			       GConfEntry *entry,
			       gpointer dialog)
{
  GConfValue *value;
  const gchar *key, *string_value;
  gchar  **parts, *name, *group_name = NULL;
  GtkWidget *widget;
  GList *possibilities;
  GladeXML *xml;

  ENTER("key %s, value %p", entry->key, entry->value);
  key = gconf_entry_get_key(entry);
  value = gconf_entry_get_value(entry);
  if (!value) {
    /* Values can be unset */
    LEAVE("Unset valued for %s", key);
    return;
  }

  parts = g_strsplit(entry->key, "/", 4);
  name = g_strconcat("gconf/", parts[3], NULL);
  g_strfreev(parts);
  DEBUG("proposed widget name %s", name);

  widget = gnc_glade_lookup_widget(dialog, name);
  if ((widget == NULL) && (entry->value->type == GCONF_VALUE_STRING)) {
    string_value = gconf_value_get_string(entry->value);
    group_name = name;
    name = g_strjoin("/", group_name, string_value, NULL);
    DEBUG("proposed widget name %s", name);
    widget = gnc_glade_lookup_widget(dialog, name);
    if (widget == NULL) {
      /* Mutter, mutter. Someone must have typed a bad string into
       * gconf.  Force the value to a legal string.  Do this by
       * directly setting the first widget in the group. This will
       * ensure synchronization of Gnucash, Gconf, and the Prefs
       * Dialog. */
      DEBUG("bad value");
      xml = g_object_get_data(G_OBJECT(dialog), "xml");
      possibilities = glade_xml_get_widget_prefix(xml, group_name);
      if (possibilities) {
	DEBUG("forcing %s", gtk_widget_get_name(possibilities->data));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(possibilities->data), TRUE);
	g_list_free(possibilities);
      }
    }
    g_free(group_name);
  }
  if (widget != NULL) {
    if (GTK_IS_RADIO_BUTTON(widget)) {
      DEBUG("widget %p - radio button", widget);
      gnc_prefs_radio_button_gconf_cb(GTK_RADIO_BUTTON(widget));
    } else if (GTK_IS_CHECK_BUTTON(widget)) {
      DEBUG("widget %p - check button", widget);
      gnc_prefs_check_button_gconf_cb(GTK_CHECK_BUTTON(widget),
					    gconf_value_get_bool(entry->value));
    } else if (GTK_IS_SPIN_BUTTON(widget)) {
      DEBUG("widget %p - spin button", widget);
      gnc_prefs_spin_button_gconf_cb(GTK_SPIN_BUTTON(widget),
				     gconf_value_get_float(entry->value));
    } else if (GTK_IS_COMBO_BOX(widget)) {
      DEBUG("widget %p - combo_box", widget);
      gnc_prefs_combo_box_gconf_cb(GTK_COMBO_BOX(widget),
				   gconf_value_get_int(entry->value));
    } else {
      DEBUG("widget %p - unsupported %s", widget,
	    G_OBJECT_TYPE_NAME(G_OBJECT(widget)));
    }
  }

  g_free(name);
  LEAVE(" ");
}


static gboolean
show_handler (const char *class, gint component_id,
	      gpointer user_data, gpointer iter_data)
{
  GtkWidget *dialog;

  ENTER(" ");
  dialog = GTK_WIDGET(user_data);
  gtk_window_present(GTK_WINDOW(dialog));
  LEAVE(" ");
  return(TRUE);
}


static void
close_handler (gpointer user_data)
{
  GtkWidget *dialog;

  ENTER(" ");
  dialog = GTK_WIDGET(user_data);
  gnc_unregister_gui_component_by_data(DIALOG_PREFERENCES_CM_CLASS, dialog);
  gtk_widget_destroy(dialog);
  LEAVE(" ");
}


void
gnc_preferences_dialog (void)
{
  GtkWidget *dialog;

  ENTER("");
  if (gnc_forall_gui_components(DIALOG_PREFERENCES_CM_CLASS,
				show_handler, NULL)) {
    LEAVE("existing window");
    return;
  }

  dialog = gnc_preferences_dialog_create();

  gnc_restore_window_size(GCONF_SECTION, GTK_WINDOW(dialog));

  gnc_gconf_add_notification(G_OBJECT(dialog), NULL,
			     gnc_preferences_gconf_changed);
  gnc_register_gui_component(DIALOG_PREFERENCES_CM_CLASS,
			     NULL, close_handler, dialog);
  LEAVE(" ");
}
