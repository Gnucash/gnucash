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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/** @addtogroup Dialogs
    @{ */
/** @addtogroup PrefDialog Preferences Dialog
    @{ */
/** @file dialog-preferences.c
    @brief Dialog for handling user preferences.
    @author Copyright (c) 2005 David Hampton <hampton@employees.org>

    These functions are the external API available for the new user
    preference dialog.  Preferences are now stored in GConf.  This
    code ends up being nothing more than a pretty interface to set
    key/value pairs in that database.  Any module may add a page (or
    partial page) of preferences to the dialog.  These additions are
    done by providing the name of a glade file, and a widget in that
    file.  If a partial page is added, the widget name provided must
    be that of a GtkTable containing four columns. If a full page is
    added, the widget name provided to this code can be any kind of
    widget, but for consistence it should probably be the same.

    If a widget names is in the form gconf/xxx/yyy... and it is a type
    of widget this code knows how to handle, then the callback signals
    will be automatically wired up for the widget.  The only fields
    that is required to be set in the glade file is the widget name.
    This code currently knows about radio buttons, check buttons, spin
    boxes, combo boxes, gnucash currency select widgets, gnucash
    accounting period widgets, and a gnucash date edit widget.  (Combo
    boxes should not be used for less than six choices.  Use a radio
    button group instead.)

    The argument *is* a glade file, so if your code has special
    requirements (e.g. make one widget insensitive until another is
    selected) feel free to go ahead and add your own callbacks to the
    glade file.  This code will connect any callbacks that exist in
    the glade file.
*/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glade/glade.h>

#include "dialog-utils.h"
#include "gnc-currency-edit.h"
#include "gnc-date-edit.h"
#include "gnc-gconf-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-period-select.h"
#include "gnc-engine.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-component-manager.h"
#include "dialog-preferences.h"

#define GLADE_FILENAME			"preferences.glade"
#define DIALOG_PREFERENCES_CM_CLASS	"dialog-newpreferences"
#define GCONF_SECTION			"dialogs/preferences"
#define PREFIX_LEN			sizeof("gconf/") - 1
#define WIDGET_HASH			"widget_hash"
#define NOTEBOOK			"notebook"

/** The debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_PREFS;

void gnc_preferences_response_cb(GtkDialog *dialog, gint response, GtkDialog *unused);
void gnc_reset_warnings_select_all_cb (GtkButton *button, gpointer user_data);
void gnc_reset_warnings_unselect_all_cb (GtkButton *button, gpointer user_data);
void gnc_reset_warnings_response_cb (GtkDialog *dialog, gint arg1, gpointer user_data);


/** This data structure holds the information for a single addition to
 *  the preferences dialog. */
typedef struct addition_t {
  /** The relative name of the file where the glade data for this
   *  addition can be found. */
  gchar *filename;
  /** The name of the widget within the glade data file that should be
   *  added to the preferences dialog.  This should point to a
   *  GtkTable widget that has four columns. */
  gchar *widgetname;
  /** The name of the tab within the preferences dialog where these
   *  widgets should be placed. */
  gchar *tabname;
  /** TRUE if this addition represents a full page in the preferences
   *  dialog.  FALSE if this page may be combined with other pages. */
  gboolean full_page;
} addition;

/** A list of all additions that have been made to the preferences
 *  dialog.  The data fields for this list are ::addition data
 *  structures. */
GSList *add_ins = NULL;


/** This function is called whenever the account separator is changed
 *  in gconf.  It updates the label in the "Account" page of the
 *  preferences dialog.
 *
 *  @internal
 *
 *  @param unused A pointer to the changed gconf entry.
 *
 *  @param dialog A pointer to the preferences dialog.
 */
static void
gnc_account_separator_prefs_cb (GConfEntry *unused, GtkWidget *dialog)
{
  GtkWidget *label;
  gchar *sample;

  label = gnc_glade_lookup_widget(dialog, "sample_account");
  /* Translators: Both %s will be the account separator character; the
     resulting string is a demonstration how the account separator
     character will look like. You can replace these three account
     names with other account names that are more suitable for your
     language - just keep in mind to have exactly two %s in your
     translation. */
  sample = g_strdup_printf(_("Income%sSalary%sTaxable"),
			   gnc_get_account_separator_string(),
			   gnc_get_account_separator_string());
  DEBUG(" Label set to '%s'", sample);
  gtk_label_set_text(GTK_LABEL(label), sample);
  g_free(sample);
}


/** This function compares two add-ins to see if they specify the same
 *  tab name.
 *
 *  @internal
 *
 *  @param a A pointer to the first add-in.
 *
 *  @param b A pointer to the second add-in.
 *
 *  @return Zero if the tab name is the same in both add-ins. Non-zero otherwise.
 */
static gint
gnc_prefs_compare_addins (addition *a,
			  addition *b)
{
  return g_utf8_collate(a->tabname, b->tabname);
}


/** This is the common function that adds any set of preferences to
 *  the preferences dialog.  It allocates a data structure to remember
 *  the passed in data and queues it for later when the dialog is
 *  actually built.  This code does check to insure there aren't any
 *  conflicts, like multiple additions of the same tab name when the
 *  two pages being added aren't compatible.
 *
 *  @internal
 *
 *  @param filename The name of a glade file.
 *
 *  @param widgetname The name of the widget to extract from the glade file.
 *
 *  @param tabname The name this page of preferences should have in
 *  the dialog notebook.
 *
 *  @param full_page Is this a full page of preferences or a partial page.
 */
static void
gnc_preferences_add_page_internal (const gchar *filename,
				   const gchar *widgetname,
				   const gchar *tabname,
				   gboolean full_page)
{
  addition *add_in, *preexisting;
  gboolean error = FALSE;
  GSList *ptr;

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
    g_free(add_in);
    LEAVE("no memory");
    return;
  }

  ptr = g_slist_find_custom(add_ins, add_in, (GCompareFunc)gnc_prefs_compare_addins);
  if (ptr) {
    /* problem? */
    preexisting = ptr->data;

    if (preexisting->full_page) {
      g_warning("New tab %s(%s/%s/%s) conflicts with existing tab %s(%s/%s/full)",
		add_in->tabname, add_in->filename, add_in->widgetname,
		add_in->full_page ? "full" : "partial",
		preexisting->tabname, preexisting->filename, preexisting->widgetname);
      error = TRUE;
    } else if (add_in->full_page) {
      g_warning("New tab %s(%s/%s/%s) conflicts with existing tab %s(%s/%s/partial)",
		add_in->tabname, add_in->filename, add_in->widgetname,
		add_in->full_page ? "full" : "partial",
		preexisting->tabname, preexisting->filename, preexisting->widgetname);
      error = TRUE;
    }
  }

  if (error) {
    g_free(add_in->filename);
    g_free(add_in->widgetname);
    g_free(add_in->tabname);
    g_free(add_in);
    LEAVE("err");
    return;
  } else {
    add_ins = g_slist_append(add_ins, add_in);
  }
  LEAVE("");
}


/*  This function adds a full page of preferences to the preferences
 *  dialog.  When the dialog is created, the specified widget will be
 *  pulled from the specified glade file and added to the preferences
 *  dialog with the specified tab name.  The tab name may not be
 *  duplicated.  For example, the Business code might have a full page
 *  of its own preferences. */
void
gnc_preferences_add_page (const gchar *filename,
			  const gchar *widgetname,
			  const gchar *tabname)
{
  gnc_preferences_add_page_internal(filename, widgetname, tabname, TRUE);
}


/*  This function adds a partial page of preferences to the
 *  preferences dialog.  When the dialog is created, the specified
 *  widget will be pulled from the specified glade file and added to
 *  the preferences dialog with the specified tab name.  The tab name
 *  may be duplicated.  For example, the HBCI preferences may share a
 *  "Data Import" page with QIF and other methods. */
void
gnc_preferences_add_to_page (const gchar *filename,
			     const gchar *widgetname,
			     const gchar *tabname)
{
  gnc_preferences_add_page_internal(filename, widgetname, tabname, FALSE);
}

/****************************************/

/** This function builds a hash table of "interesting" widgets,
 *  i.e. widgets whose name starts with "gconf/".  This table is
 *  needed to perform name->widget lookups in the gconf callback
 *  functions.  Normally glade could be used for this function, but
 *  since the widgets come from multiple glade files that won;t work
 *  in this dialog.
 *
 *  @internal
 *
 *  @param xml A pointer to glade xml file currently being added to
 *  the dialog.
 *
 *  @param dialog A pointer to the dialog.  The hash table is stored
 *  as a pointer off the dialog so that it can be found in the
 *  callback from gconf. */
static void
gnc_prefs_build_widget_table (GladeXML *xml,
			      GtkWidget *dialog)
{
  GHashTable *table;
  GList *interesting, *runner;
  const gchar *name;
  GtkWidget *widget;

  table = g_object_get_data(G_OBJECT(dialog), WIDGET_HASH);
  interesting = glade_xml_get_widget_prefix(xml, "gconf");
  for (runner = interesting; runner; runner = g_list_next(runner)) {
    widget = runner->data;
    name = gtk_widget_get_name(widget);
    g_hash_table_insert(table, (gchar *)name, widget);
  }
  g_list_free(interesting);
}

/** This data structure is used while building the preferences dialog
 *  to copy a table from a glade file to the dialog under
 *  construction.  It maintains state information between invocations
 *  of the function gnc_prefs_move_table_entry which is called via a
 *  foreach loop over each item in the table. */
struct copy_data {
  /** The table being copied from. */
  GtkTable *table_from;
  /** The table being copied to. */
  GtkTable *table_to;
  /** The number of lines offset from the old table to the new
   *  table. */
  gint row_offset;
};


static GtkWidget *
gnc_prefs_find_page (GtkNotebook *notebook, const gchar *name)
{
  int n_pages, i;
  GtkWidget *child;
  const gchar *child_name;

  g_return_val_if_fail (GTK_IS_NOTEBOOK (notebook), NULL);
  g_return_val_if_fail (name, NULL);

  ENTER("");

  n_pages = gtk_notebook_get_n_pages (notebook);

  for (i=0; i<n_pages; i++) {
    child = gtk_notebook_get_nth_page (notebook, i);
    g_return_val_if_fail (child, NULL);

    child_name = gtk_notebook_get_tab_label_text (notebook, child);
    g_return_val_if_fail (child_name, NULL);

    if (g_utf8_collate (name, child_name) == 0) {
      LEAVE("found at index: %d", i);
      return child;
    }
  }

  LEAVE("not found");
  return NULL;
}


/** This function moves a GtkWidget from one GtkTable to another,
 *  preserving its attachment data, etc.  It is called when adding one
 *  partial preference page to another.
 *
 *  @internal
 *
 *  @param widget A pointer to the widget to move.
 *
 *  @param data A pointer to a data structure passed in by the caller.
 *  This data structure contains pointers to the old and new tables,
 *  plus the row offset into the new table.
 */
static void
gnc_prefs_move_table_entry (GtkWidget *child,
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


/** At dialog creation time, this function will be called once per
 *  adds-in.  It performs the work of adding the page into the main
 *  dialog.  It handles both the case of a full page being added to
 *  the dialog, and a partial page being added.
 *
 *  @internal
 *
 *  @param data A pointer to an addition data structure.
 *
 *  @param user_data A pointer to the dialog.
 */
static void
gnc_preferences_build_page (gpointer data,
			    gpointer user_data)
{
  GladeXML *xml;
  GtkWidget *dialog, *existing_content, *new_content, *label;
  GtkNotebook *notebook;
  addition *add_in;
  struct copy_data copydata;
  gint rows, cols;

  ENTER("add_in %p, dialog %p", data, user_data);
  add_in = (addition *)data;
  dialog = user_data;

  DEBUG("Opening %s to get %s:", add_in->filename, add_in->widgetname);
  xml = gnc_glade_xml_new(add_in->filename, add_in->widgetname);
  new_content = glade_xml_get_widget(xml, add_in->widgetname);
  DEBUG("done");

  /* Add to the list of interesting widgets */
  gnc_prefs_build_widget_table(xml, dialog);

  /* Clean up the xml data structure when the dialog is destroyed */
  g_object_set_data_full(G_OBJECT(dialog), add_in->filename,
			 xml, g_object_unref);

  /* Connect the signals in this glade file. The dialog is passed in
   * so the the callback can find "interesting" widgets from other
   * glade files if necessary (via the WIDGET_HASH hash
   * table). Widgets from the same glade file can be found with the
   * usual gnc_glade_lookup_widget() function. */
  glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func,
				    dialog);

  /* Prepare for recursion */
  notebook = g_object_get_data(G_OBJECT(dialog), NOTEBOOK);

  if (add_in->full_page) {
    label = gtk_label_new(add_in->tabname);
    gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
    gtk_notebook_append_page(notebook, new_content, label);
    LEAVE("appended page");
    return;
  }

  /* Copied tables must match the size of the main table */
  if (!GTK_IS_TABLE(new_content)) {
    g_critical("The object name %s in file %s is not a GtkTable.  It cannot "
	       "be added to the preferences dialog.",
	       add_in->widgetname, add_in->filename);
    LEAVE("");
    return;
  }
  g_object_get(G_OBJECT(new_content), "n-columns", &cols, NULL);
  if (cols != 4) {
    g_critical("The table %s in file %s does not have four columns.  It cannot "
	       "be added to the preferences dialog.",
	       add_in->widgetname, add_in->filename);
    LEAVE("");
    return;
  }

  /* Does the page exist or must we create it */
  existing_content = gnc_prefs_find_page(notebook, add_in->tabname);

  if (!existing_content) {
    /* No existing content with this name.  Create a blank page */
    rows = 0;
    existing_content = gtk_table_new(0, 4, FALSE);
    gtk_container_set_border_width(GTK_CONTAINER(existing_content), 6);
    label = gtk_label_new(add_in->tabname);
    gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
    gtk_notebook_append_page(notebook, existing_content, label);
    gtk_widget_show_all(existing_content);
    DEBUG("created new page %s, appended it", add_in->tabname);
  } else {
    g_object_get(G_OBJECT(existing_content), "n-rows", &rows, NULL);
    DEBUG("found existing page %s", add_in->tabname);
  }

  /* Maybe add a spacer row */
  DEBUG("rows is %d", rows);
  if (rows > 0) {
    label = gtk_label_new("");
    gtk_widget_show(label);
    gtk_table_attach(GTK_TABLE(existing_content), label, 0, 1, rows, rows+1,
		     GTK_FILL, GTK_FILL, 0, 0);
    rows++;
  }

  /* Now copy all the entries in the table */
  copydata.table_from = GTK_TABLE(new_content);
  copydata.table_to = GTK_TABLE(existing_content);
  copydata.row_offset = rows;
  gtk_container_foreach(GTK_CONTAINER(new_content), gnc_prefs_move_table_entry,
			&copydata);

  g_object_ref_sink(new_content);
  LEAVE("added content to page");
}

static gint
tab_cmp (GtkWidget *page_a, GtkWidget *page_b, GtkNotebook *notebook)
{
  return g_utf8_collate (gtk_notebook_get_tab_label_text (notebook, page_a),
			 gtk_notebook_get_tab_label_text (notebook, page_b));
}

static void
gnc_prefs_sort_pages (GtkNotebook *notebook)
{
  gint n_pages, i;
  GList *tabs=NULL, *iter=NULL;

  g_return_if_fail (GTK_IS_NOTEBOOK (notebook));

  /* gather tabs */
  n_pages = gtk_notebook_get_n_pages (notebook);
  for (i=n_pages-1; i>=0; i--)
    tabs = g_list_prepend (tabs, gtk_notebook_get_nth_page (notebook, i));

  /* sort in local copy */
  tabs = g_list_sort_with_data (tabs, (GCompareDataFunc) tab_cmp, notebook);

  /* reorder tabs */
  for (i=0, iter=tabs; iter; i++, iter=iter->next)
    gtk_notebook_reorder_child (notebook, GTK_WIDGET (iter->data), i);

  g_list_free (tabs);
}


/*******************************/
/* Dynamically added Callbacks */
/*******************************/


/** The user changed a GtkFontButton.  Update gconf.  Font selection
 *  choices are stored as a string.
 *
 *  @internal
 *
 *  @param gde A pointer to the GtkFontButton that was changed.
 *
 *  @param user_data Unused.
 */
static void
gnc_prefs_font_button_user_cb (GtkFontButton *fb,
                               gpointer user_data)
{
  const gchar *key, *font;

  g_return_if_fail(GTK_IS_FONT_BUTTON(fb));
  key = gtk_widget_get_name(GTK_WIDGET(fb)) + PREFIX_LEN;
  font = gtk_font_button_get_font_name(fb);

  DEBUG("font_button %s set", key);
  gnc_gconf_set_string(key, NULL, font, NULL);
}


/** A GtkFontButton choice was updated in gconf.  Update the user
 *  visible dialog.
 *
 *  @internal
 *
 *  @param gde A pointer to the GtkFontButton that changed.
 *
 *  @param value The new value of the GtkFontButton.
 */
static void
gnc_prefs_font_button_gconf_cb (GtkFontButton *fb,
                                GConfEntry *entry)
{
  const gchar *font;

  g_return_if_fail(GTK_IS_FONT_BUTTON(fb));
  ENTER("fb %p, entry %p", fb, entry);

  font = gconf_value_get_string(entry->value);

  g_signal_handlers_block_by_func(G_OBJECT(fb),
				  G_CALLBACK(gnc_prefs_font_button_user_cb),
				  NULL);
  gtk_font_button_set_font_name(fb, font);
  g_signal_handlers_unblock_by_func(G_OBJECT(fb),
				    G_CALLBACK(gnc_prefs_font_button_user_cb), NULL);
  LEAVE(" ");
}


/** Connect a GtkFontButton widget to the user callback function.  Set
 *  the font from its value in gconf.
 *
 *  @internal
 *
 *  @param gde A pointer to the date_edit that should be connected.
 */
static void
gnc_prefs_connect_font_button (GtkFontButton *fb)
{
  const gchar *name;
  gchar *font;

  g_return_if_fail(GTK_IS_FONT_BUTTON(fb));

  /* Lookup font name based upon gconf setting */
  name = gtk_widget_get_name(GTK_WIDGET(fb)) + PREFIX_LEN;
  font = gnc_gconf_get_string(name, NULL, NULL);

  gtk_font_button_set_font_name(fb, font);
  DEBUG(" font_button %s set", name);
  g_free(font);

  g_signal_connect(G_OBJECT(fb), "font_set",
		   G_CALLBACK(gnc_prefs_font_button_user_cb), NULL);

  gtk_widget_show_all(GTK_WIDGET(fb));
}


/**********/


/** The user clicked on a radio button.  Update gconf.  Radio button
 *  group choices are stored as a string.  The last component of the
 *  widget name is the string that will be stored.  I.E. The widget name
 *  must be in this form "gconf/<some-key-name>/value".
 *
 *  @internal
 *
 *  @param button A pointer to the radio button that was clicked.
 *
 *  @param user_data Unused.
 */
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
  button_name = strrchr(key, '/');
  *button_name++ = '\0';

  DEBUG("Radio button group %s now set to %s", key, button_name);
  gnc_gconf_set_string(key, NULL, button_name, NULL);
  g_free(key);
}


/** A radio button group choice was updated in gconf.  Update the user
 *  visible dialog.
 *
 *  @internal
 *
 *  @param button A pointer to the radio button that should be shown
 *  as selected.
 */
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


/** Connect a radio button widget to the user callback function.  Set
 *  the starting state of the radio button group from its value in
 *  gconf.
 *
 *  @internal
 *
 *  @param button A pointer to the radio button that should be
 *  connected.
 */
static void
gnc_prefs_connect_radio_button (GtkRadioButton *button)
{
  gchar *key, *button_name, *value;
  gboolean active;
  GSList *group;

  g_return_if_fail(GTK_IS_RADIO_BUTTON(button));

  /* Copy the widget name and split into gconf key and button name parts */
  key = g_strdup(gtk_widget_get_name(GTK_WIDGET(button)) + PREFIX_LEN);
  button_name = strrchr(key, '/');
  *button_name++ = '\0';

  /* Get the current value. */
  value = gnc_gconf_get_string(key, NULL, NULL);
  if (value) {
    active = (g_utf8_collate(value, button_name) == 0);
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


/** The user clicked on a check button.  Update gconf.  Check button
 *  choices are stored as a boolean
 *
 *  @internal
 *
 *  @param button A pointer to the check button that was clicked.
 *
 *  @param user_data Unused.
 */
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


/** A check button choice was updated in gconf.  Update the user
 *  visible dialog.
 *
 *  @internal
 *
 *  @param button A pointer to the check button that changed.
 *
 *  @param active The new state of the check button.
 */
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


/** Connect a check button widget to the user callback function.  Set
 *  the starting state of the button from its value in gconf.
 *
 *  @internal
 *
 *  @param button A pointer to the check button that should be
 *  connected.
 */
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


/** The user updated a spin button.  Update gconf.  Spin button
 *  choices are stored as a float.
 *
 *  @internal
 *
 *  @param button A pointer to the spin button that was clicked.
 *
 *  @param user_data Unused.
 */
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


/** A spin button choice was updated in gconf.  Update the user
 *  visible dialog.
 *
 *  @internal
 *
 *  @param button A pointer to the spin button that changed.
 *
 *  @param value The new value of the spin button.
 */
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


/** Connect a spin button widget to the user callback function.  Set
 *  the starting state of the button from its value in gconf.
 *
 *  @internal
 *
 *  @param button A pointer to the spin button that should be
 *  connected.
 */
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


/** The user changed a combo box.  Update gconf.  Combo box
 *  choices are stored as an int.
 *
 *  @internal
 *
 *  @param box A pointer to the combo box that was changed.
 *
 *  @param user_data Unused.
 */
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


/** A combo box choice was updated in gconf.  Update the user
 *  visible dialog.
 *
 *  @internal
 *
 *  @param box A pointer to the combo box that changed.
 *
 *  @param value The new value of the combo box.
 */
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


/** Connect a combo box widget to the user callback function.  Set
 *  the starting state of the box from its value in gconf.
 *
 *  @internal
 *
 *  @param box A pointer to the combo box that should be connected.
 */
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


/**********/


/** The user changed a currency_edit.  Update gconf.  Currency_edit
 *  choices are stored as an int.
 *
 *  @internal
 *
 *  @param gce A pointer to the currency_edit that was changed.
 *
 *  @param user_data Unused.
 */
static void
gnc_prefs_currency_edit_user_cb (GNCCurrencyEdit *gce,
				 gpointer user_data)
{
  const gchar *name, *mnemonic;
  gnc_commodity *currency;

  g_return_if_fail(GNC_IS_CURRENCY_EDIT(gce));
  name = gtk_widget_get_name(GTK_WIDGET(gce)) + PREFIX_LEN;
  currency = gnc_currency_edit_get_currency(gce);
  mnemonic = gnc_commodity_get_mnemonic(currency);

  DEBUG("currency_edit %s set to %s", name, mnemonic);
  gnc_gconf_set_string(name, NULL, mnemonic, NULL);
}


/** A currency_edit choice was updated in gconf.  Update the user
 *  visible dialog.
 *
 *  @internal
 *
 *  @param gce A pointer to the currency_edit that changed.
 *
 *  @param value The new value of the currency_edit.
 */
static void
gnc_prefs_currency_edit_gconf_cb (GNCCurrencyEdit *gce,
				  GConfEntry *entry)
{
  const gchar *mnemonic;
  gnc_commodity *currency;

  g_return_if_fail(GNC_IS_CURRENCY_EDIT(gce));
  ENTER("gce %p, entry %p", gce, entry);

  mnemonic = gconf_value_get_string(entry->value);
  DEBUG("gce %p, mnemonic %s", gce, mnemonic);
  currency = gnc_commodity_table_lookup(gnc_get_current_commodities(),
					GNC_COMMODITY_NS_CURRENCY, mnemonic);

  /* If there isn't any such commodity, get the default */
  if (!currency) {
    currency = gnc_locale_default_currency();
    DEBUG("gce %p, default currency mnemonic %s",
	  gce, gnc_commodity_get_mnemonic(currency));
  }

  g_signal_handlers_block_by_func(G_OBJECT(gce),
				 G_CALLBACK(gnc_prefs_currency_edit_user_cb),
				 NULL);
  gnc_currency_edit_set_currency(GNC_CURRENCY_EDIT(gce), currency);
  g_signal_handlers_unblock_by_func(G_OBJECT(gce),
			   G_CALLBACK(gnc_prefs_currency_edit_user_cb), NULL);
  LEAVE(" ");
}


/** Connect a currency_edit widget to the user callback function.  Set
 *  the starting state of the gce from its value in gconf.
 *
 *  @internal
 *
 *  @param gce A pointer to the currency_edit that should be connected.
 */
static void
gnc_prefs_connect_currency_edit (GNCCurrencyEdit *gce)
{
  gnc_commodity *currency;
  const gchar *name;
  gchar *mnemonic;

  g_return_if_fail(GNC_IS_CURRENCY_EDIT(gce));

  /* Lookup commodity based upon gconf setting */
  name = gtk_widget_get_name(GTK_WIDGET(gce)) + PREFIX_LEN;
  mnemonic = gnc_gconf_get_string(name, NULL, NULL);
  currency = gnc_commodity_table_lookup(gnc_get_current_commodities(),
					GNC_COMMODITY_NS_CURRENCY, mnemonic);
  if (mnemonic)
    g_free(mnemonic);

  /* If there isn't any such commodity, get the default */
  if (!currency)
    currency = gnc_locale_default_currency();

  gnc_currency_edit_set_currency(GNC_CURRENCY_EDIT(gce), currency);
  DEBUG(" currency_edit %s set to %s", name,
	gnc_commodity_get_mnemonic(currency));

  g_signal_connect(G_OBJECT(gce), "changed",
		   G_CALLBACK(gnc_prefs_currency_edit_user_cb), NULL);

  gtk_widget_show_all(GTK_WIDGET(gce));
}


/**********/


/** The user changed a gtk entry.  Update gconf.
 *
 *  @internal
 *
 *  @param entry A pointer to the entry that was changed.
 *
 *  @param user_data Unused.
 */
static void
gnc_prefs_entry_user_cb (GtkEntry *entry,
			 gpointer user_data)
{
  const gchar *name, *text;

  g_return_if_fail(GTK_IS_ENTRY(entry));
  name = gtk_widget_get_name(GTK_WIDGET(entry)) + PREFIX_LEN;
  text = gtk_entry_get_text(entry);
  DEBUG("Entry %s set to '%s'", name, text);
  gnc_gconf_set_string(name, NULL, text, NULL);
}


/** A gtk entry was updated in gconf.  Update the user visible dialog.
 *
 *  @internal
 *
 *  @param entry A pointer to the gtk entry that changed.
 *
 *  @param value The new value of the combo box.
 */
static void
gnc_prefs_entry_gconf_cb (GtkEntry *entry,
			  const gchar *value)
{
  g_return_if_fail(GTK_IS_ENTRY(entry));
  ENTER("entry %p, value '%s'", entry, value);
  g_signal_handlers_block_by_func(G_OBJECT(entry),
				 G_CALLBACK(gnc_prefs_entry_user_cb),
				 NULL);
  gtk_entry_set_text(entry, value);
  g_signal_handlers_unblock_by_func(G_OBJECT(entry),
			   G_CALLBACK(gnc_prefs_entry_user_cb), NULL);
  LEAVE(" ");
}


/** Connect a entry widget to the user callback function.  Set the
 *  starting state of the entry from its value in gconf.
 *
 *  @internal
 *
 *  @param entry A pointer to the entry that should be connected.
 */
static void
gnc_prefs_connect_entry (GtkEntry *entry)
{
  const gchar *name;
  gchar *text;

  g_return_if_fail(GTK_IS_ENTRY(entry));
  name = gtk_widget_get_name(GTK_WIDGET(entry)) + PREFIX_LEN;
  text = gnc_gconf_get_string(name, NULL, NULL);
  gtk_entry_set_text(GTK_ENTRY(entry), text ? text : "");
  DEBUG(" Entry %s set to '%s'", name?name:"(null)", text?text:"(null)");
  g_free(text);
  g_signal_connect(G_OBJECT(entry), "changed",
		   G_CALLBACK(gnc_prefs_entry_user_cb), NULL);
}


/**********/


/** The user changed a GncPeriodSelect widget.  Update gconf.
 *  GncPeriodSelect choices are stored as an int.
 *
 *  @internal
 *
 *  @param period A pointer to the GncPeriodSelect that was changed.
 *
 *  @param user_data Unused.
 */
static void
gnc_prefs_period_select_user_cb (GncPeriodSelect *period,
				 gpointer user_data)
{
  const gchar *name;
  gint active;

  g_return_if_fail(GNC_IS_PERIOD_SELECT(period));
  name = gtk_widget_get_name(GTK_WIDGET(period)) + PREFIX_LEN;
  active = gnc_period_select_get_active(period);
  DEBUG("Period select %s set to item %d", name, active);
  gnc_gconf_set_int(name, NULL, active, NULL);
}


/** A GncPeriodSelect choice was updated in gconf.  Update the user
 *  visible dialog.
 *
 *  @internal
 *
 *  @param period A pointer to the GncPeriodSelect that needs updating.
 *
 *  @param value The new value of the GncPeriodSelect.
 */
static void
gnc_prefs_period_select_gconf_cb (GncPeriodSelect *period,
				  gint value)
{
  g_return_if_fail(GNC_IS_PERIOD_SELECT(period));
  ENTER("period %p, value %d", period, value);
  g_signal_handlers_block_by_func(G_OBJECT(period),
				 G_CALLBACK(gnc_prefs_period_select_user_cb),
				 NULL);
  gnc_period_select_set_active(period, value);
  g_signal_handlers_unblock_by_func(G_OBJECT(period),
			   G_CALLBACK(gnc_prefs_period_select_user_cb), NULL);
  LEAVE(" ");
}


/** Connect a GncPeriodSelect widget to the user callback function.  Set
 *  the starting state of the period from its value in gconf.
 *
 *  @internal
 *
 *  @param period A pointer to the GncPeriodSelect that should be connected.
 */
static void
gnc_prefs_connect_period_select (GncPeriodSelect *period)
{
  const gchar *name;
  gint active;
  QofBook *book;
  KvpFrame *book_frame;
  gint64 month, day;
  GDate fy_end;

  g_return_if_fail(GNC_IS_PERIOD_SELECT(period));
  book = gnc_get_current_book();
  book_frame = qof_book_get_slots(book);
  month = kvp_frame_get_gint64(book_frame, "/book/fyear_end/month");
  day = kvp_frame_get_gint64(book_frame, "/book/fyear_end/day");
  if (g_date_valid_dmy(day, month, 2005 /* not leap year */)) {
    g_date_clear(&fy_end, 1);
    g_date_set_dmy(&fy_end, day, month, G_DATE_BAD_YEAR);
    gnc_period_select_set_fy_end(period, &fy_end);
  }

  name = gtk_widget_get_name(GTK_WIDGET(period)) + PREFIX_LEN;
  active = gnc_gconf_get_int(name, NULL, NULL);
  gnc_period_select_set_active(period, active);
  DEBUG(" Period select %s set to item %d", name, active);
  g_signal_connect(G_OBJECT(period), "changed",
		   G_CALLBACK(gnc_prefs_period_select_user_cb), NULL);
}


/**********/


/** The user changed a date_edit.  Update gconf.  Date_edit
 *  choices are stored as an int.
 *
 *  @internal
 *
 *  @param gde A pointer to the date_edit that was changed.
 *
 *  @param user_data Unused.
 */
static void
gnc_prefs_date_edit_user_cb (GNCDateEdit *gde,
			     gpointer user_data)
{
  const gchar *name;
  time_t time;

  g_return_if_fail(GNC_IS_DATE_EDIT(gde));
  name = gtk_widget_get_name(GTK_WIDGET(gde)) + PREFIX_LEN;
  time = gnc_date_edit_get_date(gde);

  DEBUG("date_edit %s set", name);
  gnc_gconf_set_int(name, NULL, time, NULL);
}


/** A date_edit choice was updated in gconf.  Update the user
 *  visible dialog.
 *
 *  @internal
 *
 *  @param gde A pointer to the date_edit that changed.
 *
 *  @param value The new value of the date_edit.
 */
static void
gnc_prefs_date_edit_gconf_cb (GNCDateEdit *gde,
			      GConfEntry *entry)
{
  time_t time;

  g_return_if_fail(GNC_IS_DATE_EDIT(gde));
  ENTER("gde %p, entry %p", gde, entry);

  time = gconf_value_get_int(entry->value);

  g_signal_handlers_block_by_func(G_OBJECT(gde),
				  G_CALLBACK(gnc_prefs_date_edit_user_cb),
				  NULL);
  gnc_date_edit_set_time(GNC_DATE_EDIT(gde), time);
  g_signal_handlers_unblock_by_func(G_OBJECT(gde),
				    G_CALLBACK(gnc_prefs_date_edit_user_cb), NULL);
  LEAVE(" ");
}


/** Connect a date_edit widget to the user callback function.  Set
 *  the starting state of the gde from its value in gconf.
 *
 *  @internal
 *
 *  @param gde A pointer to the date_edit that should be connected.
 */
static void
gnc_prefs_connect_date_edit (GNCDateEdit *gde)
{
  const gchar *name;
  time_t time;

  g_return_if_fail(GNC_IS_DATE_EDIT(gde));

  /* Lookup commodity based upon gconf setting */
  name = gtk_widget_get_name(GTK_WIDGET(gde)) + PREFIX_LEN;
  time = gnc_gconf_get_int(name, NULL, NULL);

  gnc_date_edit_set_time(GNC_DATE_EDIT(gde), time);
  DEBUG(" date_edit %s set", name);

  g_signal_connect(G_OBJECT(gde), "date_changed",
		   G_CALLBACK(gnc_prefs_date_edit_user_cb), NULL);

  gtk_widget_show_all(GTK_WIDGET(gde));
}


/********************/
/*    Callbacks     */
/********************/

/** Handle a user click on one of the buttons at the bottom of the
 *  preference dialog.  Also handles delete_window events, which have
 *  conveniently converted to a response by GtkDialog.
 *
 *  @internal
 *
 *  @param dialog A pointer to the preferences dialog.
 *
 *  @param response Indicates which button was pressed by the user.
 *  The only expected values are HELP, CLOSE, and DELETE_EVENT.
 *
 *  @param unused
 */
void
gnc_preferences_response_cb(GtkDialog *dialog, gint response, GtkDialog *unused)
{
  switch (response) {
   case GTK_RESPONSE_HELP:
     gnc_gnome_help(HF_HELP, HL_GLOBPREFS);
     break;

   default:
     gnc_save_window_size(GCONF_SECTION, GTK_WINDOW(dialog));
     gnc_unregister_gui_component_by_data(DIALOG_PREFERENCES_CM_CLASS,
					  dialog);
     gnc_gconf_general_remove_cb(
       KEY_ACCOUNT_SEPARATOR,
       (GncGconfGeneralCb)gnc_account_separator_prefs_cb,
       dialog);
     gnc_gconf_remove_notification(G_OBJECT(dialog), NULL,
				   DIALOG_PREFERENCES_CM_CLASS);
     gtk_widget_destroy(GTK_WIDGET(dialog));
     break;
  }
}

/********************/
/*    Creation      */
/********************/


/** Connect one dialog widget to the appropriate callback function for
 *  its type.
 *
 *  @internal
 *
 *  @param name The name of the widget.
 *
 *  @param widget A pointer to the widget.
 *
 *  @param dialog A pointer to the dialog.
 */
static void
gnc_prefs_connect_one (const gchar *name,
		       GtkWidget *widget,
		       gpointer user_data)
{
  /* These tests must be ordered from more specific widget to less
   * specific widget.  Test custom widgets first */
  if (GNC_IS_CURRENCY_EDIT(widget)) { /* must be tested before combo_box */
    DEBUG("  %s - currency_edit", name);
    gnc_prefs_connect_currency_edit(GNC_CURRENCY_EDIT(widget));
  } else if (GNC_IS_PERIOD_SELECT(widget)) {
    DEBUG("  %s - period_Select", name);
    gnc_prefs_connect_period_select(GNC_PERIOD_SELECT(widget));
  } else if (GNC_IS_DATE_EDIT(widget)) {
    DEBUG("  %s - date_edit", name);
    gnc_prefs_connect_date_edit(GNC_DATE_EDIT(widget));
  } else if (GTK_IS_FONT_BUTTON(widget)) {
    DEBUG("  %s - entry", name);
    gnc_prefs_connect_font_button(GTK_FONT_BUTTON(widget));
  } else if (GTK_IS_RADIO_BUTTON(widget)) {
    DEBUG("  %s - radio button", name);
    gnc_prefs_connect_radio_button(GTK_RADIO_BUTTON(widget));
  } else if (GTK_IS_CHECK_BUTTON(widget)) {
    DEBUG("  %s - check button", name);
    gnc_prefs_connect_check_button(GTK_CHECK_BUTTON(widget));
  } else if (GTK_IS_SPIN_BUTTON(widget)) {
    DEBUG("  %s - spin button", name);
    gnc_prefs_connect_spin_button(GTK_SPIN_BUTTON(widget));
  } else if (GTK_IS_COMBO_BOX(widget)) {
    DEBUG("  %s - combo box", name);
    gnc_prefs_connect_combo_box(GTK_COMBO_BOX(widget));
  } else if (GTK_IS_ENTRY(widget)) {
    DEBUG("  %s - entry", name);
    gnc_prefs_connect_entry(GTK_ENTRY(widget));
  } else {
    DEBUG("  %s - unsupported %s", name,
	  G_OBJECT_TYPE_NAME(G_OBJECT(widget)));
  }
}


/** Create the preferences dialog.  This function first reads the
 *  preferences.glade file to get obtain the dialog and set of common
 *  preferences.  It then runs the list of add-ins, calling a helper
 *  function to add each full/partial page to this dialog, Finally it
 *  runs the list of "interesting: widgets that it has built,
 *  connecting this widgets up to callback functions.
 *
 *  @internal
 *
 *  @return A pointer to the newly created dialog.
 */
static GtkWidget *
gnc_preferences_dialog_create(void)
{
  GladeXML *xml;
  GtkWidget *dialog, *notebook, *label;
  GHashTable *table;
  GDate* gdate;
  gchar buf[128];
  gnc_commodity *locale_currency;
  const gchar *currency_name;

  ENTER("");
  DEBUG("Opening preferences.glade:");
  xml = gnc_glade_xml_new(GLADE_FILENAME, "GnuCash Preferences");
  dialog = glade_xml_get_widget(xml, "GnuCash Preferences");
  DEBUG("autoconnect");
  glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func,
				    dialog);

  /* Clean up the xml data structure when the dialog is destroyed */
  g_object_set_data_full(G_OBJECT(dialog), GLADE_FILENAME,
			 xml, g_object_unref);
  DEBUG("done");

  notebook = glade_xml_get_widget(xml, "notebook1");
  table = g_hash_table_new(g_str_hash, g_str_equal);
  g_object_set_data(G_OBJECT(dialog), NOTEBOOK, notebook);
  g_object_set_data_full(G_OBJECT(dialog), WIDGET_HASH,
			 table, (GDestroyNotify)g_hash_table_destroy);

  /* Add to the list of interesting widgets */
  gnc_prefs_build_widget_table(xml, dialog);

  g_slist_foreach(add_ins, gnc_preferences_build_page, dialog);

  /* Sort tabs alphabetically */
  gnc_prefs_sort_pages(GTK_NOTEBOOK(notebook));
  gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), 0);

  DEBUG("We have the following interesting widgets:");
  g_hash_table_foreach(table, (GHFunc)gnc_prefs_connect_one, dialog);
  DEBUG("Done with interesting widgets.");

  /* Other stuff */
  gdate = g_date_new_dmy(31, G_DATE_JULY, 2005);
  g_date_strftime(buf, sizeof(buf), "%x", gdate);
  label = glade_xml_get_widget(xml, "locale_date_sample");
  gtk_label_set_text(GTK_LABEL(label), buf);
  g_date_free(gdate);

  locale_currency = gnc_locale_default_currency ();
  currency_name = gnc_commodity_get_printname(locale_currency);
  label = glade_xml_get_widget(xml, "locale_currency");
  gtk_label_set_label(GTK_LABEL(label), currency_name);
  label = glade_xml_get_widget(xml, "locale_currency2");
  gtk_label_set_label(GTK_LABEL(label), currency_name);

  gnc_account_separator_prefs_cb(NULL, dialog);

  LEAVE("dialog %p", dialog);
  return dialog;
}


/*************************************/
/*    GConf common callback code     */
/*************************************/


/** Find a partial match from gconf key to widget name.  This function
 *  is needed if the user manually updates the value of a radio button
 *  setting (a string) and types in an illegal value.  This function
 *  is called on all the "interesting" widgets in the dialog until
 *  there is a match.  This matched widget represents a legal value
 *  for the radio button.  The calling function can then force this
 *  radio button to be "on", thus insuring that Gnucash always has a
 *  legal value for the radio group.
 *
 *  @internal
 *
 *  @param key The name of a widget.
 *
 *  @param widget A pointer to the widget.
 *
 *  @param user_data The name of the gconf key that was changed.
 *
 *  @return Zero if the gconf key is a subset of this button's
 *  name. Non-zero otherwise.
 */
static gboolean
gnc_prefs_nearest_match (gpointer key,
			 gpointer value,
			 gpointer user_data)
{
  const gchar *widget_name = key;
  const gchar *gconf_name = user_data;

  return (strncmp(widget_name, gconf_name, strlen(gconf_name)) == 0);
}


/** Create the preferences dialog.  This function first reads the
 *  preferences.glade file to get obtain the dialog and set of common
 *  preferences.  It then runs the list of add-ins, calling a helper
 *  function to add each full/partial page to this dialog, Finally it
 *  runs the list of "interesting: widgets that it has built,
 *  connecting this widgets up to callback functions.
 *
 *  @internal
 *
 *  @return A pointer to the newly created dialog.
 */
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
  GHashTable *table;

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

  /* Can't just do a glade lookup here because not all of the widgets
   * came from the same xml file. That's why the extra hash table. */
  table = g_object_get_data(G_OBJECT(dialog), WIDGET_HASH);
  widget = g_hash_table_lookup(table, name);
  if ((widget == NULL) && (entry->value->type == GCONF_VALUE_STRING)) {
    string_value = gconf_value_get_string(entry->value);
    group_name = name;
    name = g_strjoin("/", group_name, string_value, NULL);
    DEBUG("proposed widget name %s", name);
    widget = g_hash_table_lookup(table, name);
    if (widget == NULL) {
      /* Mutter, mutter. Someone must have typed a bad string into
       * gconf.  Force the value to a legal string.  Do this by
       * directly setting the first widget in the group. This will
       * ensure synchronization of Gnucash, Gconf, and the Prefs
       * Dialog. */
      DEBUG("bad value");
      widget = g_hash_table_find(table, gnc_prefs_nearest_match, group_name);
      if (widget) {
           DEBUG("forcing %s", gtk_widget_get_name(widget));
           gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), TRUE);
      }
      g_free(group_name);
      g_free(name);
      LEAVE("no exact match");
      return;
    }
    g_free(group_name);
  }
  if (widget != NULL) {
    /* These tests must be ordered from more specific widget to less
     * specific widget.  Test custom widgets first */
    if (GNC_IS_CURRENCY_EDIT(widget)) { /* must come before combo box */
      DEBUG("widget %p - currency_edit", widget);
      gnc_prefs_currency_edit_gconf_cb(GNC_CURRENCY_EDIT(widget), entry);
    } else if (GNC_IS_PERIOD_SELECT(widget)) {
      DEBUG("widget %p - period_select", widget);
      gnc_prefs_period_select_gconf_cb(GNC_PERIOD_SELECT(widget),
				      gconf_value_get_int(entry->value));
    } else if (GNC_IS_DATE_EDIT(widget)) {
      DEBUG("widget %p - date_edit", widget);
      gnc_prefs_date_edit_gconf_cb(GNC_DATE_EDIT(widget), entry);
    } else if (GTK_IS_FONT_BUTTON(widget)) {
      DEBUG("widget %p - font button", widget);
      gnc_prefs_font_button_gconf_cb(GTK_FONT_BUTTON(widget), entry);
    } else if (GTK_IS_RADIO_BUTTON(widget)) {
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
    } else if (GTK_IS_ENTRY(widget)) {
      DEBUG("widget %p - entry", widget);
      gnc_prefs_entry_gconf_cb(GTK_ENTRY(widget),
			       gconf_value_get_string(entry->value));
    } else {
      DEBUG("widget %p - unsupported %s", widget,
	    G_OBJECT_TYPE_NAME(G_OBJECT(widget)));
    }
  }

  g_free(name);
  LEAVE(" ");
}


/** Raise the preferences dialog to the top of the window stack.  This
 *  function is called if the user attempts to create a second
 *  preferences dialog.
 *
 *  @internal
 *
 *  @param class Unused.
 *
 *  @param component_id Unused.
 *
 *  @param user_data A pointer to the preferences dialog.
 *
 *  @param iter_data Unused.
 */
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


/** Close the preferences dialog.
 *
 *  @internal
 *
 *  @param user_data A pointer to the preferences dialog.
 */
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


/*  This function creates the preferences dialog and presents it to
 *  the user.  The preferences dialog is a singleton, so if a
 *  preferences dialog already exists it will be raised to the top of
 *  the window stack instead of creating a new dialog. */
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
  gtk_widget_show(dialog);

  gnc_gconf_add_notification(G_OBJECT(dialog), NULL,
			     gnc_preferences_gconf_changed,
			     DIALOG_PREFERENCES_CM_CLASS);
  gnc_gconf_general_register_cb(KEY_ACCOUNT_SEPARATOR,
				(GncGconfGeneralCb)gnc_account_separator_prefs_cb,
				dialog);
  gnc_register_gui_component(DIALOG_PREFERENCES_CM_CLASS,
			     NULL, close_handler, dialog);

  LEAVE(" ");
}

/** @} */
/** @} */
