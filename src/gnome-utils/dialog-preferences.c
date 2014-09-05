/********************************************************************\
 * dialog-preferences.c -- preferences dialog                       *
 *                                                                  *
 * Copyright (C) 2005 David Hampton                                 *
 * Copyright (C) 2011 Robert Fewell                                 *
 * Copyright (C) 2013 Geert Janssens                                *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

/** @addtogroup Dialogs
    @{ */
/** @addtogroup PrefDialog Preferences Dialog
    @{ */
/** @file dialog-preferences.c
    @brief Dialog for handling user preferences.
    @author Copyright (c) 2005 David Hampton <hampton@employees.org>

    These functions are the external API available for the user
    preference dialog. This dialog allows a user to modify
    several user preferences in the gnucash preferences database.
    Any module may add a page (or partial page) of preferences
    to the dialog.  These additions are done by providing
    the name of a glade file and the content to load from that
    file along with a widget in that file.  If a partial
    page is added, the widget name provided must be that of
    a GtkTable containing four columns. If a full page is added,
    the widget name provided to this code can be any kind of
    widget, but for consistency it should probably be the same.

    If a widget name is in the form pref/aaa.bbb/ccc... and it is a type
    of widget this code knows how to handle, then the widget is bound
    to the preference named ccc in group aaa.bbb. This means that if
    the widget's value changes, the preference is automatically updated.
    The same goes the other way around. This code currently knows about
    font buttons, radio buttons, check buttons, spin boxes, combo boxes,
    gnucash currency select widgets, gnucash accounting period widgets,
    and a gnucash date edit widget. (Combo boxes should not be used for
    less than five choices. Use a radio button group instead.)

    The argument *is* a glade file, so if your code has special
    requirements (e.g. make one widget insensitive until another is
    selected) feel free to go ahead and add your own callbacks to the
    glade file.  This code will connect any callbacks that exist in
    the glade file.
*/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-currency-edit.h"
#include "gnc-date-edit.h"
#include "gnc-gobject-utils.h"
#include "gnc-period-select.h"
#include "gnc-engine.h"
#include "Account.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-component-manager.h"
#include "dialog-preferences.h"

#define DIALOG_PREFERENCES_CM_CLASS "dialog-newpreferences"
#define GNC_PREFS_GROUP             "dialogs.preferences"
#define PREF_PREFIX_LEN              sizeof("pref/") - 1
#define PREFS_WIDGET_HASH           "prefs_widget_hash"
#define NOTEBOOK                    "notebook"

/** The debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_PREFS;

void gnc_preferences_response_cb(GtkDialog *dialog, gint response, GtkDialog *unused);
void gnc_account_separator_pref_changed_cb (GtkEntry *entry, GtkWidget *dialog);
gboolean gnc_account_separator_validate_cb (GtkEntry *entry, GdkEvent *event, GtkWidget *dialog);

/** This data structure holds the information for a single addition to
 *  the preferences dialog. */
typedef struct addition_t
{
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

static gchar *gnc_account_separator_is_valid (const gchar *separator,
                                              gchar **normalized_separator)
{
    QofBook *book = gnc_get_current_book();
    GList *conflict_accts = NULL;
    gchar *message = NULL;

    *normalized_separator = gnc_normalize_account_separator (separator);
    conflict_accts = gnc_account_list_name_violations (book, *normalized_separator);
    if (conflict_accts)
        message = gnc_account_name_violations_errmsg (*normalized_separator,
                                                      conflict_accts);

    g_list_free (conflict_accts);

    return message;
}

/** This function is called whenever the account separator is changed
 *  in the preferences dialog.  It updates the example label in the
 *  "Account" page of the preferences dialog.
 *
 *  @internal
 *
 *  @param entry The text entry field for the account separator
 *
 *  @param dialog A pointer to the preferences dialog.
 */
void
gnc_account_separator_pref_changed_cb (GtkEntry *entry, GtkWidget *dialog)
{
    GtkWidget *label, *image;
    gchar *sample;
    gchar *separator;

    gchar *conflict_msg = gnc_account_separator_is_valid (gtk_entry_get_text (entry), &separator);

    label = g_object_get_data(G_OBJECT(dialog), "sample_account");
    DEBUG("Sample Account pointer is %p", label );
    /* Translators: Both %s will be the account separator character; the
       resulting string is a demonstration how the account separator
       character will look like. You can replace these three account
       names with other account names that are more suitable for your
       language - just keep in mind to have exactly two %s in your
       translation. */
    sample = g_strdup_printf(_("Income%sSalary%sTaxable"),
                             separator, separator);
    PINFO(" Label set to '%s'", sample);
    gtk_label_set_text(GTK_LABEL(label), sample);
    g_free(sample);

    /* Check if the new separator clashes with existing account names */
    image = g_object_get_data(G_OBJECT(dialog), "separator_error");
    DEBUG("Separator Error Image pointer is %p", image );

    if (conflict_msg)
    {
        gtk_widget_set_tooltip_text(GTK_WIDGET(image), conflict_msg);
        gtk_widget_show (GTK_WIDGET(image));
        g_free ( conflict_msg );
    }
    else
        gtk_widget_hide (GTK_WIDGET(image));

    g_free (separator);
}


gboolean
gnc_account_separator_validate_cb (GtkEntry *entry, GdkEvent *event, GtkWidget *dialog)
{
    gchar *separator;
    gchar *conflict_msg = gnc_account_separator_is_valid (gtk_entry_get_text (entry), &separator);

    /* Check if the new separator clashes with existing account names */

    if (conflict_msg)
    {
        gnc_warning_dialog(dialog, "%s", conflict_msg);
        g_free ( conflict_msg );
    }
    g_free (separator);
    return FALSE;
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
 *  actually built.  This code does check to ensure there aren't any
 *  conflicts, like multiple additions of the same tab name when the
 *  two pages being added aren't compatible.
 *
 *  @internal
 *
 *  @param filename The name of a glade file.
 *
 *  @param widgetname A string of content to load, the last one being
 *         the name of the widget to add to the preferences dialog.
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
    if (add_in == NULL)
    {
        g_critical("Unable to allocate memory.\n");
        LEAVE("no memory");
        return;
    }

    add_in->filename   = g_strdup(filename);
    add_in->widgetname = g_strdup(widgetname);
    add_in->tabname    = g_strdup(tabname);
    add_in->full_page  = full_page;
    if (!add_in->filename || !add_in->widgetname || !add_in->tabname)
    {
        g_critical("Unable to allocate memory.\n");
        g_free(add_in->filename);
        g_free(add_in->widgetname);
        g_free(add_in->tabname);
        g_free(add_in);
        LEAVE("no memory");
        return;
    }

    ptr = g_slist_find_custom(add_ins, add_in, (GCompareFunc)gnc_prefs_compare_addins);
    if (ptr)
    {
        /* problem? */
        preexisting = ptr->data;

        if (preexisting->full_page)
        {
            g_warning("New tab %s(%s/%s/%s) conflicts with existing tab %s(%s/%s/full)",
                      add_in->tabname, add_in->filename, add_in->widgetname,
                      add_in->full_page ? "full" : "partial",
                      preexisting->tabname, preexisting->filename, preexisting->widgetname);
            error = TRUE;
        }
        else if (add_in->full_page)
        {
            g_warning("New tab %s(%s/%s/%s) conflicts with existing tab %s(%s/%s/partial)",
                      add_in->tabname, add_in->filename, add_in->widgetname,
                      add_in->full_page ? "full" : "partial",
                      preexisting->tabname, preexisting->filename, preexisting->widgetname);
            error = TRUE;
        }
    }

    if (error)
    {
        g_free(add_in->filename);
        g_free(add_in->widgetname);
        g_free(add_in->tabname);
        g_free(add_in);
        LEAVE("err");
        return;
    }
    else
    {
        add_ins = g_slist_append(add_ins, add_in);
    }
    LEAVE("");
}


/*  This function adds a full page of preferences to the preferences
 *  dialog.  When the dialog is created, the specified content will be
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
 *  content will be pulled from the glade file and added to the
 *  preferences dialog with the specified tab name.  The tab name
 *  may be duplicated.  For example, the HBCI preferences may share a
 *  "Data Import" page with QIF and other methods. */
void
gnc_preferences_add_to_page (const gchar *filename,
                             const gchar *widgetname,
                             const gchar *tabname)
{
    gnc_preferences_add_page_internal(filename, widgetname, tabname, FALSE);
}


/*******************************************************************/

/** This function builds a hash table of "interesting" widgets,
 *  i.e. widgets whose name starts with "pref/".  This table is
 *  needed to perform name->widget lookups when binding the widgets
 *  to their matching preferences.
 *
 *  @internal
 *
 *  @param builder A pointer to builder glade file currently being
 *  added to the dialog.
 *
 *  @param dialog A pointer to the dialog. The hash table is stored
 *  as a pointer off the dialog so that it can be found in the binding
 *  code. */
static void
gnc_prefs_build_widget_table (GtkBuilder *builder,
                              GtkWidget *dialog)
{
    GHashTable *prefs_table;
    GSList *interesting, *runner;
    const gchar *name;
    const gchar *wname;
    GtkWidget *widget;

    prefs_table = g_object_get_data(G_OBJECT(dialog), PREFS_WIDGET_HASH);

    interesting = gtk_builder_get_objects(builder);

    for (runner = interesting; runner; runner = g_slist_next(runner))
    {
        widget = runner->data;
        if (GTK_IS_WIDGET(widget))
        {
            wname = gtk_widget_get_name(widget);
            name = gtk_buildable_get_name(GTK_BUILDABLE(widget));
            DEBUG("Widget type is %s and buildable get name is %s", wname, name);
            if (g_str_has_prefix (name, "pref"))
                g_hash_table_insert(prefs_table, (gchar *)name, widget);
        }
    }
    g_slist_free(interesting);

}


/** This data structure is used while building the preferences dialog
 *  to copy a table from a glade file to the dialog under construction.
 *  It maintains state information between invocations of the function
 *  gnc_prefs_move_table_entry which is called via a foreach loop over
 *  each item in the table. */
struct copy_data
{
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

    for (i = 0; i < n_pages; i++)
    {
        child = gtk_notebook_get_nth_page (notebook, i);
        g_return_val_if_fail (child, NULL);

        child_name = gtk_notebook_get_tab_label_text (notebook, child);
        g_return_val_if_fail (child_name, NULL);

        if (g_utf8_collate (name, child_name) == 0)
        {
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

    g_object_ref(child);
    gtk_container_remove(GTK_CONTAINER(copydata->table_from), child);
    gtk_table_attach(copydata->table_to, child, left, right,
                     top + copydata->row_offset, bottom + copydata->row_offset,
                     x_opts, y_opts, x_pad, y_pad);
    g_object_unref(child);
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
    GtkBuilder *builder;
    GtkWidget *dialog, *existing_content, *new_content, *label;
    GtkNotebook *notebook;
    addition *add_in;
    struct copy_data copydata;
    gint rows, cols;
    gchar **widgetname;
    gint i;

    ENTER("add_in %p, dialog %p", data, user_data);
    add_in = (addition *)data;
    dialog = user_data;

    DEBUG("Opening %s to get %s", add_in->filename, add_in->widgetname);
    builder = gtk_builder_new();

    /* Adjustments etc... must come before dialog information */
    widgetname = g_strsplit(add_in->widgetname, ",", -1);

    for (i = 0; widgetname[i]; i++)
    {
        DEBUG("Opening %s to get content %s", add_in->filename, widgetname[i]);
        gnc_builder_add_from_file (builder, add_in->filename, widgetname[i]);
    }

    DEBUG("Widget Content is %s", widgetname[i - 1]);
    new_content = GTK_WIDGET(gtk_builder_get_object (builder, widgetname[i - 1]));

    g_strfreev(widgetname);
    DEBUG("done");

    /* Add to the list of interesting widgets */
    gnc_prefs_build_widget_table(builder, dialog);

    /* Connect the signals in this glade file. The dialog is passed in
     * so the the callback can find "interesting" widgets from other
     * glade files if necessary (via the GPREFS_WIDGET_HASH hash table). */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, dialog);

    /* Prepare for recursion */
    notebook = g_object_get_data(G_OBJECT(dialog), NOTEBOOK);

    if (add_in->full_page)
    {
        label = gtk_label_new(add_in->tabname);
        gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
        gtk_notebook_append_page(notebook, new_content, label);
        g_object_unref(G_OBJECT(builder));
        LEAVE("appended page");
        return;
    }

    /* Copied tables must match the size of the main table */
    if (!GTK_IS_TABLE(new_content))
    {
        g_critical("The object name %s in file %s is not a GtkTable.  It cannot "
                   "be added to the preferences dialog.",
                   add_in->widgetname, add_in->filename);
        g_object_unref(G_OBJECT(builder));
        LEAVE("");
        return;
    }
    g_object_get(G_OBJECT(new_content), "n-columns", &cols, NULL);
    if (cols != 4)
    {
        g_critical("The table %s in file %s does not have four columns.  It cannot "
                   "be added to the preferences dialog.",
                   add_in->widgetname, add_in->filename);
        g_object_unref(G_OBJECT(builder));
        LEAVE("");
        return;
    }

    /* Does the page exist or must we create it */
    existing_content = gnc_prefs_find_page(notebook, add_in->tabname);

    if (!existing_content)
    {
        /* No existing content with this name.  Create a blank page */
        rows = 0;
        existing_content = gtk_table_new(0, 4, FALSE);
        gtk_container_set_border_width(GTK_CONTAINER(existing_content), 6);
        label = gtk_label_new(add_in->tabname);
        gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
        gtk_notebook_append_page(notebook, existing_content, label);
        gtk_widget_show_all(existing_content);
        DEBUG("created new page %s, appended it", add_in->tabname);
    }
    else
    {
        g_object_get(G_OBJECT(existing_content), "n-rows", &rows, NULL);
        DEBUG("found existing page %s", add_in->tabname);
    }

    /* Maybe add a spacer row */
    DEBUG("rows is %d", rows);
    if (rows > 0)
    {
        label = gtk_label_new("");
        gtk_widget_show(label);
        gtk_table_attach(GTK_TABLE(existing_content), label, 0, 1, rows, rows + 1,
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
    g_object_unref(G_OBJECT(builder));

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
    GList *tabs = NULL, *iter = NULL;

    g_return_if_fail (GTK_IS_NOTEBOOK (notebook));

    /* gather tabs */
    n_pages = gtk_notebook_get_n_pages (notebook);
    for (i = n_pages - 1; i >= 0; i--)
        tabs = g_list_prepend (tabs, gtk_notebook_get_nth_page (notebook, i));

    /* sort in local copy */
    tabs = g_list_sort_with_data (tabs, (GCompareDataFunc) tab_cmp, notebook);

    /* reorder tabs */
    for (i = 0, iter = tabs; iter; i++, iter = iter->next)
        gtk_notebook_reorder_child (notebook, GTK_WIDGET (iter->data), i);

    g_list_free (tabs);
}


/*******************************/
/* Dynamically added Callbacks */
/*******************************/

static void
gnc_prefs_split_widget_name (const gchar *name, gchar **group, gchar **pref)
{
    const gchar *group_with_pref = name + PREF_PREFIX_LEN;
    gchar **splits = g_strsplit (group_with_pref, "/", 0);

    *group = g_strdup (splits[0]);
    *pref = g_strdup (splits[1]);
    g_strfreev (splits);
}

/****************************************************************************/

/** Connect a GtkFontButton widget to its stored value in the preferences database.
 *
 *  @internal
 *
 *  @param fb A pointer to the font button that should be connected.
 */
static void
gnc_prefs_connect_font_button (GtkFontButton *fb)
{
    gchar *group, *pref;
    gchar *font;

    g_return_if_fail(GTK_IS_FONT_BUTTON(fb));

    gnc_prefs_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(fb)), &group, &pref);
    gnc_prefs_bind (group, pref, G_OBJECT (fb), "font-name");

    g_free (group);
    g_free (pref);

    gtk_widget_show_all(GTK_WIDGET(fb));
}

/****************************************************************************/

/** Connect a GtkRadioButton widget to its stored value in the preferences database.
 *
 *  @internal
 *
 *  @param button A pointer to the radio button that should be
 *  connected.
 */
static void
gnc_prefs_connect_radio_button (GtkRadioButton *button)
{
    gchar *group, *pref;
    gboolean active;

    g_return_if_fail(GTK_IS_RADIO_BUTTON(button));

    gnc_prefs_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(button)), &group, &pref);

//    active = gnc_prefs_get_bool (group, pref);
//    DEBUG(" Checkbox %s/%s initially %sactive", group, pref, active ? "" : "in");
//    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), active);

    gnc_prefs_bind (group, pref, G_OBJECT (button), "active");

    g_free(group);
    g_free(pref);
}

/****************************************************************************/

/** Connect a GtkCheckButton widget to its stored value in the preferences database.
 *
 *  @internal
 *
 *  @param button A pointer to the check button that should be
 *  connected.
 */
static void
gnc_prefs_connect_check_button (GtkCheckButton *button)
{
    gchar *group, *pref;
    gboolean active;

    g_return_if_fail(GTK_IS_CHECK_BUTTON(button));

    gnc_prefs_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(button)), &group, &pref);

//    active = gnc_prefs_get_bool (group, pref);
//    DEBUG(" Checkbox %s/%s initially %sactive", group, pref, active ? "" : "in");
//    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), active);

    gnc_prefs_bind (group, pref, G_OBJECT (button), "active");

    g_free (group);
    g_free (pref);
}

/****************************************************************************/

/** Connect a GtkSpinButton widget to its stored value in the preferences database.
 *
 *  @internal
 *
 *  @param button A pointer to the spin button that should be
 *  connected.
 */
static void
gnc_prefs_connect_spin_button (GtkSpinButton *spin)
{
    gchar *group, *pref;
    gdouble value;

    g_return_if_fail(GTK_IS_SPIN_BUTTON(spin));

    gnc_prefs_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(spin)), &group, &pref);

//    value = gnc_prefs_get_float (group, pref);
//    gtk_spin_button_set_value(spin, value);
//    DEBUG(" Spin button %s/%s has initial value %f", group, pref, value);

    gnc_prefs_bind (group, pref, G_OBJECT (spin), "value");

    g_free (group);
    g_free (pref);
}

/****************************************************************************/

/** Connect a GtkComboBox widget to its stored value in the preferences database.
 *
 *  @internal
 *
 *  @param box A pointer to the combo box that should be connected.
 */
static void
gnc_prefs_connect_combo_box (GtkComboBox *box)
{
    gchar *group, *pref;
    gint active;

    g_return_if_fail(GTK_IS_COMBO_BOX(box));

    gnc_prefs_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(box)), &group, &pref);

//    active = gnc_prefs_get_int(group, pref);
//    gtk_combo_box_set_active(GTK_COMBO_BOX(box), active);
//    DEBUG(" Combo box %s/%s set to item %d", group, pref, active);

    gnc_prefs_bind (group, pref, G_OBJECT (box), "active");

    g_free (group);
    g_free (pref);
}

/****************************************************************************/

/** Connect a GncCurrencyEdit widget to its stored value in the preferences database.
 *
 *  @internal
 *
 *  @param gce A pointer to the currency_edit that should be connected.
 */
static void
gnc_prefs_connect_currency_edit (GNCCurrencyEdit *gce, const gchar *boxname )
{
    gnc_commodity *currency;
    gchar *group, *pref;
    gchar *mnemonic;

    g_return_if_fail(GNC_IS_CURRENCY_EDIT(gce));

    gnc_prefs_split_widget_name (boxname, &group, &pref);

    gnc_prefs_bind (group, pref, G_OBJECT (gce), "mnemonic");

    g_free (group);
    g_free (pref);

    gtk_widget_show_all(GTK_WIDGET(gce));
}

/****************************************************************************/

/** Connect a GtkEntry widget to its stored value in the preferences database.
 *
 *  @internal
 *
 *  @param entry A pointer to the entry that should be connected.
 */
static void
gnc_prefs_connect_entry (GtkEntry *entry)
{
    gchar *group, *pref;
    gchar *text;

    g_return_if_fail(GTK_IS_ENTRY(entry));

    gnc_prefs_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(entry)), &group, &pref);

//    text = gnc_prefs_get_string(group, pref);
//    gtk_entry_set_text(GTK_ENTRY(entry), text ? text : "");
//    DEBUG(" Entry %s/%s set to '%s'", group, pref, text ? text : "(null)");
//    g_free(text);

    gnc_prefs_bind (group, pref, G_OBJECT (entry), "text");

    g_free (group);
    g_free (pref);
}

/****************************************************************************/

/** Connect a GncPeriodSelect widget to its stored value in the preferences database.
 *
 *  @internal
 *
 *  @param period A pointer to the GncPeriodSelect that should be connected.
 */
static void
gnc_prefs_connect_period_select (GncPeriodSelect *period, const gchar *boxname )
{
    gchar *group, *pref;
    gchar *mnemonic;

    g_return_if_fail(GNC_IS_PERIOD_SELECT(period));

    gnc_prefs_split_widget_name (boxname, &group, &pref);

    gnc_prefs_bind (group, pref, G_OBJECT (period), "active");

    g_free (group);
    g_free (pref);
}

/****************************************************************************/

/** Connect a GncDateEdit widget to its stored value in the preferences database.
 *
 *  @internal
 *
 *  @param gde A pointer to the date_edit that should be connected.
 */
static void
gnc_prefs_connect_date_edit (GNCDateEdit *gde , const gchar *boxname )
{
    gchar *group, *pref;
    gchar *mnemonic;

    g_return_if_fail(GNC_IS_DATE_EDIT(gde));

    gnc_prefs_split_widget_name (boxname, &group, &pref);

    gnc_prefs_bind (group, pref, G_OBJECT (gde), "time");

    g_free (group);
    g_free (pref);
}


/****************************************************************************/

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
    switch (response)
    {
    case GTK_RESPONSE_HELP:
        gnc_gnome_help(HF_HELP, HL_GLOBPREFS);
        break;

    default:
        gnc_save_window_size(GNC_PREFS_GROUP, GTK_WINDOW(dialog));
        gnc_unregister_gui_component_by_data(DIALOG_PREFERENCES_CM_CLASS,
                                             dialog);
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
     * specific widget. */

    if (GTK_IS_FONT_BUTTON(widget))
    {
        DEBUG("  %s - entry", name);
        gnc_prefs_connect_font_button(GTK_FONT_BUTTON(widget));
    }
    else if (GTK_IS_RADIO_BUTTON(widget))
    {
        DEBUG("  %s - radio button", name);
        gnc_prefs_connect_radio_button(GTK_RADIO_BUTTON(widget));
    }
    else if (GTK_IS_CHECK_BUTTON(widget))
    {
        DEBUG("  %s - check button", name);
        gnc_prefs_connect_check_button(GTK_CHECK_BUTTON(widget));
    }
    else if (GTK_IS_SPIN_BUTTON(widget))
    {
        DEBUG("  %s - spin button", name);
        gnc_prefs_connect_spin_button(GTK_SPIN_BUTTON(widget));
    }
    else if (GTK_IS_COMBO_BOX(widget))
    {
        DEBUG("  %s - combo box", name);
        gnc_prefs_connect_combo_box(GTK_COMBO_BOX(widget));
    }
    else if (GTK_IS_ENTRY(widget))
    {
        DEBUG("  %s - entry", name);
        gnc_prefs_connect_entry(GTK_ENTRY(widget));
    }
    else if (GTK_IS_HBOX(widget))
    {
        /* Test custom widgets are all children of a hbox */
        GtkWidget *widget_child;
        GList* child = gtk_container_get_children(GTK_CONTAINER(widget));
        widget_child = child->data;
        g_list_free(child);
        DEBUG("  %s - hbox", name);
        DEBUG("Hbox widget type is %s and name is %s", gtk_widget_get_name(GTK_WIDGET(widget_child)), name);

        if (GNC_IS_CURRENCY_EDIT(widget_child))
        {
            DEBUG("  %s - currency_edit", name);
            gnc_prefs_connect_currency_edit(GNC_CURRENCY_EDIT(widget_child), name );
        }
        else if (GNC_IS_PERIOD_SELECT(widget_child))
        {
            DEBUG("  %s - period_Select", name);
            gnc_prefs_connect_period_select(GNC_PERIOD_SELECT(widget_child), name );
        }
        else if (GNC_IS_DATE_EDIT(widget_child))
        {
            DEBUG("  %s - date_edit", name);
            gnc_prefs_connect_date_edit(GNC_DATE_EDIT(widget_child), name );
        }
    }
    else
    {
        DEBUG("  %s - unsupported %s", name,
              G_OBJECT_TYPE_NAME(G_OBJECT(widget)));
    }
}


/** Create the preferences dialog.  This function first reads the
 *  dialog-preferences.glade file to obtain the content and then
 *  the dialog is created with a set of common preferences.  It then
 *  runs the list of add-ins, calling a helper function to add each full/partial
 *  page to this dialog, Finally it builds the "interesting widgets"
 *  table that is used for connecting the widgets up to callback functions.
 *
 *  @internal
 *
 *  @return A pointer to the newly created dialog.
 */
static GtkWidget *
gnc_preferences_dialog_create(void)
{
    GtkBuilder *builder;
    GtkWidget *dialog, *notebook, *label, *image;
    GtkWidget *box, *date, *period, *currency;
    GHashTable *prefs_table;
    GDate* gdate = NULL;
    gchar buf[128];
    GtkListStore *store;
    GtkTreePath *path;
    GtkTreeIter iter;
    gnc_commodity *locale_currency;
    const gchar *currency_name;
    QofBook *book;
    KvpFrame *book_frame;
    gint64 month, day;
    GDate fy_end;
    gboolean date_is_valid = FALSE;

    ENTER("");
    DEBUG("Opening dialog-preferences.glade:");
    builder = gtk_builder_new();

    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "auto_decimal_places_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "autosave_interval_minutes_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "save_on_close_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "date_backmonth_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "max_transactions_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "key_length_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "new_search_limit_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "retain_days_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "tab_width_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "date_formats");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "GnuCash Preferences");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "GnuCash Preferences"));

#ifndef REGISTER2_ENABLED
    /* Hide preferences that are related to register2 */
    box = GTK_WIDGET (gtk_builder_get_object (builder, "label14"));
    gtk_widget_hide (box);
    box = GTK_WIDGET (gtk_builder_get_object (builder, "pref/general.register/key-length"));
    gtk_widget_hide (box);
    box = GTK_WIDGET (gtk_builder_get_object (builder, "pref/general.register/show-extra-dates"));
    gtk_widget_hide (box);
    box = GTK_WIDGET (gtk_builder_get_object (builder, "pref/general.register/show-calendar-buttons"));
    gtk_widget_hide (box);
    box = GTK_WIDGET (gtk_builder_get_object (builder, "pref/general.register/selection-to-blank-on-expand"));
    gtk_widget_hide (box);
    box = GTK_WIDGET (gtk_builder_get_object (builder, "pref/general.register/show-extra-dates-on-selection"));
    gtk_widget_hide (box);
#endif

    label = GTK_WIDGET(gtk_builder_get_object (builder, "sample_account"));
    g_object_set_data(G_OBJECT(dialog), "sample_account", label);

    image = GTK_WIDGET(gtk_builder_get_object (builder, "separator_error"));
    g_object_set_data(G_OBJECT(dialog), "separator_error", image);

    DEBUG("autoconnect");
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, dialog);

    DEBUG("done");

    notebook = GTK_WIDGET(gtk_builder_get_object (builder, "notebook1"));
    prefs_table = g_hash_table_new(g_str_hash, g_str_equal);
    g_object_set_data(G_OBJECT(dialog), NOTEBOOK, notebook);
    g_object_set_data_full(G_OBJECT(dialog), PREFS_WIDGET_HASH,
                           prefs_table, (GDestroyNotify)g_hash_table_destroy);


    book = gnc_get_current_book();
    g_date_clear (&fy_end, 1);
    qof_instance_get (QOF_INSTANCE (book),
		      "fy-end", &fy_end,
		      NULL);
    box = GTK_WIDGET(gtk_builder_get_object (builder,
                     "pref/" GNC_PREFS_GROUP_ACCT_SUMMARY "/" GNC_PREF_START_PERIOD));
    period = gnc_period_select_new(TRUE);
    gtk_widget_show (period);
    gtk_box_pack_start (GTK_BOX (box), period, TRUE, TRUE, 0);
    if (date_is_valid)
        gnc_period_select_set_fy_end(GNC_PERIOD_SELECT (period), &fy_end);

    box = GTK_WIDGET(gtk_builder_get_object (builder,
                     "pref/" GNC_PREFS_GROUP_ACCT_SUMMARY "/" GNC_PREF_END_PERIOD));
    period = gnc_period_select_new(FALSE);
    gtk_widget_show (period);
    gtk_box_pack_start (GTK_BOX (box), period, TRUE, TRUE, 0);
    if (date_is_valid)
        gnc_period_select_set_fy_end(GNC_PERIOD_SELECT (period), &fy_end);

    box = GTK_WIDGET(gtk_builder_get_object (builder,
                     "pref/" GNC_PREFS_GROUP_ACCT_SUMMARY "/" GNC_PREF_START_DATE));
    date = gnc_date_edit_new(gnc_time (NULL), FALSE, FALSE);
    gtk_widget_show (date);
    gtk_box_pack_start (GTK_BOX (box), date, TRUE, TRUE, 0);

    box = GTK_WIDGET(gtk_builder_get_object (builder,
                     "pref/" GNC_PREFS_GROUP_ACCT_SUMMARY "/" GNC_PREF_END_DATE));
    date = gnc_date_edit_new(gnc_time (NULL), FALSE, FALSE);
    gtk_widget_show (date);
    gtk_box_pack_start (GTK_BOX (box), date, TRUE, TRUE, 0);

    box = GTK_WIDGET(gtk_builder_get_object (builder,
                     "pref/" GNC_PREFS_GROUP_GENERAL "/" GNC_PREF_CURRENCY_OTHER));
    currency = gnc_currency_edit_new();
    gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(currency), gnc_default_currency());
    gtk_widget_show (currency);
    gtk_box_pack_start(GTK_BOX (box), currency, TRUE, TRUE, 0);

    box = GTK_WIDGET(gtk_builder_get_object (builder,
                     "pref/" GNC_PREFS_GROUP_GENERAL_REPORT "/" GNC_PREF_CURRENCY_OTHER));
    currency = gnc_currency_edit_new();
    gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(currency), gnc_default_currency());
    gtk_widget_show (currency);
    gtk_box_pack_start(GTK_BOX (box), currency, TRUE, TRUE, 0);


    /* Add to the list of interesting widgets */
    gnc_prefs_build_widget_table(builder, dialog);

    g_slist_foreach(add_ins, gnc_preferences_build_page, dialog);

    /* Sort tabs alphabetically */
    gnc_prefs_sort_pages(GTK_NOTEBOOK(notebook));
    gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), 0);

    DEBUG("We have the following interesting widgets:");
    g_hash_table_foreach(prefs_table, (GHFunc)gnc_prefs_connect_one, dialog);
    DEBUG("Done with interesting widgets.");

    /* Other stuff */
    gdate = g_date_new_dmy(31, G_DATE_JULY, 2013);
    g_date_strftime(buf, sizeof(buf), "%x", gdate);
    store = GTK_LIST_STORE(gtk_builder_get_object (builder, "date_formats"));
    path = gtk_tree_path_new_from_indices (QOF_DATE_FORMAT_LOCALE, -1);
    if (gtk_tree_model_get_iter (GTK_TREE_MODEL (store), &iter, path))
            gtk_list_store_set (store, &iter, 1, buf, -1);
    g_date_free(gdate);

    locale_currency = gnc_locale_default_currency ();
    currency_name = gnc_commodity_get_printname(locale_currency);
    label = GTK_WIDGET(gtk_builder_get_object (builder, "locale_currency"));
    gtk_label_set_label(GTK_LABEL(label), currency_name);
    label = GTK_WIDGET(gtk_builder_get_object (builder, "locale_currency2"));
    gtk_label_set_label(GTK_LABEL(label), currency_name);

    g_object_unref(G_OBJECT(builder));

    LEAVE("dialog %p", dialog);
    return dialog;
}


/*************************************/
/*    Common callback code           */
/*************************************/



/** Raise the preferences dialog to the top of the window stack.  This
 *  function is called if the user attempts to create a second
 *  preferences dialog.
 *
 *  @internal
 *
 *  @param class_name Unused.
 *
 *  @param component_id Unused.
 *
 *  @param user_data A pointer to the preferences dialog.
 *
 *  @param iter_data Unused.
 */
static gboolean
show_handler (const char *class_name, gint component_id,
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
                                  show_handler, NULL))
    {
        LEAVE("existing window");
        return;
    }

    dialog = gnc_preferences_dialog_create();

    gnc_restore_window_size(GNC_PREFS_GROUP, GTK_WINDOW(dialog));
    gtk_widget_show(dialog);

    gnc_register_gui_component(DIALOG_PREFERENCES_CM_CLASS,
                               NULL, close_handler, dialog);

    LEAVE(" ");
}

/** @} */
/** @} */
