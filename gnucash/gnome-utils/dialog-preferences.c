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
    a GtkGrid containing four columns. If a full page is added,
    the widget name provided to this code can be any kind of
    widget, but for consistency it should probably be the same.

    If a widget name is in the form pref/aaa.bbb/ccc... and it is a type
    of widget this code knows how to handle, then the widget is bound
    to the preference named ccc in group aaa.bbb. This means that if
    the widget's value changes, the preference is automatically updated.
    The same goes the other way around. This code currently knows about
    font buttons, grouped and independent check buttons, spin boxes, drop-downs,
    gnucash currency select widgets, gnucash accounting period widgets,
    and a gnucash date edit widget. (Drop-downs should not be used for
    less than five choices. Use a grouped check button instead.)

    The argument *is* a glade file, so if your code has special
    requirements (e.g. make one widget insensitive until another is
    selected) feel free to go ahead and add your own callbacks to the
    glade file.  This code will connect any callbacks that exist in
    the glade file.
*/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-gtk-utils.h"
#include "gnc-currency-edit.h"
#include "gnc-date-edit.h"
#include "gnc-gobject-utils.h"
#include "gnc-period-select.h"
#include "gnc-engine.h"
#include "Account.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include <gnc-uri-utils.h>
#include <gnc-session.h>
#include "gnc-component-manager.h"
#include "dialog-preferences.h"
#include "dialog-doclink-utils.h"
#include "qofinstance.h"

#define DIALOG_PREFERENCES_CM_CLASS "dialog-newpreferences"
#define GNC_PREFS_GROUP             "dialogs.preferences"
#define PREF_PREFIX_LEN              sizeof("pref/") - 1
#define PREFS_WIDGET_HASH           "prefs_widget_hash"
#define NOTEBOOK                    "notebook"

/** The debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_PREFS;

void gnc_account_separator_pref_changed_cb (GtkEntry *entry, GtkWidget *dialog);
void gnc_save_on_close_expires_cb (GtkCheckButton *button, GtkWidget *dialog);
static void gnc_preferences_help_clicked_cb (GtkButton *button,
                                              gpointer user_data);
static void gnc_preferences_close_clicked_cb (GtkButton *button,
                                               gpointer user_data);
static gboolean gnc_preferences_close_request_cb (GtkWindow *window,
                                                   gpointer   user_data);

/** This data structure holds the information for a single addition to
 *  the preferences dialog. */
typedef struct addition_t
{
    /** The relative name of the file where the glade data for this
     *  addition can be found. */
    gchar *filename;
    /** The name of the widget within the glade data file that should be
     *  added to the preferences dialog.  This should point to a
     *  GtkGrid widget that has four columns. */
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
    QofBook *book;
    GList *conflict_accts = NULL;
    gchar *message = NULL;

    if (!gnc_current_session_exist())
        return NULL;

    book = gnc_get_current_book ();
    *normalized_separator = gnc_normalize_account_separator (separator);
    conflict_accts = gnc_account_list_name_violations (book, *normalized_separator);
    if (conflict_accts)
        message = gnc_account_name_violations_errmsg (*normalized_separator,
                                                      conflict_accts);

    g_list_free_full (conflict_accts, g_free);
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
    gchar *separator = NULL;

    gchar *conflict_msg = gnc_account_separator_is_valid (
        gtk_editable_get_text (GTK_EDITABLE (entry)), &separator);

    label = g_object_get_data (G_OBJECT(dialog), "sample_account");
    DEBUG("Sample Account pointer is %p", label);
    /* Translators: Both %s will be the account separator character; the
       resulting string is a demonstration how the account separator
       character will look like. You can replace these three account
       names with other account names that are more suitable for your
       language - just keep in mind to have exactly two %s in your
       translation. */
    sample = g_strdup_printf (_("Income%sSalary%sTaxable"),
                              separator, separator);
    PINFO(" Label set to '%s'", sample);
    gtk_label_set_text (GTK_LABEL(label), sample);
    g_free (sample);

    /* Check if the new separator clashes with existing account names */
    image = g_object_get_data (G_OBJECT(dialog), "separator_error");
    DEBUG("Separator Error Image pointer is %p", image);

    if (conflict_msg)
    {
        gtk_widget_set_tooltip_text (GTK_WIDGET(image), conflict_msg);
        gtk_widget_set_visible (GTK_WIDGET(image), TRUE);
        g_free (conflict_msg);
    }
    else
        gtk_widget_set_visible (GTK_WIDGET(image), FALSE);

    g_free (separator);
}


static void
gnc_preferences_select_account_page (GtkWindow *dialog);

typedef struct
{
    GWeakRef dialog;
} SeparatorValidationRequest;

static void
gnc_preferences_close (GtkWindow *dialog)
{
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(dialog));
    gnc_unregister_gui_component_by_data (DIALOG_PREFERENCES_CM_CLASS,
                                          dialog);
    gtk_window_destroy (GTK_WINDOW(dialog));
}

static void
separator_validation_request_free (SeparatorValidationRequest *request)
{
    g_weak_ref_clear (&request->dialog);
    g_free (request);
}

static void
separator_validation_response_cb (GObject *source, GAsyncResult *result,
                                  gpointer user_data)
{
    SeparatorValidationRequest *request = user_data;
    GError *error = NULL;
    gint response = gtk_alert_dialog_choose_finish (GTK_ALERT_DIALOG(source),
                                                    result, &error);
    GObject *object = g_weak_ref_get (&request->dialog);

    if (object)
    {
        GtkWindow *dialog = GTK_WINDOW (object);
        GtkWidget *entry = g_object_get_data (G_OBJECT(dialog),
                                              "account-separator");

        if (!error && response == 0)
        {
            const gchar *original = g_object_get_data (G_OBJECT(entry),
                                                       "original_text");

            if (original)
                gtk_editable_set_text (GTK_EDITABLE (entry), original);
            gnc_preferences_close (dialog);
        }
        else
            gnc_preferences_select_account_page (dialog);
        g_object_unref (object);
    }

    g_clear_error (&error);
    separator_validation_request_free (request);
}

/** Called when the preferences dialog is closed to check whether the account
 *  separator is valid. Conflicts are resolved asynchronously so that closing
 *  the preferences window never enters a nested main loop. */
static void
gnc_account_separator_validate_async (GtkWindow *dialog)
{
    GtkWidget *entry = g_object_get_data (G_OBJECT(dialog),
                                          "account-separator");
    gchar *separator = NULL;
    gchar *conflict_msg = gnc_account_separator_is_valid (
        gtk_editable_get_text (GTK_EDITABLE (entry)), &separator);

    if (conflict_msg)
    {
        const char *buttons[] = { _("Reset"), _("Close"), NULL };
        GtkAlertDialog *alert = gtk_alert_dialog_new ("%s", conflict_msg);
        SeparatorValidationRequest *request =
            g_new0 (SeparatorValidationRequest, 1);

        g_weak_ref_init (&request->dialog, G_OBJECT(dialog));
        gtk_alert_dialog_set_modal (alert, TRUE);
        gtk_alert_dialog_set_buttons (alert, buttons);
        gtk_alert_dialog_set_default_button (alert, 1);
        gtk_alert_dialog_set_cancel_button (alert, 1);
        gtk_alert_dialog_choose (alert, GTK_WINDOW(dialog), NULL,
                                 separator_validation_response_cb, request);
        g_object_unref (alert);
    }
    else
        gnc_preferences_close (dialog);

    g_free (conflict_msg);
    g_free (separator);
}
/** Used to select the 'Accounts' page when the user wants
 *  to return from the account separator validation dialog
 *  to the preference dialog.
 *
 *  @internal
 *
 *  @param user_data A pointer to the dialog.
 */
static void
gnc_preferences_select_account_page (GtkWindow *dialog)
{
    GtkWidget *notebook = g_object_get_data (G_OBJECT(dialog), NOTEBOOK);
    GtkWidget *acc_page = NULL;
    gint n_pages;

    if (!notebook)
        return;

    n_pages = gtk_notebook_get_n_pages (GTK_NOTEBOOK (notebook));
    for (gint page_num = 0; page_num < n_pages; page_num++)
    {
        GtkWidget *page = gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook), page_num);
        if (g_strcmp0 (gtk_widget_get_name (page), "accounts_page") == 0)
        {
            acc_page = page;
            break;
        }
    }

    if (acc_page)
        gtk_notebook_set_current_page (GTK_NOTEBOOK(notebook),
                                       gtk_notebook_page_num (GTK_NOTEBOOK(notebook),
                                       acc_page));
}


/** Called when the save-on-close checkbutton is toggled.
 * @internal
 * @param button the toggle button.
 * @param dialog the prefs dialog.
 */
void
gnc_save_on_close_expires_cb (GtkCheckButton *button, GtkWidget *dialog)
{
    GtkWidget *spinner = g_object_get_data (G_OBJECT(dialog),
                                            "save_on_close_wait_time");
    gtk_widget_set_sensitive (spinner, gtk_check_button_get_active (button));
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
    return g_utf8_collate (a->tabname, b->tabname);
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

    add_in = g_malloc (sizeof(addition));
    if (add_in == NULL)
    {
        g_critical ("Unable to allocate memory.\n");
        LEAVE("no memory");
        return;
    }

    add_in->filename   = g_strdup (filename);
    add_in->widgetname = g_strdup (widgetname);
    add_in->tabname    = g_strdup (tabname);
    add_in->full_page  = full_page;
    if (!add_in->filename || !add_in->widgetname || !add_in->tabname)
    {
        g_critical ("Unable to allocate memory.\n");
        g_free (add_in->filename);
        g_free (add_in->widgetname);
        g_free (add_in->tabname);
        g_free (add_in);
        LEAVE("no memory");
        return;
    }

    ptr = g_slist_find_custom (add_ins, add_in, (GCompareFunc)gnc_prefs_compare_addins);
    if (ptr)
    {
        /* problem? */
        preexisting = ptr->data;

        if (preexisting->full_page)
        {
            g_warning ("New tab %s(%s/%s/%s) conflicts with existing tab %s(%s/%s/full)",
                       add_in->tabname, add_in->filename, add_in->widgetname,
                       add_in->full_page ? "full" : "partial",
                       preexisting->tabname, preexisting->filename, preexisting->widgetname);
            error = TRUE;
        }
        else if (add_in->full_page)
        {
            g_warning ("New tab %s(%s/%s/%s) conflicts with existing tab %s(%s/%s/partial)",
                       add_in->tabname, add_in->filename, add_in->widgetname,
                       add_in->full_page ? "full" : "partial",
                       preexisting->tabname, preexisting->filename, preexisting->widgetname);
            error = TRUE;
        }
    }

    if (error)
    {
        g_free (add_in->filename);
        g_free (add_in->widgetname);
        g_free (add_in->tabname);
        g_free (add_in);
        LEAVE("err");
        return;
    }
    else
    {
        add_ins = g_slist_append (add_ins, add_in);
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
    gnc_preferences_add_page_internal (filename, widgetname, tabname, TRUE);
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
    gnc_preferences_add_page_internal (filename, widgetname, tabname, FALSE);
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

    prefs_table = g_object_get_data (G_OBJECT(dialog), PREFS_WIDGET_HASH);

    interesting = gtk_builder_get_objects (builder);

    for (runner = interesting; runner; runner = g_slist_next(runner))
    {
        widget = runner->data;
        if (GTK_IS_WIDGET(widget))
        {
            wname = gtk_widget_get_name (widget);
            name = gtk_buildable_get_buildable_id (GTK_BUILDABLE(widget));
            DEBUG("Widget type is %s and buildable get name is %s", wname, name);
            if (g_str_has_prefix (name, "pref"))
                g_hash_table_insert (prefs_table, (gchar *)name, widget);
        }
    }
    g_slist_free (interesting);

}


/** This data structure is used while building the preferences dialog
 *  to copy a grid from a glade file to the dialog under construction.
 *  It maintains state information between invocations of the function
 *  gnc_prefs_move_grid_entry which is called via a foreach loop over
 *  each item in the grid. */
struct copy_data
{
    /** The grid being copied from. */
    GtkGrid *grid_from;
    /** The grid being copied to. */
    GtkGrid *grid_to;
    /** The number of columns and rows in the grid. */
    gint cols, rows;
};


static GtkWidget *
gnc_prefs_find_page (GtkNotebook *notebook, const gchar *name)
{
    int n_pages, i;
    GtkWidget *child;
    const gchar *child_name;

    g_return_val_if_fail (GTK_IS_NOTEBOOK(notebook), NULL);
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


/** This function finds the size of a GtkGrid and saves it to
 *  the data structure.
 *
 *  @internal
 *
 *  @param widget A pointer to the widget to move.
 *
 *  @param data A pointer to a data structure passed in by the caller.
 *  This data structure contains pointers to the old and new grids
 *  plus the row offset into the new grid.
 */
static void
gnc_prefs_get_grid_size (GtkWidget *child, gpointer data)
{
    struct copy_data *copydata = data;
    gint top, left, height, width;

    gtk_grid_query_child (copydata->grid_to, child, &left, &top, &width, &height);

    if (left + width >= copydata->cols)
        copydata->cols = left + width;

    if (top + height >= copydata->rows)
        copydata->rows = top + height;
}


/** This function moves a GtkWidget from one GtkGrid to another,
 *  preserving its attachment data, etc.  It is called when adding one
 *  partial preference page to another.
 *
 *  @internal
 *
 *  @param widget A pointer to the widget to move.
 *
 *  @param data A pointer to a data structure passed in by the caller.
 *  This data structure contains pointers to the old and new grids
 *  plus the row offset into the new grid.
 */
static void
gnc_prefs_move_grid_entry (GtkWidget *child,
                            gpointer data)
{
    struct copy_data *copydata = data;
    gint top, left, height, width;
    gboolean hexpand, vexpand;
    GtkAlign halign, valign;
    gint topm, bottomm, leftm, rightm;

    ENTER("child %p, copy data %p", child, data);
    gtk_grid_query_child (copydata->grid_from, child, &left, &top, &width, &height);
    hexpand = gtk_widget_get_hexpand (child);
    vexpand = gtk_widget_get_vexpand (child);
    halign = gtk_widget_get_halign (child);
    valign = gtk_widget_get_valign (child);

    topm = gtk_widget_get_margin_top (child);
    bottomm = gtk_widget_get_margin_bottom (child);
    leftm = gtk_widget_get_margin_start (child);
    rightm = gtk_widget_get_margin_end (child);

    g_object_ref (child);
    gtk_grid_remove (copydata->grid_from, child);

    gtk_grid_attach (copydata->grid_to, child, left, copydata->rows + top , width, height);

    gtk_widget_set_hexpand (child, hexpand);
    gtk_widget_set_vexpand (child, vexpand);
    gtk_widget_set_halign (child, halign);
    gtk_widget_set_valign (child, valign);

    gtk_widget_set_margin_start (child, leftm);
    gtk_widget_set_margin_end (child, rightm);
    gtk_widget_set_margin_top (child, topm);
    gtk_widget_set_margin_bottom (child, bottomm);

    g_object_unref (child);
    LEAVE(" ");
}

static void
gnc_prefs_foreach_child (GtkWidget *parent,
                         void (*callback) (GtkWidget *, gpointer),
                         gpointer user_data)
{
    for (GtkWidget *child = gtk_widget_get_first_child (parent); child; )
    {
        GtkWidget *next = gtk_widget_get_next_sibling (child);
        callback (child, user_data);
        child = next;
    }
}


/** At dialog creation time, this function will be called once per
 *  add-in.  It performs the work of adding the page into the main
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
    struct copy_data copydata = {NULL, NULL, 0, 0};
    gchar **widgetname;
    gint i;

    ENTER("add_in %p, dialog %p", data, user_data);
    add_in = (addition *)data;
    dialog = user_data;

    DEBUG("Opening %s to get %s", add_in->filename, add_in->widgetname);
    builder = gtk_builder_new ();

    /* Adjustments etc... must come before dialog information */
    widgetname = g_strsplit (add_in->widgetname, ",", -1);

    for (i = 0; widgetname[i]; i++)
    {
        DEBUG("Opening %s to get content %s", add_in->filename, widgetname[i]);
        gnc_builder_add_from_file (builder, add_in->filename, widgetname[i]);
    }

    DEBUG("Widget Content is %s", widgetname[i - 1]);
    new_content = GTK_WIDGET(gtk_builder_get_object (builder, widgetname[i - 1]));

    g_strfreev (widgetname);
    DEBUG("done");

    /* Add to the list of interesting widgets */
    gnc_prefs_build_widget_table (builder, dialog);

    /* Connect the signals in this glade file. The dialog is passed in
     * so the callback can find "interesting" widgets from other
     * glade files if necessary (via the GPREFS_WIDGET_HASH hash table). */
    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, dialog);

    /* Prepare for recursion */
    notebook = g_object_get_data (G_OBJECT(dialog), NOTEBOOK);

    if (add_in->full_page)
    {
        label = gtk_label_new (add_in->tabname);
        gnc_label_set_alignment (label, 0.0, 0.5);
        gtk_notebook_append_page (notebook, new_content, label);
        g_object_unref (G_OBJECT(builder));
        LEAVE("appended page");
        return;
    }

    /* Copied grids must be grids */
    if (!GTK_IS_GRID(new_content))
    {
        g_critical ("The object name %s in file %s is not a GtkGrid. It cannot "
                    "be added to the preferences dialog.",
                    add_in->widgetname, add_in->filename);
        g_object_unref (G_OBJECT(builder));
        LEAVE("");
        return;
    }

    /* Does the page exist or must we create it */
    existing_content = gnc_prefs_find_page (notebook, add_in->tabname);

    if (!existing_content)
    {
        /* No existing content with this name.  Create a blank page */
        existing_content = gtk_grid_new ();
        gnc_widget_set_all_margins (existing_content, 6);
        label = gtk_label_new (add_in->tabname);
        gnc_label_set_alignment (label, 0.0, 0.5);
        gtk_notebook_append_page (notebook, existing_content, label);
        gtk_widget_set_visible (existing_content, TRUE);
        DEBUG("created new page %s, appended it", add_in->tabname);
    }
    else
    {
        /* Lets get the size of the existing grid */
        copydata.grid_to = GTK_GRID(existing_content);
        gnc_prefs_foreach_child (existing_content, gnc_prefs_get_grid_size, &copydata);

        DEBUG("found existing page %s, grid size is %d x %d", add_in->tabname, copydata.rows, copydata.cols);
    }

    /* Maybe add a spacer row */
    if (copydata.rows > 0)
    {
        label = gtk_label_new ("");
        gtk_widget_set_visible (label, TRUE);
        gtk_grid_attach (GTK_GRID(existing_content), label, 0, copydata.rows, 1, 1);
        copydata.rows = copydata.rows + 1;

        DEBUG("add spacer row");
    }

    /* Now copy all the entries in the grid */
    copydata.grid_from = GTK_GRID(new_content);
    copydata.grid_to = GTK_GRID(existing_content);
    gnc_prefs_foreach_child (new_content, gnc_prefs_move_grid_entry, &copydata);

    g_object_ref_sink (new_content);
    g_object_unref (G_OBJECT(builder));

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

    g_return_if_fail (GTK_IS_NOTEBOOK(notebook));

    /* gather tabs */
    n_pages = gtk_notebook_get_n_pages (notebook);
    for (i = n_pages - 1; i >= 0; i--)
        tabs = g_list_prepend (tabs, gtk_notebook_get_nth_page (notebook, i));

    /* sort in local copy */
    tabs = g_list_sort_with_data (tabs, (GCompareDataFunc) tab_cmp, notebook);

    /* reorder tabs */
    for (i = 0, iter = tabs; iter; i++, iter = iter->next)
        gtk_notebook_reorder_child (notebook, GTK_WIDGET(iter->data), i);

    g_list_free (tabs);
}


/*******************************/
/* Dynamically added Callbacks */
/*******************************/

static void
gnc_prefs_split_widget_name (const gchar *name, gchar **group, gchar **pref, gchar **value)
{
    const gchar *group_with_pref = name + PREF_PREFIX_LEN;
    gchar **splits = g_strsplit (group_with_pref, "/", 0);
    gchar **value_splits = g_strsplit (splits[1], "=", 0);

    *group = g_strdup (splits[0]);
    *pref = g_strdup (value_splits[0]);
    if (value)
        *value = g_strdup (value_splits[1]); /* may be NULL */
    g_strfreev (splits);
    g_strfreev (value_splits);
}

/****************************************************************************/

typedef struct
{
    GWeakRef button;
    gchar *group;
    gchar *pref;
    gboolean syncing;
} GncPrefsFontBinding;

static void
gnc_prefs_font_button_pref_changed (gpointer prefs, gchar *pref,
                                    gpointer user_data);

static void
gnc_prefs_font_binding_free (GncPrefsFontBinding *binding)
{
    if (!binding)
        return;

    gnc_prefs_remove_cb_by_func (binding->group, binding->pref,
                                 gnc_prefs_font_button_pref_changed, binding);
    g_weak_ref_clear (&binding->button);
    g_free (binding->group);
    g_free (binding->pref);
    g_free (binding);
}

static void
gnc_prefs_font_button_pref_changed (gpointer prefs, gchar *pref,
                                    gpointer user_data)
{
    GncPrefsFontBinding *binding = user_data;
    GtkFontDialogButton *button;
    PangoFontDescription *description;
    gchar *font;

    (void)prefs;
    (void)pref;

    button = GTK_FONT_DIALOG_BUTTON (g_weak_ref_get (&binding->button));
    if (!button)
        return;

    font = gnc_prefs_get_string (binding->group, binding->pref);
    if (font && *font)
    {
        description = pango_font_description_from_string (font);
        if (description)
        {
            binding->syncing = TRUE;
            gtk_font_dialog_button_set_font_desc (button, description);
            binding->syncing = FALSE;
            pango_font_description_free (description);
        }
    }
    g_free (font);
    g_object_unref (button);
}

static void
gnc_prefs_font_button_changed (GtkFontDialogButton *button,
                               GParamSpec *pspec, gpointer user_data)
{
    GncPrefsFontBinding *binding = user_data;
    PangoFontDescription *description;
    gchar *font;

    (void)pspec;

    if (binding->syncing)
        return;

    description = gtk_font_dialog_button_get_font_desc (button);
    if (!description)
        return;

    font = pango_font_description_to_string (description);
    if (!gnc_prefs_set_string (binding->group, binding->pref, font))
        PINFO ("Failed to save preference at %s, %s with %s",
               binding->group, binding->pref, font);
    g_free (font);
}

/** Connect a GtkFontDialogButton widget to its stored value in the preferences database.
 *
 *  @internal
 *
 *  @param button A pointer to the font dialog button that should be connected.
 */
static void
gnc_prefs_connect_font_button (GtkFontDialogButton *button)
{
    GncPrefsFontBinding *binding;
    gchar *group, *pref;

    g_return_if_fail (GTK_IS_FONT_DIALOG_BUTTON (button));

    gnc_prefs_split_widget_name (gtk_buildable_get_buildable_id (
                                    GTK_BUILDABLE (button)),
                                &group, &pref, NULL);

    binding = g_new0 (GncPrefsFontBinding, 1);
    g_weak_ref_init (&binding->button, G_OBJECT (button));
    binding->group = group;
    binding->pref = pref;
    g_object_set_data_full (G_OBJECT (button), "gnc-prefs-font-binding",
                            binding, (GDestroyNotify)gnc_prefs_font_binding_free);
    gnc_prefs_register_cb (binding->group, binding->pref,
                           gnc_prefs_font_button_pref_changed, binding);
    g_signal_connect (button, "notify::font-desc",
                      G_CALLBACK (gnc_prefs_font_button_changed), binding);
    gnc_prefs_font_button_pref_changed (NULL, NULL, binding);

    gtk_widget_set_visible (GTK_WIDGET(button), TRUE);
}

/****************************************************************************/

/** Set the visible name of a preference folder chooser.
 *
 *  @internal
 *
 *  @param button A button representing the selected folder.
 *  @param uri The selected folder URI.
 */
static void
set_file_chooser_button_label (GtkButton *button, const gchar *uri)
{
    GFile *folder = uri && *uri ? g_file_new_for_uri (uri) : NULL;
    gchar *label = folder ? g_file_get_parse_name (folder) : NULL;

    gtk_button_set_label (button, label ? label : _("Select a folder"));

    g_free (label);
    g_clear_object (&folder);
}

static void
file_chooser_selected_cb (GObject *source_object, GAsyncResult *result,
                          gpointer user_data)
{
    GtkButton *button = GTK_BUTTON (user_data);
    GtkImage *image = g_object_get_data (G_OBJECT(button), "path_head_error");
    const gchar *group = g_object_get_data (G_OBJECT(button), "group");
    const gchar *pref = g_object_get_data (G_OBJECT(button), "pref");
    GError *error = NULL;
    GFile *folder;
    gchar *folder_uri;
    gchar *old_path_head_uri = gnc_doclink_get_path_head ();
    GtkRoot *root;

    folder = gtk_file_dialog_select_folder_finish (GTK_FILE_DIALOG (source_object),
                                                    result, &error);
    if (!folder)
    {
        if (error && !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
            PINFO("Failed to select preference folder: %s", error->message);
        g_clear_error (&error);
        g_free (old_path_head_uri);
        g_object_unref (button);
        return;
    }

    folder_uri = g_file_get_uri (folder);
    if (!g_str_has_suffix (folder_uri, "/"))
    {
        gchar *folder_with_slash = g_strconcat (folder_uri, "/", NULL);
        g_free (folder_uri);
        folder_uri = folder_with_slash;
    }

    gtk_widget_set_visible (GTK_WIDGET(image), FALSE);
    set_file_chooser_button_label (button, folder_uri);

    if (!gnc_prefs_set_string (group, pref, folder_uri))
        PINFO("Failed to save preference at %s, %s with %s", group, pref, folder_uri);
    else
    {
        root = gtk_widget_get_root (GTK_WIDGET (button));
        gnc_doclink_pref_path_head_changed (
            GTK_IS_WINDOW (root) ? GTK_WINDOW (root) : NULL,
            old_path_head_uri);
    }

    g_object_unref (folder);
    g_free (old_path_head_uri);
    g_free (folder_uri);
    g_object_unref (button);
}

static void
file_chooser_button_clicked_cb (GtkButton *button, gpointer user_data)
{
    const gchar *group = g_object_get_data (G_OBJECT(button), "group");
    const gchar *pref = g_object_get_data (G_OBJECT(button), "pref");
    GtkFileDialog *dialog;
    GtkRoot *root;
    GtkWindow *parent = NULL;
    gchar *uri;
    GFile *initial_folder = NULL;

    (void)user_data;

    dialog = gtk_file_dialog_new ();
    gtk_file_dialog_set_title (dialog, _("Select a folder"));

    uri = gnc_prefs_get_string (group, pref);
    if (uri && *uri)
    {
        initial_folder = g_file_new_for_uri (uri);
        gtk_file_dialog_set_initial_folder (dialog, initial_folder);
    }

    root = gtk_widget_get_root (GTK_WIDGET (button));
    if (GTK_IS_WINDOW (root))
        parent = GTK_WINDOW (root);

    gtk_file_dialog_select_folder (dialog, parent, NULL,
                                   file_chooser_selected_cb,
                                   g_object_ref (button));

    g_clear_object (&initial_folder);
    g_object_unref (dialog);
    g_free (uri);
}

/** Connect a folder chooser button to its stored value in the preferences database.
 *
 *  @internal
 *
 *  @param button A button that opens the folder chooser.
 *
 *  @param boxname The Hbox name that contains the folder chooser and Clear button.
 */
static void
gnc_prefs_connect_file_chooser_button (GtkButton *button, const gchar *boxname)
{
    GtkImage *image;
    gchar *group, *pref;
    gchar *uri;
    gboolean folder_set = TRUE;

    g_return_if_fail (GTK_IS_BUTTON(button));

    if (boxname == NULL)
        gnc_prefs_split_widget_name (gtk_buildable_get_buildable_id (GTK_BUILDABLE(button)), &group, &pref, NULL);
    else
        gnc_prefs_split_widget_name (boxname, &group, &pref, NULL);

    uri = gnc_prefs_get_string (group, pref);

    PINFO("Uri is %s", uri);

    if (uri && *uri) // default entry
    {
        gchar *path_head = gnc_uri_get_path (uri);

        // Test whether the current folder still exists.
        if (g_file_test (path_head, G_FILE_TEST_IS_DIR))
            set_file_chooser_button_label (button, uri);
        else
            folder_set = FALSE;

        g_free (path_head);
    }

    image = g_object_get_data (G_OBJECT(button), "path_head_error");

    if (folder_set) // If current folder missing, display error and tt message
        gtk_widget_set_visible (GTK_WIDGET(image), FALSE);
    else
    {
        gchar *path_head = gnc_doclink_get_unescape_uri (NULL, uri, "file");
        gchar *ttip = g_strconcat (_("Path does not exist, "), path_head, NULL);

        gtk_widget_set_tooltip_text (GTK_WIDGET(image), ttip);
        gtk_widget_set_visible (GTK_WIDGET(image), TRUE);

        g_free (ttip);
        g_free (path_head);
    }

    g_signal_connect (button, "clicked",
                      G_CALLBACK(file_chooser_button_clicked_cb), NULL);

    g_object_set_data_full (G_OBJECT(button), "group", g_strdup (group), (GDestroyNotify) g_free);
    g_object_set_data_full (G_OBJECT(button), "pref", g_strdup (pref), (GDestroyNotify) g_free);

    g_free (group);
    g_free (pref);
    g_free (uri);

    gtk_widget_set_visible (GTK_WIDGET(button), TRUE);
}

/** Callback for a Clear button for a folder chooser.
 *
 *  @internal
 *
 *  @param button A pointer to the button widget emitting signal.
 *
 *  @param user_data A pointer to the folder chooser button.
 */
static void
file_chooser_clear_cb (GtkButton *button, gpointer user_data)
{
    GtkButton *folder_button = GTK_BUTTON(user_data);
    const gchar *group = g_object_get_data (G_OBJECT(folder_button), "group");
    const gchar *pref = g_object_get_data (G_OBJECT(folder_button), "pref");
    GtkImage *image = g_object_get_data (G_OBJECT(folder_button), "path_head_error");
    gchar *old_path_head_uri = gnc_doclink_get_path_head ();
    GtkRoot *root;

    (void)button;

    if (!gnc_prefs_set_string (group, pref, ""))
        PINFO("Failed to Clear preference at %s, %s", group, pref);
    else
    {
        root = gtk_widget_get_root (GTK_WIDGET (folder_button));
        gnc_doclink_pref_path_head_changed (
            GTK_IS_WINDOW (root) ? GTK_WINDOW (root) : NULL,
            old_path_head_uri);
    }

    set_file_chooser_button_label (folder_button, NULL);
    gtk_widget_set_visible (GTK_WIDGET (image), FALSE);
    g_free (old_path_head_uri);
}

/****************************************************************************/

/** Connect a GtkCheckButton widget to its stored value in the preferences database.
 *  This covers both independent boolean check buttons and grouped check
 *  buttons.
 *
 *  @internal
 *
 *  @param button A pointer to the check button that should be
 *  connected.
 */
static void
gnc_prefs_connect_check_button (GtkCheckButton *button)
{
    gchar *group, *pref, *value;

    g_return_if_fail (GTK_IS_CHECK_BUTTON(button));

    gnc_prefs_split_widget_name (gtk_buildable_get_buildable_id (GTK_BUILDABLE(button)), &group, &pref, &value);

    gnc_prefs_bind (group, pref, value, G_OBJECT(button), "active");

    g_free (group);
    g_free (pref);
    g_free (value);
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

    g_return_if_fail (GTK_IS_SPIN_BUTTON(spin));

    gnc_prefs_split_widget_name (gtk_buildable_get_buildable_id (GTK_BUILDABLE(spin)), &group, &pref, NULL);

    gnc_prefs_bind (group, pref, NULL, G_OBJECT(spin), "value");

    g_free (group);
    g_free (pref);
}

/****************************************************************************/

/** Connect a GtkDropDown widget to its stored value in the preferences database.
 *
 *  @internal
 *
 *  @param drop_down A pointer to the drop-down that should be connected.
 */
static void
gnc_prefs_connect_drop_down (GtkDropDown *drop_down)
{
    gchar *group, *pref;

    g_return_if_fail (GTK_IS_DROP_DOWN(drop_down));

    gnc_prefs_split_widget_name (gtk_buildable_get_buildable_id (
                                    GTK_BUILDABLE(drop_down)),
                                &group, &pref, NULL);

    gnc_prefs_bind (group, pref, NULL, G_OBJECT(drop_down), "selected");

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
    gchar *group, *pref;

    g_return_if_fail (GNC_IS_CURRENCY_EDIT(gce));

    gnc_prefs_split_widget_name (boxname, &group, &pref, NULL);

    gnc_prefs_bind (group, pref, NULL, G_OBJECT(gce), "mnemonic");

    g_free (group);
    g_free (pref);

    gtk_widget_set_visible (GTK_WIDGET(gce), TRUE);
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

    g_return_if_fail (GTK_IS_ENTRY(entry));

    gnc_prefs_split_widget_name (gtk_buildable_get_buildable_id (GTK_BUILDABLE(entry)), &group, &pref, NULL);

    gnc_prefs_bind (group, pref, NULL, G_OBJECT(entry), "text");

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

    g_return_if_fail (GNC_IS_PERIOD_SELECT(period));

    gnc_prefs_split_widget_name (boxname, &group, &pref, NULL);

    gnc_prefs_bind (group, pref, NULL, G_OBJECT(period), "active");

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

    g_return_if_fail (GNC_IS_DATE_EDIT(gde));

    gnc_prefs_split_widget_name (boxname, &group, &pref, NULL);

    gnc_prefs_bind (group, pref, NULL, G_OBJECT(gde), "time");

    g_free (group);
    g_free (pref);
}


/****************************************************************************/

static void
gnc_preferences_help_clicked_cb (GtkButton *button, gpointer user_data)
{
    (void)button;
    gnc_gnome_help (GTK_WINDOW (user_data), DF_MANUAL, DL_GLOBPREFS);
}

static void
gnc_preferences_close_clicked_cb (GtkButton *button, gpointer user_data)
{
    (void)button;
    gnc_account_separator_validate_async (GTK_WINDOW (user_data));
}

static gboolean
gnc_preferences_close_request_cb (GtkWindow *window, gpointer user_data)
{
    (void)user_data;
    gnc_account_separator_validate_async (window);
    return TRUE;
}

static gboolean
gnc_preferences_escape_cb (GtkWidget *widget, GVariant *args,
                           gpointer user_data)
{
    (void)widget;
    (void)args;
    gnc_account_separator_validate_async (GTK_WINDOW (user_data));
    return TRUE;
}

static void
gnc_preferences_add_shortcuts (GtkWindow *window)
{
    GtkShortcutController *controller = GTK_SHORTCUT_CONTROLLER (
        gtk_shortcut_controller_new ());

    gtk_shortcut_controller_set_scope (controller, GTK_SHORTCUT_SCOPE_MANAGED);
    gtk_shortcut_controller_add_shortcut (
        controller,
        gtk_shortcut_new (
            gtk_keyval_trigger_new (GDK_KEY_Escape, 0),
            gtk_callback_action_new (gnc_preferences_escape_cb, window, NULL)));
    gtk_widget_add_controller (GTK_WIDGET (window),
                               GTK_EVENT_CONTROLLER (controller));
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

    if (GTK_IS_FONT_DIALOG_BUTTON(widget))
    {
        DEBUG("  %s - font dialog button", name);
        gnc_prefs_connect_font_button (GTK_FONT_DIALOG_BUTTON(widget));
    }
    else if (GTK_IS_CHECK_BUTTON(widget))
    {
        DEBUG("  %s - check button", name);
        gnc_prefs_connect_check_button (GTK_CHECK_BUTTON(widget));
    }
    else if (GTK_IS_SPIN_BUTTON(widget))
    {
        DEBUG("  %s - spin button", name);
        gnc_prefs_connect_spin_button (GTK_SPIN_BUTTON(widget));
    }
    else if (GTK_IS_DROP_DOWN(widget))
    {
        DEBUG("  %s - drop-down", name);
        gnc_prefs_connect_drop_down (GTK_DROP_DOWN(widget));
    }
    else if (GTK_IS_ENTRY(widget))
    {
        DEBUG("  %s - entry", name);
        gnc_prefs_connect_entry (GTK_ENTRY(widget));
    }
    else if (GTK_IS_BOX(widget))
    {
        /* Test custom widgets are all children of a hbox */
        GtkWidget *widget_child = gtk_widget_get_first_child (widget);
        if (!widget_child)
            return;
        DEBUG("  %s - box", name);
        DEBUG("Box widget type is %s and name is %s", gtk_widget_get_name (GTK_WIDGET(widget_child)), name);
        if (GNC_IS_CURRENCY_EDIT(widget_child))
        {
            DEBUG("  %s - currency_edit", name);
            gnc_prefs_connect_currency_edit (GNC_CURRENCY_EDIT(widget_child), name );
        }
        else if (GNC_IS_PERIOD_SELECT(widget_child))
        {
            DEBUG("  %s - period_select", name);
            gnc_prefs_connect_period_select (GNC_PERIOD_SELECT(widget_child), name );
        }
        else if (GNC_IS_DATE_EDIT(widget_child))
        {
            DEBUG("  %s - date_edit", name);
            gnc_prefs_connect_date_edit (GNC_DATE_EDIT(widget_child), name );
        }
        else if (g_object_get_data (G_OBJECT(widget_child), "file-chooser-button"))
        {
            DEBUG("  %s - file chooser button", name);
            gnc_prefs_connect_file_chooser_button (GTK_BUTTON(widget_child), name );
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
gnc_preferences_dialog_create (GtkWindow *parent)
{
    GtkBuilder *builder;
    GtkWidget *dialog, *notebook, *label, *image, *spinner, *entry;
    GtkWidget *box, *date, *period, *currency, *fcb, *button;
    GtkWidget *help_button, *close_button;
    GHashTable *prefs_table;
    GDate* gdate = NULL;
    gchar buf[128];
    GtkStringList *date_formats;
    const char *locale_items[2];
    gchar *locale_item;
    gnc_commodity *locale_currency;
    gnc_commodity *root_currency;
    const gchar *currency_name;

    ENTER("");
    DEBUG("Opening dialog-preferences.glade:");
    builder = gtk_builder_new ();

    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "auto_decimal_places_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "autosave_interval_minutes_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "save_on_close_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "date_backmonth_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "default_zoom_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "default_chart_point_style_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "default_chart_point_size_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "default_chart_point_size_hover_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "default_chart_tooltip_engage_radius_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "default_chart_caret_size_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "chart_point_style_list");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "max_transactions_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "key_length_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "new_search_limit_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "retain_days_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "tab_width_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "date_formats");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "atm_fee_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "auto_add_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "auto_clear_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "match_adj");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "likely_day_threshold");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "unlikely_day_threshold");
    gnc_builder_add_from_file (builder, "dialog-preferences.glade", "gnucash_preferences_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "gnucash_preferences_dialog"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(dialog), "gnc-id-preferences");

    /* parent */
    gtk_window_set_transient_for (GTK_WINDOW(dialog), GTK_WINDOW(parent));

    label = GTK_WIDGET(gtk_builder_get_object (builder, "sample_account"));
    g_object_set_data (G_OBJECT(dialog), "sample_account", label);

    image = GTK_WIDGET(gtk_builder_get_object (builder, "separator_error"));
    g_object_set_data (G_OBJECT(dialog), "separator_error", image);

    entry = GTK_WIDGET(gtk_builder_get_object (builder, "pref/general/account-separator"));
    g_object_set_data (G_OBJECT(dialog), "account-separator", entry);

    spinner = GTK_WIDGET(gtk_builder_get_object (builder, "pref/general/save-on-close-wait-time"));
    g_object_set_data (G_OBJECT(dialog), "save_on_close_wait_time", spinner);

    DEBUG("autoconnect");
    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, dialog);

    help_button = GTK_WIDGET (gtk_builder_get_object (builder, "helpbutton2"));
    close_button = GTK_WIDGET (gtk_builder_get_object (builder, "closebutton2"));
    g_signal_connect (help_button, "clicked",
                      G_CALLBACK (gnc_preferences_help_clicked_cb), dialog);
    g_signal_connect (close_button, "clicked",
                      G_CALLBACK (gnc_preferences_close_clicked_cb), dialog);
    g_signal_connect (dialog, "close-request",
                      G_CALLBACK (gnc_preferences_close_request_cb), NULL);
    gnc_preferences_add_shortcuts (GTK_WINDOW (dialog));

    DEBUG("done");

    notebook = GTK_WIDGET(gtk_builder_get_object (builder, "notebook1"));
    prefs_table = g_hash_table_new (g_str_hash, g_str_equal);
    g_object_set_data (G_OBJECT(dialog), NOTEBOOK, notebook);
    g_object_set_data_full (G_OBJECT(dialog), PREFS_WIDGET_HASH,
                            prefs_table, (GDestroyNotify)g_hash_table_destroy);

    box = GTK_WIDGET(gtk_builder_get_object (builder,
                     "pref/" GNC_PREFS_GROUP_ACCT_SUMMARY "/" GNC_PREF_START_PERIOD));
    period = gnc_period_select_new (TRUE);
    gtk_widget_set_visible (period, TRUE);
    gnc_box_append_full (GTK_BOX(box), period, TRUE, TRUE, 0);

    box = GTK_WIDGET(gtk_builder_get_object (builder,
                     "pref/" GNC_PREFS_GROUP_ACCT_SUMMARY "/" GNC_PREF_END_PERIOD));
    period = gnc_period_select_new (FALSE);
    gtk_widget_set_visible (period, TRUE);
    gnc_box_append_full (GTK_BOX(box), period, TRUE, TRUE, 0);

    box = GTK_WIDGET(gtk_builder_get_object (builder,
                     "pref/" GNC_PREFS_GROUP_ACCT_SUMMARY "/" GNC_PREF_START_DATE));
    date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
    gtk_widget_set_visible (date, TRUE);
    gnc_box_append_full (GTK_BOX(box), date, TRUE, TRUE, 0);

    box = GTK_WIDGET(gtk_builder_get_object (builder,
                     "pref/" GNC_PREFS_GROUP_ACCT_SUMMARY "/" GNC_PREF_END_DATE));
    date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
    gtk_widget_set_visible (date, TRUE);
    gnc_box_append_full (GTK_BOX(box), date, TRUE, TRUE, 0);

    box = GTK_WIDGET(gtk_builder_get_object (builder,
                     "pref/" GNC_PREFS_GROUP_GENERAL "/" GNC_PREF_CURRENCY_OTHER));
    currency = gnc_currency_edit_new ();
    gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(currency), gnc_default_currency());
    gtk_widget_set_visible (currency, TRUE);
    gnc_box_append_full (GTK_BOX(box), currency, TRUE, TRUE, 0);

    box = GTK_WIDGET(gtk_builder_get_object (builder,
                     "pref/" GNC_PREFS_GROUP_GENERAL_REPORT "/" GNC_PREF_CURRENCY_OTHER));
    currency = gnc_currency_edit_new ();
    gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(currency), gnc_default_currency());
    gtk_widget_set_visible (currency, TRUE);
    gnc_box_append_full (GTK_BOX(box), currency, TRUE, TRUE, 0);

    box = GTK_WIDGET(gtk_builder_get_object (builder,
                     "pref/" GNC_PREFS_GROUP_GENERAL "/" GNC_DOC_LINK_PATH_HEAD));
    fcb = gtk_button_new_with_label (_("Select a folder"));
    g_object_set_data (G_OBJECT(fcb), "file-chooser-button", GINT_TO_POINTER (TRUE));
    gnc_box_append_full (GTK_BOX(box), fcb, TRUE, TRUE, 0);
    button = gtk_button_new_with_label (_("Clear"));
    gnc_box_append_full (GTK_BOX(box), button, TRUE, TRUE, 0);
    gtk_widget_set_visible (button, TRUE);
    g_signal_connect (GTK_BUTTON(button), "clicked",
                      G_CALLBACK(file_chooser_clear_cb), fcb);

    image = GTK_WIDGET(gtk_builder_get_object (builder, "path_head_error"));
    g_object_set_data (G_OBJECT(fcb), "path_head_error", image);

    /* Add to the list of interesting widgets */
    gnc_prefs_build_widget_table (builder, dialog);

    g_slist_foreach (add_ins, gnc_preferences_build_page, dialog);

    /* Sort tabs alphabetically */
    gnc_prefs_sort_pages (GTK_NOTEBOOK(notebook));
    gtk_notebook_set_current_page (GTK_NOTEBOOK(notebook), 0);

    DEBUG("We have the following interesting widgets:");
    gnc_prefs_block_all (); // Block All Registered callbacks
    g_hash_table_foreach (prefs_table, (GHFunc)gnc_prefs_connect_one, dialog);
    gnc_prefs_unblock_all (); // UnBlock All Registered callbacks
    DEBUG("Done with interesting widgets.");

    /* Other stuff */
    gdate = g_date_new_dmy (31, G_DATE_JULY, 2013);
    g_date_strftime (buf, sizeof(buf), "%x", gdate);
    date_formats = GTK_STRING_LIST (
        gtk_builder_get_object (builder, "date_formats"));
    locale_item = g_strdup_printf ("%s  %s", _("Locale"), buf);
    locale_items[0] = locale_item;
    locale_items[1] = NULL;
    gtk_string_list_splice (date_formats, QOF_DATE_FORMAT_LOCALE, 1,
                            locale_items);
    g_date_free (gdate);
    g_free (locale_item);

    locale_currency = gnc_locale_default_currency ();
    currency_name = gnc_commodity_get_printname (locale_currency);
    label = GTK_WIDGET(gtk_builder_get_object (builder, "locale_currency"));
    gtk_label_set_label (GTK_LABEL(label), currency_name);
    label = GTK_WIDGET(gtk_builder_get_object (builder, "locale_currency2"));
    gtk_label_set_label (GTK_LABEL(label), currency_name);

    root_currency = xaccAccountGetCommodity (gnc_get_current_root_account ());

    /* Prior to version 5.0, if the qif importer was used from 'New User'
       dialog the Root Account currency was not set */
    if (!root_currency)
    {
        QofBook *book = gnc_get_current_book();
        Account *root = gnc_get_current_root_account ();
        xaccAccountBeginEdit (root);
        xaccAccountSetCommodity (root, locale_currency);
        xaccAccountCommitEdit (root);
        qof_book_mark_session_dirty (book);
        currency_name = gnc_commodity_get_printname (locale_currency);
    }
    else
        currency_name = gnc_commodity_get_printname (root_currency);
    label = GTK_WIDGET(gtk_builder_get_object (builder, "root_currency"));
    gtk_label_set_label (GTK_LABEL(label), currency_name);
    label = GTK_WIDGET(gtk_builder_get_object (builder, "root_currency2"));
    gtk_label_set_label (GTK_LABEL(label), currency_name);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "pref/general/save-on-close-expires"));
    gnc_save_on_close_expires_cb (GTK_CHECK_BUTTON(button), dialog);

    g_object_unref (G_OBJECT(builder));

    /* save the original account separator in case it changes */
    g_object_set_data_full (G_OBJECT(entry), "original_text",
                            g_strdup (gtk_editable_get_text (GTK_EDITABLE (entry))),
                            g_free);

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
    gtk_window_present (GTK_WINDOW(dialog));
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
    gnc_unregister_gui_component_by_data (DIALOG_PREFERENCES_CM_CLASS, dialog);
    gtk_window_destroy (GTK_WINDOW(dialog));
    LEAVE(" ");
}


/*  This function creates the preferences dialog and presents it to
 *  the user.  The preferences dialog is a singleton, so if a
 *  preferences dialog already exists it will be raised to the top of
 *  the window stack instead of creating a new dialog. */
void
gnc_preferences_dialog (GtkWindow *parent)
{
    GtkWidget *dialog;

    ENTER("");
    if (gnc_forall_gui_components (DIALOG_PREFERENCES_CM_CLASS,
                                   show_handler, NULL))
    {
        LEAVE("existing window");
        return;
    }

    dialog = gnc_preferences_dialog_create(parent);

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(dialog), parent);
    gtk_window_present (GTK_WINDOW (dialog));

    gnc_register_gui_component (DIALOG_PREFERENCES_CM_CLASS,
                                NULL, close_handler, dialog);

    LEAVE(" ");
}

/** @} */
/** @} */
