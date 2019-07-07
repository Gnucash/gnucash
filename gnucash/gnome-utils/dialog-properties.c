/********************************************************************\
 * dialog-properties.c -- properties dialog                         *
 *                                                                  *
 * Copyright (C) 2005 David Hampton                                 *
 * Copyright (C) 2013 Geert Janssens                                *
 * Copyright (C) 2019 Robert Fewell                                 *
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
/** @addtogroup PropDialog Properties Dialog
    @{ */
/** @file dialog-properties.c
    @brief Dialog for handling Gnucash file properties.
    @author Copyright (c) 2019 Robert Fewell

    These functions are the external API available for the
    properties dialog. This dialog allows a user to modify
    several Gnucash properties.
    Any module may add a page (or partial page) of properties
    to the dialog.  These additions are done by providing
    the name of a glade file and the content to load from that
    file along with a widget in that file.  If a partial
    page is added, the widget name provided must be that of
    a GtkGrid containing four columns. If a full page is added,
    the widget name provided to this code can be any kind of
    widget, but for consistency it should probably be the same.

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
#include "gnc-gobject-utils.h"
#include "gnc-engine.h"

#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-component-manager.h"
#include "dialog-properties.h"

#include "gncTaxTable.h"
#include "gnc-date-format.h"
#include "gnc-tree-model-budget.h"
#include "gnc-currency-edit.h"
#include "gnc-date-edit.h"
#include "gnc-period-select.h"


#define DIALOG_PROPERTIES_CM_CLASS  "dialog-properties"
#define GNC_PREFS_GROUP             "dialogs.properties"
#define PROP_PREFIX_LEN              sizeof("prop/") - 1
#define PROPS_WIDGET_HASH           "props_widget_hash"
#define NOTEBOOK                    "notebook"

/** The debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_PROPS;

void gnc_properties_response_cb (GtkDialog *dialog, gint response, GtkDialog *unused);

/** This data structure holds the information for a single addition to
 *  the properties dialog. */
typedef struct addition_t
{
    /** The relative name of the file where the glade data for this
     *  addition can be found. */
    gchar *filename;
    /** The name of the widget within the glade data file that should be
     *  added to the properties dialog.  This should point to a
     *  GtkGrid widget that has four columns. */
    gchar *widgetname;
    /** The name of the tab within the properties dialog where these
     *  widgets should be placed. */
    gchar *tabname;
    /** TRUE if this addition represents a full page in the properties
     *  dialog.  FALSE if this page may be combined with other pages. */
    gboolean full_page;
} addition;


/** A list of all additions that have been made to the properties
 *  dialog.  The data fields for this list are ::addition data
 *  structures. */
GSList *add_ins_props = NULL;

/** Record some values so we can test for refresh
 *  after changes
 */
gboolean use_split_action_for_num_before;
// gboolean use_book_currency_before;


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
gnc_props_compare_addins (addition *a,
                          addition *b)
{
    return g_utf8_collate(a->tabname, b->tabname);
}


/** This is the common function that adds any set of properties to
 *  the properties dialog.  It allocates a data structure to remember
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
 *         the name of the widget to add to the properties dialog.
 *
 *  @param tabname The name this page of properties should have in
 *  the dialog notebook.
 *
 *  @param full_page Is this a full page of properties or a partial page.
 */
static void
gnc_properties_add_page_internal (const gchar *filename,
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

    ptr = g_slist_find_custom (add_ins_props, add_in, (GCompareFunc)gnc_props_compare_addins);
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
        add_ins_props = g_slist_append (add_ins_props, add_in);
    }
    LEAVE("");
}


/*  This function adds a full page of properties to the properties
 *  dialog.  When the dialog is created, the specified content will be
 *  pulled from the specified glade file and added to the properties
 *  dialog with the specified tab name.  The tab name may not be
 *  duplicated.  For example, the Business code might have a full page
 *  of its own properties. */
void
gnc_properties_add_page (const gchar *filename,
                          const gchar *widgetname,
                          const gchar *tabname)
{
    gnc_properties_add_page_internal (filename, widgetname, tabname, TRUE);
}


/*  This function adds a partial page of properties to the
 *  properties dialog.  When the dialog is created, the specified
 *  content will be pulled from the glade file and added to the
 *  properties dialog with the specified tab name.  The tab name
 *  may be duplicated.  For example, the HBCI properties may share a
 *  "Data Import" page with QIF and other methods. */
void
gnc_properties_add_to_page (const gchar *filename,
                             const gchar *widgetname,
                             const gchar *tabname)
{
    gnc_properties_add_page_internal (filename, widgetname, tabname, FALSE);
}


/*******************************************************************/

/** This function builds a hash table of "interesting" widgets,
 *  i.e. widgets whose name starts with "pref/".  This table is
 *  needed to perform name->widget lookups when binding the widgets
 *  to their matching properties.
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
gnc_props_build_widget_table (GtkBuilder *builder,
                              GtkWidget *dialog)
{
    GHashTable *props_table;
    GSList *interesting, *runner;
    const gchar *name;
    const gchar *wname;
    GtkWidget *widget;

    props_table = g_object_get_data (G_OBJECT(dialog), PROPS_WIDGET_HASH);

    interesting = gtk_builder_get_objects (builder);

    for (runner = interesting; runner; runner = g_slist_next (runner))
    {
        widget = runner->data;
        if (GTK_IS_WIDGET(widget))
        {
            wname = gtk_widget_get_name (widget);
            name = gtk_buildable_get_name (GTK_BUILDABLE(widget));
            DEBUG("Widget type is %s and buildable get name is %s", wname, name);
            if (g_str_has_prefix (name, "prop"))
                g_hash_table_insert (props_table, (gchar *)name, widget);
        }
    }
    g_slist_free (interesting);
}


/** This data structure is used while building the properties dialog
 *  to copy a grid from a glade file to the dialog under construction.
 *  It maintains state information between invocations of the function
 *  gnc_props_move_grid_entry which is called via a foreach loop over
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
gnc_props_find_page (GtkNotebook *notebook, const gchar *name)
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
gnc_props_get_grid_size (GtkWidget *child, gpointer data)
{
    struct copy_data *copydata = data;
    gint top, left, height, width;

    gtk_container_child_get (GTK_CONTAINER(copydata->grid_to), child,
                             "left-attach", &left,
                             "top-attach", &top,
                             "height", &height,
                             "width", &width,
                             NULL);

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
gnc_props_move_grid_entry (GtkWidget *child,
                            gpointer data)
{
    struct copy_data *copydata = data;
    gint top, left, height, width;
    gboolean hexpand, vexpand;
    GtkAlign halign, valign;
    gint topm, bottomm, leftm, rightm;

    ENTER("child %p, copy data %p", child, data);
    gtk_container_child_get (GTK_CONTAINER(copydata->grid_from), child,
                             "left-attach", &left,
                             "top-attach", &top,
                             "height", &height,
                             "width", &width,
                             NULL);
    hexpand = gtk_widget_get_hexpand (child);
    vexpand = gtk_widget_get_vexpand (child);
    halign = gtk_widget_get_halign (child);
    valign = gtk_widget_get_valign (child);

    g_object_get (child, "margin-top", &topm, "margin-bottom", &bottomm, NULL);
    g_object_get (child, "margin-left", &leftm, "margin-right", &rightm, NULL);

    g_object_ref (child);
    gtk_container_remove (GTK_CONTAINER(copydata->grid_from), child);

    gtk_grid_attach (copydata->grid_to, child, left, copydata->rows + top , width, height);

    gtk_widget_set_hexpand (child, hexpand);
    gtk_widget_set_vexpand (child, vexpand);
    gtk_widget_set_halign (child, halign);
    gtk_widget_set_valign (child, valign);

    g_object_set (child, "margin-left", leftm, "margin-right", rightm, NULL);
    g_object_set (child, "margin-top", topm, "margin-bottom", bottomm, NULL);

    g_object_unref (child);
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
gnc_properties_build_page (gpointer data,
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
    builder = gtk_builder_new();

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
    gnc_props_build_widget_table (builder, dialog);

    /* Connect the signals in this glade file. The dialog is passed in
     * so the the callback can find "interesting" widgets from other
     * glade files if necessary (via the GPROPS_WIDGET_HASH hash table). */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, dialog);

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
        g_critical("The object name %s in file %s is not a GtkGrid. It cannot "
                   "be added to the properties dialog.",
                   add_in->widgetname, add_in->filename);
        g_object_unref (G_OBJECT(builder));
        LEAVE("");
        return;
    }

    /* Does the page exist or must we create it */
    existing_content = gnc_props_find_page (notebook, add_in->tabname);

    if (!existing_content)
    {
        /* No existing content with this name.  Create a blank page */
        existing_content = gtk_grid_new();
        gtk_container_set_border_width (GTK_CONTAINER(existing_content), 6);
        label = gtk_label_new (add_in->tabname);
        gnc_label_set_alignment (label, 0.0, 0.5);
        gtk_notebook_append_page (notebook, existing_content, label);
        gtk_widget_show_all (existing_content);
        DEBUG("created new page %s, appended it", add_in->tabname);
    }
    else
    {
        /* Lets get the size of the existing grid */
        copydata.grid_to = GTK_GRID(existing_content);
        gtk_container_foreach (GTK_CONTAINER(existing_content), gnc_props_get_grid_size, &copydata);

        DEBUG("found existing page %s, grid size is %d x %d", add_in->tabname, copydata.rows, copydata.cols);
    }

    /* Maybe add a spacer row */
    if (copydata.rows > 0)
    {
        label = gtk_label_new("");
        gtk_widget_show (label);
        gtk_grid_attach (GTK_GRID(existing_content), label, 0, copydata.rows, 1, 1);
        copydata.rows = copydata.rows + 1;

        DEBUG("add spacer row");
    }

    /* Now copy all the entries in the grid */
    copydata.grid_from = GTK_GRID(new_content);
    copydata.grid_to = GTK_GRID(existing_content);
    gtk_container_foreach (GTK_CONTAINER(new_content), gnc_props_move_grid_entry, &copydata);

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
gnc_props_sort_pages (GtkNotebook *notebook)
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


static void
gnc_properties_do_refresh (void)
{
    QofBook *book = gnc_get_current_book ();
    gint use_read_only_threshold_before = qof_book_get_num_days_autoreadonly (book);

    gboolean use_split_action_for_num_after;
//    gboolean use_book_currency_after = gnc_book_use_book_currency (book);
    gint use_read_only_threshold_after;

    gboolean do_refresh = FALSE;

    // mark cached value as invalid so we get new value
    book->cached_num_days_autoreadonly_isvalid = FALSE;
    use_read_only_threshold_after = qof_book_get_num_days_autoreadonly (book);

    // mark cached value as invalid so we get new value
    book->cached_num_field_source_isvalid = FALSE;
    use_split_action_for_num_after = qof_book_use_split_action_for_num_field (book);

    if (use_split_action_for_num_before != use_split_action_for_num_after)
    {
        gnc_book_option_num_field_source_change_cb (
                                                use_split_action_for_num_after);
        do_refresh = TRUE;
    }

//    if (use_book_currency_before != use_book_currency_after)
//    {
//        gnc_book_option_book_currency_selected_cb (use_book_currency_after);
//        do_refresh = TRUE;
//    }

    if (use_read_only_threshold_before != use_read_only_threshold_after)
        do_refresh = TRUE;

    if (do_refresh)
        gnc_gui_refresh_all ();
}

/*******************************/
/* Dynamically added Callbacks */
/*******************************/

static void
gnc_props_split_widget_name (const gchar *name, gchar **path)
{
    const gchar *group_with_pref = name + PROP_PREFIX_LEN;
    *path = g_strdup (group_with_pref);
}

/****************************************************************************/

/** Connect a GtkFontButton widget to its properties value.
 *
 *  @internal
 *
 *  @param fb A pointer to the font button that should be connected.
 */
static void
gnc_props_connect_font_button (GtkFontButton *fb)
{
    gchar *path = NULL;

    g_return_if_fail (GTK_IS_FONT_BUTTON(fb));

//    gnc_props_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(fb)), &path);

    g_free (path);

    gtk_widget_show_all (GTK_WIDGET(fb));
}

/****************************************************************************/

/** Connect a GtkRadioButton widget to its properties value.
 *
 *  @internal
 *
 *  @param button A pointer to the radio button that should be
 *  connected.
 */
static void
gnc_props_connect_radio_button (GtkRadioButton *button)
{
    gchar *path = NULL;

    g_return_if_fail (GTK_IS_RADIO_BUTTON(button));

//    gnc_props_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(button)), &path);

    g_free (path);
}

/****************************************************************************/

/** Connect a GtkCheckButton widget to its properties value.
 *
 *  @internal
 *
 *  @param button A pointer to the check button that should be
 *  connected.
 */
static void
gnc_props_connect_check_button (GtkCheckButton *button)
{
    gchar *path;
    const gchar *text;
    gboolean active = FALSE;

    g_return_if_fail (GTK_IS_CHECK_BUTTON(button));

    gnc_props_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(button)), &path);

    text = qof_book_get_string_option (gnc_get_current_book(), path);

    if (text)
        g_object_set_data_full (G_OBJECT(button), "old-value", g_strdup (text), g_free);

    if (g_strcmp0 (text, "t") == 0)
        active = TRUE;

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button), active);

    g_free (path);
}

static void
gnc_props_save_check_button (GtkCheckButton *button)
{
    gchar *path;
    const gchar *old_value = g_object_get_data (G_OBJECT (button), "old-value");
    gboolean active = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(button));
    gchar *new_value = active ? "t" : "";

    gnc_props_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(button)), &path);

    if (g_strcmp0 (new_value, old_value) != 0)
        qof_book_set_string_option (gnc_get_current_book(), path, new_value);

    g_free (path);
}

/****************************************************************************/

/** Connect a GtkSpinButton widget to its properties value.
 *
 *  @internal
 *
 *  @param button A pointer to the spin button that should be
 *  connected, values used will be integers.
 */
static void
gnc_props_connect_spin_button (GtkSpinButton *spin)
{
    gchar *path;
    gint    val = 0;

    g_return_if_fail (GTK_IS_SPIN_BUTTON(spin));

    gnc_props_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(spin)), &path);

    val = qof_book_get_int64_option (gnc_get_current_book(), path);

    g_object_set_data (G_OBJECT (spin), "old-value", GINT_TO_POINTER (val));

    gtk_spin_button_set_value (spin, val);

    g_free (path);
}

static void
gnc_props_save_spin_button (GtkSpinButton *spin_button)
{
    gchar *path;
    gint new_value = (gint)gtk_spin_button_get_value (spin_button);
    gint old_value = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (spin_button), "old-value"));

    gnc_props_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(spin_button)), &path);

    if (new_value != old_value)
        qof_book_set_int64_option (gnc_get_current_book(), path, new_value);

    g_free (path);
}


/****************************************************************************/

/** Connect a GtkSpinButton widget to its properties value.
 *
 *  @internal
 *
 *  @param button A pointer to the spin button that should be
 *  connected, values used will be doubles.
 */
static void
gnc_props_connect_spin_button_double (GtkSpinButton *spin)
{
    gchar *path;
    gint val;

    g_return_if_fail (GTK_IS_SPIN_BUTTON(spin));

    gnc_props_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(spin)), &path);

    val = (gint)qof_book_get_double_option (gnc_get_current_book(), path);

    g_object_set_data (G_OBJECT (spin), "old-value", GINT_TO_POINTER (val));

    gtk_spin_button_set_value (spin, val);

    g_free (path);
}

static void
gnc_props_save_spin_button_double (GtkSpinButton *spin_button)
{
    gchar *path;
    gint new_value = (gint)gtk_spin_button_get_value (spin_button);
    gint old_value = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (spin_button), "old-value"));

    gnc_props_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(spin_button)), &path);

    if (new_value != old_value)
        qof_book_set_double_option (gnc_get_current_book(), path, new_value);

    g_free (path);
}

/****************************************************************************/

/** Connect a GtkTextView widget to its properties value.
 *
 *  @internal
 *
 *  @param button A pointer to the spin button that should be
 *  connected.
 */
static void
gnc_props_connect_text_view (GtkTextView *view)
{
    gchar *path;
    const gchar *text;

    g_return_if_fail (GTK_IS_TEXT_VIEW(view));

    gnc_props_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(view)), &path);

    // do not allow tabs
    gtk_text_view_set_accepts_tab (view, FALSE);

    text = qof_book_get_string_option (gnc_get_current_book(), path);

    if (text)
    {
        GtkTextIter iter;
        GtkTextBuffer *buffer = gtk_text_view_get_buffer (view);

        gtk_text_buffer_get_start_iter (buffer, &iter);
        gtk_text_buffer_insert (buffer, &iter, text, -1);

        g_object_set_data_full (G_OBJECT(view), "old-value", g_strdup (text), g_free);
    }
    g_free (path);
}

static void
gnc_props_save_text_view (GtkTextView *view)
{
    gchar       *path;
    gchar       *new_value;
    const gchar *old_value = g_object_get_data (G_OBJECT (view), "old-value");
    GtkTextIter siter, eiter;
    GtkTextBuffer *buffer;

    g_return_if_fail (GTK_IS_TEXT_VIEW(view));

    buffer = gtk_text_view_get_buffer (view);

    gnc_props_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(view)), &path);

    gtk_text_buffer_get_start_iter (buffer, &siter);
    gtk_text_buffer_get_end_iter (buffer, &eiter);

    new_value = gtk_text_buffer_get_text (buffer, &siter, &eiter, FALSE);

    if (g_strcmp0 (new_value, old_value) != 0)
        qof_book_set_string_option (gnc_get_current_book(), path, new_value);

    g_free (new_value);
    g_free (path);
}

/****************************************************************************/

/** Connect a GtkComboBox widget to its properties value.
 *
 *  @internal
 *
 *  @param cbox A pointer to the combo box that should be connected.
 */
static void
gnc_props_connect_box_combo_box (GtkComboBox *cbox, const gchar *boxname)
{
    gchar *path;
    const gchar *text;
    GncGUID *old_guid;
    gint guid_column;

    g_return_if_fail (GTK_IS_COMBO_BOX(cbox));

    gnc_props_split_widget_name (boxname, &path);

    old_guid = qof_book_get_guid_option (gnc_get_current_book(), path);

    guid_column = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cbox), "guid-column"));

    if (old_guid)
    {
        GtkTreeModel *model = gtk_combo_box_get_model (cbox);
        GtkTreeIter iter;

        g_object_set_data (G_OBJECT (cbox), "old-value", old_guid);

        if (gtk_tree_model_get_iter_first (model, &iter))
        {
            while (gtk_list_store_iter_is_valid (GTK_LIST_STORE(model), &iter))
            {
                GValue gv = { 0 };
                GncGUID *guid;

                gtk_tree_model_get_value (model, &iter, guid_column, &gv);
                guid = (GncGUID *) g_value_get_pointer (&gv);
                g_value_unset (&gv);

                if (guid_equal (old_guid, guid))
                    gtk_combo_box_set_active_iter (cbox, &iter);

                if (!gtk_tree_model_iter_next (model, &iter))
                    break;
            }
        }
    }
    g_free (path);
}

static void
gnc_props_save_box_combo_box (GtkComboBox *cbox, const gchar *boxname)
{
    gchar *path;
    const gchar *text;
    GncGUID *old_guid;
    gint guid_column;
    GtkTreeModel *model = gtk_combo_box_get_model (cbox);
    GtkTreeIter iter;

    g_return_if_fail (GTK_IS_COMBO_BOX(cbox));

    gnc_props_split_widget_name (boxname, &path);

    old_guid = g_object_get_data (G_OBJECT (cbox), "old-value");

    guid_column = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cbox), "guid-column"));

    if (gtk_combo_box_get_active_iter (cbox, &iter))
    {
        GValue gv = { 0 };
        GncGUID *guid;

        gtk_tree_model_get_value (model, &iter, guid_column, &gv);
        guid = (GncGUID *) g_value_get_pointer (&gv);
        g_value_unset (&gv);

        if (!guid_equal (old_guid, guid))
            qof_book_set_guid_option (gnc_get_current_book(), path, guid);
    }
    g_free (path);
}

/****************************************************************************/

/** Connect a GncCurrencyEdit widget to its properties value.
 *
 *  @internal
 *
 *  @param gce A pointer to the currency_edit that should be connected.
 */
static void
gnc_props_connect_box_currency_edit (GNCCurrencyEdit *gce, const gchar *boxname )
{
    gchar *path = NULL;

    g_return_if_fail (GNC_IS_CURRENCY_EDIT(gce));

//    gnc_props_split_widget_name (boxname, &path);

    g_free (path);

    gtk_widget_show_all (GTK_WIDGET(gce));
}

/****************************************************************************/

/** Connect a GtkEntry widget to its properties value.
 *
 *  @internal
 *
 *  @param entry A pointer to the entry that should be connected.
 */
static void
gnc_props_connect_entry (GtkEntry *entry)
{
    gchar       *path;
    const gchar *text;

    g_return_if_fail (GTK_IS_ENTRY(entry));

    gnc_props_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(entry)), &path);

    text = qof_book_get_string_option (gnc_get_current_book(), path);

    if (text)
    {
        g_object_set_data_full (G_OBJECT(entry), "old-value", g_strdup (text), g_free);
        gtk_entry_set_text (entry, text);
    }
    g_free (path);
}

static void
gnc_props_save_entry (GtkEntry *entry)
{
    gchar *path;
    const gchar *text = gtk_entry_get_text (entry);
    const gchar *old_value = g_object_get_data (G_OBJECT (entry), "old-value");

    gnc_props_split_widget_name (gtk_buildable_get_name(GTK_BUILDABLE(entry)), &path);

    if (g_strcmp0 (text, old_value) != 0)
        qof_book_set_string_option (gnc_get_current_book(), path, text);

    g_free (path);
}

/****************************************************************************/

/** Connect a GncPeriodSelect widget to its properties value.
 *
 *  @internal
 *
 *  @param period A pointer to the GncPeriodSelect that should be connected.
 */
static void
gnc_props_connect_box_period_select (GncPeriodSelect *period, const gchar *boxname )
{
    gchar *path = NULL;

    g_return_if_fail (GNC_IS_PERIOD_SELECT(period));

//    gnc_props_split_widget_name (boxname, &path);

    g_free (path);
}

/****************************************************************************/

/** Connect a GncDateEdit widget to its properties value.
 *
 *  @internal
 *
 *  @param gde A pointer to the date_edit that should be connected.
 */
static void
gnc_props_connect_box_date_edit (GNCDateEdit *gde , const gchar *boxname )
{
    gchar *path = NULL;

    g_return_if_fail (GNC_IS_DATE_EDIT(gde));

//    gnc_props_split_widget_name (boxname, &path);

    g_free (path);
}


/****************************************************************************/

/** Connect a GncDateFormat widget to its properties value.
 *
 *  @internal
 *
 *  @param gdf A pointer to the date_format that should be connected.
 */
static void
gnc_props_connect_box_date_format (GNCDateFormat *gdf , const gchar *boxname )
{
    gchar       *path;
    gchar       *new_path;
    const gchar *text;
    gint         years;

    g_return_if_fail (GNC_IS_DATE_FORMAT(gdf));

    gnc_props_split_widget_name (boxname, &path);

    new_path = g_strconcat (path, "/fmt", NULL);
    text = qof_book_get_string_option (gnc_get_current_book(), new_path);
    if (text)
    {
        QofDateFormat format;
        g_object_set_data_full (G_OBJECT(gdf), "old-value-fmt", g_strdup (text), g_free);
        gnc_date_string_to_dateformat (text, &format);
        gnc_date_format_set_format (gdf, format);
    }
    else
    {
        g_free (path);
        g_free (new_path);
        return;
    }
    g_free (new_path);

    new_path = g_strconcat (path, "/custom", NULL);
    text = qof_book_get_string_option (gnc_get_current_book(), new_path);
    if (text)
    {
        g_object_set_data_full (G_OBJECT(gdf), "old-value-custom", g_strdup (text), g_free);
        gnc_date_format_set_custom (gdf, text);
    }
    g_free (new_path);

    new_path = g_strconcat (path, "/month", NULL);
    text = qof_book_get_string_option (gnc_get_current_book(), new_path);
    if (text)
    {
        GNCDateMonthFormat format;
        g_object_set_data_full (G_OBJECT(gdf), "old-value-month", g_strdup (text), g_free);
        gnc_date_string_to_monthformat (text, &format);
        gnc_date_format_set_months (gdf, format);
    }
    g_free (new_path);

    new_path = g_strconcat (path, "/years", NULL);
    years = qof_book_get_int64_option (gnc_get_current_book(), new_path);
    if (years == 1)
    {
        g_object_set_data (G_OBJECT(gdf), "old-value-years", GINT_TO_POINTER(years));
        gnc_date_format_set_years (gdf, years);
    }
    g_free (new_path);
    g_free (path);
}

static void
gnc_props_save_box_date_format (GNCDateFormat *gdf , const gchar *boxname )
{
    gchar       *path;
    gchar       *new_path;
    const gchar *new_value;
    const gchar *old_value;
    QofDateFormat df;
    GNCDateMonthFormat mf;
    gboolean old_bvalue;
    gboolean new_bvalue;
    QofBook *book = gnc_get_current_book();

    g_return_if_fail (GNC_IS_DATE_FORMAT(gdf));

    gnc_props_split_widget_name (boxname, &path);

    df = gnc_date_format_get_format (gdf);
    new_value = gnc_date_dateformat_to_string (df);
    old_value = g_object_get_data (G_OBJECT (gdf), "old-value-fmt");

    if (old_value && (df == QOF_DATE_FORMAT_UNSET))
    {
        qof_book_option_frame_delete (book, path);
        g_free (path);
        return;
    }

    if (g_strcmp0 (old_value, new_value) != 0)
    {
        new_path = g_strconcat (path, "/fmt", NULL);
        qof_book_set_string_option (book, new_path, new_value);
        g_free (new_path);
    }

    new_value = gnc_date_format_get_custom (gdf);
    old_value = g_object_get_data (G_OBJECT (gdf), "old-value-custome");
    if (g_strcmp0 (old_value, new_value) != 0)
    {
        new_path = g_strconcat (path, "/custom", NULL);
        qof_book_set_string_option (book, new_path, new_value);
        g_free (new_path);
    }

    mf = gnc_date_format_get_months (gdf);
    new_value = gnc_date_monthformat_to_string (mf);
    old_value = g_object_get_data (G_OBJECT (gdf), "old-value-month");
    if (g_strcmp0 (old_value, new_value) != 0)
    {
        new_path = g_strconcat (path, "/month", NULL);
        qof_book_set_string_option (book, new_path, new_value);
        g_free (new_path);
    }

    new_bvalue = gnc_date_format_get_years (gdf);
    old_bvalue = GPOINTER_TO_INT(g_object_get_data (G_OBJECT (gdf), "old-value-years"));
    if (old_bvalue != new_bvalue)
    {
        new_path = g_strconcat (path, "/years", NULL);
        qof_book_set_int64_option (book, new_path, new_bvalue);
        g_free (new_path);
    }
    g_free (path);
}

/****************************************************************************/

/********************/
/*    Callbacks     */
/********************/


static void
gnc_props_save_one (const gchar *name, GtkWidget *widget, gpointer user_data)
{
    /* These tests must be ordered from more specific widget to less
     * specific widget. */

//    if (GTK_IS_FONT_BUTTON(widget))
//    {
//        DEBUG("  %s - font button", name);
//        gnc_props_save_font_button (GTK_FONT_BUTTON(widget));
//    }
//    else if (GTK_IS_FILE_CHOOSER_BUTTON(widget))
//    {
//        DEBUG("  %s - file chooser button", name);
//        gnc_props_save_file_chooser_button (GTK_FILE_CHOOSER_BUTTON(widget), NULL);
//    }
//    else if (GTK_IS_RADIO_BUTTON(widget))
//    {
//        DEBUG("  %s - radio button", name);
//        gnc_props_save_radio_button (GTK_RADIO_BUTTON(widget));
//    }
//    else if (GTK_IS_CHECK_BUTTON(widget))
    if (GTK_IS_CHECK_BUTTON(widget))
    {
        DEBUG("  %s - check button", name);
        gnc_props_save_check_button (GTK_CHECK_BUTTON(widget));
    }
    else if (GTK_IS_SPIN_BUTTON(widget))
    {
        DEBUG("  %s - spin button, widget name is %s", name, gtk_widget_get_name (widget));
        
        if (g_strcmp0 (gtk_widget_get_name (widget), "GtkSpinButton-double") == 0)
            gnc_props_save_spin_button_double (GTK_SPIN_BUTTON(widget));
        else
            gnc_props_save_spin_button (GTK_SPIN_BUTTON(widget));
    }
//    else if (GTK_IS_COMBO_BOX(widget))
//    {
//        DEBUG("  %s - combo box", name);
//        gnc_props_save_combo_box (GTK_COMBO_BOX(widget));
//    }
    else if (GTK_IS_ENTRY(widget))
    {
        DEBUG("  %s - entry", name);
        gnc_props_save_entry (GTK_ENTRY(widget));
    }
    else if (GTK_IS_TEXT_VIEW(widget))
    {
        DEBUG("  %s - text view", name);
        gnc_props_save_text_view (GTK_TEXT_VIEW(widget));
    }
    else if (GTK_IS_BOX(widget))
    {
        /* Test custom widgets are all children of a hbox */
        GtkWidget *widget_child;
        GList* child = gtk_container_get_children (GTK_CONTAINER(widget));
        widget_child = child->data;
        g_list_free (child);
        DEBUG("  %s - box", name);
        DEBUG("Box widget type is %s and name is %s", gtk_widget_get_name (GTK_WIDGET(widget_child)), name);
//        if (GNC_IS_CURRENCY_EDIT(widget_child))
//        {
//            DEBUG("  %s - currency_edit", name);
//            gnc_props_save_box_currency_edit (GNC_CURRENCY_EDIT(widget_child), name);
//        }
//        else if (GNC_IS_PERIOD_SELECT(widget_child))
//        {
//            DEBUG("  %s - period_select", name);
//            gnc_props_save_box_period_select (GNC_PERIOD_SELECT(widget_child), name);
//        }
//        else if (GNC_IS_DATE_EDIT(widget_child))
//       {
//            DEBUG("  %s - date_edit", name);
//            gnc_props_save_box_date_edit (GNC_DATE_EDIT(widget_child), name);
//        }
//        else if (GNC_DATE_FORMAT(widget_child))
        if (GNC_IS_DATE_FORMAT(widget_child))
        {
            DEBUG("  %s - date_edit", name);
            gnc_props_save_box_date_format (GNC_DATE_FORMAT(widget_child), name);
        }
        else if (GTK_IS_COMBO_BOX(widget_child))
        {
            DEBUG("  %s - combo box", name);
            gnc_props_save_box_combo_box (GTK_COMBO_BOX(widget_child), name);
        }
//        else if (GTK_FILE_CHOOSER_BUTTON(widget_child))
//        {
//            DEBUG("  %s - file chooser button", name);
//            gnc_props_connect_file_chooser_button (GTK_FILE_CHOOSER_BUTTON(widget_child), name);
//        }
    }
}



/** Handle a user click on one of the buttons at the bottom of the
 *  preference dialog.  Also handles delete_window events, which have
 *  conveniently converted to a response by GtkDialog.
 *
 *  @internal
 *
 *  @param dialog A pointer to the properties dialog.
 *
 *  @param response Indicates which button was pressed by the user.
 *  The only expected values are HELP, CLOSE, and DELETE_EVENT.
 *
 *  @param unused
 */
void
gnc_properties_response_cb (GtkDialog *dialog, gint response, GtkDialog *unused)
{
    GHashTable *props_table;

    switch (response)
    {
    case GTK_RESPONSE_HELP:
        gnc_gnome_help (HF_HELP, HL_BOOK_OPTIONS);
        break;

    case GTK_RESPONSE_APPLY:
        props_table = g_object_get_data (G_OBJECT(dialog), PROPS_WIDGET_HASH);
        g_hash_table_foreach (props_table, (GHFunc)gnc_props_save_one, dialog);
        break;

    case GTK_RESPONSE_OK:
    default:
        props_table = g_object_get_data (G_OBJECT(dialog), PROPS_WIDGET_HASH);
        g_hash_table_foreach (props_table, (GHFunc)gnc_props_save_one, dialog);

    case GTK_RESPONSE_CANCEL:
        gnc_properties_do_refresh ();
        gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(dialog));
        gnc_unregister_gui_component_by_data (DIALOG_PROPERTIES_CM_CLASS,
                                              dialog);

        gtk_widget_destroy (GTK_WIDGET(dialog));
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
gnc_props_connect_one (const gchar *name, GtkWidget *widget, gpointer user_data)
{
    /* These tests must be ordered from more specific widget to less
     * specific widget. */

//    if (GTK_IS_FONT_BUTTON(widget))
//    {
//        DEBUG("  %s - font button", name);
//        gnc_props_connect_font_button (GTK_FONT_BUTTON(widget));
//    }
//    else if (GTK_IS_FILE_CHOOSER_BUTTON(widget))
//    {
//        DEBUG("  %s - file chooser button", name);
//        gnc_props_connect_file_chooser_button (GTK_FILE_CHOOSER_BUTTON(widget), NULL);
//    }
//    else if (GTK_IS_RADIO_BUTTON(widget))
//    {
//        DEBUG("  %s - radio button", name);
//        gnc_props_connect_radio_button (GTK_RADIO_BUTTON(widget));
//    }
//    else if (GTK_IS_CHECK_BUTTON(widget))
    if (GTK_IS_CHECK_BUTTON(widget))
    {
        DEBUG("  %s - check button", name);
        gnc_props_connect_check_button (GTK_CHECK_BUTTON(widget));
    }
    else if (GTK_IS_SPIN_BUTTON(widget))
    {
        DEBUG("  %s - spin button, widget name is %s", name, gtk_widget_get_name (widget));
        
        if (g_strcmp0 (gtk_widget_get_name (widget), "GtkSpinButton-double") == 0)
            gnc_props_connect_spin_button_double (GTK_SPIN_BUTTON(widget));
        else
            gnc_props_connect_spin_button (GTK_SPIN_BUTTON(widget));
    }
//    else if (GTK_IS_COMBO_BOX(widget))
//    {
//        DEBUG("  %s - combo box", name);
//        gnc_props_connect_combo_box (GTK_COMBO_BOX(widget));
//    }
    else if (GTK_IS_ENTRY(widget))
    {
        DEBUG("  %s - entry", name);
        gnc_props_connect_entry (GTK_ENTRY(widget));
    }
    else if (GTK_IS_TEXT_VIEW(widget))
    {
        DEBUG("  %s - text view", name);
        gnc_props_connect_text_view (GTK_TEXT_VIEW(widget));
    }
    else if (GTK_IS_BOX(widget))
    {
        /* Test custom widgets are all children of a hbox */
        GtkWidget *widget_child;
        GList* child = gtk_container_get_children (GTK_CONTAINER(widget));
        widget_child = child->data;
        g_list_free (child);
        DEBUG("  %s - box", name);
        DEBUG("Box widget type is %s and name is %s", gtk_widget_get_name (GTK_WIDGET(widget_child)), name);
//        if (GNC_IS_CURRENCY_EDIT(widget_child))
//        {
//            DEBUG("  %s - currency_edit", name);
//            gnc_props_connect_box_currency_edit (GNC_CURRENCY_EDIT(widget_child), name);
//        }
//        else if (GNC_IS_PERIOD_SELECT(widget_child))
//        {
//            DEBUG("  %s - period_select", name);
//            gnc_props_connect_box_period_select (GNC_PERIOD_SELECT(widget_child), name);
//        }
//        else if (GNC_IS_DATE_EDIT(widget_child))
//       {
//            DEBUG("  %s - date_edit", name);
//            gnc_props_connect_box_date_edit (GNC_DATE_EDIT(widget_child), name);
//        }
//        else if (GNC_DATE_FORMAT(widget_child))
        if (GNC_IS_DATE_FORMAT(widget_child))
        {
            DEBUG("  %s - date_edit", name);
            gnc_props_connect_box_date_format (GNC_DATE_FORMAT(widget_child), name);
        }
        else if (GTK_IS_COMBO_BOX(widget_child))
        {
            DEBUG("  %s - combo box", name);
            gnc_props_connect_box_combo_box (GTK_COMBO_BOX(widget_child), name);
        }
//        else if (GTK_FILE_CHOOSER_BUTTON(widget_child))
//        {
//            DEBUG("  %s - file chooser button", name);
//            gnc_props_connect_file_chooser_button (GTK_FILE_CHOOSER_BUTTON(widget_child), name);
//        }
    }
    else
    {
        DEBUG("  %s - unsupported %s", name,
              G_OBJECT_TYPE_NAME(G_OBJECT(widget)));
    }
}


/** Create a combo widget for a list of budgets.
 *
 *  @internal
 */
static GtkWidget *
gnc_props_create_budget_combo_widget (void)
{
    GtkTreeModel *tm;
    GtkComboBox *cb;
    GtkCellRenderer *cr;

    tm = gnc_tree_model_budget_new (gnc_get_current_book());
    cb = GTK_COMBO_BOX (gtk_combo_box_new_with_model (tm));

    g_object_set_data (G_OBJECT (cb), "guid-column", GINT_TO_POINTER (BUDGET_GUID_COLUMN));

    g_object_unref (tm);
    cr = gtk_cell_renderer_text_new ();
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT(cb), cr, TRUE);

    gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT(cb), cr, "text",
                                    BUDGET_NAME_COLUMN, NULL);
    return GTK_WIDGET(cb);
}


/** Create a combo widget for a list of tax tables.
 *
 *  @internal
 */
static GtkWidget *
gnc_props_create_tax_combo_widget (void)
{
    GList *list, *node;
    GtkListStore *ls;
    GtkTreeIter iter;
    GtkComboBox *cb;
    GtkCellRenderer *cr;

    ls = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_POINTER);

    /* Add the items to the list */
    list = gncTaxTableGetTables (gnc_get_current_book());
    if (list)
        list = g_list_reverse (g_list_copy (list));

    for (node = list; node; node = node->next)
    {
        GncTaxTable *table = node->data;

        gtk_list_store_prepend (ls, &iter);
        gtk_list_store_set (ls, &iter,
                           0, gncTaxTableGetName (table),
                           1, gncTaxTableGetGUID (table),
                          -1);
    }

    cb = GTK_COMBO_BOX(gtk_combo_box_new_with_model (GTK_TREE_MODEL(ls)));

    g_object_set_data (G_OBJECT (cb), "guid-column", GINT_TO_POINTER (1));

    g_object_unref (GTK_TREE_MODEL(ls));
    cr = gtk_cell_renderer_text_new ();
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT(cb), cr, TRUE);

    gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT(cb), cr, "text",
                                    0, NULL);

    return GTK_WIDGET(cb);
}


/** Create the properties dialog.  This function first reads the
 *  dialog-properties.glade file to obtain the content and then
 *  the dialog is created with a set of common properties.  It then
 *  runs the list of add-ins, calling a helper function to add each full/partial
 *  page to this dialog, Finally it builds the "interesting widgets"
 *  table that is used for connecting the widgets up to callback functions.
 *
 *  @internal
 *
 *  @return A pointer to the newly created dialog.
 */
static GtkWidget *
gnc_properties_dialog_create (GtkWindow *parent)
{
    GtkBuilder *builder;
    GtkWidget *dialog, *notebook;
    GtkWidget *box, *combo, *fdate;
    GHashTable *props_table;
    QofBook *book = gnc_get_current_book();

    ENTER("");
    DEBUG("Opening dialog-properties.glade:");
    builder = gtk_builder_new();

    gnc_builder_add_from_file (builder, "dialog-properties.glade", "read_only_threshold_adj");
    gnc_builder_add_from_file (builder, "dialog-properties.glade", "textview_h_adj");
    gnc_builder_add_from_file (builder, "dialog-properties.glade", "textview_v_adj");
    gnc_builder_add_from_file (builder, "dialog-properties.glade", "textbuffer1");

    gnc_builder_add_from_file (builder, "dialog-properties.glade", "bill_adj");
    gnc_builder_add_from_file (builder, "dialog-properties.glade", "customer_adj");
    gnc_builder_add_from_file (builder, "dialog-properties.glade", "employee_adj");
    gnc_builder_add_from_file (builder, "dialog-properties.glade", "expense_adj");
    gnc_builder_add_from_file (builder, "dialog-properties.glade", "invoice_adj");
    gnc_builder_add_from_file (builder, "dialog-properties.glade", "job_adj");
    gnc_builder_add_from_file (builder, "dialog-properties.glade", "order_adj");
    gnc_builder_add_from_file (builder, "dialog-properties.glade", "vendor_adj");

    gnc_builder_add_from_file (builder, "dialog-properties.glade", "date_formats");
    gnc_builder_add_from_file (builder, "dialog-properties.glade", "gnucash_properties_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "gnucash_properties_dialog"));

    // Name the dialog widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(dialog), "gnc-id-properties-dialog");

    /* parent */
    gtk_window_set_transient_for (GTK_WINDOW(dialog), GTK_WINDOW(parent));

    /* Save some values so we can test for refresh */
    use_split_action_for_num_before = qof_book_use_split_action_for_num_field (book);
//    use_book_currency_before = gnc_book_use_book_currency (book);

    DEBUG("autoconnect");
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, dialog);
    DEBUG("done");

    notebook = GTK_WIDGET(gtk_builder_get_object (builder, "notebook1"));
    props_table = g_hash_table_new (g_str_hash, g_str_equal);
    g_object_set_data (G_OBJECT(dialog), NOTEBOOK, notebook);
    g_object_set_data_full (G_OBJECT(dialog), PROPS_WIDGET_HASH,
                            props_table, (GDestroyNotify)g_hash_table_destroy);

    // setup the budget combo
    box = GTK_WIDGET(gtk_builder_get_object (builder, "prop/options/Budgeting/Default Budget"));
    combo = gnc_props_create_budget_combo_widget ();
    gtk_box_pack_start (GTK_BOX (box), combo, TRUE, TRUE, 0);
    gtk_widget_show (combo);

    // setup the tax combo
    box = GTK_WIDGET(gtk_builder_get_object (builder, "prop/options/Business/Default Customer TaxTable"));
    combo = gnc_props_create_tax_combo_widget ();
    gtk_box_pack_start (GTK_BOX (box), combo, TRUE, TRUE, 0);
    gtk_widget_show (combo);

    // setup the tax combo
    box = GTK_WIDGET(gtk_builder_get_object (builder, "prop/options/Business/Default Vendor TaxTable"));
    combo = gnc_props_create_tax_combo_widget ();
    gtk_box_pack_start (GTK_BOX (box), combo, TRUE, TRUE, 0);
    gtk_widget_show (combo);

    // setup the fancy date widget
    box = GTK_WIDGET(gtk_builder_get_object (builder, "prop/options/Business/Fancy Date Format"));
    fdate = gnc_date_format_new_without_label ();
    gtk_box_pack_start (GTK_BOX (box), fdate, TRUE, TRUE, 0);
    gtk_widget_show (fdate);

    /* Add to the list of interesting widgets */
    gnc_props_build_widget_table (builder, dialog);

    g_slist_foreach (add_ins_props, gnc_properties_build_page, dialog);

    /* Sort tabs alphabetically */
    gnc_props_sort_pages (GTK_NOTEBOOK(notebook));
    gtk_notebook_set_current_page (GTK_NOTEBOOK(notebook), 0);

    DEBUG("We have the following interesting widgets:");
    g_hash_table_foreach (props_table, (GHFunc)gnc_props_connect_one, dialog);
    DEBUG("Done with interesting widgets.");

    g_object_unref (G_OBJECT(builder));

    LEAVE("dialog %p", dialog);
    return dialog;
}


/*************************************/
/*    Common callback code           */
/*************************************/


/** Raise the properties dialog to the top of the window stack.  This
 *  function is called if the user attempts to create a second
 *  properties dialog.
 *
 *  @internal
 *
 *  @param class_name Unused.
 *
 *  @param component_id Unused.
 *
 *  @param user_data A pointer to the properties dialog.
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


/** Close the properties dialog.
 *
 *  @internal
 *
 *  @param user_data A pointer to the properties dialog.
 */
static void
close_handler (gpointer user_data)
{
    GtkWidget *dialog;

    ENTER(" ");

    dialog = GTK_WIDGET(user_data);
    gnc_unregister_gui_component_by_data (DIALOG_PROPERTIES_CM_CLASS, dialog);
    gtk_widget_destroy (dialog);
    LEAVE(" ");
}


/*  This function creates the properties dialog and presents it to
 *  the user.  The properties dialog is a singleton, so if a
 *  properties dialog already exists it will be raised to the top of
 *  the window stack instead of creating a new dialog. */
void
gnc_properties_dialog (GtkWindow *parent)
{
    GtkWidget *dialog;

    ENTER("");
    if (gnc_forall_gui_components (DIALOG_PROPERTIES_CM_CLASS,
                                   show_handler, NULL))
    {
        LEAVE("existing window");
        return;
    }

    dialog = gnc_properties_dialog_create (parent);

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(dialog), parent);
    gtk_widget_show (dialog);

    gnc_register_gui_component (DIALOG_PROPERTIES_CM_CLASS,
                                NULL, close_handler, dialog);
    LEAVE(" ");
}

/** @} */
/** @} */
