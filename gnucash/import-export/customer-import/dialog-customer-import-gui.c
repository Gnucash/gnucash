/*
 * dialog-customer-import-gui.c --
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

/**
 * @internal
 * @file gui.c
 * @brief GUI handling for customer import plugin
 * @author Copyright (C) 2009 Sebastian Held <sebastian.held@gmx.de>
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <glib/gi18n.h>

#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-component-manager.h"
#include "dialog-utils.h"
#include "gnc-gui-query.h"
#include "gnc-file.h"
#include "dialog-customer-import.h"
#include "dialog-customer-import-gui.h"

struct _customer_import_gui
{
    GtkWidget    *dialog;
    GtkWidget    *tree_view;
    GtkWidget    *entryFilename;
    GtkListStore *store;
    gint          component_id;
    GString      *regexp;
    gchar       *type;
    QofBook      *book;
};

// callback routines
void gnc_customer_import_gui_ok_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_cancel_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_help_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_destroy_cb (GtkWidget *widget, gpointer data);
static void gnc_customer_import_gui_close_handler (gpointer user_data);
void gnc_customer_import_gui_buttonOpen_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_filenameChanged_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_option1_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_option2_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_option3_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_option4_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_option5_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_type_cb (GtkWidget *widget, gpointer data);

// utils
static gchar *gnc_input_dialog (GtkWidget *parent, const gchar *title, const gchar *msg, const gchar *default_input);
static void gnc_info2_dialog (GtkWidget *parent, const gchar *title, const gchar *msg);


CustomerImportGui *
gnc_plugin_customer_import_showGUI(GtkWindow *parent)
{
    CustomerImportGui *gui;
    //gktbuilderXML *xml;
    GtkBuilder *builder;
    GList *glist;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    // if window exists already, activate it
    glist = gnc_find_gui_components ("dialog-customer-import-gui", NULL, NULL);
    if (glist)
    {
        // window found
        gui = g_list_nth_data (glist, 0);
        g_list_free (glist);
        gtk_window_present (GTK_WINDOW(gui->dialog));
        return gui;
    }

    // create new window
    gui = g_new0 (CustomerImportGui, 1);

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-customer-import-gui.glade", "customer_import_dialog");
    gui->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "customer_import_dialog"));
    gui->tree_view = GTK_WIDGET(gtk_builder_get_object (builder, "treeview1"));
    gui->entryFilename = GTK_WIDGET(gtk_builder_get_object (builder, "entryFilename"));
    gui->type = "CUSTOMER"; // Set a default type to import

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(gui->dialog), "GncCustomerImportDialog");

    gtk_window_set_transient_for (GTK_WINDOW (gui->dialog), parent);

    gui->regexp = g_string_new ( "^(?<id>[^;]+);(?<company>[^;]*);(?<name>[^;]+);(?<addr1>[^;]+);?(?<addr2>[^;]*);?(?<addr3>[^;]*);?(?<addr4>[^;]*);?(?<phone>[^;]*);?(?<fax>[^;]*);?(?<email>[^;]*);?(?<shipname>[^;]*);?(?<shipaddr1>[^;]*);?(?<shipaddr2>[^;]*);?(?<shipaddr3>[^;]*);?(?<shipaddr4>[^;]*);?(?<shipphone>[^;]*);?(?<shipfax>[^;]*);?(?<shipemail>[^;]*)");
    gui->book = gnc_get_current_book();

    // create model and bind to view
    gui->store = gtk_list_store_new (CI_N_COLUMNS,
                                     G_TYPE_STRING, G_TYPE_STRING,
                                     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
                                     G_TYPE_STRING,
                                     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
    gtk_tree_view_set_model( GTK_TREE_VIEW(gui->tree_view), GTK_TREE_MODEL(gui->store) );
#define CREATE_COLUMN(description,column_id) \
  renderer = gtk_cell_renderer_text_new (); \
  column = gtk_tree_view_column_new_with_attributes (description, renderer, "text", column_id, NULL); \
  gtk_tree_view_column_set_resizable (column, TRUE); \
  gtk_tree_view_append_column (GTK_TREE_VIEW (gui->tree_view), column);
    CREATE_COLUMN ("id", CI_ID);
    CREATE_COLUMN ("company", CI_COMPANY);
    CREATE_COLUMN ("name", CI_NAME);
    CREATE_COLUMN ("addr1", CI_ADDR1);
    CREATE_COLUMN ("addr2", CI_ADDR2);
    CREATE_COLUMN ("addr3", CI_ADDR3);
    CREATE_COLUMN ("addr4", CI_ADDR4);
    CREATE_COLUMN ("phone", CI_PHONE);
    CREATE_COLUMN ("fax", CI_FAX);
    CREATE_COLUMN ("email", CI_EMAIL);
    CREATE_COLUMN ("notes", CI_NOTES);
    CREATE_COLUMN ("shipname", CI_SHIPNAME);
    CREATE_COLUMN ("shipaddr1", CI_SHIPADDR1);
    CREATE_COLUMN ("shipaddr2", CI_SHIPADDR2);
    CREATE_COLUMN ("shipaddr3", CI_SHIPADDR3);
    CREATE_COLUMN ("shipaddr4", CI_SHIPADDR4);
    CREATE_COLUMN ("shipphone", CI_SHIPPHONE);
    CREATE_COLUMN ("shipfax", CI_SHIPFAX);
    CREATE_COLUMN ("shipemail", CI_SHIPEMAIL);

    gui->component_id = gnc_register_gui_component ("dialog-customer-import-gui",
                        NULL,
                        gnc_customer_import_gui_close_handler,
                        gui);

    /* Setup signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, gui);
    gtk_widget_show_all ( gui->dialog );
    return gui;
}

static gchar *
gnc_plugin_customer_import_getFilename (GtkWindow *parent)
{
    // prepare file import dialog
    gchar *filename;
    GList *filters;
    GtkFileFilter *filter;
    filters = NULL;
    filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (filter, "comma separated values (*.csv)");
    gtk_file_filter_add_pattern (filter, "*.csv");
    filters = g_list_append( filters, filter );
    filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (filter, "text files (*.txt)");
    gtk_file_filter_add_pattern (filter, "*.txt");
    filters = g_list_append( filters, filter );
    filename = gnc_file_dialog(parent,
                               _("Import Customers from csv"), filters, NULL, GNC_FILE_DIALOG_IMPORT);

    return filename;
}

void
gnc_customer_import_gui_ok_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    gchar *filename = g_strdup( gtk_entry_get_text( GTK_ENTRY(gui->entryFilename) ) );
    customer_import_stats stats;
    customer_import_result res;
    guint n_fixed, n_deleted, n_customers_created, n_customers_updated;
    gchar *cv_type_text;

    // import
    if (g_ascii_strcasecmp (gui->type, "CUSTOMER") == 0) cv_type_text = _("customers");
    else cv_type_text = _("vendors");

    gtk_list_store_clear (gui->store);
    res = gnc_customer_import_read_file (filename, gui->regexp->str, gui->store, 0, &stats);
    if (res == CI_RESULT_OK)
    {
        gnc_customer_import_fix_customers (gui->store, &n_fixed, &n_deleted, gui->type);
        gnc_customer_import_create_customers (gui->store, gui->book, &n_customers_created, &n_customers_updated, gui->type);
        gnc_info_dialog (GTK_WINDOW (gui->dialog), _("Import results:\n%i lines were ignored\n%i lines imported:\n   %u %s fixed\n   %u %s ignored (not fixable)\n\n   %u %s created\n   %u %s updated (based on id)"), \
                         stats.n_ignored, stats.n_imported, n_fixed, cv_type_text, n_deleted, cv_type_text, n_customers_created, cv_type_text, n_customers_updated, cv_type_text);

        if (stats.n_ignored > 0)
            gnc_info2_dialog (gui->dialog, _("These lines were ignored during import"), stats.ignored_lines->str);

        g_string_free (stats.ignored_lines, TRUE);
        gnc_close_gui_component (gui->component_id);
    }
    else if (res == CI_RESULT_OPEN_FAILED)
    {
        gnc_error_dialog (GTK_WINDOW (gui->dialog), _("The input file can not be opened."));
    }
    else if (res == CI_RESULT_ERROR_IN_REGEXP)
    {
        //gnc_error_dialog (GTK_WINDOW (gui->dialog), "The regular expression is faulty:\n\n%s", stats.err->str);
    }
}

void
gnc_customer_import_gui_cancel_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;

    gnc_close_gui_component (gui->component_id);
}

void
gnc_customer_import_gui_help_cb (GtkWidget *widget, gpointer data)
{
    gnc_gnome_help(HF_HELP, HL_USAGE_BSNSS);
}

static void
gnc_customer_import_gui_close_handler (gpointer user_data)
{
    CustomerImportGui *gui = user_data;

    gtk_widget_destroy (gui->dialog);
    // gui has already been freed by this point.
    // gui->dialog = NULL;
}

void
gnc_customer_import_gui_destroy_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;

    gnc_suspend_gui_refresh ();
    gnc_unregister_gui_component (gui->component_id);
    gnc_resume_gui_refresh ();

    g_object_unref (gui->store);
    g_string_free (gui->regexp, TRUE);
    g_free (gui);
}

void gnc_customer_import_gui_buttonOpen_cb (GtkWidget *widget, gpointer data)
{
    gchar *filename;
    CustomerImportGui *gui = data;

    filename = gnc_plugin_customer_import_getFilename (gnc_ui_get_gtk_window (widget));
    if (filename)
    {
        gtk_entry_set_text( GTK_ENTRY(gui->entryFilename), filename );
        g_free( filename );
    }
}

void gnc_customer_import_gui_filenameChanged_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    gchar *filename = g_strdup( gtk_entry_get_text( GTK_ENTRY(gui->entryFilename) ) );

    // generate preview
    gtk_list_store_clear (gui->store);
    gnc_customer_import_read_file (filename, gui->regexp->str, gui->store, 10, NULL);

    g_free( filename );
}
// Semicolon separated.
void gnc_customer_import_gui_option1_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(?<id>[^;]*);(?<company>[^;]*);(?<name>[^;]*);(?<addr1>[^;]*);?(?<addr2>[^;]*);?(?<addr3>[^;]*);?(?<addr4>[^;]*);?(?<phone>[^;]*);?(?<fax>[^;]*);?(?<email>[^;]*);?(?<notes>[^;]*);?(?<shipname>[^;]*);?(?<shipaddr1>[^;]*);?(?<shipaddr2>[^;]*);?(?<shipaddr3>[^;]*);?(?<shipaddr4>[^;]*);?(?<shipphone>[^;]*);(?<shipfax>[^;]*);(?<shipemail>[^;]*)"); //;(?<account_posted>[^;]*);(?<memo_posted>[^;]*);(?<accu_splits>[^;]*)$");
    gnc_customer_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}
// Comma separated.
void gnc_customer_import_gui_option2_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(?<id>[^,]*),(?<company>[^,]*),(?<name>[^,]*),(?<addr1>[^,]*),?(?<addr2>[^,]*),?(?<addr3>[^,]*),?(?<addr4>[^,]*),?(?<phone>[^,]*),?(?<fax>[^,]*),?(?<email>[^,]*),?(?<notes>[^,]*),?(?<shipname>[^,]*),?(?<shipaddr1>[^,]*),?(?<shipaddr2>[^,]*),?(?<shipaddr3>[^,]*),?(?<shipaddr4>[^,]*),?(?<shipphone>[^,]*),(?<shipfax>[^,]*),(?<shipemail>[^,]*)"); //,(?<account_posted>[^,]*),(?<memo_posted>[^,]*),(?<accu_splits>[^,]*)$");
    gnc_customer_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}
// Semicolon separated with quoted strings.
void gnc_customer_import_gui_option3_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    //g_string_assign (gui->regexp, "^((?<id>[^\";]*)|\"(?<id>[^\"]*)\");((?<company>[^\";]*)|\"(?<company>[^\"]*)\");((?<name>[^\";]*)|\"(?<name>[^\"]*)\");((?<addr1>[^\";]*)|\"(?<addr1>[^\"]*)\");((?<addr2>[^\";]*)|\"(?<addr2>[^\"]*)\");((?<addr3>[^\";]*)|\"(?<addr3>[^\"]*)\");((?<addr4>[^\";]*)|\"(?<addr4>[^\"]*)\");((?<phone>[^\";]*)|\"(?<phone>[^\"]*)\");((?<fax>[^\";]*)|\"(?<fax>[^\"]*)\");((?<email>[^\";]*)|\"(?<email>[^\"]*)\");((?<notes>[^\";]*)|\"(?<notes>[^\"]*)\");((?<shipname>[^\";]*)|\"(?<shipname>[^\"]*)\");((?<shipaddr1>[^\";]*)|\"(?<shipaddr1>[^\"]*)\");((?<shipaddr2>[^\";]*)|\"(?<shipaddr2>[^\"]*)\");((?<shipaddr3>[^\";]*)|\"(?<shipaddr3>[^\"]*)\");((?<shipaddr4>[^\";]*)|\"(?<shipaddr4>[^\"]*)\");((?<shipphone>[^\";]*)|\"(?<shipphone>[^\"]*)\");((?<shipfax>[^\";]*)|\"(?<shipfax>[^\"]*)\");((?<shipmail>[^\";]*)|\"(?<shipemail>[^\"]*)\")$");
    g_string_assign (gui->regexp, "^((?<id>[^\";]*)|\"(?<id>[^\"]*)\");((?<company>[^\";]*)|\"(?<company>[^\"]*)\");((?<name>[^\";]*)|\"(?<name>[^\"]*)\");((?<addr1>[^\";]*)|\"(?<addr1>[^\"]*)\");((?<addr2>[^\";]*)|\"(?<addr2>[^\"]*)\");((?<addr3>[^\";]*)|\"(?<addr3>[^\"]*)\");((?<addr4>[^\";]*)|\"(?<addr4>[^\"]*)\");((?<phone>[^\";]*)|\"(?<phone>[^\"]*)\");((?<fax>[^\";]*)|\"(?<fax>[^\"]*)\");((?<email>[^\";]*)|\"(?<email>[^\"]*)\");((?<notes>[^\";]*)|\"(?<notes>[^\"]*)\");((?<shipname>[^\";]*)|\"(?<shipname>[^\"]*)\");((?<shipaddr1>[^\";]*)|\"(?<shipaddr1>[^\"]*)\");((?<shipaddr2>[^\";]*)|\"(?<shipaddr2>[^\"]*)\");((?<shipaddr3>[^\";]*)|\"(?<shipaddr3>[^\"]*)\");((?<shipaddr4>[^\";]*)|\"(?<shipaddr4>[^\"]*)\");((?<shipphone>[^\";]*)|\"(?<shipphone>[^\"]*)\");((?<shipfax>[^\";]*)|\"(?<shipfax>[^\"]*)\");((?<shipemail>[^\";]*)|\"(?<shipemail>[^\"]*)\")$");
    gnc_customer_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}
// Comma separated with quoted strings.
void gnc_customer_import_gui_option4_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    //g_string_assign (gui->regexp, "^(?<id>[^,]*),(?<company>[^,]*),(?<name>[^,]*),(?<addr1>[^,]*),?(?<addr2>[^,]*),?(?<addr3>[^,]*),?(?<addr4>[^,]*),?(?<phone>[^,]*),?(?<fax>[^,]*),?(?<email>[^,]*),?(?<notes>[^,]*),?(?<shipname>[^,]*),?(?<shipaddr1>[^,]*),?(?<shipaddr2>[^,]*),?(?<shipaddr3>[^,]*),?(?<shipaddr4>[^,]*),?(?<shipphone>[^,]*),(?<shipfax>[^,]*),(?<shipemail>[^,]*)"); //,(?<account_posted>[^,]*),(?<memo_posted>[^,]*),(?<accu_splits>[^,]*)$");
    g_string_assign (gui->regexp, "^((?<id>[^\",]*)|\"(?<id>[^\"]*)\"),((?<company>[^\",]*)|\"(?<company>[^\"]*)\"),((?<name>[^\",]*)|\"(?<name>[^\"]*)\"),((?<addr1>[^\",]*)|\"(?<addr1>[^\"]*)\"),((?<addr2>[^\",]*)|\"(?<addr2>[^\"]*)\"),((?<addr3>[^\",]*)|\"(?<addr3>[^\"]*)\"),((?<addr4>[^\",]*)|\"(?<addr4>[^\"]*)\"),((?<phone>[^\",]*)|\"(?<phone>[^\"]*)\"),((?<fax>[^\",]*)|\"(?<fax>[^\"]*)\"),((?<email>[^\",]*)|\"(?<email>[^\"]*)\"),((?<notes>[^\",]*)|\"(?<notes>[^\"]*)\"),((?<shipname>[^\",]*)|\"(?<shipname>[^\"]*)\"),((?<shipaddr1>[^\",]*)|\"(?<shipaddr1>[^\"]*)\"),((?<shipaddr2>[^\",]*)|\"(?<shipaddr2>[^\"]*)\"),((?<shipaddr3>[^\",]*)|\"(?<shipaddr3>[^\"]*)\"),((?<shipaddr4>[^\",]*)|\"(?<shipaddr4>[^\"]*)\"),((?<shipphone>[^\",]*)|\"(?<shipphone>[^\"]*)\"),((?<shipfax>[^\",]*)|\"(?<shipfax>[^\"]*)\"),((?<shipemail>[^\",]*)|\"(?<shipemail>[^\"]*)\")$");
    gnc_customer_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}
void gnc_customer_import_gui_option5_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    gchar *temp;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    temp = gnc_input_dialog (0, _("Adjust regular expression used for import"), _("This regular expression is used to parse the import file. Modify according to your needs.\n"), gui->regexp->str);
    if (temp)
    {
        g_string_assign (gui->regexp, temp);
        g_free (temp);
        gnc_customer_import_gui_filenameChanged_cb (gui->entryFilename, gui);
    }
}




/*****************************************************************
 * Set whether we are importing a Customer or Vendor
 * ****************************************************************/
void gnc_customer_import_gui_type_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    const gchar *name;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    name = gtk_buildable_get_name(GTK_BUILDABLE(widget));
    if (name)
    {
        if  (g_ascii_strcasecmp(name, "radiobutton_customer") == 0)gui->type = "CUSTOMER";
        else if (g_ascii_strcasecmp(name, "radiobutton_vendor") == 0)gui->type = "VENDOR";
    }
    //printf ("TYPE set to, %s\n",gui->type); // DEBUG

}



/********************************************************************\
 * gnc_input_dialog                                                 *
 *   simple convenience dialog to get a single value from the user  *
 *   user may choose between "Ok" and "Cancel"                      *
 *                                                                  *
 * NOTE: This function does not return until the dialog is closed   *
 *                                                                  *
 * Args:   parent  - the parent window or NULL                      *
 *         title   - the title of the dialog                        *
 *         msg     - the message to display                         *
 *         default_input - will be displayed as default input       *
 * Return: the input (text) the user entered, if pressed "Ok"       *
 *         NULL, if pressed "Cancel"                                *
\********************************************************************/
static gchar *
gnc_input_dialog (GtkWidget *parent, const gchar *title, const gchar *msg, const gchar *default_input)
{
    GtkWidget *dialog, *label, *content_area;
    gint result;
    GtkWidget *view;
    GtkTextBuffer *buffer;
    gchar *user_input;
    GtkTextIter start, end;

    /* Create the widgets */
    dialog = gtk_dialog_new_with_buttons (title, GTK_WINDOW (parent),
                                          GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                          _("_OK"), GTK_RESPONSE_ACCEPT,
                                          _("_Cancel"), GTK_RESPONSE_REJECT,
                                          NULL);
    content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));

    // add a label
    label = gtk_label_new (msg);
    gtk_container_add (GTK_CONTAINER (content_area), label);

    // add a textview
    view = gtk_text_view_new ();
    gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (view), GTK_WRAP_WORD_CHAR);
    buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));
    gtk_text_buffer_set_text (buffer, default_input, -1);
    gtk_container_add (GTK_CONTAINER (content_area), view);

    // run the dialog
    gtk_widget_show_all (dialog);
    result = gtk_dialog_run (GTK_DIALOG (dialog));

    if (result == GTK_RESPONSE_REJECT)
        user_input = 0;
    else
    {
        gtk_text_buffer_get_start_iter (buffer, &start);
        gtk_text_buffer_get_end_iter (buffer, &end);
        user_input = gtk_text_buffer_get_text (buffer,
                                               &start, &end, FALSE);
    }

    gtk_widget_destroy (dialog);

    return user_input;
}

/********************************************************************\
 * gnc_info2_dialog                                                 *
 *   displays an information dialog box (with scrollable text area) *
 *                                                                  *
 * NOTE: This function does not return until the dialog is closed   *
 *                                                                  *
 * Args:   parent  - the parent window or NULL                      *
 *         title   - the title of the dialog                        *
 *         msg     - the message to display                         *
 * Return: none                                                     *
\********************************************************************/
static void
gnc_info2_dialog (GtkWidget *parent, const gchar *title, const gchar *msg)
{
    GtkWidget *dialog, *scrolledwindow, *content_area;
    GtkWidget *view;
    GtkTextBuffer *buffer;
    gint width, height;

    /* Create the widgets */
    dialog = gtk_dialog_new_with_buttons (title, GTK_WINDOW (parent),
                                          GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                          _("_OK"), GTK_RESPONSE_ACCEPT,
                                          NULL);
    content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));

    // add a scroll area
    scrolledwindow = gtk_scrolled_window_new (NULL, NULL);
    gtk_container_add (GTK_CONTAINER (content_area), scrolledwindow);

    // add a textview
    view = gtk_text_view_new ();
//    gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (view), GTK_WRAP_WORD_CHAR);
    gtk_text_view_set_editable (GTK_TEXT_VIEW (view), FALSE);
    buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));
    gtk_text_buffer_set_text (buffer, msg, -1);
    gtk_container_add (GTK_CONTAINER (scrolledwindow), view);

    // run the dialog
    if (parent)
    {
        gtk_window_get_size (GTK_WINDOW(parent), &width, &height);
        gtk_window_set_default_size (GTK_WINDOW(dialog), width, height);
    }
    gtk_widget_show_all (dialog);
    gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);
}
