/*
 * dialog-bi-import-gui.c -- Invoice Importer GUI
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
 * @file dialog-bi-import-gui.c
 * @brief GUI handling for bi-import plugin
 * @author Copyright (C) 2009 Sebastian Held <sebastian.held@gmx.de>
 * @author Mike Evans <mikee@saxicola.co.uk>
 * @author Rob Laan <rob.laan@chello.nl>
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
#include "dialog-bi-import.h"
#include "dialog-bi-import-gui.h"

struct _bi_import_gui
{
    GtkWindow    *parent;
    GtkWidget    *dialog;
    GtkWidget    *tree_view;
    GtkWidget    *entryFilename;
    GtkListStore *store;
    gint          component_id;
    GString      *regexp;
    QofBook      *book;
    gchar        *type;
    gchar        *open_mode;
};


// callback routines
void gnc_bi_import_gui_ok_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_cancel_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_help_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_destroy_cb (GtkWidget *widget, gpointer data);
static void gnc_bi_import_gui_close_handler (gpointer user_data);
void gnc_bi_import_gui_buttonOpen_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_filenameChanged_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_option1_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_option2_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_option3_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_option4_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_option5_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_open_mode_cb (GtkWidget *widget, gpointer data);
void gnc_import_gui_type_cb (GtkWidget *widget, gpointer data);

// utils
static gchar *gnc_input_dialog (GtkWidget *parent, const gchar *title, const gchar *msg, const gchar *default_input);
static void gnc_info2_dialog (GtkWidget *parent, const gchar *title, const gchar *msg);

#define UNUSED_VAR     __attribute__ ((unused))

static QofLogModule UNUSED_VAR log_module = G_LOG_DOMAIN; //G_LOG_BUSINESS;

BillImportGui *
gnc_plugin_bi_import_showGUI (GtkWindow *parent)
{
    BillImportGui *gui;
    GtkBuilder *builder;
    GList *glist;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    // if window exists already, activate it
    glist = gnc_find_gui_components ("dialog-bi-import-gui", NULL, NULL);
    if (glist)
    {
        // window found
        gui = g_list_nth_data (glist, 0);
        g_list_free (glist);

        gtk_window_set_transient_for(GTK_WINDOW(gui->dialog), GTK_WINDOW(parent));
        gui->parent = parent;
        gtk_window_present (GTK_WINDOW(gui->dialog));
        return gui;
    }

    // create new window
    gui = g_new0 (BillImportGui, 1);
    gui->type = "BILL"; // Set default type to match gui.  really shouldn't be here TODO change me
    gui->open_mode = "ALL";

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-bi-import-gui.glade", "bi_import_dialog");
    gui->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "bi_import_dialog"));
    gtk_window_set_transient_for(GTK_WINDOW(gui->dialog), GTK_WINDOW(parent));
    gui->parent = parent;
    gui->tree_view = GTK_WIDGET(gtk_builder_get_object (builder, "treeview1"));
    gui->entryFilename = GTK_WIDGET(gtk_builder_get_object (builder, "entryFilename"));

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(gui->dialog), "GncBillImportDialog");

    gtk_window_set_transient_for (GTK_WINDOW (gui->dialog), parent);

    gui->book = gnc_get_current_book();

    gui->regexp = g_string_new ( "^(\\x{FEFF})?(?<id>[^;]*);(?<date_opened>[^;]*);(?<owner_id>[^;]*);(?<billing_id>[^;]*);(?<notes>[^;]*);(?<date>[^;]*);(?<desc>[^;]*);(?<action>[^;]*);(?<account>[^;]*);(?<quantity>[^;]*);(?<price>[^;]*);(?<disc_type>[^;]*);(?<disc_how>[^;]*);(?<discount>[^;]*);(?<taxable>[^;]*);(?<taxincluded>[^;]*);(?<tax_table>[^;]*);(?<date_posted>[^;]*);(?<due_date>[^;]*);(?<account_posted>[^;]*);(?<memo_posted>[^;]*);(?<accu_splits>[^;]*)$");

    // create model and bind to view
    gui->store = gtk_list_store_new (N_COLUMNS,
                                     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, // invoice settings
                                     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, // entry settings
                                     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING); // autopost settings
    gtk_tree_view_set_model( GTK_TREE_VIEW(gui->tree_view), GTK_TREE_MODEL(gui->store) );
#define CREATE_COLUMN(description,column_id) \
  renderer = gtk_cell_renderer_text_new (); \
  column = gtk_tree_view_column_new_with_attributes (description, renderer, "text", column_id, NULL); \
  gtk_tree_view_column_set_resizable (column, TRUE); \
  gtk_tree_view_append_column (GTK_TREE_VIEW (gui->tree_view), column);
    CREATE_COLUMN (_("ID"), ID);
    CREATE_COLUMN (_("Date-opened"), DATE_OPENED);
    CREATE_COLUMN (_("Owner-ID"), OWNER_ID);
    CREATE_COLUMN (_("Billing-ID"), BILLING_ID);
    CREATE_COLUMN (_("Notes"), NOTES);

    CREATE_COLUMN (_("Date"), DATE);
    CREATE_COLUMN (_("Description"), DESC);
    CREATE_COLUMN (_("Action"), ACTION);
    CREATE_COLUMN (_("Account"), ACCOUNT);
    CREATE_COLUMN (_("Quantity"), QUANTITY);
    CREATE_COLUMN (_("Price"), PRICE);
    CREATE_COLUMN (_("Disc-type"), DISC_TYPE);
    CREATE_COLUMN (_("Disc-how"), DISC_HOW);
    CREATE_COLUMN (_("Discount"), DISCOUNT);
    CREATE_COLUMN (_("Taxable"), TAXABLE);
    CREATE_COLUMN (_("Taxincluded"), TAXINCLUDED);
    CREATE_COLUMN (_("Tax-table"), TAX_TABLE);

    CREATE_COLUMN (_("Date-posted"), DATE_POSTED);
    CREATE_COLUMN (_("Due-date"), DUE_DATE);
    CREATE_COLUMN (_("Account-posted"), ACCOUNT_POSTED);
    CREATE_COLUMN (_("Memo-posted"), MEMO_POSTED);
    CREATE_COLUMN (_("Accu-splits"), ACCU_SPLITS);

    gui->component_id = gnc_register_gui_component ("dialog-bi-import-gui",
                        NULL,
                        gnc_bi_import_gui_close_handler,
                        gui);

    /* Setup signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, gui);

    gtk_widget_show_all ( gui->dialog );

    g_object_unref(G_OBJECT(builder));

    return gui;
}

static gchar *
gnc_plugin_bi_import_getFilename(GtkWindow *parent)
{
    // prepare file import dialog
    gchar *filename = NULL;
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
    filename = gnc_file_dialog(parent, _("Import Bills or Invoices from csv"), filters, NULL, GNC_FILE_DIALOG_IMPORT);

    return filename;
}

void
gnc_bi_import_gui_ok_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    gchar *filename = g_strdup( gtk_entry_get_text( GTK_ENTRY(gui->entryFilename) ) );
    bi_import_stats stats;
    bi_import_result res;
    guint n_fixed, n_deleted, n_invoices_created, n_invoices_updated;
    GString *info;

    // import
    info = g_string_new("");

    gtk_list_store_clear (gui->store);
    res = gnc_bi_import_read_file (filename, gui->regexp->str, gui->store, 0, &stats);
    if (res == RESULT_OK)
    {
        gnc_bi_import_fix_bis (gui->store, &n_fixed, &n_deleted, info, gui->type);
        gnc_bi_import_create_bis (gui->store, gui->book, &n_invoices_created, &n_invoices_updated, &n_deleted,
                                  gui->type, gui->open_mode, info, gui->parent);
        if (info->len > 0)
            gnc_info_dialog (GTK_WINDOW (gui->dialog), "%s", info->str);
        g_string_free( info, TRUE );
        gnc_info_dialog (GTK_WINDOW (gui->dialog), _("Import:\n- rows ignored: %i\n- rows imported: %i\n\nValidation & processing:\n- rows fixed: %u\n- rows ignored: %u\n- invoices created: %u\n- invoices updated: %u"),
                         stats.n_ignored, stats.n_imported, n_fixed, n_deleted, n_invoices_created, n_invoices_updated);
        if (stats.n_ignored > 0)
            gnc_info2_dialog (gui->dialog, _("These lines were ignored during import"), stats.ignored_lines->str);

        g_string_free (stats.ignored_lines, TRUE);
        gnc_close_gui_component (gui->component_id);
    }
    else if (res ==  RESULT_OPEN_FAILED)
    {
        gnc_error_dialog (GTK_WINDOW (gui->dialog), _("The input file can not be opened."));
    }
    else if (res ==  RESULT_ERROR_IN_REGEXP)
    {
        //gnc_error_dialog (GTK_WINDOW (gui->dialog), "The regular expression is faulty:\n\n%s", stats.err->str);
    }
}

void
gnc_bi_import_gui_cancel_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;

    gnc_close_gui_component (gui->component_id);
}

void
gnc_bi_import_gui_help_cb (GtkWidget *widget, gpointer data)
{
    gnc_gnome_help(HF_HELP, HL_USAGE_BSNSS);
}

static void
gnc_bi_import_gui_close_handler (gpointer user_data)
{
    BillImportGui *gui = user_data;

    gtk_widget_destroy (gui->dialog);
    // gui has already been freed by this point.
    // gui->dialog = NULL;
}

void
gnc_bi_import_gui_destroy_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;

    gnc_suspend_gui_refresh ();
    gnc_unregister_gui_component (gui->component_id);
    gnc_resume_gui_refresh ();

    g_object_unref (gui->store);
    g_string_free (gui->regexp, TRUE);
    g_free (gui);
}

void gnc_bi_import_gui_buttonOpen_cb (GtkWidget *widget, gpointer data)
{
    gchar *filename = NULL;
    BillImportGui *gui = data;

    filename = gnc_plugin_bi_import_getFilename (gnc_ui_get_gtk_window (widget));
    if (filename)
    {
        //printf("Setting filename"); // debug
        gtk_entry_set_text( GTK_ENTRY(gui->entryFilename), filename );
        //printf("Set filename"); // debug
        g_free( filename );
    }
}

void gnc_bi_import_gui_filenameChanged_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    gchar *filename = g_strdup( gtk_entry_get_text( GTK_ENTRY(gui->entryFilename) ) );

    // generate preview
    gtk_list_store_clear (gui->store);
    gnc_bi_import_read_file (filename, gui->regexp->str, gui->store, 100, NULL);

    g_free( filename );
}

// Semicolon separated
void gnc_bi_import_gui_option1_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?(?<id>[^;]*);(?<date_opened>[^;]*);(?<owner_id>[^;]*);(?<billing_id>[^;]*);(?<notes>[^;]*);(?<date>[^;]*);(?<desc>[^;]*);(?<action>[^;]*);(?<account>[^;]*);(?<quantity>[^;]*);(?<price>[^;]*);(?<disc_type>[^;]*);(?<disc_how>[^;]*);(?<discount>[^;]*);(?<taxable>[^;]*);(?<taxincluded>[^;]*);(?<tax_table>[^;]*);(?<date_posted>[^;]*);(?<due_date>[^;]*);(?<account_posted>[^;]*);(?<memo_posted>[^;]*);(?<accu_splits>[^;]*)$");
    gnc_bi_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}

// Comma separated
void gnc_bi_import_gui_option2_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?(?<id>[^,]*),(?<date_opened>[^,]*),(?<owner_id>[^,]*),(?<billing_id>[^,]*),(?<notes>[^,]*),(?<date>[^,]*),(?<desc>[^,]*),(?<action>[^,]*),(?<account>[^,]*),(?<quantity>[^,]*),(?<price>[^,]*),(?<disc_type>[^,]*),(?<disc_how>[^,]*),(?<discount>[^,]*),(?<taxable>[^,]*),(?<taxincluded>[^,]*),(?<tax_table>[^,]*),(?<date_posted>[^,]*),(?<due_date>[^,]*),(?<account_posted>[^,]*),(?<memo_posted>[^,]*),(?<accu_splits>[^,]*)$");
    gnc_bi_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}

// Semicolon separated with quotes
void gnc_bi_import_gui_option3_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?((?<id>[^\";]*)|\"(?<id>[^\"]*)\");((?<date_opened>[^\";]*)|\"(?<date_opened>[^\"]*)\");((?<owner_id>[^\";]*)|\"(?<owner_id>[^\"]*)\");((?<billing_id>[^\";]*)|\"(?<billing_id>[^\"]*)\");((?<notes>[^\";]*)|\"(?<notes>([^\"]|\"\")*)\");((?<date>[^\";]*)|\"(?<date>[^\"]*)\");((?<desc>[^\";]*)|\"(?<desc>([^\"]|\"\")*)\");((?<action>[^\";]*)|\"(?<action>[^\"]*)\");((?<account>[^\";]*)|\"(?<account>[^\"]*)\");((?<quantity>[^\";]*)|\"(?<quantity>[^\"]*)\");((?<price>[^\";]*)|\"(?<price>[^\"]*)\");((?<disc_type>[^\";]*)|\"(?<disc_type>[^\"]*)\");((?<disc_how>[^\";]*)|\"(?<disc_how>[^\"]*)\");((?<discount>[^\";]*)|\"(?<discount>[^\"]*)\");((?<taxable>[^\";]*)|\"(?<taxable>[^\"]*)\");((?<taxincluded>[^\";]*)|\"(?<taxincluded>[^\"]*)\");((?<tax_table>[^\";]*)|\"(?<tax_table>[^\"]*)\");((?<date_posted>[^\";]*)|\"(?<date_posted>[^\"]*)\");((?<due_date>[^\";]*)|\"(?<due_date>[^\"]*)\");((?<account_posted>[^\";]*)|\"(?<account_posted>[^\"]*)\");((?<memo_posted>[^\";]*)|\"(?<memo_posted>[^\"]*)\");((?<accu_splits>[^\";]*)|\"(?<accu_splits>[^\"]*)\")$");
    gnc_bi_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}

// Comma separated with quote
void gnc_bi_import_gui_option4_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?((?<id>[^\",]*)|\"(?<id>[^\"]*)\"),((?<date_opened>[^\",]*)|\"(?<date_opened>[^\"]*)\"),((?<owner_id>[^\",]*)|\"(?<owner_id>[^\"]*)\"),((?<billing_id>[^\",]*)|\"(?<billing_id>[^\"]*)\"),((?<notes>[^\",]*)|\"(?<notes>([^\"]|\"\")*)\"),((?<date>[^\",]*)|\"(?<date>[^\"]*)\"),((?<desc>[^\",]*)|\"(?<desc>([^\"]|\"\")*)\"),((?<action>[^\",]*)|\"(?<action>[^\"]*)\"),((?<account>[^\",]*)|\"(?<account>[^\"]*)\"),((?<quantity>[^\",]*)|\"(?<quantity>[^\"]*)\"),((?<price>[^\",]*)|\"(?<price>[^\"]*)\"),((?<disc_type>[^\",]*)|\"(?<disc_type>[^\"]*)\"),((?<disc_how>[^\",]*)|\"(?<disc_how>[^\"]*)\"),((?<discount>[^\",]*)|\"(?<discount>[^\"]*)\"),((?<taxable>[^\",]*)|\"(?<taxable>[^\"]*)\"),((?<taxincluded>[^\",]*)|\"(?<taxincluded>[^\"]*)\"),((?<tax_table>[^\",]*)|\"(?<tax_table>[^\"]*)\"),((?<date_posted>[^\",]*)|\"(?<date_posted>[^\"]*)\"),((?<due_date>[^\",]*)|\"(?<due_date>[^\"]*)\"),((?<account_posted>[^\",]*)|\"(?<account_posted>[^\"]*)\"),((?<memo_posted>[^\",]*)|\"(?<memo_posted>[^\"]*)\"),((?<accu_splits>[^\",]*)|\"(?<accu_splits>[^\"]*)\")$");
    gnc_bi_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}

// DIY regex.
void gnc_bi_import_gui_option5_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    gchar *temp = NULL;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    temp = gnc_input_dialog (0, _("Adjust regular expression used for import"), _("This regular expression is used to parse the import file. Modify according to your needs.\n"), gui->regexp->str);
    if (temp)
    {
        g_string_assign (gui->regexp, temp);
        g_free (temp);
        gnc_bi_import_gui_filenameChanged_cb (gui->entryFilename, gui);
    }
}

void gnc_bi_import_gui_open_mode_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    const gchar *name = NULL;
    name = gtk_buildable_get_name(GTK_BUILDABLE(widget));
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    if  (g_ascii_strcasecmp(name, "radiobuttonOpenAll") == 0)gui->open_mode = "ALL";
    else if (g_ascii_strcasecmp(name, "radiobuttonOpenNotPosted") == 0)gui->open_mode = "NOT_POSTED";
    else if (g_ascii_strcasecmp(name, "radiobuttonOpenNone") == 0)gui->open_mode = "NONE";
}


/*****************************************************************
 * Set whether we are importing a bill, invoice, Customer or Vendor
 * ****************************************************************/
void gnc_import_gui_type_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    const gchar *name = NULL;
    name = gtk_buildable_get_name(GTK_BUILDABLE(widget));
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    if  (g_ascii_strcasecmp(name, "radiobuttonInvoice") == 0)gui->type = "INVOICE";
    else if (g_ascii_strcasecmp(name, "radiobuttonBill") == 0)gui->type = "BILL";
    //printf ("TYPE set to, %s\n",gui->type);

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
    gchar *user_input = NULL;
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
    gtk_box_pack_start(GTK_BOX(content_area), label, FALSE, FALSE, 0);

    // add a textview
    view = gtk_text_view_new ();
    gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (view), GTK_WRAP_WORD_CHAR);
    buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));
    gtk_text_buffer_set_text (buffer, default_input, -1);
    gtk_box_pack_start(GTK_BOX(content_area), view, TRUE, TRUE, 0);

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
    gtk_box_pack_start(GTK_BOX(content_area), scrolledwindow, TRUE, TRUE, 0);

    // add a textview
    view = gtk_text_view_new ();
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
