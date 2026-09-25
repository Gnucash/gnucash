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
#include "gnc-session.h"
#include "dialog-utils.h"
#include "qof.h"
#include "gnc-gui-query.h"
#include "gnc-file.h"
#include "dialog-customer-import.h"
#include "dialog-customer-import-gui.h"

struct _customer_import_gui
{
    GtkWidget    *dialog;
    GtkColumnView *preview_view;
    GtkWidget    *entryFilename;
    GListStore   *store;
    gint          component_id;
    GString      *regexp;
    gchar       *type;
    QofBook      *book;
};

typedef struct
{
    GWeakRef dialog;
} CustomerImportFileDialogData;

static void
customer_import_file_dialog_data_free (CustomerImportFileDialogData *data)
{
    g_weak_ref_clear (&data->dialog);
    g_free (data);
}

void gnc_customer_import_gui_filenameChanged_cb (GtkWidget *widget, gpointer data);

typedef struct
{
    GWeakRef dialog;
    QofBook *book;
} CustomerImportRegexRequest;

static CustomerImportRegexRequest *
customer_import_regex_request_new (CustomerImportGui *gui)
{
    CustomerImportRegexRequest *request = g_new0 (CustomerImportRegexRequest, 1);

    g_weak_ref_init (&request->dialog, gui->dialog);
    request->book = gui->book;
    return request;
}

static void
customer_import_regex_request_free (CustomerImportRegexRequest *request)
{
    g_weak_ref_clear (&request->dialog);
    g_free (request);
}

static void
customer_import_regex_input_finished (gchar *input, gpointer user_data)
{
    CustomerImportRegexRequest *request = user_data;
    GtkWidget *dialog = g_weak_ref_get (&request->dialog);
    CustomerImportGui *gui = NULL;

    if (dialog)
        gui = g_object_get_data (G_OBJECT (dialog), "gnc-customer-import-gui");
    if (input && gui && gui->book == request->book && request->book == gnc_get_current_book () &&
        !qof_book_shutting_down (request->book))
    {
        g_string_assign (gui->regexp, input);
        gnc_customer_import_gui_filenameChanged_cb (gui->entryFilename, gui);
    }

    g_free (input);
    g_clear_object (&dialog);
    customer_import_regex_request_free (request);
}

// callback routines
void gnc_customer_import_gui_ok_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_cancel_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_help_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_destroy_cb (GtkWidget *widget, gpointer data);
static void gnc_customer_import_gui_request_close (CustomerImportGui *gui);
static gboolean gnc_customer_import_gui_close_request_cb (GtkWindow *window,
                                                          gpointer user_data);
static gboolean gnc_customer_import_gui_key_pressed_cb (GtkEventControllerKey *key,
                                                         guint keyval, guint keycode,
                                                         GdkModifierType state,
                                                         gpointer user_data);
static void gnc_customer_import_gui_close_handler (gpointer user_data);
void gnc_customer_import_gui_buttonOpen_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_filenameChanged_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_option1_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_option2_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_option3_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_option4_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_option5_cb (GtkWidget *widget, gpointer data);
void gnc_customer_import_gui_type_cb (GtkWidget *widget, gpointer data);

static void
customer_import_preview_item_setup (GtkListItemFactory *factory, GtkListItem *item,
                                    gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);

    (void)factory;
    (void)user_data;
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (item, label);
}

static void
customer_import_preview_item_bind (GtkListItemFactory *factory, GtkListItem *item,
                                   gpointer user_data)
{
    GObject *row = gtk_list_item_get_item (item);

    (void)factory;
    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (item)),
                        gnc_customer_import_row_get (row, GPOINTER_TO_UINT (user_data)));
}

static void
customer_import_preview_add_column (GtkColumnView *view, const gchar *title, guint column)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkColumnViewColumn *view_column;

    g_signal_connect (factory, "setup", G_CALLBACK (customer_import_preview_item_setup),
                      GUINT_TO_POINTER (column));
    g_signal_connect (factory, "bind", G_CALLBACK (customer_import_preview_item_bind),
                      GUINT_TO_POINTER (column));
    view_column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_append_column (view, view_column);
    g_object_unref (view_column);
}

CustomerImportGui *
gnc_plugin_customer_import_showGUI(GtkWindow *parent)
{
    CustomerImportGui *gui;
    //gktbuilderXML *xml;
    GtkBuilder *builder;
    GList *glist;
    GtkNoSelection *selection;
    GtkScrolledWindow *preview_scrolledwindow;

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
    gui->component_id = NO_COMPONENT;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-customer-import-gui.glade", "customer_import_dialog");
    gui->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "customer_import_dialog"));
    g_object_set_data (G_OBJECT (gui->dialog), "gnc-customer-import-gui", gui);
    gui->entryFilename = GTK_WIDGET(gtk_builder_get_object (builder, "entryFilename"));
    preview_scrolledwindow = GTK_SCROLLED_WINDOW (gtk_builder_get_object (builder,
                                                   "scrolledwindow2"));
    gui->type = "CUSTOMER"; // Set a default type to import

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(gui->dialog), "gnc-id-customer-import");
    gnc_widget_style_context_add_class (GTK_WIDGET(gui->dialog), "gnc-class-imports");

    gtk_window_set_transient_for (GTK_WINDOW (gui->dialog), parent);

    gui->regexp = g_string_new ( "^(\\x{FEFF})?(?<id>[^;]*);(?<company>[^;]*);(?<name>[^;]*);(?<addr1>[^;]*);(?<addr2>[^;]*);(?<addr3>[^;]*);(?<addr4>[^;]*);(?<phone>[^;]*);(?<fax>[^;]*);(?<email>[^;]*);(?<notes>[^;]*);(?<shipname>[^;]*);(?<shipaddr1>[^;]*);(?<shipaddr2>[^;]*);(?<shipaddr3>[^;]*);(?<shipaddr4>[^;]*);(?<shipphone>[^;]*);(?<shipfax>[^;]*);(?<shipemail>[^;]*)$");
    gui->book = gnc_get_current_book();

    /* The GTK4 preview uses the same row model as parsing and importing. */
    gui->store = g_list_store_new (G_TYPE_OBJECT);
    selection = gtk_no_selection_new (G_LIST_MODEL (g_object_ref (gui->store)));
    gui->preview_view = GTK_COLUMN_VIEW (gtk_column_view_new (GTK_SELECTION_MODEL (selection)));
    customer_import_preview_add_column (gui->preview_view, _("ID"), CI_ID);
    customer_import_preview_add_column (gui->preview_view, _("Company"), CI_COMPANY);
    customer_import_preview_add_column (gui->preview_view, _("Name"), CI_NAME);
    customer_import_preview_add_column (gui->preview_view, _("Address 1"), CI_ADDR1);
    customer_import_preview_add_column (gui->preview_view, _("Address 2"), CI_ADDR2);
    customer_import_preview_add_column (gui->preview_view, _("Address 3"), CI_ADDR3);
    customer_import_preview_add_column (gui->preview_view, _("Address 4"), CI_ADDR4);
    customer_import_preview_add_column (gui->preview_view, _("Phone"), CI_PHONE);
    customer_import_preview_add_column (gui->preview_view, _("Fax"), CI_FAX);
    customer_import_preview_add_column (gui->preview_view, _("Email"), CI_EMAIL);
    customer_import_preview_add_column (gui->preview_view, _("Notes"), CI_NOTES);
    customer_import_preview_add_column (gui->preview_view, _("Shipping Name"), CI_SHIPNAME);
    customer_import_preview_add_column (gui->preview_view, _("Shipping Address 1"), CI_SHIPADDR1);
    customer_import_preview_add_column (gui->preview_view, _("Shipping Address 2"), CI_SHIPADDR2);
    customer_import_preview_add_column (gui->preview_view, _("Shipping Address 3"), CI_SHIPADDR3);
    customer_import_preview_add_column (gui->preview_view, _("Shipping Address 4"), CI_SHIPADDR4);
    customer_import_preview_add_column (gui->preview_view, _("Shipping Phone"), CI_SHIPPHONE);
    customer_import_preview_add_column (gui->preview_view, _("Shipping Fax"), CI_SHIPFAX);
    customer_import_preview_add_column (gui->preview_view, _("Shipping Email"), CI_SHIPEMAIL);
    gtk_scrolled_window_set_child (preview_scrolledwindow, GTK_WIDGET (gui->preview_view));

    gui->component_id = gnc_register_gui_component ("dialog-customer-import-gui",
                        NULL,
                        gnc_customer_import_gui_close_handler,
                        gui);

    /* Setup Builder callbacks and GTK4 window lifecycle. */
    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, gui);
    g_signal_connect (gui->dialog, "close-request",
                      G_CALLBACK (gnc_customer_import_gui_close_request_cb), gui);

    GtkEventController *key_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (gui->dialog, key_controller);
    g_signal_connect (key_controller, "key-pressed",
                      G_CALLBACK (gnc_customer_import_gui_key_pressed_cb), gui);
    gtk_window_set_default_widget (
        GTK_WINDOW (gui->dialog),
        GTK_WIDGET (gtk_builder_get_object (builder, "okbutton")));

    g_object_unref (G_OBJECT (builder));
    gnc_gui_component_set_session (gui->component_id, gnc_get_current_session ());
    gtk_window_present (GTK_WINDOW (gui->dialog));
    return gui;
}

static GList *
customer_import_file_filters (void)
{
    GList *filters = NULL;
    GtkFileFilter *filter;

    filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (filter, "comma separated values (*.csv)");
    gtk_file_filter_add_pattern (filter, "*.csv");
    filters = g_list_append (filters, filter);
    filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (filter, "text files (*.txt)");
    gtk_file_filter_add_pattern (filter, "*.txt");
    return g_list_append (filters, filter);
}

static void
customer_import_file_dialog_finished (GObject *source, GAsyncResult *result,
                                      gpointer user_data)
{
    CustomerImportFileDialogData *data = user_data;
    GncFileDialogRequest *request = GNC_FILE_DIALOG_REQUEST (source);
    GError *error = NULL;
    GFile *file;
    GtkWidget *dialog;
    CustomerImportGui *gui = NULL;

    file = gnc_file_dialog_request_finish (request, result, &error);
    dialog = g_weak_ref_get (&data->dialog);
    if (dialog)
        gui = g_object_get_data (G_OBJECT (dialog),
                                 "gnc-customer-import-gui");

    if (file)
    {
        gchar *filename = g_file_get_path (file);

        if (gui && filename)
            gnc_entry_set_text (GTK_ENTRY (gui->entryFilename), filename);
        else if (gui)
            gnc_error_dialog (GTK_WINDOW (dialog), "%s",
                              _("The selected file has no local path."));
        g_free (filename);
        g_object_unref (file);
    }
    else if (gui && error &&
             !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
        gnc_error_dialog (GTK_WINDOW (dialog), "%s", error->message);

    g_clear_error (&error);
    g_clear_object (&dialog);
    customer_import_file_dialog_data_free (data);
}

void
gnc_customer_import_gui_ok_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    gchar *filename = g_strdup( gnc_entry_get_text( GTK_ENTRY(gui->entryFilename) ) );
    customer_import_stats stats;
    customer_import_result res;
    guint n_fixed, n_deleted, n_customers_created, n_customers_updated;
    gchar *cv_type_text;

    // import
    if (g_ascii_strcasecmp (gui->type, "CUSTOMER") == 0) cv_type_text = _("customers");
    else cv_type_text = _("vendors");

    g_list_store_remove_all (gui->store);
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
gnc_customer_import_gui_cancel_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer data)
{
    gnc_customer_import_gui_request_close (data);
}

void
gnc_customer_import_gui_help_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    gnc_gnome_help (GTK_WINDOW(gui->dialog), DF_GUIDE, DL_IMPORT_CUST);
}

static void
gnc_customer_import_gui_request_close (CustomerImportGui *gui)
{
    if (!gui)
        return;

    if (gui->component_id != NO_COMPONENT)
        gnc_close_gui_component (gui->component_id);
    else if (gui->dialog)
        gtk_window_destroy (GTK_WINDOW (gui->dialog));
}

static gboolean
gnc_customer_import_gui_close_request_cb (GtkWindow *window, gpointer user_data)
{
    CustomerImportGui *gui = user_data;

    if (!gui || gui->dialog != GTK_WIDGET (window))
        return FALSE;

    gnc_customer_import_gui_request_close (gui);
    return TRUE;
}

static gboolean
gnc_customer_import_gui_key_pressed_cb (G_GNUC_UNUSED GtkEventControllerKey *key,
                                         guint keyval, G_GNUC_UNUSED guint keycode,
                                         G_GNUC_UNUSED GdkModifierType state,
                                         gpointer user_data)
{
    if (keyval != GDK_KEY_Escape)
        return FALSE;

    gnc_customer_import_gui_request_close (user_data);
    return TRUE;
}

static void
gnc_customer_import_gui_close_handler (gpointer user_data)
{
    CustomerImportGui *gui = user_data;

    if (gui && gui->dialog)
        gtk_window_destroy (GTK_WINDOW (gui->dialog));
}

void
gnc_customer_import_gui_destroy_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;

    if (!gui)
        return;

    if (gui->dialog)
        g_object_set_data (G_OBJECT (gui->dialog), "gnc-customer-import-gui", NULL);

    gnc_suspend_gui_refresh ();
    if (gui->component_id != NO_COMPONENT)
        gnc_unregister_gui_component (gui->component_id);
    gnc_resume_gui_refresh ();

    gui->dialog = NULL;
    g_clear_object (&gui->store);
    g_string_free (gui->regexp, TRUE);
    g_free (gui);
}

void
gnc_customer_import_gui_buttonOpen_cb (GtkWidget *widget, gpointer user_data)
{
    CustomerImportGui *gui = user_data;
    CustomerImportFileDialogData *data;
    GncFileDialogRequest *request;

    data = g_new0 (CustomerImportFileDialogData, 1);
    g_weak_ref_init (&data->dialog, gui->dialog);
    request = gnc_file_dialog_request_new (
        gnc_ui_get_gtk_window (widget), _("Import Customers from CSV"),
        customer_import_file_filters (), NULL, GNC_FILE_DIALOG_IMPORT);
    gnc_file_dialog_request_open_async (request, NULL,
                                        customer_import_file_dialog_finished,
                                        data);
    g_object_unref (request);
}

void gnc_customer_import_gui_filenameChanged_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    gchar *filename = g_strdup( gnc_entry_get_text( GTK_ENTRY(gui->entryFilename) ) );

    // generate preview
    g_list_store_remove_all (gui->store);
    gnc_customer_import_read_file (filename, gui->regexp->str, gui->store, 10, NULL);

    g_free( filename );
}
// Semicolon separated.
void gnc_customer_import_gui_option1_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    if (!gtk_check_button_get_active( GTK_CHECK_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?(?<id>[^;]*);(?<company>[^;]*);(?<name>[^;]*);(?<addr1>[^;]*);(?<addr2>[^;]*);(?<addr3>[^;]*);(?<addr4>[^;]*);(?<phone>[^;]*);(?<fax>[^;]*);(?<email>[^;]*);(?<notes>[^;]*);(?<shipname>[^;]*);(?<shipaddr1>[^;]*);(?<shipaddr2>[^;]*);(?<shipaddr3>[^;]*);(?<shipaddr4>[^;]*);(?<shipphone>[^;]*);(?<shipfax>[^;]*);(?<shipemail>[^;]*)$");
    gnc_customer_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}
// Comma separated.
void gnc_customer_import_gui_option2_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    if (!gtk_check_button_get_active( GTK_CHECK_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?(?<id>[^,]*),(?<company>[^,]*),(?<name>[^,]*),(?<addr1>[^,]*),(?<addr2>[^,]*),(?<addr3>[^,]*),(?<addr4>[^,]*),(?<phone>[^,]*),(?<fax>[^,]*),(?<email>[^,]*),(?<notes>[^,]*),(?<shipname>[^,]*),(?<shipaddr1>[^,]*),(?<shipaddr2>[^,]*),(?<shipaddr3>[^,]*),(?<shipaddr4>[^,]*),(?<shipphone>[^,]*),(?<shipfax>[^,]*),(?<shipemail>[^,]*)$");
    gnc_customer_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}
// Semicolon separated with quoted strings.
void gnc_customer_import_gui_option3_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    if (!gtk_check_button_get_active( GTK_CHECK_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?((?<id>[^\";]*)|\"(?<id>[^\"]*)\");((?<company>[^\";]*)|\"(?<company>[^\"]*)\");((?<name>[^\";]*)|\"(?<name>[^\"]*)\");((?<addr1>[^\";]*)|\"(?<addr1>[^\"]*)\");((?<addr2>[^\";]*)|\"(?<addr2>[^\"]*)\");((?<addr3>[^\";]*)|\"(?<addr3>[^\"]*)\");((?<addr4>[^\";]*)|\"(?<addr4>[^\"]*)\");((?<phone>[^\";]*)|\"(?<phone>[^\"]*)\");((?<fax>[^\";]*)|\"(?<fax>[^\"]*)\");((?<email>[^\";]*)|\"(?<email>[^\"]*)\");((?<notes>[^\";]*)|\"(?<notes>[^\"]*)\");((?<shipname>[^\";]*)|\"(?<shipname>[^\"]*)\");((?<shipaddr1>[^\";]*)|\"(?<shipaddr1>[^\"]*)\");((?<shipaddr2>[^\";]*)|\"(?<shipaddr2>[^\"]*)\");((?<shipaddr3>[^\";]*)|\"(?<shipaddr3>[^\"]*)\");((?<shipaddr4>[^\";]*)|\"(?<shipaddr4>[^\"]*)\");((?<shipphone>[^\";]*)|\"(?<shipphone>[^\"]*)\");((?<shipfax>[^\";]*)|\"(?<shipfax>[^\"]*)\");((?<shipemail>[^;]*)|\"(?<shipemail>[^\"]*)\")$");
    gnc_customer_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}
// Comma separated with quoted strings.
void gnc_customer_import_gui_option4_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    if (!gtk_check_button_get_active( GTK_CHECK_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?((?<id>[^\",]*)|\"(?<id>[^\"]*)\"),((?<company>[^\",]*)|\"(?<company>[^\"]*)\"),((?<name>[^\",]*)|\"(?<name>[^\"]*)\"),((?<addr1>[^\",]*)|\"(?<addr1>[^\"]*)\"),((?<addr2>[^\",]*)|\"(?<addr2>[^\"]*)\"),((?<addr3>[^\",]*)|\"(?<addr3>[^\"]*)\"),((?<addr4>[^\",]*)|\"(?<addr4>[^\"]*)\"),((?<phone>[^\",]*)|\"(?<phone>[^\"]*)\"),((?<fax>[^\",]*)|\"(?<fax>[^\"]*)\"),((?<email>[^\",]*)|\"(?<email>[^\"]*)\"),((?<notes>[^\",]*)|\"(?<notes>[^\"]*)\"),((?<shipname>[^\",]*)|\"(?<shipname>[^\"]*)\"),((?<shipaddr1>[^\",]*)|\"(?<shipaddr1>[^\"]*)\"),((?<shipaddr2>[^\",]*)|\"(?<shipaddr2>[^\"]*)\"),((?<shipaddr3>[^\",]*)|\"(?<shipaddr3>[^\"]*)\"),((?<shipaddr4>[^\",]*)|\"(?<shipaddr4>[^\"]*)\"),((?<shipphone>[^\",]*)|\"(?<shipphone>[^\"]*)\"),((?<shipfax>[^\",]*)|\"(?<shipfax>[^\"]*)\"),((?<shipemail>[^\",]*)|\"(?<shipemail>[^\"]*)\")$");
    gnc_customer_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}
void gnc_customer_import_gui_option5_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    CustomerImportRegexRequest *request;

    if (!gtk_check_button_get_active (GTK_CHECK_BUTTON (widget)))
        return;

    request = customer_import_regex_request_new (gui);
    gnc_input_dialog_async (GTK_WINDOW (gui->dialog),
                            _("Adjust regular expression used for import"),
                            _("This regular expression is used to parse the import file. Modify according to your needs.\n"),
                            gui->regexp->str,
                            customer_import_regex_input_finished, request);
}




/*****************************************************************
 * Set whether we are importing a Customer or Vendor
 * ****************************************************************/
void gnc_customer_import_gui_type_cb (GtkWidget *widget, gpointer data)
{
    CustomerImportGui *gui = data;
    const gchar *name;
    if (!gtk_check_button_get_active( GTK_CHECK_BUTTON(widget) ))
        return;
    name = gtk_buildable_get_buildable_id(GTK_BUILDABLE(widget));
    if (name)
    {
        if  (g_ascii_strcasecmp(name, "radiobutton_customer") == 0)gui->type = "CUSTOMER";
        else if (g_ascii_strcasecmp(name, "radiobutton_vendor") == 0)gui->type = "VENDOR";
    }
    //printf ("TYPE set to, %s\n",gui->type); // DEBUG

}
