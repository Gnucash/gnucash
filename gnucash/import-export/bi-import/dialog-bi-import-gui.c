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
#include "qof.h"
#include "gnc-gui-query.h"
#include "gnc-file.h"
#include "dialog-bi-import.h"
#include "dialog-bi-import-gui.h"

struct _bi_import_gui
{
    GtkWindow    *parent;
    GtkWidget    *dialog;
    GtkColumnView *preview_view;
    GtkWidget    *entryFilename;
    GtkWidget    *ok_button;
    GListStore   *store;
    gint          component_id;
    gboolean      import_pending;
    GString      *regexp;
    QofBook      *book;
    gchar        *type;
    gchar        *open_mode;
};

typedef struct
{
    GWeakRef dialog;
} BiImportFileDialogData;

static void
bi_import_file_dialog_data_free (BiImportFileDialogData *data)
{
    g_weak_ref_clear (&data->dialog);
    g_free (data);
}

typedef struct
{
    GWeakRef dialog;
    QofBook *book;
    gint n_input_ignored;
    gint n_input_imported;
    guint n_fixed;
    GString *ignored_lines;
} BiImportRunRequest;

static BiImportRunRequest *
bi_import_run_request_new (BillImportGui *gui, bi_import_stats *stats,
                           guint n_fixed)
{
    BiImportRunRequest *request = g_new0 (BiImportRunRequest, 1);

    g_weak_ref_init (&request->dialog, gui->dialog);
    request->book = gui->book;
    request->n_input_ignored = stats->n_ignored;
    request->n_input_imported = stats->n_imported;
    request->n_fixed = n_fixed;
    request->ignored_lines = g_steal_pointer (&stats->ignored_lines);
    return request;
}

static void
bi_import_run_request_free (BiImportRunRequest *request)
{
    g_weak_ref_clear (&request->dialog);
    if (request->ignored_lines)
        g_string_free (request->ignored_lines, TRUE);
    g_free (request);
}

static void
bi_import_run_finished (gboolean completed, guint n_invoices_created,
                        guint n_invoices_updated, guint n_rows_ignored,
                        const gchar *info, gpointer user_data)
{
    BiImportRunRequest *request = user_data;
    GtkWidget *dialog = g_weak_ref_get (&request->dialog);
    BillImportGui *gui = NULL;

    if (dialog)
        gui = g_object_get_data (G_OBJECT (dialog), "gnc-bi-import-gui");
    if (gui)
    {
        gui->import_pending = FALSE;
        gtk_widget_set_sensitive (gui->ok_button, TRUE);
    }

    if (completed && gui && gui->book == request->book &&
        request->book == gnc_get_current_book () &&
        !qof_book_shutting_down (request->book))
    {
        if (info && *info)
            gnc_info_dialog (GTK_WINDOW (gui->dialog), "%s", info);
        gnc_info_dialog (GTK_WINDOW (gui->dialog),
                         _("Import:\n- rows ignored: %i\n- rows imported: %i\n\nValidation & processing:\n- rows fixed: %u\n- rows ignored: %u\n- invoices created: %u\n- invoices updated: %u"),
                         request->n_input_ignored, request->n_input_imported,
                         request->n_fixed, n_rows_ignored,
                         n_invoices_created, n_invoices_updated);
        if (request->n_input_ignored > 0 && request->ignored_lines)
            gnc_info2_dialog (gui->dialog,
                              _("These lines were ignored during import"),
                              request->ignored_lines->str);
        gnc_close_gui_component (gui->component_id);
    }

    g_clear_object (&dialog);
    bi_import_run_request_free (request);
}

void gnc_bi_import_gui_filenameChanged_cb (GtkWidget *widget, gpointer data);

typedef struct
{
    GWeakRef dialog;
    QofBook *book;
} BiImportRegexRequest;

static BiImportRegexRequest *
bi_import_regex_request_new (BillImportGui *gui)
{
    BiImportRegexRequest *request = g_new0 (BiImportRegexRequest, 1);

    g_weak_ref_init (&request->dialog, gui->dialog);
    request->book = gui->book;
    return request;
}

static void
bi_import_regex_request_free (BiImportRegexRequest *request)
{
    g_weak_ref_clear (&request->dialog);
    g_free (request);
}

static void
bi_import_regex_input_finished (gchar *input, gpointer user_data)
{
    BiImportRegexRequest *request = user_data;
    GtkWidget *dialog = g_weak_ref_get (&request->dialog);
    BillImportGui *gui = NULL;

    if (dialog)
        gui = g_object_get_data (G_OBJECT (dialog), "gnc-bi-import-gui");
    if (input && gui && gui->book == request->book && request->book == gnc_get_current_book () &&
        !qof_book_shutting_down (request->book))
    {
        g_string_assign (gui->regexp, input);
        gnc_bi_import_gui_filenameChanged_cb (gui->entryFilename, gui);
    }

    g_free (input);
    g_clear_object (&dialog);
    bi_import_regex_request_free (request);
}

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

#define UNUSED_VAR     __attribute__ ((unused))

static QofLogModule UNUSED_VAR log_module = G_LOG_DOMAIN; //G_LOG_BUSINESS;

static void
bi_import_preview_item_setup (GtkListItemFactory *factory, GtkListItem *item,
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
bi_import_preview_item_bind (GtkListItemFactory *factory, GtkListItem *item,
                             gpointer user_data)
{
    GObject *row = gtk_list_item_get_item (item);
    guint column = GPOINTER_TO_UINT (user_data);

    (void)factory;
    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (item)),
                        gnc_bi_import_row_get (row, column));
}

static void
bi_import_preview_add_column (GtkColumnView *view, const gchar *title, guint column)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkColumnViewColumn *view_column;

    g_signal_connect (factory, "setup", G_CALLBACK (bi_import_preview_item_setup),
                      GUINT_TO_POINTER (column));
    g_signal_connect (factory, "bind", G_CALLBACK (bi_import_preview_item_bind),
                      GUINT_TO_POINTER (column));
    view_column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_append_column (view, view_column);
    g_object_unref (view_column);
}

BillImportGui *
gnc_plugin_bi_import_showGUI (GtkWindow *parent)
{
    BillImportGui *gui;
    GtkBuilder *builder;
    GList *glist;
    GtkNoSelection *selection;
    GtkScrolledWindow *preview_scrolledwindow;

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
    g_object_set_data (G_OBJECT (gui->dialog), "gnc-bi-import-gui", gui);
    gtk_window_set_transient_for(GTK_WINDOW(gui->dialog), GTK_WINDOW(parent));
    gui->parent = parent;
    gui->entryFilename = GTK_WIDGET(gtk_builder_get_object (builder, "entryFilename"));
    gui->ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "okbutton"));
    preview_scrolledwindow = GTK_SCROLLED_WINDOW (gtk_builder_get_object (builder,
                                                   "scrolledwindow2"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(gui->dialog), "gnc-id-bill-import");
    gnc_widget_style_context_add_class (GTK_WIDGET(gui->dialog), "gnc-class-imports");

    gtk_window_set_transient_for (GTK_WINDOW (gui->dialog), parent);

    gui->book = gnc_get_current_book();

    gui->regexp = g_string_new ( "^(\\x{FEFF})?(?<id>[^;]*);(?<date_opened>[^;]*);(?<owner_id>[^;]*);(?<billing_id>[^;]*);(?<notes>[^;]*);(?<date>[^;]*);(?<desc>[^;]*);(?<action>[^;]*);(?<account>[^;]*);(?<quantity>[^;]*);(?<price>[^;]*);(?<disc_type>[^;]*);(?<disc_how>[^;]*);(?<discount>[^;]*);(?<taxable>[^;]*);(?<taxincluded>[^;]*);(?<tax_table>[^;]*);(?<date_posted>[^;]*);(?<due_date>[^;]*);(?<account_posted>[^;]*);(?<memo_posted>[^;]*);(?<accu_splits>[^;]*)$");

    /* The preview and import logic share one GTK4 list model. */
    gui->store = g_list_store_new (G_TYPE_OBJECT);
    selection = gtk_no_selection_new (G_LIST_MODEL (g_object_ref (gui->store)));
    gui->preview_view = GTK_COLUMN_VIEW (gtk_column_view_new (GTK_SELECTION_MODEL (selection)));
    bi_import_preview_add_column (gui->preview_view, _("ID"), ID);
    bi_import_preview_add_column (gui->preview_view, _("Date Opened"), DATE_OPENED);
    bi_import_preview_add_column (gui->preview_view, _("Owner-ID"), OWNER_ID);
    bi_import_preview_add_column (gui->preview_view, _("Billing-ID"), BILLING_ID);
    bi_import_preview_add_column (gui->preview_view, _("Notes"), NOTES);
    bi_import_preview_add_column (gui->preview_view, _("Date"), DATE);
    bi_import_preview_add_column (gui->preview_view, _("Description"), DESC);
    bi_import_preview_add_column (gui->preview_view, _("Action"), ACTION);
    bi_import_preview_add_column (gui->preview_view, _("Account"), ACCOUNT);
    bi_import_preview_add_column (gui->preview_view, _("Quantity"), QUANTITY);
    bi_import_preview_add_column (gui->preview_view, _("Price"), PRICE);
    bi_import_preview_add_column (gui->preview_view, _("Disc-type"), DISC_TYPE);
    bi_import_preview_add_column (gui->preview_view, _("Disc-how"), DISC_HOW);
    bi_import_preview_add_column (gui->preview_view, _("Discount"), DISCOUNT);
    bi_import_preview_add_column (gui->preview_view, _("Taxable"), TAXABLE);
    bi_import_preview_add_column (gui->preview_view, _("Taxincluded"), TAXINCLUDED);
    bi_import_preview_add_column (gui->preview_view, _("Tax-table"), TAX_TABLE);
    bi_import_preview_add_column (gui->preview_view, _("Date Posted"), DATE_POSTED);
    bi_import_preview_add_column (gui->preview_view, _("Due Date"), DUE_DATE);
    bi_import_preview_add_column (gui->preview_view, _("Account-posted"), ACCOUNT_POSTED);
    bi_import_preview_add_column (gui->preview_view, _("Memo-posted"), MEMO_POSTED);
    bi_import_preview_add_column (gui->preview_view, _("Accu-splits"), ACCU_SPLITS);
    gtk_scrolled_window_set_child (preview_scrolledwindow, GTK_WIDGET (gui->preview_view));

    gui->component_id = gnc_register_gui_component ("dialog-bi-import-gui",
                        NULL,
                        gnc_bi_import_gui_close_handler,
                        gui);

    /* Setup signals */
gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, gui);

    g_object_unref(G_OBJECT(builder));
    gtk_window_present (GTK_WINDOW (gui->dialog));

    return gui;
}

static GList *
bi_import_file_filters (void)
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
bi_import_file_dialog_finished (GObject *source, GAsyncResult *result,
                                gpointer user_data)
{
    BiImportFileDialogData *data = user_data;
    GncFileDialogRequest *request = GNC_FILE_DIALOG_REQUEST (source);
    GError *error = NULL;
    GFile *file;
    GtkWidget *dialog;
    BillImportGui *gui = NULL;

    file = gnc_file_dialog_request_finish (request, result, &error);
    dialog = g_weak_ref_get (&data->dialog);
    if (dialog)
        gui = g_object_get_data (G_OBJECT (dialog), "gnc-bi-import-gui");

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
    bi_import_file_dialog_data_free (data);
}

void
gnc_bi_import_gui_ok_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    gchar *filename;
    bi_import_stats stats;
    bi_import_result res;
    guint n_fixed;
    guint n_deleted;
    GString *info;
    BiImportRunRequest *request;

    (void)widget;
    if (gui->import_pending)
        return;
    if (gui->book != gnc_get_current_book () || qof_book_shutting_down (gui->book))
        return;

    filename = g_strdup (gnc_entry_get_text (GTK_ENTRY (gui->entryFilename)));
    g_list_store_remove_all (gui->store);
    res = gnc_bi_import_read_file (filename, gui->regexp->str, gui->store, 0, &stats);
    if (res == RESULT_OK)
    {
        info = g_string_new ("");
        gnc_bi_import_fix_bis (gui->store, &n_fixed, &n_deleted, info, gui->type);
        request = bi_import_run_request_new (gui, &stats, n_fixed);
        gui->import_pending = TRUE;
        gtk_widget_set_sensitive (gui->ok_button, FALSE);
        gnc_bi_import_create_bis_async (gui->store, gui->book, gui->type,
                                        gui->open_mode, n_deleted, info,
                                        GTK_WINDOW (gui->dialog), gui->parent,
                                        bi_import_run_finished, request);
    }
    else if (res == RESULT_OPEN_FAILED)
    {
        gnc_error_dialog (GTK_WINDOW (gui->dialog),
                          _("The input file can not be opened."));
    }
    else if (res == RESULT_ERROR_IN_REGEXP)
    {
        /* gnc_bi_import_read_file already reports the expression error. */
    }

    g_free (filename);
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
    BillImportGui *gui = data;
    gnc_gnome_help (GTK_WINDOW(gui->dialog), DF_GUIDE, DL_IMPORT_BC);
}

static void
gnc_bi_import_gui_close_handler (gpointer user_data)
{
    BillImportGui *gui = user_data;

    gtk_window_destroy (GTK_WINDOW(gui->dialog));
}

void
gnc_bi_import_gui_destroy_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;

    g_object_set_data (G_OBJECT (widget), "gnc-bi-import-gui", NULL);

    gnc_suspend_gui_refresh ();
    gnc_unregister_gui_component (gui->component_id);
    gnc_resume_gui_refresh ();

    g_object_unref (gui->store);
    g_string_free (gui->regexp, TRUE);
    g_free (gui);
}

void
gnc_bi_import_gui_buttonOpen_cb (GtkWidget *widget, gpointer user_data)
{
    BillImportGui *gui = user_data;
    BiImportFileDialogData *data;
    GncFileDialogRequest *request;

    data = g_new0 (BiImportFileDialogData, 1);
    g_weak_ref_init (&data->dialog, gui->dialog);
    request = gnc_file_dialog_request_new (
        gnc_ui_get_gtk_window (widget),
        _("Import Bills or Invoices from CSV"), bi_import_file_filters (), NULL,
        GNC_FILE_DIALOG_IMPORT);
    gnc_file_dialog_request_open_async (request, NULL,
                                        bi_import_file_dialog_finished, data);
    g_object_unref (request);
}

void gnc_bi_import_gui_filenameChanged_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    gchar *filename = g_strdup( gnc_entry_get_text( GTK_ENTRY(gui->entryFilename) ) );

    // generate preview
    g_list_store_remove_all (gui->store);
    gnc_bi_import_read_file (filename, gui->regexp->str, gui->store, 100, NULL);

    g_free( filename );
}

// Semicolon separated
void gnc_bi_import_gui_option1_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    if (!gtk_check_button_get_active( GTK_CHECK_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?(?<id>[^;]*);(?<date_opened>[^;]*);(?<owner_id>[^;]*);(?<billing_id>[^;]*);(?<notes>[^;]*);(?<date>[^;]*);(?<desc>[^;]*);(?<action>[^;]*);(?<account>[^;]*);(?<quantity>[^;]*);(?<price>[^;]*);(?<disc_type>[^;]*);(?<disc_how>[^;]*);(?<discount>[^;]*);(?<taxable>[^;]*);(?<taxincluded>[^;]*);(?<tax_table>[^;]*);(?<date_posted>[^;]*);(?<due_date>[^;]*);(?<account_posted>[^;]*);(?<memo_posted>[^;]*);(?<accu_splits>[^;]*)$");
    gnc_bi_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}

// Comma separated
void gnc_bi_import_gui_option2_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    if (!gtk_check_button_get_active( GTK_CHECK_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?(?<id>[^,]*),(?<date_opened>[^,]*),(?<owner_id>[^,]*),(?<billing_id>[^,]*),(?<notes>[^,]*),(?<date>[^,]*),(?<desc>[^,]*),(?<action>[^,]*),(?<account>[^,]*),(?<quantity>[^,]*),(?<price>[^,]*),(?<disc_type>[^,]*),(?<disc_how>[^,]*),(?<discount>[^,]*),(?<taxable>[^,]*),(?<taxincluded>[^,]*),(?<tax_table>[^,]*),(?<date_posted>[^,]*),(?<due_date>[^,]*),(?<account_posted>[^,]*),(?<memo_posted>[^,]*),(?<accu_splits>[^,]*)$");
    gnc_bi_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}

// Semicolon separated with quotes
void gnc_bi_import_gui_option3_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    if (!gtk_check_button_get_active( GTK_CHECK_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?((?<id>[^\";]*)|\"(?<id>[^\"]*)\");((?<date_opened>[^\";]*)|\"(?<date_opened>[^\"]*)\");((?<owner_id>[^\";]*)|\"(?<owner_id>[^\"]*)\");((?<billing_id>[^\";]*)|\"(?<billing_id>[^\"]*)\");((?<notes>[^\";]*)|\"(?<notes>([^\"]|\"\")*)\");((?<date>[^\";]*)|\"(?<date>[^\"]*)\");((?<desc>[^\";]*)|\"(?<desc>([^\"]|\"\")*)\");((?<action>[^\";]*)|\"(?<action>[^\"]*)\");((?<account>[^\";]*)|\"(?<account>[^\"]*)\");((?<quantity>[^\";]*)|\"(?<quantity>[^\"]*)\");((?<price>[^\";]*)|\"(?<price>[^\"]*)\");((?<disc_type>[^\";]*)|\"(?<disc_type>[^\"]*)\");((?<disc_how>[^\";]*)|\"(?<disc_how>[^\"]*)\");((?<discount>[^\";]*)|\"(?<discount>[^\"]*)\");((?<taxable>[^\";]*)|\"(?<taxable>[^\"]*)\");((?<taxincluded>[^\";]*)|\"(?<taxincluded>[^\"]*)\");((?<tax_table>[^\";]*)|\"(?<tax_table>[^\"]*)\");((?<date_posted>[^\";]*)|\"(?<date_posted>[^\"]*)\");((?<due_date>[^\";]*)|\"(?<due_date>[^\"]*)\");((?<account_posted>[^\";]*)|\"(?<account_posted>[^\"]*)\");((?<memo_posted>[^\";]*)|\"(?<memo_posted>[^\"]*)\");((?<accu_splits>[^\";]*)|\"(?<accu_splits>[^\"]*)\")$");
    gnc_bi_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}

// Comma separated with quote
void gnc_bi_import_gui_option4_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    if (!gtk_check_button_get_active( GTK_CHECK_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?((?<id>[^\",]*)|\"(?<id>[^\"]*)\"),((?<date_opened>[^\",]*)|\"(?<date_opened>[^\"]*)\"),((?<owner_id>[^\",]*)|\"(?<owner_id>[^\"]*)\"),((?<billing_id>[^\",]*)|\"(?<billing_id>[^\"]*)\"),((?<notes>[^\",]*)|\"(?<notes>([^\"]|\"\")*)\"),((?<date>[^\",]*)|\"(?<date>[^\"]*)\"),((?<desc>[^\",]*)|\"(?<desc>([^\"]|\"\")*)\"),((?<action>[^\",]*)|\"(?<action>[^\"]*)\"),((?<account>[^\",]*)|\"(?<account>[^\"]*)\"),((?<quantity>[^\",]*)|\"(?<quantity>[^\"]*)\"),((?<price>[^\",]*)|\"(?<price>[^\"]*)\"),((?<disc_type>[^\",]*)|\"(?<disc_type>[^\"]*)\"),((?<disc_how>[^\",]*)|\"(?<disc_how>[^\"]*)\"),((?<discount>[^\",]*)|\"(?<discount>[^\"]*)\"),((?<taxable>[^\",]*)|\"(?<taxable>[^\"]*)\"),((?<taxincluded>[^\",]*)|\"(?<taxincluded>[^\"]*)\"),((?<tax_table>[^\",]*)|\"(?<tax_table>[^\"]*)\"),((?<date_posted>[^\",]*)|\"(?<date_posted>[^\"]*)\"),((?<due_date>[^\",]*)|\"(?<due_date>[^\"]*)\"),((?<account_posted>[^\",]*)|\"(?<account_posted>[^\"]*)\"),((?<memo_posted>[^\",]*)|\"(?<memo_posted>[^\"]*)\"),((?<accu_splits>[^\",]*)|\"(?<accu_splits>[^\"]*)\")$");
    gnc_bi_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}

// DIY regex.
void gnc_bi_import_gui_option5_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    BiImportRegexRequest *request;

    if (!gtk_check_button_get_active (GTK_CHECK_BUTTON (widget)))
        return;

    request = bi_import_regex_request_new (gui);
    gnc_input_dialog_async (GTK_WINDOW (gui->dialog),
                            _("Adjust regular expression used for import"),
                            _("This regular expression is used to parse the import file. Modify according to your needs.\n"),
                            gui->regexp->str,
                            bi_import_regex_input_finished, request);
}

void gnc_bi_import_gui_open_mode_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    const gchar *name = NULL;
    name = gtk_buildable_get_buildable_id(GTK_BUILDABLE(widget));
    if (!gtk_check_button_get_active( GTK_CHECK_BUTTON(widget) ))
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
    name = gtk_buildable_get_buildable_id(GTK_BUILDABLE(widget));
    if (!gtk_check_button_get_active( GTK_CHECK_BUTTON(widget) ))
        return;
    if  (g_ascii_strcasecmp(name, "radiobuttonInvoice") == 0)gui->type = "INVOICE";
    else if (g_ascii_strcasecmp(name, "radiobuttonBill") == 0)gui->type = "BILL";
    //printf ("TYPE set to, %s\n",gui->type);

}
