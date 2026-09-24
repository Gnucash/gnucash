/*******************************************************************\
 * assistant-csv-account-import.c -- An assistant for importing     *
 *                                         Accounts from a file.    *
 *                                                                  *
 * Copyright (C) 2012 Robert Fewell                                 *
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
/** @file assistant-csv-account-import.c
    @brief CSV Import Assistant
    @author Copyright (c) 2012 Robert Fewell
*/
#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-ui.h"
#include "gnc-uri-utils.h"
#include "gnc-ui-util.h"
#include "gnc-file.h"

#include "gnc-component-manager.h"
#include "qof.h"

#include "assistant-csv-account-import.h"
#include "csv-account-import.h"
#include "gnc-import-assistant.h"

#define GNC_PREFS_GROUP "dialogs.import.csv"
#define ASSISTANT_CSV_IMPORT_CM_CLASS "assistant-csv-account-import"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

/*************************************************************************/

void csv_import_assistant_prepare (GncImportAssistant  *assistant, GtkWidget *page, gpointer user_data);
void csv_import_assistant_finish (GncImportAssistant *gtkassistant, gpointer user_data);
void csv_import_assistant_cancel (GncImportAssistant *gtkassistant, gpointer user_data);
void csv_import_assistant_close (GncImportAssistant *gtkassistant, gpointer user_data);

void csv_import_assistant_start_page_prepare (GncImportAssistant *gtkassistant, gpointer user_data);
void csv_import_assistant_account_page_prepare (GncImportAssistant *gtkassistant, gpointer user_data);
void csv_import_assistant_file_page_prepare (GncImportAssistant *assistant, gpointer user_data);
void csv_import_assistant_finish_page_prepare (GncImportAssistant *assistant, gpointer user_data);
void csv_import_assistant_summary_page_prepare (GncImportAssistant *assistant, gpointer user_data);

void csv_import_sep_cb (GtkWidget *radio, gpointer user_data );
void csv_import_hrows_cb (GtkWidget *spin, gpointer user_data );


static const gchar *finish_tree_string = N_(
            "The accounts will be imported from the file '%s' when you click 'Apply'.\n\n"
            "You can verify your selections by clicking on 'Back' or 'Cancel' to Abort Import.\n");

static const gchar *new_book_finish_tree_string = N_(
            "The accounts will be imported from the file '%s' when you click 'Apply'.\n\n"
            "You can verify your selections by clicking on 'Back' or 'Cancel' to Abort Import.\n\n"
            "If this is your initial import into a new file, you will first see "
            "a dialog for setting book options, since these can affect how "
            "imported data is converted to GnuCash transactions.\n"
            "Note: After import, you may need to use 'View / Filter By / Other' menu option "
            "and select to show unused Accounts.\n");

/* Escape '_' in string */
static gchar *mnemonic_escape (const gchar *source);
static gchar *mnemonic_escape (const gchar *source)
{
    const guchar *p;
    gchar *dest;
    gchar *q;

    g_return_val_if_fail (source != NULL, NULL);

    p = (guchar *) source;
    q = dest = g_malloc (strlen (source) * 2 + 1);

    while (*p)
    {
        switch (*p)
        {
        case '_':
            *q++ = '_';
            *q++ = '_';
            break;
        default:
            *q++ = *p;
            break;
        }
        p++;
    }
    *q = 0;
    return dest;
}

static void
csv_import_preview_item_setup (GtkListItemFactory *factory, GtkListItem *item,
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
csv_import_preview_item_bind (GtkListItemFactory *factory, GtkListItem *item,
                              gpointer user_data)
{
    GObject *row = gtk_list_item_get_item (item);
    GtkLabel *label = GTK_LABEL (gtk_list_item_get_child (item));
    const gchar *value = csv_import_row_get (row, GPOINTER_TO_UINT (user_data));

    (void)factory;
    if (g_strcmp0 (csv_import_row_get (row, ROW_COLOR), "pink") == 0)
    {
        gchar *escaped = g_markup_escape_text (value, -1);
        gchar *markup = g_strdup_printf ("<span background=\"pink\">%s</span>", escaped);

        gtk_label_set_markup (label, markup);
        g_free (markup);
        g_free (escaped);
    }
    else
        gtk_label_set_text (label, value);
}

static void
csv_import_preview_add_column (GtkColumnView *view, const gchar *title, guint column)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkColumnViewColumn *view_column;

    g_signal_connect (factory, "setup", G_CALLBACK (csv_import_preview_item_setup),
                      GUINT_TO_POINTER (column));
    g_signal_connect (factory, "bind", G_CALLBACK (csv_import_preview_item_bind),
                      GUINT_TO_POINTER (column));
    view_column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_append_column (view, view_column);
    g_object_unref (view_column);
}

static
void create_regex (GString *regex_str, const gchar *sep)
{
    if (!sep) return;

    g_string_printf (regex_str,
            "\\G(?<type>[^%s]*)%s"
            "(?<full_name>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<name>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<code>\"(?:[^\"]|\"\")*\"|[^%s]*)%s?"
            "(?<description>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<color>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<notes>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<symbol>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<namespace>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<hidden>[^%s]*)%s"
            "(?<tax>[^%s]*)%s"
            "(?<placeholder>[^%s[:cntrl:]]*)(?:\\R*)",
            sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep,
            sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep);

}

/*************************************************************************/

typedef struct
{
    GWeakRef assistant;
} CsvImportFileDialogData;

static void
csv_import_file_dialog_data_free (CsvImportFileDialogData *data)
{
    g_weak_ref_clear (&data->assistant);
    g_free (data);
}

static void
csv_import_file_dialog_finished (GObject *source, GAsyncResult *result,
                                 gpointer user_data)
{
    CsvImportFileDialogData *data = user_data;
    GncFileDialogRequest *request = GNC_FILE_DIALOG_REQUEST (source);
    GError *error = NULL;
    GFile *file;
    GtkWidget *assistant_widget;
    CsvImportInfo *info = NULL;

    file = gnc_file_dialog_request_finish (request, result, &error);
    assistant_widget = g_weak_ref_get (&data->assistant);
    if (assistant_widget)
        info = g_object_get_data (G_OBJECT (assistant_widget),
                                  "gnc-csv-account-import-info");

    if (file)
    {
        gchar *file_name = g_file_get_path (file);

        if (info && file_name && !g_file_test (file_name, G_FILE_TEST_IS_DIR))
        {
            gchar *filedir = g_path_get_dirname (file_name);
            GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT (info->assistant);

            g_free (info->file_name);
            info->file_name = g_strdup (file_name);
            g_free (info->starting_dir);
            info->starting_dir = filedir;
            gtk_button_set_label (GTK_BUTTON (info->file_button), file_name);
            gnc_import_assistant_set_page_complete (assistant, info->account_page,
                                             FALSE);
            gnc_import_assistant_set_page_complete (assistant, info->file_page, TRUE);
            gnc_import_assistant_next_page (assistant);
            DEBUG ("file_name selected is %s", info->file_name);
            DEBUG ("starting directory is %s", info->starting_dir);
        }
        else if (info)
            gnc_error_dialog (GTK_WINDOW (assistant_widget), "%s",
                              _("Please select a local file to import."));
        g_free (file_name);
        g_object_unref (file);
    }
    else if (info && error &&
             !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
        gnc_error_dialog (GTK_WINDOW (assistant_widget), "%s", error->message);

    g_clear_error (&error);
    g_clear_object (&assistant_widget);
    csv_import_file_dialog_data_free (data);
}

static void
csv_import_select_file_cb (GtkButton *button, gpointer user_data)
{
    CsvImportInfo *info = user_data;
    CsvImportFileDialogData *data;
    GncFileDialogRequest *request;

    data = g_new0 (CsvImportFileDialogData, 1);
    g_weak_ref_init (&data->assistant, info->assistant);
    request = gnc_file_dialog_request_new (
        GTK_WINDOW (info->assistant), _("Choose CSV account file"), NULL,
        info->starting_dir, GNC_FILE_DIALOG_IMPORT);
    gnc_file_dialog_request_open_async (request, NULL,
                                        csv_import_file_dialog_finished, data);
    g_object_unref (request);
}

/*******************************************************
 * csv_import_hrows_cb
 *
 * call back for the start row / number of header rows
 *******************************************************/
void csv_import_hrows_cb (GtkWidget *spin, gpointer user_data)
{
    CsvImportInfo *info = user_data;
    guint count;

    /* Get number of rows for header */
    info->header_rows = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spin));

    /* Keep the preview highlighting derived from the one header-row invariant
     * instead of incrementally changing individual legacy model rows. */
    count = g_list_model_get_n_items (G_LIST_MODEL (info->store));
    for (guint position = 0;
         position < count;
         position++)
    {
        GObject *row = g_list_model_get_item (G_LIST_MODEL (info->store), position);
        csv_import_row_set (row, ROW_COLOR,
                            position < (guint)info->header_rows ? "pink" : "");
        g_object_unref (row);
    }
    if (count)
        g_list_model_items_changed (G_LIST_MODEL (info->store), 0, count, count);
}


/*******************************************************
 * csv_import_assistant_enable_account_forward
 *
 * enable "Next" button on account_page if store has rows
 *******************************************************/
static void csv_import_assistant_enable_account_forward (CsvImportInfo *info)
{
    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT(info->assistant);
    gboolean store_has_rows = TRUE;

    /* if the store is empty, disable "Next" button */
    if (g_list_model_get_n_items (G_LIST_MODEL (info->store)) == 0)
        store_has_rows = FALSE;

    gnc_import_assistant_set_page_complete (assistant, info->account_page, store_has_rows);
}


static void
csv_import_regex_changed (CsvImportInfo *info)
{
    /* Generate preview only after the selected regular expression is stable. */
    g_list_store_remove_all (info->store);
    gtk_widget_set_sensitive (info->header_row_spin, TRUE);

    if (csv_import_read_file (GTK_WINDOW (info->assistant), info->file_name,
                              info->regexp->str, info->store, 11) == MATCH_FOUND)
        gtk_spin_button_set_value (GTK_SPIN_BUTTON (info->header_row_spin), 1);
    else
        gtk_spin_button_set_value (GTK_SPIN_BUTTON (info->header_row_spin), 0);

    csv_import_assistant_enable_account_forward (info);
}

typedef struct
{
    GWeakRef assistant;
    QofBook *book;
} CsvImportRegexRequest;

static CsvImportRegexRequest *
csv_import_regex_request_new (CsvImportInfo *info)
{
    CsvImportRegexRequest *request = g_new0 (CsvImportRegexRequest, 1);

    g_weak_ref_init (&request->assistant, info->assistant);
    request->book = gnc_get_current_book ();
    return request;
}

static void
csv_import_regex_request_free (CsvImportRegexRequest *request)
{
    g_weak_ref_clear (&request->assistant);
    g_free (request);
}

static void
csv_import_regex_input_finished (gchar *input, gpointer user_data)
{
    CsvImportRegexRequest *request = user_data;
    GtkWidget *assistant = g_weak_ref_get (&request->assistant);
    CsvImportInfo *info = NULL;

    if (assistant)
        info = g_object_get_data (G_OBJECT (assistant),
                                  "gnc-csv-account-import-info");
    if (input && info && request->book &&
        request->book == gnc_get_current_book () &&
        !qof_book_shutting_down (request->book))
    {
        g_string_assign (info->regexp, input);
        csv_import_regex_changed (info);
    }

    g_free (input);
    g_clear_object (&assistant);
    csv_import_regex_request_free (request);
}

/*******************************************************
 * csv_import_sep_cb
 *
 * call back for type of separator required
 *******************************************************/
void csv_import_sep_cb (GtkWidget *radio, gpointer user_data)
{
    CsvImportInfo *info = user_data;
    const gchar *name;
    const gchar *sep = NULL;

    if (!gtk_check_button_get_active (GTK_CHECK_BUTTON (radio)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }

    name = gtk_buildable_get_buildable_id (GTK_BUILDABLE (radio));
    if (g_strcmp0 (name, "radio_semi") == 0)
        sep = ";";
    else if (g_strcmp0 (name, "radio_colon") == 0)
        sep = ":";
    else
        sep = ","; /* Default and custom preview baseline. */

    if (g_strcmp0 (name, "radio_custom") == 0)
    {
        CsvImportRegexRequest *request = csv_import_regex_request_new (info);
        GString *default_regex = g_string_new (NULL);

        create_regex (default_regex, sep);
        gnc_input_dialog_async (GTK_WINDOW (info->assistant),
                                _("Adjust regular expression used for import"),
                                _("This regular expression is used to parse the import file. Modify according to your needs.\n"),
                                default_regex->str,
                                csv_import_regex_input_finished, request);
        g_string_free (default_regex, TRUE);
        return;
    }

    create_regex (info->regexp, sep);
    csv_import_regex_changed (info);
}


/*******************************************************
 * load_settings
 *
 * load the default settings for the assistant
 *******************************************************/
static
void load_settings (CsvImportInfo *info)
{
    info->header_rows = 0;
    info->error = "";
    info->starting_dir = NULL;
    info->file_name = NULL;
    info->error = "";

    /* The default directory for the user to select files. */
    info->starting_dir = gnc_get_default_directory (GNC_PREFS_GROUP);
}


/* =============================================================== */

/*******************************************************
 * Assistant page prepare functions
 *******************************************************/
void
csv_import_assistant_start_page_prepare (GncImportAssistant *assistant,
        gpointer user_data)
{
    gint num = gnc_import_assistant_get_current_page (assistant);
    GtkWidget *page = gnc_import_assistant_get_nth_page (assistant, num);

    /* Enable the Assistant Buttons */
    gnc_import_assistant_set_page_complete (assistant, page, TRUE);
}


void
csv_import_assistant_file_page_prepare (GncImportAssistant *assistant,
                                        gpointer user_data)
{
    CsvImportInfo *info = user_data;

    gtk_button_set_label (GTK_BUTTON (info->file_button),
                          info->file_name ? info->file_name :
                          _("Choose File…"));
    /* Selecting a file is the only transition from this page. */
    gnc_import_assistant_set_page_complete (assistant, info->file_page, FALSE);
}


void
csv_import_assistant_account_page_prepare (GncImportAssistant *assistant,
        gpointer user_data)
{
    CsvImportInfo *info = user_data;
    csv_import_result res;

    /* Disable the "Next" Assistant Button */
    gnc_import_assistant_set_page_complete (assistant, info->account_page, FALSE);

    /* test read one line */
    g_list_store_remove_all (info->store);
    res = csv_import_read_file (GTK_WINDOW (info->assistant), info->file_name, info->regexp->str, info->store, 1 );
    if (res == RESULT_OPEN_FAILED)
    {
        gnc_error_dialog (GTK_WINDOW (info->assistant), _("The input file can not be opened."));
        gnc_import_assistant_previous_page (assistant);
    }
    else if (res == RESULT_OK)
        gnc_import_assistant_set_page_complete (assistant, info->account_page, TRUE);
    else if (res == MATCH_FOUND)
        gnc_import_assistant_set_page_complete (assistant, info->account_page, TRUE);

    // generate preview
    g_list_store_remove_all (info->store);

    gtk_widget_set_sensitive (info->header_row_spin, TRUE);

    if (csv_import_read_file (GTK_WINDOW (info->assistant), info->file_name, info->regexp->str, info->store, 11 ) == MATCH_FOUND)
        gtk_spin_button_set_value (GTK_SPIN_BUTTON(info->header_row_spin), 1); // set header spin to 1
    else
        gtk_spin_button_set_value (GTK_SPIN_BUTTON(info->header_row_spin), 0); //reset header spin to 0

    /* if the store has rows, enable "Next" button */
    csv_import_assistant_enable_account_forward (info);
}


void
csv_import_assistant_finish_page_prepare (GncImportAssistant *assistant,
        gpointer user_data)
{
    CsvImportInfo *info = user_data;
    gchar *text;

    /* Set Finish page text */
    /* Before creating accounts, if this is a new book, tell user they can
     * specify book options, since they affect how transactions are created */
    if (info->new_book)
        text = g_strdup_printf (gettext (new_book_finish_tree_string), info->file_name);
    else
        text = g_strdup_printf (gettext (finish_tree_string), info->file_name);

    gtk_label_set_text (GTK_LABEL(info->finish_label), text);
    g_free (text);

    /* Save the Window size and directory */
    gnc_set_default_directory (GNC_PREFS_GROUP, info->starting_dir);

    /* Enable the Assistant Buttons */
    gnc_import_assistant_set_page_complete (assistant, info->finish_label, TRUE);
}


void
csv_import_assistant_summary_page_prepare (GncImportAssistant *assistant,
        gpointer user_data)
{
    CsvImportInfo *info = user_data;
    gchar *text, *errtext, *mtext;

    if (g_strcmp0 (info->error, "") != 0)
    {
        GtkTextBuffer *buffer;

        buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(info->summary_error_view));
        text = g_strdup_printf (gettext ("Import completed but with errors!\n\nThe number of Accounts added was %u and "
                                        "%u were updated.\n\nSee below for errors…"), info->num_new, info->num_updates);
        errtext = g_strdup_printf ("%s", info->error);
        gtk_text_buffer_set_text (buffer, errtext, -1);
        g_free (errtext);
        g_free (info->error);
    }
    else
        text = g_strdup_printf (gettext ("Import completed successfully!\n\nThe number of Accounts added was %u and "
                                        "%u were updated.\n"), info->num_new, info->num_updates);

    mtext = g_strdup_printf ("<span size=\"medium\"><b>%s</b></span>", text);
    gtk_label_set_markup (GTK_LABEL(info->summary_label), mtext);

    g_free (text);
    g_free (mtext);
}


void
csv_import_assistant_prepare (GncImportAssistant *assistant, GtkWidget *page,
                              gpointer user_data)
{
    gint currentpage = gnc_import_assistant_get_current_page (assistant);

    switch (currentpage)
    {
    case 0:
        /* Current page is Import Start page */
        csv_import_assistant_start_page_prepare (assistant, user_data);
        break;
    case 1:
        /* Current page is File select page */
        csv_import_assistant_file_page_prepare (assistant, user_data);
        break;
    case 2:
        /* Current page is Account page */
        csv_import_assistant_account_page_prepare (assistant, user_data);
        break;
    case 3:
        /* Current page is Finish page */
        csv_import_assistant_finish_page_prepare (assistant, user_data);
        break;
    case 4:
        /* Current page is Summary page */
        csv_import_assistant_summary_page_prepare (assistant, user_data);
        break;
    }
}


/*******************************************************
 * Assistant call back functions
 *******************************************************/
static void
csv_import_assistant_destroy_cb (GtkWidget *object, gpointer user_data)
{
    CsvImportInfo *info = user_data;

    g_object_set_data (G_OBJECT (object), "gnc-csv-account-import-info", NULL);
    gnc_unregister_gui_component_by_data (ASSISTANT_CSV_IMPORT_CM_CLASS, info);
    g_free (info);
}

void
csv_import_assistant_cancel (GncImportAssistant *assistant, gpointer user_data)
{
    CsvImportInfo *info = user_data;
    gnc_close_gui_component_by_data (ASSISTANT_CSV_IMPORT_CM_CLASS, info);
}

void
csv_import_assistant_close (GncImportAssistant *assistant, gpointer user_data)
{
    CsvImportInfo *info = user_data;
    gnc_close_gui_component_by_data (ASSISTANT_CSV_IMPORT_CM_CLASS, info);
}

typedef struct
{
    GWeakRef assistant;
    QofBook *book;
} CsvImportNewBookRequest;

static void
csv_import_new_book_request_free (CsvImportNewBookRequest *request)
{
    g_weak_ref_clear (&request->assistant);
    g_free (request);
}

static void
csv_import_finish_import (GncImportAssistant *assistant, CsvImportInfo *info)
{
    g_list_store_remove_all (info->store);
    csv_import_read_file (GTK_WINDOW (assistant), info->file_name,
                          info->regexp->str, info->store, 0);
    csv_account_import (info);
    gnc_import_assistant_set_current_page (assistant, 4);
}

static void
csv_import_new_book_options_finished (GtkWindow *parent, gboolean applied,
                                      gpointer user_data)
{
    CsvImportNewBookRequest *request = user_data;
    GtkWidget *assistant_widget = g_weak_ref_get (&request->assistant);
    CsvImportInfo *info = assistant_widget ?
        g_object_get_data (G_OBJECT (assistant_widget),
                           "gnc-csv-account-import-info") : NULL;

    if (applied && info && request->book == gnc_get_current_book () &&
        !qof_book_shutting_down (request->book))
    {
        info->new_book = FALSE;
        csv_import_finish_import (GNC_IMPORT_ASSISTANT (assistant_widget), info);
    }
    g_clear_object (&assistant_widget);
    csv_import_new_book_request_free (request);
    (void)parent;
}

void
csv_import_assistant_finish (GncImportAssistant *assistant, gpointer user_data)
{
    CsvImportInfo *info = user_data;

    if (info->new_book)
    {
        CsvImportNewBookRequest *request = g_new0 (CsvImportNewBookRequest, 1);

        g_weak_ref_init (&request->assistant, GTK_WIDGET (assistant));
        request->book = gnc_get_current_book ();
        gnc_new_book_option_display_async (GTK_WIDGET (assistant),
                                           csv_import_new_book_options_finished,
                                           request);
        return;
    }
    csv_import_finish_import (assistant, info);
}

static void
csv_import_close_handler (gpointer user_data)
{
    CsvImportInfo *info = user_data;

    g_free (info->starting_dir);
    g_free (info->file_name);
    g_string_free (info->regexp, TRUE);
    g_object_unref (info->store);

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->assistant));
    gtk_window_destroy (GTK_WINDOW(info->assistant));
}

/*******************************************************
 * Create the Assistant
 *******************************************************/
static GtkWidget *
csv_import_assistant_create (CsvImportInfo *info)
{
    GtkBuilder *builder;
    GtkNoSelection *selection;
    GtkScrolledWindow *preview_scrolledwindow;
    gchar *mnemonic_desc = NULL;

    builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder, "assistant-csv-account-import.glade", "num_hrows_adj");
    gnc_builder_add_from_file  (builder, "assistant-csv-account-import.glade", "csv_account_import_assistant");
    info->assistant = GTK_WIDGET(gtk_builder_get_object (builder, "csv_account_import_assistant"));
    GncImportAssistant *assistant = gnc_import_assistant_new (
        GTK_WINDOW (info->assistant),
        GTK_STACK (gtk_builder_get_object (builder, "gnc_import_assistant_stack")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_page_title")),
        GTK_BOX (gtk_builder_get_object (builder, "gnc_import_assistant_actions")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_back")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_next")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_apply")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_cancel")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_close")));
    if (!assistant)
    {
        g_object_unref (builder);
        return NULL;
    }

    // Set the name for this assistant so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(info->assistant), "gnc-id-assistant-csv-account-import");
    gnc_widget_style_context_add_class (GTK_WIDGET(info->assistant), "gnc-class-imports");

    /* Load default settings */
    load_settings (info);

    /* Enable buttons on all page. */
    gnc_import_assistant_set_page_complete (GNC_IMPORT_ASSISTANT(info->assistant),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "start_page")),
                                     TRUE);
    gnc_import_assistant_set_page_complete (GNC_IMPORT_ASSISTANT(info->assistant),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "file_page")),
                                     FALSE);
    gnc_import_assistant_set_page_complete (GNC_IMPORT_ASSISTANT(info->assistant),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "import_tree_page")),
                                     TRUE);
    gnc_import_assistant_set_page_complete (GNC_IMPORT_ASSISTANT(info->assistant),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "end_page")),
                                     FALSE);
    gnc_import_assistant_set_page_complete (GNC_IMPORT_ASSISTANT(info->assistant),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "summary_page")),
                                     TRUE);

    /* Start Page */

    /* File selection page */
    info->file_page = GTK_WIDGET(gtk_builder_get_object(builder, "file_page"));
    info->file_button = gtk_button_new_with_label (_("Choose File…"));
    gtk_widget_set_halign (info->file_button, GTK_ALIGN_START);
    g_signal_connect (info->file_button, "clicked",
                      G_CALLBACK (csv_import_select_file_cb), info);
    gtk_box_append (GTK_BOX (info->file_page), info->file_button);

    /* Account Tree Page */
    info->account_page = GTK_WIDGET(gtk_builder_get_object(builder, "import_tree_page"));
    info->header_row_spin = GTK_WIDGET(gtk_builder_get_object (builder, "num_hrows"));
    preview_scrolledwindow = GTK_SCROLLED_WINDOW (gtk_builder_get_object (builder,
                                                   "scroll_window"));

    /* Comma Separated file default */
    info->regexp = g_string_new ("");
    create_regex (info->regexp, ",");

    /* The preview shares the parsed GTK4 row model with the importer. */
    info->store = g_list_store_new (G_TYPE_OBJECT);
    selection = gtk_no_selection_new (G_LIST_MODEL (g_object_ref (info->store)));
    info->preview_view = GTK_COLUMN_VIEW (gtk_column_view_new (GTK_SELECTION_MODEL (selection)));
#define CREATE_COLUMN(description,column_id) \
  mnemonic_desc = mnemonic_escape (_(description)); \
  csv_import_preview_add_column (info->preview_view, mnemonic_desc, column_id); \
  g_free (mnemonic_desc);
    CREATE_COLUMN ("Type", TYPE);
    CREATE_COLUMN ("Account Full Name", FULL_NAME);
    CREATE_COLUMN ("Account Name", NAME);
    CREATE_COLUMN ("Account Code", CODE);
    CREATE_COLUMN ("Description", DESCRIPTION);
    CREATE_COLUMN ("Account Color", COLOR);
    CREATE_COLUMN ("Notes", NOTES);
    CREATE_COLUMN ("Symbol", SYMBOL);
    CREATE_COLUMN ("Namespace", NAMESPACE);
    CREATE_COLUMN ("Hidden", HIDDEN);
    CREATE_COLUMN ("Tax Info", TAX);
    CREATE_COLUMN ("Placeholder", PLACE_HOLDER);
#undef CREATE_COLUMN
    gtk_scrolled_window_set_child (preview_scrolledwindow, GTK_WIDGET (info->preview_view));

    /* Finish Page */
    info->finish_label = GTK_WIDGET(gtk_builder_get_object (builder, "end_page"));
    /* Summary Page */
    info->summary_label = GTK_WIDGET(gtk_builder_get_object (builder, "summary_label"));
    info->summary_error_view = GTK_WIDGET(gtk_builder_get_object (builder, "summary_error_view"));

    g_object_set_data (G_OBJECT (info->assistant), "gnc-csv-account-import-info",
                       info);
    g_signal_connect (G_OBJECT(info->assistant), "destroy",
                      G_CALLBACK(csv_import_assistant_destroy_cb), info);

    gnc_restore_window_size (GNC_PREFS_GROUP,
                             GTK_WINDOW(info->assistant), gnc_ui_get_main_window(NULL));

gnc_builder_connect_signals (builder, info);
    gnc_import_assistant_set_page_action (assistant, 3,
                                          GNC_IMPORT_ASSISTANT_PAGE_APPLY);
    gnc_import_assistant_set_page_action (assistant, 4,
                                          GNC_IMPORT_ASSISTANT_PAGE_CLOSE);
    gnc_import_assistant_set_callbacks (assistant, csv_import_assistant_prepare,
                                        csv_import_assistant_finish,
                                        csv_import_assistant_cancel,
                                        csv_import_assistant_close, info);
    g_object_unref (G_OBJECT(builder));
    return info->assistant;
}


/********************************************************************\
 * gnc_file_csv_account_import                                      *
 * opens up a assistant to import accounts.                         *
 *                                                                  *
 * Args:   import_type                                              *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_file_csv_account_import(void)
{
    CsvImportInfo *info;

    info = g_new0 (CsvImportInfo, 1);

    /* In order to trigger a book options display on the creation of a new book,
     * we need to detect when we are dealing with a new book. */
    info->new_book = gnc_is_new_book();

    csv_import_assistant_create (info);

    gnc_register_gui_component (ASSISTANT_CSV_IMPORT_CM_CLASS,
                                NULL, csv_import_close_handler,
                                info);

    gnc_window_adjust_for_screen (GTK_WINDOW(info->assistant));
    gtk_window_present (GTK_WINDOW (info->assistant));
}
