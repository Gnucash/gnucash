/*******************************************************************\
 * assistant-csv-fixed-trans-import.c -- An assistant for importing *
 *                                       set format transction file.*
 *                                                                  *
 * Copyright (C) 2014 Robert Fewell                                 *
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
/** @file assistant-csv-fixed-trans-import.c
    @brief CSV Import Assistant
    @author Copyright (c) 2012 Robert Fewell
*/
#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-ui.h"
#include "gnc-uri-utils.h"
#include "gnc-ui-util.h"

#include "gnc-component-manager.h"

#include "assistant-utils.h"
#include "assistant-csv-fixed-trans-import.h"
#include "csv-fixed-trans-import.h"
#include "gnc-csv-model.h"

#define GNC_PREFS_GROUP "dialogs.import.csv"
#define ASSISTANT_CSV_FIXED_TRANS_IMPORT_CM_CLASS "assistant-csv-fixed-trans-import"

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

/*************************************************************************/

void csv_fixed_trans_import_assistant_prepare (GtkAssistant *assistant, GtkWidget *page, gpointer user_data);
void csv_fixed_trans_import_assistant_finish (GtkAssistant *gtkassistant, gpointer user_data);
void csv_fixed_trans_import_assistant_cancel (GtkAssistant *gtkassistant, gpointer user_data);
void csv_fixed_trans_import_assistant_close (GtkAssistant *gtkassistant, gpointer user_data);

void csv_fixed_trans_import_start_page_prepare (GtkAssistant *gtkassistant, gpointer user_data);
void csv_fixed_trans_import_file_page_prepare (GtkAssistant *assistant, gpointer user_data);
void csv_fixed_trans_import_view_page_prepare (GtkAssistant *gtkassistant, gpointer user_data);
void csv_fixed_trans_import_finish_page_prepare (GtkAssistant *assistant, gpointer user_data);
void csv_fixed_trans_import_summary_page_prepare (GtkAssistant *assistant, gpointer user_data);

void csv_fixed_trans_import_sep_cb (GtkWidget *radio, gpointer user_data );
void csv_fixed_trans_import_hrows_cb (GtkWidget *spin, gpointer user_data );

void csv_fixed_trans_import_file_chooser_confirm_cb (GtkWidget *button, CsvFTImportInfo *info);

static gchar *gnc_input_dialog (GtkWidget *parent, const gchar *title, const gchar *msg, const gchar *default_input);

static const gchar *finish_tree_string = N_(
            "The transactions will be imported from the file '%s' when you click 'Apply'.\n\n"
            "This operation is not reversable, so make sure you have a working backup.\n\n"
            "You can verify your selections by clicking on 'Back' or 'Cancel' to Abort Import.\n");

static const gchar *new_book_finish_tree_string = N_(
            "The transactions will be imported from the file '%s' when you click 'Apply'.\n\n"
            "This operation is not reversable, so please make sure you have a working backup.\n\n"
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

static
void create_regex (GString *regex_str, const gchar *sep)
{
    if (!sep) return;

    g_string_printf (regex_str,
            "\\G(?<date>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<type>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<sdate>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<acct_name>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<number>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<description>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<notes>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<memo>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<full_cat_name>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<cat_name>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<row_type>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<action>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<reconcile>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<amount_with_sym>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<commoditym>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<commodityn>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<amount_num>\"(?:[^\"]|\"\")*\"|[^%s]*)%s"
            "(?<rate>\"(?:[^\"]|\"\")*\"|[^%s[:cntrl:]]*)(?:\\R*)",
            sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep,
            sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep, sep);
}

/*************************************************************************/

/**************************************************
 * csv_fixed_trans_import_file_chooser_confirm_cb
 *
 * call back for ok button in file chooser widget
 **************************************************/
void
csv_fixed_trans_import_file_chooser_confirm_cb (GtkWidget *button, CsvFTImportInfo *info)
{
    GtkAssistant *assistant = GTK_ASSISTANT(info->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    gchar *file_name;
    csv_fixed_trans_import_result res;

    gtk_assistant_set_page_complete (assistant, page, FALSE);

    file_name = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER(info->file_chooser));

    if (file_name)
    {
        gchar *filepath = gnc_uri_get_path (file_name);
        gchar *filedir = g_path_get_dirname (filepath);
        info->starting_dir = g_strdup (filedir);
        g_free (filedir);
        g_free (filepath);

        info->file_name = g_strdup (file_name);

        // generate preview
        gtk_list_store_clear (info->store);
        res = csv_fixed_trans_import_read_file (info->file_name, info->regexp->str, info->store, 1 );
        if (res == RESULT_OPEN_FAILED)
            gnc_error_dialog (info->window, _("The input file can not be opened."));
        else if (res == RESULT_OK)
            gtk_assistant_set_page_complete (assistant, page, TRUE);
        else if (res == MATCH_FOUND)
            gtk_assistant_set_page_complete (assistant, page, TRUE);
    }
    g_free (file_name);

    DEBUG("file_name selected is %s", info->file_name);
    DEBUG("starting directory is %s", info->starting_dir);

    /* Step to next page if page is complete */
    if(gtk_assistant_get_page_complete (assistant, page))
        gtk_assistant_set_current_page (assistant, num + 1);

}


/*******************************************************
 * csv_fxed_trans_import_hrows_cb
 *
 * call back for the start row / number of header rows
 *******************************************************/
void csv_fixed_trans_import_hrows_cb (GtkWidget *spin, gpointer user_data)
{
    CsvFTImportInfo *info = user_data;

    GtkTreeIter iter;
    gboolean valid;
    int num_rows;

    /* Get number of rows for header */
    info->header_rows = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spin));

    /* Get number of rows displayed */
    num_rows = gtk_tree_model_iter_n_children (GTK_TREE_MODEL(info->store), NULL);

    /* Modify background color for header rows */
    if (info->header_rows == 0)
    {
        valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(info->store), &iter, NULL, 0 );
        if (valid)
            gtk_list_store_set (info->store, &iter, FTROW_COLOR, NULL, -1);
    }
    else
    {
        if (info->header_rows - 1 < num_rows)
        {
            valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(info->store), &iter, NULL, info->header_rows - 1 );
            if (valid)
                gtk_list_store_set (info->store, &iter, FTROW_COLOR, "pink", -1);
            valid = gtk_tree_model_iter_next (GTK_TREE_MODEL(info->store), &iter);
            if (valid)
                gtk_list_store_set (info->store, &iter, FTROW_COLOR, NULL, -1);
        }
    }
}


/*******************************************************
 * csv_fixed_trans_import_sep_cb
 *
 * call back for type of separartor required
 *******************************************************/
void csv_fixed_trans_import_sep_cb (GtkWidget *radio, gpointer user_data)
{
    CsvFTImportInfo *info = user_data;
    const gchar *name;
    gchar *temp;
    gchar *sep = NULL;

    if (!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(radio)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }

    name = gtk_buildable_get_name (GTK_BUILDABLE(radio));
    if (g_strcmp0 (name, "radio_semi") == 0)
        sep = ";";
    else if (g_strcmp0 (name, "radio_colon") == 0)
        sep = ":";
    else
        sep = ","; /* Use as default as well */

    create_regex (info->regexp, sep);

    if (g_strcmp0 (name, "radio_custom") == 0)
    {
        temp = gnc_input_dialog (0, _("Adjust regular expression used for import"), _("This regular expression is used to parse the import file. Modify according to your needs.\n"), info->regexp->str);
        if (temp)
        {
            g_string_assign (info->regexp, temp);
            g_free (temp);
        }
    }

    /* Generate preview */
    gtk_list_store_clear (info->store);

    gtk_tree_view_columns_autosize (GTK_TREE_VIEW(info->tree_view));

    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(info->tree_view), GTK_TREE_VIEW_GRID_LINES_BOTH);

    if (csv_fixed_trans_import_read_file (info->file_name, info->regexp->str, info->store, 11) == MATCH_FOUND)
        gtk_widget_set_sensitive (info->header_row_spin, TRUE);
    else
        gtk_widget_set_sensitive (info->header_row_spin, FALSE);

    /* Reset Header spin to 1 */
    gtk_spin_button_set_value (GTK_SPIN_BUTTON(info->header_row_spin), 1);
    csv_fixed_trans_import_hrows_cb (info->header_row_spin, info);
}


/*******************************************************
 * date_format_selected
 *
 * Event handler for selecting a new date format.
 *******************************************************/
static void date_format_selected (GtkComboBoxText* format_selector, CsvFTImportInfo *info)
{
    info->date_format = gtk_combo_box_get_active (GTK_COMBO_BOX(format_selector));
}


/*******************************************************
 * currency_format_selected
 *
 * Event handler for selecting a new currency format.
 *******************************************************/
static void currency_format_selected (GtkComboBoxText* currency_selector, CsvFTImportInfo *info)
{
    info->currency_format = gtk_combo_box_get_active (GTK_COMBO_BOX(currency_selector));
}


/*******************************************************
 * load_settings
 *
 * load the default settings for the assistant
 *******************************************************/
static void
load_settings (CsvFTImportInfo *info)
{
    info->header_rows = 0;
    info->error = "";
    info->starting_dir = NULL;
    info->file_name = NULL;
    info->date_format = 0;
    info->currency_format = 0;

    /* The default directory for the user to select files. */
    info->starting_dir = gnc_get_default_directory (GNC_PREFS_GROUP);
}


/* =============================================================== */


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
    dialog = gtk_dialog_new_with_buttons (title, GTK_WINDOW(parent),
                                          GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                          GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
                                          GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
                                          NULL);

    content_area = gtk_dialog_get_content_area (GTK_DIALOG(dialog));

    // add a label
    label = gtk_label_new (msg);
    gtk_container_add (GTK_CONTAINER(content_area), label);

    // add a textview
    view = gtk_text_view_new ();
    gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW(view), GTK_WRAP_WORD_CHAR);
    buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(view));
    gtk_text_buffer_set_text (buffer, default_input, -1);
    gtk_container_add (GTK_CONTAINER(content_area), view);

    // run the dialog
    gtk_widget_show_all (dialog);
    result = gtk_dialog_run (GTK_DIALOG(dialog));

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


/* =============================================================== */


/*******************************************************
 * Assistant page prepare functions
 *******************************************************/
void
csv_fixed_trans_import_start_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    /* Enable the Assistant Buttons */
    gtk_assistant_set_page_complete (assistant, page, TRUE);
}


void
csv_fixed_trans_import_file_page_prepare (GtkAssistant *assistant,
                                        gpointer user_data)
{
    CsvFTImportInfo *info = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    info->header_rows = 0;

    /* Set the default directory */
    if (info->starting_dir)
        gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER(info->file_chooser), info->starting_dir);

    /* Disable the Forward Assistant Button */
    gtk_assistant_set_page_complete (assistant, page, FALSE);
}


void
csv_fixed_trans_import_view_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvFTImportInfo *info = user_data;

    gtk_list_store_clear (info->store);

    if (csv_fixed_trans_import_read_file (info->file_name, info->regexp->str, info->store, 11 ) == MATCH_FOUND)
    {
        /* Reset Header spin to 1 */
        if (info->header_rows != 1)
            gtk_spin_button_set_value (GTK_SPIN_BUTTON(info->header_row_spin), 1);
        csv_fixed_trans_import_hrows_cb (info->header_row_spin, info);
        gtk_widget_set_sensitive (info->header_row_spin, TRUE);
    }
    else
        gtk_widget_set_sensitive (info->header_row_spin, FALSE);
}


void
csv_fixed_trans_import_finish_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvFTImportInfo *info = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);
    gchar *text;
    gboolean test_ok = FALSE;

    /* Set Finish page text */
    if (csv_fixed_trans_test_one_line (info))
    {
        /* Before creating accounts, if this is a new book, tell user they can
         * specify book options, since they affect how transactions are created */
        if (info->new_book)
            text = g_strdup_printf (gettext (new_book_finish_tree_string), info->file_name);
        else
            text = g_strdup_printf (gettext (finish_tree_string), info->file_name);
        test_ok = TRUE;
    }
    else
        text = g_strdup (_("Date or Number format is incorrect, press 'back' to correct"));

    gtk_label_set_text (GTK_LABEL(info->finish_label), text);
    g_free (text);

    /* Save the Window size and directory */
    gnc_set_default_directory (GNC_PREFS_GROUP, info->starting_dir);

    /* Enable the Assistant Buttons */
    gtk_assistant_set_page_complete (assistant, page, test_ok);
}


void
csv_fixed_trans_import_summary_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvFTImportInfo *info = user_data;
    gchar *text, *errtext, *mtext;

    /* Before creating accounts, if this is a new book, let user specify
     * book options, since they affect how transactions are created */
    if (info->new_book)
        info->new_book = gnc_new_book_option_display (info->window);

    if (!g_strcmp0 (info->error, "") == 0)
    {
        GtkTextBuffer *buffer;

        buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(info->summary_error_view));
        text = g_strdup_printf (gettext ("Import completed but with warnings!\n\nThe number of Transactions added was %u and "
                                        "%u were duplicates.\n\nSee below for warnings..."), info->num_new, info->num_duplicates);
        errtext = g_strdup_printf ("%s", info->error);
        gtk_text_buffer_set_text (buffer, errtext, -1);
        g_free (errtext);
        g_free (info->error);
    }
    else
        text = g_strdup_printf (gettext ("Import completed successfully!\n\nThe number of Transactions added was %u and "
                                        "%u were duplicates.\n"), info->num_new, info->num_duplicates);

    mtext = g_strdup_printf ("<span size=\"medium\"><b>%s</b></span>", text);
    gtk_label_set_markup (GTK_LABEL(info->summary_label), mtext);

    g_free (text);
    g_free (mtext);
}


void
csv_fixed_trans_import_assistant_prepare (GtkAssistant *assistant, GtkWidget *page,
                              gpointer user_data)
{
    gint currentpage = gtk_assistant_get_current_page (assistant);

    switch (currentpage)
    {
    case 0:
        /* Current page is Import Start page */
        csv_fixed_trans_import_start_page_prepare (assistant, user_data);
        break;
    case 1:
        /* Current page is File select page */
        csv_fixed_trans_import_file_page_prepare (assistant, user_data);
        break;
    case 2:
        /* Current page is Inmport View page */
        csv_fixed_trans_import_view_page_prepare (assistant, user_data);
        break;
    case 3:
        /* Current page is Finish page */
        csv_fixed_trans_import_finish_page_prepare (assistant, user_data);
        break;
    case 4:
        /* Current page is Summary page */
        csv_fixed_trans_import_summary_page_prepare (assistant, user_data);
        break;
    }
}


/*******************************************************
 * Assistant call back functions
 *******************************************************/
static void
csv_fixed_trans_import_assistant_destroy_cb (GtkObject *object, gpointer user_data)
{
    CsvFTImportInfo *info = user_data;
    gnc_unregister_gui_component_by_data (ASSISTANT_CSV_FIXED_TRANS_IMPORT_CM_CLASS, info);
    g_free (info);
}

void
csv_fixed_trans_import_assistant_cancel (GtkAssistant *assistant, gpointer user_data)
{
    CsvFTImportInfo *info = user_data;
    gnc_close_gui_component_by_data (ASSISTANT_CSV_FIXED_TRANS_IMPORT_CM_CLASS, info);
}

void
csv_fixed_trans_import_assistant_close (GtkAssistant *assistant, gpointer user_data)
{
    CsvFTImportInfo *info = user_data;
    gnc_close_gui_component_by_data (ASSISTANT_CSV_FIXED_TRANS_IMPORT_CM_CLASS, info);
}

void
csv_fixed_trans_import_assistant_finish (GtkAssistant *assistant, gpointer user_data)
{
    CsvFTImportInfo *info = user_data;

    gtk_list_store_clear (info->store);
    csv_fixed_trans_import_read_file (info->file_name, info->regexp->str, info->store, 0 );
    csv_fixed_trans_import (info);
}

static void
csv_fixed_trans_import_close_handler (gpointer user_data)
{
    CsvFTImportInfo *info = user_data;

    g_free (info->starting_dir);
    g_free (info->file_name);
    g_string_free (info->regexp, TRUE);

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->window));
    gtk_widget_destroy (info->window);
}

/*******************************************************
 * Create the Assistant
 *******************************************************/
static GtkWidget *
csv_fixed_trans_import_assistant_create (CsvFTImportInfo *info)
{
    GtkBuilder *builder;
    GtkWidget *window;
    GtkWidget *box, *h_box;
    GtkWidget *button;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    gchar *mnemonic_desc = NULL;
    gint i;
    GtkContainer *date_format_container, *currency_format_container;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "assistant-csv-fixed-trans-import.glade", "num_hrows_adj");
    gnc_builder_add_from_file (builder, "assistant-csv-fixed-trans-import.glade", "CSV Fixed Transaction Import Assistant");
    window = GTK_WIDGET(gtk_builder_get_object (builder, "CSV Fixed Transaction Import Assistant"));
    info->window = window;

    /* Set the assistant colors */
    gnc_assistant_set_colors (GTK_ASSISTANT (info->window));

    /* Load default settings */
    load_settings (info);

    /* Enable buttons on all page. */
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "start_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "file_page")),
                                     FALSE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "import_view_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "finish_page")),
                                     FALSE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "summary_page")),
                                     TRUE);

    /* Start Page */

    /* File chooser Page */
    info->file_chooser = gtk_file_chooser_widget_new (GTK_FILE_CHOOSER_ACTION_OPEN);
    g_signal_connect (G_OBJECT(info->file_chooser), "file-activated",
                      G_CALLBACK(csv_fixed_trans_import_file_chooser_confirm_cb), info);
    button = gtk_button_new_from_stock (GTK_STOCK_OK);
    gtk_widget_set_size_request (button, 100, -1);
    gtk_widget_show (button);
    h_box = gtk_hbox_new (TRUE, 0);
    gtk_box_pack_start (GTK_BOX(h_box), button, FALSE, FALSE, 0);
    gtk_file_chooser_set_extra_widget (GTK_FILE_CHOOSER(info->file_chooser), h_box);
    g_signal_connect (G_OBJECT(button), "clicked",
                      G_CALLBACK(csv_fixed_trans_import_file_chooser_confirm_cb), info);

    box = GTK_WIDGET(gtk_builder_get_object(builder, "file_page"));
    gtk_box_pack_start (GTK_BOX(box), info->file_chooser, TRUE, TRUE, 6);
    gtk_widget_show (info->file_chooser);

    /* Preview Page */
    info->header_row_spin = GTK_WIDGET(gtk_builder_get_object (builder, "num_hrows"));
    info->tree_view = GTK_WIDGET(gtk_builder_get_object (builder, "treeview"));

    /* Add in the date format combo box and hook it up to an event handler. */
    info->date_format_combo = GTK_COMBO_BOX_TEXT(gtk_combo_box_text_new());
    for (i = 0; i < num_date_formats; i++)
    {
        gtk_combo_box_text_append_text (info->date_format_combo, _(date_format_user[i]));
    }
    gtk_combo_box_set_active (GTK_COMBO_BOX(info->date_format_combo), 0);
    g_signal_connect (G_OBJECT(info->date_format_combo), "changed",
                      G_CALLBACK(date_format_selected), (gpointer)info);

    /* Add it to the assistant. */
    date_format_container = GTK_CONTAINER(gtk_builder_get_object (builder, "date_format_container"));
    gtk_container_add (date_format_container, GTK_WIDGET(info->date_format_combo));
    gtk_widget_show_all (GTK_WIDGET(date_format_container));

    /* Add in the currency format combo box and hook it up to an event handler. */
    info->currency_format_combo = GTK_COMBO_BOX_TEXT(gtk_combo_box_text_new());
    for (i = 0; i < num_currency_formats; i++)
    {
        gtk_combo_box_text_append_text (info->currency_format_combo, _(currency_format_user[i]));
    }
    /* Default will the locale */
    gtk_combo_box_set_active (GTK_COMBO_BOX(info->currency_format_combo), 0);
    g_signal_connect (G_OBJECT(info->currency_format_combo), "changed",
                      G_CALLBACK(currency_format_selected), (gpointer)info);

    /* Add it to the assistant. */
    currency_format_container = GTK_CONTAINER(gtk_builder_get_object (builder, "currency_format_container"));
    gtk_container_add (currency_format_container, GTK_WIDGET(info->currency_format_combo));
    gtk_widget_show_all (GTK_WIDGET(currency_format_container));

    /* Comma Separated file default */
    info->regexp = g_string_new ("");
    create_regex (info->regexp, ",");

    /* create model and bind to view */
    info->store = gtk_list_store_new (FTN_COLUMNS,
                                      G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
                                      G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
                                      G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
                                      G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);

    gtk_tree_view_set_model (GTK_TREE_VIEW(info->tree_view), GTK_TREE_MODEL(info->store));

#define CREATE_COLUMN(description,column_id) \
  renderer = gtk_cell_renderer_text_new (); \
  mnemonic_desc = mnemonic_escape (_(description)); \
  column = gtk_tree_view_column_new_with_attributes (mnemonic_desc, renderer, "text", column_id, NULL); \
  gtk_tree_view_column_add_attribute (column, renderer, "background", FTROW_COLOR); \
  gtk_tree_view_column_set_resizable (column, TRUE); \
  gtk_tree_view_append_column (GTK_TREE_VIEW(info->tree_view), column); \
  g_free (mnemonic_desc);
    CREATE_COLUMN ("date", FTDATE);
    CREATE_COLUMN ("type", FTTYPE);
    CREATE_COLUMN ("sdate", FTSDATE);
    CREATE_COLUMN ("acct_name", FTACCT_NAME);
    CREATE_COLUMN ("number", FTNUMBER);
    CREATE_COLUMN ("description", FTDESCRIPTION);
    CREATE_COLUMN ("notes", FTNOTES);
    CREATE_COLUMN ("memo", FTMEMO);
    CREATE_COLUMN ("full_cat_name", FTFULL_CAT_NAME);
    CREATE_COLUMN ("cat_name", FTCAT_NAME);
    CREATE_COLUMN ("row_type", FTRTYPE);
    CREATE_COLUMN ("action", FTACTION);
    CREATE_COLUMN ("reconcile", FTRECONCILE);
    CREATE_COLUMN ("amount_with_sym", FTAMOUNT_WITH_SYM);
    CREATE_COLUMN ("commoditym", FTCOMMODITYM);
    CREATE_COLUMN ("commodityn", FTCOMMODITYN);
    CREATE_COLUMN ("amount_num", FTAMOUNT_NUM);
    CREATE_COLUMN ("rate", FTRATE);

    // Enable Horizontal and vertical grid lines
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(info->tree_view), GTK_TREE_VIEW_GRID_LINES_BOTH);

    /* Finish Page */
    info->finish_label = GTK_WIDGET(gtk_builder_get_object (builder, "finish_label"));
    info->progressbar = GTK_WIDGET(gtk_builder_get_object (builder, "progressbar"));

    /* Summary Page */
    info->summary_label = GTK_WIDGET(gtk_builder_get_object (builder, "summary_label"));
    info->summary_error_view = GTK_WIDGET(gtk_builder_get_object (builder, "summary_error_view"));

    g_signal_connect (G_OBJECT(window), "destroy",
                      G_CALLBACK(csv_fixed_trans_import_assistant_destroy_cb), info);

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->window));

    gtk_builder_connect_signals (builder, info);
    g_object_unref (G_OBJECT(builder));
    return window;
}


/********************************************************************\
 * gnc_file_csv_account_import                                      *
 * opens up a assistant to import accounts.                         *
 *                                                                  *
 * Args:   import_type                                              *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_file_csv_fixed_trans_import(void)
{
    CsvFTImportInfo *info;

    info = g_new0 (CsvFTImportInfo, 1);

    /* In order to trigger a book options display on the creation of a new book,
     * we need to detect when we are dealing with a new book. */
    info->new_book = gnc_is_new_book();

    csv_fixed_trans_import_assistant_create (info);

    gnc_register_gui_component (ASSISTANT_CSV_FIXED_TRANS_IMPORT_CM_CLASS,
                                NULL, csv_fixed_trans_import_close_handler,
                                info);

    gtk_widget_show_all (info->window);

    gnc_window_adjust_for_screen (GTK_WINDOW(info->window));
}

