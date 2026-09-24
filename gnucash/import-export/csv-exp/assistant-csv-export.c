/*******************************************************************\
 * assistant-csv-export.c -- An assistant for exporting Accounts    *
 *                            and Transactions to a file            *
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
/** @file assistant-csv-export.c
    @brief CSV Export Assistant
    @author Copyright (c) 2012 Robert Fewell
*/
#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-ui.h"
#include "gnc-uri-utils.h"
#include "gnc-component-manager.h"
#include "gnc-ui-util.h"
#include "gnc-date-edit.h"
#include "gnc-import-assistant.h"
#include "gnc-prefs.h"
#include "dialog-utils.h"
#include "gnc-file.h"
#include "Query.h"
#include "Transaction.h"

#include "assistant-csv-export.h"
#include "csv-tree-export.h"
#include "csv-transactions-export.h"

#define GNC_PREFS_GROUP               "dialogs.export.csv"
#define GNC_PREF_PANED_POS            "paned-position"
#define ASSISTANT_CSV_EXPORT_CM_CLASS "assistant-csv-export"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

/*************************************************************************/

static void csv_export_assistant_prepare (GncImportAssistant *assistant,
                                          GtkWidget *page, gpointer user_data);
static void csv_export_assistant_finish (GncImportAssistant *assistant,
                                         gpointer user_data);
static void csv_export_assistant_cancel (GncImportAssistant *assistant,
                                         gpointer user_data);
static void csv_export_assistant_close (GncImportAssistant *assistant,
                                        gpointer user_data);

static void csv_export_assistant_start_page_prepare (GncImportAssistant *assistant,
                                                     gpointer user_data);
static void csv_export_assistant_account_page_prepare (GncImportAssistant *assistant,
                                                       gpointer user_data);
static void csv_export_assistant_file_page_prepare (GncImportAssistant *assistant,
                                                    gpointer user_data);
static void csv_export_assistant_finish_page_prepare (GncImportAssistant *assistant,
                                                      gpointer user_data);
static void csv_export_assistant_summary_page_prepare (GncImportAssistant *assistant,
                                                       gpointer user_data);

void csv_export_quote_cb (GtkCheckButton *button, gpointer user_data);
void csv_export_simple_cb (GtkCheckButton *button, gpointer user_data);
void csv_export_sep_cb (GtkWidget *radio, gpointer user_data);
void csv_export_custom_entry_cb (GtkWidget *widget, gpointer user_data);

void csv_export_show_range_cb (GtkCheckButton *button, gpointer user_data);
void csv_export_start_date_cb (GtkWidget *radio, gpointer user_data);
void csv_export_end_date_cb (GtkWidget *radio, gpointer user_data);


static const gchar *start_tree_string = N_(
            "This assistant will help you export the Account Tree to a file "
            "with the separator specified below.\n\n"
            "Select the settings you require for the file and then click \"Next\" "
            "to proceed or \"Cancel\" to abort the export.\n");

static const gchar *start_trans_common_string = N_(
            /* Translators: %s is one of the following paragraphs about rows/transaction. */
            "This assistant will help you export the Transactions to a file "
            "with the separator specified below.\n\n"
            "%s\n\n"
            "While a transaction may have splits in several of the selected accounts "
            "it will only be exported once. It will appear under the first processed "
            "account it has a split in.\n\n"
            "The Price/Rate output format is controlled by the preference\n"
            "\"Numbers, Date, Time\"->\"Force Prices to display as decimals\".\n\n"
            "Select the settings you require for the file and then click \"Next\" "
            "to proceed or \"Cancel\" to abort the export.\n");

static const gchar *start_trans_multi_string = N_(
            "There will be multiple rows for each transaction with each row "
            "representing one split.");

static const gchar *start_trans_simple_string = N_(
            "There will be one row for each transaction, equivalent to a single row "
            "in a register in 'Basic Ledger' mode. As such some transfer detail "
            "could be lost.");

static const gchar *finish_tree_string = N_(
            /* Translators: %s is the file name. */
            "The account tree will be exported to the file '%s' when you click \"Apply\".\n\n"
            "You can also verify your selections by clicking on \"Back\" or \"Cancel\" to abort the export.\n");

static const gchar *finish_trans_string = N_(
            /* Translators: %s is the file name and %u the number of accounts. */
            "When you click \"Apply\", the transactions will be exported to the file '%s' "
            "and the number of accounts exported will be %u.\n\n"
            "You can also verify your selections by clicking on \"Back\" or \"Cancel\" to abort the export.\n");

static const gchar *finish_trans_search_gl_string = N_(
            /* Translators: %s is the file name. */
            "When you click \"Apply\", the transactions will be exported to the file '%s'.\n\n"
            "You can also verify your selections by clicking on \"Back\" or \"Cancel\" to abort the export.\n");


#define CSV_EXPORT_INFO_DATA_KEY "gnc-csv-export-info"

typedef struct
{
    GWeakRef assistant;
} CsvExportFileDialogData;

typedef struct
{
    GWeakRef assistant;
} CsvExportOverwriteRequest;

static void
csv_export_file_dialog_data_free (CsvExportFileDialogData *data)
{
    g_weak_ref_clear (&data->assistant);
    g_free (data);
}

static void
csv_export_overwrite_request_free (CsvExportOverwriteRequest *request)
{
    g_weak_ref_clear (&request->assistant);
    g_free (request);
}

static void
csv_export_overwrite_finished (GtkWindow *parent, gint response,
                               gpointer user_data)
{
    CsvExportOverwriteRequest *request = user_data;
    GtkWidget *assistant_widget = g_weak_ref_get (&request->assistant);
    CsvExportInfo *info = assistant_widget ?
        g_object_get_data (G_OBJECT (assistant_widget), CSV_EXPORT_INFO_DATA_KEY) : NULL;

    if (info && gnc_import_assistant_get_current_page (
            GNC_IMPORT_ASSISTANT (assistant_widget)) == 3)
    {
        if (response == GTK_RESPONSE_YES)
            gnc_import_assistant_set_page_complete (
                GNC_IMPORT_ASSISTANT (assistant_widget), info->finish_label, TRUE);
        else
            gnc_import_assistant_previous_page (
                GNC_IMPORT_ASSISTANT (assistant_widget));
    }

    g_clear_object (&assistant_widget);
    csv_export_overwrite_request_free (request);
    (void)parent;
}

static gboolean
csv_export_assistant_set_filename (CsvExportInfo *info, GFile *file)
{
    gchar *file_name = g_file_get_path (file);
    gchar *filedir;

    if (!file_name || g_file_test (file_name, G_FILE_TEST_IS_DIR))
    {
        g_free (file_name);
        return FALSE;
    }

    filedir = g_path_get_dirname (file_name);
    g_free (info->file_name);
    info->file_name = file_name;
    g_free (info->starting_dir);
    info->starting_dir = filedir;

    DEBUG ("file_name selected is %s", info->file_name);
    DEBUG ("starting directory is %s", info->starting_dir);
    return TRUE;
}

static void
csv_export_file_dialog_finished (GObject *source, GAsyncResult *result,
                                 gpointer user_data)
{
    CsvExportFileDialogData *data = user_data;
    GncFileDialogRequest *request = GNC_FILE_DIALOG_REQUEST (source);
    GError *error = NULL;
    GFile *file;
    GtkWidget *assistant;
    CsvExportInfo *info = NULL;

    file = gnc_file_dialog_request_finish (request, result, &error);
    assistant = g_weak_ref_get (&data->assistant);
    if (assistant)
        info = g_object_get_data (G_OBJECT (assistant),
                                  CSV_EXPORT_INFO_DATA_KEY);

    if (file && info)
    {
        if (csv_export_assistant_set_filename (info, file))
        {
            gtk_label_set_text (GTK_LABEL (info->file_name_label),
                                info->file_name);
            gnc_import_assistant_set_page_complete (
                GNC_IMPORT_ASSISTANT (assistant), info->file_page, TRUE);
        }
        else
        {
            gnc_error_dialog (GTK_WINDOW (assistant), "%s",
                              _("Please select a file, not a folder."));
        }
    }
    else if (info && error &&
             !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
    {
        gnc_error_dialog (GTK_WINDOW (assistant), "%s", error->message);
    }

    g_clear_object (&file);
    g_clear_error (&error);
    g_clear_object (&assistant);
    csv_export_file_dialog_data_free (data);
}

static void
csv_export_choose_file_cb (GtkButton *button, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    CsvExportFileDialogData *data;
    GncFileDialogRequest *request;

    data = g_new0 (CsvExportFileDialogData, 1);
    g_weak_ref_init (&data->assistant, info->assistant);
    request = gnc_file_dialog_request_new (
        GTK_WINDOW (info->assistant), _("Select CSV Export File"), NULL,
        info->starting_dir, GNC_FILE_DIALOG_EXPORT);
    gnc_file_dialog_request_save_async (request, NULL,
                                        csv_export_file_dialog_finished, data);
    g_object_unref (request);

    (void)button;
}
/*******************************************************
 * csv_export_sep_cb
 *
 * call back for type of separator required
 *******************************************************/
void
csv_export_sep_cb (GtkWidget *radio, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT (info->assistant);
    const gchar *name;

    if (!gtk_check_button_get_active (GTK_CHECK_BUTTON(radio)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }

    name = gtk_buildable_get_buildable_id (GTK_BUILDABLE(radio));

    gtk_widget_set_sensitive (info->custom_entry, FALSE);
    info->use_custom = FALSE;
    gnc_import_assistant_set_page_complete (assistant, info->start_page, TRUE);

    if (g_strcmp0 (name, "comma_radio") == 0)
        info->separator_str = ",";
    if (g_strcmp0 (name, "colon_radio") == 0)
        info->separator_str = ":";
    if (g_strcmp0 (name, "semicolon_radio") == 0)
        info->separator_str = ";";

    if (g_strcmp0 (name, "custom_radio") == 0)
    {
        gtk_widget_set_sensitive (info->custom_entry, TRUE);
        info->use_custom = TRUE;
        if (gtk_entry_get_text_length (GTK_ENTRY(info->custom_entry)) == 0)
            gnc_import_assistant_set_page_complete (assistant, info->start_page, FALSE);
    }
}


/*******************************************************
 * csv_export_quote_cb
 *
 * call back for use of quotes
 *******************************************************/
void
csv_export_quote_cb (GtkCheckButton *button, gpointer user_data)
{
    CsvExportInfo *info = user_data;

    if (gtk_check_button_get_active (GTK_CHECK_BUTTON(button)))
        info->use_quotes = TRUE;
    else
        info->use_quotes = FALSE;
}

/*******************************************************
 * csv_export_simple_cb
 *
 * call back for use of simple_layout
 *******************************************************/
void
csv_export_simple_cb (GtkCheckButton *button, gpointer user_data)
{
    CsvExportInfo *info = user_data;

    info->simple_layout = gtk_check_button_get_active (GTK_CHECK_BUTTON(button));

    gchar *msg = NULL;
    if (info->simple_layout)
        msg = g_strdup_printf (_(start_trans_common_string), _(start_trans_simple_string));
    else
        msg = g_strdup_printf (_(start_trans_common_string), _(start_trans_multi_string));

    gtk_label_set_text (GTK_LABEL(info->start_label), msg);
    g_free (msg);
}

/*******************************************************
 * csv_export_custom_entry_cb
 *
 * call back for custom separator
 *******************************************************/
void
csv_export_custom_entry_cb (GtkWidget *widget, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT (info->assistant);
    const gchar *custom_str;

    custom_str = gnc_entry_get_text (GTK_ENTRY(info->custom_entry));
    info->separator_str = strdup (custom_str);

    if (info->use_custom == TRUE && gtk_entry_get_text_length (GTK_ENTRY(info->custom_entry)) == 0)
        gnc_import_assistant_set_page_complete (assistant, info->start_page, FALSE);
    else
        gnc_import_assistant_set_page_complete (assistant, info->start_page, TRUE);
}


/*******************************************************
 * load_settings
 *
 * load the default settings for the assistant
 *******************************************************/
static void
load_settings (CsvExportInfo *info)
{
    info->use_quotes = FALSE;
    info->simple_layout = FALSE;
    info->separator_str = ",";
    info->file_name = NULL;
    info->starting_dir = NULL;

    /* The default directory for the user to select files. */
    info->starting_dir = gnc_get_default_directory (GNC_PREFS_GROUP);
}

/* =============================================================== */

typedef struct
{
    GObject parent_instance;
    Account *account;
    gchar *full_name;
} CsvExportAccountRow;

typedef struct
{
    GObjectClass parent_class;
} CsvExportAccountRowClass;

GType csv_export_account_row_get_type (void);

G_DEFINE_TYPE (CsvExportAccountRow, csv_export_account_row, G_TYPE_OBJECT)

static void
csv_export_account_row_finalize (GObject *object)
{
    CsvExportAccountRow *row = (CsvExportAccountRow *)object;

    g_free (row->full_name);
    G_OBJECT_CLASS (csv_export_account_row_parent_class)->finalize (object);
}

static void
csv_export_account_row_class_init (CsvExportAccountRowClass *klass)
{
    G_OBJECT_CLASS (klass)->finalize = csv_export_account_row_finalize;
}

static void
csv_export_account_row_init (CsvExportAccountRow *row)
{
    (void)row;
}

static CsvExportAccountRow *
csv_export_account_row_new (Account *account)
{
    CsvExportAccountRow *row =
        (CsvExportAccountRow *)g_object_new (csv_export_account_row_get_type (), NULL);

    row->account = account;
    row->full_name = gnc_account_get_full_name (account);
    return row;
}

static void
csv_export_account_factory_setup (GtkSignalListItemFactory *factory,
                                  GtkListItem *list_item,
                                  gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);

    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (list_item, label);
    (void)factory;
    (void)user_data;
}

static void
csv_export_account_factory_bind (GtkSignalListItemFactory *factory,
                                 GtkListItem *list_item,
                                 gpointer user_data)
{
    CsvExportAccountRow *row =
        (CsvExportAccountRow *)gtk_list_item_get_item (list_item);

    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (list_item)),
                        row ? row->full_name : "");
    (void)factory;
    (void)user_data;
}

static gboolean
csv_export_account_type_visible (GNCAccountType type)
{
    return type == ACCT_TYPE_BANK || type == ACCT_TYPE_CASH ||
           type == ACCT_TYPE_CREDIT || type == ACCT_TYPE_ASSET ||
           type == ACCT_TYPE_LIABILITY || type == ACCT_TYPE_STOCK ||
           type == ACCT_TYPE_MUTUAL || type == ACCT_TYPE_INCOME ||
           type == ACCT_TYPE_EXPENSE || type == ACCT_TYPE_EQUITY ||
           type == ACCT_TYPE_RECEIVABLE || type == ACCT_TYPE_PAYABLE ||
           type == ACCT_TYPE_TRADING;
}

static void
csv_export_add_account (Account *account, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    CsvExportAccountRow *row;

    if (!csv_export_account_type_visible (xaccAccountGetType (account)))
        return;
    row = csv_export_account_row_new (account);
    g_list_store_append (info->csva.account_store, row);
    g_object_unref (row);
}

static Account *
csv_export_cursor_account (CsvExportInfo *info)
{
    GListModel *model = G_LIST_MODEL (info->csva.account_store);
    guint n_items = g_list_model_get_n_items (model);
    guint position = info->csva.cursor_position;
    CsvExportAccountRow *row;

    if (position >= n_items || !gtk_selection_model_is_selected (
            GTK_SELECTION_MODEL (info->csva.account_selection), position))
    {
        for (position = 0; position < n_items; ++position)
            if (gtk_selection_model_is_selected (
                    GTK_SELECTION_MODEL (info->csva.account_selection), position))
                break;
    }
    if (position >= n_items)
        return NULL;

    row = (CsvExportAccountRow *)g_list_model_get_item (model, position);
    if (!row)
        return NULL;
    Account *account = row->account;
    g_object_unref (row);
    return account;
}

static void
show_acct_type_accounts (CsvExportInfo *info)
{
    g_list_store_remove_all (info->csva.account_store);
    info->csva.cursor_position = GTK_INVALID_LIST_POSITION;
    gnc_account_foreach_descendant (gnc_get_current_root_account (),
                                    (AccountCb)csv_export_add_account, info);
}

static void
update_accounts_tree (CsvExportInfo *info)
{
    GListModel *model = G_LIST_MODEL (info->csva.account_store);
    guint position;
    guint num_accounts = 0;
    gchar *string;

    for (position = 0; position < g_list_model_get_n_items (model); ++position)
        if (gtk_selection_model_is_selected (GTK_SELECTION_MODEL (
                info->csva.account_selection), position))
            ++num_accounts;

    string = g_strdup_printf (_("Accounts Selected: %u"), num_accounts);
    gtk_label_set_text (GTK_LABEL (info->csva.num_acct_label), string);
    g_free (string);
}

static void
csv_export_account_changed_cb (GtkSelectionModel *selection,
                               guint position, guint n_items,
                               gpointer user_data)
{
    CsvExportInfo *info = user_data;
    GListModel *model = G_LIST_MODEL (info->csva.account_store);
    guint index;
    Account *cursor;

    g_list_free (info->csva.account_list);
    info->csva.account_list = NULL;
    for (index = 0; index < g_list_model_get_n_items (model); ++index)
    {
        CsvExportAccountRow *row;

        if (!gtk_selection_model_is_selected (selection, index))
            continue;
        row = (CsvExportAccountRow *)g_list_model_get_item (model, index);
        info->csva.account_list = g_list_prepend (info->csva.account_list,
                                                  row->account);
        g_object_unref (row);
    }
    info->csva.account_list = g_list_reverse (info->csva.account_list);
    info->csva.cursor_position = position;
    cursor = csv_export_cursor_account (info);
    gtk_widget_set_sensitive (info->csva.select_subaccounts_button,
                              cursor && gnc_account_n_descendants (cursor) > 0);

    gnc_import_assistant_set_page_complete (GNC_IMPORT_ASSISTANT (info->assistant),
                                            info->account_page,
                                            info->csva.account_list != NULL);
    update_accounts_tree (info);
    (void)n_items;
}

static void
csv_export_select_all_clicked_cb (GtkWidget *widget, gpointer user_data)
{
    CsvExportInfo *info = user_data;

    gtk_selection_model_select_all (GTK_SELECTION_MODEL (info->csva.account_selection));
    gtk_widget_grab_focus (info->csva.account_treeview);
    (void)widget;
}

static void
csv_export_select_subaccounts_clicked_cb (GtkWidget *widget, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    Account *account = csv_export_cursor_account (info);
    GListModel *model = G_LIST_MODEL (info->csva.account_store);
    guint position;

    if (!account)
        return;

    for (position = 0; position < g_list_model_get_n_items (model); ++position)
    {
        CsvExportAccountRow *row =
            (CsvExportAccountRow *)g_list_model_get_item (model, position);
        Account *parent = gnc_account_get_parent (row->account);

        while (parent && parent != account)
            parent = gnc_account_get_parent (parent);
        if (parent == account)
            gtk_selection_model_select_item (GTK_SELECTION_MODEL (
                info->csva.account_selection), position, FALSE);
        g_object_unref (row);
    }

    gtk_widget_grab_focus (info->csva.account_treeview);
    (void)widget;
}

/* =============================================================== */

/*******************************************************
 * get_filter_times
 *
 * get the start and end times from the dialog
 *******************************************************/
static void
get_filter_times (CsvExportInfo *info)
{
    time64 time_val;

    if (gtk_check_button_get_active (GTK_CHECK_BUTTON(info->csvd.start_date_choose)))
    {
        time_val = gnc_date_edit_get_date (GNC_DATE_EDIT(info->csvd.start_date));
        time_val = gnc_time64_get_day_start (time_val);
        info->csvd.start_time = time_val;
    }
    else
    {
        if (gtk_check_button_get_active (GTK_CHECK_BUTTON(info->csvd.start_date_today)))
            info->csvd.start_time = gnc_time64_get_today_start();
        else
            info->csvd.start_time = info->csvd.earliest_time;
    }

    if (gtk_check_button_get_active (GTK_CHECK_BUTTON(info->csvd.end_date_choose)))
    {
        time_val = gnc_date_edit_get_date (GNC_DATE_EDIT(info->csvd.end_date));
        time_val = gnc_time64_get_day_end (time_val);
        info->csvd.end_time = time_val;
    }
    else
    {
        if (gtk_check_button_get_active (GTK_CHECK_BUTTON(info->csvd.end_date_today)))
            info->csvd.end_time = gnc_time64_get_today_end();
        else
            info->csvd.end_time = info->csvd.latest_time;
    }
}


/*******************************************************
 * csv_export_show_range_cb
 *
 * call back for show range button
 *******************************************************/
void
csv_export_show_range_cb (GtkCheckButton *button, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    gboolean active;

    g_return_if_fail (GTK_IS_TOGGLE_BUTTON(button));

    active = gtk_check_button_get_active (GTK_CHECK_BUTTON(button));

    if (!active)
    {
        info->csvd.start_time = info->csvd.earliest_time;
        info->csvd.end_time = info->csvd.latest_time;
    }
    else
        get_filter_times (info);

    gtk_widget_set_sensitive (info->csvd.table, active);
}


/*******************************************************
 * csv_export_date_changed_cb
 *
 * call back for when a date changes
 *******************************************************/
static void
csv_export_date_changed_cb (GtkWidget *w, gpointer user_data)
{
    CsvExportInfo *info = user_data;

    get_filter_times (info);
}


/*******************************************************
 * csv_export_start_date_cb
 *
 * call back for when the start date changes
 *******************************************************/
void
csv_export_start_date_cb (GtkWidget *radio, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    const gchar *name;
    gboolean active;

    g_return_if_fail (GTK_IS_TOGGLE_BUTTON(radio));

    if (!gtk_check_button_get_active (GTK_CHECK_BUTTON(radio)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }

    name = gtk_buildable_get_buildable_id (GTK_BUILDABLE(radio));
    active = (g_strcmp0 (name, "start_date_choose") == 0 ? 1 : 0 );
    gtk_widget_set_sensitive (info->csvd.start_date, active);
    get_filter_times (info);
}


/*******************************************************
 * csv_export_end_date_cb
 *
 * call back for when the end date changes
 *******************************************************/
void
csv_export_end_date_cb (GtkWidget *radio, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    const gchar *name;
    gboolean active;

    g_return_if_fail (GTK_IS_TOGGLE_BUTTON(radio));

    if (!gtk_check_button_get_active (GTK_CHECK_BUTTON(radio)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }

    name = gtk_buildable_get_buildable_id (GTK_BUILDABLE(radio));
    active = (g_strcmp0 (name, "end_date_choose") == 0 ? 1 : 0 );
    gtk_widget_set_sensitive (info->csvd.end_date, active);
    get_filter_times (info);
}


/*******************************************************************
 * get_earliest_and_latest_in_book
 *
 * Find the earliest and latest dates occurring in the book.
 *******************************************************************/
static void
get_earliest_and_latest_in_book (CsvExportInfo *info, QofBook *book)
{
    QofQuery *q;
    GSList *p1, *p2;
    GList *res;
    time64 etime, ltime;

    q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, book);

    /* Sort by transaction date */
    p1 = g_slist_prepend (NULL, TRANS_DATE_POSTED);
    p1 = g_slist_prepend (p1, SPLIT_TRANS);
    p2 = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);
    qof_query_set_sort_order (q, p1, p2, NULL);

    /* Run the query, find the earliest and latest transaction dates */
    res = qof_query_run (q);

    if (res)
    {
        etime = xaccQueryGetEarliestDateFound (q);
        ltime = xaccQueryGetLatestDateFound (q);
    }
    else
    {
        /* If no results, we don't want to bomb totally */
        etime = gnc_time (0);
        ltime = gnc_time (NULL);
    }
    info->csvd.earliest_time = gnc_time64_get_day_start (etime);
    info->csvd.latest_time = gnc_time64_get_day_end (ltime);

    qof_query_destroy (q);
}


/* =============================================================== */


/*******************************************************
 * Assistant page prepare functions
 *******************************************************/
static void
csv_export_assistant_start_page_prepare (GncImportAssistant *assistant,
                                         gpointer user_data)
{
    CsvExportInfo *info = user_data;
    gchar *msg = NULL;

    if (info->export_type == XML_EXPORT_TREE)
        msg = g_strdup (_(start_tree_string));
    else
        msg = g_strdup_printf (_(start_trans_common_string), _(start_trans_multi_string));

    gtk_label_set_text (GTK_LABEL(info->start_label), msg);
    g_free (msg);

    /* Enable the Assistant Buttons */
    gnc_import_assistant_set_page_complete (assistant, info->start_page, TRUE);
}


static void
csv_export_assistant_account_page_prepare (GncImportAssistant *assistant,
                                           gpointer user_data)
{
    CsvExportInfo *info = user_data;

    /* Enable the "Next" Assistant Button if we have accounts */
    if (g_list_length(info->csva.account_list) > 0)
        gnc_import_assistant_set_page_complete (assistant, info->account_page, TRUE);
    else
        gnc_import_assistant_set_page_complete (assistant, info->account_page, FALSE);
}


static void
csv_export_assistant_file_page_prepare (GncImportAssistant *assistant,
                                        gpointer user_data)
{
    CsvExportInfo *info = user_data;

    /* A new request inherits the last selected directory. */
    g_clear_pointer (&info->file_name, g_free);
    gtk_label_set_text (GTK_LABEL (info->file_name_label),
                        _("No file selected"));
    gnc_import_assistant_set_page_complete (assistant, info->file_page, FALSE);
}


static void
csv_export_assistant_finish_page_prepare (GncImportAssistant *assistant,
                                          gpointer user_data)
{
    CsvExportInfo *info = user_data;
    gchar *text;

    /* Set Finish page text */
    if (info->export_type == XML_EXPORT_TREE)
        text = g_strdup_printf (gettext (finish_tree_string), info->file_name);
    else
    {
        if ((info->export_type == XML_EXPORT_REGISTER) &&
            (g_list_length (info->csva.account_list) == 0))
            text = g_strdup_printf (gettext (finish_trans_search_gl_string), info->file_name);
        else
            text = g_strdup_printf (gettext (finish_trans_string),
                                    info->file_name,
                                    g_list_length (info->csva.account_list));
    }
    gtk_label_set_text (GTK_LABEL(info->finish_label), text);
    g_free (text);

    /* Test if the filename exists */
    if (g_file_test (info->file_name, G_FILE_TEST_EXISTS))
    {
        const char *format = _("The file %s already exists. "
                               "Are you sure you want to overwrite it?");

        CsvExportOverwriteRequest *request = g_new0 (CsvExportOverwriteRequest, 1);

        g_weak_ref_init (&request->assistant, GTK_WIDGET (assistant));
        gnc_import_assistant_set_page_complete (assistant, info->finish_label, FALSE);
        gnc_verify_dialog_async (GTK_WINDOW (assistant), FALSE,
                                 csv_export_overwrite_finished, request,
                                 format, info->file_name);
        return;
    }
    /* Enable the Assistant Buttons */
    gnc_import_assistant_set_page_complete (assistant, info->finish_label, TRUE);
}


static void
csv_export_assistant_summary_page_prepare (GncImportAssistant *assistant,
                                           gpointer user_data)
{
    CsvExportInfo *info = user_data;
    gchar *text, *mtext;
    gnc_set_default_directory (GNC_PREFS_GROUP, info->starting_dir);

    if (info->failed)
        text = _("There was a problem with the export, this could be due to lack of space, "
                 "permissions or unable to access folder. Check the trace file for further logging!\n"
                 "You may need to enable debugging.\n");
    else
        text = _("File exported successfully!\n");

    mtext = g_strdup_printf ("<span size=\"medium\"><b>%s</b></span>", text);

    gtk_label_set_markup (GTK_LABEL(info->summary_label), mtext);

    g_free (mtext);
}


static void
csv_export_assistant_prepare (GncImportAssistant *assistant, GtkWidget *page,
                              gpointer user_data)
{
    CsvExportInfo *info = user_data;

    if (page == info->start_page)
        csv_export_assistant_start_page_prepare (assistant, user_data);
    else if (page == info->account_page)
        csv_export_assistant_account_page_prepare (assistant, user_data);
    else if (page == info->file_page)
        csv_export_assistant_file_page_prepare (assistant, user_data);
    else if (page == info->finish_label)
        csv_export_assistant_finish_page_prepare (assistant, user_data);
    else if (page == info->summary_label)
        csv_export_assistant_summary_page_prepare (assistant, user_data);
    else
        g_assert_not_reached();
}


/*******************************************************
 * Assistant call back functions
 *******************************************************/
static void
csv_export_assistant_destroy_cb (GtkWidget *object, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    g_object_set_data (G_OBJECT (object), CSV_EXPORT_INFO_DATA_KEY, NULL);
    gnc_unregister_gui_component_by_data (ASSISTANT_CSV_EXPORT_CM_CLASS, info);
    g_list_free (info->csva.account_list);
    g_clear_object (&info->csva.account_selection);
    g_clear_object (&info->csva.account_store);
    g_free (info);
}

static void
csv_export_assistant_cancel (GncImportAssistant *assistant, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    gnc_close_gui_component_by_data (ASSISTANT_CSV_EXPORT_CM_CLASS, info);
}

static void
csv_export_assistant_close (GncImportAssistant *assistant, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    gnc_close_gui_component_by_data (ASSISTANT_CSV_EXPORT_CM_CLASS, info);
}

static void
csv_export_assistant_finish (GncImportAssistant *assistant, gpointer user_data)
{
    CsvExportInfo *info = user_data;

    if (info->export_type == XML_EXPORT_TREE)
        csv_tree_export (info);
    else
        csv_transactions_export (info);

    gnc_import_assistant_commit (assistant);
    gnc_import_assistant_set_current_page (assistant, 4);
}

static void
csv_export_close_handler (gpointer user_data)
{
    CsvExportInfo *info = user_data;

    g_free (info->file_name);
    g_free (info->starting_dir);
    if (info->mid_sep)
    g_free (info->mid_sep);

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->assistant));
    gtk_window_destroy (GTK_WINDOW(info->assistant));
}

static int
csv_export_assistant_forward_page (int current_page, gpointer user_data)
{
    CsvExportInfo *info = user_data;

    if (current_page == 0 &&
        (info->export_type == XML_EXPORT_TREE ||
         info->export_type == XML_EXPORT_REGISTER))
        return 2;
    return current_page + 1;
}

/*******************************************************
 * Create the Assistant
 *******************************************************/
static GtkWidget *
csv_export_assistant_create (CsvExportInfo *info)
{
    GtkBuilder *builder;
    GtkWidget *button;
    GtkWidget *table, *hbox;
    GncImportAssistant *assistant;

    builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder , "assistant-csv-export.glade", "csv_export_assistant");
    info->assistant = GTK_WIDGET(gtk_builder_get_object (builder, "csv_export_assistant"));
    assistant = gnc_import_assistant_new (
        GTK_WINDOW (info->assistant),
        GTK_STACK (gtk_builder_get_object (builder, "gnc_export_assistant_stack")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_export_assistant_page_title")),
        GTK_BOX (gtk_builder_get_object (builder, "gnc_export_assistant_actions")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_export_assistant_back")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_export_assistant_next")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_export_assistant_apply")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_export_assistant_cancel")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_export_assistant_close")));
    if (!assistant)
    {
        info->assistant = NULL;
        g_object_unref (builder);
        return NULL;
    }
    g_object_set_data (G_OBJECT (info->assistant), CSV_EXPORT_INFO_DATA_KEY, info);

    // Set the name for this assistant so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(info->assistant), "gnc-id-assistant-csv-export");
    gnc_widget_style_context_add_class (GTK_WIDGET(info->assistant), "gnc-class-exports");

    /* Load default settings */
    load_settings (info);

    /* Start Page */
    info->start_page = GTK_WIDGET(gtk_builder_get_object(builder, "start_page"));
    info->start_label = GTK_WIDGET(gtk_builder_get_object(builder, "start_label"));
    info->custom_entry = GTK_WIDGET(gtk_builder_get_object(builder, "custom_entry"));
    gtk_widget_set_sensitive (info->custom_entry, FALSE);

    /* Account Page */
    info->account_page = GTK_WIDGET(gtk_builder_get_object(builder, "account_page"));

    if ((info->export_type == XML_EXPORT_TREE) || (info->export_type == XML_EXPORT_REGISTER))
    {
        /* Tree and active-register exports do not require account selection.
         * The stack keeps the page alive while the forward function skips it. */
    }
    else
    {
        GtkColumnView *account_view;
        GtkListItemFactory *factory;
        GtkColumnViewColumn *column;
        GtkWidget *box, *label;

        info->csva.acct_info = GTK_WIDGET(gtk_builder_get_object (builder, "acct_info_vbox"));
        info->csva.num_acct_label = GTK_WIDGET(gtk_builder_get_object (builder, "num_accounts_label"));

        info->csva.account_store = g_list_store_new (csv_export_account_row_get_type ());
        info->csva.account_selection = gtk_multi_selection_new (G_LIST_MODEL (
            g_object_ref (info->csva.account_store)));
        account_view = GTK_COLUMN_VIEW (gtk_column_view_new (GTK_SELECTION_MODEL (
            g_object_ref (info->csva.account_selection))));
        info->csva.account_treeview = GTK_WIDGET (account_view);
        factory = GTK_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
        g_signal_connect (factory, "setup",
                          G_CALLBACK (csv_export_account_factory_setup), NULL);
        g_signal_connect (factory, "bind",
                          G_CALLBACK (csv_export_account_factory_bind), NULL);
        column = gtk_column_view_column_new (_("Account"), factory);
        gtk_column_view_column_set_expand (column, TRUE);
        gtk_column_view_column_set_resizable (column, TRUE);
        gtk_column_view_append_column (account_view, column);
        g_object_unref (column);
        g_signal_connect (info->csva.account_selection, "selection-changed",
                          G_CALLBACK (csv_export_account_changed_cb), info);

        box = GTK_WIDGET(gtk_builder_get_object (builder, "account_scroll"));
        gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (box),
                                       info->csva.account_treeview);

        label = GTK_WIDGET(gtk_builder_get_object (builder, "accounts_label"));
        gtk_label_set_mnemonic_widget (GTK_LABEL(label), info->csva.account_treeview);

        /* select subaccounts button */
        button = GTK_WIDGET(gtk_builder_get_object (builder, "select_subaccounts_button"));
        info->csva.select_subaccounts_button = button;
        gtk_widget_set_sensitive (button, FALSE);
        g_signal_connect (G_OBJECT(button), "clicked",
                          G_CALLBACK(csv_export_select_subaccounts_clicked_cb), info);

        button = GTK_WIDGET(gtk_builder_get_object (builder, "select_all_button"));
        g_signal_connect (G_OBJECT(button), "clicked",
                          G_CALLBACK(csv_export_select_all_clicked_cb), info);

        /* Set the date info */
        button = GTK_WIDGET(gtk_builder_get_object (builder, "show_range"));

        /* Get the Earliest and Latest dates in Book */
        get_earliest_and_latest_in_book (info, gnc_get_current_book());

        info->csvd.start_time = info->csvd.earliest_time;
        info->csvd.end_time = info->csvd.latest_time;
        gtk_check_button_set_active (GTK_CHECK_BUTTON(button), FALSE);

        table = GTK_WIDGET(gtk_builder_get_object (builder, "select_range_table"));
        info->csvd.table = table;
        gtk_widget_set_sensitive (GTK_WIDGET(table), FALSE);

        info->csvd.start_date_choose = GTK_WIDGET(gtk_builder_get_object (builder, "start_date_choose"));
        info->csvd.start_date_today = GTK_WIDGET(gtk_builder_get_object (builder, "start_date_today"));
        info->csvd.end_date_choose = GTK_WIDGET(gtk_builder_get_object (builder, "end_date_choose"));
        info->csvd.end_date_today = GTK_WIDGET(gtk_builder_get_object (builder, "end_date_today"));

        /* Start date info */
        info->csvd.start_date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
        gtk_widget_set_sensitive (info->csvd.start_date, FALSE);
        hbox = GTK_WIDGET(gtk_builder_get_object (builder, "start_date_hbox"));
        gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(info->csvd.start_date));
        gtk_widget_set_visible (GTK_WIDGET(info->csvd.start_date), TRUE);
        gnc_date_edit_set_time (GNC_DATE_EDIT(info->csvd.start_date), info->csvd.start_time);
        g_signal_connect (G_OBJECT(info->csvd.start_date), "date-changed",
                        G_CALLBACK(csv_export_date_changed_cb), info);

        /* End date info */
        info->csvd.end_date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
        gtk_widget_set_sensitive (info->csvd.end_date, FALSE);
        hbox = GTK_WIDGET(gtk_builder_get_object (builder, "end_date_hbox"));
        gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(info->csvd.end_date));
        gtk_widget_set_visible (GTK_WIDGET(info->csvd.end_date), TRUE);
        gnc_date_edit_set_time (GNC_DATE_EDIT(info->csvd.end_date), info->csvd.end_time);
        g_signal_connect (G_OBJECT (info->csvd.end_date), "date-changed",
                        G_CALLBACK (csv_export_date_changed_cb), info);

        /* Load Accounts */
        show_acct_type_accounts (info);
        update_accounts_tree (info);
    }

    /* File selection page */
    info->file_page = GTK_WIDGET (gtk_builder_get_object (builder, "file_page"));
    info->file_select_button = GTK_WIDGET (gtk_builder_get_object (
        builder, "file_select_button"));
    info->file_name_label = GTK_WIDGET (gtk_builder_get_object (
        builder, "file_name_label"));
    g_signal_connect (info->file_select_button, "clicked",
                      G_CALLBACK (csv_export_choose_file_cb), info);

    /* Finish Page */
    info->finish_label = GTK_WIDGET(gtk_builder_get_object (builder, "end_page"));

    /* Summary Page */
    info->summary_label = GTK_WIDGET(gtk_builder_get_object (builder, "summary_page"));

    g_signal_connect (G_OBJECT(info->assistant), "destroy",
                      G_CALLBACK(csv_export_assistant_destroy_cb), info);

    gnc_restore_window_size (GNC_PREFS_GROUP,
                             GTK_WINDOW(info->assistant), gnc_ui_get_main_window(NULL));
    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_SAVE_GEOMETRY))
    {
        GObject *object = gtk_builder_get_object (builder, "paned");
        gnc_prefs_bind (GNC_PREFS_GROUP, GNC_PREF_PANED_POS, NULL, object, "position");
    }

    gnc_builder_connect_signals (builder, info);
    gnc_import_assistant_set_page_complete (assistant, info->start_page, TRUE);
    gnc_import_assistant_set_page_complete (assistant, info->account_page, FALSE);
    gnc_import_assistant_set_page_complete (assistant, info->file_page, FALSE);
    gnc_import_assistant_set_page_complete (assistant, info->finish_label, FALSE);
    gnc_import_assistant_set_page_complete (assistant, info->summary_label, TRUE);
    gnc_import_assistant_set_page_action (assistant, 3,
                                          GNC_IMPORT_ASSISTANT_PAGE_APPLY);
    gnc_import_assistant_set_page_action (assistant, 4,
                                          GNC_IMPORT_ASSISTANT_PAGE_CLOSE);
    gnc_import_assistant_set_forward_page_func (assistant,
                                                csv_export_assistant_forward_page,
                                                info, NULL);
    gnc_import_assistant_set_callbacks (assistant, csv_export_assistant_prepare,
                                        csv_export_assistant_finish,
                                        csv_export_assistant_cancel,
                                        csv_export_assistant_close, info);
    g_object_unref (G_OBJECT(builder));
    return info->assistant;
}

static void
gnc_file_csv_export_internal (CsvExportType export_type, Query *q, Account *acc)
{
    CsvExportInfo *info;

    info = g_new0 (CsvExportInfo, 1);
    info->export_type = export_type;

    if (q)
        info->query = q;
    if (acc)
        info->csva.account_list = g_list_prepend(info->csva.account_list, acc);

    if (!csv_export_assistant_create (info))
    {
        g_list_free (info->csva.account_list);
        g_free (info);
        return;
    }
    gnc_register_gui_component (ASSISTANT_CSV_EXPORT_CM_CLASS,
                                NULL, csv_export_close_handler,
                                info);
    gnc_window_adjust_for_screen (GTK_WINDOW(info->assistant));
    gtk_window_present (GTK_WINDOW(info->assistant));
}


/********************************************************************\
 * gnc_file_csv_export                                              *
 * opens up a assistant to export accounts or transactions based on *
 * the type.                                                        *
 * Args:   export_type                                              *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_file_csv_export (CsvExportType export_type)
{
    gnc_file_csv_export_internal (export_type, NULL, NULL);
}


/********************************************************************\
 * gnc_file_csv_export_register                                     *
 * opens up a assistant to export register transactions based.      *
 * Args:   export_type                                              *
 * Args:   Query                                                    *
 * Args:   Account                                                  *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_file_csv_export_register (CsvExportType export_type, Query *q, Account *acc)
{
    gnc_file_csv_export_internal (export_type, q, acc);
}
