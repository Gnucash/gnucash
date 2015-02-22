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
#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-ui.h"
#include "gnc-uri-utils.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-prefs.h"
#include "gnc-tree-view-account.h"
#include "dialog-utils.h"
#include "Query.h"
#include "Transaction.h"

#include "assistant-utils.h"
#include "assistant-csv-export.h"
#include "csv-tree-export.h"
#include "csv-transactions-export.h"

#define GNC_PREFS_GROUP    "dialogs.export.csv"
#define GNC_PREF_PANED_POS "paned-position"
#define ASSISTANT_CSV_EXPORT_CM_CLASS "assistant-csv-export"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

/*************************************************************************/

void csv_export_assistant_prepare (GtkAssistant  *assistant, GtkWidget *page, gpointer user_data);
void csv_export_assistant_finish (GtkAssistant *gtkassistant, gpointer user_data);
void csv_export_assistant_cancel (GtkAssistant *gtkassistant, gpointer user_data);
void csv_export_assistant_close (GtkAssistant *gtkassistant, gpointer user_data);

void csv_export_assistant_start_page_prepare (GtkAssistant *assistant, gpointer user_data);
void csv_export_assistant_account_page_prepare (GtkAssistant *gtkassistant, gpointer user_data);
void csv_export_assistant_file_page_prepare (GtkAssistant *assistant, gpointer user_data);
void csv_export_assistant_finish_page_prepare (GtkAssistant *assistant, gpointer user_data);
void csv_export_assistant_summary_page_prepare (GtkAssistant *assistant, gpointer user_data);

void csv_export_quote_cb (GtkToggleButton *button, gpointer user_data);
void csv_export_sep_cb (GtkWidget *radio, gpointer user_data);
void csv_export_custom_entry_cb (GtkWidget *widget, gpointer user_data);

void csv_export_show_range_cb (GtkRadioButton *button, gpointer user_data);
void csv_export_start_date_cb (GtkWidget *radio, gpointer user_data);
void csv_export_end_date_cb (GtkWidget *radio, gpointer user_data);

void csv_export_file_chooser_confirm_cb (GtkWidget *button, CsvExportInfo *info);

static const gchar *finish_tree_string = N_(
            /* Translators: %s is the file name string. */
            "The account tree will be exported to the file '%s' when you click 'Apply'.\n\n"
            "You can also verify your selections by clicking on 'Back' or 'Cancel' to Abort Export.\n");

static const gchar *finish_trans_string = N_(
            /* Translators: %s is the file name string and %u the number of accounts. */
            "When you click 'Apply', the transactions will be exported to the file '%s' and"
            " the number of accounts exported will be %u.\n\n"
            "You can also verify your selections by clicking on 'Back' or 'Cancel' to Abort Export.\n");

static const gchar *start_tree_string = N_(
        "This assistant will help you export the Account Tree to a file\n"
        " with the separator specified below.\n\n"
        "Select the settings you require for the file and then click 'Forward' to proceed"
        " or 'Cancel' to Abort Export.\n");

static const gchar *start_trans_string = N_(
            "This assistant will help you export the Transactions to a file\n"
            " with the separator specified below.\n\n"
            "There will be multiple rows for each transaction and may"
            " require further manipulation to get them in a format you can use.\n\n"
            "Each Transaction will appear once in the export and will be listed in"
            " the order the accounts were processed\n\n"
            "Select the settings you require for the file and then click 'Forward' to proceed"
            " or 'Cancel' to Abort Export.\n");


/**************************************************
 * csv_export_file_chooser_confirm_cb
 *
 * call back for ok button in file chooser widget
 **************************************************/
void
csv_export_file_chooser_confirm_cb (GtkWidget *button, CsvExportInfo *info)
{
    GtkAssistant *assistant = GTK_ASSISTANT(info->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    gchar *file_name;

    gtk_assistant_set_page_complete (assistant, page, FALSE);

    file_name = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER(info->file_chooser));

    if (file_name)
    {
        if (g_file_test (file_name, G_FILE_TEST_EXISTS))
        {
            const char *format = _("The file %s already exists. "
                                   "Are you sure you want to overwrite it?");

            /* if user says cancel, we should break out */
            if (!gnc_verify_dialog (NULL, FALSE, format, file_name))
                return;
        }

        info->file_name = g_strdup (file_name);
        gtk_assistant_set_page_complete (assistant, page, TRUE);
    }

    if (file_name)
    {
        gchar *filepath = gnc_uri_get_path (file_name);
        gchar *filedir = g_path_get_dirname (filepath);
        info->starting_dir = g_strdup (filedir);
        g_free (filedir);
        g_free (filepath);
    }
    g_free (file_name);

    DEBUG("file_name selected is %s", info->file_name);
    DEBUG("starting directory is %s", info->starting_dir);

    /* Step to next page if page is complete */
    if(gtk_assistant_get_page_complete (assistant, page))
        gtk_assistant_set_current_page (assistant, num + 1);
}


/*******************************************************
 * csv_export_sep_cb
 *
 * call back for type of separartor required
 *******************************************************/
void csv_export_sep_cb (GtkWidget *radio, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    const gchar *name;

    GtkAssistant *assistant = GTK_ASSISTANT(info->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    if (!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(radio)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }

    name = gtk_buildable_get_name (GTK_BUILDABLE(radio));

    gtk_widget_set_sensitive (info->custom_entry, FALSE);
    info->use_custom = FALSE;
    gtk_assistant_set_page_complete (assistant, page, TRUE);

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
            gtk_assistant_set_page_complete (assistant, page, FALSE);
    }
}


/*******************************************************
 * csv_export_quote_cb
 *
 * call back for use of quotes
 *******************************************************/
void csv_export_quote_cb (GtkToggleButton *button, gpointer user_data)
{
    CsvExportInfo *info = user_data;

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(button)))
        info->use_quotes = TRUE;
    else
        info->use_quotes = FALSE;
}


/*******************************************************
 * csv_export_custom_entry_cb
 *
 * call back for custom separator
 *******************************************************/
void csv_export_custom_entry_cb (GtkWidget *widget, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    const gchar *custom_str;

    GtkAssistant *assistant = GTK_ASSISTANT(info->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    custom_str = gtk_entry_get_text (GTK_ENTRY(info->custom_entry));
    info->separator_str = strdup (custom_str);

    if (info->use_custom == TRUE && gtk_entry_get_text_length (GTK_ENTRY(info->custom_entry)) == 0)

        gtk_assistant_set_page_complete (assistant, page, FALSE);
    else
        gtk_assistant_set_page_complete (assistant, page, TRUE);
}


/*******************************************************
 * load_settings
 *
 * load the default settings for the assistant
 *******************************************************/
static
void load_settings (CsvExportInfo *info)
{
    info->use_quotes = FALSE;
    info->separator_str = ",";
    info->file_name = NULL;
    info->starting_dir = NULL;
    info->trans_list = NULL;

    /* The default directory for the user to select files. */
    info->starting_dir = gnc_get_default_directory (GNC_PREFS_GROUP);
}

/* =============================================================== */

/*******************************************************
 * csv_export_cursor_changed_cb
 *
 * call back for cursor selection in account tree
 *******************************************************/
static void
csv_export_cursor_changed_cb (GtkWidget *widget, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    GncTreeViewAccount *account_tree;
    Account *account;
    gint num_children;

    account_tree = GNC_TREE_VIEW_ACCOUNT (info->csva.account_treeview);
    account = gnc_tree_view_account_get_cursor_account (account_tree);
    if (!account)
    {
        gtk_widget_set_sensitive (info->csva.select_button, FALSE);
        return;
    }
    num_children = gnc_tree_view_account_count_children (account_tree, account);
    gtk_widget_set_sensitive (info->csva.select_button, num_children > 0);
}


/*******************************************************
 * show_acct_type_accounts
 *
 * show required accounts in account tree
 *******************************************************/
static void
show_acct_type_accounts (CsvExportInfo *info)
{
    GncTreeViewAccount *tree;
    AccountViewInfo Viewinfo;
    GNCAccountType type;

    tree = GNC_TREE_VIEW_ACCOUNT (info->csva.account_treeview);

    gnc_tree_view_account_get_view_info (tree, &Viewinfo);

    for (type = 0; type < NUM_ACCOUNT_TYPES; type++) /* from Account.h */
    {
        Viewinfo.include_type[type] = ((type == ACCT_TYPE_BANK)      ||
                                       (type == ACCT_TYPE_CASH)      ||
                                       (type == ACCT_TYPE_CREDIT)    ||
                                       (type == ACCT_TYPE_ASSET)     ||
                                       (type == ACCT_TYPE_LIABILITY) ||
                                       (type == ACCT_TYPE_STOCK)     ||
                                       (type == ACCT_TYPE_MUTUAL)    ||
                                       (type == ACCT_TYPE_INCOME)    ||
                                       (type == ACCT_TYPE_EXPENSE)   ||
                                       (type == ACCT_TYPE_EQUITY)    ||
                                       (type == ACCT_TYPE_RECEIVABLE)||
                                       (type == ACCT_TYPE_PAYABLE)   ||
                                       (type == ACCT_TYPE_ROOT)      ||
                                       (type == ACCT_TYPE_TRADING));
    }
    gnc_tree_view_account_set_view_info (tree, &Viewinfo);
    csv_export_cursor_changed_cb (GTK_WIDGET(tree), info);
}


/*******************************************************
 * update_accounts_tree
 *
 * update the account tree
 *******************************************************/
static int
update_accounts_tree (CsvExportInfo *info)
{
    GncTreeViewAccount *tree;
    GtkTreeSelection* selection;
    GtkWidget *label;
    int num_accounts;
    char *string;

    tree = GNC_TREE_VIEW_ACCOUNT(info->csva.account_treeview);
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(tree));
    num_accounts = gtk_tree_selection_count_selected_rows (selection);

    label = info->csva.num_acct_label;

    string = g_strdup_printf ("%d", num_accounts);
    gtk_label_set_text (GTK_LABEL (label), string);
    g_free (string);

    return num_accounts;
}


/*******************************************************
 * csv_export_account_changed_cb
 *
 * update account list after selection changed
 *******************************************************/
static void
csv_export_account_changed_cb (GtkTreeSelection *selection,
                               gpointer user_data)
{
    CsvExportInfo *info = user_data;
    GtkAssistant *assistant = GTK_ASSISTANT(info->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    GncTreeViewAccount *view;

    g_return_if_fail(GTK_IS_TREE_SELECTION(selection));

    info->csva.num_accounts = update_accounts_tree (info);

    /* Enable the Forward Assistant Button if we have accounts */
    if (info->csva.num_accounts > 0)
        gtk_assistant_set_page_complete (assistant, page, TRUE);
    else
        gtk_assistant_set_page_complete (assistant, page, FALSE);

    view = GNC_TREE_VIEW_ACCOUNT(info->csva.account_treeview);
    info->csva.account_list = gnc_tree_view_account_get_selected_accounts (view);
}


/*******************************************************
 * csv_export_select_subaccounts_clicked_cb
 *
 * select all the sub accounts
 *******************************************************/
static void
csv_export_select_subaccounts_clicked_cb (GtkWidget *widget, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    GncTreeViewAccount *account_tree;
    Account *account;

    account_tree = GNC_TREE_VIEW_ACCOUNT (info->csva.account_treeview);
    account = gnc_tree_view_account_get_cursor_account (account_tree);
    if (!account)
        return;

    gnc_tree_view_account_select_subaccounts (account_tree, account);

    gtk_widget_grab_focus (info->csva.account_treeview);
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

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(info->csvd.start_date_choose)))
    {
        time_val = gnc_date_edit_get_date (GNC_DATE_EDIT(info->csvd.start_date));
        time_val = gnc_time64_get_day_start (time_val);
        info->csvd.start_time = time_val;
    }
    else
    {
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(info->csvd.start_date_today)))
            info->csvd.start_time = gnc_time64_get_today_start();
        else
            info->csvd.start_time = 0;
    }

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(info->csvd.end_date_choose)))
    {
        time_val = gnc_date_edit_get_date (GNC_DATE_EDIT(info->csvd.end_date));
        time_val = gnc_time64_get_day_end (time_val);
        info->csvd.end_time = time_val;
    }
    else
    {
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(info->csvd.end_date_today)))
            info->csvd.end_time = gnc_time64_get_today_end();
        else
            info->csvd.end_time = gnc_time (NULL);
    }
}


/*******************************************************
 * csv_export_show_range_cb
 *
 * call back for show range button
 *******************************************************/
void
csv_export_show_range_cb (GtkRadioButton *button, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    gboolean active;

    g_return_if_fail (GTK_IS_RADIO_BUTTON(button));

    active = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(button));
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

    g_return_if_fail (GTK_IS_RADIO_BUTTON(radio));

    if (!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(radio)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }

    name = gtk_buildable_get_name (GTK_BUILDABLE(radio));
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

    g_return_if_fail (GTK_IS_RADIO_BUTTON(radio));

    if (!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(radio)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }

    name = gtk_buildable_get_name (GTK_BUILDABLE(radio));
    active = (g_strcmp0 (name, "end_date_choose") == 0 ? 1 : 0 );
    gtk_widget_set_sensitive (info->csvd.end_date, active);
    get_filter_times (info);
}


/*******************************************************************
 * get_earliest_in_book
 *
 * Find the earliest date occuring in the book.  Do this by making
 * a query and sorting by date. Since the truncated sort returns
 * only the *last* search results, sort in decreasing order.
 *******************************************************************/
static time64
get_earliest_in_book (QofBook *book)
{
    QofQuery *q;
    GSList *p1, *p2;
    GList *res;
    time64 earliest;

    q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_max_results (q, 1);
    qof_query_set_book (q, book);

    /* Sort by transaction date */
    p1 = g_slist_prepend (NULL, TRANS_DATE_POSTED);
    p1 = g_slist_prepend (p1, SPLIT_TRANS);
    p2 = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);
    qof_query_set_sort_order (q, p1, p2, NULL);

    /* Reverse the sort order */
    qof_query_set_sort_increasing (q, FALSE, FALSE, FALSE);

    /* Run the query, find the earliest transaction date */
    res = qof_query_run (q);

    if (res)
    {
        earliest = xaccQueryGetEarliestDateFound (q);
    }
    else
    {
        /* If no results, we don't want to bomb totally */
        earliest = gnc_time (0);
    }

    qof_query_destroy (q);
    return earliest;
}


/* =============================================================== */


/*******************************************************
 * Assistant page prepare functions
 *******************************************************/
void
csv_export_assistant_start_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvExportInfo *info = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    /* Set Start page text */
    if (info->export_type == XML_EXPORT_TREE)
        gtk_label_set_text (GTK_LABEL(info->start_label), gettext (start_tree_string));
    else
        gtk_label_set_text (GTK_LABEL(info->start_label), gettext (start_trans_string));

    /* Enable the Assistant Buttons */
    gtk_assistant_set_page_complete (assistant, page, TRUE);
}


void
csv_export_assistant_account_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvExportInfo *info = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    /* Enable the Forward Assistant Button if we have accounts */
    if (info->csva.num_accounts > 0)
        gtk_assistant_set_page_complete (assistant, page, TRUE);
    else
        gtk_assistant_set_page_complete (assistant, page, FALSE);
}


void
csv_export_assistant_file_page_prepare (GtkAssistant *assistant,
                                        gpointer user_data)
{
    CsvExportInfo *info = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    /* Set the default directory */
    if (info->starting_dir)
        gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER(info->file_chooser), info->starting_dir);
    gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER(info->file_chooser), "");

    /* Disable the Forward Assistant Button */
    gtk_assistant_set_page_complete (assistant, page, FALSE);
}


void
csv_export_assistant_finish_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvExportInfo *info = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);
    gchar *text;

    /* Set Finish page text */
    if (info->export_type == XML_EXPORT_TREE)
        text = g_strdup_printf (gettext (finish_tree_string), info->file_name);
    else
        text = g_strdup_printf (gettext (finish_trans_string), info->file_name, info->csva.num_accounts);

    gtk_label_set_text (GTK_LABEL(info->finish_label), text);
    g_free (text);

    /* Enable the Assistant Buttons */
    gtk_assistant_set_page_complete (assistant, page, TRUE);
}


void
csv_export_assistant_summary_page_prepare (GtkAssistant *assistant,
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


void
csv_export_assistant_prepare (GtkAssistant *assistant, GtkWidget *page,
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
csv_export_assistant_destroy_cb (GtkObject *object, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    gnc_unregister_gui_component_by_data (ASSISTANT_CSV_EXPORT_CM_CLASS, info);
    g_free (info);
}

void
csv_export_assistant_cancel (GtkAssistant *assistant, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    gnc_close_gui_component_by_data (ASSISTANT_CSV_EXPORT_CM_CLASS, info);
}

void
csv_export_assistant_close (GtkAssistant *assistant, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    gnc_close_gui_component_by_data (ASSISTANT_CSV_EXPORT_CM_CLASS, info);
}

void
csv_export_assistant_finish (GtkAssistant *assistant, gpointer user_data)
{
    CsvExportInfo *info = user_data;

    if (info->export_type == XML_EXPORT_TREE)
        csv_tree_export (info);
    else
        csv_transactions_export (info);
}

static void
csv_export_close_handler (gpointer user_data)
{
    CsvExportInfo *info = user_data;

    g_free (info->file_name);
    g_free (info->starting_dir);

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->window));
    gtk_widget_destroy (info->window);
}

/*******************************************************
 * Create the Assistant
 *******************************************************/
static GtkWidget *
csv_export_assistant_create (CsvExportInfo *info)
{
    GtkBuilder *builder;
    GtkWidget *window;
    GtkWidget *box, *h_box;
    GtkWidget *button;
    GtkWidget *table, *hbox;
    time64 start_time, end_time;

    builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder , "assistant-csv-export.glade", "CSV Export Assistant");
    window = GTK_WIDGET(gtk_builder_get_object (builder, "CSV Export Assistant"));
    info->window = window;

    /* Set the assistant colors */
    gnc_assistant_set_colors (GTK_ASSISTANT (info->window));

    /* Load default settings */
    load_settings (info);

    /* Start Page */
    info->start_page = GTK_WIDGET(gtk_builder_get_object(builder, "start_page"));
    info->start_label = GTK_WIDGET(gtk_builder_get_object(builder, "start_label"));
    info->custom_entry = GTK_WIDGET(gtk_builder_get_object(builder, "custom_entry"));
    gtk_widget_set_sensitive (info->custom_entry, FALSE);

    /* Account Page */
    info->account_page = GTK_WIDGET(gtk_builder_get_object(builder, "account_page"));

    if (info->export_type == XML_EXPORT_TREE)
        gtk_widget_destroy (info->account_page);
    else
    {
        GtkTreeView *tree_view;
        GtkTreeSelection *selection;
        GtkWidget *box, *label;

        info->csva.acct_info = GTK_WIDGET(gtk_builder_get_object (builder, "acct_info_vbox"));
        info->csva.num_acct_label = GTK_WIDGET(gtk_builder_get_object (builder, "num_accounts_label"));

        tree_view = gnc_tree_view_account_new (FALSE);
        info->csva.account_treeview = GTK_WIDGET(tree_view);

        selection = gtk_tree_view_get_selection (tree_view);
        gtk_tree_selection_set_mode (selection, GTK_SELECTION_EXTENDED);
        g_signal_connect (G_OBJECT(selection), "changed",
                          G_CALLBACK(csv_export_account_changed_cb), info);

        gtk_widget_show (info->csva.account_treeview);
        box = GTK_WIDGET(gtk_builder_get_object (builder, "account_scroll"));
        gtk_container_add (GTK_CONTAINER(box), info->csva.account_treeview);

        label = GTK_WIDGET(gtk_builder_get_object (builder, "accounts_label"));
        gtk_label_set_mnemonic_widget (GTK_LABEL(label), GTK_WIDGET(tree_view));

        /* select subaccounts button */
        button = GTK_WIDGET(gtk_builder_get_object (builder, "select_subaccounts_button"));
        info->csva.select_button = button;

        g_signal_connect (G_OBJECT(button), "clicked",
                          G_CALLBACK(csv_export_select_subaccounts_clicked_cb), info);
        g_signal_connect (G_OBJECT(info->csva.account_treeview), "cursor_changed",
                          G_CALLBACK(csv_export_cursor_changed_cb), info);

        /* Set the date info */
        button = GTK_WIDGET(gtk_builder_get_object (builder, "show_range"));

        /* Earliest and Latest in Book */
        start_time = get_earliest_in_book (gnc_get_current_book());
        end_time = gnc_time (NULL);

        info->csvd.start_time = start_time;
        info->csvd.end_time = end_time;
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button), FALSE);

        table = GTK_WIDGET(gtk_builder_get_object (builder, "select_range_table"));
        info->csvd.table = table;
        gtk_widget_set_sensitive (GTK_WIDGET(table), FALSE);

        info->csvd.start_date_choose = GTK_WIDGET(gtk_builder_get_object (builder, "start_date_choose"));
        info->csvd.start_date_today = GTK_WIDGET(gtk_builder_get_object (builder, "start_date_today"));
        info->csvd.end_date_choose = GTK_WIDGET(gtk_builder_get_object (builder, "end_date_choose"));
        info->csvd.end_date_today = GTK_WIDGET(gtk_builder_get_object (builder, "end_date_today"));

        /* Start date info */
        info->csvd.start_date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
        hbox = GTK_WIDGET(gtk_builder_get_object (builder, "start_date_hbox"));
        gtk_box_pack_start (GTK_BOX(hbox), info->csvd.start_date, TRUE, TRUE, 0);
        gtk_widget_show (info->csvd.start_date);
        gnc_date_edit_set_time (GNC_DATE_EDIT(info->csvd.start_date), start_time);
        g_signal_connect (G_OBJECT(info->csvd.start_date), "date-changed",
                        G_CALLBACK(csv_export_date_changed_cb), info);

        /* End date info */
        info->csvd.end_date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
        hbox = GTK_WIDGET(gtk_builder_get_object (builder, "end_date_hbox"));
        gtk_box_pack_start (GTK_BOX(hbox), info->csvd.end_date, TRUE, TRUE, 0);
        gtk_widget_show (info->csvd.end_date);
        gnc_date_edit_set_time (GNC_DATE_EDIT(info->csvd.end_date), end_time);
        g_signal_connect (G_OBJECT (info->csvd.end_date), "date-changed",
                        G_CALLBACK (csv_export_date_changed_cb), info);

        /* Load Accounts */
        show_acct_type_accounts (info);
        update_accounts_tree (info);
    }

    /* File chooser Page */
    info->file_page = GTK_WIDGET(gtk_builder_get_object(builder, "file_page"));
    info->file_chooser = gtk_file_chooser_widget_new (GTK_FILE_CHOOSER_ACTION_SAVE);
    button = gtk_button_new_from_stock (GTK_STOCK_OK);
    gtk_widget_set_size_request (button, 100, -1);
    gtk_widget_show (button);
    h_box = gtk_hbox_new (TRUE, 0);
    gtk_box_pack_start(GTK_BOX(h_box), button, FALSE, FALSE, 0);
    gtk_file_chooser_set_extra_widget (GTK_FILE_CHOOSER(info->file_chooser), h_box);
    g_signal_connect (G_OBJECT(button), "clicked",
                      G_CALLBACK(csv_export_file_chooser_confirm_cb), info);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "file_page"));
    gtk_box_pack_start (GTK_BOX (box), info->file_chooser, TRUE, TRUE, 6);
    gtk_widget_show (info->file_chooser);

    /* Finish Page */
    info->finish_label = GTK_WIDGET(gtk_builder_get_object (builder, "end_page"));

    /* Summary Page */
    info->summary_label = GTK_WIDGET(gtk_builder_get_object (builder, "summary_page"));

    g_signal_connect (G_OBJECT(window), "destroy",
                      G_CALLBACK(csv_export_assistant_destroy_cb), info);

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->window));
    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_SAVE_GEOMETRY))
    {
        GObject *object = gtk_builder_get_object (builder, "paned");
        gnc_prefs_bind (GNC_PREFS_GROUP, GNC_PREF_PANED_POS, object, "position");
    }

    gtk_builder_connect_signals (builder, info);
    g_object_unref (G_OBJECT(builder));
    return window;
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
    CsvExportInfo *info;

    info = g_new0 (CsvExportInfo, 1);
    info->export_type = export_type;
    csv_export_assistant_create (info);
    gnc_register_gui_component (ASSISTANT_CSV_EXPORT_CM_CLASS,
                                NULL, csv_export_close_handler,
                                info);
    gtk_widget_show_all (info->window);
    gnc_window_adjust_for_screen (GTK_WINDOW(info->window));
}
