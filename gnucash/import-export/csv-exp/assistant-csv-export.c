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
#include "gnc-date-edit.h"
#include "gnc-prefs.h"
#include "gnc-tree-view-account.h"
#include "dialog-utils.h"
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
void csv_export_simple_cb (GtkToggleButton *button, gpointer user_data);
void csv_export_sep_cb (GtkWidget *radio, gpointer user_data);
void csv_export_custom_entry_cb (GtkWidget *widget, gpointer user_data);

void csv_export_show_range_cb (GtkRadioButton *button, gpointer user_data);
void csv_export_start_date_cb (GtkWidget *radio, gpointer user_data);
void csv_export_end_date_cb (GtkWidget *radio, gpointer user_data);

void csv_export_file_chooser_file_activated_cb (GtkFileChooser *chooser, CsvExportInfo *info);
void csv_export_file_chooser_selection_changed_cb (GtkFileChooser *chooser, CsvExportInfo *info);

/* Fixme: Can we simplify the work of translators by splitting in invariant and variant paragraphs? */
static const gchar *finish_tree_string = N_(
            /* Translators: %s is the file name string. */
            "The account tree will be exported to the file '%s' when you click \"Apply\".\n\n"
            "You can also verify your selections by clicking on \"Back\" or \"Cancel\" to abort the export.\n");

static const gchar *finish_trans_string = N_(
            /* Translators: %s is the file name string and %u the number of accounts. */
            "When you click \"Apply\", the transactions will be exported to the file '%s' and"
            " the number of accounts exported will be %u.\n\n"
            "You can also verify your selections by clicking on \"Back\" or \"Cancel\" to abort the export.\n");

static const gchar *finish_trans_search_gl_string = N_(
            /* Translators: %s is the file name string. */
            "When you click \"Apply\", the transactions will be exported to the file '%s'.\n\n"
            "You can also verify your selections by clicking on \"Back\" or \"Cancel\" to abort the export.\n");

static const gchar *start_tree_string = N_(
            "This assistant will help you export the Account Tree to a file\n"
            " with the separator specified below.\n\n"
            "Select the settings you require for the file and then click \"Next\" to proceed"
            " or \"Cancel\" to abort the export.\n");

static const gchar *start_trans_string = N_(
            "This assistant will help you export the Transactions to a file\n"
            " with the separator specified below.\n\n"
            "There will be multiple rows for each transaction and may"
            " require further manipulation to get them in a format you can use.\n\n"
            "Each Transaction will appear once in the export and will be listed in"
            " the order the accounts were processed\n\n"
            "Select the settings you require for the file and then click \"Next\" to proceed"
            " or \"Cancel\" to abort the export.\n");

static const gchar *start_trans_simple_string = N_(
            "This assistant will help you export the Transactions to a file\n"
            " with the separator specified below.\n\n"
            "There will be multiple rows for each transaction and may require further"
            " manipulation to get them in a format you can use. Each Transaction will"
            " appear once in the export and will be listed in the order the accounts"
            " were processed\n\n"
            "By selecting the simple layout, the output will be equivalent to a single"
            " row register view and as such some of the transfer detail could be lost.\n\n"
            "Select the settings you require for the file and then click \"Next\" to proceed"
            " or \"Cancel\" to abort the export.\n");


/**************************************************
 * csv_export_assistant_check_filename
 *
 * check for a valid filename for GtkFileChooser callbacks
 **************************************************/
static gboolean
csv_export_assistant_check_filename (GtkFileChooser *chooser,
                                     CsvExportInfo *info)
{
    gchar *file_name = gtk_file_chooser_get_filename (chooser);

    /* Test for a valid filename and not a directory */
    if (file_name && !g_file_test (file_name, G_FILE_TEST_IS_DIR))
    {
        gchar *filepath = gnc_uri_get_path (file_name);
        gchar *filedir = g_path_get_dirname (filepath);

        g_free (info->file_name);
        info->file_name = g_strdup (file_name);

        g_free (info->starting_dir);
        info->starting_dir = g_strdup (filedir);

        g_free (filedir);
        g_free (filepath);
        g_free (file_name);

        DEBUG("file_name selected is %s", info->file_name);
        DEBUG("starting directory is %s", info->starting_dir);
        return TRUE;
    }
    g_free (file_name);
    return FALSE;
}


/**************************************************
 * csv_export_file_chooser_file_activated_cb
 *
 * call back for GtkFileChooser file-activated signal
 **************************************************/
void
csv_export_file_chooser_file_activated_cb (GtkFileChooser *chooser,
                                           CsvExportInfo *info)
{
    GtkAssistant *assistant = GTK_ASSISTANT(info->assistant);
    gtk_assistant_set_page_complete (assistant, info->file_page, FALSE);

    /* Test for a valid filename and not a directory */
    if (csv_export_assistant_check_filename (chooser, info))
    {
        gtk_assistant_set_page_complete (assistant, info->file_page, TRUE);
        gtk_assistant_next_page (assistant);
    }
}


/**************************************************
 * csv_export_file_chooser_selection_changed_cb
 *
 * call back for GtkFileChooser widget
 **************************************************/
void
csv_export_file_chooser_selection_changed_cb (GtkFileChooser *chooser,
                                              CsvExportInfo *info)
{
    GtkAssistant *assistant = GTK_ASSISTANT(info->assistant);

    /* Enable the "Next" button based on a valid filename */
    gtk_assistant_set_page_complete (assistant, info->file_page,
        csv_export_assistant_check_filename (chooser, info));
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
    GtkAssistant *assistant = GTK_ASSISTANT(info->assistant);
    const gchar *name;

    if (!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(radio)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }

    name = gtk_buildable_get_name (GTK_BUILDABLE(radio));

    gtk_widget_set_sensitive (info->custom_entry, FALSE);
    info->use_custom = FALSE;
    gtk_assistant_set_page_complete (assistant, info->start_page, TRUE);

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
            gtk_assistant_set_page_complete (assistant, info->start_page, FALSE);
    }
}


/*******************************************************
 * csv_export_quote_cb
 *
 * call back for use of quotes
 *******************************************************/
void
csv_export_quote_cb (GtkToggleButton *button, gpointer user_data)
{
    CsvExportInfo *info = user_data;

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(button)))
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
csv_export_simple_cb (GtkToggleButton *button, gpointer user_data)
{
    CsvExportInfo *info = user_data;

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(button)))
        info->simple_layout = TRUE;
    else
        info->simple_layout = FALSE;
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
    GtkAssistant *assistant = GTK_ASSISTANT(info->assistant);
    const gchar *custom_str;

    custom_str = gtk_entry_get_text (GTK_ENTRY(info->custom_entry));
    info->separator_str = strdup (custom_str);

    if (info->use_custom == TRUE && gtk_entry_get_text_length (GTK_ENTRY(info->custom_entry)) == 0)
        gtk_assistant_set_page_complete (assistant, info->start_page, FALSE);
    else
        gtk_assistant_set_page_complete (assistant, info->start_page, TRUE);
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
    GtkAssistant *assistant = GTK_ASSISTANT(info->assistant);
    GncTreeViewAccount *view;

    g_return_if_fail(GTK_IS_TREE_SELECTION(selection));

    info->csva.num_accounts = update_accounts_tree (info);

    /* Enable the "Next" Assistant Button if we have accounts */
    if (info->csva.num_accounts > 0)
        gtk_assistant_set_page_complete (assistant, info->account_page, TRUE);
    else
        gtk_assistant_set_page_complete (assistant, info->account_page, FALSE);

    view = GNC_TREE_VIEW_ACCOUNT(info->csva.account_treeview);
    info->csva.account_list = gnc_tree_view_account_get_selected_accounts (view);
}


/*******************************************************
 * csv_export_select_all_clicked_cb
 *
 * select all the accounts
 *******************************************************/
static void
csv_export_select_all_clicked_cb (GtkWidget *widget, gpointer user_data)
{
    CsvExportInfo *info = user_data;
    GtkTreeSelection *selection = gtk_tree_view_get_selection
                                    (GTK_TREE_VIEW (info->csva.account_treeview));

    gtk_tree_view_expand_all (GTK_TREE_VIEW (info->csva.account_treeview));
    gtk_tree_selection_select_all (selection);

    gtk_widget_grab_focus (info->csva.account_treeview);
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
            info->csvd.start_time = info->csvd.earliest_time;
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
            info->csvd.end_time = info->csvd.latest_time;
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
void
csv_export_assistant_start_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvExportInfo *info = user_data;

    /* Set Start page text */
    if (info->export_type == XML_EXPORT_TREE)
        gtk_label_set_text (GTK_LABEL(info->start_label), gettext (start_tree_string));
    else
    {
        /* General Journal and search registers are always multi-line exported */
        if ((info->export_type == XML_EXPORT_REGISTER) && (info->account == NULL))
            gtk_label_set_text (GTK_LABEL(info->start_label), gettext (start_trans_string));
        else
            gtk_label_set_text (GTK_LABEL(info->start_label), gettext (start_trans_simple_string));
    }

    /* Enable the Assistant Buttons */
    gtk_assistant_set_page_complete (assistant, info->start_page, TRUE);
}


void
csv_export_assistant_account_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvExportInfo *info = user_data;

    /* Enable the "Next" Assistant Button if we have accounts */
    if (info->csva.num_accounts > 0)
        gtk_assistant_set_page_complete (assistant, info->account_page, TRUE);
    else
        gtk_assistant_set_page_complete (assistant, info->account_page, FALSE);
}


void
csv_export_assistant_file_page_prepare (GtkAssistant *assistant,
                                        gpointer user_data)
{
    CsvExportInfo *info = user_data;

    /* Set the default directory */
    if (info->starting_dir)
        gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER(info->file_chooser), info->starting_dir);
    gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER(info->file_chooser), "");

    /* Disable the "Next" Assistant Button */
    gtk_assistant_set_page_complete (assistant, info->file_page, FALSE);
}


void
csv_export_assistant_finish_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvExportInfo *info = user_data;
    gchar *text;

    /* Set Finish page text */
    if (info->export_type == XML_EXPORT_TREE)
        text = g_strdup_printf (gettext (finish_tree_string), info->file_name);
    else
    {
        if ((info->export_type == XML_EXPORT_REGISTER) && (info->account == NULL))
            text = g_strdup_printf (gettext (finish_trans_search_gl_string), info->file_name);
        else
            text = g_strdup_printf (gettext (finish_trans_string), info->file_name, info->csva.num_accounts);
    }
    gtk_label_set_text (GTK_LABEL(info->finish_label), text);
    g_free (text);

    /* Test if the filename exists */
    if (g_file_test (info->file_name, G_FILE_TEST_EXISTS))
    {
        const char *format = _("The file %s already exists. "
                               "Are you sure you want to overwrite it?");

        /* if user says cancel, we should go back a page */
        if (!gnc_verify_dialog (GTK_WINDOW (assistant), FALSE, format, info->file_name))
            gtk_assistant_previous_page (assistant);
    }
    /* Enable the Assistant Buttons */
    gtk_assistant_set_page_complete (assistant, info->finish_label, TRUE);
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
csv_export_assistant_destroy_cb (GtkWidget *object, gpointer user_data)
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
    if (info->mid_sep)
        g_free (info->mid_sep);

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->assistant));
    gtk_widget_destroy (info->assistant);
}

/*******************************************************
 * Create the Assistant
 *******************************************************/
static GtkWidget *
csv_export_assistant_create (CsvExportInfo *info)
{
    GtkBuilder *builder;
    GtkWidget *h_box;
    GtkWidget *button;
    GtkWidget *table, *hbox;

    builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder , "assistant-csv-export.glade", "csv_export_assistant");
    info->assistant = GTK_WIDGET(gtk_builder_get_object (builder, "csv_export_assistant"));

    // Set the style context for this assistant so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(info->assistant), "GncAssistExport");

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
        GtkWidget *chkbox = GTK_WIDGET(gtk_builder_get_object(builder, "simple_layout"));

        // Don't provide simple export layout for search registers and General Journal
        if ((info->export_type == XML_EXPORT_TREE) || (info->account == NULL))
            gtk_widget_destroy (chkbox);
        gtk_assistant_remove_page (GTK_ASSISTANT(info->assistant), 1); //remove accounts page
    }
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
        gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);
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

        button = GTK_WIDGET(gtk_builder_get_object (builder, "select_all_button"));
        info->csva.select_button = button;
        g_signal_connect (G_OBJECT(button), "clicked",
                          G_CALLBACK(csv_export_select_all_clicked_cb), info);

        g_signal_connect (G_OBJECT(info->csva.account_treeview), "cursor_changed",
                          G_CALLBACK(csv_export_cursor_changed_cb), info);

        /* Set the date info */
        button = GTK_WIDGET(gtk_builder_get_object (builder, "show_range"));

        /* Get the Earliest and Latest dates in Book */
        get_earliest_and_latest_in_book (info, gnc_get_current_book());

        info->csvd.start_time = info->csvd.earliest_time;
        info->csvd.end_time = info->csvd.latest_time;
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
        gnc_date_edit_set_time (GNC_DATE_EDIT(info->csvd.start_date), info->csvd.start_time);
        g_signal_connect (G_OBJECT(info->csvd.start_date), "date-changed",
                        G_CALLBACK(csv_export_date_changed_cb), info);

        /* End date info */
        info->csvd.end_date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
        hbox = GTK_WIDGET(gtk_builder_get_object (builder, "end_date_hbox"));
        gtk_box_pack_start (GTK_BOX(hbox), info->csvd.end_date, TRUE, TRUE, 0);
        gtk_widget_show (info->csvd.end_date);
        gnc_date_edit_set_time (GNC_DATE_EDIT(info->csvd.end_date), info->csvd.end_time);
        g_signal_connect (G_OBJECT (info->csvd.end_date), "date-changed",
                        G_CALLBACK (csv_export_date_changed_cb), info);

        /* Load Accounts */
        show_acct_type_accounts (info);
        update_accounts_tree (info);
    }

    /* File chooser Page */
    info->file_page = GTK_WIDGET(gtk_builder_get_object(builder, "file_page"));
    info->file_chooser = gtk_file_chooser_widget_new (GTK_FILE_CHOOSER_ACTION_SAVE);

    g_signal_connect (G_OBJECT(info->file_chooser), "selection-changed",
                      G_CALLBACK(csv_export_file_chooser_selection_changed_cb), info);

    g_signal_connect (G_OBJECT(info->file_chooser), "file-activated",
                      G_CALLBACK(csv_export_file_chooser_file_activated_cb), info);

    gtk_box_pack_start (GTK_BOX (info->file_page), info->file_chooser, TRUE, TRUE, 6);
    gtk_widget_show (info->file_chooser);

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
        gnc_prefs_bind (GNC_PREFS_GROUP, GNC_PREF_PANED_POS, object, "position");
    }

    gtk_builder_connect_signals (builder, info);
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
        info->account = acc;
    if ((export_type == XML_EXPORT_REGISTER) && acc)
        info->csva.num_accounts = 1;

    csv_export_assistant_create (info);
    gnc_register_gui_component (ASSISTANT_CSV_EXPORT_CM_CLASS,
                                NULL, csv_export_close_handler,
                                info);
    gtk_widget_show_all (info->assistant);
    gnc_window_adjust_for_screen (GTK_WINDOW(info->assistant));
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
