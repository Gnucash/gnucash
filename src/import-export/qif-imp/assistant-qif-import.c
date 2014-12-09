/********************************************************************\
 * assistant-qif-import.c -- window for importing QIF files         *
 *                        (GnuCash)                                 *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
 * Copyright (c) 2011 Robert Fewell                                 *
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <libguile.h>
#include <sys/time.h>
#include <unistd.h>

#include "Account.h"
#include "Transaction.h"
#include "dialog-account-picker.h"
#include "dialog-commodity.h"
#include "dialog-progress.h"
#include "dialog-utils.h"
#include "dialog-file-access.h"
#include "assistant-qif-import.h"
#include "assistant-utils.h"
#include "gnc-component-manager.h"
#include "qof.h"
#include "gnc-file.h"
#include "gnc-gui-query.h"
#include "gnc-guile-utils.h"
#include "gnc-currency-edit.h"
#include "gnc-ui-util.h"
#include "gnc-gtk-utils.h"
#include "gnc-main-window.h"
#include "gnc-plugin-page-account-tree.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "guile-mappings.h"

#include "swig-runtime.h"

#define ASSISTANT_QIF_IMPORT_CM_CLASS "assistant-qif-import"
#define GNC_PREFS_GROUP   "dialogs.import.qif"
#define GNC_PREF_SHOW_DOC "show-doc"
#define GNC_PREF_DEFAULT_TRANS_STATUS_CLEARED "default-status-cleared"
#define GNC_PREF_DEFAULT_TRANS_STATUS_NOTCLEARED "default-status-notcleared"
#define GNC_PREF_DEFAULT_TRANS_STATUS_RECONCILED "default-status-reconciled"

#define PREV_ROW "prev_row"

static QofLogModule log_module = GNC_MOD_ASSISTANT;

enum filename_cols
{
    FILENAME_COL_INDEX = 0,
    FILENAME_COL_NAME,
    NUM_FILENAME_COLS
};

enum account_cols
{
    ACCOUNT_COL_INDEX = 0,
    ACCOUNT_COL_QIF_NAME,
    ACCOUNT_COL_GNC_NAME,
    ACCOUNT_COL_NEW,
    ACCOUNT_COL_ELLIPSIZE,
    NUM_ACCOUNT_COLS
};

enum qif_trans_cols
{
    QIF_TRANS_COL_INDEX = 0,
    QIF_TRANS_COL_DATE,
    QIF_TRANS_COL_DESCRIPTION,
    QIF_TRANS_COL_AMOUNT,
    QIF_TRANS_COL_CHECKED,
    NUM_QIF_TRANS_COLS
};

struct _qifimportwindow
{
    GtkWidget * window;
    GtkWidget * assistant;

    /* Widgets on the file selection page. */
    GtkWidget * filename_entry;

    /* File loading progress page. */
    GtkWidget * load_pause;
    GtkWidget * load_start;
    GtkWidget * load_log;
    GNCProgressDialog *load_progress;

    /* Widgets on the default account page. */
    GtkWidget * acct_entry;

    /* Widgets on the date format page. */
    GtkWidget * date_format_combo;

    /* Widgets on the files loaded page. */
    GtkWidget * selected_file_view;
    GtkWidget * unload_file_btn;

    /* Widgets on the account matching page. */
    GtkWidget * acct_view;
    GtkWidget * acct_view_count;
    GtkWidget * acct_view_btn;

    /* Widgets on the category matching page. */
    GtkWidget * cat_view;
    GtkWidget * cat_view_count;
    GtkWidget * cat_view_btn;

    /* Widgets on the memo matching page. */
    GtkWidget * memo_view;
    GtkWidget * memo_view_count;
    GtkWidget * memo_view_btn;

    /* Widgets on the currency & book options page. */
    GtkWidget * currency_picker;
    GtkWidget * book_option_label;
    GtkWidget * book_option_message;

    /* Widgets on the commodity page. */
    gint        num_new_pages;

    /* Conversion progress page. */
    GtkWidget * convert_pause;
    GtkWidget * convert_start;
    GtkWidget * convert_log;
    GNCProgressDialog *convert_progress;

    /* Widgets on the duplicates page. */
    GtkWidget * new_transaction_view;
    GtkWidget * old_transaction_view;

    /* Widgets on the summary page. */
    GtkWidget * summary_text;

    GList     * commodity_pages;

    gboolean  show_doc_pages;
    gboolean  ask_date_format;
    gboolean  busy;
    gboolean  load_stop;
    gboolean  acct_tree_found;
    gboolean  new_book;

    SCM       imported_files;
    SCM       selected_file;

    SCM       acct_map_info;
    SCM       acct_display_info;

    SCM       cat_map_info;
    SCM       cat_display_info;

    SCM       memo_map_info;
    SCM       memo_display_info;

    SCM       gnc_acct_info;
    SCM       security_hash;
    SCM       security_prefs;
    SCM       new_securities;
    GList   * new_namespaces;
    SCM       ticker_map;

    SCM       imported_account_tree;
    SCM       match_transactions;
    SCM       transaction_status;
    int       selected_transaction;
};

struct _qifassistantpage
{
    GtkWidget     *page;
    GtkWidget     *namespace_combo;
    GtkWidget     *name_entry;
    GtkWidget     *mnemonic_entry;
    gnc_commodity *commodity;
    SCM            hash_key;
};

typedef struct _qifassistantpage QIFAssistantPage;

static void gnc_ui_qif_import_assistant_destroy (GtkObject *object, gpointer user_data);
static void gnc_ui_qif_import_assistant_close_handler (gpointer user_data );

void gnc_ui_qif_import_cancel_cb (GtkAssistant *gtkassistant, gpointer user_data);
void gnc_ui_qif_import_prepare_cb (GtkAssistant *assistant, GtkWidget *page, gpointer user_data);
void gnc_ui_qif_import_finish_cb (GtkAssistant *gtkassistant, gpointer user_data);
void gnc_ui_qif_import_close_cb (GtkAssistant *gtkassistant, gpointer user_data);

void gnc_ui_qif_import_intro_prepare (GtkAssistant  *assistant, gpointer user_data);

void gnc_ui_qif_import_load_file_prepare (GtkAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_select_file_cb (GtkButton *button, gpointer user_data);

void gnc_ui_qif_import_load_progress_prepare (GtkAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_load_progress_pause_cb (GtkButton *button, gpointer user_data);
void gnc_ui_qif_import_load_progress_start_cb (GtkButton * button, gpointer user_data);

void gnc_ui_qif_import_date_format_prepare (GtkAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_date_valid_cb (GtkWidget *widget, gpointer user_data);

void gnc_ui_qif_import_account_prepare (GtkAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_acct_valid_cb (GtkWidget *widget, gpointer user_data);

void gnc_ui_qif_import_loaded_files_prepare (GtkAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_load_another_cb (GtkButton *button, gpointer user_data);
void gnc_ui_qif_import_unload_file_cb (GtkButton *button, gpointer user_data);

static void update_file_page (QIFImportWindow * wind);

void gnc_ui_qif_import_account_match_prepare (GtkAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_account_doc_prepare (GtkAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_account_rematch_cb (GtkButton *button, gpointer user_data);

void gnc_ui_qif_import_catagory_match_prepare (GtkAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_catagory_doc_prepare (GtkAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_category_rematch_cb (GtkButton *button, gpointer user_data);

void gnc_ui_qif_import_memo_match_prepare (GtkAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_memo_doc_prepare (GtkAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_memo_rematch_cb (GtkButton *button, gpointer user_data);

void gnc_ui_qif_import_currency_prepare (GtkAssistant *assistant, gpointer user_data);

void gnc_ui_qif_import_commodity_new_prepare (GtkAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_commodity_doc_prepare (GtkAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_comm_changed_cb (GtkWidget *widget, gpointer user_data);

void gnc_ui_qif_import_convert_progress_prepare (GtkAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_convert_progress_pause_cb (GtkButton * button, gpointer user_data);
void gnc_ui_qif_import_convert_progress_start_cb(GtkButton * button, gpointer user_data);

void gnc_ui_qif_import_duplicates_match_prepare (GtkAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_duplicates_doc_prepare (GtkAssistant *assistant, gpointer user_data);

void gnc_ui_qif_import_end_page_prepare (GtkAssistant *assistant, gpointer user_data);

void gnc_ui_qif_import_summary_page_prepare (GtkAssistant *assistant, gpointer user_data);


/****************************************************************
 * update_account_picker_page
 *
 * Generic function to update an account_picker page.  This
 * generalizes the code shared whenever any QIF -> GNC mapper is
 * updating it's LIST STORE.  It asks the Scheme side to guess some account
 * translations and then shows the account name and suggested
 * translation in the Accounts page view (acount picker list).
 ****************************************************************/
static void
update_account_picker_page(QIFImportWindow * wind, SCM make_display,
                           GtkWidget *view, SCM map_info, SCM * display_info)
{

    SCM  get_qif_name = scm_c_eval_string("qif-map-entry:qif-name");
    SCM  get_gnc_name = scm_c_eval_string("qif-map-entry:gnc-name");
    SCM  get_new      = scm_c_eval_string("qif-map-entry:new-acct?");
    SCM  accts_left;
    gchar *qif_name = NULL;
    gchar *gnc_name = NULL;
    gboolean checked;
    gint row = 0;
    gint prev_row;
    GtkListStore *store;
    GtkTreeIter iter;
    GtkTreePath *path;
    GtkTreeSelection *selection;

    store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(view)));

    /* now get the list of strings to display in the gtk_list_store widget */
    accts_left = scm_call_3(make_display,
                            wind->imported_files,
                            map_info,
                            wind->gnc_acct_info);

    scm_gc_unprotect_object(*display_info);
    *display_info = accts_left;
    scm_gc_protect_object(*display_info);

    /* clear the list */
    gtk_list_store_clear(store);

    while (!scm_is_null(accts_left))
    {
        qif_name = gnc_scm_call_1_to_string(get_qif_name, SCM_CAR(accts_left));
        gnc_name = gnc_scm_call_1_to_string(get_gnc_name, SCM_CAR(accts_left));
        checked  = (scm_call_1(get_new, SCM_CAR(accts_left)) == SCM_BOOL_T);

        gtk_list_store_append(store, &iter);
        gtk_list_store_set(store, &iter,
                           ACCOUNT_COL_INDEX,     row++,
                           ACCOUNT_COL_QIF_NAME,  qif_name,
                           ACCOUNT_COL_GNC_NAME,  gnc_name,
                           ACCOUNT_COL_NEW,       checked,
                           ACCOUNT_COL_ELLIPSIZE, PANGO_ELLIPSIZE_START,
                           -1);
        accts_left = SCM_CDR(accts_left);
        g_free (qif_name);
        g_free (gnc_name);
    }

    /* move to the old selected row */
    prev_row = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(store), PREV_ROW));
    if (prev_row != -1)
    {
        selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
        path = gtk_tree_path_new_from_indices(prev_row, -1);
        gtk_tree_selection_select_path(selection, path);
        gtk_tree_path_free(path);
    }
    else
    {
        selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
        path = gtk_tree_path_new_from_indices( 0, -1);
        gtk_tree_selection_select_path(selection, path);
        gtk_tree_path_free(path);
    }
}


/****************************************************************
 * update_account_page
 *
 * update the QIF account -> GNC Account picker
 ****************************************************************/
static void
update_account_page(QIFImportWindow * wind)
{

    SCM  make_account_display = scm_c_eval_string("qif-dialog:make-account-display");

    update_account_picker_page(wind, make_account_display, wind->acct_view,
                               wind->acct_map_info, &(wind->acct_display_info));
}


/****************************************************************
 * update_category_page
 *
 * update the QIF category -> GNC Account picker
 ****************************************************************/
static void
update_category_page(QIFImportWindow * wind)
{
    SCM  make_category_display = scm_c_eval_string("qif-dialog:make-category-display");

    update_account_picker_page(wind, make_category_display, wind->cat_view,
                               wind->cat_map_info, &(wind->cat_display_info));
}


/****************************************************************
 * update_memo_page
 *
 * update the QIF memo -> GNC Account picker
 ****************************************************************/
static void
update_memo_page(QIFImportWindow * wind)
{
    SCM  make_memo_display = scm_c_eval_string("qif-dialog:make-memo-display");

    update_account_picker_page(wind, make_memo_display, wind->memo_view,
                               wind->memo_map_info, &(wind->memo_display_info));
}


/****************************************************************
 * gnc_ui_qif_import_commodity_destroy
 *
 * This function destroys any commodity pages.
 ****************************************************************/
static void
gnc_ui_qif_import_commodity_destroy(QIFImportWindow * wind)
{
    GList              *pageptr;
    GtkWidget          *gtkpage;
    QIFAssistantPage   *page;

    for (pageptr = wind->commodity_pages; pageptr; pageptr = pageptr->next)
    {
        gtkpage   = pageptr->data;
        page      = g_object_get_data(G_OBJECT(gtkpage), "page_struct");

        /* Unprotect the Scheme hash key. */
        scm_gc_unprotect_object(page->hash_key);

        /* Free the memory allocated for the page's struct. */
        g_free(page);
    }

    /* Free the list of pages. */
    g_list_free(wind->commodity_pages);
    wind->commodity_pages = NULL;
}


/**********************************************
 * gnc_ui_qif_import_assistant_destroy
 * close the QIF Import assistant window
 **********************************************/
static void
gnc_ui_qif_import_assistant_destroy(GtkObject *object, gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    /* Destroy the progress dialog helpers. */
    gnc_progress_dialog_destroy(wind->load_progress);

    /* Destroy any commodity pages. */
    gnc_ui_qif_import_commodity_destroy(wind);

    gnc_unregister_gui_component_by_data(ASSISTANT_QIF_IMPORT_CM_CLASS, wind);

    gtk_widget_destroy(wind->window);

    scm_gc_unprotect_object(wind->imported_files);
    scm_gc_unprotect_object(wind->selected_file);
    scm_gc_unprotect_object(wind->gnc_acct_info);
    scm_gc_unprotect_object(wind->cat_display_info);
    scm_gc_unprotect_object(wind->cat_map_info);
    scm_gc_unprotect_object(wind->memo_display_info);
    scm_gc_unprotect_object(wind->memo_map_info);
    scm_gc_unprotect_object(wind->acct_display_info);
    scm_gc_unprotect_object(wind->acct_map_info);
    scm_gc_unprotect_object(wind->security_hash);
    scm_gc_unprotect_object(wind->security_prefs);
    scm_gc_unprotect_object(wind->new_securities);
    scm_gc_unprotect_object(wind->ticker_map);
    scm_gc_unprotect_object(wind->imported_account_tree);
    scm_gc_unprotect_object(wind->match_transactions);

    g_free(wind);
}


/****************************************************************
 * gnc_ui_qif_import_select_loaded_file_cb
 * callback when a file is clicked in the "loaded files" page
 ****************************************************************/
static void
gnc_ui_qif_import_select_loaded_file_cb(GtkTreeSelection *selection,
                                        gpointer          user_data)
{
    QIFImportWindow * wind = user_data;
    GtkTreeModel *model;
    GtkTreeIter iter;
    gint row;
    GtkWidget *button;

    button = (wind->unload_file_btn);
    if (gtk_tree_selection_get_selected(selection, &model, &iter))
    {
        gtk_tree_model_get(model, &iter, FILENAME_COL_INDEX, &row, -1);
        if (scm_is_list(wind->imported_files) &&
                (scm_ilength(wind->imported_files) > row))
        {
            scm_gc_unprotect_object(wind->selected_file);
            wind->selected_file = scm_list_ref(wind->imported_files,
                                               scm_from_int (row));
            scm_gc_protect_object(wind->selected_file);
            g_object_set(button, "sensitive", TRUE, (gchar*)NULL);
        }
    }
    else
    {
        scm_gc_unprotect_object(wind->selected_file);
        wind->selected_file = SCM_BOOL_F;
        scm_gc_protect_object(wind->selected_file);
        g_object_set(button, "sensitive", FALSE, (gchar*)NULL);
    }
}


/****************************************************
 * create_account_picker_view
 ****************************************************/
static void
create_account_picker_view(GtkWidget *widget,
                           const gchar *col_name,
                           GCallback activate_cb,
                           GCallback select_cb,
                           gpointer user_data)
{
    GtkTreeView *view = GTK_TREE_VIEW(widget);
    GtkTreeSelection *selection = gtk_tree_view_get_selection(view);
    GtkListStore *store;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    store = gtk_list_store_new(NUM_ACCOUNT_COLS, G_TYPE_INT, G_TYPE_STRING,
                               G_TYPE_STRING, G_TYPE_BOOLEAN,
                               PANGO_TYPE_ELLIPSIZE_MODE);
    gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
    g_object_unref(store);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(col_name,
             renderer,
             "text",
             ACCOUNT_COL_QIF_NAME,
             "ellipsize",
             ACCOUNT_COL_ELLIPSIZE,
             NULL);
    g_object_set(column, "expand", TRUE, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("GnuCash account name"),
             renderer,
             "text",
             ACCOUNT_COL_GNC_NAME,
             "ellipsize",
             ACCOUNT_COL_ELLIPSIZE,
             NULL);

    g_object_set(column, "expand", TRUE, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_toggle_new();
    g_object_set(renderer, "activatable", FALSE, NULL);
    column = gtk_tree_view_column_new_with_attributes(_("New?"),
             renderer,
             "active",
             ACCOUNT_COL_NEW,
             NULL);
    gtk_tree_view_append_column(view, column);

    g_object_set_data(G_OBJECT(store), PREV_ROW, GINT_TO_POINTER(-1));

    /* Connect the signal handlers. */
    g_signal_connect(view, "row-activated", G_CALLBACK(activate_cb), user_data);
    g_signal_connect(selection, "changed", G_CALLBACK(select_cb), user_data);

    /* Allow multiple rows to be selected. */
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
}


/********************************************************************
 * rematch_line
 *
 * This is a helper function for tree controls used by some assistant
 * pages for mapping QIF values to GnuCash accounts. It processes
 * the selected rows when a user tries to edit the account mappings.
 * The account picker is displayed, and the chosen GnuCash account
 * becomes the new mapping for each row.  Finally, the update_page
 * function is called.
 ********************************************************************/
static void
rematch_line(QIFImportWindow *wind, GtkTreeSelection *selection,
             SCM display_info, SCM map_info,
             void (*update_page)(QIFImportWindow *))
{
    SCM           get_qif_name = scm_c_eval_string("qif-map-entry:qif-name");
    SCM           get_gnc_name = scm_c_eval_string("qif-map-entry:gnc-name");
    SCM           set_gnc_name = scm_c_eval_string("qif-map-entry:set-gnc-name!");
    SCM           map_entry;
    SCM           gnc_name;
    GList        *pathlist;
    GList        *current;
    GtkTreeModel *model;
    GtkTreeIter   iter;
    gint          row;

    /* Get a list of selected rows. */
    pathlist = gtk_tree_selection_get_selected_rows(selection, &model);
    if (!pathlist)
        return;

    /*
     * Update the first selected row.
     */

    /* Get the row number of the first selected row. */
    if (!gtk_tree_model_get_iter(model, &iter, (GtkTreePath *) pathlist->data))
        return;
    gtk_tree_model_get(model, &iter, ACCOUNT_COL_INDEX, &row, -1);

    /* Save the row number. */
    g_object_set_data(G_OBJECT(model), PREV_ROW, GINT_TO_POINTER(row));
    if (row == -1)
        return;

    /* Find the <qif-map-entry> corresponding to the selected row. */
    map_entry = scm_list_ref(display_info, scm_from_int (row));

    /* Call the account picker to update it. */
    if (!qif_account_picker_dialog(wind, map_entry))
        return;
    gnc_name = scm_call_1(get_gnc_name, map_entry);

    /* Update the mapping hash table. */
    scm_hash_set_x(map_info, scm_call_1(get_qif_name, map_entry), map_entry);

    /*
     * Map all other selected rows to the same GnuCash account.
     */
    for (current = pathlist->next; current; current = current->next)
    {
        /* Get the row number. */
        gtk_tree_model_get_iter(model, &iter, (GtkTreePath *) current->data);
        gtk_tree_model_get(model, &iter, ACCOUNT_COL_INDEX, &row, -1);

        /* Update the <qif-map-entry> for the selected row. */
        map_entry = scm_list_ref(display_info, scm_from_int (row));
        scm_call_2(set_gnc_name, map_entry, gnc_name);

        /* Update the mapping hash table. */
        scm_hash_set_x(map_info, scm_call_1(get_qif_name, map_entry), map_entry);
    }

    /* Free the path list. */
    g_list_foreach(pathlist, (GFunc) gtk_tree_path_free, NULL);
    g_list_free(pathlist);

    /* Update the display. */
    update_page(wind);
}


/********************************************************************
 * gnc_ui_qif_import_account_activate_cb
 *
 * This handler is invoked when a row is double-clicked in the "Match
 * QIF accounts to GnuCash accounts" page.
 ********************************************************************/
static void
gnc_ui_qif_import_account_activate_cb(GtkTreeView *view, GtkTreePath *path,
                                      GtkTreeViewColumn *column,
                                      gpointer user_data)
{
    QIFImportWindow  *wind = user_data;

    g_return_if_fail(wind);

    rematch_line(wind, gtk_tree_view_get_selection(view),
                 wind->acct_display_info, wind->acct_map_info,
                 update_account_page);
}


/********************************************************************
 * gnc_ui_qif_import_account_select_cb
 *
 * This handler is invoked when the selection of account matchings
 * has changed.  It updates the selection count and enables/disables
 * the "Change" button.
 ********************************************************************/
static void
gnc_ui_qif_import_account_select_cb(GtkTreeSelection *selection,
                                    gpointer user_data)
{
    QIFImportWindow  *wind = user_data;
    gint              count = gtk_tree_selection_count_selected_rows(selection);
    gchar            *count_str;

    g_return_if_fail(wind);

    /* Update the "items selected" count. */
    if (wind->acct_view_count)
    {
        count_str = g_strdup_printf("%d", count);
        gtk_label_set_text(GTK_LABEL(wind->acct_view_count), count_str);
        g_free(count_str);
    }

    /* Enable/disable the Change button. */
    if (wind->acct_view_btn)
    {
        if (count)
            gtk_widget_set_sensitive(wind->acct_view_btn, TRUE);
        else
            gtk_widget_set_sensitive(wind->acct_view_btn, FALSE);
    }
}


/********************************************************************
 * gnc_ui_qif_import_category_activate_cb
 *
 * This handler is invoked when a row is double-clicked in the "Match
 * QIF categories to GnuCash accounts" page.
 ********************************************************************/
static void
gnc_ui_qif_import_category_activate_cb(GtkTreeView *view, GtkTreePath *path,
                                       GtkTreeViewColumn *column,
                                       gpointer user_data)
{
    QIFImportWindow *wind = user_data;
    GtkTreeSelection *selection;

    g_return_if_fail(view && wind);
    selection = gtk_tree_view_get_selection(view);

    rematch_line(wind, selection, wind->cat_display_info, wind->cat_map_info,
                 update_category_page);
}


/********************************************************************
 * gnc_ui_qif_import_category_select_cb
 *
 * This handler is invoked when the selection of category matchings
 * has changed.  It updates the selection count and enables/disables
 * the "Change" button.
 ********************************************************************/
static void
gnc_ui_qif_import_category_select_cb(GtkTreeSelection *selection,
                                     gpointer user_data)
{
    QIFImportWindow  *wind = user_data;
    gint              count = gtk_tree_selection_count_selected_rows(selection);
    gchar            *count_str;

    g_return_if_fail(wind);

    /* Update the "items selected" count. */
    if (wind->cat_view_count)
    {
        count_str = g_strdup_printf("%d", count);
        gtk_label_set_text(GTK_LABEL(wind->cat_view_count), count_str);
        g_free(count_str);
    }

    /* Enable/disable the Change button. */
    if (wind->cat_view_btn)
    {
        if (count)
            gtk_widget_set_sensitive(wind->cat_view_btn, TRUE);
        else
            gtk_widget_set_sensitive(wind->cat_view_btn, FALSE);
    }
}


/********************************************************************
 *  gnc_ui_qif_import_memo_activate_cb
 *
 * This handler is invoked when a row is double-clicked in the "Match
 * QIF payee/memo to GnuCash accounts" page.
 ********************************************************************/
static void
gnc_ui_qif_import_memo_activate_cb(GtkTreeView *view, GtkTreePath *path,
                                   GtkTreeViewColumn *column,
                                   gpointer user_data)
{
    QIFImportWindow *wind = user_data;
    GtkTreeSelection *selection;

    g_return_if_fail(view && wind);
    selection = gtk_tree_view_get_selection(view);

    rematch_line(wind, selection, wind->memo_display_info, wind->memo_map_info,
                 update_memo_page);
}


/********************************************************************
 * gnc_ui_qif_import_memo_select_cb
 *
 * This handler is invoked when the selection of memo matchings
 * has changed.  It updates the selection count and enables/disables
 * the "Change" button.
 ********************************************************************/
static void
gnc_ui_qif_import_memo_select_cb(GtkTreeSelection *selection,
                                 gpointer user_data)
{
    QIFImportWindow  *wind = user_data;
    gint              count = gtk_tree_selection_count_selected_rows(selection);
    gchar            *count_str;

    g_return_if_fail(wind);

    /* Update the "items selected" count. */
    if (wind->memo_view_count)
    {
        count_str = g_strdup_printf("%d", count);
        gtk_label_set_text(GTK_LABEL(wind->memo_view_count), count_str);
        g_free(count_str);
    }

    /* Enable/disable the Change button. */
    if (wind->memo_view_btn)
    {
        if (count)
            gtk_widget_set_sensitive(wind->memo_view_btn, TRUE);
        else
            gtk_widget_set_sensitive(wind->memo_view_btn, FALSE);
    }
}


/*********************************************
 * new_security_page
 *********************************************/
static QIFAssistantPage *
new_security_page(SCM security_hash_key, gnc_commodity *comm, QIFImportWindow *wind )
{

    QIFAssistantPage *retval = g_new0(QIFAssistantPage, 1);
    GtkListStore *store;
    GtkWidget    *table;
    GtkWidget    *label;
    gchar        *title = NULL;
    const char   *str;
    GtkWidget    *page;
    char         *name_tooltip =
        _("Enter a name or short description, such as \"Red Hat Stock\".");
    char         *mnemonic_tooltip =
        _("Enter the ticker symbol or other well known abbreviation, such as"
          " \"RHT\". If there isn't one, or you don't know it, create your own.");
    char         *namespace_tooltip =
        _("Select the exchange on which the symbol is traded, or select the"
          " type of investment (such as FUND for mutual funds.) If you don't"
          " see your exchange or an appropriate investment type, you can"
          " enter a new one.");

    /* Make the page widget. */
    page = gtk_vbox_new( FALSE, 0 );
    retval->page = page;
    g_object_set_data(G_OBJECT(retval->page), "page_struct", retval);
    page = retval->page;

    /* Insert the new page */
    gtk_assistant_insert_page (GTK_ASSISTANT(wind->window), page, 14);
    gtk_assistant_set_page_type(GTK_ASSISTANT(wind->window), page, GTK_ASSISTANT_PAGE_PROGRESS );
    gtk_assistant_set_page_complete (GTK_ASSISTANT (wind->window), page, TRUE);
    gtk_assistant_update_buttons_state (GTK_ASSISTANT (wind->window));

    /* Save the commodity and the hash table key. */
    retval->commodity = comm;
    retval->hash_key = security_hash_key;
    scm_gc_protect_object(retval->hash_key);

    /* Set the page title. */
    str = gnc_commodity_get_mnemonic(comm);
    str = str ? str : "";
    title = _("Enter information about");
    title =  g_strdup_printf("%s \"%s\"", title, str);
    gtk_assistant_set_page_title(GTK_ASSISTANT(wind->window), page, title);
    g_free(title);

    /*
     * Add all the widgets to the page.
     */
    table = gtk_table_new(3, 2, FALSE);
    gtk_table_set_row_spacings(GTK_TABLE(table), 6);
    gtk_table_set_col_spacings(GTK_TABLE(table), 12);

    /* Name entry */
    retval->name_entry = gtk_entry_new();
    gtk_entry_set_text(GTK_ENTRY(retval->name_entry),
                       gnc_commodity_get_fullname(comm));
    label = gtk_label_new_with_mnemonic(_("_Name or description:"));
    gtk_label_set_mnemonic_widget(GTK_LABEL(label), retval->name_entry);
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

    gtk_widget_set_tooltip_text(label, name_tooltip);
    gtk_widget_set_tooltip_text(retval->name_entry, name_tooltip);

    gtk_table_attach(GTK_TABLE(table), label, 0, 1, 0, 1,
                     GTK_FILL, GTK_EXPAND | GTK_FILL, 0, 0);
    gtk_table_attach_defaults(GTK_TABLE(table), retval->name_entry,
                              1, 2, 0, 1);

    g_signal_connect (retval->name_entry, "changed",
                      G_CALLBACK (gnc_ui_qif_import_comm_changed_cb), wind);

    /* Mnemonic entry */
    retval->mnemonic_entry = gtk_entry_new();
    gtk_entry_set_text(GTK_ENTRY(retval->mnemonic_entry),
                       gnc_commodity_get_mnemonic(comm));
    label = gtk_label_new_with_mnemonic(
                _("_Ticker symbol or other abbreviation:"));
    gtk_label_set_mnemonic_widget(GTK_LABEL(label), retval->mnemonic_entry);
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

    gtk_widget_set_tooltip_text(label, mnemonic_tooltip);
    gtk_widget_set_tooltip_text(retval->mnemonic_entry, mnemonic_tooltip);

    gtk_table_attach(GTK_TABLE(table), label, 0, 1, 1, 2,
                     GTK_FILL, GTK_EXPAND | GTK_FILL, 0, 0);
    gtk_table_attach_defaults(GTK_TABLE(table), retval->mnemonic_entry,
                              1, 2, 1, 2);

    g_signal_connect (retval->mnemonic_entry, "changed",
                      G_CALLBACK (gnc_ui_qif_import_comm_changed_cb), wind);

    /* Namespace entry */
    store = gtk_list_store_new (1, G_TYPE_STRING);
    retval->namespace_combo = gtk_combo_box_new_with_model_and_entry(GTK_TREE_MODEL(store));
    g_object_unref(store);

    /* Set the column for the text */
    gtk_combo_box_set_entry_text_column (GTK_COMBO_BOX(retval->namespace_combo), 0);

    gnc_cbwe_add_completion(GTK_COMBO_BOX(retval->namespace_combo));
    label = gtk_label_new_with_mnemonic(
                _("_Exchange or abbreviation type:"));
    gtk_label_set_mnemonic_widget(GTK_LABEL(label), retval->namespace_combo);
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

    gtk_widget_set_tooltip_text(label, namespace_tooltip);
    gtk_widget_set_tooltip_text(retval->namespace_combo, namespace_tooltip);

    gtk_table_attach(GTK_TABLE(table), label, 0, 1, 2, 3,
                     GTK_FILL, GTK_EXPAND | GTK_FILL, 0, 0);
    gtk_table_attach_defaults(GTK_TABLE(table), retval->namespace_combo,
                              1, 2, 2, 3);

    gtk_container_set_border_width(GTK_CONTAINER(page), 12);

    gtk_box_pack_start(GTK_BOX(page), table, FALSE, FALSE, 12);

    return retval;
}


/********************************************************************
 * prepare_security_pages
 *
 * Prepare the assistant page for each security.
 ********************************************************************/
static void
prepare_security_pages(QIFImportWindow * wind)
{
    SCM   hash_ref  = scm_c_eval_string("hash-ref");
    SCM   securities;
    SCM   comm_ptr_token;

    GList          * current;
    gnc_commodity  * commodity;
    QIFAssistantPage   * new_page;

    /*
     * Make assistant pages for each new QIF security.
     */
    gnc_set_busy_cursor(NULL, TRUE);
    securities = wind->new_securities;
    current = wind->commodity_pages;
    while (!scm_is_null(securities) && (securities != SCM_BOOL_F))
    {
        if (current)
        {
            /* The page has already been made. */
            current = current->next;
        }
        else
        {
            /* Get the GnuCash commodity corresponding to the new QIF security. */
            comm_ptr_token = scm_call_2(hash_ref,
                                        wind->security_hash,
                                        SCM_CAR(securities));
#define FUNC_NAME "new_security_page"
            commodity = SWIG_MustGetPtr(comm_ptr_token,
                                        SWIG_TypeQuery("_p_gnc_commodity"), 1, 0);
#undef FUNC_NAME

            /* Build a new security page. */
            new_page = new_security_page(SCM_CAR(securities), commodity, wind);

            /* Add it to the list of security pages. */
            wind->commodity_pages = g_list_append(wind->commodity_pages,
                                                  new_page->page);

            gtk_widget_show_all(new_page->page);
        }
        wind->num_new_pages = wind->num_new_pages + 1;
        securities = SCM_CDR(securities);
    }
    gnc_unset_busy_cursor(NULL);
    PINFO("Number of New Security pages is %d", wind->num_new_pages);
}


/****************************************************************
 * gnc_ui_qif_import_commodity_update
 *
 * This function updates the commodities based on the values for
 * mnemonic, namespace, and name approved by the user.
 ****************************************************************/
static void
gnc_ui_qif_import_commodity_update(QIFImportWindow * wind)
{
    GList              *pageptr;
    GtkWidget          *gtkpage;
    QIFAssistantPage   *page;
    const gchar        *mnemonic = NULL;
    gchar              *name_space = NULL;
    const gchar        *fullname = NULL;
    gnc_commodity      *tab_commodity;

    for (pageptr = wind->commodity_pages; pageptr; pageptr = pageptr->next)
    {
        gtkpage   = pageptr->data;
        page      = g_object_get_data(G_OBJECT(gtkpage), "page_struct");

        /* Get any changes from the commodity page. */
        mnemonic  = gtk_entry_get_text(GTK_ENTRY(page->mnemonic_entry));
        name_space = gnc_ui_namespace_picker_ns(page->namespace_combo);
        fullname  = gtk_entry_get_text(GTK_ENTRY(page->name_entry));

        /* Update the commodity with the new values. */
        gnc_commodity_set_namespace(page->commodity, name_space);
        gnc_commodity_set_fullname(page->commodity, fullname);
        gnc_commodity_set_mnemonic(page->commodity, mnemonic);

        /* Add the commodity to the commodity table (if it isn't a duplicate). */
        tab_commodity = gnc_commodity_table_lookup(gnc_get_current_commodities(),
                        name_space, mnemonic);
        if (!tab_commodity || tab_commodity == page->commodity)
            tab_commodity = gnc_commodity_table_insert(gnc_get_current_commodities(),
                            page->commodity);

        /* Update the security hash table. */
        scm_hash_set_x(wind->security_hash,
                       page->hash_key,
                       SWIG_NewPointerObj(tab_commodity,
                                          SWIG_TypeQuery("_p_gnc_commodity"), 0));

        g_free(name_space);
    }
}


/****************************************************************
 * gnc_ui_qif_import_convert_undo
 *
 * This function launches the Scheme procedure that un-imports
 * any imported accounts and transactions.
 ****************************************************************/
static void
gnc_ui_qif_import_convert_undo(QIFImportWindow * wind)
{
    SCM undo = scm_c_eval_string("qif-import:qif-to-gnc-undo");

    gnc_set_busy_cursor(NULL, TRUE);

    /* Undo the conversion. */
    scm_call_1(undo, wind->imported_account_tree);

    /* There's no imported account tree any more. */
    scm_gc_unprotect_object(wind->imported_account_tree);
    wind->imported_account_tree = SCM_BOOL_F;
    scm_gc_protect_object(wind->imported_account_tree);

    /* Get rid of the list of matched transactions. */
    scm_gc_unprotect_object(wind->match_transactions);
    wind->match_transactions = SCM_BOOL_F;
    scm_gc_protect_object(wind->match_transactions);

    gnc_unset_busy_cursor(NULL);
}


/****************************************************************
 * refresh_old_transactions
 *
 * This function launches the Scheme procedure that refreshes
 * the old transactions.
 ****************************************************************/
static void
refresh_old_transactions(QIFImportWindow * wind, int selection)
{
    SCM          possible_matches;
    SCM          current_xtn;
    SCM          selected;
    Transaction  * gnc_xtn;
    Split        * gnc_split;
    const gchar  * amount_str;
    int          rownum = 0;
    GtkTreeView *view;
    GtkListStore *store;
    GtkTreeIter iter;

    view = GTK_TREE_VIEW(wind->old_transaction_view);
    store = GTK_LIST_STORE(gtk_tree_view_get_model(view));
    gtk_list_store_clear(store);

    if (wind->match_transactions != SCM_BOOL_F)
    {
        possible_matches = SCM_CDR(scm_list_ref(wind->match_transactions,
                                                scm_from_int (wind->selected_transaction)));
        scm_call_2(scm_c_eval_string("qif-import:refresh-match-selection"),
                   possible_matches, scm_from_int (selection));

        while (!scm_is_null(possible_matches))
        {
            current_xtn = SCM_CAR(possible_matches);
#define FUNC_NAME "xaccTransCountSplits"
            gnc_xtn     = SWIG_MustGetPtr(SCM_CAR(current_xtn),
                                          SWIG_TypeQuery("_p_Transaction"), 1, 0);
#undef FUNC_NAME
            selected    = SCM_CDR(current_xtn);

            if (xaccTransCountSplits(gnc_xtn) > 2)
            {
                amount_str = _("(split)");
            }
            else
            {
                gnc_split = xaccTransGetSplit(gnc_xtn, 0);
                amount_str =
                    xaccPrintAmount(gnc_numeric_abs(xaccSplitGetValue(gnc_split)),
                                    gnc_account_print_info
                                    (xaccSplitGetAccount(gnc_split), TRUE));
            }

            gtk_list_store_append(store, &iter);
            gtk_list_store_set
            (store, &iter,
             QIF_TRANS_COL_INDEX, rownum++,
             QIF_TRANS_COL_DATE, gnc_print_date(xaccTransRetDatePostedTS(gnc_xtn)),
             QIF_TRANS_COL_DESCRIPTION, xaccTransGetDescription(gnc_xtn),
             QIF_TRANS_COL_AMOUNT, amount_str,
             QIF_TRANS_COL_CHECKED, selected != SCM_BOOL_F,
             -1);

            possible_matches = SCM_CDR(possible_matches);
        }
    }
}


/****************************************************************
 * gnc_ui_qif_import_duplicate_new_select_cb
 *
 * This function is the call back for duplicate transactions.
 ****************************************************************/
static void
gnc_ui_qif_import_duplicate_new_select_cb(GtkTreeSelection *selection,
        QIFImportWindow  *wind)
{
    GtkTreeModel *model;
    GtkTreeIter iter;

    if (gtk_tree_selection_get_selected(selection, &model, &iter))
        gtk_tree_model_get(model, &iter,
                           QIF_TRANS_COL_INDEX, &wind->selected_transaction,
                           -1);
    refresh_old_transactions(wind, -1);
}


/****************************************************************
 * reset_ignore_old_select
 ****************************************************************/
static gboolean
reset_ignore_old_select(gboolean *ignore)
{
    *ignore = FALSE;
    return FALSE;
}


/****************************************************************
 * gnc_ui_qif_import_duplicate_old_select_cb
 *
 * This function is the call back for duplicate transactions.
 ****************************************************************/
static void
gnc_ui_qif_import_duplicate_old_select_cb(GtkTreeSelection *selection,
        QIFImportWindow  *wind)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    gint row;
    static gboolean ignore_old_select = FALSE;

    /* Get the current selection then clear it.  We're about to clear
     * the entire list store and rebuild it so this prevents errors. */
    if (!gtk_tree_selection_get_selected(selection, &model, &iter))
        return;
    gtk_tree_selection_unselect_all(selection);

    /* Getting a weird double call the first time a line is clicked.
     * Once via gtk_tree_view_button_press and then again via
     * gtk_tree_view_grab_focus. */
    if (ignore_old_select)
        return;
    ignore_old_select = TRUE;
    g_idle_add((GSourceFunc)reset_ignore_old_select, &ignore_old_select);

    /* Get the row the user clicked on and update the scheme
     * code/rebuild the list store.  */
    gtk_tree_model_get(model, &iter,
                       QIF_TRANS_COL_INDEX, &row,
                       -1);
    refresh_old_transactions(wind, row);
}


/********************************************************************
 * gnc_ui_qif_import_check_acct_tree
 *
 * Designed for use with gnc_main_window_foreach_page(), this
 * function determines whether an account tab is open in the main
 * window. The parameter user_data must point to a gboolean.
 ********************************************************************/
static void
gnc_ui_qif_import_check_acct_tree(GncPluginPage *page, gpointer user_data)
{
    gboolean *found = user_data;

    if (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page) && found)
        *found = TRUE;
}


/****************************************************************
 * do_cancel
 *
 * Clears out any imported data and shuts down the importer.
 ****************************************************************/
static void
do_cancel(QIFImportWindow * wind)
{
    GList                *pageptr;
    GtkWidget            *gtkpage;
    QIFAssistantPage     *page;
    gnc_commodity_table  *table;

    gnc_set_busy_cursor(NULL, TRUE);

    /* Remove any converted data. */
    gnc_ui_qif_import_convert_undo(wind);

    /* Remove any commodities created for assistant pages. */
    for (pageptr = wind->commodity_pages; pageptr; pageptr = pageptr->next)
    {
        gtkpage   = pageptr->data;
        page      = g_object_get_data(G_OBJECT(gtkpage), "page_struct");
        gnc_commodity_destroy(page->commodity);
    }

    /* Remove any namespaces created by the user. */
    table = gnc_get_current_commodities();
    while (wind->new_namespaces)
    {
        gnc_commodity_table_delete_namespace(table, (gchar *) wind->new_namespaces->data);

        /* Free the data and the list element. */
        g_free(wind->new_namespaces->data);
        wind->new_namespaces = g_list_delete_link(wind->new_namespaces,
                               wind->new_namespaces);
    }
    gnc_unset_busy_cursor(NULL);

    /* Destroy the assistant. */
    gnc_close_gui_component_by_data(ASSISTANT_QIF_IMPORT_CM_CLASS, wind);
}


/****************************************************************
 * cancel_timeout_cb
 *
 * This timer callback function waits until the busy flag
 * has been cleared before acting to cancel the import.
 ****************************************************************/
static gboolean
cancel_timeout_cb(gpointer data)
{
    QIFImportWindow *wind = data;

    if (wind->busy)
        /* Wait for timer to go off again. */
        return TRUE;

    /* The busy flag was lowered. Perform the cancel. */
    do_cancel(wind);

    /* Cancel the timer. */
    return FALSE;
}


/****************************************************************
 * gnc_ui_qif_import_cancel_cb
 *
 * Invoked when the "Cancel" button is clicked.
 ****************************************************************/
void
gnc_ui_qif_import_cancel_cb(GtkAssistant *gtkassistant, gpointer user_data)
{
    QIFImportWindow  *wind = user_data;
    gint currentpage = gtk_assistant_get_current_page(gtkassistant);
    GtkWidget *mypage = gtk_assistant_get_nth_page (gtkassistant, currentpage);
    const char *pagename = gtk_buildable_get_name(GTK_BUILDABLE(mypage));

    if (!g_strcmp0 (pagename, "summary_page"))
    {
        /* Hitting the window close button on the summary page should not
           invoke a cancel action. The import has finised at that point. */
        gnc_ui_qif_import_close_cb(gtkassistant, user_data);
    }
    else if (wind->busy)
    {
        /* Cancel any long-running Scheme operation. */
        scm_c_eval_string("(qif-import:cancel)");

        /* Wait for the busy flag to be lowered. */
        g_timeout_add(200, cancel_timeout_cb, user_data);
    }
    else
        do_cancel(wind);
}


/****************************************************************
 * gnc_ui_qif_import_close_cb
 *
 * Invoked when the "Close" button is clicked.
 ****************************************************************/
void
gnc_ui_qif_import_close_cb(GtkAssistant *gtkassistant, gpointer user_data)
{
    QIFImportWindow  *wind = user_data;

    /* If We did not have an account tree, lets save it */
    if (!wind->acct_tree_found)
    {
        qof_book_mark_session_dirty(gnc_get_current_book());
        gnc_ui_file_access_for_save_as();
    }

    gnc_close_gui_component_by_data( ASSISTANT_QIF_IMPORT_CM_CLASS, wind );
}


/****************************************************************
 * gnc_ui_qif_import_assistant_get_mappings
 *
 * SCM get mappings.
 ****************************************************************/
SCM
gnc_ui_qif_import_assistant_get_mappings(QIFImportWindow * w)
{
    return SCM_LIST3(w->acct_map_info,
                     w->cat_map_info,
                     w->memo_map_info);
}


/* ================================================================== */
/*                                                                    */
/*                         IMPORTER CREATION                          */
/*                                                                    */
/* ================================================================== */

/********************************************************************
 * get_preferences
 *
 * Get all user preferences related to QIF import.
 ********************************************************************/
static void
get_preferences(QIFImportWindow *wind)
{
    gchar tmp_transaction_status = 'n';

    g_return_if_fail(wind);

    /* Get the user's preference for showing documentation pages. */
    wind->show_doc_pages =
        gnc_prefs_get_bool (GNC_PREFS_GROUP, GNC_PREF_SHOW_DOC);

    /* Clear / Reconcile transaction if not specified in QIF file. */
    if (gnc_prefs_get_bool (GNC_PREFS_GROUP, GNC_PREF_DEFAULT_TRANS_STATUS_CLEARED))
        tmp_transaction_status = 'c';
    else if (gnc_prefs_get_bool (GNC_PREFS_GROUP, GNC_PREF_DEFAULT_TRANS_STATUS_RECONCILED))
        tmp_transaction_status = 'y';

    wind->transaction_status = SCM_MAKE_CHAR(tmp_transaction_status);
}


/********************************************************************
 * initialize_scheme
 *
 * Initialize all Scheme-controlled objects.
 ********************************************************************/
static void
initialize_scheme(QIFImportWindow *wind)
{
    SCM  load_map_prefs;
    SCM  mapping_info;
    SCM  create_ticker_map;

    g_return_if_fail(wind);

    /* Initialize Scheme variables. */
    wind->imported_files        = SCM_EOL;
    wind->selected_file         = SCM_BOOL_F;
    wind->gnc_acct_info         = SCM_BOOL_F;
    wind->cat_display_info      = SCM_BOOL_F;
    wind->cat_map_info          = SCM_BOOL_F;
    wind->acct_display_info     = SCM_BOOL_F;
    wind->acct_map_info         = SCM_BOOL_F;
    wind->memo_display_info     = SCM_BOOL_F;
    wind->memo_map_info         = SCM_BOOL_F;
    wind->security_hash         = SCM_BOOL_F;
    wind->security_prefs        = SCM_BOOL_F;
    wind->new_securities        = SCM_BOOL_F;
    wind->ticker_map            = SCM_BOOL_F;
    wind->imported_account_tree = SCM_BOOL_F;
    wind->match_transactions    = SCM_BOOL_F;

    /* Get the saved state of mappings from Quicken accounts and
     * categories to GnuCash accounts. */
    load_map_prefs = scm_c_eval_string("qif-import:load-map-prefs");
    mapping_info = scm_call_0(load_map_prefs); /* <- gets/creates session/book */
    wind->gnc_acct_info         = scm_list_ref(mapping_info, scm_from_int (0));
    wind->acct_map_info         = scm_list_ref(mapping_info, scm_from_int (1));
    wind->cat_map_info          = scm_list_ref(mapping_info, scm_from_int (2));
    wind->memo_map_info         = scm_list_ref(mapping_info, scm_from_int (3));
    wind->security_hash         = scm_list_ref(mapping_info, scm_from_int (4));
    wind->security_prefs        = scm_list_ref(mapping_info, scm_from_int (5));

    /* Get the initial ticker map. */
    create_ticker_map = scm_c_eval_string("make-ticker-map");
    wind->ticker_map            = scm_call_0(create_ticker_map);

    /* Protect our data from garbage collection. */
    scm_gc_protect_object(wind->imported_files);
    scm_gc_protect_object(wind->selected_file);
    scm_gc_protect_object(wind->gnc_acct_info);
    scm_gc_protect_object(wind->cat_display_info);
    scm_gc_protect_object(wind->cat_map_info);
    scm_gc_protect_object(wind->memo_display_info);
    scm_gc_protect_object(wind->memo_map_info);
    scm_gc_protect_object(wind->acct_display_info);
    scm_gc_protect_object(wind->acct_map_info);
    scm_gc_protect_object(wind->security_hash);
    scm_gc_protect_object(wind->security_prefs);
    scm_gc_protect_object(wind->new_securities);
    scm_gc_protect_object(wind->ticker_map);
    scm_gc_protect_object(wind->imported_account_tree);
    scm_gc_protect_object(wind->match_transactions);
}


/*****************************************
 * Page 0 - Intro Page Page
 ****************************************/

/********************************************************************
 * gnc_ui_qif_import_intro_prepare
 *
 * Prepare the intro page for display.
 ********************************************************************/
void
gnc_ui_qif_import_intro_prepare (GtkAssistant  *assistant, gpointer user_data)
{
    QIFImportWindow   *wind = user_data;
    SCM unload = scm_c_eval_string("qif-dialog:unload-qif-file");
    SCM files_list;

    /* Set load stop to FALSE */
    wind->load_stop = FALSE;

    files_list = scm_call_2(unload, wind->selected_file, wind->imported_files);

    scm_gc_unprotect_object(wind->imported_files);
    wind->imported_files = files_list;
    scm_gc_protect_object(wind->imported_files);

    scm_gc_unprotect_object(wind->selected_file);
    wind->selected_file = SCM_BOOL_F;
    scm_gc_protect_object(wind->selected_file);
}


/*****************************************
 * Page 1 - Load File Page Procedures
 ****************************************/

/********************************************************************
 * gnc_ui_qif_import_load_file_complete
 *
 * Do we have a file to load.
 ********************************************************************/
static gboolean
gnc_ui_qif_import_load_file_complete (GtkAssistant  *assistant,
                                      gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    const gchar * path_to_load;

    /* Get the file name. */
    path_to_load = gtk_entry_get_text(GTK_ENTRY(wind->filename_entry));

    /* Validate the chosen filename. */
    if (strlen(path_to_load) == 0)
        gnc_error_dialog(wind->window, "%s", _("Please select a file to load."));
    else if (g_access(path_to_load, R_OK) < 0)
        gnc_error_dialog(wind->window, "%s",
                         _("File not found or read permission denied. "
                           "Please select another file."));
    else
    {
        SCM qif_file_loaded = scm_c_eval_string("qif-dialog:qif-file-loaded?");

        /* See if the file is already loaded. */
        if (scm_call_2(qif_file_loaded,
                       scm_from_locale_string(path_to_load ? path_to_load : ""),
                       wind->imported_files) == SCM_BOOL_T)
            gnc_error_dialog(wind->window, "%s",
                             _("That QIF file is already loaded. "
                               "Please select another file."));
        else
        {
            /* Passed all checks; proceed to the next page. */
            return TRUE;
        }
    }

    /* Stay on this page. */
    return FALSE;
}


/********************************************************************
 * gnc_ui_qif_import_load_file_prepare
 *
 * Prepare the load file page for display.
 ********************************************************************/
void
gnc_ui_qif_import_load_file_prepare (GtkAssistant  *assistant, gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    const gchar * path_to_load;
    gboolean page_status = FALSE;

    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    /* Get the file name. */
    path_to_load = gtk_entry_get_text(GTK_ENTRY(wind->filename_entry));

    /* Calculate status for the Assistant Forward Button */
    if (strlen(path_to_load) != 0)
    {
       page_status = gnc_ui_qif_import_load_file_complete(assistant, user_data);
    }
    gtk_assistant_set_page_complete (assistant, page, page_status);
}


/********************************************************************
 * gnc_ui_qif_import_select_file_cb
 *
 * invoked when the "select file" button is clicked
 * this is just to pick a file name and reset-to-defaults all the
 * fields describing how to parse the file.
 ********************************************************************/
void
gnc_ui_qif_import_select_file_cb(GtkButton * button,
                                 gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    GtkAssistant *assistant = GTK_ASSISTANT(wind->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    GtkFileFilter *filter;
    char * new_file_name;
    char *file_name, *default_dir;

    /* Default to whatever's already present */
    default_dir = gnc_get_default_directory(GNC_PREFS_GROUP);

    filter = gtk_file_filter_new();
    gtk_file_filter_set_name(filter, "*.qif");
    gtk_file_filter_add_pattern(filter, "*.[Qq][Ii][Ff]");
    new_file_name = gnc_file_dialog(_("Select QIF File"),
                                    g_list_prepend (NULL, filter),
                                    default_dir,
                                    GNC_FILE_DIALOG_IMPORT);

    /* Insure valid data, and something that can be freed. */
    if (new_file_name == NULL)
    {
        file_name = g_strdup(default_dir);
    }
    else if (!g_path_is_absolute(new_file_name))
    {
        file_name = g_build_filename(default_dir, new_file_name, NULL);
        g_free(new_file_name);
    }
    else
    {
        file_name = new_file_name;
        /* Update the working directory */
        g_free(default_dir);
        default_dir = g_path_get_dirname(file_name);
        gnc_set_default_directory(GNC_PREFS_GROUP, default_dir);
    }
    g_free(default_dir);

    /* set the filename entry for what was selected */
    gtk_entry_set_text(GTK_ENTRY(wind->filename_entry), file_name);
    g_free(file_name);

    gtk_assistant_set_page_complete (assistant, page,
                                     gnc_ui_qif_import_load_file_complete (assistant, user_data));
}


/*****************************************
 * Page 2 - Load Progress Page Procedures
 ****************************************/

/********************************************************************
 * gnc_ui_qif_import_load_progress_pause_cb
 *
 * Invoked when the "Pause" button is clicked.
 ********************************************************************/
void
gnc_ui_qif_import_load_progress_pause_cb(GtkButton * button,
        gpointer user_data)
{
    QIFImportWindow *wind = user_data;
    SCM toggle_pause      = scm_c_eval_string("qif-import:toggle-pause");
    SCM progress;

    if (!wind->busy)
        return;

    /* Create SCM for the progress helper. */
    progress = SWIG_NewPointerObj(wind->load_progress,
                                  SWIG_TypeQuery("_p__GNCProgressDialog"),
                                  0);

    /* Pause (or resume) the currently running operation. */
    scm_call_1(toggle_pause, progress);

    /* Swap the button label between pause and resume. */
    if (strcmp(gtk_button_get_label(button), _("_Resume")))
    {
        gtk_button_set_use_stock(button, FALSE);
        gtk_button_set_use_underline(button, TRUE);
        gtk_button_set_label(button, _("_Resume"));
    }
    else
    {
        gtk_button_set_use_stock(button, TRUE);
        gtk_button_set_use_underline(button, FALSE);
        gtk_button_set_label(button, "gtk-media-pause");
    }
}


/********************************************************************
 * gnc_ui_qif_import_load_progress_start_cb
 *
 * Invoked when the "Start" button is clicked.
 ********************************************************************/
void
gnc_ui_qif_import_load_progress_start_cb(GtkButton * button,
        gpointer user_data)
{
    QIFImportWindow   *wind = user_data;
    gint num = gtk_assistant_get_current_page (GTK_ASSISTANT(wind->window));
    GtkWidget *page = gtk_assistant_get_nth_page (GTK_ASSISTANT(wind->window), num);

    const gchar * path_to_load;

    SCM make_qif_file   = scm_c_eval_string("make-qif-file");
    SCM qif_file_load   = scm_c_eval_string("qif-file:read-file");
    SCM qif_file_parse  = scm_c_eval_string("qif-file:parse-fields");
    SCM unload_qif_file = scm_c_eval_string("qif-dialog:unload-qif-file");
    SCM parse_results   = scm_c_eval_string("qif-file:parse-fields-results");
    SCM scm_qiffile;
    SCM imported_files = SCM_EOL;
    SCM load_return, parse_return;
    SCM progress;

    /* Raise the busy flag so the assistant can't be canceled unexpectedly. */
    wind->busy = TRUE;
    gtk_widget_set_sensitive(wind->load_pause, TRUE);

    /* Get the file name. */
    path_to_load = gtk_entry_get_text(GTK_ENTRY(wind->filename_entry));

    /* Create the <qif-file> object. */
    scm_qiffile          = scm_call_0(make_qif_file);
    scm_gc_unprotect_object(wind->selected_file);
    wind->selected_file  = scm_qiffile;
    scm_gc_protect_object(wind->selected_file);
    imported_files       = scm_cons(scm_qiffile, wind->imported_files);

    /* Create SCM for the progress helper. */
    progress = SWIG_NewPointerObj(wind->load_progress,
                                  SWIG_TypeQuery("_p__GNCProgressDialog"),
                                  0);

    /* Clear any previous pause or cancel state. */
    scm_c_eval_string("(qif-import:reset-cancel-pause)");

    /*
     * Load the file.
     *
     * The loader returns:
     *  success:   ()
     *  failure:   (#f error-message)
     *  warning:   (#t error-message)
     *  cancel:    #t
     *  exception: #f
     */

    /* This step will fill 70% of the bar. */
    gnc_progress_dialog_push(wind->load_progress, 0.7);
    load_return = scm_call_4(qif_file_load,
                             SCM_CAR(imported_files),
                             scm_from_locale_string(path_to_load ? path_to_load : ""),
                             wind->ticker_map,
                             progress);
    gnc_progress_dialog_pop(wind->load_progress);
    if (load_return == SCM_BOOL_T)
    {
        /* Canceled by the user. */

        /* Disable the pause button. */
        gtk_widget_set_sensitive(wind->load_pause, FALSE);

        /* Inform the user. */
        gnc_progress_dialog_set_sub(wind->load_progress, _("Canceled"));

        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }
    else if (load_return == SCM_BOOL_F || !scm_is_list(load_return))
    {
        /* A bug was detected. */

        /* Disable the pause button. */
        gtk_widget_set_sensitive(wind->load_pause, FALSE);

        /* Inform the user. */
        gnc_progress_dialog_append_log(wind->load_progress,
                                       _( "An error occurred while loading the QIF file."));
        gnc_progress_dialog_set_sub(wind->load_progress, _("Failed"));
        gnc_progress_dialog_reset_value(wind->load_progress);
        gnc_error_dialog(wind->window, "%s",
                         _( "An error occurred while loading the QIF file."));
        /* FIXME: How should we request that the user report this problem? */

        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }
    else if (!scm_is_null(load_return))
    {
        if (SCM_CAR(load_return) == SCM_BOOL_F)
        {
            imported_files = scm_call_2(unload_qif_file, scm_qiffile, imported_files);
            scm_gc_unprotect_object(wind->imported_files);
            wind->imported_files = imported_files;
            scm_gc_protect_object(wind->imported_files);

            gnc_progress_dialog_set_sub(wind->load_progress, _("Failed"));
            gnc_progress_dialog_reset_value(wind->load_progress);

            gtk_widget_set_sensitive(wind->load_pause, FALSE);
            wind->busy = FALSE;
            wind->load_stop = TRUE;
        }
    }

    /*
     * Parse the fields.
     *
     * The parser returns:
     *   success:   ()
     *   failure:   (#f . ((type . error) ...))
     *   warning:   (#t . ((type . error) ...))
     *   cancel:    #t
     *   exception: #f
     */

    /* This step will fill the remainder of the bar. */
    gnc_progress_dialog_push(wind->load_progress, 1);
    parse_return = scm_call_2(qif_file_parse, SCM_CAR(imported_files), progress);
    gnc_progress_dialog_pop(wind->load_progress);
    wind->ask_date_format = FALSE;
    if (parse_return == SCM_BOOL_T)
    {
        /* Canceled by the user. */

        /* Disable the pause button. */
        gtk_widget_set_sensitive(wind->load_pause, FALSE);

        /* Unload the file. */
        gnc_progress_dialog_set_sub(wind->load_progress, _("Cleaning up"));
        imported_files = scm_call_2(unload_qif_file, scm_qiffile, imported_files);

        /* Inform the user. */
        gnc_progress_dialog_set_sub(wind->load_progress, _("Canceled"));

        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }
    else if (parse_return == SCM_BOOL_F || !scm_is_list(parse_return))
    {
        /* A bug was detected. */

        /* Disable the pause button. */
        gtk_widget_set_sensitive(wind->load_pause, FALSE);

        /* Unload the file. */
        gnc_progress_dialog_set_sub(wind->load_progress, _("Cleaning up"));
        imported_files = scm_call_2(unload_qif_file, scm_qiffile, imported_files);

        /* Inform the user. */
        gnc_progress_dialog_append_log(wind->load_progress,
                                       _( "A bug was detected while parsing the QIF file."));
        gnc_progress_dialog_set_sub(wind->load_progress, _("Failed"));
        gnc_progress_dialog_reset_value(wind->load_progress);
        gnc_error_dialog(wind->window, "%s",
                         _( "A bug was detected while parsing the QIF file."));
        /* FIXME: How should we request that the user report this problem? */

        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }
    else if (!scm_is_null(parse_return))
    {
        /* Are there only warnings? */
        if (SCM_CAR(parse_return) == SCM_BOOL_T)
        {
            SCM date_formats;

            /* A warning means that (potentially) the date format is
             * ambiguous.  So search the results for the "date" type and if
             * it's found, set up the format selector page. */
            if ((date_formats = scm_call_2(parse_results,
                                           SCM_CDR(parse_return),
                                           scm_from_locale_symbol ("date"))) != SCM_BOOL_F)
            {
                GtkComboBox *combo_box;
                GtkTreeModel *model;
                GtkTreeIter iter;

                /* Block the date call back */
                g_signal_handlers_block_by_func( wind->date_format_combo, gnc_ui_qif_import_date_valid_cb, wind );

                /* Clear the date format combo box. */
                combo_box = GTK_COMBO_BOX(wind->date_format_combo);
                model = gtk_combo_box_get_model(combo_box);
                gtk_list_store_clear(GTK_LIST_STORE(model));

                gtk_combo_box_set_active(GTK_COMBO_BOX(wind->date_format_combo), -1);

                /* Add the formats for the user to select from. */
                while (scm_is_list(date_formats) && !scm_is_null(date_formats))
                {
                    gtk_list_store_append(GTK_LIST_STORE(model), &iter);
                    gtk_list_store_set (GTK_LIST_STORE(model), &iter, 0, gnc_scm_symbol_to_locale_string(SCM_CAR(date_formats)), -1);

                    date_formats = SCM_CDR(date_formats);
                }

                /* Unblock the date call back */
                g_signal_handlers_unblock_by_func( wind->date_format_combo, gnc_ui_qif_import_date_valid_cb, wind );

                wind->ask_date_format = TRUE;
            }
        }
        else
        {
            /* Parsing failed. */
            imported_files = scm_call_2(unload_qif_file, scm_qiffile, imported_files);
            gnc_progress_dialog_set_sub(wind->load_progress, _("Failed"));
            gnc_progress_dialog_reset_value(wind->load_progress);

            gtk_widget_set_sensitive(wind->load_pause, FALSE);
            wind->busy = FALSE;
            wind->load_stop = TRUE;
        }
    }

    /* Enable the assistant Forward button */
    gtk_assistant_set_page_complete (GTK_ASSISTANT(wind->window), page, TRUE);

    /* Set Pause and Start buttons */
    gtk_widget_set_sensitive(wind->load_pause, FALSE);
    gtk_widget_set_sensitive(wind->load_start, FALSE);

    if (wind->load_stop == FALSE)
    {
        /* The file was loaded successfully. */
        gnc_progress_dialog_set_sub(wind->load_progress, _("Loading completed"));
        gnc_progress_dialog_set_value(wind->load_progress, 1);

        scm_gc_unprotect_object(wind->imported_files);
        wind->imported_files = imported_files;
        scm_gc_protect_object(wind->imported_files);

        gtk_widget_set_sensitive(wind->load_pause, FALSE);
        wind->busy = FALSE;

        /* Auto step to next page */
        gtk_assistant_set_current_page (GTK_ASSISTANT(wind->window), num + 1);
    }
}


/********************************************************************
 * gnc_ui_qif_import_load_progress_prepare
 *
 * Prepare the file loading progress page for display.
 ********************************************************************/
void
gnc_ui_qif_import_load_progress_prepare (GtkAssistant  *assistant, gpointer user_data)
{
    QIFImportWindow   *wind = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    /* Reset the progress display. */
    gnc_progress_dialog_set_primary(wind->load_progress, "");
    gnc_progress_dialog_set_secondary(wind->load_progress,
                                      _("When you press the Start Button, GnuCash will load your QIF file. If there are no errors or warnings, you will automatically proceed to the next step. Otherwise, the details will be shown below for your review."));
    gnc_progress_dialog_set_sub(wind->load_progress, " ");
    gnc_progress_dialog_reset_value(wind->load_progress);
    gnc_progress_dialog_reset_log(wind->load_progress);

    /* Set Pause and Start buttons */
    gtk_widget_set_sensitive(wind->load_pause, FALSE);
    gtk_widget_set_sensitive(wind->load_start, TRUE);

    /* Disable the assistant Forward button */
    gtk_assistant_set_page_complete (assistant, page, FALSE);
}


/*****************************************
 * Page 3 - Date format Page Procedures
 ****************************************/

/********************************************************************
 * gnc_ui_qif_import_date_format_prepare
 *
 * Determine if we need the date page and what is next page.
 ********************************************************************/
void
gnc_ui_qif_import_date_format_prepare (GtkAssistant  *assistant, gpointer user_data)

{
    QIFImportWindow *wind = user_data;
    gint num = gtk_assistant_get_current_page (assistant);

    if (wind->ask_date_format)
    {
        /* We need to get a date format, so stay here. */
    }
    else
    {
        /* Skip ahead to the Account page. */
        gtk_assistant_set_current_page (assistant, num + 1);
    }
}


/********************************************************************
 * gnc_ui_qif_import_date_valid_cb
 *
 * Reparse file with new date format.
 ********************************************************************/
void
gnc_ui_qif_import_date_valid_cb (GtkWidget *widget, gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    GtkTreeModel *model;
    GtkTreeIter iter;

    GtkAssistant *assistant = GTK_ASSISTANT(wind->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    SCM  reparse_dates   = scm_c_eval_string("qif-file:reparse-dates");
    SCM  format_sym;
    gchar *text;

    /* Get the selected date format. */
    model = gtk_combo_box_get_model(GTK_COMBO_BOX(wind->date_format_combo));
    gtk_combo_box_get_active_iter (GTK_COMBO_BOX(wind->date_format_combo), &iter);
    gtk_tree_model_get( model, &iter, 0, &text, -1 );

    if (!text)
    {
        g_critical("QIF import: BUG DETECTED in gnc_ui_qif_import_date_valid_cb. Format is NULL.");
    }
    format_sym = scm_from_locale_symbol (text);
    g_free(text);

    /* Reparse the dates using the selected format. */
    scm_call_2(reparse_dates, wind->selected_file, format_sym);

    gtk_assistant_set_page_complete (assistant, page, TRUE);
}


/******************************************
 * Page 4 - Account Setup Page Procedures
 ******************************************/

/********************************************************************
 * gnc_ui_qif_import_account_prepare
 *
 * Do we need to specify an account.
 ********************************************************************/
void
gnc_ui_qif_import_account_prepare (GtkAssistant  *assistant, gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    gint num = gtk_assistant_get_current_page (assistant);

    SCM  check_from_acct = scm_c_eval_string("qif-file:check-from-acct");

    /* Determine the next page to display. */
    if (scm_call_1(check_from_acct, wind->selected_file) != SCM_BOOL_T)
    {
        /* There is an account name missing. Ask the user to provide one. */
        SCM default_acct = scm_c_eval_string("qif-file:path-to-accountname");
        gchar * default_acctname = NULL;

        default_acctname = gnc_scm_call_1_to_string(default_acct, wind->selected_file);
        gtk_entry_set_text(GTK_ENTRY(wind->acct_entry), default_acctname);
        g_free (default_acctname);
    }
    else
    {
        /* Skip ahead to the "loaded files" page. */
        gtk_assistant_set_current_page (assistant, num + 1);
    }
}


/********************************************************************
 * gnc_ui_qif_import_acct_valid_cb
 *
 * Invoked when the "next" button is clicked on the default acct page.
 ********************************************************************/
void
gnc_ui_qif_import_acct_valid_cb(GtkWidget * widget,
                                gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    GtkAssistant *assistant = GTK_ASSISTANT(wind->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    const gchar * acct_name = gtk_entry_get_text(GTK_ENTRY(wind->acct_entry));

    if (!acct_name || acct_name[0] == 0)
    {
        /* Disable the assistant Forward Button */
        gtk_assistant_set_page_complete (assistant, page, FALSE);
    }
    else
    {
        /* Enable the assistant Forward Button */
        gtk_assistant_set_page_complete (assistant, page, TRUE);
    }
}


/*****************************************
 * Page 5 - Loaded Files Page Procedures
 ****************************************/

/********************************************************************
 * gnc_ui_qif_import_loaded_files_prepare
 *
 * Get the loaded files page ready for viewing
 ********************************************************************/
void
gnc_ui_qif_import_loaded_files_prepare (GtkAssistant *assistant,
                                        gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    const gchar * acct_name = gtk_entry_get_text(GTK_ENTRY(wind->acct_entry));
    SCM    fix_default = scm_c_eval_string("qif-import:fix-from-acct");
    SCM    scm_name;

    scm_name = scm_from_utf8_string(acct_name ? acct_name : "");
    scm_call_2(fix_default, wind->selected_file, scm_name);

    /* Enable the assistant Forward Button */
    gtk_assistant_set_page_complete (assistant, page, TRUE);

    update_file_page(wind);
}


/********************************************************************
 * gnc_ui_qif_import_load_another_cb
 * Invoked when the "load another" button is clicked on the loaded
 * files page.
 ********************************************************************/
void
gnc_ui_qif_import_load_another_cb(GtkButton * button,
                                  gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    GtkAssistant *assistant = GTK_ASSISTANT(wind->window);

    gtk_assistant_set_current_page (assistant, 1);
}


/********************************************************************
 * gnc_ui_qif_import_unload_cb
 * Invoked when the "unload" button is clicked on the loaded files
 * page.
 ********************************************************************/
void
gnc_ui_qif_import_unload_file_cb(GtkButton * button,
                                 gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    SCM unload_qif_file = scm_c_eval_string("qif-dialog:unload-qif-file");
    SCM imported_files;

    if (wind->selected_file != SCM_BOOL_F)
    {
        imported_files =
            scm_call_2(unload_qif_file, wind->selected_file, wind->imported_files);

        scm_gc_unprotect_object(wind->imported_files);
        wind->imported_files = imported_files;
        scm_gc_protect_object(wind->imported_files);

        scm_gc_unprotect_object(wind->selected_file);
        wind->selected_file = SCM_BOOL_F;
        scm_gc_protect_object(wind->selected_file);

        update_file_page(wind);
    }
}


/********************************************************************
 * update_file_page
 *
 * Update the list of loaded files.
 ********************************************************************/
static void
update_file_page(QIFImportWindow * wind)
{
    SCM       loaded_file_list = wind->imported_files;
    SCM       qif_file_path;
    int       row = 0;
    GtkTreeView *view;
    GtkListStore *store;
    GtkTreeIter iter;
    GtkTreePath *path;
    GtkTreeRowReference *reference = NULL;

    /* clear the list */
    view = GTK_TREE_VIEW(wind->selected_file_view);
    store = GTK_LIST_STORE(gtk_tree_view_get_model(view));
    gtk_list_store_clear(store);
    qif_file_path = scm_c_eval_string("qif-file:path");

    while (!scm_is_null(loaded_file_list))
    {
        gchar *row_text    = NULL;
        SCM    scm_qiffile = SCM_BOOL_F;

        scm_qiffile = SCM_CAR(loaded_file_list);
        row_text = gnc_scm_call_1_to_string(qif_file_path, scm_qiffile);

        gtk_list_store_append(store, &iter);
        gtk_list_store_set(store, &iter,
                           FILENAME_COL_INDEX, row++,
                           FILENAME_COL_NAME, row_text,
                           -1);
        g_free (row_text);

        if (scm_qiffile == wind->selected_file)
        {
            path = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &iter);
            reference = gtk_tree_row_reference_new(GTK_TREE_MODEL(store), path);
            gtk_tree_path_free(path);
        }
        loaded_file_list = SCM_CDR(loaded_file_list);
    }

    if (reference)
    {
        GtkTreeSelection* selection = gtk_tree_view_get_selection(view);
        path = gtk_tree_row_reference_get_path(reference);
        if (path)
        {
            gtk_tree_selection_select_path(selection, path);
            gtk_tree_path_free(path);
        }
        gtk_tree_row_reference_free(reference);
    }
}


/**********************************************
 * Page 6 - Account Doc. Page Procedures
 **********************************************/

/********************************************************************
 * gnc_ui_qif_import_account_doc_prepare
 ********************************************************************/
void
gnc_ui_qif_import_account_doc_prepare (GtkAssistant *assistant,
                                       gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);
    gint total = gtk_assistant_get_n_pages (assistant);
    gtk_assistant_update_buttons_state (assistant);

    PINFO("Total Number of Assistant Pages is %d", gtk_assistant_get_n_pages (assistant));

    /* Enable the Assistant Forward Button */
    gtk_assistant_set_page_complete (assistant, page, TRUE);

    /* Jump to Summary page if load_stop TRUE */
    if (wind->load_stop)
        gtk_assistant_set_current_page (assistant, total - 1 );

    /* Jump over doc page if show_doc_pages FALSE */
    if (!wind->show_doc_pages)
        gtk_assistant_set_current_page (assistant, num + 1 );
}


/******************************************
 * Page 7 - Account Match Page Procedures
 ******************************************/

/********************************************************************
 * gnc_ui_qif_import_account_match_prepare
 *
 * Get the matching pages ready for viewing.
 ********************************************************************/
void
gnc_ui_qif_import_account_match_prepare(GtkAssistant *assistant,
                                        gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    /* Prepare the matching pages. */
    gnc_set_busy_cursor(NULL, TRUE);
    update_account_page(wind);
    update_category_page(wind);
    update_memo_page(wind);
    gnc_unset_busy_cursor(NULL);

    /* Enable the Assistant Forward Button */
    gtk_assistant_set_page_complete (assistant, page, TRUE);
}


/****************************************************************
 * gnc_ui_qif_import_account_rematch_cb
 *
 * This handler is invoked when the user clicks the "Change
 * GnuCash account" button on the account mapping page. This
 * button is an alternative to double-clicking a row.
 ****************************************************************/
void
gnc_ui_qif_import_account_rematch_cb(GtkButton *button, gpointer user_data)
{
    QIFImportWindow  *wind = user_data;

    g_return_if_fail(wind);

    rematch_line(wind,
                 gtk_tree_view_get_selection(GTK_TREE_VIEW(wind->acct_view)),
                 wind->acct_display_info,
                 wind->acct_map_info,
                 update_account_page);
}


/*******************************************
 * Page 8 - Catagory Doc. Page Procedures
 *******************************************/

/********************************************************************
 * gnc_ui_qif_import_catagory_doc_prepare
 ********************************************************************/
void
gnc_ui_qif_import_catagory_doc_prepare (GtkAssistant *assistant,
                                        gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);
    gint total = gtk_assistant_get_n_pages (assistant);
    gtk_assistant_update_buttons_state (assistant);

    PINFO("Total Number of Assistant Pages is %d", gtk_assistant_get_n_pages (assistant));

    /* Enable the Assistant Forward Button */
    gtk_assistant_set_page_complete (assistant, page, TRUE);

    /* Jump to Summary page if load_stop TRUE */
    if (wind->load_stop)
        gtk_assistant_set_current_page (assistant, total - 1 );

    /* Jump over doc page if show_doc_pages FALSE */
    if (!wind->show_doc_pages)
        gtk_assistant_set_current_page (assistant, num + 1 );

    /* If there are no category mappings, jump the doc page. */
    if (scm_is_list(wind->cat_display_info) && scm_is_null(wind->cat_display_info))
        gtk_assistant_set_current_page (assistant, num + 1);
}


/******************************************
 * Page 9 - Catagory Match Page Procedures
 ******************************************/

/****************************************************************
 * gnc_ui_qif_import_catagory_match_prepare
 *
 * Find the next page to show, depending on whether there are
 * category or payee/memo mappings to be dealt with.
 ****************************************************************/
void
gnc_ui_qif_import_catagory_match_prepare(GtkAssistant *assistant,
        gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    /* Enable the Assistant Forward Button */
    gtk_assistant_set_page_complete (assistant, page, TRUE);

    /* If there are no category mappings, jump this step. */
    if (scm_is_list(wind->cat_display_info) && scm_is_null(wind->cat_display_info))
        gtk_assistant_set_current_page (assistant, num + 1);
}


/****************************************************************
 * gnc_ui_qif_import_category_rematch_cb
 *
 * This handler is invoked when the user clicks the "Change
 * GnuCash account" button on the category mapping page. This
 * button is an alternative to double-clicking a row.
 ****************************************************************/
void
gnc_ui_qif_import_category_rematch_cb(GtkButton *button, gpointer user_data)
{
    QIFImportWindow  *wind = user_data;

    g_return_if_fail(wind);

    rematch_line(wind,
                 gtk_tree_view_get_selection(GTK_TREE_VIEW(wind->cat_view)),
                 wind->cat_display_info,
                 wind->cat_map_info,
                 update_category_page);
}


/****************************************
 * Page 10 - Memo Doc. Page Procedures
 ****************************************/

/********************************************************************
 * gnc_ui_qif_import_memo_doc_prepare
 ********************************************************************/
void
gnc_ui_qif_import_memo_doc_prepare (GtkAssistant *assistant,
                                    gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);
    gint total = gtk_assistant_get_n_pages (assistant);
    gtk_assistant_update_buttons_state (assistant);

    PINFO("Total Number of Assistant Pages is %d", gtk_assistant_get_n_pages (assistant));

    /* Enable the Assistant Forward Button */
    gtk_assistant_set_page_complete (assistant, page, TRUE);

    /* Jump to Summary page if load_stop TRUE */
    if (wind->load_stop)
        gtk_assistant_set_current_page (assistant, total - 1 );

    /* Jump over doc page if show_doc_pages FALSE */
    if (!wind->show_doc_pages)
        gtk_assistant_set_current_page (assistant, num + 1 );

    /* If there are no memo mappings, jump the doc page. */
    if (scm_is_list(wind->memo_display_info) && scm_is_null(wind->memo_display_info))
        gtk_assistant_set_current_page (assistant, num + 1);
}


/****************************************
 * Page 11 - Memo Match Page Procedures
 ****************************************/

/****************************************************************
 * gnc_ui_qif_import_memo_match_prepare
 *
 * Find the next page to show, depending on whether there are
 * category or payee/memo mappings to be dealt with.
 ****************************************************************/
void
gnc_ui_qif_import_memo_match_prepare (GtkAssistant *assistant,
                                      gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    /* Enable the Assistant Forward Button */
    gtk_assistant_set_page_complete (assistant, page, TRUE);

    /* If there are no memo mappings, jump this step. */
    if (scm_is_list(wind->memo_display_info) && scm_is_null(wind->memo_display_info))
        gtk_assistant_set_current_page (assistant, num + 1);
}


/****************************************************************
 * gnc_ui_qif_import_memo_rematch_cb
 *
 * This handler is invoked when the user clicks the "Change
 * GnuCash account" button on the memo mapping page. This
 * button is an alternative to double-clicking a row.
 ****************************************************************/
void
gnc_ui_qif_import_memo_rematch_cb(GtkButton *button, gpointer user_data)
{
    QIFImportWindow  *wind = user_data;

    g_return_if_fail(wind);

    rematch_line(wind,
                 gtk_tree_view_get_selection(GTK_TREE_VIEW(wind->memo_view)),
                 wind->memo_display_info,
                 wind->memo_map_info,
                 update_memo_page);
}


/*****************************************
 * Page 12  - Currency Page Procedures
 ****************************************/

/****************************************************************
 * gnc_ui_qif_import_currency_prepare
 *
 * Find the next page to show, depending on whether there are
 * category or payee/memo mappings to be dealt with.
 ****************************************************************/
void
gnc_ui_qif_import_currency_prepare(GtkAssistant *assistant,
                                   gpointer user_data)
{
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);
    QIFImportWindow  *wind = user_data;

    g_return_if_fail(wind);

    /* Only display Book Option data if new book */
    if (wind->new_book)
    {
        gtk_assistant_set_page_title (assistant, page,
                                      _("Choose the QIF file currency and select Book Options"));
        gtk_widget_show (wind->book_option_label);
        gtk_widget_show (wind->book_option_message);
    }
    else
    {
        gtk_assistant_set_page_title (assistant, page,
                                      _("Choose the QIF file currency"));
        gtk_widget_hide (wind->book_option_label);
        gtk_widget_hide (wind->book_option_message);
    }

    /* Enable the Assistant Forward Button */
    gtk_assistant_set_page_complete (assistant, page, TRUE);
}


/**************************************************
 * Page 13 - Commodity Doc. Page Procedures
 **************************************************/

/****************************************************************
 * gnc_ui_qif_import_new_securities
 *
 * This function creates or updates the list of QIF securities
 * for which no corresponding GnuCash commodity existed prior to
 * import. If there are any such securities, TRUE is returned.
 * Otherwise, FALSE is returned.
 ****************************************************************/
static gboolean
gnc_ui_qif_import_new_securities(QIFImportWindow * wind)
{
    SCM updates;
    SCM update_securities = scm_c_eval_string("qif-import:update-security-hash");

    /* Get a list of any new QIF securities since the previous call. */
    updates = scm_call_4(update_securities,
                         wind->security_hash,
                         wind->ticker_map,
                         wind->acct_map_info,
                         wind->security_prefs);
    if (updates != SCM_BOOL_F)
    {
        /* A list of new QIF securities was returned. Save it. */
        scm_gc_unprotect_object(wind->new_securities);
        if (wind->new_securities != SCM_BOOL_F)
            /* There is an existing list, so append the new list. */
            wind->new_securities = scm_append(scm_list_2(wind->new_securities,
                                              updates));
        else
            wind->new_securities = updates;
        scm_gc_protect_object(wind->new_securities);

        return TRUE;
    }

    if (wind->new_securities != SCM_BOOL_F)
        return TRUE;

    return FALSE;
}


/********************************************************************
 * gnc_ui_qif_import_commodity_doc_prepare
 ********************************************************************/
void
gnc_ui_qif_import_commodity_doc_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);
    gint total = gtk_assistant_get_n_pages (assistant);
    gtk_assistant_update_buttons_state (assistant);

    PINFO("Total Number of Assistant Pages is %d", gtk_assistant_get_n_pages (assistant));

    /* Enable the Assistant Forward Button */
    gtk_assistant_set_page_complete (assistant, page, TRUE);

    /* Jump to Summary page if load_stop TRUE */
    if (wind->load_stop)
        gtk_assistant_set_current_page (assistant, total - 1 );

    /* If there are new securities, prepare the security pages. */
    if (gnc_ui_qif_import_new_securities(wind))
        prepare_security_pages(wind);
    else
        /* If there are no securities, jump the doc page */
        gtk_assistant_set_current_page (assistant, num + 1 );

    /* Jump over doc page if show_doc_pages FALSE */
    if (!wind->show_doc_pages)
        gtk_assistant_set_current_page (assistant, num + 1 );
}


/********************************************
 * Page xx - Commodity New Pages Procedures
 ********************************************/

/********************************************
 * gnc_ui_qif_import_commodity_new_prepare
 *******************************************/
void
gnc_ui_qif_import_commodity_new_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    QIFAssistantPage    *qpage = g_object_get_data(G_OBJECT(page), "page_struct");
    const gchar         *ns;

    g_return_if_fail (qpage != NULL);

    /* Get any entered namespace. */
    ns = gtk_entry_get_text( GTK_ENTRY( gtk_bin_get_child( GTK_BIN( GTK_COMBO_BOX(qpage->namespace_combo)))));

    /* Update the namespaces available to select. */
    if (!ns || !ns[0])
        gnc_ui_update_namespace_picker(
            qpage->namespace_combo,
            gnc_commodity_get_namespace(qpage->commodity),
            DIAG_COMM_ALL);
    else
        gnc_ui_update_namespace_picker(qpage->namespace_combo, ns, DIAG_COMM_ALL);
}


/*********************************
 * gnc_ui_qif_import_comm_valid
 ********************************/
static gboolean
gnc_ui_qif_import_comm_valid (GtkAssistant *assistant,
                              gpointer user_data)
{
    QIFImportWindow *wind = user_data;
    gint num = gtk_assistant_get_current_page (GTK_ASSISTANT(wind->window));
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);
    QIFAssistantPage    *qpage = g_object_get_data(G_OBJECT(page), "page_struct");

    QofBook                 *book;
    gnc_commodity_table     *table;
    gnc_commodity_namespace *newns;

    gchar       *name_space = gnc_ui_namespace_picker_ns(qpage->namespace_combo);
    const gchar *name      = gtk_entry_get_text(GTK_ENTRY(qpage->name_entry));
    const gchar *mnemonic  = gtk_entry_get_text(GTK_ENTRY(qpage->mnemonic_entry));

    if (!name || (name[0] == 0))
    {
        gnc_warning_dialog(wind->window, "%s",
                           _("Enter a name or short description, such as \"Red Hat Stock\"."));
        g_free(name_space);
        return FALSE;
    }
    else if (!mnemonic || (mnemonic[0] == 0))
    {
        gnc_warning_dialog(wind->window, "%s",
                           _("Enter the ticker symbol or other well known abbreviation, such as"
                             " \"RHT\". If there isn't one, or you don't know it, create your own."));
        g_free(name_space);
        return FALSE;
    }
    else if (!name_space || (name_space[0] == 0))
    {
        gnc_warning_dialog(wind->window, "%s",
                           _("Select the exchange on which the symbol is traded, or select the"
                             " type of investment (such as FUND for mutual funds.) If you don't"
                             " see your exchange or an appropriate investment type, you can"
                             " enter a new one."));
        if (name_space)
            g_free(name_space);
        return FALSE;
    }

    /* FIXME: Should check whether a commodity with this namespace and
     *        mnemonic already exists. If so, ask the user whether to use
     *        the existing one, or go back and change what they've entered.
     */

    book = gnc_get_current_book();
    table = gnc_commodity_table_get_table(book);
    if (gnc_commodity_namespace_is_iso(name_space) &&
            !gnc_commodity_table_lookup(table, name_space, mnemonic))
    {
        gnc_warning_dialog(wind->window, "%s",
                           _("You must enter an existing national "
                             "currency or enter a different type."));

        g_free(name_space);
        return FALSE;
    }

    /* Is the namespace a new one? */
    if (!gnc_commodity_table_has_namespace(table, name_space))
    {
        /* Register it so that it will appear as an option on other pages. */
        newns = gnc_commodity_table_add_namespace(table, name_space, book);

        /* Remember it so it can be removed if the import gets canceled. */
        if (newns)
            wind->new_namespaces = g_list_prepend(wind->new_namespaces, name_space);
        else
        {
            g_warning("QIF import: Couldn't create namespace %s", name_space);
            g_free(name_space);
        }
    }
    else
        g_free(name_space);

    return TRUE;
}


/*************************************
 * gnc_ui_qif_import_comm_changed_cb
 ************************************/
void
gnc_ui_qif_import_comm_changed_cb (GtkWidget *widget, gpointer user_data)
{
    QIFImportWindow *wind = user_data;
    GtkAssistant *assistant = GTK_ASSISTANT(wind->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    gtk_assistant_set_page_complete (assistant, page, gnc_ui_qif_import_comm_valid (assistant, user_data));
}


/**********************************************
 * Page 14 - Convert progress Page Procedures
 *********************************************/

/********************************************************************
 * gnc_ui_qif_import_convert_progress_pause_cb
 *
 * Invoked when the "Pause" button is clicked.
 ********************************************************************/
void
gnc_ui_qif_import_convert_progress_pause_cb(GtkButton * button,
        gpointer user_data)
{
    QIFImportWindow *wind = user_data;
    SCM toggle_pause      = scm_c_eval_string("qif-import:toggle-pause");
    SCM progress;

    if (!wind->busy)
        return;

    /* Create SCM for the progress helper. */
    progress = SWIG_NewPointerObj(wind->convert_progress,
                                  SWIG_TypeQuery("_p__GNCProgressDialog"),
                                  0);

    /* Pause (or resume) the currently running operation. */
    scm_call_1(toggle_pause, progress);

    /* Swap the button label between pause and resume. */
    if (strcmp(gtk_button_get_label(button), _("_Resume")))
    {
        gtk_button_set_use_stock(button, FALSE);
        gtk_button_set_use_underline(button, TRUE);
        gtk_button_set_label(button, _("_Resume"));
    }
    else
    {
        gtk_button_set_use_stock(button, TRUE);
        gtk_button_set_use_underline(button, FALSE);
        gtk_button_set_label(button, "gtk-media-pause");
    }
}


/********************************************************************
 * gnc_ui_qif_import_convert_progress_start_cb
 *
 * Invoked when the "Start" button is clicked.
 ********************************************************************/
void
gnc_ui_qif_import_convert_progress_start_cb(GtkButton * button,
        gpointer user_data)
{
    QIFImportWindow   *wind = user_data;
    gint num = gtk_assistant_get_current_page (GTK_ASSISTANT(wind->window));
    GtkWidget *page = gtk_assistant_get_nth_page (GTK_ASSISTANT(wind->window), num);

    SCM qif_to_gnc      = scm_c_eval_string("qif-import:qif-to-gnc");
    SCM find_duplicates = scm_c_eval_string("gnc:account-tree-find-duplicates");
    SCM retval;

    /* SCM for the progress dialog. */
    SCM progress = SWIG_NewPointerObj(wind->convert_progress,
                                      SWIG_TypeQuery("_p__GNCProgressDialog"),
                                      0);

    /* The default currency. */
    const gchar *currname = gtk_entry_get_text( GTK_ENTRY( gtk_bin_get_child( GTK_BIN( GTK_COMBO_BOX(wind->currency_picker)))));

    /* Raise the busy flag so the assistant can't be canceled unexpectedly. */
    wind->busy = TRUE;
    gtk_widget_set_sensitive(wind->convert_pause, TRUE);
    gtk_widget_set_sensitive(wind->convert_start, FALSE);

    /* Clear any previous pause or cancel state. */
    scm_c_eval_string("(qif-import:reset-cancel-pause)");

    /* Update the commodities. */
    gnc_ui_qif_import_commodity_update(wind);

    /*
     * Convert the QIF data into GnuCash data.
     *
     * A Scheme function does all the work.  The return value is the
     * root account of an account tree containing all the new accounts
     * and transactions. Upon failure, #f is returned. If the user
     * cancels, #t is returned.
     */

    /* This step will fill 70% of the bar. */
    gnc_progress_dialog_push(wind->convert_progress, 0.7);
    retval = scm_apply(qif_to_gnc,
                       SCM_LIST8(wind->imported_files,
                                 wind->acct_map_info,
                                 wind->cat_map_info,
                                 wind->memo_map_info,
                                 wind->security_hash,
                                 scm_from_utf8_string(currname ? currname : ""),
                                 wind->transaction_status,
                                 progress),
                       SCM_EOL);
    gnc_progress_dialog_pop(wind->convert_progress);

    if (retval == SCM_BOOL_T)
    {
        /* Canceled by the user. */

        /* Disable the pause button. */
        gtk_widget_set_sensitive(wind->convert_pause, FALSE);

        /* Remove any converted data. */
        gnc_progress_dialog_set_sub(wind->convert_progress, _("Cleaning up"));
        gnc_ui_qif_import_convert_undo(wind);

        /* Inform the user. */
        gnc_progress_dialog_set_sub(wind->convert_progress, _("Canceled"));
        gnc_progress_dialog_reset_value(wind->convert_progress);

        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }
    else if (retval == SCM_BOOL_F)
    {
        /* An bug was encountered during conversion. */

        /* Disable the pause button. */
        gtk_widget_set_sensitive(wind->convert_pause, FALSE);

        /* Remove any converted data. */
        gnc_progress_dialog_set_sub(wind->convert_progress, _("Cleaning up"));
        gnc_ui_qif_import_convert_undo(wind);

        /* Inform the user. */
        gnc_progress_dialog_append_log(wind->convert_progress,
                                       _( "A bug was detected while converting the QIF data."));
        gnc_progress_dialog_set_sub(wind->convert_progress, _("Failed"));
        gnc_progress_dialog_reset_value(wind->convert_progress);
        gnc_error_dialog(wind->window, "%s",
                         _( "A bug was detected while converting the QIF data."));
        /* FIXME: How should we request that the user report this problem? */

        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }
    else if (scm_is_symbol(retval))
    {
        /* An error was encountered during conversion. */

        /* Disable the pause button. */
        gtk_widget_set_sensitive(wind->convert_pause, FALSE);

        /* Remove any converted data. */
        gnc_progress_dialog_set_sub(wind->convert_progress, _("Cleaning up"));
        gnc_ui_qif_import_convert_undo(wind);

        /* Inform the user. */
        gnc_progress_dialog_set_sub(wind->convert_progress, _("Failed"));
        gnc_progress_dialog_reset_value(wind->convert_progress);

        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }

    /* Save the imported account tree. */
    scm_gc_unprotect_object(wind->imported_account_tree);
    wind->imported_account_tree = retval;
    scm_gc_protect_object(wind->imported_account_tree);

    /*
     * Detect potentially duplicated transactions.
     */

    /* This step will fill the remainder of the bar. */
    gnc_progress_dialog_push(wind->convert_progress, 1);
    retval = scm_call_3(find_duplicates,
                        scm_c_eval_string("(gnc-get-current-root-account)"),
                        wind->imported_account_tree, progress);
    gnc_progress_dialog_pop(wind->convert_progress);

    /* Save the results. */
    scm_gc_unprotect_object(wind->match_transactions);
    wind->match_transactions = retval;
    scm_gc_protect_object(wind->match_transactions);

    if (retval == SCM_BOOL_T)
    {
        /* Canceled by the user. */
        gtk_widget_set_sensitive(wind->convert_pause, FALSE);
        gnc_progress_dialog_set_sub(wind->convert_progress, _("Canceling"));
        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }
    else if (retval == SCM_BOOL_F)
    {
        /* An error occurred during duplicate checking. */

        /* Remove any converted data. */
        gnc_progress_dialog_set_sub(wind->convert_progress, _("Cleaning up"));
        gnc_ui_qif_import_convert_undo(wind);

        /* Inform the user. */
        gnc_progress_dialog_append_log(wind->convert_progress,
                                       _( "A bug was detected while detecting duplicates."));
        gnc_progress_dialog_set_sub(wind->convert_progress, _("Failed"));
        gnc_progress_dialog_reset_value(wind->convert_progress);
        gnc_error_dialog(wind->window, "%s",
                         _( "A bug was detected while detecting duplicates."));
        /* FIXME: How should we request that the user report this problem? */

        gtk_widget_set_sensitive(wind->convert_pause, FALSE);
        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }

    /* Enable the Assistant Forward Button */
    gtk_assistant_set_page_complete (GTK_ASSISTANT(wind->window), page, TRUE);

    /* Set Pause and Start buttons */
    gtk_widget_set_sensitive(wind->convert_pause, FALSE);
    gtk_widget_set_sensitive(wind->convert_start, FALSE);

    if (wind->load_stop == FALSE)
    {
        /* The conversion completed successfully. */
        gnc_progress_dialog_set_sub(wind->convert_progress,
                                    _("Conversion completed"));
        gnc_progress_dialog_set_value(wind->convert_progress, 1);

        gtk_widget_set_sensitive(wind->convert_pause, FALSE);
        wind->busy = FALSE;

        /* If the log is empty, move on to the next page automatically. */
        if (gtk_text_buffer_get_char_count(gtk_text_view_get_buffer(GTK_TEXT_VIEW(wind->convert_log))) == 0)
            gtk_assistant_set_current_page (GTK_ASSISTANT(wind->window), num + 1);
    }
}


/********************************************************************
 * gnc_ui_qif_import_convert_progress_prepare
 *
 * Prepare the data conversion progress page for display.
 ********************************************************************/
void
gnc_ui_qif_import_convert_progress_prepare(GtkAssistant *assistant,
        gpointer user_data)
{
    QIFImportWindow   *wind = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    /* Recompute assistant Buttons */
    gtk_assistant_update_buttons_state( assistant );

    /* Reset the progress display. */
    gnc_progress_dialog_set_primary(wind->convert_progress, "");
    gnc_progress_dialog_set_secondary(wind->convert_progress,
                                      _("When you press the Start Button, GnuCash will import your QIF data. If there are no errors or warnings, you will automatically proceed to the next step. Otherwise, the details will be shown below for your review."));
    gnc_progress_dialog_set_sub(wind->convert_progress, " ");
    gnc_progress_dialog_reset_value(wind->convert_progress);
    gnc_progress_dialog_reset_log(wind->convert_progress);

    /* Set Pause and Start buttons */
    gtk_widget_set_sensitive(wind->convert_pause, FALSE);
    gtk_widget_set_sensitive(wind->convert_start, TRUE);

    /* Disable the assistant Forward button */
    gtk_assistant_set_page_complete (assistant, page, FALSE);

    /* Before creating transactions, if this is a new book, let user specify
     * book options, since they affect how transactions are created */
    if (wind->new_book)
        wind->new_book = gnc_new_book_option_display(wind->window);
}


/*****************************************
 * Page 15 - Match Doc. Page Procedures
 *****************************************/

/********************************************************************
 * gnc_ui_qif_import_duplicates_doc_prepare
 ********************************************************************/
void
gnc_ui_qif_import_duplicates_doc_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);
    gint total = gtk_assistant_get_n_pages (assistant);
    gtk_assistant_update_buttons_state (assistant);

    PINFO("Total Number of Assistant Pages is %d", gtk_assistant_get_n_pages (assistant));

    /* Enable the Assistant Forward Button */
    gtk_assistant_set_page_complete (assistant, page, TRUE);

    /* Jump to Summary page if load_stop TRUE */
    if (wind->load_stop)
        gtk_assistant_set_current_page (assistant, total - 1 );

    /* Jump over doc page if show_doc_pages FALSE */
    if (!wind->show_doc_pages)
        gtk_assistant_set_current_page (assistant, num + 1 );

    /* Don't show doc page if there are no duplicates */
    if (scm_is_null(wind->match_transactions))
        gtk_assistant_set_current_page (assistant, num + 1 );
}


/**********************************************
 * Page 16 - Match Duplicates Page Procedures
 **********************************************/

/********************************************************************
 * gnc_ui_qif_import_duplicates_match_prepare
 ********************************************************************/
void
gnc_ui_qif_import_duplicates_match_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    GtkTreeView      *view;
    GtkListStore     *store;
    SCM               duplicates;
    SCM               current_xtn;
    Transaction      *gnc_xtn;
    Split            *gnc_split;
    GtkTreeIter       iter;
    GtkTreeSelection *selection;
    GtkTreePath      *path;
    const gchar      *amount_str;
    int               rownum = 0;

    if (!scm_is_null(wind->match_transactions))
    {
        view = GTK_TREE_VIEW(wind->new_transaction_view);
        store = GTK_LIST_STORE(gtk_tree_view_get_model(view));
        gtk_list_store_clear(store);

        if (!scm_is_list(wind->match_transactions))
            return;

        /* Loop through the list of new, potentially duplicate transactions. */
        duplicates = wind->match_transactions;
        while (!scm_is_null(duplicates))
        {
            current_xtn = SCM_CAAR(duplicates);
#define FUNC_NAME "xaccTransCountSplits"
            gnc_xtn = SWIG_MustGetPtr(current_xtn,
                                      SWIG_TypeQuery("_p_Transaction"), 1, 0);
#undef FUNC_NAME
            if (xaccTransCountSplits(gnc_xtn) > 2)
                amount_str = _("(split)");
            else
            {
                gnc_split = xaccTransGetSplit(gnc_xtn, 0);
                amount_str =
                    xaccPrintAmount(gnc_numeric_abs(xaccSplitGetValue(gnc_split)),
                                    gnc_account_print_info
                                    (xaccSplitGetAccount(gnc_split), TRUE));
            }
            gtk_list_store_append(store, &iter);
            gtk_list_store_set
            (store, &iter,
             QIF_TRANS_COL_INDEX, rownum++,
             QIF_TRANS_COL_DATE,
             gnc_print_date(xaccTransRetDatePostedTS(gnc_xtn)),
             QIF_TRANS_COL_DESCRIPTION, xaccTransGetDescription(gnc_xtn),
             QIF_TRANS_COL_AMOUNT, amount_str,
             -1);

            duplicates = SCM_CDR(duplicates);
        }
        selection = gtk_tree_view_get_selection(view);
        path = gtk_tree_path_new_from_indices(0, -1);
        gtk_tree_selection_select_path(selection, path);
        gtk_tree_path_free(path);
    }
    else
        gtk_assistant_set_current_page (assistant, num + 1 );

    /* Enable the Assistant Forward Button */
    gtk_assistant_set_page_complete (assistant, page, TRUE);
}


/*************************************
 * Page 17 - Apply Page Procedures
 *************************************/

/********************************************************************
 * gnc_ui_qif_import_end_page_prepare
 ********************************************************************/
void
gnc_ui_qif_import_end_page_prepare (GtkAssistant *assistant,
                                    gpointer user_data)
{
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    /* Enable the Assistant Forward Button */
    gtk_assistant_set_page_complete (assistant, page, TRUE);
}


/********************************************************************
 * gnc_ui_qif_import_finish_cb
 *
 * Invoked when the "Apply" button is clicked on the final page.
 ********************************************************************/
void
gnc_ui_qif_import_finish_cb (GtkAssistant *gtkassistant,
                             gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    SCM save_map_prefs = scm_c_eval_string("qif-import:save-map-prefs");
    SCM cat_and_merge = scm_c_eval_string("gnc:account-tree-catenate-and-merge");
    SCM prune_xtns = scm_c_eval_string("gnc:prune-matching-transactions");
    SCM scm_result;

    GncPluginPage *page;
    gboolean acct_tree_found = FALSE;

    gnc_suspend_gui_refresh();

    /* Prune any imported transactions that were determined to be duplicates. */
    if (wind->match_transactions != SCM_BOOL_F)
        scm_call_1(prune_xtns, wind->match_transactions);

    /* Merge the imported account tree with the existing one. */
    if (wind->imported_account_tree != SCM_BOOL_F)
        scm_call_2(cat_and_merge,
                   scm_c_eval_string("(gnc-get-current-root-account)"),
                   wind->imported_account_tree);

    gnc_resume_gui_refresh();

    /* Save the user's mapping preferences. */
    scm_result = scm_apply(save_map_prefs,
                           SCM_LIST5(wind->acct_map_info, wind->cat_map_info,
                                     wind->memo_map_info, wind->security_hash,
                                     wind->security_prefs),
                           SCM_EOL);

    if (scm_result == SCM_BOOL_F)
        gnc_warning_dialog(wind->window, "%s",
                           _("GnuCash was unable to save your mapping preferences."));

    /* Open an account tab in the main window if one doesn't exist already. */
    gnc_main_window_foreach_page(gnc_ui_qif_import_check_acct_tree,
                                 &acct_tree_found);

    wind->acct_tree_found = acct_tree_found;
    if (!acct_tree_found)
    {
        page = gnc_plugin_page_account_tree_new();
        gnc_main_window_open_page(NULL, page);
    }
}


/***************************************
 * Page 18 - Summary Page Procedures
 ***************************************/

/********************************************************************
 * gnc_ui_qif_import_summary_page_prepare
 ********************************************************************/
void
gnc_ui_qif_import_summary_page_prepare (GtkAssistant *assistant,
                                        gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    gchar *text;

    if (wind->load_stop)
        text = g_strdup_printf(_("There was a problem with the import."));
    else
        text = g_strdup_printf(_("QIF Import Completed."));

    gtk_label_set_markup(GTK_LABEL(wind->summary_text), g_strdup_printf("<span size=\"large\"><b>%s</b></span>", text));

    g_free(text);

    /* Enable the Assistant Forward Button */
    gtk_assistant_set_page_complete (assistant, page, TRUE);
}


/********************************************************************
 * Prepare callback for assistant pages.
 ********************************************************************/
void gnc_ui_qif_import_prepare_cb (GtkAssistant  *assistant, GtkWidget *page,
                                   gpointer user_data)
{
    gint currentpage = gtk_assistant_get_current_page(assistant);
    GtkWidget *mypage = gtk_assistant_get_nth_page (assistant, currentpage);
    const char *pagename = gtk_buildable_get_name(GTK_BUILDABLE(mypage));

    PINFO("Builder Page Name is %s", gtk_buildable_get_name(GTK_BUILDABLE(mypage)));

    if (!g_strcmp0 (pagename, "start_page"))
    {
        /* Current page is Intro page */
        gnc_ui_qif_import_intro_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "load_file_page"))
    {
        /* Current page is File Load */
        gnc_ui_qif_import_load_file_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "load_progress_page"))
    {
        /* Current page is Load Progress */
        gnc_ui_qif_import_load_progress_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "date_format_page"))
    {
        /* Current page is date page */
        gnc_ui_qif_import_date_format_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "account_name_page"))
    {
        /* Current page is account page */
        gnc_ui_qif_import_account_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "loaded_files_page"))
    {
        /* Current page is loaded files page */
        gnc_ui_qif_import_loaded_files_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "account_doc_page"))
    {
        /* Current page is  Account Doc. page */
        gnc_ui_qif_import_account_doc_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "account_match_page"))
    {
        /* Current page is Account Match page */
        gnc_ui_qif_import_account_match_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "category_doc_page"))
    {
        /* Current page is Catagory Doc. page */
        gnc_ui_qif_import_catagory_doc_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "category_match_page"))
    {
        /* Current page is Catagory Match page */
        gnc_ui_qif_import_catagory_match_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "memo_doc_page"))
    {
        /* Current page is Memo Doc. page */
        gnc_ui_qif_import_memo_doc_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "memo_match_page"))
    {
        /* Current page is Memo Match page */
        gnc_ui_qif_import_memo_match_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "currency_book_option_page"))
    {
        /* Current page is Currency page */
        gnc_ui_qif_import_currency_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "commodity_doc_page"))
    {
        /* Current page is Commodity Doc. page */
        gnc_ui_qif_import_commodity_doc_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "commodity_page"))
    {
        /* Current page is Commodity page */
        /* gnc_ui_qif_import_commodity_prepare (assistant, user_data); */
    }
    else if (!g_strcmp0 (pagename, "convert_progress_page"))
    {
        /* Current page is Conversion progress page */
        gnc_ui_qif_import_convert_progress_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "duplicates_doc_page"))
    {
        /* Current page is Duplicates Doc page */
        gnc_ui_qif_import_duplicates_doc_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "duplicates_match_page"))
    {
        /* Current page is Duplicates Match page */
        gnc_ui_qif_import_duplicates_match_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "end_page"))
    {
        /* Current page is the end page */
        gnc_ui_qif_import_end_page_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "summary_page"))
    {
        /* Current page is the summary page */
        gnc_ui_qif_import_summary_page_prepare (assistant, user_data);
    }
    else
    {
        /* Current page is a new commodity page */
        gnc_ui_qif_import_commodity_new_prepare (assistant, user_data);
    }
}


/********************************************************************
 * get_assistant_widgets
 *
 * Get all builder-defined widgets that need to be actively managed.
 ********************************************************************/
static void
get_assistant_widgets(QIFImportWindow *wind, GtkBuilder *builder)
{
    g_return_if_fail(wind);
    g_return_if_fail(builder);

    wind->window             = GTK_WIDGET(gtk_builder_get_object (builder, "QIF Import Assistant"));
    wind->filename_entry     = GTK_WIDGET(gtk_builder_get_object (builder, "qif_filename_entry"));
    wind->load_pause         = GTK_WIDGET(gtk_builder_get_object (builder, "load_progress_pause"));
    wind->load_start         = GTK_WIDGET(gtk_builder_get_object (builder, "load_progress_start"));
    wind->load_log           = GTK_WIDGET(gtk_builder_get_object (builder, "load_progress_log"));
    wind->load_progress      = gnc_progress_dialog_custom(
                                   GTK_LABEL(gtk_builder_get_object (builder, "load_progress_primary")),
                                   GTK_LABEL(gtk_builder_get_object (builder, "load_progress_secondary")),
                                   GTK_PROGRESS_BAR(gtk_builder_get_object (builder, "load_progress_bar")),
                                   GTK_LABEL(gtk_builder_get_object (builder, "load_progress_sub")),
                                   GTK_TEXT_VIEW(wind->load_log));
    wind->acct_entry         = GTK_WIDGET(gtk_builder_get_object (builder, "qif_account_entry"));
    wind->date_format_combo  = GTK_WIDGET(gtk_builder_get_object (builder, "date_format_combobox"));
    wind->selected_file_view = GTK_WIDGET(gtk_builder_get_object (builder, "selected_file_view"));
    wind->unload_file_btn    = GTK_WIDGET(gtk_builder_get_object (builder, "unload_file_button"));
    wind->currency_picker    = GTK_WIDGET(gtk_builder_get_object (builder, "currency_comboboxentry"));
    wind->book_option_label  = GTK_WIDGET(gtk_builder_get_object (builder, "book_option_label"));
    wind->book_option_message = GTK_WIDGET(gtk_builder_get_object (builder, "book_option_message_label"));
    wind->acct_view          = GTK_WIDGET(gtk_builder_get_object (builder, "account_page_view"));
    wind->acct_view_count    = GTK_WIDGET(gtk_builder_get_object (builder, "account_page_count"));
    wind->acct_view_btn      = GTK_WIDGET(gtk_builder_get_object (builder, "account_page_change"));
    wind->cat_view           = GTK_WIDGET(gtk_builder_get_object (builder, "category_page_view"));
    wind->cat_view_count     = GTK_WIDGET(gtk_builder_get_object (builder, "category_page_count"));
    wind->cat_view_btn       = GTK_WIDGET(gtk_builder_get_object (builder, "category_page_change"));
    wind->memo_view          = GTK_WIDGET(gtk_builder_get_object (builder, "memo_page_view"));
    wind->memo_view_count    = GTK_WIDGET(gtk_builder_get_object (builder, "memo_page_count"));
    wind->memo_view_btn      = GTK_WIDGET(gtk_builder_get_object (builder, "memo_page_change"));
    wind->convert_pause      = GTK_WIDGET(gtk_builder_get_object (builder, "convert_progress_pause"));
    wind->convert_start      = GTK_WIDGET(gtk_builder_get_object (builder, "convert_progress_start"));
    wind->convert_log        = GTK_WIDGET(gtk_builder_get_object (builder, "convert_progress_log"));
    wind->convert_progress   = gnc_progress_dialog_custom(
                                   GTK_LABEL(gtk_builder_get_object (builder, "convert_progress_primary")),
                                   GTK_LABEL(gtk_builder_get_object (builder, "convert_progress_secondary")),
                                   GTK_PROGRESS_BAR(gtk_builder_get_object (builder, "convert_progress_bar")),
                                   GTK_LABEL(gtk_builder_get_object (builder, "convert_progress_sub")),
                                   GTK_TEXT_VIEW(wind->convert_log));
    wind->summary_text       = GTK_WIDGET(gtk_builder_get_object (builder, "summary_page"));

    wind->new_transaction_view =
        GTK_WIDGET(gtk_builder_get_object (builder, "new_transaction_view"));
    wind->old_transaction_view =
        GTK_WIDGET(gtk_builder_get_object (builder, "old_transaction_view"));

    gnc_assistant_set_colors (GTK_ASSISTANT (wind->window));
}


/********************************************************************
 * build_views
 *
 * Build the details of all GtkTreeView widgets.
 ********************************************************************/
static void
build_views(QIFImportWindow *wind)
{
    GtkTreeView *view;
    GtkListStore *store;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;

    g_return_if_fail(wind);

    /* Set up the selected file view */
    view = GTK_TREE_VIEW(wind->selected_file_view);
    store = gtk_list_store_new(NUM_FILENAME_COLS, G_TYPE_INT, G_TYPE_STRING);
    gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
    g_object_unref(store);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes("unused",
             renderer,
             "text",
             FILENAME_COL_NAME,
             NULL);
    gtk_tree_view_append_column(view, column);

    selection = gtk_tree_view_get_selection(view);
    g_signal_connect(selection, "changed",
                     G_CALLBACK(gnc_ui_qif_import_select_loaded_file_cb),
                     wind);

    /* Set up the QIF account to GnuCash account matcher. */
    create_account_picker_view(wind->acct_view, _("QIF account name"),
                               G_CALLBACK(gnc_ui_qif_import_account_activate_cb),
                               G_CALLBACK(gnc_ui_qif_import_account_select_cb),
                               wind);

    /* Set up the QIF category to GnuCash account matcher. */
    create_account_picker_view(wind->cat_view,  _("QIF category name"),
                               G_CALLBACK(gnc_ui_qif_import_category_activate_cb),
                               G_CALLBACK(gnc_ui_qif_import_category_select_cb),
                               wind);

    /* Set up the QIF payee/memo to GnuCash account matcher. */
    create_account_picker_view(wind->memo_view, _("QIF payee/memo"),
                               G_CALLBACK(gnc_ui_qif_import_memo_activate_cb),
                               G_CALLBACK(gnc_ui_qif_import_memo_select_cb),
                               wind);

    /* Set up the new transaction view */
    view = GTK_TREE_VIEW(wind->new_transaction_view);
    store = gtk_list_store_new(NUM_QIF_TRANS_COLS, G_TYPE_INT, G_TYPE_STRING,
                               G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN);
    gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
    g_object_unref(store);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Date"),
             renderer,
             "text",
             QIF_TRANS_COL_DATE,
             NULL);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Description"),
             renderer,
             "text",
             QIF_TRANS_COL_DESCRIPTION,
             NULL);
    gtk_tree_view_append_column(view, column);
    gtk_tree_view_column_set_expand(column, TRUE);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Amount"),
             renderer,
             "text",
             QIF_TRANS_COL_AMOUNT,
             NULL);
    gtk_tree_view_append_column(view, column);

    selection = gtk_tree_view_get_selection(view);
    g_signal_connect(selection, "changed",
                     G_CALLBACK(gnc_ui_qif_import_duplicate_new_select_cb),
                     wind);

    /* Set up the old transaction view */
    view = GTK_TREE_VIEW(wind->old_transaction_view);
    store = gtk_list_store_new(NUM_QIF_TRANS_COLS, G_TYPE_INT, G_TYPE_STRING,
                               G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN);
    gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
    g_object_unref(store);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Date"),
             renderer,
             "text",
             QIF_TRANS_COL_DATE,
             NULL);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Description"),
             renderer,
             "text",
             QIF_TRANS_COL_DESCRIPTION,
             NULL);
    gtk_tree_view_append_column(view, column);
    gtk_tree_view_column_set_expand(column, TRUE);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Amount"),
             renderer,
             "text",
             QIF_TRANS_COL_AMOUNT,
             NULL);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_toggle_new();
    column = gtk_tree_view_column_new_with_attributes(_("Match?"),
             renderer,
             "active",
             QIF_TRANS_COL_CHECKED,
             NULL);
    gtk_tree_view_append_column(view, column);

    selection = gtk_tree_view_get_selection(view);
    g_signal_connect(selection, "changed",
                     G_CALLBACK(gnc_ui_qif_import_duplicate_old_select_cb),
                     wind);
}


/********************************************************************
 * gnc_ui_qif_import_assistant_make
 *
 * Build a new QIF import assistant.
 ********************************************************************/
static GtkWidget *
gnc_ui_qif_import_assistant_make(QIFImportWindow *qif_win)
{
    GtkBuilder        *builder;
    GtkWidget         *box;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "assistant-qif-import.glade", "currency_liststore");
    gnc_builder_add_from_file (builder, "assistant-qif-import.glade", "date_format_liststore");
    gnc_builder_add_from_file (builder, "assistant-qif-import.glade", "QIF Import Assistant");

    qif_win->new_namespaces       = NULL;
    qif_win->selected_transaction = 0;
    qif_win->busy                 = FALSE;
    /* In order to include a book options display on the creation of a new book,
     * we need to detect when we are dealing with a new book. */
    qif_win->new_book = gnc_is_new_book();

    /* Get all user preferences related to QIF importing. */
    get_preferences(qif_win);

    /* Set up the Scheme side of things. Note that if a session/book did not
     * exist prior to this function, it is created within scheme function
     * "qif-import:load-map-prefs", so we need to have set the flag previously */
    initialize_scheme(qif_win);

    /* Get all interesting builder-defined widgets. */
    get_assistant_widgets(qif_win, builder);

    /* Make this window stay on top */
    gtk_window_set_transient_for (GTK_WINDOW (qif_win->window),
				  GTK_WINDOW (gnc_ui_get_toplevel ()));

    /* Build the details of all GtkTreeView widgets. */
    build_views(qif_win);

    /* Currency Page */
    /* Set a default currency for new accounts */
    qif_win->currency_picker = gnc_currency_edit_new();
    gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(qif_win->currency_picker), gnc_default_currency());
    gtk_widget_show (qif_win->currency_picker);
    box = GTK_WIDGET(gtk_builder_get_object (builder, "currency_picker_hbox"));
    gtk_box_pack_start(GTK_BOX(box), qif_win->currency_picker, TRUE, TRUE, 0);

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(qif_win->window));

    g_signal_connect( qif_win->window, "destroy",
                      G_CALLBACK(gnc_ui_qif_import_assistant_destroy), qif_win );

    gtk_builder_connect_signals(builder, qif_win);

    g_object_unref(G_OBJECT(builder));

    gtk_widget_show_all(qif_win->window);
    gtk_window_present(GTK_WINDOW(qif_win->window));

    return qif_win->window;
}


/********************************************
 * gnc_ui_qif_import_assistant_close_handler
 ********************************************/
static void
gnc_ui_qif_import_assistant_close_handler( gpointer user_data )
{
    QIFImportWindow *qif_win = user_data;

    gnc_save_window_size(GNC_PREFS_GROUP, GTK_WINDOW(qif_win->window));
    gtk_widget_destroy( qif_win->window );
}


/********************************************
 * gnc_file_qif_import
 ********************************************/
void
gnc_file_qif_import(void)
{
    QIFImportWindow *qif_win;
    gint component_id;

    qif_win = g_new0 (QIFImportWindow, 1);

    /* pop up the QIF File Import dialog box */
    gnc_ui_qif_import_assistant_make(qif_win);

    component_id = gnc_register_gui_component (ASSISTANT_QIF_IMPORT_CM_CLASS,
                   NULL, gnc_ui_qif_import_assistant_close_handler,
                   qif_win);

    gnc_gui_component_watch_entity_type (component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    gtk_widget_show_all (qif_win->window);

    gnc_window_adjust_for_screen (GTK_WINDOW(qif_win->window));
}
