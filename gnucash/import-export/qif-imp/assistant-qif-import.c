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

#include <config.h>

#include <platform.h>
#include <libguile.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

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
#include "gnc-import-assistant.h"
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
#include "swig-runtime.h"
#include "guile-mappings.h"
#include <gfec.h>

#define ASSISTANT_QIF_IMPORT_CM_CLASS            "assistant-qif-import"
#define GNC_PREFS_GROUP                          "dialogs.import.qif"
#define GNC_PREF_SHOW_DOC                        "show-doc"
#define GNC_PREF_DEFAULT_TRANS_STATUS_CLEARED    "default-status-cleared"
#define GNC_PREF_DEFAULT_TRANS_STATUS_NOTCLEARED "default-status-notcleared"
#define GNC_PREF_DEFAULT_TRANS_STATUS_RECONCILED "default-status-reconciled"

static QofLogModule log_module = GNC_MOD_ASSISTANT;

typedef struct
{
    GObject parent_instance;
    gint index;
    gchar *qif_name;
    gchar *gnc_name;
    gboolean is_new;
} QIFAccountMappingRow;

typedef struct
{
    GObjectClass parent_class;
} QIFAccountMappingRowClass;

GType qif_account_mapping_row_get_type (void);

G_DEFINE_TYPE (QIFAccountMappingRow, qif_account_mapping_row, G_TYPE_OBJECT)

typedef struct _qifaccountmappingview
{
    GtkBox *container;
    GListStore *rows;
    GtkMultiSelection *selection;
    GtkColumnView *view;
    GtkWidget *count_label;
    GtkWidget *change_button;
    QIFImportWindow *wind;
    SCM *map_info;
    SCM *display_info;
    void (*update_page)(QIFImportWindow *);
    gint previous_row;
} QIFAccountMappingView;

typedef struct
{
    GObject parent_instance;
    gint index;
    gchar *path;
} QIFFileRow;

typedef struct
{
    GObjectClass parent_class;
} QIFFileRowClass;

GType qif_file_row_get_type (void);

G_DEFINE_TYPE (QIFFileRow, qif_file_row, G_TYPE_OBJECT)

typedef struct
{
    GObject parent_instance;
    gint index;
    gchar *date;
    time64 date_value;
    gchar *description;
    gchar *amount;
    gdouble amount_value;
    gboolean checked;
} QIFTransactionRow;

typedef struct
{
    GObjectClass parent_class;
} QIFTransactionRowClass;

GType qif_transaction_row_get_type (void);

G_DEFINE_TYPE (QIFTransactionRow, qif_transaction_row, G_TYPE_OBJECT)

typedef struct _qiffileview
{
    GtkBox *container;
    GListStore *rows;
    GtkSingleSelection *selection;
    GtkColumnView *view;
    QIFImportWindow *wind;
} QIFFileView;

typedef struct _qiftransactionview
{
    GtkBox *container;
    GListStore *rows;
    GtkSortListModel *sorted_rows;
    GtkSingleSelection *selection;
    GtkColumnView *view;
    QIFImportWindow *wind;
} QIFTransactionView;

struct _qifimportwindow
{
    GtkWidget * window;

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
    GtkDropDown * date_format_dropdown;
    GtkStringList * date_format_model;

    /* Widgets on the files loaded page. */
    GtkWidget * selected_file_view;
    QIFFileView file_view;
    GtkWidget * unload_file_btn;

    /* Widgets on the account matching page. */
    GtkWidget * acct_view;
    GtkWidget * acct_view_count;
    GtkWidget * acct_view_btn;
    QIFAccountMappingView acct_mapping;

    /* Widgets on the category matching page. */
    GtkWidget * cat_view;
    GtkWidget * cat_view_count;
    GtkWidget * cat_view_btn;
    QIFAccountMappingView cat_mapping;

    /* Widgets on the memo matching page. */
    GtkWidget * memo_view;
    GtkWidget * memo_view_count;
    GtkWidget * memo_view_btn;
    QIFAccountMappingView memo_mapping;

    /* Widgets on the currency & book options page. */
    GtkWidget * currency_picker;
    GtkWidget * book_option_label;
    GtkWidget * book_option_message;

    /* Widgets on the commodity page. */
    gint        num_new_pages;
    GtkWidget * commodity_notebook;
    GList     * commodity_notebook_pages;
    gint        timeout_id;

    /* Conversion progress page. */
    GtkWidget * convert_pause;
    GtkWidget * convert_start;
    GtkWidget * convert_log;
    GNCProgressDialog *convert_progress;

    /* Widgets on the duplicates page. */
    GtkWidget * new_transaction_view;
    GtkWidget * old_transaction_view;
    QIFTransactionView new_transactions;
    QIFTransactionView old_transactions;

    /* Widgets on the summary page. */
    GtkWidget * summary_text;

    gboolean  show_doc_pages;
    gboolean  ask_date_format;
    gboolean  busy;
    gboolean  read_file_warnings;
    gboolean  load_stop;
    gboolean  acct_tree_found;
    gboolean  new_book;
    gboolean  new_book_options_pending;
    gboolean  cancel_pending;

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
    gchar    *date_format;
};

struct _qifnotebookpage
{
    GtkWidget     *notebook_page;
    GtkWidget     *namespace_combo;
    GtkWidget     *name_entry;
    GtkWidget     *mnemonic_entry;
    gnc_commodity *commodity;
    gboolean       page_complete;
    SCM            hash_key;
};

typedef struct _qifnotebookpage QIFCommNotebookPage;

static void gnc_ui_qif_import_assistant_destroy (GtkWidget *object, gpointer user_data);
static void gnc_ui_qif_import_assistant_close_handler (gpointer user_data);

static gboolean gnc_ui_qif_import_assistant_skip_page (GncImportAssistant *assistant, GtkWidget *page, QIFImportWindow *wind);
static int gnc_ui_qif_import_assistant_page_forward (int current_page, gpointer data);

void gnc_ui_qif_import_cancel_cb (GncImportAssistant *gtkassistant, gpointer user_data);
void gnc_ui_qif_import_prepare_cb (GncImportAssistant *assistant, GtkWidget *page, gpointer user_data);
void gnc_ui_qif_import_finish_cb (GncImportAssistant *gtkassistant, gpointer user_data);
void gnc_ui_qif_import_close_cb (GncImportAssistant *gtkassistant, gpointer user_data);

void gnc_ui_qif_import_intro_prepare (GncImportAssistant *assistant, gpointer user_data);

void gnc_ui_qif_import_load_file_prepare (GncImportAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_select_file_cb (GtkButton *button, gpointer user_data);

void gnc_ui_qif_import_load_progress_prepare (GncImportAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_load_progress_pause_cb (GtkButton *button, gpointer user_data);
void gnc_ui_qif_import_load_progress_start_cb (GtkButton * button, gpointer user_data);

static gboolean gnc_ui_qif_import_skip_date_format (GncImportAssistant *assistant, QIFImportWindow *wind);
void gnc_ui_qif_import_date_valid_cb (GtkDropDown *dropdown,
                                      GParamSpec *pspec, gpointer user_data);

void gnc_ui_qif_import_account_prepare (GncImportAssistant *assistant, gpointer user_data);
static gboolean gnc_ui_qif_import_skip_account (GncImportAssistant *assistant, QIFImportWindow *wind);
void gnc_ui_qif_import_acct_valid_cb (GtkWidget *widget, gpointer user_data);
void gnc_ui_qif_import_acct_enter_cb (GtkWidget * widget, gpointer user_data);

void gnc_ui_qif_import_loaded_files_prepare (GncImportAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_load_another_cb (GtkButton *button, gpointer user_data);
void gnc_ui_qif_import_unload_file_cb (GtkButton *button, gpointer user_data);

static void update_file_page (QIFImportWindow * wind);

void gnc_ui_qif_import_account_match_prepare (GncImportAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_account_doc_prepare (GncImportAssistant *assistant, gpointer user_data);
static gboolean gnc_ui_qif_import_skip_account_doc (QIFImportWindow *wind);
void gnc_ui_qif_import_account_rematch_cb (GtkButton *button, gpointer user_data);

void gnc_ui_qif_import_category_match_prepare (GncImportAssistant *assistant, gpointer user_data);
static gboolean gnc_ui_qif_import_skip_category_match (QIFImportWindow *wind);
void gnc_ui_qif_import_category_doc_prepare (GncImportAssistant *assistant, gpointer user_data);
static gboolean gnc_ui_qif_import_skip_category_doc (QIFImportWindow *wind);
void gnc_ui_qif_import_category_rematch_cb (GtkButton *button, gpointer user_data);

void gnc_ui_qif_import_memo_match_prepare (GncImportAssistant *assistant, gpointer user_data);
static gboolean gnc_ui_qif_import_skip_memo_match (QIFImportWindow *wind);
void gnc_ui_qif_import_memo_doc_prepare (GncImportAssistant *assistant, gpointer user_data);
static gboolean gnc_ui_qif_import_skip_memo_doc (QIFImportWindow *wind);
void gnc_ui_qif_import_memo_rematch_cb (GtkButton *button, gpointer user_data);

void gnc_ui_qif_import_currency_prepare (GncImportAssistant *assistant, gpointer user_data);

void gnc_ui_qif_import_commodity_prepare (GncImportAssistant *assistant, gpointer user_data);
static gboolean gnc_ui_qif_import_skip_commodity (QIFImportWindow *wind);
void gnc_ui_qif_import_comm_changed_cb (GtkWidget *widget, gpointer user_data);
void gnc_ui_qif_import_comm_namespace_changed_cb (GtkWidget *widget, gpointer user_data);

void gnc_ui_qif_import_convert_progress_prepare (GncImportAssistant *assistant, gpointer user_data);
void gnc_ui_qif_import_convert_progress_pause_cb (GtkButton * button, gpointer user_data);
void gnc_ui_qif_import_convert_progress_start_cb (GtkButton * button, gpointer user_data);

void gnc_ui_qif_import_duplicates_match_prepare (GncImportAssistant *assistant, gpointer user_data);
static gboolean gnc_ui_qif_import_skip_duplicates_match (QIFImportWindow *wind);
void gnc_ui_qif_import_duplicates_doc_prepare (GncImportAssistant *assistant, gpointer user_data);
static gboolean gnc_ui_qif_import_skip_duplicates_doc (QIFImportWindow *wind);

void gnc_ui_qif_import_end_page_prepare (GncImportAssistant *assistant, gpointer user_data);

void gnc_ui_qif_import_summary_page_prepare (GncImportAssistant *assistant, gpointer user_data);

static inline void
mark_page_complete (GncImportAssistant *assistant, gboolean page_status)
{
    gint num = gnc_import_assistant_get_current_page (assistant);
    GtkWidget *page = gnc_import_assistant_get_nth_page (assistant, num);
    gnc_import_assistant_set_page_complete (assistant, page, page_status);
}

/****************************************************************
 * update_account_picker_page
 *
 * Generic function to update an account_picker page.  This
 * generalizes the code shared whenever any QIF -> GNC mapper is
 * updating its model. It asks the Scheme side to guess some account
 * translations and then shows the account name and suggested
 * translation in the Accounts page view (account picker list).
 ****************************************************************/
static void
qif_account_mapping_row_finalize (GObject *object)
{
    QIFAccountMappingRow *row = (QIFAccountMappingRow *)object;

    g_free (row->qif_name);
    g_free (row->gnc_name);
    G_OBJECT_CLASS (qif_account_mapping_row_parent_class)->finalize (object);
}

static void
qif_account_mapping_row_class_init (QIFAccountMappingRowClass *klass)
{
    G_OBJECT_CLASS (klass)->finalize = qif_account_mapping_row_finalize;
}

static void
qif_account_mapping_row_init (QIFAccountMappingRow *row)
{
    (void)row;
}

static QIFAccountMappingRow *
qif_account_mapping_row_new (gint index, const gchar *qif_name,
                             const gchar *gnc_name, gboolean is_new)
{
    QIFAccountMappingRow *row = (QIFAccountMappingRow *)g_object_new (
        qif_account_mapping_row_get_type (), NULL);

    row->index = index;
    row->qif_name = g_strdup (qif_name);
    row->gnc_name = g_strdup (gnc_name);
    row->is_new = is_new;
    return row;
}

static gint
qif_account_mapping_row_compare (gconstpointer left, gconstpointer right)
{
    const QIFAccountMappingRow *left_row = left;
    const QIFAccountMappingRow *right_row = right;

    return g_utf8_collate (left_row->qif_name, right_row->qif_name);
}

static void
update_account_picker_page (QIFAccountMappingView *mapping, SCM make_display)
{
    QIFImportWindow *wind = mapping->wind;
    SCM get_qif_name = scm_c_eval_string ("qif-map-entry:qif-name");
    SCM get_gnc_name = scm_c_eval_string ("qif-map-entry:gnc-name");
    SCM get_new = scm_c_eval_string ("qif-map-entry:new-acct?");
    SCM accts_left = scm_call_3 (make_display, wind->imported_files,
                                 *mapping->map_info, wind->gnc_acct_info);
    GList *rows = NULL;
    gint row_index = 0;
    guint selected_position = GTK_INVALID_LIST_POSITION;

    scm_gc_unprotect_object (*mapping->display_info);
    *mapping->display_info = accts_left;
    scm_gc_protect_object (*mapping->display_info);
    g_list_store_remove_all (mapping->rows);

    while (!scm_is_null (accts_left))
    {
        gchar *qif_name = gnc_scm_call_1_to_string (get_qif_name, SCM_CAR (accts_left));
        gchar *gnc_name = gnc_scm_call_1_to_string (get_gnc_name, SCM_CAR (accts_left));
        gboolean is_new = scm_call_1 (get_new, SCM_CAR (accts_left)) == SCM_BOOL_T;
        QIFAccountMappingRow *row = qif_account_mapping_row_new (
            row_index++, qif_name, gnc_name, is_new);

        rows = g_list_insert_sorted (rows, row,
                                     qif_account_mapping_row_compare);
        g_free (qif_name);
        g_free (gnc_name);
        accts_left = SCM_CDR (accts_left);
    }

    for (GList *node = rows; node; node = node->next)
    {
        QIFAccountMappingRow *row = node->data;
        guint position = g_list_model_get_n_items (G_LIST_MODEL (mapping->rows));

        if (row->index == mapping->previous_row)
            selected_position = position;
        g_list_store_append (mapping->rows, row);
        g_object_unref (row);
    }
    g_list_free (rows);

    if (selected_position == GTK_INVALID_LIST_POSITION &&
        g_list_model_get_n_items (G_LIST_MODEL (mapping->rows)) > 0)
        selected_position = 0;
    if (selected_position != GTK_INVALID_LIST_POSITION)
        gtk_selection_model_select_item (GTK_SELECTION_MODEL (mapping->selection),
                                         selected_position, TRUE);
}


/****************************************************************
 * update_account_page
 *
 * update the QIF account -> GNC Account picker
 ****************************************************************/
static void
update_account_page (QIFImportWindow * wind)
{

    SCM  make_account_display = scm_c_eval_string ("qif-dialog:make-account-display");

    update_account_picker_page (&wind->acct_mapping, make_account_display);
}


/****************************************************************
 * update_category_page
 *
 * update the QIF category -> GNC Account picker
 ****************************************************************/
static void
update_category_page (QIFImportWindow * wind)
{
    SCM  make_category_display = scm_c_eval_string ("qif-dialog:make-category-display");

    update_account_picker_page (&wind->cat_mapping, make_category_display);
}


/****************************************************************
 * update_memo_page
 *
 * update the QIF memo -> GNC Account picker
 ****************************************************************/
static void
update_memo_page (QIFImportWindow * wind)
{
    SCM  make_memo_display = scm_c_eval_string ("qif-dialog:make-memo-display");

    update_account_picker_page (&wind->memo_mapping, make_memo_display);
}


/****************************************************************
 * gnc_ui_qif_import_commodity_destroy
 *
 * This function destroys any commodity pages.
 ****************************************************************/
static void
gnc_ui_qif_import_commodity_destroy (QIFImportWindow * wind)
{
    GList               *pageptr;
    GtkWidget           *notebook_page;
    QIFCommNotebookPage *comm_nb_page;

    for (pageptr = wind->commodity_notebook_pages; pageptr; pageptr = pageptr->next)
    {
        notebook_page = pageptr->data;
        comm_nb_page  = g_object_get_data (G_OBJECT(notebook_page), "page_struct");

        /* Unprotect the Scheme hash key. */
        scm_gc_unprotect_object (comm_nb_page->hash_key);

        /* Free the memory allocated for the page's struct. */
        g_free (comm_nb_page);
    }

    /* Free the list of pages. */
    g_list_free (wind->commodity_notebook_pages);
    wind->commodity_notebook_pages = NULL;
}


/**********************************************
 * gnc_ui_qif_import_assistant_destroy
 * close the QIF Import assistant window
 **********************************************/
static void
gnc_ui_qif_import_assistant_destroy (GtkWidget *object, gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    g_object_set_data (G_OBJECT (object), "gnc-qif-import-window", NULL);

    /* Destroy the progress dialog helpers. */
    gnc_progress_dialog_destroy (wind->load_progress);

    /* Destroy any commodity pages. */
    gnc_ui_qif_import_commodity_destroy (wind);

    gnc_unregister_gui_component_by_data (ASSISTANT_QIF_IMPORT_CM_CLASS, wind);

    scm_gc_unprotect_object (wind->imported_files);
    scm_gc_unprotect_object (wind->selected_file);
    scm_gc_unprotect_object (wind->gnc_acct_info);
    scm_gc_unprotect_object (wind->cat_display_info);
    scm_gc_unprotect_object (wind->cat_map_info);
    scm_gc_unprotect_object (wind->memo_display_info);
    scm_gc_unprotect_object (wind->memo_map_info);
    scm_gc_unprotect_object (wind->acct_display_info);
    scm_gc_unprotect_object (wind->acct_map_info);
    scm_gc_unprotect_object (wind->security_hash);
    scm_gc_unprotect_object (wind->security_prefs);
    scm_gc_unprotect_object (wind->new_securities);
    scm_gc_unprotect_object (wind->ticker_map);
    scm_gc_unprotect_object (wind->imported_account_tree);
    scm_gc_unprotect_object (wind->match_transactions);

    g_clear_object (&wind->acct_mapping.selection);
    g_clear_object (&wind->acct_mapping.rows);
    g_clear_object (&wind->cat_mapping.selection);
    g_clear_object (&wind->cat_mapping.rows);
    g_clear_object (&wind->memo_mapping.selection);
    g_clear_object (&wind->memo_mapping.rows);
    g_clear_object (&wind->file_view.selection);
    g_clear_object (&wind->file_view.rows);
    g_clear_object (&wind->new_transactions.selection);
    g_clear_object (&wind->new_transactions.sorted_rows);
    g_clear_object (&wind->new_transactions.rows);
    g_clear_object (&wind->old_transactions.selection);
    g_clear_object (&wind->old_transactions.sorted_rows);
    g_clear_object (&wind->old_transactions.rows);
    g_clear_object (&wind->date_format_model);

    g_free (wind);
}


static void rematch_line (QIFAccountMappingView *mapping);

static void
qif_account_mapping_cell_setup (GtkSignalListItemFactory *factory,
                                GtkListItem *list_item, gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);

    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (list_item, label);
    (void)factory;
    (void)user_data;
}

static void
qif_account_mapping_cell_bind (GtkSignalListItemFactory *factory,
                               GtkListItem *list_item, gpointer user_data)
{
    QIFAccountMappingRow *row =
        (QIFAccountMappingRow *)gtk_list_item_get_item (list_item);
    GtkLabel *label = GTK_LABEL (gtk_list_item_get_child (list_item));
    guint column = GPOINTER_TO_UINT (user_data);

    if (!row)
        return;

    switch (column)
    {
    case 0:
        gtk_label_set_text (label, row->qif_name);
        break;
    case 1:
        gtk_label_set_text (label, row->gnc_name);
        break;
    case 2:
        gtk_label_set_text (label, row->is_new ? "✓" : "");
        gtk_label_set_xalign (label, 0.5);
        break;
    default:
        g_assert_not_reached ();
    }
    (void)factory;
}

static void
qif_account_mapping_add_column (QIFAccountMappingView *mapping,
                                const gchar *title, guint field,
                                gboolean expand)
{
    GtkListItemFactory *factory = GTK_LIST_ITEM_FACTORY (
        gtk_signal_list_item_factory_new ());
    GtkColumnViewColumn *column;

    g_signal_connect (factory, "setup",
                      G_CALLBACK (qif_account_mapping_cell_setup), NULL);
    g_signal_connect (factory, "bind",
                      G_CALLBACK (qif_account_mapping_cell_bind),
                      GUINT_TO_POINTER (field));
    column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_expand (column, expand);
    gtk_column_view_column_set_resizable (column, TRUE);
    gtk_column_view_append_column (mapping->view, column);
    g_object_unref (column);
}

static void
qif_account_mapping_selection_changed (GtkSelectionModel *selection,
                                       guint position, guint n_items,
                                       gpointer user_data)
{
    QIFAccountMappingView *mapping = user_data;
    guint count = 0;
    guint index;
    gchar *count_text;

    for (index = 0;
         index < g_list_model_get_n_items (G_LIST_MODEL (mapping->rows));
         ++index)
        if (gtk_selection_model_is_selected (selection, index))
            ++count;
    count_text = g_strdup_printf ("%u", count);
    gtk_label_set_text (GTK_LABEL (mapping->count_label), count_text);
    gtk_widget_set_sensitive (mapping->change_button, count > 0);
    g_free (count_text);
    (void)position;
    (void)n_items;
}

static void
qif_account_mapping_activated (GtkColumnView *view, guint position,
                               gpointer user_data)
{
    QIFAccountMappingView *mapping = user_data;

    gtk_selection_model_select_item (GTK_SELECTION_MODEL (mapping->selection),
                                     position, FALSE);
    rematch_line (mapping);
    (void)view;
}

static void
create_account_picker_view (QIFAccountMappingView *mapping,
                            GtkWidget *container, const gchar *col_name,
                            GtkWidget *count_label, GtkWidget *change_button,
                            QIFImportWindow *wind, SCM *map_info,
                            SCM *display_info,
                            void (*update_page)(QIFImportWindow *))
{
    mapping->container = GTK_BOX (container);
    mapping->rows = g_list_store_new (qif_account_mapping_row_get_type ());
    mapping->selection = gtk_multi_selection_new (G_LIST_MODEL (
        g_object_ref (mapping->rows)));
    mapping->view = GTK_COLUMN_VIEW (gtk_column_view_new (GTK_SELECTION_MODEL (
        g_object_ref (mapping->selection))));
    mapping->count_label = count_label;
    mapping->change_button = change_button;
    mapping->wind = wind;
    mapping->map_info = map_info;
    mapping->display_info = display_info;
    mapping->update_page = update_page;
    mapping->previous_row = -1;

    qif_account_mapping_add_column (mapping, col_name, 0, TRUE);
    qif_account_mapping_add_column (mapping, _("GnuCash account name"), 1, TRUE);
    qif_account_mapping_add_column (mapping, _("New?"), 2, FALSE);
    gtk_box_append (mapping->container, GTK_WIDGET (mapping->view));
    g_signal_connect (mapping->selection, "selection-changed",
                      G_CALLBACK (qif_account_mapping_selection_changed), mapping);
    g_signal_connect (mapping->view, "activate",
                      G_CALLBACK (qif_account_mapping_activated), mapping);
}

static void
qif_file_row_finalize (GObject *object)
{
    QIFFileRow *row = (QIFFileRow *)object;

    g_free (row->path);
    G_OBJECT_CLASS (qif_file_row_parent_class)->finalize (object);
}

static void
qif_file_row_class_init (QIFFileRowClass *klass)
{
    G_OBJECT_CLASS (klass)->finalize = qif_file_row_finalize;
}

static void
qif_file_row_init (QIFFileRow *row)
{
    (void)row;
}

static QIFFileRow *
qif_file_row_new (gint index, const gchar *path)
{
    QIFFileRow *row = (QIFFileRow *)g_object_new (qif_file_row_get_type (), NULL);

    row->index = index;
    row->path = g_strdup (path);
    return row;
}

static void
qif_file_cell_setup (GtkSignalListItemFactory *factory,
                     GtkListItem *list_item, gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);

    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_MIDDLE);
    gtk_list_item_set_child (list_item, label);
    (void)factory;
    (void)user_data;
}

static void
qif_file_cell_bind (GtkSignalListItemFactory *factory,
                    GtkListItem *list_item, gpointer user_data)
{
    QIFFileRow *row = (QIFFileRow *)gtk_list_item_get_item (list_item);

    if (row)
        gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (list_item)),
                            row->path);
    (void)factory;
    (void)user_data;
}

static void
qif_file_selection_changed (GtkSelectionModel *selection, guint position,
                            guint n_items, gpointer user_data)
{
    QIFFileView *file_view = user_data;
    QIFFileRow *row = (QIFFileRow *)gtk_single_selection_get_selected_item (
        file_view->selection);
    QIFImportWindow *wind = file_view->wind;

    scm_gc_unprotect_object (wind->selected_file);
    if (row && scm_is_list (wind->imported_files) &&
        scm_ilength (wind->imported_files) > row->index)
        wind->selected_file = scm_list_ref (wind->imported_files,
                                            scm_from_int (row->index));
    else
        wind->selected_file = SCM_BOOL_F;
    scm_gc_protect_object (wind->selected_file);
    gtk_widget_set_sensitive (wind->unload_file_btn, row != NULL);
    (void)selection;
    (void)position;
    (void)n_items;
}

static void
create_file_view (QIFFileView *file_view, GtkWidget *container,
                  QIFImportWindow *wind)
{
    GtkListItemFactory *factory = GTK_LIST_ITEM_FACTORY (
        gtk_signal_list_item_factory_new ());
    GtkColumnViewColumn *column;

    file_view->container = GTK_BOX (container);
    file_view->rows = g_list_store_new (qif_file_row_get_type ());
    file_view->selection = gtk_single_selection_new (G_LIST_MODEL (
        g_object_ref (file_view->rows)));
    gtk_single_selection_set_autoselect (file_view->selection, FALSE);
    file_view->view = GTK_COLUMN_VIEW (gtk_column_view_new (GTK_SELECTION_MODEL (
        g_object_ref (file_view->selection))));
    file_view->wind = wind;

    g_signal_connect (factory, "setup", G_CALLBACK (qif_file_cell_setup), NULL);
    g_signal_connect (factory, "bind", G_CALLBACK (qif_file_cell_bind), NULL);
    column = gtk_column_view_column_new ("", factory);
    gtk_column_view_column_set_expand (column, TRUE);
    gtk_column_view_append_column (file_view->view, column);
    g_object_unref (column);
    gtk_column_view_set_show_column_separators (file_view->view, FALSE);
    gtk_column_view_set_show_row_separators (file_view->view, FALSE);
    gtk_box_append (file_view->container, GTK_WIDGET (file_view->view));
    g_signal_connect (file_view->selection, "selection-changed",
                      G_CALLBACK (qif_file_selection_changed), file_view);
}

static void
qif_transaction_row_finalize (GObject *object)
{
    QIFTransactionRow *row = (QIFTransactionRow *)object;

    g_free (row->date);
    g_free (row->description);
    g_free (row->amount);
    G_OBJECT_CLASS (qif_transaction_row_parent_class)->finalize (object);
}

static void
qif_transaction_row_class_init (QIFTransactionRowClass *klass)
{
    G_OBJECT_CLASS (klass)->finalize = qif_transaction_row_finalize;
}

static void
qif_transaction_row_init (QIFTransactionRow *row)
{
    (void)row;
}

static QIFTransactionRow *
qif_transaction_row_new (gint index, const gchar *date, time64 date_value,
                         const gchar *description, const gchar *amount,
                         gdouble amount_value, gboolean checked)
{
    QIFTransactionRow *row = (QIFTransactionRow *)g_object_new (
        qif_transaction_row_get_type (), NULL);

    row->index = index;
    row->date = g_strdup (date);
    row->date_value = date_value;
    row->description = g_strdup (description);
    row->amount = g_strdup (amount);
    row->amount_value = amount_value;
    row->checked = checked;
    return row;
}

static void
qif_transaction_cell_setup (GtkSignalListItemFactory *factory,
                            GtkListItem *list_item, gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);

    gtk_label_set_xalign (GTK_LABEL (label),
                          GPOINTER_TO_UINT (user_data) == 3 ? 0.5 : 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (list_item, label);
    (void)factory;
}

static void
qif_transaction_cell_bind (GtkSignalListItemFactory *factory,
                           GtkListItem *list_item, gpointer user_data)
{
    QIFTransactionRow *row = (QIFTransactionRow *)gtk_list_item_get_item (
        list_item);
    GtkLabel *label = GTK_LABEL (gtk_list_item_get_child (list_item));

    if (!row)
        return;
    switch (GPOINTER_TO_UINT (user_data))
    {
    case 0:
        gtk_label_set_text (label, row->date);
        break;
    case 1:
        gtk_label_set_text (label, row->description);
        break;
    case 2:
        gtk_label_set_text (label, row->amount);
        break;
    case 3:
        gtk_label_set_text (label, row->checked ? "✓" : "");
        break;
    default:
        g_assert_not_reached ();
    }
    (void)factory;
}

static GtkOrdering
qif_transaction_row_compare (gconstpointer left, gconstpointer right,
                             gpointer user_data)
{
    const QIFTransactionRow *left_row = left;
    const QIFTransactionRow *right_row = right;
    gint result = 0;

    switch (GPOINTER_TO_UINT (user_data))
    {
    case 0:
        result = (left_row->date_value > right_row->date_value) -
                 (left_row->date_value < right_row->date_value);
        break;
    case 1:
        result = g_utf8_collate (left_row->description, right_row->description);
        break;
    case 2:
        result = (left_row->amount_value > right_row->amount_value) -
                 (left_row->amount_value < right_row->amount_value);
        break;
    default:
        g_assert_not_reached ();
    }
    return result < 0 ? GTK_ORDERING_SMALLER :
           result > 0 ? GTK_ORDERING_LARGER : GTK_ORDERING_EQUAL;
}

static GtkColumnViewColumn *
qif_transaction_view_add_column (QIFTransactionView *transaction_view,
                                 const gchar *title, guint field,
                                 gboolean expand, gboolean sortable)
{
    GtkListItemFactory *factory = GTK_LIST_ITEM_FACTORY (
        gtk_signal_list_item_factory_new ());
    GtkColumnViewColumn *column;

    g_signal_connect (factory, "setup",
                      G_CALLBACK (qif_transaction_cell_setup),
                      GUINT_TO_POINTER (field));
    g_signal_connect (factory, "bind", G_CALLBACK (qif_transaction_cell_bind),
                      GUINT_TO_POINTER (field));
    column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_expand (column, expand);
    gtk_column_view_column_set_resizable (column, TRUE);
    if (sortable)
    {
        GtkSorter *sorter = GTK_SORTER (gtk_custom_sorter_new (
            qif_transaction_row_compare, GUINT_TO_POINTER (field), NULL));

        gtk_column_view_column_set_sorter (column, sorter);
        g_object_unref (sorter);
    }
    gtk_column_view_append_column (transaction_view->view, column);
    g_object_unref (column);
    return column;
}

static void
create_transaction_view (QIFTransactionView *transaction_view,
                         GtkWidget *container, QIFImportWindow *wind,
                         gboolean show_match)
{
    GtkColumnViewColumn *date_column;

    transaction_view->container = GTK_BOX (container);
    transaction_view->rows = g_list_store_new (qif_transaction_row_get_type ());
    transaction_view->view = GTK_COLUMN_VIEW (gtk_column_view_new (NULL));
    transaction_view->sorted_rows = gtk_sort_list_model_new (G_LIST_MODEL (
        g_object_ref (transaction_view->rows)), g_object_ref (
        gtk_column_view_get_sorter (transaction_view->view)));
    transaction_view->selection = gtk_single_selection_new (G_LIST_MODEL (
        g_object_ref (transaction_view->sorted_rows)));
    gtk_single_selection_set_autoselect (transaction_view->selection, FALSE);
    gtk_column_view_set_model (transaction_view->view, GTK_SELECTION_MODEL (
        g_object_ref (transaction_view->selection)));
    transaction_view->wind = wind;

    date_column = qif_transaction_view_add_column (transaction_view, _("Date"),
                                                    0, FALSE, TRUE);
    qif_transaction_view_add_column (transaction_view, _("Description"),
                                     1, TRUE, TRUE);
    qif_transaction_view_add_column (transaction_view, _("Amount"),
                                     2, FALSE, TRUE);
    if (show_match)
        qif_transaction_view_add_column (transaction_view, _("Match?"),
                                         3, FALSE, FALSE);
    gtk_column_view_sort_by_column (transaction_view->view, date_column,
                                    GTK_SORT_ASCENDING);
    gtk_box_append (transaction_view->container,
                    GTK_WIDGET (transaction_view->view));
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
typedef struct
{
    QIFImportWindow *wind;
    GWeakRef assistant_window;
    SCM display_info;
    SCM map_info;
    GArray *rows;
    void (*update_page)(QIFImportWindow *);
} QIFRematchRequest;

static void
qif_rematch_request_free (QIFRematchRequest *request)
{
    g_weak_ref_clear (&request->assistant_window);
    scm_gc_unprotect_object (request->display_info);
    scm_gc_unprotect_object (request->map_info);
    g_array_free (request->rows, TRUE);
    g_free (request);
}

static void
qif_rematch_selected (gboolean accepted, gpointer user_data)
{
    QIFRematchRequest *request = user_data;
    GObject *owner = g_weak_ref_get (&request->assistant_window);
    GtkWidget *assistant_window = owner ? GTK_WIDGET (owner) : NULL;

    if (accepted && assistant_window)
    {
        SCM get_qif_name = scm_c_eval_string ("qif-map-entry:qif-name");
        SCM get_gnc_name = scm_c_eval_string ("qif-map-entry:gnc-name");
        SCM set_gnc_name = scm_c_eval_string ("qif-map-entry:set-gnc-name!");
        SCM gnc_name = SCM_BOOL_F;
        guint index;

        for (index = 0; index < request->rows->len; ++index)
        {
            gint row = g_array_index (request->rows, gint, index);
            SCM map_entry = scm_list_ref (request->display_info,
                                          scm_from_int (row));

            if (index == 0)
                gnc_name = scm_call_1 (get_gnc_name, map_entry);
            else
                scm_call_2 (set_gnc_name, map_entry, gnc_name);
            scm_hash_set_x (request->map_info,
                            scm_call_1 (get_qif_name, map_entry), map_entry);
        }
        request->update_page (request->wind);
    }

    g_clear_object (&owner);
    qif_rematch_request_free (request);
}

static void
rematch_line (QIFAccountMappingView *mapping)
{
    GListModel *model = G_LIST_MODEL (mapping->rows);
    GArray *rows = g_array_new (FALSE, FALSE, sizeof (gint));
    QIFRematchRequest *request;
    guint position;
    gint row = -1;
    SCM map_entry;

    for (position = 0; position < g_list_model_get_n_items (model); ++position)
    {
        QIFAccountMappingRow *mapping_row;

        if (!gtk_selection_model_is_selected (
                GTK_SELECTION_MODEL (mapping->selection), position))
            continue;
        mapping_row = (QIFAccountMappingRow *)g_list_model_get_item (model,
                                                                       position);
        row = mapping_row->index;
        g_array_append_val (rows, row);
        g_object_unref (mapping_row);
    }

    if (rows->len == 0)
    {
        g_array_free (rows, TRUE);
        return;
    }

    row = g_array_index (rows, gint, 0);
    mapping->previous_row = row;
    map_entry = scm_list_ref (*mapping->display_info, scm_from_int (row));

    request = g_new0 (QIFRematchRequest, 1);
    request->wind = mapping->wind;
    request->display_info = *mapping->display_info;
    request->map_info = *mapping->map_info;
    request->rows = rows;
    request->update_page = mapping->update_page;
    g_weak_ref_init (&request->assistant_window, mapping->wind->window);
    scm_gc_protect_object (request->display_info);
    scm_gc_protect_object (request->map_info);

    qif_account_picker_dialog_async (GTK_WINDOW (mapping->wind->window),
                                     mapping->wind, map_entry,
                                     qif_rematch_selected, request);
}


/*********************************************
 * new_security_notebook_page
 *********************************************/
static QIFCommNotebookPage *
new_security_notebook_page (SCM security_hash_key, gnc_commodity *comm, QIFImportWindow *wind)
{
    QIFCommNotebookPage *comm_nb_page = g_new0(QIFCommNotebookPage, 1);
    GtkWidget    *table;
    GtkWidget    *label;
    gchar        *title = NULL;
    const char   *str;
    GtkWidget    *notebook_page;
    GtkWidget    *notebook_label;
    GtkWidget    *entry;
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
    notebook_page = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (notebook_page), FALSE);
    comm_nb_page->notebook_page = notebook_page;
    g_object_set_data (G_OBJECT(notebook_page), "page_struct", comm_nb_page);

    /* Save the commodity and the hash table key. */
    comm_nb_page->commodity = comm;
    comm_nb_page->hash_key = security_hash_key;
    scm_gc_protect_object (comm_nb_page->hash_key);

    /* Set the page title. */
    str = gnc_commodity_get_mnemonic (comm);
    str = str ? str : "";
    title =  g_strdup_printf ("\"%s\"", str);

    /* Insert the new notebook page */
    notebook_label = gtk_label_new (title);
    gnc_label_set_alignment (notebook_label, 0.0, 0.5);
    gtk_notebook_append_page (GTK_NOTEBOOK(wind->commodity_notebook),
                              notebook_page, notebook_label);
    g_free (title);

    /* set the page complete flag as on creation all fields will be OK */
    comm_nb_page->page_complete = TRUE;

    /* Add all the widgets to the page. */
    table = gtk_grid_new ();
    gtk_grid_set_row_spacing (GTK_GRID(table), 6);
    gtk_grid_set_column_spacing (GTK_GRID(table), 12);

    /* Name entry */
    comm_nb_page->name_entry = gtk_entry_new ();
    gnc_entry_set_text (GTK_ENTRY(comm_nb_page->name_entry),
                        gnc_commodity_get_fullname (comm));
    label = gtk_label_new_with_mnemonic (_("Name or _description"));
    gtk_label_set_mnemonic_widget (GTK_LABEL(label), comm_nb_page->name_entry);
    gnc_label_set_alignment (label, 0, 0.5);

    gtk_widget_set_tooltip_text (label, name_tooltip);
    gtk_widget_set_tooltip_text (comm_nb_page->name_entry, name_tooltip);

    gtk_grid_attach (GTK_GRID(table), label, 0, 0, 1, 1);
    gtk_widget_set_halign (label, GTK_ALIGN_FILL);
    gtk_widget_set_valign (label, GTK_ALIGN_FILL);
    gtk_widget_set_hexpand (label, TRUE);
    gtk_widget_set_vexpand (label, FALSE);
    g_object_set (label, "margin", 0, NULL);

    gtk_grid_attach (GTK_GRID(table), comm_nb_page->name_entry, 1, 0, 1, 1);

    g_signal_connect (comm_nb_page->name_entry, "changed",
                      G_CALLBACK(gnc_ui_qif_import_comm_changed_cb), wind);

    /* Mnemonic entry */
    comm_nb_page->mnemonic_entry = gtk_entry_new ();
    gnc_entry_set_text (GTK_ENTRY(comm_nb_page->mnemonic_entry),
                       gnc_commodity_get_mnemonic (comm));
    label = gtk_label_new_with_mnemonic (
                _("_Ticker symbol or other abbreviation"));
    gtk_label_set_mnemonic_widget (GTK_LABEL(label), comm_nb_page->mnemonic_entry);
    gnc_label_set_alignment (label, 0, 0.5);

    gtk_widget_set_tooltip_text (label, mnemonic_tooltip);
    gtk_widget_set_tooltip_text (comm_nb_page->mnemonic_entry, mnemonic_tooltip);

    gtk_grid_attach (GTK_GRID(table), label, 0, 1, 1, 1);
    gtk_widget_set_halign (label, GTK_ALIGN_FILL);
    gtk_widget_set_valign (label, GTK_ALIGN_FILL);
    gtk_widget_set_hexpand (label, TRUE);
    gtk_widget_set_vexpand (label, FALSE);
    g_object_set (label, "margin", 0, NULL);

    gtk_grid_attach (GTK_GRID(table), comm_nb_page->mnemonic_entry, 1, 1, 1, 1);

    g_signal_connect (comm_nb_page->mnemonic_entry, "changed",
                      G_CALLBACK(gnc_ui_qif_import_comm_changed_cb), wind);

    /* Namespace entry */
    comm_nb_page->namespace_combo = gnc_ui_commodity_picker_new ();
    entry = GTK_WIDGET (gnc_ui_commodity_picker_get_entry (comm_nb_page->namespace_combo));
    g_signal_connect (G_OBJECT (entry), "changed",
                      G_CALLBACK(gnc_ui_qif_import_comm_namespace_changed_cb), wind);

    label = gtk_label_new_with_mnemonic (
                _("_Exchange or abbreviation type"));
    gtk_label_set_mnemonic_widget (GTK_LABEL(label), entry);
    gnc_label_set_alignment (label, 0, 0.5);

    gtk_widget_set_tooltip_text (label, namespace_tooltip);
    gtk_widget_set_tooltip_text (comm_nb_page->namespace_combo, namespace_tooltip);

    gtk_grid_attach (GTK_GRID(table), label, 0, 2, 1, 1);
    gtk_widget_set_halign (label, GTK_ALIGN_FILL);
    gtk_widget_set_valign (label, GTK_ALIGN_FILL);
    gtk_widget_set_hexpand (label, TRUE);
    gtk_widget_set_vexpand (label, FALSE);
    g_object_set (label, "margin", 0, NULL);

    gtk_grid_attach (GTK_GRID(table), comm_nb_page->namespace_combo, 1, 2, 1, 1);
    gnc_box_set_all_margins (GTK_BOX(notebook_page), 12);
    gtk_box_append (GTK_BOX(notebook_page), GTK_WIDGET(table));
    gtk_box_set_spacing (GTK_BOX(notebook_page), 12);
    return comm_nb_page;
}


/********************************************************************
 * prepare_security_pages
 *
 * Prepare the assistant page for each security.
 ********************************************************************/
static void
prepare_security_pages (QIFImportWindow * wind)
{
    SCM   hash_ref  = scm_c_eval_string ("hash-ref");
    SCM   securities;
    SCM   comm_ptr_token;

    GList               * current;
    gnc_commodity       * commodity;
    QIFCommNotebookPage * new_comm_nb_page;

    /*
     * Make assistant pages for each new QIF security.
     */
    gnc_set_busy_cursor (NULL, TRUE);
    securities = wind->new_securities;
    current = wind->commodity_notebook_pages;
    while (!scm_is_null (securities) && (securities != SCM_BOOL_F))
    {
        if (current)
        {
            /* The page has already been made. */
            current = current->next;
        }
        else
        {
            /* Get the GnuCash commodity corresponding to the new QIF security. */
            comm_ptr_token = scm_call_2 (hash_ref,
                                         wind->security_hash,
                                         SCM_CAR(securities));

#define FUNC_NAME "new_security_notebook_page"
            commodity = SWIG_MustGetPtr (comm_ptr_token,
                                         SWIG_TypeQuery ("_p_gnc_commodity"), 1, 0);
#undef FUNC_NAME

            /* Build a new security notebook page. */
            new_comm_nb_page = new_security_notebook_page (SCM_CAR(securities), commodity, wind);

            /* Add it to the list of security notebook pages. */
            wind->commodity_notebook_pages = g_list_append (wind->commodity_notebook_pages,
                                                            new_comm_nb_page->notebook_page);

        }
        wind->num_new_pages = wind->num_new_pages + 1;
        securities = SCM_CDR(securities);
    }
    gnc_unset_busy_cursor (NULL);
    PINFO ("Number of New Security pages is %d", wind->num_new_pages);
}


/****************************************************************
 * gnc_ui_qif_import_commodity_update
 *
 * This function updates the commodities based on the values for
 * mnemonic, namespace, and name approved by the user.
 ****************************************************************/
static void
gnc_ui_qif_import_commodity_update (QIFImportWindow * wind)
{
    GList               *pageptr;
    GtkWidget           *notebook_page;
    QIFCommNotebookPage *comm_nb_page;
    const gchar         *mnemonic = NULL;
    gchar               *name_space = NULL;
    const gchar         *fullname = NULL;
    gnc_commodity       *tab_commodity;

    for (pageptr = wind->commodity_notebook_pages; pageptr; pageptr = pageptr->next)
    {
        notebook_page = pageptr->data;
        comm_nb_page  = g_object_get_data (G_OBJECT(notebook_page), "page_struct");

        /* Get any changes from the commodity page. */
        mnemonic  = gnc_entry_get_text (GTK_ENTRY(comm_nb_page->mnemonic_entry));
        name_space = gnc_ui_namespace_picker_ns (comm_nb_page->namespace_combo);
        fullname  = gnc_entry_get_text (GTK_ENTRY(comm_nb_page->name_entry));

        /* Update the commodity with the new values. */
        gnc_commodity_set_namespace (comm_nb_page->commodity, name_space);
        gnc_commodity_set_fullname (comm_nb_page->commodity, fullname);
        gnc_commodity_set_mnemonic (comm_nb_page->commodity, mnemonic);

        /* Add the commodity to the commodity table (if it isn't a duplicate). */
        tab_commodity = gnc_commodity_table_lookup (gnc_get_current_commodities(),
                        name_space, mnemonic);
        if (!tab_commodity || tab_commodity == comm_nb_page->commodity)
            tab_commodity = gnc_commodity_table_insert (gnc_get_current_commodities(),
                                                        comm_nb_page->commodity);

        /* Update the security hash table. */
        scm_hash_set_x (wind->security_hash,
                        comm_nb_page->hash_key,
                        SWIG_NewPointerObj (tab_commodity,
                                            SWIG_TypeQuery("_p_gnc_commodity"), 0));

        g_free (name_space);
    }
}

static void
_gfec_error_handler (const char *message)
{
    PERR ("qif-import:qif-to-gnc-undo encountered an error: %s", message);
}

/****************************************************************
 * gnc_ui_qif_import_convert_undo
 *
 * This function launches the Scheme procedure that un-imports
 * any imported accounts and transactions.
 ****************************************************************/
static void
gnc_ui_qif_import_convert_undo (QIFImportWindow * wind)
{
    SCM undo = scm_c_eval_string ("qif-import:qif-to-gnc-undo");

    gnc_set_busy_cursor (NULL, TRUE);

    /* Undo the conversion. */
    if (wind->imported_account_tree != SCM_BOOL_F)
        gfec_apply (undo, scm_list_1 (wind->imported_account_tree),
                    _gfec_error_handler);

    /* There's no imported account tree any more. */
    scm_gc_unprotect_object (wind->imported_account_tree);
    wind->imported_account_tree = SCM_BOOL_F;
    scm_gc_protect_object (wind->imported_account_tree);

    /* Get rid of the list of matched transactions. */
    scm_gc_unprotect_object (wind->match_transactions);
    wind->match_transactions = SCM_BOOL_F;
    scm_gc_protect_object (wind->match_transactions);

    gnc_unset_busy_cursor (NULL);
}


/****************************************************************
 * refresh_old_transactions
 *
 * This function launches the Scheme procedure that refreshes
 * the old transactions.
 ****************************************************************/
static void
refresh_old_transactions (QIFImportWindow * wind, int selection)
{
    SCM          possible_matches;
    SCM          current_xtn;
    SCM          selected;
    Transaction  * gnc_xtn;
    Split        * gnc_split;
    const gchar  * amount_str;
    int          rownum = 0;
    QIFTransactionView *transaction_view = &wind->old_transactions;
    static GMutex mutex;
    if (!g_mutex_trylock(&mutex))
      return;

    g_list_store_remove_all (transaction_view->rows);
    g_mutex_unlock (&mutex);

    if (wind->match_transactions != SCM_BOOL_F)
    {
        possible_matches = SCM_CDR(scm_list_ref (wind->match_transactions,
                                                 scm_from_int (wind->selected_transaction)));
        scm_call_2 (scm_c_eval_string ("qif-import:refresh-match-selection"),
                    possible_matches, scm_from_int (selection));

        while (!scm_is_null (possible_matches))
        {
            gdouble amount_gd = 0;
            char datebuff [MAX_DATE_LENGTH + 1];
            memset (datebuff, 0, sizeof (datebuff));
            current_xtn = SCM_CAR(possible_matches);
#define FUNC_NAME "xaccTransCountSplits"
            gnc_xtn     = SWIG_MustGetPtr (SCM_CAR(current_xtn),
                                           SWIG_TypeQuery ("_p_Transaction"), 1, 0);
#undef FUNC_NAME
            selected    = SCM_CDR(current_xtn);

            if (xaccTransCountSplits (gnc_xtn) > 2)
            {
                amount_str = _("(split)");
            }
            else
            {
                gnc_split = xaccTransGetSplit(gnc_xtn, 0);
                amount_str =
                    xaccPrintAmount (gnc_numeric_abs (xaccSplitGetValue (gnc_split)),
                                     gnc_account_print_info
                                     (xaccSplitGetAccount (gnc_split), TRUE));
                amount_gd = gnc_numeric_to_double (xaccSplitGetValue(gnc_split));
            }

            qof_print_date_buff (datebuff, MAX_DATE_LENGTH,
                                xaccTransRetDatePosted (gnc_xtn));
            QIFTransactionRow *row = qif_transaction_row_new (
                rownum++, datebuff, xaccTransRetDatePosted (gnc_xtn),
                xaccTransGetDescription (gnc_xtn), amount_str, amount_gd,
                selected != SCM_BOOL_F);

            g_list_store_append (transaction_view->rows, row);
            g_object_unref (row);

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
gnc_ui_qif_import_duplicate_new_select_cb (GtkSelectionModel *selection,
        guint position, guint n_items, QIFImportWindow *wind)
{
    QIFTransactionRow *row = (QIFTransactionRow *)
        gtk_single_selection_get_selected_item (wind->new_transactions.selection);

    if (row)
    {
        wind->selected_transaction = row->index;
        refresh_old_transactions (wind, -1);
    }
    (void)selection;
    (void)position;
    (void)n_items;
}


/****************************************************************
 * gnc_ui_qif_import_duplicate_old_select_cb
 *
 * This function is the call back for duplicate transactions.
 ****************************************************************/
static void
gnc_ui_qif_import_duplicate_old_select_cb (GtkSelectionModel *selection,
        guint position, guint n_items, QIFImportWindow *wind)
{
    QIFTransactionRow *row = (QIFTransactionRow *)
        gtk_single_selection_get_selected_item (wind->old_transactions.selection);

    if (row)
        refresh_old_transactions (wind, row->index);
    (void)selection;
    (void)position;
    (void)n_items;
}


/********************************************************************
 * gnc_ui_qif_import_check_acct_tree
 *
 * Designed for use with gnc_main_window_foreach_page(), this
 * function determines whether an account tab is open in the main
 * window. The parameter user_data must point to a gboolean.
 ********************************************************************/
static void
gnc_ui_qif_import_check_acct_tree (GncPluginPage *page, gpointer user_data)
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
do_cancel (QIFImportWindow * wind)
{
    GList                *pageptr;
    GtkWidget            *notebook_page;
    QIFCommNotebookPage  *comm_nb_page;
    gnc_commodity_table  *table;

    gnc_set_busy_cursor (NULL, TRUE);

    /* Remove any converted data. */
    gnc_ui_qif_import_convert_undo (wind);

    /* Remove any commodities created for assistant pages. */
    for (pageptr = wind->commodity_notebook_pages; pageptr; pageptr = pageptr->next)
    {
        notebook_page = pageptr->data;
        comm_nb_page = g_object_get_data (G_OBJECT(notebook_page), "page_struct");
        gnc_commodity_destroy (comm_nb_page->commodity);
    }

    /* Remove any namespaces created by the user. */
    table = gnc_get_current_commodities ();
    while (wind->new_namespaces)
    {
        gnc_commodity_table_delete_namespace (table, (gchar *) wind->new_namespaces->data);

        /* Free the data and the list element. */
        g_free (wind->new_namespaces->data);
        wind->new_namespaces = g_list_delete_link (wind->new_namespaces,
                               wind->new_namespaces);
    }
    gnc_unset_busy_cursor (NULL);

    /* Destroy the assistant. */
    gnc_close_gui_component_by_data (ASSISTANT_QIF_IMPORT_CM_CLASS, wind);
}


/****************************************************************
 * cancel_timeout_cb
 *
 * This timer callback function waits until the busy flag
 * has been cleared before acting to cancel the import.
 ****************************************************************/
static gboolean
cancel_timeout_cb (gpointer data)
{
    QIFImportWindow *wind = data;

    if (wind->busy)
        /* Wait for timer to go off again. */
        return TRUE;

    /* The busy flag was lowered. Perform the cancel. */
    do_cancel (wind);

    /* Cancel the timer. */
    return FALSE;
}


/****************************************************************
 * gnc_ui_qif_import_cancel_cb
 *
 * Invoked when the "Cancel" button is clicked.
 ****************************************************************/
typedef struct
{
    GWeakRef window;
} QIFCancelRequest;

static void
qif_cancel_request_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    QIFCancelRequest *request = user_data;
    GtkWidget *window = g_weak_ref_get (&request->window);
    QIFImportWindow *wind = window ?
        g_object_get_data (G_OBJECT (window), "gnc-qif-import-window") : NULL;

    if (wind)
    {
        wind->cancel_pending = FALSE;
        if (response == GTK_RESPONSE_YES)
        {
            if (wind->busy)
            {
                scm_c_eval_string ("(qif-import:cancel)");
                g_timeout_add (200, cancel_timeout_cb, wind);
            }
            else
                do_cancel (wind);
        }
    }
    g_clear_object (&window);
    g_weak_ref_clear (&request->window);
    g_free (request);
    (void)parent;
}

void
gnc_ui_qif_import_cancel_cb (GncImportAssistant *gtkassistant, gpointer user_data)
{
    QIFImportWindow  *wind = user_data;
    gint currentpage = gnc_import_assistant_get_current_page (gtkassistant);
    GtkWidget *mypage = gnc_import_assistant_get_nth_page (gtkassistant, currentpage);
    const char *pagename = gtk_buildable_get_buildable_id (GTK_BUILDABLE(mypage));

    if (!g_strcmp0 (pagename, "summary_page"))
    {
        gnc_ui_qif_import_close_cb (gtkassistant, user_data);
        return;
    }
    if (wind->cancel_pending)
        return;

    QIFCancelRequest *request = g_new0 (QIFCancelRequest, 1);
    wind->cancel_pending = TRUE;
    g_weak_ref_init (&request->window, GTK_WIDGET (gtkassistant));
    gnc_verify_dialog_async (GTK_WINDOW (gtkassistant), FALSE,
                             qif_cancel_request_finished, request,
                             "%s", _("Are you sure you want to cancel?"));
}


/****************************************************************
 * gnc_ui_qif_import_close_cb
 *
 * Invoked when the "Close" button is clicked.
 ****************************************************************/
void
gnc_ui_qif_import_close_cb (GncImportAssistant *gtkassistant, gpointer user_data)
{
    QIFImportWindow  *wind = user_data;

    /* If We did not have an account tree, lets save it */
    if (!wind->acct_tree_found)
    {
        qof_book_mark_session_dirty (gnc_get_current_book ());
        gnc_ui_file_access_for_save_as (gnc_ui_get_main_window (GTK_WIDGET(gtkassistant)));
    }

    gnc_close_gui_component_by_data (ASSISTANT_QIF_IMPORT_CM_CLASS, wind);
}


/****************************************************************
 * gnc_ui_qif_import_assistant_get_mappings
 *
 * SCM get mappings.
 ****************************************************************/
SCM
gnc_ui_qif_import_assistant_get_mappings (QIFImportWindow * w)
{
    return scm_list_3 (w->acct_map_info,
                       w->cat_map_info,
                       w->memo_map_info);
}

/***************************************************************************
 *  gnc_ui_qif_import_assistant_page_forward - custom page forward function.
 *    This gives us the ability to skip pages that are not relevant.
 *    GncImportAssistant does not give us a custom back function, but
 *    it tracks pages as it runs, and you end up with effective
 *    support for the back button as well
 ***************************************************************************/
static int gnc_ui_qif_import_assistant_page_forward (int current_page, gpointer data)
{
    QIFImportWindow *wind = data;
    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT(wind->window);
    int page_count = gnc_import_assistant_get_n_pages (assistant);
    int next_page = current_page;

    for (next_page = current_page + 1; next_page < page_count; next_page++)
    {
        GtkWidget *page = gnc_import_assistant_get_nth_page (assistant, next_page);

        /* If the 'stop the presses' flag is set, move all the way to the end.
           TODO:  This does not allow for any chance to recover
                  and try a different approach.  That is the historic
                  behavior, and a moderately hard problem to solve.
                  See bug 698804
        */
        if (wind->load_stop && next_page < (page_count - 1))
            continue;

        if (!gnc_ui_qif_import_assistant_skip_page (assistant, page, wind))
            return next_page;
    }

    /* If nothing forward is visible, just don't move */
    return current_page;
}

/****************************************************************************
 *  gnc_ui_qif_import_assistant_skip_page - page specific 'skip me' functions
 *    We can write page specific functions which can determine if a
 *    given page should be skipped.  This function routes to the
 *    appropriate callback for a given page.
 ****************************************************************************/
static gboolean
gnc_ui_qif_import_assistant_skip_page (GncImportAssistant *assistant, GtkWidget *page, QIFImportWindow *wind)
{
    const char *pagename = gtk_buildable_get_buildable_id (GTK_BUILDABLE(page));
    gboolean rv = FALSE;

    ENTER("Page %s", pagename);

    if (!g_strcmp0 (pagename, "date_format_page"))
    {
        /* Current page is date page */
        rv = gnc_ui_qif_import_skip_date_format (assistant, wind);
    }
    else if (!g_strcmp0 (pagename, "account_name_page"))
    {
        /* Current page is account page */
        rv = gnc_ui_qif_import_skip_account (assistant, wind);
    }
    else if (!g_strcmp0 (pagename, "account_doc_page"))
    {
        /* Current page is  Account Doc. page */
        rv = gnc_ui_qif_import_skip_account_doc (wind);
    }
    else if (!g_strcmp0 (pagename, "category_doc_page"))
    {
        /* Current page is Category Doc. page */
        rv = gnc_ui_qif_import_skip_category_doc (wind);
    }
    else if (!g_strcmp0 (pagename, "category_match_page"))
    {
        /* Current page is Category Match page */
        rv = gnc_ui_qif_import_skip_category_match (wind);
    }
    else if (!g_strcmp0 (pagename, "memo_doc_page"))
    {
        /* Current page is Memo Doc. page */
        rv = gnc_ui_qif_import_skip_memo_doc (wind);
    }
    else if (!g_strcmp0 (pagename, "memo_match_page"))
    {
        /* Current page is Memo Match page */
        rv = gnc_ui_qif_import_skip_memo_match (wind);
    }
    else if (!g_strcmp0 (pagename, "commodity_page"))
    {
        /* Current page is Commodity page */
        rv = gnc_ui_qif_import_skip_commodity (wind);
    }
    else if (!g_strcmp0 (pagename, "duplicates_doc_page"))
    {
        /* Current page is Duplicates Doc page */
        rv = gnc_ui_qif_import_skip_duplicates_doc (wind);
    }
    else if (!g_strcmp0 (pagename, "duplicates_match_page"))
    {
        /* Current page is Duplicates Match page */
        rv = gnc_ui_qif_import_skip_duplicates_match (wind);
    }

    /* By default, we do not skip */
    LEAVE("%s", rv ? "Skipped" : "Not Skipped");
    return rv;
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
get_preferences (QIFImportWindow *wind)
{
    gchar tmp_transaction_status = 'n';

    g_return_if_fail (wind);

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
initialize_scheme (QIFImportWindow *wind)
{
    SCM  load_map_prefs;
    SCM  mapping_info;
    SCM  create_ticker_map;

    g_return_if_fail (wind);

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
    load_map_prefs = scm_c_eval_string ("qif-import:load-map-prefs");
    mapping_info = scm_call_0 (load_map_prefs); /* <- gets/creates session/book */
    wind->gnc_acct_info         = scm_list_ref (mapping_info, scm_from_int (0));
    wind->acct_map_info         = scm_list_ref (mapping_info, scm_from_int (1));
    wind->cat_map_info          = scm_list_ref (mapping_info, scm_from_int (2));
    wind->memo_map_info         = scm_list_ref (mapping_info, scm_from_int (3));
    wind->security_hash         = scm_list_ref (mapping_info, scm_from_int (4));
    wind->security_prefs        = scm_list_ref (mapping_info, scm_from_int (5));

    /* Get the initial ticker map. */
    create_ticker_map = scm_c_eval_string ("make-ticker-map");
    wind->ticker_map            = scm_call_0 (create_ticker_map);

    /* Protect our data from garbage collection. */
    scm_gc_protect_object (wind->imported_files);
    scm_gc_protect_object (wind->selected_file);
    scm_gc_protect_object (wind->gnc_acct_info);
    scm_gc_protect_object (wind->cat_display_info);
    scm_gc_protect_object (wind->cat_map_info);
    scm_gc_protect_object (wind->memo_display_info);
    scm_gc_protect_object (wind->memo_map_info);
    scm_gc_protect_object (wind->acct_display_info);
    scm_gc_protect_object (wind->acct_map_info);
    scm_gc_protect_object (wind->security_hash);
    scm_gc_protect_object (wind->security_prefs);
    scm_gc_protect_object (wind->new_securities);
    scm_gc_protect_object (wind->ticker_map);
    scm_gc_protect_object (wind->imported_account_tree);
    scm_gc_protect_object (wind->match_transactions);
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
gnc_ui_qif_import_intro_prepare (GncImportAssistant  *assistant, gpointer user_data)
{
    QIFImportWindow   *wind = user_data;
    SCM unload = scm_c_eval_string ("qif-dialog:unload-qif-file");
    SCM files_list;

    /* Set load stop to FALSE */
    wind->load_stop = FALSE;
    wind->read_file_warnings = FALSE;

    files_list = scm_call_2 (unload, wind->selected_file, wind->imported_files);

    scm_gc_unprotect_object (wind->imported_files);
    wind->imported_files = files_list;
    scm_gc_protect_object (wind->imported_files);

    scm_gc_unprotect_object (wind->selected_file);
    wind->selected_file = SCM_BOOL_F;
    scm_gc_protect_object (wind->selected_file);
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
gnc_ui_qif_import_load_file_complete (GncImportAssistant  *assistant,
                                      gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    const gchar * path_to_load;

    /* Get the file name. */
    path_to_load = gnc_entry_get_text (GTK_ENTRY(wind->filename_entry));

    /* Validate the chosen filename. */
    if (strlen (path_to_load) == 0)
        gnc_error_dialog (GTK_WINDOW(assistant), "%s", _("Please select a file to load."));
    else if (g_access (path_to_load, R_OK) < 0)
        gnc_error_dialog (GTK_WINDOW(assistant), "%s",
                          _("File not found or read permission denied. "
                            "Please select another file."));
    else
    {
        SCM qif_file_loaded = scm_c_eval_string("qif-dialog:qif-file-loaded?");

        /* See if the file is already loaded. */
        if (scm_call_2 (qif_file_loaded,
                        scm_from_locale_string (path_to_load ? path_to_load : ""),
                        wind->imported_files) == SCM_BOOL_T)
            gnc_error_dialog (GTK_WINDOW(assistant), "%s",
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

typedef struct
{
    GWeakRef window;
} QIFFileDialogData;

static void
qif_file_dialog_data_free (QIFFileDialogData *data)
{
    g_weak_ref_clear (&data->window);
    g_free (data);
}

static void
qif_file_dialog_finished (GObject *source, GAsyncResult *result,
                          gpointer user_data)
{
    QIFFileDialogData *data = user_data;
    GncFileDialogRequest *request = GNC_FILE_DIALOG_REQUEST (source);
    GError *error = NULL;
    GFile *file;
    GtkWidget *window;
    QIFImportWindow *wind = NULL;

    file = gnc_file_dialog_request_finish (request, result, &error);
    window = g_weak_ref_get (&data->window);
    if (window)
        wind = g_object_get_data (G_OBJECT (window), "gnc-qif-import-window");

    if (file)
    {
        gchar *filename = g_file_get_path (file);

        if (wind && filename)
        {
            gchar *default_dir = g_path_get_dirname (filename);
            GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT (wind->window);

            gnc_set_default_directory (GNC_PREFS_GROUP, default_dir);
            g_free (default_dir);
            gnc_entry_set_text (GTK_ENTRY (wind->filename_entry), filename);
            mark_page_complete (
                assistant,
                gnc_ui_qif_import_load_file_complete (assistant, wind));
        }
        else if (wind)
            gnc_error_dialog (GTK_WINDOW (window), "%s",
                              _("The selected file has no local path."));
        g_free (filename);
        g_object_unref (file);
    }
    else if (wind && error &&
             !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
        gnc_error_dialog (GTK_WINDOW (window), "%s", error->message);

    g_clear_error (&error);
    g_clear_object (&window);
    qif_file_dialog_data_free (data);
}


/********************************************************************
 * gnc_ui_qif_import_load_file_prepare
 *
 * Prepare the load file page for display.
 ********************************************************************/
void
gnc_ui_qif_import_load_file_prepare (GncImportAssistant *assistant, gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    const gchar * path_to_load;
    gboolean page_status = FALSE;


    /* Get the file name. */
    path_to_load = gnc_entry_get_text (GTK_ENTRY(wind->filename_entry));

    /* Calculate status for the Assistant "Next" Button */
    if (strlen (path_to_load) != 0)
    {
       page_status = gnc_ui_qif_import_load_file_complete (assistant, user_data);
    }
    mark_page_complete(assistant, page_status);
}


/********************************************************************
 * gnc_ui_qif_import_select_file_cb
 *
 * invoked when the "select file" button is clicked
 * this is just to pick a file name and reset-to-defaults all the
 * fields describing how to parse the file.
 ********************************************************************/
void
gnc_ui_qif_import_select_file_cb (GtkButton *button, gpointer user_data)
{
    QIFImportWindow *wind = user_data;
    QIFFileDialogData *data;
    GncFileDialogRequest *request;
    GtkFileFilter *filter;
    GList *filters;
    gchar *default_dir;

    filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (filter, "*.qif");
    gtk_file_filter_add_pattern (filter, "*.[Qq][Ii][Ff]");
    filters = g_list_prepend (NULL, filter);
    default_dir = gnc_get_default_directory (GNC_PREFS_GROUP);
    request = gnc_file_dialog_request_new (
        gnc_ui_get_gtk_window (GTK_WIDGET (button)), _("Select QIF File"),
        filters, default_dir, GNC_FILE_DIALOG_IMPORT);
    g_free (default_dir);

    data = g_new0 (QIFFileDialogData, 1);
    g_weak_ref_init (&data->window, wind->window);
    gnc_file_dialog_request_open_async (request, NULL,
                                        qif_file_dialog_finished, data);
    g_object_unref (request);
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
gnc_ui_qif_import_load_progress_pause_cb (GtkButton * button,
        gpointer user_data)
{
    QIFImportWindow *wind = user_data;
    SCM toggle_pause      = scm_c_eval_string ("qif-import:toggle-pause");
    SCM progress;

    if (!wind->busy)
        return;

    /* Create SCM for the progress helper. */
    progress = SWIG_NewPointerObj (wind->load_progress,
                                  SWIG_TypeQuery ("_p__GNCProgressDialog"),
                                  0);

    /* Pause (or resume) the currently running operation. */
    scm_call_1 (toggle_pause, progress);

    /* Swap the button label between pause and resume. */
    if (strcmp (gtk_button_get_label (button), _("_Resume")))
    {
        gtk_button_set_use_underline (button, TRUE);
        gtk_button_set_label (button, _("_Resume"));
    }
    else
    {
        gtk_button_set_use_underline (button, FALSE);
        gtk_button_set_label (button, _("P_ause"));
    }
}


/********************************************************************
 * gnc_ui_qif_import_load_progress_start_cb
 *
 * Invoked when the "Start" button is clicked.
 ********************************************************************/
void
gnc_ui_qif_import_load_progress_start_cb (GtkButton * button,
        gpointer user_data)
{
    QIFImportWindow   *wind = user_data;
    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT(wind->window);

    const gchar * path_to_load;

    SCM make_qif_file   = scm_c_eval_string ("make-qif-file");
    SCM qif_file_load   = scm_c_eval_string ("qif-file:read-file");
    SCM qif_file_parse  = scm_c_eval_string ("qif-file:parse-fields");
    SCM unload_qif_file = scm_c_eval_string ("qif-dialog:unload-qif-file");
    SCM parse_results   = scm_c_eval_string ("qif-file:parse-fields-results");
    SCM scm_qiffile;
    SCM imported_files = SCM_EOL;
    SCM load_return = SCM_BOOL_F, parse_return = SCM_BOOL_F;
    SCM progress;

    /* Raise the busy flag so the assistant can't be canceled unexpectedly. */
    wind->busy = TRUE;
    gtk_widget_set_sensitive (wind->load_pause, TRUE);

    /* Get the file name. */
    path_to_load = gnc_entry_get_text (GTK_ENTRY(wind->filename_entry));

    /* Create the <qif-file> object. */
    scm_qiffile          = scm_call_0 (make_qif_file);
    scm_gc_unprotect_object (wind->selected_file);
    wind->selected_file  = scm_qiffile;
    scm_gc_protect_object (wind->selected_file);
    imported_files       = scm_cons (scm_qiffile, wind->imported_files);

    /* Create SCM for the progress helper. */
    progress = SWIG_NewPointerObj (wind->load_progress,
                                   SWIG_TypeQuery ("_p__GNCProgressDialog"),
                                   0);

    /* Clear any previous pause or cancel state. */
    scm_c_eval_string ("(qif-import:reset-cancel-pause)");

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
    gnc_progress_dialog_push (wind->load_progress, 0.7);
    load_return = scm_call_4 (qif_file_load,
                              SCM_CAR(imported_files),
                              scm_from_locale_string (path_to_load ? path_to_load : ""),
                              wind->ticker_map,
                              progress);
    gnc_progress_dialog_pop (wind->load_progress);
    if (load_return == SCM_BOOL_T)
    {
        /* Canceled by the user. */

        /* Disable the pause button. */
        gtk_widget_set_sensitive (wind->load_pause, FALSE);

        /* Inform the user. */
        gnc_progress_dialog_set_sub (wind->load_progress, _("Canceled"));

        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }
    else if (load_return == SCM_BOOL_F || !scm_is_list (load_return))
    {
        /* A bug was detected. */

        /* Disable the pause button. */
        gtk_widget_set_sensitive (wind->load_pause, FALSE);

        /* Inform the user. */
        gnc_progress_dialog_append_log (wind->load_progress,
                                        _("An error occurred while loading the QIF file."));
        gnc_progress_dialog_set_sub (wind->load_progress, _("Failed"));
        gnc_progress_dialog_reset_value (wind->load_progress);
        gnc_error_dialog (GTK_WINDOW(assistant), "%s",
                          _("An error occurred while loading the QIF file."));
        /* FIXME: How should we request that the user report this problem? */

        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }
    else if (!scm_is_null (load_return))
    {
        if (SCM_CAR(load_return) == SCM_BOOL_F)
        {
            imported_files = scm_call_2 (unload_qif_file, scm_qiffile, imported_files);
            scm_gc_unprotect_object (wind->imported_files);
            wind->imported_files = imported_files;
            scm_gc_protect_object (wind->imported_files);

            gnc_progress_dialog_set_sub (wind->load_progress, _("Failed"));
            gnc_progress_dialog_reset_value (wind->load_progress);

            gtk_widget_set_sensitive (wind->load_pause, FALSE);
            wind->busy = FALSE;
            wind->load_stop = TRUE;
        }
        else
            wind->read_file_warnings = TRUE;
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
    if (!wind->load_stop)
    {
    gnc_progress_dialog_push (wind->load_progress, 1);
    parse_return = scm_call_2 (qif_file_parse, SCM_CAR(imported_files),
                   progress);
    gnc_progress_dialog_pop (wind->load_progress);
    wind->ask_date_format = FALSE;
    wind->date_format = NULL;
    }
    if (parse_return == SCM_BOOL_T)
    {
        /* Canceled by the user. */

        /* Disable the pause button. */
        gtk_widget_set_sensitive (wind->load_pause, FALSE);

        /* Unload the file. */
        gnc_progress_dialog_set_sub (wind->load_progress, _("Cleaning up"));
        imported_files = scm_call_2 (unload_qif_file, scm_qiffile, imported_files);

        /* Inform the user. */
        gnc_progress_dialog_set_sub (wind->load_progress, _("Canceled"));

        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }
    else if (parse_return == SCM_BOOL_F || !scm_is_list(parse_return))
    {
        /* A bug was detected. */

        /* Disable the pause button. */
        gtk_widget_set_sensitive (wind->load_pause, FALSE);

        /* Unload the file. */
        gnc_progress_dialog_set_sub (wind->load_progress, _("Cleaning up"));
        imported_files = scm_call_2 (unload_qif_file, scm_qiffile, imported_files);

        /* Inform the user. */
        gnc_progress_dialog_append_log (wind->load_progress,
                                        _("A bug was detected while parsing the QIF file."));
        gnc_progress_dialog_set_sub (wind->load_progress, _("Failed"));
        gnc_progress_dialog_reset_value (wind->load_progress);
        gnc_error_dialog (GTK_WINDOW(assistant), "%s",
                          _("A bug was detected while parsing the QIF file."));
        /* FIXME: How should we request that the user report this problem? */

        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }
    else if (!scm_is_null (parse_return))
    {
        /* Are there only warnings? */
        if (SCM_CAR(parse_return) == SCM_BOOL_T)
        {
            SCM date_formats;

            /* A warning means that (potentially) the date format is
             * ambiguous.  So search the results for the "date" type and if
             * it's found, set up the format selector page. */
            if ((date_formats = scm_call_2 (parse_results,
                                            SCM_CDR(parse_return),
                                            scm_from_locale_symbol ("date"))) != SCM_BOOL_F)
            {
                /* Block the date call back */
                g_signal_handlers_block_by_func (wind->date_format_dropdown,
                                                 gnc_ui_qif_import_date_valid_cb,
                                                 wind);

                /* Clear the date format selection model. */
                gtk_string_list_splice (wind->date_format_model, 0,
                                        g_list_model_get_n_items (G_LIST_MODEL (
                                            wind->date_format_model)), NULL);
                gtk_drop_down_set_selected (wind->date_format_dropdown,
                                            GTK_INVALID_LIST_POSITION);

                /* Add the formats for the user to select from. */
                while (scm_is_list (date_formats) && !scm_is_null (date_formats))
                {
                    gchar *format = gnc_scm_symbol_to_locale_string (
                        SCM_CAR (date_formats));

                    gtk_string_list_append (wind->date_format_model, format);
                    g_free (format);

                    date_formats = SCM_CDR(date_formats);
                }

                /* Unblock the date call back */
                g_signal_handlers_unblock_by_func (wind->date_format_dropdown,
                                                   gnc_ui_qif_import_date_valid_cb,
                                                   wind);

                wind->ask_date_format = TRUE;
            }
            wind->load_stop = TRUE;
        }
        else
        {
            /* Parsing failed. */
            imported_files = scm_call_2 (unload_qif_file, scm_qiffile, imported_files);
            gnc_progress_dialog_set_sub (wind->load_progress, _("Failed"));
            gnc_progress_dialog_reset_value (wind->load_progress);

            gtk_widget_set_sensitive (wind->load_pause, FALSE);
            wind->busy = FALSE;
            wind->load_stop = TRUE;
        }
    }

    /* Enable the assistant "Next" button */
    mark_page_complete (assistant, TRUE);

    /* Set Pause and Start buttons */
    gtk_widget_set_sensitive (wind->load_pause, FALSE);
    gtk_widget_set_sensitive (wind->load_start, FALSE);

    /* The file was loaded successfully. */
    gnc_progress_dialog_set_sub (wind->load_progress, _("Loading completed"));
    gnc_progress_dialog_set_value (wind->load_progress, 1);

    scm_gc_unprotect_object (wind->imported_files);
    wind->imported_files = imported_files;
    scm_gc_protect_object (wind->imported_files);

    gtk_widget_set_sensitive (wind->load_pause, FALSE);
    wind->busy = FALSE;


    if (wind->load_stop == FALSE && wind->read_file_warnings == FALSE)
    {
        /* Auto step to next page */
        gnc_import_assistant_next_page (assistant);
    }
    wind->load_stop = FALSE;
}


/********************************************************************
 * gnc_ui_qif_import_load_progress_prepare
 *
 * Prepare the file loading progress page for display.
 ********************************************************************/
void
gnc_ui_qif_import_load_progress_prepare (GncImportAssistant  *assistant, gpointer user_data)
{
    QIFImportWindow   *wind = user_data;

    /* Reset the progress display. */
    gnc_progress_dialog_set_primary (wind->load_progress, "");
    gnc_progress_dialog_set_secondary (wind->load_progress,
                                       _("When you press the Start Button, GnuCash will load your QIF file. If there are no errors or warnings, you will automatically proceed to the next step. Otherwise, the details will be shown below for your review."));
    gnc_progress_dialog_set_sub (wind->load_progress, " ");
    gnc_progress_dialog_reset_value (wind->load_progress);
    gnc_progress_dialog_reset_log (wind->load_progress);

    /* Set Pause and Start buttons */
    gtk_widget_set_sensitive (wind->load_pause, FALSE);
    gtk_widget_set_sensitive (wind->load_start, TRUE);

    /* Disable the assistant "Next" button */
    mark_page_complete (assistant, FALSE);
}


/*****************************************
 * Page 3 - Date format Page Procedures
 ****************************************/

/********************************************************************
 * gnc_ui_qif_import_skip_date_format
 *
 * Determine if we need the date page
 ********************************************************************/
static gboolean
gnc_ui_qif_import_skip_date_format (GncImportAssistant *assistant, QIFImportWindow *wind)
{
    return ! wind->ask_date_format;
}


/********************************************************************
 * gnc_ui_qif_import_date_valid_cb
 *
 * Reparse file with new date format.
 ********************************************************************/
static void
qif_import_reparse_dates (QIFImportWindow* wind)
{
    SCM  reparse_dates   = scm_c_eval_string ("qif-file:reparse-dates");
    SCM format_sym = scm_from_locale_symbol (wind->date_format);

    /* Reparse the dates using the selected format. */
    scm_call_2 (reparse_dates, wind->selected_file, format_sym);
    g_free (wind->date_format);
    wind->date_format = NULL;
    wind->ask_date_format = FALSE;
}

void
gnc_ui_qif_import_date_valid_cb (GtkDropDown *dropdown, GParamSpec *pspec,
                                 gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    GtkStringObject *item;
    guint selected = gtk_drop_down_get_selected (dropdown);

    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT(wind->window);

    /* Get the selected date format. */
    if (selected == GTK_INVALID_LIST_POSITION)
        return;
    item = GTK_STRING_OBJECT (g_list_model_get_item (gtk_drop_down_get_model (
        dropdown), selected));
    wind->date_format = item ? g_strdup (gtk_string_object_get_string (item)) : NULL;
    g_clear_object (&item);

    if (!wind->date_format)
    {
        g_critical ("QIF import: BUG DETECTED in gnc_ui_qif_import_date_valid_cb. Format is NULL.");
        return;
    }

    qif_import_reparse_dates (wind);

    mark_page_complete (assistant, TRUE);
    (void)pspec;
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
gnc_ui_qif_import_account_prepare (GncImportAssistant  *assistant, gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    SCM  check_from_acct = scm_c_eval_string ("qif-file:check-from-acct");

    /* make sure there is a file selected, may have come back */
    if (wind->selected_file == SCM_BOOL_F)
    {
        GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT(wind->window);
        gnc_entry_set_text (GTK_ENTRY(wind->filename_entry), "");
        gnc_import_assistant_set_current_page (assistant, 1);
    }
    else
    {
        /* Determine the next page to display. */
        if (scm_call_1 (check_from_acct, wind->selected_file) != SCM_BOOL_T)
        {
            /* There is an account name missing. Ask the user to provide one. */
            SCM default_acct = scm_c_eval_string ("qif-file:path-to-accountname");
            gchar * default_acctname = NULL;

            default_acctname = gnc_scm_call_1_to_string (default_acct, wind->selected_file);
            gnc_entry_set_text (GTK_ENTRY(wind->acct_entry), default_acctname);
            g_free (default_acctname);
        }
    }
}

/********************************************************************
 * gnc_ui_qif_import_skip_account
 *
 * Determine if we need the import account page
 ********************************************************************/
static gboolean
gnc_ui_qif_import_skip_account (GncImportAssistant *assistant, QIFImportWindow *wind)
{
    SCM  check_from_acct = scm_c_eval_string ("qif-file:check-from-acct");
    if (wind->selected_file != SCM_BOOL_F &&
        scm_call_1 (check_from_acct, wind->selected_file) == SCM_BOOL_T)
        return TRUE;
    return FALSE;
}

/********************************************************************
 * gnc_ui_qif_import_acct_enter_cb
 *
 * Invoked when the "enter" button is clicked on the acct entry.
 ********************************************************************/
void
gnc_ui_qif_import_acct_enter_cb (GtkWidget * widget,
                                 gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT(wind->window);

    const gchar * acct_name = gnc_entry_get_text (GTK_ENTRY(wind->acct_entry));

    if (!acct_name || acct_name[0] == 0)
    {
        /* Disable the assistant "Next" Button */
        mark_page_complete (assistant, FALSE);
    }
    else
    {
        /* Enable the assistant "Next" Button and proceed */
        mark_page_complete (assistant, TRUE);

        /* Move on to the next page automatically */
        gnc_import_assistant_next_page (assistant);
    }
}


/********************************************************************
 * gnc_ui_qif_import_acct_valid_cb
 *
 * Change signal for the acct entry to enable "Next" button.
 ********************************************************************/
void
gnc_ui_qif_import_acct_valid_cb (GtkWidget * widget,
                                 gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT(wind->window);

    const gchar * acct_name = gnc_entry_get_text (GTK_ENTRY(wind->acct_entry));

    if (!acct_name || acct_name[0] == 0)
    {
        /* Disable the assistant "Next" Button */
        mark_page_complete (assistant, FALSE);
    }
    else
    {
        /* Enable the assistant "Next" Button */
        mark_page_complete (assistant, TRUE);
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
gnc_ui_qif_import_loaded_files_prepare (GncImportAssistant *assistant,
                                        gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    const gchar * acct_name = gnc_entry_get_text (GTK_ENTRY(wind->acct_entry));
    SCM    fix_default = scm_c_eval_string ("qif-import:fix-from-acct");
    SCM    scm_name;

    if (wind->selected_file != SCM_BOOL_F)
    {
        scm_name = scm_from_utf8_string (acct_name ? acct_name : "");
        scm_call_2 (fix_default, wind->selected_file, scm_name);

        /* Enable the assistant "Next" Button */
        mark_page_complete (assistant, TRUE);
    }

    update_file_page (wind);
}


/********************************************************************
 * gnc_ui_qif_import_load_another_cb
 * Invoked when the "load another" button is clicked on the loaded
 * files page.
 ********************************************************************/
void
gnc_ui_qif_import_load_another_cb (GtkButton * button,
                                   gpointer user_data)
{
    QIFImportWindow * wind = user_data;
    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT(wind->window);

    gnc_entry_set_text (GTK_ENTRY(wind->filename_entry), "");

    gnc_import_assistant_set_current_page (assistant, 1);
}


/********************************************************************
 * gnc_ui_qif_import_unload_cb
 * Invoked when the "unload" button is clicked on the loaded files
 * page.
 ********************************************************************/
void
gnc_ui_qif_import_unload_file_cb (GtkButton * button,
                                  gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    SCM unload_qif_file = scm_c_eval_string ("qif-dialog:unload-qif-file");
    SCM imported_files;

    if (wind->selected_file != SCM_BOOL_F)
    {
        imported_files =
            scm_call_2 (unload_qif_file, wind->selected_file, wind->imported_files);

        scm_gc_unprotect_object (wind->imported_files);
        wind->imported_files = imported_files;
        scm_gc_protect_object (wind->imported_files);

        scm_gc_unprotect_object (wind->selected_file);
        wind->selected_file = SCM_BOOL_F;
        scm_gc_protect_object (wind->selected_file);

        update_file_page (wind);
    }
}


/********************************************************************
 * update_file_page
 *
 * Update the list of loaded files.
 ********************************************************************/
static void
update_file_page (QIFImportWindow * wind)
{
    SCM       loaded_file_list = wind->imported_files;
    SCM       qif_file_path;
    int       row = 0;
    QIFFileView *file_view = &wind->file_view;
    guint selected_position = GTK_INVALID_LIST_POSITION;

    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT(wind->window);
    gint num_of_files = 0;

    /* Clear the list before repopulating it from the Scheme file list. */
    g_list_store_remove_all (file_view->rows);
    qif_file_path = scm_c_eval_string ("qif-file:path");

    mark_page_complete (assistant, FALSE);

    while (!scm_is_null (loaded_file_list))
    {
        gchar *row_text    = NULL;
        SCM    scm_qiffile = SCM_BOOL_F;

        scm_qiffile = SCM_CAR(loaded_file_list);
        row_text = gnc_scm_call_1_to_string (qif_file_path, scm_qiffile);

        QIFFileRow *file_row = qif_file_row_new (row, row_text);

        g_list_store_append (file_view->rows, file_row);
        g_object_unref (file_row);
        g_free (row_text);

        if (scm_qiffile == wind->selected_file)
            selected_position = row;
        ++row;
        loaded_file_list = SCM_CDR(loaded_file_list);
    }

    if (selected_position != GTK_INVALID_LIST_POSITION)
        gtk_single_selection_set_selected (file_view->selection,
                                           selected_position);

    /* get the number of files in the list */
    num_of_files = g_list_model_get_n_items (G_LIST_MODEL (file_view->rows));

    if (num_of_files > 0)
        mark_page_complete (assistant, TRUE);
    else
    {
        /*  TODO: It would be ideal to disable the back button at this point
            until all files have been unloaded.  However, GncImportAssistant does
            not provide a way to do that.

            The back button works at this point, but results in mildly
            confusing behavior - you get an error on the select page,
            and you are forced to load another file; you can't just skip
            forward and back.  Fixing that may be possible; changing the
            load page to more intelligently handle the case where the selected
            file is already loaded should work.  But that will be fiddly,
            as you likely want to force an already loaded file to be reloaded
            as we come forward.  The current muddle 'feels' bad, but gives
            a user a fairly clear understanding of what is happening, and
            so I am choosing to prefer it.
        */
    }

}


/**********************************************
 * Page 6 - Account Doc. Page Procedures
 **********************************************/

/********************************************************************
 * gnc_ui_qif_import_account_doc_prepare
 ********************************************************************/
void
gnc_ui_qif_import_account_doc_prepare (GncImportAssistant *assistant,
                                       gpointer user_data)
{
    /* Enable the Assistant "Next" Button */
    mark_page_complete (assistant, TRUE);
}

/********************************************************************
 * gnc_ui_qif_import_skip_account_doc
 *
 * Determine if we need the import account doc page
 ********************************************************************/
static gboolean
gnc_ui_qif_import_skip_account_doc (QIFImportWindow *wind)
{
    return !wind->show_doc_pages;
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
gnc_ui_qif_import_account_match_prepare (GncImportAssistant *assistant,
                                         gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    /* Prepare the matching pages. */
    gnc_set_busy_cursor (NULL, TRUE);
    update_account_page (wind);
    update_category_page (wind);
    update_memo_page (wind);
    gnc_unset_busy_cursor (NULL);

    /* Enable the Assistant "Next" Button */
    mark_page_complete (assistant, TRUE);
}


/****************************************************************
 * gnc_ui_qif_import_account_rematch_cb
 *
 * This handler is invoked when the user clicks the "Change
 * GnuCash account" button on the account mapping page. This
 * button is an alternative to double-clicking a row.
 ****************************************************************/
void
gnc_ui_qif_import_account_rematch_cb (GtkButton *button, gpointer user_data)
{
    QIFImportWindow  *wind = user_data;

    g_return_if_fail (wind);

    rematch_line (&wind->acct_mapping);
}


/*******************************************
 * Page 8 - Category Doc. Page Procedures
 *******************************************/

/********************************************************************
 * gnc_ui_qif_import_category_doc_prepare
 ********************************************************************/
void
gnc_ui_qif_import_category_doc_prepare (GncImportAssistant *assistant,
                                        gpointer user_data)
{
    /* Enable the Assistant "Next" Button */
    mark_page_complete (assistant, TRUE);
}

/********************************************************************
 * gnc_ui_qif_import_skip_category_doc
 *
 * Determine if we need the import category doc page
 ********************************************************************/
static gboolean
gnc_ui_qif_import_skip_category_doc (QIFImportWindow *wind)
{
    /* Jump over doc page if show_doc_pages FALSE */
    if (!wind->show_doc_pages)
        return TRUE;

    /* If there are no category mappings, jump the doc page. */
    if (scm_is_list (wind->cat_display_info) && scm_is_null (wind->cat_display_info))
        return TRUE;

    return FALSE;
}


/******************************************
 * Page 9 - Category Match Page Procedures
 ******************************************/

/****************************************************************
 * gnc_ui_qif_import_category_match_prepare
 *
 * Find the next page to show, depending on whether there are
 * category or payee/memo mappings to be dealt with.
 ****************************************************************/
void
gnc_ui_qif_import_category_match_prepare (GncImportAssistant *assistant,
        gpointer user_data)
{
    /* Enable the Assistant "Next" Button */
    mark_page_complete (assistant, TRUE);
}

/********************************************************************
 * gnc_ui_qif_import_skip_category_match
 *
 * Determine if we need the import category match page
 ********************************************************************/
static gboolean
gnc_ui_qif_import_skip_category_match (QIFImportWindow *wind)
{
    /* If there are no category mappings, jump this step. */
    if (scm_is_list (wind->cat_display_info) && scm_is_null (wind->cat_display_info))
        return TRUE;

    return FALSE;
}


/****************************************************************
 * gnc_ui_qif_import_category_rematch_cb
 *
 * This handler is invoked when the user clicks the "Change
 * GnuCash account" button on the category mapping page. This
 * button is an alternative to double-clicking a row.
 ****************************************************************/
void
gnc_ui_qif_import_category_rematch_cb (GtkButton *button, gpointer user_data)
{
    QIFImportWindow  *wind = user_data;

    g_return_if_fail (wind);

    rematch_line (&wind->cat_mapping);
}


/****************************************
 * Page 10 - Memo Doc. Page Procedures
 ****************************************/

/********************************************************************
 * gnc_ui_qif_import_memo_doc_prepare
 ********************************************************************/
void
gnc_ui_qif_import_memo_doc_prepare (GncImportAssistant *assistant, gpointer user_data)
{
    /* Enable the Assistant "Next" Button */
    mark_page_complete (assistant, TRUE);
}

/********************************************************************
 * gnc_ui_qif_import_skip_memo_doc
 *
 * Determine if we need the import memo doc page
 ********************************************************************/
static gboolean
gnc_ui_qif_import_skip_memo_doc (QIFImportWindow *wind)
{
    /* Jump over doc page if show_doc_pages FALSE */
    if (!wind->show_doc_pages)
        return TRUE;

    /* If there are no memo mappings, jump the doc page. */
    if (scm_is_list (wind->memo_display_info) && scm_is_null (wind->memo_display_info))
        return TRUE;

    return FALSE;
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
gnc_ui_qif_import_memo_match_prepare (GncImportAssistant *assistant, gpointer user_data)
{
    /* Enable the Assistant "Next" Button */
    mark_page_complete (assistant, TRUE);
}

/********************************************************************
 * gnc_ui_qif_import_skip_memo_match
 *
 * Determine if we need the import memo match page
 ********************************************************************/
static gboolean
gnc_ui_qif_import_skip_memo_match (QIFImportWindow *wind)
{
    /* If there are no memo mappings, jump this step. */
    if (scm_is_list (wind->memo_display_info) && scm_is_null (wind->memo_display_info))
        return TRUE;

    return FALSE;
}


/****************************************************************
 * gnc_ui_qif_import_memo_rematch_cb
 *
 * This handler is invoked when the user clicks the "Change
 * GnuCash account" button on the memo mapping page. This
 * button is an alternative to double-clicking a row.
 ****************************************************************/
void
gnc_ui_qif_import_memo_rematch_cb (GtkButton *button, gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    g_return_if_fail (wind);

    rematch_line (&wind->memo_mapping);
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
gnc_ui_qif_import_currency_prepare (GncImportAssistant *assistant, gpointer user_data)
{
    gint num = gnc_import_assistant_get_current_page (assistant);
    GtkWidget *page = gnc_import_assistant_get_nth_page (assistant, num);
    QIFImportWindow  *wind = user_data;

    g_return_if_fail (wind);

    /* Only display Book Option data if new book */
    if (wind->new_book)
    {
        gnc_import_assistant_set_page_title (assistant, page,
                                      _("Choose the QIF file currency and select Book Options"));
        gtk_widget_set_visible (GTK_WIDGET(wind->book_option_label), TRUE);
        gtk_widget_set_visible (GTK_WIDGET(wind->book_option_message), TRUE);
    }
    else
    {
        gnc_import_assistant_set_page_title (assistant, page,
                                      _("Choose the QIF file currency"));
        gtk_widget_set_visible (GTK_WIDGET(wind->book_option_label), FALSE);
        gtk_widget_set_visible (GTK_WIDGET(wind->book_option_message), FALSE);
    }

    /* Enable the Assistant "Next" Button */
    mark_page_complete (assistant, TRUE);
}


/**************************************************
 * Page 13 - Commodity Page Procedures
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
gnc_ui_qif_import_new_securities (QIFImportWindow * wind)
{
    SCM updates;
    SCM update_securities = scm_c_eval_string ("qif-import:update-security-hash");

    /* Get a list of any new QIF securities since the previous call. */
    updates = scm_call_4 (update_securities,
                          wind->security_hash,
                          wind->ticker_map,
                          wind->acct_map_info,
                          wind->security_prefs);
    if (updates != SCM_BOOL_F)
    {
        /* A list of new QIF securities was returned. Save it. */
        scm_gc_unprotect_object (wind->new_securities);
        if (wind->new_securities != SCM_BOOL_F)
            /* There is an existing list, so append the new list. */
            wind->new_securities = scm_append (scm_list_2 (wind->new_securities,
                                               updates));
        else
            wind->new_securities = updates;
        scm_gc_protect_object (wind->new_securities);

        return TRUE;
    }

    if (wind->new_securities != SCM_BOOL_F)
        return TRUE;

    return FALSE;
}


/********************************************************************
 * gnc_ui_qif_import_commodity_notebook_update_combos
 *
 * Scans all commodity notebook pages to make sure all the name space
 * combos are in sync
 ********************************************************************/
static void
gnc_ui_qif_import_commodity_notebook_update_combos (QIFImportWindow * wind, gboolean init_combos)
{
    GList               *pageptr;
    GtkWidget           *notebook_page;
    QIFCommNotebookPage *comm_nb_page;

    for (pageptr = wind->commodity_notebook_pages; pageptr; pageptr = pageptr->next)
    {
        notebook_page = pageptr->data;
        comm_nb_page = g_object_get_data (G_OBJECT(notebook_page), "page_struct");

        /* Get any entered namespace. */
        gchar *ns = gnc_ui_namespace_picker_ns (comm_nb_page->namespace_combo);

        /* Update the namespaces available to select. */
        if (!ns || !ns[0])
        {
            gnc_ui_update_namespace_picker (
                comm_nb_page->namespace_combo,
                gnc_commodity_get_namespace (comm_nb_page->commodity),
                DIAG_COMM_ALL);

            if(!init_combos)
                gnc_entry_set_text (gnc_ui_commodity_picker_get_entry (
                                        comm_nb_page->namespace_combo), "");
        }
        else
            gnc_ui_update_namespace_picker (comm_nb_page->namespace_combo, ns, DIAG_COMM_ALL);
        g_free (ns);
    }
}


/********************************************************************
 * gnc_ui_qif_import_commodity_all_notebook_pages_complete
 *
 * Scans all commodity notebook pages for the page_complete flag and
 * return TRUE if they are all complete
 ********************************************************************/
static gboolean
gnc_ui_qif_import_commodity_all_notebook_pages_complete (QIFImportWindow * wind)
{
    GList               *pageptr;
    GtkWidget           *notebook_page;
    QIFCommNotebookPage *comm_nb_page;
    gboolean             pages_complete = TRUE;

    for (pageptr = wind->commodity_notebook_pages; pageptr; pageptr = pageptr->next)
    {
        notebook_page = pageptr->data;
        comm_nb_page  = g_object_get_data (G_OBJECT(notebook_page), "page_struct");

        if (!comm_nb_page->page_complete)
            pages_complete = FALSE;
    }
    return pages_complete;
}


/********************************************************************
+ * gnc_ui_qif_import_commodity_prepare
 ********************************************************************/
void
gnc_ui_qif_import_commodity_prepare (GncImportAssistant *assistant, gpointer user_data)
{
    QIFImportWindow *wind = user_data;

    /* Enable the Assistant "Next" Button */
    mark_page_complete (assistant,
                        gnc_ui_qif_import_commodity_all_notebook_pages_complete (wind));

    /* If there are new securities, prepare the security pages. */
    if (wind->new_securities != SCM_BOOL_F)
    {
        wind->timeout_id = 0;

        /* add the commodity notebook pages */
        prepare_security_pages (wind);

        /* make sure all the namespace combos are in sync */
        gnc_ui_qif_import_commodity_notebook_update_combos (wind, TRUE);
    }
}

/********************************************************************
 * gnc_ui_qif_import_skip_commodity
 *
 * Determine if we need the import commodity page
 ********************************************************************/
static gboolean
gnc_ui_qif_import_skip_commodity (QIFImportWindow *wind)
{
    return !gnc_ui_qif_import_new_securities (wind);
}



/*********************************
 * gnc_ui_qif_import_comm_valid
 ********************************/
static gboolean
gnc_ui_qif_import_comm_valid (GncImportAssistant *assistant, gpointer user_data)
{
    QIFImportWindow *    wind = user_data;
    gint                  num = gtk_notebook_get_current_page (GTK_NOTEBOOK(wind->commodity_notebook));
    GtkWidget * notebook_page = gtk_notebook_get_nth_page (GTK_NOTEBOOK(wind->commodity_notebook), num);
    QIFCommNotebookPage * comm_nb_page = g_object_get_data (G_OBJECT(notebook_page), "page_struct");

    QofBook                 *book;
    gnc_commodity_table     *table;
    gnc_commodity_namespace *newns;

    gchar       *name_space = gnc_ui_namespace_picker_ns (comm_nb_page->namespace_combo);
    const gchar *name       = gnc_entry_get_text (GTK_ENTRY(comm_nb_page->name_entry));
    const gchar *mnemonic   = gnc_entry_get_text (GTK_ENTRY(comm_nb_page->mnemonic_entry));

    /* set the page complete flag to TRUE to start with */
    comm_nb_page->page_complete = TRUE;

    if (!name || (name[0] == 0))
    {
        comm_nb_page->page_complete = FALSE;
        g_free (name_space);
        return FALSE;
    }
    else if (!mnemonic || (mnemonic[0] == 0))
    {
        comm_nb_page->page_complete = FALSE;
        g_free (name_space);
        return FALSE;
    }
    else if (!name_space || (name_space[0] == 0))
    {
        comm_nb_page->page_complete = FALSE;
        if (name_space)
            g_free (name_space);
        return FALSE;
    }

    /* FIXME: Should check whether a commodity with this namespace and
     *        mnemonic already exists. If so, ask the user whether to use
     *        the existing one, or go back and change what they've entered.
     */

    book = gnc_get_current_book ();
    table = gnc_commodity_table_get_table (book);
    if (gnc_commodity_namespace_is_iso (name_space) &&
            !gnc_commodity_table_lookup (table, name_space, mnemonic))
    {
        gnc_warning_dialog (GTK_WINDOW(assistant), "%s",
                            _("You must enter an existing national "
                              "currency or enter a different type."));

        comm_nb_page->page_complete = FALSE;
        g_free (name_space);
        return FALSE;
    }

    /* Is the namespace a new one? */
    if (!gnc_commodity_table_has_namespace (table, name_space))
    {
        /* Register it so that it will appear as an option on other pages. */
        newns = gnc_commodity_table_add_namespace (table, name_space, book);

        /* Remember it so it can be removed if the import gets canceled. */
        if (newns)
            wind->new_namespaces = g_list_prepend (wind->new_namespaces, name_space);
        else
        {
            g_warning ("QIF import: Couldn't create namespace %s", name_space);
            g_free (name_space);
        }
    }
    else
        g_free (name_space);

    /* make sure all the namespace combos are in sync */
    gnc_ui_qif_import_commodity_notebook_update_combos (wind, FALSE);

    return gnc_ui_qif_import_commodity_all_notebook_pages_complete (wind);
}


/*************************************
 * gnc_ui_qif_import_comm_changed_cb
 ************************************/
void
gnc_ui_qif_import_comm_changed_cb (GtkWidget *widget, gpointer user_data)
{
    QIFImportWindow *wind = user_data;
    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT(wind->window);

    mark_page_complete (assistant,
                        gnc_ui_qif_import_comm_valid (assistant, user_data));
}


static gboolean
do_page_check (gpointer user_data)
{
    QIFImportWindow *wind = user_data;
    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT(wind->window);

    mark_page_complete (assistant,
                        gnc_ui_qif_import_comm_valid (assistant, wind));

    wind->timeout_id = 0;
    return FALSE;
}


/**********************************************
 * gnc_ui_qif_import_comm_namespace_changed_cb
 **********************************************/
void
gnc_ui_qif_import_comm_namespace_changed_cb (GtkWidget *widget, gpointer user_data)
{
    QIFImportWindow *wind = user_data;

    if (wind->timeout_id)
        g_source_remove (wind->timeout_id);

    /* delay the page check while typing in the combo entry, this should
     * reduce the number of false entries as new name spaces are typed, its
     * not really a problem as name spaces created but not used do not get imported. */
    wind->timeout_id = g_timeout_add (500, (GSourceFunc)do_page_check, wind);
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
gnc_ui_qif_import_convert_progress_pause_cb (GtkButton * button,
        gpointer user_data)
{
    QIFImportWindow *wind = user_data;
    SCM toggle_pause      = scm_c_eval_string ("qif-import:toggle-pause");
    SCM progress;

    if (!wind->busy)
        return;

    /* Create SCM for the progress helper. */
    progress = SWIG_NewPointerObj (wind->convert_progress,
                                   SWIG_TypeQuery ("_p__GNCProgressDialog"),
                                   0);

    /* Pause (or resume) the currently running operation. */
    scm_call_1 (toggle_pause, progress);

    /* Swap the button label between pause and resume. */
    if (strcmp (gtk_button_get_label (button), _("_Resume")))
    {
        gtk_button_set_use_underline (button, TRUE);
        gtk_button_set_label (button, _("_Resume"));
    }
    else
    {
        gtk_button_set_use_underline (button, FALSE);
        gtk_button_set_label (button, _("P_ause"));
    }
}


/********************************************************************
 * gnc_ui_qif_import_convert_progress_start_cb
 *
 * Invoked when the "Start" button is clicked.
 ********************************************************************/
void
gnc_ui_qif_import_convert_progress_start_cb (GtkButton * button,
        gpointer user_data)
{
    QIFImportWindow   *wind = user_data;
    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT(wind->window);

    SCM qif_to_gnc      = scm_c_eval_string ("qif-import:qif-to-gnc");
    SCM find_duplicates = scm_c_eval_string ("gnc:account-tree-find-duplicates");
    SCM retval;

    /* SCM for the progress dialog. */
    SCM progress = SWIG_NewPointerObj (wind->convert_progress,
                                       SWIG_TypeQuery ("_p__GNCProgressDialog"),
                                       0);

    /* The default currency. */
    gnc_commodity *currency = gnc_currency_edit_get_currency (
        GNC_CURRENCY_EDIT (wind->currency_picker));
    const gchar *currname = gnc_commodity_get_printname (currency);

    /* Raise the busy flag so the assistant can't be canceled unexpectedly. */
    wind->busy = TRUE;
    gtk_widget_set_sensitive (wind->convert_pause, TRUE);
    gtk_widget_set_sensitive (wind->convert_start, FALSE);

    /* Clear any previous pause or cancel state. */
    scm_c_eval_string ("(qif-import:reset-cancel-pause)");

    /* Update the commodities. */
    gnc_ui_qif_import_commodity_update (wind);

    /*
     * Convert the QIF data into GnuCash data.
     *
     * A Scheme function does all the work.  The return value is the
     * root account of an account tree containing all the new accounts
     * and transactions. Upon failure, #f is returned. If the user
     * cancels, #t is returned.
     */

    /* This step will fill 70% of the bar. */
    gnc_progress_dialog_push (wind->convert_progress, 0.7);
    retval = scm_apply (qif_to_gnc,
                        scm_list_n (wind->imported_files,
                                    wind->acct_map_info,
                                    wind->cat_map_info,
                                    wind->memo_map_info,
                                    wind->security_hash,
                                    scm_from_utf8_string (currname ? currname : ""),
                                    wind->transaction_status,
                                    progress,
                                    SCM_UNDEFINED),
                        SCM_EOL);
    gnc_progress_dialog_pop (wind->convert_progress);

    if (retval == SCM_BOOL_T)
    {
        /* Canceled by the user. */

        /* Disable the pause button. */
        gtk_widget_set_sensitive (wind->convert_pause, FALSE);

        /* Remove any converted data. */
        gnc_progress_dialog_set_sub (wind->convert_progress, _("Cleaning up"));
        gnc_ui_qif_import_convert_undo (wind);

        /* Inform the user. */
        gnc_progress_dialog_set_sub (wind->convert_progress, _("Canceled"));
        gnc_progress_dialog_reset_value (wind->convert_progress);

        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }
    else if (retval == SCM_BOOL_F)
    {
        /* An bug was encountered during conversion. */

        /* Disable the pause button. */
        gtk_widget_set_sensitive (wind->convert_pause, FALSE);

        /* Remove any converted data. */
        gnc_progress_dialog_set_sub (wind->convert_progress, _("Cleaning up"));
        gnc_ui_qif_import_convert_undo (wind);

        /* Inform the user. */
        gnc_progress_dialog_append_log (wind->convert_progress,
                                        _("A bug was detected while converting the QIF data."));
        gnc_progress_dialog_set_sub (wind->convert_progress, _("Failed"));
        gnc_progress_dialog_reset_value (wind->convert_progress);
        gnc_error_dialog (GTK_WINDOW(assistant), "%s",
                          _("A bug was detected while converting the QIF data."));
        /* FIXME: How should we request that the user report this problem? */

        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }
    else if (scm_is_symbol (retval))
    {
        /* An error was encountered during conversion. */

        /* Disable the pause button. */
        gtk_widget_set_sensitive (wind->convert_pause, FALSE);

        /* Remove any converted data. */
        gnc_progress_dialog_set_sub (wind->convert_progress, _("Cleaning up"));
        gnc_ui_qif_import_convert_undo (wind);

        /* Inform the user. */
        gnc_progress_dialog_set_sub (wind->convert_progress, _("Failed"));
        gnc_progress_dialog_reset_value (wind->convert_progress);

        wind->busy = FALSE;
        wind->load_stop = TRUE;
    }
    if (wind->load_stop == FALSE)
    {
        /* Save the imported account tree. */
        scm_gc_unprotect_object (wind->imported_account_tree);
        wind->imported_account_tree = retval;
        scm_gc_protect_object (wind->imported_account_tree);

        /*
         * Detect potentially duplicated transactions.
         */

        /* This step will fill the remainder of the bar. */
        gnc_progress_dialog_push (wind->convert_progress, 1);
        retval = scm_call_3 (find_duplicates,
                             scm_c_eval_string ("(gnc-get-current-root-account)"),
                             wind->imported_account_tree, progress);
        gnc_progress_dialog_pop (wind->convert_progress);

        /* Save the results. */
        scm_gc_unprotect_object (wind->match_transactions);
        wind->match_transactions = retval;
        scm_gc_protect_object (wind->match_transactions);

        if (retval == SCM_BOOL_T)
        {
            /* Canceled by the user. */
            gtk_widget_set_sensitive (wind->convert_pause, FALSE);
            gnc_progress_dialog_set_sub (wind->convert_progress, _("Canceling"));
            wind->busy = FALSE;
            wind->load_stop = TRUE;
        }
        else if (retval == SCM_BOOL_F)
        {
            /* An error occurred during duplicate checking. */

            /* Remove any converted data. */
            gnc_progress_dialog_set_sub (wind->convert_progress, _("Cleaning up"));
            gnc_ui_qif_import_convert_undo (wind);

            /* Inform the user. */
            gnc_progress_dialog_append_log (wind->convert_progress,
                                            _("A bug was detected while detecting duplicates."));
            gnc_progress_dialog_set_sub (wind->convert_progress, _("Failed"));
            gnc_progress_dialog_reset_value (wind->convert_progress);
            gnc_error_dialog (GTK_WINDOW(assistant), "%s",
                              _("A bug was detected while detecting duplicates."));
            /* FIXME: How should we request that the user report this problem? */

            gtk_widget_set_sensitive (wind->convert_pause, FALSE);
            wind->busy = FALSE;
            wind->load_stop = TRUE;
        }
    }
    /* Enable the Assistant "Next" Button */
    mark_page_complete (assistant, TRUE);

    /* Set Pause and Start buttons */
    gtk_widget_set_sensitive (wind->convert_pause, FALSE);
    gtk_widget_set_sensitive (wind->convert_start, FALSE);

    if (wind->load_stop == FALSE)
    {
        /* The conversion completed successfully. */
        gnc_progress_dialog_set_sub (wind->convert_progress,
                                     _("Conversion completed"));
        gnc_progress_dialog_set_value (wind->convert_progress, 1);

        gtk_widget_set_sensitive (wind->convert_pause, FALSE);
        wind->busy = FALSE;

        /* If the log is empty, move on to the next page automatically. */
        if (gtk_text_buffer_get_char_count (gtk_text_view_get_buffer (GTK_TEXT_VIEW(wind->convert_log))) == 0) {
            gnc_import_assistant_next_page (assistant);
        }
    }
}


/********************************************************************
 * gnc_ui_qif_import_convert_progress_prepare
 *
 * Prepare the data conversion progress page for display.
 ********************************************************************/
typedef struct
{
    GWeakRef window;
    QofBook *book;
} QIFNewBookRequest;

static void
qif_new_book_options_finished (GtkWindow *parent, gboolean applied,
                               gpointer user_data)
{
    QIFNewBookRequest *request = user_data;
    GtkWidget *window = g_weak_ref_get (&request->window);
    QIFImportWindow *wind = window ?
        g_object_get_data (G_OBJECT (window), "gnc-qif-import-window") : NULL;

    if (wind)
    {
        wind->new_book_options_pending = FALSE;
        if (applied && request->book == gnc_get_current_book () &&
            !qof_book_shutting_down (request->book))
        {
            wind->new_book = FALSE;
            gnc_ui_qif_import_convert_progress_prepare (
                GNC_IMPORT_ASSISTANT (window), wind);
        }
    }
    g_clear_object (&window);
    g_weak_ref_clear (&request->window);
    g_free (request);
    (void)parent;
}

void
gnc_ui_qif_import_convert_progress_prepare (GncImportAssistant *assistant,
        gpointer user_data)
{
    QIFImportWindow   *wind = user_data;

    /* Reset the progress display. */
    gnc_progress_dialog_set_primary (wind->convert_progress, "");
    gnc_progress_dialog_set_secondary (wind->convert_progress,
            _("When you press the Start Button, GnuCash will import your QIF data. "
              "If there are no errors or warnings, you will automatically proceed to "
              "the next step. Otherwise, the details will be shown below for your review."));
    gnc_progress_dialog_set_sub (wind->convert_progress, " ");
    gnc_progress_dialog_reset_value (wind->convert_progress);
    gnc_progress_dialog_reset_log (wind->convert_progress);

    /* Set Pause and Start buttons */
    gtk_widget_set_sensitive (wind->convert_pause, FALSE);
    gtk_widget_set_sensitive (wind->convert_start, TRUE);

    /* Conversion remains unavailable until New Book Options applies. */
    mark_page_complete (assistant, FALSE);
    if (wind->new_book && !wind->new_book_options_pending)
    {
        QIFNewBookRequest *request = g_new0 (QIFNewBookRequest, 1);
        wind->new_book_options_pending = TRUE;
        g_weak_ref_init (&request->window, GTK_WIDGET (assistant));
        request->book = gnc_get_current_book ();
        gnc_new_book_option_display_async (GTK_WIDGET (assistant),
                                           qif_new_book_options_finished,
                                           request);
    }
}


/*****************************************
 * Page 15 - Match Doc. Page Procedures
 *****************************************/

/********************************************************************
 * gnc_ui_qif_import_duplicates_doc_prepare
 ********************************************************************/
void
gnc_ui_qif_import_duplicates_doc_prepare (GncImportAssistant *assistant,
        gpointer user_data)
{
    /* Enable the Assistant "Next" Button */
    mark_page_complete (assistant, TRUE);

}

/********************************************************************
 * gnc_ui_qif_import_skip_duplicates_doc
 *
 * Determine if we need the import duplicates doc page
 ********************************************************************/
static gboolean
gnc_ui_qif_import_skip_duplicates_doc (QIFImportWindow *wind)
{
    /* Jump over doc page if show_doc_pages FALSE */
    if (!wind->show_doc_pages)
        return TRUE;

    /* Don't show doc page if there are no duplicates */
    if (scm_is_null (wind->match_transactions))
        return TRUE;

    return FALSE;
}

/**********************************************
 * Page 16 - Match Duplicates Page Procedures
 **********************************************/

/********************************************************************
 * gnc_ui_qif_import_duplicates_match_prepare
 ********************************************************************/
void
gnc_ui_qif_import_duplicates_match_prepare (GncImportAssistant *assistant,
        gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    SCM               duplicates;
    SCM               current_xtn;
    Transaction      *gnc_xtn;
    Split            *gnc_split;
    const gchar      *amount_str;
    int               rownum = 0;

    if (!scm_is_null (wind->match_transactions))
    {
        g_list_store_remove_all (wind->new_transactions.rows);

        if (!scm_is_list (wind->match_transactions))
            return;

        /* Loop through the list of new, potentially duplicate transactions. */
        duplicates = wind->match_transactions;
        while (!scm_is_null (duplicates))
        {
            gdouble amount_gd = 0;
            time64 send_time = 0;
            char datebuff [MAX_DATE_LENGTH + 1];
            memset (datebuff, 0, MAX_DATE_LENGTH);
            current_xtn = SCM_CAAR(duplicates);
#define FUNC_NAME "xaccTransCountSplits"
            gnc_xtn = SWIG_MustGetPtr (current_xtn,
                                       SWIG_TypeQuery ("_p_Transaction"), 1, 0);
#undef FUNC_NAME
            if (xaccTransCountSplits (gnc_xtn) > 2)
                amount_str = _("(split)");
            else
            {
                gnc_split = xaccTransGetSplit (gnc_xtn, 0);
                amount_str =
                    xaccPrintAmount (gnc_numeric_abs (xaccSplitGetValue (gnc_split)),
                                     gnc_account_print_info
                                     (xaccSplitGetAccount (gnc_split), TRUE));
                amount_gd = gnc_numeric_to_double (xaccSplitGetValue(gnc_split));
            }
            send_time = xaccTransRetDatePosted (gnc_xtn);
            qof_print_date_buff (datebuff, MAX_DATE_LENGTH, send_time);
            QIFTransactionRow *row = qif_transaction_row_new (
                rownum++, datebuff, send_time, xaccTransGetDescription (gnc_xtn),
                amount_str, amount_gd, FALSE);

            g_list_store_append (wind->new_transactions.rows, row);
            g_object_unref (row);

            duplicates = SCM_CDR(duplicates);
        }
        if (g_list_model_get_n_items (G_LIST_MODEL (wind->new_transactions.rows)) > 0)
            gtk_single_selection_set_selected (wind->new_transactions.selection, 0);
    }

    /* Enable the Assistant "Next" Button */
    mark_page_complete (assistant, TRUE);
}

/********************************************************************
 * gnc_ui_qif_import_skip_duplicates_match
 *
 * Determine if we need the import duplicates match page
 ********************************************************************/
static gboolean
gnc_ui_qif_import_skip_duplicates_match (QIFImportWindow *wind)
{
    /* Don't show page if there are no duplicates */
    return scm_is_null (wind->match_transactions);
}


/*************************************
 * Page 17 - Apply Page Procedures
 *************************************/

/********************************************************************
 * gnc_ui_qif_import_end_page_prepare
 ********************************************************************/
void
gnc_ui_qif_import_end_page_prepare (GncImportAssistant *assistant,
                                    gpointer user_data)
{
    /* Enable the Assistant "Next" Button */
    mark_page_complete (assistant, TRUE);
}


/********************************************************************
 * gnc_ui_qif_import_finish_cb
 *
 * Invoked when the "Apply" button is clicked on the final page.
 ********************************************************************/
void
gnc_ui_qif_import_finish_cb (GncImportAssistant *assistant,
                             gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    SCM save_map_prefs = scm_c_eval_string ("qif-import:save-map-prefs");
    SCM cat_and_merge = scm_c_eval_string ("gnc:account-tree-catenate-and-merge");
    SCM prune_xtns = scm_c_eval_string ("gnc:prune-matching-transactions");
    SCM scm_result;

    GncPluginPage *page;
    gboolean acct_tree_found = FALSE;

    gnc_suspend_gui_refresh ();

    /* Prune any imported transactions that were determined to be duplicates. */
    if (wind->match_transactions != SCM_BOOL_F)
        scm_call_1 (prune_xtns, wind->match_transactions);

    /* Merge the imported account tree with the existing one. */
    if (wind->imported_account_tree != SCM_BOOL_F)
        scm_call_2 (cat_and_merge,
                   scm_c_eval_string ("(gnc-get-current-root-account)"),
                   wind->imported_account_tree);

    /* Set the currency of the root account, if this is a new user import it may be null */
    if (!xaccAccountGetCommodity (gnc_get_current_root_account ()))
    {
        Account *root = gnc_get_current_root_account ();
        xaccAccountBeginEdit (root);
        xaccAccountSetCommodity (root, gnc_currency_edit_get_currency
                                       (GNC_CURRENCY_EDIT(wind->currency_picker)));
        xaccAccountCommitEdit (root);
    }

    gnc_resume_gui_refresh ();

    /* Save the user's mapping preferences. */
    scm_result = scm_apply (save_map_prefs,
                            scm_list_5 (wind->acct_map_info, wind->cat_map_info,
                                        wind->memo_map_info, wind->security_hash,
                                        wind->security_prefs),
                            SCM_EOL);

    if (scm_result == SCM_BOOL_F)
        gnc_warning_dialog (GTK_WINDOW(assistant), "%s",
                            _("GnuCash was unable to save your mapping preferences."));

    /* Open an account tab in the main window if one doesn't exist already. */
    gnc_main_window_foreach_page (gnc_ui_qif_import_check_acct_tree,
                                  &acct_tree_found);

    wind->acct_tree_found = acct_tree_found;
    if (!acct_tree_found)
    {
        page = gnc_plugin_page_account_tree_new ();
        gnc_main_window_open_page (NULL, page);
    }
    gnc_import_assistant_set_current_page (assistant, 18);
}


/***************************************
 * Page 18 - Summary Page Procedures
 ***************************************/

/********************************************************************
 * gnc_ui_qif_import_summary_page_prepare
 ********************************************************************/
void
gnc_ui_qif_import_summary_page_prepare (GncImportAssistant *assistant,
                                        gpointer user_data)
{
    QIFImportWindow * wind = user_data;

    const gchar *msg = wind->load_stop ?
        _("There was a problem with the import.") :
        _("QIF Import Completed.");

    gchar *text = g_markup_printf_escaped ("<span size=\"large\"><b>%s</b></span>", msg);

    gtk_label_set_markup (GTK_LABEL(wind->summary_text), text);

    g_free (text);

    /* Enable the Assistant "Next" Button */
    mark_page_complete (assistant, TRUE);
}


/********************************************************************
 * Prepare callback for assistant pages.
 ********************************************************************/
void gnc_ui_qif_import_prepare_cb (GncImportAssistant  *assistant, GtkWidget *page,
                                   gpointer user_data)
{
    gint currentpage = gnc_import_assistant_get_current_page (assistant);
    GtkWidget *mypage = gnc_import_assistant_get_nth_page (assistant, currentpage);
    const char *pagename = gtk_buildable_get_buildable_id (GTK_BUILDABLE(mypage));

    ENTER("Page %s", pagename);

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
        /* No preparation required */
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
        /* Current page is Category Doc. page */
        gnc_ui_qif_import_category_doc_prepare (assistant, user_data);
    }
    else if (!g_strcmp0 (pagename, "category_match_page"))
    {
        /* Current page is Category Match page */
        gnc_ui_qif_import_category_match_prepare (assistant, user_data);
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
    else if (!g_strcmp0 (pagename, "commodity_page"))
    {
        /* Current page is Commodity page */
        gnc_ui_qif_import_commodity_prepare (assistant, user_data);
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
    LEAVE("");
}


/********************************************************************
 * get_assistant_widgets
 *
 * Get all builder-defined widgets that need to be actively managed.
 ********************************************************************/
static void
get_assistant_widgets (QIFImportWindow *wind, GtkBuilder *builder)
{
    g_return_if_fail (wind);
    g_return_if_fail (builder);

    wind->window             = GTK_WIDGET(gtk_builder_get_object (builder, "qif_import_assistant"));
    wind->filename_entry     = GTK_WIDGET(gtk_builder_get_object (builder, "qif_filename_entry"));
    wind->load_pause         = GTK_WIDGET(gtk_builder_get_object (builder, "load_progress_pause"));
    wind->load_start         = GTK_WIDGET(gtk_builder_get_object (builder, "load_progress_start"));
    wind->load_log           = GTK_WIDGET(gtk_builder_get_object (builder, "load_progress_log"));
    wind->load_progress      = gnc_progress_dialog_custom (
                                   GTK_LABEL(gtk_builder_get_object (builder, "load_progress_primary")),
                                   GTK_LABEL(gtk_builder_get_object (builder, "load_progress_secondary")),
                                   GTK_PROGRESS_BAR(gtk_builder_get_object (builder, "load_progress_bar")),
                                   GTK_LABEL(gtk_builder_get_object (builder, "load_progress_sub")),
                                   GTK_TEXT_VIEW(wind->load_log));
    wind->acct_entry         = GTK_WIDGET(gtk_builder_get_object (builder, "qif_account_entry"));
    wind->date_format_dropdown = GTK_DROP_DOWN(gtk_builder_get_object (
        builder, "date_format_dropdown"));
    wind->date_format_model = GTK_STRING_LIST(gtk_builder_get_object (
        builder, "date_format_model"));
    wind->selected_file_view = GTK_WIDGET(gtk_builder_get_object (builder, "selected_file_view"));
    wind->unload_file_btn    = GTK_WIDGET(gtk_builder_get_object (builder, "unload_file_button"));
    wind->book_option_label  = GTK_WIDGET(gtk_builder_get_object (builder, "book_option_label"));
    wind->book_option_message = GTK_WIDGET(gtk_builder_get_object (builder, "book_option_message_label"));
    wind->commodity_notebook = GTK_WIDGET(gtk_builder_get_object (builder, "commodity_notebook"));
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
    wind->convert_progress   = gnc_progress_dialog_custom (
                                   GTK_LABEL(gtk_builder_get_object (builder, "convert_progress_primary")),
                                   GTK_LABEL(gtk_builder_get_object (builder, "convert_progress_secondary")),
                                   GTK_PROGRESS_BAR(gtk_builder_get_object (builder, "convert_progress_bar")),
                                   GTK_LABEL(gtk_builder_get_object (builder, "convert_progress_sub")),
                                   GTK_TEXT_VIEW(wind->convert_log));
    wind->summary_text       = GTK_WIDGET(gtk_builder_get_object (builder, "summary_page"));

    // Set the name for this assistant so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(wind->window), "gnc-id-assistant-qif-import");
    gnc_widget_style_context_add_class (GTK_WIDGET(wind->window), "gnc-class-imports");

    wind->new_transaction_view =
        GTK_WIDGET(gtk_builder_get_object (builder, "new_transaction_view"));
    wind->old_transaction_view =
        GTK_WIDGET(gtk_builder_get_object (builder, "old_transaction_view"));
}


/********************************************************************
 * build_views
 *
 * Build the data views used by the assistant.
 ********************************************************************/
static void
build_views (QIFImportWindow *wind)
{
    g_return_if_fail (wind);

    /* Set up the selected file view */
    create_file_view (&wind->file_view, wind->selected_file_view, wind);

    /* Set up the QIF account to GnuCash account matcher. */
    create_account_picker_view (&wind->acct_mapping, wind->acct_view,
                                _("QIF account name"), wind->acct_view_count,
                                wind->acct_view_btn, wind, &wind->acct_map_info,
                                &wind->acct_display_info, update_account_page);

    /* Set up the QIF category to GnuCash account matcher. */
    create_account_picker_view (&wind->cat_mapping, wind->cat_view,
                                _("QIF category name"), wind->cat_view_count,
                                wind->cat_view_btn, wind, &wind->cat_map_info,
                                &wind->cat_display_info, update_category_page);

    /* Set up the QIF payee/memo to GnuCash account matcher. */
    create_account_picker_view (&wind->memo_mapping, wind->memo_view,
                                _("QIF payee/memo"), wind->memo_view_count,
                                wind->memo_view_btn, wind, &wind->memo_map_info,
                                &wind->memo_display_info, update_memo_page);

    /* Set up the new and old transaction views with the same sortable model
     * contract as the legacy trees. */
    create_transaction_view (&wind->new_transactions, wind->new_transaction_view,
                             wind, FALSE);
    create_transaction_view (&wind->old_transactions, wind->old_transaction_view,
                             wind, TRUE);
    g_signal_connect (wind->new_transactions.selection, "selection-changed",
                      G_CALLBACK (gnc_ui_qif_import_duplicate_new_select_cb), wind);
    g_signal_connect (wind->old_transactions.selection, "selection-changed",
                      G_CALLBACK (gnc_ui_qif_import_duplicate_old_select_cb), wind);
}


/********************************************************************
 * gnc_ui_qif_import_assistant_make
 *
 * Build a new QIF import assistant.
 ********************************************************************/
static GtkWidget *
gnc_ui_qif_import_assistant_make (QIFImportWindow *qif_win)
{
    GtkBuilder        *builder;
    GtkWidget         *box;

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "assistant-qif-import.glade", "date_format_model");
    gnc_builder_add_from_file (builder, "assistant-qif-import.glade", "qif_import_assistant");

    qif_win->new_namespaces       = NULL;
    qif_win->selected_transaction = 0;
    qif_win->busy                 = FALSE;
    /* In order to include a book options display on the creation of a new book,
     * we need to detect when we are dealing with a new book. */
    qif_win->new_book = gnc_is_new_book ();

    /* Get all user preferences related to QIF importing. */
    get_preferences (qif_win);

    /* Set up the Scheme side of things. Note that if a session/book did not
     * exist prior to this function, it is created within scheme function
     * "qif-import:load-map-prefs", so we need to have set the flag previously */
    initialize_scheme (qif_win);

    /* Get all interesting builder-defined widgets. */
    get_assistant_widgets (qif_win, builder);
    GncImportAssistant *assistant = gnc_import_assistant_new (
        GTK_WINDOW (qif_win->window),
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

    /* Make this window stay on top */
    gtk_window_set_transient_for (GTK_WINDOW(qif_win->window), gnc_ui_get_main_window (NULL));

    /* Build the data views used by the assistant. */
    build_views (qif_win);
    PINFO ("Total Number of Assistant Pages is %d", gnc_import_assistant_get_n_pages (assistant));

    /* Establish the custom next-page and history-aware back navigation. */
    gnc_import_assistant_set_forward_page_func (assistant,
                                                gnc_ui_qif_import_assistant_page_forward,
                                                qif_win, NULL);

    /* Currency Page */
    /* Set a default currency for new accounts */
    qif_win->currency_picker = gnc_currency_edit_new ();
    gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(qif_win->currency_picker), gnc_default_currency ());
    gtk_widget_set_visible (GTK_WIDGET(qif_win->currency_picker), TRUE);
    box = GTK_WIDGET(gtk_builder_get_object (builder, "currency_picker_hbox"));
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(qif_win->currency_picker));

    gnc_restore_window_size (GNC_PREFS_GROUP,
                             GTK_WINDOW(qif_win->window), gnc_ui_get_main_window (NULL));

    g_object_set_data (G_OBJECT (qif_win->window), "gnc-qif-import-window",
                       qif_win);
    g_signal_connect (qif_win->window, "destroy",
                      G_CALLBACK(gnc_ui_qif_import_assistant_destroy), qif_win);

gnc_builder_connect_signals (builder, qif_win);
    gnc_import_assistant_set_page_action (assistant, 17,
                                          GNC_IMPORT_ASSISTANT_PAGE_APPLY);
    gnc_import_assistant_set_page_action (assistant, 18,
                                          GNC_IMPORT_ASSISTANT_PAGE_CLOSE);
    gnc_import_assistant_set_callbacks (assistant, gnc_ui_qif_import_prepare_cb,
                                        gnc_ui_qif_import_finish_cb,
                                        gnc_ui_qif_import_cancel_cb,
                                        gnc_ui_qif_import_close_cb, qif_win);

    g_object_unref (G_OBJECT(builder));

    gtk_window_present (GTK_WINDOW(qif_win->window));

    return qif_win->window;
}


/********************************************
 * gnc_ui_qif_import_assistant_close_handler
 ********************************************/
static void
gnc_ui_qif_import_assistant_close_handler (gpointer user_data)
{
    QIFImportWindow *qif_win = user_data;

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(qif_win->window));
    gtk_window_destroy (GTK_WINDOW(qif_win->window));
}


/********************************************
 * gnc_file_qif_import
 ********************************************/
void
gnc_file_qif_import (void)
{
    QIFImportWindow *qif_win;
    gint component_id;
    SCM  has_regex = scm_c_eval_string ("(defined? 'make-regexp)");

    if (scm_is_false(has_regex) == 1)
    {
        gnc_warning_dialog(NULL, _("QIF import requires guile with regex support."));
        return;
    }

    qif_win = g_new0 (QIFImportWindow, 1);

    /* pop up the QIF File Import dialog box */
    gnc_ui_qif_import_assistant_make (qif_win);

    component_id = gnc_register_gui_component (ASSISTANT_QIF_IMPORT_CM_CLASS,
                   NULL, gnc_ui_qif_import_assistant_close_handler,
                   qif_win);

    gnc_gui_component_watch_entity_type (component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);


    gnc_window_adjust_for_screen (GTK_WINDOW(qif_win->window));
}
