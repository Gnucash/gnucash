/*******************************************************************\
 * assistant-csv-trans-import.c -- An assistant for importing       *
 *                                     Transactions from a file.    *
 *                                                                  *
 * Copyright (C) 2012 Robert Fewell                                 *
 * Copyright (c) 2007 Benny Sperisen <lasindi@gmail.com>            *
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
/** @file assistant-csv-trans-import.cpp
    @brief CSV Import Assistant
    @author Copyright (c) 2012 Robert Fewell
    @author Copyright (c) 2016 Geert Janssens
*/

#include <guid.hpp>

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <stdexcept>
#include <stdlib.h>
#include <cstdint>

#include "gnc-path.h"
#include "gnc-ui.h"
#include "gnc-uri.hpp"
#include "gnc-ui-util.h"
#include "gnc-file.h"
#include "dialog-utils.h"
#include "gnc-gtk-utils.h"

#include "gnc-component-manager.h"

#include "gnc-state.h"

#include "assistant-csv-trans-import.h"
#include "gnc-csv-preview-refresh.hpp"
#include "gnc-import-assistant.h"

#include "import-account-matcher.h"
#include "import-main-matcher.h"
#include "import-backend.h"
#include "gnc-account-sel.h"

#include "go-charmap-sel.h"

#include "gnc-imp-settings-csv-tx.hpp"
#include "gnc-import-tx.hpp"
#include "gnc-tokenizer-fw.hpp"
#include "gnc-tokenizer-csv.hpp"

#include <algorithm>
#include <exception>
#include <iostream>
#include <memory>
#include <numeric>
#include <string>
#include <tuple>
#include <vector>

#include <gnc-locale-utils.hpp>
#include <boost/locale.hpp>

namespace bl = boost::locale;

#define MIN_COL_WIDTH 70
#define GNC_PREFS_GROUP "dialogs.import.csv"
#define ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS "assistant-csv-trans-import"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

enum GncImportColumn {
    MAPPING_STRING,
    MAPPING_FULLPATH,
    MAPPING_ACCOUNT
};

/* A note on memory management
 *
 * This source file is mixture of several completely different memory models
 * - it defines a c++ class which is managed the c++ way
 * - the c++ class encapsulates a gtk based assistant which is managed according to
 *   the GObject/Gtk memory model.
 * - gnucash manages gui objects via its "Component Manager". Dialogs and windows are
 *   registered with this component manager when they are opened and the component
 *   manager handles the lifecycle of toplevel widgets. When a dialog is closed
 *   the component manager invokes a close handler which is responsible for cleaning up.
 *
 * Care must be taken in places where these models intersect. Here is how it is
 * handled for this source file:
 * - First in the context of the import assistant the gnucash component manager is
 *   merely a wrapper to keep track of which gui objects exist so they can be cleanly
 *   destroyed. But the component manager itself doesn't do any memory management
 *   on the objects it tracks. Instead it delegates this back to the objects themselves
 *   via callbacks which the objects have to supply. It merely helps in the coordination.
 *   So we can further ignore it in the memory management discussion.
 *
 * - Next there is only one entry point to start this assistant: gnc_file_csv_trans_import
 * - The full assistant functionality is wrapped in a C++ class using RAII.
 * - The entry point function will create one instance of this class using the c++
 *   "new" method. This in turn will create several objects like a (GObject managed)
 *   GncImportAssistant, and a few C++ member objects.
 * - The entry point function will also register the created object in the
 *   component manager. This works because the (plain C) component manager just stores
 *   the (C++) pointer to the object, it doesn't act on it directly in any way.
 * - When the assistant is closed the component manager will invoke a
 *   close handler on the class pointer. We supply this close handler ourselves
 *   in csv_tximp_close_handler. Aside from some component management administration
 *   the essential action of this function is to (c++) "delete"
 *   the class object again. As the C++ class implements RAII this destruction will take care
 *   of freeing all the member objects it manages.
 * - Note the component manager's only benefit in this context is that at gnucash shutdown
 *   all still open dialogs can be closed cleanly. Whether this benefit is enough to
 *   justify the added complexity is debatable. However currently the calling code is not
 *   c++ yet so we can't use RAII in the calling object to better handle this right now.
 *
 * - Let's zoom in on the c++ member objects and in particular the GncImportAssistant and related objects.
 *   These are created the gtk way in the c++ class constructor. That means the main GncImportAssistant widget
 *   will be responsible for the lifecycle of its child widgets.
 * - Thanks to the RAII implementation the destruction of this widget is commanded in the c++ class
 *   destructor. This gets activated when the user clicks the assistant's close button via the component
 *   manager callback mechanism as mentioned above.
 *
 * - There is one case that needs some additional attention. At some point the csv importer assistant hands
 *   over control to a generic import matcher (created via gnc_gen_trans_assist_new). This generic import
 *   matcher unfortunately destroys itself when run. However it is not run in all our possible user scenarios.
 *   This means we sometimes have to free it and sometimes we don't. This could have been
 *   avoided if we didn't have to track the object across several gtk callback functions and
 *   instead just create it only right before using it. To handle this we start with RAII:
 *   the c++ class object assumes ownership of the generic import matcher object and the class destructor will
 *   attempt to free it. This is safe both if the object is effectively allocated or when it's nullified.
 *   Then to handle the case were the generic import matcher will free the matcher object, the c++ class object
 *   will release ownership of the generic pointer object right before starting the generic import matcher.
 *
 *   TODO this is pretty confusing and should be cleaned up when we rewrite the generic importer.
 */

class  CsvImpTransAssist
{
public:
    CsvImpTransAssist ();
    ~CsvImpTransAssist ();

    /* Delete copy and move constructor/assignments
     * We don't want gui elements to be moved around or copied at all */
    CsvImpTransAssist(const CsvImpTransAssist&) = delete;            // copy constructor
    CsvImpTransAssist& operator=(const CsvImpTransAssist&) = delete; // copy assignment
    CsvImpTransAssist(CsvImpTransAssist&&) = delete;                 // move constructor
    CsvImpTransAssist& operator=(CsvImpTransAssist&&) = delete;      // move assignment

    void assist_prepare_cb (GtkWidget *page);
    void assist_file_page_prepare ();
    void assist_preview_page_prepare ();
    void assist_account_match_page_prepare ();
    void assist_doc_page_prepare ();
    void assist_match_page_prepare ();
    void assist_summary_page_prepare ();
    void assist_finish ();
    void new_book_options_finished (gboolean applied, QofBook *book);
    void assist_compmgr_close ();

    void select_file_cb ();

    void preview_settings_delete ();
    void preview_settings_save ();
    void preview_settings_name (GtkEntry* entry);
    void preview_settings_load ();
    void preview_update_skipped_rows ();
    void preview_multi_split (bool multi);
    void preview_update_separators (GtkWidget* widget);
    void preview_update_file_format ();
    void preview_update_account ();
    void preview_update_encoding (const char* encoding);
    void preview_update_date_format ();
    void preview_update_currency_format ();
    void preview_update_col_type (GtkDropDown* dropdown);

    void preview_populate_settings_combo();
    void preview_handle_save_del_sensitivity ();
    void preview_refresh_table ();
    void preview_refresh ();
    void preview_validate_settings ();

    void acct_match_via_button ();
    void acct_match_select (GObject *row);
    void acct_match_select_at (guint position);
    void acct_match_apply_selection (GObject *row, Account *account);
    void acct_match_set_accounts ();

private:
    struct FileDialogData
    {
        GWeakRef assistant;
    };

    struct SettingsConfirmationData
    {
        GWeakRef assistant;
        std::string name;
        bool deleting;
    };

    static void settings_confirmation_cb (GtkWindow *parent, gint response,
                                          gpointer user_data);
    static void settings_confirmation_data_free (SettingsConfirmationData *data);
    void complete_settings_delete (const std::string& name);
    void complete_settings_save (const std::string& name);

    static void file_dialog_finished_cb (GObject *source, GAsyncResult *result,
                                         gpointer user_data);
    bool set_selected_file (GFile *file);
    void preview_queue_refresh_table ();
    static void preview_refresh_table_idle_cb (gpointer user_data);

    CsvPreviewRefreshIdle preview_refresh_idle;

    GncImportAssistant    *csv_imp_asst;

    GtkWidget       *file_page;                     /**< Assistant file page widget */
    GtkWidget       *file_select_button;            /**< Opens the native file dialog */
    GtkWidget       *file_name_label;               /**< Displays the selected import file */
    std::string      m_fc_file_name;                /**< The file currently selected for import */
    std::string      m_final_file_name;             /**< The name of the import file effectively to use */

    GtkWidget       *preview_page;                  /**< Assistant preview page widget */
    GtkDropDown     *settings_dropdown;             /**< The saved settings selector */
    GtkEntry        *settings_entry;                /**< The editable saved settings name */
    GtkWidget       *save_button;                   /**< The Save Settings button */
    GtkWidget       *del_button;                    /**< The Delete Settings button */
    GtkWidget       *acct_selector;                 /**< The Account selector */
    GtkWidget       *combo_hbox;                    /**< The Settings Combo hbox */
    GtkSpinButton   *start_row_spin;                /**< The widget for the start row spinner */
    GtkSpinButton   *end_row_spin;                  /**< The widget for the end row spinner */
    GtkWidget       *skip_alt_rows_button;          /**< The widget for Skip alternate rows from start row */
    GtkWidget       *skip_errors_button;            /**< The widget for Skip error rows*/
    GtkWidget       *csv_button;                    /**< The widget for the CSV button */
    GtkWidget       *fixed_button;                  /**< The widget for the Fixed Width button */
    GtkWidget       *multi_split_cbutton;           /**< The widget for Multi-split */
    GOCharmapSel    *encselector;                   /**< The widget for selecting the encoding */
    GtkWidget       *separator_table;               /**< Container for the separator checkboxes */
    GtkCheckButton  *sep_button[SEP_NUM_OF_TYPES];  /**< Checkbuttons for common separators */
    GtkCheckButton  *escape_cbutton;                /**< The checkbutton for escape processing */
    GtkWidget       *fw_instructions_hbox;          /**< Container for fixed-width instructions */
    GtkCheckButton  *custom_cbutton;                /**< The checkbutton for a custom separator */
    GtkEntry        *custom_entry;                  /**< The entry for custom separators */
    GtkDropDown     *date_format_dropdown;          /**< The date format selector */
    GtkDropDown     *currency_format_dropdown;      /**< The currency format selector */
    GtkColumnView   *preview_view;                  /**< The GTK4 preview table */
    GtkBox          *preview_column_selectors;      /**< The column type selectors */
    GtkLabel        *instructions_label;            /**< The instructions label */
    GtkImage        *instructions_image;            /**< The instructions image */
    bool             encoding_selected_called;      /**< Before encoding_selected is first called, this is false.
                                                       * error lines, instead of all the file data. */

    GtkWidget            *account_match_page;       /**< Assistant account matcher page widget */
    GtkColumnView        *account_match_view;       /**< Assistant account matcher view */
    GListStore           *account_match_store;      /**< The account matching rows */
    GtkSingleSelection   *account_match_selection;  /**< The selected mapping */
    GtkWidget            *account_match_label;      /**< Assistant account matcher label widget */
    GtkWidget            *account_match_btn;        /**< Assistant account matcher button widget */

    GtkWidget            *doc_page;                 /**< Assistant doc page widget */

    GtkWidget            *match_page;               /**< Assistant match page widget, to be packed with the transaction matcher */
    GtkWidget            *match_label;              /**< The match label at the bottom of the page */
    GNCImportMainMatcher *gnc_csv_importer_gui;     /**< The GNCImportMainMatcher structure */
    GtkWidget            *help_button;              /**< The widget for the help button on the matcher page */
    GtkWidget            *cancel_button = nullptr;  /**< Kept for matcher cleanup compatibility. */

    GtkWidget            *summary_page;             /**< Assistant summary page widget */
    GtkWidget            *summary_label;            /**< The summary text */

    bool                  new_book;                 /**< Are we importing into a new book?; if yes, call book options */
    std::unique_ptr<GncTxImport> tx_imp;            /**< The actual data we are previewing */

    bool                  m_req_mapped_accts;
};


/*******************************************************
 * Assistant call back functions
 *******************************************************/

extern "C"
{
void csv_tximp_assist_prepare_cb (GncImportAssistant  *assistant, GtkWidget *page, gpointer user_data);
void csv_tximp_assist_close_cb (GncImportAssistant *gtkassistant, gpointer user_data);
void csv_tximp_assist_finish_cb (GncImportAssistant *gtkassistant, gpointer user_data);
void csv_tximp_select_file_cb (GtkButton *button, CsvImpTransAssist *info);
void csv_tximp_preview_del_settings_cb (GtkWidget *button, CsvImpTransAssist *info);
void csv_tximp_preview_save_settings_cb (GtkWidget *button, CsvImpTransAssist *info);
void csv_tximp_preview_settings_sel_changed_cb (GtkDropDown *dropdown, GParamSpec *pspec,
                                                CsvImpTransAssist *info);
void csv_tximp_preview_settings_text_inserted_cb (GtkEditable *entry, gchar *new_text,
        gint new_text_length, gint *position, CsvImpTransAssist *info);
void csv_tximp_preview_settings_text_changed_cb (GtkEntry *entry, CsvImpTransAssist *info);
void csv_tximp_preview_srow_cb (GtkSpinButton *spin, CsvImpTransAssist *info);
void csv_tximp_preview_erow_cb (GtkSpinButton *spin, CsvImpTransAssist *info);
void csv_tximp_preview_skiprows_cb (GtkCheckButton *checkbox, CsvImpTransAssist *info);
void csv_tximp_preview_skiperrors_cb (GtkCheckButton *checkbox, CsvImpTransAssist *info);
void csv_tximp_preview_multisplit_cb (GtkCheckButton *checkbox, CsvImpTransAssist *info);
void csv_tximp_preview_sep_button_cb (GtkWidget* widget, CsvImpTransAssist* info);
void csv_tximp_preview_sep_fixed_sel_cb (GtkCheckButton* csv_button, CsvImpTransAssist* info);
void csv_tximp_preview_acct_sel_cb (GtkWidget* widget, CsvImpTransAssist* info);
void csv_tximp_preview_enc_sel_cb (GOCharmapSel* selector, const char* encoding,
                              CsvImpTransAssist* info);
void csv_tximp_acct_match_button_clicked_cb (GtkWidget *widget, CsvImpTransAssist* info);
}

void
csv_tximp_assist_prepare_cb (GncImportAssistant *assistant, GtkWidget *page,
        gpointer user_data)
{
    auto info = static_cast<CsvImpTransAssist *> (user_data);
    info->assist_prepare_cb(page);
}

void
csv_tximp_assist_close_cb (GncImportAssistant *assistant, gpointer user_data)
{
    auto info = static_cast<CsvImpTransAssist *> (user_data);
    gnc_close_gui_component_by_data (ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS, info);
}

void
csv_tximp_assist_finish_cb (GncImportAssistant *assistant, gpointer user_data)
{
    auto info = static_cast<CsvImpTransAssist *> (user_data);
    info->assist_finish ();
}

void csv_tximp_select_file_cb (GtkButton *button, CsvImpTransAssist *info)
{
    info->select_file_cb ();
    (void)button;
}

void csv_tximp_preview_del_settings_cb (GtkWidget *button, CsvImpTransAssist *info)
{
    info->preview_settings_delete();
}

void csv_tximp_preview_save_settings_cb (GtkWidget *button, CsvImpTransAssist *info)
{
    info->preview_settings_save();
}

void csv_tximp_preview_settings_sel_changed_cb (GtkDropDown *dropdown, GParamSpec *pspec,
                                                CsvImpTransAssist *info)
{
    info->preview_settings_load();
}

void
csv_tximp_preview_settings_text_inserted_cb (GtkEditable *entry, gchar *new_text,
        gint new_text_length, gint *position, CsvImpTransAssist *info)
{
    if (!new_text)
        return;

    /* Prevent entering [], which are invalid characters in key files */
    auto base_txt = std::string (new_text);
    auto mod_txt = base_txt;
    std::replace (mod_txt.begin(), mod_txt.end(), '[', '(');
    std::replace (mod_txt.begin(), mod_txt.end(), ']', ')');
    if (base_txt == mod_txt)
        return;
    g_signal_handlers_block_by_func (entry, (gpointer) csv_tximp_preview_settings_text_inserted_cb, info);
    gtk_editable_insert_text (entry, mod_txt.c_str(), mod_txt.size() , position);
    g_signal_handlers_unblock_by_func (entry, (gpointer) csv_tximp_preview_settings_text_inserted_cb, info);

    g_signal_stop_emission_by_name (entry, "insert_text");
}

void
csv_tximp_preview_settings_text_changed_cb (GtkEntry *entry, CsvImpTransAssist *info)
{
    info->preview_settings_name(entry);
}

void csv_tximp_preview_srow_cb (GtkSpinButton *spin, CsvImpTransAssist *info)
{
    info->preview_update_skipped_rows();
}

void csv_tximp_preview_erow_cb (GtkSpinButton *spin, CsvImpTransAssist *info)
{
    info->preview_update_skipped_rows();
}

void csv_tximp_preview_skiprows_cb (GtkCheckButton *checkbox, CsvImpTransAssist *info)
{
    info->preview_update_skipped_rows();
}

void csv_tximp_preview_skiperrors_cb (GtkCheckButton *checkbox, CsvImpTransAssist *info)
{
    info->preview_update_skipped_rows();
}

void csv_tximp_preview_multisplit_cb (GtkCheckButton *checkbox, CsvImpTransAssist *info)
{
    info->preview_multi_split (gtk_check_button_get_active (checkbox));
}

void csv_tximp_preview_sep_button_cb (GtkWidget* widget, CsvImpTransAssist* info)
{
    info->preview_update_separators(widget);
}

void csv_tximp_preview_sep_fixed_sel_cb (GtkCheckButton* csv_button, CsvImpTransAssist* info)
{
    info->preview_update_file_format();
}

void csv_tximp_preview_acct_sel_cb (GtkWidget* widget, CsvImpTransAssist* info)
{
    info->preview_update_account();
}

void csv_tximp_preview_enc_sel_cb (GOCharmapSel* selector, const char* encoding,
                              CsvImpTransAssist* info)
{
    info->preview_update_encoding(encoding);
}

static void csv_tximp_preview_date_fmt_sel_cb (GtkDropDown* format_selector, GParamSpec* pspec,
                                               CsvImpTransAssist* info)
{
    info->preview_update_date_format();
}

static void csv_tximp_preview_currency_fmt_sel_cb (GtkDropDown* format_selector, GParamSpec* pspec,
                                                   CsvImpTransAssist* info)
{
    info->preview_update_currency_format();
}

static void csv_tximp_preview_col_type_changed_cb (GtkDropDown* dropdown, GParamSpec* pspec,
                                                   CsvImpTransAssist* info)
{
    info->preview_update_col_type (dropdown);
}


void csv_tximp_acct_match_button_clicked_cb (GtkWidget *widget, CsvImpTransAssist* info)
{
    info->acct_match_via_button();
}

static constexpr auto ACCOUNT_MATCH_ROW_DATA = "csv-transaction-account-match-row";

struct CsvTransactionAccountMatchRow
{
    std::string mapping;
    std::string fullpath;
    Account *account;
};

static GObject*
csv_tximp_account_match_row_new (const std::string& mapping)
{
    auto row = G_OBJECT (g_object_new (G_TYPE_OBJECT, nullptr));
    auto values = new CsvTransactionAccountMatchRow { mapping, _("No Linked Account"), nullptr };
    g_object_set_data_full (row, ACCOUNT_MATCH_ROW_DATA, values,
                            [] (gpointer data) { delete static_cast<CsvTransactionAccountMatchRow*> (data); });
    return row;
}

static CsvTransactionAccountMatchRow*
csv_tximp_account_match_row_get (GObject *row)
{
    return static_cast<CsvTransactionAccountMatchRow*> (g_object_get_data (row, ACCOUNT_MATCH_ROW_DATA));
}

static void
csv_tximp_account_match_item_setup (GtkListItemFactory *factory, GtkListItem *item,
                                    gpointer user_data)
{
    auto label = gtk_label_new (nullptr);

    (void)factory;
    (void)user_data;
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (item, label);
}

static void
csv_tximp_account_match_item_bind (GtkListItemFactory *factory, GtkListItem *item,
                                   gpointer user_data)
{
    auto row = csv_tximp_account_match_row_get (G_OBJECT (gtk_list_item_get_item (item)));

    (void)factory;
    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (item)),
                        GPOINTER_TO_UINT (user_data) == MAPPING_STRING
                        ? row->mapping.c_str () : row->fullpath.c_str ());
}

static void
csv_tximp_account_match_add_column (GtkColumnView *view, const gchar *title, guint column)
{
    auto factory = gtk_signal_list_item_factory_new ();
    auto view_column = gtk_column_view_column_new (title, factory);

    g_signal_connect (factory, "setup", G_CALLBACK (csv_tximp_account_match_item_setup),
                      GUINT_TO_POINTER (column));
    g_signal_connect (factory, "bind", G_CALLBACK (csv_tximp_account_match_item_bind),
                      GUINT_TO_POINTER (column));
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_append_column (view, view_column);
    g_object_unref (view_column);
}

static void
csv_tximp_account_match_view_activated_cb (GtkColumnView *view, guint position,
                                           CsvImpTransAssist *info)
{
    (void)view;
    info->acct_match_select_at (position);
}


/*******************************************************
 * Assistant Constructor
 *******************************************************/
CsvImpTransAssist::CsvImpTransAssist () :
    preview_refresh_idle {preview_refresh_table_idle_cb, this}
{
    auto builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "start_row_adj");
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "end_row_adj");
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "csv_transaction_assistant");
    csv_imp_asst = gnc_import_assistant_new (
        GTK_WINDOW (gtk_builder_get_object (builder, "csv_transaction_assistant")),
        GTK_STACK (gtk_builder_get_object (builder, "gnc_import_assistant_stack")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_page_title")),
        GTK_BOX (gtk_builder_get_object (builder, "gnc_import_assistant_actions")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_back")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_next")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_apply")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_cancel")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_close")));
    if (!csv_imp_asst)
        throw std::runtime_error ("Unable to construct CSV import assistant");
    g_object_set_data (G_OBJECT (csv_imp_asst), "gnc-csv-import-assistant-owner", this);

    // Set the name for this assistant so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(csv_imp_asst), "gnc-id-assistant-csv-transaction-import");
    gnc_widget_style_context_add_class (GTK_WIDGET(csv_imp_asst), "gnc-class-imports");

    /* Enable buttons on all page. */
    gnc_import_assistant_set_page_complete (csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "start_page")),
                                     true);
    gnc_import_assistant_set_page_complete (csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "file_page")),
                                     false);
    gnc_import_assistant_set_page_complete (csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "preview_page")),
                                     false);
    gnc_import_assistant_set_page_complete (csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "account_match_page")),
                                     false);
    gnc_import_assistant_set_page_complete (csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "doc_page")),
                                     true);
    gnc_import_assistant_set_page_complete (csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "match_page")),
                                     true);
    gnc_import_assistant_set_page_complete (csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "summary_page")),
                                     true);

    /* File selection page */
    file_page = GTK_WIDGET (gtk_builder_get_object (builder, "file_page"));
    file_select_button = GTK_WIDGET (gtk_builder_get_object (
        builder, "file_select_button"));
    file_name_label = GTK_WIDGET (gtk_builder_get_object (
        builder, "file_name_label"));
    g_signal_connect (file_select_button, "clicked",
                      G_CALLBACK (csv_tximp_select_file_cb), this);

    /* Preview Settings Page */
    {
        preview_page = GTK_WIDGET(gtk_builder_get_object (builder, "preview_page"));

        // GtkDropDown provides the saved presets while GtkEntry keeps custom names editable.
        auto settings_store = g_list_store_new (GTK_TYPE_STRING_OBJECT);
        auto settings_expression = gtk_property_expression_new (GTK_TYPE_STRING_OBJECT, nullptr, "string");
        settings_dropdown = gnc_gtk_drop_down_new (G_LIST_MODEL (settings_store), settings_expression);
        settings_entry = GTK_ENTRY (gtk_entry_new ());
        gtk_widget_set_hexpand (GTK_WIDGET (settings_entry), true);
        combo_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "combo_hbox"));
        auto settings_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
        gtk_box_append (GTK_BOX (settings_box), GTK_WIDGET (settings_dropdown));
        gtk_box_append (GTK_BOX (settings_box), GTK_WIDGET (settings_entry));
        gtk_center_box_set_start_widget (GTK_CENTER_BOX (combo_hbox), settings_box);

        g_signal_connect (settings_dropdown, "notify::selected",
                         G_CALLBACK(csv_tximp_preview_settings_sel_changed_cb), this);
        g_signal_connect (settings_entry, "changed",
                         G_CALLBACK(csv_tximp_preview_settings_text_changed_cb), this);
        g_signal_connect (settings_entry, "insert-text",
                         G_CALLBACK(csv_tximp_preview_settings_text_inserted_cb), this);

        // Add Save Settings button
        save_button = GTK_WIDGET(gtk_builder_get_object (builder, "save_settings"));

        // Add Delete Settings button
        del_button = GTK_WIDGET(gtk_builder_get_object (builder, "delete_settings"));

        /* The table containing the separator configuration widgets */
        start_row_spin = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "start_row"));
        end_row_spin = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "end_row"));
        skip_alt_rows_button = GTK_WIDGET(gtk_builder_get_object (builder, "skip_rows"));
        skip_errors_button = GTK_WIDGET(gtk_builder_get_object (builder, "skip_errors_button"));
        multi_split_cbutton = GTK_WIDGET(gtk_builder_get_object (builder, "multi_split_button"));
        separator_table = GTK_WIDGET(gtk_builder_get_object (builder, "separator_table"));
        fw_instructions_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "fw_instructions_hbox"));

        /* Load the separator buttons from the glade builder file into the
         * sep_buttons array. */
        const char* sep_button_names[] = {
                "space_cbutton",
                "tab_cbutton",
                "comma_cbutton",
                "colon_cbutton",
                "semicolon_cbutton",
                "hyphen_cbutton"
            };
        for (int i = 0; i < SEP_NUM_OF_TYPES; i++)
            sep_button[i]
                = GTK_CHECK_BUTTON(gtk_builder_get_object (builder, sep_button_names[i]));

        /* Load and connect the custom separator checkbutton in the same way
         * as the other separator buttons. */
        custom_cbutton
            = GTK_CHECK_BUTTON(gtk_builder_get_object (builder, "custom_cbutton"));

        /* Load the entry for the custom separator entry. Connect it to the
         * sep_button_clicked event handler as well. */
        custom_entry = GTK_ENTRY(gtk_builder_get_object (builder, "custom_entry"));

        /* Load and connect the escape checkbutton in the same way
         * as the other separator buttons. */
        escape_cbutton
            = GTK_CHECK_BUTTON(gtk_builder_get_object (builder, "escape_cbutton"));

        /* Add account selection widget */
        acct_selector = gnc_account_sel_new();
        auto account_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "account_hbox"));
        gtk_box_append (GTK_BOX(account_hbox), GTK_WIDGET(acct_selector));
        gtk_box_set_spacing (GTK_BOX(account_hbox), 6);
        gtk_widget_set_visible (GTK_WIDGET(acct_selector), true);

        g_signal_connect(G_OBJECT(acct_selector), "account_sel_changed",
                         G_CALLBACK(csv_tximp_preview_acct_sel_cb), this);


        /* Create the encoding selector widget and add it to the assistant */
        encselector = GO_CHARMAP_SEL(go_charmap_sel_new(GO_CHARMAP_SEL_TO_UTF8));
        /* Connect the selector to the encoding_selected event handler. */
        g_signal_connect (G_OBJECT(encselector), "charmap_changed",
                         G_CALLBACK(csv_tximp_preview_enc_sel_cb), this);

        auto encoding_box = GTK_BOX(gtk_builder_get_object (builder, "encoding_container"));
        gtk_box_prepend (GTK_BOX(encoding_box), GTK_WIDGET(encselector));
        gtk_widget_set_hexpand (GTK_WIDGET(encselector), true);

        /* The instructions label and image */
        instructions_label = GTK_LABEL(gtk_builder_get_object (builder, "instructions_label"));
        instructions_image = GTK_IMAGE(gtk_builder_get_object (builder, "instructions_image"));

        /* Add in the date format combo box and hook it up to an event handler. */
        std::vector<const char*> date_formats;
        for (auto& date_fmt : GncDate::c_formats)
            date_formats.emplace_back (_(date_fmt.m_fmt.c_str()));
        date_formats.emplace_back (nullptr);
        date_format_dropdown = gnc_gtk_drop_down_new_from_strings (date_formats.data());
        gtk_drop_down_set_selected (date_format_dropdown, 0);
        g_signal_connect (date_format_dropdown, "notify::selected",
                         G_CALLBACK(csv_tximp_preview_date_fmt_sel_cb), this);

        /* Add it to the assistant. */
        auto date_format_box = GTK_BOX(gtk_builder_get_object (builder, "date_format_container"));
        gtk_box_prepend (GTK_BOX(date_format_box), GTK_WIDGET(date_format_dropdown));
        gtk_widget_set_hexpand (GTK_WIDGET(date_format_dropdown), true);

        /* Add in the currency format combo box and hook it up to an event handler. */
        std::vector<const char*> currency_formats;
        for (int i = 0; i < num_currency_formats; i++)
            currency_formats.emplace_back (_(currency_format_user[i]));
        currency_formats.emplace_back (nullptr);
        currency_format_dropdown = gnc_gtk_drop_down_new_from_strings (currency_formats.data());
        /* Default will the locale */
        gtk_drop_down_set_selected (currency_format_dropdown, 0);
        g_signal_connect (currency_format_dropdown, "notify::selected",
                         G_CALLBACK(csv_tximp_preview_currency_fmt_sel_cb), this);

        /* Add it to the assistant. */
        auto currency_format_box = GTK_BOX(gtk_builder_get_object (builder, "currency_format_container"));
        gtk_box_prepend (GTK_BOX(currency_format_box), GTK_WIDGET(currency_format_dropdown));
        gtk_widget_set_hexpand (GTK_WIDGET(currency_format_dropdown), true);

        /* Connect the CSV/Fixed-Width radio button event handler. */
        csv_button = GTK_WIDGET(gtk_builder_get_object (builder, "csv_button"));
        fixed_button = GTK_WIDGET(gtk_builder_get_object (builder, "fixed_button"));

        /* The GTK4 preview owns its list model and columns programmatically. */
        preview_view = GTK_COLUMN_VIEW (gtk_column_view_new (nullptr));
        gtk_column_view_set_show_column_separators (preview_view, true);
        gtk_column_view_set_show_row_separators (preview_view, true);
        auto preview_scrolled = GTK_SCROLLED_WINDOW (gtk_builder_get_object (builder, "scrolledwindow2"));
        gtk_scrolled_window_set_child (preview_scrolled, GTK_WIDGET (preview_view));
        preview_column_selectors = GTK_BOX (gtk_builder_get_object (builder, "preview_column_selectors"));

        /* This is true only after encoding_selected is called, so we must
         * set it initially to false. */
        encoding_selected_called = false;
    }

    /* Account Match Page */
    account_match_page  = GTK_WIDGET(gtk_builder_get_object (builder, "account_match_page"));
    account_match_label = GTK_WIDGET(gtk_builder_get_object (builder, "account_match_label"));
    account_match_btn = GTK_WIDGET(gtk_builder_get_object (builder, "account_match_change"));
    account_match_store = g_list_store_new (G_TYPE_OBJECT);
    account_match_selection = gtk_single_selection_new (G_LIST_MODEL (g_object_ref (account_match_store)));
    account_match_view = GTK_COLUMN_VIEW (gtk_column_view_new (GTK_SELECTION_MODEL (
        g_object_ref (account_match_selection))));
    csv_tximp_account_match_add_column (account_match_view, _("Account ID"), MAPPING_STRING);
    csv_tximp_account_match_add_column (account_match_view, _("Account Name"), MAPPING_FULLPATH);
    auto account_match_scrolled = GTK_SCROLLED_WINDOW (gtk_builder_get_object (builder,
                                                         "account_match_swindow"));
    gtk_scrolled_window_set_child (account_match_scrolled, GTK_WIDGET (account_match_view));
    g_signal_connect (account_match_view, "activate",
                      G_CALLBACK (csv_tximp_account_match_view_activated_cb), this);

    /* Doc Page */
    doc_page = GTK_WIDGET(gtk_builder_get_object (builder, "doc_page"));

    /* Matcher page */
    match_page  = GTK_WIDGET(gtk_builder_get_object (builder, "match_page"));
    match_label = GTK_WIDGET(gtk_builder_get_object (builder, "match_label"));

    /* Create the generic transaction importer GUI.
       Note, this will call g_new0 internally. The returned object is g_freed again
       either directly by the main matcher or in our assistant_finish code of the matcher
       is never reached. */
    gnc_csv_importer_gui = gnc_gen_trans_assist_new (GTK_WIDGET(csv_imp_asst),
                                                     match_page, nullptr, false, 42);

    /* Summary Page */
    summary_page  = GTK_WIDGET(gtk_builder_get_object (builder, "summary_page"));
    summary_label = GTK_WIDGET(gtk_builder_get_object (builder, "summary_label"));

    gnc_restore_window_size (GNC_PREFS_GROUP,
                             GTK_WINDOW(csv_imp_asst), gnc_ui_get_main_window(nullptr));

gnc_builder_connect_signals (builder, this);
    gnc_import_assistant_set_page_action (csv_imp_asst, 5,
                                          GNC_IMPORT_ASSISTANT_PAGE_APPLY);
    gnc_import_assistant_set_page_action (csv_imp_asst, 6,
                                          GNC_IMPORT_ASSISTANT_PAGE_CLOSE);
    gnc_import_assistant_set_callbacks (csv_imp_asst, csv_tximp_assist_prepare_cb,
                                        csv_tximp_assist_finish_cb,
                                        csv_tximp_assist_close_cb,
                                        csv_tximp_assist_close_cb, this);
    g_object_unref (G_OBJECT(builder));

    gnc_window_adjust_for_screen (GTK_WINDOW(csv_imp_asst));
    gtk_window_present (GTK_WINDOW(csv_imp_asst));

    /* In order to trigger a book options display on the creation of a new book,
     * we need to detect when we are dealing with a new book. */
    new_book = gnc_is_new_book();
}


/*******************************************************
 * Assistant Destructor
 *******************************************************/
CsvImpTransAssist::~CsvImpTransAssist ()
{
    preview_refresh_idle.cancel ();
    g_object_set_data (G_OBJECT (csv_imp_asst), "gnc-csv-import-assistant-owner", nullptr);
    /* This function is safe to call on a null pointer */
    gnc_gen_trans_list_delete (gnc_csv_importer_gui);
    /* The call above frees gnc_csv_importer_gui but can't nullify it.
     * Do it here so no one accidentally can access it still */
    gnc_csv_importer_gui = nullptr;
    g_clear_object (&account_match_selection);
    g_clear_object (&account_match_store);
    gtk_window_destroy (GTK_WINDOW(csv_imp_asst));
}


/**************************************************
 * Code related to the file selection page
 **************************************************/

bool
CsvImpTransAssist::set_selected_file (GFile *file)
{
    auto file_name = g_file_get_path (file);

    if (!file_name || g_file_test (file_name, G_FILE_TEST_IS_DIR))
    {
        g_free (file_name);
        return false;
    }

    auto filepath = GncUri{file_name}.path().value_or ("");
    auto starting_dir = g_path_get_dirname (filepath.c_str());

    m_fc_file_name = file_name;
    gnc_set_default_directory (GNC_PREFS_GROUP, starting_dir);

    DEBUG ("file_name selected is %s", m_fc_file_name.c_str());
    DEBUG ("starting directory is %s", starting_dir);

    g_free (file_name);
    g_free (starting_dir);
    return true;
}

void
CsvImpTransAssist::file_dialog_finished_cb (GObject *source, GAsyncResult *result,
                                            gpointer user_data)
{
    auto data = static_cast<FileDialogData *> (user_data);
    auto request = GNC_FILE_DIALOG_REQUEST (source);
    GError *error = nullptr;
    auto file = gnc_file_dialog_request_finish (request, result, &error);
    auto assistant = GTK_WIDGET (g_weak_ref_get (&data->assistant));
    auto info = assistant ? static_cast<CsvImpTransAssist *> (
        g_object_get_data (G_OBJECT (assistant),
                           "gnc-csv-import-assistant-owner")) : nullptr;

    if (file && info)
    {
        if (info->set_selected_file (file))
        {
            gtk_label_set_text (GTK_LABEL (info->file_name_label),
                                info->m_fc_file_name.c_str());
            gnc_import_assistant_set_page_complete (info->csv_imp_asst, info->file_page,
                                             true);
            gnc_import_assistant_next_page (info->csv_imp_asst);
        }
        else
        {
            gnc_error_dialog (GTK_WINDOW (assistant), "%s",
                              _("Please select a local file, not a folder."));
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
    g_weak_ref_clear (&data->assistant);
    delete data;
}

void
CsvImpTransAssist::select_file_cb ()
{
    auto starting_dir = m_fc_file_name.empty ()
                        ? gnc_get_default_directory (GNC_PREFS_GROUP)
                        : g_path_get_dirname (m_fc_file_name.c_str());
    auto data = new FileDialogData{};
    g_weak_ref_init (&data->assistant, csv_imp_asst);
    auto request = gnc_file_dialog_request_new (
        GTK_WINDOW (csv_imp_asst), _("Select CSV Import File"), nullptr,
        starting_dir, GNC_FILE_DIALOG_IMPORT);
    gnc_file_dialog_request_open_async (request, nullptr,
                                        file_dialog_finished_cb, data);
    g_object_unref (request);
    g_free (starting_dir);
}

/**************************************************
 * Code related to the preview page
 **************************************************/

static constexpr auto TRANS_PRESET_ROW_DATA = "csv-transaction-preset";
static constexpr auto TRANS_COLUMN_TYPE_DATA = "csv-transaction-column-type";
static constexpr auto TRANS_PREVIEW_ROW_DATA = "csv-transaction-preview-row";

static CsvTransImpSettings*
csv_tximp_selected_preset (GtkDropDown *dropdown)
{
    auto position = gtk_drop_down_get_selected (dropdown);
    if (position == GTK_INVALID_LIST_POSITION)
        return nullptr;
    auto item = g_list_model_get_item (gtk_drop_down_get_model (dropdown), position);
    auto preset = static_cast<CsvTransImpSettings*> (g_object_get_data (G_OBJECT (item), TRANS_PRESET_ROW_DATA));
    g_object_unref (item);
    return preset;
}

static guint
csv_tximp_find_preset (GtkDropDown *dropdown, const std::string& name)
{
    auto model = gtk_drop_down_get_model (dropdown);
    for (guint position = 0; position < g_list_model_get_n_items (model); position++)
    {
        auto item = g_list_model_get_item (model, position);
        auto preset = static_cast<CsvTransImpSettings*> (g_object_get_data (G_OBJECT (item), TRANS_PRESET_ROW_DATA));
        g_object_unref (item);
        if (preset && preset->m_name == name)
            return position;
    }
    return GTK_INVALID_LIST_POSITION;
}

void
CsvImpTransAssist::settings_confirmation_data_free (SettingsConfirmationData *data)
{
    g_weak_ref_clear (&data->assistant);
    delete data;
}

void
CsvImpTransAssist::settings_confirmation_cb (GtkWindow *parent, gint response,
                                              gpointer user_data)
{
    auto data = static_cast<SettingsConfirmationData *> (user_data);
    auto assistant = static_cast<GncImportAssistant *> (
        g_weak_ref_get (&data->assistant));

    (void)parent;
    if (!assistant)
    {
        settings_confirmation_data_free (data);
        return;
    }

    auto owner = static_cast<CsvImpTransAssist *> (g_object_get_data (
        G_OBJECT (assistant), "gnc-csv-import-assistant-owner"));
    if (owner)
    {
        gtk_widget_set_sensitive (GTK_WIDGET (assistant), TRUE);
        if (response == GTK_RESPONSE_OK)
        {
            if (data->deleting)
                owner->complete_settings_delete (data->name);
            else
                owner->complete_settings_save (data->name);
        }
    }

    g_object_unref (assistant);
    settings_confirmation_data_free (data);
}

/* Set the available presets in the settings combo box
 */
void CsvImpTransAssist::preview_populate_settings_combo()
{
    auto store = G_LIST_STORE (gtk_drop_down_get_model (settings_dropdown));
    g_list_store_remove_all (store);

    auto presets = get_import_presets_trans ();
    for (const auto& preset : presets)
    {
        auto row = gtk_string_object_new (_(preset->m_name.c_str()));
        g_object_set_data (G_OBJECT (row), TRANS_PRESET_ROW_DATA, preset.get());
        g_list_store_append (store, row);
        g_object_unref (row);
    }
}

/* Enable or disable the save and delete settings buttons
 * depending on what is selected and entered as settings name
 */
void CsvImpTransAssist::preview_handle_save_del_sensitivity ()
{
    auto can_delete = false;
    auto can_save = false;
    auto entry_text = gnc_entry_get_text (settings_entry);
    auto preset = csv_tximp_selected_preset (settings_dropdown);
    /* Handle sensitivity of the delete and save button */
    if (preset && entry_text && preset->m_name == entry_text &&
        !preset_is_reserved_name (preset->m_name))
    {
        can_delete = true;
        can_save = true;
    }
    else if (entry_text && (strlen (entry_text) > 0) &&
            !preset_is_reserved_name (std::string(entry_text)))
        can_save = true;

    gtk_widget_set_sensitive (save_button, can_save);
    gtk_widget_set_sensitive (del_button, can_delete);

}

void
CsvImpTransAssist::preview_settings_name (GtkEntry* entry)
{
    auto text = gnc_entry_get_text (entry);
    if (text)
        tx_imp->settings_name(text);

    preview_handle_save_del_sensitivity ();
}


/* Use selected preset to configure the import. Triggered when
 * a preset is selected in the settings combo.
 */
void
CsvImpTransAssist::preview_settings_load ()
{
    auto preset = csv_tximp_selected_preset (settings_dropdown);

    if (!preset)
        return;

    gtk_editable_set_text (GTK_EDITABLE (settings_entry), preset->m_name.c_str());
    tx_imp->settings (*preset);
    if (preset->m_load_error)
        gnc_error_dialog (GTK_WINDOW (csv_imp_asst),
            "%s", _("There were problems reading some saved settings, continuing to load.\n"
                    "Please review and save again."));

    preview_refresh ();
    preview_handle_save_del_sensitivity ();
}

/* Callback to delete a settings entry
 */
void
CsvImpTransAssist::preview_settings_delete ()
{
    auto preset = csv_tximp_selected_preset (settings_dropdown);
    if (!preset)
        return;

    auto data = new SettingsConfirmationData{};
    g_weak_ref_init (&data->assistant, csv_imp_asst);
    data->name = preset->m_name;
    data->deleting = true;
    gnc_ok_cancel_dialog_async (GTK_WINDOW (csv_imp_asst), GTK_RESPONSE_CANCEL,
                                settings_confirmation_cb, data, "%s",
                                _("Delete the Import Settings."));
    gtk_widget_set_sensitive (GTK_WIDGET (csv_imp_asst), FALSE);
}

void
CsvImpTransAssist::complete_settings_delete (const std::string& name)
{
    if (preset_is_reserved_name (name))
        return;

    const auto& presets = get_import_presets_trans ();
    auto it = std::find_if (presets.begin (), presets.end (), [&name] (const auto& preset)
    {
        return preset && preset->m_name == name;
    });
    if (it == presets.end ())
        return;

    auto preset = *it;
    preset->remove ();
    preview_populate_settings_combo ();
    gtk_drop_down_set_selected (settings_dropdown, 0); // Default
    preview_refresh (); // Reset the widgets
}

/* Callback to save the current settings to the gnucash state file.
 */
void
CsvImpTransAssist::preview_settings_save ()
{
    auto new_name = tx_imp->settings_name ();
    auto existing = csv_tximp_find_preset (settings_dropdown, new_name);

    if (existing != GTK_INVALID_LIST_POSITION &&
        gtk_drop_down_get_selected (settings_dropdown) != existing)
    {
        auto data = new SettingsConfirmationData{};
        g_weak_ref_init (&data->assistant, csv_imp_asst);
        data->name = new_name;
        data->deleting = false;
        gnc_ok_cancel_dialog_async (GTK_WINDOW (csv_imp_asst), GTK_RESPONSE_OK,
                                    settings_confirmation_cb, data, "%s",
                                    _("Setting name already exists, overwrite?"));
        gtk_widget_set_sensitive (GTK_WIDGET (csv_imp_asst), FALSE);
        return;
    }

    complete_settings_save (new_name);
}

void
CsvImpTransAssist::complete_settings_save (const std::string& name)
{
    tx_imp->settings_name (name);
    if (!tx_imp->save_settings ())
    {
        gnc_info_dialog (GTK_WINDOW (csv_imp_asst),
                         "%s", _("The settings have been saved."));
        preview_populate_settings_combo ();
        auto position = csv_tximp_find_preset (settings_dropdown, name);
        if (position != GTK_INVALID_LIST_POSITION)
            gtk_drop_down_set_selected (settings_dropdown, position);
    }
    else
        gnc_error_dialog (GTK_WINDOW (csv_imp_asst),
                          "%s", _("There was a problem saving the settings, please try again."));
}
/* Callback triggered when user adjusts skip start lines
 */
void CsvImpTransAssist::preview_update_skipped_rows ()
{
    /* Update skip rows in the parser */
    tx_imp->update_skipped_lines (gtk_spin_button_get_value_as_int (start_row_spin),
        gtk_spin_button_get_value_as_int (end_row_spin),
        gtk_check_button_get_active (GTK_CHECK_BUTTON(skip_alt_rows_button)),
        gtk_check_button_get_active (GTK_CHECK_BUTTON(skip_errors_button)));

    /* And adjust maximum number of lines that can be skipped at each end accordingly */
    auto adj = gtk_spin_button_get_adjustment (end_row_spin);
    gtk_adjustment_set_upper (adj, tx_imp->m_parsed_lines.size()
            - tx_imp->skip_start_lines() -1);

    adj = gtk_spin_button_get_adjustment (start_row_spin);
    gtk_adjustment_set_upper (adj, tx_imp->m_parsed_lines.size()
            - tx_imp->skip_end_lines() - 1);

    preview_refresh_table ();
}

void CsvImpTransAssist::preview_multi_split (bool multi)
{
    tx_imp->multi_split(multi);
    preview_refresh ();
}


/** Event handler for separator changes. This function is called
 * whenever one of the widgets for configuring the separators (the
 * separator checkbuttons, the escape checkbutton or the custom
 * separator entry) is changed.
 * @param widget The widget that was changed
 */
void CsvImpTransAssist::preview_update_separators (GtkWidget* widget)
{

    /* Only manipulate separator characters if the currently open file is
     * csv separated. */
    if (tx_imp->file_format() != GncImpFileFormat::CSV)
        return;

    /* Add the corresponding characters to checked_separators for each
     * button that is checked. */
    auto checked_separators = std::string();
    const auto stock_sep_chars = std::string (" \t,:;-");
    for (int i = 0; i < SEP_NUM_OF_TYPES; i++)
    {
        if (gtk_check_button_get_active (sep_button[i]))
            checked_separators += stock_sep_chars[i];
    }

    /* Add the custom separator if the user checked its button. */
    if (gtk_check_button_get_active (custom_cbutton))
    {
        auto custom_sep = gnc_entry_get_text (custom_entry);
        if (custom_sep[0] != '\0') /* Don't add a blank separator (bad things will happen!). */
            checked_separators += custom_sep;
    }

    /* Set the parse options using the checked_separators list. */
    tx_imp->separators (checked_separators);
    tx_imp->enable_escape (gtk_check_button_get_active (escape_cbutton));

    /* Parse the data using the new options. We don't want to reguess
     * the column types because we want to leave the user's
     * configurations intact. */
    try
    {
        tx_imp->tokenize (false);
        preview_refresh_table ();
    }
    catch (std::range_error &e)
    {
        /* Warn the user there was a problem and try to undo what caused
         * the error. (This will cause a reparsing and ideally a usable
         * configuration.) */
        gnc_error_dialog (GTK_WINDOW (csv_imp_asst), "Error in parsing");
        /* If we're here because the user changed the file format, we should just wait for the user
         * to update the configuration */
        if (!widget)
            return;
        /* If the user changed the custom separator, erase that custom separator. */
        if (widget == GTK_WIDGET(custom_entry))
            gnc_entry_set_text (GTK_ENTRY(widget), "");
        /* If the user checked a checkbutton, toggle that checkbutton back. */
        else
            gtk_check_button_set_active (GTK_CHECK_BUTTON(widget),
                                         !gtk_check_button_get_active (GTK_CHECK_BUTTON(widget)));
        return;
    }
}


/** Event handler for clicking one of the format type radio
 * buttons. This occurs if the format (Fixed-Width or CSV) is changed.
 */
void CsvImpTransAssist::preview_update_file_format ()
{
    /* Set the parsing type correctly. */
    try
    {
        if (gtk_check_button_get_active (GTK_CHECK_BUTTON(csv_button)))
        {
            tx_imp->file_format (GncImpFileFormat::CSV);
            gtk_widget_set_visible (separator_table, true);
            gtk_widget_set_visible (fw_instructions_hbox, false);
        }
        else
        {
            tx_imp->file_format (GncImpFileFormat::FIXED_WIDTH);
            gtk_widget_set_visible (separator_table, false);
            gtk_widget_set_visible (fw_instructions_hbox, true);

        }

        tx_imp->tokenize (false);
        preview_refresh_table ();
    }
    catch (std::range_error &e)
    {
        /* Parsing failed ... */
        gnc_error_dialog (GTK_WINDOW (csv_imp_asst), "%s", e.what());
        return;
    }
    catch (...)
    {
        // FIXME Handle file loading errors (possibly thrown by file_format above)
        PWARN("Got an error during file loading");
    }
}


void CsvImpTransAssist::preview_update_account ()
{;
    auto acct = gnc_account_sel_get_account( GNC_ACCOUNT_SEL(acct_selector) );
    tx_imp->base_account(acct);
    preview_refresh_table ();
}


/** Event handler for a new encoding. This is called when the user
 * selects a new encoding; the data is reparsed and shown to the
 * user.
 * @param encoding The encoding that the user selected
 */
void
CsvImpTransAssist::preview_update_encoding (const char* encoding)
{
    /* This gets called twice every time a new encoding is selected. The
     * second call actually passes the correct data; thus, we only do
     * something the second time this is called. */

    /* If this is the second time the function is called ... */
    if (encoding_selected_called)
    {
        std::string previous_encoding = tx_imp->m_tokenizer->encoding();
        /* Try converting the new encoding and reparsing. */
        try
        {
            tx_imp->encoding (encoding);
            preview_refresh_table ();
        }
        catch (...)
        {
            /* If it fails, change back to the old encoding. */
            gnc_error_dialog (GTK_WINDOW (csv_imp_asst), "%s", _("Invalid encoding selected"));
            go_charmap_sel_set_encoding (encselector, previous_encoding.c_str());
        }
    }

    encoding_selected_called = !encoding_selected_called;
}


void
CsvImpTransAssist::preview_update_date_format ()
{
    tx_imp->date_format (gtk_drop_down_get_selected (date_format_dropdown));
    preview_refresh_table ();
}


void
CsvImpTransAssist::preview_update_currency_format ()
{
    tx_imp->currency_format (gtk_drop_down_get_selected (currency_format_dropdown));
    preview_refresh_table ();
}

void
CsvImpTransAssist::preview_refresh_table_idle_cb (gpointer user_data)
{
    auto assist = static_cast<CsvImpTransAssist *> (user_data);

    assist->preview_refresh_table ();
}

void
CsvImpTransAssist::preview_queue_refresh_table ()
{
    preview_refresh_idle.queue ();
}

/* Internally used enum to access the columns in the comboboxes
 * the user can click to set a type for each column of the data
 */
enum PreviewHeaderComboCols { COL_TYPE_NAME, COL_TYPE_ID };
/* Internally used enum to access the first two (fixed) columns
 * in the model used to display the prased data.
 */
enum PreviewDataTableCols {
    PREV_COL_FCOLOR,
    PREV_COL_BCOLOR,
    PREV_COL_STRIKE,
    PREV_COL_ERROR,
    PREV_COL_ERR_ICON,
    PREV_N_FIXED_COLS };

/** Event handler for the user selecting a new column type. When the
 * user selects a new column type, that column's text must be changed
 * to the selection, and any other columns containing that selection
 * must be changed to "None" because we don't allow duplicates.
 * @param cbox The combo box the user just clicked to make a change
 */
void CsvImpTransAssist::preview_update_col_type (GtkDropDown* dropdown)
{
    auto position = gtk_drop_down_get_selected (dropdown);
    if (position == GTK_INVALID_LIST_POSITION)
        return;
    auto item = g_list_model_get_item (gtk_drop_down_get_model (dropdown), position);
    auto new_col_type = static_cast<GncTransPropType> (GPOINTER_TO_INT (
        g_object_get_data (G_OBJECT (item), TRANS_COLUMN_TYPE_DATA)));
    g_object_unref (item);
    auto col_num = GPOINTER_TO_UINT (g_object_get_data (G_OBJECT(dropdown), "col-num"));
    tx_imp->set_column_type (col_num, new_col_type);

    /* Delay rebuilding our data table to avoid critical warnings due to
     * pending events still acting on them after this event is processed.
     */
    preview_queue_refresh_table ();

}


struct CsvTransactionPreviewRow
{
    std::vector<std::string> cells;
    std::string error;
    bool skipped;
};

static GObject*
csv_tximp_preview_row_new (std::vector<std::string> cells, std::string error, bool skipped)
{
    auto row = G_OBJECT (g_object_new (G_TYPE_OBJECT, nullptr));
    auto values = new CsvTransactionPreviewRow { std::move (cells), std::move (error), skipped };
    g_object_set_data_full (row, TRANS_PREVIEW_ROW_DATA, values,
                            [] (gpointer data) { delete static_cast<CsvTransactionPreviewRow*> (data); });
    return row;
}

static CsvTransactionPreviewRow*
csv_tximp_preview_row_get (GObject *row)
{
    return static_cast<CsvTransactionPreviewRow*> (g_object_get_data (row, TRANS_PREVIEW_ROW_DATA));
}

static std::string
csv_tximp_preview_error (const ErrMap& errors, bool skipped)
{
    auto non_account_error = [] (const ErrPair& error)
    {
        return error.first != GncTransPropType::ACCOUNT && error.first != GncTransPropType::TACCOUNT;
    };
    if (skipped || !std::any_of (errors.cbegin (), errors.cend (), non_account_error))
        return {};

    auto message = std::string (_("This line has the following parse issues:"));
    for (const auto& error : errors)
        if (non_account_error (error))
            message += "\n• " + error.second;
    return message;
}

static void
csv_tximp_preview_item_setup (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto column = GPOINTER_TO_UINT (user_data);
    GtkWidget *child = column == G_MAXUINT ? gtk_image_new () : gtk_label_new (nullptr);
    (void)factory;
    if (column != G_MAXUINT)
    {
        gtk_label_set_xalign (GTK_LABEL (child), 0.0);
        gtk_label_set_ellipsize (GTK_LABEL (child), PANGO_ELLIPSIZE_END);
        gtk_widget_add_css_class (child, "monospace");
    }
    gtk_list_item_set_child (item, child);
}

static void
csv_tximp_preview_item_bind (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto row = csv_tximp_preview_row_get (G_OBJECT (gtk_list_item_get_item (item)));
    auto column = GPOINTER_TO_UINT (user_data);
    auto child = gtk_list_item_get_child (item);
    (void)factory;
    if (column == G_MAXUINT)
    {
        gtk_image_set_from_icon_name (GTK_IMAGE (child), row->error.empty () || row->skipped ? nullptr : "dialog-error");
        gtk_widget_set_tooltip_text (child, row->error.empty () ? nullptr : row->error.c_str ());
        return;
    }
    const auto& value = column < row->cells.size () ? row->cells.at (column) : std::string ();
    auto escaped = g_markup_escape_text (value.c_str (), -1);
    if (!row->error.empty () && !row->skipped)
    {
        auto markup = g_strdup_printf ("<span foreground=\"black\" background=\"pink\">%s</span>", escaped);
        gtk_label_set_markup (GTK_LABEL (child), markup);
        g_free (markup);
    }
    else if (row->skipped)
    {
        auto markup = g_strdup_printf ("<span strikethrough=\"true\">%s</span>", escaped);
        gtk_label_set_markup (GTK_LABEL (child), markup);
        g_free (markup);
    }
    else
        gtk_label_set_text (GTK_LABEL (child), value.c_str ());
    g_free (escaped);
    gtk_widget_set_tooltip_text (child, row->error.empty () ? nullptr : row->error.c_str ());
}

static void
csv_tximp_preview_add_column (GtkColumnView *view, const gchar *title, guint column)
{
    auto factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (factory, "setup", G_CALLBACK (csv_tximp_preview_item_setup), GUINT_TO_POINTER (column));
    g_signal_connect (factory, "bind", G_CALLBACK (csv_tximp_preview_item_bind), GUINT_TO_POINTER (column));
    auto view_column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (view_column, column != G_MAXUINT);
    gtk_column_view_append_column (view, view_column);
    g_object_unref (view_column);
}

static void
csv_tximp_preview_clear_columns (GtkColumnView *view)
{
    auto columns = gtk_column_view_get_columns (view);
    while (g_list_model_get_n_items (columns) > 0)
    {
        auto column = GTK_COLUMN_VIEW_COLUMN (g_list_model_get_item (columns, 0));
        gtk_column_view_remove_column (view, column);
        g_object_unref (column);
    }
}

static void
csv_tximp_remove_children (GtkWidget *widget)
{
    for (auto child = gtk_widget_get_first_child (widget); child; )
    {
        auto next = gtk_widget_get_next_sibling (child);
        gtk_widget_unparent (child);
        child = next;
    }
}

static GtkDropDown*
csv_tximp_preview_column_selector_new (uint32_t column, GncTransPropType selected,
                                       bool multi_split, CsvImpTransAssist *assist)
{
    auto store = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    guint selected_position = 0;
    guint position = 0;
    for (const auto& column_type : gnc_csv_col_type_strs)
    {
        if (sanitize_trans_prop (column_type.first, multi_split) != column_type.first)
            continue;
        auto row = gtk_string_object_new (_(column_type.second));
        g_object_set_data (G_OBJECT (row), TRANS_COLUMN_TYPE_DATA,
                           GINT_TO_POINTER (static_cast<int> (column_type.first)));
        g_list_store_append (store, row);
        g_object_unref (row);
        if (column_type.first == selected)
            selected_position = position;
        position++;
    }
    auto expression = gtk_property_expression_new (GTK_TYPE_STRING_OBJECT, nullptr, "string");
    auto dropdown = gnc_gtk_drop_down_new (G_LIST_MODEL (store), expression);
    g_object_set_data (G_OBJECT (dropdown), "col-num", GUINT_TO_POINTER (column));
    gtk_drop_down_set_selected (dropdown, selected_position);
    g_signal_connect (dropdown, "notify::selected", G_CALLBACK (csv_tximp_preview_col_type_changed_cb), assist);
    return dropdown;
}

void CsvImpTransAssist::preview_refresh_table ()
{
    preview_validate_settings ();
    auto store = g_list_store_new (G_TYPE_OBJECT);
    for (const auto& parse_line : tx_imp->m_parsed_lines)
    {
        std::vector<std::string> cells;
        for (const auto& cell : std::get<PL_INPUT> (parse_line))
            cells.emplace_back (cell);
        auto row = csv_tximp_preview_row_new (std::move (cells),
            csv_tximp_preview_error (std::get<PL_ERROR> (parse_line), std::get<PL_SKIP> (parse_line)),
            std::get<PL_SKIP> (parse_line));
        g_list_store_append (store, row);
        g_object_unref (row);
    }
    auto selection = gtk_no_selection_new (G_LIST_MODEL (store));
    gtk_column_view_set_model (preview_view, GTK_SELECTION_MODEL (selection));
    g_object_unref (selection);

    csv_tximp_preview_clear_columns (preview_view);
    csv_tximp_remove_children (GTK_WIDGET (preview_column_selectors));
    csv_tximp_preview_add_column (preview_view, "", G_MAXUINT);
    const auto column_types = tx_imp->column_types ();
    for (uint32_t column = 0; column < column_types.size (); column++)
    {
        auto box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 2);
        auto label = g_strdup_printf (_("Column %u"), column + 1);
        auto title = gtk_label_new (label);
        g_free (label);
        gtk_label_set_xalign (GTK_LABEL (title), 0.0);
        auto selector = csv_tximp_preview_column_selector_new (column, column_types.at (column),
                                                                tx_imp->multi_split (), this);
        gtk_box_append (GTK_BOX (box), title);
        gtk_box_append (GTK_BOX (box), GTK_WIDGET (selector));
        gtk_box_append (preview_column_selectors, box);
        auto column_title = g_strdup_printf (_("Column %u"), column + 1);
        csv_tximp_preview_add_column (preview_view, column_title, column);
        g_free (column_title);
    }

    auto base_account = gnc_account_sel_get_account (GNC_ACCOUNT_SEL (acct_selector));
    if (tx_imp->base_account () != base_account)
    {
        g_signal_handlers_block_by_func (acct_selector, (gpointer) csv_tximp_preview_acct_sel_cb, this);
        gnc_account_sel_set_account (GNC_ACCOUNT_SEL (acct_selector), tx_imp->base_account (), false);
        g_signal_handlers_unblock_by_func (acct_selector, (gpointer) csv_tximp_preview_acct_sel_cb, this);
    }
}

/* Update the preview page based on the current state of the importer.
 * Should be called when settings are changed.
 */
void
CsvImpTransAssist::preview_refresh ()
{
    // Cache skip settings. Updating the widgets one by one
    // triggers a callback the transfers all skip widgets'
    // values to settings. So by the time the next widget value
    // is to be set, that widget's 'new' setting has already been replaced by
    // its old setting preventing us from using it here sensibly.
    // Another solution might have been to delay callbacks from running
    // until after all values are set.
    auto skip_start_lines = tx_imp->skip_start_lines();
    auto skip_end_lines = tx_imp->skip_end_lines();
    auto skip_alt_lines = tx_imp->skip_alt_lines();

    // Set start row
    auto adj = gtk_spin_button_get_adjustment (start_row_spin);
    gtk_adjustment_set_upper (adj, tx_imp->m_parsed_lines.size());
    gtk_spin_button_set_value (start_row_spin, skip_start_lines);

    // Set end row
    adj = gtk_spin_button_get_adjustment (end_row_spin);
    gtk_adjustment_set_upper (adj, tx_imp->m_parsed_lines.size());
    gtk_spin_button_set_value (end_row_spin, skip_end_lines);

    // Set Alternate rows
    gtk_check_button_set_active (GTK_CHECK_BUTTON(skip_alt_rows_button),
            skip_alt_lines);

    // Set multi-split indicator
    gtk_check_button_set_active (GTK_CHECK_BUTTON(multi_split_cbutton),
            tx_imp->multi_split());
    gtk_widget_set_sensitive (acct_selector, !tx_imp->multi_split());

    // Set Import Format
    gtk_check_button_set_active (GTK_CHECK_BUTTON(csv_button),
            (tx_imp->file_format() == GncImpFileFormat::CSV));
    gtk_check_button_set_active (GTK_CHECK_BUTTON(fixed_button),
            (tx_imp->file_format() != GncImpFileFormat::CSV));

    // Set Date & Currency Format and Character encoding
    gtk_drop_down_set_selected (date_format_dropdown, tx_imp->date_format());
    gtk_drop_down_set_selected (currency_format_dropdown, tx_imp->currency_format());
    go_charmap_sel_set_encoding (encselector, tx_imp->encoding().c_str());

    // Handle separator checkboxes and custom field, only relevant if the file format is csv
    // Note we defer the change signal until all buttons have been updated
    // An early update may result in an incomplete tokenize run and that would
    // cause our list of saved column types to be truncated
    if (tx_imp->file_format() == GncImpFileFormat::CSV)
    {
        auto separators = tx_imp->separators();
        auto enable_escape = tx_imp->enable_escape();
        const auto stock_sep_chars = std::string (" \t,:;-");
        for (int i = 0; i < SEP_NUM_OF_TYPES; i++)
        {
            g_signal_handlers_block_by_func (sep_button[i], (gpointer) csv_tximp_preview_sep_button_cb, this);
            gtk_check_button_set_active (sep_button[i],
                separators.find (stock_sep_chars[i]) != std::string::npos);
            g_signal_handlers_unblock_by_func (sep_button[i], (gpointer) csv_tximp_preview_sep_button_cb, this);
        }

        // If there are any other separators in the separators string,
        // add them as custom separators
        auto pos = separators.find_first_of (stock_sep_chars);
        while (!separators.empty() && pos != std::string::npos)
        {
            separators.erase(pos);
            pos = separators.find_first_of (stock_sep_chars);
        }
        g_signal_handlers_block_by_func (custom_cbutton, (gpointer) csv_tximp_preview_sep_button_cb, this);
        g_signal_handlers_block_by_func (custom_entry, (gpointer) csv_tximp_preview_sep_button_cb, this);
        gtk_check_button_set_active (custom_cbutton,
                                      !separators.empty());
        gnc_entry_set_text (GTK_ENTRY(custom_entry), separators.c_str());
        g_signal_handlers_block_by_func (escape_cbutton, (gpointer) csv_tximp_preview_sep_button_cb, this);
        gtk_check_button_set_active (escape_cbutton,
                                      enable_escape);
        g_signal_handlers_unblock_by_func (escape_cbutton, (gpointer) csv_tximp_preview_sep_button_cb, this);
        g_signal_handlers_unblock_by_func (custom_cbutton, (gpointer) csv_tximp_preview_sep_button_cb, this);
        g_signal_handlers_unblock_by_func (custom_entry, (gpointer) csv_tximp_preview_sep_button_cb, this);
        try
        {
            tx_imp->tokenize (false);
        }
        catch(std::range_error& err)
        {
            PERR("CSV Tokenization Failed: %s", err.what());
        }
    }

    // Repopulate the parsed data table
    preview_refresh_table ();
}

/* Check if all selected data can be parsed sufficiently to continue
 */
void CsvImpTransAssist::preview_validate_settings ()
{
    /* Allow the user to proceed only if there are no inconsistencies in the settings */
    auto has_non_acct_errors = !tx_imp->verify (false).empty();
    auto error_msg = tx_imp->verify (m_req_mapped_accts);
    gnc_import_assistant_set_page_complete (csv_imp_asst, preview_page, !has_non_acct_errors);
    gtk_label_set_markup(GTK_LABEL(instructions_label), error_msg.c_str());
    gtk_widget_set_visible (GTK_WIDGET(instructions_image), !error_msg.empty());

    /* Show or hide the account match page based on whether there are
     * accounts in the user data according to the importer configuration
     * only if there are no errors
     */
    if (!has_non_acct_errors)
        gtk_widget_set_visible (GTK_WIDGET(account_match_page),
                !tx_imp->accounts().empty());
}


/**************************************************
 * Code related to the account match page
 **************************************************/

/* Populates the account match view with all potential
 * account names found in the parse data.
 */
void CsvImpTransAssist::acct_match_set_accounts ()
{
    g_list_store_remove_all (account_match_store);

    auto accts = tx_imp->accounts();
    for (const auto& acct : accts)
    {
        auto row = csv_tximp_account_match_row_new (acct);
        g_list_store_append (account_match_store, row);
        g_object_unref (row);
    }
}

static void
csv_tximp_acct_match_load_mappings (GListModel *mappings_model)
{
    for (guint position = 0; position < g_list_model_get_n_items (mappings_model); position++)
    {
        auto item = G_OBJECT (g_list_model_get_item (mappings_model, position));
        auto row = csv_tximp_account_match_row_get (item);
        auto account = row->account;

        // Look for an account matching the imported mapping string.
        // It may already be set in the row. If not we try to match it with
        // - an entry in our saved account maps
        // - a full name of any of our existing accounts
        if (account ||
            (account = gnc_account_imap_find_any (gnc_get_current_book(), IMAP_CAT_CSV, row->mapping.c_str ())) ||
            (account = gnc_account_lookup_by_full_name (gnc_get_current_root_account(), row->mapping.c_str ())))
        {
            auto fullpath = gnc_account_get_full_name (account);
            row->fullpath = fullpath;
            row->account = account;
            g_free (fullpath);
        }
        g_object_unref (item);
    }
}

static bool
csv_tximp_acct_match_check_all (GListModel *model)
{
    for (guint position = 0; position < g_list_model_get_n_items (model); position++)
    {
        auto item = G_OBJECT (g_list_model_get_item (model, position));
        auto account = csv_tximp_account_match_row_get (item)->account;
        g_object_unref (item);
        if (!account)
            return false;
    }
    return true;
}


/* Evaluate acct_name as a full account name. Try if it
 * contains a path to an existing parent account. If not,
 * alter the full path name to use a fake separator to
 * avoid calling multiple new account windows for each
 * non-existent parent account.
 */
static std::string
csv_tximp_acct_match_text_parse (std::string acct_name)
{
    auto sep = gnc_get_account_separator_string ();
    auto sep_pos = acct_name.rfind(sep);
    if (sep_pos == std::string::npos)
        // No separators found in acct_name -> return as is
        return acct_name;

    auto parent = acct_name.substr(0, sep_pos);
    auto root = gnc_get_current_root_account ();

    if (gnc_account_lookup_by_full_name (root, parent.c_str()))
        // acct_name's parent matches an existing account -> acct_name as is
        return acct_name;
    else
    {
        // Acct name doesn't match an existing account
        // -> return the name with a fake separator to avoid
        // asking the user to create each intermediary account as well
        const gchar *alt_sep;
        if (g_strcmp0 (sep,":") == 0)
            alt_sep = "-";
        else
            alt_sep = ":";
        for (sep_pos = acct_name.find(sep); sep_pos != std::string::npos;
                sep_pos = acct_name.find(sep))
            acct_name.replace (sep_pos, strlen(sep), alt_sep);
        return acct_name;
    }
}

struct CsvAccountSelection
{
    CsvImpTransAssist *info;
    GWeakRef assistant;
    GObject *row;
};

static void
csv_account_selected_cb (Account *account, gboolean accepted, gpointer user_data)
{
    auto selection = static_cast<CsvAccountSelection*> (user_data);
    auto assistant = G_OBJECT (g_weak_ref_get (&selection->assistant));
    if (assistant && accepted &&
        g_object_get_data (assistant, "gnc-csv-import-assistant-owner") == selection->info)
        selection->info->acct_match_apply_selection (selection->row, account);
    g_clear_object (&assistant);
    g_weak_ref_clear (&selection->assistant);
    g_clear_object (&selection->row);
    delete selection;
}

void
CsvImpTransAssist::acct_match_apply_selection (GObject *item, Account *gnc_acc)
{
    auto row = csv_tximp_account_match_row_get (item);
    auto account = row->account;

    if (gnc_acc)
    {
        auto fullpath = gnc_account_get_full_name (gnc_acc);
        row->account = gnc_acc;
        row->fullpath = fullpath;

        // Update the account kvp mappings
        if (!row->mapping.empty ())
        {
            gnc_account_imap_delete_account (account, IMAP_CAT_CSV, row->mapping.c_str ());
            gnc_account_imap_add_account (gnc_acc, IMAP_CAT_CSV, row->mapping.c_str (), gnc_acc);
        }

        // Force reparsing of account columns - may impact multi-currency mode
        auto col_types = tx_imp->column_types();
        auto col_type_it = std::find (col_types.cbegin(),
                                      col_types.cend(), GncTransPropType::ACCOUNT);
        if (col_type_it != col_types.cend())
            tx_imp->set_column_type(col_type_it - col_types.cbegin(),
                                    GncTransPropType::ACCOUNT, true);
        col_type_it = std::find (col_types.cbegin(),
                                 col_types.cend(), GncTransPropType::TACCOUNT);
        if (col_type_it != col_types.cend())
            tx_imp->set_column_type(col_type_it - col_types.cbegin(),
                                    GncTransPropType::TACCOUNT, true);

        g_free (fullpath);
    }

    /* Enable the "Next" Assistant Button */
    auto all_checked = csv_tximp_acct_match_check_all (G_LIST_MODEL (account_match_store));
    gnc_import_assistant_set_page_complete (csv_imp_asst, account_match_page,
                                     all_checked);

    /* Update information message and whether to display account errors */
    m_req_mapped_accts = all_checked;
    auto errs = tx_imp->verify(m_req_mapped_accts);
    gtk_label_set_text (GTK_LABEL(account_match_label), errs.c_str());

    auto count = g_list_model_get_n_items (G_LIST_MODEL (account_match_store));
    for (guint position = 0; position < count; ++position)
    {
        auto current = G_OBJECT (g_list_model_get_item (G_LIST_MODEL (account_match_store), position));
        auto matches = current == item;
        g_object_unref (current);
        if (matches)
        {
            g_list_model_items_changed (G_LIST_MODEL (account_match_store), position, 1, 1);
            break;
        }
    }
}

void
CsvImpTransAssist::acct_match_select (GObject *item)
{
    auto row = csv_tximp_account_match_row_get (item);
    auto selection = new CsvAccountSelection { this, {}, G_OBJECT (g_object_ref (item)) };
    auto acct_name = csv_tximp_acct_match_text_parse (row->mapping);
    g_weak_ref_init (&selection->assistant, csv_imp_asst);
    gnc_import_select_account_async (GTK_WIDGET (csv_imp_asst), nullptr, true,
        acct_name.c_str(), nullptr, ACCT_TYPE_NONE, row->account,
        csv_account_selected_cb, selection);
}

void
CsvImpTransAssist::acct_match_select_at (guint position)
{
    auto item = G_OBJECT (g_list_model_get_item (G_LIST_MODEL (account_match_store), position));
    if (!item)
        return;
    acct_match_select (item);
    g_object_unref (item);
}

void
CsvImpTransAssist::acct_match_via_button ()
{
    auto position = gtk_single_selection_get_selected (account_match_selection);
    if (position != GTK_INVALID_LIST_POSITION)
        acct_match_select_at (position);
}


/*******************************************************
 * Assistant page prepare functions
 *******************************************************/

void
CsvImpTransAssist::assist_file_page_prepare ()
{
    gtk_label_set_text (GTK_LABEL (file_name_label),
                        m_fc_file_name.empty () ? _("No file selected")
                                               : m_fc_file_name.c_str());
    gnc_import_assistant_set_page_complete (csv_imp_asst, file_page,
                                     !m_fc_file_name.empty ());
    gnc_import_assistant_set_page_complete (csv_imp_asst, account_match_page, false);
}


void
CsvImpTransAssist::assist_preview_page_prepare ()
{
    auto go_back = false;

    if (m_final_file_name != m_fc_file_name)
    {
        tx_imp = std::unique_ptr<GncTxImport>(new GncTxImport);

        /* Assume data is CSV. User can later override to Fixed Width if needed */
        try
        {
            tx_imp->file_format (GncImpFileFormat::CSV);
            tx_imp->load_file (m_fc_file_name);
            tx_imp->tokenize (true);
            m_req_mapped_accts = false;

            /* Get settings store and populate */
            preview_populate_settings_combo();
            gtk_drop_down_set_selected (settings_dropdown, 0);

            /* Disable the "Next" Assistant Button */
            gnc_import_assistant_set_page_complete (csv_imp_asst, preview_page, false);
        }
        catch (std::ifstream::failure& e)
        {
            /* File loading failed ... */
            gnc_error_dialog (GTK_WINDOW (csv_imp_asst), "%s", e.what());
            go_back = true;
        }
        catch (std::range_error &e)
        {
            /* Parsing failed ... */
            gnc_error_dialog (GTK_WINDOW (csv_imp_asst), "%s", _(e.what()));
            /* Stay in this step so user can override */
            go_back = false;
        }
    }

    if (go_back)
        gnc_import_assistant_previous_page (csv_imp_asst);
    else
    {
        m_final_file_name = m_fc_file_name;
        preview_refresh ();

        /* Populate the GTK4 column view after the page becomes active. */
        preview_queue_refresh_table ();
    }
}
void
CsvImpTransAssist::assist_account_match_page_prepare ()
{

    // Load the account strings into the store
    acct_match_set_accounts ();

    // Match the account strings to account maps from previous imports
    csv_tximp_acct_match_load_mappings (G_LIST_MODEL (account_match_store));
    auto row_count = g_list_model_get_n_items (G_LIST_MODEL (account_match_store));
    if (row_count)
        g_list_model_items_changed (G_LIST_MODEL (account_match_store), 0, row_count, row_count);

    // Enable the view, possibly after an error
    gtk_widget_set_sensitive (GTK_WIDGET (account_match_view), true);
    gtk_widget_set_sensitive (account_match_btn, true);

    /* Enable the "Next" Assistant Button */
    auto all_checked = csv_tximp_acct_match_check_all (G_LIST_MODEL (account_match_store));
    gnc_import_assistant_set_page_complete (csv_imp_asst, account_match_page,
                                     all_checked);

    /* Update information message and whether to display account errors */
    m_req_mapped_accts = all_checked;
    auto text = tx_imp->verify (m_req_mapped_accts);
    gtk_label_set_text (GTK_LABEL(account_match_label), text.c_str());
}


void
CsvImpTransAssist::assist_doc_page_prepare ()
{
    if (!tx_imp->verify (true).empty())
    {
        /* New accounts can change the multi-currency situation and hence
         * may require more column tweaks. If so
         * inform the user and go back to the preview page.
         */
        gnc_import_assistant_set_current_page (csv_imp_asst, 2);
        return;
    }

    /* Do not create transactions while the new-book options window is open.
     * The weak window reference and book identity keep the continuation from
     * touching a closed assistant or a replacement book. */
    if (new_book)
    {
        struct CsvTxNewBookRequest
        {
            GWeakRef assistant;
            QofBook *book;
        };
        auto request = new CsvTxNewBookRequest{};
        g_weak_ref_init (&request->assistant, GTK_WIDGET (csv_imp_asst));
        request->book = gnc_get_current_book ();
        gnc_import_assistant_set_page_complete (csv_imp_asst, doc_page, false);
        gnc_new_book_option_display_async (
            GTK_WIDGET (csv_imp_asst),
            [] (GtkWindow *parent, gboolean applied, gpointer user_data)
            {
                auto request = static_cast<CsvTxNewBookRequest *> (user_data);
                auto assistant = GTK_WIDGET (g_weak_ref_get (&request->assistant));
                auto info = assistant ? static_cast<CsvImpTransAssist *> (
                    g_object_get_data (G_OBJECT (assistant),
                                       "gnc-csv-import-assistant-owner")) : nullptr;
                if (info)
                    info->new_book_options_finished (applied, request->book);
                g_clear_object (&assistant);
                g_weak_ref_clear (&request->assistant);
                delete request;
                (void)parent;
            }, request);
        return;
    }

    /* Block going back only after all account and book settings are valid. */
    gnc_import_assistant_commit (csv_imp_asst);

    /* The shared window controller keeps one Cancel control visible while
     * committed pages suppress only Back. Do not add a duplicate matcher
     * button; summary cleanup remains harmless with the null pointer. */
}

void
CsvImpTransAssist::new_book_options_finished (gboolean applied, QofBook *book)
{
    if (!applied || book != gnc_get_current_book () || qof_book_shutting_down (book))
        return;
    new_book = false;
    gnc_import_assistant_set_page_complete (csv_imp_asst, doc_page, true);
}


void
CsvImpTransAssist::assist_match_page_prepare ()
{
    /* Create transactions from the parsed data */
    try
    {
        tx_imp->create_transactions ();
    }
    catch (const GncCsvImpParseError& err)
    {
        /* Oops! This shouldn't happen when using the import assistant !
         * Inform the user and go back to the preview page.
         */
        auto err_msg = std::string(err.what());
        auto err_msgs = err.errors();
        auto add_bullet_item = [](std::string&& a, ErrMap::value_type& b)->std::string { return std::move(a) + "\n• " + b.second; };
        err_msg = std::accumulate (err_msgs.begin(), err_msgs.end(), std::move (err_msg), add_bullet_item);

        gnc_error_dialog (GTK_WINDOW (csv_imp_asst),
            _("An unexpected error has occurred while creating transactions. Please report this as a bug.\n\n"
              "Error message:\n%s"), err_msg.c_str());
        gnc_import_assistant_set_current_page (csv_imp_asst, 2);
    }

    /* Block going back */
    gnc_import_assistant_commit (csv_imp_asst);

    auto text = std::string( "<span size=\"medium\" color=\"red\"><b>");
    text += _("Double click on rows to change, then click on Apply to Import");
    text += "</b></span>";
    gtk_label_set_markup (GTK_LABEL(match_label), text.c_str());

    /* Add the help button for the matcher */
    help_button = gtk_button_new_with_mnemonic (_("_Help"));
    gnc_import_assistant_add_action_widget (csv_imp_asst, help_button);
    g_signal_connect (help_button, "clicked",
                     G_CALLBACK(on_matcher_help_clicked), gnc_csv_importer_gui);

    gtk_widget_set_visible (GTK_WIDGET(help_button), true);

    /* Copy all of the transactions to the importer GUI. */
    for (auto trans_it : tx_imp->m_transactions)
    {
        auto draft_trans = trans_it.second;
        if (draft_trans->trans)
        {
            auto lsplit = GNCImportLastSplitInfo {
                draft_trans->m_price ? static_cast<gnc_numeric>(*draft_trans->m_price) : gnc_numeric{0, 0},
                draft_trans->m_taction ? draft_trans->m_taction->c_str() : nullptr,
                draft_trans->m_tmemo ? draft_trans->m_tmemo->c_str() : nullptr,
                draft_trans->m_tamount ? static_cast<gnc_numeric>(*draft_trans->m_tamount) : gnc_numeric{0, 0},
                draft_trans->m_taccount ? *draft_trans->m_taccount : nullptr,
                draft_trans->m_trec_state ? *draft_trans->m_trec_state : '\0',
                draft_trans->m_trec_date ? static_cast<time64>(GncDateTime(*draft_trans->m_trec_date, DayPart::neutral)) : 0,
            };

//A tramsaction with no splits is invalid and will crash later.
            if (xaccTransGetSplit(draft_trans->trans, 0))
                gnc_gen_trans_list_add_trans_with_split_data (gnc_csv_importer_gui, std::move (draft_trans->trans),
                                                              &lsplit);
            else
                xaccTransDestroy(draft_trans->trans);
            draft_trans->trans = nullptr;
        }
    }
    /* Show the matcher dialog */
    gnc_gen_trans_list_show_all (gnc_csv_importer_gui);
}


void
CsvImpTransAssist::assist_summary_page_prepare ()
{
    /* Remove the added buttons */
    gnc_import_assistant_remove_action_widget (csv_imp_asst, help_button);
    gnc_import_assistant_remove_action_widget (csv_imp_asst, cancel_button);

    auto text = std::string("<span size=\"medium\"><b>");
    try
    {
    /* Translators: {1} will be replaced with a filename */
        text += (bl::format (std::string{_("The transactions were imported from file '{1}'.")}) % m_final_file_name).str();
        text += "</b></span>";
    }
    catch (const bl::conv::conversion_error& err)
    {
        PERR("Transcoding error: %s", err.what());
        text += "The transactions were imported from the file.</b></span>";
    }
    catch (const bl::conv::invalid_charset_error& err)
    {
        PERR("Invalid charset error: %s", err.what());
        text += "The transactions were imported from the file.</b></span>";
    }
    gtk_label_set_markup (GTK_LABEL(summary_label), text.c_str());
}


void
CsvImpTransAssist::assist_prepare_cb (GtkWidget *page)
{
    if (page == file_page)
        assist_file_page_prepare ();
    else if (page == preview_page)
        assist_preview_page_prepare ();
    else if (page == account_match_page)
        assist_account_match_page_prepare ();
    else if (page == doc_page)
        assist_doc_page_prepare ();
    else if (page == match_page)
        assist_match_page_prepare ();
    else if (page == summary_page)
        assist_summary_page_prepare ();
}


void
CsvImpTransAssist::assist_finish ()
{
    /* Start the import */
    if (!tx_imp->m_transactions.empty())
    {
        /* The call to gnc_gen_trans_assist_start below will free the
         * object passed into it. To avoid our c++ destructor from
         * attempting a second free on that object, we'll release
         * our own reference to it here  before passing it to
         * gnc_gen_trans_assist_start.
         */
        auto local_csv_imp_gui = gnc_csv_importer_gui;
        gnc_csv_importer_gui = nullptr;
        gnc_gen_trans_assist_start (local_csv_imp_gui);
        gnc_import_assistant_set_current_page (csv_imp_asst, 6);
    }
}


void
CsvImpTransAssist::assist_compmgr_close ()
{
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(csv_imp_asst));
}


static void
csv_tximp_close_handler (gpointer user_data)
{
    auto info = (CsvImpTransAssist*)user_data;
    gnc_unregister_gui_component_by_data (ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS, info);
    info->assist_compmgr_close();
    delete info;
}

/********************************************************************\
 * gnc_file_csv_trans_import                                        *
 * opens up a assistant to import accounts.                         *
 *                                                                  *
 * Args:   import_type                                              *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_file_csv_trans_import(void)
{
    auto info = new CsvImpTransAssist;
    gnc_register_gui_component (ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS,
                                nullptr, csv_tximp_close_handler,
                                info);
}
