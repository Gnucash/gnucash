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

extern "C"
{
#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <stdlib.h>

#include "gnc-path.h"
#include "gnc-ui.h"
#include "gnc-uri-utils.h"
#include "gnc-ui-util.h"
#include "dialog-utils.h"

#include "gnc-component-manager.h"

#include "gnc-state.h"

#include "assistant-csv-trans-import.h"

#include "import-account-matcher.h"
#include "import-main-matcher.h"
#include "gnc-csv-account-map.h"
#include "gnc-account-sel.h"

#include "gnc-csv-gnumeric-popup.h"
#include "go-charmap-sel.h"
}

#include "gnc-imp-settings-csv-tx.hpp"
#include "gnc-import-tx.hpp"
#include "gnc-tokenizer-fw.hpp"
#include "gnc-tokenizer-csv.hpp"

#include <boost/locale.hpp>

namespace bl = boost::locale;

#define MIN_COL_WIDTH 70
#define GNC_PREFS_GROUP "dialogs.import.csv"
#define ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS "assistant-csv-trans-import"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ASSISTANT;


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
 *   GtkAssistant, and a few C++ member objects.
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
 * - Let's zoom in on the c++ member objects and in particular the GtkAssistant and related objects.
 *   These are created the gtk way in the c++ class constructor. That means the main GtkAssistant widget
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
    void assist_compmgr_close ();

    void file_confirm_cb ();

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
    void preview_update_col_type (GtkComboBox* cbox);
    void preview_update_fw_columns (GtkTreeView* treeview, GdkEventButton* event);

    void preview_populate_settings_combo();
    void preview_handle_save_del_sensitivity (GtkComboBox* combo);
    void preview_split_column (int col, int offset);
    void preview_refresh_table ();
    void preview_refresh ();
    void preview_validate_settings ();

    void acct_match_via_button ();
    bool acct_match_via_view_dblclick (GdkEventButton *event);
    void acct_match_select(GtkTreeModel *model, GtkTreeIter* iter);
    void acct_match_set_accounts ();

    friend gboolean
    fixed_context_menu_handler (GnumericPopupMenuElement const *element,
            gpointer userdata);
private:
    /* helper functions to manage the context menu for fixed with columns */
    uint32_t get_new_col_rel_pos (GtkTreeViewColumn *tcol, int dx);
    void fixed_context_menu (GdkEventButton *event, int col, int dx);
    /* helper function to calculate row colors for the preview table (to visualize status) */
    void preview_row_fill_state_cells (GtkListStore *store, GtkTreeIter *iter,
            std::string& err_msg, bool skip);
    /* helper function to create preview header cell combo boxes listing available column types */
    GtkWidget* preview_cbox_factory (GtkTreeModel* model, uint32_t colnum);
    /* helper function to set rendering parameters for preview data columns */
    void preview_style_column (uint32_t col_num, GtkTreeModel* model);

    GtkAssistant    *csv_imp_asst;

    GtkWidget       *file_page;                     /**< Assistant file page widget */
    GtkWidget       *file_chooser;                  /**< The widget for the file chooser */
    std::string      m_file_name;                     /**< The import file name */

    GtkWidget       *preview_page;                  /**< Assistant preview page widget */
    GtkComboBox     *settings_combo;                /**< The Settings Combo */
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
    GtkWidget       *fw_instructions_hbox;          /**< Container for fixed-width instructions */
    GtkCheckButton  *custom_cbutton;                /**< The checkbutton for a custom separator */
    GtkEntry        *custom_entry;                  /**< The entry for custom separators */
    GtkComboBoxText *date_format_combo;             /**< The Combo Text widget for selecting the date format */
    GtkComboBoxText *currency_format_combo;         /**< The Combo Text widget for selecting the currency format */
    GtkTreeView     *treeview;                      /**< The treeview containing the data */
    GtkLabel        *instructions_label;            /**< The instructions label */
    GtkImage        *instructions_image;            /**< The instructions image */
    bool             encoding_selected_called;      /**< Before encoding_selected is first called, this is false.
                                                       * error lines, instead of all the file data. */
    int              fixed_context_col;             /**< The number of the column the user has clicked */
    int              fixed_context_offset;          /**< The offset (in characters) in the column
                                                       * the user has clicked */

    GtkWidget            *account_match_page;       /**< Assistant account matcher page widget */
    GtkWidget            *account_match_view;       /**< Assistant account matcher view widget */
    GtkWidget            *account_match_label;      /**< Assistant account matcher label widget */
    GtkWidget            *account_match_btn;        /**< Assistant account matcher button widget */

    GtkWidget            *doc_page;                 /**< Assistant doc page widget */

    GtkWidget            *match_page;               /**< Assistant match page widget, to be packed with the transaction matcher */
    GtkWidget            *match_label;              /**< The match label at the bottom of the page */
    GNCImportMainMatcher *gnc_csv_importer_gui;     /**< The GNCImportMainMatcher structure */
    GtkWidget            *help_button;              /**< The widget for the help button on the matcher page */
    GtkWidget            *cancel_button;            /**< The widget for the new cancel button when going back is blocked */

    GtkWidget            *summary_page;             /**< Assistant summary page widget */
    GtkWidget            *summary_label;            /**< The summary text */

    bool                  new_book;                 /**< Are we importing into a new book?; if yes, call book options */
    std::unique_ptr<GncTxImport> tx_imp;            /**< The actual data we are previewing */
};


/*******************************************************
 * Assistant call back functions
 *******************************************************/

extern "C"
{
void csv_tximp_assist_prepare_cb (GtkAssistant  *assistant, GtkWidget *page, CsvImpTransAssist* info);
void csv_tximp_assist_close_cb (GtkAssistant *gtkassistant, CsvImpTransAssist* info);
void csv_tximp_assist_finish_cb (GtkAssistant *gtkassistant, CsvImpTransAssist* info);
void csv_tximp_file_confirm_cb (GtkWidget *button, CsvImpTransAssist *info);
void csv_tximp_preview_del_settings_cb (GtkWidget *button, CsvImpTransAssist *info);
void csv_tximp_preview_save_settings_cb (GtkWidget *button, CsvImpTransAssist *info);
void csv_tximp_preview_settings_sel_changed_cb (GtkComboBox *combo, CsvImpTransAssist *info);
void csv_tximp_preview_settings_text_inserted_cb (GtkEditable *entry, gchar *new_text,
        gint new_text_length, gint *position, CsvImpTransAssist *info);
void csv_tximp_preview_settings_text_changed_cb (GtkEntry *entry, CsvImpTransAssist *info);
void csv_tximp_preview_srow_cb (GtkSpinButton *spin, CsvImpTransAssist *info);
void csv_tximp_preview_erow_cb (GtkSpinButton *spin, CsvImpTransAssist *info);
void csv_tximp_preview_skiprows_cb (GtkToggleButton *checkbox, CsvImpTransAssist *info);
void csv_tximp_preview_skiperrors_cb (GtkToggleButton *checkbox, CsvImpTransAssist *info);
void csv_tximp_preview_multisplit_cb (GtkToggleButton *checkbox, CsvImpTransAssist *info);
void csv_tximp_preview_sep_button_cb (GtkWidget* widget, CsvImpTransAssist* info);
void csv_tximp_preview_sep_fixed_sel_cb (GtkToggleButton* csv_button, CsvImpTransAssist* info);
void csv_tximp_preview_acct_sel_cb (GtkWidget* widget, CsvImpTransAssist* info);
void csv_tximp_preview_enc_sel_cb (GOCharmapSel* selector, const char* encoding,
                              CsvImpTransAssist* info);
void csv_tximp_acct_match_button_clicked_cb (GtkWidget *widget, CsvImpTransAssist* info);
bool csv_tximp_acct_match_view_clicked_cb (GtkWidget *widget, GdkEventButton *event, CsvImpTransAssist* info);
}

void
csv_tximp_assist_prepare_cb (GtkAssistant *assistant, GtkWidget *page,
        CsvImpTransAssist* info)
{
    info->assist_prepare_cb(page);
}

void
csv_tximp_assist_close_cb (GtkAssistant *assistant, CsvImpTransAssist* info)
{
    gnc_close_gui_component_by_data (ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS, info);
}

void
csv_tximp_assist_finish_cb (GtkAssistant *assistant, CsvImpTransAssist* info)
{
    info->assist_finish ();
}


void csv_tximp_file_confirm_cb (GtkWidget *button, CsvImpTransAssist *info)
{
    info->file_confirm_cb();
}

void csv_tximp_preview_del_settings_cb (GtkWidget *button, CsvImpTransAssist *info)
{
    info->preview_settings_delete();
}

void csv_tximp_preview_save_settings_cb (GtkWidget *button, CsvImpTransAssist *info)
{
    info->preview_settings_save();
}

void csv_tximp_preview_settings_sel_changed_cb (GtkComboBox *combo, CsvImpTransAssist *info)
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

void csv_tximp_preview_skiprows_cb (GtkToggleButton *checkbox, CsvImpTransAssist *info)
{
    info->preview_update_skipped_rows();
}

void csv_tximp_preview_skiperrors_cb (GtkToggleButton *checkbox, CsvImpTransAssist *info)
{
    info->preview_update_skipped_rows();
}

void csv_tximp_preview_multisplit_cb (GtkToggleButton *checkbox, CsvImpTransAssist *info)
{
    info->preview_multi_split (gtk_toggle_button_get_active (checkbox));
}

void csv_tximp_preview_sep_button_cb (GtkWidget* widget, CsvImpTransAssist* info)
{
    info->preview_update_separators(widget);
}

void csv_tximp_preview_sep_fixed_sel_cb (GtkToggleButton* csv_button, CsvImpTransAssist* info)
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

static void csv_tximp_preview_date_fmt_sel_cb (GtkComboBox* format_selector, CsvImpTransAssist* info)
{
    info->preview_update_date_format();
}

static void csv_tximp_preview_currency_fmt_sel_cb (GtkComboBox* format_selector, CsvImpTransAssist* info)
{
    info->preview_update_currency_format();
}

static void csv_tximp_preview_col_type_changed_cb (GtkComboBox* cbox, CsvImpTransAssist* info)
{
    info->preview_update_col_type (cbox);
}

static bool
csv_tximp_preview_treeview_clicked_cb (GtkTreeView* treeview, GdkEventButton* event,
                                        CsvImpTransAssist* info)
{
    info->preview_update_fw_columns(treeview, event);
    return false;
}


void csv_tximp_acct_match_button_clicked_cb (GtkWidget *widget, CsvImpTransAssist* info)
{
    info->acct_match_via_button();
}

bool csv_tximp_acct_match_view_clicked_cb (GtkWidget *widget, GdkEventButton *event, CsvImpTransAssist* info)
{
    return info->acct_match_via_view_dblclick(event);
}


/*******************************************************
 * Assistant Constructor
 *******************************************************/
CsvImpTransAssist::CsvImpTransAssist ()
{
    auto builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "start_row_adj");
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "end_row_adj");
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "account_match_store");
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "csv_transaction_assistant");
    csv_imp_asst = GTK_ASSISTANT(gtk_builder_get_object (builder, "csv_transaction_assistant"));

    // Set the style context for this assistant so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(csv_imp_asst), "GncAssistTransImport");

    /* Enable buttons on all page. */
    gtk_assistant_set_page_complete (csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "start_page")),
                                     true);
    gtk_assistant_set_page_complete (csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "file_page")),
                                     false);
    gtk_assistant_set_page_complete (csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "preview_page")),
                                     false);
    gtk_assistant_set_page_complete (csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "account_match_page")),
                                     false);
    gtk_assistant_set_page_complete (csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "doc_page")),
                                     true);
    gtk_assistant_set_page_complete (csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "match_page")),
                                     true);
    gtk_assistant_set_page_complete (csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "summary_page")),
                                     true);

    /* File chooser Page */
    file_page = GTK_WIDGET(gtk_builder_get_object (builder, "file_page"));
    file_chooser = gtk_file_chooser_widget_new (GTK_FILE_CHOOSER_ACTION_OPEN);
    g_signal_connect (G_OBJECT(file_chooser), "file-activated",
                      G_CALLBACK(csv_tximp_file_confirm_cb), this);
    auto button = gtk_button_new_with_mnemonic (_("_OK"));
    gtk_widget_set_size_request (button, 100, -1);
    gtk_widget_show (button);
    auto h_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (h_box), TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET(h_box), TRUE);
    gtk_box_pack_start (GTK_BOX(h_box), button, FALSE, FALSE, 0);
    gtk_file_chooser_set_extra_widget (GTK_FILE_CHOOSER(file_chooser), h_box);
    g_signal_connect (G_OBJECT(button), "clicked",
                      G_CALLBACK(csv_tximp_file_confirm_cb), this);

    auto box = GTK_WIDGET(gtk_builder_get_object (builder, "file_page"));
    gtk_box_pack_start (GTK_BOX(box), file_chooser, TRUE, TRUE, 6);
    gtk_widget_show (file_chooser);

    /* Preview Settings Page */
    {
        preview_page = GTK_WIDGET(gtk_builder_get_object (builder, "preview_page"));

        // Add Settings combo
        auto settings_store = gtk_list_store_new (2, G_TYPE_POINTER, G_TYPE_STRING);
        settings_combo = GTK_COMBO_BOX(gtk_combo_box_new_with_model_and_entry (GTK_TREE_MODEL(settings_store)));
        gtk_combo_box_set_entry_text_column (GTK_COMBO_BOX(settings_combo), SET_NAME);
        gtk_combo_box_set_active (GTK_COMBO_BOX(settings_combo), 0);

        combo_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "combo_hbox"));
        gtk_box_pack_start (GTK_BOX(combo_hbox), GTK_WIDGET(settings_combo), true, true, 6);
        gtk_widget_show (GTK_WIDGET(settings_combo));

        g_signal_connect (G_OBJECT(settings_combo), "changed",
                         G_CALLBACK(csv_tximp_preview_settings_sel_changed_cb), this);

        // Additionally connect to the changed signal of the embedded GtkEntry
        auto emb_entry = gtk_bin_get_child (GTK_BIN (settings_combo));
        g_signal_connect (G_OBJECT(emb_entry), "changed",
                         G_CALLBACK(csv_tximp_preview_settings_text_changed_cb), this);
        g_signal_connect (G_OBJECT(emb_entry), "insert-text",
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
                = (GtkCheckButton*)GTK_WIDGET(gtk_builder_get_object (builder, sep_button_names[i]));

        /* Load and connect the custom separator checkbutton in the same way
         * as the other separator buttons. */
        custom_cbutton
            = (GtkCheckButton*)GTK_WIDGET(gtk_builder_get_object (builder, "custom_cbutton"));

        /* Load the entry for the custom separator entry. Connect it to the
         * sep_button_clicked event handler as well. */
        custom_entry = (GtkEntry*)GTK_WIDGET(gtk_builder_get_object (builder, "custom_entry"));

        /* Add account selection widget */
        acct_selector = gnc_account_sel_new();
        auto account_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "account_hbox"));
        gtk_box_pack_start (GTK_BOX(account_hbox), acct_selector, TRUE, TRUE, 6);
        gtk_widget_show (acct_selector);

        g_signal_connect(G_OBJECT(acct_selector), "account_sel_changed",
                         G_CALLBACK(csv_tximp_preview_acct_sel_cb), this);


        /* Create the encoding selector widget and add it to the assistant */
        encselector = GO_CHARMAP_SEL(go_charmap_sel_new(GO_CHARMAP_SEL_TO_UTF8));
        /* Connect the selector to the encoding_selected event handler. */
        g_signal_connect (G_OBJECT(encselector), "charmap_changed",
                         G_CALLBACK(csv_tximp_preview_enc_sel_cb), this);

        auto encoding_container = GTK_CONTAINER(gtk_builder_get_object (builder, "encoding_container"));
        gtk_container_add (encoding_container, GTK_WIDGET(encselector));
        gtk_widget_show_all (GTK_WIDGET(encoding_container));

        /* The instructions label and image */
        instructions_label = GTK_LABEL(gtk_builder_get_object (builder, "instructions_label"));
        instructions_image = GTK_IMAGE(gtk_builder_get_object (builder, "instructions_image"));

        /* Add in the date format combo box and hook it up to an event handler. */
        date_format_combo = GTK_COMBO_BOX_TEXT(gtk_combo_box_text_new());
        for (auto& date_fmt : GncDate::c_formats)
            gtk_combo_box_text_append_text (date_format_combo, _(date_fmt.m_fmt.c_str()));
        gtk_combo_box_set_active (GTK_COMBO_BOX(date_format_combo), 0);
        g_signal_connect (G_OBJECT(date_format_combo), "changed",
                         G_CALLBACK(csv_tximp_preview_date_fmt_sel_cb), this);

        /* Add it to the assistant. */
        auto date_format_container = GTK_CONTAINER(gtk_builder_get_object (builder, "date_format_container"));
        gtk_container_add (date_format_container, GTK_WIDGET(date_format_combo));
        gtk_widget_show_all (GTK_WIDGET(date_format_container));

        /* Add in the currency format combo box and hook it up to an event handler. */
        currency_format_combo = GTK_COMBO_BOX_TEXT(gtk_combo_box_text_new());
        for (int i = 0; i < num_currency_formats; i++)
        {
            gtk_combo_box_text_append_text (currency_format_combo, _(currency_format_user[i]));
        }
        /* Default will the locale */
        gtk_combo_box_set_active (GTK_COMBO_BOX(currency_format_combo), 0);
        g_signal_connect (G_OBJECT(currency_format_combo), "changed",
                         G_CALLBACK(csv_tximp_preview_currency_fmt_sel_cb), this);

        /* Add it to the assistant. */
        auto currency_format_container = GTK_CONTAINER(gtk_builder_get_object (builder, "currency_format_container"));
        gtk_container_add (currency_format_container, GTK_WIDGET(currency_format_combo));
        gtk_widget_show_all (GTK_WIDGET(currency_format_container));

        /* Connect the CSV/Fixed-Width radio button event handler. */
        csv_button = GTK_WIDGET(gtk_builder_get_object (builder, "csv_button"));
        fixed_button = GTK_WIDGET(gtk_builder_get_object (builder, "fixed_button"));

        /* Load the data treeview and connect it to its resizing event handler. */
        treeview = (GtkTreeView*)GTK_WIDGET(gtk_builder_get_object (builder, "treeview"));
        gtk_tree_view_set_headers_clickable (treeview, true);

        /* This is true only after encoding_selected is called, so we must
         * set it initially to false. */
        encoding_selected_called = false;
    }

    /* Account Match Page */
    account_match_page  = GTK_WIDGET(gtk_builder_get_object (builder, "account_match_page"));
    account_match_view  = GTK_WIDGET(gtk_builder_get_object (builder, "account_match_view"));
    account_match_label = GTK_WIDGET(gtk_builder_get_object (builder, "account_match_label"));
    account_match_btn = GTK_WIDGET(gtk_builder_get_object (builder, "account_match_change"));

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

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(csv_imp_asst));

    gtk_builder_connect_signals (builder, this);
    g_object_unref (G_OBJECT(builder));

    gtk_widget_show_all (GTK_WIDGET(csv_imp_asst));
    gnc_window_adjust_for_screen (GTK_WINDOW(csv_imp_asst));

    /* In order to trigger a book options display on the creation of a new book,
     * we need to detect when we are dealing with a new book. */
    new_book = gnc_is_new_book();
}


/*******************************************************
 * Assistant Destructor
 *******************************************************/
CsvImpTransAssist::~CsvImpTransAssist ()
{
    /* This function is safe to call on a null pointer */
    gnc_gen_trans_list_delete (gnc_csv_importer_gui);
    /* The call above frees gnc_csv_importer_gui but can't nullify it.
     * Do it here so noone accidentally can access it still */
    gnc_csv_importer_gui = nullptr;
    gtk_widget_destroy (GTK_WIDGET(csv_imp_asst));
}


/**************************************************
 * Code related to the file chooser page
 **************************************************/

/* csv_tximp_file_confirm_cb
 *
 * call back for ok button in file chooser widget
 */
void
CsvImpTransAssist::file_confirm_cb ()
{
    gtk_assistant_set_page_complete (csv_imp_asst, account_match_page, false);

    auto file_name = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER(file_chooser));
    if (!file_name)
        return;

    auto filepath = gnc_uri_get_path (file_name);
    auto starting_dir = g_path_get_dirname (filepath);

    m_file_name = file_name;
    gnc_set_default_directory (GNC_PREFS_GROUP, starting_dir);

    DEBUG("file_name selected is %s", m_file_name.c_str());
    DEBUG("starting directory is %s", starting_dir);

    g_free (filepath);
    g_free (file_name);
    g_free (starting_dir);

    /* Load the file into parse_data. */
    tx_imp = std::unique_ptr<GncTxImport>(new GncTxImport);
    /* Assume data is CSV. User can later override to Fixed Width if needed */
    try
    {
        tx_imp->file_format (GncImpFileFormat::CSV);
        tx_imp->load_file (m_file_name);
        tx_imp->tokenize (true);
    }
    catch (std::ifstream::failure& e)
    {
        /* File loading failed ... */
        gnc_error_dialog (GTK_WINDOW (csv_imp_asst), "%s", e.what());
        return;
    }
    catch (std::range_error &e)
    {
        /* Parsing failed ... */
        gnc_error_dialog (GTK_WINDOW (csv_imp_asst), "%s", _(e.what()));
        return;
    }

    preview_refresh ();

    /* Get settings store and populate */
    preview_populate_settings_combo();
    gtk_combo_box_set_active (settings_combo, 0);

    gtk_assistant_set_page_complete (csv_imp_asst, account_match_page, true);
    auto num = gtk_assistant_get_current_page (csv_imp_asst);
    gtk_assistant_set_current_page (csv_imp_asst, num + 1);
}


/**************************************************
 * Code related to the preview page
 **************************************************/

/* Set the available presets in the settings combo box
 */
void CsvImpTransAssist::preview_populate_settings_combo()
{
    // Clear the list store
    auto model = gtk_combo_box_get_model (settings_combo);
    gtk_list_store_clear (GTK_LIST_STORE(model));

    // Append the default entry
    auto presets = get_import_presets_trans ();
    for (auto preset : presets)
    {
        GtkTreeIter iter;
        gtk_list_store_append (GTK_LIST_STORE(model), &iter);
        /* FIXME we store the raw pointer to the preset, while it's
         * managed by a shared pointer. This is dangerous because
         * when the shared pointer goes out of scope, our pointer will dangle.
         * For now this is safe, because the shared pointers in this case are
         * long-lived, but this may need refactoring.
         */
        gtk_list_store_set (GTK_LIST_STORE(model), &iter, SET_GROUP, preset.get(), SET_NAME, _(preset->m_name.c_str()), -1);
    }
}

/* Enable or disable the save and delete settings buttons
 * depending on what is selected and entered as settings name
 */
void CsvImpTransAssist::preview_handle_save_del_sensitivity (GtkComboBox* combo)
{
    GtkTreeIter iter;
    auto can_delete = false;
    auto can_save = false;
    auto entry = gtk_bin_get_child (GTK_BIN(combo));
    auto entry_text = gtk_entry_get_text (GTK_ENTRY(entry));
    /* Handle sensitivity of the delete and save button */
    if (gtk_combo_box_get_active_iter (combo, &iter))
    {
        CsvTransImpSettings *preset;
        GtkTreeModel *model = gtk_combo_box_get_model (combo);
        gtk_tree_model_get (model, &iter, SET_GROUP, &preset, -1);

        if (preset && !preset_is_reserved_name (preset->m_name))
        {
            /* Current preset is not read_only, so buttons can be enabled */
            can_delete = true;
            can_save = true;
        }
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
    auto text = gtk_entry_get_text (entry);
    if (text)
        tx_imp->settings_name(text);

    auto box = gtk_widget_get_parent (GTK_WIDGET(entry));
    auto combo = gtk_widget_get_parent (GTK_WIDGET(box));

    preview_handle_save_del_sensitivity (GTK_COMBO_BOX(combo));
}


/* Use selected preset to configure the import. Triggered when
 * a preset is selected in the settings combo.
 */
void
CsvImpTransAssist::preview_settings_load ()
{
    // Get the Active Selection
    GtkTreeIter iter;
    if (!gtk_combo_box_get_active_iter (settings_combo, &iter))
        return;

    CsvTransImpSettings *preset = nullptr;
    auto model = gtk_combo_box_get_model (settings_combo);
    gtk_tree_model_get (model, &iter, SET_GROUP, &preset, -1);

    if (!preset)
        return;

    tx_imp->settings (*preset);
    if (preset->m_load_error)
        gnc_error_dialog (GTK_WINDOW (csv_imp_asst),
            "%s", _("There were problems reading some saved settings, continuing to load.\n"
                    "Please review and save again."));

    preview_refresh ();
    preview_handle_save_del_sensitivity (settings_combo);
}

/* Callback to delete a settings entry
 */
void
CsvImpTransAssist::preview_settings_delete ()
{
    // Get the Active Selection
    GtkTreeIter iter;
    if (!gtk_combo_box_get_active_iter (settings_combo, &iter))
        return;

    CsvTransImpSettings *preset = nullptr;
    auto model = gtk_combo_box_get_model (settings_combo);
    gtk_tree_model_get (model, &iter, SET_GROUP, &preset, -1);

    auto response = gnc_ok_cancel_dialog (GTK_WINDOW (csv_imp_asst),
                                GTK_RESPONSE_CANCEL,
                                "%s", _("Delete the Import Settings."));
    if (response == GTK_RESPONSE_OK)
    {
        preset->remove();
        preview_populate_settings_combo();
        gtk_combo_box_set_active (settings_combo, 0); // Default
        preview_refresh (); // Reset the widgets
    }
}

/* Callback to save the current settings to the gnucash state file.
 */
void
CsvImpTransAssist::preview_settings_save ()
{
    auto new_name = tx_imp->settings_name();

    /* Check if the entry text matches an already existing preset */
    GtkTreeIter iter;
    if (!gtk_combo_box_get_active_iter (settings_combo, &iter))
    {

        auto model = gtk_combo_box_get_model (settings_combo);
        bool valid = gtk_tree_model_get_iter_first (model, &iter);
        while (valid)
        {
            // Walk through the list, reading each row
            CsvTransImpSettings *preset;
            gtk_tree_model_get (model, &iter, SET_GROUP, &preset, -1);

            if (preset && (preset->m_name == std::string(new_name)))
            {
                auto response = gnc_ok_cancel_dialog (GTK_WINDOW (csv_imp_asst),
                        GTK_RESPONSE_OK,
                        "%s", _("Setting name already exists, over write?"));
                if (response != GTK_RESPONSE_OK)
                    return;

                break;
            }
            valid = gtk_tree_model_iter_next (model, &iter);
        }
    }

    /* All checks passed, let's save this preset */
    if (!tx_imp->save_settings())
    {
        gnc_info_dialog (GTK_WINDOW (csv_imp_asst),
            "%s", _("The settings have been saved."));

        // Update the settings store
        preview_populate_settings_combo();
        auto model = gtk_combo_box_get_model (settings_combo);

        // Get the first entry in model
        GtkTreeIter   iter;
        bool valid = gtk_tree_model_get_iter_first (model, &iter);
        while (valid)
        {
            // Walk through the list, reading each row
            gchar *name = nullptr;
            gtk_tree_model_get (model, &iter, SET_NAME, &name, -1);

            if (g_strcmp0 (name, new_name.c_str()) == 0) // Set Active, the one Saved.
                gtk_combo_box_set_active_iter (settings_combo, &iter);

            g_free (name);

            valid = gtk_tree_model_iter_next (model, &iter);
        }
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
        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(skip_alt_rows_button)),
        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(skip_errors_button)));

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
 * separator checkbuttons or the custom separator entry) is
 * changed.
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
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(sep_button[i])))
            checked_separators += stock_sep_chars[i];
    }

    /* Add the custom separator if the user checked its button. */
    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(custom_cbutton)))
    {
        auto custom_sep = gtk_entry_get_text (custom_entry);
        if (custom_sep[0] != '\0') /* Don't add a blank separator (bad things will happen!). */
            checked_separators += custom_sep;
    }

    /* Set the parse options using the checked_separators list. */
    tx_imp->separators (checked_separators);

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
            gtk_entry_set_text (GTK_ENTRY(widget), "");
        /* If the user checked a checkbutton, toggle that checkbutton back. */
        else
            gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(widget),
                                         !gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(widget)));
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
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(csv_button)))
        {
            tx_imp->file_format (GncImpFileFormat::CSV);
            g_signal_handlers_disconnect_by_func(G_OBJECT(treeview),
                    (gpointer)csv_tximp_preview_treeview_clicked_cb, (gpointer)this);
            gtk_widget_set_visible (separator_table, true);
            gtk_widget_set_visible (fw_instructions_hbox, false);
        }
        else
        {
            tx_imp->file_format (GncImpFileFormat::FIXED_WIDTH);
            /* Enable context menu for adding/removing columns. */
            g_signal_connect (G_OBJECT(treeview), "button-press-event",
                    G_CALLBACK(csv_tximp_preview_treeview_clicked_cb), (gpointer)this);
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
    tx_imp->date_format (gtk_combo_box_get_active (GTK_COMBO_BOX(date_format_combo)));
    preview_refresh_table ();
}


void
CsvImpTransAssist::preview_update_currency_format ()
{
    tx_imp->currency_format (gtk_combo_box_get_active (GTK_COMBO_BOX(currency_format_combo)));
    preview_refresh_table ();
}

static gboolean
csv_imp_preview_queue_rebuild_table (CsvImpTransAssist *assist)
{
    assist->preview_refresh_table ();
    return false;
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
void CsvImpTransAssist::preview_update_col_type (GtkComboBox* cbox)
{
    /* Get the new text */
    GtkTreeIter iter;
    auto model = gtk_combo_box_get_model (cbox);
    gtk_combo_box_get_active_iter (cbox, &iter);
    auto new_col_type = GncTransPropType::NONE;
    gtk_tree_model_get (model, &iter, COL_TYPE_ID, &new_col_type, -1);

    auto col_num = GPOINTER_TO_UINT (g_object_get_data (G_OBJECT(cbox), "col-num"));
    tx_imp->set_column_type (col_num, new_col_type);

    /* Delay rebuilding our data table to avoid critical warnings due to
     * pending events still acting on them after this event is processed.
     */
    g_idle_add ((GSourceFunc)csv_imp_preview_queue_rebuild_table, this);

}

/*======================================================================*/
/*================== Beginning of Gnumeric Code ========================*/

/* The following is code copied from Gnumeric 1.7.8 licensed under the
 * GNU General Public License version 2 and/or version 3. It is from the file
 * gnumeric/src/dialogs/dialog-stf-fixed-page.c, and it has been
 * modified slightly to work within GnuCash. */

/*
 * Copyright 2001 Almer S. Tigelaar <almer@gnome.org>
 * Copyright 2003 Morten Welinder <terra@gnome.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

enum
{
    CONTEXT_STF_IMPORT_MERGE_LEFT = 1,
    CONTEXT_STF_IMPORT_MERGE_RIGHT = 2,
    CONTEXT_STF_IMPORT_SPLIT = 3,
    CONTEXT_STF_IMPORT_WIDEN = 4,
    CONTEXT_STF_IMPORT_NARROW = 5
};

static GnumericPopupMenuElement const popup_elements[] =
{
    {
        N_("Merge with column on _left"), "list-remove",
        0, 1 << CONTEXT_STF_IMPORT_MERGE_LEFT, CONTEXT_STF_IMPORT_MERGE_LEFT
    },
    {
        N_("Merge with column on _right"), "list-remove",
        0, 1 << CONTEXT_STF_IMPORT_MERGE_RIGHT, CONTEXT_STF_IMPORT_MERGE_RIGHT
    },
    { "", nullptr, 0, 0, 0 },
    {
        N_("_Split this column"), nullptr,
        0, 1 << CONTEXT_STF_IMPORT_SPLIT, CONTEXT_STF_IMPORT_SPLIT
    },
    { "", nullptr, 0, 0, 0 },
    {
        N_("_Widen this column"), "go-next",
        0, 1 << CONTEXT_STF_IMPORT_WIDEN, CONTEXT_STF_IMPORT_WIDEN
    },
    {
        N_("_Narrow this column"), "go-previous",
        0, 1 << CONTEXT_STF_IMPORT_NARROW, CONTEXT_STF_IMPORT_NARROW
    },
    { nullptr, nullptr, 0, 0, 0 },
};

uint32_t CsvImpTransAssist::get_new_col_rel_pos (GtkTreeViewColumn *tcol, int dx)
{
    auto renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT(tcol));
    auto cell = GTK_CELL_RENDERER(renderers->data);
    g_list_free (renderers);
    PangoFontDescription *font_desc;
    g_object_get (G_OBJECT(cell), "font_desc", &font_desc, nullptr);

    PangoLayout *layout = gtk_widget_create_pango_layout (GTK_WIDGET(treeview), "x");
    pango_layout_set_font_description (layout, font_desc);
    int width;
    pango_layout_get_pixel_size (layout, &width, nullptr);
    if (width < 1) width = 1;
    uint32_t charindex = (dx + width / 2) / width;
    g_object_unref (layout);
    pango_font_description_free (font_desc);

    return charindex;
}

gboolean
fixed_context_menu_handler (GnumericPopupMenuElement const *element,
        gpointer userdata)
{
    auto info = (CsvImpTransAssist*)userdata;
    auto fwtok = dynamic_cast<GncFwTokenizer*>(info->tx_imp->m_tokenizer.get());

    switch (element->index)
    {
    case CONTEXT_STF_IMPORT_MERGE_LEFT:
        fwtok->col_delete (info->fixed_context_col - 1);
        break;
    case CONTEXT_STF_IMPORT_MERGE_RIGHT:
        fwtok->col_delete (info->fixed_context_col);
        break;
    case CONTEXT_STF_IMPORT_SPLIT:
        fwtok->col_split (info->fixed_context_col, info->fixed_context_offset);
        break;
    case CONTEXT_STF_IMPORT_WIDEN:
        fwtok->col_widen (info->fixed_context_col);
        break;
    case CONTEXT_STF_IMPORT_NARROW:
        fwtok->col_narrow (info->fixed_context_col);
        break;
    default:
        ; /* Nothing */
    }

    try
    {
        info->tx_imp->tokenize (false);
    }
    catch(std::range_error& e)
    {
        gnc_error_dialog (GTK_WINDOW (info->csv_imp_asst), "%s", e.what());
        return false;
    }
    info->preview_refresh_table ();
    return true;
}

void
CsvImpTransAssist::fixed_context_menu (GdkEventButton *event,
                    int col, int offset)
{
    auto fwtok = dynamic_cast<GncFwTokenizer*>(tx_imp->m_tokenizer.get());
    fixed_context_col = col;
    fixed_context_offset = offset;

    int sensitivity_filter = 0;
    if (!fwtok->col_can_delete (col - 1))
        sensitivity_filter |= (1 << CONTEXT_STF_IMPORT_MERGE_LEFT);
    if (!fwtok->col_can_delete (col))
        sensitivity_filter |= (1 << CONTEXT_STF_IMPORT_MERGE_RIGHT);
    if (!fwtok->col_can_split (col, offset))
        sensitivity_filter |= (1 << CONTEXT_STF_IMPORT_SPLIT);
    if (!fwtok->col_can_widen (col))
        sensitivity_filter |= (1 << CONTEXT_STF_IMPORT_WIDEN);
    if (!fwtok->col_can_narrow (col))
        sensitivity_filter |= (1 << CONTEXT_STF_IMPORT_NARROW);

    gnumeric_create_popup_menu (popup_elements, &fixed_context_menu_handler,
                                this, 0,
                                sensitivity_filter, event);
}

/*===================== End of Gnumeric Code ===========================*/
/*======================================================================*/
void
CsvImpTransAssist::preview_split_column (int col, int offset)
{
    auto fwtok = dynamic_cast<GncFwTokenizer*>(tx_imp->m_tokenizer.get());
    fwtok->col_split (col, offset);
    try
    {
        tx_imp->tokenize (false);
    }
    catch (std::range_error& e)
    {
        gnc_error_dialog (GTK_WINDOW (csv_imp_asst), "%s", e.what());
        return;
    }
    preview_refresh_table();
}


/** Event handler for clicking on column headers. This function is
 * called whenever the user clicks on column headers in
 * preview->treeview to modify columns when in fixed-width mode.
 * @param event The event that happened (where the user clicked)
 */
void
CsvImpTransAssist::preview_update_fw_columns (GtkTreeView* treeview, GdkEventButton* event)
{
    /* Nothing to do if this was not triggered on our treeview body */
    if (event->window != gtk_tree_view_get_bin_window (treeview))
        return;

    /* Find the column that was clicked. */
    GtkTreeViewColumn *tcol = nullptr;
    int cell_x = 0;
    auto success = gtk_tree_view_get_path_at_pos (treeview,
            (int)event->x, (int)event->y,
            nullptr, &tcol, &cell_x, nullptr);
    if (!success)
        return;

    /* Stop if no column found in this treeview (-1) or
     * if column is the error messages column (0) */
    auto tcol_list = gtk_tree_view_get_columns(treeview);
    auto tcol_num = g_list_index (tcol_list, tcol);
    g_list_free (tcol_list);
    if (tcol_num <= 0)
        return;

    /* Data columns in the treeview are offset by one
     * because the first column is the error column
     */
    auto dcol = tcol_num - 1;
    auto offset = get_new_col_rel_pos (tcol, cell_x);
    if (event->type == GDK_2BUTTON_PRESS && event->button == 1)
        /* Double clicks can split columns. */
        preview_split_column (dcol, offset);
    else if (event->type == GDK_BUTTON_PRESS && event->button == 3)
        /* Right clicking brings up a context menu. */
        fixed_context_menu (event, dcol, offset);
}


/* Convert state info (errors/skipped) in visual feedback to decorate the preview table */
void
CsvImpTransAssist::preview_row_fill_state_cells (GtkListStore *store, GtkTreeIter *iter,
        std::string& err_msg, bool skip)
{
    /* Extract error status for all non-skipped lines */
    const char *c_err_msg = nullptr;
    const char *icon_name = nullptr;
    const char *fcolor = nullptr;
    const char *bcolor = nullptr;
    if (!skip && !err_msg.empty())
    {
        fcolor = "black";
        bcolor = "pink";
        c_err_msg = err_msg.c_str();
        icon_name = "dialog-error";
    }
    gtk_list_store_set (store, iter,
            PREV_COL_FCOLOR, fcolor,
            PREV_COL_BCOLOR, bcolor,
            PREV_COL_STRIKE, skip,
            PREV_COL_ERROR, c_err_msg,
            PREV_COL_ERR_ICON, icon_name, -1);
}

/* Helper function that creates a combo_box using a model
 * with valid column types and selects the given column type
 */
GtkWidget*
CsvImpTransAssist::preview_cbox_factory (GtkTreeModel* model, uint32_t colnum)
{
    GtkTreeIter iter;
    auto cbox = gtk_combo_box_new_with_model(model);

    /* Set up a renderer for this combobox. */
    auto renderer = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT(cbox),
            renderer, true);
    gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT(cbox),
            renderer, "text", COL_TYPE_NAME);

    auto valid = gtk_tree_model_get_iter_first (model, &iter);
    while (valid)
    {
        gint stored_col_type;
        gtk_tree_model_get (model, &iter,
                COL_TYPE_ID, &stored_col_type, -1);
        if (stored_col_type == static_cast<int>( tx_imp->column_types()[colnum]))
            break;
        valid = gtk_tree_model_iter_next(model, &iter);
    }
    if (valid)
        gtk_combo_box_set_active_iter (GTK_COMBO_BOX(cbox), &iter);

    g_object_set_data (G_OBJECT(cbox), "col-num", GUINT_TO_POINTER(colnum));
    g_signal_connect (G_OBJECT(cbox), "changed",
                     G_CALLBACK(csv_tximp_preview_col_type_changed_cb), (gpointer)this);

    gtk_widget_show (cbox);
    return cbox;
}

void
CsvImpTransAssist::preview_style_column (uint32_t col_num, GtkTreeModel* model)
{
    auto col = gtk_tree_view_get_column (treeview, col_num);
    auto renderer = static_cast<GtkCellRenderer*>(gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(col))->data);
    /* First column -the error status column- is rendered differently */
    if (col_num == 0)
    {
        gtk_tree_view_column_set_attributes (col, renderer,
                "icon-name", PREV_COL_ERR_ICON,
                "cell-background", PREV_COL_BCOLOR, nullptr);
        g_object_set (G_OBJECT(renderer), "stock-size", GTK_ICON_SIZE_MENU, nullptr);
        g_object_set (G_OBJECT(col), "sizing", GTK_TREE_VIEW_COLUMN_FIXED,
                "fixed-width", 20, nullptr);
        gtk_tree_view_column_set_resizable (col, false);
    }
    else
    {
        gtk_tree_view_column_set_attributes (col, renderer,
                "foreground", PREV_COL_FCOLOR,
                "background", PREV_COL_BCOLOR,
                "strikethrough", PREV_COL_STRIKE,
                "text", col_num + PREV_N_FIXED_COLS -1, nullptr);

        /* We want a monospace font fixed-width data is properly displayed. */
        g_object_set (G_OBJECT(renderer), "family", "monospace", nullptr);

        /* Add a combobox to select column types as column header. Each uses the same
         * common model for the dropdown list. The selected value is taken
         * from the column_types vector. */
        auto cbox = preview_cbox_factory (GTK_TREE_MODEL(model), col_num - 1);
        gtk_tree_view_column_set_widget (col, cbox);

        /* Enable resizing of the columns. */
        gtk_tree_view_column_set_resizable (col, true);
        gtk_tree_view_column_set_clickable (col, true);
    }

}

/* Helper to create a shared store for the header comboboxes in the preview treeview.
 * It holds the possible column types */
static GtkTreeModel*
make_column_header_model (bool multi_split)
{
    auto combostore = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);
    for (auto col_type : gnc_csv_col_type_strs)
    {
        /* Only add column types that make sense in
         * the chosen import mode (multi-split vs two-split).
         */
        if (sanitize_trans_prop(col_type.first, multi_split) == col_type.first)
        {
            GtkTreeIter iter;
            gtk_list_store_append (combostore, &iter);
            gtk_list_store_set (combostore, &iter,
                    COL_TYPE_NAME, _(col_type.second),
                    COL_TYPE_ID, static_cast<int>(col_type.first), -1);
        }
    }
    return GTK_TREE_MODEL(combostore);
}

/* Updates the preview treeview to show the data as parsed based on the user's
 * import parameters.
 */
void CsvImpTransAssist::preview_refresh_table ()
{
    preview_validate_settings ();

    /* Create a new liststore to hold status and data from the file being imported.
       The first columns hold status information (row-color, row-errors, row-error-icon,...
       All following columns represent the tokenized data as strings. */
    auto ncols = PREV_N_FIXED_COLS + tx_imp->column_types().size();
    auto model_col_types = g_new (GType, ncols);
    model_col_types[PREV_COL_FCOLOR] = G_TYPE_STRING;
    model_col_types[PREV_COL_BCOLOR] = G_TYPE_STRING;
    model_col_types[PREV_COL_ERROR] = G_TYPE_STRING;
    model_col_types[PREV_COL_ERR_ICON] = G_TYPE_STRING;
    model_col_types[PREV_COL_STRIKE] = G_TYPE_BOOLEAN;
    for (guint i = PREV_N_FIXED_COLS; i <  ncols; i++)
        model_col_types[i] = G_TYPE_STRING;
    auto store = gtk_list_store_newv (ncols, model_col_types);
    g_free (model_col_types);

    /* Fill the data liststore with data from importer object. */
    for (auto parse_line : tx_imp->m_parsed_lines)
    {
        /* Fill the state cells */
        GtkTreeIter iter;
        gtk_list_store_append (store, &iter);
        preview_row_fill_state_cells (store, &iter,
                std::get<PL_ERROR>(parse_line), std::get<PL_SKIP>(parse_line));

        /* Fill the data cells. */
        for (auto cell_str_it = std::get<PL_INPUT>(parse_line).cbegin(); cell_str_it != std::get<PL_INPUT>(parse_line).cend(); cell_str_it++)
        {
            uint32_t pos = PREV_N_FIXED_COLS + cell_str_it - std::get<PL_INPUT>(parse_line).cbegin();
            gtk_list_store_set (store, &iter, pos, cell_str_it->c_str(), -1);
        }
    }
    gtk_tree_view_set_model (treeview, GTK_TREE_MODEL(store));
    gtk_tree_view_set_tooltip_column (treeview, PREV_COL_ERROR);

    /* Adjust treeview to go with the just created model. This consists of adding
     * or removing columns and resetting any parameters related to how
     * the columns and data should be rendered.
     */

    /* Start with counting the current number of columns (ntcols)
     * we have in the treeview */
    auto columns = gtk_tree_view_get_columns (treeview);
    auto ntcols = g_list_length(columns);
    g_list_free (columns);

    /* Drop redundant columns if the model has less data columns than the new model
     * ntcols = n° of columns in treeview (1 error column + x data columns)
     * ncols = n° of columns in model (fixed state columns + x data columns)
     */
    while (ntcols > ncols - PREV_N_FIXED_COLS + 1)
    {
        auto col = gtk_tree_view_get_column (treeview, ntcols - 1);
        gtk_tree_view_column_clear (col);
        ntcols = gtk_tree_view_remove_column(treeview, col);
    }

    /* Insert columns if the model has more data columns than the treeview. */
    while (ntcols < ncols - PREV_N_FIXED_COLS + 1)
    {
        /* Default cell renderer is text, except for the first (error) column */
        auto renderer = gtk_cell_renderer_text_new();
        if (ntcols == 0)
            renderer = gtk_cell_renderer_pixbuf_new(); // Error column uses an icon
        auto col = gtk_tree_view_column_new ();
        gtk_tree_view_column_pack_start (col, renderer, false);
        ntcols = gtk_tree_view_append_column (treeview, col);
    }

    /* Reset column attributes as they are undefined after recreating the model */
    auto combostore = make_column_header_model (tx_imp->multi_split());
    for (uint32_t i = 0; i < ntcols; i++)
        preview_style_column (i, combostore);

    /* Release our reference for the stores to allow proper memory management. */
    g_object_unref (store);
    g_object_unref (combostore);

    /* Also reset the base account combo box as it's value may have changed due to column changes here */
    g_signal_handlers_block_by_func (acct_selector, (gpointer) csv_tximp_preview_acct_sel_cb, this);
    gnc_account_sel_set_account(GNC_ACCOUNT_SEL(acct_selector),
            tx_imp->base_account() , false);
    g_signal_handlers_unblock_by_func (acct_selector, (gpointer) csv_tximp_preview_acct_sel_cb, this);

    /* Make the things actually appear. */
    gtk_widget_show_all (GTK_WIDGET(treeview));
}

/* Update the preview page based on the current state of the importer.
 * Should be called when settings are changed.
 */
void
CsvImpTransAssist::preview_refresh ()
{
    // Set start row
    auto adj = gtk_spin_button_get_adjustment (start_row_spin);
    gtk_adjustment_set_upper (adj, tx_imp->m_parsed_lines.size());
    gtk_spin_button_set_value (start_row_spin,
            tx_imp->skip_start_lines());

    // Set end row
    adj = gtk_spin_button_get_adjustment (end_row_spin);
    gtk_adjustment_set_upper (adj, tx_imp->m_parsed_lines.size());
    gtk_spin_button_set_value (end_row_spin,
            tx_imp->skip_end_lines());

    // Set Alternate rows
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(skip_alt_rows_button),
            tx_imp->skip_alt_lines());

    // Set multi-split indicator
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(multi_split_cbutton),
            tx_imp->multi_split());
    gtk_widget_set_sensitive (acct_selector, !tx_imp->multi_split());

    // Set Import Format
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(csv_button),
            (tx_imp->file_format() == GncImpFileFormat::CSV));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(fixed_button),
            (tx_imp->file_format() != GncImpFileFormat::CSV));

    // Set Date & Currency Format and Character encoding
    gtk_combo_box_set_active (GTK_COMBO_BOX(date_format_combo),
            tx_imp->date_format());
    gtk_combo_box_set_active (GTK_COMBO_BOX(currency_format_combo),
            tx_imp->currency_format());
    go_charmap_sel_set_encoding (encselector, tx_imp->encoding().c_str());

    // Handle separator checkboxes and custom field, only relevant if the file format is csv
    if (tx_imp->file_format() == GncImpFileFormat::CSV)
    {
        auto separators = tx_imp->separators();
        const auto stock_sep_chars = std::string (" \t,:;-");
        for (int i = 0; i < SEP_NUM_OF_TYPES; i++)
            gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(sep_button[i]),
                separators.find (stock_sep_chars[i]) != std::string::npos);

        // If there are any other separators in the separators string,
        // add them as custom separators
        auto pos = separators.find_first_of (stock_sep_chars);
        while (!separators.empty() && pos != std::string::npos)
        {
            separators.erase(pos);
            pos = separators.find_first_of (stock_sep_chars);
        }
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(custom_cbutton),
                !separators.empty());
        gtk_entry_set_text (GTK_ENTRY(custom_entry), separators.c_str());
    }

    // Repopulate the parsed data table
    preview_refresh_table ();
}

/* Check if all selected data can be parsed sufficiently to continue
 */
void CsvImpTransAssist::preview_validate_settings ()
{
    /* Allow the user to proceed only if there are no inconsistencies in the settings */
    auto error_msg = tx_imp->verify();
    gtk_assistant_set_page_complete (csv_imp_asst, preview_page, error_msg.empty());
    gtk_label_set_markup(GTK_LABEL(instructions_label), error_msg.c_str());
    gtk_widget_set_visible (GTK_WIDGET(instructions_image), !error_msg.empty());

    /* Show or hide the account match page based on whether there are
     * accounts in the user data according to the importer configuration
     */
    gtk_widget_set_visible (GTK_WIDGET(account_match_page),
            !tx_imp->accounts().empty());
}


/**************************************************
 * Code related to the account match page
 **************************************************/

/* Populates the account match view with all potential
 * account names found in the parse data.
 *
 * @param info The data being previewed
 */
void CsvImpTransAssist::acct_match_set_accounts ()
{
    auto store = gtk_tree_view_get_model (GTK_TREE_VIEW(account_match_view));
    gtk_list_store_clear (GTK_LIST_STORE(store));

    auto accts = tx_imp->accounts();
    for (auto acct : accts)
    {
        GtkTreeIter acct_iter;
        gtk_list_store_append (GTK_LIST_STORE(store), &acct_iter);
        gtk_list_store_set (GTK_LIST_STORE(store), &acct_iter, MAPPING_STRING, acct.c_str(),
                            MAPPING_FULLPATH, _("No Linked Account"), MAPPING_ACCOUNT, nullptr, -1);
    }
}


static bool
csv_tximp_acct_match_check_all (GtkTreeModel *model)
{
    // Set iter to first entry of store
    GtkTreeIter iter;
    auto valid = gtk_tree_model_get_iter_first (model, &iter);

    // Walk through the store looking for nullptr accounts
    while (valid)
    {
        Account *account;
        gtk_tree_model_get (model, &iter, MAPPING_ACCOUNT, &account, -1);
        if (!account)
            return false;

        valid = gtk_tree_model_iter_next (model, &iter);
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
        sep_pos = acct_name.find(sep);
        for (sep_pos = acct_name.find(sep); sep_pos != std::string::npos;
                sep_pos = acct_name.find(sep))
            acct_name.replace (sep_pos, strlen(sep), alt_sep);
        return acct_name;
    }
}

void
CsvImpTransAssist::acct_match_select(GtkTreeModel *model, GtkTreeIter* iter)
{
    // Get the the stored string and account (if any)
    gchar *text = nullptr;
    Account *account = nullptr;
    gtk_tree_model_get (model, iter, MAPPING_STRING, &text,
                                     MAPPING_ACCOUNT, &account, -1);

    auto acct_name = csv_tximp_acct_match_text_parse (text);
    auto gnc_acc = gnc_import_select_account (GTK_WIDGET(csv_imp_asst), nullptr, true,
            acct_name.c_str(), nullptr, ACCT_TYPE_NONE, account, nullptr);

    if (gnc_acc) // We may have canceled
    {
        auto fullpath = gnc_account_get_full_name (gnc_acc);
        gtk_list_store_set (GTK_LIST_STORE(model), iter,
                MAPPING_ACCOUNT, gnc_acc,
                MAPPING_FULLPATH, fullpath, -1);

        // Update the account kvp mappings
        gnc_csv_account_map_change_mappings (account, gnc_acc, text);

        g_free (fullpath);
    }
    g_free (text);

    gtk_assistant_set_page_complete (csv_imp_asst, account_match_page,
            csv_tximp_acct_match_check_all (model));

}

void
CsvImpTransAssist::acct_match_via_button ()
{
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW(account_match_view));
    auto selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(account_match_view));

    GtkTreeIter iter;
    if (gtk_tree_selection_get_selected (selection, &model, &iter))
        acct_match_select (model, &iter);
}


/* This is the callback for the mouse click */
bool
CsvImpTransAssist::acct_match_via_view_dblclick (GdkEventButton *event)
{
    /* This is for a double click */
    if (event->button == 1 && event->type == GDK_2BUTTON_PRESS)
    {
        auto window = gtk_tree_view_get_bin_window (GTK_TREE_VIEW (account_match_view));
        if (event->window != window)
            return false;

        /* Get tree path for row that was clicked, true if row exists */
        GtkTreePath *path;
        if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (account_match_view), (gint) event->x, (gint) event->y,
                                             &path, nullptr, nullptr, nullptr))
        {
            DEBUG("event->x is %d and event->y is %d", (gint)event->x, (gint)event->y);

            auto model = gtk_tree_view_get_model (GTK_TREE_VIEW(account_match_view));
            GtkTreeIter iter;
            if (gtk_tree_model_get_iter (model, &iter, path))
                acct_match_select (model, &iter);
            gtk_tree_path_free (path);
        }
        return true;
    }
    return false;
}


/*******************************************************
 * Assistant page prepare functions
 *******************************************************/

void
CsvImpTransAssist::assist_file_page_prepare ()
{
    /* Set the default directory */
    auto starting_dir = gnc_get_default_directory (GNC_PREFS_GROUP);
    if (starting_dir)
    {
        gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER(file_chooser), starting_dir);
        g_free (starting_dir);
    }

    /* Disable the Forward Assistant Button */
    gtk_assistant_set_page_complete (csv_imp_asst, account_match_page, false);
}


void
CsvImpTransAssist::assist_preview_page_prepare ()
{
    tx_imp->req_mapped_accts (false);

    /* Disable the Forward Assistant Button */
    gtk_assistant_set_page_complete (csv_imp_asst, preview_page, false);

    /* Load the data into the treeview. */
    preview_refresh_table ();
}

void
CsvImpTransAssist::assist_account_match_page_prepare ()
{
    tx_imp->req_mapped_accts(true);

    // Load the account strings into the store
    acct_match_set_accounts ();

    // Match the account strings to the mappings
    auto store = gtk_tree_view_get_model (GTK_TREE_VIEW(account_match_view));
    gnc_csv_account_map_load_mappings (store);

    auto text = std::string ("<span size=\"medium\" color=\"red\"><b>");
    text += _("To change mapping, double click on a row or select a row and press the button...");
    text += "</b></span>";
    gtk_label_set_markup (GTK_LABEL(account_match_label), text.c_str());

    // Enable the view, possibly after an error
    gtk_widget_set_sensitive (account_match_view, true);
    gtk_widget_set_sensitive (account_match_btn, true);

    /* Enable the Forward Assistant Button */
       gtk_assistant_set_page_complete (csv_imp_asst, account_match_page,
               csv_tximp_acct_match_check_all (store));
}


void
CsvImpTransAssist::assist_doc_page_prepare ()
{
    /* Block going back */
    gtk_assistant_commit (csv_imp_asst);

    /* At this stage in the assistant each account should be mapped so
     * complete the split properties with this information. If this triggers
     * an exception it indicates a logic error in the code.
     */
    try
    {
        auto col_types = tx_imp->column_types();
        auto acct_col = std::find (col_types.begin(),
                col_types.end(), GncTransPropType::ACCOUNT);
        if (acct_col != col_types.end())
            tx_imp->set_column_type (acct_col - col_types.begin(),
                    GncTransPropType::ACCOUNT, true);
        acct_col = std::find (col_types.begin(),
                col_types.end(), GncTransPropType::TACCOUNT);
        if (acct_col != col_types.end())
            tx_imp->set_column_type (acct_col - col_types.begin(),
                    GncTransPropType::TACCOUNT, true);
    }
    catch (const std::invalid_argument& err)
    {
        /* Oops! This shouldn't happen when using the import assistant !
         * Inform the user and go back to the preview page.
         */
        gnc_error_dialog (GTK_WINDOW (csv_imp_asst),
            _("An unexpected error has occurred while mapping accounts. Please report this as a bug.\n\n"
              "Error message:\n%s"), err.what());
        gtk_assistant_set_current_page (csv_imp_asst, 2);

    }

    /* Before creating transactions, if this is a new book, let user specify
     * book options, since they affect how transactions are created */
    if (new_book)
        new_book = gnc_new_book_option_display (GTK_WIDGET(csv_imp_asst));

    /* Add the Cancel button for the matcher */
    cancel_button = gtk_button_new_with_mnemonic (_("_Cancel"));
    gtk_assistant_add_action_widget (csv_imp_asst, cancel_button);
    g_signal_connect (cancel_button, "clicked",
                     G_CALLBACK(csv_tximp_assist_close_cb), this);
    gtk_widget_show (GTK_WIDGET(cancel_button));
}


void
CsvImpTransAssist::assist_match_page_prepare ()
{
    /* Create transactions from the parsed data */
    try
    {
        tx_imp->create_transactions ();
    }
    catch (const std::invalid_argument& err)
    {
        /* Oops! This shouldn't happen when using the import assistant !
         * Inform the user and go back to the preview page.
         */
        gnc_error_dialog (GTK_WINDOW (csv_imp_asst),
            _("An unexpected error has occurred while creating transactions. Please report this as a bug.\n\n"
              "Error message:\n%s"), err.what());
        gtk_assistant_set_current_page (csv_imp_asst, 2);
    }

    /* Block going back */
    gtk_assistant_commit (csv_imp_asst);

    auto text = std::string( "<span size=\"medium\" color=\"red\"><b>");
    text += _("Double click on rows to change, then click on Apply to Import");
    text += "</b></span>";
    gtk_label_set_markup (GTK_LABEL(match_label), text.c_str());

    /* Add the help button for the matcher */
    help_button = gtk_button_new_with_mnemonic (_("_Help"));
    gtk_assistant_add_action_widget (csv_imp_asst, help_button);
    g_signal_connect (help_button, "clicked",
                     G_CALLBACK(on_matcher_help_clicked), gnc_csv_importer_gui);
    gtk_widget_show (GTK_WIDGET(help_button));

    /* Copy all of the transactions to the importer GUI. */
    for (auto trans_it : tx_imp->m_transactions)
    {
        auto draft_trans = trans_it.second;
        if (draft_trans->trans)
        {
            gnc_gen_trans_list_add_trans (gnc_csv_importer_gui, draft_trans->trans);
            draft_trans->trans = nullptr;
        }
    }
}


void
CsvImpTransAssist::assist_summary_page_prepare ()
{
    /* Remove the added buttons */
    gtk_assistant_remove_action_widget (csv_imp_asst, help_button);
    gtk_assistant_remove_action_widget (csv_imp_asst, cancel_button);

    bl::generator gen;
    gen.add_messages_path(gnc_path_get_datadir());
    gen.add_messages_domain(PACKAGE);

    // FIXME Rather than passing a locale generator below we probably should set std::locale::global appropriately somewhere.
    auto text = std::string("<span size=\"medium\"><b>");
    /* Translators: {1} will be replaced with a filename */
    text += (bl::format (bl::translate ("The transactions were imported from file '{1}'.")) % m_file_name).str(gen(""));
    text += "</b></span>";
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
