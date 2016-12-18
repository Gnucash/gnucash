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
/** @file assistant-csv-trans-import.c
    @brief CSV Import Assistant
    @author Copyright (c) 2012 Robert Fewell
*/

#include <guid.hpp>

extern "C"
{
#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <stdlib.h>

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

#include "gnc-csv-trans-settings.hpp"
#include "gnc-tx-import.hpp"
#include "gnc-fw-tokenizer.hpp"
#include "gnc-csv-tokenizer.hpp"

#define MIN_COL_WIDTH 70
#define GNC_PREFS_GROUP "dialogs.import.csv"
#define ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS "assistant-csv-trans-import"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

struct  CsvImportTrans
{

    GtkAssistant    *csv_imp_asst;

    GtkWidget       *start_page;                    /**< Assistant start page widget */

    GtkWidget       *file_page;                     /**< Assistant file page widget */
    GtkWidget       *file_chooser;                  /**< The widget for the file chooser */
    std::string      file_name;                     /**< The import file name */

    GtkWidget       *preview_page;                  /**< Assistant preview page widget */
    GtkComboBox     *settings_combo;                /**< The Settings Combo */
    GtkWidget       *save_button;                   /**< The Save Settings button */
    GtkWidget       *del_button;                    /**< The Delete Settings button */
    GtkWidget       *acct_selector;                 /**< The Account selector */
    GtkWidget       *combo_hbox;                    /**< The Settings Combo hbox */
    GtkWidget       *skip_errors_button;            /**< The widget for the check label button */
    GtkSpinButton   *start_row_spin;                /**< The widget for the start row spinner */
    GtkSpinButton   *end_row_spin;                  /**< The widget for the end row spinner */
    GtkWidget       *skip_alt_rows_button;          /**< The widget for Skip alternate rows from start row */
    GtkWidget       *csv_button;                    /**< The widget for the CSV button */
    GtkWidget       *fixed_button;                  /**< The widget for the Fixed Width button */
    GtkWidget       *multi_split_cbutton;           /**< The widget for Multi-split */
    GOCharmapSel    *encselector;                   /**< The widget for selecting the encoding */
    GtkCheckButton  *sep_button[SEP_NUM_OF_TYPES];  /**< Checkbuttons for common separators */
    GtkCheckButton  *custom_cbutton;                /**< The checkbutton for a custom separator */
    GtkEntry        *custom_entry;                  /**< The entry for custom separators */
    GtkComboBoxText *date_format_combo;             /**< The Combo Text widget for selecting the date format */
    GtkComboBoxText *currency_format_combo;         /**< The Combo Text widget for selecting the currency format */
    GtkTreeView     *treeview;                      /**< The treeview containing the data */
    GtkTreeView     *ctreeview;                     /**< The treeview containing the column types */
    GtkLabel        *instructions_label;            /**< The instructions label */
    GtkImage        *instructions_image;            /**< The instructions image */
    bool             encoding_selected_called;      /**< Before encoding_selected is first called, this is false.
                                                       * (See description of encoding_selected.) */
    bool             previewing_errors;             /**< true if the dialog is displaying
                                                       * error lines, instead of all the file data. */
    bool             skip_errors;                   /**< This is false until the user checks the skip errors. */
    int              fixed_context_col;             /**< The number of the column whose the user has clicked */
    int              fixed_context_dx;              /**< The horizontal coordinate of the pixel in the header of the column
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
    bool                  match_parse_run = false;  /**< This is set after the first run */

    GtkWidget            *summary_page;             /**< Assistant summary page widget */
    GtkWidget            *summary_label;            /**< The summary text */

    bool                  new_book;                 /**< Are we importing into a new book?; if yes, call book options */
    GncTxImport          *tx_imp;                   /**< The actual data we are previewing */
    int                   callcount;                /**< Number of times the assistant page forward function called */
    int                   next_page;                /**< The saved assistant next page number */

};


/*************************************************************************/

extern "C"
{
void csv_tximp_assist_prepare_cb (GtkAssistant  *assistant, GtkWidget *page, CsvImportTrans* info);
void csv_tximp_assist_finish_cb (GtkAssistant *gtkassistant, CsvImportTrans* info);
void csv_tximp_assist_cancel_cb (GtkAssistant *gtkassistant, CsvImportTrans* info);
void csv_tximp_assist_close_cb (GtkAssistant *gtkassistant, CsvImportTrans* info);
void csv_tximp_assist_destroy_cb (GtkWidget *object, CsvImportTrans* info);
void csv_tximp_file_confirm_cb (GtkWidget *button, CsvImportTrans *info);
void csv_tximp_preview_srow_cb (GtkSpinButton *spin, CsvImportTrans *info);
void csv_tximp_preview_erow_cb (GtkSpinButton *spin, CsvImportTrans *info);
void csv_tximp_preview_skiperrors_cb (GtkToggleButton *checkbox, CsvImportTrans *info);
void csv_tximp_preview_skiprows_cb (GtkToggleButton *checkbox, CsvImportTrans *info);
void csv_tximp_preview_multisplit_cb (GtkToggleButton *checkbox, CsvImportTrans *info);
void csv_tximp_preview_del_settings_cb (GtkWidget *button, CsvImportTrans *info);
void csv_tximp_preview_save_settings_cb (GtkWidget *button, CsvImportTrans *info);
void csv_tximp_preview_settings_sel_changed_cb (GtkComboBox *combo, CsvImportTrans *info);
void csv_tximp_preview_settings_text_changed_cb (GtkEntry *entry, CsvImportTrans *info);
void csv_tximp_preview_sep_button_cb (GtkWidget* widget, CsvImportTrans* info);
void csv_tximp_preview_sep_fixed_sel_cb (GtkToggleButton* csv_button, CsvImportTrans* info);
void csv_tximp_preview_acct_sel_cb (GtkWidget* widget, CsvImportTrans* info);
void csv_tximp_preview_enc_sel_cb (GOCharmapSel* selector, const char* encoding,
                              CsvImportTrans* info);
void csv_tximp_acct_match_button_clicked_cb (GtkWidget *widget, CsvImportTrans* info);
bool csv_tximp_acct_match_view_clicked_cb (GtkWidget *widget, GdkEventButton *event, CsvImportTrans* info);
}

void csv_tximp_assist_start_page_prepare (GtkAssistant *gtkassistant, CsvImportTrans* info);
void csv_tximp_assist_file_page_prepare (GtkAssistant *assistant, CsvImportTrans* info);
void csv_tximp_assist_preview_page_prepare (GtkAssistant *gtkassistant, CsvImportTrans* info);
void csv_tximp_assist_account_match_page_prepare (GtkAssistant *assistant, CsvImportTrans* info);
void csv_tximp_assist_doc_page_prepare (GtkAssistant *assistant, CsvImportTrans* info);
void csv_tximp_assist_match_page_prepare (GtkAssistant *assistant, CsvImportTrans* info);
void csv_tximp_assist_summary_page_prepare (GtkAssistant *assistant, CsvImportTrans* info);

void csv_tximp_preview_refresh_table (CsvImportTrans* info);
void csv_tximp_preview_refresh (CsvImportTrans *info);
void csv_tximp_validate_preview_settings (CsvImportTrans* info);
bool get_list_of_accounts (CsvImportTrans* info, GtkTreeModel *store);

/*************************************************************************/



/*******************************************************
 * populate_settings_combo
 *
 * Set the available presets in the settings combo box
 *******************************************************/

static void populate_settings_combo(GtkComboBox* combo)
{
    // Clear the list store
    auto model = gtk_combo_box_get_model (combo);
    gtk_list_store_clear (GTK_LIST_STORE(model));

    // Append the default entry

    auto presets = get_trans_presets ();
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
        gtk_list_store_set (GTK_LIST_STORE(model), &iter, SET_GROUP, preset.get(), SET_NAME, preset->m_name.c_str(), -1);
    }
}


static void handle_save_del_sensitivity (GtkComboBox* combo, CsvImportTrans *info)
{
    GtkTreeIter iter;
    auto can_delete = false;
    auto can_save = false;
    auto entry = gtk_bin_get_child (GTK_BIN(combo));
    auto entry_text = gtk_entry_get_text (GTK_ENTRY(entry));
    /* Handle sensitivity of the delete and save button */
    if (gtk_combo_box_get_active_iter (combo, &iter))
    {
        CsvTransSettings *preset;
        GtkTreeModel *model = gtk_combo_box_get_model (combo);
        gtk_tree_model_get (model, &iter, SET_GROUP, &preset, -1);

        if (preset && !trans_preset_is_reserved_name (preset->m_name))
        {
            /* Current preset is not read_only, so buttons can be enabled */
            can_delete = true;
            can_save = true;
        }
    }
    else if (entry_text && (strlen (entry_text) > 0) &&
            !trans_preset_is_reserved_name (std::string(entry_text)))
        can_save = true;

    gtk_widget_set_sensitive (info->save_button, can_save);
    gtk_widget_set_sensitive (info->del_button, can_delete);

}


/*******************************************************
 * csv_tximp_preview_settings_sel_changed_cb
 *
 * Triggered when a preset is selected in the settings combo.
 *******************************************************/
void
csv_tximp_preview_settings_sel_changed_cb (GtkComboBox *combo, CsvImportTrans *info)
{
    // Get the Active Selection
    GtkTreeIter iter;
    if (!gtk_combo_box_get_active_iter (combo, &iter))
        return;

    CsvTransSettings *preset = nullptr;
    auto model = gtk_combo_box_get_model (combo);
    gtk_tree_model_get (model, &iter, SET_GROUP, &preset, -1);

    if (!preset)
        return;

    info->tx_imp->settings (*preset);
    if (preset->m_load_error)
    {
        auto dialog = gtk_message_dialog_new (GTK_WINDOW(info->csv_imp_asst),
                                    (GtkDialogFlags) 0,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    "%s", _("Load the Import Settings."));
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
            "%s", _("There were problems reading some saved settings, continuing to load.\n Please review and save again."));
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
    }

    csv_tximp_preview_refresh (info);
    handle_save_del_sensitivity (combo, info);
    csv_tximp_validate_preview_settings (info);
}


/*******************************************************
 * csv_tximp_preview_settings_text_changed_cb
 *
 * Triggered while the user is editing text the settings combo.
 *******************************************************/
void
csv_tximp_preview_settings_text_changed_cb (GtkEntry *entry, CsvImportTrans *info)
{
    auto text = gtk_entry_get_text (entry);
    if (text)
        info->tx_imp->settings_name(text);

    auto combo = gtk_widget_get_parent (GTK_WIDGET(entry));
    handle_save_del_sensitivity (GTK_COMBO_BOX(combo), info);
}


/*******************************************************
 * csv_tximp_preview_del_settings_cb
 *
 * call back to delete a settings entry
 *******************************************************/
void
csv_tximp_preview_del_settings_cb (GtkWidget *button, CsvImportTrans *info)
{
    // Get the Active Selection
    GtkTreeIter iter;
    if (!gtk_combo_box_get_active_iter (info->settings_combo, &iter))
        return;

    CsvTransSettings *preset = nullptr;
    auto model = gtk_combo_box_get_model (info->settings_combo);
    gtk_tree_model_get (model, &iter, SET_GROUP, &preset, -1);

    auto dialog = gtk_message_dialog_new (GTK_WINDOW(info->csv_imp_asst),
                                (GtkDialogFlags) 0,
                                GTK_MESSAGE_QUESTION,
                                GTK_BUTTONS_OK_CANCEL,
                                "%s", _("Delete the Import Settings."));
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
        "%s", _("Do you really want to delete the selection?"));
    auto response = gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);

    if (response == GTK_RESPONSE_OK)
    {
        preset->remove();
        populate_settings_combo(info->settings_combo);
        gtk_combo_box_set_active (info->settings_combo, 0); // Default
        csv_tximp_preview_refresh (info); // Reset the widgets
    }
}

/*******************************************************
 * csv_tximp_preview_save_settings_cb
 *
 * Save the settings to a Key File.
 *******************************************************/
void
csv_tximp_preview_save_settings_cb (GtkWidget *button, CsvImportTrans *info)
{
    auto title = _("Save the Import Settings.");
    auto new_name = info->tx_imp->settings_name();

    /* Check if the entry text matches an already existing preset */
    GtkTreeIter iter;
    if (!gtk_combo_box_get_active_iter (info->settings_combo, &iter))
    {

        auto model = gtk_combo_box_get_model (info->settings_combo);
        bool valid = gtk_tree_model_get_iter_first (model, &iter);
        while (valid)
        {
            // Walk through the list, reading each row
            CsvTransSettings *preset;
            gtk_tree_model_get (model, &iter, SET_GROUP, &preset, -1);

            if (preset && (preset->m_name == std::string(new_name)))
            {
                auto dialog = gtk_message_dialog_new (GTK_WINDOW(info->csv_imp_asst),
                                        (GtkDialogFlags) 0,
                                        GTK_MESSAGE_QUESTION,
                                        GTK_BUTTONS_OK_CANCEL,
                                        "%s", title);
                gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                "%s", _("Setting name already exists, over write?"));
                auto response = gtk_dialog_run (GTK_DIALOG (dialog));
                gtk_widget_destroy (dialog);

                if (response != GTK_RESPONSE_OK)
                    return;

                break;
            }
            valid = gtk_tree_model_iter_next (model, &iter);
        }
    }

    /* All checks passed, let's save this preset */
    if (!info->tx_imp->save_settings())
    {
        auto dialog = gtk_message_dialog_new (GTK_WINDOW(info->csv_imp_asst),
                                    (GtkDialogFlags) 0,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_OK,
                                    "%s", title);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
            "%s", _("The settings have been saved."));
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);

        // Update the settings store
        populate_settings_combo(info->settings_combo);
        auto model = gtk_combo_box_get_model (info->settings_combo);

        // Get the first entry in model
        GtkTreeIter   iter;
        bool valid = gtk_tree_model_get_iter_first (model, &iter);
        while (valid)
        {
            // Walk through the list, reading each row
            gchar *name = nullptr;
            gtk_tree_model_get (model, &iter, SET_NAME, &name, -1);

            if (g_strcmp0 (name, new_name.c_str()) == 0) // Set Active, the one Saved.
                gtk_combo_box_set_active_iter (info->settings_combo, &iter);

            g_free (name);

            valid = gtk_tree_model_iter_next (model, &iter);
        }
    }
    else
    {
        auto dialog = gtk_message_dialog_new (GTK_WINDOW(info->csv_imp_asst),
                                    (GtkDialogFlags) 0,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    "%s", title);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
            "%s", _("There was a problem saving the settings, please try again."));
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
    }
}


/**************************************************
 * csv_tximp_file_confirm_cb
 *
 * call back for ok button in file chooser widget
 **************************************************/
void
csv_tximp_file_confirm_cb (GtkWidget *button, CsvImportTrans *info)
{
    auto num = gtk_assistant_get_current_page (info->csv_imp_asst);
    auto page = gtk_assistant_get_nth_page (info->csv_imp_asst, num);

    gtk_assistant_set_page_complete (info->csv_imp_asst, page, false);

    auto file_name = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER(info->file_chooser));
    if (!file_name)
        return;

    auto filepath = gnc_uri_get_path (file_name);
    auto starting_dir = g_path_get_dirname (filepath);

    info->file_name = file_name;
    gnc_set_default_directory (GNC_PREFS_GROUP, starting_dir);

    DEBUG("file_name selected is %s", info->file_name.c_str());
    DEBUG("starting directory is %s", starting_dir);

    g_free (filepath);
    g_free (file_name);
    g_free (starting_dir);

    /* Load the file into parse_data. */
    auto parse_data = new GncTxImport;
    /* Assume data is CSV. User can later override to Fixed Width if needed */
    try
    {
        parse_data->file_format (GncImpFileFormat::CSV);
        parse_data->load_file (info->file_name);
        parse_data->tokenize (true);
    }
    catch (std::ifstream::failure& e)
    {
        /* File loading failed ... */
        gnc_error_dialog (nullptr, "%s", e.what());
            delete parse_data;
            return;
    }
    catch (std::range_error &e)
    {
        /* Parsing failed ... */
        gnc_error_dialog (nullptr, "%s", e.what());
        delete parse_data;
        return;
    }

    if (info->tx_imp) // Free parse_data if we have come back here
        delete info->tx_imp;
    info->tx_imp = parse_data;
    info->previewing_errors = false; /* We're looking at all the data. */
    info->skip_errors = false; // Set skip_errors to False
    csv_tximp_preview_refresh (info);

    /* Get settings store and populate */
    populate_settings_combo(info->settings_combo);
    gtk_combo_box_set_active (info->settings_combo, 0);

    gtk_assistant_set_page_complete (info->csv_imp_asst, page, true);
    gtk_assistant_set_current_page (info->csv_imp_asst, num + 1);
}


/**************************************************
 * csv_tximp_row_sel_update
 *
 * refresh the start and end row highlighting
 **************************************************/
static
void csv_tximp_preview_row_sel_update (CsvImportTrans* info)
{
    GtkTreeIter iter;
    auto store = GTK_LIST_STORE(gtk_tree_view_get_model (info->treeview));

    /* Colorize rows that will be skipped */
    for (uint i = 0; i < info->tx_imp->m_parsed_lines.size(); i++)
    {
        const char *color = nullptr;
        if ((i < info->tx_imp->skip_start_lines()) ||             // start rows to skip
            (i >= info->tx_imp->m_parsed_lines.size()
                    - info->tx_imp->skip_end_lines()) ||          // end rows to skip
            (((i - info->tx_imp->skip_start_lines()) % 2 == 1) && // skip every second row...
             info->tx_imp->skip_alt_lines()))                     // ...if requested
            color = "pink";
        else
            color = nullptr;                                          // all other rows

        bool valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(store), &iter, nullptr, i);
        if (valid)
            gtk_list_store_set (store, &iter, 0, color, -1);
    }
}

/*******************************************************
 * csv_import_preview_refresh
 *
 * Update the preview page based on the current state of the importer.
 * Should be called when settings are changed.
 *******************************************************/
void
csv_tximp_preview_refresh (CsvImportTrans *info)
{
    // Set start row
    auto adj = gtk_spin_button_get_adjustment (info->start_row_spin);
    gtk_adjustment_set_upper (adj, info->tx_imp->m_parsed_lines.size());
    gtk_spin_button_set_value (info->start_row_spin,
            info->tx_imp->skip_start_lines());

    // Set end row
    adj = gtk_spin_button_get_adjustment (info->end_row_spin);
    gtk_adjustment_set_upper (adj, info->tx_imp->m_parsed_lines.size());
    gtk_spin_button_set_value (info->end_row_spin,
            info->tx_imp->skip_end_lines());

    // Set Alternate rows
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->skip_alt_rows_button),
            info->tx_imp->skip_alt_lines());

    // Set multi-split indicator
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->multi_split_cbutton),
            info->tx_imp->multi_split());

    // Set Import Format
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->csv_button),
            (info->tx_imp->file_format() == GncImpFileFormat::CSV));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->fixed_button),
            (info->tx_imp->file_format() != GncImpFileFormat::CSV));

    // This section deals with the combo's and character encoding
    gtk_combo_box_set_active (GTK_COMBO_BOX(info->date_format_combo),
            info->tx_imp->date_format());
    gtk_combo_box_set_active (GTK_COMBO_BOX(info->currency_format_combo),
            info->tx_imp->currency_format());
    go_charmap_sel_set_encoding (info->encselector, info->tx_imp->encoding().c_str());

    gnc_account_sel_set_account(GNC_ACCOUNT_SEL(info->acct_selector),
            info->tx_imp->base_account() , false);

    // Handle separator checkboxes and custom field, only relevant if the file format is csv
    if (info->tx_imp->file_format() == GncImpFileFormat::CSV)
    {
        auto separators = info->tx_imp->separators();
        const auto stock_sep_chars = std::string (" \t,:;-");
        for (int i = 0; i < SEP_NUM_OF_TYPES; i++)
            gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->sep_button[i]),
                separators.find (stock_sep_chars[i]) != std::string::npos);

        // If there are any other separators in the separators string,
        // add them as custom separators
        auto pos = separators.find_first_of (stock_sep_chars);
        while (!separators.empty() && pos != std::string::npos)
        {
            separators.erase(pos);
            pos = separators.find_first_of (stock_sep_chars);
        }
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->custom_cbutton),
                !separators.empty());
        gtk_entry_set_text (GTK_ENTRY(info->custom_entry), separators.c_str());
    }

    // Repopulate the parsed data table
    csv_tximp_preview_refresh_table (info);
}



/*******************************************************
 * csv_tximp_preview_srow_cb
 *
 * call back for import start row
 *******************************************************/
void csv_tximp_preview_srow_cb (GtkSpinButton *spin, CsvImportTrans *info)
{

    /* Get number of lines to skip at the beginning */
    info->tx_imp->skip_start_lines (gtk_spin_button_get_value_as_int (spin));

    /* And adjust maximum number of lines that can be skipped at the end accordingly */
    auto adj = gtk_spin_button_get_adjustment (info->end_row_spin);
    gtk_adjustment_set_upper (adj, info->tx_imp->m_parsed_lines.size()
            - info->tx_imp->skip_start_lines());

    csv_tximp_preview_row_sel_update (info);
    csv_tximp_validate_preview_settings (info);
}


/*******************************************************
 * csv_tximp_preview_erow_cb
 *
 * call back for import end row
 *******************************************************/
void csv_tximp_preview_erow_cb (GtkSpinButton *spin, CsvImportTrans *info)
{
    /* Get number of lines to skip at the end */
    info->tx_imp->skip_end_lines (gtk_spin_button_get_value_as_int (spin));

    /* And adjust maximum number of lines that can be skipped at the beginning accordingly */
    auto adj = gtk_spin_button_get_adjustment (info->start_row_spin);
    gtk_adjustment_set_upper (adj, info->tx_imp->m_parsed_lines.size()
            - info->tx_imp->skip_end_lines());

    csv_tximp_preview_row_sel_update (info);
    csv_tximp_validate_preview_settings (info);
}


/*******************************************************
 * csv_tximp_preview_skiperrors_cb
 *
 * call back for Skip Errors
 *******************************************************/
void csv_tximp_preview_skiperrors_cb (GtkToggleButton *checkbox, CsvImportTrans *info)
{
    info->skip_errors = gtk_toggle_button_get_active (checkbox);
    csv_tximp_validate_preview_settings (info);
}


/*******************************************************
 * csv_tximp_preview_multisplit_cb
 *
 * call back for import multi-split checkbox
 *******************************************************/
void csv_tximp_preview_multisplit_cb (GtkToggleButton *checkbox, CsvImportTrans *info)
{
    info->tx_imp->multi_split (gtk_toggle_button_get_active (checkbox));
    csv_tximp_preview_refresh_table (info);
    csv_tximp_validate_preview_settings (info);
}


/*******************************************************
 * csv_tximp_preview_skiprows_cb
 *
 * call back for import skip rows checkbox
 *******************************************************/
void csv_tximp_preview_skiprows_cb (GtkToggleButton *checkbox, CsvImportTrans *info)
{
    info->tx_imp->skip_alt_lines (gtk_toggle_button_get_active (checkbox));
    csv_tximp_preview_row_sel_update (info);
    csv_tximp_validate_preview_settings (info);
}


/** Returns the cell renderer from a column in the preview's treeview.
 * @param info The display of the data being imported
 * @param col The number of the column whose cell renderer is being retrieved
 * @return The cell renderer of column number col
 */
static GtkCellRenderer* gnc_csv_preview_get_cell_renderer (CsvImportTrans* info, int col)
{
    auto renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT(gtk_tree_view_get_column (info->treeview, col)));
    auto cell = GTK_CELL_RENDERER(renderers->data);
    g_list_free (renderers);
    return cell;
}


/** Event handler for separator changes. This function is called
 * whenever one of the widgets for configuring the separators (the
 * separator checkbuttons or the custom separator entry) is
 * changed.
 * @param widget The widget that was changed
 * @param info The data that is being configured
 */
void csv_tximp_preview_sep_button_cb (GtkWidget* widget, CsvImportTrans* info)
{

    /* Only manipulate separator characters if the currently open file is
     * csv separated. */
    if (info->tx_imp->file_format() != GncImpFileFormat::CSV)
        return;

    /* Add the corresponding characters to checked_separators for each
     * button that is checked. */
    auto checked_separators = std::string();
    const auto stock_sep_chars = std::string (" \t,:;-");
    for (int i = 0; i < SEP_NUM_OF_TYPES; i++)
    {
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(info->sep_button[i])))
            checked_separators += stock_sep_chars[i];
    }

    /* Add the custom separator if the user checked its button. */
    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(info->custom_cbutton)))
    {
        auto custom_sep = gtk_entry_get_text (info->custom_entry);
        if (custom_sep[0] != '\0') /* Don't add a blank separator (bad things will happen!). */
            checked_separators += custom_sep;
    }

    /* Set the parse options using the checked_separators list. */
    info->tx_imp->separators (checked_separators);

    /* Parse the data using the new options. We don't want to reguess
     * the column types because we want to leave the user's
     * configurations intact. */
    try
    {
        info->tx_imp->tokenize (false);
        csv_tximp_preview_refresh_table (info);
        csv_tximp_validate_preview_settings (info);
    }
    catch (std::range_error &e)
    {
        /* Warn the user there was a problem and try to undo what caused
         * the error. (This will cause a reparsing and ideally a usable
         * configuration.) */
        gnc_error_dialog (nullptr, "Error in parsing");
        /* If we're here because the user changed the file format, we should just wait for the user
         * to update the configuration */
        if (!widget)
            return;
        /* If the user changed the custom separator, erase that custom separator. */
        if (widget == GTK_WIDGET(info->custom_entry))
            gtk_entry_set_text (GTK_ENTRY(widget), "");
        /* If the user checked a checkbutton, toggle that checkbutton back. */
        else
            gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(widget),
                                         !gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(widget)));
        return;
    }
}

void csv_tximp_preview_acct_sel_cb (GtkWidget* widget, CsvImportTrans* info)
{

    auto acct = gnc_account_sel_get_account( GNC_ACCOUNT_SEL(widget) );
    info->tx_imp->base_account(acct);
    csv_tximp_preview_refresh_table (info);
    csv_tximp_validate_preview_settings (info);
}


/** Event handler for clicking one of the format type radio
 * buttons. This occurs if the format (Fixed-Width or CSV) is changed.
 * @param csv_button The "Separated" radio button
 * @param info The display of the data being imported
 */
void csv_tximp_preview_sep_fixed_sel_cb (GtkToggleButton* csv_button, CsvImportTrans* info)
{
    /* Set the parsing type correctly. */
    try
    {
        if (gtk_toggle_button_get_active (csv_button))
            info->tx_imp->file_format (GncImpFileFormat::CSV);
        else
            info->tx_imp->file_format (GncImpFileFormat::FIXED_WIDTH);

        info->tx_imp->tokenize (false);
        csv_tximp_preview_refresh_table (info);
        csv_tximp_validate_preview_settings (info);
    }
    catch (std::range_error &e)
    {
        /* Parsing failed ... */
        gnc_error_dialog (nullptr, "%s", e.what());
        return;
    }
    catch (...)
    {
        // FIXME Handle file loading errors (possibly thrown by file_format above)
        PWARN("Got an error during file loading");
    }
}


/** Event handler for a new encoding. This is called when the user
 * selects a new encoding; the data is reparsed and shown to the
 * user.
 * @param selector The widget the user uses to select a new encoding
 * @param encoding The encoding that the user selected
 * @param info The display of the data being imported
 */
void csv_tximp_preview_enc_sel_cb (GOCharmapSel* selector, const char* encoding,
                              CsvImportTrans* info)
{
    /* This gets called twice every time a new encoding is selected. The
     * second call actually passes the correct data; thus, we only do
     * something the second time this is called. */

    /* If this is the second time the function is called ... */
    if (info->encoding_selected_called)
    {
        std::string previous_encoding = info->tx_imp->m_tokenizer->encoding();
        /* Try converting the new encoding and reparsing. */
        try
        {
            info->tx_imp->encoding (encoding);
            info->tx_imp->tokenize (false);

            csv_tximp_preview_refresh_table (info);
            csv_tximp_validate_preview_settings (info);

            info->encoding_selected_called = false;
        }
        catch (...)
        {
            /* If it fails, change back to the old encoding. */
            gnc_error_dialog (nullptr, "%s", _("Invalid encoding selected"));
            info->encoding_selected_called = false;
            go_charmap_sel_set_encoding (selector, previous_encoding.c_str());
        }
    }
    else /* If this is the first call of the function ... */
    {
        info->encoding_selected_called = true; /* ... set the flag and wait for the next call. */
    }
}


/** Event handler for selecting a new date format.
 * @param format_selector The combo box for selecting date formats
 * @param info The display of the data being imported
 */
static void csv_tximp_preview_date_fmt_sel_cb (GtkComboBox* format_selector, CsvImportTrans* info)
{
    info->tx_imp->date_format (gtk_combo_box_get_active (format_selector));
    csv_tximp_validate_preview_settings (info);
}


/** Event handler for selecting a new currency format.
 * @param currency_selector The combo box for selecting currency formats
 * @param info The display of the data being imported
 */
static void csv_tximp_preview_currency_fmt_sel_cb (GtkComboBox* currency_selector, CsvImportTrans* info)
{
    info->tx_imp->currency_format (gtk_combo_box_get_active (currency_selector));
    csv_tximp_validate_preview_settings (info);
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
        N_("Merge with column on _left"), GTK_STOCK_REMOVE,
        0, 1 << CONTEXT_STF_IMPORT_MERGE_LEFT, CONTEXT_STF_IMPORT_MERGE_LEFT
    },
    {
        N_("Merge with column on _right"), GTK_STOCK_REMOVE,
        0, 1 << CONTEXT_STF_IMPORT_MERGE_RIGHT, CONTEXT_STF_IMPORT_MERGE_RIGHT
    },
    { "", nullptr, 0, 0, 0 },
    {
        N_("_Split this column"), nullptr,
        0, 1 << CONTEXT_STF_IMPORT_SPLIT, CONTEXT_STF_IMPORT_SPLIT
    },
    { "", nullptr, 0, 0, 0 },
    {
        N_("_Widen this column"), GTK_STOCK_GO_FORWARD,
        0, 1 << CONTEXT_STF_IMPORT_WIDEN, CONTEXT_STF_IMPORT_WIDEN
    },
    {
        N_("_Narrow this column"), GTK_STOCK_GO_BACK,
        0, 1 << CONTEXT_STF_IMPORT_NARROW, CONTEXT_STF_IMPORT_NARROW
    },
    { nullptr, nullptr, 0, 0, 0 },
};

static uint get_new_col_rel_pos (CsvImportTrans* info, int col, int dx)
{
    auto cell = gnc_csv_preview_get_cell_renderer (info, col);
    PangoFontDescription *font_desc;
    g_object_get (G_OBJECT(cell), "font_desc", &font_desc, nullptr);

    PangoLayout *layout = gtk_widget_create_pango_layout (GTK_WIDGET(info->treeview), "x");
    pango_layout_set_font_description (layout, font_desc);
    int width;
    pango_layout_get_pixel_size (layout, &width, nullptr);
    if (width < 1) width = 1;
    uint charindex = (dx + width / 2) / width;
    g_object_unref (layout);
    pango_font_description_free (font_desc);

    return charindex;
}

static gboolean
fixed_context_menu_handler (GnumericPopupMenuElement const *element,
        gpointer userdata)
{
    auto info = (CsvImportTrans*)userdata;
    auto col = info->fixed_context_col;
    auto rel_pos = get_new_col_rel_pos (info, col, info->fixed_context_dx);
    auto fwtok = dynamic_cast<GncFwTokenizer*>(info->tx_imp->m_tokenizer.get());

    switch (element->index)
    {
    case CONTEXT_STF_IMPORT_MERGE_LEFT:
        fwtok->col_delete (col - 1);
        break;
    case CONTEXT_STF_IMPORT_MERGE_RIGHT:
        fwtok->col_delete (col);
        break;
    case CONTEXT_STF_IMPORT_SPLIT:
        fwtok->col_split (col, rel_pos);
        break;
    case CONTEXT_STF_IMPORT_WIDEN:
        fwtok->col_widen (col);
        break;
    case CONTEXT_STF_IMPORT_NARROW:
        fwtok->col_narrow (col);
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
        gnc_error_dialog (nullptr, "%s", e.what());
        return false;
    }
    csv_tximp_preview_refresh_table (info);
    csv_tximp_validate_preview_settings (info);
    return true;
}
static void
select_column (CsvImportTrans* info, int col)
{
    if (col < 0)
        return;

    auto column = gtk_tree_view_get_column (info->treeview, col);
    gtk_widget_grab_focus (gtk_tree_view_column_get_widget(column));
}

static void
fixed_context_menu (CsvImportTrans* info, GdkEventButton *event,
                    int col, int dx)
{
    auto fwtok = dynamic_cast<GncFwTokenizer*>(info->tx_imp->m_tokenizer.get());
    info->fixed_context_col = col;
    info->fixed_context_dx = dx;
    uint rel_pos = get_new_col_rel_pos (info, col, dx);

    int sensitivity_filter = 0;
    if (!fwtok->col_can_delete (col - 1))
        sensitivity_filter |= (1 << CONTEXT_STF_IMPORT_MERGE_LEFT);
    if (!fwtok->col_can_delete (col))
        sensitivity_filter |= (1 << CONTEXT_STF_IMPORT_MERGE_RIGHT);
    if (!fwtok->col_can_split (col, rel_pos))
        sensitivity_filter |= (1 << CONTEXT_STF_IMPORT_SPLIT);
    if (!fwtok->col_can_widen (col))
        sensitivity_filter |= (1 << CONTEXT_STF_IMPORT_WIDEN);
    if (!fwtok->col_can_narrow (col))
        sensitivity_filter |= (1 << CONTEXT_STF_IMPORT_NARROW);

    select_column (info, col);
    gnumeric_create_popup_menu (popup_elements, &fixed_context_menu_handler,
                                info, 0,
                                sensitivity_filter, event);
}

/*===================== End of Gnumeric Code ===========================*/
/*======================================================================*/


/** Event handler for the data treeview being resized. When the data
 * treeview is resized, the column types treeview's columns are also resized to
 * match.
 * @param widget The data treeview
 * @param allocation The size of the data treeview
 * @param info The display of the data being imported
 */
static void csv_tximp_preview_treeview_resized_cb (GtkWidget* widget, GtkAllocation* allocation, CsvImportTrans* info)
{
    /* Go through each column except for the last. (We don't want to set
     * the width of the last column because the user won't be able to
     * shrink the dialog back if it's expanded.) */
    for (uint i = 0; i < info->tx_imp->column_types().size() - 1; i++)
    {
        /* Get the width. */
        auto col_width = gtk_tree_view_column_get_width (gtk_tree_view_get_column (info->treeview, i));
        /* Set the minimum width for a column so that drop down selector can be seen. */
        if (col_width < MIN_COL_WIDTH)
            col_width = MIN_COL_WIDTH;
        auto pcol = gtk_tree_view_get_column (info->treeview, i);
        gtk_tree_view_column_set_min_width (pcol, col_width);
        /* Set ccol's width the same. */
        auto ccol = gtk_tree_view_get_column (info->ctreeview, i);
        gtk_tree_view_column_set_min_width (ccol, col_width);
        gtk_tree_view_column_set_max_width (ccol, col_width);
    }
}


/** Event handler for the user selecting a new column type. When the
 * user selects a new column type, that column's text must be changed
 * to that selection, and any other columns containing that selection
 * must be changed to "None" because we don't allow duplicates.
 * @param renderer The renderer of the column the user changed
 * @param path There is only 1 row in info->ctreeview, so this is always 0.
 * @param new_text The text the user selected
 * @param info The display of the data being imported
 */
static void csv_tximp_preview_col_type_changed_cb (GtkCellRenderer* renderer, gchar* path,
                                GtkTreeIter* new_text_iter, CsvImportTrans* info)
{
    /* Get the new text */
    auto col_num = GPOINTER_TO_UINT (g_object_get_data (G_OBJECT(renderer), "col-num"));
    GtkTreeModel* model;
    g_object_get (renderer, "model", &model, nullptr);
    auto new_col_type = GncTransPropType::NONE;
    gtk_tree_model_get (model, new_text_iter,
            1, &new_col_type, // Invisible column in the combobox' model containing the column type
            -1);

    info->tx_imp->set_column_type (col_num, new_col_type);
    csv_tximp_preview_refresh_table (info);
    csv_tximp_validate_preview_settings (info);
}

static void
split_column (CsvImportTrans* info, int col, int dx)
{
    auto rel_pos = get_new_col_rel_pos (info, col, dx);
    auto fwtok = dynamic_cast<GncFwTokenizer*>(info->tx_imp->m_tokenizer.get());
    fwtok->col_split (col, rel_pos);
    try
    {
        info->tx_imp->tokenize (false);
    }
    catch (std::range_error& e)
    {
        gnc_error_dialog (nullptr, "%s", e.what());
        return;
    }
    csv_tximp_preview_refresh_table (info);
    csv_tximp_validate_preview_settings (info);
}


/** Event handler for clicking on column headers. This function is
 * called whenever the user clicks on column headers in
 * preview->treeview to modify columns when in fixed-width mode.
 * @param button The button at the top of a column of the treeview
 * @param event The event that happened (where the user clicked)
 * @param info The data being configured
 */
static void header_button_press_handler (GtkWidget* button, GdkEventButton* event,
                                        CsvImportTrans* info)
{
    /* Find the column that was clicked. */
    auto col = GPOINTER_TO_UINT(g_object_get_data (G_OBJECT(button), "col-num"));

    /* Don't let the user affect the last column if it has error messages. */
    if (col == info->tx_imp->column_types().size())
        return;

    /*  calculate offset to compensate for the button indentation. */
    GtkAllocation alloc;
    gtk_widget_get_allocation (gtk_bin_get_child (GTK_BIN(button)), &alloc);
    auto offset = alloc.x - alloc.x;
    if (event->type == GDK_2BUTTON_PRESS && event->button == 1)
        /* Double clicks can split columns. */
        split_column (info, col, (int)event->x - offset);
    else if (event->type == GDK_BUTTON_PRESS && event->button == 3)
        /* Right clicking brings up a context menu. */
        fixed_context_menu (info, event, col, (int)event->x - offset);
}


/* Test for the string being in the liststore
 * Returns true if it is or false if not.
 *
 * @param liststore The data being reviewed
 *
 * @param string to check for
 */
static bool
check_for_duplicates (GtkListStore *liststore, const gchar *string)
{
    GtkTreeIter iter;
    auto valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL(liststore), &iter);
    while (valid)
    {
        gchar *text;
        // Walk through the list, reading each row of column string
        gtk_tree_model_get (GTK_TREE_MODEL(liststore), &iter, MAPPING_STRING, &text, -1);

        if(!(g_strcmp0 (text, string)))
        {
            g_free (text);
            return true;
        }
        g_free (text);

        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL(liststore), &iter);
    }
    return false;
}


/* Get the list of accounts
 * Returns true if we have any accounts
 *
 * @param info The data being previewed
 *
 * @param store for the account match page
 */
bool get_list_of_accounts (CsvImportTrans* info, GtkTreeModel *store)
{
    /* ctstore contains the actual strings appearing in the column types treeview. */
    auto ctstore = gtk_tree_view_get_model (info->ctreeview);
    GtkTreeIter ct_iter;
    gtk_tree_model_get_iter_first (ctstore, &ct_iter);

    /* datastore contains the actual strings appearing in the preview treeview. */
    auto datastore = gtk_tree_view_get_model (info->treeview);
    auto have_accounts = false;
    for (uint j = info->tx_imp->skip_start_lines();
            j < info->tx_imp->m_parsed_lines.size() - info->tx_imp->skip_end_lines() - 1;
            j++)
    {
        /* Go through each of the columns. */
        for (uint i = 0; i < info->tx_imp->column_types().size(); i++)
        {
            auto col_type = GncTransPropType::NONE;

            /* Get the column type. Store is arranged so that every two
             * columns is a pair of
             * - the column type as a user visible (translated) string
             * - the internal type for this column
             * So store looks like:
             * col_type_str 0, col_type, col_type_str 1, col_type 1, ..., col_type_str ncols, col_type ncols. */
            gtk_tree_model_get (ctstore, &ct_iter, 2 * i + 1, &col_type, -1);

            /* We're only interested in columns of type ACCOUNT and OACCOUNT. */
            if ((col_type == GncTransPropType::ACCOUNT) || (col_type == GncTransPropType::TACCOUNT))
            {
                /* Get an iterator for the row in the data store. */
                GtkTreeIter data_iter;
                if (gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(datastore), &data_iter, nullptr, j))
                {
                    gchar* accstr = nullptr;   /* The string in this column from datastore. */
                    gtk_tree_model_get (datastore, &data_iter, i + 1, &accstr, -1);
                    if (!accstr || *accstr == '\0')
                        continue;

                    // Append the entry
                    if (!check_for_duplicates (GTK_LIST_STORE(store), accstr))
                    {
                        GtkTreeIter acct_iter;
                        gtk_list_store_append (GTK_LIST_STORE(store), &acct_iter);
                        gtk_list_store_set (GTK_LIST_STORE(store), &acct_iter, MAPPING_STRING, accstr,
                                            MAPPING_FULLPATH, _("No Linked Account"), MAPPING_ACCOUNT, nullptr, -1);
                        have_accounts = true;
                    }
                    g_free (accstr);
                }
            }
        }
    }

    return have_accounts;
}


/* Loads the preview's data into its data treeview.
 *
 * @param info The data being previewed
 */
void csv_tximp_preview_refresh_table (CsvImportTrans* info)
{
    /* ncols is the number of columns in the file data. */
    auto column_types = info->tx_imp->column_types();
    auto ncols = column_types.size();

    // Set up the header liststore

    /* ctstore will be the liststore for the header row, which displays the user's
     * column type selection
     * its related treeview is info->ctreeview
     * ctstore is arranged so that every two
     * columns form a pair of
     * - the column type as a user visible (translated) string
     * - the internal type for this column
     * So store looks like:
     * col_type_str 0, col_type, col_type_str 1, col_type 1, ..., col_type_str ncols, col_type ncols.
     * And then a final column is added for the Error column (which doesn't require a col_type) */
    auto headertypes = g_new (GType, 2 * ncols + 1);
    for (guint i = 0; i < 2 * ncols; i += 2)
    {
        headertypes[i] = G_TYPE_STRING;
        headertypes[i+1] = G_TYPE_INT;
    }
    headertypes[2 * ncols] = G_TYPE_STRING;
    auto ctstore = gtk_list_store_newv (2 * ncols + 1, headertypes);
    g_free (headertypes);

    /* Set all the column types to what's in the parse data. */
    GtkTreeIter iter;
    gtk_list_store_append (ctstore, &iter);
    for (guint i = 0; i < ncols; i++)
    {
        gtk_list_store_set (ctstore, &iter,
                2 * i, _(gnc_csv_col_type_strs[column_types[i]]),
                2 * i + 1, static_cast<int>(column_types[i]),
                -1);
    }
    gtk_list_store_set (ctstore, &iter, 2 * ncols, _("Errors"), -1);
    gtk_tree_view_set_model (info->ctreeview, GTK_TREE_MODEL(ctstore));


    // Set up file data liststore

    /* store is a liststore to hold the data from the file being imported.
       it contains only strings. */
    auto bodytypes = g_new (GType, ncols + 2);
    for (guint i = 0; i <  ncols + 2; i++)
        bodytypes[i] = G_TYPE_STRING;
    auto store = gtk_list_store_newv (ncols + 2, bodytypes);
    g_free (bodytypes);

    /* Fill the data liststore with data from the file. */
    for (auto parse_line : info->tx_imp->m_parsed_lines)
    {
        // When previewing errors skip all lines that don't have errors
        if (info->previewing_errors && std::get<1>(parse_line).empty())
            continue;

        GtkTreeIter iter;
        gtk_list_store_append (store, &iter);

        /* Row Color column */
        gtk_list_store_set (store, &iter, 0, nullptr, -1);

        for (auto cell_str_it = std::get<0>(parse_line).cbegin(); cell_str_it != std::get<0>(parse_line).cend(); cell_str_it++)
        {
            /* Set the value of the proper column in the list store. */
            uint pos = cell_str_it - std::get<0>(parse_line).cbegin() + 1;
            gtk_list_store_set (store, &iter, pos, cell_str_it->c_str(), -1);
        }
        /* Add the optional error messages in the last column of the store */
        gtk_list_store_set (store, &iter, ncols + 1, std::get<1>(parse_line).c_str(), -1);
    }
    gtk_tree_view_set_model (info->treeview, GTK_TREE_MODEL(store));

    // Set up the two header and file data treeviews using the liststores created above

    /* Clear any columns from a previous invocation */
    auto column = gtk_tree_view_get_column (info->ctreeview, 0);
    while (column)
    {
        gtk_tree_view_remove_column (info->ctreeview, column);
        column = gtk_tree_view_get_column (info->ctreeview, 0);
    }
    column = gtk_tree_view_get_column (info->treeview, 0);
    while (column)
    {
        gtk_tree_view_remove_column (info->treeview, column);
        column = gtk_tree_view_get_column (info->treeview, 0);
    }

    /* combostore is a shared store for the header combocells in the header row.
     * It holds the possible column types */
    auto combostore = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);
    for (auto col_type : gnc_csv_col_type_strs)
    {
        /* Only add column types that make sense in
         * the chosen import mode (multi-split vs two-split).
         */
        if (sanitize_trans_prop(col_type.first, info->tx_imp->multi_split()) == col_type.first)
        {
            GtkTreeIter iter;
            gtk_list_store_append (combostore, &iter);
            gtk_list_store_set (combostore, &iter, 0, _(col_type.second),
                                                   1, static_cast<int>(col_type.first),
                                                   -1);
        }
    }

    /* Insert columns into the data and column type treeviews. */
    for (uint i = 0; i < ncols ; i++)
    {
        /* The header cells are combobox entries. They all use the same
         * common model for the dropdown list while their text value
         * comes from the header liststore (ctstore). */
        auto crenderer = gtk_cell_renderer_combo_new();
        /* Set the properties for the dropdown list */
        g_object_set (G_OBJECT(crenderer), "model", combostore, "text-column", 0,
                     "editable", TRUE, "has-entry", FALSE, nullptr);
        g_object_set_data (G_OBJECT(crenderer),
                           "col-num", GUINT_TO_POINTER(i));
        g_signal_connect (G_OBJECT(crenderer), "changed",
                         G_CALLBACK(csv_tximp_preview_col_type_changed_cb), (gpointer)info);
        /* Insert the column */
        gtk_tree_view_insert_column_with_attributes (info->ctreeview,
                -1, "", crenderer, "text", 2 * i, nullptr);

        /* The file data treeview cells are simple text cells. */
        auto renderer = gtk_cell_renderer_text_new();
        /* We want a monospace font for the data in case of fixed-width data. */
        g_object_set (G_OBJECT(renderer), "family", "monospace", nullptr);

        /* Add a single column for the treeview. */
        auto col = gtk_tree_view_column_new_with_attributes ("", renderer, "background", 0,
                                                             "text", i + 1, nullptr);
        gtk_tree_view_insert_column (info->treeview, col, -1);
        /* Enable resizing of the columns. */
        gtk_tree_view_column_set_resizable (col, TRUE);

        /* We need to allow clicking on the file data's column headers for
         * fixed-width column splitting and merging. */
        g_object_set (G_OBJECT(col), "clickable", TRUE, nullptr);
        auto button = gtk_tree_view_column_get_widget(col);
        g_signal_connect (G_OBJECT(button), "button_press_event",
                         G_CALLBACK(header_button_press_handler), (gpointer)info);
        /* Store the column number in the button to know which one was clicked later on */
        g_object_set_data (G_OBJECT(button), "col-num",GINT_TO_POINTER(i));
    }

    /* Add a column for the error messages when reviewing errors */
    if (info->previewing_errors)
    {
        auto crenderer = gtk_cell_renderer_text_new();
        gtk_tree_view_insert_column_with_attributes (info->ctreeview,
                -1, "", crenderer, "text", 2 * ncols, nullptr);

        auto renderer = gtk_cell_renderer_text_new();
        auto col = gtk_tree_view_column_new_with_attributes ("", renderer, "background", 0,
                                                             "text", ncols + 1, nullptr);
        gtk_tree_view_insert_column (info->treeview, col, -1);
        /* Enable resizing of the columns. */
        gtk_tree_view_column_set_resizable (col, TRUE);
    }

    /* Select the header row */
    gtk_tree_model_get_iter_first (GTK_TREE_MODEL(ctstore), &iter);
    auto selection = gtk_tree_view_get_selection (info->ctreeview);
    gtk_tree_selection_select_iter (selection, &iter);

    /* Release our reference for the stores to allow proper memory management. */
    g_object_unref (store);
    g_object_unref (ctstore);
    g_object_unref (combostore);

    /* Make the things actually appear. */
    gtk_widget_show_all (GTK_WIDGET(info->treeview));
    gtk_widget_show_all (GTK_WIDGET(info->ctreeview));

    /* Update the row selection highlight */
    csv_tximp_preview_row_sel_update (info);

}


void csv_tximp_validate_preview_settings (CsvImportTrans* info)
{
    /* Allow the user to proceed only if there are no inconsistencies in the settings */
    auto error_msg = info->tx_imp->verify();
    gtk_assistant_set_page_complete (info->csv_imp_asst, info->preview_page, error_msg.empty());
    gtk_label_set_text(GTK_LABEL(info->instructions_label), error_msg.c_str());
    gtk_widget_set_visible (GTK_WIDGET(info->instructions_image), !error_msg.empty());
}

/*======================================================================*/
/*======================================================================*/

/*******************************************************
 * Assistant page prepare functions
 *******************************************************/
void
csv_tximp_assist_start_page_prepare (GtkAssistant *assistant,
        CsvImportTrans* info)
{
    auto num = gtk_assistant_get_current_page (assistant);
    auto page = gtk_assistant_get_nth_page (assistant, num);

    // Clear the treemodel list store
    auto store = gtk_tree_view_get_model (GTK_TREE_VIEW(info->account_match_view));
    gtk_list_store_clear (GTK_LIST_STORE(store));

    /* Enable the Assistant Buttons */
    gtk_assistant_set_page_complete (assistant, page, true);
}


void
csv_tximp_assist_file_page_prepare (GtkAssistant *assistant,
        CsvImportTrans* info)
{
    auto num = gtk_assistant_get_current_page (assistant);
    auto page = gtk_assistant_get_nth_page (assistant, num);

    info->previewing_errors = false; // We're looking at all the data.
    info->skip_errors = false; // Set skip_errors to False to start with.

    /* Set the default directory */
    auto starting_dir = gnc_get_default_directory (GNC_PREFS_GROUP);
    if (starting_dir)
    {
        gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER(info->file_chooser), starting_dir);
        g_free (starting_dir);
    }

    /* Disable the Forward Assistant Button */
    gtk_assistant_set_page_complete (assistant, page, false);
}


void
csv_tximp_assist_preview_page_prepare (GtkAssistant *assistant,
        CsvImportTrans* info)
{
    g_signal_connect (G_OBJECT(info->treeview), "size-allocate",
                     G_CALLBACK(csv_tximp_preview_treeview_resized_cb), (gpointer)info);

    // Hide the skip errors check button
    gtk_widget_hide (GTK_WIDGET(info->skip_errors_button));

    if (info->previewing_errors) // We are looking at errors to display
    {
        /* Block going back */
        gtk_assistant_commit (info->csv_imp_asst);

        gchar* name;
        GtkIconSize size;
        gtk_image_get_stock (info->instructions_image, &name, &size);
        gtk_image_set_from_stock (info->instructions_image, GTK_STOCK_DIALOG_ERROR, size);
        gtk_label_set_text (info->instructions_label,
                           _("The rows displayed below had errors which are in the last column. You can attempt to correct them by changing the configuration."));
        gtk_widget_show (GTK_WIDGET(info->instructions_image));
        gtk_widget_show (GTK_WIDGET(info->instructions_label));

        /* Reset start and end row */
        gtk_spin_button_set_value (info->start_row_spin, 0);
        gtk_spin_button_set_value (info->end_row_spin, 0);

        /* Set spin buttons and settings combo hbox not sensitive */
        gtk_widget_set_sensitive (info->combo_hbox, FALSE);
        gtk_widget_set_sensitive (GTK_WIDGET(info->start_row_spin), FALSE);
        gtk_widget_set_sensitive (GTK_WIDGET(info->end_row_spin), FALSE);
        gtk_widget_set_sensitive (info->skip_alt_rows_button, FALSE);
        gtk_widget_set_sensitive (info->multi_split_cbutton, FALSE);
        info->tx_imp->skip_alt_lines (false);

        /* Show the skip errors check button */
        gtk_widget_show (GTK_WIDGET(info->skip_errors_button));
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->skip_errors_button), FALSE);
    }
    else
    {
        // Load the account strings into the store
        auto store = gtk_tree_view_get_model (GTK_TREE_VIEW(info->account_match_view));
        gtk_list_store_clear (GTK_LIST_STORE(store)); // Clear list of accounts unless we are looking at errors
    }

    /* Disable the Forward Assistant Button */
    auto num = gtk_assistant_get_current_page (assistant);
    auto page = gtk_assistant_get_nth_page (assistant, num);
    gtk_assistant_set_page_complete (assistant, page, false);

    /* Load the data into the treeview. */
    csv_tximp_preview_refresh_table (info);
    csv_tximp_validate_preview_settings (info);

}


static bool
import_account_check_all (GtkTreeModel *model)
{
    // Set iter to first entry of store
    GtkTreeIter iter;
    auto valid = gtk_tree_model_get_iter_first (model, &iter);

    // Walk through the store looking for nullptr accounts
    auto ret = true;
    while (valid)
    {
        Account *account;

        // Walk through the list, reading each row
        gtk_tree_model_get (model, &iter, MAPPING_ACCOUNT, &account, -1);

        if (account == nullptr)
            ret = false;

        valid = gtk_tree_model_iter_next (model, &iter);
    }
    return ret;
}


/*****************************************************************
 * Parse the text splitting into a path and the last_part based on
 * account separator. If the path is valid, add the last_part and
 * return this. If the path is invalid, use the new separator path
 * with the last_part and return that so there is only one new
 * account dialog.
 *****************************************************************/
static gchar *
import_account_text_parse (gchar *text)
{
    // Split the incoming string by current separator
    auto sep = gnc_get_account_separator_string ();
    auto names = g_strsplit (text, sep, -1);

    /* find the last_part and count parts */
    int count = 0;
    gchar *last_part = nullptr;
    for (auto ptr = names; *ptr; ptr++)
    {
        if (g_strcmp0 (*ptr,"") != 0) //separator is last in string
        {
            if (last_part)
                g_free (last_part);
            last_part = g_strdup (*ptr);
            count = count + 1;
        }
    }

    // If count is 1 we have no path
    gchar *ret_string = nullptr;
    if (count == 1)
        ret_string = g_strdup (last_part);
    else
    {
        // Start to create two paths based on current and possibly new separator
        auto sep_path = g_strdup (names[0]);
        auto newsep_path = g_strdup (names[0]);

        // Setup an alternative separator
        const gchar *alt_sep;
        if (g_strcmp0 (sep,":") == 0)
            alt_sep = "-";
        else
            alt_sep = ":";

        // Join the parts together apart from last_part which could be account name
        for (int i = 1; i < count - 1; i++)
        {
            gchar *temp_sep_path, *temp_newsep_path;

            temp_sep_path = g_strdup (sep_path);
            temp_newsep_path = g_strdup (newsep_path);
            g_free (sep_path);
            g_free (newsep_path);
            sep_path = g_strconcat (temp_sep_path, sep, names[i], nullptr);
            newsep_path = g_strconcat (temp_newsep_path, alt_sep, names[i], nullptr);
            g_free (temp_sep_path);
            g_free (temp_newsep_path);
        }
        auto book = gnc_get_current_book ();
        auto account = gnc_account_lookup_by_full_name (gnc_book_get_root_account (book), sep_path);

        if (!account) // path not found
            ret_string = g_strconcat (newsep_path, alt_sep, last_part, nullptr);
        else
            ret_string = g_strconcat (sep_path, sep, last_part, nullptr);

        g_free (sep_path);
        g_free (newsep_path);
    }
    g_free (last_part);
    g_strfreev (names);
    return ret_string;
}

static void import_account_select_internal(CsvImportTrans* info, GtkTreeModel *model, GtkTreeIter& iter)
{
    // Get the the String
    gchar *text = nullptr;
    gtk_tree_model_get (model, &iter, MAPPING_STRING, &text, -1);

    // Get the pointer to the Account
    Account *account = nullptr;
    gtk_tree_model_get (model, &iter, MAPPING_ACCOUNT, &account, -1);

    auto parsed_text = import_account_text_parse (text);
    auto gnc_acc = gnc_import_select_account (nullptr, nullptr, 1,
            parsed_text, nullptr, ACCT_TYPE_NONE, account, nullptr);

    if (gnc_acc) // We may have canceled
    {
        auto fullpath = gnc_account_get_full_name (gnc_acc);
        gtk_list_store_set (GTK_LIST_STORE(model), &iter,
                MAPPING_ACCOUNT, gnc_acc,
                MAPPING_FULLPATH, fullpath, -1);

        // Update the account kvp mappings
        gnc_csv_account_map_change_mappings (account, gnc_acc, text);

        g_free (fullpath);
    }
    g_free (text);
    g_free (parsed_text);

    if (import_account_check_all (model))
        gtk_assistant_set_page_complete (info->csv_imp_asst, info->account_match_page, true);

}

void
csv_tximp_acct_match_button_clicked_cb (GtkWidget *widget, CsvImportTrans* info)
{
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW(info->account_match_view));
    auto selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(info->account_match_view));

    GtkTreeIter iter;
    if (gtk_tree_selection_get_selected (selection, &model, &iter))
        import_account_select_internal (info, model, iter);
}


/* This is the callback for the mouse click */
bool
csv_tximp_acct_match_view_clicked_cb (GtkWidget *widget, GdkEventButton *event, CsvImportTrans* info)
{
    /* This is for a double click */
    if (event->button == 1 && event->type == GDK_2BUTTON_PRESS)
    {
        auto window = gtk_tree_view_get_bin_window (GTK_TREE_VIEW (info->account_match_view));
        if (event->window != window)
            return false;

        /* Get tree path for row that was clicked, true if row exists */
        GtkTreePath *path;
        if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (info->account_match_view), (gint) event->x, (gint) event->y,
                                             &path, nullptr, nullptr, nullptr))
        {
            DEBUG("event->x is %d and event->y is %d", (gint)event->x, (gint)event->y);

            auto model = gtk_tree_view_get_model (GTK_TREE_VIEW(info->account_match_view));
            GtkTreeIter iter;
            if (gtk_tree_model_get_iter (model, &iter, path))
                import_account_select_internal (info, model, iter);
            gtk_tree_path_free (path);
        }
        return true;
    }
    return false;
}


void
csv_tximp_assist_account_match_page_prepare (GtkAssistant *assistant,
        CsvImportTrans* info)
{
    // Load the account strings into the store
    auto store = gtk_tree_view_get_model (GTK_TREE_VIEW(info->account_match_view));
    get_list_of_accounts (info, store);

    // Match the account strings to the mappings
    gnc_csv_account_map_load_mappings (store);

    auto text = std::string ("<span size=\"medium\" color=\"red\"><b>");
    text += _("To change mapping, double click on a row or select a row and press the button...");
    text += "</b></span>";
    gtk_label_set_markup (GTK_LABEL(info->account_match_label), text.c_str());

    // Enable the view, possibly after an error
    gtk_widget_set_sensitive (info->account_match_view, true);
    gtk_widget_set_sensitive (info->account_match_btn, true);

    /* Enable the Forward Assistant Button */
       gtk_assistant_set_page_complete (assistant, info->account_match_page,
               import_account_check_all (store));
}


void
csv_tximp_assist_doc_page_prepare (GtkAssistant *assistant,
        CsvImportTrans* info)
{
    /* Block going back */
    gtk_assistant_commit (info->csv_imp_asst);

    /* Before creating transactions, if this is a new book, let user specify
     * book options, since they affect how transactions are created */
    if (info->new_book)
        info->new_book = gnc_new_book_option_display (GTK_WIDGET(info->csv_imp_asst));

    if (!info->match_parse_run)
    {
        /* Add the Cancel button for the matcher */
        info->cancel_button = gtk_button_new_with_mnemonic (_("_Cancel"));
        gtk_assistant_add_action_widget (assistant, info->cancel_button);
        g_signal_connect (info->cancel_button, "clicked",
                         G_CALLBACK(csv_tximp_assist_cancel_cb), info);
        gtk_widget_show (GTK_WIDGET(info->cancel_button));
    }
    info->match_parse_run = true;
}


void
csv_tximp_assist_match_page_prepare (GtkAssistant *assistant,
        CsvImportTrans* info)
{
    /* Block going back */
    gtk_assistant_commit (info->csv_imp_asst);

    if (!info->tx_imp->m_parse_errors || info->skip_errors)
    {
        auto text = std::string( "<span size=\"medium\" color=\"red\"><b>");
        text += _("Double click on rows to change, then click on Apply to Import");
        text += "</b></span>";
        gtk_label_set_markup (GTK_LABEL(info->match_label), text.c_str());

        if (!info->gnc_csv_importer_gui)
        {
            /* Create the generic transaction importer GUI. */
            info->gnc_csv_importer_gui = gnc_gen_trans_assist_new (info->match_page, nullptr, false, 42);

            /* Add the help button for the matcher */
            info->help_button = gtk_button_new_with_mnemonic (_("_Help"));
            gtk_assistant_add_action_widget (assistant, info->help_button);
            g_signal_connect (info->help_button, "clicked",
                             G_CALLBACK(on_matcher_help_clicked), info->gnc_csv_importer_gui);
            gtk_widget_show (GTK_WIDGET(info->help_button));

            /* Copy all of the transactions to the importer GUI. */
            for (auto trans_it : info->tx_imp->m_transactions)
            {
                auto draft_trans = trans_it.second;
                if (draft_trans->trans)
                {
                    gnc_gen_trans_list_add_trans (info->gnc_csv_importer_gui, draft_trans->trans);
                    draft_trans->trans = nullptr;
                }
            }
        }
    }
    /* Enable the Forward Assistant Button */
    gtk_assistant_set_page_complete (assistant, info->match_page, true);
}


void
csv_tximp_assist_summary_page_prepare (GtkAssistant *assistant,
        CsvImportTrans* info)
{
    /* Remove the added buttons */
    gtk_assistant_remove_action_widget (assistant, info->help_button);
    gtk_assistant_remove_action_widget (assistant, info->cancel_button);

    auto text = std::string("<span size=\"medium\"><b>");
    text += _("The transactions were imported from the file '") + info->file_name + "'.";
    text += "</b></span>";
    gtk_label_set_markup (GTK_LABEL(info->summary_label), text.c_str());
}


static gint
csv_tximp_forward_page_func (gint current_page, gpointer user_data)
{
    auto info = (CsvImportTrans*)user_data;

    /* Note: This function gets called multiple times by the GtkAssistant code and so
       as we need to run tests only once I have used a counter that gets incremented on
       every call but the tests only get run when the counter is at 1 */
    info->callcount = info->callcount + 1;

    auto next_page = info->next_page;
    switch (current_page)
    {
	case 0: //from start page
            if (info->callcount == 1)
            {
                next_page = 1;
                info->next_page = next_page;
            }
            break;

        case 1: //from file page
            if (info->callcount == 1)
            {
                next_page = 2;
                info->next_page = next_page;
            }
            break;

        case 2: //from preview page
            if (info->callcount == 1)
            {
                // Skip Errors set, goto to doc page
                if (info->skip_errors)
                    next_page = 4;
                // Check to see if we have an account / other account columns
                else if (info->tx_imp->check_for_column_type (GncTransPropType::ACCOUNT) ||
                        info->tx_imp->check_for_column_type (GncTransPropType::TACCOUNT))
                    next_page = 3;
                else
                    next_page = 4;

                info->next_page = next_page;
            }
            break;

        case 3: //from account match page
            if (info->callcount == 1)
            {
                next_page = 4;
                info->next_page = next_page;
            }
            break;

        case 4: //from doc page
            if (info->callcount == 1)
            {
                /* Create transactions from the parsed data, first time with false
                   Subsequent times with true */
                info->tx_imp->create_transactions (info->match_parse_run);

                /* if there are errors, we jump back to preview to correct */
                if (info->tx_imp->m_parse_errors && !info->skip_errors)
                {
                    info->previewing_errors = true; /* We're looking at errors. */
                    next_page = 2;
                }
                else
                    next_page = 5;

                info->next_page = next_page;
            }
            break;

        case 5: //from match page
            if (info->callcount == 1)
            {
                next_page = 6;
                info->next_page = next_page;
            }
            break;

        case 6: //from summary page
            if (info->callcount == 1)
            {
                next_page = 7;
                info->next_page = next_page;
            }
            break;

        default:
            next_page = -1;
    }
    return next_page;
}


void
csv_tximp_assist_prepare_cb (GtkAssistant *assistant, GtkWidget *page,
        CsvImportTrans* info)
{
    // Reset callcount on every prepare
    info->callcount = 0;

    if (page == info->start_page)
        csv_tximp_assist_start_page_prepare (assistant, info);
    else if (page == info->file_page)
        csv_tximp_assist_file_page_prepare (assistant, info);
    else if (page == info->preview_page)
        csv_tximp_assist_preview_page_prepare (assistant, info);
    else if (page == info->account_match_page)
        csv_tximp_assist_account_match_page_prepare (assistant, info);
    else if (page == info->doc_page)
        csv_tximp_assist_doc_page_prepare (assistant, info);
    else if (page == info->match_page)
        csv_tximp_assist_match_page_prepare (assistant, info);
    else if (page == info->summary_page)
        csv_tximp_assist_summary_page_prepare (assistant, info);
    else
        g_assert_not_reached();
}


/*******************************************************
 * Assistant call back functions
 *******************************************************/
void
csv_tximp_assist_destroy_cb (GtkWidget *object, CsvImportTrans* info)
{
    gnc_unregister_gui_component_by_data (ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS, info);
    g_free (info);
}

void
csv_tximp_assist_cancel_cb (GtkAssistant *assistant, CsvImportTrans* info)
{
    if (info->gnc_csv_importer_gui)
        gnc_gen_trans_list_delete (info->gnc_csv_importer_gui);

    gnc_close_gui_component_by_data (ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS, info);
}

void
csv_tximp_assist_close_cb (GtkAssistant *assistant, CsvImportTrans* info)
{
    gnc_close_gui_component_by_data (ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS, info);
}

void
csv_tximp_assist_finish_cb (GtkAssistant *assistant, CsvImportTrans* info)
{
    /* Start the import */
    if (!info->tx_imp->m_transactions.empty())
        gnc_gen_trans_assist_start (info->gnc_csv_importer_gui);
    else
        gnc_gen_trans_list_delete (info->gnc_csv_importer_gui);
}

static void
csv_tximp_close_handler (gpointer user_data)
{
    auto info = (CsvImportTrans*)user_data;

    /* Free the memory we allocated. */
    if (info->tx_imp)
        delete info->tx_imp;

    if (info->gnc_csv_importer_gui)
        info->gnc_csv_importer_gui = nullptr;

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->csv_imp_asst));
    gtk_widget_destroy (GTK_WIDGET(info->csv_imp_asst));
}

/*******************************************************
 * Create the Assistant
 *******************************************************/
static void
csv_tximp_assist_create (CsvImportTrans *info)
{
    auto builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "start_row_adj");
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "end_row_adj");
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "account_match_store");
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "CSV Transaction Assistant");
    info->csv_imp_asst = GTK_ASSISTANT(gtk_builder_get_object (builder, "CSV Transaction Assistant"));

    /* Set the forward function */
    gtk_assistant_set_forward_page_func (info->csv_imp_asst, csv_tximp_forward_page_func, info, nullptr);

    /* Enable buttons on all page. */
    gtk_assistant_set_page_complete (info->csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "start_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (info->csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "file_page")),
                                     FALSE);
    gtk_assistant_set_page_complete (info->csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "preview_page")),
                                     FALSE);
    gtk_assistant_set_page_complete (info->csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "account_match_page")),
                                     FALSE);
    gtk_assistant_set_page_complete (info->csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "doc_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (info->csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "match_page")),
                                     FALSE);
    gtk_assistant_set_page_complete (info->csv_imp_asst,
                                     GTK_WIDGET(gtk_builder_get_object (builder, "summary_page")),
                                     TRUE);

    /* Start Page */
    info->start_page = GTK_WIDGET(gtk_builder_get_object (builder, "start_page"));

    /* File chooser Page */
    info->file_page = GTK_WIDGET(gtk_builder_get_object (builder, "file_page"));
    info->file_chooser = gtk_file_chooser_widget_new (GTK_FILE_CHOOSER_ACTION_OPEN);
    g_signal_connect (G_OBJECT(info->file_chooser), "file-activated",
                      G_CALLBACK(csv_tximp_file_confirm_cb), info);
    auto button = gtk_button_new_from_stock (GTK_STOCK_OK);
    gtk_widget_set_size_request (button, 100, -1);
    gtk_widget_show (button);
    auto h_box = gtk_hbox_new (TRUE, 0);
    gtk_box_pack_start (GTK_BOX(h_box), button, FALSE, FALSE, 0);
    gtk_file_chooser_set_extra_widget (GTK_FILE_CHOOSER(info->file_chooser), h_box);
    g_signal_connect (G_OBJECT(button), "clicked",
                      G_CALLBACK(csv_tximp_file_confirm_cb), info);

    auto box = GTK_WIDGET(gtk_builder_get_object (builder, "file_page"));
    gtk_box_pack_start (GTK_BOX(box), info->file_chooser, TRUE, TRUE, 6);
    gtk_widget_show (info->file_chooser);

    /* Preview Settings Page */
    {
        info->preview_page = GTK_WIDGET(gtk_builder_get_object (builder, "preview_page"));

        // Add Settings combo
        auto settings_store = gtk_list_store_new (2, G_TYPE_POINTER, G_TYPE_STRING);
        info->settings_combo = GTK_COMBO_BOX(gtk_combo_box_new_with_model_and_entry (GTK_TREE_MODEL(settings_store)));
        gtk_combo_box_set_entry_text_column (GTK_COMBO_BOX(info->settings_combo), SET_NAME);
        gtk_combo_box_set_active (GTK_COMBO_BOX(info->settings_combo), 0);

        info->combo_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "combo_hbox"));
        gtk_box_pack_start (GTK_BOX(info->combo_hbox), GTK_WIDGET(info->settings_combo), true, true, 6);
        gtk_widget_show (GTK_WIDGET(info->settings_combo));

        g_signal_connect (G_OBJECT(info->settings_combo), "changed",
                         G_CALLBACK(csv_tximp_preview_settings_sel_changed_cb), (gpointer)info);

        // Additionally connect to the changed signal of the embedded GtkEntry
        auto emb_entry = gtk_bin_get_child (GTK_BIN (info->settings_combo));
        g_signal_connect (G_OBJECT(emb_entry), "changed",
                         G_CALLBACK(csv_tximp_preview_settings_text_changed_cb), (gpointer)info);

        // Add Save Settings button
        info->save_button = GTK_WIDGET(gtk_builder_get_object (builder, "save_settings"));

        // Add Delete Settings button
        info->del_button = GTK_WIDGET(gtk_builder_get_object (builder, "delete_settings"));

        /* The table containing the separator configuration widgets */
        info->start_row_spin = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "start_row"));
        info->end_row_spin = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "end_row"));
        info->skip_alt_rows_button = GTK_WIDGET(gtk_builder_get_object (builder, "skip_rows"));
        info->skip_errors_button = GTK_WIDGET(gtk_builder_get_object (builder, "skip_errors_button"));
        info->multi_split_cbutton = GTK_WIDGET(gtk_builder_get_object (builder, "multi_split_button"));

        /* Load the separator buttons from the glade builder file into the
         * info->sep_buttons array. */
        const char* sep_button_names[] = {
                "space_cbutton",
                "tab_cbutton",
                "comma_cbutton",
                "colon_cbutton",
                "semicolon_cbutton",
                "hyphen_cbutton"
            };
        for (int i = 0; i < SEP_NUM_OF_TYPES; i++)
            info->sep_button[i]
                = (GtkCheckButton*)GTK_WIDGET(gtk_builder_get_object (builder, sep_button_names[i]));

        /* Load and connect the custom separator checkbutton in the same way
         * as the other separator buttons. */
        info->custom_cbutton
            = (GtkCheckButton*)GTK_WIDGET(gtk_builder_get_object (builder, "custom_cbutton"));

        /* Load the entry for the custom separator entry. Connect it to the
         * sep_button_clicked event handler as well. */
        info->custom_entry = (GtkEntry*)GTK_WIDGET(gtk_builder_get_object (builder, "custom_entry"));

        /* Add account selection widget */
        info->acct_selector = gnc_account_sel_new();
        auto account_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "account_hbox"));
        gtk_box_pack_start (GTK_BOX(account_hbox), info->acct_selector, TRUE, TRUE, 6);
        gtk_widget_show (info->acct_selector);

        g_signal_connect(G_OBJECT(info->acct_selector), "account_sel_changed",
                         G_CALLBACK(csv_tximp_preview_acct_sel_cb), (gpointer)info);


        /* Create the encoding selector widget and add it to the assistant */
        info->encselector = GO_CHARMAP_SEL(go_charmap_sel_new(GO_CHARMAP_SEL_TO_UTF8));
        /* Connect the selector to the encoding_selected event handler. */
        g_signal_connect (G_OBJECT(info->encselector), "charmap_changed",
                         G_CALLBACK(csv_tximp_preview_enc_sel_cb), (gpointer)info);

        auto encoding_container = GTK_CONTAINER(gtk_builder_get_object (builder, "encoding_container"));
        gtk_container_add (encoding_container, GTK_WIDGET(info->encselector));
        gtk_widget_show_all (GTK_WIDGET(encoding_container));

        /* The instructions label and image */
        info->instructions_label = GTK_LABEL(gtk_builder_get_object (builder, "instructions_label"));
        info->instructions_image = GTK_IMAGE(gtk_builder_get_object (builder, "instructions_image"));

        /* Add in the date format combo box and hook it up to an event handler. */
        info->date_format_combo = GTK_COMBO_BOX_TEXT(gtk_combo_box_text_new());
        for (int i = 0; i < num_date_formats; i++)
        {
            gtk_combo_box_text_append_text (info->date_format_combo, _(date_format_user[i]));
        }
        gtk_combo_box_set_active (GTK_COMBO_BOX(info->date_format_combo), 0);
        g_signal_connect (G_OBJECT(info->date_format_combo), "changed",
                         G_CALLBACK(csv_tximp_preview_date_fmt_sel_cb), (gpointer)info);

        /* Add it to the assistant. */
        auto date_format_container = GTK_CONTAINER(gtk_builder_get_object (builder, "date_format_container"));
        gtk_container_add (date_format_container, GTK_WIDGET(info->date_format_combo));
        gtk_widget_show_all (GTK_WIDGET(date_format_container));

        /* Add in the currency format combo box and hook it up to an event handler. */
        info->currency_format_combo = GTK_COMBO_BOX_TEXT(gtk_combo_box_text_new());
        for (int i = 0; i < num_currency_formats; i++)
        {
            gtk_combo_box_text_append_text (info->currency_format_combo, _(currency_format_user[i]));
        }
        /* Default will the locale */
        gtk_combo_box_set_active (GTK_COMBO_BOX(info->currency_format_combo), 0);
        g_signal_connect (G_OBJECT(info->currency_format_combo), "changed",
                         G_CALLBACK(csv_tximp_preview_currency_fmt_sel_cb), (gpointer)info);

        /* Add it to the assistant. */
        auto currency_format_container = GTK_CONTAINER(gtk_builder_get_object (builder, "currency_format_container"));
        gtk_container_add (currency_format_container, GTK_WIDGET(info->currency_format_combo));
        gtk_widget_show_all (GTK_WIDGET(currency_format_container));

        /* Connect the CSV/Fixed-Width radio button event handler. */
        info->csv_button = GTK_WIDGET(gtk_builder_get_object (builder, "csv_button"));
        info->fixed_button = GTK_WIDGET(gtk_builder_get_object (builder, "fixed_button"));

        /* Load the data treeview and connect it to its resizing event handler. */
        info->treeview = (GtkTreeView*)GTK_WIDGET(gtk_builder_get_object (builder, "treeview"));

        /* Load the column type treeview. */
        info->ctreeview = (GtkTreeView*)GTK_WIDGET(gtk_builder_get_object (builder, "ctreeview"));

        /* This is true only after encoding_selected is called, so we must
         * set it initially to false. */
        info->encoding_selected_called = false;
    }

    /* Account Match Page */
    info->account_match_page  = GTK_WIDGET(gtk_builder_get_object (builder, "account_match_page"));
    info->account_match_view  = GTK_WIDGET(gtk_builder_get_object (builder, "account_match_view"));
    info->account_match_label = GTK_WIDGET(gtk_builder_get_object (builder, "account_match_label"));
    info->account_match_btn = GTK_WIDGET(gtk_builder_get_object (builder, "account_match_change"));

    /* Doc Page */
    info->doc_page = GTK_WIDGET(gtk_builder_get_object (builder, "doc_page"));

    /* Matcher page */
    info->match_page  = GTK_WIDGET(gtk_builder_get_object (builder, "match_page"));
    info->match_label = GTK_WIDGET(gtk_builder_get_object (builder, "match_label"));

    /* Summary Page */
    info->summary_page  = GTK_WIDGET(gtk_builder_get_object (builder, "summary_page"));
    info->summary_label = GTK_WIDGET(gtk_builder_get_object (builder, "summary_label"));

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->csv_imp_asst));

    gtk_builder_connect_signals (builder, info);
    g_object_unref (G_OBJECT(builder));
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
    CsvImportTrans *info;

    info = g_new0 (CsvImportTrans, 1);

    /* In order to trigger a book options display on the creation of a new book,
     * we need to detect when we are dealing with a new book. */
    info->new_book = gnc_is_new_book();

    csv_tximp_assist_create (info);

    gnc_register_gui_component (ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS,
                                nullptr, csv_tximp_close_handler,
                                info);

    gtk_widget_show_all (GTK_WIDGET(info->csv_imp_asst));

    gnc_window_adjust_for_screen (GTK_WINDOW(info->csv_imp_asst));
}
