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

    GtkWidget       *window;

    GtkWidget       *start_page;                    /**< Assistant start page widget */

    GtkWidget       *file_page;                     /**< Assistant file page widget */
    GtkWidget       *file_chooser;                  /**< The widget for the file chooser */
    std::string      file_name;                     /**< The import file name */
    std::string      error_text;                    /**< Error Text */

    GtkWidget       *preview_page;                  /**< Assistant preview page widget */
    GtkWidget       *settings_combo;                /**< The Settings Combo */
    GtkWidget       *save_button;                   /**< The Save Settings button */
    GtkWidget       *del_button;                    /**< The Delete Settings button */
    GtkWidget       *acct_selector;                 /**< The Account selector */
    GtkWidget       *combo_hbox;                    /**< The Settings Combo hbox */
    GtkWidget       *check_butt;                    /**< The widget for the check label button */
    GtkWidget       *start_row_spin;                /**< The widget for the start row spinner */
    GtkWidget       *end_row_spin;                  /**< The widget for the end row spinner */
    GtkWidget       *skip_rows;                     /**< The widget for Skip alternate rows from start row */
    GtkWidget       *csv_button;                    /**< The widget for the CSV button */
    GtkWidget       *fixed_button;                  /**< The widget for the Fixed Width button */
    GtkWidget       *multi_split_cbutton;           /**< The widget for Multi-split */

    GncTxImport     *parse_data;                    /**< The actual data we are previewing */
    GOCharmapSel    *encselector;                   /**< The widget for selecting the encoding */
    GtkCheckButton  *sep_buttons[SEP_NUM_OF_TYPES]; /**< Checkbuttons for common separators */
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
    int              code_encoding_calls;           /**< Normally this is 0. If the computer
                                                       * changes encselector, this is set to
                                                       * 2. encoding_selected is called twice,
                                                       * each time decrementing this by 1. */
    bool             skip_errors;                   /**< This is false until the user checks the skip errors. */
    int              fixed_context_col;             /**< The number of the column whose the user has clicked */
    int              fixed_context_dx;              /**< The horizontal coordinate of the pixel in the header of the column
                                                       * the user has clicked */

    GtkWidget            *account_match_page;       /**< Assistant account matcher page widget */
    GtkWidget            *account_match_view;       /**< Assistant account matcher view widget */
    GtkWidget            *account_match_label;      /**< Assistant account matcher label widget */
    GtkWidget            *account_match_btn;        /**< Assistant account matcher button widget */

    GtkWidget            *doc_page;                 /**< Assistant doc page widget */

    GNCImportMainMatcher *gnc_csv_importer_gui;     /**< The GNCImportMainMatcher structure */
    GtkWidget            *match_page;               /**< Assistant match page widget, to be packed with the transaction matcher */
    GtkWidget            *match_label;              /**< The match label at the bottom of the page */
    GtkWidget            *help_button;              /**< The widget for the help button on the matcher page */
    GtkWidget            *cancel_button;            /**< The widget for the new cancel button when going back is blocked */
    bool                  match_parse_run = false;  /**< This is set after the first run */

    GtkWidget            *summary_page;             /**< Assistant summary page widget */
    GtkWidget            *summary_label;            /**< The summary text */

    bool                  new_book;                 /**< Are we importing into a new book?; if yes, call book options */
    int                   callcount;                /**< Number of times the assistant page forward function called */
    int                   next_page;                /**< The saved assistant next page number */

};


/*************************************************************************/

extern "C"
{
void csv_import_trans_assistant_prepare (GtkAssistant  *assistant, GtkWidget *page, gpointer user_data);
void csv_import_trans_assistant_finish (GtkAssistant *gtkassistant, gpointer user_data);
void csv_import_trans_assistant_cancel (GtkAssistant *gtkassistant, gpointer user_data);
void csv_import_trans_assistant_close (GtkAssistant *gtkassistant, gpointer user_data);
void csv_import_trans_srow_cb (GtkWidget *spin, gpointer user_data);
void csv_import_trans_erow_cb (GtkWidget *spin, gpointer user_data);
void csv_import_trans_skip_errors_cb (GtkToggleButton *checkbox, gpointer user_data);
void csv_import_trans_skiprows_cb (GtkToggleButton *checkbox, gpointer user_data);
void csv_import_trans_multisplit_cb (GtkToggleButton *checkbox, gpointer user_data);
void csv_import_trans_auto_cb (GtkWidget *cb, gpointer user_data);
void csv_import_trans_file_chooser_confirm_cb (GtkWidget *button, CsvImportTrans *info);

void csv_import_trans_delete_settings_cb (GtkWidget *button, CsvImportTrans *info);
void csv_import_trans_save_settings_cb (GtkWidget *button, CsvImportTrans *info);
void csv_import_trans_changed_settings_cb (GtkWidget *combo, CsvImportTrans *info);
void csv_import_trans_changed_settings_text_cb (GtkWidget *entry, CsvImportTrans *info);
void sep_button_clicked (GtkWidget* widget, CsvImportTrans* info);
void account_selected_cb (GtkWidget* widget, CsvImportTrans* info);
}

void csv_import_trans_assistant_start_page_prepare (GtkAssistant *gtkassistant, gpointer user_data);
void csv_import_trans_assistant_file_page_prepare (GtkAssistant *assistant, gpointer user_data);
void csv_import_trans_assistant_preview_page_prepare (GtkAssistant *gtkassistant, gpointer user_data);
void csv_import_trans_assistant_account_match_page_prepare (GtkAssistant *assistant, gpointer user_data);
void csv_import_trans_assistant_doc_page_prepare (GtkAssistant *assistant, gpointer user_data);
void csv_import_trans_assistant_match_page_prepare (GtkAssistant *assistant, gpointer user_data);
void csv_import_trans_assistant_summary_page_prepare (GtkAssistant *assistant, gpointer user_data);

void csv_import_preview_refresh_table (CsvImportTrans* info);
void csv_import_preview_refresh (CsvImportTrans *info);
void csv_import_validate_preview_settings (CsvImportTrans* info);
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
    GtkTreeModel *model = gtk_combo_box_get_model (combo);
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
        gtk_list_store_set (GTK_LIST_STORE(model), &iter, SET_GROUP, preset.get(), SET_NAME, preset->name.c_str(), -1);
    }
}


static void handle_save_del_sensitivity (GtkWidget* combo, CsvImportTrans *info)
{
    GtkTreeIter iter;
    auto can_delete = false;
    auto can_save = false;
    auto entry = gtk_bin_get_child (GTK_BIN(combo));
    auto entry_text = gtk_entry_get_text (GTK_ENTRY(entry));
    /* Handle sensitivity of the delete button and save button */
    auto got_iter = gtk_combo_box_get_active_iter (GTK_COMBO_BOX(combo), &iter);
    if (got_iter)
    {
        CsvTransSettings *preset;
        GtkTreeModel *model = gtk_combo_box_get_model (GTK_COMBO_BOX(combo));
        gtk_tree_model_get (model, &iter, SET_GROUP, &preset, -1);

        if (preset && !trans_preset_is_reserved_name (preset->name))
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
 * csv_import_trans_changed_settings_cb
 *
 * Triggered when a preset is selected in the settings combo.
 *******************************************************/
void
csv_import_trans_changed_settings_cb (GtkWidget *combo, CsvImportTrans *info)
{
    // Get the Active Selection
    GtkTreeIter iter;
    if (!gtk_combo_box_get_active_iter (GTK_COMBO_BOX(info->settings_combo), &iter))
        return;

    CsvTransSettings *preset = nullptr;
    GtkTreeModel *model = gtk_combo_box_get_model (GTK_COMBO_BOX(info->settings_combo));
    gtk_tree_model_get (model, &iter, SET_GROUP, &preset, -1);

    if (!preset)
        return;

    info->parse_data->settings (*preset);
    if (preset->load_error)
    {
        const gchar *title = _("Load the Import Settings.");
        GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(info->window),
                                    (GtkDialogFlags) 0,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    "%s", title);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
            "%s", _("There were problems reading some saved settings, continuing to load.\n Please review and save again."));
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
    }

    csv_import_preview_refresh (info);
    handle_save_del_sensitivity (combo, info);
    csv_import_validate_preview_settings (info);
}


/*******************************************************
 * csv_import_trans_changed_settings_text_cb
 *
 * Triggered while the user is editing text the settings combo.
 *******************************************************/
void
csv_import_trans_changed_settings_text_cb (GtkWidget *entry, CsvImportTrans *info)
{
    auto text = gtk_entry_get_text (GTK_ENTRY(entry));
    if (text)
        info->parse_data->settings_name(text);

    auto combo = gtk_widget_get_parent (entry);
    handle_save_del_sensitivity (combo, info);
}


/*******************************************************
 * csv_import_trans_delete_settings_cb
 *
 * call back to delete a settings entry
 *******************************************************/
void
csv_import_trans_delete_settings_cb (GtkWidget *button, CsvImportTrans *info)
{
    const gchar  *title = _("Delete the Import Settings.");

    // Get the Active Selection
    GtkTreeIter iter;
    if (!gtk_combo_box_get_active_iter (GTK_COMBO_BOX(info->settings_combo), &iter))
        return;

    CsvTransSettings *preset = nullptr;
    auto model = gtk_combo_box_get_model (GTK_COMBO_BOX(info->settings_combo));
    gtk_tree_model_get (model, &iter, SET_GROUP, &preset, -1);

    auto dialog = gtk_message_dialog_new (GTK_WINDOW(info->window),
                                (GtkDialogFlags) 0,
                                GTK_MESSAGE_QUESTION,
                                GTK_BUTTONS_OK_CANCEL,
                                "%s", title);
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
        "%s", _("Do you really want to delete the selection?"));
    auto response = gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);

    if (response == GTK_RESPONSE_OK)
    {
        preset->remove();
        populate_settings_combo(GTK_COMBO_BOX(info->settings_combo));
        gtk_combo_box_set_active (GTK_COMBO_BOX(info->settings_combo), 0); // Default
        csv_import_preview_refresh (info); // Reset the widgets
    }
}

/*******************************************************
 * csv_import_trans_save_settings_cb
 *
 * Save the settings to a Key File.
 *******************************************************/
void
csv_import_trans_save_settings_cb (GtkWidget *button, CsvImportTrans *info)
{
    auto title = _("Save the Import Settings.");
    auto new_name = info->parse_data->settings_name();

    /* Check if the entry text matches an already existing preset */
    GtkTreeIter iter;
    if (!gtk_combo_box_get_active_iter (GTK_COMBO_BOX(info->settings_combo), &iter))
    {

        GtkTreeModel *model = gtk_combo_box_get_model (GTK_COMBO_BOX(info->settings_combo));
        bool valid = gtk_tree_model_get_iter_first (model, &iter);
        while (valid)
        {
            // Walk through the list, reading each row
            CsvTransSettings *preset;
            gtk_tree_model_get (model, &iter, SET_GROUP, &preset, -1);

            if (preset && (preset->name == std::string(new_name)))
            {
                auto dialog = gtk_message_dialog_new (GTK_WINDOW(info->window),
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
    if (!info->parse_data->save_settings())
    {
        auto dialog = gtk_message_dialog_new (GTK_WINDOW(info->window),
                                    (GtkDialogFlags) 0,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_OK,
                                    "%s", title);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
            "%s", _("The settings have been saved."));
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);

        // Update the settings store
        populate_settings_combo(GTK_COMBO_BOX(info->settings_combo));
        auto model = gtk_combo_box_get_model (GTK_COMBO_BOX(info->settings_combo));

        // Get the first entry in model
        GtkTreeIter   iter;
        bool valid = gtk_tree_model_get_iter_first (model, &iter);
        while (valid)
        {
            gchar *name = nullptr;

            // Walk through the list, reading each row
            gtk_tree_model_get (model, &iter, SET_NAME, &name, -1);

            if (g_strcmp0 (name, new_name.c_str()) == 0) // Set Active, the one Saved.
                gtk_combo_box_set_active_iter (GTK_COMBO_BOX(info->settings_combo), &iter);

            g_free (name);

            valid = gtk_tree_model_iter_next (model, &iter);
        }
    }
    else
    {
        auto dialog = gtk_message_dialog_new (GTK_WINDOW(info->window),
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
 * csv_import_trans_file_chooser_confirm_cb
 *
 * call back for ok button in file chooser widget
 **************************************************/
void
csv_import_trans_file_chooser_confirm_cb (GtkWidget *button, CsvImportTrans *info)
{
    GtkAssistant *assistant = GTK_ASSISTANT(info->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    gtk_assistant_set_page_complete (assistant, page, FALSE);

    auto file_name = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER(info->file_chooser));
    if (!file_name)
        return;

    gchar *filepath = gnc_uri_get_path (file_name);
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
        gnc_error_dialog (NULL, "%s", e.what());
            delete parse_data;
            return;
    }
    catch (std::range_error &e)
    {
        /* Parsing failed ... */
        gnc_error_dialog (NULL, "%s", e.what());
        delete parse_data;
        return;
    }

    if (info->parse_data) // Free parse_data if we have come back here
        delete info->parse_data;
    info->parse_data = parse_data;
    info->previewing_errors = false; /* We're looking at all the data. */
    info->skip_errors = false; // Set skip_errors to False
    csv_import_preview_refresh (info);

    /* Get settings store and populate */
    populate_settings_combo(GTK_COMBO_BOX(info->settings_combo));
    gtk_combo_box_set_active (GTK_COMBO_BOX(info->settings_combo), 0);

    gtk_assistant_set_page_complete (assistant, page, TRUE);
    gtk_assistant_set_current_page (assistant, num + 1);
}


/**************************************************
 * row_selection_update
 *
 * refresh the start and end row highlighting
 **************************************************/
static
void row_selection_update (CsvImportTrans* info)
{
    GtkTreeIter iter;
    auto store = GTK_LIST_STORE(gtk_tree_view_get_model (info->treeview));

    /* Colorize rows that will be skipped */
    for (uint i = 0; i < info->parse_data->parsed_lines.size(); i++)
    {
        const char *color = NULL;
        if ((i < info->parse_data->skip_start_lines()) ||             // start rows to skip
            (i >= info->parse_data->parsed_lines.size()
                    - info->parse_data->skip_end_lines()) ||          // end rows to skip
            (((i - info->parse_data->skip_start_lines()) % 2 == 1) && // skip every second row...
             info->parse_data->skip_alt_lines()))                     // ...if requested
            color = "pink";
        else
            color = NULL;                                           // all other rows

        bool valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(store), &iter, NULL, i);
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
csv_import_preview_refresh (CsvImportTrans *info)
{
    GtkTreeIter   iter;

    // Set start row
    GtkAdjustment *adj = gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON(info->start_row_spin));
    gtk_adjustment_set_upper (adj, info->parse_data->parsed_lines.size());
    gtk_spin_button_set_value (GTK_SPIN_BUTTON(info->start_row_spin),
            info->parse_data->skip_start_lines());

    // Set end row
    adj = gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON(info->end_row_spin));
    gtk_adjustment_set_upper (adj, info->parse_data->parsed_lines.size());
    gtk_spin_button_set_value (GTK_SPIN_BUTTON(info->end_row_spin),
            info->parse_data->skip_end_lines());

    // Set Alternate rows
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->skip_rows), info->parse_data->skip_alt_lines());

    // Set multi-split indicator
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->multi_split_cbutton), info->parse_data->multi_split());

    // Set Import Format
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->csv_button),
            (info->parse_data->file_format() == GncImpFileFormat::CSV));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->fixed_button),
            (info->parse_data->file_format() != GncImpFileFormat::CSV));

    // This section deals with the combo's and character encoding
    gtk_combo_box_set_active (GTK_COMBO_BOX(info->date_format_combo), info->parse_data->date_format());
    gtk_combo_box_set_active (GTK_COMBO_BOX(info->currency_format_combo), info->parse_data->currency_format());
    go_charmap_sel_set_encoding (info->encselector, info->parse_data->encoding().c_str());

    gnc_account_sel_set_account(GNC_ACCOUNT_SEL(info->acct_selector),
            info->parse_data->base_account() , false);

    // Handle separator checkboxes and custom field, only relevant if the file format is csv
    if (info->parse_data->file_format() == GncImpFileFormat::CSV)
    {
        auto separators = info->parse_data->separators();
        const auto stock_sep_chars = std::string (" \t,:;-");
        for (int i = 0; i < SEP_NUM_OF_TYPES; i++)
            gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->sep_buttons[i]),
                separators.find (stock_sep_chars[i]) != std::string::npos);

        // If there are any other separators in the separators string,
        // add them as custom separators
        auto pos = separators.find_first_of (stock_sep_chars);
        while (!separators.empty() && pos != std::string::npos)
        {
            separators.erase(pos);
            pos = separators.find_first_of (" \t,:;-");
        }
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->custom_cbutton),
                !separators.empty());
        gtk_entry_set_text (GTK_ENTRY(info->custom_entry), separators.c_str());
    }

    // Repopulate the parsed data table
    csv_import_preview_refresh_table (info);
}



/*******************************************************
 * csv_import_trans_srow_cb
 *
 * call back for import start row
 *******************************************************/
void csv_import_trans_srow_cb (GtkWidget *spin, gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;
    GtkAdjustment *adj;

    /* Get number of lines to skip at the beginning */
    info->parse_data->skip_start_lines (gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spin)));

    /* And adjust maximum number of lines that can be skipped at the end accordingly */
    adj = gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON(info->end_row_spin));
    gtk_adjustment_set_upper (adj, info->parse_data->parsed_lines.size()
            - info->parse_data->skip_start_lines());

    /* Refresh the row highlighting */
    row_selection_update (info);

    csv_import_validate_preview_settings (info);
}


/*******************************************************
 * csv_import_trans_erow_cb
 *
 * call back for import end row
 *******************************************************/
void csv_import_trans_erow_cb (GtkWidget *spin, gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;
    GtkAdjustment *adj;

    /* Get number of lines to skip at the end */
    info->parse_data->skip_end_lines (gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spin)));

    /* And adjust maximum number of lines that can be skipped at the beginning accordingly */
    adj = gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON(info->start_row_spin));
    gtk_adjustment_set_upper (adj, info->parse_data->parsed_lines.size()
            - info->parse_data->skip_end_lines());

    /* Refresh the row highlighting */
    row_selection_update (info);

    csv_import_validate_preview_settings (info);
}


/*******************************************************
 * csv_import_trans_skip_errors_cb
 *
 * call back for Skip Errors
 *******************************************************/
void csv_import_trans_skip_errors_cb (GtkToggleButton *checkbox, gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;

    info->skip_errors = gtk_toggle_button_get_active (checkbox);
    csv_import_validate_preview_settings (info);
}


/*******************************************************
 * csv_import_trans_multisplit_cb
 *
 * call back for import multi-split checkbox
 *******************************************************/
void csv_import_trans_multisplit_cb (GtkToggleButton *checkbox, gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;

    info->parse_data->multi_split (gtk_toggle_button_get_active (checkbox));
    csv_import_preview_refresh_table (info);
    csv_import_validate_preview_settings (info);
}


/*******************************************************
 * csv_import_trans_skiprows_cb
 *
 * call back for import skip rows checkbox
 *******************************************************/
void csv_import_trans_skiprows_cb (GtkToggleButton *checkbox, gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;

    info->parse_data->skip_alt_lines (gtk_toggle_button_get_active (checkbox));
    row_selection_update (info);
    csv_import_validate_preview_settings (info);
}


/** Returns the cell renderer from a column in the preview's treeview.
 * @param info The display of the data being imported
 * @param col The number of the column whose cell renderer is being retrieved
 * @return The cell renderer of column number col
 */
static GtkCellRenderer* gnc_csv_preview_get_cell_renderer (CsvImportTrans* info, int col)
{
    GList* renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT(gtk_tree_view_get_column (info->treeview, col)));

    GtkCellRenderer* cell = GTK_CELL_RENDERER(renderers->data);
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
void sep_button_clicked (GtkWidget* widget, CsvImportTrans* info)
{

    /* Only manipulate separator characters if the currently open file is
     * csv separated. */
    if (info->parse_data->file_format() != GncImpFileFormat::CSV)
        return;

    /* Add the corresponding characters to checked_separators for each
     * button that is checked. */
    std::string checked_separators;
    const auto stock_sep_chars = std::string (" \t,:;-");
    for (int i = 0; i < SEP_NUM_OF_TYPES; i++)
    {
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(info->sep_buttons[i])))
            checked_separators += stock_sep_chars[i];
    }

    /* Add the custom separator if the user checked its button. */
    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(info->custom_cbutton)))
    {
        char* custom_sep = (char*)gtk_entry_get_text (info->custom_entry);
        if (custom_sep[0] != '\0') /* Don't add a blank separator (bad things will happen!). */
            checked_separators += custom_sep;
    }

    /* Set the parse options using the checked_separators list. */
    info->parse_data->separators (checked_separators);

    /* Parse the data using the new options. We don't want to reguess
     * the column types because we want to leave the user's
     * configurations intact. */
    try
    {
        info->parse_data->tokenize (false);
        csv_import_preview_refresh_table (info);
        csv_import_validate_preview_settings (info);
    }
    catch (std::range_error &e)
    {
        /* Warn the user there was a problem and try to undo what caused
         * the error. (This will cause a reparsing and ideally a usable
         * configuration.) */
        gnc_error_dialog (NULL, "Error in parsing");
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

void account_selected_cb (GtkWidget* widget, CsvImportTrans* info)
{

    auto acct = gnc_account_sel_get_account( GNC_ACCOUNT_SEL(widget) );
    info->parse_data->base_account(acct);
    csv_import_preview_refresh_table (info);
    csv_import_validate_preview_settings (info);
}


/** Event handler for clicking one of the format type radio
 * buttons. This occurs if the format (Fixed-Width or CSV) is changed.
 * @param csv_button The "Separated" radio button
 * @param info The display of the data being imported
 */
static void separated_or_fixed_selected (GtkToggleButton* csv_button, CsvImportTrans* info)
{
    /* Set the parsing type correctly. */
    try
    {
        if (gtk_toggle_button_get_active (csv_button))
            info->parse_data->file_format (GncImpFileFormat::CSV);
        else
            info->parse_data->file_format (GncImpFileFormat::FIXED_WIDTH);

        info->parse_data->tokenize (false);
        csv_import_preview_refresh_table (info);
        csv_import_validate_preview_settings (info);
    }
    catch (std::range_error &e)
    {
        /* Parsing failed ... */
        gnc_error_dialog (NULL, "%s", e.what());
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
static void encoding_selected (GOCharmapSel* selector, const char* encoding,
                              CsvImportTrans* info)
{
    /* This gets called twice every time a new encoding is selected. The
     * second call actually passes the correct data; thus, we only do
     * something the second time this is called. */

    /* Prevent code-caused calls of this function from having an impact. */
    if (info->code_encoding_calls > 0)
    {
        info->code_encoding_calls--;
        return;
    }

    /* If this is the second time the function is called ... */
    if (info->encoding_selected_called)
    {
        std::string previous_encoding = info->parse_data->tokenizer->encoding();
        /* Try converting the new encoding and reparsing. */
        try
        {
            info->parse_data->encoding (encoding);
            info->parse_data->tokenize (false);

            csv_import_preview_refresh_table (info);
            csv_import_validate_preview_settings (info);

            info->encoding_selected_called = false;
        }
        catch (...)
        {
            /* If it fails, change back to the old encoding. */
            gnc_error_dialog (NULL, "%s", _("Invalid encoding selected"));
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
static void date_format_selected (GtkComboBox* format_selector, CsvImportTrans* info)
{
    info->parse_data->date_format (gtk_combo_box_get_active (format_selector));
    csv_import_validate_preview_settings (info);
}


/** Event handler for selecting a new currency format.
 * @param currency_selector The combo box for selecting currency formats
 * @param info The display of the data being imported
 */
static void currency_format_selected (GtkComboBox* currency_selector, CsvImportTrans* info)
{
    info->parse_data->currency_format (gtk_combo_box_get_active (currency_selector));
    csv_import_validate_preview_settings (info);
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
    { "", NULL, 0, 0, 0 },
    {
        N_("_Split this column"), NULL,
        0, 1 << CONTEXT_STF_IMPORT_SPLIT, CONTEXT_STF_IMPORT_SPLIT
    },
    { "", NULL, 0, 0, 0 },
    {
        N_("_Widen this column"), GTK_STOCK_GO_FORWARD,
        0, 1 << CONTEXT_STF_IMPORT_WIDEN, CONTEXT_STF_IMPORT_WIDEN
    },
    {
        N_("_Narrow this column"), GTK_STOCK_GO_BACK,
        0, 1 << CONTEXT_STF_IMPORT_NARROW, CONTEXT_STF_IMPORT_NARROW
    },
    { NULL, NULL, 0, 0, 0 },
};

static uint get_new_col_rel_pos (CsvImportTrans* info, int col, int dx)
{
    PangoFontDescription *font_desc;
    int width;
    uint charindex;
    GtkCellRenderer *cell = gnc_csv_preview_get_cell_renderer (info, col);
    g_object_get (G_OBJECT(cell), "font_desc", &font_desc, NULL);

    PangoLayout *layout = gtk_widget_create_pango_layout (GTK_WIDGET(info->treeview), "x");
    pango_layout_set_font_description (layout, font_desc);
    pango_layout_get_pixel_size (layout, &width, NULL);
    if (width < 1) width = 1;
    charindex = (dx + width / 2) / width;
    g_object_unref (layout);
    pango_font_description_free (font_desc);

    return charindex;
}

static gboolean
fixed_context_menu_handler (GnumericPopupMenuElement const *element,
                            gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;
    int col = info->fixed_context_col;
    uint rel_pos = get_new_col_rel_pos (info, col, info->fixed_context_dx);
    GncFwTokenizer *fwtok = dynamic_cast<GncFwTokenizer*>(info->parse_data->tokenizer.get());

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
        info->parse_data->tokenize (false);
    }
    catch(std::range_error& e)
    {
        gnc_error_dialog (NULL, "%s", e.what());
        return FALSE;
    }
    csv_import_preview_refresh_table (info);
    csv_import_validate_preview_settings (info);
    return TRUE;
}
static void
select_column (CsvImportTrans* info, int col)
{
    GtkTreeViewColumn *column;

    if (col < 0)
        return;

    column = gtk_tree_view_get_column (info->treeview, col);
    gtk_widget_grab_focus (column->button);
}

static void
fixed_context_menu (CsvImportTrans* info, GdkEventButton *event,
                    int col, int dx)
{
    int sensitivity_filter = 0;

    GncFwTokenizer *fwtok = dynamic_cast<GncFwTokenizer*>(info->parse_data->tokenizer.get());
    info->fixed_context_col = col;
    info->fixed_context_dx = dx;
    uint rel_pos = get_new_col_rel_pos (info, col, dx);

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
static void treeview_resized (GtkWidget* widget, GtkAllocation* allocation, CsvImportTrans* info)
{
    /* Go through each column except for the last. (We don't want to set
     * the width of the last column because the user won't be able to
     * shrink the dialog back if it's expanded.) */
    for (uint i = 0; i < info->parse_data->column_types().size() - 1; i++)
    {
        gint col_width; /* The width of the column in info->treeview. */
        GtkTreeViewColumn* pcol;
        GtkTreeViewColumn* ccol; /* The corresponding column in info->ctreeview. */

        /* Get the width. */
        col_width = gtk_tree_view_column_get_width (gtk_tree_view_get_column (info->treeview, i));
        /* Set the minimum width for a column so that drop down selector can be seen. */
        if (col_width < MIN_COL_WIDTH)
        {
            col_width = MIN_COL_WIDTH;
        }
        pcol = gtk_tree_view_get_column (info->treeview, i);
        gtk_tree_view_column_set_min_width (pcol, col_width);
        /* Set ccol's width the same. */
        ccol = gtk_tree_view_get_column (info->ctreeview, i);
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
static void column_type_changed (GtkCellRenderer* renderer, gchar* path,
                                GtkTreeIter* new_text_iter, CsvImportTrans* info)
{
    GtkTreeModel* model;
    gchar* new_text;
    auto new_col_type = GncTransPropType::NONE;

    /* Get the new text */
    auto col_num = GPOINTER_TO_UINT (g_object_get_data (G_OBJECT(renderer), "col-num"));
    g_object_get (renderer, "model", &model, NULL);
    gtk_tree_model_get (model, new_text_iter,
            1, &new_col_type, // Invisible column in the combobox' model containing the column type
            -1);

    info->parse_data->set_column_type (col_num, new_col_type);
    csv_import_preview_refresh_table (info);
    csv_import_validate_preview_settings (info);
}

static void
split_column (CsvImportTrans* info, int col, int dx)
{
    uint rel_pos = get_new_col_rel_pos (info, col, dx);

    GncFwTokenizer *fwtok = dynamic_cast<GncFwTokenizer*>(info->parse_data->tokenizer.get());
    fwtok->col_split (col, rel_pos);
    try
    {
        info->parse_data->tokenize (false);
    }
    catch (std::range_error& e)
    {
        gnc_error_dialog (NULL, "%s", e.what());
        return;
    }
    csv_import_preview_refresh_table (info);
    csv_import_validate_preview_settings (info);
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
    if (col == info->parse_data->column_types().size())
        return;

    /*  calculate offset to compensate for the button indentation. */
    GtkAllocation alloc;
    gtk_widget_get_allocation (gtk_bin_get_child (GTK_BIN(button)), &alloc);
    auto offset = alloc.x - alloc.x;
    /* Double clicks can split columns. */
    if (event->type == GDK_2BUTTON_PRESS && event->button == 1)
    {
        split_column (info, col, (int)event->x - offset);
    }
    /* Right clicking brings up a context menu. */
    else if (event->type == GDK_BUTTON_PRESS && event->button == 3)
    {
        fixed_context_menu (info, event, col, (int)event->x - offset);
    }
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
    bool valid;

    valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL(liststore), &iter);
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
    bool     have_accounts = false;

    /* ctstore contains the actual strings appearing in the column types treeview. */
    GtkTreeModel* ctstore = gtk_tree_view_get_model (info->ctreeview);
    /* datastore contains the actual strings appearing in the preview treeview. */
    GtkTreeModel* datastore = gtk_tree_view_get_model (info->treeview);

    GtkTreeIter iter1, iter2, iter3;

    /* Get an iterator for the first (and only) row of the column store. */
    gtk_tree_model_get_iter_first (ctstore, &iter1);

    for (uint j = info->parse_data->skip_start_lines();
            j < info->parse_data->parsed_lines.size() - info->parse_data->skip_end_lines() - 1;
            j++)
    {
        /* Go through each of the columns. */
        for (uint i = 0; i < info->parse_data->column_types().size(); i++)
        {
            gchar* accstr = NULL;   /* The string in this column from datastore. */
            auto col_type = GncTransPropType::NONE;

            /* Get the column type. Store is arranged so that every two
             * columns is a pair of
             * - the column type as a user visible (translated) string
             * - the internal type for this column
             * So store looks like:
             * col_type_str 0, col_type, col_type_str 1, col_type 1, ..., col_type_str ncols, col_type ncols. */
            gtk_tree_model_get (ctstore, &iter1, 2 * i + 1, &col_type, -1);

            /* We're only interested in columns of type ACCOUNT and OACCOUNT. */
            if ((col_type == GncTransPropType::ACCOUNT) || (col_type == GncTransPropType::TACCOUNT))
            {
                /* Get an iterator for the row in the data store. */
                if (gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(datastore), &iter2, NULL, j))
                {
                    gtk_tree_model_get (datastore, &iter2, i + 1, &accstr, -1);
                    if (!accstr || *accstr == '\0')
                        continue;

                    // Append the entry
                    if (!check_for_duplicates (GTK_LIST_STORE(store), accstr))
                    {
                        gtk_list_store_append (GTK_LIST_STORE(store), &iter3);
                        gtk_list_store_set (GTK_LIST_STORE(store), &iter3, MAPPING_STRING, accstr,
                                            MAPPING_FULLPATH, _("No Linked Account"), MAPPING_ACCOUNT, NULL, -1);
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
void csv_import_preview_refresh_table (CsvImportTrans* info)
{
    /* ncols is the number of columns in the file data. */
    auto column_types = info->parse_data->column_types();
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
    GType* headertypes = g_new (GType, 2 * ncols + 1);
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
    GType* bodytypes = g_new (GType, ncols + 2);
    for (guint i = 0; i <  ncols + 2; i++)
        bodytypes[i] = G_TYPE_STRING;
    auto store = gtk_list_store_newv (ncols + 2, bodytypes);
    g_free (bodytypes);

    /* Fill the data liststore with data from the file. */
    for (auto parse_line : info->parse_data->parsed_lines)
    {
        // When previewing errors skip all lines that don't have errors
        if (info->previewing_errors && std::get<1>(parse_line).empty())
            continue;

        GtkTreeIter iter;
        gtk_list_store_append (store, &iter);

        /* Row Color column */
        gtk_list_store_set (store, &iter, 0, NULL, -1);

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
        if (sanitize_trans_prop(col_type.first, info->parse_data->multi_split()) == col_type.first)
        {
            GtkTreeIter iter;
            gtk_list_store_append (combostore, &iter);
            gtk_list_store_set (combostore, &iter, 0, _(col_type.second),
                                                   1, static_cast<int>(col_type.first),
                                                   -1);
        }
    }

    /* Insert columns into the data and column type treeviews. */
    for (guint i = 0; i < ncols ; i++)
    {
        /* The header cells are combobox entries. They all use the same
         * common model for the dropdown list while their text value
         * comes from the header liststore (ctstore). */
        auto crenderer = gtk_cell_renderer_combo_new();
        /* Set the properties for the dropdown list */
        g_object_set (G_OBJECT(crenderer), "model", combostore, "text-column", 0,
                     "editable", TRUE, "has-entry", FALSE, NULL);
        g_object_set_data (G_OBJECT(crenderer),
                           "col-num", GUINT_TO_POINTER(i));
        g_signal_connect (G_OBJECT(crenderer), "changed",
                         G_CALLBACK(column_type_changed), (gpointer)info);
        /* Insert the column */
        gtk_tree_view_insert_column_with_attributes (info->ctreeview,
                -1, "", crenderer, "text", 2 * i, NULL);

        /* The file data treeview cells are simple text cells. */
        auto renderer = gtk_cell_renderer_text_new();
        /* We want a monospace font for the data in case of fixed-width data. */
        g_object_set (G_OBJECT(renderer), "family", "monospace", NULL);

        /* Add a single column for the treeview. */
        auto col = gtk_tree_view_column_new_with_attributes ("", renderer, "background", 0,
                                                             "text", i + 1, NULL);
        gtk_tree_view_insert_column (info->treeview, col, -1);
        /* Enable resizing of the columns. */
        gtk_tree_view_column_set_resizable (col, TRUE);

        /* We need to allow clicking on the file data's column headers for
         * fixed-width column splitting and merging. */
        g_object_set (G_OBJECT(col), "clickable", TRUE, NULL);
        g_signal_connect (G_OBJECT(col->button), "button_press_event",
                         G_CALLBACK(header_button_press_handler), (gpointer)info);
        /* Store the column number in the button to know which one was clicked later on */
        g_object_set_data (G_OBJECT(col->button), "col-num",GINT_TO_POINTER(i));
    }

    /* Add a column for the error messages when reviewing errors */
    if (info->previewing_errors)
    {
        auto crenderer = gtk_cell_renderer_text_new();
        gtk_tree_view_insert_column_with_attributes (info->ctreeview,
                -1, "", crenderer, "text", 2 * ncols, NULL);

        auto renderer = gtk_cell_renderer_text_new();
        auto col = gtk_tree_view_column_new_with_attributes ("", renderer, "background", 0,
                                                             "text", ncols + 1, NULL);
        gtk_tree_view_insert_column (info->treeview, col, -1);
        /* Enable resizing of the columns. */
        gtk_tree_view_column_set_resizable (col, TRUE);
    }

    /* Select the header row */
    gtk_tree_model_get_iter_first (GTK_TREE_MODEL(ctstore), &iter);
    auto selection = gtk_tree_view_get_selection (info->ctreeview);
    gtk_tree_selection_select_iter (selection, &iter);

    /* Release our reference for the stores to allow proper memory management. */
    g_object_unref (GTK_TREE_MODEL(store));
    g_object_unref (GTK_TREE_MODEL(ctstore));
    g_object_unref (GTK_TREE_MODEL(combostore));

    /* Make the things actually appear. */
    gtk_widget_show_all (GTK_WIDGET(info->treeview));
    gtk_widget_show_all (GTK_WIDGET(info->ctreeview));

    /* Update the row selection highlight */
    row_selection_update (info);

}


void csv_import_validate_preview_settings (CsvImportTrans* info)
{
    /* Allow the user to proceed only if there are no inconsistencies in the settings */
    auto error_msg = info->parse_data->verify();
    gtk_assistant_set_page_complete (GTK_ASSISTANT(info->window), info->preview_page, error_msg.empty());
    gtk_label_set_text(GTK_LABEL(info->instructions_label), error_msg.c_str());
    gtk_widget_set_visible (GTK_WIDGET(info->instructions_image), !error_msg.empty());
}

/*======================================================================*/
/*======================================================================*/

/*******************************************************
 * Assistant page prepare functions
 *******************************************************/
void
csv_import_trans_assistant_start_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;
    GtkTreeModel   *store;

    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    // Clear the treemodel list store
    store = gtk_tree_view_get_model (GTK_TREE_VIEW(info->account_match_view));
    gtk_list_store_clear (GTK_LIST_STORE(store));

    /* Enable the Assistant Buttons */
    gtk_assistant_set_page_complete (assistant, page, TRUE);
}


void
csv_import_trans_assistant_file_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;
    gint            num = gtk_assistant_get_current_page (assistant);
    GtkWidget      *page = gtk_assistant_get_nth_page (assistant, num);

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
    gtk_assistant_set_page_complete (assistant, page, FALSE);
}


void
csv_import_trans_assistant_preview_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;

    g_signal_connect (G_OBJECT(info->treeview), "size-allocate",
                     G_CALLBACK(treeview_resized), (gpointer)info);

    // Hide the skip errors check button
    gtk_widget_hide (GTK_WIDGET(info->check_butt));

    if (info->previewing_errors) // We are looking at errors to display
    {
        gchar* name;
        GtkIconSize size;

        /* Block going back */
        gtk_assistant_commit (GTK_ASSISTANT(info->window));

        gtk_image_get_stock (info->instructions_image, &name, &size);
        gtk_image_set_from_stock (info->instructions_image, GTK_STOCK_DIALOG_ERROR, size);
        gtk_label_set_text (info->instructions_label,
                           _("The rows displayed below had errors which are in the last column. You can attempt to correct them by changing the configuration."));
        gtk_widget_show (GTK_WIDGET(info->instructions_image));
        gtk_widget_show (GTK_WIDGET(info->instructions_label));

        /* Reset start and end row */
        gtk_spin_button_set_value (GTK_SPIN_BUTTON(info->start_row_spin), 0);
        gtk_spin_button_set_value (GTK_SPIN_BUTTON(info->end_row_spin), 0);

        /* Set spin buttons and settings combo hbox not sensitive */
        gtk_widget_set_sensitive (info->combo_hbox, FALSE);
        gtk_widget_set_sensitive (info->start_row_spin, FALSE);
        gtk_widget_set_sensitive (info->end_row_spin, FALSE);
        gtk_widget_set_sensitive (info->skip_rows, FALSE);
        gtk_widget_set_sensitive (info->multi_split_cbutton, FALSE);
        info->parse_data->skip_alt_lines (false);

        /* Show the skip errors check button */
        gtk_widget_show (GTK_WIDGET(info->check_butt));
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->check_butt), FALSE);
    }
    else
    {
        GtkTreeModel *store;

        // Load the account strings into the store
        store = gtk_tree_view_get_model (GTK_TREE_VIEW(info->account_match_view));
        gtk_list_store_clear (GTK_LIST_STORE(store)); // Clear list of accounts unless we are looking at errors
    }

    /* Disable the Forward Assistant Button */
    auto num = gtk_assistant_get_current_page (assistant);
    auto page = gtk_assistant_get_nth_page (assistant, num);
    gtk_assistant_set_page_complete (assistant, page, false);

    /* Load the data into the treeview. */
    csv_import_preview_refresh_table (info);
    csv_import_validate_preview_settings (info);

}


static bool
import_account_check_all (GtkTreeModel *model)
{
    GtkTreeIter iter;
    bool valid, ret = true;

    // Set iter to first entry of store
    valid = gtk_tree_model_get_iter_first (model, &iter);

    // Walk through the store looking for Null accounts
    while (valid)
    {
        Account *account;

        // Walk through the list, reading each row
        gtk_tree_model_get (model, &iter, MAPPING_ACCOUNT, &account, -1);

        if (account == NULL)
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
    QofBook     *book;
    const gchar *sep;
    const gchar *newsep;
    gchar      **names;
    gchar      **ptr = NULL;
    gint         i, count = 0;
    gchar       *ret_string, *last_part = NULL;;

    // Get the book
    book = gnc_get_current_book ();
    sep = gnc_get_account_separator_string ();

    // Setup an alternative separator
    if (g_strcmp0 (sep,":") == 0)
        newsep = "-";
    else
        newsep = ":";

    // Split the incoming string by current separator
    names = g_strsplit (text, sep, -1);

    /* find the last_part and count parts */
    for (ptr = names; *ptr; ptr++)
    {
        if (g_strcmp0 (*ptr,"") != 0) //separator is last in string
        {
            g_free (last_part);
            last_part = g_strdup (*ptr);
            count = count + 1;
        }
    }

    // If count is 1 we have no path
    if (count == 1)
        ret_string = g_strdup (last_part);
    else
    {
        Account   *account;
        gchar     *sep_path, *newsep_path;

        // Start to create two paths based on current and possibly new separator
        sep_path = g_strdup (names[0]);
        newsep_path = g_strdup (names[0]);

        // Join the parts together apart from last_part which could be account name
        for (i = 1; i < count - 1; i++)
        {
            gchar *temp_sep_path, *temp_newsep_path;

            temp_sep_path = g_strdup (sep_path);
            temp_newsep_path = g_strdup (newsep_path);
            g_free (sep_path);
            g_free (newsep_path);
            sep_path = g_strconcat (temp_sep_path, sep, names[i], NULL);
            newsep_path = g_strconcat (temp_newsep_path, newsep, names[i], NULL);
            g_free (temp_sep_path);
            g_free (temp_newsep_path);
        }
        account = gnc_account_lookup_by_full_name (gnc_book_get_root_account (book), sep_path);

        if (account == NULL) // path not found
            ret_string = g_strconcat (newsep_path, newsep, last_part, NULL);
        else
            ret_string = g_strconcat (sep_path, sep, last_part, NULL);

        g_free (sep_path);
        g_free (newsep_path);
    }
    g_free (last_part);
    g_strfreev (names);
    return ret_string;
}


static void
import_account_select_cb (GtkWidget *widget, gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeIter iter;

    model = gtk_tree_view_get_model (GTK_TREE_VIEW(info->account_match_view));

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(info->account_match_view));

    if (gtk_tree_selection_get_selected (selection, &model, &iter))
    {
        Account *gnc_acc = NULL, *account = NULL;
        gchar *text = NULL;
        gchar *parsed_text = NULL;

        // Get the the String
        gtk_tree_model_get (model, &iter, MAPPING_STRING, &text, -1);

        // Get the pointer to the Account
        gtk_tree_model_get (model, &iter, MAPPING_ACCOUNT, &account, -1);

        parsed_text = import_account_text_parse (text);

        gnc_acc = gnc_import_select_account (NULL, NULL, 1, parsed_text, NULL, ACCT_TYPE_NONE, account, NULL);

        if (gnc_acc != NULL) // We may of canceled
        {
            gchar *fullpath = NULL;

            gtk_list_store_set (GTK_LIST_STORE(model), &iter, MAPPING_ACCOUNT, gnc_acc, -1);

            fullpath = gnc_account_get_full_name (gnc_acc);
            gtk_list_store_set (GTK_LIST_STORE(model), &iter, MAPPING_FULLPATH, fullpath, -1);

            // Update the account kvp mappings
            gnc_csv_account_map_change_mappings (account, gnc_acc, text);

            g_free (fullpath);
        }
        g_free (text);
        g_free (parsed_text);
    }
    if (import_account_check_all (model))
        gtk_assistant_set_page_complete (GTK_ASSISTANT(info->window), info->account_match_page, TRUE);
}


/* This is the callback for the mouse click */
static bool
import_account_button_cb (GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    CsvImportTrans     *info = (CsvImportTrans*) user_data;
    GtkTreeModel       *model;
    GtkTreeIter         iter;
    GtkTreePath        *path;
    GtkTreeViewColumn  *col;

    model = gtk_tree_view_get_model (GTK_TREE_VIEW(info->account_match_view));

    /* This is for a double click */
    if (event->button == 1 && event->type == GDK_2BUTTON_PRESS)
    {
        GdkWindow *window = gtk_tree_view_get_bin_window (GTK_TREE_VIEW (info->account_match_view));

        if (event->window != window)
            return false;

        /* Get tree path for row that was clicked, true if row exists */
        if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (info->account_match_view), (gint) event->x, (gint) event->y,
                                             &path, &col, NULL, NULL))
        {
            DEBUG("event->x is %d and event->y is %d", (gint)event->x, (gint)event->y);

            if (gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &iter, path))
            {
                Account *gnc_acc = NULL, *account = NULL;
                gchar *text = NULL;
                gchar *parsed_text = NULL;

                // Get the the String
                gtk_tree_model_get (model, &iter, MAPPING_STRING, &text, -1);

                // Get the pointer to the Account
                gtk_tree_model_get (model, &iter, MAPPING_ACCOUNT, &account, -1);

                parsed_text = import_account_text_parse (text);

                gnc_acc = gnc_import_select_account (NULL, NULL, 1, parsed_text, NULL, ACCT_TYPE_NONE, account, NULL);

                if (gnc_acc != NULL) // We may of canceled
                {
                    gchar *fullpath = NULL;

                    gtk_list_store_set (GTK_LIST_STORE(model), &iter, MAPPING_ACCOUNT, gnc_acc, -1);

                    fullpath = gnc_account_get_full_name (gnc_acc);
                    gtk_list_store_set (GTK_LIST_STORE(model), &iter, MAPPING_FULLPATH, fullpath, -1);

                    // Update the account kvp mappings
                    gnc_csv_account_map_change_mappings (account, gnc_acc, text);

                    g_free (fullpath);
                }
                g_free (text);
                g_free (parsed_text);
            }
            gtk_tree_path_free (path);
        }
        if (import_account_check_all (model))
            gtk_assistant_set_page_complete (GTK_ASSISTANT(info->window), info->account_match_page, TRUE);

        return true;
    }
    return false;
}


void
csv_import_trans_assistant_account_match_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;
    gint            num = gtk_assistant_get_current_page (assistant);
    gchar          *text, *mtext;

    auto store = gtk_tree_view_get_model (GTK_TREE_VIEW(info->account_match_view));

    // Load the account strings into the store
    get_list_of_accounts (info, store);

    // Match the account strings to the mappings
    gnc_csv_account_map_load_mappings (store);

    text = g_strdup_printf (gettext ("To change mapping, Double Click on a row or select a row and press the button..."));
    mtext = g_strdup_printf ("<span size=\"medium\" color=\"red\"><b>%s</b></span>", text);
    gtk_label_set_markup (GTK_LABEL(info->account_match_label), mtext);
    g_free (mtext);
    g_free (text);

    // Enable the view, possibly after an error
    gtk_widget_set_sensitive (info->account_match_view, TRUE);
    gtk_widget_set_sensitive (info->account_match_btn, TRUE);

    /* Enable the Forward Assistant Button */
    if (import_account_check_all (store))
       gtk_assistant_set_page_complete (assistant, info->account_match_page, TRUE);
    else
        gtk_assistant_set_page_complete (assistant, info->account_match_page, FALSE);
}


void
csv_import_trans_assistant_doc_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;

    /* Block going back */
    gtk_assistant_commit (GTK_ASSISTANT(info->window));

    /* Before creating transactions, if this is a new book, let user specify
     * book options, since they affect how transactions are created */
    if (info->new_book)
        info->new_book = gnc_new_book_option_display (info->window);

    if (!info->match_parse_run)
    {
        /* Add the Cancel button for the matcher */
        info->cancel_button = gtk_button_new_with_mnemonic (_("_Cancel"));
        gtk_assistant_add_action_widget (assistant, info->cancel_button);
        g_signal_connect (info->cancel_button, "clicked",
                         G_CALLBACK(csv_import_trans_assistant_cancel), info);
        gtk_widget_show (GTK_WIDGET(info->cancel_button));
    }
    info->match_parse_run = true;
}


void
csv_import_trans_assistant_match_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);
    gchar *text, *mtext;

    /* Block going back */
    gtk_assistant_commit (GTK_ASSISTANT(info->window));

    if (!info->parse_data->parse_errors || info->skip_errors)
    {
        text =  _("Double click on rows to change, then click on Apply to Import");
        mtext = g_strdup_printf ("<span size=\"medium\" color=\"red\"><b>%s</b></span>", text);
        gtk_label_set_markup (GTK_LABEL(info->match_label), mtext);
        g_free (mtext);

        if (info->gnc_csv_importer_gui == NULL)
        {
            /* Create the generic transaction importer GUI. */
            info->gnc_csv_importer_gui = gnc_gen_trans_assist_new (info->match_page, NULL, FALSE, 42);

            /* Add the help button for the matcher */
            info->help_button = gtk_button_new_with_mnemonic (_("_Help"));
            gtk_assistant_add_action_widget (assistant, info->help_button);
            g_signal_connect (info->help_button, "clicked",
                             G_CALLBACK(on_matcher_help_clicked), info->gnc_csv_importer_gui);
            gtk_widget_show (GTK_WIDGET(info->help_button));

            /* Copy all of the transactions to the importer GUI. */
            for (auto trans_it : info->parse_data->transactions)
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
    gtk_assistant_set_page_complete (assistant, page, TRUE);
}


void
csv_import_trans_assistant_summary_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;
    gchar *text, *mtext;

    /* Remove the added button */
    gtk_assistant_remove_action_widget (assistant, info->help_button);
    gtk_assistant_remove_action_widget (assistant, info->cancel_button);

    text = g_strdup_printf (gettext ("The transactions were imported from the file '%s'."), info->file_name.c_str());
    mtext = g_strdup_printf ("<span size=\"medium\"><b>%s</b></span>", text);
    gtk_label_set_markup (GTK_LABEL(info->summary_label), mtext);
    g_free (text);
    g_free (mtext);
}


static gint
csv_import_trans_forward_page_func (gint current_page, gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;
    gint next_page = 0;

    /* Note: This function gets called multiple times by the GtkAssistant code and so
       as we need to run tests only once I have used a counter that gets incremented on
       every call but the tests only get run when the counter is at 1 */

    info->callcount = info->callcount + 1;

    switch (current_page)
    {
	case 0: //from start page
            if (info->callcount == 1)
            {
                next_page = 1;
                info->next_page = next_page;
            }
            else
                next_page = info->next_page;
            break;

        case 1: //from file page
            if (info->callcount == 1)
            {
                next_page = 2;
                info->next_page = next_page;
            }
            else
                next_page = info->next_page;
            break;

        case 2: //from preview page
            if (info->callcount == 1)
            {
                // Skip Errors set, goto to doc page
                if (info->skip_errors)
                    next_page = 4;
                // Check to see if we have an account / other account columns
                else if (info->parse_data->check_for_column_type (GncTransPropType::ACCOUNT) ||
                        info->parse_data->check_for_column_type (GncTransPropType::TACCOUNT))
                    next_page = 3;
                else
                    next_page = 4;

                info->next_page = next_page;
            }
            else
                next_page = info->next_page;
            break;

        case 3: //from account match page
            if (info->callcount == 1)
            {
                next_page = 4;
                info->next_page = next_page;
            }
            else
                next_page = info->next_page;
            break;

        case 4: //from doc page
            if (info->callcount == 1)
            {
                /* Create transactions from the parsed data, first time with false
                   Subsequent times with true */
                    info->parse_data->create_transactions (info->match_parse_run);

                /* if there are errors, we jump back to preview to correct */
                if (info->parse_data->parse_errors && !info->skip_errors)
                {
                    info->previewing_errors = true; /* We're looking at errors. */
                    next_page = 2;
                }
                else
                    next_page = 5;

                info->next_page = next_page;
            }
            else
                next_page = info->next_page;
            break;

        case 5: //from match page
            if (info->callcount == 1)
            {
                next_page = 6;
                info->next_page = next_page;
            }
            else
                next_page = info->next_page;
            break;

        case 6: //from summary page
            if (info->callcount == 1)
            {
                next_page = 7;
                info->next_page = next_page;
            }
            else
                next_page = info->next_page;
            break;

        default:
            next_page = -1;
    }
    return next_page;
}


void
csv_import_trans_assistant_prepare (GtkAssistant *assistant, GtkWidget *page,
                                    gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;

    // Reset callcount on every prepare
    info->callcount = 0;

    if (page == info->start_page)
        csv_import_trans_assistant_start_page_prepare (assistant, user_data);
    else if (page == info->file_page)
        csv_import_trans_assistant_file_page_prepare (assistant, user_data);
    else if (page == info->preview_page)
        csv_import_trans_assistant_preview_page_prepare (assistant, user_data);
    else if (page == info->account_match_page)
        csv_import_trans_assistant_account_match_page_prepare (assistant, user_data);
    else if (page == info->doc_page)
        csv_import_trans_assistant_doc_page_prepare (assistant, user_data);
    else if (page == info->match_page)
        csv_import_trans_assistant_match_page_prepare (assistant, user_data);
    else if (page == info->summary_page)
        csv_import_trans_assistant_summary_page_prepare (assistant, user_data);
    else
        g_assert_not_reached();
}


/*******************************************************
 * Assistant call back functions
 *******************************************************/
static void
csv_import_trans_assistant_destroy_cb (GtkWidget *object, gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;
    gnc_unregister_gui_component_by_data (ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS, info);
    g_free (info);
}

void
csv_import_trans_assistant_cancel (GtkAssistant *assistant, gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;

    if (!(info->gnc_csv_importer_gui == NULL))
        gnc_gen_trans_list_delete (info->gnc_csv_importer_gui);

    gnc_close_gui_component_by_data (ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS, info);
}

void
csv_import_trans_assistant_close (GtkAssistant *assistant, gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;
    gnc_close_gui_component_by_data (ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS, info);
}

void
csv_import_trans_assistant_finish (GtkAssistant *assistant, gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;

    /* Start the import */
    if (!info->parse_data->transactions.empty())
        gnc_gen_trans_assist_start (info->gnc_csv_importer_gui);
    else
        gnc_gen_trans_list_delete (info->gnc_csv_importer_gui);
}

static void
csv_import_trans_close_handler (gpointer user_data)
{
    CsvImportTrans *info = (CsvImportTrans*) user_data;

    /* Free the memory we allocated. */
    if (!(info->parse_data == NULL))
        delete info->parse_data;

    if (!(info->gnc_csv_importer_gui == NULL))
        info->gnc_csv_importer_gui = NULL;

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->window));
    gtk_widget_destroy (info->window);
}

/*******************************************************
 * Create the Assistant
 *******************************************************/
static GtkWidget *
csv_import_trans_assistant_create (CsvImportTrans *info)
{
    GtkBuilder *builder;
    GtkWidget *window;
    GtkWidget *box;
    GtkWidget *button, *h_box;

    builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "start_row_adj");
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "end_row_adj");
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "account_match_store");
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "CSV Transaction Assistant");
    window = GTK_WIDGET(gtk_builder_get_object (builder, "CSV Transaction Assistant"));
    info->window = window;

    /* Set the forward function */
    gtk_assistant_set_forward_page_func (GTK_ASSISTANT(window), csv_import_trans_forward_page_func, info, NULL);

    /* Enable buttons on all page. */
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object (builder, "start_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object (builder, "file_page")),
                                     FALSE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object (builder, "preview_page")),
                                     FALSE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object (builder, "account_match_page")),
                                     FALSE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object (builder, "doc_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object (builder, "match_page")),
                                     FALSE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object (builder, "summary_page")),
                                     TRUE);

    /* Start Page */
    info->start_page = GTK_WIDGET(gtk_builder_get_object (builder, "start_page"));

    /* File chooser Page */
    info->file_page = GTK_WIDGET(gtk_builder_get_object (builder, "file_page"));
    info->file_chooser = gtk_file_chooser_widget_new (GTK_FILE_CHOOSER_ACTION_OPEN);
    g_signal_connect (G_OBJECT(info->file_chooser), "file-activated",
                      G_CALLBACK(csv_import_trans_file_chooser_confirm_cb), info);
    button = gtk_button_new_from_stock (GTK_STOCK_OK);
    gtk_widget_set_size_request (button, 100, -1);
    gtk_widget_show (button);
    h_box = gtk_hbox_new (TRUE, 0);
    gtk_box_pack_start (GTK_BOX(h_box), button, FALSE, FALSE, 0);
    gtk_file_chooser_set_extra_widget (GTK_FILE_CHOOSER(info->file_chooser), h_box);
    g_signal_connect (G_OBJECT(button), "clicked",
                      G_CALLBACK(csv_import_trans_file_chooser_confirm_cb), info);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "file_page"));
    gtk_box_pack_start (GTK_BOX(box), info->file_chooser, TRUE, TRUE, 6);
    gtk_widget_show (info->file_chooser);

    /* Preview Settings Page */
    {
        info->preview_page = GTK_WIDGET(gtk_builder_get_object (builder, "preview_page"));

        // Add Settings combo
        auto settings_store = gtk_list_store_new (2, G_TYPE_POINTER, G_TYPE_STRING);
        info->settings_combo = gtk_combo_box_new_with_model_and_entry (GTK_TREE_MODEL(settings_store));
        gtk_combo_box_set_entry_text_column (GTK_COMBO_BOX(info->settings_combo), SET_NAME);
        gtk_combo_box_set_active (GTK_COMBO_BOX(info->settings_combo), 0);

        info->combo_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "combo_hbox"));
        gtk_box_pack_start (GTK_BOX(info->combo_hbox), info->settings_combo, true, true, 6);
        gtk_widget_show (info->settings_combo);

        g_signal_connect (G_OBJECT(info->settings_combo), "changed",
                         G_CALLBACK(csv_import_trans_changed_settings_cb), (gpointer)info);

        // Additionally connect to the changed signal of the embedded GtkEntry
        auto emb_entry = gtk_bin_get_child (GTK_BIN (info->settings_combo));
        g_signal_connect (G_OBJECT(emb_entry), "changed",
                         G_CALLBACK(csv_import_trans_changed_settings_text_cb), (gpointer)info);

        // Add Save Settings button
        info->save_button = GTK_WIDGET(gtk_builder_get_object (builder, "save_settings"));

        // Add Delete Settings button
        info->del_button = GTK_WIDGET(gtk_builder_get_object (builder, "delete_settings"));


        g_signal_connect (G_OBJECT(info->del_button), "clicked",
                         G_CALLBACK(csv_import_trans_delete_settings_cb), (gpointer)info);

        /* The table containing the separator configuration widgets */
        info->start_row_spin = GTK_WIDGET(gtk_builder_get_object (builder, "start_row"));
        info->end_row_spin = GTK_WIDGET(gtk_builder_get_object (builder, "end_row"));
        info->skip_rows = GTK_WIDGET(gtk_builder_get_object (builder, "skip_rows"));
        info->check_butt = GTK_WIDGET(gtk_builder_get_object (builder, "check_butt"));
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
        {
            info->sep_buttons[i]
                = (GtkCheckButton*)GTK_WIDGET(gtk_builder_get_object (builder, sep_button_names[i]));
            /* Connect them to the sep_button_clicked event handler. */
            g_signal_connect (G_OBJECT(info->sep_buttons[i]), "toggled",
                             G_CALLBACK(sep_button_clicked), (gpointer)info);
        }

        /* Load and connect the custom separator checkbutton in the same way
         * as the other separator buttons. */
        info->custom_cbutton
            = (GtkCheckButton*)GTK_WIDGET(gtk_builder_get_object (builder, "custom_cbutton"));
        g_signal_connect (G_OBJECT(info->custom_cbutton), "clicked",
                         G_CALLBACK(sep_button_clicked), (gpointer)info);

        /* Load the entry for the custom separator entry. Connect it to the
         * sep_button_clicked event handler as well. */
        info->custom_entry = (GtkEntry*)GTK_WIDGET(gtk_builder_get_object (builder, "custom_entry"));
        g_signal_connect (G_OBJECT(info->custom_entry), "changed",
                         G_CALLBACK(sep_button_clicked), (gpointer)info);

        /* Add account selection widget */
        info->acct_selector = gnc_account_sel_new();
        auto account_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "account_hbox"));
        gtk_box_pack_start (GTK_BOX(account_hbox), info->acct_selector, TRUE, TRUE, 6);
        gtk_widget_show (info->acct_selector);


        g_signal_connect(G_OBJECT(info->acct_selector), "account_sel_changed",
                         G_CALLBACK(account_selected_cb), (gpointer)info);


        /* Create the encoding selector widget and add it to the assistant */
        info->encselector = GO_CHARMAP_SEL(go_charmap_sel_new(GO_CHARMAP_SEL_TO_UTF8));
        /* Connect the selector to the encoding_selected event handler. */
        g_signal_connect (G_OBJECT(info->encselector), "charmap_changed",
                         G_CALLBACK(encoding_selected), (gpointer)info);

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
                         G_CALLBACK(date_format_selected), (gpointer)info);

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
                         G_CALLBACK(currency_format_selected), (gpointer)info);

        /* Add it to the assistant. */
        auto currency_format_container = GTK_CONTAINER(gtk_builder_get_object (builder, "currency_format_container"));
        gtk_container_add (currency_format_container, GTK_WIDGET(info->currency_format_combo));
        gtk_widget_show_all (GTK_WIDGET(currency_format_container));

        /* Connect the CSV/Fixed-Width radio button event handler. */
        info->csv_button = GTK_WIDGET(gtk_builder_get_object (builder, "csv_button"));
        info->fixed_button = GTK_WIDGET(gtk_builder_get_object (builder, "fixed_button"));
        g_signal_connect (info->csv_button, "toggled",
                         G_CALLBACK(separated_or_fixed_selected), (gpointer)info);

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
    // This is for double click mouse and buttons...
    g_signal_connect (G_OBJECT(info->account_match_view), "button_press_event", G_CALLBACK(import_account_button_cb), info);
    g_signal_connect (G_OBJECT(info->account_match_btn), "clicked", G_CALLBACK(import_account_select_cb), info);

    /* Doc Page */
    info->doc_page = GTK_WIDGET(gtk_builder_get_object (builder, "doc_page"));

    /* Matcher page */
    info->match_page  = GTK_WIDGET(gtk_builder_get_object (builder, "match_page"));
    info->match_label = GTK_WIDGET(gtk_builder_get_object (builder, "match_label"));

    /* Summary Page */
    info->summary_page  = GTK_WIDGET(gtk_builder_get_object (builder, "summary_page"));
    info->summary_label = GTK_WIDGET(gtk_builder_get_object (builder, "summary_label"));

    g_signal_connect (G_OBJECT(window), "destroy",
                      G_CALLBACK (csv_import_trans_assistant_destroy_cb), info);

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(info->window));

    gtk_builder_connect_signals (builder, info);
    g_object_unref (G_OBJECT(builder));
    return window;
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

    csv_import_trans_assistant_create (info);

    gnc_register_gui_component (ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS,
                                NULL, csv_import_trans_close_handler,
                                info);

    gtk_widget_show_all (info->window);

    gnc_window_adjust_for_screen (GTK_WINDOW(info->window));
}
