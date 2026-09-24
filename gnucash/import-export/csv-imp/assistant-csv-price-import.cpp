/*******************************************************************\
 * assistant-csv-price-import.cpp -- An assistant for importing     *
 *                                     Prices from a file.          *
 *                                                                  *
 * Copyright (C) 2017 Robert Fewell                                 *
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
/** @file assistant-csv-price-import.cpp
    @brief CSV Import Assistant
    @author Copyright (c) 2016 Geert Janssens
    @author Copyright (c) 2017 Robert Fewell
*/

#include <guid.hpp>

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <stdlib.h>
#include <cstdint>

#include "gnc-ui.h"
#include "gnc-uri.hpp"
#include "gnc-ui-util.h"
#include "gnc-file.h"
#include "dialog-utils.h"
#include "gnc-gtk-utils.h"

#include "gnc-component-manager.h"

#include "gnc-state.h"

#include "assistant-csv-price-import.h"
#include "gnc-csv-preview-refresh.hpp"
#include "gnc-import-assistant.h"

#include "go-charmap-sel.h"

#include <algorithm>
#include <exception>
#include <iostream>
#include <memory>
#include <string>
#include <tuple>

#include "gnc-imp-settings-csv-price.hpp"
#include "gnc-import-price.hpp"
#include "gnc-tokenizer-fw.hpp"
#include "gnc-tokenizer-csv.hpp"

#define MIN_COL_WIDTH 70
#define GNC_PREFS_GROUP "dialogs.import.csv"
#define ASSISTANT_CSV_IMPORT_PRICE_CM_CLASS "assistant-csv-price-import"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

/* Note on memory management
 *
 * The same notes as for assistant-csv-trans-import.cpp apply to
 * this assistant as well. Please read the note at the top of that
 * file to understand important details about the use of several
 * memory management models in one file.
 */

class  CsvImpPriceAssist
{
public:
    CsvImpPriceAssist ();
    ~CsvImpPriceAssist ();

    /* Delete copy and move constructor/assignments
     * We don't want gui elements to be moved around or copied at all */
    CsvImpPriceAssist(const CsvImpPriceAssist&) = delete;            // copy constructor
    CsvImpPriceAssist& operator=(const CsvImpPriceAssist&) = delete; // copy assignment
    CsvImpPriceAssist(CsvImpPriceAssist&&) = delete;                 // move constructor
    CsvImpPriceAssist& operator=(CsvImpPriceAssist&&) = delete;      // move assignment

    void assist_prepare_cb (GtkWidget *page);
    void assist_file_page_prepare ();
    void assist_preview_page_prepare ();
    void assist_confirm_page_prepare ();
    void assist_summary_page_prepare ();
    void assist_finish ();
    void assist_compmgr_close ();

    void select_file_cb ();

    void preview_settings_delete ();
    void preview_settings_save ();
    void preview_settings_name (GtkEntry* entry);
    void preview_settings_load ();
    void preview_update_skipped_rows ();
    void preview_over_write (bool over);
    void preview_update_separators (GtkWidget* widget);
    void preview_update_file_format ();
    void preview_update_encoding (const char* encoding);
    void preview_update_date_format ();
    void preview_update_currency_format ();
    void preview_update_currency ();
    void preview_update_commodity ();
    void preview_reparse_col_type (GncPricePropType type);
    void preview_update_col_type (GtkDropDown* dropdown);

    void preview_populate_settings_combo();
    void preview_handle_save_del_sensitivity ();
    void preview_refresh_table ();
    void preview_refresh ();
    void preview_validate_settings ();

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

    GtkWidget       *combo_hbox;                    /**< The Settings Combo hbox */
    GtkSpinButton   *start_row_spin;                /**< The widget for the start row spinner */
    GtkSpinButton   *end_row_spin;                  /**< The widget for the end row spinner */
    GtkWidget       *skip_alt_rows_button;          /**< The widget for Skip alternate rows from start row */
    GtkWidget       *skip_errors_button;            /**< The widget for Skip error rows*/
    GtkWidget       *csv_button;                    /**< The widget for the CSV button */
    GtkWidget       *fixed_button;                  /**< The widget for the Fixed Width button */
    GtkWidget       *over_write_cbutton;            /**< The widget for Price Overwrite */
    GtkDropDown     *commodity_selector;            /**< The commodity selector */
    GtkDropDown     *currency_selector;             /**< The currency selector */
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

    GtkWidget       *confirm_page;                  /**< Assistant confirm page widget */

    GtkWidget       *summary_page;                  /**< Assistant summary page widget */
    GtkWidget       *summary_label;                 /**< The summary text */

    std::unique_ptr<GncPriceImport> price_imp;      /**< The actual data we are previewing */
};


/*******************************************************
 * Assistant call back functions
 *******************************************************/

extern "C"
{
void csv_price_imp_assist_prepare_cb (GncImportAssistant  *assistant, GtkWidget *page, gpointer user_data);
void csv_price_imp_assist_close_cb (GncImportAssistant *gtkassistant, gpointer user_data);
void csv_price_imp_assist_finish_cb (GncImportAssistant *gtkassistant, gpointer user_data);
void csv_price_imp_select_file_cb (GtkButton *button, CsvImpPriceAssist *info);
void csv_price_imp_preview_del_settings_cb (GtkWidget *button, CsvImpPriceAssist *info);
void csv_price_imp_preview_save_settings_cb (GtkWidget *button, CsvImpPriceAssist *info);
void csv_price_imp_preview_settings_sel_changed_cb (GtkDropDown *dropdown, GParamSpec *pspec,
                                                    CsvImpPriceAssist *info);
void csv_price_imp_preview_settings_text_inserted_cb (GtkEditable *entry, gchar *new_text,
        gint new_text_length, gint *position, CsvImpPriceAssist *info);
void csv_price_imp_preview_settings_text_changed_cb (GtkEntry *entry, CsvImpPriceAssist *info);
void csv_price_imp_preview_srow_cb (GtkSpinButton *spin, CsvImpPriceAssist *info);
void csv_price_imp_preview_erow_cb (GtkSpinButton *spin, CsvImpPriceAssist *info);
void csv_price_imp_preview_skiprows_cb (GtkCheckButton *checkbox, CsvImpPriceAssist *info);
void csv_price_imp_preview_skiperrors_cb (GtkCheckButton *checkbox, CsvImpPriceAssist *info);
void csv_price_imp_preview_overwrite_cb (GtkCheckButton *checkbox, CsvImpPriceAssist *info);
void csv_price_imp_preview_sep_button_cb (GtkWidget* widget, CsvImpPriceAssist* info);
void csv_price_imp_preview_sep_fixed_sel_cb (GtkCheckButton* csv_button, CsvImpPriceAssist* info);
void csv_price_imp_preview_acct_sel_cb (GtkWidget* widget, CsvImpPriceAssist* info);
void csv_price_imp_preview_enc_sel_cb (GOCharmapSel* selector, const char* encoding,
                              CsvImpPriceAssist* info);
}

void
csv_price_imp_assist_prepare_cb (GncImportAssistant *assistant, GtkWidget *page,
        gpointer user_data)
{
    auto info = static_cast<CsvImpPriceAssist *> (user_data);
    info->assist_prepare_cb(page);
}

void
csv_price_imp_assist_close_cb (GncImportAssistant *assistant, gpointer user_data)
{
    auto info = static_cast<CsvImpPriceAssist *> (user_data);
    gnc_close_gui_component_by_data (ASSISTANT_CSV_IMPORT_PRICE_CM_CLASS, info);
}

void
csv_price_imp_assist_finish_cb (GncImportAssistant *assistant, gpointer user_data)
{
    auto info = static_cast<CsvImpPriceAssist *> (user_data);
    info->assist_finish ();
}

void csv_price_imp_select_file_cb (GtkButton *button, CsvImpPriceAssist *info)
{
    info->select_file_cb ();
    (void)button;
}

void csv_price_imp_preview_del_settings_cb (GtkWidget *button, CsvImpPriceAssist *info)
{
    info->preview_settings_delete();
}

void csv_price_imp_preview_save_settings_cb (GtkWidget *button, CsvImpPriceAssist *info)
{
    info->preview_settings_save();
}

void csv_price_imp_preview_settings_sel_changed_cb (GtkDropDown *dropdown, GParamSpec *pspec,
                                                    CsvImpPriceAssist *info)
{
    info->preview_settings_load();
}

void
csv_price_imp_preview_settings_text_inserted_cb (GtkEditable *entry, gchar *new_text,
        gint new_text_length, gint *position, CsvImpPriceAssist *info)
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
    g_signal_handlers_block_by_func (entry, (gpointer) csv_price_imp_preview_settings_text_inserted_cb, info);
    gtk_editable_insert_text (entry, mod_txt.c_str(), mod_txt.size() , position);
    g_signal_handlers_unblock_by_func (entry, (gpointer) csv_price_imp_preview_settings_text_inserted_cb, info);

    g_signal_stop_emission_by_name (entry, "insert_text");
}

void
csv_price_imp_preview_settings_text_changed_cb (GtkEntry *entry, CsvImpPriceAssist *info)
{
    info->preview_settings_name(entry);
}

void csv_price_imp_preview_srow_cb (GtkSpinButton *spin, CsvImpPriceAssist *info)
{
    info->preview_update_skipped_rows();
}

void csv_price_imp_preview_erow_cb (GtkSpinButton *spin, CsvImpPriceAssist *info)
{
    info->preview_update_skipped_rows();
}

void csv_price_imp_preview_skiprows_cb (GtkCheckButton *checkbox, CsvImpPriceAssist *info)
{
    info->preview_update_skipped_rows();
}

void csv_price_imp_preview_skiperrors_cb (GtkCheckButton *checkbox, CsvImpPriceAssist *info)
{
    info->preview_update_skipped_rows();
}

void csv_price_imp_preview_overwrite_cb (GtkCheckButton *checkbox, CsvImpPriceAssist *info)
{
    info->preview_over_write (gtk_check_button_get_active (checkbox));
}

void csv_price_imp_preview_sep_button_cb (GtkWidget* widget, CsvImpPriceAssist* info)
{
    info->preview_update_separators(widget);
}

void csv_price_imp_preview_sep_fixed_sel_cb (GtkCheckButton* csv_button, CsvImpPriceAssist* info)
{
    info->preview_update_file_format();
}

void csv_price_imp_preview_enc_sel_cb (GOCharmapSel* selector, const char* encoding,
                              CsvImpPriceAssist* info)
{
    info->preview_update_encoding(encoding);
}

static void csv_price_imp_preview_date_fmt_sel_cb (GtkDropDown* format_selector, GParamSpec* pspec,
                                                   CsvImpPriceAssist* info)
{
    info->preview_update_date_format();
}

static void csv_price_imp_preview_currency_fmt_sel_cb (GtkDropDown* format_selector, GParamSpec* pspec,
                                                       CsvImpPriceAssist* info)
{
    info->preview_update_currency_format();
}

static void csv_price_imp_preview_currency_sel_cb (GtkDropDown* currency_selector, GParamSpec* pspec,
                                                   CsvImpPriceAssist* info)
{
    info->preview_update_currency();
}

static void csv_price_imp_preview_commodity_sel_cb (GtkDropDown* commodity_selector, GParamSpec* pspec,
                                                    CsvImpPriceAssist* info)
{
    info->preview_update_commodity();
}

static void csv_price_imp_preview_col_type_changed_cb (GtkDropDown* dropdown, GParamSpec* pspec,
                                                       CsvImpPriceAssist* info)
{
    info->preview_update_col_type (dropdown);
}

static constexpr auto COMMODITY_ROW_DATA = "csv-price-commodity";
static constexpr auto PRESET_ROW_DATA = "csv-price-preset";
static constexpr auto COLUMN_TYPE_DATA = "csv-price-column-type";
static constexpr auto PREVIEW_ROW_DATA = "csv-price-preview-row";

static gnc_commodity*
get_commodity_from_dropdown (GtkDropDown *dropdown)
{
    auto position = gtk_drop_down_get_selected (dropdown);
    if (position == GTK_INVALID_LIST_POSITION)
        return nullptr;

    auto item = g_list_model_get_item (gtk_drop_down_get_model (dropdown), position);
    auto commodity = static_cast<gnc_commodity*> (g_object_get_data (G_OBJECT (item), COMMODITY_ROW_DATA));
    g_object_unref (item);
    return commodity;
}

static void
set_commodity_for_dropdown (GtkDropDown *dropdown, gnc_commodity *commodity)
{
    auto model = gtk_drop_down_get_model (dropdown);
    for (guint position = 0; position < g_list_model_get_n_items (model); position++)
    {
        auto item = g_list_model_get_item (model, position);
        auto row_commodity = static_cast<gnc_commodity*> (g_object_get_data (G_OBJECT (item), COMMODITY_ROW_DATA));
        g_object_unref (item);
        if (row_commodity == commodity)
        {
            gtk_drop_down_set_selected (dropdown, position);
            return;
        }
    }
    gtk_drop_down_set_selected (dropdown, 0);
}

static GListStore*
get_commodity_model (bool all_commodity)
{
    struct CommodityRow { std::string sort; std::string label; gnc_commodity *commodity; };
    std::vector<CommodityRow> rows;
    const gnc_commodity_table *commodity_table = gnc_get_current_commodities ();
    GList *namespace_list = gnc_commodity_table_get_namespaces (commodity_table);
    rows.emplace_back (CommodityRow{" ", " ", nullptr});

    for (auto node = namespace_list; node; node = g_list_next (node))
    {
        auto tmp_namespace = static_cast<char*> (node->data);
        DEBUG("Looking at namespace %s", tmp_namespace);

        /* Hide the template entry */
        if (g_utf8_collate (tmp_namespace, "template" ) != 0)
        {
            if ((g_utf8_collate (tmp_namespace, GNC_COMMODITY_NS_CURRENCY ) == 0) || (all_commodity == true))
            {
                auto comm_list = gnc_commodity_table_get_commodities (commodity_table, tmp_namespace);

                // if this is the CURRENCY, add a row to be identified as a separator row
                if ((g_utf8_collate (tmp_namespace, GNC_COMMODITY_NS_CURRENCY) == 0) && (all_commodity == true))
                {
                    /* GtkDropDown deliberately has no selectable separator rows. */
                }

                for (auto node = comm_list; node; node = g_list_next (node))
                {
                    const gchar *name_str;
                    auto tmp_commodity = static_cast<gnc_commodity*> (node->data);
                    DEBUG("Looking at commodity %s", gnc_commodity_get_fullname (tmp_commodity));

                    name_str = gnc_commodity_get_printname (tmp_commodity);

                    auto sort_str = std::string (g_utf8_collate (tmp_namespace, GNC_COMMODITY_NS_CURRENCY) == 0
                                                   ? "CURRENCY-" : "ALL-OTHER-") + name_str;
                    DEBUG("Name string is '%s', Sort string is '%s'", name_str, sort_str.c_str());
                    rows.emplace_back (CommodityRow{std::move (sort_str), name_str, tmp_commodity});
                }
                g_list_free (comm_list);
            }
        }
    }
    g_list_free (namespace_list);

    std::sort (rows.begin(), rows.end(), [] (const auto& first, const auto& second)
               { return g_utf8_collate (first.sort.c_str(), second.sort.c_str()) < 0; });
    auto store = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    for (const auto& row_data : rows)
    {
        auto row = gtk_string_object_new (row_data.label.c_str());
        g_object_set_data (G_OBJECT (row), COMMODITY_ROW_DATA, row_data.commodity);
        g_list_store_append (store, row);
        g_object_unref (row);
    }
    return store;
}

static CsvPriceImpSettings*
get_selected_preset (GtkDropDown *dropdown)
{
    auto position = gtk_drop_down_get_selected (dropdown);
    if (position == GTK_INVALID_LIST_POSITION)
        return nullptr;

    auto item = g_list_model_get_item (gtk_drop_down_get_model (dropdown), position);
    auto preset = static_cast<CsvPriceImpSettings*> (g_object_get_data (G_OBJECT (item), PRESET_ROW_DATA));
    g_object_unref (item);
    return preset;
}

static guint
find_preset_position (GtkDropDown *dropdown, const std::string& name)
{
    auto model = gtk_drop_down_get_model (dropdown);
    for (guint position = 0; position < g_list_model_get_n_items (model); position++)
    {
        auto item = g_list_model_get_item (model, position);
        auto preset = static_cast<CsvPriceImpSettings*> (g_object_get_data (G_OBJECT (item), PRESET_ROW_DATA));
        g_object_unref (item);
        if (preset && preset->m_name == name)
            return position;
    }
    return GTK_INVALID_LIST_POSITION;
}

void
CsvImpPriceAssist::settings_confirmation_data_free (SettingsConfirmationData *data)
{
    g_weak_ref_clear (&data->assistant);
    delete data;
}

void
CsvImpPriceAssist::settings_confirmation_cb (GtkWindow *parent, gint response,
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

    auto owner = static_cast<CsvImpPriceAssist *> (g_object_get_data (
        G_OBJECT (assistant), "gnc-csv-price-import-assistant-owner"));
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


/*******************************************************
 * Assistant Constructor
 *******************************************************/
CsvImpPriceAssist::CsvImpPriceAssist () :
    preview_refresh_idle {preview_refresh_table_idle_cb, this}
{
    auto builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder , "assistant-csv-price-import.glade", "start_row_adj");
    gnc_builder_add_from_file  (builder , "assistant-csv-price-import.glade", "end_row_adj");
    gnc_builder_add_from_file  (builder , "assistant-csv-price-import.glade", "CSV Price Assistant");
    csv_imp_asst = gnc_import_assistant_new (
        GTK_WINDOW (gtk_builder_get_object (builder, "CSV Price Assistant")),
        GTK_STACK (gtk_builder_get_object (builder, "gnc_import_assistant_stack")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_page_title")),
        GTK_BOX (gtk_builder_get_object (builder, "gnc_import_assistant_actions")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_back")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_next")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_apply")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_cancel")),
        GTK_WIDGET (gtk_builder_get_object (builder, "gnc_import_assistant_close")));
    if (!csv_imp_asst)
        throw std::runtime_error ("Unable to construct CSV price import assistant");
    g_object_set_data (G_OBJECT (csv_imp_asst), "gnc-csv-price-import-assistant-owner", this);

    // Set the name for this assistant so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(csv_imp_asst), "gnc-id-assistant-csv-price-import");
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
                                     GTK_WIDGET(gtk_builder_get_object (builder, "confirm_page")),
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
                      G_CALLBACK (csv_price_imp_select_file_cb), this);

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
                         G_CALLBACK(csv_price_imp_preview_settings_sel_changed_cb), this);
        g_signal_connect (settings_entry, "changed",
                         G_CALLBACK(csv_price_imp_preview_settings_text_changed_cb), this);
        g_signal_connect (settings_entry, "insert-text",
                         G_CALLBACK(csv_price_imp_preview_settings_text_inserted_cb), this);

        // Add Save Settings button
        save_button = GTK_WIDGET(gtk_builder_get_object (builder, "save_settings"));

        // Add Delete Settings button
        del_button = GTK_WIDGET(gtk_builder_get_object (builder, "delete_settings"));

        /* The table containing the separator configuration widgets */
        start_row_spin = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "start_row"));
        end_row_spin = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "end_row"));
        skip_alt_rows_button = GTK_WIDGET(gtk_builder_get_object (builder, "skip_rows"));
        skip_errors_button = GTK_WIDGET(gtk_builder_get_object (builder, "skip_errors_button"));
        over_write_cbutton = GTK_WIDGET(gtk_builder_get_object (builder, "over_write_button"));
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

        /* Create the encoding selector widget and add it to the assistant */
        encselector = GO_CHARMAP_SEL(go_charmap_sel_new(GO_CHARMAP_SEL_TO_UTF8));
        /* Connect the selector to the encoding_selected event handler. */
        g_signal_connect (G_OBJECT(encselector), "charmap_changed",
                         G_CALLBACK(csv_price_imp_preview_enc_sel_cb), this);

        auto encoding_box = GTK_BOX(gtk_builder_get_object (builder, "encoding_container"));
        gtk_box_prepend (GTK_BOX(encoding_box), GTK_WIDGET(encselector));
        gtk_widget_set_hexpand (GTK_WIDGET(encselector), true);

        /* Add commodity selection widget */
        auto commodity_expression = gtk_property_expression_new (GTK_TYPE_STRING_OBJECT, nullptr, "string");
        commodity_selector = gnc_gtk_drop_down_new (G_LIST_MODEL (get_commodity_model (true)),
                                                     commodity_expression);
        auto commodity_box = GTK_BOX (gtk_builder_get_object (builder, "commodity_hbox"));
        gtk_box_append (commodity_box, GTK_WIDGET (commodity_selector));
        g_signal_connect (commodity_selector, "notify::selected",
                          G_CALLBACK(csv_price_imp_preview_commodity_sel_cb), this);

        /* Add currency selection widget */
        auto currency_expression = gtk_property_expression_new (GTK_TYPE_STRING_OBJECT, nullptr, "string");
        currency_selector = gnc_gtk_drop_down_new (G_LIST_MODEL (get_commodity_model (false)),
                                                    currency_expression);
        auto currency_box = GTK_BOX (gtk_builder_get_object (builder, "currency_hbox"));
        gtk_box_append (currency_box, GTK_WIDGET (currency_selector));
        g_signal_connect (currency_selector, "notify::selected",
                         G_CALLBACK(csv_price_imp_preview_currency_sel_cb), this);

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
                         G_CALLBACK(csv_price_imp_preview_date_fmt_sel_cb), this);

        /* Add it to the assistant. */
        auto date_format_box = GTK_BOX(gtk_builder_get_object (builder, "date_format_container"));
        gtk_box_prepend (GTK_BOX(date_format_box), GTK_WIDGET(date_format_dropdown));
        gtk_widget_set_hexpand (GTK_WIDGET(date_format_dropdown), true);

        /* Add in the currency format combo box and hook it up to an event handler. */
        std::vector<const char*> currency_formats;
        for (int i = 0; i < num_currency_formats_price; i++)
            currency_formats.emplace_back (_(currency_format_user_price[i]));
        currency_formats.emplace_back (nullptr);
        currency_format_dropdown = gnc_gtk_drop_down_new_from_strings (currency_formats.data());
        /* Default will the locale */
        gtk_drop_down_set_selected (currency_format_dropdown, 0);
        g_signal_connect (currency_format_dropdown, "notify::selected",
                         G_CALLBACK(csv_price_imp_preview_currency_fmt_sel_cb), this);

        /* Add it to the assistant. */
        auto currency_format_box = GTK_BOX(gtk_builder_get_object (builder, "currency_format_container"));
        gtk_box_prepend (GTK_BOX(currency_format_box), GTK_WIDGET(currency_format_dropdown));
        gtk_widget_set_hexpand (GTK_WIDGET(currency_format_dropdown), true);

        /* Connect the CSV/Fixed-Width radio button event handler. */
        csv_button = GTK_WIDGET(gtk_builder_get_object (builder, "csv_button"));
        fixed_button = GTK_WIDGET(gtk_builder_get_object (builder, "fixed_button"));

        /* The preview keeps its data and column configuration in GTK4 models. */
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

    /* Confirm Page */
    confirm_page = GTK_WIDGET(gtk_builder_get_object (builder, "confirm_page"));

    /* Summary Page */
    summary_page  = GTK_WIDGET(gtk_builder_get_object (builder, "summary_page"));
    summary_label = GTK_WIDGET(gtk_builder_get_object (builder, "summary_label"));

    gnc_restore_window_size (GNC_PREFS_GROUP,
                             GTK_WINDOW(csv_imp_asst), gnc_ui_get_main_window(nullptr));

gnc_builder_connect_signals (builder, this);
    gnc_import_assistant_set_page_action (csv_imp_asst, 3,
                                          GNC_IMPORT_ASSISTANT_PAGE_APPLY);
    gnc_import_assistant_set_page_action (csv_imp_asst, 4,
                                          GNC_IMPORT_ASSISTANT_PAGE_CLOSE);
    gnc_import_assistant_set_callbacks (csv_imp_asst, csv_price_imp_assist_prepare_cb,
                                        csv_price_imp_assist_finish_cb,
                                        csv_price_imp_assist_close_cb,
                                        csv_price_imp_assist_close_cb, this);
    g_object_unref (G_OBJECT(builder));

    gnc_window_adjust_for_screen (GTK_WINDOW(csv_imp_asst));
    gtk_window_present (GTK_WINDOW(csv_imp_asst));
}

/*******************************************************
 * Assistant Destructor
 *******************************************************/
CsvImpPriceAssist::~CsvImpPriceAssist ()
{
    preview_refresh_idle.cancel ();
    g_object_set_data (G_OBJECT (csv_imp_asst),
                       "gnc-csv-price-import-assistant-owner", nullptr);
    gtk_window_destroy (GTK_WINDOW(csv_imp_asst));
}

/**************************************************
 * Code related to the file selection page
 **************************************************/

bool
CsvImpPriceAssist::set_selected_file (GFile *file)
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
CsvImpPriceAssist::file_dialog_finished_cb (GObject *source, GAsyncResult *result,
                                            gpointer user_data)
{
    auto data = static_cast<FileDialogData *> (user_data);
    auto request = GNC_FILE_DIALOG_REQUEST (source);
    GError *error = nullptr;
    auto file = gnc_file_dialog_request_finish (request, result, &error);
    auto assistant = GTK_WIDGET (g_weak_ref_get (&data->assistant));
    auto info = assistant ? static_cast<CsvImpPriceAssist *> (
        g_object_get_data (G_OBJECT (assistant),
                           "gnc-csv-price-import-assistant-owner")) : nullptr;

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
CsvImpPriceAssist::select_file_cb ()
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

/* Set the available presets in the settings combo box
 */
void CsvImpPriceAssist::preview_populate_settings_combo()
{
    auto store = G_LIST_STORE (gtk_drop_down_get_model (settings_dropdown));
    g_list_store_remove_all (store);

    auto presets = get_import_presets_price ();
    for (const auto& preset : presets)
    {
        auto row = gtk_string_object_new (_(preset->m_name.c_str()));
        /* Presets are owned by the long-lived importer settings registry. */
        g_object_set_data (G_OBJECT (row), PRESET_ROW_DATA, preset.get());
        g_list_store_append (store, row);
        g_object_unref (row);
    }
}

/* Enable or disable the save and delete settings buttons
 * depending on what is selected and entered as settings name
 */
void CsvImpPriceAssist::preview_handle_save_del_sensitivity ()
{
    auto can_delete = false;
    auto can_save = false;
    auto entry_text = gnc_entry_get_text (settings_entry);
    auto preset = get_selected_preset (settings_dropdown);
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
CsvImpPriceAssist::preview_settings_name (GtkEntry* entry)
{
    auto text = gnc_entry_get_text (entry);
    if (text)
        price_imp->settings_name(text);

    preview_handle_save_del_sensitivity ();
}

/* Use selected preset to configure the import. Triggered when
 * a preset is selected in the settings combo.
 */
void
CsvImpPriceAssist::preview_settings_load ()
{
    auto preset = get_selected_preset (settings_dropdown);

    if (!preset)
        return;

    gtk_editable_set_text (GTK_EDITABLE (settings_entry), preset->m_name.c_str());
    price_imp->settings (*preset);
    if (preset->m_load_error)
        gnc_error_dialog (GTK_WINDOW(csv_imp_asst),
            "%s", _("There were problems reading some saved settings, continuing to load.\n"
                    "Please review and save again."));

    preview_refresh ();
    preview_handle_save_del_sensitivity ();
}

/* Callback to delete a settings entry
 */
void
CsvImpPriceAssist::preview_settings_delete ()
{
    auto preset = get_selected_preset (settings_dropdown);
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
CsvImpPriceAssist::complete_settings_delete (const std::string& name)
{
    if (preset_is_reserved_name (name))
        return;

    const auto& presets = get_import_presets_price ();
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
CsvImpPriceAssist::preview_settings_save ()
{
    auto new_name = price_imp->settings_name ();
    auto existing = find_preset_position (settings_dropdown, new_name);

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
CsvImpPriceAssist::complete_settings_save (const std::string& name)
{
    price_imp->settings_name (name);
    if (!price_imp->save_settings ())
    {
        gnc_info_dialog (GTK_WINDOW (csv_imp_asst),
                         "%s", _("The settings have been saved."));
        preview_populate_settings_combo ();
        auto position = find_preset_position (settings_dropdown, name);
        if (position != GTK_INVALID_LIST_POSITION)
            gtk_drop_down_set_selected (settings_dropdown, position);
    }
    else
        gnc_error_dialog (GTK_WINDOW (csv_imp_asst),
                          "%s", _("There was a problem saving the settings, please try again."));
}
/* Callback triggered when user adjusts skip start lines
 */
void CsvImpPriceAssist::preview_update_skipped_rows ()
{
    /* Update skip rows in the parser */
    price_imp->update_skipped_lines (gtk_spin_button_get_value_as_int (start_row_spin),
        gtk_spin_button_get_value_as_int (end_row_spin),
        gtk_check_button_get_active (GTK_CHECK_BUTTON(skip_alt_rows_button)),
        gtk_check_button_get_active (GTK_CHECK_BUTTON(skip_errors_button)));

    /* And adjust maximum number of lines that can be skipped at each end accordingly */
    auto adj = gtk_spin_button_get_adjustment (end_row_spin);
    gtk_adjustment_set_upper (adj, price_imp->m_parsed_lines.size()
            - price_imp->skip_start_lines() -1);

    adj = gtk_spin_button_get_adjustment (start_row_spin);
    gtk_adjustment_set_upper (adj, price_imp->m_parsed_lines.size()
            - price_imp->skip_end_lines() - 1);

    preview_refresh_table ();
}

/* Callback triggered when user clicks on Overwrite option
 */
void CsvImpPriceAssist::preview_over_write (bool over)
{
    price_imp->over_write (over);
}

/** Event handler for separator changes. This function is called
 * whenever one of the widgets for configuring the separators (the
 * separator checkbuttons, the escape checkbutton or the custom
 * separator entry) is changed.
 * @param widget The widget that was changed
 * @param info The data that is being configured
 */
void CsvImpPriceAssist::preview_update_separators (GtkWidget* widget)
{
    /* Only manipulate separator characters if the currently open file is
     * csv separated. */
    if (price_imp->file_format() != GncImpFileFormat::CSV)
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
    price_imp->separators (checked_separators);
    price_imp->enable_escape (gtk_check_button_get_active (escape_cbutton));

    /* if there are no separators, there will only be one column
     * so make sure column header is NONE */
    if (checked_separators.empty())
        price_imp->set_column_type_price (0, GncPricePropType::NONE);

    /* Parse the data using the new options. We don't want to reguess
     * the column types because we want to leave the user's
     * configurations intact. */
    try
    {
        price_imp->tokenize (false);
        preview_refresh_table ();
    }
    catch (std::range_error &e)
    {
        /* Warn the user there was a problem and try to undo what caused
         * the error. (This will cause a reparsing and ideally a usable
         * configuration.) */
        gnc_error_dialog (GTK_WINDOW(csv_imp_asst), "Error in parsing");
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
 * @param csv_button The "Separated" radio button
 * @param info The display of the data being imported
 */
void CsvImpPriceAssist::preview_update_file_format ()
{
    /* Set the parsing type correctly. */
    try
    {
        if (gtk_check_button_get_active (GTK_CHECK_BUTTON(csv_button)))
        {
            price_imp->file_format (GncImpFileFormat::CSV);
            gtk_widget_set_visible (separator_table, true);
            gtk_widget_set_visible (fw_instructions_hbox, false);
        }
        else
        {
            price_imp->file_format (GncImpFileFormat::FIXED_WIDTH);
            gtk_widget_set_visible (separator_table, false);
            gtk_widget_set_visible (fw_instructions_hbox, true);

        }
        price_imp->tokenize (false);
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

/** Event handler for a new encoding. This is called when the user
 * selects a new encoding; the data is reparsed and shown to the
 * user.
 * @param selector The widget the user uses to select a new encoding
 * @param encoding The encoding that the user selected
 */
void
CsvImpPriceAssist::preview_update_encoding (const char* encoding)
{
    /* This gets called twice every time a new encoding is selected. The
     * second call actually passes the correct data; thus, we only do
     * something the second time this is called. */

    /* If this is the second time the function is called ... */
    if (encoding_selected_called)
    {
        std::string previous_encoding = price_imp->m_tokenizer->encoding();
        /* Try converting the new encoding and reparsing. */
        try
        {
            price_imp->encoding (encoding);
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
CsvImpPriceAssist::preview_update_date_format ()
{
    price_imp->date_format (gtk_drop_down_get_selected (date_format_dropdown));
    preview_refresh_table ();
}

void
CsvImpPriceAssist::preview_update_currency_format ()
{
    price_imp->currency_format (gtk_drop_down_get_selected (currency_format_dropdown));
    preview_refresh_table ();
}

void
CsvImpPriceAssist::preview_update_currency ()
{
    gnc_commodity *comm = get_commodity_from_dropdown (currency_selector);
    price_imp->to_currency (comm);
    preview_refresh_table ();
}

void
CsvImpPriceAssist::preview_update_commodity ()
{
    gnc_commodity *comm = get_commodity_from_dropdown (commodity_selector);
    price_imp->from_commodity (comm);
    preview_refresh_table ();
}

void
CsvImpPriceAssist::preview_refresh_table_idle_cb (gpointer user_data)
{
    auto assist = static_cast<CsvImpPriceAssist *> (user_data);

    assist->preview_refresh_table ();
}

void
CsvImpPriceAssist::preview_queue_refresh_table ()
{
    preview_refresh_idle.queue ();
}

void
CsvImpPriceAssist::preview_reparse_col_type (GncPricePropType type)
{
    auto column_types = price_imp->column_types_price();

    // look for column type and force a reparse
    auto col_type = std::find (column_types.begin(),
                column_types.end(), type);
    if (col_type != column_types.end())
    {
        price_imp->set_column_type_price (col_type -column_types.begin(),
                        type, true);
    }
}

/** Applies a column-type selection and revalidates dependent commodity data. */
void CsvImpPriceAssist::preview_update_col_type (GtkDropDown* dropdown)
{
    auto position = gtk_drop_down_get_selected (dropdown);
    if (position == GTK_INVALID_LIST_POSITION)
        return;

    auto item = g_list_model_get_item (gtk_drop_down_get_model (dropdown), position);
    auto new_col_type = static_cast<GncPricePropType> (GPOINTER_TO_INT (
        g_object_get_data (G_OBJECT (item), COLUMN_TYPE_DATA)));
    g_object_unref (item);
    auto col_num = GPOINTER_TO_UINT (g_object_get_data (G_OBJECT(dropdown), "col-num"));

    auto column_types = price_imp->column_types_price();
    auto old_col_type = column_types.at(col_num);

    price_imp->set_column_type_price (col_num, new_col_type);

    // if old_col_type is TO_CURRENCY, force a reparse of commodity
    if (old_col_type == GncPricePropType::TO_CURRENCY)
    {
        // look for a from_commodity column to reparse
        preview_reparse_col_type (GncPricePropType::FROM_SYMBOL);
        preview_reparse_col_type (GncPricePropType::FROM_NAMESPACE);
    }

    // if old_col_type is FROM_SYMBOL, or FROM_NAMESPACE force a reparse of currency
    if ((old_col_type == GncPricePropType::FROM_SYMBOL) ||
        (old_col_type == GncPricePropType::FROM_NAMESPACE))
    {
        // look for a to_currency column to reparse
        preview_reparse_col_type (GncPricePropType::TO_CURRENCY);
    }

    /* Delay rebuilding our data table to avoid critical warnings due to
     * pending events still acting on them after this event is processed.
     */
    preview_queue_refresh_table ();
}

struct CsvPricePreviewRow
{
    std::vector<std::string> cells;
    std::string error;
    bool skipped;
};

static GObject*
csv_price_preview_row_new (std::vector<std::string> cells, const std::string& error, bool skipped)
{
    auto row = G_OBJECT (g_object_new (G_TYPE_OBJECT, nullptr));
    auto values = new CsvPricePreviewRow { std::move (cells), error, skipped };
    g_object_set_data_full (row, PREVIEW_ROW_DATA, values,
                            [] (gpointer data) { delete static_cast<CsvPricePreviewRow*> (data); });
    return row;
}

static CsvPricePreviewRow*
csv_price_preview_row_get (GObject *row)
{
    return static_cast<CsvPricePreviewRow*> (g_object_get_data (row, PREVIEW_ROW_DATA));
}

static void
csv_price_preview_item_setup (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
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
csv_price_preview_item_bind (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto row = csv_price_preview_row_get (G_OBJECT (gtk_list_item_get_item (item)));
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
csv_price_preview_add_column (GtkColumnView *view, const gchar *title, guint column)
{
    auto factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (factory, "setup", G_CALLBACK (csv_price_preview_item_setup), GUINT_TO_POINTER (column));
    g_signal_connect (factory, "bind", G_CALLBACK (csv_price_preview_item_bind), GUINT_TO_POINTER (column));
    auto view_column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (view_column, column != G_MAXUINT);
    gtk_column_view_append_column (view, view_column);
    g_object_unref (view_column);
}

static void
remove_all_children (GtkWidget *widget)
{
    for (auto child = gtk_widget_get_first_child (widget); child; )
    {
        auto next = gtk_widget_get_next_sibling (child);
        gtk_widget_unparent (child);
        child = next;
    }
}

static void
csv_price_preview_clear_columns (GtkColumnView *view)
{
    auto columns = gtk_column_view_get_columns (view);
    while (g_list_model_get_n_items (columns) > 0)
    {
        auto column = GTK_COLUMN_VIEW_COLUMN (g_list_model_get_item (columns, 0));
        gtk_column_view_remove_column (view, column);
        g_object_unref (column);
    }
}

static GtkDropDown*
csv_price_preview_column_selector_new (uint32_t column, GncPricePropType selected,
                                       CsvImpPriceAssist *assist)
{
    auto store = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    guint selected_position = 0;
    guint position = 0;
    for (auto column_type : gnc_price_col_type_strs)
    {
        auto row = gtk_string_object_new (_(column_type.second));
        g_object_set_data (G_OBJECT (row), COLUMN_TYPE_DATA,
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
    g_signal_connect (dropdown, "notify::selected",
                      G_CALLBACK (csv_price_imp_preview_col_type_changed_cb), assist);
    return dropdown;
}

/* Updates the preview table to show the data parsed from the import file. */
void CsvImpPriceAssist::preview_refresh_table ()
{
    preview_validate_settings ();

    auto store = g_list_store_new (G_TYPE_OBJECT);
    for (const auto& parse_line : price_imp->m_parsed_lines)
    {
        std::vector<std::string> cells;
        for (const auto& cell : std::get<PL_INPUT> (parse_line))
            cells.emplace_back (cell);
        auto row = csv_price_preview_row_new (std::move (cells), std::get<PL_ERROR> (parse_line),
                                              std::get<PL_SKIP> (parse_line));
        g_list_store_append (store, row);
        g_object_unref (row);
    }

    auto selection = gtk_no_selection_new (G_LIST_MODEL (store));
    gtk_column_view_set_model (preview_view, GTK_SELECTION_MODEL (selection));
    g_object_unref (selection);

    csv_price_preview_clear_columns (preview_view);
    remove_all_children (GTK_WIDGET (preview_column_selectors));
    csv_price_preview_add_column (preview_view, "", G_MAXUINT);

    auto column_types = price_imp->column_types_price ();
    for (uint32_t column = 0; column < column_types.size (); column++)
    {
        auto selector_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 2);
        auto selector_label = gtk_label_new (nullptr);
        auto label = g_strdup_printf (_("Column %u"), column + 1);
        gtk_label_set_text (GTK_LABEL (selector_label), label);
        gtk_label_set_xalign (GTK_LABEL (selector_label), 0.0);
        g_free (label);
        auto selector = csv_price_preview_column_selector_new (column, column_types.at (column), this);
        gtk_box_append (GTK_BOX (selector_box), selector_label);
        gtk_box_append (GTK_BOX (selector_box), GTK_WIDGET (selector));
        gtk_box_append (preview_column_selectors, selector_box);

        auto title = g_strdup_printf (_("Column %u"), column + 1);
        csv_price_preview_add_column (preview_view, title, column);
        g_free (title);
    }

    auto any_of_type = [] (const std::vector<GncPricePropType>& types, GncPricePropType wanted)
    {
        return std::any_of (types.begin (), types.end (), [wanted] (auto type) { return type == wanted; });
    };
    if (any_of_type (column_types, GncPricePropType::FROM_NAMESPACE) ||
        any_of_type (column_types, GncPricePropType::FROM_SYMBOL))
    {
        g_signal_handlers_block_by_func (commodity_selector,
                                         (gpointer) csv_price_imp_preview_commodity_sel_cb, this);
        set_commodity_for_dropdown (commodity_selector, nullptr);
        g_signal_handlers_unblock_by_func (commodity_selector,
                                           (gpointer) csv_price_imp_preview_commodity_sel_cb, this);
    }
    if (any_of_type (column_types, GncPricePropType::TO_CURRENCY))
    {
        g_signal_handlers_block_by_func (currency_selector,
                                         (gpointer) csv_price_imp_preview_currency_sel_cb, this);
        set_commodity_for_dropdown (currency_selector, nullptr);
        g_signal_handlers_unblock_by_func (currency_selector,
                                           (gpointer) csv_price_imp_preview_currency_sel_cb, this);
    }
}

/* Update the preview page based on the current state of the importer.
 * Should be called when settings are changed.
 */
void
CsvImpPriceAssist::preview_refresh ()
{
    // Cache skip settings. Updating the widgets one by one
    // triggers a callback that transfers all skip widgets'
    // values to settings. So by the time the next widget value
    // is to be set, that widget's 'new' setting has already been
    // replaced by its old setting preveneting us from using it
    // here sensibly.

    auto skip_start_lines = price_imp->skip_start_lines();
    auto skip_end_lines = price_imp->skip_end_lines();
    auto skip_alt_lines = price_imp->skip_alt_lines();

    // Set start row
    auto adj = gtk_spin_button_get_adjustment (start_row_spin);
    gtk_adjustment_set_upper (adj, price_imp->m_parsed_lines.size());
    gtk_spin_button_set_value (start_row_spin, skip_start_lines);

    // Set end row
    adj = gtk_spin_button_get_adjustment (end_row_spin);
    gtk_adjustment_set_upper (adj, price_imp->m_parsed_lines.size());
    gtk_spin_button_set_value (end_row_spin, skip_end_lines);

    // Set Alternate rows
    gtk_check_button_set_active (GTK_CHECK_BUTTON(skip_alt_rows_button),
                                  skip_alt_lines);

    // Set over-write indicator
    gtk_check_button_set_active (GTK_CHECK_BUTTON(over_write_cbutton),
                                  price_imp->over_write());

    // Set Import Format
    gtk_check_button_set_active (GTK_CHECK_BUTTON(csv_button),
            (price_imp->file_format() == GncImpFileFormat::CSV));
    gtk_check_button_set_active (GTK_CHECK_BUTTON(fixed_button),
            (price_imp->file_format() != GncImpFileFormat::CSV));

    // This section deals with the combo's and character encoding
    gtk_drop_down_set_selected (date_format_dropdown, price_imp->date_format());
    gtk_drop_down_set_selected (currency_format_dropdown, price_imp->currency_format());
    go_charmap_sel_set_encoding (encselector, price_imp->encoding().c_str());

    // Set the commodity and currency combos
    set_commodity_for_dropdown (commodity_selector, price_imp->from_commodity());

    set_commodity_for_dropdown (currency_selector, price_imp->to_currency());

    // Handle separator checkboxes and custom field, only relevant if the file format is csv
    // Note we defer the change signal until all buttons have been updated
    // An early update may result in an incomplete tokenize run that would
    // cause our list of saved column types to be truncated
    if (price_imp->file_format() == GncImpFileFormat::CSV)
    {
        auto separators = price_imp->separators();
        auto escape = price_imp->enable_escape();
        const auto stock_sep_chars = std::string (" \t,:;-");

        for (int i = 0; i < SEP_NUM_OF_TYPES; i++)
        {
            g_signal_handlers_block_by_func (sep_button[i], (gpointer) csv_price_imp_preview_sep_button_cb, this);
            gtk_check_button_set_active (sep_button[i],
                separators.find (stock_sep_chars[i]) != std::string::npos);
            g_signal_handlers_unblock_by_func (sep_button[i], (gpointer) csv_price_imp_preview_sep_button_cb, this);
        }

        // If there are any other separators in the separators string,
        // add them as custom separators
        auto pos = separators.find_first_of (stock_sep_chars);
        while (!separators.empty() && pos != std::string::npos)
        {
            separators.erase(pos);
            pos = separators.find_first_of (stock_sep_chars);
        }
        g_signal_handlers_block_by_func (escape_cbutton, (gpointer) csv_price_imp_preview_sep_button_cb, this);
        gtk_check_button_set_active (escape_cbutton,
                                      escape);
        g_signal_handlers_unblock_by_func (escape_cbutton, (gpointer) csv_price_imp_preview_sep_button_cb, this);

        g_signal_handlers_block_by_func (custom_cbutton, (gpointer) csv_price_imp_preview_sep_button_cb, this);
        g_signal_handlers_block_by_func (custom_entry, (gpointer) csv_price_imp_preview_sep_button_cb, this);

        gtk_check_button_set_active (custom_cbutton,
                                      !separators.empty());
        gnc_entry_set_text (GTK_ENTRY(custom_entry), separators.c_str());

        g_signal_handlers_unblock_by_func (custom_cbutton, (gpointer) csv_price_imp_preview_sep_button_cb, this);
        g_signal_handlers_unblock_by_func (custom_entry, (gpointer) csv_price_imp_preview_sep_button_cb, this);
        try
        {
            price_imp->tokenize (false);
        }
        catch (std::range_error& err)
        {
            PERR ("CSV Tokenization Failed: %s", err.what ());
        }
    }
    // Repopulate the parsed data table
    preview_queue_refresh_table ();
}

/* Check if all selected data can be parsed sufficiently to continue
 */
void CsvImpPriceAssist::preview_validate_settings ()
{
    /* Allow the user to proceed only if there are no inconsistencies in the settings */
    auto error_msg = price_imp->verify();
    gnc_import_assistant_set_page_complete (csv_imp_asst, preview_page, error_msg.empty());
    gtk_label_set_markup(GTK_LABEL(instructions_label), error_msg.c_str());
    gtk_widget_set_visible (GTK_WIDGET(instructions_image), !error_msg.empty());
}

/*******************************************************
 * Assistant page prepare functions
 *******************************************************/

void
CsvImpPriceAssist::assist_file_page_prepare ()
{
    gtk_label_set_text (GTK_LABEL (file_name_label),
                        m_fc_file_name.empty () ? _("No file selected")
                                               : m_fc_file_name.c_str());
    gnc_import_assistant_set_page_complete (csv_imp_asst, file_page,
                                     !m_fc_file_name.empty ());
    gnc_import_assistant_set_page_complete (csv_imp_asst, preview_page, false);
}

void
CsvImpPriceAssist::assist_preview_page_prepare ()
{
    auto go_back = false;


    if (m_final_file_name != m_fc_file_name)
    {
        /* Load the file into parse_data. */
        price_imp = std::unique_ptr<GncPriceImport>(new GncPriceImport);
        /* Assume data is CSV. User can later override to Fixed Width if needed */
        try
        {
            price_imp->file_format (GncImpFileFormat::CSV);
            price_imp->load_file (m_fc_file_name);
            price_imp->tokenize (true);

            /* Get settings store and populate */
            preview_populate_settings_combo();
            gtk_drop_down_set_selected (settings_dropdown, 0);

            // set over_write to false as default
            price_imp->over_write (false);

            /* Disable the "Next" Assistant Button */
            gnc_import_assistant_set_page_complete (csv_imp_asst, preview_page, false);
        }
        catch (std::ifstream::failure& e)
        {
            /* File loading failed ... */
            gnc_error_dialog (GTK_WINDOW(csv_imp_asst), "%s", e.what());
            go_back = true;
        }
        catch (std::range_error &e)
        {
            /* Parsing failed ... */
            gnc_error_dialog (GTK_WINDOW(csv_imp_asst), "%s", _(e.what()));
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
CsvImpPriceAssist::assist_confirm_page_prepare ()
{
    /* Confirm Page */
}

void
CsvImpPriceAssist::assist_summary_page_prepare ()
{
    auto text = std::string("<span size=\"medium\"><b>");
    /* Translators: This is a ngettext(3) message, %d is the number of prices added */
    auto added_str = g_strdup_printf (ngettext ("%d added price",
                                                "%d added prices",
                                                price_imp->m_prices_added),
                                                price_imp->m_prices_added);
    /* Translators: This is a ngettext(3) message, %d is the number of duplicate prices */
    auto dupl_str = g_strdup_printf (ngettext ("%d duplicate price",
                                               "%d duplicate prices",
                                               price_imp->m_prices_duplicated),
                                               price_imp->m_prices_duplicated);
    /* Translators: This is a ngettext(3) message, %d is the number of replaced prices */
    auto repl_str = g_strdup_printf (ngettext ("%d replaced price",
                                               "%d replaced prices",
                                               price_imp->m_prices_replaced),
                                               price_imp->m_prices_replaced);
    auto msg = g_strdup_printf (
        _("The prices were imported from file '%s'.\n\n"
          "Import summary:\n"
          "- %s\n"
          "- %s\n"
          "- %s"),
          m_final_file_name.c_str(), added_str, dupl_str,repl_str);
    text += msg;
    text += "</b></span>";

    g_free (added_str);
    g_free (dupl_str);
    g_free (repl_str);

    gtk_label_set_markup (GTK_LABEL(summary_label), text.c_str());
}

void
CsvImpPriceAssist::assist_prepare_cb (GtkWidget *page)
{
    if (page == file_page)
        assist_file_page_prepare ();
    else if (page == preview_page)
        assist_preview_page_prepare ();
    else if (page == confirm_page)
        assist_confirm_page_prepare ();
    else if (page == summary_page)
        assist_summary_page_prepare ();
}

void
CsvImpPriceAssist::assist_finish ()
{
    /* Start the import */
    /* Create prices from the parsed data */
    try
    {
        price_imp->create_prices ();
        gnc_gui_refresh_all ();
        gnc_import_assistant_set_current_page (csv_imp_asst, 4);
    }
    catch (const std::invalid_argument& err)
    {
        /* Oops! This shouldn't happen when using the import assistant !
         * Inform the user and go back to the preview page.
         */
        gnc_error_dialog (GTK_WINDOW(csv_imp_asst),
            _("An unexpected error has occurred while creating prices. Please report this as a bug.\n\n"
              "Error message:\n%s"), err.what());
        gnc_import_assistant_set_current_page (csv_imp_asst, 2);
    }
}

void
CsvImpPriceAssist::assist_compmgr_close ()
{
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(csv_imp_asst));
}

static void
csv_price_imp_close_handler (gpointer user_data)
{
    auto info = (CsvImpPriceAssist*)user_data;
    gnc_unregister_gui_component_by_data (ASSISTANT_CSV_IMPORT_PRICE_CM_CLASS, info);
    info->assist_compmgr_close();
    delete info;
}

/********************************************************************\
 * gnc_file_csv_price_import                                        *
 * opens up a assistant to import prices.                           *
 *                                                                  *
 * Args:   none                                                     *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_file_csv_price_import(void)
{
    auto info = new CsvImpPriceAssist;
    gnc_register_gui_component (ASSISTANT_CSV_IMPORT_PRICE_CM_CLASS,
                                nullptr, csv_price_imp_close_handler,
                                info);
}
