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
#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-ui.h"
#include "gnc-uri-utils.h"
#include "gnc-ui-util.h"
#include "dialog-utils.h"

#include "gnc-component-manager.h"

#include "assistant-utils.h"
#include "assistant-csv-trans-import.h"

#include "import-account-matcher.h"
#include "import-main-matcher.h"

#include "gnc-csv-model.h"
#include "gnc-csv-gnumeric-popup.h"
#include <goffice/gtk/go-charmap-sel.h>

#define MIN_COL_WIDTH 70
#define GNC_PREFS_GROUP "dialogs.import.csv"
#define ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS "assistant-csv-trans-import"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

/** Enumeration for separator checkbutton types. These are the
 * different types of checkbuttons that the user can click to
 * configure separators in a delimited file. */
enum SEP_BUTTON_TYPES {SEP_SPACE, SEP_TAB, SEP_COMMA, SEP_COLON, SEP_SEMICOLON, SEP_HYPHEN,
                       SEP_NUM_OF_TYPES
                      };

typedef struct
{

    GtkWidget       *window;

    GtkWidget       *file_chooser;                  /**< The widget for the file chooser */
    gchar           *starting_dir;                  /**< The starting directory for import file */
    gchar           *file_name;                     /**< The import file name */

    GtkWidget       *check_label;                   /**< The widget for the check label */
    GtkWidget       *check_butt;                    /**< The widget for the check label button */
    GtkWidget       *start_row_spin;                /**< The widget for the start row spinner */
    GtkWidget       *end_row_spin;                  /**< The widget for the end row spinner */
    GtkWidget       *skip_rows;                     /**< The widget for Skip alternate rows from start row */
    int              start_row;                     /**< The liststore start row, smallest is 0 */
    int              end_row;                       /**< The liststore end row, max number of rows -1 */

    GncCsvParseData *parse_data;                    /**< The actual data we are previewing */
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
    gboolean         encoding_selected_called;      /**< Before encoding_selected is first called, this is FALSE.
                                                       * (See description of encoding_selected.) */
    gboolean         not_empty;                     /**< FALSE initially, true after the first type gnc_csv_preview_update_assist is called. */
    gboolean         previewing_errors;             /**< TRUE if the dialog is displaying
                                                       * error lines, instead of all the file data. */
    int              code_encoding_calls;           /**< Normally this is 0. If the computer
                                                       * changes encselector, this is set to
                                                       * 2. encoding_selected is called twice,
                                                       * each time decrementing this by 1. */
    gboolean         approved;                      /**< This is FALSE until the user clicks "OK". */
    GtkWidget      **treeview_buttons;              /**< This array contains the header buttons in treeview */
    int              num_of_rows;                   /**< The number of rows in the store */
    int              longest_line;                  /**< The length of the longest row */
    int              fixed_context_col;             /**< The number of the column whose the user has clicked */
    int              fixed_context_dx;              /**< The horizontal coordinate of the pixel in the header of the column
                                                       * the user has clicked */

    GtkWidget           *account_page;              /**< The widget for the account page, to be packed with the account matcher */
    GtkWidget           *account_label;             /**< The account page label at bottom of page */
    AccountPickerDialog *account_picker;            /**< The AccountPickerDialog structure */
    gboolean             account_page_step;         /**< Allows for auto stepping the account page if we match online id */

    GNCImportMainMatcher *gnc_csv_importer_gui;     /**< The GNCImportMainMatcher structure */
    GtkWidget            *match_page;               /**< The widget for the matcher page, to be packed with the transaction matcher */
    GtkWidget            *match_label;              /**< The match label at the bottom of the page */
    GtkWidget            *help_button;              /**< The widget for the help button on the matcher page */
    GtkWidget            *cancel_button;            /**< The widget for the new cancel button when going back is blocked */
    gboolean              match_parse_run;          /**< This is set after the first run */

    gchar                *checked_date;             /**< the text checked to be a date, but failed */

    GtkWidget            *summary_label;            /**< The summary text */

    gboolean              new_book;                 /**< Are we importing into a new book?; if yes, call book options */

} CsvImportTrans;


/*************************************************************************/

void csv_import_trans_assistant_prepare (GtkAssistant  *assistant, GtkWidget *page, gpointer user_data);
void csv_import_trans_assistant_finish (GtkAssistant *gtkassistant, gpointer user_data);
void csv_import_trans_assistant_cancel (GtkAssistant *gtkassistant, gpointer user_data);
void csv_import_trans_assistant_close (GtkAssistant *gtkassistant, gpointer user_data);

void csv_import_trans_assistant_start_page_prepare (GtkAssistant *gtkassistant, gpointer user_data);
void csv_import_trans_assistant_file_page_prepare (GtkAssistant *assistant, gpointer user_data);
void csv_import_trans_assistant_preview_page_prepare (GtkAssistant *gtkassistant, gpointer user_data);
void csv_import_trans_assistant_account_page_prepare (GtkAssistant *assistant, gpointer user_data);
void csv_import_trans_assistant_doc_page_prepare (GtkAssistant *assistant, gpointer user_data);
void csv_import_trans_assistant_match_page_prepare (GtkAssistant *assistant, gpointer user_data);
void csv_import_trans_assistant_finish_page_prepare (GtkAssistant *assistant, gpointer user_data);
void csv_import_trans_assistant_summary_page_prepare (GtkAssistant *assistant, gpointer user_data);

void csv_import_trans_srow_cb (GtkWidget *spin, gpointer user_data);
void csv_import_trans_erow_cb (GtkWidget *spin, gpointer user_data);
void csv_import_trans_skiprows_cb (GtkWidget *checkbox, gpointer user_data);
void csv_import_trans_auto_cb (GtkWidget *cb, gpointer user_data);
void csv_import_trans_file_chooser_confirm_cb (GtkWidget *button, CsvImportTrans *info);

static void gnc_csv_preview_update_assist (CsvImportTrans* info);
int preview_settings_valid (CsvImportTrans *info);

/*************************************************************************/

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
    GError* error;
    gchar *file_name;
    GncCsvParseData* parse_data;

    gtk_assistant_set_page_complete (assistant, page, FALSE);

    file_name = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER(info->file_chooser));

    if (file_name)
    {
        gchar *filepath = gnc_uri_get_path (file_name);
        gchar *filedir = g_path_get_dirname (filepath);
        info->starting_dir = g_strdup (filedir);
        g_free (filedir);
        g_free (filepath);

        info->file_name = g_strdup (file_name);
        error = NULL;
        /* Load the file into parse_data. */
        parse_data = gnc_csv_new_parse_data();
        if (gnc_csv_load_file (parse_data, file_name, &error))
        {
            /* If we couldn't load the file ... */
            gnc_error_dialog (NULL, "%s", error->message);
            if (error->code == GNC_CSV_FILE_OPEN_ERR)
            {
                gnc_csv_parse_data_free (parse_data);
                return;
            }
            /* If we couldn't guess the encoding, we are content with just
             * displaying an error message and move on with a blank
             * display. */
        }
        else
        {
            /* Parse the data. */
            if (gnc_csv_parse (parse_data, TRUE, &error))
            {
                /* If we couldn't parse the data ... */
                gnc_error_dialog (NULL, "%s", error->message);
                gnc_csv_parse_data_free (parse_data);
            }
            else
            {
                info->parse_data = parse_data;
                info->previewing_errors = FALSE; /* We're looking at all the data. */
                info->approved = FALSE; /* This is FALSE until the user clicks "OK". */
                gtk_assistant_set_page_complete (assistant, page, TRUE);
            }
        }
    }
    g_free (file_name);

    DEBUG("file_name selected is %s", info->file_name);
    DEBUG("starting directory is %s", info->starting_dir);

    /* Step to next page if page is complete */
    if(gtk_assistant_get_page_complete (assistant, page))
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
    GtkListStore *store;
    GtkTreeIter iter;
    gboolean valid;
    int i = 0;

    store = GTK_LIST_STORE(gtk_tree_view_get_model (info->treeview));

    /* Start of file */
    for ( i = 0; i <= info->start_row; i++)
    {
        /* Modify background color of rows less than start row */
        if (info->start_row == i)
        {
            valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(store), &iter, NULL, i);
            if (valid)
                gtk_list_store_set (store, &iter, 0, NULL, -1);
        }
        else
        {
            valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(store), &iter, NULL, i);
            if (valid)
                gtk_list_store_set (store, &iter, 0, "pink", -1);
            valid = gtk_tree_model_iter_next (GTK_TREE_MODEL(store), &iter);
            if (valid)
                gtk_list_store_set (store, &iter, 0, NULL, -1);
        }

    }

    /* End of File */
    for ( i = info->num_of_rows - 1; i >= info->end_row; i--)
    {
        /* Modify background color of rows more than end row */
        if (i == info->end_row)
        {
            valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(store), &iter, NULL, i);
            if (valid)
                gtk_list_store_set (store, &iter, 0, NULL, -1);
        }
        else
        {
            valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(store), &iter, NULL, i);
            if (valid)
                gtk_list_store_set (store, &iter, 0, "pink", -1);
            valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(store), &iter, NULL, i - 1);
            if (valid)
                gtk_list_store_set (store, &iter, 0, NULL, -1);
        }
    }

    /* Remove background color from the start row to end row */
    for ( i = info->start_row + 1; i <= info->end_row; i++)
    {
        valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(store), &iter, NULL, i);
        if (valid)
            gtk_list_store_set (store, &iter, 0, NULL, -1);
    }

    /* Skip rows */
    if (info->parse_data->skip_rows == TRUE)
    {
        for ( i = info->start_row + 1; i <= info->end_row; i = i + 2)
        {
            /* Modify background color of alternate rows from the start row */
            valid = gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(store), &iter, NULL, i);
            if (valid)
                gtk_list_store_set (store, &iter, 0, "pink", -1);
        }
    }
}


/*******************************************************
 * csv_import_trans_srow_cb
 *
 * call back for import start row
 *******************************************************/
void csv_import_trans_srow_cb (GtkWidget *spin, gpointer user_data)
{
    CsvImportTrans *info = user_data;
    GtkAdjustment *adj;

    /* Get number of rows for header */
    info->start_row = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spin)) - 1;

    info->parse_data->start_row = info->start_row;

    adj = gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON(info->end_row_spin));
    gtk_adjustment_set_lower (adj, info->start_row + 1);

    /* Refresh the row highlighting */
    row_selection_update (info);
}


/*******************************************************
 * csv_import_trans_erow_cb
 *
 * call back for import end row
 *******************************************************/
void csv_import_trans_erow_cb (GtkWidget *spin, gpointer user_data)
{
    CsvImportTrans *info = user_data;
    GtkAdjustment *adj;

    /* Get number of rows for header */
    info->end_row = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spin)) - 1;

    info->parse_data->end_row = info->end_row + 1;

    adj = gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON(info->start_row_spin));
    gtk_adjustment_set_upper (adj, info->end_row + 1);

    /* Refresh the row highlighting */
    row_selection_update (info);
}


/*******************************************************
 * csv_import_trans_auto_cb
 *
 * call back for auto create account / Skip Errors
 *******************************************************/
void csv_import_trans_auto_cb (GtkWidget *cb, gpointer user_data)
{
    CsvImportTrans *info = user_data;

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(cb)))
    {
        if (info->previewing_errors == TRUE)
            info->approved = TRUE;
        else
            info->account_picker->auto_create = TRUE;
    }
    else
    {
        if (info->previewing_errors == TRUE)
            info->approved = FALSE;
        else
            info->account_picker->auto_create = FALSE;
    }
}


/*******************************************************
 * csv_import_trans_skiprows_cb
 *
 * call back for import skip rows checkbox
 *******************************************************/
void csv_import_trans_skiprows_cb (GtkWidget *checkbox, gpointer user_data)
{
    CsvImportTrans *info = user_data;

    /* Set the skip_rows variable */
    info->parse_data->skip_rows = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(checkbox));

    /* Refresh the row highlighting */
    row_selection_update (info);
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
static void sep_button_clicked (GtkWidget* widget, CsvImportTrans* info)
{
    int i;
    char* stock_separator_characters[] = {" ", "\t", ",", ":", ";", "-"};
    GSList* checked_separators = NULL;
    GError* error;

    /* Add the corresponding characters to checked_separators for each
     * button that is checked. */
    for (i = 0; i < SEP_NUM_OF_TYPES; i++)
    {
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(info->sep_buttons[i])))
            checked_separators = g_slist_append (checked_separators, stock_separator_characters[i]);
    }

    /* Add the custom separator if the user checked its button. */
    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(info->custom_cbutton)))
    {
        char* custom_sep = (char*)gtk_entry_get_text (info->custom_entry);
        if (custom_sep[0] != '\0') /* Don't add a blank separator (bad things will happen!). */
            checked_separators = g_slist_append (checked_separators, custom_sep);
    }

    /* Set the parse options using the checked_separators list. */
    stf_parse_options_csv_set_separators (info->parse_data->options, NULL, checked_separators);
    g_slist_free (checked_separators);

    /* Parse the data using the new options. We don't want to reguess
     * the column types because we want to leave the user's
     * configurations in tact. */
    if (gnc_csv_parse (info->parse_data, FALSE, &error))
    {
        /* Warn the user there was a problem and try to undo what caused
         * the error. (This will cause a reparsing and ideally a usable
         * configuration.) */
        gnc_error_dialog (NULL, "Error in parsing");
        /* If the user changed the custom separator, erase that custom separator. */
        if (widget == GTK_WIDGET(info->custom_entry))
        {
            gtk_entry_set_text (GTK_ENTRY(widget), "");
        }
        /* If the user checked a checkbutton, toggle that checkbutton back. */
        else
        {
            gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(widget),
                                         !gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(widget)));
        }
        return;
    }

    /* If we parsed successfully, redisplay the data. */
    gnc_csv_preview_update_assist (info);

    /* Refresh the row highlighting */
    row_selection_update (info);
}


/** Event handler for clicking one of the format type radio
 * buttons. This occurs if the format (Fixed-Width or CSV) is changed.
 * @param csv_button The "Separated" radio button
 * @param info The display of the data being imported
 */
static void separated_or_fixed_selected (GtkToggleButton* csv_button, CsvImportTrans* info)
{
    GError* error = NULL;
    /* Set the parsing type correctly. */
    if (gtk_toggle_button_get_active (csv_button)) /* If we're in CSV mode ... */
    {
        stf_parse_options_set_type (info->parse_data->options, PARSE_TYPE_CSV);
    }
    else /* If we're in fixed-width mode ... */
    {
        stf_parse_options_set_type (info->parse_data->options, PARSE_TYPE_FIXED);
    }

    /* Reparse the data. */
    if (gnc_csv_parse (info->parse_data, FALSE, &error))
    {
        /* Show an error dialog explaining the problem. */
        gnc_error_dialog (NULL, "%s", error->message);
        return;
    }

    /* Show the new data. */
    gnc_csv_preview_update_assist (info);

    /* Refresh the row highlighting */
    row_selection_update (info);
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
    /* This gets called twice everytime a new encoding is selected. The
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
        const char* previous_encoding = info->parse_data->encoding;
        GError* error = NULL;
        /* Try converting the new encoding and reparsing. */
        if (gnc_csv_convert_encoding (info->parse_data, encoding, &error) ||
                gnc_csv_parse (info->parse_data, FALSE, &error))
        {
            /* If it fails, change back to the old encoding. */
            gnc_error_dialog (NULL, "%s", _("Invalid encoding selected"));
            info->encoding_selected_called = FALSE;
            go_charmap_sel_set_encoding (selector, previous_encoding);
            return;
        }

        gnc_csv_preview_update_assist (info);
        info->encoding_selected_called = FALSE;
    }
    else /* If this is the first call of the function ... */
    {
        info->encoding_selected_called = TRUE; /* ... set the flag and wait for the next call. */
    }
}


/** Event handler for selecting a new date format.
 * @param format_selector The combo box for selecting date formats
 * @param info The display of the data being imported
 */
static void date_format_selected (GtkComboBoxText* format_selector, CsvImportTrans* info)
{
    info->parse_data->date_format = gtk_combo_box_get_active (GTK_COMBO_BOX(format_selector));
}


/** Event handler for selecting a new currency format.
 * @param currency_selector The combo box for selecting currency formats
 * @param info The display of the data being imported
 */
static void currency_format_selected (GtkComboBoxText* currency_selector, CsvImportTrans* info)
{
    info->parse_data->currency_format = gtk_combo_box_get_active (GTK_COMBO_BOX(currency_selector));
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

static gboolean
make_new_column (CsvImportTrans* info, int col, int dx, gboolean test_only)
{
    PangoLayout *layout;
    PangoFontDescription *font_desc;
    int charindex, width;
    GtkCellRenderer *cell =	gnc_csv_preview_get_cell_renderer (info, col);
    int colstart, colend;
    GError* error = NULL;

    colstart = (col == 0)
               ? 0
               : stf_parse_options_fixed_splitpositions_nth (info->parse_data->options, col - 1);
    colend = stf_parse_options_fixed_splitpositions_nth (info->parse_data->options, col);

    g_object_get (G_OBJECT(cell), "font_desc", &font_desc, NULL);
    layout = gtk_widget_create_pango_layout (GTK_WIDGET(info->treeview), "x");
    pango_layout_set_font_description (layout, font_desc);
    pango_layout_get_pixel_size (layout, &width, NULL);
    if (width < 1) width = 1;
    charindex = colstart + (dx + width / 2) / width;
    g_object_unref (layout);
    pango_font_description_free (font_desc);

    if (charindex <= colstart || (colend != -1 && charindex >= colend))
        return FALSE;

    if (!test_only)
    {
        stf_parse_options_fixed_splitpositions_add (info->parse_data->options, charindex);
        if (gnc_csv_parse (info->parse_data, FALSE, &error))
        {
            gnc_error_dialog (NULL, "%s", error->message);
            return FALSE;
        }
        gnc_csv_preview_update_assist (info);
    }

    return TRUE;
}

static gboolean
widen_column (CsvImportTrans* info, int col, gboolean test_only)
{
    int colcount = stf_parse_options_fixed_splitpositions_count (info->parse_data->options);
    int nextstart, nextnextstart;
    GError* error = NULL;

    if (col >= colcount - 1)
        return FALSE;

    nextstart = stf_parse_options_fixed_splitpositions_nth (info->parse_data->options, col);

    nextnextstart = (col == colcount - 2)
                    ? info->longest_line
                    : stf_parse_options_fixed_splitpositions_nth (info->parse_data->options, col + 1);

    if (nextstart + 1 >= nextnextstart)
        return FALSE;

    if (!test_only)
    {
        stf_parse_options_fixed_splitpositions_remove (info->parse_data->options, nextstart);
        stf_parse_options_fixed_splitpositions_add (info->parse_data->options, nextstart + 1);
        if (gnc_csv_parse (info->parse_data, FALSE, &error))
        {
            gnc_error_dialog (NULL, "%s", error->message);
            return FALSE;
        }
        gnc_csv_preview_update_assist (info);
    }
    return TRUE;
}

static gboolean
narrow_column (CsvImportTrans* info, int col, gboolean test_only)
{
    int colcount = stf_parse_options_fixed_splitpositions_count (info->parse_data->options);
    int thisstart, nextstart;
    GError* error = NULL;

    if (col >= colcount - 1)
        return FALSE;

    thisstart = (col == 0)
                ? 0
                : stf_parse_options_fixed_splitpositions_nth (info->parse_data->options, col - 1);
    nextstart = stf_parse_options_fixed_splitpositions_nth (info->parse_data->options, col);

    if (nextstart - 1 <= thisstart)
        return FALSE;

    if (!test_only)
    {
        stf_parse_options_fixed_splitpositions_remove (info->parse_data->options, nextstart);
        stf_parse_options_fixed_splitpositions_add (info->parse_data->options, nextstart - 1);
        if (gnc_csv_parse (info->parse_data, FALSE, &error))
        {
            gnc_error_dialog (NULL, "%s", error->message);
            return FALSE;
        }
        gnc_csv_preview_update_assist (info);
    }
    return TRUE;
}

static gboolean
delete_column (CsvImportTrans* info, int col, gboolean test_only)
{
    GError* error = NULL;
    int colcount = stf_parse_options_fixed_splitpositions_count (info->parse_data->options);
    if (col < 0 || col >= colcount - 1)
        return FALSE;

    if (!test_only)
    {
        int nextstart = stf_parse_options_fixed_splitpositions_nth (info->parse_data->options, col);
        stf_parse_options_fixed_splitpositions_remove (info->parse_data->options, nextstart);
        if (gnc_csv_parse (info->parse_data, FALSE, &error))
        {
            gnc_error_dialog (NULL, "%s", error->message);
            return FALSE;
        }
        gnc_csv_preview_update_assist (info);
    }
    return TRUE;
}

static void
select_column (CsvImportTrans* info, int col)
{
    int colcount = stf_parse_options_fixed_splitpositions_count (info->parse_data->options);
    GtkTreeViewColumn *column;

    if (col < 0 || col >= colcount)
        return;

    column = gtk_tree_view_get_column (info->treeview, col);
    gtk_widget_grab_focus (column->button);
}

static gboolean
fixed_context_menu_handler (GnumericPopupMenuElement const *element,
                            gpointer user_data)
{
    CsvImportTrans* info = user_data;
    int col = info->fixed_context_col;

    switch (element->index)
    {
    case CONTEXT_STF_IMPORT_MERGE_LEFT:
        delete_column (info, col - 1, FALSE);
        break;
    case CONTEXT_STF_IMPORT_MERGE_RIGHT:
        delete_column (info, col, FALSE);
        break;
    case CONTEXT_STF_IMPORT_SPLIT:
        make_new_column (info, col, info->fixed_context_dx, FALSE);
        break;
    case CONTEXT_STF_IMPORT_WIDEN:
        widen_column (info, col, FALSE);
        break;
    case CONTEXT_STF_IMPORT_NARROW:
        narrow_column (info, col, FALSE);
        break;
    default:
        ; /* Nothing */
    }
    return TRUE;
}

static void
fixed_context_menu (CsvImportTrans* info, GdkEventButton *event,
                    int col, int dx)
{
    int sensitivity_filter = 0;

    info->fixed_context_col = col;
    info->fixed_context_dx = dx;

    if (!delete_column (info, col - 1, TRUE))
        sensitivity_filter |= (1 << CONTEXT_STF_IMPORT_MERGE_LEFT);
    if (!delete_column (info, col, TRUE))
        sensitivity_filter |= (1 << CONTEXT_STF_IMPORT_MERGE_RIGHT);
    if (!make_new_column (info, col, dx, TRUE))
        sensitivity_filter |= (1 << CONTEXT_STF_IMPORT_SPLIT);
    if (!widen_column (info, col, TRUE))
        sensitivity_filter |= (1 << CONTEXT_STF_IMPORT_WIDEN);
    if (!narrow_column (info, col, TRUE))
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
    /* ncols is the number of columns in the data. */
    int i, ncols = info->parse_data->column_types->len;

    /* Go through each column except for the last. (We don't want to set
     * the width of the last column because the user won't be able to
     * shrink the dialog back if it's expanded.) */

    for (i = 0; i < ncols - 1; i++)
    {
        gint col_width; /* The width of the column in info->treeview. */
        GtkTreeViewColumn* pcol;
        GtkTreeViewColumn* ccol; /* The corresponding column in info->ctreeview. */

        /* Get the width. */
        col_width = gtk_tree_view_column_get_width (gtk_tree_view_get_column (info->treeview, i));
        /* Set the minumum width for a column so that drop down selector can be seen. */
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
    /* ncols is the number of columns in the data. */
    int i, ncols = info->parse_data->column_types->len;
    /* store has the actual strings that appear in info->ctreeview. */
    GtkTreeModel* store = gtk_tree_view_get_model (info->ctreeview);
    GtkTreeModel* model;
    gint textColumn;
    GtkTreeIter iter;
    gchar* new_text;

    /* Get the new text */
    g_object_get (renderer, "model", &model, "text-column", &textColumn, NULL);
    gtk_tree_model_get (model, new_text_iter, textColumn, &new_text, -1);

    /* Get an iterator for the first (and only) row. */
    gtk_tree_model_get_iter_first (store, &iter);

    /* Go through each column. */
    for (i = 0; i < ncols; i++)
    {
        /* We need all this stuff so that we can find out whether or not
         * this was the column that was changed. */
        GtkCellRenderer* col_renderer; /* The renderer for this column. */
        /* The column in the treeview we are looking at */
        GtkTreeViewColumn* col = gtk_tree_view_get_column (info->ctreeview, i);
        /* The list of renderers for col */
        GList* rend_list = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT(col));
        /* rend_list has only one entry, which we put in col_renderer. */
        col_renderer = rend_list->data;
        g_list_free (rend_list);

        /* If this is not the column that was changed ... */
        if (col_renderer != renderer)
        {
            /* The string that appears in the column */
            gchar* contents = NULL;
            /* Get the type string. (store is arranged so that every two
             * columns is a pair of the model used for the combobox and the
             * string that appears, so that store looks like:
             * model 0, string 0, model 1, string 1, ..., model ncols, string ncols. */
            gtk_tree_model_get(store, &iter, 2 * i + 1, &contents, -1);
            /* If this column has the same string that the user selected ... */
            if (!g_strcmp0 (contents, new_text))
            {
                /* ... set this column to the "None" type. (We can't allow duplicate types.) */
                gtk_list_store_set (GTK_LIST_STORE(store), &iter, 2 * i + 1,
                                   _(gnc_csv_column_type_strs[GNC_CSV_NONE]), -1);
            }
            g_free (contents);
        }
        else /* If this is the column that was changed ... */
        {
            /* Set the text for this column to what the user selected. (See
             * comment above "Get the type string. ..." for why we set
             * column 2*i+1 in store.) */
            gtk_list_store_set (GTK_LIST_STORE(store), &iter, 2 * i + 1, new_text, -1);
        }
    }
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
    /* col is the number of the column that was clicked, and offset is
       to correct for the indentation of button. */
    int i, col = 0, offset = GTK_BIN(button)->child->allocation.x - button->allocation.x,
           ncols = info->parse_data->column_types->len;
    /* Find the column that was clicked. */
    for (i = 0; i < ncols; i++)
    {
        if (info->treeview_buttons[i] == button)
        {
            col = i;
            break;
        }
    }

    /* Don't let the user affect the last column if it has error messages. */
    if (info->parse_data->orig_max_row < ncols && ncols - col == 1)
    {
        return;
    }

    /* Double clicks can split columns. */
    if (event->type == GDK_2BUTTON_PRESS && event->button == 1)
    {
        make_new_column (info, col, (int)event->x - offset, FALSE);
    }
    /* Right clicking brings up a context menu. */
    else if (event->type == GDK_BUTTON_PRESS && event->button == 3)
    {
        fixed_context_menu (info, event, col, (int)event->x - offset);
    }
}


/** Test for the required minimum number of coloumns selected and
 * a valid date format.
 *
 *  @param info The data being previewed
 *  @retval 0 all is ok
 *  @retval 1 date format is wrong
 *  @retval 2 too few columns selected
 *  @retval 3 both conditions apply
 */
int preview_settings_valid (CsvImportTrans* info)
{
    /* Shorten the column_types identifier. */
    GArray* column_types = info->parse_data->column_types;
    int i, ncols = column_types->len; /* ncols is the number of columns in the data. */
    int weight = 0;
    gboolean valid = TRUE;
    int ret = 0;
    /* store contains the actual strings appearing in the column types treeview. */
    GtkTreeModel* store = gtk_tree_view_get_model (info->ctreeview);
    /* datastore contains the actual strings appearing in the preview treeview. */
    GtkTreeModel* datastore = gtk_tree_view_get_model (info->treeview);
    GtkTreeIter iter, iter2;
    /* Get an iterator for the first (and only) row. */
    gtk_tree_model_get_iter_first (store, &iter);

    /* Get an iterator for the first required row in the data store. */
    gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(datastore), &iter2, NULL, info->start_row);

    /* Go through each of the columns. */
    for (i = 0; i < ncols; i++)
    {
        int type; /* The column type contained in this column. */
        gchar* contents = NULL; /* The column type string in this column. */
        gchar* prevstr = NULL; /* The string in this column from datastore. */
        gchar* accstr = NULL; /* The string in this column from datastore. */
        /* Get the type string first. (store is arranged so that every two
         * columns is a pair of the model used for the combobox and the
         * string that appears, so that store looks like:
         * model 0, string 0, model 1, string 1, ..., model ncols, string ncols. */
        gtk_tree_model_get (store, &iter, 2 * i + 1, &contents, -1);

        /* Go through each column type until ... */
        for (type = 0; type < GNC_CSV_NUM_COL_TYPES; type++)
        {
            /* ... we find one that matches with what's in the column. */
            if (!g_strcmp0 (contents, _(gnc_csv_column_type_strs[type])))
            {
                /* Set the column_types array appropriately and quit. */
                column_types->data[i] = type;

                switch (type)
                {
                case GNC_CSV_DATE:
                    weight = weight + 1000;
                    gtk_tree_model_get (datastore, &iter2, i + 1, &prevstr, -1);

                    if (parse_date (prevstr, info->parse_data->date_format) == -1) {
                        if (info->checked_date == NULL) { /** no bad example yet? provide one */
                            info->checked_date = g_strdup(prevstr);
                        }
                        valid = FALSE;
                    }
                    break;

                case GNC_CSV_DESCRIPTION:
                case GNC_CSV_NOTES:
                    weight = weight + 100;
                    break;

                case GNC_CSV_BALANCE:
                case GNC_CSV_DEPOSIT:
                case GNC_CSV_WITHDRAWAL:
                    weight = weight + 10;
                    break;

                case GNC_CSV_NUM:
                    weight = weight + 1;
                    break;
                case GNC_CSV_ACCOUNT:
                    weight = weight + 1;
                    gtk_tree_model_get (datastore, &iter2, i + 1, &accstr, -1);
                    info->account_picker->account_online_id_value = strdup (accstr);
                    break;
                }
                break;
            }
        }
        /* Free the type string created by gtk_tree_model_get() */
        g_free (contents);
        g_free (prevstr);
        g_free (accstr);
    }

    if (valid == FALSE) ret |= 1;
    if (weight < 1109)  ret |= 2;

    return ret;
}


/* Loads the preview's data into its data treeview. not_empty is TRUE
 * when the data treeview already contains data, FALSE otherwise
 * (e.g. the first time this function is called on a preview).
 * @param info The data being previewed
 */
static void gnc_csv_preview_update_assist (CsvImportTrans* info)
{
    /* store has the data from the file being imported. cstores is an
     * array of stores that hold the combo box entries for each
     * column. ctstore contains both pointers to models in cstore and
     * the actual text that appears in info->ctreeview. */
    GtkListStore *store, **cstores, *ctstore;
    GtkTreeIter iter;
    GtkTreeSelection *selection;
    /* ncols is the number of columns in the file data. */
    int i, j, ncols = info->parse_data->column_types->len,
              max_str_len = info->parse_data->file_str.end - info->parse_data->file_str.begin;

    /* store contains only strings. */
    GType* types = g_new (GType, 2 * ncols);
    for (i = 0; i <  ncols + 1; i++)
        types[i] = G_TYPE_STRING;
    store = gtk_list_store_newv (ncols + 1, types);

    /* ctstore is arranged as follows:
     * model 0, text 0, model 1, text 1, ..., model ncols, text ncols. */
    for (i = 0; i < 2 * ncols; i += 2)
    {
        types[i] = GTK_TYPE_TREE_MODEL;
        types[i+1] = G_TYPE_STRING;
    }
    ctstore = gtk_list_store_newv (2 * ncols, types);

    g_free (types);

    /* Each element in cstores is a single column model. */
    cstores = g_new (GtkListStore*, ncols);
    for (i = 0; i < ncols; i++)
    {
        cstores[i] = gtk_list_store_new (1, G_TYPE_STRING);
        /* Add all of the possible entries to the combo box. */
        for (j = 0; j < GNC_CSV_NUM_COL_TYPES; j++)
        {
            gtk_list_store_append (cstores[i], &iter);
            gtk_list_store_set (cstores[i], &iter, 0, _(gnc_csv_column_type_strs[j]), -1);
        }
    }

    if (info->not_empty)
    {
        GList *tv_columns, *tv_columns_begin, *ctv_columns, *ctv_columns_begin;
        tv_columns = tv_columns_begin = gtk_tree_view_get_columns (info->treeview);
        ctv_columns = ctv_columns_begin = gtk_tree_view_get_columns (info->ctreeview);
        /* Clear out exisiting columns in info->treeview. */
        while (tv_columns != NULL)
        {
            gtk_tree_view_remove_column (info->treeview, GTK_TREE_VIEW_COLUMN(tv_columns->data));
            tv_columns = g_list_next (tv_columns);
        }
        /* Do the same in info->ctreeview. */
        while (ctv_columns != NULL)
        {
            gtk_tree_view_remove_column (info->ctreeview, GTK_TREE_VIEW_COLUMN(ctv_columns->data));
            ctv_columns = g_list_next (ctv_columns);
        }
        g_list_free (tv_columns_begin);
        g_list_free (ctv_columns_begin);
        g_free (info->treeview_buttons);
    }

    /* Fill the data treeview with data from the file. */
    /* Also, update the longest line value within the following loop (whichever is executed). */
    info->longest_line = 0;
    if (info->previewing_errors) /* If we are showing only errors ... */
    {
        /* ... only pick rows that are in info->error_lines. */
        GList* error_lines = info->parse_data->error_lines;
        while (error_lines != NULL)
        {
            int this_line_length = 0;
            i = GPOINTER_TO_INT(error_lines->data);
            gtk_list_store_append (store, &iter);

            /* Row Color column */
            gtk_list_store_set (store, &iter, 0, NULL, -1);

            for (j = 0; j < ((GPtrArray*)(info->parse_data->orig_lines->pdata[i]))->len; j++)
            {
                /* Add this cell's length to the row's length and set the value of the list store. */
                gchar* cell_string = (gchar*)((GPtrArray*)(info->parse_data->orig_lines->pdata[i]))->pdata[j];
                this_line_length += g_utf8_strlen(cell_string, max_str_len);
                gtk_list_store_set (store, &iter, j + 1, cell_string, -1);
            }

            if (this_line_length > info->longest_line)
                info->longest_line = this_line_length;

            error_lines = g_list_next (error_lines);
        }
    }
    else /* Otherwise, put in all of the data. */
    {
        for (i = 0; i < info->parse_data->orig_lines->len; i++)
        {
            int this_line_length = 0;
            gtk_list_store_append (store, &iter);

            /* Row Color column */
            gtk_list_store_set (store, &iter, 0, NULL, -1);

            for (j = 0; j < ((GPtrArray*)(info->parse_data->orig_lines->pdata[i]))->len; j++)
            {
                /* Add this cell's length to the row's length and set the value of the list store. */
                gchar* cell_string = (gchar*)((GPtrArray*)(info->parse_data->orig_lines->pdata[i]))->pdata[j];
                this_line_length += g_utf8_strlen(cell_string, max_str_len);
                gtk_list_store_set (store, &iter, j + 1, cell_string, -1);
            }

            if (this_line_length > info->longest_line)
                info->longest_line = this_line_length;

            /* Set the number of rows in the store */
            info->num_of_rows = i + 1;
        }
    }

    /* Set all the column types to what's in the parse data. */
    gtk_list_store_append (ctstore, &iter);
    gtk_list_store_set (ctstore, &iter, 0, NULL, -1); /* Dummy Column to match row color */
    for (i = 0; i < ncols; i++)
    {
        gtk_list_store_set (ctstore, &iter, 2 * i, cstores[i], 2 * i + 1,
                           _(gnc_csv_column_type_strs[(int)(info->parse_data->column_types->data[i])]),
                           -1);
    }

    info->treeview_buttons = g_new (GtkWidget*, ncols);
    /* Insert columns into the data and column type treeviews. */
    for (i = 0; i < ncols ; i++)
    {
        GtkTreeViewColumn* col; /* The column we add to info->treeview. */
        /* Create renderers for the data treeview (renderer) and the
         * column type treeview (crenderer). */
        GtkCellRenderer* renderer = gtk_cell_renderer_text_new(),
                         *crenderer = gtk_cell_renderer_combo_new();
        /* We want a monospace font for the data in case of fixed-width data. */
        g_object_set (G_OBJECT(renderer), "family", "monospace", NULL);
        /* We are using cstores for the combo box entries, and we don't
         * want the user to be able to manually enter their own column
         * types. */
        g_object_set (G_OBJECT(crenderer), "model", cstores[i], "text-column", 0,
                     "editable", TRUE, "has-entry", FALSE, NULL);
        g_signal_connect (G_OBJECT(crenderer), "changed",
                         G_CALLBACK(column_type_changed), (gpointer)info);

        /* Add a single column for the treeview. */
        col = gtk_tree_view_column_new_with_attributes ("", renderer, "text", i + 1, NULL);

        /* Add the Color column 0 to the renderer */
        gtk_tree_view_column_add_attribute (col, renderer, "background", 0);

        gtk_tree_view_insert_column (info->treeview, col, -1);
        /* Enable resizing of the columns. */
        gtk_tree_view_column_set_resizable (col, TRUE);
        /* Use the alternating model and text entries from ctstore in
         * info->ctreeview. */
        gtk_tree_view_insert_column_with_attributes (info->ctreeview,
                -1, "", crenderer, "model", 2 * i,
                "text", 2 * i + 1, NULL);

        /* We need to allow clicking on the column headers for fixed-width
         * column splitting and merging. */
        g_object_set (G_OBJECT(col), "clickable", TRUE, NULL);
        g_signal_connect (G_OBJECT(col->button), "button_press_event",
                         G_CALLBACK(header_button_press_handler), (gpointer)info);
        info->treeview_buttons[i] = col->button;
    }

    /* Set the treeviews to use the models. */
    gtk_tree_view_set_model (info->treeview, GTK_TREE_MODEL(store));
    gtk_tree_view_set_model (info->ctreeview, GTK_TREE_MODEL(ctstore));

    /* Select the header row */
    gtk_tree_model_get_iter_first (GTK_TREE_MODEL(ctstore), &iter);
    selection = gtk_tree_view_get_selection (info->ctreeview);
    gtk_tree_selection_select_iter (selection, &iter);

    /* Free the memory for the stores. */
    g_object_unref (GTK_TREE_MODEL(store));
    g_object_unref (GTK_TREE_MODEL(ctstore));
    for (i = 0; i < ncols; i++)
        g_object_unref (GTK_TREE_MODEL(cstores[i]));

    /* Make the things actually appear. */
    gtk_widget_show_all (GTK_WIDGET(info->treeview));
    gtk_widget_show_all (GTK_WIDGET(info->ctreeview));

    /* Set the encoding selector to the right encoding. */
    info->code_encoding_calls = 2;
    go_charmap_sel_set_encoding (info->encselector, info->parse_data->encoding);

    /* Set the date format to what's in the combo box (since we don't
     * necessarily know if this will always be the same). */
    info->parse_data->date_format = gtk_combo_box_get_active (GTK_COMBO_BOX(info->date_format_combo));

    /* It's now been filled with some stuff. */
    info->not_empty = TRUE;
}


/*******************************************************
 * load_settings
 *
 * load the default settings for the assistant
 *******************************************************/
static
void load_settings (CsvImportTrans *info)
{
    info->start_row = 0;
    info->account_page_step = TRUE;
    info->match_parse_run = FALSE;
    info->file_name = NULL;
    info->starting_dir = NULL;

    /* The default directory for the user to select files. */
    info->starting_dir = gnc_get_default_directory (GNC_PREFS_GROUP);
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
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    /* Enable the Assistant Buttons */
    gtk_assistant_set_page_complete (assistant, page, TRUE);
}


void
csv_import_trans_assistant_file_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvImportTrans *info = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    info->account_picker->auto_create = TRUE; /* Step over account page if we find matching online id */
    info->previewing_errors = FALSE;          /* We're looking at all the data. */
    info->approved = FALSE;                   /* This is FALSE until the user clicks "OK". */

    /* Set the default directory */
    if (info->starting_dir)
        gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER(info->file_chooser), info->starting_dir);

    /* Disable the Forward Assistant Button */
    gtk_assistant_set_page_complete (assistant, page, FALSE);
}


void
csv_import_trans_assistant_preview_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvImportTrans *info = user_data;
    GtkAdjustment *adj;

    g_signal_connect (G_OBJECT(info->treeview), "size-allocate",
                     G_CALLBACK(treeview_resized), (gpointer)info);

    if (info->previewing_errors == TRUE)
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

        /* Set spin buttons not sensative */
        gtk_widget_set_sensitive (info->start_row_spin, FALSE);
        gtk_widget_set_sensitive (info->end_row_spin, FALSE);
        gtk_widget_set_sensitive (info->skip_rows, FALSE);
        info->parse_data->skip_rows = FALSE;

        /* Set check button label */
        gtk_label_set_text (GTK_LABEL(info->check_label), _("Skip Errors"));
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(info->check_butt), FALSE);
    }

    /* Load the data into the treeview. */
    gnc_csv_preview_update_assist (info);

    /* Set the upper limit of spin button to number of rows */
    adj = gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON(info->end_row_spin));
    if (gtk_adjustment_get_upper (adj) != info->num_of_rows)
    {
        gtk_adjustment_set_upper (adj, info->num_of_rows);
        gtk_spin_button_set_value (GTK_SPIN_BUTTON(info->end_row_spin), info->num_of_rows);
    }

    /* Set start row */
    adj = gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON(info->start_row_spin));
    gtk_spin_button_set_value (GTK_SPIN_BUTTON(info->start_row_spin), 1);

    /* Update the row selection highlight */
    row_selection_update (info);
}


void
csv_import_trans_assistant_account_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvImportTrans *info = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);
    gchar *text, *mtext;
    Account * account = NULL;
    int settings_ret;

    settings_ret = preview_settings_valid(info);
    if ((settings_ret != 0) && (info->approved == FALSE))
    {
        text = g_strdup_printf(gettext ("There are problems with the import settings:\n"));
        if (settings_ret & 0x1) {
            mtext = g_strdup_printf("%s%s (\"%s\")\n", text, gettext("Date format is wrong!"), info->checked_date);
            free(text);
            text = mtext;
        }
        if (settings_ret & 0x2) {
            mtext = g_strdup_printf("%s%s", text, gettext("Too few columns selected!\n"));
            free(text);
            text = mtext;
        }
        mtext = g_strdup_printf ("<span size=\"medium\" color=\"red\"><b>%s</b></span>", text);
        gtk_label_set_markup (GTK_LABEL(info->account_label), mtext);
        g_free (mtext);
        g_free (text);

        gtk_widget_set_sensitive (info->account_page, FALSE);
    }
    else
    {
        text = g_strdup_printf (gettext ("To Change the account, double click on the required account, click Forward to proceed."));
        mtext = g_strdup_printf ("<span size=\"medium\" color=\"red\"><b>%s</b></span>", text);
        gtk_label_set_markup (GTK_LABEL(info->account_label), mtext);
        g_free (mtext);
        g_free (text);

        gtk_widget_set_sensitive (info->account_page, TRUE);

        /* Let the user select an account to put the transactions in. */
        account = gnc_import_account_assist_update (info->account_picker);

        /* If we have a valid account and auto_create is TRUE, move on to matcher */
        if (!(account == NULL) && (info->account_picker->auto_create == TRUE))
            gtk_assistant_set_current_page (assistant, num + 1);
    }

    g_free(info->checked_date);
    info->checked_date = NULL;

    /* Enable the Forward Assistant Button */
    if (account == NULL)
        gtk_assistant_set_page_complete (assistant, page, FALSE);
    else
        gtk_assistant_set_page_complete (assistant, page, TRUE);
}


void
csv_import_trans_assistant_doc_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvImportTrans *info = user_data;

    /* Block going back */
    gtk_assistant_commit (GTK_ASSISTANT(info->window));

    if ( info->match_parse_run == FALSE)
    {
        /* Add the Cancel button for the matcher */
        info->cancel_button = gtk_button_new_with_mnemonic (_("_Cancel"));
        gtk_assistant_add_action_widget (assistant, info->cancel_button);
        g_signal_connect (info->cancel_button, "clicked",
                         G_CALLBACK(csv_import_trans_assistant_cancel), info);
        gtk_widget_show (GTK_WIDGET(info->cancel_button));
    }
}


void
csv_import_trans_assistant_match_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvImportTrans *info = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);
    gchar *text, *mtext;

    /* Block going back */
    gtk_assistant_commit (GTK_ASSISTANT(info->window));

    /* Before creating transactions, if this is a new book, let user specify
     * book options, since they affect how transactions are created */
    if (info->new_book)
        info->new_book = gnc_new_book_option_display (info->window);

    /* Create transactions from the parsed data, first time with FALSE
       Subsequent times with TRUE */
    if ( info->match_parse_run == FALSE)
    {
        gnc_csv_parse_to_trans (info->parse_data, info->account_picker->retAccount, FALSE);
    }
    else
        gnc_csv_parse_to_trans (info->parse_data, info->account_picker->retAccount, TRUE);
    info->match_parse_run = TRUE;

    /* if there are errors, we jump back to preview to correct */
    if (!(info->parse_data->error_lines == NULL) && (info->approved == FALSE) )
    {
        info->previewing_errors = TRUE; /* We're looking at errors. */
        gtk_assistant_set_current_page (assistant, 2);
    }

    if ((info->parse_data->error_lines == NULL) || (info->approved == TRUE) )
    {
        GList* transactions; /* A list of the transactions we create */

        text =  _("Double click on rows to change, then click on Apply to Import");
        mtext = g_strdup_printf ("<span size=\"medium\" color=\"red\"><b>%s</b></span>", text);
        gtk_label_set_markup (GTK_LABEL(info->match_label), mtext);
        g_free (mtext);

        if (info->gnc_csv_importer_gui == NULL)
        {
            /* Create the genereic transaction importer GUI. */
            info->gnc_csv_importer_gui = gnc_gen_trans_assist_new (info->match_page, NULL, FALSE, 42);

            /* Add the help button for the matcher */
            info->help_button = gtk_button_new_with_mnemonic (_("_Help"));
            gtk_assistant_add_action_widget (assistant, info->help_button);
            g_signal_connect (info->help_button, "clicked",
                             G_CALLBACK(on_matcher_help_clicked), info->gnc_csv_importer_gui);
            gtk_widget_show (GTK_WIDGET(info->help_button));

            /* Get the list of the transactions that were created. */
            transactions = info->parse_data->transactions;
            /* Copy all of the transactions to the importer GUI. */
            while (transactions != NULL)
            {
                GncCsvTransLine* trans_line = transactions->data;
                gnc_gen_trans_list_add_trans (info->gnc_csv_importer_gui, trans_line->trans);
                transactions = g_list_next (transactions);
            }
            g_list_free (transactions);
        }
    }
    /* Enable the Forward Assistant Button */
    gtk_assistant_set_page_complete (assistant, page, TRUE);
}


void
csv_import_trans_assistant_summary_page_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    CsvImportTrans *info = user_data;
    gchar *text, *mtext;

    /* Save the Window size and directory */
    gnc_set_default_directory (GNC_PREFS_GROUP, info->starting_dir);

    /* Remove the added button */
    gtk_assistant_remove_action_widget (assistant, info->help_button);
    gtk_assistant_remove_action_widget (assistant, info->cancel_button);

    text = g_strdup_printf (gettext ("The transactions were imported from the file '%s'."), info->file_name);
    mtext = g_strdup_printf ("<span size=\"medium\"><b>%s</b></span>", text);
    gtk_label_set_markup (GTK_LABEL(info->summary_label), mtext);
    g_free (text);
    g_free (mtext);
}


void
csv_import_trans_assistant_prepare (GtkAssistant *assistant, GtkWidget *page,
                                    gpointer user_data)
{
    gint currentpage = gtk_assistant_get_current_page (assistant);

    switch (currentpage)
    {
    case 0:
        /* Current page is Import Start page */
        csv_import_trans_assistant_start_page_prepare (assistant, user_data);
        break;
    case 1:
        /* Current page is File select page */
        csv_import_trans_assistant_file_page_prepare (assistant, user_data);
        break;
    case 2:
        /* Current page is Preview page */
        csv_import_trans_assistant_preview_page_prepare (assistant, user_data);
        break;
    case 3:
        /* Current page is Account page */
        csv_import_trans_assistant_account_page_prepare (assistant, user_data);
        break;
    case 4:
        /* Current page is Transaction Doc page */
        csv_import_trans_assistant_doc_page_prepare (assistant, user_data);
        break;
    case 5:
        /* Current page is Match page */
        csv_import_trans_assistant_match_page_prepare (assistant, user_data);
        break;
    case 6:
        /* Current page is Summary page */
        csv_import_trans_assistant_summary_page_prepare (assistant, user_data);
        break;
    }
}


/*******************************************************
 * Assistant call back functions
 *******************************************************/
static void
csv_import_trans_assistant_destroy_cb (GtkObject *object, gpointer user_data)
{
    CsvImportTrans *info = user_data;
    gnc_unregister_gui_component_by_data (ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS, info);
    g_free (info);
}

void
csv_import_trans_assistant_cancel (GtkAssistant *assistant, gpointer user_data)
{
    CsvImportTrans *info = user_data;
    gnc_close_gui_component_by_data (ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS, info);
}

void
csv_import_trans_assistant_close (GtkAssistant *assistant, gpointer user_data)
{
    CsvImportTrans *info = user_data;
    gnc_close_gui_component_by_data (ASSISTANT_CSV_IMPORT_TRANS_CM_CLASS, info);
}

void
csv_import_trans_assistant_finish (GtkAssistant *assistant, gpointer user_data)
{
    CsvImportTrans *info = user_data;

    /* Start the import */
    if (info->parse_data->transactions != NULL)
        gnc_gen_trans_assist_start (info->gnc_csv_importer_gui);
    else
        gnc_gen_trans_list_delete (info->gnc_csv_importer_gui);
}

static void
csv_import_trans_close_handler (gpointer user_data)
{
    CsvImportTrans *info = user_data;

    g_free(info->file_name);
    g_free(info->starting_dir);

    /* Free the memory we allocated. */
    if (!(info->parse_data == NULL))
        gnc_csv_parse_data_free (info->parse_data);

    if (!(info->account_picker == NULL))
        info->account_picker = NULL;

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
    GtkWidget *button, *csv_button, *h_box;

    builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "start_row_adj");
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "end_row_adj");
    gnc_builder_add_from_file  (builder , "assistant-csv-trans-import.glade", "CSV Transaction Assistant");
    window = GTK_WIDGET(gtk_builder_get_object (builder, "CSV Transaction Assistant"));
    info->window = window;

    /* Set the assistant colors */
    gnc_assistant_set_colors (GTK_ASSISTANT (info->window));

    /* Load default settings */
    load_settings (info);

    /* Enable buttons on all page. */
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object (builder, "start_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object (builder, "file_page")),
                                     FALSE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object (builder, "preview_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT(window),
                                     GTK_WIDGET(gtk_builder_get_object (builder, "account_page")),
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

    /* File chooser Page */
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

    box = GTK_WIDGET(gtk_builder_get_object(builder, "file_page"));
    gtk_box_pack_start (GTK_BOX(box), info->file_chooser, TRUE, TRUE, 6);
    gtk_widget_show (info->file_chooser);

    /* Preview Settings Page */
    {
        char* sep_button_names[] = {"space_cbutton",
                                    "tab_cbutton",
                                    "comma_cbutton",
                                    "colon_cbutton",
                                    "semicolon_cbutton",
                                    "hyphen_cbutton"
                                   };
        GtkContainer *date_format_container, *currency_format_container;
        int i;

        /* The table containing info->encselector and the separator configuration widgets */
        GtkTable* enctable;

        info->start_row_spin = GTK_WIDGET(gtk_builder_get_object (builder, "start_row"));
        info->end_row_spin = GTK_WIDGET(gtk_builder_get_object (builder, "end_row"));
        info->skip_rows = GTK_WIDGET(gtk_builder_get_object (builder, "skip_rows"));
        info->check_label = GTK_WIDGET(gtk_builder_get_object (builder, "check_label"));
        info->check_butt = GTK_WIDGET(gtk_builder_get_object (builder, "check_butt"));

        info->encselector = GO_CHARMAP_SEL(go_charmap_sel_new(GO_CHARMAP_SEL_TO_UTF8));
        /* Connect the selector to the encoding_selected event handler. */
        g_signal_connect (G_OBJECT(info->encselector), "charmap_changed",
                         G_CALLBACK(encoding_selected), (gpointer)info);

        /* Load the separator buttons from the glade builder file into the
         * info->sep_buttons array. */
        for (i = 0; i < SEP_NUM_OF_TYPES; i++)
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

        /* Get the table from the Glade builder file. */
        enctable = GTK_TABLE(gtk_builder_get_object (builder, "enctable"));
        /* Put the selector in at the top. */
        gtk_table_attach_defaults (enctable, GTK_WIDGET(info->encselector), 1, 2, 0, 1);
        /* Show the table in all its glory. */
        gtk_widget_show_all (GTK_WIDGET(enctable));

        /* The instructions label and image */
        info->instructions_label = GTK_LABEL(gtk_builder_get_object (builder, "instructions_label"));
        info->instructions_image = GTK_IMAGE(gtk_builder_get_object (builder, "instructions_image"));

        /* Add in the date format combo box and hook it up to an event handler. */
        info->date_format_combo = GTK_COMBO_BOX_TEXT(gtk_combo_box_text_new());
        for (i = 0; i < num_date_formats; i++)
        {
            gtk_combo_box_text_append_text (info->date_format_combo, _(date_format_user[i]));
        }
        gtk_combo_box_set_active (GTK_COMBO_BOX(info->date_format_combo), 0);
        g_signal_connect (G_OBJECT(info->date_format_combo), "changed",
                         G_CALLBACK(date_format_selected), (gpointer)info);

        /* Add it to the assistant. */
        date_format_container = GTK_CONTAINER(gtk_builder_get_object (builder, "date_format_container"));
        gtk_container_add (date_format_container, GTK_WIDGET(info->date_format_combo));
        gtk_widget_show_all (GTK_WIDGET(date_format_container));

        /* Add in the currency format combo box and hook it up to an event handler. */
        info->currency_format_combo = GTK_COMBO_BOX_TEXT(gtk_combo_box_text_new());
        for (i = 0; i < num_currency_formats; i++)
        {
            gtk_combo_box_text_append_text (info->currency_format_combo, _(currency_format_user[i]));
        }
        /* Default will the locale */
        gtk_combo_box_set_active (GTK_COMBO_BOX(info->currency_format_combo), 0);
        g_signal_connect (G_OBJECT(info->currency_format_combo), "changed",
                         G_CALLBACK(currency_format_selected), (gpointer)info);

        /* Add it to the assistant. */
        currency_format_container = GTK_CONTAINER(gtk_builder_get_object (builder, "currency_format_container"));
        gtk_container_add (currency_format_container, GTK_WIDGET(info->currency_format_combo));
        gtk_widget_show_all (GTK_WIDGET(currency_format_container));

        /* Connect the CSV/Fixed-Width radio button event handler. */
        csv_button = GTK_WIDGET(gtk_builder_get_object (builder, "csv_button"));
        g_signal_connect (csv_button, "toggled",
                         G_CALLBACK(separated_or_fixed_selected), (gpointer)info);

        /* Load the data treeview and connect it to its resizing event handler. */
        info->treeview = (GtkTreeView*)GTK_WIDGET(gtk_builder_get_object (builder, "treeview"));

        /* Load the column type treeview. */
        info->ctreeview = (GtkTreeView*)GTK_WIDGET(gtk_builder_get_object (builder, "ctreeview"));

        /* This is TRUE only after encoding_selected is called, so we must
         * set it initially to FALSE. */
        info->encoding_selected_called = FALSE;

        /* It is empty at first. */
        info->not_empty = FALSE;
    }

    /* Account page */
    /* Initialise the Account Picker and add to the Assistant */
    info->account_page  = GTK_WIDGET(gtk_builder_get_object (builder, "account_page"));
    info->account_picker = gnc_import_account_assist_setup (info->account_page);
    info->account_label = GTK_WIDGET(gtk_builder_get_object (builder, "account_label"));

    /* Matcher page */
    info->match_page  = GTK_WIDGET(gtk_builder_get_object (builder, "match_page"));
    info->match_label = GTK_WIDGET(gtk_builder_get_object (builder, "match_label"));

    /* Summary Page */
    info->summary_label = GTK_WIDGET(gtk_builder_get_object(builder, "summary_label"));

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
