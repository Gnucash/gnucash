/**********************************************************************
 * gnc-plugin-page-register-sort.c -- register page sort              *
 *                                                                    *
 * Copyright (C) 2026, Robert Fewell                                  *
 *                                                                    *
 * This program is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU General Public License as     *
 * published by the Free Software Foundation; either version 2 of     *
 * the License, or (at your option) any later version.                *
 *                                                                    *
 * This program is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
 * GNU General Public License for more details.                       *
 *                                                                    *
 * You should have received a copy of the GNU General Public License  *
 * along with this program; if not, contact:                          *
 *                                                                    *
 * Free Software Foundation           Voice:  +1-617-542-5942         *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652         *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                     *
 **********************************************************************/

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup RegisterPlugin Register Page Sort
    @{ */
/** @file gnc-plugin-page-register-sort.c
    @brief  Functions providing a register page sort for the GnuCash UI
    @author Copyright (C) 2026 Bob Fewell
*/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-plugin-page-register.h"
#include "gnc-plugin-page-register-sort.hpp"
#include "dialog-utils.h"
#include "gnc-state.h"
#include "gnc-prefs.h"
#include "gnc-ui-util.h"
#include "gnc-window.h"
#include "gnc-main-window.h"
#include "engine-helpers.h"
#include "qofbookslots.h"
#include "qof.h"

#include <algorithm>
#include <string>

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static std::string DEFAULT_SORT_ORDER = "BY_STANDARD";

struct RegisterSortDialog
{
    GncPluginPage* plugin_page;
    SplitRegister* reg;
    GtkWidget*     dialog;
    GtkWidget*     num_radio;
    GtkWidget*     act_radio;

    SortType       original_sort_type;
    bool           original_reverse_order;
    bool           original_save_order;

    bool           show_save_button;
};

extern "C"
{
// These functions are the dialog callbacks. They're connected to their
// signals in gnc-plugin-page-register.glade so they mustn't be name-mangled.
void
gnc_ppr_sort_response_cb (GtkDialog* dialog,
                          gint response,
                          RegisterSortDialog *rsd);

void
gnc_ppr_sort_button_cb (GtkToggleButton* button,
                        RegisterSortDialog *rsd);

void
gnc_ppr_sort_order_save_cb (GtkToggleButton* button,
                            RegisterSortDialog *rsd);

void
gnc_ppr_sort_order_reverse_cb (GtkToggleButton* button,
                               RegisterSortDialog *rsd);
}

static inline bool
gboolean_to_bool (gboolean value)
{
    return value ? true : false;
}

static inline gboolean
bool_to_gboolean (bool value)
{
    return value ? TRUE : FALSE;
}

static void
gnc_ppr_check_for_empty_group (GKeyFile *state_file,
                               const gchar *state_section)
{
    gsize num_keys;
    gchar **keys = g_key_file_get_keys (state_file, state_section, &num_keys, nullptr);

    if (num_keys == 0)
        gnc_state_drop_sections_for (state_section);

    g_strfreev (keys);
}

static std::string
gnc_ppr_sort_get_order (GNCSplitReg *gsr)
 {
    if (!gsr)
        return _("unknown");

    // get the sort_order from the .gcm file
    GKeyFile* state_file = gnc_state_get_current();
    auto state_section = gsr_get_register_state_section (gsr);
    GError* error = nullptr;

    auto sort_text = g_key_file_get_string (state_file, state_section,
                                            KEY_PAGE_SORT, &error);
    std::string sort_order;

    if (error)
        g_clear_error (&error);
    else
    {
        sort_order = (sort_text);
        g_free (sort_text);
    }
    g_free (state_section);

    return !sort_order.empty() ? sort_order : (DEFAULT_SORT_ORDER);
}

static void
gnc_ppr_sort_set_order (GNCSplitReg *gsr, std::string sort_order)
{
    if (!gsr)
        return;

    // save sort_order to the .gcm file also
    GKeyFile* state_file = gnc_state_get_current();
    auto state_section = gsr_get_register_state_section (gsr);

    if (sort_order.compare (DEFAULT_SORT_ORDER) == 0)
    {
        if (g_key_file_has_key (state_file, state_section, KEY_PAGE_SORT, nullptr))
            g_key_file_remove_key (state_file, state_section, KEY_PAGE_SORT, nullptr);

        gnc_ppr_check_for_empty_group (state_file, state_section);
    }
    else
        g_key_file_set_string (state_file, state_section, KEY_PAGE_SORT, sort_order.c_str());

    g_free (state_section);
}

static bool
gnc_ppr_sort_get_reversed (GNCSplitReg *gsr)
{
    if (!gsr)
        return false;

    // get the sort_reversed from the .gcm file
    GKeyFile* state_file = gnc_state_get_current();
    auto state_section = gsr_get_register_state_section (gsr);
    GError* error = nullptr;
    gboolean sort_reversed = g_key_file_get_boolean (state_file, state_section,
                                                     KEY_PAGE_SORT_REV, &error);

    if (error)
        g_clear_error (&error);

    g_free (state_section);
    return gboolean_to_bool (sort_reversed);
}

static void
gnc_ppr_sort_set_reversed (GNCSplitReg* gsr, bool reverse_order)
{
    if (!gsr)
        return;

    // save reverse_order to the .gcm file also
    GKeyFile* state_file = gnc_state_get_current();
    auto state_section = gsr_get_register_state_section (gsr);

    if (!reverse_order)
    {
        if (g_key_file_has_key (state_file, state_section, KEY_PAGE_SORT_REV, nullptr))
            g_key_file_remove_key (state_file, state_section, KEY_PAGE_SORT_REV, nullptr);

        gnc_ppr_check_for_empty_group (state_file, state_section);
    }
    else
        g_key_file_set_boolean (state_file, state_section, KEY_PAGE_SORT_REV,
                                bool_to_gboolean (reverse_order));

    g_free (state_section);
}

/** This function is called to update the register.
 *
 *  @param page A pointer to the GncPluginPageRegister that is
 *  associated with this sort order dialog.
 */
void
gnc_ppr_sort_update_register (GncPluginPage* plugin_page)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(plugin_page));

    auto sd = gnc_plugin_page_register_get_sort_data (plugin_page);
    auto gsr = gnc_plugin_page_register_get_gsr (plugin_page);
    GNCLedgerDisplayType ledger_type = gnc_ledger_display_type (gsr->ledger);

    sd->save_order = false;

    // Set the sort direction for the split register and status of save order button
    sd->reverse_order = gnc_ppr_sort_get_reversed (gsr);

    PINFO("Loaded Sort reversed order is %s", sd->reverse_order ? "true" : "false");

    gnc_split_reg_set_sort_reversed (gsr, sd->reverse_order, no_refresh);
    if (sd->reverse_order)
        sd->save_order = true;

    // Set the sort order for the split register and status of save order button
    std::string sort_type = gnc_ppr_sort_get_order (gsr);

    PINFO("Loaded Sort type is %s", sort_type.c_str());

    SortType type = SortTypefromString (sort_type.c_str());

    gnc_split_reg_sort (gsr, type, no_force, no_refresh);

    if (sort_type.compare (DEFAULT_SORT_ORDER) != 0)
        sd->save_order = true;

    if (ledger_type == LD_GL)
    {
        auto reg = gnc_ledger_display_get_split_register (gsr->ledger);

        if (reg->type != GENERAL_JOURNAL) // search ledger and the like
        {
            gnc_split_reg_sort (gsr, SortTypefromString (DEFAULT_SORT_ORDER.c_str()), no_force, no_refresh);
            sd->reverse_order = false;
            sd->save_order = false;
        }
    }
}

/** This function is called whenever the number source book options is changed
 *  to adjust the displayed labels. Since the book option change may change the
 *  query sort, the gnc_split_reg_sort function is called with force and refresh
 *  to ensure the page is refreshed.
 *
 *  @param new_val A pointer to the boolean for the new value of the book option.
 *
 *  @param user_data A pointer to the sort dialog structure.
 */
static void
gnc_ppr_sort_book_option_changed (gpointer new_val,
                                  gpointer user_data)
{
    RegisterSortDialog *rsd = (RegisterSortDialog*)user_data;
    gboolean* new_data = (gboolean*)new_val;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rsd->plugin_page));

    if (*new_data)
    {
        gtk_button_set_label (GTK_BUTTON(rsd->num_radio), _("Transaction Number"));
        gtk_button_set_label (GTK_BUTTON(rsd->act_radio), _("Number/Action"));
    }
    else
    {
        gtk_button_set_label (GTK_BUTTON(rsd->num_radio), _("Number"));
        gtk_button_set_label (GTK_BUTTON(rsd->act_radio), _("Action"));
    }

    auto gsr = gnc_plugin_page_register_get_gsr (rsd->plugin_page);

    gnc_split_reg_sort (gsr, (SortType)gsr->sort_type, force, refresh);
}

/** This function is called when the "Sort By…" dialog is closed.
 *  If the dialog was closed by any method other than clicking the OK
 *  button, the original sorting order will be restored.
 *
 *  @param dialog A pointer to the dialog box.
 *
 *  @param response A numerical value indicating why the dialog box was closed.
 *
 *  @param rsd A pointer to the sort dialog structure.
 */
void
gnc_ppr_sort_response_cb (GtkDialog* dialog,
                          gint response,
                          RegisterSortDialog *rsd)
{
    g_return_if_fail (GTK_IS_DIALOG(dialog));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rsd->plugin_page));

    ENTER(" ");

    auto sd = gnc_plugin_page_register_get_sort_data (rsd->plugin_page);
    auto gsr = gnc_plugin_page_register_get_gsr (rsd->plugin_page);

    if (response != GTK_RESPONSE_OK)
    {
        // Restore the original sort order
        gnc_split_reg_set_sort_reversed (gsr, rsd->original_reverse_order, no_refresh);
        sd->reverse_order = rsd->original_reverse_order;
        // use force as sort_type may still be the same if only reverse_order changed
        gnc_split_reg_sort (gsr, rsd->original_sort_type, force, refresh);
        sd->sort_type = rsd->original_sort_type;
        sd->save_order = rsd->original_save_order;
    }
    else
    {
        // clear the sort when unticking the save option
        if ((!sd->save_order) && ((rsd->original_sort_type) ||
                                  (rsd->original_reverse_order)))
        {
            gnc_ppr_sort_set_order (gsr, DEFAULT_SORT_ORDER);
            gnc_ppr_sort_set_reversed (gsr, false);
        }
        rsd->original_sort_type = sd->sort_type;
        rsd->original_reverse_order = sd->reverse_order;

        if (sd->save_order)
        {
            SortType type = gnc_split_reg_get_sort_type (gsr);
            std::string sort_type = (SortTypeasString (type));

            gnc_ppr_sort_set_order (gsr, sort_type);
            gnc_ppr_sort_set_reversed (gsr, sd->reverse_order);
        }
    }
    gnc_book_option_remove_cb (OPTION_NAME_NUM_FIELD_SOURCE,
                               gnc_ppr_sort_book_option_changed,
                               (gpointer)rsd);
    rsd->dialog = nullptr;
    rsd->num_radio = nullptr;
    rsd->act_radio = nullptr;
    sd->dialog = nullptr;
    g_free (rsd);
    gtk_widget_destroy (GTK_WIDGET(dialog));
    LEAVE (" ");
}

/** This function is called when a radio button in the "Sort By…"
 *  dialog is clicked.
 *
 *  @param button The button that was toggled.
 *
 *  @param rsd A pointer to the sort dialog structure.
 */
void
gnc_ppr_sort_button_cb (GtkToggleButton* button,
                        RegisterSortDialog *rsd)
{
    g_return_if_fail (GTK_IS_TOGGLE_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rsd->plugin_page));

    auto name = gtk_buildable_get_name (GTK_BUILDABLE(button));

    ENTER("button %s(%p), page %p", name, button, rsd->plugin_page);

    if (!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(button)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }

    auto gsr = gnc_plugin_page_register_get_gsr (rsd->plugin_page);

    SortType type = SortTypefromString (name);
    gnc_split_reg_sort (gsr, type, no_force, refresh);
    LEAVE (" ");
}

/** This function is called whenever the save sort order is checked
 *  or unchecked which allows saving of the sort order.
 *
 *  @param button The toggle button that was changed.
 *
 *  @param rsd A pointer to the sort dialog structure.
 */
void
gnc_ppr_sort_order_save_cb (GtkToggleButton* button,
                            RegisterSortDialog *rsd)
{
    g_return_if_fail (GTK_IS_CHECK_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rsd->plugin_page));

    ENTER("Save toggle button (%p), page %p", button, rsd->plugin_page);

    /* Compute the new save sort order */
    auto sd = gnc_plugin_page_register_get_sort_data (rsd->plugin_page);

    if (gtk_toggle_button_get_active (button))
        sd->save_order = true;
    else
        sd->save_order = false;
    LEAVE (" ");
}

/** This function is called whenever the reverse sort order is checked
 *  or unchecked which allows reversing of the sort order.
 *
 *  @param button The toggle button that was changed.
 *
 *  @param rsd A pointer to the sort dialog structure.
 */
void
gnc_ppr_sort_order_reverse_cb (GtkToggleButton* button,
                               RegisterSortDialog *rsd)
{
    g_return_if_fail (GTK_IS_CHECK_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rsd->plugin_page));

    ENTER("Reverse toggle button (%p), page %p", button, rsd->plugin_page);

    /* Compute the new save sort order */
    auto sd = gnc_plugin_page_register_get_sort_data (rsd->plugin_page);
    auto gsr = gnc_plugin_page_register_get_gsr (rsd->plugin_page);

    sd->reverse_order = gtk_toggle_button_get_active (button);
    gnc_split_reg_set_sort_reversed (gsr, sd->reverse_order, refresh);
    LEAVE (" ");
}

/** This function is called to create the sort dialog.
 *
 *  @param rsd A pointer to the sort dialog structure.
 * 
 *  @param sd The sort data structure for remembering state.
 */
static void
gnc_ppr_sort_dialog_create (RegisterSortDialog *rsd, SortData *sd)
{
    /* Create the dialog */
    auto builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "gnc-plugin-page-register.glade", "sort_by_dialog");
    auto dialog = GTK_WIDGET(gtk_builder_get_object (builder, "sort_by_dialog"));
    rsd->dialog = dialog;
    sd->dialog = rsd->dialog;

    gtk_window_set_transient_for (GTK_WINDOW(dialog),
                                  gnc_window_get_gtk_window (GNC_WINDOW(
                                      GNC_PLUGIN_PAGE(rsd->plugin_page)->window)));
    /* Translators: The %s is the name of the plugin page */
    auto title = g_strdup_printf (_("Sort %s by…"),
                           gnc_plugin_page_get_page_name (rsd->plugin_page));
    gtk_window_set_title (GTK_WINDOW(dialog), title);
    g_free (title);

    auto gsr = gnc_plugin_page_register_get_gsr (rsd->plugin_page);

    /* Set the button for the current sort order */
    SortType sort = gnc_split_reg_get_sort_type (gsr);
    auto name = SortTypeasString (sort);
    auto button = GTK_WIDGET(gtk_builder_get_object (builder, name));
    DEBUG("current sort %d, button %s(%p)", sort, name, button);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button), TRUE);
    rsd->original_sort_type = sort;

    button = GTK_WIDGET(gtk_builder_get_object (builder, "sort_save"));
    if (sd->save_order)
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button), TRUE);

    rsd->original_save_order = sd->save_order;

    // hide the save button if appropriate
    gtk_widget_set_visible (GTK_WIDGET(button), bool_to_gboolean (rsd->show_save_button));

    /* Set the button for the current reverse_order order */
    button = GTK_WIDGET(gtk_builder_get_object (builder, "sort_reverse"));
    if (sd->reverse_order)
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button), TRUE);
    rsd->original_reverse_order = sd->reverse_order;

    rsd->num_radio = GTK_WIDGET(gtk_builder_get_object (builder, "BY_NUM"));
    rsd->act_radio = GTK_WIDGET(gtk_builder_get_object (builder, "BY_ACTION"));
    /* Adjust labels related to Num/Action radio buttons based on book option */
    if (rsd->reg && !rsd->reg->use_tran_num_for_num_field)
    {
        gtk_button_set_label (GTK_BUTTON(rsd->num_radio), _ ("Transaction Number"));
        gtk_button_set_label (GTK_BUTTON(rsd->act_radio), _ ("Number/Action"));
    }
    gnc_book_option_register_cb (OPTION_NAME_NUM_FIELD_SOURCE,
                                 (GncBOCb)gnc_ppr_sort_book_option_changed,
                                 (gpointer)rsd);

    /* Wire it up */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func,
                                      rsd);

    /* Show it */
    gtk_widget_show (dialog);
    g_object_unref (G_OBJECT(builder));
    LEAVE (" ");
}

/** This function is called for the sort dialog.
 *
 *  @param plugin_page  A pointer to the GncPluginPageRegister that is
 *  associated with this sort dialog.
 * 
 *  @param reg A pointer to the SplitRegister of the current register.
 * 
 *  @param fd A pointer to the sort data structure for remembering state.
 *
 *  @param show_save_button Set to True to show save button.
 */
void
gnc_ppr_sort_dialog (GncPluginPage *plugin_page, SplitRegister* reg,
                     SortData *sd, bool show_save_button)
{
    RegisterSortDialog *rsd;

    ENTER(" ");

    rsd = g_new0 (RegisterSortDialog, 1);

    rsd->plugin_page = plugin_page;
    rsd->reg = reg;
    rsd->show_save_button = show_save_button;

    gnc_ppr_sort_dialog_create (rsd, sd);

    LEAVE(" ");
}
