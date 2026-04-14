/**********************************************************************
 * gnc-plugin-page-register-filter.cpp -- register page filter        *
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
/** @addtogroup RegisterPlugin Register Page Filter
    @{ */
/** @file gnc-plugin-page-register-filter.cpp
    @brief  Functions providing a register page filter for the GnuCash UI
    @author Copyright (C) 2026 Bob Fewell
*/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "dialog-utils.h"
#include "gnc-date.h"
#include "gnc-date-edit.h"
#include "gnc-glib-utils.h"
#include "gnc-state.h"
#include "gnc-prefs.h"
#include "gnc-ui-util.h"
#include "gnc-window.h"
#include "gnc-main-window.h"
#include "engine-helpers.h"
#include "qofbookslots.h"
#include "qof.h"
#include "Query.h"

#include <algorithm>
#include <cstdio>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "gnc-plugin-page-register.h"
#include "gnc-plugin-page-register-filter.hpp"

static std::string DEFAULT_FILTER_NUM_DAYS_GL = "30";
static std::string DEFAULT_FILTER = "0x001f";

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

extern "C"
{
// These functions are the dialog callbacks. They're connected to their
// signals in gnc-plugin-page-register.glade so they mustn't be name-mangled.
void
gnc_ppr_filter_select_range_cb (GtkRadioButton* button,
                                GncPluginPageRegister* page);
void
gnc_ppr_filter_start_cb (GtkWidget* radio,
                         GncPluginPageRegister* page);
void
gnc_ppr_filter_end_cb (GtkWidget* radio,
                       GncPluginPageRegister* page);
void
gnc_ppr_filter_response_cb (GtkDialog* dialog,
                            gint response,
                            GncPluginPageRegister* page);
void
gnc_ppr_filter_status_select_all_cb (GtkButton* button,
                                     GncPluginPageRegister* page);
void
gnc_ppr_filter_status_clear_all_cb (GtkButton* button,
                                    GncPluginPageRegister* page);
void
gnc_ppr_filter_status_one_cb (GtkToggleButton* button,
                              GncPluginPageRegister* page);
void
gnc_ppr_filter_save_cb (GtkToggleButton* button,
                        GncPluginPageRegister* page);
void
gnc_ppr_filter_days_changed_cb (GtkSpinButton* button,
                                GncPluginPageRegister* page);
}

struct status_action
{
    const char* action_name;
    int value;
    GtkWidget* widget;
};

static struct status_action status_actions[] =
{
    { "filter_status_reconciled",   CLEARED_RECONCILED, nullptr },
    { "filter_status_cleared",      CLEARED_CLEARED, nullptr },
    { "filter_status_voided",       CLEARED_VOIDED, nullptr },
    { "filter_status_frozen",       CLEARED_FROZEN, nullptr },
    { "filter_status_unreconciled", CLEARED_NO, nullptr },
    { nullptr, 0, nullptr },
};
#ifdef skip
static inline bool
gboolean_to_bool (gboolean value)
{
    return value ? true : false;
}
#endif
static inline gboolean
bool_to_gboolean (bool value)
{
    return value ? TRUE : FALSE;
}

static std::string
get_filter_default_num_of_days (GNCLedgerDisplayType ledger_type)
{
    if (ledger_type == LD_GL)
        return DEFAULT_FILTER_NUM_DAYS_GL;
    else
        return "0";
}

/* This function converts a time64 value date to a string */
static std::string
gnc_ppr_filter_time2dmy (time64 raw_time)
{
    struct tm* timeinfo;
    char date_string[11];

    timeinfo = gnc_localtime (&raw_time);
    strftime (date_string, 11, "%d-%m-%Y", timeinfo);
    PINFO("Date to string is %s", date_string);
    gnc_tm_free (timeinfo);

    return (date_string);
}

/* This function converts a string date to a time64 value */
static time64
gnc_ppr_filter_dmy2time (std::string date_string)
{
    struct tm when;

    PINFO("Date from string is %s", date_string.c_str());
    memset (&when, 0, sizeof (when));

    std::sscanf (date_string.c_str(), "%d-%d-%d", &when.tm_mday,
                 &when.tm_mon, &when.tm_year);

    when.tm_mon -= 1;
    when.tm_year -= 1900;

    return gnc_mktime (&when);
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
gnc_ppr_filter_get_filter (GNCSplitReg *gsr, GNCLedgerDisplayType ledger_type)
{
    if (!gsr)
        return _("unknown");

    // get the filter from the .gcm file
    GKeyFile* state_file = gnc_state_get_current();
    auto state_section = gsr_get_register_state_section (gsr);
    GError* error = nullptr;

    auto filter = g_key_file_get_string (state_file, state_section,
                                         KEY_PAGE_FILTER, &error);
    std::string filter_str;

    if (error)
        g_clear_error (&error);
    else
        filter_str = std::string (filter);

    g_free (filter);
    g_free (state_section);

    if (!filter_str.empty())
        return filter_str;

    return DEFAULT_FILTER + ";0;0;" + get_filter_default_num_of_days (ledger_type);
}

static void
gnc_ppr_filter_set_filter (GNCSplitReg *gsr, std::string filter)
{
    if (!gsr)
        return;

    GNCLedgerDisplayType ledger_type = gnc_ledger_display_type (gsr->ledger);

    std::string default_filter_str = DEFAULT_FILTER + ";0;0;" +
                                     get_filter_default_num_of_days (ledger_type);

    // save the filter to the .gcm file also
    GKeyFile* state_file = gnc_state_get_current();
    auto state_section = gsr_get_register_state_section (gsr);

    if (filter.empty() || (filter.compare (default_filter_str) == 0))
    {
        if (g_key_file_has_key (state_file, state_section, KEY_PAGE_FILTER, nullptr))
            g_key_file_remove_key (state_file, state_section, KEY_PAGE_FILTER, nullptr);

        gnc_ppr_check_for_empty_group (state_file, state_section);
    }
    else
    {
        g_key_file_set_string (state_file, state_section, KEY_PAGE_FILTER,
                               filter.c_str());

    }
    g_free (state_section);
}

static void
gpp_update_match_filter_text (cleared_match_t match, const guint mask,
                              const gchar* filter_name, GList **show, GList **hide)
{
    if ((match & mask) == mask)
        *show = g_list_prepend (*show, g_strdup (filter_name));
    else
        *hide = g_list_prepend (*hide, g_strdup (filter_name));
}

void
gnc_ppr_filter_set_tooltip (GncPluginPage* plugin_page, FilterData *fd)
{
    GList *t_list = nullptr;

    ENTER(" ");

    auto gsr = gnc_plugin_page_register_get_gsr (plugin_page);

    // filtered start time
    if (fd->start_time != 0)
    {
        auto sdate = qof_print_date (fd->start_time);
        t_list = g_list_prepend
            (t_list, g_strdup_printf ("%s %s", _("Start Date:"), sdate));
        g_free (sdate);
    }

    // filtered number of days
    if (fd->days > 0)
    {
        t_list = g_list_prepend
            (t_list, g_strdup_printf ("%s %d", _("Show previous number of days:"),
                                      fd->days));
    }
    // filtered end time
    if (fd->end_time != 0)
    {
        auto edate = qof_print_date (fd->end_time);
        t_list = g_list_prepend
            (t_list, g_strdup_printf ("%s %s", _("End Date:"), edate));
        g_free (edate);
    }

    // filtered match items
    if (fd->cleared_match != CLEARED_ALL)
    {
        GList *show = nullptr;
        GList *hide = nullptr;

        gpp_update_match_filter_text (fd->cleared_match, 0x01, _("Unreconciled"),
                                      &show, &hide);
        gpp_update_match_filter_text (fd->cleared_match, 0x02, _("Cleared"),
                                      &show, &hide);
        gpp_update_match_filter_text (fd->cleared_match, 0x04, _("Reconciled"),
                                      &show, &hide);
        gpp_update_match_filter_text (fd->cleared_match, 0x08, _("Frozen"),
                                      &show, &hide);
        gpp_update_match_filter_text (fd->cleared_match, 0x10, _("Voided"),
                                      &show, &hide);

        show = g_list_reverse (show);
        hide = g_list_reverse (hide);

        if (show)
        {
            auto str = gnc_list_formatter (show);
            t_list = g_list_prepend
                (t_list, g_strdup_printf ("%s %s", _("Show:"), str));
            g_free (str);
        }

        if (hide)
        {
            auto str = gnc_list_formatter (hide);
            t_list = g_list_prepend
                (t_list, g_strdup_printf ("%s %s", _("Hide:"), str));
            g_free (str);
        }

        g_list_free_full (show, g_free);
        g_list_free_full (hide, g_free);
    }

    t_list = g_list_reverse (t_list);

    if (t_list)
        t_list = g_list_prepend (t_list, g_strdup (_("Filter By:")));

    // free the existing text if present
    if (gsr->filter_text)
        g_free (gsr->filter_text);

    // set the tooltip text variable in the gsr
    gsr->filter_text = gnc_g_list_stringjoin (t_list, "\n");

    g_list_free_full (t_list, g_free);

    LEAVE(" ");
}

/** This function updates the "cleared match" term of the register
 *  query.  It unconditionally removes any old "cleared match" query
 *  term, then adds back a new query term if needed.  There seems to
 *  be a bug in the current g2 register code such that when the number
 *  of entries in the register doesn't fill up the window, the blank
 *  space at the end of the window isn't correctly redrawn.  This
 *  function works around that problem, but a root cause analysis
 *  should probably be done.
 *
 *  @param plugin_page A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 */
static void
gnc_ppr_filter_update_status_query (GncPluginPage* plugin_page)
{
    ENTER(" ");

    auto gsr = gnc_plugin_page_register_get_gsr (plugin_page);
    if (!gsr->ledger)
    {
        LEAVE("no ledger");
        return;
    }

    // check if this a search register and save query
    gnc_plugin_page_register_update_for_search_query (GNC_PLUGIN_PAGE_REGISTER(plugin_page));

    auto query = gnc_plugin_page_register_get_query (plugin_page);
    if (!query)
    {
        LEAVE("no query found");
        return;
    }

    auto fd = gnc_plugin_page_register_get_filter_data (plugin_page);
    auto reg = gnc_ledger_display_get_split_register (gsr->ledger);

    /* Remove the old status match */
    if (reg->type != SEARCH_LEDGER)
    {
        GSList *param_list = qof_query_build_param_list (SPLIT_RECONCILE, nullptr);
        qof_query_purge_terms (query, param_list);
        g_slist_free (param_list);
    }

    /* Install the new status match */
    if (fd->cleared_match != CLEARED_ALL)
        xaccQueryAddClearedMatch (query, fd->cleared_match, QOF_QUERY_AND);

    // Set filter tooltip for summary bar
    gnc_ppr_filter_set_tooltip (plugin_page, fd);

    gnc_plugin_page_register_query_update (GNC_PLUGIN_PAGE_REGISTER(plugin_page), query);
    LEAVE (" ");
}

/** This function updates the "date posted" term of the register
 *  query.  It unconditionally removes any old "date posted" query
 *  term, then adds back a new query term if needed.  There seems to
 *  be a bug in the current g2 register code such that when the number
 *  of entries in the register doesn't fill up the window, the blank
 *  space at the end of the window isn't correctly redrawn.  This
 *  function works around that problem, but a root cause analysis
 *  should probably be done.
 *
 *  @param plugin_page A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 */
static void
gnc_ppr_filter_update_date_query (GncPluginPage* plugin_page)
{
    ENTER(" ");

    auto gsr = gnc_plugin_page_register_get_gsr (plugin_page);
    if (!gsr->ledger)
    {
        LEAVE("no ledger");
        return;
    }

    // check if this a search register and save query
    gnc_plugin_page_register_update_for_search_query (GNC_PLUGIN_PAGE_REGISTER(plugin_page));

    auto query = gnc_plugin_page_register_get_query (plugin_page);
    if (!query)
    {
        LEAVE("no query found");
        return;
    }

    auto fd = gnc_plugin_page_register_get_filter_data (plugin_page);
    auto reg = gnc_ledger_display_get_split_register (gsr->ledger);

    /* Delete any existing old date spec. */
    if (reg->type != SEARCH_LEDGER)
    {
        GSList *param_list = qof_query_build_param_list (SPLIT_TRANS,
                                                         TRANS_DATE_POSTED, nullptr);
        qof_query_purge_terms (query, param_list);
        g_slist_free (param_list);
    }

    if (fd->start_time || fd->end_time)
    {
        /* Build a new spec */
        xaccQueryAddDateMatchTT (query,
                                 fd->start_time != 0, fd->start_time,
                                 fd->end_time != 0, fd->end_time,
                                 QOF_QUERY_AND);
    }

    if (fd->days > 0)
    {
        time64 start;
        struct tm tm;

        gnc_tm_get_today_start (&tm);

        tm.tm_mday = tm.tm_mday - fd->days;
        start = gnc_mktime (&tm);
        xaccQueryAddDateMatchTT (query, TRUE, start, FALSE, 0, QOF_QUERY_AND);
    }

    // Set filter tooltip for summary bar
    gnc_ppr_filter_set_tooltip (plugin_page, fd);

    gnc_plugin_page_register_query_update (GNC_PLUGIN_PAGE_REGISTER(plugin_page), query);
    LEAVE(" ");
}

void
gnc_ppr_filter_clear_current_filter (GncPluginPage* plugin_page)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(plugin_page));

    auto fd = gnc_plugin_page_register_get_filter_data (plugin_page);

    fd->days = 0;
    fd->start_time = 0;
    fd->end_time = 0;
    fd->cleared_match = (cleared_match_t)std::stol (DEFAULT_FILTER, nullptr, 16);

    gnc_ppr_filter_update_date_query (plugin_page);
}

static std::vector<std::string>
split_filter_by_delimiter (std::string str, char delimiter)
{
    std::istringstream ss;
    std::vector<std::string> res;
    std::string token;
    ss.str (str);
    while (std::getline (ss, token, delimiter))
    {
        res.push_back (token);
    }
    return res;
}

void
gnc_ppr_filter_update_register (GncPluginPage* plugin_page)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(plugin_page));

    auto fd = gnc_plugin_page_register_get_filter_data (plugin_page);
    auto gsr = gnc_plugin_page_register_get_gsr (plugin_page);
    GNCLedgerDisplayType ledger_type = gnc_ledger_display_type (gsr->ledger);
    int filter_changed = 0;

    /* Set the filter for the split register and status of save filter button */
    fd->save_filter = false;

    std::string filter_strx = gnc_ppr_filter_get_filter (gsr, ledger_type);

    std::vector<std::string> split_filter = split_filter_by_delimiter (filter_strx, ';');
    int split_filter_size = split_filter.size();

    PINFO("Loaded Filter Status is %s", split_filter[0].c_str());

    fd->cleared_match = (cleared_match_t)std::stol (split_filter[0], nullptr, 16);

    if (split_filter_size > 0 && (split_filter[0].compare (DEFAULT_FILTER)) != 0)
        filter_changed++;

    if (split_filter_size > 1 && (split_filter[1].compare (std::string ("0"))) != 0)
    {
        PINFO("Loaded Filter Start Date is %s", split_filter[1].c_str());

        fd->start_time = gnc_ppr_filter_dmy2time (split_filter[1]);
        fd->start_time = gnc_time64_get_day_start (fd->start_time);
        filter_changed++;
    }

    if (split_filter_size > 2 && (split_filter[2].compare (std::string ("0"))) != 0)
    {
        PINFO("Loaded Filter End Date is %s", split_filter[2].c_str());

        fd->end_time = gnc_ppr_filter_dmy2time (split_filter[2]);
        fd->end_time = gnc_time64_get_day_end (fd->end_time);
        filter_changed++;
    }

    // set the default for the number of days
    fd->days = (int)std::stol (get_filter_default_num_of_days (ledger_type), nullptr, 10);

    if (split_filter_size > 3 &&
        (split_filter[3].compare (get_filter_default_num_of_days (ledger_type)) != 0))
    {
        PINFO("Loaded Filter Days is %s", split_filter[3].c_str());

        fd->days = (int)std::stol (split_filter[3], nullptr, 10);
        filter_changed++;
    }

    if (filter_changed != 0)
        fd->save_filter = true;

    fd->original_save_filter = fd->save_filter;

    if (ledger_type == LD_GL)
    {
        SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);
        time64 start_time = 0, end_time = 0;

        if (reg->type == GENERAL_JOURNAL)
        {
            start_time = fd->start_time;
            end_time = fd->end_time;
        }
        else // search ledger and the like
        {
            fd->days = 0;
            fd->cleared_match = (cleared_match_t)std::stol (DEFAULT_FILTER, nullptr, 16);
            fd->save_filter = false;
        }

        fd->original_days = fd->days;
        fd->original_start_time = start_time;
        fd->start_time = start_time;
        fd->original_end_time = end_time;
        fd->end_time = end_time;
    }
    /* Update Query with Filter Status and Dates */
    gnc_ppr_filter_update_status_query (plugin_page);
    gnc_ppr_filter_update_date_query (plugin_page);
}

/** This function is called whenever one of the status entries is
 *  checked or unchecked.  It updates the status value maintained for
 *  the filter dialog, and calls another function to do the work of
 *  applying the change to the register itself.
 *
 *  @param button The toggle button that was changed.
 *
 *  @param page A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 */
void
gnc_ppr_filter_status_one_cb (GtkToggleButton* button,
                              GncPluginPageRegister* page)
{
    g_return_if_fail (GTK_IS_CHECK_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(page));

    auto name = gtk_buildable_get_name (GTK_BUILDABLE(button));

    ENTER("toggle button %s (%p), plugin_page %p", name, button, page);

    auto fd = gnc_plugin_page_register_get_filter_data (GNC_PLUGIN_PAGE(page));

    /* Determine what status bit to change */
    int value = CLEARED_NONE;
    for (int i = 0; status_actions[i].action_name; i++)
    {
        if (g_strcmp0 (name, status_actions[i].action_name) == 0)
        {
            value = status_actions[i].value;
            break;
        }
    }

    /* Compute the new match status */
    if (gtk_toggle_button_get_active (button))
        fd->cleared_match = (cleared_match_t)(fd->cleared_match | value);
    else
        fd->cleared_match = (cleared_match_t)(fd->cleared_match & ~value);

    gnc_ppr_filter_update_status_query (GNC_PLUGIN_PAGE(page));

    LEAVE(" ");
}

/** This function is called whenever the "select all" status button is
 *  clicked.  It updates all of the checkbox widgets, then updates the
 *  query on the register.
 *
 *  @param button The button that was clicked.
 *
 *  @param page A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 */
void
gnc_ppr_filter_status_select_all_cb (GtkButton* button,
                                     GncPluginPageRegister* page)
{
    g_return_if_fail (GTK_IS_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(page));

    ENTER("(button %p, page %p)", button, page);

    auto fd = gnc_plugin_page_register_get_filter_data (GNC_PLUGIN_PAGE(page));

    /* Turn on all the check menu items */
    for (int i = 0; status_actions[i].action_name; i++)
    {
        auto widget = status_actions[i].widget;
        g_signal_handlers_block_by_func (widget,
                                         (gpointer)gnc_ppr_filter_status_one_cb, page);
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(widget), TRUE);
        g_signal_handlers_unblock_by_func (widget,
                                           (gpointer)gnc_ppr_filter_status_one_cb, page);
    }

    /* Set the requested status */
    fd->cleared_match = CLEARED_ALL;
    gnc_ppr_filter_update_status_query (GNC_PLUGIN_PAGE(page));
    LEAVE(" ");
}

/** This function is called whenever the "clear all" status button is
 *  clicked.  It updates all of the checkbox widgets, then updates the
 *  query on the register.
 *
 *  @param button The button that was clicked.
 *
 *  @param page A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 */
void
gnc_ppr_filter_status_clear_all_cb (GtkButton* button,
                                    GncPluginPageRegister* page)
{
    g_return_if_fail (GTK_IS_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(page));

    ENTER("(button %p, page %p)", button, page);

    auto fd = gnc_plugin_page_register_get_filter_data (GNC_PLUGIN_PAGE(page));

    /* Turn off all the check menu items */
    for (int i = 0; status_actions[i].action_name; i++)
    {
        auto widget = status_actions[i].widget;
        g_signal_handlers_block_by_func (widget,
                                         (gpointer)gnc_ppr_filter_status_one_cb, page);
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(widget), FALSE);
        g_signal_handlers_unblock_by_func (widget,
                                           (gpointer)gnc_ppr_filter_status_one_cb, page);
    }

    /* Set the requested status */
    fd->cleared_match = CLEARED_NONE;
    gnc_ppr_filter_update_status_query (GNC_PLUGIN_PAGE(page));
    LEAVE(" ");
}

/** This function computes the starting and ending times for the
 *  filter by examining the dialog widgets to see which ones are
 *  selected, and will pull times out of the data entry boxes if
 *  necessary.  This function must exist to handle the case where the
 *  "show all" button was Selected, and the user clicks on the "select
 *  range" button.  Since it exists, it make sense for the rest of the
 *  callbacks to take advantage of it.
 *
 *  @param page A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 */
static void
get_filter_times (GncPluginPageRegister* page)
{
    time64 time_val;

    auto fd = gnc_plugin_page_register_get_filter_data (GNC_PLUGIN_PAGE(page));

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(fd->start_date_choose)))
    {
        time_val = gnc_date_edit_get_date (GNC_DATE_EDIT(fd->start_date));
        time_val = gnc_time64_get_day_start (time_val);
        fd->start_time = time_val;
    }
    else
    {
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(fd->start_date_today)))
            fd->start_time = gnc_time64_get_today_start();
        else
            fd->start_time = 0;
    }

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(fd->end_date_choose)))
    {
        time_val = gnc_date_edit_get_date (GNC_DATE_EDIT(fd->end_date));
        time_val = gnc_time64_get_day_end (time_val);
        fd->end_time = time_val;
    }
    else
    {
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(fd->end_date_today)))
            fd->end_time = gnc_time64_get_today_end();
        else
            fd->end_time = 0;
    }
}

/** This function is called when the radio buttons changes state. This
 *  function is responsible for setting the sensitivity of the widgets
 *  controlled by each radio button choice and updating the time
 *  limitation on the register query. This is handled by a helper
 *  function as potentially all widgets will need to be examined.
 *
 *  @param button A pointer to the "select range" radio button.
 *
 *  @param page A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 */
void
gnc_ppr_filter_select_range_cb (GtkRadioButton* button,
                                GncPluginPageRegister* page)
{
    g_return_if_fail (GTK_IS_RADIO_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(page));

    ENTER("(button %p, page %p)", button, page);

    auto fd = gnc_plugin_page_register_get_filter_data (GNC_PLUGIN_PAGE(page));

    auto name = gtk_buildable_get_name (GTK_BUILDABLE(button));
    gboolean active = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(button));

    if (active && g_strcmp0 (name, "filter_show_range") == 0)
    {
        gtk_widget_set_sensitive (fd->table, active);
        gtk_widget_set_sensitive (fd->num_days, !active);
        get_filter_times (page);
    }
    else if (active && g_strcmp0 (name, "filter_show_days") == 0)
    {
        gtk_widget_set_sensitive (fd->table, !active);
        gtk_widget_set_sensitive (fd->num_days, active);
        gtk_spin_button_set_value (GTK_SPIN_BUTTON(fd->num_days), fd->days);
    }
    else
    {
        gtk_widget_set_sensitive (fd->table, FALSE);
        gtk_widget_set_sensitive (fd->num_days, FALSE);
        fd->days = 0;
        fd->start_time = 0;
        fd->end_time = 0;
    }
    gnc_ppr_filter_update_date_query (GNC_PLUGIN_PAGE(page));

    LEAVE(" ");
}

/** This function is called when the "number of days" spin button is
 *  changed which is then saved and updates the time limitation on
 *  the register query. This is handled by a helper function as
 *  potentially all widgets will need to be examined.
 *
 *  @param button A pointer to the "number of days" spin button.
 *
 *  @param page A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 */
void
gnc_ppr_filter_days_changed_cb (GtkSpinButton* button,
                                GncPluginPageRegister* page)
{
    g_return_if_fail (GTK_IS_SPIN_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(page));

    ENTER("(button %p, page %p)", button, page);

    auto fd = gnc_plugin_page_register_get_filter_data (GNC_PLUGIN_PAGE(page));

    fd->days = gtk_spin_button_get_value (GTK_SPIN_BUTTON(button));
    gnc_ppr_filter_update_date_query (GNC_PLUGIN_PAGE(page));

    LEAVE(" ");
}

/** This function is called when one of the start date entry widgets
 *  is updated.  It simply calls common routines to determine the
 *  start/end times and update the register query.
 *
 *  @param unused A pointer to a GncDateEntry widgets, but it could be
 *  any widget.
 *
 *  @param page A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 */
static void
gnc_ppr_filter_gde_changed_cb (GtkWidget* unused,
                               GncPluginPageRegister* page)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(page));

    ENTER("(widget %s(%p), page %p)",
           gtk_buildable_get_name (GTK_BUILDABLE(unused)), unused, page);

    get_filter_times (page);
    gnc_ppr_filter_update_date_query (GNC_PLUGIN_PAGE(page));

    LEAVE(" ");
}

/** This function is called when one of the start date radio buttons
 *  is selected.  It updates the sensitivity of the date entry widget,
 *  then calls a common routine to determine the start/end times and
 *  update the register query.
 *
 *  *Note: This function is actually called twice for each new radio
 *  button selection.  The first time call is to uncheck the old
 *  button, and the second time to check the new button.  This does
 *  make a kind of sense, as radio buttons are nothing more than
 *  linked toggle buttons where only one can be active.
 *
 *  @param radio The button whose state is changing.  This will be
 *  the previously selected button the first of the pair of calls to
 *  this function, and will be the newly selected button the second
 *  time.
 *
 *  @param page A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 */
void
gnc_ppr_filter_start_cb (GtkWidget* radio,
                         GncPluginPageRegister* page)
{
    g_return_if_fail (GTK_IS_RADIO_BUTTON(radio));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(page));

    ENTER("(radio %s(%p), page %p)",
           gtk_buildable_get_name (GTK_BUILDABLE(radio)), radio, page);

    if (!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(radio)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }
    auto fd = gnc_plugin_page_register_get_filter_data (GNC_PLUGIN_PAGE(page));

    auto name = gtk_buildable_get_name (GTK_BUILDABLE(radio));
    gboolean active = !g_strcmp0 (name, "start_date_choose");
    gtk_widget_set_sensitive (fd->start_date, active);
    get_filter_times (page);
    gnc_ppr_filter_update_date_query (GNC_PLUGIN_PAGE(page));

    LEAVE(" ");
}

/** This function is called when one of the end date radio buttons is
 *  selected.  It updates the sensitivity of the date entry widget,
 *  then calls a common routine to determine the start/end times and
 *  update the register query.
 *
 *  *Note: This function is actually called twice for each new radio
 *  button selection.  The first time call is to uncheck the old
 *  button, and the second time to check the new button.  This does
 *  make a kind of sense, as radio buttons are nothing more than
 *  linked toggle buttons where only one can be active.
 *
 *  @param radio The button whose state is changing.  This will be
 *  the previously selected button the first of the pair of calls to
 *  this function, and will be the newly selected button the second
 *  time.
 *
 *  @param page A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 */
void
gnc_ppr_filter_end_cb (GtkWidget* radio,
                       GncPluginPageRegister* page)
{
    g_return_if_fail (GTK_IS_RADIO_BUTTON(radio));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(page));

    ENTER("(radio %s(%p), page %p)",
          gtk_buildable_get_name (GTK_BUILDABLE(radio)), radio, page);

    if (!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(radio)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }

    auto fd = gnc_plugin_page_register_get_filter_data (GNC_PLUGIN_PAGE(page));
    auto name = gtk_buildable_get_name (GTK_BUILDABLE(radio));
    gboolean active = !g_strcmp0 (name, "end_date_choose");
    gtk_widget_set_sensitive (fd->end_date, active);
    get_filter_times (page);
    gnc_ppr_filter_update_date_query (GNC_PLUGIN_PAGE(page));

    LEAVE(" ");
}

/** This function is called whenever the save status is checked
 *  or unchecked. It will allow saving of the filter if required.
 *
 *  @param button The toggle button that was changed.
 *
 *  @param page A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 */
void
gnc_ppr_filter_save_cb (GtkToggleButton* button,
                        GncPluginPageRegister* page)
{
    g_return_if_fail (GTK_IS_CHECK_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(page));

    ENTER("Save toggle button (%p), plugin_page %p", button, page);

    auto fd = gnc_plugin_page_register_get_filter_data (GNC_PLUGIN_PAGE(page));

    /* Compute the new save filter status */
    if (gtk_toggle_button_get_active (button))
        fd->save_filter = true;
    else
        fd->save_filter = false;

    LEAVE(" ");
}

/** This function is called when the "Filter By…" dialog is closed.
 *  If the dialog was closed by any method other than clicking the OK
 *  button, the original sorting order will be restored.
 *
 *  @param dialog A pointer to the dialog box.
 *
 *  @param response A numerical value indicating why the dialog box was closed.
 *
 *  @param page A pointer to the GncPluginPageRegister associated with
 *  this dialog box.
 */
void
gnc_ppr_filter_response_cb (GtkDialog* dialog,
                            gint response,
                            GncPluginPageRegister* page)
{
    g_return_if_fail (GTK_IS_DIALOG(dialog));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(page));

    ENTER(" ");

    auto fd = gnc_plugin_page_register_get_filter_data (GNC_PLUGIN_PAGE(page));
    auto gsr = gnc_plugin_page_register_get_gsr (GNC_PLUGIN_PAGE(page));

    if (response != GTK_RESPONSE_OK)
    {
        /* Remove the old status match */
        fd->cleared_match = fd->original_cleared_match;
        gnc_plugin_register_set_enable_refresh (page, FALSE);
        gnc_ppr_filter_update_status_query (GNC_PLUGIN_PAGE(page));
        gnc_plugin_register_set_enable_refresh (page, TRUE);
        fd->start_time = fd->original_start_time;
        fd->end_time = fd->original_end_time;
        fd->days = fd->original_days;
        fd->save_filter = fd->original_save_filter;
        gnc_ppr_filter_update_date_query (GNC_PLUGIN_PAGE(page));
    }
    else
    {
        // clear the filter when unticking the save option
        if (!fd->save_filter && fd->original_save_filter)
            gnc_ppr_filter_set_filter (gsr, "");

        fd->original_save_filter = fd->save_filter;

        if (fd->save_filter)
        {
            std::string save_filter_str;
            static const size_t buffer_size = 10;
            char buffer [buffer_size];

            // cleared match
            std::snprintf (buffer, buffer_size, "0x%04x", fd->cleared_match);
            save_filter_str.append (buffer);

            // start time
            if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(fd->start_date_choose))
                && fd->start_time != 0)
            {
                save_filter_str.append (";" + gnc_ppr_filter_time2dmy (fd->start_time));
            }
            else
                save_filter_str.append (";0");

            // end time
            if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(fd->end_date_choose))
                && fd->end_time != 0)
            {
                save_filter_str.append (";" + gnc_ppr_filter_time2dmy (fd->end_time));
            }
            else
                save_filter_str.append (";0");

            // number of days
            if (fd->days > 0)
            {
                save_filter_str.append (";" + std::to_string (fd->days));
            }
            else
                save_filter_str.append (";0");

            PINFO("The filter to save is %s", save_filter_str.c_str());

            gnc_ppr_filter_set_filter (gsr, save_filter_str);
        }
    }
    fd->dialog = nullptr;
    gtk_widget_destroy (GTK_WIDGET(dialog));

    LEAVE(" ");
}

void
gnc_ppr_filter_by (GncPluginPage *plugin_page, Query *query,
                   FilterData *fd, bool show_save_button)
{
    time64 start_time, end_time, time_val;

    /* Create the dialog */
    auto builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "gnc-plugin-page-register.glade",
                               "days_adjustment");
    gnc_builder_add_from_file (builder, "gnc-plugin-page-register.glade",
                               "filter_by_dialog");
    auto dialog = GTK_WIDGET(gtk_builder_get_object (builder, "filter_by_dialog"));
    fd->dialog = dialog;
    gtk_window_set_transient_for (GTK_WINDOW(dialog),
                                  gnc_window_get_gtk_window (GNC_WINDOW(GNC_PLUGIN_PAGE(plugin_page)->window)));

    /* Translators: The %s is the name of the plugin page */
    auto title = g_strdup_printf (_ ("Filter %s by…"),
                     gnc_plugin_page_get_page_name (GNC_PLUGIN_PAGE(plugin_page)));
    gtk_window_set_title (GTK_WINDOW(dialog), title);
    g_free (title);

    /* Set the check buttons for the current status */
    for (int i = 0; status_actions[i].action_name; i++)
    {
        auto toggle = GTK_WIDGET(gtk_builder_get_object (builder,
                                                     status_actions[i].action_name));
        bool value = fd->cleared_match & status_actions[i].value;
        status_actions[i].widget = toggle;
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(toggle), bool_to_gboolean (value));
    }
    fd->original_cleared_match = fd->cleared_match;

    auto button = GTK_WIDGET(gtk_builder_get_object (builder, "filter_save"));
    if (fd->save_filter)
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button), TRUE);

    // hide the save button if appropriate
    gtk_widget_set_visible (GTK_WIDGET(button), bool_to_gboolean (show_save_button));

    /* Set up number of days */
    fd->num_days = GTK_WIDGET(gtk_builder_get_object (builder,
                                                            "filter_show_num_days"));
    button = GTK_WIDGET(gtk_builder_get_object (builder, "filter_show_days"));

    if (fd->days > 0) // using number of days
    {
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button), TRUE);
        gtk_widget_set_sensitive (GTK_WIDGET(fd->num_days), TRUE);
        gtk_spin_button_set_value (GTK_SPIN_BUTTON(fd->num_days), fd->days);
        fd->original_days = fd->days;

        /* Set the start_time and end_time to 0 */
        start_time = 0;
        end_time = 0;
    }
    else
    {
        gtk_widget_set_sensitive (GTK_WIDGET(fd->num_days), FALSE);
        fd->original_days = 0;
        fd->days = 0;

        /* Get the start and end times */
        xaccQueryGetDateMatchTT (query, &start_time, &end_time);
    }

    /* Set the date info */
    fd->original_start_time = start_time;
    fd->start_time = start_time;
    fd->original_end_time = end_time;
    fd->end_time = end_time;

    button = GTK_WIDGET(gtk_builder_get_object (builder, "filter_show_range"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button), start_time || end_time);
    auto table = GTK_WIDGET(gtk_builder_get_object (builder, "select_range_table"));
    fd->table = table;
    gtk_widget_set_sensitive (GTK_WIDGET(table), start_time || end_time);

    fd->start_date_choose = GTK_WIDGET(gtk_builder_get_object (builder, "start_date_choose"));
    fd->start_date_today = GTK_WIDGET(gtk_builder_get_object (builder, "start_date_today"));
    fd->end_date_choose = GTK_WIDGET(gtk_builder_get_object (builder, "end_date_choose"));
    fd->end_date_today = GTK_WIDGET(gtk_builder_get_object (builder, "end_date_today"));

    bool sensitive;
    {
        /* Start date info */
        if (start_time == 0)
        {
            button = GTK_WIDGET(gtk_builder_get_object(builder, "start_date_earliest"));
            time_val = xaccQueryGetEarliestDateFound (query);
            sensitive = false;
        }
        else
        {
            time_val = start_time;
            if ((start_time >= gnc_time64_get_today_start()) &&
                (start_time <= gnc_time64_get_today_end()))
            {
                button = fd->start_date_today;
                sensitive = false;
            }
            else
            {
                button = fd->start_date_choose;
                sensitive = true;
            }
        }
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button), TRUE);
        fd->start_date = gnc_date_edit_new (gnc_time (nullptr), FALSE, FALSE);
        auto hbox = GTK_WIDGET(gtk_builder_get_object (builder, "start_date_hbox"));
        gtk_box_pack_start (GTK_BOX(hbox), fd->start_date, TRUE, TRUE, 0);
        gtk_widget_show (fd->start_date);
        gtk_widget_set_sensitive (GTK_WIDGET(fd->start_date), bool_to_gboolean (sensitive));
        gnc_date_edit_set_time (GNC_DATE_EDIT(fd->start_date), time_val);
        g_signal_connect (G_OBJECT(fd->start_date), "date-changed",
                          G_CALLBACK(gnc_ppr_filter_gde_changed_cb),
                          GNC_PLUGIN_PAGE_REGISTER(plugin_page));
    }

    {
        /* End date info */
        if (end_time == 0)
        {
            button = GTK_WIDGET(gtk_builder_get_object (builder, "end_date_latest"));
            time_val = xaccQueryGetLatestDateFound (query);
            sensitive = false;
        }
        else
        {
            time_val = end_time;
            if ((end_time >= gnc_time64_get_today_start()) &&
                (end_time <= gnc_time64_get_today_end()))
            {
                button = fd->end_date_today;
                sensitive = false;
            }
            else
            {
                button = fd->end_date_choose;
                sensitive = true;
            }
        }
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button), TRUE);
        fd->end_date = gnc_date_edit_new (gnc_time (nullptr), FALSE, FALSE);
        auto hbox = GTK_WIDGET(gtk_builder_get_object (builder, "end_date_hbox"));
        gtk_box_pack_start (GTK_BOX(hbox), fd->end_date, TRUE, TRUE, 0);
        gtk_widget_show (fd->end_date);
        gtk_widget_set_sensitive (GTK_WIDGET(fd->end_date), bool_to_gboolean (sensitive));
        gnc_date_edit_set_time (GNC_DATE_EDIT(fd->end_date), time_val);
        g_signal_connect (G_OBJECT(fd->end_date), "date-changed",
                          G_CALLBACK(gnc_ppr_filter_gde_changed_cb),
                          GNC_PLUGIN_PAGE_REGISTER(plugin_page));
    }

    /* Wire it up */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func,
                                      GNC_PLUGIN_PAGE_REGISTER(plugin_page));

    /* Show it */
    gtk_widget_show (dialog);
    g_object_unref (G_OBJECT(builder));
    LEAVE (" ");
}
