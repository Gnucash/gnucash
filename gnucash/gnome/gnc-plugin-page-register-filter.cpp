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
#include "gnc-gtk-utils.h"
#include "gnc-date.h"
#include "gnc-date-edit.h"
#include "gnc-string-utils.h"
#include "gnc-ui.h"
#include "gnc-state.h"
#include "gnc-period-select.h"
#include "gnc-prefs.h"
#include "gnc-ui-util.h"
#include "gnc-window.h"
#include "gnc-main-window.h"
#include "engine-helpers.h"
#include "qofbookslots.h"
#include "qof.h"
#include "Query.h"

#include <algorithm>
#include <array>
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

struct RegisterFilterDialog
{
    GncPluginPage* plugin_page;
    GtkWidget*     dialog;
    guint          references;
    bool           completed;
    bool           validation_pending;
    GtkWidget*     table;
    GtkWidget*     start_earliest;       //label
    GtkWidget*     start_relative_check; //checkbutton
    GtkWidget*     start_relative;       //account period
    GtkWidget*     start_date_check;     //checkbutton
    GtkWidget*     start_date;           //date
    GtkWidget*     start_days_check;     //checkbutton
    GtkWidget*     start_days;           //spin

    GtkWidget*     end_latest;
    GtkWidget*     end_relative_check;
    GtkWidget*     end_relative;
    GtkWidget*     end_date_check;
    GtkWidget*     end_date;
    GtkWidget*     end_days_check;
    GtkWidget*     end_days;

    GtkWidget*     num_days;

    cleared_match_t     original_cleared_match;
    GncAccountingPeriod original_start_ap;
    time64              original_start_time;
    int                 original_start_days;
    GncAccountingPeriod original_end_ap;
    time64              original_end_time;
    int                 original_end_days;
    int                 original_days;
    bool                original_save_filter;

    bool                show_save_button;
};

extern "C"
{
// These functions are the dialog callbacks. They're connected to their
// signals in gnc-plugin-page-register.glade so they mustn't be name-mangled.
void
gnc_ppr_filter_select_range_cb (GtkCheckButton* button,
                                RegisterFilterDialog* rfd);
void
gnc_ppr_filter_start_cb (GtkWidget* radio,
                         RegisterFilterDialog* rfd);
void
gnc_ppr_filter_end_cb (GtkWidget* radio,
                       RegisterFilterDialog* rfd);
void
gnc_ppr_filter_status_select_all_cb (GtkButton* button,
                                     RegisterFilterDialog* rfd);
void
gnc_ppr_filter_status_clear_all_cb (GtkButton* button,
                                    RegisterFilterDialog* rfd);
void
gnc_ppr_filter_status_one_cb (GtkCheckButton* button,
                              RegisterFilterDialog* rfd);
void
gnc_ppr_filter_save_cb (GtkCheckButton* button,
                        RegisterFilterDialog* rfd);
void
gnc_ppr_filter_days_changed_cb (GtkSpinButton* button,
                                RegisterFilterDialog* rfd);
void
gnc_ppr_filter_start_toggle_cb (GtkCheckButton* button,
                                RegisterFilterDialog* rfd);
void
gnc_ppr_filter_end_toggle_cb (GtkCheckButton* button,
                              RegisterFilterDialog* rfd);
void
gnc_ppr_filter_start_end_days_changed_cb (GtkSpinButton* button,
                                          RegisterFilterDialog* rfd);
}

struct status_action
{
    std::string action_name;
    int value;
    GtkWidget* widget;
};

static std::array<status_action, 5> status_actions {{
    { "filter_status_reconciled",   CLEARED_RECONCILED, nullptr },
    { "filter_status_cleared",      CLEARED_CLEARED, nullptr },
    { "filter_status_voided",       CLEARED_VOIDED, nullptr },
    { "filter_status_frozen",       CLEARED_FROZEN, nullptr },
    { "filter_status_unreconciled", CLEARED_NO, nullptr }
}};

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
ppr_filter_time2dmy (time64 raw_time)
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
ppr_filter_dmy2time (std::string date_string)
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

/* This function subtracts a number of days from now to a time64 value */
static time64
get_time_for_days_ago (int days, bool use_day_start)
{
    time64 time_val = 0;

    if (days > 0)
    {
        struct tm tm;
        if (use_day_start)
            gnc_tm_get_today_start (&tm);
        else
            gnc_tm_get_today_end (&tm);
        tm.tm_mday = tm.tm_mday - days;
        time_val = gnc_mktime (&tm);
    }
    return time_val;
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

static void
ppr_check_for_empty_group (GKeyFile *state_file,
                           const gchar *state_section)
{
    gsize num_keys;
    gchar **keys = g_key_file_get_keys (state_file, state_section, &num_keys, nullptr);

    if (num_keys == 0)
        gnc_state_drop_sections_for (state_section);

    g_strfreev (keys);
}

static std::string
ppr_filter_load_filter (GNCSplitReg *gsr, GNCLedgerDisplayType ledger_type)
{
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
set_filterdata_to_defaults (FilterData *fd, bool date_parts_only)
{
    if (!date_parts_only)
    {
        fd->cleared_match = (cleared_match_t)std::stol (DEFAULT_FILTER, nullptr, 16);
        fd->save_filter = false;
    }
    fd->start_ap = GNC_ACCOUNTING_PERIOD_INVALID;
    fd->start_time = 0;
    fd->start_days = 0;
    fd->end_ap = GNC_ACCOUNTING_PERIOD_INVALID;
    fd->end_time = 0;
    fd->end_days = 0;
    fd->days = 0;
}

static int
get_trailing_int (const std::string split_filter, const std::string find_text)
{
    int ret_int = -1;
    std::size_t found = split_filter.find (find_text);

    if (found != std::string::npos)
    {
        std::string found_str = split_filter.substr (found + find_text.length(), std::string::npos);
        ret_int = std::stol (found_str, nullptr, 10);
    }
    return ret_int;
}

static void
update_fd_with_date_filter_parts (FilterData *fd, const std::string filter_part,
                                  bool start_filter, int ap_trailing_int, int days_trailing_int)
{
    if (ap_trailing_int != -1)
    {
        GDate *tmp_date = nullptr;

        if (start_filter)
        {
            fd->start_ap = (GncAccountingPeriod)ap_trailing_int;
            tmp_date = gnc_accounting_period_start_gdate (fd->start_ap, nullptr, nullptr);
        }
        else
        {
            fd->end_ap = (GncAccountingPeriod)ap_trailing_int;
            tmp_date = gnc_accounting_period_end_gdate (fd->end_ap, nullptr, nullptr);
        }

        if (tmp_date)
        {
            if (start_filter)
                fd->start_time = gnc_time64_get_day_start_gdate (tmp_date);
            else
                fd->end_time = gnc_time64_get_day_end_gdate (tmp_date);

            g_date_free (tmp_date);
        }
    }
    else
    {
        if (days_trailing_int != -1)
        {
            if (start_filter)
            {
                fd->start_days = days_trailing_int;
                fd->start_time = get_time_for_days_ago (fd->start_days, true);
            }
            else
            {
                fd->end_days = days_trailing_int;
                fd->end_time = get_time_for_days_ago (fd->end_days, false);
            }
        }
        else
        {
            time64 tmp_time = ppr_filter_dmy2time (filter_part);
            if (start_filter)
                fd->start_time = gnc_time64_get_day_start (tmp_time);
            else
                fd->end_time = gnc_time64_get_day_end (tmp_time);
        }
    }
    fd->save_filter = true;
}

static void
ppr_filter_load_filter_parts (GNCSplitReg *gsr, GNCLedgerDisplayType ledger_type, FilterData *fd)
{
    set_filterdata_to_defaults (fd, false);
    fd->dialog = nullptr;

    if (!gsr)
        return;

    std::string filter_str = ppr_filter_load_filter (gsr, ledger_type);

    PINFO("Loaded Filter String is %s", filter_str.c_str());

    std::vector<std::string> split_filter = split_filter_by_delimiter (filter_str, ';');
    int split_filter_size = split_filter.size();

    if (split_filter_size > 0 && (split_filter[0].compare (DEFAULT_FILTER)) != 0)
    {
        PINFO("Loaded Filter Status is %s", split_filter[0].c_str());

        fd->cleared_match = (cleared_match_t)std::stol (split_filter[0], nullptr, 16);
        fd->save_filter = true;
    }

    if (split_filter_size > 1 && (split_filter[1].compare (std::string ("0"))) != 0)
    {
        PINFO("Loaded Filter Start Date is %s", split_filter[1].c_str());

        int ap_trailing_int = get_trailing_int (split_filter[1], "SAP");
        int days_trailing_int = get_trailing_int (split_filter[1], "SDAY");

        update_fd_with_date_filter_parts (fd, split_filter[1], true, ap_trailing_int, days_trailing_int);
    }

    if (split_filter_size > 2 && (split_filter[2].compare (std::string ("0"))) != 0)
    {
        PINFO("Loaded Filter End Date is %s", split_filter[2].c_str());

        int ap_trailing_int = get_trailing_int (split_filter[2], "EAP");
        int days_trailing_int = get_trailing_int (split_filter[2], "EDAY");

        update_fd_with_date_filter_parts (fd, split_filter[2], false, ap_trailing_int, days_trailing_int);
    }

    // set the default for the number of days
    fd->days = (int)std::stol (get_filter_default_num_of_days (ledger_type), nullptr, 10);

    if (split_filter_size > 3 &&
        (split_filter[3].compare (get_filter_default_num_of_days (ledger_type)) != 0))
    {
        PINFO("Loaded Filter Days is %s", split_filter[3].c_str());

        fd->days = (int)std::stol (split_filter[3], nullptr, 10);
        fd->save_filter = true;
    }
}

static void
ppr_filter_save_filter (GNCSplitReg *gsr, std::string filter)

{
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

        ppr_check_for_empty_group (state_file, state_section);
    }
    else
    {
        PINFO("The filter to save is %s", filter.c_str());
        g_key_file_set_string (state_file, state_section, KEY_PAGE_FILTER,
                               filter.c_str());
    }
    g_free (state_section);
}

static void
ppr_filter_save_filter_parts (GNCSplitReg *gsr, FilterData *fd)
{
    if (!gsr)
        return;

    std::string save_filter_str;

    if (fd->save_filter)
    {
        static const size_t buffer_size = 10;
        char buffer [buffer_size];

        // cleared match
        std::snprintf (buffer, buffer_size, "0x%04x", fd->cleared_match);
        save_filter_str.append (buffer);

        // start time
        if (fd->start_ap != GNC_ACCOUNTING_PERIOD_INVALID)
            save_filter_str.append (";SAP" + std::to_string (fd->start_ap));
        else if (fd->start_days > 0)
            save_filter_str.append (";SDAY" + std::to_string (fd->start_days));
        else if (fd->start_time != 0)
            save_filter_str.append (";" + ppr_filter_time2dmy (fd->start_time));
        else
            save_filter_str.append (";0");

        // end time
        if (fd->end_ap != GNC_ACCOUNTING_PERIOD_INVALID)
            save_filter_str.append (";EAP" + std::to_string (fd->end_ap));
        else if (fd->end_days > 0)
            save_filter_str.append (";EDAY" + std::to_string (fd->end_days));
        else if (fd->end_time != 0)
            save_filter_str.append (";" + ppr_filter_time2dmy (fd->end_time));
        else
            save_filter_str.append (";0");

        // number of days
        if (fd->days > 0)
        {
             save_filter_str.append (";" + std::to_string (fd->days));
        }
        else
             save_filter_str.append (";0");
    }
    ppr_filter_save_filter (gsr, save_filter_str);
}

static void
update_match_filter_text (cleared_match_t match, const guint mask,
                          const gchar* filter_name, GList **show, GList **hide)
{
    if ((match & mask) == mask)
        *show = g_list_prepend (*show, g_strdup (filter_name));
    else
        *hide = g_list_prepend (*hide, g_strdup (filter_name));
}

/** This function is used to update the tooltip shown in the register
 *  which shows a summary of the current filter.
 *
 *  @param page A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 *
 *  @param fd A pointer to the filter data used for filter state.
 */
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

        update_match_filter_text (fd->cleared_match, 0x01, _("Unreconciled"),
                                  &show, &hide);
        update_match_filter_text (fd->cleared_match, 0x02, _("Cleared"),
                                  &show, &hide);
        update_match_filter_text (fd->cleared_match, 0x04, _("Reconciled"),
                                  &show, &hide);
        update_match_filter_text (fd->cleared_match, 0x08, _("Frozen"),
                                  &show, &hide);
        update_match_filter_text (fd->cleared_match, 0x10, _("Voided"),
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

static void
ppr_filter_update_status_query (GncPluginPage* plugin_page)
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

static void
ppr_filter_update_date_query (GncPluginPage* plugin_page)
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
        time64 start = get_time_for_days_ago (fd->days, true);
        xaccQueryAddDateMatchTT (query, TRUE, start, FALSE, 0, QOF_QUERY_AND);
    }

    // Set filter tooltip for summary bar
    gnc_ppr_filter_set_tooltip (plugin_page, fd);

    gnc_plugin_page_register_query_update (GNC_PLUGIN_PAGE_REGISTER(plugin_page), query);
    LEAVE(" ");
}

/** This function is used to clear the current filter so that a
 *  specific split can be shown in the register.
 *
 *  @param page A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 */
void
gnc_ppr_filter_clear_current_filter (GncPluginPage* plugin_page)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(plugin_page));

    auto fd = gnc_plugin_page_register_get_filter_data (plugin_page);

    set_filterdata_to_defaults (fd, false);

    ppr_filter_update_date_query (plugin_page);
}

/** This function is called to update the register.
 *
 *  @param page A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 */
void
gnc_ppr_filter_update_register (GncPluginPage* plugin_page)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(plugin_page));

    auto gsr = gnc_plugin_page_register_get_gsr (plugin_page);

    if (!gsr)
        return;

    auto fd = gnc_plugin_page_register_get_filter_data (plugin_page);
    GNCLedgerDisplayType ledger_type = gnc_ledger_display_type (gsr->ledger);

    /* Set the filter for the split register and status of save filter button */
    fd->save_filter = false;

    ppr_filter_load_filter_parts (gsr, ledger_type, fd);

    if (ledger_type == LD_GL)
    {
        SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);

        if (reg->type != GENERAL_JOURNAL) // search ledger and the like
            set_filterdata_to_defaults (fd, false);
    }
    /* Update Query with Filter Status and Dates */
    ppr_filter_update_status_query (plugin_page);
    ppr_filter_update_date_query (plugin_page);
}

/** This function is called whenever one of the status entries is
 *  checked or unchecked.  It updates the status value maintained for
 *  the filter dialog, and calls another function to do the work of
 *  applying the change to the register itself.
 *
 *  @param button The toggle button that was changed.
 *
 *  @param rfd A pointer to the filter dialog structure.
 */
void
gnc_ppr_filter_status_one_cb (GtkCheckButton* button,
                              RegisterFilterDialog* rfd)
{
    g_return_if_fail (GTK_IS_CHECK_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rfd->plugin_page));

    auto name = gtk_buildable_get_buildable_id (GTK_BUILDABLE(button));

    ENTER("toggle button %s (%p), plugin_page %p", name, button, rfd->plugin_page);

    auto fd = gnc_plugin_page_register_get_filter_data (rfd->plugin_page);

    /* Determine what status bit to change */
    int value = CLEARED_NONE;
    for (const auto& action : status_actions)
    {
        if (action.action_name.compare (name) == 0)
        {
            value = action.value;
            break;
        }
    }

    /* Compute the new match status */
    if (gtk_check_button_get_active (button))
        fd->cleared_match = (cleared_match_t)(fd->cleared_match | value);
    else
        fd->cleared_match = (cleared_match_t)(fd->cleared_match & ~value);

    ppr_filter_update_status_query (rfd->plugin_page);

    LEAVE(" ");
}

static void
set_sensitive_start_widget (RegisterFilterDialog *rfd, GtkWidget *enable_widget, gboolean active)
{
    gtk_widget_set_sensitive (GTK_WIDGET(rfd->start_earliest), !active);
    gtk_widget_set_sensitive (GTK_WIDGET(rfd->start_relative), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET(rfd->start_date), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET(rfd->start_days), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET(enable_widget), active);
}

static void
set_sensitive_end_widget (RegisterFilterDialog *rfd, GtkWidget *enable_widget, gboolean active)
{
    gtk_widget_set_sensitive (GTK_WIDGET(rfd->end_latest), !active);
    gtk_widget_set_sensitive (GTK_WIDGET(rfd->end_relative), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET(rfd->end_date), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET(rfd->end_days), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET(enable_widget), active);
}

static void
set_checkbutton_with_blocking (GtkWidget *widget1, GtkWidget *widget2,
                               GFunc function,
                               RegisterFilterDialog *rfd,
                               gboolean active)
{
    PINFO("Block GtkCheckButton %p for setting active %s",
           widget1, active ? "TRUE" : "FALSE");
    g_signal_handlers_block_by_func (widget1,
                                     (gpointer)function, rfd);
    gtk_check_button_set_active (GTK_CHECK_BUTTON(widget1), active);
    g_signal_handlers_unblock_by_func (widget1,
                                       (gpointer)function, rfd);

    if (widget2)
    {
        PINFO("Block GtkCheckButton %p for setting active %s",
               widget2, active ? "TRUE" : "FALSE");
        g_signal_handlers_block_by_func (widget2,
                                         (gpointer)function, rfd);
        gtk_check_button_set_active (GTK_CHECK_BUTTON(widget2), active);
        g_signal_handlers_unblock_by_func (widget2,
                                           (gpointer)function, rfd);
    }
}

/** This function is called whenever the "select all" status button is
 *  clicked.  It updates all of the checkbox widgets, then updates the
 *  query on the register.
 *
 *  @param button The button that was clicked.
 *
 *  @param rfd A pointer to the filter dialog structure.
 */
void
gnc_ppr_filter_status_select_all_cb (GtkButton* button,
                                     RegisterFilterDialog* rfd)
{
    g_return_if_fail (GTK_IS_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rfd->plugin_page));

    ENTER("(button %p, page %p)", button, rfd->plugin_page);

    auto fd = gnc_plugin_page_register_get_filter_data (rfd->plugin_page);

    /* Turn on all the check menu items */
    for (const auto& action : status_actions)
    {
        set_checkbutton_with_blocking (action.widget, nullptr,
                                       (GFunc)gnc_ppr_filter_status_one_cb,
                                       rfd, TRUE);
    }

    /* Set the requested status */
    fd->cleared_match = CLEARED_ALL;
    ppr_filter_update_status_query (rfd->plugin_page);
    LEAVE(" ");
}

/** This function is called whenever the "clear all" status button is
 *  clicked.  It updates all of the checkbox widgets, then updates the
 *  query on the register.
 *
 *  @param button The button that was clicked.
 *
 *  @param rfd A pointer to the filter dialog structure.
 */
void
gnc_ppr_filter_status_clear_all_cb (GtkButton* button,
                                    RegisterFilterDialog* rfd)
{
    g_return_if_fail (GTK_IS_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rfd->plugin_page));

    ENTER("(button %p, page %p)", button, rfd->plugin_page);

    auto fd = gnc_plugin_page_register_get_filter_data (rfd->plugin_page);

    /* Turn off all the check menu items */
    for (const auto& action : status_actions)
    {
        set_checkbutton_with_blocking (action.widget, nullptr,
                                       (GFunc)gnc_ppr_filter_status_one_cb,
                                       rfd, FALSE);
    }

    /* Set the requested status */
    fd->cleared_match = CLEARED_NONE;
    ppr_filter_update_status_query (rfd->plugin_page);
    LEAVE(" ");
}

static void
print_info_time64_date (const gchar *text, time64 date)
{
   gchar *date_txt = qof_print_date (date);
   PINFO("%s, %s", text, date_txt);
   g_free (date_txt);
}

static void
get_filter_times (RegisterFilterDialog* rfd)
{
    time64 time_val;

    auto fd = gnc_plugin_page_register_get_filter_data (rfd->plugin_page);

    if (gtk_check_button_get_active (GTK_CHECK_BUTTON(rfd->start_date_check)))
    {
        time_val = gnc_date_edit_get_date (GNC_DATE_EDIT(rfd->start_date));
        time_val = gnc_time64_get_day_start (time_val);
        fd->start_time = time_val;
        fd->start_ap = GNC_ACCOUNTING_PERIOD_INVALID;
        print_info_time64_date ("Start date is", fd->start_time);
    }
    else if (gtk_check_button_get_active (GTK_CHECK_BUTTON(rfd->start_relative_check)))
    {
        auto *sdate = gnc_period_select_get_date (GNC_PERIOD_SELECT(rfd->start_relative));
        fd->start_time = gnc_time64_get_day_start_gdate (sdate);
        fd->start_ap = gnc_period_select_get_active (GNC_PERIOD_SELECT(rfd->start_relative));
        print_info_time64_date ("Start date relative is", fd->start_time);
        g_date_free (sdate);
    }
    else if (gtk_check_button_get_active (GTK_CHECK_BUTTON(rfd->start_days_check)))
    {
        fd->start_days = gtk_spin_button_get_value (GTK_SPIN_BUTTON(rfd->start_days));
        fd->start_time = get_time_for_days_ago (fd->start_days, true);
        fd->start_ap = GNC_ACCOUNTING_PERIOD_INVALID;
        print_info_time64_date ("Start date using days is", fd->start_time);
    }
    else
        fd->start_time = 0;

    if (gtk_check_button_get_active (GTK_CHECK_BUTTON(rfd->end_date_check)))
    {
        time_val = gnc_date_edit_get_date (GNC_DATE_EDIT(rfd->end_date));
        time_val = gnc_time64_get_day_end (time_val);
        fd->end_time = time_val;
        fd->end_ap = GNC_ACCOUNTING_PERIOD_INVALID;
        print_info_time64_date ("End date is", fd->end_time);
    }
    else if (gtk_check_button_get_active (GTK_CHECK_BUTTON(rfd->end_relative_check)))
    {
        auto *edate = gnc_period_select_get_date (GNC_PERIOD_SELECT(rfd->end_relative));
        fd->end_time = gnc_time64_get_day_end_gdate (edate);
        fd->end_ap = gnc_period_select_get_active (GNC_PERIOD_SELECT(rfd->end_relative));
        print_info_time64_date ("End date relative is", fd->end_time);
        g_date_free (edate);
    }
    else if (gtk_check_button_get_active (GTK_CHECK_BUTTON(rfd->end_days_check)))
    {
        fd->end_days = gtk_spin_button_get_value (GTK_SPIN_BUTTON(rfd->end_days));
        fd->end_time = get_time_for_days_ago (fd->end_days, false);
        fd->end_ap = GNC_ACCOUNTING_PERIOD_INVALID;
        print_info_time64_date ("End date using days is", fd->end_time);
    }
    else
        fd->end_time = 0;
}

/** This function is called when the radio buttons changes state. This
 *  function is responsible for setting the sensitivity of the widgets
 *  controlled by each radio button choice and updating the time
 *  limitation on the register query. This is handled by a helper
 *  function as potentially all widgets will need to be examined.
 *
 *  @param button A pointer to the "select range" radio button.
 *
 *  @param rfd A pointer to the filter dialog structure.
 */
void
gnc_ppr_filter_select_range_cb (GtkCheckButton* button,
                                RegisterFilterDialog* rfd)
{
    g_return_if_fail (GTK_IS_CHECK_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rfd->plugin_page));

    ENTER("(button %p, page %p)", button, rfd->plugin_page);

    auto fd = gnc_plugin_page_register_get_filter_data (rfd->plugin_page);

    auto name = gtk_buildable_get_buildable_id (GTK_BUILDABLE(button));
    gboolean active = gtk_check_button_get_active (GTK_CHECK_BUTTON(button));

    if (active && g_strcmp0 (name, "filter_show_range") == 0)
    {
        gtk_widget_set_sensitive (rfd->table, active);
        gtk_widget_set_sensitive (rfd->num_days, !active);
        get_filter_times (rfd);
    }
    else if (active && g_strcmp0 (name, "filter_show_days") == 0)
    {
        gtk_widget_set_sensitive (rfd->table, !active);
        gtk_widget_set_sensitive (rfd->num_days, active);
        fd->days = gtk_spin_button_get_value (GTK_SPIN_BUTTON(rfd->num_days));
    }
    else
    {
        gtk_widget_set_sensitive (rfd->table, FALSE);
        gtk_widget_set_sensitive (rfd->num_days, FALSE);
        set_filterdata_to_defaults (fd, true);
    }
    ppr_filter_update_date_query (rfd->plugin_page);
    LEAVE(" ");
}

/** This function is called when the "number of days" spin button is
 *  changed which is then saved and updates the time limitation on
 *  the register query. This is handled by a helper function as
 *  potentially all widgets will need to be examined.
 *
 *  @param button A pointer to the "number of days" spin button.
 *
 *  @param rfd A pointer to the filter dialog structure.
 */
void
gnc_ppr_filter_days_changed_cb (GtkSpinButton* button,
                                RegisterFilterDialog* rfd)
{
    g_return_if_fail (GTK_IS_SPIN_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rfd->plugin_page));

    ENTER("(button %p, page %p)", button, rfd->plugin_page);

    auto fd = gnc_plugin_page_register_get_filter_data (rfd->plugin_page);

    fd->days = gtk_spin_button_get_value (GTK_SPIN_BUTTON(button));
    ppr_filter_update_date_query (rfd->plugin_page);

    LEAVE(" ");
}

static void
ppr_filter_gde_changed_cb (GtkWidget* unused,
                           RegisterFilterDialog* rfd)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rfd->plugin_page));

    ENTER("(widget %s(%p), page %p)",
           gtk_buildable_get_buildable_id (GTK_BUILDABLE(unused)), unused, rfd->plugin_page);

    get_filter_times (rfd);
    ppr_filter_update_date_query (rfd->plugin_page);

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
 *  @param rfd A pointer to the filter dialog structure.
 */
void
gnc_ppr_filter_start_cb (GtkWidget* radio,
                         RegisterFilterDialog* rfd)
{
    g_return_if_fail (GTK_IS_CHECK_BUTTON(radio));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rfd->plugin_page));

    ENTER("(radio %s(%p), page %p)",
           gtk_buildable_get_buildable_id (GTK_BUILDABLE(radio)), radio, rfd->plugin_page);

    if (!gtk_check_button_get_active (GTK_CHECK_BUTTON(radio)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }

    auto name = gtk_buildable_get_buildable_id (GTK_BUILDABLE(radio));
    gboolean active = !g_strcmp0 (name, "start_date_choose");
    gtk_widget_set_sensitive (rfd->start_date, active);
    get_filter_times (rfd);
    ppr_filter_update_date_query (rfd->plugin_page);

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
 *  @param rfd A pointer to the filter dialog structure.
 */
void
gnc_ppr_filter_end_cb (GtkWidget* radio,
                       RegisterFilterDialog* rfd)
{
    g_return_if_fail (GTK_IS_CHECK_BUTTON(radio));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rfd->plugin_page));

    ENTER("(radio %s(%p), page %p)",
          gtk_buildable_get_buildable_id (GTK_BUILDABLE(radio)), radio, rfd->plugin_page);

    if (!gtk_check_button_get_active (GTK_CHECK_BUTTON(radio)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }

    auto name = gtk_buildable_get_buildable_id (GTK_BUILDABLE(radio));
    gboolean active = !g_strcmp0 (name, "end_date_choose");
    gtk_widget_set_sensitive (rfd->end_date, active);
    get_filter_times (rfd);
    ppr_filter_update_date_query (rfd->plugin_page);

    LEAVE(" ");
}

static void
ppr_filter_relative_changed_cb (GtkWidget *widget,
                                RegisterFilterDialog* rfd)
{
    g_return_if_fail (GNC_IS_PERIOD_SELECT(widget));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rfd->plugin_page));

    ENTER("Period Select (%p), active_id %d, plugin_page %p",
           widget, gnc_period_select_get_active (GNC_PERIOD_SELECT(widget)),
           rfd->plugin_page);

    get_filter_times (rfd);
    ppr_filter_update_date_query (rfd->plugin_page);

    LEAVE("  ");
}

/** This function is called when the "days ago" spin button is
 *  changed which is then saved and updates the time limitation on
 *  the register query. This is handled by a helper function as
 *  potentially all widgets will need to be examined.
 *
 *  @param button A pointer to the "days ago" spin button.
 *
 *  @param rfd A pointer to the filter dialog structure.
 */
void
gnc_ppr_filter_start_end_days_changed_cb (GtkSpinButton* button,
                                          RegisterFilterDialog* rfd)
{
    g_return_if_fail (GTK_IS_SPIN_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rfd->plugin_page));

    ENTER("(button %p, page %p)", button, rfd->plugin_page);

    auto fd = gnc_plugin_page_register_get_filter_data (rfd->plugin_page);

    auto name = gtk_buildable_get_buildable_id (GTK_BUILDABLE(button));

    if (g_strcmp0 (name, "start_days_spin") == 0)
        fd->start_days = gtk_spin_button_get_value (GTK_SPIN_BUTTON(button));

    if (g_strcmp0 (name, "end_days_spin") == 0)
        fd->end_days = gtk_spin_button_get_value (GTK_SPIN_BUTTON(button));

    get_filter_times (rfd);
    ppr_filter_update_date_query (rfd->plugin_page);

    LEAVE(" ");
}

/** This function is called when one of the check buttons for start
 *  relative or start date is changed. It activates the associated
 *  widget and deactivates the other.
 *
 *  @param button A pointer to a GtkCheckButton widget.
 *
 *  @param rfd A pointer to the filter dialog structure.
 */
void
gnc_ppr_filter_start_toggle_cb (GtkCheckButton* button,
                                RegisterFilterDialog* rfd)
{
    g_return_if_fail (GTK_IS_CHECK_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rfd->plugin_page));

    ENTER("Start toggle button (%p), plugin_page %p", button, rfd->plugin_page);

    auto name = gtk_buildable_get_buildable_id (GTK_BUILDABLE(button));

    gboolean active = gtk_check_button_get_active (button);

    gtk_widget_set_sensitive (rfd->start_earliest, !active);

    if (g_strcmp0 (name, "start_date_check") == 0)
    {
        set_sensitive_start_widget (rfd, rfd->start_date, active);

        set_checkbutton_with_blocking (rfd->start_relative_check,
                                       rfd->start_days_check,
                                       (GFunc)gnc_ppr_filter_start_toggle_cb,
                                       rfd, FALSE);
    }
    if (g_strcmp0 (name, "start_relative_check") == 0)
    {
        set_sensitive_start_widget (rfd, rfd->start_relative, active);

        set_checkbutton_with_blocking (rfd->start_date_check,
                                       rfd->start_days_check,
                                       (GFunc)gnc_ppr_filter_start_toggle_cb,
                                       rfd, FALSE);
    }
    if (g_strcmp0 (name, "start_days_check") == 0)
    {
        set_sensitive_start_widget (rfd, rfd->start_days, active);

        set_checkbutton_with_blocking (rfd->start_date_check,
                                       rfd->start_relative_check,
                                       (GFunc)gnc_ppr_filter_start_toggle_cb,
                                       rfd, FALSE);
    }
    get_filter_times (rfd);
    ppr_filter_update_date_query (rfd->plugin_page);

    LEAVE(" ");
}

/** This function is called when one of the check buttons for end
 *  relative or end date is changed. It activates the associated
 *  widget and deactivates the other.
 *
 *  @param button A pointer to a GtkCheckButton widget.
 *
 *  @param rfd A pointer to the filter dialog structure.
 */
void
gnc_ppr_filter_end_toggle_cb (GtkCheckButton* button,
                              RegisterFilterDialog* rfd)
{
    g_return_if_fail (GTK_IS_CHECK_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rfd->plugin_page));

    ENTER("End toggle button (%p), plugin_page %p", button, rfd->plugin_page);

    auto name = gtk_buildable_get_buildable_id (GTK_BUILDABLE(button));

    gboolean active = gtk_check_button_get_active (button);

    gtk_widget_set_sensitive (rfd->end_latest, !active);

    if (g_strcmp0 (name, "end_date_check") == 0)
    {
        set_sensitive_end_widget (rfd, rfd->end_date, active);

        set_checkbutton_with_blocking (rfd->end_relative_check,
                                       rfd->end_days_check,
                                       (GFunc)gnc_ppr_filter_end_toggle_cb,
                                       rfd, FALSE);
    }
    if (g_strcmp0 (name, "end_relative_check") == 0)
    {
        set_sensitive_end_widget (rfd, rfd->end_relative, active);

        set_checkbutton_with_blocking (rfd->end_date_check,
                                       rfd->end_days_check,
                                       (GFunc)gnc_ppr_filter_end_toggle_cb,
                                       rfd, FALSE);
    }
    if (g_strcmp0 (name, "end_days_check") == 0)
    {
        set_sensitive_end_widget (rfd, rfd->end_days, active);

        set_checkbutton_with_blocking (rfd->end_date_check,
                                       rfd->end_relative_check,
                                       (GFunc)gnc_ppr_filter_end_toggle_cb,
                                       rfd, FALSE);
    }
    get_filter_times (rfd);
    ppr_filter_update_date_query (rfd->plugin_page);

    LEAVE(" ");
}

/** This function is called whenever the save status is checked
 *  or unchecked. It will allow saving of the filter if required.
 *
 *  @param button The toggle button that was changed.
 *
 *  @param rfd A pointer to the filter dialog structure.
 */
void
gnc_ppr_filter_save_cb (GtkCheckButton* button,
                        RegisterFilterDialog* rfd)
{
    g_return_if_fail (GTK_IS_CHECK_BUTTON(button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(rfd->plugin_page));

    ENTER("Save toggle button (%p), plugin_page %p", button, rfd->plugin_page);

    auto fd = gnc_plugin_page_register_get_filter_data (rfd->plugin_page);

    /* Compute the new save filter status */
    if (gtk_check_button_get_active (button))
        fd->save_filter = true;
    else
        fd->save_filter = false;

    LEAVE(" ");
}

static RegisterFilterDialog *
ppr_filter_dialog_ref (RegisterFilterDialog *rfd)
{
    rfd->references++;
    return rfd;
}

static void
ppr_filter_dialog_unref (RegisterFilterDialog *rfd)
{
    g_return_if_fail (rfd->references > 0);

    if (--rfd->references)
        return;

    g_clear_object (&rfd->plugin_page);
    g_free (rfd);
}

static void
ppr_filter_dialog_restore (RegisterFilterDialog *rfd)
{
    auto fd = gnc_plugin_page_register_get_filter_data (rfd->plugin_page);

    /* Remove the changed status match before restoring the original value. */
    fd->cleared_match = rfd->original_cleared_match;
    gnc_plugin_register_set_enable_refresh (GNC_PLUGIN_PAGE_REGISTER (rfd->plugin_page),
                                            FALSE);
    ppr_filter_update_status_query (rfd->plugin_page);
    gnc_plugin_register_set_enable_refresh (GNC_PLUGIN_PAGE_REGISTER (rfd->plugin_page),
                                            TRUE);

    fd->start_ap = rfd->original_start_ap;
    fd->start_time = rfd->original_start_time;
    fd->start_days = rfd->original_start_days;
    fd->end_ap = rfd->original_end_ap;
    fd->end_time = rfd->original_end_time;
    fd->end_days = rfd->original_end_days;
    fd->days = rfd->original_days;
    fd->save_filter = rfd->original_save_filter;
    ppr_filter_update_date_query (rfd->plugin_page);
}

static void
ppr_filter_dialog_complete (RegisterFilterDialog *rfd, bool accepted,
                            bool destroy_window)
{
    GtkWidget *dialog;
    auto fd = gnc_plugin_page_register_get_filter_data (rfd->plugin_page);
    auto gsr = gnc_plugin_page_register_get_gsr (rfd->plugin_page);

    if (rfd->completed)
        return;

    rfd->completed = true;
    rfd->validation_pending = false;
    dialog = rfd->dialog;
    rfd->dialog = nullptr;
    fd->dialog = nullptr;

    if (!accepted)
    {
        ppr_filter_dialog_restore (rfd);
    }
    else
    {
        /* Clear a saved filter when saving was explicitly disabled. */
        if (!fd->save_filter && rfd->original_save_filter)
            ppr_filter_save_filter (gsr, "");

        if (fd->save_filter)
            ppr_filter_save_filter_parts (gsr, fd);
    }

    if (destroy_window && dialog)
        gtk_window_destroy (GTK_WINDOW (dialog));
}

static void
ppr_filter_invalid_date_finished (GtkWindow *parent, gint response,
                                  gpointer user_data)
{
    auto rfd = static_cast<RegisterFilterDialog *> (user_data);

    if (!rfd->completed)
    {
        rfd->validation_pending = false;
        if (response == GTK_RESPONSE_OK && parent && rfd->dialog)
            ppr_filter_dialog_complete (rfd, true, true);
        else if (parent && rfd->dialog)
        {
            gtk_widget_set_sensitive (rfd->dialog, TRUE);
            gtk_window_present (GTK_WINDOW (rfd->dialog));
        }
    }

    ppr_filter_dialog_unref (rfd);
}

static void
ppr_filter_dialog_accept_clicked (GtkButton *button, RegisterFilterDialog *rfd)
{
    auto fd = gnc_plugin_page_register_get_filter_data (rfd->plugin_page);

    (void)button;
    if (rfd->completed || rfd->validation_pending)
        return;

    if (fd->start_time > 0 && fd->end_time > 0 && fd->start_time > fd->end_time)
    {
        rfd->validation_pending = true;
        gtk_widget_set_sensitive (rfd->dialog, FALSE);
        gnc_ok_cancel_dialog_async (
            GTK_WINDOW (rfd->dialog), GTK_RESPONSE_CANCEL,
            ppr_filter_invalid_date_finished, ppr_filter_dialog_ref (rfd),
            "%s", _("The Start date is after the End date.\n"
                    "Select Cancel to change dates.\n"));
        return;
    }

    ppr_filter_dialog_complete (rfd, true, true);
}

static void
ppr_filter_dialog_cancel_clicked (GtkButton *button, RegisterFilterDialog *rfd)
{
    (void)button;
    if (!rfd->validation_pending)
        ppr_filter_dialog_complete (rfd, false, true);
}

static gboolean
ppr_filter_dialog_close_request (GtkWindow *window, RegisterFilterDialog *rfd)
{
    (void)window;
    if (!rfd->validation_pending)
        ppr_filter_dialog_complete (rfd, false, true);
    return TRUE;
}

static gboolean
ppr_filter_dialog_escape (GtkWidget *widget, GVariant *args, gpointer user_data)
{
    auto rfd = static_cast<RegisterFilterDialog *> (user_data);

    (void)widget;
    (void)args;
    if (!rfd->validation_pending)
        ppr_filter_dialog_complete (rfd, false, true);
    return TRUE;
}

static void
ppr_filter_dialog_destroyed (GtkWidget *widget, RegisterFilterDialog *rfd)
{
    (void)widget;
    rfd->dialog = nullptr;
    if (!rfd->completed)
        ppr_filter_dialog_complete (rfd, false, false);
    ppr_filter_dialog_unref (rfd);
}

static void
ppr_filter_dialog_add_shortcuts (GtkWindow *dialog, RegisterFilterDialog *rfd)
{
    auto controller = GTK_SHORTCUT_CONTROLLER (gtk_shortcut_controller_new ());

    gtk_shortcut_controller_set_scope (controller, GTK_SHORTCUT_SCOPE_MANAGED);
    gtk_shortcut_controller_add_shortcut (
        controller,
        gtk_shortcut_new (
            gtk_keyval_trigger_new (GDK_KEY_Escape, GDK_NO_MODIFIER_MASK),
            gtk_callback_action_new (ppr_filter_dialog_escape, rfd, NULL)));
    gtk_widget_add_controller (GTK_WIDGET (dialog), GTK_EVENT_CONTROLLER (controller));
}

static GtkWidget *
setup_period_select (GtkBuilder *builder, gboolean start_type, const gchar *hbox_txt)
{
    GtkWidget *period_select = GTK_WIDGET(gnc_period_select_new (start_type));

    auto hbox = GTK_WIDGET(gtk_builder_get_object (builder, hbox_txt));
    gnc_box_append_full (GTK_BOX(hbox), period_select, TRUE, TRUE, 0);
    gtk_widget_set_visible (period_select, TRUE);
    gnc_period_select_set_active (GNC_PERIOD_SELECT(period_select), GNC_ACCOUNTING_PERIOD_TODAY);
    gtk_widget_set_sensitive (GTK_WIDGET(period_select), FALSE);
    return period_select;
}

static GtkWidget *
setup_date_edit (GtkBuilder *builder, const gchar *hbox_txt)
{
    GtkWidget *date_widget = gnc_date_edit_new (gnc_time (nullptr), FALSE, FALSE);
    auto hbox = GTK_WIDGET(gtk_builder_get_object (builder, hbox_txt));
    gnc_box_append_full (GTK_BOX(hbox), date_widget, TRUE, TRUE, 0);
    gtk_widget_set_visible (date_widget, TRUE);
    gtk_widget_set_sensitive (GTK_WIDGET(date_widget), FALSE);
    return date_widget;
}

static void
ppr_filter_dialog_create (RegisterFilterDialog* rfd, FilterData *fd, Query *query)
{
    GtkWidget *cancel_button;
    GtkWidget *ok_button;
    time64 start_time, end_time, time_val;

    /* Create the dialog */
    auto builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "gnc-plugin-page-register.glade", "days_adjustment");
    gnc_builder_add_from_file (builder, "gnc-plugin-page-register.glade", "start_days_adjustment");
    gnc_builder_add_from_file (builder, "gnc-plugin-page-register.glade", "end_days_adjustment");
    gnc_builder_add_from_file (builder, "gnc-plugin-page-register.glade", "filter_by_dialog");
    auto dialog = GTK_WIDGET(gtk_builder_get_object (builder, "filter_by_dialog"));
    rfd->dialog = dialog;
    fd->dialog = rfd->dialog;

    gtk_window_set_transient_for (GTK_WINDOW(dialog),
                                  gnc_window_get_gtk_window (GNC_WINDOW(
                                      GNC_PLUGIN_PAGE(rfd->plugin_page)->window)));

    /* Translators: The %s is the name of the plugin page */
    auto title = g_strdup_printf (_ ("Filter %s by…"),
                     gnc_plugin_page_get_page_name (rfd->plugin_page));
    gtk_window_set_title (GTK_WINDOW(dialog), title);
    g_free (title);

    /* Set the check buttons for the current status */
    for (auto& action : status_actions)
    {
        auto toggle = GTK_WIDGET(gtk_builder_get_object (builder,
                                                         action.action_name.c_str()));
        bool value = fd->cleared_match & action.value;
        action.widget = toggle;
        gtk_check_button_set_active (GTK_CHECK_BUTTON(toggle), bool_to_gboolean (value));
    }
    rfd->original_cleared_match = fd->cleared_match;

    auto button = GTK_WIDGET(gtk_builder_get_object (builder, "filter_save"));
    if (fd->save_filter)
        gtk_check_button_set_active (GTK_CHECK_BUTTON(button), TRUE);

    rfd->original_save_filter = fd->save_filter;

    // hide the save button if appropriate
    gtk_widget_set_visible (GTK_WIDGET(button), bool_to_gboolean (rfd->show_save_button));

    /* Set up number of days */
    rfd->num_days = GTK_WIDGET(gtk_builder_get_object (builder, "filter_show_num_days"));
    button = GTK_WIDGET(gtk_builder_get_object (builder, "filter_show_days"));

    if (fd->days > 0) // using number of days
    {
        gtk_check_button_set_active (GTK_CHECK_BUTTON(button), TRUE);
        gtk_widget_set_sensitive (GTK_WIDGET(rfd->num_days), TRUE);
        gtk_spin_button_set_value (GTK_SPIN_BUTTON(rfd->num_days), fd->days);
        rfd->original_days = fd->days;

        /* Set the start_time and end_time to 0 */
        start_time = 0;
        end_time = 0;
    }
    else
    {
        gtk_widget_set_sensitive (GTK_WIDGET(rfd->num_days), FALSE);
        rfd->original_days = 0;
        fd->days = 0;

        /* Get the start and end times */
        xaccQueryGetDateMatchTT (query, &start_time, &end_time);
    }

    /* Set the date info */
    rfd->original_start_time = start_time;
    fd->start_time = start_time;
    rfd->original_end_time = end_time;
    fd->end_time = end_time;

    button = GTK_WIDGET(gtk_builder_get_object (builder, "filter_show_range"));
    gtk_check_button_set_active (GTK_CHECK_BUTTON(button), start_time || end_time);
    auto table = GTK_WIDGET(gtk_builder_get_object (builder, "select_range_table"));
    rfd->table = table;
    gtk_widget_set_sensitive (GTK_WIDGET(table), start_time || end_time);

    rfd->start_earliest = GTK_WIDGET(gtk_builder_get_object (builder, "earliest_label"));
    rfd->start_date_check = GTK_WIDGET(gtk_builder_get_object (builder, "start_date_check"));
    rfd->start_relative_check = GTK_WIDGET(gtk_builder_get_object (builder, "start_relative_check"));
    rfd->start_days_check = GTK_WIDGET(gtk_builder_get_object (builder, "start_days_check"));

    {
        rfd->start_relative = setup_period_select (builder, TRUE, "start_relative_hbox");
        rfd->start_date = setup_date_edit (builder, "start_date_hbox");
        rfd->start_days = GTK_WIDGET(gtk_builder_get_object (builder, "start_days_spin"));

        /* Start date info */
        if (start_time == 0)
        {
            set_sensitive_start_widget (rfd, rfd->start_earliest, TRUE);
            time_val = xaccQueryGetEarliestDateFound (query);
        }
        else
        {
            rfd->original_start_ap = fd->start_ap;
            if (fd->start_ap != GNC_ACCOUNTING_PERIOD_INVALID)
            {
                set_sensitive_start_widget (rfd, rfd->start_relative, TRUE);
                gnc_period_select_set_active (GNC_PERIOD_SELECT(rfd->start_relative), fd->start_ap);
                gtk_check_button_set_active (GTK_CHECK_BUTTON(rfd->start_relative_check), TRUE);
            }
            else if (fd->start_days != 0)
            {
                set_sensitive_start_widget (rfd, rfd->start_days, TRUE);
                gtk_check_button_set_active (GTK_CHECK_BUTTON(rfd->start_days_check), TRUE);
                gtk_spin_button_set_value (GTK_SPIN_BUTTON(rfd->start_days), fd->start_days);
                rfd->original_start_days = fd->start_days;

            }
            else
            {
                set_sensitive_start_widget (rfd, rfd->start_date, TRUE);
                gtk_check_button_set_active (GTK_CHECK_BUTTON(rfd->start_date_check), TRUE);
            }
            time_val = start_time;
        }
        g_signal_connect (G_OBJECT(rfd->start_relative), "changed",
                          G_CALLBACK(ppr_filter_relative_changed_cb), rfd);

        if (time_val == 0)
            time_val = gnc_time64_get_today_start();
        gnc_date_edit_set_time (GNC_DATE_EDIT(rfd->start_date), time_val);
        g_signal_connect (G_OBJECT(rfd->start_date), "date-changed",
                          G_CALLBACK(ppr_filter_gde_changed_cb), rfd);
    }

    rfd->end_latest = GTK_WIDGET(gtk_builder_get_object (builder, "latest_label"));
    rfd->end_date_check = GTK_WIDGET(gtk_builder_get_object (builder, "end_date_check"));
    rfd->end_relative_check = GTK_WIDGET(gtk_builder_get_object (builder, "end_relative_check"));
    rfd->end_days_check = GTK_WIDGET(gtk_builder_get_object (builder, "end_days_check"));

    {
        rfd->end_relative = setup_period_select (builder, FALSE, "end_relative_hbox");
        rfd->end_date = setup_date_edit (builder, "end_date_hbox");
        rfd->end_days = GTK_WIDGET(gtk_builder_get_object (builder, "end_days_spin"));

        /* End date info */
        if (end_time == 0)
        {
            set_sensitive_end_widget (rfd, rfd->end_latest, TRUE);
            time_val = xaccQueryGetLatestDateFound (query);
        }
        else
        {
            rfd->original_end_ap = fd->end_ap;
            if (fd->end_ap != GNC_ACCOUNTING_PERIOD_INVALID)
            {
                set_sensitive_end_widget (rfd, rfd->end_relative, TRUE);
                gnc_period_select_set_active (GNC_PERIOD_SELECT(rfd->end_relative), fd->end_ap);
                gtk_check_button_set_active (GTK_CHECK_BUTTON(rfd->end_relative_check), TRUE);
            }
            else if (fd->end_days != 0)
            {
                set_sensitive_end_widget (rfd, rfd->end_days, TRUE);
                gtk_check_button_set_active (GTK_CHECK_BUTTON(rfd->end_days_check), TRUE);
                gtk_spin_button_set_value (GTK_SPIN_BUTTON(rfd->end_days), fd->end_days);
                rfd->original_end_days = fd->end_days;
            }
            else
            {
                set_sensitive_end_widget (rfd, rfd->end_date, TRUE);
                gtk_check_button_set_active (GTK_CHECK_BUTTON(rfd->end_date_check), TRUE);
            }
            time_val = end_time;
        }
        g_signal_connect (G_OBJECT(rfd->end_relative), "changed",
                          G_CALLBACK(ppr_filter_relative_changed_cb), rfd);

        if (time_val == 0)
            time_val = gnc_time64_get_today_end();
        gnc_date_edit_set_time (GNC_DATE_EDIT(rfd->end_date), time_val);
        g_signal_connect (G_OBJECT(rfd->end_date), "date-changed",
                          G_CALLBACK(ppr_filter_gde_changed_cb), rfd);
    }

    /* Wire the control callbacks declared in the builder and the explicit
     * window lifecycle callbacks here. */
    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, rfd);
    cancel_button = GTK_WIDGET (gtk_builder_get_object (builder, "cancelbutton4"));
    ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "okbutton4"));
    g_signal_connect (cancel_button, "clicked",
                      G_CALLBACK (ppr_filter_dialog_cancel_clicked), rfd);
    g_signal_connect (ok_button, "clicked",
                      G_CALLBACK (ppr_filter_dialog_accept_clicked), rfd);
    g_signal_connect (dialog, "close-request",
                      G_CALLBACK (ppr_filter_dialog_close_request), rfd);
    g_signal_connect (dialog, "destroy",
                      G_CALLBACK (ppr_filter_dialog_destroyed), rfd);
    ppr_filter_dialog_add_shortcuts (GTK_WINDOW (dialog), rfd);
    gtk_window_set_default_widget (GTK_WINDOW (dialog), ok_button);

    /* Show it */
    gtk_window_present (GTK_WINDOW (dialog));
    g_object_unref (G_OBJECT(builder));
    LEAVE (" ");
}

/** This function is called for the filter dialog.
 *
 *  @param plugin_page  A pointer to the GncPluginPageRegister that is
 *  associated with this filter dialog.
 *
 *  @param query A pointer to the current register query.
 *
 *  @param fd A pointer to the filter data structure for remembering state.
 *
 *  @param show_save_button Set to True to show save button.
 */
void
gnc_ppr_filter_by (GncPluginPage *plugin_page, Query *query,
                   FilterData *fd, bool show_save_button)
{
    RegisterFilterDialog *rfd;

    ENTER(" ");

    rfd = g_new0 (RegisterFilterDialog, 1);

    rfd->references = 1;
    rfd->plugin_page = GNC_PLUGIN_PAGE (g_object_ref (plugin_page));
    rfd->show_save_button = show_save_button;

    ppr_filter_dialog_create (rfd, fd, query);

    LEAVE(" ");
}
