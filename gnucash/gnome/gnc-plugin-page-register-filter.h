/**********************************************************************
 * gnc-plugin-page-register-filter.h -- register page filter          *
 *                                                                    *
 * Copyright (C) 2026 Bob Fewell                                      *
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
/** @file gnc-plugin-page-register-filter.h
    @brief  Functions providing a register page filter for the GnuCash UI
    @author Copyright (C) 2026 Bob Fewell
*/

#ifndef __GNC_PLUGIN_PAGE_REGISTER_FILTER_H
#define __GNC_PLUGIN_PAGE_REGISTER_FILTER_H

#include <gtk/gtk.h>
#include "gnc-split-reg.h"
#include "gnc-plugin-page.h"

#ifdef __cplusplus
extern "C"
{
#endif

typedef struct filter_data
{
    GtkWidget* dialog;
    GtkWidget* table;
    GtkWidget* start_date_choose;
    GtkWidget* start_date_today;
    GtkWidget* start_date;
    GtkWidget* end_date_choose;
    GtkWidget* end_date_today;
    GtkWidget* end_date;
    GtkWidget* num_days;
    cleared_match_t original_cleared_match;
    cleared_match_t cleared_match;
    time64 original_start_time;
    time64 original_end_time;
    time64 start_time;
    time64 end_time;
    gint days;
    gint original_days;
    gboolean original_save_filter;
    gboolean save_filter;
}FilterData;

void gnc_ppr_filter_set_tooltip (GncPluginPage* plugin_page, FilterData *fd);

void gnc_ppr_filter_clear_current_filter (GncPluginPage* plugin_page);

void gnc_ppr_filter_update_register (GncPluginPage* plugin_page);

void gnc_ppr_filter_by (GncPluginPage *plugin_page, Query *query,
                        FilterData *fd, gboolean show_save_button);

#ifdef __cplusplus
}
#endif

#endif /* __GNC_PLUGIN_PAGE_REGISTER_FILTER_H */
