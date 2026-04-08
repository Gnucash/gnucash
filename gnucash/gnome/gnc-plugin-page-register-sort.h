/**********************************************************************
 * gnc-plugin-page-register-sort.h -- register page sort              *
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
/** @addtogroup RegisterPlugin Register Page Sort
    @{ */
/** @file gnc-plugin-page-register-filter.h
    @brief  Functions providing a register page sort for the GnuCash UI
    @author Copyright (C) 2026 Bob Fewell
*/

#ifndef __GNC_PLUGIN_PAGE_REGISTER_SORT_H
#define __GNC_PLUGIN_PAGE_REGISTER_SORT_H

#include <gtk/gtk.h>
#include "gnc-split-reg.h"
#include "gnc-plugin-page.h"

#ifdef __cplusplus
extern "C"
{
#endif

typedef struct sort_data
{
    GtkWidget* dialog;
    GtkWidget* num_radio;
    GtkWidget* act_radio;
    SortType   original_sort_type;
    gboolean   original_save_order;
    gboolean   save_order;
    gboolean   reverse_order;
    gboolean   original_reverse_order;
} SortData;

#define DEFAULT_SORT_ORDER "BY_STANDARD"

gboolean gnc_ppr_sort_get_reversed (GNCSplitReg *gsr);
void gnc_ppr_sort_set_reversed (GNCSplitReg* gsr, gboolean reverse_order);

gchar* gnc_ppr_sort_get_order (GNCSplitReg *gsr);
void gnc_ppr_sort_set_order (GNCSplitReg *gsr, const gchar* sort_order);

void gnc_ppr_sort_dialog (GncPluginPage *plugin_page, SplitRegister* reg,
                          SortData *sd, gboolean show_save_button);

#ifdef __cplusplus
}
#endif

#endif /* __GNC_PLUGIN_PAGE_REGISTER_SORT_H */
