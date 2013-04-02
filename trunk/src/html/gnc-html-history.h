/********************************************************************
 * gnc-html-history.h -- keep a HTML history                        *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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
 ********************************************************************/

#ifndef GNC_HTML_HISTORY_H
#define GNC_HTML_HISTORY_H

typedef struct _gnc_html_history_node gnc_html_history_node;
typedef struct _gnc_html_history gnc_html_history;

#include "gnc-html.h"

struct _gnc_html_history_node
{
    URLType type;
    gchar   * location;
    gchar   * label;
};

typedef void (* gnc_html_history_destroy_cb)(gnc_html_history_node * n,
        gpointer user_data);

gnc_html_history      * gnc_html_history_new(void);
void                    gnc_html_history_destroy(gnc_html_history * hist);

void                    gnc_html_history_append(gnc_html_history * h,
        gnc_html_history_node * n);
gnc_html_history_node * gnc_html_history_get_current(gnc_html_history * h);
gnc_html_history_node * gnc_html_history_forward(gnc_html_history * h);
gnc_html_history_node * gnc_html_history_back(gnc_html_history * h);
int                     gnc_html_history_forward_p(gnc_html_history * h);
int                     gnc_html_history_back_p(gnc_html_history * h);
void  gnc_html_history_set_node_destroy_cb(gnc_html_history * h,
        gnc_html_history_destroy_cb cb,
        gpointer cb_data);

gnc_html_history_node * gnc_html_history_node_new(URLType type,
        const gchar * location,
        const gchar * label);

void                    gnc_html_history_node_destroy(gnc_html_history_node *
        node);


#endif


