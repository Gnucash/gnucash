/*
 * dialog-query-list.h -- a simple dialog to display a querylist and
 *                        allow users to select items (or close the list)
 *
 * Created By:	Derek Atkins <derek@ihtfp.com>
 * Copyright (c) 2003 Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef GNC_DIALOG_QUERY_LIST_H
#define GNC_DIALOG_QUERY_LIST_H

#include "Query.h"

typedef struct _DialogQueryList DialogQueryList;

typedef void (*GNCDisplayListCB)(gpointer obj, gpointer user_data);
typedef struct
{
    const char *		label;
    GNCDisplayListCB	cb_fcn;
} GNCDisplayListButton;

DialogQueryList *
gnc_dialog_query_list_new (GList *param_list, Query *q);

void gnc_dialog_query_list_set_title (DialogQueryList *dql, const char *title);
void gnc_dialog_query_list_set_label (DialogQueryList *dql, const char *label);
void gnc_dialog_query_list_set_buttons (DialogQueryList *dql,
                                        GNCDisplayListButton *buttons,
                                        gpointer user_data);
void gnc_dialog_query_list_set_numerics (DialogQueryList *dql, gboolean abs,
        gboolean inv_sort);

void gnc_dialog_query_list_refresh (DialogQueryList *dql);
void gnc_dialog_query_list_destroy (DialogQueryList *dql);

DialogQueryList *
gnc_dialog_query_list_create (GList *param_list, Query *q,
                              const char *title, const char *label,
                              gboolean abs, gboolean inv_sort,
                              GNCDisplayListButton *buttons, gpointer user_data);


#endif /* GNC_DIALOG_QUERY_LIST_H */
