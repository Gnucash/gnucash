/********************************************************************\
 * gnc-query-list.h -- GnuCash GNOME query display list widget      *
 * Copyright (C) 2003 Derek Atkins <derek@ihtfp.com>                *
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

#ifndef GNC_QUERY_LIST_H
#define GNC_QUERY_LIST_H

#include <gtk/gtk.h>

#include "Query.h"

#ifdef __cplusplus
extern "C" {
#endif				/* __cplusplus */

#define GNC_TYPE_QUERY_LIST (gnc_query_list_get_type ())
#define GNC_QUERY_LIST(obj) (GTK_CHECK_CAST ((obj), GNC_TYPE_QUERY_LIST, GNCQueryList))
#define GNC_QUERY_LIST_CLASS(klass) (GTK_CHECK_CLASS_CAST ((klass), GNC_TYPE_QUERY_LIST, GNCQueryListClass))
#define IS_GNC_QUERY_LIST(obj) (GTK_CHECK_TYPE ((obj), GNC_TYPE_QUERY_LIST))
#define IS_GNC_QUERY_LIST_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GNC_TYPE_QUERY_LIST))

    typedef struct _GNCQueryList      GNCQueryList;
    typedef struct _GNCQueryListClass GNCQueryListClass;

    struct _GNCQueryList
    {
        GtkCList clist;

        /* Query information */
        Query *query;
        gboolean no_toggle;
        gboolean always_unselect;
        gint current_row;
        gint num_entries;
        gpointer current_entry;

        /* Column information */
        gint num_columns;
        GList *column_params;

        /* numeric information */
        gboolean numeric_abs;
        gboolean numeric_inv_sort;

        /* Sorting info */
        gint sort_column;
        gboolean increasing;
        GtkWidget **title_arrows;

        /* Column resizing */
        gint prev_allocation;
        gint *title_widths;
    };

    struct _GNCQueryListClass
    {
        GtkCListClass clist_class;

        void (*line_toggled) (GNCQueryList *list, gpointer entry);
        void (*double_click_entry) (GNCQueryList *list, gpointer entry);
    };

    /***********************************************************
     *                public functions                         *
     ***********************************************************/

    GType gnc_query_list_get_type (void);

    /* The param_list remains owned by the caller but is used by the
     * query-list; do not destroy it until you destroy this query-list.
     * The query will be copied by the query-list so the caller may do
     * whatever they want.
     */
    GtkWidget * gnc_query_list_new (GList *param_list, Query *query);
    void gnc_query_list_construct (GNCQueryList *list, GList *param_list, Query *query);
    void gnc_query_list_reset_query (GNCQueryList *list, Query *query);

    void gnc_query_list_set_numerics (GNCQueryList *list, gboolean abs, gboolean inv_sort);

    gint gnc_query_list_get_needed_height(GNCQueryList *list, gint num_rows);

    gint gnc_query_list_get_num_entries(GNCQueryList *list);

    gpointer gnc_query_list_get_current_entry(GNCQueryList *list);

    void gnc_query_list_refresh (GNCQueryList *list);

    void gnc_query_list_unselect_all(GNCQueryList *list);

    gboolean gnc_query_list_item_in_list (GNCQueryList *list, gpointer item);

    void gnc_query_list_refresh_item (GNCQueryList *list, gpointer item);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* GNC_QUERY_LIST_H */
