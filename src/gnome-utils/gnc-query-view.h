/********************************************************************\
 * gnc-query-view.h -- GnuCash GNOME query display view widget      *
 * Copyright (C) 2003 Derek Atkins <derek@ihtfp.com>                *
 * Copyright (C) 2012 Robert Fewell                                 *
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

#ifndef GNC_QUERY_VIEW_H
#define GNC_QUERY_VIEW_H

#include <gtk/gtk.h>

#include "Query.h"

#ifdef __cplusplus
extern "C" {
#endif				/* __cplusplus */

#define GNC_TYPE_QUERY_VIEW            (gnc_query_view_get_type ())
#define GNC_QUERY_VIEW(obj)            G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_QUERY_VIEW, GNCQueryView)
#define GNC_QUERY_VIEW_CLASS(klass)    G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_QUERY_VIEW, GNCQueryViewClass)
#define GNC_IS_QUERY_VIEW(obj)         G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_QUERY_VIEW)
#define GNC_IS_QUERY_VIEW_CLASS(klass) G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_QUERY_VIEW)

    typedef struct _GNCQueryView      GNCQueryView;
    typedef struct _GNCQueryViewClass GNCQueryViewClass;

    struct _GNCQueryView
    {
        GtkTreeView qview;

        /* Query information */
        Query      *query;
        gint        num_entries;

        /* Select information */
        gpointer    selected_entry;
        GList      *selected_entry_list;
        gint        toggled_row;
        gint        toggled_column;

        /* Column information */
        gint        num_columns;
        GList      *column_params;

        /* numeric information */
        gboolean    numeric_abs;
        gboolean    numeric_inv_sort;

        /* Sorting info */
        gint        sort_column;
        gboolean    increasing;
    };

    struct _GNCQueryViewClass
    {
        GtkTreeViewClass view_class;

        /* This signal is emitted when a toggle happens, the pointer has
           an interger value for the active setting of the toggle */
        void (*column_toggled) (GNCQueryView *qview, gpointer item);

        /* This signal is emitted when a row is selected, the pointer has
           an interger value for the number of rows selected */
        void (*row_selected) (GNCQueryView *qview, gpointer item);

        /* This signal is emitted when a row is double clicked, the pointer has
           a pointer to the entry */
        void (*double_click_entry) (GNCQueryView *qview, gpointer entry);
    };

    /***********************************************************
     *                public functions                         *
     ***********************************************************/

    GType gnc_query_view_get_type (void);

    /* The param_list remains owned by the caller but is used by the
     * query-view; do not destroy it until you destroy this query-view.
     * The query will be copied by the query-view so the caller may do
     * whatever they want.
     */
    GtkWidget * gnc_query_view_new (GList *param_list, Query *query);

    void gnc_query_view_construct (GNCQueryView *qview, GList *param_list, Query *query);

    void gnc_query_view_reset_query (GNCQueryView *view, Query *query);

    void gnc_query_view_set_numerics (GNCQueryView *qview, gboolean abs, gboolean inv_sort);

    gint gnc_query_view_get_num_entries (GNCQueryView *qview);

    gpointer gnc_query_view_get_selected_entry (GNCQueryView *qview);

    GList * gnc_query_view_get_selected_entry_list (GNCQueryView *qview);

    void gnc_query_view_refresh (GNCQueryView *qview);

    void gnc_query_view_unselect_all (GNCQueryView *qview);

    gboolean gnc_query_view_item_in_view (GNCQueryView *qview, gpointer item);

    void gnc_query_sort_order (GNCQueryView *qview, gint column, GtkSortType order);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* GNC_QUERY_VIEW_H */
