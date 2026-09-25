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
extern "C"
{
#endif              /* __cplusplus */

#define GNC_TYPE_QUERY_VIEW            (gnc_query_view_get_type ())
#define GNC_QUERY_VIEW(obj)            G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_QUERY_VIEW, GNCQueryView)
#define GNC_QUERY_VIEW_CLASS(klass)    G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_QUERY_VIEW, GNCQueryViewClass)
#define GNC_IS_QUERY_VIEW(obj)         G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_QUERY_VIEW)
#define GNC_IS_QUERY_VIEW_CLASS(klass) G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_QUERY_VIEW)

typedef struct _GNCQueryView      GNCQueryView;
typedef struct _GNCQueryViewClass GNCQueryViewClass;

struct _GNCQueryView
{
    GtkBox parent_instance;

    /* Query information */
    Query      *query;

    /* Select information */
    gint        toggled_row;
    gint        toggled_column;
    gboolean    use_scroll_to_selection;

    /* Column information */
    gint        num_columns;
    GList      *column_params;

    /* numeric information */
    gboolean    numeric_abs;
    gboolean    numeric_inv_sort;

    /* Sorting info */
    gint        sort_column;
    gboolean    increasing;

    /* The last boolean-cell interaction. The signal still carries the
     * requested state; subclasses use this stable item identity. */
    gpointer    toggled_entry;
};

struct _GNCQueryViewClass
{
    GtkBoxClass parent_class;

    /* This signal is emitted when a toggle happens, the pointer has
       an integer value for the active setting of the toggle */
    void (*column_toggled) (GNCQueryView *qview, gpointer item);

    /* This signal is emitted when a row is selected, the pointer has
       an integer value for the number of rows selected */
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

/** Returns a list of selected entries in the query view.
 *  The returned GList should be freed by the caller */
GList * gnc_query_view_get_selected_entry_list (GNCQueryView *qview);

void gnc_query_view_refresh (GNCQueryView *qview);

void gnc_query_view_unselect_all (GNCQueryView *qview);

gboolean gnc_query_view_item_in_view (GNCQueryView *qview, gpointer item);

void gnc_query_sort_order (GNCQueryView *qview, gint column, GtkSortType order);

void gnc_query_set_expand_column (GNCQueryView *qview, gint column);

void gnc_query_scroll_to_selection (GNCQueryView *qview);

void gnc_query_force_scroll_to_selection (GNCQueryView *qview);

void gnc_query_use_scroll_to_selection (GNCQueryView *qview, gboolean scroll);

/* GTK4 selection and navigation helpers for direct consumers. */
void gnc_query_view_set_selection_mode (GNCQueryView *qview,
                                        GtkSelectionMode mode);
void gnc_query_view_select_first (GNCQueryView *qview);
void gnc_query_view_grab_focus (GNCQueryView *qview);
void gnc_query_view_select_entry (GNCQueryView *qview, gpointer entry,
                                  gboolean exclusive);
gboolean gnc_query_view_select_at_point (GNCQueryView *qview,
                                         double x, double y);
gpointer gnc_query_view_get_adjacent_entry (GNCQueryView *qview,
                                            gpointer entry,
                                            gboolean previous);
GList *gnc_query_view_get_entry_list (GNCQueryView *qview);

/* Reconciliation uses this for its derived Reconciled column. The compare
 * result is ordered in the active query direction. */
typedef gint (*GncQueryViewCompareFunc) (gpointer first, gpointer second,
                                         gpointer user_data);
void gnc_query_view_set_custom_sort_func (GNCQueryView *qview,
                                          GncQueryViewCompareFunc compare,
                                          gpointer user_data);

void gnc_query_view_set_column_ellipsize (GNCQueryView *qview, gint column,
                                          PangoEllipsizeMode mode,
                                          gboolean show_tooltip);
void gnc_query_view_add_column_padding (GNCQueryView *qview, gint column,
                                        gint xpadding);
gint gnc_query_view_get_column_width (GNCQueryView *qview, gint column);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* GNC_QUERY_VIEW_H */
