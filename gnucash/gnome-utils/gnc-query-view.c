/********************************************************************\
 * gnc-query-view.c -- A GTK4 query display view.                   *
 * Copyright (C) 2003 Derek Atkins <derek@ihtfp.com>                *
 * Copyright (C) 2012 Robert Fewell                                 *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
\********************************************************************/

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-component-manager.h"
#include "gnc-query-view.h"
#include "gnc-tree-view.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "search-param.h"

static QofLogModule log_module = GNC_MOD_GUI;

enum
{
    COLUMN_TOGGLED,
    ROW_SELECTED,
    DOUBLE_CLICK_ENTRY,
    LAST_SIGNAL
};

typedef struct
{
    gchar *text;
    gboolean boolean;
} GncQueryCell;

typedef struct _GncQueryRow GncQueryRow;
typedef struct _GncQueryRowClass GncQueryRowClass;

struct _GncQueryRow
{
    GObject parent_instance;
    gpointer entry;
    GPtrArray *cells;
};

struct _GncQueryRowClass
{
    GObjectClass parent_class;
};

GType gnc_query_row_get_type (void) G_GNUC_CONST;

G_DEFINE_TYPE (GncQueryRow, gnc_query_row, G_TYPE_OBJECT)

static void
query_cell_free (gpointer data)
{
    GncQueryCell *cell = data;

    if (!cell)
        return;
    g_free (cell->text);
    g_free (cell);
}

static void
gnc_query_row_finalize (GObject *object)
{
    GncQueryRow *row = (GncQueryRow *)object;

    g_clear_pointer (&row->cells, g_ptr_array_unref);
    G_OBJECT_CLASS (gnc_query_row_parent_class)->finalize (object);
}

static void
gnc_query_row_class_init (GncQueryRowClass *klass)
{
    G_OBJECT_CLASS (klass)->finalize = gnc_query_row_finalize;
}

static void
gnc_query_row_init (GncQueryRow *row)
{
    row->cells = g_ptr_array_new_with_free_func (query_cell_free);
}

static GncQueryRow *
gnc_query_row_new (gpointer entry, guint n_cells)
{
    GncQueryRow *row = g_object_new (gnc_query_row_get_type (), NULL);

    row->entry = entry;
    for (guint i = 0; i < n_cells; i++)
        g_ptr_array_add (row->cells, g_new0 (GncQueryCell, 1));
    return row;
}

static GncQueryCell *
gnc_query_row_cell (GncQueryRow *row, guint column)
{
    return row && column < row->cells->len ?
        g_ptr_array_index (row->cells, column) : NULL;
}

static void
gnc_query_row_set_text (GncQueryRow *row, guint column, const gchar *text)
{
    GncQueryCell *cell = gnc_query_row_cell (row, column);

    if (!cell)
        return;
    g_free (cell->text);
    cell->text = g_strdup (text ? text : "");
}

static void
gnc_query_row_set_boolean (GncQueryRow *row, guint column, gboolean value)
{
    GncQueryCell *cell = gnc_query_row_cell (row, column);

    if (cell)
        cell->boolean = value;
}

typedef struct
{
    GtkColumnViewColumn *column;
    gboolean boolean;
    gfloat xalign;
    PangoEllipsizeMode ellipsize;
    gboolean show_tooltip;
    gint padding;
} QueryColumnInfo;

typedef struct
{
    GNCQueryView *view;
    guint column;
} QueryColumnFactoryData;

typedef struct
{
    const QofParam *get_guid;
    gint component_id;
    GtkColumnView *view;
    GListStore *rows;
    GtkSortListModel *sorted_rows;
    GtkSelectionModel *selection;
    GtkSelectionMode selection_mode;
    GPtrArray *columns;
    GtkCustomSorter *custom_sorter;
    GtkSorter *view_sorter;
    gulong view_sorter_changed_id;
    gboolean syncing_sorter;
    GncQueryViewCompareFunc custom_compare;
    gpointer custom_compare_data;
} GNCQueryViewPrivate;

G_DEFINE_TYPE_WITH_PRIVATE (GNCQueryView, gnc_query_view, GTK_TYPE_BOX)

#define GNC_QUERY_VIEW_GET_PRIVATE(o) \
    ((GNCQueryViewPrivate *)gnc_query_view_get_instance_private (GNC_QUERY_VIEW (o)))

static guint query_view_signals[LAST_SIGNAL] = { 0 };

static void gnc_query_view_fill (GNCQueryView *qview);
static void gnc_query_view_set_query_sort (GNCQueryView *qview,
                                           gboolean new_column);

static void
query_column_info_free (gpointer data)
{
    QueryColumnInfo *info = data;

    if (!info)
        return;
    g_clear_object (&info->column);
    g_free (info);
}

static void
query_column_factory_data_free (gpointer data)
{
    g_free (data);
}

static QueryColumnInfo *
query_column_info_get (GNCQueryView *qview, guint column)
{
    GNCQueryViewPrivate *priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);

    return column < priv->columns->len ?
        g_ptr_array_index (priv->columns, column) : NULL;
}

/* ColumnView sorters provide native sortable headers and sort indicators.
 * QueryView continues to sort its data through QOF (or custom_sorter), so a
 * column sorter must describe every pair as equal and serve only as the UI
 * sorting affordance. */
static GtkOrdering
query_column_indicator_sort (gconstpointer first, gconstpointer second,
                             gpointer user_data)
{
    (void)first;
    (void)second;
    (void)user_data;
    return GTK_ORDERING_EQUAL;
}

static void
query_view_sync_sorter (GNCQueryView *qview)
{
    GNCQueryViewPrivate *priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    QueryColumnInfo *info;
    GtkColumnViewColumn *current;
    GtkSortType order;

    if (!priv->view || !priv->view_sorter || priv->syncing_sorter)
        return;
    info = query_column_info_get (qview, qview->sort_column);
    order = qview->increasing ? GTK_SORT_ASCENDING : GTK_SORT_DESCENDING;
    current = gtk_column_view_sorter_get_primary_sort_column
        (GTK_COLUMN_VIEW_SORTER (priv->view_sorter));

    if (!info || !gtk_column_view_column_get_sorter (info->column))
    {
        if (!current)
            return;
        priv->syncing_sorter = TRUE;
        gtk_column_view_sort_by_column (priv->view, NULL, order);
        priv->syncing_sorter = FALSE;
        return;
    }
    if (current == info->column &&
        gtk_column_view_sorter_get_primary_sort_order
        (GTK_COLUMN_VIEW_SORTER (priv->view_sorter)) == order)
        return;

    priv->syncing_sorter = TRUE;
    gtk_column_view_sort_by_column (priv->view, info->column, order);
    priv->syncing_sorter = FALSE;
}

static GncQueryRow *
query_row_at (GNCQueryView *qview, guint position)
{
    GNCQueryViewPrivate *priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);

    return (GncQueryRow *)g_list_model_get_item (G_LIST_MODEL (priv->selection), position);
}

static gint
query_find_entry (GNCQueryView *qview, gpointer entry)
{
    GNCQueryViewPrivate *priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    guint count = g_list_model_get_n_items (G_LIST_MODEL (priv->selection));

    for (guint position = 0; position < count; position++)
    {
        GncQueryRow *row = query_row_at (qview, position);
        gboolean found = row && row->entry == entry;

        g_clear_object (&row);
        if (found)
            return (gint)position;
    }
    return -1;
}

static guint
query_selection_count (GNCQueryView *qview)
{
    GNCQueryViewPrivate *priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    GtkBitset *selected = gtk_selection_model_get_selection (priv->selection);
    guint count = gtk_bitset_get_size (selected);

    gtk_bitset_unref (selected);
    return count;
}

static GtkOrdering
query_custom_sort (gconstpointer first, gconstpointer second, gpointer user_data)
{
    GNCQueryView *qview = GNC_QUERY_VIEW (user_data);
    GNCQueryViewPrivate *priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    const GncQueryRow *first_row = first;
    const GncQueryRow *second_row = second;
    gint result;

    if (!priv->custom_compare)
        return GTK_ORDERING_EQUAL;
    result = priv->custom_compare (first_row->entry, second_row->entry,
                                   priv->custom_compare_data);
    if (!qview->increasing)
        result = -result;
    return result < 0 ? GTK_ORDERING_SMALLER :
        result > 0 ? GTK_ORDERING_LARGER : GTK_ORDERING_EQUAL;
}

static void
query_view_selection_changed (GtkSelectionModel *selection, guint position,
                              guint n_items, gpointer user_data)
{
    GNCQueryView *qview = GNC_QUERY_VIEW (user_data);

    g_signal_emit (qview, query_view_signals[ROW_SELECTED], 0,
                   GINT_TO_POINTER (query_selection_count (qview)));
    (void)selection;
    (void)position;
    (void)n_items;
}

static void
query_view_activated (GtkColumnView *view, guint position, gpointer user_data)
{
    GNCQueryView *qview = GNC_QUERY_VIEW (user_data);
    GncQueryRow *row = query_row_at (qview, position);
    gpointer entry = row ? row->entry : NULL;

    g_signal_emit (qview, query_view_signals[DOUBLE_CLICK_ENTRY], 0, entry);
    g_clear_object (&row);
    (void)view;
}

static void
query_view_sorter_changed (GtkSorter *sorter, GtkSorterChange change,
                           gpointer user_data)
{
    GNCQueryView *qview = GNC_QUERY_VIEW (user_data);
    GNCQueryViewPrivate *priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    GtkColumnViewColumn *column;
    guint index;
    QueryColumnInfo *info;
    GtkSortType order;

    if (priv->syncing_sorter || !qview->query)
        return;
    column = gtk_column_view_sorter_get_primary_sort_column
        (GTK_COLUMN_VIEW_SORTER (sorter));
    if (!column)
        return;
    index = GPOINTER_TO_UINT (g_object_get_data (G_OBJECT (column),
                                                 "gnc-query-column-index"));
    info = query_column_info_get (qview, index);
    if (!info || info->column != column ||
        gnc_search_param_get_passive
        (GNC_SEARCH_PARAM (g_list_nth_data (qview->column_params, index))))
        return;
    order = gtk_column_view_sorter_get_primary_sort_order
        (GTK_COLUMN_VIEW_SORTER (sorter));
    gnc_query_sort_order (qview, (gint)index + 1, order);
    (void)change;
}

static void
query_cell_toggle_cb (GtkCheckButton *button, gpointer user_data)
{
    QueryColumnFactoryData *data = user_data;
    GtkListItem *list_item = g_object_get_data (G_OBJECT (button),
                                                "gnc-query-list-item");
    GncQueryRow *row;
    gboolean active;

    if (!list_item || !data->view)
        return;
    row = (GncQueryRow *)gtk_list_item_get_item (list_item);
    if (!row)
        return;

    active = gtk_check_button_get_active (button);
    gnc_query_row_set_boolean (row, data->column, active);
    data->view->toggled_row = (gint)gtk_list_item_get_position (list_item);
    data->view->toggled_column = (gint)data->column;
    data->view->toggled_entry = row->entry;
    g_signal_emit (data->view, query_view_signals[COLUMN_TOGGLED], 0,
                   GINT_TO_POINTER (active));
}

static void
query_cell_setup (GtkListItemFactory *factory, GtkListItem *list_item,
                  gpointer user_data)
{
    QueryColumnFactoryData *data = user_data;
    QueryColumnInfo *info = query_column_info_get (data->view, data->column);
    GtkWidget *child;

    if (info && info->boolean)
    {
        child = gtk_check_button_new ();
        gtk_widget_set_halign (child, GTK_ALIGN_CENTER);
        g_signal_connect (child, "toggled", G_CALLBACK (query_cell_toggle_cb), data);
    }
    else
    {
        child = gtk_label_new (NULL);
        gtk_label_set_xalign (GTK_LABEL (child), 0.0);
        gtk_widget_set_hexpand (child, TRUE);
    }
    gtk_list_item_set_child (list_item, child);
    (void)factory;
}

static void
query_cell_bind (GtkListItemFactory *factory, GtkListItem *list_item,
                 gpointer user_data)
{
    QueryColumnFactoryData *data = user_data;
    QueryColumnInfo *info = query_column_info_get (data->view, data->column);
    GncQueryRow *row = (GncQueryRow *)gtk_list_item_get_item (list_item);
    GncQueryCell *cell = gnc_query_row_cell (row, data->column);
    GtkWidget *child = gtk_list_item_get_child (list_item);

    if (!info || !cell)
        return;
    g_object_set_data (G_OBJECT (child), "gnc-query-row", row);
    if (info->boolean)
    {
        g_signal_handlers_block_by_func (child, query_cell_toggle_cb, data);
        gtk_check_button_set_active (GTK_CHECK_BUTTON (child), cell->boolean);
        g_signal_handlers_unblock_by_func (child, query_cell_toggle_cb, data);
        g_object_set_data (G_OBJECT (child), "gnc-query-list-item", list_item);
    }
    else
    {
        gtk_label_set_text (GTK_LABEL (child), cell->text ? cell->text : "");
        gtk_label_set_xalign (GTK_LABEL (child), info->xalign);
        gtk_label_set_ellipsize (GTK_LABEL (child), info->ellipsize);
        gtk_widget_set_tooltip_text (child, info->show_tooltip ? cell->text : NULL);
    }
    (void)factory;
}

static void
query_cell_unbind (GtkListItemFactory *factory, GtkListItem *list_item,
                   gpointer user_data)
{
    GtkWidget *child = gtk_list_item_get_child (list_item);

    if (child)
    {
        g_object_set_data (G_OBJECT (child), "gnc-query-row", NULL);
        g_object_set_data (G_OBJECT (child), "gnc-query-list-item", NULL);
        gtk_widget_set_tooltip_text (child, NULL);
    }
    (void)factory;
    (void)user_data;
}

static void
query_view_append_column (GNCQueryView *qview, GNCSearchParamSimple *param,
                          guint index)
{
    GNCQueryViewPrivate *priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    const gchar *type = gnc_search_param_get_param_type (GNC_SEARCH_PARAM (param));
    QueryColumnInfo *info = g_new0 (QueryColumnInfo, 1);
    QueryColumnFactoryData *data = g_new0 (QueryColumnFactoryData, 1);
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();

    info->boolean = g_strcmp0 (type, QOF_TYPE_BOOLEAN) == 0;
    info->ellipsize = PANGO_ELLIPSIZE_NONE;
    if (gnc_search_param_get_justify (GNC_SEARCH_PARAM (param)) == GTK_JUSTIFY_CENTER)
        info->xalign = 0.5;
    else if (gnc_search_param_get_justify (GNC_SEARCH_PARAM (param)) == GTK_JUSTIFY_RIGHT &&
             gtk_widget_get_direction (GTK_WIDGET (qview)) != GTK_TEXT_DIR_RTL)
        info->xalign = 1.0;

    data->view = qview;
    data->column = index;
    g_signal_connect (factory, "setup", G_CALLBACK (query_cell_setup), data);
    g_signal_connect (factory, "bind", G_CALLBACK (query_cell_bind), data);
    g_signal_connect (factory, "unbind", G_CALLBACK (query_cell_unbind), data);
    g_object_set_data_full (G_OBJECT (factory), "gnc-query-column-data", data,
                            query_column_factory_data_free);

    info->column = gtk_column_view_column_new
        (gnc_search_param_get_title (GNC_SEARCH_PARAM (param)), factory);
    if (!gnc_search_param_get_passive (GNC_SEARCH_PARAM (param)))
    {
        GtkSorter *sorter = GTK_SORTER
            (gtk_custom_sorter_new (query_column_indicator_sort, NULL, NULL));

        gtk_column_view_column_set_sorter (info->column, sorter);
        g_object_unref (sorter);
    }
    gtk_column_view_column_set_resizable (info->column,
                                          !gnc_search_param_get_non_resizeable
                                          (GNC_SEARCH_PARAM (param)));
    gtk_column_view_column_set_expand (info->column, FALSE);
    g_object_set_data (G_OBJECT (info->column), "gnc-query-column-index",
                       GUINT_TO_POINTER (index));
    gtk_column_view_append_column (priv->view, info->column);
    g_ptr_array_add (priv->columns, info);
}

static void
query_view_clear_columns (GNCQueryView *qview)
{
    GNCQueryViewPrivate *priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);

    while (g_list_model_get_n_items (gtk_column_view_get_columns (priv->view)) > 0)
    {
        GtkColumnViewColumn *column = g_list_model_get_item
            (gtk_column_view_get_columns (priv->view), 0);
        gtk_column_view_remove_column (priv->view, column);
        g_object_unref (column);
    }
    g_ptr_array_set_size (priv->columns, 0);
}

void
gnc_query_view_construct (GNCQueryView *qview, GList *param_list, Query *query)
{
    GNCQueryViewPrivate *priv;
    GList *node;
    guint index;

    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    g_return_if_fail (param_list);
    g_return_if_fail (query);

    if (qview->query)
        qof_query_destroy (qview->query);
    qview->query = qof_query_copy (query);
    qview->column_params = param_list;
    qview->num_columns = g_list_length (param_list);
    qview->sort_column = 0;
    qview->increasing = FALSE;

    priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    priv->get_guid = qof_class_get_parameter (qof_query_get_search_for (query),
                                               QOF_PARAM_GUID);
    priv->syncing_sorter = TRUE;
    query_view_clear_columns (qview);
    for (node = param_list, index = 0; node; node = node->next, index++)
    {
        GNCSearchParamSimple *param = node->data;

        g_assert (GNC_IS_SEARCH_PARAM_SIMPLE (param));
        query_view_append_column (qview, param, index);
    }
    priv->syncing_sorter = FALSE;
    gnc_query_view_set_query_sort (qview, TRUE);
}

GtkWidget *
gnc_query_view_new (GList *param_list, Query *query)
{
    GNCQueryView *qview;

    g_return_val_if_fail (param_list, NULL);
    g_return_val_if_fail (query, NULL);
    qview = GNC_QUERY_VIEW (g_object_new (GNC_TYPE_QUERY_VIEW, NULL));
    gnc_query_view_construct (qview, param_list, query);
    return GTK_WIDGET (qview);
}

void
gnc_query_view_reset_query (GNCQueryView *qview, Query *query)
{
    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    g_return_if_fail (query);

    qof_query_destroy (qview->query);
    qview->query = qof_query_copy (query);
    gnc_query_view_set_query_sort (qview, TRUE);
}

static void
gnc_query_view_refresh_handler (GHashTable *changes, gpointer user_data)
{
    GNCQueryView *qview = GNC_QUERY_VIEW (user_data);

    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    gnc_query_view_set_query_sort (qview, TRUE);
    (void)changes;
}

static void
gnc_query_view_init (GNCQueryView *qview)
{
    GNCQueryViewPrivate *priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    GtkSingleSelection *selection;

    gtk_widget_set_name (GTK_WIDGET (qview), "gnc-id-query-view-view");
    qview->sort_column = 0;
    qview->increasing = FALSE;
    qview->toggled_entry = NULL;

    priv->rows = g_list_store_new (gnc_query_row_get_type ());
    priv->sorted_rows = gtk_sort_list_model_new (G_LIST_MODEL (g_object_ref (priv->rows)), NULL);
    selection = gtk_single_selection_new (G_LIST_MODEL (g_object_ref (priv->sorted_rows)));
    gtk_single_selection_set_autoselect (selection, FALSE);
    gtk_single_selection_set_can_unselect (selection, TRUE);
    priv->selection = GTK_SELECTION_MODEL (selection);
    priv->selection_mode = GTK_SELECTION_SINGLE;
    priv->view = GTK_COLUMN_VIEW (gtk_column_view_new (g_object_ref (priv->selection)));
    priv->columns = g_ptr_array_new_with_free_func (query_column_info_free);
    priv->custom_sorter = gtk_custom_sorter_new (query_custom_sort, qview, NULL);
    priv->view_sorter = g_object_ref (gtk_column_view_get_sorter (priv->view));
    priv->view_sorter_changed_id = g_signal_connect
        (priv->view_sorter, "changed", G_CALLBACK (query_view_sorter_changed), qview);

    gnc_column_view_bind_grid_line_preferences (priv->view);
    gtk_column_view_set_reorderable (priv->view, TRUE);
    gtk_box_append (GTK_BOX (qview), GTK_WIDGET (priv->view));
    g_signal_connect_object (priv->selection, "selection-changed",
                             G_CALLBACK (query_view_selection_changed), qview, 0);
    g_signal_connect_object (priv->view, "activate",
                             G_CALLBACK (query_view_activated), qview, 0);

    priv->component_id = gnc_register_gui_component ("gnc-query-view-cm-class",
                                                      gnc_query_view_refresh_handler,
                                                      NULL, qview);
}

static void
gnc_query_view_dispose (GObject *object)
{
    GNCQueryView *qview = GNC_QUERY_VIEW (object);
    GNCQueryViewPrivate *priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);

    if (priv->view_sorter && priv->view_sorter_changed_id)
    {
        g_signal_handler_disconnect (priv->view_sorter,
                                     priv->view_sorter_changed_id);
        priv->view_sorter_changed_id = 0;
    }
    if (priv->selection)
        g_signal_handlers_disconnect_by_func (priv->selection,
                                              query_view_selection_changed, qview);
    if (priv->view)
    {
        g_signal_handlers_disconnect_by_func (priv->view, query_view_activated, qview);
        gnc_column_view_unbind_grid_line_preferences (priv->view);
        gtk_column_view_set_model (priv->view, NULL);
        priv->view = NULL;
    }
    if (priv->component_id > 0)
    {
        gnc_unregister_gui_component (priv->component_id);
        priv->component_id = 0;
    }
    if (qview->query)
    {
        qof_query_destroy (qview->query);
        qview->query = NULL;
    }
    if (priv->sorted_rows)
        gtk_sort_list_model_set_sorter (priv->sorted_rows, NULL);
    g_clear_object (&priv->view_sorter);
    g_clear_object (&priv->custom_sorter);
    g_clear_pointer (&priv->columns, g_ptr_array_unref);
    g_clear_object (&priv->selection);
    g_clear_object (&priv->sorted_rows);
    g_clear_object (&priv->rows);
    G_OBJECT_CLASS (gnc_query_view_parent_class)->dispose (object);
}

static void
gnc_query_view_class_init (GNCQueryViewClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    object_class->dispose = gnc_query_view_dispose;
    query_view_signals[COLUMN_TOGGLED] =
        g_signal_new ("column_toggled", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GNCQueryViewClass, column_toggled), NULL, NULL,
                      g_cclosure_marshal_VOID__POINTER, G_TYPE_NONE, 1, G_TYPE_POINTER);
    query_view_signals[ROW_SELECTED] =
        g_signal_new ("row_selected", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GNCQueryViewClass, row_selected), NULL, NULL,
                      g_cclosure_marshal_VOID__POINTER, G_TYPE_NONE, 1, G_TYPE_POINTER);
    query_view_signals[DOUBLE_CLICK_ENTRY] =
        g_signal_new ("double_click_entry", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GNCQueryViewClass, double_click_entry), NULL, NULL,
                      g_cclosure_marshal_VOID__POINTER, G_TYPE_NONE, 1, G_TYPE_POINTER);
}

void
gnc_query_sort_order (GNCQueryView *qview, gint column, GtkSortType order)
{
    gint sort_column;
    gboolean new_column;

    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    sort_column = column <= 0 || column > qview->num_columns ? 0 : column - 1;
    new_column = qview->sort_column != sort_column;
    qview->sort_column = sort_column;
    qview->increasing = order == GTK_SORT_ASCENDING;
    gnc_query_view_set_query_sort (qview, new_column);
}

static void
gnc_query_view_set_query_sort (GNCQueryView *qview, gboolean new_column)
{
    GNCQueryViewPrivate *priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    GNCSearchParamSimple *param;
    gboolean sort_order = qview->increasing;
    const gchar *type;

    if (!qview->query || qview->sort_column < 0 || qview->sort_column >= qview->num_columns)
        return;
    param = g_list_nth_data (qview->column_params, qview->sort_column);
    g_return_if_fail (GNC_IS_SEARCH_PARAM_SIMPLE (param));
    query_view_sync_sorter (qview);

    if (gnc_search_param_has_param_fcn (param))
    {
        gtk_sort_list_model_set_sorter (priv->sorted_rows,
                                        priv->custom_compare ? GTK_SORTER (priv->custom_sorter) : NULL);
        gnc_query_view_refresh (qview);
        return;
    }

    gtk_sort_list_model_set_sorter (priv->sorted_rows, NULL);
    type = gnc_search_param_get_param_type (GNC_SEARCH_PARAM (param));
    if (qview->numeric_inv_sort &&
        (!g_strcmp0 (type, QOF_TYPE_NUMERIC) || !g_strcmp0 (type, QOF_TYPE_DEBCRED)))
        sort_order = !sort_order;

    if (new_column)
    {
        GSList *path = gnc_search_param_get_param_path (param);
        GSList *default_sort = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);

        qof_query_set_sort_order (qview->query, path, default_sort, NULL);
    }
    qof_query_set_sort_increasing (qview->query, sort_order, sort_order, sort_order);
    gnc_query_view_refresh (qview);
}

gint
gnc_query_view_get_num_entries (GNCQueryView *qview)
{
    GNCQueryViewPrivate *priv;

    g_return_val_if_fail (GNC_IS_QUERY_VIEW (qview), 0);
    priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    return (gint)g_list_model_get_n_items (G_LIST_MODEL (priv->selection));
}

gpointer
gnc_query_view_get_selected_entry (GNCQueryView *qview)
{
    GList *entries;
    gpointer entry = NULL;

    g_return_val_if_fail (GNC_IS_QUERY_VIEW (qview), NULL);
    entries = gnc_query_view_get_selected_entry_list (qview);
    if (entries)
        entry = entries->data;
    if (g_list_length (entries) > 1)
        PWARN ("Expected one selected entry; returning the first selection.");
    g_list_free (entries);
    return entry;
}

GList *
gnc_query_view_get_selected_entry_list (GNCQueryView *qview)
{
    GNCQueryViewPrivate *priv;
    GList *entries = NULL;
    guint count;

    g_return_val_if_fail (GNC_IS_QUERY_VIEW (qview), NULL);
    priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    count = g_list_model_get_n_items (G_LIST_MODEL (priv->selection));
    for (guint position = 0; position < count; position++)
    {
        GncQueryRow *row;

        if (!gtk_selection_model_is_selected (priv->selection, position))
            continue;
        row = query_row_at (qview, position);
        if (row)
        {
            entries = g_list_append (entries, row->entry);
            g_object_unref (row);
        }
    }
    return entries;
}

GList *
gnc_query_view_get_entry_list (GNCQueryView *qview)
{
    GNCQueryViewPrivate *priv;
    GList *entries = NULL;
    guint count;

    g_return_val_if_fail (GNC_IS_QUERY_VIEW (qview), NULL);
    priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    count = g_list_model_get_n_items (G_LIST_MODEL (priv->selection));
    for (guint position = 0; position < count; position++)
    {
        GncQueryRow *row = query_row_at (qview, position);

        if (row)
        {
            entries = g_list_append (entries, row->entry);
            g_object_unref (row);
        }
    }
    return entries;
}

void
gnc_query_use_scroll_to_selection (GNCQueryView *qview, gboolean scroll)
{
    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    qview->use_scroll_to_selection = scroll;
}

static void
scroll_to_selection (GNCQueryView *qview, gboolean override_scroll)
{
    GNCQueryViewPrivate *priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    guint count;
    guint selected = GTK_INVALID_LIST_POSITION;

    if (!qview->use_scroll_to_selection && !override_scroll)
        return;
    count = g_list_model_get_n_items (G_LIST_MODEL (priv->selection));
    for (guint position = 0; position < count; position++)
        if (gtk_selection_model_is_selected (priv->selection, position))
            selected = position;
    if (selected != GTK_INVALID_LIST_POSITION)
        gtk_column_view_scroll_to (priv->view, selected, NULL, GTK_LIST_SCROLL_NONE, NULL);
}

void
gnc_query_scroll_to_selection (GNCQueryView *qview)
{
    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    scroll_to_selection (qview, FALSE);
}

void
gnc_query_force_scroll_to_selection (GNCQueryView *qview)
{
    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    scroll_to_selection (qview, TRUE);
}

void
gnc_query_view_select_entry (GNCQueryView *qview, gpointer entry, gboolean exclusive)
{
    GNCQueryViewPrivate *priv;
    gint position;

    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    position = query_find_entry (qview, entry);
    if (position < 0)
        return;
    priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    gtk_selection_model_select_item (priv->selection, (guint)position, exclusive);
}

void
gnc_query_view_select_first (GNCQueryView *qview)
{
    GNCQueryViewPrivate *priv;

    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    if (g_list_model_get_n_items (G_LIST_MODEL (priv->selection)) > 0)
        gtk_selection_model_select_item (priv->selection, 0, TRUE);
}

void
gnc_query_view_grab_focus (GNCQueryView *qview)
{
    GNCQueryViewPrivate *priv;

    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    gtk_widget_grab_focus (GTK_WIDGET (priv->view));
}

void
gnc_query_view_unselect_all (GNCQueryView *qview)
{
    GNCQueryViewPrivate *priv;

    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    gtk_selection_model_unselect_all (priv->selection);
}

void
gnc_query_view_set_selection_mode (GNCQueryView *qview, GtkSelectionMode mode)
{
    GNCQueryViewPrivate *priv;
    GtkSelectionModel *replacement;
    GList *selected;

    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    g_return_if_fail (mode == GTK_SELECTION_SINGLE || mode == GTK_SELECTION_MULTIPLE);
    priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    if (priv->selection_mode == mode)
        return;

    selected = gnc_query_view_get_selected_entry_list (qview);
    if (mode == GTK_SELECTION_MULTIPLE)
        replacement = GTK_SELECTION_MODEL (gtk_multi_selection_new
                                             (G_LIST_MODEL (g_object_ref (priv->sorted_rows))));
    else
    {
        GtkSingleSelection *selection = gtk_single_selection_new
            (G_LIST_MODEL (g_object_ref (priv->sorted_rows)));
        gtk_single_selection_set_autoselect (selection, FALSE);
        gtk_single_selection_set_can_unselect (selection, TRUE);
        replacement = GTK_SELECTION_MODEL (selection);
    }

    g_signal_connect_object (replacement, "selection-changed",
                             G_CALLBACK (query_view_selection_changed), qview, 0);
    GtkSelectionModel *old_selection = g_steal_pointer (&priv->selection);
    if (old_selection)
        g_signal_handlers_disconnect_by_func (old_selection,
                                              query_view_selection_changed, qview);
    priv->selection = replacement;
    priv->selection_mode = mode;
    gtk_column_view_set_model (priv->view, replacement);
    g_clear_object (&old_selection);
    for (GList *node = selected; node; node = node->next)
        gnc_query_view_select_entry (qview, node->data,
                                     mode == GTK_SELECTION_SINGLE);
    g_list_free (selected);
}

gboolean
gnc_query_view_select_at_point (GNCQueryView *qview, double x, double y)
{
    GtkWidget *picked;

    g_return_val_if_fail (GNC_IS_QUERY_VIEW (qview), FALSE);
    picked = gtk_widget_pick (GTK_WIDGET (qview), x, y, GTK_PICK_DEFAULT);
    while (picked)
    {
        GncQueryRow *row = g_object_get_data (G_OBJECT (picked), "gnc-query-row");

        if (row)
        {
            gnc_query_view_select_entry (qview, row->entry, TRUE);
            return TRUE;
        }
        picked = gtk_widget_get_parent (picked);
    }
    return FALSE;
}

gpointer
gnc_query_view_get_adjacent_entry (GNCQueryView *qview, gpointer entry,
                                   gboolean previous)
{
    gint position;
    GncQueryRow *row;
    gpointer result = NULL;

    g_return_val_if_fail (GNC_IS_QUERY_VIEW (qview), NULL);
    position = query_find_entry (qview, entry);
    if (position < 0)
        return NULL;
    position += previous ? -1 : 1;
    if (position < 0 || position >= gnc_query_view_get_num_entries (qview))
        return NULL;
    row = query_row_at (qview, (guint)position);
    if (row)
    {
        result = row->entry;
        g_object_unref (row);
    }
    return result;
}

static void
gnc_query_view_refresh_selected (GNCQueryView *qview, GList *old_entries)
{
    for (GList *node = old_entries; node; node = node->next)
        gnc_query_view_select_entry (qview, node->data, FALSE);
    gnc_query_scroll_to_selection (qview);
}

void
gnc_query_view_refresh (GNCQueryView *qview)
{
    GNCQueryViewPrivate *priv;
    GList *selected_entries;

    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    selected_entries = gnc_query_view_get_selected_entry_list (qview);
    g_list_store_remove_all (priv->rows);
    gnc_query_view_fill (qview);
    gnc_query_view_refresh_selected (qview, selected_entries);
    g_list_free (selected_entries);
}

gboolean
gnc_query_view_item_in_view (GNCQueryView *qview, gpointer item)
{
    g_return_val_if_fail (GNC_IS_QUERY_VIEW (qview), FALSE);
    return query_find_entry (qview, item) >= 0;
}

void
gnc_query_set_expand_column (GNCQueryView *qview, gint column)
{
    QueryColumnInfo *info;

    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    if (column < 0)
        return;
    info = query_column_info_get (qview, (guint)column);
    if (info)
        gtk_column_view_column_set_expand (info->column, TRUE);
}

void
gnc_query_view_set_numerics (GNCQueryView *qview, gboolean abs, gboolean inv_sort)
{
    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    qview->numeric_abs = abs;
    qview->numeric_inv_sort = inv_sort;
}

void
gnc_query_view_set_custom_sort_func (GNCQueryView *qview,
                                     GncQueryViewCompareFunc compare,
                                     gpointer user_data)
{
    GNCQueryViewPrivate *priv;

    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    priv->custom_compare = compare;
    priv->custom_compare_data = user_data;
    gtk_sorter_changed (GTK_SORTER (priv->custom_sorter), GTK_SORTER_CHANGE_DIFFERENT);
}

void
gnc_query_view_set_column_ellipsize (GNCQueryView *qview, gint column,
                                     PangoEllipsizeMode mode, gboolean show_tooltip)
{
    QueryColumnInfo *info;

    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    if (column < 0)
        return;
    info = query_column_info_get (qview, (guint)column);
    if (!info)
        return;
    info->ellipsize = mode;
    info->show_tooltip = show_tooltip;
}

void
gnc_query_view_add_column_padding (GNCQueryView *qview, gint column, gint xpadding)
{
    QueryColumnInfo *info;
    gint width;

    g_return_if_fail (GNC_IS_QUERY_VIEW (qview));
    if (column < 0)
        return;
    info = query_column_info_get (qview, (guint)column);
    if (!info)
        return;
    width = gtk_column_view_column_get_fixed_width (info->column);
    if (width <= 0)
        width = 28;
    info->padding += MAX (xpadding, 0);
    gtk_column_view_column_set_fixed_width (info->column, width + MAX (xpadding, 0));
}

gint
gnc_query_view_get_column_width (GNCQueryView *qview, gint column)
{
    QueryColumnInfo *info;
    gint width;

    g_return_val_if_fail (GNC_IS_QUERY_VIEW (qview), 0);
    if (column < 0)
        return 0;
    info = query_column_info_get (qview, (guint)column);
    if (!info)
        return 0;
    width = gtk_column_view_column_get_fixed_width (info->column);
    return width > 0 ? width : 0;
}

static void
gnc_query_view_fill (GNCQueryView *qview)
{
    GNCQueryViewPrivate *priv = GNC_QUERY_VIEW_GET_PRIVATE (qview);
    GList *entries;

    gnc_gui_component_clear_watches (priv->component_id);
    entries = qof_query_run (qview->query);
    for (GList *item = entries; item; item = item->next)
    {
        GncQueryRow *row = gnc_query_row_new (item->data, qview->num_columns);
        const GncGUID *guid;
        const QofParam *get_guid;
        guint column = 0;

        for (GList *node = qview->column_params; node; node = node->next, column++)
        {
            GNCSearchParamSimple *param = node->data;
            GSList *converters;
            QofParam *last_param = NULL;
            const gchar *type;
            gpointer result = item->data;

            g_assert (GNC_IS_SEARCH_PARAM_SIMPLE (param));
            type = gnc_search_param_get_param_type (GNC_SEARCH_PARAM (param));
            if (g_strcmp0 (type, QOF_TYPE_BOOLEAN) == 0)
            {
                gnc_query_row_set_boolean
                    (row, column, GPOINTER_TO_INT (gnc_search_param_compute_value (param, result)));
                continue;
            }

            converters = gnc_search_param_get_converters (param);
            for (; converters; converters = converters->next)
            {
                last_param = converters->data;
                if (converters->next)
                    result = (last_param->param_getfcn) (result, last_param);
            }
            if (last_param && (!g_strcmp0 (type, QOF_TYPE_DEBCRED) ||
                               !g_strcmp0 (type, QOF_TYPE_NUMERIC)))
            {
                union { QofAccessFunc access; gnc_numeric (*numeric) (gpointer, QofParam *); } getter;
                getter.access = last_param->param_getfcn;
                gnc_numeric value = getter.numeric (result, last_param);

                if (qview->numeric_abs)
                    value = gnc_numeric_abs (value);
                gnc_query_row_set_text (row, column,
                                        xaccPrintAmount (value, gnc_default_print_info (FALSE)));
            }
            else
            {
                gchar *value = qof_query_core_to_string (type, result, last_param);

                gnc_query_row_set_text (row, column, value);
                g_free (value);
            }
        }
        g_list_store_append (priv->rows, row);
        g_object_unref (row);

        get_guid = priv->get_guid;
        guid = get_guid ? (const GncGUID *)get_guid->param_getfcn (item->data, get_guid) : NULL;
        if (guid)
            gnc_gui_component_watch_entity (priv->component_id, guid,
                                            QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);
    }
}
