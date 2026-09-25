/*
 * Copyright (C) 2003,2005 David Hampton <hampton@employees.org>    *
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
 *                                                                  *
\********************************************************************/

#include <config.h>
#include <stdarg.h>
#include <glib/gi18n.h>
#include "gnc-tree-view-price.h"
#include "gnc-tree-model-price.h"
#include "gnc-engine.h"
#include "gnc-string-utils.h"
struct _GncTreeViewPrice
{
    GncTreeView parent_instance;
};
typedef struct
{
    GncTreeModelPrice *model;
    GListStore *roots;
    GtkTreeListModel *rows;
    GtkMultiSelection *selection;
    GHashTable *selected;
    GHashTable *expanded;
    gnc_tree_view_price_ns_filter_func ns_filter;
    gnc_tree_view_price_cm_filter_func cm_filter;
    gnc_tree_view_price_pc_filter_func pc_filter;
    gpointer filter_data;
    GDestroyNotify filter_destroy;
    GncTreeModelPriceColumn sort_column;
    GtkSortType sort_order;
    GtkSorter *view_sorter;
    gulong view_sorter_changed_id;
    guint restore_source;
    guint suspended;
    gboolean dirty;
    gboolean synchronizing;
    gboolean disposing;
    struct _PriceChildrenContext *children_context;
} GncTreeViewPricePrivate;

typedef struct _PriceChildrenContext
{
    GWeakRef view;
    gboolean disposed;
} PriceChildrenContext;
typedef struct
{
    gatomicrefcount ref_count;
    GWeakRef view;
    GncTreeModelPriceColumn column;
    gboolean tree;
} PriceColumn;
G_DEFINE_TYPE_WITH_PRIVATE (GncTreeViewPrice, gnc_tree_view_price, GNC_TYPE_TREE_VIEW)

static GncTreeViewPricePrivate *
priv (GncTreeViewPrice *view)
{
    return gnc_tree_view_price_get_instance_private (view);
}
static GncTreeModelPriceRow *
row_from_item (gpointer item)
{
    if (!GTK_IS_TREE_LIST_ROW (item))
        return NULL;
    GObject *row_item = gtk_tree_list_row_get_item (GTK_TREE_LIST_ROW (item));
    GncTreeModelPriceRow *row = GNC_TREE_MODEL_PRICE_ROW (row_item);
    g_clear_object (&row_item);
    return row;
}
static gboolean
row_visible (GncTreeViewPricePrivate *p, GncTreeModelPriceRow *row)
{
    switch (gnc_tree_model_price_row_get_kind (row))
    {
        case GNC_TREE_MODEL_PRICE_ROW_NAMESPACE: return !p->ns_filter || p->ns_filter (gnc_tree_model_price_row_get_namespace (row), p->filter_data);
        case GNC_TREE_MODEL_PRICE_ROW_COMMODITY: return !p->cm_filter || p->cm_filter (gnc_tree_model_price_row_get_commodity (row), p->filter_data);
        case GNC_TREE_MODEL_PRICE_ROW_PRICE: return !p->pc_filter || p->pc_filter (gnc_tree_model_price_row_get_price (row), p->filter_data);
        default: return FALSE;
    }
}
static gint
compare_prices (GNCPrice *a, GNCPrice *b, GncTreeModelPriceColumn column)
{
    gint result = 0;
    if (column == GNC_TREE_MODEL_PRICE_COL_DATE)
    {
        time64 ta = gnc_price_get_time64 (a), tb = gnc_price_get_time64 (b);
        result = ta < tb? 1: ta > tb? -1: 0;
    }
    else if (column == GNC_TREE_MODEL_PRICE_COL_SOURCE) result = (gint)gnc_price_get_source (a) - (gint)gnc_price_get_source (b);
    else if (column == GNC_TREE_MODEL_PRICE_COL_TYPE) result = safe_utf8_collate (gnc_price_get_typestr (a), gnc_price_get_typestr (b));
    else if (column == GNC_TREE_MODEL_PRICE_COL_VALUE) result = gnc_numeric_compare (gnc_price_get_value (a), gnc_price_get_value (b));
    if (result) return result;
    gnc_commodity *ca = gnc_price_get_currency (a), *cb = gnc_price_get_currency (b);
    result = safe_utf8_collate (ca? gnc_commodity_get_unique_name (ca): "", cb? gnc_commodity_get_unique_name (cb): "");
    if (result) return result;
    return gnc_numeric_compare (gnc_price_get_value (a), gnc_price_get_value (b));
}
static gint
row_compare_column (gconstpointer left, gconstpointer right,
                    GncTreeModelPriceColumn column)
{
    GncTreeModelPriceRow *a = GNC_TREE_MODEL_PRICE_ROW ((gpointer)left), *b = GNC_TREE_MODEL_PRICE_ROW ((gpointer)right);
    gint result;
    if (gnc_tree_model_price_row_get_kind (a) == GNC_TREE_MODEL_PRICE_ROW_PRICE && gnc_tree_model_price_row_get_kind (b) == GNC_TREE_MODEL_PRICE_ROW_PRICE) result = compare_prices (gnc_tree_model_price_row_get_price (a), gnc_tree_model_price_row_get_price (b), column);
    else
    {
        gchar *sa = gnc_tree_model_price_row_get_string (a, GNC_TREE_MODEL_PRICE_COL_COMMODITY);
        gchar *sb = gnc_tree_model_price_row_get_string (b, GNC_TREE_MODEL_PRICE_COL_COMMODITY);
        result = g_utf8_collate (sa, sb);
        g_free (sa);
        g_free (sb);
    }
    return result;
}
static gint
row_compare (gconstpointer left, gconstpointer right, gpointer data)
{
    GncTreeViewPricePrivate *p = data;
    gint result = row_compare_column (left, right, p->sort_column);

    return p->sort_order == GTK_SORT_DESCENDING? -result: result;
}
static void
append_sorted_visible (GncTreeViewPricePrivate *p, GListStore *store,
                       GListModel *source)
{
    for (guint i = 0;
         !p->disposing && i < g_list_model_get_n_items (source); i++)
    {
        GncTreeModelPriceRow *row = g_list_model_get_item (source, i);
        gboolean visible = row_visible (p, row);

        if (!p->disposing && visible)
            g_list_store_insert_sorted (store, row, row_compare, p);
        g_object_unref (row);
    }
}
static GListModel *
create_children (gpointer item, gpointer user_data)
{
    PriceChildrenContext *context = user_data;
    GncTreeViewPrice *view;
    GncTreeViewPricePrivate *p;
    GListModel *source = gnc_tree_model_price_row_get_children (GNC_TREE_MODEL_PRICE_ROW (item));
    GListStore *children;
    if (context->disposed || !source || g_list_model_get_n_items (source) == 0) return NULL;
    view = g_weak_ref_get (&context->view);
    if (!view) return NULL;
    p = priv (view);
    children = g_list_store_new (GNC_TYPE_TREE_MODEL_PRICE_ROW);
    append_sorted_visible (p, children, source);
    if (g_list_model_get_n_items (G_LIST_MODEL (children)) == 0)
    {
        g_object_unref (children);
        g_object_unref (view);
        return NULL;
    }
    g_object_unref (view);
    return G_LIST_MODEL (children);
}

static void
price_children_context_free (PriceChildrenContext *context)
{
    g_weak_ref_clear (&context->view);
    g_free (context);
}

static void
price_column_free (PriceColumn *column)
{
    g_weak_ref_clear (&column->view);
    g_free (column);
}

static PriceColumn *
price_column_ref (PriceColumn *column)
{
    g_atomic_ref_count_inc (&column->ref_count);
    return column;
}

static void
price_column_unref (PriceColumn *column)
{
    if (g_atomic_ref_count_dec (&column->ref_count))
        price_column_free (column);
}

static void
price_column_closure_free (gpointer data, GClosure *closure)
{
    price_column_unref (data);
    (void)closure;
}

static GncTreeViewPrice *
price_column_get_view (PriceColumn *column)
{
    return g_weak_ref_get (&column->view);
}

static void
rebuild_roots (GncTreeViewPrice *view)
{
    GncTreeViewPricePrivate *p = priv (view);
    GListStore *roots;
    GncTreeModelPrice *model;
    GtkMultiSelection *selection = NULL;

    if (p->disposing || !p->roots || !p->model)
        return;
    roots = g_object_ref (p->roots);
    model = g_object_ref (p->model);
    if (p->selection)
        selection = g_object_ref (p->selection);
    p->synchronizing = TRUE;
    if (selection)
        gtk_selection_model_unselect_all (GTK_SELECTION_MODEL (selection));
    if (!p->disposing)
        g_list_store_remove_all (roots);
    if (!p->disposing)
        append_sorted_visible (p, roots, gnc_tree_model_price_get_roots (model));
    g_clear_object (&selection);
    g_object_unref (model);
    g_object_unref (roots);
}
static gboolean
restore_state (gpointer data)
{
    GncTreeViewPrice *view = GNC_TREE_VIEW_PRICE (data);
    GncTreeViewPricePrivate *p = priv (view);
    GtkTreeListModel *rows;
    GtkMultiSelection *selection;
    GHashTable *selected;
    GHashTable *expanded;
    gboolean expanded_any = FALSE;

    if (p->disposing || !p->rows || !p->selection || !p->selected ||
        !p->expanded)
        return G_SOURCE_REMOVE;
    rows = g_object_ref (p->rows);
    selection = g_object_ref (p->selection);
    selected = g_hash_table_ref (p->selected);
    expanded = g_hash_table_ref (p->expanded);
    p->synchronizing = TRUE;
    /* Expanding changes flattened row positions. Reconcile the selection only
     * after the hierarchy has reached a stable pass. */
    for (guint i = 0;
         !p->disposing && i < g_list_model_get_n_items (G_LIST_MODEL (rows)); i++)
    {
        GtkTreeListRow *tr = gtk_tree_list_model_get_row (rows, i);
        GncTreeModelPriceRow *row = row_from_item (tr);
        if (row && gtk_tree_list_row_is_expandable (tr) &&
            g_hash_table_contains (expanded,
                                   gnc_tree_model_price_row_get_id (row)) &&
            !gtk_tree_list_row_get_expanded (tr))
        {
            gtk_tree_list_row_set_expanded (tr, TRUE);
            expanded_any = TRUE;
        }
        g_object_unref (tr);
    }
    if (!p->disposing && !expanded_any)
    {
        GtkBitset *desired = gtk_bitset_new_empty ();
        guint n_items = g_list_model_get_n_items (G_LIST_MODEL (rows));

        for (guint i = 0; i < n_items; i++)
        {
            GtkTreeListRow *tr = gtk_tree_list_model_get_row (rows, i);
            GncTreeModelPriceRow *row = row_from_item (tr);

            if (row &&
                g_hash_table_contains (selected,
                                       gnc_tree_model_price_row_get_id (row)))
                gtk_bitset_add (desired, i);
            g_object_unref (tr);
        }
        if (!p->disposing)
        {
            GtkBitset *mask = gtk_bitset_new_range (0, n_items);

            /* The desired IDs are the complete selection, not additions to
             * whichever rows happened to remain selected. */
            gtk_selection_model_set_selection (GTK_SELECTION_MODEL (selection),
                                               desired, mask);
            gtk_bitset_unref (mask);
        }
        gtk_bitset_unref (desired);
    }
    g_hash_table_unref (expanded);
    g_hash_table_unref (selected);
    g_object_unref (selection);
    g_object_unref (rows);
    if (!p->disposing && expanded_any)
        return G_SOURCE_CONTINUE;
    if (!p->disposing)
    {
        p->synchronizing = FALSE;
        p->restore_source = 0;
    }
    return G_SOURCE_REMOVE;
}
static void
schedule_restore (GncTreeViewPrice *view)
{
    GncTreeViewPricePrivate *p = priv (view);
    if (p->disposing || !p->rows || !p->selection || !p->selected ||
        !p->expanded)
        return;
    if (!p->restore_source)
        p->restore_source = g_idle_add_full (G_PRIORITY_DEFAULT_IDLE,
                                             restore_state,
                                             g_object_ref (view),
                                             g_object_unref);
}
static void
model_changed (GncTreeModelPrice *model, GncTreeViewPrice *view)
{
    GncTreeViewPricePrivate *p = priv (view);
    (void)model;
    if (p->disposing)
        return;
    if (p->suspended)
    {
        p->dirty = TRUE;
        return;
    }
    rebuild_roots (view);
    schedule_restore (view);
}
static void
selection_changed (GtkSelectionModel *selection, guint position, guint n_items, GncTreeViewPrice *view)
{
    GncTreeViewPricePrivate *p = priv (view);
    if (p->disposing || p->synchronizing) return;
    g_hash_table_remove_all (p->selected);
    for (guint i = 0; i < g_list_model_get_n_items (G_LIST_MODEL (p->rows)); i++) if (gtk_selection_model_is_selected (selection, i))
    {
        GtkTreeListRow *tr = gtk_tree_list_model_get_row (p->rows, i);
        GncTreeModelPriceRow *row = row_from_item (tr);
        if (row) g_hash_table_add (p->selected, g_strdup (gnc_tree_model_price_row_get_id (row)));
        g_object_unref (tr);
    }
    (void)position;
    (void)n_items;
}
static void
row_expanded (GtkTreeListRow *tr, GParamSpec *pspec, GncTreeViewPrice *view)
{
    GncTreeViewPricePrivate *p = priv (view);
    GncTreeModelPriceRow *row = row_from_item (tr);
    if (row && !p->disposing && !p->synchronizing)
    {
        const gchar *id = gnc_tree_model_price_row_get_id (row);
        if (gtk_tree_list_row_get_expanded (tr)) g_hash_table_add (p->expanded, g_strdup (id));
        else g_hash_table_remove (p->expanded, id);
    }
    (void)pspec;
}
static void
factory_setup (GtkSignalListItemFactory *factory, GtkListItem *item, PriceColumn *column)
{
    GtkWidget *label = gtk_label_new (NULL);
    gtk_widget_set_halign (label, GTK_ALIGN_START);
    if (column->tree)
    {
        GtkWidget *expander = gtk_tree_expander_new ();
        gtk_tree_expander_set_child (GTK_TREE_EXPANDER (expander), label);
        gtk_list_item_set_child (item, expander);
    }
    else gtk_list_item_set_child (item, label);
    (void)factory;
}
static void
factory_bind (GtkSignalListItemFactory *factory, GtkListItem *item, PriceColumn *column)
{
    GncTreeViewPrice *view = price_column_get_view (column);
    GtkTreeListRow *tr = GTK_TREE_LIST_ROW (gtk_list_item_get_item (item));
    GncTreeModelPriceRow *row = row_from_item (tr);
    GtkWidget *child = gtk_list_item_get_child (item);
    GtkWidget *label = column->tree? gtk_tree_expander_get_child (GTK_TREE_EXPANDER (child)): child;
    gchar *text;

    if (!view || priv (view)->disposing)
    {
        g_clear_object (&view);
        return;
    }
    text = gnc_tree_model_price_row_get_string (row, column->column);
    if (column->tree) gtk_tree_expander_set_list_row (GTK_TREE_EXPANDER (child), tr);
    gtk_label_set_text (GTK_LABEL (label), text);
    g_free (text);
    g_signal_connect_object (tr, "notify::expanded", G_CALLBACK (row_expanded), view, 0);
    g_object_unref (view);
    (void)factory;
}
static void
factory_unbind (GtkSignalListItemFactory *factory, GtkListItem *item, PriceColumn *column)
{
    GncTreeViewPrice *view = price_column_get_view (column);
    GtkTreeListRow *tr = GTK_TREE_LIST_ROW (gtk_list_item_get_item (item));
    if (view)
    {
        g_signal_handlers_disconnect_by_func (tr, row_expanded, view);
        g_object_unref (view);
    }
    (void)factory;
}
static GtkOrdering
sorter_cb (gconstpointer left, gconstpointer right, gpointer user_data)
{
    PriceColumn *column = user_data;
    GncTreeViewPrice *view = price_column_get_view (column);
    GncTreeModelPriceRow *a = row_from_item ((gpointer)left), *b = row_from_item ((gpointer)right);
    gint result;

    if (!view || priv (view)->disposing)
    {
        g_clear_object (&view);
        return GTK_ORDERING_EQUAL;
    }
    result = row_compare_column (a, b, column->column);
    g_object_unref (view);
    return result < 0? GTK_ORDERING_SMALLER: result > 0? GTK_ORDERING_LARGER: GTK_ORDERING_EQUAL;
}
static void
sort_changed (GtkSorter *sorter, GtkSorterChange change,
              GncTreeViewPrice *view)
{
    GncTreeViewPricePrivate *p;
    GtkColumnViewColumn *column_view;
    PriceColumn *column;

    g_object_ref (view);
    if (priv (view)->disposing)
        goto cleanup;
    column_view = gtk_column_view_sorter_get_primary_sort_column
        (GTK_COLUMN_VIEW_SORTER (sorter));
    if (!column_view)
        goto cleanup;
    column = g_object_get_data (G_OBJECT (column_view), "gnc-price-column");
    if (!column)
        goto cleanup;
    p = priv (view);
    p->sort_column = column->column;
    p->sort_order = gtk_column_view_sorter_get_primary_sort_order
        (GTK_COLUMN_VIEW_SORTER (sorter));
    rebuild_roots (view);
    schedule_restore (view);
cleanup:
    g_object_unref (view);
    (void)change;
}
static GtkColumnViewColumn *
add_column (GncTreeViewPrice *view, const gchar *title, const gchar *id, GncTreeModelPriceColumn value, gboolean tree, gboolean visible)
{
    PriceColumn *data = g_new0 (PriceColumn, 1);
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkCustomSorter *sorter;
    GtkColumnViewColumn *column;
    g_atomic_ref_count_init (&data->ref_count);
    g_weak_ref_init (&data->view, view);
    data->column = value;
    data->tree = tree;
    g_signal_connect_data (factory, "setup", G_CALLBACK (factory_setup),
                           price_column_ref (data), price_column_closure_free, 0);
    g_signal_connect_data (factory, "bind", G_CALLBACK (factory_bind),
                           price_column_ref (data), price_column_closure_free, 0);
    g_signal_connect_data (factory, "unbind", G_CALLBACK (factory_unbind),
                           price_column_ref (data), price_column_closure_free, 0);
    column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_id (column, id);
    gtk_column_view_column_set_resizable (column, TRUE);
    gtk_column_view_column_set_expand (column, tree);
    gtk_column_view_column_set_visible (column, visible);
    sorter = gtk_custom_sorter_new (sorter_cb, price_column_ref (data),
                                    (GDestroyNotify)price_column_unref);
    gtk_column_view_column_set_sorter (column, GTK_SORTER (sorter));
    g_object_set_data_full (G_OBJECT (column), "gnc-price-column",
                            price_column_ref (data),
                            (GDestroyNotify)price_column_unref);
    gtk_column_view_append_column (gnc_tree_view_get_column_view (GNC_TREE_VIEW (view)), column);
    g_object_unref (sorter);
    g_object_unref (column);
    price_column_unref (data);
    return column;
}
static void
view_dispose (GObject *object)
{
    GncTreeViewPrice *view = GNC_TREE_VIEW_PRICE (object);
    GncTreeViewPricePrivate *p = priv (view);
    GtkColumnView *column_view;
    GDestroyNotify filter_destroy;
    gpointer filter_data;
    guint restore_source = p->restore_source;

    p->disposing = TRUE;
    if (p->view_sorter && p->view_sorter_changed_id)
    {
        g_signal_handler_disconnect (p->view_sorter,
                                     p->view_sorter_changed_id);
        p->view_sorter_changed_id = 0;
    }
    p->restore_source = 0;
    if (restore_source) g_source_remove (restore_source);
    if (p->children_context)
        p->children_context->disposed = TRUE;
    p->children_context = NULL;
    if (p->selection)
        g_signal_handlers_disconnect_by_func (p->selection, selection_changed, view);
    if (p->model)
        g_signal_handlers_disconnect_by_func (p->model, model_changed, view);
    column_view = gnc_tree_view_get_column_view (GNC_TREE_VIEW (view));
    if (column_view)
        gtk_column_view_set_model (column_view, NULL);
    filter_destroy = g_steal_pointer (&p->filter_destroy);
    filter_data = g_steal_pointer (&p->filter_data);
    p->ns_filter = NULL;
    p->cm_filter = NULL;
    p->pc_filter = NULL;
    if (filter_destroy) filter_destroy (filter_data);
    g_clear_pointer (&p->selected, g_hash_table_unref);
    g_clear_pointer (&p->expanded, g_hash_table_unref);
    g_clear_object (&p->selection);
    g_clear_object (&p->rows);
    g_clear_object (&p->roots);
    g_clear_object (&p->model);
    g_clear_object (&p->view_sorter);
    G_OBJECT_CLASS (gnc_tree_view_price_parent_class)->dispose (object);
}
static void
gnc_tree_view_price_class_init (GncTreeViewPriceClass *klass)
{
    G_OBJECT_CLASS (klass)->dispose = view_dispose;
}
static void
gnc_tree_view_price_init (GncTreeViewPrice *view)
{
    GncTreeViewPricePrivate *p = priv (view);
    p->selected = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    p->expanded = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    p->sort_column = GNC_TREE_MODEL_PRICE_COL_COMMODITY;
    p->sort_order = GTK_SORT_ASCENDING;
}
GtkWidget *
gnc_tree_view_price_new (QofBook *book, const gchar *first_property_name, ...)
{
    GncTreeViewPrice *view = g_object_new (GNC_TYPE_TREE_VIEW_PRICE, "name", "gnc-id-price-tree", NULL);
    GncTreeViewPricePrivate *p = priv (view);
    GtkColumnView *column_view;
    GtkColumnViewColumn *default_column;
    va_list args;
    p->model = gnc_tree_model_price_new (book, gnc_pricedb_get_db (book));
    p->roots = g_list_store_new (GNC_TYPE_TREE_MODEL_PRICE_ROW);
    p->children_context = g_new0 (PriceChildrenContext, 1);
    g_weak_ref_init (&p->children_context->view, view);
    rebuild_roots (view);
    p->rows = gtk_tree_list_model_new (g_object_ref (G_LIST_MODEL (p->roots)), FALSE, FALSE,
                                       create_children, p->children_context,
                                       (GDestroyNotify) price_children_context_free);
    p->selection = gtk_multi_selection_new (g_object_ref (G_LIST_MODEL (p->rows)));
    column_view = gnc_tree_view_get_column_view (GNC_TREE_VIEW (view));
    gtk_column_view_set_model (column_view, GTK_SELECTION_MODEL (p->selection));
    default_column = add_column (view, _("Security"), "security", GNC_TREE_MODEL_PRICE_COL_COMMODITY, TRUE, TRUE);
    add_column (view, _("Currency"), "currency", GNC_TREE_MODEL_PRICE_COL_CURRENCY, FALSE, TRUE);
    add_column (view, _("Date"), "date", GNC_TREE_MODEL_PRICE_COL_DATE, FALSE, TRUE);
    add_column (view, _("Source"), "source", GNC_TREE_MODEL_PRICE_COL_SOURCE, FALSE, TRUE);
    add_column (view, _("Type"), "type", GNC_TREE_MODEL_PRICE_COL_TYPE, FALSE, TRUE);
    add_column (view, _("Price"), "price", GNC_TREE_MODEL_PRICE_COL_VALUE, FALSE, TRUE);
    va_start (args, first_property_name);
    g_object_set_valist (G_OBJECT (view), first_property_name, args);
    va_end (args);
    gtk_column_view_sort_by_column (column_view, default_column,
                                    GTK_SORT_ASCENDING);
    p->view_sorter = g_object_ref (gtk_column_view_get_sorter (column_view));
    p->view_sorter_changed_id = g_signal_connect
        (p->view_sorter, "changed", G_CALLBACK (sort_changed), view);
    g_signal_connect_object (p->selection, "selection-changed", G_CALLBACK (selection_changed), view, 0);
    g_signal_connect_object (p->model, "changed", G_CALLBACK (model_changed), view, 0);
    return GTK_WIDGET (view);
}
GtkColumnView *
gnc_tree_view_price_get_column_view (GncTreeViewPrice *view)
{
    g_return_val_if_fail (GNC_IS_TREE_VIEW_PRICE (view), NULL);
    return gnc_tree_view_get_column_view (GNC_TREE_VIEW (view));
}
GtkSelectionModel *
gnc_tree_view_price_get_selection_model (GncTreeViewPrice *view)
{
    g_return_val_if_fail (GNC_IS_TREE_VIEW_PRICE (view), NULL);
    return GTK_SELECTION_MODEL (priv (view)->selection);
}
void
gnc_tree_view_price_set_filter (GncTreeViewPrice *view, gnc_tree_view_price_ns_filter_func ns, gnc_tree_view_price_cm_filter_func cm, gnc_tree_view_price_pc_filter_func pc, gpointer data, GDestroyNotify destroy)
{
    GncTreeViewPricePrivate *p = priv (view);
    if (p->filter_destroy) p->filter_destroy (p->filter_data);
    p->ns_filter = ns;
    p->cm_filter = cm;
    p->pc_filter = pc;
    p->filter_data = data;
    p->filter_destroy = destroy;
    rebuild_roots (view);
    schedule_restore (view);
}
void
gnc_tree_view_price_suspend_updates (GncTreeViewPrice *view)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_PRICE (view));
    priv (view)->suspended++;
}
void
gnc_tree_view_price_resume_updates (GncTreeViewPrice *view)
{
    GncTreeViewPricePrivate *p;
    g_return_if_fail (GNC_IS_TREE_VIEW_PRICE (view));
    p = priv (view);
    if (!p->suspended) return;
    if (--p->suspended == 0 && p->dirty)
    {
        p->dirty = FALSE;
        rebuild_roots (view);
        schedule_restore (view);
    }
}
void
gnc_tree_view_price_toggle_expand (GncTreeViewPrice *view, guint position)
{
    GncTreeViewPricePrivate *p = priv (view);
    GtkTreeListRow *row;
    if (position >= g_list_model_get_n_items (G_LIST_MODEL (p->rows))) return;
    row = gtk_tree_list_model_get_row (p->rows, position);
    if (gtk_tree_list_row_is_expandable (row)) gtk_tree_list_row_set_expanded (row, !gtk_tree_list_row_get_expanded (row));
    g_object_unref (row);
}
static GncTreeModelPriceRow *
first_selected (GncTreeViewPrice *view)
{
    GncTreeViewPricePrivate *p = priv (view);
    for (guint i = 0; i < g_list_model_get_n_items (G_LIST_MODEL (p->rows)); i++) if (gtk_selection_model_is_selected (GTK_SELECTION_MODEL (p->selection), i))
    {
        GtkTreeListRow *tr = gtk_tree_list_model_get_row (p->rows, i);
        GncTreeModelPriceRow *row = tr? g_object_ref (row_from_item (tr)): NULL;
        g_clear_object (&tr);
        return row;
    }
    return NULL;
}
GNCPrice *
gnc_tree_view_price_get_cursor_price (GncTreeViewPrice *view)
{
    GncTreeModelPriceRow *row = first_selected (view);
    GNCPrice *price = row? gnc_tree_model_price_row_get_price (row): NULL;
    g_clear_object (&row);
    return price;
}
GNCPrice *
gnc_tree_view_price_get_selected_price (GncTreeViewPrice *view)
{
    return gnc_tree_view_price_get_cursor_price (view);
}
void
gnc_tree_view_price_set_selected_price (GncTreeViewPrice *view, GNCPrice *price)
{
    GncTreeViewPricePrivate *p = priv (view);
    gchar guid[GUID_ENCODING_LENGTH + 1];
    gnc_commodity *commodity;
    gnc_commodity_namespace *ns;
    if (!price)
    {
        g_hash_table_remove_all (p->selected);
        schedule_restore (view);
        return;
    }
    guid_to_string_buff (gnc_price_get_guid (price), guid);
    g_hash_table_remove_all (p->selected);
    g_hash_table_add (p->selected, g_strconcat ("price:", guid, NULL));
    commodity = gnc_price_get_commodity (price);
    if (commodity)
    {
        guid_to_string_buff (qof_instance_get_guid (QOF_INSTANCE (commodity)), guid);
        g_hash_table_add (p->expanded, g_strconcat ("commodity:", guid, NULL));
        ns = gnc_commodity_get_namespace_ds (commodity);
        g_hash_table_add (p->expanded, g_strconcat ("namespace:", gnc_commodity_namespace_get_name (ns), NULL));
    }
    schedule_restore (view);
}
GList *
gnc_tree_view_price_get_selected_prices (GncTreeViewPrice *view)
{
    GncTreeViewPricePrivate *p = priv (view);
    GList *result = NULL;
    for (guint i = 0; i < g_list_model_get_n_items (G_LIST_MODEL (p->rows)); i++) if (gtk_selection_model_is_selected (GTK_SELECTION_MODEL (p->selection), i))
    {
        GtkTreeListRow *tr = gtk_tree_list_model_get_row (p->rows, i);
        GncTreeModelPriceRow *row = row_from_item (tr);
        GNCPrice *price = row? gnc_tree_model_price_row_get_price (row): NULL;
        if (price) result = g_list_prepend (result, price);
        g_object_unref (tr);
    }
    return g_list_reverse (result);
}
GList *
gnc_tree_view_price_get_selected_commodities (GncTreeViewPrice *view)
{
    GncTreeViewPricePrivate *p = priv (view);
    GList *result = NULL;
    for (guint i = 0; i < g_list_model_get_n_items (G_LIST_MODEL (p->rows)); i++) if (gtk_selection_model_is_selected (GTK_SELECTION_MODEL (p->selection), i))
    {
        GtkTreeListRow *tr = gtk_tree_list_model_get_row (p->rows, i);
        GncTreeModelPriceRow *row = row_from_item (tr);
        gnc_commodity *commodity = row? gnc_tree_model_price_row_get_commodity (row): NULL;
        if (commodity) result = g_list_prepend (result, commodity);
        g_object_unref (tr);
    }
    return g_list_reverse (result);
}
