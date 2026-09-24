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
#include "gnc-tree-view-commodity.h"
#include "gnc-tree-model-commodity.h"
#include "gnc-engine.h"
struct _GncTreeViewCommodity
{
    GncTreeView parent_instance;
};
typedef struct
{
    GncTreeModelCommodity *model;
    GListStore *roots;
    GtkTreeListModel *rows;
    GtkSingleSelection *selection;
    GHashTable *selected;
    GHashTable *expanded;
    gnc_tree_view_commodity_ns_filter_func ns_filter;
    gnc_tree_view_commodity_cm_filter_func cm_filter;
    gpointer filter_data;
    GDestroyNotify filter_destroy;
    GncTreeModelCommodityColumn sort_column;
    GtkSortType sort_order;
    GtkSorter *view_sorter;
    gulong view_sorter_changed_id;
    gboolean synchronizing;
    gboolean disposing;
    guint restore_source;
    struct _CommodityChildrenContext *children_context;
} GncTreeViewCommodityPrivate;

typedef struct _CommodityChildrenContext
{
    GWeakRef view;
    gboolean disposed;
} CommodityChildrenContext;
typedef struct
{
    gatomicrefcount ref_count;
    GWeakRef view;
    GncTreeModelCommodityColumn column;
    gboolean tree;
    gboolean toggle;
} CommodityColumn;
G_DEFINE_TYPE_WITH_PRIVATE (GncTreeViewCommodity, gnc_tree_view_commodity, GNC_TYPE_TREE_VIEW)

static GncTreeViewCommodityPrivate *
priv (GncTreeViewCommodity *view)
{
    return gnc_tree_view_commodity_get_instance_private (view);
}
static GncTreeModelCommodityRow *
row_from_item (gpointer item)
{
    if (!GTK_IS_TREE_LIST_ROW (item))
        return NULL;
    GObject *row_item = gtk_tree_list_row_get_item (GTK_TREE_LIST_ROW (item));
    GncTreeModelCommodityRow *row = GNC_TREE_MODEL_COMMODITY_ROW (row_item);
    g_clear_object (&row_item);
    return row;
}
static gboolean
row_visible (GncTreeViewCommodityPrivate *p, GncTreeModelCommodityRow *row)
{
    if (gnc_tree_model_commodity_row_get_kind (row) == GNC_TREE_MODEL_COMMODITY_ROW_NAMESPACE) return !p->ns_filter || p->ns_filter (gnc_tree_model_commodity_row_get_namespace (row), p->filter_data);
    return !p->cm_filter || p->cm_filter (gnc_tree_model_commodity_row_get_commodity (row), p->filter_data);
}
static gint
row_compare (gconstpointer left, gconstpointer right, gpointer data)
{
    GncTreeViewCommodityPrivate *p = data;
    gchar *a = gnc_tree_model_commodity_row_get_string (GNC_TREE_MODEL_COMMODITY_ROW ((gpointer)left), p->sort_column);
    gchar *b = gnc_tree_model_commodity_row_get_string (GNC_TREE_MODEL_COMMODITY_ROW ((gpointer)right), p->sort_column);
    gint result = g_utf8_collate (a? a: "", b? b: "");
    g_free (a);
    g_free (b);
    return p->sort_order == GTK_SORT_DESCENDING? -result: result;
}
static void
append_sorted_visible (GncTreeViewCommodityPrivate *p, GListStore *store,
                       GListModel *source)
{
    for (guint index = 0;
         !p->disposing && index < g_list_model_get_n_items (source); index++)
    {
        GncTreeModelCommodityRow *row = g_list_model_get_item (source, index);
        gboolean visible = row_visible (p, row);

        if (!p->disposing && visible)
            g_list_store_insert_sorted (store, row, row_compare, p);
        g_object_unref (row);
    }
}
static GListModel *
create_children (gpointer item, gpointer user_data)
{
    CommodityChildrenContext *context = user_data;
    GncTreeViewCommodity *view;
    GncTreeViewCommodityPrivate *p;
    GncTreeModelCommodityRow *row = GNC_TREE_MODEL_COMMODITY_ROW (item);
    GListModel *source = gnc_tree_model_commodity_row_get_children (row);
    GListStore *children;
    if (context->disposed || !source || g_list_model_get_n_items (source) == 0) return NULL;
    view = g_weak_ref_get (&context->view);
    if (!view) return NULL;
    p = priv (view);
    children = g_list_store_new (GNC_TYPE_TREE_MODEL_COMMODITY_ROW);
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
commodity_children_context_free (CommodityChildrenContext *context)
{
    g_weak_ref_clear (&context->view);
    g_free (context);
}

static void
commodity_column_free (CommodityColumn *column)
{
    g_weak_ref_clear (&column->view);
    g_free (column);
}

static CommodityColumn *
commodity_column_ref (CommodityColumn *column)
{
    g_atomic_ref_count_inc (&column->ref_count);
    return column;
}

static void
commodity_column_unref (CommodityColumn *column)
{
    if (g_atomic_ref_count_dec (&column->ref_count))
        commodity_column_free (column);
}

static void
commodity_column_closure_free (gpointer data, GClosure *closure)
{
    commodity_column_unref (data);
    (void)closure;
}

static GncTreeViewCommodity *
commodity_column_get_view (CommodityColumn *column)
{
    return g_weak_ref_get (&column->view);
}

static void
rebuild_roots (GncTreeViewCommodity *view)
{
    GncTreeViewCommodityPrivate *p = priv (view);
    GListStore *roots;
    GncTreeModelCommodity *model;
    GtkSingleSelection *selection = NULL;

    if (p->disposing || !p->roots || !p->model)
        return;
    roots = g_object_ref (p->roots);
    model = g_object_ref (p->model);
    if (p->selection)
        selection = g_object_ref (p->selection);
    if (selection)
    {
        p->synchronizing = TRUE;
        gtk_selection_model_unselect_all (GTK_SELECTION_MODEL (selection));
    }
    if (!p->disposing)
        g_list_store_remove_all (roots);
    if (!p->disposing)
        append_sorted_visible (p, roots,
                               gnc_tree_model_commodity_get_roots (model));
    g_clear_object (&selection);
    g_object_unref (model);
    g_object_unref (roots);
}
static gboolean
restore_state (gpointer data)
{
    GncTreeViewCommodity *view = GNC_TREE_VIEW_COMMODITY (data);
    GncTreeViewCommodityPrivate *p = priv (view);
    GtkTreeListModel *rows;
    GtkSingleSelection *selection;
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
    for (guint position = 0;
         !p->disposing &&
         position < g_list_model_get_n_items (G_LIST_MODEL (rows)); position++)
    {
        GtkTreeListRow *tree_row = gtk_tree_list_model_get_row (rows, position);
        GncTreeModelCommodityRow *row = row_from_item (tree_row);
        if (row && gtk_tree_list_row_is_expandable (tree_row) &&
            g_hash_table_contains (expanded,
                                   gnc_tree_model_commodity_row_get_id (row)) &&
            !gtk_tree_list_row_get_expanded (tree_row))
        {
            gtk_tree_list_row_set_expanded (tree_row, TRUE);
            expanded_any = TRUE;
        }
        g_object_unref (tree_row);
    }
    if (!p->disposing && !expanded_any)
    {
        guint desired_position = GTK_INVALID_LIST_POSITION;
        guint n_items = g_list_model_get_n_items (G_LIST_MODEL (rows));

        for (guint position = 0; position < n_items; position++)
        {
            GtkTreeListRow *tree_row = gtk_tree_list_model_get_row (rows, position);
            GncTreeModelCommodityRow *row = row_from_item (tree_row);

            if (row &&
                g_hash_table_contains (selected,
                                       gnc_tree_model_commodity_row_get_id (row)))
                desired_position = position;
            g_object_unref (tree_row);
            if (desired_position != GTK_INVALID_LIST_POSITION)
                break;
        }
        if (!p->disposing)
            /* GtkSingleSelection deliberately implements select_item rather
             * than the optional set_selection operation. Set its one desired
             * row atomically, including the empty-selection case. */
            gtk_single_selection_set_selected (selection, desired_position);
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
schedule_restore (GncTreeViewCommodity *view)
{
    GncTreeViewCommodityPrivate *p = priv (view);
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
model_changed (GncTreeModelCommodity *model, GncTreeViewCommodity *view)
{
    (void)model;
    rebuild_roots (view);
    schedule_restore (view);
}
static void
selection_changed (GtkSelectionModel *selection, guint position, guint n_items, GncTreeViewCommodity *view)
{
    GncTreeViewCommodityPrivate *p = priv (view);
    if (p->disposing || p->synchronizing) return;
    g_hash_table_remove_all (p->selected);
    for (guint i = 0; i < g_list_model_get_n_items (G_LIST_MODEL (p->rows)); i++)
    {
        GtkTreeListRow *tree_row = gtk_tree_list_model_get_row (p->rows, i);
        GncTreeModelCommodityRow *row = row_from_item (tree_row);
        if (row && gtk_selection_model_is_selected (selection, i)) g_hash_table_add (p->selected, g_strdup (gnc_tree_model_commodity_row_get_id (row)));
        g_object_unref (tree_row);
    }
    (void)position;
    (void)n_items;
}
static void
row_expanded (GtkTreeListRow *tree_row, GParamSpec *pspec, GncTreeViewCommodity *view)
{
    GncTreeViewCommodityPrivate *p = priv (view);
    GncTreeModelCommodityRow *row = row_from_item (tree_row);
    if (row && !p->disposing && !p->synchronizing)
    {
        const gchar *id = gnc_tree_model_commodity_row_get_id (row);
        if (gtk_tree_list_row_get_expanded (tree_row)) g_hash_table_add (p->expanded, g_strdup (id));
        else g_hash_table_remove (p->expanded, id);
    }
    (void)pspec;
}
static void
factory_setup (GtkSignalListItemFactory *factory, GtkListItem *item, CommodityColumn *column)
{
    GtkWidget *child = column->toggle? gtk_check_button_new (): gtk_label_new (NULL);
    gtk_widget_set_halign (child, column->toggle? GTK_ALIGN_CENTER: GTK_ALIGN_START);
    if (column->tree)
    {
        GtkWidget *expander = gtk_tree_expander_new ();
        gtk_tree_expander_set_child (GTK_TREE_EXPANDER (expander), child);
        gtk_list_item_set_child (item, expander);
    }
    else gtk_list_item_set_child (item, child);
    (void)factory;
}
static void
factory_bind (GtkSignalListItemFactory *factory, GtkListItem *item, CommodityColumn *column)
{
    GncTreeViewCommodity *view = commodity_column_get_view (column);
    GtkTreeListRow *tree_row = GTK_TREE_LIST_ROW (gtk_list_item_get_item (item));
    GncTreeModelCommodityRow *row = row_from_item (tree_row);
    GtkWidget *child = gtk_list_item_get_child (item);
    GtkWidget *value = column->tree? gtk_tree_expander_get_child (GTK_TREE_EXPANDER (child)): child;

    if (!view || priv (view)->disposing)
    {
        g_clear_object (&view);
        return;
    }
    if (column->tree) gtk_tree_expander_set_list_row (GTK_TREE_EXPANDER (child), tree_row);
    if (column->toggle) gtk_check_button_set_active (GTK_CHECK_BUTTON (value), gnc_tree_model_commodity_row_get_boolean (row, column->column));
    else
    {
        gchar *text = gnc_tree_model_commodity_row_get_string (row, column->column);
        gtk_label_set_text (GTK_LABEL (value), text);
        g_free (text);
    }
    g_signal_connect_object (tree_row, "notify::expanded", G_CALLBACK (row_expanded), view, 0);
    g_object_unref (view);
    (void)factory;
}
static void
factory_unbind (GtkSignalListItemFactory *factory, GtkListItem *item, CommodityColumn *column)
{
    GncTreeViewCommodity *view = commodity_column_get_view (column);
    GtkTreeListRow *tree_row = GTK_TREE_LIST_ROW (gtk_list_item_get_item (item));
    if (view)
    {
        g_signal_handlers_disconnect_by_func (tree_row, row_expanded, view);
        g_object_unref (view);
    }
    (void)factory;
}
static GtkOrdering
sort_cb (gconstpointer left, gconstpointer right, gpointer user_data)
{
    CommodityColumn *column = user_data;
    GncTreeModelCommodityRow *a = row_from_item ((gpointer)left), *b = row_from_item ((gpointer)right);
    gchar *sa = gnc_tree_model_commodity_row_get_string (a, column->column), *sb = gnc_tree_model_commodity_row_get_string (b, column->column);
    gint result = g_utf8_collate (sa, sb);
    g_free (sa);
    g_free (sb);
    return result < 0? GTK_ORDERING_SMALLER: result > 0? GTK_ORDERING_LARGER: GTK_ORDERING_EQUAL;
}
static void
sort_changed (GtkSorter *sorter, GtkSorterChange change,
              GncTreeViewCommodity *view)
{
    GncTreeViewCommodityPrivate *p;
    GtkColumnViewColumn *column_view;
    CommodityColumn *column;

    g_object_ref (view);
    if (priv (view)->disposing)
        goto cleanup;
    column_view = gtk_column_view_sorter_get_primary_sort_column
        (GTK_COLUMN_VIEW_SORTER (sorter));
    if (!column_view)
        goto cleanup;
    column = g_object_get_data (G_OBJECT (column_view),
                                "gnc-commodity-column");
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
add_column (GncTreeViewCommodity *view, const gchar *title, const gchar *id, GncTreeModelCommodityColumn value, gboolean tree, gboolean toggle, gboolean visible)
{
    CommodityColumn *data = g_new0 (CommodityColumn, 1);
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkCustomSorter *sorter;
    GtkColumnViewColumn *column;
    g_atomic_ref_count_init (&data->ref_count);
    g_weak_ref_init (&data->view, view);
    data->column = value;
    data->tree = tree;
    data->toggle = toggle;
    g_signal_connect_data (factory, "setup", G_CALLBACK (factory_setup),
                           commodity_column_ref (data), commodity_column_closure_free, 0);
    g_signal_connect_data (factory, "bind", G_CALLBACK (factory_bind),
                           commodity_column_ref (data), commodity_column_closure_free, 0);
    g_signal_connect_data (factory, "unbind", G_CALLBACK (factory_unbind),
                           commodity_column_ref (data), commodity_column_closure_free, 0);
    column = gtk_column_view_column_new (title, GTK_LIST_ITEM_FACTORY (factory));
    gtk_column_view_column_set_id (column, id);
    gtk_column_view_column_set_resizable (column, TRUE);
    gtk_column_view_column_set_expand (column, tree);
    gtk_column_view_column_set_visible (column, visible);
    sorter = gtk_custom_sorter_new (sort_cb, commodity_column_ref (data),
                                    (GDestroyNotify)commodity_column_unref);
    gtk_column_view_column_set_sorter (column, GTK_SORTER (sorter));
    g_object_set_data_full (G_OBJECT (column), "gnc-commodity-column",
                            commodity_column_ref (data),
                            (GDestroyNotify)commodity_column_unref);
    gtk_column_view_append_column (gnc_tree_view_get_column_view (GNC_TREE_VIEW (view)), column);
    g_object_unref (sorter);
    g_object_unref (column);
    commodity_column_unref (data);
    return column;
}
static void
view_dispose (GObject *object)
{
    GncTreeViewCommodity *view = GNC_TREE_VIEW_COMMODITY (object);
    GncTreeViewCommodityPrivate *p = priv (view);
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
    if (filter_destroy) filter_destroy (filter_data);
    g_clear_pointer (&p->selected, g_hash_table_unref);
    g_clear_pointer (&p->expanded, g_hash_table_unref);
    g_clear_object (&p->selection);
    g_clear_object (&p->rows);
    g_clear_object (&p->roots);
    g_clear_object (&p->model);
    g_clear_object (&p->view_sorter);
    G_OBJECT_CLASS (gnc_tree_view_commodity_parent_class)->dispose (object);
}
static void
gnc_tree_view_commodity_class_init (GncTreeViewCommodityClass *klass)
{
    G_OBJECT_CLASS (klass)->dispose = view_dispose;
}
static void
gnc_tree_view_commodity_init (GncTreeViewCommodity *view)
{
    GncTreeViewCommodityPrivate *p = priv (view);
    p->selected = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    p->expanded = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    p->sort_column = GNC_TREE_MODEL_COMMODITY_COL_FULLNAME;
    p->sort_order = GTK_SORT_ASCENDING;
}
GtkWidget *
gnc_tree_view_commodity_new (QofBook *book, const gchar *first_property_name, ...)
{
    GncTreeViewCommodity *view = g_object_new (GNC_TYPE_TREE_VIEW_COMMODITY, "name", "gnc-id-commodity-tree", NULL);
    GncTreeViewCommodityPrivate *p = priv (view);
    GtkColumnView *column_view;
    GtkColumnViewColumn *default_column;
    va_list args;
    p->model = gnc_tree_model_commodity_new (book, gnc_commodity_table_get_table (book));
    p->roots = g_list_store_new (GNC_TYPE_TREE_MODEL_COMMODITY_ROW);
    p->children_context = g_new0 (CommodityChildrenContext, 1);
    g_weak_ref_init (&p->children_context->view, view);
    rebuild_roots (view);
    p->rows = gtk_tree_list_model_new (g_object_ref (G_LIST_MODEL (p->roots)), FALSE, FALSE,
                                       create_children, p->children_context,
                                       (GDestroyNotify) commodity_children_context_free);
    p->selection = gtk_single_selection_new (g_object_ref (G_LIST_MODEL (p->rows)));
    gtk_single_selection_set_autoselect (p->selection, FALSE);
    gtk_single_selection_set_can_unselect (p->selection, TRUE);
    gtk_single_selection_set_selected (p->selection, GTK_INVALID_LIST_POSITION);
    column_view = gnc_tree_view_get_column_view (GNC_TREE_VIEW (view));
    gtk_column_view_set_model (column_view, GTK_SELECTION_MODEL (p->selection));
    add_column (view, _("Namespace"), "namespace", GNC_TREE_MODEL_COMMODITY_COL_NAMESPACE, TRUE, FALSE, TRUE);
    add_column (view, _("Symbol"), "symbol", GNC_TREE_MODEL_COMMODITY_COL_MNEMONIC, FALSE, FALSE, TRUE);
    default_column = add_column (view, _("Name"), "name", GNC_TREE_MODEL_COMMODITY_COL_FULLNAME, FALSE, FALSE, TRUE);
    add_column (view, _("Print Name"), "printname", GNC_TREE_MODEL_COMMODITY_COL_PRINTNAME, FALSE, FALSE, FALSE);
    add_column (view, _("Display symbol"), "user_symbol", GNC_TREE_MODEL_COMMODITY_COL_USER_SYMBOL, FALSE, FALSE, TRUE);
    add_column (view, _("Unique Name"), "uniquename", GNC_TREE_MODEL_COMMODITY_COL_UNIQUE_NAME, FALSE, FALSE, FALSE);
    add_column (view, _("ISIN/CUSIP"), "cusip_code", GNC_TREE_MODEL_COMMODITY_COL_CUSIP, FALSE, FALSE, TRUE);
    add_column (view, _("Fraction"), "fraction", GNC_TREE_MODEL_COMMODITY_COL_FRACTION, FALSE, FALSE, TRUE);
    add_column (view, _("Get Quotes"), "quote_flag", GNC_TREE_MODEL_COMMODITY_COL_QUOTE_FLAG, FALSE, TRUE, TRUE);
    add_column (view, _("Source"), "quote_source", GNC_TREE_MODEL_COMMODITY_COL_QUOTE_SOURCE, FALSE, FALSE, FALSE);
    add_column (view, _("Timezone"), "quote_timezone", GNC_TREE_MODEL_COMMODITY_COL_QUOTE_TZ, FALSE, FALSE, TRUE);
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
gnc_tree_view_commodity_get_column_view (GncTreeViewCommodity *view)
{
    g_return_val_if_fail (GNC_IS_TREE_VIEW_COMMODITY (view), NULL);
    return gnc_tree_view_get_column_view (GNC_TREE_VIEW (view));
}
GtkSelectionModel *
gnc_tree_view_commodity_get_selection_model (GncTreeViewCommodity *view)
{
    g_return_val_if_fail (GNC_IS_TREE_VIEW_COMMODITY (view), NULL);
    return GTK_SELECTION_MODEL (priv (view)->selection);
}
void
gnc_tree_view_commodity_configure_columns (GncTreeViewCommodity *view, GSList *names)
{
    GListModel *columns = gtk_column_view_get_columns (gnc_tree_view_commodity_get_column_view (view));
    for (guint i = 0; i < g_list_model_get_n_items (columns); i++)
    {
        GtkColumnViewColumn *column = g_list_model_get_item (columns, i);
        const gchar *id = gtk_column_view_column_get_id (column);
        gtk_column_view_column_set_visible (column, g_slist_find_custom (names, id, (GCompareFunc)g_strcmp0) != NULL || g_strcmp0 (id, "namespace") == 0);
        g_object_unref (column);
    }
}
void
gnc_tree_view_commodity_set_filter (GncTreeViewCommodity *view, gnc_tree_view_commodity_ns_filter_func ns, gnc_tree_view_commodity_cm_filter_func cm, gpointer data, GDestroyNotify destroy)
{
    GncTreeViewCommodityPrivate *p = priv (view);
    if (p->filter_destroy) p->filter_destroy (p->filter_data);
    p->ns_filter = ns;
    p->cm_filter = cm;
    p->filter_data = data;
    p->filter_destroy = destroy;
    rebuild_roots (view);
    schedule_restore (view);
}
void
gnc_tree_view_commodity_refilter (GncTreeViewCommodity *view)
{
    rebuild_roots (view);
    schedule_restore (view);
}
static GncTreeModelCommodityRow *
first_selected (GncTreeViewCommodity *view)
{
    GncTreeViewCommodityPrivate *p = priv (view);
    for (guint i = 0; i < g_list_model_get_n_items (G_LIST_MODEL (p->rows)); i++) if (gtk_selection_model_is_selected (GTK_SELECTION_MODEL (p->selection), i))
    {
        GtkTreeListRow *tr = gtk_tree_list_model_get_row (p->rows, i);
        GncTreeModelCommodityRow *row = tr? g_object_ref (row_from_item (tr)): NULL;
        g_clear_object (&tr);
        return row;
    }
    return NULL;
}
gnc_commodity *
gnc_tree_view_commodity_get_cursor_commodity (GncTreeViewCommodity *view)
{
    GncTreeModelCommodityRow *row = first_selected (view);
    gnc_commodity *commodity = row? gnc_tree_model_commodity_row_get_commodity (row): NULL;
    g_clear_object (&row);
    return commodity;
}
gnc_commodity *
gnc_tree_view_commodity_get_selected_commodity (GncTreeViewCommodity *view)
{
    return gnc_tree_view_commodity_get_cursor_commodity (view);
}
gnc_commodity_namespace *
gnc_tree_view_commodity_get_selected_namespace (GncTreeViewCommodity *view)
{
    GncTreeModelCommodityRow *row = first_selected (view);
    gnc_commodity_namespace *ns = row? gnc_tree_model_commodity_row_get_namespace (row): NULL;
    g_clear_object (&row);
    return ns;
}
void
gnc_tree_view_commodity_select_commodity (GncTreeViewCommodity *view, gnc_commodity *commodity)
{
    GncTreeViewCommodityPrivate *p = priv (view);
    gchar guid[GUID_ENCODING_LENGTH + 1];
    gchar *id;
    if (!commodity) return;
    guid_to_string_buff (qof_instance_get_guid (QOF_INSTANCE (commodity)), guid);
    id = g_strconcat ("commodity:", guid, NULL);
    g_hash_table_remove_all (p->selected);
    g_hash_table_add (p->selected, id);
    gnc_commodity_namespace *ns = gnc_commodity_get_namespace_ds (commodity);
    g_hash_table_add (p->expanded, g_strconcat ("namespace:", gnc_commodity_namespace_get_name (ns), NULL));
    schedule_restore (view);
}
void
gnc_tree_view_commodity_select_subcommodities (GncTreeViewCommodity *view, gnc_commodity *commodity)
{
    gnc_tree_view_commodity_select_commodity (view, commodity);
}
