/********************************************************************\
 * gnc-tree-view-price.c -- GtkTreeView implementation to display   *
 *                            prices in a GtkTreeView.              *
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

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include "gnc-tree-view.h"
#include "gnc-tree-model-price.h"
#include "gnc-tree-view-price.h"

#include "gnc-pricedb.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-glib-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-icons.h"
#include "gnc-ui-util.h"


/** Static Globals *******************************************************/

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

/** Declarations *********************************************************/
static void gnc_tree_view_price_class_init (GncTreeViewPriceClass *klass);
static void gnc_tree_view_price_init (GncTreeViewPrice *view);
static void gnc_tree_view_price_finalize (GObject *object);
static void gnc_tree_view_price_destroy (GtkWidget *widget);

typedef struct GncTreeViewPricePrivate
{
    gpointer dummy;
} GncTreeViewPricePrivate;

#define GNC_TREE_VIEW_PRICE_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_TREE_VIEW_PRICE, GncTreeViewPricePrivate))


/************************************************************/
/*               g_object required functions                */
/************************************************************/

static GObjectClass *parent_class = NULL;

G_DEFINE_TYPE_WITH_PRIVATE(GncTreeViewPrice, gnc_tree_view_price, GNC_TYPE_TREE_VIEW)

static void
gnc_tree_view_price_class_init (GncTreeViewPriceClass *klass)
{
    GObjectClass *o_class;
    GtkWidgetClass *widget_class;

    parent_class = g_type_class_peek_parent (klass);

    o_class = G_OBJECT_CLASS (klass);
    widget_class = GTK_WIDGET_CLASS (klass);

    /* GObject signals */
    o_class->finalize = gnc_tree_view_price_finalize;

    /* GtkWidget signals */
    widget_class->destroy = gnc_tree_view_price_destroy;
}

static void
gnc_tree_view_price_init (GncTreeViewPrice *view)
{
}

static void
gnc_tree_view_price_finalize (GObject *object)
{
    ENTER("view %p", object);
    gnc_leave_return_if_fail (object != NULL);
    gnc_leave_return_if_fail (GNC_IS_TREE_VIEW_PRICE (object));

    if (G_OBJECT_CLASS (parent_class)->finalize)
        (* G_OBJECT_CLASS (parent_class)->finalize) (object);
    LEAVE(" ");
}

static void
gnc_tree_view_price_destroy (GtkWidget *widget)
{
    ENTER("view %p", widget);
    gnc_leave_return_if_fail (widget != NULL);
    gnc_leave_return_if_fail (GNC_IS_TREE_VIEW_PRICE (widget));

    if (GTK_WIDGET_CLASS (parent_class)->destroy)
        (* GTK_WIDGET_CLASS (parent_class)->destroy) (widget);
    LEAVE(" ");
}


/************************************************************/
/*                      sort functions                      */
/************************************************************/

static gboolean
get_prices (GtkTreeModel *f_model,
            GtkTreeIter *f_iter_a,
            GtkTreeIter *f_iter_b,
            GNCPrice **price_a,
            GNCPrice **price_b)
{
    GncTreeModelPrice *model;
    GtkTreeModel *tree_model;
    GtkTreeIter iter_a, iter_b;

    tree_model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
    model = GNC_TREE_MODEL_PRICE(tree_model);

    gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
            &iter_a,
            f_iter_a);

    /* The iters must point to prices for this to be meaningful */
    if (!gnc_tree_model_price_iter_is_price (model, &iter_a))
        return FALSE;

    gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
            &iter_b,
            f_iter_b);

    *price_a = gnc_tree_model_price_get_price (model, &iter_a);
    *price_b = gnc_tree_model_price_get_price (model, &iter_b);
    return TRUE;
}

static gint
sort_ns_or_cm (GtkTreeModel *f_model,
               GtkTreeIter *f_iter_a,
               GtkTreeIter *f_iter_b)
{
    GncTreeModelPrice *model;
    GtkTreeModel *tree_model;
    GtkTreeIter iter_a, iter_b;
    gnc_commodity_namespace *ns_a, *ns_b;
    gnc_commodity *comm_a, *comm_b;

    tree_model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
    model = GNC_TREE_MODEL_PRICE(tree_model);

    gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
            &iter_a,
            f_iter_a);
    gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
            &iter_b,
            f_iter_b);

    if (gnc_tree_model_price_iter_is_namespace (model, &iter_a))
    {
        ns_a = gnc_tree_model_price_get_namespace (model, &iter_a);
        ns_b = gnc_tree_model_price_get_namespace (model, &iter_b);
        return safe_utf8_collate (gnc_commodity_namespace_get_gui_name (ns_a),
                                  gnc_commodity_namespace_get_gui_name (ns_b));
    }

    comm_a = gnc_tree_model_price_get_commodity (model, &iter_a);
    comm_b = gnc_tree_model_price_get_commodity (model, &iter_b);
    return safe_utf8_collate (gnc_commodity_get_mnemonic (comm_a),
                              gnc_commodity_get_mnemonic (comm_b));
}

static gint
default_sort (GNCPrice *price_a, GNCPrice *price_b)
{
    gnc_commodity *curr_a, *curr_b;
    time64 time_a, time_b;
    gint result;

    /* Primary sort (i.e. commodity name) handled by the tree structure.  */

    /* secondary sort: currency */
    curr_a = gnc_price_get_currency (price_a);
    curr_b = gnc_price_get_currency (price_b);

    result = safe_utf8_collate (gnc_commodity_get_namespace (curr_a),
                                gnc_commodity_get_namespace (curr_b));
    if (result != 0) return result;

    result = safe_utf8_collate (gnc_commodity_get_mnemonic (curr_a),
                                gnc_commodity_get_mnemonic (curr_b));
    if (result != 0) return result;

    /* tertiary sort: time */
    time_a = gnc_price_get_time64 (price_a);
    time_b = gnc_price_get_time64 (price_b);
    result = time_a < time_b ? -1 : time_a > time_b ? 1 : 0;
    if (result)
        /* Reverse the result to present the most recent quote first. */
        return -result;

    /* last sort: value */
    return gnc_numeric_compare (gnc_price_get_value (price_a),
                                gnc_price_get_value (price_b));
}

static gint
sort_by_name (GtkTreeModel *f_model,
              GtkTreeIter *f_iter_a,
              GtkTreeIter *f_iter_b,
              gpointer user_data)
{
    GNCPrice *price_a, *price_b;

    if (!get_prices (f_model, f_iter_a, f_iter_b, &price_a, &price_b))
        return sort_ns_or_cm (f_model, f_iter_a, f_iter_b);

    return default_sort (price_a, price_b);
}

static gint
sort_by_date (GtkTreeModel *f_model,
              GtkTreeIter *f_iter_a,
              GtkTreeIter *f_iter_b,
              gpointer user_data)
{
    GNCPrice *price_a, *price_b;
    time64 time_a, time_b;
    gboolean result;

    if (!get_prices (f_model, f_iter_a, f_iter_b, &price_a, &price_b))
        return sort_ns_or_cm (f_model, f_iter_a, f_iter_b);

    /* sort by time first */
    time_a = gnc_price_get_time64 (price_a);
    time_b = gnc_price_get_time64 (price_b);
    result = time_a < time_b ? -1 : time_a > time_b ? 1 : 0;
    if (result)
        /* Reverse the result to present the most recent quote first. */
        return -result;

    return default_sort (price_a, price_b);
}

static gint
sort_by_source (GtkTreeModel *f_model,
                GtkTreeIter *f_iter_a,
                GtkTreeIter *f_iter_b,
                gpointer user_data)
{
    GNCPrice *price_a, *price_b;
    gint result;

    if (!get_prices (f_model, f_iter_a, f_iter_b, &price_a, &price_b))
        return sort_ns_or_cm (f_model, f_iter_a, f_iter_b);

    /* sort by source first */
    result = gnc_price_get_source (price_a) < gnc_price_get_source (price_b);
    if (result != 0)
        return result;

    return default_sort (price_a, price_b);
}

static gint
sort_by_type (GtkTreeModel *f_model,
              GtkTreeIter *f_iter_a,
              GtkTreeIter *f_iter_b,
              gpointer user_data)
{
    GNCPrice *price_a, *price_b;
    gint result;

    if (!get_prices (f_model, f_iter_a, f_iter_b, &price_a, &price_b))
        return sort_ns_or_cm (f_model, f_iter_a, f_iter_b);

    /* sort by source first */
    result = safe_utf8_collate (gnc_price_get_typestr (price_a),
                                gnc_price_get_typestr (price_b));
    if (result != 0)
        return result;

    return default_sort (price_a, price_b);
}

static gint
sort_by_value (GtkTreeModel *f_model,
               GtkTreeIter *f_iter_a,
               GtkTreeIter *f_iter_b,
               gpointer user_data)
{
    gnc_commodity *comm_a, *comm_b;
    GNCPrice *price_a, *price_b;
    gboolean result;
    gint value;

    if (!get_prices (f_model, f_iter_a, f_iter_b, &price_a, &price_b))
        return sort_ns_or_cm (f_model, f_iter_a, f_iter_b);

    /*
     * Sorted by commodity because of the tree structure.  Now sort by
     * currency so we're only comparing numbers in the same currency
     * denomination.
     */
    comm_a = gnc_price_get_currency (price_a);
    comm_b = gnc_price_get_currency (price_b);
    if (comm_a && comm_b)
    {
        value = safe_utf8_collate (gnc_commodity_get_namespace (comm_a),
                                   gnc_commodity_get_namespace (comm_b));
        if (value != 0)
            return value;
        value = safe_utf8_collate (gnc_commodity_get_mnemonic (comm_a),
                                   gnc_commodity_get_mnemonic (comm_b));
        if (value != 0)
            return value;
    }

    /*
     * Now do the actual price comparison now we're sure that its an
     * apples to apples comparison.
     */
    result = gnc_numeric_compare (gnc_price_get_value (price_a),
                                  gnc_price_get_value (price_b));
    if (result)
        return result;

    return default_sort (price_a, price_b);
}


/************************************************************/
/*                    New View Creation                     */
/************************************************************/

/*
 * Create a new price tree view with (optional) top level root node.
 * This view will be based on a model that is common to all view of
 * the same set of books, but will have its own private filter on that
 * model.
 */
GtkTreeView *
gnc_tree_view_price_new (QofBook *book,
                         const gchar *first_property_name,
                         ...)
{
    GncTreeView *view;
    GtkTreeModel *model, *f_model, *s_model;
    GtkTreeViewColumn *col;
    GNCPriceDB *price_db;
    va_list var_args;
    const gchar *sample_text;
    gchar *sample_text2;

    ENTER(" ");
    /* Create/get a pointer to the existing model for this set of books. */
    price_db = gnc_pricedb_get_db(book);
    model = gnc_tree_model_price_new (book, price_db);

    /* Set up the view private filter on the common model. */
    f_model = gtk_tree_model_filter_new (model, NULL);
    g_object_unref(G_OBJECT(model));
    s_model = gtk_tree_model_sort_new_with_model (f_model);
    g_object_unref(G_OBJECT(f_model));

    /* Create our view */
    view = g_object_new (GNC_TYPE_TREE_VIEW_PRICE,
                         "name", "price_tree", NULL);
    gtk_tree_view_set_model (GTK_TREE_VIEW (view), s_model);
    g_object_unref(G_OBJECT(s_model));

    DEBUG("model ref count is %d",   G_OBJECT(model)->ref_count);
    DEBUG("f_model ref count is %d", G_OBJECT(f_model)->ref_count);
    DEBUG("s_model ref count is %d", G_OBJECT(s_model)->ref_count);

    sample_text = gnc_commodity_get_printname(gnc_default_currency());
    sample_text2 = g_strdup_printf("%s%s", sample_text, sample_text);
    gnc_tree_view_add_text_column (
              view, _("Security"), "security", NULL, sample_text2,
              GNC_TREE_MODEL_PRICE_COL_COMMODITY,
              GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
              sort_by_name);
    g_free(sample_text2);
    col = gnc_tree_view_add_text_column (
              view, _("Currency"), "currency", NULL, sample_text,
              GNC_TREE_MODEL_PRICE_COL_CURRENCY,
              GNC_TREE_MODEL_PRICE_COL_VISIBILITY,
              sort_by_name);
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
    col = gnc_tree_view_add_text_column (
              view, _("Date"), "date", NULL, "2005-05-20",
              GNC_TREE_MODEL_PRICE_COL_DATE,
              GNC_TREE_MODEL_PRICE_COL_VISIBILITY,
              sort_by_date);
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
    col = gnc_tree_view_add_text_column (
              view, _("Source"), "source", NULL, "Finance::Quote",
              GNC_TREE_MODEL_PRICE_COL_SOURCE,
              GNC_TREE_MODEL_PRICE_COL_VISIBILITY,
              sort_by_source);
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
    col = gnc_tree_view_add_text_column (
              view, _("Type"), "type", NULL, "last",
              GNC_TREE_MODEL_PRICE_COL_TYPE,
              GNC_TREE_MODEL_PRICE_COL_VISIBILITY,
              sort_by_type);
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
    col = gnc_tree_view_add_numeric_column (
              view, _("Price"), "price", "100.00000",
              GNC_TREE_MODEL_PRICE_COL_VALUE,
              GNC_TREE_VIEW_COLUMN_COLOR_NONE,
              GNC_TREE_MODEL_PRICE_COL_VISIBILITY,
              sort_by_value);
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));

    gnc_tree_view_configure_columns(view);

    /* Set properties */
    va_start (var_args, first_property_name);
    g_object_set_valist (G_OBJECT(view), first_property_name, var_args);
    va_end (var_args);

    /* Sort on the commodity column by default. This allows for a consistent
     * sort if commodities are removed and re-added from the model. */
    if (!gtk_tree_sortable_get_sort_column_id(GTK_TREE_SORTABLE(s_model),
            NULL, NULL))
    {
        gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(s_model),
                                             GNC_TREE_MODEL_PRICE_COL_COMMODITY,
                                             GTK_SORT_ASCENDING);
    }

    gtk_widget_show(GTK_WIDGET(view));
    LEAVE(" %p", view);
    return GTK_TREE_VIEW(view);
}

/************************************************************/
/*                   Auxiliary Functions                    */
/************************************************************/

#define debug_path(fn, path) {				\
    gchar *path_string = gtk_tree_path_to_string(path); \
    fn("tree path %s", path_string);			\
    g_free(path_string);				\
  }

#if 0 /* Not Used */
static gboolean
gnc_tree_view_price_get_iter_from_price (GncTreeViewPrice *view,
        GNCPrice *price,
        GtkTreeIter *s_iter)
{
    GtkTreeModel *model, *f_model, *s_model;
    GtkTreeIter iter, f_iter;

    g_return_val_if_fail(GNC_IS_TREE_VIEW_PRICE(view), FALSE);
    g_return_val_if_fail(price != NULL, FALSE);
    g_return_val_if_fail(s_iter != NULL, FALSE);

    ENTER("view %p, price %p", view, price);

    /* Reach down to the real model and get an iter for this price */
    s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
    if (!gnc_tree_model_price_get_iter_from_price (GNC_TREE_MODEL_PRICE(model), price, &iter))
    {
        LEAVE("model_get_iter_from_price failed");
        return FALSE;
    }

    /* convert back to a sort iter */
    gtk_tree_model_filter_convert_child_iter_to_iter (GTK_TREE_MODEL_FILTER(f_model),
            &f_iter, &iter);
    gtk_tree_model_sort_convert_child_iter_to_iter (GTK_TREE_MODEL_SORT(s_model),
            s_iter, &f_iter);
    LEAVE(" ");
    return TRUE;
}
#endif /* Not Used */

/************************************************************/
/*            Price Tree View Filter Functions            */
/************************************************************/

/************************************************************/
/*          Price Tree View Visibility Filter           */
/************************************************************/

typedef struct
{
    gnc_tree_view_price_ns_filter_func user_ns_fn;
    gnc_tree_view_price_cm_filter_func user_cm_fn;
    gnc_tree_view_price_pc_filter_func user_pc_fn;
    gpointer                           user_data;
    GDestroyNotify                     user_destroy;
} filter_user_data;

static void
gnc_tree_view_price_filter_destroy (gpointer data)
{
    filter_user_data *fd = data;

    if (fd->user_destroy)
        fd->user_destroy(fd->user_data);
    g_free(fd);
}

static gboolean
gnc_tree_view_price_filter_helper (GtkTreeModel *model,
                                   GtkTreeIter *iter,
                                   gpointer data)
{
    gnc_commodity_namespace *name_space;
    gnc_commodity *commodity;
    GNCPrice *price;
    filter_user_data *fd = data;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE (model), FALSE);
    g_return_val_if_fail (iter != NULL, FALSE);

    if (gnc_tree_model_price_iter_is_namespace (GNC_TREE_MODEL_PRICE(model), iter))
    {
        if (fd->user_ns_fn)
        {
            name_space = gnc_tree_model_price_get_namespace (GNC_TREE_MODEL_PRICE(model), iter);
            return fd->user_ns_fn(name_space, fd->user_data);
        }
        return TRUE;
    }

    if (gnc_tree_model_price_iter_is_commodity (GNC_TREE_MODEL_PRICE(model), iter))
    {
        if (fd->user_cm_fn)
        {
            commodity = gnc_tree_model_price_get_commodity (GNC_TREE_MODEL_PRICE(model), iter);
            return fd->user_cm_fn(commodity, fd->user_data);
        }
        return TRUE;
    }

    if (gnc_tree_model_price_iter_is_price (GNC_TREE_MODEL_PRICE(model), iter))
    {
        if (fd->user_pc_fn)
        {
            price = gnc_tree_model_price_get_price (GNC_TREE_MODEL_PRICE(model), iter);
            return fd->user_pc_fn(price, fd->user_data);
        }
        return TRUE;
    }

    return FALSE;
}

/*
 * Set an GtkTreeModel visible filter on this price.  This filter will be
 * called for each price that the tree is about to show, and the
 * price will be passed to the callback function.
 */
void
gnc_tree_view_price_set_filter (GncTreeViewPrice *view,
                                gnc_tree_view_price_ns_filter_func ns_func,
                                gnc_tree_view_price_cm_filter_func cm_func,
                                gnc_tree_view_price_pc_filter_func pc_func,
                                gpointer data,
                                GDestroyNotify destroy)
{
    GtkTreeModel *f_model, *s_model;
    filter_user_data *fd = data;

    ENTER("view %p, ns func %p, cm func %p, pc func %p, data %p, destroy %p",
          view, ns_func, cm_func, pc_func, data, destroy);

    g_return_if_fail(GNC_IS_TREE_VIEW_PRICE(view));
    g_return_if_fail((ns_func != NULL) || (cm_func != NULL));

    fd = g_malloc(sizeof(filter_user_data));
    fd->user_ns_fn   = ns_func;
    fd->user_cm_fn   = cm_func;
    fd->user_pc_fn   = pc_func;
    fd->user_data    = data;
    fd->user_destroy = destroy;

    s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    gtk_tree_model_filter_set_visible_func (GTK_TREE_MODEL_FILTER (f_model),
                                            gnc_tree_view_price_filter_helper,
                                            fd,
                                            gnc_tree_view_price_filter_destroy);

    /* Whack any existing levels. The top two levels have been created
     * before this routine can be called.  Unfortunately, if the just
     * applied filter filters out all the nodes in the tree, the gtk
     * code throws a critical error.  This occurs when there are no
     * prices in the price database.  Once the very first price has been
     * added this error message goes away. */
    gtk_tree_model_filter_refilter (GTK_TREE_MODEL_FILTER (f_model));
    LEAVE(" ");
}

/************************************************************/
/*           Price Tree View Get/Set Functions            */
/************************************************************/

/*
 * Retrieve the selected price from an price tree view.  The
 * price tree must be in single selection mode.
 */
GNCPrice *
gnc_tree_view_price_get_selected_price (GncTreeViewPrice *view)
{
    GtkTreeSelection *selection;
    GtkTreeModel *model, *f_model, *s_model;
    GtkTreeIter iter, f_iter, s_iter;
    GNCPrice *price;

    ENTER("view %p", view);
    g_return_val_if_fail (GNC_IS_TREE_VIEW_PRICE (view), NULL);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(view));
    if (!gtk_tree_selection_get_selected (selection, &s_model, &s_iter))
    {
        LEAVE("no price, get_selected failed");
        return FALSE;
    }

    gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model),
            &f_iter, &s_iter);

    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER (f_model),
            &iter, &f_iter);

    model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
    price = gnc_tree_model_price_get_price (GNC_TREE_MODEL_PRICE(model),
                                            &iter);
    LEAVE("price %p", price);
    return price;
}

/*
 * Selects a single price in the price tree view.  The price
 * tree must be in single selection mode.
 */
void
gnc_tree_view_price_set_selected_price (GncTreeViewPrice *view,
                                        GNCPrice *price)
{
    GtkTreeModel *model, *f_model, *s_model;
    GtkTreePath *path, *f_path, *s_path, *parent_path;
    GtkTreeSelection *selection;

    ENTER("view %p, price %p", view, price);

    /* Clear any existing selection. */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
    gtk_tree_selection_unselect_all (selection);

    if (price == NULL)
        return;

    s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    model = gtk_tree_model_filter_get_model (GTK_TREE_MODEL_FILTER (f_model));

    path = gnc_tree_model_price_get_path_from_price (GNC_TREE_MODEL_PRICE(model), price);
    if (path == NULL)
    {
        LEAVE("get_path_from_price failed");
        return;
    }
    debug_path(DEBUG, path);

    f_path = gtk_tree_model_filter_convert_child_path_to_path (GTK_TREE_MODEL_FILTER (f_model),
             path);
    gtk_tree_path_free(path);
    if (f_path == NULL)
    {
        LEAVE("no filter path");
        return;
    }
    debug_path(DEBUG, f_path);

    s_path = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (s_model),
             f_path);
    gtk_tree_path_free(f_path);
    if (s_path == NULL)
    {
        LEAVE("no sort path");
        return;
    }

    /* gtk_tree_view requires that a row be visible before it can be selected */
    parent_path = gtk_tree_path_copy (s_path);
    if (gtk_tree_path_up (parent_path))
    {
        /* This function is misnamed.  It expands the actual item
         * specified, not the path to the item specified. I.E. It expands
         * one level too many, thus the get of the parent. */
        gtk_tree_view_expand_to_path(GTK_TREE_VIEW(view), parent_path);
    }
    gtk_tree_path_free(parent_path);

    gtk_tree_selection_select_path (selection, s_path);
    gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW(view), s_path, NULL, FALSE, 0.0, 0.0);
    debug_path(LEAVE, s_path);
    gtk_tree_path_free(s_path);
}

/*
 * This helper function is called once for each row in the tree view
 * that is currently selected.  Its task is to add the corresponding
 * price to the end of a glist.
 */
static void
get_selected_prices_helper (GtkTreeModel *s_model,
                            GtkTreePath *s_path,
                            GtkTreeIter *s_iter,
                            gpointer data)
{
    GList **return_list = data;
    GtkTreeModel *model, *f_model;
    GtkTreeIter iter, f_iter;
    GNCPrice *price;

    gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model),
            &f_iter, s_iter);

    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER (f_model),
            &iter, &f_iter);

    model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
    price = gnc_tree_model_price_get_price (GNC_TREE_MODEL_PRICE(model),
                                            &iter);
    if (price)
        *return_list = g_list_append(*return_list, price);
}

/*
 * Given an price tree view, return a list of the selected commodities. The
 * price tree must be in multiple selection mode.
 *
 * Note: It is the responsibility of the caller to free the returned
 * list.
 */
GList *
gnc_tree_view_price_get_selected_prices (GncTreeViewPrice *view)
{
    GtkTreeSelection *selection;
    GList *return_list = NULL;

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(view));
    gtk_tree_selection_selected_foreach(selection, get_selected_prices_helper, &return_list);
    return return_list;
}
