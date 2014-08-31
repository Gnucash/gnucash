/********************************************************************\
 * gnc-tree-view-owner.c -- GtkTreeView implementation to display   *
 *                            owners in a GtkTreeView.              *
 * Copyright (C) 2011 Geert Janssens <geert@kobaltwit.be>           *
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include "gnc-tree-view.h"
#include "gnc-tree-model-owner.h"
#include "gnc-tree-view-owner.h"

#include "gncOwner.h"
#include "gnc-accounting-period.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-glib-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-hooks.h"
#include "gnc-session.h"
#include "gnc-icons.h"
#include "gnc-ui-balances.h"
#include "dialog-utils.h"
#include "window-main-summarybar.h"
#include "assistant-utils.h"

#define SAMPLE_OWNER_VALUE "$1,000,000.00"

/** Static Globals *******************************************************/

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

/** Declarations *********************************************************/
static void gnc_tree_view_owner_class_init (GncTreeViewOwnerClass *klass);
static void gnc_tree_view_owner_init (GncTreeViewOwner *view);
static void gnc_tree_view_owner_finalize (GObject *object);

static void gtvo_update_column_names (GncTreeView *view);
static void gtvo_currency_changed_cb (void);

static gboolean gnc_tree_view_owner_filter_helper (GtkTreeModel *model,
        GtkTreeIter *iter,
        gpointer data);

#if 0 /* Not Used */
static void gtvo_setup_column_renderer_edited_cb(GncTreeViewOwner *owner_view,
        GtkTreeViewColumn *column,
        GtkCellRenderer *renderer,
        GncTreeViewOwnerColumnTextEdited col_edited_cb);
#endif /* Not Used */

typedef struct GncTreeViewOwnerPrivate
{
    OwnerViewInfo ovi;

    gnc_tree_view_owner_filter_func filter_fn;
    gpointer                          filter_data;
    GSourceFunc                       filter_destroy;

    GtkTreeViewColumn *name_column;
    GtkTreeViewColumn *id_column;
    GtkTreeViewColumn *balance_report_column;
    GtkTreeViewColumn *notes_column;
} GncTreeViewOwnerPrivate;

#define GNC_TREE_VIEW_OWNER_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_TREE_VIEW_OWNER, GncTreeViewOwnerPrivate))


/************************************************************/
/*               g_object required functions                */
/************************************************************/

static GObjectClass *parent_class = NULL;

GType
gnc_tree_view_owner_get_type (void)
{
    static GType gnc_tree_view_owner_type = 0;

    if (gnc_tree_view_owner_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncTreeViewOwnerClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_tree_view_owner_class_init,
            NULL,
            NULL,
            sizeof (GncTreeViewOwner),
            0,
            (GInstanceInitFunc) gnc_tree_view_owner_init
        };

        gnc_tree_view_owner_type = g_type_register_static (
                                       GNC_TYPE_TREE_VIEW, GNC_TREE_VIEW_OWNER_NAME,
                                       &our_info, 0);
    }

    return gnc_tree_view_owner_type;
}

static void
gnc_tree_view_owner_class_init (GncTreeViewOwnerClass *klass)
{
    GObjectClass *o_class;

    parent_class = g_type_class_peek_parent (klass);

    /* GObject signals */
    o_class = G_OBJECT_CLASS (klass);
    o_class->finalize = gnc_tree_view_owner_finalize;

    g_type_class_add_private(klass, sizeof(GncTreeViewOwnerPrivate));

    gnc_hook_add_dangler(HOOK_CURRENCY_CHANGED,
                         (GFunc)gtvo_currency_changed_cb, NULL);
}

/********************************************************************\
 * gnc_init_owner_view_info                                         *
 *   initialize an owner view info structure with default values    *
 *                                                                  *
 * Args: ovi - structure to initialize                              *
 * Returns: nothing                                                 *
\********************************************************************/
static void
gnc_init_owner_view_info(OwnerViewInfo *ovi)
{
    ovi->show_inactive = FALSE;
}

static void
gnc_tree_view_owner_init (GncTreeViewOwner *view)
{
    GncTreeViewOwnerPrivate *priv;

    priv = GNC_TREE_VIEW_OWNER_GET_PRIVATE(view);
    gnc_init_owner_view_info(&priv->ovi);
}

static void
gnc_tree_view_owner_finalize (GObject *object)
{
    GncTreeViewOwner *owner_view;
    GncTreeViewOwnerPrivate *priv;

    ENTER("view %p", object);
    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_TREE_VIEW_OWNER (object));

    owner_view = GNC_TREE_VIEW_OWNER (object);

    priv = GNC_TREE_VIEW_OWNER_GET_PRIVATE(owner_view);
    if (priv->filter_destroy)
    {
        priv->filter_destroy(priv->filter_data);
        priv->filter_destroy = NULL;
    }
    priv->filter_fn = NULL;

    if (G_OBJECT_CLASS (parent_class)->finalize)
        (* G_OBJECT_CLASS (parent_class)->finalize) (object);
    LEAVE(" ");
}


/************************************************************
 *                        Callbacks                         *
 ************************************************************/
static void
gnc_tree_view_owner_active_toggled (GtkCellRendererToggle *cell,
                                    const gchar *s_path_str,
                                    gpointer user_data)
{
    GncTreeViewOwner *tree_view;
    GtkTreePath *s_path;
    GncOwner *owner;
    gboolean active;

    /* Change the requested owner */
    tree_view = user_data;
    s_path = gtk_tree_path_new_from_string (s_path_str);
    owner = gnc_tree_view_owner_get_owner_from_path (tree_view, s_path);
    if (owner)
    {
        active = !gtk_cell_renderer_toggle_get_active (cell); // hasn't changed yet.
        gncOwnerSetActive (owner, active);
    }

    /* Clean up */
    gtk_tree_path_free (s_path);
}


/************************************************************/
/*                      sort functions                      */
/************************************************************/

static GtkTreeModel *
sort_cb_setup_w_iters (GtkTreeModel *f_model,
                       GtkTreeIter *f_iter_a,
                       GtkTreeIter *f_iter_b,
                       GtkTreeIter *iter_a,
                       GtkTreeIter *iter_b,
                       const GncOwner **owner_a,
                       const GncOwner **owner_b)
{
    GtkTreeModel *model;

    model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
    gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
            iter_a,
            f_iter_a);
    gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
            iter_b,
            f_iter_b);
    *owner_a = gnc_tree_model_owner_get_owner (GNC_TREE_MODEL_OWNER(model), iter_a);
    *owner_b = gnc_tree_model_owner_get_owner (GNC_TREE_MODEL_OWNER(model), iter_b);
    return model;
}

static void
sort_cb_setup (GtkTreeModel *f_model,
               GtkTreeIter *f_iter_a,
               GtkTreeIter *f_iter_b,
               const GncOwner **owner_a,
               const GncOwner **owner_b)
{
    GtkTreeIter iter_a, iter_b;

    sort_cb_setup_w_iters (f_model, f_iter_a, f_iter_b,
                           &iter_a, &iter_b, owner_a, owner_b);
}

static gint
sort_by_string (GtkTreeModel *f_model,
                GtkTreeIter *f_iter1,
                GtkTreeIter *f_iter2,
                gpointer user_data)
{
    GtkTreeModel *model;
    GtkTreeIter iter1, iter2;
    const GncOwner *owner1, *owner2;
    gchar *str1, *str2;
    gint column = GPOINTER_TO_INT(user_data);
    gint result;

    model = sort_cb_setup_w_iters(f_model, f_iter1, f_iter2, &iter1, &iter2, &owner1, &owner2);

    /* Get the strings. */
    gtk_tree_model_get(GTK_TREE_MODEL(model), &iter1,  column, &str1, -1);
    gtk_tree_model_get(GTK_TREE_MODEL(model), &iter2,  column, &str2, -1);

    result = safe_utf8_collate(str1, str2);
    g_free(str1);
    g_free(str2);
    if (result != 0)
        return result;
    return gncOwnerCompare(owner1, owner2);
}

static gint
sort_by_boolean (GtkTreeModel *f_model,
                 GtkTreeIter *f_iter1,
                 GtkTreeIter *f_iter2,
                 gpointer user_data)
{
    GtkTreeModel *model;
    GtkTreeIter iter1, iter2;
    const GncOwner *owner1, *owner2;
    gboolean *bool1, *bool2;
    gint column = GPOINTER_TO_INT(user_data);

    model = sort_cb_setup_w_iters(f_model, f_iter1, f_iter2, &iter1, &iter2, &owner1, &owner2);

    /* Get the booleans. */
    gtk_tree_model_get(GTK_TREE_MODEL(model), &iter1,  column, &bool1, -1);
    gtk_tree_model_get(GTK_TREE_MODEL(model), &iter2,  column, &bool2, -1);

    if (bool1)
    {
        if (!bool2)
            return 1; /* bool1 > bool2 */
    }
    else
    {
        if (bool2)
            return -1; /* bool2 > bool1 */
    }
    return gncOwnerCompare(owner1, owner2);
}

static gint
sort_by_xxx_value (GtkTreeModel *f_model,
                   GtkTreeIter *f_iter_a,
                   GtkTreeIter *f_iter_b,
                   gpointer user_data)
{
    GncOwner *owner_a, *owner_b;
    gnc_numeric balance_a, balance_b;
    gint result;

    /* Find the owners */
    sort_cb_setup (f_model, f_iter_a, f_iter_b, (const GncOwner**)&owner_a, (const GncOwner**)&owner_b);

    balance_a = gnc_ui_owner_get_balance_full(owner_a, NULL, NULL);
    balance_b = gnc_ui_owner_get_balance_full(owner_b, NULL, NULL);

    result = gnc_numeric_compare(balance_a, balance_b);
    if (result != 0)
        return result;
    return gncOwnerCompare(owner_a, owner_b);
}

static gint
sort_by_balance_value (GtkTreeModel *f_model,
                       GtkTreeIter *f_iter_a,
                       GtkTreeIter *f_iter_b,
                       gpointer user_data)
{
    return sort_by_xxx_value (f_model, f_iter_a, f_iter_b, user_data);
}


/************************************************************/
/*                    New View Creation                     */
/************************************************************/

/*
 * Create a new owner tree view for one type of owners.
 * This view will be based on a model that is common to all views of
 * the same set of books, but will have its own private filter on that
 * model.
 */
GtkTreeView *
gnc_tree_view_owner_new (GncOwnerType owner_type)
{
    GncTreeView *view;
    GtkTreeModel *model, *f_model, *s_model;
    const gchar *sample_type, *sample_currency;
    GncTreeViewOwnerPrivate *priv;

    ENTER(" ");
    /* Create our view */
    view = g_object_new (GNC_TYPE_TREE_VIEW_OWNER,
                         "name", "owner_tree", NULL);

    priv = GNC_TREE_VIEW_OWNER_GET_PRIVATE(GNC_TREE_VIEW_OWNER (view));

    /* Create/get a pointer to the existing model for this set of books. */
    model = gnc_tree_model_owner_new (owner_type);

    /* Set up the view private filter layer on the common model. */
    f_model = gtk_tree_model_filter_new (model, NULL);
    /* A GncTreeModelOwner is based on a GncTreeModel, which is a
     * GObject that provides a GtkTreeModel interface. */
    g_object_unref(G_OBJECT(model));

    /* Set up the view private sort layer on the common model. */
    s_model = gtk_tree_model_sort_new_with_model(f_model);
    g_object_unref(G_OBJECT(f_model));
    gtk_tree_view_set_model (GTK_TREE_VIEW (view), s_model);
    g_object_unref(G_OBJECT(s_model));

    /* Set default visibilities */
    gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(view), FALSE);

    sample_type = gncOwnerTypeToQofIdType (GNC_OWNER_CUSTOMER);
    sample_currency = gnc_commodity_get_fullname(gnc_default_currency());

    priv->name_column
        = gnc_tree_view_add_text_column(view, _("Owner Name"), GNC_OWNER_TREE_NAME_COL,
                                        NULL, "GnuCash Inc.",
                                        GNC_TREE_MODEL_OWNER_COL_NAME,
                                        GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                        sort_by_string);
    gnc_tree_view_add_text_column(view, _("Type"), GNC_OWNER_TREE_TYPE_COL,
                                  NULL, sample_type,
                                  GNC_TREE_MODEL_OWNER_COL_TYPE,
                                  GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                  sort_by_string);
    priv->id_column
        = gnc_tree_view_add_text_column(view, _("Owner ID"), GNC_OWNER_TREE_ID_COL,
                                        NULL, "1-123-1234",
                                        GNC_TREE_MODEL_OWNER_COL_ID,
                                        GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                        sort_by_string);
    gnc_tree_view_add_text_column(view, _("Currency"), GNC_OWNER_TREE_CURRENCY_COL,
                                  NULL, sample_currency,
                                  GNC_TREE_MODEL_OWNER_COL_CURRENCY,
                                  GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                  sort_by_string);
    gnc_tree_view_add_text_column(view, _("Address Name"), GNC_OWNER_TREE_ADDRESS_NAME_COL,
                                  NULL, "GnuCash Inc.",
                                  GNC_TREE_MODEL_OWNER_COL_ADDRESS_NAME,
                                  GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                  sort_by_string);
    gnc_tree_view_add_text_column(view, _("Address 1"), GNC_OWNER_TREE_ADDRESS_1_COL,
                                  NULL, "Free Software Foundation",
                                  GNC_TREE_MODEL_OWNER_COL_ADDRESS_1,
                                  GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                  sort_by_string);
    gnc_tree_view_add_text_column(view, _("Address 2"), GNC_OWNER_TREE_ADDRESS_2_COL,
                                  NULL, "51 Franklin Street, Fifth Floor",
                                  GNC_TREE_MODEL_OWNER_COL_ADDRESS_2,
                                  GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                  sort_by_string);
    gnc_tree_view_add_text_column(view, _("Address 3"), GNC_OWNER_TREE_ADDRESS_3_COL,
                                  NULL, "Boston, MA  02110-1301",
                                  GNC_TREE_MODEL_OWNER_COL_ADDRESS_3,
                                  GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                  sort_by_string);
    gnc_tree_view_add_text_column(view, _("Address 4"), GNC_OWNER_TREE_ADDRESS_4_COL,
                                  NULL, "USA",
                                  GNC_TREE_MODEL_OWNER_COL_ADDRESS_4,
                                  GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                  sort_by_string);
    gnc_tree_view_add_text_column(view, _("Phone"), GNC_OWNER_TREE_PHONE_COL,
                                  NULL, "+1-617-542-5942",
                                  GNC_TREE_MODEL_OWNER_COL_PHONE,
                                  GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                  sort_by_string);
    gnc_tree_view_add_text_column(view, _("Fax"), GNC_OWNER_TREE_FAX_COL,
                                  NULL, "+1-617-542-2652",
                                  GNC_TREE_MODEL_OWNER_COL_FAX,
                                  GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                  sort_by_string);
    gnc_tree_view_add_text_column(view, _("E-mail"), GNC_OWNER_TREE_EMAIL_COL,
                                  NULL, "gnu@gnu.org",
                                  GNC_TREE_MODEL_OWNER_COL_EMAIL,
                                  GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                  sort_by_string);
    gnc_tree_view_add_numeric_column(view, _("Balance"), GNC_OWNER_TREE_BALANCE_COL,
                                     SAMPLE_OWNER_VALUE,
                                     GNC_TREE_MODEL_OWNER_COL_BALANCE,
                                     GNC_TREE_MODEL_OWNER_COL_COLOR_BALANCE,
                                     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                     sort_by_balance_value);

    priv->balance_report_column
        = gnc_tree_view_add_numeric_column(view, _("Balance"), GNC_OWNER_TREE_BALANCE_REPORT_COL,
                                           SAMPLE_OWNER_VALUE,
                                           GNC_TREE_MODEL_OWNER_COL_BALANCE_REPORT,
                                           GNC_TREE_MODEL_OWNER_COL_COLOR_BALANCE,
                                           GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                           sort_by_balance_value);

    priv->notes_column
        = gnc_tree_view_add_text_column(view, _("Notes"), GNC_OWNER_TREE_NOTES_COL, NULL,
                                        "Sample owner notes.",
                                        GNC_TREE_MODEL_OWNER_COL_NOTES,
                                        GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                        sort_by_string);
    gnc_tree_view_add_toggle_column (view, _("Active"),
                                     /* Translators: This string has a context prefix; the translation
                                        must only contain the part after the | character. */
                                     Q_("Column letter for 'Active'|A"),
                                     GNC_OWNER_TREE_ACTIVE_COL,
                                     GNC_TREE_MODEL_OWNER_COL_ACTIVE,
                                     GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                                     sort_by_boolean,
                                     gnc_tree_view_owner_active_toggled);

    /* Update column titles to use the currency name. */
    gtvo_update_column_names(view);

    /* By default only the first column is visible. */
    gnc_tree_view_configure_columns(view);
    gtk_tree_model_filter_set_visible_func (GTK_TREE_MODEL_FILTER (f_model),
                                            gnc_tree_view_owner_filter_helper,
                                            view,
                                            NULL);

    /* Default the sorting to owner name */
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(s_model),
                                         GNC_TREE_MODEL_OWNER_COL_NAME,
                                         GTK_SORT_ASCENDING);

    gtk_widget_show(GTK_WIDGET(view));
    LEAVE("%p", view);
    return GTK_TREE_VIEW(view);
}

/************************************************************/
/*                   Auxiliary Functions                    */
/************************************************************/

#define debug_path(fn, path) {                          \
    gchar *path_string = gtk_tree_path_to_string(path); \
    fn("tree path %s", path_string);                    \
    g_free(path_string);                                \
  }

#if 0 /* Not Used */
static GtkTreePath *
gnc_tree_view_owner_get_path_from_owner (GncTreeViewOwner *view,
        GncOwner *owner)
{
    GtkTreeModel *model, *f_model, *s_model;
    GtkTreePath *path, *f_path, *s_path;

    ENTER("view %p, owner %p (%s)", view, owner, gncOwnerGetName(owner));

    if (owner == NULL)
    {
        LEAVE("no owner");
        return NULL;
    }

    /* Reach down to the real model and get a path for this owner */
    s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
    path = gnc_tree_model_owner_get_path_from_owner (GNC_TREE_MODEL_OWNER(model), owner);
    if (path == NULL)
    {
        LEAVE("no path");
        return NULL;
    }

    /* convert back to a filtered path */
    f_path = gtk_tree_model_filter_convert_child_path_to_path (GTK_TREE_MODEL_FILTER (f_model), path);
    gtk_tree_path_free(path);
    if (!f_path)
    {
        LEAVE("no filter path");
        return NULL;
    }

    /* convert back to a sorted path */
    s_path = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (s_model), f_path);
    gtk_tree_path_free(f_path);
    debug_path(LEAVE, s_path);
    return s_path;
}

static gboolean
gnc_tree_view_owner_get_iter_from_owner (GncTreeViewOwner *view,
        GncOwner *owner,
        GtkTreeIter *s_iter)
{
    GtkTreeModel *model, *f_model, *s_model;
    GtkTreeIter iter, f_iter;

    g_return_val_if_fail(GNC_IS_TREE_VIEW_OWNER(view), FALSE);
    g_return_val_if_fail(owner != NULL, FALSE);
    g_return_val_if_fail(s_iter != NULL, FALSE);

    ENTER("view %p, owner %p (%s)", view, owner, gncOwnerGetName(owner));

    /* Reach down to the real model and get an iter for this owner */
    s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
    if (!gnc_tree_model_owner_get_iter_from_owner (
                GNC_TREE_MODEL_OWNER(model), owner, &iter))
    {
        LEAVE("model_get_iter_from_owner failed");
        return FALSE;
    }

    /* convert back to a sort iter */
    gtk_tree_model_filter_convert_child_iter_to_iter (
        GTK_TREE_MODEL_FILTER(f_model), &f_iter, &iter);
    gtk_tree_model_sort_convert_child_iter_to_iter (GTK_TREE_MODEL_SORT(s_model),
            s_iter, &f_iter);
    LEAVE(" ");
    return TRUE;
}
#endif /* Not Used */

/************************************************************/
/*            Owner Tree View Filter Functions            */
/************************************************************/

static gboolean
gnc_tree_view_owner_filter_helper (GtkTreeModel *model,
                                   GtkTreeIter *iter,
                                   gpointer data)
{
    GncOwner *owner;
    GncTreeViewOwner *view = data;
    GncTreeViewOwnerPrivate *priv;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_OWNER (model), FALSE);
    g_return_val_if_fail (iter != NULL, FALSE);

    owner = gnc_tree_model_owner_get_owner (
                GNC_TREE_MODEL_OWNER(model), iter);

    priv = GNC_TREE_VIEW_OWNER_GET_PRIVATE(view);
    if (priv->filter_fn)
        return priv->filter_fn(owner, priv->filter_data);
    else return TRUE;
}

/*
 * Set an GtkTreeModel visible filter on this owner.  This filter will be
 * called for each owner that the tree is about to show, and the
 * owner will be passed to the callback function.
 *
 * Use NULL as func to remove filter.
 */
void
gnc_tree_view_owner_set_filter (GncTreeViewOwner *view,
                                gnc_tree_view_owner_filter_func func,
                                gpointer data,
                                GSourceFunc destroy)
{
    GncTreeViewOwnerPrivate *priv;

    ENTER("view %p, filter func %p, data %p, destroy %p",
          view, func, data, destroy);

    g_return_if_fail(GNC_IS_TREE_VIEW_OWNER(view));

    priv = GNC_TREE_VIEW_OWNER_GET_PRIVATE(view);
    if (priv->filter_destroy)
    {
        priv->filter_destroy(priv->filter_data);
    }
    priv->filter_destroy = destroy;
    priv->filter_data = data;
    priv->filter_fn = func;

    gnc_tree_view_owner_refilter(view);
    LEAVE(" ");
}

/*
 * Forces the entire owner tree to be re-evaluated for visibility.
 */
void
gnc_tree_view_owner_refilter (GncTreeViewOwner *view)
{
    GtkTreeModel *f_model, *s_model;

    g_return_if_fail(GNC_IS_TREE_VIEW_OWNER(view));

    s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    gtk_tree_model_filter_refilter (GTK_TREE_MODEL_FILTER (f_model));
}

/************************************************************/
/*           Owner Tree View Get/Set Functions            */
/************************************************************/

/*
 * Retrieve the selected owner from an owner tree view.  The
 * owner tree must be in single selection mode.
 */
GncOwner *
gnc_tree_view_owner_get_owner_from_path (GncTreeViewOwner *view,
        GtkTreePath *s_path)
{
    GtkTreeModel *model, *f_model, *s_model;
    GtkTreePath *path, *f_path;
    GtkTreeIter iter;
    GncOwner *owner;

    ENTER("view %p", view);
    g_return_val_if_fail (GNC_IS_TREE_VIEW_OWNER (view), NULL);
    g_return_val_if_fail (s_path != NULL, NULL);

    s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
    f_path = gtk_tree_model_sort_convert_path_to_child_path (
                 GTK_TREE_MODEL_SORT (s_model), s_path);
    if (!f_path)
    {
        LEAVE("no filter path");
        return NULL;
    }

    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    path = gtk_tree_model_filter_convert_path_to_child_path (
               GTK_TREE_MODEL_FILTER (f_model), f_path);
    gtk_tree_path_free(f_path);
    if (!path)
    {
        LEAVE("no path");
        return NULL;
    }

    model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
    if (!gtk_tree_model_get_iter (model, &iter, path))
    {
        LEAVE("no iter");
        return NULL;
    }

    owner = iter.user_data;
    gtk_tree_path_free(path);
    LEAVE("owner %p (%s)", owner, gncOwnerGetName (owner));
    return owner;
}


GncOwner *
gnc_tree_view_owner_get_owner_from_iter (GtkTreeModel *s_model,
        GtkTreeIter  *s_iter)
{
    GtkTreeModel *model, *f_model;
    GtkTreeIter iter, f_iter;
    GncOwner *owner;

    g_return_val_if_fail (GTK_IS_TREE_MODEL_SORT(s_model), NULL);
    g_return_val_if_fail (s_iter != NULL, NULL);

    ENTER("model %p, iter %p", s_model, s_iter);

    gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT(s_model),
            &f_iter,
            s_iter);
    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    gtk_tree_model_filter_convert_iter_to_child_iter (
        GTK_TREE_MODEL_FILTER(f_model), &iter, &f_iter);
    model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
    owner = gnc_tree_model_owner_get_owner (
                GNC_TREE_MODEL_OWNER(model), &iter);
    LEAVE("owner %p (%s)", owner, gncOwnerGetName (owner));
    return owner;
}


/*
 * Retrieve the selected owner from an owner tree view.  The
 * owner tree must be in single selection mode.
 */
GncOwner *
gnc_tree_view_owner_get_selected_owner (GncTreeViewOwner *view)
{
    GtkTreeSelection *selection;
    GtkTreeModel *f_model, *s_model;
    GtkTreeIter iter, f_iter, s_iter;
    GncOwner *owner;
    GtkSelectionMode mode;

    ENTER("view %p", view);
    g_return_val_if_fail (GNC_IS_TREE_VIEW_OWNER (view), NULL);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(view));
    mode = gtk_tree_selection_get_mode(selection);
    if ((mode != GTK_SELECTION_SINGLE) && (mode != GTK_SELECTION_BROWSE))
    {
        return NULL;
    }
    if (!gtk_tree_selection_get_selected (selection, &s_model, &s_iter))
    {
        LEAVE("no owner, get_selected failed");
        return FALSE;
    }

    gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model),
            &f_iter, &s_iter);

    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    gtk_tree_model_filter_convert_iter_to_child_iter (
        GTK_TREE_MODEL_FILTER (f_model), &iter, &f_iter);

    owner = iter.user_data;
    LEAVE("owner %p (%s)", owner, gncOwnerGetName (owner));
    return owner;
}

/*
 * Selects a single owner in the owner tree view.  The owner
 * tree must be in single selection mode.
 */
void
gnc_tree_view_owner_set_selected_owner (GncTreeViewOwner *view,
                                        GncOwner *owner)
{
    GtkTreeModel *model, *f_model, *s_model;
    GtkTreePath *path, *f_path, *s_path;
    GtkTreeSelection *selection;

    ENTER("view %p, owner %p (%s)", view,
          owner, gncOwnerGetName (owner));
    g_return_if_fail (GNC_IS_TREE_VIEW_OWNER (view));

    /* Clear any existing selection. */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
    gtk_tree_selection_unselect_all (selection);

    if (owner == NULL)
        return;

    s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));

    path = gnc_tree_model_owner_get_path_from_owner (
               GNC_TREE_MODEL_OWNER(model), owner);
    if (path == NULL)
    {
        LEAVE("no path");
        return;
    }
    debug_path(DEBUG, path);

    f_path = gtk_tree_model_filter_convert_child_path_to_path (
                 GTK_TREE_MODEL_FILTER (f_model), path);
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

    gtk_tree_selection_select_path (selection, s_path);

    /* give gtk+ a chance to resize the tree view first by handling pending
     * configure events */
    while (gtk_events_pending ())
        gtk_main_iteration ();
    gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW(view), s_path, NULL, FALSE, 0.0, 0.0);
    debug_path(LEAVE, s_path);
    gtk_tree_path_free(s_path);
}

/* Information re selection process */
typedef struct
{
    GList* return_list;
    GncTreeViewOwnerPrivate* priv;
} GncTreeViewSelectionInfo;

#if 0 /* Not Used */
/*
 * This helper function is called once for each row in the tree view
 * that is currently selected.  Its task is to append the corresponding
 * owner to the end of a glist.
 */
static void
get_selected_owners_helper (GtkTreeModel *s_model,
                            GtkTreePath *s_path,
                            GtkTreeIter *s_iter,
                            gpointer data)
{
    GncTreeViewSelectionInfo *gtvsi = data;
    GtkTreeModel *f_model;
    GtkTreeIter iter, f_iter;
    GncOwner *owner;

    gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model),
            &f_iter, s_iter);

    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER (f_model),
            &iter, &f_iter);
    owner = iter.user_data;

    /* Only selected if it passes the filter */
    if (gtvsi->priv->filter_fn == NULL || gtvsi->priv->filter_fn(owner, gtvsi->priv->filter_data))
    {
        gtvsi->return_list = g_list_append(gtvsi->return_list, owner);
    }
}
#endif /* Not Used */

/************************************************************/
/*         Owner Tree View Add Column Functions           */
/************************************************************/

static void
gtvo_update_column_name (GtkTreeViewColumn *column,
                         const gchar *fmt,
                         const gchar *mnemonic)
{
    gchar *name;

    g_return_if_fail(column);

    name = g_strdup_printf(fmt, mnemonic);
    gtk_tree_view_column_set_title(column, name);
    g_free(name);
}


static void
gtvo_update_column_names (GncTreeView *view)
{
    GncTreeViewOwnerPrivate *priv;
    const gchar *mnemonic;

    priv = GNC_TREE_VIEW_OWNER_GET_PRIVATE(view);
    mnemonic = gnc_commodity_get_mnemonic(gnc_default_report_currency());

    gtvo_update_column_name(priv->balance_report_column,
                            /* Translators: %s is a currency mnemonic.*/
                            _("Balance (%s)"), mnemonic);
    gnc_tree_view_set_show_column_menu(view, FALSE);
    gnc_tree_view_set_show_column_menu(view, TRUE);
}


static void
gtvo_currency_changed_cb (void)
{
    const GList *views, *ptr;

    views = gnc_gobject_tracking_get_list (GNC_TREE_VIEW_OWNER_NAME);
    for (ptr = views; ptr; ptr = g_list_next(ptr))
    {
        gtvo_update_column_names (ptr->data);
    }
}

#if 0 /* Not Used */
/* This function implements a custom mapping between an owner's KVP
 * and the cell renderer's 'text' property. */
static void
owner_cell_kvp_data_func (GtkTreeViewColumn *tree_column,
                          GtkCellRenderer *cell,
                          GtkTreeModel *s_model,
                          GtkTreeIter *s_iter,
                          gpointer key)
{
    GncOwner *owner;
    kvp_frame * frame;

    g_return_if_fail (GTK_IS_TREE_MODEL_SORT (s_model));
    owner = gnc_tree_view_owner_get_owner_from_iter(s_model, s_iter);
    frame = gncOwnerGetSlots(owner);

    g_object_set (G_OBJECT (cell),
                  "text", kvp_frame_get_string(frame, (gchar *)key),
                  "xalign", 0.0,
                  NULL);

}

static void col_edited_helper(GtkCellRendererText *cell, gchar *path_string,
                              gchar *new_text, gpointer _s_model)
{
    GncOwner *owner;
    GtkTreeModel *s_model;
    GtkTreeIter s_iter;
    GncTreeViewOwnerColumnTextEdited col_edited_cb;
    GtkTreeViewColumn *col;

    col_edited_cb = g_object_get_data(G_OBJECT(cell),
                                      "column_edited_callback");
    col = GTK_TREE_VIEW_COLUMN(g_object_get_data(G_OBJECT(cell),
                               "column_view"));
    s_model = GTK_TREE_MODEL(_s_model);

    if (!gtk_tree_model_get_iter_from_string(s_model, &s_iter, path_string))
        return;

    owner = gnc_tree_view_owner_get_owner_from_iter(s_model, &s_iter);
    col_edited_cb(owner, col, new_text);
}

static void col_source_helper(GtkTreeViewColumn *col, GtkCellRenderer *cell,
                              GtkTreeModel *s_model, GtkTreeIter *s_iter,
                              gpointer _col_source_cb)
{
    GncOwner *owner;
    gchar *text;
    GncTreeViewOwnerColumnSource col_source_cb;

    g_return_if_fail (GTK_IS_TREE_MODEL_SORT (s_model));
    col_source_cb = (GncTreeViewOwnerColumnSource) _col_source_cb;
    owner = gnc_tree_view_owner_get_owner_from_iter(s_model, s_iter);
    text = col_source_cb(owner, col, cell);
    g_object_set (G_OBJECT (cell), "text", text, "xalign", 1.0, NULL);
    g_free(text);
}

/**
 * If col_edited_cb is null, the editing callback (helper) will be
 * effectively disconnected.
 **/
void
gtvo_setup_column_renderer_edited_cb(GncTreeViewOwner *owner_view,
                                     GtkTreeViewColumn *column,
                                     GtkCellRenderer *renderer,
                                     GncTreeViewOwnerColumnTextEdited col_edited_cb)
{
    GtkTreeModel *s_model;

    if (col_edited_cb == NULL)
    {
        g_object_set(G_OBJECT(renderer), "editable", FALSE, NULL);
        g_object_set_data(G_OBJECT(renderer), "column_edited_callback", col_edited_cb);
        s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(owner_view));
        g_signal_handlers_disconnect_by_func(G_OBJECT(renderer), col_edited_cb, s_model);
        g_object_set_data(G_OBJECT(renderer), "column_view", column);
    }
    else
    {
        g_object_set(G_OBJECT(renderer), "editable", TRUE, NULL);
        g_object_set_data(G_OBJECT(renderer), "column_edited_callback",
                          col_edited_cb);
        s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(owner_view));
        g_signal_connect(G_OBJECT(renderer), "edited",
                         (GCallback) col_edited_helper, s_model);
        g_object_set_data(G_OBJECT(renderer), "column_view", column);
    }
}
#endif /* Not Used */

/* BEGIN FILTER FUNCTIONS */
#define FILTER_TREE_VIEW "types_tree_view"

/** This function tells the owner tree view whether or not to filter
 *  out a particular owner.  Owners may be filtered if the user
 *  has decided not to display inactive owners, or if the
 *  user has requested that owners with a zero total not be shown.
 *
 *  @param owner The owner that is being evaluated.
 *
 *  @param user_data A pointer to the OwnerFilterDialog struct.
 *
 *  @return TRUE if the owner should be visible.  FALSE if the
 *  owner should be hidden. */
gboolean
gnc_plugin_page_owner_tree_filter_owners (GncOwner *owner,
        gpointer user_data)
{
    OwnerFilterDialog *fd = user_data;
    gnc_numeric total;

    ENTER("owner %p:%s", owner, gncOwnerGetName(owner));

    if (!fd->show_inactive && !gncOwnerGetActive (owner))
    {
        LEAVE(" hide: inactive");
        return FALSE;
    }

    if (!fd->show_zero_total)
    {
        total = gncOwnerGetBalanceInCurrency (owner, NULL);
        if (gnc_numeric_zero_p(total))
        {
            LEAVE(" hide: zero balance");
            return FALSE;
        }
    }

    return TRUE;
}

/** The "only show active" button in the Filter dialog changed state.
 *  Update the page to reflect these changes.
 *
 *  @param button The GtkCheckButton that was toggled.
 *
 *  @param fd A pointer to the owner filter dialog struct. */
void
gppot_filter_show_inactive_toggled_cb (GtkToggleButton *button,
                                       OwnerFilterDialog *fd)
{
    g_return_if_fail(GTK_IS_TOGGLE_BUTTON(button));

    ENTER("button %p", button);
    fd->show_inactive = !gtk_toggle_button_get_active(button);
    gnc_tree_view_owner_refilter(fd->tree_view);
    LEAVE("show_inactive %d", fd->show_inactive);
}

/** The "show zero totals" button in the Filter dialog changed state.
 *  Update the page to reflect these changes.
 *
 *  @param button The GtkCheckButton that was toggled.
 *
 *  @param fd A pointer to the owner filter dialog struct. */
void
gppot_filter_show_zero_toggled_cb (GtkToggleButton *button,
                                   OwnerFilterDialog *fd)
{
    g_return_if_fail(GTK_IS_TOGGLE_BUTTON(button));

    ENTER("button %p", button);
    fd->show_zero_total = gtk_toggle_button_get_active(button);
    gnc_tree_view_owner_refilter(fd->tree_view);
    LEAVE("show_zero %d", fd->show_zero_total);
}

/** The Filter dialog was closed.  Check to see if this was done via
 *  the OK button.  If so, make the changes permanent.  If not, revert
 *  any changes.
 *
 *  @param dialog A pointer to the "Filter By" dialog.
 *
 *  @param response The response code from closing the dialog.
 *
 *  @param fd A pointer to the owner filter dialog struct. */
void
gppot_filter_response_cb (GtkWidget *dialog,
                          gint       response,
                          OwnerFilterDialog *fd)
{
    gpointer gptemp;

    g_return_if_fail(GTK_IS_DIALOG(dialog));

    ENTER("dialog %p, response %d", dialog, response);

    if (response != GTK_RESPONSE_OK)
    {
        fd->show_inactive = fd->original_show_inactive;
        fd->show_zero_total = fd->original_show_zero_total;
        gnc_tree_view_owner_refilter(fd->tree_view);
    }

    /* Clean up and delete dialog */
    gptemp = (gpointer *)fd->dialog;
    g_atomic_pointer_compare_and_exchange(&gptemp,
                                          dialog, NULL);
    fd->dialog = gptemp;
    gtk_widget_destroy(dialog);
    LEAVE("");
}

void
owner_filter_dialog_create(OwnerFilterDialog *fd, GncPluginPage *page)
{
    GtkWidget *dialog, *button;
    GtkBuilder *builder;
    gchar *title;

    ENTER("(fd %p, page %p)", fd, page);

    if (fd->dialog)
    {
        gtk_window_present(GTK_WINDOW(fd->dialog));
        LEAVE("existing dialog");
        return;
    }

    /* Create the dialog */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "gnc-tree-view-owner.glade", "Filter By");
    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "Filter By"));
    fd->dialog = dialog;
    gtk_window_set_transient_for(GTK_WINDOW(dialog),
                                 GTK_WINDOW(GNC_PLUGIN_PAGE(page)->window));
    /* Translators: The %s is the name of the plugin page */
    title = g_strdup_printf(_("Filter %s by..."),
                            gnc_plugin_page_get_page_name(GNC_PLUGIN_PAGE(page)));
    gtk_window_set_title(GTK_WINDOW(dialog), title);
    g_free(title);

    /* Remember current state */
    fd->original_show_inactive = fd->show_inactive;
    fd->original_show_zero_total = fd->show_zero_total;

    /* Update the dialog widgets for the current state */
    button = GTK_WIDGET(gtk_builder_get_object (builder, "show_inactive"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button),
                                  !fd->show_inactive);
    button = GTK_WIDGET(gtk_builder_get_object (builder, "show_zero"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button),
                                  fd->show_zero_total);

    /* Wire up the rest of the callbacks */
    gtk_builder_connect_signals (builder, fd);
    g_object_unref(G_OBJECT(builder));

    /* Show it */
    gtk_widget_show_all (dialog);
    LEAVE(" ");
}

#define OWNER_SELECTED_LABEL "SelectedOwner"
#define SHOW_INACTIVE_LABEL  "ShowInactive"
#define SHOW_ZERO_LABEL      "ShowZeroTotal"

typedef struct foo
{
    GKeyFile *key_file;
    const gchar *group_name;
} bar_t;

/** Save information about the selected row.  Its job is to write the
 *  full owner name of the row out to the state file.
 *
 *  @param view A pointer to the GtkTreeView embedded in an
 *  owner tree page.
 *
 *  @param path A pointer to a particular entry in the tree.
 *
 *  @param data A pointer to a data structure holding the information
 *  related to the state file. */
static void
tree_save_selected_row (GncTreeViewOwner *view,
                        gpointer user_data)
{
    GncOwner *owner;
    bar_t *bar = user_data;
    const gchar *owner_name;

    owner = gnc_tree_view_owner_get_selected_owner(view);
    if (owner == NULL)
        return;

    owner_name = gncOwnerGetName (owner);
    if (owner_name == NULL)
        return;

    g_key_file_set_string(bar->key_file, bar->group_name, OWNER_SELECTED_LABEL,
                          owner_name);
}

void
gnc_tree_view_owner_save(GncTreeViewOwner *view,
                         OwnerFilterDialog *fd,
                         GKeyFile *key_file, const gchar *group_name)
{
    bar_t bar;

    g_return_if_fail (key_file != NULL);
    g_return_if_fail (group_name != NULL);

    ENTER("view %p, key_file %p, group_name %s", view, key_file,
          group_name);

    g_key_file_set_boolean(key_file, group_name, SHOW_INACTIVE_LABEL,
                           fd->show_inactive);
    g_key_file_set_boolean(key_file, group_name, SHOW_ZERO_LABEL,
                           fd->show_zero_total);

    bar.key_file = key_file;
    bar.group_name = group_name;
    tree_save_selected_row(view, &bar);
    LEAVE(" ");

}

/** Select the row in the tree that was selected when the user last
 *  quit gnucash.  Its job is to map from owner name to tree row and
 *  select the row.
 *
 *  @param tree A pointer to the GncTreeViewOwner embedded.
 *
 *  @param owner_name A pointer to the full owner name. */
static void
tree_restore_selected_row (GncTreeViewOwner *view,
                           GncOwnerType owner_type,
                           const gchar *owner_guid_str)
{
    GncOwner *owner = gncOwnerNew();
    QofBook *book;
    GncGUID owner_guid;

    book = qof_session_get_book (gnc_get_current_session());
    if (string_to_guid (owner_guid_str, &owner_guid))
        if (gncOwnerGetOwnerFromTypeGuid (book, owner, gncOwnerTypeToQofIdType(owner_type), &owner_guid))
            gnc_tree_view_owner_set_selected_owner(view, owner);
}

void
gnc_tree_view_owner_restore(GncTreeViewOwner *view,
                            OwnerFilterDialog *fd,
                            GKeyFile *key_file, const gchar *group_name,
                            GncOwnerType owner_type)
{
    GError *error = NULL;
    gchar *value;
    gboolean show;

    /* Filter information. Ignore missing keys. */
    show = g_key_file_get_boolean(key_file, group_name, SHOW_INACTIVE_LABEL, &error);
    if (error)
    {
        g_warning("error reading group %s key %s: %s",
                  group_name, SHOW_INACTIVE_LABEL, error->message);
        g_error_free(error);
        error = NULL;
        show = TRUE;
    }
    fd->show_inactive = show;

    show = g_key_file_get_boolean(key_file, group_name, SHOW_ZERO_LABEL, &error);
    if (error)
    {
        g_warning("error reading group %s key %s: %s",
                  group_name, SHOW_ZERO_LABEL, error->message);
        g_error_free(error);
        error = NULL;
        show = TRUE;
    }
    fd->show_zero_total = show;

    /* Selected owner (if any) */
    value = g_key_file_get_string(key_file, group_name, OWNER_SELECTED_LABEL, NULL);
    if (value)
    {
        tree_restore_selected_row(view, owner_type, value);
        g_free(value);
    }

    /* Update tree view for any changes */
    gnc_tree_view_owner_refilter(view);
}

#if 0 /* Not Used */
static void
gtvo_set_column_editor(GncTreeViewOwner *view,
                       GtkTreeViewColumn *column,
                       GncTreeViewOwnerColumnTextEdited edited_cb)
{
    GList *renderers_orig, *renderers;
    GtkCellRenderer *renderer;

    // look for the first text-renderer; on the 0th column of the owner tree,
    // there are two renderers: pixbuf and text.  So find the text one.
    for (renderers_orig = renderers = gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(column));
            renderers && !GTK_IS_CELL_RENDERER_TEXT(renderers->data);
            renderers = renderers->next);
    renderer = GTK_CELL_RENDERER(renderers->data);
    g_list_free(renderers_orig);
    g_return_if_fail(renderer != NULL);
    gtvo_setup_column_renderer_edited_cb(GNC_TREE_VIEW_OWNER(view), column, renderer, edited_cb);
}
#endif /* Not Used */
